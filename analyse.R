#' Segments fashion products using PCA, t-SNE and cross-validated K-means
#'
#' Runs the full unsupervised segmentation pipeline on the feature matrix
#' produced by \code{\link{prep_data}}:
#' \enumerate{
#'   \item \strong{PCA} — reduces dimensionality and captures variance structure.
#'   \item \strong{t-SNE} — further reduces PCA scores to 2-D for visualisation.
#'   \item \strong{CV-validated K-means} — clusters products into profit tiers
#'     (Low, Medium and High) to report out of sample
#'     within cluster sum of squares (WCSS) as a quality metric.
#' }
#' Clusters are labelled by comparing mean price per cluster: the top third
#' become \emph{High}, the bottom third \emph{Low}, and the remainder \emph{Mid}.
#'
#' @param prepped A list returned by \code{\link{prep_data}}, containing a
#'   numeric matrix in \code{prepped$features}.
#' @param n_clusters Integer. Number of clusters (profit tiers). Default is
#'   \code{3} (Low, Medium and High). Must be \eqn{\ge 2}.
#' @param n_pca_components Integer. Maximum number of principal components to
#'   keep before running t-SNE. default = 10, but automatically limited if too large).
#' @param tsne_perplexity Numeric. t-SNE perplexity parameter. Automatically
#'   restricted to \code{max(1, floor((n-1)/3))} to avoid errors on small datasets.
#'   Default is \code{30}.
#' @param seed Integer. Random seed for reproducibility of K-means and t-SNE.
#'   Default is \code{42}.
#'
#' @return An object of class \code{"retailr_result"}, which is a named list:
#'   \describe{
#'     \item{\code{clusters}}{Integer vector (1=High, 2=Mid, 3=Low) of length \eqn{n}.}
#'     \item{\code{cluster_labels}}{Character vector of tier labels
#'       (\code{"High"}, \code{"Mid"}, \code{"Low"}) of length \eqn{n}.}
#'     \item{\code{cluster_means}}{Data frame of per-cluster mean feature values,
#'       used for recommendation gap analysis.}
#'     \item{\code{pca}}{The \code{prcomp} object.}
#'     \item{\code{pca_scores}}{Matrix of retained PCA scores (n × n_pca).}
#'     \item{\code{pca_importance}}{Data frame with columns \code{PC},
#'       \code{PropVariance}, and \code{CumulativeVariance}.}
#'     \item{\code{top_features}}{Character vector of feature names sorted by
#'       absolute loading on PC1 (most discriminating feature first).}
#'     \item{\code{tsne_coords}}{Numeric matrix (n × 2) of t-SNE coordinates.}
#'     \item{\code{cv_wcss}}{cross-validated WCSS at the chosen K.}
#'     \item{\code{n_clusters}}{The number of clusters used.}
#'     \item{\code{features}}{The original feature matrix from \code{prep_data}.}
#'     \item{\code{km}}{The \code{kmeans} object from the final full data run.}
#'     \item{\code{n_pca}}{Number of PCA components actually used.}
#'   }
#'
#' @details
#' \strong{Why K = 3 is fixed by default:}
#' The three-tier labelling (High / Mid / Low profit) is a domain requirement for
#' fashion retail segmentation. The CV step does not select K; it validates the
#' chosen K by reporting out of sample WCSS, giving a quantitative quality signal
#' without relying on the elbow method (which is unreliable on smooth,
#' continuous price distributions).
#'
#' Packages \pkg{Rtsne} and \pkg{cluster} must be installed:
#' \code{install.packages(c("Rtsne", "cluster"))}.
#'
#' @examples
#' \dontrun{
#' raw     <- read.csv("fashion_products.csv", stringsAsFactors = FALSE)
#' prepped <- prep_data(raw)
#' result  <- analyse(prepped)
#' print(result)
#'
#' # Custom number of clusters
#' result4 <- analyse(prepped, n_clusters = 4)
#' }
#'
#' @seealso \code{\link{prep_data}}, \code{\link{recommend}},
#'   \code{\link{plot_clusters}}, \code{\link{save_all_plots}}
#' @importFrom stats prcomp kmeans aggregate sd median tapply
#' @export
analyse <- function(prepped,
                    n_clusters       = NULL,
                    n_pca_components = 10,
                    tsne_perplexity  = 30,
                    seed             = 42) {
  
  if (!is.list(prepped) || is.null(prepped$features))
    stop("'prepped' must be the list returned by prep_data().")
  
  X <- prepped$features
  n <- nrow(X)
  p <- ncol(X)
  
  if (n < 10) stop("Need at least 10 products to run analyse().")
  
  set.seed(seed)
  
  #PCA
  n_pca  <- min(n_pca_components, p, n - 1)
  pca    <- prcomp(X, center = TRUE, scale. = TRUE)
  scores <- pca$x[, seq_len(n_pca), drop = FALSE]
  
  pca_var <- summary(pca)$importance
  pca_imp <- data.frame(
    PC                 = colnames(pca_var),
    PropVariance       = as.numeric(pca_var[2, ]),
    CumulativeVariance = as.numeric(pca_var[3, ]),
    stringsAsFactors   = FALSE
  )
  
  pc1_load     <- abs(pca$rotation[, 1])
  top_features <- names(sort(pc1_load, decreasing = TRUE))
  
  # t-SNE
  if (!requireNamespace("Rtsne", quietly = TRUE))
    stop("Package 'Rtsne' is required. Install with: install.packages('Rtsne')")
  
  perp        <- max(1, min(tsne_perplexity, floor((n - 1) / 3)))
  tsne_out    <- Rtsne::Rtsne(scores, dims = 2, perplexity = perp,
                              verbose = FALSE, check_duplicates = FALSE)
  tsne_coords <- tsne_out$Y
  colnames(tsne_coords) <- c("tSNE1", "tSNE2")
  
  #clustering 
  if (!requireNamespace("cluster", quietly = TRUE))
    stop("Package 'cluster' is required. Install with: install.packages('cluster')")
  
  if (is.null(n_clusters)) n_clusters <- 3L
  
  n_folds      <- 5
  fold_ids     <- sample(rep(1:n_folds, length.out = n))
  cv_wcss_vals <- sapply(1:n_folds, function(f) {
    train_idx <- which(fold_ids != f)
    test_idx  <- which(fold_ids == f)
    km_cv     <- kmeans(scores[train_idx, , drop = FALSE],
                        centers = n_clusters, nstart = 10, iter.max = 100)
    test_pts  <- scores[test_idx, , drop = FALSE]
    dists_mat <- sapply(1:n_clusters, function(c)
      rowSums((test_pts - matrix(km_cv$centers[c, ],
                                 nrow = nrow(test_pts),
                                 ncol = ncol(test_pts),
                                 byrow = TRUE))^2))
    assigned  <- apply(dists_mat, 1, which.min)
    sum(sapply(seq_len(nrow(test_pts)), function(i)
      sum((test_pts[i, ] - km_cv$centers[assigned[i], ])^2)))
  })
  cv_wcss_mean <- mean(cv_wcss_vals)
  message(sprintf("CV-validated K = %d clusters | 5-fold CV WCSS = %.1f",
                  n_clusters, cv_wcss_mean))
  
  km <- kmeans(scores, centers = n_clusters, nstart = 25, iter.max = 100)
  
  # label clusters
  price_feat    <- X[, 1]
  cluster_price <- tapply(price_feat, km$cluster, mean)
  price_rank    <- rank(-cluster_price)
  total_k       <- length(price_rank)
  tier_size     <- max(1, floor(total_k / 3))
  
  label_map <- setNames(
    ifelse(price_rank <= tier_size, "High",
           ifelse(price_rank > total_k - tier_size, "Low", "Mid")),
    names(price_rank)
  )
  cluster_labels <- as.character(label_map[as.character(km$cluster)])
  
  lvl_map  <- c("High" = 1L, "Mid" = 2L, "Low" = 3L)
  clusters <- lvl_map[cluster_labels]
  
  # cluster means table 
  feat_df <- as.data.frame(X)
  feat_df$cluster_label <- cluster_labels
  cluster_means <- aggregate(. ~ cluster_label, data = feat_df, FUN = mean)
  
  #result
  result <- list(
    clusters         = clusters,
    cluster_labels   = cluster_labels,
    cluster_means    = cluster_means,
    pca              = pca,
    pca_scores       = scores,
    pca_importance   = pca_imp,
    top_features     = top_features,
    tsne_coords      = tsne_coords,
    cv_wcss          = cv_wcss_mean,
    n_clusters       = n_clusters,
    features         = X,
    km               = km,
    n_pca            = n_pca
  )
  class(result) <- "retailr_result"
  return(result)
}


#' Print a summary of a retailr analysis result
#'
#' S3 print method for objects of class \code{"retailr_result"} returned by
#' \code{\link{analyse}}. Displays a concise summary: product and feature
#' counts, cluster sizes by profit tier, cross-validated WCSS and the five
#' most discriminating features.
#'
#' @param x A \code{retailr_result} object.
#' @param \dots Further arguments passed to or from other methods (currently
#'   unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#' print(result)   # or simply: result
#' }
#'
#' @export
print.retailr_result <- function(x, ...) {
  cat("retailr analysis result \n")
  cat(sprintf("  Products      : %d\n", nrow(x$features)))
  cat(sprintf("  Features used : %d\n", ncol(x$features)))
  cat(sprintf("  Clusters (K)  : %d\n", x$n_clusters))
  tbl <- table(x$cluster_labels)
  for (lbl in c("High", "Mid", "Low")) {
    if (lbl %in% names(tbl))
      cat(sprintf("    %-4s profit : %d products\n", lbl, tbl[[lbl]]))
  }
  cat(sprintf("  CV WCSS       : %.1f\n", x$cv_wcss))
  cat(sprintf("  Top features  : %s\n",
              paste(head(x$top_features, 5), collapse = ", ")))
  invisible(x)
}
