#' Generates actionable upgrade recommendations for fashion products
#'
#' For a given product (existing or new), \code{recommend} identifies its
#' profit tier, finds the nearest higher-tier centroid, quantifies the feature
#' gap and returns a plain English verdict with the top actionable suggestions.
#' New products are classified via k-nearest-neighbour (\code{kNN}) before
#' gap analysis.
#'
#' @param result A \code{retailr_result} object from \code{\link{analyse}}.
#' @param product_index Integer. Row index (1-based) of an existing product in
#'   \code{result$features}. Ignored when \code{new_product} is supplied.
#'   Default is \code{1}.
#' @param new_product Numeric vector or single-row data frame with the same
#'   number of columns as \code{result$features}. Supply this to classify and
#'   analyse a brand-new product not present in the training data. When
#'   provided, \code{product_index} is ignored and kNN classification is used.
#' @param k_knn Integer. Number of neighbours for kNN classification of
#'   \code{new_product}. Default is \code{5}.
#' @param top_n Integer. Maximum number of actionable feature gaps to return.
#'   Default is \code{5}.
#' @param precomputed_centroids Named list of centroid vectors (one per tier
#'   label). Pass this when calling \code{recommend} in a loop to avoid
#'   recomputing centroids for every product. Default is \code{NULL}.
#'
#' @return A named list:
#'   \describe{
#'     \item{\code{current_cluster}}{Character. The product's current tier
#'       (\code{"High"}, \code{"Mid"}, or \code{"Low"}).}
#'     \item{\code{nearest_upgrade_cluster}}{Character. The nearest higher
#'       tier or \code{"Already High"} if already at the top.}
#'     \item{\code{distance_to_upgrade}}{Numeric. Euclidean distance from the
#'       product to the upgrade centroid in feature space.}
#'     \item{\code{feature_gap}}{Data frame with columns \code{feature},
#'       \code{current_value}, \code{target_value} and \code{gap} for the top
#'       actionable features (price, premium, luxury, material, volume).}
#'     \item{\code{context_features}}{Data frame of fixed/demographic features
#'       (gender, colour, category) and their gap to the upgrade centroid.}
#'     \item{\code{verdict}}{Character. One of:
#'       \code{"Scale — already in high-profit tier"},
#'       \code{"Reposition — easy upgrade to High profit"},
#'       \code{"Reposition — moderate effort to reach next tier"}, or
#'       \code{"Discontinue — large actionable gap, hard to fix"}.}
#'     \item{\code{mode}}{Character. \code{"existing"} or \code{"new_product"}.}
#'     \item{\code{predicted_cluster_probabilities}}{Named numeric vector of
#'       kNN vote proportions per tier (only populated when
#'       \code{new_product} is supplied, otherwise \code{NULL}).}
#'   }
#'
#' @details
#' \strong{Verdict thresholds:}
#' \itemize{
#'   \item Mean actionable gap \eqn{< 0.2} to High → \emph{easy fix}.
#'   \item Mean actionable gap \eqn{< 0.4} → \emph{moderate fix}.
#'   \item Mean actionable gap \eqn{\ge 0.4} → \emph{discontinue}.
#' }
#' All features are already normalised to \eqn{[0,1]} by \code{prep_data}, so
#' gaps are directly comparable across features.
#'
#' \strong{Performance tip:} when iterating over thousands of products,
#' pre-compute centroids once and pass them via \code{precomputed_centroids}:
#' \preformatted{
#' X      <- result$features
#' labels <- result$cluster_labels
#' cents  <- lapply(unique(labels), function(l)
#'              colMeans(X[labels == l, , drop = FALSE]))
#' names(cents) <- unique(labels)
#' for (i in 1:nrow(X))
#'   rec <- recommend(result, i, precomputed_centroids = cents)
#' }
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#'
#' # Existing product
#' rec <- recommend(result, product_index = 42)
#' rec$verdict
#' rec$feature_gap
#'
#' # New product
#' new_vec <- result$features[1, ]  # pretend it is new
#' rec2 <- recommend(result, new_product = new_vec)
#' rec2$predicted_cluster_probabilities
#' }
#'
#' @seealso \code{\link{analyse}}, \code{\link{prep_data}}
#' @export
recommend <- function(result,
                      product_index       = 1L,
                      new_product         = NULL,
                      k_knn               = 5L,
                      top_n               = 5L,
                      precomputed_centroids = NULL) {
  
  if (!inherits(result, "retailr_result"))
    stop("'result' must be a retailr_result object from analyse().")
  
  X      <- result$features
  labels <- result$cluster_labels
  n      <- nrow(X)
  p      <- ncol(X)
  
  knn_probs <- NULL
  mode_used <- "existing"
  
  if (!is.null(new_product)) {
    mode_used  <- "new_product"
    new_vec    <- .parse_product_vec(new_product, p, colnames(X))
    knn_out    <- .knn_predict(X, labels, new_vec, k = k_knn)
    pred_label <- knn_out$predicted
    knn_probs  <- knn_out$vote_proportions
    prod_vec   <- new_vec
    message(sprintf("KNN prediction: '%s' profit tier (k = %d)", pred_label, k_knn))
  } else {
    if (!is.numeric(product_index) || product_index < 1 || product_index > n)
      stop(sprintf("'product_index' must be between 1 and %d.", n))
    product_index <- as.integer(product_index)
    pred_label    <- labels[product_index]
    prod_vec      <- X[product_index, ]
  }
  
  # Pre-compute or reuse centroids
  if (is.null(precomputed_centroids)) {
    unique_labels <- unique(labels)
    centroids     <- lapply(unique_labels, function(lbl)
      colMeans(X[labels == lbl, , drop = FALSE]))
    names(centroids) <- unique_labels
  } else {
    centroids <- precomputed_centroids
  }
  
  dist_to <- sapply(names(centroids), function(lbl)
    sqrt(sum((prod_vec - centroids[[lbl]])^2)))
  
  upgrade_order <- c("Low" = 1, "Mid" = 2, "High" = 3)
  
  if (pred_label == "High") {
    nearest_upgrade      <- "Already High"
    dist_upgrade         <- 0
    upgrade_centroid_vec <- centroids[["High"]]
  } else {
    higher_tiers <- names(upgrade_order)[upgrade_order > upgrade_order[pred_label]]
    higher_tiers <- intersect(higher_tiers, names(centroids))
    if (length(higher_tiers) == 0) {
      nearest_upgrade      <- "Already High"
      dist_upgrade         <- 0
      upgrade_centroid_vec <- centroids[[pred_label]]
    } else {
      d_higher        <- dist_to[higher_tiers]
      nearest_upgrade <- names(which.min(d_higher))
      dist_upgrade    <- min(d_higher)
      upgrade_centroid_vec <- centroids[[nearest_upgrade]]
    }
  }
  
  gap_vec <- upgrade_centroid_vec - prod_vec
  gap_df  <- data.frame(
    feature       = colnames(X),
    current_value = round(as.numeric(prod_vec),             4),
    target_value  = round(as.numeric(upgrade_centroid_vec), 4),
    gap           = round(as.numeric(gap_vec),              4),
    stringsAsFactors = FALSE
  )
  
  actionable_keywords <- c("price", "premium", "luxury",
                           "material_blend", "material_purity", "volume")
  fixed_keywords      <- c("product_type", "gender", "color",
                           "cat_depth", "cat_freq", "newArrival")
  
  is_actionable <- sapply(gap_df$feature, function(f)
    any(sapply(actionable_keywords, function(kw) grepl(kw, f, ignore.case = TRUE))))
  is_fixed <- sapply(gap_df$feature, function(f)
    any(sapply(fixed_keywords, function(kw) grepl(kw, f, ignore.case = TRUE))))
  
  actionable_gap <- gap_df[is_actionable, ]
  actionable_gap <- actionable_gap[order(abs(actionable_gap$gap), decreasing = TRUE), ]
  actionable_gap <- head(actionable_gap, top_n)
  rownames(actionable_gap) <- NULL
  
  context_features <- gap_df[is_fixed, ]
  context_features <- context_features[order(abs(context_features$gap), decreasing = TRUE), ]
  rownames(context_features) <- NULL
  
  mean_actionable_gap <- if (nrow(actionable_gap) > 0)
    mean(abs(actionable_gap$gap), na.rm = TRUE) else 0
  
  verdict <- if (pred_label == "High") {
    "Scale — already in high-profit tier"
  } else if (nearest_upgrade == "High" && mean_actionable_gap < 0.2) {
    "Reposition — easy upgrade to High profit"
  } else if (mean_actionable_gap < 0.4) {
    "Reposition — moderate effort to reach next tier"
  } else {
    "Discontinue — large actionable gap, hard to fix"
  }
  
  return(list(
    current_cluster                 = pred_label,
    nearest_upgrade_cluster         = nearest_upgrade,
    distance_to_upgrade             = round(dist_upgrade, 4),
    feature_gap                     = actionable_gap,
    context_features                = context_features,
    verdict                         = verdict,
    mode                            = mode_used,
    predicted_cluster_probabilities = knn_probs
  ))
}


# internal helpers

#' @keywords internal
.parse_product_vec <- function(new_product, p, feat_names) {
  if (is.data.frame(new_product)) {
    if (nrow(new_product) != 1)
      stop("'new_product' data.frame must have exactly 1 row.")
    new_product <- as.numeric(new_product[1, ])
  } else {
    new_product <- as.numeric(new_product)
  }
  if (length(new_product) != p)
    stop(sprintf("'new_product' has %d values but feature matrix has %d columns.",
                 length(new_product), p))
  if (!is.null(feat_names)) names(new_product) <- feat_names
  new_product[is.na(new_product)] <- 0
  new_product
}

#' @keywords internal
.knn_predict <- function(X_train, y_train, x_new, k = 5) {
  dists   <- apply(X_train, 1, function(row) sqrt(sum((row - x_new)^2)))
  nn_idx  <- order(dists)[seq_len(min(k, length(dists)))]
  nn_labs <- y_train[nn_idx]
  votes   <- table(nn_labs)
  predicted        <- names(which.max(votes))
  vote_proportions <- as.numeric(votes) / sum(votes)
  names(vote_proportions) <- names(votes)
  list(predicted = predicted, vote_proportions = vote_proportions)
}
