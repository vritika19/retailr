#' Plot a t-SNE scatter coloured by profit tier
#'
#' Displays all products in the 2-D t-SNE space computed by
#' \code{\link{analyse}}, coloured by their assigned profit tier:
#' green (High), orange (Mid), red (Low).
#'
#' @param result A \code{retailr_result} object from \code{\link{analyse}}.
#' @param title Character. Plot title. Default is
#'   \code{"Product Profit Clusters (t-SNE)"}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   drawing a plot.
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#' plot_clusters(result)
#' plot_clusters(result, title = "Title")
#' }
#'
#' @seealso \code{\link{analyse}}, \code{\link{save_all_plots}}
#' @export
plot_clusters <- function(result,
                          title = "Product Profit Clusters (t-SNE)") {
  if (!inherits(result, "retailr_result"))
    stop("'result' must be a retailr_result object from analyse().")
  
  coords <- result$tsne_coords
  labels <- result$cluster_labels
  cols   <- c("High" = "lightgreen", "Mid" = "orange", "Low" = "tomato")
  
  plot(coords[, 1], coords[, 2],
       col  = cols[labels],
       pch  = 19, cex = 0.5,
       xlab = "t-SNE 1", ylab = "t-SNE 2",
       main = title)
  legend("topright",
         legend = names(cols), col = cols, pch = 19,
         title = "Profit tier", bty = "n")
  invisible(NULL)
}


#' Bar chart of product counts per profit tier
#'
#' Plots a bar chart showing how many products fall into each profit tier
#' (High / Mid / Low), labelled with both the count and the recommended
#' business action (SCALE / REPOSITION / DISCONTINUE).
#'
#' @param result A \code{retailr_result} object from \code{\link{analyse}}.
#' @param title Character. Plot title. Default is
#'   \code{"Product Distribution by Profit Tier"}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   drawing a plot.
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#' plot_cluster_sizes(result)
#' }
#'
#' @seealso \code{\link{plot_clusters}}, \code{\link{save_all_plots}}
#' @export
plot_cluster_sizes <- function(result,
                               title = "Product Distribution by Profit Tier") {
  if (!inherits(result, "retailr_result"))
    stop("'result' must be a retailr_result object from analyse().")
  
  labels        <- result$cluster_labels
  tbl           <- table(factor(labels, levels = c("High", "Mid", "Low")))
  cols          <- c("High" = "lightgreen", "Mid" = "orange", "Low" = "tomato")
  action_labels <- c("High" = "SCALE", "Mid" = "REPOSITION", "Low" = "DISCONTINUE")
  
  bp <- barplot(tbl,
                col = cols[names(tbl)], main = title,
                ylab = "Number of Products", xlab = "Profit Tier",
                ylim = c(0, max(tbl) * 1.2), border = NA)
  text(bp, tbl + max(tbl) * 0.05,
       labels = paste0(tbl, "\n(", action_labels[names(tbl)], ")"),
       cex = 0.9)
  invisible(NULL)
}


#' Boxplot of price by profit tier
#'
#' Draws a side-by-side boxplot comparing price distributions across the three
#' profit tiers. If \code{original_prices} is supplied the y-axis shows real
#' currency values; otherwise the scaled \eqn{[0,1]} price feature is used.
#' Mean price per tier is overlaid as a diamond point.
#'
#' @param result A \code{retailr_result} object from \code{\link{analyse}}.
#' @param original_prices Numeric vector of length \eqn{n} containing the raw
#'   (un-scaled) prices, e.g. \code{raw_data$price}. If \code{NULL} (the
#'   default) the scaled price feature from \code{result$features} is used.
#' @param title Character. Plot title. Default is
#'   \code{"Price Distribution by Profit Tier"}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   drawing a plot.
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#'
#' # With scaled prices
#' plot_price_distribution(result)
#'
#' # With real currency values
#' plot_price_distribution(result, original_prices = raw_data$price)
#' }
#'
#' @seealso \code{\link{plot_clusters}}, \code{\link{save_all_plots}}
#' @importFrom stats boxplot tapply
#' @export
plot_price_distribution <- function(result,
                                    original_prices = NULL,
                                    title = "Price Distribution by Profit Tier") {
  if (!inherits(result, "retailr_result"))
    stop("'result' must be a retailr_result object from analyse().")
  
  labels <- result$cluster_labels
  
  if (is.null(original_prices)) {
    prices    <- result$features[, 1]
    ylab_text <- "Scaled Price (0-1)"
  } else {
    if (length(original_prices) != length(labels))
      stop("'original_prices' must have the same length as the number of products.")
    prices    <- original_prices
    ylab_text <- "Price"
  }
  
  tier_factor <- factor(labels, levels = c("High", "Mid", "Low"))
  cols        <- c("High" = "lightgreen", "Mid" = "orange", "Low" = "tomato")
  
  boxplot(prices ~ tier_factor,
          col = cols, main = title,
          xlab = "Profit Tier", ylab = ylab_text,
          border = "gray", outline = FALSE)
  
  means         <- tapply(prices, tier_factor, mean, na.rm = TRUE)
  ordered_means <- means[c("High", "Mid", "Low")]
  valid         <- !is.na(ordered_means)
  if (any(valid))
    points((1:3)[valid], ordered_means[valid], pch = 18, cex = 1.5, col = "black")
  
  invisible(NULL)
}


#' Heatmap of mean feature values by profit tier
#'
#' Creates a colour coded heatmap where rows are profit tiers (High / Mid / Low)
#' and columns are features. Values are standardised (z-scored) so that
#' relative profiles across tiers are easy to compare regardless of scale.
#' Raw centroid means are printed in each cell.
#'
#' @param result A \code{retailr_result} object from \code{\link{analyse}}.
#' @param title Character. Plot title. Default is
#'   \code{"Feature Profile by Cluster"}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   drawing a plot.
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#' plot_feature_heatmap(result)
#' }
#'
#' @seealso \code{\link{plot_clusters}}, \code{\link{save_all_plots}}
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics axis image text par
#' @export
plot_feature_heatmap <- function(result,
                                 title = "Feature Profile by Cluster") {
  if (!inherits(result, "retailr_result"))
    stop("'result' must be a retailr_result object from analyse().")
  
  cm  <- result$cluster_means
  row_labels <- cm$cluster_label
  cm  <- cm[, colnames(cm) != "cluster_label", drop = FALSE]
  
  if (nrow(cm) < 2)
    stop("plot_feature_heatmap requires at least 2 clusters.")
  if (ncol(cm) < 2)
    stop("plot_feature_heatmap requires at least 2 features.")
  
  row_order  <- match(c("High", "Mid", "Low"), row_labels)
  row_order  <- row_order[!is.na(row_order)]
  cm         <- cm[row_order, , drop = FALSE]
  row_labels <- row_labels[row_order]
  
  mat        <- as.matrix(cm)
  rownames(mat) <- row_labels
  mat_scaled <- scale(mat)
  mat_scaled[is.nan(mat_scaled)] <- 0
  
  color_ramp <- colorRampPalette(c("lightblue", "white", "pink"))(100)
  
  old_par <- par(mar = c(8, 6, 4, 2))
  on.exit(par(old_par))
  
  image(t(mat_scaled[nrow(mat_scaled):1, ]),
        col = color_ramp, axes = FALSE, main = title)
  axis(1, at = seq(0, 1, length.out = ncol(mat)),
       labels = colnames(mat), las = 2, cex.axis = 0.8)
  axis(2, at = seq(0, 1, length.out = nrow(mat)),
       labels = rev(row_labels), las = 1, cex.axis = 0.9)
  
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      y_pos <- 1 - (i - 1) / (nrow(mat) - 1)
      x_pos <- (j - 1) / (ncol(mat) - 1)
      text(x_pos, y_pos, sprintf("%.2f", mat[i, j]), cex = 0.6)
    }
  }
  invisible(NULL)
}


#' Scree plot of PCA variance explained
#'
#' Draws a combined bar and line scree plot: blue bars show the individual
#' proportion of variance explained by each principal component; a red line
#' with labels shows the cumulative variance explained. Up to the first 10
#' components are displayed.
#'
#' @param result A \code{retailr_result} object from \code{\link{analyse}}.
#' @param title Character. Plot title. Default is
#'   \code{"PCA Variance Explained"}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   drawing a plot.
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#' plot_pca_variance(result)
#' }
#'
#' @seealso \code{\link{analyse}}, \code{\link{save_all_plots}}
#' @importFrom graphics barplot lines legend text
#' @export
plot_pca_variance <- function(result,
                              title = "PCA Variance Explained") {
  if (!inherits(result, "retailr_result"))
    stop("'result' must be a retailr_result object from analyse().")
  
  pca_imp  <- result$pca_importance
  n_show   <- min(nrow(pca_imp), 10)
  prop_var <- pca_imp$PropVariance[1:n_show]
  cum_var  <- pca_imp$CumulativeVariance[1:n_show]
  
  old_par <- par(mar = c(5, 4, 4, 4))
  on.exit(par(old_par))
  
  bp <- barplot(prop_var * 100,
                col = "lightblue", main = title,
                ylab = "Variance Explained (%)", xlab = "Principal Component",
                names.arg = paste0("PC", 1:n_show),
                ylim = c(0, 100), border = NA)
  lines(bp, cum_var * 100, type = "b", col = "tomato", lwd = 2, pch = 19)
  text(bp, cum_var * 100 + 5,
       labels = sprintf("%.0f%%", cum_var * 100),
       col = "tomato", cex = 0.7)
  legend("right",
         legend = c("Individual", "Cumulative"),
         col = c("lightblue", "tomato"),
         pch = c(15, 19), lty = c(NA, 1), bty = "n")
  invisible(NULL)
}

