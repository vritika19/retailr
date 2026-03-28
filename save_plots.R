#' Export all five diagnostic plots as PNG files
#'
#' Saves the full set of \pkg{retailr} visualisations in one call:
#' the t-SNE cluster scatter, tier size bar chart, price distribution boxplot,
#' feature heatmap and PCA scree plot. Each plot is written as a PNG file.
#' If a plot fails (e.g. because only one cluster exists), the error is caught,
#' reported as a message and the remaining plots are still saved.
#'
#' @param result A \code{retailr_result} object from \code{\link{analyse}}.
#' @param original_prices Numeric vector of raw (un-scaled) prices passed
#'   through to \code{\link{plot_price_distribution}}. If \code{NULL} (default)
#'   the scaled price feature is used on the y-axis.
#' @param output_dir Character. Directory where PNG files are saved. Created
#'   recursively if it does not exist. Default is \code{"."} (current working
#'   directory).
#' @param prefix Character. File name prefix for all output PNGs. Default is
#'   \code{"fashion_segment"}. Files will be named, e.g.,
#'   \code{fashion_segment_tsne_clusters.png}.
#' @param width Integer. Width of each PNG in pixels. Default is \code{800}.
#' @param height Integer. Height of each PNG in pixels. Default is \code{600}.
#'
#' @return Invisibly returns a character vector of the file paths that were
#'   successfully saved (failed plots are excluded).
#'
#' @details
#' The five plots saved are:
#' \enumerate{
#'   \item \code{<prefix>_tsne_clusters.png} — t-SNE scatter by tier.
#'   \item \code{<prefix>_cluster_sizes.png} — bar chart of tier sizes.
#'   \item \code{<prefix>_price_distribution.png} — boxplot of price by tier.
#'   \item \code{<prefix>_feature_heatmap.png} — centroid heatmap.
#'   \item \code{<prefix>_pca_variance.png} — scree plot.
#' }
#'
#' @examples
#' \dontrun{
#' result <- analyse(prep_data(raw_data))
#'
#' # Save to current directory
#' save_all_plots(result)
#'
#' # Save to a sub-folder with real prices on the y-axis
#' save_all_plots(result,
#'                original_prices = raw_data$price,
#'                output_dir      = "output/plots",
#'                prefix          = "my_analysis")
#' }
#'
#' @seealso \code{\link{plot_clusters}}, \code{\link{plot_cluster_sizes}},
#'   \code{\link{plot_price_distribution}}, \code{\link{plot_feature_heatmap}},
#'   \code{\link{plot_pca_variance}}
#' @importFrom grDevices png dev.off
#' @export
save_all_plots <- function(result,
                           original_prices = NULL,
                           output_dir      = ".",
                           prefix          = "fashion_segment",
                           width           = 800,
                           height          = 600) {
  if (!inherits(result, "retailr_result"))
    stop("'result' must be a retailr_result object from analyse().")
  
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  
  plots <- list(
    list(name = "tsne_clusters",      fn = function() plot_clusters(result)),
    list(name = "cluster_sizes",      fn = function() plot_cluster_sizes(result)),
    list(name = "price_distribution", fn = function() plot_price_distribution(result, original_prices)),
    list(name = "feature_heatmap",    fn = function() plot_feature_heatmap(result)),
    list(name = "pca_variance",       fn = function() plot_pca_variance(result))
  )
  
  saved_files <- character(length(plots))
  
  for (i in seq_along(plots)) {
    fname <- file.path(output_dir, paste0(prefix, "_", plots[[i]]$name, ".png"))
    png(fname, width = width, height = height, res = 100)
    err <- tryCatch({ plots[[i]]$fn(); NULL }, error = function(e) e)
    dev.off()
    if (!is.null(err)) {
      message(sprintf("ERROR in %s: %s -- skipped.", plots[[i]]$name, err$message))
      file.remove(fname)
    } else {
      saved_files[i] <- fname
      message(sprintf("Saved: %s", fname))
    }
  }
  invisible(saved_files[nchar(saved_files) > 0])
}
