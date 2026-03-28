\name{save_all_plots}
\alias{save_all_plots}
\title{Export all five diagnostic plots as PNG files}
\usage{
save_all_plots(result, original_prices = NULL, output_dir = ".", prefix = "fashion_segment", width = 800, height = 600)
}
\arguments{
\item{result}{A \code{retailr_result} object from \code{\link{analyse}}.}
\item{original_prices}{Numeric vector of raw prices passed to \code{\link{plot_price_distribution}}. Default is \code{NULL}.}
\item{output_dir}{Character. Directory to save PNGs. Created if it does not exist. Default is \code{"."}.}
\item{prefix}{Character. File name prefix. Default is \code{"fashion_segment"}.}
\item{width}{Integer. PNG width in pixels. Default is \code{800}.}
\item{height}{Integer. PNG height in pixels. Default is \code{600}.}
}
\value{
Invisibly returns a character vector of successfully saved file paths.
}
\description{
Saves all five retailr visualisations in one call: t-SNE cluster scatter, tier size bar chart, price distribution boxplot, feature heatmap, and PCA scree plot.
}
\examples{
\dontrun{
result <- analyse(prep_data(raw_data))
save_all_plots(result)
save_all_plots(result, original_prices = raw_data$price, output_dir = "output/plots")
}
}
\seealso{
\code{\link{plot_clusters}}, \code{\link{plot_cluster_sizes}}, \code{\link{plot_price_distribution}}, \code{\link{plot_feature_heatmap}}, \code{\link{plot_pca_variance}}
}
