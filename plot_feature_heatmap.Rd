\name{plot_feature_heatmap}
\alias{plot_feature_heatmap}
\title{Heatmap of mean feature values by profit tier}
\usage{
plot_feature_heatmap(result, title = "Feature Profile by Cluster")
}
\arguments{
\item{result}{A \code{retailr_result} object from \code{\link{analyse}}.}
\item{title}{Character. Plot title. Default is \code{"Feature Profile by Cluster"}.}
}
\value{
Invisibly returns \code{NULL}. Called for its side effect of drawing a plot.
}
\description{
Colour-coded heatmap where rows are profit tiers and columns are features. Values are z-scored for comparability. Raw centroid means printed in each cell.
}
\examples{
\dontrun{
result <- analyse(prep_data(raw_data))
plot_feature_heatmap(result)
}
}
\seealso{
\code{\link{plot_clusters}}, \code{\link{save_all_plots}}
}
