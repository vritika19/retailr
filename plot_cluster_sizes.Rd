\name{plot_cluster_sizes}
\alias{plot_cluster_sizes}
\title{Bar chart of product counts per profit tier}
\usage{
plot_cluster_sizes(result, title = "Product Distribution by Profit Tier")
}
\arguments{
\item{result}{A \code{retailr_result} object from \code{\link{analyse}}.}
\item{title}{Character. Plot title. Default is \code{"Product Distribution by Profit Tier"}.}
}
\value{
Invisibly returns \code{NULL}. Called for its side effect of drawing a plot.
}
\description{
Bar chart showing product counts per profit tier, labelled with counts and recommended business actions (SCALE / REPOSITION / DISCONTINUE).
}
\examples{
\dontrun{
result <- analyse(prep_data(raw_data))
plot_cluster_sizes(result)
}
}
\seealso{
\code{\link{plot_clusters}}, \code{\link{save_all_plots}}
}
