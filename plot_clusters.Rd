\name{plot_clusters}
\alias{plot_clusters}
\title{Plot a t-SNE scatter coloured by profit tier}
\usage{
plot_clusters(result, title = "Product Profit Clusters (t-SNE)")
}
\arguments{
\item{result}{A \code{retailr_result} object from \code{\link{analyse}}.}
\item{title}{Character. Plot title. Default is \code{"Product Profit Clusters (t-SNE)"}.}
}
\value{
Invisibly returns \code{NULL}. Called for its side effect of drawing a plot.
}
\description{
Displays all products in the 2-D t-SNE space, coloured by profit tier: green (High), orange (Mid), red (Low).
}
\examples{
\dontrun{
result <- analyse(prep_data(raw_data))
plot_clusters(result)
}
}
\seealso{
\code{\link{analyse}}, \code{\link{save_all_plots}}
}
