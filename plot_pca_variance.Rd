\name{plot_pca_variance}
\alias{plot_pca_variance}
\title{Scree plot of PCA variance explained}
\usage{
plot_pca_variance(result, title = "PCA Variance Explained")
}
\arguments{
\item{result}{A \code{retailr_result} object from \code{\link{analyse}}.}
\item{title}{Character. Plot title. Default is \code{"PCA Variance Explained"}.}
}
\value{
Invisibly returns \code{NULL}. Called for its side effect of drawing a plot.
}
\description{
Combined bar and line scree plot: blue bars show individual variance explained per PC; red line shows cumulative variance. Up to 10 components displayed.
}
\examples{
\dontrun{
result <- analyse(prep_data(raw_data))
plot_pca_variance(result)
}
}
\seealso{
\code{\link{analyse}}, \code{\link{save_all_plots}}
}
