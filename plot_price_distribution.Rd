\name{plot_price_distribution}
\alias{plot_price_distribution}
\title{Boxplot of price by profit tier}
\usage{
plot_price_distribution(result, original_prices = NULL, title = "Price Distribution by Profit Tier")
}
\arguments{
\item{result}{A \code{retailr_result} object from \code{\link{analyse}}.}
\item{original_prices}{Numeric vector of raw un-scaled prices. If \code{NULL}, the scaled price feature is used.}
\item{title}{Character. Plot title.}
}
\value{
Invisibly returns \code{NULL}. Called for its side effect of drawing a plot.
}
\description{
Side-by-side boxplot comparing price distributions across profit tiers with mean overlaid as a diamond.
}
\examples{
\dontrun{
result <- analyse(prep_data(raw_data))
plot_price_distribution(result)
plot_price_distribution(result, original_prices = raw_data$price)
}
}
\seealso{
\code{\link{plot_clusters}}, \code{\link{save_all_plots}}
}
