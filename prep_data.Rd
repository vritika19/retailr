\name{prep_data}
\alias{prep_data}
\title{Prepare and encode raw fashion product data into a feature matrix}
\usage{
prep_data(data, price_col = NULL, volume_col = NULL)
}
\arguments{
\item{data}{A \code{data.frame} of raw fashion product records. Every row is one product.}
\item{price_col}{Character. Name of the price column. If \code{NULL}, auto-detected from common keywords.}
\item{volume_col}{Character or \code{NULL}. Name of the sales volume column. If \code{NULL}, auto-detected or skipped.}
}
\value{
A named list with element \code{features}: a numeric matrix with one row per product and one column per encoded feature.
}
\description{
Cleans a raw product data frame and encodes all relevant columns into a numeric feature matrix suitable for \code{\link{analyse}}. Automatically detects price, volume, color, material, gender, category and binary columns.
}
\details{
Encoding steps applied in order:
\enumerate{
  \item \strong{Price}: stripped, median-imputed, log1p transformed, min-max scaled to [0,1].
  \item \strong{Volume} (optional): min-max scaled to [0,1].
  \item \strong{Binary flags}: TRUE/FALSE, yes/no, 1/0 columns one-hot encoded.
  \item \strong{Product name}: last meaningful word frequency-encoded and min-max scaled.
  \item \strong{Colour}: mapped to 11 colour families, normalised to [0,1].
  \item \strong{Gender/Category}: encoded as 0 (female), 0.5 (unisex), 1 (male).
  \item \strong{Premium flag}: description word count > 30 words.
  \item \strong{Materials}: luxury score, blend count and fibre purity normalised.
}
}
\examples{
\dontrun{
raw     <- read.csv("fashion_products.csv", stringsAsFactors = FALSE)
prepped <- prep_data(raw)
dim(prepped$features)
colnames(prepped$features)

prepped <- prep_data(raw, price_col = "selling_price")
prepped <- prep_data(raw, price_col = "mrp", volume_col = "units_sold")
}
}
\seealso{
\code{\link{analyse}}, \code{\link{recommend}}
}
