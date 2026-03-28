#' retailr: Fashion Product Segmentation and Profit-Tier Analysis
#'
#' @description
#' \pkg{retailr} provides an end-to-end pipeline for segmenting fashion
#' retail products into profit tiers using a combination of PCA, t-SNE,
#' cross-validated K-means clustering, and kNN classification.
#'
#' @section Main workflow:
#' \enumerate{
#'   \item \strong{\code{\link{prep_data}}} — Clean raw product data and encode
#'     it into a numeric feature matrix. Handles price, colour, materials,
#'     category, gender, binary flags, and description text automatically.
#'   \item \strong{\code{\link{analyse}}} — Run PCA + t-SNE + CV-validated
#'     K-means to assign every product a profit tier label
#'     (\code{"High"}, \code{"Mid"}, or \code{"Low"}).
#'   \item \strong{\code{\link{recommend}}} — For any product (existing or
#'     new), compute the feature gap to the nearest higher tier and return a
#'     business verdict (Scale / Reposition / Discontinue).
#'   \item \strong{\code{\link{save_all_plots}}} — Export all five diagnostic
#'     visualisations to PNG in one call.
#' }
#'
#' @section Statistical techniques used:
#' All techniques are drawn from the course curriculum:
#' \itemize{
#'   \item \strong{PCA} — principal component analysis for dimensionality
#'     reduction and feature importance ranking.
#'   \item \strong{t-SNE} — non-linear 2D embedding for cluster visualisation
#'     (via \pkg{Rtsne}).
#'   \item \strong{K-means} — partitional clustering with \code{nstart = 25}
#'     for stable solutions.
#'   \item \strong{Cross-validation} — 5 fold CV to report out-of-sample WCSS
#'     as a cluster quality metric.
#'   \item \strong{kNN} — k-nearest-neighbour classification for predicting the
#'     tier of brand-new products not seen during clustering.
#' }
#'
#' @section Quick start:
#' \preformatted{
#' library(retailr)
#'
#' raw     <- read.csv("fashion_products.csv", stringsAsFactors = FALSE)
#' prepped <- prep_data(raw)
#' result  <- analyse(prepped)          # PCA -> t-SNE -> CV K-means
#' print(result)                        # tier summary
#'
#' recommend(result, product_index = 1) # upgrade path for product 1
#' save_all_plots(result, original_prices = raw$price)
#' }
#'
#' @author Vritika, \email{vritika25@@iitk.ac.in}
#' @name retailr-package
#' @aliases retailr
"_PACKAGE"
