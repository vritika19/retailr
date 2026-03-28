\name{recommend}
\alias{recommend}
\title{Generates actionable upgrade recommendations for fashion products}
\usage{
recommend(result, product_index = 1L, new_product = NULL, k_knn = 5L, top_n = 5L, precomputed_centroids = NULL)
}
\arguments{
\item{result}{A \code{retailr_result} object from \code{\link{analyse}}.}
\item{product_index}{Integer. Row index of an existing product. Ignored when \code{new_product} is supplied. Default is \code{1}.}
\item{new_product}{Numeric vector or single-row data frame. Supply to classify a brand-new product via kNN. When provided, \code{product_index} is ignored.}
\item{k_knn}{Integer. Number of neighbours for kNN classification. Default is \code{5}.}
\item{top_n}{Integer. Maximum number of actionable feature gaps to return. Default is \code{5}.}
\item{precomputed_centroids}{Named list of centroid vectors. Pass when calling in a loop to avoid recomputing centroids. Default is \code{NULL}.}
}
\value{
A named list: \code{current_cluster}, \code{nearest_upgrade_cluster}, \code{distance_to_upgrade}, \code{feature_gap}, \code{context_features}, \code{verdict}, \code{mode}, \code{predicted_cluster_probabilities}.
}
\description{
For a given product (existing or new), identifies its profit tier, finds the nearest higher-tier centroid, quantifies the feature gap and returns a plain English verdict with actionable suggestions.
}
\details{
Verdict thresholds (based on mean actionable gap):
\itemize{
  \item Already High tier → \emph{Scale — already in high-profit tier}
  \item Gap < 0.2 to High → \emph{Reposition — easy upgrade to High profit}
  \item Gap < 0.4 → \emph{Reposition — moderate effort to reach next tier}
  \item Gap >= 0.4 → \emph{Discontinue — large actionable gap, hard to fix}
}
All features are normalised to [0,1] by \code{prep_data} so gaps are directly comparable.
}
\examples{
\dontrun{
result <- analyse(prep_data(raw_data))

rec <- recommend(result, product_index = 42)
rec$verdict
rec$feature_gap

new_vec <- result$features[1, ]
rec2 <- recommend(result, new_product = new_vec)
rec2$predicted_cluster_probabilities
}
}
\seealso{
\code{\link{analyse}}, \code{\link{prep_data}}
}
