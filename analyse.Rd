\name{analyse}
\alias{analyse}
\title{Segments fashion products using PCA, t-SNE and cross-validated K-means}
\usage{
analyse(prepped, n_clusters = NULL, n_pca_components = 10, tsne_perplexity = 30, seed = 42)
}
\arguments{
\item{prepped}{A list returned by \code{\link{prep_data}}, containing a numeric matrix in \code{prepped$features}.}
\item{n_clusters}{Integer. Number of clusters (profit tiers). Default is \code{3}.}
\item{n_pca_components}{Integer. Maximum number of principal components to retain. Default is \code{10}, auto-capped if too large.}
\item{tsne_perplexity}{Numeric. t-SNE perplexity. Auto-restricted to avoid errors on small datasets. Default is \code{30}.}
\item{seed}{Integer. Random seed for reproducibility. Default is \code{42}.}
}
\value{
An object of class \code{"retailr_result"} containing: \code{clusters}, \code{cluster_labels}, \code{cluster_means}, \code{pca}, \code{pca_scores}, \code{pca_importance}, \code{top_features}, \code{tsne_coords}, \code{cv_wcss}, \code{n_clusters}, \code{features}, \code{km}, \code{n_pca}.
}
\description{
Runs the full unsupervised segmentation pipeline: PCA for dimensionality reduction, t-SNE for 2-D visualisation, and 5-fold cross-validated K-means to assign every product a profit tier label (High, Mid, or Low).
}
\details{
Clusters are labelled by comparing mean price per cluster. K = 3 is the default as High/Mid/Low is a domain requirement for fashion retail. The CV step validates the chosen K by reporting out-of-sample WCSS.
}
\examples{
\dontrun{
raw     <- read.csv("fashion_products.csv", stringsAsFactors = FALSE)
prepped <- prep_data(raw)
result  <- analyse(prepped)
print(result)

result4 <- analyse(prepped, n_clusters = 4)
}
}
\seealso{
\code{\link{prep_data}}, \code{\link{recommend}}, \code{\link{plot_clusters}}, \code{\link{save_all_plots}}
}
