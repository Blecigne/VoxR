% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_noise.R
\name{filter_noise}
\alias{filter_noise}
\title{Statistical filtering of a point cloud.}
\usage{
filter_noise(data, k, sigma, store_noise, message)
}
\arguments{
\item{data}{a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.}

\item{k}{numeric. The number of nearest neighbours to use. Default = 5.}

\item{sigma}{numeric. The multiplier of standard deviation to consider a point as noise. Default = 1.5.}

\item{store_noise}{logical. Should the noisy points be retained ? Default = FALSE.}

\item{message}{logical. If FALSE, messages are disabled. Default = TRUE.}
}
\value{
If \code{store_noise = TRUE} the input data is returned with an additional field ("Noise")
         where points that are classified as noise points are labaled with 2 and the points not classified as noise are labeled as 1.
         If \code{store_noise = FALSE} only the points that were not classified as noise are returned.
}
\description{
Implements the Statistical Outliers Removal (SOR) filter available in
\href{https://www.cloudcompare.org/doc/wiki/index.php?title=SOR_filter}{CloudCompare}.
Computes the distance of each point to its \code{k} nearest neighbours and considers
a point as noise if it is further than the average distance (for the entire point cloud)
plus \code{sigma} times the standard deviation away from other points.
}
\examples{
#- import tls data
tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))

#- run noise filter
clean=VoxR::filter_noise(tls,store_noise = TRUE)

#- plot the result (noise in red)
rgl::open3d()
rgl::plot3d(clean,col=clean$Noise,add=TRUE)
}
