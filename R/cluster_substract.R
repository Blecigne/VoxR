
#' Clustering of non connected objects in a point cloud.
#'
#' @description Clustering objects with non common points: two points located within a user defined distance from each other are considered
#'              as the parts of a unique object. This function is well suited to be applied to the outputs of the
#'              \link{substract_point_clouds} function.
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud or voxel cloud.
#' @param d_clust numeric. The distance required to consider two points as being part of two different clusters. Default = 0.02.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return The input data with an additionnal field containing the cluster ID.
#'
#' @importFrom data.table :=
#'
#'
#' @examples
#' #- import datasets
#' t0=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#' t1=data.table::fread(system.file("extdata", "Tree_t1.asc", package="VoxR"))
#'
#' #- keep only the tree crown
#' t0 = t0[z>=0,]
#' t1 = t1[z>=0,]
#'
#' #- substract t0 to t1 with the hull method
#' diff = VoxR::substract_point_clouds(t0 = t0,t1 = t1, method = "hull")
#'
#' #- clustering the difference between t0 and t1
#' clust = VoxR::distance_clustering(diff,d_clust = 0.03)
#'
#' #- plot the result (NOTE that colors are redundant)
#' rgl::open3d()
#' rgl::plot3d(clust,col=clust$cluster,add=TRUE)

distance_clustering = function(data,d_clust,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  dist=cluster=cutree=':='=NULL

  if(missing(d_clust)) d_clust = 0.02

  #- check parameters consistancy
  if(is.numeric(d_clust)==FALSE) stop("d_clust must be numeric")
  if(d_clust<=0) stop("d_clust must be greater than 0")

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)

  dspatial <- dist(data[,1:3])
  data[,cluster:=cutree(fastcluster::hclust(dspatial, method = "single"),h=d_clust)]

  if(check$dfr) data = as.data.frame(data)

  return(data)
}
