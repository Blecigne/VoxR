
#' Clustering of non connected objects in a point cloud.
#'
#' @description Clustering objects with non common points: two points located within a user defined distance from each other are considered
#'              as the parts of a unique object. This function is well suited to be applied to the outputs of the
#'              \link{substract_point_clouds} function.
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud or voxel cloud.
#' @param d_clust numeric. The distance required to consider two points as being part of two different clusters. Default = 0.02.
#' @param method character. The algorithm to use for clustering. Can be either "D_mat" or "Iter", see details. Default = "D_mat".
#' @param C_size (optional) numeric. If \code{method = "Iter"}, sets the maximal size of a cluster (in distance unit).
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @details If \code{method == "D_mat"} the clustering process is based on building a matrix distance. This is time efficient but use a lot of memory.
#'          If \code{method == "Iter"} a slower but memory efficient iterative process is used. In some cases, D_clust can help to speed up the process.
#'
#' @return The input data with an additionnal field containing the cluster ID.
#'
#' @references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
#' @importFrom data.table :=
#' @export
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
#' #- clustering the difference between t0 and t1 with the matrix distance based method
#' clust = VoxR::distance_clustering(diff,d_clust = 0.03)
#'
#' #- plot the result (NOTE that colors are redundant)
#' rgl::open3d()
#' rgl::plot3d(clust,col=clust$cluster,add=TRUE)
#'
#' #- clustering the difference between t0 and t1 with the iterative method
#' clust = VoxR::distance_clustering(diff,d_clust = 0.03,method = "Iter")
#'
#' #- plot the result (NOTE that colors are redundant)
#' rgl::open3d()
#' rgl::plot3d(clust,col=clust$cluster,add=TRUE)
#'
#' #- clustering the difference between t0 and t1 with the iterative method with maximum object size
#' clust = VoxR::distance_clustering(diff,d_clust = 0.03,method = "Iter",C_size = 1)
#'
#' #- plot the result (NOTE that colors are redundant)
#' rgl::open3d()
#' rgl::plot3d(clust,col=clust$cluster,add=TRUE)

distance_clustering = function(data,d_clust,method,C_size,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  dist=cluster=cutree=x=y=z=index=':='=NULL

  if(missing(d_clust)) d_clust = 0.02
  if(missing(method)) method = "D_mat"

  #- check parameters consistancy
  if(is.numeric(d_clust)==FALSE) stop("d_clust must be numeric")
  if(d_clust<=0) stop("d_clust must be greater than 0")

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)

  #- if method == D_mat: build a distance matrix and use hierarchical clustering
  if(method == "D_mat"){
    dspatial <- dist(data[,1:3])
    data[,cluster:=cutree(fastcluster::hclust(dspatial, method = "single"),h=d_clust)]
  }

  #- if method == iter: use an iterative process to cluster data
  if(method == "Iter"){

    if(missing(C_size)){
      C_test = FALSE
    }else{
      if(!is.numeric(C_size)) stop("C_size must be numeric")
      C_test = TRUE
    }

    #- add an index and a cluster field
    data[,index := 1:nrow(data)]
    data[,cluster := 0]

    #- define curent cluster
    cl = 1

    pb <- utils::txtProgressBar(min = 0, max = nrow(data), style = 3,width=33) # progress bar
    while(min(data$cluster) == 0){

      #- keep non classified points
      n_class = data[cluster==0]

      utils::setTxtProgressBar(pb, nrow(data)-nrow(n_class)) # progress bar

      #- points in the cluster (here the root is randomly selected)
      in_clust = n_class[sample(1:nrow(n_class),1),]

      #- keep only the points around the root based on the maximal culster size
      if(C_test) n_class = n_class[which( sqrt( (x-in_clust$x)^2 + (y-in_clust$y)^2 + (z-in_clust$z)^2 ) <= C_size)]

      #- as long as there are points in the curent cluster and points to be classified
      while(nrow(in_clust) > 0 & nrow(n_class)>0){

        #- points in the cluster (here the points that are close to the root or the points from the previous iteration)
        in_clust = n_class[which(FNN::knnx.dist(data = in_clust[,1:3],query=n_class[,1:3],algorithm = "kd_tree",k=1)<=d_clust)]

        #- if neighbors exist
        if(nrow(in_clust) > 0){
          #- add the curent cluster ID
          data[in_clust$index,cluster := cl]
          #- remove the points from the unclassified data
          n_class = n_class[!(index %in% in_clust$index)]
        }else{
          #- if no neighboors -> increment curent cluster ID
          cl = cl+1
        }
      }
    }
   data[,index:=NULL]
  }

  if(check$dfr) data = as.data.frame(data)

  return(data)
}
