#' Point clouds substraction: identification of changes between two measuring times.
#'
#' @description Identify the point that are unique to one of two point clouds
#'              to detect the changes that occured between two measuring times (e.g. growth,
#'              branches losses, branch motion). Two methods are available (see details).
#'
#'
#' @param t0 a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud or voxel cloud
#'           acquired at time 0.
#' @param t1 a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud or voxel cloud
#'           acquired at time 1.
#' @param method character. The method to use to identify the difference between t0 and t1. Can be either "hull" or
#'               "distance", see details.
#' @param dist numeric. The threshold distance to consider a point is unique to t1 if \code{method = "distance"}.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @details If \code{method = "hull"}, the convex hull that wraps t0 is constructed and the difference between t1 and t0 are
#'          the points outside the convex hull. If \code{method = "distance"}, the points in t1 that are distant (i.e. further than
#'          \code{dist}) from the points in t0 are returned.
#'
#' @note \code{t0} and \code{t1} must be registered in the same coordinates system.
#'
#'  @references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
#' @return a data.frame or data.table containing the x, y, z, ... coordinates of points that are unique to t1.
#'
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
#' ####- substract t0 to t1 with the hull method
#' diff = VoxR::substract_point_clouds(t0 = t0,t1 = t1, method = "hull")
#' #- plot the result (t0 in black, the difference between t1 and t0 in red)
#' rgl::open3d()
#' rgl::plot3d(t0,add=TRUE)
#' rgl::plot3d(diff,col="red",add=TRUE)
#'
#' ####- substract t0 to t1 with the distance based method
#' diff = substract_point_clouds(t0 = t0,t1 = t1, method = "distance",dist = 0.1)
#' #- plot the result (t0 in black, the difference between t1 and t0 in red)
#' rgl::open3d()
#' rgl::plot3d(t0,add=TRUE)
#' rgl::plot3d(diff,col="red",add=TRUE)

substract_point_clouds = function(t0,t1,method,dist,message){

  #- default method
  if(missing(method)) method = "hull"
  #- default distance
  if(missing(dist)) dist = 0.05

  #- check for data consistancy and convert to data.table
  check_t0=VoxR::ck_conv_dat(t0,message)
  check_t1=VoxR::ck_conv_dat(t1,message)

  #- check parameters consistancy
  if(is.numeric(dist)==FALSE) stop("threshold must be numeric")
  if(dist<=0) stop("threshold and sigma must be greater than 0")
  if(!method %in% c("distance","hull")) stop("distance must be either distance or hull")

  t0=check_t0$data
  t1=check_t1$data

  if(method == "distance"){
    #- computes distance of points in t1 to the nearest neighbours in t0 and keep thoose with distance >= dist
    return(t1[which(FNN::knnx.dist(data= as.matrix(t0[,1:3]),query=as.matrix(t1[,1:3]),algorithm = "kd_tree",k=1)>=dist),])
  }

  if(method == "hull"){
    #- computes the convex hull that wraps t0 and return points of t1 outside the hull
    return(t1[!geometry::inhulln(geometry::convhulln(t0),as.matrix(t1[,1:3])),])
  }
}
