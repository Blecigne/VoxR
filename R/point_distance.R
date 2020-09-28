#' Computes the distance of a set of points to a user defined point.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud or voxel cloud.
#' @param point a vector of length 3 containing the x, y and z coordintes of the reference point.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
#' @return A vector containing the distance values of the points.
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- compute distance to the crown centre
#' tls[,dist:=VoxR::point_distance(tls,c(mean(x),mean(y),mean(z)))]
#'
#' #- round distance values for visualization
#' tls[,dist:=round(dist*100)]
#'
#' #- plot the distance to crown centre
#' cols=rev(rainbow(max(tls$dist)+1,end=4/6)) # color scale
#' rgl::open3d()
#' rgl::plot3d(tls,col=cols[tls$dist+1],add=TRUE)

point_distance <- function(data,point,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  dist=':='=NULL

  if(missing(point)) stop("No point defined.")
  if(length(point)!=3) stop("point must of length 3.")
  if(!all(is.numeric(point))) stop("point must be numeric.")

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)
  data=check$data

  data[,dist:=sqrt( (data$x - point[1])^2 + (data$y - point[2])^2 + (data$z - point[3])^2)]

  return(data$dist)
}
