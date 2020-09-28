#' Computes points distance to an axis of the cartesian coordinates system.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud or voxel cloud.
#' @param axis charecter. Specifying the reference axis to compute the distance: "X", "Y" or "Z".
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return A vector containing the distance values of the points
#'
#'@references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- compute distance to the Z axis
#' tls[,dist:=VoxR::axis_distance(tls,"Z")]
#'
#' #- round distance values for visualization
#' tls[,dist:=round(dist*100)]
#'
#' #- plot the distance to the Z axis
#' cols=rev(rainbow(max(tls$dist)+1,end=4/6)) # color scale
#' rgl::open3d()
#' rgl::plot3d(tls,col=cols[tls$dist+1],add=TRUE)

axis_distance <- function(data,axis,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  dist=':='=NULL

  if(missing(axis)) stop("axis not defined.")

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)
  data=check$data
  if(axis == "X"){
    data[,dist:=sqrt( data$y^2 + data$z^2 )]
  }
  if(axis == "Y"){
    data[,dist:=sqrt( data$x^2 + data$z^2 )]
  }
  if(axis == "Z"){
    data[,dist:=sqrt( data$x^2 + data$y^2 )]
  }

  return(data$dist)
}
