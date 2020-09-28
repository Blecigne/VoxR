#' Computes points angle with an axis (X, Y or Z) and the origin of the 3D cartesian coordinates system.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud or voxel cloud.
#' @param axis charecter. Specifying the reference axis to compute the angles: "X", "Y" or "Z".
#' @param project character. If specifyed the point cloud is projected into a 2D plan before computing the angles.
#'                Can be "xy", "yz" or "xz". Default is without projection.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#'  @references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
#' @importFrom data.table :=
#'
#' @return A vector containing the angle values of the points.
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- compute angle with the Z axis
#' tls[,angle_z:=VoxR::axis_angle(tls,axis = "Z")]
#'
#' #- compute angle with the X axis with projection in the xy plan
#' tls[,angle_x:=VoxR::axis_angle(tls,axis = "X",project = "xy")]
#'
#' #- round angle values for visualization
#' tls[,angle_z:=round(angle_z)]
#' tls[,angle_x:=round(angle_x)]
#'
#' #- plot the angle with Z axis
#' cols=rev(rainbow(max(tls$angle_z)+1,end=4/6)) # color scale
#' rgl::open3d()
#' rgl::plot3d(tls,col=cols[tls$angle_z+1],add=TRUE)
#'
#' #- plot the angle with X axis
#' cols=rev(rainbow(max(tls$angle_x)+1,end=4/6)) # color scale
#' rgl::open3d()
#' rgl::plot3d(tls,col=cols[tls$angle_x+1],add=TRUE)

axis_angle = function(data,axis,project,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  x=y=z=angle=':='=NULL

  if(missing(project)){
    project = "ND"
  }else{
    if(!project %in% c("xy","xz","yz")) stop("invalid entry for project.")
  }
  if(missing(axis)) stop("axis not defined")

  #- check parameters consistancy
  if(missing(axis)) stop("axis not defined")

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)
  data=check$data

  #- define parameters depending on the axis
  if(axis == "X"){
    if(project == "yz") stop("Axis = X and projected = yz is an invalid combination,
                             the reference axis must be in the projected plane.")
    ACx = 100
    ACy = 0
    ACz = 0
  }
  if(axis == "Y"){
    if(project == "xz") stop("Axis = Y and projected = xz is an invalid combination,
                             the reference axis must be in the projected plane.")
    ACx = 0
    ACy = 100
    ACz = 0
  }
  if(axis == "Z"){
    if(project == "xy") stop("Axis = Z and projected = xy is an invalid combination,
                             the reference axis must be in the projected plane.")
    ACx = 0
    ACy = 0
    ACz = 100
  }

  #- defines projection
  if(project == "xy") data[,z:=0]
  if(project == "yz") data[,x:=0]
  if(project == "xz") data[,y:=0]

  #- compute angle
  data[,angle:=acos((data$x * ACx + data$y * ACy + data$z * ACz)/
                      (sqrt(data$x^2 + data$y^2 + data$z^2) *
                         sqrt(ACx * ACx + ACy * ACy + ACz * ACz))) * (180/pi)]

  #- if projected -> transform angle range from 0-180 to 0-360
  if(project %in% c("xy","xz","yz")){
    if(axis == "X"){
      if(project == "xy"){
        data[y>0,angle:=360-angle]
      }else{
        data[z>0,angle:=360-angle]
      }
    }
    if(axis == "Y"){
      if(project == "xy"){
        data[x<0,angle:=360-angle]
      }else{
        data[z<0,angle:=360-angle]
      }
    }
    if(axis == "Z"){
      if(project == "xz"){
        data[x<0,angle:=360-angle]
      }else{
        data[y<0,angle:=360-angle]
      }
    }
  }
  return(data$angle)
}
