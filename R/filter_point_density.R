#' Retains one point of the original point cloud within a voxel of given size.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.
#' @param res numeric. The voxel resolution.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return a data.frame or data.table with reduced point sensity.
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- keep one point in 2cm voxels
#' filtered=VoxR::filter_point_density(tls,0.02)
#' rgl::open3d()
#' rgl::plot3d(filtered,add=TRUE)
#'
#' #- keep one point in 10cm voxels
#' filtered=VoxR::filter_point_density(tls,0.1)
#' rgl::open3d()
#' rgl::plot3d(filtered,add=TRUE)

filter_point_density = function(data,res,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  x=y=z=.=':='=NULL

  if(missing(res)) res = 0.01

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)

  #- check parameters consistancy
  if(is.numeric(res)==FALSE) stop("res must be numeric")
  if(res<=0) stop("res must be greater than 0")

  temp_dat = check$data[,1:3]
  #data.table::setnames(temp_dat,c("X","Y","Z"))

  #- create a voxel cloud with an index
  temp_dat[,':='(X_vox = round(x/res)*res,
                 Y_vox = round(y/res)*res,
                 Z_vox = round(z/res)*res,
                 row = 1:nrow(temp_dat))]

  #- for each voxel keep the point with the greater index
  temp <- temp_dat[,.(to_keep = max(row)), by = 'X_vox,Y_vox,Z_vox']
  rm(temp_dat)

  #- keep the selected points from the original dataset
  data <- data[temp$to_keep,]

  #- remove useless dataframe and columns
  rm(temp)

  if(check$dfr) data = as.data.frame(data)

  return(data)
}
