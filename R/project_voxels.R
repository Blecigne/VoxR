
#' Project a voxel cloud in a 2D plan formed by two axes of the cartesian coordiantes system.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a voxel cloud.
#' @param plan character. Defines the projection plan: "xy", "xz" or "yz". Default = "xy".
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return A data frame of a 2D point cloud containing : x, y coordinates of the pixels and the
#' number of voxels (nvox), number of points (npts), ratio npts/nvox contained in each pixel.
#'
#' @references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
#'
#' @importFrom data.table .N :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- voxelisation
#' voxels = VoxR::vox(tls,0.05)
#'
#' #- project into the xy plan
#' project_xy = VoxR::project_voxels(voxels,"xy")
#' VoxR::plot_projection(project_xy) # plot projection
#'
#' #- project into the xz plan
#' project_xy = VoxR::project_voxels(voxels,"xz")
#' VoxR::plot_projection(project_xy) # plot projection

project_voxels = function(data,plan,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  x=y=z=npts=ratio=nvox=.N=.=':='=NULL

  if(missing(plan)) plan = "xy"

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_vox(data,message)
  data=check$data

  if(plan == "xy"){
    proj=data[,.(nvox = .N,npts=sum(npts)),by=.(x,y)]
  }
  if(plan == "xz"){
    proj=data[,.(nvox = .N,npts=sum(npts)),by=.(x,z)]
  }
  if(plan == "yz"){
    proj=data[,.(nvox = .N,npts=sum(npts)),by=.(y,z)]
  }

  proj[,ratio:=npts/nvox]
  data.table::setnames(proj,c("x","y","nvox","npts","ratio"))

  return(proj)
}
