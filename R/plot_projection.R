
#' Visualization of a projected voxel cloud.
#'
#' @param data a data.frame or data.table containing the output of the \link{project_voxels} function:
#'             x, y, number of voxels, number of points and ratio of a projected voxel cloud.
#' @param var character. The variable to plot: "nvox" for the number of voxels per pixel, "npts" for
#'            the number of points or "ratio" for the ratio npts/nvox. Default is "nvox".
#' @param th numeric between 0 and 1. A quantile threshold that defines the maximum value of \code{var}
#'           to be plotted. Values > th are replaced by the value of th. Desabled by default.
#' @param palette a color palette to use for plotting.
#'
#' @references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
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
#' project = VoxR::project_voxels(voxels,"xy")
#'
#' #- plot the number of voxels
#' VoxR::plot_projection(project,var = "nvox")
#'
#' #- plot the number of points
#' VoxR::plot_projection(project,var = "npts")
#'
#' #- plot the ratio npts/nvox
#' VoxR::plot_projection(project,var = "ratio")
#'
#' #- plot the number of voxels with different color palette
#' VoxR::plot_projection(project,palette = terrain.colors)
#'
#' #- plot the number of voxels with a 95% percentile threshold
#' VoxR::plot_projection(project,th = 0.95)

plot_projection=function(data,var,th,palette){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  nvox=npts=ratio=quantile=rainbow=':='=NULL

  if(missing(var)) var="nvox" ; main = "number of voxels"

  if(var == "nvox"){data[,var:=nvox] ; main = "number of voxels"}
  if(var == "npts"){data[,var:=npts] ; main = "number of points"}
  if(var == "ratio"){data[,var:=ratio] ; main = "ratio npts/nvox"}

  if(!missing(th)){
    q=quantile(data$var,th)
    data[var>q,var:=q]
  }

  if(missing(palette)){
    col = rev(grDevices::rainbow(max(data$var),end = 4/6))
  }else{
    col = palette(max(data$var))
  }

  xyz = raster::rasterFromXYZ(data[,c(1,2,6)],res = guess_resolution(data,message = FALSE))
  raster::plot(xyz,col=col,main=main)
}
