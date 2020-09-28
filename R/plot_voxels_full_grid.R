#' Voxel cloud visualization when voxel cloud includes the empty voxels.
#' @description Voxel cloud visualization when voxel cloud includes the empty voxels.
#'              Filled voxels are plotted as plain vertices and only the edges of empty voxels are plotted.
#'
#' @param data a data.frame or data.table containing at least the voxel cloud x, y, z coordinates.
#' @param res numeric. The voxel resolution. If not provided, the function will guess it.
#' @param ecol color for the edges of empty voxels.
#' @param fcol color for the facets of filled voxels.
#' @param lwd numeric. The line width for the edges of empty voxels.
#' @param alpha numeric. The transparency of the voxel facets for filled voxels.
#' @param plot logical. Plot the voxels ? See return for mesh capture. Default = TRUE.
#' @param message logical. If FALSE removes the message from the resolution guessing.
#'
#' @return At anytime the mesh object that enables to plot the voxels can be captured to plot it using
#'         the \code{\link[rgl]{shade3d}} function from rgl. The returned object is a list containing
#'         the 3D mesh of filled and empty voxels separately.
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- voxelisation with full.grid option
#' voxels=VoxR::vox(tls,0.3,full.grid = TRUE)
#'
#' #- plot the voxels
#' VoxR::plot_voxels_full_grid(voxels)

plot_voxels_full_grid = function(data,res,ecol,fcol,lwd,alpha,plot,message){

  #- check data is a data.frame or data.table
  if(!(is.data.frame(data))) stop("data must be a data.frame or a data.table")

  if(min(data[,4])>0) stop("data does not contains empty voxels. Use plot_vox instead.")

  #- check res integrity
  if(missing(res)){
    if(missing(message)) message = TRUE
    res = guess_resolution(data,message = message)
  }else{
    #- res must be a vector
    if(!is.vector(res)) stop("res must be a vector of length 1")
    #- res must be numeric
    if(!is.numeric(res)) stop("res must be numeric")
    #- res must be numeric
    if(res<0) stop("res must be positive")
    #- res must be of length 1
    if(length(res)>1){
      res=res[1]
      warning("res contains more than 1 element. Only the first was used")
    }
  }

  #- keep coords
  coord=data.table::data.table(data[,1:3])
  data.table::setnames(coord,c("x","y","z"))

  #- check all coordinates fields are numeric
  if(!(all(sapply(coord,class)=="numeric"))){
    stop("All the coordinates fields of the data must be numeric")
  }
  #- is there any NA ?
  if(any(is.na(coord))) warning("coordinates fields contain missing values.")

  #- default plot
  if(missing(plot)) plot=TRUE

  #- non coords fields
  other=data.table::data.table(data[,4:ncol(data)])

  #- store filled and empty voxels separately
  fill = coord[c(other[,1] > 0)]
  empty = coord[c(other[,1] == 0)]

  #- produce a mesh for each dataset
  fill = plot_voxels(fill,type="n",res=res)
  empty = plot_voxels(empty,type="n",res=res)

  #- default ploting parameters for lines
  if(missing(ecol)){ecol= "black"}
  if(missing(lwd)){lwd= 0.1}

  #- default ploting parameters for plain voxels
  if(missing(fcol)){fcol="green"}
  if(missing(alpha)){alpha=1}

  if(plot){
    rgl::open3d()
    rgl::shade3d(fill$mesh,col=fcol,alpha=alpha,lit=FALSE)
    rgl::wire3d(empty$mesh,col=ecol,lwd=lwd,lit=FALSE)
  }

  #- return a list with mesh and additionnal infos for filled and empty voxels separately
  invisible(list(filled=list(mesh = fill$mesh,additionnal = other[c(other[,1] > 0)]),
                empty = list(mesh = empty$mesh,additionnal = other[c(other[,1] == 0)])))
}
