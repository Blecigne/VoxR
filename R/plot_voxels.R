#' Voxel cloud visualization.
#'
#' @param data a data.frame or data.table containing at least the voxel cloud x, y, z coordinates.
#' @param res numeric. The voxel resolution. If not provided, the function will guess it.
#' @param type character. How to represent a voxel ? If "w" only the voxel hedges are plotted,
#'             if "p" plain voxels are plotted, if "b" both hedges and plain voxels are plotted. Default = "b".
#' @param lcol the line color for \code{type} = "w" or "b".
#' @param fcol the facets color for \code{type} = "p" or "b".
#' @param lwd numeric. The line width for \code{type} = "w" or "b".
#' @param alpha numeric. The transparency of the voxel faces for \code{type} = "p" or "b".
#' @param plot logical. Plot the voxels ? See return for mesh capture. Default = TRUE.
#' @param message logical. Removes the message from the resolution guessing. Default = FALSE.
#'
#' @return If \code{plot = TRUE}, the 3D plot of voxels is plotted. At anytime the mesh object that enables
#'         to plot the voxels can be captured and to be plotted using the
#'         \code{\link[rgl]{shade3d}} function from rgl.
#'         The returned object is a list containing the 3D mesh of the voxel cloud and all additionnal fields
#'         of the input data.
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- voxelisation
#' voxels = VoxR::vox(tls,res=0.05)
#'
#' #- plot the voxels
#' VoxR::plot_voxels(voxels)
#'
#' #- capture the voxels mesh to plot with color scale
#' ###- number of points in the voxel
#' voxels_mesh = VoxR::plot_voxels(voxels,plot = FALSE) # capture the mesh
#' colors=rev(rainbow(max(voxels_mesh$additionnal$npts),end=4/6)) # color scale
#' rgl::open3d()
#' rgl::shade3d(voxels_mesh$mesh,col=colors[round(voxels_mesh$additionnal$npts)]
#'              ,lit=FALSE,alpha=0.5) # plot
#'
#' ###- distance from the crow center
#' # compute distnce
#' voxels[,distance:=round(VoxR::point_distance(voxels[,1:3],c(mean(x),mean(y),mean(z)))*100)]
#' voxels_mesh = VoxR::plot_voxels(voxels,plot = FALSE) # capture mesh
#' cols=rev(rainbow(max(voxels_mesh$additionnal$distance),end=4/6)) # color scale
#' rgl::open3d()
#' rgl::shade3d(voxels_mesh$mesh,col=cols[round(voxels_mesh$additionnal$distance)]
#'              ,lit=FALSE,alpha=0.5) # plot

plot_voxels=function(data,res,type,lcol,fcol,lwd,alpha,plot,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  x=y=z=k=.=':='=NULL

  #- check data is a data.frame or data.table
  if(!(is.data.frame(data))) stop("data must be a data.frame or a data.table")

  #- store coordinates and other fields separately
  if(ncol(data)>3){
    coord=data.table::data.table(data[,1:3])
    data.table::setnames(coord,c("x","y","z"))
    other=data.table::data.table(data[,4:ncol(data)])
  }else{
    coord=data.table::data.table(data[,1:3])
  }

  #- check all coordinates fields are numeric
  if(!(all(sapply(coord,class)=="numeric"))){
    stop("All the coordinates fields of the data must be numeric")
  }
  #- is there any NA ?
  if(any(is.na(coord))) warning("coordinates fields contain missing values.")

  #- check res integrity
  if(missing(res)){
    if(missing(message)) message = FALSE
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

  #- default voxel ploting mode
  if(missing(type)) type = "b"

  #- default is plotted
  if(missing(plot)) plot = TRUE


  mesh=rgl::cube3d() #- the original mesh to duplicate

  nr= nrow(data) #- how many voxels ?

  vb=t(mesh$vb) #- the vertices for one voxel
  ib=t(mesh$ib) #- the vertices index forone voxel

  vb=vb*res/2 # scaling the voxel size

  #- duplicate the vertices coordinates nr times
  mat_vb = data.table::data.table(x=rep(vb[,1],nr),
                                  y=rep(vb[,2],nr),
                                  z=rep(vb[,3],nr),
                                  res=rep(vb[,4],nr))


  #- translate the voxel: repeate 8 times the data coordinates
  mat_vb[,':='(
    x=x+rep(coord$x,each=8),
    y=y+rep(coord$y,each=8),
    z=z+rep(coord$z,each=8)
  )]

  #- duplicate the vertices index
  mat_ib = data.table::data.table(x=rep(ib[,1],nr),
                                  y=rep(ib[,2],nr),
                                  z=rep(ib[,3],nr),
                                  k=rep(ib[,4],nr))

  #- the string to correctly update the vertices index
  seq_ib = rep(seq(0,(nr-1)*8,8),each=6)

  #- update the vertices indexes for each voxel
  mat_ib[,':='(x=x+seq_ib,y=y+seq_ib,z=z+seq_ib,k=k+seq_ib)]

  #- produce the final mesh
  final_mesh = rgl::qmesh3d(t(as.matrix(mat_vb)),t(as.matrix(mat_ib)))

  if(ncol(data)>=4){ # export mesh and other infos separately
    other = other[rep(c(1:nrow(data)),each=8)] #- replicate other columns 8 times
    out = list(mesh = final_mesh, additionnal = other)
  }else{
    out = list(mesh = final_mesh)
  }

  if(plot){
    if(type=="w"){
      #- default ploting parameters for lines
      if(missing(lcol)){lcol= "black"}
      if(missing(lwd)){lwd= 0.5}

      if(!missing(fcol)) print("NOTE: fcol was not used (for plain voxels only)")
      if(!missing(alpha)) print("NOTE: alpha was not used (for plain voxels only)")

      rgl::open3d()
      rgl::wire3d(final_mesh,col=lcol,lwd=lwd,lit=FALSE)
    }
    if(type=="p"){
      #- default ploting parameters for plain voxels
      if(missing(fcol)){fcol="green"}
      if(missing(alpha)){alpha=1}

      if(!missing(lcol)) print("NOTE: fcol was not used (for wired voxels only)")
      if(!missing(lwd)) print("NOTE:lwd was not used (for wired voxels only)")

      rgl::open3d()
      rgl::shade3d(final_mesh,col=fcol,alpha=alpha,lit=FALSE)
    }
    if(type=="b"){
      #- default ploting parameters for lines
      if(missing(lcol)){lcol= "black"}
      if(missing(lwd)){lwd= 0.1}

      #- default ploting parameters for plain voxels
      if(missing(fcol)){fcol="green"}
      if(missing(alpha)){alpha=1}

      rgl::open3d()
      rgl::shade3d(final_mesh,col=fcol,alpha=alpha,lit=FALSE)
      rgl::wire3d(final_mesh,col=lcol,lwd=lwd,lit=FALSE)
    }
  }
  invisible(out)
}

