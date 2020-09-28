
#' Computes fractal dimension using the box counting method.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.
#' @param min_vox_size numeric. The minimum size of a voxel. Default = 0.01.
#' @param store_fit logical. If TRUE, the parameters linear model's fit are returned. Default = FALSE.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return If \code{store_fit = FALSE} only the fractal dimension is returned. If \code{store_fit = TRUE}
#'                             the parameters of the linear model used to estimate the fractal dimension and
#'                             a table containing the number of boxes (i.e. voxels) at any resolution are
#'                             returned in a list in addition to the fractal dimension.
#'
#' @references  Seidel, D. (2018). A holistic approach to determine tree structural complexity based on laser
#'              scanning data and fractal analysis. Ecology and evolution, 8(1), 128-134.
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- box counting
#' FD = VoxR::box_counting(tls,store_fit = TRUE)
#'
#' #- fractal dimension
#' FD$fractal_dim
#' #- linear model fit
#' FD$fit_summary
#' #- plot fit
#' plot(log(FD$fit_table$N)~log(1/FD$fit_table$res))
#' abline(FD$fit_summary)

box_counting = function(data,min_vox_size,store_fit,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  x=y=z=x2=y2=z2=.=':='=NULL

  if(missing(min_vox_size)) min_vox_size = 0.01
  if(missing(store_fit)) store_fit = FALSE

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)

  #- check parametes consistancy
  if(is.numeric(min_vox_size)==FALSE) stop("min_vox_size must be numeric")
  if(is.logical(store_fit)==FALSE) stop("store_fit must be TRUE or FALSE")

  #- convert data
  data=check$data
  data.table::setnames(data,c("x","y","z"))

  #- size and cebter of the bounding box
  bounding_box_size = c(max(data$x)-min(data$x),max(data$y)-min(data$y),max(data$z)-min(data$z))
  bounding_box_center = c((max(data$x)+min(data$x))/2,(max(data$y)+min(data$y))/2,(max(data$z)+min(data$z))/2)

  #- initial voxel resolution
  res = max(bounding_box_size)

  #- recenter the bounding box to 0 to ensure only one voxel in produced at iteration 1
  data[,":="(x=x-bounding_box_center[1],y=y-bounding_box_center[2],z=z-bounding_box_center[3])]

  if(min_vox_size>res) stop("min_box_size is larger than the tree bounding box.")

  #- data.frame to store the number of voxels for each voxel resolution
  temp = data.frame(NA,NA) ; temp=temp[-1,] ; names(temp) = c("res","N")

  while(res > min_vox_size){
    #- voxelize data
    data[,':='(x2=round(x/res)*res,y2=round(y/res)*res,z2=round(z/res)*res)]
    vox = unique(data[,.(x2,y2,z2)])

    #- number of voxels
    N=nrow(vox)

    #- store the results
    temp[nrow(temp)+1,1:2] = c(res,N)

    #- decrease res
    res=res/2
  }

  #- compute fractal dimension as the slope of log(number of voxels)~log(1/resolution)
  lm = summary(lm(log(temp$N) ~ log(1/temp$res)))

  if(store_fit){
    return(list(fractal_dim=lm$coefficients[2,1],fit_summary=lm,fit_table=temp))
  }else{
    return(lm$coefficients[2,1])
  }
}


