#' Statistical filtering of a point cloud.
#'
#' @description Implements the Statistical Outliers Removal (SOR) filter available in
#' \href{https://www.cloudcompare.org/doc/wiki/index.php?title=SOR_filter}{CloudCompare}.
#' Computes the distance of each point to its \code{k} nearest neighbours and considers
#' a point as noise if it is further than the average distance (for the entire point cloud)
#' plus \code{sigma} times the standard deviation away from other points.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.
#' @param k numeric. The number of nearest neighbours to use. Default = 5.
#' @param sigma numeric. The multiplier of standard deviation to consider a point as noise. Default = 1.5.
#' @param store_noise logical. Should the noisy points be retained ? Default = FALSE.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return  If \code{store_noise = TRUE} the input data is returned with an additional field ("Noise")
#'          where points that are classified as noise points are labaled with 2 and the points not classified as noise are labeled as 1.
#'          If \code{store_noise = FALSE} only the points that were not classified as noise are returned.
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- run noise filter
#' clean=VoxR::filter_noise(tls,store_noise = TRUE)
#'
#' #- plot the result (noise in red)
#' rgl::open3d()
#' rgl::plot3d(clean,col=clean$Noise,add=TRUE)

filter_noise = function(data,k,sigma,store_noise,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  dist=Noise=sd=':='=NULL

  if(missing(k)) k = 5
  if(missing(sigma)) sigma = 1.5
  if(missing(store_noise)) store_noise = FALSE

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message)

  #- check parameters consistancy
  if(is.numeric(k)==FALSE) stop("k must be numeric")
  if(is.numeric(sigma)==FALSE) stop("sigma must be numeric")
  if(k<=0|sigma<=0) stop("k and sigma must be greater than 0")

  #- computes average distance of k nearest neighbours distance
  data[,dist:=rowSums(FNN::knn.dist(data=as.matrix(data[,1:3]),algorithm = "kd_tree",k=k))/k]

  #- points above the threshold are noise (1 = not noise, 2 = noise)
  if(store_noise){
    data[,Noise:=1]
    data[dist > (mean(data[,dist])+(sigma*sd(data[,dist]))),Noise := 2]
  }else{
    data=data[dist <= (mean(data[,dist])+(sigma*sd(data[,dist])))]
  }

  if(check$dfr) data = as.data.frame(data)

  return(data)
}



