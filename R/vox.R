#' Voxelisation of 3D point cloud recording the number of points within each voxels.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.
#' @param res numeric. Resolution of a voxel.
#' @param full.grid logical. If TRUE empty voxels contained in the tree bounding box are returned.
#'                  If FALSE, only filled voxels are returned. Default = FALSE.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return A data.frame or data.table containing the x, y, z coordinates of the voxel center and
#'         the number of points within each voxel of a voxel cloud.
#'
#' @references Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
#'             analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
#'
#' @importFrom data.table := .N
#'
#' @export
#'
#' @examples
#'
#' #- import file
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- resolution = 0.02m
#' voxels_002 = VoxR::vox(tls,res=0.02) # voxelisation
#' VoxR::plot_voxels(voxels_002) # voxels plot
#'
#' #- resolution = 0.2m
#' voxels_02 = VoxR::vox(tls,res=0.2) # voxelisation
#' VoxR::plot_voxels(voxels_02) # voxels plot


vox = function(data,res,full.grid,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  x=y=z=npts=.N=.=':='=NULL

  #- check for data consistancy and convert to data.table
  check=VoxR::ck_conv_dat(data,message=message)

  if(missing(res)){
    stop("No voxel resolution (res) provided")
  }else{
    #- res must be a vector
    if(!is.vector(res)) stop("res must be a vector of length 1")
    #- res must be numeric
    if(!is.numeric(res)) stop("res must be numeric")
    #- res must be numeric
    if(res<=0) stop("res must be positive")
    #- res must be of length 1
    if(length(res)>1){
      res=res[1]
      warning("res contains more than 1 element. Only the first was used")
    }
  }

  #- default is without full grid
  if(missing(full.grid)) full.grid = FALSE

  #- keep the data
  data=check$data

  #- round the data coordinates with the user defines resolution
  data[,':='(x = Rfast::Round( x / res ) * res,
             y = Rfast::Round( y / res ) * res,
             z = Rfast::Round( z / res ) * res)]

  data = unique(data[,npts:=.N,by=.(x,y,z)])

  if(full.grid){

    x_seq = seq(min(data$x),max(data$x),res)
    y_seq = seq(min(data$y),max(data$y),res)
    z_seq = seq(min(data$z),max(data$z),res)

    est_weight = round(length(x_seq)*length(y_seq)*length(z_seq)*4*8/2^{20}/1024,1)
    if(est_weight > 2){
      cat(paste("Final data is estimated to be ",est_weight,"GB in size. Continue execution ? y or n"))
      test=readline()
      if(test != "y"){
        stop("Execution stoped.")
      }
    }

    empty=data.table::data.table(expand.grid(x_seq,y_seq,z_seq))
    data.table::setnames(empty,c("x","y","z"))
    empty[,npts:=0]
    data = dplyr::bind_rows(data,empty)
    data = data[,npts:=sum(npts),keyby=.(x,y,z)]
  }

  if(check$dfr) data = as.data.frame(data)

  return(data) #- output = coordinates + number of points
}


