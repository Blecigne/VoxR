#' Produces a filled voxel cloud.
#'
#' @description This function produces a filled voxel cloud of a tree, i.e. a voxels cloud within which
#'              empty objects (e.g. trunk and large branches) are filled. The algorithm was inspired from
#'              the one described by Vonderach et al. (2012) with some modifications. First, the point cloud is
#'              is voxelized with a given (\code{res}) voxel resolution. The voxel cloud is then sliced into one voxel
#'              tick layers. Within a single layer different objects are then clustered based on their
#'              distance to each other (see the \link{distance_clustering} function for more details). Each
#'              cluster is then filled by addind voxels along the range of Y for each X value of the cluster and reversly along
#'              the range of X for each Y of the cluster. All unique voxels are then returned.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.
#' @param res numeric. Resolution of a voxel.
#' @param d_clust numeric. The distance to use for clustering, see the \link{distance_clustering} for more details.
#' @param estimate_volume logical. If TRUE the tree volume is computed as done in Vonderach et al. (2012).
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return If \code{estimate_volume = FLASE} a data.frame or data.table containing the voxels coordinates is returned. If
#'         \code{estimate_volume = TRUE} a list containing the voxels coordinates and the estimated tree volume
#'         is returned.
#'
#' @references Vonderach, C., Voegtle, T., & Adler, P. (2012). Voxel-based approach for estimating urban tree volume from
#'             terrestrial laser scanning data. International Archives of Photogrammetry, Remote Sensing and Spatial Information Sciences,
#'             39, 451-456.
#' @importFrom data.table .N :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t1.asc", package="VoxR"))
#'
#' #- keep the tree trunk
#' tls=tls[z<=0]
#'
#' #- run filled voxel voxelisation
#' filled = VoxR::filled_voxel_cloud(tls,0.02)
#'
#' #- run usual voxelisation
#' voxels = VoxR::vox(tls,0.02)
#'
#' #- compare filled voxel cloud to empty voxel cloud
#' VoxR::plot_voxels(filled,res = 0.02)
#' VoxR::plot_voxels(voxels,res = 0.02)
#'
#' #- compare the volume estimate from Vonderach et al. 2012 to estimate based on voxel volume
#' #- run filled voxel voxelisation with volume estimation
#' filled = VoxR::filled_voxel_cloud(tls,0.01,estimate_volume = TRUE)
#'
#' #- compare volumes
#' filled$estimated_volume # Vonderach
#' nrow(filled$filled_voxels)*0.01^3 # voxel based

filled_voxel_cloud = function(data,res,d_clust,estimate_volume,message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  index=volume=x=y=z=cluster=dist=cutree=.=N=.N=npts=':='=NULL

  if(missing(estimate_volume)) estimate_volume = FALSE
  if(missing(res)) res = 0.01
  if(missing(d_clust)) d_clust = 2*res

  data=vox(data,res=res,message)

  data[,index:=1:nrow(data)]

  sequence <- function(from,to,by){
    out=c()
    for(i in 1:length(from)) out = c(out,seq(from[i],to[i],by))
    return(out)
  }

  first=TRUE
  Z=sort(unique(data$z))
  data[,volume:=0]
  pb <- utils::txtProgressBar(min = 0, max = length(Z), style = 3,width=33)
  Volume=0
  for(i in 1:length(Z)){
    utils::setTxtProgressBar(pb, i)
    layer = data[z==Z[i],]

    #- clustering
    if(nrow(layer) == 1){
      layer[,cluster:=1]
    }else{
      dist_mat <- dist (layer[,1:2])
      layer[,cluster:=cutree(fastcluster::hclust(dist_mat, method = "single"),h=d_clust)]
    }

    #- interpolate y voxels coordinates for each x value
    x_range = layer[,.(min=min(y),max=max(y)),by=.(x,cluster)] #- find y ranges for each x
    x_range[,N:=round((max-min)/res+1)] #- number of voxels in a given row
    #- create the first voxel cloud for the layer
    x_temp = data.table::data.table(x=rep(x_range$x,x_range$N),
                                    y=sequence(from = x_range$min, to = x_range$max, by = res),
                                    z=Z[i],
                                    cluster=rep(x_range$cluster,x_range$N))

    #- interpolate x voxels coordinates for each y value
    y_range = layer[,.(min=min(x),max=max(x)),by=.(y,cluster)] #- find x range for each y
    y_range[,N:=round((max-min)/res+1)] #- nmber of voxels in a given row
    #- create the second voxel cloud for the layer
    y_temp = data.table::data.table(x=sequence(from = y_range$min, to = y_range$max, by = res),
                                    y=rep(y_range$y,y_range$N),
                                    z=Z[i],
                                    cluster=rep(y_range$cluster,y_range$N))

    #- combine the two voxels clouds
    recons = unique(dplyr::bind_rows(y_temp,x_temp))

    #- add the original voxel cloud
    recons_cl = dplyr::bind_rows(recons,layer[,.(x,y,z,cluster)])
    #- label original voxels as 2 and added voxels as 1
    recons_cl = recons_cl[,.N,by=.(x,y,z,cluster)]

    if(estimate_volume){
      #- estimate the layer volume as done in Vonderach et al. 2012:
      #- the mean value between the surface with and without external voxels
      vol=(nrow(recons)+nrow(recons_cl[N==1]))/2
      Volume=Volume+(vol*res^3) #- the total volume
    }
    if(first){
      out=recons_cl[,.(x,y,z)]
      first=FALSE
    }else{
      out=data.table::rbindlist(list(out,recons_cl[,.(x,y,z)]))
    }
  }

  out[,npts:=0]
  out = dplyr::bind_rows(out,data[,':='(index=NULL,volume=NULL)])
  out[,npts:=sum(npts),by=.(x,y,z)]

  if(estimate_volume){
    return(list(estimated_volume = Volume,filled_voxels = unique(out)))
  }else{
    return(unique(out))
  }
}
