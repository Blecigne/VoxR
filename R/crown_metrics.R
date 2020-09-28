
#' Estimates a set of morphological parameters from a tls point cloud of a tree.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.
#' @param dbh numeric and optional. Estimate tree DBH ?
#' @param height numeric and optional. Estimate tree height ?
#' @param crown_diameter numeric and optional. Estimate tree average crown diameter ?
#' @param crown_proj_area numeric and optional. Estimate tree crown projected area ?
#' @param volume numeric and optional. Estimate tree volume ?
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @details
#' \subsection{Selecting parameters to compute}{If none of \code{dbh},\code{height},\code{crown_diameter},
#'             \code{crown_proj_area} and \code{volume} are passed, all parameters are computed. However, the
#'             user can select a set of parameters by declaring wich parameters should be computed (all other are not).}
#' \subsection{Parameters estimates}{The tree DBH is estimated as the diameter of a circle fitted to the point cloud between
#'             1.2m and 1.4m above the ground. The tree height is computed as the elevation difference between the lowest and
#'             the highest points of the point cloud. Two values are provided for crown parameter. First a 2D convex hull
#'             is used to identify the external points of the crown. Then, a first estimate of the crown diameter ("distant_points")
#'             is computed as the average distance of each point to the further point. A second estimate ("circle_fitting") correspond
#'             to the diameter of a circle fitted to the crown external points. The crown projected area is computed as the area of a
#'             2D convex hull that wraps the projected crown. The volume is computed as the volume of a 3D convex hull that wraps the point cloud.}
#'
#' @return a list containing the estimated value for each parameter.
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#' #- import tls data
#' tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))
#'
#' #- compute all metrics
#' VoxR::tree_metrics(tls)
#'
#' #- compute DBH only
#' VoxR::tree_metrics(tls,dbh = TRUE)
#'
#' #- compute DBH and height
#' VoxR::tree_metrics(tls,dbh = TRUE,height = TRUE)

tree_metrics = function(data,dbh,height,crown_diameter,crown_proj_area,volume, message){

  #- declare variables to pass CRAN check as suggested by data.table mainaitners
  x=y=z=dist=.=':='=NULL

  #- create a vector of the variables to estimate
  if(missing(dbh)&missing(height)&missing(crown_diameter)&missing(crown_proj_area)&missing(volume)){
    test_vec = c(TRUE,TRUE,TRUE,TRUE,TRUE)
  }else{
    test_vec = c(FALSE,FALSE,FALSE,FALSE,FALSE)
    if(!missing(dbh)){
      if(!is.logical(dbh)) stop("dbh must be logical")
      test_vec[1] = dbh
    }
    if(!missing(height)){
      if(!is.logical(height)) stop("height must be logical")
      test_vec[2] = height
    }
    if(!missing(crown_diameter)){
      if(!is.logical(crown_diameter)) stop("crown_diameter must be logical")
      test_vec[3] = crown_diameter
    }
    if(!missing(crown_proj_area)){
      if(!is.logical(crown_proj_area)) stop("crown_proj_area must be logical")
      test_vec[4] = crown_proj_area
    }
    if(!missing(volume)){
      if(!is.logical(volume)) stop("tree_volume must be logical")
      test_vec[5] = volume
    }
  }

  check=VoxR::ck_conv_dat(data, message)

  data=check$data

  #- downsample point cloud to speed up the process
  data[,':='(X_vox = round(x/0.02)*0.02,
             Y_vox = round(y/0.02)*0.02,
             Z_vox = round(z/0.02)*0.02,
             row = 1:nrow(data))]
  temp <- data[,.(to_keep = max(row)), by = 'X_vox,Y_vox,Z_vox']
  data <- data[temp$to_keep,]

  #- remove useless dataframe and columns
  rm(temp)
  data[,':='(X_vox = NULL,Y_vox = NULL,Z_vox = NULL,row = NULL)]

  #- create the output list
  out_list = list()

  #- dbh is computed by fitting a circle on the point cloud between 1.2 and 1.4 m above the tree base
  if(test_vec[1]){
    base = min(data$z)
    DBH = (circular::lsfit.circle(data[ z > base + 1.2 & z <= base + 1.4,1:2])$coefficients[1])*2
    names(DBH) = NULL
    out_list[["DBH"]] = DBH
  }

  #- tree height is the difference in elevation between the lowest and the upper points
  if(test_vec[2]) out_list[["height"]] = max(data$z)-min(data$z)

  #- crown diameter and projected area require to compute a 2D convex hull
  if(test_vec[3] | test_vec[4]) hull2D=geometry::convhulln(data[,1:2], output.options=TRUE)

  #- crown diameter is the average distance among opposite points of the crown
  if(test_vec[3]){
    #- fit a circle to the crown external points
    circ = circular::lsfit.circle(data[hull2D$hull[,1],1:2])$coefficients[1]*2
    names(circ) = NULL
    out_list[["crown_diameter"]] = c(
      #- diameter is the average distance between the most diatant external points
      distant_points = mean(Rfast::rowMaxs(as.matrix(dist(unique(hull2D$p[hull2D$hull[,1],]),upper = TRUE)),value = T)),
      circle_fitting = circ
    )
  }

  #- crown projected area is the area of the 2D convex hull
  if(test_vec[4]) out_list[["crown_projected_area"]] = hull2D$area

  #- the tree volume is the volume of a 3D convex hull that wrap the point cloud
  if(test_vec[5]){
    hull3D=geometry::convhulln(data[,1:3], output.options=TRUE)
    out_list[["volume"]] = hull3D$vol
  }

  return(out_list)
}
