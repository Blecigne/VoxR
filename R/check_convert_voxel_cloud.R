
#' Check voxel cloud suitability
#'
#' @param data a data.frame or data.table.
#' @param message logical. If FALSE, messages are disabled. Default = TRUE.
#'
#' @return the data converted to data.table and a logical value indicating if the input data was a base R data.frame or a data.table.
#' @export
#'
#' @keywords internal
ck_conv_vox = function(data,message){

  if(missing(message)) message = TRUE

  #- right type ?
  if(!(is.data.frame(data))) stop("data must be a data.frame or a data.table")

  #- right number of columns ?
  if(ncol(data) > 4){
    if(message) print("NOTE: data contain more than 3 columns, three first used")
  }
  if(ncol(data) < 4){
    stop("data must contain 4 columns, x, y, z and number of points")
  }

  #- store the data type to set the output correctly
  dfr=FALSE
  if(!data.table::is.data.table(data)) dfr = TRUE

  #- convert the data into a data.frame
  data=data.table::data.table(data[,1:4])
  data.table::setnames(data,c("x","y","z","npts"))

  #- numeric ?
  if(!(all(sapply(data[,1:3],class)=="numeric") & (all(sapply(data[,4],class)=="integer")|all(sapply(data[,4],class)=="numeric"))  )){
    stop("All the fields of the data must be numeric")
  }

  #- is there any NA ?
  if(any(is.na(data))&message) warning("data contains missing values.")

  invisible(list(data=data,dfr=dfr))
}
