
#' Check point cloud suitability.
#'
#' @param data a data.frame or data.table.
#'
#' @return the data converted to data.table and a logical value indicating if the input data was a base R data.frame or a data.table.
#' @export
#'
#' @keywords internal
ck_conv_dat = function(data, message){

  if(missing(message)) message = TRUE

  #- right type ?
  if(!(is.data.frame(data))) stop("data must be a data.frame or a data.table")

  #- right number of columns ?
  if(ncol(data) > 3){
    if(message) print("NOTE: data contain more than 3 columns, three first used")
  }

  #- store the data type to set the output correctly
  dfr=FALSE
  if(!data.table::is.data.table(data)) dfr = TRUE

  #- convert the data into a data.frame
  data=data.table::data.table(data[,1:3])
  data.table::setnames(data,c("x","y","z"))

  #- numeric ?
  if(!(all(sapply(data,class)=="numeric"))){
    stop("All the fields of the data must be numeric")
  }

  #- is there any NA ?
  if(any(is.na(data))&message) warning("data contains missing values.")

  invisible(list(data=data,dfr=dfr))
}
