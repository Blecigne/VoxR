#' Guess the voxel resolution in a voxel cloud
#'
#' @description measure the distance to the nearest neighbour for \code{N_sample} voxels and guess the resolution:
#'              the majority wins.
#'
#' @param data a data.frame or data.table containing the x, y, z, ... coordinates of a voxel cloud.
#' @param N_sample numeric. The number of voxels to sample. Default = 100.
#' @param message logical. FALSE desables the message.
#'
#' @return the guessed resolution.
#'
#' @keywords internal
#'
#' @export

guess_resolution=function(data,N_sample,message){

  if(missing(N_sample)) N_sample = 100
  if(missing(message)) message = TRUE

  sub=data[sample(1:nrow(data),N_sample),]
  dist = FNN::knnx.dist(data=as.matrix(data[,1:3]),query=as.matrix(sub[,1:3]),algorithm = "kd_tree",k=2)[,2]
  times=table(round(dist,digits=10))

  if(length(times)>N_sample*0.9) stop("Could not guess the voxel cloud resolution. Verify that data is a voxel cloud or provide the resolution manually")

  guess = as.numeric(names(which.max(times)))

  if(guess == 0) stop("Guessed resolution is 0, please define resolution manually.")
  if(message) print(paste("NOTE: the resolution was gessed as",guess))

  return(guess)
}
