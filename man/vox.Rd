% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vox.R
\name{vox}
\alias{vox}
\title{Voxelisation of 3D point cloud recording the number of points within each voxels.}
\usage{
vox(data, res, full.grid, message)
}
\arguments{
\item{data}{a data.frame or data.table containing the x, y, z, ... coordinates of a point cloud.}

\item{res}{numeric. Resolution of a voxel.}

\item{full.grid}{logical. If TRUE empty voxels contained in the tree bounding box are returned.
If FALSE, only filled voxels are returned. Default = FALSE.}

\item{message}{logical. If FALSE, messages are disabled. Default = TRUE.}
}
\value{
A data.frame or data.table containing the x, y, z coordinates of the voxel center and
        the number of points within each voxel of a voxel cloud.
}
\description{
Voxelisation of 3D point cloud recording the number of points within each voxels.
}
\examples{

#- import file
tls=data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))

#- resolution = 0.02m
voxels_002 = VoxR::vox(tls,res=0.02) # voxelisation
VoxR::plot_voxels(voxels_002) # voxels plot

#- resolution = 0.2m
voxels_02 = VoxR::vox(tls,res=0.2) # voxelisation
VoxR::plot_voxels(voxels_02) # voxels plot
}
\references{
Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to
            analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
}
