# VoxR
## Main concept
The VoxR package was primarily developed to process Terrestrial Laser Scanning data. The core of the package is based on the assumption that a voxel can be understood as a portion of space explored by the tree (i.e. containing a least an arbitrary number of TLS points). Most analyses and algorithms provided in the VoxR package are based on the concept of space exploration. This approach enables multiple metrics to be derived from TLS data that describe the way a tree occupies the 3-D space quantitatively (i.e. estimate the volume of space explored by the tree) or qualitatively (i.e. localise the biomass within a tree crown).

## Related paper
More information about the space exploration concept can be found in:

Lecigne, B., Delagrange, S., & Messier, C. (2018). Exploring trees in three dimensions: VoxR, a novel voxel-based R package dedicated to analysing the complex arrangement of tree crowns. Annals of botany, 121(4), 589-601.
https://academic.oup.com/aob/article/121/4/589/4107549

## Other functionalities
The VoxR package also includes functions to measure simple morphological metrics (e.g. DBH, tree height, volume), measure the tree fractal dimension and improve TLS point cloud quality (noise filter, point density filter).
