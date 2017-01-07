RasterForResampling <- function(soilMap, vegMap, catchment){

  # dummy extent from your rasters, instead use lapply(raster list, extent)
  a <- as.vector(raster::extent(soilMap))
  b <- as.vector(raster::extent(vegMap))
  c <- as.vector(raster::extent(catchment))
  extent_list<-list(a, b, c)

  # make a matrix out of it, each column represents a raster, rows the values
  extent_list <- do.call(cbind, extent_list)
  rownames(extent_list)<-c("xmin", "xmax", "ymin", "ymax")

  # create an extent with the extrem values of your extent
  best_extent <- c(min(extent_list[1,]), max(extent_list[2,]),
                   min(extent_list[3,]), max(extent_list[4,]))

  # the range of your extent in degrees
  ranges <- apply(matrix(best_extent, ncol = 2), 1, diff)
  # the resolution of your raster (pick one) or add a desired resolution
  reso <- raster::res(vegMap)
  # deviding the range by your desired resolution gives you the number of rows and columns
  nrow_ncol<-ranges/reso

  # create your raster with the following externt
  s <- raster::raster(raster::extent(best_extent), nrows=nrow_ncol[2],
                      ncols=nrow_ncol[1], crs=vegMap@crs)

  return(s)
}
