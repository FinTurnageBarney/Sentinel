# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)
library(EBImage)




folder_path <- list.files(path = "Data/Scenes/2020_2021/MNDWI_Scenes/Test", full.names = TRUE)

#function that reads rasters, crops to AOI, removes bad data using SCL layer, 

Water_area_per_Scene <-  lapply(folder_paths, function(folder_path){
  
  # List all files with the ".jp2" extension
  jp2_files <- list.files(path = folder_path, pattern = "\\.jp2$", ignore.case = TRUE, full.names = TRUE)
  
  # Print the list of files
  print(jp2_files)
  
  # Extract Blue, Green, and Red bands based on their file names
  blue_band  <- jp2_files[grep("_B02_", jp2_files)]
  green_band <- jp2_files[grep("_B03_", jp2_files)]
  red_band   <- jp2_files[grep("_B04_", jp2_files)]
  NIR_band   <- jp2_files[grep("_B08_", jp2_files)]
  SWIR_band1 <- jp2_files[grep("_B12_", jp2_files)]
  SCL_band   <- jp2_files[grep("_SCL_", jp2_files)]
  
  
  # List the specific raster files
  file_list <- c(blue_band, green_band, red_band, NIR_band , SWIR_band1,SCL_band  )
  
  # Define the AOI polygon
  #aoi_char <- "POLYGON ((-82.25035 43.01371, -80.90025 43.01371, -80.90025 42.62048, -82.25035 42.62048, -82.25035 43.01371))"
  
  # Read and convert the AOI polygon to an sf object
  #aoi <- sf::st_as_sfc(aoi_char)
  #aoi <- st_read(aoi)
  
  # Define the initial CRS for the AOI polygon (assuming it's in WGS84, EPSG:4326)
  #aoi <- sf::st_set_crs(aoi, 4326)
  
  # set working directory
  
  setwd("C:/Users/fturnage/OneDrive - University of Waterloo/Work/Sentinel")
  
  # Read the first raster file to get its projection
  raster_file <- raster::raster(file.path(file_list[1]))
  raster_projection <- raster::projection(raster_file)
  
  # Reproject the AOI polygon to match the raster projection
  #aoi_transformed <- sf::st_transform(aoi, raster_projection)
  
  # Read, clip, and stack the raster files
  raster_list <- lapply(file_list, function(file) {
    raster_file <- raster::raster(file)
    #raster_clipped <- raster::crop(raster_file, sf::st_bbox(aoi_transformed))
    return(raster_file)
  })
  
  #raster_stack_10m <- raster::stack(raster_list[1:7])
  
  blue <- raster_list[[1]]
  
  #plot(raster_stack_10m)
  
  #SCL         <- raster::crop(raster::raster(SCL_band), sf::st_bbox(aoi_transformed))
  #SWIR_1  <- raster::crop(raster::raster(SWIR_band1), sf::st_bbox(aoi_transformed))
  
 
  SCL <- raster_list[[6]]
  SWIR_1 <- raster_list[[5]]
  
  #plotting SCL 20 meter raster layer
  raster::plot(SCL)
  
  plot(SWIR_1)
  # using SCL to remove bad data areas
  
  # these values correspond to issues like cloud cover and etc
  bad_vals <- c(1, 3, 8, 9 , 10, 11)
  
  # this function keeps all the bad values to be used to overlay later.
  
  SCL_Remove <- raster::calc(SCL, fun = function(x) {
    #ifelse(x %in% bad_vals,NA,x)
    x[!x %in% bad_vals] <- NA
    return(x)
  })
  
  
  SCL_Remove_10m <- raster::resample(SCL_Remove, blue, method = "ngb")
  
  SWIR_1 <- raster::resample(SWIR_1, blue, method = "ngb")
  
  raster_list <- raster_list[-5]
  raster_list <- raster_list[-5]
  
  raster_list <- append(raster_list, SWIR_1, after = 4 )
  
  # now overlaying these bad values on my NDWI raster to remove any issues caused by clouds/bad data/ shadows etc
  
  cropped_Bands <- lapply(raster_list, function(ras){
    
    new_raster <- raster::overlay(ras, SCL_Remove_10m, fun = function(x, y) {
      x[!is.na(y)] <- NA
      return(x)
    })
    return(new_raster)
    
  })
  
  bands_10m_cropped <- raster::stack(cropped_Bands)
  
  raster::plot(bands_10m_cropped)
  
  blue <- bands_10m_cropped[[1]]
  green <- bands_10m_cropped[[2]] 
  red <- bands_10m_cropped[[3]] 
  NIR <- bands_10m_cropped[[4]]
  SWIR1 <- bands_10m_cropped[[5]]
  
  
  #calculating NDWI
  MNDWI <- ((green - SWIR1)/(green + SWIR1))
  
  # Removing stuff no longer needed
  rm(blue, green, red, NIR, bands_10m_cropped, cropped_Bands, SCL_Remove_10m, 
     SCL_Remove, bad_vals, SCL, raster_list, aoi_transformed, raster_projection, 
     raster_file, aoi, aoi_char, file_list, blue_band, red_band, green_band,
     NIR_band, SCL_band, jp2_files )
  
  #plotting NDWI
  
  raster::plot(MNDWI)
  
  
  MNDWI <- (MNDWI + 1)*100
  
  raster::plot(MNDWI)
 
  
  
 return(MNDWI)
  
  
})  





MNDWI[MNDWI < 0] <- NA




file_path <- "MNDWI_Test_above_Zero.tif"

# Export the raster to a file (GeoTIFF format in this case)
writeRaster(MNDWI, filename = file_path, format = "GTiff", overwrite = TRUE)

