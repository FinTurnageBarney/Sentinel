# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)
library(EBImage)


folder_paths <- list.files(path = "Data/Scenes/2018_2019/All_Scenes/Test", full.names = TRUE)



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
  TCI_band   <- jp2_files[grep("_TCI_", jp2_files)]
  AOT_band   <- jp2_files[grep("_AOT_", jp2_files)]
  WVP_band   <- jp2_files[grep("_WVP_", jp2_files)]
  SCL_band   <- jp2_files[grep("_SCL_", jp2_files)]
  # List the specific raster files
  file_list <- c(blue_band, green_band, red_band, NIR_band, TCI_band, AOT_band, WVP_band  )
  
  
  
  setwd("C:/Users/fturnage/OneDrive - University of Waterloo/Work/Sentinel")
  
  # Read the first raster file to get its projection
  raster_file <- raster::raster(file.path(file_list[1]))
  raster_projection <- raster::projection(raster_file)
  
  
  # Read, clip, and stack the raster files
  raster_list <- lapply(file_list, function(file) {
    raster_file <- raster::raster(file)
    return(raster_file)
  })
  
  
  raster_stack_10m <- raster::stack(raster_list[1:7])
  
  blue <- raster_list[[1]]
  
  
  SCL <- raster::raster(SCL_band)
  
  
  raster::plot(SCL)
  
  # these values correspond to issues like cloud cover and etc
  bad_vals <- c(1, 3, 8, 9 , 10, 11)
  
  
  
  SCL_Remove <- raster::calc(SCL, fun = function(x) {
    #ifelse(x %in% bad_vals,NA,x)
    x[!x %in% bad_vals] <- NA
    return(x)
  })
  
  
  SCL_Remove_10m <- raster::resample(SCL_Remove, blue, method = "ngb")
  
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
  
  
  #calculating NDWI
  NDWI <- ((green - NIR)/(green + NIR))
  
  # Removing stuff no longer needed
  rm(blue, green, red, NIR, bands_10m_cropped, cropped_Bands, SCL_Remove_10m, 
     SCL_Remove, bad_vals, SCL, raster_list, raster_projection, 
     raster_file, file_list, blue_band, red_band, green_band,
     NIR_band, TCI_band, AOT_band, SCL_band, WVP_band, jp2_files )
  
  #plotting NDWI
  
  raster::plot(NDWI)
  
  # Loading Wetland Boundaries
  
  FE_Boundary <- sf::st_read("Data/Wetland_boundaries/Wetland_GPS/FE_202306.shp")
  
  Extra1 <- sf::st_read("Data/Wetland_boundaries/Extra_Wetlands_Near_FE/Additional_WL_1.shp")
  
  Extra2 <- sf::st_read("Data/Wetland_boundaries/Extra_Wetlands_Near_FE/Additional_WL_2.shp")
  
  Extra3 <- sf::st_read("Data/Wetland_boundaries/Extra_Wetlands_Near_FE/Additional_WL_3.shp")
  
  Extra4 <- sf::st_read("Data/Wetland_boundaries/Extra_Wetlands_Near_FE/Additional_WL_4.shp")
  
  Extra5 <- sf::st_read("Data/Wetland_boundaries/Extra_Wetlands_Near_FE/Additional_WL_5.shp")
  
  Extra6 <- sf::st_read("Data/Wetland_boundaries/Extra_Wetlands_Near_FE/Additional_WL_6.shp")
 
  WL_list <- list(FE_Boundary, Extra1, Extra2, Extra3, Extra4, Extra5, Extra6 )
   
  Wetland_Names <- c("FE", "EX1" , "EX2" , "EX3", "EX4", "EX5", "EX6")
  
 
  names(WL_list) <- Wetland_Names
  
  
  
  Wetland_Rasters <- lapply(WL_list, function(b) {
    
    
    rasters <- raster::crop(x = NDWI, y = sf::as_Spatial(b))
    
    
        
    
  })
  
  Wetland_Raster_Values <- lapply(WL_list, function(b) {
    
    
    rasters <- raster::crop(x = NDWI, y = sf::as_Spatial(b))
    
    Values <- raster::values(rasters)
    
    
  })
  

  plot(Wetland_Rasters$EX2)
  
  
  
})



  
  