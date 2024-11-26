# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)
library(EBImage)


#define scene folders

folder_paths <- list.files(path = "Data/Scenes/2020_2021/Only_Full_scenes/Cloudcover10", full.names = TRUE)

Otsu_Water_area_per_Scene <-  lapply(folder_paths, function(folder_path){
  
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
  
  # Define the AOI polygon
  aoi_char <- "POLYGON ((-82.25035 43.01371, -80.90025 43.01371, -80.90025 41.62048, -82.25035 41.62048, -82.25035 43.01371))"
  
  # Read and convert the AOI polygon to an sf object
  aoi <- sf::st_as_sfc(aoi_char)
  #aoi <- st_read(aoi)
  
  # Define the initial CRS for the AOI polygon (assuming it's in WGS84, EPSG:4326)
  aoi <- sf::st_set_crs(aoi, 4326)
  
  # set working directory
  
  setwd("C:/Users/fturnage/OneDrive - University of Waterloo/Work/Sentinel")
  
  # Read the first raster file to get its projection
  raster_file <- raster::raster(file.path(file_list[1]))
  raster_projection <- raster::projection(raster_file)
  
  # Reproject the AOI polygon to match the raster projection
  aoi_transformed <- sf::st_transform(aoi, raster_projection)
  
  # Read, clip, and stack the raster files
  raster_list <- lapply(file_list, function(file) {
    raster_file <- raster::raster(file)
    raster_clipped <- raster::crop(raster_file, sf::st_bbox(aoi_transformed))
    return(raster_clipped)
  })
  
  raster_stack_10m <- raster::stack(raster_list[1:7])
  
  blue <- raster_list[[1]]
  
  #plot(raster_stack_10m)
  
  SCL <- raster::crop(raster::raster(SCL_band), sf::st_bbox(aoi_transformed))
  
  #plotting SCL 20 meter raster layer
  raster::plot(SCL)
  
  
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
     SCL_Remove, bad_vals, SCL, raster_list, aoi_transformed, raster_projection, 
     raster_file, aoi, aoi_char, file_list, blue_band, red_band, green_band,
     NIR_band, TCI_band, AOT_band, SCL_band, WVP_band, jp2_files )
  
  #plotting NDWI
  
  raster::plot(NDWI)
  
  
  NDWI <- (NDWI + 1)*100
  
  raster::plot(NDWI)
  
  #loading in the boundaries for the wetlands **** DY uses the smaller boundary as it over estimates
  
  BL_Boundary <- sf::st_read("Data/New_Wetland_Boundaries/BL_New_Buffer_V2.shp")
  
  DY_Boundary <- sf::st_read("Data/New_Wetland_Boundaries/DY_New_Buffer_V2.shp")
  
  FE_Boundary <- sf::st_read("Data/New_Wetland_Boundaries/FE_New_Buffer_V2.shp")
  
  KE_Boundary <- sf::st_read("Data/New_Wetland_Boundaries/KE_New_Buffer_V2.shp")
  
  LL_Boundary <- sf::st_read("Data/New_Wetland_Boundaries/LL_New_Buffer_V2.shp")
  
  MA_Boundary <- sf::st_read("Data/New_Wetland_Boundaries/MA_New_Buffer_V2.shp")
  
  MO_Boundary <- sf::st_read("Data/New_Wetland_Boundaries/MO_New_Buffer_V2.shp")
  
  # Making wetland site groups to loop through each site and calculate area
  
  # first the eastern wetland Group
  
  E_WL_Group <- list(FE_Boundary, BL_Boundary, MA_Boundary, DY_Boundary)
  
  E_WL_Group_Names <- c("FE", "BL", "MA", "DY")
  
  names(E_WL_Group) <- E_WL_Group_Names
  
  # Now the Central Wetland Group
  
  C_WL_Group <- list(KE_Boundary, LL_Boundary, MO_Boundary )
  
  C_WL_Group_Names <- c("KE", "LL", "MO")
  
  names(C_WL_Group) <- C_WL_Group_Names
  
  # Wetland Groups Bounding Boxes to process  
  
  E_WL_Group_Bbox <- sf::st_read("Data/WL_Group_Boxes/Grouped_wetlands_box_east.shp")
  
  C_WL_Group_Bbox <- sf::st_read("Data/WL_Group_Boxes/Grouped_wetlands_box_central_New22.shp")
  
  
  WL_Group_BBox_Names <- c("E_WL_Group_Bbox","C_WL_Group_Bbox" )
  
  bound_list <- list(E_WL_Group_Bbox, C_WL_Group_Bbox )
  
  names(bound_list) <- WL_Group_BBox_Names
  
  Wetland_Group_Rasters <- lapply(bound_list, function(b) {
    
    
    rasters <- raster::crop(x = NDWI, y = sf::as_Spatial(b))
    
    
  })
  
  
  Otsu_Thresholds <- lapply(Wetland_Group_Rasters, function(wl) {
    
    
    wl.dat <- matrix(wl)
    
    hist(wl.dat)
    
    wl.dat <- wl.dat[!is.na(wl.dat)]
    
    wl.dat <- matrix(wl.dat)
    
    #print(hist(wl.dat))
    
    wl.range <- c(range(wl.dat))
    
    
    
    th <- EBImage::otsu(wl.dat, range = wl.range )
    
    
  })
  
  
  E_Wetland_Group_Areas <- lapply(E_WL_Group, function(E){
    # cropping NDWI to each site level
    Wl_NDWI <- raster::crop(x = NDWI, y = sf::as_Spatial(E))
    
    raster::plot(Wl_NDWI)
    
    wl.th = raster::calc(x = Wl_NDWI, fun = function(x){
      ifelse(x >= Otsu_Thresholds$E_WL_Group_Bbox ,1,0 )
    
    })
    
    raster::plot(wl.th)
    
    
  })
    
  
  
  C_Wetland_Group_Areas <- lapply(C_WL_Group, function(C){
    # cropping NDWI to each site level
    Wl_NDWI <- raster::crop(x = NDWI, y = sf::as_Spatial(C))
    
    raster::plot(Wl_NDWI)
    
    #th <- Otsu_Thresholds$C_Wl_Group_Bbox
    
    wl.th = raster::calc(x = Wl_NDWI, fun = function(x){
      ifelse(x >= Otsu_Thresholds$C_WL_Group_Bbox ,1,0 )
    })
    
    raster::plot(wl.th)
    
  })
    
    
  
  
  
})
