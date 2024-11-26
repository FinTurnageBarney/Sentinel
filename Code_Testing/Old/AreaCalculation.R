# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)
library(snow)
library(doSNOW)

folder_paths <- list.files(path = "Data/Scenes/2020_2021/Cloudcover40", full.names = TRUE)

# Set the folder path
#folder_paths <- c("Data/Scenes/2019-07-24", "Data/Scenes/2018-10-22" ,"Data/Scenes/2019-03-26", "Data/Scenes/2019-09-02" )

custom_parallel_function = function(x){
  # DEFINE CUSTOM FUNCTION
  
  
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
    
    # Define the AOI polygon
    aoi_char <- "POLYGON ((-82.25035 43.01371, -80.90025 43.01371, -80.90025 42.62048, -82.25035 42.62048, -82.25035 43.01371))"
    
    # Read and convert the AOI polygon to an sf object
    aoi <- sf::st_as_sfc(aoi_char)
    #aoi <- st_read(aoi)
    
    # Define the initial CRS for the AOI polygon (assuming it's in WGS84, EPSG:4326)
    aoi <- sf::st_set_crs(aoi, 4326)
    
    
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
    
    
    blue <- raster_list[[1]]
    
    SCL <- raster::crop(raster::raster(SCL_band), sf::st_bbox(aoi_transformed))
    
    #plotting SCL 20 meter raster layer
    plot(SCL)
    
    
    # using SCL to remove bad data areas
    
    # these values correspond to issues like cloud cover and etc
    bad_vals <- c(1, 3, 8, 9 , 10)
    
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
    
    plot(bands_10m_cropped)
    
    blue <- bands_10m_cropped[[1]]
    green <- bands_10m_cropped[[2]] 
    red <- bands_10m_cropped[[3]] 
    NIR <- bands_10m_cropped[[4]]
    
    
    
    #calculating NDWI
    NDWI <- ((green - NIR)/(green + NIR))
    
    #plotting NDWI
    
    plot(NDWI)
    
    
    #loading in the boundaries for the wetlands
    
    BL_Boundary <- sf::st_read("Data/Wetland_boundaries/BL_202307_buffer.shp")
    
    DY_Boundary <- sf::st_read("Data/Wetland_boundaries/DY_202308_buffer.shp")
    
    FE_Boundary <- sf::st_read("Data/Wetland_boundaries/FE_202306_buffer.shp")
    
    KE_Boundary <- sf::st_read("Data/Wetland_boundaries/KE_202307_buffer.shp")
    
    LL_Boundary <- sf::st_read("Data/Wetland_boundaries/LL_202308_buffer.shp")
    
    MA_Boundary <- sf::st_read("Data/Wetland_boundaries/MA_202308_buffer.shp")
    
    MO_Boundary <- sf::st_read("Data/Wetland_boundaries/MO_202308_buffer.shp")
    
    OH_Boundary <- sf::st_read("Data/Wetland_boundaries/OH_202308_buffer.shp")
    
    bound_list <- list(BL_Boundary, DY_Boundary, FE_Boundary, KE_Boundary, LL_Boundary, MA_Boundary, MO_Boundary, OH_Boundary)
    
    Wetland_Names <- c("BL", "DY" , "FE" , "KE", "LL", "MA", "MO", "OH")
    
    names(bound_list) <- Wetland_Names
    
    # cropping the NDWI raster to the wetlands area
    
    Wetland_rasters <- lapply(bound_list, function(b) {
      
      
      raster::crop(x = NDWI, y = sf::as_Spatial(b))
      
    })
    
    
    threshold_list <- c(-.3, -.2, -.1, 0)
    
    wetland_Water_Areas <- lapply(threshold_list, function(t){
      
      wetland_Water <- lapply(Wetland_rasters, function(w){ 
        
        (sum(raster::values(w) > t , na.rm = TRUE))*100
      })
      
      
    })
    
    Threshold_names <- c("-.3", "-.2" , "-.1" , "0")
    
    names(wetland_Water_Areas) <- Threshold_names
    
    return(wetland_Water_Areas)
    
    
  })

}


ncores = 2
# = NUMBER OF OPERATIONS (e.g. nrow(data))
noperations = 22 

cl <- snow::makeCluster(ncores, type = "SOCK")
doSNOW::registerDoSNOW(cl)
pb <- utils::txtProgressBar(max = noperations, style = 3)
progress <- function(n){utils::setTxtProgressBar(pb, n)}
opts <- list(progress = progress)
foreach::foreach(
  INDEX = folder_paths,
  .options.snow = opts
) %dopar% {custom_parallel_function(INDEX)}
snow::stopCluster(cl)





















names(Water_area_per_Scene) <- folder_paths


keep_last_10 <- function(x) {
  substr(x, nchar(x) - 9, nchar(x))
}

output <- do.call("rbind", lapply(folder_paths, function(f){
  s = Water_area_per_Scene[[f]]
  
  do.call("rbind", lapply(names(s), function(ns){
    t = s[[ns]]
    
    d = as.data.frame(t)
    d$image = keep_last_10(f)
    d$threshold = ns
    return(d)
  }))
}))


# 

utils::write.csv(output, file = "2020_Scene_Test.csv")
