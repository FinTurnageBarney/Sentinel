# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)


# Set the folder path
folder_path <- "Data/Scenes/2018-10-22"

# List all files with the ".jp2" extension
jp2_files <- list.files(path = folder_path, pattern = "\\.jp2$", ignore.case = TRUE, full.names = TRUE)

# Print the list of files
print(jp2_files)

# Extract Blue, Green, and Red bands based on their file names
blue_band  <- jp2_files[grep("_B02_", jp2_files)]
green_band <- jp2_files[grep("_B03_", jp2_files)]
red_band   <- jp2_files[grep("_B04_", jp2_files)]
NIR_band   <- jp2_files[grep("_B08_", jp2_files)]

# List the specific raster files
file_list <- c(blue_band, green_band, red_band, NIR_band )

# Define the AOI polygon
aoi_char <- "POLYGON ((-82.25035 43.01371, -80.90025 43.01371, -80.90025 42.62048, -82.25035 42.62048, -82.25035 43.01371))"

# Read and convert the AOI polygon to an sf object
aoi <- st_as_sfc(aoi_char)
#aoi <- st_read(aoi)

# Define the initial CRS for the AOI polygon (assuming it's in WGS84, EPSG:4326)
aoi <- st_set_crs(aoi, 4326)

# Read the first raster file to get its projection
raster_file <- raster(file.path(file_list[1]))
raster_projection <- projection(raster_file)

# Reproject the AOI polygon to match the raster projection
aoi_transformed <- st_transform(aoi, raster_projection)

# Read, clip, and stack the raster files
raster_list <- lapply(file_list, function(file) {
  raster_file <- raster(file)
  raster_clipped <- crop(raster_file, st_bbox(aoi_transformed))
  return(raster_clipped)
})

raster_stack <- stack(raster_list)

# Plot the stacked raster
plot(raster_stack)

## loading in wetland site boundaries

BL_Boundary <- st_read("Data/Wetland_boundaries/BL_202307_buffer.shp")

DY_Boundary <- st_read("Data/Wetland_boundaries/DY_202308_buffer.shp")

FE_Boundary <- st_read("Data/Wetland_boundaries/FE_202306_buffer.shp")

KE_Boundary <- st_read("Data/Wetland_boundaries/KE_202307_buffer.shp")

LL_Boundary <- st_read("Data/Wetland_boundaries/LL_202308_buffer.shp")

MA_Boundary <- st_read("Data/Wetland_boundaries/MA_202308_buffer.shp")

MO_Boundary <- st_read("Data/Wetland_boundaries/MO_202308_buffer.shp")

OH_Boundary <- st_read("Data/Wetland_boundaries/OH_202308_buffer.shp")

bound_list <- list(BL_Boundary, DY_Boundary, FE_Boundary, KE_Boundary, LL_Boundary, MA_Boundary, MO_Boundary, OH_Boundary)

Wetland_Names <- c("BL", "DY" , "FE" , "KE", "LL", "MA", "MO", "OH")

names(bound_list) <- Wetland_Names
## Creating NDWI raster layer and plotting


Wetland_Raster_cropped <- lapply(bound_list, function(b){ 
  
  Wetland_rasters <- lapply(raster_list, function(r) {
    
    
    crop(x = r, y = as_Spatial(b))
    
  })
  
  blue <- Wetland_rasters[[1]]
  green <- Wetland_rasters[[2]] 
  red <- Wetland_rasters[[3]] 
  NIR <- Wetland_rasters[[4]]
  
  NDWI <- ((green - NIR)/(green + NIR))
  
  plot(NDWI)
  
  raster::hist(NDWI )
  
  return(NDWI)
 
  
})



wetland_Water_Areas <- lapply(Wetland_Raster_cropped, function(w){ 
  
    (sum(values(w) > -.2 , na.rm = TRUE))*100
})
  


list_matrix <- do.call(cbind, lapply(wetland_Water_Areas, unlist))

df <- as.data.frame(list_matrix)

#write.csv(df, file = "2019_08_28_wetland_Area.CSV")
