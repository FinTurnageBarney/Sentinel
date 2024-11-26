# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)

# Define the base URL for the Copernicus Data Space API
url_dataspace <- "https://catalogue.dataspace.copernicus.eu/odata/v1"

# Define filtering parameters
satellite <- "SENTINEL-2"
level <- "S2MSI2A"

# Define Area of Interest (AOI) in point or polygon formats
#aoi_point <- "POINT(-120.9970 37.6393)"
aoi_char <- "POLYGON ((-82.25035 43.01371, -80.90025 43.01371, -80.90025 42.62048, -82.25035 42.62048, -82.25035 43.01371))"
aoi_polygon = sf::st_as_sfc(aoi_char,crs=4326) |> sf::st_as_sf()

# Define start and end dates for data acquisition
start_date <- "2019-03-01"
end_date <- "2019-11-30"
start_date_full <- paste0(start_date, "T00:00:00.000Z")
end_date_full <- paste0(end_date, "T00:00:00.000Z")

# Construct the query to filter products based on specified parameters
query <- paste0(url_dataspace, "/Products?$filter=Collection/Name%20eq%20'", satellite, "'%20and%20Attributes/OData.CSC.DoubleAttribute/any(att:att/Name%20eq%20'cloudCover'%20and%20att/OData.CSC.DoubleAttribute/Value%20lt%2010.00)%20and%20Attributes/OData.CSC.StringAttribute/any(att:att/Name%20eq%20'productType'%20and%20att/OData.CSC.StringAttribute/Value%20eq%20'", level, "')%20and%20OData.CSC.Intersects(area=geography'SRID=4326;", URLencode(aoi_char), "')%20and%20ContentDate/Start%20gt%20", start_date_full, "%20and%20ContentDate/Start%20lt%20", end_date_full)
response <- GET(query)


# Extract and process the JSON response
response_content <- content(response, "text", encoding = "UTF-8")
response_json <- fromJSON(response_content)
result <- as.data.frame(response_json$value)

# Filter records where 'Online' column is TRUE
result <- filter(result, Online == TRUE)

result_box = do.call("rbind",lapply(1:nrow(result),function(r){
  
  result$GeoFootprint$coordinates[[r]][1,,1:2] |>
    list() |>
    sf::st_polygon() |>
    sf::st_sfc(crs=4326) |>
    sf::st_as_sf() |>
    data.table::setnames(new=c("geometry")) -> footprint
  st_geometry(footprint) <- "geometry"
  footprint = cbind(subset(result[r,],select=-c(Footprint,GeoFootprint)),footprint) |>
    sf::st_as_sf()
  
  footproj = sf::st_transform(footprint,5070)
  aoiproj = sf::st_transform(aoi_polygon,5070)
  overlap = sf::st_intersection(
    sf::st_geometry(footproj),
    sf::st_geometry(aoiproj))
  overlap_area = sf::st_area(overlap)
  aoi_area = sf::st_area(aoiproj)
  footprint$overlap_pct = as.numeric(overlap_area/aoi_area)
  
  return(footprint)
}))



# Display the first 10 results
head(result, 10)

# Authentication for accessing secured resources
username = "fturnage@uwaterloo.ca"
password = "x!6EaB2D,*qPDuf"

# Define authentication server URL
auth_server_url <- "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"

# Prepare authentication data
data <- list(
  "client_id" = "cdse-public",
  "grant_type" = "password",
  "username" = username,
  "password" = password
)

# Request access token
response <- POST(auth_server_url, body = data, encode = "form", verify = TRUE)
access_token <- fromJSON(content(response, "text", encoding = "UTF-8"))$access_token

# Set the Authorization header using obtained access token
headers <- add_headers(Authorization = paste("Bearer", access_token))

# Extract product information for the third product in the list
product_row_id <- 18 # The first image in the list 
product_id <- result[product_row_id, "Id"]
product_name <- result[product_row_id, "Name"]

# Create the URL for MTD file
url_MTD <- paste0(url_dataspace, "/Products(", product_id, ")/Nodes(", product_name, ")/Nodes(MTD_MSIL2A.xml)/$value")

# GET request for MTD file and handle redirects
response <- httr::GET(url_MTD, headers, config = httr::config(followlocation = FALSE))
print(response$status_code)

# Extract the final URL for MTD file
url_MTD_location <- response$headers[["Location"]]
print(url_MTD_location)

# Download the MTD file
file <- httr::GET(url_MTD_location, headers, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))

# Set working directory and save the MTD file
setwd("C:/Users/fturnage/OneDrive - University of Waterloo/Work/Sentinel")
outfile <- file.path(getwd(), "MTD_MSIL2A.xml")
writeBin(content(file, "raw"), outfile)

# Load XML2 library for XML processing
library(xml2)

# Read the MTD XML file
tree <- read_xml(outfile)
root <- xml_root(tree)

# Get paths for individual bands in Sentinel-2 granule
band_path <- list()
for (i in 1:4) {
  band_path[[i]] <- strsplit(paste0(product_name, "/", xml_text(xml_find_all(root, ".//IMAGE_FILE"))[i], ".jp2"), "/")[[1]]
}

# Display band paths
for (band_node in band_path) {
  cat(band_node[2], band_node[3], band_node[4], band_node[5], band_node[6], "\n")
}

# Build URLs for individual bands and download them
for (band_node in band_path) {
  url_full <- paste0(url_dataspace, "/Products(", product_id, ")/Nodes(", product_name, ")/Nodes(", band_node[2], ")/Nodes(", band_node[3], ")/Nodes(", band_node[4], ")/Nodes(", band_node[5], ")/Nodes(", band_node[6], ")/$value")
  print(url_full)
  
  # Perform GET request and handle redirects
  response <- GET(url_full, headers, config = httr::config(followlocation = FALSE))
  
  if (status_code(response) %in% c(301, 302, 303, 307)) {
    url_full_location <- response$headers$location
    print(url_full_location)
  }
  
  # Download the file
  file <- httr::GET(url_full_location, headers, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
  print(status_code(file))
  
  # Save the product
  outfile <- file.path(getwd(), band_node[6])
  writeBin(content(file, "raw"), outfile)
  cat("Saved:", band_node[6], "\n")
}

# Set the folder path
folder_path <- getwd()

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


# Extract individual bands from the stacked raster
blue <- raster_stack[[1]]
green <- raster_stack[[2]] 
red <- raster_stack[[3]] 
NIR <- raster_stack[[4]]

# Set gain for better visualization
gain <- 3

# Normalize the bands and apply gain
blue_n <- clamp(blue * gain / 10000, 0, 1)
green_n <- clamp(green * gain / 10000, 0, 1)
red_n <- clamp(red * gain / 10000, 0, 1)

# Create an RGB composite
rgb_composite_n <- brick(red_n, green_n, blue_n)

# Plot the RGB composite
plotRGB(rgb_composite_n, scale = 1, stretch = "lin")


## Creating NDWI raster layer and plotting


NDWI <- ((green - NIR)/(green + NIR))

plot(NDWI)

mean_NDWI <- cellStats(NDWI, stat = 'mean')

sd_NDWI <- cellStats(NDWI, stat = 'sd')

## reading in boundaries of the wetland shape files

BL_Boundary <- st_read("Data/Wetland_boundaries/BL_202307_buffer.shp")

DY_Boundary <- st_read("Data/Wetland_boundaries/DY_202308_buffer.shp")

FE_Boundary <- st_read("Data/Wetland_boundaries/FE_202306_buffer.shp")

KE_Boundary <- st_read("Data/Wetland_boundaries/KE_202307_buffer.shp")

LL_Boundary <- st_read("Data/Wetland_boundaries/LL_202308_buffer.shp")

MA_Boundary <- st_read("Data/Wetland_boundaries/MA_202308_buffer.shp")

MO_Boundary <- st_read("Data/Wetland_boundaries/MO_202308_buffer.shp")

OH_Boundary <- st_read("Data/Wetland_boundaries/OH_202308_buffer.shp")

### Cropping the NWDI raster to the extent of the wetland shape files

#  BL 
BL_NDWI <- crop(x = NDWI, y = as_Spatial(BL_Boundary))
plot(BL_NDWI)

# DY
DY_NDWI <- crop(x = NDWI, y = as_Spatial(DY_Boundary))
plot(DY_NDWI)

# FE
FE_NDWI <- crop(x = NDWI, y = as_Spatial(FE_Boundary))
plot(FE_NDWI)

#KE
KE_NDWI <- crop(x = NDWI, y = as_Spatial(KE_Boundary))
plot(KE_NDWI)

#LL
LL_NDWI <- crop(x = NDWI, y = as_Spatial(LL_Boundary))
plot(LL_NDWI)

#MA
MA_NDWI <- crop(x = NDWI, y = as_Spatial(MA_Boundary))
plot(MA_NDWI)

#MO
MO_NDWI <- crop(x = NDWI, y = as_Spatial(MO_Boundary))
plot(MO_NDWI)

#MO
OH_NDWI <- crop(x = NDWI, y = as_Spatial(OH_Boundary))
plot(OH_NDWI)

Wetland_list <- c(BL_NDWI, DY_NDWI, FE_NDWI, KE_NDWI, LL_NDWI, MA_NDWI, MO_NDWI, OH_NDWI)

Wetland_Names <- c("BL", "DY" , "FE" , "KE", "LL", "MA", "MO", "OH")

# creating the thresholds for what is water based on NDWI

threshold1 <- mean_NDWI + sd_NDWI

threshold2 <- mean_NDWI + (1.25 * sd_NDWI)

threshold3 <- mean_NDWI + (1.5 * sd_NDWI)

threshold4 <- mean_NDWI + (1.75 * sd_NDWI)

threshold5 <- mean_NDWI + (2 * sd_NDWI)

threshold6 <- mean_NDWI + (2.25 * sd_NDWI)

threshold7 <- mean_NDWI + (2.5 * sd_NDWI)

threshold8 <- mean_NDWI + (2.75 * sd_NDWI)

threshold9 <- mean_NDWI + (3 * sd_NDWI)


thresholds <- c(threshold1, threshold2, threshold3, threshold4, threshold5, threshold6, threshold7, threshold8, threshold9)

threshold_names <- c("threshold1", "threshold2", "threshold3", "threshold4", "threshold5", "threshold6", "threshold7", "threshold8", "threshold9")

wetland_Water_Areas <- lapply(Wetland_list, function(w){ 

  water_area <- lapply(thresholds, function(t) {
  
    (sum(values(w) > t, na.rm = TRUE))*100
  })
  
})

names(wetland_Water_Areas) <- Wetland_Names

list_matrix <- do.call(cbind, lapply(wetland_Water_Areas, unlist))

df <- as.data.frame(list_matrix)

write.csv(df, file = "2019_08_28_wetland_Area.CSV")
