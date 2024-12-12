# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)

# Define the  URL for the Copernicus Data Space API

url_dataspace <- "https://catalogue.dataspace.copernicus.eu/odata/v1"

# Define satellite product

satellite <- "SENTINEL-2"
level <- "S2MSI2A"

# Define Area of Interest (AOI) polygon

aoi_char <- "POLYGON ((-82.25035 43.01371, -80.90025 43.01371, -80.90025 42.62048, -82.25035 42.62048, -82.25035 43.01371))"
aoi_polygon = sf::st_as_sfc(aoi_char,crs=4326) |> sf::st_as_sf()

# Define Data time period

start_date <- "2020-10-01"
end_date <- "2021-09-30"
start_date_full <- paste0(start_date, "T00:00:00.000Z")
end_date_full <- paste0(end_date, "T00:00:00.000Z")

# define maximum cloud cover variable


cloudcover <- 40

# Construct the query to filter products based on parameters

query <- paste0(url_dataspace, "/Products?$filter=Collection/Name%20eq%20'", satellite, "'%20and%20Attributes/OData.CSC.DoubleAttribute/any(att:att/Name%20eq%20'cloudCover'%20and%20att/OData.CSC.DoubleAttribute/Value%20lt%20", cloudcover ,")%20and%20Attributes/OData.CSC.StringAttribute/any(att:att/Name%20eq%20'productType'%20and%20att/OData.CSC.StringAttribute/Value%20eq%20'", level, "')%20and%20OData.CSC.Intersects(area=geography'SRID=4326;", URLencode(aoi_char), "')%20and%20ContentDate/Start%20gt%20", start_date_full, "%20and%20ContentDate/Start%20lt%20", end_date_full)
response <- httr::GET(query)

# Extract and process the JSON response

response_content <- httr::content(response, "text", encoding = "UTF-8")
response_json <- jsonlite::fromJSON(response_content)

# continue adding on json results to get all possible images

resultlist = list()
resultlist[[1]] = as.data.frame(response_json$value)
nextisvalid = !is.null(response_json$`@odata.nextLink`)
nextindex = 1
while(nextisvalid){
  nextindex = nextindex + 1
  print(nextindex)
    
  response <- httr::GET(response_json$`@odata.nextLink`)
  
  # Extract and process the JSON response
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  response_json <- jsonlite::fromJSON(response_content)
  resultlist[[nextindex]] = as.data.frame(response_json$value)
  
  nextisvalid = !is.null(response_json$`@odata.nextLink`)
}


for(res in 1:length(resultlist)){
  print(paste0("res=",res))
  if(!("sf" %in% class(resultlist[[res]]))){
    if(!is.null(dim(resultlist[[res]][,13]))){
      resultlist[[res]][,13] <- resultlist[[res]][,13][,1]
    }
    resultlist[[res]] <- do.call("rbind",lapply(1:nrow(resultlist[[res]]),function(r){
      print(paste0("r=",r))
      coord = resultlist[[res]]$GeoFootprint$coordinates[[r]]
      if(length(dim(coord))==3){coord = coord[1,,1:2]}
      if(length(dim(coord))==4){coord = coord[1,,,1:2]}
      coord |>
        list() |>
        sf::st_polygon() |>
        sf::st_sfc(crs=4326) |>
        sf::st_as_sf() |>
        data.table::setnames(new=c("geometry")) -> footprint
      st_geometry(footprint) <- "geometry"
      footprint = cbind(subset(resultlist[[res]][r,],select=-c(Footprint,GeoFootprint)),footprint) |>
        sf::st_as_sf()
      
    # create new variable that measures overlap of AOI and images
      
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
  }
}

# bind lists of images together

result <- do.call("rbind",resultlist)

# Filter records where 'Online' column is TRUE
result <- filter(result, Online == TRUE)


# filter list to have images that cover most of AOI polygon
result <-  subset(result, result$overlap_pct > .5)

# add a date column
result$Date <- as.Date(result$ContentDate)


# Authentication for accessing secured resources
username = 
password = 
  
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
access_token <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))$access_token

# Set the Authorization header using access token
headers <- add_headers(Authorization = paste("Bearer", access_token))




for (x in 1:nrow(result)) {
  
  # Extract product information for each scene
 
  product_id <- result$Id[x]
  product_name <- result$Name[x]
  
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
  folder_name <- paste0(getwd(), "/Data/Scenes/2020_2021/MNDWI_Scenes/Cloudcover40/", result$Date[x])
  
  if (!dir.create(folder_name, showWarnings = FALSE)) {
    message("Folder already exists or could not be created: ", folder_name)
    next
  }
  
  #define metadata file path
  outfile <- file.path(folder_name, "MTD_MSIL2A.xml")
  # write metadatafile
  writeBin(content(file, "raw"), outfile)
  
  # Load XML2 library for XML processing
 
  
  # Read the MTD XML file
  tree <- read_xml(outfile)
  root <- xml_root(tree)
  
  # Get paths for individual bands in Sentinel-2 granule
  band_path <- list()
  
  bands = xml2::xml_text(xml2::xml_find_all(root, ".//IMAGE_FILE"))
  for (i in grep("_10m|SCL_20m",bands)) {
    band_path[[i]] <- strsplit(paste0(product_name, "/", xml2::xml_text(xml2::xml_find_all(root, ".//IMAGE_FILE"))[i], ".jp2"), "/")[[1]]
  }
  band_path = band_path[which(sapply(band_path,function(b){!is.null(b)}))]
  
  # Display band paths
  for (band_node in band_path) {
    cat(band_node[2], band_node[3], band_node[4], band_node[5], band_node[6], "\n")
  }
  
  # Build URLs for individual bands and download them
  for (band_node in band_path) {
    url_full <- paste0(url_dataspace, "/Products(", product_id, ")/Nodes(", product_name, ")/Nodes(", band_node[2], ")/Nodes(", band_node[3], ")/Nodes(", band_node[4], ")/Nodes(", band_node[5], ")/Nodes(", band_node[6], ")/$value")
    print(url_full)
    
    # Perform GET request and handle redirects
    response <- httr::GET(url_full, headers, config = httr::config(followlocation = FALSE))
    
    if (status_code(response) %in% c(301, 302, 303, 307)) {
      url_full_location <- response$headers$location
      print(url_full_location)
    }
    
    # Download the file
    file <- httr::GET(url_full_location, headers, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
    print(status_code(file))
    
    # Save the product
    outfile <- file.path(paste0(getwd(), "/Data/Scenes/2020_2021/MNDWI_Scenes/Cloudcover40/", result$Date[x]), band_node[6])
    writeBin(content(file, "raw"), outfile)
    cat("Saved:", band_node[6], "\n")
  }
}



