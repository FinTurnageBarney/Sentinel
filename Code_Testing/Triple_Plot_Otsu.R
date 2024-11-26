# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(raster)
library(sf)
library(EBImage)
library(ggplot2)
library(tmap)
library(gridExtra)
library(grid)

#define scene folders

folder_paths <- list.files(path = "Data/Scenes/2018_2019/All_Scenes/Cloudcover40", full.names = TRUE)

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
  
  #raster_stack_10m <- raster::stack(raster_list[1:7])
  
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
  
  # resampling SCL layer to be at 10m resolution to match the rest of the rasters
  
  SCL_Remove_10m <- raster::resample(SCL_Remove, blue, method = "ngb")
  
  # now overlaying these bad values on my NDWI raster to remove any issues caused by clouds/bad data/ shadows etc
  
  cropped_Bands <- lapply(raster_list, function(ras){
    
    new_raster <- raster::overlay(ras, SCL_Remove_10m, fun = function(x, y) {
      x[!is.na(y)] <- NA
      return(x)
    })
    return(new_raster)
    
  })
  
  # stacking cropped bands
  
  bands_10m_cropped <- raster::stack(cropped_Bands)
  
  #plotting all the bands
  
  #raster::plot(bands_10m_cropped)
  
  # making separate rasters for the layers needed to calculated NDWI
  
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
  
  BL_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/BL_50M_Buffer.shp")
  
  DY_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/DY_50M_Buffer.shp")
  
  FE_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/FE_50M_Buffer.shp")
  
  KE_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/KE_50M_Buffer.shp")
  
  LL_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/LL_50M_Buffer.shp")
  
  MA_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/MA_50M_Buffer.shp")
  
  MO_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/MO_50M_Buffer.shp")
  
  OH_Boundary <- sf::st_read("Data/Wetland_boundaries/20240906_New_Bounds/OH_50M_buffer.shp")
  
  # creating a list of all the site boundaries
  
  bound_list <- list(BL_Boundary, DY_Boundary, FE_Boundary, KE_Boundary, LL_Boundary, MA_Boundary, MO_Boundary, OH_Boundary)
  
  # Creating a list of the site names
  
  Wetland_Names <- c("BL", "DY" , "FE" , "KE", "LL", "MA", "MO", "OH")
  
  # naming the list to know which site is associated with each area
  
  names(bound_list) <- Wetland_Names
  
  # cropping the NDWI raster to the wetlands area
  
  Wetland_rasters <- lapply(bound_list, function(b) {
    
    
    rasters <- raster::crop(x = NDWI, y = sf::as_Spatial(b))
    
    
  })
  
  
  Otsu_Histograms <- lapply(Wetland_rasters, function(wl) {
  
  #saving each cropped wetland NDWI raster to a matrix for compatability with otsu function
  
  wl.dat <- raster::as.matrix(wl)  
  
  #calculating range of data for otsu function input
  
  wl.range <- c(range(wl.dat))
  
  
  #check to not run on NA sites where clouds or other sat errors occured
  
  if (!is.na(any(wl.range))) {
  
  #take the threshold from the otsu results 
    
  th <- EBImage::otsu(wl.dat, range = wl.range )
  
  #making dataframe of values to use with histogram
  
  hist.df <- as.data.frame(raster::values(wl))
  
  # making the histograms for each site that has data in the scene
  
  p <- ggplot(hist.df, aes(x=`raster::values(wl)`))+ 
    geom_histogram(color="black", fill="white", binwidth=1)+
    geom_vline(xintercept = th, color = "blue", size=1)
    
  return(p)
  }
  
  })
  
  
  # function to calculate otsu threshold for each wetland
  
  Otsu_Areas <- lapply(Wetland_rasters, function(wl) {
    
    
    wl.dat <- raster::as.matrix(wl)  
    
    #print(hist(wl.dat))
    
    wl.range <- c(range(wl.dat))
    
    
    if (!is.na(any(wl.range))) {
      
      th <- EBImage::otsu(wl.dat, range = wl.range )
      
      #abline(v = th)
      
      area <- (sum(wl.dat > th , na.rm = TRUE))*100  
      
    }
  })
  
  # Code to convert rasters to show what OTSU thinks is water vs not water
  
  Th_Raster <- lapply(Wetland_rasters, function(wl) {
    
    # saving site NDWI raster as matrix
    
    wl.dat <- raster::as.matrix(wl)  
    
    #calculating range for otsu function
    
    wl.range <- c(range(wl.dat))
    
    #check to not run on NA sites where clouds or other sat errors occured
    
    if (!is.na(any(wl.range))) {
      
      #take the threshold from the otsu results
      
      th <- EBImage::otsu(wl.dat, range = wl.range )
      
      #assign 1 or 0 values based on threshold from otsu to show calculated water areas.
      wl.th = raster::calc(x = wl, fun = function(x){
        ifelse(x >= th ,1,0 )
        
      })
      
    }
  })
  
  #plotting OTSU result rasters in TMap then converting to a format that is compatible with 
  
  Otsu_Tmap_Raster <- lapply(Th_Raster, function(wl) {
    #skip if Null
    if (!is.null(wl)) {
    Otsu_Tmap <- tm_shape(wl)+
      tm_raster(palette="cividis", legend.show = FALSE)+
      tm_layout(legend.outside = TRUE, asp = 0.75, main.title = "OTSU Raster")
    # making the plots grobs for gg plot format compatability
    tmap_grob(Otsu_Tmap)
    }
  })
  
  #plotting NDWI rasters in TMap then converting to a format that is compatible with 
  
  NDWI_Tmap_Raster <- lapply(Wetland_rasters, function(wl) {
    
    #making the plots
    
    NDWI_Tmap <- tm_shape(wl)+
                 tm_raster(palette="cividis", legend.show = FALSE)+
                 tm_layout(legend.outside = TRUE, asp = 0.75, main.title = "NDWI Raster")
    
    # making the plots grobs for gg plot format compatability
    tmap_grob(NDWI_Tmap)
   
  })
  
# combining the 3 kinds of plot into 1 list of plots  
  
Ostu.Dat.Combined <- c(Otsu_Histograms,NDWI_Tmap_Raster ,Otsu_Tmap_Raster )
  
# making labels for each site for plotting all together

column_labels <- list(
  textGrob("BL", gp = gpar(fontsize = 15, fontface = "bold")),
  textGrob("DY", gp = gpar(fontsize = 15, fontface = "bold")),
  textGrob("FE", gp = gpar(fontsize = 15, fontface = "bold")),
  textGrob("KE", gp = gpar(fontsize = 15, fontface = "bold")),
  textGrob("LL", gp = gpar(fontsize = 15, fontface = "bold")),
  textGrob("MA", gp = gpar(fontsize = 15, fontface = "bold")),
  textGrob("MO", gp = gpar(fontsize = 15, fontface = "bold")),
  textGrob("OH", gp = gpar(fontsize = 15, fontface = "bold"))
)


# assembling combined plots all together
  
combined_grob <- arrangeGrob(
  grobs = c(column_labels, Ostu.Dat.Combined),  # Combine labels and plots
  nrow = 4,  # 1 row for labels, 3 rows for plots
  ncol = 8,  # Number of columns
  heights = c(0.1, 1, 1, 1)  # Adjust height ratio: small height for labels
)  

# Printing combined plot
 
grid.draw(combined_grob)


#function to only keep the date for each image to label them appropriately

keep_date <- function(x) {
  rev(strsplit(x,"/")[[1]])[1]
}

#using function to keep only the dates portion of the file path

dates <- keep_date(folder_path)

#saving the array of result rasters for each site as pngs

ggsave(paste0("Result_Otsu/2018_2019_plots/50M/",keep_date(folder_path),".PNG"), combined_grob, width = 20, height = 10)

return(NULL)

})  
