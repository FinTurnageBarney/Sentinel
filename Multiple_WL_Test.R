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
library(tmaptools)

#define scene folders

folder_paths <- list.files(path = "Data/Scenes/2018_2019/All_Scenes/Cloudcover10", full.names = TRUE)

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
  
  BL_Boundaries <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/BL_max/BL_max.shp")
  
  DY_Boundaries <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/DY_max/DY_max.shp")
  
  FE_Boundaries <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/FE_max/FE_max.shp")
  
  KE_Boundaries <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/KE_max/KE_max.shp")
  
  LL_Boundaries <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/LL_max/LL_max.shp")
  
  MA_Boundaries <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/MA_max/MA_max.shp")
  
  MO_Boundaries <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/MO_max/MO_max.shp")
  
  
  #Buffering all of the wetlaound outlines from apoorva
  
  buffer_dist <- 10
  
  BL_Boundaries_10m <- sf::st_buffer(BL_Boundaries, buffer_dist)
  
  DY_Boundaries_10m <- sf::st_buffer(DY_Boundaries, buffer_dist)

  FE_Boundaries_10m <- sf::st_buffer(FE_Boundaries, buffer_dist)
  
  KE_Boundaries_10m <- sf::st_buffer(KE_Boundaries, buffer_dist)
  
  LL_Boundaries_10m <- sf::st_buffer(LL_Boundaries, buffer_dist)
  
  MA_Boundaries_10m <- sf::st_buffer(MA_Boundaries, buffer_dist)
  
  MO_Boundaries_10m <- sf::st_buffer(MO_Boundaries, buffer_dist)
  
  
  
  
  
  
  # this one came with the 10m buffer so I will not be doing the buffer on it
  
  OH_Boundaries_10m <- sf::st_read("Data/Wetland_boundaries/Apoorva_New_Boundaries/OH_10m_buffer/OH_10m_buffer.shp")
  
  
  bound_list <- list(BL_Boundaries_10m, DY_Boundaries_10m, FE_Boundaries_10m, KE_Boundaries_10m, LL_Boundaries_10m, MO_Boundaries_10m, MO_Boundaries, OH_Boundaries_10m)
  
  
  Wetland_Names <- c("BL", "DY" , "FE" , "KE", "LL", "MA", "MO", "OH")
  
  names(bound_list) <- Wetland_Names
  
  
  
  bound_list <- do.call("rbind",bound_list)
  
  bound_list$Wetland_Name = rownames(bound_list)
  
  # cropping the NDWI raster to the wetlands area
  
   Wetland_rasters <- lapply(1:nrow(bound_list), function(i) {
    rasters <- raster::crop(x = NDWI, y = bound_list[i,])
   })
  
   
  #Wetland_rasters <- for(i in (1:length(bound_list))) {
    #rasters <- raster::crop(x = NDWI, y = bound_list$geometry[i])
    #return(rasters)
  #}
  
  
  names(Wetland_rasters) <- rownames(bound_list)
  
  
  
  Otsu_Areas <- lapply(Wetland_rasters, function(wl) {
    
    
    wl.dat <- as.matrix(wl)  
    
    #print(hist(wl.dat))
    
    wl.range <- c(range(wl.dat))
    
    
    if (!is.na(any(wl.range))) {
      
      th <- EBImage::otsu(wl.dat, range = wl.range )
      
      #abline(v = th)
      
      area <- (sum(wl.dat > th , na.rm = TRUE))*100
      
    }
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
      
      p <- ggplot2::ggplot(hist.df, ggplot2::aes(x=`raster::values(wl)`))+ 
        geom_histogram(color="black", fill="white", binwidth=1)+
        geom_vline(xintercept = th, color = "blue", size=1)
      
      
      p <- ggplotGrob(p)
      
      
      return(p)
    }
  })
  
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
  
  

  #Otsu_Areas <- unlist(Otsu_Areas)
  
  #Wetland_rasters
  
  #bound_list
  
  
  combo_list <- c(list(Otsu_Areas), list(Wetland_rasters), list(bound_list), list(Otsu_Histograms) , list(Th_Raster) )
  
  
  names(combo_list) <- c("Otsu_Areas", "WL_Rasters", "WL_Boundaries", "Otsu_Histograms", "TH_Rasters")
  
  return(combo_list)
  
  
  
})  





keep_last_10 <- function(x) {
  substr(x, nchar(x) - 9, nchar(x))
}

names(Water_area_per_Scene) <- keep_last_10(folder_paths)





Boundary <- Water_area_per_Scene$`2019-07-04`$WL_Boundaries



for(z in 1:(length(Water_area_per_Scene))){
  
  print(z)
  
  Current_Scene <- Water_area_per_Scene[z]
  
  Current_Date <- Current_Scene[[1]]

  
  for(i in 1:(length(Current_Date$Otsu_Areas))){
    
    #Current_Date <- Current_Scene[[1]]
    
    print(i)
    
    bbox <- sf::st_bbox(Boundary[i,])
    
    Hist <- Current_Date$Otsu_Histograms[i]
    
    Area <- Current_Date$Otsu_Areas
    
    WL_Raster <- Current_Date$WL_Rasters[[i]]
    
    TH_Raster <- Current_Date$TH_Rasters[[i]]
    
    Site_Names <- names(Current_Date$Otsu_Areas)
    
    
    if(is.null( TH_Raster)){
      next
    }
    
    else
      
    #if (!is.null(TH_Raster)) {
      TH_Raster_plt <- tm_shape(TH_Raster) +
        tm_raster(palette="cividis", legend.show = TRUE) +
        tm_shape(Boundary) +
        tm_borders(lwd = 2, col = "blue", alpha = 0.8) +
        tm_layout(legend.outside = TRUE, legend.outside.position = "right", asp = 0.75, main.title = "OTSU Raster")
      # making the plots grobs for gg plot format compatability
      
      TH_Raster_plt <- tmap_grob(TH_Raster_plt)
      
      #return(TH_Raster_plt)
    #}
    
    
    #if (!is.null(WL_Raster)) {
      
      WL_Raster_plt <- tm_shape(WL_Raster)+
        tm_raster(palette="cividis", legend.show = TRUE)+
        tm_shape(Boundary) +
        tm_borders(lwd = 2, col = "blue", alpha = 0.8)+
        tm_layout(legend.outside = TRUE, legend.outside.position = "right", asp = 0.75, main.title = "NDWI Raster")
      # making the plots grobs for gg plot format compatability
      
      WL_Raster_plt <- tmap::tmap_grob(WL_Raster_plt)
      
      #return(WL_Raster_plt)
    #}
      
      
    

    
    
    #raster_bands <-  list.files(path = "Data/Scenes/2018_2019/All_Scenes/Test/2019-07-04", full.names = TRUE)
    
    #R1 <- raster::raster(raster_bands[5])
    #G1 <- raster::raster(raster_bands[4])
    #B1 <- raster::raster(raster_bands[3])

    
     #r_crop <- raster::crop(R1, bbox)
     #g_crop <- raster::crop(G1, bbox)
     #b_crop <- raster::crop(B1, bbox)
     
    
    #rgb_stack <- raster::stack(r_crop,g_crop,b_crop)
    
    #Basemap  <- recordPlot({(raster::plotRGB(rgb_stack, r = 1, g = 2, b = 3,
                     #scale = 800,
                     #stretch = "lin"))})
    
    #ggsave( paste0("Result_Plotting/OTSU_Multiple_WL_Individual_TH/",names(Water_area_per_Scene),"/" ,names(Hist),"_Raster_Basemap.PNG"), Basemap, width = 13, height = 13)                 
                     
                     
   # Basemap <- grob(Basemap)
    
   # Basemap <- 
      #tm_shape(rgb_stack) +
       #tm_rgb( r = 1,
                #g = 2 ,
               # b = 3,
               # alpha = NA,
               # saturation = 1,
               # interpolate = TRUE,
                #max.value = 65535) +
       # tm_shape(Boundary) +
       # tm_borders(lwd = 2, col = "red")
    
   # Basemap <- tmap::tmap_grob(Basemap)
    
    
    Ostu.Data.Combined <- c(Hist ,  list(WL_Raster_plt) , list(TH_Raster_plt) )
    
    column_labels <- list(
      textGrob( paste0("Scene: " , names(Current_Scene),"    WL Site:", Site_Names[i]) , gp = gpar(fontsize = 15, fontface = "bold")))
    
    
    combined_grob <- arrangeGrob(
      grobs = c( column_labels, Ostu.Data.Combined ),  # Combine labels and plots
      nrow = 2,  # 1 row for labels, 3 rows for plots
      ncol = 2,  # Number of columns
      widths = unit(c(15, 15), "cm"), heights = unit(c(15,15), "cm"), # Adjust height ratio: small height for labels
      padding = unit(2, "line")) 
      
      grid.draw(combined_grob)
    
      
      ggsave( paste0("Result_Plotting/OTSU_Multiple_WL_Individual_TH/",names(Water_area_per_Scene),"/" ,names(Hist),".PNG"), combined_grob, width = 13, height = 13)
      
      next

  } 
  
 return(NULL)
  
  
}


### Now working on Full Scene Level Otsu for the Wetlands


Scene_Thresholds <- data.frame(row.names = (keep_last_10(folder_paths)) )

for(z in 1:(length(Water_area_per_Scene))){
  
  print(z)
  
  Current_Scene <- Water_area_per_Scene[z]
  
  Current_Date <- Current_Scene[[1]]
  
  WL_Rasters <- Current_Date$WL_Rasters

  NDWI_Raster_Values <- lapply(WL_Rasters, function(i){
    
    
    
    Areas <- raster::values(i)
    
    return(Areas)
  
    })
  
  NDWI_Raster_Values <- unlist(NDWI_Raster_Values)

  NDWI_Raster_Values <- NDWI_Raster_Values[!is.na(NDWI_Raster_Values)]
  
  NDWI_Raster_Values <- as.matrix(NDWI_Raster_Values)
  
  wl.range <- (range(NDWI_Raster_Values))
  
  th <- EBImage::otsu(NDWI_Raster_Values, range = wl.range )

  Scene_Thresholds[z] <- th
}





#output <- do.call("rbind", lapply(folder_paths, function(f){
#  s = unlist(Water_area_per_Scene[[f]])
#  d = data.frame(area=s,wetland=names(s))
#  d$image = keep_last_10(f)
#  return(d)
#}))


#write.csv(output, file = "Otsu_Results_2018_2019_custom_buffers.csv")








