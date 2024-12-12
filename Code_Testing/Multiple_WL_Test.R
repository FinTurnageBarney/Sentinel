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
library(lubridate)
library(purrr)


#define scene folders

folder_paths <- list.files(path = "Data/Scenes/2020_2021/All_Scenes/Cloudcover40", full.names = TRUE)

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
  
  # creating a stack of raster bands. Used for initial testing 
  
  #raster_stack_10m <- raster::stack(raster_list[1:7])
  
  #picking out one band to use with resampling later
  
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
  
  # Resampling the 20 meter SCL layer to make it compatible with the 10 meter layers
  
  SCL_Remove_10m <- raster::resample(SCL_Remove, blue, method = "ngb")
  
  # now overlaying these bad values on my NDWI raster to remove any issues caused by clouds/bad data/ shadows etc
  
  cropped_Bands <- lapply(raster_list, function(ras){
    
    new_raster <- raster::overlay(ras, SCL_Remove_10m, fun = function(x, y) {
      x[!is.na(y)] <- NA
      return(x)
    })
    return(new_raster)
    
  })
  
  #stacking the cropped bands for testing plotting and stuff
  
  bands_10m_cropped <- raster::stack(cropped_Bands)
  
  #plot test
  
  raster::plot(bands_10m_cropped)
  
  # redundant separation of bands for raster math
  
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
  
  # Scaline up the NDWI raster values to be compatible with the OTSU package that I found. 
  
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
  
  # Making a list of Boundaries
  
  bound_list <- list(BL_Boundaries_10m, DY_Boundaries_10m, FE_Boundaries_10m, KE_Boundaries_10m, LL_Boundaries_10m, MO_Boundaries_10m, MO_Boundaries, OH_Boundaries_10m)
  
  # naming the boundaries
  
  Wetland_Names <- c("BL", "DY" , "FE" , "KE", "LL", "MA", "MO", "OH")
  
  #applying names
  
  names(bound_list) <- Wetland_Names
  
  # rbinding the boundary list
  
  bound_list <- do.call("rbind",bound_list)
  
  #naming the boundary dataframe
  
  bound_list$Wetland_Name = rownames(bound_list)
  
  # cropping the NDWI raster to the wetlands area
  
   Wetland_rasters <- lapply(1:nrow(bound_list), function(i) {
    rasters <- raster::crop(x = NDWI, y = bound_list[i,])
   })
  
   
  #Wetland_rasters <- for(i in (1:length(bound_list))) {
    #rasters <- raster::crop(x = NDWI, y = bound_list$geometry[i])
    #return(rasters)
  #}
  
  # Naming the wetland rasters
   
  names(Wetland_rasters) <- rownames(bound_list)
  
  # calculating Area using individual Thresholds 
   
  Otsu_Areas <- lapply(Wetland_rasters, function(wl) {
    
    # making wetland raster data a matrix
    
    wl.dat <- as.matrix(wl)  
    
    #print(hist(wl.dat))
    
    # extract range of data for otsu
    
    wl.range <- c(range(wl.dat))
    
    # do otsu if wl data has no NA values
    
    if (!is.na(any(wl.range))) {
      
      # get threshold from otsu
      
      th <- EBImage::otsu(wl.dat, range = wl.range )
      
      #abline(v = th)
      
      #calculate area based on TH
      
      area <- (sum(wl.dat > th , na.rm = TRUE))*100
      
    }
  })
  
  # creating histograms from wetland rasters
  
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
  
  # makebinary  rasters of the wetlands after the OTSU thresholds have been applied
  
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
  
  # set up a list to extract all of the stuff we just created
  
  combo_list <- c(list(Otsu_Areas), list(Wetland_rasters), list(bound_list), list(Otsu_Histograms) , list(Th_Raster), NDWI )
  
  #name each list item that is extracted
  
  names(combo_list) <- c("Otsu_Areas", "WL_Rasters", "WL_Boundaries", "Otsu_Histograms", "TH_Rasters", "NDWI_Raster")
  
  return(combo_list)
  
  
  
})  



# function to keep the last 10 characters of the folder path names. Makes each name just the date of the scene.

keep_last_10 <- function(x) {
  substr(x, nchar(x) - 9, nchar(x))
}

#apply function to output list.

names(Water_area_per_Scene) <- keep_last_10(folder_paths)



# Loading in wl boundries.

Boundary <- Water_area_per_Scene$`2019-07-04`$WL_Boundaries

# double loop function that plots the wetland NDWI raster, Binary raster and the histogram all together. 

for(z in 1:(length(Water_area_per_Scene))){
  
  #print loop iteration
  
  print(z)
  
  #loads loop data to an object for next loop
  
  Current_Scene <- Water_area_per_Scene[z]
  
  Current_Date <- Current_Scene[[1]]

  
  for(i in 1:(length(Current_Date$Otsu_Areas))){
    
    #Current_Date <- Current_Scene[[1]]
    
    print(i)
    
    #create bounding box for plot
    
    bbox <- sf::st_bbox(Boundary[i,])
    
    #read histogram for current loop
    
    Hist <- Current_Date$Otsu_Histograms[i]
    
    #pull area value
    
    Area <- Current_Date$Otsu_Areas
    
    # pull WL NDWI raster
    
    WL_Raster <- Current_Date$WL_Rasters[[i]]
    
    # pull WL Bindary TH raster
    
    TH_Raster <- Current_Date$TH_Rasters[[i]]
    
    # Name each sites
    
    Site_Names <- names(Current_Date$Otsu_Areas)
    
    #skip function if no data for binary raster.
    
    if(is.null( TH_Raster)){
      next
    }
    
    else
      
      # set up plots for each wetland
      
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
      
      
    # combine all the data

    
    Ostu.Data.Combined <- c(Hist ,  list(WL_Raster_plt) , list(TH_Raster_plt) )
    
    # set up names for plot columns
    
    column_labels <- list(
      textGrob( paste0("Scene: " , names(Current_Scene),"    WL Site:", Site_Names[i]) , gp = gpar(fontsize = 15, fontface = "bold")))
    
    #arrage the grob for plotting
    
    combined_grob <- arrangeGrob(
      grobs = c( column_labels, Ostu.Data.Combined ),  # Combine labels and plots
      nrow = 2,  # 1 row for labels, 3 rows for plots
      ncol = 2,  # Number of columns
      widths = unit(c(15, 15), "cm"), heights = unit(c(15,15), "cm"), # Adjust height ratio: small height for labels
      padding = unit(2, "line")) 
  
    #plot the grobs all together  
    
      grid.draw(combined_grob)
    
      # save each plot
      
      #ggsave( paste0("Result_Plotting/OTSU_Multiple_WL_Individual_TH/",names(Water_area_per_Scene),"/" ,names(Hist),".PNG"), combined_grob, width = 13, height = 13)
      
      next

  } 
  
 return(NULL)
  
  
}


### Now working on Full Scene Level Otsu for the Wetlands


# this loop here makes the full scene level otsu thresholds

Scene_Thresholds <- list()

for(z in 1:(length(Water_area_per_Scene))){
  
  #print current loop
  
  print(z)
  
  # pull current loops list of data
  
  Current_Scene <- Water_area_per_Scene[z]
  
  Current_Date <- Current_Scene[[1]]
  
  #pull the rasters for each wetland.
  
  WL_Rasters <- Current_Date$WL_Rasters

  #loop to pull all the area values for each raster
  
  NDWI_Raster_Values <- lapply(WL_Rasters, function(i){
    
    
    Areas <- raster::values(i)
    
    return(Areas)
  
    })
  
  # make all values in one list
  
  NDWI_Raster_Values <- unlist(NDWI_Raster_Values)

  # remove all the NA values
  
  NDWI_Raster_Values <- NDWI_Raster_Values[!is.na(NDWI_Raster_Values)]
  
  # SAve as a matrix
  
  NDWI_Raster_Values <- as.matrix(NDWI_Raster_Values)
  
  # get range of values for OTSU
  
  wl.range <- (range(NDWI_Raster_Values))
  
  #calculate the OTSU TH for all wetlands values together
  
  th <- EBImage::otsu(NDWI_Raster_Values, range = wl.range )

  #save the thresholds
  
  Scene_Thresholds[z] <- th
  
  
}


# Making names better again

names(Scene_Thresholds) <- keep_last_10(folder_paths)


# this loop just makes the full scene level NDWI rasters easier to work with for the next loop. Just making a list of only the raster layers.

# just pulling out the NDWI rasters from the nested lists.

NDWI_Rasters <- lapply(Water_area_per_Scene, function(m){
  
  Current_Scene <- m
  # 6 is the list item of the NDWI rasters
  NDWI_Raster <- Current_Scene[[6]]
  
  return(NDWI_Raster)
  
})



# Now we are thresholding each full scene raster.

#blank list to add stuff to

Full_Scene_TH_Results <- list()

# loop function to make binary rasters of the full sentinel scenes using the thresholds from the previous loop. 

for(f in 1:(length(Water_area_per_Scene))){

  #if the values are above the threshold they equal water, if not they are 0s
  
  wl.th = raster::calc(x = NDWI_Rasters[[f]], fun = function(x){
    ifelse(x >= Scene_Thresholds[[f]] ,1,0 )
  })
  
  # print the th
  
  print(wl.th)
  
  # add the results to the empty list
  
  Full_Scene_TH_Results <- append(Full_Scene_TH_Results,wl.th)
  
  
}


# making histograms for the full scene thresholded rasters.
# Using to see if there is a good double peak distribution

NDWI_Full_Scene_Otsu_Histograms <- lapply(NDWI_Rasters, function(wl) {
  
  #saving each cropped wetland NDWI raster to a matrix for compatability with otsu function
  
  #grabbing values from each loop
  
  wl.dat <- raster::values(wl[[1]])
  
  #remove NA values
  
  wl.dat <- na.omit(wl.dat) 
  
  #make into matrix format
  wl.dat <- matrix(wl.dat) 
  
  #calculating range of data for otsu function input
  
  wl.range <- c(range(wl.dat))
  
  
  #check to not run on NA sites where clouds or other sat errors occured
  
    
    #take the threshold from the otsu results 
    
    th <- EBImage::otsu(wl.dat, range = wl.range )
    
    #making dataframe of values to use with histogram
    
    hist.df <- as.data.frame(raster::values(wl))
    
    # making the histograms for each site that has data in the scene
    
    p <- ggplot2::ggplot(hist.df, ggplot2::aes(x=`raster::values(wl)`))+ 
      geom_histogram(color="black", fill="white", binwidth=1)+
      geom_vline(xintercept = th, color = "blue", size=1)
    
    p <- ggplotGrob(p)
    
    #naming plot according to wetland name
    
    plt_name <- names(wl)
    
    
    #jpeg(paste0(plt_name), width = 800, height = 600, units = "px", res = 100)  # Open JPEG device
    #grid.draw(p)  # Draw the grob to the device
    #dev.off()
    
    return(p)

  
})


# loop to write the rasters for visualization

for (p in 1:(length(NDWI_Rasters))){
  
  writeRaster(Full_Scene_TH_Results[[p]], filename = (names(NDWI_Rasters[p])), format = "GTiff", overwrite = TRUE)
  
  
}


# simple loop to make rgb rasters for visualization

RGB_Rasters <- lapply(folder_paths, function(folder_path){

  # getting band paths
  
raster_bands <-  list.files(path = paste0(folder_path), full.names = TRUE)

#get rgb bands

R1 <- raster::raster(raster_bands[5])
G1 <- raster::raster(raster_bands[4])
B1 <- raster::raster(raster_bands[3])


#stack bands

rgb_stack <- raster::stack(R1,G1,B1)

# create names again
fname <- keep_last_10(folder_path)


#write raster
writeRaster(rgb_stack, filename = fname, format = "GTiff", overwrite = TRUE)

})


raster::plot(RGB_Rasters[[1]])


# loading in merged boundary data set for testing OTSU data further

Merge_Boundaries <- sf::st_read("Data/12_06/WL_Merge.shp")


# subsetting to only include wetlands above certain sizes

Merge_Boundaries_1500 <- Merge_Boundaries[Merge_Boundaries$Area > 1500,]

Merge_Boundaries_2000 <- Merge_Boundaries[Merge_Boundaries$Area > 2000,]


# loop to select only the areas overlapping to check histogram distributions




Values_2018_2019 <- lapply(NDWI_Rasters, function(WL){
  
  test <-  raster::mask( WL , Merge_Boundaries )
  
  values <- raster::values(test)
  
  test <- values[which(values > 0)]
  
  return(test)
  
})


Values1500_2018_2019 <- lapply(NDWI_Rasters, function(WL){
  
  test <-  raster::mask( WL , Merge_Boundaries_1500 )
  
  values <- raster::values(test)
  
  test <- values[which(values > 0)]
  
  return(test)
  
})


Values2000_2018_2019 <- lapply(NDWI_Rasters, function(WL){
  
  test <-  raster::mask( WL , Merge_Boundaries_2000 )
  
  values <- raster::values(test)
  
  test <- values[which(values > 0)]
  
  return(test)
  
})


NDWI_Hist_Group_2018_2019 <- unlist(Values_2018_2019)

NDWI_Hist_Group_1500_2018_2019 <- unlist(Values1500_2018_2019)

NDWI_Hist_Group_2000_2018_2019 <- unlist(Values2000_2018_2019)



print(hist(NDWI_Hist_Group_2018_2019, breaks = 100))

print(hist(NDWI_Hist_Group_1500_2018_2019, breaks = 100))

print(hist(NDWI_Hist_Group_2000_2018_2019, breaks = 100))


## Now choosing only spring months for potential seasonal thresholding


dates <- names(NDWI_Rasters) %>%
  ymd() # Converts strings to Date objects


# Extract the month from the dates
months <- month(dates)

# selected motnhs for seasonal th
target_months <- c(2, 3, 4, 5)

# filtering command

NDWI_Rasters_filtered <- NDWI_Rasters[months %in% target_months]


# histograms of filtered ndwi rasters


Values_Filtered_NDWI <- lapply(NDWI_Rasters_filtered, function(WL){
  
  test <-  raster::mask( WL , Merge_Boundaries )
  
  values <- raster::values(test)
  
  test <- values[which(values > 0)]
  
  
  
  return(test)
  
})


Values_Filtered_NDWI_1500 <- lapply(NDWI_Rasters_filtered, function(WL){
  
  test <-  raster::mask( WL , Merge_Boundaries_1500 )
  
  values <- raster::values(test)
  
  test <- values[which(values > 0)]
  
  return(test)
  
})


Values_Filtered_NDWI_2000 <- lapply(NDWI_Rasters_filtered, function(WL){
  
  test <-  raster::mask( WL , Merge_Boundaries_2000 )
  
  values <- raster::values(test)
  
  test <- values[which(values > 0)]
  
  return(test)
  
})


NDWI_Hist_Filtered <- unlist(Values_Filtered_NDWI)

NDWI_Hist_Filtered_1500 <- unlist(Values_Filtered_NDWI_1500)

NDWI_Hist_Filtered_2000 <- unlist(Values_Filtered_NDWI_2000)

print(hist(NDWI_Hist_Filtered, breaks = 100))

print(hist(NDWI_Hist_Filtered_1500, breaks = 100))

print(hist(NDWI_Hist_Filtered_2000, breaks = 100))

