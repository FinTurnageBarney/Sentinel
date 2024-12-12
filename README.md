Sentinel Small Waterbody Remote Sensing using R


R project code includes 2 main scripts in the Code_Testing folder

The first is to download desired sentinel imagery using copernicus API. 

The second script is my testing script that is used to calculate NDWI rasters, use OTSU, load in shapefiles for wetlands. 

There is also a folder named “Old” within code testing that contains various other scripts that I used to test different code and methods etc. 


To install and run the project all of the libraries needed for the R code are listed at the top of each of the scripts. 

You also need one library that has to be installed using BiocManager. To install this you run this code: 

	if (!require("BiocManager", quietly = TRUE))
   	 install.packages("BiocManager")

BiocManager::install("EBImage")




Script 1: “PullingMultipleRasterallbands”

To use this script effectively you need to customize a few things. The aoi polygon, start and end dates, amount of cloud cover and folder pathing is the most important stuff. 

You also need to add in your own API download login information


Script 2: “Multiple_WL_Test”

The main thing you need to make this script work is correcting linking the file paths to load the satellite data. 

For this script you need to run the EBI image library download command as this is used for the OTSU functions.


