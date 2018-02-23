# ########
# R Matrix 
# ########
# Site (rows) x environmental variables (columns) 


# ----------------------------------------
# Data extraction 1. Sites from GBR surveys
# ----------------------------------------
library(tidyverse)
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

# Read in gbr surveys data 

gbr <- read.csv("Data/gbr_surveys_ld.csv")

# Look at the structure of GBR survey data
head(gbr)
str(gbr)

# Create a tibble with the data frame
data <- as_data_frame(gbr)

# Extract the columns of interest from the data frame
sites <- select(data, reef.key, reef.name, lat, long)

# How many repeat transects do we have per site? 
# n = 110 sites, max repeat = 31
summarisedsites <- count(data, reef.name)


# --------------------------------------
# Data extraction 2. Environmental Data   
# --------------------------------------

# Thermal History products offered by NOAA coral reef watch 

# E.g. 	SST Variability for 1985-2012, Pathfinder ~4km dataset (netCDF files)

# a)	Warm season variability 
# b)	Annual variability 
# c)	Warmest month variability 


ncin <- nc_open("Data/noaa_crw_thermal_history_variability.nc")
print(ncin)

{
sink("Thermal_History_Variability.txt")
print(ncin)
sink()
}

# Get a list of variables in the file 

variables <- names(ncin[['var']])
variables

# Get the longitudes and latitudes 

lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))



# get a variable e.g "Warm-season variability"
ndvi.array <- ncvar_get(ncin, "stdv_warmseason")
dim(ndvi.array)

# What fill value was used for missing data?
fillvalue <- ncatt_get(ncin, "stdv_warmseason", "_FillValue")
fillvalue

# Get the long name of the variable
dlname <- ncatt_get(ncin, "stdv_warmseason", "long_name")
dlname

# Get the units of the variable
dunits <- ncatt_get(ncin, "stdv_warmseason", "units")
dunits


# We can then close the file 

nc_close(ncin)

########################
# Working with this data
########################

# First lets replace fill values ("0") in this case with NA 
ndvi.array[ndvi.array == fillvalue$value] <- NA

dim(ndvi.array)

# Data represents one time slice
# We can now save this data as a raster 

r <- raster(t(ndvi.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))



# May want to add reef and continent shape files.

plot(r)

plot(r)

# -------------------------------
# Extracting data at a study site 
# -------------------------------

# First, we will need to convert the entire 3d array of data to a raster brick.
# Note, this step may take several minutes.

r_brick <- brick(ndvi.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
       

# Build a vector of loop that takes long and lat of each reef and gives you value + puts in a dataframe
# But to check if it works, first take long and lat of first reef, Pethebridge reef


# Organise long and lat into a spatial object 

# Doesn't work - maybe go lat and long mixed up!
coords<-data.frame(lon = sites[,4], lat=sites[,3])

coordinates(coords) <- c("long", "lat")

points <- SpatialPoints(coords=coords, proj4string = CRS("+proj=longlat +datum=WGS84"))


class (points)

plot(points, pch=16, cex=0.5)



warmseason <- extract(r, points, method='simple')

warmseason


        