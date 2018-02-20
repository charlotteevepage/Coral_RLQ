# ########
# R Matrix 
# ########
# Site (rows) x environmental variables (columns) 


# ----------------------------------------
# Data extraction 1. Sites from GBR surveys
# ----------------------------------------

# Read in gbr surveys data 

gbr <- read.csv("data/gbr_surveys_ld.csv")

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

# For example: data on salinity 






# Thermal History products offered by NOAA coral reef watch 

#E.g 1.	SST trend for 1985-2012, Pathfinder ~4km dataset (netCDF files) 
# a)	Warm season trend 
# b)	Annual trend 
# c)	Warmest month trend

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)

ncin <- nc_open(ncfname)
print(ncin)









