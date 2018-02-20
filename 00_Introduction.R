# 19/02/2018 Charlotte's code

# Construction of RLQ matrices. 

# Extract sites long and lat from GBR surveys data 

# Things to take into consideration
# a) Missing trait variables (see how to estimate, 
# can use mean but I think it would be better touse other methods)
# b) Species names in gbr data are shortened, this needs to be rectified


library(tidyverse)
library(dplyr)

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











# #########
# L Matrix 
# #########
# Site (rows) x species abundance (columns)


# Read in gbr surveys data 

gbr <- read.csv("data/gbr_surveys_ld.csv")

head(gbr)
str(gbr)

data <- as_data_frame(gbr)

# Find out how many columns are in the data 

ncol(data)
# 405 

# Extract species names

cols1 <- select(data,porites.massive:acropor.multiacu)

colnames(cols1)

# #########
# Q matrix 
# #########
# Species (rows) x trait (columns)


# Example download and data manipulation on physiological traits from coraltraits.org
# Download trait data for symbiodium density (Trait # 135)
data1 <- read.csv("https://coraltraits.org/traits/135.csv", as.is=TRUE)

data <- data1
str(data)

# Select out colomns of interest 
# tr1cols <- select(data1, specie_name, trait_name, standard_unit, methodology_name, value)


library(reshape2)

# Develop your aggregation rules function for the "acast" function
my_aggregate_rules <- function(x) {
if (length(x) > 1) {               # Does a species by trait combination have more than 1 value?
x <- type.convert(x, as.is=TRUE)
if (is.character(x)) {
return(x[1])                   # If values are strings (characters), then return the first value
} else {
return(as.character(mean(x)))  # If values are numbers, then return the mean (converted back to character)
}
} else {
return(x)                        # If a species by trait combination has 1 value, then just return that value 
}
}

# Reshape your data using "acast".  Fill gaps with NAs
data_reshaped <- acast(data, specie_name~trait_name, value.var="value", fun.aggregate=my_aggregate_rules, fill="")

data_reshaped[data_reshaped == ""] <- NA

# If desired, convert the reshaped data into a data frame for analysis in R
data_final <- data.frame(data_reshaped, stringsAsFactors=FALSE)

# Extract symbiodinium density
data_symden <- select(data_final, Symbiodinium.density)

# Note that all variables are still character-formatted.  Use as.numeric() and as.factor() accordingly.  For example,
data_symden$Symbiodinium.density <- as.numeric(data_symden$Symbiodinium.density)


# Now download another set of trait data
# Download trait data for tissue thickness (Trait # 132)
data2 <- read.csv("https://coraltraits.org/traits/132.csv", as.is=TRUE)
str(data2)

# Reshape your data using "acast".  Fill gaps with NAs
data_reshaped2 <- acast(data2, specie_name~trait_name, value.var="value", fun.aggregate=my_aggregate_rules, fill="")

data_reshaped2[data_reshaped2 == ""] <- NA

# If desired, convert the reshaped data into a data frame for analysis in R
data_final2 <- data.frame(data_reshaped2, stringsAsFactors=FALSE)

# Extract tissue thickness
data_thick<- select(data_final2, Tissue.thickness)

# Note that all variables are still character-formatted.  Use as.numeric() and as.factor() accordingly.  For example,
data_thick$Tissue.thickness <- as.numeric(data_thick$Tissue.thickness)


