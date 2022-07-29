# Preprocessing

### LOAD LIBRARIES - install with:
#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos"))
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

### DATA PREPARATION

# Census data comes in counts of people per area. 
# To compare areas, we will convert a number of the
# stats collected into percentages. Without this, 
# the primary differentiator between the different 
# areas would be population size.

# Load the data into a data frame
# Get the map of these areas and filter for Dublin areas.

data_raw <- read.csv("./creditworthiness.csv")  
#idcol="functionary"
#names(data_raw)[1] <- "functionary"
dataSet <- data_raw[!(data_raw$credit.rating==0),]
for (i in 1:45){
  print(cor(dataSet[46], dataSet[i] ))
}
#data <- data[c(1, 40:46)]
