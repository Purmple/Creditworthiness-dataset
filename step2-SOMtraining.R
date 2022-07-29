# ------------------- SOM TRAINING ---------------------------

#choose the variables with which to train the SOM
#the following selects column 2,4,5,8
data_train <- dataSet[, c(2,3,5,42,45)]

# now train the SOM using the Kohonen method
data_train_matrix <- as.matrix(scale(data_train))
names(data_train_matrix) <- names(data_train)
require(kohonen)
x_dim=20
y_dim=20
if (small_areas){
  # larger grid for the small areas example (more samples)
  som_grid <- somgrid(xdim = x_dim, ydim=y_dim, topo="hexagonal")  
} else {
  som_grid <- somgrid(xdim = x_dim/2, ydim=y_dim/2, topo="hexagonal")  
}

# Train the SOM model!
if (packageVersion("kohonen") < 3){
  system.time(som_model <- som(data_train_matrix, 
                             grid=som_grid, 
                             rlen=1000, 
                             alpha=c(0.9,0.01),
                             n.hood = "circular",
                             keep.data = TRUE ))
}else{
  system.time(som_model <- som(data_train_matrix, 
                             grid=som_grid, 
                             rlen=1000, 
                             alpha=c(0.9,0.01),
                             mode="online",
                             normalizeDataLayers=false,
                             keep.data = TRUE ))
}
summary(som_model)
rm(som_grid, data_train_matrix)
