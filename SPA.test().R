#This is an example of how the function works. 
#Within the example, I use Census data for the population of Camden, London. 
#The area of Camden is formatted in the British National grid system. 

#Load Packages
setwd('mdecara_pols0010')
library(tmap)
library(leaflet)
library(rgdal)
library(rgeos)
library(sp)
library(RColorBrewer)
library(spdep)

#Load data
Census.Data <-read.csv("worksheet_data/camden/practical_data.csv")
Output.Areas <- readOGR("worksheet_data/camden", "Camden_oa11")
#Merge data
data <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

SPA.test <- function(data, variable, variable_name, neighbour, d1, d2, k, style, method){
  
  #Step 1 - Plot basic template of area and chosen variable to give an idea 
  #to the user of map and variable presence.
  
  print(tm_shape(data) + 
          tm_fill(variable_name, palette = "Reds", 
                  style = "quantile", title = "% of chosen variable") + 
          tm_borders(alpha=.4))

  #Step 2 - Function identifies neighbours by chosen measure and adds chosen 
  #weight style to then plot and summarize the results. 
  #In this case: ‘queens’ measure.  
  
  if(neighbour == 'queens'){
    #create neighbour ‘nb’ type object 
    N <- poly2nb(data, queen = TRUE)
    #summary of object
    summary.nb(N)
    #plot of neighbour object
    plot(data, border = 'lightgrey')
    plot(N, coordinates(data), add=TRUE, col='blue')
    title(main='Neighbour Map by Queens Case')
    #create list of neighbour object with chosen weight. This case: ‘W’
    listw <- nb2listw(N, style = style)
    #summary of list
    summary.listw(listw)
    
  } else if(neighbour == 'rooks'){
    N <- poly2nb(data, queen = FALSE) 
    #Plot and Print Neighbour Outcome
    summary.nb(N)
    plot(data, border = 'lightgrey')
    plot(N, coordinates(data), add=TRUE, col='red')
    title(main='Neighbour Map by Rooks Case')
    listw <- nb2listw(N, style = style)
    summary.listw(listw)
    
  } else if(neighbour == 'distance'){
    N <- dnearneigh(coordinates(data), d1, d2) 
    #Plot and Print Neighbour Outcome
    summary.nb(N)
    plot(data, border = 'lightgrey')
    plot(N, coordinates(data), add=TRUE, col='green')
    title(main='Neighbour Map by Distance')
    listw <- nb2listw(N, style = style)
    summary.listw(listw)
    
  } else if(neighbour == 'knn'){
    N <- knearneigh(coordinates(data), k = k) 
    #Plot and Print Neighbour Outcome
    plot(data, border = 'lightgrey')
    plot(knn2nb(N), coordinates(data), add=TRUE, col='orange')
    title(main='Neighbour Map by K Nearest Neighbours')
    N <- knn2nb(N)
    summary.nb(N)
    listw <- nb2listw(N, style = style)
    summary.listw(listw)
  }
  
  #Step 3 - Function then runs the chosen spatial autocorrelation test, 
  #prints global and local results and plots certain graphs. 
  #In this case: everything under ‘Morans I’ method.
  
  if(method == 'Morans I'){
    test_scores <- moran.test(variable, listw)
    print(test_scores)
    if(test_scores$estimate['Moran I statistic'] > 0){
      print(paste("It can be said there is POSITIVE spatial autocorrelation with a Global Moran I statistic of: ",test_scores$estimate['Moran I statistic'],sep=""))
    } else if(test_scores$estimate['Moran I statistic'] == 0) {
      print(paste("It can be said there is NO spatial autocorrelation with a Global Moran I statistic of: ",test_scores$estimate['Moran I statistic'],sep=""))
    } else if(test_scores$estimate['Moran I statistic'] < 0) {
      print(paste("It can be said there is NEGATIVE spatial autocorrelation with a Global Moran I statistic of: ",test_scores$estimate['Moran I statistic'],sep=""))
    }
    if(test_scores$statistic['Moran I statistic standard deviate'] >= 1.96){
      print(paste('It can be said the data is significantly CLUSTERED with a z-score of:', test_scores$statistic['Moran I statistic standard deviate'], sep=''))
    } else if(test_scores$statistic['Moran I statistic standard deviate'] <= -1.96){
      print(paste('It can be said the data is significantly DISPERSED with a z-score of:', test_scores$statistic['Moran I statistic standard deviate'], sep=''))
    } else if(test_scores$statistic['Moran I statistic standard deviate'] < 1.96 & test_scores$statistic['Moran I statistic standard deviate'] > -1.96){
      print(paste('It can be said the data is spatially RANDOM with a z-score of:', test_scores$statistic['Moran I statistic standard deviate'], sep=''))
    } 
    if(test_scores$p.value <= 0.10){
      print(paste("It can also be said the model IS statistically significant with a Global P-value of: ",test_scores$p.value, sep=""))
    } else {
      print(paste("It can also be said the model IS NOT statistically significant with a Global P-value of: ",test_scores$p.value, sep=""))
    }
  
    #creates moran scatter plot
    moran <- moran.plot(variable, listw = nb2listw(N, style = style), asp = T)
    
    #creates local moran results plot
    local <- localmoran(x = variable, listw = nb2listw(N, style = style))
    moran.map <- data
    moran.map@data <- cbind(data@data, local)
    print(tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "Local Moran Statistic", midpoint = NA))

    
    #Step 4 - Creates a LISA cluster map  
    quadrant <- vector(mode="numeric",length=nrow(local))
    #centers the variable of interest around its mean
    m.variable <- variable - mean(variable)     
    #centers the local Moran's around the mean
    m.local <- local[,1] - mean(local[,1])    
    #significance threshold
    signif <- 0.1 
    #builds a data quadrant
    quadrant[m.variable < 0 & m.local< 0] <- 1      
    quadrant[m.variable < 0 & m.local> 0] <- 2
    quadrant[m.variable > 0 & m.local< 0] <- 3
    quadrant[m.variable > 0 & m.local> 0] <- 4 
    quadrant[local[,5] > signif] <- 0   
    #plot
    breaks <- c(0,1,2,3,4)
    colors <- c("white", "blue", rgb(0,0,1, alpha=0.4), rgb(1,0,0, alpha=0.4), "red")
    plot(data, border = "lightgray", col = colors[findInterval(quadrant, breaks, all.inside = FALSE)])
    box()
    legend("bottomleft", legend = c("insignificant", "low-low", "low-high", "high-low", "high-high"),
           fill = colors, bty = "n")
    legend('topright', legend = 'LISA cluster map', bty = 'n')
  } 
  
  else if (method == 'Gearys C'){
    test_scores <- geary.test(variable, listw)
    print(test_scores)
    if(test_scores$estimate['Geary C statistic'] == 1){
      print(paste("It can be said there is NO spatial autocorrelation with a Global Geary C statistic of: ",test_scores$estimate['Geary C statistic'],sep=""))
    } else if(test_scores$estimate['Geary C statistic'] > 1) {
      print(paste("It can be said the variable is NEGATIVELY spatially autocorrelated with a Global Geary C statistic of: ",test_scores$estimate['Geary C statistic'],sep=""))
    } else if(test_scores$estimate['Geary C statistic'] < 1) {
      print(paste("It can be said the variable is POSITIVELY spatially autocorrelated with a Global Geary C statistic of: ",test_scores$estimate['Geary C statistic'],sep=""))
    }
    if(test_scores$p.value <= 0.05){
      print(paste("It can also be said the model IS statistically significant with a Global P-value of: ",test_scores$p.value, sep=""))
    } else {
      print(paste("It can also be said the model IS NOT statistically significant with a Global P-value of: ",test_scores$p.value, sep=""))
    }
  }
  
  else if(method == 'Getis Ord'){
    test_scores <- globalG.test(variable, listw)
    print(test_scores)
    if(test_scores$estimate['Global G statistic'] == 0){
      print(paste("It can be said the variable has NO spatial autocorrelation with a Global Getis Ord G statistic of: ",test_scores$estimate['Global G statistic'],sep=""))
    } else if(test_scores$estimate['Global G statistic'] > 0) {
      print(paste("It can be said the variable is POSITIVELY spatially autocorrelated with a Global Getis Ord G statistic of: ",test_scores$estimate['Global G statistic'],sep=""))
    } else if(test_scores$estimate['Global G statistic'] < 0) {
      print(paste("It can be said the variable is NEGATIVELY spatially autocorrelated with a Global Getis Ord G statistic of: ",test_scores$estimate['Global G statistic'],sep=""))
    }
    if(test_scores$statistic['standard deviate'] > 0){
      print(paste('It can be said the HIGH VALUES are clustered within the data with a z-score of:', test_scores$statistic['standard deviate'], sep=''))
    } else if(test_scores$statistic['standard deviate'] < 0){
      print(paste('It can be said the LOW VALUES are clustered within the data with a z-score of:', test_scores$statistic['standard deviate'], sep=''))
    } 
    if(test_scores$p.value <= 0.05){
      print(paste("It can also be said the model IS statistically significant with a Global P-value of: ",test_scores$p.value, sep=""))
    } else {
      print(paste("It can also be said the model IS NOT statistically significant with a Global P-value of: ",test_scores$p.value, sep=""))
    }
    
    local_g <- localG(variable, listw)
    print('This is the summary of the local G statistic:') 
    print(summary(local_g))
    local_g_sp <- data
    local_g_sp@data <- cbind(data@data, as.matrix(local_g))
    names(local_g_sp)[6] <- "gstat"
    #plot
    print(tm_shape(local_g_sp) + tm_fill("gstat", palette = "RdBu", style = "pretty", title = 'gstat') + tm_borders(alpha=.4))
  }
}

#This will be parameters imputed into the function for demonstration purposes. 
#Therefore, every result displayed by the function in the code below will 
#be a result of only these parameters. 

SPA.test(data = data, 
         variable = data$Qualification,
         variable_name = 'Qualification',
         neighbour = 'queens', 
         d1 = 0, d2 = 800, k = 4,
         style = 'W',
         method = 'Morans I')

#Note: parameters ‘d1,d2 & k’ will simply not be run as they are dependent on 
#other inputs such as distance or knn measure of neighbours.
