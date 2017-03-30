require(RJSONIO)
require(fpc)
require(maps)
# require(mapdata)
isRStudio <- Sys.getenv("RSTUDIO") == "1"

## Example Usage ---------------------------------------------------------------------
# ## State origins and destinations
# origins <- c("raleigh, nc", "charlotte, nc", "wilmington, nc")
# destinations <- c("asheville, nc", "johnson city, tn", "eden, nc",
#                   "ahoskie, nc", "clemson, sc", "fayetteville, nc", "greensboro, nc")
# ## Download the data using specified origins and destinations
rawData <- get_api_locations(c(origins, destinations))
geoData <- parse_geoLocation(origins, destinations)
# 
# ## Generate potential routes and optimize travel plan
# plan <- genRoutes(origins, rawData, geoData)
# 
# ## genRoutes can override calculated number of routes
# plan <- genRoutes(origins, rawData, geoData, num_routes = 4)
# 
# ## genRoutes can also change the datatype between "duration" or "distance"
# plan <- genRoutes(origins, rawData, geoData, 4, datatype="distance")
# 
# ## Change the max_driving amount to calculate estimated days
# ## Default values are 4 hours for "duration" and 240 miles for "distance".
# plan <- genRoutes(Origins, rawData, geoData, max_driving=5)
# 
# ## Plans and can be called using Route plus the route number
# plan$Route3
# 
# ## Plans and can be called using Route plus the route number
# plan$Route3
# 
# ## datatype and max_driving are also stored.
# plan$datatype
# plan$max_driving


## ============================ THE MAIN FUNCTION =====================================
## Route generation and optimization
## Combines cluster analysis with route optimization functions for final output
## Analysis based on travel time. 
genRoutes <- function(origins, rawData, geoData, num_routes, datatype, max_driving) { # Return final travel plan
  data <- parse_data(rawData, datatype)
  dataForRoutes <- data
  data <- destination_data(origins, data) # Separate origins from clustering calculations
  clusterdata <- pamk(data, 2:(nrow(data)-1))  # Calculate number of clusters (routes) if not specified
  calculated_num_routes <- clusterdata$nc
  print(paste0("Estimated number of routes: ", calculated_num_routes))
  if(missing(num_routes)) {
    num_routes <- calculated_num_routes
  }


  ## Calculates Sum of Squared Error plot.  ´Elbow´ shows optimal number of clusters. 
  ## Primarily for visualization and tweating number of routes if calculated does not
  ## make sense. 
  wss <- ((nrow(data)-1)*sum(apply(data,2,var)))
  for (i in 1:(nrow(data)-1)) {
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  ## Create distance matrix for Ward hierarchical cluster analysis. 
  d <- dist(data, "euclidean") 
  ## Generate hierarchical dendrogram and highlight groupings
  fit <- hclust(d, "ward")
  
  ## Plot results
  if (isRStudio == TRUE) {graphics.off()} # I'm not sure how to get around this.
  layout(matrix(c(1,3,3,2,3,3), 2, 3, byrow = TRUE), respect=TRUE)
#   layout.show(nf)
  par(new=FALSE)
  plot(1:(nrow(data)-1), wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Sum of Squared Error")
  print(paste0("Final number of routes: ", num_routes))
  par(new=FALSE)
  plot(fit, xlab="", ylab="", main="Proposed routes", axes=FALSE)
  rect.hclust(fit, num_routes, border="red")
  
  ## Final groups
  groups <- cutree(fit, num_routes)
  groups <- as.data.frame(groups)
  groups$groups <- paste0("Route",groups$groups)
  
  ## Map the data
  par(new=FALSE)
  map_geoLocations(geoData, groups)
  
  ## Generate the optimized routes
  if (missing(datatype)) {
    datatype <- "duration" # Defaults to duration
  }
  if (missing(max_driving)) {
    if (datatype == "distance") {
      max_driving <- 240
    } else {
      max_driving <- 4 # Defaults to hours
    }
  }
  plan <- travel_routes(origins, listof_routes(groups), dataForRoutes, datatype, max_driving)

  print(paste("datatype =", datatype))
  print(paste("max_driving =", max_driving))
  return(plan)
}

## ============================ SUPPORTING FUNCTIONS ==================================
## Raw data handling ------------------------------------------------------------------
### Google Maps API Functions
gen_api_locations <- function(locations) { # Return formatted string for URL
  ## Remove commas and spaces for URL generation.
  locations <- gsub(",", "", locations)
  locations <- gsub(" ", "+", locations)
  
  ## Combine locations into API format.
  formatted <- locations[1]
  for(i in 2:length(locations)) {
    formatted <- paste(formatted, locations[i], sep="|")
  }
  return(formatted)
}

gen_api_url <- function(origins, destinations) { # Return formatted URL for distance matrix
  ## Generates Google Maps API URL for JSON data output based on origins and destinations. 
  url <- "http://maps.googleapis.com/maps/api/distancematrix/json?"
  points <- gen_api_locations(c(origins, destinations))
  
  ## All points are combined and used for Origins and Destinations in order to 
  ## generate a matrix with distance to and from all points stated. 
  url <- paste(url, "origins=", points, "&destinations=", points, "&language=en-EN&units=imperial&sensor=false", sep="")
  return(url)
}

# get_api_data <- function(origins, destinations) { # Return raw distance matrix data from JSON
#   ## Check to make sure total number of points does not exceed 10.
#   if (length(origins)+length(destinations) > 10) {
#     stop("Total number of locations cannot exceed 10 points due to API restrictions")
#   }
#   
#   ## Download API data and convert from JSON using RJSONIO library. 
#   url <- gen_api_url(origins, destinations)
#   data <- fromJSON(url)
#   return(data)
# }

gen_geoUrl <- function(location) { # Return geoLocation URL
  url <- 'http://maps.googleapis.com/maps/api/geocode/json?address='
  location <- gsub(",", "", location)
  location <- gsub(" ", "+", location)
  url <- paste0(url, location)
  return(url)
}

get_geolocation <- function(location) { # Return raw geoLocation data for parsing
  ## Download API data and convert JSON using RJSONIO library. 
  url <- gen_geoUrl(location)
  data <- fromJSON(url)
  return(data)
}

## Data parsing functions -------------------------------------------------------------
## Parsing kept separate from API download to prevent over polling API. 
parse_data <- function(data, datatype, pretty = FALSE) { # Return matrix of values
  ## Parses JSON object into a matrix of values.  Defaults to raw duration values in seconds. 
  if (data$status == "OVER_QUERY_LIMIT") {
    stop(data$error_message)}
  if(pretty == TRUE) {
    valuetype <- "text"
  } else {
    valuetype <- "value"} 
  if(missing(datatype)) {
    datatype <- "duration"
  }
  
  ## Create matrix based on number of locations
  len <- length(data$rows)
  values <- matrix(NA, nrow = len, ncol = len)
  
  ## Fill matrix with data from JSON
  for(row in 1:len) {
    for(col in 1:len) {
      values[row, col] <- eval(parse(text=paste("data$rows[[", row, "]]$elements[[", col, "]]$", datatype, "$", valuetype)))
    }
  }
  
  ## Name matrix rows and columns with location names
  rownames(values) <- data$origin_addresses
  colnames(values) <- data$destination_addresses
  
  return(values)
}

parse_geoLocation <- function(origins, destinations) { # Return matrix with latitude and longitude for locations
  ## Parses JSON object into a matrix of values.  Defaults to raw duration values in seconds. 
  if (missing(destinations)) {locations <- origins
  } else {locations <- c(origins, destinations)
  }
  ## Create matrix based on number of locations
  len <- length(locations)
  values <- as.data.frame(matrix(NA, nrow = len, 2))
  
  ## Fill matrix with data from JSON
  rnames <- as.character()
  for(i in 1:len) {
    geodata <- get_geolocation(locations[i])
    
    values[i,] <- c(geodata$results[[1]]$geometry$location) # Lat and Long
    rnames <- c(rnames, geodata$results[[1]]$formatted_address)
  }
  
  ## Name matrix rows and columns with location names
  #  values[,1:2] <- as.numeric(values[,1:2])
  values[,1] <- as.numeric(values[,1]) # Lat to numeric
  values[,2] <- as.numeric(values[,2]) # Long to numeric
  rownames(values) <- rnames
  colnames(values) <- c("Lat","Long")
  
  return(values)
}

destination_data <- function(origins, data) { # Return list of matrix row names with origins removed
  destinationData <- data[(length(origins)+1):nrow(data), ]
  return(destinationData)
}

## Route optimization ----------------------------------------------------------------
## Take group data from hierarchical clustering and convert to list
listof_routes <- function(groups) {  # Return list of destination vectors
  rows <- row.names(groups)
  routes <- list()
  for (i in (unique(groups$groups))) {
    routes <- c(routes, list(rows[groups$groups == i]))
  }
  namelist <- as.character()
  for (i in 1:length(routes)) {namelist <- c(namelist, paste0("Route", i))}
  names(routes) <- namelist # Name routes
  return(routes)
}

## Find row and column names for value in matrix.  I found this function in a forum. 
Which.names <- function(DF, value){ # Return index location in matrix for value
  ind <- which(DF==value, arr.ind=TRUE)
  return(ind)
}

## Use list to read matrix of values
optimizeRoute <- function(origins, route, data_matrix, datatype, max_driving) { # Return matrix with travel calculations
  ## Prioritize route based on origin with farthest travel
  if (length(origins)==1) {
    od <- data_matrix[1, route]
  } else {
    od <- data_matrix[1:length(origins), route]
  }
    if (length(route) > 1) { # If route only has more than one destination
            index <- Which.names(od, max(od)) # farthest point from origins
            if (length(origins) > 1) { # Route has more than one origin
              start <- route[index[1,2]] # First destination of the route
              print(paste(start, "is the farthest from",rownames(index)))
              print(paste("Optimizing route starting with",rownames(index))) 
              toFirst <- od[,start]
            } else { # Route only has one origin
              start <- route[index[1]] # First destination of the route
              print(paste(start, "requires the most travel time"))
              toFirst <- matrix(od[start], 1, 1)
              colnames(toFirst) <- names(od[start])
              rownames(toFirst) <- origins
            }
            startRoute <- start
            
            
            d <- data_matrix[route ,route] # Matrix within the route itself
            d[d==0] <- NA # Distance to self.  Assuming origin is not one of the destinations.
            d[,start] <- NA # Remove first destination values since we won't return here
            optimRoute <- as.character() 
            optimRouteValues <- as.numeric()
          
            for (i in 1:(length(route)-1)) {
              new <- d[start,]
              shortest <- min(new, na.rm=TRUE) # smallest value in that line
              index <- Which.names(new, shortest) # closest point
              stopI <- route[index[1]] # closest point becomes next stop
              optimRouteValues <- c(optimRouteValues, shortest) # paste the value into list
              d[stopI, start] <- NA # remove values so they don't interfere
              d[,stopI] <- NA # remove values
              optimRoute <- c(optimRoute, stopI) # list of names
              start <- stopI
            }
            optimRouteValues <- optimRouteValues # NA for origin to first location
            names(optimRouteValues) <- optimRoute
            
            routeMatrix <- as.matrix(toFirst)
            for (i in 1:(length(route)-1)) {
              routeMatrix <- cbind(routeMatrix, optimRouteValues[i])
            }
            if (length(origins) > 1) {backHome <- od[,stopI]}
            else {backHome <- od[stopI]} # Only one origin
    } else {
            routeMatrix <- od
            backHome <- od
            startRoute <- route # First destination of the route
    }
  routeMatrix <- cbind(routeMatrix, backHome)
  
  ## Route totals for each origin
  routeMatrix <- cbind(routeMatrix, NA)
  for (i in 1:nrow(routeMatrix)) {
    routeMatrix[i,ncol(routeMatrix)] <- sum(routeMatrix[i,1:(ncol(routeMatrix)-1)])
  }
  
  ## Convert to hours if datatype= "duration".  This is the default.
  if (datatype == "duration") {
    routeMatrix <- round(routeMatrix/3600,1) # Convert to hours
  }
  else if (datatype == "distance") {
    routeMatrix <- round((routeMatrix/1000)*0.621371,1) # Convert meters to kilometers and then to miles
  }
  else if (datatype != "duration" | datatype != "distance") {datatype <- "duration"}
  
  ## Estimated travel days based on max_driving argument.  Default is 4 hours.
  routeMatrix <- cbind(routeMatrix, NA)
  for (i in 1:nrow(routeMatrix)) {
    routeMatrix[i,ncol(routeMatrix)] <- ceiling(routeMatrix[i, (ncol(routeMatrix)-1)]/max_driving)
  }
#   routeMatrix <- matrix(paste((routeMatrix),"miles"), nrow(routeMatrix), ncol(routeMatrix)) # Pretty output
#   routeMatrix <- matrix(paste(floor(routeMatrix/3600), " hrs", round((routeMatrix/3600 - floor(routeMatrix/3600))*60), " min"), nrow(routeMatrix), ncol(routeMatrix))  

## Name the columns with location information
  if (length(route) > 1) {
    colnames(routeMatrix) <- c(startRoute, optimRoute, "Return Home", "Total", "Est. Days")
  } else {
    colnames(routeMatrix) <- c(startRoute, "Return Home", "Total", "Est. Days")
  }
  print(routeMatrix)
  return(routeMatrix)
}

## Make matrices with calculated times
travel_routes <- function(origins, routes, data_matrix, datatype, max_driving) { # Return list of optimized route matrices
  if (length(origins)==1) {
    origins <- names(data_matrix[1,])[1]    
  } else {
    origins <- rownames(data_matrix[1:length(origins),]) # Reformat for Google's output in matrix.  grep not working
  }
  for (route in 1:length(routes)) {
    print(paste0("===== Route", route, " ====="))
    output <- optimizeRoute(origins, routes[[route]], data_matrix, datatype, max_driving)
    routes[[route]] <- output
    
  }
  namelist <- as.character() 
  for (i in 1:length(routes)) {namelist <- c(namelist, paste0("Route", i))}
  names(routes) <- namelist # Name routes
  dataT <- c(datatype, max_driving)
  names(dataT) <- c("datatype", "max_driving")
  routes <- c(dataT, routes)
    
  return(routes) #List of matrices for all routes generated. 
}

## Mapping the data ------------------------------------------------------------------
map_geoLocations <- function(geoData, groups) { # Return merged geoLocation data with groups and colors
  geolocationData <- as.matrix(geoData[,1:2])
  geolocationData <- merge(geolocationData, groups, by = "row.names", all = TRUE)
  geolocationData[is.na(geolocationData)] <- "Origin"
  geolocationData$color <- "NA"
  grouplabels <- unique(geolocationData$groups)
  colorlist <- rainbow(length(grouplabels), start=.5, end=1)
  for(i in 1:length(grouplabels)) {
    geolocationData[geolocationData$groups == grouplabels[i],]['color'] <- colorlist[i]
  }
  
  labels <- unique(geolocationData[,4:5])
  labels <- labels[order(labels$groups),]
  ## , xlim=c((min(geolocationData[,3])-1),(max(geolocationData[,3])+1))
  ## Calculate states needed for map
  # statelist <- unique(geoData$State)
  
  testX <- abs(max(geolocationData[,3]) - min(geolocationData[,3]-1))
  testY <- abs(max(geolocationData[,2]) - min(geolocationData[,2]-1))
  if (testX > 7 | testY > 7) {map("state", # Draw whole US map if lat and long bounds are greater than 7x7 
                                  add=FALSE
                                 )  
  } else if (testX > 3 | testY > 3) {
    map("state", #regions= region,
        omi=c(1,2,1,2), 
        fill=FALSE,
        boundary=1,
        myborder=c(0,0),
        xlim=c((min(geolocationData[,3])-1),(max(geolocationData[,3])+1)), # "Zoom" onto map
        ylim=c((min(geolocationData[,2])-1),(max(geolocationData[,2])+1)),
        add=FALSE,
    )    
    } else {
      map("county",
          omi=c(1,2,1,2), 
          fill=FALSE,
          boundary=1,
          myborder=c(0,0),
          xlim=c((min(geolocationData[,3])-.2),(max(geolocationData[,3])+.2)), # "Zoom" onto map
          ylim=c((min(geolocationData[,2])-.2),(max(geolocationData[,2])+.2)),
          add=FALSE,
      ) 
    }
  
  title(main="Map of groupings", 
        sub="based on travel time between locations",
        cex.sub=.7)
  points(geolocationData$Long, geolocationData$Lat,
         pch=19,
         cex=1.2,
         col=geolocationData$color
  )
  text(geolocationData$Long, 
       geolocationData$Lat, 
       labels=geolocationData[,1],
       pos=1,
       cex=.7)
  legend("bottomleft",
         legend=labels$groups,
         col=labels$color,
         pch=19,
         cex=.9,
         bty="o",
         bg="#FFFFFF",
         title="Groupings",
         fill=FALSE,
         density=.3,
  )
#   for (i in grouplabels) {
#     if (i != "Origin") {
#       text(mean(geolocationData[geolocationData$groups == i,][,3]), 
#            max(geolocationData[,2])+.3, 
#            labels= i,
#            col= geolocationData[geolocationData$groups == i,]$color,
#            cex=.7)}
#   }
  return(geolocationData)
}













