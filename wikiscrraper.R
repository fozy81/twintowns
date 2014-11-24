#### Wikipedia twinned towns network scraping script:

### 1. Scrape wikipedia list of twinned towns 
### 2. Using twinned town names scrape individual town pages for lat/lon
### 3. Manipulate data into data.frame suitable for iGraph
### 4. Display network of connections


### Load required packages - check you have these installed ###

library(RCurl) # compose general HTTP requests 
library(stringr) # stringr is a set of simple wrappers that make R's string functions more consistent, simpler and easier to use. 
library(igraph) # for displaying graph networks
library(reshape2) # for pivoting data.frame
library(httpuv)

### Setup url link to twinned towns list and read page ###

path <-"http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_the_United_Kingdom"
webpage <- getURL(path)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)


### Use stringer package and regular expressions (regex) to match against html tags ###

towns <-  str_match_all(webpage, "<dd><a href=\"(.*?)\"|<li><a href=\"(.*?)\"")

towns.df <- do.call(rbind, lapply(towns, data.frame, stringsAsFactors=FALSE,check.names=F)) # turn list into data.frame

towns.df$'4' <- paste(towns.df$'2', towns.df$'3', sep="") # paste all towns into single column

getCoordinates <- lapply(towns.df$'4'[1:length(towns.df$'4')], function(x){ # lapply function loops through the list of places
  
  path <- paste("http://en.wikipedia.org",x,sep="")  # append this url with the town name     
  webpage <- getURL(path)
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  latlon <-  str_match_all(webpage, "lat\":(.*?),\"|lon\":(.*?),\"|<title>(.*?)-") # match on lat and lon regex (maybe looking for 'coordinates' will work better for regional areas but need to convert to decimal degrees)
  latlon.df <- do.call(rbind, lapply(latlon, data.frame, stringsAsFactors=FALSE,check.names=F)) 
  Sys.sleep(0.2) # sleep between page requests so not overload wikipedia with pages requests although I'm sure they can cope!
return(latlon.df) 
})

coordinates <- do.call(rbind, lapply(getCoordinates, data.frame, stringsAsFactors=FALSE,check.names=F)) # make list into data.frame
coordinates[,3] <- gsub("}","",coordinates[,3] ) # remove trailing '}' which I couldn't work out how remove during earlier regex 

### Prepare dataframe already for igraph package ###

## loop to fill in blank place names

for (i in 1:length(coordinates$'4')){  

  if (coordinates$'4'[i] == ""){
    
    up <- i - 1
    coordinates$'4'[i] <- coordinates$'4'[up] 
  }
    else  coordinates$'4'[i] 
}


## loop to move lat lon into single column

for (i in 1:length(coordinates$'2')){  
  
  if (coordinates$'2'[i] == ""){
     
    coordinates$'2'[i] <- coordinates$'3'[i] 
  }
  else  coordinates$'2'[i] 
}


## loop to move title into single column

for (i in 1:length(coordinates$'2')){  
  
  if (coordinates$'2'[i] == ""){
    
    coordinates$'2'[i] <- coordinates$'4'[i] 
  }
  else  coordinates$'2'[i] 
}

## Title to name

for (i in 1:length(coordinates$'1')){  
  
  if (grepl("[i]", coordinates$'1'[i]) == TRUE){
    
    coordinates$'1'[i] <- "Name"
  }
 else  coordinates$'1'[i] 
}

## Lat / Lon title

for (i in 1:length(coordinates$'1')){  
  
  if (grepl("(lat)", coordinates$'1'[i]) == 1){
    
    coordinates$'1'[i] <- "Lat"
  }
  if (grepl("(lon)", coordinates$'1'[i]) == 1){
    
    coordinates$'1'[i] <- "Lon"
  }
  
  else  coordinates$'1'[i] 
}

coordinates$'3' <- NULL # remove redundant column

coordinates$'1' <- as.factor(coordinates$'1')
coordinates$'4' <- as.factor(coordinates$'4')
coordinates$'2' <- as.factor(coordinates$'2')

names(coordinates) <- c('columns', 'rows',"towns")

coordinates$towns <- gsub("^\\s+|\\s+$","",coordinates[,3] ) 


### Join with towns.df

for (i in 1:length(towns.df$'3')){  
    
  if (towns.df$'3'[i] == ""){
    
    up = i - 1
    towns.df$'3'[i] <- towns.df$'3'[up] 
  }
  else  towns.df$'3'[i] 
}

towns.df$'3' <- gsub("/wiki/", "", towns.df$'3')
towns.df$'4' <- gsub("/wiki/", "", towns.df$'4')

towns <- as.data.frame(towns.df[,3:4])

towns$'4' <- decodeURI(towns$'4')
towns$'3' <- decodeURI(towns$'3')
towns$'4' <- gsub("_", " ", towns$'4')
towns$'3' <- gsub("_", " ", towns$'3')

towns <- towns[!towns$'3' == towns$'4',]  

test <- merge(towns, coordinates[coordinates$columns == 'Lat',], by.x="3",by.y="towns", all.x=T)
test <- merge(test, coordinates[coordinates$columns == 'Lon',], by.x="3",by.y="towns", all.x=T)
test <- merge(test, coordinates[coordinates$columns == 'Lat',], by.x="4",by.y="towns", all.x=T)
test <- merge(test, coordinates[coordinates$columns == 'Lon',], by.x="4",by.y="towns", all.x=T)

df.cols <- grep('columns', names(test),invert=T) 
test <- test[, c(df.cols)] 

test2 <- test[complete.cases(test),]
test3 <- test2[! duplicated(test2[,c(2,3,4,5,6)]),]
names(test3) <- ("town", "townUK","latuk","lonuk","lat","lon")
test3$geom <- paste("LINESTRING ("  "")" )

### iGraph

g <- graph.data.frame(d = towns[1:1000,1:2], directed = FALSE)

tkplot(g, vertex.label = V(g)$name)
myc <- clusters(g, mode="strong")
mycolor <- c(1:373)
V(g)$color <- mycolor[myc$membership + 1]
tkplot(g, layout=layout.fruchterman.reingold)
plot(g, layout=layout.reingold.tilford)