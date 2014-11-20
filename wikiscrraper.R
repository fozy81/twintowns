#### Wikipedia twinned towns network scraping script:

### 1. Scrape wikipedia list of twinned towns 
### 2. Using twinned town names scrape individual town pages for lat/lon
### 3. Manipulate data into data.frame suitable for iGraph
### 4. Display network of connections


### Load required packages - check you have these installed ###

library(RCurl) # compose general HTTP requests 
library(stringr) # stringr is a set of simple wrappers that make R's string functions more consistent, simpler and easier to use. 
library(igraph) # for displaying graph networks


### Setup url link to twinned towns list and read page ###

path <-"http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_the_United_Kingdom"
webpage <- getURL(path)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)


### Use stringer package and regular expressions (regex) to match against html tags ###

towns <-  str_match_all(webpage, "<dd><a href=\"(.*?)\"|<li><a href=\"(.*?)\"")

towns.df <- do.call(rbind, lapply(towns, data.frame, stringsAsFactors=FALSE,check.names=F)) # turn list into data.frame

towns.df$'4' <- paste(towns.df$'2', towns.df$'3', sep="") # paste all towns into single column

getCoordinates <- lapply(towns.df$'4'[1:100], function(x){ # lapply function loops through the list of places
  
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

coordinates <- 


