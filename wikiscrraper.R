library(XML)
library(RCurl)

path <-"http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_the_United_Kingdom"
webpage <- getURL(path)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

library(stringr)
#town <- str_match_all(webpage, "<li><a href=\"(.*?)\"") 
#twins <- str_match_all(webpage, "<dd><a href=\"(.*?)\"")
all <-  str_match_all(webpage, "<dd><a href=\"(.*?)\"|<li><a href=\"(.*?)\"")
#town.df <- do.call(rbind, lapply(town, data.frame, stringsAsFactors=FALSE,check.names=F)) 
#twins.df <- do.call(rbind, lapply(twins, data.frame, stringsAsFactors=FALSE,check.names=F)) 
all.df <- do.call(rbind, lapply(all, data.frame, stringsAsFactors=FALSE,check.names=F)) 
all.df$'4' <- paste(all.df$'2', all.df$'3', sep="") # paste all towns into single column

getCoordinates <- lapply(all.df$'4'[1:100], function(x){
  
  path <- paste("http://en.wikipedia.org",x,sep="")
  
   webpage <- getURL(path)
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  all <-  str_match_all(webpage, "lat\":(.*?),\"|lon\":(.*?),\"|<title>(.*?)-")
  all.df <- do.call(rbind, lapply(all, data.frame, stringsAsFactors=FALSE,check.names=F)) 
 # all.df[1] <- paste(x)
  Sys.sleep(0.2)
return(all.df)
})

coordinates <- do.call(rbind, lapply(getCoordinates, data.frame, stringsAsFactors=FALSE,check.names=F)) 
coordinates[,3] <- gsub("}","",coordinates[,3] )

,"wgCoordinates":{"lat":49.9,"lon":10.9},


links <- matched[[1]][, 1]

pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
tablehead <- xpathSApply(pagetree, "//href", xmlValue)
tablehead2 <- xpathSApply(pagetree, "//dd", xmlValue)
tablehead3 <- xpathSApply(pagetree, "//a", xmlValue)
class(pagetree)