library(XML)
library(RCurl)

path <-"http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_the_United_Kingdom"
webpage <- getURL(path)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

library(stringr)
matched <- str_match_all(webpage, "<dd><a href=\"(.*?)\"")
linkslist<- do.call(rbind, lapply(matched, data.frame, stringsAsFactors=FALSE,check.names=F)) 
links <- matched[[1]][, 1]

pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
tablehead <- xpathSApply(pagetree, "//href", xmlValue)
tablehead2 <- xpathSApply(pagetree, "//dd", xmlValue)
tablehead3 <- xpathSApply(pagetree, "//a", xmlValue)
class(pagetree)