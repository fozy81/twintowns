library(XML)
library(RCurl)

path <-"http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_the_United_Kingdom#Conwy"
webpage <- getURL(path)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
tablehead <- xpathSApply(pagetree, "//li", xmlValue)
tablehead2 <- xpathSApply(pagetree, "//dd", xmlValue)
tablehead3 <- xpathSApply(pagetree, "//li|//dd", xmlValue)
class(pagetree)