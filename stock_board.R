# 嘿咻嘿咻呼呼嚕

### Load library ####
library(XML)
library(RCurl)

url <- "https://www.ptt.cc/bbs/Stock/index.html"

signatures <- system.file("CurlSSL", cainfo="cacert.pem", package="RCurl")

getLastPage <- function(url, signatures) {
  htmlElement <- getURL(paste0(url), cainfo = signatures)
  lastpage <- unlist(
    xpathSApply(htmlParse(htmlElement),  "//div[@class='btn-group btn-group-paging']/a", xmlGetAttr, "href"))[[2]]
  lastpage <- gsub(".*index", "", lastpage)
  lastpage <- as.numeric(gsub("[.]*html", "", lastpage)) + 1
  return (lastpage)
}

lastPage <- getLastPage(url, signatures)

host <- sub(url, pattern = ".html", replacement = "")

#最有貢獻王

toGetFamousResult <- function() {
  
  FamousFrame <- data.frame(user= character(0), record = character(0), stringsAsFactors=FALSE)
  FamousFrame[1, ] <- c("", as.integer(123))
  
  for( i in (lastPage - 20):lastPage) {
    tempUrl <- paste0(host, i, ".html")
    html <- htmlParse(getURL(tempUrl, cainfo = signatures))
    tempUrl.list <- unlist(xpathSApply(html, "//div[@class='r-ent']", xmlNode))
    tempUrl.list[which(names(tempUrl.list) %in% c("namespace"))] <- NULL
    
    #To get page information, and convert it into data frame
    data <- data.frame(
      xpathSApply(tempUrl.list$name, "//div[@class='nrec']", xmlValue), 
      xpathSApply(tempUrl.list$name, "//div[@class='author']", xmlValue),
      stringsAsFactors=FALSE)
    names(data) <- c("record", "user")
    
    for (i in 1:nrow(data)) {
      row <- data[i,]
      index <- FamousFrame$user %in% row$user
      if(any(index>0)) {
        FamousFrame[index ,]$record = as.integer(row$record) + as.integer(FamousFrame[index ,]$record)
      } else {
        FamousFrame[nrow(FamousFrame) + 1 , ] <- c(row$user , as.integer(row$record))
      }
    }
  }
  
  return (FamousFrame)
}





