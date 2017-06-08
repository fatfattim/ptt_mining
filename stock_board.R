# 嘿咻嘿咻呼呼嚕

### Load library ####
library(XML)
library(RCurl)

url <- "https://www.ptt.cc/bbs/Stock/index.html"

getLastPage <- function(url) {
  signatures <- system.file("CurlSSL", cainfo="cacert.pem", package="RCurl")
  htmlElement <- getURL(paste0(url), cainfo = signatures)
  lastpage <- unlist(
    xpathSApply(htmlParse(htmlElement),  "//div[@class='btn-group btn-group-paging']/a", xmlGetAttr, "href"))[[2]]
  lastpage <- gsub(".*index", "", lastpage)
  lastpage <- as.numeric(gsub("[.]*html", "", lastpage)) + 1
  return (lastpage)
}

lastPage <- getLastPage(url)
