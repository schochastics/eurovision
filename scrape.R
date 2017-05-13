library(RSelenium)
library(XML)

# helper function ------
getAlt <- function(node, ...){
  if(xmlName(node) == "td" && !is.null(node[["img"]]))
    xmlGetAttr(node[["img"]], "alt")
  else
    xmlValue(node)
}
# (make sure docker is running: https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-docker.html)

# scrape data-----
base.url <- "http://eurovisionworld.com/?eurovision="

remDr <- remoteDriver(port = 4445L)
remDr$open()

for(y in 1958:2016){
  print(y)
  url <- paste0(base.url,y)
  remDr$navigate(url)
  webElem <- remDr$findElement(using = 'xpath', value = '//*[(@id = "voting_grid")]')
  webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]]
  table <- readHTMLTable(webElemtxt,elFun = getAlt)$`NULL`
  table[1,] <- c(NA,NA,NA,table[1,1:(ncol(table)-3)]) %>% unlist
  table <- table[,-c(1,2,4)]
  names(table) <- table[1,]
  names(table)[1] <- "country"
  table <- table[-1,]
  voting.df <- table %>% gather(voter,points,-country) 
  write.table(voting.df,paste0(y,".csv"),row.names = F)
}
remDr$close()