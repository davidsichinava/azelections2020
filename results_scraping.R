# Load necessary variables

library(readr)
library(rvest)
library(stringr)
library(stringi)
library(httr)
library(jsonlite)
library(tidyr)

setwd("D:\\Dropbox\\My projects\\Elections\\Azerbaijan")

az_vote <- data.frame()

for (i in 1:125) { ### There are 125 electoral districts
  tryCatch({

scrapurl <- paste0("https://www.infocenter.gov.az/archive/millimeclis2020.aspx?i=2&dsk=", i)
  
toscrap <- GET(iconv(scrapurl, to="UTF-8"), user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"), encoding="utf8" ) %>%
  read_html()

scrap <- toscrap%>%
  html_table()

scrap <- scrap[[1]] %>%
  gather(candidate, votes, -c(1:18))

az_vote <- rbind(az_vote, scrap)
  }, error=function(e){cat("ERROR in district No: ", i, "; ", conditionMessage(e), "\n")})
}

write_csv(az_vote,'az_vote.csv')


### get coordinates

az_vote_addr <- data.frame()

for(i in 1:125){
scrape_url <- paste0("http://gomap.az/info/infocenter/poi/", i, ".js")
a <- fromJSON(scrape_url, flatten = T)
b <- as.data.frame(a[[7]])
b$district<-unlist(a[1])
b$district_x<-unlist(a[2])
b$district_y<-unlist(a[3])
b$district_name<-unlist(a[5])
b$district_address<-unlist(a[6])
az_vote_addr <- as.data.frame(rbind(az_vote_addr, b))
}

write_csv(az_vote_addr, "az_vote_addr.csv")
