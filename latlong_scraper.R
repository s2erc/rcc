# phone number lat/long webscraper

require(rvest)
require(stringr)
require(RCurl)
#require(ggmap)
if(file.exists("l3retry_term_nums.Rda")){
load("l3retry_term_nums.Rda")} else {
  source("DBConnect.R")
  l3retry_term_nums <- dbGetQuery(con, "select * from (select unique term_nbr from l3_cdr_gap where retry_bin = 1)")
}

formatter <- function(x){
  num <- paste(substring(x, c(1, 4, 7), c(3, 6, 10)), collapse = '-')
  return(num)
}

getAddress <- function(x){
  page <- read_html(x)
  page %>%
    html_node("h4:nth-child(3)") %>%
    html_text() -> address
  return(address)
}

l3retry_term_nums$TERM_NBR <- sapply(l3retry_term_nums$TERM_NBR, formatter, simplify = TRUE, USE.NAMES = FALSE)
address <- rep.int(0, nrow(l3retry_term_nums))
lat <- rep.int(0, nrow(l3retry_term_nums))
lon <- rep.int(0, nrow(l3retry_term_nums))
l3retry_locations <- data.frame(l3retry_term_nums, address, lon, lat, stringsAsFactors = FALSE)
rm(l3retry_term_nums)

for (i in 1:length(l3retry_locations)) {
  url <- paste(paste("https://www.phonelookup.com/1/", l3retry_locations$TERM_NBR[i], sep = ''))
  text <- tryCatch(getAddress(url), error = function(cond) TRUE)
  if (isTRUE(text)){
    Sys.sleep(300)
    text <- tryCatch(getAddress(url), error = function(cond) TRUE)
    if (isTRUE(text)){
      l3retry_locations$address[i] <- NA}
  }
  else {l3retry_locations$address[i] <- str_split(text, pattern = "Address: ", simplify = TRUE)[2]}
  Sys.sleep(5)
}

save(l3retry_locations, "l3retry_locations.Rda")
q('no')







require(RCurl)
#require(ggmap)

if(file.exists("l3retry_term_nums.Rda")){
load("l3retry_term_nums.Rda")} else {
source("DBConnect.R")
l3retry_term_nums <- dbGetQuery(con, "select * from (select unique term_nbr from l3_cdr_gap where retry_bin = 1)")
}

formatter <- function(x){
num <- paste(substring(x, c(1, 4, 7), c(3, 6, 10)), collapse = '-')
return(num)
}

getAddress <- function(x){
page <- read_html(x)
page %>%
html_node("h4:nth-child(3)") %>%
html_text() -> address
return(address)
}

l3retry_term_nums$TERM_NBR <- sapply(l3retry_term_nums$TERM_NBR, formatter, simplify = TRUE, USE.NAMES = FALSE)
address <- rep.int(0, nrow(l3retry_term_nums))
lat <- rep.int(0, nrow(l3retry_term_nums))
lon <- rep.int(0, nrow(l3retry_term_nums))
l3retry_locations <- data.frame(l3retry_term_nums, address, lon, lat, stringsAsFactors = FALSE)
rm(l3retry_term_nums)

for (i in 1:nrow(l3retry_locations)) {
url <- paste(paste("https://www.phonelookup.com/1/", l3retry_locations$TERM_NBR[i], sep = ''))
address <- tryCatch(getAddress(url), error = function(cond) "wait")
if(address == "wait"){
  Sys.sleep(300)
  address <- tryCatch(getAddress(url), error = function(cond) "skip")
if(address == 'skip'){
l3retry_locations$address[i] <- NA}
}
l3retry_locations$address[i] <- str_split(address, pattern = "Address: ", simplify = TRUE)[2]
Sys.sleep(5)
}

save(l3retry_locations, "l3retry_locations.Rda")
q('no')

