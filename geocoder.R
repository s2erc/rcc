require(jsonlite)
require(stringr)
require(plyr)
require(dplyr)
load("l3retry_locations.Rda")


uniques <- subset(l3retry_locations, !duplicated(l3retry_locations$term_nbr), select = c(term_nbr, address)) #subset full list to get only unique term_nbr's
invalid.address <- grep('^[0-9]*$', uniques$address) # search for addresses that only contain digits
uniques <- uniques[-invalid.address,] # remove invalid addresses
valid.number <- grep('^[0-9]{3}-[0-9]{3}-[0-9]{4}$', uniques$term_nbr) # serach for numbers that are complet
e
uniques <- uniques[valid.number,] # include only complete numbers
uniques <- uniques[complete.cases(uniques),] # remove incomplete observations
uniques <- data.frame(uniques, street = NA, city = NA, state = NA, postalCode = NA) # add fields in order to parse address


for (i in 1:nrow(uniques)){
  splits <- str_locate_all(uniques$address[i], ',')[[1]][,1] # locate commas in address
  if (length(splits) == 0){ # State only
    uniques[i, 5] <- uniques$address[i]
  }
  else if (length(splits) == 1){ # City and State only
    uniques[i, 4:5] <- substring(uniques$address[i], c(1, splits + 2), c(splits - 1, splits + 4))
  }
  else if (length(splits) == 2){ # full address
    uniques[i, 3:6] <- substring(uniques$address[i], c(1, splits[1] + 2, splits[2] + 2, splits[2] + 5), # I know this is ugly, but regex doesn't work well for addresses
                                 c(splits[1] -1, splits[2] - 1, splits[2] + 3, splits[2] + 9))
  }
}

uniques <- uniques[-which(is.na(uniques$state)),] # clean up incomplete observations induced by string split
ting

uniques <- uniques[,-2] # remove 'address' field for preparation

r <- rep(1:ceiling(nrow(uniques)/100), each = 100)[1:nrow(uniques)]
chunks <- split(uniques, r)

KEY <- "3uPiZRVuzgxcevYOViosrAqCScRwC1tP"
apiurl <- paste("http://open.mapquestapi.com/geocoding/v1/batch?key=", KEY, sep = '')
opts <- data.frame('maxResults' = -1, 'thumbMaps' = FALSE, 'ignoreLatLngInput' = FALSE)

for (i in 1:length(chunks)){
  JSON <- toJSON(list(locations = chunks[[i]][,-1], options = unbox(opts)))
  req <- RETRY('POST', url = apiurl, body = JSON, content_type_json(), verbose(), timeout(60))
  resp <- content(req)
  results <- lapply(resp$results, function(x){ unlist(x)})
  results <- rbind.fill(lapply(results, function(x) do.call('data.frame', as.list(x))))
  chunks[[i]] <- data.frame(chunks[[i]],
                            lat = results$locations.latLng.lat[1:nrow(chunks[[i]])],
                            lng = results$locations.latLng.lng[1:nrow(chunks[[i]])])
  Sys.sleep(10)
}

l3retry_latlng <- rbind.fill(chunks)
save(l3retry_latlng, file = 'l3retry_latlng.Rda')

l3retry_locations <- l3retry_locations[,-c(5,6)]
l3retry_locations <- left_join(l3retry_locations, l3retry_latlng, by = 'term_nbr')
save(l3retry_locations, file = 'l3retry_locations.Rda')
