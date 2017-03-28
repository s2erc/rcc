if (!require(AnomalyDetection)){
  install.packages('AnomalyDetection')
  library(AnomalyDetection)
}

# Author: Andrew Stewart
# Organization: Georgetown S2ERC
# Date Created: 10/8/2016
# Date Modified: 10/25/2016

find.anoms <- function(data, name, direc){
  lastcol <- length(data)
  anoms <- AnomalyDetectionTs(data[,c(1,lastcol)], max_anoms=0.02, direction = direc, plot=FALSE)
  
  #anoms <- AnomalyDetectionVec(data$NER, max_anoms = 0.02, direction = 'both', alpha = 0.05, plot = TRUE, period = 5)
  if (length(anoms$anoms)!= 0){
    obs <- list("OCN" = rep.int(name, nrow(anoms$anoms)), "date_time" = anoms$anoms[,1], "anoms" = anoms$anoms[,2])
    return(obs)
  }
}

output.anoms <- function(direction = 'both'){
  direction <- readline("Which direction do you want to detect anomalies in? Your choices are 'pos', 'neg', or 'both'. ")
  
  if (!direction %in% c("pos", "neg", "both")) {
    stop("direction options are: 'pos' | 'neg' | 'both'.")
  }
  
  files <- dir()
  all.anoms <- data.frame('OCN' = NULL, 'date_time' = NULL, 'anoms' = NULL)
  
  for (i in 1:length(files)) { 
    assign(files[i], read.csv(files[i], sep = ",", header = TRUE, row.names = NULL))
    name <- paste(files[i], sep = '')
    all.anoms <- rbind(all.anoms, as.data.frame(find.anoms(get(name), name, direction)))
  }
  
  output <- readline("Name the output file (extension will be added): ")
  write.csv(all.anoms, file = paste(output, '.csv', sep = ''))
}

output.anoms(direction)