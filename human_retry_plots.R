hist <- ggplot(data = newall, aes(newall[1,])) + geom_histogram()
hist
hist + coord_cartesian(xlim = c(0,15))

setwd("~/Desktop/rcc_data/l3_human_retries_v1")
files <- dir()

all <- data.frame(TIME = seq(
  from= ISOdate(2016,02,01,00,00,00),
  length.out = 696,
  by="hour"
), RETRIES = rep.int(0, times = 696)
)

for (i in 1:length(files)) { 
  retries <- read.csv(files[i], sep = ",", header = TRUE, row.names = NULL)
  print(files[i])
  retries$TIME <- as.POSIXct(retries$TIME)
  for (j in 1:nrow(retries)) {
    all$RETRIES[which(all$TIME==retries$TIME[j])] <- all$RETRIES[which(all$TIME==retries$TIME[j])] + retries$RETRIES[j]
  }
  rm(retries)
}


newall <- data.frame(TIME = seq(
  from= ISOdate(2016,02,01,00,00,00),
  length.out = 696,
  by="hour"
))

newall$TIME <- as.factor(newall$TIME)

for (i in 1:length(files)) { 
  retries <- read.csv(files[i], sep = ",", header = TRUE, row.names = NULL)
  print(files[i])
  retries$TIME <- as.factor(retries$TIME)
  names(retries) <- c("TIME", paste("RETRIES", i, sep = ''))
  newall <- left_join(newall, retries, by = "TIME")
  rm(retries)
}

hist(as.numeric(newall[3,]))
