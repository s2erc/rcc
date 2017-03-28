library(ggplot2)
library(AnomalyDetection)
trace(AnomalyDetectionTs, edit = TRUE)
# Human Retries
#vz
x <- read.csv("~/Desktop/rcc_data/human_retries_v1/human_retries_0084.lst")
AnomalyDetectionTs(x,max_anoms = 0.1,direction = 'pos',  plot = TRUE)
#l3
y <- read.csv("~/Desktop/rcc_data/l3_human_retries_v1/l3_human_retries_0084.lst")
AnomalyDetectionTs(y,max_anoms = 0.1,direction = 'pos',  plot = TRUE)
untrace(AnomalyDetectionTs)


# Time-Series Decomposition Example ---------------------------------------------------

x <- read.csv("~/Desktop/rcc_data/vz_hour_ners/hour_ners_0084.lst")
x <- x[,c(1,4)]
myts <- ts(data = x[,2], , start = 1, end = 28, frequency = 24)
plot(stl(myts, s.window = 'periodic'))




# Response Code Distribution ----------------------------------------------
vz_code_distro_w_totals <- read.csv("~/Desktop/rcc_data/vz_code_distro_w_totals.txt")
names(vz_code_distro_w_totals) <- tolower(names(vz_code_distro_w_totals))
vz_code_distro_w_totals$response_code <- factor(vz_code_distro_w_totals$response_code)
codes <- levels(vz_code_distro_w_totals$response_code)

# histogram of each response code across OCNs
for (code in codes) {
code_dist <- subset(vz_code_distro_w_totals, subset = response_code == code)
code_hist <- ggplot(code_dist, aes(x = pct)) + 
  geom_histogram(aes(y= ..density..)) + coord_cartesian(xlim = c(0,0.1))
print(code_hist)
invisible(readline(prompt="Press [enter] to continue"))
}

# QQ-plots of select codes in VZ data
for (code in c(1,3,16,17,31,34)) {
  code_dist <- subset(vz_code_distro_w_totals, subset = response_code == code)
  qqplot <- ggplot(code_dist, aes(sample = log10(pct))) + stat_qq()
  print(qqplot)
  invisible(readline(prompt="Press [enter] to continue"))
}


l3_code_distro_w_totals <- read.csv("~/Desktop/rcc_data/l3_code_distro_w_totals.txt")
names(l3_code_distro_w_totals) <- tolower(names(vz_code_distro_w_totals))
l3_code_distro_w_totals$response_code <- factor(l3_code_distro_w_totals$response_code)
codes <- levels(l3_code_distro_w_totals$response_code)

for (code in codes) {
  code_dist <- subset(l3_code_distro_w_totals, subset = response_code == code)
  code_hist <- ggplot(code_dist, aes(x = pct)) + 
    geom_histogram(aes(y= ..density..))
  print(code_hist)
  invisible(readline(prompt="Press [enter] to continue"))
}





#vz
vz_code_distro_w_totals <- read.csv("~/Desktop/rcc_data/vz_code_distro_w_totals.txt")
names(vz_code_distro_w_totals) <- tolower(names(vz_code_distro_w_totals))
vz_dist_0575<- subset(vz_code_distro_w_totals, subset = term_ocn == "0575")
vz_dist_2147<- subset(vz_code_distro_w_totals, subset = term_ocn == "2147")

#l3
l3_code_distro_w_totals <- read.csv("~/Desktop/rcc_data/l3_code_distro_w_totals.txt")
names(l3_code_distro_w_totals) <- names(vz_code_distro_w_totals)
l3_dist_0575 <- subset(l3_code_distro_w_totals, subset = term_ocn == "0575")
l3_dist_2147 <- subset(l3_code_distro_w_totals, subset = term_ocn == "2147")

resp_code <- c(16, 3, 17, 1, 34, 31)

vz_dist_0575 <- subset(vz_dist_0575, subset = response_code %in% resp_code)
l3_dist_0575 <- subset(l3_dist_0575, subset = response_code %in% resp_code)

vz_dist_0575 <- cbind(vz_dist_0575, carrier = rep.int("Carrier '1'", 6))
l3_dist_0575 <- cbind(l3_dist_0575, carrier = rep.int("Carrier '2'", 6))

both_dist_0575 <- rbind(vz_dist_0575, l3_dist_0575)
both_dist_0575$response_code <- factor(both_dist_0575$response_code)

plot_0575 <- ggplot(both_dist_0575, aes(response_code, pct, fill = carrier)) + 
  geom_bar(stat = "identity", position = 'dodge') + coord_cartesian(ylim = c(0, 0.1)) +
  geom_text(stat='identity', position = position_dodge(width =.99), aes(label= round(pct,3)),vjust=-0.2)
plot_0575


#difference in proportions tests
both_dist_0575 <- both_dist_0575[order(both_dist_0575$response_code),]



for (code in c(1,3,16,17,31,34)){
  sub <- subset(both_dist_0575, subset = response_code == code)
  sub$pct <- log(sub$pct)
  prop.test()
}