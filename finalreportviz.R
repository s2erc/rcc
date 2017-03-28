library(AnomalyDetection)
source('multiplot.R')
source('xgb.plot.multi.trees.R')
library(lubridate)
require(ggplot2)

# HMR NER proportions -----------------------------------------------------
ner_hmr <- read_csv("~/Desktop/rcc_data/ner_vs_hmr_all_ocn_months.csv")
names(ner_hmr) <- tolower(names(ner_hmr))
names(ner_hmr) <- c('ner', 'hmr')

summary(ner_hmr)

# proportion dependent variable
lm.ner.hmr <- lm(hmr~ner, data = ner_hmr)
summary(lm.ner.hmr)
plot(ner_hmr$ner, ner_hmr$hmr, pch = 20)

#logit transformed dependent variable
ner_hmr$logit.hmr <- log(ner_hmr$hmr/(1-ner_hmr$hmr))
hist(ner_hmr$logit.hmr, breaks= 20)
plot(ner_hmr$ner, ner_hmr$logit.hmr, pch = 20)
log.ner.hmr <- lm(logit.hmr~ner, data = ner_hmr)
summary(log.ner.hmr)
coefficients(log.ner.hmr)[2]

(exp(coefficients(log.ner.hmr)[2]) - 1)
####### 9.8% decrease in hmr for 1% increase in NER

# OCN 0827 HMR ------------------------------------------------------------

ocn_0827_daily_pair_hmr <- read_csv("~/Desktop/rcc_data/ocn_0827_daily_pair_hmr.lst")
str(ocn_0827_daily_pair_hmr)
ocn_0827_daily_pair_hmr <- as.data.frame(ocn_0827_daily_pair_hmr)
names(ocn_0827_daily_pair_hmr) <- tolower(names(ocn_0827_daily_pair_hmr))
ocn_0827_daily_pair_hmr$time <- as_datetime(ocn_0827_daily_pair_hmr$time)
ocn_0827_daily_pair_hmr$nums_w_retry <- as.numeric(ocn_0827_daily_pair_hmr$nums_w_retry)
ocn_0827_daily_pair_hmr$total_nums <- as.numeric(ocn_0827_daily_pair_hmr$total_nums)
ocn_0827_daily_pair_hmr$pct_retry <- as.numeric(ocn_0827_daily_pair_hmr$pct_retry)


attach(ocn_0827_daily_pair_hmr)
names(ocn_0827_daily_pair_hmr)
oct <- ocn_0827_daily_pair_hmr[time< as_date("2016-11-15") & time> as_date("2016-10-01"),]
par(mfrow=c(2,2))
#nums with retries
plot(time, nums_w_retry, type='l')
plot(oct$time, oct$nums_w_retry, type='l')
#pct_retries
plot(time, pct_retry, type='l')
plot(oct$time, oct$pct_retry, type='l')
par(mfrow=c(2,1))
#total nums
plot(time, total_nums, type = 'l')
plot(oct$time, oct$total_nums, type = 'l')
detach(ocn_0827_daily_pair_hmr)

# OCN 0256 ----------------------------------------------------------------

hmr0256 <- read.csv('/Users/Andrew/Desktop/rcc_data/ocn_0256_daily_pair_hmr.lst')
names(hmr0256) <- tolower(names(hmr0256))
ner0256 <- read.csv('/Users/Andrew/Desktop/rcc_data/ocn_0256_daily_ner.csv')
names(ner0256) <- tolower(names(ner0256))
hmr0256$ner <- ner0256$ner
hmr0256$time <- as_datetime(hmr0256$time)

novjan0256 <- hmr0256[hmr0256$time > as_date("2016-11-01") & hmr0256$time < as_date("2017-01-01"),]

colors <- c('pair HMR' = 'black', 'NER (%)' = 'red')
p5 <- ggplot(data = novjan0256, aes(x = time, y = nums_w_retry, colour = 'pair HMR')) +
  geom_line() + geom_line(aes(y = ner*100, colour = 'NER (%)')) + theme_bw() +
  scale_colour_manual(name = 'Metric', values = colors) +
  labs(title = 'Unique number pairs with retries vs. NER (Nov 2016 - Jan 2017)',
       x = '', y = 'Unique number pairs with retries') +
  theme(plot.title = element_text(hjust = 0.5))
p5 # Don't use.

# OCN 0438 ----------------------------------------------------------------

hmr0438 <- read.csv('/Users/Andrew/Desktop/rcc_data/ocn_0438_daily_pair_hmr.csv')
hmr0438without <- read.csv('/Users/Andrew/Desktop/rcc_data/ocn_0438_daily_pair_hmr_robos_out.csv')
names(hmr0438) <- tolower(names(hmr0438))
names(hmr0438without) <- tolower(names(hmr0438without))
hmr0438$hmr.norobo <- hmr0438without$pct_retry
hmr0438$time <- as_datetime(hmr0438$time)
ner0438 <- read.table('/Users/Andrew/Desktop/rcc_data/vz_day_ners/days_0438.lst', header = TRUE)
hmr0438$ner <- ner0438$NER

colors <- c('With Robo' = 'black', 'Robo Removed' = 'red')
p6 <- ggplot(data = hmr0438, aes(x = time, y = pct_retry*100, colour = 'With Robo')) +
  geom_line(linetype = 'dashed') +
  geom_line(aes(y = hmr.norobo*100, colour = 'Robo Removed'), linetype = 'dashed') +
  theme_bw() + scale_colour_manual(name = 'Metric', values = colors) +
  labs(title = 'HMR retry percent with and without robocallers ',
       x = '', y = 'HMR (%)') +
  theme(plot.title = element_text(hjust = 0.5))
p6

# Add NER 
colors <- c('With Robo' = 'black', 'Robo Removed' = 'red', 'NER (%)' = 'royalblue2')
p6 <- ggplot(data = hmr0438, aes(x = time, y = ner*100, colour = 'NER (%)')) +
  geom_line(linetype = 'solid') +
  geom_line(aes(y = pct_retry*100, colour = 'With Robo'), linetype = 'dashed') +
  geom_line(aes(y = hmr.norobo*100, colour = 'Robo Removed'), linetype = 'dashed') +
  theme_bw() + scale_colour_manual(name = 'Metric', values = colors) +
  labs(title = 'HMR retry percent with and without robocallers ',
       x = '', y = 'HMR (%)') +
  theme(plot.title = element_text(hjust = 0.5))
p6

# Anomaly Detection Examples ----------------------------------------------
vzfeb0471ner <- read.csv('/Users/Andrew/Desktop/rcc_data/vz_hour_ners/hour_ners_0471.lst')
names(vzfeb0471ner) <- tolower(names(vzfeb0471ner))
vzfeb0471ner$time_stamp <- as_datetime(vzfeb0471ner$time_stamp)
vzfeb0471ner <- vzfeb0471ner[vzfeb0471ner$time_stamp > as_date("2016-02-15") & vzfeb0471ner$time_stamp < as_date("2016-02-24"),]
vzfeb0471ner$time_stamp <- factor(vzfeb0471ner$time_stamp)
p1 <- AnomalyDetectionTs(vzfeb0471ner[,c(1,4)], max_anoms = 0.1, direction = 'neg',  plot = TRUE)$plot
p1 + labs(title = 'Anomaly Detection Example', y = 'Network Metric') +
  theme(plot.title = element_text(hjust = 0.5, size = 11, family = '', face = 'plain'),
        axis.title.y = element_text(size = 11)) 


# Plot xgBoost Tree -------------------------------------------------------
load("/Users/Andrew/Desktop/treemodel.Rda")
require(xgboost)
j <- multi.trees(feature_names = xgmat.train@Dimnames[[2]], model = xgretry)
print(j)
