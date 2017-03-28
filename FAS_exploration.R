# Explore distributions of alert to answer times
# Develop distribution params for each trunk group
# scale times 0-100 to visualize easily

#options(java.parameters = "-Xmx2g") #increase memory to 2G
library(lubridate)
source("DBConnect.R")

ptm <- proc.time()
samp <- dbGetQuery(con, "select orig_dt, answer_dt, guid, discnct_dt, resp_code from l3_cdr_gap where term_ocn_nbr = '4332' and guid not like '%G'")
names(samp) <- tolower(names(samp))

# coerce date strings to POSIXct format
samp$orig_dt <- as_datetime(samp$orig_dt)
samp$discnct_dt <- as_datetime(samp$discnct_dt)
samp$answer_dt <- as_datetime(samp$answer_dt)

# answer_dt is in different time zone from orig_dt
# we need to calculate this difference in order to
# standardize timezones

# calculate difference between orig_dt and ans_dt
tz_diff <- difftime(samp$orig_dt, samp$answer_dt, unit = "hours")
samp <- data.frame(samp, tz_diff)
samp$tz_diff <- round(samp$tz_diff)
samp$tz_diff <- hours(samp$tz_diff)

# standardize answer_dt to same time zone as orig_dt
std_ans_dt <- samp$answer_dt + samp$tz_diff
samp <- data.frame(samp, std_ans_dt)



# calculate ring_time = std_ans_dt - orig_dt
ring_time <- samp$std_ans_dt - samp$orig_dt
samp <- data.frame(samp, ring_time)

# calls that are not answered do not get billed
# remove un-answered calls
comp_samp <- samp[!is.na(samp$answer_dt),]
comp_samp$ring_time <- as.numeric(comp_samp$ring_time)

# plot the ring times
time_density <- as.data.frame(table(comp_samp$ring_time))
names(time_density) <- c("ring_time", "freq")

# plot(time_density$ring_time, time_density$freq, type = "l")
# abline(v = median(time_density$ring_time), color = 'red')
# 
# ring_time <- c(0,   1,   2,   3,   4,   5 ,  6  , 7 ,  8  , 9 ,  10 , 11  ,12  ,13  ,14 , 15,  16 , 17 ,
#  18 , 19  ,20 , 21  ,22 , 23,  24,  25  ,26 , 27,  28,  29  ,30 , 31,  32,  33,  34,  35 ,
#  36,  37,  38,  39,  40,  41,  42,  43,  44,  45  ,46 , 47,  48,  49,  50,  51,  52,  53,
#  54 , 55,  56,  57  ,58 , 59  ,60 , 61  ,62 , 63  ,64 , 65,  66,  67,  68,  69,  70,  71,
#  72 , 73  ,74 , 75  ,76 , 77  ,78 , 79  ,80 , 81,  82,  84,  85,  87,  88,  89,  90,  91 ,
#  92,  93  ,94 , 95,  96,  97,  99,  100, 102, 103, 108, 109, 112, 120, 121, 148)
# 
# freq <- c(758, 6044, 5090, 2981, 3189, 3843, 3286, 2877, 3008, 3214, 3508, 3812, 3497, 2881, 2798,
# 2622, 2540, 2385, 2220, 1887, 1722, 1826, 2330, 2715, 2676, 2318, 1926, 1399, 1201, 965,
# 606,  598,  431,  281,  245,  283,  281,  235,  169,  117,   97,   73,   61,   42,   36,
# 40,   40,   34,   36,   19,   20,   21,   15,   17,   13,    9,   15,   14,   26,   52,
# 80,   88,   21,   15,   11,    8,   13,    7,    1,    1,    7,    4,    2,    5,    4,
# 2,    1,    4,    1,    3,    1,    2,    2,    1,    2,    1,    2,    6,    4,    2,
# 2,    1,    1,    1,    1,    1,    1,    1,    2,    1,    1,    2,    1,    4,    2,
# 1)
# 
# plot(ring_time, freq, type = "l")
# 
# 
# 
# 
# 
# 
# 
# 
runtime <- proc.time() - ptm


# TO DO LIST --------------------------------------------------------------
# separate data by trunk group to analyze


