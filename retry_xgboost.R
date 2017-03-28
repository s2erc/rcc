# Human retry xgBoost Model
# Utilize Boosted Trees to predict retries

# start writing output to file
sink('xgboost_out.txt', append = TRUE, split = TRUE)
print(Sys.time())

set.seed(1775)
source("DBConnect.R")
require(xgboost)
require(lubridate)
require(Matrix)
require(ggplot2)

# Import Data -------------------------------------------------------------

# import sample from L3 data
l3samp <- dbGetQuery(con, "select ORIG_DT, DISCNCT_DT, SWITCH_ID, ANSWER_DT, CALL_DURATION_VAL,
                     CALL_DIR_DESC, JURS_DESC, ORIG_END_OFFICE_CD, ORIG_LATA_NBR, ORIG_OCN_NBR, ORIG_STATE_CD,
                     POST_DIAL_DELAY_VAL, ROUTE_ATT_NBR, ROUTE_SELECTED, TERM_END_OFFICE_CD, TERM_LATA_NBR, TERM_OCN_NBR,
                     TERM_STATE_CD, RETRY_BIN from l3_sample_01 where guid not like '%G'")
names(l3samp) <- tolower(names(l3samp))

# Data Preparation --------------------------------------------------------

# convert to datetime objects
l3samp$orig_dt <- as_datetime(l3samp$orig_dt)
l3samp$discnct_dt <- as_datetime(l3samp$discnct_dt)
l3samp$answer_dt <- as_datetime(l3samp$answer_dt)

# add features #
# parse date objects
#l3samp$orig_sec <- second(l3samp$orig_dt) # origination seconds
#l3samp$orig_min <- minute(l3samp$orig_dt) # origination minute
l3samp$orig_hr <- hour(l3samp$orig_dt) # origination hour
l3samp$orig_day <- day(l3samp$orig_dt) # origination day (ex. "2009-09-02" -> "2")
l3samp$orig_wday <- wday(l3samp$orig_dt) # origination weekday (Sunday = 1, Monday = 2, ..., Saturday = 7)
l3samp$orig_bus <- ifelse(l3samp$orig_wday %in% c(1,7), 0, 1) # binary indicating business day of origination date (Mon-Fri = 1, Sat-Sun = 0)

#l3samp$discnct_sec <- second(l3samp$discnct_dt) # disconnect seconds
#l3samp$discnct_min <- minute(l3samp$discnct_dt) # disconnect minute
l3samp$discnct_hr <- hour(l3samp$discnct_dt) # disconnect hour
l3samp$discnct_day <- day(l3samp$discnct_dt) # disconnect day (ex. "2009-09-02" -> "2")
l3samp$discnct_wday <- wday(l3samp$discnct_dt) # disconnect weekday (Sunday = 1, Monday = 2, ..., Saturday = 7)
l3samp$discnct_bus <- ifelse(l3samp$discnct_wday %in% c(1,7), 0, 1) # binary indicating business day of disconnect date (Mon-Fri = 1, Sat-Sun = 0)

#l3samp$answer_sec <- second(l3samp$answer_dt) # answer seconds
#l3samp$answer_min <- minute(l3samp$answer_dt) # answer minute
l3samp$answer_hr <- hour(l3samp$answer_dt) # answer hour
l3samp$answer_day <- day(l3samp$answer_dt) # answer day (ex. "2009-09-02" -> "2")
l3samp$answer_wday <- wday(l3samp$answer_dt) # answer weekday (Sunday = 1, Monday = 2, ..., Saturday = 7)
l3samp$answer_bus <- ifelse(l3samp$answer_wday %in% c(1,7), 0, 1) # binary indicating business day of answer date (Mon-Fri = 1, Sat-Sun = 0)

# was call answered?
l3samp$ans_bin <- ifelse(is.na(l3samp$answer_dt), 0, 1)

# remove date objects
l3samp <- subset(l3samp, select = -c(orig_dt, answer_dt, discnct_dt))

# convert to factors
factors <- c('switch_id', 'call_dir_desc', 'jurs_desc', 'orig_end_office_cd',
             'orig_lata_nbr', 'orig_ocn_nbr', 'orig_state_cd', 'route_selected',
             'term_end_office_cd', 'term_lata_nbr', 'term_ocn_nbr', 'term_state_cd',
             'retry_bin')
l3samp[,factors] <- lapply(l3samp[,factors], factor)
rm(factors)


#create training and test datasets
l3_sparse <- sparse.model.matrix(retry_bin~. -1, data = l3samp)
subsamp <- sample(1:nrow(l3samp), 0.6 * nrow(l3samp)) # randomly select 60% of observations from l3samp
train <- l3samp[subsamp,] #training set composed of 60% observations from l3samp
train <- train[which(complete.cases(train)),]
test <- l3samp[-subsamp,] #test set composed of 40% observations from l3samp
test <- test[which(complete.cases(test)),]
rm(subsamp)

# convert train and test to sparse model matrices
xgmat.train <- sparse.model.matrix(retry_bin~. -retry_bin, data = train)
xgmat.test <- sparse.model.matrix(retry_bin~. -retry_bin, data = test)

#create target vector
target_vec <- as.numeric(levels(train$retry_bin))[train$retry_bin]

# Cross-Validation for Parameter tuning -----------------------------------

# xgboost cross-validation for parameter tuning
param <- list(objective = 'binary:logistic',
              eta = 0.1, # learning rate (0,1) [lower eta -> higher nrounds]: Default = 0.3
              max.depth = 6, # max depth of a tree: Default = 6
              eval_metric =  'error', # metric minimize in model training
              gamma = 0, # min loss to make further partition on leaf node [0, inf)
              colsample_bytree = 0.7) # ratio of features used in each tree: Default = 1

nround <- 300 # number of iterations to train

cat('====== Training Parameters ======\n')
print(param)
cat('nrounds =', nround, '\n')
cat('=================================\n')

sink()


xgcv <- xgb.cv(params = param, data = xgmat.train, label = target_vec, nfold = 3,
               nrounds = nround, early_stop_round = 10, prediction = TRUE)

sink('xgboost_out.txt', append = TRUE, split = TRUE)
# save evaluation log for inspection
cv_log <- xgcv$evaluation_log
# print details of iteration with minimum test error
cat('\n',"Best iteration:", "\n")
print(cv_log[which.min(cv_log[[4]]),])
best_iter <- cv_log$iter[which.min(cv_log[[4]])]

sink()

# Plotting evaluation metrics --------------------------------------------------------

# plot AUC for train vs test prediction

#plot(x = cv_log[,1], y = cv_log[,2], type = 'l', col = 'red', xlab = "Iteration", ylab = "AUC", main = "AUROC")
#lines(x = cv_log[,1], y = cv_log[,4], col = 'green')
#legend(x = nrow(cv_log), y = min(cv_log[,2])+ 0.01, xjust = 1, yjust = 0,
#       legend = c("Train AUC", "Test AUC"), lty = c(1,1), lwd = c(2.5, 2.5),
#       col = c("red", "green"))

# plot prediction error for train vs test

errplot <- ggplot(data = cv_log, aes(x = iter)) +
  geom_line(aes(y = train_error_mean, colour = "train_error_mean")) +
  geom_ribbon(aes(ymin = train_error_mean - train_error_std,
                  ymax = train_error_mean + train_error_std), alpha = 0.3) +
  geom_line(aes(y = test_error_mean, colour = "test_error_mean")) +
  geom_ribbon(aes(ymin = test_error_mean - test_error_std,
                  ymax = test_error_mean + test_error_std), alpha = 0.3)
print(errplot)

# Train model on tuned parameters --------------------------------------------------------

xgretry <- xgboost(params = param, data = xgmat.train, nrounds = best_iter, label = target_vec)

test.pred <- predict(xgretry, newdata = xgmat.test)

sink('xgboost_out.txt', append = TRUE, split = TRUE)
# Contingency Table
test_true <- test$retry_bin[which(complete.cases(test))]
# test_guess <- ifelse(test.pred >= 0.4, 1, 0)
test_guess <- round(test.pred)

cat('\n', '====== Contingency Table ======\n')
print(table(test_true, test_guess))
cat('\n', '\n', '\n', '\n')
sink()

# To Do ------------------------------------------------------
# use xgb.DMatrix instead of removing missing data before sparse.model.matrix()
# examine chi square tests for significance of predictors.
# create additional features as neccessary.
# remove insignificant features
