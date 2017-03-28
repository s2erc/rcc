# Script to explore most important attributes in the data
# Feature selection using the Boruta package


# Run in background -------------------------------------------------------
# nohup Rscript --save humanretry_featureselection.R > nohup.out 2> nohup.out < /dev/null &





options(java.parameters = "-Xmx2g") #increase memory to 2G

ptm <- proc.time()
source("DBConnect.R")
l3samp <- dbGetQuery(con, "select ORIG_DT, DISCNCT_DT, SWITCH_ID, ANSWER_DT, CALL_DURATION_VAL, CALL_DIR_DESC, JURS_DESC, ORIG_END_OFFICE_CD, ORIG_LATA_NBR, ORIG_OCN_NBR, ORIG_STATE_CD, POST_DIAL_DELAY_VAL, ROUTE_ATT_NBR, ROUTE_SELECTED, TERM_END_OFFICE_CD, TERM_LATA_NBR, TERM_OCN_NBR, TERM_STATE_CD, PRODUCT_CAT, RETRY_BIN from l3_sample_01")

library(Boruta)
set.seed(1775)

names(l3samp) <- tolower(names(l3samp))
l3samp$retry_bin <- factor(l3samp$retry_bin) #convert binary to factor

keep <- c('orig_dt', 'discnct_dt', 'switch_id', 'answer_dt', 'call_duration_val', 'call_dir_desc', 'jurs_desc', 'orig_end_office_cd', 'orig_lata_nbr', 'orig_ocn_nbr', 'orig_state_cd', 'post_dial_delay_val', 'route_att_nbr', 'route_selected', 'term_end_office_cd', 'term_lata_nbr', 'term_ocn_nbr', 'term_state_cd', 'product_cat', 'retry_bin') # select subset which are likely to be most predictive and have least number of missing values

l3samp <- l3samp[,keep] #subset Level3 sample data
l3samp$answer_dt[is.na(l3samp$answer_dt)] <- 999 #replace NAs in answer_dt with 999

l3samp <- l3samp[complete.cases(l3samp),] #subset rows with complete cases


l3boruta <- Boruta(retry_bin~., data = l3samp, doTrace = 1) # Boruta run

# clean up workspace
rm(keep)
runtime <-  proc.time() - ptm

