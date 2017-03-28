# This script is an exploration of the Level3 CDR Data
# The purpose of this script is to be a reference for exepensive/time-consuming results and queries

# count of unique observations (i.e. duplicates removed)
# result = 168,994,250
dbGetQuery(con, "select count(*) from l3_cdr_gap where guid not like '%G'")
dbGetQuery(con, "select count(*) from lec_cdr_gap")

# count of unique switch IDs
# result = 448
dbGetQuery(con, "select count(*) from (select unique switch_id from l3_cdr_gap)")

# count of unique terminating OCN numbers
# result = 1282
dbGetQuery(con, "select count(*) from (select unique term_ocn_nbr from l3_cdr_gap)")

# count of unique terminating end offices
# result = 8561
dbGetQuery(con, "select count(*) from (select unique term_end_office_cd from l3_cdr_gap)")


# Munging -----------------------------------------------------------------
options(java.parameters = "-Xmx2g") # defualt memory limit is too low. increase to 2G
library(lubridate)
source("DBConnect.R")

raw <- dbGetQuery(con, "select * from level_3_sample")
names(raw) <- tolower(names(raw))
keep <- c(
'orig_dt', 'discnct_dt', 'switch_id', 'answer_dt', 'orig_nbr', 'term_nbr', 'call_duration_val', 'call_dir_desc',
'jurs_desc', 'orig_end_office_cd', 'orig_lata_nbr', 'orig_ocn_nbr', 'orig_state_cd', 'post_dial_delay_val',
'route_att_nbr', 'route_selected', 'term_end_office_cd', 'term_lata_nbr', 'term_ocn_nbr', 'term_state_cd', 
'product_cat')

l3 <- raw[,keep]

# coerce date strings to POSIXct format
l3$orig_dt <- as_datetime(l3$orig_dt)
l3$discnct_dt <- as_datetime(l3$discnct_dt)
l3$answer_dt <- as_datetime(l3$answer_dt)

# coerce some columns to factors
l3$switch_id <- factor(l3$switch_id)
l3$call_dir_desc <- factor(l3$call_dir_desc)
l3$orig_end_office_cd <- factor(l3$orig_end_office_cd)
l3$orig_lata_nbr <- factor(l3$orig_lata_nbr)
l3$orig_ocn_nbr <- factor(l3$orig_ocn_nbr)
l3$orig_state_cd <- factor(l3$orig_state_cd)
l3$route_selected <- factor(l3$route_selected)
l3$term_end_office_cd <- factor(l3$term_end_office_cd)
l3$term_lata_nbr <- factor(l3$term_lata_nbr)
l3$term_ocn_nbr <- factor(l3$term_ocn_nbr)
l3$term_state_cd <- factor(l3$term_state_cd)
l3$product_cat <- factor(l3$product_cat)


# Correlation Analysis ----------------------------------------------------
attach(l3)

chisq.test(post_dial_delay_val, route_selected)
# Result
# Pearson's Chi-squared test

# data:  post_dial_delay_val and route_selected
# X-squared = 3017000, df = 4647600, p-value = 1

chisq.test(post_dial_delay_val, route_att_nbr)
# Result
# Pearson's Chi-squared test

# data:  post_dial_delay_val and route_att_nbr
# X-squared = 40108, df = 16824, p-value < 2.2e-16

chisq.test(post_dial_delay_val, term_end_office_cd)
# Pearson's Chi-squared test

# data:  post_dial_delay_val and term_end_office_cd
# X-squared = 23737000, df = 15699000, p-value < 2.2e-16

