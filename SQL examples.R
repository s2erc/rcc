# get count of unique switch_id numbers from L3 data
# this is an example of a subquery
dbGetQuery(con, "select count(*) from (select unique switch_id from lec_cdr_gap)")

