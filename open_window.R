source("db_connect.R")
query_p1 = "select * from lec_cdr_gap where term_ocn = '"
query_p2 = "' and extract(day from call_orig_tm) = "
ocn <- readline("Which OCN are you querying? ")
day <- readline("Which Day are you querying? ")
df <- dbGetQuery(con, paste0(query_p1, ocn, query_p2, day))