# plotting L3 retries
require(lubridate)
require(ggmap)
register_google(key = "AIzaSyC3M-pms5_z0Pi1LfoThpF7QGsvfVkGZ3w")


# Data Prep ---------------------------------------------------------------
# additional feature creation

l3retry_locations$orig_dt <- as_datetime(l3retry_locations$orig_dt)
l3retry_locations$orig_hr <- hour(l3retry_locations$orig_dt) # origination hour
l3retry_locations$orig_day <- day(l3retry_locations$orig_dt) # origination day (ex. "2009-09-02" -> "2")
l3retry_locations$orig_wday <- wday(l3retry_locations$orig_dt) # origination weekday (Sunday = 1, Monday = 2, ..., Saturday = 7)
l3retry_locations$orig_bus <- ifelse(l3retry_locations$orig_wday %in% c(1,7), 0, 1) # binary indicating business day of origination date (Mon-Fri = 1, Sat-Sun = 0)

l3retry_locations$answer_dt <- as_datetime(l3retry_locations$answer_dt)
l3retry_locations$answer_hr <- hour(l3retry_locations$answer_dt) # answer hour
l3retry_locations$answer_day <- day(l3retry_locations$answer_dt) # answer day (ex. "2009-09-02" -> "2")
l3retry_locations$answer_wday <- wday(l3retry_locations$answer_dt) # answer weekday (Sunday = 1, Monday = 2, ..., Saturday = 7)
l3retry_locations$answer_bus <- ifelse(l3retry_locations$answer_wday %in% c(1,7), 0, 1) # binary indicating business day of answer date (Mon-Fri = 1, Sat-Sun = 0)


