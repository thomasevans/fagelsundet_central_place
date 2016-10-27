# Not used!!!!!!!!!!!!!!!!!!
# Use 'foraging_trip_labelling' instead !!!!




# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!












# Script to segment GPS data into foraging trips, and label GPS locations by trip

# Load in GPS data ----
# Device info serial, date_time, coldist
library("RODBC")

# Load in GPS point data (lat, long, device_info_serial, date_time)

# Connect to DB
# To link to database

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Get data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT fagelsundet_gulls_all_2014_gps_coldist.device_info_serial, fagelsundet_gulls_all_2014_gps_coldist.date_time, fagelsundet_gulls_all_2014_gps_coldist.col_dist, fagelsundet_gulls_all_2014_gps_coldist.trip_1km
FROM fagelsundet_gulls_all_2014_gps_coldist
                       ORDER BY fagelsundet_gulls_all_2014_gps_coldist.device_info_serial, fagelsundet_gulls_all_2014_gps_coldist.date_time;
                       "
                       ,as.is = TRUE)



# Label trips -------

# What threshold to use??
hist(gps.points$col_dist, breaks = 10000, xlim = c(0,5000))
# 500 m looks like it should be generally ok

# Label if on trip
# Code to recognise trips, and adds this column, labelling
# with 0 if 'at the nest' and 1 if over 500 m from nest location
gps.points$trip <- ifelse(gps.points$col_dist < 500, FALSE, TRUE)

summary(gps.points$trip)

# Label points with respect to whether at the beggining or end of trip

# We want to label the positions for each trip with a unique trip id
# first we make some vectors of next, previous point etc, to find start
# and end points of trips
trip1 <- gps.points$trip +1

#make vector of next point value
trip2 <- (2* c(gps.points$trip[2:length(gps.points$trip)],0))+1

#make vector of prev point value
trip3 <- (3* c(0,gps.points$trip[1:(length(gps.points$trip)-1)]))+1


#label by type of point: 0 - trip, 1 - start, 2 - end, 3 - nest
gps.points$loc_type <- trip1*trip2*trip3   #product of above three vectors
loc_calc     <- gps.points$loc_type        #keep a copy of above calculation

summary(as.factor(loc_calc))

gps.points$loc_calc_temp <- loc_calc

#Reduce to the four possibilties
gps.points$loc_type[(gps.points$loc_type == 1)  ]  <- 0
gps.points$loc_type[gps.points$loc_type == 3 | (gps.points$loc_type == 12)] <- 1
gps.points$loc_type[(gps.points$loc_type == 24) | (gps.points$loc_type == 6) 
             | (gps.points$loc_type == 8) | (gps.points$loc_type == 2)]<- 3
gps.points$loc_type[gps.points$loc_type == 4] <- 2

summary(as.factor(gps.points$loc_type))


# Label individual trips ----
# make column for trip id, start with value 0, which will be null value
# - i.e. not a trip (points at the nest)
gps.points$trip_id <- NA
x <- 0 #initiate trip count thing

# Only do trip points (not colony location)
f <- gps.points$loc_type != 0  & !is.na(gps.points$loc_type)
# summary(f)

# label all point by trip_id
# Slow!! ***
for(i in 1:length(gps.points$loc_type[f])){
  # If a new trip, increment by 1
  if(gps.points$loc_type[f][i] == 1) x <- x+1 
  gps.points$trip_id[f][i] <- x 
  
}



# Output to DB ------
export.table <- gps.points[,c(1,2)]

# Connect to DB (if not already)
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')
str(export.table)

# Fix date-time
export.table$date_time <-  as.POSIXct(strptime(export.table$date_time,
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = "UTC"))

#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, export.table, tablename = "fagelsundet_gulls_all_2014_gps_trip_label",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date")
)
# ?sqlSave

close(gps.db)


