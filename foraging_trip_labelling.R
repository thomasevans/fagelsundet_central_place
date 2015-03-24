


# Packages -----
library("RODBC")


# 1. Get data -----
# Connect to DB
# To link to database

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)

# 1. GPS point data (device_info_serial, date_time, ring_number)
# 2. Distance from colony data (device_info_serial, date_time, on_trip)
# 3. Connect above (i.e. extract as single SQL query)
# 4. Make sure in order (device_info_serial, then date_time)

# Get data
gps.points <- sqlQuery(gps.db,
                       query =
                      "SELECT DISTINCT g.device_info_serial, g.date_time, t.trip_1km, c.coast_dist_sign
FROM (fagelsundet_gulls_all_2014_gps_points AS g INNER JOIN fagelsundet_gulls_all_2014_gps_coldist AS t ON (g.date_time = t.date_time) AND (g.device_info_serial = t.device_info_serial)) INNER JOIN fagelsundet_gulls_all_2014_gps_coastdist AS c ON (g.date_time = c.date_time) AND (g.device_info_serial = c.device_info_serial)
ORDER BY g.device_info_serial, g.date_time;"
                       ,as.is = TRUE)


gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))

gps.points$trip_1km <- as.logical(gps.points$trip_1km)

str(gps.points)


# 2. Calculate time interval (for time-weighting) -----
n <- length(gps.points$date_time)
time_interval <- difftime(gps.points$date_time[-1],
                          gps.points$date_time[-n],
                          units = "secs")
# Add value for first point
time_interval <- c(0,time_interval)


# Get area class
cuts <- c(-Inf, -500, 500, Inf)
labs <- c("Land", "Coast", "Marine")
area_class <- labs[findInterval(gps.points$coast_dist_sign, cuts)]
area_class <- as.factor(area_class)

# 3. Go through all points (for loop) -----
# 1. Number points by foraging trip number (NA if not a trip)
# 2. If same device, as on_trip, same as last point etc... (like labelling flights - test last point)

last_on_trip <- FALSE
# Pre-size empty vector to avoid slow opperation of expanding vector
trip_id <- rep(NULL, length(gps.points$device_info_serial))
trip_id[1] <- 0
trip_id_x <- 0
for(i in 2:n){
  a <- FALSE
  if(gps.points$device_info_serial[i] != 
       gps.points$device_info_serial[i-1]){
    a <- TRUE
    time_interval[i] <- 0
  }
  
  if(gps.points$trip_1km[i] == TRUE){
    if((last_on_trip == TRUE) & (a == FALSE)) {trip_id[i] <- trip_id_x
    } else {trip_id[i] <- trip_id_x + 1
          trip_id_x <- trip_id_x + 1
    } 
    last_on_trip <- TRUE
  } else {trip_id[i] <- 0
          last_on_trip <- FALSE}
}



# 4. Replace colony based locations with label 'colony' ----
area_class2 <- area_class
levels(area_class2) <- c(levels(area_class2), "Colony")
area_class2[gps.points$trip_1km == FALSE] <- "Colony"
summary(area_class2)

# 5. Output new table   ------
# 
# 1. device_info_serial
# 2. date_time
# 3. trip_id
# 4. time_interval
# 5. area_class

# range(trip_id)

out.tab <- cbind.data.frame(gps.points$device_info_serial,
                            gps.points$date_time,
                            trip_id,
                            time_interval,
                            area_class2)


names(out.tab) <- c("device_info_serial",
                    "date_time",
                    "trip_id",
                    "time_interval",
                    "area_class")


# Fix date-time
# out.tab$date_time <-  as.POSIXct(strptime(out.tab$date_time,
#                                           format = "%Y-%m-%d %H:%M:%S",
#                                           tz = "UTC"))

str(out.tab)

# 5. Output to DB ------
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, out.tab, tablename = "fagelsundet_gulls_all_2014_gps_trip_id_par2",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date")
)

close(gps.db)
