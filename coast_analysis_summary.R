

# Packages -----
library("RODBC")
library("reshape2")


# 1. Get data from DB -----
# device_info_serial, date_time
# ring_number, species (join to fagelsundet_gulls_2014_deployment_data)
# coast_dist_sign, on_land (fagelsundet_gulls_all_2014_gps_coastdist)
# trip_id, time_interval, area_class (fagelsundet_gulls_all_2014_gps_trip_id_par)

# Connect to DB
# To link to database

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)




# Get data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial, fagelsundet_gulls_all_2014_gps_coastdist.date_time, fagelsundet_all_2014_deployment_data.ring_number, fagelsundet_all_2014_deployment_data.species, fagelsundet_gulls_all_2014_gps_coastdist.coast_dist_sign, fagelsundet_gulls_all_2014_gps_coastdist.on_land, fagelsundet_gulls_all_2014_gps_trip_id_par.trip_id, fagelsundet_gulls_all_2014_gps_trip_id_par.time_interval, fagelsundet_gulls_all_2014_gps_trip_id_par.area_class
FROM (fagelsundet_all_2014_deployment_data INNER JOIN fagelsundet_gulls_all_2014_gps_coastdist ON fagelsundet_all_2014_deployment_data.device_info_serial = fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial) INNER JOIN fagelsundet_gulls_all_2014_gps_trip_id_par ON (fagelsundet_gulls_all_2014_gps_coastdist.date_time = fagelsundet_gulls_all_2014_gps_trip_id_par.date_time) AND (fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial = fagelsundet_gulls_all_2014_gps_trip_id_par.device_info_serial);"
                       , as.is = TRUE)

# Re-cast some of the variables

gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))

gps.points$on_land <- as.logical(gps.points$on_land)

gps.points$ring_number <- as.factor(gps.points$ring_number)

gps.points$species <- as.factor(gps.points$species)

gps.points$area_class <- as.factor(gps.points$area_class)

# 2. filter out colony points + outliers ------
levels(gps.points$area_class)


date.s <-  as.POSIXct(strptime("2014-05-15 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC"))
date.e <-  as.POSIXct(strptime("2014-08-01 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC"))

# Include certain date period only, and exclude points where
# time interval is greater than 20 minutes
f <- ((gps.points$area_class != "Colony") &
      (gps.points$date_time > date.s) &
      (gps.points$date_time < date.e) &
      (gps.points$time_interval < 1200))
# 
# sort(gps.points$time_interval, decreasing = TRUE)[1:10]
# summary(gps.points$time_interval > 1200)
summary(f)
# ?sort

points.trips <- gps.points[f,]


# 3. Summarise by species (time_interval ~ species + area_class) ----


agg.sp.class <- aggregate(time_interval ~ species + area_class,
                          data = points.trips,
                          FUN = sum, na.rm = TRUE)


agg.sp <- aggregate(time_interval ~ species,
                          data = points.trips,
                          FUN = sum, na.rm = TRUE)

str(agg.sp.class)

agg.sp.class$perc <- 100*agg.sp.class$time_interval/agg.sp$time_interval

agg.sp.class


# 3. Summarise by ring_number (time_interval ~ ring_number + area_class) ----

agg.bird.class <- aggregate(time_interval ~ ring_number + area_class,
                          data = points.trips,
                          FUN = sum, na.rm = TRUE)


agg.bird <- aggregate(time_interval ~ ring_number,
                    data = points.trips,
                    FUN = sum, na.rm = TRUE)

str(agg.bird.class)

agg.bird.class$perc <- 100*agg.bird.class$time_interval/agg.bird$time_interval

agg.bird.class



