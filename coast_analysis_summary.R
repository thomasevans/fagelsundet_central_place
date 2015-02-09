

# Packages -----
library("RODBC")



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




# 2.