# Annotate GPS positions with distance from nest


# Database function -----

# To link to database
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)




# 1. Get required GPS data from DB (device_info_serial, ring_number, date_time,
# lat, long)
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.device_info_serial, g.ring_number, g.date_time, g.latitude, g.longitude
                       FROM fagelsundet_gulls_all_2014_gps_points AS g
                       ORDER BY g.device_info_serial ASC, g.date_time ASC;"
                       ,as.is = TRUE)

str(gps.points)

# Get ring numbers as factor
gps.points$ring_number <- as.factor(gps.points$ring_number)

# Fix date_time
gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))




# 2. Get DB table for deployment_info (including nest location and island location)
dep.info <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT d.ID, d.ring_number, d.species, d.device_info_serial, d.island_name, d.start_latitude_island, d.start_longitude_island
                       FROM fagelsundet_all_2014_deployment_data AS d
                       ORDER BY d.device_info_serial ASC;"
                       ,as.is = TRUE)
str(dep.info)

# 3. Get distance function (saved script - can copy from lbbg project)




# 4. Calculate distance for each GPS (from nest and island)

# 5. Put in table and output to DB
