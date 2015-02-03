


# Packages -----
# spatial data handling
# library("sp")
# library("rgdal")
# # library("raster")
# # library("gdistance")
# library("rgeos")
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
                      "SELECT DISTINCT g.device_info_serial, g.date_time, t.trip_1km
                       FROM fagelsundet_gulls_all_2014_gps_points AS g, fagelsundet_gulls_all_2014_gps_coldist AS t
                       WHERE g.device_info_serial = t.device_info_serial
                       AND g.date_time = t.date_time
                       ORDER BY g.device_info_serial ASC, g.date_time ASC;"
                       ,as.is = TRUE)




# 2. Go through all points (for loop) -----
# 1. Number points by foraging trip number (NA if not a trip)
# 2. If same device, as on_trip, same as last point etc... (like labelling flights - test last point)

# If trip, was last point trip? If yes and device_id the same, get same ID, otherwise get ID+1


# 3. Output new table   ------
# 
# 1. device_info_serial
# 2. date_time
# 3. trip_id

