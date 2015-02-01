


# 1. Load required data ---------

# Load in GPS point data (lat, long, device_info_serial, date_time)

# Connect to DB
# To link to database
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)

# Get data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude
                       FROM fagelsundet_gulls_all_2014_gps_points AS g
                       ORDER BY g.device_info_serial ASC, g.date_time ASC;"
                       ,as.is = TRUE)


# Load coastline data (previously prepared as R binnary files)



# 2. Distance to coast ----


# 3. On land/ sea ----


# 4. Add sign to distance to coast
# If on land negative, at sea positive


# 5. Assemble data and output to new DB table
