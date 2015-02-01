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

# 3. Get distance function (saved script - copied from lbbg project)
# source("deg.dist.R")


# 4. Calculate distance for each GPS (from island centre to be
# consistant with Kozue's approach)


# Distance from nest -----

# Distance from nest function
bird.dist <- function(device_id,lat,long, z, ...){
  source("deg.dist.R")
  x <- as.data.frame(z[1])
  lat.1 <- as.numeric(x[x$device_info_serial == device_id, 6])
  long.1 <- as.numeric(x[x$device_info_serial == device_id, 7])
  dist <- deg.dist(long.1,lat.1,
                   long, lat, km = FALSE) 
  return(unlist(dist)[1])
}

# Initialise empty vector for distance data
col.dist <- NULL

# Run for all points
col.dist <- mapply(bird.dist,
                   lat = gps.points$latitude,
                   long = gps.points$longitude,
                   device_id = gps.points$device_info_serial,
                   MoreArgs = (list(z = list(dep.info))))

# hist(col.dist)

# TRUE/ FALSE, whether >1 km from island
dist.fun <- function(x){
  if(is.na(x)) return(NA) else {
    if(x > 1000) return(TRUE)
    else return(FALSE)  
  }
}

on.trip <- sapply(col.dist, dist.fun)

str(on.trip)
summary(on.trip)

# 5. Put in table and output to DB
out.tab <- cbind.data.frame(gps.points$device_info_serial,
                            gps.points$date_time,
                            col.dist,
                            on.trip)


names(out.tab) <- c("device_info_serial",
                   "date_time",
                   "col_dist",
                   "trip_1km")

# str(out.tab)

# Output to Database ------
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, out.tab, tablename = "fagelsundet_gulls_all_2014_gps_coldist",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date")
)

close(gps.db)