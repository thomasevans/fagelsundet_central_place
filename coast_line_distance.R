


# 1. Load required data and packages ---------

# packages to install if running on Amazon EC2 thing
install.packages(c("sp","rgdal","rgeos"))


# spatial data handling
library("sp")
library("rgdal")
# library("raster")
# library("gdistance")
library("rgeos")
library("RODBC")

# Load in GPS point data (lat, long, device_info_serial, date_time)

# Connect to DB
# To link to database

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


# Drop NA rows
drop <- is.na(gps.points$longitude)

gps.points <- gps.points[!drop,]

# To test, take first 100 values
# gps.points <- gps.points[1:100,]



# Load coastline data (previously prepared as R binnary files)
# Load coast-line data
load("openstreetmap_coast_polyline.RData")

# Re-project coast-line
# Transform points and coastline to Swedish map projection ()
openstreetmap_polyline_SWEREF_99 <- spTransform(openstreetmap_coast_polyline, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))



# Load coast-line data
load("openstreetmap_coast_polygon.RData")

# Re-project coast-line polygon
# Transform points and coastline to Swedish map projection ()
openstreetmap_polygon_SWEREF_99 <- spTransform(openstreetmap_coast_polygon, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))





# 2. Distance to coast ----

# Make points object
xy.loc <- cbind(gps.points$longitude,
                gps.points$latitude)



# Make spatial points object from xy locations
xy.sp.points <- SpatialPoints(xy.loc,
                              proj4string = CRS(
                                "+proj=longlat +datum=WGS84"))

# Re-project point data
xy.sp.points_SWEREF_99 <- spTransform(xy.sp.points, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))



# Calculate distances
n         <- length(xy.loc[,1])




# break-point -------
# save workspace - export to Amazon, and run slow calculations
# on their server.
save.image(file = "coast_line_distance_export.RData")

# Resume from here ------
load("coast_line_distance_export.RData")




coast.dist <- NA
# Eventuall ran the following on Amazon EC2 (cloud)
# instance of RStudio, which completed in ca. 30 hours
# on m3.large instance
system.time(
  for(i in 1:n){
    # Distance calculated in metres
    coast.dist[i] <- gDistance(xy.sp.points_SWEREF_99[i,],
                        spgeom2 = openstreetmap_polyline_SWEREF_99,
                        byid = FALSE, hausdorff = FALSE,
                        densifyFrac = NULL)
  }
)



# 3. On land/ sea ----
# Calculate if in polygon
# Useing package 'sp'
points.on.land <- over(xy.sp.points_SWEREF_99,
                       openstreetmap_polygon_SWEREF_99,
                       returnList = FALSE, fn = NULL)


# Label 'land' or 'sea' depending on above output
on_land <- !is.na(points.on.land)
summary(on_land)


# 4. Add sign to distance to coast
# If on land negative, at sea positive
sign.dist <- (1 - 2*on_land)*coast.dist


# 5. Assemble data and output to new DB table

out.tab <- cbind.data.frame(gps.points$device_info_serial,
                            gps.points$date_time,
                            coast.dist,
                            on_land,
                            sign.dist)


names(out.tab) <- c("device_info_serial",
                    "date_time",
                    "coast_dist",
                    "on_land",
                    "coast_dist_sign")


save(out.tab,
     file = "out.tab.RData")


# Resume (if ran above in cloud) ----

load("out.tab.RData")

str(out.tab)
# hist(out.tab$coast_dist_sign, xlim = c(-5000,5000),
#      breaks = 2000)


# Fix date-time
out.tab$date_time <-  as.POSIXct(strptime(out.tab$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))


# 4. Output to DB ------
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, out.tab, tablename = "fagelsundet_gulls_all_2014_gps_coastdist",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date")
)

close(gps.db)



