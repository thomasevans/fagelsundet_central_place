# Use of inland water (lakes, rivers, streams)

# Packages -----
library("RODBC")
library("dplyr")
# library("ggplot2")
# library(scales)
# library(cowplot)
library("maptools")

# Working with spatial data
library(rgeos)
library(rgdal)
library(sp)


# 1. Load in relevant data -----

# Connect to DB
# To link to database

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Get data
gps.points <- sqlQuery(gps.db,
                       query =
                         "SELECT fagelsundet_gulls_all_2014_gps_points.device_info_serial, fagelsundet_gulls_all_2014_gps_points.date_time, fagelsundet_gulls_all_2014_gps_points.latitude, fagelsundet_gulls_all_2014_gps_points.longitude, fagelsundet_gulls_all_2014_gps_points.speed_3d, fagelsundet_gulls_all_2014_gps_points.speed, fagelsundet_gulls_all_2014_gps_trip_id_par.trip_id, fagelsundet_gulls_all_2014_gps_trip_id_par.time_interval, fagelsundet_gulls_all_2014_gps_trip_id_par.area_class, fagelsundet_gulls_all_2014_gps_coastdist.coast_dist, fagelsundet_gulls_all_2014_gps_coastdist.on_land, fagelsundet_gulls_all_2014_gps_coastdist.coast_dist_sign, fagelsundet_gulls_all_2014_gps_coldist.col_dist, fagelsundet_gulls_all_2014_gps_coldist.trip_1km, fagelsundet_gulls_all_2014_gps_sitedist.landfill_dist_min, fagelsundet_gulls_all_2014_gps_sitedist.light_dist_min, fagelsundet_gulls_all_2014_gps_sitedist.landfill_closest, fagelsundet_gulls_all_2014_gps_sitedist.light_closest
                       FROM (((fagelsundet_gulls_all_2014_gps_points INNER JOIN fagelsundet_gulls_all_2014_gps_trip_id_par ON (fagelsundet_gulls_all_2014_gps_points.date_time = fagelsundet_gulls_all_2014_gps_trip_id_par.date_time) AND (fagelsundet_gulls_all_2014_gps_points.device_info_serial = fagelsundet_gulls_all_2014_gps_trip_id_par.device_info_serial)) INNER JOIN fagelsundet_gulls_all_2014_gps_coastdist ON (fagelsundet_gulls_all_2014_gps_points.date_time = fagelsundet_gulls_all_2014_gps_coastdist.date_time) AND (fagelsundet_gulls_all_2014_gps_points.device_info_serial = fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial)) INNER JOIN fagelsundet_gulls_all_2014_gps_sitedist ON (fagelsundet_gulls_all_2014_gps_points.date_time = fagelsundet_gulls_all_2014_gps_sitedist.date_time) AND (fagelsundet_gulls_all_2014_gps_points.device_info_serial = fagelsundet_gulls_all_2014_gps_sitedist.device_info_serial)) INNER JOIN fagelsundet_gulls_all_2014_gps_coldist ON (fagelsundet_gulls_all_2014_gps_points.date_time = fagelsundet_gulls_all_2014_gps_coldist.date_time) AND (fagelsundet_gulls_all_2014_gps_points.device_info_serial = fagelsundet_gulls_all_2014_gps_coldist.device_info_serial)
                       ORDER BY fagelsundet_gulls_all_2014_gps_points.device_info_serial, fagelsundet_gulls_all_2014_gps_points.date_time;
                       
                       "
                       ,as.is = TRUE)

str(gps.points)
gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))

# Deployment data including species
deployments <- sqlQuery(gps.db,
                        query =
                          "SELECT gps_ee_track_session_limited2.*, gps_ee_individual_limited.species_latin_name, gps_ee_individual_limited.colour_ring, gps_ee_individual_limited.start_date, gps_ee_individual_limited.individual_id
                        FROM gps_ee_track_session_limited2 INNER JOIN gps_ee_individual_limited ON gps_ee_track_session_limited2.ring_number = gps_ee_individual_limited.ring_number
                        WHERE (((gps_ee_track_session_limited2.key_name)='V_FAGELSUNDET'));
                        "
                        ,as.is = TRUE)


# Are all gps location points represented in deployment table?
all(gps.points$device_info_serial %in% (deployments$device_info_serial))
# Yes!

# Check what species are included
unique(deployments$species_latin_name)

# Merge GPS + deployment info to include species and ring_number
gps.points <- merge(gps.points,
                    deployments[,c(2,3,13)],
                    by.x = "device_info_serial",
                    by.y = "device_info_serial",
                    all.x = TRUE,
                    all.y = FALSE)

# Drop caspian terns
gps.points <- filter(gps.points, species_latin_name !=  "Hydroprogne caspia")

# summary((gps.points$trip_id == 0))

# # Drop non-trip points
# gps.points <- filter(gps.points, trip_id !=  0)
# gps.points <- gps.points.original
# gps.points.original <- gps.points
# 
# gps.points <- gps.points.original[1:100,]

# summary(is.na(gps.points$trip_id))

# 2. Load in water body data -------
load("waterways.Rdata")


# 3. Make water polygons buffers ------

# Check projection of data
projection(water.lines)
projection(water.polygons)

# 
# # Testing with subset
# # ?gBuffer
# x <- water.lines@data$id
# # ids.sub <- sample(x, 100)
# ids.sub <- x[1:100]
# water.lines.sub <- water.lines.sub[[1]]
# 
# water.lines@data$X.id[1:10]
# # water.lines.sub$NAME[1]
# water.lines@data$id[1:10]
# water.lines.sub <- subset(water.lines, water.lines@data$id %in% ids.sub[2])
# # 
# # water.lines.sub.proj <- spTransform(water.lines.sub, CRS=CRS("+proj=merc +ellps=GRS80"))
# # # ?spTransform


water.lines.SWEREF_99 <- spTransform(water.lines[,c(1)],
                                     CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))

water.lines.SWEREF_99.20m <- gBuffer(water.lines.SWEREF_99, byid = TRUE,
                           width = 20, quadsegs = 8,
                           capStyle = "ROUND"
                           )

water.lines.SWEREF_99.50m <- gBuffer(water.lines.SWEREF_99, byid = TRUE,
                           width = 50, quadsegs = 8,
                           capStyle = "ROUND"
)

# # Check how this looks
# plot(water.lines.SWEREF_99.50m[5,], col = "red", border = NA)
# plot(water.lines.SWEREF_99.20m[5,], col = "yellow", add = TRUE, border = NA)
# plot(water.lines.SWEREF_99[5,], col = "black", add = TRUE, lwd = 0.5)


water.lines2.SWEREF_99 <- spTransform(water.lines2[,c(1)],
                                     CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))

water.lines2.SWEREF_99.20m <- gBuffer(water.lines2.SWEREF_99, byid = TRUE,
                                     width = 20, quadsegs = 8,
                                     capStyle = "ROUND"
)

water.lines2.SWEREF_99.50m <- gBuffer(water.lines2.SWEREF_99, byid = TRUE,
                                     width = 50, quadsegs = 8,
                                     capStyle = "ROUND"
)

# # Check how this looks
# plot(water.lines2.SWEREF_99.50m[5,], col = "red", border = NA)
# plot(water.lines2.SWEREF_99.20m[5,], col = "yellow", add = TRUE, border = NA)
# plot(water.lines2.SWEREF_99[5,], col = "black", add = TRUE, lwd = 0.5)





water.polygons.SWEREF_99 <- spTransform(water.polygons[,c(1)],
                                     CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))


# Are polygons single polygons?
z <- sapply(water.polygons.SWEREF_99@polygons, function(x) length(x))
all(z == 1)
# Yes

# Number of vertices
n.vert <- sapply(water.polygons.SWEREF_99@polygons, function(y) nrow(y@Polygons[[1]]@coords))
# # How many fewer than 4?
# summary(n.vert <4)
# # just 1

# Drop this
# water.polygons.SWEREF_99
water.polygons.SWEREF_99 <- water.polygons.SWEREF_99[n.vert >3,]

water.polygons.SWEREF_99.20m <- gBuffer(water.polygons.SWEREF_99, byid = TRUE,
                                        width = 20, quadsegs = 8,
                                        capStyle = "ROUND"
)

water.polygons.SWEREF_99.50m <- gBuffer(water.polygons.SWEREF_99, byid = TRUE,
                                        width = 50, quadsegs = 8,
                                        capStyle = "ROUND"
)

# # Check how this looks
# i <- 6
# plot(water.polygons.SWEREF_99[i,], col = "black",  lwd = 0.5)
# 
# plot(water.polygons.SWEREF_99.50m[i,], col = "red", border = NA)
# plot(water.polygons.SWEREF_99.20m[i,], col = "yellow", add = TRUE, border = NA)
# plot(water.polygons.SWEREF_99[i,], col = "black", add = TRUE, lwd = 0.5)
# 



water.polygons2.SWEREF_99 <- spTransform(water.polygons2[,c(1)],
                                        CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))


# Are polygons single polygons?
z <- sapply(water.polygons2.SWEREF_99@polygons, function(x) length(x))
all(z == 1)
# Yes

# Number of vertices
n.vert <- sapply(water.polygons2.SWEREF_99@polygons, function(y) nrow(y@Polygons[[1]]@coords))
# # How many fewer than 4?
# summary(n.vert <4)
# # just 1

# Drop this
# water.polygons2.SWEREF_99
water.polygons2.SWEREF_99 <- water.polygons2.SWEREF_99[n.vert >3,]

water.polygons2.SWEREF_99.20m <- gBuffer(water.polygons2.SWEREF_99, byid = TRUE,
                                        width = 20, quadsegs = 8,
                                        capStyle = "ROUND"
)

water.polygons2.SWEREF_99.50m <- gBuffer(water.polygons2.SWEREF_99, byid = TRUE,
                                        width = 50, quadsegs = 8,
                                        capStyle = "ROUND"
)

# # Check how this looks
# i <- 6
# plot(water.polygons2.SWEREF_99[i,], col = "black",  lwd = 0.5)
# 
# plot(water.polygons2.SWEREF_99.50m[i,], col = "red", border = NA)
# plot(water.polygons2.SWEREF_99.20m[i,], col = "yellow", add = TRUE, border = NA)
# plot(water.polygons2.SWEREF_99[i,], col = "black", add = TRUE, lwd = 0.5)


# 4. Make spatial points dataframe from gps.points ------
# Make points object
xy.loc <- cbind(gps.points$longitude,
                gps.points$latitude)



# Make spatial points object from xy locations
xy.sp.points <- SpatialPoints(xy.loc,
                              proj4string = CRS(
                                "+proj=longlat +datum=WGS84"))

# Re-project point data
xy.sp.points_SWEREF_99 <- spTransform(xy.sp.points, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))




# 5. Perform calculations -------



# Calculate if in polygon
# Useing package 'sp'
points.in.water.lines.SWEREF_99.20m <- over(xy.sp.points_SWEREF_99,
                       water.lines.SWEREF_99.20m,
                       returnList = FALSE, fn = NULL)


# # Label 'land' or 'sea' depending on above output
# test.x <- !is.na(points.in.water.lines.SWEREF_99.20m$id)
# summary(test.x)

points.in.water.lines.SWEREF_99.50m <- over(xy.sp.points_SWEREF_99,
                                            water.lines.SWEREF_99.50m,
                                            returnList = FALSE, fn = NULL)



points.in.water.lines2.SWEREF_99.20m <- over(xy.sp.points_SWEREF_99,
                                            water.lines2.SWEREF_99.20m,
                                            returnList = FALSE, fn = NULL)



points.in.water.lines2.SWEREF_99.50m <- over(xy.sp.points_SWEREF_99,
                                            water.lines2.SWEREF_99.50m,
                                            returnList = FALSE, fn = NULL)


# Calculate if in polygon
# Useing package 'sp'
points.in.water.polygons.SWEREF_99.20m <- over(xy.sp.points_SWEREF_99,
                                            water.polygons.SWEREF_99.20m,
                                            returnList = FALSE, fn = NULL)


# # Label 'land' or 'sea' depending on above output
# test.x <- !is.na(points.in.water.polygons.SWEREF_99.20m$id)
# summary(test.x)

points.in.water.polygons.SWEREF_99.50m <- over(xy.sp.points_SWEREF_99,
                                            water.polygons.SWEREF_99.50m,
                                            returnList = FALSE, fn = NULL)



points.in.water.polygons2.SWEREF_99.20m <- over(xy.sp.points_SWEREF_99,
                                             water.polygons2.SWEREF_99.20m,
                                             returnList = FALSE, fn = NULL)



points.in.water.polygons2.SWEREF_99.50m <- over(xy.sp.points_SWEREF_99,
                                             water.polygons2.SWEREF_99.50m,
                                             returnList = FALSE, fn = NULL)


# 6. Combind into table for export ------
# z <- gps.points[,c("device_info_serial",
#               "date_time",
#               "trip_id")]

water.df <- cbind.data.frame(gps.points[,c("device_info_serial",
                                           "date_time",
                                           "trip_id")],
                           !is.na(points.in.water.lines.SWEREF_99.20m$id),
                           !is.na(points.in.water.lines.SWEREF_99.50m$id),
                           !is.na(points.in.water.lines2.SWEREF_99.20m$id),
                           !is.na(points.in.water.lines2.SWEREF_99.50m$id),
                           !is.na(points.in.water.polygons.SWEREF_99.20m$id),
                           !is.na(points.in.water.polygons.SWEREF_99.50m$id),
                           !is.na(points.in.water.polygons2.SWEREF_99.20m$id),
                           !is.na(points.in.water.polygons2.SWEREF_99.50m$id)
)

# Fix names
names(water.df) <- c(names(water.df)[1:3],
                     "water.lines.20",
                     "water.lines.50",
                     "water.lines2.20",
                     "water.lines2.50",
                     "water.polygons.20",
                     "water.polygons.50",
                     "water.polygons2.20",
                     "water.polygons2.50"
)



# Combine some columns
water.df$all.water.50 <- rowSums(water.df[,4:11])>0
summary(as.factor(water.df$all.water.50))
water.df$all.water.20 <- rowSums(water.df[,c(4,6,8,10)])>0
summary(water.df$all.water.20)

water.df$poly.water.50 <- rowSums(water.df[,c(9,11)])>0
water.df$poly.water.20 <- rowSums(water.df[,c(8,10)])>0
summary(water.df$poly.water.20)



# 7. Output to database -----


gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')
# str(water.df)
# getSqlTypeInfo()
getSqlTypeInfo("ACCESS")
fagelsundet_gulls_all_2014_gps_inland_water <- water.df

# x <- names(fagelsundet_gulls_all_2014_gps_inland_water)
varTypes <- list(device_info_serial = "INTEGER", date_time = "Date",
                 trip_id = "varchar(255)", water.lines.20 = "varchar(5)",
                  water.lines.50 = "varchar(5)", water.lines2.20 = "varchar(5)",
                  water.lines2.50 = "varchar(5)", water.polygons.20 = "varchar(5)",
                  water.polygons.50 = "varchar(5)", water.polygons2.20 = "varchar(5)",
                  water.polygons2.50 = "varchar(5)", all.water.50 = "varchar(5)",
                  all.water.20 = "varchar(5)", poly.water.50 = "varchar(5)",
                  poly.water.20 = "varchar(5)")

#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, fagelsundet_gulls_all_2014_gps_inland_water,
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = varTypes
)

# ?sqlSave

close(gps.db)

# 
# water.df[2048,2]
                             