# Calculation of movement paramaters based on re-interpolated data



# Packages -----
library("RODBC")
library("dplyr")
# library("ggplot2")
# library(scales)
# library(cowplot)
# library("maptools")
library("adehabitatLT")


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

str(gps.points)


# Get DB table for deployment_info (including nest location and island location)
dep.info <- sqlQuery(gps.db,
                     query = "SELECT DISTINCT d.ID, d.ring_number, d.species, d.device_info_serial, d.island_name, d.start_latitude_island, d.start_longitude_island
                       FROM fagelsundet_all_2014_deployment_data AS d
                       ORDER BY d.device_info_serial ASC;"
                     ,as.is = TRUE)
str(dep.info)

# 2. Interpolation of foraging trips -------
gps.points.trips <- filter(gps.points, trip_id != 0 & !is.na(trip_id))

trips_ids <- unique(gps.points.trips$trip_id)

# Treat each trip as a 'burst'
gps.points.trips.ltraj <- as.ltraj(gps.points.trips[,4:3], gps.points.trips$date_time,
                                  gps.points.trips$device_info_serial,
                                  burst = gps.points.trips$trip_id, typeII = TRUE)

# ?as.ltraj



# Resample to new fixed time interval ------
# Process with redisltraj from adehabitatLT


# Try 600 s initially
gps.points.trips.ltraj.600 <- redisltraj(gps.points.trips.ltraj, 600, type = "time")

# summary.ltraj(gps.points.trips.ltraj.600)

# win.metafile("trips_original.wmf", width = 7, height = 7)
# plot(gps.points.trips.ltraj[c(1,2,8,6)])
# dev.off()
# 
# win.metafile("trips_600s_int.wmf", width = 7, height = 7)
# plot(gps.points.trips.ltraj.600[c(1,2,8,6)])
# dev.off()



# Convert data back to data.frame ----
gps.points.trips.ltraj.600.df <- ld(gps.points.trips.ltraj.600)


# Change radian angles to degrees
gps.points.trips.ltraj.600.df$deg.relangle <- deg(gps.points.trips.ltraj.600.df$rel.angle)

# Need p2p distance (maybe included???)
# Is column 'dist' the same thing?
source("deg.dist.R")
n <- nrow(gps.points.trips.ltraj.600.df)
gps.points.trips.ltraj.600.df$p2p_dist <- c(0,deg.dist(
  gps.points.trips.ltraj.600.df$x[1:(n-1)],
  gps.points.trips.ltraj.600.df$y[1:(n-1)],
  gps.points.trips.ltraj.600.df$x[2:(n)],
  gps.points.trips.ltraj.600.df$y[2:(n)], km = FALSE
))





# Add colony distance ----
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
gps.points.trips.ltraj.600.df$col.dist <- NULL

# Run for all points
gps.points.trips.ltraj.600.df$col.dist <- mapply(bird.dist,
                   lat = gps.points.trips.ltraj.600.df$y,
                   long = gps.points.trips.ltraj.600.df$x,
                   device_id = gps.points.trips.ltraj.600.df$id,
                   MoreArgs = (list(z = list(dep.info))))



# Output datafile
save(gps.points.trips.ltraj.600.df,
     file = "gps.points.trips.ltraj.600.df.RData")

