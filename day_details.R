# Day summary details

# Packages -----
library("RODBC")
library("dplyr")
library("ggplot2")
library(scales)
library(cowplot)
library("maptools")

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



# 2. Summary info for each calendar day -------
# For each trip do following ...
# Probably use plyr thing for speed an efficiency
browseVignettes(package = "dplyr")


# get julian day for each point
gps.points$j_day <- format(gps.points$date_time, "%j")

j_days <- unique(gps.points$j_day)

# J day range
# 25th May - 5th July 2014
s_day <- as.POSIXct("2014-05-25 12:00:00", tz = "utc")
e_day <- as.POSIXct("2014-07-05 12:00:00", tz = "utc")
s_day_j <- format(s_day, "%j")
e_day_j <- format(e_day, "%j")
j_days_include <- c(s_day_j:e_day_j)


# Only retain data from study period
gps.points <- filter(gps.points, j_day %in%  j_days_include)

# Also from 2014 (in case other years are included at all)
gps.points$year <- format(gps.points$date_time, "%Y")
gps.points <- filter(gps.points, year == "2014")
# ?group_by
# str(gps.points)


# ungroup(gps.points)
# Summary information for foraging trips
gps.points$ring_number <- as.factor(gps.points$ring_number)
gps.points$j_day <- as.factor(gps.points$j_day)

day.df <-gps.points %>%
group_by(bird_day = paste(ring_number, j_day, sep = "_")) %>%
 summarise(
                      
                      #details
                      ring_number = first(ring_number),
                      device_info_serial = first(device_info_serial),
                      species = first(species_latin_name),
                      j_day = first(j_day),
                      
                      
                      # Variables to extract
                      #Coldist
                      coldist_max = max(col_dist, na.rm = TRUE),
                      col_dist_mean = mean(col_dist, na.rm = TRUE),
                      col_dist_median = median(col_dist, na.rm = TRUE),
                      
                      #duration + time
                      first_point_time = first(date_time),
                      last_point_time = last(date_time),
                      duration_sum_points = sum(time_interval, na.rm = TRUE),

                      # Quality criteria
                      gps_time_interval_min = min(time_interval, na.rm = TRUE),
                      gps_time_interval_max = max(time_interval, na.rm = TRUE),
                      gps_time_interval_median = median(time_interval, na.rm = TRUE),
                      n_points = n(),
                      
                      #p_flight
                      flight_duration = sum(time_interval[speed > 3 & !is.na(speed)], na.rm = TRUE),
                      p_flight = flight_duration/duration_sum_points,
                      
                      #p areas
                      p_sea = sum(time_interval[area_class == "Marine"], na.rm = TRUE)/duration_sum_points,
                      p_coast = sum(time_interval[area_class == "Coast"], na.rm = TRUE)/duration_sum_points,
                      p_land = sum(time_interval[area_class == "Land"], na.rm = TRUE)/duration_sum_points,
                      p_landfill = sum(time_interval[landfill_dist_min <0.5], na.rm = TRUE)/duration_sum_points,
                      p_light = sum(time_interval[light_dist_min <0.05], na.rm = TRUE)/duration_sum_points,
                      p_colony = sum(time_interval[col_dist <500], na.rm = TRUE)/duration_sum_points,
                      colony_visited = p_colony >0,
              # gps.points$col_dist
              
              # Time gaps
              gps_time_interval_max_1st_excl = max(time_interval[2:n()], na.rm = TRUE)/60/60,
              time_to_first_point_h = as.numeric(format(
                first(date_time), "%H")) +  (as.numeric(
                  format(first(date_time), "%M"))/60),
              time_last_point2endday_h  = 24 - as.numeric(format(
                last(date_time), "%H")) -  (as.numeric(
                  format(last(date_time), "%M"))/60),
              gap_max_4h = any(c(gps_time_interval_max_1st_excl,
                              time_to_first_point_h,
                              time_last_point2endday_h) >4),
              gap_max = max(c(gps_time_interval_max_1st_excl,
                              time_to_first_point_h,
                              time_last_point2endday_h))
                  
)



# hist(day.df$gap_max)
# 
# # ?first
# hist(day.df$p_colony)
# 
# x <- gps.points$date_time[1]
# # format(x, "%I", tz = "UTC")
# x
# as.numeric(format(first(date_time), "%H")) +  as.numeric(format(first(date_time), "%M"))/60
# 
# 24 - as.numeric(format(
#   last(x), "%H")) +  (as.numeric(
#     format(last(x), "%M"))/60)
# 27/60
# # 30/60
# 
# as.numeric(format(
#   first(x), "%H")) +  (as.numeric(
#     format(first(x), "%M"))/60)
# 
# 24 - as.numeric(format(
#   last(x), "%H")) -  (as.numeric(
#     format(last(x), "%M"))/60)
# 27/60


# Find missing days -----
ring_numbers <- unique(gps.points$ring_number)
# j_days_include

x <- expand.grid(ring_numbers, j_days_include)
all.days <- cbind.data.frame(paste(x[,1],x[,2],sep="_"), x)
names(all.days) <- c("bird_day", "ring_number", "j_day")
all.days <- arrange(all.days, bird_day)

# Merge all.days + deployment info to include species and device_info_serial
all.days <- merge(all.days,
                    deployments[,c(2,3,13)],
                    by.x = "ring_number",
                    by.y = "ring_number",
                    all.x = TRUE,
                    all.y = FALSE)
names(all.days)[5] <- "species"
# all.days$bird_day

all.days$bird_day <- as.character(all.days$bird_day)

# Combine with day info
all.days.combined <- merge(all.days, day.df,
                           by = intersect(names(all.days), names(days.df)),
                           all.x = TRUE)

# Add null data for days with no data
all.days.combined$no_data <- TRUE
all.days.combined$no_data[!is.na(all.days.combined$n_points)] <- FALSE





ggplot(all.days.combined, aes(j_day, ring_number, fill = 100*p_colony)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient2(low = muted("red"), mid = "white",
                       high = muted("blue"), midpoint = 5, space = "Lab",
                       na.value = "grey50", guide = "colourbar")+
  facet_grid(species~., scales = "free" , space = "free_y")

#   
#   scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 50, breaks = seq(0,1,0.05))+
  # scale_colour_gradient2(aes(midpoint = 0.5)) +
  # scale_fill_gradientn(colours = c("#D61818","#FFAE63","#FFFFBD","#B5E384")) + 
  # facet_wrap(~ species, ncol = 1)
# ?facet_grid
  