# Trip summary details

# Packages -----
library("RODBC")
library("dplyr")
library("ggplot2")
library(scales)
library(cowplot)

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


# 2. Determine flight speed cut-offs for each species ------
theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        legend.key.width = unit(3, "lines"),
        legend.title = element_blank()
  )


ggplot(gps.points[gps.points$speed>0.5,], aes(speed, colour = species_latin_name)) +
  geom_density(adjust = 1/4, lwd = 1, alpha = 0.7) +
  xlim(0,30)+
  # ylim(0,0.2)+
  geom_vline(xintercept = 3, lwd = 1.5, lty = 2,
             col = "black",
             alpha = 0.5) +
  theme_new +
  labs(list(x = expression("Speed ms"^{-1}),
            y = "Density",
            colour = "Species")) 

ggsave(filename = "ground_speed_gulls_2.svg", width = 5, height = 4,
       units = "in")
ggsave(filename = "ground_speed_gulls_2.png", width = 5, height = 4,
       units = "in")
ggsave(filename = "ground_speed_gulls_2.pdf", width = 5, height = 4,
       units = "in")


# 3. Summary info for foraging trips -------
# For each trip do following ...
# Probably use plyr thing for speed an efficiency
browseVignettes(package = "dplyr")

# Unique vector of foraging trips
trip_ids <- unique(gps.points$trip_id)

# Summary information for foraging trips
trips.df <- summarise(group_by(filter(gps.points, trip_id !=  0),
                               trip_id),
                      
                      #details
                      ring_number = first(ring_number),
                      device_info_serial = first(device_info_serial),
                      species = first(species_latin_name),
                      
                      
                      # Variables to extract
                      #Coldist
                      coldist_max = max(col_dist, na.rm = TRUE),
                      col_dist_mean = mean(col_dist, na.rm = TRUE),
                      col_dist_median = median(col_dist, na.rm = TRUE),

                                            #duration + time
                      start_time = first(date_time),
                      end_time = last(date_time),
                      duration_s = end_time - start_time,

#                       #p_flight
#                       flight_duration = sum(time_interval[speed > 3 & !is.na(speed)], na.rm = TRUE),
#                       p_flight = flight_duration/duration_s,
#                       
#                       #p areas
#                       p_sea = sum(time_interval[area_class == "Marine"], na.rm = TRUE)/duration_s,
#                       p_coast = sum(time_interval[area_class == "Coast"], na.rm = TRUE)/duration_s,
#                       p_land = sum(time_interval[area_class == "Land"], na.rm = TRUE)/duration_s,
#                       p_landfill = sum(time_interval[landfill_dist_min <0.5], na.rm = TRUE)/duration_s,
#                       p_light = sum(time_interval[light_dist_min <0.05], na.rm = TRUE)/duration_s,
                      
                      # Quality criteria
                      gps_time_interval_min = min(time_interval, na.rm = TRUE),
                      gps_time_interval_max = max(time_interval, na.rm = TRUE),
                      gps_time_interval_median = median(time_interval, na.rm = TRUE),
#                       gps_time_interval_first = first(time_interval),
#                       gps_time_interval_first_max = gps_time_interval_max == gps_time_interval_first,
#                       gps_time_interval_max_5h = gps_time_interval_max >5*60*60,
                      n_points = n()
                      
                      
                      )




trips.df2 <- summarise(group_by(filter(gps.points, trip_id !=  0),
                               trip_id) %>% slice(2:n()) %>% group_by(trip_id),
                      
#                       #details
#                       ring_number = first(ring_number),
#                       device_info_serial = first(device_info_serial),
#                       species = first(species_latin_name),
#                       
#                       
#                       # Variables to extract
#                       #Coldist
#                       coldist_max = max(col_dist, na.rm = TRUE),
#                       col_dist_mean = mean(col_dist, na.rm = TRUE),
#                       col_dist_median = median(col_dist, na.rm = TRUE),
#                       
#                       #duration + time
#                       start_time = first(date_time),
#                       end_time = last(date_time),
                      duration_s_sum = sum(time_interval, na.rm =TRUE),
                      
                      #p_flight
                      flight_duration = sum(time_interval[speed > 3 & !is.na(speed)], na.rm = TRUE),
                      p_flight = flight_duration/duration_s_sum,
                      
                      #p areas
                      p_sea = sum(time_interval[area_class == "Marine"], na.rm = TRUE)/duration_s_sum,
                      p_coast = sum(time_interval[area_class == "Coast"], na.rm = TRUE)/duration_s_sum,
                      p_land = sum(time_interval[area_class == "Land"], na.rm = TRUE)/duration_s_sum,
                      p_landfill = sum(time_interval[landfill_dist_min <0.5], na.rm = TRUE)/duration_s_sum,
                      p_light = sum(time_interval[light_dist_min <0.05], na.rm = TRUE)/duration_s_sum,
                      
#                       # Quality criteria
#                       gps_time_interval_min = min(time_interval, na.rm = TRUE),
#                       gps_time_interval_max = max(time_interval, na.rm = TRUE),
#                       gps_time_interval_median = median(time_interval, na.rm = TRUE),
#                       gps_time_interval_first = first(time_interval),
#                       gps_time_interval_first_max = gps_time_interval_max == gps_time_interval_first,
#                       gps_time_interval_max_5h = gps_time_interval_max >5*60*60,
                      n_points2 = n()
                      
                      
)


trips.all <- merge(trips.df,trips.df2,by="trip_id")


# gps.points$light_dist_min[1:100]
# 
# 
# hist(gps.points$time_interval[gps.points$time_interval < 10000], breaks = 100, xlim = c(0,10000))
# x <- filter(gps.points, trip_id == 20)
# 1500/60
# 
# summary(gps.points$time_interval > 1500 & gps.points$trip_id > 0)

hist(trips.df$coldist_max, xlim = c(0,60000), breaks = 1000)

hist(trips.df$col_dist_median, xlim = c(0,60000), breaks = 1000)

hist(trips.df2$p_sea[trips.df2$p_sea <2], breaks = 100)
hist(trips.df2$p_coast[trips.df2$p_coast <2], breaks = 100)
hist(trips.df2$p_land[trips.df2$p_land <2], breaks = 100)
hist(trips.df2$p_landfill[trips.df2$p_landfill <2], breaks = 20)
hist(trips.df2$p_light[trips.df2$p_light <2], breaks = 20)
sort(trips.df2$p_light, decreasing = TRUE)
sort(trips.df2$p_sea)


x <- filter(trips.df2, p_sea >1.1)
  
# ?sort

# 4. Add additional trip level info ---------
# Departure times according to sunrise
