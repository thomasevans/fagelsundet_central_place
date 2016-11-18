# Summarising activity by 15 minute blocks, to look at activity by time of day


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



# 2. Summary info for each calendar day + time block -------
# For each trip do following ...
# Probably use plyr thing for speed an efficiency
# browseVignettes(package = "dplyr")


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


# 3. Time blockof day -------

# Second of day
gps.points$second_day <- as.numeric(format(gps.points$date_time, "%S")) + (as.numeric(format(gps.points$date_time, "%H"))*60*60)+ (as.numeric(format(gps.points$date_time, "%M"))*60)

# 15 minute block of day
gps.points$block_15min <- floor(gps.points$second_day/900)

# unique j_day + minute block
gps.points$j_day_block_15min <- paste(gps.points$j_day, gps.points$block_15min, sep = "_")
# head(gps.points$j_day_block_15min)

# length(unique(gps.points$j_day_block_15min))

# ring_number + unique j_day + minute block
gps.points$ring_j_day_block_15min <- paste(gps.points$ring_number,
                                           gps.points$j_day,
                                           gps.points$block_15min, sep = "_")
# head(gps.points$ring_j_day_block_15min)


# 4. Summarise by block of day ------

block.df <-gps.points %>%
  group_by(ring_j_day_block_15min) %>%
  summarise(
    
    #details
    ring_number = first(ring_number),
    device_info_serial = first(device_info_serial),
    species = first(species_latin_name),
    j_day = first(j_day),
    block_15min = first(block_15min),
    
    # Variables to extract
    #Coldist
    coldist_max = max(col_dist, na.rm = TRUE),
    # col_dist_mean = mean(col_dist, na.rm = TRUE),
    col_dist_median = median(col_dist, na.rm = TRUE),
    col_dist_min = min(col_dist, na.rm = TRUE),
      
    #duration + time
    first_point_time = first(date_time),
    last_point_time = last(date_time),
    duration_sum_points = sum(time_interval, na.rm = TRUE),
    
    # Quality criteria
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
    colony_visited = p_colony >0

  )


# Main habitat
p_max <- apply(block.df[,c("p_sea","p_coast","p_land","p_colony")],1,max)
col_max <- block.df[,c("p_sea","p_coast","p_land","p_colony")] == p_max

# How many records have more than one habitat selected?
cols_selcted <- apply(col_max,1,sum)
summary(as.factor(cols_selcted))
# - Not many (<50 of 55,500)

small.fun <- function(x){
  if(sum(is.na(x))>0)return(NA) else{
    habs <- c("sea","coast","land","colony")[x]
    return(habs[1])
  }
}

block.df$habitat_main <- apply(col_max,1,small.fun)



# 5. Fill in missing blocks ------


# Find missing days -----
ring_numbers <- unique(gps.points$ring_number)
# j_days_include

x <- expand.grid(unique(gps.points$ring_number), j_days_include, unique(gps.points$block_15min))
all.blocks <- cbind.data.frame(paste(x[,1],x[,2], x[,3],sep="_"), x)

names(all.blocks) <- c("ring_j_day_block_15min",
                       "ring_number",
                       "j_day",
                       "block_15min")
# Order
all.blocks <- arrange(all.blocks, ring_j_day_block_15min)


# Merge all.blocks + deployment info to include species and device_info_serial
all.blocks <- merge(all.blocks,
                  deployments[,c(2,3,13)],
                  by.x = "ring_number",
                  by.y = "ring_number",
                  all.x = TRUE,
                  all.y = FALSE)
names(all.blocks)[5] <- "species"
# all.days$bird_day
str(all.blocks)
str(block.df)
all.blocks$ring_j_day_block_15min <- as.character(all.blocks$ring_j_day_block_15min)

# Combine with block info
all.blocks.combined <- merge(all.blocks, block.df,
                           by = "ring_j_day_block_15min",
                           all.x = TRUE)

# sum(block.df$ring_j_day_block_15min %in% all.blocks$ring_j_day_block_15min)

# all.blocks.combined$coldist_max
# all(is.na(all.blocks.combined$coldist_max))
# ?geom_tile

# Add date_time
# date from j_day and time from block (time difference + to day)

all.blocks.combined$date <- as.Date(all.blocks.combined$j_day.x, origin=as.Date("2014-01-01", tz = "UTC"))
all.blocks.combined$date_time <- as.POSIXct(all.blocks.combined$date, tz = "UTC") + (all.blocks.combined$block_15min.x * 900) + 450

# fix names 
all.blocks.combined <- all.blocks.combined[,!(colnames(all.blocks.combined) %in% c("ring_number.y","species.y", 
                                               "j_day.y","block_15min.y"))]

names(all.blocks.combined)[2:5] <- c("ring_number", "j_day", "block_15min", "species")




# 6. Illustrate some of this --------

theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "top",
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12, face = "italic"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        # legend.text = element_text(colour="blue", size = 16, face = "bold")
        legend.key.width = unit(3, "lines")
  )


# all.blocks.combined$coldist_max_calc <- sqrt(all.blocks.combined$coldist_max/1000)/sqrt(0.5)
all.blocks.combined$coldist_max_calc <- log(all.blocks.combined$coldist_max/1000)
plot(log(c(0.1,0.5,1:100))~c(0.1,0.5,1:100))
hist(all.blocks.combined$coldist_max_calc)

ggplot(all.blocks.combined, aes(date_time, ring_number, fill = coldist_max_calc)) + 
  # geom_tile() + 
  geom_raster()+
  scale_fill_gradient2(low = ("#5e3c99"), mid = "white",
                       high = ("#e66101"), midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar")+
  facet_grid(species_latin_name~., scales = "free" , space = "free_y") +
  theme_new

ggsave(file="colony_distance_15min.png")
ggsave(file="colony_distance_15min.svg")
ggsave(file="colony_distance_15min.pdf")

all.blocks.combined$colony_visited <- log(all.blocks.combined$coldist_max/1000)


ggplot(all.blocks.combined, aes(date_time, ring_number, fill = colony_visited)) + 
  # geom_tile() + 
  geom_raster()+
#   scale_fill_gradient2(low = ("#5e3c99"), mid = "white",
#                        high = ("#e66101"), midpoint = 0, space = "Lab",
#                        na.value = "grey50", guide = "colourbar")+
  facet_grid(species_latin_name~., scales = "free" , space = "free_y") +
  theme_new

ggsave(file="colony_visited_15min.png")
ggsave(file="colony_visited_15min.svg")
ggsave(file="colony_visited_15min.pdf")



unique(all.blocks.combined$habitat_main)
all.blocks.combined$habitat_main <- factor(all.blocks.combined$habitat_main,
                                           levels =c("colony", "sea","coast","land"))
# str(all.blocks.combined$habitat_main)
ggplot(all.blocks.combined, aes(date_time, ring_number, fill = habitat_main,
                                order=habitat_main)) + 
  # geom_tile() + 
  geom_raster()+
  scale_fill_manual(values=c("white", "#8da0cb", "#fc8d62", "#66c2a5"),
                    na.value = "grey50")+
#   scale_fill_gradient2(low = ("#5e3c99"), mid = "white",
#                        high = ("#e66101"), midpoint = 0, space = "Lab",
#                        na.value = "grey50", guide = "colourbar")+
  facet_grid(species_latin_name~., scales = "free" , space = "free_y") +
  theme_new

ggsave(file="habitat_15min.png")
ggsave(file="habitat_15min.svg")
ggsave(file="habitat_15min.pdf")

# log(c(0.1,0.5,1,5,10,100))
