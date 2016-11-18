# Summarising activity by 30 minute blocks, to look at activity by time of day


# Packages -----
library("RODBC")
library("dplyr")
library("ggplot2")
library(scales)
library(cowplot)
library("maptools")
library("reshape2")

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


# 3. Time block of day -------

# Second of day
gps.points$second_day <- as.numeric(format(gps.points$date_time, "%S")) + (as.numeric(format(gps.points$date_time, "%H"))*60*60)+ (as.numeric(format(gps.points$date_time, "%M"))*60)

# 30 minute block of day
gps.points$block_30min <- floor(gps.points$second_day/1800)

# unique j_day + minute block
gps.points$j_day_block_30min <- paste(gps.points$j_day, gps.points$block_30min, sep = "_")
# head(gps.points$j_day_block_30min)

# length(unique(gps.points$j_day_block_30min))

# ring_number + unique j_day + minute block
gps.points$ring_j_day_block_30min <- paste(gps.points$ring_number,
                                           gps.points$j_day,
                                           gps.points$block_30min, sep = "_")
# head(gps.points$ring_j_day_block_30min)


# 4. Summarise by block of day ------

block.df <-gps.points %>%
  group_by(ring_j_day_block_30min) %>%
  summarise(
    
    #details
    ring_number = first(ring_number),
    device_info_serial = first(device_info_serial),
    species = first(species_latin_name),
    j_day = first(j_day),
    block_30min = first(block_30min),
    
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




# Main habitat with landfill included
block.df$p_land_nolandfill <- block.df$p_land - block.df$p_landfill

p_max <- apply(block.df[,c("p_sea","p_coast",
                           "p_land_nolandfill","p_colony","p_landfill")],1,max)
col_max <- block.df[,c("p_sea","p_coast",
                       "p_land_nolandfill","p_colony","p_landfill")] == p_max

# How many records have more than one habitat selected?
cols_selcted <- apply(col_max,1,sum)
summary(as.factor(cols_selcted))
# - Not many (<50 of 55,500)

small.fun <- function(x){
  if(sum(is.na(x))>0)return(NA) else{
    habs <- c("sea","coast",
              "land_nolandfill","colony","landfill")[x]
    return(habs[1])
  }
}

block.df$habitat_main_incl_landfill <- apply(col_max,1,small.fun)
summary(as.factor(block.df$habitat_main_incl_landfill))


# 5. Fill in missing blocks ------


# Find missing days -----
ring_numbers <- unique(gps.points$ring_number)
# j_days_include
# length(unique(gps.points$block_30min))
x <- expand.grid(unique(gps.points$ring_number), j_days_include, unique(gps.points$block_30min))
all.blocks <- cbind.data.frame(paste(x[,1],x[,2], x[,3],sep="_"), x)

names(all.blocks) <- c("ring_j_day_block_30min",
                       "ring_number",
                       "j_day",
                       "block_30min")
# Order
all.blocks <- arrange(all.blocks, ring_j_day_block_30min)


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
all.blocks$ring_j_day_block_30min <- as.character(all.blocks$ring_j_day_block_30min)

# Combine with block info
all.blocks.combined <- merge(all.blocks, block.df,
                             by = "ring_j_day_block_30min",
                             all.x = TRUE)

# sum(block.df$ring_j_day_block_30min %in% all.blocks$ring_j_day_block_30min)

# all.blocks.combined$coldist_max
# all(is.na(all.blocks.combined$coldist_max))
# ?geom_tile

# Add date_time
# date from j_day and time from block (time difference + to day)

all.blocks.combined$date <- as.Date(all.blocks.combined$j_day.x, origin=as.Date("2014-01-01", tz = "UTC"))
all.blocks.combined$date_time <- as.POSIXct(all.blocks.combined$date, tz = "UTC") + (all.blocks.combined$block_30min.x * 1800) + 900

# fix names 
all.blocks.combined <- all.blocks.combined[,!(colnames(all.blocks.combined) %in% c("ring_number.y","species.y", 
                                                                                   "j_day.y","block_30min.y"))]

names(all.blocks.combined)[2:5] <- c("ring_number", "j_day", "block_30min", "species")




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

theme_new_rt <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "right",
        legend.justification = c(1, 1),
        legend.key.size =   unit(1, "lines"),
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
# plot(log(c(0.1,0.5,1:100))~c(0.1,0.5,1:100))
# hist(all.blocks.combined$coldist_max_calc)

ggplot(all.blocks.combined, aes(date_time, ring_number, fill = coldist_max_calc)) + 
  geom_tile() + 
  # geom_raster()+
  scale_fill_gradient2(low = ("#5e3c99"), mid = "white",
                       high = ("#e66101"), midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar")+
  facet_grid(species_latin_name~., scales = "free" , space = "free_y") +
  theme_new

ggsave(file="colony_distance_30min.png")
ggsave(file="colony_distance_30min.svg")
ggsave(file="colony_distance_30min.pdf")

# all.blocks.combined$colony_visited <- log(all.blocks.combined$coldist_max/1000)


ggplot(all.blocks.combined, aes(date_time, ring_number, fill = colony_visited)) + 
  # geom_tile() + 
  geom_raster()+
  #   scale_fill_gradient2(low = ("#5e3c99"), mid = "white",
  #                        high = ("#e66101"), midpoint = 0, space = "Lab",
  #                        na.value = "grey50", guide = "colourbar")+
  facet_grid(species_latin_name~., scales = "free" , space = "free_y") +
  theme_new

ggsave(file="colony_visited_30min.png")
ggsave(file="colony_visited_30min.svg")
ggsave(file="colony_visited_30min.pdf")



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

ggsave(file="habitat_30min.png")
ggsave(file="habitat_30min.svg")
ggsave(file="habitat_30min.pdf")

# log(c(0.1,0.5,1,5,10,100))


x <- filter(all.blocks.combined, ring_number == "90A94466")

x <- filter(all.blocks.combined, ring_number == "90A94468")
x <- filter(all.blocks.combined, ring_number == "9181960")
x <- filter(all.blocks.combined, ring_number == "90A94464")
x <- filter(all.blocks.combined, ring_number == "90A94463")



 ggplot(x, aes(block_30min, j_day, fill = habitat_main)) + 
  geom_tile(colour = "white") +
  scale_fill_manual(values=c("white", "#8da0cb", "#fc8d62", "#66c2a5"),
                    na.value = "grey50")
  # facet_grid(year~monthf) +
  # scale_fill_gradient(low="red", high="yellow") +
  # opts(title = "Time-Series Calendar Heatmap") +
  # xlab("Week of Month") +
  # ylab("")

P


# 7. Individual summaries of habitats by time of day ------


all.blocks.combined$ring_number_block_30min <- paste(all.blocks.combined$ring_number,
                                                     all.blocks.combined$block_30min,
                                                     sep = "_")

individual_time_df <-all.blocks.combined %>%
  group_by(ring_number_block_30min) %>%
  summarise(
    
    #details
    ring_number = first(ring_number),
    device_info_serial = first(device_info_serial),
    species = first(species_latin_name),
    block_30min = first(block_30min),
    
    # Variables to extract
    #Coldist
    col_dist_max_median = median(coldist_max, na.rm = TRUE),
    col_dist_max_mean = mean(coldist_max, na.rm = TRUE),
    
    # Habitats
    n = n(),
    n_noNA = n - sum(is.na(habitat_main_incl_landfill)),
    sea_p_incl_landfill = sum(habitat_main_incl_landfill == "sea", na.rm = TRUE)/n_noNA,
    coast_p_incl_landfill = sum(habitat_main_incl_landfill == "coast", na.rm = TRUE)/n_noNA,
    land_nolandfill_p_incl_landfill = sum(habitat_main_incl_landfill == "land_nolandfill", na.rm = TRUE)/n_noNA,
    colony_p_incl_landfill = sum(habitat_main_incl_landfill == "colony", na.rm = TRUE)/n_noNA,
    landfill_p_incl_landfill = sum(habitat_main_incl_landfill == "landfill", na.rm = TRUE)/n_noNA

  )

# ??melt

individual_time_df_sub <- filter(individual_time_df, ring_number == "90A94466")
melted.df <- melt(individual_time_df_sub, id.vars = "block_30min",
                  measure.vars = c("sea_p_incl_landfill",
                                   "coast_p_incl_landfill",
                                   "land_nolandfill_p_incl_landfill",
                                   "colony_p_incl_landfill",
                                   "landfill_p_incl_landfill"),
                  variable.name = "habitat")
str(melted.df)

ggplot(melted.df,
       aes(x=block_30min)) + 
   geom_bar(aes(weight=value, fill = habitat), position = 'fill')



# individual_time_df_sub <- filter(individual_time_df, ring_number == "90A94466")
melted.df <- melt(individual_time_df, id.vars = c("ring_number_block_30min",
                                                      "ring_number",
                                                      "species",
                                                      "block_30min"),
                  measure.vars = c("sea_p_incl_landfill",
                                   "coast_p_incl_landfill",
                                   "land_nolandfill_p_incl_landfill",
                                   "colony_p_incl_landfill",
                                   "landfill_p_incl_landfill"),
                  variable.name = "habitat")

df.names <- unique(melted.df[,c("species", "ring_number")])
df.names$individual <- c(1:3,1:6,1:8,1:3)

melted.df <- merge(melted.df, df.names,
                   by = intersect(names(melted.df), names(df.names)))


melted.df$habitat <- factor(melted.df$habitat,
                                           levels =c("landfill_p_incl_landfill",
                                                     "land_nolandfill_p_incl_landfill",
                                                     "coast_p_incl_landfill",
                                                     "sea_p_incl_landfill",
                                                     "colony_p_incl_landfill"
                                                     ))

melted.df$habitat <- plyr::mapvalues(melted.df$habitat, from = c("landfill_p_incl_landfill",
                                      "land_nolandfill_p_incl_landfill",
                                      "coast_p_incl_landfill",
                                      "sea_p_incl_landfill",
                                      "colony_p_incl_landfill"),
          to = c("landfill",
                 "land",
                 "coast",
                 "sea",
                 "colony"))


#sunrise/set times
# Island centre
lon <- 17.9328291
lat <- 60.6309369
coord <- matrix(c(lon, lat), nrow = 1)
date_time_centre <- as.POSIXct("2014-06-15 00:00:00", tz = "UTC")
t_sunrise <- sunriset(coord, date_time_centre, direction = "sunrise",
                      POSIXct.out = TRUE)[,2]
t_sunset <- sunriset(coord, date_time_centre, direction = "sunset",
                     POSIXct.out = TRUE)[,2]
t_noon <- solarnoon(coord, date_time_centre,
                    POSIXct.out = TRUE)[,2]

t_times <- c(((2+(17/60))*2), ((11+(48/60))*2), ((21+(21/60))*2))
t_times <- t_times - 0.5

melted.df$block_30min_new <- (melted.df$block_30min + 2) %% 48


ggplot(melted.df,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(species~individual, space = "free_x",
             drop = TRUE, margins = c("individual"))+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")



ggsave(file="habitat_time_of_day_all_birds.svg",
       width = 16, height = 10)
ggsave(file="habitat_time_of_day_all_birds.png",
       width = 16, height = 10)




# 8. Species summaries of habitats by time of day ------




individual_time_df$species_block_30min <- paste(individual_time_df$species,
                                                individual_time_df$block_30min,
                                                     sep = "_")

species_time_df <-individual_time_df %>%
  group_by(species_block_30min) %>%
  summarise(
    
    #details
    species = first(species),
    block_30min = first(block_30min),
    

    # Habitats
    sea_p_incl_landfill = mean(sea_p_incl_landfill , na.rm = TRUE),
    coast_p_incl_landfill = mean(coast_p_incl_landfill , na.rm = TRUE),
    land_nolandfill_p_incl_landfill = mean(land_nolandfill_p_incl_landfill, na.rm = TRUE),
    colony_p_incl_landfill = mean(colony_p_incl_landfill , na.rm = TRUE),
    landfill_p_incl_landfill = mean(landfill_p_incl_landfill , na.rm = TRUE),
    total_p = sum(sea_p_incl_landfill, coast_p_incl_landfill,
                  land_nolandfill_p_incl_landfill, colony_p_incl_landfill,
                  landfill_p_incl_landfill)
    
  )

# ??melt
# 
# # individual_time_df_sub <- filter(individual_time_df, ring_number == "90A94466")
# melted.df <- melt(species_time_df, id.vars = "block_30min",
#                   measure.vars = c("sea_p_incl_landfill",
#                                    "coast_p_incl_landfill",
#                                    "land_nolandfill_p_incl_landfill",
#                                    "colony_p_incl_landfill",
#                                    "landfill_p_incl_landfill"),
#                   variable.name = "habitat")
# str(melted.df)
# 
# ggplot(melted.df,
#        aes(x=block_30min)) + 
#   geom_bar(aes(weight=value, fill = habitat), position = 'fill')



# individual_time_df_sub <- filter(individual_time_df, ring_number == "90A94466")
melted.df <- melt(species_time_df, id.vars = c("species",
                                                  "block_30min"),
                  measure.vars = c("sea_p_incl_landfill",
                                   "coast_p_incl_landfill",
                                   "land_nolandfill_p_incl_landfill",
                                   "colony_p_incl_landfill",
                                   "landfill_p_incl_landfill"),
                  variable.name = "habitat")

melted.df$habitat <- factor(melted.df$habitat,
                            levels =c("landfill_p_incl_landfill",
                                      "land_nolandfill_p_incl_landfill",
                                      "coast_p_incl_landfill",
                                      "sea_p_incl_landfill",
                                      "colony_p_incl_landfill"
                            ))

melted.df$habitat <- plyr::mapvalues(melted.df$habitat, from = c("landfill_p_incl_landfill",
                                                                 "land_nolandfill_p_incl_landfill",
                                                                 "coast_p_incl_landfill",
                                                                 "sea_p_incl_landfill",
                                                                 "colony_p_incl_landfill"),
                                     to = c("landfill",
                                            "land",
                                            "coast",
                                            "sea",
                                            "colony"))


#sunrise/set times
# Island centre
lon <- 17.9328291
lat <- 60.6309369
coord <- matrix(c(lon, lat), nrow = 1)
date_time_centre <- as.POSIXct("2014-06-15 00:00:00", tz = "UTC")
t_sunrise <- sunriset(coord, date_time_centre, direction = "sunrise",
                              POSIXct.out = TRUE)[,2]
t_sunset <- sunriset(coord, date_time_centre, direction = "sunset",
                             POSIXct.out = TRUE)[,2]
t_noon <- solarnoon(coord, date_time_centre,
                    POSIXct.out = TRUE)[,2]

t_times <- c(((2+(17/60))*2), ((11+(48/60))*2), ((21+(21/60))*2))
t_times <- t_times - 0.5

melted.df$block_30min_new <- (melted.df$block_30min + 2) %% 48
sort(unique(melted.df$block_30min_new))


ggplot(melted.df,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(species~., space = "free_x",
             drop = TRUE, margins = c("species"))+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")




ggsave(file="habitat_time_of_day_species.svg",
       width = 10, height = 10)
ggsave(file="habitat_time_of_day_species.png",
       width = 10, height = 10)




# no margin thing

ggplot(melted.df,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(species~., space = "free_x",
             drop = TRUE)+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")




ggsave(file="habitat_time_of_day_species_no_margin.svg",
       width = 10, height = 10)
ggsave(file="habitat_time_of_day_species_no_margin.png",
       width = 10, height = 10)


# 9. Individual summaries by day of week ------

str(block.df)

# j_day to date
block.df$date <- as.Date(as.numeric(as.character(block.df$j_day)), origin=as.Date("2014-01-01", tz = "UTC"))

# date to weekday
block.df$weekday_name <- format(block.df$date, "%A")
block.df$weekday_n <- format(block.df$date, "%u")


block.df$ring_w_day_block_30min <- paste(block.df$ring_number,
                                         block.df$weekday_n,
                                         block.df$block_30min, sep = "_")

# Summarise
individual_day_week_block_df <-block.df %>%
  group_by(ring_w_day_block_30min) %>%
  summarise(
    
    #details
    ring_number = first(ring_number),
    device_info_serial = first(device_info_serial),
    species = first(species),
    block_30min = first(block_30min),
    weekday = first(weekday_name),
    weekday_n = first(weekday_n),
    
    
#     # Variables to extract
#     #Coldist
#     col_dist_max_median = median(coldist_max, na.rm = TRUE),
#     col_dist_max_mean = mean(coldist_max, na.rm = TRUE),
#     
    # Habitats
    n = n(),
    n_noNA = n - sum(is.na(habitat_main_incl_landfill)),
    sea_p_incl_landfill = sum(habitat_main_incl_landfill == "sea", na.rm = TRUE)/n_noNA,
    coast_p_incl_landfill = sum(habitat_main_incl_landfill == "coast", na.rm = TRUE)/n_noNA,
    land_nolandfill_p_incl_landfill = sum(habitat_main_incl_landfill == "land_nolandfill", na.rm = TRUE)/n_noNA,
    colony_p_incl_landfill = sum(habitat_main_incl_landfill == "colony", na.rm = TRUE)/n_noNA,
    landfill_p_incl_landfill = sum(habitat_main_incl_landfill == "landfill", na.rm = TRUE)/n_noNA
    
  )




melted.df <- melt(individual_day_week_block_df, id.vars = c("ring_w_day_block_30min",
                                                  "ring_number",
                                                  "species",
                                                  "block_30min",
                                                  "weekday",
                                                  "weekday_n"),
                  measure.vars = c("sea_p_incl_landfill",
                                   "coast_p_incl_landfill",
                                   "land_nolandfill_p_incl_landfill",
                                   "colony_p_incl_landfill",
                                   "landfill_p_incl_landfill"),
                  variable.name = "habitat")

df.names <- unique(melted.df[,c("species", "ring_number")])
df.names$individual <- c(1:3,1:6,1:8,1:3)

melted.df <- merge(melted.df, df.names,
                   by = intersect(names(melted.df), names(df.names)))


melted.df$habitat <- factor(melted.df$habitat,
                            levels =c("landfill_p_incl_landfill",
                                      "land_nolandfill_p_incl_landfill",
                                      "coast_p_incl_landfill",
                                      "sea_p_incl_landfill",
                                      "colony_p_incl_landfill"
                            ))

melted.df$habitat <- plyr::mapvalues(melted.df$habitat, from = c("landfill_p_incl_landfill",
                                                                 "land_nolandfill_p_incl_landfill",
                                                                 "coast_p_incl_landfill",
                                                                 "sea_p_incl_landfill",
                                                                 "colony_p_incl_landfill"),
                                     to = c("landfill",
                                            "land",
                                            "coast",
                                            "sea",
                                            "colony"))

unique(melted.df$species)

# HG
melted.df_hg <- filter(melted.df, species == "Larus argentatus")
melted.df_hg$block_30min_new <- (melted.df_hg$block_30min + 2) %% 48


ggplot(melted.df_hg,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(ring_number~weekday_n, space = "free_x",
             drop = TRUE, margins = c("ring_number","weekday_n"))+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")

ggsave(file="habitat_time_of_day_week_hg.svg",
       width = 20, height = 20)
ggsave(file="habitat_time_of_day_week_hg.png",
       width = 20, height = 20)

# GBBG
melted.df_gbbg <- filter(melted.df, species == "Larus marinus")
melted.df_gbbg$block_30min_new <- (melted.df_gbbg$block_30min + 2) %% 48

# summary(as.factor(melted.df_gbbg$w))

ggplot(melted.df_gbbg,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(ring_number~weekday_n, space = "free_x",
             drop = TRUE, margins = c("ring_number","weekday_n"))+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")

ggsave(file="habitat_time_of_day_week_gbbg.svg",
       width = 20, height = 20)
ggsave(file="habitat_time_of_day_week_gbbg.png",
       width = 20, height = 20)



# LBBG
melted.df_lbbg <- filter(melted.df, species == "Larus fuscus")
melted.df_lbbg$block_30min_new <- (melted.df_lbbg$block_30min + 2) %% 48

# summary(as.factor(melted.df_lbbg$w))

ggplot(melted.df_lbbg,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(ring_number~weekday_n, space = "free_x",
             drop = TRUE, margins = c("ring_number","weekday_n"))+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")

ggsave(file="habitat_time_of_day_week_lbbg.svg",
       width = 20, height = 20)
ggsave(file="habitat_time_of_day_week_lbbg.png",
       width = 20, height = 20)


# CG
melted.df_cg <- filter(melted.df, species == "Larus canus")
melted.df_cg$block_30min_new <- (melted.df_cg$block_30min + 2) %% 48

# summary(as.factor(melted.df_cg$w))

ggplot(melted.df_cg,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(ring_number~weekday_n, space = "free_x",
             drop = TRUE, margins = c("ring_number","weekday_n"))+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")

ggsave(file="habitat_time_of_day_week_cg.svg",
       width = 20, height = 20)
ggsave(file="habitat_time_of_day_week_cg.png",
       width = 20, height = 20)


# 10. Species summaries by days of week -----


individual_day_week_block_df$species_day_block_30min <- paste(individual_day_week_block_df$species,
                                                              individual_day_week_block_df$weekday_n,
                                                              individual_day_week_block_df$block_30min,
                                                sep = "_")

species_day_block_df <-individual_day_week_block_df %>%
  group_by(species_day_block_30min) %>%
  summarise(
    
    #details
    species = first(species),
    block_30min = first(block_30min),
    day_n = first(weekday_n),
    
    
    # Habitats
    sea_p_incl_landfill = mean(sea_p_incl_landfill , na.rm = TRUE),
    coast_p_incl_landfill = mean(coast_p_incl_landfill , na.rm = TRUE),
    land_nolandfill_p_incl_landfill = mean(land_nolandfill_p_incl_landfill, na.rm = TRUE),
    colony_p_incl_landfill = mean(colony_p_incl_landfill , na.rm = TRUE),
    landfill_p_incl_landfill = mean(landfill_p_incl_landfill , na.rm = TRUE),
    total_p = sum(sea_p_incl_landfill, coast_p_incl_landfill,
                  land_nolandfill_p_incl_landfill, colony_p_incl_landfill,
                  landfill_p_incl_landfill)
    
  )




# individual_time_df_sub <- filter(individual_time_df, ring_number == "90A94466")
melted.df <- melt(species_day_block_df, id.vars = c("species",
                                                    "day_n",
                                               "block_30min"),
                  measure.vars = c("sea_p_incl_landfill",
                                   "coast_p_incl_landfill",
                                   "land_nolandfill_p_incl_landfill",
                                   "colony_p_incl_landfill",
                                   "landfill_p_incl_landfill"),
                  variable.name = "habitat")

melted.df$habitat <- factor(melted.df$habitat,
                            levels =c("landfill_p_incl_landfill",
                                      "land_nolandfill_p_incl_landfill",
                                      "coast_p_incl_landfill",
                                      "sea_p_incl_landfill",
                                      "colony_p_incl_landfill"
                            ))

melted.df$habitat <- plyr::mapvalues(melted.df$habitat, from = c("landfill_p_incl_landfill",
                                                                 "land_nolandfill_p_incl_landfill",
                                                                 "coast_p_incl_landfill",
                                                                 "sea_p_incl_landfill",
                                                                 "colony_p_incl_landfill"),
                                     to = c("landfill",
                                            "land",
                                            "coast",
                                            "sea",
                                            "colony"))

melted.df$block_30min_new <- (melted.df$block_30min + 2) %% 48



ggplot(melted.df,
       aes(x=block_30min_new)) + 
  geom_bar(aes(weight=value, fill = habitat), position = 'fill') +
  facet_grid(species~day_n, space = "free_x",
             drop = TRUE, margins = c("species","day_n"))+
  scale_fill_manual(values=c("#e7298a","#66c2a5" , "#fc8d62", "#8da0cb",
                             "grey80")) +
  scale_x_continuous(breaks=c(0,12,24,36,48)-0.5, limits=c(-0.5, 47.5),
                     labels=c("0","6", "12", "18", "24"))  +
  geom_vline(xintercept = t_times, lwd = 1, lty = 1, col = "black",
             alpha = 0.5)+
  theme_new_rt +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Hour of day (UTC+1)", y = "Proportion (half-hour bins)")

ggsave(file="habitat_time_of_day_week_species.svg",
       width = 30, height = 20)
ggsave(file="habitat_time_of_day_week_species.png",
       width = 30, height = 20)

# Check how day numbers align with actual weekdays
unique(cbind(individual_day_week_block_df$weekday, individual_day_week_block_df$weekday_n))

