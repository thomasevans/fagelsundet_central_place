

# Packages -----
library("RODBC")
library("reshape2")


# 1. Get data from DB -----
# device_info_serial, date_time
# ring_number, species (join to fagelsundet_gulls_2014_deployment_data)
# coast_dist_sign, on_land (fagelsundet_gulls_all_2014_gps_coastdist)
# trip_id, time_interval, area_class (fagelsundet_gulls_all_2014_gps_trip_id_par)

# Connect to DB
# To link to database

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)




# Get data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial, fagelsundet_gulls_all_2014_gps_coastdist.date_time, fagelsundet_all_2014_deployment_data.ring_number, fagelsundet_all_2014_deployment_data.species, fagelsundet_gulls_all_2014_gps_coastdist.coast_dist_sign, fagelsundet_gulls_all_2014_gps_coastdist.on_land, fagelsundet_gulls_all_2014_gps_trip_id_par.trip_id, fagelsundet_gulls_all_2014_gps_trip_id_par.time_interval, fagelsundet_gulls_all_2014_gps_trip_id_par.area_class
FROM (fagelsundet_all_2014_deployment_data INNER JOIN fagelsundet_gulls_all_2014_gps_coastdist ON fagelsundet_all_2014_deployment_data.device_info_serial = fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial) INNER JOIN fagelsundet_gulls_all_2014_gps_trip_id_par ON (fagelsundet_gulls_all_2014_gps_coastdist.date_time = fagelsundet_gulls_all_2014_gps_trip_id_par.date_time) AND (fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial = fagelsundet_gulls_all_2014_gps_trip_id_par.device_info_serial);"
                       , as.is = TRUE)

# Re-cast some of the variables

gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))

gps.points$on_land <- as.logical(gps.points$on_land)

gps.points$ring_number <- as.factor(gps.points$ring_number)

gps.points$species <- as.factor(gps.points$species)

gps.points$area_class <- as.factor(gps.points$area_class)

# 2. filter out colony points + outliers ------
levels(gps.points$area_class)

# Set start and end dates for data to include
date.s <-  as.POSIXct(strptime("2014-05-01 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC"))
date.e <-  as.POSIXct(strptime("2014-08-01 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC"))

# date.e <-  as.POSIXct(strptime("2014-06-01 00:00:00",
#                                format = "%Y-%m-%d %H:%M:%S",
#                                tz = "UTC"))


# Include certain date period only, and exclude points where
# time interval is greater than 20 minutes
f <- ((gps.points$area_class != "Colony") &
      (gps.points$date_time > date.s) &
      (gps.points$date_time < date.e) &
      (gps.points$time_interval < 1200) &
        (gps.points$species != "Hydroprogne caspia"))

summary(f)

points.trips <- gps.points[f,]



# 3. Lable time periods -----
# Find time intervals
time.per <- findInterval(points.trips$date_time,
                         c(as.POSIXct("2014-05-15 00:00", tz = "UTC"),
                           as.POSIXct("2014-06-01 00:00", tz = "UTC"),
                           as.POSIXct("2014-06-15 00:00", tz = "UTC"),
                           as.POSIXct("2014-07-01 00:00", tz = "UTC"),
                           as.POSIXct("2014-07-15 00:00", tz = "UTC")))

# summary(as.factor(time.per))

# Label time intervals
time.per[time.per == 0] <- NA
time.per[time.per == 1] <- "may2"
time.per[time.per == 2] <- "jun1"
time.per[time.per == 3] <- "jun2"
time.per[time.per == 4] <- "jul1"
time.per[time.per == 5] <- NA
time.per <- as.factor(time.per)

# View summary
summary(time.per)

# Check this looks correct
# test <- cbind.data.frame(time.per[c(1:10,10000:10010,30000:30010)],gps.points$date_time[c(1:10,10000:10010,30000:30010)] )

points.trips <- cbind.data.frame(points.trips, time.per)

str(points.trips)

summary(points.trips$ring_number)

# Only retain levels that have some data (drops CT terns for example)
points.trips <- droplevels(points.trips)

# 4. Make summaries (%) by habitat, species, period etc.  -------
# Use 'plyr' package
library(plyr)

df_ind_per <- ddply(points.trips, c("area_class", "time.per",
                             "ring_number"),
             function(x) colSums(x[c("time_interval")]),
             .drop = FALSE)
df_ind_per <- df_ind_per[ do.call(order, df_ind_per), ]


df_per <- ddply(points.trips, c("time.per", "ring_number"
                             ), function(x) colSums(x[c("time_interval")]),
             .drop = FALSE)
df_per <- df_per[ do.call(order, df_per), ]

# Test that data is in same order (same bird IDs)
all.equal(df_ind_per[1:100,3],df_per[,2])

# Calculate %
df_ind_per$percent <- 100 * df_ind_per$time_interval / df_per$time_interval


# When no data for period, replace percentage values with zero
df_ind_per$percent[df_per$time_interval == 0] <- 0

# Remove rows of NA categories
df_ind_per <- df_ind_per[!is.na(df_ind_per[,2]),]


# # 3. Summarise by species (time_interval ~ species + area_class) ----
# 
# 
# agg.sp.class <- aggregate(time_interval ~ species + area_class,
#                           data = points.trips,
#                           FUN = sum, na.rm = TRUE)
# 
# 
# 
# library(plyr)
# df2 <- ddply(points.trips, c("species", "area_class", "ring_number"), function(x) colSums(x[c("time_interval")]), .drop = FALSE)
# df2
# 
# 
# df3 <- ddply(df2, c("species", "area_class"), function(x) colSums(x[c("time_interval")]), .drop = FALSE)
# df3
# # ?ddply
# 
# 
# agg.sp <- aggregate(time_interval ~ species,
#                           data = points.trips,
#                           FUN = sum, na.rm = TRUE)
# 
# str(agg.sp.class)
# 
# agg.sp.class$perc <- 100*agg.sp.class$time_interval/agg.sp$time_interval
# 
# agg.sp.class
# 
# 
# # 3. Summarise by ring_number (time_interval ~ ring_number + area_class) ----
# 
# agg.bird.class <- aggregate(time_interval ~ ring_number + area_class,
#                           data = points.trips,
#                           FUN = length)
# ?aggregate
# 
# agg.bird <- aggregate(time_interval ~ ring_number,
#                     data = points.trips,
#                     FUN = sum, na.rm = TRUE)
# 
# str(agg.bird.class)
# 
# agg.bird.class$perc <- 100*agg.bird.class$time_interval/agg.bird$time_interval
# 
# agg.bird.class
# 
# x <- cbind.data.frame(points.trips$ring_number, points.trips$species)
# 
# x2 <- unique(x)
# names(x2) <- c("ring_number", "species")
# x2 <- x2[order(x2$ring_number),]
# all.equal(x2$ring_number, agg.bird.class$ring_number[1:27])
# 
# agg.bird.class$species <- x2$species



# ring + species key table -----

# Add species key
sp.ring.key <- gps.points[,c("ring_number","species")]
sp.ring.key <- unique(sp.ring.key)
# Remove CT terns
sp.ring.key <- sp.ring.key[sp.ring.key$species != "Hydroprogne caspia",]
sp.ring.key <- droplevels(sp.ring.key)





# Define plotting function ------
# Using code from http://stackoverflow.com/questions/12664820/add-count-and-labels-to-stacked-bar-plot-with-facet-wrap#
plot.fun <- function(x, title.text = "Proportion of time spent \n by area type",
                     no_legend = FALSE){
  library(ggplot2)
  library(scales)
  library(RColorBrewer)
  
  m <- melt(x)
  # names(m) <- c("Bird ID", "Area class", "Species", "variable", "")
  names(m)
  
  ar_lev <- levels(m$area_class)
  ar_lev <- ar_lev[c(2,3,1,4)]
  m$area_class <- factor(m$area_class, levels = ar_lev)
  
  
  levels(m$species)
  levels(m$species) <- c("L. argentatus",
                         "L. canus", "L. fuscus",
                         "L. marinus")
  
  
  
  a <- ggplot(m, aes(x = ring_number, y = value))
  
  if(no_legend == TRUE) {a <- a + theme(legend.position="none")}
  
  
  # pdf("time_activity_habitat.pdf")
  a +   facet_grid(~species, scales = "free", space = "free" ) +
    geom_bar(stat = "identity", aes(fill = area_class, order = area_class), position = "fill") +      
    scale_fill_manual(values = (brewer.pal(4, "Spectral")),
                      name = "Area type") + 
  #   coord_flip() + 
    scale_y_continuous("Percent", labels = percent) +
    ylab('Percent') +
    theme(axis.text.x = element_text(angle = 90, size = 10, hjust = 0.5, vjust = 0.5),
          axis.text = element_text(colour = 'black', size = 10),
          axis.title = element_text(face = "bold",
                                    colour = 'black', size = 14),
          strip.text.x = element_text(size = 10, face = "italic"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold")) +
    xlab("Bird ID") +
    ggtitle(title.text)
  # plot.all
  # dev.off()
}



# Plot figures for each time period -----

# Function wrap
plot.time.per.fun <- function(x.df, per = "may2", sp.ring.key2 = sp.ring.key,
                              no_legend = TRUE){
  # Choose time period, and drop now unused levels
  x <- x.df[(x.df$time.per == per),c(3,1,5)]
  x <- droplevels(x)
  
  # Add species category
  x <- join(x, sp.ring.key2)
  
  # Plot
  plot.fun(x, title.text = "", no_legend = no_legend)
}

png("time_area_may2.png")
plot.time.per.fun(x.df = df_ind_per, per = "may2")
dev.off()

png("time_area_jun1.png")
plot.time.per.fun(x.df = df_ind_per, per = "jun1")
dev.off()

png("time_area_jun2.png")
plot.time.per.fun(x.df = df_ind_per, per = "jun2")
dev.off()

png("time_area_jul1.png")
plot.time.per.fun(x.df = df_ind_per, per = "jul1")
dev.off()

