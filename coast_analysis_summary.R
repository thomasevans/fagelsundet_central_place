

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
                       query = "SELECT DISTINCT fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial, fagelsundet_gulls_all_2014_gps_coastdist.date_time, fagelsundet_all_2014_deployment_data.ring_number, fagelsundet_all_2014_deployment_data.species, fagelsundet_gulls_all_2014_gps_coastdist.coast_dist_sign, fagelsundet_gulls_all_2014_gps_coastdist.on_land, fagelsundet_gulls_all_2014_gps_trip_id_par2.trip_id, fagelsundet_gulls_all_2014_gps_trip_id_par2.time_interval, fagelsundet_gulls_all_2014_gps_trip_id_par2.area_class
FROM (fagelsundet_all_2014_deployment_data INNER JOIN fagelsundet_gulls_all_2014_gps_coastdist ON fagelsundet_all_2014_deployment_data.device_info_serial = fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial) INNER JOIN fagelsundet_gulls_all_2014_gps_trip_id_par2 ON (fagelsundet_gulls_all_2014_gps_coastdist.date_time = fagelsundet_gulls_all_2014_gps_trip_id_par2.date_time) AND (fagelsundet_gulls_all_2014_gps_coastdist.device_info_serial = fagelsundet_gulls_all_2014_gps_trip_id_par2.device_info_serial);"
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




# ring + species key table -----

# Add species key
sp.ring.key <- gps.points[,c("ring_number","species")]
sp.ring.key <- unique(sp.ring.key)
# Remove CT terns
sp.ring.key <- sp.ring.key[sp.ring.key$species != "Hydroprogne caspia",]
sp.ring.key <- droplevels(sp.ring.key)


# Add species category
df_ind_per <- join(df_ind_per, sp.ring.key)


# Calculate mean by activity period for each species  -----
df.sp <- ddply(points.trips, c("area_class", "time.per", "species"
), function(x) colSums(x[c("time_interval")], na.rm = FALSE),
.drop = FALSE)

df.sp.time  <- ddply(points.trips, c("time.per", "species"
), function(x) colSums(x[c("time_interval")], na.rm = FALSE),
.drop = FALSE)


# Calculate %
df.sp$percent <- 100 * df.sp$time_interval / df.sp.time$time_interval


# When no data for period, replace percentage values with zero
df.sp$percent[df.sp$time_interval == 0] <- 0

# Remove rows of NA categories
df.sp <- df.sp[!is.na(df.sp[,2]),]


df.sp$ring_number <- "mean"


names(df.sp)
names(df_ind_per)

df.sp <- df.sp[,c(1,2,6,4,5,3)]

com.df <- rbind.data.frame(df_ind_per, df.sp)
#   ?rbind
levels(com.df$ring_number)

str(com.df)

# Define plotting function ------
# Using code from http://stackoverflow.com/questions/12664820/add-count-and-labels-to-stacked-bar-plot-with-facet-wrap#
plot.fun <- function(x, title.text = "Proportion of time spent \n by area type",
                     no_legend = FALSE,
                     col.pal = "Set1",
                     col.rev = FALSE,
                     text.rot = 0,
                     eq.size = FALSE){
  library(ggplot2)
  library(scales)
  library(RColorBrewer)
  
  m <- melt(x)
  # names(m) <- c("Bird ID", "Area class", "Species", "variable", "")
  names(m)
  
  ar_lev <- levels(m$area_class)
  ar_lev <- ar_lev[c(2,1,3)]
  m$area_class <- factor(m$area_class, levels = ar_lev)
  
  
  levels(m$species)
  levels(m$species) <- c("L. argentatus",
                         "L. canus", "L. fuscus",
                         "L. marinus")
  
  
  
  a <- ggplot(m, aes(x = ring_number, y = value))
  
  if(no_legend == TRUE) {a <- a + theme(legend.position="none")}
  
  col.fill <- (brewer.pal(3, col.pal))
  if(col.rev){col.fill <- rev(col.fill)}

  # pdf("time_activity_habitat.pdf")
 if(eq.size){
   a <- a +   facet_grid(~species, scales = "free", space = "free" )} else {
   a <- a +   facet_grid(~species, scales = "free")}
  
  a + geom_bar(stat = "identity", aes(fill = area_class, order = area_class), position = "fill") +     
#     coord_flip() +
    scale_fill_manual(values = col.fill,
                      name = "Area type") + 
  #   coord_flip() + 
    scale_y_continuous("Percent", labels = percent) +
    ylab('Percent') +
    theme(axis.text.x = element_text(angle = 90 , size = 10, hjust = 0.5, vjust = 0.5),
          axis.text = element_text(angle = text.rot, colour = 'black', size = 10),
#           axis.text.x = element_text(angle = 90 - text.rot, colour = 'black', size = 10),
          axis.title.y = element_text(angle = text.rot, face = "bold",
                                    colour = 'black', size = 14),
          axis.title.x = element_text(angle = 270 - text.rot, face = "bold",
                          colour = 'black', size = 14),        
          strip.text.x = element_text(angle = text.rot, size = 10, face = "italic"),
          legend.text = element_text(angle = text.rot, size = 10),
          legend.title = element_text(angle = text.rot, size = 10, face = "bold")) +
    xlab("Bird ID") +
    ggtitle(title.text)
  # plot.all
  # dev.off()
}



# Plot figures for each time period -----

# Function wrap
plot.time.per.fun <- function(x.df, per = "may2",
                              no_legend = TRUE){
  # Choose time period, and drop now unused levels
  x <- x.df[(x.df$time.per == per),c(3,1,5,6)]
  x <- droplevels(x)
#   levels(x$species)

  # Plot
  plot.fun(x, title.text = "", no_legend = no_legend)
}

png("time_area_may2.png")
plot.time.per.fun(x.df = com.df, per = "may2")
dev.off()

png("time_area_jun1.png")
plot.time.per.fun(x.df = com.df, per = "jun1")
dev.off()

png("time_area_jun2.png")
plot.time.per.fun(x.df = com.df, per = "jun2")
dev.off()

png("time_area_jul1.png")
plot.time.per.fun(x.df = com.df, per = "jul1")
dev.off()



hist(points.trips$coast_dist_sign/1000, breaks = 200)

hist(points.trips$coast_dist_sign/1000, breaks = 100,
     xlim = c(-50,50))


hist(points.trips$coast_dist_sign/1000, breaks = 20000,
     xlim = c(-5,5))


hist(points.trips$coast_dist_sign/1000, breaks = 20000,
     xlim = c(-1,2))


hg <- points.trips$species == "Larus argentatus"
gbbg <- points.trips$species == "Larus marinus"
cg <- points.trips$species == "Larus canus"
lbbg <- points.trips$species == "Larus fuscus"


par(mfrow=c(4,1))
hist(points.trips$coast_dist_sign[hg]/1000, breaks = 20000,
     xlim = c(-1,2))
hist(points.trips$coast_dist_sign[gbbg]/1000, breaks = 2000,
     xlim = c(-1,2))
hist(points.trips$coast_dist_sign[cg]/1000, breaks = 2000,
     xlim = c(-1,2))
hist(points.trips$coast_dist_sign[lbbg]/1000, breaks = 10000,
     xlim = c(-1,2))

# ?hist



par(mfrow=c(4,1))
hist(points.trips$coast_dist_sign[hg]/1000, breaks = 20000,
     xlim = c(-20,50))
hist(points.trips$coast_dist_sign[gbbg]/1000, breaks = 2000,
     xlim = c(-20,50))
hist(points.trips$coast_dist_sign[cg]/1000, breaks = 2000,
     xlim = c(-20,50))
hist(points.trips$coast_dist_sign[lbbg]/1000, breaks = 10000,
     xlim = c(-20,50))



x.lims <- c(-50,50)
par(mfrow=c(4,1))
hist(points.trips$coast_dist_sign[hg]/1000, breaks = 20000,
     xlim = x.lims, probability = TRUE)
# ?hist
hist(points.trips$coast_dist_sign[gbbg]/1000, breaks = 2000,
     xlim = x.lims, probability = TRUE)
hist(points.trips$coast_dist_sign[cg]/1000, breaks = 2000,
     xlim = x.lims, probability = TRUE)
hist(points.trips$coast_dist_sign[lbbg]/1000, breaks = 10000,
     xlim = x.lims, probability = TRUE)



# Histogram of distances by species -------
# These are weighted by time (hours - w)

library(ggplot2)

# Make dataframe of weights (time interval - hours), distances (km) and species
w <- points.trips$time_interval/60/60
v <- points.trips$coast_dist_sign/1000
s <- points.trips$species
foo <- data.frame(v, w, s)

# Basic plot
a <- ggplot(foo, aes(v, weight = w)) 

pdf("foraging_distance_hist.pdf")
png("foraging_distance_hist.png")
win.metafile("foraging_distance_hist.wmf")
# ?geom_histogram
# Plot figure
a + aes(y = ..density..) +
  # bidwidth = 0.5 is 0.5 km intervals
  geom_histogram(binwidth = .5) +
  xlim(-50, 60) +
  # scales = "free" allows different y-scales for each species
  # facet by 's', species
  facet_grid(s~. , scales = "free") +
  ylab('Proportion of foraging time spent in 0.5 km bins') +
  xlab('Distance from coastline (km)') +
  # Set some aesthetic paramaters
  theme(axis.text.x = element_text(angle = 0, size = 10, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(colour = 'black', size = 10),
        axis.title = element_text(face = "bold",
                                  colour = 'black', size = 14),
        strip.text.x = element_text(size = 10, face = "italic"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"))
dev.off()
# getwd()




# Time spent in areas - activity budget - overall species -----

# Use 'plyr' package
library(plyr)

df_ind_area <- ddply(points.trips, c("area_class",
                                    "ring_number"),
                    function(x) colSums(x[c("time_interval")]),
                    .drop = FALSE)
df_ind_area <- df_ind_area[ do.call(order, df_ind_area), ]


df_ind <- ddply(points.trips, c("ring_number"
), function(x) colSums(x[c("time_interval")]),
.drop = FALSE)
df_ind <- df_ind[ do.call(order, df_ind), ]

# Test that data is in same order (same bird IDs)
all.equal(df_ind_area[1:100,3], df_ind[,2])

# Calculate %
df_ind_area$percent <- 100 * df_ind_area$time_interval / df_ind$time_interval


# When no data for period, replace percentage values with zero
df_ind_area$percent[df_ind$time_interval == 0] <- 0

# Remove rows of NA categories
df_ind_area <- df_ind_area[!is.na(df_ind_area[,2]),]


# Then add species
# Add species category
df_ind_area <- join(df_ind_area, sp.ring.key)




# Prepare DF for plotting
# Remove rows of NA categories
df_ind_area <- df_ind_area[!is.na(df_ind_area[,2]),]



levels(df_ind_area$area_class)
levels(df_ind_area$area_class) <- c("Coast", "Land", "Sea")


# Plot figure of activity budget (whole period) ----
# Plot
win.metafile("activity_plot_col.wmf")
plot.fun(df_ind_area, title.text = "", no_legend = TRUE,
         col.pal = "RdYlBu", col.rev = FALSE,
         text.rot = 90,
         eq.size = TRUE)
# plot.fun(df_ind_area, title.text = "", no_legend = FALSE,
#          col.pal = "RdYlBu", col.rev = FALSE)
dev.off()

pdf("activity_plot.pdf")
png("activity_plot.png")
win.metafile("activity_plot.wmf")
plot.fun(df_ind_area, title.text = "", no_legend = TRUE,
         col.pal = "Greys", col.rev = FALSE,
         text.rot = 90,
         eq.size = FALSE)
dev.off()
