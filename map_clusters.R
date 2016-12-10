
# Map clusters


# 1. Required packages ------
# Loading in Access DB data
library("RODBC")

# Handling data
library("dplyr")
# library(plyr)

# # Plotting data
# library("ggplot2")
# library(scales)
# library(cowplot)

# More plotting etc
# library("maptools")
# library("reshape2")
# 
# # PCA analysis
# library("FactoMineR")
# library("factoextra")

# For logit transform
# library("car")

# Plot correlation matrix thing
# library("PerformanceAnalytics")

# Track interpolation
# library("adehabitatLT")


# Mapping
library(maps)
library(RColorBrewer)
library(raster)

myColors <- c("#66c2a5", "#fc8d62", "#8da0cb", "grey40")


# Alpha channel
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

red.alpha <- addalpha("red", alpha = 0.7)

blue.alpha <- addalpha("blue", alpha = 0.7)


# hack map.scale function
# map.scale2 <- map.scale
# fix(map.scale2)
source("map.scale2.R")

# 2. Load in data ------
# Coastline data
load("SWE_adm0.RData")

# Detailed coastline
# Load coast-line data
load("openstreetmap_coast_polygon.RData")


# Cluster data
load("clusters_newx.RData")

# Waterways data
# Load same data
load("waterways.Rdata")

# GPS point data
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

# Keep only those points that lie within 
gps.points <- filter(gps.points, trip_id %in% clust.out$trip_id)

# Remove one dodgy point
gps.points <- filter(gps.points, latitude <62.5)



# # 3. Resample data to 5 minute intervals for illustration -----
# # Treat each trip as a 'burst'
# gps.points.ltraj <- as.ltraj(gps.points[,4:3], gps.points$date_time,
#                                    gps.points$device_info_serial,
#                                    burst = gps.points$trip_id, typeII = TRUE)
# # plot(gps.points.ltraj[600])
# 
# 
# # Interpolate to 300 s for plotting
# gps.points.ltraj.300 <- redisltraj(gps.points.ltraj, 300, type = "time")
# 
# # Convert to data frame
# gps.points.ltraj.300.df <- ld(gps.points.ltraj.300)
# 
# # Check all trips are here!
# length(unique(gps.points.ltraj.300.df$burst))
# 
# # Merge info with cluster df
# names(gps.points.ltraj.300.df)[11:12] <- c("device_info_serial",
#                                            "trip_id")
# 
# str(gps.points.ltraj.300.df)
# 
# gps.points.ltraj.300.df$trip_id <- as.numeric(as.character(gps.points.ltraj.300.df$trip_id))
# 
# # If merging do this:
# # gps.points.all <- merge(gps.points.ltraj.300.df,
# #                         clust.out,
# #                         by = "trip_id")



# 4. Proportions -------
tab.k.sp <- table(clust.out[,c(9,5)])
# 
# Numbers
tab.k.sp
tab.k.sp[1,2]

# Proportions
#Within clusters
prop.table(tab.k.sp, 1)
#WIthin species
prop.table(tab.k.sp, 2)


# 5. Map data -----

# Map range
lat.range <- range(gps.points$latitude)
long.range <- range(gps.points$longitude)

lat.range.dif <- lat.range[2]-lat.range[1]
lat.range[1] <- lat.range[1] - (0.15*lat.range.dif)
lat.range[2] <- lat.range[2] + (0.15*lat.range.dif)

long.range.dif <- long.range[2]-long.range[1]
long.range[1] <- long.range[1] - (0.15*long.range.dif)
long.range[2] <- long.range[2] + (0.15*long.range.dif)

# Reduce file size by clipping only map area required 
# gadm_clip <- crop(gadm, extent(long.range[1],
#                                long.range[2],
#                                lat.range[1],
#                                lat.range[2]))

gadm_clip <- crop(openstreetmap_coast_polygon, extent(long.range[1],
                               long.range[2],
                               lat.range[1],
                               lat.range[2]))

map.base.fun <- function(xlim = c(17,18.3), ylim =  c(57,57.7),
                         title.text = "", col.out = "black"){
  # par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  
  plot(gadm_clip, xlim = xlim,
       ylim = ylim, col= "dark grey", bg = NA,
       main = title.text,
       lty = 0)
 
  plot(water.lines,
       add = TRUE, col="light blue" )
  plot(water.lines2,
       add = TRUE, col="light blue" )
  plot(water.polygons,
       add = TRUE, col="light blue", border = NA)
  plot(water.polygons2,
       add = TRUE, col="light blue", border = NA)
  
  axis(side=(1),las=1)
  axis(side=(2),las=1)
   
  ## Scale bar and axis
  box(lwd=3, col = col.out)
 
  
}

# i <- 1
# Map cluster
# Cluster Number
# ?pdf

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

col.outs <- gg_color_hue(4)

pdf("cluster_maps_ind_detailed_map2_col.pdf", width = 12, height = 8)
png("cluster_maps_grid_detailed_map2_col.png", width = 12, height = 8,
    units = "in", res = 300)
svg("cluster_maps_grid_detailed_map2.svg", width = 12, height = 8)

tiff("cluster_maps_grid_detailed_map2_col_test.png", width = 4, height = 4,
     units = "in", res = 300, compression = "lzw")

tiff("cluster_maps_grid_detailed_map2_col_2.tiff", width = 12, height = 8,
     units = "in", res = 600, compression = "lzw")

tiff("cluster_maps_grid_detailed_map2_all_species_scrambled.tiff", width = 8, height = 8,
     units = "in", res = 600, compression = "lzw")

clust.out.original <- clust.out

clust.out <- clust.out.original
clust.out <- clust.out.original[sample(795,795),]
# ?sample
# ?svg
# ?png
# i <- 1
specs <- sort(unique(clust.out$species))


par(mfrow=c(1,1))
# par(mfrow=c(2,3))

unique(clust.out$cluster)
# Plot base map
map.base.fun(xlim = range(gps.points$longitude),
             ylim = range(gps.points$latitude),
             title.text = "All trips - Species")
# 
# for(i in 1:4){
#   

plot(water.lines, col="blue", add = TRUE)
plot(water.polygons, add=TRUE, col = "blue")
  

# clust.num <- i
cluster.trips <- clust.out$trip_id
# points.f <- dplyr::filter(gps.points, trip_id %in% cluster.trips)

# Map each trip in turn
# ix <- 1
for(ix in 1:length(cluster.trips)){
  # cluster.trips[i] %in% gps.points.ltraj.300.df$trip_id
  
  gps.sub <- filter(gps.points, trip_id == clust.out$trip_id[ix])
  
  sp.col <- addalpha(col.outs[specs == clust.out$species[ix]
                              ], alpha = 0.15)
  
  # x <- cluster.trips[i]
  # ?subset
  # ?filter
  n <- length(gps.sub$long)
  segments(gps.sub$long[-1], gps.sub$lat[-1],
           gps.sub$long[1:n-1], gps.sub$lat[1:n-1],
           # col = "light grey", lty = 1, lwd = 0.4)
  col = sp.col, lty = 1, lwd = 1)

}

# }



# Add colony location
points(17.93, 60.63, pch = 21,
       col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.5),
       cex = 2)


# Add landfill locations
lat <- c( 60.686575,  60.639238,  60.611447,
          60.290733,  59.920735,  59.929981,
          59.550136,  59.172017)
long <- c( 17.155740,  16.879813,  15.574975,
           17.574914,  16.722996,  17.770531,
           17.615439,  17.989668)
points(long, lat, pch = 23,
       col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.4),
       cex = 2)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 2,
           relwidth = 0.25, cex = 1.2)

# ?legend
  legend(x = "topleft",
         legend = c("Colony",
                    "Landfill",
                    "L. argentatus/ HG",
                    "L. canutus/ CG",
                    "L. fuscus/ LBBG",
                    "L. marinus/ GBBG"),
         col = c("#7570b3", "#7570b3",
                 col.outs),
         pt.bg = c(addalpha("#7570b3", alpha = 0.4), addalpha("#7570b3", alpha = 0.4),
                   NA, NA, NA, NA),
         lwd = c(1, 1, 3, 3, 3, 3),
         lty = c(NA, NA, 1, 1, 1, 1),
         pch = c(21, 23, NA, NA, NA, NA),
         pt.cex = 2,
         bty = "n",
         cex = 1.3)



dev.off()




# Plot sepperate panels for each cluster -----

clusts <- sort(unique(clust.out$cluster))



svg("cluster_maps_grid_detailed_newx.svg", width = 12, height = 12)


# par(mfrow=c(1,1))
par(mfrow=c(3,3))

# Plot base map
# i <- 1
# 
for(i in 1:length(clusts)){
#   


# clust.num <- i
cluster.trips <- clust.out$trip_id[clust.out$cluster == clusts[i]]
points.f <- dplyr::filter(gps.points, trip_id %in% cluster.trips)

map.base.fun(xlim = range(points.f$longitude),
             ylim = range(points.f$latitude),
             title.text = paste("cluster: ", clusts[i]))



# Map each trip in turn
ix <- 1
for(ix in 1:length(cluster.trips)){
  # cluster.trips[i] %in% gps.points.ltraj.300.df$trip_id
  
  gps.sub <- filter(points.f, trip_id == cluster.trips[ix])
  
  # sp.col <- addalpha(col.outs[specs == clust.out$species[ix]
                              # ], alpha = 0.15)
  
  # x <- cluster.trips[i]
  # ?subset
  # ?filter
  n <- length(gps.sub$long)
  segments(gps.sub$long[-1], gps.sub$lat[-1],
           gps.sub$long[1:n-1], gps.sub$lat[1:n-1],
           col = addalpha("black", 0.5), lty = 1, lwd = 0.4)
           # col = sp.col, lty = 1, lwd = 1)
  


}



# Add colony location
points(17.93, 60.63, pch = 21,
       col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.5),
       cex = 2)


# Add landfill locations
lat <- c( 60.686575,  60.639238,  60.611447,
          60.290733,  59.920735,  59.929981,
          59.550136,  59.172017)
long <- c( 17.155740,  16.879813,  15.574975,
           17.574914,  16.722996,  17.770531,
           17.615439,  17.989668)
points(long, lat, pch = 23,
       col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.4),
       cex = 2)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 2,
           relwidth = 0.25, cex = 1.2)

}

dev.off()

# # ?legend
# legend(x = "topleft",
#        legend = c("Colony",
#                   "Landfill",
#                   "L. argentatus/ HG",
#                   "L. canutus/ CG",
#                   "L. fuscus/ LBBG",
#                   "L. marinus/ GBBG"),
#        col = c("#7570b3", "#7570b3",
#                col.outs),
#        pt.bg = c(addalpha("#7570b3", alpha = 0.4), addalpha("#7570b3", alpha = 0.4),
#                  NA, NA, NA, NA),
#        lwd = c(1, 1, 3, 3, 3, 3),
#        lty = c(NA, NA, 1, 1, 1, 1),
#        pch = c(21, 23, NA, NA, NA, NA),
#        pt.cex = 2,
#        bty = "n",
#        cex = 1.3)



