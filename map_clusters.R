
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


# 8 Colours from:
# http://colorbrewer2.org/?type=qualitative&scheme=Set1&n=8
myColors <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf")

# 4 Colours for species:
# From: mkweb.bcgsc.ca
spColors <- c("#0072b2",  "#d55e00",
              "#009e73", "#cc79a7")
# Blue, Bluish green, Vermillion, Reddish purple
# RGB:
# Orange: 230,159,0
# Sky blue: 86, 180, 233
# Vermillion 213, 94, 0
# Reddish purple 204, 121, 167


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
load("cluster_data_detailed.RData")
clust.out <- clust.tab


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




# 4. Proportions -------
tab.k.sp <- table(clust.out[,c("species","memb")])

# 
# Numbers
tab.k.sp
tab.k.sp[1,2]

# Proportions
#Within species
prop.table(tab.k.sp, 1)
#WIthin clusters
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


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

col.outs <- gg_color_hue(8)





# 6. Plot sepperate panels for each cluster -----
clust.out$cluster <- clust.out$memb
clusts <- sort(unique(clust.out$cluster))



svg("cluster_maps_grid_detailed_test_alpha.svg", width = 12, height = 12)

png("cluster_maps_grid_detailed_test_alpha.png", width = 12, height = 12, units = "in",
    res = 600)


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
             title.text = paste("cluster: ", clusts[i]),
             col.out = col.outs[i])



# Map each trip in turn
# ix <- 1
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
           # col = "black", lty = 1, lwd = 0.4)
           col = addalpha("black", 0.2), lty = 1, lwd = 0.4)
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





# 7. Species map -----
svg("species_maps_grid_detailed.svg", width = 8, height = 8)

png("species_maps_grid_detailed_alpha_thicker_lines.png", width = 8, height = 8, units = "in",
    res = 600)
# ?png
par(mar=c(2, 2, 0.5, 0.5) + 0.1)
par(mfrow=c(1,1))
par(mar=c(2, 2, 0.5, 0.5) + 0.1)

  
  # clust.num <- i
  # cluster.trips <- clust.out$trip_id[clust.out$cluster == clusts[i]]
  points.f <- dplyr::filter(gps.points, trip_id %in% clust.out$trip_id)
  
  map.base.fun(xlim = range(points.f$longitude),
               ylim = range(points.f$latitude),
               title.text = "",
               col.out = "black")
  
  
  # sp.col.alpha <- sapply(X = spColors, alpha = 0.15, FUN = addalpha)
  
  sp.col.alpha <- addalpha(spColors, 0.3)

  sp <- sort(unique(clust.out$species))
  
         
  
  clust.out <- clust.out[sample(c(1:nrow(clust.out))),]
  # ?sample
  # Map each trip in turn
  # ix <- 1
  for(ix in 1:length(clust.out$trip_id)){
    # for(ix in 1:100){
      
    # cluster.trips[i] %in% gps.points.ltraj.300.df$trip_id
    
    gps.sub <- filter(points.f, trip_id == clust.out$trip_id[ix])
    
    # sp.col <- addalpha(col.outs[specs == clust.out$species[ix]
    # ], alpha = 0.15)
    
    # x <- cluster.trips[i]
    # ?subset
    # ?filter
    n <- length(gps.sub$long)
    segments(gps.sub$long[-1], gps.sub$lat[-1],
             gps.sub$long[1:n-1], gps.sub$lat[1:n-1],
             # col = "black", lty = 1, lwd = 0.4)
             col = sp.col.alpha[clust.out$species[ix] == sp],
             lty = 1, lwd = 0.6)
    # col = sp.col, lty = 1, lwd = 1)
    
    
    
  }
  
  
  
  # Add colony location
  points(17.93, 60.63, pch = 21,
         col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.7),
         cex = 2)
  
  
  # Add landfill locations
  lat <- c( 60.686575,  60.639238,  60.611447,
            60.290733,  59.920735,  59.929981,
            59.550136,  59.172017)
  long <- c( 17.155740,  16.879813,  15.574975,
             17.574914,  16.722996,  17.770531,
             17.615439,  17.989668)
  points(long, lat, pch = 23,
         col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.6),
         cex = 2)
  
  # Add map scale bar
  map.scale2(ratio = FALSE, lwd.line = 2,
             relwidth = 0.25, cex = 1.2)
  

  
legend(x = "topleft",
       legend = c("Colony",
                  "Landfill",
                  expression(italic(L.~argentatus)~~-~HG),
                  expression(italic(L.~canus)~~-~CG),
                  expression(italic(L.~fuscus)~~-~LBBG),
                  expression(italic(L.~marinus)~~-~GBBG)),
       col = c("#7570b3", "#7570b3",
               spColors),
       pt.bg = c(addalpha("#7570b3", alpha = 0.4), addalpha("#7570b3", alpha = 0.4),
                 NA, NA, NA, NA),
       lwd = c(1, 1, 3, 3, 3, 3),
       lty = c(NA, NA, 1, 1, 1, 1),
       pch = c(21, 23, NA, NA, NA, NA),
       pt.cex = 2,
       bty = "n",
       cex = 1.3)

dev.off()
