
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


# Colour by clusters
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Get 7 colours
col.7 <- gg_color_hue(7)

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
tab.k.sp <- table(clust.out[,c("species","cluster")])

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
  par( mar = c(1.5, 2, .5, .5))
  
  plot(gadm_clip, xlim = xlim,
       ylim = ylim, col= "dark grey", bg = NA,
       # main = title.text,
       main = "",
       lty = 0)
 
  plot(water.lines,
       add = TRUE, col="light blue" )
  plot(water.lines2,
       add = TRUE, col="light blue" )
  plot(water.polygons,
       add = TRUE, col="light blue", border = NA)
  plot(water.polygons2,
       add = TRUE, col="light blue", border = NA)
  
  axis(side=(1),las=1, cex.lab = 0.5, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
  axis(side=(2),las=1, cex.lab = 0.5, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)
  
  # ?axis 
  
  ## Scale bar and axis
  box(lwd=3, col = col.out)
 
  
}

# 
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# 
# col.outs <- gg_color_hue(8)
col.outs <- col.7

# col.outs == myColors


# 6. Plot sepperate panels for each cluster -----
# clust.out$cluster <- clust.out$memb
clusts <- sort(unique(clust.out$cluster))



svg("cluster_maps_grid_detailed_test_alpha.svg", width = 12, height = 12)

png("cluster_maps_grid_detailed_test_alpha.png", width = 12, height = 12, units = "in",
    res = 600)


par(mfrow=c(3,3))

# Plot base map
i <- 2
# 


par(mfrow=c(1,1))


# plot(c(1:4), col = addalpha(spColors, 0.6))


sp.col.alpha <- addalpha(spColors, 0.25)
sp <- sort(unique(clust.out$species))
sp.col.alpha[clust.out$species[ix] == sp]

# i <- 3
for(i in 1:length(clusts)){
#   

  png(paste("cluster_maps_grid_detailed_species_colour2_clust_", clusts[i], ".png", sep = ""), width = 3, height = 3, units = "in",
      res = 600)
#   

# clust.num <- i
cluster.trips <- clust.out$trip_id[clust.out$cluster == clusts[i]]
cluster.sp <- clust.out$species[clust.out$cluster == clusts[i]]
points.f <- dplyr::filter(gps.points, trip_id %in% cluster.trips)

map.base.fun(xlim = range(points.f$longitude),
             ylim = range(points.f$latitude),
             title.text = paste("Cluster: ", clusts[i]),
             col.out = col.outs[i])

# x <- sort(points.f$latitude)
# head(x)
# plot(x)
# tail(x)

# plot(c(1:7),c(1:7), col = col.outs, pch = 20)

# Randomize order that trips are drawn
trip.df <- cbind.data.frame(cluster.trips, cluster.sp)

trip.df <- trip.df[sample(c(1:nrow(trip.df)), nrow(trip.df)),]

# Map each trip in turn
# ix <- 1
for(ix in 1:length(cluster.trips)){
  # cluster.trips[i] %in% gps.points.ltraj.300.df$trip_id
  
  
  
  
  gps.sub <- filter(points.f, trip_id == trip.df$cluster.trips[ix])
  
  # sp.col <- addalpha(col.outs[specs == clust.out$species[ix]
                              # ], alpha = 0.15)
  
  # x <- cluster.trips[i]
  # ?subset
  # ?filter
  n <- length(gps.sub$long)
  segments(gps.sub$long[-1], gps.sub$lat[-1],
           gps.sub$long[1:n-1], gps.sub$lat[1:n-1],

           col = sp.col.alpha[trip.df$cluster.sp[ix] == sp], lty = 1, lwd = 0.8)
          # col = addalpha("black", 0.12), lty = 1, lwd = 0.8)



}



# Add colony location
points(17.93, 60.63, pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5)


# Add landfill locations
lat <- c( 60.686575,  60.639238,  60.611447,
          60.290733,  59.920735,  59.929981,
          59.550136,  59.172017)
long <- c( 17.155740,  16.879813,  15.574975,
           17.574914,  16.722996,  17.770531,
           17.615439,  17.989668)
points(long, lat, pch = 21,
       col = "black", bg = addalpha("white", alpha = 0.4),
       cex = 1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.25, cex = 0.5)


dev.off()
}

dev.off()


# Legend ----

png("map_clust_legend.png", width = 3, height = 3, units = "in",
    res = 600)
par( mar = c(0, 0, 0, 0))

plot(1:10,1:10,type = "n",
     bty = NA)



legend(x = "bottomleft",
       legend = c(expression(italic(L.~argentatus)~~-~HG),
                  expression(italic(L.~canus)~~-~CG),
                  expression(italic(L.~fuscus)~~-~LBBG),
                  expression(italic(L.~marinus)~~-~GBBG)),
       col = c(spColors),
       pt.bg = c(NA, NA, NA, NA),
       lwd = c(8, 8, 8, 8),
       lty = c(1, 1, 1, 1),
       pch = c(NA, NA, NA, NA),
       pt.cex = 2,
       bty = "n",
       cex = 1.2)

legend(x = "topright",
       legend = c("Colony",
                  "Landfill",
                  "Land",
                  "Water (inland)",
                  "Sea",
                  "GPS track"),
       col = c("black", "black",
               "light grey", "light grey", "light grey"),
       pt.bg = c("white", "white", "dark grey",
                 "light blue", "white",
                 "black"),
       lwd = c(rep(NA,5), 2),
       lty = c(rep(NA,5), 1),
       pch = c(23, 21, 22, 22, 22, NA),
       pt.cex = c(2,2, 2.5, 2.5, 2.5, NA),
       bty = "n",
       cex = 1.2)

dev.off()


# Legend for PCA plot:


png("pca_clust_legend.png", width = 1, height = 3, units = "in",
    res = 600)
svg("pca_clust_legend.svg", width = 1, height = 3)
# ?svg
par( mar = c(0, 0, 0, 0))
# ?plot
plot(1:10,1:10,type = "n",
     bty =  "n")
# ?legend

legend(title = "Cluster",
       x = "topright",
       legend = c(1:7),
       col = "dark grey",
       pt.bg = col.7,
       lwd = 1,
       lty = NA,
       pch = 21,
       pt.cex = 3.5,
       bty = "n",
       cex = 1.5)

dev.off()

#   



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

png("species_maps_grid_detailed_alpha_thicker_lines_new.png", width = 8, height = 8, units = "in",
    res = 600)
# ?png
# par(mar=c(2, 2, 0.5, 0.5) + 0.1)
par(mfrow=c(1,1))
par(mar=c(3, 3, 0.5, 0.5) + 0.1)

  
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
             # col = spColors[clust.out$species[ix] == sp],
             col = sp.col.alpha[clust.out$species[ix] == sp],
             lty = 1, lwd = 1)
    # col = sp.col, lty = 1, lwd = 1)
    
    
    
  }
  
  
  
  # Add colony location
  points(17.93, 60.63, pch = 23,
         # col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.7),
         col = "black", bg = addalpha("white", alpha = 0.5),
         lwd = 2,
         cex = 2)
  
  
  # Add landfill locations
  lat <- c( 60.686575,  60.639238,  60.611447,
            60.290733,  59.920735,  59.929981,
            59.550136,  59.172017)
  long <- c( 17.155740,  16.879813,  15.574975,
             17.574914,  16.722996,  17.770531,
             17.615439,  17.989668)
  points(long, lat, pch = 21,
         # col = "#7570b3", bg = addalpha("#7570b3", alpha = 0.6),
         col = "black", bg = addalpha("white", alpha = 0.5),
         lwd = 2,
         cex = 2)
  
  # Add map scale bar
  map.scale2(ratio = FALSE, lwd.line = 2,
             relwidth = 0.25, cex = 1.2)
  

  
legend(x = "topleft",
       legend = c(expression(italic(L.~argentatus)~~-~HG),
                  expression(italic(L.~canus)~~-~CG),
                  expression(italic(L.~fuscus)~~-~LBBG),
                  expression(italic(L.~marinus)~~-~GBBG)),
       col = c(spColors),
       pt.bg = c(NA, NA, NA, NA),
       lwd = c(3, 3, 3, 3),
       lty = c(1, 1, 1, 1),
       pch = c(NA, NA, NA, NA),
       pt.cex = 2,
       bty = "n",
       cex = 1.3)

legend(x = "bottomright",
       legend = c("Colony",
                  "Landfill",
                  "Land",
                  "Water (inland)",
                  "Sea"),
       col = c("black", "black",
               "light grey", "light grey", "light grey"),
       pt.bg = c("white", "white", "dark grey", "light blue", "white"),
       lwd = NA,
       lty = NA,
       pch = c(23, 21, 22, 22, 22),
       pt.cex = c(2,2, 2.5, 2.5, 2.5),
       bty = "n",
       cex = 1.3)

dev.off()
