#  Calculate distances to a few important features -----


# Load in GPS points data -------
library("RODBC")

# Load in GPS point data (lat, long, device_info_serial, date_time)

# Connect to DB
# To link to database

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)

# Get data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude
                       FROM fagelsundet_gulls_all_2014_gps_points AS g
                       ORDER BY g.device_info_serial ASC, g.date_time ASC;"
                       ,as.is = TRUE)


# Drop NA rows
drop <- is.na(gps.points$longitude)

gps.points <- gps.points[!drop,]


# Locations of key sites -------

# Workout light-house location

# gps.points.light_house <- gps.points[gps.points$latitude >60.82 &
#                                        gps.points$latitude <60.90 &
#                                        gps.points$longitude >17.80 &
#                                        gps.points$longitude <18.15,]
# hist(gps.points.light_house$latitude)
# hist(gps.points.light_house$longitude)
# median(gps.points.light_house$latitude)
# 
# # Get mode
# mode.fun <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# # lat
# mode.fun(signif(gps.points.light_house$latitude,6))
# # Long
# mode.fun(signif(gps.points.light_house$longitude,6))
# Alternatively get this from Eniro map
# http://kartor.eniro.se/

# Landfill sites (centres)
# Light-house thing
sites <- c("landfill_gavle", "landfill_forsbacka", "landfill_falun",
           "landfill_tierp", "landfill_sala", "landfill_uppsala",
           "landfill_hogbytorp", "landfill_stockholm",
           "light_vastra_banken_kassunfyr",
           "light_finngrundet")
type <- c("landfill", "landfill", "landfill",
          "landfill", "landfill", "landfill",
          "landfill", "landfill",
          "light_house",
          "light_house")
lat <- c( 60.686575,  60.639238,  60.611447,
          60.290733,  59.920735,  59.929981,
          59.550136,  59.172017,
          60.8781,
          60.98436)
long <- c( 17.155740,  16.879813,  15.574975,
           17.574914,  16.722996,  17.770531,
           17.615439,  17.989668,
           17.9184,
           18.605282)

site.df <- cbind.data.frame(sites,type,lat,long)


# Calculate distance to sites
# Define function to calculate distance for each point
source("deg.dist.r")

site.dist <- function(long, lat){
  t(deg.dist(long, lat,
         site.df$long, site.df$lat))
}

# Fine distances to sites
site_distances_all <- t(mapply(site.dist, long = gps.points$longitude,
       lat = gps.points$latitude))


# Fast! Using tip from http://stackoverflow.com/questions/6338517/equivalent-to-rowmeans-for-min
landfill_dist_min <- do.call(pmin, as.data.frame(site_distances_all[,c(1:8)]))
light_dist_min <- do.call(pmin, as.data.frame(site_distances_all[,c(9:10)]))

# Which is closest landfill?
landfill_closest <- sites[apply( as.data.frame(site_distances_all[,c(1:8)]), 1, which.min)]
summary(as.factor(landfill_closest))

# Which is closest light-house?
light_closest <- sites[9:10][apply( as.data.frame(site_distances_all[,c(9:10)]), 1, which.min)]
summary(as.factor(light_closest))


# Combine above into single data.frame ------
export.table <- cbind.data.frame(gps.points[,c(1,2)], landfill_dist_min,
                                 light_dist_min, landfill_closest,
                                 light_closest)

hist(export.table$landfill_dist_min, breaks = 1000, xlim = c(0,100))
hist(export.table$light_dist_min, breaks = 1000, xlim = c(0,100))

# 4. Output to DB ------
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')
str(export.table)

# Fix date-time
export.table$date_time <-  as.POSIXct(strptime(export.table$date_time,
                                          format = "%Y-%m-%d %H:%M:%S",
                                          tz = "UTC"))

#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, export.table, tablename = "fagelsundet_gulls_all_2014_gps_sitedist",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date")
)
# ?sqlSave

close(gps.db)

# export.table[608:610,]

write.csv(export.table, file = "site_distances.csv",
          row.names = FALSE)
