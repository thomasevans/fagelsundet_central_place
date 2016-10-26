# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)

# Get data
gps.points_raw <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude
                       FROM fagelsundet_gulls_all_2014_gps_points AS g
                       ORDER BY g.device_info_serial ASC, g.date_time ASC;"
                       ,as.is = TRUE)

gps.points_raw$date_time <-  as.POSIXct(strptime(gps.points_raw$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))


gps.points_details <- merge(gps.points,gps.points_raw, by =
                              c("device_info_serial", "date_time"))

# ?merge

# What is thing at 35 km in lbbg data?
lbbg.points.35 <- gps.points_details[(
  gps.points_details$species == "Larus fuscus" &
    gps.points_details$coast_dist_sign > 30000 &
    gps.points_details$coast_dist_sign < 40000 &
    gps.points_details$date_time < as.POSIXct("2014-06-10 00:00:00", tz = "UTC")
),]

hist(gps.points_details$date_time, breaks = "days")
hist(gps.points_details$coast_dist_sign/1000)

plot(lbbg.points.35$latitude~lbbg.points.35$longitude)

# Alpha channel ----
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

plot(lbbg.points.35$latitude~lbbg.points.35$longitude, col = addalpha("black", alpha = 0.05))

hist(lbbg.points.35$coast_dist_sign, breaks = 50)

plot(lbbg.points.35$latitude~lbbg.points.35$longitude, col = addalpha("black", alpha = 0.05),
     xlim = c(17.5,19.1),
     ylim = c(60.5,61.25))


levels(gps.points$species)
