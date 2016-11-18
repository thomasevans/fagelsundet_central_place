# PCA/ cluster analysis of foraging trip types of gulls @Fågelsundet


# 1. Required packages ------

# Loading in Access DB data
library("RODBC")

# Handling data
library("dplyr")

# Plotting data
library("ggplot2")
library(scales)
library(cowplot)

# More plotting etc
# library("maptools")
# library("reshape2")

# # PCA analysis
# library("FactoMineR")
# library("factoextra")

# Themes etc for ggplot figures
theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12, face = "italic"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        # legend.text = element_text(colour="blue", size = 16, face = "bold")
        legend.key.width = unit(1, "lines"),
        legend.title = element_blank()
  )
myColors <- c("#66c2a5", "#fc8d62", "#8da0cb", "grey40")


# 2. Load in trip details ------


# Connect to DB
# To link to database

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get data
trip.details <- sqlQuery(gps.db,
                       query =
                         "SELECT fagelsundet_gulls_all_2014_trips_info.*
FROM fagelsundet_gulls_all_2014_trips_info
                       ORDER BY fagelsundet_gulls_all_2014_trips_info.ring_number, fagelsundet_gulls_all_2014_trips_info.trip_id;
                       "
                       ,as.is = TRUE)

str(trip.details)

trip.details$start_time <-  as.POSIXct(strptime(trip.details$start_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
trip.details$end_time <-  as.POSIXct(strptime(trip.details$end_time,
                                                format = "%Y-%m-%d %H:%M:%S",
                                                tz = "UTC"))
trip.details$sunrise <-  as.POSIXct(strptime(trip.details$sunrise,
                                              format = "%Y-%m-%d %H:%M:%S",
                                              tz = "UTC"))
trip.details$sunset <-  as.POSIXct(strptime(trip.details$sunset,
                                              format = "%Y-%m-%d %H:%M:%S",
                                              tz = "UTC"))

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


# 3. Include data from study period only -----

# get julian day for each point
trip.details$j_day <- format(trip.details$start_time, "%j")

j_days <- unique(gps.points$j_day)

# J day range
# 25th May - 5th July 2014
s_day <- as.POSIXct("2014-05-25 12:00:00", tz = "utc")
e_day <- as.POSIXct("2014-07-05 12:00:00", tz = "utc")
s_day_j <- format(s_day, "%j")
e_day_j <- format(e_day, "%j")
j_days_include <- c(s_day_j:e_day_j)


# Only retain data from study period
trip.details <- filter(trip.details, j_day %in%  j_days_include)



# 4. filtering criteria - quality criteria ------


# Look at the higher resolution data first to determine minimum permissable
# data resolution, data gaps etc.
# 10 minutes max gap
trip.sub <- filter(trip.details, gps_time_interval_max < 600)

# What proportion of trips are retained?
summary(as.factor(trip.sub$species))/summary(as.factor(trip.details$species))

# Also remove those that start/ end far from colony (thus likely truncated)
trip.sub <- filter(trip.sub, col_dist_start < 1000 & col_dist_end < 1000)
summary(as.factor(trip.sub$species))/summary(as.factor(trip.details$species))
summary(as.factor(trip.sub$species))

nrow(trip.sub)/nrow(trip.details)


# 5. filter - trip-range -----

# trip.sub$coldist_max

# Trip_duration distributions 
range.100km <- ggplot(trip.sub, aes(coldist_max/1000, colour = species)) +
  geom_density(adjust = 1, lwd = 1, alpha = 0.7,
               trim = TRUE) +
  xlim(0,100)+
  guides(colour = "none") +
  
  # ylim(0,0.2)+
  #   geom_vline(xintercept = 1, lwd = 1.5, lty = 2,
  #              col = "black",
  #              alpha = 0.5) +
  theme_new +
  labs(list(x = "",
            y = "Density",
            title = "Trip range <100 km",
            colour = "Species")) +
  scale_color_manual(values=myColors)
# ?geom_density
# summary(trip.sub$duration_s/60/60 <0.5)

range.10km <- ggplot(filter(trip.sub, coldist_max < 10000), aes(coldist_max/1000, colour = species)) +
  geom_density(adjust = 1, lwd = 1, alpha = 0.7,
               trim = TRUE) +
  guides(colour = "none") +
  
  # xlim(0,100)+
  # ylim(0,0.2)+
  #   geom_vline(xintercept = 1, lwd = 1.5, lty = 2,
  #              col = "black",
  #              alpha = 0.5) +
  theme_new +
  labs(list(x = "Trip range (km)",
            y = "Density",
            title = "Trip range <10 km",
            colour = "Species"))  +
  scale_color_manual(values=myColors)

range.6km <- ggplot(filter(trip.sub, coldist_max < 5000), aes(coldist_max/1000, colour = species)) +
  geom_density(adjust = 2/3, lwd = 1, alpha = 0.7,
               trim = TRUE) +
  # xlim(0,100)+
  # ylim(0,0.2)+
    geom_vline(xintercept = 2, lwd = 1.5, lty = 2,
               col = "red",
               alpha = 0.5) +
  theme_new +
  labs(list(x = "",
            y = "Density",
            title = "Trip range <6 km",
            colour = "Species"))  +
  scale_color_manual(values=myColors)

plot_grid(range.100km, range.6km, ncol = 2)

ggsave(filename = "trip_trip_ranges.svg", width = 8, height = 4,
       units = "in")
ggsave(filename = "trip_trip_ranges.png", width = 8, height = 4,
       units = "in")
ggsave(filename = "trip_trip_ranges.pdf", width = 8, height = 4,
       units = "in")



# 6. filter - quality colony distance -----
trip.sub <- filter(trip.details, coldist_max > 2000)
trip.sub2 <- filter(trip.details, coldist_max > 2000 &
                     col_dist_start < 3000 &
                     col_dist_end < 3000 )
hist(trip.sub$col_dist_end[trip.sub$col_dist_end < 10000])
hist(trip.sub$col_dist_start[trip.sub$col_dist_start < 10000])

summary(as.factor(trip.sub$species))
summary(as.factor(trip.sub2$species))


hist(trip.sub2$gps_time_interval_max_ex1[trip.sub2$gps_time_interval_max_ex1 <3600],
     breaks = 100)
trip.sub3 <- filter(trip.details, coldist_max > 2000 &
                      col_dist_start < 3000 &
                      col_dist_end < 3000 &
                      gps_time_interval_max_ex1 < 2000)
summary(as.factor(trip.sub3$species))

969/1301
832/969




# 7. Filter - maximum duration -------
hist(trip.sub3$duration_s/60/60/24, breaks = 100)

trip.sub3_long <- filter(trip.sub3, duration_s/60/60/24 > 2)



ggplot(trip.sub3, aes(duration_s/60/60/24, colour = species)) +
  geom_density(adjust = 1, lwd = 1, alpha = 0.5,
               trim = TRUE) +
  # guides(colour = "none") +
  geom_rug()+
  # xlim(0,100)+
  # ylim(0,0.2)+
    geom_vline(xintercept = 2, lwd = 1.5, lty = 2,
               col = "red",
               alpha = 0.5) +
  # coord_trans(x="log") +
  theme_new +
  labs(list(x = "Trip duration (days)",
            y = "Density",
            title = "",
            colour = "Species"))  +
  scale_color_manual(values=myColors)

ggsave(filename = "trip_duration_max.svg", width = 8, height = 4,
       units = "in")
ggsave(filename = "trip_duration_max.png", width = 8, height = 4,
       units = "in")
ggsave(filename = "trip_duration_max.pdf", width = 8, height = 4,
       units = "in")


# 

# Cite Camphuysen (thesis chapter on long trips) for long duration trips occuring rarely also during breeding
# Maybe set relatively high cut-off (2 days??)


# 8. Number of locations ------
trips.sub4 <- filter(trip.sub3, duration_s/60/60/24 < 2)

hist(trips.sub4$n_points)
sort(trips.sub4$n_points)

trips.fewpoints <- filter(trips.sub4, n_points < 10)




# # 9. PCA analysis ------
# # Use trips.sub5
# trips.sub5 <- filter(trips.sub4, n_points >= 10)
# 
# # See number of trips per species
# summary(as.factor(trips.sub5$species))
# 
# 
# trips.filtered <- filter()
# 
# # Use FactoMineR and factoextra for visualisation
# # install.packages(c("factoextra", "FactoMineR"))
# # See: http://www.sthda.com/english/wiki/print.php?id=225
# 
# names(trips.sub5)
# 
# trips.sub5$p_land_excl <- trips.sub5$p_land - trips.sub5$p_landfill
# sort(trips.sub5$p_land_excl)
# x <- filter(trips.sub5, p_land_excl <0)
# 
# # Fix one trip where landfill site was on coast
# trips.sub5$p_coast[trips.sub5$trip_id == "1950"] <- trips.sub5$p_coast[trips.sub5$trip_id == "1950"] - trips.sub5$p_landfill[trips.sub5$trip_id == "1950"]
# trips.sub5$p_land_excl[trips.sub5$trip_id == "1950"] <- trips.sub5$p_land[trips.sub5$trip_id == "1950"]
# 
# 
# # Time before/ after sunrise
# # cos(0)
# # cos(1*pi)
# trips.sub5$sunrise.prox <- cos((trips.sub5$sunrise_after_h/12)*pi)
# 
# hist(trips.sub5$sunrise.prox)
# plot(trips.sub5$sunrise.prox~trips.sub5$sunrise_after_h)
# 
# # Time before/ after sunset
# # cos(0)
# # cos(1*pi)
# trips.sub5$sunset.prox <- cos((trips.sub5$sunset_after_h/12)*pi)
# 
# hist(trips.sub5$sunset.prox)
# plot(trips.sub5$sunset.prox~trips.sub5$sunset_after_h)
# 
# # Prepare df for PCA
# vars <- c("trip_id", "ring_number", "device_info_serial", "species",
#           "coldist_max", "col_dist_median", "duration_s", "p_flight",
#           "p_land_excl", "p_sea", "p_coast", "p_landfill",
#           "tortoisity",
#           "sunrise.prox", "sunset.prox"
#           )
#  # vars  %in% names(trips.sub5)
# pca.df_incl_trip_info <- dplyr::select(trips.sub5, one_of(vars))
# row.names(pca.df_incl_trip_info) <- trips.sub5$trip_id
# # # ??one_of
# # ?select
# pca.df_only <- dplyr::select(pca.df_incl_trip_info, -c(1:4))
# row.names(pca.df_only) <- trips.sub5$trip_id
# 
# 
# 
# # Hierachial clustering analysis ------
# # Following hybrid approach of PCA, hierachial clustering, and k-means
# # See: http://www.sthda.com/english/wiki/hcpc-hierarchical-clustering-on-principal-components-hybrid-approach-2-2-unsupervised-machine-learning
# 
# 
# # Scale the data
# # See: http://www.sthda.com/english/wiki/clarifying-distance-measures-unsupervised-machine-learning#distances-and-scaling
# # Scale to make variables more comparable
# df <- scale(pca.df_only)
# 
# # It looks like we don't need to do this, as PCA function by default scales variables anyway - 'scale.unit = TRUE'
# 
# # library("PerformanceAnalytics")
# # chart.Correlation(df, histogram=TRUE, pch=19)
# # 
# # cor.matrix <- cor(df, method = "pearson", use = "complete.obs")
# 
# # Compute principal component analysis
# res.pca <- PCA(df, ncp = 5, graph=FALSE)
# 
# # Barplot of eigenvalues
# barplot(res.pca$eig[,1], main = "Eigenvalues",
#         names.arg = paste("Dim", 1:nrow(res.pca$eig), sep = ""))
# abline(h=1, lwd = 1.5, lty = 2)
# 
# # View how variables correlate with PCA components
# # Can reduce number of variables displayed by limiting cosines
# plot(res.pca, choix = "var", axes = c(1, 2), lim.cos2.var = 0.4)
# plot(res.pca, choix = "var", axes = c(1, 3), lim.cos2.var = 0.4)
# plot(res.pca, choix = "var", axes = c(1, 4), lim.cos2.var = 0.4)
# plot(res.pca, choix = "var", axes = c(1, 5), lim.cos2.var = 0.4)
# 
# # 
# # print(res.pca)
# 
# dimdesc(res.pca, proba = 0.2)
# 
# # Percentage of information retained by each
# # dimensions
# fviz_eig(res.pca)
# 
# # Visualize variables
# fviz_pca_var(res.pca)
# 
# 
# # Visualize trips
# fviz_pca_ind(res.pca)
# 
# # View eigenvalues and cumulative variance explained
# get_eig(res.pca)
# # First 5 PCA dimensions explain 80.9% of variance and have eigenvalues >1
# 
# # Compute hierarchical clustering on the PCA results
# # Compute PCA with ncp = 5
# res.pca <- PCA(df, ncp = 5, graph = TRUE)
# ?PCA
# # ?PCA
# 
# # ?HCPC
# 
# # Compute HCPC (hierachial clustering principle components)
# res.hcpc <- HCPC(res.pca, graph = FALSE)
# # ?HCPC
# # Optimal number of clusters - 4
# res.hcpc$call$t$nb.clust
# 
# # res.hcpc$call$t$tree
# 
# # See what clusters each trip belongs to along with variables
# x <- res.hcpc$data.clust
# 
# # See what variables are important to different clusters and what defines clusters
# res.hcpc$desc.var
# 
# # See how clusters correspond to PCA axes
# res.hcpc$desc.axes
# 
# # See which trips are closest to cluster centres, the 'paradogm'
# res.hcpc$desc.ind
# 
# 
# # Calculate loading of different variables on PCA axes
# # See: http://factominer.free.fr/faq/index.html
# sweep(res.pca$var$coord,2,sqrt(res.pca$eig[1:ncol(res.pca$var$coord),1]),FUN="/")
# 
# 
# 
# #PCs + tree
# plot(res.hcpc, choice = "3D.map")
# 
# # Dendrogram
# plot(res.hcpc, choice ="tree", cex = 0.6)
# 
# # Factor map
# plot(res.hcpc, choice ="map", draw.tree = FALSE)
# 
# # Remove labels and add cluster centers
# plot(res.hcpc, choice ="map", draw.tree = FALSE,
#      ind.names = FALSE, centers.plot = TRUE)
# 
# 
# 
# fviz_cluster(res.hcpc)
# 
# 
# # K-means clustering
# km.res <- eclust(df, "kmeans", k = 4,
#                  nstart = 25, graph = FALSE)
# # k-means group number of each observation
# head(km.res$cluster, 15)
# 
# fviz_cluster(km.res,  frame.type = "norm", frame.level = 0.68)
# 
# # Visualize the silhouette of clusters
# fviz_silhouette(km.res)
# 
# sil <- km.res$silinfo$widths[, 1:3]
# # Objects with negative silhouette
# neg_sil_index <- which(sil[, 'sil_width'] < 0)
# sil[neg_sil_index, , drop = FALSE]
# 
# #
# 
# 
# # Alternative hybrid k-means thing approach
# # Compute hierarchical k-means clustering
# res.hk <-hkmeans(df, 4)
# 
# res.hk
# 
# fviz_dend(res.hk, cex = 0.6, rect = TRUE)
# 
# fviz_cluster(res.hk, frame.type = "norm", frame.level = 0.68)
# 
# #
# 
# 
# 
# 
# 
# 
# 
# 
# # Old -----
# # Perform PCA
# res.pca <- PCA(pca.df_only,  graph = FALSE)
# 
# # Extract eigenvalues/variances
# get_eig(res.pca)
# 
# # Visualize eigenvalues/variances
# fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3)+
#   theme_minimal()
# 
# # Extract the results for variables
# var <- get_pca_var(res.pca)
# var
# 
# # Coordinates of variables
# (var$coord)
# 
# 
# # Contribution of variables
# (var$contrib)
# 
# # Graph of variables: default plot
# fviz_pca_var(res.pca, col.var = "steelblue")
# 
# # Control variable colors using their contributions
# # Use gradient color
# fviz_pca_var(res.pca, col.var="contrib")+
#   scale_color_gradient2(low="white", mid="blue", 
#                         high="red", midpoint = 96) +
#   theme_minimal()
# 
# 
# # Variable contributions on axis 1
fviz_contrib(res.pca, choice="var", axes = 1 )+
  labs(title = "Contributions to Dim 1")
# 
# Variable contributions on axes 1 + 2
fviz_contrib(res.pca, choice="var", axes = 1:2)+
  labs(title = "Contributions to Dim 1+2")

# # Variable contributions on axes 1:4
# # par(mfrow=c(2,2))
# fviz_contrib(res.pca, choice="var", axes = 1)+
#   labs(title = "Contributions to Dim 1")
# 
# fviz_contrib(res.pca, choice="var", axes = 2)+
#   labs(title = "Contributions to Dim 2")
# 
# fviz_contrib(res.pca, choice="var", axes = 3)+
#   labs(title = "Contributions to Dim 3")
# 
# fviz_contrib(res.pca, choice="var", axes = 4)+
#   labs(title = "Contributions to Dim 4")
# 
# fviz_contrib(res.pca, choice="var", axes = 5)+
#   labs(title = "Contributions to Dim 5")
# 
# 
# # Extract the results for trips
trip.pca <- get_pca_ind(res.pca)
trip.pca
# 
# 
# # Coordinates of trips
# head(trip.pca$coord)
# 
# 
# fviz_pca_ind(res.pca, repel = TRUE, col.ind = "cos2")+
#   scale_color_gradient2(low="blue", mid="white",
#                         high="red", midpoint=0.6)+
#   theme_minimal()
# 
# 
# 
# # Color by groups: habillage=iris$Species
# ?fviz_pca_ind
# # Show points only: geom = "point"
# p <- fviz_pca_ind(res.pca, geom = "point",
#                   axes = c(1,2),
#                   habillage=pca.df_incl_trip_info$species, addEllipses=TRUE,
#                   ellipse.level= 0.5)+ theme_minimal() +
#                   scale_fill_manual(values=myColors) +
#                   scale_color_manual(values=myColors)
# #   
# print(p)
# 
# 
# p <- fviz_pca_ind(res.pca, geom = "point",
#                   axes = c(1,3),
#                   habillage=pca.df_incl_trip_info$species, addEllipses=TRUE,
#                   ellipse.level= 0.5)+ theme_minimal()
# print(p)
# 
# p <- fviz_pca_ind(res.pca, geom = "point",
#                   axes = c(1,4),
#                   habillage=pca.df_incl_trip_info$species, addEllipses=TRUE,
#                   ellipse.level= 0.5)+ theme_minimal()
# print(p)
# 
# p <- fviz_pca_ind(res.pca, geom = "point",
#                   axes = c(1,5),
#                   habillage=pca.df_incl_trip_info$species, addEllipses=TRUE,
#                   ellipse.level= 0.5)+ theme_minimal()
# print(p)
# 
# 
# 
# fviz_pca_biplot(res.pca,  label="var", habillage=pca.df_incl_trip_info$species,
#                 addEllipses=TRUE, ellipse.level=0.50) +
#   theme_minimal()+
#   scale_fill_manual(values=myColors) +
#   scale_color_manual(values=myColors)
# 
# 
# 
# # K-means clustering
# kmeans.thing <- fviz_nbclust(pca.df_only, kmeans, method = "gap_stat")
# 
# # Compute hierarchical clustering and cut into 4 clusters
# res <- hcut(pca.df_only, k = 6, stand = TRUE)
# # 
# # # Visualize
# par(mfrow=c(1,1))
# fviz_dend(res, rect = TRUE, cex = 0.1)
# 
# k.df <- cbind.data.frame(res$cluster, pca.df_incl_trip_info[,c(1:4)])
# # 
# tab.k.sp <- table(k.df[,c(1,5)])
# # 
# prop.table(tab.k.sp, 1)
# prop.table(tab.k.sp, 2)
