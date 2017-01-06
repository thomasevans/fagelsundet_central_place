# 1. Required packages ------

# Loading in Access DB data
library("RODBC")

# Handling data
# Should load plyr first to avoid conflicts with dplyr
library(plyr)
library("dplyr")

# Plotting data
library("ggplot2")
library(scales)
library(cowplot)

# More plotting etc
# library("maptools")
# library("reshape2")

# PCA analysis
library("FactoMineR")
library("factoextra")

# For logit transform
library("car")

# Plot correlation matrix thing
library("PerformanceAnalytics")

# To determine optimal number of clusters, and which clusters are statistically supported
install.packages("pvclust")
library("pvclust")
# vignette("pvclust")

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
trip.details$solarnoon <-  as.POSIXct(strptime(trip.details$solarnoon,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
trip.details$time_midpoint <-  as.POSIXct(strptime(trip.details$time_midpoint,
                                            format = "%Y-%m-%d %H:%M:%S",
                                            tz = "UTC"))


# 3. Include data from study period only -----

# get julian day for each point
trip.details$j_day <- format(trip.details$start_time, "%j")

j_days <- unique(trip.details$j_day)

# J day range
# 25th May - 5th July 2014
s_day <- as.POSIXct("2014-05-25 12:00:00", tz = "utc")
e_day <- as.POSIXct("2014-07-05 12:00:00", tz = "utc")
s_day_j <- format(s_day, "%j")
e_day_j <- format(e_day, "%j")
j_days_include <- c(s_day_j:e_day_j)


# test_date_time <- seq.POSIXt(from = as.POSIXct("2015-07-12 09:00"),
#                                    to =as.POSIXct("2015-07-12 15:00"), by =  "min",
#                              tz = "UTC")
# 
# start_time <- as.POSIXct("2015-07-12 10:50", tz = "utc")
# 
# # Get index of first row where date_time is more than start_time
# Position(function(x) x > start_time, test_date_time)


# Only retain data from study period
trip.details <- filter(trip.details, j_day %in%  j_days_include)

# 4. Filter data -------
trips.sub <- filter(trip.details, coldist_max > 2000&
                      col_dist_start < 3000 &
                      col_dist_end < 3000 &
                      gps_time_interval_max_ex1 < 2000 &
                      duration_s/60/60/24 < 2 &
                      n_points >= 10)


# 5. Calculated variables

# Time before/ after sunrise
trips.sub$sunrise.prox <- cos((trips.sub$sunrise_after_h/12)*pi)

hist(trips.sub$sunrise.prox)
plot(trips.sub$sunrise.prox~trips.sub$sunrise_after_h)

# Time before/ after sunset
trips.sub$sunset.prox <- cos((trips.sub$sunset_after_h/12)*pi)


# Time before/ after noon
trips.sub$solarnoon.prox <- cos((trips.sub$solarnoon_after_h/12)*pi)
hist(trips.sub$solarnoon.prox)


# Time before/ after sunrise
trips.sub$sunrise.prox.mid <- cos((trips.sub$sunrise_after_h_mid/12)*pi)

hist(trips.sub$sunrise.prox.mid)
# plot(trips.sub$sunrise.prox.mid~trips.sub$sunrise_after_h)

# Time before/ after sunset
trips.sub$sunset.prox.mid <- cos((trips.sub$sunset_after_h_mid/12)*pi)
hist(trips.sub$sunset.prox.mid)

# Time before/ after noon
trips.sub$solarnoon.prox.mid <- cos((trips.sub$solarnoon_after_h_mid/12)*pi)
hist(trips.sub$solarnoon.prox.mid)



# Transformations of proportion variables
trips.sub$p_flight_logit <- logit(trips.sub$p_flight)
hist(trips.sub$p_flight_logit)

trips.sub$p_land_logit <- logit(trips.sub$p_land)
hist(trips.sub$p_land_logit)

trips.sub$p_sea_logit <- logit(trips.sub$p_sea)
hist(trips.sub$p_sea_logit)

trips.sub$p_coast_logit <- logit(trips.sub$p_coast)
hist(trips.sub$p_coast_logit)

trips.sub$p_landfill_logit <- logit(trips.sub$p_landfill)
hist(trips.sub$p_landfill_logit)

trips.sub$p_water_20m_logit <- logit(trips.sub$p_water_20m)
hist(trips.sub$p_water_20m_logit)

trips.sub$p_water_50m_logit <- logit(trips.sub$p_water_50m)
hist(trips.sub$p_water_50m_logit)



# Transformations of other variables
# "coldist_max", "col_dist_median", "duration_s", "tortoisity"

trips.sub$log_coldist_max <- log10(trips.sub$coldist_max)
hist(trips.sub$log_coldist_max)

trips.sub$log_col_dist_median <- log10(trips.sub$col_dist_median)
hist(trips.sub$log_col_dist_median)

trips.sub$log_duration_s <- log10(trips.sub$duration_s)
hist(trips.sub$log_duration_s)

trips.sub$log_tortoisity <- log(trips.sub$tortoisity-0.999)
hist(trips.sub$log_tortoisity)

# # Time of day variables
# trips.sub$sunrise.prox_logit <- logit((trips.sub$sunrise.prox+1)/2)
# hist(trips.sub$sunrise.prox_logit)
# 
# trips.sub$sunset.prox_logit <- logit((trips.sub$sunset.prox+1)/2)
# hist(trips.sub$sunset.prox_logit)

# plot(trips.sub$sunset.prox_logit~trips.sub$sunset_after_h)
# plot(trips.sub$sunset.prox~trips.sub$sunset_after_h)


# 6. PCA analysis - prepare dataframe ------

# Use FactoMineR and factoextra for visualisation
# install.packages(c("factoextra", "FactoMineR"))
# See: http://www.sthda.com/english/wiki/print.php?id=225

# Prepare df for PCA
vars <- c("trip_id", "ring_number", "device_info_serial", "species",
          "log_coldist_max", "log_col_dist_median", "log_duration_s",
          "log_tortoisity", "p_flight_logit",
          "p_land_logit", "p_sea_logit", "p_coast_logit", "p_landfill_logit",
          "p_water_50m_logit",
          "sunrise.prox", "sunset.prox", "solarnoon.prox",
          "sunrise.prox.mid", "sunset.prox.mid", "solarnoon.prox.mid"
# "sunrise_after_h",
# "sunset_after_h",
# "solarnoon_after_h",
# "sunrise_after_h_mid", "sunset_after_h_mid",
# "solarnoon_after_h_mid"
)

pca.df_incl_trip_info <- select(trips.sub, one_of(vars))


pca.df_only <- select(pca.df_incl_trip_info, -c(1:4))
row.names(pca.df_only) <- trips.sub$trip_id


# 7. Bootstrapping the PCA ----------
# For each species to have equal influence on the drived PCAs we include
# equal numbers of foraging trips per species
# To make this robust, we resample many times and re-run the PCA


# summary(sample.trips)
pca.scores.list <- list()
pca.eigens.list <- list()

trips.sp <- split(trips.sub$trip_id, trips.sub$species)

summary(trips.sp)

# summary(sample.trips)
pca.scores.list <- list()
pca.eigens.list <- list()

# Run PCA lots of times
# for(run_num in c(1:1000)){

for(run_num in c(1:1000)){
  
  
  # Take a subset of data
  samples <- lapply(trips.sp, function(x) sample(x, 87, replace = FALSE))
  trips.sample <- unlist(samples)
  # ?sample
  
  sample.trips <- pca.df_incl_trip_info$trip_id %in% trips.sample
  # length(sample.trips)
  sup.inds <- c(1:nrow(pca.df_only))[!sample.trips]
  
  # Compute principal component analysis
  res.pca <- PCA(pca.df_only, ncp = 10,
                 ind.sup = sup.inds,
                 graph=FALSE)
  
  # Make dataframe of PC scores
  pca.score.df <- rbind.data.frame(res.pca$ind$coord,
                                   res.pca$ind.sup$coord)
  pca.score.df <- cbind.data.frame(pca.score.df, c(row.names(res.pca$ind$coord),
                                                   row.names(res.pca$ind.sup$coord)))
  names(pca.score.df)[11] <- "trip_id"
  
  # Sort df
  pca.score.df <- pca.score.df[order(as.numeric(as.character(pca.score.df$trip_id))),]
  pca.score.df$run <- run_num
  
  # Get eigen values of PCs
  eigens <- get_eig(res.pca)
  # Keep first 10 PCAs only
  eigens <- eigens[c(1:10),]
  eigens$run <- run_num
  eigens$pca <- row.names(eigens)
  
  
  # Add these to lists:
  pca.scores.list[[run_num]] <- pca.score.df
  pca.eigens.list[[run_num]] <- eigens
  
}

pca.eigens.df <- do.call(rbind.data.frame, pca.eigens.list)

pca.scores.df <- do.call(rbind.data.frame, pca.scores.list)

hist(pca.eigens.df$cumulative.variance.percent[pca.eigens.df$pca == 5])
mean(pca.eigens.df$cumulative.variance.percent[pca.eigens.df$pca == 5], na.rm = TRUE)
sort(pca.eigens.df$cumulative.variance.percent[pca.eigens.df$pca == 5])[c(50,500,950)]

# plot density plot thing for eigenvalues
theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "right",
        legend.justification = c(1, 1),
        legend.key.size =   unit(1, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12, face = "italic"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        # legend.text = element_text(colour="blue", size = 16, face = "bold")
        legend.key.width = unit(1.5, "lines")
  )

# str(pca.eigens.df)
library(reshape2)
plot.eig.df <- melt(pca.eigens.df, "pca", "eigenvalue")
str(plot.eig.df)
plot.eig.df$pca <- factor(plot.eig.df$pca, levels = unique(plot.eig.df$pca),
                          labels = c(1:10))
ggplot(plot.eig.df, aes(value, colour = pca)) +
  geom_density(adjust = 1/2, lwd = 0.5, alpha = 0.7)+
  geom_vline(xintercept = 1, lwd = 1, lty = 2,
             col = "black") +
  theme_new +
  labs(list(x = "Eigen value",
            y = "Density",
            colour = "PCA")) 

ggsave(filename = "pca_eigen_density.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "pca_eigen_density.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "pca_eigen_density.pdf", width = 4, height = 4,
       units = "in")

str(pca.eigens.df)
pca.eigens.df$pca <- factor(pca.eigens.df$pca, levels = unique(pca.eigens.df$pca),
                          labels = c(1:10))
eig.summary.df <- dplyr::summarise(group_by(pca.eigens.df,
                   pca),
          eigen_mean = mean(eigenvalue),
          eigen_median = median(eigenvalue),
          eigen_sd = sd(eigenvalue),
          var_per_mean = mean(variance.percent),
          var_per_median = median(variance.percent),
          var_per_sd = sd(variance.percent),
          cum_per_mean = mean(cumulative.variance.percent),
          cum_per_median = median(cumulative.variance.percent),
          cum_per_sd = sd(cumulative.variance.percent)
)
write.csv(eig.summary.df, file = "pca_eigen_summary.csv")


str(pca.scores.df)
# Get mean PCA components
pca.scores.mean.df <- dplyr::summarise(group_by(pca.scores.df,
                                     trip_id),
                           pc1 = mean(Dim.1),
                           pc2 = mean(Dim.2),
                           pc3 = mean(Dim.3),
                           pc4 = mean(Dim.4),
                           pc5 = mean(Dim.5),
                           pc6 = mean(Dim.6),
                           pc7 = mean(Dim.7),
                           pc8 = mean(Dim.8),
                           pc9 = mean(Dim.9),
                           pc10 = mean(Dim.10),
                           pc1_SD = sd(Dim.1),
                           pc2_SD = sd(Dim.2),
                           pc3_SD = sd(Dim.3),
                           pc4_SD = sd(Dim.4),
                           pc5_SD = sd(Dim.5),
                           pc6_SD = sd(Dim.6),
                           pc7_SD = sd(Dim.7),
                           pc8_SD = sd(Dim.8),
                           pc9_SD = sd(Dim.9),
                           pc10_SD = sd(Dim.10)
)
# 
hist(pca.scores.mean.df$pc1_SD)
hist(pca.scores.mean.df$pc2_SD)
hist(pca.scores.mean.df$pc1)

# Combine with details columns
pca.table <- merge(pca.scores.mean.df,
                   trips.sub[,c(1:4)],
                   by = "trip_id")
write.csv(pca.table, file = "pca.table.csv")

# save(list(pca.table,eig.summary.df,
#           pca.scores.list, pca.eigens.list), file = "pca.data.RData")
# 
# 
colMeans(pca.scores.mean.df[c(2:21)])





# 8. Find optimal clusters using pvclust -------

result < - pvclust(data, nboot=10000)

# data (first 5 PCs)
pca.table[,c(2:6)]

# ?pvclust

result <- pvclust(t(pca.table[c(1:100),c(2:6)]),
                  nboot=10000, parallel = TRUE,
                  method.hclust = "ward.D2",
                  method.dist = "euclidean")



seplot(result)

# 
# det(as.matrix(t(pca.table[,c(2:6)])))
# 
# dupli.mat <- duplicated.matrix(as.matrix((pca.table[,c(2:6)])))
# ?duplicated.matrix
# any(dupli.mat)
# 
# ?dist


result

pdf("test_clust.pdf", width = 100, height = 5)
plot(result)
pvrect(result)
dev.off()



# parallel::detectCores()


# # 8. Find optimal number of clusters -------
# # Run as above many times contrained to 7 clusters
# # Get means of centroid centres and PCA weights/ loadings etc...
# 
# 
# 
# # split trips between species
# trips.sp <- split(trips.sub$trip_id, trips.sub$species)
# 
# # To run adapted NbClust function
# source("NbClust_new_d_index_only.R")
# 
# nclus3 <- NULL
# nclus4 <- NULL
# nclus5 <- NULL
# nclus2 <- NULL
# 
# inertias <- list()
# dinertias <- list()
# 
# for(i in 1:1000){
#   
#   # Take a subset of data
#   samples <- lapply(trips.sp, function(x) sample(x, 87, replace = TRUE))
#   trips.sample <- unlist(samples)
#  
#   # Find rows with these trips  
#   trip.row <- sapply(trips.sample ,function(x){which(pca.table$trip_id == x)})
#   
#  
#   # need to source NbClust_new_d_index_only
#   # Run on first 5 PCA axes
#   res <- NbClust_new(pca.table[trip.row, c(2:6)], distance = "euclidean", min.nc=2, max.nc=23, 
#           method = "ward.D2", index = "dindex")
# 
#   
#   #inertia values (2-20 clusters)
#   inertias[[i]] <- res[[1]]
#   dinertias[[i]] <- res[[2]][2:21]
# # plot(res[[2]], type = "l")
#   
#   # Optimal cluster
# 
#   nclus2[i] <- which.max(res[[2]][2:20])  + 1
#   
#   
#   # Best more than 2 (3 or greater)
#   nclus3[i] <- which.max(res[[2]][3:20])  + 2
#   
#   
#   # Best more than 3 (4 or greater)
#   nclus4[i] <- which.max(res[[2]][4:20])  + 3
#   
#   # Best more than 4 (5 or greater)
#   nclus5[i] <- which.max(res[[2]][5:20])  + 4
#   
#   
# }
# 
# 
# 
# interia.gain.change.df <- do.call(rbind.data.frame, dinertias)
# names(interia.gain.change.df) <- c(2:20)
# 
# interia.gain.change.df.melt <- melt(interia.gain.change.df,measure.vars = c(1:19))
# 
# colMeans(interia.gain.change.df)
# boxplot(interia.gain.change.df.melt$value~
#           interia.gain.change.df.melt$variable)
# abline(h=0)
# 
# # nclus3
# # nclus4
# # min == 4
# hist(nclus2, breaks = 100)
# hist(nclus4, breaks = 100)
# hist(nclus3, breaks = 100)
# hist(nclus5, breaks = 100)
# 
# mean(nclus4)
# median(nclus4)
# median(nclus5)
# mean(nclus5)
# 
# inertia.gain.df <- do.call(rbind.data.frame, inertias)
# names(inertia.gain.df) <- c(2:20)
# 
# # library(reshape2)
# interia.gain.change.df.melt <- melt(inertia.gain.df,measure.vars = c(1:19))
# 
# boxplot(interia.gain.change.df.melt$value~interia.gain.change.df.melt$variable)
# 
# # Median, 95% CI and 50% CI
# sort(nclus4)[c(25,500,950)]
# 
# sort(nclus3)[c(250,2500, 5000, 7500, 9750)]
# 
# levels(interia.gain.change.df.melt$variable)
# 
# inertia.gain.summary <- interia.gain.change.df.melt %>% group_by(variable) %>%
#   summarise(inertia.med = median(value, na.rm = TRUE),
#             inertia.up95 = quantile(value, 0.95),
#             inertia.low95 = quantile(value, 0.05)
#             )
# 
# quantile(c(0:100), 0.95)
# 
# 
# # ggplot for inertia gain
# ggplot(interia.gain.change.df.melt, aes_string(x = "variable", y = "value")) +
#   geom_point(alpha = 0.01)
# 
# 
# 
# # 9. Find cluster centres ------
# # Bootstrap again but with predefined number of clusters (5)
# 
# 
# centres.ls <- list()
# # k_clust.ls <- list()
# 
# for(i in 1:1000){
#   
#   # Take a subset of data
#   samples <- lapply(trips.sp, function(x) sample(x, 87, replace = TRUE))
#   trips.sample <- unlist(samples)
#   
#   # Find rows with these trips  
#   trip.row <- sapply(trips.sample ,function(x){which(pca.table$trip_id == x)})
#   
#   # ?dist
#   
#   hc <- hclust(dist(pca.table[trip.row, c(2:6)]),  "ward.D2")
#   memb <- cutree(hc, k = 5)
#   cent <- NULL
#   for(k in 1:5){
#     cent <- rbind(cent, colMeans(pca.table[trip.row, c(2:6)][memb == k, , drop = FALSE]))
#   }
# 
#   x <- cent
#   x <- x[order(round(x[,1],0),round(x[,2],0),round(x[,3],0),round(x[,4],0),round(x[,5],0)),]
#   x <- as.data.frame(x)
#   x$clust <- c(1:5)
#   x$run <- i
# 
#   centres.ls[[i]] <- x
# 
#   }
# 
# 
# # clusters(pca.table[trip.row, c(2:6)], cent)
# 
# 
# centres.df <- do.call(rbind.data.frame, centres.ls)
# # 
# # hist(centres.df$pc1[centres.df$clust == 1])
# # hist(centres.df$pc1[centres.df$clust == 2])
# # hist(centres.df$pc1[centres.df$clust == 3])
# # hist(centres.df$pc2[centres.df$clust == 1])
# # hist(centres.df$pc2[centres.df$clust == 2])
# # hist(centres.df$pc2[centres.df$clust == 3])
# # str(centres.df)
# 
# centres.df$clust <- as.factor(centres.df$clust)
# 
# ggplot(data = centres.df, aes_string("pc1","pc2", color = "clust",
#                                      shape = "clust")) +
#   geom_point()
# 
# 
# # Possible alternative with GGally::ggscatmat
# # install.packages("GGally", type = "source")
# # ?install.packages
# library(GGally)
# library(cowplot)
# # 
# p <- ggpairs(centres.df, mapping = aes(color = clust, alpha = 0.5),
#              columns = c(1:5),
#              upper=list(continuous='blank'))
# p
# 
# 
# 
# # Apply to all data ------
# clusters <- function(x, centers) {
#   # compute squared euclidean distance from each sample to each cluster center
#   tmp <- sapply(seq_len(nrow(x)),
#                 function(i) apply(centers, 1,
#                                   function(v) sum((x[i, ]-v)^2)))
#   clust <- max.col(-t(tmp))  # find index of min distance
#   dist <- t(tmp)[clust]
#   return(cbind(clust, dist))
# }
# # ?max.col
# 
# # Summarise centres
# clust.cen <- dplyr::summarise(group_by(centres.df,
#                                                 clust),
#                                        pc1 = mean(pc1),
#                                        pc2 = mean(pc2),
#                                        pc3 = mean(pc3),
#                                        pc4 = mean(pc4),
#                                        pc5 = mean(pc5)
# )
# 
# 
# str(clust.cen)
# # clusters()
# # ?matrix
# x <- matrix(nrow = nrow(pca.table), ncol = 2)
# for(i in 1:nrow(pca.table)){
#   x[i,] <- clusters(pca.table[i, c(2:6)], clust.cen[,c(2:6)])
# }
# 
# # str(x)
# hist(x[,2], breaks = 20)
# 
# 
# # Output table
# out.tab <- cbind.data.frame(pca.table, x)
# names(out.tab)[c(25:26)] <- c("clust", "dist")
# save(out.tab, file = "cluster_data.RData")
# write.csv(out.tab, file = "cluster_data.csv")
# 
# 
# trips.clust <- cbind.data.frame(pca.table[, c(1:6)],x)
# names(trips.clust)[7:8] <- c("clust", "dist")
# trips.clust$clust <- as.factor(trips.clust$clust)
# 
# p <- ggpairs(trips.clust, mapping = aes(color = clust, alpha = 0.5),
#              columns = c(2:6),
#              upper=list(continuous='blank'))
# p
# 
# 
# 
# 
# # Build tree to show final clusters -----
# par(mfrow=c(1,1))
# final_clust <- hclust(dist(clust.cen),  "ward.D2")
# plot(final_clust)
# 
# library(ggplot2)
# library("ggdendro")
# 
# # ?ggdendrogram
# ggdendrogram(final_clust)
# # ggplot(ggd1) 
# 
# 
# trip_clust <- hclust(dist(pca.table[,c(2:6)]),  "ward.D2")
# plot(trip_clust)
# 
# 
# 
# # 
# # # Playing with distance matrix thing -----
# # a <- dist(pca.table[trip.row, c(2:6)])
# # 
# # ?dist
# # # length(trip.row)
# # 
# # str(a)
# # hist(a)
# # 
# # m <- as.matrix(a)
# # str(m)
# # 
# # x <- as.data.frame(m)
# # x$trip1 <- row.names(x)
# # 
# # ?melt
# # x.long <- melt(x, "trip1")
# # 
# # # If wanted to boostrap distance matrix could do above - but think these should always be the same, so
# # # doesn't make sense to do this way
# # 
# # plot(hclust(a,  "ward.D2"))
