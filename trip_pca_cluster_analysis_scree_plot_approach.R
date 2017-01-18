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
# library("PerformanceAnalytics")

# # To determine optimal number of clusters, and which clusters are statistically supported
# install.packages("pvclust")
# library("pvclust")
# # vignette("pvclust")

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


theme_new_rt <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.justification = c(1, 1),
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
# myColors <- c("#66c2a5", "#fc8d62", "#8da0cb", "grey40")

# 4 Colours for species:
# From: mkweb.bcgsc.ca
spColors <- c("#0072b2",  "#d55e00",
              "#009e73", "#cc79a7")


# 8 Colours for clusters:
# http://colorbrewer2.org/?type=qualitative&scheme=Set1&n=8
myColors <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf")


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

# hist(trips.sub$sunrise.prox)
# plot(trips.sub$sunrise.prox~trips.sub$sunrise_after_h)

# Time before/ after sunset
trips.sub$sunset.prox <- cos((trips.sub$sunset_after_h/12)*pi)


# Time before/ after noon
trips.sub$solarnoon.prox <- cos((trips.sub$solarnoon_after_h/12)*pi)
# hist(trips.sub$solarnoon.prox)


# Time before/ after sunrise
trips.sub$sunrise.prox.mid <- cos((trips.sub$sunrise_after_h_mid/12)*pi)

# hist(trips.sub$sunrise.prox.mid)
# plot(trips.sub$sunrise.prox.mid~trips.sub$sunrise_after_h)

# Time before/ after sunset
trips.sub$sunset.prox.mid <- cos((trips.sub$sunset_after_h_mid/12)*pi)
# hist(trips.sub$sunset.prox.mid)

# Time before/ after noon
trips.sub$solarnoon.prox.mid <- cos((trips.sub$solarnoon_after_h_mid/12)*pi)
# hist(trips.sub$solarnoon.prox.mid)



# Transformations of proportion variables
trips.sub$p_flight_logit <- logit(trips.sub$p_flight)
# hist(trips.sub$p_flight_logit)

trips.sub$p_land_logit <- logit(trips.sub$p_land)
# hist(trips.sub$p_land_logit)

trips.sub$p_sea_logit <- logit(trips.sub$p_sea)
# hist(trips.sub$p_sea_logit)

trips.sub$p_coast_logit <- logit(trips.sub$p_coast)
# hist(trips.sub$p_coast_logit)

trips.sub$p_landfill_logit <- logit(trips.sub$p_landfill)
# hist(trips.sub$p_landfill_logit)

trips.sub$p_water_20m_logit <- logit(trips.sub$p_water_20m)
# hist(trips.sub$p_water_20m_logit)

trips.sub$p_water_50m_logit <- logit(trips.sub$p_water_50m)
# hist(trips.sub$p_water_50m_logit)



# Transformations of other variables
# "coldist_max", "col_dist_median", "duration_s", "tortoisity"

trips.sub$log_coldist_max <- log10(trips.sub$coldist_max)
# hist(trips.sub$log_coldist_max)

trips.sub$log_col_dist_median <- log10(trips.sub$col_dist_median)
# hist(trips.sub$log_col_dist_median)

trips.sub$log_duration_s <- log10(trips.sub$duration_s)
# hist(trips.sub$log_duration_s)

trips.sub$log_tortoisity <- log10(trips.sub$tortoisity-0.999)
# hist(trips.sub$log_tortoisity)



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
)

# "sunrise_after_h",
# "sunset_after_h",
# "solarnoon_after_h",
# "sunrise_after_h_mid", "sunset_after_h_mid",
# "solarnoon_after_h_mid"
# )

pca.df_incl_trip_info <- dplyr::select(trips.sub, one_of(vars))


pca.df_only <- dplyr::select(pca.df_incl_trip_info, -c(1:4))
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

pca.var.load.list <- list()
pca.var.contr.list <- list()

# Run PCA lots of times
# for(run_num in c(1:1000)){

# For reproducibility specify seed
# ?set.seed
set.seed(1)
for(run_num in c(1:20000)){
  
  # all.equal(unlist(samples), unlist(samples2), unlist(samples3))
  
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
  # ?PCA

  # Summarise variable loadings
  # str(res.pca)
  
  pc.contr <- (res.pca$var$contrib)
  
  loadings <- sweep(res.pca$var$coord, 2,
                    sqrt(res.pca$eig[1:10,1]), FUN = "/")
  
  
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
  
  pca.var.load.list[[run_num]] <- loadings
  pca.var.contr.list[[run_num]] <- pc.contr
  
}

pca.eigens.df <- bind_rows(pca.eigens.list)

pca.scores.df <- bind_rows(pca.scores.list)


# x <- as.data.frame(loadings)
# loadings

# Function to check each run for PC behaviour
# To get repeatable PCs, and to avoid PCs being derived with both positive and negative directions
test.fun <- function(x){
  x <- as.data.frame(x)
  t <- sum(
    (x$Dim.1[12] < -0.1) +
    (x$Dim.2[12] > 0.1) +
    (x$Dim.1[16] > 0.1) +  
    (x$Dim.2[16] < -0.1) +
    (abs(x$Dim.4[11]) > 0.3) +
    (abs(x$Dim.5[4]) > 0.3) +  
    ((x$Dim.4[15]) < 0) +
    ((x$Dim.5[4]) > 0.1)
      
  )==8
  return(t)
}

a <- NULL
for(i in 1:20000){
 a[i] <- test.fun(pca.var.load.list[[i]])
}
summary(a)
# 
# test.fun(pca.var.load.list[[2]])
# x <- as.data.frame(pca.var.load.list[[2]])
# 
# test <- lapply(pca.var.load.list, function(x){ test.fun(x[[1]])})
# summary(test)


# Select just those runs fulfilling criteria
pca.scores.df.sub <- bind_rows(pca.scores.list[a])
pca.eigens.df.sub <- bind_rows(pca.eigens.list[a])




# Filter by loadings to get consistent PCs (i.e. same order etc)
pca.var.load.list.df <- bind_rows(pca.var.load.list)

str(pca.var.load.list[[1]])



# test.mod <- lm(pca.scores.df$Dim.4~pca.scores.df$Dim.5)
# ?filter
# str(pca.score.df)

s.test <- sample(unique(pca.scores.df.sub$trip_id),1)
test.pc5 <- dplyr::filter(pca.scores.df.sub, trip_id == s.test)
par(mfrow = c(2,3))
hist(test.pc5$Dim.1)
abline(v=c(median(test.pc5$Dim.1), mean(test.pc5$Dim.1)),
       col = c("red", "blue"), lty = c(1, 2))
hist(test.pc5$Dim.2)
abline(v=c(median(test.pc5$Dim.2), mean(test.pc5$Dim.2)),
       col = c("red", "blue"), lty = c(1, 2))
hist(test.pc5$Dim.3)
abline(v=c(median(test.pc5$Dim.3), mean(test.pc5$Dim.3)),
       col = c("red", "blue"), lty = c(1, 2))
hist(test.pc5$Dim.4)
abline(v=c(median(test.pc5$Dim.4), mean(test.pc5$Dim.4)),
       col = c("red", "blue"), lty = c(1, 2))
hist(test.pc5$Dim.5)
abline(v=c(median(test.pc5$Dim.5), mean(test.pc5$Dim.5)),
       col = c("red", "blue"), lty = c(1, 2))
# boxplot((test.pc5$Dim.5))
#



# ci05 <- function(x){
#   return(sort(x)[50])
# }
# 
# ci95 <- function(x){
#   return(sort(x)[950])
# }

# Mean contribution for variables
contribution.means <- apply(simplify2array(pca.var.contr.list[a]), c(1,2), mean)
contribution.sd <- apply(simplify2array(pca.var.contr.list[a]), c(1,2), sd)
# ?sd

# contribution.05 <- apply(simplify2array(pca.var.contr.list), c(1,2), ci05)
# contribution.95 <- apply(simplify2array(pca.var.contr.list), c(1,2), ci95)
# # ?sd

x <- contribution.means/contribution.sd

colSums(contribution.means)

mean.abs <- function(x){
  return(mean(abs(x)))
}

# Mean loading for variables
loadings.means <- apply(simplify2array(pca.var.load.list[a]), c(1,2), mean)
loadings.sd <- apply(simplify2array(pca.var.load.list[a]), c(1,2), sd)
loadings.means.abs <- apply(simplify2array(pca.var.load.list[a]), c(1,2), mean.abs)

loadings.mean.dif <- loadings.means.abs- abs(loadings.means)

# ?sd
x <- loadings.means/loadings.sd

# ?simplify2array

summary.tab <- paste(trimws(format(round(loadings.means, 3), nsmall = 3),
                            which = "left"), " (",
                     trimws(format(round(contribution.means, 1), nsmall = 1), which = c("left")),
                     ")", sep = "")
summary.tab <- as.data.frame(matrix(summary.tab, ncol = 10))
summary.tab$var <- row.names(loadings.means)

summary.tab.export <- cbind.data.frame(summary.tab[,c(11,1:5)])
# paste(".", format(098, nsmall = 0), sep = "")

write.csv(summary.tab.export, file = "loading_contribution_pca.csv")

# paste(".", format(round(contribution.means[3], 0), nsmall = 1), sep = "")
# paste(".", format(round(contribution.means[2:3], 0), nsmall = 1), sep = "")

hist(pca.eigens.df.sub$cumulative.variance.percent[pca.eigens.df.sub$pca == "Dim.5"])
mean(pca.eigens.df.sub$cumulative.variance.percent[pca.eigens.df.sub$pca == "Dim.5"], na.rm = TRUE)
n <- nrow(pca.eigens.df.sub[pca.eigens.df.sub$pca == "Dim.5",])
cum.per <- sort(pca.eigens.df.sub$cumulative.variance.percent[pca.eigens.df.sub$pca == "Dim.5"])
cum.per[c(floor(c(n*0.05, n*0.5, n*0.95)))]

# plot(sort(pca.eigens.df.sub$cumulative.variance.percent[pca.eigens.df.sub$pca == "Dim.5"]))

# summary(is.na(pca.eigens.df.sub$cumulative.variance.percent[pca.eigens.df.sub$pca == "Dim.5"]))


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
plot.eig.df <- melt(pca.eigens.df.sub, "pca", "eigenvalue")
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

str(pca.eigens.df.sub)
pca.eigens.df.sub$pca <- factor(pca.eigens.df.sub$pca, levels = unique(pca.eigens.df.sub$pca),
                          labels = c(1:10))
eig.summary.df <- dplyr::summarise(group_by(pca.eigens.df.sub,
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

# # Correlation between variables and components (just last run)
# dimdesc(res.pca, axes = 1:5, proba = 0.05)


str(pca.scores.df.sub)

# Get mean PCA components
pca.scores.mean.df <- dplyr::summarise(group_by(pca.scores.df.sub,
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
dev.off()
hist(pca.scores.mean.df$pc1_SD)
hist(pca.scores.mean.df$pc2_SD)
hist(pca.scores.mean.df$pc1)

# Combine with details columns
pca.table <- merge(pca.scores.mean.df,
                   trips.sub[,c(1:4)],
                   by = "trip_id")
write.csv(pca.table, file = "pca.table.csv")
save(list= c("pca.table", "eig.summary.df"), file = "pca.data.RData")
# ?save
# 
colMeans(pca.scores.mean.df[c(2:21)])


# 8. Determine number of clusters and cluster ID -----
# If resume here:
load("pca.data.RData")


# To run adapted NbClust function
source("NbClust_new_d_index_only.R")

  # need to source NbClust_new_d_index_only
  # Run on first 5 PCA axes
  res <- NbClust_new(pca.table[,c(2:6)], distance = "euclidean", min.nc=2, max.nc=23, 
          method = "ward.D2", index = "dindex")

  
  
  
  #inertia values (2-20 clusters)
  # Scree plot
  plot(res[[1]]~c(2:23), type = "b")
  abline(v=7)
  
  df.inertia <- cbind.data.frame(res[[1]], c(2:23))
  names(df.inertia) <- c("inertia", "n_clust")
  df.inertia$col <- c(rep("dark grey",6), rep("light grey",16))
  
  # ?geom_bar
 inertia.plot <-  ggplot(df.inertia, aes(y = inertia, x = n_clust)) +
    geom_bar(aes(fill = col), stat="identity")+
    scale_fill_manual(values=c("grey10", "grey90")) +
    labs(list(x = "Number of clusters",
            y = "Inertia gain")) +
    guides(fill=FALSE)
            
  ggsave(inertia.plot, filename = "inertia_gain_clusters.svg", width = 4, height = 4,
         units = "in")
  ggsave(inertia.plot, filename = "inertia_gain_clusters.png", width = 4, height = 4,
         units = "in")
  ggsave(inertia.plot, filename = "inertia_gain_clusters.pdf", width = 4, height = 4,
         units = "in")
  
    # Change in inertia thing
  res[[2]][2:21]
  plot(res[[2]][2:21]~c(1:20), type = "l")

  
  
  df.inertia2 <- cbind.data.frame(res[[2]][2:21], c(3:22))
  names(df.inertia2) <- c("inertia", "n_clust")
  
  
  inertia.change <- ggplot(df.inertia2, aes(y = inertia, x = n_clust)) +
    geom_line()+
    labs(list(x = "Number of clusters",
              y = "Rate of change of inertia"))
  
  ggsave(inertia.change, filename = "inertia_gain_clusters_rate.svg", width = 4, height = 4,
         units = "in")
  ggsave(inertia.change, filename = "inertia_gain_clusters_rate.png", width = 4, height = 4,
         units = "in")
  ggsave(inertia.change, filename = "inertia_gain_clusters_rate.pdf", width = 4, height = 4,
         units = "in")
  
  # Combine the two plots (cowplot function)
  g <- plot_grid(inertia.plot, inertia.change)
  
  ggsave(g, filename = "inertia_change_combined.png", width = 8, height = 4,
         units = "in")
  ggsave(g, filename = "inertia_change_combined.pdf", width = 8, height = 4,
         units = "in")
  ggsave(g, filename = "inertia_change_combined.svg", width = 8, height = 4,
         units = "in")
  
  # Indicates one optima of 7 clusters
  
  
  
  
# 9. Determine cluster IDs -------
  # ?dist
  hc <- hclust(dist(pca.table[,c(2:6)],
                    method = "euclidean"),  "ward.D2")
# ?hclust
  plot(hc)
  
  hcd <- as.dendrogram(hc)
  plot(hcd, ylab = "Height",
       # nodePar = nodePar,
       leaflab = "none")
  
  # rect.hclust(hc, k = 4, border = "blue")
  rect.hclust(hc, k = 7, border = "red")
  
  # What cluster (of 8) are each trip in?
  memb <- cutree(hc, k = 7)
  
  # Re-label clusters (to get in logical order)
  memb.new <- memb
  
  
  memb.new[memb == 1] <- 7  
  memb.new[memb == 2] <- 5 
  memb.new[memb == 3] <- 3 
  memb.new[memb == 4] <- 1 
  memb.new[memb == 5] <- 2 
  memb.new[memb == 6] <- 4 
  memb.new[memb == 7] <- 6 
  
#   
#   memb <- cutree(hc, h = 4)
#   max(memb)
#   
#   
#   ?cutree
#   
#   # 7 Cluster centres
#     cent <- NULL
#     for(k in 1:max(memb)){
#       cent <- rbind(cent, colMeans(pca.table[,c(2:6)][memb == k, , drop = FALSE]))
#     }
#   
# 
#     # Dendrogram for cluster centres only
#     hc_8 <- hclust(dist(cent,
#                       method = "euclidean"),  "ward.D2")
#     # ?hclust
#     plot(hc_8)
    
    # See cluster distribution by species
    table(memb.new,
          pca.table$species)
    
    
    
    # Output labelled trip table
    
    pca.table.sort <- pca.table[order(as.numeric(as.character(pca.table$trip_id))),]
    trips.sub.sort <- trips.sub[order(trips.sub$trip_id),]
   
    
    all(trips.sub.sort$trip_id == as.numeric(as.character(pca.table.sort$trip_id)))
    
    clust.tab <- cbind.data.frame(pca.table.sort, trips.sub.sort,
                                  memb.new[order(as.numeric(as.character(pca.table$trip_id)))])
    names(clust.tab)
    
    names(clust.tab)[91] <- "cluster"
    clust.tab$cluster_fac <- as.factor(clust.tab$cluster)
    table(clust.tab$cluster,clust.tab$cluster_fac)
    levels(clust.tab$cluster_fac)
    
    # Check data are aligned correctly
    test.x <- (clust.tab[,c(1)]==as.numeric(clust.tab[,c(25)]))
    all(test.x)  

    save(clust.tab, file = "cluster_data_detailed.RData")
    
    write.csv(clust.tab, file = "cluster_data_detailed.csv")
    
    
    
    
    # 10. Plots to summarise PCA and clustering -------
    # on resume:
    load("cluster_data_detailed.RData")
    
    # 10a. PCA + clusters multi-panel plot -------
    library(GGally)
    library(cowplot)
   
    
    # Ref: John W Emerson, Walton A Green, Barret Schloerke, Jason Crowley, Dianne Cook, Heike Hofmann, Hadley Wickham. The Generalized Pairs Plot. Journal of Computational and Graphical Statistics, vol. 22, no. 1, pp. 79-91, 2012.
    
   summary(clust.tab$cluster_fac)
    
    p <- ggpairs(clust.tab, mapping = aes(color = cluster_fac, alpha = 0.5),
                 columns = c(2:6),
                 upper=list(continuous='blank')) 
      
    p
    # ?ggpairs
    
    
    ggsave(p, file = "pca_clusters.svg", width = 10, height = 10, units = "in")
    ggsave(p, file = "pca_clusters.png", width = 10, height = 10, units = "in")
    ggsave(p, file = "pca_clusters.pdf", width = 10, height = 10, units = "in")
    
    
    
    
    # 10b. Cladogram thing -------
    # Read in PCA table
    # pca.table <- read.csv("pca.table.csv")
    
    # install.packages("dendextend")
    # Package for extended options for dendrograms
    library(dendextend)
    
    # See guide here:
    # https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html
    
    
    
    
    # Change labels
    species.short <- clust.tab$species
    species.short <- as.factor(species.short)
    str(species.short)
    levels(species.short) <- c("L.arg", "L.can", "L.fus", "L.mar")
    
    birds <- unique(clust.tab$ring_number)
    
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }    
    
    length(birds)
    
    birds_3 <- ((substrRight(clust.tab$ring_number, 3)))
    
    length(unique(birds_3))
    
    sp.ind <- paste(species.short, birds_3, clust.tab$trip_id, sep = ".")
    
    row.names(clust.tab) <- as.character(sp.ind)
    
    hc <- hclust(dist(clust.tab[,c(2:6)],
                      method = "euclidean"),  "ward.D2")
    # ?hclust
    plot(hc)
    
    
    # plot(hc)
    
    # rect.hclust(hc, k = 4, border = "blue")
    rect.hclust(hc, k = 7, border = "red")
    
#     # What cluster (of 7) are each trip in?
    memb <- cutree(hc, k = 7)
head(memb)
head(clust.tab$cluster)  
clust.tab$memb.2 <- memb

all(as.numeric(as.character(clust.tab$cluster)) == clust.tab$memb.2)
    
table(clust.tab$cluster, clust.tab$memb.2)
# ?table
#     # ?cutree
#     
    # make as dendrogram
    dend <- as.dendrogram(hc)
    
    sp <- sort(unique(clust.tab$species))
    
    # Species colours
    colors_to_use <- NULL
    for(i in 1:nrow(clust.tab)){
      colors_to_use[i] <- spColors[clust.tab$species[i] == sp]
    }
    colors_to_use_original <- colors_to_use
    # But sort them based on their order in dend:
    colors_to_use <- colors_to_use[order.dendrogram(dend)]
    
    labels_colors(dend) <- colors_to_use
    
    plot(dend)
    
    # Colour by clusters
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    # Get 7 colours
    col.7 <- gg_color_hue(7)
    
    # levels(clust_dend)
    
    # i <- 90
    
    colors_clust <- NULL
    for(i in 1:nrow(clust.tab)){
      colors_clust[i] <- col.7[as.numeric(as.character(clust.tab$cluster[i])) == c(1:7)]
    }
    colors_clust_dend <- colors_clust[order.dendrogram(dend)]
    clust_dend <- clust.tab$cluster[order.dendrogram(dend)]
    
    
    dend1 <- color_branches(dend, k = 7,
                            col = col.7)
                            # col = col.7[c(6,3,5,4,2,7,1)])
    # ?color_branches
    # ?color_branches
    # plot(c(1:7), pch = 12, col = col.7)
    
    
#     # Reduce text size
    dend1 <- set(dend1, "labels_cex", 0.2)
#     
    
    
    pdf("dendrogram_test_label.pdf", width = 48, height = 12)
    plot(dend1)
    dev.off()
    
    
   
    

    # levels(clust.tab$cluster)
    
    pdf("dendrogram_colour.pdf", width = 6, height = 6)
    svg("dendrogram_colour.svg", width = 6, height = 6)
    png("dendrogram_colour.png", width = 6, height = 6,
        units = "in",
        res = 600)
    
    # With species bar
    par(mar = c(6.5,5,1,1))
    plot(dend1, leaflab = "none", cex.axis = 1,
         las = 1, ylab = "Height", cex.lab = 1)
    colored_bars(cbind(colors_to_use_original, colors_clust),
                 dend1, rowLabels = c("Species", "Cluster"),
                 y_shift = -1, y_scale = 12,
                 cex = 1)
    axis(side=(1),las=1, cex.lab = 1, cex.axis =1,
         cex = 1, padj = -0.9, hadj = NA,
         line = 3.7)
    mtext("Foraging trips", side = 1,
          line = 5)
    dev.off()
    
    pdf("dendrogram_colour_horizontal.pdf", width = 8, height = 8)
    # With species bar
    par(mar = c(7,2,2,10))
    plot(dend1, leaflab = "none", cex.axis = 1,
         las = 1, xlab = "Height", cex.lab = 1,
         horiz = TRUE)
    colored_bars(cbind(colors_clust, colors_to_use_original),
                 dend1, rowLabels = c( "Cluster", "Species"),
                  y_scale = 10, y_shift = 11,
                 cex = 1, horiz = TRUE)
    # ?colored_bars
    legend("topleft", legend =  c(
      "L. argentatus", "L. canus", "L. fuscus", "L. marinus"
    ), fill = spColors, bty = "n")
    # ?legend
    dev.off()
    
    
#     par(mar = c(4,1,1,12))
#     plot(dend, horiz = TRUE)
#     colored_bars(cbind(k234[,3:1], col_car_type), dend, rowLabels = c(paste0("k = ", 4:2), "Car Type"), horiz = TRUE)
#     legend("topleft", legend = levels(car_type), fill = cols_4)
#     
    # ?colored_bars
    
    # Circular version
    # install.packages("circlize")
    library(circlize)
    
    
    #     # Reduce text size
    dend1 <- set(dend1, "labels_cex", 0.2)
    #     
    pdf("dendrogram_test_label_circle.pdf")
    svg("dendrogram_test_label_circle.svg")
    png("dendrogram_test_label_circle.png", width = 12, height = 12,
        units = "in",
        res = 600)
    # plot the radial plot
    par(mar = rep(0,4))
    # ?circlize_dendrogram
    # dend2 <- set(dend1, "labels_cex", 1)
    
    circlize_dendrogram(dend1,  facing = "outside", labels_track_height = 0.1,
                        labels = TRUE) 
    # circlize_dendrogram(dend)
    # circos.dendrogram(dend)
    dev.off()
    
    
    # 10c. Barplot thing -----
    
    tab.k.sp <- table(clust.tab[,c("cluster", "species")])
    # 
    # Numbers
    tab.k.sp
    # tab.k.sp[1,2]
    rowSums(tab.k.sp)
    
    tab.k.sp.t <- as.data.frame(tab.k.sp)
    str(tab.k.sp.t)
    
    # Proportions
    #Within species
    tab.sp.t.p <- prop.table(tab.k.sp, 2)
    
 
    
    tab.sp.t.p.t <- t(tab.sp.t.p)
    tab.sp.t.p.t.df <- as.data.frame(tab.sp.t.p.t)
    str(tab.sp.t.p.t.df)
    names(tab.sp.t.p.t.df)[3] <- "perc_sp"
    tab.sp.t.p.t.df
    
    
    
    library(scales)
    
   p <-  ggplot(tab.sp.t.p.t.df, aes( x= cluster, group = cluster)) + 
      geom_bar(aes(weight = perc_sp, fill = cluster)) + 
      facet_grid(~species) +
      scale_y_continuous(labels=percent) +
      labs(y = "Proportion of trips (%)",
           x = "Cluster", fill="Cluster") +
      theme_new_rt
   
   p <- p + theme(legend.position='none') 
   # Italicise species names
   p <- p  + theme(strip.text = element_text(face = "italic"))
   p
    # ?ggsave
    ggsave(p, file = "cluster.prop.sp.prop.png", width = 8, height = 4)
    ggsave(p, file = "cluster.prop.sp.prop.svg", width = 8, height = 4)
    ggsave(p, file = "cluster.prop.sp.prop.pdf", width = 8, height = 4)

    
    
# 11. Summarise clusters by differences/ similarities -----
    
# See seppearte script file    
    
  