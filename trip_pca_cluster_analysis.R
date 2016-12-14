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


# Perform analysis on data subset --------
# Get sample of trips for each species
trips.sp <- split(trips.sub$trip_id, trips.sub$species)
samples <- lapply(trips.sp, function(x) sample(x, 60))
trips.sample <- unlist(samples)


# vars  %in% names(trips.sub)
pca.df_incl_trip_info <- select(trips.sub, one_of(vars))
row.names(pca.df_incl_trip_info) <- trips.sub$trip_id

pca.df_only <- select(pca.df_incl_trip_info, -c(1:4))
row.names(pca.df_only) <- trips.sub$trip_id

# take sub.sets for sampled trips only
pca.df_incl_trip_info <- dplyr::filter(pca.df_incl_trip_info, trip_id %in% trips.sample)

pca.df_only <- subset(pca.df_only, rownames(pca.df_only) %in% trips.sample)


# Inspect data (scaled)
df <- scale(pca.df_only)

summary(df)

chart.Correlation(df, histogram=TRUE, pch=19)
# warnings()

# 7. Perform PCA etc -----
# Compute principal component analysis
res.pca <- PCA(pca.df_only, ncp = 5, graph=FALSE)

# Barplot of eigenvalues to choose number of PCA dimensions
barplot(res.pca$eig[,1], main = "Eigenvalues",
        names.arg = paste("Dim", 1:nrow(res.pca$eig), sep = ""))
abline(h=1, lwd = 1.5, lty = 2)
# First 4 PCAs have positive eigen values

# View how variables correlate with PCA components
png("PCA_variables_polar_plots.png",
    width = 4, height = 12, units = "in",
    res = 300)

par(mfrow=c(2,2))
# Can reduce number of variables displayed by limiting cosines
plot(res.pca, choix = "var", axes = c(1, 2), lim.cos2.var = 0.4)
plot(res.pca, choix = "var", axes = c(1, 3), lim.cos2.var = 0.4)
plot(res.pca, choix = "var", axes = c(1, 4), lim.cos2.var = 0.4)
plot(res.pca, choix = "var", axes = c(1, 5), lim.cos2.var = 0.4)
dev.off()


# plot(res.pca, choix = "var", axes = c(1, 5), lim.cos2.var = 0.4)

# Percentage of information retained by each
# dimensions
fviz_eig(res.pca)

# View eigenvalues and cumulative variance explained
get_eig(res.pca)
# First 4 PCA dimensions explain 79.2% of variance and have eigenvalues >1

# # Visualize variables with factoextra package instead
# fviz_pca_var(res.pca,
#              axes = c(1, 2),
#              repel = TRUE)
# fviz_pca_var(res.pca,
#              axes = c(1, 3))
# fviz_pca_var(res.pca,
#              axes = c(1, 4))


# ?fviz_pca_var

# str(res.pca)

# Compute HCPC (hierachial clustering principle components)
res.hcpc <- HCPC(res.pca, graph = TRUE)
# ?HCPC

# res.hcpc$call$t$tree
# res.hcpc$call$t$quot 
svg("dendrogram_tree.svg", width = 5, height = 5)
plot(res.hcpc, draw.tree = TRUE,choice = "tree")
dev.off()


svg("dendrogram_inertia.svg", width = 3, height = 3)
plot(res.hcpc, draw.tree = TRUE,choice = "bar")
dev.off()

# # Build dendrogram object from hclust results
# dend <- as.dendrogram(res.hcpc$call)
# # Extract the data (for rectangular lines)
# # Type can be "rectangle" or "triangle"
# dend_data <- dendro_data(dend, type = "rectangle")


# Summary of type of analysis
res.hcpc$call$t$tree

# Optimal number of clusters - 6
res.hcpc$call$t$nb.clust

# See what clusters each trip belongs to along with variables
x <- res.hcpc$data.clust

# See what variables are important to different clusters and what defines clusters
res.hcpc$desc.var

# See also NS variables
df.tab <- catdes(x, num.var = ncol(x), proba = 1)
df.tab

# See how clusters correspond to PCA axes
res.hcpc$desc.axes


dat.plot <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(dat.plot) <- c("pc1", "pc2", "pc3", "pc4", "pc5", "cluster")

# See also NS ones
catdes(dat.plot, num.var = ncol(dat.plot), proba = 1)


# See which trips are closest to cluster centres, the 'paragon' and furtherst from any other cluster (but still within this cluster, the most specific) under 'dist'
res.hcpc$desc.ind





# Calculate loading of different variables on PCA axes
# See: http://factominer.free.fr/faq/index.html
sweep(res.pca$var$coord,2,sqrt(res.pca$eig[1:ncol(res.pca$var$coord),1]),FUN="/")


#PCs + tree
par(mfrow=c(1,1))
plot(res.hcpc, choice = "3D.map")

plot(res.hcpc, choice = "3D.map",
     ind.names = FALSE)


# Dendrogram
plot(res.hcpc, choice ="tree", cex = 0.6,
     ind.names = FALSE)

# Factor map
par(mfrow=c(3,1))
plot(res.hcpc, choice ="map", draw.tree = FALSE)
plot(res.hcpc, choice ="map", draw.tree = FALSE,
     axes=c(1,3))
plot(res.hcpc, choice ="map", draw.tree = FALSE,
     axes=c(1,4))


# ?FactoMineR::plot.HCPC


# Remove labels and add cluster centers
plot(res.hcpc, choice ="map", draw.tree = FALSE,
     ind.names = FALSE, centers.plot = TRUE)
plot(res.hcpc, choice ="map", draw.tree = FALSE,
     ind.names = FALSE, centers.plot = TRUE,
     axes=c(1,3))
plot(res.hcpc, choice ="map", draw.tree = FALSE,
     ind.names = FALSE, centers.plot = TRUE,
     axes=c(1,4))


# View clusters according to PCs with fviz functions
fviz_cluster(res.hcpc,
             geom = "point")

# Contributions of original variables to PCA dimension 1
fviz_pca_contrib(res.pca)
# fviz_silhouette(res.pca)

# See how clusters are distributed accross the species
k.df <- cbind.data.frame(x$clust, pca.df_incl_trip_info[,c(1:4)])
# 
tab.k.sp <- table(k.df[,c(1,5)])
# 
# Numbers
tab.k.sp
rowSums(tab.k.sp)

# Proportions
#Within clusters
prop.table(tab.k.sp, 1)
#WIthin species
prop.table(tab.k.sp, 2)


# 8. ggplot figures of PCs + clusters ------
# Influenced by fviz_cluster in factoextra but extended to multiple PC dimensions + customised

# PC1 + PC2
dat.plot <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(dat.plot) <- c("pc1", "pc2", "pc3", "pc4", "pc5", "cluster")
dat.plot <- cbind.data.frame(dat.plot, pca.df_incl_trip_info[,c(1:4)])

p <- ggplot()
p <- p + geom_point(data = dat.plot, aes_string("pc1", 
                                                "pc2", color = "cluster", shape = "species"))
# Add convex hull thing
find_hull <- function(dat.plot) dat.plot[chull(dat.plot$pc1, dat.plot$pc2), ]
hulls <- ddply(dat.plot, "cluster", find_hull)

p + geom_polygon(data = hulls, aes_string("pc1", 
                                          "pc2", color = "cluster",
                                          fill = "cluster"), alpha = 0.3)

# PC1 + PC3
dat.plot <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(dat.plot) <- c("pc1", "pc2", "pc3", "pc4", "cluster")
dat.plot <- cbind.data.frame(dat.plot, pca.df_incl_trip_info[,c(1:4)])

p <- ggplot()
p <- p + geom_point(data = dat.plot, aes_string("pc1", 
                                                "pc3", color = "cluster", shape = "species"))
# Add convex hull thing
find_hull <- function(dat.plot) dat.plot[chull(dat.plot$pc1, dat.plot$pc3), ]
hulls <- ddply(dat.plot, "cluster", find_hull)

p + geom_polygon(data = hulls, aes_string("pc1", 
                                          "pc3", color = "cluster",
                                          fill = "cluster"), alpha = 0.3)



# PC1 + PC4
dat.plot <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(dat.plot) <- c("pc1", "pc2", "pc3", "pc4", "cluster")
dat.plot <- cbind.data.frame(dat.plot, pca.df_incl_trip_info[,c(1:4)])

p <- ggplot()
p <- p + geom_point(data = dat.plot, aes_string("pc1", 
                                                "pc4", color = "cluster", shape = "species"))
# Add convex hull thing
find_hull <- function(dat.plot) dat.plot[chull(dat.plot$pc1, dat.plot$pc4), ]
hulls <- ddply(dat.plot, "cluster", find_hull)

p + geom_polygon(data = hulls, aes_string("pc1", 
                                          "pc4", color = "cluster",
                                          fill = "cluster"), alpha = 0.3)


# PC2 + PC4
dat.plot <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(dat.plot) <- c("pc1", "pc2", "pc3", "pc4", "cluster")
dat.plot <- cbind.data.frame(dat.plot, pca.df_incl_trip_info[,c(1:4)])

p <- ggplot()
p <- p + geom_point(data = dat.plot, aes_string("pc2", 
                                                "pc4", color = "cluster", shape = "species"))
# Add convex hull thing
find_hull <- function(dat.plot) dat.plot[chull(dat.plot$pc2, dat.plot$pc4), ]
hulls <- ddply(dat.plot, "cluster", find_hull)

p + geom_polygon(data = hulls, aes_string("pc2", 
                                          "pc4", color = "cluster",
                                          fill = "cluster"), alpha = 0.3)




# Possible alternative with GGally::ggscatmat
# install.packages("GGally", type = "source")
# ?install.packages
library(GGally)
library(cowplot)
# 
# str(dat.plot)
# 
# ggscatmat(dat.plot, columns = 1:4, color="cluster", alpha=0.6)
# ?ggscatmat
# 
# ggpairs(dat.plot, mapping = aes(color = cluster, alpha = 0.5),
#         columns = c(1:4),
#         upper=list(continuous='blank'))
# 


p <- ggpairs(dat.plot, mapping = aes(color = cluster, alpha = 0.5),
             columns = c(1:5),
             upper=list(continuous='blank'))

p

# # ?geom_point
# # Some weird hack to include legend
# colidx <- c(1:4)
# for (i in 1:length(colidx)) {
#   
#   # Address only the diagonal elements
#   # Get plot out of plot-matrix
#   inner <- getPlot(p, i, i);
#   
#   # Add ggplot2 settings (here we remove gridlines)
#   inner <- inner + theme(panel.grid = element_blank()) +
#     theme(axis.text.x = element_blank())
#   
#   # Put it back into the plot-matrix
#   p <- putPlot(p, inner, i, i)
#   
#   for (j in 1:length(colidx)){
#     if((i==1 & j==1)){
#       
#       # Move the upper-left legend to the far right of the plot
#       inner <- getPlot(p, i, j)
#       inner <- inner + theme(legend.position=c(length(colidx)-0.25,0.50)) 
#       p <- putPlot(p, inner, i, j)
#     }
#     else{
#       
#       # Delete the other legends
#       inner <- getPlot(p, i, j)
#       inner <- inner + theme(legend.position="none")
#       p <- putPlot(p, inner, i, j)
#     }
#   }
# }
# ?ggsave
ggsave(p, file = "pca_clusters_newx.svg", width = 10, height = 10, units = "in")
ggsave(p, file = "pca_clusters_newx.png", width = 10, height = 10, units = "in")
ggsave(p, file = "pca_clusters_newx.pdf", width = 10, height = 10, units = "in")


# ?ggpairs

# 9. Output details for cluster mapping -----
# PC1 + PC2
clust.out <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(clust.out) <- c("pc1", "pc2", "pc3", "pc4", "pc5", "cluster")
clust.out <- cbind.data.frame(clust.out, pca.df_incl_trip_info[,c(1:4)])

dist <- NA
group <- NA
trip_id <- NA

for(i in 1:25){
  x <- ceiling(i/5) 
  id <- i + 5 -(x*5)
  dist[i] <- res.hcpc$desc.ind$para[[x]][id]
  trip_id[i] <- attr(res.hcpc$desc.ind$para[[x]][id], "names")
  group[i] <- x
}
paragons <- cbind.data.frame(group,trip_id,dist)


for(i in 1:25){
  x <- ceiling(i/5) 
  id <- i + 5 -(x*5)
  dist[i] <- res.hcpc$desc.ind$dist[[x]][id]
  trip_id[i] <- attr(res.hcpc$desc.ind$dist[[x]][id], "names")
  group[i] <- x
}
distincts <- cbind.data.frame(group,trip_id,dist)

# Is there any overlap?
any(paragons$trip_id %in% distincts$trip_id)
# NO

clust.out$paragon <- FALSE
clust.out$paragon[clust.out$trip_id %in% paragons$trip_id] <- TRUE

clust.out$distinct <- FALSE
clust.out$distinct[clust.out$trip_id %in% distincts$trip_id] <- TRUE
# sum(clust.out$paragon)

save(clust.out, file = "clusters_newx.RData")



# Compare numbers of trips by cluster by species ------

tab.k.sp <- table(clust.out[,c(10,6)])
# 
# Numbers
tab.k.sp
# tab.k.sp[1,2]
rowSums(tab.k.sp)

tab.k.sp.t <- as.data.frame(tab.k.sp)
str(tab.k.sp.t)

# Proportions
#Within clusters
prop.table(tab.k.sp, 2)

#WIthin species
tab.sp <- prop.table(tab.k.sp, 1)
tab.sp.t <- t(tab.sp)
tab.sp.t <- as.data.frame(tab.sp.t)
str(tab.sp.t)
names(tab.sp.t)[3] <- "perc_sp"

library(scales)

ggplot(tab.sp.t, aes( x= cluster, group = cluster)) + 
  geom_bar(aes(weight = perc_sp, fill = cluster)) + 
  facet_grid(~species) +
  scale_y_continuous(labels=percent) +
  labs(y = "Percent", fill="Cluser") 

ggsave("cluster.prop.new_subset_1.png", width = 7, height = 5)


# ?labels
#   scale_y_continuous(labels=perc_sp)
# ?geom_bar


# Use RVAideMemoire package
install.packages("RVAideMemoire")
library("RVAideMemoire")

# See: https://rdrr.io/cran/RVAideMemoire/man/fisher.multcomp.html
# Also: http://rcompanion.org/rcompanion/b_07.html
# Fisher's exact advised for smaller sample sizes (<1000), see:
#  http://www.biostathandbook.com/fishers.html

# Example
# 3-column contingency table: independence test
tab.cont2 <- as.table(matrix(c(25,10,12,6,15,14,9,16,9),ncol=3,dimnames=list(c("fair",
                                                                               "dark","russet"),c("blue","brown","green"))))

# Are clusters spread equally accross species?
fisher.test(tab.k.sp.t)

# Which clusterXspecies combinations are more/less than expected?
fisher.multcomp(tab.cont2)




# Alternatively can use fisher.test function in base R stats package
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fisher.test.html
fisher.test(Job)

# Or with simulated Monte Carlo p-value thing
# B number of replicates used
fisher.test(Job, simulate.p.value = TRUE, B = 1e5)


# Summarise variables by cluster (mean + sd) -------
trips.sub$cluster <- clust.out$cluster
vars <- c("coldist_max", "col_dist_median",
          "duration_s", "p_flight",
          "p_sea", "p_coast",
          "p_land", "p_landfill",
          "tortoisity", "sunrise_after_h",
          "sunset_after_h",
          "solarnoon_after_h",
          "sunrise_after_h_mid", "sunset_after_h_mid",
          "solarnoon_after_h_mid", "cluster" )
  
  
  
trips.sub.vars <- select(trips.sub, one_of(vars))

summar.tab <- trips.sub.vars %>% 
  group_by(cluster) %>% 
  summarise_each(funs(mean))

summar.tab.trans <- t(summar.tab)


summar.tab2 <- trips.sub.vars %>% 
  group_by(cluster) %>% 
  summarise_each(funs(sd))

summar.tab.trans2 <- t(summar.tab)

summar.tab.comb <- rbind.data.frame(summar.tab, summar.tab2)

write.csv(summar.tab.comb, file = "summary_cluster_var.csv")






# Bootstrapping the PCA ----------
# For each species to have equal influence on the drived PCAs we include
# equal numbers of foraging trips per species
# To make this robust, we resample many times and re-run the PCA







# summary(sample.trips)
pca.scores.list <- list()
pca.eigens.list <- list()

# Run PCA lots of times
# for(run_num in c(1:1000)){

for(run_num in c(1:1000)){
    
  
  # Take a subset of data
  samples <- lapply(trips.sp, function(x) sample(x, 60, replace = FALSE))
  trips.sample <- unlist(samples)
  # ?sample
  
  sample.trips <- pca.df_incl_trip_info$trip_id %in% trips.sample
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
eig.summary.df <- summarise(group_by(pca.eigens.df,
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
pca.scores.mean.df <- summarise(group_by(pca.scores.df,
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
                           pc10 = mean(Dim.10)
)

# Combine with details columns
pca.table <- merge(pca.scores.mean.df,
                   trips.sub[,c(1:4)],
                   by = "trip_id")
write.csv(pca.table, file = "pca.table.csv")

save(list(pca.table,eig.summary.df,
          pca.scores.list, pca.eigens.list))

# Bootstrapping the clustering ------
pca.table <- read.csv(file="pca.table.csv", row.names = NULL)
pca.table <- pca.table[,c(2:ncol(pca.table))]

# split trips between species
trips.sp <- split(trips.sub$trip_id, trips.sub$species)

nclust <- list()
inertia.gain <- list()
interia.gain.change <- list()

for(i in 1:10000){
  
  # Take a subset of data
  samples <- lapply(trips.sp, function(x) sample(x, 50, replace = TRUE))
  trips.sample <- unlist(samples)
  # ?sample
  
#   idx <- c(1:nrow(pca.table))
#   sample.trips <- vapply()
#     idx[ pca.table$trip_id == trips.sample]
#   # sup.inds <- c(1:nrow(pca.df_only))[!sample.trips]

  # Find rows with these trips  
  trip.row <- sapply(trips.sample ,function(x){which(pca.table$trip_id == x)})

#   # Check we've got what we think we've got!
#   all(trips.sample %in% pca.table[trip.row,1])
#   # Yes!
  
  # Run clustering algorithm
  res.hcpc <- HCPC(pca.table[trip.row, c(2:6)], nb.clust = -1,
                   min = 5, max = 20,
                   graph = FALSE)
  # ?HCPC
  # str(res.hcpc)
  
  q1 <- res.hcpc$call$t$inert.gain[c(1:18)]-res.hcpc$call$t$inert.gain[c(2:19)]
  q2 <- res.hcpc$call$t$inert.gain[c(2:19)]-res.hcpc$call$t$inert.gain[c(3:20)]
  q_change <- (q1-q2)/res.hcpc$call$t$inert.gain[c(2:19)]
#  
#    par(mfrow=c(3,1))
#   plot(res.hcpc, draw.tree = TRUE,choice = "bar")
#   plot(res.hcpc$call$t$inert.gain[1:20])
#   plot(q_change~c(2:19))
  
#   # where(sort(q_change)
#   q1[6]/q2[6]
#   q1[7]/q2[7]
#   
  q_order <- rev(sort(q_change))
  top_1 <- which(q_change == q_order[1]) + 1
  top_2 <- which(q_change == q_order[2]) + 1
  
  if(top_1 >3) top_clust <- top_1 else{
    top_clust <- top_2
  }
  
  # Optimal number of clusters
  nclust[[i]] <- c(top_1, top_2, top_clust)
  inertia.gain[[i]] <- res.hcpc$call$t$inert.gain[1:20]
  interia.gain.change[[i]] <- q_change
  
}

nclust.df <- do.call(rbind.data.frame, nclust)
names(nclust.df) <- c("top_1", "top_2", "top_over_3")

hist(nclust.df$top_over_3, breaks = 100)
median(nclust.df$top_over_3)
abline(v= median(nclust.df$top_over_3))
abline(v= mean(nclust.df$top_over_3), col="red")


sorted_top_over_3 <- sort(nclust.df$top_over_3)
df.loess <- cbind.data.frame(sorted_top_over_3,c(1:10000))
names(df.loess) <- c("clust_n", "ob_n")
loess.line <- loess(clust_n~ob_n, df.loess,span = 0.25)
z <- predict(loess.line, df.loess, se=TRUE)
df.loess$loess_predict <- z$fit

# ?loess
# 95% CI for true number of clusters
plot(sorted_top_over_3)
abline(v=c(250,2500, 5000, 7500, 9750))
abline(loess.line, col = "red")
points(df.loess$loess_predict~df.loess$ob_n, col = "red", type = "l")

median(sorted_top_over_3)
sorted_top_over_3[250]
# .025*10000
sorted_top_over_3[10000-250]
# 7 [4, 14]


# 50% interval
sorted_top_over_3[2500]
# .025*10000
sorted_top_over_3[7500]
# 7 [5, 9]

inertia.gain.df <- do.call(rbind.data.frame, inertia.gain)
names(inertia.gain.df) <- c(1:20)

inertia.gain.df.melt <- melt(inertia.gain.df,measure.vars = c(1:20))

interia.gain.change.df <- do.call(rbind.data.frame, interia.gain.change)
names(interia.gain.change.df) <- c(2:19)

interia.gain.change.df.melt <- melt(interia.gain.change.df,measure.vars = c(1:18))

colMeans(interia.gain.change.df)
# hist(interia.gain.change.df[,7], breaks = 50)


boxplot(interia.gain.change.df.melt$value~interia.gain.change.df.melt$variable,
        ylim = c(-0.5,0.5))
# ?boxplot


# See how inertia gain looks
boxplot(inertia.gain.df.melt$value~inertia.gain.df.melt$variable)



# Get cluster centroids -------
# Run as above many times contrained to 7 clusters
# Get means of centroid centres and PCA weights/ loadings etc...



# split trips between species
trips.sp <- split(trips.sub$trip_id, trips.sub$species)

# To run adapted NbClust function
source("NbClust_new_d_index_only.R")

nclus3 <- NULL
nclus4 <- NULL
nclus5 <- NULL

inertias <- list()

for(i in 1:10000){
  
  # Take a subset of data
  samples <- lapply(trips.sp, function(x) sample(x, 50, replace = TRUE))
  trips.sample <- unlist(samples)
 
  # Find rows with these trips  
  trip.row <- sapply(trips.sample ,function(x){which(pca.table$trip_id == x)})
  
  #   # Check we've got what we think we've got!
  #   all(trips.sample %in% pca.table[trip.row,1])
  #   # Yes!
#   
#   # Run clustering algorithm
#   res.hcpc <- HCPC(pca.table[trip.row, c(2:6)], nb.clust = 7,
#                    min = 5, max = 20,
#                    graph = TRUE)
#   
#   # Perform heirachial clustering on PCA scores
#   clusters <- hclust(dist(pca.table[trip.row, c(2:6)]),
#                      method = "ward.D2")
#   
#   # ?hclust
#   plot(clusters, hang = -5)
#   
  
  
  # Find optimal number of clusters using NbClust package
#   install.packages(c("NbClust"))
#   library(NbClust)
#   x <- NbClust(pca.table[trip.row, c(2:6)], distance = "euclidean",
#                method = "ward.D2", index = "frey", alphaBeale = 0.05,
#                min.nc = 3, max.nc = 20)
#   plot(x$All.index~c(3:20))
#   x$Best.nc
# NbClust  

#   res<-NbClust(pca.table[trip.row, c(2:6)], distance = "euclidean", min.nc=2, max.nc=20, 
#                method = "complete", index = "dindex")
#   
#   
  # need to source NbClust_new_d_index_only
  res <- NbClust_new(pca.table[trip.row, c(2:6)], distance = "euclidean", min.nc=2, max.nc=20, 
          method = "ward.D2", index = "dindex")
  # str(res)
  # If use d-index (inertia gain thing), need to implement some function to store the optimal number of clusters
  
  # Inertia plot
#       plot(c(2:20), res[[1]], tck = 0, type = "b", col = "red", 
#            xlab = expression(paste("Number of clusters ")), 
#            ylab = expression(paste("Dindex Values")))
  
  
#   plot(res[[2]], type ="b")
#   
  
  #inertia values (2-20 clusters)
  inertias[[i]] <- res[[1]]
  
  # Optimal cluster

  # Best more than 2 (3 or greater)
  nclus3[i] <- which.max(res[[2]][3:20])  + 2
  
  
  # Best more than 3 (4 or greater)
  nclus4[i] <- which.max(res[[2]][4:20])  + 3
  
  # Best more than 4 (5 or greater)
  nclus5[i] <- which.max(res[[2]][5:20])  + 4
  
  # res$All.index
  # nclus[i] <- res$Best.nc[1]
#   res$Best.partition
#   
#   
#   # ?NbClust
#   plot(res$All.index~c(3:20))
#   
#   # ?dist
#   str(clusters)
# 
#   res.dist <- get_dist((pca.table[trip.row, c(2:6)]), stand = TRUE, method = "pearson")
#   
#   fviz_dist(res.dist, 
#             gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#   
#   
#   res.hcpc$desc.axes
#   
#   # Get centroid-centres
#   res.hcpc$call$t
#   
#   # ?HCPC
#   str(res.hcpc)
 
  
}

# nclus3
# nclus4
# min == 4
hist(nclus4, breaks = 100)
hist(nclus3, breaks = 100)
hist(nclus5, breaks = 100)

mean(nclus4)
median(nclus4)
median(nclus5)
mean(nclus5)

inertia.gain.df <- do.call(rbind.data.frame, inertias)
names(inertia.gain.df) <- c(2:20)

# library(reshape2)
interia.gain.change.df.melt <- melt(inertia.gain.df,measure.vars = c(1:19))

boxplot(interia.gain.change.df.melt$value~interia.gain.change.df.melt$variable)

# Median, 95% CI and 50% CI
sort(nclus5)[c(250,2500, 5000, 7500, 9750)]

sort(nclus3)[c(250,2500, 5000, 7500, 9750)]




# Apply to all data ------
# Use k-means approach based on cluster centres + record distances
# See code below (end of file)
#





# 
# 
# q1 <- inertia.gain.df[,c(1:18)]-inertia.gain.df[,c(2:19)]
# q2 <- inertia.gain.df[,c(2:19)]-inertia.gain.df[,c(3:20)]
# q_change <- q1/q2
# inertia.gain.chang.df.melt <- melt(q_change,measure.vars = c(1:18))
# boxplot(inertia.gain.chang.df.melt$value~inertia.gain.chang.df.melt$variable,
#         ylim = c(0,10))
# 
# # Do a nice plot with medians + 95% CI (bootstraped - i.e. 95% quartile range)
# 
# quantile(q_change[,5], c(0.05,0.5,0.95))
# 
# 
# 
# # ?melt
# 
# median(nclust)
# hist(nclust, breaks = 100)
# 
# sum(nclust > 6)/length(nclust)
# 
# 
# nclus.min2 <- nclust
# 
# nclus.min3 <- nclust
# 
# nclust.min4 <- nclust
# hist(nclust.min4)
# 
# # See what clusters each trip belongs to along with variables
# x <- res.hcpc$data.clust
# 
# 
# # See what variables are important to different clusters and what defines clusters
# res.hcpc$desc.var
# 
# 
# # See also NS variables
# df.tab <- catdes(x, num.var = ncol(x), proba = 1)
# df.tab
# 
# 
# # See how clusters correspond to PCA axes
# res.hcpc$desc.axes
# 
# 
# # See which trips are closest to cluster centres, the 'paragon' and furtherst from any other cluster (but still within this cluster, the most specific) under 'dist'
# res.hcpc$desc.ind






# Allocating clusters from cluster centroids (minimising euclidian distance)
# create a simple data set with two clusters
set.seed(1)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
x_new <- rbind(matrix(rnorm(10, sd = 0.3), ncol = 2),
               matrix(rnorm(10, mean = 1, sd = 0.3), ncol = 2))
colnames(x_new) <- c("x", "y")

cl <- kmeans(x, centers=2)




clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  clust <- max.col(-t(tmp))  # find index of min distance
  return(c(clust, max(tmp)))
}

clusters()
clusters(cbind.data.frame(0,0), cl[["centers"]])
