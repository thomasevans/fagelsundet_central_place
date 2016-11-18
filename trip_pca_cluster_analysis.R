# Echo of PCA thing



# 1. Required packages ------

# Loading in Access DB data
library("RODBC")

# Handling data
library("dplyr")
library(plyr)
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

hist(trips.sub$sunrise.prox)
plot(trips.sub$sunrise.prox~trips.sub$sunrise_after_h)

# Time before/ after sunset
trips.sub$sunset.prox <- cos((trips.sub$sunset_after_h/12)*pi)



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

# Time of day variables
trips.sub$sunrise.prox_logit <- logit((trips.sub$sunrise.prox+1)/2)
hist(trips.sub$sunrise.prox_logit)

trips.sub$sunset.prox_logit <- logit((trips.sub$sunset.prox+1)/2)
hist(trips.sub$sunset.prox_logit)

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
          "sunrise.prox", "sunset.prox"
)
# vars  %in% names(trips.sub)
pca.df_incl_trip_info <- select(trips.sub, one_of(vars))
row.names(pca.df_incl_trip_info) <- trips.sub$trip_id

pca.df_only <- select(pca.df_incl_trip_info, -c(1:4))
row.names(pca.df_only) <- trips.sub$trip_id


# Inspect data (scaled)
df <- scale(pca.df_only)

summary(df)

chart.Correlation(df, histogram=TRUE, pch=19)
# warnings()

# 7. Perform PCA etc -----
# Compute principal component analysis
res.pca <- PCA(pca.df_only, ncp = 4, graph=FALSE)

# Barplot of eigenvalues to choose number of PCA dimensions
barplot(res.pca$eig[,1], main = "Eigenvalues",
        names.arg = paste("Dim", 1:nrow(res.pca$eig), sep = ""))
abline(h=1, lwd = 1.5, lty = 2)
# First 4 PCAs have positive eigen values

# View how variables correlate with PCA components
png("PCA_variables_polar_plots.png",
    width = 4, height = 12, units = "in",
    res = 300)

par(mfrow=c(3,1))
# Can reduce number of variables displayed by limiting cosines
plot(res.pca, choix = "var", axes = c(1, 2), lim.cos2.var = 0.4)
plot(res.pca, choix = "var", axes = c(1, 3), lim.cos2.var = 0.4)
plot(res.pca, choix = "var", axes = c(1, 4), lim.cos2.var = 0.4)
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
df.tab <- catdes(x, num.var = 12, proba = 1)
df.tab

# See how clusters correspond to PCA axes
res.hcpc$desc.axes


dat.plot <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(dat.plot) <- c("pc1", "pc2", "pc3", "pc4", "cluster")

# See also NS ones
catdes(dat.plot, num.var = 5, proba = 1)


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
names(dat.plot) <- c("pc1", "pc2", "pc3", "pc4", "cluster")
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
             columns = c(1:4),
             upper=list(continuous='blank'))

# ?geom_point
# Some weird hack to include legend
colidx <- c(1:4)
for (i in 1:length(colidx)) {
  
  # Address only the diagonal elements
  # Get plot out of plot-matrix
  inner <- getPlot(p, i, i);
  
  # Add ggplot2 settings (here we remove gridlines)
  inner <- inner + theme(panel.grid = element_blank()) +
    theme(axis.text.x = element_blank())
  
  # Put it back into the plot-matrix
  p <- putPlot(p, inner, i, i)
  
  for (j in 1:length(colidx)){
    if((i==1 & j==1)){
      
      # Move the upper-left legend to the far right of the plot
      inner <- getPlot(p, i, j)
      inner <- inner + theme(legend.position=c(length(colidx)-0.25,0.50)) 
      p <- putPlot(p, inner, i, j)
    }
    else{
      
      # Delete the other legends
      inner <- getPlot(p, i, j)
      inner <- inner + theme(legend.position="none")
      p <- putPlot(p, inner, i, j)
    }
  }
}
# ?ggsave
ggsave(p, file = "pca_clusters.svg", width = 10, height = 10, units = "in")
ggsave(p, file = "pca_clusters.png", width = 10, height = 10, units = "in")
ggsave(p, file = "pca_clusters.pdf", width = 10, height = 10, units = "in")


# ?ggpairs

# 9. Output details for cluster mapping -----
# PC1 + PC2
clust.out <- cbind.data.frame(res.pca$ind$coord,res.hcpc$data.clust$clust )
names(clust.out) <- c("pc1", "pc2", "pc3", "pc4", "cluster")
clust.out <- cbind.data.frame(clust.out, pca.df_incl_trip_info[,c(1:4)])

dist <- NA
group <- NA
trip_id <- NA

for(i in 1:30){
  x <- ceiling(i/5) 
  id <- i + 5 -(x*5)
  dist[i] <- res.hcpc$desc.ind$para[[x]][id]
  trip_id[i] <- attr(res.hcpc$desc.ind$para[[x]][id], "names")
  group[i] <- x
}
paragons <- cbind.data.frame(group,trip_id,dist)


for(i in 1:30){
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

save(clust.out, file = "clusters.RData")



# Compare numbers of trips by cluster by species ------

tab.k.sp <- table(clust.out[,c(9,5)])
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

ggsave("cluster.prop.png", width = 7, height = 5)


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
trips.sub$cluster <- x$clust
vars <- names(trips.sub)[c(5,7,12,19:23,33,37:38,40:41,53)]
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
