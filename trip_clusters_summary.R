# Summarising information about foraging trip clusters

# Load in foraging trip + cluster data ------
load("cluster_data_detailed.RData")

# Required packages -------

# Handling data
# Should load plyr first to avoid conflicts with dplyr
library(plyr)
library("dplyr")

# Rearrange df ------
dup.col <- duplicated(names(clust.tab))

# Only keep first instances of columns
clust.df <- clust.tab[,!dup.col]

# Functions for cicurlar mean and dispertion (r vector) -----
# # Using code from comment below this http://stackoverflow.com/a/32404360/1172358
# # Mean hour
# conv <- 2*pi/24 ## hours -> radians
# (24+Arg(mean(exp(conv*(hours.x)*1i)))/conv)%%24
# 
# # Dispertion (0 - equally dispersed, 1 - all the same)
# Mod(mean(exp(conv*(hours.x)*1i)))

circ.mean <- function(x){
  # First re-scale to 0, 24 (from -12, +12)
  hours.x <- x %% 24
  
  # For conversion to radians
  conv <- 2*pi/24 ## hours -> radians
  
  # Circ.mean
  (24+Arg(mean(exp(conv*(hours.x)*1i)))/conv)%%24
}


circ.disp <- function(x){
  # First re-scale to 0, 24 (from -12, +12)
  hours.x <- x %% 24
  
  # For conversion to radians
  conv <- 2*pi/24 ## hours -> radians
  
  # dispertion (0-1, with 1 fully concentrated)
  Mod(mean(exp(conv*(hours.x)*1i)))
}

# scale 0-24 to -12, +12
time.neg.pos <- function(x){
  int.fun <- function(xi){
    if(xi>12) return(xi-24) else return(xi)
  }
  z <- sapply(x, int.fun)
}

# 
# # Test these
# # Uniform distribution
# x.disp <- sample(c(-1200:1200), 1000)/100
# hist(x.disp, breaks = 100)
# # ?qqnorm
# mean(x.disp)
# circ.mean(x.disp)
# circ.disp(x.disp)
# 
# # Concentrated distribution
# x.conc <- rnorm(1000, mean = 3, sd = 2) %%24
# hist(x.conc, breaks = 100)
# mean(x.conc)
# circ.mean(x.conc)
# circ.disp(x.conc)


# Tabular summary -------
names(clust.tab)

# Use dplyr
var.summary.df <- dplyr::summarise(group_by(clust.df,
                                            cluster_fac),
                                   
#                                    Trip parameters
#                                    Movement/ Behaviour
#                                    Distance from colony (maximum)
coldist_max_km_mean = mean(coldist_max/1000),
coldist_max_km_sd = sd(coldist_max/1000),
#                                    Distance from colony (median)
col_dist_median_km_mean = mean(col_dist_median/1000),
col_dist_median_km_sd = sd(col_dist_median/1000),
#                                    Duration
duration_h_mean = mean(duration_s/60/60),
duration_h_sd = sd(duration_s/60/60),
#                                    Tortuosity
tortoisity_mean = mean(tortoisity),
tortoisity_sd = sd(tortoisity),
#                                    % Flight
p_flight_mean = mean(p_flight),
p_flight_sd = sd(p_flight),
# )
# range(clust.df$tortoisity)
#                                    
#                                    Habitat
#                                    % Land
p_land_mean = mean(p_land),
p_land_sd = sd(p_land),
#                                    % Sea
p_sea_mean = mean(p_sea),
p_sea_sd = sd(p_sea),
#                                    % Coast
p_coast_mean = mean(p_coast),
p_coast_sd = sd(p_coast),
#                                    % Landfill
p_landfill_mean = mean(p_landfill),
p_landfill_sd = sd(p_landfill),
#                                    % Water (terrestrial)
p_water_50m_mean = mean(p_water_50m),
p_water_50m_sd = sd(p_water_50m),
# )
#                                    
#                                    Time of day
#                                    Sunrise at departure time (proximity to)
# range(clust.df$sunrise_after_h)
# -2%%24
sunrise_after_h_mean = time.neg.pos(circ.mean(sunrise_after_h)),
sunrise_after_h_conc = circ.disp(sunrise_after_h),
# )
#                                    Sunset at departure time (proximity to)
sunset_after_h_mean = time.neg.pos(circ.mean(sunset_after_h)),
sunset_after_h_conc = circ.disp(sunset_after_h),
# )
#                                    Solar noon at departure time (proximity to)
solarnoon_after_h_mean = time.neg.pos(circ.mean(solarnoon_after_h)),
solarnoon_after_h_conc = circ.disp(solarnoon_after_h),
#                                    Sunrise at trip midpoint (proximity to)
sunrise_after_h_mid_mean = time.neg.pos(circ.mean(sunrise_after_h_mid)),
sunrise_after_h_mid_conc = circ.disp(sunrise_after_h_mid),
#                                    Sunset at trip midpoint (proximity to)
sunset_after_h_mid_mean = time.neg.pos(circ.mean(sunset_after_h_mid)),
sunset_after_h_mid_conc = circ.disp(sunset_after_h_mid),
#                                    Solar noon at trip midpoint (proximity to)
solarnoon_after_h_mid_mean = time.neg.pos(circ.mean(solarnoon_after_h_mid)),
solarnoon_after_h_mid_conc = circ.disp(solarnoon_after_h_mid),
# )
#                                    
#                                    Time at departure*
# Could use: solarnoon_after_h (-12, 12), change to 0, 24,
# if 0, depart at noon, if 12, at midnight, if 18, at 6 am, etc...
solar_time_dept_mean = time.neg.pos(circ.mean(solarnoon_after_h))+12,
solar_time_dept_conc = circ.disp(solarnoon_after_h),
#                                      Time at midpoint*
#                                      Hours since sunrise at departure time**
solar_time_mid_mean = time.neg.pos(circ.mean(solarnoon_after_h_mid))+12,
solar_time_mid_conc = circ.disp(solarnoon_after_h_mid),

#                                      
#                                      Principle components
#                                    PC 1
#                                    PC 2
#                                    PC 3
#                                    PC 4
#                                    PC 5
                                   
                                   
                                   
                                   pc1_mean = mean(pc1),
                                   pc1_sd = sd(pc1),
                                   pc2_mean = mean(pc2),
                                   pc2_sd = sd(pc2),
                                   pc3_mean = mean(pc3),
                                   pc3_sd = sd(pc3),
                                   pc4_mean = mean(pc4),
                                   pc4_sd = sd(pc4),
                                   pc5_mean = mean(pc5),
                                   pc5_sd = sd(pc5)
                                   

)


#


# Reformat for printed table ------
var.sum.df.t <- as.data.frame(t(var.summary.df))

row.names(var.sum.df.t)


n.fun <- function(x){
  x <- mapply(as.character, x)
  x <- mapply(as.numeric, x)
  return(x)
}

# ?mapply
# n.fun(var.sum.df.t[2,])
row.m <- seq(2,8,2)
row.sd <- seq(3,9,2)
x <- paste(trimws(format(round(n.fun(var.sum.df.t[row.m,]), 2), nsmall = 2),
             which = "left"), " ±",
      trimws(format(round(n.fun(var.sum.df.t[row.sd,]), 2), nsmall = 2), which = c("left")),
      "", sep = "")
x1 <- matrix(x,ncol = 7)

row.m <- seq(10,20,2)
row.sd <- seq(11,21,2)

trimws(format(round(c(1, 0.1, 0.001, 0.0001, 0.00001), 2), nsmall = 2),
              which = "left")


x <- paste(trimws(format(round(n.fun(var.sum.df.t[row.m,])*100, 1), nsmall = 1),
                  which = "left"), " ±",
           trimws(format(round(n.fun(var.sum.df.t[row.sd,])*100, 1), nsmall = 1), which = c("left")),
           "", sep = "")
x2 <- matrix(x,ncol = 7)

row.m <- seq(22,36,2)
row.sd <- seq(23,37,2)

x <- paste(trimws(format(round(n.fun(var.sum.df.t[row.m,]), 1), nsmall = 1),
                  which = "left"), " (",
           trimws(format(round(n.fun(var.sum.df.t[row.sd,]), 2), nsmall = 2), which = c("left")),
           ")", sep = "")
x3 <- matrix(x,ncol = 7)
x3

row.m <- seq(38,46,2)
row.sd <- seq(39,47,2)
x <- paste(trimws(format(round(n.fun(var.sum.df.t[row.m,]), 2), nsmall = 2),
                  which = "left"), " ±",
           trimws(format(round(n.fun(var.sum.df.t[row.sd,]), 2), nsmall = 2), which = c("left")),
           "", sep = "")
x4 <- matrix(x,ncol = 7)
x4


all.x <- rbind.data.frame(x1,x2,x3,x4)
names(all.x) <- paste("clust", c(1:7), sep = "_")

# Output to file
write.csv(all.x, file = "var_summary.csv")



# Statistics to compare -----
