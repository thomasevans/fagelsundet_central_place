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

# Stats
library(lme4)
library(lmerTest)
library(multcomp)

names(clust.df)
clust.df.stats <- clust.df

clust.df.stats$cluster_fac <- as.factor(clust.df.stats$cluster_fac)
clust.df.stats$ring_number <- as.factor(clust.df.stats$ring_number)
clust.df.stats$species <- as.factor(clust.df.stats$species)

levels(clust.df.stats$cluster_fac)
levels(clust.df.stats$ring_number)
levels(clust.df.stats$species)

var.txt <- paste("log_coldist_max")


test.var.fun <- function(var.txt = NA, data = clust.df.stats){
  # Test example:
  formular <- as.formula(paste(var.txt,"~ cluster_fac + (1|ring_number)", sep = ""))
  mod.test <- lmer(formular, data = data)
  # ?parse
  # 
  # summary(mod.test)
  # 
  # anova(mod.test, mod.test.int)
  # 
  # Uses Satterthwaite approximation, which is implemented in the lmerTest
  # See: http://mindingthebrain.blogspot.se/2014/02/three-ways-to-get-parameter-specific-p.html
  # Have to have lmerTest loaded after lme4
  fact.sig <- anova(mod.test)
  # str(fact.sig)
  # f
  f <- fact.sig$F.value
  # p value
  p <- fact.sig$'Pr(>F)'
  
  # # extract coefficients
  # coefs <- data.frame(coef(summary(mod.test)))
  # # get Satterthwaite-approximated degrees of freedom
  # coefs$df.Satt <- coef(summary(mod.test))[, 3]
  # # get approximate p-values
  # coefs$p.Satt <- coef(summary(mod.test))[, 5]
  # coefs
  
  # Post-hoc test, see: http://stats.stackexchange.com/questions/237512/how-to-perform-post-hoc-test-on-lmer-model
  # uses multcomp package
  post.hoc.mod <-summary(glht(mod.test, linfct = mcp(cluster_fac = "Tukey")), test = adjusted("holm"))
  # Which groups are different?
  ph <- post.hoc.mod$test$pvalues <0.05
  
  x <- list(f, p, ph)
  names(x) <- c("F", "p", "p_groups")
  x
}

test.var.fun("log_coldist_max")
test.var.fun("sunrise.prox")

vars.to.test <- c("sunrise.prox", "sunset.prox",  "solarnoon.prox",  
                  "sunrise.prox.mid", "sunset.prox.mid",  "solarnoon.prox.mid",            
                  "p_flight_logit",   "p_land_logit", "p_sea_logit", 
                  "p_coast_logit",    "p_landfill_logit", "p_water_20m_logit",             
                  "p_water_50m_logit", "log_coldist_max",  "log_col_dist_median"   ,        
                  "log_duration_s",   "log_tortoisity",
                  "pc1", "pc2",
                  "pc3", "pc4", "pc5")

test.ls <- list()

for(i in 1:length(vars.to.test)){
  test.ls[[i]] <- test.var.fun(var.txt = vars.to.test[i])
}


test.ls[[1]]




# Diversity/ specialism index - Eveness - Hill numbers ------
# Based on Jost, L. (2007). Partitioning Diversity into Independent Alpha and Beta Components. Ecology 88, 2427–2439.
# Though most familiar in ecology for analysis of biodiversity, same measures may be used
# to assess eveness/ diversity in other cases, here to look at the distribution of different
# types of foraging trip within and between species. Thus providing a measure of generalism
# /specialism, at the individual level (aka. alpha diversity) and the species level (
# aka. beta diversity)


# Load require package
# install.packages("vegetarian")
library(vegetarian)


# Make a table of clusters per species
clust.sp.table <- table(clust.df$species,clust.df$cluster_fac)


clust.ind.hg.table <- table(clust.df$ring_number[clust.df$species == "Larus argentatus"],clust.df$cluster_fac[clust.df$species == "Larus argentatus"])

clust.ind.gbbg.table <- table(clust.df$ring_number[clust.df$species == "Larus marinus"],clust.df$cluster_fac[clust.df$species == "Larus marinus"])

clust.ind.cg.table <- table(clust.df$ring_number[clust.df$species == "Larus canus"],clust.df$cluster_fac[clust.df$species == "Larus canus"])

clust.ind.lbbg.table <- table(clust.df$ring_number[clust.df$species == "Larus fuscus"],clust.df$cluster_fac[clust.df$species == "Larus fuscus"])



# Apha diversity ---
d(clust.ind.hg.table, lev = "alpha", boot = TRUE)
d(clust.ind.cg.table, lev = "alpha", boot = TRUE)
d(clust.ind.lbbg.table, lev = "alpha", boot = TRUE)
d(clust.ind.gbbg.table, lev = "alpha", boot = TRUE)

# Beta diversity ----
d(clust.ind.hg.table, lev = "beta", boot = TRUE)
d(clust.ind.cg.table, lev = "beta", boot = TRUE)
d(clust.ind.lbbg.table, lev = "beta", boot = TRUE)
d(clust.ind.gbbg.table, lev = "beta", boot = TRUE)


# Gamma diversity ----
d(clust.ind.hg.table, lev = "gamma", boot = TRUE)
d(clust.ind.cg.table, lev = "gamma", boot = TRUE)
d(clust.ind.lbbg.table, lev = "gamma", boot = TRUE)
d(clust.ind.gbbg.table, lev = "gamma", boot = TRUE)


# MacArthur's homogeneity measure (standardized) -----
M.homog(clust.ind.hg.table, std = TRUE, boot = TRUE)
M.homog(clust.ind.cg.table, std = TRUE, boot = TRUE)
M.homog(clust.ind.lbbg.table, std = TRUE, boot = TRUE)
M.homog(clust.ind.gbbg.table, std = TRUE, boot = TRUE)


# Overlaps ------
# HG vs CG
x <- sim.groups(clust.ind.hg.table,
           clust.ind.cg.table,
           q = 1,
           labels=TRUE,boot=FALSE)
# HG
mean(x[[1]])
sd(x[[1]])

# CG
mean(x[[2]])
sd(x[[2]])

# HG vs CG
mean(x[[3]])
sd(x[[3]])

# HGself vs. HG/CG
t.test(x[[1]],x[[3]])


# HG vs LBBGG
x <- sim.groups(clust.ind.hg.table,
                clust.ind.lbbg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# HG
mean(x[[1]])
sd(x[[1]])

# LBBG
mean(x[[2]])
sd(x[[2]])

# HG vs LBBG
mean(x[[3]])
sd(x[[3]])

# HGself vs. HG/LBBG
t.test(x[[1]],x[[3]])

# HG vs GBBG
x <- sim.groups(clust.ind.hg.table,
                clust.ind.gbbg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# HG
mean(x[[1]])
sd(x[[1]])

# GBBG
mean(x[[2]])
sd(x[[2]])

# HG vs GBBG
mean(x[[3]])
sd(x[[3]])

# HGself vs. HG/GBBG
t.test(x[[1]],x[[3]])

mean(c(0.57,0.42,0.72))


# CG vs LBBGG
x <- sim.groups(clust.ind.cg.table,
                clust.ind.lbbg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# CG
mean(x[[1]])
sd(x[[1]])

# LBBG
mean(x[[2]])
sd(x[[2]])

# CG vs LBBG
mean(x[[3]])
sd(x[[3]])

# CGself vs. CG/LBBG
t.test(x[[1]],x[[3]])

# CG vs GBBGG
x <- sim.groups(clust.ind.cg.table,
                clust.ind.gbbg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# CG
mean(x[[1]])
sd(x[[1]])

# GBBG
mean(x[[2]])
sd(x[[2]])

# CG vs GBBG
mean(x[[3]])
sd(x[[3]])

# CGself vs. CG/GBBG
t.test(x[[1]],x[[3]])



# CG vs HG
x <- sim.groups(clust.ind.cg.table,
                clust.ind.hg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# CG
mean(x[[1]])
sd(x[[1]])

# CG
mean(x[[2]])
sd(x[[2]])

# CG vs CG
mean(x[[3]])
sd(x[[3]])

# CGself vs. CG/CG
t.test(x[[1]],x[[3]])

mean(0.57,0.54,0.46)
t.test(c(0.57,0.54,0.46), x[[1]])

# LBBG vs GBBGG
x <- sim.groups(clust.ind.lbbg.table,
                clust.ind.gbbg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# LBBG
mean(x[[1]])
sd(x[[1]])

# GBBG
mean(x[[2]])
sd(x[[2]])

# LBBG vs GBBG
mean(x[[3]])
sd(x[[3]])

# CGself vs. LBBG/GBBG
t.test(x[[1]],x[[3]])



# LBBG vs CG
x <- sim.groups(clust.ind.lbbg.table,
                clust.ind.cg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# LBBG
mean(x[[1]])
sd(x[[1]])

# CG
mean(x[[2]])
sd(x[[2]])

# LBBG vs CG
mean(x[[3]])
sd(x[[3]])

# CGself vs. LBBG/CG
t.test(x[[1]],x[[3]])




# LBBG vs HG
x <- sim.groups(clust.ind.lbbg.table,
                clust.ind.hg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# LBBG
mean(x[[1]])
sd(x[[1]])

# HG
mean(x[[2]])
sd(x[[2]])

# LBBG vs HG
mean(x[[3]])
sd(x[[3]])

# CGself vs. LBBG/HG
t.test(x[[1]],x[[3]])

mean(0.42,0.54,0.27)




# GBBG vs HG
x <- sim.groups(clust.ind.gbbg.table,
                clust.ind.hg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# GBBG
mean(x[[1]])
sd(x[[1]])

# HG
mean(x[[2]])
sd(x[[2]])

# GBBG vs HG
mean(x[[3]])
sd(x[[3]])

# CGself vs. GBBG/HG
t.test(x[[1]],x[[3]])

# mean(0.42,0.54,0.27)



# GBBG vs CG
x <- sim.groups(clust.ind.gbbg.table,
                clust.ind.cg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# GBBG
mean(x[[1]])
sd(x[[1]])

# CG
mean(x[[2]])
sd(x[[2]])

# GBBG vs CG
mean(x[[3]])
sd(x[[3]])

# CGself vs. GBBG/CG
t.test(x[[1]],x[[3]])



# GBBG vs LBBG
x <- sim.groups(clust.ind.gbbg.table,
                clust.ind.lbbg.table,
                q = 1,
                labels=TRUE,boot=FALSE)
# GBBG
mean(x[[1]])
sd(x[[1]])

# LBBG
mean(x[[2]])
sd(x[[2]])

# GBBG vs LBBG
mean(x[[3]])
sd(x[[3]])

# CGself vs. GBBG/LBBG
t.test(x[[1]],x[[3]])

mean(c(0.72,0.46,0.27))

mean(c(0.57,0.54,0.46))

mean(c(0.57,0.42,0.72))

#



