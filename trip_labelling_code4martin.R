

# Dummy data
nest_dist <- c(0,40,30,500,600,2000, 2200, 2180, 2700, 1400,390,20, 10, 5,
               0,40,30,500,600,1800, 2800, 3500, 2600, 2200, 2180, 2700, 1400,390,20, 10, 5)

# See how this looks
plot(nest_dist, type = "b")
# Add cut-off line
abline(h=50, col = "red")

# So here the trip would run from the 4th to the 11th point

# Boolean (TRUE/FALSE vector, any point that is more than 50 m from the nest is labelled as true)
on_trip <- nest_dist>50

# See this:
on_trip

# Number of points:
n_points <- length(nest_dist)

# Weird thing to label points with number indicating:
# 0 - not on trip
# 3 - last point at nest before trip
# 4 - first point on a trip
# 9 - a middle point from a trip
# 6 - final point on a trip
# 5 - fist point back at the colony

point_type <- on_trip[2:(n_points-1)]+
  (on_trip[3:(n_points)]*3)+
  (on_trip[1:(n_points-2)])*5
# as use next and previous location can't calculate for the first and last point, I just set these to zero
point_type <- c(0, point_type, 0)


# Trip IDs - loop through above point_type thing
# Null value (0) vector for trip number
trip_id <- rep(0,n_points)
# Keep a count of what the current trip is
trip_count <- 0

for(i in 1:n_points){
  # If it's on a trip (not at the nest)
  if(point_type[i] %in% c(4,9,6)){
    if(point_type[i] == 4){
      # if first point of trip, increment trip_id by 1
      trip_count <- trip_count + 1
    }
    # Label with current trip number
    trip_id[i] <- trip_count
  }  
}

# see what this looks like:
trip_id

# Plot it, colour by trip_id:
plot(nest_dist, type = "b", col = as.numeric(trip_id+2))
# Add cut-off line
abline(h=50, col = "black")

# Summarise some stuff about 'trips
library(dplyr)

# Put together into a dataframe
trip.df <- cbind.data.frame(nest_dist,on_trip,point_type,trip_id)

# Summarise some basic things:
trip_summary.df <- dplyr::summarise(group_by(trip.df,
                                             trip_id),
                                   
                                    # distances
                                    dist_max = max(nest_dist),
                                    dist_mean = mean(nest_dist),
                                    n_points = n(),
                                    
                                    dist_first = first(nest_dist),
                                    dist_last = last(nest_dist)
                                    
                                    # You can add all sort of things here
                                    # Min and max are useful, as are fist(), and last()
                                    # e.g. can use to get start time and end time
)

# See this:
trip_summary.df

# You'd want to remove the '0' trip, as that's nonsence, just all the gaps between trips
