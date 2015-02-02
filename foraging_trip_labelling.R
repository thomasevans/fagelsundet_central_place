



# 1. Get data -----


# 1. GPS point data (device_info_serial, date_time, ring_number)
# 2. Distance from colony data (device_info_serial, date_time, on_trip)
# 3. Connect above (i.e. extract as single SQL query)
# 4. Make sure in order (device_info_serial, then date_time)



# 2. Go through all points (for loop) -----
# 1. Number points by foraging trip number (NA if not a trip)
# 2. If same device, as on_trip, same as last point etc... (like labelling flights)



# 3. Output new table   ------
# 
# 1. device_info_serial
# 2. date_time
# 3. trip_id

