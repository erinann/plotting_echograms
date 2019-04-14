library(tidyverse)
library(lubridate)
#library(naniar)
library(tictoc)
library(geosphere)

# To Dos
# Get distance to SB and recode GPS distance to SB distance
# Bring in XBT data (another file) and contour


# Known ISSUES - pings to drop
# - Bad Lat/Lon
# - Bad depth



#loading, clearning, checking tracklines, and combining 38 and 120.
# Currently only working with 38 kHz to get the plotting
data_raw <- read_csv("data/SB_038.sv.csv",
                 skip = 1, col_names = FALSE,
                 col_types = cols(X4 = col_datetime(format = "%Y-%m-%d"),
                                  X5 = col_time(format = "%H:%M:%S"))) 

nb_rows <- nrow(data_raw)

data <- data_raw %>% 
  rowid_to_column(var = "row_id") %>% 
  filter(between(row_id, 3, (nb_rows - 2))) # First few and last few pings usually are problem pings. Removing them.

meta_raw <- data %>% 
  select(row_id, Ping_index = X1, 
         Distance_gps = X2, Distance_vl = X3,
         Ping_date = X4, Ping_time = X5, 
         Latitude = X7, Longitude = X8,
         Depth_start = X9, Depth_stop = X10,
         Range_start = X11, Range_stop = X12,
         Sample_count = X13) %>% 
  mutate(DT = Ping_date + Ping_time) 

# Making ping index for bottom data
ping_index <- select(meta_raw, Ping_index)

# Both the bottom line and ping data are collected at the same time (from the same echogram). 
# These two datasets can be combined without a join if no data munging has occured yet.
bottom <- read_csv("data/bottom.line.csv",
                   col_types = cols(Ping_date = col_datetime(format = "%Y-%m-%d"))) %>% 
  mutate(DT = Ping_date + Ping_time) %>% 
  rowid_to_column(var = "row_id") %>% 
  filter(between(row_id, 3, (nb_rows - 2))) %>% # First few and last few pings usually are problem pings. Removing them.
  bind_cols(ping_index)

# bad depth points.
bad_dpth_idx <- bottom %>% 
  filter(Depth < 0) %>% 
  select(row_id) %>% 
  pull()

# bad location points
bad_loc_idx <- meta_raw %>% 
  filter(Longitude == 999.0) %>% 
  select(row_id) %>% 
  pull()

# Adding Depth to the ping data
meta_raw_2 <- meta_raw %>% 
  left_join(bottom, by = "Ping_index") %>%
  select(row_id = row_id.x, Ping_index, Distance_gps,
         Latitude = Latitude.x, Longitude = Longitude.x, 
         Depth, DT_1 = DT.x, DT_2 = DT.y)

#Quick check nothing is messed up or backwards
sum(meta_raw_2$DT_1 - meta_raw_2$DT_2)


meta <- meta_raw_2 %>% 
  select(-DT_2) %>% 
  rename(DT = DT_1) %>% 
  filter(!row_number() %in% c(bad_dpth_idx, bad_loc_idx)) %>% 
  mutate(dist_btw = c(0, distHaversine(cbind(Longitude[-n()], Latitude[-n()]),         # distance is in meters
                                       cbind(Longitude[  -1], Latitude[  -1]))),
         dst_alng = cumsum(dist_btw)) 

# Quick plot check for bad GPS or ping data
plot(meta$Longitude, meta$Latitude)

#remove(data_raw, meta_raw, meta_raw_2)  # clean up

meta <- meta %>% 
  mutate(dpth_plt = ifelse(Depth >= 250, NA, Depth)) # Only collected data to 250 m.

# Replacing masked data (-999) with NA and "no analysis above/below" with NA
pings <- data %>% 
  select(X14:X263) %>% 
  filter(!row_number() %in% c(bad_dpth_idx, bad_loc_idx)) %>%
  na_if(-999) %>% 
  na_if(-9.900000e+37) 

#vals_to_na <- c(-999, -9.900000e+37)
#replace_with_na_all(condition = ~.x %in% vals_to_na) %>%   #too slow.

# Replacing masked data (-999) with NA and "no analysis above/below" with NA
pings <- data %>% 
  select(X14:X263) %>% 
  filter(!row_number() %in% c(bad_dpth_idx, bad_loc_idx)) %>% 
  na_if(-999) %>% 
  na_if(-9.900000e+37) %>% 
  bind_cols(dst_alng = meta$dst_alng)  


pings_plot <- pings %>%
  gather(key, value, -dst_alng) %>%
  mutate(key = as.numeric(gsub("X", "", key))) 


gg <- ggplot()
gg <- gg + geom_tile(data = pings_plot, aes(x = dst_alng, y = key, fill = value), width = 100) +
  scale_fill_viridis_c(na.value = "white") +
  scale_y_reverse() +
  theme_classic() +
  labs(x = "Distance along track line (m)",
       y = "Depth (m)")
gg <- gg + geom_line(data = meta, aes(dst_alng, dpth_plt))  
