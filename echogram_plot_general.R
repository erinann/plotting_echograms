library(tidyverse)
library(lubridate)
library(tictoc)
library(geosphere)
library(here)

# Data are Sv exports of .sv.csv (ping)
# Data live in the folder data_ping
# Bottom data live in the folder data_bottom
# Images are saved to images

png_fldr <- paste0(here(), "/data_ping/")
bot_fldr <- paste0(here(), "/data_bottom/")
img_pth <-  paste0(here(), "/images/")

# These are parameters that describe the data. Change as you wish.
dt <- '0614'              # Date
line <- '17_D_SB'         # Transect number
frq1 <- '038_lgSBF.sv'    # Frequency data
frq2 <- '120_zoop.sv'     # Frequnecy data (assuming you have more than one freqency )

# Picking the frequency you want to use.
frq <- frq2

# Loading data. Pasting parameter strings together to make the folder/filename.
data_raw <- read_csv(paste0(png_fldr, dt, "_", line, "_", frq, ".csv"),
                     skip = 1, col_names = FALSE,
                     col_types = cols(X4 = col_datetime(format = "%Y-%m-%d"),
                                      X5 = col_time(format = "%H:%M:%S"))) 

# Number of samples
nb_smpl <- data_raw %>% 
  select(X13) %>% 
  slice(1) %>% 
  pull()

# First and last columns with Sv
first_Sv_column <-  14
last_Sv_column <- (first_Sv_column + nb_smpl) -1
  
# Number of pings (number of rows)
nb_rows <- nrow(data_raw)

# Brut force cleaning. Removing the first 2 and last 2 pings. They usually have bad GPS points. 
data <- data_raw %>% 
  rowid_to_column(var = "row_id") %>% 
  filter(between(row_id, 3, (nb_rows - 2)))

# Giving the meta data column names.
# DT = datetime stamp
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
# These two datasets can be combined without a join if no data munging has occured 
# First two and last two pings are removed to match ping data.

bottom <- read_csv(paste0(bot_fldr, dt, "_", line, "_bottom.line.csv"),
                   col_types = cols(Ping_date = col_datetime(format = "%Y-%m-%d"))) %>% 
  mutate(DT = Ping_date + Ping_time) %>% 
  rowid_to_column(var = "row_id") %>% 
  filter(between(row_id, 3, (nb_rows - 2))) %>% 
  bind_cols(ping_index) 


# Finding pings with bad depth points.
bad_dpth_idx <- bottom %>% 
  filter(Depth < 0) %>% 
  select(row_id) %>% 
  pull()

# Finding pings with bad location points.
bad_loc_idx <- meta_raw %>% 
  filter(Longitude == 999.0) %>% 
  select(row_id) %>% 
  pull()

# Adding the bottom data (Depth) to the ping data.
meta_raw_2 <- meta_raw %>% 
  left_join(bottom, by = "Ping_index") %>%
  select(row_id = row_id.x, Ping_index, Distance_gps,
         Latitude = Latitude.x, Longitude = Longitude.x, 
         Depth, DT_1 = DT.x, DT_2 = DT.y)

# Quick check nothing is messed up or backwards. 
# Subtracting times. If time difference is more than 0, something is probably wrong.
sum(meta_raw_2$DT_1 - meta_raw_2$DT_2)

# Cleaned meta
# row_id, Ping_index, Distance_gps (from EV), Latitude, Longitude, Depth, DT (date-timestamp),
# dist_btw (distance between pings calculated from lon/lat), 
# dst_alng (distance along the trackline calculated from the start of the trackline, lon/lat)
meta <- meta_raw_2 %>% 
  select(-DT_2) %>% 
  rename(DT = DT_1) %>% 
  filter(!row_number() %in% c(bad_dpth_idx, bad_loc_idx)) %>% 
  mutate(dist_btw = c(0, distHaversine(cbind(Longitude[-n()], Latitude[-n()]),  # distance is in meters
                                       cbind(Longitude[  -1], Latitude[  -1]))),
         dst_alng = cumsum(dist_btw)) 

# Quick plot check for bad GPS or ping data
plot(meta$Longitude, meta$Latitude)

# Removing variables not needed for further processing
remove(data_raw, meta_raw, meta_raw_2)  

# Again for me, setting all depth greter than 250 meter to 250. 
# Either comment this out or change the depth if you would like.
meta <- meta %>% 
  mutate(dpth_plt = ifelse(Depth >= 250, NA, Depth))


#### Now using bottom data to create polygon for bathymetry. 
# You probably won't need this.
# Removing NA rows to reduce size and make it easier to get extra points
bottom <- bottom %>% 
  filter(!row_number() %in% c(bad_dpth_idx, bad_loc_idx)) %>% 
  mutate(dist_btw = c(0, distHaversine(cbind(Longitude[-n()], Latitude[-n()]),    # distance is in meters
                                       cbind(Longitude[  -1], Latitude[  -1]))),
         dst_alng = cumsum(dist_btw)) 

# Column names for first and last columns
first_col_name <-  paste0("X", first_Sv_column)
last_col_name <-  paste0("X", last_Sv_column)


# Replacing masked data (-999) with NA and "no analysis above/below" with NA
pings <- data %>% 
  select(first_col_name:last_col_name) %>% 
  filter(!row_number() %in% c(bad_dpth_idx, bad_loc_idx)) %>% 
  na_if(-999) %>% 
  na_if(-9.900000e+37) %>% 
  bind_cols(dst_alng = meta$dst_alng)  

# Removing no data to reduce size
pings_plot <- pings %>%
  gather(key, value, -dst_alng) %>%
  mutate(key = as.numeric(gsub("X", "", key)),
         dpth_bin = key - 8) %>% 
  filter(!is.na(value))

# color scale
color_scale <- scale_fill_gradientn(
  colours=colors()[c(227,177,26,30,448,139,652,90,451,552,426,61)],
  limits=c(-85,-25), na.value = "white", breaks=c(-85,-65,-45,-25),
  name=expression(atop(S[v],paste("(dB re. ",m^-1,")"))))

# plotting the echogram and bathymetry
gg <- ggplot()
gg <- gg + 
  geom_tile(data = pings_plot, aes(x = dst_alng, y = dpth_bin, fill = value), width = 150) +
  color_scale +
  scale_y_reverse(limits = c(250, 0), expand = c(0, 0)) + #NOTE: Vertical limits set here. You might want to change.
  scale_x_continuous(limits = c(0, max(pings_plot$dst_alng)), expand = c(0, 0)) + #NOTE: Horizontal limits here.
  theme_classic(base_size = 16) 
gg <- gg + 
  geom_path(data = bottom, aes(dst_alng, Depth)) +
  labs(x = "Distance along trackline (m)",
       y = "Depth (m)")
gg

# Creating a file name to save the pplot as a .png
flnm <- paste0(dt, "_", line, str_remove(frq, ".sv"))

# Saving the .png
# Note: Files do not get overwritten. If you make a bad plot, delete it.
ggsave(paste0(here(),"/images/", flnm, ".png"), plot = gg, device = "png", width = 45, height = 17, units = "cm" )

