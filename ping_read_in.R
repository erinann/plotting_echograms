library(tidyverse)
library(lubridate)
#library(naniar)
library(tictoc)

png_fldr <- "C://Users//ealab//Documents//WHOIcruise//dissPapers//Rwrk_pings//data_ping//"
bot_fldr <- "C://Users//ealab//Documents//WHOIcruise//dissPapers//Rwrk_pings//data_bottom//"
img_pth <- "C://Users//ealab//Documents//WHOIcruise//dissPapers//Rwrk_pings//images"

dt <- '0606'
line <- '16_D_SB'
frq1 <- '038_lgSBF.sv'
frq2 <- '120_zoop.sv'


#loading, clearning, checking tracklines, and combining 38 and 120.
data <- read_csv(paste0(png_fldr, dt, "_", line, "_", frq1, ".csv"),
                 skip = 1, col_names = FALSE,
                 col_types = cols(X4 = col_datetime(format = "%Y-%m-%d"),
                                  X5 = col_time(format = "%H:%M:%S"))) 


meta_raw <- data %>% 
  select(Ping_index = X1, 
         Distance_gps = X2, Distance_vl = X3,
         Ping_date = X4, Ping_time = X5, 
         Latitude = X7, Longitude = X8,
         Depth_start = X9, Depth_stop = X10,
         Range_start = X11, Range_stop = X12,
         Sample_count = X13) %>% 
  mutate(DT = Ping_date + Ping_time) 

ping_index <- select(meta_raw, Ping_index)


bottom <- read_csv(paste0(bot_fldr, dt, "_", line, "_bottom.line.csv"),
                   col_types = cols(Ping_date = col_datetime(format = "%Y-%m-%d"))) %>% 
  mutate(DT = Ping_date + Ping_time) %>% 
  bind_cols(ping_index)


meta <- meta_raw %>% 
  left_join(bottom, by = "Ping_index") %>% 
  select(Ping_index, Distance_gps, 
         Latitude = Latitude.x, Longitude = Longitude.x, 
         Depth, DT_1 = DT.x, DT_2 = DT.y)

#Quick chech nothing is messed up or backwards
sum(meta$DT_1 - meta$DT_2)


meta <- meta %>% 
  select(-DT_2) %>% 
  rename(DT = DT_1)


for_pings <- meta %>% 
  select(Distance_gps, Depth, Longitude, Latitude) %>% 
  filter(Longitude != 999.0)


#need to creat shelf break and dist from.


#vals_to_na <- c(-999, -9.900000e+37)


pings <- data %>% 
  select(X14:X263) %>% 
  #replace_with_na_all(condition = ~.x %in% vals_to_na) %>%   #too slow.
  na_if(-999) %>% 
  na_if(-9.900000e+37) %>%    
  rownames_to_column(var = "Ping_index") %>% 
  bind_cols(for_pings) 
  

pings_mtx <- data %>% 
  select(X14:X263) %>% 
  na_if(-999) %>% 
  na_if(-9.900000e+37) %>% 
  slice(-1)

#Fucken end depth!!! Need qualtity control!!

image(for_pings$Distance_gps, for_pings$Depth, pings_mtx)
# fix - need Pind_index, dist, Sv.

ping_plot <- pings %>% 
  gather(key = "Ping", value = "Sv", X14:X263) %>% 
  mutate(Depth = ifelse(Depth > 250, 250, Depth))


tic("geom_raster")
ggplot(ping_plot, aes(Distance_gps, Depth, fill = Sv)) +
  geom_raster() +
  scale_y_reverse() 
 # scale_color_gradient(na.value = "white")
toc()




#______________
  
                 
dta_lgSBF <- read_csv(read_csv(paste0(png_fldr, dt, "_", line, "_", frq1, ".csv"),
                               skip = 1,
                               col_types = col_skip()))




dta_250_zp <- read_csv(paste0(ac_fldr, "//", dt, "_", line, "_", frq2, "_", hr_res, "_", vrt_res, ".csv"),
                       col_types = cols(Date_M = col_datetime(format = "%Y%m%d")))

bottom <- read_rds(paste0(bot_fldr, "bottom_", dt, "_", line, ".rds")) #created from bottom_clean.R
glimpse(bottom)