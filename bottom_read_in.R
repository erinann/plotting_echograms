library(tidyverse)
library(lubridate)

dt <- '0606'
line <- '16_D_SB'
bot_fldr <- "C://Users//ealab//Documents//WHOIcruise//dissPapers//Rwrk_pings//data_bottom//"


bottom <- read_csv(paste0(bot_fldr, dt, "_", line, "_bottom.line.csv"),
                col_types = cols(Ping_date = col_datetime(format = "%Y-%m-%d"))) %>% 
  mutate(DT = Ping_date + Ping_time)


par(mfrow = c(1, 4))
plot(bottom$Longitude, bottom$Latitude)
boxplot(bottom$Ping_status, main = "Ping status")
boxplot(bottom$Line_status, main = "Line status")
boxplot(bottom$Position_status, main = "Position status")
par(mfrow = c(1,1))

    
#remove bad positions
bottom <- bottom %>%
  filter(Position_status == 1,
         Line_status != 3)

glimpse(bottom)

#quick check to see if deviations from the trackline were removed correctly
plot(bottom$Longitude, bottom$Latitude)

write_rds(bottom, paste0(bot_fldr, "bottom_", dt, "_", line, ".rds"))

