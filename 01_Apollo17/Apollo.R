library(tidyverse)
library(lubridate)
library(FIPS)
library(ggExtra)

# Old utility function
split_times <- function(duration_stamp) {
  splitted = strsplit(duration_stamp, ":")
  splitted = lapply(splitted, FUN = setNames, c("hour", "min", "sec"))
  return(splitted)
}

# Imported Data
CMDR_DATA = data.frame(
  stringsAsFactors = FALSE,
       Rest.Period = c("1","2","3","4","5","6LO",
                       "6 LS","7 LO","7 LS","8 LO","8 LS","9","10",NA,
                       NA,NA),
       Description = c("TLC, all 3 crew",
                       "TLC, all 3 crew","TLC, all 3 crew","TLC, all 3 crew",
                       "LO, all 3 crew","CMP in LO","LM crew on LS","CMP in LO",
                       "LM crew on LS","CMP in LO","LM crew on LS","LO, all 3 crew",
                       "LO, all 3 crew","TEC, all 3 crew","TEC, all 3 crew",
                       "TEC, all 3 crew"),
             Start = c("8:59:31","25:26:26",
                       "48:36:12","71:07:15","95:07:27","120:53:36","125:35:32",
                       "143:46:00","149:46:16","168:16:24","172:40:55",
                       "196:05:01","217:46:55","240:28:47","265:35:12","288:00:53"),
               End = c("15:02:57","33:00:38",
                       "56:35:43","78:50:09","103:07:28","128:43:33","134:16:39",
                       "152:01:17","157:45:04","177:05:02","180:49:59",
                       "204:57:10","225:30:51","247:50:01","272:20:01","295:20:02"),
               CDR = c("3","6.5","7.5",
                       "5","6.5","-","6","-",
                       "6","-","5","7",
                       "5","5","5","5")
)

# Launched on 05:33:00 GMT, 7 December 1972
LAUNCH_DATE = as.POSIXct("1972/12/7 00:33:00", tz = "GMT") 
END_DATE = LAUNCH_DATE + ddays(12) + dhours(13) + dminutes(15) + dseconds(59)

# Remove null
CMDR_DATA = subset(CMDR_DATA, CDR != "-")

# Clean up and calc for FIPS
CMDR_DATA_PARSED = CMDR_DATA %>% 
  tidyr::separate(Start, c("start_hour", "start_min", "start_second")) %>% 
  tidyr::separate(End, c("end_hour", "end_min", "end_second")) %>% 
  dplyr::mutate(Rest.Period = seq(1:dplyr::n())) %>% 
  dplyr::mutate(DateTime_RestStart = LAUNCH_DATE + hours(start_hour) + minutes(start_min)) %>% 
  dplyr::mutate(DateTime_RestEnd = LAUNCH_DATE + hours(end_hour) + minutes(end_min)) %>% 
  dplyr::mutate(RestDuration = DateTime_RestEnd - DateTime_RestStart) %>% 
  dplyr::mutate(MaxOffset = as.numeric(RestDuration) - as.numeric(CDR)) %>% 
  dplyr::mutate(sleep_start = DateTime_RestStart + dminutes(15)) %>% 
  dplyr::mutate(sleep_end = sleep_start + dhours(as.numeric(CDR)))

# Check all good - YEP!
all.equal(as.numeric(CMDR_DATA_PARSED$CDR),
          as.numeric(CMDR_DATA_PARSED$sleep_end - CMDR_DATA_PARSED$sleep_start))
  

CMDR_MINIMAL = CMDR_DATA_PARSED %>% 
  select(sleep.id = Rest.Period, sleep.start = sleep_start, sleep.end = sleep_end) %>% 
  as_tibble()


CMDR_FIPS_Times = FIPS::parse_sleeptimes(
  sleeptimes = CMDR_MINIMAL,
  series.start = LAUNCH_DATE - dminutes(3),
  series.end = END_DATE,
  roundvalue = 15,
  sleep.start.col = "sleep.start",
  sleep.end.col = "sleep.end",
  sleep.id.col = "sleep.id")

CMDR_FIPS_Times_Sim = FIPS_simulate(CMDR_FIPS_Times, "TPM", FIPS::TPM_make_pvec(S0 = 14))

heatmap_dats = CMDR_FIPS_Times_Sim %>% 
  mutate(fatigue = KSS) %>%
  mutate(fatigue = ifelse(wake_status == F, NA, fatigue)) %>% 
  mutate(floor_time = floor(time))
  
p = ggplot(heatmap_dats, aes(day, time, fill = fatigue, group = floor_time)) +
  scale_x_reverse(expand = c(0, 0), breaks = seq(13,1,-1)) +
  geom_tile(color = "black", size = 0.1) +
  scale_y_continuous(expand = c(0, 0),breaks = seq(0,23,1)) +
  labs(y = "Hour of Day", x = "Day of Mission") +
  scale_fill_distiller(name = "KSS", palette = "Spectral", direction = -1, limits = c(1, 9), breaks = c(1,2,3,4,5,6,7,8,9), 
                       labels = c(1,2,3,4,5,6,7,8,9), guide = guide_colorbar(ticks.colour = "black", 
                                                                             ticks.linewidth  = 1,
                                                                             frame.colour = "black",
                                                                             draw.ulim = F,
                                                                             draw.llim = F,
                                                                             direction = "horizontal",
                                                                             barwidth = 20)) +
  coord_flip() + 
  theme(strip.background = element_rect(colour="white"), legend.position="bottom") +
  removeGrid()
  # viridis::scale_fill_viridis(name = "Hrly Cognitive Eff.", direction = 1, option = "D", limits = c(0, 10)) +
  # viridis::scale_fill_viridis(name = "Hrly Cognitive Eff.", direction = -1, option = "A", begin = 0.1) +
p
  # theme(axis.title = element_text(size = 12)) +
  
  
  
