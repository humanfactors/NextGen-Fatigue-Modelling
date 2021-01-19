library(tidyverse)
library(lubridate)
library(FIPS)
library(RColorBrewer)

## Watch keep revision one

sleeptimes15 <- tibble::tibble(
  sleep.start = seq(
    from = lubridate::ymd_hms('2018-05-01 17:15:00', tz = "Australia/Perth"), 
    to = lubridate::ymd_hms('2018-05-10 17:15:00', tz = "Australia/Perth"),
    by = '24 hours'),
  sleep.end = sleep.start + lubridate::dhours(5.75),
  sleep.id = rank(sleep.start))

FIPS_TIMES = FIPS::parse_sleeptimes(sleeptimes15,
                       sleep.end.col = "sleep.end",
                       sleep.id.col = "sleep.id",
                       sleep.start.col = "sleep.start",
                       roundvalue = 15,
                       series.start = ymd_hms('2018-05-01 00:00:00', tz = "Australia/Perth"),
                       series.end = ymd_hms('2018-05-10 23:00:00', tz = "Australia/Perth"))

FIPS_TIMES_CALCD = FIPS_TIMES %>% 
  mutate(working = case_when(
    dplyr::between(time, 0, 3) ~ T,
    dplyr::between(time, 6, 9) ~ T,
    !is.null(time) ~ F)) %>% 
  mutate(shiftnumber = case_when(
    dplyr::between(time, 0, 3) ~ 1,
    dplyr::between(time, 6, 9) ~ 2,
    !is.null(time) ~ 0))


FIPS_WK1 = FIPS_simulate(FIPS_TIMES_CALCD, "unified", FIPS::unified_make_pvec(phi = 12)) %>% 
  mutate(ID = "Phi12")

FIPS_WK2 = FIPS_simulate(FIPS_TIMES_CALCD, "unified", FIPS::unified_make_pvec(phi = 6)) %>% 
  mutate(ID = "Phi6")

FIPS_WK3 = FIPS_simulate(FIPS_TIMES_CALCD, "unified", FIPS::unified_make_pvec(phi = 18)) %>% 
  mutate(ID = "Phi18")

FIPS_WK4 = FIPS_simulate(FIPS_TIMES_CALCD, "unified", FIPS::unified_make_pvec(phi = 0)) %>% 
  mutate(ID = "Phi0")

FINAL_BIND = FIPS_WK1 %>% 
  bind_rows(FIPS_WK2, FIPS_WK3, FIPS_WK4) %>% 
  mutate(fatigue = scale(fatigue, center = T)) %>% 
  mutate(fatigue = ifelse(wake_status == F, NA, fatigue),
         ID = as.factor(ID)) %>% 
  mutate(floor_time = floor(time)) 

p = ggplot(FINAL_BIND, aes(day, time, fill = fatigue)) +
  scale_x_reverse(expand = c(0, 0), breaks = seq(13,1,-1)) +
  geom_tile(color = "black", size = 0.1) +
  scale_y_continuous(expand = c(0, 0),breaks = seq(0,23,1)) +
  labs(y = "Hour of Day", x = "Day of Mission") +
  scale_fill_distiller(
    name = "Centered Fatigue Score",
    palette = "Spectral",
    direction = -1,
    # limits = c(1, 24),
    # breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    # labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    guide = guide_colorbar(
      ticks.colour = "black",
      ticks.linewidth  = 1,
      frame.colour = "black",
      draw.ulim = F,
      draw.llim = F,
      direction = "horizontal",
      barwidth = 20)) +
  coord_flip() +
  facet_wrap(~ID) +
  theme(strip.background = element_rect(colour="white"), legend.position="bottom") +
  removeGrid() 

p



## Watchkeep revision 2


PhiFunc = function(FIPS_TIMES,phi) {
  FIPS_simulate(FIPS_TIMES, "TPM", FIPS::TPM_make_pvec(p = phi)) %>% 
    mutate(ID = phi) %>%
    select(ID, datetime,shiftnumber, day, c, working, KSS)
}





phi_hours = list(0:24)

for (i in 0:24) {
  phi_hours[[i+1]] = PhiFunc(FIPS_TIMES_CALCD, i)
}

phi_hours_full = bind_rows(phi_hours)

phi_hours_dats = phi_hours_full %>% 
  filter(working == T, day > 1)
  # mutate(fatigue = scPhiFunc(FIPS_TIMES_CALCD, i)ale(KSS, center = T, scale = F)) %>% 
  group_by(ID) %>% 


phi_hours_dats %>% 
  summarise(fatigue_mean = mean(KSS)) %>% 
  arrange(-fatigue_mean)

# ggstatsplot::ggbetweenstats(phi_hours_dats, ID, fatigue,
#                             package = "dichromat",
#                             palette = "DarkRedtoBlue.25")

ggplot(phi_hours_dats, aes(x = ID, y = KSS, group = ID, colour = as.factor(shiftnumber))) +
  ggplot2::geom_point(position = position_jitter(0.1), alpha = 0.4) +
  geom_violin(size = 1,
              alpha = 0.5) +
  geom_boxplot(alpha = 0.2, position = ggplot2::position_dodge(width = NULL)) +
  

mypal = colorRampPalette(brewer.pal(4, "PuOr"))(25)
