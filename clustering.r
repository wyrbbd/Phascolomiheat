library(tidyverse)
library(spotoroo)
library(readr)
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(gganimate)
d <- read_csv("data/hotspot_2019_2020.csv")

data(hotspots)


result <- hotspot_cluster(d,
  lon = "lon",
  lat = "lat",
  obsTime = 'obsTime',
  activeTime = 24,
  adjDist = 3000,
  minPts = 4,
  minTime = 3,
  ignitionCenter = "mean",
  timeUnit = "h",
  timeStep = 1
)

result

merged_result <- extract_fire(result, cluster = "all", noise = TRUE)

plot_spotoroo(result, type = "def")

plot_spotoroo(result, type = "timeline")

plot_spotoroo(result, type = "mov", step = 6)

if (requireNamespace("sf", quietly = TRUE)) {
  plot_spotoroo(result, bg = plot_vic_map())
}

if (requireNamespace("sf", quietly = TRUE)) {
  plot_spotoroo(result, type = "mov", bg = plot_vic_map(), step = 6)
}

result_data <- result$hotspots%>%
  mutate(membership = as.factor(membership))

vic_map = ne_states(country = 'Australia', returnclass = 'sf')%>%
  filter(name == "Victoria")

p = ggplot(result_data) +
  geom_sf(data = vic_map) +
  geom_point(aes(x = lon, y = lat,color = membership), alpha = 0.2) + 
  transition_time(obsTime) +
  exit_fade() +
  enter_fade() +
  labs(title = 'Time: {frame_time}') +
  theme(legend.position = "none")+
  theme_bw()
animate(p)


 
# Code to make density plot
library(ggthemes)
bkgd_map <- ggplot() + 
  geom_density_2d_filled(data=d, aes(x=lon, y=lat)) +
  geom_sf(data = vic_map, colour="white", fill=NA) +
  #xlim(c(140, 150.5)) + ylim(c(33.5, 39.5)) +
  scale_fill_grey() +
  theme_map() +
  theme(legend.position="none")

save(bkgd_map, file="bkgd_map.rda")  

load("bkgd_map.rda")

bkgd_map +
  geom_point(d %>% dplyr::filter(date==as.Date("2020-01-08")), 
             mapping=aes(x=lon, y=lat), 
             colour="orange") 


