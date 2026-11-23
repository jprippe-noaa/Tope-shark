suppressWarnings(library(tidyverse))
suppressWarnings(library(maptools))
suppressWarnings(library(raster))
suppressWarnings(library(sf))

# Load trawl survey dataset
fi_trawls <- read_csv('../FisheriesData/Trawl survey_tope shark catch.csv')

# Map data
westcoast_polygon <- rworldmap::getMap(resolution = 'high') %>%
  crop(extent(-130,-117,30,50))
wc_shape <- st_as_sf(westcoast_polygon)

wc_map <- ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank())+
  geom_sf(data = wc_shape, fill = 'grey70', color = 'black', size = 0.1)+
  geom_point(data = fi_trawls, aes(x=longitude_dd, y=latitude_dd, fill = depth_m, size = cpue_kg_per_ha_der), shape = 21)+
  scale_fill_continuous(trans = 'reverse')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
wc_map

cowplot::save_plot('FI_trawl_surveys_byDepth.png', wc_map, base_height = 8, base_width = 5)

# CPUE x depth correlation and plot
library(lme4)

depth.mod <- lm(cpue_kg_per_ha_der ~ depth_m, data = fi_trawls)
summary(depth.mod)

ggplot(data = fi_trawls, aes(x = depth_m, y = cpue_kg_per_ha_der))+
  theme_bw()+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_continuous(limits = c(0, 425), expand = c(0,0))+
  scale_y_continuous(limits = c(0, 40), expand = c(0,0))

