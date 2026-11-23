library(tidyverse)

sau_chile_peru <- read_csv('data/SAU Taxa 604642 v50_Chile and Peru.csv') 

sau_chile_peru_byGear <- sau_chile_peru %>%
  group_by(year, fishing_entity, gear_type, reporting_status) %>%
  summarise(landings_mt = sum(tonnes)) %>%
  mutate(reporting_status = factor(reporting_status, levels = c('Unreported', 'Reported')))

sau_landings_plot_byGear <- ggplot(sau_chile_peru_byGear, aes(x = year, y = landings_mt, fill = reporting_status))+
  theme_bw()+
  geom_col(position = 'stack')+
  scale_x_continuous(name = 'Year', limits = c(1949, 2021), expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(name = 'Landings (mt)', expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(name = 'Reporting Status', values = c('grey80', 'black'))+
  facet_grid(gear_type~fishing_entity, scales = 'free_y')
sau_landings_plot_byGear

sau_chile_peru_byYearOnly <- sau_chile_peru %>%
  group_by(year, fishing_entity, reporting_status) %>%
  summarise(landings_mt = sum(tonnes)) %>%
  mutate(reporting_status = factor(reporting_status, levels = c('Unreported', 'Reported')))

sau_landings_plot_byYearOnly <- ggplot(sau_chile_peru_byYearOnly, aes(x = year, y = landings_mt, fill = reporting_status))+
  theme_bw()+
  geom_col(position = 'stack')+
  scale_x_continuous(name = 'Year', limits = c(1949, 2021), expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(name = 'Landings (mt)', expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(name = 'Reporting Status', values = c('grey80', 'black'))+
  facet_grid(~fishing_entity, scales = 'free_y')
sau_landings_plot_byYearOnly

write.csv(sau_chile_peru_byGear, '../../Downloads/SAU_Chile_Peru_byGear.csv')
write.csv(sau_chile_peru_byGear, '../../Downloads/SAU_Chile_Peru_byYear.csv')

cowplot::save_plot('figures/SE_Pacific/SAU_Chile_Peru_byGear.png', sau_landings_plot_byGear, base_height = 6, base_width = 7)
cowplot::save_plot('figures/SE_Pacific/SAU_Chile_Peru_byYear.png', sau_landings_plot_byYearOnly, base_height = 3, base_width = 6)


