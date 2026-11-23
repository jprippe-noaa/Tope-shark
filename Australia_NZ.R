library(tidyverse)


# NZ Fisheries Assessment Plenary Report (2024) ---------------------------

#Table 7
nzfish_table7 <- read_csv('data/NZPlenary_May2024_Table7.csv') %>%
  mutate(sd = B_core * CV_perc / 100,
         x_nudge = ifelse(Survey == "TBGB_autumn", -0.15, ifelse(Survey == "WCSI_autumn", 0.15, 0)),
         plot_color = ifelse(Survey == "TBGB_autumn", 'TBGB', 'WCSI'),
         region = factor(ifelse(Survey %in% c("TBGB_autumn", "WCSI_autumn"), "WCSI", Survey),
                         levels = c('WCNI','Chatham Rise','WCSI','ECSI'))) %>%
  mutate(upper = B_core + (2 * sd),
         lower = B_core - (2 * sd))

nzfish_table7_plot <- ggplot(data = nzfish_table7, aes(x = Year + x_nudge, y = B_core, group = Survey, color = plot_color))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'),
        legend.position = 'right')+
  ggtitle('NZ Tope Shark Biomass Estimates',
          subtitle = 'Inshore Bottom Trawl Surveys (1989-2023)')+
  ylab('Biomass (mt)')+
  geom_segment(aes(x = Year + x_nudge, xend = Year + x_nudge, y = lower, yend = upper))+
  geom_point()+
  coord_cartesian(xlim = c(1988, 2024), ylim = c(0, 2300), expand = 0)+
  scale_color_manual(values = c('TBGB' = 'blue', 'WCSI' = 'black'))+
  facet_wrap(.~region)
nzfish_table7_plot

cowplot::save_plot('figures/Australia_NZ/NZ fisheries assessment_May2024_Fig7.png', nzfish_table7_plot, base_height = 5, base_width = 7)


# NZ FishStock data -------------------------------------------------------

nz_fishstock <- read_csv('data/FisheriesNZ_Fishstock_ReportedLandings_1931-2015.csv') %>%
  rename(Weight_tonnes = "Landings")



