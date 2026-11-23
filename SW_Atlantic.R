library(tidyverse)

######################
###~~~~ BRAZIL ~~~~###
######################

##~~~~~ CPUE of demersal sharks (principally G. galeus and M. schmitti) Rio Grande do Sul (1975-1997) [Villwock de Miranda and Vooren, 2003]
rgds_cpue <- read_csv('data/Villock_de_Miranda_Vooren_2003_RioGrandedoSulCPUE_1975-1997.csv') %>%
  pivot_longer(cols = -c(Year), names_to = 'Gear', values_to = 'CPUE') %>%
  mutate(Gear = factor(Gear))

# Reorder levels of 'Gear' column
rgds_cpue$Gear <- factor(rgds_cpue$Gear, levels = c('Simple_Trawl','Pair_Trawl','Dual_rig_trawl','Coastal_gillnet','Pelagic_gillnet'))

# Plot CPUE data
rgds_cpue_plot <- ggplot(rgds_cpue, aes(x = Year, y = CPUE, col = Gear))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  guides(color = guide_legend(nrow = 3, byrow = TRUE))+
  labs(y = 'CPUE (tonnes/trip)')+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(1950, 2015), expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(limits = c(0, 13), expand = expansion(mult = c(0, 0.05)))+
  scale_color_brewer(palette = 'Set2', labels = c('Arrasto Simples (Simple Trawl)',
                                                  'Arrasto de Parelha (Pair Trawl)',
                                                  'Arrasto de Tangone (Dual Rig Trawl)',
                                                  'Emalhe Costeira (Coastal Gillnet)',
                                                  'Emalhe Oceánica (Pelagic Gillnet)'))
rgds_cpue_plot

cowplot::save_plot('figures/SW_Atlantic/rgds_cpue_plot.png', rgds_cpue_plot, base_height = 5, base_width = 8)

##~~~~~ Brazil reconstructed landings data (1950-2015) [Freire et al. 2021]
brazil_landings <- read_csv('data/Freire_etal_2021_Brazil landings_1950 to 2015.csv') %>%
  select(-c('TaxonName', 'Input', 'Notes')) %>%
  mutate(State = factor(State), Sector = factor(Sector))

# Reorder levels of 'State' column by latitude
brazil_landings$State <- factor(brazil_landings$State, levels = c('Rio de Janeiro','Santa Catarina','Rio Grande do Sul'))
brazil_landings$Sector <- factor(brazil_landings$Sector, levels = c('Industrial (LS, C)','Artisanal (SS, C)'))

# Plot landings data
brazil_landings_plot <- ggplot(brazil_landings, aes(x = Year, y = CatchAmount_t, fill = Sector))+
  theme_bw()+
  geom_col(position = 'stack')+
  scale_x_continuous(limits = c(1950, 2015), expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(limits = c(0, 1750), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c('grey60', 'black'))+
  facet_wrap(~State, ncol = 1)
brazil_landings_plot

brazil_catch_proportions <- group_by(brazil_landings, State) %>% 
  summarize(total_catch = sum(CatchAmount_t)) %>%
  mutate(prop_catch = total_catch / sum(brazil_landings$CatchAmount_t))

# Plot landings data for Rio Grande do Sul only
rgds_landings_plot <- ggplot(filter(brazil_landings, State == 'Rio Grande do Sul'), aes(x = Year, y = CatchAmount_t, fill = Sector))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none')+
  labs(y = 'Catch (tonnes)')+
  geom_col(position = 'stack')+
  scale_x_continuous(limits = c(1950, 2015), expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(limits = c(0, 1750), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c('grey60', 'black'))
rgds_landings_plot

cowplot::save_plot('figures/SW_Atlantic/rgds_landings_plot.png', rgds_landings_plot, base_height = 3, base_width = 8)

# Combine Rio Grande do Sul CPUE and landings datasets
rgds_combined <- bind_rows(rgds_cpue, filter(brazil_landings, State == 'Rio Grande do Sul'))
rgds_combined_plot <- ggplot(rgds_combined, aes(x = Year))+
  theme_bw()+
  geom_col(aes(y = CatchAmount_t / 100, fill = Sector), position = 'stack')+
  geom_point(aes(y = CPUE, col = Gear), size = 0.6)+
  geom_line(aes(y = CPUE, col = Gear))+
  scale_x_continuous(limits = c(1949, 2016), expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(name = 'CPUE (mt / trip)', expand = expansion(mult = c(0, 0.05)), 
                     sec.axis = sec_axis(~ . * 100, name = "Catch (mt)"))+
  scale_color_manual(values = c("#66C2A5","orange","#8DA0CB","red3","#A6D854"),, 
                     labels = c('Simple Trawl','Pair Trawl','Dual Rig Trawl','Coastal Gillnet','Oceanic Gillnet'))+
  scale_fill_manual(values = c('grey90', 'grey60'),
                    labels = c('Industrial','Artisanal'))
rgds_combined_plot

cowplot::save_plot('figures/SW_Atlantic/rgds_landings_cpue_plot_revised.png', rgds_combined_plot, base_height = 4, base_width = 7)



#######################
###~~~~ URUGUAY ~~~~###
#######################

##~~~~~ CPUE of cazón in Uruguay longline fishery (1972-1985) [Nion 1999, Table 16]
uruguay_cpue <- read_csv('data/Nion1999_longlineCPUE_Uruguay_1972-1985.csv') %>%
  mutate(Area = factor(Area))

uruguay_cpue_std_byArea <- group_by(uruguay_cpue, Year) %>%
  mutate(CPUE_std = CPUE / mean(uruguay_cpue$CPUE))

uruguay_cpue_std_total <- group_by(uruguay_cpue, Year) %>%
  summarize(CPUE = mean(CPUE)) %>%
  mutate(CPUE_std = (CPUE / mean(uruguay_cpue$CPUE, na.rm = T)) - 1)

# Plot CPUE by fishing area
uruguay_cpue_plot <- ggplot(uruguay_cpue, aes(x = Year, y = CPUE, fill = Area))+
  theme_bw()+
  labs(y = 'CPUE (kg/100 hooks)')+
  geom_col(position = position_dodge())+
  scale_y_continuous(limits = c(0, 400), expand = expansion(mult = c(0, 0.05)))
uruguay_cpue_plot

# Plot mean-standardized CPUE (averaged across fishing areas)
uruguay_cpue_std_plot <- ggplot(uruguay_cpue_std_total, aes(x = Year, y = CPUE_std))+
  theme_bw()+
  labs(y = 'CPUE (kg/100 hooks)')+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits = c(-1, 1), expand = expansion(mult = c(0, 0.05)))
uruguay_cpue_std_plot

##~~~~~ Catch of cazones in Uruguay by gear type (1977-1997) [Nion 1999, Table 10]
uruguay_landings_byGear <- read_csv('data/Nion1999_catchByGear_1977-1997.csv') %>%
  pivot_longer(cols = -c(Year), names_to = 'Gear', values_to = 'Catch_t') %>%
  mutate(Gear = factor(Gear))

uruguay_landings_byGear$Gear <- factor(uruguay_landings_byGear$Gear, levels = c('Trawl_bottom', 'Gillnet_common', 'Gillnet_shark', 'Longline_common', 'Longline_shark'))

uruguay_landings_plot <- ggplot(uruguay_landings_byGear, aes(x = Year, y = Catch_t, fill = Gear))+
  theme_bw()+
  geom_col(position = 'stack')+
  scale_x_continuous(limits = c(1971, 1998), expand = expansion(mult = c(0, 0.05)), breaks = c(1975, 1980, 1985, 1990, 1995, 2000))+
  scale_y_continuous(name = 'Catch (tonnes)', expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("#FB9A99", "#B2DF8A", "#33A02C", "#A6CEE3", "#1F78B4"),
                    labels = c('Deepwater Trawl', 'Gillnet (common)', 'Gillnet (shark)', 'Longline (common)', 'Longline (shark)'))
uruguay_landings_plot

uruguay_combined <- bind_rows(uruguay_landings_byGear, uruguay_cpue_std_total)
uruguay_combined$Gear <- factor(uruguay_combined$Gear, levels = c('Trawl_bottom', 'Gillnet_common', 'Gillnet_shark', 'Longline_common', 'Longline_shark'))

uruguay_combined_plot <- ggplot(uruguay_combined, aes(x = Year, fill = Gear))+
  theme_bw()+
#  theme(legend.position = 'none')+
  geom_col(aes(y = Catch_t / 100000), position = 'stack')+
  geom_point(aes(y = (CPUE / 50) - 4), color = '#1F78B4')+
  geom_line(aes(y = (CPUE / 50) - 4), color = '#1F78B4')+
  geom_hline(yintercept = 0)+
  scale_x_continuous(limits = c(1971, 1998), expand = expansion(mult = c(0, 0.05)), breaks = c(1975, 1980, 1985, 1990, 1995, 2000))+
  scale_y_continuous(name = 'Catch (tonnes, x10^6)', expand = expansion(mult = c(0, 0.05)), limits = c(-4.1, 12.5),
                     sec.axis = sec_axis(~ ., name = "CPUE (kg/100 hooks)", breaks = c(-4,-3,-2,-1)))+
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#A6CEE3", "#1F78B4", "#FB9A99"))
uruguay_combined_plot

cowplot::save_plot('figures/SW_Atlantic/uruguay_landings_cpue_plot.png', uruguay_combined_plot, base_height = 4, base_width = 7)


#########################
###~~~~ ARGENTINA ~~~~###
#########################

##~~~~~ Sharks + school sharks landings in Argentina (1979-2015) [Chiaramonte et al 2016, Figure 3]
##~~~~~ Added data (2016-2023) provided by G.E. Chiaramonte in peer review comments
argentina_landings <- read_csv('data/Chiaramonte_etal_2016_Argentina landings_1979-2023.csv')
  
argentina_landings_plot <- ggplot(argentina_landings, aes(x = Year))+
  theme_bw()+
  theme(legend.position = 'none')+
  geom_col(aes(y = Capture_tons, fill = Source))+
  geom_point(aes(y = Proportion * 100), size = 1)+
  geom_line(aes(y = Proportion * 100))+
  scale_x_continuous(limits = c(1978, 2024), expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(name = 'Landings (mt)', expand = expansion(mult = c(0, 0.05)), 
                     sec.axis = sec_axis(~ . / 100, name = "Proportion (%)"))+
  scale_fill_manual(values = c('grey70','grey90'))
argentina_landings_plot

cowplot::save_plot('figures/SW_Atlantic/argentina_landings_plot.png', argentina_landings_plot, base_height = 3, base_width = 5)

mean(argentina_landings$Capture_tons[argentina_landings$Year > 1996])


#########################################################
###~~~~ ARGENTINEAN-URUGUAYAN COMMON FISHING ZONE ~~~~###
#########################################################

cfz <- read_csv('data/CTMFM_estadisticas_pesqueras_2000-2024.csv') %>%
  group_by(Year, Country) %>%
  summarise(Tonnes = sum(Tonnes))

cfz_plot <- ggplot(cfz, aes(x = Year, y = Tonnes, fill = Country))+
  theme_bw()+
  geom_col(position = 'stack')+
  scale_x_continuous(limits = c(1999, 2025), expand = expansion(mult = c(0, 0.05)), breaks = c(2000, 2005, 2010, 2015, 2020, 2025))+
  scale_y_continuous(name = 'Landings (mt)', expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("#B2DF8A", "#1F78B4"),
                    labels = c('Argentina', 'Uruguay'))
cfz_plot
cowplot::save_plot('figures/SW_Atlantic/CTMFM_CFZlandings_2000-2024.png', cfz_plot, base_height = 3, base_width = 5)




