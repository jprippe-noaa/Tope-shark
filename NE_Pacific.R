library(tidyverse)

# PacFIN (by Gear) ------------------------------------------------------------------
# Added historical data (1950-1980) from NOAA's Fisheries One Stop Shop (FOSS) database;
# (California: 1941-1944) from Ripley (1946);
# (Oregon: 1942-1949) from Oregon Fish Commission (1951);
# and (California: 1938-1940, 1945-1980; Oregon: 1940-41) from King et al (2017)

# Load PacFIN_byGear, NOAA FOSS data set, data from Ripley (1946) Table 4, approximate data from King et al (2017) Figure 7B
pacfin_g <- read_csv('data/PacFIN/PacFIN_1981-2024_byGear.csv') %>%
  mutate(DATA_SOURCE = 'PACFIN')
pacfin_foss <- read_csv('data/PacFIN/FOSS_1950-2024.csv') %>%
  filter(`Scientific Name` == 'Galeorhinus galeus') %>%
  mutate(DATA_SOURCE = 'NOAA FOSS')
ripley_t4 <- read_csv('data/Ripley_1946_Table4.csv') %>%
  rename(LANDING_YEAR = Year) %>%
  group_by(LANDING_YEAR) %>%
  summarise(LANDED_WEIGHT_LBS = sum(Landed_lbs)) %>%
  mutate(LANDED_WEIGHT_MTONS = LANDED_WEIGHT_LBS / 2204.62,
         AGENCY_CODE = 'C',
         GEAR_CODE = 'MSC',
         DATA_SOURCE = 'RIPLEY 1946')
odfw_tope <- read_csv('data/ODFW1951_Table1.csv') %>%
  filter(Tope == 'Tope') %>%
  select(Year, `Metric Tons`) %>%
  rename(LANDING_YEAR = Year,
         LANDED_WEIGHT_MTONS = `Metric Tons`) %>%
  mutate(AGENCY_CODE = 'O',
         GEAR_CODE = 'MSC',
         DATA_SOURCE = 'OREGON FISH COMMISSION 1951')
king_f7a <- read_csv('data/King_etal_2017_California tope landings_1938-1980.csv')

# Convert to long-format based on GEAR_CODE for MTON, PPP (price per pound), and CONFIDENTIAL_FLAG values
pacfin_g_mton <- pivot_longer(pacfin_g, cols = seq(8, ncol(pacfin_g), by = 4), names_to = 'GEAR_CODE', values_to = 'LANDED_WEIGHT_MTONS') %>%
  mutate(GEAR_CODE = sub('\\_.*', '', GEAR_CODE)) %>%
  select(c(1:2,38,39,37))
pacfin_g_ppp <- pivot_longer(pacfin_g, cols = seq(9, ncol(pacfin_g), by = 4), names_to = 'GEAR_CODE', values_to = 'LANDED_WEIGHT_PPP') %>%
  mutate(GEAR_CODE = sub('\\_.*', '', GEAR_CODE)) %>%
  select(c(1:2,38,39,37))
pacfin_g_confidentialFlag <- pivot_longer(pacfin_g, cols = seq(7, ncol(pacfin_g)-1, by = 4), names_to = 'GEAR_CODE', values_to = 'CONFIDENTIAL_FLAG') %>%
  mutate(GEAR_CODE = sub('\\_.*', '', GEAR_CODE)) %>%
  select(c(1:2,38,39,37))

# Merge the three datasets
pacfin_g_long <- left_join(left_join(pacfin_g_mton, pacfin_g_ppp), pacfin_g_confidentialFlag)

# Format historical dataset for merging
pacfin_g_foss_ripley_odfw_king <- pacfin_foss %>%
  mutate(AGENCY_CODE = ifelse(State == 'PROCESS AT SEA', 'AT-SEA', substr(State, 1, 1)),
         DATA_SOURCE = 'NOAA FOSS') %>%
  rename(LANDING_YEAR = Year,
         PACFIN_SPECIES_COMMON_NAME = `NMFS Name`,
         LANDED_WEIGHT_MTONS = `Metric Tons`,
         CONFIDENTIAL_FLAG = Confidentiality) %>%
  select(LANDING_YEAR, AGENCY_CODE, PACFIN_SPECIES_COMMON_NAME, LANDED_WEIGHT_MTONS, CONFIDENTIAL_FLAG, DATA_SOURCE) %>%
  filter(LANDING_YEAR <= 1980) %>%
  bind_rows(pacfin_g_long, ripley_t4, odfw_tope, king_f7a) %>%
  mutate(GEAR_CODE = ifelse(is.na(GEAR_CODE), 'MSC', GEAR_CODE)) %>%
  filter(GEAR_CODE != 'TOTAL') %>%
  arrange(LANDING_YEAR, AGENCY_CODE)

# Reorder GEAR_CODE and AGENCY_CODE levels for plot legend
pacfin_g_foss_ripley_odfw_king$GEAR_CODE <- factor(pacfin_g_foss_ripley_odfw_king$GEAR_CODE, levels = c(unique(pacfin_g_foss_ripley_odfw_king$GEAR_CODE)[2:9], 'MSC'))
pacfin_g_foss_ripley_odfw_king$AGENCY_CODE <- factor(pacfin_g_foss_ripley_odfw_king$AGENCY_CODE, levels = c('C','O','W','WOC','AT-SEA'))

# Subset only data from California (C), Oregon (O), and Washington (W)
pacfin_g_foss_ripley_odfw_king <- filter(pacfin_g_foss_ripley_odfw_king, AGENCY_CODE %in% c('C','O','W')) %>%
  mutate(AGENCY_CODE = factor(AGENCY_CODE, levels = c('W','O','C')))

state_names <- list(
  'W'='WASHINGTON',
  'O'='OREGON',
  'C' = 'CALIFORNIA'
)
state_labeller <- function(variable,value){
  return(state_names[value])
}

##Figure edit: removed data before 1981 
# Plot landed weight (metric tons) by AGENCY_CODE
pacfin_g_foss_ripley_mton_plot <- ggplot(data = pacfin_g_foss_ripley_odfw_king, aes(x = LANDING_YEAR, y = LANDED_WEIGHT_MTONS, fill = GEAR_CODE))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'))+
  ggtitle('U.S. West Coast Shark Landings',
          subtitle = 'Tope sharks (1941-2024)')+
  ylab('Landed Weight (mt)')+
  geom_bar(position = 'stack', stat = 'identity', width = 1, color = 'black', linewidth = 0.1)+
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(name = 'Paired', n = 8), 'gray40'), name = 'Gear Type',
                    labels = c('DRG'='Dredge', 'HKL'='Hook & Line (except troll)', 'NET'='Net gear (except trawl)', 'POT'='Pot & Trap',
                               'TLS'='Troll', 'TWL'='Trawls (except shrimp)', 'TWS'='Shrimp Trawl', 'NTW'='Non-Trawl',
                               'MSC'='Unknown'))+
  coord_cartesian(ylim = c(0, 130))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     breaks = c(0,25,50,75,100,125))+
  facet_wrap(~AGENCY_CODE, nrow = 3, labeller = state_labeller)
pacfin_g_foss_ripley_mton_plot

cowplot::save_plot('figures/NE_Pacific/PacFIN_byGear_mton_withHistorical.png', pacfin_g_foss_ripley_mton_plot, base_height = 6, base_width = 7)

# Plot average price per pound by AGENCY_CODE
pacfin_g_COW_avgPPP <- pacfin_g_foss_COW %>%
  group_by(LANDING_YEAR, GEAR_CODE, AGENCY_CODE) %>%
  summarize(PPP = mean(LANDED_WEIGHT_PPP, na.rm = T))
pacfin_g_ppp_plot <- ggplot(data = pacfin_g_COW_avgPPP, aes(x = LANDING_YEAR, y = PPP))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('PacFIN Price Per Pound (1981-2022)')+
  ylab('Price per Pound ($)')+
  geom_bar(stat = 'identity')+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_grid(GEAR_CODE~AGENCY_CODE)
pacfin_g_ppp_plot

cowplot::save_plot('figures/NE_Pacific/PacFIN_byGear_PPP.png', pacfin_g_ppp_plot, base_height = 8, base_width = 8)


# PacFIN (by Area) ------------------------------------------------------------------

# Load PacFIN_byArea dataset
pacfin_a <- read_csv('data/PacFIN/PacFIN_1981-2022_byArea.csv')

# Convert to long-format based on FISHING_AREA for MTON, PPP (price per pound), and CONFIDENTIAL_FLAG values
pacfin_a_mton <- pivot_longer(pacfin_a, cols = seq(7, ncol(pacfin_a), by = 4), names_to = 'FISHING_AREA', values_to = 'LANDED_WEIGHT_MTONS') %>%
  mutate(FISHING_AREA = sub('\\_.*', '', FISHING_AREA)) %>%
  select(c(1:6,40,41))
pacfin_a_ppp <- pivot_longer(pacfin_a, cols = seq(8, ncol(pacfin_a), by = 4), names_to = 'FISHING_AREA', values_to = 'LANDED_WEIGHT_PPP') %>%
  mutate(FISHING_AREA = sub('\\_.*', '', FISHING_AREA)) %>%
  select(c(1:6,40,41))
pacfin_a_confidentialFlag <- pivot_longer(pacfin_a, cols = seq(10, ncol(pacfin_a), by = 4), names_to = 'FISHING_AREA', values_to = 'CONFIDENTIAL_FLAG') %>%
  mutate(FISHING_AREA = sub('\\_.*', '', FISHING_AREA)) %>%
  select(c(1:6,40,41))

# Merge the three datasets
pacfin_a_long <- left_join(left_join(pacfin_a_mton, pacfin_a_ppp), pacfin_a_confidentialFlag)

# Change FISHING_AREA labels for plotting
pacfin_a_long$FISHING_AREA[pacfin_a_long$FISHING_AREA == 'VN'] <- 'Vancouver (US)'
pacfin_a_long$FISHING_AREA[pacfin_a_long$FISHING_AREA == 'CL'] <- 'Columbia'
pacfin_a_long$FISHING_AREA[pacfin_a_long$FISHING_AREA == 'EK'] <- 'Eureka'
pacfin_a_long$FISHING_AREA[pacfin_a_long$FISHING_AREA == 'MT'] <- 'Monterey'
pacfin_a_long$FISHING_AREA[pacfin_a_long$FISHING_AREA == 'CP'] <- 'Conception'
pacfin_a_long$FISHING_AREA[pacfin_a_long$FISHING_AREA == 'UI'] <- 'Unknown'

# Reorder FISHING_AREA and AGENCY levels for plot legend
levels(pacfin_a_long$FISHING_AREA)
pacfin_a_long$FISHING_AREA <- factor(pacfin_a_long$FISHING_AREA, levels = c('Vancouver (US)','Columbia','PG','CR','OC','Eureka','Monterey','Conception','Unknown','ZZ'))
pacfin_a_long$AGENCY <- factor(pacfin_a_long$AGENCY, levels = c('C','O','W','WOC','AT-SEA'))

# Subset only data from California (C), Oregon (O), and Washington (W) 
# Remove fishing areas with no data
pacfin_a_COW <- filter(pacfin_a_long, AGENCY %in% c('C','O','W')) %>%
  filter(FISHING_AREA %in% c('Vancouver (US)','Columbia','Eureka','Monterey','Conception','Unknown'))

# Plot data by AGENCY
pacfin_a_mton_plot <- ggplot(data = pacfin_a_COW, aes(x = YEAR, y = LANDED_WEIGHT_MTONS, fill = AGENCY))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('PacFIN Landings Data (1981-2022)')+
  ylab('Landed Weight (tonnes)')+
  #  geom_point()+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(name = 'Set2', n = 7)))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~FISHING_AREA, ncol = 1)
pacfin_a_mton_plot

cowplot::save_plot('figures/NE_Pacific/PacFIN_byArea_mton_sameScale.png', pacfin_a_mton_plot, base_height = 8, base_width = 4)


# PacFIN (from WCR) -------------------------------------------------------

# Load PacFIN dataset provided by WCR SFD
pacfin_wcr <- read_csv('data/PacFIN/SSRK_WOC05132022_PacFIN.csv')

# Summarize by Year, Agency, and Port Area
# NOTE: Includes data reported by AKFSC (AGENCY_CODE = F) from the 'at-sea hake' fishery (possibly observer data)
# (All AKFSC-reported catch landed at Vancouver, Columbia, and Eureka ports)
pacfin_wcr_summ <- group_by(pacfin_wcr, PACFIN_YEAR, AGENCY_CODE, IOPAC_PORT_GROUP_AREA) %>%
  summarise(LANDED_MTONS = sum(ROUND_WEIGHT_MTONS)) %>%
  mutate(IOPAC_PORT_GROUP_AREA = replace_na(IOPAC_PORT_GROUP_AREA, 'UNKNOWN'))

# Plot
pacfin_wcr_mton_plot <- ggplot(data = pacfin_wcr_summ, aes(x = PACFIN_YEAR, y = LANDED_MTONS, fill = IOPAC_PORT_GROUP_AREA))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('PacFIN Landings Data (2002-2022)')+
  ylab('Landed Weight (tonnes)')+
  #  geom_point()+
  geom_bar(position = 'stack', stat = 'identity')+
#  scale_fill_manual(values = c(RColorBrewer::brewer.pal(name = 'Set2', n = 7)))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~AGENCY_CODE, ncol = 1, scales = 'free_y')
pacfin_wcr_mton_plot


# Compare PacFIN datasets -------------------------------------------------

pacfin_online_compare <- group_by(pacfin_g, LANDING_YEAR, AGENCY_CODE) %>%
  summarize(WEIGHT_MTONS_ONLINE = sum(TOTAL_LANDED_WEIGHT_MTONS)) %>%
  rename(YEAR = LANDING_YEAR)
pacfin_wcr_compare <- group_by(pacfin_wcr, PACFIN_YEAR, AGENCY_CODE) %>%
  summarize(WEIGHT_MTONS_WCR = sum(ROUND_WEIGHT_MTONS)) %>%
  rename(YEAR = PACFIN_YEAR)
pacfin_compare <- full_join(pacfin_online_compare, pacfin_wcr_compare)

pacfin_compare_long <- pivot_longer(pacfin_compare, cols = c(3,4), names_to = 'DATASET', values_to = 'WEIGHT_MTONS') %>%
  mutate(DATASET = ifelse(DATASET == 'WEIGHT_MTONS_ONLINE', 'Online','WCR'),
         WEIGHT_MTONS = replace_na(WEIGHT_MTONS, 0)) %>%
  filter(AGENCY_CODE != 'AT-SEA')

ggplot(data = pacfin_compare_long, aes(x = YEAR, y = WEIGHT_MTONS, group = DATASET, fill = DATASET))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('PacFIN Landings Data (1981-2022)')+
  ylab('Landed Weight (tonnes)')+
  #  geom_point()+
  geom_bar(position = 'dodge', stat = 'identity')+
  #  scale_fill_manual(values = c(RColorBrewer::brewer.pal(name = 'Set2', n = 7)))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~AGENCY_CODE, scales = 'free_y')


# U.S. West Coast Shark Landings (1916-2023) -----------------------------------

foss_landings <- read_csv('data/PacFIN/FOSS_1950-2024.csv') %>%
  mutate(Tope = ifelse(`Scientific Name` == 'Galeorhinus galeus', 'Tope', ifelse(`Scientific Name` == 'Chondrichthyes', 'Unknown', 'Other species')))
bulletins_pre1950 <- read_csv('data/CA_FishBulletins_1916-1999.csv') %>%
  filter(Year < 1950) %>%
  mutate(State = factor(State))
ripley_t4 <- read_csv('data/Ripley_1946_Table4.csv') %>%
  group_by(Year) %>%
  summarise(Pounds = sum(Landed_lbs)) %>%
  mutate(`Metric Tons` = Pounds / 2204.62,
         State = 'CALIFORNIA',
         Tope = 'Tope',
         Source = 'Ripley (1946)')
odfw <- read_csv('data/ODFW1951_Table1.csv')
king_f7a <- read_csv('data/King_etal_2017_California tope landings_1938-1980.csv') %>%
  mutate(State = ifelse(AGENCY_CODE == 'C', 'CALIFORNIA', 'OREGON'),
         Tope = 'Tope',
         Source = 'King et al (2017)') %>%
  rename(Year = LANDING_YEAR,
         `Metric Tons` = LANDED_WEIGHT_MTONS) %>%
  select(Year, State, Tope, `Metric Tons`, Source)

foss_summ <- group_by(foss_landings, Year, State, Tope) %>%
  summarize(Pounds = sum(Pounds, na.rm = T),
            `Metric Tons` = sum(`Metric Tons`, na.rm = T)) %>%
  mutate(Source = 'NOAA FOSS')

tope_landings <- rbind(rbind(rbind(foss_summ, ripley_t4), odfw), king_f7a) %>%
  mutate(State = factor(State, levels = c('WASHINGTON','OREGON','CALIFORNIA')),
         Tope = factor(Tope, levels = c('Tope','Other species','Unknown'))) %>%
  select(-Pounds) %>%
  arrange(Year, State, Tope, Source)

#Subtract King et al. (2017) values from 'Unknown' values from other sources (unless 'Tope' values are provided by another source)
tope_landings_adj <- tope_landings %>%
  group_by(Year, State) %>%
  mutate(
    # Check if ANY source other than King et al. reports "Tope" this year/state
    other_tope_present = any(Source != 'King et al (2017)' & Tope == 'Tope', na.rm = TRUE),
    # Determine the weight to subtract from "Unknown" rows.
    # If another Tope source is present, we set this to 0 because King et al. will be removed.
    # Otherwise, we grab the King et al. Tope weight.
    king_tope_weight = if_else(other_tope_present == TRUE, 0, sum(`Metric Tons`[Source == 'King et al (2017)' & Tope == 'Tope'], na.rm = TRUE)),
    # Subtract the King et al. weight from other source's Unknown category.
    `Metric Tons` = if_else(Tope == 'Unknown' & Source != 'King et al (2017)', `Metric Tons` - king_tope_weight, `Metric Tons`)) %>%
  # Filter out the King et al. Tope row if another Tope source is already present
  filter(!(other_tope_present == TRUE & Source == 'King et al (2017)')) %>%
  select(-other_tope_present, -king_tope_weight) %>%
  ungroup()

landings_plot <- ggplot(data = tope_landings_adj, aes(x = Year, y = `Metric Tons`, fill = Tope))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'))+
  ggtitle('U.S. West Coast Shark Landings',
          subtitle = 'All sharks (1916-2024)')+
  ylab('Landed Weight (mt)')+
  geom_bar(data = bulletins_pre1950, aes(x = Year, y = `Metric Tons`), fill = 'gray90', stat = 'identity', width = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity', width = 1)+
  scale_fill_manual(name = 'Species', values = c('gray40', 'gray70', 'gray90'))+
  scale_x_continuous(limits = c(1915,2024), breaks = seq(1920, 2020, by = 20))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~State, nrow = 3)
landings_plot

cowplot::save_plot('figures/NE_Pacific/WestCoast_sharkLandings_1916-2024.png', landings_plot, base_height = 6, base_width = 5)


# RecFIN (online) ------------------------------------------------------------------

# Load RecFIN dataset
recfin <- read_csv('data/RecFIN/RecFIN_1990-2024.csv')

# Sum all data for each year and region
recfin_summ_byArea <- group_by(recfin, RECFIN_YEAR, AGENCY, DISTRICT_NAME) %>%
  summarise_at(vars(SUM_RETAINED_NUM:SUM_TOTAL_MORTALITY_MT), sum, na.rm = TRUE) %>%
  mutate(TOTAL_CATCH_NUM = SUM_RETAINED_NUM + SUM_RELEASED_ALIVE_NUM + SUM_RELEASED_DEAD_NUM,
         TOTAL_CATCH_MT = SUM_RETAINED_MT + SUM_RELEASED_ALIVE_MT + SUM_RELEASED_DEAD_MT)

# Change DISTRICT_NAME labels for plotting
recfin_summ_byArea$DISTRICT_NAME[startsWith(recfin_summ_byArea$DISTRICT_NAME, 'Bay')] <- 'Bay Area'
recfin_summ_byArea$DISTRICT_NAME[startsWith(recfin_summ_byArea$DISTRICT_NAME, 'Central (San Luis Obispo')] <- 'Central California'
recfin_summ_byArea$DISTRICT_NAME[startsWith(recfin_summ_byArea$DISTRICT_NAME, 'Channel')] <- 'Channel Islands'
recfin_summ_byArea$DISTRICT_NAME[startsWith(recfin_summ_byArea$DISTRICT_NAME, 'Redwood')] <- 'Redwoods'
recfin_summ_byArea$DISTRICT_NAME[startsWith(recfin_summ_byArea$DISTRICT_NAME, 'South (San Diego')] <- 'Southern California'
recfin_summ_byArea$DISTRICT_NAME[startsWith(recfin_summ_byArea$DISTRICT_NAME, 'Wine')] <- 'Mendocino County'

# Reorder DISTRICT_NAME and AGENCY levels for plot legend
levels(recfin_summ_byArea$DISTRICT_NAME)
recfin_summ_byArea$DISTRICT_NAME <- factor(recfin_summ_byArea$DISTRICT_NAME, 
                                           levels = c('Northern Oregon',
                                                      'Central Oregon',
                                                      'Southern Oregon',
                                                      'Redwoods',
                                                      'Mendocino County',
                                                      'Bay Area',
                                                      'Central California',
                                                      'Channel Islands',
                                                      'Southern California'))

# Plot data by DISTRICT_NAME
total_catch_plot <- ggplot(data = recfin_summ_byArea, aes(x = RECFIN_YEAR, y = TOTAL_CATCH_MT, fill = AGENCY))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('RecFIN Landings Data\n(2004-2024)')+
  ylab('Total Catch (tonnes)')+
  #  geom_point()+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(name = 'Set2', n = 3)))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~DISTRICT_NAME, ncol = 1, scales = 'free_y')
total_catch_plot

cowplot::save_plot('figures/NE_Pacific/RecFIN_byArea_mton.png', total_catch_plot, base_height = 10, base_width = 3)


# RecFIN (WCR) ------------------------------------------------------------

recfin_wcr <- read_csv('data/RecFIN/Rec Catch_Soupfin Shark_2005-2021_RecFIN Report CTE001.csv')

# Sum all data for each year and region
recfin_wcr_summ_byArea <- group_by(recfin_wcr, `RecFIN Year`, Agency, `District Name`) %>%
  summarise_at(vars(`Retained (# fish)`:`Total Mortality (mt)`), sum, na.rm = TRUE) %>%
  mutate(TOTAL_CATCH_NUM_WCR = `Total Mortality (# fish)` + `Released Alive (# fish)`,
         TOTAL_CATCH_MT_WCR = `Total Mortality (mt)` + `Released Alive (mt)`)

# Change District Name labels for plotting
recfin_wcr_summ_byArea$`District Name`[startsWith(recfin_wcr_summ_byArea$`District Name`, 'Bay')] <- 'Bay Area'
recfin_wcr_summ_byArea$`District Name`[startsWith(recfin_wcr_summ_byArea$`District Name`, 'Central (San Luis Obispo')] <- 'Central California'
recfin_wcr_summ_byArea$`District Name`[startsWith(recfin_wcr_summ_byArea$`District Name`, 'Channel')] <- 'Channel Islands'
recfin_wcr_summ_byArea$`District Name`[startsWith(recfin_wcr_summ_byArea$`District Name`, 'Redwood')] <- 'Redwoods'
recfin_wcr_summ_byArea$`District Name`[startsWith(recfin_wcr_summ_byArea$`District Name`, 'South (San Diego')] <- 'Southern California'
recfin_wcr_summ_byArea$`District Name`[startsWith(recfin_wcr_summ_byArea$`District Name`, 'Wine')] <- 'Mendocino County'

# Compare with data set downloaded from internet
recfin_wcr_compare <- recfin_wcr_summ_byArea %>%
  rename(RECFIN_YEAR = `RecFIN Year`, AGENCY = Agency, DISTRICT_NAME = `District Name`) %>%
  select(c(1:3, 12:13))
recfin_online_compare <- recfin_summ_byArea %>%
  select(c(1:3, 12:13))
recfin_compare <- full_join(recfin_online_compare, recfin_wcr_compare) %>%
  mutate(DIFF_NUM = TOTAL_CATCH_NUM - TOTAL_CATCH_NUM_WCR,
         DIFF_MT = TOTAL_CATCH_MT - TOTAL_CATCH_MT_WCR)

ggplot(data = recfin_compare, aes(x = RECFIN_YEAR, y = DIFF_MT, group = AGENCY, fill = AGENCY))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('RecFIN Landings Data (Online vs. WCR)')+
  ylab('Difference')+
  geom_hline(yintercept = 0)+
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_y_continuous(limits = c(-0.01, 0.01), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~DISTRICT_NAME)
# Essentially no difference


# RecFIN (CDFW) -----------------------------------------------------------

recfin_cdfw <- read_csv('data/RecFIN/CTE001-California-2010---2021_SoupfinSummary.csv')

# Sum all data for each year and region
recfin_cdfw_summ_byArea <- group_by(recfin_cdfw, RECFIN_YEAR, AGENCY, DISTRICT_NAME) %>%
  summarise_at(vars(SUM_RETAINED_NUM:SUM_TOTAL_MORTALITY_MT), sum, na.rm = TRUE) %>%
  mutate(TOTAL_CATCH_NUM_CDFW = SUM_RETAINED_NUM + SUM_RELEASED_ALIVE_NUM + SUM_RELEASED_DEAD_NUM,
         TOTAL_CATCH_MT_CDFW = SUM_RETAINED_MT + SUM_RELEASED_ALIVE_MT + SUM_RELEASED_DEAD_MT)

# Change DISTRICT_NAME labels for plotting
recfin_cdfw_summ_byArea$DISTRICT_NAME[startsWith(recfin_cdfw_summ_byArea$DISTRICT_NAME, 'Bay')] <- 'Bay Area'
recfin_cdfw_summ_byArea$DISTRICT_NAME[startsWith(recfin_cdfw_summ_byArea$DISTRICT_NAME, 'Central (San Luis Obispo')] <- 'Central California'
recfin_cdfw_summ_byArea$DISTRICT_NAME[startsWith(recfin_cdfw_summ_byArea$DISTRICT_NAME, 'Channel')] <- 'Channel Islands'
recfin_cdfw_summ_byArea$DISTRICT_NAME[startsWith(recfin_cdfw_summ_byArea$DISTRICT_NAME, 'Redwood')] <- 'Redwoods'
recfin_cdfw_summ_byArea$DISTRICT_NAME[startsWith(recfin_cdfw_summ_byArea$DISTRICT_NAME, 'South (San Diego')] <- 'Southern California'
recfin_cdfw_summ_byArea$DISTRICT_NAME[startsWith(recfin_cdfw_summ_byArea$DISTRICT_NAME, 'Wine')] <- 'Mendocino County'

# Compare with data set downloaded from internet
recfin_cdfw_compare <- recfin_cdfw_summ_byArea %>%
  select(c(1:3, 12:13))
recfin_compare <- full_join(recfin_online_compare, recfin_cdfw_compare) %>%
  mutate(DIFF_NUM = TOTAL_CATCH_NUM - TOTAL_CATCH_NUM_CDFW,
         DIFF_MT = TOTAL_CATCH_MT - TOTAL_CATCH_MT_CDFW)
# Essentially no difference


# WCGOP Groundfish Expanded Multi-year Mortality (GEMM) -------------------
# https://repository.library.noaa.gov/view/noaa/22943

gemm_online <- read_csv('data/GEMM_2002-2023.csv')
gemm_wcr <- read_csv('data/SSRK_WOC05132022_GEMM.csv')

#Compare GEMM data sets
gemm_online_compare <- gemm_online %>%
  mutate(sector = toupper(sector)) %>%
  select(year, sector, total_discard_mt)
gemm_wcr_compare <- gemm_wcr %>%
  rename(year = YEAR, sector = FOS_GROUNDFISHSECTOR_CODE, total_discard_mt_wcr = TOTAL_DISCARD_MT) %>%
  select(year, sector, total_discard_mt_wcr)
gemm_compare <- full_join(gemm_online_compare, gemm_wcr_compare)
# Essentially the same; online data set is more comprehensive, but is missing some shoreside hake data for certain years

# Add missing shoreside hake data to online data set
missing_online <- filter(gemm_compare, is.na(total_discard_mt)) %>%
  mutate(total_discard_mt = total_discard_mt_wcr) %>%
  select(-total_discard_mt_wcr) %>%
  mutate(sector = c(rep('Shoreside Hake', 4), 'Ridgeback Prawn Trawl'))
gemm <- full_join(gemm_online, missing_online)

gemm_totalCatch <- gemm %>%
  select(-c(2,3,5, 8:10))
  # Retaining total discard and landings data to understand total catch (discard mortality not relevant to CPUE estimate)

gemm_mort <- gemm %>%
  select(c(1,4,9,10)) %>%
  rename(Discards = total_discard_with_mort_rates_applied_mt,
         Landings = total_landings_mt) %>%
  pivot_longer(cols = c("Discards", "Landings"),
               names_to = "Type",
               values_to = "Mortality") %>%
  mutate(Fishery = ifelse(sector %in% c('At-Sea Hake CP', 'At-Sea Hake MSCV', 'Tribal At-Sea Hake','Midwater Hake','Midwater Hake EM','Midwater Rockfish','Midwater Rockfish EM','Shoreside Hake', 'Tribal Shoreside'), 'Midwater Trawl',
                          ifelse(sector %in% c('Limited Entry Trawl','CS - Bottom Trawl','CS EM - Bottom Trawl','CS - Bottom and Midwater Trawl','Combined LE & OA CA Halibut','LE CA Halibut','OA CA Halibut'), 'Bottom Trawl',
                                 ifelse(sector %in% c('CS - Hook & Line','LE Fixed Gear DTL - Hook & Line','LE Sablefish - Hook & Line','OA Fixed Gear - Hook & Line','OA Fixed Gear - Pot','Nearshore'), 'Fixed Gear',
                                        ifelse(sector %in% c('California Recreational','Oregon Recreational'), 'Recreational*',
                                               ifelse(sector == 'Incidental','Incidental*','Other')))))) %>%
  filter(Fishery != 'Other') %>%
  mutate(Fishery = factor(Fishery, levels = c('Bottom Trawl','Midwater Trawl','Fixed Gear','Incidental*','Recreational*'))) %>%
  group_by(year, Type, Fishery) %>%
  summarise(Mortality = sum(Mortality, na.rm = T))

# Plot discards and landings (TOTAL CATCH)
gemm_catch_plot <- ggplot(data = gemm_totalCatch, aes(x = year, y = total_discard_and_landings_mt))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('GEMM Catch Data (2002-2023)')+
  ylab('Total Catch (tonnes)')+
  geom_bar(stat = 'identity')+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~sector, scales = 'free_y')
gemm_catch_plot

# Plot discards and landings (TOTAL MORTALITY)
gemm_mort_plot <- ggplot(data = gemm_mort, aes(x = year, y = Mortality, fill = Type))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'))+
  ggtitle('Total Mortality in U.S. Commercial Groundfish and Recreational Fisheries')+
  ylab('Weight (mt)')+
  geom_bar(stat = 'identity', position = 'stack', width = 1)+
  scale_fill_manual(name = 'Type', values = c(RColorBrewer::brewer.pal(name = 'Paired', n = 4)))+
  scale_y_continuous(limits = c(0, 13), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~Fishery, scales = 'free', ncol = 3)
gemm_mort_plot

cowplot::save_plot('figures/NE_Pacific/GEMM_catch_mton.png', gemm_catch_plot, base_height = 8, base_width = 14)
cowplot::save_plot('figures/NE_Pacific/GEMM_mortality_mton.png', gemm_mort_plot, base_height = 5, base_width = 8)


# West Coast Groundfish Observer Program (WCGOP) discard and landings data --------------------------------------
# Provided by NWFSC/WCR
# Color: discard v. landing, Facet: gear x agency

wcgop_full <- read_csv('data/WCGOP_full_coverage_sectors.csv') %>%
  mutate(obs_discard_per_haul_mt = obs_byc_mt / n_obs_hauls,
         obs_discard_per_vessel_mt = obs_byc_mt / n_obs_ves,
         dataset = 'full') %>%
  mutate(state = case_when(
    state == "CA" ~ "California",
    state == "OR" ~ "Oregon",
    state == "WA" ~ "Washington",
    TRUE ~ state
  ))
wcgop_partial <- read_csv('data/WCGOP_partial_coverage_sectors.csv') %>%
  mutate(obs_discard_per_haul_mt = obs_byc_mt / n_obs_hauls,
         obs_discard_per_vessel_mt = obs_byc_mt / n_obs_ves,
         dataset = 'partial') %>%
  mutate(state = case_when(
    state == "CA" ~ "California",
    state == "OR" ~ "Oregon",
    state == "WA" ~ "Washington",
    TRUE ~ state
  ))
wcgop_landings <- read_csv('data/WCGOP_landings.csv') %>%
  mutate(landed_per_vessel_mt = landed_mt / n_ves,
         state = case_when(
           state == "CA" ~ "California", 
           state == "OR" ~ "Oregon", 
           state == "WA" ~ "Washington",
           TRUE ~ state
           ))
wcgop_landings_byGear = wcgop_landings %>%
  group_by(year, gear, state) %>%
  summarise(landed_mt = sum(landed_mt, na.rm = T),
            n_ves = sum(n_ves, na.rm = T)) %>%
  mutate(landed_per_vessel_mt = landed_mt / n_ves)

# Combine discard data sets for plotting
wcgop_discard_per_haul_all <- rbind(select(wcgop_full, c(1:4, 12, 14)), select(wcgop_partial, c(1:4, 18, 20))) %>%
  filter(gear != 'Pot') #No topes observed in pot gear

# Total catch = landings per vessel + discards per vessel (averaged between full and partial data sets)
wcgop_catch_per_vessel_all <- rbind(select(wcgop_full, c(1:4, 13)), select(wcgop_partial, c(1:4, 19))) %>%
  group_by(year, gear, state) %>%
  summarise(obs_discard_per_vessel_mt = mean(obs_discard_per_vessel_mt)) %>%
  ungroup() %>%
  full_join(select(wcgop_landings_byGear, c(1:3, 6))) %>%
  rowwise() %>%
  mutate(catch_per_vessel_mt = sum(obs_discard_per_vessel_mt, landed_per_vessel_mt, na.rm = T)) %>%
  filter(gear != 'Pot') %>%
  filter(gear != 'Other Fixed Gears') %>%
  filter(gear != 'Other Miscellaneous Gears')

# Reorder gear categories for plotting
wcgop_discard_per_haul_ordered <- wcgop_discard_per_haul_all %>%
  mutate(gear = factor(gear, levels = c('Bottom Trawl',
                                        'Bottom/Midwater Trawl',
                                        'Midwater Trawl',
                                        'Shrimp Trawl',
                                        'Hook & Line',
                                        'Fixed Gears')))
wcgop_catch_per_vessel_ordered <- wcgop_catch_per_vessel_all %>%
  mutate(gear = factor(gear, levels = c('Bottom Trawl',
                                        'Bottom/Midwater Trawl',
                                        'Midwater Trawl',
                                        'Shrimp Trawl',
                                        'Hook & Line',
                                        'Fixed Gears',
                                        'Other Net Gears')))

# Another option for stacked bar plot
wcgop_catch_per_vessel_stacked <- wcgop_catch_per_vessel_all %>%
  pivot_longer(cols = c(obs_discard_per_vessel_mt, landed_per_vessel_mt),
               names_to = 'catch_type',
               values_to = 'weight_per_vessel_mt') %>%
  select(-catch_per_vessel_mt) %>%
  mutate(gear = factor(gear, levels = c('Bottom Trawl',
                                        'Bottom/Midwater Trawl',
                                        'Midwater Trawl',
                                        'Shrimp Trawl',
                                        'Hook & Line',
                                        'Fixed Gears',
                                        'Other Net Gears')),
         catch_type = case_when(
           catch_type == "obs_discard_per_vessel_mt" ~ "Discard", 
           catch_type == "landed_per_vessel_mt" ~ "Landed",
           TRUE ~ catch_type
         ))


all_discard_per_haul_plot <- ggplot(data = wcgop_discard_per_haul_ordered, aes(x = year, y = obs_discard_per_haul_mt*1000, fill = dataset))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom')+
  ggtitle('Tope Shark Discard Rate - WCGOP\n(2002-2021)')+
  ylab('Observed discard weight per 1,000 hauls (mt)')+
  #  geom_point()+
  geom_bar(position ='dodge', stat = 'identity')+
  scale_fill_manual(name = 'Observer coverage', labels = c('Full', 'Partial'),
                    values = c(RColorBrewer::brewer.pal(name = 'Set2', n = 3)))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_grid(gear~state, scales = 'free_y', labeller = labeller(gear = label_wrap_gen(10)))
all_discard_per_haul_plot

all_catch_per_vessel_plot <- ggplot(data = wcgop_catch_per_vessel_ordered, aes(x = year, y = catch_per_vessel_mt))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('Total Tope Shark Catch (Landings + Discards) - WCGOP\n(2002-2021)')+
  ylab('Catch per vessel (mt)')+
  geom_bar(position = 'dodge', stat = 'identity')+
  #scale_fill_manual(values = c(RColorBrewer::brewer.pal(name = 'Set2', n = 3)))+
  scale_y_continuous(limits = c(0, 0.12), expand = expansion(mult = c(0, 0.05)), breaks = c(0,0.05,0.1))+
  facet_grid(gear~state, labeller = labeller(gear = label_wrap_gen(15)))
all_catch_per_vessel_plot

all_catch_per_vessel_plot_stacked <- ggplot(data = wcgop_catch_per_vessel_stacked, aes(x = year, y = weight_per_vessel_mt, fill = catch_type))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('Total Tope Shark Catch (Landings + Discards) - WCGOP\n(2002-2021)')+
  ylab('Catch per vessel (mt)')+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_manual(name = 'Catch', values = c(RColorBrewer::brewer.pal(name = 'Paired', n = 4)))+
  scale_y_continuous(limits = c(0, 0.12), expand = expansion(mult = c(0, 0.05)), breaks = c(0,0.05,0.1))+
  facet_grid(gear~state, labeller = labeller(gear = label_wrap_gen(15)))
all_catch_per_vessel_plot_stacked

cowplot::save_plot('figures/NE_Pacific/WCGOP_Discard_per_haul.png', all_discard_per_haul_plot, base_height = 8, base_width = 8)
cowplot::save_plot('figures/NE_Pacific/WCGOP_Catch_per_vessel.png', all_catch_per_vessel_plot, base_height = 9, base_width = 8)
cowplot::save_plot('figures/NE_Pacific/WCGOP_Catch_per_vessel_stacked.png', all_catch_per_vessel_plot_stacked, base_height = 9, base_width = 8)


# At-Sea Hake Observer Program (A-SHOP) discard and landings data (2002-2021) -----------------------
# Provided by NWFSC/WCR

ashop <- read_csv('data/A-SHOP_2002-2021.csv') %>%
  mutate(obs_discard_per_haul_mt = obs_byc_mt / n_obs_hauls,
         obs_discard_per_vessel_mt = obs_byc_mt / n_obs_ves,
         catch_area = factor(catch_area, levels = c('South of 46.25','North of 46.25')))
fmt_names <- list(
  'South of 46.25'='South of 46.25°N',
  'North of 46.25'='North of 46.25°N'
)
fmt_labeller <- function(variable,value){
  return(fmt_names[value])
}

ashop_discard_per_haul_plot <- ggplot(data = ashop, aes(x = year, y = obs_discard_per_haul_mt * 1000, fill = sector))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('Tope Shark Discard Rate - A-SHOP\n(2002-2021)')+
  ylab('Discard weight per 1,000 hauls (mt)')+
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(name = 'Fishery Sector', labels = c('Tribal', 'Catcher-Processor', 'Mothership'), 
                    values = RColorBrewer::brewer.pal(name = 'Set2', n = 3))+
  facet_grid(~catch_area, scales = 'free_y', labeller = fmt_labeller)
ashop_discard_per_haul_plot

cowplot::save_plot('figures/NE_Pacific/A-SHOP_Discard_per_haul.png', ashop_discard_per_haul_plot, base_height = 4, base_width = 8)


# Trawl surveys (1977-2018) -----------------------------------------------

tope_trawls <- read_csv('data/WC_trawl_surveys_1977-2022.csv')
tope_trawls_data <- tope_trawls %>%
  select(c(2,3,19,20,23))
all_trawls_meta <- read_csv('data/WC_trawl_metadata.csv') %>%
  select(c(1:3,5,6,11:12,14:19,21)) %>%
  rename(depth_m = depth_hi_prec_m,
         latitude_dd = latitude_hi_prec_dd,
         longitude_dd = longitude_hi_prec_dd)

all_trawls <- full_join(all_trawls_meta, tope_trawls_data, by = 'trawl_id') %>%
  mutate(date_yyyymmdd = as.Date(as.character(date_yyyymmdd), format = '%Y%m%d'),
         year = as.numeric(format(date_yyyymmdd,"%Y")),
         region = ifelse(latitude_dd >= 46.26, 'W', 
                         ifelse(latitude_dd < 46.26 & latitude_dd >= 42, 'O', 
                                ifelse(latitude_dd < 42 & latitude_dd >= 38.758, 'NC',
                                       ifelse(latitude_dd < 34.975, 'SC', 'CC')))))

# Replace NAs with mean of all other transects in each project
all_trawls_byProject <- all_trawls %>%
  group_by(project) %>%
  summarise(mean_area = mean(area_swept_ha_der, na.rm = T),
            median_area = median(area_swept_ha_der, na.rm = T),
            sd_area = sd(area_swept_ha_der, na.rm = T),
            n_transects = n(),
            n_NAs = sum(is.na(area_swept_ha_der)))
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
all_trawls_imputedArea <- all_trawls %>%
  group_by(project) %>%
  mutate(area_swept_impute = impute.mean(area_swept_ha_der))

#write_csv(all_trawls, file = 'data/WC_trawl_joined.csv', na = "")

all_trawls_summ <- all_trawls_imputedArea %>%
  mutate(region = factor(region, levels = c('W','O','NC','CC','SC'))) %>%
  group_by(region, year) %>%
  summarise(area_swept_ha = sum(area_swept_ha_der, na.rm = T),
            area_swept_ha_imputed = sum(area_swept_impute, na.rm = T),
            total_catch_numbers = sum(total_catch_numbers, na.rm = T),
            total_catch_wt_kg = sum(total_catch_wt_kg, na.rm = T)) %>%
  mutate(year = as.Date(as.character(year), format = '%Y'),
         cpue_numbers_per_ha = total_catch_numbers / area_swept_ha,
         cpue_numbers_per_ha_imputed = total_catch_numbers / area_swept_ha_imputed,
         cpue_kg_per_ha = total_catch_wt_kg / area_swept_ha,
         cpue_kg_per_ha_imputed = total_catch_wt_kg / area_swept_ha_imputed) %>%
  filter(!is.na(region))

# Plot catch over time
trawls_plot <- ggplot(data = all_trawls_summ, aes(x = year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  annotate('rect', xmin = as.Date('1975-07-01'), xmax = as.Date('2002-07-01'), ymin = -Inf, ymax = Inf, fill = 'grey70', alpha = 0.4)+
  geom_col(data = all_trawls_summ, aes(x = year, y = total_catch_numbers), position = 'identity', fill = 'grey40')+
  geom_point(aes(y = (area_swept_ha / 250) - 6), size = 0.5)+
  #  geom_line(aes(y = (area_swept_ha / 250) - 6))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = as.Date('2002-07-01'), linetype = 'dashed')+
  scale_x_date(expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(name = '# Caught', expand = expansion(mult = c(0, 0.05)), limits = c(-6.1, 6), breaks = c(0,2,4,6),
                     sec.axis = sec_axis(~ ., name = 'Area Swept (ha)', breaks = c(-6,-4,-2), labels = c(0, 500, 1000)))+
  facet_wrap(.~region, ncol = 1, labeller = labeller(region = c('W' = 'Washington',
                                                                'O' = 'Oregon', 
                                                                'NC' = 'N California',
                                                                'CC' = 'C California',
                                                                'SC' = 'S California')))
trawls_plot

# Plot CPUE over time
trawls_cpue_plot <- ggplot(data = all_trawls_summ, aes(x = year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  annotate('rect', xmin = as.Date('1975-07-01'), xmax = as.Date('2002-07-01'), ymin = -Inf, ymax = Inf, fill = 'grey70', alpha = 0.4)+
  geom_col(data = all_trawls_summ, aes(x = year, y = cpue_numbers_per_ha_imputed * 100), position = 'identity', fill = 'grey40')+
  geom_point(aes(y = (area_swept_ha_imputed / 250) - 6), size = 0.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = as.Date('2002-07-01'), linetype = 'dashed')+
  scale_x_date(expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(name = '# Caught / 100 ha', expand = expansion(mult = c(0, 0.05)), limits = c(-6, 4), breaks = c(0,1,2,3,4),
                     sec.axis = sec_axis(~ ., name = 'Area Swept (ha)', breaks = c(-6,-4,-2), labels = c(0, 500, 1000)))+
  facet_wrap(.~region, ncol = 1, labeller = labeller(region = c('W' = 'Washington',
                                                                'O' = 'Oregon', 
                                                                'NC' = 'N California',
                                                                'CC' = 'C California',
                                                                'SC' = 'S California')))
trawls_cpue_plot

cowplot::save_plot('figures/NE_Pacific/Trawl_surveys_byRegion.png', trawls_plot, base_height = 8, base_width = 3)
cowplot::save_plot('figures/NE_Pacific/Trawl_surveys_byRegion_areaImputed.png', trawls_cpue_plot, base_height = 8, base_width = 3)

# Map data
westcoast_polygon <- rworldmap::getMap(resolution = 'low') %>%
  raster::crop(raster::extent(-130,-117,30,50))
wc_shape <- sf::st_as_sf(westcoast_polygon)

wc_map <- ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank())+
  geom_hline(yintercept = c(34.975, 38.758, 42, 46.26), linetype = 'dashed')+
  geom_sf(data = wc_shape, fill = 'grey90', color = 'black', size = 0.1)+
  geom_point(data = all_trawls, aes(x=longitude_dd, y=latitude_dd, fill = year, size = total_catch_numbers), shape = 21)+
  viridis::scale_fill_viridis(name = 'Year')+
  scale_size(range = c(2,7.5), name = '# Caught')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
wc_map

cowplot::save_plot('figures/NE_Pacific/Trawl_surveys_byYear.png', wc_map, base_height = 8, base_width = 5)

# CPUE x depth correlation and plot
library(lme4)

depth.mod <- lm(cpue_kg_per_ha_der ~ depth_m, data = trawls)
summary(depth.mod)

ggplot(data = fi_trawls, aes(x = depth_m, y = cpue_kg_per_ha_der))+
  theme_bw()+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_continuous(limits = c(0, 425), expand = c(0,0))+
  scale_y_continuous(limits = c(0, 40), expand = c(0,0))


# CDFW Gillnet Observer data (1983-1989) ----------------------------------

cdfw_gn_obs <- read_csv('~/../Desktop/CDFW tope shark data/Gill Net Survey 1983-89 Soupfin Shark data.csv') %>%
  mutate(DATE = as.Date(DATE, format = '%y/%m/%d'),
         LAT_DD = as.numeric(substr(LAT,1,2)) + (as.numeric(substr(LAT,3,nchar(LAT))))/60,
         LONG_DD = as.numeric(substr(LONG,1,2)) + (as.numeric(substr(LONG,3,nchar(LONG))))/60,
         SETDUR_HRS = sapply(strsplit(SETDUR,':'),
                             function(x) {
                               x = as.numeric(x)
                               x[1]+x[2]/60
                             }),
         NTYPE = as.factor(NTYPE),
         # EFFORT = net length (fathoms -> meters) x net depth (# meshes) x mesh size (inches -> meters) x set duration (hours)
         EFFORT = (NLEN*1.8288) * (MSIZE1*NDEPTH*0.0254) * SETDUR_HRS,
         CPUE = TCAT / EFFORT,
         CPUE_STD = CPUE) %>%
  map_at('CPUE_STD', ~./mean(.)) %>%
  as_tibble()

cdfw_gn_obs_plot <- ggplot(data = cdfw_gn_obs, aes(x = DATE, y = CPUE_STD, group = NTYPE, color = NTYPE))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank())+
  geom_point()
cdfw_gn_obs_plot

cowplot::save_plot('figures/NE_Pacific/CDFW_GNObserver_CPUE.png', cdfw_gn_plot, base_height = 6, base_width = 5)


# CDFW Gillnet Logbook (1981-2021) ----------------------------------------
# (Note from CDFW: An important caveat to pass on from our gillnet log expert is that the data are not appropriate for determining effort on a set basis. Vessel days are fair game.) 

cdfw_gnlog <- read_csv('~/../Desktop/CDFW tope shark data/1980-21_TopeShark_GillnetLogbook_061124.csv') %>%
  mutate(FISHING_DATE = as.Date(FISHING_DATE, format = '%m/%d/%Y'),
         YYYY_MM = as.character(format(FISHING_DATE, '%Y-%m')),
         MONTH = format(FISHING_DATE, '%B'),
         UNIQUE_ID = ifelse(is.na(CDFW_VESSEL_ID), VESSEL_NAME, CDFW_VESSEL_ID),
         EFFORT_TEST = NET_LENGTH * HOURS_NET_SOAKED,
         CPUE_TEST = NOCATCH / EFFORT_TEST)

cdfw_vesseldays <- group_by(cdfw_gnlog, YYYY_MM) %>%
  summarize(VESSEL_DAYS = n(),
            NOCATCH = sum(NOCATCH, na.rm = T),
            AVG_DEPTH = mean(DEPTHS, na.rm = T),
            AVG_NETLENGTH = mean(NET_LENGTH, na.rm = T)) %>%
  mutate(YYYY_MM = as.Date(paste0(YYYY_MM, '-01'), format = '%Y-%m-%d'),
         CPUE_VD = NOCATCH / VESSEL_DAYS)

cdfw_gn_alltrips <- read_csv('~/../Desktop/CDFW tope shark data/CDFW_Gill net trips_1981-2021.csv') %>%
  pivot_longer(!Year, names_to = 'Month', values_to = 'Trips') %>%
  mutate(YYYY_MM = as.Date(paste(Year, Month, '01', sep = '-'), format = '%Y-%B-%d')) %>%
  full_join(cdfw_vesseldays, by = 'YYYY_MM') %>%
  mutate(NOCATCH = replace_na(NOCATCH, 0),
         CPUE_ALLTRIPS = NOCATCH / Trips)

cdfw_cpue_plot <- ggplot(data = cdfw_gn_alltrips, aes(x = YYYY_MM, y = CPUE_ALLTRIPS))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('CDFW Gillnet Logbook (1981-2021) - CPUE')+
  ylab('CPUE (# Captured / Trips)')+
  geom_point()+
  geom_smooth(method = 'gam')+
  scale_y_continuous(limits = c(0,2), expand = expansion(mult = c(0, 0.05)))
cdfw_cpue_plot

cdfw_gn_alltrips_byYear <- group_by(cdfw_gn_alltrips, Year) %>%
  summarize(Trips = sum(Trips),
            NOCATCH = sum(NOCATCH)) %>%
  mutate(CPUE = NOCATCH / Trips)

cdfw_cpue_plot_byYear <- ggplot(data = cdfw_gn_alltrips_byYear, aes(x = Year, y = NOCATCH/10000))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('CDFW Gillnet Logbook (1981-2021)')+
  ylab(expression(paste('# Captured ( ', x10^4, ')', sep="")))+
  geom_bar(stat = 'identity', fill= 'grey70')+
  geom_point(aes(y = (Trips / 20000)), size = 1)+
  geom_line(aes(y = (Trips / 20000)), linewidth = 0.5, linetype = 'dashed')+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1.6), breaks = c(0, 0.5, 1, 1.5),
                     sec.axis = sec_axis(~ ., name = expression(paste('# Gillnet Trips ( ', x10^4, ')', sep="")), breaks = c(0, 10000/20000, 20000/20000, 30000/20000), labels = c(0,1,2,3)))
cdfw_cpue_plot_byYear

cdfw_gn_alltrips$Month <- factor(cdfw_gn_alltrips$Month, levels = month.name)
cdfw_cpue_plot_byMonth <- ggplot(data = cdfw_gn_alltrips, aes(x = Year, y = CPUE_ALLTRIPS))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('CDFW Gillnet Logbook (1981-2021) - CPUE (by month)')+
  ylab('CPUE (# Captured / Trips)')+
  geom_bar(stat = 'identity')+
  scale_y_continuous(limits = c(0,3.5), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~Month, ncol = 4)
cdfw_cpue_plot_byMonth

gntrips_plot <- ggplot(data = cdfw_gn_alltrips, aes(x = MM_YYYY, y = Trips))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('CDFW Gillnet Logbook (1981-2021) - Total Trips')+
  ylab('Trips')+
  geom_point()+
  geom_smooth(method = 'gam')+
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.05)))
gntrips_plot

gntrips_byYear_plot <- ggplot(data = cdfw_gn_alltrips_byYear, aes(x = Year, y = Trips))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('CDFW Gillnet Logbook (1981-2021) - Total Trips')+
  ylab('Trips')+
  geom_bar(stat = 'identity')+
  scale_y_continuous(limits = c(0,30000), expand = expansion(mult = c(0, 0.05)))
gntrips_byYear_plot

depth_plot <- ggplot(data = cdfw_vesseldays, aes(x = MM_YYYY, y = AVG_DEPTH))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('CDFW Gillnet Logbook (1981-2017) - DEPTH')+
  ylab('Average Net Depth')+
  geom_point()+
  geom_smooth(method = 'gam')+
  scale_y_continuous(limits = c(0,250), expand = expansion(mult = c(0, 0.05)))
depth_plot

netlength_plot <- ggplot(data = cdfw_vesseldays, aes(x = MM_YYYY, y = AVG_NETLENGTH))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('CDFW Gillnet Logbook (1981-2017) - NET LENGTH')+
  ylab('Average Net Length')+
  geom_point()+
  geom_smooth(method = 'gam')+
  scale_y_continuous(limits = c(0,1600), expand = expansion(mult = c(0, 0.05)))
netlength_plot

cowplot::save_plot('figures/NE_Pacific/CDFW_GillnetLogbook/GNLog_CPUE_byYear.png', cdfw_cpue_plot_byYear, base_height = 3, base_width = 5)
cowplot::save_plot('figures/NE_Pacific/CDFW_GillnetLogbook/GNLog_CPUE_byMonth.png', cdfw_cpue_plot_byMonth, base_height = 6, base_width = 8)
cowplot::save_plot('figures/NE_Pacific/CDFW_GillnetLogbook/GNLog_Trips_byYear.png', gntrips_byYear_plot, base_height = 3, base_width = 5)
cowplot::save_plot('figures/NE_Pacific/CDFW_GillnetLogbook/GNLog_Depth.png', depth_plot, base_height = 6, base_width = 5)
cowplot::save_plot('figures/NE_Pacific/CDFW_GillnetLogbook/GNLog_NetLength.png', netlength_plot, base_height = 6, base_width = 5)

## Extra analysis (looks at CPUE based on net_length x soak_time, grouped by vessel)
## Need to circle back with CDFW to better understand caveat above re: effort calculations
cdfw_gnlog_100plus <- filter(cdfw_gnlog, !is.na(UNIQUE_ID)) %>%
  filter(!is.na(CPUE_TEST) & is.finite(CPUE_TEST))
  group_by(UNIQUE_ID) %>%
  filter(n() >= 100)

cdfw_gnlog_plot <- ggplot(data = cdfw_gnlog_50plus, aes(x = FISHING_DATE, y = CPUE_TEST, group = UNIQUE_ID, color = UNIQUE_ID))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank())+
  geom_point(size = 0.25)+
#  geom_smooth(method = 'lm', fill = NA)+
  scale_y_continuous(limits = c(0,0.05))+
  facet_wrap(~UNIQUE_ID)
cdfw_gnlog_plot

## Extra EXTRA analysis (plotting monthly location of tope shark catch based on the fishing block ID in ArcGIS)
cdfw_gnlog_summary <- cdfw_gnlog %>%
  mutate(MONTH_DATE = as.Date(paste0(YYYY_MM, '-01')),
         DECADE = ifelse(FISHING_YEAR < 1990, '1980-1989',
                         ifelse(FISHING_YEAR < 2000 & FISHING_YEAR > 1989, '1990-1999',
                                ifelse(FISHING_YEAR < 2010 & FISHING_YEAR > 1999, '2000-2009',
                                       ifelse(FISHING_YEAR < 2020 & FISHING_YEAR > 2009, '2010-2019', '2020-2021'))))) %>% 
  group_by(DECADE, FG_BLOCKS) %>%
  summarize(NOCATCH = sum(NOCATCH, na.rm = T)) %>%
  rename(Block_ID = FG_BLOCKS)
write.csv(cdfw_gnlog_summary, '~/../Desktop/CDFW tope shark data/cdfw_gnlog_summary_byDecade.csv', row.names = F)

shark_targets <- filter(cdfw_gnlog, grepl("Shark", TARSPC))


# CDFW Marine Fisheries Statistical Unit (MFSU) datasets ------------------

mlds <- read_csv('~/../Desktop/CDFW tope shark data/MLDS Data Extract 159 Soupfin Shark.csv')
mls_trawl <- read_csv('~/../Desktop/CDFW tope shark data/MLS Trawl Extract 159 Soupfin Shark.csv')
cpfv_log <- read_csv('~/../Desktop/CDFW tope shark data/CPFV Log Data 159 Soupfin Shark.csv')

mlds_grouped <- mlds %>%
  mutate(LandingDate = as.Date(LandingDate, format = '%m/%d/%Y'),
         GearGroup = ifelse(GearID %in% 1:9, 'Hook & Line',
                            ifelse(GearID %in% c(11:18, 40), 'Manual',
                                   ifelse(GearID %in% 20:27, 'Trap',
                                          ifelse(GearID %in% c(35, 47:59), 'Trawl',
                                                 ifelse(GearID %in% 60:68, 'Entangling Nets',
                                                        ifelse(GearID %in% 70:74, 'Encircling Nets',
                                                               ifelse(GearID == 80, 'Nets (Generic)', 'Unknown'))))))),
         Region = ifelse(PortID %in% 201:231, 'Northern',
                         ifelse(PortID %in% c(330:602, 606, 607, 609, 615, 638), 'Central', 'Southern')))

mlds_yr_summ <- mlds_grouped %>%
  mutate(LandingYear = as.numeric(format(LandingDate,"%Y"))) %>%
  group_by(LandingYear, Region, GearGroup) %>%
  summarise(Pounds = sum(Pounds, na.rm = T)) %>%
  mutate(Tonnes = Pounds / 2204.62,
         Region = factor(Region, levels = c('Northern', 'Central', 'Southern')))

mlds_plot <- ggplot(data = mlds_yr_summ, aes(x = LandingYear, y = Tonnes, fill = GearGroup))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('California Marine Landings Data System \n(1969-2022)')+
  ylab('Landings (tonnes)')+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_manual(values = RColorBrewer::brewer.pal(name = 'Set2', n = 8))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~Region, ncol = 1)
mlds_plot

cowplot::save_plot('figures/NE_Pacific/MLDS_Landings_byRegion.png', mlds_plot, base_height = 6, base_width = 5)

mls_trawl_grouped <- mls_trawl %>%
  mutate(LandingDate = as.Date(gsub("^(.*?),.*", "\\1", LandingDate), format = '%Y-%m-%d'),
         Region2 = ifelse(SetLatitude >= 38.758, 'Northern',
                         ifelse(SetLatitude < 34.975, 'Southern', 'Central')))
# Includes data for each individual trawl tow (can be grouped by vessel trip)
# However, reported weight is unclear (for some trips, every tow has the same reported weight)
# If we get the full data set, could calculate encounter rate...

cpfv_log_summ <- cpfv_log %>%
  rowwise() %>%
  mutate(LogYear = as.Date(paste0(LogYear, '-01-01'), format = '%Y-%m-%d'),
         Region = ifelse(PortCode %in% 201:231, 'Northern',
                         ifelse(PortCode %in% c(330:602, 606, 607, 609, 615, 638), 'Central', 'Southern')),
         TotalCaught = sum(NumberKept, NumberReleased, NumberLostToSeaLions, NumberOfFishCaughtByCrew, na.rm = T),
         TotalFishers = sum(NumberOfFishers, NumberOfCrewFished, na.rm = T),) %>%
  group_by(LogYear, Region) %>%
  summarise(TotalFishers = sum(TotalFishers, na.rm = T),
            HoursFished = sum(HoursFished, na.rm = T),
            TotalCaught = sum(TotalCaught, na.rm = T)) %>%
  mutate(Region = factor(Region, levels = c('Northern', 'Central', 'Southern')),
         FisherHours = TotalFishers * HoursFished,
         CPUE = TotalCaught / FisherHours * 1000) %>%
  arrange(LogYear, Region)

cpfv_plot <- ggplot(data = cpfv_log_summ, aes(x = LogYear, y = CPUE))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('California Commercial Passenger\nFishing Vessel Log (1980-2022)')+
  ylab('# Caught / 1000 fisher-hours')+
  geom_bar(stat = 'identity')+
  coord_cartesian(ylim = c(0,15))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~Region, ncol = 1)
cpfv_plot

cpfv_fisher_hours_plot <- ggplot(data = cpfv_log_summ, aes(x = LogYear, y = FisherHours/1000))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  ggtitle('California Commercial Passenger Fishing Vessel Log \n(1980-2022)')+
  ylab('Fisher-hours (x1000)')+
  geom_bar(stat = 'identity')+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~Region, ncol = 1)
cpfv_fisher_hours_plot

cowplot::save_plot('figures/NE_Pacific/CPFV_CPUE_byRegion.png', cpfv_plot, base_height = 5, base_width = 4)
cowplot::save_plot('figures/NE_Pacific/CPFV_Fisherhours_byRegion.png', cpfv_fisher_hours_plot, base_height = 6, base_width = 5)


# IPHC Annual Set Line data (1998-2024) -----------------------------------
# https://www.iphc.int/data/fiss-survey-raw-survey-data/
# Surveys conducted during summer months (May-September), centered around June/July

iphc_tope <- read_csv('data/IPHC_non-Pacific halibut data.csv')
iphc_halibut <- read_csv('data/IPHC_set and Pacific halibut data.csv')
iphc_adj <- read_csv('data/IPHC_adjustment.csv')

iphc_tope_allinfo <- left_join(iphc_tope, iphc_halibut, by = c('Year', 'Stlkey', 'Station', 'Setno')) %>%
  mutate(Date_fmt = as.Date(Date, format = '%d-%b-%y'),
         Month = factor(format(Date_fmt, '%B'), month.name, ordered = T),
         MM_YYYY = format(Date_fmt, '%m-%Y'))

iphc_full <- left_join(iphc_halibut, iphc_tope, by = c('Year', 'Stlkey', 'Station', 'Setno')) %>%
  left_join(iphc_adj, by = c('Stlkey')) %>%
#  filter(Eff == 'Y') %>%
  mutate(h.adj = replace_na(h.adj, 1),
         Date_fmt = as.Date(Date, format = '%d-%b-%y'),
         Month = factor(format(Date_fmt, '%B'), month.name, ordered = T),
         MM_YYYY = format(Date_fmt, '%m-%Y'),
         `Hooks Observed` = coalesce(HooksObserved, `Hooks Observed`),
         `Effective Skates` = coalesce(`Effective skates hauled`, `Effective Skates`),
         `Number Observed` = replace_na(`Number Observed`, 0))

US_Canada_polygon <- rworldmap::getMap(resolution = 'low') %>%
  raster::crop(raster::extent(-140,-115,30,62))
usc_shape <- sf::st_as_sf(US_Canada_polygon)

iphc_map <- ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank())+
  ggpointdensity::geom_pointdensity(data = iphc_full, aes(x = `Lon - Grid target`, y = `Lat - Grid target`))+
  scale_color_gradient(low = 'white', high = 'red')+
  geom_sf(data = usc_shape, fill = 'grey70', color = 'black', size = 0.1)+
  scale_x_continuous(limits = c(-140, -120))+
  scale_y_continuous(limits = c(30,60))+
  facet_wrap(~Year, nrow = 3)
iphc_map

iphc_map_byMonth_wholehaul <- ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face= 'italic'),
        axis.title = element_blank(),
        legend.position = 'bottom')+
  ggtitle('IPHC Tope Shark Observations',
          subtitle = 'Whole Haul Sample')+
  geom_sf(data = usc_shape, fill = 'grey70', color = 'black', size = 0.1)+
  geom_point(data = mutate(add_row(filter(iphc_tope_allinfo, SampleType == 'WholeHaul'), Date_fmt = as.Date('01-Sep-2003', format = '%d-%b-%y'), SampleType = 'WholeHaul', Year = 2003), Month = factor(format(Date_fmt, '%B'), month.name, ordered = T)), #for plotting
             aes(x = `Lon - Grid target`, y = `Lat - Grid target`, size = `Number Observed`, fill = Year), shape = 21)+
  viridis::scale_fill_viridis(limits = c(2001, 2025), breaks = c(2005, 2013, 2021))+
  scale_size(range = c(1,5), limits = c(1, 40), breaks = c(1, 20, 40))+
  scale_x_continuous(expand = c(0,0), breaks = c(-130, -125))+
  scale_y_continuous(expand = c(0,0), breaks = c(40, 45, 50, 55))+
  facet_wrap(~Month, nrow = 1)
iphc_map_byMonth_20hooks <- ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face= 'italic'),
        axis.title = element_blank(),
        legend.position = 'bottom')+
  ggtitle('IPHC Tope Shark Observations',
          subtitle = '20-Hooks Sample')+
  geom_sf(data = usc_shape, fill = 'grey70', color = 'black', size = 0.1)+
  geom_point(data = filter(iphc_tope_allinfo, SampleType == '20Hook'), aes(x = `Lon - Grid target`, y = `Lat - Grid target`, size = `Number Observed`, fill = Year), shape = 21)+
  viridis::scale_fill_viridis(limits = c(2001, 2025), breaks = c(2005, 2013, 2021))+
  scale_size(range = c(1,5), limits = c(1, 40), breaks = c(1, 20, 40))+
  scale_x_continuous(expand = c(0,0), breaks = c(-130, -125))+
  scale_y_continuous(expand = c(0,0), breaks = c(40, 45, 50, 55))+
  facet_wrap(~Month, nrow = 1)

iphc_map_byMonth_wholehaul
iphc_map_byMonth_20hooks

cowplot::save_plot('figures/NE_Pacific/IPHC_FISS/IPHC_map_byMonth_wholehaul.png', iphc_map_byMonth_wholehaul, base_height = 5, base_width = 9)
cowplot::save_plot('figures/NE_Pacific/IPHC_FISS/IPHC_map_byMonth_20hooks.png', iphc_map_byMonth_20hooks, base_height = 5, base_width = 9)

#Where/how were the tope caught?
table(iphc_tope_allinfo$`IPHC Reg Area`) #all tope catches are in zones 2A and 2B
table(iphc_tope_allinfo$Gear) #All tope were caught using fixed hooks
table(iphc_tope_allinfo$Bait) #nearly all tope were caught using chum salmon (SA)
catch_summs <- gather(iphc_tope_allinfo[,c(23,28,30,31,33)], factor_key = T) %>%
  group_by(key) %>%
  summarise(mean = mean(value, na.rm = T), sd = sd(value, na.rm = T), max = max(value, na.rm = T), min = min(value, na.rm = T), n = sum(!is.na(value)))
hist(log2(iphc_tope_allinfo$`AvgDepth (fm)`), breaks = 50)

#Filter datasets based on region and gear (removes areas where topes aren't found from effort calculation)
iphc_2a2b <- filter(iphc_full, `IPHC Reg Area` %in% c('2A','2B') &
                      Gear == 'Fixed Hook')

#Add data for rows where Hooks Observed fields is NA
for(i in which(is.na(iphc_2a2b$`Hooks Observed`))) {
  iphc_2a2b$`Hooks Observed`[i] <- iphc_2a2b$`No. skates hauled`[i] * iphc_2a2b$`Avg no. hook/skate`[i]
}

#Add SampleType category to all hauls based on the number of skates hauled compared to the number of hooks observed
for(i in which(is.na(iphc_2a2b$SampleType))) {
  exp.hooks <- iphc_2a2b$`Effective Skates`[i] * 100
  obs.hooks <- iphc_2a2b$`Hooks Observed`[i]
  
  if(is.na(obs.hooks)) next
  
  if(obs.hooks < (exp.hooks + 50) & obs.hooks > (exp.hooks - 50)) {
    iphc_2a2b$SampleType[i] <- 'WholeHaul'
  } else {
    iphc_2a2b$SampleType[i] <- '20Hook'
  }
}

#Depth of tope catch is right-skewed and bounded by zero, so log transformation allows Normal distribution-based summary stats
depth_log <- log2(iphc_tope_allinfo$`AvgDepth (fm)`)
mean(depth_log)
sd(depth_log)
#iphc_2a2b_depth <- filter(iphc_2a2b, `AvgDepth (fm)` < 2^(mean(depth_log) + (2*sd(depth_log)))) 

iphc_2a2b_effort = group_by(iphc_2a2b, Year, MM_YYYY) %>%
  summarise(SoakTime_min = sum(`Soak time (min.)`, na.rm = T)) %>%
  mutate(SoakTime_24hr = SoakTime_min/60/24) %>%
  full_join(summarise(group_by(iphc_tope_allinfo, Year, MM_YYYY), Catch = sum(`Number Observed`))) %>%
  replace(is.na(.), 0) %>%
  mutate(CPUE_nPer24hSoak = Catch/SoakTime_24hr,
         MM_YYYY_fmt = as.Date(paste0('01-', MM_YYYY), format = '%d-%m-%Y')) %>%
  mutate(Month = factor(format(MM_YYYY_fmt, '%B'), month.name, ordered = T))

iphc_2a2b_CPUEbyYear = iphc_2a2b %>%
  filter(`Effective Skates` > 0 & `Hooks Observed` > 0) %>%
  mutate(CPUE_bySkate = `Number Observed` / `Effective Skates`,
         CPUE_bySkate_adj = `Number Observed` / `Effective Skates` * h.adj,
         CPUE_by100Hooks = `Number Observed` / `Hooks Observed` * 100,
         CPUE_by100Hooks_adj = `Number Observed` / `Hooks Observed` * 100 * h.adj) %>%
  group_by(Year, SampleType) %>%
  summarise(n = n(),
            `Effective Skates` = sum(`Effective Skates`),
            `Hooks Observed` = sum(`Hooks Observed`),
            `Number Observed` = sum(`Number Observed`),
            CPUE_bySkate = mean(CPUE_bySkate),
            CPUE_bySkate_adj = mean(CPUE_bySkate_adj),
            CPUE_by100Hooks = mean(CPUE_by100Hooks),
            CPUE_by100Hooks_adj = mean(CPUE_by100Hooks_adj)) %>%
  filter(!is.na(SampleType)) %>%
  ungroup() %>%
  complete(Year, SampleType) %>%
  mutate(n = replace_na(n, 0))

iphc_2a2b_cpue_plot <- ggplot(data = iphc_2a2b_CPUEbyYear, aes(x = Year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('IPHC Tope Shark CPUE (Zones 2A & 2B)')+
  geom_col(aes(y = CPUE_bySkate_adj * 100, fill = SampleType), position = 'dodge')+
  geom_point(aes(y = n/5, colour = SampleType), size = 1.5)+
  geom_line(aes(y = n/5, colour = SampleType), linewidth = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = 'Catch / 100 Skates', expand = expansion(mult = c(0, 0.05)),
                     sec.axis = sec_axis(~ ., name = '# Skates Hauled', breaks = c(0, 100/5, 200/5, 300/5), labels = c(0,100,200,300)))
iphc_2a2b_cpue_plot #Hardly changes when using data filtered by depth

whole_haul <- iphc_2a2b_CPUEbyYear %>%
  pivot_longer(cols = starts_with("CPUE"),
               names_to = "Metric",
               names_prefix = "CPUE_",
               values_to = "CPUE") %>%
  filter(SampleType == 'WholeHaul' & Metric %in% c('by100Hooks', 'by100Hooks_adj')) %>%
  mutate(Metric = factor(Metric),
         `Hooks Observed` = replace_na(`Hooks Observed`, 0))

hooks20 <- iphc_2a2b_CPUEbyYear %>%
  pivot_longer(cols = starts_with("CPUE"),
               names_to = "Metric",
               names_prefix = "CPUE_",
               values_to = "CPUE") %>%
  filter(SampleType == '20Hook' & Metric %in% c('by100Hooks', 'by100Hooks_adj')) %>%
  mutate(Metric = factor(Metric),
         `Hooks Observed` = replace_na(`Hooks Observed`, 0))

whole_haul_plot <- ggplot(data = whole_haul, aes(x = Year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'))+
  ggtitle('IPHC Tope Shark CPUE (Zones 2A & 2B)',
          subtitle = 'Whole Haul Sample')+
  geom_col(aes(y = CPUE * 10, fill = Metric), position = 'dodge')+
  geom_point(aes(y = `Hooks Observed`/30000), colour = 'black', size = 1.5)+
  geom_line(aes(y = `Hooks Observed`/30000), colour = 'black', linewidth = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = 'Catch / 1,000 Hooks', expand = expansion(mult = c(0, 0.05)), limits = c(0, 8), breaks = c(0,2,4,6,8),
                     sec.axis = sec_axis(~ ., name = expression(paste('# Hooks Observed ( ', x10^4, ')', sep="")), breaks = c(0,4,8), labels = c(0,12,24)))+
  scale_fill_manual(values = c('#1F78B4','#A6CEE3'), labels = c('Unmodified','Corrected'))
whole_haul_plot

hooks20_plot <- ggplot(data = hooks20, aes(x = Year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'))+
  ggtitle('IPHC Tope Shark CPUE (Zones 2A & 2B)',
          subtitle = '20-Hook Sample')+
  geom_col(aes(y = CPUE * 10, fill = Metric), position = 'dodge')+
  geom_point(aes(y = `Hooks Observed`/30000), colour = 'black', size = 1.5)+
  geom_line(aes(y = `Hooks Observed`/30000), colour = 'black', linewidth = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = 'Catch / 1,000 Hooks', expand = expansion(mult = c(0, 0.05)), limits = c(0, 8), breaks = c(0,2,4,6,8),
                     sec.axis = sec_axis(~ ., name = expression(paste('# Hooks Observed ( ', x10^4, ')', sep="")), breaks = c(0,4,8), labels = c(0,12,24)))+
  scale_fill_manual(values = c('#E31A1C','#FB9A99'), labels = c('Unmodified','Corrected'))
hooks20_plot

cowplot::save_plot('figures/NE_Pacific/IPHC_FISS/IPHC_CPUE_wholehaul_skates.png', whole_haul_plot, base_height = 3, base_width = 7)
cowplot::save_plot('figures/NE_Pacific/IPHC_FISS/IPHC_CPUE_20hook_skates.png', hooks20_plot, base_height = 3, base_width = 7)

# Generalized linear model to estimate trend, accounting for sampling design and climate cycles
# helpful guide: https://stats.oarc.ucla.edu/r/dae/zinb/
#Import Pacific Decadal Oscillation monthly indices
pdo <- read_csv('data/PDO Index.csv') %>%
  pivot_longer(cols = c(2:13),
               names_to = 'Month',
               values_to = 'pdo_index')
pdo_byYear <- group_by(pdo, Year) %>%
  summarise(pdo_index = mean(pdo_index))

#Add PDO indices to main data set and center the Year by subtracting the mean
iphc_2a2b_pdo <- left_join(iphc_2a2b, pdo, by = c('Year', 'Month')) %>%
  rename(catch = `Number Observed`,
         nhooks = `Hooks Observed`) %>%
  filter(nhooks > 0)
iphc_2a2b_pdo <- iphc_2a2b_pdo %>%
  mutate(yr = Year - mean(unique(iphc_2a2b_pdo$Year)),
         midlat = `MidLat fished` - mean(iphc_2a2b_pdo$`MidLat fished`),
         middep = `AvgDepth (fm)` - mean(iphc_2a2b_pdo$`AvgDepth (fm)`))

#Test various models of increasing complexity
iphc_glm1 <- pscl::zeroinfl(catch ~ yr | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm1.1 <- pscl::zeroinfl(catch ~ yr | SampleType, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm1.2 <- pscl::zeroinfl(catch ~ yr | midlat, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm1.3 <- pscl::zeroinfl(catch ~ yr | middep, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
AIC(iphc_glm1)
AIC(iphc_glm1.1)
AIC(iphc_glm1.2)
AIC(iphc_glm1.3) #

iphc_glm2.1 <- pscl::zeroinfl(catch ~ yr + SampleType | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm2.2 <- pscl::zeroinfl(catch ~ yr + midlat | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm2.3 <- pscl::zeroinfl(catch ~ yr + middep | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm2.4 <- pscl::zeroinfl(catch ~ yr + pdo_index | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
AIC(iphc_glm1)
AIC(iphc_glm2.1)
AIC(iphc_glm2.2)
AIC(iphc_glm2.3) #
AIC(iphc_glm2.4)

iphc_glm3.1 <- pscl::zeroinfl(catch ~ yr + middep + midlat | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm3.2 <- pscl::zeroinfl(catch ~ yr + middep + SampleType | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm3.3 <- pscl::zeroinfl(catch ~ yr + middep + midlat + SampleType | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
AIC(iphc_glm2.3)
AIC(iphc_glm3.1) #
AIC(iphc_glm3.2)
AIC(iphc_glm3.3)

iphc_glm3.1.1 <- pscl::zeroinfl(catch ~ yr + middep:midlat | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm4.1 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:middep | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm4.2 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:midlat | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm4.3 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:middep:midlat | 1, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
AIC(iphc_glm3.1)
AIC(iphc_glm3.1.1)
AIC(iphc_glm4.1)
AIC(iphc_glm4.2) #
AIC(iphc_glm4.3)

iphc_glm4.2.1 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:midlat | middep, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm4.2.2 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:midlat | midlat, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
iphc_glm4.2.3 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:midlat | SampleType, offset = log(nhooks), data = iphc_2a2b_pdo, dist = 'negbin')
AIC(iphc_glm4.2)
AIC(iphc_glm4.2.1)
AIC(iphc_glm4.2.2) #
AIC(iphc_glm4.2.3)

#Add correction factors provided by IPHC as weights (based on estimated catchability)
iphc_glm5 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:midlat | midlat, offset = log(nhooks), weights = h.adj, data = iphc_2a2b_pdo, dist = 'negbin')
summary(iphc_glm5)

#Test whether addition of Soak Time as explanatory variable alters results
iphc_glm6 <- pscl::zeroinfl(catch ~ yr + middep + midlat + yr:midlat + `Soak time (min.)` | midlat, offset = log(nhooks), weights = h.adj, data = iphc_2a2b_pdo, dist = 'negbin')
summary(iphc_glm6) #very little change; main takeaways are the same

#Retrieve marginal effects and associated CIs using reference grid
#~~Marginal effect at the mean
#latitude and depth fixed at their mean values
#reflects the slope with respect to the focal predictor (Year) for an individual whose characteristics (lat and depth) are exactly average
rg <- expand.grid(yr = unique(iphc_2a2b_pdo$yr),
                  nhooks = 1000, #predicted catch per 1,000 hooks
                  middep = 0,
                  midlat = 0)
meff.all <- marginaleffects::predictions(iphc_glm5, by = c('yr'), newdata = rg, type = 'response') %>%
  mutate(Year = yr+2011,
         type = 'all')
#~~Marginal effect at the mean (using only stations where tope sharks were observed)
rg2 <- select(iphc_tope_allinfo, 'Stlkey') %>%
  left_join(iphc_2a2b_pdo) %>%
  mutate(nhooks = 1000,
         middep = mean(middep),
         midlat = mean(midlat)) %>%
  select(c('yr','middep','midlat','nhooks')) %>%
  distinct() %>%
  left_join(x = data.frame(yr = seq(-13,13)), y = .) %>%
  fill(nhooks, middep, midlat, .direction = 'up')
meff.tope <- marginaleffects::predictions(iphc_glm5, by = c('yr'), newdata = rg2, type = 'response') %>%
  mutate(Year = yr+2011,
         type = 'tope')
#~~Average marginal effect
#uses empirical grid to compute a slope for every observation at the observed values in the original sample, and averages them
rg3 <- select(iphc_2a2b_pdo, c('yr','middep','midlat','h.adj')) %>%
  mutate(nhooks = 1000)
emp.pred <- marginaleffects::avg_predictions(iphc_glm5, by = 'yr', newdata = rg3, type = 'response') %>%
  mutate(Year = yr + 2011)

#Add to original dataset
meff <- rbind(meff.all, meff.tope)
iphc.pred <- iphc_2a2b_pdo %>%
  mutate(CPUE_byHooks = catch / nhooks * 1000,
         CPUE_byHooks_adj = catch / nhooks * 1000 * h.adj) %>%
  group_by(Year) %>%
  summarise(n = n(),
            nhooks = sum(nhooks),
            catch = sum(catch),
            CPUE_byHooks = mean(CPUE_byHooks),
            CPUE_byHooks_adj = mean(CPUE_byHooks_adj)) %>%
  full_join(meff, by = c('Year'))

#Extract CPUE grouped by unmodified vs. corrected values for plotting purposes
bars.extract <- iphc.pred %>%
  select(Year, CPUE_byHooks, CPUE_byHooks_adj) %>%
  pivot_longer(cols = starts_with("CPUE_byHooks"),
               names_to = "adjust",
               names_prefix = "CPUE_",
               values_to = "CPUE")

#Plot adjusted mean prediction (i.e., expected marginal mean using averages of continuous covariates) overlaying observation data 
pred_plot <- ggplot(data = iphc.pred, aes(x = Year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'))+
  ggtitle('IPHC Tope Shark CPUE (Zones 2A & 2B)',
          subtitle = 'GLM Model Prediction - Marginal Effect at the Mean')+
  geom_col(data = bars.extract, aes(y = CPUE, fill = adjust), position = 'dodge', alpha = 0.3)+
  geom_ribbon(data = meff.all, aes(ymin = conf.low, ymax = conf.high), fill = 'grey70', alpha = 0.4)+
  geom_ribbon(data = meff.tope, aes(ymin = conf.low, ymax = conf.high), fill = 'grey70', alpha = 0.4)+
  geom_line(data = iphc.pred, aes(y = estimate, linetype = type), linewidth = 0.75)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = 'Catch / 1,000 Hooks', limits = c(0, 2.1), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c('#1F78B4','#A6CEE3'), labels = c('Unmodified','Corrected'))+
  scale_linetype(labels = c('All', 'Tope'))
pred_plot

cowplot::save_plot('figures/NE_Pacific/IPHC_FISS/IPHC_CPUE_margEffAtMean.png', pred_plot, base_height = 3, base_width = 5)

#Plot slope x latitude
nd <- expand.grid(yr = unique(iphc_2a2b_pdo$yr),
                  nhooks = 1000,
                  middep = mean(iphc_2a2b_pdo$middep),
                  midlat = seq(40, 52.5, by = 0.05) - 49.77224)
slope_lat <- marginaleffects::slopes(iphc_glm5, variable = c('yr'), by = c('midlat'), newdata = nd) %>%
  mutate(sig = ifelse(midlat >= -6.3 & midlat <= -3.85, 'negative', ifelse(midlat >= -1.9, 'positive', NA))) %>%
  mutate(overlay = ifelse(!is.na(sig), estimate, NA))

marginaleffects::plot_slopes(iphc_glm5, variable = c('yr'), by = c('midlat'), newdata = nd, type = 'response')

slope_plot <- ggplot(data = slope_lat, aes(x = midlat, group = 1))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.subtitle = element_text(colour = 'grey50', face = 'italic'))+
  ggtitle('IPHC Tope Shark CPUE (Zones 2A & 2B)',
          subtitle = 'Variation in predicted trend by latitude')+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = 'grey70', alpha = 0.2)+
  geom_line(aes(y = estimate))+
  geom_line(aes(y = overlay, col = sig), size = 1.5)+
  scale_x_continuous(name = 'Latitude (°N)', limits = c(40, 52.5) - 49.77224, breaks = seq(40, 50, by=2.5) - 49.77224, labels = seq(40, 50, by=2.5), expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(name = 'Slope', limits = c(-0.1, 0.1))+
  scale_color_manual(values = c('#E31A1C','forestgreen'))
slope_plot

cowplot::save_plot('figures/NE_Pacific/IPHC_FISS/IPHC_CPUE_slope-lat.png', slope_plot, base_height = 3, base_width = 5)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Does converting latitude and/or depth to categorical variable(s) highlight interesting breaks in the data?
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Find latitude where N-S grouping most improves model fit by iterating through the latitudinal range in increments of 0.2 deg
latbreaks <- seq(40, 55, by = 0.2)
aic <- numeric(length(latbreaks))
modelInput <- iphc_2a2b_pdo
for(i in 1:length(latbreaks)){
  modelInput$latbreak <- ifelse(modelInput$midlat < latbreaks[i], 'below', 'above')
  iphc_glm_ns <- pscl::zeroinfl(catch ~ yr + middep + latbreak + yr:latbreak | midlat, offset = log(nhooks), weights = h.adj,
                                data = modelInput, dist = 'negbin')
  aic[i] <- extractAIC(iphc_glm_ns)[2]
}
aic.df <- data.frame(Latitude = seq(40, 55, by = 0.2), AIC = as.numeric(aic)) %>%
  mutate(deltaAIC = AIC - min(AIC, na.rm = T)) %>%
  mutate(exps = exp(-deltaAIC/2)) %>%
  mutate(wts = exps/sum(exps, na.rm = T)) %>%
  mutate(wts = ifelse(wts < 0.0001, 0, wts))
#grouping north and south of 49.2 N provides the best fit

#All observations south of 49.2 N categorized as 'S', and those greater than or equal to 49.2 N categorized as 'N'
iphc_2a2b_pdo$ns_break <- ifelse(iphc_2a2b_pdo$midlat < 49.2, 'S', 'N')

iphc_glm5.1 <- pscl::zeroinfl(catch ~ yr + middep + ns_break + yr:ns_break | midlat, offset = log(nhooks), weights = h.adj,
                              data = iphc_2a2b_pdo, dist = 'negbin')
AIC(iphc_glm5)
AIC(iphc_glm5.1) #converting latitude to categorical variable (vs. continuous) does not improve model fit

#Find depth where shallow-deep grouping most improves model fit by iterating through the depth range in increments of 50 fm
depbreaks <- seq(50, 350, by = 50)
aic <- numeric(length(depbreaks))
modelInput <- iphc_2a2b_pdo
for(i in 1:length(depbreaks)){
  modelInput$depbreak <- ifelse(modelInput$middep < depbreaks[i], 'below', 'above')
  iphc_glm_ns <- pscl::zeroinfl(catch ~ yr + depbreak + midlat + yr:midlat | midlat, offset = log(nhooks), weights = h.adj,
                                data = modelInput, dist = 'negbin')
  aic[i] <- extractAIC(iphc_glm_ns)[2]
}
aic.df <- data.frame(Depth = seq(50, 350, by = 50), AIC = as.numeric(aic)) %>%
  mutate(deltaAIC = AIC - min(AIC, na.rm = T)) %>%
  mutate(exps = exp(-deltaAIC/2)) %>%
  mutate(wts = exps/sum(exps, na.rm = T)) %>%
  mutate(wts = ifelse(wts < 0.0001, 0, wts))
#grouping around 100 fm provides the best fit

#All observations shallower than 100 fm categorized as 'S', and those deeper than or equal to 100 fm categorized as 'D'
iphc_2a2b_pdo$depbreak <- ifelse(iphc_2a2b_pdo$middep < 100, 'S', 'D')

iphc_glm5.2 <- pscl::zeroinfl(catch ~ yr + depbreak + midlat + yr:midlat | midlat, offset = log(nhooks), weights = h.adj,
                              data = iphc_2a2b_pdo, dist = 'negbin')
AIC(iphc_glm5)
AIC(iphc_glm5.2) #converting depth to categorical variable does not improve model fit

# #Normalizing `Effective Skates` data by min-max scaling (should `Number Observed` also be scaled in the same way?)
# min_skates <- min(iphc_2a2b_pdo$`Effective Skates`); max_skates <- max(iphc_2a2b_pdo$`Effective Skates`)
# iphc_2a2b_pdo$skates_nrm <- ((iphc_2a2b_pdo$`Effective Skates` - min_skates) / (max_skates - min_skates))
# 
# iphc_glm6 <- pscl::zeroinfl(`Number Observed` ~ yr + ns_break + yr:ns_break + SampleType | SampleType, offset = skates_nrm,
#                             data = iphc_2a2b_pdo, dist = 'negbin')
# AIC(iphc_glm6)

### Visualize model using categorical 'ns_break' factor, despite worse fit to data
#Retrieve marginal effects and associated CIs using reference grid
rg2 <- expand.grid(yr = unique(iphc_2a2b_pdo$yr),
                   ns_break = unique(iphc_2a2b_pdo$ns_break),
                   midlat = mean(iphc_2a2b_pdo$midlat),
                   middep = mean(iphc_2a2b_pdo$middep),
                   nhooks = mean(iphc_2a2b_pdo$nhooks))
meff2 <- marginaleffects::predictions(iphc_glm5.1, by = c('yr', 'ns_break'), newdata = rg2, type = 'response') %>%
  mutate(Year = yr+2011,
         CPUE.pred = estimate/nhooks*1000,
         CPUE.pred.low = conf.low/nhooks*1000,
         CPUE.pred.high = conf.high/nhooks*1000)

#Add to original dataset
iphc.pred2 <- iphc_2a2b_pdo %>%
  mutate(CPUE_byHooks = catch / nhooks * 1000,
         CPUE_byHooks_adj = catch / nhooks * 1000 * h.adj) %>%
  group_by(Year, ns_break) %>%
  summarise(n = n(),
            nhooks = sum(nhooks),
            catch = sum(catch),
            CPUE_byHooks = mean(CPUE_byHooks),
            CPUE_byHooks_adj = mean(CPUE_byHooks_adj)) %>%
  ungroup() %>%
  complete(Year, ns_break) %>%
  mutate(CPUE_byHooks = replace_na(CPUE_byHooks, 0),
         CPUE_byHooks_adj = replace_na(CPUE_byHooks_adj, 0)) %>%
  full_join(meff2, by = c('Year', 'ns_break'))

pred_plot2 <- ggplot(data = iphc.pred2, aes(x = Year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'bottom')+
  ggtitle('IPHC Tope Shark CPUE (Zones 2A & 2B)')+
  geom_col(aes(y = CPUE_byHooks, fill = ns_break), position = 'dodge')+
  geom_ribbon(aes(ymin = CPUE.pred.low, ymax = CPUE.pred.high, fill = ns_break), alpha = 0.4)+
  geom_line(aes(y = CPUE.pred, colour = ns_break), size = 1)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = 'Catch / 1,000 Hooks', expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c('#A6CEE3','#FB9A99'), labels = c('North of 49.2 N','South of 49.2 N'))+
  scale_colour_manual(values = c('#1F78B4','#E31A1C'), labels = c('North of 49.2 N','South of 49.2 N'))
pred_plot2

# #Bootstrap to obtain CIs for prediction
# resp <- predict(iphc_glm6.2)
# stat <- function(df, inds) {
#   model <- formula(`Number Observed` ~ yr + ns_break + yr:ns_break | `MidLat fished`)
#   predict(
#     pscl::zeroinfl(model, offset = log(`Hooks Observed`), weights = h.adj, dist = 'negbin', data = df[inds, ]), newdata = df
#     )
# }
# set.seed(101)
# res <- boot::boot(iphc_2a2b_pdo, stat, R = 500, parallel = "multicore", ncpus = 12)
# CI <- setNames(as.data.frame(t(sapply(c(1:nrow(iphc_2a2b_pdo)), function(row)
#   boot::boot.ci(res, conf = 0.95, type = "basic", index = row)$basic[,4:5]))),
#   c("lower", "upper"))



# BMF 1949 Figure 63 - reproduction ---------------------------------------

bmf1949_f63 <- read_csv('data/Ripley_1949_BMF_FishBulletin74.csv')

bmf1949_f63_plot <- ggplot(data = bmf1949_f63, aes(x = Year))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())+
  ggtitle('Total Shark Landings - California')+
  geom_segment(aes(x = 1915.55, xend = 1937.45, y = 4600, yend = 4600), linewidth = 1.5, color = '#56B4E9')+
  geom_segment(aes(x = 1937.55, xend = 1939.45, y = 4600, yend = 4600), linewidth = 1.5, color = '#E69F00')+
  geom_segment(aes(x = 1939.55, xend = 1944.45, y = 4600, yend = 4600), linewidth = 1.5, color = '#009E73')+
  geom_point(aes(y = Landings_lbs/2204.62), size = 1.5)+
  geom_line(aes(y = Landings_lbs/2204.62), linewidth = 0.5)+
  geom_point(aes(y = Correction/2204.62), size = 1.5)+
  geom_line(aes(y = Correction/2204.62), linewidth = 0.5, linetype = 'dashed')+
  scale_x_continuous(breaks = c(1915, 1920, 1925, 1930, 1935, 1940, 1945))+
  scale_y_continuous(name = 'Landings (mt)', expand = expansion(mult = c(0, 0.05)), breaks = seq(0,4000, by = 1000), limits = c(0,5000))
bmf1949_f63_plot

cowplot::save_plot('figures/NE_Pacific/CA_sharkLandings_1916-1947.png', bmf1949_f63_plot, base_height = 3, base_width = 5)









