library(tidyverse)

range_current <- read_csv('data/Habitat_Suitability/nativeRange_current.csv')
range_2100 <- read_csv('data/Habitat_Suitability/nativeRange_2100.csv')

range_joined <- full_join(range_current, range_2100, by = 'CsquareCode') %>%
  rename(Prob_current = Probability.x, Prob_2100 = Probability.y) %>%
  mutate(CenterLong = ifelse(is.na(CenterLong.x), CenterLong.y, CenterLong.x),
         CenterLat = ifelse(is.na(CenterLat.x), CenterLat.y, CenterLat.x),
         Prob_current_noNA = Prob_current,
         Prob_2100_noNA = Prob_2100) %>%
  replace_na(list(Prob_current_noNA = 0, Prob_2100_noNA = 0)) %>%
  mutate(Prob_diff = Prob_2100_noNA - Prob_current_noNA) %>%
  select(CenterLong, CenterLat, Prob_current, Prob_2100, Prob_current_noNA, Prob_2100_noNA, Prob_diff)

# How many more/less pixels with probability of occurrence >50% in 2100 compared to present?
sum(range_joined$Prob_2100_noNA >= 0.5) - sum(range_joined$Prob_current_noNA >= 0.5)
## 1028 or ~9.2% more area with "suitable habitat" 
## [This was calculated incorrectly, should be divided by "current" number, which would equal a ~22% increase]

write.csv(range_joined, 'data/Habitat_Suitability/nativeRange_difference.csv', row.names = F)

# Add dataset with DPSs
range_joined_DPS <- read_csv('data/Habitat_Suitability/nativeRange_difference_withDPSs.csv')
DPS_gain_loss <- group_by(range_joined_DPS, DPS) %>%
  summarise(gain = sum(Prob_2100 >= 0.5, na.rm = T) - sum(Prob_current >= 0.5, na.rm = T),
            pct_gain = (sum(Prob_2100 >= 0.5, na.rm = T) - sum(Prob_current >= 0.5, na.rm = T)) / n())


# UPDATED: SSP2-RCP4.5 ----------------------------------------------------

range_all <- read_csv('data/Habitat_Suitability/AquaX_Tope_Shark.csv') %>%
  select(x, y, Current, RCP45_2050, RCP45_2100) %>%
  mutate(PresAbs = ifelse(Current >= 685, 'P',
                          ifelse(RCP45_2100 >= 685, 'P', 'A'))) %>%
  mutate(across(Current:RCP45_2100, ~ .x / 1000)) %>%
  mutate(Diff_2050 = RCP45_2050 - Current,
         Diff_2100 = RCP45_2100 - Current)

# How many more/less pixels with HSI > 0.685 in 2100 compared to current day?
(sum(range_all$RCP45_2100 >= 0.685) - sum(range_all$Current >= 0.685)) / sum(range_all$Current >= 0.685)
## The amount of suitable area (in terms of pixel #) is projected to increase by 22.6% (compared to current day)

# Calculate area of each grid cell based on latitude to determine change in area
# Define grid size in degrees
grid_size_deg <- 0.05
# Function to convert degrees to radians for R's cos() function
deg2rad <- function(deg) {
  return(deg * pi / 180)
}
# Calculate cell height (constant) and width (varies with latitude) in km
cell_height_km <- grid_size_deg * 111.132
cell_width_km <- grid_size_deg * 111.320 * cos(deg2rad(range_all$y))
# Calculate the area for each location in square kilometers
range_all$area_sq_km <- cell_height_km * cell_width_km
rm(cell_width_km)

# Calculate changes in area occupied/unoccupied
p_current <- c(cells = sum(range_all$Current >= 0.685),
               area = sum(range_all$area_sq_km[range_all$Current >= 0.685])) 
#current occupied area: 207,943 cells; 4,748,652 km2
p_2100 <- c(cells = sum(range_all$RCP45_2100 >= 0.685),
            area = sum(range_all$area_sq_km[range_all$RCP45_2100 >= 0.685])) 
#future occupied area: 255,061 cells; 5,375,460 km2
ap <- c(cells = sum(range_all$Current < 0.685 & range_all$RCP45_2100 >= 0.685),
        area = sum(range_all$area_sq_km[range_all$Current < 0.685 & range_all$RCP45_2100 >= 0.685])) 
#future occupied area where currently absent: 67,615 cells; 1,165,287 km2
pa <- c(cells = sum(range_all$Current >= 0.685 & range_all$RCP45_2100 < 0.685),
        area = sum(range_all$area_sq_km[range_all$Current >= 0.685 & range_all$RCP45_2100 < 0.685])) 
#future unoccupied area where currently occupied: 20,497 cells; 538,479 km2
pp <- c(cells = sum(range_all$Current >= 0.685 & range_all$RCP45_2100 >= 0.685),
        area = sum(range_all$area_sq_km[range_all$Current >= 0.685 & range_all$RCP45_2100 >= 0.685])) 
#future occupied area where currently occupied: 187,446 cells; 4,210,174 km2

total_area_change <- (p_2100[2] - p_current[2]) / p_current[2] #13.2% increase in suitable area overall
current_area_lost <- pa[2] / p_current[2] #11.3% current occupied area predicted to become unsuitable
future_area_gained <- ap[2] / p_2100[2] #21.7% of predicted future area is currently unoccupied

# Filter to only cells where species is predicted to occur presently or in the future
# Export to csv for ArcGIS import
present <- filter(range_all, PresAbs == 'P') %>%
  select(-PresAbs)
write.csv(present, 'data/Habitat_Suitability/HSI_presence.csv', row.names = F)

# Calculate total area change for all DPSs
nepac <- read_csv('data/Habitat_Suitability/DPSs/NE_Pacific.csv') %>%
  select(-Diff_2100) %>%
  left_join(range_all)
nepac_total_area_change <- (sum(nepac$area_sq_km[nepac$RCP45_2100 >= 0.685]) - sum(nepac$area_sq_km[nepac$Current >= 0.685])) / sum(nepac$area_sq_km[nepac$Current >= 0.685])
nepac_area <- c(current = sum(nepac$area_sq_km[nepac$Current >= 0.685]),
                yr2100 = sum(nepac$area_sq_km[nepac$RCP45_2100 >= 0.685]),
                difference = sum(nepac$area_sq_km[nepac$RCP45_2100 >= 0.685]) - sum(nepac$area_sq_km[nepac$Current >= 0.685]))
#NE Pacific: 28.6% increase

neatl <- read_csv('data/Habitat_Suitability/DPSs/NE_Atlantic.csv') %>%
  select(-Diff_2100) %>%
  left_join(range_all)
neatl_total_area_change <- (sum(neatl$area_sq_km[neatl$RCP45_2100 >= 0.685]) - sum(neatl$area_sq_km[neatl$Current >= 0.685])) / sum(neatl$area_sq_km[neatl$Current >= 0.685])
neatl_area <- c(current = sum(neatl$area_sq_km[neatl$Current >= 0.685]),
                yr2100 = sum(neatl$area_sq_km[neatl$RCP45_2100 >= 0.685]),
                difference = sum(neatl$area_sq_km[neatl$RCP45_2100 >= 0.685]) - sum(neatl$area_sq_km[neatl$Current >= 0.685]))
#NE Atlantic: 13.6% increase

safr <- read_csv('data/Habitat_Suitability/DPSs/SoAfrica.csv') %>%
  select(-Diff_2100) %>%
  left_join(range_all)
safr_total_area_change <- (sum(safr$area_sq_km[safr$RCP45_2100 >= 0.685]) - sum(safr$area_sq_km[safr$Current >= 0.685])) / sum(safr$area_sq_km[safr$Current >= 0.685])
safr_area <- c(current = sum(safr$area_sq_km[safr$Current >= 0.685]),
                yr2100 = sum(safr$area_sq_km[safr$RCP45_2100 >= 0.685]),
                difference = sum(safr$area_sq_km[safr$RCP45_2100 >= 0.685]) - sum(safr$area_sq_km[safr$Current >= 0.685]))
#So Africa: 6.4% decrease

swpac <- read_csv('data/Habitat_Suitability/DPSs/SW_Pacific.csv') %>%
  select(-Diff_2100) %>%
  left_join(range_all)
swpac_total_area_change <- (sum(swpac$area_sq_km[swpac$RCP45_2100 >= 0.685]) - sum(swpac$area_sq_km[swpac$Current >= 0.685])) / sum(swpac$area_sq_km[swpac$Current >= 0.685])
swpac_area <- c(current = sum(swpac$area_sq_km[swpac$Current >= 0.685]),
                yr2100 = sum(swpac$area_sq_km[swpac$RCP45_2100 >= 0.685]),
                difference = sum(swpac$area_sq_km[swpac$RCP45_2100 >= 0.685]) - sum(swpac$area_sq_km[swpac$Current >= 0.685]))
#SW Pacific: 13.4% increase

sepac <- read_csv('data/Habitat_Suitability/DPSs/SE_Pacific.csv') %>%
  select(-Diff_2100) %>%
  left_join(range_all)
sepac_total_area_change <- (sum(sepac$area_sq_km[sepac$RCP45_2100 >= 0.685]) - sum(sepac$area_sq_km[sepac$Current >= 0.685])) / sum(sepac$area_sq_km[sepac$Current >= 0.685])
sepac_area <- c(current = sum(sepac$area_sq_km[sepac$Current >= 0.685]),
                yr2100 = sum(sepac$area_sq_km[sepac$RCP45_2100 >= 0.685]),
                difference = sum(sepac$area_sq_km[sepac$RCP45_2100 >= 0.685]) - sum(sepac$area_sq_km[sepac$Current >= 0.685]))
#SE Pacific: 6.6% increase

swatl <- read_csv('data/Habitat_Suitability/DPSs/SW_Atlantic.csv') %>%
  select(-Diff_2100) %>%
  left_join(range_all)
swatl_total_area_change <- (sum(swatl$area_sq_km[swatl$RCP45_2100 >= 0.685]) - sum(swatl$area_sq_km[swatl$Current >= 0.685])) / sum(swatl$area_sq_km[swatl$Current >= 0.685])
swatl_area <- c(current = sum(swatl$area_sq_km[swatl$Current >= 0.685]),
                yr2100 = sum(swatl$area_sq_km[swatl$RCP45_2100 >= 0.685]),
                difference = sum(swatl$area_sq_km[swatl$RCP45_2100 >= 0.685]) - sum(swatl$area_sq_km[swatl$Current >= 0.685]))
#SW Atlantic: 12.1% increase

#Categorical variable to indicate stable areas vs. areas of loss/gain
present$change <- ifelse(present$Current >= 0.685 & present$RCP45_2100 >= 0.685, 'No change',
                         ifelse(present$Current >= 0.685 & present$RCP45_2100 < 0.685, 'Loss', 'Gain'))
change_df <- select(present, x, y, change)
write.csv(change_df, 'data/Habitat_Suitability/HSI_change.csv', row.names = F)

