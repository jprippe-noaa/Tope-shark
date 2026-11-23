library(tidyverse)

# FAO Global Captures Data (1950-2021) ------------------------------------
# https://github.com/openfigis/RefData

fao_captures <- read_csv('data/FAO Capture Quantity_Global_summarized.csv') %>%
  pivot_longer(cols = -c("Country Name En", "Unit Name"),
               names_to = c("Year",".value"),
               names_pattern = "(.*)_(.*)") %>%
  rename(all_of(c(Country = "Country Name En", Weight_tonnes = "Weight"))) %>%
  select(-c("Unit Name"))

fao_captures_byArea <- read_csv('data/FAO Capture Quantity_Global.csv')


# NE Atlantic -------------------------------------------------------------

# All FAO catch data for FAO Areas 24 (NE Atl), 34 (NE Africa), 37 (Mediterranean)
neatl <- filter(fao_captures_byArea, AREA.CODE %in% c('27','34','37'))
neatl_plot <- ggplot(neatl, aes(x=PERIOD, y=VALUE, fill=COUNTRY_NAME))+
  theme_bw()+
  geom_col(position = 'stack')+
  scale_x_continuous(limits = c(1949, NA), breaks = seq(1950, 2020, by = 10), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(12, "Set3"),"#000000"))+
  facet_wrap(.~AREA.CODE, ncol = 1, scales = 'free_y')
neatl_plot

##~~ FAO Area 27 (NE Atlantic) ~~##

# Load ICES historical nominal catch 1950-2010 dataset
# NOTE: a value of 0.5 indicates that reported catch was LESS THAN 0.5 tonnes
ices_historical <- read_csv("data/ICES_Ggaleus_1950-2010_zeros_0_5replaced.csv") %>%
  pivot_longer(cols = -c("Country", "Species", "Division"),
               names_to = "Year",
               values_to = "Weight_tonnes")

# Load ICES official nominal catch 2006-2020 dataset
# NOTE: data are nested (subareas within areas are on separate rows), so the 'filter' steps below retain only the largest area classification
ices_recent <- read_csv('data/ICES_2006-2020.csv') %>%
  pivot_longer(cols = -c(1:4),
               names_to = "Year",
               values_to = "Weight_tonnes") %>%
  filter(!grepl("\\.", Area)) %>%
  filter(!grepl("_", Area))

# Combine historical and recent ICES datasets; ensure consistency between Country labels
ices_historical_byCountry <- ices_historical %>%
  mutate(Country = replace(Country, Country == 'UK - Eng+Wales+N.Irl.', 'United Kingdom')) %>%
  mutate(Country = replace(Country, Country == 'UK - Scotland', 'United Kingdom')) %>%
  group_by(Year, Country) %>%
  summarise(catch = sum(Weight_tonnes, na.rm = TRUE)) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(dataset = 'historical')
ices_recent_byCountry <- ices_recent %>%
  group_by(Year, Country) %>%
  summarise(catch = sum(Weight_tonnes, na.rm = TRUE)) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(dataset = 'recent')
ices_combined <- rbind(filter(ices_historical_byCountry, Year < 2006), ices_recent_byCountry)

# Load "reassessed" ICES data from WGEF 2022 report (Table 10.1, 10.3)
wgef <- read_csv('data/ICES_WGEF 2023 report_Table10.1_10.3.csv') %>%
  select(Year, Country, Catch_tonnes)

# Filter FAO Area 27 data from FAO global catch dataset
fao27 <- fao_captures_byArea %>%
  filter(AREA.CODE == 27) %>%
  select(PERIOD, COUNTRY_NAME, VALUE)

# Compare ICES datasets, WGEF 2022 report (Table 10.1, 10.3), and FAO Subarea 27 dataset
ices_toCompare <- ices_combined %>% 
  select(-dataset) %>%
  rename(catch_ices = catch) %>%
  ungroup()
wgef_toCompare <- wgef %>% rename(catch_wgef = Catch_tonnes)
fao27_toCompare <- fao27 %>% rename(Year = PERIOD, Country = COUNTRY_NAME, catch_fao = VALUE)
compare_ices_wgef_fao <- full_join(full_join(ices_toCompare, wgef_toCompare), fao27_toCompare)
#WGEF data appears to be the most thorough and includes "reassessed" data from 2005-2015

# Plot WGEF data
wgef_plot <-  ggplot(wgef, aes(x = Year, y = Catch_tonnes, fill = Country))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y = 'Estimated catch (tonnes)')+
  geom_rect(aes(xmin = -Inf, xmax = 2004.5, ymin = 0, ymax = Inf), fill = 'grey90', alpha = 0.03)+
  geom_col(position = 'stack', width = 0.8)+
  geom_vline(xintercept = 2004.5, linetype = 'dashed', linewidth = 1)+
  scale_x_continuous(limits = c(1974, NA), breaks = seq(1980, 2020, by = 10), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(limits = c(0, 2500), expand = expansion(mult = c(0, 0.05)))
#  scale_fill_manual(values = c("#004753","#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462"))
wgef_plot

# Remove countries with total catch <10 tonnes
wgef_totals <- wgef %>%
  group_by(Country) %>%
  summarize(total = sum(Catch_tonnes, na.rm = TRUE))
wgef_major_countries <- wgef_totals$Country[which(wgef_totals$total>=10)]
wgef_major_data <- filter(wgef, Country %in% wgef_major_countries)

# Re-plot WGEF data with minor countries removed
wgef_major_plot <-  ggplot(wgef_major_data, aes(x = Year, y = Catch_tonnes, fill = Country))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y = 'Estimated catch (tonnes)')+
#  geom_rect(aes(xmin = -Inf, xmax = 2004.5, ymin = 0, ymax = Inf), fill = 'grey90', alpha = 0.01)+
  geom_col(position = 'stack', width = 0.8)+
  geom_vline(xintercept = 2004.5, linetype = 'dashed', linewidth = 1)+
  scale_x_continuous(limits = c(1974, NA), breaks = seq(1980, 2020, by = 10), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(limits = c(0, 2500), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("#004753","#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462"))
wgef_major_plot

cowplot::save_plot('figures/WGEF2023_byCountry.png', wgef_major_plot, base_height = 3, base_width = 8)

# Plot ICES data
ices_combined_plot <- ggplot(ices_combined, aes(x=Year, y=catch, fill=Country))+
  theme_bw()+
  geom_col(position = 'stack')+
  scale_x_continuous(limits = c(1969, NA), breaks = seq(1970, 2020, by = 10), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(limits = c(0, 2500), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(12, "Set3"),"#000000"))
ices_combined_plot

# Remove countries with <10 tonnes of total reported catch
ices_totals <- ices_combined %>%
  group_by(Country) %>%
  summarize(total = sum(catch))
ices_major_countries <- ices_totals$Country[which(ices_totals$total>=10)]
ices_combined_major <- filter(ices_combined, Country %in% ices_major_countries)

# Re-plot ICES data with minor countries removed
ices_combined_major_plot <- ggplot(ices_combined_major, aes(x=Year, y=catch, fill=Country))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y = 'Estimated catch (tonnes)')+
  geom_rect(aes(xmin = -Inf, xmax = 2004.5, ymin = 0, ymax = Inf), fill = 'grey90', alpha = 0.03)+
  geom_col(position = 'stack', width = 0.8)+
  geom_vline(xintercept = 2004.5, linetype = 'dashed', linewidth = 1)+
  scale_x_continuous(limits = c(1977, NA), breaks = seq(1980, 2020, by = 10), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(limits = c(0, 2500), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("#004753","#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462"))
ices_combined_major_plot

cowplot::save_plot('figures/ICES_byCountry.pdf', ices_combined_major_plot, base_height = 3, base_width = 8)


##~~ FAO Area 34 (E Central Atlantic / NE Africa) ~~##

# Load FAO Area 34 data from CECAF
# https://www.fao.org/fishery/en/collection/cecaf_capture
# STATUS field refers to zero values: "M" - Data not available, unobtainable; "N" - More than zero but less than half the unit used; "" - Zero reported
fao34 <- read_csv('data/FAO Capture Quantity_CECAF_Area34.csv') %>%
  select(PERIOD, COUNTRY_NAME, DIVISION.CODE, VALUE, STATUS) %>% 
  rename(Year = PERIOD, Country = COUNTRY_NAME, Subarea = DIVISION.CODE, Catch_tonnes = VALUE)

# Change subarea labels for plotting
fao34$Subarea[fao34$Subarea == '34.1.1'] <- '34.1.1 - Morocco Coastal'
fao34$Subarea[fao34$Subarea == '34.1.2'] <- '34.1.2 - Canaries/Madeira'
fao34$Subarea[fao34$Subarea == '34.1.3'] <- '34.1.3 - Sahara Coastal'
fao34$Subarea[fao34$Subarea == '34.3.1'] <- '34.3.1 - Cape Verde Coastal'
fao34$Subarea[fao34$Subarea == '34.3.2'] <- '34.3.2 - Cape Verde Insular'
fao34$Subarea[fao34$Subarea == '34.4.2'] <- '34.4.2 - W Gulf of Guinea'

# Remove Greece (recorded no catch)
fao34_noGreece <- fao34 %>%
  filter(Country != 'Greece')

# Plot
fao34_plot <- ggplot(fao34_noGreece, aes(x=Year, y=Catch_tonnes, fill=Country))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y = 'Estimated catch (tonnes)')+
  geom_col(position = 'stack', width = 0.8)+
  scale_x_continuous(limits = c(1995, NA), breaks = seq(2000, 2020, by = 10), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(limits = c(0, 160), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("darkolivegreen3", "#FB8072", "#80B1D3")) +
  facet_wrap(.~Subarea)
fao34_plot

cowplot::save_plot('figures/CECAF_byCountry.pdf', fao34_plot, base_height = 5, base_width = 8.5)

# Investigate Portuguese fishing patterns in subarea 34.1.2
un_codes <- read_csv('data/UN_countryCodes.csv') %>% 
  select(COUNTRY.UN_CODE, Country)
spp_codes <- read_csv('data/FAO_species_codes.csv') %>%
  select(SPECIES.ALPHA_3_CODE, Scientific_Name, English_name)
cecaf_full <- read_csv('data/FAO Capture Quantity_CECAF_Area34_FULL.csv') %>%
  full_join(un_codes) %>%
  full_join(spp_codes) %>%
  select(Country, PERIOD, Scientific_Name, English_name, DIVISION.CODE, VALUE, STATUS)
cecaf_34.1.2 <- filter(cecaf_full, Country == 'Portugal' & DIVISION.CODE == '34.1.2')
cecaf_34.1.2_summ <- group_by(cecaf_34.1.2, Scientific_Name, English_name) %>%
  summarise(total = sum(VALUE)) %>%
  arrange(desc(total))


##~~ FAO Area 37 (Mediterranean) ~~##

# Load FAO Area 37 data from GFCM
# https://www.fao.org/fishery/en/collection/gfcm_capture
# STATUS field refers to zero values: "M" - Data not available, unobtainable; "N" - More than zero but less than half the unit used; "" - Zero reported
fao37 <- read_csv('data/FAO Capture Quantity_GCFM_Area37.csv') %>%
  select(PERIOD, COUNTRY_NAME, DIVISION.CODE, VALUE, STATUS, FAO_RECORD) %>% 
  rename(Year = PERIOD, Country = COUNTRY_NAME, Subarea = DIVISION.CODE, Catch_tonnes = VALUE, Zero_code = STATUS)

# Change subarea labels for plotting
fao37$Subarea[fao37$Subarea == '37.1.1'] <- '37.1.1 - Balearic'
fao37$Subarea[fao37$Subarea != '37.1.1 - Balearic'] <- 'Other Subareas'

# Plot
fao37_plot <- ggplot(fao37, aes(x=Year, y=Catch_tonnes, fill=Country))+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y = 'Estimated catch (tonnes)')+
  geom_col(position = 'stack', width = 0.8)+
  scale_x_continuous(limits = c(1995, NA), breaks = seq(2000, 2020, by = 5), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("#8DD3C7", "darkolivegreen3", "#80B1D3")) +
  facet_wrap(.~Subarea)
fao37_plot

cowplot::save_plot('figures/GFCM_byCountry.pdf', fao37_plot, base_height = 3, base_width = 8.5)

# Add data from Andalusia fishing ports (1985-2021)

andalusia <- read_csv('data/Andalusia landings_Surmediterranea_1985-2021.csv') %>%
  select(Year, `Live weight_mt`) %>%
  rename(Catch_tonnes_Andalusia = `Live weight_mt`) %>%
  mutate(Subarea = '37.1.1 - Balearic')

fao37_andalusia <- bind_rows(fao37, andalusia)

# Split dataframe for plotting
fao37_orig <- subset(fao37_andalusia, is.na(FAO_RECORD))
spain_addition <- subset(fao37_andalusia, !is.na(FAO_RECORD))

# Plot
fao37_andalusia_plot <- ggplot(fao37_orig, aes(x=Year, y=Catch_tonnes, fill=Country))+
  theme_bw()+
  theme(axis.title.x = element_blank(), legend.position = 'bottom')+
  labs(y = 'Estimated catch (tonnes)')+
  geom_col(position = 'stack', width = 0.8)+
  ggpattern::geom_col_pattern(data = spain_addition,
                   pattern = 'stripe',
                   pattern_fill = '#80B1D3',
                   pattern_colour = NA,
                   pattern_angle = 45,
                   pattern_density = 0.5,
                   pattern_spacing = 0.015,
                   pattern_key_scale_factor = 0.6,
                   fill = 'white')+
  geom_line(aes(y=Catch_tonnes_Andalusia, group = Country), linewidth = 0.2)+
  geom_point(aes(y=Catch_tonnes_Andalusia), size = 1)+
  scale_x_continuous(limits = c(1984, NA), breaks = seq(1985, 2020, by = 10), expand = expansion(mult = c(0, 0.01)))+
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("#8DD3C7", "darkolivegreen3", "#80B1D3", "black")) +
  facet_wrap(.~Subarea)
fao37_andalusia_plot

cowplot::save_plot('figures/GFCM_byCountry_withAndalusia.pdf', fao37_andalusia_plot, base_height = 3, base_width = 7.5)

##~~ Compare FAO data to Spain fisheries data ~~##

spain <- read_csv('data/Spain landings_1992-2022.csv')
spain37 <- filter(spain, `FAO Region` == 37) %>%
  rename(Catch_tonnes_Spain = `Live weight (mt)`) %>%
  select(Year, Catch_tonnes_Spain)
fao37_spain <- filter(fao37, Country == 'Spain') %>%
  group_by(Year) %>%
  summarize(Catch_tonnes_FAO = sum(Catch_tonnes))
compare_fao37_spain37 <- full_join(fao37_spain, spain37)
#Essentially the same, except Spain records include 1996-2003. Will add these years to FAO dataset.


##~~ Azores ~~##

azores <- read_csv('data/ICES_2006-2020.csv') %>%
  pivot_longer(cols = -c(1:4),
               names_to = "Year",
               values_to = "Weight_tonnes") %>%
  filter(Area == '27.10.a.2')


# Miscellaneous -----------------------------------------------------------

# Plot colors...
RColorBrewer::brewer.pal(6,"Set3")



