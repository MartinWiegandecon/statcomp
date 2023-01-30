# ------------------------------------------------------------------------------
# name: 01_seminar.R
# author: martin wiegand (UPF)
# description: R code for seminar 1  
# last updated: january 17, 2022
# ------------------------------------------------------------------------------

# Clean workspace 
rm(list = ls())

# DIRECTORIES 
directory = getwd()
datapath = file.path(directory, "01_seminar_files/data") 
funpath = file.path(directory, "01_seminar_files/funs")
outpath = file.path(directory, "01_seminar_files/out")
source(file = file.path(funpath, "funs.R"))




# Required packages 
packages <- c(
  "eurostat", 
  "readxl",
  "lubridate", 
  "tidyverse", 
  "dplyr", 
  "countrycode", 
  "extrafont", 
  "RColorBrewer"
)

lapply(packages[!(packages %in% installed.packages())], install.packages)

# Load all packages to library
lapply(packages, library, character.only = TRUE)

# AMBIGUOUS COMMANDS 
select <- dplyr::select

# Color settings 
spectral_colors <- RColorBrewer::brewer.pal(11, "Spectral")

col1 = spectral_colors[1]
col2 = spectral_colors[10]
col3 = spectral_colors[3]
col4 = spectral_colors[11]
col5 = spectral_colors[8]

### Selection criteria for data ### 

# Selection criteria for data from EUROSTAT -------------------------------

# Pick two countries 
countrylist = c("DE", "ES")

# Determine year-range of interest 
eurostat_startyear = 2011 
eurostat_endyear = 2019 

migrantnuts2_df %>% 
  mutate(year = year(time))



### Clean NUTS File for students ### 

# Clean nuts file  --------------------------------------------------------

nutsnames_df <- read_excel(path = file.path(datapath, "/NUTS2021.xlsx"), 
           sheet = "NUTS & SR 2021") %>% 
  rename(nuts_code = `Code 2021`, 
         nuts2_name = "NUTS level 2") %>% 
  filter(is.na(nuts2_name) == FALSE) %>% 
  mutate(country_iso2 = str_sub(nuts_code, 1, 2)) %>% 
  select(nuts_code, country_iso2, nuts2_name)

countrylist <- nutsnames_df %>% 
  group_by(country_iso2) %>% 
  count() %>% 
  select(country_iso2) %>%  
  ungroup()

countryvec = countrylist$country_iso2

# Merge in full countryname 
countryname <- countrycode(countryvec, origin = 'eurostat', destination = 'country.name')

country_df <- cbind(countryvec, countryname) %>% 
  as_tibble()

nutsnames_df %<>% 
  left_join(country_df, by = c("country_iso2" = "countryvec")) %>% 
  select(nuts_code, countryname, nuts2_name)

rm(countryvec, countrylist, countryname, country_df)

# Replace extra region suffices 
nutsnames_df %<>% 
  arrange(countryname) %>% 
  mutate(first = str_sub(nuts_code, 1, 2), 
         nuts_code = ifelse(str_sub(nuts_code, 3, 4) == "ZZ", 
                            paste(first, "XX", sep = ""), nuts_code)) %>% 
  select(-first)


write_csv(nutsnames_df, file = file.path(datapath, "nuts2_overview.csv"))


### Load Demographic data ### 
# Pull data from EUROSTAT -------------------------------------------------

population_df <- pull_eurostat_data(tablename = "tgs00096", 
                                    varname = "population")


# Population density 
popdensity_df <- pull_eurostat_data(tablename = "tgs00024", 
                                    varname = "popdensity")

# Population changes per year 
popchanges_df <- get_eurostat("tgs00099", filters = list(
  sinceTimePeriod = eurostat_startyear)
) %>%  
  mutate(year = year(time), 
         country = str_sub(geo, 1, 2)) %>% 
  filter(year <= eurostat_endyear) %>% 
  rename(popchanges = values) %>%  
  select(geo, country, year, indic_de, popchanges) 
                      
# Need to reshape 
popchanges_df %<>% 
  pivot_wider(id_cols = c(geo:year), names_from = indic_de, 
              values_from = popchanges) %>%  
  rename(popgrowth = GROWRT, 
         netmigr = CNMIGRATRT, 
         natgrowth = NATGROWRT)


# Fertility 
fertility_df <- pull_eurostat_data(tablename = "tgs00100", 
                                   varname = "fertilityrate")


# Merge name information from excel 
nutsnames_df <- read_csv(file = file.path(datapath, 'nuts2_overview.csv')) %>% 
  arrange(countryname)
e
# Merge 
nuts2_df <- population_df %>% 
  left_join(popdensity_df) %>% 
  left_join(popchanges_df) %>% 
  left_join(fertility_df) %>% 
  left_join(nutsnames_df, by = c('geo' = 'nuts_code')) 

# For some regions, missing countryname 
nuts2_df <- nuts2_df  %>%    
  mutate(countryname = ifelse(country == "HR", "Croatia", 
                              ifelse(country == "NO", "Norway", 
                                     countryname)))  
  

# Generate country-version of this dataset 
sumvars <- nuts2_df %>% 
  group_by(year, country, countryname) %>% 
  summarise_at(vars("population"), sum, na.rm = TRUE) %>% 
  ungroup()

weightedvars <- nuts2_df %>%  
  group_by(year, country, countryname) %>% 
  summarise_at(vars(popdensity:fertilityrate), ~ weighted.mean(., w = population, 
                                                               na.rm = TRUE)) %>% 
  ungroup()

# Join together 
country_df <- nuts2_df %>% 
  group_by(year, country, countryname) %>% 
  summarise(fertilityrate = weighted.mean(fertilityrate, population, na.rm = TRUE),  
population = sum(population, na.rm = TRUE)) %>%  
    arrange(country, year) %>% 
  ungroup()


### Select Countries and First Descriptives ### 

# Exercise 2  -------------------------------------------------------------

nuts2_geresp_df <- nuts2_df %>% 
  filter(country %in% countrylist) %>% 
  select(geo, nuts2_name, country, year, everything())

country_geresp_df <- country_df %>% 
  filter(country %in% countrylist) %>% 
  group_by(countryname) %>% 
  mutate(natpopgrowth = (population - lag(population))/lag(population) * 100) %>% 
  ungroup()

## Part 1. ## 

# Population growth over time period 

country_geresp_df %>% 
ggplot(mapping = aes(x = year, y = natpopgrowth, color = countryname)) + 
  geom_line(aes(linetype = countryname)) + 
  geom_point(aes(shape = countryname), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Population Growth in %") +
  scale_color_manual(values = c(col1, col2)) + 
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2017, 2019)) +
  theme(
    text = element_text(family = "Palatino Linotype"), 
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, 
                                     colour = "white"),
    legend.justification = c(-0.5, 1.5),
    legend.position = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  ) + 
  ggtitle("Population Growth")


# Fertility rate over time period  
country_geresp_df %>% 
  ggplot(mapping = aes(x = year, y = fertilityrate, color = countryname)) + 
  geom_line(aes(linetype = countryname)) + 
  geom_point(aes(shape = countryname), size = 2) +
  theme_bw() + 
  xlab("Year") + 
  ylab("Fertility Rate") +
  scale_color_manual(values = c(col1, col2)) + 
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2017, 2019)) +
  theme(
    text = element_text(family = "Palatino Linotype"), 
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, 
                                     colour = "white"),
    legend.justification = c(-0.5, 1.5),
    legend.position = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  ) + 
  ggtitle("Fertility Rate")


## Part 2. ## 

# Merge national growth rates to regions 

nuts2_geresp_df %<>% 
  left_join(select(country_geresp_df, country, year, natpopgrowth), 
            by = c("country", "year")) %>% 
  mutate(devnatgrowth = popgrowth/10 - natpopgrowth, 
         abovenatgrowth = ifelse(popgrowth/10 - natpopgrowth > 0, 
                                 1, 0))

# Which factors contribute more to growth 
nuts2_geresp_df %>% 
  mutate(naturalshare = natgrowth/popgrowth, 
         migrationshare = netmigr/popgrowth) %>% 
  select(ends_with("share"), natgrowth, netmigr, popgrowth)


nuts2_geresp_df %>% 
  ggplot(mapping = aes(x = devnatgrowth, group = countryname, 
                       fill = countryname)) + 
  geom_density(alpha = .4) + 
  theme_bw() +
  xlab("Deviation from National Growth Rate") + 
  ylab("Density") +
  scale_fill_manual(values = c(col1, col2)) + 
  theme(
    text = element_text(family = "Palatino Linotype"), 
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, 
                                     colour = "white"),
    legend.justification = c(-0.5, 1.5),
    legend.position = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  ) + 
  ggtitle("Deviation from National Population Growth (in PP)")

# Regions that grew more/less than their nations 

# GER 
nuts2_geresp_df %>%
  filter(devnatgrowth > 1 & country == 'DE') %>%  
  select(nuts2_name, year, popgrowth) %>% 
  arrange(-popgrowth)

# SPAIN 
nuts2_geresp_df %>%
  filter(devnatgrowth > 1 & country == 'ES') %>%  
  select(nuts2_name, year, popgrowth) %>% 
  arrange(-popgrowth)
  




# Which factors contribute more to local growth 

barformat <- nuts2_geresp_df %>% 
  rename(netmigrgrowth = netmigr) %>%  
  select(geo, country, year, popgrowth, natgrowth, netmigrgrowth, abovenatgrowth)
  
barformat %<>% 
  pivot_longer(cols = c(popgrowth, natgrowth, netmigrgrowth)) %>% 
  rename(growth = value)


for(country in c('DE', 'ES')){
  
  # Baseline Graph 
  barformat %>%
    filter(abovenatgrowth == 1 & !!country == paste(country)) %>% 
    ggplot(mapping = aes(x = factor(year), y = growth, fill = name)) + 
    geom_bar(color = "black", position = position_dodge(0.9), stat = 'summary', fun = 'mean') + 
    scale_fill_manual(values = c(col1, col2, col3), 
                      labels = c("Natural", "Net Migration", "Total")) +
    labs(x = "Year", y = "Growth Rate", 
         fill = "Growth Type") + 
    theme(
      text = element_text(family = "Palatino Linotype"), 
      plot.title = element_text(face = "bold", size = 12),
      legend.background = element_rect(fill = "white", size = 4, 
                                       colour = "white"),
      legend.justification = c(-0.5, 1.5),
      legend.position = c(0, 1),
      axis.ticks = element_line(colour = "grey70", size = 0.2),
      panel.grid.major = element_line(colour = "grey70", size = 0.2),
      panel.grid.minor = element_blank()
    ) + 
    theme_bw() + 
    ggtitle(paste("Contribution to Growth in Country", country, sep = " ")) 
  
  # Loop below and across national growth 
  for(above in c(0, 1)){
    
    if(above == 0){
      titlelabel = "Below National Growth"
    }
    else {
      titlelabel = "Above National Growth"
    }
    
    (barformat %>%
    filter(abovenatgrowth == 1 & !!country == paste(country)) %>% 
    ggplot(mapping = aes(x = factor(year), y = growth, fill = name)) + 
    geom_bar(color = "black", position = position_dodge(0.9), stat = 'summary', fun = 'mean') + 
    scale_fill_manual(values = c(col1, col2, col3), 
                      labels = c("Natural", "Net Migration", "Total")) +
    labs(x = "Year", y = "Growth Rate", 
         fill = "Growth Type") + 
    theme(
      text = element_text(family = "Palatino Linotype"), 
      plot.title = element_text(face = "bold", size = 12),
      legend.background = element_rect(fill = "white", size = 4, 
                                       colour = "white"),
      legend.justification = c(-0.5, 1.5),
      legend.position = c(0, 1),
      axis.ticks = element_line(colour = "grey70", size = 0.2),
      panel.grid.major = element_line(colour = "grey70", size = 0.2),
      panel.grid.minor = element_blank()
    ) + 
    theme_bw() + 
    ggtitle(paste("Contribution to Growth in Country", country, "(", titlelabel, ")", 
                  sep = " "))) %>% 
      print()
    
  }
}




### Exercise 3 ### 

# Exercise 3 --------------------------------------------------------------

# National migrant stocks 
migrantnat_df <- get_eurostat("cens_11cob_n") %>%
  filter(geo %in% c('DE', 'ES'), 
         c_birth %in% c('FOR', 'NAT'), 
         age == 'TOTAL' & sex == 'T') %>%
  pivot_wider(id_cols = c(time, geo), names_from = c_birth, 
              values_from = values) %>% 
  rename(nationalmigrants = FOR, 
          nationalnatives = NAT)

# NUTS 2 (need to select NUTS2 later)
migrant_df <- get_eurostat("cens_11cobe_r2") %>%   
  filter(c_birth %in% c('FOR', 'NAT'), 
         age == 'TOTAL' & sex == 'T') %>% 
  pivot_wider(id_cols = c(geo, time, isced97), names_from = c_birth, 
              values_from = values)

# Generate joined education variable 
migrant_df %<>% 
  mutate(edu_broad = ifelse(isced97 == "NED", 0, 
ifelse(isced97 %in% c('ED1'), 1, 
ifelse(isced97 %in% c('ED2', 'ED3', 'ED4'), 2, 
ifelse(isced97 %in% c('ED5', 'ED6'), 3, 
ifelse(isced97 == "TOTAL", 99, 
NA)
)
)
)
)
,
  country = str_sub(geo, 1, 2)) %>% 
  filter(edu_broad %in% c(0, 1, 2, 3, 99), 
         country %in% countrylist)

# Select NUTS2 Regions, and national values 
migrantnuts2_df <- migrant_df %>% 
  mutate(nuts2_helper = str_sub(geo, 4, 4)) %>% 
  filter(nuts2_helper != "") %>% 
  rename(nuts2_code = geo) %>% 
  select(nuts2_code, country, edu_broad, FOR, NAT) %>% 
  group_by(nuts2_code, country, edu_broad) %>% 
  summarise_at(vars(FOR, NAT), ~sum(.)) %>% 
  ungroup()

# National values for stock 
migrantnat_df <- migrant_df %>% 
  filter(geo %in% countrylist) %>% 
  rename(nationalmigrants = FOR, 
         nationalnatives = NAT) %>% 
  select(country, edu_broad, nationalmigrants, nationalnatives)  %>% 
  group_by(country, edu_broad) %>% 
  summarise_at(vars(nationalmigrants, nationalnatives), ~sum(.)) %>% 
  ungroup()

# Merge to compute concentration variables 
migrantnuts2_df %<>% 
  left_join(migrantnat_df) %>% 
  mutate(concentration = (FOR/nationalmigrants)/(NAT/nationalnatives))
  
# Merge in population density 
finalnuts2_df <- migrantnuts2_df %>%  
  left_join(filter(select(nuts2_geresp_df, geo, year, popdensity), year == 2011), 
            by = c("nuts2_code" = "geo"))


  
# Generate plots 

# Loop across education categories 

for(edu in c(99, 0, 1, 2, 3)){
  
  if(edu == 0){
    titlelab = "No Education"
  }
  else if(edu == 1){
    titlelab = "Primary Education"
  }
  else if(edu == 2){
    titlelab = "Secondary Education"
  }
  else if(edu == 3){
    titlelab = "Tertiary Education"
  }
  else {
    titlelab = ""
  }
  
  (finalnuts2_df %>% 
    filter(edu_broad == edu) %>% 
    ggplot(mapping = aes(x = log(popdensity), y = concentration, 
                         color = country)) + 
    geom_point() + 
    geom_smooth(method = 'lm') + 
    scale_color_manual(values = c(col1, col2), 
                    labels = c("Germany", "Spain")) +
    labs(x = "Log Population Density", y = "Migrant Concentration", 
         color = "Country") + 
    theme(
      text = element_text(family = "Palatino Linotype"), 
      plot.title = element_text(face = "bold", size = 12),
      legend.background = element_rect(fill = "white", size = 4, 
                                       colour = "white"),
      legend.justification = c(-0.5, 1.5),
      legend.position = c(0, 1),
      axis.ticks = element_line(colour = "grey70", size = 0.2),
      panel.grid.major = element_line(colour = "grey70", size = 0.2),
      panel.grid.minor = element_blank()
    ) + 
    theme_bw() + 
     ggtitle(paste("Migrant Concentration", titlelab, sep = "\n"))) %>% 
  print()
  
  
}

#### end 



















