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

population <- get_eurostat("tgs00096", filters = list(
  sinceTimePeriod = 2011
))


population <- select(population, geo, time, values) %>% 
  rename(population = values)


popdensity %<>%
  select(geo, time, values) %>% 
  rename(popdensity = values)

population 
popdensity

population %>% 
  mutate(country = str_sub(geo, 1, 2)) %>% 
  filter(country == "ES" | country == "UK") %>% 
  View()

population %>% 
  mutate(year = year(time)) %>% 
  left_join(popdensity_df, by = c("geo", "year"))

popdensity_df



population %>% 
  mutate(year = year(time))


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








population_df %>% 
  left_join(popdensity_df, by = c("geo", "country", "year"))

popdensity_df

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
  filter(country %in% countrylist)

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
  ggtitle("Deviation from National Population Growth")


# Which factors contribute more to local growth 
nuts2_geresp_df %>% 
  group_by(geo, year) %>%  
  ggplot(mapping = aes(x = year, y = popgrowth, group = year)) + 
  geom_boxplot()


nuts2_geresp_df %>% 
  ggplot() + 
  geom_boxplot(aes(x = factor(year), y = popgrowth))

barformat %>% 
  filter(country == "DE", 
         name != "popgrowth") %>% 
  ggplot(aes(x = year, y = , fill = name)) + 
  geom_boxplot() + 
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2017, 2019))


barformat <- nuts2_geresp_df %>% 
  rename(netmigrgrowth = netmigr) %>%  
  select(geo, country, year, popgrowth, natgrowth, netmigrgrowth, abovenatgrowth)
  
barformat %<>% 
  pivot_longer(cols = c(popgrowth, natgrowth, netmigrgrowth)) %>% 
  rename(growth = value)


barformat %>%
  filter(abovenatgrowth == 1 &  country == 'DE') %>% 
  ggplot(mapping = aes(x = factor(year), y = growth, fill = name)) + 
  geom_bar(color = "black", position = position_dodge(0.9), stat = 'summary', fun = 'mean') + 
  scale_fill_manual(values = c(col1, col2, col3)) + 
  labs(x = "Year", y = "Growth Rate") + 
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
  ggtitle()


### Exercise 3 ### 

# Exercise 3 --------------------------------------------------------------

migrantnat_df <- get_eurostat("cens_11cob_n") %>%
  filter(geo %in% c('DE', 'ES'), 
         c_birth %in% c('FOR', 'NAT'), 
         age == 'TOTAL' & sex == 'T') %>%
  pivot_wider(id_cols = c(time, geo), names_from = c_birth, 
              values_from = values) %>% 
  rename(nationalmigrants = FOR, 
          nationalnatives = NAT)


migrantnuts2_df <- get_eurostat("cens_11cobe_r2")  
  filter(c_birth %in% c('FOR', 'NAT'), 
         y_arriv == 'TOTAL' & sex == 'T') %>% 
  pivot_wider(id_cols = c(geo, time), names_from = c_birth, 
              values_from = values)

migrantnuts2_df %<>%
  mutate(country = str_sub(geo, 1, 2), 
         nuts2_digit = str_sub(geo, 4, 4)) %>% 
  filter(nuts2_digit != "", 
         country %in% c('DE', 'ES')) %>% 
  left_join(migrantnat_df, by = c("country" = "geo", "time")) %>%  
  mutate(year = year(time)) %>% 
  select(geo, year, FOR, NAT, nationalmigrants, nationalnatives, country)

# Merge in popdensity 
migrantnuts2_df <- migrantnuts2_df %>%   
  left_join(select(nuts2_geresp_df, geo, year, popdensity))

migrantnuts2_df <- migrantnuts2_df %>% 
  mutate(migrantconcentr = FOR/nationalmigrants/(NAT/nationalnatives), 
         log_popdensity = log(popdensity)


migrantnuts2_df %>%  
  ggplot(mapping = aes(x = log(popdensity), y = migrantconcentr, color = country)) + 
  geom_point()
ggplot(mapping = aes(x = popdensity_df, y = 
                         migrantconcentr))+ 
  geom_point()























