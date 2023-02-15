# ------------------------------------------------------------------------------
# name: 03_seminar.R
# author: martin wiegand (UPF)
# description: R code for seminar 2  
# last updated: february 2, 2022
# ------------------------------------------------------------------------------

# Clean workspace 
rm(list = ls())

# DIRECTORIES 
directory = getwd()
datapath = file.path(directory, "02_seminar_files/data") 
funpath = file.path(directory, "02_seminar_files/funs")
outpath = file.path(directory, "02_seminar_files/out")
# source(file = file.path(funpath, "funs.R"))


# Required packages 
packages <- c(
  "eurostat", 
  "readxl",
  "lubridate", 
  "tidyverse", 
  "dplyr", 
  "countrycode", 
  "extrafont", 
  "RColorBrewer", 
  "haven", 
  "magrittr", 
  "Hmisc", 
  "stargazer"
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

# Load data into R 

# Load and explore data  --------------------------------------------------

muniraw_df <- read_dta(file = file.path(
  datapath, "Inquisition_analysis_dataset.dta"
))

muniraw_df %<>% 
  select(munisid, muni_name, autonomia, province, adj_impact, gdppc,  
         religious, c_secondplus, trust2, 
         log_pop, age, class_upper, civil_married, 
         longitude, latitude, dist_river, dist_sea) %>% 
  arrange(munisid)

saveRDS(muniraw_df, file = file.path(
  datapath, "munidata.rds"
))





# Write DATASETS 

write_csv(muniraw_df, file = file.path(
  datapath, "munidata.csv"
))

# Coord dataset 

municoord_df <- read_dta(file = file.path(
  datapath, "muniscoord.dta"
)) %>% 
  rename(munisid = 1, 
         x = 2, 
         y = 3)

# Write 
write_csv(municoord_df, file = file.path(
  datapath, "municoordinates.csv"
))

saveRDS(municoord_df, file = file.path(
  datapath, "municoordinates.rds"
))





# Make a table of summary statistics 

# Which regions had the largest impact of the inquisition 


blues <- brewer.pal(9, "Blues")
blue_palette <- c(blues[2], blues[4], blues[6], blues[8], blues[9])
blue_inverse_palette <- rev(blue_palette)

muniraw_df %<>% 
  mutate(quintiles = as.factor(ntile(adj_impact, 5)))


muniraw_df

municoord_df %<>%
  
  %
  left_join(select(muniraw_df, munisid, adj_impact, quintiles), 
            by = "munisid")

ggplot() + 
  geom_polygon(data = municoord_df, aes(fill = adj_impact, 
                                        x = x, y = y, group = munisid)) + 
  theme_void() + 
  coord_quickmap(
  )


ggplot() + 
  geom_polygon(data = municoord_df, aes(fill = quintiles, 
               x = x, y = y, group = munisid), 
               size = 0, color = "gray45", alpha = 1) + 
  scale_fill_manual(values = blue_palette, name = "Inquisition Impact \n Quintile", 
                    breaks = c(1, 2, 3, 4, 5)) +
  theme_void() + 
  coord_quickmap()


# Correlation 

muniraw_df %>%  
  mutate(non_zero_impact = ifelse(adj_impact > 0, adj_impact, 
                                  NA), 
         block = ifelse(is.na(non_zero_impact) == TRUE, 0, 
                        as.factor(ntile(non_zero_impact, 3)))) %>% 
  ggplot(mapping = aes(x = gdppc, group = block, 
                       fill = block)) + 
  geom_density(alpha = .4)
        

muniraw_df %>% 
  ggplot(mapping = aes(x = log_gdppc)) + 
  geom_density(alpha = .4)







