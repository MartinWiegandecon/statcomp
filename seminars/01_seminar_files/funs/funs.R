# ------------------------------------------------------------------------------
# name: funs.R
# author: martin wiegand (UPF)
# description: Functions for seminar1  
# last updated: january 17, 2022
# ------------------------------------------------------------------------------

### Function for pulling eurostat data ### 

# Function for pulling eurostat data  -------------------------------------

pull_eurostat_data <- function(tablename, varname){
  
  df <- get_eurostat(paste(tablename), filters = list(
    sinceTimePeriod = eurostat_startyear)
  ) %>% 
  mutate(year = year(time), 
         country = str_sub(geo, 1, 2)) %>% 
  filter(year <= eurostat_endyear) %>% 
  rename(!!paste(varname) := values) %>% 
  select(geo, country, year, starts_with(!!paste(varname)))
  
  return(df)
}

#### 
