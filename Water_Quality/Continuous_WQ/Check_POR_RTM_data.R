library(tidyverse)
library(lubridate)

count_mo_yr <- function(df, param) {
  param_ensym <- ensym(param)
  param_enquo <- enquo(param_ensym)  
  
  df_count <- df %>% 
    select(Year, Month, !!param_enquo) %>% 
    filter(!is.na(!!param_enquo)) %>% 
    count(Year, Month) %>% 
    arrange(Month) %>% 
    pivot_wider(names_from = Month, values_from = n) %>% 
    arrange(Year)
  
  return(df_count)
}

sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Processed_Data/Continuous"
  )
)

filename <- "/RTM_OUTPUT_TOE_formatted.csv"

df_orig <- read_csv(
  file = paste0(sharepoint_path, filename),
  col_types = paste0("cTc", str_c(rep("dc", 11), collapse = ""))
)

glimpse(df_orig)

df_mod <- df_orig %>% 
  mutate(
    Year = year(DateTime),
    Month = month(DateTime, label = TRUE)
  )

df_check <- tibble(
  parameter = c(
    "Flow",
    "FlowTF",
    "WaterTemp",
    "Turbidity",
    "SpCnd",
    "DO",
    "pH",
    "Chla",
    "Chla_RFU",
    #"fDOM",
    "Phyco_RFU",
    "NitrateNitrite"
  ),
  dataframe = rep(list(df_mod), 11)
)

df_check <- df_check %>% 
  mutate(df_count = map2(dataframe, parameter, .f = count_mo_yr))

df_check

df_check$df_count[[1]]

