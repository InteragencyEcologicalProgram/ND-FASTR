
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Processed_Data/Continuous"
  )
)

ryi <- read_csv(
  file = paste0(sharepoint_path, "/RTM_OUTPUT_RYI_formatted.csv"),
  col_types = paste0("cTc", str_c(rep("dc", 12), collapse = ""))
)

ryi_mod <- ryi %>% 
  mutate(
    Year = year(DateTime),
    Month = month(DateTime, label = TRUE)
  )

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

ryi_check <- tibble(
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
    "fDOM",
    "Phyco_RFU",
    "NitrateNitrite"
  ),
  dataframe = rep(list(ryi_mod), 12)
)

ryi_check <- ryi_check %>% 
  mutate(df_count = map2(dataframe, parameter, .f = count_mo_yr))

View(ryi_check$df_count[[1]])

