# Load Libraries and Data Files ------------------------------------------------
## Script just to look at Aulacoseira

library("tidyverse");packageVersion("tidyverse")

# Set working directory
setwd("./phyto_code/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Set folder name to output graphs into
output <- "plots"

# Import combined EMP & AEU data files 
load("RData/df_phyto.RData")

# Filter out non-Aulacoseira taxa and years outside of FASTR study
df_Aul <- df_phyto %>% 
  filter(Genus == "Aulacoseira") %>%
  filter(Year %in% c(2014,2015,2016,2017,2018,2019))

# Plot GALD by year
plot_GALD <- ggplot(df_Aul, aes(x = Year, y = GALD)) +
  geom_jitter(width = 0.1)

plot_GALD +
  facet_wrap(Study ~ ., ncol = 1)

# Plot Cell Biovolume by Year
plot_Cell_BV <- ggplot(df_Aul, aes(x = Year, y = BV.Avg)) +
  geom_jitter(width = 0.1)

plot_Cell_BV +
  facet_wrap(Study ~ ., ncol = 1)

# Plot EMP and FASTR samples by year, facet by study
ggplot(df_Aul, aes(x = Year, y = BV.um3.per.L)) +
  geom_jitter(width = 0.1,
              size = 2,
              pch = 21, 
              color = "black",
              fill = "darkgreen") +
  labs(x = "Year",
       y = "Biovolume (um^3 per L)") +
  facet_wrap(Study ~ ., ncol = 1)

ggsave(path = output,
       filename = "Fig 4.13 - Aulacoseira_abundance_FASTR_EMP_yearly_all.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=4,
       width=4, 
       dpi="print")

## Plot EMP and FASTR samples by month, facet by study
ggplot(df_Aul, aes(x = Month, y = BV.um3.per.L)) +
  geom_jitter(data = subset(df_Aul, Year == "2016"),
              width = 0.1,
              size = 2,
              pch = 21, 
              color = "black",
              fill = "darkgreen") +
  labs(x = "Year",
       y = "Biovolume (um^3 per L)") +
  facet_wrap(Study ~ ., ncol = 1)

ggsave(path = output,
       filename = "Fig 4.14 - Aulacoseira_abundance_FASTR_EMP_month_2016.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=4,
       width=4, 
       dpi="print")
