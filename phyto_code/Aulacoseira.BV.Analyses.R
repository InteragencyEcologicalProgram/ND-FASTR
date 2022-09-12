## Script juts to look at Aulacoseira

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")
library("vegan");packageVersion("vegan")

# Set working directory
setwd("./FASTR.phyto.final/")
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

ggplot(df_Aul, aes(x = Year, y = GALD)) +
  geom_jitter(width = 0.1)

ggplot(df_Aul, aes(x = Year, y = BV.Avg)) +
  geom_jitter(width = 0.1)

## Plot EMP and FASTR samples by year, facet by study
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
       filename = "Aulacoseira_abundance_FASTR_EMP_yearly_all.png", 
       device = "png",
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
       filename = "Aulacoseira_abundance_FASTR_EMP_month_2016.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=4, 
       dpi="print")
