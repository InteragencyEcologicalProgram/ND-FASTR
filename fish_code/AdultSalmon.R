#File to download and graph the adult salmon return data from GrandTab
#http://www.cbr.washington.edu/sacramento/

library(tidyverse)
library(lubridate)


#URLs to pull the .csvs directly

SpringURL = "http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3ASpring&type=In-River&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"
FallURL = "http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3AFall&type=In-River&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"
WinterURL = "http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3AWinter&type=All&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"


#read in the data
Spring = read.csv(SpringURL, stringsAsFactors = F)

#there is an obnoxious "notes" row at the end we need to get rid of
Spring = Spring[1:(which(Spring$Year == "Notes:")-1),]

#convert the year to a number
Spring = mutate(Spring, Year = as.numeric(substr(Year, 1, 4))) %>%
  rename(sprinrun = Annual)


#fall run
Fall = read.csv(FallURL, stringsAsFactors = F)
Fall = Fall[1:(which(Fall$Year == "Notes:")-1),]

#convert the year to a number
Fall = mutate(Fall, Year = as.numeric(substr(Year, 1, 4))) %>%
  rename(FallRun = Annual)

#winter run
Winter = read.csv(WinterURL, stringsAsFactors = F)
Winter = Winter[1:(which(Winter$Year == "Notes:")-1),]

#convert the year to a number
Winter = mutate(Winter, Year = as.numeric(substr(Year, 1, 4))) %>%
  rename(WinterRun = Annual)

AllAdults = left_join(Fall, Winter) %>%
  left_join(Spring)

#CSV of all the adult escapement
write.csv(AllAdults, "data/Grandtab_adultsalmon.csv", row.names = F)

################################################################################################

#now for some graphs:
#Spring run salmon

p_spch <- ggplot(Spring, aes(x=Year, y=sprinrun))+
  geom_bar(stat="identity", fill="salmon1") +
  theme(legend.position="none") + 
  scale_y_continuous("Spring Run Chinook Adult Returns",limits=c(0, max(Spring$sprinrun))) +
  lt_avg_line(mean(Spring$sprinrun))
p_spch

#Fall run salmon

p_frch <- ggplot(Fall, aes(x=Year, y=FallRun))+
  geom_bar(stat="identity", fill="salmon1") +
  theme(legend.position="none") + 
  scale_y_continuous("Fall Run Chinook Adult Returns",limits=c(0, max(Fall$FallRun))) +
  lt_avg_line(mean(Fall$FallRun))
p_frch

