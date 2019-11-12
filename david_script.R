# David's Script

# attach packages and read in data

library(tidyverse)
library(janitor)

lobster_abundance <- read_csv("lobster_abundance_sbc_lter.csv",
                              na = "-99999")%>%
  clean_names()


lobster_changes <- lobster_abundance %>%
  select(year, site, count) %>% 
  group_by(site, year) %>% 
  summarize(total_count = sum(count))

view(lobster_changes)

MPA_only <- lobster_changes %>% 
  filter(site %in% c("IVEE", "NAPL"))

non_MPA <- lobster_changes %>% 
  filter(site %in% c("CARP", "MOHK", "AQUE"))

plot <- ggplot(lobster_changes, aes(x = year, y = total_count))+
  geom_line(MPA_only, aes(color = site), show.legend = FALSE)+
  geom_line(non_MPA, aes(color = site), show.legend = FALSE) +
  labs(x = "Year",
       y = "Total Count",
       title = "Lobster Count")

