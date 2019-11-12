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
  summarize(total_count = sum(count)) %>% 
  mutate() 

### Try to mutate MPA or not and get that as a legend


view(lobster_changes)

# MPA_only <- lobster_changes %>% 
#   filter(site %in% c("IVEE", "NAPL"))
# 
# non_MPA <- lobster_changes %>% 
#   filter(site %in% c("CARP", "MOHK", "AQUE"))
# 
# plot <- ggplot(lobster_changes, aes(x = year, y = total_count))+
#   geom_line(MPA_only, aes(color = site), show.legend = FALSE)+
#   geom_line(non_MPA, aes(color = site), show.legend = FALSE) +
#   labs(x = "Year",
#        y = "Total Count",
#        title = "Lobster Count")

#Part 1

# Make a ggplot

ggplot(lobster_changes, aes(x = year, y = total_count))+
  geom_line(aes(color = site), size = 1.5)+
  scale_color_manual(values = c("red", "red", "dodgerblue", "red","dodgerblue"))+
  geom_dl(aes(label = site), method = list(dl.combine("last.points"), cex = 0.75))+
  annotate(geom = "text", x = 2013.5, y = 800, label = "Green = Marine Protected Area", size = 5)+
  annotate(geom = "text", x = 2013, y = 700, label = "Red = Non-MPA", size = 5)+
  scale_x_continuous(expand = c(0, 0),
                     limits = c(2012, 2018.5),
                     breaks = seq(2012, 2018, by = 1))+
  scale_y_continuous(limits = c(0, 1000),
                     expand = c(0,0),
                     breaks = seq(0, 1000, by = 250))+
  labs(x = "Year",
       y = "Total Count",
       title = "Lobster Count (Spiney SeaBrockster)")+
  theme_light()



# Part 2

lobster_sizes <- lobster_abundance %>% 
  select(size_mm, year, site) %>% 
  filter(year %in% c("2012", "2018"))


ggplot(lobster_sizes, aes(x = site, y = size_mm)) +
  geom_jitter(aes(color = site)) +
  geom_boxplot(alpha = .3)


# Part 3
# Compare mean lobster sizes at MPA vs. non-MPA sites in 2012 and 2018

MPA_v_nonMPA <- lobster_abundance %>% 
  mutate(site_status = 
           ifelse(site == "IVEE", "MPA",
         ifelse(site == "NAPL", "MPA", "non-MPA"))) %>% 
  select(size_mm, year, site, count, site_status) %>% 
  filter(year %in% c("2012", "2018"))

# Make different datasets

MPA <- lobster_abundance %>% 
  filter(site %in% c("IVEE", "NAPL")) %>% 
  filter(year %in% c("2012", "2018"))

MPA_2018 <- MPA %>% 
  filter(year %in% c("2018"))

MPA_2012 <- MPA %>% 
  filter(year %in% c("2012"))

non_MPA <- lobster_abundance %>% 
  filter(site %in% c("CARP", "MOHK", "AQUE")) %>% 
  filter(year %in% c("2012", "2018"))

non_MPA_2018 <- non_MPA %>% 
  filter(year %in% c("2018"))

non_MPA_2012 <- non_MPA %>% 
  filter(year %in% c("2012"))
  
# Exploring datasets
ggplot(data = MPA, aes(x = size_mm)) +
  geom_histogram()

ggplot(data = MPA, aes(sample = size_mm)) +
  geom_qq()

ggplot(data = non_MPA, aes(x = size_mm)) +
  geom_histogram()

ggplot(data = non_MPA, aes(sample = size_mm)) +
  geom_qq()         

# Plot side by side
# Overlap is purple

ggplot() +
  geom_histogram(data = MPA, aes(x = size_mm),
                 fill = "dodgerblue") +
  geom_histogram(data = non_MPA, aes(x = size_mm),
                 fill = "red", alpha = .4)

# t-test

MPA_diff_size <- t.test(MPA$size_mm, non_MPA$size_mm)
MPA_diff_count <- t.test(MPA$count, non_MPA$count)

