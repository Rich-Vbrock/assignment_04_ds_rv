# David's Script

# ---
#   title: "assignment_04_rmd"
# author: "Richard Viebrock"
# date: "11/11/2019"
# output: html_document
# ---
#   
  ### Part I
  
#   First, set echo to false. Make sure no messages are showing in final knitted document.
# 
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = FALSE)
# ```
# 
# ```{r}

# ---------------------------
# Install & Attach Packages
# ---------------------------

install.packages("directlabels")

library(tidyverse)
library(janitor)
library(directlabels)

# -----------------------------------
# Read-in & clean-up lobster data csv 
# -----------------------------------

lobster_abundance <- read_csv("lobster_abundance_sbc_lter.csv",
                              na = "-99999")%>%
  clean_names()

# --------------------------------
# Create data frame for ggplot
# --------------------------------

lobster_changes <- lobster_abundance %>%
  select(year, site, count) %>% 
  group_by(site, year) %>% 
  summarize(total_count = sum(count)) %>% 
  mutate(site_status = ifelse(site == "IVEE", "MPA",
                              ifelse(site == "NAPL", "MPA", "Non-MPA"))) %>% 
  mutate(site_name = ifelse(site == "IVEE", "Isla Vista",
                            ifelse(site == "CARP", "Carpinteria",
                                   ifelse(site == "AQUE", "Arroyo Quemado", 
                                          ifelse(site == "NAPL", "Naples", "Mohawk")))))


# ----------------------------
# Make a beautiful GGplot
# ----------------------------

ggplot(lobster_changes, aes(x = year, y = total_count, group = site))+
  geom_line(aes(color = site_status), size = 1.5)+
  geom_point(color = "gray35")+
  geom_dl(aes(label = site_name, color = site_status), method = list(dl.combine("last.points"), cex = 0.85))+
  scale_x_continuous(expand = c(0, 0),
                     limits = c(2012, 2019.25),
                     breaks = seq(2012, 2018, by = 1))+
  scale_y_continuous(limits = c(0, 1000),
                     expand = c(0,0),
                     breaks = seq(0, 1000, by = 250))+
  labs(color = "Marine Protected Area (MPA)",
       x = "Year",
       y = "Total Count (Number)",
       title = "Lobster Count by Year (Segan & Vieborck)")+
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "gray75"),
        legend.key = element_rect(fill = "gray85"))

### Part 2

# --------------------------------------------------
# Create a GGplot with two box-plots side-by-side
# --------------------------------------------------

lobster_sizes <- lobster_abundance %>%
  select(size_mm, year, site) %>% 
  mutate(site_name = ifelse(site == "IVEE" & year == "2018", "Isla Vista (2018)",
                            ifelse(site == "IVEE" & year == "2012", "Isla Vista (2012)",
                                   ifelse(site == "NAPL" & year == "2018", "Naples (2018)",
                                          ifelse(site == "NAPL" & year == "2012", "Naples (2012)",
                                                 ifelse(site == "CARP" & year == "2018", "Carpinteria (2018)",
                                                        ifelse(site == "CARP" & year == "2012", "Carpinteria (2012)",
                                                               ifelse(site == "AQUE" & year == "2018", "Arroyo Quemado (2018)",
                                                                      ifelse(site == "AQUE" & year == "2012", "Arroyo Quemado (2012)",
                                                                             ifelse(site == "MOHK" & year == "2018", "Mohawk (2018)", "Mohawk (2012)")))))))))) 
# GGPlot Time

ggplot(data = lobster_sizes, aes(x = site_name, y = size_mm, color = site_name, fill = site_name))+
  geom_boxplot(show.legend = FALSE, alpha = 0.60)+
  scale_color_manual(values = c("dodgerblue", "dodgerblue", "red", "red", "purple", "purple", "orange", "orange", "green", "green"))+
  scale_fill_manual(values = c("dodgerblue", "dodgerblue", "red", "red", "purple", "purple", "orange", "orange", "green", "green"))+
  labs(x = "Site Name",
       y = "Spiny Lobster Size (mm)",
       title = "Size Comparison (Segan & Viebrock")+
  scale_x_discrete(labels = c(
    "Isla Vista (2018)" = "IV '18",
    "Isla Vista (2012)" = "IV '12",
    "Naples (2018)" = "NPLS '18",
    "Naples (2012)" = "NPLS '12",
    "Carpinteria (2018)" = "CARP '18",
    "Carpinteria (2012)" = "CARP '12",
    "Arroyo Quemado (2018)" = "AQUE '18",
    "Arroyo Quemado (2012)" = "AQUE '12",
    "Mohawk (2018)" = "MOHK '18",
    "Mohawk (2012)" = "MOHK '12"))+
  theme_light()

# OR

ggplot(data = lobster_sizes, aes(x = site_name, y = size_mm, color = site_name, fill = site_name))+
  geom_boxplot(show.legend = FALSE, alpha = 0.60)+
  scale_color_manual(values = c("dodgerblue", "dodgerblue", "red", "red", "purple", "purple", "orange", "orange", "green", "green"))+
  scale_fill_manual(values = c("dodgerblue", "dodgerblue", "red", "red", "purple", "purple", "orange", "orange", "green", "green"))+
  labs(x = "Site Name",
       y = "Spiny Lobster Size (mm)",
       title = "Spiny Lobster Size Comparison: 2012 vs. 2018")+
  theme_light()+
  coord_flip()


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

# t-test questions

# 1. For 2012 observations, is there a significant difference in lobster size between MPA and non-MPA sites? 

diff_size_2012 <- t.test(MPA_2012$size_mm, non_MPA_2012$size_mm)

# 2. For 2018 observations, is there a significant difference in lobster size between MPA and non-MPA sites? 

diff_size_2018 <- t.test(MPA_2018$size_mm, non_MPA_2018$size_mm)

# 3. For MPA sites only, is there a significant difference in lobsters observed in 2012 vs. 2018?

diff_size_MPA <- t.test(MPA_2012$count, MPA_2018$count)

# 4. For non-MPA sites only, is there a significant difference in lobsters observed in 2012 vs. 2018?

diff_size_non_MPA <- t.test(non_MPA_2012$count, MPA_2018$count)


# Make a finalized table that includes means, standard deviations, and sample sizes

library(DT)
library(plotly)

summary_table <- MPA_v_nonMPA %>% 
  group_by(site_status, year) %>% 
  summarize(round(mean = mean(size_mm, na.rm = TRUE),digits = 1),
            round(standard_deviation = sd(size_mm, na.rm = TRUE),digits = 1),
            round(sample_size = sum(count),digits = 1))
              
datatable(summary_table)
