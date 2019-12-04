#-----------------TidyTuesday 47-----------------
# This weeks is from the Department of Education 
# courtesy of Alex Albright.
# 
# The below code produces a gif using gganimate
#
#----------------- Setup-------------------------
# Install tidytuesdayR package
#devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(stringr)
library(gganimate)
library(ggrepel)
library(ggthemes)
library(scales)
library(magick)
library(gifski)
library(extrafont)
library(fontcm)
library(RColorBrewer)
library(colorRamps)
loadfonts()
theme_set(theme_minimal())

#---------------- Data Cleaning-------------------------

# Data
tuesdata <- tidytuesdayR::tt_load("2019-11-19")
tuesdata <- tidytuesdayR::tt_load(2019, week = 48)

# Format date
data <- tuesdata$loans

data <- data %>% 
  mutate(period = year + (quarter-1)/4)

# QUick rename function
rn <- function(data, from, to){
  data <- data %>% mutate(
    agency_name = 
      ifelse(agency_name == from, to, agency_name)
  )
  return(data)
}

# Multiple instances of same company
data <- data %>% 
  mutate(agency_name = str_remove_all(agency_name, "\\.|,|\\*| Inc")) %>% 
  rn("Windham", "Windham Professionals") %>% 
  rn("Pioneer", "Pioneer Credit Recovery") %>% 
  rn("GC Services LP", "GC Services") %>% 
  rn("FMS", "FMS Investment Corp") %>% 
  rn("ACT", "Account Control Technology") %>% 
  arrange(desc(starting))

# Find total debt owed in each period
data.summary <- data %>% 
  group_by(period) %>% 
  summarise(owed = sum(starting, na.rm = T))

#---------------------Setup frame titles-----------------------------

# String vector giving frame title for each frame in gif
titles <- data %>% 
  mutate(title = paste(year+2000, ": Quarter ", quarter, sep = "")) %>% 
  group_by(title) %>% 
  summarise() %>% 
  pull()
n_titles <- length(titles)
n_times <- 17 # number of frames between data points
n_frames <- n_times*n_titles
titles <- titles %>% rep(each = n_times)

#----------------------Plot Total Debt--------------------------

# Static plot of total studedent debt
static_p1 <- data.summary %>% 
  ggplot(aes(x = period+2000, y = owed/1e9)) + 
  geom_line()+
  geom_area(alpha = 0.3) + 
  geom_point(aes(group = seq_along(period))) + 
  theme_solarized() +
  labs(x = "Year", y = "Total Student Loan Debt (USD)", title = 'US Student Loans', subtitle = '{titles[frame]}') +
  scale_y_continuous(labels = dollar_format(suffix = " Billion"), limits = c(0,120)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14, face = NULL),
        axis.title.x = element_text(size = 10, face = NULL),
        axis.title.y = element_text(size = 10, face = NULL))

# Animate
p1 <- static_p1 +
  transition_reveal(period)
p1_gif <- animate(p1, nframes =n_frames, fps = n_times*2, renderer = gifski_renderer("total.gif"), width = 800, height = 250)

#----------------------Plot Inviddual Agencies--------------------------

# Set up colours for each agency
colourCount <- length(unique(data$agency_name))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# Static plot of agency debt 
static_plot <- data %>% 
  filter(!is.na(starting)) %>% 
  ggplot(aes(reorder(agency_name, starting), starting/1e9, fill = agency_name)) + 
  geom_bar(
    stat = "identity",
    na.rm = TRUE,
    alpha = 0.9
  ) + 
  coord_flip() + 
  theme_solarized()+
  theme(legend.position = "none") +
  labs(y = "Student Loan Debt (USD)",x = "", title = 'Loans Within Agencies',caption = "Data Source: US Department of Education.\nVisualisation: Curtis Murray @curtis_w_murray.") +
  scale_y_continuous(labels = dollar_format(suffix = " Billion"))+
  theme(title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 10, face = NULL),
        plot.caption = element_text(size = 8, face = NULL,hjust = 0)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Accent"))(colourCount),
                    guide = guide_legend(nrow=2))

# Animate
p2 <- static_plot + 
  transition_states(period)
p2_gif <- animate(p2, nframes =n_frames,fps = n_times*2, renderer = gifski_renderer("company.gif"), width = 800, height = 550)

#------------------------- Merge Gifs---------------------

#
p1_mgif <- image_read(p1_gif)
p2_mgif <- image_read(p2_gif)

new_gif <- image_append(c(p1_mgif[1], p2_mgif[1]), stack = TRUE)
for(i in 2:n_frames){
  combined <- image_append(c(p1_mgif[i], p2_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

new_gifS
