#-----------------TidyTuesday 47-----------------
# This weeks challenge is to visualise vote data
# of New Zealands favourite birds. 

# Attempting to present it in the form of a circular
# bar-chart, webscraping more info regarding the birds.
#
#----------------- Setup-------------------------
# Install tidytuesdayR package
#devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(fmsb)
library(xml2)
library(rvest)
library(stringr)
theme_set(theme_minimal())
#---------------- Data Cleaning-------------------------

# Data
tuesdata <- tidytuesdayR::tt_load("2019-11-19")
tuesdata <- tidytuesdayR::tt_load(2019, week = 47)

# Some bird names were not the same as website used to scrape
nz_bird <- tuesdata$nz_bird %>% 
  filter(bird_breed != "Bittern",
         bird_breed != "Harrier",
         bird_breed != "New Zealand Robin",
         bird_breed != "Saddleback",
         bird_breed != "Takahē") %>% 
  mutate(bird_breed = ifelse(bird_breed == "Gibson's Albatross","Antipodean Albatross", bird_breed),
         bird_breed = ifelse(bird_breed == "Fantail","New Zealand Fantail", bird_breed),
         bird_breed = ifelse(bird_breed == "Kererū","New Zealand Pigeon", bird_breed),
         bird_breed = ifelse(bird_breed == "Kingfisher","Sacred Kingfisher", bird_breed),
         bird_breed = ifelse(bird_breed == "Kōkako","North Island Kōkako", bird_breed),
         bird_breed = ifelse(bird_breed == "Mōhua","Yellowhead", bird_breed),
         bird_breed = ifelse(bird_breed == "Rockhopper Penguin","Moseley's Rockhopper Penguin", bird_breed),
         bird_breed = ifelse(bird_breed == "Scaup","New Zealand Scaup", bird_breed),
         bird_breed = ifelse(bird_breed == "Takahe","South Island Takahe", bird_breed),
         bird_breed = ifelse(bird_breed == "The Otago Shag","Stewart Island Shag", bird_breed),
         bird_breed = ifelse(bird_breed == "Whenua Hou Diving Petrel","Common Diving Petrel", bird_breed),
         bird_breed = ifelse(bird_breed == "Campbell Black-Browed Albatross","Campbell Black Browed Mollymawk", bird_breed)
  )

# Formate votes, only use 1st preference, find total count
nz_bird_clean <- nz_bird %>% 
  drop_na() %>% 
  mutate(vote_rank = str_remove(vote_rank, "vote_")) %>% 
  group_by(bird_breed, vote_rank) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  group_by(bird_breed) %>% 
  filter(vote_rank == 1) %>% 
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  #top_n(1*80,total) %>% 
  group_by(bird_breed) %>% 
  arrange(desc(total)) %>% 
  ungroup()

#------------------------- Webscraping -------------------------

# Function to webscrape
scrape_bird <- function(messy_name){
  
  cat("Scraping ",messy_name, "\n")
  
  name <- str_replace_all(messy_name, " ", "-")
  name <- str_remove_all(name, "'")
  
  # Find bird webpage
  url <- paste("http://nzbirdsonline.org.nz/species/", name, sep = "")
  # Read html
  html <- try(read_html(url))
  if('try-error' %in% class(html)){ 
    cat("Cannot load webpage", url, "\n") 
    return(NA) 
  }
  # Scrape info about bird eg order, family, etc
  info <- html %>% 
    html_nodes(".bird-header") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    as_vector()
  # Format in Tibble
  info_tibble <- tibble(bird_breed = messy_name,
                        Order = info[1],
                        Family = info[2], 
                        NZ_status = info[3], 
                        Cons_status = info[4])
  # Scrape image (save to file)
  img_messy <- html %>% 
    html_nodes(".main-bird-image") %>% 
    html_node("a.fresco") %>% 
    as.character() %>% 
    str_split("src=") %>% 
    unlist() %>% 
    str_split("></a>") %>% 
    unlist()
  img_url <- str_remove_all(img_messy[2], "\"")
  download.file(img_url, paste("scraped_imgs/",name,".jpg", sep = ""), mode = "wb")
  
  return(info_tibble)
}

# Map scrape function to each bird
bird_names <- nz_bird %>% 
  group_by(bird_breed) %>% 
  summarise() %>% 
  group_by(bird_breed) %>% 
  nest() %>% 
  mutate(
    data = map(bird_breed, scrape_bird)
  )

# Having trouble unnesting the above df so 
# using a loop
new_df <- bird_names$data[1] %>% unlist()
for(i in 2:nrow(bird_names)){
  tmp <- bird_names$data[i] %>% unlist()
  if(is.na(tmp)){
    next
  }
  new_df <- bind_rows(new_df, tmp)
}

# Merge with full data
birds_with_info <- new_df %>% 
  right_join(nz_bird_clean) %>% 
  arrange(desc(total)) %>% 
  mutate(bird_breed = ifelse(bird_breed == "Yellow-eyed penguin", "Yellow-Eyed Penguin", bird_breed)) %>% 
  arrange(Cons_status, desc(total)) %>% 
  mutate(id = 1:n()) %>% 
  mutate(Cons_status = Cons_status) %>% 
  group_by(Cons_status) %>% 
  mutate(n_group = n()) %>% 
  ungroup() %>% 
  mutate(Cons_status = as.factor(ifelse(n_group < 3, "Other", Cons_status)))

#----------------- Plots ----------------- 
# Adapted from 
# https://www.r-graph-gallery.com/299-circular-stacked-barplot.html

empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*length(unique(birds_with_info$Cons_status)), ncol(birds_with_info)) )
colnames(to_add) <- colnames(birds_with_info)
to_add$Cons_status <- rep(levels(birds_with_info$Cons_status), each=empty_bar)
to_add$id <- (nlevels(birds_with_info$id)+1):(nlevels(birds_with_info)+nrow(to_add))
data <- rbind(birds_with_info, to_add)
data <- data %>% arrange(Cons_status, total)
data$id <- seq(1, nrow(data))


label_data <- data %>% 
  select(id, bird_breed, total) %>%
  group_by(id, bird_breed) %>% 
  summarise(value = total[1])

number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

Conservation_status <- birds_with_info %>% 
  select(Cons_status) %>% 
  group_by(Cons_status) %>% 
  summarise() %>% 
  mutate(color = c("blue", "grey", "yellow", "purple", "red", "orange", "brown","green", "pink", "cyan"))

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(Cons_status) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
base_angle <- - 360 *((base_data$end+base_data$start)/2)/number_of_bar
base_angle <- ifelse(base_angle < -90 & base_angle>-270, base_angle+180, base_angle)
base_data$angle <- base_angle
base_data$hjust <- ifelse(base_angle < -90, 1, 0)

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] 
grid_data$start <- grid_data$start 
grid_data <- grid_data[-1,]
grid_angle <- 90 - 360 *((grid_data$end+grid_data$start)/2)/number_of_bar
grid_data$hjust <- ifelse(grid_angle < -90, 1, 0)
grid_data$angle <- ifelse(grid_angle < -90, angle+180, angle)

tot <- max(birds_with_info$total)

data %>% 
  ggplot(aes(as.factor(id), total), alpha = 0.5) + 
  ylim(-6000, 10000) +
  geom_bar(stat = "identity", aes(fill = Cons_status), alpha = 0.7) + 
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) + 
  coord_polar(start = 0) + 
  geom_text(data = label_data, 
            aes(x=as.factor(id), 
                y=value+600, 
                label=bird_breed,
                hjust=hjust), 
            color="black", 
            fontface="bold",
            alpha=0.6, 
            size=2.5, 
            angle= label_data$angle, 
            inherit.aes = FALSE ) +
  geom_segment(data=base_data, aes(x = start, y = -400, xend = end, yend = -400), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, 
            aes(x = title, 
                y = -1500, 
                label= str_replace(Cons_status, " ", "\n"),
                hjust = 0.5), 
            colour = "black", 
            alpha=0.8, 
            size=2, 
            fontface="bold", 
            angle = base_data$angle,
            inherit.aes = FALSE)
