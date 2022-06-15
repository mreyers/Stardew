library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(janitor)
library(ggplot2)
library(ggimage)
library(ggthemes)

# Stardew Valley data!
# Lets load in each file and get an idea of what its all about

crab_pots <- read_csv("data/crabpotandothercatchables.csv",
                      col_names = c("Name", "Description", "Price", 
                                    "Fisher Profession (+25%)",
                                    "Angler Profession (+50%)",
                                    "Location",
                                    "Trap Chance (Non-Mariner)",
                                    "Trap Chance (Mariner)",
                                    "Size (inches)",
                                    "Used In" ),
                      skip = 2,
                      show_col_types = FALSE) %>%
  clean_names()
fish_detail <- read_csv("data/fish_detail.csv", show_col_types = FALSE) %>%
  clean_names()
fish_price <- read_csv("data/fish_price_breakdown.csv", show_col_types = FALSE)
legendary_fish_detail <- read_csv("data/legendary_fish_detail.csv", show_col_types = FALSE)
legendary_fish_price <- read_csv("data/legendary_fish_price_breakdown.csv", show_col_types = FALSE)
legendary_fish_two <- read_csv("data/legendaryfishII.csv", show_col_types = FALSE)
nightmarket_fish <- read_csv("data/nightmarketfish.csv", show_col_types = FALSE)
villagers <- read_csv("data/villagers.csv", show_col_types = FALSE)


# Crab Pots section!
head(crab_pots)

# Looks like the tibble needs some tidy up
# The first two rows actually make up the column names
# Simplest fix is to specify column names and then skip the first two rows (orig title and broken row)

# Fisher profession and angler look a little weird
crab_pots$fisher_profession_25_percent

# Ah its because there are qualities valued here
# Not every good has a quality associated with it
# Lets make a simple formatter to nest a tibble
quality_converter <- function(price_str){
  
  simplified <- stringr::str_remove_all(price_str, "g ?")
  split_res <- stringr::str_split(simplified, "\n\n\n")
  
  quality <- tibble(
    qualities = list(c("Basic", "Iron", "Gold", "Iridium")),
    prices =split_res
    ) %>%
    unnest_longer(col = c(qualities, prices)) %>%
    mutate(prices = as.numeric(prices))
}

crab_pots <- crab_pots %>%
  mutate(
    price = map(price, quality_converter),
    fisher_profession_25_percent = map(fisher_profession_25_percent, quality_converter),
    angler_profession_50_percent = map(angler_profession_50_percent, quality_converter)
  ) 

# Min and max sizes seem easy enough with separate
crab_pots <- crab_pots %>%
  separate(size_inches, into = c("min_size", "max_size"), sep = "-") %>%
  mutate(across(contains("_size"), as.numeric))

# Percentages are dictated by the location they are in
# Freshwater doesnt sum to 100% because of the probability of trash, not listed here

# Also convert the trap chances to numeric values
crab_pots <- crab_pots %>%
  mutate(
    across(contains("trap_chance"), ~as.numeric(stringr::str_remove(., "%")) / 100)
  )

# So anyways there are also images available on the Wiki
# I've downloaded them, lets merge them in
crab_pot_images <- tibble(file_names = list.files("images/crab_pot", full.names=TRUE)) %>%
  mutate(name = stringr::str_remove(
      stringr::str_replace(
        stringr::str_extract(
          file_names, "[A-z_]+\\.png"
        ), "_", " "
      ), "\\.png" 
    )
  )

crab_pots_and_images <- crab_pots %>%
  mutate(name = ifelse(name == "Clam[2]", "Clam", name)) %>%
  mutate(across(contains("size"), ~ifelse(is.na(.), 1, .))) %>%
  left_join(crab_pot_images, by = "name")

# Background image: Skip for now
# crab_pot_pic <- "images/backgrounds/Crab_Pot.png"
# img <- png::readPNG(crab_pot_pic)
# g <- grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE,
#                       gp = grid::gpar(alpha = 0.1))

# Plot szn shortly!
# I want to make a vertical bar plot essentially, replacing the bar with these images and scaling size
# to be max - min size
crab_pots_and_images %>%
  ggplot(aes(y = name)) +
  #annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_image(aes(x = min_size, image = file_names), size = 0.05) +
  geom_image(aes(x = max_size, image = file_names), size = 0.1) + 
  geom_segment(aes(x = min_size + 0.5, xend = max_size - 0.5, y = name, yend = name),
               arrow = arrow(length = unit(0.1, "cm"))) +
  theme_bw() +
  xlab("Range of Sizes (Inches)") +
  ylab("Crab Pot Bounty")


# Lets just move on to the next dataset
# Fish Detail!
fish_detail %>% View()

# I think it'd be cool to see fish difficulty by location
# River, Ocean, Lake, Other will be the selections
fish_detail <- fish_detail %>%
  mutate(primary_location = case_when(
    stringr::str_detect(location, "[Rr]iver") ~ "river",
    stringr::str_detect(location, "[Oo]cean") ~ "ocean",
    stringr::str_detect(location, "[Ll]ake") ~ "lake",
    TRUE ~ "other"
  )) %>%
  separate(difficulty_behavior, into = c("difficulty", "behaviour"), sep = " ") %>%
  mutate(difficulty = as.numeric(difficulty))

# Organize by ascending difficulty
fish_detail <- fish_detail %>%
  group_by(primary_location) %>%
  arrange(difficulty) %>%
  mutate(rank = row_number())

# Fish images
fish_images <- tibble(file_names = list.files("images/fish", full.names=TRUE)) %>%
  mutate(name = stringr::str_remove(
    stringr::str_replace(
      stringr::str_extract(
        file_names, "[A-z_]+\\.png"
      ), "_", " "
    ), "\\.png" 
  )
  )

fish_detail <- fish_detail %>%
  left_join(fish_images, by = "name")

# Time to plot!
fish_detail %>%
  ggplot(aes(x = rank, y = difficulty)) +
  geom_image(aes(image = file_names), size = 0.1) +
  theme_bw() +
  xlab("Difficulty Rank") +
  ylab("Difficulty") +
  facet_wrap(~primary_location)

# Too tight, would want to space this out a little more
