# load packages
library(tidyverse)
library(rvest)
library(extrafont)
library(scales)
library(janitor)
library(prismatic)
library(paletteer)

# set theme
theme_owen <- function () { 
  theme_minimal(base_size=9, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

# set URL
url <- "https://basketball.realgm.com/nba/players"

# read in URL
page <- url %>% read_html() 

# Create a tibble (dataframe)
df <- page %>% 
  html_nodes('table') %>%
  html_table() %>%
  pluck(1) %>% 
  as_tibble()
