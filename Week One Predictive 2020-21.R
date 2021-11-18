library(tidyverse)
library(extrafont)
library(jsonlite)
library(httr)
library(hablar)
library(janitor)
library(paletteer)
library(prismatic)
library(scales)
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ballr)
library(rvest)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(dplyr)
library(gt)
library(ggpubr)

theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

data <- read.csv(file = 'one_week_diff.csv')

ggplot(data, aes(x=Point.Diff)) + 
geom_histogram(color="black", color='turquoise4', binwidth = 2) +
theme_owen() +
  theme(legend.position="none") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 20),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(y = "# of Teams", 
       x = "Diff. in Net Rating", 
       title = "How Predictive is Week 1?", 
       subtitle = "Data per CleaningTheGlass")

ggplot(data, aes(x=Off....)) + 
  geom_histogram(color="black", color='turquoise4', binwidth = 2) +
  theme_owen() +
  theme(legend.position="none") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 20),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(y = "# of Teams", 
       x = "Diff. in Off. Rating", 
       title = "How Predictive is Week 1?", 
       subtitle = "Data per CleaningTheGlass")

ggplot(data, aes(x=Def....)) + 
  geom_histogram(color="black", color='turquoise4', binwidth = 2) +
  theme_owen() +
  theme(legend.position="none") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 20),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(y = "# of Teams", 
       x = "Diff. in Def. Rating", 
       title = "How Predictive is Week 1?", 
       subtitle = "Data per CleaningTheGlass")