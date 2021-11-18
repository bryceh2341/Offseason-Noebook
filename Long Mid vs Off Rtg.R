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
library(janitor)
library(hablar)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(dplyr)
library(gt)
library(teamcolors)

theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

#get nba playerIds and headshots
teams <- nbastatR::nba_teams()
teams <- teams %>% select(nameTeam, urlThumbnailTeam)
colnames(teams)[which(names(teams) == "nameTeam")] <- "TEAM_NAME"
teams[teams$TEAM_NAME == "Los Angeles Clippers", "TEAM_NAME"] <- "LA Clippers"
teams <- subset(teams, urlThumbnailTeam!= "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg")

# Get NBA teams and their abbreviations 
nbaTeams <- nba_teams() %>% 
  filter(isNonNBATeam == 0) %>% 
  select(slugTeam, nameTeam)

# Get NBA team colors
teamColors <- teamcolors %>% 
  filter(league == "nba") %>% 
  select(name, primary, secondary)

# Merge team colors with full team names and their abbreviations
teamColors <- left_join(teamColors, nbaTeams, by = c("name" = "nameTeam")) 

url <- "https://api.pbpstats.com/get-totals/nba?Season=2021-22&SeasonType=Regular%2BSeason&StartType=All&Type=Team"
  
json_data <- fromJSON(paste(readLines(url, warn=FALSE), collapse=""))
League <- json_data[["multi_row_table_data"]]
League <- League %>%
  clean_names() %>%
  mutate(off_rtg = points/off_poss*100) %>%
  select(name, long_mid_range_frequency, off_rtg)

df <- left_join(League, teamColors, by = c("name" = "slugTeam"))

# Tweak a few primary colors
df <- df %>% 
  mutate(primary = case_when(
    name == "LAL" ~ "#552583", 
    name == "PHI" ~ "#006BB6", 
    name == "MIA" ~ "#41B6E6", 
    name == "MEM" ~ "#00B2A9", 
    name == "BRK" ~ "#000000", 
    TRUE ~ primary
  ))

# Tweak a few secondary colors
df <- df %>% 
  mutate(secondary = case_when(
    name %in% c("LAC", "BKN", "BOS", "WAS", "PHI") ~ "#FFFFFF", 
    name == "MIA" ~ "#DB3EB1", 
    name == "MEM" ~ "#E43C40", 
    name == "LAL" ~ "#FDB927", 
    name == "DAL" ~ "#000000", 
    TRUE ~ secondary
  ))

# make chart
df %>%
  ggplot() +
  # add points
  geom_label(aes(x = off_rtg,
                 y = long_mid_range_frequency,
                 fill = primary,
                 color = secondary,
                 label = name),
             hjust = .5,
             fontface = 'bold',
             size = 5,
             alpha = .9) +
  # edit axis
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .2)) +
  scale_x_continuous(limits = c(90, 130)) +
  # use primary/secondary as the fills/colors
  scale_fill_identity() +
  scale_color_identity() +
  # facet data, sort by total diff
  #facet_wrap(~fct_reorder(player_name, total_diff)) +
  # Add thematic elements
  theme_owen() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 20, hjust = .5),
        plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5),
        plot.caption = element_text(color = 'gray40'),
        plot.margin = margin(10, 10, 15, 10),
        strip.text.x = element_text(size = 6)) +
  # Add title, subttile, caption
  labs(title = "Offensive Rating and Long 2's",
       subtitle = "2021-22 Regular Season",
       x = "Offensive Rating",
       y = "Long 2 Frequency",
       caption = "pbpstats.com")

# Save plot
ggsave("Team Off Rtg vs. Long 2's.png", w = 6, h = 6, dpi = 300, type = 'cairo')
