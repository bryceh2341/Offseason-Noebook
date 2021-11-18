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

url <- "https://api.pbpstats.com/get-game-logs/nba?Season=2020-21&SeasonType=Playoffs&EntityId=203507&EntityType=Player"

json_data <- fromJSON(paste(readLines(url), collapse=""))
Stats <- json_data[["multi_row_table_data"]]
Stats[is.na(Stats)] <- 0
Stats <- Stats %>%
  clean_names() %>%
  mutate(fga = fg2a + fg3a) %>%
  mutate(distance = (fg2a / fga * Stats$Avg2ptShotDistance) + (fg3a / fga * Stats$Avg3ptShotDistance) - 9.49) %>%
  filter(opponent == "BKN") %>%
  mutate(efg_pct = efg_pct - .5995) %>%
  select(distance, efg_pct) %>%
  mutate(game = c("Game 1", "Game 2", "Game 3", "Game 4", "Game 5", "Game 6", "Game 7"))

ggplot(Stats, aes(distance, efg_pct)) +
  geom_abline(slope=0, intercept=0, size=1) +
  geom_vline(xintercept = 0, size = 1) +
  geom_text(aes(label=game), size=5,col = "#00471B") +
  xlim(-4.5, 4.5) +
  ylim(-0.15, 0.15) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Average Shot Distance\n+/- Reg. Season Avg.",
       y = "eFG%\n+/- Reg. Season Avg.",
       title = "Giannis Antetokounmpo Shot Distance",
       subtitle = "Eastern Conference Semifinals")
