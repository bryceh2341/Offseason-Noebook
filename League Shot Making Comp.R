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

teams <- nbastatR::nba_teams()
teams <- teams %>% select(slugTeam, urlThumbnailTeam) %>% filter(urlThumbnailTeam != "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg")
colnames(teams)[which(names(teams) == "slugTeam")] <- "name"

League <- "https://api.pbpstats.com/get-totals/nba?Season=2021-22&SeasonType=Regular%2BSeason&StartType=All&Type=Team"
Opponent <- "https://api.pbpstats.com/get-totals/nba?Season=2021-22&SeasonType=Regular%2BSeason&StartType=All&Type=Opponent"


json_data <- fromJSON(paste(readLines(League, warn=FALSE), collapse=""))
League <- json_data[["multi_row_table_data"]]
League <- League %>%
  clean_names() %>%
  mutate(Shot_Making = efg_pct - shot_quality_avg) %>%
  select(name, Shot_Making)

json_data <- fromJSON(paste(readLines(Opponent, warn=FALSE), collapse=""))
Opponent <- json_data[["multi_row_table_data"]]
Opponent <- Opponent %>%
  clean_names() %>%
  mutate(Opp_Shot_Making = efg_pct - shot_quality_avg) %>%
  select(name, Opp_Shot_Making)

data <- merge(League, Opponent, by="name")
data <- data %>%
  arrange(desc(Shot_Making)) %>%
  mutate(SM_Rank = c(1:30)) %>%
  arrange(Opp_Shot_Making) %>%
  mutate(OSM_Rank = c(1:30)) %>%
  mutate(Shot_Making_Diff = Shot_Making-Opp_Shot_Making) %>%
  arrange(desc(Shot_Making_Diff)) %>%
  mutate(SM_Diff_Rank = c(1:30)) %>%
  select(name, Shot_Making, SM_Rank, Opp_Shot_Making, OSM_Rank, Shot_Making_Diff, SM_Diff_Rank)

data <- merge(teams, data, by="name")
data <- data %>%
  select(urlThumbnailTeam, name, Shot_Making, SM_Rank, Opp_Shot_Making, OSM_Rank, Shot_Making_Diff, SM_Diff_Rank)

data %>%
  arrange(SM_Diff_Rank) %>%
  #slice(1:10) %>%
  gt()  %>%
  cols_label(urlThumbnailTeam = "",
             name = "Team",
             Shot_Making = "Off Shot Making",
             SM_Rank = "Rank",
             Opp_Shot_Making = "Opp. Shot Making",
             OSM_Rank = "Rank",
             Shot_Making_Diff = "Net Shot Making",
             SM_Diff_Rank = "Rank") %>%
  tab_header(
    title = "What's Sustainable?",
    subtitle = "2021-22 Regular Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlThumbnailTeam)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  fmt_percent(
    columns = vars(Shot_Making, Opp_Shot_Making, Shot_Making_Diff),
    decimals = 2
  )  %>%
  data_color(
    columns = c(Shot_Making, Opp_Shot_Making, Shot_Making_Diff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(Shot_Making, Opp_Shot_Making, Shot_Making_Diff)
  ) %>%
  cols_width(vars(Shot_Making, Opp_Shot_Making, Shot_Making_Diff) ~ px(45),
             vars(Shot_Making, Opp_Shot_Making, Shot_Making_Diff) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = name == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = name == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 24,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.font.names = "Consolas",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(2),
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1),
  ) %>%
  gtsave("League Shot Making Comp.png")

