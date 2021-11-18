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

players <- nbastatR::nba_players()
players <- players %>% select(idPlayer, urlPlayerHeadshot)
colnames(players)[which(names(players) == "idPlayer")] <- "PLAYER_ID"


url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=G&PtMeasureType=Drives&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Drives <- data.frame(json_resp$resultSets$rowSet)
colnames(Drives) <- json_resp[["resultSets"]][["headers"]][[1]] 
Drives[c("DRIVE_FGA", "MIN", "GP", "DRIVE_FTA", "DRIVE_PTS")] <- sapply(Drives[c("DRIVE_FGA", "MIN", "GP", "DRIVE_FTA", "DRIVE_PTS")], as.numeric)
Drives <- Drives %>%
  mutate(Dper = round(36/MIN * DRIVE_FGA, 2)) %>%
  mutate(TS = round((DRIVE_PTS/(2*(DRIVE_FGA + 0.44 * DRIVE_FTA))), 4)) %>%
  filter(GP > 10) %>%
  filter(MIN > 10) %>%
  filter(PLAYER_NAME != c("Luka Doncic", "DeMar DeRozan")) %>%
  select(PLAYER_ID, PLAYER_NAME, Dper, TS) %>%
  add_row(PLAYER_ID = "203915", PLAYER_NAME = "Spencer Dinwiddie*", Dper = 8.88, TS = 0.5602) %>%
  arrange(desc(Dper))

combined <- merge(players, Drives, by="PLAYER_ID")

combined <- combined %>%
  select(urlPlayerHeadshot, PLAYER_NAME, Dper, TS)

combined %>%
  arrange(desc(Dper)) %>%
  slice(1:10) %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             PLAYER_NAME = "Player",
             Dper = "FGA/36",
             TS = "TS%") %>%
  tab_header(
    title = md("Attacking the Rim"),
    subtitle = "2020-2021 Guards"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Drives",
    columns = vars(Dper, TS)
  ) %>%
  fmt_percent(
    columns = vars(TS),
    decimals = 1
  )  %>%
  data_color(
    columns = c(Dper),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(TS),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Dper, TS)
  ) %>%
  cols_width(vars(Dper, TS) ~ px(45),
             vars(Dper, TS) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = PLAYER_NAME == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = PLAYER_NAME == "League Average")
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
  gtsave("Reg Drives.png")