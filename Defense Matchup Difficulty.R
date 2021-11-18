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

players <- nbastatR::nba_players()
players <- players %>% select(namePlayer, urlPlayerHeadshot)
colnames(players)[which(names(players) == "namePlayer")] <- "Player"

url <- "https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&DefPlayerID=1629027&LeagueID=00&Outcome=&PORound=0&PerMode=Totals&Season=2020-21&SeasonType=Regular+Season"
url_2 <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C1 <- data.frame(json_resp$resultSets$rowSet)
colnames(C1) <- json_resp[["resultSets"]][["headers"]][[1]]
colnames(C1)[which(names(C1) == "OFF_PLAYER_ID")] <- "PLAYER_ID"
C1[c("PARTIAL_POSS", "GP")] <- sapply(C1[c("PARTIAL_POSS", "GP")], as.numeric)
C1 <- C1 %>%
  select(PLAYER_ID, PARTIAL_POSS)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C3 <- data.frame(json_resp$resultSets$rowSet)
colnames(C3) <- json_resp[["resultSets"]][["headers"]][[1]]
C3[c("USG_PCT", "GP")] <- sapply(C3[c("USG_PCT", "GP")], as.numeric)
C3 <- C3 %>%
  select(PLAYER_ID, USG_PCT)

data <- read.csv(file = 'FourFactorsRAPM.csv')
colnames(data)[which(names(data) == "ï..playerId")] <- "PLAYER_ID"

data <- data %>%
  select(PLAYER_ID, LA_RAPM__Off)

combined <- merge(C1, C3, by="PLAYER_ID")
combined <- merge(combined, data, by="PLAYER_ID")

combined <- combined %>%
  mutate(USG = PARTIAL_POSS / sum(combined$PARTIAL_POSS) * USG_PCT) %>%
  mutate(OFF = PARTIAL_POSS / sum(combined$PARTIAL_POSS) * LA_RAPM__Off)

C3 <- C3 %>%
  add_row(PLAYER_ID = "1", USG_PCT = sum(combined$USG)) %>%
  mutate(PCTU = ntile(USG_PCT, 100)/100) %>%
  filter(PLAYER_ID == "1")

data <- data %>%
  add_row(PLAYER_ID = 1, LA_RAPM__Off = sum(combined$OFF)) %>%
  mutate(PCTO = ntile(LA_RAPM__Off, 100)/100) %>%
  filter(PLAYER_ID == 1)

combined <- data.frame(Player = "Trae Young", OFF = round(sum(combined$OFF), 2), Off_Diff = data$PCTO[1], USG = sum(combined$USG), Usg_Avg = C3$PCTU[1])
combined <- merge(players, combined, by="Player")
combined <- combined %>%
  select(urlPlayerHeadshot, Player, OFF, Off_Diff, USG, Usg_Avg)


combined %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             Player = "",
             OFF = "LA-ORAPM",
             Off_Diff = "LA-ORAPM Percentile",
             USG = "USG%",
             Usg_Avg = "USG% Percentile"
  ) %>%
  tab_header(
    title = "Defensive Mathcups",
    subtitle = "2020-21 Regular Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Average Matchup Difficulty",
    columns = vars(OFF, Off_Diff, USG, Usg_Avg)
  ) %>%
  fmt_percent(
    columns = vars(Off_Diff, Usg_Avg),
    decimals = 0
  )  %>%
  fmt_percent(
    columns = vars(USG),
    decimals = 2
  )  %>%
  data_color(
    columns = vars(Off_Diff, Usg_Avg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(0, 1),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(Off_Diff, Usg_Avg)
  ) %>%
  cols_width(vars(Off_Diff, Usg_Avg) ~ px(45),
             vars(Off_Diff, Usg_Avg) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Player == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Player == "League Average")
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
  gtsave("Defenese Matchup Difficulty.png")
