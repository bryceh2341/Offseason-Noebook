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
players <- players %>% select(idPlayer, urlPlayerHeadshot)
colnames(players)[which(names(players) == "idPlayer")] <- "PLAYER_ID"

url <- "https://stats.nba.com/stats/leaguedashptdefend?College=&Conference=&Country=&DateFrom=&DateTo=&DefenseCategory=Less+Than+6Ft&Division=&DraftPick=&DraftYear=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=Touch+6%2B+Seconds&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C1 <- data.frame(json_resp$resultSets$rowSet)
colnames(C1) <- json_resp[["resultSets"]][["headers"]][[1]] 
colnames(C1)[which(names(C1) == "CLOSE_DEF_PERSON_ID")] <- "PLAYER_ID"
C1[c("GP", "PLUSMINUS", "FGA_LT_06")] <- sapply(C1[c("GP", "PLUSMINUS", "FGA_LT_06")], as.numeric)
C1 <- C1 %>%
  filter(PLAYER_POSITION %in% c("C", "F-C", "F", "C-F")) %>%
  select(PLAYER_ID, PLAYER_NAME, GP, PLUSMINUS, FGA_LT_06)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C3 <- data.frame(json_resp$resultSets$rowSet)
colnames(C3) <- json_resp[["resultSets"]][["headers"]][[1]]
C3[c("MIN")] <- sapply(C3[c("MIN")], as.numeric)
C3 <- C3 %>%
  select(PLAYER_ID, MIN)

combined <- merge(C1, C3, by="PLAYER_ID")

combined <- combined %>%
  filter(GP > 3) %>%
  filter(MIN > 10) %>%
  mutate(DFGA = round(36/MIN * (FGA_LT_06), 2)) %>%
  mutate(PCTDFGA = ntile(DFGA, 100)/100) %>%
  mutate(PM = PLUSMINUS * (-1)) %>%
  mutate(PCTPM = ntile(PM, 100)/100) %>%
  select(PLAYER_ID, PLAYER_NAME, DFGA, PCTDFGA, PLUSMINUS, PCTPM) %>%
  filter(PLAYER_NAME %in% c("Nic Claxton", "Paul Millsap", "LaMarcus Aldridge", "Blake Griffin"))

combined <- merge(players, combined, by="PLAYER_ID")
combined <- combined %>% select(urlPlayerHeadshot, PLAYER_NAME, DFGA, PCTDFGA, PLUSMINUS, PCTPM)

combined %>%
  arrange(desc(PCTPM)) %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             PLAYER_NAME = "",
             DFGA = "DFGA",
             PCTDFGA = "Percentile",
             PLUSMINUS = "+/- Exp.",
             PCTPM = "Percentile"
             ) %>%
  tab_header(
    title = "Rim Protection",
    subtitle = "2021-22 Regular Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Shots Contested",
    columns = vars(DFGA, PCTDFGA)
  ) %>%
  tab_spanner(
    label = "Oppoenent FG%",
    columns = vars(PLUSMINUS, PCTPM)
  ) %>%
  fmt_percent(
    columns = vars(PCTDFGA, PCTPM),
    decimals = 0
  )  %>%
  fmt_percent(
    columns = vars(PLUSMINUS),
    decimals = 1
  )  %>%
  data_color(
    columns = vars(PCTDFGA, PCTPM),
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
    columns = vars(PCTDFGA, PCTPM)
  ) %>%
  cols_width(vars(PCTDFGA, PCTPM) ~ px(45),
             vars(PCTDFGA, PCTPM) ~ px(30)) %>%
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
  gtsave("Player Rim Protection.png")
