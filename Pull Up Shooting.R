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

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=PullUpShot&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C1 <- data.frame(json_resp$resultSets$rowSet)
colnames(C1) <- json_resp[["resultSets"]][["headers"]][[1]]
C1[c("PULL_UP_FG3A", "PULL_UP_FG3_PCT", "MIN")] <- sapply(C1[c("PULL_UP_FG3A", "PULL_UP_FG3_PCT", "MIN")], as.numeric)
C1 <- C1 %>%
  select(PLAYER_ID, PLAYER_NAME, PULL_UP_FG3A, PULL_UP_FG3_PCT, MIN)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C3 <- data.frame(json_resp$resultSets$rowSet)
colnames(C3) <- json_resp[["resultSets"]][["headers"]][[1]]
C3[c("FG3A")] <- sapply(C3[c("FG3A")], as.numeric)
C3 <- C3 %>%
 select(PLAYER_ID, FG3A)

combined <- merge(C1, C3, by="PLAYER_ID")

combined <- combined %>%
  filter(MIN > 100) %>%
  mutate(FGA = round(PULL_UP_FG3A/MIN * (36), 2)) %>%
  mutate(Freq = round(PULL_UP_FG3A/FG3A, 2)) %>%
  mutate(PPS = PULL_UP_FG3_PCT * 3) %>%
  mutate(PCTFGA = ntile(FGA, 100)/100) %>%
  mutate(PCTFreq = ntile(Freq, 100)/100) %>%
  mutate(PCTPPS = ntile(PPS, 100)/100) %>%
  select(PLAYER_ID, PLAYER_NAME, FGA, PCTFGA, Freq, PCTFreq, PPS, PCTPPS) %>%
  filter(PLAYER_NAME %in% c("Shai Gilgeous-Alexander"))

combined <- merge(players, combined, by="PLAYER_ID")
combined <- combined %>% select(urlPlayerHeadshot, PLAYER_NAME, FGA, PCTFGA, Freq, PCTFreq, PPS, PCTPPS)

combined %>%
  arrange(desc(FGA)) %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             PLAYER_NAME = "",
             FGA = "FGA/36",
             PCTFGA = "Percentile",
             Freq = "Freq.",
             PCTFreq = "Percentile",
             PPS = "PPS",
             PCTPPS = "Percentile"
  ) %>%
  tab_header(
    title = "3-pt. Pull Ups",
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
    label = "Shots Taken",
    columns = vars(FGA, PCTFGA)
  ) %>%
  tab_spanner(
    label = "Frequency",
    columns = vars(Freq, PCTFreq)
  ) %>%
  tab_spanner(
    label = "Efficiency",
    columns = vars(PPS, PCTPPS)
  ) %>%
  fmt_percent(
    columns = vars(PCTFGA, PCTFreq, PCTPPS),
    decimals = 0
  )  %>%
  data_color(
    columns = vars(PCTFGA, PCTFreq, PCTPPS),
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
    columns = vars(PCTFGA, PCTFreq, PCTPPS)
  ) %>%
  cols_width(vars(PCTFGA, PCTFreq, PCTPPS) ~ px(45),
             vars(PCTFGA, PCTFreq, PCTPPS) ~ px(30)) %>%
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
  gtsave("Player Pull Up Shooting.png")
