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

url <- "https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C1 <- data.frame(json_resp$resultSets$rowSet)
colnames(C1) <- json_resp[["resultSets"]][["headers"]][["columnNames"]][[2]] 
C1[c("FGA", "FG_PCT")] <- sapply(C1[c("FGA", "FG_PCT")], as.numeric)
C1 <- C1 %>%
  select(PLAYER_ID, PLAYER_NAME, 8, 9)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C3 <- data.frame(json_resp$resultSets$rowSet)
colnames(C3) <- json_resp[["resultSets"]][["headers"]][[1]]
C3[c("MIN", "POTENTIAL_AST", "GP")] <- sapply(C3[c("MIN", "POTENTIAL_AST", "GP")], as.numeric)
C3 <- C3 %>%
  select(PLAYER_ID, MIN, GP)

combined <- merge(C1, C3, by="PLAYER_ID")
 
combined <- combined %>%
  filter(GP > 3) %>%
  filter(MIN > 10) %>%
  mutate(FGA = round(36/MIN * (FGA), 2)) %>%
  mutate(PCTFGA = ntile(FGA, 100)/100) %>%
  mutate(PCTFGP = ntile(FG_PCT, 100)/100) %>%
  select(PLAYER_ID, PLAYER_NAME, FGA, PCTFGA, FG_PCT, PCTFGP) %>%
  filter(PLAYER_NAME %in% c("James Harden", "LaMarcus Aldridge", "Blake Griffin", "Paul Millsap", "Nic Claxton"))

combined <- merge(players, combined, by="PLAYER_ID")
combined <- combined %>% select(urlPlayerHeadshot, PLAYER_NAME, FGA, PCTFGA, FG_PCT, PCTFGP)

combined %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             PLAYER_NAME = "",
             FGA = "FGA",
             PCTFGA = "Percentile",
             FG_PCT = "FG%",
             PCTFGP = "Percentile"
             ) %>%
  tab_header(
    title = "Finishing",
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
    label = "Shots Taken",
    columns = vars(FGA, PCTFGA)
  ) %>%
  tab_spanner(
    label = "Efficiency",
    columns = vars(FG_PCT, PCTFGP)
  ) %>%
  fmt_percent(
    columns = vars(PCTFGA, PCTFGP),
    decimals = 0
  )  %>%
  fmt_percent(
    columns = vars(FG_PCT),
    decimals = 1
  )  %>%
  data_color(
    columns = vars(PCTFGA, PCTFGP),
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
    columns = vars(PCTFGA, PCTFGP)
  ) %>%
  cols_width(vars(PCTFGA, PCTFGP) ~ px(45),
             vars(PCTFGA, PCTFGP) ~ px(30)) %>%
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
  gtsave("Player Finishing.png")
