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

url <- "https://stats.nba.com/stats/playerindex?College=&Country=&DraftPick=&DraftRound=&DraftYear=&Height=&Historical=1&LeagueID=00&Season=2020-21&SeasonType=Regular%20Season&TeamID=0&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
players <- data.frame(json_resp$resultSets$rowSet)
colnames(players) <- json_resp[["resultSets"]][["headers"]][[1]]
players <- select(players, "PERSON_ID", "HEIGHT", "WEIGHT")
colnames(players)[which(names(players) == "PERSON_ID")] <- "PLAYER_ID"
players <- players %>% 
  separate(HEIGHT, c('feet', 'inches'), "-", convert = TRUE) %>% 
  mutate(HEIGHT = (12*feet + inches)) %>%
  select(PLAYER_ID, HEIGHT)

player <- nbastatR::nba_players()
player <- player %>% select(idPlayer, urlPlayerHeadshot)
colnames(player)[which(names(player) == "idPlayer")] <- "PLAYER_ID"

url_1 <- "https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=6%2B+Feet+-+Wide+Open&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=4-6+Feet+-+Open&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight="
url_3 <- "https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=2-4+Feet+-+Tight&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight="
url_4 <- "https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=0-2+Feet+-+Very+Tight&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight="

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C2 <- data.frame(json_resp$resultSets$rowSet)
colnames(C2) <- json_resp[["resultSets"]][["headers"]][[1]] 
C2[c("FG3A", "FG3_PCT")] <- sapply(C2[c("FG3A", "FG3_PCT")], as.numeric)
C2[is.na(C2)] <- 0
C2 <- C2 %>%
  select(PLAYER_ID, PLAYER_NAME, FG3A, FG3_PCT)

combined <- merge(players, C2, by="PLAYER_ID")

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C3 <- data.frame(json_resp$resultSets$rowSet)
colnames(C3) <- json_resp[["resultSets"]][["headers"]][[1]] 
C3[c("FG3A", "FG3M")] <- sapply(C3[c("FG3A", "FG3M")], as.numeric)
C3[is.na(C3)] <- 0
C3 <- C3 %>%
  select(PLAYER_ID, FG3A, FG3M)

res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C4 <- data.frame(json_resp$resultSets$rowSet)
colnames(C4) <- json_resp[["resultSets"]][["headers"]][[1]] 
C4[c("FG3A", "FG3M")] <- sapply(C4[c("FG3A", "FG3M")], as.numeric)
C4[is.na(C4)] <- 0
C4 <- C4 %>%
  select(PLAYER_ID, FG3A, FG3M)

res <- GET(url = url_4, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C5 <- data.frame(json_resp$resultSets$rowSet)
colnames(C5) <- json_resp[["resultSets"]][["headers"]][[1]] 
C5[c("FG3A", "FG3M")] <- sapply(C5[c("FG3A", "FG3M")], as.numeric)
C5[is.na(C5)] <- 0
C5 <- C5 %>%
  select(PLAYER_ID, FG3A, FG3M)

contested <- merge(C3, C4, by="PLAYER_ID")
contested <- merge(contested, C5, by="PLAYER_ID")
contested <- contested %>%
  mutate(CFGA = FG3A.x + FG3A.y + FG3A) %>%
  mutate(CFGM = FG3M + FG3M.x + FG3M.y) %>%
  mutate(CFGPer = round(CFGM / CFGA, 3)) %>%
  select(PLAYER_ID, CFGA, CFGPer)

combined <- merge(combined, contested, by="PLAYER_ID")
combined <- merge(player, combined, by="PLAYER_ID")

combined <- combined %>%
  filter((FG3A + CFGA) > 100) %>%
  mutate(Open = round(FG3A / (FG3A + CFGA), 3)) %>%
  mutate(Tot = FG3A + CFGA) %>%
  arrange(desc(Tot)) %>%
  filter(HEIGHT >= 82) %>%
  slice(1:10) %>%
  select(urlPlayerHeadshot, PLAYER_NAME, FG3A, FG3_PCT, CFGA, CFGPer, Open)

combined %>% 
  gt()  %>% 
  cols_label(urlPlayerHeadshot = "",
             PLAYER_NAME = "Player", 
             FG3A = "3PA", 
             FG3_PCT = "3P%", 
             CFGA = "3PA", 
             CFGPer = "3P%",
             Open = "Open %") %>% 
  tab_header(
    title = md("Big Man Shooting"), 
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
    label = "Open",
    columns = vars(FG3A, FG3_PCT)
  ) %>% 
  tab_spanner(
    label = "Contested",
    columns = vars(CFGA, CFGPer)
  ) %>% 
  fmt_percent(
    columns = vars(FG3_PCT, CFGPer, Open),
    decimals = 1
  )  %>%
  data_color(
    columns = c(FG3_PCT),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(CFGPer),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Open),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(FG3_PCT, CFGPer, Open)
  ) %>%
  cols_width(vars(FG3_PCT, CFGPer, Open) ~ px(45),
             vars(FG3_PCT, CFGPer, Open) ~ px(30)) %>% 
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
  gtsave("Big Man Shooting.png")