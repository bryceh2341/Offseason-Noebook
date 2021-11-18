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
colnames(players)[which(names(players) == "idPlayer")] <- "Player_ID"

url_1 <- "https://stats.nba.com/stats/playerdashboardbyshootingsplits?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerID=1629027&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision="

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
All <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
colnames(All) <- json_resp[["resultSets"]][["headers"]][[1]]
All[c("FGM", "FGA", "EFG_PCT", "PCT_UAST_FGM")] <- sapply(All[c("FGM", "FGA", "EFG_PCT", "PCT_UAST_FGM")], as.numeric)
All <- All %>%
  mutate(perc = FGA / sum(FGA)) %>%
  select(GROUP_VALUE, perc, EFG_PCT, PCT_UAST_FGM) %>%
  slice(1:6)

 target <- c("Less Than 5 ft.", "5-9 ft.", "10-14 ft.", "15-19 ft.", "20-24 ft.", "25-29 ft.")
 All <- All[match(target, All$GROUP_VALUE),]
 
 url <- "https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
 url_2 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
 
 res <- GET(url = url, add_headers(.headers=headers))
 json_resp <- fromJSON(content(res, "text"))
 C1 <- data.frame(json_resp$resultSets$rowSet)
 colnames(C1) <- c(1:30)
 C1[is.na(C1)] <- 0
 C1[c(8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24)] <- sapply(C1[c(8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24)], as.numeric)
 C1 <- C1 %>%
   select(1, 2, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24)
 colnames(C1) <- c("Player_ID", "Player", "U5FGA", "U5FGPCT", "FiveFGA", "FiveFGPCT", "TenFGA", "TenFGPCT", "FifteenFGA", "FifteenFGPCT", "TwentyFGA", "TwentyFGPCT", "TwentyfiveFGA", "TwentyfiveFGPCT")

 C1 <- C1 %>%
    mutate(Tot = U5FGA + FiveFGA + TenFGA + FifteenFGA + TwentyFGA + TwentyfiveFGA) %>%
   mutate(U5USG = U5FGA / Tot) %>%
   mutate(FiveUSG = FiveFGA / Tot) %>%
   mutate(TenUSG = TenFGA / Tot) %>%
   mutate(FifteenUSG = FifteenFGA / Tot) %>%
   mutate(TwentyUSG = TwentyFGA / Tot) %>%
   mutate(TwentyfiveUSG = TwentyfiveFGA / Tot) %>%
   mutate(U5PCT = ntile(U5FGPCT, 100)/100) %>%
   mutate(FivePCT = ntile(C1[6], 100)/100) %>%
   mutate(TenPCT = ntile(C1[8], 100)/100) %>%
   mutate(FifteenPCT = ntile(C1[10], 100)/100) %>%
   mutate(TwentyPCT = ntile(C1[12], 100)/100) %>%
   mutate(TwentyfivePCT = ntile(C1[14], 100)/100) %>%
   select(1, 2, 4, 16, 22, 6, 17, 23, 8, 18, 24, 10, 19, 25, 12, 20, 26, 14, 21, 27) %>%
   filter(Player %in% c("Russell Westbrook"))

 combined <- merge(C1, players, by="Player_ID")
 combined <- combined %>%
   select(21, 2:20)



combined %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             Player = "Player",
             U5FGPCT = "FG%",
             U5USG = "% of Shots",
             U5PCT = "Perc.",
             FiveFGPCT = "FG%",
             FiveUSG = "% of Shots",
             FivePCT = "Perc.",
             TenFGPCT = "FG%",
             TenUSG = "% of Shots",
             TenPCT = "Perc.",
             FifteenFGPCT = "FG%",
             FifteenUSG = "% of Shots",
             FifteenPCT = "Perc.",
             TwentyFGPCT = "FG%",
             TwentyUSG = "% of Shots",
             TwentyPCT = "Perc.",
             TwentyfiveFGPCT = "FG%",
             TwentyfiveUSG = "% of Shots",
             TwentyfivePCT = "Perc."
  ) %>%
  tab_header(
    title = md("Player Shot Location"),
    subtitle = "2020-21 Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  fmt_percent(
    columns = vars(U5USG, FiveUSG, TenUSG, FifteenUSG, TwentyUSG, TwentyfiveUSG),
    decimals = 2
  )  %>%
  fmt_percent(
    columns = vars(U5FGPCT, FiveFGPCT, TenFGPCT, FifteenFGPCT, TwentyFGPCT, TwentyfiveFGPCT),
    decimals = 1
  )  %>%
  fmt_percent(
    columns = vars(U5PCT, FivePCT, TenPCT, FifteenPCT, TwentyPCT, TwentyfivePCT),
    decimals = 0
  )  %>%
  tab_spanner(
    label = "<5 ft.",
    columns = vars(U5FGPCT, U5USG, U5PCT)
  ) %>%
  tab_spanner(
    label = "5-9 ft.",
    columns = vars(FiveFGPCT, FiveUSG, FivePCT)
  ) %>%
  tab_spanner(
    label = "10-14 ft.",
    columns = vars(TenFGPCT, TenUSG, TenPCT)
  ) %>%
  tab_spanner(
    label = "15-19 ft.",
    columns = vars(FifteenFGPCT, FifteenUSG, FifteenPCT)
  ) %>%
  tab_spanner(
    label = "20-24 ft.",
    columns = vars(TwentyFGPCT, TwentyUSG, TwentyPCT)
  ) %>%
  tab_spanner(
    label = "25-29 ft.",
    columns = vars(TwentyfiveFGPCT, TwentyfiveUSG, TwentyfivePCT)
  ) %>%
  data_color(
    columns = vars(U5PCT, FivePCT, TenPCT, FifteenPCT, TwentyPCT, TwentyfivePCT),
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
    columns = vars(U5PCT, FivePCT, TenPCT, FifteenPCT, TwentyPCT, TwentyfivePCT)
  ) %>%
  cols_width(vars(U5PCT, FivePCT, TenPCT, FifteenPCT, TwentyPCT, TwentyfivePCT) ~ px(45),
             vars(U5PCT, FivePCT, TenPCT, FifteenPCT, TwentyPCT, TwentyfivePCT) ~ px(30)) %>%
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
  gtsave("Shots.png")