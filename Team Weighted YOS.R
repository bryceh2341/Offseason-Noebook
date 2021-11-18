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
library(plyr)

# set theme
theme_owen <- function () { 
  theme_minimal(base_size=9, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
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

# set URL
url <- "https://basketball.realgm.com/nba/players"

# read in URL
page <- url %>% read_html() 

# Create a tibble (dataframe)
df <- page %>% 
  html_nodes('table') %>%
  html_table() %>%
  pluck(1) %>% 
  as_tibble()

df <- df %>%
  select(Player, YOS, `Current Team`)  

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=PullUpShot&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C1 <- data.frame(json_resp$resultSets$rowSet)
colnames(C1) <- json_resp[["resultSets"]][["headers"]][[1]]
C1[c("PULL_UP_FG3A", "PULL_UP_FG3_PCT", "MIN")] <- sapply(C1[c("PULL_UP_FG3A", "PULL_UP_FG3_PCT", "MIN")], as.numeric)
colnames(C1)[which(names(C1) == "PLAYER_NAME")] <- "Player"
C1 <- C1 %>%
  select(Player, MIN)


combined <- merge(df, C1, by="Player")

colnames(combined)[which(names(combined) == "Current Team")] <- "Team"
combined <- combined %>%
  arrange(Team) %>%
  mutate(Count = 1) %>%
  select(Team, MIN, YOS, Count)

total <- ddply(combined,"Team",numcolwise(sum))
total <- total %>%
  mutate(weighted = Count/YOS * MIN)

ages = c(rep(total$MIN[1:30], total$Count[1:30]))

combined <- combined %>%
  mutate(Total = ages) %>%
  mutate(Weight = round(MIN/Total * YOS, 2)) %>%
  select(Team, Weight)

total <- ddply(combined,"Team",numcolwise(sum))
total <- total %>%
  arrange(desc(Weight)) %>%
  mutate(Rank = c(1:30)) %>%
  select(Rank, Team, Weight)

total %>%
  arrange(desc(Weight)) %>%
  gt()  %>%
  cols_label(Team = "Team",
             Weight = "Weighted Experience",
  ) %>%
  tab_header(
    title = md("2021-22 Team Experience"),
    subtitle = "Weighted for Minutes Played Last Season"
  )  %>%
  data_color(
    columns = c(Weight),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Weight)
  ) %>%
  cols_width(vars(Weight) ~ px(50),
             vars(Weight) ~ px(50)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Team == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Team == "League Average")
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
  gtsave("Team YOS.png")