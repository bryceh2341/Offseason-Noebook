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

#get nba playerIds and headshots
teams <- nbastatR::nba_teams()
teams <- teams %>% select(nameTeam, urlThumbnailTeam)
colnames(teams)[which(names(teams) == "nameTeam")] <- "TEAM_NAME"
teams[teams$TEAM_NAME == "Los Angeles Clippers", "TEAM_NAME"] <- "LA Clippers"
teams <- subset(teams, urlThumbnailTeam!= "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg")

# Get NBA teams and their abbreviations 
nbaTeams <- nba_teams() %>% 
  filter(isNonNBATeam == 0) %>% 
  select(slugTeam, nameTeam)

# Get NBA team colors
teamColors <- teamcolors %>% 
  filter(league == "nba") %>% 
  select(name, primary, secondary)

# Merge team colors with full team names and their abbreviations
teamColors <- left_join(teamColors, nbaTeams, by = c("name" = "nameTeam")) 

url <- "https://stats.nba.com/stats/leaguedashteamptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=Touch+2-6+Seconds&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashteamptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=Touch+6%2B+Seconds&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C1 <- data.frame(json_resp$resultSets$rowSet)
colnames(C1) <- json_resp[["resultSets"]][["headers"]][[1]] 
C1[c("FGA_FREQUENCY", "FG2M", "FG3M", "FGA")] <- sapply(C1[c("FGA_FREQUENCY", "FG2M", "FG3M", "FGA")], as.numeric)
C1 <- C1 %>%
  select(TEAM_ABBREVIATION, FGA_FREQUENCY, FG2M, FG3M, FGA)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
C2 <- data.frame(json_resp$resultSets$rowSet)
colnames(C2) <- json_resp[["resultSets"]][["headers"]][[1]] 
C2[c("FGA_FREQUENCY", "FG2M", "FG3M", "FGA")] <- sapply(C2[c("FGA_FREQUENCY", "FG2M", "FG3M", "FGA")], as.numeric)
C2 <- C2 %>%
  select(TEAM_ABBREVIATION, FGA_FREQUENCY, FG2M, FG3M, FGA)

combined <- merge(C1, C2, by="TEAM_ABBREVIATION")

combined <- combined %>%
  mutate(Freq = FGA_FREQUENCY.x + FGA_FREQUENCY.y) %>%
  mutate(efg = ((FG2M.x + FG2M.y) + (1.5*(FG3M.x + FG3M.y))) / (FGA.x + FGA.y)) %>%
  select(TEAM_ABBREVIATION, Freq, efg)

df <- left_join(combined, teamColors, by = c("TEAM_ABBREVIATION" = "slugTeam"))

# Tweak a few primary colors
df <- df %>% 
  mutate(primary = case_when(
    TEAM_ABBREVIATION == "LAL" ~ "#552583", 
    TEAM_ABBREVIATION == "PHI" ~ "#006BB6", 
    TEAM_ABBREVIATION == "MIA" ~ "#41B6E6", 
    TEAM_ABBREVIATION == "MEM" ~ "#00B2A9", 
    TEAM_ABBREVIATION == "BRK" ~ "#000000", 
    TRUE ~ primary
  ))

# Tweak a few secondary colors
df <- df %>% 
  mutate(secondary = case_when(
    TEAM_ABBREVIATION %in% c("LAC", "BKN", "BOS", "WAS", "PHI") ~ "#FFFFFF", 
    TEAM_ABBREVIATION == "MIA" ~ "#DB3EB1", 
    TEAM_ABBREVIATION == "MEM" ~ "#E43C40", 
    TEAM_ABBREVIATION == "LAL" ~ "#FDB927", 
    TEAM_ABBREVIATION == "DAL" ~ "#000000", 
    TRUE ~ secondary
  ))


# make chart
df %>%
  ggplot() +
  # add points
  geom_label(aes(x = Freq,
                 y = efg,
                 fill = primary,
                 color = secondary,
                 label = TEAM_ABBREVIATION),
             hjust = .5,
             fontface = 'bold',
             size = 5,
             alpha = .9) +
  # edit axis
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(.4, .55)) +
  # use primary/secondary as the fills/colors
  scale_fill_identity() +
  scale_color_identity() +
  geom_vline(xintercept = 0.4805333, size = 1) +
  geom_abline(slope=0, intercept = 0.4847428,  col = "black", size=1) +
  # facet data, sort by total diff
  #facet_wrap(~fct_reorder(player_name, total_diff)) +
  # Add thematic elements
  theme_owen() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 20, hjust = .5),
        plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5),
        plot.caption = element_text(color = 'gray40'),
        plot.margin = margin(10, 10, 15, 10),
        strip.text.x = element_text(size = 6)) +
  # Add title, subttile, caption
  labs(title = "Self Creation and Scoring Efficiency",
       subtitle = "2021-22 Regular Season",
       x = "Frequency",
       y = "Efficiency (eFG%)",
       caption = "stats.nba.com")

# Save plot
ggsave("Team Self Creation.png", w = 6, h = 6, dpi = 300, type = 'cairo')
