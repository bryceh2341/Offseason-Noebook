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
library(future)
library(furrr)
library(ggrepel)
library(ggtext)


theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

data <- read.csv(file = 'FourFactorsRAPM.csv')
colnames(data)[which(names(data) == "ï..playerId")] <- "PLAYER_ID"

players <- nbastatR::nba_players()
players <- players %>% select(idPlayer, urlPlayerHeadshot)
colnames(players)[which(names(players) == "idPlayer")] <- "PLAYER_ID"

# ggplot(data, aes(RA_ORBD__Off, RA_ORBD__Def)) +
#   geom_point(color="#585858") +
#   geom_abline(slope=0, intercept=0,  col = "black", lty=1, size=1) +
#   geom_vline(xintercept = 0, size = 1) +
#   annotate("text", label = "Steven Adams", x = 4.25, y = 2.5, size = 5, colour = "#0f586c", face = 'bold') +
#   geom_segment(aes(x = 4.25, y = 2.375, xend = 4.68, yend = 1.3),
#                arrow = arrow(length = unit(0.5, "cm")), size=1.1, colour = "#0f586c") +
#   theme_owen() +
#   theme(strip.text.x = element_blank(),
#         panel.spacing.x = unit(1, "lines"),
#         plot.title.position = 'plot',
#         plot.title = element_text(face =  'bold', size = 14),
#         plot.subtitle = element_text(size = 12),
#         plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
#   labs(x = "Offensive Rebounds Generated",
#        y = "Defensive Rebounds Generated",
#        title = "RAPM Rebound Data",
#        subtitle = "Over the Past 5 Seasons | Data Per nbashotcharts.com")

data <- data %>%
  mutate(PCT = ntile(RA_ORBD, 100)/100) %>%
  mutate(PCTDef = ntile(RA_ORBD__Def, 100)/100) %>%
  mutate(PCTOff = ntile(RA_ORBD__Off, 100)/100) %>%
  filter(playerName %in% c("Richaun Holmes", "Alex Len", "Tristan Thompson")) %>%
  select(PLAYER_ID, playerName, RA_ORBD, PCT, RA_ORBD__Def, PCTDef, RA_ORBD__Off, PCTOff)

data <- merge(players, data, by="PLAYER_ID")
data <- data %>% select(urlPlayerHeadshot, playerName, RA_ORBD, PCT, RA_ORBD__Def, PCTDef, RA_ORBD__Off, PCTOff)

data %>%
  arrange(desc(RA_ORBD)) %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             playerName = "",
             RA_ORBD = "RAPM Reb.",
             PCT = "Percentile",
             RA_ORBD__Def = "RAPM DReb.",
             PCTDef = "Percentile",
             RA_ORBD__Off = "RAPM OReb.",
             PCTOff = "Percentile") %>%
  tab_header(
    title = "Player Rebounding",
    subtitle = "RAPM Rebound Data"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Total",
    columns = vars(RA_ORBD, PCT)
  ) %>%
  tab_spanner(
    label = "Defensive",
    columns = vars(RA_ORBD__Def, PCTDef)
  ) %>%
  tab_spanner(
    label = "Offensive",
    columns = vars(RA_ORBD__Off, PCTOff)
  ) %>%
  fmt_percent(
    columns = vars(PCT, PCTDef, PCTOff),
    decimals = 0
  )  %>%
  data_color(
    columns = vars(PCT, PCTDef, PCTOff),
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
    columns = vars(PCT, PCTDef, PCTOff)
  ) %>%
  cols_width(vars(PCT, PCTDef, PCTOff) ~ px(45),
             vars(PCT, PCTDef, PCTOff) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = playerName == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = playerName == "League Average")
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
  gtsave("RAPM Rebounding.png")
