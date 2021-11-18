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

url <- "https://api.pbpstats.com/get-totals/nba?Season=2021-22&SeasonType=Regular%2BSeason&StartType=All&Type=Team"

json_data <- fromJSON(paste(readLines(url, warn=FALSE), collapse=""))
Reg <- json_data[["single_row_table_data"]][["ThreePtShootingFoulsDrawnPct"]] * 100

get_data <- function(year) {

  url <- paste0("https://api.pbpstats.com/get-totals/nba?Season=",year,"&SeasonType=Regular%2BSeason&StartType=All&Type=Team")

  json_data <- fromJSON(paste(readLines(url, warn=FALSE), collapse=""))
  data <- json_data[["single_row_table_data"]][["ThreePtShootingFoulsDrawnPct"]]

  return(data*100)

}

years <- c("2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22")

Data = c()

for (i in years){
  Data <- c(Data, get_data(i[1]))
}

data <- data.frame(fg3fr = Data, years = c(1:22))

ggplot(data = data, aes(x = years, y = Data, fill=Data)) +
geom_bar(stat = "identity") +
scale_fill_gradient2(low="#ED174C", mid="#006BB6", high="#002B5C") +
theme_owen() +
scale_x_continuous(breaks = seq(1, 22, by = 1)) +
theme(legend.position="none") +
theme(strip.text.x = element_blank(),
      panel.spacing.x = unit(1, "lines"),
      plot.title.position = 'plot',
      plot.title = element_text(face =  'bold', size = 20),
      plot.subtitle = element_text(size = 12),
      plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
labs(x = "Season (Since 2000)", 
     y = "3-Point Shooting Fouls Drawn Rate", 
     title = "Problem Solved?", 
     subtitle = "Data per pbpstats")

