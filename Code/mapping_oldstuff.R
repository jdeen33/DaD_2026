install.packages("maps")
library(maps)
library(tidyverse)
library(tidytext)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(quanteda)
library(tidytext)
library(igraph)
library(leaflet)


data <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/network_analysis/to_present/everything.csv")



pal <- colorFactor(
  palette = "Dark2",
  domain = data$Title
)

data |>
  leaflet() |>
  addTiles()|>
  addCircleMarkers(
    lat = ~lat,
    lng = ~long,
    label = ~entity_text,
    popup = ~paste(
      "Name:", entity_text, "<br/>",
      "Sentence:", sent_lim, "<br/>",
      "Ancient Place Description:", desc),
   # popup = ~sent_lim,
    color = ~pal(Title),
    fillColor = ~pal(Title),
    radius =  ~sqrt(ent_count_by_life) * 2,
    stroke = TRUE,
    fillOpacity = 0.7,
    weight = 1)    |>

        addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Title,
        title = "Title",
        opacity = 0.7
      )
  





