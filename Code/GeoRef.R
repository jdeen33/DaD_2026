library(dplyr)
library(rvest)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(stringi)
install.packages("jsonlite") 
install.packages("httr")
library(httr)
library(jsonlite)
library(glue)

#set working directory 

setwd("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/georef")

p_ids <- read.csv("pid_pleiades.csv")

test <- p_ids[1,2]
test1 <- str_split(test,"=")

# https://tidyr.tidyverse.org/reference/separate_wider_delim.html#:~:text=Source:%20R/separate%2Dwider,splits%20with%20regular%20expression%20matches.
df_separated <- p_ids %>% #some small data cleaning tasks 
  separate_wider_delim(
    cols = Loc,
    delim = "=",
    names = c("Place", "id")
  )


#https://www.geeksforgeeks.org/r-language/replace-specific-values-in-column-using-regex-in-r/

df_separated$id <- sub("[a-zA-Z]+", "", df_separated$id)
df_separated$id <- gsub("[^0-9]", "", df_separated$id)


write.csv(df_separated,"/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/georef/cleaned.csv")



location_ids <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/georef/cleaned.csv")


locs <- as.list(location_ids[["id"]])
## ^ this will make our column into a list data type
## we do this so that we can iterate through our ids 



## Before we get to iterating, we can see an example 
 url = "https://pleiades.stoa.org/places/991379/json" #This is the API
 response = GET(url)
 body <- content(response, "text")
 parsed_data <- fromJSON(body, flatten= TRUE)
 
 # The feats data type is nested..like a list of lists 
 #think..russian doll
 # Let's explore!
 ## The information we are interested in extracting 
 ## Latitude and longitude (for mapping)
 ## any other description of time period, etc
 ## first we can parse the JSON data we get back
 feats <- parsed_data$features
 

 ##getting lat and long coordinates
 coordinates<- parsed_data$features$geometry.coordinates

#brief description 
tiny_desc<-  parsed_data$features$properties.snippet


## Yay! Except.... we have 7,976 location ids
## This is when for loops become our friend
smaller <- locs[1:15]
locs_list <- list()
for (i in smaller){
  url <- glue("https://pleiades.stoa.org/places/{i}/json")
  response = GET(url)
  body <- content(response, "text")
  parsed_data <- fromJSON(body)
  lat <-  parsed_data$features$geometry.coordinates
  tiny_desc<- parsed_data$features$properties.snippet
  locs_list<- list(name=smaller[i],Lat = lat, desc= tiny_desc)
}



#chat gpt 

locs_list <- list()

for (i in smaller) {
  url <- glue("https://pleiades.stoa.org/places/{i}/json")
  response <- GET(url)
  body <- content(response, "text", encoding = "UTF-8")
  parsed_data <- fromJSON(body)
  
  coords <- parsed_data$features[[1]]$geometry$coordinates
  tiny_desc <- parsed_data$features[[1]]$properties$snippet
  
  locs_list[[as.character(i)]] <- list(
    name = i,
    lon = coords[1],
    lat = coords[2],
    desc = tiny_desc
  )
}









## chatgpt 

locs_list <- list()

for (pid in smaller) {
  url <- glue("https://pleiades.stoa.org/places/{pid}/json")
  response <- GET(url)
  body <- content(response, "text", encoding = "UTF-8")
  parsed_data <- fromJSON(body)
  
  # first feature (usually the main one)
  feature <- parsed_data$features[[1]]
  
  coords <- feature$geometry$coordinates
  tiny_desc <- feature$properties$snippet
  
  locs_list[[as.character(pid)]] <- list(
    name = pid,
    lon = coords[1],
    lat = coords[2],
    desc = tiny_desc
  )
}
