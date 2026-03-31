install.packages("RSelenium")
install.packages("selenider")
library(selenider)
library(rvest)
library(dplyr)
library(ggplot2)
library(selenium)
# ref https://medium.com/technology-nineleaps/how-to-use-selenium-in-r-b4c92cc3be70
#set our working directory 
#https://ashbythorpe.github.io/selenium-r/articles/selenium.html
#change path to whatever folder you have our named entities in (bert_ner.csv) 
setwd("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books")

ents <- read.csv("bert_ner.csv")

# we want to filter to only 'locations'

locs <- ents %>% filter(label=="loc")

# Since we will be sending requests to an API, we want to limit
# the amount of input in general
#so we can also filter for unique values ie, occuring more than once

locs <- locs %>% distinct(entity_text)
locs <- as.list(locs[["entity_text"]])

# using selenium to automate searching Pleiades 
server <- selenium_server(temp = FALSE)
session <- SeleniumSession$new()
session$navigate("https://pleiades.stoa.org/#searchform")
search_bar <- session$find_element(using = "css selector", value = "#search")

#input <- session$find_element(using = "css selector", value = "input[type='search']")
#find_element(using = "xpath", value="//*[@id="search"]")
search_bar$send_keys("Delphi") 

locs <- c("Scythia")

## ty chatgpt for helping me make this 
## into nested for loops :) 

get_pids <- function(locs){
  results_t <- c()
  for(x in locs){
    search_bar$send_keys(x)
    Sys.sleep(3)
    cat("Searching for:", x, "\n")
    results <- session$find_element(using = "css selector", value = "#results")
    ids <- results$find_elements(using="tag name",value="a")
    test <- results$find_elements(using = "tag name", value = "h2")
    for (t in test) {
      res <- t$get_text()
      results_t <- c(results_t, res)
    }
   
    
    search_bar$clear()
  }
  return(results_t)
  }


testing <-  get_pids(locs)

df <- data.frame(Loc=testing)  

write.csv(df,"/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/georef/pid_pleiades.csv")
  
  for( x in locs){
  search_bar$send_keys(x)
 # Sys.sleep(2)
  
  results <-  session$find_element(using="css selector",value="#results")
  test <- results$find_elements(using="tag name",value="h2")
  for(t in test){
    res <- (t$get_text())
    results_t <- c(res)
  }
return(results_t)
  search_bar$clear()
}

results <- session$find_element(using="css selector",value="#results")

test <- results$find_elements(using="tag name",value="h2")

ids <- results$find_elements(using="tag name",value="a")

for (t in test){
  print(t$get_text())
}

for (i in ids){
  print(i$get_attribute('href'[[1]]))
}





# 2. Stop the Selenium server process
server$stop()
