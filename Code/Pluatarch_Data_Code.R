install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr) 
install.packages("tidytext")
library(tidytext)
data("stop_words")
install.packages("ggplot2")
library(ggplot2)
install.packages("RColorBrewer")                  
library("RColorBrewer")   
install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)
install.packages("textdata")
library(textdata)
install.packages("tidyr")
library(tidyr)
install.packages("stringr")
library(stringr)

## loading in text files 
Plu_V1 <- file("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Potential_Corpora/Plutarch_Lives_V3.txt",open="r")
Plu_V1_l <- paste(readLines("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Potential_Corpora/Plutarch_Lives_V3.txt"),collapse= " ")
# ^ making one big text file so that I can split it with regular expressions 

splits =strsplit(Plu_V1_l, split="(LIFE OF|COMPARISON OF)\\s+.+?\\.",fixed= FALSE)[[1]]
## ^ This will split up the string into a nested lists based on regular expression matches 
## wrote this regular expression to match all title headings of different works in Lives (these results don't include)
# the titles themselves 

titles <-str_extract_all(Plu_V1_l, "(LIFE OF|COMPARISON OF)\\s+.+?\\.")
# ^ extracts titles 

t <- unlist(titles)
# were in a nested list 

c_df <- data.frame(t[4:16],splits[5:17])
# slight misalignment between # of titles and chapter text, so splits is indexed weirdly
c_df<- c_df %>% rename(t=Title)
c_df<- c_df %>% rename(Text=splits.2.20.)
c_df <- colnames(c_df)[which(names(c_df) == "t")] <- "Title"
c_df colnames(c_df)[which(names(c_df) == "splits.2.22.")] <- "Text"


write.csv(c_df, "/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/V3_Chapters.csv")


### some text analysis 

# most common words per work

c_df <-  c_df[-c(1:1),]


tokens_df <- c_df %>% 
  unnest_tokens(trigram,Text,token= "ngrams",n=3)
  filter(!is.na(tokens_df))
write.csv(tokens_df, "/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/trigrams_V3.csv")

tokens_df  <- tokens_df  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(bigram))

word_counts <- tokens_df  %>% 
  group_by(t) %>%
  count(word, sort = TRUE)

no_stops <- word_counts %>%
  anti_join(stop_words)


write.csv(no_stops, "/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/word_counts__nostops_V3.csv")


#word_counts['per']<- (word_counts['n']/180079)*100

by_work <- no_stops %>%
  select(t,word,n)%>%
  group_by(t) %>%
  mutate(freq= (n/sum(n))*100)

write.csv(by_work, "/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/word_counts_by_work_V3.csv")
 
#percentage by work 

most_freq <- by_work %>%
  subset(freq>= 0.08)

write.csv(most_freq, "/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/most_freq_tokens_V3.csv")


#theseus <- subset(c_df,t== "LIFE OF THESEUS."|t=="LIFE OF ROMULUS."|t=="COMPARISON OF THESEUS AND ROMULUS.")

#t_theseus <- theseus %>% 
  #unnest_tokens(output= word, input= splits.2.22.)

#token_df_thes <- t_theseus %>%
  #anti_join(stop_words)

#word_counts <- token_df_thes %>% 
 # count(word, sort = TRUE)

#### Network analysis experiments ####
#tutorial https://kenbenoit.net/pdfs/text_analysis_in_R.pdf
install.packages("stringi")
library(stringi)

chaps <- read.csv( "/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/word_counts__nostops_V1.csv")



# text pre processing 

#c_df[c('Text')] <- lapply(c_df[c('Text')],stri_trans_tolower) #lowercase

install.packages("quanteda")
library(quanteda)

# https://books.psychstat.org/textmining/document-term-matrix.html

plu_dtm <- chaps %>% cast_dtm(Title, word, n)

tm::inspect(plu_dtm)

termDocMatrix <- as.matrix(plu_dtm)

network <- graph_from_biadjacency_matrix(termDocMatrix)

# plot it
plot(network)


#tokenizing/stemming 
#c_df[c('Text')] <- lapply(c_df[c('Text')],tokens)
#c_df[c('Text')] <- lapply(c_df[c('Text')],tokens_tolower)
#c_df[c('Text')] <- lapply(c_df[c('Text')],tokens_wordstem)
#sw <- stopwords(language="en")

install.packages('shiny')
install.packages('leaflet')
install.packages('tidyverse')
install.packages('sf')  

library(sf)
library(leaflet)