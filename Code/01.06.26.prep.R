## dependencies 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(tidyverse)
## preprocess text into chapters for all volumes 

V_2 <- file("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Potential_Corpora/Plutarch_Lives_V2.txt",open= "r")
V2 <- paste(readLines("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Potential_Corpora/Plutarch_Lives_V2.txt"),collapse= " ")
splits_v2 =strsplit(V2, split="(LIFE OF|COMPARISON OF)\\s+.+?\\.",fixed= FALSE)[[1]]
titlesV2 <-str_extract_all(V2, "(LIFE OF|COMPARISON OF)\\s+.+?\\.")
# ^ extracts titles 
Daimonds()
t2 <- unlist(titlesV2)
# were in a nested list 

df_v2 <- data.frame(t2[3:19],splits_v2[4:20])
df_v2<-df_v2 %>% rename(Title=t2.3.19.)
df_v2<- df_v2 %>% rename(Text=splits_v2.4.20.)

V_4 <- file("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Potential_Corpora/Plutarch_Lives_V4.txt",open= "r")
V4<- paste(readLines("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Potential_Corpora/Plutarch_Lives_V4.txt"),collapse= " ")
splits_v4 =strsplit(V4, split="(LIFE OF|COMPARISON OF)\\s+.+?\\.",fixed= FALSE)[[1]]
titlesV4 <-str_extract_all(V4, "(LIFE OF|COMPARISON OF)\\s+.+?\\.")


t4 <- unlist(titlesV4)

df_v4 <- data.frame(t4[3:20],splits_v4[4:21])
df_v4<-df_v4 %>% rename(Title=t4.3.20.)
df_v4<- df_v4 %>% rename(Text=splits_v4.4.21.)

## loading in volume 1 and 3 which are already in book format 
df_v1 <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/V1_Chapters.csv")
df_v3 <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/V3_Chapters.csv")

df_v3<-df_v3 %>% rename(Title=t.4.16.)
df_v3<- df_v3 %>% rename(Text=splits.5.17.)

## combining all chapter dataframes together 
lives_chaps <-bind_rows(df_v1,df_v2,df_v3,df_v4)
lives_chaps$X <- NULL

write.csv(lives_chaps,"/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_chapters/lives_chapters.csv")
## pre process text into sentences for all volumes 



## tokenize and prep text 
#loading csv
lives_books <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books/lives_books.csv")
lives_books <- lives_books[-1,] 
  
all_books_tokens <- lives_books %>%
  unnest_tokens(word, Text)

##get word counts and frequencies  per book 
word_counts_books <- all_books_tokens  %>% ## tokenizing 
  group_by(Title) %>%
  count(word, sort = TRUE) #counting words 

no_stops <- word_counts_books %>% #removing stopwords 
  anti_join(stop_words)

##frequencies 
by_work <- no_stops %>%
  select(Title,word,n)%>%
  group_by(Title) %>%
  mutate(freq= (n/sum(n))*100)

write.csv(by_work,"/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books/all_books_tokens.csv")
## bigrams and trigrams 
#bigrams 
bigrams_df <- lives_books %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

## we now need to recollapse the data frame 
#https://bookdown.org/Maxine/tidy-text-mining/tokenizing-by-n-gram.html

bigrams_sep <-bigrams_df%>%  
  separate(bigram, into = c("word1", "word2"), sep = " ")

bigrams_united <- bigrams_sep  %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, c(word1, word2), sep= " ")


bigrams_counts <- bigrams_united %>%
  group_by(Title) %>%
  count(bigram, sort = TRUE)

bigrams_counts <- bigrams_counts%>% mutate(freq= (n/sum(n))*100)

write.csv(bigrams_counts,  "/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books/all_books_bigrams.csv")

####### Class Part 

# loading in dataframes 

all_books <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books/all_books_tokens.csv")
all_books_bigrams <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books/all_books_bigrams.csv")

## different types of graphs 
#https://bookdown.org/Maxine/tidy-text-mining/tokenizing-by-n-gram.html#visualizing-a-network-of-bigrams-with-ggraph

Caesar_bigrams <- all_books_bigrams %>% 
  filter(Title == "LIFE OF C.")

Alexander_bigrams <- all_books_bigrams %>% 
  filter(Title == "LIFE OF ALEXANDER.")

## bar graph attempts 

ggplot(Alexander_bigrams[1:20,], aes(x=n, y=bigram)) + 
  geom_bar(stat = "identity")

ggplot(Caesar_bigrams[20:40,], aes(x=n, y=bigram)) + 
  geom_bar(stat = "identity")

data <- Caesar_bigrams[20:50,]

## line graph attempts 
ggplot(data, aes(x=freq, y=bigram, group=1)) +
  geom_line(color="blue")
 
## how to change colors, add axis titles, size, etc 

##faceting ? 
pdf("ggplot_graphs.pdf", width = 8, height = 6)
print(plot1)  # First plot
print(plot2)  # Second plot
dev.off()

# Save a plot to PNG, adjusting resolution and size
png("ggplot_graph.png", width = 800, height = 600, res = 150)
print(plot1)  # Print the plot

# Close the Graphics Device
dev.off()

##geom_XX + filtering 