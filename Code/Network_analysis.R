library(tidyverse)
library(tidytext)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(quanteda)
library(tidytext)
library(quanteda.textplots)

## much improved NER output 
## applied a pre- trained machine learning model for historical texts 
## below are links to all references used to write this code
## ref: https://huggingface.co/stefan-it/hmbench-ajmc-en-hmbert-bs8-wsFalse-e10-lr5e-05-poolingfirst-layers-1-crfFalse-3
# Source - https://stackoverflow.com/a
# Posted by Gopala
# Retrieved 2026-01-14, License - CC BY-SA 3.0
# Source - https://stackoverflow.com/q
# Posted by onlyjust17
# Retrieved 2026-01-14, License - CC BY-SA 4.0
#https://slcladal.github.io/net.html#Data_preparation 

#loading our data 
 ents <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books/bert_ner.csv")
 

 #filtering to the life of alexander
 #just to have a smaller dataset to practice indexing
  alexander <- ents %>% filter(Title == "LIFE OF ALEXANDER.") 


#indexing a matrix (dataframe) # R indexes by [row,column] and indices start at 1 
alexander[1] #selects first column

alexander[1,] #selects all columns and first row

alexander[1,2] #selects row 1 column 2 

x <- as.data.frame(alexander[1:2,2:4]) #selects first and second rows for all columns

t <- as.data.frame(alexander[5:10,])

# now let's practice formatting data for our networks 

## I want to find all co occurences of 
## entities in each sentence 
## for every text in the corpus 
ents <- read.csv("/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/all_books/bert_ner.csv")


# The 'scope' label captured a lot of 
# numbers and dates 
#for now we are going to remove them
# although it would be nice to get dates back

 ents <- ents %>%
   filter(label != "scope")
 


 # this is our format for counting all of the co-occurences of entities across works 
 ents_tidy <- ents %>% group_by(Title)%>% count(entity_text,sort=TRUE)

Caesar <- ents_tidy %>% filter(Title=="LIFE OF C.")
 
 ents_tidy$dup_ents <- ents_tidy$entity_text
 
 ents_tidy$dup_ents <- NULL
 
 #ents_tidy$has_match <- sapply(ents_tidy$entity_text, function(x) {
  # any(grepl(x, ents_tidy$dup_ents))
 #})
 
 
 
 write.csv(ents_pivot,"/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/network_analysis/by_book_pivot.csv")
 

 
 ## now we can prep the data to load into a network graph
 
 ents_cmx <- crossprod(table(ents_tidy[1:2]))
 diag(ents_cmx) <- 0
 ent_df <- as.data.frame(ents_cmx)
 #saving this format too just in case 
 write.csv(ent_df,"/Users/jdeen@middlebury.edu/Documents/DaD_2026/Formatted_Text_Data/network_analysis/co_coo_by_text.csv")

 # create a document feature matrix
 net_dfm <- quanteda::as.dfm(ent_df)
 # create feature co-occurrence matrix
 net_fcm <- quanteda::fcm(net_dfm, tri = F)
 # inspect data
 head(net_fcm)
 
# trimming terms 
dfmat <- dfm_trim(net_dfm, min_termfreq = 500) 
fcmat<- fcm(dfmat)
 
feat <- names(topfeatures(dfmat, 40))
fcmat_news_select <- fcm_select(fcmat, pattern = feat, selection = "keep")
dim(fcmat_news_select)

size <- log(colSums(dfm_select(dfmat, feat, selection = "keep"))) ## log scale > check

set.seed(144)
textplot_network(fcmat_news_select, min_freq = 0.8, vertex_size = size / max(size) * 3)







 
 
 
 
 
 
 
 quanteda.textplots::textplot_network(
   x = net_fcm,                    # a fcm or dfm object
   min_freq = 0.9,                   # frequency count threshold or proportion for co-occurrence frequencies (default = 0.5)
   edge_alpha = 0.5,                 # opacity of edges ranging from 0 to 1.0 (default = 0.5)
   edge_color = "gray",            # color of edges that connect vertices (default = "#1F78B4")
   edge_size = 2,                    # size of edges for most frequent co-occurrence (default = 2)
   # calculate the size of vertex labels for the network plot
   vertex_labelsize = net_dfm %>%
     # convert the dfm object to a data frame
     quanteda::convert(to = "data.frame") %>% 
     # exclude the 'doc_id' column
     dplyr::select(-doc_id) %>%
     # calculate the sum of row values for each row
     rowSums() %>%
     # apply the natural logarithm to the resulting sums
     log(),
   vertex_color = "#4D4D4D",         # color of vertices (default = "#4D4D4D")
   vertex_size = 2                   # size of vertices (default = 2)
 )
 
 
 ## lexical dispersion plot 
 # https://quanteda.io/articles/pkgdown/examples/plotting.html#lexical-dispersion-plot

 
 