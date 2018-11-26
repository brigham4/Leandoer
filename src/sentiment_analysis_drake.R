library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
library(tidytext)
library(readr)
library(ggplot)
library(dplyr)

#load in the lyrics
drake_discography <- read.csv("/Users/ericbrigham/Desktop/datamining/project/drakelyrics.csv")

drake_discography$X <- NULL
drake_discography[] <- lapply(drake_discography, as.character)

#break lyrics into unigrams
broken_lyrics <- drake_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics <- broken_lyrics %>%
  filter(!word %in% custom_stop_words$word)

#finding most common words used by drake
most_common_words <- broken_lyrics %>% count(word) %>% arrange(desc(n))
most_common_words %>% top_n(10) %>% ggplot(aes(x = reorder(word, -n), n)) + geom_col() + labs(y = "Count of Word", x = "Word") 

#Compute average sentiment per album
sentiment_lyrics <- broken_lyrics %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(drake_discography %>% filter(Album == Album) %>% tally())$n)

#finding top 10 positive and negative words used by drake
top_sentiment <- broken_lyrics %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

top_sentiment %>% 
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = TRUE)


#graph per year
sentiment_by_year <- data.frame(Album = sentiment_lyrics$Album, Year = c(2015, 2017, 2013, 2018, 2018, 2016))
sentiment_by_year <- merge(sentiment_by_year, sentiment_lyrics, by = "Album")

ggplot(sentiment_by_year, aes(Year, sentiment)) + geom_point() + geom_text(label = sentiment_by_year$Album, vjust = 2, size = 3) 





