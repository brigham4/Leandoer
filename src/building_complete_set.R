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
leandoer_discography <- read.csv("/Users/ericbrigham/Desktop/datamining/project/leandoerlyrics.csv")
migos_discography <- read.csv("/Users/ericbrigham/Desktop/datamining/project/migoslyrics.csv")
thug_discography <- read.csv("/Users/ericbrigham/Desktop/datamining/project/thuglyrics.csv")
post_discography <- read.csv("/Users/ericbrigham/Desktop/datamining/project/postlyrics.csv")
tekashi_discography <- read.csv("/Users/ericbrigham/Desktop/datamining/project/tekashilyrics.csv")
yachty_discography <- read.csv("/Users/ericbrigham/Desktop/datamining/project/yachtylyrics.csv")

drake_discography$X <- NULL
drake_discography[] <- lapply(drake_discography, as.character)

leandoer_discography$X <- NULL
leandoer_discography[] <- lapply(leandoer_discography, as.character)

migos_discography$X <- NULL
migos_discography[] <- lapply(migos_discography, as.character)

thug_discography$X <- NULL
thug_discography[] <- lapply(thug_discography, as.character)

post_discography$X <- NULL
post_discography[] <- lapply(post_discography, as.character)

tekashi_discography$X <- NULL
tekashi_discography[] <- lapply(tekashi_discography, as.character)

yachty_discography$X <- NULL
yachty_discography[] <- lapply(yachty_discography, as.character)


#Handling Yachty
#break lyrics into unigrams
broken_lyrics_yachty <- yachty_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics_yachty <- broken_lyrics_yachty %>%
  filter(!word %in% custom_stop_words$word)

#Compute average sentiment per album
sentiment_lyrics_yachty <- broken_lyrics_yachty %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(yachty_discography %>% filter(Album == Album) %>% tally())$n)

#Add Years
sentiment_by_year_yachty <- data.frame(Album = sentiment_lyrics_yachty$Album, Year = c(2017, 2018, 2016, 2018, 2016, 2018, 2015, 2016, 2017))
sentiment_by_year_yachty <- merge(sentiment_by_year_yachty, sentiment_lyrics_yachty, by = "Album")

#Handling Drake
#break lyrics into unigrams
broken_lyrics_drake <- drake_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics_drake <- broken_lyrics_drake %>%
  filter(!word %in% custom_stop_words$word)

#Compute average sentiment per album
sentiment_lyrics_drake <- broken_lyrics_drake %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(drake_discography %>% filter(Album == Album) %>% tally())$n)

#Add Years
sentiment_by_year_drake <- data.frame(Album = sentiment_lyrics_drake$Album, Year = c(2015, 2017, 2013, 2018, 2018, 2016))
sentiment_by_year_drake <- merge(sentiment_by_year_drake, sentiment_lyrics_drake, by = "Album")

#Handling the man: Leandoer
#break lyrics into unigrams
broken_lyrics_leandoer <- leandoer_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics_leandoer <- broken_lyrics_leandoer %>%
  filter(!word %in% custom_stop_words$word)

#Compute average sentiment per album
sentiment_lyrics_leandoer <- broken_lyrics_leandoer %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(leandoer_discography %>% filter(Album == Album) %>% tally())$n)

#Add Years
sentiment_by_year_leandoer <- data.frame(Album = sentiment_lyrics_leandoer$Album, Year = c(2018, 2016, 2013, 2013, 2018, 2014, 2018, 2017, 2013, 2014, 2016))
sentiment_by_year_leandoer <- merge(sentiment_by_year_leandoer, sentiment_lyrics_leandoer, by = "Album")

#Handling Migos
#break lyrics into unigrams
broken_lyrics_migos <- migos_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics_migos <- broken_lyrics_migos %>%
  filter(!word %in% custom_stop_words$word)

#Compute average sentiment per album
sentiment_lyrics_migos <- broken_lyrics_migos %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(migos_discography %>% filter(Album == Album) %>% tally())$n)

#Add Years
sentiment_by_year_migos <- data.frame(Album = sentiment_lyrics_migos$Album, Year = c(2016, 2015, 2017, 2018, 2015, 2014, 2014, 2013, 2016, 2015))
sentiment_by_year_migos <- merge(sentiment_by_year_migos, sentiment_lyrics_migos, by = "Album")

#Handling Thug
#break lyrics into unigrams
broken_lyrics_thug <- thug_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics_thug <- broken_lyrics_thug %>%
  filter(!word %in% custom_stop_words$word)

#Compute average sentiment per album
sentiment_lyrics_thug <- broken_lyrics_thug %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(thug_discography %>% filter(Album == Album) %>% tally())$n)

#Add Years
sentiment_by_year_thug <- data.frame(Album = sentiment_lyrics_thug$Album, Year = c(2013, 2014, 2014, 2015, 2017, 2018, 2016, 2016, 2018, 2015, 2015, 2016))
sentiment_by_year_thug <- merge(sentiment_by_year_thug, sentiment_lyrics_thug, by = "Album")

#Handling Post
#break lyrics into unigrams
broken_lyrics_post <- post_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics_post <- broken_lyrics_post %>%
  filter(!word %in% custom_stop_words$word)

#Compute average sentiment per album
sentiment_lyrics_post <- broken_lyrics_post %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(post_discography %>% filter(Album == Album) %>% tally())$n)

#Add Years
sentiment_by_year_post <- data.frame(Album = sentiment_lyrics_post$Album, Year = c(2018, 2016))
sentiment_by_year_post <- merge(sentiment_by_year_post, sentiment_lyrics_post, by = "Album")

#Handling Tekashi
#break lyrics into unigrams
broken_lyrics_tekashi <- tekashi_discography %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)

#remove the stop words
broken_lyrics_tekashi <- broken_lyrics_tekashi %>%
  filter(!word %in% custom_stop_words$word)

#Compute average sentiment per album
sentiment_lyrics_tekashi <- broken_lyrics_tekashi %>%
  inner_join(get_sentiments("bing")) %>% 
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(tekashi_discography %>% filter(Album == Album) %>% tally())$n)

#Add Years
sentiment_by_year_tekashi <- data.frame(Album = sentiment_lyrics_tekashi$Album, Year = c(2018, 2018, 2015))
sentiment_by_year_tekashi <- merge(sentiment_by_year_tekashi, sentiment_lyrics_tekashi, by = "Album")

complete_year_sent <-  do.call("rbind", list(sentiment_by_year_drake, sentiment_by_year_leandoer,
                                             sentiment_by_year_migos, sentiment_by_year_post, 
                                             sentiment_by_year_tekashi, sentiment_by_year_thug, 
                                             sentiment_by_year_yachty))

write.csv(complete_year_sent, "/Users/ericbrigham/Desktop/datamining/project/complete.csv")

#graph per year
ggplot(complete_year_sent, aes(Year, sentiment)) + geom_point(color = "blue")+ geom_point(data=complete_year_sent[7:17, ], aes(Year, sentiment), colour="red")


#Remove Tekashi
complete_year_sent_notekashi <-  do.call("rbind", list(sentiment_by_year_drake, sentiment_by_year_leandoer,
                                             sentiment_by_year_migos, sentiment_by_year_post, 
                                             sentiment_by_year_thug, sentiment_by_year_yachty))

write.csv(complete_year_sent_notekashi, "/Users/ericbrigham/Desktop/datamining/project/completenotekashi.csv")


#graph per year
ggplot(complete_year_sent_notekashi, aes(Year, sentiment)) + geom_point()



