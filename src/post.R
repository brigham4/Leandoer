library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
library(tidytext)

start_url <- 'https://genius.com/albums/Post-malone/'
other_url <- "https://genius.com/Post-malone"

post_scrape <- function(album){
  song_titles <- list()
  album_titles <- list()
  lyrics <- list()
  html_search_terms <- unlist(c(album))
  
  for(h in html_search_terms){
    album_url <- url(paste(start_url, h, sep = ''), 'rb')
    album_webpage <- read_html(album_url)
    
    song_title_html <- html_nodes(album_webpage, '.chart_row-content-title')
    s <- gsub("\\s*\\([^\\)]+\\)","", html_text(song_title_html))
    s <- str_replace(gsub("[^[:alnum:] ]", " ",s), " Lyrics", "")
    s <- tolower(gsub("\\s+", " ", s))
    
    filter_s <- tolower(str_replace_all(h, '-', ' '))
    
    s <- s[!(grepl(filter_s, s))]
    
    #s <- Filter(function(x) {!(grepl(filter_s, x))}, s))
    
    print(s)
    
    
    song_titles <- unlist(c(song_titles, s))
    
    album_title_html <- html_nodes(album_webpage, '.header_with_cover_art-primary_info-title--white')
    
    s <- str_replace_all(s, " ", "-")
    l <- paste(other_url, paste(s, "lyrics", sep = ""), sep = "")
    
    for(l_url in l){
      if(l_url == 'https://genius.com/Post-malone-rich-sad-lyrics'){
        l_url = 'https://genius.com/Post-malone-rich-and-sad-lyrics'
      }
      if(l_url == 'https://genius.com/Post-malone-jonestown-lyrics'){
        l_url = 'https://genius.com/Post-malone-jonestown-interlude-lyrics'
      }
      album_titles <- unlist(c(album_titles, html_text(album_title_html)))
      lyrics_url <- url(l_url, 'rb')
      lyrics_webpage <- read_html(lyrics_url)
      lyrics_html <- html_nodes(lyrics_webpage, '.lyrics p')
      xml_find_all(lyrics_html, ".//br") %>% xml_add_sibling("p", "\n")
      xml_find_all(lyrics_html, ".//br") %>% xml_remove()
      removen <- str_replace_all(html_text(lyrics_html), "\n", " ")
      removen <- gsub("\\[.*?\\]", "", removen)
      removen <- gsub("\\s+", " ", removen)
      lyrics <- unlist(c(lyrics, removen))
      #print(l_url)
      #print(html_text(lyrics_html))
    }
  }
  
  return(data.frame(Album = album_titles, Song = song_titles, Lyrics = lyrics))
  
}
#name <- function_name("url")

stoney <- post_scrape("Stoney")
bbab <- post_scrape("Beerbongs-bentleys")


post_discography <-  do.call("rbind", list(stoney, bbab))


write.csv(post_discography, "/Users/ericbrigham/Desktop/datamining/project/postlyrics.csv")

