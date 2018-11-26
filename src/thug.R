library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
library(tidytext)

start_url <- 'https://genius.com/albums/Young-thug/'
other_url <- "https://genius.com/Young-thug"

thug_scrape <- function(album){
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
      if(l_url == 'https://genius.com/Young-thug-yeah-yeah-og-boo-dirty-lyrics'){
        l_url = "https://genius.com/Young-thug-yeah-yeah-1017-thug-lyrics"
      }
      if(l_url == 'https://genius.com/Young-thug-i-m-fo-real-lyrics'){
        l_url = "https://genius.com/Young-thug-im-fo-real-lyrics"
      }
      if(l_url == 'https://genius.com/Young-thug-i-ain-t-drunk-yet-lyrics'){
        l_url = 'https://genius.com/Young-thug-i-aint-drunk-yet-lyrics'
      }
      if(l_url == 'https://genius.com/Young-thug-can-t-tell-lyrics'){
        l_url = 'https://genius.com/Young-thug-cant-tell-lyrics'
      }
      if(l_url == 'https://genius.com/Young-thug-take-kare-by-rich-gang-lyrics'){
        l_url = "https://genius.com/Rich-gang-take-kare-lyrics"
      }
      if(l_url == 'https://genius.com/Young-thug-that-s-all-lyrics'){
        l_url = "https://genius.com/Young-thug-thats-all-lyrics"
      }
      if(l_url == 'https://genius.com/Young-thug-big-racks-lyrics'){
        l_url = "https://genius.com/Young-thug-big-racks-intro-lyrics"
      }
      if(l_url == "https://genius.com/Young-thug-don-t-know-lyrics"){
        l_url = "https://genius.com/Young-thug-dont-know-lyrics"
      }
      if(l_url == "https://genius.com/Young-thug-i-ll-tell-you-what-lyrics"){
        l_url = 'https://genius.com/Young-thug-ill-tell-you-what-lyrics'
      }
      if(l_url == "https://genius.com/Young-thug-bout-time-lyrics"){
        l_url = "https://genius.com/Young-thug-bout-damn-time-lyrics"
      }
      if(l_url == "https://genius.com/Young-thug-pick-up-the-phone-by-young-thug-travis-scott-lyrics"){
        l_url = "https://genius.com/Young-thug-and-travis-scott-pick-up-the-phone-lyrics"
      }
      if(l_url == "https://genius.com/Young-thug-family-don-t-matter-lyrics"){
        l_url = 'https://genius.com/Young-thug-family-dont-matter-lyrics'
      }
      if(l_url == 'https://genius.com/Young-thug-daddy-s-birthday-lyrics'){
        l_url = 'https://genius.com/Young-thug-daddys-birthday-lyrics'
      }
      if(l_url == 'https://genius.com/Young-thug-for-y-all-lyrics'){
        l_url = 'https://genius.com/Young-thug-for-yall-lyrics'
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

thug_1017 <- thug_scrape("1017-thug")
thug2_1017 <- thug_scrape("1017-thug-2")
thug3_1017 <- thug_scrape("1017-thug-3-the-finale")
barter6 <- thug_scrape("Barter-6")
slime_season <- thug_scrape("Slime-season")
slime_season2 <- thug_scrape("Slime-season-2")
im_up <- thug_scrape("I-m-up")
slime_season3 <- thug_scrape("Slime-season-3")
jeffery <- thug_scrape("Jeffery")
beautiful_thugger_girls <- thug_scrape("Beautiful-thugger-girls")
hear_no_evil <- thug_scrape("Hear-no-evil")
on_the_rvn <- thug_scrape("On-the-rvn")

thug_discography <-  do.call("rbind", list(thug_1017, thug2_1017, thug3_1017, barter6,
                                           slime_season, slime_season2, im_up, slime_season3,
                                           jeffery, beautiful_thugger_girls, hear_no_evil, on_the_rvn))


write.csv(thug_discography, "/Users/ericbrigham/Desktop/datamining/project/thuglyrics.csv")

