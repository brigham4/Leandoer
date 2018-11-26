library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
library(tidytext)

start_url <- 'https://genius.com/albums/Lil-yachty/'
other_url <- "https://genius.com/Lil-yachty"

yachty_scrape <- function(album){
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
      if(l_url == 'https://genius.com/Lil-yachty-i-don-t-fuck-with-niggas-lyrics'){
        l_url = "https://genius.com/Lil-yachty-i-dont-fuck-with-niggas-lyrics"
      }
      if(l_url == 'https://genius.com/Lil-yachty-flexin-on-my-ex-s-lyrics'){
        l_url = 'https://genius.com/Lil-yachty-flexin-on-my-exs-lyrics'        
      }
      if(l_url == 'https://genius.com/Lil-yachty-hella-o-s-lyrics'){
        l_url = 'https://genius.com/Lil-yachty-hella-os-lyrics'
      }
      if(l_url == 'https://genius.com/Lil-yachty-i-m-sorry-lyrics'){
        l_url = 'https://genius.com/Lil-yachty-im-sorry-lyrics'
      }
      if(l_url == 'https://genius.com/Lil-yachty-why-lyrics'){
        l_url = "https://genius.com/Lil-yachty-why-interlude-lyrics"
      }
      if(l_url == 'https://genius.com/Lil-yachty-battle-of-the-bands-lyrics'){
        l_url = "https://genius.com/Lil-yachty-battle-of-the-bands-im-back-lyrics"
      }
      if(l_url == 'https://genius.com/Lil-yachty-how-d-it-happen-lyrics'){
        l_url = "https://genius.com/Lil-yachty-howd-it-happen-lyrics"
      }
      if(l_url == 'https://genius.com/Lil-yachty-otha-shit-lyrics'){
        l_url = "https://genius.com/Lil-yachty-otha-shit-interlude-lyrics"
      }
      if(l_url == 'https://genius.com/Lil-yachty-fyi-lyrics'){
        l_url = "https://genius.com/Lil-yachty-fyi-know-now-lyrics"
      }
      if(l_url == "https://genius.com/Lil-yachty-momma-lyrics"){
        l_url = "https://genius.com/Lil-yachty-momma-outro-lyrics"
      }
      if(l_url == 'https://genius.com/Lil-yachty-dinner-s-ready-lyrics'){
        l_url = 'https://genius.com/Lil-yachty-dinners-ready-lyrics'
      }
      if(l_url == 'https://genius.com/Lil-yachty-still-don-t-lyrics'){
        l_url = 'https://genius.com/Lil-yachty-still-dont-lyrics'
      }      
      if(l_url == 'https://genius.com/Lil-yachty-perplexing-pegasus-lyrics'){
        l_url = 'https://genius.com/Lil-yachty-perplexing-pegasus-freestyle-lyrics'
      }
      if(l_url == 'https://genius.com/Lil-yachty-die-by-myself-3-by-lil-yachty-k-upreme-lyrics'){
        l_url = "https://genius.com/Lil-yachty-and-k-upreme-die-by-myself-3-annotated"
      }
      if(l_url == 'https://genius.com/Lil-yachty-i-m-the-mac-lyrics'){
        l_url ='https://genius.com/Lil-yachty-im-the-mac-lyrics'
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
summer_song <- yachty_scrape("Summer-songs-ep")
lil_boat <- yachty_scrape("Lil-boat")
summer_songs_2 <- yachty_scrape("Summer-songs-2")
lbbdaymix <- yachty_scrape("Lil-boat-s-birthday-mix")
teenage_emotions <- yachty_scrape("Teenage-emotions")
bday_mix_2 <- yachty_scrape("Birthday-mix-2-0-boat")
lil_boat_2 <- yachty_scrape("Lil-boat-2")
bday_mix_3 <- yachty_scrape("Birthday-mix-3")
nuthin_2_prove <- yachty_scrape('Nuthin-2-prove')

yachty_discography <-  do.call("rbind", list(summer_song, lil_boat, summer_songs_2,
                                             lbbdaymix, teenage_emotions, bday_mix_2,
                                             lil_boat_2, bday_mix_3, nuthin_2_prove))


write.csv(yachty_discography, "/Users/ericbrigham/Desktop/datamining/project/yachtylyrics.csv")

