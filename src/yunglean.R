library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
library(tidytext)

start_url <- 'https://genius.com/albums/Yung-lean/'
other_url <- "https://genius.com/Yung-lean"

yung_lean_scrape <- function(album){
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
    if(h == "Unknown-memory" || h == "Unknown-death-2002" || h == "Warlord" || h == "Frost-god"){
      s <- s[-length(s)]
      print(s)
    }
    song_titles <- unlist(c(song_titles, s))
    
    album_title_html <- html_nodes(album_webpage, '.header_with_cover_art-primary_info-title--white')
    
    s <- str_replace_all(s, " ", "-")
    l <- paste(other_url, paste(s, "lyrics", sep = ""), sep = "")

    for(l_url in l){
      if(l_url == "https://genius.com/Yung-lean-hennessy-sailor-moon-lyrics"){
        l_url = "https://genius.com/Yung-lean-hennessy-and-sailor-moon-lyrics"
      }
      if(l_url == "https://genius.com/Yung-lean-af1-s-lyrics"){
        l_url = "https://genius.com/Yung-lean-af1s-air-force-1s-lyrics"
      }
      if(l_url == "https://genius.com/Yung-lean-blommor-lyrics"){
        l_url = "https://genius.com/Yung-lean-blommor-intro-lyrics"
      }
      if(l_url == "https://genius.com/Yung-lean-dog-walk-lyrics"){
        l_url = "https://genius.com/Yung-lean-dog-walk-intermission-lyrics"
      }
      if(l_url == "https://genius.com/Yung-lean-don-t-go-lyrics"){
        l_url = "https://genius.com/Yung-lean-dont-go-lyrics"
      }
      if(l_url == "https://genius.com/Yung-lean-helt-ensam-lyrics"){
        l_url = "https://genius.com/Yung-lean-helt-ensam-outro-lyrics"
      }
      if(l_url == "https://genius.com/Yung-lean-spider-feet-by-suicideyear-lyrics"){
        l_url = "https://genius.com/Suicideyear-spider-feet-lyrics"
      }
      if(l_url == 'https://genius.com/Yung-lean-bitch-named-bitch-by-denzel-curry-lyrics'){
        l_url = "https://genius.com/Denzel-curry-bitch-named-bitch-lyrics"
      }
      if(l_url == "https://genius.com/Yung-lean-pixelatedtears-by-bones-lyrics"){
        l_url = "https://genius.com/Bones-pixelatedtears-lyrics"
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

poison_ivy <- yung_lean_scrape("Poison-ivy")
crash_bandicoot <- yung_lean_scrape("Crash-bandicoot-ghostface-shyguy")
stranger <- yung_lean_scrape("Stranger")
warlord <- yung_lean_scrape("Warlord")
frost_god <- yung_lean_scrape("Frost-god")
sadness <- yung_lean_scrape("Profound-sadness-2004")
unknown_memory <- yung_lean_scrape("Unknown-memory")
neal_yung <- yung_lean_scrape("Neal-yung-2003")
lavender <- yung_lean_scrape("Lavender-ep")
unknown_death <- yung_lean_scrape("Unknown-death-2002")
spider_feet <- yung_lean_scrape("Spider-feet-single")

discography <- do.call("rbind", list(poison_ivy, crash_bandicoot, stranger, warlord, 
                                     frost_god, sadness, unknown_memory, neal_yung,
                                     lavender, unknown_death, spider_feet))


write.csv(discography, "/Users/ericbrigham/Desktop/datamining/project/leandoerlyrics.csv")
