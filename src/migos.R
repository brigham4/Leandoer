library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
library(tidytext)

start_url <- 'https://genius.com/albums/Migos/'
other_url <- "https://genius.com/Migos"

migos_scrape <- function(album){
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
      if(l_url == "https://genius.com/Migos-rich-then-famous-lyrics"){
        l_url = 'https://genius.com/Migos-rich-then-famous-intro-lyrics'
      }
      if(l_url == "https://genius.com/Migos-out-the-gym-lyrics"){
        l_url = 'https://genius.com/Migos-out-the-gym-remix-lyrics'
      }
      if(l_url == "https://genius.com/Migos-r-i-p-lyrics"){
        l_url = "https://genius.com/Migos-rip-lyrics"
      }
      if(l_url == "https://genius.com/Migos-thank-you-god-lyrics"){
        l_url = "https://genius.com/Migos-thank-you-god-outro-lyrics"
      }
      if(l_url == "https://genius.com/Migos-we-ready-lyrics"){
        l_url = "https://genius.com/Migos-we-ready-remix-lyrics"
      }
      if(l_url == "https://genius.com/Migos-no-label-2-lyrics"){
        l_url = "https://genius.com/Migos-no-label-2-intro-lyrics"
      }
      if(l_url == "https://genius.com/Migos-m-m-s-lyrics"){
        l_url = "https://genius.com/Migos-m-ms-lyrics"
      }
      if(l_url == "https://genius.com/Migos-can-t-believe-it-lyrics"){
        l_url = "https://genius.com/Migos-cant-believe-it-lyrics"
      }
      if(l_url == "https://genius.com/Migos-ain-t-mine-lyrics"){
        l_url = "https://genius.com/Migos-aint-mine-lyrics"
      }
      if(l_url == "https://genius.com/Migos-what-y-all-doin-lyrics"){
        l_url = "https://genius.com/Migos-what-yall-doin-lyrics"
      }
      if(l_url == "https://genius.com/Migos-protect-my-millions-by-yrn-lingo-lyrics"){
        l_url = "https://genius.com/Yrn-lingo-protect-my-millions-lyrics"        
      }
      if(l_url == "https://genius.com/Migos-quit-playin-by-rich-the-kid-lyrics"){
        l_url = "https://genius.com/Rich-the-kid-quit-playin-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-go-get-em-by-migo-domingo-lyrics'){
        l_url = "https://genius.com/Migo-domingo-go-get-em-lyrics"
      }
      if(l_url == "https://genius.com/Migos-change-by-rich-the-kid-lyrics"){
        l_url = "https://genius.com/Rich-the-kid-quit-playin-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-dealer-by-migo-bands-lyrics'){
        l_url = "https://genius.com/Migo-bands-dealer-lyrics"
      }
      if(l_url == "https://genius.com/Migos-srgyrn-by-johnny-cinco-lyrics"){
        l_url = "https://genius.com/Johnny-cinco-srgyrn-lyrics"
      }
      if(l_url == "https://genius.com/Migos-kick-it-by-rich-the-kid-lyrics"){
        l_url = "https://genius.com/Rich-the-kid-kick-it-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-all-dis-money-by-mango-foo-lyrics'){
        l_url = "https://genius.com/Mango-foo-all-dis-money-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-real-fake-by-yrn-lingo-lyrics'){
        l_url = "https://genius.com/Yrn-lingo-real-and-fake-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-on-go-by-yrn-lingo-lyrics'){
        l_url = "https://genius.com/Yrn-lingo-on-go-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-wtf-wrong-by-mango-foo-lyrics'){
        l_url = "https://genius.com/Mango-foo-wtf-wrong-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-due2dafact-by-skippa-da-flippa-lyrics'){
        l_url = "https://genius.com/Mango-foo-wtf-wrong-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-can-t-talk-to-me-by-mango-foo-lyrics'){
        l_url = "https://genius.com/Mango-foo-cant-talk-to-me-lyrics"
      }
      if(l_url == "https://genius.com/Migos-see-what-i-m-saying-lyrics"){
        l_url = 'https://genius.com/Migos-see-what-im-saying-intro-lyrics'
      }
      if(l_url == 'https://genius.com/Migos-3-way-lyrics'){
        l_url = 'https://genius.com/Migos-3-way-intro-lyrics'
      }
      if(l_url == 'https://genius.com/Migos-can-t-go-out-sad-lyrics'){
        l_url = 'https://genius.com/Migos-cant-go-out-sad-lyrics'
      }
      if(l_url == "https://genius.com/Migos-higher-we-go-lyrics"){
        l_url = "https://genius.com/Migos-higher-we-go-intro-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-bbo-lyrics'){
        l_url = "https://genius.com/Migos-bbo-bad-bitches-only-lyrics"
      }
      if(l_url == "https://genius.com/Migos-motorsport-by-migos-nicki-minaj-cardi-b-lyrics"){
        l_url = "https://genius.com/Migos-nicki-minaj-and-cardi-b-motorsport-lyrics"
      }
      if(l_url == 'https://genius.com/Migos-yrn-2-lyrics'){
        l_url = "https://genius.com/Migos-yrn-2-intro-lyrics"
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

YRN <- migos_scrape("Young-rich-niggas")
NL2 <- migos_scrape("No-label-ii")
RNT <- migos_scrape("Rich-nigga-timeline")
migo_lingo <- migos_scrape("Migo-lingo")
rich_nation <- migos_scrape("Yung-rich-nation")
back_bando <- migos_scrape("Back-to-the-bando")
YRN2 <- migos_scrape("Young-rich-niggas-2")
three_way <- migos_scrape("3-way-ep")
culture <- migos_scrape("Culture")
culture_2 <- migos_scrape("Culture-ii")

migos_discography <-  do.call("rbind", list(YRN, NL2, RNT, migo_lingo, rich_nation, back_bando, YRN2, three_way, culture, culture_2))


write.csv(migos_discography, "/Users/ericbrigham/Desktop/datamining/project/migoslyrics.csv")

