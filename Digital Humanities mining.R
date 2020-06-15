
###### Präambel ####
require("RCurl")
require("XML")
#install.packages("corpora", repos="http://R-Forge.R-project.org")
#install.packages("tm")
#install.packages("tmap")
library(tm)
library(tmap)
library(corpora)

#Example from https://quantmacro.wordpress.com/2016/04/30/web-scraping-for-text-mining-in-r/

##### Blaupause ####
.request <- getURL("URL", ssl.verifypeer = FALSE)

class(.request)
is.vector(.request)

.tree <- htmlTreeParse(.request, useInternal = T)


.tree.parse <- unlist(xpathApply(.tree, path = "//p", fun = xmlValue))

.txt <- NULL 
for (i in 2: (length(.tree.parse)-1)){
  .txt <- paste(.txt, as.character(.tree.parse[i]), sep = ' ')
}

is.vector(.txt)
length(.txt)

print(.txt)

.txt <- gsub("\r?\n|\r", "", .txt)
my.corpus <- Corpus(VectorSource(.txt))

strwrap(my.corpus[[1]])


##### text mining, removing uninteresting information ####

my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus <- tm_map(my.corpus, removeWords, stopwords('english'))
my.corpus <- tm_map(my.corpus, PlainTextDocument)


##### Matrix erstellen um Wörter einzeln zu betrachten ####

tdm <- TermDocumentMatrix(my.corpus)

#### visualisierung ####
library(wordcloud)
library(RColorBrewer)

my.word.matrix <- as.matrix(tdm)


#colnames(my.word.matrix) <- 'Karneval Köln 18'
v <- sort(rowSums(my.word.matrix), decreasing = T)
d <- data.frame(word=names(v), freq=v)
par(bg="black")
wordcloud(d$word, d$freq, random.order = FALSE, max.words = 300, color="violetred")





##### Spiegel Versuch #### 


spon.request <- getURL("http://www.let.rug.nl/usa/presidents/john-fitzgerald-kennedy/ich-bin-ein-berliner-speech-1963.php", ssl.verifypeer = FALSE)

class(spon.request)
is.vector(spon.request)

spon.tree <- htmlTreeParse(spon.request, useInternal = T)
print(spon.tree)


spon.tree.parse <- unlist(xpathApply(spon.tree, path = "//p", fun = xmlValue))
class(spon.tree.parse)
length(spon.tree.parse)
spon.tree.parse


spon.txt <- NULL 
for (i in 2: (length(spon.tree.parse)-1)){
  spon.txt <- paste(spon.txt, as.character(spon.tree.parse[i]), sep = ' ')
}

is.vector(spon.txt)
length(spon.txt)

print(spon.txt)

spon.txt <- gsub("\r?\n|\r", "", spon.txt)
my.corpus <- Corpus(VectorSource(spon.txt))

strwrap(my.corpus[[1]])


##### text mining, removing uninteresting information ####

my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, stripWhitespace)
#words_to_remove <- c("2018", "adblocker", "aktiviert")
my.corpus <- tm_map(my.corpus, removeWords, stopwords('english'))
my.corpus <- tm_map(my.corpus, PlainTextDocument)

strwrap(my.corpus[[1]])

##### Matrix erstellen um Wörter einzeln zu betrachten ####

tdm <- TermDocumentMatrix(my.corpus)
inspect(tdm[1:10,])



#### visualisierung ####
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
help("RColorBrewer")
display.brewer.pal(10, "Accent")
colors()

my.word.matrix <- as.matrix(tdm)

d

#colnames(my.word.matrix) <- 'Karneval Köln 18'
v <- sort(rowSums(my.word.matrix), decreasing = T)
d <- data.frame(word=names(v), freq=v)
par(bg="black")
wordcloud(d$word, d$freq, random.order = FALSE, max.words = 300, color="violetred")



##### 2. Eigene Datenaggregierung Songtexte ####

cool95.request <- getURL("http://www.metrolyrics.com/gangstas-paradise-lyrics-coolio.html")
cool95.tree <- htmlTreeParse(cool95.request, useInternal = T)


cool95.tree.parse <- unlist(xpathApply(cool95.tree, path = "//p", fun = xmlValue))



cool95.txt <- NULL 

for (i in 2:8){
  cool95.txt <- paste(cool95.txt, as.character(cool95.tree.parse[i]), sep = ' ')
}



cool95.txt <- gsub("\r?\n|\r", " ", cool95.txt)
my.corpus <- Corpus(VectorSource(cool95.txt))

strwrap(my.corpus[[1]])

####visualising 
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus <- tm_map(my.corpus, removeWords, stopwords('english'))
my.corpus <- tm_map(my.corpus, PlainTextDocument)

strwrap(my.corpus[[1]])

##### Matrix erstellen um Wörter einzeln zu betrachten ####
tdm <- TermDocumentMatrix(my.corpus)
my.word.matrix <- as.matrix(tdm)

v95 <- sort(rowSums(my.word.matrix), decreasing = T)
d95 <- data.frame(word=names(v95), freq=v95)
par(bg="black")
wordcloud(d95$word, d95$freq, random.order = FALSE, max.words = 300, color="salmon")

colors()


#### Versuch ob Blaupause funktioniert ####

creep.request <- getURL("http://www.metrolyrics.com/creep-lyrics-tlc.html", ssl.verifypeer = FALSE)


creep.tree <- htmlTreeParse(creep.request, useInternal = T)


creep.tree.parse <- unlist(xpathApply(creep.tree, path = "//p", fun = xmlValue))

creep.txt <- NULL
# 3 Mal einsetzen
for (i in 2: (length(creep.tree.parse)-1)){
  creep.txt <- paste(creep.txt, as.character(creep.tree.parse[i]), sep = ' ')
}

creep.txt <- gsub("\r?\n|\r", "", creep.txt)
my.corpus <- Corpus(VectorSource(creep.txt))

strwrap(my.corpus[[1]])


##### text mining, removing uninteresting information ####

my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus <- tm_map(my.corpus, removeWords, stopwords('english'))
my.corpus <- tm_map(my.corpus, PlainTextDocument)


##### Matrix erstellen um Wörter einzeln zu betrachten ####

tdm <- TermDocumentMatrix(my.corpus)

#### visualisierung ####
library(wordcloud)
library(RColorBrewer)

my.word.matrix <- as.matrix(tdm)


#colnames(my.word.matrix) <- 'Karneval Köln 18'
v <- sort(rowSums(my.word.matrix), decreasing = T)
d <- data.frame(word=names(v), freq=v)
par(bg="black")
wordcloud(d$word, d$freq, random.order = FALSE, max.words = 300, color="violetred")


###### Beginn großer Analyse ####
library(XML)
library(RCurl)
library(stringr)
allthesongs <- data.frame()

genres <- c("pop", "rock", "latin", "country", "r-b-hip-hop") #auf unterschiedliche Länge achten

for (i in genres){
URL <- paste("https://www.billboard.com/charts/",genres,"-songs/2010-01-01", sep = "")
}

testresults <- htmlTreeParse(getURL("https://www.billboard.com/charts/pop-songs/2010-01-01", followlocation=T), useInternal=T) 
testresults.tree <- unlist(xpathSApply(testresults, "//article//div[@class='chart-row__title']", xmlValue))
clean_testresults.tree <-  gsub('\n', " ", testresults.tree)
split_testresults <- str_split_fixed(testresults.tree, '\n\n', 2)


results <- htmlTreeParse(getURL(URL, followlocation=T), useInternal=T)
billboard.tree <- unlist(xpathSApply(results, "//article//div[@class='chart-row__title']", xmlValue))
split_billboard.tree <- str_split_fixed(billboard.tree, '\n\n', 2)
billboard_2 <- as.data.frame(cbind(split_billboard.tree[1:240, ]), stringsAsFactors=FALSE)

colnames(billboard_2) <- c( "Song", "Artist")

billboard_2$Song <- gsub('\"', "", billboard_2$Song)
billboard_2$Song <- gsub('\n', "", billboard_2$Song)
billboard_2$Song <- tolower(gsub("[^[:alnum:] ]", "", billboard_2$Song))
billboard_2$Song <- gsub("\\'", "", iconv(billboard_2$Song, to='ASCII//TRANSLIT')) # fix special accent chars



billboard_2$Artist <- tolower(gsub("[^[:alnum:] ]", "", billboard_2$Artist))
billboard_2$Artist <- gsub("'e", "e", iconv(billboard_2$Artist, to='ASCII//TRANSLIT')) # fix special accent chars
billboard_2$Artist<- gsub("'o", "o", billboard_2$Artist)
#article beschreibt jeweils die Einträge


### Versuch Test


#creating Id variable
id <- rownames(billboard_2)
billboard_2 <- cbind(id=id, billboard_2)

#Genre nach ID hinzufügen. Gibt sicher einen eleganteren Weg, aber so sollte es auch gehen
billboard_2 <- billboard_2 %>%
  mutate(genre = 
           ifelse(billboard_2$id %in% 1:40, "Pop", 
                  ifelse(billboard_2$id %in% 41:90, "Rock", 
                         ifelse(billboard_2$id %in% 91:140, "Latin", 
                                ifelse(billboard_2$id %in% 141:190, "Country", 
                                       ifelse(billboard_2$id %in% 191:240, "Hip-Hop", 
                                              "NA"))))))
#creating new variables

billboard_2$Lyrics <- ""  
billboard_2$Source <- ""

##### Scraping Lyrics #### 


### source: multiple. 1=metorlyics.com, 3=songlyrics.com, 5=lyricsmode.com
for (s in 1:length(billboard_2$Song))  {
  
  lyrics <- "Not set yet."
  results <- 12 # arbitrary number
  
  # clean up the artist field to fit in the URL
  artist <- strsplit(billboard_2$Artist, " featuring | feat | feat. | with | duet | and ")
  artist <- unlist(artist)[1]
  artist2 <- gsub("the ", "", artist)
  firstletter <- substring(artist2, 1, 1)
  
  # make URLs
  metroURL <- paste("http://metrolyrics.com/",billboard_2$Song[s],"-lyrics-",artist2,".html",sep="")
  songURL <- paste("http://songlyrics.com/",artist2,"/",billboard_2$Song[s],"-lyrics",sep="")
  modeURL <- paste("http://www.lyricsmode.com/lyrics/", firstletter, "/", artist2, "/", billboard_2$Song[s], ".html", sep="")

  help("unlist")
  
  URLs <- c(metroURL, songURL, modeURL)
  
  lyriclocs <- c("//div[@id='lyrics-body-text']", 
                 "//p[@id='songLyricsDiv']", 
                 "//p[@id='lyrics_text']")
  for (b in 1:length(URLs)) {
    billboard_2$Lyrics[s] <- "Not set yet."
    
    results <- 12 # arbitrary number
    
    if(b!=3) URL <- tolower(gsub(" ", "-", URLs[b]))
    if(b==3) URL <- URLs[b]
    
    tryCatch({ 
      results <- htmlTreeParse(URL, useInternal=TRUE, isURL=TRUE)
      lyrics <- xpathSApply(results, lyriclocs[b], xmlValue) },
      error = function(x) { 
        message(paste(s, "failed")) },
      finally={ 
        if (!is.numeric(results)) { 
          if (length(lyrics)!=0) { 
            billboard_2$Lyrics[s] <- lyrics[[1]]
            message(paste(s, "success"))
            billboard_2$Source[s] <- b
            break
          }
        } 
      }) # end tryCatch
  } # end URL for
}



######## Walker #####

allthesongs <- data.frame() 
for (i in 1965:2015) { 
  # create the URL for each year
  URL <- paste("http://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_",i,sep="")
  # parse the HTML
  results <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
  billboard_text <- xpathSApply(results, "//table[@class='wikitable sortable']//tr",xmlValue)
  split_billboard_text <- str_split_fixed(billboard_text,"\n",3) 
  billboard <- as.data.frame(cbind(split_billboard_text[2:101, ], rep(i,100)), stringsAsFactors=FALSE)
  # row bind this year's data to all the data
  allthesongs <- rbind(allthesongs, billboard) 
  
}
colnames(allthesongs) <- c("Rank", "Song", "Artist", "Year")
allthesongs$Song <- gsub('\\"', "", allthesongs$Song)
allthesongs$Song <- tolower(gsub("[^[:alnum:] ]", "", allthesongs$Song))
allthesongs$Song <- gsub("\\'", "", iconv(allthesongs$Song, to='ASCII//TRANSLIT')) # fix special accent chars

allthesongs$Artist <- tolower(gsub("[^[:alnum:] ]", "", allthesongs$Artist))
allthesongs$Artist <- gsub("'e", "e", iconv(allthesongs$Artist, to='ASCII//TRANSLIT')) # fix special accent chars
allthesongs$Artist<- gsub("'o", "o", allthesongs$Artist)

# new variables
allthesongs$Lyrics <- ""
allthesongs$Source <- ""

################### SCRAPE THE LYRICS ###################
### source: multiple. 1=metorlyics.com, 3=songlyrics.com, 5=lyricsmode.com
for (s in 1:length(allthesongs$Song))  {
  
  lyrics <- "Not set yet."
  results <- 12 # arbitrary number
  
  # clean up the artist field to fit in the URL
  artist <- strsplit(allthesongs$Artist, " featuring | feat | feat. | with | duet | and ")
  artist <- unlist(artist)[[1]]
  artist2 <- gsub("the ", "", artist)
  firstletter <- substring(artist2, 1, 1)
  
  # make URLs
  metroURL <- paste("http://metrolyrics.com/",allthesongs$Song[s],"-lyrics-",artist2,".html",sep="")
  songURL <- paste("http://songlyrics.com/",artist2,"/",allthesongs$Song[s],"-lyrics",sep="")
  modeURL <- paste("http://www.lyricsmode.com/lyrics/", firstletter, "/", artist2, "/", allthesongs$Song[s], ".html", sep="")
  
  
  URLs <- c(metroURL, songURL, modeURL)
  
  lyriclocs <- c("//div[@id='lyrics-body-text']", 
                 "//p[@id='songLyricsDiv']", 
                 "//p[@id='lyrics_text']")
  
  for (b in 1:length(URLs)) {
    allthesongs$Lyrics[s] <- "Not set yet."
    
    results <- 12 # arbitrary number
    
    if(b!=3) URL <- tolower(gsub(" ", "-", URLs[b]))
    if(b==3) URL <- URLs[b]
    
    tryCatch({ 
      results <- htmlTreeParse(URL, useInternal=TRUE, isURL=TRUE)
      lyrics <- xpathSApply(results, lyriclocs[b], xmlValue) },
      error = function(x) { 
        message(paste(s, "failed")) },
      finally={ 
        if (!is.numeric(results)) { 
          if (length(lyrics)!=0) { 
            allthesongs$Lyrics[s] <- lyrics[[1]]
            message(paste(s, "success"))
            allthesongs$Source[s] <- b
            break
          }
        } 
      }) # end tryCatch
  } # end URL for
} # end for

allthesongs$Lyrics <- gsub("\\\n|\\\t"," ",allthesongs$Lyrics)
allthesongs$Lyrics <- tolower(gsub("[^[:alnum:] ]", "", allthesongs$Lyrics))
missing <- round(length(allthesongs[allthesongs$Lyrics=="not set yet", 1])/length(allthesongs[,1]), 4)*100
## 3.67% of lyrics are missing
allthesongs$Lyrics <- gsub("not set yet", "NA", allthesongs$Lyrics)
allthesongs$Lyrics <- gsub("we are not in a position to display these lyrics due to licensing restrictions sorry for the inconvenience", "NA", allthesongs$Lyrics)

# setwd("/Users/kaylinwalker/R/kw_musiclyrics")
# write.csv(allthesongs, "billboard_lyrics_1964-2015.csv", row.names=FALSE)
# allthesongs <- read.csv("billboard_lyrics_1964-2015.csv", stringsAsFactors=FALSE)