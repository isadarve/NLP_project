#libraries needed
library(dplyr) #data manipulation
library(tidytext) #text mining
library(tm) #text mining
library(textstem) #lemmatization
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(wordcloud) #plot the word cloud
library(wordcloud2) #creative visualizations
library(readr) #read the csv
library(data.table)
library(sentimentr) #emotional analysis

# Read the CSVs with the data
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
ts <- read_csv("Taylor_Swift.csv")
mc <- read_csv("Miley_Cyrus.csv")

# Part 1 of Experiment done in the report
# We change all song lyrics to lowercase
ts$Lyrics <- sapply(ts$Lyrics, tolower)
mc$Lyrics <- sapply(mc$Lyrics, tolower)

# now we transform the data to drop lines that are the same
spl_ts <- strsplit(as.character(ts$Lyrics), "(?<=[.])(?=.)", perl=TRUE)
spl_ts <- stack(setNames(spl_ts, ts$Title))
spl_ts <- aggregate(values ~ ind, data=spl_ts[!duplicated(spl_ts$values),], FUN=paste, collapse=" ")
ts$Lyrics <- spl_ts$values
taylor <- ts

spl_mc <- strsplit(as.character(mc$Lyrics), "(?<=[.])(?=.)", perl=TRUE)
spl_mc <- stack(setNames(spl_mc, mc$Title))
spl_mc <- aggregate(values ~ ind, data=spl_mc[!duplicated(spl_mc$values),], FUN=paste, collapse=" ")
mc$Lyrics <- spl_mc$values
miley <- mc

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("'s", " is", doc)
  doc <- gsub("-", " ", doc) 
  doc <- gsub("'", " ", doc)
  # "hangin'" is "hanging"
  doc <- gsub("n' ", "ng ", doc)
  doc <- gsub( " *\\[.*?\\] *", "", doc)
  return(doc)
}
# expand contractions
taylor$Lyrics <- sapply(taylor$Lyrics, fix.contractions)
miley$Lyrics <- sapply(miley$Lyrics, fix.contractions)

# function to remove special characters, if there are any
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
taylor$Lyrics <- sapply(taylor$Lyrics, removeSpecialChars)
taylor$Single <- sapply(taylor$Single, removeSpecialChars)
miley$Lyrics <- sapply(miley$Lyrics, removeSpecialChars)
miley$Single <- sapply(miley$Single, removeSpecialChars)

# Part 1.1 of Experiment done in the report
# Now we modify the initial dataset to have all the discography: albums and singles
# In taylor_wsingles there are just three columns: Album, Year and the Lyrics of all the album's songs
taylor_wsingles <- aggregate(Lyrics ~ Album+Year, data = taylor, paste, collapse = " ")
miley_wsingles <- aggregate(Lyrics ~ Album+Year, data = miley, paste, collapse = " ")

# We create the corpus
taylor_ws <- VectorSource(taylor_wsingles$Lyrics)
tay_ws <- Corpus(taylor_ws)
miley_ws <- VectorSource(miley_wsingles$Lyrics)
mc_ws <- Corpus(miley_ws)

# We perform lemmatization
tay_ws <- tm_map(tay_ws, lemmatize_strings)
mc_ws <- tm_map(mc_ws, lemmatize_strings)

# Drop the stopwords and other undesirable words
undesirable_words <- c('oh', 'na', 'la', 'ah', 'uh', 'aand', 'yeah', 'wanna', 'ooh', 'gonna', 'shake', 'good', 'sit',
                       'make', 'thing', 'back', 'baby', 'break', 'give', 'show', 'stay', 'amma', 'someday', 'nahh', 
                       'walk', 'hear', 'hey', 'baby', 'pum', 'back', 'nah', 'find', 'stuff', 'thing', 'bangerz', 
                       'round', 'whoa')
mystopwords <- c(undesirable_words, stopwords("SMART"))
tay_ws <- tm_map(tay_ws,removeWords,mystopwords)
mc_ws <- tm_map(mc_ws,removeWords,mystopwords)

# Remove punctuation
tay_ws <- tm_map(tay_ws, removePunctuation)
mc_ws <- tm_map(mc_ws, removePunctuation)

# Drop double spaces
tay_ws <- tm_map(tay_ws,stripWhitespace)
mc_ws <- tm_map(mc_ws,stripWhitespace)

# Part 2.1 of Experiment done in the report
# TermDocumentMatrix: count the number of each word in each of the documents of the corpus (in this case, in each album)
tdm_ws_ts <- TermDocumentMatrix(tay_ws)
tdm_ws_mc <- TermDocumentMatrix(mc_ws)

# Part 3.1 of Experiment done in the report
# Calculate the frequency of each word
freq_ts <- rowSums(as.matrix(tdm_ws_ts))
word_ts <- tail(sort(freq_ts), n = 10)
freq_mc <- rowSums(as.matrix(tdm_ws_mc))
word_mc <- tail(sort(freq_mc), n = 10)

# Part 4.1 of Experiment done in the report
# We plot a wordcloud to see the most used words in the whole discography of the singers
pal <- brewer.pal(9, 'RdPu')
pal <- pal[4:6]
wordcloud(words = names(freq_ts), freq = freq_ts, min.freq = 0, random.order = FALSE, colors = pal)
wordcloud(words = names(freq_mc), freq = freq_mc, min.freq = 0, random.order = FALSE, colors = pal)

# Part 1.2 of Experiment done in the report
# Now we modify the initial dataset to have just the albums to find the most used words in each album
taylor_album <- filter(taylor, taylor$Single=="No ")
taylor_album$Single <- NULL
taylor_album <- aggregate(Lyrics ~ Album+Year, data = taylor_album, paste, collapse = " ")
miley_album <- filter(miley, miley$Single=="No ")
miley_album$Single <- NULL
miley_album <- aggregate(Lyrics ~ Album+Year, data = miley_album, paste, collapse = " ")

# Part 2.2 of Experiment done in the report
# We create the correspondent corpus
taylor_alb <- VectorSource(taylor_album$Lyrics)
tay <- Corpus(taylor_alb)
miley_alb <- VectorSource(miley_album$Lyrics)
mc <- Corpus(miley_alb)

#Lemmatization
tay <- tm_map(tay, lemmatize_strings)
mc <- tm_map(mc, lemmatize_strings)

# Drop the stopwords and other undesirable words
tay <- tm_map(tay,removeWords,mystopwords)
mc <- tm_map(mc,removeWords,mystopwords)

# Drop double spaces
tay <- tm_map(tay,stripWhitespace)
mc <- tm_map(mc,stripWhitespace)

# Remove punctuation
tay <- tm_map(tay, removePunctuation)
mc <- tm_map(mc, removePunctuation)

# Part 3.2 of Experiment done in the report
# We perform the analysis of emotions of each album (singles included)
emotions_ts <- emotion(tay_ws$content)
emotions_ts$element_id <- recode(emotions_ts$element_id, '1'='Taylor Swift', '2'='Fearless', '3'='Safe & Sound', 
                                 '4'='Speak Now', '5'='Today Was a Fairytale', '6'='Red', '7'='Ronan', 
                                 '8'='Sweeter Than Fiction', '9'='1989', '10'='reputation', '11'='Beautiful Ghosts',
                                 '12'='Christmas Tree Farm','13'='Lover', '14'='evermore', '15'='folklore', 
                                 '16'='Only The Young')
emotions_mc <- emotion(mc_ws$content, drop.unused.emotions=TRUE)
emotions_mc$element_id <- recode(emotions_mc$element_id, '1'='Breakout', '2'='The Time Of Our Lives', '3'='Cant Be Tamed',
                                 '4'='23', '5'='Ashtrays and Heartbreaks', '6'='Bangerz', '7'='Nightmare', 
                                 '8'='Pretty Girls (Fun)', '9'='Real and True', '10'='Hand of Love', 
                                 '11'='Miley Cyrus And Her Dead Petz', '12'='Teardrop', '13'='Younger Now', 
                                 '14'='Charlies Angels', '15'='Nothing Breaks Like A Heart', '16'='SHE IS COMING', 
                                 '17'='Slide Away', '18'='Plastic Hearts')


# Part 4.2 of Experiment done in the report
# We plot the results. The albums are in chronological order, and so are the plots
plot(emotions_ts) + ggtitle('Taylor Swift')
plot(emotions_ts, facet = FALSE) + ggtitle('Taylor Swift')

plot(emotions_mc) + ggtitle('Miley Cyrus')
plot(emotions_mc, facet = FALSE) + ggtitle('Miley Cyrus')


# Part 2.3 of Experiment done in the report
# TermDocumentMatrix: count the number of each word in each of the documents of the corpus (in this case, in each album)
tdm_ts <- TermDocumentMatrix(tay)
tdm_mc <- TermDocumentMatrix(mc)


# Part 3.3 and 4.3 of Experiment done in the report
wordcount_by_album_ts <- tidy(tdm_ts)
wordcount_by_album_ts <- wordcount_by_album_ts %>%  group_by(document) %>% top_n(n=3)
wordcount_by_album_ts$document <- recode(wordcount_by_album_ts$document, '1'='Taylor Swift', '2'='Fearless', '3'='Speak Now', 
                                         '4'='Red', '5'='1989', '6'='reputation', '7'='Lover','8'='evermore', '9'='folklore')

wordcount_by_album_mc <- tidy(tdm_mc)
wordcount_by_album_mc <- wordcount_by_album_mc %>%  group_by(document) %>% top_n(n=3)
wordcount_by_album_mc$document <- recode(wordcount_by_album_mc$document, '1'='Breakout', '2'='The Time Of Our Lives', 
                                         '3'='Cant Be Tamed','4'='Bangerz', '5'='Miley Cyrus And Her Dead Petz', 
                                         '6'='Younger Now','7'='SHE IS COMING', '8'='Plastic Hearts')
