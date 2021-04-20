library(quanteda)
library(SnowballC)
library(ggplot2)
library(tm)
library(wordcloud)


# create corpus
post = Corpus(VectorSource(chat$text)) # creating corpus


post = tm_map(post,content_transformer(tolower)) #lowercase

# remove punctuation without @
removepunct <- function(x) gsub("[^@[:^punct:]]", "", x, perl=T)
post <- tm_map(post, removepunct)

# remove url
removeURL <- function(y) gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", y)
post <- tm_map(post, removeURL)

# remove "media tidak disertakan"
library(tidyverse)
removemedia <- function (z) str_remove_all(z, "media tidak disertakan")
post <- tm_map(post, removemedia)

post = tm_map(post, stripWhitespace) #remove spasi ganda

# convert data to dtm
tdm <- TermDocumentMatrix(post,
                          control = list(wordLengths = c(1, Inf)))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)
df <- data.frame(term = names(term.freq), freq = term.freq)
df <- df %>%
  arrange(desc(freq)) 
n=dim(df)[1]
rownames(df) <- seq(1,n,1)
View(df)

# create wordcloud
library(wordcloud2)
wordcloud2(df, size = 0.7, shape = 'star')

library(devtools)
devtools::install_github("lchiffon/wordcloud2")
wordcloud2(df, figPath = "D:/Project Iseng/WA2.png" ,size = 1,color = "green")
