library(tm)
library(dplyr)
library(NLP)
library(RWeka)
library(tidyr)
library(stringi)
library(stringr)
library(quanteda)
library(data.table)
library(readr)
rm(list = ls(all.names = TRUE))


# ******************************************************************************
# Download and load the training data
# ******************************************************************************

trainURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
trainDataFile <- "data/Coursera-SwiftKey.zip"

if (!file.exists('data')) {
        dir.create('data')
}

if (!file.exists("data/final/en_US")) {
        tempFile <- tempfile()
        download.file(trainURL, tempFile)
        unzip(tempFile, exdir = "data")
        unlink(tempFile)
}

# blogs
blogsFile <- "data/final/en_US/en_US.blogs.txt"
con <- file(blogsFile, open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# news
newsFile <- "data/final/en_US/en_US.news.txt"
con <- file(newsFile, open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# twitter
twitterFile <- "data/final/en_US/en_US.twitter.txt"
con <- file(twitterFile, open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

rm(con)

# ******************************************************************************
# Prepare the data
# ******************************************************************************
set.seed(12345)
sampleSize = 0.01

#take a sample of each data set
sampleBlogs <- sample(blogs, length(blogs) * sampleSize, replace = FALSE)
sampleNews <- sample(news, length(news) * sampleSize, replace = FALSE)
sampleTwitter <- sample(twitter, length(twitter) * sampleSize, replace = FALSE)

# remove all non-English characters
sampleBlogs <- iconv(sampleBlogs, "latin1", "ASCII", sub = "")
sampleNews <- iconv(sampleNews, "latin1", "ASCII", sub = "")
sampleTwitter <- iconv(sampleTwitter, "latin1", "ASCII", sub = "")

# combine the data sets
sampleData <- c(sampleBlogs, sampleNews, sampleTwitter)
sampleDataFileName <- "data/final/en_US/en_US.sample.txt"
con <- file(sampleDataFileName, open = "w")
writeLines(sampleData, con)
close(con)

rm(blogs, news, twitter, sampleBlogs, sampleNews, sampleTwitter)

# ******************************************************************************
# Build the corpus
# ******************************************************************************

library(tm)

buildCorpus <- function (dataSet) {
        docs <- VCorpus(VectorSource(dataSet))
        toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
        
        # remove URL, Twitter handles, email patterns, punctuation, etc
        docs <- tm_map(docs, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
        docs <- tm_map(docs, toSpace, "@[^\\s]+")
        docs <- tm_map(docs, toSpace, "\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b")
        
        docs <- tm_map(docs, tolower)
        docs <- tm_map(docs, removeWords, stopwords("english"))
        docs <- tm_map(docs, removePunctuation)
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, stripWhitespace)
        docs <- tm_map(docs, PlainTextDocument)
        return(docs)
}

# build the corpus and write to disk (RDS)
corpus <- buildCorpus(sampleData)

# Save the final corpus
saveRDS(corpus, file = "./final_corpus.RData")
rm(corpus)

# Build the n-grams
final_corpus <- readRDS("./final_corpus.RData")
final_corpus_DF <-data.frame(text=unlist(sapply(final_corpus,`[`, "content")), 
                             stringsAsFactors = FALSE)

# Build the tokenization function for the n-grams
ngramTokenizer <- function(the_corpus, ngramCount) {
        ngramFunction <- NGramTokenizer(the_corpus, 
                                        Weka_control(min = ngramCount, max = ngramCount, 
                                                     delimiters = " \\r\\n\\t.,;:\"()?!"))
        ngramFunction <- data.frame(table(ngramFunction))
        ngramFunction <- ngramFunction[order(ngramFunction$Freq, 
                                             decreasing = TRUE),]
        colnames(ngramFunction) <- c("String","Count")
        ngramFunction
}

unigram <- ngramTokenizer(final_corpus_DF, 1)
saveRDS(unigram, file = "./unigram.RData")
bigram <- ngramTokenizer(final_corpus_DF, 2)
bigram <- bigram[bigram$Count>1,]
bigram <- bigram %>% separate(String, c("unigram","bigram"), " ", remove=TRUE)
names(bigram)[3] <- "frequency"
saveRDS(bigram, file = "./bigram.RData")
trigram <- ngramTokenizer(final_corpus_DF, 3)
trigram <- trigram[trigram$Count>1,]
trigram <- trigram %>% separate(String, c("unigram","bigram","trigram"), " ", remove=TRUE)
names(trigram)[4] <- "frequency"
saveRDS(trigram, file = "./trigram.RData")
quadgram <- ngramTokenizer(final_corpus_DF, 4)
quadgram <- quadgram[quadgram$Count>1,]
quadgram <- quadgram %>% separate(String, c("unigram","bigram","trigram","quadgram"), " ", remove=TRUE)
names(quadgram)[5] <- "frequency"
saveRDS(quadgram, file = "./quadgram.RData")