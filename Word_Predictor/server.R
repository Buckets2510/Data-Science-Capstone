#
# Author: Brandon Robinson
# Description: Shiny UI App, Coursera Capstone Project - Word Predictor
# Github: 

# load the n-grams (generated by "n-gram_function.R")
source("./clean-token.R")
bigrams <- readRDS(file="./bigram.RData")
trigrams <- readRDS(file="./trigram.RData")
quadgrams <- readRDS(file="./quadgram.RData")

shinyServer(function(input, output) {
        wordPrediction <- reactive({
                text <- input$text
                textInput <- cleanInput(text)
                wordCount <- length(textInput)
                wordPrediction <- nextWordPrediction(wordCount,textInput)
        })
        
        output$predicted_word <- renderPrint(wordPrediction())
        output$entered_words <- renderText({ input$text }, quoted = FALSE)
})