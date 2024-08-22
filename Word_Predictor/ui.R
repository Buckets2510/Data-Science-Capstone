#
# Author: Brandon Robinson
"`r Sys.Date()`"
# Description: Shiny UI App, Coursera Capstone Project - Word Predictor
# Github: 

library(shiny)
library(shinythemes)
library(markdown)
library(dplyr)
library(tm)

shinyUI(fluidPage(
        titlePanel("Word Prediction"),
        h4("Coursera Capstone Project by Brandon Robinson"),
        h5("Word Prediction is a Shiny app that uses a text
           prediction algorithm to predict the next word
           based on user entry."),
        
        sidebarLayout(
                sidebarPanel(
                        textInput("text", label = h3("Enter Text:"), value = "What"),
                        helpText("Type in a sentence above and the results will display to the right."),
                        hr()
                ),
                mainPanel(
                        br(),
                        h2(textOutput("entered_words"), align="center"),
                        h1(textOutput("predicted_word"), align="center", style="color:red")
                )
        )
))
