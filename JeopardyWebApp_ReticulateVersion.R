# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages(c("shiny", "dplyr", "shinythemes"))
}

if (!requireNamespace("shinycssloaders", quietly = TRUE)) {
  install.packages("shinycssloaders")
}

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}

# Load libraries
library(shiny)
library(dplyr)
library(shinythemes)
library(textclean)
library(tm)
library(stringr)
library(textstem)
library(shinycssloaders)
library(shinyWidgets)

source('./InitializeModel.R')

#loading icon
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# Preprocessing function
preprocess_text <- function(text) {
  # Remove "'s "
  text <- gsub("'s ", " ", text)
  
  # Replace U.S. with United States
  text <- gsub("U.S.", "United States", text)
  
  # Remove " x "
  text <- gsub(" x ", "", text)
  
  # Remove " X "
  text <- gsub(" X ", "", text)
  
  # Fix contractions
  text <- replace_contraction(text)
  
  # Remove non-alphanumeric
  text <- gsub("[^0-9A-Za-z///' ]", "", text, ignore.case = TRUE)
  
  # Convert to lowercase
  text <- tolower(text)
  
  # Stopword removal
  stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  text <- str_replace_all(text, stopwords_regex, '')
  
  # Lemmatize
  text <- lemmatize_strings(text)
  text <- gsub("unite state", "united state", text)
  
  return(text)
}

# Vectorization function
vectorize_text <- function(text) {
  # Tokenize into unigrams
  unigrams <- unlist(strsplit(text, "\\s+"))
  
  # Tokenize into bigrams
  bigrams <- c()
  if (length(unigrams) > 1) {
    bigrams <- sapply(1:(length(unigrams)-1), function(i) paste(unigrams[i:(i+1)], collapse = " "))
  }
  
  # Combine unigrams and bigrams
  vectorized_text <- c(unigrams, bigrams)
  
  return(vectorized_text)
}

# Mapping of numeric predictions to textual categories
topic_mapping <- c(
  "Literary legacy, storytelling, influential figures.",
  "Life moments, fame, family dynamics.",
  "State history, geographical significance.",
  "Creative works, artistic impact, cultural references.",
  "City narratives, historical events, societal shifts.",
  "Time periods, political changes, regional influence.",
  "Creative expressions, entertainment culture, artistic symbolism.",
  "Titles and symbols, leadership, national identity.",
  "Cultural expressions, historical landmarks, identity.",
  "Language and leadership, media impact, cultural milestones."
)

# Shiny UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  setBackgroundImage(
    src = "https://www.jeopardy.com/sites/default/files/styles/article_image_960_/public/2020-09/3_s37_logo.jpg?itok=sOoQKMxl"
  ),
  titlePanel("Jeopardy! Topic Analyzer"),
  tags$head(tags$style("h2 { color: #fff; font-size: 52px;}")),
  sidebarLayout(
    sidebarPanel(
      textInput("question", "Enter your Jeopardy! question:"),
      actionButton("analyze", "Analyze", style = "color: #fff; background-color: #007BFF; border-color: #007BFF;")
    ),
    mainPanel(
      fluidRow(
        column(12, textOutput("topic") %>% withSpinner(color = '#0dc5c1')),
        tags$head(tags$style("#topic { color: #fff; font-size: 24px; }"))
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$topic <- eventReactive(input$analyze, {
    
    if (input$question == "") {
      return("Please enter a Jeopardy! question.")
    }
    
    # Preprocess input
    processed_text = preprocess_text(input$question)
    
    # Vectorize input
    vectorized_text = vectorize_text(processed_text)
    
    # Perform analysis based on vectorized input
    predicted_topic_numeric = predict_topic(vectorized_text)
    
    #Map numeric prediction from model to text-defined categories
    predicted_topic_text <- topic_mapping[predicted_topic_numeric]
    
    # Display resulting topic
    paste("Predicted Topic:", predicted_topic_text)
  })
  
  output$accuracy <- eventReactive(input$analyze, {
    #Display model accuracy
    paste("Model Accuracy:", model_accuracy)
  })

  output$f1 <- eventReactive(input$analyze, {
    #Display model's F1 score
    paste("Model F1:", model_f1)
  })
}

# Run Shiny app
shinyApp(ui, server)

