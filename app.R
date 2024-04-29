library(shiny)
library(readxl)
library(tm)
library(wordcloud)
library(tidytext)
library(matrixStats)
library(ggplot2)
library(dplyr)
library(dbplyr)
library(dtplyr)
library(wordcloud2)
CoE_Dataset2 <- read_excel("Desktop/MS983_LITERATURE/Coffee/CoE_Dataset.xlsx")
stopwords2 <- c("the", "and", "or", "but", "is", "are", "like","complex", 
               "bright", "citric acid","citrus", 
               "tart",  "crisp",  "citrus fruit","delicate",  
               "mellow",   "red apple",  "round", 
               "clean",  "lactic", "sour","malic",  "simple", "prune", 
               "sharp", "lively",
               "citric","sparkling",  "tangy","sweet","balanced", 
               "tangerine",  "nectarine",  "effervescent", "intense",   
               "mild", "vibrant","mild acidity" , "flat",   "smooth",
               "thin",   "creamy",
               "well balanced" ,  "elegant","silky",  "syrupy", "long lasting" ,  
               "pleasant","savory",  
               "spicy",  "rounded","very sweet", "cinnamon",
               "winey",  "firm",   "full",   "dense",  "light",
               "rich", "winy",  
               "bright citric", "layered","fresh", "structure",  "sweetness",
               "sweet citric","malty" )  # Add  stopwords here

# Tokenize the text values and remove stopwords and punctuation
tokens2 <- CoE_Dataset2 %>%
  mutate(token = str_split(tolower(`Aroma/Flavour`), ",\\s*|\\sand\\s*")) %>%
  unnest(token) %>%
  mutate(token = str_replace_all(token, "[[:punct:]]", ""),  # Remove punctuation
         token = str_replace(token, "\\d+", ""),  # Remove numbers after tokens
         token = str_replace_all(token, "hint of", ""),
         token = str_trim(token, side="both"), 
         token = ifelse(token %in% stopwords, "", token))   %>%
  filter(!str_detect(token, paste0("\\b", paste(stopwords, collapse = "\\b|\\b"), "\\b")))# Remove stopwords

# Filter out empty tokens
tokens2 <- tokens2 %>%
  filter(token != "")

duplicated_rows <- duplicated(tokens2) | duplicated(tokens2, fromLast = TRUE)
num_duplicates <- sum(duplicated_rows)

tokens2 <- unique(tokens2)

ui <- fluidPage(
  titlePanel("Cup of Excellence Coffee Aroma/Flavour Analysis- WordCloud"),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("price_start",
                  "Price Starting:",
                  min = 1,  max = 200, value = 15),
      sliderInput("price_end",
                  "Price Ending",
                  min = 1,  max = 200,  value = 25),width = 3
    ),
    mainPanel(
      wordcloud2Output("plot",width="150%",height="750px")
    )
    
  ))
  
 server <- function(input, output,session) {
  
  CoE_fltr_10usdnew <- reactive(
    tokens2[tokens2$USDPrice >= input$price_start & tokens2$USDPrice <= input$price_end, ])
  
  #wordcloud_rep <- repeatable(wordcloud2)
  #browser()
  CoE_fltr_10usd_topn_new <- reactive({
    result <- CoE_fltr_10usdnew() %>% count(token, sort = TRUE)
    #print(result)  # Add print statement to check the contents of the dataframe
    # return(result) # Ensure to return the result
  })
  column_vector <- reactive({
    result <- CoE_fltr_10usd_topn_new()[[2]]
    #print(result)  # Add print statement to check the contents of the column vector
    #return(result) # Ensure to return the result
  })
  quantiles <- reactive({
    result <- quantile(column_vector(), probs = c(0.7, 0.9))
    #print(result)  # Add print statement to check the calculated quantiles
    #return(result) # Ensure to return the result
  })
 
  
  output$plot <- renderWordcloud2({
    
    wordcloud2(data = CoE_fltr_10usd_topn_new(), 
               shape = 'star',
               minRotation = -pi/4, 
               color = ifelse(column_vector() >= quantiles()[2], 'lightgreen',
                              ifelse(column_vector() >= quantiles()[1] & column_vector() < quantiles()[2], 'orange', 'yellow')),
               backgroundColor = "grey", 
               size = 0.5)
  })
 }
 
 shinyApp(ui, server)