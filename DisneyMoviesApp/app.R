
ui <-   navbarPage("Disney",
                   
                   
                   tabPanel("VIEW DATA",
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("dataset", "Select a dataset:",
                                            choices = c("Gross Income", "Characters", "Directors")),
                                selectizeInput('columns','Select The Movie',"")),
                              
                              mainPanel(
                                
                                tableOutput("view")
                                
                              )
                            )
                            
                   ),
                   tabPanel("WORDCLOUD",
                            sidebarPanel(
                              selectInput("income1","Select Income for wordcloud",choices = c("Total Gross Income","Inflated Gross Income"))),
                            mainPanel(
                              plotOutput("cloud",width = "100%")
                            )
                            
                   ),
                   tabPanel("GENRE",
                            sidebarPanel(
                              selectInput("income","Select Income for genre",choices = c("Total Gross Income","Inflated Gross Income"))),
                            
                            mainPanel(
                              plotOutput("genre")
                            )
                            
                   )
                   
                   
)





server <- function(input, output,session) {
  
  

  Gross_Income <- read.csv("disney_movies_total_gross.csv",header = TRUE,fileEncoding="UTF-8-BOM")
  Characters <- read.csv("disney-characters.csv",header = TRUE,fileEncoding="UTF-8-BOM")
  Directors <- read.csv("disney-director.csv",header = TRUE,fileEncoding="UTF-8-BOM")
  
  library(shiny)
  library(ggplot2)
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(NLP)
  library(RColorBrewer)
  library(plyr)
  library(date)
  library(dplyr)
  
  
  
  
  outVar <- reactive({
    
    switch(input$dataset,
           "Gross Income" = Gross_Income,
           "Characters" = Characters,
           "Directors" = Directors)
    
  })
  
  
  observe({
    updateSelectInput(session,"columns",choices = outVar()$movie_title)
  })
  
  
  output$view <- renderTable({
    test <- subset(outVar(),outVar()$movie_title == input$columns)
    return(test)
  })
  
  output$cloud <- renderPlot({
    
    if(input$income1 == "Total Gross Income"){
      
      
      
      topDataGross <- as.numeric(gsub('[$,]','',Gross_Income$total_gross))
      
      
      Gross_Income$total <- topDataGross
      
      
      datacloud <-head(arrange(Gross_Income,desc(total)), n = 20)
      
      datacloud$Rank <- rank(datacloud$total) 
      
      
      docs <- Corpus(VectorSource(datacloud$movie_title))
      inspect(docs)
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      
      inspect(docs)
      dataframe <- data.frame(text=sapply(docs, identity), 
                              stringsAsFactors=F)
      
      datacloud$movie_title <- dataframe$text
      
      
      wordcloud(words = datacloud$movie_title ,freq = datacloud$Rank,min.freq=1,scale = c(1.5,0.7),
                max.words=200,random.order=FALSE,rot.per=0.5,colors=brewer.pal(8,"Dark2") ) 
    }
    else if (input$income1 == "Inflated Gross Income"){
      topDataGross <- as.numeric(gsub('[$,]','',Gross_Income$inflation_adjusted_gross))
      
      
      Gross_Income$total <- topDataGross
      
      
      datacloud <-head(arrange(Gross_Income,desc(total)), n = 20)
      
      datacloud$Rank <- rank(datacloud$total) 
      docs <- Corpus(VectorSource(datacloud$movie_title))
      inspect(docs)
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers.
      docs <- tm_map(docs, removeNumbers)
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      
      inspect(docs)
      dataframe <- data.frame(text=sapply(docs, identity), 
                              stringsAsFactors=F)
      
      datacloud$movie_title <- dataframe$text
      
      
      wordcloud(words = datacloud$movie_title ,freq = datacloud$Rank,min.freq=1,scale = c(1.5,0.7),
                max.words=200,random.order=FALSE,rot.per=0.5,colors=brewer.pal(8,"Dark2") ) 
      
    }
    
  },height = 490,width = 400)
  output$genre <- renderPlot({
    
    if(input$income == "Total Gross Income"){
      topDataGross <- as.numeric(gsub('[$,]','',Gross_Income$total_gross))
      
      
      Gross_Income$total_gross <- topDataGross
      
      subsetData <- subset(Gross_Income,Gross_Income$genre != "")
      
      
      
      group_genre <- aggregate(total_gross ~ genre,data = subsetData,FUN = sum)
      group_genre
      
      group_genre$genre <- factor(group_genre$genre, levels = group_genre$genre[order(group_genre$total_gross)])
      
      options(scipen=1000000)
      
      ggplot(group_genre,aes(genre,total_gross)) + geom_bar(stat = "identity", aes(fill = genre)) + coord_flip() + theme_minimal() +
        scale_y_continuous(limits=c(0, 20000000000), breaks=c(0, 5000000000, 10000000000, 15000000000,20000000000))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab("Genre")+ylab("Total Gross")
    }
    else if(input$income == "Inflated Gross Income"){
      topDataGross1 <- as.numeric(gsub('[$,]','',Gross_Income$inflation_adjusted_gross))
      Gross_Income$inflation_adjusted_gross <- topDataGross1
      
      subsetData <- subset(Gross_Income,Gross_Income$genre != "")
      
      group_genre <- aggregate(inflation_adjusted_gross ~ genre,data = subsetData,FUN = sum)
      group_genre
      
      group_genre$genre <- factor(group_genre$genre, levels = group_genre$genre[order(group_genre$inflation_adjusted_gross)])
      options(scipen=1000000)
      
      p <- ggplot(group_genre,aes(genre,inflation_adjusted_gross)) 
      p +geom_bar(stat = "identity", aes(fill = genre)) + coord_flip() + theme_minimal()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ xlab("Genre")+ ylab("Inflation Adjusted Gross")
      
      
    }
    
    
    
    
    
  })
}
shinyApp(ui = ui, server = server)


