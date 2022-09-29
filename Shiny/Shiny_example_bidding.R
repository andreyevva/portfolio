#Setting the working directory
setwd("C:/Users/anandreeva/Downloads")

#Importing the necessary libraries
library(shiny) # <-- R Shiny app developer package
library(mlbench)
library(plotly)
library(shinythemes)
library(dplyr)
library(Rcpp)
library(yaml)
library(rlang)
library(httpuv)
library(mime)
library(jsonlite)
library(stringr)
library(rsconnect) # <-- package to publish visualisations online

#Import data
df_full=read.csv('Rshiny.csv',sep=";", colClasses=c("factor","numeric","integer","factor","factor","factor","factor"), na.strings=c("missing",""))
df_full=df_full %>% filter(singleb != "")
df_full=df_full %>% filter(market_id_name != "")


###Creating a user interface
choiceList <- unique(df_full$country)

# ui.R definition
ui <- fluidPage(    
  # Give the page a title
  titlePanel("Single bidding by country"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("country", "Country:", 
                  choices=choiceList),
      hr(),
      helpText("Data collected by the Horizont2020 funded research project: DIGIWHIST. It collected data from European jurisdictions and it contains both below and above EU-threshold public procurement contracts where the national system was of sufficient scope.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("countryPlot", height = "600px", width = "850px")  
    )
    
  )
)

###Creating  server tasks

# server.R definition
server <- function(input,output){
  
  # Creating a dataset for a chosen country
  df_subset <- reactive({
    a <- subset(df_full, country == input$country)
    column1=a %>% 
      group_by(market_id_name) %>% 
      summarise(Singleb = sum(singleb))
    
    column2=a %>% 
      group_by(market_id_name) %>% 
      summarise(Overall = n())
    
    graph1=merge(column1,column2,bu='market_id_name')
    graph1$Share_singleb=graph1$Singleb*100/graph1$Overall
    return(graph1)
  })
  
 
  output$countryPlot <- renderPlot({
    
    # Render a barplot
    ggplot(df_subset(), aes(x=reorder(market_id_name, Share_singleb),y=Share_singleb)) + geom_bar(stat="identity") + xlab("Market") + ylab("% of single bidding") + ggtitle("Share of single bidding by market") +
      theme_bw ()+
      theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
      theme(panel.border = element_blank())
  })
  
}

#Launching the app
shinyApp(ui = ui, server = server)



