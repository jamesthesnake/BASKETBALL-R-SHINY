


library(shiny)
library(d3heatmap)
library(RColorBrewer)
library(networkD3)


# frontend
ui <- shinyUI(pageWithSidebar(
  headerPanel("NBA TimeOut"),
  
  sidebarPanel(
    
    downloadButton("downloadData", label = "Download Sample Input File"),
    fileInput("filename", "Choose File to Upload:", accept = c('.csv')),
    numericInput('col',"number of col",value=2),
    numericInput('rows',"number of rows",value=15),
    selectInput("Time", "Choose Before or After time out", selected="Before",c("Before","After")),
    actionButton("goButton", "Run Statistics"),
    downloadButton("downloadtable", "Download Stats Table"),
    textInput("player", label = h3("player input"), value = "Curry"), 
    actionButton("goButton", "find player"),
    checkboxInput("homeTeam","home team"),
    downloadLink("downloadPlayer","Download player"),
    selectInput("Type","type of PCA",selected="Covariance Matrix",c("Covariance Matrix","Correlation Matrix"))
    
   
    
  ),

  mainPanel(
  
    tabsetPanel(
      tabPanel("Instructions", textOutput("text1"), img(src='FullCourt.jpeg'), textOutput("text2")),
      tabPanel("table Analysis", tableOutput("table")),
      tabPanel("Find Player", verbatimTextOutput("findPlayer")),
      tabPanel("should i call a time out", verbatimTextOutput("timeout")),
      tabPanel("PCA", verbatimTextOutput("pca_summary_table")),
      
      tabPanel("heatmap",uiOutput("heatmap"))
	  )
	  
  )


)
)
