# Copyright (C) 2015-2016 Bohdan Khomtchouk and James Hennessy

# This file is part of MicroScope.

# ------------------------------------------------------------------------------------

library(shiny)
library(d3heatmap)
library(htmlwidgets)
library(tools)
library(data.table)
library(dplyr)
library(png)
options(shiny.maxRequestSize = 200*1024^2)

# backend 
server <- shinyServer(function(input, output) {	
  
  # instructions tab
  output$text1 <- renderText({ "0) You can easily make a .csv file by simply saving your Microsoft Excel workbook as a .csv through 'Save As'.  Before saving as a .csv, your Excel file should look something like:" })
  output$text2 <- renderText({ "Nba Timeout finder, find intersting stats"})

  # sample file download
  output$downloadData <- downloadHandler(
  	filename <- function() {
    	paste('basketball', '_file', '.csv', sep='')
  	},
  	content <- function(file) {
    	file.copy("basketball_file.csv", file)
  	},
  	contentType = "text/csv"
	)


  # file upload
  datasetInput <- reactive({
    validate(
    	need(input$filename != 0, "To perform statistical analysis, please select a file for input") 
    )
    inFile <- input$filename
    if (is.null(inFile)) return(NULL)
    read.table(inFile$datapath, header= TRUE, sep=",", quote='"', row.names=1)
  })
  
  
  
  # filter stats table based on cutoffs

    forStats<-reactive({
      
        if(!is.null(datasetInput()) ){
          Hackathon_play_by_play<-datasetInput()
          results=""
          Timeout_loc<-which(Hackathon_play_by_play$Event_Description=="Timeout")
          timeoutDataSet<-Hackathon_play_by_play[Timeout_loc,]
          Sub_loc<-which(Hackathon_play_by_play$Event_Description=="Substitution")
          newPlay_play<-Hackathon_play_by_play[-c(Sub_loc)]
          
          pre_TO<-which(newPlay_play$Event_Num==(timeoutDataSet$Event_Num-1))
          post_TO<-which(newPlay_play$Event_Num==(timeoutDataSet$Event_Num))
          post_TO<-post_TO+1
          
          dataBall<-unique(Hackathon_play_by_play$Event_Description)
          after<-which(Hackathon_play_by_play$Event_Description=="Time Out")
          afterTime<-Hackathon_play_by_play[post_TO,]
          
          
          for(value in dataBall){
            len<-which(afterTime$Event_Description==value)
            rbind(results,c(value,length(len)/length(afterTime$Event_Num)))
          }
          results_df <- as.data.frame(results)
          
          
        }
      })
      
      
    output$timeout<-renderPrint({
          if(input$homeTeam){
          print("Yes you should")
          }
      else{
        print("no you shouldn't ")
      }
      
    })
    player<-reactive({
      player<-input$player
    })
    
    playerOutput<-reactive({
      if(input$goButton ==0){
        return(validate(
          need(input$filename != 0, "For gene ontology analysis, please first select a file for input") %then%
            need(input$goButton != 0, "For gene ontology analysis, please first select your experimental samples under 'Specify Non-Control Samples' then click 'Run Statistics'")
          
        ))}
      if(input$goButton!=0){
        afterTime<-datasetInput()
        player<-player()
        pos=grep(pattern=toString(player()),afterTime$Description)
        finish<-afterTime$Description[pos]
        finish_df<-as.data.frame(finish)
      }
    })
    output$findPlayer<-renderPrint({
      if(input$goButton ==0){
        return(validate(
          need(input$filename != 0, "For bb ball analysis, please first select a file for input") %then%
            need(input$goButton != 0, "For bball analysis, please first select your experimental samples under")
            
      ))}
      if(input$goButton!=0){
      afterTime<-datasetInput()
      player<-player()
      pos=grep(pattern=toString(player()),afterTime$Description)
      finish<-afterTime$Description[pos]
      finish_df<-as.data.frame(finish)
      print(finish_df)
      }
      else{
        print("Hello no luck sorry")
      }
      

    })
  
  
  # PCA engine
  PCAfun <- function() {
    Hackathon_play_by_play<-datasetInput()
    results=""
    Timeout_loc<-which(Hackathon_play_by_play$Event_Description=="Timeout")
    timeoutDataSet<-Hackathon_play_by_play[Timeout_loc,]
    Sub_loc<-which(Hackathon_play_by_play$Event_Description=="Substitution")
    newPlay_play<-Hackathon_play_by_play[-c(Sub_loc)]
    
    pre_TO<-which(newPlay_play$Event_Num==(timeoutDataSet$Event_Num-1))
    post_TO<-which(newPlay_play$Event_Num==(timeoutDataSet$Event_Num))
    post_TO<-post_TO+1
    
    dataBall<-unique(Hackathon_play_by_play$Event_Description)
    after<-which(Hackathon_play_by_play$Event_Description=="Time Out")
    afterTime<-Hackathon_play_by_play[post_TO,]
    rownames(afterTime) <- c()
  		dm <- data.matrix(afterTime)
  		print("hello")
  		if (input$Type == "Covariance Matrix") {
  			PCA <<- prcomp(dm, scale = TRUE)
  		}  
  		else if (input$Type == "Correlation Matrix") {
  			PCA <<- prcomp(dm, scale = FALSE)
  		}
  		
  	}
  
  
  
  
  
  # edgeR prep
  
  
  stats <- reactive({
    if(!is.null(datasetInput()) ){
   
    
      Hackathon_play_by_play<-datasetInput()
      results=matrix(, nrow = input$rows, ncol = input$col)

      Timeout_loc<-which(Hackathon_play_by_play$Event_Description=="Timeout")
      timeoutDataSet<-Hackathon_play_by_play[Timeout_loc,]
      Sub_loc<-which(Hackathon_play_by_play$Event_Description=="Substitution")
      newPlay_play<-Hackathon_play_by_play[-c(Sub_loc)]
      
      pre_TO<-which(newPlay_play$Event_Num==(timeoutDataSet$Event_Num-1))
      post_TO<-which(newPlay_play$Event_Num==(timeoutDataSet$Event_Num))
      post_TO<-post_TO+1
      
      dataBall<-unique(Hackathon_play_by_play$Event_Description)
      group <- as.numeric(names(datasetInput()) %in% input$expcolumns)
      after<-which(Hackathon_play_by_play$Event_Description=="Time Out")
      if(input$Time =="After"){
        
      afterTime<-Hackathon_play_by_play[post_TO,]
      }
      else{
        
        afterTime<-Hackathon_play_by_play[pre_TO,]
      }
      n<-0
      for(value in dataBall){
        len<-which(afterTime$Event_Description==value)
        v<-(c(value,length(len)/length(afterTime$Event_Num)))
        results[n,]<-v
        n<-n+1
      }
      print(results)
      results_df <- as.data.frame(results)
    }
  })
  
  
  # differential expression analysis table
  `%then%` <- shiny:::`%OR%`
  output$table <- renderTable({
    if (input$goButton == 0) {return(validate(
    	need(input$filename != 0, "To run differential expression analysis, please first select a file for input") %then%
        need(input$goButton != 0, "To run  analysis, please select your expernce'")
    ))}        
    else {
      stats()
    }
  }, digits = -3)
  
  output$pca_summary_table <- renderPrint({
    if (input$goButton == 0) {return(validate(
      need(input$filename != 0, "Table of PCA summary: To conduct principal component analysis, please first select a file for input") %then%
        need(input$goButton != 0, "Table of PCA summary: To conduct principal component analysis, click 'Run PCA'")
    ))}        
    else {
      PCAfun()
      summary(PCA)
    }
  }) 
  # statistical table download
  output$downloadtable <- downloadHandler(
    filename = function() {
      paste(basename(file_path_sans_ext(input$filename)), '_stats_table', '.csv', sep='')
    },
    content = function(file) {
      if (input$goButton != 0) write.csv(stats(), file) else return()
    }
  )
  output$downloadPlayer<-downloadHandler(
    filename = function() {
      paste(basename(file_path_sans_ext(input$filename)), '_player_table', '.csv', sep='')
    },
    content = function(file) {
      if (input$goButton != 0) write.csv(playerOutput(), file) else return()
    }
    
  )

   
    
	
  
						
})
  