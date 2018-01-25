#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)

# Define UI for application that draws a histogram

started<<-FALSE

DF<-data.frame(Time=c(""),Question=c(""),Category=factor(c(NA),levels=c("Convergent","Divergent","Other")))
print(DF$Category)

ui <- shinyUI(
  fluidPage(
    titlePanel("Question VTC"),
    sidebarLayout(
      sidebarPanel(
        helpText("This is the app for recording the question VTC's.", 
               "Press the record question button when the teacher asks a question to the class.", 
               "You can edit the text under the question column to put in what the specific question was.",
               "The category column has a drop down option to categorize what kind of question the teacher asked.",
               "Put in a file location to save the table for the post consultation."),
        br(), 
        wellPanel(h3("Record Question"),
          
          div(class="col-sm-5",
            actionButton("start", "Start")),
          div(class="col-sm-5", 
              actionButton("record","Record Question")),
          br(),br()
        ),
        wellPanel(
          h3("Save table"), 
          div(class='row', 
            div(class="col-sm-6", 
                actionButton("save", "Save Data")),
            div(class="col-sm-6",
                textInput("saveFile","File name")
            )
          )
        ),
        wellPanel(
          h3("Load table"), 
          div(class='row', 
              div(class="col-sm-6", 
                  actionButton("load", "Load Data")),
              div(class="col-sm-6",
                  textInput("loadFile","File name")
              )
          )
        )
      ),
    
      mainPanel(
        rHandsontableOutput("hot")
      )
    )
  )
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      values[["previous"]] <- isolate(values[["DF"]])
      DF = hot_to_r(input$hot)
      
    } else {
      if (is.null(values[["DF"]]))
        if(nrow(DF)==0)
          DF<-NULL
        else
          DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF)){
        rhandsontable(DF, stretchH = "all",height="500") %>%
        hot_col("Question", allowInvalid=TRUE) %>%
        hot_col("Category","dropdown",source=c("Convergent","Divergent","Other"),strict=TRUE)
    }
    else{
      NULL
    }
      
  })
  
  ## Save data
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    write.csv(finalDF,input$saveFile,row.names=FALSE)
    
    })
  # Load data
  observeEvent(input$load, {
    finalDF <- isolate(values[["DF"]])
    DF<-read.csv(input$loadFile,stringsAsFactors = TRUE)
    
    values[["DF"]]<-DF
    
  })
  # check if started timer
  observeEvent(input$start, {
    if(!started){
      startTime <<-Sys.time()
      started   <<-TRUE
    }
  })

  
  
  ## Add row
  
  observeEvent(input$record, {
    if(started){
      DF <- isolate(values[["DF"]])
      values[["previous"]] <- DF
      t<-round(as.numeric(difftime(Sys.time(),startTime,units="sec")))
      m<-t%/%60
      s<-t-m*60
    
      if(m<10){
        M<-paste("0",m,sep="")
      }
      else{
        S<-paste(s,sep="")
      }
      if(s<10){
        S<-paste("0",s,sep="")
      }
      else{
        S<-paste(s,sep="")
      }
      tt<-paste(M,":",S,sep="")
      if(nrow(DF)<=1 && DF$Time==""){
        values[["DF"]]<-data.frame(Time=c(tt),Question=c("Put question here."),
                   Category=factor(c(NA),levels=c("Convergent","Divergent","Other")))
      }
      else{
        values[["DF"]]<-rbind(DF,data.frame(Time=c(tt),Question=c("Put question here."),
                  Category=factor(c(NA),levels=c("Convergent","Divergent","Other"))))
      }
    }
    else{
      showModal(modalDialog(
        title = "Press Start",
        "Press start button to add a new question."
      ))
    }
  })
  
 

})


# Run the application
shinyApp(ui = ui, server = server)

