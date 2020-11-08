# epsilon index R Shiny app
# Corey Bradshaw
# Flinders University

## remove everything
rm(list = ls())

# load libraries
library(shiny)

## call functions
source(file.path("./", "epsilonIndexFunc.R"), local=T)

ui <- fluidPage(
  
  tags$head(
    tags$meta(name="google-site-verification", content="bVQ54bgpekDeCqy30pOSnGOMgbNXXV1AdwIoMRXSAAI")
  ),
  
  # title of app
  titlePanel("calculate ε-index"),
  
  wellPanel(
    tags$p(style="font-family:Avenir", "R Shiny App by", tags$a(href="https://globalecologyflinders.com/people/#CJAB", "Corey Bradshaw "),
           tags$a(href = "mailto:corey.bradshaw@flinders.edu.au","(e-mail; "),
           tags$a(href = "https://github.com/cjabradshaw", "Github)")),
    tags$p(style="font-family:Avenir", "Existing citation-based indices used to rank research performance do not permit a fair comparison 
           of researchers among career stages or disciplines, nor do they treat women and men equally. We designed 
           the ε-index, which is simple to calculate, based on open-access data, corrects for disciplinary variation, 
           can be adjusted for career breaks, and sets a sample-specific threshold above and below which a researcher 
           is deemed to be performing above or below expectation. This R Shiny App estimates the ε-index and its variants 
           using user-provided data files. All R code and example data necessary to reproduce these analyses is available ",
           tags$a(href = "https://github.com/cjabradshaw/EpsilonIndex", "here"), ".")
  ),
  
  tabsetPanel(id="tabs",
              tabPanel(value="tab1", title="user-collated citation data",
                       
                       sidebarLayout(
                         sidebarPanel(
                           wellPanel(
                             fileInput("file1", "choose delimited file with the necessary citation data (6 columns)",
                                       multiple=F, buttonLabel="choose file",placeholder="no file selected"),
                             radioButtons("sep","separator",choices=c(comma=',',space="",tab="\t")),
                             checkboxInput("header1", "header?", TRUE),
                             tags$hr(),
                             selectInput("sortind", "choose index to sort by", 
                                         c("ε-index"="e","gender-debiased ε-index"="d","ε′-index"="ep","gender-debiased ε′-index"="dp")),
                             actionButton("calcButton", label="calculate ε-index"),
                             br(),
                             tags$small(style="font-family:Avenir", "(refresh page to clear data)"),
                             br(),
                             downloadButton('downloadData', 'download',icon = shiny::icon("download"))
                           ),
                         ),
                         
                         # open main panel
                         mainPanel(
                           
                           fluidRow(
                             tags$div(id = "firstOutput", 
                                      h3("input data"),
                                      dataTableOutput("table1")) 
                           ),
                           
                           fluidRow(
                             tags$div(id = "placeholder") # the dynamic UI will be inserted relative to this placeholder
                           ),
                           
                         ) # close mainPanel
                         
                       ) # sidebarLayout
              ), # end tab1
              
              tabPanel(value="tab3", title=tags$strong("notes (input/output column descriptions)"),
                       tags$h2(style="font-family:Avenir", "Column descriptors"),
                       tags$h3(style="font-family:Avenir", "User-collated citation data"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("personID")," — any character identification of an individual researcher (can be a name)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("gender")," — researcher's gender (F or M)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("i10")," — researcher's i10 index (# papers with ≥ 10 citations); must be > 0"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("h")," — researcher's h-index"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("maxcit")," — number of citations of researcher's most cited peer-reviewed paper"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("firstyrpub")," — the year of the researcher's first published peer-reviewed paper"),
                       
                       tags$h3(style="font-family:Avenir", "ε-index output"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("ID")," — researcher ID"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("gen")," — researcher's gender (F or M)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("yrsP")," — number of years since first peer-reviewed paper"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("genE")," — ε-index relative to others of the same gender in the sample"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("exp")," — whether above or below expectation based on chosen index (default is 'e' = pooled index)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("m")," — h-index ÷ yrsP"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 7"),": ", tags$em("h")," — h-index"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 8"),": ", tags$em("genRnk")," — rank from gender.eindex (1 = highest)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 9"),": ", tags$em("debRnk")," — gender-debiased rank (1 = highest)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 10"),": ", tags$em("poolE")," — ε-index generated from the entire sample (not gender-specific)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 11"),": ", tags$em("poolRnk")," — rank from poolE (1 = highest)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 12"),": ", tags$em("eP")," — scaled poolE (ε′-index)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 13"),": ", tags$em("debEP")," — scaled genE (gender ε′-index)"),
                       tags$br(),
                       tags$p(style="font-family:Avenir", tags$em("if sort index = 'ε′-index'")),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 14"),": ", tags$em("ePRnk")," — rank from ε′-index"),
                       tags$br(),
                       tags$p(style="font-family:Avenir", tags$em("if sort index = 'gender-debiased ε′-index'")),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 14"),": ", tags$em("ePddebRnk")," — rank from gender ε′-index"),
                       tags$br()
                       
                       
              ) # end tab3
  ) # end tabsetPanel
  
) # close fluidPage


server <- function(input, output, session) {
  
  observeEvent(input$tabs, {
    
    if(input$tabs == "tab1"){
      
      output$table1 <- renderDataTable({
        file_to_read = input$file1
        if(is.null(file_to_read)){
          return()
        }
        read.table(file_to_read$datapath, sep=input$sep, header=input$header1)
      }) # end output table1
      
      datin <- reactive({
        fileinp <- input$file1
        if(is.null(fileinp)){return()}
        inpdat <- data.frame(read.csv(fileinp$datapath, header = input$header1))
        return(inpdat)
      }) # end datin
      
      # when action button pressed ...
      observeEvent(input$calcButton, {
        removeUI("div:has(>#firstOutput)")
        insertUI(
          selector = "#placeholder",
          where = "afterEnd", # inserts UI after the end of the placeholder element
          ui = fluidRow(
            h3("output"),
            output$etable <- renderDataTable({
              if(is.null(datin())){return ()}
              results <<- epsilonIndexFunc(datsamp=(datin()), sortindex=input$sortind)
            })))
      }) # end observeEvent
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("epsilonIndexOut", "csv", sep = ".")
        },
        
        content = function(file) {
          sep <- ","
          
          write.table(results, file, sep=sep, row.names = F)
        }
      )
    } # end if for tab1
    
  }) # end tab Events
  
  session$onSessionEnded(stopApp)
  
} # end server

shinyApp(ui, server)

