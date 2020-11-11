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
  titlePanel(tags$p(style="font-family:Avenir", "Calculate the ε-index: a fairer way to rank researchers with citation data")),
  
  wellPanel(style = "background: azure",
    tags$a(href="https://github.com/cjabradshaw/EpsilonIndexShiny", tags$img(height = 200, src = "epsilonIndex logo.png", style="float:right")),
    tags$p(style="font-family:Avenir", tags$i(class="fab fa-r-project", title="R Project"),"Shiny App by", tags$a(href="https://globalecologyflinders.com/people/#CJAB", "Corey Bradshaw "),
           tags$a(href = "mailto:corey.bradshaw@flinders.edu.au","(",tags$i(class="far fa-envelope"),"e-mail"),";",
           tags$a(href = "https://github.com/cjabradshaw", tags$i(class="fab fa-github",title="Github"),"Github)")),
    tags$h4(style="font-family:Avenir", "Preamble"),
    tags$p(style="font-family:Avenir", "Existing citation-based indices used to rank research performance do not permit a fair comparison 
           of researchers among career stages or disciplines, nor do they treat women and men equally. We designed 
           the ε-index, which is simple to calculate, based on open-access data, corrects for disciplinary variation, 
           can be adjusted for career breaks, and sets a sample-specific threshold above and below which a researcher 
           is deemed to be performing above or below expectation. This", tags$i(class="fab fa-r-project"), "Shiny App estimates the ε-index and its variants 
           using user-provided data files. This", tags$i(class="fab fa-github"), "Github ",
           tags$a(href = "https://github.com/cjabradshaw/EpsilonIndexShiny", "repository"),
           "provides all the 'under-the-bonnet'",tags$i(class="fab fa-r-project"),"code for the app. The related paper (pre-print) is available",
           tags$a(href="https://doi.org/10.22541/au.160373218.83526843/v1", "here", tags$i(class="far fa-file"),".")),
    tags$h4(style="font-family:Avenir", "Instructions"),
    tags$ol(tags$li(tags$p(style="font-family:Avenir", "Create a delimited text file of", tags$strong("exactly the same format"), "as the example file in this",
           tags$a(href="https://github.com/cjabradshaw/EpsilonIndex/blob/main/datasample.csv","repository", tags$i(class="far fa-file")), ",
           although you can specify the delimit character (", tags$em("comma"),", ", tags$em("space"),", ", tags$em("tab"),").")),
           tags$li(tags$p(style="font-family:Avenir", "Load your delimited text file in the app by clicking the",tags$i(class="fas fa-file-import"),
           tags$strong("choose file"), "button.")),
           tags$li(tags$p(style="font-family:Avenir", "Select whether you want the index to be calculated for women and men separately as well as pooled (",
           tags$i(class="fas fa-venus-mars"), tags$strong("include gender split?"), "). If there are too few researchers in any gender category, the algorithm will fail.")),
           tags$a(href="https://globalecologyflinders.com/", tags$img(height = 100, src = "GEL Logo Kaurna transparent.png", style="float:right",
                                                               title="Global Ecology @ Flinders University")),
           tags$li(tags$p(style="font-family:Avenir", "Choose how you want the output file to be", tags$i(class="fas fa-sort"),
           "sorted by selecting one of the four choices in the drop-down menu:", tags$strong("ε-index"),",",tags$strong("gender-debiased ε-index"),",",
           tags$strong("ε′-index"),", or",tags$strong("gender-debiased ε′-index"),".")),
           tags$li(tags$p(style="font-family:Avenir", "Click the", tags$i(class="fas fa-calculator"), tags$strong("calculate ε-index"), "button.")),
           tags$li(tags$p(style="font-family:Avenir", "Download the results table as a", tags$i(class="fas fa-file-csv"), "file by clicking the", tags$i(class="fas fa-download"),
           tags$strong("download"), "button.")))
  ),
  
  tabsetPanel(id="tabs",
              tabPanel(value="tab1", title="user-collated citation data",
                       
                       sidebarLayout(
                         sidebarPanel(
                           wellPanel(style = "background: LightCyan",
                             fileInput("file1", label=tags$p(tags$i(class='fas fa-file-import'),"choose delimited file with citation data (6 columns)"),
                                       multiple=F, buttonLabel="choose file", placeholder="no file selected"),
                             tags$hr(),
                             radioButtons("sep",label=tags$p(tags$i(class="fas fa-file-csv"),"separator"),choices=c(comma=',',space="",tab="\t"), inline=T),
                             checkboxInput("header1", "header?", TRUE),
                             tags$hr(),
                             radioButtons("bygender", label=tags$p(tags$i(class='fas fa-venus-mars'), "include gender split?"), inline=T,
                                          choiceNames = list((icon("fas fa-thumbs-down")), (icon("fas fa-thumbs-up"))), choiceValues = list("no","yes")),
                             tags$hr(),
                             selectInput("sortind",label=tags$p(tags$i(class='fas fa-sort'), "choose sort index"), 
                                         c("ε-index"="e","gender-debiased ε-index"="d","ε′-index"="ep","gender-debiased ε′-index"="dp")),
                             tags$hr(),
                             actionButton("calcButton", label="calculate ε-index",icon=shiny::icon("fas fa-calculator")),
                             br(),
                             tags$small(style="font-family:Avenir", "(refresh page to clear data)"),
                             tags$hr(),
                             downloadButton('downloadData', 'download',icon = shiny::icon("download"))
                           ),
                         ),
                         
                         # open main panel
                         mainPanel(style = "background: GhostWhite",
                           
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
              
              tabPanel(value="tab3", title=tags$strong("input/output column descriptors"), style = "background: MintCream",
                       tags$h2(style="font-family:Avenir", "Column descriptors"),
                       tags$a(href="https://flinders.edu.au/", tags$img(height = 100, src = "F_V_CMYK.png", style="float:right",title="Flinders University")),
                       tags$h3(style="font-family:Avenir", "User-collated citation data"),
                       tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("personID")," — any character identification of an individual researcher (can be a name)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("gender")," — researcher's gender (F or M)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("i10")," — researcher's i10 index (# papers with ≥ 10 citations); must be > 0")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("h")," — researcher's h-index")),
                       tags$a(href="https://epicaustralia.org.au/", tags$img(height = 150, src = "CABAHlogo.png",
                                                                             style="float:right", title="ARC Centre of Excellence for Australian Biodiversity and Heritage")),
                       tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("maxcit")," — number of citations of researcher's most cited peer-reviewed paper")),
                       tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("firstyrpub")," — the year of the researcher's first published peer-reviewed paper"))),
                       
                       tags$h3(style="font-family:Avenir", "ε-index output"),
                       tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("ID")," — researcher ID")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("gen")," — researcher's gender (F or M)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("yrsP")," — number of years since first peer-reviewed paper")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("genE")," — ε-index relative to others of the same gender in the sample (not included if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("exp")," — whether above or below expectation based on chosen index (default is 'e' = pooled index)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("m")," — h-index ÷ yrsP")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 7"),": ", tags$em("h")," — h-index")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 8"),": ", tags$em("genRnk")," — rank from gender.eindex (1 = highest) (not included if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 9"),": ", tags$em("debRnk")," — gender-debiased rank (1 = highest) (not included if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 10"),": ", tags$em("poolE")," — ε-index generated from the entire sample (not gender-specific) (COLUMN 4 if you select gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 11"),": ", tags$em("poolRnk")," — rank from poolE (1 = highest) (COLUMN 8 if you select a gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 12"),": ", tags$em("eP")," — scaled poolE (ε′-index) (COLUMN 9 if you select a gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 13"),": ", tags$em("debEP")," — scaled genE (gender ε′-index) (not included if you select no gender split)")),
                       tags$br(),
                       tags$li(tags$p(style="font-family:Avenir", tags$em("if sort index = 'ε′-index'")),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 14"),": ", tags$em("ePRnk")," — rank from ε′-index")),
                       tags$br(),
                       tags$a(href="https://github.com/cjabradshaw/EpsilonIndexShiny/blob/main/LICENSE", tags$img(height = 50, src = "GNU GPL3.png", style="float:right", title="GNU General Public Licence v3.0")),
                       tags$li(tags$p(style="font-family:Avenir", tags$em("if sort index = 'gender-debiased ε′-index'")),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 14"),": ", tags$em("ePddebRnk")," — rank from gender ε′-index"))),
                       
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
        inpdat <- data.frame(read.table(fileinp$datapath, sep=input$sep, header = input$header1))
        return(inpdat)
      }) # end datin
      
      sortInd <- reactiveValues()
      observe({
        sortInd$x <- as.character(input$sortind)
        sortInd$y <- ifelse(input$bygender == "no", "d", sortInd$x)
      })
      
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
              results <<- epsilonIndexFunc(datsamp=(datin()), bygender=input$bygender, sortindex=sortInd$y)
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
