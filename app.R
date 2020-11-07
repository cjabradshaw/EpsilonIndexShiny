# epsilon index R Shiny app
# Corey Bradshaw
# Flinders University

## remove everything
rm(list = ls())

# load libraries
library(shiny)
library(scholar)

# Google authentication
library(googleID)
library(shinyjs)
library(googleAuthR)

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"
                                        ))
options("googleAuthR.webapp.client_id" = "500611116199-1let7ftg4hgni9ccv6b2297ovv1ij48i.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "h-U5SScXB1xKNJWOqMDaKMWL")

#gar_set_client(scopes="https://www.googleapis.com/auth/analytics.readonly")

library(googleAnalyticsR)

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
              
              tabPanel(value="tab2", title="retrieve Google Scholar citation data",
                       useShinyjs(),
                       sidebarLayout(
                         sidebarPanel(
                           #gar_auth_jsUI("auth"),
                           
                           p(style="font-family:Avenir","To use this function, you must log in to your Google account",
                             googleAuthUI("gauth_login")),

                           wellPanel(
                             fileInput("file2", "choose delimited file with Google Scholar IDs & genders (2 columns)",
                                       multiple=F, buttonLabel="choose file",placeholder="no file selected"),
                             radioButtons("sep2","separator",choices=c(comma=',',space="",tab="\t")),
                             checkboxInput("header2", "header?", TRUE),
                             hr(),
                             actionButton("genButton", label="retrieve GS data"),
                             hr(),
                             selectInput("sortind2", "choose index to sort by:", 
                                         c("ε-index"="e","gender-debiased ε-index"="d","ε′-index"="ep","gender-debiased ε′-index"="dp")),
                             actionButton("calcButton2", label="calculate ε-index"),
                             br(),
                             tags$small(style="font-family:Avenir", "(refresh page to clear data)"),
                             br(),
                             downloadButton('downloadData2', 'download',icon = shiny::icon("download"))
                             
                             
                           ),
                         ),
                         
                         # open main panel
                         mainPanel(
                           textOutput("display_username"),
                           
                           fluidRow(
                             tags$div(id = "secondOutput", 
                                      h3("input data"),
                                      dataTableOutput("table2")) 
                           ),
                           
                           fluidRow(
                             tags$div(id = "placeholder2") # the dynamic UI will be inserted relative to this placeholder
                           ),
                           
                           dataTableOutput("etable2")
                           
                         ) # close mainPanel
                         
                       ) # sidebarLayout
                       
              ), # end tab2
              
              tabPanel(value="tab3", title=tags$strong("notes (input/output column descriptions)"),
                       tags$h2(style="font-family:Avenir", "Column descriptors"),
                       tags$h3(style="font-family:Avenir", "User-collated citation data"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("personID")," — any character identification of an individual researcher (can be a name)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("gender")," — researcher's gender (F or M)"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("i10")," — researcher's i10 index (# papers with ≥ 10 citations); must be > 0"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("h")," — researcher's h-index"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("maxcit")," — number of citations of researcher's most cited peer-reviewed paper"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("firstyrpub")," — the year of the researcher's first published peer-reviewed paper"),
                       
                       tags$h3(style="font-family:Avenir", "Google Scholar IDs & gender input"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("GSID")," — Google Scholar researcher ID"),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("gender")," — researcher's gender (F or M)"),
                       
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

        
    if(input$tabs == "tab2"){
      #auth <- callModule(gar_auth_js, "auth")
      
      ## Global variables needed throughout the app
      rv <- reactiveValues(
        login = FALSE
      )

      ## Authentication
      accessToken <- callModule(googleAuth, "gauth_login",
                                login_class = "btn btn-primary",
                                logout_class = "btn btn-primary")
      userDetails <- reactive({
        validate(
          need(accessToken(), "not logged in")
        )
        rv$login <- TRUE
        with_shiny(get_user_info, shiny_access_token = accessToken())
      })

      ## Display user's Google display name after successful login
      output$display_username <- renderText({
        validate(
          need(userDetails(), "getting user details")
        )
        userDetails()$displayName
      })

      ## Workaround to avoid shinyaps.io URL problems
      observe({
        if (rv$login) {
          shinyjs::onclick("gauth_login-googleAuthUi",
                           shinyjs::runjs("window.location.href = 'https://cjabradshaw.shinyapps.io/epsilonIndex/';"))
        }
      })
      
      output$table2 <- renderDataTable({
        file_to_read = input$file2
        if(is.null(file_to_read)){
          return()
        }
        read.table(file_to_read$datapath, sep=input$sep2, header=input$header2)
      }) # end output table2
      
      datin2 <- reactive({
        fileinp2 <- input$file2
        if(is.null(fileinp2)){return()}
        inpdat2 <- data.frame(read.csv(fileinp2$datapath, header = input$header2))
        return(inpdat2)
      }) # end datin2
      
      getProfileFunc <- function(gsdata) {
        gsdatalen <- dim(gsdata)[1]
        profileslist <- lapply(gsdata[,1], scholar::get_profile)
        hs <- i10s <- maxcits <- Y1s <- rep(NA,gsdatalen)
        for (r in 1:gsdatalen) {
          hs[r] <- profileslist[[r]]$h_index
          i10s[r] <- profileslist[[r]]$i10_index
          maxcits[r] <- get_publications(as.character(profileslist[[r]]$id))$cites[1]
          Y1s[r] <- get_oldest_article(profileslist[[r]]$id)
        }
        inputdata <- data.frame(gsdata[,1], gsdata[,2], i10s, hs, maxcits, Y1s)
        colnames(inputdata) <- c("personID", "gender", "i10", "h", "maxcit","firstyrpub")
        return(inputdata)
      }
      
      # when action button pressed ...
      observeEvent(input$genButton, {
        removeUI("div:has(>#secondOutput)")
        insertUI(
          selector = "#placeholder2",
          where = "afterEnd", # inserts UI after the end of the placeholder element
          ui = fluidRow(
            h3("GS data loading from server ..."),
            output$gstable <- renderDataTable({
              if(is.null(datin2())){return ()}
              results2 <<- getProfileFunc(gsdata=(datin2()))
            })))
      }) # end observeEvent
      
      observeEvent(input$calcButton2,
                   output$etable2 <- renderDataTable({
                     results3 <<- epsilonIndexFunc(datsamp=(results2), sortindex=input$sortind2)
                     results3
                   })
      ) # end observeEvent
      
      output$downloadData2 <- downloadHandler(
        filename = function() {
          paste("epsilonIndexOut", "csv", sep = ".")
        },
        
        content = function(file2) {
          sep <- ","
          
          write.table(results3, file2, sep=sep, row.names = F)
        }
      )
      
    } # end if for tab2
    
  }) # end tab Events
  
  session$onSessionEnded(stopApp)
} # end server

shinyApp(ui, server)

