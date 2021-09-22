# epsilon index R Shiny app
# Corey Bradshaw
# Flinders University

## remove everything
rm(list = ls())

# load libraries
library(shiny)
library(shinybusy)
library(ggplot2)
library(ggpubr)
library(ggrepel)

## call functions
source(file.path("./functions/", "AICc.R"), local=T)
source(file.path("./functions/", "deltaIC.R"), local=T)
source(file.path("./functions/", "weightIC.R"), local=T)
source(file.path("./functions/", "linregER.R"), local=T)
source(file.path("./functions/", "setBackgroundColor.R"), local=T)
source(file.path("./functions/", "epsilonIndexFunc.R"), local=T)

ui <- fluidPage(
  
  tags$head(
    tags$meta(name="google-site-verification", content="bVQ54bgpekDeCqy30pOSnGOMgbNXXV1AdwIoMRXSAAI")
  ),
  
  # title of app
  titlePanel("The ε-index: a fairer way to rank researchers with citation data"),
  
  wellPanel(style = "background: azure",
    tags$a(href="https://github.com/cjabradshaw/EpsilonIndexShiny", tags$img(height = 200, src = "epsilonIndex logo.png", style="float:right")),
    tags$p(style="font-family:Avenir", tags$i(class="fab fa-r-project", title="R Project"),"Shiny App by", tags$a(href="https://globalecologyflinders.com/people/#CJAB", "Corey Bradshaw "),
           tags$a(href = "mailto:corey.bradshaw@flinders.edu.au","(",tags$i(class="far fa-envelope"),"e-mail"),";",
           tags$a(href = "https://github.com/cjabradshaw", tags$i(class="fab fa-github",title="Github"),"Github)")),
    tags$h4(style="font-family:Avenir", "Preamble"),
    tags$p(style="font-family:Avenir", "Existing citation-based indices used to rank research performance do not permit a fair comparison 
           of researchers among career stages or disciplines, nor do they treat women and men equally. We designed 
           the ε-index, which is simple to calculate, is based on open-access data, corrects for disciplinary variation, 
           can be adjusted for career breaks (see item #4 in the 'index variants: explainer' tab), and sets a sample-specific threshold above and below which a researcher 
           is deemed to be performing above or below expectation. This", tags$i(class="fab fa-r-project"), "Shiny App estimates the ε-index and its variants 
           using user-provided data files. Note that a file containing multiple individuals (> 5 is best) is required for the ε-index
           to be calculated — it only makes sense as a relative index (see more instructions for comparing among disciplines in the 'index variants: explainer' tab). This", tags$i(class="fab fa-github"), "Github ",
           tags$a(href = "https://github.com/cjabradshaw/EpsilonIndexShiny", "repository"),
           "provides all the 'under-the-bonnet'",tags$i(class="fab fa-r-project"),"code for the app. Read the related",
           tags$a(href="http://doi.org/10.1371/journal.pone.0257141", "paper", tags$i(class="far fa-file")),
           " and/or", tags$a(href="https://conservationbytes.com/2020/11/09/the-ε-index-app-a-fairer-way-to-rank-researchers-with-citation-data/",
                                        "blog post", tags$i(class="fas fa-blog"), ".")),
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
  ), # end wellPanel
  
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
                         mainPanel(style = "background: MintCream",
                           
                           fluidRow(
                             tags$div(id = "firstOutput", 
                                      h3("input data"),
                                      add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                                      dataTableOutput("table1")) 
                           ),
                           
                           fluidRow(
                             tags$div(id = "placeholder"), # the dynamic UI will be inserted relative to this placeholder
                             add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                             
                           ),
                           
                         ) # close mainPanel
                         
                       ) # sidebarLayout
              ), # end tab1
              
              tabPanel(value="tab2", title=tags$strong("rank plots"), style = "background: MintCream",
                       
                       tags$br(),
                       tags$p(style="font-family:Avenir", "The following plot(s) show the relative index and ranks for the entire sample pooled, and (if gender
                              split selected) for the gender-debiased ranking:"),
                       
                       mainPanel(
                         tags$br(),
                         add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                         plotOutput(height="1000px","rankPlots")
                       ) # end mainPanel
                       
              ), # end tab2
              
              tabPanel(value="tab3", title=tags$strong("citation mass", tags$em("vs."), "yrsP"), style = "background: MintCream",
                       
                       tags$br(),
                       tags$p(style="font-family:Avenir", "The following plots show the relationship between log citation mass and log years publishing:"),
                       tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A: pooled citation mass", tags$em("versus"), "years publishing"))),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("B: gender-debiased citation mass", tags$em("versus"), "years publishing
                                                                                      (if gender split selected)")))
                       ), # end ul
                       
                       mainPanel(
                         tags$br(),
                         htmlOutput('ERMY'),
                         htmlOutput('ERMsY'),
                         tags$head(tags$style("#ERMY{font-family:Avenir}"
                         )),
                         tags$head(tags$style("#ERMsY{font-family:Avenir}"
                         )),
                         tags$br(),
                         tags$p(style="font-family:Avenir","The linear trend is indicated by the dashed red line."),
                         tags$br(),
                         add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                         plotOutput(height="800px", width="150%", "cMYPlots")
                       ) # end mainPanel
                       
              ), # end tab3
              
              tabPanel(value="tab4", title=tags$strong("ε-index", tags$em("vs."), "m-quotient"), style = "background: MintCream",
                       
                         tags$br(),
                         tags$p(style="font-family:Avenir", "The following plots show the relationship between the ε-index variants and the m-quotient:"),
                         tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A: ε-index", tags$em("versus"), "m-quotient"))),
                                 tags$li(tags$p(style="font-family:Avenir", tags$strong("B: gender-debiased ε-index", tags$em("versus"), "m-quotient (visible only if
                                                                                        'gender-split' activated)")))
                         ), # end ul
                         
                         mainPanel(
                           tags$br(),
                           tags$p(style="font-family:Avenir","In each panel below, the loess trend is indicated by the blue line."),
                           tags$br(),
                           add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                           plotOutput(height="800px", width="150%", "EmPlots")
                         ) # end mainPanel 
              ), # end tab4

              tabPanel(value="tab5", title=tags$strong("gender-debiased ε-index", tags$em("vs."), "ε-index"), style = "background: MintCream",
                       
                       tags$br(),
                       tags$p(style="font-family:Avenir", "The following plot shows the relationship between the gender-debiased ε-index rank and the ε-index
                               rank (if gender split selected):"),
                       
                       mainPanel(
                         tags$br(),
                         tags$p(style="font-family:Avenir","The 1:1", tags$em("y"), "~", tags$em("x"), "relationship is indicated by the black dashed line."),
                         tags$br(),
                         add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                         plotOutput(height="800px", width="150%", "EEPlots")
                       ) # end mainPanel
                       
              ), # end tab5
              
              tabPanel(value="tab6", title=tags$strong("input/output column descriptors"), style = "background: MintCream",
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
                       tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("firstyrpub")," — the year of the researcher's first published peer-reviewed paper (for instructions on how to adjust this value for career breaks, see item #4 in the 'index variants: explainer' tab)"))),
                       
                       tags$h3(style="font-family:Avenir", "ε-index output"),
                       tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("ID")," — researcher ID")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("gen")," — researcher's gender (F or M)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("yrsP")," — number of years since first peer-reviewed paper")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("cM")," — citation mass (or, cMs = normalised citation mass if you select gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("genE")," — ε-index relative to others of the same gender in the sample (not included if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("exp")," — whether above or below expectation based on chosen index (default is 'e' = pooled index)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 7"),": ", tags$em("m")," — h-index ÷ yrsP")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 8"),": ", tags$em("h")," — h-index")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 9"),": ", tags$em("debEP")," — scaled genE (gender ε′-index) (not included if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 10"),": ", tags$em("genRnk")," — rank from gender.eindex (1 = highest) (not included if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 11"),": ", tags$em("debRnk")," — gender-debiased rank (1 = highest) (not included if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 12"),": ", tags$em("cM")," — citation mass (COLUMN 4 if you select no gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 13"),": ", tags$em("poolE")," — ε-index generated from the entire sample (not gender-specific) (COLUMN 4 if you select gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 14"),": ", tags$em("eP")," — scaled poolE (ε′-index) (COLUMN 9 if you select a gender split)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 15"),": ", tags$em("poolRnk")," — rank from poolE (1 = highest) (COLUMN 8 if you select a gender split)")),
                               
                       tags$br(),
                       tags$li(tags$p(style="font-family:Avenir", tags$em("if sort index = 'ε′-index'")),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 16"),": ", tags$em("ePRnk")," — rank from ε′-index")),
                       tags$br(),
                       tags$a(href="https://github.com/cjabradshaw/EpsilonIndexShiny/blob/main/LICENSE", tags$img(height = 50, src = "GNU GPL3.png", style="float:right", title="GNU General Public Licence v3.0")),
                       tags$li(tags$p(style="font-family:Avenir", tags$em("if sort index = 'gender-debiased ε′-index'")),
                       tags$p(style="font-family:Avenir", tags$strong("COLUMN 16"),": ", tags$em("ePddebRnk")," — rank from gender ε′-index"))),
                       
                       tags$br()
              ), # end tab6

              tabPanel(value="tab7", title=tags$strong("index variants: explainer"), style = "background: MintCream",
                       tags$h2(style="font-family:Avenir", "Description of the four variants of the ε-index"),
                       tags$a(href="https://flinders.edu.au/", tags$img(height = 100, src = "F_V_CMYK.png", style="float:right",title="Flinders University")),
                       tags$ol(tags$li(tags$u(tags$p(style="font-family:Avenir", tags$strong("ε-index"), "(COLUMN", tags$em("poolE"),") — the base index not taking gender into account, or normalisation")),
                                       tags$p(style="font-family:Avenir", "The base ε-index is the residual of the linear relationship between a researcher's citation mass (",tags$em("A"),
                                              tags$sub("rel"), ") and the log", tags$sub(tags$em("e")), "of the number of years publishing (", tags$em("t"), ")."),
                                       tags$p(style="font-family:Avenir", "The citation mass (",tags$em("A"),tags$sub("rel"),") is the area under the log",tags$sub(tags$em("e")), "-log",tags$sub(tags$em("e")), "relationship between",
                                              tags$em("y"), "= [i",tags$sub("10"),",", tags$em("h"), ", 1] and", tags$em("x"), "= [10,",tags$em("h"),",", tags$em("c"),tags$sub("m"), "],"),
                                       tags$p(style="font-family:Avenir", "where i",tags$sub("10"), "= i", tags$sub("10"), "index,", tags$em("h"), "=", tags$em("h"),
                                              "-index, and", tags$em("c"),tags$sub("m"), "= number of citations of the researcher’s most highly cited paper."),
                                       tags$p(style="font-family:Avenir", "These data can be founded for any researcher listed on", tags$a(href="http://scholar.google.com", "Google Scholar"), ".")),
                               tags$a(href="https://epicaustralia.org.au/", tags$img(height = 150, src = "CABAHlogo.png",
                                                                                     style="float:right", title="ARC Centre of Excellence for Australian Biodiversity and Heritage")),
                               tags$li(tags$u(tags$p(style="font-family:Avenir", tags$strong("gender-debiased ε-index"), "(COLUMN", tags$em("genE"),") — the base index corrected for gender bias, but not normalised")),
                                       tags$p(style="font-family:Avenir", "Here, researchers are split into each main gender (F = women; M = men; at this stage we have not implemented a function to treat non-binary genders separately), and then the index is calculated for each researcher relative to her/his respective group. The resulting indices are then ranked to provided a gender-debiased final rank")),
                               tags$li(tags$u(tags$p(style="font-family:Avenir", tags$strong("ε′-index"), "(COLUMN", tags$em("eP"),") — the normalised base index")),
                                       tags$p(style="font-family:Avenir","The approach to calculate the ε-index is the same, except the citation mass (", tags$em("A"),tags$sub("rel"), ") is normalised relative to the sample:"),
                                       tags$img(height = 50, src = "normaliseEq.png", style="vertical-align:middle"),
                                       tags$br(),
                                       tags$br(),
                                       tags$p(style="font-family:Avenir", "To compare researchers from different disciplines, first take a single discipline sample of researchers, calculate their discipline-specific ε′-indices, and then compare the ε′-indices to those from another sample from another discipline. So, say you have 20 researchers from DISCIPLINE A, and 20 from DISCIPLINE B. Run each discipline sample separately, and then pool the two sets of ε′-indices to compare researchers on the same scale. The app will not automatically order and plot the combined ε′ scores (you will have to do this outside the app)."),),
                               tags$br(),
                               tags$a(href="https://github.com/cjabradshaw/EpsilonIndexShiny/blob/main/LICENSE", tags$img(height = 50, src = "GNU GPL3.png", style="float:right", title="GNU General Public Licence v3.0")),
                               tags$li(tags$u(tags$p(style="font-family:Avenir", tags$strong("gender-debiased ε′-index"), "(COLUMN", tags$em("debEP"),") — the normalised index accounting for gender bias")),
                                       tags$p(style="font-family:Avenir","Following the same approach as above, but normalising for each gender separately."),
                                       tags$p(style="font-family:Avenir","N.B. To adjust for career breaks, it is necessary to adjust the", tags$em("firstyrpub"), " value in the uploaded text file for the researcher(s) in question. The following example explains how this can be done:"),
                                       tags$p(style="font-family:Avenir","Let’s say a researcher published her 1", tags$sup("st"), " paper in 2005. From then until now, if she were on a 0.6 full-time equivalent (FTE) for 3 years, 0.2 FTE for 2 years, and had 18 months maternity leave, then the effective ‘start’ year would be:"),
                                       tags$p(style="font-family:Avenir","2005 + (3 - 1.8) + (2 - 0.4) + 1.5 ≅ 2009 (i.e., rounded to nearest calendar year).")),                               
                               ),
                       tags$br(),
                       tags$p(style="font-family:Avenir","For more information, read the original",tags$a(href="http://doi.org/10.1371/journal.pone.0257141", "paper.")),
                       tags$br()
                       
              ) # end tab7
  ) # end tabsetPanel
  
) # close fluidPage


server <- function(input, output, session) {
  
  observeEvent(input$file1, {
    
    if(input$tabs == "tab1"){
      
      output$table1 <- renderDataTable({
        file_to_read = input$file1
        if(is.null(file_to_read)){
          return()
        }
        read.table(file_to_read$datapath, sep=input$sep, header=input$header1, quote=NULL)
      }) # end output table1
      
      datin <- reactive({
        fileinp <- input$file1
        if(is.null(fileinp)){return()}
        inpdat <- data.frame(read.table(fileinp$datapath, sep=input$sep, header = input$header1, quote=NULL))
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
    
  }) # end Events

  observeEvent(input$tabs, {
    
    if(input$tabs == "tab2"){
      
      if(input$bygender == "no") {

        output$rankPlots <- renderPlot({
          input$rankPlots
          
          Ctheme1 = theme(
            axis.title.x = element_text(size = 14),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          AllRank <- ggplot(results, aes(y = poolE, x = 1)) + 
            geom_jitter(position = position_jitter(height = 0, width = 0.0), show.legend = F) +
            geom_hline(yintercept=0, linetype=3, color="black", size=0.5) +
            labs(x='ALL', y="ε-index", color="gender", show.legend = F) +
            geom_label_repel(aes(label = paste(poolRnk,"-",ID,sep="")),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            scale_radius(c(0.6,0.6)) +
            Ctheme1
          
          ggarrange(AllRank,
                    labels=NULL,
                    ncol=1, nrow=1)
        })
      } # end if
      
      if(input$bygender == "yes") {
 
        output$rankPlots <- renderPlot({
          input$rankPlots
          
          Ctheme1 = theme(
            axis.title.x = element_text(size = 14),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          AllRank <- ggplot(results, aes(y = poolE, x = 1)) + 
            geom_jitter(position = position_jitter(height = 0, width = 0.0), show.legend = F) +
            geom_hline(yintercept=0, linetype=3, color="black", size=0.5) +
            labs(x='ALL', y="ε-index", color="gender", show.legend = F) +
            geom_label_repel(aes(label = paste(poolRnk,"-",ID,sep="")),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            scale_radius(c(0.6,0.6)) +
            Ctheme1
          
          Ctheme2 = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          genRank <- ggplot(results, aes(y = genE, x = gen, col = gen, show.legend = F)) + 
            geom_jitter(position = position_jitter(height = 0, width = 0.0), show.legend = F) +
            geom_hline(yintercept=0, linetype=3, color="black", size=0.5, show.legend = F) +
            labs(x=NULL, y="gender-debiased ε-index", color="gender", show.legend = F) +
            geom_label_repel(aes(label = paste(debRnk,"-",ID,sep="")),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            scale_radius(c(0.6,0.6)) +
            scale_colour_manual(values = c("black", "red")) +
            Ctheme2
          
          ggarrange(AllRank, genRank,
                    labels=NULL,
                    ncol=2, nrow=1)
        })
      } # end if
      
    } # end if for tab2
    
    
    
    if(input$tabs == "tab3"){

      if(input$bygender == "no") {
        output$ERMY <- renderText({
          massdat1 <- data.frame(log(results$yrsP), log(results$cM))
          massdat <- na.omit(do.call(data.frame,lapply(massdat1,function(x) replace(x, is.infinite(x), NA))))
          ER <- linregER(massdat[,1], massdat[,2])[1]
          ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
          paste("A. evidence ratio = ", ERf,";",
                " R<sup>2</sup>",  " = ",round(linregER(massdat[,1], massdat[,2])[2], 3),sep="")
        })
        
        output$cMYPlots <- renderPlot({
          input$cMYPlots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          massdat1 <- data.frame(results$ID, results$gen, log(results$yrsP), log(results$cM))
          massdat2 <- na.omit(do.call(data.frame,lapply(massdat1,function(x) replace(x, is.infinite(x), NA))))
          fit <- lm(massdat2[,4] ~ massdat2[,3])
          massdat <- data.frame(massdat2, fit$fitted.values)
          colnames(massdat) <- c("ID", "gen","lyrsP", "lcM", "pred")
          
          cMY <- ggplot(data=massdat, aes(x=lyrsP, y=lcM)) + 
            geom_segment(aes(xend = lyrsP, yend = pred), linetype ="dashed", colour="dark grey") +
            geom_point(aes(color=factor(gen))) +
            scale_colour_manual(values = c("black", "red")) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="red") +
            labs(x="log years publishing", y="log citation mass", color="gender") +
            geom_label_repel(aes(label = ID),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            Ctheme
          
          ggarrange(cMY,
                    labels=NULL,
                    ncol=1, nrow=1)
        })
      } # end if
      
      if(input$bygender == "yes") {
        output$ERMY <- renderText({
          massdat1 <- data.frame(log(results$yrsP), log(results$cM))
          massdat <- na.omit(do.call(data.frame,lapply(massdat1,function(x) replace(x, is.infinite(x), NA))))
          ER <- linregER(massdat[,1], massdat[,2])[1]
          ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
          paste("A. evidence ratio = ", ERf,";",
                " R<sup>2</sup>",  " = ",round(linregER(massdat[,1], massdat[,2])[2], 3),sep="")
        })
        output$ERMsY <- renderText({
          massdat1 <- data.frame(log(results$yrsP), log(results$cMs))
          massdat <- na.omit(do.call(data.frame,lapply(massdat1,function(x) replace(x, is.infinite(x), NA))))
          ER <- linregER(massdat[,1], massdat[,2])[1]
          ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
          paste("B. evidence ratio = ", ERf,";",
                " R<sup>2</sup>",  " = ",round(linregER(massdat[,1], massdat[,2])[2], 3),sep="")
        })
        
        output$cMYPlots <- renderPlot({
          input$cMYPlots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          massdat1 <- data.frame(results$ID, results$gen, log(results$yrsP), log(results$cM))
          massdat2 <- na.omit(do.call(data.frame,lapply(massdat1,function(x) replace(x, is.infinite(x), NA))))
          fit <- lm(massdat2[,4] ~ massdat2[,3])
          massdat <- data.frame(massdat2, fit$fitted.values)
          colnames(massdat) <- c("ID", "gen","lyrsP", "lcM", "pred")
          
          cMY <- ggplot(data=massdat, aes(x=lyrsP, y=lcM)) + 
            geom_segment(aes(xend = lyrsP, yend = pred), linetype ="dashed", colour="dark grey") +
            geom_point(aes(color=factor(gen))) +
            scale_colour_manual(values = c("black", "red")) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="red") +
            labs(x=NULL, y="log citation mass", color="gender") +
            geom_label_repel(aes(label = ID),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            Ctheme

          massdat3 <- data.frame(results$ID, results$gen, log(results$yrsP), log(results$cMs))
          massdat4 <- na.omit(do.call(data.frame,lapply(massdat3,function(x) replace(x, is.infinite(x), NA))))
          fit2 <- lm(massdat4[,4] ~ massdat4[,3])
          massdt <- data.frame(massdat4, fit2$fitted.values)
          colnames(massdt) <- c("ID", "gen","lyrsP", "lcMs", "pred")
          
          cMsY <- ggplot(data=massdt, aes(x=lyrsP, y=lcMs)) + 
            geom_segment(aes(xend = lyrsP, yend = pred), linetype ="dashed", colour="dark grey") +
            geom_point(aes(color=factor(gen))) +
            scale_colour_manual(values = c("black", "red")) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="red") +
            labs(x="log years publishing", y="log gender-debiased citation mass", color="gender") +
            geom_label_repel(aes(label = ID),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            Ctheme
          
          ggarrange(cMY, cMsY,
                    labels=NULL,
                    ncol=1, nrow=2)
        })
      } # end if
      
    } # end if for tab3
    
    if(input$tabs == "tab4"){
      
        output$EmPlots <- renderPlot({
          input$EmPlots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          mpoolE <- ggplot(data=results, aes(x=m, y=poolE)) + 
            geom_point(aes(color=factor(gen))) +
            geom_hline(yintercept=0, linetype=3, color="black", size=0.5) +
            scale_colour_manual(values = c("black", "red")) +
            geom_smooth() +
            labs(x=NULL, y="ε-index", color="gender") +
            geom_label_repel(aes(label = ID),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            Ctheme
            
          mgenE <- ggplot(data=results, aes(x=m, y=genE)) + 
            geom_point(aes(color=factor(gen))) +
            geom_hline(yintercept=0, linetype=3, color="black", size=0.5) +
            scale_colour_manual(values = c("black", "red")) +
            geom_smooth() +
            labs(x="m-quotient", y="gender-debiased ε-index", color="gender") +
            geom_label_repel(aes(label = ID),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            Ctheme
            
          ggarrange(mpoolE, mgenE,
                    labels=c("A", "B"),
                    ncol=1, nrow=2)
          })
      
      if (input$bygender == "no") {

        output$EmPlots <- renderPlot({
          input$EmPlots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          mpoolE <- ggplot(data=results, aes(x=m, y=poolE)) +
            geom_point(aes(color=factor(gen))) +
            geom_hline(yintercept=0, linetype=3, color="black", size=0.5) +
            scale_colour_manual(values = c("black", "red")) +
            geom_smooth() +
            labs(x='m-quotient', y="ε-index", color="gender") +
            geom_label_repel(aes(label = ID),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            Ctheme
            
          ggarrange(mpoolE,
                    labels=c("A"),
                    ncol=1, nrow=1)
        })
      } # end if
      
    } # end if for tab4

    if(input$tabs == "tab5"){
      
      if (input$bygender == "yes") {
        
        output$EEPlots <- renderPlot({
          input$EEPlots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16))
          
          EE <- ggplot(data=results, aes(x=poolRnk, y=debRnk)) + 
            geom_point(aes(color=factor(gen))) +
            scale_colour_manual(values = c("black", "red")) +
            geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
            labs(x="ε-index rank", y="gender-debiased ε-index rank", color="gender") +
            geom_label_repel(aes(label = ID),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            Ctheme
            
          ggarrange(EE,
                    labels=NULL,
                    ncol=1, nrow=1)
        })
      } # end if
      
    } # end if for tab5
    
  }) # end Events
  
  session$onSessionEnded(stopApp)
  
} # end server

shinyApp(ui, server)
