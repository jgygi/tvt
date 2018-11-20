library(RColorBrewer)
library("Cairo")
library("httr")
library("readr")
library("stringr")
library("magrittr")
library("ggplot2")
library("rowr")
library("DT")
library("jsonlite")
library("beeswarm")
library("scatterD3")
library("shinythemes")
library("shinyWidgets")
library("shinyBS")
library("sfsmisc")
source("gfy_api.R")

ui <- fluidPage(
  #Tags
  tags$style(type="text/css", "
           #loadmessage {
             position: fixed;
             top: 330px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 500%;
             color: snow;
             background-color: #787878;
             z-index: 105;
           }
           #loadmessage2 {
             position: fixed;
             top: 330px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 500%;
             color: snow;
             background-color: #787878;
             z-index: 105;
           }
  "),
  navbarPage("",
      tabPanel("Home",
        textOutput("downloadProteinStatus"),
        textOutput("filterProteinStatus"),
        textOutput("downloadChromaStatus"),
        textOutput("downloadPepStatus"),
        textOutput("downloadStatStatus"),
        textOutput("downloadStatus"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 tags$div("Loading...",id="loadmessage")
                 ),
        progressBar(id = "progressBar1", value = 0),
        fluidRow(
          column(12, align="center", span(htmlOutput(outputId = "homeTitle"), style="font-size: 40px"))
        ),
        tags$h2("Please wait while the data is being pulled:", style="text-align: center; font-size: 24px"),
        tags$h2(paste("This could take as long as 30-45 seconds", sep=""), style="text-align: center; font-size: 18px"),
        column(12,span(htmlOutput(outputId = "searchInformation"), style="font-size: 20px")),
        tags$hr(),
        tags$h2("Welcome! Please select an option below:", style="text-align: center; font-size: 24px"),
        fluidRow(
          column(6, align="right", actionButton('showTKOModal', 'TKO Proteins', icon("flask"), class = "tko")),
            tags$head(tags$style(".tko{color: #fff;} .tko{font-family: Arial}.tko{background-color: #4B966E;} .tko{border-color: #4B966E;} .tko{height: 100px;}
                                .tko{width: 300px;} .tko{font-size:150%}"),
                      tags$style(".tkoSelect{color: #fff;} .tkoSelect{font-family: Arial}.tkoSelect{background-color: #4B966E;} .tkoSelect{border-color: #4B966E;} .tkoSelect{height: 50px;}
                                .tkoSelect{width: 250px;} .tkoSelect{font-size:100%}"),
                      tags$style(".reg{color: #fff;} .reg{font-family: Arial}.reg{background-color: #B92938;} .reg{border-color: #B92938;} .reg{height: 100px;}
                                .reg{width: 300px;} .reg{font-size:150%}"),
                      tags$style(".regSelect{color: #fff;} .regSelect{font-family: Arial}.regSelect{background-color: #B92938;} .regSelect{border-color: #B92938;} .regSelect{height: 50px;}
                                .regSelect{width: 250px;} .regSelect{font-size:100%}"),
                      tags$style(".inst{color: #fff;} .inst{font-family: Arial}.inst{background-color: #2980B9;} .inst{border-color: #2980B9;} .inst{height: 100px;}
                                .inst{width: 300px;} .inst{font-size:150%}"),
                      tags$style(".instSelect{color: #fff;} .instSelect{font-family: Arial}.instSelect{background-color: #2980B9;} .instSelect{border-color: #2980B9;} .instSelect{height: 50px;}
                                .instSelect{width: 250px;} .instSelect{font-size:100%}"),
                      tags$style(".syst{color: #fff;} .syst{font-family: Arial}.syst{background-color: #AA29B9;} .syst{border-color: #AA29B9;} .syst{height: 100px;}
                                .syst{width: 300px;} .syst{font-size:150%}"),
                      tags$style(".systSelect{color: #fff;} .systSelect{font-family: Arial}.systSelect{background-color: #AA29B9;} .systSelect{border-color: #AA29B9;} .systSelect{height: 50px;}
                                .systSelect{width: 250px;} .systSelect{font-size:100%}")),
          column(6, align="left", actionButton('showNonChangingModal', 'Unchanging Prot.', icon("flask"), class = "reg"))
        ),
        br(),
        fluidRow(
          column(6, align="right", actionButton('showInstrumentModal', 'Instrument Stats', icon("signal"), class = "inst")),
          column(6, align="left", actionButton('showSystemModal', 'Settings / Help', icon("cogs"), class = "syst"))
        ),
        tags$hr(),
        fluidRow(
          column(6, align = "right", img(src='myImage.png', align = "right")),
          column(6, align = "left", img(src='ThermoImage.png', align = "left", width = 415, height = 100))
        ),
        bsModal("tkoModal", "Select an option for TKO:", "", size = "small",
          fluidRow(
            column(12, align="center", actionButton("goToTKOTable", "DataTable", icon("table"), class = "tkoSelect"))
          ),
          br(),
          fluidRow(
            column(12, align="center", actionButton("goToTKOGraph", "TMT Graphs", icon("signal"), class = "tkoSelect"))
          ),
          br(),
          fluidRow(
            column(12, align="center", actionButton("goToIFI", "IFI Graph", icon("sliders"), class = "tkoSelect"))
          )
        ),
        bsModal("regModal", "Select an option for Unchanging Proteins:", "", size = "small",
          fluidRow(
            column(12, align="center", actionButton("goToRegTable", "DataTable", icon("table"), class = "regSelect"))
          ),
          br(),
          fluidRow(
            column(12, align="center", actionButton("goToRegGraph", "TMT Graphs", icon("signal"), class = "regSelect"))
          )
        ),
        bsModal("instModal", "Select an option for Instrument Performance:", "", size = "small",
          fluidRow(
            column(12, align="center", actionButton("goToInstrumentStats", "Instrument Statistics", icon("list"), class = "instSelect"))
          ),
          br(),
          fluidRow(
            column(12, align="center", actionButton("goToComparison", "Instrument Comparison", icon("search-plus"), class = "instSelect"))
          )
        ),
        bsModal("systModal", "Select an option for Settings / Help:", "", size = "small",
          fluidRow(
            column(12, align="center", actionButton("goToSettings", "Settings", icon("cog"), class = "systSelect"))
          ),
          br(),
          fluidRow(
            column(12, align="center", actionButton("goToHelp", "Help", icon("question"), class = "systSelect"))
          )
        ),
        fluidRow(
            column(4, offset = 4, align = "center", uiOutput("progressUI"))
        )
      ),
      navbarMenu("TKO Proteins",
        tabPanel("KO Peptide Table",
            fluidRow(
              column(4, checkboxInput("onlyshowtko", "Only Show TKO's?:", value = TRUE)),
              column(4, checkboxInput("convertToPercentage1", "Scale to Percentages (each peptide)"))
            ),
            tableOutput("quicksummarytable"),
            dataTableOutput("peptidetableout")),
        tabPanel("KO Graphs",
          #output
            fluidRow(
              plotOutput("proteinplot1")
              ),
            fluidRow(
              plotOutput("proteinplot2")
              ),
            fluidRow(
              plotOutput("proteinplot3")
              ),
            plotOutput("peptideplot")),
        tabPanel("Interactive Graph (IFI)",
          fluidRow(
            column(4, numericInput("protcutoff", "Interference-Free Index (IFI) Cut-Off:", value = .95, step = .05)),
            column(4, checkboxInput("interylim","Zoom? (y=0.9)", value = FALSE))
            ),
            fluidRow(
              column(4, selectInput(inputId = "colorGood", label = "Cut-Off Color - Good:",
                  c("Blue" = "varA",
                    "Green" = "varB",
                    "Yellow" = "varC",
                    "Orange" = "varD",
                    "Red" = "varE",
                    "Purple" = "varF",
                    "Black" = "varG"
                    ),multiple = FALSE, selected = "varA")),
              column(4, selectInput(inputId = "colorBad", label = "Cut-Off Color - Bad:",
                  c("Blue" = "varA",
                    "Green" = "varB",
                    "Yellow" = "varC",
                    "Orange" = "varD",
                    "Red" = "varE",
                    "Purple" = "varF",
                    "Black" = "varG"
                    ),multiple = FALSE, selected = "varE")),
              column(4, downloadButton("downloadIFIData", "Download as CSV"))
              ),
              fluidRow(scatterD3Output("interactiveplot")),
              fluidRow(column(10, offset = 1, span(htmlOutput("interactiveplotnote"), style = "font-size: 14px")))
        )
      ),
      navbarMenu("Unchanging Proteins",
        tabPanel("Unchanging Peptide Table",
            fluidRow(
            column(4, checkboxInput("onlyshowreg", "Only Show Unchanging Protein's?:", value = TRUE)),
            column(4, checkboxInput("convertToPercentage2", "Scale to Percentages (each peptide)"))
            ),
            tableOutput("quicksummarytable2"),
            dataTableOutput("peptidetableout2")),
        tabPanel("Unchanging Graphs",
            fluidRow(
              plotOutput("proteinplot4")
              ),
            fluidRow(
              plotOutput("proteinplot5")
              ),
            fluidRow(
              plotOutput("proteinplot6")
              ),
            plotOutput("peptideplot2")
          )
      ),
      navbarMenu("Instrument Performance",
        tabPanel("Performance Stats",
            fluidRow(
              column(7, tableOutput("summaryStatsTable")),
              column(4, tableOutput("statsTable"))
            ),
            fluidRow(
              column(12, tableOutput("stats2Table"))
              ),
            fluidRow(
              column(12, tableOutput("stats3Table"))
              ),
            fluidRow(
              column(6, plotOutput("peakwidthGraph")),
              column(6, plotOutput("ppmGraph"))
            ),
            fluidRow(
              column(12, plotOutput("chromaTable"))
            ),
            fluidRow(
              column(12, plotOutput("ms2persecondTable"))
            ),
            fluidRow(
              column(6, plotOutput("xcorrGraph")),
              column(6, plotOutput("sumsnGraph"))
              ),
            fluidRow(
              column(6, plotOutput("ms2ioninjectiontimeGraph")),
              column(6, plotOutput("ms3ioninjectiontimeGraph"))
              ),
            fluidRow(
              column(6, plotOutput("precursormaxGraph")),
              column(6, plotOutput("difscoreGraph"))
              ),
            fluidRow(
              column(6, plotOutput("missedcleavagesGraph")),
              column(6, plotOutput("isospecGraph"))
            ),
            column(12, plotOutput("ppmvsrtGraph"))
        ),
        tabPanel("Performance Comparison",
          #Inputs:
          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 tags$div("Loading...",id="loadmessage2")
                 ),
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "comparisonTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(12, span(htmlOutput(outputId = "comparisonNote"), style="font-size: 20pxpx"))
          ),
          fluidRow(
            column(3, textInput("prevSID", "", value = "", placeholder = "Type Search ID here:"))
          ),
          column(3, align = "left",  actionBttn("updateComparison", "Confirm", color="success", style = "simple", size = "lg")),
          br(),
          br(),
          fluidRow(
            column(12, span(htmlOutput(outputId = "comparisonWarning"), style="font-size: 20pxpx"))
          ),
          tags$hr(),
          #Peak Width:
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "pwTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("peakWidthGraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("peakWidthTableCompare"))
          ),
          tags$hr(),
          #PPM:
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "ppmTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("ppmGraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("ppmTableCompare"))
          ),
          tags$hr(),
          #XCorr:
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "xcTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("xcorrGraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("xcorrTableCompare"))
          ),
          tags$hr(),
          #SumSN:
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "ssnTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("ssnGraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("ssnTableCompare"))
          ),
          tags$hr(),
          #MS2InjTime
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "m2Title"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("ms2GraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("ms2TableCompare"))
          ),
          tags$hr(),
          #MS3InjTime
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "m3Title"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("ms3GraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("ms3TableCompare"))
          ),
          tags$hr(),
          #PrecMaxIntensity
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "pmiTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("pmGraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("pmTableCompare"))
          ),
          tags$hr(),
          #DiffScore
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "dsTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("dsGraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("dsTableCompare"))
          ),
          tags$hr(),
          #MissedCleavages
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "mcTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            #column(7, plotOutput("mcGraphCompare")),
            column(6, align = 'center', offset = 3, br(), br(), br(), br(), tableOutput("mcTableCompare"))
          ),
          tags$hr(),
          #IsolationSpecificity
          fluidRow(
            column(12, align="center", span(htmlOutput(outputId = "isTitle"), style="font-size: 40px"))
          ),
          fluidRow(
            column(7, plotOutput("isGraphCompare")),
            column(5, br(), br(), br(), br(), tableOutput("isTableCompare"))
          ),
          tags$hr(),
          tags$h2("Options:", style="text-align: center; font-size: 24px"),
          checkboxInput("showOutliers", "Show Outliers?", value=FALSE)
        )
      ),
      navbarMenu("Settings / Help",
        tabPanel("Settings",
            span(textOutput("settingsHeader"), style="font-size: 18px"),
            fluidRow(
              column(3, sliderInput("isoCutOff", "Isolation Spec. Cut Off:", min = 0, max = 1, step = .05, value = .8)),
              column(3, numericInput("ssCutOff", "Sum Signal Cut Off:", min = 0, value = 200))
            ),
            fluidRow(
              actionBttn("changeFilter", "Click To Confirm Settings", size = "lg", color = "success", style = "simple")
            ),
            tags$hr(),
            #input
            fluidRow(
                column(2, textInput("proteinchoice1", "KO Protein 1:", value = "MET6")),
                column(4, textOutput("proteinchoice1output"))
            ),#end fluidRow
            fluidRow(
                column(2, textInput("proteinchoice2", "KO Protein 2:", value = "HIS4")),
                column(4, textOutput("proteinchoice2output"))
            ),#end fluidRow
            fluidRow(
                column(2, textInput("proteinchoice3", "KO Protein 3:", value = "URA2")),
                column(4, textOutput("proteinchoice3output"))
            ),#end fluidRow
            fluidRow(
                column(2, textInput("proteinchoice4", "Unchanging Prot. 1:", value = "ENO2")),
                column(4, textOutput("proteinchoice4output"))
            ),#end fluidRow
            fluidRow(
                column(2, textInput("proteinchoice5", "Unchanging Prot. 2:", value = "PGK1")),
                column(4, textOutput("proteinchoice5output"))
            ),#end fluidRow
            fluidRow(
                column(2, textInput("proteinchoice6", "Unchanging Prot. 3:", value = "RPL10")),
                column(4, textOutput("proteinchoice6output"))
            ),#end fluidRow
            span(textOutput("settings1"), style="font-size: 18px"),
            #input
            fluidRow(
              column(4, selectInput(inputId = "colorType", label = "Protein Level Color Scheme:",
                  c("Accent" = "varA",
                    "Basic" = "varB",
                    "Dark" = "varC",
                    "Pastel" = "varG",
                    "Blue" = "varD",
                    "Green" = "varF",
                    "Red" = "varE"
                    ),multiple = FALSE, selected = "varB")),
                    column(2,checkboxInput(inputId = "blackback2", label = "Invert BG?", value = FALSE))
                  ),#endfluidRow
                  plotOutput("testprotplot"),
            span(textOutput("settings2"), style="font-size: 18px"),
            fluidRow(
              column(4, selectInput(inputId = "colorTypePeptide", label = "Peptide Graph Color Scheme:",
                  c("Accent" = "varA",
                    "Basic" = "varB",
                    "Dark" = "varC",
                    "Pastel" = "varG",
                    "Blue" = "varD",
                    "Green" = "varF",
                    "Red" = "varE"
                    ),multiple = FALSE, selected = "varB")),
                    column(4, selectInput(inputId = "methodchoice", label = "Cluster Method:", choices =
                              c("Swarm" = "swarm",
                              "Center" = "center",
                              "Hex" = "hex",
                              "Square" = "square")
                              )),
                column(2,numericInput(inputId = "pointsize", label = "Point Size:", value = 3, step = .5)),
                column(2,checkboxInput(inputId = "pointshape", label = "Fill Circles?", value = TRUE)),
                column(2,checkboxInput(inputId = "blackback", label = "Invert BG?", value = FALSE))
                  ),#endfluidRow
                  fluidRow(
                    column(4, selectInput(inputId = "kolocations1", label = "KO Prot.1 Channels", choices =
                    c("1-3" = "varA",
                      "4-6" = "varB",
                      "7-9" = "varC"),
                      selected = "varA"))
                    ,
                    column(4, selectInput(inputId = "kolocations2", label = "KO Prot.2 Channels", choices =
                    c("1-3" = "varA",
                      "4-6" = "varB",
                      "7-9" = "varC"),
                      selected = "varB"))
                    ,
                    column(4, selectInput(inputId = "kolocations3", label = "KO Prot.3 Channels", choices =
                    c("1-3" = "varA",
                      "4-6" = "varB",
                      "7-9" = "varC"),
                      selected = "varC"))
                  ),
                  plotOutput("peptidetestplot")
                  
          ),
        tabPanel("Help",
              fluidRow(
                textOutput("pqid"),
                textOutput("sid")
              ),
              fluidRow(
                column(9, offset = 2, span(textOutput(outputId = "titleheader"), style="font-size: 22px; color:black;"))
                ),
            fluidRow(
              column(6, offset = 2, span(textOutput(outputId = "introtextoutheader"), style="font-size: 22px; color:orange;"))
              ),
            fluidRow(
              column(9, offset = 2, span(textOutput(outputId = "introtextout"), style="font-size: 14px"))
            ),
            fluidRow(
              column(9, offset = 2, helpText(a("Link to TKO Paper (https://www.ncbi.nlm.nih.gov/pubmed/27400695)", href="https://www.ncbi.nlm.nih.gov/pubmed/27400695")))
              ),
            fluidRow(
              column(12, img(src='ExampleImage.png', align = "center", width = 800, height = 400), align = "center")
              ),
            br(),
            fluidRow(
              column(12, img(src='ifi9plex.png', align = "center", width = 650, height = 150), align = "center")
              ),
            br(),
            fluidRow(
              column(12, img(src='ifi11plex.png', align = "center", width = 650, height = 150), align = "center")
              ),
            fluidRow(
                column(9, offset = 2, span(htmlOutput(outputId = "instructionnote"), style="font-size: 11px"))
              )
          )
      ),
      #tabPanel("Tests",
      #  dataTableOutput("testTable1"),
      #  dataTableOutput("testTable2"),
      #  dataTableOutput("testTable3"),
      #  dataTableOutput("testTable4")
      #),
    id="tabs",
    collapsible=TRUE,
    theme= shinytheme("flatly"),
    footer= fluidPage(
      tags$hr(),
      fluidRow(
        column(12,span(htmlOutput(outputId = "pqidAndSid"), style="font-size: 20px"))
      ),
      br(),
      fluidRow(
        column(12,span(htmlOutput(outputId = "credits"), style="font-size: 14px"))
      )
    )
  )
)

server <- function(session, input, output) {
  pdf(NULL)
  
  #Reactive Data
  values <- reactiveValues()
  usernamein <- reactive({
    return("single_standard_module")
    })
  passwordin <- reactive({
    return("LBtqkEkRXt34InnzPjK6s3uVkMmOXnZVhbdI4aJI7eo=")
    })
  totalpepdata <- reactive({
    idstring2 <- paste("protein_quant_peptide/",values$pqid,"?all_users=1", sep="")
    #will extract information into tables
    gfy <- gfy_api("https://wren.hms.harvard.edu/gfy/www/modules/api/v1", usernamein(), passwordin())
    peptides <-  gfy.get(gfy, idstring2)
    return(data.frame(peptides))
    })
  totaldata <- reactive({
    idstring2 <- paste("peptide_view/",values$sid,"?type=peptide&all_users=1&set_bit=3", sep="")
    #will extract information into tables
    gfy <- gfy_api("https://wren.hms.harvard.edu/gfy/www/modules/api/v1/", usernamein(), passwordin())
    peptides <-  gfy.get(gfy, idstring2)
    #filter peptides for duplicates
    peptides <- as.data.frame(peptides)
    peptides <- peptides[!duplicated(peptides$scanf),]
    values$totalDataUnfiltered <- peptides
    peptides <- peptides[which(peptides$validity == "3"),]
    return(peptides)
    })
  totalcolumns <- reactive({
    return(colnames(totaldata()))
    })
  totalcolumnsUnfiltered <- reactive({
    return(colnames(values$totalDataUnfiltered))
    })

  totalchromadata <- reactive({
            idstring2 <- paste("chromato/",values$sid,"?all_users=1", sep="")
            #will extract information into tables
            gfy <- gfy_api("https://wren.hms.harvard.edu/gfy/www/modules/api/v1", usernamein(), passwordin())
            nesteddata <-  gfy.get(gfy, idstring2)
            totaldata <- t(unlist(nesteddata[1]))
            for(i in seq(2,length(nesteddata))){
                totaldata <- rbind(totaldata, t(unlist(nesteddata[i])))
            }
            df <- data.frame(totaldata)

            colnames(df) <- c("Time", "Intensity")
            return(df)
            })
  totalstatsdata <- reactive({
            idstring2 <- paste("saved_sets_stats/",values$sid,"?all_users=1", sep="")
            #will extract information into tables
            gfy <- gfy_api("https://wren.hms.harvard.edu/gfy/www/modules/api/v1", usernamein(), passwordin())
            nesteddata <-  gfy.get(gfy, idstring2, parse = "text")
            return(fromJSON(nesteddata))
            })


  #Observe / ObserveEvent
  observe({
     query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['pqid']])) {
        values$pqid <- query[['pqid']]
      }
      if (!is.null(query[['sid']])) {
         values$sid <- query[['sid']]
         updateTextInput(session, "prevSID", value = values$sid)
      }
      if (!is.null(query[['plex']])) {
         plex <- query[['plex']]
         if(plex != 9)
         {
           values$plexNum <- 11
         }
         else
         {
           values$plexNum <- 9
         }
      }
      if (is.null(query[['plex']])) {
         values$plexNum <- 11
      }
      if (is.null(query[['pqid']])) {
         values$pqid <- -404
      }
      if (is.null(query[['sid']])) {
         values$sid <- -404
         updateTextInput(session, "prevSID", value = -404)
      }
     })
  observeEvent(input$showTKOModal, {
    toggleModal(session, modalId = "tkoModal", toggle = "open")
    })
  observeEvent(input$showNonChangingModal, {
    toggleModal(session, modalId = "regModal", toggle = "open")
    })
  observeEvent(input$showInstrumentModal, {
    toggleModal(session, modalId = "instModal", toggle = "open")
    })
  observeEvent(input$showSystemModal, {
    toggleModal(session, modalId = "systModal", toggle = "open")
    })
  observeEvent(input$goToTKOTable, {
    toggleModal(session, modalId = "tkoModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "KO Peptide Table")
    })
  observeEvent(input$goToTKOGraph, {
    toggleModal(session, modalId = "tkoModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "KO Graphs")
    })
  observeEvent(input$goToIFI, {
    toggleModal(session, modalId = "tkoModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Interactive Graph (IFI)")
    })
  observeEvent(input$goToRegTable, {
    toggleModal(session, modalId = "regModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Unchanging Peptide Table")
    })
  observeEvent(input$goToRegGraph, {
    toggleModal(session, modalId = "regModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Unchanging Graphs")
    })
  observeEvent(input$goToInstrumentStats, {
    toggleModal(session, modalId = "instModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Performance Stats")
    })
  observeEvent(input$goToComparison, {
    toggleModal(session, modalId = "instModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Performance Comparison")
    })
  observeEvent(input$goToSettings, {
    toggleModal(session, modalId = "systModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
    })
  observeEvent(input$goToHelp, {
    toggleModal(session, modalId = "systModal", toggle = "close")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Help")
    })
  observeEvent(input$updateComparison, {
    if(input$prevSID == "")
    {
      showNotification("ERROR: No Search ID inputted to fetch", duration = 6)
    }
    else
    {
      showNotification("Fetching previous data, this will likely take about a minute...", duration = 15)
      values$totalgolddata <- getUpdatedGoldData()
    }
    })
  observeEvent(input$changeFilter, {
      showNotification("Changing Filter...", duration = 4)
      values$totalfilteredpepdata <- filterPepData(input$isoCutOff, input$ssCutOff)
      showNotification("Filtered Successfully!", duration = 4)
  })

  #helper functions:
  getPepDataOff <- function(){
    peptidedata <- totalpepdata()[12:22]
    names <- totalpepdata()[2]
    combinedpeptidedata <- cbind(names, peptidedata)
    #declaration of labels
    colnames(combinedpeptidedata) <- c("Protein Name:","126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
    return(combinedpeptidedata)
  }
  getFiltPepDataOff <- function(){
    peptidedata <- values$totalfilteredpepdata[12:22]
    names <- values$totalfilteredpepdata[2]
    combinedpeptidedata <- cbind(names, peptidedata)
    #declaration of labels
    colnames(combinedpeptidedata) <- c("Protein Name:","126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
    return(combinedpeptidedata)
  }
  getUniquePepDataOff <- function(){
    peptidedata <- values$totalfilteredpepdata
    peptidedata <- peptidedata[9:22]
    names <- values$totalfilteredpepdata[2]
    combinedpeptidedata <- cbind(names, peptidedata)
    #declaration of labels
    colnames(combinedpeptidedata) <- c("Gene Symbol:", "unique", "seq", "collapsed", "126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
    combinedpeptidedata <- combinedpeptidedata[which(combinedpeptidedata$unique == "U"),]
    combinedpeptidedata <- combinedpeptidedata[-c(2:4)]
    return(combinedpeptidedata)
  }
  getUniquePepSeqDataOff <- function(){
    peptidedata <- values$totalfilteredpepdata
    peptidedata <- peptidedata[9:22]
    names <- values$totalfilteredpepdata[2]
    combinedpeptidedata <- cbind(names, peptidedata)
    #declaration of labels
    colnames(combinedpeptidedata) <- c("Protein Name:", "unique", "Peptide Sequence:", "collapsed", "126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
    combinedpeptidedata <- combinedpeptidedata[which(combinedpeptidedata$unique == "U"),]
    combinedpeptidedata <- combinedpeptidedata[-c(2,4)]
    return(combinedpeptidedata)
  }
  getPepSignalSumOff <- function(){
      peptidedata <- values$totalfilteredpepdata[12:22]
      Signal_Sum <- NULL
      Signal_Sum <- rowSums(peptidedata, na.rm=TRUE)
      return(data.frame(Signal_Sum))
  }
  getPepNames <- function(){
    names <- values$totalfilteredpepdata[2]
    #declaration of labels
    colnames(names) <- c("Gene Symbol:")
    return(names)
  }
  getUniquePepNames <- function(){
    names <- getUniquePepDataOff()
    return(names[1])
  }
  getPepSequenceOff <- function(){
      pepseq <- values$totalfilteredpepdata[10]
      #declaration of labels
      colnames(pepseq) <- c("Peptide Sequence:")
      return(pepseq)
  }
  getUniquePepSequenceOff <- function(){
    peptidedata <- values$totalfilteredpepdata
    peptidedata <- peptidedata[9:22]
    names <- values$totalfilteredpepdata[2]
    combinedpeptidedata <- cbind(names, peptidedata)
    #declaration of labels
    colnames(combinedpeptidedata) <- c("Protein Name:", "unique", "seq", "collapsed", "126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
    combinedpeptidedata <- combinedpeptidedata[which(combinedpeptidedata$unique == "U"),]
    pepseq <- combinedpeptidedata[3]
    colnames(pepseq) <- c("Peptide Sequence:")
    return(pepseq)
  }
  getFiltIsoSpec <- function(){
    isospec <- data.frame(values$totalfilteredpepdata$IsolationSpecificity)
    return(isospec)
  }
  getPotpourriColumnAll <- function(colnum){
      return(totaldata()[colnum])
  }
  getPotpourriColumn <- function(colnum){
      return(totaldata()[colnum])
  }
  getGoldenPotpourriColumn <- function(colnum){
    return(values$totalgolddata[colnum])
  }
  getUpdatedGoldData <- function(){
    idstring2 <- paste("peptide_view/", input$prevSID,"?type=peptide&all_users=1&set_bit=3", sep="")
    #will extract information into tables
    gfy <- gfy_api("https://wren.hms.harvard.edu/gfy/www/modules/api/v1/", usernamein(), passwordin())
    peptides <-  gfy.get(gfy, idstring2)
    #filter peptides for duplicates
    peptides <- as.data.frame(peptides)
    peptides <- peptides[!duplicated(peptides$scanf),]
    peptides <- peptides[which(peptides$validity == "3"),]
    return(data.frame(peptides))
  }
  filterPepData <- function(iso, ss){
    df <- totalpepdata()
    df <- df[which(df$IsolationSpecificity >= iso),]
    sums <- NULL
    sums <- lapply(1:nrow(df), function(i){
      sums <- c(sums, do.call(sum, df[i,12:22]))
    })
    df <- df[which(sums >= ss),]
    return(df)
  }
  bold <- function(string){
    return(paste0("<b>",string,"</b>"))
  }
  color <- function(string, color){
    return(paste0("<font color = \"", color, "\">", string, "</font>"))
  }

  #Home Tab ----------------------------------------------
  output$pqidAndSid <- renderUI({
    plexOut <- NULL
    if(values$plexNum == 9)
    {
      plexOut <- bold("TKO9-plex")
    }
    else
    {
      plexOut <- bold("TKO11-plex")
    }
    HTML(paste(bold("Protein Quant ID: "), values$pqid, " | ",
               bold("Search ID: "), values$sid, " | ",
               plexOut, sep=""))
  })
  output$homeTitle <- renderUI({
    HTML(paste(color(bold("TVT: TKO Visualization Tool"),"MediumSeaGreen")))
  })
  output$credits <- renderUI({
    HTML(color(paste("v1 | © 2017 | Contact Joao Paulo",
        "(joao_paulo@hms.harvard.edu)", "or Jeremy Gygi",
        "(jeremy.gygi@gmail.com) with questions/issues."), "#3c3c3c"))
  })
  output$searchInformation <- renderUI({
    timeStart <- Sys.time()
    updateProgressBar(session = session, id = "progressBar1", value = 0)

    #Search for Proteins:
    numProt <- nrow(totalpepdata())
    updateProgressBar(session = session, id = "progressBar1", value = 25)
    timeProt <- Sys.time()
    timeProtDif <- timeProt - timeStart
    output$downloadProteinStatus <- renderText({
      paste("Protein Data loaded in ", round(as.numeric(toString(timeProtDif)), 3), " seconds.",sep="")
    })
    showNotification(paste("(1/5) Prot. Data loaded in ", round(as.numeric(toString(timeProtDif)), 3), " seconds.",sep=""), duration = 4)

    values$totalfilteredpepdata <- filterPepData(input$isoCutOff, input$ssCutOff)
    updateProgressBar(session = session, id = "progressBar1", value = 35)
    timeProtFilt <- Sys.time()
    timeProtDifFilt <- timeProtFilt - timeProt
    output$filterProteinStatus <- renderText({
      paste("Protein Data filtered in ", round(as.numeric(toString(timeProtDifFilt)), 3), " seconds.",sep="")
    })
    showNotification(paste("(2/5) Prot. Data Filtered in ", round(as.numeric(toString(timeProtDifFilt)), 3), " seconds.",sep=""), duration = 4)

    #Search for Chroma Data:
    chroma <- totalchromadata()
    updateProgressBar(session = session, id = "progressBar1", value = 50)
    timeChroma <- Sys.time()
    timeChromaDif <- timeChroma - timeProtFilt
    output$downloadChromaStatus <- renderText({
      paste("Chroma Data loaded in ", round(as.numeric(toString(timeChromaDif)), 3), " seconds.",sep="")
    })
    showNotification(paste("(3/5) Chroma Data loaded in ", round(as.numeric(toString(timeChromaDif)), 3), " seconds.",sep=""), duration = 4)

    #Search for Peptides:
    numPep <- nrow(totaldata())
    updateProgressBar(session = session, id = "progressBar1", value = 85)
    timePep <- Sys.time()
    timePepDif <- timePep - timeChroma
    output$downloadPepStatus <- renderText({
      paste("Peptide Data loaded in ", round(as.numeric(toString(timePepDif)), 3), " seconds.",sep="")
    })
    showNotification(paste("(4/5) Pep. Data loaded in ", round(as.numeric(toString(timePepDif)), 3), " seconds.",sep=""), duration = 4)

    #Search for Stat Data:
    chroma <- totalchromadata()
    updateProgressBar(session = session, id = "progressBar1", value = 100)
    timeFinish <- Sys.time()
    timeStatsDif <- timeFinish - timePep
    output$downloadStatsStatus <- renderText({
      paste("Stats Data loaded in ", round(as.numeric(toString(timeStatsDif)), 3), " seconds.",sep="")
    })
    showNotification(paste("(5/5) Instrument Stats loaded in ", round(as.numeric(toString(timeStatsDif)), 3), " seconds.",sep=""), duration = 4)

    #Finished
    values$totalgolddata <- NULL
    timeDifferenceTot <- timeFinish - timeStart
    output$downloadStatus <- renderText({
      paste("Total Data loaded in ", round(as.numeric(toString(timeDifferenceTot)), 3), " seconds.",sep="")
    })

    df <- totalstatsdata()
    numProt <- round(df[3,21],0)
    numUniquePep <- round(df[3,20],0)
    showNotification(paste("DONE. All data loaded in ", round(as.numeric(toString(timeDifferenceTot)), 3), " seconds.",sep=""), duration = 4)
    HTML(paste(bold("Unfiltered Statistics:<br/>"), bold("Total Proteins: "), numProt, "<br/>", bold("Unique Peptides: "), numUniquePep, "<br/>", bold("Total Peptides: "), numPep, "<br>", bold(color("Data loaded successfully!", "MediumSeaGreen")),sep=""))
  })

  #Single Peptide Tab ------------------------------------

  #KO Peptide Table Tab ----------------------------------
  output$quicksummarytable <- renderTable({
    peptidedatatable <- getUniquePepDataOff()
    pepnames <- peptidedatatable[1]
    pepdata <- peptidedatatable[2:12]

    if(values$plexNum == 9)
    {
        totalinterfreeindexvec <- NULL
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice1))
        cutpepdata <- pepdata[pepindex,]

        if (input$kolocations1 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations1 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations1 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum1 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- ((interfreeindexvec))
        len1 <- nrow(cutpepdata)

        interfreeindexvec1 <- interfreeindexvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice2))
        cutpepdata <- pepdata[pepindex,]
        if (input$kolocations2 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations2 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations2 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum2 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
        len2 <- nrow(cutpepdata)

        interfreeindexvec2 <- interfreeindexvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice3))
        cutpepdata <- pepdata[pepindex,]
        if (input$kolocations3 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations3 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations3 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum3 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
        len3 <- nrow(cutpepdata)


                          interfreeindexvec3 <- interfreeindexvec


                          totalinterfreeindexvec <- cbind.fill(interfreeindexvec1, interfreeindexvec2, interfreeindexvec3, fill = NA)

                          #plot(interfreeindexvec, ylim=c(0,1.2), xaxt='n', yaxt='n')

                          colnames(totalinterfreeindexvec) <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                          #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
                          #marks <- c(0.2,0.4,0.6,0.8,1.0)
                          #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
                          #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
                          #par(new=T)

                          avg1 <- sum1/len1
                          avg2 <- sum2/len2
                          avg3 <- sum3/len3

                          line1x <- c(.7,1.3)
                          line2x <- c(1.7, 2.3)
                          line3x <- c(2.7, 3.3)
                          line1y <- c(avg1,avg1)
                          line2y <- c(avg2,avg2)
                          line3y <- c(avg3,avg3)


                          column1 <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                          column2 <- c(len1, len2, len3)
                          column3 <- c(paste(round(avg1,3), "+/-", round(sd(unlist(totalinterfreeindexvec[1]), na.rm=TRUE), 3)),
                                      paste(round(avg2,3), "+/-", round(sd(unlist(totalinterfreeindexvec[2]), na.rm=TRUE), 3)),
                                      paste(round(avg3,3), "+/-", round(sd(unlist(totalinterfreeindexvec[3]), na.rm=TRUE), 3)))
                          table_to_output <- cbind(column1, column2, column3)
                          colnames(table_to_output) <- c("Gene Symbol:", "Num. of Peptides:", "IFI +/- Std.Dev.")
                          return(table_to_output)
    }
    else
    {
      totalinterfreeindexvec <- NULL
      sum1 <- NULL
      sum2 <- NULL
      sum3 <- NULL
      len1 <- NULL
      len2 <- NULL
      len3 <- NULL

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice1))
      cutpepdata <- pepdata[pepindex,]

      if (input$kolocations1 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations1 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations1 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum1 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- ((interfreeindexvec))
      len1 <- nrow(cutpepdata)

      interfreeindexvec1 <- interfreeindexvec

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice2))
      cutpepdata <- pepdata[pepindex,]
      if (input$kolocations2 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations2 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations2 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum2 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
      len2 <- nrow(cutpepdata)

      interfreeindexvec2 <- interfreeindexvec

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice3))
      cutpepdata <- pepdata[pepindex,]
      if (input$kolocations3 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations3 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations3 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum3 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
      len3 <- nrow(cutpepdata)


                        interfreeindexvec3 <- interfreeindexvec


                        totalinterfreeindexvec <- cbind.fill(interfreeindexvec1, interfreeindexvec2, interfreeindexvec3, fill = NA)

                        #plot(interfreeindexvec, ylim=c(0,1.2), xaxt='n', yaxt='n')

                        colnames(totalinterfreeindexvec) <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                        #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
                        #marks <- c(0.2,0.4,0.6,0.8,1.0)
                        #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
                        #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
                        #par(new=T)

                        avg1 <- sum1/len1
                        avg2 <- sum2/len2
                        avg3 <- sum3/len3

                        line1x <- c(.7,1.3)
                        line2x <- c(1.7, 2.3)
                        line3x <- c(2.7, 3.3)
                        line1y <- c(avg1,avg1)
                        line2y <- c(avg2,avg2)
                        line3y <- c(avg3,avg3)


                        column1 <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                        column2 <- c(len1, len2, len3)
                        column3 <- c(paste(round(avg1,3), "+/-", round(sd(unlist(totalinterfreeindexvec[1]), na.rm=TRUE), 3)),
                                    paste(round(avg2,3), "+/-", round(sd(unlist(totalinterfreeindexvec[2]), na.rm=TRUE), 3)),
                                    paste(round(avg3,3), "+/-", round(sd(unlist(totalinterfreeindexvec[3]), na.rm=TRUE), 3)))
                        table_to_output <- cbind(column1, column2, column3)
                        colnames(table_to_output) <- c("Gene Symbol:", "Num. of Peptides:", "IFI +/- Std.Dev.")
                        return(table_to_output)
    }
    })
  output$peptidetableout <- renderDataTable({
    if(input$onlyshowtko)
    {
      peptidedatatable <- getUniquePepSeqDataOff()
      peptidedatatable <- cbind(peptidedatatable[1:2], round(peptidedatatable[3:13],1))
    }
    else
    {
      peptidedatatable <- getFiltPepDataOff()
      pepseq <- getPepSequenceOff()
      peptidedatatable <- cbind(peptidedatatable[1], pepseq, round(peptidedatatable[2:12],1))
    }
    
    

    if(input$onlyshowtko){
      names <- getUniquePepNames()
      namesvec <- c(t(names))
      #prot1
      protname <- toupper(input$proteinchoice1)
      currentnamesvec <- which(namesvec == protname)
      totaldatatable <- peptidedatatable[currentnamesvec,]
      #prot2
      protname <- toupper(input$proteinchoice2)
      currentnamesvec <- which(namesvec == protname)
      totaldatatable <- rbind(totaldatatable, peptidedatatable[currentnamesvec,])
      #prot3
      protname <- toupper(input$proteinchoice3)
      currentnamesvec <- which(namesvec == protname)
      totaldatatable <- rbind(totaldatatable, peptidedatatable[currentnamesvec,])

      if(values$plexNum == 9)
      {
        totaldatatable <- totaldatatable[-((ncol(totaldatatable)-1):ncol(totaldatatable))]
        if(input$convertToPercentage1)
        {
          for(i in 1:nrow(totaldatatable))
          {
            rowsum <- sum(as.numeric(totaldatatable[i,3:11]))
            for(j in 3:11)
            {
              totaldatatable[i,j] <- round(100*as.numeric(totaldatatable[i,j])/rowsum,1)
            }
          }
          brks <- quantile(totaldatatable[3:11], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
                  {paste0("rgb(255,", ., ",", ., ")")}
          
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable))) %>%
            formatStyle(c('126', '127N', '127C', '128N', '128C', '129N', '129C', '130N', '130C'), 
                backgroundColor = styleInterval(brks, clrs))
        }
        else
        {
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable)))
        }
      }
      else
      {
        if(input$convertToPercentage1)
        {
          for(i in 1:nrow(totaldatatable))
          {
            rowsum <- sum(as.numeric(totaldatatable[i,3:13]))
            for(j in 3:13)
            {
              totaldatatable[i,j] <- round(100*as.numeric(totaldatatable[i,j])/rowsum,1)
            }
          }
          brks <- quantile(totaldatatable[3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
                  {paste0("rgb(255,", ., ",", ., ")")}
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable))) %>%
            formatStyle(c('126', '127N', '127C', '128N', '128C', '129N', '129C', '130N', '130C', '131N', '131C'), 
                backgroundColor = styleInterval(brks, clrs))
        }
        else
        {
          #totaldatatable$isospec <- getFiltIsoSpec()[rownames(totaldatatable),1]
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable)))
        }
    }

      
    } else {
      if(values$plexNum == 9)
      {
        peptidedatatable <- peptidedatatable[-((ncol(peptidedatatable)-1):ncol(peptidedatatable))]
        DT::datatable(peptidedatatable, options = list(initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"),
          searchHighlight = TRUE,
          lengthMenu = c(10, 25, 50, 100, nrow(peptidedatatable)), pageLength = nrow(peptidedatatable)))
      }
      else
      {
        DT::datatable(peptidedatatable, options = list(initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"),
          searchHighlight = TRUE,
          lengthMenu = c(10, 25, 50, 100, nrow(peptidedatatable)), pageLength = nrow(peptidedatatable)))
      }
      
    }

    })

  #Unchanging Peptide Table Tab ------------------------
  output$quicksummarytable2 <- renderTable({
    if(values$plexNum == 9)
    {
        peptidedatatable <- getFiltPepDataOff()
        pepnames <- peptidedatatable[1]
        pepdata <- peptidedatatable[2:10]
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice4))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/9))
        }
        sum1 <- do.call(sum, as.list(percentCVvec))
        len1 <- nrow(cutpepdata)
        percentCVvec1 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice5))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/9))
        }
        sum2 <- do.call(sum, as.list(percentCVvec))
        len2 <- nrow(cutpepdata)
        percentCVvec2 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice6))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/9))
        }
        sum3 <- do.call(sum, as.list(percentCVvec))
        len3 <- nrow(cutpepdata)
        percentCVvec3 <- percentCVvec

        percentCVTotal <- cbind.fill(percentCVvec1, percentCVvec2, percentCVvec3, fill = NA)
        colnames(percentCVTotal) <- c(toupper(input$proteinchoice4), toupper(input$proteinchoice5), toupper(input$proteinchoice6))
        avg1 <- sum1/len1
        avg2 <- sum2/len2
        avg3 <- sum3/len3
        column1 <- c(toupper(input$proteinchoice4), toupper(input$proteinchoice5), toupper(input$proteinchoice6))
        column2 <- c(len1, len2, len3)
        column3 <- c(paste(round(avg1,3), "% +/-", round(sd(percentCVvec1, na.rm=TRUE), 3)),
                    paste(round(avg2,3), "% +/-", round(sd(percentCVvec2, na.rm=TRUE), 3)),
                    paste(round(avg3,3), "% +/-", round(sd(percentCVvec3, na.rm=TRUE), 3)))
        table_to_output <- cbind(column1, column2, column3)
        colnames(table_to_output) <- c("Gene Symbol:", "Num. of Peptides:", "%CV +/- Std.Dev.")
        return(table_to_output)
    }
    else
    {
        peptidedatatable <- getFiltPepDataOff()
        pepnames <- peptidedatatable[1]
        pepdata <- peptidedatatable[2:12]
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice4))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/11))
        }
        sum1 <- do.call(sum, as.list(percentCVvec))
        len1 <- nrow(cutpepdata)
        percentCVvec1 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice5))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/11))
        }
        sum2 <- do.call(sum, as.list(percentCVvec))
        len2 <- nrow(cutpepdata)
        percentCVvec2 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice6))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/11))
        }
        sum3 <- do.call(sum, as.list(percentCVvec))
        len3 <- nrow(cutpepdata)
        percentCVvec3 <- percentCVvec

        percentCVTotal <- cbind.fill(percentCVvec1, percentCVvec2, percentCVvec3, fill = NA)
        colnames(percentCVTotal) <- c(toupper(input$proteinchoice4), toupper(input$proteinchoice5), toupper(input$proteinchoice6))
        avg1 <- sum1/len1
        avg2 <- sum2/len2
        avg3 <- sum3/len3
        column1 <- c(toupper(input$proteinchoice4), toupper(input$proteinchoice5), toupper(input$proteinchoice6))
        column2 <- c(len1, len2, len3)
        column3 <- c(paste(round(avg1,3), "% +/-", round(sd(percentCVvec1, na.rm=TRUE), 3)),
                    paste(round(avg2,3), "% +/-", round(sd(percentCVvec2, na.rm=TRUE), 3)),
                    paste(round(avg3,3), "% +/-", round(sd(percentCVvec3, na.rm=TRUE), 3)))
        table_to_output <- cbind(column1, column2, column3)
        colnames(table_to_output) <- c("Gene Symbol:", "Num. of Peptides:", "%CV +/- Std.Dev.")
        return(table_to_output)
    }
    })
  output$peptidetableout2 <- renderDataTable({
    peptidedatatable <- getFiltPepDataOff()
    pepseq <- getPepSequenceOff()
    peptidedatatable <- cbind(peptidedatatable[1], pepseq, round(peptidedatatable[2:12],1))

    if(input$onlyshowreg){
      names <- getPepNames()
      namesvec <- c(t(names))
      #prot1
      protname <- toupper(input$proteinchoice4)
      currentnamesvec <- which(namesvec == protname)
      totaldatatable <- peptidedatatable[currentnamesvec,]
      #prot2
      protname <- toupper(input$proteinchoice5)
      currentnamesvec <- which(namesvec == protname)
      totaldatatable <- rbind(totaldatatable, peptidedatatable[currentnamesvec,])
      #prot3
      protname <- toupper(input$proteinchoice6)
      currentnamesvec <- which(namesvec == protname)
      totaldatatable <- rbind(totaldatatable, peptidedatatable[currentnamesvec,])

      if(values$plexNum == 9)
      {
        totaldatatable <- totaldatatable[-((ncol(totaldatatable)-1):ncol(totaldatatable))]
        if(input$convertToPercentage2)
        {
          for(i in 1:nrow(totaldatatable))
          {
            rowsum <- sum(as.numeric(totaldatatable[i,3:11]))
            for(j in 3:11)
            {
              totaldatatable[i,j] <- round(100*as.numeric(totaldatatable[i,j])/rowsum,1)
            }
          }
          brks <- quantile(totaldatatable[3:11], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
                  {paste0("rgb(255,", ., ",", ., ")")}
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable))) %>%
            formatStyle(c('126', '127N', '127C', '128N', '128C', '129N', '129C', '130N', '130C'), 
                backgroundColor = styleInterval(brks, clrs))
        }
        else
        {
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable)))
        }
      }
      else
      {
        if(input$convertToPercentage2)
        {
          for(i in 1:nrow(totaldatatable))
          {
            rowsum <- sum(as.numeric(totaldatatable[i,3:13]))
            for(j in 3:13)
            {
              totaldatatable[i,j] <- round(100*as.numeric(totaldatatable[i,j])/rowsum,1)
            }
          }
          brks <- quantile(totaldatatable[3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
                  {paste0("rgb(255,", ., ",", ., ")")}
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable))) %>%
            formatStyle(c('126', '127N', '127C', '128N', '128C', '129N', '129C', '130N', '130C', '131N', '131C'), 
                backgroundColor = styleInterval(brks, clrs))
        }
        else
        {
          DT::datatable(totaldatatable, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            searchHighlight = TRUE,
            lengthMenu = c(10, 25, 50, 100, nrow(totaldatatable)), pageLength = nrow(totaldatatable)))
        }
    }

      
    } else {
      if(values$plexNum == 9)
      {
        peptidedatatable <- peptidedatatable[-((ncol(peptidedatatable)-1):ncol(peptidedatatable))]
        DT::datatable(peptidedatatable, options = list(initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"),
          searchHighlight = TRUE,
          lengthMenu = c(10, 25, 50, 100, nrow(peptidedatatable)), pageLength = nrow(peptidedatatable)))
      }
      else
      {
        DT::datatable(peptidedatatable, options = list(initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"),
          searchHighlight = TRUE,
          lengthMenu = c(10, 25, 50, 100, nrow(peptidedatatable)), pageLength = nrow(peptidedatatable)))
      }
      
    }

    })
  #Downloader --------------------------------------------
  output$downloadIFIData <- downloadHandler(
        filename = function() {
            paste(values$pqid, 'tvtdata.csv', sep='')
          },
        content = function(file) {
          sep <- ','
          write.table(values$IFIDF, file, sep=sep, col.names=TRUE, row.names=FALSE)
        }
      )

  #Interactive Graph Tab  --------------------------------
  output$interactiveplot <- renderScatterD3({
    if(values$plexNum == 9)
    {
        peptidedatatable <- getUniquePepDataOff()
        symbols <- getPepSequenceOff()
        pepnames <- peptidedatatable[1]
        pepdata <- peptidedatatable[2:10]
        sigsums <- getPepSignalSumOff()

        totalinterfreeindexvec <- NULL
        symbolvec <- NULL
        sigsumvec <- NULL
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice1))
        cutpepdata <- pepdata[pepindex,]
        cutsymboldata <- t(symbols[pepindex,])
        cutsigsumdata <- sigsums[pepindex,]
        cutsigsumdata <- data.frame(cutsigsumdata)


        if (input$kolocations1 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations1 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations1 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        cutoffcheck <- NULL

        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))

          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, as.numeric(1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6))))
          symbolvec <- c(symbolvec, toString(cutsymboldata[i]))
          sigsumvec <- c(sigsumvec, as.numeric(cutsigsumdata[i,]))

          if (1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)) >= input$protcutoff){
            cutoffcheck <- c(cutoffcheck, "Good")
          } else {
            cutoffcheck <- c(cutoffcheck, "Bad")
          }
        }



        sum1 <- do.call(sum, as.list(interfreeindexvec))
        nameaddition <- rep(toupper(input$proteinchoice1),nrow(cutpepdata))
        totalinterfreeindexvec <- cbind(nameaddition,as.numeric(interfreeindexvec), cutoffcheck, symbolvec, sigsumvec)
        len1 <- nrow(cutpepdata)



        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice2))
        cutpepdata <- pepdata[pepindex,]
        cutsymboldata <- symbols[pepindex,]
        cutsigsumdata <- sigsums[pepindex,]
        cutsigsumdata <- data.frame(cutsigsumdata)

        if (input$kolocations2 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations2 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations2 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        symbolvec <- NULL
        sigsumvec <- NULL
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        cutoffcheck <- NULL

        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, as.numeric(1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6))))
          symbolvec <- c(symbolvec, toString(cutsymboldata[i]))
          sigsumvec <- c(sigsumvec, as.numeric(cutsigsumdata[i,]))
          if (1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)) >= input$protcutoff){
            cutoffcheck <- c(cutoffcheck, "Good")
          } else {
            cutoffcheck <- c(cutoffcheck, "Bad")
          }
        }
        sum2 <- do.call(sum, as.list(interfreeindexvec))

        nameaddition <- rep(toupper(input$proteinchoice2),nrow(cutpepdata))
        toadd <- cbind(nameaddition,as.numeric(interfreeindexvec), cutoffcheck, symbolvec, sigsumvec)
        totalinterfreeindexvec <- rbind(totalinterfreeindexvec,toadd)
        len2 <- nrow(cutpepdata)

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice3))
        cutpepdata <- pepdata[pepindex,]
        cutsymboldata <- symbols[pepindex,]
        cutsigsumdata <- sigsums[pepindex,]
        cutsigsumdata <- data.frame(cutsigsumdata)

        if (input$kolocations3 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations3 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations3 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        symbolvec <- NULL
        sigsumvec <- NULL
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        cutoffcheck <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, as.numeric(1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6))))
          symbolvec <- c(symbolvec, toString(cutsymboldata[i]))
          sigsumvec <- c(sigsumvec, as.numeric(cutsigsumdata[i,]))
          if (1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)) >= input$protcutoff){
            cutoffcheck <- c(cutoffcheck, "Good")
          } else {
            cutoffcheck <- c(cutoffcheck, "Bad")
          }
        }
        sum3 <- do.call(sum, as.list(interfreeindexvec))

        nameaddition <- rep(toupper(input$proteinchoice3),nrow(cutpepdata))
        toadd <- cbind(nameaddition,as.numeric(interfreeindexvec), cutoffcheck, symbolvec, sigsumvec)
        totalinterfreeindexvec <- rbind(totalinterfreeindexvec,toadd)
        len3 <- nrow(cutpepdata)

        #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
        #marks <- c(0.2,0.4,0.6,0.8,1.0)
        #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
        #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
        #par(new=T)


        colorvec <- brewer.pal(4,"Set3"<-switch(input$colorTypePeptide, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
                                          varA = "Accent",
                                          varB = "Set1",
                                          varC = "Dark2",
                                          varD = "Blues",
                                          varE = "Reds",
                                          varF = "Greens",
                                          varG = "Pastel1"
                                          )#endSwitch
                                )#endBrewerPal
        avg1 <- sum1/len1
        avg2 <- sum2/len2
        avg3 <- sum3/len3

        line1x <- c(.7,1.3)
        line2x <- c(1.7, 2.3)
        line3x <- c(2.7, 3.3)
        line1y <- c(avg1,avg1)
        line2y <- c(avg2,avg2)
        line3y <- c(avg3,avg3)

        for(i in 1:nrow(totalinterfreeindexvec))
        {
          if(totalinterfreeindexvec[i,1] == input$proteinchoice1)
          {
            totalinterfreeindexvec[i,1] <- paste0("1: ", input$proteinchoice1)
          }
          if(totalinterfreeindexvec[i,1] == input$proteinchoice2)
          {
            totalinterfreeindexvec[i,1] <- paste0("2: ", input$proteinchoice2)
          }
          if(totalinterfreeindexvec[i,1] == input$proteinchoice3)
          {
            totalinterfreeindexvec[i,1] <- paste0("3: ", input$proteinchoice3)
          }
        }

        totalinterfreeindexvec <- data.frame(totalinterfreeindexvec)



        colnames(totalinterfreeindexvec) <- c("nameaddition","interfreeindexvec", "matchcut", "pepsym", "sigsumtot")
        #return(totalinterfreeindexvec)
        finaldf <- data.frame(nametitle = unlist(totalinterfreeindexvec[1]), interfreetitle = as.numeric(unlist(t(totalinterfreeindexvec[2]))), matchcut = totalinterfreeindexvec[3], pepsym = totalinterfreeindexvec[4], sigsumtot = totalinterfreeindexvec[5])
        invertedSizes <- data.frame(rep(NA, nrow(finaldf)))
        for(i in 1:nrow(finaldf))
        {
          invertedSizes[i,1] <- (as.numeric(finaldf[i,5]))^-1
        }
        finaldf <- cbind(finaldf, invertedSizes)
        colnames(finaldf) <- c("nametitle","interfreetitle", "matchcut", "pepsym", "sigsumtot", "invsigsumtot")
        tooltips <- paste("Peptide Seq:",finaldf$pepsym,"<br/>Signal Sum:", finaldf$sigsumtot, "<br/>IFI:",finaldf$interfreetitle)
        newdf <- finaldf
        colnames(newdf) <- c("Gene Symbol", "IFI", paste("IFI > ", input$protcutoff, "?", sep=""), "Peptide Symbol", "Signal Sum", "inv")
        values$IFIDF <- newdf[1:5]
        scatterD3(data = finaldf, x = nametitle, y = interfreetitle, point_size = 200, point_opacity = 0.7, hover_size = 2, hover_opacity = 1,
          col_var = matchcut, size_var = invsigsumtot, lines = data.frame(slope = 0,intercept = 1,stroke = "black",stroke_width = 3),
          legend_width = 0,
          size_range = c(300,300),
          axes_font_size = "175%",
          transitions = TRUE,
          colors = c("Good" = "blue" <-switch(input$colorGood, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
                                            varA = "blue",
                                            varB = "green",
                                            varC = "yellow",
                                            varD = "orange",
                                            varE = "red",
                                            varF = "purple",
                                            varG = "black"
                                            ),
                      "Bad" = "red" <-switch(input$colorBad, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
                                            varA = "blue",
                                            varB = "green",
                                            varC = "yellow",
                                            varD = "orange",
                                            varE = "red",
                                            varF = "purple",
                                            varG = "black"
                                            )
                        ),
          xlab = "",
          ylab = "",
          ellipses = TRUE,
          ylim = if(input$interylim){c(.9,1.02)}else{c(0,1.05)},
          tooltip_text = tooltips,
          caption = paste("Interactive Scatterplot for ",toupper(input$proteinchoice1),", ", toupper(input$proteinchoice2),", and ", toupper(input$proteinchoice3), sep = "")
          )
        #interfreeindexvec = vector with all Fx numbers
        #Plotting + Data
    }
    else
    {
        peptidedatatable <- getUniquePepDataOff()
        symbols <- getPepSequenceOff()
        pepnames <- peptidedatatable[1]
        pepdata <- peptidedatatable[2:12]
        sigsums <- getPepSignalSumOff()

        totalinterfreeindexvec <- NULL
        symbolvec <- NULL
        sigsumvec <- NULL
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice1))
        cutpepdata <- pepdata[pepindex,]
        cutsymboldata <- t(symbols[pepindex,])
        cutsigsumdata <- sigsums[pepindex,]
        cutsigsumdata <- data.frame(cutsigsumdata)


        if (input$kolocations1 == "varA"){
          knockout <- 1:3
          notknockout <- c(10,11)
        } else if (input$kolocations1 == "varB"){
          knockout <- 4:6
          notknockout <- c(10,11)
        } else if (input$kolocations1 == "varC"){
          knockout <- 7:9
          notknockout <- c(10,11)
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        cutoffcheck <- NULL

        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))

          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, as.numeric(1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2))))
          symbolvec <- c(symbolvec, toString(cutsymboldata[i]))
          sigsumvec <- c(sigsumvec, as.numeric(cutsigsumdata[i,]))

          if (1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)) >= input$protcutoff){
            cutoffcheck <- c(cutoffcheck, "Good")
          } else {
            cutoffcheck <- c(cutoffcheck, "Bad")
          }
        }



        sum1 <- do.call(sum, as.list(interfreeindexvec))
        nameaddition <- rep(toupper(input$proteinchoice1),nrow(cutpepdata))
        totalinterfreeindexvec <- cbind(nameaddition,as.numeric(interfreeindexvec), cutoffcheck, symbolvec, sigsumvec)
        len1 <- nrow(cutpepdata)



        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice2))
        cutpepdata <- pepdata[pepindex,]
        cutsymboldata <- symbols[pepindex,]
        cutsigsumdata <- sigsums[pepindex,]
        cutsigsumdata <- data.frame(cutsigsumdata)

        if (input$kolocations2 == "varA"){
          knockout <- 1:3
          notknockout <- c(10,11)
        } else if (input$kolocations2 == "varB"){
          knockout <- 4:6
          notknockout <- c(10,11)
        } else if (input$kolocations2 == "varC"){
          knockout <- 7:9
          notknockout <- c(10,11)
        }
        symbolvec <- NULL
        sigsumvec <- NULL
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        cutoffcheck <- NULL

        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, as.numeric(1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2))))
          symbolvec <- c(symbolvec, toString(cutsymboldata[i]))
          sigsumvec <- c(sigsumvec, as.numeric(cutsigsumdata[i,]))
          if (1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)) >= input$protcutoff){
            cutoffcheck <- c(cutoffcheck, "Good")
          } else {
            cutoffcheck <- c(cutoffcheck, "Bad")
          }
        }
        sum2 <- do.call(sum, as.list(interfreeindexvec))

        nameaddition <- rep(toupper(input$proteinchoice2),nrow(cutpepdata))
        toadd <- cbind(nameaddition,as.numeric(interfreeindexvec), cutoffcheck, symbolvec, sigsumvec)
        totalinterfreeindexvec <- rbind(totalinterfreeindexvec,toadd)
        len2 <- nrow(cutpepdata)

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice3))
        cutpepdata <- pepdata[pepindex,]
        cutsymboldata <- symbols[pepindex,]
        cutsigsumdata <- sigsums[pepindex,]
        cutsigsumdata <- data.frame(cutsigsumdata)

        if (input$kolocations3 == "varA"){
          knockout <- 1:3
          notknockout <- c(10,11)
        } else if (input$kolocations3 == "varB"){
          knockout <- 4:6
          notknockout <- c(10,11)
        } else if (input$kolocations3 == "varC"){
          knockout <- 7:9
          notknockout <- c(10,11)
        }
        symbolvec <- NULL
        sigsumvec <- NULL
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        cutoffcheck <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, as.numeric(1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2))))
          symbolvec <- c(symbolvec, toString(cutsymboldata[i]))
          sigsumvec <- c(sigsumvec, as.numeric(cutsigsumdata[i,]))
          if (1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)) >= input$protcutoff){
            cutoffcheck <- c(cutoffcheck, "Good")
          } else {
            cutoffcheck <- c(cutoffcheck, "Bad")
          }
        }
        sum3 <- do.call(sum, as.list(interfreeindexvec))

        nameaddition <- rep(toupper(input$proteinchoice3),nrow(cutpepdata))
        toadd <- cbind(nameaddition,as.numeric(interfreeindexvec), cutoffcheck, symbolvec, sigsumvec)
        totalinterfreeindexvec <- rbind(totalinterfreeindexvec,toadd)
        len3 <- nrow(cutpepdata)

        #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
        #marks <- c(0.2,0.4,0.6,0.8,1.0)
        #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
        #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
        #par(new=T)


        colorvec <- brewer.pal(4,"Set3"<-switch(input$colorTypePeptide, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
                                          varA = "Accent",
                                          varB = "Set1",
                                          varC = "Dark2",
                                          varD = "Blues",
                                          varE = "Reds",
                                          varF = "Greens",
                                          varG = "Pastel1"
                                          )#endSwitch
                                )#endBrewerPal
        avg1 <- sum1/len1
        avg2 <- sum2/len2
        avg3 <- sum3/len3

        line1x <- c(.7,1.3)
        line2x <- c(1.7, 2.3)
        line3x <- c(2.7, 3.3)
        line1y <- c(avg1,avg1)
        line2y <- c(avg2,avg2)
        line3y <- c(avg3,avg3)

        for(i in 1:nrow(totalinterfreeindexvec))
        {
          if(totalinterfreeindexvec[i,1] == input$proteinchoice1)
          {
            totalinterfreeindexvec[i,1] <- paste0("1: ", input$proteinchoice1)
          }
          if(totalinterfreeindexvec[i,1] == input$proteinchoice2)
          {
            totalinterfreeindexvec[i,1] <- paste0("2: ", input$proteinchoice2)
          }
          if(totalinterfreeindexvec[i,1] == input$proteinchoice3)
          {
            totalinterfreeindexvec[i,1] <- paste0("3: ", input$proteinchoice3)
          }
        }

        totalinterfreeindexvec <- data.frame(totalinterfreeindexvec)



        colnames(totalinterfreeindexvec) <- c("nameaddition","interfreeindexvec", "matchcut", "pepsym", "sigsumtot")
        #return(totalinterfreeindexvec)
        finaldf <- data.frame(nametitle = unlist(totalinterfreeindexvec[1]), interfreetitle = as.numeric(unlist(t(totalinterfreeindexvec[2]))), matchcut = totalinterfreeindexvec[3], pepsym = totalinterfreeindexvec[4], sigsumtot = totalinterfreeindexvec[5])
        invertedSizes <- data.frame(rep(NA, nrow(finaldf)))
        for(i in 1:nrow(finaldf))
        {
          invertedSizes[i,1] <- (as.numeric(finaldf[i,5]))^-1
        }
        finaldf <- cbind(finaldf, invertedSizes)
        colnames(finaldf) <- c("nametitle","interfreetitle", "matchcut", "pepsym", "sigsumtot", "invsigsumtot")
        tooltips <- paste("Peptide Seq:",finaldf$pepsym,"<br/>Signal Sum:", finaldf$sigsumtot, "<br/>IFI:",finaldf$interfreetitle)

        scatterD3(data = finaldf, x = nametitle, y = interfreetitle, point_size = 200, point_opacity = 0.7, hover_size = 2, hover_opacity = 1,
          col_var = matchcut, size_var = invsigsumtot, lines = data.frame(slope = 0,intercept = 1,stroke = "black",stroke_width = 3),
          legend_width = 0,
          size_range = if(input$showsizes){c(300,1000)}else{c(300,300)},
          axes_font_size = "175%",
          transitions = TRUE,
          colors = c("Good" = "blue" <-switch(input$colorGood, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
                                            varA = "blue",
                                            varB = "green",
                                            varC = "yellow",
                                            varD = "orange",
                                            varE = "red",
                                            varF = "purple",
                                            varG = "black"
                                            ),
                      "Bad" = "red" <-switch(input$colorBad, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
                                            varA = "blue",
                                            varB = "green",
                                            varC = "yellow",
                                            varD = "orange",
                                            varE = "red",
                                            varF = "purple",
                                            varG = "black"
                                            )
                        ),
          xlab = "",
          ylab = "",
          ellipses = TRUE,
          ylim = if(input$interylim){c(.9,1.02)}else{c(0,1.05)},
          tooltip_text = tooltips,
          caption = paste("Interactive Scatterplot for ",toupper(input$proteinchoice1),", ", toupper(input$proteinchoice2),", and ", toupper(input$proteinchoice3), sep = "")
          )
        #interfreeindexvec = vector with all Fx numbers
        #Plotting + Data
    }
    
    })
  output$interactiveplotnote <- renderUI({
      if(values$plexNum == 9)
      {
      output <- "<br/>For each peptide, we calculate an <b>interference-free index (IFI) </b>as the
                difference from one of the average TMT signal-to-noise value from the KO channels
                (for Met6: 126, 127N, and 127C) divided by the average TMT signal-to-noise of the
                other six channels. As such, a score of 1 reflects no interference, whereas a score
                of 0 or less would indicate equal or greater signal for a given TKO peptide in the
                KO channel than the non-KO channel; in other words, greater interference.
                <br/><br/><B>IFI = 1 – ((average TMT S:N in KO channels)/ (average TMT S:N in non-KO channels))</B><br/><br/>"
      }
      else 
      {
        output <- "<br/>For each peptide, we calculate an <b>interference-free index
                  (IFI)</b> as the difference from one of the average TMT signal-to-noise
                  value from each KO channels (i.e., for Met6: 126, 127N, and 127C) divided
                  by the average TMT signal-to-noise of the for the WT channels. As such, a
                  score of 1 reflects no interference, whereas a score of 0 or less would
                  indicate equal or greater signal for a given TKO peptide in the KO
                  channel than the non-KO channel; in other words, greater interference.
                  <br/>
                  <br/>
                  <B>IFI = 1 – ((average TMT S:N in KO channels)/
                  (average TMT S:N in WT channels))</B><br/><br/>"
      }
      HTML(output)
      })

  #KO Graphs Tab  ----------------------------------------
  output$proteinplot1 <- renderPlot({
            colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                    varA = "Accent",
                                                    varB = "Set1",
                                                    varC = "Dark2",
                                                    varD = "Blues",
                                                    varE = "Reds",
                                                    varF = "Greens",
                                                    varG = "Pastel1"
                                              )#endSwitch
                                    )#endBrewerPal
            if(input$colorType == "varB"){
              mycolors <- c(colorvec[2],colorvec[2],colorvec[2],colorvec[1],colorvec[1],colorvec[1],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            } else {
              mycolors <- c(colorvec[1],colorvec[1],colorvec[1],colorvec[2],colorvec[2],colorvec[2],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            }
            spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1,.5,.1)
            plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
            names <- getUniquePepNames()
            peptidedata <- getUniquePepDataOff()
            namesvec <- c(t(names))

            protname <- toupper(input$proteinchoice1)
            currentnamesvec <- which(namesvec == protname)
            totaldatatable <- peptidedata[currentnamesvec,2:12]
            totaldatatable <- colSums(totaldatatable)

            if(values$plexNum == 9)
            {
              plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C")
              mycolors <- mycolors[-((length(mycolors)-1):length(mycolors))]
              totaldatatable <- totaldatatable[-((length(totaldatatable)-1):length(totaldatatable))]
              spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1)
            }

            #Graphs
            barplot(totaldatatable, main=protname, col="white", names.arg = plotlabels, space = spaces);par(new=T)
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback2){"black"} else {"white"}, border = FALSE);par(new=T)
            barplot(totaldatatable, main=protname, col=mycolors, names.arg = plotlabels, space = spaces, font.lab = 2, ylab = "TMT Summed Signal-To-Noise")
            dev.off()
    })#end renderPlot proteinplot
  output$proteinplot2 <- renderPlot({
            colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                    varA = "Accent",
                                                    varB = "Set1",
                                                    varC = "Dark2",
                                                    varD = "Blues",
                                                    varE = "Reds",
                                                    varF = "Greens",
                                                    varG = "Pastel1"
                                              )#endSwitch
                                    )#endBrewerPal
            if(input$colorType == "varB"){
              mycolors <- c(colorvec[2],colorvec[2],colorvec[2],colorvec[1],colorvec[1],colorvec[1],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            } else {
              mycolors <- c(colorvec[1],colorvec[1],colorvec[1],colorvec[2],colorvec[2],colorvec[2],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            }
            plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
            names <- getUniquePepNames()
            peptidedata <- getUniquePepDataOff()
            namesvec <- c(t(names))

            protname <- toupper(input$proteinchoice2)
            currentnamesvec <- which(namesvec == protname)
            totaldatatable <- peptidedata[currentnamesvec,2:12]
            totaldatatable <- colSums(totaldatatable)

            #Graphs
            spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1,.5,.1)

            if(values$plexNum == 9)
            {
              plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C")
              mycolors <- mycolors[-((length(mycolors)-1):length(mycolors))]
              totaldatatable <- totaldatatable[-((length(totaldatatable)-1):length(totaldatatable))]
              spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1)
            }

            barplot(totaldatatable, main=protname, col="white", names.arg = plotlabels, space = spaces);par(new=T)
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback2){"black"} else {"white"}, border = FALSE);par(new=T)
            barplot(totaldatatable, main=protname, col=mycolors, names.arg = plotlabels, space = spaces, font.lab = 2, ylab = "TMT Summed Signal-To-Noise")
            dev.off()
    })#end renderPlot proteinplot
  output$proteinplot3 <- renderPlot({
            colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                    varA = "Accent",
                                                    varB = "Set1",
                                                    varC = "Dark2",
                                                    varD = "Blues",
                                                    varE = "Reds",
                                                    varF = "Greens",
                                                    varG = "Pastel1"
                                              )#endSwitch
                                    )#endBrewerPal
            if(input$colorType == "varB"){
              mycolors <- c(colorvec[2],colorvec[2],colorvec[2],colorvec[1],colorvec[1],colorvec[1],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            } else {
              mycolors <- c(colorvec[1],colorvec[1],colorvec[1],colorvec[2],colorvec[2],colorvec[2],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            }
            plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
            names <- getUniquePepNames()
            peptidedata <- getUniquePepDataOff()
            namesvec <- c(t(names))

            protname <- toupper(input$proteinchoice3)
            currentnamesvec <- which(namesvec == protname)
            totaldatatable <- peptidedata[currentnamesvec,2:12]
            totaldatatable <- colSums(totaldatatable)

            #Graphs
            spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1,.5,.1)

            if(values$plexNum == 9)
            {
              plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C")
              mycolors <- mycolors[-((length(mycolors)-1):length(mycolors))]
              totaldatatable <- totaldatatable[-((length(totaldatatable)-1):length(totaldatatable))]
              spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1)
            }

            barplot(totaldatatable, main=protname, col="white", names.arg = plotlabels, space = spaces);par(new=T)
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback2){"black"} else {"white"}, border = FALSE);par(new=T)
            barplot(totaldatatable, main=protname, col=mycolors, names.arg = plotlabels, space = spaces, font.lab = 2, ylab = "TMT Summed Signal-To-Noise")
            dev.off()
            })#end renderPlot proteinplot
  output$peptideplot <- renderPlot({
    peptidedatatable <- getUniquePepDataOff()
    pepnames <- peptidedatatable[1]
    pepdata <- peptidedatatable[2:12]
    if(values$plexNum == 9)
    {
        totalinterfreeindexvec <- NULL
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice1))
        cutpepdata <- pepdata[pepindex,]

        if (input$kolocations1 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations1 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations1 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum1 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- ((interfreeindexvec))
        len1 <- nrow(cutpepdata)

        interfreeindexvec1 <- interfreeindexvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice2))
        cutpepdata <- pepdata[pepindex,]
        if (input$kolocations2 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations2 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations2 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum2 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
        len2 <- nrow(cutpepdata)

        interfreeindexvec2 <- interfreeindexvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice3))
        cutpepdata <- pepdata[pepindex,]
        if (input$kolocations3 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations3 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations3 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum3 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
        len3 <- nrow(cutpepdata)


                          interfreeindexvec3 <- interfreeindexvec


                          totalinterfreeindexvec <- cbind.fill(interfreeindexvec1, interfreeindexvec2, interfreeindexvec3, fill = NA)

                          #plot(interfreeindexvec, ylim=c(0,1.2), xaxt='n', yaxt='n')

                          colnames(totalinterfreeindexvec) <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                          #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
                          #marks <- c(0.2,0.4,0.6,0.8,1.0)
                          #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
                          #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
                          #par(new=T)

                          avg1 <- sum1/len1
                          avg2 <- sum2/len2
                          avg3 <- sum3/len3

                          line1x <- c(.7,1.3)
                          line2x <- c(1.7, 2.3)
                          line3x <- c(2.7, 3.3)
                          line1y <- c(avg1,avg1)
                          line2y <- c(avg2,avg2)
                          line3y <- c(avg3,avg3)
    }
    else
    {
      totalinterfreeindexvec <- NULL
      sum1 <- NULL
      sum2 <- NULL
      sum3 <- NULL
      len1 <- NULL
      len2 <- NULL
      len3 <- NULL

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice1))
      cutpepdata <- pepdata[pepindex,]

      if (input$kolocations1 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations1 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations1 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum1 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- ((interfreeindexvec))
      len1 <- nrow(cutpepdata)

      interfreeindexvec1 <- interfreeindexvec

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice2))
      cutpepdata <- pepdata[pepindex,]
      if (input$kolocations2 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations2 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations2 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum2 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
      len2 <- nrow(cutpepdata)

      interfreeindexvec2 <- interfreeindexvec

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice3))
      cutpepdata <- pepdata[pepindex,]
      if (input$kolocations3 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations3 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations3 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum3 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
      len3 <- nrow(cutpepdata)


                        interfreeindexvec3 <- interfreeindexvec


                        totalinterfreeindexvec <- cbind.fill(interfreeindexvec1, interfreeindexvec2, interfreeindexvec3, fill = NA)

                        #plot(interfreeindexvec, ylim=c(0,1.2), xaxt='n', yaxt='n')

                        colnames(totalinterfreeindexvec) <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                        #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
                        #marks <- c(0.2,0.4,0.6,0.8,1.0)
                        #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
                        #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
                        #par(new=T)

                        avg1 <- sum1/len1
                        avg2 <- sum2/len2
                        avg3 <- sum3/len3

                        line1x <- c(.7,1.3)
                        line2x <- c(1.7, 2.3)
                        line3x <- c(2.7, 3.3)
                        line1y <- c(avg1,avg1)
                        line2y <- c(avg2,avg2)
                        line3y <- c(avg3,avg3)
    }

                      colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                              varA = "Accent",
                                                              varB = "Set1",
                                                              varC = "Dark2",
                                                              varD = "Blues",
                                                              varE = "Reds",
                                                              varF = "Greens",
                                                              varG = "Pastel1"
                                                        )#endSwitch
                                              )#endBrewerPal
                      if(input$colorType == "varB"){
                        colorvec <- c(colorvec[2],colorvec[1],colorvec[3])
                      } else {
                        colorvec <- c(colorvec[1],colorvec[2],colorvec[3])
                      }

                    beeswarm(totalinterfreeindexvec, main="TKO Peptides", xlab = NULL, col="white", xaxt='n', yaxt='n', method = input$methodchoice,
                    pch = if(input$pointshape){16}else{21}, cex = input$pointsize, lwd = 4, ylim = c(ymin = 0, ymax = 1.1))
                    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback){"black"} else {"white"})
                    lines(c(0,4), c(.2,.2), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(.4,.4), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(.6,.6), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(.8,.8), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(1,1), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(line1x,line1y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
                    lines(line2x,line2y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
                    lines(line3x,line3y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
                    marks <- c(0.2,0.4,0.6,0.8,1.0)
                    title(ylab = "IFI (Interference-Free Index)", cex.lab = 1.5)
                    axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1, cex.axis = 1.2, col = if(input$blackback){"white"} else {"black"})
                    axis(1, at = c(1,2,3), labels = format(c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))), cex.axis = 1.5, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    beeswarm(totalinterfreeindexvec, main="TKO Peptides", xlab = NULL, col=colorvec, xaxt='n', yaxt='n', method = input$methodchoice,
                    pch = if(input$pointshape){16}else{1}, lwd = 2 , cex = input$pointsize, ylim = c(ymin = 0, ymax = 1.1))
                    par(new = T)
                    text(.5,.2, "IFI:", cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(.6,.1, "Peptides:", cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(1,.1, paste("n =", len1), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(2,.1, paste("n =", len2), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(3,.1, paste("n =", len3), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(1,.2, paste(round(avg1,3), "+/-", round(sd(unlist(totalinterfreeindexvec[1]), na.rm=TRUE), 3)), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(2,.2, paste(round(avg2,3), "+/-", round(sd(unlist(totalinterfreeindexvec[2]), na.rm=TRUE), 3)), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(3,.2, paste(round(avg3,3), "+/-", round(sd(unlist(totalinterfreeindexvec[3]), na.rm=TRUE), 3)), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
              dev.off()
    })

  #Unchanging Graphs Tab -------------------------------
  output$proteinplot4 <- renderPlot({
            colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                    varA = "Accent",
                                                    varB = "Set1",
                                                    varC = "Dark2",
                                                    varD = "Blues",
                                                    varE = "Reds",
                                                    varF = "Greens",
                                                    varG = "Pastel1"
                                              )#endSwitch
                                    )#endBrewerPal
            if(input$colorType == "varB"){
              mycolors <- c(colorvec[2],colorvec[2],colorvec[2],colorvec[1],colorvec[1],colorvec[1],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            } else {
              mycolors <- c(colorvec[1],colorvec[1],colorvec[1],colorvec[2],colorvec[2],colorvec[2],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            }
            plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
            names <- getPepNames()
            peptidedata <- getFiltPepDataOff()
            namesvec <- c(t(names))

            protname <- toupper(input$proteinchoice4)
            currentnamesvec <- which(namesvec == protname)
            totaldatatable <- peptidedata[currentnamesvec,2:12]
            totaldatatable <- colSums(totaldatatable)

            #Graphs
            spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1,.5,.1)

            if(values$plexNum == 9)
            {
              plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C")
              mycolors <- mycolors[-((length(mycolors)-1):length(mycolors))]
              totaldatatable <- totaldatatable[-((length(totaldatatable)-1):length(totaldatatable))]
              spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1)
            }

            barplot(totaldatatable, main=protname, col="white", names.arg = plotlabels, space = spaces);par(new=T)
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback2){"black"} else {"white"}, border = FALSE);par(new=T)
            barplot(totaldatatable, main=protname, col=mycolors, names.arg = plotlabels, space = spaces, font.lab = 2, ylab = "TMT Summed Signal-To-Noise")
            dev.off()
            })#end renderPlot proteinplot
  output$proteinplot5 <- renderPlot({
            colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                    varA = "Accent",
                                                    varB = "Set1",
                                                    varC = "Dark2",
                                                    varD = "Blues",
                                                    varE = "Reds",
                                                    varF = "Greens",
                                                    varG = "Pastel1"
                                              )#endSwitch
                                    )#endBrewerPal
            if(input$colorType == "varB"){
              mycolors <- c(colorvec[2],colorvec[2],colorvec[2],colorvec[1],colorvec[1],colorvec[1],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            } else {
              mycolors <- c(colorvec[1],colorvec[1],colorvec[1],colorvec[2],colorvec[2],colorvec[2],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            }
            plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
            names <- getPepNames()
            peptidedata <- getFiltPepDataOff()
            namesvec <- c(t(names))

            protname <- toupper(input$proteinchoice5)
            currentnamesvec <- which(namesvec == protname)
            totaldatatable <- peptidedata[currentnamesvec,2:12]
            totaldatatable <- colSums(totaldatatable)

            #Graphs
            spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1,.5,.1)

            if(values$plexNum == 9)
            {
              plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C")
              mycolors <- mycolors[-((length(mycolors)-1):length(mycolors))]
              totaldatatable <- totaldatatable[-((length(totaldatatable)-1):length(totaldatatable))]
              spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1)
            }

            barplot(totaldatatable, main=protname, col="white", names.arg = plotlabels, space = spaces);par(new=T)
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback2){"black"} else {"white"}, border = FALSE);par(new=T)
            barplot(totaldatatable, main=protname, col=mycolors, names.arg = plotlabels, space = spaces, font.lab = 2, ylab = "TMT Summed Signal-To-Noise")
            dev.off()
            })#end renderPlot proteinplot
  output$proteinplot6 <- renderPlot({
            colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                    varA = "Accent",
                                                    varB = "Set1",
                                                    varC = "Dark2",
                                                    varD = "Blues",
                                                    varE = "Reds",
                                                    varF = "Greens",
                                                    varG = "Pastel1"
                                              )#endSwitch
                                    )#endBrewerPal
            if(input$colorType == "varB"){
              mycolors <- c(colorvec[2],colorvec[2],colorvec[2],colorvec[1],colorvec[1],colorvec[1],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            } else {
              mycolors <- c(colorvec[1],colorvec[1],colorvec[1],colorvec[2],colorvec[2],colorvec[2],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            }
            plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
            names <- getPepNames()
            peptidedata <- getFiltPepDataOff()
            namesvec <- c(t(names))

            protname <- toupper(input$proteinchoice6)
            currentnamesvec <- which(namesvec == protname)
            totaldatatable <- peptidedata[currentnamesvec,2:12]
            totaldatatable <- colSums(totaldatatable)

            #Graphs
            spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1,.5,.1)

            if(values$plexNum == 9)
            {
              plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C")
              mycolors <- mycolors[-((length(mycolors)-1):length(mycolors))]
              totaldatatable <- totaldatatable[-((length(totaldatatable)-1):length(totaldatatable))]
              spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1)
            }

            barplot(totaldatatable, main=protname, col="white", names.arg = plotlabels, space = spaces);par(new=T)
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback2){"black"} else {"white"}, border = FALSE);par(new=T)
            barplot(totaldatatable, main=protname, col=mycolors, names.arg = plotlabels, space = spaces, font.lab = 2, ylab = "TMT Summed Signal-To-Noise")
            dev.off()
            })#end renderPlot proteinplot
  output$peptideplot2 <- renderPlot({
    if(values$plexNum == 9)
    {
        peptidedatatable <- getFiltPepDataOff()
        pepnames <- peptidedatatable[1]
        pepdata <- peptidedatatable[2:10]
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice4))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/9))
        }
        sum1 <- do.call(sum, as.list(percentCVvec))
        len1 <- nrow(cutpepdata)
        percentCVvec1 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice5))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/9))
        }
        sum2 <- do.call(sum, as.list(percentCVvec))
        len2 <- nrow(cutpepdata)
        percentCVvec2 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice6))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/9))
        }
        sum3 <- do.call(sum, as.list(percentCVvec))
        len3 <- nrow(cutpepdata)
        percentCVvec3 <- percentCVvec

        percentCVTotal <- cbind.fill(percentCVvec1, percentCVvec2, percentCVvec3, fill = NA)
        colnames(percentCVTotal) <- c(toupper(input$proteinchoice4), toupper(input$proteinchoice5), toupper(input$proteinchoice6))
        avg1 <- sum1/len1
        avg2 <- sum2/len2
        avg3 <- sum3/len3
    }
    else
    {
        peptidedatatable <- getFiltPepDataOff()
        pepnames <- peptidedatatable[1]
        pepdata <- peptidedatatable[2:12]
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice4))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/11))
        }
        sum1 <- do.call(sum, as.list(percentCVvec))
        len1 <- nrow(cutpepdata)
        percentCVvec1 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice5))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/11))
        }
        sum2 <- do.call(sum, as.list(percentCVvec))
        len2 <- nrow(cutpepdata)
        percentCVvec2 <- percentCVvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice6))
        cutpepdata <- pepdata[pepindex,]
        percentCVvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          totalsum <- do.call(sum, cutpepdata[i,])
          stdev <- sd(cutpepdata[i,])
          percentCVvec <- c(percentCVvec, 100*stdev/(totalsum/11))
        }
        sum3 <- do.call(sum, as.list(percentCVvec))
        len3 <- nrow(cutpepdata)
        percentCVvec3 <- percentCVvec

        percentCVTotal <- cbind.fill(percentCVvec1, percentCVvec2, percentCVvec3, fill = NA)
        colnames(percentCVTotal) <- c(toupper(input$proteinchoice4), toupper(input$proteinchoice5), toupper(input$proteinchoice6))
        avg1 <- sum1/len1
        avg2 <- sum2/len2
        avg3 <- sum3/len3
    }

    line1x <- c(.7,1.3)
    line2x <- c(1.7, 2.3)
    line3x <- c(2.7, 3.3)
    line1y <- c(avg1,avg1)
    line2y <- c(avg2,avg2)
    line3y <- c(avg3,avg3)

    colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                            varA = "Accent",
                                            varB = "Set1",
                                            varC = "Dark2",
                                            varD = "Blues",
                                            varE = "Reds",
                                            varF = "Greens",
                                            varG = "Pastel1"
                                      )#endSwitch
                            )#endBrewerPal
    if(input$colorType == "varB"){
      colorvec <- c(colorvec[2],colorvec[1],colorvec[3])
    } else {
      colorvec <- c(colorvec[1],colorvec[2],colorvec[3])
    }
    beeswarm(percentCVTotal, main="Unchanging Peptides", xlab = NULL, col="white", xaxt='n', yaxt='n', method = input$methodchoice,
    pch = if(input$pointshape){16}else{21}, cex = input$pointsize, lwd = 4, ylim = c(ymin = 0, ymax = 110))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback){"black"} else {"white"})
    lines(c(0,4), c(20,20), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
    lines(c(0,4), c(40,40), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
    lines(c(0,4), c(60,60), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
    lines(c(0,4), c(80,80), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
    lines(c(0,4), c(100,100), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
    marks <- c(20,40,60,80,100)
    title(ylab = "Coefficient of Variation (CV)", cex.lab = 1.5)
    axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1, cex.axis = 1.2, col = if(input$blackback){"white"} else {"black"})
    axis(1, at = c(1,2,3), labels = format(c(toupper(input$proteinchoice4), toupper(input$proteinchoice5), toupper(input$proteinchoice6))), cex.axis = 1.5, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    beeswarm(percentCVTotal, main="Unchanging Peptides", xlab = NULL, col=colorvec, xaxt='n', yaxt='n', method = input$methodchoice,
    pch = if(input$pointshape){16}else{1}, lwd = 2 , cex = input$pointsize, ylim = c(ymin = 0, ymax = 110))
    par(new = T)
    text(.6,80, "%CV:", cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    text(.6,100, "Peptides:", cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    text(1,100, paste("n =", len1), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    text(2,100, paste("n =", len2), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    text(3,100, paste("n =", len3), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    text(1,80, paste(round(avg1,1), "%", " +/- ", round(sd(percentCVvec1),1), sep=""), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    text(2,80, paste(round(avg2,1), "%", " +/- ", round(sd(percentCVvec2),1), sep=""), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    text(3,80, paste(round(avg3,1), "%", " +/- ", round(sd(percentCVvec3),1), sep=""), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
    par(new = T)
    lines(line1x,line1y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
    lines(line2x,line2y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
    lines(line3x,line3y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
    par(new = T)

    dev.off()
   })

  #Settings Tab  -----------------------------------------
  output$proteinchoice1output <- renderText({
        namesvec <- getPepNames()
        if (toupper(input$proteinchoice1) %in% unlist(namesvec)){
            paste("Success! KO Protein 1 is now ", toupper(input$proteinchoice1))
        } else {
            "Sorry, protein not recognized / not found."
        }
  })
  output$proteinchoice2output <- renderText({
      namesvec <- getPepNames()
      if (toupper(input$proteinchoice2) %in% unlist(namesvec)){
          paste("Success! KO Protein 2 is now ", toupper(input$proteinchoice2))
      } else {
          "Sorry, protein not recognized / not found."
      }
  })
  output$proteinchoice3output <- renderText({
      namesvec <- getPepNames()
      if (toupper(input$proteinchoice3) %in% unlist(namesvec)){
          paste("Success! KO Protein 3 is now ", toupper(input$proteinchoice3))
      } else {
          "Sorry, protein not recognized / not found."
      }
  })
  output$proteinchoice4output <- renderText({
        namesvec <- getPepNames()
        if (toupper(input$proteinchoice4) %in% unlist(namesvec)){
            paste("Success! Unchanging Protein 1 is now ", toupper(input$proteinchoice4))
        } else {
            "Sorry, protein not recognized / not found."
        }
  })
  output$proteinchoice5output <- renderText({
        namesvec <- getPepNames()
        if (toupper(input$proteinchoice5) %in% unlist(namesvec)){
            paste("Success! Unchanging Protein 1 is now ", toupper(input$proteinchoice5))
        } else {
            "Sorry, protein not recognized / not found."
        }
  })
  output$proteinchoice6output <- renderText({
        namesvec <- getPepNames()
        if (toupper(input$proteinchoice6) %in% unlist(namesvec)){
            paste("Success! Unchanging Protein 3 is now ", toupper(input$proteinchoice6))
        } else {
            "Sorry, protein not recognized / not found."
        }
  })
  output$settingsHeader <- renderText({"Filter Settings:"})
  output$settings1 <- renderText({"Protein Level Graph Settings:"})
  output$settings2 <- renderText({"Peptide Level Graph Settings:"})
  output$testprotplot <- renderPlot({
    colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                    varA = "Accent",
                                                    varB = "Set1",
                                                    varC = "Dark2",
                                                    varD = "Blues",
                                                    varE = "Reds",
                                                    varF = "Greens",
                                                    varG = "Pastel1"
                                              )#endSwitch
                                    )#endBrewerPal
            if(input$colorType == "varB"){
              mycolors <- c(colorvec[2],colorvec[2],colorvec[2],colorvec[1],colorvec[1],colorvec[1],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            } else {
              mycolors <- c(colorvec[1],colorvec[1],colorvec[1],colorvec[2],colorvec[2],colorvec[2],colorvec[3],colorvec[3],colorvec[3],colorvec[4],colorvec[4])
            }
            spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1,.5,.1)
            plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C", "131N", "131C")
            names <- getPepNames()
            peptidedata <- getFiltPepDataOff()
            namesvec <- c(t(names))

            protname <- toupper(input$proteinchoice1)
            currentnamesvec <- which(namesvec == protname)
            totaldatatable <- peptidedata[currentnamesvec,2:12]
            totaldatatable <- colSums(totaldatatable)

            if(values$plexNum == 9)
            {
              plotlabels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C")
              mycolors <- mycolors[-((length(mycolors)-1):length(mycolors))]
              totaldatatable <- totaldatatable[-((length(totaldatatable)-1):length(totaldatatable))]
              spaces <- c(0,.1,.1,.5,.1,.1,.5,.1,.1)
            }

            #Graphs
            barplot(totaldatatable, main=protname, col="white", names.arg = plotlabels, space = spaces);par(new=T)
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback2){"black"} else {"white"}, border = FALSE);par(new=T)
            barplot(totaldatatable, main=protname, col=mycolors, names.arg = plotlabels, space = spaces, font.lab = 2, ylab = "TMT Summed Signal-To-Noise")
            dev.off()
    })
  output$peptidetestplot <- renderPlot({
    peptidedatatable <- getFiltPepDataOff()
    pepnames <- peptidedatatable[1]
    pepdata <- peptidedatatable[2:12]
    if(values$plexNum == 9)
    {
        totalinterfreeindexvec <- NULL
        sum1 <- NULL
        sum2 <- NULL
        sum3 <- NULL
        len1 <- NULL
        len2 <- NULL
        len3 <- NULL

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice1))
        cutpepdata <- pepdata[pepindex,]

        if (input$kolocations1 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations1 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations1 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum1 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- ((interfreeindexvec))
        len1 <- nrow(cutpepdata)

        interfreeindexvec1 <- interfreeindexvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice2))
        cutpepdata <- pepdata[pepindex,]
        if (input$kolocations2 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations2 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations2 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum2 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
        len2 <- nrow(cutpepdata)

        interfreeindexvec2 <- interfreeindexvec

        namesvec <- c(t(pepnames))
        pepindex <- which(namesvec == toupper(input$proteinchoice3))
        cutpepdata <- pepdata[pepindex,]
        if (input$kolocations3 == "varA"){
          knockout <- 1:3
          notknockout <- 4:9
        } else if (input$kolocations3 == "varB"){
          knockout <- 4:6
          notknockout <- c(1,2,3,7,8,9)
        } else if (input$kolocations3 == "varC"){
          knockout <- 7:9
          notknockout <- 1:6
        }
        tmtkosum <- NULL #list of KO's
        tmtnokosum <- NULL #list of no KO's
        interfreeindexvec <- NULL
        for (i in 1:nrow(cutpepdata)){
          tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
          tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
          interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/6)))
        }
        sum3 <- do.call(sum, as.list(interfreeindexvec))
        totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
        len3 <- nrow(cutpepdata)


                          interfreeindexvec3 <- interfreeindexvec


                          totalinterfreeindexvec <- cbind.fill(interfreeindexvec1, interfreeindexvec2, interfreeindexvec3, fill = NA)

                          #plot(interfreeindexvec, ylim=c(0,1.2), xaxt='n', yaxt='n')

                          colnames(totalinterfreeindexvec) <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                          #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
                          #marks <- c(0.2,0.4,0.6,0.8,1.0)
                          #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
                          #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
                          #par(new=T)

                          avg1 <- sum1/len1
                          avg2 <- sum2/len2
                          avg3 <- sum3/len3

                          line1x <- c(.7,1.3)
                          line2x <- c(1.7, 2.3)
                          line3x <- c(2.7, 3.3)
                          line1y <- c(avg1,avg1)
                          line2y <- c(avg2,avg2)
                          line3y <- c(avg3,avg3)
    }
    else
    {
      totalinterfreeindexvec <- NULL
      sum1 <- NULL
      sum2 <- NULL
      sum3 <- NULL
      len1 <- NULL
      len2 <- NULL
      len3 <- NULL

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice1))
      cutpepdata <- pepdata[pepindex,]

      if (input$kolocations1 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations1 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations1 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum1 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- ((interfreeindexvec))
      len1 <- nrow(cutpepdata)

      interfreeindexvec1 <- interfreeindexvec

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice2))
      cutpepdata <- pepdata[pepindex,]
      if (input$kolocations2 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations2 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations2 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum2 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
      len2 <- nrow(cutpepdata)

      interfreeindexvec2 <- interfreeindexvec

      namesvec <- c(t(pepnames))
      pepindex <- which(namesvec == toupper(input$proteinchoice3))
      cutpepdata <- pepdata[pepindex,]
      if (input$kolocations3 == "varA"){
        knockout <- 1:3
        notknockout <- c(10,11)
      } else if (input$kolocations3 == "varB"){
        knockout <- 4:6
        notknockout <- c(10,11)
      } else if (input$kolocations3 == "varC"){
        knockout <- 7:9
        notknockout <- c(10,11)
      }
      tmtkosum <- NULL #list of KO's
      tmtnokosum <- NULL #list of no KO's
      interfreeindexvec <- NULL
      for (i in 1:nrow(cutpepdata)){
        tmtkosum <- c(tmtkosum, do.call(sum, cutpepdata[i,knockout]))
        tmtnokosum <- c(tmtnokosum, do.call(sum, cutpepdata[i,notknockout]))
        interfreeindexvec <- c(interfreeindexvec, 1 - ((tmtkosum[i]/3)/(tmtnokosum[i]/2)))
      }
      sum3 <- do.call(sum, as.list(interfreeindexvec))
      totalinterfreeindexvec <- cbind(totalinterfreeindexvec, ((interfreeindexvec)))
      len3 <- nrow(cutpepdata)


                        interfreeindexvec3 <- interfreeindexvec


                        totalinterfreeindexvec <- cbind.fill(interfreeindexvec1, interfreeindexvec2, interfreeindexvec3, fill = NA)

                        #plot(interfreeindexvec, ylim=c(0,1.2), xaxt='n', yaxt='n')

                        colnames(totalinterfreeindexvec) <- c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))
                        #plot(line1x,line1y, type = 'l', xlab = NULL, ylab = NULL, xaxt='n', yaxt='n', ylim=c(0,1.2), xlim=c(.9,2.1))
                        #marks <- c(0.2,0.4,0.6,0.8,1.0)
                        #axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1)
                        #axis(1, at = c(1,1.5,2), labels = format(c("MET6", "PFK2", "URA2")))
                        #par(new=T)

                        avg1 <- sum1/len1
                        avg2 <- sum2/len2
                        avg3 <- sum3/len3

                        line1x <- c(.7,1.3)
                        line2x <- c(1.7, 2.3)
                        line3x <- c(2.7, 3.3)
                        line1y <- c(avg1,avg1)
                        line2y <- c(avg2,avg2)
                        line3y <- c(avg3,avg3)
    }

                      colorvec <- brewer.pal(4,"Set3"<-switch(input$colorType,
                                                              varA = "Accent",
                                                              varB = "Set1",
                                                              varC = "Dark2",
                                                              varD = "Blues",
                                                              varE = "Reds",
                                                              varF = "Greens",
                                                              varG = "Pastel1"
                                                        )#endSwitch
                                              )#endBrewerPal
                    if(input$colorType == "varB"){
                        colorvec <- c(colorvec[2],colorvec[1],colorvec[3])
                      } else {
                        colorvec <- c(colorvec[1],colorvec[2],colorvec[3])
                      }
                    beeswarm(totalinterfreeindexvec, main="TKO Peptides", xlab = NULL, col="white", xaxt='n', yaxt='n', method = input$methodchoice,
                    pch = if(input$pointshape){16}else{21}, cex = input$pointsize, lwd = 4, ylim = c(ymin = 0, ymax = 1.1))
                    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = if(input$blackback){"black"} else {"white"})
                    lines(c(0,4), c(.2,.2), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(.4,.4), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(.6,.6), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(.8,.8), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(c(0,4), c(1,1), lwd = .5, col = if(input$blackback){"white"} else {"gray"})
                    lines(line1x,line1y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
                    lines(line2x,line2y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
                    lines(line3x,line3y, lwd = 5, col = if(input$blackback){"white"} else {"black"})
                    marks <- c(0.2,0.4,0.6,0.8,1.0)
                    title(ylab = "IFI (Interference-Free Index)", cex.lab = 1.5)
                    axis(2, at = marks, labels = format(marks, scientific = FALSE), las = 1, cex.axis = 1.2, col = if(input$blackback){"white"} else {"black"})
                    axis(1, at = c(1,2,3), labels = format(c(toupper(input$proteinchoice1), toupper(input$proteinchoice2), toupper(input$proteinchoice3))), cex.axis = 1.5, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    beeswarm(totalinterfreeindexvec, main="TKO Peptides", xlab = NULL, col=colorvec, xaxt='n', yaxt='n', method = input$methodchoice,
                    pch = if(input$pointshape){16}else{1}, lwd = 2 , cex = input$pointsize, ylim = c(ymin = 0, ymax = 1.1))
                    par(new = T)
                    text(.5,.2, "IFI:", cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(.6,.1, "Peptides:", cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(1,.1, paste("n =", len1), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(2,.1, paste("n =", len2), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(3,.1, paste("n =", len3), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(1,.2, paste(round(avg1,3), "+/-", round(sd(unlist(totalinterfreeindexvec[1]), na.rm=TRUE), 3)), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(2,.2, paste(round(avg2,3), "+/-", round(sd(unlist(totalinterfreeindexvec[2]), na.rm=TRUE), 3)), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
                    par(new = T)
                    text(3,.2, paste(round(avg3,3), "+/-", round(sd(unlist(totalinterfreeindexvec[3]), na.rm=TRUE), 3)), cex = 1.7, col = if(input$blackback){"white"} else {"black"})
            dev.off()
  })

  #Help Tab -----------------------------------------------
  output$tkout_pepview_example1.tsv <- downloadHandler(
    filename <- "tkout_pepview_example1.tsv",
    content <- function(file) {
      file.copy("www/tkout_pepview_example1.tsv", file)
    }
    )
  output$titleheader <- renderText({
    return("A Triple Knockout (TKO) Proteomics Standard for Diagnosing Ion Interference in Isobaric Labeling Experiments.
    (J Am Soc Mass Spectrom., 2016)")
    })
  output$introtextoutheader <- renderText({
    to_output <- "ABSTRACT"
    })
  output$introtextout <- renderText({
    to_output <- "Isobaric labeling is a powerful strategy for quantitative mass spectrometry-based proteomic investigations. A complication of such analyses has been the co-isolation of multiple analytes of similar mass-to-charge resulting in the distortion of relative protein abundance measurements across samples. When properly implemented, synchronous precursor selection and triple-stage mass spectrometry (SPS-MS3) can reduce the occurrence of this phenomenon, referred to as ion interference. However, no diagnostic tool is available currently to rapidly and accurately assess ion interference. To address this need, we developed a multiplexed tandem mass tag (TMT)-based standard, termed the triple knockout (TKO). This standard is comprised of three yeast proteomes in triplicate, each from a strain deficient in a highly abundant protein (Met6, HIS4, or Ura2). The relative abundance patterns of these proteins, which can be inferred from dozens of peptide measurements can demonstrate ion interference in peptide quantification. We expect no signal in channels where the protein is knocked out, permitting maximum sensitivity for measurements of ion interference against a null background. Here, we emphasize the need to investigate further ion interference-generated ratio distortion and promote the TKO standard as a tool to investigate such issues."
    })

  output$instructionnote <- renderUI({
              HTML("<br/>NOTE: If experiencing long load times, try switching your web browser. This app is best viewed with Chrome, Firefox, Opera and Safari.<br/>")
              })

  #Instrument Performance Tab -----------------------------
  output$peakwidthGraph <- renderPlot({
    
    pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_peak_width"))
    pwdata <- as.matrix(pwdata)
    par(lwd=.5)
    hist(pwdata, breaks = 75, main = "Peak Width", xlab = "Minutes", ylab = "Counts", col = "#FF4500", border = input$snapview_bordercolor)
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- length(pwdata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$ppmGraph <- renderPlot({
    ppmdata <- getPotpourriColumn(which(totalcolumns() == "ppm"))
    ppmdata <- as.matrix(ppmdata)
    par(lwd=.5)
    MyMean <- mean(ppmdata)
    MyMedian <- median(ppmdata)
    MySd <- sd(ppmdata)
    xmax <- max(ppmdata)
    xmin <- min(ppmdata)
    totnum <- length(ppmdata)
    hist(ppmdata, breaks = 100, main = "PPM", xlab = "Bins (PPM)", ylab = "Counts", col = "#00BFFF", border = input$snapview_bordercolor, xlim = c(-15,15))
    par(new = T)
    rng <- par("usr")
    legend(3,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$xcorrGraph <- renderPlot({
    xcorrdata <- getPotpourriColumn(which(totalcolumns() == "peptide_score1"))
    xcorrdata <- as.matrix(xcorrdata)
    par(lwd=.5)
    hist(xcorrdata, breaks = 75, main = "XCorr", xmin = 0, xlab = "Bins (Peptide Score)", ylab = "Counts", col = "#32CD32", border = input$snapview_bordercolor)
    MyMean <- mean(xcorrdata)
    MyMedian <- median(xcorrdata)
    MySd <- sd(xcorrdata)
    xmax <- max(xcorrdata)
    xmin <- min(xcorrdata)
    totnum <- length(xcorrdata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    
    dev.off()
    })
  output$difscoreGraph <- renderPlot({
    difscoredata <- getPotpourriColumn(which(totalcolumns() == "peptide_score2"))
    difscoredata <- as.matrix(difscoredata)
    par(lwd=.5)
    hist(difscoredata, breaks = 75, main = "Diff Score", xlab = "Bins (Peptide Score Difference)", ylab = "Counts", col = "#DC143C", border = input$snapview_bordercolor)
    MyMean <- mean(difscoredata)
    MyMedian <- median(difscoredata)
    MySd <- sd(difscoredata)
    xmax <- max(difscoredata)
    xmin <- min(difscoredata)
    totnum <- length(difscoredata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$sumsnGraph <- renderPlot({
    sumsndata <- getPotpourriColumn(which(totalcolumns() == "rq_sum_sn"))
    sumsndata <- as.matrix(sumsndata)
    par(lwd=.5)
    hist(sumsndata, breaks = 75, main = "Sum SN", xlab = "Bins (Sum SN)", ylab = "Counts", col = "#9370DB", border = input$snapview_bordercolor)
    MyMean <- mean(sumsndata)
    MyMedian <- median(sumsndata)
    MySd <- sd(sumsndata)
    xmax <- max(sumsndata)
    xmin <- min(sumsndata)
    totnum <- length(sumsndata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$missedcleavagesGraph <- renderPlot({
    misdata <- getPotpourriColumn(which(totalcolumns() == "mis"))
    misdata <- as.matrix(misdata)
    par(lwd=.5)
    hist(misdata, breaks = c(0,1,2), main = "Missed Cleavages", xlab = "Bins (Missed Cleavages)", ylab = "Counts", col = "#FFA500", border = input$snapview_bordercolor)
    MyMean <- mean(misdata)
    MyMedian <- median(misdata)
    MySd <- sd(misdata)
    xmax <- max(misdata)
    xmin <- min(misdata)
    totnum <- length(misdata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$ms3ioninjectiontimeGraph <- renderPlot({
    ms3ioninjectiontimedata <- getPotpourriColumn(which(totalcolumns() == "rq_ion_injection_time"))
    ms3ioninjectiontimedata <- as.matrix(ms3ioninjectiontimedata)
    par(lwd=.5)
    hist(ms3ioninjectiontimedata, breaks = 75, main = "MS3 Ion Injection Time", xlab = "Bins (MS3 Ion Injection Time)", ylab = "Counts", col = "#FFFF00", border = input$snapview_bordercolor)
    MyMean <- mean(ms3ioninjectiontimedata)
    MyMedian <- median(ms3ioninjectiontimedata)
    MySd <- sd(ms3ioninjectiontimedata)
    xmax <- max(ms3ioninjectiontimedata)
    xmin <- min(ms3ioninjectiontimedata)
    totnum <- length(ms3ioninjectiontimedata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$ms2ioninjectiontimeGraph <- renderPlot({
    ms2ioninjectiontimedata <- getPotpourriColumn(which(totalcolumns() == "rq_ms2_ion_inj_time"))
    ms2ioninjectiontimedata <- as.matrix(ms2ioninjectiontimedata)
    par(lwd=.5)
    hist(ms2ioninjectiontimedata, breaks = 75, main = "MS2 Ion Injection Time", xlab = "Bins (MS2 Ion Injection Time)", ylab = "Counts", col = "#FF1493", border = input$snapview_bordercolor)
    MyMean <- mean(ms2ioninjectiontimedata)
    MyMedian <- median(ms2ioninjectiontimedata)
    MySd <- sd(ms2ioninjectiontimedata)
    xmax <- max(ms2ioninjectiontimedata)
    xmin <- min(ms2ioninjectiontimedata)
    totnum <- length(ms2ioninjectiontimedata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$isospecGraph <- renderPlot({
    isospecdata <- getPotpourriColumn(which(totalcolumns() == "rq_isolation_specificity"))
    isospecdata <- as.matrix(isospecdata)
    par(lwd=.5)
    hist(isospecdata, breaks = 100, main = "Isolation Specificity", xlab = "Bins (Isolation Specificity)", ylab = "Counts", col = "#5F9EA0", border = input$snapview_bordercolor)
    MyMean <- mean(isospecdata)
    MyMedian <- median(isospecdata)
    MySd <- sd(isospecdata)
    xmax <- max(isospecdata)
    xmin <- min(isospecdata)
    totnum <- length(isospecdata)
    par(new = T)
    rng <- par("usr")
    legend(7*rng[2]/12,rng[4],legend = c(paste("Mean         ", round(MyMean, 3)),
                                      paste("Median      ",round(MyMedian, 3)),
                                      paste("Minimum   ", xmin),
                                      paste("Maximum  ", xmax),
                                      paste("Std.Dev     ", round(MySd, 3)),
                                      paste("Count       ", totnum)),
                  bty = "n")
    dev.off()
    })
  output$precursormaxGraph <- renderPlot({
    premaxdata <- getPotpourriColumn(which(totalcolumns() == "rq_precursor_max_intensity"))
    premaxdata <- as.matrix(premaxdata)
    par(lwd=.5)
    hist(premaxdata, breaks = 100, main = "Precursor Max Intensity", xlab = "Bins (Precursor Max Intensity)", ylab = "Counts", col = "#A9A9A9", border = input$snapview_bordercolor)
    MyMean <- mean(premaxdata)
    MyMedian <- median(premaxdata)
    MySd <- sd(premaxdata)
    xmax <- max(premaxdata)
    xmin <- min(premaxdata)
    totnum <- length(premaxdata)
    par(new = T)
    rng <- par("usr")
    legend(5*rng[2]/12,rng[4],legend = c(paste("Mean         ", formatC(MyMean, format = "e", digits = 2)),
                                      paste("Median      ", formatC(MyMedian, format = "e", digits = 2)),
                                      paste("Minimum   ", formatC(as.numeric(xmin), format = "e", digits = 2)),
                                      paste("Maximum  ", formatC(as.numeric(xmax), format = "e", digits = 2)),
                                      paste("Std.Dev     ", formatC(MySd, format = "e", digits = 2)),
                                      paste("Count       ", totnum)),
                  bty = "n")
   dev.off()
   })
  output$chromaTable <- renderPlot({
        plot(as.vector(t(totalchromadata()[1])),as.vector(t(totalchromadata()[2])), xlab = "Retention Time (minutes)", main = "Base Peak Chromatogram", ylab = "Intensity", type = 'l', lwd = 2, col = "#4682B4")
      dev.off()
      })
  output$ms2persecondTable <- renderPlot({
        #X -- rt minutes
        #Y -- add num hits to 'counts'
        rt <- getPotpourriColumn(which(totalcolumns() == "rt"))
        counts <- NULL
        min <- 1
        totmin <- NULL
        prev <- 0

        for(i in 1:nrow(rt))
        {
          if(rt[i,1] >= min)
          {
            counts <- c(counts, (i - prev)/60)
            prev <- i
            totmin <- c(totmin, min)
            min <- min + 1
          }
        }

        plot(totmin, counts, xlab = "Retention Time (minutes)", main = "Matched MS2 per Second", ylab = "Counts", type = 'b', lwd = 2, col = "#ffa500")
        par(new=T)
        runningavg <- filter(counts, rep(1/3, 3), sides=2)
        lines(totmin, runningavg, col="#ff6347", lwd=5)
      dev.off()
      })
  output$statsTable <- renderTable({
    df <- totalstatsdata()
    finaldf <- data.frame(c("Unique Peptides", "Validated Reverse Hits", "Total Peptides", "Total Protein", "Sensitivity", "Success Rate"),#, "Contaminant Peptides"),
                          c(df[3,20],df[3,12],round(df[3,19],0),round(df[3,21],0),paste0(round(df[3,13],2),"%"),paste0(round(df[3,26],2),"%")))#,"?"))
    return(finaldf)
    }, include.colnames=FALSE, include.rownames=FALSE, striped=TRUE, bordered=TRUE)
  output$summaryStatsTable <- renderTable({
    df <- data.frame(c(round(mean(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_score1")))),3),
                      round(median(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_score1")))),3),
                      round(min(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_score1")))),3),
                      round(max(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_score1")))),3),
                      round(sd(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_score1")))),3)), #XCorr
                      c(round(mean(as.matrix(getPotpourriColumn(which(totalcolumns() == "ppm")))),3),
                      round(median(as.matrix(getPotpourriColumn(which(totalcolumns() == "ppm")))),3),
                      round(min(as.matrix(getPotpourriColumn(which(totalcolumns() == "ppm")))),3),
                      round(max(as.matrix(getPotpourriColumn(which(totalcolumns() == "ppm")))),3),
                      round(sd(as.matrix(getPotpourriColumn(which(totalcolumns() == "ppm")))),3)), #PPM
                      c(round(mean(as.matrix(getPotpourriColumn(which(totalcolumns() == "mis")))),3),
                      round(median(as.matrix(getPotpourriColumn(which(totalcolumns() == "mis")))),3),
                      round(min(as.matrix(getPotpourriColumn(which(totalcolumns() == "mis")))),3),
                      round(max(as.matrix(getPotpourriColumn(which(totalcolumns() == "mis")))),3),
                      round(sd(as.matrix(getPotpourriColumn(which(totalcolumns() == "mis")))),3)), #Missed Cleavages
                      c(round(mean(as.matrix(getPotpourriColumn(which(totalcolumns() == "charge")))),3),
                      round(median(as.matrix(getPotpourriColumn(which(totalcolumns() == "charge")))),3),
                      round(min(as.matrix(getPotpourriColumn(which(totalcolumns() == "charge")))),3),
                      round(max(as.matrix(getPotpourriColumn(which(totalcolumns() == "charge")))),3),
                      round(sd(as.matrix(getPotpourriColumn(which(totalcolumns() == "charge")))),3)), #Charge
                      c(round(mean(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_length")))),3),
                      round(median(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_length")))),3),
                      round(min(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_length")))),3),
                      round(max(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_length")))),3),
                      round(sd(as.matrix(getPotpourriColumn(which(totalcolumns() == "peptide_length")))),3))) #Peptide Length
    df <- t(df)
    rownames(df) <- c("XCorr","PPM", "Missed Cleavages", "Charge", "Peptide Length")
    colnames(df) <- c("Mean","Median","Minimum", "Maximum", "Std Dev")

    return(df)
    }, include.rownames=TRUE, border=TRUE)
  output$stats2Table <- renderTable({
      df <- totalstatsdata()
      finaldf <- data.frame(
      c(values$sid,df[3,5],df[3,6],df[3,8],df[3,10],df[3,11],df[3,14])
      )
      rownames(finaldf) <- c("Search ID", "Forward Hits", "Reverse Hits", "Total MS/MS", "Set Bit", "Validated Forward Hits", "TP Max")
      return(t(finaldf))
    }, include.colnames=TRUE, include.rownames=FALSE, border=TRUE, striped=TRUE)
  output$stats3Table <- renderTable({
      df <- totalstatsdata()
      finaldf <- data.frame(
      format(c(trunc(df[3,9], prec = 0),round(df[3,15],2),round(df[3,16],2),round(df[3,17],2),round(df[3,18],2),round(df[3,22],2),round(df[3,24],2)), scientific=FALSE)
      )
      rownames(finaldf) <- c("Forward + Reverse","Lower Bound","Upper Bound","Mean","Std. Dev","Protein FDR","Peptide FDR")
      return(t(finaldf))
    }, include.colnames=TRUE, include.rownames=FALSE, border=TRUE, striped=TRUE)
  output$ppmvsrtGraph <- renderPlot({
    rt <- totaldata()[which(totalcolumns() == "rt")]
    ppm <- totaldata()[which(totalcolumns() == "ppm")]
    rownames(rt) <- rownames(ppm)
    DF <- data.frame(cbind(rt, ppm))
    colnames(DF) <- c("rt", "ppm")
    plot(DF$rt, DF$ppm, xlab = "RT (mins)", ylab = "PPM", col="SlateBlue", main="PPM vs Retention Time")
    par(new=T)
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    par(new=T)
    plot(DF$rt, DF$ppm, xlab = "RT (mins)", ylab = "PPM", col="SlateBlue", main="PPM vs Retention Time")
    dev.off()
    })


  #Comparison Tab--------------------------------------------
  output$comparisonTitle <- renderUI({
    HTML(paste(color(bold("Instrument Comparison:"),"black")))
    })
  output$comparisonNote <- renderUI({
    HTML(paste(color(bold("To compare against a previous run, input the search ID from the previous session into the box and hit submit.</br>
    Search ID's can be pulled from previous URL's ('?pqid=__&sid=__') or the bottom of the page."),"black")))
    })
  output$comparisonWarning <- renderUI({
    HTML(paste(color(bold("WARNING: Once 'Confirm' is pressed, the application will take about a minute to fetch data and will be unresponsive.</br>Please wait for the graphs to load before manipulating other data."), "#D6B521")))
    })
  output$pwTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("Peak Width:"),"#FF4500")))
    })
  output$peakWidthGraphCompare <- renderPlot({
      shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_peak_width"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_peak_width"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#FF4500", ylab="Minutes")
      dev.off()
      return(b)
   })
  output$peakWidthTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_peak_width"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_peak_width"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
    })
  output$ppmTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("PPM:"),"#00BFFF")))
    })
  output$ppmGraphCompare <- renderPlot({
      shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "ppm"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "ppm"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#00BFFF", ylab="Bins(PPM)")
      dev.off()
      return(b)
    })
  output$ppmTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "ppm"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "ppm"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
    })
  output$xcTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("XCorr:"),"#32CD32")))
  })
  output$xcorrGraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "peptide_score1"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "peptide_score1"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#32CD32", ylab="Bins (Peptide Score)")
      dev.off()
      return(b)
  })
  output$xcorrTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "peptide_score1"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "peptide_score1"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })
  output$ssnTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("Sum Signal Noise:"),"#9370DB")))
  })
  output$ssnGraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_sum_sn"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_sum_sn"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#9370DB", ylab="Bins (SumSN)")
      dev.off()
      return(b)
  })
  output$ssnTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_sum_sn"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_sum_sn"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })
  output$m3Title <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("MS3 Ion Injection Time:"),"#FFFF00")))
  })
  output$ms3GraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_ion_injection_time"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_ion_injection_time"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#FFFF00", ylab="Bins (MS3)")
      dev.off()
      return(b)
  })
  output$ms3TableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_ion_injection_time"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_ion_injection_time"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })
  output$m2Title <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("MS2 Ion Injection Time:"),"#FF1493")))
  })
  output$ms2GraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_ms2_ion_inj_time"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_ms2_ion_inj_time"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#FF1493", ylab="Bins (MS2)")
      dev.off()
      return(b)
  })
  output$ms2TableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_ms2_ion_inj_time"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_ms2_ion_inj_time"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })
  output$pmiTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("Precursor Max Intensity:"),"#A9A9A9")))
  })
  output$pmGraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_precursor_max_intensity"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_precursor_max_intensity"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#A9A9A9", ylab="Bins (Prec. Max Intensity)")
      dev.off()
      return(b)
  })
  output$pmTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_precursor_max_intensity"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_precursor_max_intensity"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(formatC(MyMean, format = "e", digits = 2), 
                        formatC(MyMedian, format = "e", digits = 2),
                        formatC(MySd, format = "e", digits = 2),
                        formatC(xmax, format = "e", digits = 2),
                        formatC(xmin, format = "e", digits = 2),
                        totnum),
                      c(formatC(MyMeanNew, format = "e", digits = 2), 
                        formatC(MyMedianNew, format = "e", digits = 2),
                        formatC(MySdNew, format = "e", digits = 2),
                        formatC(xmaxNew, format = "e", digits = 2),
                        formatC(xminNew, format = "e", digits = 2),
                        totnumNew),
                      c(formatC(MyMeanDif, format = "e", digits = 2), 
                        formatC(MyMedianDif, format = "e", digits = 2),
                        formatC(MySdDif, format = "e", digits = 2),
                        formatC(xmaxDif, format = "e", digits = 2),
                        formatC(xminDif, format = "e", digits = 2),
                        totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })
  output$dsTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("Diff Score:"),"#DC143C")))
  })
  output$dsGraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "peptide_score2"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "peptide_score2"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#DC143C", ylab="Bins (Diff. Score)")
      dev.off()
      return(b)
  })
  output$dsTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "peptide_score2"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "peptide_score2"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })
  output$mcTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("Missed Cleavages:"),"#FFA500")))
  })
  output$mcGraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "mis"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "mis"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      table.data <- data.frame(values = c(MyMean,MyMeanNew), names = c("Current Data", "Comparison Data"))

      par(lwd=.5)
      barplot(table.data$values, main = "Mean Missed Cleavages", beside = T, ylab = "Counts", col ="#FFA500", border = input$snapview_bordercolor)
      dev.off()
  })
  output$mcTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "mis"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "mis"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })
  output$isTitle <- renderUI({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    HTML(paste(color(bold("Isolation Specificity:"),"#5F9EA0")))
  })
  output$isGraphCompare <- renderPlot({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
      pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_isolation_specificity"))
      pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_isolation_specificity"))
      pwdata <- as.matrix(pwdata)
      pwdatanew <- as.matrix(pwdatanew)

      if(nrow(pwdata) > nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdata))
        {
          if(i > nrow(pwdatanew))
          {
            pwdatanew <- rbind(pwdatanew, NA)
          }
        }
      }
      if(nrow(pwdata) < nrow(pwdatanew))
      {
        for(i in 1:nrow(pwdatanew))
        {
          if(i > nrow(pwdata))
          {
            pwdata <- rbind(pwdata, NA)
          }
        }
      }

      pwdatatotal <- cbind(pwdata, pwdatanew)
      colnames(pwdatatotal) <- c("Current Data", "Comparison Data")

      #Get Current Data:
      MyMean <- mean(pwdata)
      MyMedian <- median(pwdata)
      MySd <- sd(pwdata)
      xmax <- max(pwdata)
      xmin <- min(pwdata)
      totnum <- length(pwdata)

      #Get "Gold" Data:
      MyMeanNew <- mean(pwdatanew)
      MyMedianNew <- median(pwdatanew)
      MySdNew <- sd(pwdatanew)
      xmaxNew <- max(pwdatanew)
      xminNew <- min(pwdatanew)
      totnumNew <- length(pwdatanew)

      #Get Differences:
      MyMeanDif <- MyMean - MyMeanNew
      MyMedianDif <- MyMedian - MyMedianNew
      MySdDif <- MySd - MySdNew
      xmaxDif <- xmax - xmaxNew
      xminDif <- xmin - xminNew
      totnumDif <- totnum - totnumNew

      b <- boxplot.matrix(pwdatatotal, outline=input$showOutliers,
                          col="#5F9EA0", ylab="Bins (Iso. Spec.)")
      dev.off()
      return(b)
  })
  output$isTableCompare <- renderTable({
    shiny::validate(
              need(!is.null(values$totalgolddata), "")
              )
    pwdata <- getPotpourriColumn(which(totalcolumns() == "rq_isolation_specificity"))
    pwdatanew <- getGoldenPotpourriColumn(which(totalcolumns() == "rq_isolation_specificity"))
    pwdata <- as.matrix(pwdata)
    pwdatanew <- as.matrix(pwdatanew)

    #Get Current Data:
    MyMean <- mean(pwdata)
    MyMedian <- median(pwdata)
    MySd <- sd(pwdata)
    xmax <- max(pwdata)
    xmin <- min(pwdata)
    totnum <- round(length(pwdata),0)

    #Get "Gold" Data:
    MyMeanNew <- mean(pwdatanew)
    MyMedianNew <- median(pwdatanew)
    MySdNew <- sd(pwdatanew)
    xmaxNew <- max(pwdatanew)
    xminNew <- min(pwdatanew)
    totnumNew <- round(length(pwdatanew),0)

    #Get Differences:
    MyMeanDif <- MyMean - MyMeanNew
    MyMedianDif <- MyMedian - MyMedianNew
    MySdDif <- MySd - MySdNew
    xmaxDif <- xmax - xmaxNew
    xminDif <- xmin - xminNew
    totnumDif <- round(totnum - totnumNew,0)

    DF <- data.frame(c("Mean", "Median", "SD", "XMAX", "XMIN", "Total Num"),
                      c(MyMean, MyMedian, MySd, xmax, xmin, totnum),
                      c(MyMeanNew, MyMedianNew, MySdNew, xmaxNew, xminNew, totnumNew),
                      c(MyMeanDif, MyMedianDif, MySdDif, xmaxDif, xminDif, totnumDif))
    colnames(DF) <- c("Label", "Current", "Standard", "Difference")
    return(DF)
  })

  output$progressUI <- renderUI({
    numplots <- length(dev.list())
    cat(unlist(dev.list()), file=stderr())
    progressBar("plotstatus", title = "Application Load:", value = (numplots + 2), total = 64, status = 'danger', display_pct = T)
  })

  output$testTable1 <- renderDataTable({
    return(totalpepdata())
  })

  output$testTable2 <- renderDataTable({
    return(totaldata())
  })

  output$testTable3 <- renderDataTable({
    return(values$totalDataUnfiltered)
  })

  output$testTable4 <- renderDataTable({
    return(totalstatsdata())
  })

}

shinyApp(ui, server)
