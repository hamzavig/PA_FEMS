library(shiny)
library(shinythemes)
library(devtools)
library(DT)
library(data.table)
library(rlist)
devtools::install_github("hamzavig/PA_FEMS") 
library("PAFEMS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("readable"),
    
    #top images 
    img(src = "actus-logo.png", height = 100, width = 220,
        style="float:right; padding-right:25px"),
    img(src="soe.png",height = 138, width = 239),
    
    navbarPage("Risk Analysis and Risk Reporting", fluid = TRUE,
               
               tabPanel("Institution1", fluid = TRUE,
                        
                        tabsetPanel(
                            tabPanel("Overview", fluid = TRUE,
                                     
                                     fluidRow(style = "margin: 10px 0px 10px 0px; border: 1px solid lightgrey; border-top: none;",
                                        column(3,
                                                textInput(inputId = "instName", 
                                                          label = "Name your Institution:", 
                                                          placeholder = "Name"),
                                                fileInput(inputId = "anns", 
                                                          label = "Import your ANN Portfolio:"),
                                                fileInput(inputId = "pams", 
                                                          label = "Import your PAM Portfolio:")
                                                ),
                                         column(3,
                                                fileInput(inputId = "ops", 
                                                          label = "Import your Operations:"),
                                                fileInput(inputId = "rfs", 
                                                          label = "Import your Risk Factors:"),
                                                actionButton("send", "Send")
                                                ),
                                        column(6,
                                               p(style = "text-align:justify",
                                                 "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod 
                                                 tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
                                                 At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, 
                                                 no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, 
                                                 consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et 
                                                 dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo 
                                                 duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
                                               p(style = "text-align:justify",
                                                 "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod 
                                                 tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
                                                 At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, 
                                                 no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, 
                                                 consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et 
                                                 dolore magna aliquyam erat, sed diam voluptua.")
                                        )
                                     ),
                                     fluidRow(style = "margin: 10px 0px 10px 0px; border: 1px solid lightgrey; border-top: none;",
                                         column(12,
                                                h4("Contracts Overview"),
                                                h4(textOutput('tableTitle_1')),
                                                DTOutput('table_1'),
                                                h4(textOutput('tableTitle_2')),
                                                DTOutput('table_2'),
                                                h4(textOutput('tableTitle_3')),
                                                DTOutput('table_3'),
                                                h4(textOutput('tableTitle_4')),
                                                DTOutput('table_4'),
                                                h4(textOutput('tableTitle_5')),
                                                DTOutput('table_5'),
                                                h4(textOutput('tableTitle_6')),
                                                DTOutput('table_6'),
                                                h4(textOutput('tableTitle_7')),
                                                DTOutput('table_7'),
                                                h4(textOutput('tableTitle_8')),
                                                DTOutput('table_8'),
                                                h4(textOutput('tableTitle_9')),
                                                DTOutput('table_9'),
                                                h4(textOutput('tableTitle_10')),
                                                DTOutput('table_10'),
                                                h4(textOutput('tableTitle_11')),
                                                DTOutput('table_11'),
                                                h4(textOutput('tableTitle_12')),
                                                DTOutput('table_12'),
                                                h4(textOutput('tableTitle_13')),
                                                DTOutput('table_13'),
                                                h4(textOutput('tableTitle_14')),
                                                DTOutput('table_14'),
                                                h4(textOutput('tableTitle_15')),
                                                DTOutput('table_15'),
                                                )
                                     )
                            ),
                            tabPanel("Event Details", fluid = TRUE,
                                     fluidRow(style = "margin: 10px 0px 10px 0px; border: 1px solid lightgrey; border-top: none;",
                                              column(12,
                                                     textInput(inputId = "CID", 
                                                               label = "Select Contract:", 
                                                               placeholder = "ABC0001"
                                                     ),
                                                     actionButton("getEvent", "Show Events"),
                                                     plotOutput("cashFlowPlot"),
                                                     DTOutput("CFDF")
                                              )
                                     )
                            ),
                            tabPanel("Financial Statements"),
                            tabPanel("Risk Reporting")
                            
                        )
                        
                )
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # set the ACTUS serverURL
    serverURL = "https://demo.actusfrf.org:8080/"
    
    observeEvent(input$send, {
        inst <- createInstitution(input$instName)
        ann_ptf <- samplePortfolio(input$anns$datapath)
        pam_ptf <- samplePortfolio(input$pams$datapath)
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        inst <- assignContracts2Tree(inst, ptf)
        
        leaf_dfs <- getLeafsAsDataFrames(inst)
        
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('tableTitle_', i)]] <- renderText({leaf_dfs[[i]]$leaf})
        })
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('table_', i)]] <- DT::renderDataTable({leaf_dfs[[i]]$contracts})
        })
    })
    
    observeEvent(input$getEvent, {
        
        ann_ptf <- samplePortfolio(input$anns$datapath)
        pam_ptf <- samplePortfolio(input$pams$datapath)
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        riskFactors <- defineReferenceIndex(input$rfs$datapath)
        c1 <- getContract(ptf, input$CID)
        evs <- generateEventSeries(c1,serverURL, riskFactors)
        
        #creation of the desired plot
        output$cashFlowPlot <- renderPlot({
            cashflowPlot(evs)
        })
    
        #optional data table of the event list
        output$CFDF <- renderDataTable({
        evs$events_df
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
