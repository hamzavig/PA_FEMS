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
                                                fileInput(inputId = "rfsC", 
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
                                                DTOutput('table_13')
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
                            tabPanel("Financial Statements", fluid = TRUE,
                                     
                                     fluidRow(style = "margin: 10px 0px 10px 0px; border: 1px solid lightgrey; border-top: none;",
                                              column(3,
                                                     h4("Balance Sheet"),
                                                     textInput(inputId = "valueT0", 
                                                               label = "From", 
                                                               placeholder = "YYYY-MM-DD"),
                                                     textInput(inputId = "valueTn", 
                                                               label = "To", 
                                                               placeholder = "YYYY-MM-DD"),
                                                     selectInput(inputId = "valueType",
                                                                 label = "Type",
                                                                 choices = c("nominal", "market"),
                                                                 selected = NULL,
                                                                 multiple = FALSE)
                                              ),
                                              column(3,
                                                     h4("Market Environment"),
                                                     fileInput(inputId = "valueRfs", 
                                                               label = "Import your market environment"),
                                                     actionButton("getValue", "Send")
                                              ),
                                              column(6,
                                                     h4("Yield Curve: Market")
                                              )
                                     ),
                                     fluidRow(style = "margin: 10px 0px 10px 0px; border: 1px solid lightgrey; border-top: none;",
                                              column(6,
                                                     verbatimTextOutput("treeValue")
                                              )
                                     )
                            ),
                            tabPanel("Risk Reporting")
                            
                        )
                        
                )
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$send, {
        inst <- createInstitution(input$instName)
        ann_ptf <- samplePortfolio(input$anns$datapath, "contracts")
        pam_ptf <- samplePortfolio(input$pams$datapath, "contracts")
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        
        ops_ptf <- samplePortfolio(input$ops$datapath, "operations")
        
        inst <- assignContracts2Tree(inst, ptf)
        inst <- assignContracts2Tree(inst, ops_ptf)
        
        leaf_dfs <- getLeafsAsDataFrames(inst)
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('tableTitle_', i)]] <- renderText({leaf_dfs[[i]]$leaf})
        })
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('table_', i)]] <- DT::renderDataTable({leaf_dfs[[i]]$contracts})
        })
    })
    
    observeEvent(input$getEvent, {
        
        inst <- createInstitution(input$instName)
        ann_ptf <- samplePortfolio(input$anns$datapath, "contracts")
        pam_ptf <- samplePortfolio(input$pams$datapath, "contracts")
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        
        ops_ptf <- samplePortfolio(input$ops$datapath, "operations")
        
        inst <- assignContracts2Tree(inst, ptf)
        inst <- assignContracts2Tree(inst, ops_ptf)
        
        rfList <- getRFList(input$rfsC$datapath)
        rfCtrs <- RFConn()
        add(rfCtrs, rfList)
        
        inst <- events(object=inst, riskFactors = rfCtrs)
        
        evs <- getEvents(inst, input$CID)
        
        #creation of the desired plot
        output$cashFlowPlot <- renderPlot({
            cashflowPlot(evs)
        })
    
        #optional data table of the event list
        output$CFDF <- renderDataTable({
        evs$events_df
        })
    })
    
    
    observeEvent(input$getValue, {
        
        inst <- createInstitution(input$instName)
        ann_ptf <- samplePortfolio(input$anns$datapath, "contracts")
        pam_ptf <- samplePortfolio(input$pams$datapath, "contracts")
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        
        ops_ptf <- samplePortfolio(input$ops$datapath, "operations")
        
        inst <- assignContracts2Tree(inst, ptf)
        inst <- assignContracts2Tree(inst, ops_ptf)
        
        rfList <- getRFList(input$rfsC$datapath)
        rfCtrs <- RFConn()
        add(rfCtrs, rfList)
        
        inst <- events(object=inst, riskFactors = rfCtrs)
        
        t0 <- input$valueT0
        tn <- input$valueTn
        
        n <- yearFraction(t0, tn)
        t0Year <- as.numeric(substr(t0,1,4))
        tnYear <- as.numeric(substr(tn,1,4))
        by <- timeSequence(t0, by="1 years", length.out=n+1)
        tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                          breakLabs=substr(as.character(by),3,10))
        scale = 1000000
        
        rfList2 <- getRFList(input$valueRfs$datapath)
        rfEnv <- RFConn()
        add(rfEnv, rfList2)
        
        method <- DcEngine(rfEnv)
        
        if (input$valueType == "nominal"){
            val <- value(inst, tb, type="nominal", scale=scale, digits=2)
        }else{
            val <- value(inst, by, type=input$type, method = method, scale=scale, digits=2)
        }
        
        output$valueMarketYC <- renderPlot({
            plot(rfList2[[1]])
        })
        
        output$treeValue <- renderPrint({
            print(val)
        }) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
