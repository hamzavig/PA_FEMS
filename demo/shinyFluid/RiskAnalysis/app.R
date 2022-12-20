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
               
               tabPanel("Interest Rate Risk", fluid = TRUE,
                        
                        tabsetPanel(
                            tabPanel("Risk Reporting", fluid = TRUE,
                                     
                                     fluidRow(
                                         h4("Create Institution", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(3,
                                                textInput(inputId = "instName", 
                                                          label = "Name your Institution:", 
                                                          placeholder = "Name"),
                                                actionButton("createInst", "Create")
                                                ),
                                         column(3,
                                                fileInput(inputId = "ops", 
                                                          label = "Import your Operations:")
                                                ),
                                         column(3,
                                                fileInput(inputId = "anns", 
                                                          label = "Import your ANN Portfolio:")
                                                ),
                                         column(3,
                                                fileInput(inputId = "pams", 
                                                          label = "Import your PAM Portfolio:")
                                                )
                                     ),
                                     fluidRow(
                                         h4("Risk Analysis: Parallel Yield Curve Shift", style = "margin-left: 15px; margin-top: 15px;"),
                                         h5("Market Environment", style = "margin-left: 15px; margin-top: 15px;")
                                     ),
                                     fluidRow(
                                         column(3,
                                                fileInput(inputId = "rfs", 
                                                          label = "Import your Risk Factors:"),
                                                actionButton("importYC", "Import")
                                         )
                                     ),
                                     fluidRow(
                                         column(6,
                                                h5("Default YieldCuve"),
                                                verbatimTextOutput("ycDefaultDetails"),
                                                plotOutput("ycDefaultPlot")
                                         ),
                                         column(6,
                                                h5("Shifted YieldCuve"),
                                                verbatimTextOutput("ycShiftedDetails"),
                                                plotOutput("ycShiftedPlot")
                                         )
                                     ),
                                     fluidRow(
                                         h5("Simulation Input", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(3,
                                                textInput(inputId = "simFrom", 
                                                          label = "From", 
                                                          placeholder = "2022-01-01"),
                                                actionButton("sim", "Run")
                                         ),
                                         column(3,
                                                textInput(inputId = "simTo", 
                                                          label = "To", 
                                                          placeholder = "2030-01-01")
                                         ),
                                         column(4,
                                                numericInput(inputId = "spreadRate", 
                                                          label = "Sprade Rate", 
                                                          value = 0.001,
                                                          min = 0.001,
                                                          max = 0.1,
                                                          step = 0.001)
                                         ),
                                     ),
                                     fluidRow(
                                         h5("Financial Statements", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(6,
                                                h5("Value"),
                                                verbatimTextOutput("valDefault"),
                                                h5("Income"),
                                                verbatimTextOutput("incDefault"),
                                                h5("Liquidity"),
                                                verbatimTextOutput("liqDefault")
                                         ),
                                         column(6,
                                                h5("Value Shifted"),
                                                verbatimTextOutput("valShifted"),
                                                h5("Income Shifted"),
                                                verbatimTextOutput("incShifted"),
                                                h5("Liquidity Shifted"),
                                                verbatimTextOutput("liqShifted")
                                         ),
                                     )
                            ),
                            tabPanel("Contracts Overview", fluid = TRUE,
                                     fluidRow(column(12,
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
                                     fluidRow(column(12,
                                                     textInput(inputId = "cid", 
                                                               label = "Contract ID", 
                                                               placeholder = "ASL0001"),
                                                     actionButton("getEvent", "Show Events")
                                                     )
                                     ),
                                     fluidRow(column(6,
                                                     h5("Events Default"),
                                                     plotOutput("eventsPlotDef"),
                                                     DTOutput("eventsDef")
                                                     ),
                                              column(6,
                                                     h5("Events Shifted"),
                                                     plotOutput("eventsPlotShifted"),
                                                     DTOutput("eventsShifted")
                                                     )
                                     )
                            )
                            
                        )
                        
                ),
               tabPanel("Default Riks", fluid = TRUE)
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$createInst, {
        
        inst <<- createInstitution(input$instName)
        ann_ptf <- samplePortfolio(input$anns$datapath, "contracts")
        pam_ptf <- samplePortfolio(input$pams$datapath, "contracts")
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        ops_ptf <- samplePortfolio(input$ops$datapath, "operations")
        
        inst <<- assignContracts2Tree(inst, ptf)
        inst <<- assignContracts2Tree(inst, ops_ptf)
        
        output$instDefault <- renderPrint({
            print(inst)
        })
        
        leaf_dfs <- getLeafsAsDataFrames(inst)
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('tableTitle_', i)]] <- renderText({leaf_dfs[[i]]$leaf})
        })
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('table_', i)]] <- DT::renderDataTable({leaf_dfs[[i]]$contracts})
        })
        
        instShifted <<- createInstitution("BankShifted")
        ann_ptf <- samplePortfolio(input$anns$datapath, "contracts")
        pam_ptf <- samplePortfolio(input$pams$datapath, "contracts")
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        ops_ptf <- samplePortfolio(input$ops$datapath, "operations")
        
        instShifted <<- assignContracts2Tree(instShifted, ptf)
        instShifted <<- assignContracts2Tree(instShifted, ops_ptf)
        
        output$instShifted <- renderPrint({
            print(instShifted)
        })
    })
    
    
    observeEvent(input$importYC, {
        
        riskFactors <- input$rfs$datapath
        rfList <<- getRFList(riskFactors)
        
        ycDefault <<- rfList[[1]]
        ycShifted <- rfList[[2]]
        
        output$ycDefaultPlot <- renderPlot({
            plot(ycDefault)
        })
        output$ycDefaultDetails <- renderPrint({
            print(ycDefault)
        })
        
        output$ycShiftedPlot <- renderPlot({
            plot(ycShifted)
        })
        output$ycShiftedDetails <- renderPrint({
            print(ycShifted)
        })
        
    })
    
    
    observeEvent(input$sim, {
        
        riskFactors <- input$rfs$datapath
        rfList <<- getRFList(riskFactors)
        ycShifted <- rfList[[2]]
        
        ycShifted <- shiftYieldCurve(ycShifted, input$spreadRate)
        
        output$ycShiftedPlot <- renderPlot({
            plot(ycShifted)
        })
        output$ycShiftedDetails <- renderPrint({
            print(ycShifted)
        })
        
        t0 <- input$simFrom
        tn <- input$simTo
        
        n <- yearFraction(t0, tn)
        t0Year <- as.numeric(substr(t0,1,4))
        tnYear <- as.numeric(substr(tn,1,4))
        by <- timeSequence(t0, by="1 years", length.out=n+2)
        tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                          breakLabs=substr(as.character(by),3,10))
        scale = 1000000
        
        # Case1: Default
        rfDefault <- RFConn()
        add(rfDefault, list(ycDefault))
        
        spreadRateDefault <- 0.0
        cycleDefault <- "P1YL1"
        inst <<- addMarketObject2Contracts(inst, ycDefault, spreadRateDefault, cycleDefault)
        
        inst <<- events(object = inst, riskFactors = rfDefault)
        
        # Case 2: Yield Curve Shift
        rfShifted <- RFConn()
        add(rfShifted, list(ycShifted))
        
        spreadRateShifted <- input$spreadRate
        cycleShifted <- "P1YL1"
        instShifted <<- addMarketObject2Contracts(instShifted, ycShifted, spreadRateShifted, cycleShifted)
        
        instShifted <<- events(object = instShifted, riskFactors = rfShifted)
        
        val <- value(inst, tb, scale=scale, digits=2)
        valShifted <- value(instShifted, tb, scale=scale, digits=2)
        inc <- income(inst, tb, type="marginal", scale=scale, digits=2)
        liq <- liquidity(inst, tb, scale=scale, digits=2)
        incShifted <- income(instShifted, tb, type="marginal", scale=scale, digits=2)
        liqShifted <- liquidity(instShifted, tb, scale=scale, digits=2)
        
        
        output$valDefault <- renderPrint({
            return(val)
        })
        
        output$valShifted <- renderPrint({
            return(valShifted)
        })
        
        output$incDefault <- renderPrint({
            return(inc)
        })

        output$incShifted <- renderPrint({
         return(incShifted)
        })
         
        output$liqDefault <- renderPrint({
            return(liq)
        })

        output$liqShifted <- renderPrint({
            return(liqShifted)
        })
        
    })
    
    
    observeEvent(input$getEvent, {
        
        evsDef <- getEvents(inst, input$cid)
        
        #creation of the desired plot
        output$eventsPlotDef <- renderPlot({
            cashflowPlot(evsDef)
        })
        
        #optional data table of the event list
        output$eventsDef <- renderDataTable({
            evsDef$events_df
        })
        
        evsShifted <- getEvents(instShifted, input$cid)
        
        #creation of the desired plot
        output$eventsPlotShifted <- renderPlot({
            cashflowPlot(evsShifted)
        })
        
        #optional data table of the event list
        output$eventsShifted <- renderDataTable({
            evsShifted$events_df
        })
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
