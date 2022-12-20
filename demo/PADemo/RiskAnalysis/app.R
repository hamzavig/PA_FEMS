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
                            tabPanel("Institution", fluid = TRUE,
                                     
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
                                         column(6,
                                                h5("Institution Structure"),
                                                verbatimTextOutput("instDefault")
                                         )
                                     ),
                                     fluidRow(
                                         column(12,
                                                h5("Contracts Overview"),
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
                            tabPanel("Market", fluid = TRUE,
                                     fluidRow(
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
                                         h5("Parallel Yield Curve Shift", style = "margin-left: 15px; margin-top: 15px;"),
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
                                     )
                            ),
                            tabPanel("Financial Statements", fluid = TRUE,
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
                            tabPanel("Sensitivity", fluid = TRUE,
                                     fluidRow(
                                         column(6,
                                                h5("Financial Ratios"),
                                                verbatimTextOutput("eqrDefault"),
                                                verbatimTextOutput("lcrDefault"),
                                                h5("Present Value & Duration"),
                                                verbatimTextOutput("senDefault")
                                         ),
                                         column(6,
                                                h5("Financial Ratios Shifted"),
                                                verbatimTextOutput("eqrShifted"),
                                                verbatimTextOutput("lcrShifted"),
                                                h5("Present Value & Duration Shifted"),
                                                verbatimTextOutput("senShifted")
                                         ),
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
               tabPanel("Default Risk", fluid = TRUE,
                        
                        tabsetPanel(
                            tabPanel("Institution", fluid = TRUE,
                                     fluidRow(
                                         h4("Create Institutions", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(3,
                                                h5("Create Institution A"),
                                                textInput(inputId = "instNameA", 
                                                          label = "Name your Institution:", 
                                                          placeholder = "Name A"),
                                                fileInput(inputId = "opsA", 
                                                          label = "Import your Operations:"),
                                                fileInput(inputId = "annsA", 
                                                          label = "Import your ANN Portfolio:"),
                                                fileInput(inputId = "pamsA", 
                                                          label = "Import your PAM Portfolio:"),
                                                actionButton("createInstA", "Create")
                                         ),
                                         column(3,
                                                verbatimTextOutput("instA")
                                         ),
                                         column(3,
                                                h5("Create Institution B"),
                                                textInput(inputId = "instNameB", 
                                                          label = "Name your Institution:", 
                                                          placeholder = "Name B"),
                                                fileInput(inputId = "opsB", 
                                                          label = "Import your Operations:"),
                                                fileInput(inputId = "annsB", 
                                                          label = "Import your ANN Portfolio:"),
                                                fileInput(inputId = "pamsB", 
                                                          label = "Import your PAM Portfolio:"),
                                                actionButton("createInstB", "Create")
                                         ),
                                         column(3,
                                                verbatimTextOutput("instB")
                                         )
                                     ),
                                     fluidRow(
                                         column(6,
                                                h5("Contracts Overview A"),
                                                h4(textOutput('tableTitleA_1')),
                                                DTOutput('tableA_1'),
                                                h4(textOutput('tableTitleA_2')),
                                                DTOutput('tableA_2'),
                                                h4(textOutput('tableTitleA_3')),
                                                DTOutput('tableA_3'),
                                                h4(textOutput('tableTitleA_4')),
                                                DTOutput('tableA_4'),
                                                h4(textOutput('tableTitleA_5')),
                                                DTOutput('tableA_5'),
                                                h4(textOutput('tableTitleA_6')),
                                                DTOutput('tableA_6'),
                                                h4(textOutput('tableTitleA_7')),
                                                DTOutput('tableA_7'),
                                                h4(textOutput('tableTitleA_8')),
                                                DTOutput('tableA_8'),
                                                h4(textOutput('tableTitleA_9')),
                                                DTOutput('tableA_9'),
                                                h4(textOutput('tableTitleA_10')),
                                                DTOutput('tableA_10'),
                                                h4(textOutput('tableTitleA_11')),
                                                DTOutput('tableA_11'),
                                                h4(textOutput('tableTitleA_12')),
                                                DTOutput('tableA_12'),
                                                h4(textOutput('tableTitleA_13')),
                                                DTOutput('tableA_13')
                                         ),
                                         column(6,
                                                h5("Contracts Overview B"),
                                                h4(textOutput('tableTitleB_1')),
                                                DTOutput('tableB_1'),
                                                h4(textOutput('tableTitleB_2')),
                                                DTOutput('tableB_2'),
                                                h4(textOutput('tableTitleB_3')),
                                                DTOutput('tableB_3'),
                                                h4(textOutput('tableTitleB_4')),
                                                DTOutput('tableB_4'),
                                                h4(textOutput('tableTitleB_5')),
                                                DTOutput('tableB_5'),
                                                h4(textOutput('tableTitleB_6')),
                                                DTOutput('tableB_6'),
                                                h4(textOutput('tableTitleB_7')),
                                                DTOutput('tableB_7'),
                                                h4(textOutput('tableTitleB_8')),
                                                DTOutput('tableB_8'),
                                                h4(textOutput('tableTitleB_9')),
                                                DTOutput('tableB_9'),
                                                h4(textOutput('tableTitleB_10')),
                                                DTOutput('tableB_10'),
                                                h4(textOutput('tableTitleB_11')),
                                                DTOutput('tableB_11'),
                                                h4(textOutput('tableTitleB_12')),
                                                DTOutput('tableB_12'),
                                                h4(textOutput('tableTitleB_13')),
                                                DTOutput('tableB_13')
                                         )
                                     )
                            ),
                            tabPanel("Market", fluid = TRUE,
                                     fluidRow(
                                         h5("Market Environment", style = "margin-left: 15px; margin-top: 15px;")
                                     ),
                                     fluidRow(
                                         column(3,
                                                fileInput(inputId = "rfsDR", 
                                                          label = "Import your Risk Factors:"),
                                                actionButton("importYCDR", "Import")
                                         )
                                     ),
                                     fluidRow(
                                         column(6,
                                                h5("YieldCuve"),
                                                verbatimTextOutput("ycDefaultDetailsDR"),
                                                plotOutput("ycDefaultPlotDR")
                                         )
                                     ),
                                     fluidRow(
                                         column(3,
                                                fileInput(inputId = "dfs", 
                                                          label = "Import your Default Factors:"),
                                                actionButton("importDC", "Import")
                                         )
                                     ),
                                     fluidRow(
                                         h5("Default Curves"),
                                         column(4,
                                                h5("High Sensitive Economic Sector"),
                                                verbatimTextOutput("dcHigh"),
                                                plotOutput("dcHighPlot")
                                         ),
                                         column(4,
                                                h5("Mediocre Sensitive Economic Sector"),
                                                verbatimTextOutput("dcMed"),
                                                plotOutput("dcMedPlot")
                                         ),
                                         column(4,
                                                h5("Low Sensitive Economic Sector"),
                                                verbatimTextOutput("dcLow"),
                                                plotOutput("dcLowPlot")
                                         )
                                     ),
                                     fluidRow(
                                         h5("Default Simulation Input", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(3,
                                                textInput(inputId = "simFromDR", 
                                                          label = "From", 
                                                          placeholder = "2022-01-01"),
                                                actionButton("simDR", "Run")
                                         ),
                                         column(3,
                                                textInput(inputId = "simToDR", 
                                                          label = "To", 
                                                          placeholder = "2027-01-01")
                                         ),
                                         column(4,
                                                textInput(inputId = "defDate", 
                                                          label = "Default Date", 
                                                          placeholder = "2024-01-01")
                                         ),
                                     )
                            )
                        )
                        
               )
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
        
        val <- value(inst, tb, type = "market", method = DcEngine(rfDefault), scale=scale, digits=2)
        valShifted <- value(instShifted, tb, type = "market", method = DcEngine(rfShifted), scale=scale, digits=2)
        inc <- income(inst, tb, type="marginal", scale=scale, digits=2)
        incShifted <- income(instShifted, tb, type="marginal", scale=scale, digits=2)
        liq <- liquidity(inst, tb, scale=scale, digits=2)
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
        
        equityRatio <- valueEquityRatio(val)
        liquidityCoverageRatio <- valueLiquidityCoverageRatio(val)
        
        equityRatioShifted <- valueEquityRatio(valShifted)
        liquidityCoverageRatioShifted <- valueLiquidityCoverageRatio(valShifted)
        
        sensitivity(inst, ycDefault)
        sensitivity(instShifted, ycShifted)
        
        sen <- showSensitivity(inst)
        senShifted <- showSensitivity(instShifted)
        
        output$eqrDefault <- renderPrint({
            print(equityRatio)
        })
        
        output$lcrDefault <- renderPrint({
            print(liquidityCoverageRatio)
        })
        
        output$senDefault <- renderPrint({
            print(sen)
        })
        
        output$eqrShifted <- renderPrint({
            print(equityRatioShifted)
        })
        
        output$lcrShifted <- renderPrint({
            print(liquidityCoverageRatioShifted)
        })
        
        output$senShifted <- renderPrint({
            print(senShifted)
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
    
    
    
    # Default Risk Tab
    
    observeEvent(input$createInstA, {
        
        instA <<- createInstitution(input$instNameA)
        ann_ptf <- samplePortfolio(input$annsA$datapath, "contracts")
        pam_ptf <- samplePortfolio(input$pamsA$datapath, "contracts")
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        ops_ptf <- samplePortfolio(input$opsA$datapath, "operations")
        
        instA <<- assignContracts2Tree(instA, ptf)
        instA <<- assignContracts2Tree(instA, ops_ptf)
        
        output$instA <- renderPrint({
            print(instA)
        })
        
        leaf_dfs <- getLeafsAsDataFrames(instA)
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('tableTitleA_', i)]] <- renderText({leaf_dfs[[i]]$leaf})
        })
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('tableA_', i)]] <- DT::renderDataTable({leaf_dfs[[i]]$contracts})
        })

    })
    
    observeEvent(input$createInstB, {
        
        instB <<- createInstitution(input$instNameB)
        ann_ptf <- samplePortfolio(input$annsB$datapath, "contracts")
        pam_ptf <- samplePortfolio(input$pamsB$datapath, "contracts")
        ptf <- mergePortfolios(ann_ptf, pam_ptf)
        ops_ptf <- samplePortfolio(input$opsB$datapath, "operations")
        
        instB <<- assignContracts2Tree(instB, ptf)
        instB <<- assignContracts2Tree(instB, ops_ptf)
        
        output$instB <- renderPrint({
            print(instB)
        })
        
        leaf_dfs <- getLeafsAsDataFrames(instB)
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('tableTitleB_', i)]] <- renderText({leaf_dfs[[i]]$leaf})
        })
        
        lapply(1:length(leaf_dfs), function(i) {
            output[[paste0('tableB_', i)]] <- DT::renderDataTable({leaf_dfs[[i]]$contracts})
        })
        
    })
    
    
    observeEvent(input$importYCDR, {
        
        riskFactors <- input$rfsDR$datapath
        rfListDR <<- getRFList(riskFactors)
        
        ycDefaultDR <<- rfListDR[[1]]
        
        output$ycDefaultPlotDR <- renderPlot({
            plot(ycDefaultDR)
        })
        output$ycDefaultDetailsDR <- renderPrint({
            print(ycDefaultDR)
        })
        
        output$ycShiftedPlot <- renderPlot({
            plot(ycShifted)
        })
        output$ycShiftedDetails <- renderPrint({
            print(ycShifted)
        })
        
    })
    
    
    observeEvent(input$importDC, {
        
        defaultFactors <- input$dfs$datapath
        dfList <<- getRFList(defaultFactors)
        
        dcHigh <<- dfList[[1]]
        dcMed <<- dfList[[2]]
        dcLow <<- dfList[[3]]
        
        output$dcHighPlot <- renderPlot({
            plot(dcHigh)
        })
        output$dcHigh <- renderPrint({
            print(dcHigh)
        })
        
        output$dcMedPlot <- renderPlot({
            plot(dcMed)
        })
        output$dcMed <- renderPrint({
            print(dcMed)
        })
        
        output$dcLowPlot <- renderPlot({
            plot(dcLow)
        })
        output$dcLow <- renderPrint({
            print(dcLow)
        })

        
    })
    
    
    observeEvent(input$sim, {
        
        t0 <- input$simFromDR
        tn <- input$simToDR
        
        n <- yearFraction(t0, tn)
        t0Year <- as.numeric(substr(t0,1,4))
        tnYear <- as.numeric(substr(tn,1,4))
        by <- timeSequence(t0, by="1 years", length.out=n+2)
        tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                          breakLabs=substr(as.character(by),3,10))
        scale = 1000000

        rfDefault <- RFConn()
        add(rfDefault, list(ycDefaultDR))
        
        defDate <- input$defDate
        
        
        
        rawCtrs <- list(ann_df, pam_df)
        default(bankDefault, rfDCList, "2024-01-01", rawCtrs)
        
        instA <<- events(object = instA, riskFactors = rfDefault)
        instB <<- events(object = instB, riskFactors = rfDefault)
        
        val <- value(instA, tb, type = "market", method = DcEngine(rfDefault), scale=scale, digits=2)
        valB <- value(instB, tb, type = "market", method = DcEngine(rfShifted), scale=scale, digits=2)
        inc <- income(instA, tb, type="marginal", scale=scale, digits=2)
        incB <- income(instB, tb, type="marginal", scale=scale, digits=2)
        liq <- liquidity(instA, tb, scale=scale, digits=2)
        liqB <- liquidity(instB, tb, scale=scale, digits=2)
        
        
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
        
        equityRatio <- valueEquityRatio(val)
        liquidityCoverageRatio <- valueLiquidityCoverageRatio(val)
        
        equityRatioShifted <- valueEquityRatio(valShifted)
        liquidityCoverageRatioShifted <- valueLiquidityCoverageRatio(valShifted)
        
        output$eqrDefault <- renderPrint({
            print(equityRatio)
        })
        
        output$lcrDefault <- renderPrint({
            print(liquidityCoverageRatio)
        })
        
        output$eqrShifted <- renderPrint({
            print(equityRatioShifted)
        })
        
        output$lcrShifted <- renderPrint({
            print(liquidityCoverageRatioShifted)
        })

        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
