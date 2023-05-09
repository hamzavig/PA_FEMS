library(shiny)
library(shinythemes)
library(devtools)
library(DT)
library(data.table)
library(rlist)
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
                            tabPanel("Introduction / Guide", fluid = TRUE,
                                     fluidRow(
                                         h4("Introduction", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(6, 
                                         p("The interest rate risk in this project thesis refers to a parallel shift in the yield curve of the market environment 
                                           with a given default yield curve. Given the modelled yield curve of the initial market environment, 
                                           a shift of ten base points or 0.1% is added or subtracted from the initial yield curve."
                                           )
                                         )
                                     ),
                                     fluidRow(
                                         h4("Guide", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(12,
                                                p("The following instructions provide a superficial guide through the individual steps for the execution of the interest rate risk scenario."),
                                                h5("Tab: Institution"),
                                                tags$ol(
                                                    tags$li("Download the required data provided in GitHub: ", tags$a(href="https://github.com/hamzavig/PA_FEMS/tree/main/src/data/bankA/csv", "Data")),
                                                    tags$li("Add a name for your institution in the field 'Name your institution'"),
                                                    tags$li("Upload the operations predefined in the downloaded dataset 'operations.csv' into the field 'Import your Operations'."),
                                                    tags$li("Upload the annuity contracts predefined in the downloaded dataset 'ann_ptf.csv' into the field 'Import your ANN Portfolio'."),
                                                    tags$li("Upload the principal at maturity contracts predefined in the downloaded dataset 'pam_ptf.csv' into the field 'Import your PAM Portfolio'."),
                                                    tags$li("Click on 'Create' to create and initialize your institution with the given financial contracts."),
                                                    tags$li("After the creation of the institution you can scroll down to see the institution's contract structure and the individual financial contracts added to the institution and their details.")
                                                ),
                                                h5("Tab: Market"),
                                                tags$ol(
                                                    tags$li("Upload the risk factors in this case the default yield curves predefined in the downloaded dataset 'rf_markets.csv' into the field 'Import your Risk Factors' to create the market environment."),
                                                    tags$li("There are two yield curves imported: the default yield curve and the shifted yield curve. Which are the same after the initial import."),
                                                    tags$li("Continuing in the 'Simulation input' part add a start date for the simulation of the yield curve shift like '2022-01-01' in the field 'From'."),
                                                    tags$li("Add a end date for the simulation of the yield curve shift like '2026-01-01' in the field 'To'."),
                                                    tags$li("Define the shift amount by adding the base points manually in the field 'Shift amount' or pressing the up/down arrow to adjust the amount."),
                                                    tags$li("Click on 'Run' to sttart the simulation."),
                                                    tags$li("After the simualtion is started the 'Shifted Yield Curve' turns grey which indicates that the simulation is still running."),
                                                    tags$li("When the 'Shifted Yield Curve' appears black again the simulation is through. The values as well as the curve of the 'Shifted Yield Curve' represent now the parallel shift according the defined shift amount.")
                                                ),
                                                h5("Tab: Financial Statements"),
                                                tags$ol(
                                                    tags$li("In this tab you can analyse the financial statements such as value and income of your institution."),
                                                    tags$li("On the left side you have the finacial statements based on the 'Default Yield Curve' imported in the previous tab while on the right side you'll see the ones calculated based on the 'Shifted Yield Curve'.")
                                                ),
                                                h5("Tab: Sensitivity"),
                                                tags$ol(
                                                    tags$li("In this tab you can analyse the financial ratios as well as the duration and fisher-weil duration of your institution's contracts on portfolio and contract level."),
                                                    tags$li("On the left side you have the sensitivity metrics based on the 'Default Yield Curve' while on the right side you'll see the ones calculated based on the 'Shifted Yield Curve'.")
                                                ),
                                                h5("Tab: Event Details"),
                                                tags$ol(
                                                    tags$li("In this tab you can analyse an individual financial contracts on cash flow event level."),
                                                    tags$li("Add a contract id like 'ASL0001' in the field 'Contract ID'."),
                                                    tags$li("Click on 'Show Events' to get the cash flow events of the respective financial contract."),
                                                    tags$li("Compare the cash flow events of the financial contracts between the 'Default Yield Curve' on the left and the 'Shifted Yield Curve' on the right.")
                                                )
                                         )
                                     )
                            ),
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
                                                             label = "Shift amount", 
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
                                                verbatimTextOutput("incDefault")
                                         ),
                                         column(6,
                                                h5("Value Shifted"),
                                                verbatimTextOutput("valShifted"),
                                                h5("Income Shifted"),
                                                verbatimTextOutput("incShifted")
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
                            tabPanel("Introduction / Guide", fluid = TRUE,
                                     fluidRow(
                                         h4("Introduction", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(6, 
                                                p("For determining the default risk of a bank, default curves are used to show the probability of default over time for a given economic sector and a given time rage. 
                                                  In this project thesis, three default curves are used to represent the risk of default for counterparties operating in three different economic sectors."
                                                )
                                         )
                                     ),
                                     fluidRow(
                                         h4("Guide", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(12,
                                                p("The following instructions provide a superficial guide through the individual steps for the execution of the interest rate risk scenario."),
                                                h5("Tab: Institution"),
                                                tags$ol(
                                                    tags$li("Download the required data provided in GitHub which contain datasets for two banks (bankA and bankB): ", tags$a(href="https://github.com/hamzavig/PA_FEMS/tree/main/src/data/", "Data")),
                                                    tags$li("Add a name for your first institution in the field 'Name your institution' in the left column 'Create Institution A'."),
                                                    tags$li("Upload the operations for the first institution predefined in the downloaded dataset 'bankA/csv/operations.csv' into the field 'Import your Operations' in the left colum 'Create Institution A'."),
                                                    tags$li("Upload the annuity contracts for the first institution predefined in the downloaded dataset 'bankA/csv/ann_ptf.csv' into the field 'Import your ANN Portfolio' in the left column 'Create Institution A'."),
                                                    tags$li("Upload the principal at maturity contracts for the first institution predefined in the downloaded dataset 'bankA/csv/pam_ptf.csv' into the field 'Import your PAM Portfolio' in the left column 'Create Institution A'."),
                                                    tags$li("Click on 'Create' in the left column to create and initialize the institution A with the given financial contracts."),
                                                    tags$li("After the creation of the institution you can scroll down to see the institution's contract structure and the individual financial contracts added to the institution and their details."),
                                                    tags$li("Repeat the same instruction for the second institution in the right column 'Create Institution B' while using the already downloaded datasets of the following path: bankB/csv/")
                                                ),
                                                h5("Tab: Market"),
                                                tags$ol(
                                                    tags$li("Upload the risk factor in this case the default yield curve predefined in the downloaded dataset 'bankA/csv/rf_markets.csv' into the field 'Import your Risk Factors' to create the market environment."),
                                                    tags$li("Click on 'Import' to upload and initialize the risk factors."),
                                                    tags$li("Upload the default factors in this case the default curve predefined in the downloaded dataset 'bankA/csv/rf_defaultCurves.csv' into the field 'Import your Default Factors'. The three curves represent the probability of default of three different economic sector to which the financial contracts are mapped."),
                                                    tags$li("Continuing in the 'Defauult Simulation input' part add a start date for the simulation of the yield curve shift like '2022-01-01' in the field 'From'."),
                                                    tags$li("Add a end date for the simulation of the yield curve shift like '2026-01-01' in the field 'To'."),
                                                    tags$li("Add a default date which applies to the probability date of the default curves such as '2024-01-01' in the field 'Default Date'."),
                                                    tags$li("Define the recovery rate by adding the manually a rate between 0 and 1 in the field 'Recovery rate'."),
                                                    tags$li("Click on 'Run' to sttart the simulation."),
                                                    tags$li("After the simualtion is started the output is visible in the following tabs. This simulation takes a couple minutes.")
                                                ),
                                                h5("Tab: Financial Statements"),
                                                tags$ol(
                                                    tags$li("In this tab you can analyse the financial statements such as financial ratios and value of both institution in comparison."),
                                                    tags$li("On the left side you have the finacial statements of the first institution while on the right side you'll find the financial statements of the second institution.")
                                                )
                                         )
                                     )
                            ),
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
                                         column(3,
                                                textInput(inputId = "defDate", 
                                                          label = "Default Date", 
                                                          placeholder = "2024-01-01")
                                         ),
                                         column(3,
                                                numericInput(inputId = "defRecovery", 
                                                             label = "Recovery rate", 
                                                             value = 0.01,
                                                             min = 0.01,
                                                             max = 1,
                                                             step = 0.01)
                                         )
                                     )
                            ),
                            tabPanel("Financial Statements", fluid = TRUE,
                                     fluidRow(
                                         h5("Financial Statements", style = "margin-left: 15px; margin-top: 15px;"),
                                         column(6,
                                                h5("Financial Ratios A"),
                                                verbatimTextOutput("eqrA"),
                                                verbatimTextOutput("lcrA"),
                                                h5("Value Bank A"),
                                                verbatimTextOutput("valA")
                                         ),
                                         column(6,
                                                h5("Financial Ratios B"),
                                                verbatimTextOutput("eqrB"),
                                                verbatimTextOutput("lcrB"),
                                                h5("Value Bank B"),
                                                verbatimTextOutput("valB")
                                         ),
                                     )
                            ),
                            
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
        
        #val <- value(inst, tb, type = "nominal", scale=scale, digits=2)
        #valShifted <- value(instShifted, tb, type = "nominal", scale=scale, digits=2)
        
        inc <- income(inst, tb, type="marginal", scale=scale, digits=2)
        incShifted <- income(instShifted, tb, type="marginal", scale=scale, digits=2)
        
        
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
    
    
    observeEvent(input$simDR, {
        
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
        
        ann_dfA <- contractFile2dataframe(input$annsA$datapath)
        pam_dfA <- contractFile2dataframe(input$pamsA$datapath)
        
        rawCtrsA <- list(ann_dfA, pam_dfA)
        default(instA, dfList, defDate, rawCtrsA)
        
        
        ann_dfB <- contractFile2dataframe(input$annsB$datapath)
        pam_dfB <- contractFile2dataframe(input$pamsB$datapath)
        
        rawCtrsB <- list(ann_dfB, pam_dfB)
        default(instB, dfList, defDate, rawCtrsB)
        
        instA <<- events(object = instA, riskFactors = rfDefault)
        instB <<- events(object = instB, riskFactors = rfDefault)
        
        val <- value(instA, tb, type = "market", method = DcEngine(rfDefault), scale=scale, digits=2)
        valB <- value(instB, tb, type = "market", method = DcEngine(rfDefault), scale=scale, digits=2)
        
        
        output$valA <- renderPrint({
            return(val)
        })
        
        output$valB <- renderPrint({
            return(valB)
        })
        
        equityRatio <- valueEquityRatio(val)
        liquidityCoverageRatio <- valueLiquidityCoverageRatio(val)
        
        equityRatioB <- valueEquityRatio(valB)
        liquidityCoverageRatioB <- valueLiquidityCoverageRatio(valB)
        
        output$eqrA <- renderPrint({
            print(equityRatio)
        })
        
        output$lcrA <- renderPrint({
            print(liquidityCoverageRatio)
        })
        
        output$eqrB <- renderPrint({
            print(equityRatioB)
        })
        
        output$lcrB <- renderPrint({
            print(liquidityCoverageRatioB)
        })

        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
