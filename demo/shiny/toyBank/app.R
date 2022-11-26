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
                        sidebarLayout(
                            sidebarPanel(
                                width = 3,
                                textInput(inputId = "instName", 
                                          label = "Name your Institution:", 
                                          placeholder = "Name"),
                                fileInput(inputId = "anns", 
                                          label = "Import your ANN Portfolio:"),
                                fileInput(inputId = "pams", 
                                          label = "Import your PAM Portfolio:"),
                                fileInput(inputId = "ops", 
                                          label = "Import your Operations:"),
                                fileInput(inputId = "rfs", 
                                          label = "Import your Risk Factors:"),
                                actionButton("send", "Send")
                                
                            ),
                            mainPanel(
                                width = 9,
                                tabsetPanel(
                                    tabPanel("Overview",
                                             mainPanel(
                                                 h4(textOutput('tableTitle_1')),
                                                 div(DTOutput('table_1'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_2')),
                                                 div(DTOutput('table_2'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_3')),
                                                 div(DTOutput('table_3'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_4')),
                                                 div(DTOutput('table_4'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_5')),
                                                 div(DTOutput('table_5'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_6')),
                                                 div(DTOutput('table_6'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_7')),
                                                 div(DTOutput('table_7'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_8')),
                                                 div(DTOutput('table_8'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_9')),
                                                 div(DTOutput('table_9'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_10')),
                                                 div(DTOutput('table_10'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_11')),
                                                 div(DTOutput('table_11'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_12')),
                                                 div(DTOutput('table_12'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_13')),
                                                 div(DTOutput('table_13'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_14')),
                                                 div(DTOutput('table_14'), style = "font-size: 75%;margin:10 0 10 0;"),
                                                 h4(textOutput('tableTitle_15')),
                                                 div(DTOutput('table_15'), style = "font-size: 75%;margin:10 0 10 0;")
                                             )),
                                    tabPanel("Financial Statements"),
                                    tabPanel("Risk Reporting")
                                )
                            )
                        ),
                        actionButton("add", "Add new Institution", icon = icon("plus-circle"))
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

}

# Run the application 
shinyApp(ui = ui, server = server)
