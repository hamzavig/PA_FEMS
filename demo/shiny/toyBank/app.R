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

    navbarPage("Risk Analysis and Risk Reporting",
               tabPanel("Institution", fluid = TRUE,
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
                                                 width = 9,
                                                 h3(textOutput("tableTitle_1")),
                                                 DT::dataTableOutput("table_1"),
                                                 h3(textOutput("tableTitle_2")),
                                                 DT::dataTableOutput("table_2"),
                                                 h3(textOutput("tableTitle_3")),
                                                 DT::dataTableOutput("table_3"),
                                                 h3(textOutput("tableTitle_4")),
                                                 DT::dataTableOutput("table_4"),
                                                 h3(textOutput("tableTitle_5")),
                                                 DT::dataTableOutput("table_5"),
                                                 h3(textOutput("tableTitle_6")),
                                                 DT::dataTableOutput("table_6"),
                                                 h3(textOutput("tableTitle_7")),
                                                 DT::dataTableOutput("table_7"),
                                                 h3(textOutput("tableTitle_8")),
                                                 DT::dataTableOutput("table_8"),
                                                 h3(textOutput("tableTitle_9")),
                                                 DT::dataTableOutput("table_9"),
                                                 h3(textOutput("tableTitle_10")),
                                                 DT::dataTableOutput("table_10"),
                                                 h3(textOutput("tableTitle_11")),
                                                 DT::dataTableOutput("table_11"),
                                                 h3(textOutput("tableTitle_12")),
                                                 DT::dataTableOutput("table_12"),
                                                 h3(textOutput("tableTitle_13")),
                                                 DT::dataTableOutput("table_13"),
                                                 h3(textOutput("tableTitle_14")),
                                                 DT::dataTableOutput("table_14"),
                                                 h3(textOutput("tableTitle_15")),
                                                 DT::dataTableOutput("table_15")
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
        
        output$tableTitle_1 <- renderText({leaf_dfs[[1]]$leaf})
        output$table_1 <- DT::renderDataTable({leaf_dfs[[1]]$contracts})
        output$tableTitle_2 <- renderText({leaf_dfs[[2]]$leaf})
        output$table_2 <- DT::renderDataTable({leaf_dfs[[2]]$contracts})
        output$tableTitle_3 <- renderText({leaf_dfs[[3]]$leaf})
        output$table_3 <- DT::renderDataTable({leaf_dfs[[3]]$contracts})
        output$tableTitle_4 <- renderText({leaf_dfs[[4]]$leaf})
        output$table_4 <- DT::renderDataTable({leaf_dfs[[4]]$contracts})
        output$tableTitle_5 <- renderText({leaf_dfs[[5]]$leaf})
        output$table_5 <- DT::renderDataTable({leaf_dfs[[5]]$contracts})
        output$tableTitle_6 <- renderText({leaf_dfs[[6]]$leaf})
        output$table_6 <- DT::renderDataTable({leaf_dfs[[6]]$contracts})
        output$tableTitle_7 <- renderText({leaf_dfs[[7]]$leaf})
        output$table_7 <- DT::renderDataTable({leaf_dfs[[7]]$contracts})
        output$tableTitle_8 <- renderText({leaf_dfs[[8]]$leaf})
        output$table_8 <- DT::renderDataTable({leaf_dfs[[8]]$contracts})
        output$tableTitle_9 <- renderText({leaf_dfs[[9]]$leaf})
        output$table_9 <- DT::renderDataTable({leaf_dfs[[9]]$contracts})
        output$tableTitle_10 <- renderText({leaf_dfs[[10]]$leaf})
        output$table_10 <- DT::renderDataTable({leaf_dfs[[10]]$contracts})
        output$tableTitle_11 <- renderText({leaf_dfs[[11]]$leaf})
        output$table_11 <- DT::renderDataTable({leaf_dfs[[11]]$contracts})
        output$tableTitle_12 <- renderText({leaf_dfs[[12]]$leaf})
        output$table_12 <- DT::renderDataTable({leaf_dfs[[12]]$contracts})
        output$tableTitle_13 <- renderText({leaf_dfs[[13]]$leaf})
        output$table_13 <- DT::renderDataTable({leaf_dfs[[13]]$contracts})
        output$tableTitle_14 <- renderText({leaf_dfs[[14]]$leaf})
        output$table_14 <- DT::renderDataTable({leaf_dfs[[14]]$contracts})
        output$tableTitle_15 <- renderText({leaf_dfs[[15]]$leaf})
        output$table_15 <- DT::renderDataTable({leaf_dfs[[15]]$contracts})
    })
    
    # for(i in 1:length(leaf_dfs)){
    #     
    #     assign(paste0("output$tableTitle_", i), renderText({
    #         leaf_dfs[[i]]$leaf
    #     })
    #     )
    #     assign(paste0("output$table_", i), DT::renderDataTable({
    #         leaf_dfs[[i]]$contracts
    #     })
    #     )
    # }
}

# Run the application 
shinyApp(ui = ui, server = server)
