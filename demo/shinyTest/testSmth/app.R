#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages("fontawesome")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(id = "tabs",
                tabPanel(title = "Main",
                         value = "main",
                         
                         ## CONTENT PANEL ----- :
                         p("Add a new tab"),
                         actionButton("add", "Add", icon = icon("plus-circle"))
                ))
)

server <- function(input, output, session) {
    
    shinyInput <- function(name, id) paste(name, id, sep = "_")
    rv <- reactiveValues(counter = 0L)
    
    observeEvent(input$add, {
        rv$counter <- rv$counter + 1L
        ## GO TO THE NEWLY CREATED TAB:
        updateTabsetPanel(session, "tabs", shinyInput("new_tab", rv$counter))
    }, ignoreInit = TRUE)
    
    observeEvent(input$add, {
        appendTab(inputId = "tabs",
                  tabPanel(title = paste("Tab", rv$counter),
                           value = shinyInput("new_tab", rv$counter),
                           
                           ## CONTENT PANEL ----- :
                           tags$h4(paste("This is tab:", rv$counter)),
                           p("Remove this tab"),
                           actionButton(shinyInput("remove_btn", rv$counter), "Remove", icon = icon("minus-circle"))
                  )
        )
    })
    
    ## REACTIVITY TO ARRANGE TAB NAMES:
    current.tab <- eventReactive(input$tabs, {
        # don't accidentally remove main tab:
        if (!identical(input$tabs, "main")) {
            input$tabs
        } else {
            NULL
        }
    })
    
    ## OBSERVERS FOR THE REMOVE BTNS:
    observe({
        if (rv$counter > 0L) {
            lapply(seq(rv$counter), function(x) {
                observeEvent(input[[paste("remove_btn", x, sep = "_")]], {
                    removeTab(inputId = "tabs", target = current.tab())
                })
            })
        }
    })
    
}

shinyApp(ui, server)
