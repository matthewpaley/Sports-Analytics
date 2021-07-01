#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(formattable)

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(), br(), br(),
  # Popup box to show that it's working
  tags$head(tags$script(src = "message-handler.js")),
  actionButton("do", "Get Spreads"),
  fluidRow(
    column(width = 6, 
           DT::dataTableOutput(outputId = "table"),
           DT::dataTableOutput(outputId = "table2")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # observeEvent(input$do, {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'Calculating predictions...')
  #  })
  vals <- reactiveValues()
  observeEvent(input$do, {
     vals$out <- scrapeSpreads("2020-01-01")
     session$sendCustomMessage(type = 'testmessage',
                               message = 'Calculating predictions...')
  })
  output$table <- DT::renderDataTable(scrapeNBA("2020-01-01"))
  output$table2 <- DT::renderDataTable(vals$out)
}  

# Run the application 
shinyApp(ui, server)


#team summary table
# table <- bound%>%  select(posteam, rowname, epa, success, play) %>% group_by(posteam) %>% gt() %>%
#   cols_label(
#     epa = md("**EPA/<br>play**"), success = md("**Success<br>rate**"), play = md("**Plays**")) %>%
#   cols_align(align = "center") %>%
#   tab_source_note(
#     source_note = "Table: @benbbaldwin | Data: @nflscrapR") %>%
#   tab_header(title = paste("Game Summary,", away, "@", home)) %>%
#   tab_style(
#     style = list(
#       cell_text(weight = "bold")), locations = cells_group(groups=TRUE)) %>%
#   tab_style(
#     style = list(
#       cell_text(style = "italic", align="center")), 
#     locations = cells_stub(rows=c(2,3,9,10,5,6,12,13))) 
