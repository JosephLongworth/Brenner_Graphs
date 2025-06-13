library(shiny)
library(excelR)

ui <- fluidPage(
  h4("Editable Table with Copy-Paste (excelR)"),
  excelOutput("my_table"),
  verbatimTextOutput("table_data")
)

server <- function(input, output, session) {
  data <- reactiveVal(head(mtcars))
  
  output$my_table <- renderExcel({
    excelTable(data = data())
  })
  
  # To get table contents after editing:
  output$table_data <- renderPrint({
    req(input$my_table)
    input$my_table
  })
}

shinyApp(ui, server)

