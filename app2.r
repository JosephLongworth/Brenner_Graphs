library(shiny)
library(DT)

ui <- fluidPage(
  DTOutput("editable_table"),
  actionButton("save", "Save Changes")
)

server <- function(input, output, session) {
  values <- reactiveVal(data.frame(Name = c("A", "B", "C"), Value = c(10, 20, 30)))
  
  output$editable_table <- renderDT({
    datatable(values(), editable = "cell")
  }, server = FALSE)
  
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    df <- values()
    df[info$row, info$col] <- as.numeric(info$value)
    values(df)
  })
}

shinyApp(ui, server)


