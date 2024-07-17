UI_barplot_annotated <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
              box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
                  numericInput(ns("ylab_split"), "Paper ylab split", 100),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("width"), "Plot width mm", 40),
                    numericInput(ns("height"), "Plot height mm", 30)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("font"), "Plot font size", 7),
                    numericInput(ns("dotsize"), "Plot dotsize", 1)),
                  numericInput(ns("space_top"), "Plot space top", 1.1),
                  checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
                  checkboxInput(ns("Show_ns"), "Show NS", value = F),
                  
              ),
              box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE,
                  downloadButton(ns("downloadPaper"), "Paper SVG"),
                  downloadButton(ns("downloadPaperpng"), "Paper PNG"),
                  plotOutput(ns("plot"))
              )
            ),
    fluidRow(
      box(title = "Data", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6, collapsed = FALSE,
          rHandsontableOutput(ns("hot"))
      ),
      box(title = "Colour Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6, collapsed = FALSE,
          rHandsontableOutput(ns("colour_key_hot"))
      )
    )
    )

}

Server_barplot_annotated <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      df <- read_csv("Data/example_barplot_annotation.csv")
      
      if("Unit_barplot" %in% colnames(df)){
          df <- df %>%
            mutate(Unit = Unit_barplot,.keep = c("unused"))}
      colour_key <- read_csv("Data/example_colour_key.csv")
      
      output$hot = renderRHandsontable({
        rhandsontable(df)
      })
      output$colour_key_hot = renderRHandsontable({
        rhandsontable(colour_key)
      })
      outfile <- tempfile(fileext='.svg')
      outfile_png <- tempfile(fileext='.png')
      output$plot <- renderImage({
        req(input$hot)
        empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
          theme_void()
        plot <- JPL_barplot_annotation(hot_to_df(input$hot),
                                       hot_to_df(input$colour_key_hot),
                                       ylab_split=input$ylab_split,
                                       font = input$font,
                                       dotsize = input$dotsize,
                                       space_top = input$space_top,
                                       var_equal = input$var_equal,
                                       Show_ns = input$Show_ns,
                                       legend_loc = "none")+
        theme(rect = element_rect(fill = "transparent"))
        set_panel_size(plot, file = outfile ,
                       width = unit(input$width, "mm"),
                       height = unit(input$height,"mm"))
        set_panel_size(plot, file = outfile_png,
                       width = unit(input$width, "mm"),
                       height = unit(input$height,"mm"))
        list(src = outfile,
             alt = "This is alternate text")
      }, deleteFile = F)
      output$downloadPaper <- downloadHandler(
        filename = function() {
          paste("PaperSize-", Sys.Date(), ".svg", sep="")
        },
        content = function(file) {
          file.copy(
            from = outfile,
            to = file
          )
        }
      )
      output$downloadPaperpng <- downloadHandler(
        filename = function() {
          paste("PaperSize-", Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          file.copy(
            from = outfile_png,
            to = file
          )
        }
      )
      
      
      }
  )
}

