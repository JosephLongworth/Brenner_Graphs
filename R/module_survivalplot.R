UI_survivalplot <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
          numericInput(ns("ylab_split"), "Paper ylab split", 20),
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput(ns("width"), "Plot width mm", 40),
            numericInput(ns("height"), "Plot height mm", 30)),
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput(ns("font"), "Plot font size", 7),
            numericInput(ns("dotsize"), "Plot dotsize", 1)),
          numericInput(ns("space_top"), "Plot space top", 1.1)
      ),
      box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE,
          downloadButton(ns("downloadPaper"), "Paper SVG"),
          downloadButton(ns("downloadPaperpng"), "Paper PNG"),
          plotOutput(ns("plot")) %>% 
            shinycustomloader::withLoader()
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

Server_survivalplot <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      df <- read_csv("Data/example_survivalplot.csv",show_col_types = T)
      # browser()
      if("Unit_survivalplot" %in% colnames(df)){
        df <- df %>%
          mutate(Unit = Unit_survivalplot,.keep = c("unused")) }
      
      colour_key <- read_csv("Data/example_colour_key.csv",show_col_types = FALSE)
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

        # plot <- JPL_survivalplot(df)
        # 
        # browser()
        plot <- JPL_survivalplot(hot_to_df(input$hot),
                                 hot_to_df(input$colour_key_hot),
                                 font = input$font,
                                 legend_loc = "none",
                                 ylab_split=input$ylab_split)
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