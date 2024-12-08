UI_barplot_annotated <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
              box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
                  radioButtons(inputId = ns("defaults"),label = NULL, choices = c("Paper", "Presentation"),
                               selected = "Presentation",inline = T),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("width"), "Plot width mm (per bar)", 7.5),
                    numericInput(ns("height"), "Plot height mm", 25)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("font"), "Plot font size", 7),
                    numericInput(ns("dotsize"), "Plot dotsize", 1)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    checkboxInput(ns("Group_Stats"), "Group Stats", value = T),
                    checkboxInput(ns("Sample_Stats"), "Sample Stats", value = F)),
                  numericInput(ns("top"), "Plot space top mm", 5, step = 1),
                  checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
                  checkboxInput(ns("Show_ns"), "Show NS", value = F),
                  selectizeInput(ns("legend_loc"), "Legend location", choices = c("none","top","bottom","left","right"), selected = "none"),
                  selectizeInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}","p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}")
                  
              ),
              box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE,
                  downloadButton(ns("downloadPaper"), "SVG"),
                  downloadButton(ns("downloadPaperpng"), "PNG"),
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

Server_barplot_annotated <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      observeEvent(input$defaults,{
        if(input$defaults == "Paper"){
          updateNumericInput(session, "width", "Plot width mm (per bar)", value = 7.5)
          updateNumericInput(session, "height", "Plot height mm", value = 25)
          updateNumericInput(session, "font", "Plot font size", value = 7)
          updateNumericInput(session, "dotsize", "Plot dotsize", value = 1)
        } else {
          updateNumericInput(session, "width", "Plot width mm (per bar)", value = 15)
          updateNumericInput(session, "height", "Plot height mm", value = 60)
          updateNumericInput(session, "font", "Plot font size", value = 12)
          updateNumericInput(session, "dotsize", "Plot dotsize", value = 2)
        }
      })
      
      df <- read_csv("Data/example_barplot_annotation2.csv",show_col_types = FALSE)
      
      if("Unit_barplot" %in% colnames(df)){
          df <- df %>%
            mutate(Unit = Unit_barplot,.keep = c("unused"))}
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
        # browser()
        plot <- JPL_barplot_annotation(hot_to_df(input$hot),
                                       hot_to_df(input$colour_key_hot),
                                       font = input$font,
                                       dotsize = input$dotsize,
                                       top = input$top,
                                       var_equal = input$var_equal,
                                       Show_ns = input$Show_ns,
                                       legend_loc = input$legend_loc,
                                       label = input$Stat_type,
                                       Group_Stats = input$Group_Stats,
                                       Sample_Stats = input$Sample_Stats
                                       ) +
        theme(rect = element_rect(fill = "transparent"))
        
        nbars <- hot_to_df(input$hot) |>
          select(Sample,Annotation_1_Symbol,Annotation_2_Symbol) |>
          distinct() |>
          summarise(nbars=n()) |>
          as.double()
        
        set_panel_size(plot, file = outfile ,
                       width = unit(nbars * input$width, "mm"),
                       height = unit(input$height,"mm"))
        set_panel_size(plot, file = outfile_png,
                       width = unit(nbars * input$width, "mm"),
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

