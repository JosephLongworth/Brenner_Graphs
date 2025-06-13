UI_barplot_annotated2 <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3,
          radioButtons(inputId = ns("defaults"),label = NULL, choices = c("Paper", "Presentation"),
                       selected = "Presentation",inline = T),
          splitLayout(
            numericInput(ns("width"), "Plot width mm (per bar)", 7.5),
            numericInput(ns("height"), "Plot height mm", 25)),
          splitLayout(
            numericInput(ns("font"), "Plot font size", 7),
            numericInput(ns("dotsize"), "Plot dotsize", 1)),
          splitLayout(
            checkboxInput(ns("Group_Stats"), "Group Stats", value = T),
            checkboxInput(ns("Sample_Stats"), "Sample Stats", value = F)),
          numericInput(ns("top"), "Plot space top mm", 5, step = 1),
          checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
          checkboxInput(ns("Show_ns"), "Show NS", value = F),
          selectizeInput(ns("legend_loc"), "Legend location", choices = c("none","top","bottom","left","right"), selected = "none"),
          selectizeInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}","p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}")
      ),
      box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9,
          downloadButton(ns("downloadPaper"), "SVG"),
          downloadButton(ns("downloadPaperpng"), "PNG"),
          plotOutput(ns("plot")) %>% shinycustomloader::withLoader()
      )
    ),
    fluidRow(
      box(title = "Data", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6,
          excelOutput(ns("hot"),height = "100%")
      ),
      box(title = "Colour Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6,
          excelOutput(ns("colour_key_hot"))
      )
    )
  )
}

Server_barplot_annotated2 <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      pastel_palette <- RColorBrewer::brewer.pal(8, "Pastel1")
      
      observeEvent(input$defaults, {
        if(input$defaults == "Paper"){
          updateNumericInput(session, "width", value = 7.5)
          updateNumericInput(session, "height", value = 25)
          updateNumericInput(session, "font", value = 7)
          updateNumericInput(session, "dotsize", value = 1)
        } else {
          updateNumericInput(session, "width", value = 15)
          updateNumericInput(session, "height", value = 60)
          updateNumericInput(session, "font", value = 12)
          updateNumericInput(session, "dotsize", value = 2)
        }
      })
      
      # Load and store data
      df_data <- reactiveVal(readr::read_csv("Data/example_barplot_annotation2.csv", show_col_types = FALSE))
      colour_key_data <- reactiveVal(NULL)
      
      output$hot <- renderExcel({
        df <- df_data()
        if ("Unit_barplot" %in% colnames(df)) {
          df$Unit <- df$Unit_barplot
        }
        excelTable(data = df,
                   style = list(
                     "A1" = 'background-color: orange; fontWeight: bold; color: white;',
                     "B1" = 'background-color: orange;',
                     "C1" = 'background-color: #808080; fontWeight: bold; color: white;',
                     "C2" = 'background-color: black;',
                     "D2" = 'background-color: black;',
                     "F2" = 'background-color: black;',
                     "C3" = 'background-color: black;',
                     "D3" = 'background-color: black;',
                     "F3" = 'background-color: black;',
                     "D1" = 'background-color: orange;'
                   ),
                   loadingSpin=T)
      })
      
      output$colour_key_hot <- renderExcel({
        # req(input$hot)
        
        colour_key <- excel_to_R(input$hot) |> 
          as_tibble() |> 
          select(Sample) |> 
          unique()
          
        colour_key <- dplyr::left_join(colour_key,read_csv("Data/example_colour_key.csv", show_col_types = FALSE), by = "Sample")
        
        # Replace NAs in colour_hex with values from pastel_palette
        colour_key$color_hex[is.na(colour_key$color_hex)] <- pastel_palette[1:sum(is.na(colour_key$color_hex))]
        excelTable(data = colour_key)
      }) |> 
        bindEvent(input$hot)
      
      # Plot generation
      outfile <- tempfile(fileext = '.svg')
      outfile_png <- tempfile(fileext = '.png')
      
      output$plot <- renderImage({
        req(input$hot)
        browser()
        
        data_df <- as.data.frame(do.call(rbind, input$hot))
        colour_df <- colour_key_data()
        if (is.null(colour_df)) return(NULL)
        
        nbars <- data_df |>
          dplyr::select(Sample, Annotation_1_Symbol, Annotation_2_Symbol) |>
          dplyr::distinct() |>
          dplyr::summarise(nbars = dplyr::n()) |>
          as.double()
        
        plot <- JPL_barplot_annotation(
          data_df,
          colour_df,
          font = input$font,
          dotsize = input$dotsize,
          top = input$top,
          var_equal = input$var_equal,
          Show_ns = input$Show_ns,
          legend_loc = input$legend_loc,
          label = input$Stat_type,
          Group_Stats = input$Group_Stats,
          Sample_Stats = input$Sample_Stats
        ) + ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent"))
        
        ggsave(outfile, plot = plot, width = nbars * input$width / 25.4, height = input$height / 25.4, units = "in")
        ggsave(outfile_png, plot = plot, width = nbars * input$width / 25.4, height = input$height / 25.4, units = "in")
        
        list(src = outfile, contentType = "image/svg+xml", alt = "SVG Plot")
      }, deleteFile = FALSE)
      
      output$downloadPaper <- downloadHandler(
        filename = function() paste0("PaperSize-", Sys.Date(), ".svg"),
        content = function(file) file.copy(outfile, file)
      )
      
      output$downloadPaperpng <- downloadHandler(
        filename = function() paste0("PaperSize-", Sys.Date(), ".png"),
        content = function(file) file.copy(outfile_png, file)
      )
    }
  )
}