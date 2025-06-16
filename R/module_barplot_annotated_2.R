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
            checkboxInput(ns("Sample_Stats"), "Sample Stats", value = F),
            checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
            checkboxInput(ns("Show_ns"), "Show NS", value = F)
            ),
          fluidRow(
            column(6,selectInput(ns("legend_loc"), "Legend location", choices = c("none","top","bottom","left","right"), selected = "right")),
            column(6,selectInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}","p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}"))
          ),
          numericInput(ns("top"), "Plot space top mm", 5, step = 1)
          ),
      box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9,
          downloadButton(ns("downloadPaper"), "SVG"),
          downloadButton(ns("downloadPaperpng"), "PNG"),
          plotOutput(ns("plot")) %>% shinycustomloader::withLoader()
      )
    ),
    fluidRow(
      box(title = "Data", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 9,
          excelOutput(ns("hot"),height = "100%")
      ),
      box(title = "Colour Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 3,
          
          HTML("
    <p><strong>How the Colour Key Works:</strong></p>
    <ul>
      <li>All <code>Sample</code> names found in the main data table need to be in the colour key.</li>
      <li>New <code>Sample</code> ccan be added or populated by reseting the colour key.</li>
      <li>New sample names will receive default colours from a pastel palette.</li>
      <li>Hex codes will be retrieved based on database unless altered or not present. The database can be updated by committing.</li>
      <li>The <strong>order</strong> of samples in the Colour Key determines how they appear in the plot (e.g., left-to-right).</li>
    </ul>
  "),
          excelOutput(ns("colour_key_hot")),
          splitLayout(
          actionButton(ns("colour_key_update"), "Reset Colour Key"),
          actionButton(ns("colour_key_commit"), "Commit Colour Key")
          )
      )
    )
  )
}

Server_barplot_annotated2 <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      # Prepare some server wide values
      df_data <- reactiveVal(readr::read_csv("Data/example_barplot_annotation2.csv", show_col_types = FALSE))
      colour_key_data <- reactiveVal(NULL)
      pastel_palette <- RColorBrewer::brewer.pal(8, "Pastel1")
      outfile <- tempfile(fileext = '.svg')
      outfile_png <- tempfile(fileext = '.png')

      
            
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
      
      output$hot <- renderExcel({
        df <- df_data()
        if ("Unit_barplot" %in% colnames(df)) {
          df$Unit <- df$Unit_barplot
        }
        
        # Start with static header styles
        style <- list(
        )
        
        # Dynamically add styles for rows 2 to n in columns C, D, F
        cols <- c("C", "D", "F")
        for (col in cols) {
          for (row in 2:nrow(df)) {
            cell <- paste0(col, row)
            style[[cell]] <- 'background-color: black;'
          }
        }
        
        excelTable(data = df,
                   style = style,
                   loadingSpin=T)
      })
      

      output$colour_key_hot <- renderExcel({
        
        df <- if (!is.null(input$hot)) excel_to_R(input$hot) else df_data()
        
        colour_key <- df |> 
          as_tibble() |> 
          select(Sample) |> 
          unique()
          
        colour_key <- dplyr::left_join(colour_key,read_csv("Data/example_colour_key.csv", show_col_types = FALSE), by = "Sample")
        

        colour_key$color_hex[is.na(colour_key$color_hex)] <- pastel_palette[1:sum(is.na(colour_key$color_hex))]
        colour_key_data(colour_key)
        excelTable(data = colour_key,
                   # id = "colour_key_hot",
                   # style = style
                   )
        }) |> 
        bindEvent(input$colour_key_update,
                  ignoreNULL = F,
                  ignoreInit = F)
      
      observeEvent(input$colour_key_commit, {
        req(input$colour_key_hot)
          
          # Convert Excel input to tibble
          new_colour_key <- excel_to_R(input$colour_key_hot) |> 
            dplyr::as_tibble()
          
          # Path to the colour key file
          path <- "Data/example_colour_key.csv"
          
          # Load existing file or start empty
          if (file.exists(path)) {
            existing_key <- readr::read_csv(path, show_col_types = FALSE)
          } else {
            existing_key <- tibble::tibble(Sample = character(), color_hex = character())
          }
          
          # Merge: keep new values where present, fall back to old
          updated_key <- dplyr::full_join(existing_key, new_colour_key, by = "Sample", suffix = c(".old", ".new")) |>
            dplyr::mutate(color_hex = dplyr::coalesce(color_hex.new, color_hex.old)) |>
            dplyr::select(Sample, color_hex) |>
            dplyr::arrange(Sample)
          
          # Save back to CSV
          readr::write_csv(updated_key, path)
          
          # Optional: notify user
          showNotification("Colour key saved to CSV.", type = "message")
        })
        
        
      output$plot <- renderImage({

        
        data_df <- tryCatch({
          if (!is.null(input$hot)) excel_to_R(input$hot) else df_data()
        }, error = function(e) df_data())
        
        colour_df <- tryCatch({
          if (!is.null(input$colour_key_hot)) excel_to_R(input$colour_key_hot) else colour_key_data()
        }, error = function(e) colour_key_data())
        
        
        data_df <- data_df |> 
          as_tibble() |> 
          mutate(Value=as.double(Value)) |> 
          glimpse()
        # 
        # colour_df <- excel_to_R(input$colour_key_hot)
        #   
          
        # colour_df <- colour_key_data()
        # if (is.null(colour_df)) return(NULL)
        
        nbars <- data_df |>
          dplyr::select(Sample, Annotation_1_Symbol, Annotation_2_Symbol) |>
          dplyr::distinct() |>
          dplyr::summarise(nbars = dplyr::n()) |>
          as.double()

        
        data_df <- data_df |>
          mutate(Sample = factor(Sample, levels = colour_df$Sample,ordered = T)) |>
          glimpse()
        
        
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
        
        # ggsave(outfile, plot = plot, width = nbars * input$width / 25.4, height = input$height / 25.4, units = "in")
        # ggsave(outfile_png, plot = plot, width = nbars * input$width / 25.4, height = input$height / 25.4, units = "in")
        
        set_panel_size(plot, file = outfile ,
                       width = unit(nbars * input$width, "mm"),
                       height = unit(input$height,"mm"))
        set_panel_size(plot, file = outfile_png,
                       width = unit(nbars * input$width, "mm"),
                       height = unit(input$height,"mm"))
        
        
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