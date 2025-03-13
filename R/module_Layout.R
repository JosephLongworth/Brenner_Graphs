UI_Layout <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    tabBox(
      width = 12,
      tabPanel("Layout_Table", fluidRow(
        box(
          width = 4,
          solidHeader = TRUE,
          radioGroupButtons(ns("plate_size"),individual = T,choices = c("384", "96")),
          DTOutput(ns("layout_table")),
          splitLayout(
            cellWidths = c("50%", "50%"),
            fileInput(
              ns("upload_layout_table"),
              label = NULL,
              buttonLabel = "Upload Excel",
              accept = c(".xlsx")
            ),
            downloadButton(ns("download_layout_table"), "Download Excel")
          ),
          splitLayout(
            cellWidths = c("33%", "33%", "33%"),
            downloadButton(ns("downloadlayout_pdf"), "Layout PDF"),
            downloadButton(ns("downloadlayout_png"), "Layout PNG"),
            downloadButton(ns("downloadlayout_svg"), "Layout SVG")
          )
          
        ),
        box(
          width = 8,
          solidHeader = TRUE,
          imageOutput(ns("plot")) |>
            shinycustomloader::withLoader()
        )
      )),
      tabPanel("Gene", rHandsontableOutput(ns("hot_Gene"))),
      tabPanel("Sample", rHandsontableOutput(ns("hot_Sample"))),
      tabPanel("Sample Key", rHandsontableOutput(ns("hot_sample_key"))),
    )
  ))
  
}

Server_Layout <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    observe({
      # browser()
    session$userData$vars$layout <- tibble(
      Well = wellr::well_from_index(1:as.numeric(input$plate_size), plate = as.numeric(input$plate_size), num_width = 0),
      Gene = NA_character_,
      Sample = NA_character_,
      Genotype = NA_character_,
      Treatment = NA_character_,
      Replicate = NA_character_
    )
    }) |> 
      bindEvent(input$plate_size)
    
    
    outfile_svg <- tempfile(fileext = '.svg')
    outfile_pdf <- tempfile(fileext = '.pdf')
    outfile_png <- tempfile(fileext = '.png')
    
    

    
    observeEvent(input$upload_layout_table, {
      req(input$upload_layout_table)
      
      file_path <- input$upload_layout_table$datapath
      df <- read_xlsx(file_path)  |> 
        full_join(tibble(Well = wellr::well_from_index(1:as.numeric(input$plate_size), plate = as.numeric(input$plate_size), num_width = 0)))
      
      # Ensure required columns exist
      required_cols <- c("Well", "Gene", "Sample", "Genotype", "Treatment", "Replicate")
      
      if (!all(required_cols %in% colnames(df))) {
        showNotification("Error: Missing required columns!", type = "error")
        return()
      }
      session$userData$vars$layout <- df  # Update the dataset only if valid
    })
    

    output$plot <- renderImage({
      plate_plot <- session$userData$vars$layout |>
        mutate(Well = gsub("([A-P])0([1-9])", "\\1\\2", Well)) |>
        ggplate::plate_plot(
          position = Well,
          value = Gene,
          # label = paste(Sample, Replicate, "\n", Treatment),
          label = Sample,
          plate_size = as.numeric(input$plate_size),
          legend_n_row = 8,
          plate_type = "square",
          scale = 2
          # ,
          # label_size = .6
        )
      
      set_panel_size(
        plate_plot,
        file = outfile_png,
        width = unit(210, "mm"),
        height = unit(115, "mm")
      )
      set_panel_size(
        plate_plot,
        file = outfile_pdf,
        width = unit(297, "mm"),
        height = unit(210, "mm")
      )
      set_panel_size(
        plate_plot,
        file = outfile_svg,
        width = unit(200, "mm"),
        height = unit(100, "mm")
      )
      list(src = outfile_svg, alt = "This is alternate text")
    }, deleteFile = F) |>
      bindEvent(
        c(input$plate_size,
          input$upload_layout_table,
          input$hot_Gene,
          input$hot_Sample,
          input$hot_sample_key
        ),
        ignoreNULL = F
      )
    
    
    output$layout_table <- renderDT({
      # browser()
      # df_reactive <- reactive({ session$userData$vars$layout})
      datatable(
        session$userData$vars$layout,
        options = list(
          pageLength = 10,
          # Show 10 rows per page
          autoWidth = TRUE,
          searching = TRUE,
          # Enable search
          dom = 'tip'  # Remove unnecessary elements, keep table, search, pagination
        ),
        class = "display nowrap compact"
      )
    }) |>
      bindEvent(
        c(input$plate_size,
          input$upload_layout_table,
          input$hot_Gene,
          input$hot_Sample,
          input$hot_sample_key
        ),
        ignoreNULL = F
      )
    
    output$download_layout_table <- downloadHandler(
      filename = function() {
        paste("layout_Table", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(session$userData$vars$layout, file)
      }
    )
    
    # Update main layout df based on inputs from hot tables---------------------
    
    observeEvent(input$hot_Gene, {
      session$userData$vars$layout <- session$userData$vars$layout |> 
        select(-Gene) |>
        left_join({hot_to_df(input$hot_Gene) |>
            pivot_longer(everything(),
                         names_to = "Col",
                         values_to = "Gene") |>
            mutate(Well = wellr::well_from_index(1:as.numeric(input$plate_size), plate = as.numeric(input$plate_size), num_width = 0)) |>
            select(c(Well,Gene))}) 
    })
    
    observeEvent(input$hot_Sample, {
      session$userData$vars$layout <- session$userData$vars$layout |> 
        select(-Sample) |>
        left_join({hot_to_df(input$hot_Sample) |>
            pivot_longer(everything(),
                         names_to = "Col",
                         values_to = "Sample") |>
            mutate(Well = wellr::well_from_index(1:as.numeric(input$plate_size), plate = as.numeric(input$plate_size), num_width = 0)) |>
            select(c(Well,Sample))}) 
    })
    
    observeEvent(input$hot_sample_key, {
      session$userData$vars$layout <- session$userData$vars$layout |> 
        select(-c(Genotype,Treatment,Replicate)) |> 
        left_join(hot_to_df(input$hot_sample_key))
    })
    
    # Prepare Hot tables for Gene Sample and Treatment--------------------------
    
    output$hot_Gene = renderRHandsontable({
      rhandsontable({
        session$userData$vars$layout |>
          select(Well, Gene) |>
          mutate(
            Col = wellr::well_to_col_num(Well),
            Row = wellr::well_to_row_let(Well),
            .keep = "unused"
          ) |>
          pivot_wider(names_from = Col, values_from = Gene) |>
          column_to_rownames(var = "Row")
      })
    }) |>
      bindEvent(c(input$upload_layout_table), ignoreNULL = F)
    
    
    output$hot_Sample = renderRHandsontable({
      rhandsontable({
        session$userData$vars$layout |>
          select(Well, Sample) |>
          mutate(
            Col = wellr::well_to_col_num(Well),
            Row = wellr::well_to_row_let(Well),
            .keep = "unused"
          ) |>
          pivot_wider(names_from = Col, values_from = Sample) |>
          column_to_rownames(var = "Row")
      })
    }) |>
      bindEvent(c(input$upload_layout_table), ignoreNULL = F)
    
    output$hot_sample_key = renderRHandsontable({
      rhandsontable({
        session$userData$vars$layout |>
          select(Sample, Genotype, Treatment, Replicate) |>
          distinct()
      },
      width = 550, height = 600) %>%
        hot_cols(columnSorting = TRUE)
    }) |>
      bindEvent(c(input$upload_layout_table, input$hot_Sample), ignoreNULL = F)

    # Prepare Downloads--------------------------
    
    output$downloadlayout_png <- downloadHandler(
      filename = function() {
        paste("layout-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        file.copy(from =  outfile_png, to = file)
      }
    )
    output$downloadlayout_svg <- downloadHandler(
      filename = function() {
        paste("layout-", Sys.Date(), ".svg", sep = "")
      },
      content = function(file) {
        file.copy(from =  outfile_svg, to = file)
      }
    )
    output$downloadlayout_pdf <- downloadHandler(
      filename = function() {
        paste("layout-", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        file.copy(from =  outfile_pdf, to = file)
      }
    )
  })
}