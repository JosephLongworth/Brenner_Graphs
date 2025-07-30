UI_qPCR_plot <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 3,
      box(
        title = "upload", collapsible = TRUE, solidHeader = TRUE, status = "info",width = 12,
        splitLayout(
          cellWidths = c("50%", "50%"),
          fileInput(
            ns("upload_layout_table"),
            label = NULL,
            buttonLabel = "Upload Layout",
            accept = c(".xlsx")
          ),
          fileInput(
            ns("upload_Cq_table"),
            label = NULL,
            buttonLabel = "Upload Cqs",
            accept = c(".csv")
          )
        ),
        selectizeInput(ns("HK_gene"), "HK gene", choices = NULL),
        selectizeInput(ns("control_condtion"), "Control Condition", choices = NULL),
        selectizeInput(ns("displayed_genes"), "Displayed Genes", choices = NULL, multiple = T)
      ),
      box(
        title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", collapsed = FALSE,width = 12,
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("width"), "Plot width mm (per bar)", 15),
          numericInput(ns("height"), "Plot height mm", 25)
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("font"), "Plot font size", 10),
          numericInput(ns("dotsize"), "Plot dotsize", 1)
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          checkboxInput(ns("Group_Stats"), "Group Stats", value = F),
          checkboxInput(ns("Sample_Stats"), "Sample Stats", value = F)
        ),
        numericInput(ns("top"), "Plot space top mm", 5, step = 1),
        checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
        checkboxInput(ns("Show_ns"), "Show NS", value = F),
        selectizeInput(ns("legend_loc"), "Legend location", choices = c("none", "top", "bottom", "left", "right"), selected = "top"),
        selectizeInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}", "p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}")
      )
      ),
      column(width = 9,
      box(
        height = "calc(70vh - 20px)", 
        title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", collapsed = FALSE,width = 12,
        downloadButton(ns("download_SVG"), "SVG"),
        downloadButton(ns("download_PNG"), "PNG"),
        downloadButton(ns("download_CSV"), "CSV"),
        downloadButton(ns("download_XLSX"), "XLSX"),
        switchInput(inputId = ns("switch"), label = "Live",value = T,inline=T),
        # materialSwitch(inputId = ns("switch"), label = "Live Updates",value = T,inline=T),
        # input_switch(ns("switch"), "Plot Updates",value = T),
        plotOutput(ns("plot"), height = "calc(100vh - 200px)") 
        # %>%
        #   shinycustomloader::withLoader()
      ),
      box(
        title = "Display Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12, collapsed = FALSE,
        splitLayout(
          cellWidths = c("50%", "50%"),
          rHandsontableOutput(ns("Genotype_key_hot")),
          rHandsontableOutput(ns("Treatment_key_hot"))),
        actionButton(ns("colour_key_commit"), "Commit Colour Key")
      )
    )
    )
  )
}

Server_qPCR_plot <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$upload_layout_table, {
        req(input$upload_layout_table)

        df <- read_xlsx(input$upload_layout_table$datapath) |> 
          mutate(across(c("Genotype", "Treatment"), ~ replace_na(.x, ""))) |> 
          glimpse()

        # Ensure required columns exist
        required_cols <- c("Well", "Gene", "Sample", "Genotype", "Treatment", "Replicate")

        if (!all(required_cols %in% colnames(df))) {
          showNotification("Error: Missing required columns!", type = "error")
          return()
        }
        session$userData$vars$qPCR_layout <- df # Update the dataset only if valid
      })


      observeEvent(input$upload_Cq_table, {
        req(input$upload_Cq_table)


        # file_path <- input$upload_Cq_table$datapath
        df <- read_csv(input$upload_Cq_table$datapath)

        # Ensure required columns exist
        required_cols <- c("Well", "Cq")

        if (!all(required_cols %in% colnames(df))) {
          showNotification("Error: Missing required columns!", type = "error")
          return()
        }
        session$userData$vars$Cq_table <- df # Update the dataset only if valid
      })

      observeEvent(input$upload_layout_table, {
        req(session$userData$vars$qPCR_layout)
        updateSelectizeInput(session, "HK_gene",
          choices = na.omit(unique(session$userData$vars$qPCR_layout$Gene)),
          selected = last(na.omit(unique(session$userData$vars$qPCR_layout$Gene)))
        )
        updateSelectizeInput(session, "control_condtion", choices = c("None", unique(paste0(
          session$userData$vars$qPCR_layout$Genotype,
          "_",
          session$userData$vars$qPCR_layout$Treatment
        ))))

        updateSelectizeInput(session, "displayed_genes",
          choices = unique(session$userData$vars$qPCR_layout$Gene),
          selected = unique(session$userData$vars$qPCR_layout$Gene)
        )
      })

      
      output$Genotype_key_hot <- renderRHandsontable({
        req(input$upload_layout_table)
        Genotype_key <- session$userData$vars$qPCR_layout |> 
          select(Genotype) |> 
          filter(!Genotype=="dH2O") |> 
          distinct() |> 
          dplyr::left_join(read_csv("Data_Gitignore/Colour_Key.csv", show_col_types = FALSE), by = join_by(Genotype == Sample)) |> 
          transmute(Genotype = factor(Genotype, levels = unique(Genotype), ordered = TRUE),
                    Colour = ifelse(is.na(color_hex),pastel_palette[seq_len(sum(is.na(color_hex)))],color_hex),
                    Order = as.factor(as.numeric(Genotype)),
                    Show = T)
      
        rhandsontable(Genotype_key)
      })
      
      observeEvent(input$colour_key_commit, {
        req(input$Genotype_key_hot)
        browser()
        # Convert Excel input to tibble
        new_colour_key <- hot_to_df(input$Genotype_key_hot) |> 
          dplyr::as_tibble() |> 
          mutate(color_hex=Colour)
        
        
        
        # Path to the colour key file
        path <- "Data_Gitignore/Colour_Key.csv"
        
        # Load existing file or start empty
        if (file.exists(path)) {
          existing_key <- readr::read_csv(path, show_col_types = FALSE)
        } else {
          existing_key <- tibble::tibble(Sample = character(), color_hex = character())
        }
        
        # Merge: keep new values where present, fall back to old
        updated_key <- dplyr::full_join(existing_key, new_colour_key, by = join_by(Sample == Genotype), suffix = c(".old", ".new")) |>
          dplyr::mutate(color_hex = dplyr::coalesce(color_hex.new, color_hex.old)) |>
          dplyr::select(Sample, color_hex) |>
          dplyr::arrange(Sample)
        
        # Save back to CSV
        readr::write_csv(updated_key, path)
        
        # Optional: notify user
        showNotification("Colour key saved to CSV.", type = "message")
      })
      
      
      
      
      
      output$Treatment_key_hot <- renderRHandsontable({
        req(input$upload_layout_table)
        # browser()
        Treatment_key <- session$userData$vars$qPCR_layout |> 
          select(Treatment) |> 
          distinct() |> 
          mutate(
            Treatment = factor(Treatment, levels = unique(Treatment), ordered = TRUE),
            Order = as.factor(as.numeric(Treatment)),
            Show = T) |> 
          glimpse()
        rhandsontable(Treatment_key)
      })
      

      
      
      outfile_svg <- tempfile(fileext = ".svg")
      outfile_png <- tempfile(fileext = ".png")
      outfile_csv <- tempfile(fileext = ".csv")
      outfile_xlsx <- tempfile(fileext = ".xlsx")
      
      # outfile <- tempfile(fileext = ".svg")
      # outfile_png <- tempfile(fileext = ".png")
      output$plot <- renderImage({
          req(input$upload_Cq_table)
          req(input$upload_layout_table)
          req(input$upload_layout_table)
          req(input$switch)
          

          df <- full_join(session$userData$vars$qPCR_layout,
            session$userData$vars$Cq_table,
            by = "Well", suffix = c("", ".Cq")
          ) |>
            janitor::clean_names() %>%
            left_join(., {
              . |>
                filter(gene %in% input$HK_gene) |>
                transmute(sample, hk_cq = cq) |>
                filter(!is.na(sample))
            }) |>
            mutate(diff_cq = cq - hk_cq) %>%
            left_join(., {
              . |>
                filter(paste0(genotype, "_", treatment) %in% input$control_condtion) |>
                summarise(
                  control_cq = mean(diff_cq,na.rm = T),
                  .by = c(gene)
                )
            }) |>
            mutate(
              expression = 2^(-diff_cq), ,
              rel_expression = 2^(control_cq - diff_cq)
            ) |> 
            filter(!genotype=="dH2O") |> 
            mutate(genotype = factor(genotype, levels = hot_to_df(input$Genotype_key_hot) |> 
                                       arrange(Order) %>%
                                       .$Genotype, ordered = TRUE)) |> 
            mutate(treatment = factor(treatment, levels = hot_to_df(input$Treatment_key_hot) |> 
                                       arrange(Order) %>%
                                       .$Treatment, ordered = TRUE)) |> 
            filter(genotype %in% as.character(hot_to_df(input$Genotype_key_hot) |> 
                                                filter(Show)%>%
                                                .$Genotype)) |> 
            filter(treatment %in% as.character(hot_to_df(input$Treatment_key_hot) |>
                                                 filter(Show)%>%
                                                 .$Treatment))
          
          if (input$control_condtion == "None") {
            df <- df |>
              mutate(value = expression)
            ylab <- "Expression"
          } else {
            df <- df |>
              mutate(value = rel_expression)
            ylab <- "Relative Expression"
          }
          
          genotype_colors <- setNames(hot_to_df(input$Genotype_key_hot)$Colour, 
                                      hot_to_df(input$Genotype_key_hot)$Genotype)
          
          names(genotype_colors) <- gsub("\\^fl/fl","<sup>fl/fl</sup>",names(genotype_colors))
          names(genotype_colors) <- gsub("\\^+","<sup>+</sup>",names(genotype_colors))
          # names(genotype_colors) <- paste0("<i>",names(genotype_colors),"</i>")

          
          #Prepare output for download 
          out <- df |>
            filter(gene %in% input$displayed_genes) |>
            filter(!is.na(genotype)) %>%
            transmute(
              Sample= genotype,
              Value = value,
              Unit_barplot_annotation = ylab,
              Annotation_1_label = "",
              Annotation_1_Symbol = treatment,
              Annotation_2_label = "",
              Annotation_2_Symbol = gene) |>
            write_csv(file = outfile_csv) |>
            glimpse()

          # Split data by gene
          gene_list <- split(out, out$Annotation_2_Symbol)

          # Create a new workbook
          wb <- openxlsx2::wb_workbook()
          

          # Add a worksheet for each gene
          for (gene_name in names(gene_list)) {
            wb$add_worksheet(sheet = gene_name, tab_color = wb_color("lightblue"))
            wb$add_data(sheet = gene_name, gene_list[[gene_name]])
          }
          
          # Save the workbook
          wb_save(wb, file = outfile_xlsx, overwrite = TRUE)
          
          genotype_labels <- levels(df$genotype) %>%
            gsub("\\^fl/fl","<sup>fl/fl</sup>",.) %>%
            gsub("\\^+","<sup>+</sup>",.)
          
          
          # Create Plot
          plot <- df %>%
            left_join(., {
              . |>
                summarise(
                  mean = mean(value),
                  sd = sd(value),
                  n = n(),
                  se = sd / n,
                  .by = c(gene, genotype, treatment)
                )
            }) |>
            filter(!gene %in% input$HK_gene) |>
            filter(gene %in% input$displayed_genes) |>
            filter(!is.na(genotype)) %>%
            # mutate(genotype = gsub("\\^fl/fl"," <sup>fl/fl</sup>",genotype),
            #        genotype = gsub("\\^+"," <sup>+</sup>",genotype)
            #        # genotype = paste0("<i>",genotype,"</i>")
            #        ) |>
            # glimpse() |> 
            mutate(genotype = factor(genotype, levels = levels(genotype), labels = genotype_labels)) |> 
            ggplot(aes(x = treatment, y = value, colour = genotype, fill = genotype)) +
            geom_bar(aes(fill = genotype,group = genotype),
              stat = "summary", fun = "mean",
              colour = "#111111", width = 0.65, linewidth = 0.1, alpha = 0.5,
              position = position_dodge(width = 0.85)
            ) +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se,group = genotype),
                            width = 0.35, linewidth = 0.1,
                            position = position_dodge(width = 0.85)
              )+
            geom_point(aes(fill = genotype,group = genotype),
              size = input$dotsize,
              pch = 21,
              stroke = 0.2,
              color = "#111111",
              position = position_jitterdodge(dodge.width = 0.85,jitter.width = 0.1)
            ) +
            ylab(ylab) +
            scale_fill_manual(values = genotype_colors) +  # Set custom fill colors
            scale_color_manual(values = genotype_colors) + # Set custom outline colors
            facet_wrap(~gene, scales = "free") +
            JPL_genral_theme(font = input$font, legend_loc = input$legend_loc)




          nbars <- 4
          set_panel_size(plot,
                         file = outfile_svg,
                         width = unit(nbars * input$width, "mm"),
                         height = unit(input$height, "mm")
          )
          set_panel_size(plot,
                         file = outfile_png,
                         width = unit(nbars * input$width, "mm"),
                         height = unit(input$height, "mm")
          )
          list(
            src = outfile_svg,
            alt = "This is alternate text"
          )
        },
        deleteFile = F
      )

      output$download_SVG <- downloadHandler(
        filename = function() {
          paste("qPCR-", Sys.Date(), ".svg", sep = "")
        },
        content = function(file) {
          file.copy(
            from = outfile_svg,
            to = file
          )
        }
      )
      output$download_PNG <- downloadHandler(
        filename = function() {
          paste("qPCR-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          file.copy(
            from = outfile_png,
            to = file
          )
        }
      )
      output$download_CSV <- downloadHandler(
        filename = function() {
          paste("qPCR-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          file.copy(
            from = outfile_csv,
            to = file
          )
        }
      )
      output$download_XLSX <- downloadHandler(
        filename = function() {
          paste("qPCR-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          file.copy(
            from = outfile_xlsx,
            to = file
          )
        }
      )
      
      
    }
  )
}