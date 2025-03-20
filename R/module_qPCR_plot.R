UI_qPCR_plot <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        title = "upload", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3,
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
        title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE,
        downloadButton(ns("download_SVG"), "SVG"),
        downloadButton(ns("download_PNG"), "PNG"),
        downloadButton(ns("download_CSV"), "CSV"),
        switchInput(inputId = ns("switch"), label = "Live",value = T,inline=T),
        # materialSwitch(inputId = ns("switch"), label = "Live Updates",value = T,inline=T),
        # input_switch(ns("switch"), "Plot Updates",value = T),
        plotOutput(ns("plot")) 
        # %>%
        #   shinycustomloader::withLoader()
      )
    ),
    fluidRow(
      box(
        title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
        radioButtons(
          inputId = ns("defaults"), label = NULL, choices = c("Paper", "Presentation"),
          selected = "Presentation", inline = T
        ),
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
      ),
      box(
        title = "Display Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 9, collapsed = FALSE,
        splitLayout(
          cellWidths = c("50%", "50%"),
          rHandsontableOutput(ns("Genotype_key_hot")),
          rHandsontableOutput(ns("Treatment_key_hot")))
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
        # browser()
        Genotype_key <- session$userData$vars$qPCR_layout |> 
          select(Genotype) |> 
          distinct() |> 
          mutate(
            Genotype = factor(Genotype, levels = unique(Genotype), ordered = TRUE),
            Order = as.factor(as.numeric(Genotype)),
            Colour = pastel_palette[seq_len(n())],
            Show = T)|> 
          glimpse()
      rhandsontable(Genotype_key)
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
      
      # outfile <- tempfile(fileext = ".svg")
      # outfile_png <- tempfile(fileext = ".png")
      output$plot <- renderImage(
        {
          req(input$upload_Cq_table)
          req(input$upload_layout_table)
          req(input$upload_layout_table)
          req(input$switch)
          # browser()

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
                  control_cq = mean(diff_cq),
                  .by = c(gene)
                )
            }) |>
            mutate(
              expression = 2^(diff_cq), ,
              rel_expression = 2^(control_cq - diff_cq)
            ) |> 
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
                                                 .$Treatment)) |>
            glimpse()
          
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

          # browser()
          
          
          df |>
            filter(gene %in% input$displayed_genes) |>
            filter(!is.na(genotype)) %>%
            separate_wider_delim(treatment,delim = "_",names = c("Annotation_1_Symbol","Annotation_2_Symbol"),too_few = "align_start",too_many = "merge") |>
            transmute(
              Sample= genotype,
              Value = value,
              Unit_barplot_annotation = ylab,
              Annotation_1_label = "",
              Annotation_1_Symbol,
              Annotation_2_label = "",
              Annotation_2_Symbol) |>
            write_csv(file = outfile_csv) |>
            glimpse()

          

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
            ggplot(aes(x = treatment, y = value, colour = genotype, fill = genotype)) +
            geom_bar(aes(fill = genotype),
              stat = "summary", fun = "mean",
              colour = "#111111", width = 0.65, linewidth = 0.1, alpha = 0.5,
              position = position_dodge(width = 0.85)
            ) +
            geom_point(aes(fill = genotype),
              size = input$dotsize,
              pch = 21,
              stroke = 0.2,
              color = "#111111",
              position = (position_jitterdodge(dodge.width = 0.85))
            ) +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
              width = 0.3, linewidth = 0.1,
              position = position_dodge(width = 0.85)
            ) +
            # {
            #   if (input$Group_Stats) {
            #     geom_pwc(
            #       tip.length = 0,
            #       # ref.group = 1,
            #       group.by = "x.var",
            #       method = "t_test",
            #       method.args = list(var.equal = input$var_equal),
            #       p.adjust.method = "bonferroni",
            #       label = input$Stat_type,
            #       label.size = input$font / .pt, size = 0.1,
            #       hide.ns = !input$Show_ns,
            #       colour = "#111111",
            #       family = family
            #       # )+
            #     )
            #   }
            # } +
            # {
            #   if (input$Sample_Stats) {
            #     geom_pwc(aes(group = Sample),
            #       tip.length = 0,
            #       # ref.group = "all",
            #       group.by = "legend.var",
            #       bracket.group.by = "legend.var",
            #       dodge = 0.85,
            #       method = "t_test",
            #       method.args = list(var.equal = input$var_equal),
            #       p.adjust.method = "bonferroni",
            #       label = input$Stat_type,
            #       label.size = input$font / .pt, size = 0.1,
            #       hide.ns = !input$Show_ns,
            #       bracket.nudge.y = 0.2,
            #       colour = "#111111",
            #       family = family
            #     )
            #   }
            # } +
            ylab(ylab) +
            scale_fill_manual(values = genotype_colors) +  # Set custom fill colors
            scale_color_manual(values = genotype_colors) + # Set custom outline colors
            facet_wrap(~gene, scales = "free") +
            JPL_genral_theme(font = input$font, legend_loc = input$legend_loc)



browser
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
      # output$downloadPaper <- downloadHandler(
      #   filename = function() {
      #     paste("PaperSize-", Sys.Date(), ".svg", sep = "")
      #   },
      #   content = function(file) {
      #     file.copy(
      #       from = outfile,
      #       to = file
      #     )
      #   }
      # )
      # output$downloadPaperpng <- downloadHandler(
      #   filename = function() {
      #     paste("PaperSize-", Sys.Date(), ".png", sep = "")
      #   },
      #   content = function(file) {
      #     file.copy(
      #       from = outfile_png,
      #       to = file
      #     )
      #   }
      # )
      
      output$download_SVG <- downloadHandler(
        filename = function() {
          paste("PCA-", Sys.Date(), ".svg", sep = "")
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
          paste("PCA-", Sys.Date(), ".png", sep = "")
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
          paste("PCA-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          file.copy(
            from = outfile_csv,
            to = file
          )
        }
      )
      
      
    }
  )
}