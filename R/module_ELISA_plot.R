UI_ELISA_plot <- function(id) {
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
      tabBox(
        width = 8,
        tabPanel("Plot",
                 downloadButton(ns("downloadPaper"), "SVG"),
                 downloadButton(ns("downloadPaperpng"), "PNG"),
                 switchInput(inputId = ns("switch"), label = "Live",value = T,inline=T),
                 # materialSwitch(inputId = ns("switch"), label = "Live Updates",value = T,inline=T),
                 # input_switch(ns("switch"), "Plot Updates",value = T),
                 plotOutput(ns("plot"))),
        tabPanel("diff", rHandsontableOutput(ns("hot_diff"))),
        tabPanel("450nm", rHandsontableOutput(ns("hot_450nm"))),
        tabPanel("570nm", rHandsontableOutput(ns("hot_570nm"))),
      )
      )
    ,
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

Server_ELISA_plot <- function(id) {
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
        session$userData$vars$ELISA_df <- df |> 
          mutate(X450 = NA_real_,
                 X570 = NA_real_,
                 diff = NA_real_)})



      
      
      
      
      
      
      
      
      
      
      # 
      # output$hot_570nm = renderRHandsontable({
      #   rhandsontable({
      #     #####################
      #     
      #     Well = wellr::well_from_index(1:as.numeric(input$plate_size), plate = as.numeric(input$plate_size), num_width = 0),
      #     
      #     session$userData$vars$ELISA_df |>
      #       select(Well, Gene) |>
      #       mutate(
      #         Col = wellr::well_to_col_num(Well),
      #         Row = wellr::well_to_row_let(Well),
      #         .keep = "unused"
      #       ) |>
      #       pivot_wider(names_from = Col, values_from = Gene) |>
      #       column_to_rownames(var = "Row")
      #   })
      # }) 
      
      
      
      output$hot_450nm <- renderRHandsontable({
        req(input$upload_layout_table)
        rhandsontable({session$userData$vars$ELISA_df |> 
            select(Well,X450) |> 
            mutate(
              Col = wellr::well_to_col_num(Well),
              Row = wellr::well_to_row_let(Well),
              .keep = "unused"
            ) |>
            pivot_wider(names_from = Col, values_from = X450) |>
            column_to_rownames(var = "Row")})
      })
      output$hot_570nm <- renderRHandsontable({
        req(input$upload_layout_table)
        rhandsontable({session$userData$vars$ELISA_df |> 
            select(Well,X570) |> 
            mutate(
              Col = wellr::well_to_col_num(Well),
              Row = wellr::well_to_row_let(Well),
              .keep = "unused"
            ) |>
            pivot_wider(names_from = Col, values_from = X570) |>
            column_to_rownames(var = "Row")})
      })
      
      output$hot_diff <- renderRHandsontable({
        req(input$upload_layout_table)
        rhandsontable({session$userData$vars$ELISA_df |> 
            select(Well,diff) |> 
            mutate(
              Col = wellr::well_to_col_num(Well),
              Row = wellr::well_to_row_let(Well),
              .keep = "unused"
            ) |>
            pivot_wider(names_from = Col, values_from = diff) |>
            column_to_rownames(var = "Row")})
      }) |> 
        bindEvent(c(input$hot_450nm),
                  c(input$hot_570nm))
      
      
      
      observe({
        
        req(input$hot_450nm)
        req(input$hot_570nm)

        session$userData$vars$ELISA_df$X450 <- hot_to_df(input$hot_450nm) |>
          pivot_longer(everything(),
                       names_to = "Col",
                       values_to = "X450") |>
          select(X450) |>
          unlist() |>
          as.vector()
        
        session$userData$vars$ELISA_df$X570 <- hot_to_df(input$hot_570nm) |>
          pivot_longer(everything(),
                       names_to = "Col",
                       values_to = "X570") |>
          select(X570) |>
          unlist() |>
          as.vector()
        
        
        # if(  !(all(is.na(session$userData$vars$ELISA_df$X450))|all(is.na(session$userData$vars$ELISA_df$X570)))
        # ){ 
          session$userData$vars$ELISA_df$diff <- session$userData$vars$ELISA_df$X450 - session$userData$vars$ELISA_df$X570
        # }
        
        
          }) |> 
        bindEvent(c(input$hot_450nm,
                    input$hot_570nm,
                    input$hot_diff))
      
      observeEvent(input$upload_layout_table, {
        req(session$userData$vars$ELISA_df)
        updateSelectizeInput(session, "HK_gene",
          choices = unique(session$userData$vars$ELISA_df$Gene),
          selected = last(unique(session$userData$vars$ELISA_df$Gene))
        )
        updateSelectizeInput(session, "control_condtion", choices = c("None", unique(paste0(
          session$userData$vars$ELISA_df$Genotype,
          "_",
          session$userData$vars$ELISA_df$Treatment
        ))))

        updateSelectizeInput(session, "displayed_genes",
          choices = unique(session$userData$vars$ELISA_df$Gene),
          selected = unique(session$userData$vars$ELISA_df$Gene)
        )
      })



      
      observe({
        
        TESTING <<- session$userData$vars$ELISA_df
        browser()
        
      }) |> 
        bindEvent(c(input$hot_diff))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      output$Genotype_key_hot <- renderRHandsontable({
        req(input$upload_layout_table)
        # browser()
        Genotype_key <- session$userData$vars$ELISA_df |> 
          dplyr::select(Genotype) |> 
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
        Treatment_key <- session$userData$vars$ELISA_df |> 
          select(Treatment) |> 
          distinct() |> 
          mutate(
            Treatment = factor(Treatment, levels = unique(Treatment), ordered = TRUE),
            Order = as.factor(as.numeric(Treatment)),
            Show = T) |> 
          glimpse()
        rhandsontable(Treatment_key)
      })
      

      
      
      
      outfile <- tempfile(fileext = ".svg")
      outfile_png <- tempfile(fileext = ".png")
      output$plot <- renderImage(
        {
          req(input$upload_Cq_table)
          req(input$upload_layout_table)
          req(input$switch)
          # browser()

          df <- full_join(session$userData$vars$ELISA_df,
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
            {
              if (input$Group_Stats) {
                geom_pwc(
                  tip.length = 0,
                  # ref.group = 1,
                  group.by = "x.var",
                  method = "t_test",
                  method.args = list(var.equal = input$var_equal),
                  p.adjust.method = "bonferroni",
                  label = input$Stat_type,
                  label.size = input$font / .pt, size = 0.1,
                  hide.ns = !input$Show_ns,
                  colour = "#111111",
                  family = family
                  # )+
                )
              }
            } +
            {
              if (input$Sample_Stats) {
                geom_pwc(aes(group = Sample),
                  tip.length = 0,
                  # ref.group = "all",
                  group.by = "legend.var",
                  bracket.group.by = "legend.var",
                  dodge = 0.85,
                  method = "t_test",
                  method.args = list(var.equal = input$var_equal),
                  p.adjust.method = "bonferroni",
                  label = input$Stat_type,
                  label.size = input$font / .pt, size = 0.1,
                  hide.ns = !input$Show_ns,
                  bracket.nudge.y = 0.2,
                  colour = "#111111",
                  family = family
                )
              }
            } +
            ylab(ylab) +
            scale_fill_manual(values = genotype_colors) +  # Set custom fill colors
            scale_color_manual(values = genotype_colors) + # Set custom outline colors
            facet_wrap(~gene, scales = "free") +
            JPL_genral_theme(font = input$font, legend_loc = input$legend_loc)




          nbars <- 4

          set_panel_size(plot,
            file = outfile,
            width = unit(nbars * input$width, "mm"),
            height = unit(input$height, "mm")
          )
          set_panel_size(plot,
            file = outfile_png,
            width = unit(nbars * input$width, "mm"),
            height = unit(input$height, "mm")
          )
          list(
            src = outfile,
            alt = "This is alternate text"
          )
        },
        deleteFile = F
      )
      output$downloadPaper <- downloadHandler(
        filename = function() {
          paste("PaperSize-", Sys.Date(), ".svg", sep = "")
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
          paste("PaperSize-", Sys.Date(), ".png", sep = "")
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
