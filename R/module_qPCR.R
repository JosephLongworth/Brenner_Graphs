UI_qPCR <- function(id) {
  ns <- NS(id)
  fluidPage(
fluidRow(
      tabBox(width=12,
        tabPanel("Layout_Table",
          fluidRow(
            box(width=6,solidHeader = TRUE,
                DTOutput(ns("layout_table"))
                ),
            box(width=6,solidHeader = TRUE,
                imageOutput(ns("plot")) |> 
                  shinycustomloader::withLoader(),
                downloadButton(ns("downloadPaperpng"), "PNG")
                )
          )
          ),
        # tabPanel("Long",rHandsontableOutput(ns("hot_Long"))),
        tabPanel("Gene",rHandsontableOutput(ns("hot_Gene"))),
        tabPanel("Genotype",rHandsontableOutput(ns("hot_Genotype"))),
        tabPanel("Treatment",rHandsontableOutput(ns("hot_Treatment")))
             # tabPanel("Replicate",rHandsontableOutput(ns("hot"))),
      )
    ),
fluidRow(
  box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
      radioButtons(inputId = ns("defaults"),label = NULL, choices = c("Paper", "Presentation"),
                   selected = "Presentation",inline = T),
      selectizeInput(ns("legend_loc"), "Legend location", choices = c("none","top","bottom","left","right"), selected = "none"),
      selectizeInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}","p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}")
      
  ),
  box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE
      # downloadButton(ns("downloadPaper"), "SVG"),
      # downloadButton(ns("downloadPaperpng"), "PNG"),
     
  )
)
    )

}

Server_qPCR <- function(id) {
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
      
      session$userData$vars$df <- tibble(
        Well = wellr::well_from_index(1:384, plate = 384,num_width = 0),
        Gene = NA_character_,
        # Sample = NA_character_,
        Genotype = NA_character_,
        Treatment = NA_character_,
        Replicate = NA_character_
      )
      
      outfile <- tempfile(fileext='.svg')
      outfile_png <- tempfile(fileext='.png')
      
      # df_reactive <- reactive({ session$userData$vars$df})

      
      
      output$plot <- renderImage({
        plate_plot <- session$userData$vars$df |>
          mutate(Well = gsub("([A-P])0([1-9])", "\\1\\2", Well)) |>
          ggplate::plate_plot(
            position = Well,
            value = Gene,
            label = paste(Genotype,"\n",Treatment),
            plate_size = 384,,
            legend_n_row = 8,  
            plate_type = "square")
        
        set_panel_size(plate_plot, file = outfile,
                       width = unit(120, "mm"),
                       height = unit(80,"mm"))
        list(src = outfile,
             alt = "This is alternate text")
        
        }) |>
        bindEvent(c(input$hot_Gene,
                    input$hot_Genotype,
                    input$hot_Treatment),
                  ignoreNULL = F)
      
      
        output$layout_table <- renderDT({
          # browser()
          # df_reactive <- reactive({ session$userData$vars$df})
          datatable(session$userData$vars$df, 
                    options = list(
                      pageLength = 10,  # Show 10 rows per page
                      autoWidth = TRUE, 
                      searching = TRUE, # Enable search
                      dom = 'tip'  # Remove unnecessary elements, keep table, search, pagination
                    ),
                    class = "display nowrap compact") }) |> 
          bindEvent(c(input$hot_Gene,
                      input$hot_Genotype,
                      input$hot_Treatment),
                    ignoreNULL = F)
        
        
 
      
      
      
      
### Update main layout df based on inputs from hot tables
### 
      observeEvent(input$hot_Gene,{
        session$userData$vars$df$Gene <- hot_to_df(input$hot_Gene) |>
          pivot_longer(everything(),names_to = "Col",values_to = "Gene") |>
          select(Gene) |>
          unlist() |> 
          as.vector()
      })
      
      observeEvent(input$hot_Genotype,{
        session$userData$vars$df$Genotype <- hot_to_df(input$hot_Genotype) |>
          pivot_longer(everything(),names_to = "Col",values_to = "Genotype") |>
          select(Genotype) |>
          unlist() |> 
          as.vector()
      })
      observeEvent(input$hot_Treatment,{
        session$userData$vars$df$Treatment <- hot_to_df(input$hot_Treatment) |>
          pivot_longer(everything(),names_to = "Col",values_to = "Treatment") |>
          select(Treatment) |>
          unlist() |> 
          as.vector()
      })
      
### Prepare Hot tables for Gene Genotype and Treatment
      

      output$hot_Gene = renderRHandsontable({
        rhandsontable({
          session$userData$vars$df |>
            select(Well,Gene) |>
            mutate(Col = wellr::well_to_col_num(Well),
                   Row = wellr::well_to_row_let(Well),.keep = "unused") |>
            pivot_wider(names_from = Col,values_from = Gene) |>
            column_to_rownames(var = "Row")
        })
      })
      output$hot_Genotype = renderRHandsontable({
        rhandsontable({
          session$userData$vars$df |>
            select(Well,Genotype) |>
            mutate(Col = wellr::well_to_col_num(Well),
                   Row = wellr::well_to_row_let(Well),.keep = "unused") |>
            pivot_wider(names_from = Col,values_from = Genotype) |>
            column_to_rownames(var = "Row")
        })
      })

      output$hot_Treatment = renderRHandsontable({
        rhandsontable({
          session$userData$vars$df |>
            select(Well,Treatment) |>
            mutate(Col = wellr::well_to_col_num(Well),
                   Row = wellr::well_to_row_let(Well),.keep = "unused") |>
            pivot_wider(names_from = Col,values_from = Treatment) |>
            column_to_rownames(var = "Row")
        })
      })
      
      

      
      # output$plot <- renderImage({
      #   req(input$hot)
      #   empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
      #     theme_void()
      #   # browser()
      #   plot <- JPL_barplot_annotation(hot_to_df(input$hot),
      #                                  hot_to_df(input$colour_key_hot),
      #                                  font = input$font,
      #                                  dotsize = input$dotsize,
      #                                  top = input$top,
      #                                  var_equal = input$var_equal,
      #                                  Show_ns = input$Show_ns,
      #                                  legend_loc = input$legend_loc,
      #                                  label = input$Stat_type,
      #                                  Group_Stats = input$Group_Stats,
      #                                  Sample_Stats = input$Sample_Stats
      #                                  ) +
      #   theme(rect = element_rect(fill = "transparent"))
      # 
      #   nbars <- hot_to_df(input$hot) |>
      #     select(Sample,Annotation_1_Symbol,Annotation_2_Symbol) |>
      #     distinct() |>
      #     summarise(nbars=n()) |>
      #     as.double()
      # 
      #   set_panel_size(plot, file = outfile ,
      #                  width = unit(nbars * input$width, "mm"),
      #                  height = unit(input$height,"mm"))
      #   set_panel_size(plot, file = outfile_png,
      #                  width = unit(nbars * input$width, "mm"),
      #                  height = unit(input$height,"mm"))
      #   list(src = outfile,
      #        alt = "This is alternate text")
      # }, deleteFile = F)
      # 
      # 






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

