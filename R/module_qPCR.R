UI_qPCR <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
              box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
                  radioButtons(inputId = ns("defaults"),label = NULL, choices = c("Paper", "Presentation"),
                               selected = "Presentation",inline = T),
                  # splitLayout(
                  #   cellWidths = c("50%", "50%"),
                  #   numericInput(ns("width"), "Plot width mm (per bar)", 7.5),
                  #   numericInput(ns("height"), "Plot height mm", 25)),
                  # splitLayout(
                  #   cellWidths = c("50%", "50%"),
                  #   numericInput(ns("font"), "Plot font size", 7),
                  #   numericInput(ns("dotsize"), "Plot dotsize", 1)),
                  # splitLayout(
                  #   cellWidths = c("50%", "50%"),
                  #   checkboxInput(ns("Group_Stats"), "Group Stats", value = T),
                  #   checkboxInput(ns("Sample_Stats"), "Sample Stats", value = F)),
                  # numericInput(ns("top"), "Plot space top mm", 5, step = 1),
                  # checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
                  # checkboxInput(ns("Show_ns"), "Show NS", value = F),
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
      tabBox(width=12,
        tabPanel("Layout_Table",DTOutput(ns("layout_table"))),
        # tabPanel("Long",rHandsontableOutput(ns("hot_Long"))),
        tabPanel("Genotype",rHandsontableOutput(ns("hot_Genotype"))),
        tabPanel("Treatment",rHandsontableOutput(ns("hot_Treatment")))
             # tabPanel("Replicate",rHandsontableOutput(ns("hot"))),
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
      
      # df <- tibble(Well = wellr::well_from_index(1:384,plate = 384),
      #              Gene = NA,
      #              Sample = NA,
      #              Genotype = NA,
      #              Treatment = NA,
      #              Replicate = NA)
      # 
      session$userData$vars$df <- tibble(
        Well = wellr::well_from_index(1:384, plate = 384,num_width = 0),
        Gene = NA_character_,
        Sample = NA_character_,
        Genotype = NA_character_,
        Treatment = NA_character_,
        Replicate = NA_character_
      )
    
      
      # df$Genotype = values[["hot_Genotype"]]$Genotype
      
      # df$Well
      # 
      # 
      # well_plate()
      
      # wellr::well_platefrom_indexindex(25, plate = 384,num_width = )
      
      # df$Genotype <- sample(c("WT","KO"),384,replace = T)
      
      
      df_Genotype <- session$userData$vars$df |>
        select(Well,Genotype) |>
        mutate(Col = wellr::well_to_col_num(Well),
               Row = wellr::well_to_row_let(Well),.keep = "unused") |>
        pivot_wider(names_from = Col,values_from = Genotype) |>
        column_to_rownames(var = "Row") |>
        glimpse()
      
      
      ######
      ######
      ######
      ###### Try to build it with a non hot table for hot long?
      ######
      ######
      
      

      library(DT)



      
      observeEvent(input$hot_Genotype,{
        output$layout_table <- renderDT({
          # browser()
          datatable(session$userData$vars$df, 
                    options = list(
                      pageLength = 10,  # Show 10 rows per page
                      autoWidth = TRUE, 
                      searching = TRUE, # Enable search
                      dom = 'tip'  # Remove unnecessary elements, keep table, search, pagination
                    ),
                    class = "display nowrap compact") # Make it compact
        })})
      
      
      
      ######
      
     
        observeEvent(input$hot_Genotype,{
          
          # session$userData$vars$df2 <- hot_to_df(input$hot_Genotype)
          # session$userData$vars$df$Genotype <- hot_to_df(input$hot_Genotype) |>
# 
          session$userData$vars$df$Genotype <- hot_to_df(input$hot_Genotype) |>
            pivot_longer(everything(),names_to = "Col",values_to = "Genotype") |>
            select(Genotype) |>
            unlist() |> 
            as.vector()
          
          # session$userData$vars$df
          # values[["hot_Genotype"]]
          
          # browser()
        })
      
      
      
      
      
      
       
      
      # output$hot_Long = renderRHandsontable({
      #   rhandsontable(df)
      # })
      output$hot_Genotype = renderRHandsontable({
        rhandsontable(df_Genotype)
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
            
      outfile <- tempfile(fileext='.svg')
      outfile_png <- tempfile(fileext='.png')
      
      observeEvent(input$hot_Genotype,{
      output$plot <- renderPlot({
        # browser()
      
        # req(input$hot_Long)
        # hot_to_df(input$hot_Long) 
        session$userData$vars$df |>
        mutate(Well = gsub("([A-P])0([1-9])", "\\1\\2", Well)) |>
        # mutate(Genotype = sample(c("WT","KO"),384,replace = T))  |>
        
      ggplate::plate_plot(
        # data = data,
        position = Well,
        value = Genotype,
        plate_size = 384,
        plate_type = "square"
      )
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

