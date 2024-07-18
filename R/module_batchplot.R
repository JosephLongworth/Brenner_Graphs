UI_batchplot <- function(id) {
  ns <- NS(id)
  fluidPage(
    box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
        numericInput(ns("ylab_split"), "Paper ylab split", 20),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("width"), "Plot width mm (per bar)", 5),
          numericInput(ns("height"), "Plot height mm", 30)),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("width_lineplot"), "Plot width mm (lineplot)", 60),
          numericInput(ns("width_survivalplot"), "Plot width mm (survivalplot)", 60)),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("font"), "Plot font size", 7),
          numericInput(ns("dotsize"), "Plot dotsize", 1)),
        numericInput(ns("space_top"), "Plot space top", 1.1),
        checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
        checkboxInput(ns("Show_ns"), "Show NS", value = F)
    ),
     box(title = "Input", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 9, collapsed = FALSE,
      fileInput(ns("file1"), "Choose Excell File",
                accept = c(
                  ".xlsx",
                  ".xls")),
      tags$hr(),
      shinyjs::useShinyjs(),
      actionButton(ns("Run_Plots"), "Run Plots"),
      downloadButton(ns("downloadData"), "Download")
      ),
    box(title = "Colour Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6, collapsed = FALSE,
        rHandsontableOutput(ns("colour_key_hot"))
        )
    )

}

Server_batchplot <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      colour_key <- read_csv("Data/example_colour_key.csv")
      output$colour_key_hot = renderRHandsontable({
        rhandsontable(colour_key)
      })
      
      observeEvent(input$Run_Plots, {
        req(input$file1)
        shinyjs::disable("downloadData")
        
        outfile_zip <- paste0(tempdir(),"/OUT")
        # delete the folder if it exists
        if (dir.exists(outfile_zip)) unlink(outfile_zip, recursive = TRUE)
        dir.create(outfile_zip)
        

        # Path to the excel file
        excel_path <- input$file1$datapath
        # excel_path <- "Anouk_data/figure_data.xlsx"
        # read the excel file
        
          
        for(j in excel_sheets(excel_path)[-1]){
          temp_data <- read_excel(excel_path,sheet = j)
          
          
          
          # bars_count=length(paste0(temp_data$Sample,temp_data$Annotation) %>% 
          #                     unique())
          assign(j,temp_data)
          
          
          if("Unit_barplot" %in% colnames(temp_data)){
            temp_plot <- JPL_barplot(temp_data,
                                     hot_to_df(input$colour_key_hot),
                                     ylab_split=input$ylab_split,
                                     font = input$font,
                                     dotsize = input$dotsize,
                                     space_top = input$space_top,
                                     var_equal = input$var_equal,
                                     Show_ns = input$Show_ns,
                                     legend_loc = "none")
            
            nbars <- temp_data |> 
              select(Sample,Annotation) |>
              distinct() |>
              summarise(nbars=n()) |>
              as.double()
            
            plot_width <- nbars*input$width
            }
          if("Unit_lineplot" %in% colnames(temp_data)){
            temp_plot <- JPL_lineplot(temp_data,
                                      hot_to_df(input$colour_key_hot),
                                      ylab_split=input$ylab_split,
                                      font = input$font,
                                      dotsize = input$dotsize,
                                      space_top = input$space_top,
                                      var_equal = input$var_equal,
                                      Show_ns = input$Show_ns,
                                      legend_loc = "none")
            plot_width <- input$width_lineplot
            }
          if("Unit_survivalplot" %in% colnames(temp_data)){
            temp_plot <- JPL_survivalplot(temp_data,
                                          hot_to_df(input$colour_key_hot),
                                          ylab_split=input$ylab_split,
                                          font = input$font,
                                          dotsize = input$dotsize,
                                          space_top = input$space_top,
                                          var_equal = input$var_equal,
                                          Show_ns = input$Show_ns,
                                          legend_loc = "none")
            plot_width <- input$width_survivalplot
            }
          if("Unit_barplot_annotation" %in% colnames(temp_data)){
            temp_plot <- JPL_barplot_annotation(temp_data,
                                                hot_to_df(input$colour_key_hot),
                                                ylab_split=input$ylab_split,
                                                font = input$font,
                                                dotsize = input$dotsize,
                                                space_top = input$space_top,
                                                var_equal = input$var_equal,
                                                Show_ns = input$Show_ns,
                                                legend_loc = "none")
            
            if("Annotation_2_Symbol" %in% colnames(temp_data)){
              nbars <- temp_data |> 
                select(Sample,Annotation_1_Symbol,Annotation_2_Symbol) |> 
                distinct() |>
                summarise(nbars=n()) |>
                as.double()
              } else {
                nbars <- temp_data |> 
                select(Sample,Annotation_1_Symbol) |> 
                distinct() |>
                summarise(nbars=n()) |>
                as.double()
                }
            
            plot_width <- nbars*input$width
            }
          set_panel_size(temp_plot, file = paste0(outfile_zip,"/",j,".svg"),
                         # width = unit(bars_count*10, "mm"),
                         width = unit(plot_width, "mm"),
                         height = unit(40,"mm"))
        }
        # files within the folder output
        list.files(outfile_zip,full.names = TRUE)
        
        
        
        
        # zip a the folder 'output'
        zip(paste0(tempdir(),"/OUT.zip"),flags = "-j", list.files(outfile_zip,full.names = TRUE))
        
        shinyjs::enable("downloadData")
      })
      
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("Data-", Sys.Date(), ".zip", sep="")
        },
        content = function(file) {
          file.copy(
            from = paste0(tempdir(),"/OUT.zip"),
            to = file
          )
        }
      )
      
      
      }
  )
}