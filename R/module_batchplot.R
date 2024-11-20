UI_batchplot <- function(id) {
  ns <- NS(id)
  fluidPage(
    box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("width"), "Plot width mm (per bar)", 7.5),
          numericInput(ns("height"), "Plot height mm", 25)),
        h4("Plot width (mm)"),
        splitLayout(
          cellWidths = c("33%", "33%", "33%"),
          numericInput(ns("width_blankplot"), "blankplot", 60),
          numericInput(ns("width_lineplot"), "lineplot", 60),
          numericInput(ns("width_survivalplot"), "survivalplot", 60)),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("font"), "Plot font size", 7),
          numericInput(ns("dotsize"), "Plot dotsize", 1)),
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

      colour_key <- read_csv("Data/example_colour_key.csv",show_col_types = FALSE)
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
        
          
        sheet_count_total <- length(excel_sheets(excel_path)[-1])
        sheet_count <- 0
        # start a progress bar in shiny 
        # with the number of sheets in the excel file
        progress <- shiny::Progress$new()
        
        
        for(j in excel_sheets(excel_path)[-1]){
                    temp_data <- read_excel(excel_path,sheet = j,na = c("","NA","na","N/A","n/a","NaN","nan","NAN","Nan"))
          
          sheet_count <- sheet_count + 1
          sheet_count/sheet_count_total*100
          
          # update progress bar
          progress$set(value = sheet_count/sheet_count_total,
                       message = paste0("Processing sheet ",j))

           length(excel_sheets(excel_path)[-1])
          
          # bars_count=length(paste0(temp_data$Sample,temp_data$Annotation) %>% 
          #                     unique())
          assign(j,temp_data)
          
          colnames(temp_data)
          
          barplot_head <- c("Sample","Value","Unit","Annotation")
          lineplot_head <- c("Sample","Value","Unit_lineplot","Days post infection",	"Replicate")
          survivalplot_head <- c("Day","Sample","Mouse_status","Unit_survivalplot")
          barplot_annotation_head <- c("Sample","Value","Unit","Annotation_1_label","Annotation_1_Symbol")
      # browser()
          
          if(ncol(temp_data)==0){
            temp_plot <-ggplot() +
              labs(title = j,
                   subtitle = "To be produced/uploaded")+
              theme_void()
            
            plot_width <- input$width_blankplot
            
          } else if (all( barplot_head %in% colnames(temp_data))){
            if(all(is.na(temp_data$Value))){
              temp_plot <-ggplot() +
                labs(title = j,
                     subtitle = "To be produced/uploaded")+
                theme_void()
              
              plot_width <- input$width_blankplot
              
            }else{
         
            
            temp_data <- temp_data |> 
              mutate(Annotation_1_Symbol = NA,
                     Annotation_1_label = NA,
                     Annotation_2_Symbol = NA,
                     Annotation_2_label = NA)
                     
            
            
            temp_plot <- JPL_barplot_annotation(temp_data,
                                                hot_to_df(input$colour_key_hot),
                                                font = input$font,
                                                dotsize = input$dotsize,
                                                var_equal = input$var_equal,
                                                Show_ns = input$Show_ns,
                                                legend_loc = "none")
            
            

            nbars <- temp_data |> 
              select(Sample,Annotation) |>
              distinct() |>
              summarise(nbars=n()) |>
              as.double()
            
            plot_width <- nbars*input$width
          }} else if(all( lineplot_head %in% colnames(temp_data))){
            # browser()
            
            if("Unit_lineplot" %in% colnames(temp_data)){
              temp_data <- temp_data %>%
                mutate(Unit = Unit_lineplot,.keep = c("unused"))%>% 
                mutate(Sample = as_factor(Sample))
              }
            
            
            temp_plot <- JPL_lineplot(temp_data,
                                      colour_key = hot_to_df(input$colour_key_hot),
                                      font = input$font,
                                      dotsize = input$dotsize,
                                      legend_loc = "none")
            plot_width <- input$width_lineplot
          } else if(all(survivalplot_head %in% colnames(temp_data))){
            # browser()
            
            if("Unit_survivalplot" %in% colnames(temp_data)){
            temp_data <- temp_data %>%
              mutate(Unit = Unit_survivalplot,.keep = c("unused"))%>% 
              mutate(Sample = as_factor(Sample))
          }
            
            temp_plot <- JPL_survivalplot(df = temp_data,
                                          colour_key = hot_to_df(input$colour_key_hot),
                                          font = input$font,
                                          legend_loc = "none"
                                          )
            
            testing <<- temp_plot
            
            plot_width <- input$width_survivalplot
          } else if(all( barplot_annotation_head %in% colnames(temp_data))){
            if(all(is.na(temp_data$Value))){
              temp_plot <-ggplot() +
                labs(title = j,
                     subtitle = "To be produced/uploaded")+
                theme_void()
              
              plot_width <- input$width_blankplot
              
            }else{
            temp_plot <- JPL_barplot_annotation(temp_data,
                                                colour_key=  hot_to_df(input$colour_key_hot),
                                                font = input$font,
                                                dotsize = input$dotsize,
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
            }}
          set_panel_size(temp_plot, file = paste0(outfile_zip,"/",j,".svg"),
                         # width = unit(bars_count*10, "mm"),
                         width = unit(plot_width, "mm"),
                         height = unit(input$height,"mm"))
        }
        # progress <- shiny::Progress$close()
        
        
        # files within the folder output
        list.files(outfile_zip,full.names = TRUE)
        
        
        
        # browser()
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