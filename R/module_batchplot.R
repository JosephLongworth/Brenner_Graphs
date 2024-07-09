UI_batchplot <- function(id) {
  ns <- NS(id)
  fluidPage(
     box(title = "Input", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12, collapsed = FALSE,
      fileInput(ns("file1"), "Choose Excell File",
                accept = c(
                  ".xlsx",
                  ".xls")),
      tags$hr(),
      actionButton(ns("Run_Plots"), "Run Plots")
  ), 
  box(title = "Output", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 12, collapsed = FALSE,
      downloadButton(ns("downloadData"), "Download")
  ))

}

Server_batchplot <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$Run_Plots, {
        
        req(input$file1)
        
        outfile_zip <- paste0(tempdir(),"/OUT")
        # delete the folder if it exists
        if (dir.exists(outfile_zip)) unlink(outfile_zip, recursive = TRUE)
        dir.create(outfile_zip)
        
        colour_key <- read_csv("Data/example_colour_key.csv")
        
        # Path to the excel file
        excel_path <- input$file1$datapath
        # excel_path <- "Anouk_data/figure_data.xlsx"
        
        
        # read the excel file
        
        for(j in excel_sheets(excel_path)){
          temp_data <- read_excel(excel_path,sheet = j)
          
          
          
          # bars_count=length(paste0(temp_data$Sample,temp_data$Annotation) %>% 
          #                     unique())
          assign(j,temp_data)
          
          
          if("Unit_barplot" %in% colnames(temp_data)){
            temp_plot <- JPL_barplot(temp_data, colour_key,legend_loc = "none")
            }
          if("Unit_lineplot" %in% colnames(temp_data)){
            temp_plot <- JPL_lineplot(temp_data, colour_key,legend_loc = "none")
            }
          if("Unit_survivalplot" %in% colnames(temp_data)){
            temp_plot <- JPL_survivalplot(temp_data, colour_key,legend_loc = "none")
            }
          set_panel_size(temp_plot, file = paste0(outfile_zip,"/",j,".svg"),
                         # width = unit(bars_count*10, "mm"),
                         width = unit(40, "mm"),
                         height = unit(40,"mm"))
        }
        # files within the folder output
        list.files(outfile_zip,full.names = TRUE)
        
        
        
        
        # zip a the folder 'output'
        zip(paste0(tempdir(),"/OUT.zip"),flags = "-j", list.files(outfile_zip,full.names = TRUE))
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