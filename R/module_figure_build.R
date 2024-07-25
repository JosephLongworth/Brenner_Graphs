UI_figure_builder <- function(id) {
  ns <- NS(id)
  fluidPage(
     box(title = "Input", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12, collapsed = FALSE,
         fileInput(ns("files"), "Choose SVG File", multiple = TRUE,
                accept = c(".svg")),
      tags$hr(),
      shinyjs::useShinyjs(),
      downloadButton(ns("downloadData"), "Download")
      ),
     box(title = "Layout", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 4, collapsed = FALSE,
         rHandsontableOutput(ns("Layout_hot"))
     ),
     box(title = "Labels", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 4, collapsed = FALSE,
         rHandsontableOutput(ns("Labels_hot"))
     ),
     box(title = "Plot Preview", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 8, collapsed = FALSE,
         actionButton(ns("Run_Plots"), "Update Figure"),
         imageOutput(ns("plot_preview"))
     )
     
    )

}


Server_figure_builder <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      Layout <- tibble("Panel Name"=c("GRAPH_pIC_anti-IFNAR_Ifit1_2"),"X"=c(11),"Y"=c(11),"X Offset"=c(11),"Y Offset"=c(11))
      output$Layout_hot = renderRHandsontable({
        rhandsontable(Layout)
      })
      
      Labels <- tibble("Label"=c("A"),"X"=c(11),"Y"=c(11))
      output$Labels_hot = renderRHandsontable({
        rhandsontable(Labels)
      })
      

      observeEvent(input$Run_Plots, {
        req(input$files)
        shinyjs::disable("downloadData")

        # outfile_zip <- paste0(tempdir(),"/OUT")
        # # delete the folder if it exists
        # if (dir.exists(outfile_zip)) unlink(outfile_zip, recursive = TRUE)
        # dir.create(outfile_zip)


        # Path to the excel file
        excel_path <- input$files$datapath
        # excel_path <- "Anouk_data/figure_data.xlsx"
        # read the excel file
        # browser()

        readLines("Data/Standard_SVG_Head.svg") |> 
        write_lines("Anouk_data/Figure_2.svg")
        # write_lines("www/Figure_1.svg")
        
        i <- 1
        layout <- hot_to_df(input$Layout_hot) |> 
          janitor::clean_names() 
        
        panel_name <- layout$panel_name[i]
        panel_id <- grep(pattern = panel_name,x = input$files$name)
        
        svg_code <- readLines(input$files$datapath[panel_id])
        g_code_lines <- tibble::tibble(g_start = grep("<g",svg_code,value = F),
                                       g_end = grep("</g",svg_code,value = F)) |>
          arrange(desc(g_start))
        
        
        if(grepl("transform=",svg_code[g_code_lines$g_start[1]])){
          
          
          matrix <- str_match(pattern = "\\(.+\\)",svg_code[g_code_lines$g_start[1]])
          matrix <- substr(matrix[1],2,nchar(matrix[1])-1) 
          matrix <- strsplit(matrix,",")[[1]]
          matrix <- as.numeric(matrix)
          matrix[5]=matrix[5]+layout$x[i]-0
          matrix[6]=matrix[6]+layout$y[i]-0
          matrix <- paste0("(",paste0(matrix,collapse = ","),")")
          
          
          svg_code[g_code_lines$g_start[1]] <- gsub(pattern = "\\(.+\\)",matrix,svg_code[g_code_lines$g_start[1]])
          
        } else if (grepl("id=",svg_code[g_code_lines$g_start[1]])){
          
          svg_code[g_code_lines$g_start[1]] <- paste0(substr(svg_code[g_code_lines$g_start[1]],1,nchar(svg_code[g_code_lines$g_start[1]])-1),
                                                      " transform='translate(",
                                                      layout$x[i]-0,
                                                      ",",
                                                      layout$y[i]-0,
                                                      ")' ",
                                                      ">")
          
          svg_code[g_code_lines$g_start[1]] <- gsub(pattern = "id='..'",replacement = paste0("id='",panel_name,"'"),svg_code[g_code_lines$g_start[1]])
        } else{
          
          # svg_offset <- find_svg_offset(figure_1_layout$panel_path[i])
          
          
          svg_code[g_code_lines$g_start[1]] <- paste0(substr(svg_code[g_code_lines$g_start[1]],1,nchar(svg_code[g_code_lines$g_start[1]])-1),
                                                      " transform='translate(",
                                                      layout$x[i]-layout$x_offset[i],
                                                      ",",
                                                      layout$y[i]-layout$y_offset,
                                                      ")' ",
                                                      "id='",panel_name,"' ",
                                                      ">")
          
        }
        
        write_lines(svg_code[g_code_lines$g_start[1]:g_code_lines$g_end[1]],"Anouk_data/Figure_2.svg",append = T)
        
        
        write_lines("</svg>","Anouk_data/Figure_2.svg",append = T)
        
        
        output$plot_preview <- renderImage({

          list(src = "Anouk_data/Figure_2.svg",
               contentType = "image/svg+xml")
        })
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
      #   
      #   
      #   
      #   
      #   for(j in excel_sheets(excel_path)[-1]){
      #     temp_data <- read_excel(excel_path,sheet = j)
      # 
      # 
      # 
      #     # bars_count=length(paste0(temp_data$Sample,temp_data$Annotation) %>%
      #     #                     unique())
      #     assign(j,temp_data)
      # 
      # 
      #     if("Unit_barplot" %in% colnames(temp_data)){
      #       temp_plot <- JPL_barplot(temp_data,
      #                                hot_to_df(input$colour_key_hot),
      #                                ylab_split=input$ylab_split,
      #                                font = input$font,
      #                                dotsize = input$dotsize,
      #                                space_top = input$space_top,
      #                                var_equal = input$var_equal,
      #                                Show_ns = input$Show_ns,
      #                                legend_loc = "none")
      # 
      #       nbars <- temp_data |>
      #         select(Sample,Annotation) |>
      #         distinct() |>
      #         summarise(nbars=n()) |>
      #         as.double()
      # 
      #       plot_width <- nbars*input$width
      #       }
      #     if("Unit_lineplot" %in% colnames(temp_data)){
      #       temp_plot <- JPL_lineplot(temp_data,
      #                                 hot_to_df(input$colour_key_hot),
      #                                 ylab_split=input$ylab_split,
      #                                 font = input$font,
      #                                 dotsize = input$dotsize,
      #                                 space_top = input$space_top,
      #                                 var_equal = input$var_equal,
      #                                 Show_ns = input$Show_ns,
      #                                 legend_loc = "none")
      #       plot_width <- input$width_lineplot
      #       }
      #     if("Unit_survivalplot" %in% colnames(temp_data)){
      #       temp_plot <- JPL_survivalplot(temp_data,
      #                                     hot_to_df(input$colour_key_hot),
      #                                     ylab_split=input$ylab_split,
      #                                     font = input$font,
      #                                     dotsize = input$dotsize,
      #                                     space_top = input$space_top,
      #                                     var_equal = input$var_equal,
      #                                     Show_ns = input$Show_ns,
      #                                     legend_loc = "none")
      #       plot_width <- input$width_survivalplot
      #       }
      #     if("Unit_barplot_annotation" %in% colnames(temp_data)){
      #       temp_plot <- JPL_barplot_annotation(temp_data,
      #                                           hot_to_df(input$colour_key_hot),
      #                                           ylab_split=input$ylab_split,
      #                                           font = input$font,
      #                                           dotsize = input$dotsize,
      #                                           space_top = input$space_top,
      #                                           var_equal = input$var_equal,
      #                                           Show_ns = input$Show_ns,
      #                                           legend_loc = "none")
      # 
      #       if("Annotation_2_Symbol" %in% colnames(temp_data)){
      #         nbars <- temp_data |>
      #           select(Sample,Annotation_1_Symbol,Annotation_2_Symbol) |>
      #           distinct() |>
      #           summarise(nbars=n()) |>
      #           as.double()
      #         } else {
      #           nbars <- temp_data |>
      #           select(Sample,Annotation_1_Symbol) |>
      #           distinct() |>
      #           summarise(nbars=n()) |>
      #           as.double()
      #           }
      # 
      #       plot_width <- nbars*input$width
      #       }
      #     set_panel_size(temp_plot, file = paste0(outfile_zip,"/",j,".svg"),
      #                    # width = unit(bars_count*10, "mm"),
      #                    width = unit(plot_width, "mm"),
      #                    height = unit(input$height,"mm"))
      #   }
      #   # files within the folder output
      #   list.files(outfile_zip,full.names = TRUE)
      # 
      # 
      # 
      # 
      #   # zip a the folder 'output'
      #   zip(paste0(tempdir(),"/OUT.zip"),flags = "-j", list.files(outfile_zip,full.names = TRUE))
      # 
      #   shinyjs::enable("downloadData")
      # })
      # 
      # 
      # output$downloadData <- downloadHandler(
      #   filename = function() {
      #     paste("Data-", Sys.Date(), ".zip", sep="")
      #   },
      #   content = function(file) {
      #     file.copy(
      #       from = paste0(tempdir(),"/OUT.zip"),
      #       to = file
      #     )
        }
      )

      
      }
  )
}