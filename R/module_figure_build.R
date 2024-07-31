UI_figure_builder <- function(id) {
  ns <- NS(id)
  fluidPage(
     box(title = "Input", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 4, collapsed = FALSE,
         fileInput(ns("files"), "Upload SVG File", multiple = TRUE,
                accept = c(".svg")),
      tags$hr(),
      shinyjs::useShinyjs(),
      downloadButton(ns("downloadPlot"), "Download"),
      tags$hr(),
      
      rHandsontableOutput(ns("Layout_hot")),
      actionButton(ns("predict_Offsets"), "Predict Offestes"),
      tags$hr(),
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


      Labels <- tibble("Label"=c("A"),"X"=c(11),"Y"=c(11))
      output$Labels_hot = renderRHandsontable({
        rhandsontable(Labels)
      })
      
      observeEvent(input$files, {
        Layout <- tibble("Panel Name"=rep("                                       ",10),
                         "X"=rep(0,10),
                         "Y"=rep(0,10),
                         "X Offset"=rep(0,10),
                         "Y Offset"=rep(0,10))
        output$Layout_hot = renderRHandsontable({
          rhandsontable(Layout) %>%  
            hot_col(col = "Panel Name", type = c("autocomplete"), source = input$files$name)
            })
        })
      

      observeEvent(input$predict_Offsets, {

        Layout <- hot_to_df(input$Layout_hot) |>
          filter(`Panel Name` %in% input$files$name) |>
          rowwise() |>
          mutate(panel_id = grep(pattern = `Panel Name`,x = input$files$name),
                 path = input$files$datapath[panel_id],
                 `X Offset` = as.numeric(find_svg_offset(path)[1]),
                 `Y Offset` = as.numeric(find_svg_offset(path)[2])) |>
          select (-panel_id,-path) |>
          glimpse()
        
        output$Layout_hot = renderRHandsontable({
          rhandsontable(Layout) %>%
            hot_col(col = "Panel Name", type = c("autocomplete"), source = input$files$name)
        })
      })

      outfile <- tempfile(fileext='.svg')
      # outfile <- "Anouk_data/Figure_2.svg"
      
      
      observeEvent(input$Run_Plots, {
        req(input$files)
        shinyjs::disable("downloadPlot")

        excel_path <- input$files$datapath
        readLines("Data/Standard_SVG_Head.svg") |> 
        write_lines(outfile)
        i <- 1

        layout <- hot_to_df(input$Layout_hot) |> 
          filter(`Panel Name` %in% input$files$name) |>
          janitor::clean_names()
        
        for (i in 1:nrow(layout)){
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
          svg_code[g_code_lines$g_start[1]] <- paste0(substr(svg_code[g_code_lines$g_start[1]],1,nchar(svg_code[g_code_lines$g_start[1]])-1),
                                                      " transform='translate(",
                                                      layout$x[i]-layout$x_offset[i],
                                                      ",",
                                                      layout$y[i]-layout$y_offset[i],
                                                      ")' ",
                                                      "id='",panel_name,"' ",
                                                      ">")
        }
        write_lines(svg_code[g_code_lines$g_start[1]:g_code_lines$g_end[1]],outfile,append = T)
        
        }
        lables <- hot_to_df(input$Labels_hot) |> 
          mutate(svg_code = paste0("<text x='",X,"' y='",Y,"' style='font-size: 12.00px; font-family: \"Arial\";' >",Label,"</text>"))
        
        for(i in 1:nrow(lables)){
          write_lines(lables$svg_code[i],outfile,append = T)
        }
        
        write_lines("</svg>",outfile,append = T)
        
        
        output$plot_preview <- renderImage(deleteFile=F,{
          list(src = outfile,
               contentType = "image/svg+xml")
        })
        
        shinyjs::enable("downloadPlot")

        }
      )
      
      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste("Figure-", Sys.Date(), ".svg", sep="")
        },
        content = function(file) {
          file.copy(
            from = outfile,
            to = file
          )
        }
      )

      
      }
  )
}