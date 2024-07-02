UI_survivalplot <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title = "Data", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6, collapsed = FALSE,
          rHandsontableOutput(ns("hot"))
      ),
      box(title = "Colour Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6, collapsed = FALSE,
          rHandsontableOutput(ns("colour_key_hot"))
      )
    ),
    fluidRow(
    box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
        numericInput(ns("ylab_split"), "Paper ylab split", 20),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("width"), "Plot width mm", 20),
          numericInput(ns("height"), "Plot height mm", 30)),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("font"), "Plot font size", 7),
          numericInput(ns("dotsize"), "Plot dotsize", 1)),
        numericInput(ns("space_top"), "Plot space top", 1.1)
    ),
    box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE,
        downloadButton(ns("downloadPaper"), "Paper SVG"),
        downloadButton(ns("downloadPaperpng"), "Paper PNG"),
        plotOutput(ns("plot"))
    )
    )
    )

}

Server_survivalplot <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      # df=tibble::tibble(
      #   Sample = c(rep("C57BL/6",3),rep("C57BL/6 + C.rodentium",4)),
      #   Value = 	c(5673.5,	5730,	5611.5,	6433.5,	6325.5,	6569.5,	6607.5),
      #   Unit = rep("Intracellular thiols (MFI of mBBr)",7),
      #   Annotation = c(rep("",3),rep("Day 7 p.i.",4)))
      
      
      colour_key=tibble::tibble(
        Sample=c("C57BL/6","C57BL/6 + C.rodentium","Gclc fl/fl","Cd4Cre Gclc fl/fl","Man2afl/fl","Man2afl/flCD4cre+"),
        fill=c("#d4d4d4ff","#000000ff","#000000ff","#ff0000ff","#3ba99aff","#c93963ff"))
      
      df <- read_csv("Data/Survival_plot.csv")
      
      output$hot = renderRHandsontable({
        rhandsontable(df)
      })
      output$colour_key_hot = renderRHandsontable({
        rhandsontable(colour_key)
      })
      
      
      
      outfile <- tempfile(fileext='.svg')
      outfile_png <- tempfile(fileext='.png')
      
      
      output$plot <- renderImage({
        req(input$hot)
        print("render plot1")
        
        
        # Create an empty ggplot object
        empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
          theme_void()
        plot <- Survivalplot(hot_to_df(input$hot))
          # theme(rect = element_rect(fill = "transparent"))
        set_panel_size(plot, file = outfile ,
                       width = unit(input$width, "mm"),
                       height = unit(input$height,"mm"))
        set_panel_size(plot, file = outfile_png,
                       width = unit(input$width, "mm"),
                       height = unit(input$height,"mm"))
        
        # local$svg_file <- outfile
        # local$png_file <- outfile_png
        
        
        # Return a list
        list(src = outfile,
             alt = "This is alternate text")
      }, deleteFile = F)
      
      
      
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