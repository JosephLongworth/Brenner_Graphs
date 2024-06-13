library(shinydashboard)
library(shiny)
# library(data.table)
library(rhandsontable)
library(tidyverse)
# library(survminer)
# library(survival)
library(ggpubr)
library(egg)
library(svglite)
library(scales)
# library('latex2exp')

ui = dashboardPage(
  dashboardHeader(title = "Brenner Barplots"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "Barplot", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Barplot",
              rHandsontableOutput("hot"),
              rHandsontableOutput("colour_key_hot"),
              plotOutput("paper_plot"),
              numericInput("paper_ylab_split", "Paper ylab split", 20),
              numericInput("paper_width", "Paper width", 20),
              numericInput("paper_height", "Paper height", 30),
              numericInput("paper_font", "Paper font", 7),
              numericInput("paper_dotsize", "Paper dotsize", 1),
              downloadButton("downloadPaper", "Paper SVG"),
              downloadButton("downloadPaperpng", "Paper PNG"),
              plotOutput("poster_plot"),
              numericInput("poster_ylab_split", "Poster ylab split", 200),
              numericInput("poster_width", "Poster width", 200),
              numericInput("poster_height", "Poster height", 300),
              numericInput("poster_font", "Poster font", 16),
              numericInput("poster_dotsize", "Poster dotsize", 5),
              downloadButton("downloadPoster", "Poster SVG"),
              downloadButton("downloadPosterpng", "Poster PNG")
              
      )
    )
  )
)

server = function(input, output) {
  
 
  df=tibble::tibble(
    Sample = c(rep("C57BL/6",3),rep("C57BL/6 + C.rodentium",4)),
    Value = 	c(5673.5,	5730,	5611.5,	6433.5,	6325.5,	6569.5,	6607.5),
    Unit = rep("Intracellular thiols (MFI of mBBr)",7),
    Annotation = c(rep("",3),rep("Day 7 p.i.",4)))
  
  
  colour_key=tibble::tibble(
    Sample=c("C57BL/6","C57BL/6 + C.rodentium","Gclc fl/fl","Cd4Cre Gclc fl/fl"),
    fill=c("#d4d4d4ff","#000000ff","#000000ff","#ff0000ff"))
  


   output$hot = renderRHandsontable({
    rhandsontable(df)
  })
   output$colour_key_hot = renderRHandsontable({
     rhandsontable(colour_key)
   })
  
  output$paper_plot <- renderImage({
    req(input$hot)
    print("render plot1")
   
    outfile <- tempfile(fileext='.svg')
    outfile_png <- tempfile(fileext='.png')
    
    # Create an empty ggplot object
    empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
      theme_void()
    plot <- barplot2(hot_to_df(input$hot),
                     hot_to_df(input$colour_key_hot),
                     ylab_split=input$paper_ylab_split,
                     font = input$paper_font,
                     dotsize = input$paper_dotsize,
                     legend_loc = "none")+
    # plot <- barplot2(df,colour_key,legend_loc = "none",Auto_Split_ylab = T,font = 16,dotsize = 5)+
      theme(rect = element_rect(fill = "transparent"))
    set_panel_size(plot, file = outfile ,
                   width = unit(input$paper_width, "mm"),
                   height = unit(input$paper_height,"mm"))
    set_panel_size(plot, file = outfile_png,
                   width = unit(input$paper_width, "mm"),
                   height = unit(input$paper_height,"mm"))
    
    local$paper_size_svg_file <- outfile
    local$paper_size_png_file <- outfile_png

    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = F)
  
   
   local <- reactiveValues(data = mtcars,
                           export_file = NULL,
                           poster_size_svg_file=NULL,
                           paper_size_svg_file=NULL
   )
 
   observe({
   
     req(input$hot)
     print("render plot1")
     
     outfile <- tempfile(fileext='.svg')
     outfile_png <- tempfile(fileext='.png')
     
     # Create an empty ggplot object
     empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
       theme_void()
     plot <- barplot2(hot_to_df(input$hot),
                      hot_to_df(input$colour_key_hot),
                      ylab_split=input$poster_ylab_split,
                      font = input$poster_font,
                      dotsize = input$poster_dotsize,
                      legend_loc = "none")+
       # plot <- barplot2(df,colour_key,legend_loc = "none")+
       theme(rect = element_rect(fill = "transparent"))
     set_panel_size(plot, file = outfile ,
                    width = unit(input$poster_width, "mm"),
                    height = unit(input$poster_height,"mm"))
     set_panel_size(plot, file = outfile_png ,
                    width = unit(input$poster_width, "mm"),
                    height = unit(input$poster_height,"mm"))
       
     local$poster_size_svg_file <- outfile
     local$poster_size_png_file <- outfile_png
     
     
   output$poster_plot <- renderImage({
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = F)

   
   })
  
     
     output$downloadPoster <- downloadHandler(
       filename = function() {
         paste("PosterSize-", Sys.Date(), ".svg", sep="")
       },
       content = function(file) {
         file.copy(
           from = local$poster_size_svg_file,
           to = file
         )
       }
     )
     
     output$downloadPaper <- downloadHandler(
       filename = function() {
         paste("PaperSize-", Sys.Date(), ".svg", sep="")
       },
       content = function(file) {
         file.copy(
           from = local$paper_size_svg_file,
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
           from = local$paper_size_png_file,
           to = file
         )
       }
     )
     output$downloadPosterpng <- downloadHandler(
       filename = function() {
         paste("PosterSize-", Sys.Date(), ".png", sep="")
       },
       content = function(file) {
         file.copy(
           from = local$poster_size_png_file,
           to = file
         )
       }
     )
   
  
}

shinyApp(ui, server)
