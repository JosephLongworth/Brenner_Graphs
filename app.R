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
              downloadButton("downloadData_paper_plot", "Paper SVG"),
              plotOutput("presentation_plot"),
              downloadButton("downloadData_presentation_plot", "Presentation SVG")
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
  
  # output$hot2 = renderPlot({
  #   
  #   print("render hot2" )
  #   
  #       barplot2(hot_to_df(input$hot),
  #                hot_to_df(input$colour_key_hot),
  #            font = 12,
  #            dotsize = 5)
  #   })
  # |> 
  #   bindCache(input$hot) |> 
  #   bindEvent(input$hot,input$colour_key_hot)
  
  output$paper_plot <- renderImage({
    req(input$hot)
    print("render plot1")
   
    outfile <- tempfile(fileext='.svg')
    
    # Create an empty ggplot object
    empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
      theme_void()
    plot <- barplot2(hot_to_df(input$hot),ylab_split=20,
                     hot_to_df(input$colour_key_hot),
                     legend_loc = "none")+
    # plot <- barplot2(df,colour_key,legend_loc = "none",Auto_Split_ylab = T,font = 16,dotsize = 5)+
      theme(rect = element_rect(fill = "transparent"))
    set_panel_size(plot, file = outfile ,width = unit(2, "cm"), height = unit(3,"cm"))

    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = F)
  
 
   observe({
   
     req(input$hot)
     print("render plot1")
     
     outfile <- tempfile(fileext='.svg')
     # Create an empty ggplot object
     empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
       theme_void()
     plot <- barplot2(hot_to_df(input$hot),font = 16,dotsize = 5,
                      hot_to_df(input$colour_key_hot),
                      legend_loc = "none")+
       # plot <- barplot2(df,colour_key,legend_loc = "none")+
       theme(rect = element_rect(fill = "transparent"))
     set_panel_size(plot, file = outfile ,width = unit(20, "cm"), height = unit(10,"cm"))
     
     
     
   output$presentation_plot <- renderImage({
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = F)

   
   output$downloadData_presentation_plot <- downloadHandler(
     filename = function() {
       file_name <- paste0("test",".svg")
       file_name
     },
     content = function(file) {
       print(outfile)
       file.copy(paste0("outfile"), file)
     }
   )
   
   
  # 
  # # Downloadable csv of selected dataset ----
  # output$downloadData_presentation_plot <- downloadHandler(
  #   filename = function() {
  #     "test.svg"
  #   },
  #   content = function(file) {
  #     outfile
  #     }
  # )
   })
  
   
   # ui <- fluidPage(
   #   downloadButton(outputId = "downloadData",label =  "Download")
   # )
   # server <- function(input, output) {
   #   local <- reactiveValues(data = mtcars,
   #                           export_file = NULL
   #   )
   #   observeEvent(local$data,{
   #     out <- tempfile(fileext = ".csv")
   #     write.csv(x = local$data,file = out)
   #     local$export_file <- out
   #   })
   #   output$downloadData <- downloadHandler(
   #     filename = function() {
   #       paste("data-", Sys.Date(), ".csv", sep="")
   #     },
   #     content = function(file) {
   #       file.copy(
   #         from = local$export_file,
   #         to = file
   #       )
   #     }
   #   )
   # }
   # shinyApp(ui, server)
   
  
  
}

shinyApp(ui, server)
