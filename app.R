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

ui = dashboardPage(
  dashboardHeader(title = "Brenner Barplots"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              fluidRow(box(rHandsontableOutput("hot", height = 400)),
                       box(plotOutput("hot2"))),
              fluidRow(box(plotOutput("hot2")))
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
  
 hot_to_df = function(hot) {
    hot %>%
      hot_to_r() %>%
      as.data.frame() %>%
      as_tibble()
 }
 
 
  output$hot = renderRHandsontable({
    rhandsontable(df)
  })
  
  output$hot2 = renderPlot(barplot2(hot_to_df(input$hot),font = 12,dotsize = 5))
  
  source("R/Plot_functions.R")

  # Create an empty ggplot object
  empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
    theme_void()
  # plot <- barplot2(barplot2(hot_to_df(input$hot)))+
  plot <- barplot2(df,font = 12,dotsize = 5,legend_loc = "none",scale = T)+
    theme(rect = element_rect(fill = "transparent"))
  plot
  set_panel_size(plot, file = "Figure2/p2H.svg",width = unit(2, "cm"), height = unit(3,"cm"))
  
  
  # output$hot3 = renderRHandsontable({
  #   rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))),
  #                 stretchH = "all")
  # })
}

shinyApp(ui, server)