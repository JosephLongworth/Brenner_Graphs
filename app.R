options(shiny.maxRequestSize=300*1024^2) 
library(extrafont)
extrafont::loadfonts()
loadfonts(device="win")
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(shinycustomloader)
library(shiny)
library(shinyjs)
library(rhandsontable)
library(tidyverse)
library(ggpubr)
library(egg)
library(svglite)
library(scales)
library(readxl)
library(writexl)
# library(data.table)
library(survminer)
library(survival)
library(latex2exp)
library(DT)
library(wellr)
library(ggplate)
library(bslib)
library(shinyWidgets)


ui = dashboardPage(
  dashboardHeader(title = "Brenner Barplots"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ELISA", tabName = "ELISA", icon = icon("dashboard")),
      menuItem("qPCR", tabName = "qPCR", icon = icon("dashboard")),
      menuItem("Barplot Annotated", tabName = "barplot_annotated", icon = icon("dashboard")),
      menuItem("Figure Builder", tabName = "figure_builder", icon = icon("home")),
      menuItem("Batch Plot", tabName = "batchplot", icon = icon("dashboard")),
      menuItem("Line Plot", tabName = "lineplot", icon = icon("dashboard")),
      menuItem("Survival Plot", tabName = "survivalplot", icon = icon("dashboard")),
      menuItem("FlowJo", tabName = "FlowJo", icon = icon("home"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "qPCR",
              navbarPage("qPCR",
                         tabPanel("Plate Design",UI_Layout("qPCR")),
                         tabPanel("Plotting",UI_qPCR_plot("qPCR_plot"))
              )
              ),
      tabItem(tabName = "ELISA",
              navbarPage("ELISA",
                         tabPanel("Plate Design",UI_Layout("ELISA")),
                         tabPanel("Plotting",UI_ELISA_plot("ELISA_plot"))
              )
      ),
      
      tabItem(tabName = "barplot_annotated",UI_barplot_annotated("barplot_annotated")),
      tabItem(tabName = "batchplot",UI_batchplot("batchplot")),
      tabItem(tabName = "lineplot",UI_lineplot("lineplot")),
      tabItem(tabName = "survivalplot",UI_survivalplot("survivalplot")),
      tabItem(tabName = "figure_builder",UI_figure_builder("figure_builder")),
      tabItem(tabName = "FlowJo",UI_FlowJo("FlowJo"))
      )
    )
  )

server = function(input, output) {
  counterServer("counter1")
  Server_Layout("qPCR")
  Server_Layout("ELISA")
  Server_qPCR_plot("qPCR_plot")
  Server_ELISA_plot("ELISA_plot")
  Server_barplot_annotated("barplot_annotated")
  Server_batchplot("batchplot")
  Server_lineplot("lineplot")
  Server_survivalplot("survivalplot")
  Server_figure_builder("figure_builder")
  Server_FlowJo("FlowJo")
}

shinyApp(ui, server)
