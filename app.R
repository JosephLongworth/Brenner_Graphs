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
# library(data.table)
library(survminer)
library(survival)
library(latex2exp)

ui = dashboardPage(
  dashboardHeader(title = "Brenner Barplots"),
  dashboardSidebar(
    sidebarMenu(
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
      # tabItem(tabName = "Counter",counterButton("counter1", "Counter #1")),
      tabItem(tabName = "barplot_annotated",UI_barplot_annotated("barplot_annotated")),
      tabItem(tabName = "batchplot",UI_batchplot("batchplot")),
      # tabItem(tabName = "barplot",UI_barplot("barplot")),
      tabItem(tabName = "lineplot",UI_lineplot("lineplot")),
      tabItem(tabName = "survivalplot",UI_survivalplot("survivalplot")),
      tabItem(tabName = "figure_builder",UI_figure_builder("figure_builder")),
      tabItem(tabName = "FlowJo",UI_FlowJo("FlowJo"))
      )
    )
  )

server = function(input, output) {
  counterServer("counter1")
  Server_barplot_annotated("barplot_annotated")
  Server_batchplot("batchplot")
  # Server_barplot("barplot")
  Server_lineplot("lineplot")
  Server_survivalplot("survivalplot")
  Server_figure_builder("figure_builder")
  Server_FlowJo("FlowJo")
}

shinyApp(ui, server)
