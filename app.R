options(shiny.maxRequestSize=300*1024^2) 
library(extrafont)
extrafont::loadfonts()
loadfonts(device="win")
library(drc)
library(shinydashboard)
library(shinycustomloader)
library(shiny)
library(shinyjs)
library(rhandsontable)
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
library(tidyverse)
library(openxlsx2)
library(janitor)
library(excelR)
library(scales)
# browser()
# Define paths
target_file <- "Data_Gitignore/Colour_Key.csv"
source_file <- "Data/example_colour_key.csv"
target_dir  <- dirname(target_file)

# Check if target file exists
if (!file.exists(target_file)) {
  # Create directory if needed
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  # Copy the source file
  file.copy(from = source_file, to = target_file, overwrite = FALSE)
  
  message("Copied example_colour_key.csv to ", target_file)
} else {
  message("File already exists: ", target_file)
}




ui = dashboardPage(
  dashboardHeader(title = "Brenner Barplots"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barplot Annotated2", tabName = "barplot_annotated2", icon = icon("dashboard")),
      # menuItem("Barplot Annotated", tabName = "barplot_annotated", icon = icon("dashboard")),
      menuItem("ELISA", tabName = "ELISA", icon = icon("dashboard")),
      menuItem("qPCR", tabName = "qPCR", icon = icon("dashboard")),
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
      tabItem(tabName = "barplot_annotated2",UI_barplot_annotated2("barplot_annotated2")),
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
  Server_barplot_annotated2("barplot_annotated2")
  Server_batchplot("batchplot")
  Server_lineplot("lineplot")
  Server_survivalplot("survivalplot")
  Server_figure_builder("figure_builder")
  Server_FlowJo("FlowJo")
}

shinyApp(ui, server)
