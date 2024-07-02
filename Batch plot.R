library(readxl)
library(tidyverse)
library(janitor)
library(egg)
library(ggpubr)
source("R/Plot_functions.R")



colour_key=tibble::tibble(
  Sample=c("C57BL/6","C57BL/6 + C.rodentium","Gclc fl/fl","Cd4Cre Gclc fl/fl","Man2afl/fl","Man2afl/flCD4cre+"),
  fill=c("#d4d4d4ff","#000000ff","#000000ff","#ff0000ff","#3ba99aff","#c93963ff"))

# Path to the excel file
excel_path <- "Sarah_data/Data_S1.xlsx"

# read the excel file

for(j in excel_sheets(excel_path)){
  temp_data <- read_excel(excel_path,sheet = j)
    bars_count=length(paste0(temp_data$Sample,temp_data$Annotation) %>% 
    unique())
    assign(j,temp_data)
  temp_plot <- barplot2(temp_data, colour_key,
                        legend_loc = "none")+
  theme(rect = element_rect(fill = "transparent"))
  set_panel_size(temp_plot, file = paste0("ouput/",j,".svg"),
                 width = unit(bars_count*10, "mm"),
                 height = unit(40,"mm"))
    }
# files within the folder output
list.files("output/",full.names = TRUE)

# zip a the folder 'output'
zip("output.zip",list.files("output/",full.names = TRUE))


