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

colour_key=tibble::tibble(
  Sample=c("Man2afl/fl","Man2afl/flCD4cre+"),
  fill=c("#3ba99aff","#c93963ff"))

source("R/Plot_functions.R")


df <- readxl::read_excel("Sarah_data/example_data/SA17_surface_2.0.wsp FlowJo table.xls") |> 
  janitor::clean_names() |> 
  rename(file=x1,
         ferq_parent =lymphocytes_single_cells_cd8_fsc_h_subset_cd8_ld_subset_freq_of_parent) |>
  pivot_longer(cols = contains("cd"),names_to = "marker",values_to = "Value") |> 
  separate(marker,sep = "mean_",c(NA,"marker")) |> 
  mutate(Sample = as_factor(file)) |> 
  separate(file,sep = "_",into = c("x1","x2"),extra = "drop",remove = F) |> 
  mutate(Annotation = fct_recode(x1,
                             "na" = "na",
                             "αCD3/28" = "cd3,2f,28",
                             "Sw" = "sw")) |>
  filter(Annotation %in% c("αCD3/28","Sw")) |>
  mutate(Sample = str_sub(x2,2,2)) |> 
  filter(Sample %in% c(1:8)) |> 
  mutate(Sample = if_else(Sample %in% c(1:4),"Man2afl/fl","Man2afl/flCD4cre+")) |>
  mutate(marker = toupper(marker)) |> 
  # filter(Sample %in% c("Man2afl/fl","Man2afl/flCD4cre+")) |>
  # filter(marker == "CD69") |>
  mutate(Unit = paste0(marker," (MFI)")) |> 
  filter(!Annotation %in% c("Mean","SD")) |> 
  select(Sample,Value,Unit,Annotation) |> 
  glimpse()

df2 <- readxl::read_excel("Sarah_data/example_data/mtor_SA17.wsp FlowJo table.xls") |> 
  janitor::clean_names() |> 
  rename(file=x1,
         ferq_parent =lymphocytes_single_cells_cd8_fsc_h_subset_freq_of_parent,
         ferq_parent2 =lymphocytes_single_cells_cd8_fsc_h_subset_cd8_ld_subset_freq_of_parent) |>
  pivot_longer(cols = contains("cd"),names_to = "marker",values_to = "Value") |> 
  separate(marker,sep = "mean_",c(NA,"marker")) |> 
  mutate(Sample = as_factor(file)) |> 
  separate(file,sep = "_",into = c("x1","x2"),extra = "drop",remove = F) |>
  mutate(Annotation = fct_recode(x1,
                                 "na" = "NA",
                                 "αCD3/28" = "CD3,2f,28",
                                 "Sw" = "SW")) |>
  filter(Annotation %in% c("αCD3/28","Sw")) |>
  mutate(Sample = str_sub(x2,2,2)) |> 
  filter(Sample %in% c(1:8)) |> 
  mutate(Sample = if_else(Sample %in% c(1:4),"Man2afl/fl","Man2afl/flCD4cre+")) |>
  # mutate(marker = toupper(marker)) |> 
  mutate(marker = fct_recode(marker,
                                 "p-S6" = "ps6",
                                 "p-mTOR" = "pmtor",
                                 "CD98" = "cd98")) |>
  # filter(Sample %in% c("Man2afl/fl","Man2afl/flCD4cre+")) |>
  # filter(marker == "CD69") |>
  mutate(Unit = paste0(marker," (MFI)")) |> 
  filter(!Annotation %in% c("Mean","SD")) |> 
  select(Sample,Value,Unit,Annotation) |>
  glimpse()

df2$marker |> unique()

# Treatment = fct_relevel(Treatment, "0h", "3h", "9h")

barplot2(df2,colour_key,
                 ylab_split=50,
                 font = 12,
                 dotsize = 12,
                 legend_loc = "none")+
  # facet_wrap(~marker,scales = "free_y") +
# plot <- barplot2(df,colour_key,legend_loc = "none",Auto_Split_ylab = T,font = 16,dotsize = 5)+
  theme(rect = element_rect(fill = "transparent")) 



Fig.1A <- df |> 
  filter(Unit == "CD25 (MFI)")
Fig.1B <- df |> 
  filter(Unit == "CD69 (MFI)")
Fig.1C <- df |> 
  filter(Unit == "CD44 (MFI)")

Fig.2A <- df2 |> 
  filter(Unit == "p-S6 (MFI)")
Fig.2B <- df2 |> 
  filter(Unit == "p-mTOR (MFI)")
Fig.2C <- df2 |> 
  filter(Unit == "CD98 (MFI)")


dataset_names <- list(
  "Fig.1A" = Fig.1A,
  "Fig.1B" = Fig.1B,
  "Fig.1C" = Fig.1C,
  "Fig.2A" = Fig.2A,
  "Fig.2B" = Fig.2B,
  "Fig.2C" = Fig.2C)

openxlsx::write.xlsx(dataset_names, file = 'Sarah_data/Data_S1.xlsx')

