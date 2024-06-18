require(tidyverse)

find_svg_offset <- function(svg_file){

svg_code=readLines(svg_file)

# find the coordinates of polylines and determine their lengths 
lines <- tibble::tibble(polylines = grep("<polyline points",svg_code,value = T)) |>
  separate(polylines,sep = "'",into = c(NA,"coordinates"),extra = "drop") |> 
  separate(coordinates,sep = " ",into = c("a","b")) |> 
  separate(a,sep = ",",into = c("x_1","y_1")) |> 
  separate(b,sep = ",",into = c("x_2","y_2")) |> 
  mutate_all(as.numeric) |> 
  rowwise() |> 
  mutate(x_start = min(x_1, x_2),
         x_end = max(x_1, x_2),
         y_start = min(y_1, y_2),
         y_end = max(y_1, y_2),.keep = c("unused")) |> 
  mutate(x_length = x_end - x_start,
         y_length = y_end - y_start)

# Assuming the longest polyline is the x and y axis and their cross over is the desired origin to
# work from when merging svgs determine the offsetof that 

offset <- list(
  x_offset = lines$x_start[which.max(lines$x_length)],
  y_offset = lines$y_start[which.max(lines$y_length)])
return(offset)
}


find_svg_offset("SVGS/text_combine/PaperSize-2024-06-13 (6).svg")

row_height <- 120
row_width <-  110

figure_1_layout=tibble(panel_path=c("Figure_1_panels/Panel_Aii.svg",
                                    "Figure_1_panels/Panel_B.svg",
                                    "Figure_1_panels/Panel_Cii.svg",
                                    "Figure_1_panels/Panel_Dii.svg",
                                    "Figure_1_panels/Panel_E.svg",
                                    "Figure_1_panels/Panel_Fii.svg",
                                    "Figure_1_panels/Panel_Iii.svg",
                                    "Figure_1_panels/Panel_F_clean.svg",
                                    "Figure_1_panels/Panel_Ji_clean.svg",
                                    "Figure_1_panels/Panel_K_clean.svg"
                                    ),
                       panel_name=c("panel_a","panel_b","panel_c","panel_d","panel_e","panel_f","panel_i.i","Panel_Fi","Panel_Ji","Panel_K"),
                       x_coordinate=c(0,1*row_width,3*row_width,0,1*row_width,3*row_width,1*row_width,2*row_width,3*row_width,0)+50,
                       y_coordinate=c(0,0,0,1*row_height,1*row_height,1*row_height,2*row_height,1*row_height,3*row_height,4*row_height)+50)
                       
readLines("SVGS/text_combine/svg_top.svg") |> 
  write_lines("Figure_1_panels/Figure_1.svg")

i <- 8

for(i in 1:nrow(figure_1_layout)){
  
  # for(i in 1:7){
  print(i)
  
  svg_code <- readLines(figure_1_layout$panel_path[i])
  g_code_lines <- tibble::tibble(g_start = grep("<g",svg_code,value = F),
                                 g_end = grep("</g",svg_code,value = F)) |>
    arrange(desc(g_start)) |>
    glimpse()
  
  svg_code[g_code_lines$g_start[1]]  
  
  #offset calculations should be moved out of the for loop
  # svg_offset=NULL
  # svg_offset$x_offset <- 0
  # svg_offset$y_offset <- 0
  # 
  
  svg_code[g_code_lines$g_start[1]]

  if(grepl("transform=",svg_code[g_code_lines$g_start[1]])){
  
    
  matrix <- str_match(pattern = "\\(.+\\)",svg_code[g_code_lines$g_start[1]])
  matrix <- substr(matrix[1],2,nchar(matrix[1])-1) 
  matrix <- strsplit(matrix,",")[[1]]
  matrix <- as.numeric(matrix)
  matrix[5]=matrix[5]+figure_1_layout$x_coordinate[i]-0
  matrix[6]=matrix[6]+figure_1_layout$y_coordinate[i]-0
  matrix <- paste0("(",paste0(matrix,collapse = ","),")")
  
  
  svg_code[g_code_lines$g_start[1]] <- gsub(pattern = "\\(.+\\)",matrix,svg_code[g_code_lines$g_start[1]])
  
  } else {
    
    svg_offset <- find_svg_offset(figure_1_layout$panel_path[i])
    
    
  svg_code[g_code_lines$g_start[1]] <- paste0(substr(svg_code[g_code_lines$g_start[1]],1,nchar(svg_code[g_code_lines$g_start[1]])-1),
                                              " transform='translate(",
                                              figure_1_layout$x_coordinate[i]-svg_offset$x_offset,
                                              ",",
                                              figure_1_layout$y_coordinate[i]-svg_offset$y_offset,
                                              ")' ",
                                              "id='",figure_1_layout$panel_name[i],"' ",
                                              ">")
  
  }
  
  write_lines(svg_code[g_code_lines$g_start[1]:g_code_lines$g_end[1]],"Figure_1_panels/Figure_1.svg",append = T)
}

svg_letters <- tibble(Letter=c("A","B","C","D","E","F","I.ii"),
                      x=c(0,1*row_width,2*row_width,0,1*row_width,2*row_width,1*row_width)+0,
                      y=c(0,0,0,1*row_height,1*row_height,1*row_height,2*row_height)+50) %>%
  mutate(svg_code = paste0("<text x='",x,"' y='",y,"' style='font-size: 12.00px; font-family: \"Arial\";' >",Letter,"</text>")) 

for(i in 1:nrow(svg_letters)){
  write_lines(svg_letters$svg_code[i],"Figure_1_panels/Figure_1.svg",append = T)
}




write_lines("</svg>","Figure_1_panels/Figure_1.svg",append = T)


import_inkscape <- function(file,out){
temp <- readLines(file)
temp <- trimws(temp)
temp <- paste0(temp,collapse = " ")
temp <- gsub(pattern = "\"",replacement = "'",x = temp)
temp <- gsub(pattern = ">",replacement = ">
",x = temp)
write_lines(temp,out)}

import_inkscape(file = "Figure_1_panels/Panel_F.svg",out = "Figure_1_panels/Panel_F_clean.svg")
import_inkscape(file = "Figure_1_panels/Panel_Ji.svg",out = "Figure_1_panels/Panel_Ji_clean.svg")
import_inkscape(file = "Figure_1_panels/Panel_K.svg",out = "Figure_1_panels/Panel_K_clean.svg")
