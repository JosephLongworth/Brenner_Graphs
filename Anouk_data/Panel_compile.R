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


find_svg_offset("Anouk_data/example_panel_data/GRAPH_GOT.svg")

row_height <- 120
row_width <-  110

figure_1_layout=tibble(panel_path=c("Anouk_data/example_panel_data/GRAPH_IFNB_LCMV.svg",
                                    "Anouk_data/example_panel_data/GRAPH_IFNA_VSV.svg",
                                    "Anouk_data/example_panel_data/GRAPH_PALS.svg",
                                    "Anouk_data/example_panel_data/GRAPH_PAK.svg",
                                    "Anouk_data/example_panel_data/GRAPH_PALi.svg",
                                    "Anouk_data/example_panel_data/GRAPH_PALu.svg",
                                    "Anouk_data/example_panel_data/GRAPH_GOT.svg",
                                    "Anouk_data/example_panel_data/GRAPH_GPT.svg",
                                    "Anouk_data/example_panel_data/GRAPH_pIC_anti-IFNAR_Stat1_1.svg",
                                    "Anouk_data/example_panel_data/GRAPH_pIC_anti-IFNAR_Stat1_2.svg"
                                    ),
                       panel_name=c("GRAPH_IFNB_LCMV","GRAPH_IFNA_VSV",
                                    "GRAPH_PALS","GRAPH_PAK","GRAPH_PALi","GRAPH_PALu",
                                    "GRAPH_GOT","GRAPH_GPT",
                                    "GRAPH_pIC_anti-IFNAR_Stat1_1","GRAPH_pIC_anti-IFNAR_Stat1_2"),
                       x_coordinate=c( 0, 100,
                                      0, 100,200,300,
                                      0,100,
                                      0,150)+50,
                       y_coordinate=c(  0,   0,
                                        150, 150, 150, 150,
                                        300, 300,
                                        450,450)+20)
svg_letters <- tibble(Letter=c("A","B",
                               "D","E","F","G",
                               "H","I",
                               "J","K"),
                      x=c(-40,60,
                          -40,60,160,260,
                          -40,60,
                          -40,110)+50,
                      y=c(-10,-10,
                          140,140,140,140,
                          290,290,
                          440,440)+20)

                       
readLines("svg_top2.svg") |> 
  write_lines("Anouk_data/Figure_1.svg")

i <- 12

for(i in 1:nrow(figure_1_layout)){
  
  # for(i in 1:1){
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
  
  } else if(grepl("id=",svg_code[g_code_lines$g_start[1]])){
  
      svg_code[g_code_lines$g_start[1]] <- paste0(substr(svg_code[g_code_lines$g_start[1]],1,nchar(svg_code[g_code_lines$g_start[1]])-1),
                                                " transform='translate(",
                                                figure_1_layout$x_coordinate[i]-0,
                                                ",",
                                                figure_1_layout$y_coordinate[i]-0,
                                                ")' ",
                                                ">")
    
      svg_code[g_code_lines$g_start[1]] <- gsub(pattern = "id='..'",replacement = paste0("id='",figure_1_layout$panel_name[i],"'"),svg_code[g_code_lines$g_start[1]])
      } else{
    
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
  
  write_lines(svg_code[g_code_lines$g_start[1]:g_code_lines$g_end[1]],"Anouk_data/Figure_1.svg",append = T)
}

svg_letters <- svg_letters %>% 
  mutate(svg_code = paste0("<text x='",x,"' y='",y,"' style='font-size: 12.00px; font-family: \"Arial\";' >",Letter,"</text>")) 

for(i in 1:nrow(svg_letters)){
  write_lines(svg_letters$svg_code[i],"Anouk_data/Figure_1.svg",append = T)
}




write_lines("</svg>","Anouk_data/Figure_1.svg",append = T)

# 
# import_inkscape <- function(file,out){
# temp <- readLines(file)
# temp <- trimws(temp)
# temp <- paste0(temp,collapse = " ")
# temp <- gsub(pattern = "\"",replacement = "'",x = temp)
# temp <- gsub(pattern = ">",replacement = ">
# ",x = temp)
# write_lines(temp,out)}
# 
# import_inkscape(file = "Figure_1_panels/Panel_Ai.svg",out = "Figure_1_panels/Panel_Ai.svg")
# import_inkscape(file = "Figure_1_panels/Panel_Ci.svg",out = "Figure_1_panels/Panel_Ci.svg")
# import_inkscape(file = "Figure_1_panels/Panel_Di.svg",out = "Figure_1_panels/Panel_Di.svg")
# import_inkscape(file = "Figure_1_panels/Panel_Fi.svg",out = "Figure_1_panels/Panel_Fi.svg")
# import_inkscape(file = "Figure_1_panels/Panel_Ji.svg",out = "Figure_1_panels/Panel_Ji.svg")
# import_inkscape(file = "Figure_1_panels/Panel_K.svg",out = "Figure_1_panels/Panel_K.svg")
