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


figure_1_layout=tibble(panel_path=c("SVGS/text_combine/Fig.1A.svg",
                                    "SVGS/text_combine/Fig.1B.svg",
                                    "SVGS/text_combine/Fig.1C.svg",
                                    "SVGS/text_combine/Fig.1D.svg",
                                    "SVGS/text_combine/Fig.1E.svg",
                                    "SVGS/text_combine/Fig.1F.svg",
                                    "SVGS/text_combine/Fig.1I.ii.svg"),
                       panel_name=c("panel_a","panel_b","panel_c","panel_d","panel_e","panel_f","panel_i.i"),
                       x_coordinate=c(0,110,220,0,110,220,110)+50,
                       y_coordinate=c(0,0,0,110,110,110,220)+20)
                       
readLines("SVGS/text_combine/svg_top.svg") |> 
  write_lines("SVGS/text_combine/Figure_1.svg")

for(i in 1:nrow(figure_1_layout)){
  svg_offset <- find_svg_offset(figure_1_layout$panel_path[i])
  svg_code <- readLines(figure_1_layout$panel_path[i])
  
  g_code_lines <- tibble::tibble(g_start = grep("<g",svg_code,value = F),
                                  g_end = grep("</g",svg_code,value = F)) |>
    arrange(desc(g_start)) |>
    glimpse()
  
  svg_code[g_code_lines$g_start[1]]
  svg_code[g_code_lines$g_start[1]] <- paste0(substr(svg_code[g_code_lines$g_start[1]],1,nchar(svg_code[g_code_lines$g_start[1]])-1),
                                              " transform='translate(",
                                              figure_1_layout$x_coordinate[i]-svg_offset$x_offset,
                                              ",",
                                              figure_1_layout$y_coordinate[i]-svg_offset$y_offset,
                                              ")' ",
                                              "id='",figure_1_layout$panel_name[i],"' ",
                                              ">")
  
  write_lines(svg_code[g_code_lines$g_start[1]:g_code_lines$g_end[1]],"SVGS/text_combine/Figure_1.svg",append = T)
}

svg_letters <- tibble(Letter=c("A","B","C","D","E","F","I.ii"),
                      x=c(0,110,220,0,110,220,110),
                      y=c(10,10,10,120,120,120,230)) %>% 
  mutate(svg_code = paste0("<text x='",x,"' y='",y,"' style='font-size: 12.00px; font-family: \"Arial\";' >",Letter,"</text>")) |>
  pull(svg_code)

svg_letters <- "<text x='0' y='10' style='font-size: 12.00px; font-family: \"Arial\";' >A</text>"
write_lines(svg_letters,"SVGS/text_combine/Figure_1.svg",append = T)

svg_letters <- "<text x='110' y='10' style='font-size: 12.00px; font-family: \"Arial\";' >B</text>"
write_lines(svg_letters,"SVGS/text_combine/Figure_1.svg",append = T)
  
write_lines("</svg>","SVGS/text_combine/Figure_1.svg",append = T)

