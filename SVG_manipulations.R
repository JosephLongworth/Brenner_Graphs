require(tidyverse)

svg_offset <- function(svg_file){

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


svg_offset("SVGS/text combine/PaperSize-2024-06-13 (6).svg")


svg_code=readLines("SVGS/text combine/PaperSize-2024-06-13 (6).svg")

lines <- tibble::tibble(g_start = grep("<g",svg_code,value = F),
                        g_end = grep("</g",svg_code,value = F)) |>
  glimpse()

, index, function)  