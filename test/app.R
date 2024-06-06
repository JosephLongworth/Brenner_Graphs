library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)

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
                       box(rHandsontableOutput("hot2", width = 200))),
              fluidRow(box(rHandsontableOutput("hot3")))
      )
    )
  )
)

server = function(input, output) {
  
  df=tibble::tibble(
    Sample = c("C57BL/6","C57BL/6","C57BL/6","C57BL/6 + C.rodentium","C57BL/6 + C.rodentium","C57BL/6 + C.rodentium","C57BL/6 + C.rodentium"),
    Value = 	c(5673.5,	5730,	5611.5,	6433.5,	6325.5,	6569.5,	6607.5),
    Unit = c("intracellular thiols (MFI of mBBr)","intracellular thiols (MFI of mBBr)",
    "intracellular thiols (MFI of mBBr)","intracellular thiols (MFI of mBBr)",
    "intracellular thiols (MFI of mBBr)","intracellular thiols (MFI of mBBr)",
    "intracellular thiols (MFI of mBBr)"),
    Annotation = c(NA,NA,NA,"Day 7 p.i.","Day 7 p.i.","Day 7 p.i.","Day 7 p.i."))
  
  
  
  
  output$hot = renderRHandsontable({
    rhandsontable(df)
  })
  
  output$hot2 = renderRHandsontable({
    rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))))
  })
  
  output$hot3 = renderRHandsontable({
    rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))),
                  stretchH = "all")
  })
}

shinyApp(ui, server)