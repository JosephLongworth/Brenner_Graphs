UI_FlowJo <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
              box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
                  radioButtons(inputId = ns("defaults"),label = NULL, choices = c("Paper", "Presentation"),
                               selected = "Paper",inline = T),
                  selectizeInput(ns("Compare"), "Compare", choices = c("Sample","Annotation"), selected = "Sample"),
                  textInput(ns("stat_ref"), "Stat ref", "all"),
                  numericInput(ns("ylab_split"), "Paper ylab split", 50),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("width"), "Plot width mm (per bar)", 150),
                    numericInput(ns("height"), "Plot height mm", 100)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("font"), "Plot font size", 7),
                    numericInput(ns("dotsize"), "Plot dotsize", 1)),
                  numericInput(ns("space_top"), "Plot space top", 1.5, step = 0.1),
                  checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
                  checkboxInput(ns("Show_ns"), "Show NS", value = F),
                  selectizeInput(ns("legend_loc"), "Legend location", choices = c("none","top","bottom","left","right"), selected = "right"),
                  selectizeInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}","p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}")
                  
              ),
              box(title = "Input", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 9, collapsed = FALSE,
                  fileInput(ns("file1"), "Choose Excell File",
                            accept = c(
                              ".xlsx",
                              ".xls")),
                  selectizeInput(ns("Subset"), "Subset", choices = NULL)),
              box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE,
                  downloadButton(ns("download_svg"), "SVG"),
                  downloadButton(ns("download_png"), "PNG"),
                  plotOutput(ns("plot")) %>% 
                    shinycustomloader::withLoader()
              )
            ),
    fluidRow(
      box(title = "Data", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6, collapsed = FALSE,
          rHandsontableOutput(ns("hot"))
      ),
      box(title = "Colour Key", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 6, collapsed = FALSE,
          rHandsontableOutput(ns("colour_key_hot"))
      )
    )
    )

}

Server_FlowJo <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      observeEvent(input$defaults,{
        if(input$defaults == "Paper"){
          updateNumericInput(session, "width", "Plot width mm", value = 50)
          updateNumericInput(session, "height", "Plot height mm", value = 30)
          updateNumericInput(session, "font", "Plot font size", value = 7)
          updateNumericInput(session, "dotsize", "Plot dotsize", value = 1)
        } else {
          updateNumericInput(session, "width", "Plot width mm (per bar)", value = 100)
          updateNumericInput(session, "height", "Plot height mm", value = 100)
          updateNumericInput(session, "font", "Plot font size", value = 12)
          updateNumericInput(session, "dotsize", "Plot dotsize", value = 2)
        }
      })
      
      
      outfile <- tempfile(fileext='.svg')
      outfile_png <- tempfile(fileext='.png')
      
      observe({
        req(input$file1)
        shinyjs::disable("downloadData")
        excel_path <- input$file1$datapath
        
        df=read_excel(input$file1$datapath)
        colnames(df)[1]="Sample"
        
        df <- df |> 
          filter(!Sample %in% c("Mean","SD")) |>
          glimpse() |> 
          separate_wider_delim(Sample,delim = "_",names = c("Genotype","Treatment","loaction"),too_many = "merge") |>
          mutate(across(!c(Genotype,Treatment,loaction),~as.double(gsub("%","",.x)))) |>
          pivot_longer(cols = -c(Genotype,Treatment,loaction),names_to = "Unit",values_to = "Value") |>
          rename(Sample = Genotype,Annotation = Treatment) |>
          # filter(Unit == "Lymphocytes/Single Cells/Live | Freq. of Parent") |>
          glimpse()
        
        updateSelectizeInput(session, "Subset", choices = c(unique(df$Unit)))
    
        if(input$Compare=="Sample"){updateTextInput(session, "stat_ref", "Stat ref", df$Sample[1])}
        if(input$Compare=="Annotation"){updateTextInput(session, "stat_ref", "Stat ref", df$Annotation[1])}

        
      }) |> 
        bindEvent(c(input$file1,input$Compare))
      
      
      observe({
        req(input$file1)
        req(!input$Subset == "")
        
        # browser()
        df=read_excel(input$file1$datapath)
        colnames(df)[1]="Sample"
        
        
        df <- df |> 
          filter(!Sample %in% c("Mean","SD")) |>
          glimpse() |> 
          separate_wider_delim(Sample,delim = "_",names = c("Genotype","Treatment","loaction"),too_many = "merge") |>
          mutate(across(!c(Genotype,Treatment,loaction),~as.double(gsub("%","",.x)))) |>
          pivot_longer(cols = -c(Genotype,Treatment,loaction),names_to = "Unit",values_to = "Value") |>
          rename(Sample = Genotype,Annotation = Treatment) |>
          # filter(Unit == "Lymphocytes/Single Cells/Live | Freq. of Parent") |>
          glimpse()
        
        
        req(input$Subset %in% unique(df$Unit))
        
        df <- df |>
          filter(Unit == input$Subset)

        df$Unit[2:nrow(df)] <- ""
        
        
        output$hot = renderRHandsontable({
          rhandsontable(df)
        }) |> 
          bindEvent(input$Subset)
        
        
      })
        
        output$plot <- renderImage({
          req(input$file1)
          req(!input$Subset=="")
          if(input$Compare=="Sample"){
            plot <- JPL_barplot(hot_to_df(input$hot),
                                hot_to_df(input$colour_key_hot),
                                ylab_split=input$ylab_split,
                                stat_ref = input$stat_ref,
                                font = input$font,
                                dotsize = input$dotsize,
                                space_top = input$space_top,
                                var_equal = input$var_equal,
                                Show_ns = input$Show_ns,
                                legend_loc = input$legend_loc,
                                label = input$Stat_type) +
              theme(rect = element_rect(fill = "transparent"))}
          if(input$Compare=="Annotation"){
          plot <- JPL_barplot_flip(hot_to_df(input$hot),
                                   ylab_split=input$ylab_split,
                                   stat_ref = input$stat_ref,
                                   font = input$font,
                                dotsize = input$dotsize,
                                space_top = input$space_top,
                                var_equal = input$var_equal,
                                Show_ns = input$Show_ns,
                                legend_loc = input$legend_loc,
                                label = input$Stat_type) +
            theme(rect = element_rect(fill = "transparent"))}
          
          
          
          
          set_panel_size(plot, file = outfile ,
                                        width = unit(input$width, "mm"),
                                        height = unit(input$height,"mm"))
          set_panel_size(plot, file = outfile_png,
                                          width = unit(input$width, "mm"),
                                          height = unit(input$height,"mm"))
          list(src = outfile,alt = "This is alternate text")
        }, deleteFile = F) 
        
      colour_key <- read_csv("Data/example_colour_key.csv")
      
      output$colour_key_hot = renderRHandsontable({
        rhandsontable(colour_key)
      })
      outfile <- tempfile(fileext='.svg')
      outfile_png <- tempfile(fileext='.png')
      
      output$download_svg <- downloadHandler(
        filename = function() {
          paste("", Sys.Date(), ".svg", sep="")
        },
        content = function(file) {
          file.copy(
            from = outfile,
            to = file
          )
        }
      )
      output$download_png <- downloadHandler(
        filename = function() {
          paste("", Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          file.copy(
            from = outfile_png,
            to = file
          )
        }
      )
      
      
      }
  )
}




