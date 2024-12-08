UI_FlowJo <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      tabBox(title = "Plot Parameters", width = 3,
             tabPanel("Genral",
                  fileInput(ns("file1"), "Choose Excell File",
                            accept = c(
                              ".xlsx",
                              ".xls")),
                  selectizeInput(ns("Subset"), "Subset", choices = NULL),
                  radioButtons(inputId = ns("defaults"),label = NULL, choices = c("Paper", "Presentation"),
                               selected = "Presentation",inline = T),
                  selectizeInput(ns("Compare"), "Compare", choices = c("Sample","Annotation"), selected = "Sample"),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    checkboxInput(ns("Group_Stats"), "Group Stats", value = T),
                    checkboxInput(ns("Sample_Stats"), "Sample Stats", value = F)),
                    numericInput(ns("top"), "Plot space top mm", 5, step = 1)),
             tabPanel("Advanced",
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("width"), "Plot width mm (per bar)", 150),
                    numericInput(ns("height"), "Plot height mm", 100)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("font"), "Plot font size", 7),
                    numericInput(ns("dotsize"), "Plot dotsize", 1)),
                  textInput(ns("stat_ref"), "Stat ref", "all"),
                  checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
                  checkboxInput(ns("Show_ns"), "Show NS", value = F),
                  selectizeInput(ns("legend_loc"), "Legend location", choices = c("none","top","bottom","left","right"), selected = "right"),
                  selectizeInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}","p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}")
             )
                  
              ),
              box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9,
                  height = "calc(100vh - 100px)",collapsed = FALSE,
                  downloadButton(ns("download_svg"), "SVG"),
                  downloadButton(ns("download_png"), "PNG"),
                  plotOutput(ns("plot")) %>% 
                    shinycustomloader::withLoader()
              )
            ),
    fluidRow(
      tabBox(width=12,
             tabPanel("Data",
                      rHandsontableOutput(ns("hot"))
             ),
             tabPanel("Colour Key",
                      rHandsontableOutput(ns("colour_key_hot"))
      ))
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
          rename(Annotation_1_Symbol = Genotype,Sample = Treatment) |>
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
          # browser()
          if(input$Compare=="Sample"){
            plot <- JPL_barplot_annotation(hot_to_df(input$hot),
                                hot_to_df(input$colour_key_hot),
                                # stat_ref = input$stat_ref,
                                font = input$font,
                                dotsize = input$dotsize,
                                top = input$top,
                                var_equal = input$var_equal,
                                Show_ns = input$Show_ns,
                                legend_loc = input$legend_loc,
                                label = input$Stat_type,
                                Group_Stats = input$Group_Stats,
                                Sample_Stats = input$Sample_Stats) +
              theme(rect = element_rect(fill = "transparent"))
            }
          if(input$Compare=="Annotation"){
          plot <- JPL_barplot_annotation(hot_to_df(input$hot),
                                   # stat_ref = input$stat_ref,
                                   font = input$font,
                                dotsize = input$dotsize,
                                top = input$top,
                                var_equal = input$var_equal,
                                Show_ns = input$Show_ns,
                                legend_loc = input$legend_loc,
                                label = input$Stat_type,
                                Group_Stats = input$Group_Stats,
                                Sample_Stats = input$Sample_Stats,
                                Flip=T) +
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




