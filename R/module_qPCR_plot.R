UI_qPCR_plot <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title = "upload",collapsible = TRUE,solidHeader = TRUE,status = "info",width = 3,
          splitLayout(
          cellWidths = c("50%", "50%"),
          
          fileInput(
            ns("upload_layout_table"),
            label = NULL,
            buttonLabel = "Upload Layout",
            accept = c(".xlsx")
          ),
          
          fileInput(
            ns("upload_CV_table"),
            label = NULL,
            buttonLabel = "Upload CVs",
            accept = c(".xlsx")
          )
        ),
        selectizeInput(ns("HK_gene"), "HK gene", choices = NULL),
        selectizeInput(ns("control_condtion"), "Control Condition", choices = NULL)
      ),
      
      
      box(title = "Plot", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 9, collapsed = FALSE,
          downloadButton(ns("downloadPaper"), "SVG"),
          downloadButton(ns("downloadPaperpng"), "PNG"),
          plotOutput(ns("plot")) %>% 
            shinycustomloader::withLoader()
      )
    ),
    fluidRow(
              box(title = "Plot Parameters", collapsible = TRUE, solidHeader = TRUE, status = "info", width = 3, collapsed = FALSE,
                  radioButtons(inputId = ns("defaults"),label = NULL, choices = c("Paper", "Presentation"),
                               selected = "Presentation",inline = T),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("width"), "Plot width mm (per bar)", 7.5),
                    numericInput(ns("height"), "Plot height mm", 25)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(ns("font"), "Plot font size", 7),
                    numericInput(ns("dotsize"), "Plot dotsize", 1)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    checkboxInput(ns("Group_Stats"), "Group Stats", value = T),
                    checkboxInput(ns("Sample_Stats"), "Sample Stats", value = F)),
                  numericInput(ns("top"), "Plot space top mm", 5, step = 1),
                  checkboxInput(ns("var_equal"), "Variance equal", value = TRUE),
                  checkboxInput(ns("Show_ns"), "Show NS", value = F),
                  selectizeInput(ns("legend_loc"), "Legend location", choices = c("none","top","bottom","left","right"), selected = "none"),
                  selectizeInput(ns("Stat_type"), "Stat type", choices = c("italic(p) = {p.adj.format}","p.signif", "p.adj.signif", "p.format", "p.adj.format"), selected = "italic(p) = {p.adj.format}")
                  
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

Server_qPCR_plot <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      observeEvent(input$upload_layout_table,{
        
        req(session$userData$vars$qPCR_layout)
        browser()
        
          updateSelectizeInput(session,"HK_gene", choices = unique(session$userData$vars$qPCR_layout$Gene))
          updateSelectizeInput(session,"control_condtion", choices = unique(session$userData$vars$qPCR_layout$Sample))
          
        })
      
      df <- read_csv("Data/example_barplot_annotation2.csv",show_col_types = FALSE)
      
      if("Unit_barplot" %in% colnames(df)){
          df <- df %>%
            mutate(Unit = Unit_barplot,.keep = c("unused"))}
      colour_key <- read_csv("Data/example_colour_key.csv",show_col_types = FALSE)
      
      output$hot = renderRHandsontable({
        rhandsontable(df)
      })
      output$colour_key_hot = renderRHandsontable({
        rhandsontable(colour_key)
      })
      outfile <- tempfile(fileext='.svg')
      outfile_png <- tempfile(fileext='.png')
      output$plot <- renderImage({
        req(input$hot)
        empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+
          theme_void()
        # browser()
        plot <- JPL_barplot_annotation(hot_to_df(input$hot),
                                       hot_to_df(input$colour_key_hot),
                                       font = input$font,
                                       dotsize = input$dotsize,
                                       top = input$top,
                                       var_equal = input$var_equal,
                                       Show_ns = input$Show_ns,
                                       legend_loc = input$legend_loc,
                                       label = input$Stat_type,
                                       Group_Stats = input$Group_Stats,
                                       Sample_Stats = input$Sample_Stats
                                       ) +
        theme(rect = element_rect(fill = "transparent"))
        
        
        
        # df %>%
        #   glimpse() |> 
        #   filter(!is.na(Value)) %>%
        #   mutate(Value=as.double(Value)) |> 
        #   mutate(Sample = as_factor(Sample)) %>%
        #   group_by(condition) %>%
        #   {if(log_scale){mutate(.,Value2=log10(Value))}else{mutate(.,Value2=Value)}} %>% 
        #   {full_join(x=.,y =group_by(.,Sample,condition) %>%
        #                summarise(mean=mean(Value2,na.rm = T),
        #                          sd=sd(Value,na.rm = T),
        #                          n=n()) %>%
        #                mutate(se=sd/n))} %>%
        #   {if(log_scale){mutate(.,mean=10^mean)}else{mutate(.,mean=mean)}} %>% 
        #   glimpse() |> 
        #   group_by(Sample,condition) %>%
        #   mutate(Count = n()) %>%
        #   ungroup() %>%
        #   glimpse() |> 
        #   # left_join(colour_key) %>%
        #   ggplot(aes(x=condition, y=Value))+
        #   geom_bar(aes(symbol=Sample,fill = Sample),stat = "summary", fun = "mean",
        #            colour="#111111",width = 0.65,linewidth=0.1,alpha=0.5,
        #            position = position_dodge(width = 0.85)) +
        #   geom_point(aes(fill = Sample),
        #              size=dotsize,
        #              pch=21,
        #              stroke = 0.2,
        #              width = 0.65,
        #              color = "#111111",linewidth = 0.1,
        #              position =  (position_jitterdodge(dodge.width = 0.85))) +
        #   # position =  (position_dodge2(width = 0.85,padding = 0))) +
        #   {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
        #   # scale_fill_identity()+
        #   geom_errorbar(aes(x=condition,ymin=mean-se,ymax=mean+se,symbol=Sample), width = 0.3,linewidth=0.1,
        #                 position = position_dodge(width = 0.85)) +
        #   scale_y_continuous(expand = expansion(mult = c(0, 0)))+
        #   {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
        #                                labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))} +
        #   {if(Group_Stats){
        #     geom_pwc(aes(group = Sample), tip.length = 0,
        #              # ref.group = 1,
        #              group.by = "x.var",
        #              method = "t_test",
        #              method.args = list(var.equal = var_equal),
        #              p.adjust.method="bonferroni",
        #              label = label,
        #              label.size =  font/.pt,size = 0.1,
        #              hide.ns = !Show_ns,
        #              colour = "#111111",
        #              family = family
        #     )}}+
        #   {if(Sample_Stats){
        #     geom_pwc(aes(group = Sample), tip.length = 0,
        #              # ref.group = "all",
        #              group.by = "legend.var",
        #              bracket.group.by = "legend.var",
        #              dodge = 0.85,
        #              method = "t_test",
        #              method.args = list(var.equal = var_equal),
        #              p.adjust.method="bonferroni",
        #              label = label,
        #              label.size =  font/.pt,size = 0.1,
        #              hide.ns = !Show_ns,
        #              bracket.nudge.y=0.2,
        #              colour = "#111111",
        #              family = family
        #     )}}+
        #   # geom_pwc(aes(group = Sample), tip.length = 0,
        #   #          method = "t_test",
        #   #          method.args = list(var.equal = var_equal),
        #   #          p.adjust.method="bonferroni",
        #   #          label = label,
        #   #          label.size =  font/.pt,size = 0.1,
        #   #          hide.ns = !Show_ns,
        #   #          colour = "#111111",
        #   #          family = family
        #   # )+
        #   # {if(display_N)
        #   # geom_text(aes(x = Annotation, y = 0 + 0.2, label = Count),
        #   #           hjust = 0.5, vjust = 0, size = font,inherit.aes=F)} +
        #   ylab(df$Unit[1]) +
        #   {if(log_scale)scale_y_log10()}+
        #   coord_cartesian(ylim = cartesian_ylim, clip = "off") +
        #   {if("Annotation_1_label" %in% colnames(df)){annotate("text",x = 0.4,y =annotation_1_y,label = df$Annotation_1_label[1],hjust = 1,size=font/.pt,colour = "#111111",family = family)}} +
        #   {if("Annotation_1_Symbol" %in% colnames(df)){annotate("text", x = c(1:nrow(df_anno)) ,y = annotation_1_y, label = df_anno$Annotation_1_Symbol,size=font/.pt,colour = "#111111",family = family)}}+
        #   {if("Annotation_2_label" %in% colnames(df)){annotate("text",x = 0.4,y =annotation_2_y,label = df$Annotation_2_label[1],hjust = 1,size=font/.pt,colour = "#111111",family = family)}} +
        #   {if("Annotation_2_Symbol" %in% colnames(df)){annotate("text", x = c(1:nrow(df_anno)) ,y = annotation_2_y, label = df_anno$Annotation_2_Symbol,size=font/.pt,colour = "#111111",family = family)}}+
        #   JPL_genral_theme(font = font,legend_loc = legend_loc)+
        #   # theme(plot.margin = unit(c(5,0,25,15), "mm"),
        #   theme(plot.margin = unit(c(top,0,25,15), "mm"),
        #         axis.text.x = element_blank(),
        #         axis.line.x = element_blank())+
        #   geom_hline(yintercept = yintercept, color = "#111111", lwd = 0.1)
        
      # }
      
        
        
        
        
        
        
        
        
        nbars <- hot_to_df(input$hot) |>
          select(Sample,Annotation_1_Symbol,Annotation_2_Symbol) |>
          distinct() |>
          summarise(nbars=n()) |>
          as.double()
        
        set_panel_size(plot, file = outfile ,
                       width = unit(nbars * input$width, "mm"),
                       height = unit(input$height,"mm"))
        set_panel_size(plot, file = outfile_png,
                       width = unit(nbars * input$width, "mm"),
                       height = unit(input$height,"mm"))
        list(src = outfile,
             alt = "This is alternate text")
      }, deleteFile = F)
      output$downloadPaper <- downloadHandler(
        filename = function() {
          paste("PaperSize-", Sys.Date(), ".svg", sep="")
        },
        content = function(file) {
          file.copy(
            from = outfile,
            to = file
          )
        }
      )
      output$downloadPaperpng <- downloadHandler(
        filename = function() {
          paste("PaperSize-", Sys.Date(), ".png", sep="")
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

