library(shinydashboard)
library(shiny)
# library(data.table)
library(rhandsontable)
library(tidyverse)
# library(survminer)
# library(survival)
library(ggpubr)

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
                       box(plotOutput("hot2"))),
              fluidRow(box(plotOutput("hot2")))
      )
    )
  )
)

server = function(input, output) {
  
  barplot2=function(df,font=12,legend_loc="right",space_top=1.1){
    max_y=max(val <- df$Value)*space_top
    
    df %>%
      
      # Fig.3A.ii %>% 
      # pivot_longer(cols = everything(),names_to = "x",values_to = "y")  %>%
      filter(!is.na(Value)) %>%
      mutate(Sample = as_factor(Sample)) %>%
      group_by(Sample,Annotation) %>%
      {full_join(x=.,y =group_by(.,Sample,Annotation) %>%
                   summarise(mean=mean(Value),
                             sd=sd(Value),
                             n=n()) %>%
                   mutate(se=sd/n))} %>%
      group_by(Sample,Annotation) %>%
      mutate(Count = n()) %>%
      ungroup() %>%
      # glimpse()  
      
      ggplot(aes(x=Annotation, y=Value))+
      # geom_bar(aes(symbol=Sample),stat = "summary", fun = "mean",colour="black",fill="white",
      #          position =  (position_dodge2(width = 0.85)))+
      geom_bar(aes(symbol=Sample),stat = "summary", fun = "mean",
               fill="white",colour="black",width = 0.75,linewidth=0.1,
               position = position_dodge(width = 0.85))+
      # geom_point(aes(x=Sample, y=Value,fill=Sample), size=3, shape=21 )+
      geom_point(aes(fill = Sample),size=5,pch=21,stroke = 0.2,
                 position =  (position_dodge2(width = 0.85,
                                              padding = 0)))+
      geom_errorbar(aes(x=Annotation,ymin=mean-se,ymax=mean+se,fill = Sample), width = 0.3,linewidth=0.1,
                    position = position_dodge(width = 0.85)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0)))+
      geom_pwc(aes(group = Sample), tip.length = 0,
               method = "t_test",
               method.args = list(var.equal = TRUE),
               p.adjust.method="bonferroni",
               label = "p.adj.signif",label.size =  font/.pt,size = 0.1)+
      # geom_pwc(aes(group = Sample), tip.length = 0,
      #          method = "t_test",
      #          method.args = list(var.equal = TRUE),
      #          p.adjust.method="bonferroni",
      #          label = "p.adj.format",label.size =  font/.pt,size = 0.1)+
      geom_text(aes(x = Annotation, y = 0 + 0.2, label = Count),
                hjust = 0.5, vjust = 0, size = font,inherit.aes=F) +
      
      
      theme_classic()+
      coord_cartesian(ylim = c(0, max_y), clip = "off")+
      ylab(df$Unit) +
      theme(axis.text=  element_text(size=font,family = "sans"),
            plot.title = element_text(size=font,family = "sans"),
            # axis.text.x = element_text(colour="black",size=font_x,family = "sans"),
            # axis.text.x = element_blank(),
            text=  element_text(size=font,family = "sans"),
            # plot.margin = unit(c(5,0,5+(15*length(Annotations_ids)),25), "mm"),
            element_line(size = 0.1),
            legend.position = legend_loc,
            axis.title.x = element_blank(),
            axis.ticks.x=element_blank(),
            axis.line=element_line(size=0.1),
            axis.ticks.y =element_line(size=0.1))
  }
  
  df=tibble::tibble(
    Sample = c("C57BL/6","C57BL/6","C57BL/6","C57BL/6 + C.rodentium","C57BL/6 + C.rodentium","C57BL/6 + C.rodentium","C57BL/6 + C.rodentium"),
    Value = 	c(5673.5,	5730,	5611.5,	6433.5,	6325.5,	6569.5,	6607.5),
    Unit = c("intracellular thiols (MFI of mBBr)","intracellular thiols (MFI of mBBr)",
    "intracellular thiols (MFI of mBBr)","intracellular thiols (MFI of mBBr)",
    "intracellular thiols (MFI of mBBr)","intracellular thiols (MFI of mBBr)",
    "intracellular thiols (MFI of mBBr)"),
    Annotation = c("","","","Day 7 p.i.","Day 7 p.i.","Day 7 p.i.","Day 7 p.i."))
  
 hot_to_df = function(hot) {
    hot %>%
      hot_to_r() %>%
      as.data.frame() %>%
      as_tibble()
 }
 
 
  output$hot = renderRHandsontable({
    rhandsontable(df)
  })
  
  output$hot2 = renderPlot(barplot2(hot_to_df(input$hot)))
  
  

  # Create an empty ggplot object
  empty_plot <- ggplot(NULL, aes(x = NULL, y = NULL))+theme_void()
  p2H <- AE_et_al_bar(file <- "data/Figure_2.xlsx",range = "2.81",main="TLR3",)+
    theme(rect = element_rect(fill = "transparent"))
  set_panel_size(p2H, file = "Figure2/p2H.svg",width = unit(2, "cm"), height = unit(3,"cm"))
  
  
  # output$hot3 = renderRHandsontable({
  #   rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))),
  #                 stretchH = "all")
  # })
}

shinyApp(ui, server)