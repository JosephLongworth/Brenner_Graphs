hot_to_df <- function(hot) {
  if (is.null(hot)) {
    return(NULL)
  }
  rhandsontable::hot_to_r(hot)
}


family <- "Arial"
JPL_genral_theme <- function(font=7,legend_loc="right"){
theme_classic()+
  theme(
    plot.title = element_text(size=font,family = family),
    text=  element_text(size=font,family = family,colour = "#111111"),
    axis.text=  element_text(size=font,family = family,colour = "#111111"),
    # plot.margin = unit(c(5,0,25,15), "mm"),
    axis.text.x = element_blank(),
    legend.position = legend_loc,
    axis.title.x = element_blank(),
    axis.ticks.x=element_blank(),
    # axis.line.x.bottom=element_line(color="#111111"),
    axis.line=element_line(color = "#111111",linewidth = 0.1),
    axis.ticks.y =element_line(color = "#111111",linewidth = 0.1))}



JPL_barplot=function(df,
                     colour_key=NA,
                     font=7,
                     legend_loc="right",
                     scale=F,
                     space_top=1.1,
                     dotsize=1.2,
                     display_N=F,
                     ylab_split=2000,
                     Show_ns=F,
                     var_equal=T,
                     label = "italic(p) = {p.adj.format}"){
  
  if(length(colour_key)>1){
    colour_key_vector <- deframe(colour_key)}
  
  
  if("Unit_barplot" %in% colnames(df)){
  df <- df %>%
    mutate(Unit = Unit_barplot,.keep = c("unused"))}
  
  df <- df |> 
    mutate(Annotation = as.character(Annotation)) |>
    mutate(Annotation=replace_na(Annotation,""))
  
  max_y=max(df$Value)*space_top
  
  df %>%
    filter(!is.na(Value)) %>%
    mutate(Sample = as_factor(Sample)) %>%
    {full_join(x=.,y =. |> 
                 summarise(mean=mean(Value),
                           sd=sd(Value),
                           n=n(),
                           .by = c(Sample,Annotation)) %>%
                 mutate(se=sd/n))} %>%
    ungroup() %>%
    glimpse() |> 
    ggplot(aes(x=Annotation, y=Value))+
    geom_bar(aes(symbol=Sample,fill = Sample),stat = "summary", fun = "mean",
             colour="black",width = 0.65,linewidth=0.1,alpha=0.5,
             position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Sample),
               size=dotsize,
               pch=21,
               stroke = 0.2,
               # width = 0.65,
               position =  (position_dodge2(width = 0.85,padding = 0)))+
    {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    geom_errorbar(aes(x=Annotation,ymin=mean-se,ymax=mean+se,symbol=Sample), width = 0.3,linewidth=0.1,
                  position = position_dodge(width = 0.85)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                                 labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))}+
    geom_pwc(aes(group = Sample),
             tip.length = 0,
             method = "t_test",
             method.args = list(var.equal = var_equal),
             p.adjust.method="bonferroni",
             label = label,
             label.size =  font/.pt,size = 0.1,
             hide.ns = !Show_ns,
             colour = "#111111",
             family = family
             )+
    {if(display_N)
    geom_text(aes(x = Annotation, y = 0 + 0.2, label = Count),
              hjust = 0.5, vjust = 0, size = font,inherit.aes=F)} +
    coord_cartesian(ylim = c(0, max_y), clip = "off")+
    ylab(str_wrap(df$Unit[1],width = ylab_split))+
    JPL_genral_theme(font = font,legend_loc = legend_loc)

}
JPL_lineplot=function(df,colour_key=NA,font=7,legend_loc="right",scale=F,space_top=1,dotsize=1.2,display_N=F,ylab_split=2000){
  
  if(length(colour_key)>1){
    colour_key_vector <- deframe(colour_key)}
  
    max_y=max(df$Value)*space_top
  
  df %>% 
    group_by(Sample,`Days post infection`) %>%
    summarise(mean=mean(Value),
              sd=sd(Value),
              se=sd/sqrt(n())) %>%
    ggplot(aes(x=`Days post infection`, y=mean,colour=Sample),linewidth=0.1)+
    geom_point(aes(fill = Sample),
               colour = "black",
               size=dotsize,
               pch=21,
               stroke = 0.2)+
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.3,linewidth=0.1)+
    {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                                 labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))}+

    geom_line()+
    {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    coord_cartesian(ylim = c(NA, max_y), clip = "off")+
    ylab(str_wrap(df$Unit[1],width = ylab_split))+ 
    JPL_genral_theme(font = font,legend_loc = legend_loc)

  
  }
JPL_survivalplot=function(df,colour_key=NA,font=7,legend_loc="right",ylab_split=2000){
  

  if(length(colour_key)>1){
    colour_key_vector <- colour_key %>% 
      filter(Sample %in% unique(df$Sample))
      }
  
  survdiff(Surv(Day, Mouse_Status) ~ Sample,data = df)
  fit <- survfit(Surv(Day, Mouse_Status) ~ Sample,data = df)
  plot <- ggsurvplot(fit, data = df, pval = T,palette = colour_key_vector$fill,linewidth=0.1)
  plot <- plot$plot
  plot +
    ylab(str_wrap(df$Unit[1],width = ylab_split))+
    JPL_genral_theme(font = font,legend_loc = legend_loc)

}
JPL_barplot_annotation=function(df,
                                colour_key=NA,
                                font=7,
                                legend_loc="right",
                                Show_ns=F,
                                var_equal=T,
                                scale=F,
                                space_top=1.1,
                                dotsize=1.2,
                                display_N=F,
                                ylab_split=2000,
                                label = "italic(p) = {p.adj.format}"){

  # df=read_csv("Data/example_barplot_annotation.csv")
  if(length(colour_key)>1){
    colour_key_vector <- deframe(colour_key)}


  if("Unit_barplot_annotation" %in% colnames(df)){
    df <- df %>%
      mutate(Unit = Unit_barplot_annotation,.keep = c("unused"))}

  max_y=max(na.omit(df$Value))*space_top
  
  if("Annotation_2_Symbol" %in% colnames(df)){

  df_anno <- df %>%
    dplyr::select(Annotation_1_Symbol,Annotation_2_Symbol) %>%
    distinct() %>%
    mutate(Annotation_1_Symbol = as.character(Annotation_1_Symbol)) |>
    mutate(Annotation_2_Symbol = as.character(Annotation_2_Symbol)) |>
    mutate(Annotation_1_Symbol = replace_na(Annotation_1_Symbol,"")) |> 
    mutate(Annotation_2_Symbol = replace_na(Annotation_2_Symbol,"")) |> 
    mutate(Annotation_1_Symbol = fct_recode(Annotation_1_Symbol,
                                   "-" = "NO",
                                   "+" = "YES")) |>
    mutate(Annotation_2_Symbol = fct_recode(Annotation_2_Symbol,
                                   "-" = "NO",
                                   "+" = "YES"))
  } else {
    df_anno <- df %>%
      dplyr::select(Annotation_1_Symbol) %>%
      distinct() %>%
      mutate(Annotation_1_Symbol = fct_recode(Annotation_1_Symbol,
                                   "-" = "NO",
                                   "+" = "YES"))
  }

 
  df %>%
    filter(!is.na(Value)) %>%
    mutate(Sample = as_factor(Sample)) %>%
    {if("Annotation_2_Symbol" %in% colnames(df)){mutate(.,condition = paste(Annotation_1_Symbol,Annotation_2_Symbol,sep = "_"))}else{
        mutate(.,condition = Annotation_1_Symbol)}} |> 
    group_by(condition) %>%
    {full_join(x=.,y =group_by(.,Sample,condition) %>%
                 summarise(mean=mean(Value),
                           sd=sd(Value),
                           n=n()) %>%
                 mutate(se=sd/n))} %>%
    group_by(Sample,condition) %>%
    mutate(Count = n()) %>%
    ungroup() %>%
    glimpse() %>%
    # left_join(colour_key) %>%
    ggplot(aes(x=condition, y=Value))+
    geom_bar(aes(symbol=Sample,fill = Sample),stat = "summary", fun = "mean",
             colour="#111111",width = 0.65,linewidth=0.1,alpha=0.5,
             position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Sample),
               size=dotsize,
               pch=21,
               stroke = 0.2,
               width = 0.65,
               color = "#111111",linewidth = 0.1,
               position =  (position_dodge2(width = 0.85,padding = 0))) +
    {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    # scale_fill_identity()+
    geom_errorbar(aes(x=condition,ymin=mean-se,ymax=mean+se,symbol=Sample), width = 0.3,linewidth=0.1,
                  position = position_dodge(width = 0.85)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                                 labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))} +
    geom_pwc(aes(group = Sample), tip.length = 0,
             method = "t_test",
             method.args = list(var.equal = var_equal),
             p.adjust.method="bonferroni",
             label = label,
             label.size =  font/.pt,size = 0.1,
             hide.ns = !Show_ns,
             colour = "#111111",
             family = family
    )+
    # {if(display_N)
      # geom_text(aes(x = Annotation, y = 0 + 0.2, label = Count),
      #           hjust = 0.5, vjust = 0, size = font,inherit.aes=F)} +
    ylab(latex2exp::TeX(str_wrap(df$Unit[1],width = ylab_split))) +
    coord_cartesian(ylim = c(0, max_y), clip = "off") +
    # coord_cartesian(ylim = c((-max_y/10)*2, max_y), clip = "off")+
    annotate("text",x = 0.4,y = -max_y/10,label = df$Annotation_1_label[1],hjust = 1,size=font/.pt,colour = "#111111",family = family) +
    annotate("text", x = c(1:nrow(df_anno)) ,y = -max_y/10, label = df_anno$Annotation_1_Symbol,size=font/.pt,colour = "#111111",family = family)+
    {if("Annotation_2_Symbol" %in% colnames(df)){annotate("text",x = 0.4,y = (-max_y/10)*2,label = df$Annotation_2_label[1],hjust = 1,size=font/.pt,colour = "#111111",family = family)}} +
    {if("Annotation_2_Symbol" %in% colnames(df)){annotate("text", x = c(1:nrow(df_anno)) ,y = (-max_y/10)*2, label = df_anno$Annotation_2_Symbol,size=font/.pt,colour = "#111111",family = family)}}+
    JPL_genral_theme(font = font,legend_loc = legend_loc)+
    theme(plot.margin = unit(c(5,0,25,15), "mm"))
    }

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
# library(extrafont)
# font_import()
# loadfonts(device = "win")

# #
# df <- read_csv("Data/example_barplot_annotation.csv")
# colour_key <- read_csv("Data/example_colour_key.csv")
# 
# plot2 <- JPL_barplot_annotation(df,colour_key,Show_ns=T)
# plot2  
# # theme(rect = element_rect(fill = "transparent"))
# set_panel_size(plot, file = "outfile2.svg" ,
#                width = unit(40, "mm"),
#                height = unit(30,"mm"))
# 
# df <- read_csv("Data/example_barplot.csv")
# colour_key <- read_csv("Data/example_colour_key.csv")
# # 
# JPL_barplot(df,colour_key)
# set_panel_size(plot, file = "outfile2.svg" ,
#                width = unit(40, "mm"),
#                height = unit(30,"mm"))

