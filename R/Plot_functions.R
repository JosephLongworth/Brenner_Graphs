hot_to_df <- function(hot) {
  if (is.null(hot)) {
    return(NULL)
  }
  rhandsontable::hot_to_r(hot)
}
JPL_barplot=function(df,colour_key=NA,font=7,legend_loc="right",scale=F,space_top=1.1,dotsize=1.2,display_N=F,ylab_split=2000){
  
  if(length(colour_key)>1){
    colour_key_vector <- deframe(colour_key)}
  
  
  if("Unit_barplot" %in% colnames(df)){
  df <- df %>%
    mutate(Unit = Unit_barplot,.keep = c("unused"))}
  
  max_y=max(df$Value)*space_top
  
  df %>%
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
    # left_join(colour_key) %>%
    ggplot(aes(x=Annotation, y=Value))+
    geom_bar(aes(symbol=Sample,fill = Sample),stat = "summary", fun = "mean",
             colour="black",width = 0.65,linewidth=0.1,alpha=0.5,
             position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Sample),
               size=dotsize,
               pch=21,
               stroke = 0.2,
               width = 0.65,
               position =  (position_dodge2(width = 0.85,padding = 0)))+
    {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    # scale_fill_identity()+
    geom_errorbar(aes(x=Annotation,ymin=mean-se,ymax=mean+se,symbol=Sample), width = 0.3,linewidth=0.1,
                  position = position_dodge(width = 0.85)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                                 labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))}+
    geom_pwc(aes(group = Sample), tip.length = 0,
             method = "t_test",
             method.args = list(var.equal = TRUE),
             p.adjust.method="bonferroni",
             label = "p.adj",label.size =  font/.pt,size = 0.1
             # ,
             # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "nvs"))
             )+
    {if(display_N)
    geom_text(aes(x = Annotation, y = 0 + 0.2, label = Count),
              hjust = 0.5, vjust = 0, size = font,inherit.aes=F)} +
    theme_classic()+
    coord_cartesian(ylim = c(0, max_y), clip = "off")+
    ylab(str_wrap(df$Unit[1],width = ylab_split))+ 
    theme(axis.text=  element_text(size=font,family = "sans"),
          plot.title = element_text(size=font,family = "sans"),
          text=  element_text(size=font,family = "sans"),
          element_line(size = 0.1),
          legend.position = legend_loc,
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_line(size=0.1),
          axis.ticks.y =element_line(size=0.1))
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
    theme_classic()+
    coord_cartesian(ylim = c(NA, max_y), clip = "off")+
    ylab(str_wrap(df$Unit[1],width = ylab_split))+ 
    theme(axis.text=  element_text(size=font,family = "sans"),
          plot.title = element_text(size=font,family = "sans"),
          text=  element_text(size=font,family = "sans"),
          element_line(size = 0.1),
          legend.position = legend_loc,
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_line(size=0.1),
          axis.ticks.y =element_line(size=0.1))

  
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
    # {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    theme_classic()+
    ylab(str_wrap(df$Unit[1],width = ylab_split))+
    theme(rect = element_rect(fill = "transparent"))+
    theme(axis.text=  element_text(size=font,family = "sans"),
          plot.title = element_text(size=font,family = "sans"),
          text=element_text(size=font,family = "sans"),
          element_line(size = 0.1),
          legend.position = legend_loc,
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_line(size=0.1),
          line = element_line(linewidth = 3),
          axis.ticks.y =element_line(size=0.1))
  
}

JPL_barplot_annotation=function(df,colour_key=NA,font=7,legend_loc="right",Show_ns=F,var_equal=T,scale=F,space_top=1.1,dotsize=1.2,display_N=F,ylab_split=2000){

  if(length(colour_key)>1){
    colour_key_vector <- deframe(colour_key)}


  if("Unit_barplot_annotation" %in% colnames(df)){
    df <- df %>%
      mutate(Unit = Unit_barplot_annotation,.keep = c("unused"))}

  max_y=max(na.omit(df$Value))*space_top


  df_anno <- df %>%
    dplyr::select(Annotation_1_Symbol,Annotation_2_Symbol) %>%
    distinct() %>%
    mutate(Annotation_1_Symbol = fct_recode(Annotation_1_Symbol,
                                   "-" = "NO",
                                   "+" = "YES")) |>
    mutate(Annotation_2_Symbol = fct_recode(Annotation_2_Symbol,
                                   "-" = "NO",
                                   "+" = "YES")) |>
    
    glimpse()

  # Annotations_ids <- gsub("Annotation_",replacement = "",grep("Annotation",names(df),value = T))


  df %>%
    filter(!is.na(Value)) %>%
    mutate(Sample = as_factor(Sample)) %>%
    mutate(condition = paste(Annotation_1_Symbol,Annotation_2_Symbol,sep = "_")) %>% 
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
             colour="black",width = 0.65,linewidth=0.1,alpha=0.5,
             position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Sample),
               size=dotsize,
               pch=21,
               stroke = 0.2,
               width = 0.65,
               position =  (position_dodge2(width = 0.85,padding = 0))) +
    {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    # scale_fill_identity()+
    geom_errorbar(aes(x=condition,ymin=mean-se,ymax=mean+se,symbol=Sample), width = 0.3,linewidth=0.1,
                  position = position_dodge(width = 0.85)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                                 labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))}+
    geom_pwc(aes(group = Sample), tip.length = 0,
             method = "t_test",
             method.args = list(var.equal = var_equal),
             p.adjust.method="bonferroni",
             label = "p.adj",label.size =  font/.pt,size = 0.1,
             hide.ns = Show_ns
             # ,
             # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "nvs"))
    )+
    # {if(display_N)
      # geom_text(aes(x = Annotation, y = 0 + 0.2, label = Count),
      #           hjust = 0.5, vjust = 0, size = font,inherit.aes=F)} +
    ylab(latex2exp::TeX(str_wrap(df$Unit[1],width = ylab_split))) +
    theme_classic()+
    coord_cartesian(ylim = c(0, max_y), clip = "off") +
    # coord_cartesian(ylim = c((-max_y/10)*2, max_y), clip = "off")+
    annotate("text",x = 0.4,y = -max_y/10,label = df$Annotation_1_label[1],hjust = 1,size=font/.pt) +
    annotate("text", x = c(1:nrow(df_anno)) ,y = -max_y/10, label = df_anno$Annotation_1_Symbol,size=font/.pt)+
    annotate("text",x = 0.4,y = (-max_y/10)*2,label = df$Annotation_2_label[1],hjust = 1,size=font/.pt) +
    annotate("text", x = c(1:nrow(df_anno)) ,y = (-max_y/10)*2, label = df_anno$Annotation_2_Symbol,size=font/.pt)+
    # {if(length(Annotations_ids)>1)annotate("text",x = 0.4,y = (-max_y/10)*2,label = Annotations_ids[2],size=font/.pt,hjust = 1)}+
    # {if(length(Annotations_ids)>1)annotate("text", x = c(1:length(df_anno[[2]])) ,y = (-max_y/10)*2, label = df_anno[[2]],size=font/.pt)}+
    theme(axis.text=  element_text(size=font,family = "sans"),
          plot.title = element_text(size=font,family = "sans"),
          text=  element_text(size=font,family = "sans"),
          plot.margin = unit(c(5,0,25,15), "mm"),
          axis.text.x = element_blank(),
          legend.position = legend_loc,
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_line(size=0.1),
          axis.ticks.y =element_line(size=0.1))
    }


# 
# df <- read_csv("Data/example_barplot_annotation.csv")
# colour_key <- read_csv("Data/example_colour_key.csv")
# 
# JPL_barplot_annotation(df,colour_key)
