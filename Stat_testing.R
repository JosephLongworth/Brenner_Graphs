
test=function(df,
                                colour_key=NA,
                                font=12,
                                legend_loc="right",
                                Show_ns=F,
                                var_equal=T,
                                scale=F,
                                space_top=2.5,
                                dotsize=2,
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
             group.by = "legend.var",
             # group.by = "x.var",
             dodge = 0.85,
             method.args = list(var.equal = var_equal),
             p.adjust.method="bonferroni",
             label = "p.adj.signif",
             label.size =  font/.pt,size = 0.1,
             hide.ns = Show_ns,
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
  
test(df = df) 

  