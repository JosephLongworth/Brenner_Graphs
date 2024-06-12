hot_to_df = function(hot) {
  hot %>%
    hot_to_r() %>%
    as.data.frame() %>%
    as_tibble()
}
barplot2=function(df,colour_key=NA,font=7,legend_loc="right",scale=F,space_top=1.1,dotsize=1,display_N=F,ylab_split=2000){
  
  
  colour_key_vector <- deframe(colour_key)
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
    geom_bar(aes(symbol=Sample),stat = "summary", fun = "mean",
             fill="white",colour="black",width = 0.75,linewidth=0.1,
             position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Sample),
               size=dotsize,
               pch=21,
               stroke = 0.2,
               position =  (position_dodge2(width = 0.85,padding = 0)))+
    scale_fill_manual(values = colour_key_vector)+
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
             label = "p.adj",label.size =  font/.pt,size = 0.1)+
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

