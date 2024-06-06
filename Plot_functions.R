library (tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(survminer)
library(survival)

# Plot Device -------------------------------------------------------------
barplot=function(df,font=12,legend_loc="right",space_top=1.1){df 
  max_y=max(val <- df$Value)*space_top
  
  df %>%
    # pivot_longer(cols = everything(),names_to = "x",values_to = "y")  %>%
    filter(!is.na(Value)) %>%
    mutate(Sample = as_factor(Sample)) %>%
    {full_join(x=.,y =group_by(.,Sample) %>%
                 summarise(mean=mean(Value),
                           sd=sd(Value),
                           n=n()) %>%
                 mutate(se=sd/n))} %>%
    group_by(Sample) %>% 
    mutate(Count = n()) %>%
    ggplot(aes(x=Sample, y=Value)) +
    geom_bar(stat = "summary", fun = "mean",
             fill="white",colour="black",width = 0.75,linewidth=0.1,
             position = position_dodge(width = 0.85)) +
    geom_point(aes(fill = Sample),size=5,pch=21,stroke = 0.2,
               # position = ggstance::position_dodge2v(height=1)
               position =  (position_dodge2(width = 0.85,
                                            padding = 0))
    )+
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.3,linewidth=0.1,
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
    geom_text(aes(x = Sample, y = 0 + 0.2, label = Count), # Adjust y position as needed
              hjust = 0.5, vjust = 0, size = font) +  # Adjust font size and alignment
    theme_classic()+
    coord_cartesian(ylim = c(0, max_y), clip = "off")+
    ylab(df$Unit) +
    theme(axis.text=  element_text(size=font,family = "sans"),
          plot.title = element_text(size=font,family = "sans"),
          # axis.text.x = element_text(colour="black",size=font_x,family = "sans"),
          axis.text.x = element_blank(),
          text=  element_text(size=font,family = "sans"),
          # plot.margin = unit(c(5,0,5+(15*length(Annotations_ids)),25), "mm"),
          element_line(size = 0.1),
          legend.position = legend_loc,
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_line(size=0.1),
          axis.ticks.y =element_line(size=0.1))
}
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
barplot3=function(df,font=12,legend_loc="right",space_top=1.1){df 
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
    scale_y_continuous(expand = expansion(mult = c(0, 0)),transform = "log10")+
    # scale_y_continuous(transform='log10')+
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
    coord_cartesian(ylim = c(1, max_y), clip = "off")+
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
barplot4=function(df,font=12,legend_loc="right",space_top=1.1){ 
  
  # df = Fig.2F
  max_y=max(val <- df$Value)*space_top
  
  df %>%
    filter(!is.na(Value)) %>%
    mutate(Sample = as_factor(Sample)) %>%
    {full_join(x=.,y =group_by(.,Sample) %>%
                 summarise(mean=mean(Value),
                           sd=sd(Value),
                           n=n()) %>%
                 mutate(se=sd/n))} %>%
    group_by(Sample) %>% 
    mutate(Count = n()) %>%
    ggplot(aes(x=Sample, y=Value))+
    geom_bar(stat = "summary", fun = "mean",
             fill="white",colour="black",width = 0.75,linewidth=0.1,
             position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Sample),size=5,pch=21,stroke = 0.2,
               position =  (position_dodge2(width = 0.85,
                                            padding = 0)))+
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.3,linewidth=0.1,
                  position = position_dodge(width = 0.85)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    stat_anova_test() +
    geom_text(aes(x = Sample, y = 0 + 0.2, label = Count), # Adjust y position as needed
              hjust = 0.5, vjust = 0, size = font) +  # Adjust font size and alignment
    theme_classic()+
    coord_cartesian(ylim = c(0, max_y), clip = "off")+
    ylab(df$Unit) +
    theme(axis.text=  element_text(size=font,family = "sans"),
          plot.title = element_text(size=font,family = "sans"),
          # axis.text.x = element_text(colour="black",size=font_x,family = "sans"),
          axis.text.x = element_blank(),
          text=  element_text(size=font,family = "sans"),
          # plot.margin = unit(c(5,0,5+(15*length(Annotations_ids)),25), "mm"),
          element_line(size = 0.1),
          legend.position = legend_loc,
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_line(size=0.1),
          axis.ticks.y =element_line(size=0.1))
}
lineplot=function(df){
  df %>%
    group_by(Sample,`Days post infection`) %>%
    summarise(mean=mean(Value),
              sd=sd(Value),
              se=sd/sqrt(n()),
              Count=n()) %>%
    {full_join(x=.,y =group_by(.,Sample) %>%
                 summarise(Count2=max(Count)) %>%
                 mutate(Sample2=paste0(Sample,"_(n = ",Count2,")")))} %>%
    ggplot(aes(x=`Days post infection`, y=mean,colour=Sample2))+
    # ggplot(aes(x=`Days post infection`, y=mean,colour=Sample))+
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2)+
    geom_line()+
    theme_classic()+
    ylab(df$Unit)
  
}
lineplot2=function(df){
  df %>%
    group_by(Sample,`Time (minutes)`) %>%
    summarise(mean=mean(Value),
              sd=sd(Value),
              se=sd/sqrt(n()),
              Count=n()) %>%
    {full_join(x=.,y =group_by(.,Sample) %>%
                 summarise(Count2=max(Count)) %>%
                 mutate(Sample2=paste0(Sample,"_(n = ",Count2,")")))} %>%
    ggplot(aes(x=`Time (minutes)`, y=mean,colour=Sample2))+
    # ggplot(aes(x=`Time (minutes)`, y=mean,colour=Sample))+
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2)+
    geom_line()+
    theme_classic()+
    ylab(df$Unit)}
Survivalplot=function(df){
  
  fit <- survfit(Surv(Day, Mouse_Status) ~ Sample,data = df)
  ggsurvplot(fit,legend=c(0.2,0.1), surv.scale="percent",data = df, pval = TRUE,
             risk.table = TRUE
  )
}