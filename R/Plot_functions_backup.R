AE_et_al_bar <- function(file,range,ylabel=NA,scale=F,ref_group=NULL,font=7,col_pallet= Col_pallet,dotsize=1,
                         Annotation_size=2,main=""){
  # file <- "data/Figure_2.xlsx"
  # range <- "2.81"
  # ref_group <-  "Gclcfl/fl"
  # scale=F
  # main="TLR3"
  # ylabel=NA
  # font=22
  # Annotation_size = 6
  # dotsize=100
  # col_pallet=c("Ubc13fl/fl" = "#1B9E77",
  #       "Ubc13fl/fl LysM Cre+" = "#D95F02")
  # # 
  df=readxl::read_xlsx(file,sheet = range) %>% 
    # janitor::clean_names() %>% 
    mutate(group = as_factor(Group)) %>%
    glimpse()
  
  if(is.na(ylabel)){ylab=df$ylab[1]} else {ylab=ylabel}
  
  
  df_anno <- df %>% 
    select(contains("Annotation"),x) %>% 
    distinct() %>% 
    glimpse()
  
  Annotations_ids <- gsub("Annotation_",replacement = "",grep("Annotation",names(df),value = T))
  
  
  df <- df %>%
    # pivot_longer(cols = everything(),names_to = "x",values_to = "y")  %>%
    filter(!is.na(Value)) %>%
    {full_join(x=.,y =group_by(.,group) %>%
                 summarise(mean=mean(Value),
                           sd=sd(Value),
                           n=n()) %>%
                 mutate(se=sd/n))} %>%
    mutate(Colour=as_factor(Colour)) %>% 
    glimpse()
  
  max_y=max(val <- df$Value)*1.1
  
  df %>%   
    ggplot(aes(x = x,y = Value))+
    geom_bar(aes(symbol = Colour),stat='summary', fun='mean',
             fill="white",colour="black",width = 0.75,linewidth=0.1,
             position = position_dodge(width = 0.85))+
    
    
    # geom_dotplot(stackdir = "center",
    #              binaxis = "y",
    #              method="histodot",
    #              binwidth = 1,
    #              binpositions = "all",
    #              stackratio = 1.25,
    #              dotsize = dotsize,
    #              aes(fill = Colour),
    #              stroke = 0.2,
    #              position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Colour),size=dotsize,pch=21,stroke = 0.2,
               position =  (position_dodge2(width = 0.85,
                                            padding = 0)))+
    #            position = position_dodge(width = 0.85))
    
    # geom_jitter(aes(fill = Colour), size=5,shape = 21,
    #             position = position_dodge(width = 0.85))+
    # geom_sina(aes(fill = Colour), size=5,shape = 21, binwidth = 0.2)+
    # geom_jitter()  
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se,fill = Colour), width = 0.3,linewidth=0.1,
                  position = position_dodge(width = 0.85)) +
    scale_fill_manual(values = col_pallet)+
    ylab(TeX(ylab))+
    labs(title = main)+
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                                 labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))}+
    geom_pwc(aes(group = group), tip.length = 0,
             method = "t_test",p.adjust.method="bonferroni", label = "p.adj.format",label.size =  font/.pt,size = 0.1)+
    theme_classic()+
    coord_cartesian(ylim = c(0, max_y), clip = "off")+
    {if(length(Annotations_ids)>0)annotate("text",x = 0.4,y = -max_y/10,label = Annotations_ids[1],hjust = 1,size=font/.pt)} +
    {if(length(Annotations_ids)>0)annotate("text", x = c(1:length(df_anno[[1]])) ,y = -max_y/10, label = df_anno[[1]],size=font/.pt)}+
    {if(length(Annotations_ids)>1)annotate("text",x = 0.4,y = (-max_y/10)*2,label = Annotations_ids[2],size=font/.pt,hjust = 1)}+
    {if(length(Annotations_ids)>1)annotate("text", x = c(1:length(df_anno[[2]])) ,y = (-max_y/10)*2, label = df_anno[[2]],size=font/.pt)}+
    theme(axis.text=  element_text(size=font,family = "sans"),
          plot.title = element_text(size=font,family = "sans"),
          # axis.text.x = element_text(colour="black",size=font_x,family = "sans"),
          axis.text.x = element_blank(),
          text=  element_text(size=font,family = "sans"),
          plot.margin = unit(c(5,0,5+(15*length(Annotations_ids)),25), "mm"),
          element_line(size = 0.1),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_line(size=0.1),
          axis.ticks.y =element_line(size=0.1)
    ) 
  
  
  
}





barplot2=function(df,font=7,legend_loc="right",scale=F,space_top=1.1,dotsize=1,display_N=F){
  max_y=max(val <- df$Value)*space_top
  
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
    ggplot(aes(x=Annotation, y=Value))+
    geom_bar(aes(symbol=Sample),stat = "summary", fun = "mean",
             fill="white",colour="black",width = 0.75,linewidth=0.1,
             position = position_dodge(width = 0.85))+
    geom_point(aes(fill = Sample),size=dotsize,pch=21,stroke = 0.2,
               position =  (position_dodge2(width = 0.85,
                                            padding = 0)))+
    geom_errorbar(aes(x=Annotation,ymin=mean-se,ymax=mean+se,fill = Sample), width = 0.3,linewidth=0.1,
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
    ylab(df$Unit) +
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

