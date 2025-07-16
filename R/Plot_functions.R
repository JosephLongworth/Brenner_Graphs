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
    # axis.text.x = element_blank(),
    legend.position = legend_loc,
    axis.title.x = element_blank(),
    axis.ticks.x= element_blank(),
    # axis.line.x.bottom=element_line(color="#111111"),
    axis.line=element_line(color = "#111111",linewidth = 0.1),
    axis.ticks.y = element_line(color = "#111111",linewidth = 0.1),
    legend.text = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown())}

JPL_lineplot=function(df,colour_key=NA,font=7,legend_loc="right",space_top=1,dotsize=1.2,display_N=F){


  # Create a environment for local debugging while developing
  # df <- read_csv("Data/temp3.csv")
  # colour_key=NA;font=14;legend_loc="right";space_top=1;dotsize=1.2;display_N=F
  # 
  df |> 
    select(where(~ !all(is.na(.)))) |> 
    glimpse()
  
  if("Unit_lineplot" %in% colnames(df)){
    df <- df %>%
      mutate(Unit = Unit_lineplot,.keep = c("unused"))}
  
  
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
    geom_line()+
    {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    {if(length(colour_key)>1)scale_colour_manual(values = colour_key_vector)}+
    coord_cartesian(ylim = c(NA, max_y), clip = "off")+
    labs(y=df$Unit[1])+
    JPL_genral_theme(font = font,legend_loc = legend_loc)+
    theme(axis.title.y = ggtext::element_markdown())

  
}

JPL_survivalplot=function(df,colour_key=NA,font=7,legend_loc="none"){
  
  
  # # Create a enviroment for local debugging while developing
  # df=read_csv("Data/example_survivalplot.csv")
  # colour_key=NA;font=7;legend_loc="none"

  if(length(colour_key)>1){
    colour_key_vector <- colour_key %>% 
      filter(Sample %in% unique(df$Sample))
      }
  survdiff(Surv(Day, Mouse_status) ~ Sample,data = df)
  fit <- survfit(Surv(Day, Mouse_status) ~ Sample,data = df)
  
  
  fit$std.err
      fit$logse
  
  
  plot <- ggsurvplot(fit, data = df, pval = F, 
                     onf.int = TRUE,
                     # Add risk table
                     pval.size=font/.pt,
                     risk.table = TRUE,
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     palette = colour_key_vector$color_hex)
  plot$data.survtable
  
  plot <- plot$plot
  plot +
    ylab(df$Unit[1])+
    JPL_genral_theme(font = font,legend_loc = legend_loc)+
    theme(axis.title.y = ggtext::element_markdown())

}
JPL_barplot_annotation=function(df,
                                colour_key=NA,
                                font=7,
                                legend_loc="right",
                                Show_ns=F,
                                var_equal=var_equal,
                                scale=F,
                                top=5,
                                dotsize=1.2,
                                display_N=F,
                                Flip=F,
                                Group_Stats = T,
                                Sample_Stats = F,
                                label = "italic(p) = {p.adj.format}"){

  if(Flip){
    df <- df |> 
      mutate(temp = Annotation_1_Symbol,
             Annotation_1_Symbol = Sample,
             Sample = temp) |> 
      select(!temp)
  }
  
  
  
  if(length(colour_key)>1){
    colour_key_vector <- deframe(colour_key)
    names(colour_key_vector) <- gsub("\\^fl/fl","<sup>fl/fl</sup>",names(colour_key_vector))
    names(colour_key_vector) <- gsub("\\^+","<sup>+</sup>",names(colour_key_vector))
    # names(colour_key_vector) <- paste0("<i>",names(colour_key_vector),"</i>")
    }


  if("Unit_barplot_annotation" %in% colnames(df)){
    df <- df %>%
      mutate(Unit = Unit_barplot_annotation,.keep = c("unused"))}

  df <- df |> 
    mutate(Sample = as_factor(Sample)) |> 
    select(where(~ !all(is.na(.)))) |> 
    select(where(~ !all(. == ""))) |>
    glimpse()
  
  max_y=max(na.omit(df$Value))
  min_y=min(na.omit(df$Value))
  
  range <- max(c(max_y,0)) - min(c(min_y,0))


  if("Annotation_2_Symbol" %in% colnames(df)){

    
    df <- df |> 
      mutate(Annotation_1_Symbol = na_if(Annotation_1_Symbol, ""),
             Annotation_2_Symbol = na_if(Annotation_2_Symbol, "")) |>
      mutate(
        Annotation_1_Symbol = factor(Annotation_1_Symbol,levels = unique(Annotation_1_Symbol),ordered = T),
        Annotation_2_Symbol = factor(Annotation_2_Symbol,levels = unique(Annotation_2_Symbol),ordered = T),
        condition = paste(Annotation_1_Symbol,Annotation_2_Symbol,sep = "_"),
        condition = factor(condition,levels = unique(condition),ordered = T)) |>
      select(where(~ !all(is.na(.)))) |> 
      glimpse()
    
    
  df_anno <- df %>%
    dplyr::select(Annotation_1_Symbol,Annotation_2_Symbol) %>%
    mutate(Annotation_1_Symbol = as.character(Annotation_1_Symbol)) |>
    mutate(Annotation_2_Symbol = as.character(Annotation_2_Symbol)) |>
    mutate(Annotation_1_Symbol = replace_na(Annotation_1_Symbol,"")) |> 
    mutate(Annotation_2_Symbol = replace_na(Annotation_2_Symbol,"")) |> 
    distinct() %>%
    mutate(Annotation_1_Symbol = fct_recode(Annotation_1_Symbol,
                                   "-" = "NO",
                                   "+" = "YES")) |>
    mutate(Annotation_2_Symbol = fct_recode(Annotation_2_Symbol,
                                   "-" = "NO",
                                   "+" = "YES"))
  } else if("Annotation_1_Symbol" %in% colnames(df)){
    
    df <- df |> 
      mutate(Annotation_1_Symbol = na_if(Annotation_1_Symbol, "")) |>
      mutate(
        Annotation_1_Symbol = factor(Annotation_1_Symbol,levels = unique(Annotation_1_Symbol),ordered = T),
        condition = Annotation_1_Symbol) |>
      select(where(~ !all(is.na(.)))) |> 
      glimpse()
    
    df_anno <- df %>%
      dplyr::select(Annotation_1_Symbol) %>%
      distinct() %>%
      mutate(Annotation_1_Symbol = fct_recode(Annotation_1_Symbol,
                                   "-" = "NO",
                                   "+" = "YES"))
  } else {
    
    
    df <- df |> 
      mutate(condition = "") 
    df_anno <- tibble::tibble(Annotation_1_Symbol = "NA")
  }

 
  # check id a log scale should be used based on the presence of the term "pfu/organ"in the ylab
  
  if(grepl("pfu/organ",df$Unit[1])){
    log_scale = T
    yintercept = 1
    cartesian_ylim = c((min(c(min_y,1))),(max(c(max_y,1))+((range/10)*1)))
    annotation_1_y = 0.2
    annotation_2_y = 0.1
  } else {
    log_scale = F
    yintercept = 0
    cartesian_ylim = c(min(c(min_y,0)),max(c(max_y,0))+((range/10)*1))
    annotation_1_y=min(c(min_y,0)) - ((range/10)*1)
    annotation_2_y=min(c(min_y,0)) - ((range/10)*2)
  }
  
  
  
  sample_labels <- levels(df$Sample) %>%
    gsub("\\^fl/fl","<sup>fl/fl</sup>",.) %>%
    gsub("\\^+","<sup>+</sup>",.)
  
  df %>%
    filter(!is.na(Value)) %>%
    mutate(Value=as.double(Value)) |> 
    mutate(Sample = as_factor(Sample)) %>%
    group_by(condition) %>%
    {if(log_scale){mutate(.,Value2=log10(Value))}else{mutate(.,Value2=Value)}} %>% 
    {full_join(x=.,y =group_by(.,Sample,condition) %>%
                 summarise(mean=mean(Value2,na.rm = T),
                           sd=sd(Value,na.rm = T),
                           n=n()) %>%
                 mutate(se=sd/n))} %>%
    {if(log_scale){mutate(.,mean=10^mean)}else{mutate(.,mean=mean)}} %>% 
    group_by(Sample,condition) %>%
    mutate(Count = n()) %>%
    ungroup() |> 
    mutate(Sample = factor(Sample, levels = levels(Sample), labels = sample_labels)) |> 
    ggplot(aes(x=condition, y=Value))+
    geom_bar(aes(symbol=Sample,fill = Sample),stat = "summary", fun = "mean",
             colour="#111111",width = 0.65,linewidth=0.1,alpha=0.5,
             position = position_dodge(width = 0.85)) +
    geom_point(aes(fill = Sample),
               size=dotsize,
               pch=21,
               stroke = 0.2,
               width = 0.65,
               color = "#111111",linewidth = 0.1,
               position =  (position_jitterdodge(dodge.width = 0.85))) +
               # position =  (position_dodge2(width = 0.85,padding = 0))) +
    {if(length(colour_key)>1)scale_fill_manual(values = colour_key_vector)}+
    # scale_fill_identity()+
    geom_errorbar(aes(x=condition,ymin=mean-se,ymax=mean+se,symbol=Sample), width = 0.3,linewidth=0.1,
                  position = position_dodge(width = 0.85)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    {if(scale)scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                                 labels = unit_format(unit = "e+06", scale = 1 / 1e+06, digits = 2))} +
    {if(Group_Stats){
    geom_pwc(aes(group = Sample), tip.length = 0,
             # ref.group = 1,
             group.by = "x.var",
             method = "t_test",
             method.args = list(var.equal = var_equal),
             p.adjust.method="bonferroni",
             label = label,
             label.size =  font/.pt,size = 0.1,
             hide.ns = !Show_ns,
             colour = "#111111",
             family = family
    )}}+
    {if(Sample_Stats){
    geom_pwc(aes(group = Sample), tip.length = 0,
             # ref.group = "all",
             group.by = "legend.var",
             bracket.group.by = "legend.var",
             dodge = 0.85,
             method = "t_test",
             method.args = list(var.equal = var_equal),
             p.adjust.method="bonferroni",
             label = label,
             label.size =  font/.pt,size = 0.1,
             hide.ns = !Show_ns,
             bracket.nudge.y=0.2,
             colour = "#111111",
             family = family
    )}}+
    # geom_pwc(aes(group = Sample), tip.length = 0,
    #          method = "t_test",
    #          method.args = list(var.equal = var_equal),
    #          p.adjust.method="bonferroni",
    #          label = label,
    #          label.size =  font/.pt,size = 0.1,
    #          hide.ns = !Show_ns,
    #          colour = "#111111",
    #          family = family
    # )+
    # {if(display_N)
      # geom_text(aes(x = Annotation, y = 0 + 0.2, label = Count),
      #           hjust = 0.5, vjust = 0, size = font,inherit.aes=F)} +
    ylab(df$Unit[1]) +
    {if(log_scale)scale_y_log10()}+
    coord_cartesian(ylim = cartesian_ylim, clip = "off") +
    {if("Annotation_1_label" %in% colnames(df)){annotate("text",x = 0.4,y =annotation_1_y,label = df$Annotation_1_label[1],hjust = 1,size=font/.pt,colour = "#111111",family = family)}} +
    {if("Annotation_1_Symbol" %in% colnames(df)){annotate("text", x = c(1:nrow(df_anno)) ,y = annotation_1_y, label = df_anno$Annotation_1_Symbol,size=font/.pt,colour = "#111111",family = family)}}+
    {if("Annotation_2_label" %in% colnames(df)){annotate("text",x = 0.4,y =annotation_2_y,label = df$Annotation_2_label[1],hjust = 1,size=font/.pt,colour = "#111111",family = family)}} +
    {if("Annotation_2_Symbol" %in% colnames(df)){annotate("text", x = c(1:nrow(df_anno)) ,y = annotation_2_y, label = df_anno$Annotation_2_Symbol,size=font/.pt,colour = "#111111",family = family)}}+
    JPL_genral_theme(font = font,legend_loc = legend_loc)+
    # theme(plot.margin = unit(c(5,0,25,15), "mm"),
    theme(plot.margin = unit(c(top,0,25,15), "mm"),
          axis.text.x = element_blank(),
          axis.line.x = element_blank())+
    geom_hline(yintercept = yintercept, color = "#111111", lwd = 0.1)
  
  }













find_svg_offset <- function(svg_file){
  svg_code=readLines(svg_file)
  
  
  if(any(grepl("<polyline points",svg_code))){
  
  # find the coordinates of polylines and determine their lengths 
  lines1 <- tibble::tibble(polylines = grep("<polyline points",svg_code,value = T)) |>
    separate(polylines,sep = "'",into = c(NA,"coordinates"),extra = "drop") |> 
    separate(coordinates,sep = " ",into = c("a","b")) |> 
    separate(a,sep = ",",into = c("x1","y1")) |> 
    separate(b,sep = ",",into = c("x2","y2")) |> 
    mutate_all(as.numeric) |> 
    mutate(x_start = min(x1, x2),
           x_end = max(x1, x2),
           y_start = min(y1, y2),
           y_end = max(y1, y2),.keep = c("unused")) |>
    mutate(x_length = x_end - x_start,
           y_length = y_end - y_start)} else {
      lines1 <- tibble::tibble(x_start = 0,
                               y_start = 0,
                               x_length = 0,
                               y_length = 0)}
  
  if(any(grepl("<line",svg_code))){
    extract_number <- function (pattern,string){
      as.numeric(regmatches(string,regexec(paste0("",pattern,"='([0-9]+\\.[0-9]+)"),string))[[1]][2])}
    
    lines2 <- tibble::tibble(lines = grep("<line",svg_code,value = T)) |>
      rowwise() |>
      transmute(x1=extract_number("x1",lines),
                x2=extract_number("x2",lines),
                y1=extract_number("y1",lines),
                y2=extract_number("y2",lines)) |> 
      mutate(x_start = min(x1, x2),
             x_end = max(x1, x2),
             y_start = min(y1, y2),
             y_end = max(y1, y2),.keep = c("unused")) |>
      mutate(x_length = x_end - x_start,
             y_length = y_end - y_start)} else {
                  lines2 <- tibble::tibble()}

    
  lines <- bind_rows(lines1,lines2)

  
  offset <- list(
    x_offset = lines$x_start[which.max(lines$x_length)],
    y_offset = lines$y_start[which.max(lines$y_length)])
  
}

pastel_palette <- c(
  "#AEC6CF", "#FFB347", "#77DD77", "#FF6961", "#CBAACB",
  "#FFD1DC", "#CFCFC4", "#FDFD96", "#779ECB", "#966FD6",
  "#B39EB5", "#D4A76A", "#FFB7B2", "#E6E6FA",
  "#B5EAD7", "#FF9AA2", "#A2C7E5", "#D5AAE5", "#E3A869",
  "#C4B7CB", "#A8D5BA", "#C5E1A5"
)

