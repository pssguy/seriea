# result matrix by year

output$plot_matrix <- renderPlot({
  
  df <- df  %>% filter(Season==input$year) %>% 
    
    select(home,visitor,FT,hgoal,vgoal) %>% 
    mutate(GD=hgoal-vgoal,
           result = ifelse(GD>0, "H", ifelse(GD<0, "A", "D"))
    )
  
  temp <-
    rbind(
      df %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal),
      df %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal)
    ) #rbind two copies of the orignal df, simply reversing home/away team for each match
  # win was worth 2 points until 1981
  
  if (input$year>=1994) {
    temp1<-
      temp %>%
      mutate(GD = GF-GA) %>%
      group_by(team) %>%
      summarize(GP = n(),
                gf = sum(GF),
                ga = sum(GA),
                gd = sum(GD),
                W = sum(GD>0),
                D = sum(GD==0),
                L = sum(GD<0)
      ) %>%
      mutate(Pts = (W*3) + D) %>%
      arrange(desc(Pts),desc(gd),desc(gf),team)
  } else {
    
    temp1<-
      temp %>%
      mutate(GD = GF-GA) %>%
      group_by(team) %>%
      summarize(GP = n(),
                gf = sum(GF),
                ga = sum(GA),
                gd = sum(GD),
                W = sum(GD>0),
                D = sum(GD==0),
                L = sum(GD<0)
      ) %>%
      mutate(Pts = (W*2) + D) %>%
      arrange(desc(Pts),desc(gd),desc(gf),team)
  }
  df$home <- factor(df$home, levels=rev(temp1$team))
  # levels(df$home)
  df$visitor <- factor(df$visitor, levels=temp1$team)
  #  levels(df$visitor)
  
  ggplot(df, aes(home, visitor, fill = factor(result))) + 
    geom_tile(colour="gray20", size=1.5, family="bold", stat="identity", height=1, width=1) + 
    geom_text(data=df, aes(home, visitor, label = FT), color="black", size=rel(2.5)) +
    coord_flip() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    xlab("") + 
    ylab("") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill=NA,color="gray20", size=0.5, linetype="solid"),
      axis.line = element_blank(),
      axis.ticks = element_blank(), 
      axis.text = element_text(color="white", size=rel(0.8)),
      panel.background = element_rect(fill="gray20"),
      plot.background = element_rect(fill="gray20"),
      legend.position = "none",
      axis.text.x  = element_text(angle=90, vjust=0.5, hjust=0)        
    )
  
  
})
