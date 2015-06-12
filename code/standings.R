# function to get Season data from graph and apply to table
getSeason = function(data,location,session){
  
  if(is.null(data)) return(NULL)
  
  theSeason <- data$Season
  session$output$seasonVal <- renderText({
    
    theSeason })
  
  session$output$standings <- DT::renderDataTable({
#     theDivision <-  all %>% 
#       filter(Season==theSeason&team==input$team) %>% 
#       .$division
    
    all %>% 
      filter(Season==theSeason) %>% 
      arrange(Position) %>% 
      
      select(team,Pl=GP,W,D,L,GD=gd,Pts) %>% 
      
      DT::datatable(options= list(scrollY = 500,paging = FALSE, searching = FALSE,info=FALSE))
    
  })
  
  session$output$results <- DT::renderDataTable({
    df %>% 
      filter(Season==theSeason&(home==input$team|visitor==input$team)) %>% 
      mutate(result=paste(hgoal,vgoal,sep=" - ")) %>% 
      select(date,home,result,visitor) %>% 
      arrange(date) %>% 
      DT::datatable(options= list(paging = FALSE, searching = FALSE,info=FALSE))
    
  })
  
}



# position by year graph
observe ({
  if (is.null(input$team)) return()
selection<-  all %>% 
    
   # group_by(division) %>% 
    filter(team==input$team)
  
selection  <- cbind(selection, id = seq_len(nrow(selection)))  

  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- selection[selection$id == x$id,c("Season","Position")]
    paste0(names(row),": ", format(row), collapse = "<br />")
  }
  
  selection %>% 
    
 #   group_by(division) %>% 
    filter(team==input$team) %>% 
    ggvis(~Season,~Overall,key := ~id) %>% 
    layer_points(fill := "blue") %>% 
    scale_numeric("y", reverse=TRUE) %>% 
    add_axis("y",title="Overall League Position") %>% 
    add_axis("x",title="",format="####") %>% 
    add_tooltip(all_values,"click") %>% 
    handle_click(getSeason) %>% 
    set_options(height = 480, width = 480) %>% 
    bind_shiny("plot")
})


