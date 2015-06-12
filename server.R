function(input, output, session) {
  
  
  ## set up input menu in sidebar
  output$a <- renderUI({
    
    if (input$sbMenu=="matrix") {
    
    
      sliderInput("year_matrix", "Choose Season (start year)", min=1934,max=2013,value=2013,sep="")
      
    } else {
    
      selectInput("team","Choose Team",teamOptions,selected="Juventus")
    }
    
  })
  
  
  
  # function to get Season data from graph and apply to table
  getSeason = function(data,location,session){
    
    if(is.null(data)) return(NULL)
    
    theSeason <- data$Season
    session$output$seasonVal <- renderText({
      
      theSeason })
    
    session$output$standings <- DT::renderDataTable({
#       theDivision <-  all %>% 
#         filter(Season==theSeason&team==input$team) %>% 
#         .$division
      
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
        select(date=gameDate,home,result,visitor) %>% 
        arrange(date) %>% 
        DT::datatable(options= list(paging = FALSE, searching = FALSE,info=FALSE))
      
    })
    
  }
  
  
  
  # position by year graph
  observe ({
    if (is.null(input$team)) return()
    print("enter observe")
    print(input$team)
    all %>% 
      
 #     group_by(division) %>% 
      filter(team==input$team) %>% 
      ggvis(~Season,~Overall) %>% 
      layer_points() %>% 
      scale_numeric("y", reverse=TRUE) %>% 
      add_axis("y",title="Overall League Position") %>% 
      add_axis("x",title="",format="####") %>% 
      handle_click(getSeason) %>% 
      set_options(height = 480, width = 480) %>% 
       bind_shiny("plot")
 })
  

  

  source("code/standings.R", local=TRUE)  
  source("code/matrix.R", local=TRUE)
  source("code/headToHead.R", local=TRUE)

}
