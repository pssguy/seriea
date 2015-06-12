# head to head 

headData <- reactive({
  print(input$team)
  
  home <- df %>% 
    filter(home==input$team) %>% 
    group_by(visitor) %>% 
    summarise(P = n(), GF = sum(hgoal), GA = sum(vgoal), GD=GF-GA,
              W=sum(result=="H"), D=sum(result=="D"), L=sum(result=="A") ) %>% 
    rename(opponent=visitor)
  away <- df %>% 
    filter(visitor==input$team) %>% 
    group_by(home) %>% 
    summarise(P = n(), GF = sum(vgoal), GA = sum(hgoal), GD=GF-GA,
              W=sum(result=="A"), D=sum(result=="D"), L=sum(result=="H") )%>% 
    rename(opponent=home)
  
  
  
  total <- rbind(home,away) 
  
  summary <- total %>% 
    group_by(opponent) %>% 
    summarize(P=sum(P),GF=sum(GF),GA=sum(GA),GD=GF-GA,W=sum(W),D=sum(D),L=sum(L)) 
  
  #     print(row.names(total))
  #     row.names(total) <- total$opponent
  #     print(row.names(total))
  
  info=list(total=total,summary=summary)
  return(info)
  
})



output$headToHead <- DT::renderDataTable({
  headData()$summary %>%  
    DT::datatable(rownames=TRUE,selection='single',options= list(pageLength=10,
                                                                 paging = TRUE, searching = TRUE,info=FALSE))
})


output$HtoHGames <- DT::renderDataTable({ 
  print("enter HtoHGames")
  
  if(is.null(input$headToHead_rows_selected)) return()
  print("enter HtoHGames running")
  s = as.integer(input$headToHead_rows_selected)
 
  theOpponent <-headData()$summary$opponent[s]
  
  print(theOpponent)
  print(input$team)
  
  df <-df %>% 
    filter((home==input$team&visitor==theOpponent)|(home==theOpponent&visitor==input$team)) %>% 
    select(date,home,FT,visitor) %>% 
    arrange(desc(date)) 
  
  print(glimpse(df))
  print("looks good to here")
  df <- data.frame(df) #does not help
  print(str(df))
#   df  %>% 
#     DT::datatable(rownames=TRUE,selection='single',options= list(pageLength=10,
#                                                                  paging = TRUE, searching = FALSE,info=FALSE))
  df  %>% 
    DT::datatable()
  
  m = data.frame(a = 1, b = 2, c = 3)
  datatable(m)
})

#vis  <- reactive({
observe({
  print("enter HtoHPos")
  if(is.null(input$headToHead_rows_selected)) return()
  s = input$headToHead_rows_selected
  print("enter HtoHPos running")
  teamB <-headData()$summary$opponent[s]
  teamA <- input$team
  
  test <-all %>% 
    select(Season,team,Overall) %>% 
    filter(team==teamA|team==teamB) %>% 
    spread(team,Overall) 
  
  
  if(teamA<teamB) {
    
    colnames(test) <- c("Season","team","opponent")
    
  } else {
    colnames(test) <- c("Season","opponent","team")  
  }
  
  test %>% 
    mutate(diff=opponent-team) %>% 
    filter(!is.na(diff)) %>% 
    ggvis(~diff,fill:='#f39c12') %>% 
    add_axis("x", title="Difference in Overall Standing") %>% 
    add_axis("y", title= "Seasons") %>% 
    bind_shiny("HtoHPos")
  
})

#vis %>% bind_shiny("HtoHpos")