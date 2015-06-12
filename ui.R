
dashboardPage(skin="yellow",
             
  dashboardHeader(title = "Serie A"),
  dashboardSidebar(
    
    h4("   Italian Serie A"),
    hr(),
    uiOutput("a"),
    hr(),
   # selectInput("team","Choose Team",teamOptions,selected="Liverpool"),
    
    sidebarMenu(id = "sbMenu",
      menuItem("Standings", tabName = "standings"),
              # selectInput("team","Choose",teamOptions,selected="Liverpool")),
      
      menuItem("Head To Head", tabName = "matchup"),
     menuItem("Result Matrix", tabName = "matrix"),
      menuItem("Info", tabName = "info", icon = icon("info")),
      menuItem("Other Dashboards",
                menuSubItem("Fortune500",href = "https://mytinyshinys.shinyapps.io/fortune500"),
               menuSubItem("WikiGuardian",href = "https://mytinyshinys.shinyapps.io/wikiGuardian")
               #menuSubItem("World Soccer",href = "https://mytinyshinys.shinyapps.io/worldSoccer")
               
      ),
      menuItem("", icon = icon("twitter-square"),
               href = "https://twitter.com/pssGuy"),
      menuItem("", icon = icon("envelope"),
               href = "mailto:agcur@rogers.com")
    
  #    menuItem("mts Sites", 
  #             menuSubItem("Sports",href = "https://mytinyshinys.shinyapps.io/sports/"))
  
    ) 
 
  ),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
    tabItems(
      tabItem("standings",
#               fluidRow(
#                 box(
#                   width = 4,
#                   title = "Introduction", status = "info", solidHeader = TRUE,
#                   helpText(h4("some text mmmmmm
#                                mmmmmmmmm mmmmmmm mmmmmmmm mmmmmmm"))
#                 ),
#                 box(
#                   width = 2,offset=1,
#                   title = "Select Team", status = "warning", solidHeader = TRUE
#                  # selectInput("team","",teamOptions,selected="Liverpool")
#                 )
#                 
#               ),
              fluidRow(
                column(width=6,
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "League Position by Year - click point for Table and Results",
                  ggvisOutput("plot")
                  
                )),
                column(width=6,
                box(
                  width = 12, status = "success",solidHeader = TRUE,
                  collapsible=TRUE, title = "League table",
                  DT::dataTableOutput('standings')
                ),
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "Season Results",
                  collapsible=TRUE,
                  #collapsible=TRUE,collapsed = TRUE, bug?  
                  DT::dataTableOutput("results")
                  
                )
              )

      )
),
      tabItem("matrix",
#                sliderInput("year_matrix", "Choose Season", min=1870,max=2014,value=2014),
#                selectInput("division","Choose division",c("1","2")) # this needs to be a uioutput
#               downloadButton("downloadCsv", "Download as CSV")
          fluidRow(
#             column(width=5,offset=1,sliderInput("year_matrix", "Choose Season (start year)", min=1870,max=2014,value=2014,sep="")),
#             column(width=6,selectInput("division","Choose division",c("1","2")))
#             column(width=10,offset=1,
#                    box(width=12,
#                    status="warning",solidHeader = TRUE,title="Select",
#                    sliderInput("year_matrix", "Choose Season (start year)", min=1870,max=2014,value=2014,sep=""),
#                    selectInput("division","Choose division",c("1","2"))
#             )
#             )
          ),
          fluidRow(
            column(width=10,offset=1,
                   box(width=12,
              status = "success", solidHeader = TRUE,
              title = "Match Matrix - Ordered Vertically by Final Position ",
              plotOutput("plot_matrix")
          )
            )
          )
          
  


      ),
tabItem("matchup",
#         fluidRow(
#           #                 valueBoxOutput("rate"),
#           #                 valueBoxOutput("count"),
#           selectInput("team_MU","Choose",teamOptions,selected="Arsenal")
#         ),
        fluidRow(
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "Head To Head - click row for details against opponent",
            collapsible = TRUE,
            DT::dataTableOutput("headToHead")),
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "All Games",
            #textOutput("check"))
            DT::dataTableOutput("HtoHGames"))
          ),
fluidRow(
  box(width=8,
    status = "success", solidHeader = TRUE,
    title = "Summary of Seasons Difference in Overall Position",
    ggvisOutput("HtoHPos"))
        )
),
tabItem("info", includeMarkdown("info.md"))
    )#tabitems
  ) #body
) #page


