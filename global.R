library(engsoccerdata)
library(ggvis)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyr)
library(markdown)

df <- tbl_df(italycalcio)
#glimpse(df) #max(df$Season)
df$gameDate <- as.Date(df$date) # takes a while
df$result <- ifelse(df$hgoal>df$vgoal,"H",ifelse(df$hgoal<df$vgoal,"A","D"))

## make tables - shpuld look at make table function

temp <-
  rbind(
    df %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal,Season),
    df %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal,Season)
  )

## do adjust based on points for win
# Since the 1994-1995 season, teams were awarded three points for a win, one point for a draw, and no points for a loss.

new<-
  temp %>%
  filter(Season>=1994) %>% 
  mutate(GD = GF-GA) %>%
  group_by(team,Season) %>%
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

old<-
  temp %>%
  filter(Season<=1993) %>% 
  mutate(GD = GF-GA) %>%
  group_by(team,Season) %>%
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


## add positions both within division and overall
## may want to diff for different plots

all<- rbind(old,new) %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  arrange(desc(Pts),desc(gd),desc(gf),team) %>% 
  mutate(Position=row_number()) %>% 
  ungroup()  %>%
  
  
  arrange(Position) %>% 
  group_by(Season) %>% 
  mutate(Overall=row_number()) %>% 
  ungroup()

# set to character to enbale points to be discrete colors (more relebvant if other leagues added)
#all$tier <- as.character(all$tier)



print(glimpse(all))

teamOptions <- sort(unique(all$team))

seasonOptions <- c(1934:2014)