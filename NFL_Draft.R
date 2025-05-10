install.packages("ProSportsDraftData")
library(ProSportsDraftData)
data(package = "ProSportsDraftData")
library(dplyr)
library(stringr)

colleges <- read.csv("C:/Users/ernes/OneDrive/Documents/Learning R/Full_FBS_College_Football_Programs_2025_Updated.csv", 1)

head(nfl_data)
data("nfl_data")        
data("nfl_data")

##Using espn data source
espn_data <- nfl_data_espn()
espn_data <- espn_data%>%
  mutate(name_clean = str_to_lower(str_trim(name)))
  
View(espn_data) 


#using abse data source
base_data <- nfl_data_base()
base_data <- base_data%>%
  mutate(name_clean = str_to_lower(str_trim(name)))
View(base_data)


##using ringer data source
ringer_data <- nfl_data_the_ringer()
View(ringer_data)
##REMOVING DUPLICATES
ringer_data <- ringer_data%>% distinct(name, .keep_all = TRUE)
ringer_data <- ringer_data%>%
  mutate(name_clean= str_to_lower(str_trim(name)))


qbs <- ringer_data %>% select(name, position, year,yds)%>%
  filter(position == 'Quarterback')

walter_data <- nfl_data_walter_football()
View(walter_data)

##ringer_data %>%
##  select(name,rank,year,college)%>%
##  filter(year == 2020)%>%
##  arrange(rank)

##first_round <- nfl_data %>%
##  select(name,year,rank,round)%>%
##  filter(round == 1)%>%
##  arrange(year)

##nfl_draft  <- nfl_data %>%
 ## select(name,year,rank,round)%>%
##   filter(year >= 2019)%>%
  ##arrange(year)

##combine <- nfl_data%>%
##  filter(source == 'NFL.com')

##joining base, espn and ringer data sources with college team file to have a big data set
tester <- espn_data%>% left_join(base_data, by = c("name_clean"))
new_tester <- tester%>%left_join(ringer_data, by = 'name_clean')
new_tester <- new_tester%>%filter(year.x >= 2019)
new_tester <- new_tester%>%
  rename(College.Team = college.x)
final_wiv_college <- new_tester%>%left_join(colleges, by = "College.Team")
NFL_Draft_wiv_college <- final_wiv_college%>%
  select(name.x,position,year.x,grade,College.Team,Conference,Classification,round,rank.x,
         yds,ypa,ypr,tds,ints,rtg,tkls,ypc,pbu,twenty_plus,sacks,gms,strts,sk_all)
NFL_Draft_wiv_college <- NFL_Draft_wiv_college%>%
  rename(year = year.x, name = name.x, rank =rank.x,)

NFL_Draft_wiv_college %>% select(name, year, position)%>%
  filter(position == 'Quarterback' )


getwd()

write.csv(NFL_Draft_wiv_college,"C:/Users/ernes/OneDrive/Documents/Learning R/NFL_Draft_players.csv", row.names = FALSE)


###############################################################################

##total_first_round <- ringer_data%>% left_join(first_round, by = "name")

total_nfl_draft <- base_data%>% inner_join(ringer_data, by = c("name"))
total_nfl_draft <- total_nfl_draft%>% distinct(name, .keep_all = TRUE)

nfl_draft_clean <- total_nfl_draft %>% 
  select(name,grade,year,rank.x,round,position,college,yds,ypa,ypr,tds,ints,rtg,tkls,ypc,pbu,twenty_plus,sacks,gms,strts,sk_all)%>%
  ##filter(round != 'NA')%>%
  ##filter(!is.na(round))%>%
  arrange(rank.x,year)
