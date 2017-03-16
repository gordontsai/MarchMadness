#data packages
library("reshape")
library("dplyr")
library("data.table")
options(warn=-1)

##############################################Helper functions
average_difference <- function(teams, simsize){
  difference_sample = as.numeric(simsize)
  for(i in 1:simsize) {
    if (i%%100 == 0){
      print(i)
    }
    teamOne = sample(teams,1,replace = TRUE)
    teamTwo = sample(teams,1,replace = TRUE)
    while (teamOne == teamTwo) {
      teamOne = sample(teams,1,replace = TRUE)
      teamTwo = sample(teams,1,replace = TRUE)
    }
    output = match(teamOne,teamTwo)
    difference_sample[i] = output[3]
  }
  
  return(mean(as.double(difference_sample)))
}

match <- function(teamOne, teamTwo){
  a = grid_sum(datacut1, teamOne,'offense')
  b = grid_sum(datacut2,teamTwo,'defense')
  d = grid_sum(datacut1,teamTwo ,'offense')
  e = grid_sum(datacut2, teamOne,'defense')
  resultOne = sum(a-b)
  resultTwo = sum(d-e)
  result = abs(resultTwo-resultOne)
  matchup = paste(teamOne,"vs.",teamTwo)
  if (resultOne> resultTwo){
    return(c(matchup, teamOne, result))
  } else if (resultTwo > resultOne) {
    return(c(matchup, teamTwo, result))
  }
}

offensiveplot = function(college, pivot, Year){
  tochart = subset(pivot, pivot$shooter == college & as.numeric(as.character(pivot$year)) == Year)
  x = seq(1:100)
  y = seq(1:100)
  surface = data.frame(cbind(c(0),c(0),c(0)))
  for(i in 1:length(x))
    for(j in 1:length(y))
    {
      addrow = c()
      relframe = subset(tochart, tochart$positionleft == i/100 & tochart$positiontop == j/100)
      if(length(relframe[,1])>0)
        addrow = c(i,j,as.numeric(as.character((relframe[,6]))))
      else
        addrow = c(i,j,0)
      surface = rbind(surface, addrow)
    }
  surface = surface[-1,]
  
  
  #surface = expand.grid(x,y)
  #surface = surface[order(surface$Var1),]
  #surface = cbind(surface, numeric(nrow(surface)))
  #colnames(surface) = c("X1","X2","X3")
  
  surface = cast(surface, X1~X2)
  surface = surface[,-1]
  colnames(surface) = 1:100
  newlist = c()
  for(i in 1:100)
  {
    newlist = c(newlist,as.numeric(surface[,i]))
  }
  
  mat2 = matrix(newlist, ncol = 100)
  
  persp(x = 1:100, y = 1:100, mat2)
}

defensiveplot = function(college, pivot, Year){
  tochart = subset(pivot, pivot$shooter == college & as.numeric(as.character(pivot$year)) == Year)
  x = seq(1:100)
  y = seq(1:100)
  surface = data.frame(cbind(c(0),c(0),c(0)))
  for(i in 1:length(x))
    for(j in 1:length(y))
    {
      addrow = c()
      relframe = subset(tochart, tochart$positionleft == i/100 & tochart$positiontop == j/100)
      if(length(relframe[,1])>0)
        addrow = c(i,j,as.numeric(as.character((relframe[,6]))))
      else
        addrow = c(i,j,0)
      surface = rbind(surface, addrow)
    }
  surface = surface[-1,]
  
  surface = cast(surface, X1~X2)
  surface = surface[,-1]
  colnames(surface) = 1:100
  newlist = c()
  for(i in 1:100)
  {
    newlist = c(newlist,as.numeric(surface[,i]))
  } 
  
  mat2 = matrix(newlist, ncol = 100)
  
  persp(x = 1:100, y = 1:100, mat2)
}

grid_sum <- function(datacut,team, position) { #position is shooter or defender 
  surface = expand.grid(1.0:101.0,1.0:101.0)
  surface = surface[order(surface$Var1),]
  surface = cbind(surface, numeric(nrow(surface)))
  colnames(surface) = c("X1","X2","X3")
  surface$X1 = as.numeric(surface$X1)
  surface$X2 = as.numeric(surface$X2)
  surface$X3 = as.numeric(surface$X3)
  surface = cast(surface, X1~X2)
  surface = surface[,-1]
  colnames(surface) = 1:101
  
  test = datacut
  test$positionleft = as.integer(test$positionleft*100 + 1)
  test$positiontop = as.integer(test$positiontop*100 + 1)
  if (position == "offense"){
    test = test[shooter == team]  
    test = test[,.(ExpectedShotValue.Avg = mean(ExpectedShotValue)), by = .(positiontop,positionleft,shooter)]
  } else {
    test = test[defender == team]  
    test = test[,.(ExpectedShotValue.Avg = mean(ExpectedShotValue)), by = .(positiontop,positionleft,defender)]
  }
  
  for (i in 1:nrow(test)) {
    surface[test$positionleft[i],test$positiontop[i]] = test$ExpectedShotValue.Avg[i]
  }
  return(surface)
}

######################################################################
file1 = "MarchMadnessScrape 2015-2016.csv" #Data source feed for relevant data, play by play shot data
file2 = "MarchMadnessFirstRound 2015-2016.csv" #input list of First Round Bracket
file3 = "MarchMadness 2015-2016 Correct Bracket.csv" #correct file to cross validate only used for backtesting

#relevantdata <- fread(file1)
#teams = unique(c(unique(relevantdata$`Home Team`), unique(relevantdata$`Away Team`)))
#write.csv(teams,"TeamList.csv", row.names = FALSE)

relevantdata <- fread(file1)
relevantdata = cbind(relevantdata,rep(1,nrow(relevantdata)))
colnames(relevantdata)[length(relevantdata)]  = "Shot Count"
teams = unique(c(unique(relevantdata$`Home Team`), unique(relevantdata$`Away Team`)))

attach(relevantdata)
shooter = ifelse(`Shooting Team` == "home",as.character(`Home Team`), as.character(`Away Team`))
defender = ifelse(`Shooting Team` == "home",as.character(`Away Team`), as.character(`Home Team`))
threepFlag = ifelse(as.numeric(regexpr("Three", Text)) > 0, 1, 0)
pointscored = `Shot Status`*(2+threepFlag)
pointsdefended = 2 + threepFlag - pointscored
assistflag = ifelse(as.numeric(regexpr("Assisted", Text)) > 0, 1, 0)
homepositionleft = `Left Position`
homepositiontop = `Top Position`
awaypositionleft = 1-`Left Position`
awaypositiontop = 1-`Top Position`
positionleft = ifelse(`Shooting Team` == "home", homepositionleft, awaypositionleft)
positiontop = ifelse(`Shooting Team` == "home", homepositiontop, awaypositiontop)
gameID = paste(as.character(Date),`Home Team`)
year = substring(Date, nchar(as.character(Date))-4,nchar(as.character(Date)))

relevantdata = data.table(as.character(Date), year, shooter, defender, as.character(`Home Team`), as.character(`Away Team`), as.character(`Shooter Name`), as.character(`Shooter ID`), `Shot Number`, Quarter, positionleft, positiontop, awaypositionleft, awaypositiontop, threepFlag, pointscored, pointsdefended, assistflag, as.character(Text),gameID, `Shot Count`)

colnames(relevantdata)[1] = "Date"
colnames(relevantdata)[5] = "Home"
colnames(relevantdata)[6] = "Away"
colnames(relevantdata)[7] = "Shootername"
colnames(relevantdata)[8] = "ShooterID"
colnames(relevantdata)[9] = "Shot Number"
colnames(relevantdata)[19] = "Text"
colnames(relevantdata)[21] = "Shot Count"
attach(relevantdata)
detach(relevantdata)
relevantdata = na.omit(relevantdata)

####################Data formatting above
scores = as.numeric(relevantdata$pointscored)
relevantdata$`Shot Count` = as.numeric(relevantdata$`Shot Count`)
relevantdata$pointscored = ifelse(scores == 1, 0, scores)
temp_percents = relevantdata[,lapply(.SD, function(x) x/sum(x)), by = .(gameID,shooter),.SDcols = "pointscored"]
temp_frequency = relevantdata[,.(`Shot Count` = sum(`Shot Count`)),by = .(gameID,shooter,positionleft,positiontop)]
colnames(temp_frequency)[length(temp_frequency)] = "shotFrequency" 
percentscored = temp_percents$pointscored
relevantdata = cbind(relevantdata, percentscored)
relevantdata = merge(relevantdata, temp_frequency, by = c("gameID","shooter","positionleft","positiontop"))

#need to update year to season
datacut1 = subset(relevantdata, pointscored != "0")
datacut1$ExpectedShotValue = datacut1$percentscored*datacut1$shotFrequency

#data readin
relevantdata <- fread(file1)
relevantdata = cbind(relevantdata,rep(1,nrow(relevantdata)))
colnames(relevantdata)[length(relevantdata)]  = "Shot Count"

attach(relevantdata)
shooter = ifelse(`Shooting Team` == "home",as.character(`Home Team`), as.character(`Away Team`))
defender = ifelse(`Shooting Team` == "home",as.character(`Away Team`), as.character(`Home Team`))
threepFlag = ifelse(as.numeric(regexpr("Three", Text)) > 0, 1, 0)
pointscored = `Shot Status`*(2+threepFlag)
pointsdefended = 2 + threepFlag - pointscored
assistflag = ifelse(as.numeric(regexpr("Assisted", Text)) > 0, 1, 0)
homepositionleft = `Left Position`
homepositiontop = `Top Position`
awaypositionleft = 1-`Left Position`
awaypositiontop = 1-`Top Position`
positionleft = ifelse(`Shooting Team` == "home", homepositionleft, awaypositionleft)
positiontop = ifelse(`Shooting Team` == "home", homepositiontop, awaypositiontop)
gameID = paste(as.character(Date),`Home Team`)
year = substring(Date, nchar(as.character(Date))-4,nchar(as.character(Date)))

relevantdata = data.table(as.character(Date), year, shooter, defender, as.character(`Home Team`), as.character(`Away Team`), as.character(`Shooter Name`), as.character(`Shooter ID`), `Shot Number`, Quarter, positionleft, positiontop, awaypositionleft, awaypositiontop, threepFlag, pointscored, pointsdefended, assistflag, as.character(Text),gameID, `Shot Count`)

colnames(relevantdata)[1] = "Date"
colnames(relevantdata)[5] = "Home"
colnames(relevantdata)[6] = "Away"
colnames(relevantdata)[7] = "Shootername"
colnames(relevantdata)[8] = "ShooterID"
colnames(relevantdata)[9] = "Shot Number"
colnames(relevantdata)[19] = "Text"
colnames(relevantdata)[21] = "Shot Count"
attach(relevantdata)
detach(relevantdata)
relevantdata = na.omit(relevantdata)

####################Data formatting above
scores = as.numeric(relevantdata$pointsdefended)
relevantdata$`Shot Count` = as.numeric(relevantdata$`Shot Count`)
relevantdata$pointsdefended = ifelse(scores == 1, 0, scores)
temp_percents = relevantdata[,lapply(.SD, function(x) x/sum(x)), by = .(gameID,defender),.SDcols = "pointsdefended"]
temp_frequency = relevantdata[,.(`Shot Count` = sum(`Shot Count`)),by = .(gameID,defender,positionleft,positiontop)]
colnames(temp_frequency)[length(temp_frequency)] = "shotFrequency" 
percentdefended = temp_percents$pointsdefended
relevantdata = cbind(relevantdata, percentdefended)
relevantdata = merge(relevantdata, temp_frequency, by = c("gameID","defender","positionleft","positiontop"))

#need to update year to season
datacut2 = subset(relevantdata, pointsdefended != "0")
datacut2$ExpectedShotValue = datacut2$percentdefended*datacut2$shotFrequency


#Simulate for average differneces
#difference_sample = average_difference(teams,1000)
#difference_sample = 23.5184 #from 1000 runs of 2014:2017 data
difference_sample = 24.69855 #from 1000 runs of 2014:2017 data

team_matchups = read.csv(file2)
team_matchups$Team.One = as.character(team_matchups$Team.One)
team_matchups$Team.Two = as.character(team_matchups$Team.Two)
match_up = c(numeric(nrow(team_matchups)))
winner = c(numeric(nrow(team_matchups)))
difference = c(numeric(nrow(team_matchups)))

for (i in 1:nrow(team_matchups)){
  output = match(team_matchups$Team.One[i],team_matchups$Team.Two[i])
  match_up[i] = output[1]
  winner[i] = output[2]
  difference[i] = output[3]
}

teamOne = winner[seq(1,length(winner),2)]
teamTwo = winner[seq(2,length(winner),2)]
winner_temp = as.character(vector(length = 2))

while (length(winner_temp)>1) {
  match_up_temp = c(numeric(length(teamOne)))
  winner_temp = c(numeric(length(teamOne)))
  difference_temp = c(length(nrow(teamOne)))
  for (i in 1:length(teamOne)){
    output = match(teamOne[i],teamTwo[i])
    match_up_temp[i] = output[1]
    winner_temp[i] = output[2]
    difference_temp[i] = output[3]
  }
  teamOne = winner_temp[seq(1,ifelse(length(winner_temp)==1,1,length(winner_temp)),2)]
  teamTwo = winner_temp[seq(ifelse(length(winner_temp)==1,1,2),ifelse(length(winner_temp)==1,2,length(winner_temp)),ifelse(length(winner_temp)==1,1,2))]
  match_up = c(match_up, match_up_temp)
  winner = c(winner, winner_temp)
  difference = c(difference,difference_temp)
  }


df = data.frame(match_up,winner,as.double(difference)/difference_sample)
colnames(df) = c("MatchUp","Winner","Normalized Difference")


#write.csv(df,'BracketResults.csv', row.names = FALSE)


