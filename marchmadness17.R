#data packages
library("reshape")
library("dplyr")
library("data.table")

##############################################Helper functions
match <- function(teamOne, teamTwo){
  a = grid_sum(datacut1, teamOne,'offense')
  b = grid_sum(datacut2,teamTwo,'defense')
  d = grid_sum(datacut1,teamTwo ,'offense')
  e = grid_sum(datacut2, teamOne,'defense')
  resultOne = sum(a-b)
  resultTwo = sum(d-e)
  if (resultOne> resultTwo){
    return(teamOne)
  } else if (resultTwo > resultOne) {
    return(teamTwo)
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
#relevantdata <- fread("MarchMadnessScrape 20170309gt.csv")
#teams = unique(c(unique(relevantdata$`Home Team`), unique(relevantdata$`Away Team`)))
#write.csv(teams,"TeamList.csv", row.names = FALSE)

relevantdata <- fread("MarchMadnessScrape 20170309gt.csv")
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
relevantdata <- fread("MarchMadnessScrape 20170309gt.csv")
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