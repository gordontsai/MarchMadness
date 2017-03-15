#data readin
#relevantdata <- read.csv("~/Personal/NCAAMM2017/1417data.csv")
relevantdata <- read.csv("MarchMadnessScrape 20170309gt.csv")

attach(relevantdata)
shooter = ifelse(Shooting.Team == "home",as.character(Home.Team), as.character(Away.Team))
defender = ifelse(Shooting.Team == "home",as.character(Away.Team), as.character(Home.Team))
threepFlag = ifelse(as.numeric(regexpr("Three", Text)) > 0, 1, 0)
pointscored = Shot.Status*(2+threepFlag)
pointsdefended = 2 + threepFlag - pointscored
assistflag = ifelse(as.numeric(regexpr("Assisted", Text)) > 0, 1, 0)
homepositionleft = Left.Position
homepositiontop = Top.Position
awaypositionleft = 1-Left.Position
awaypositiontop = 1-Top.Position
positionleft = ifelse(Shooting.Team == "home", homepositionleft, awaypositionleft)
positiontop = ifelse(Shooting.Team == "home", homepositiontop, awaypositiontop)
gameID = paste(as.character(Date),Home.Team)
year = substring(Date, nchar(as.character(Date))-4,nchar(as.character(Date)))

relevantdata = data.frame(cbind(as.character(Date), year, shooter, defender, as.character(Home.Team), as.character(Away.Team), as.character(Shooter.Name), as.character(Shooter.ID), Shot.Number, Quarter, positionleft, positiontop, awaypositionleft, awaypositiontop, threepFlag, pointscored, pointsdefended, assistflag, as.character(Text)),gameID)
colnames(relevantdata)[1] = "Date"
colnames(relevantdata)[5] = "Home"
colnames(relevantdata)[6] = "Away"
colnames(relevantdata)[7] = "Shootername"
colnames(relevantdata)[8] = "ShooterID"
colnames(relevantdata)[19] = "Text"
attach(relevantdata)
detach(relevantdata)
gamefloorpoints = c()
relevantdata = na.omit(relevantdata)

for(i in 1:length(relevantdata[,1]))
{
  datacut1 = subset(relevantdata, gameID == gameID[i] & shooter == shooter[i])
  scores = as.numeric(datacut1$pointscored)
  scores = ifelse(scores == 1, 0, scores)
  gamefloorpoints = c(gamefloorpoints, sum(scores))
  if (length(gamefloorpoints) %% 1000 == 0 ){
    print(length(gamefloorpoints)) #status update
  }
}

percentscored = pointscored/gamefloorpoints

relevantdata = cbind(relevantdata, percentscored)

#need to update year to season
datacut1 = subset(relevantdata, pointscored != "0")
pivot1 = aggregate(as.numeric(percentscored) ~ gameID + shooter + positionleft + positiontop + year, datacut1, FUN = sum)

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

#data readin
#relevantdata <- read.csv("~/Personal/NCAAMM2017/1417data.csv")
relevantdata <- read.csv("MarchMadnessScrape 20170309gt.csv")

attach(relevantdata)
shooter = ifelse(Shooting.Team == "home",as.character(Home.Team), as.character(Away.Team))
defender = ifelse(Shooting.Team == "home",as.character(Away.Team), as.character(Home.Team))
threepFlag = ifelse(as.numeric(regexpr("Three", Text)) > 0, 1, 0)
pointscored = Shot.Status*(2+threepFlag)
pointsdefended = 2 + threepFlag - pointscored
assistflag = ifelse(as.numeric(regexpr("Assisted", Text)) > 0, 1, 0)
homepositionleft = Left.Position
homepositiontop = Top.Position
awaypositionleft = 1-Left.Position
awaypositiontop = 1-Top.Position
positionleft = ifelse(Shooting.Team == "home", homepositionleft, awaypositionleft)
positiontop = ifelse(Shooting.Team == "home", homepositiontop, awaypositiontop)
gameID = paste(as.character(Date),Home.Team)
year = substring(Date, nchar(as.character(Date))-4,nchar(as.character(Date)))

relevantdata = data.frame(cbind(as.character(Date), year, shooter, defender, as.character(Home.Team), as.character(Away.Team), as.character(Shooter.Name), as.character(Shooter.ID), Shot.Number, Quarter, positionleft, positiontop, awaypositionleft, awaypositiontop, threepFlag, pointscored, pointsdefended, assistflag, as.character(Text)),gameID)
colnames(relevantdata)[1] = "Date"
colnames(relevantdata)[5] = "Home"
colnames(relevantdata)[6] = "Away"
colnames(relevantdata)[7] = "Shootername"
colnames(relevantdata)[8] = "ShooterID"
colnames(relevantdata)[19] = "Text"
attach(relevantdata)
detach(relevantdata)
gamefloordefense = c()
relevantdata = na.omit(relevantdata)

for(i in 1:length(relevantdata[,1]))
{
  datacut1 = subset(relevantdata, gameID == gameID[i] & defender == defender[i])
  scores = as.numeric(datacut1$pointsdefended)
  scores = ifelse(scores == 1, 0, scores)
  gamefloordefense = c(gamefloordefense, sum(scores))
  print(length(gamefloordefense)) #status update
}

percentdefended = pointsdefended/gamefloordefense

relevantdata = cbind(relevantdata, percentdefended)

#need to update year to season
datacut1 = subset(relevantdata, pointdefended != "0")
pivot1 = aggregate(as.numeric(percentdefended) ~ gameID + shooter + positionleft + positiontop + year, datacut1, FUN = sum)

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

offensiveplot("Butler", pivot1, 2016)
defensiveplot("Butler", pivot1, 2016)