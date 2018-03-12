
# coding: utf-8

# In[1]:


import pprint  
import os
import math
import numpy as np
import numpy.random as npr
import pandas as pd
import random
from scipy import stats
from scipy.stats import norm
#import seaborn as sns
#import matplotlib.pyplot as plt
from sklearn.utils.extmath import cartesian
from bs4 import BeautifulSoup
import requests
import csv
#import mechanize 
import time
import re
from collections import defaultdict
import datetime

#r = requests.get('http://www.listinga|llcars.com/results/New')
#data = r.text
#soup = BeautifulSoup(data)
#for link in sou
#p.find_all('a')



# In[3]:


#Last year season gameid http://www.espn.com/nba/game?gameId=400899695
#This year is from http://www.espn.com/nba/playbyplay?gameId=400900000
#To http://www.espn.com/nba/playbyplay?gameId=400900255

#Url for NBA
# url = 'http://www.espn.com/nba/playbyplay?gameId='
#Url for NCAAM
url = 'http://www.espn.com/mens-college-basketball/playbyplay?gameId='

#2017 NBA
# pagestart = 400900100
# pageend = 400900255

# NCAAM
# pagestart = 400910000
# pageend = 400919999
output = pd.DataFrame()

startdate = 20150126 # have to start 2 before your actual start date because of coding prep
NBAorNCAAM = 'NCAAM'


# In[4]:


###Helper functino for make or miss
def toBinary(string):
    if (string == 'made'):
        return 1
    else:
        return 0 
    
def Open_Browser(url, page):
    result=requests.get(url+str(page))
    check_status(result)
    #br.open(url+str(page))
    soup = BeautifulSoup(result.content,"html.parser")
    return soup

def check_status(r):
    if r.status_code != 200:
      print('Error with request, status code is not 200.')
    

def Open_Date_Browser(NBAorNCAAM,date):
    if NBAorNCAAM == "NBA":
        url = 'http://www.espn.com/nba/schedule/_/date/'+ str(date)+'/'
    else:
        url = 'http://www.espn.com/mens-college-basketball/schedule/_/date/'+ str(date)+'/'
    #br.open(url)
    result=requests.get(url)
    check_status(result)
    soup = BeautifulSoup(result.content,"html.parser")
    return soup

def scrape(text_team):
    shot = []
    shootername =[]
    top = []
    left = []
    status = []
    shootingteam = []
    datatext = []
    threepointer = []
    assists = []
    quarter = []
    shooterid = []
    
    for x in text_team.find_all('li'):
        status_value = str(x.get('class')[0])
        shootingteam_value = str(x.get('data-homeaway'))
        datatext_value = str(x.get('data-text'))
        quarter_value = str(x.get('data-period'))
        shooterid_value = str(x.get('data-shooter'))
        text = ('{} {}'.format(x.get('id'), x.get('style').split(';')[-3:-1]))
        text =text.split()
        shot_value = (re.search(r'\d+', text[0][4:]).group())
        left_value = float((re.search(r'\d+', text[1][4:]).group()))/100
        top_value = float((re.search(r'\d+', text[2][4:]).group()))/100
        if ('three point' or 'three') in datatext_value:
            threepointer_value = 1
        else: 
            threepointer_value = 0
        if ('(' in datatext_value and ')' in datatext_value):
            assists_value = re.search(r'\((.*?)\)',datatext_value).group(1)
        else:
            assists_value = '0'
        
        if ('assists' or 'assist') in assists_value:
            assists_value = assists_value[0:len(assists_value)-8]
        else: 
            assists_value = '0'
        if datatext_value.find('makes') != -1:
            shootername_value = datatext_value[0:datatext_value.find('makes')-1]
        elif datatext_value.find('made') != -1:
            shootername_value = datatext_value[0:datatext_value.find('made')-1]
        elif datatext_value.find('misses') != -1:
            shootername_value = datatext_value[0:datatext_value.find('misses')-1]
        elif datatext_value.find('missed') != -1:
            shootername_value = datatext_value[0:datatext_value.find('missed')-1]
        elif ('blocks' in datatext_value) or ('block' in datatext_value):
            a = datatext_value.rindex('blocks')
            b = datatext_value.rindex("'")
            shootername_value = datatext_value[a+len('blocks')+1:b]
        while shootername_value[len(shootername_value)-1]==' ':
            shootername_value = shootername_value[0:len(shootername_value)-1]
            
        shootername.append(shootername_value)
        shootingteam.append(shootingteam_value)
        shot.append(shot_value)
        top.append(top_value)
        left.append(left_value)
        status.append(toBinary(status_value))
        datatext.append(datatext_value)
        threepointer.append(threepointer_value)
        assists.append(assists_value)
        quarter.append(quarter_value)
        shooterid.append(shooterid_value)
    
    shootername = np.array(shootername)
    shootingteam = np.array(shootingteam)
    shot = np.array(shot)
    top = np.array(top)
    left = np.array(left)
    status = np.array(status)
    datatext = np.array(datatext)
    threepointer = np.array(threepointer)
    assists = np.array(assists)
    shooterid = np.array(shooterid)
    quarter = np.array(quarter)

    return shootingteam, shot, top, left, status, datatext, threepointer,assists, shootername, quarter, shooterid

def scrapeDate(soup):
    try:
        date = soup.find('div',{'class' : 'cscore_date-time'}).get('data-date')[0:10]
    except:
        try:
            title = soup.title.text
            date = title[title.find("Play -")+len("Play -")+1:title.find("- ESPN")-1]
        except:
            date = "Error in Scraping Date"
    return date

def scrapeTeams(soup):
    try:
        teams =  soup.find_all('span',{'class' : 'cscore_name cscore_name--long'})
        home_team = teams[1].contents
        away_team = teams[0].contents
    except:
        try:
            title = soup.title.text
            away_team = title[0:title.find("vs.")-1]
            home_team = title[title.find("vs.")+4:title.find("- Play-By-Play")-1]
        except:
            home_team = "Error in Team Scrape"
            away_team = "Error in Team Scrape"
    return home_team, away_team

#Download Game IDs
def ScrapeGameID(date,NBAorNCAAM):
    gameID = []
    print('Im in ScrapeGameID function')
    soup = Open_Date_Browser(NBAorNCAAM, date)
    if ("No games scheduled" in soup.text) == False:
        test = soup.find('section', { "class" : "col-a" })
        for row in test.find_all('a'):
            if 'gameId' in row.get('href'):
                href = row.get('href')
                gameID.append(href[-9:])
    return gameID

def int2Date(startdate):
    date = datetime.datetime(int(str(startdate)[0:4]),int(str(startdate)[4:6]),int(str(startdate)[6:8]))
    return date

    
def incrementDate(date):
    date = date + datetime.timedelta(days=1)
    a = datetime.date(date.year,date.month,date.day)
    a = str(a)
    a = a.replace("-","")
    return a, date


# In[5]:


# soup = Open_Browser(url, 400917636)
# print soup
# #print soup.find_all('data-date')
# # print soup.find('div',{'class' : 'cscore_date-time'}).get('data-date')[0:10]

# # for a in soup.find_all('div',{'class' : 'cscore_date-time'}):
# #     print a.get('data-date')


# In[6]:


# date = datetime.datetime(2016,12,22)
# print type(date)

# for i in range(50): 
#     date += datetime.timedelta(days=1)
#     a = datetime.date(date.year,date.month,date.day)
#     a = str(a)
#     a = a.replace("-","")
#     print(a) 
# print date
# print str(startdate)[0:4]
# print str(startdate)[4:6]
# print str(startdate)[6:8]


# In[9]:


# print startdate
# startdate = 2014010
# t0, t1 = incrementDate(int2Date(startdate))
# print t0
# gameID = ScrapeGameID(t0,NBAorNCAAM)
# print gameID

int(str(datetime.date.today()).replace("-",''))-1


# In[ ]:


t0, t1 = incrementDate(int2Date(startdate))
while int(t0) != int(str(datetime.date.today()).replace("-",''))-1:
    t0, t1 = incrementDate(t1)
    try:
        gameID = ScrapeGameID(t0,NBAorNCAAM)
    except:
        try:
            print('error')
            time.sleep(150)
            gameID = ScrapeGameID(t0,NBAorNCAAM)
        except:
                try:
                    print('error 2')
                    time.sleep(300)
                    gameID = ScrapeGameID(t0,NBAorNCAAM)
                except:
                    print('error 3')
                    time.sleep(600)
                    gameID = ScrapeGameID(t0,NBAorNCAAM)
    print(t0)
    for page in range(0,len(gameID)):
        try:
            soup = Open_Browser(url, gameID[page])
            date = scrapeDate(soup)
            home_team, away_team = scrapeTeams(soup)
    
            text_home_team = soup.find('ul', { "class" : "shots home-team" })
            text_away_team = soup.find('ul', { "class" : "shots away-team" })
            if text_home_team == None or text_away_team == None:
                continue

            shootingteam,shot,top,left,status, datatext, threepointer,assists, shootername,quarter, shooterid = scrape(text_home_team)
            HomeTeam = pd.DataFrame({'Date' : str(date),'Shooting Team': shootingteam,'Shooter Name' : shootername,'Shooter ID':shooterid,'Shot Number':shot,
                                     'Quarter':quarter,'Left Position': left,'Top Position': top,'Home Team': home_team,
                                    'Away Team': away_team,'Shot Status': status,'Three Pointer': threepointer,
                                     'Assists': assists,'Text':datatext})

            shootingteam,shot,top,left,status,datatext,threepointer,assists, shootername,quarter, shooterid = scrape(text_away_team)
            AwayTeam = pd.DataFrame({'Date' : str(date),'Shooting Team': shootingteam,'Shooter Name' : shootername,'Shooter ID':shooterid,'Shot Number':shot,
                                     'Quarter':quarter,'Left Position': left,'Top Position': top,'Home Team': home_team,
                                    'Away Team': away_team,'Shot Status': status,'Three Pointer' : threepointer,
                                     'Assists': assists,'Text':datatext})

            HomeTeam = HomeTeam[['Date','Shooting Team','Shooter Name','Shooter ID','Shot Number','Quarter','Left Position','Top Position','Home Team','Away Team',
                                 'Shot Status','Three Pointer','Assists', 'Text']]
            AwayTeam = AwayTeam[['Date','Shooting Team','Shooter Name','Shooter ID','Shot Number','Quarter','Left Position','Top Position','Home Team','Away Team',
                                 'Shot Status','Three Pointer','Assists','Text']]

            frames = [HomeTeam, AwayTeam]
            current_output = pd.concat(frames)
            output = output.append(current_output)
        except Exception as e: 
            if 'forcibly closed' in str(e):
                time.sleep(300)
                page = page - 1
            else: 
                print(str(e) + str(gameID[page]))
    #     except ValueError:
    #         print "Error on page " + str(page)
    #     except:
    #         print "Other Error"
print("Done")


# In[ ]:


# s = "Andre 'Drummond' b'locks 'Garrett Temple 's 2-foot  layup"

# 'blocks' in s
# b = s.rindex("'")

# print b


# In[ ]:


# print output


# In[ ]:


#############################Write Dataframe to .csv
#output.to_csv('C:\Users\gordon.tsai\Google Drive\MarchMadnessScrape.csv', sep = ',', encoding = 'utf-8' , index = False)
try:
  output.to_csv(os.path.join(os.path.join(os.getcwd(),'output'),'marchmadness'+str(startdate)+'.csv'), index = False)
  print('first try worked')
except:
  output.to_csv(os.path.join(os.path.join(os.getcwd(),'output'),'marchmadness'+str(startdate)+'.csv'), sep = ',', encoding = 'utf-8' , index = False)
  print('second except worked')

