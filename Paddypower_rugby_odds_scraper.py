

#Import Selenium
from selenium import webdriver
import pandas as pd
import time
import requests
from bs4 import BeautifulSoup 
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import ElementClickInterceptedException


#Writing our First Selenium Python Test
web = 'https://www.paddypower.com/rugby-union?tab=popular' #you can choose any other league (update 1)
path = '/Users/manu/Desktop/chromedriver'
driver = webdriver.Chrome(path)
driver.get(web)

#Make ChromeDriver click a button
time.sleep(8) #add implicit wait, if necessary
accept = driver.find_element_by_xpath('//*[@id="onetrust-accept-btn-handler"]')
accept.click()

#Initialize your storage
team_names = []
player_names = []
odds_events = []

time.sleep(2) #add implicit wait, if necessary
box = driver.find_element_by_xpath('/html/body/page-container/div/main/div/content-managed-page/div/div[2]/div/div[2]') #last number controls which box to choose
box.location_once_scrolled_into_view

for i in range(1, 5):
    time.sleep(2) #add implicit wait, if necessary
    for team in box.find_elements_by_class_name('avb-item__event-row'):
        if team.text:
            team_names.append(team.text)
        elif not team.text:
            print('no more teams in featured matches to append')
    
    time.sleep(2)
    match = box.find_element_by_xpath('/html/body/page-container/div/main/div/content-managed-page/div/div[2]/div/div[2]/coupon-card/div/abc-card/div/div/abc-card-content/div/avb-coupon/div/avb-coupon-card-items/div[' + str(i) + ']/ng-switch/div/div[' + str(i) + ']/ng-switch/div/avb-item/div/div/a')
    match.click()
    
    time.sleep(2) #add implicit wait, if necessary
    player_markets = driver.find_element_by_xpath('/html/body/page-container/div/main/div/event-page/div/main/div/abc-strip/div/nav/abc-tab-bar/nav/div[4]/scoped-transclude/abc-tab-bar-item/abc-tab/a')
    player_markets.click()
    
    time.sleep(2)
    close_box = driver.find_element_by_xpath('/html/body/page-container/div/main/div/event-page/div/main/div/div[3]/div/div[1]/card-event/div/abc-card[1]/div/div/abc-card-content/abc-accordion/div/div') 
    close_box.click()
    
    time.sleep(2)
    anytime_tryscorer_header = driver.find_element_by_xpath('/html/body/page-container/div/main/div/event-page/div/main/div/div[3]/div/div[1]/card-event/div/abc-card[2]/div/div/abc-card-content/abc-accordion/div/div') 
    anytime_tryscorer_header.click()
    
    time.sleep(2)
    anytime_tryscorer_box = driver.find_element_by_xpath('/html/body/page-container/div/main/div/event-page/div/main/div/div[3]/div/div[1]/card-event/div/abc-card[2]/div/div/abc-card-content/abc-accordion/div/section') 
    
    for player in anytime_tryscorer_box.find_elements_by_class_name('outright-item__runner-name'):
        if player.text:
            player_names.append(player.text)
        elif not player.text:
            print('no more players to append for this market')
            
    for odds in anytime_tryscorer_box.find_elements_by_class_name('btn-odds__label'):
        if odds.text:
            odds_events.append(odds.text)
        elif not odds.text:
            print('no more odds to append for this market')
    
    time.sleep(2)
    driver.back()
    time.sleep(2)
    
    
driver.quit()

  

team_names = pd.DataFrame(team_names)
team_names.columns = ['teams']
team_names['teams'] = team_names['teams'].str.replace('[0-9]', '')
team_names['teams'] = team_names['teams'].str.replace('/', '')




rugby = {'players': player_names, 'odds': odds_events}
df = pd.DataFrame.from_dict(rugby)
print(df)
    

#Initialize your storage
player_stats = []


for i in player_names:
    
    #Writing our First Selenium Python Test
    web = 'https://www.google.com/search?q=' + i + ' espn' #you can choose any other league (update 1)
    path = '/Users/manu/Desktop/chromedriver'
    driver = webdriver.Chrome(path)
    driver.get(web)
    
    #Make ChromeDriver click a button
    time.sleep(2) #add implicit wait, if necessary
    try:
        accept = driver.find_element_by_xpath('/html/body/div[3]/div[3]/span/div/div/div[3]/button[2]/div')
        accept.click()
    except NoSuchElementException:
        try:
            accept = driver.find_element_by_xpath('/html/body/div[3]/div[3]/span/div/div/div/div[3]/button[2]/div')
            accept.click()
        except NoSuchElementException:
            try:
                accept = driver.find_element_by_xpath('/html/body/c-wiz/div/div/div/div[2]/div[1]/div[4]/form/div[1]/div/button')
                accept.click()
            except NoSuchElementException:
                pass   


    time.sleep(1) #add implicit wait, if necessary
    try:
        result = driver.find_elements_by_xpath("//*[@id='rso']/div[1]/div/div/div[1]/a")  #might need to get full xpath
        result[0].click()
    except IndexError:
        pass
        
    time.sleep(1)
    try:
        player_stats_box = driver.find_element_by_xpath('/html/body/div[4]/div/div/div[1]/div[2]/div/table[2]') #last number controls which box to choose
        for stats in player_stats_box.find_elements_by_class_name('data1'):
            if stats.text:
                player_stats.append(stats.text)
                print('got stats for: ' + i + ' player ' + str(player_names.index(i)) + ' out of: ' + str(len(player_names)))
            elif not stats.text:
                print('no more stats to append for this player')
    except NoSuchElementException:
        print('no stats found for: ' + i)
        player_stats.append(i)
        pass

    driver.quit()
    
    




player_stats = pd.DataFrame(player_stats)
player_stats.columns = ['stats']

player_stats['stats'] = player_stats['stats'].str.replace('All Tests ', '')
player_stats = player_stats[~player_stats['stats'].str.contains("[a-zA-Z]")]


player_stats = pd.DataFrame(player_stats['stats'].str.split(' ', 13).tolist(), 
                    columns = ['Span','Mat','Start','Sub',
                               'Pts','Tries','Conv','Pens',
                               'Drop','Won','Lost','Draw','pct'])



no_player_stats = []
no_player_stats.append(df['players'][6])
no_player_stats.append(df['players'][8])
no_player_stats.append(df['players'][32])
no_player_stats.append(df['players'][38])
no_player_stats.append(df['players'][43])


for i in no_player_stats:
    df = df[~df['players'].str.contains(i)]

print(df)

df.dropna(inplace=True)
df.reset_index(drop=True, inplace=True)

player_stats = player_stats.iloc[:, 1:].astype(float)

df = df.join(player_stats)



       
from fractions import Fraction

df['odds'] = df['odds'].str.replace('EVS', '1/1')

decimals = []
for i in df.odds:
    decimals.append(int(Fraction(i)))

df['returns'] = decimals 
df['returns'] = df['returns'] * 10
df['market_is'] = df['returns'] + df['returns']
df['probs'] = (df.Tries / df.Mat) * 100
df['market_should_be'] = (df['returns'] * 10 / df['probs']) * 10
df['tradeable'] = df['market_is'] > df['market_should_be']
df['diff'] = df['market_is'] - df['market_should_be']
df['2xtradeable'] = df['market_is'] > df['market_should_be'] * 2






df.to_csv("/Users/manu/Desktop/player_names_odds.csv")
player_stats.to_csv("/Users/manu/Desktop/player_stats.csv")
df.to_csv("/Users/manu/Desktop/player_names_odds_stats.csv")





df = pd.read_csv("/Users/manu/Desktop/player_names_odds.csv")
player_stats = pd.read_csv("/Users/manu/Desktop/player_stats.csv")
df = pd.read_csv("/Users/manu/Desktop/player_names_odds_stats.csv")

