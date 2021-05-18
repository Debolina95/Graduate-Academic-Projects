#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr  4 21:13:09 2021

@author: lichangtan
"""

import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
import seaborn as sns

'''
     this dataset is for 2020
'''

# read the file 
spotify2020 = pd.read_csv("spotify2020.csv")
print(spotify2020.info()) # there are missing values


# identify the positions of missing values
null_data = spotify2020[spotify2020.isnull().any(axis=1)]
null_data

# since there are only four missing values, we can remove them
spotify2020 = spotify2020.dropna()
print(spotify2020.info())

# groupby by song names and sum the streams
spotify2020 = spotify2020.groupby(["Track Name","Artist"], as_index=False)[
    "Streams"].sum()

# get the top 100 songs in 2020
spotify2020 = spotify2020.sort_values(by='Streams', ascending=False)

spotify2020Top = spotify2020.head(n=100)

# identify the genre of each song
spotify2020Top['Genres'] = ['Synthwave,R&B/Soul','Dance/Electronic','Dance/Electronic',
'Alternative/Indie','Nu-disco','Hip-Hop/Rap','Pop','Hip-Hop/Rap','Hip-Hop/Rap',
'Hip-Hop/Rap','Alternative/Indie','Alternative/Indie','Pop','Pop','R&B','Dance/Electronic',
'Contemporary R&B/Pop','Dance-pop disco-funk','Alternative/Indie','R&B/Soul',
'Pop , Hip-Hop/Rap','Pop','Pop','R&B/Soul','Pop','Dance/Electronic','Pop',
'New wave/post-punk','Pop','Hip-Hop/Rap','Pop,R&B/Soul','Hip-Hop/Rap','Alternative/Indie',
'Contemporary R&B, Pop','Pop','Hip-Hop/Rap','Alternative/Indie','Reggaeton','Hip-Hop/Rap',
'Reggaeton','Hip-Hop/Rap','Pop','Pop , Hip-Hop/Rap','Pop , Hip-Hop','Rock','pop, reggaeton',
'Hip hop','Alternative/Indie','Alternative/Indie','Hip-Hop/Rap','Hip-Hop/Rap','Reggaeton, ‎dembow',
'Hip-Hop/Rap','Pop','Dance/Electronic','Pop','Hip-Hop/Rap','Pop','Latin pop, ‎reggaeton','house, pop,reggaeton',
'Pop','Pop','Country music','Folk music','Hip-Hop/Rap','Pop','Hip-Hop/Rap','Hip-Hop/Rap',
'Pop','Reggaeton','Pop','Hip-Hop/Rap','Pop','Pop','Hip-Hop/Rap','Pop rock','Hip-Hop/Rap',
'Pop','Pop','Electropop','Pop','Soul','Pop','EDM','R&B/Soul','Hip-Hop/Rap',
'Emo rap, Lo-fi music, Alternative hip hop, Hip-Hop/Rap','Reggaeton','Pop','Hip hop music, Rhythm, blues, Trap music, Pop',
'Hip-Hop/Rap','Reggaeton','Pop music, Pop','Alternative/Indie','Dance/Electronic','Pop',
'Hip-Hop/Rap','Pop music, Rock','Pop','Classic rock, Rock']

# use a bar plot to plot the top 50 songs 

plt.figure(figsize=(12,12))
sns.barplot(x='Streams',y='Track Name',data=spotify2020Top[0:30],hue='Genres')
plt.ticklabel_format(axis='x', style='plain')
plt.xticks(rotation = 0)
plt.xlabel('Streams', fontsize=18)
plt.ylabel('Name', fontsize=18)
plt.title('Top 30 songs from Spotify in 2020', fontsize=20)
plt.show()



'''
    one way is to count the genre
'''
spotify2020Top = spotify2020Top['Genres'].value_counts().reset_index(name='Count')


## visualization for 2020

plt.figure(figsize=(15,15))
#plt.bar(x='Genres',y='Streams',data=spotify2020Top)
sns.barplot(x='Count',y='index',data=spotify2020Top)
plt.ticklabel_format(axis='x', style='plain')
plt.xticks(rotation = 0)
plt.xlabel('Count', fontsize=18)
plt.ylabel('Genres', fontsize=18)
plt.title('Genres of Top 100 songs from Spotify in 2020', fontsize=20)
plt.show()
#from the result i think 

'''
     second way is to sum the streams based on genres 2020 
'''

# groupby by genres and sum the streams
spotify2020Top = spotify2020Top.groupby(["Genres"], as_index=False)[
    "Streams"].sum()

spotify2020Top = spotify2020Top.sort_values(by='Streams', ascending=False)

## visualization for 2020

plt.figure(figsize=(15,15))
#plt.bar(x='Genres',y='Streams',data=spotify2020Top)
sns.barplot(x='Streams',y='Genres',data=spotify2020Top)
plt.ticklabel_format(axis='x', style='plain')
plt.xticks(rotation = 0)
plt.xlabel('Streams', fontsize=18)
plt.ylabel('Genres', fontsize=18)
plt.title('Genres of Top 100 songs based on streams from Spotify in 2020', fontsize=20)
plt.show()
#from the result i think 


'''
     this dataset is for 2019
'''


### import another dataset 
pd.set_option('max_columns', 100)
pd.set_option('max_rows', 100)
spotify = pd.read_csv("spotifyWeeklyTop200Streams.csv")
print(spotify.info()) # there are missing values

# convert the week column to datetime 

spotify['Week'] = pd.to_datetime(spotify['Week'])

# add year as a new column
spotify['Year'] = spotify['Week'].dt.year

'''
    2018
'''

# subset year in 2018
spotify2018 = spotify[spotify['Year']==2018]

# get the top 100 songs in 2018
spotify2018 = spotify2018.sort_values(by='Streams', ascending=False)

spotify2018Top = spotify2018.head(n=100)

'''
    2019
'''

# subset year in 2019
spotify2019 = spotify[spotify['Year']==2019]

# get the top 100 songs in 2019
spotify2019 = spotify2019.sort_values(by='Streams', ascending=False)


# groupby by song names and sum the streams 2019 
spotify2019 = spotify2019.groupby(["Name","Artist"], as_index=False)[
    "Streams"].sum()

# get the top 100 songs in 2020
spotify2019 = spotify2019.sort_values(by='Streams', ascending=False)

spotify2019Top = spotify2019.head(n=100)


spotify2019Top['Genres'] =['Pop music, Hip-Hop/Rap',
'Hip hop music, Rhythm and blues, Trap music, Pop','Hip-Hop/Rap',
'Hip-Hop/Rap','Hip-Hop/Rap','Alternative/Indie','Hip-Hop/Rap',
'Country rap/Pop','Hip-Hop/Rap','Pop','Hip-Hop/Rap','Hip-Hop/Rap',
'Hip-Hop/Rap','Pop','Pop','R&B/Soul','Hip-Hop/Rap',
'Country rap/Pop','Pop','Hip-Hop/Rap','Alternative/Indie','Pop',
'Dance/Electronic','Hip-Hop/Rap','Contemporary R&B','Hip-Hop/Rap','Hip-Hop/Rap',
'Hip-Hop/Rap','Pop','Pop','Hip-Hop/Rap','Hip-Hop/Rap','Pop music/Rock',
'Hip-Hop/Rap','R&B/Soul','Hip-Hop/Rap','Trap music/Hip-Hop/Rap','Hip-Hop/Rap',
'Country music','Alternative rock/Pop rock/Pop','Hip-Hop/Rap','Rock',
'Hip hop music/Cloud rap/ Hip-Hop/Rap','Pop music/Alternative/Indie',
'Hip-Hop/Rap','Pop','Trap music, Cloud rap, Pop','Hip hop/‎trap',
'Pop','Emo rap, Lo-fi music, Alternative hip hop, Hip-Hop/Rap',
'Pop','Reggaeton','Hip-Hop/Rap','Hip-Hop/Rap','Jazz','Hip-Hop/Rap','Latin trap',
'Hip-Hop/Rap','Pop','Alternative/Indie','Hip-Hop/Rap','Hip-Hop/Rap',
'Contemporary R&B, Alternative hip hop, Hip-Hop/Rap','Alternative/Indie',
'Contemporary R&B','R&B/Soul','Hip-Hop/Rap','Hip-Hop/Rap','Pop',
'Pop rock, Pop','Hip-Hop/Rap','Classic rock, Rock','Trap music, Hip-Hop/Rap',
'Country','Pop','Alternative/Indie','Trap music, Pop music, Pop rap, Hip-Hop/Rap',
'Hip-Hop/Rap','R&B/Hip-Hop/Soul','Hip-Hop/Rap','Rock, Pop','Alternative/Indie',
'Hip hop emo rap cloud rap','Pop','Pop','Hip-Hop/Rap','Pop','Emo rap, Hip-Hop/Rap',
'Country pop, Contemporary R&B, Pop','Hip-Hop/Rap','R&B/Soul','R&B/Soul',
'Rock','Dance/Electronic','Pop','Alternative/Indie','Pop','Rock',
'Bubblegum pop/‎synth-pop','Hip-Hop/Rap']


# use a bar plot to plot the top 30 songs 

plt.figure(figsize=(12,12))
sns.barplot(x='Streams',y='Name',data=spotify2019Top[0:30],hue='Genres')
plt.ticklabel_format(axis='x', style='plain')
plt.xticks(rotation = 0)
plt.xlabel('Streams', fontsize=18)
plt.ylabel('Name', fontsize=18)
plt.title('Top 30 songs from Spotify in 2019', fontsize=20)
plt.show()


'''
one way is to count the genres 2019
'''
spotify2019Top = spotify2019Top['Genres'].value_counts().reset_index(name='Count')


## visualization for 2019

plt.figure(figsize=(15,15))
#plt.bar(x='Genres',y='Streams',data=spotify2020Top)
sns.barplot(x='Count',y='index',data=spotify2019Top)
plt.ticklabel_format(axis='x', style='plain')
plt.xticks(rotation = 0)
plt.xlabel('Count', fontsize=18)
plt.ylabel('Genres', fontsize=18)
plt.title('Genres of Top 100 songs from Spotify in 2019', fontsize=20)
plt.show()
#from the result i think 


'''
second way is to count the streams based on genres 2019 
'''
# groupby by genres and sum the streams
spotify2019Top = spotify2019Top.groupby(["Genres"], as_index=False)[
    "Streams"].sum()

spotify2019Top = spotify2019Top.sort_values(by='Streams', ascending=False)

## visualization for 2019

plt.figure(figsize=(15,15))
#plt.bar(x='Genres',y='Streams',data=spotify2020Top)
sns.barplot(x='Streams',y='Genres',data=spotify2019Top)
plt.ticklabel_format(axis='x', style='plain')
plt.xticks(rotation = 0)
plt.xlabel('Streams', fontsize=18)
plt.ylabel('Genres', fontsize=18)
plt.title('Genres of Top 100 songs based on streams from Spotify in 2019', fontsize=20)
plt.show()






























