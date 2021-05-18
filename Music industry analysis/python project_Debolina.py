# -*- coding: utf-8 -*-
"""
Created on Mon Apr  5 15:15:00 2021

@author: Debolina
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
grammy=pd.read_csv("grammyAlbums_199-2019.csv")
attribute=pd.read_csv("songAttributes_1999-2019.csv")

df=grammy['Album']
df2=attribute['Album']

df=pd.read_csv("updated_attributes_csv.csv")
            

##Merging data

attribute['won grammy'] = attribute['Album' and 'Artist'].isin(grammy['Album' and 'Artist']).astype(object)  

attribute.to_csv(r'C:\Users\Debolina\Desktop\python mism 6212\updated_attributes_csv.csv', index = False)




##drop irrelevant columns
#Time-signature — Almost all the songs seem to have a common time-signature of 4/4. Hence, I dropped this parameter
#Unnamed:0 makes no sense
df=df.drop(["Unnamed: 0", "TimeSignature", "Album", "Artist", "Name"], axis=1)




from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
df_scaled = sc.fit_transform(df)
pd.DataFrame(df_scaled)

## Elbow to find optimal no. of clusters for K-Means
from sklearn.cluster import KMeans

wcss = []
for i in range(1, 11):
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(df_scaled)
    wcss.append(kmeans.inertia_)

plt.plot(wcss, 'bx-')
plt.xlabel('Clusters')
plt.ylabel('Scores WCSS')



kmeans = KMeans(3)
kmeans.fit(df_scaled)
labels = kmeans.labels_
labels


df_cluster = pd.concat([df, pd.DataFrame({'cluster': labels})], axis = 1)
df_cluster.head()


for i in df.columns:
    plt.figure(figsize = (35, 5))
    for j in range(3):
        plt.subplot(1, 3 , j+1)
        cluster = df_cluster[df_cluster['cluster'] == j]
        cluster[i].hist(bins = 20)
        plt.title('{} \nCluster {}'.format(i, j))

plt.show()


df0 = df_cluster[df_cluster['cluster'] == 0]
df1 = df_cluster[df_cluster['cluster'] == 1]
df2 = df_cluster[df_cluster['cluster'] == 2]



## wordcloud
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
STOPWORDS.add("featuring")
STOPWORDS.add("songwriter")
STOPWORDS.add("nan")
STOPWORDS.add("artist")
STOPWORDS.add("the")
stopwords = set(STOPWORDS)

gr_songs=pd.read_csv("grammySongs_1999-2019.csv")
import random
gr_songs['Artist'] = gr_songs['Artist'].str.replace(" &", ",")
gr_songs['Artist'] = gr_songs['Artist'].str.title()
#grammy
def green_color_func(word, font_size, position, orientation, random_state=None,
                    **kwargs):
    return "hsl(140, 25%%, %d%%)" % random.randint(1, 60)
_words = '' 
  
# iterate through the csv file 
for val in gr_songs.Artist: 
      
    # typecaste each val to string 
    val = str(val)
  
    # split the value 
    tokens = val.split()
      
    _words += " ".join(tokens)+" "
  
wordcloud = WordCloud(width = 800, height = 800,
                background_color ='white',
                stopwords = stopwords,
                min_font_size = 10,random_state=1).generate(_words)

# plot the WordCloud image
plt.figure(figsize = (8, 8))
plt.imshow(wordcloud.recolor(color_func=green_color_func, random_state=3),
           interpolation="bilinear")
plt.axis("off")
plt.tight_layout(pad = 0)
plt.show()

"""didn't include this"""

##for eda:
bill=pd.read_csv("billboardHot100_1999-2019.csv")
bill["Date"]= bill["Date"].str.split(' ').str[0]

date=bill["Date"]
bill["Date"].value_counts().plot()
ax=sns.countplot(x="Date", data=bill, order = bill["Date"].value_counts().index)
ax.set(xlabel='Month')
plt.xticks(rotation=45)




##eda
spot=pd.read_csv("spotify.csv")
q=(spot.groupby('year').mean()['duration_ms']/60000).plot()
q.set_ylabel('Duration in minutes')


## Audio characteristics over year
plt.figure(figsize=(16, 10))
sns.set(style="whitegrid")
columns = ["acousticness","danceability","energy","speechiness","liveness", "valence"]
for col in columns:
    x = spot.groupby("year")[col].mean()
    ax= sns.lineplot(x=x.index,y=x,label=col)
ax.set_title('Trend of Audio characteristics over the years')
ax.set_ylabel('Measure')
ax.set_xlabel('Year')

#for tempo
plt.figure(figsize=(16, 8))
sns.set(style="whitegrid")
columns = ["tempo"]
for col in columns:
    x = spot.groupby("year")[col].mean()
    ax= sns.lineplot(x=x.index,y=x,label=col)
ax.set_ylabel('Count')
ax.set_xlabel('Year')


    
    
    
    
    
    
    
    
    
    
    


