# -*- coding: utf-8 -*-
"""
Created on Mon Apr 12 11:07:51 2021

@author: jq556
"""


import pandas as pd

# =============================================================================
# Cleaning and Feature selection
# =============================================================================
df_ = pd.read_csv('grammySongs_1999-2019.csv')


col_names = ['Name', 'Artist', 'Genre', 'GrammyAward']
df = df_[col_names]
df = df.dropna()
df = df.drop_duplicates(subset='Name')

cleaning_cols = ['Artist', 'GrammyAward']
for i in cleaning_cols:
    df[i] = df[i].str.replace('Featuring', "")
    df[i] = df[i].str.lower()
    df[i] = df[i].str.replace(' ', "")
    df[i] = df[i].str.replace('&', " ")
    df[i] = df[i].str.replace('.', "")
    df[i] = df[i].str.replace(';', "")
    df[i] = df[i].str.replace(',', " ")
    df[i] = df[i].str.replace('/', " ")
    
cleaning_cols = ['Genre']
for i in cleaning_cols:
    df[i] = df[i].str.lower()
    df[i] = df[i].str.replace(' ', "")
    df[i] = df[i].str.replace('&', "")
    df[i] = df[i].str.replace('/', " ")
    
#make soup
def create_soup(x):
    s = ''
    return s.join(x['Artist']) + ' ' + s.join(x['Genre'])

df['soup'] = df.apply(create_soup, axis=1)

pd.set_option('display.max_colwidth', -1)


#checking the soup/corpus
df[['soup']].head(100,)

# =============================================================================
# Cosine Simalirity - tfidf
# =============================================================================
#vectorizer, create matrix, finding the cosine similarity
from sklearn.feature_extraction.text import TfidfVectorizer
tfidf = TfidfVectorizer(stop_words='english')
tfidf_matrix = tfidf.fit_transform(df['soup'])

from sklearn.metrics.pairwise import cosine_similarity

cosine_sim = cosine_similarity(tfidf_matrix, tfidf_matrix)

indices = pd.Series(df.index, index=df['Name'])

#recommendation function (top 10 similar songs)
def get_recommendations(song, cosine_sim=cosine_sim):
    idx = indices[song]
    sim_scores = list(enumerate(cosine_sim[idx]))
    sim_scores = sorted(sim_scores, key=lambda x: x[1], reverse=True)
    sim_scores = sim_scores[1:11]
    song_indices = [i[0] for i in sim_scores]
    return df['Name'].iloc[song_indices]

#Input
get_recommendations("this is America", cosine_sim)

# =============================================================================
# K-mean clustering
# =============================================================================

from sklearn.cluster import KMeans
set(df['Genre'])

num_clusters = 11

km = KMeans(n_clusters=num_clusters)

km.fit(tfidf_matrix)

df['label'] = km.labels_.tolist()

#number of songs per cluster (clusters from 0 to 4)
df['label'].value_counts()

df0 = df.loc[df['label']==0]
df1 = df.loc[df['label']==1]
df2 = df.loc[df['label']==2]
df3 = df.loc[df['label']==3]
df4 = df.loc[df['label']==4]

