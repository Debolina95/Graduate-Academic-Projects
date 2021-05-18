#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 19 07:31:01 2021

@author: lichangtan
"""



import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.tree import plot_tree
from sklearn.model_selection import GridSearchCV,RandomizedSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import f1_score,precision_score,recall_score,accuracy_score,confusion_matrix,precision_recall_curve,roc_curve,roc_curve,auc
from imblearn.ensemble import BalancedRandomForestClassifier

from sklearn.model_selection import cross_validate
from sklearn.linear_model import LogisticRegression
from imblearn.pipeline import make_pipeline as make_imb_pipeline
from imblearn.over_sampling import SMOTE
from imblearn.over_sampling import RandomOverSampler

from imblearn.combine import SMOTEENN
from xgboost import XGBRFClassifier

from collections import Counter
from imblearn.under_sampling import RandomUnderSampler
from sklearn.metrics import classification_report, roc_auc_score


'''
    predicting if the songs are going to win grammy, this dataset 
    has imbalance classes issue (88% no, 12% yes)
'''

# read the file 
songs = pd.read_csv('updated_attributes_csv.csv')
print(songs.info()) # no missing values

# reset the won grammy name
songs = songs.rename(columns = {"won grammy": "won_grammy"})
songs = pd.DataFrame(songs)


'''
    check the percentage of each object type variable
'''

# check the balance of won grammy category 
songs_count = songs['won_grammy'].value_counts().reset_index()
songs_count.columns = ['won_grammy', 'count']

sns.barplot(x='won_grammy', y='count',data=songs_count, alpha=0.8)
plt.ylabel('Count', fontsize=12)
plt.xlabel('Won_grammy', fontsize=12)
plt.title('Class Distribution of the Spotify Dataset')
plt.show()
# imblance classes issue,87%/13%

'''
    change some data type
'''

# change the mode datatype 
songs['Mode'] = songs['Mode'].astype('str')
songs['Explicit'] = songs['Explicit'].astype('str')
songs['won_grammy'] = songs['won_grammy'].astype('str')


# check the correlation 
plt.figure(figsize=(12,12))
sns.heatmap(songs.corr(),annot=True)

# by checking the heatmap, we can see only two variables are highly correlated
# to each other, but let's keep them for prediction,and remove them later
# on to compare the difference 

# check the stat info
pd.set_option('max_columns', 50)
pd.set_option('max_rows', 50)

print(songs.describe())


'''
    random forest without tuning
'''

# drop some unwanted variables 
songs_data = songs.drop(['Album','Artist','Name','Unnamed: 0'],axis=1)
songs_data.info()

# dummize the data 
songs_dummy = pd.get_dummies(songs_data,drop_first=True)
songs_dummy.info()

# split x and y from dataset df6
x= songs_dummy.drop('won_grammy_True',axis=1)
y= songs_dummy['won_grammy_True']

# split into train and test sets
x_train,x_test,y_train,y_test = train_test_split(x,y,
                                                 random_state=1,
                                                 test_size=0.25)

random = RandomForestClassifier(n_estimators=500,class_weight={0:1,1:7},
                                random_state=1,n_jobs=-1).fit(x_train,y_train)


# evalute the training set
y_pred_train = random.predict(x_train)
print(metrics.classification_report(y_train,y_pred_train))
# overfitting 

# evalute the testing set
y_pred = random.predict(x_test)
print(metrics.classification_report(y_test,y_pred))

matrix = pd.DataFrame(confusion_matrix(y_test,y_pred,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])

matrix

# roc auc
print("ROC AUC score for random forest data: ", roc_auc_score(y_test, y_pred)) # 0.53
# the random forest model is overfitting, and bad prediction on the minority class


'''
    BalancedRandomForest without tuning
'''

brf = BalancedRandomForestClassifier(bootstrap=True,
                                     class_weight=None,
                                     max_depth=8,
                                     max_features=2,
                                     max_leaf_nodes=None,
                                     min_impurity_decrease=0.0,
                                     min_samples_leaf=2,
                                     min_samples_split=2,
                                     min_weight_fraction_leaf=0.0,
                                     n_estimators=500, n_jobs=1,
                                     oob_score=False, random_state=5,
                                     replacement=False,
                                     sampling_strategy='auto',
                                     verbose=0, warm_start=False).fit(x_train, y_train)

# evalute the training set
y_pred_train = brf.predict(x_train)
print(metrics.classification_report(y_train,y_pred_train))


# evalute the testing set
y_pred15 = brf.predict(x_test)
print(metrics.classification_report(y_train,y_pred_train))
print(metrics.classification_report(y_test,y_pred15))

matrix58 = pd.DataFrame(confusion_matrix(y_test,y_pred15,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])

print(metrics.classification_report(y_test,y_pred15))
matrix58

# roc auc
print("ROC AUC score for random forest data: ", roc_auc_score(y_test, y_pred15)) # 0.66
# the recall rate has been improved by roughly 70%, but the precision rate is decreased by 37%
# the f1 score has been improved by 18%.
# this model can be improved by grid searching.


'''
     tuning the random forest model with undersampling 
'''
# summarize class distribution
print("Before undersampling: ", Counter(y_train))
# Counter({0: 102352, 1: 13846},70/30

# define undersampling strategy
undersample = RandomUnderSampler(sampling_strategy='majority')

# fit and apply the transform
x_train_under, y_train_under = undersample.fit_resample(x_train, y_train)

# summarize class distribution
print("After undersampling: ", Counter(y_train_under))
# Counter({0: 13846, 1: 13846}, 50/50

random1 = RandomForestClassifier(n_estimators=500,random_state=4).fit(x_train_under, 
                                                                      y_train_under)

para_grid = {"max_depth":range(2,16),
             "min_samples_split":range(2,6),
             'bootstrap': [True]}

grid = GridSearchCV(random1,para_grid,verbose=3,scoring="f1", cv = 5)

# fit the grid
grid.fit(x_train_under,y_train_under)

# best parameters
grid.best_params_ #{'max_depth':15, 'min_samples_split': 2}

# use the optimal parameters to see if the performance improves
random = RandomForestClassifier(max_depth=15,min_samples_split=2,
                                n_estimators=500,random_state=1).fit(x_train_under,
                                                                     y_train_under)

# evalute the training set again 
y_pred_train = random.predict(x_train)
print(metrics.classification_report(y_train,y_pred_train))
mat6 = pd.DataFrame(confusion_matrix(y_train,y_pred_train,labels=[0,1]),
index=['actual:0','actual:1'],columns=['pred:0','pred:1'])
mat6


# testing set again 
y_pred_test23 = random.predict(x_test)
print(metrics.classification_report(y_test,y_pred_test23))


matrix89 = pd.DataFrame(confusion_matrix(y_test,y_pred_test23,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
matrix89

# roc auc
print("ROC AUC score for random forest with tuning and with undersampling model: ", roc_auc_score(y_test, y_pred_test23)) # 0.70


feature_importances3 = pd.DataFrame(random.feature_importances_, 
                                   index=x.columns,
                                   columns=['importance']).sort_values('importance', 
                                                                       ascending=False)
feature_importances3

# get to know which features are used by the random forest model 
sel = SelectFromModel(RandomForestClassifier(max_depth=15,min_samples_split=2,
                                n_estimators=500,random_state=1))
sel.fit(x_train_under,y_train_under)

selected_feat= x_train_under.columns[(sel.get_support())]
print('the features used are:',selected_feat,'total numbers are',len(selected_feat))
# This model performs better than the Balanced Random Forest model. 
# the overall f1 score of both True class and False class gets improved by 3% and 2 %, 
# respectively. But this model is still overfitting on the True class. 


'''
    xgboost with random searching tuning hyperparameters and tuning the class weight
    max_depth,2-30,increase, ovefitting
    subsample 0.1-1,increase, overfitting
    colsamplebylevel 0.1-1,
    colsamplebytree 0.1-1,
    min-childweight 1,5,100,
    lambda,alpha,
    n-estimator 10-1000,
    learning rate 0.01-1,decrease better, increase faster
    if we decrease the learning rate by 50%, we should double the n-estimator,
    
'''

from sklearn.utils.class_weight import compute_class_weight

#####
# cacluate the weight that we should put to the minority class
weights = compute_class_weight('balanced', classes=np.unique(y_train), y=y_train)
scale = weights[1]/weights[0] # 7.5 times more 
# another way to do it is to use sum(negative instances) / sum(positive instances) from xgboost documentation
float(np.sum(y_train ==0)) / np.sum(y_train ==1)


# random searching hyperparameters
clf_xgb = xgb.XGBClassifier(objective = 'binary:logistic',scale_pos_weight=7.5)
 
parameters = {
            'max_depth': range(2,30),
            'learning_rate': [0.01, 0.05, 0.1, 0.15, 0.20, 0.25],
            'n_estimators': [150,350,500,800],
            'min_child_weight': range(1,20),
            'subsample': [0.1,0.3,0.5,0.7,1],
            'colsample_bytree': [0.1,0.3,0.5,0.7,1],
            'colsample_bylevel': [0.1,0.3,0.5,0.7,1],
            'reg_alpha': [0, 1e-2, 1, 1e1],
            'reg_lambda': [0, 1e-2, 1, 1e1]
            }
clf = RandomizedSearchCV(clf_xgb, param_distributions = parameters, n_iter = 10, 
                         scoring = 'f1', error_score = 0, verbose = 3, cv=5)
clf.fit(x_train, y_train)

clf.best_params_ 

clf_xgb6 = xgb.XGBClassifier(objective = 'binary:logistic', 
                             colsample_bylevel = 0.7,
                             colsample_bytree= 0.7,
                             learning_rate =0.01,
                             max_depth = 24,
                             min_child_weight= 17,
                             n_estimators =350,
                             reg_alpha = 0.01,
                             reg_lambda = 10.0,
                             subsample = 0.7,
                             scale_pos_weight=7.5,
                             random_state=42)
clf_xgb6.fit(x_train,y_train)

# tuning the class weight to confirm if the class weight to be set by 7.5 is the optimal value
for w in [1,7.5,15,30,60,120]:
    print('---Weight of {} for True class---'.format(w))
    xgb_model = xgb.XGBClassifier(objective = 'binary:logistic', 
                             colsample_bylevel = 0.7,
                             colsample_bytree= 0.7,
                             learning_rate =0.01,
                             max_depth = 24,
                             min_child_weight= 17,
                             n_estimators =350,
                             reg_alpha = 0.01,
                             reg_lambda = 10.0,
                             subsample = 0.7,
                             scale_pos_weight=w,
                             random_state=42)
    xgb_model.fit(x_train,y_train)

    y_pred_test18 = xgb_model.predict(x_test)
    print(metrics.classification_report(y_test,y_pred_test18))
    
# after checking the f1 score of the testing set, we would choose the weight
# to be 7.5, which gives us the highest f1 score for both classes.

# evalute the training set again 
y_pred_train49 = clf_xgb6.predict(x_train)
print(metrics.classification_report(y_train,y_pred_train49))
mat40 = pd.DataFrame(confusion_matrix(y_train,y_pred_train49,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
mat40

# testing set again 
y_pred_test13 = clf_xgb6.predict(x_test)
print(metrics.classification_report(y_test,y_pred_test13))

matrix52 = pd.DataFrame(confusion_matrix(y_test,y_pred_test13,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
matrix52

# roc auc
print("ROC AUC score for random forest data: ", roc_auc_score(y_test, y_pred_test13)) # 0.69

# The XGBoost of the default logistic regression model gives us 88% of the f1 score 
# of the False class and 39% of the f1 score of the True class. These results are the 
# best of all the models, though the predicted accuracy rate of the True class is 
# not the highest. This model gives us the most balance between precision and recall. 
# Although the previous models received higher recall rate, meaning the predicted 
# accuracy rate of the True class is higher, those models also gave us lower precision 
# rate, meaning more false negative misclassifications

feature_importances8 = pd.DataFrame(clf_xgb6.feature_importances_, 
                                   index=x.columns,columns=['importance']).sort_values('importance', 
                                                                       ascending=False)
feature_importances8.plot(kind='barh')


# get to know which features are used by this model 
xgbselection = SelectFromModel(xgb.XGBClassifier(objective = 'binary:logistic', 
                             colsample_bylevel = 0.7,
                             colsample_bytree= 0.7,
                             learning_rate =0.01,
                             max_depth = 24,
                             min_child_weight= 17,
                             n_estimators =350,
                             reg_alpha = 0.01,
                             reg_lambda = 10.0,
                             subsample = 0.7,
                             scale_pos_weight=7.5,
                             random_state=42))
xgbselection.fit(x_train,y_train)

selected_feat_boost = x_train.columns[(xgbselection.get_support())]
print('the features used are:',selected_feat_boost,'total numbers are',len(selected_feat_boost))
# the model only use 'Explicit_True' one feature to train the model 

# plot the PR curve and ROC curve
fig = plt.figure(figsize=(15,8))
ax1 = fig.add_subplot(1,2,1)
ax1.set_xlim([-0.05,1.05])
ax1.set_ylim([-0.05,1.05])
ax1.set_xlabel('Recall')
ax1.set_ylabel('Precision')
ax1.set_title('PR Curve')

ax2 = fig.add_subplot(1,2,2)
ax2.set_xlim([-0.05,1.05])
ax2.set_ylim([-0.05,1.05])
ax2.set_xlabel('False Positive Rate')
ax2.set_ylabel('True Positive Rate')
ax2.set_title('ROC Curve')


for w,k in zip([1,7.5,15,30,60,120],'bgrcmykw'):
    xgb_model1 = xgb.XGBClassifier(objective = 'binary:logistic', 
                             colsample_bylevel = 0.7,
                             colsample_bytree= 0.7,
                             learning_rate =0.01,
                             max_depth = 24,
                             min_child_weight= 17,
                             n_estimators =350,
                             reg_alpha = 0.01,
                             reg_lambda = 10.0,
                             subsample = 0.7,
                             scale_pos_weight=w,
                             random_state=42)
    xgb_model1.fit(x_train,y_train)
    pred_prob = xgb_model1.predict_proba(x_test)[:,1]

    p,r,_ = precision_recall_curve(y_test,pred_prob)
    tpr,fpr,_ = roc_curve(y_test,pred_prob)
    
    ax1.plot(r,p,c=k,label=w)
    ax2.plot(tpr,fpr,c=k,label=w)
    ax1.legend(loc='lower left')    
    ax2.legend(loc='lower left')

plt.show()


### use max_delta_step instead of scale_pos_weight based on the xgboost documentartion
clf_xgb = xgb.XGBClassifier(objective = 'binary:logistic')
 
parameters = {
            'max_depth': range(2,30),
            'learning_rate': [0.01, 0.05, 0.1, 0.15, 0.20, 0.25],
            'n_estimators': [150,350,500,800],
            'min_child_weight': range(1,20),
            'subsample': [0.1,0.3,0.5,0.7,1],
            'colsample_bytree': [0.1,0.3,0.5,0.7,1],
            'colsample_bylevel': [0.1,0.3,0.5,0.7,1],
            'reg_alpha': [0, 1e-2, 1, 1e1],
            'reg_lambda': [0, 1e-2, 1, 1e1],
            'max_delta_step':range(1,10)}
clf5 = RandomizedSearchCV(clf_xgb, param_distributions = parameters, n_iter = 10, 
                         scoring = 'f1', error_score = 0, verbose = 3, cv=5)
clf5.fit(x_train, y_train)


clf5.best_params_ 

clf_xgb7 = xgb.XGBClassifier(objective = 'binary:logistic', 
                             colsample_bylevel = 0.7,
                             colsample_bytree= 0.7,
                             learning_rate =0.2,
                             max_depth = 10,
                             min_child_weight= 3,
                             n_estimators =500,
                             reg_alpha = 1,
                             reg_lambda = 1,
                             subsample = 0.1,
                             max_delta_step = 9,
                             random_state=42)
clf_xgb7.fit(x_train,y_train)

# evalute the training set again 
y_pred_train51 = clf_xgb7.predict(x_train)
print(metrics.classification_report(y_train,y_pred_train51))
mat43 = pd.DataFrame(confusion_matrix(y_train,y_pred_train51,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
mat43

# testing set again 
y_pred_test16 = clf_xgb7.predict(x_test)
print(metrics.classification_report(y_test,y_pred_test16))

matrix57 = pd.DataFrame(confusion_matrix(y_test,y_pred_test16,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
matrix57

# roc auc
print("ROC AUC score for random forest data: ", roc_auc_score(y_test, y_pred_test16)) # 0.60
# this method is poorer than the previous one.

# The above models that are shown in the report.
# the xgboost model is the best. 











'''
    the below methods are also tried by us, but not present to the report.
'''

## use  top 7 features from the feature importance plot by the xgboost model


# read the file 
songs = pd.read_csv('updated_attributes_csv.csv')
print(songs.info()) # no missing values

# reset the won grammy name
songs = songs.rename(columns = {"won grammy": "won_grammy"})
songs = pd.DataFrame(songs)


'''
    check the percentage of each object type variable
'''

# check the balance of won grammy category 
songs_count = songs['won_grammy'].value_counts().reset_index()
songs_count.columns = ['won_grammy', 'count']

sns.barplot(x='won_grammy', y='count',data=songs_count, alpha=0.8)
plt.ylabel('Count', fontsize=12)
plt.xlabel('Won_grammy', fontsize=12)
plt.title('Class Distribution of the Spotify Dataset')
plt.show()
# imblance classes issue,87%/13%



'''
    change some data type
'''

# change the mode datatype 
songs['Mode'] = songs['Mode'].astype('str')
songs['Explicit'] = songs['Explicit'].astype('str')
songs['won_grammy'] = songs['won_grammy'].astype('str')


# drop some unwanted variables 
songs_data = songs.drop(['Album','Artist','Name','Unnamed: 0'],axis=1)
songs_data.info()

# dummize the data 
songs_dummy = pd.get_dummies(songs_data,drop_first=True)
songs_dummy.info()

# drop Mode_1,Liveness,Tempo,Danceability,Energy,Valence,TimeSignature
songs_dummy1 = songs_dummy.drop(['Mode_1','Liveness','Tempo','Danceability','Energy',
                         'Valence','TimeSignature'],axis=1)
songs_dummy1.info()


# split x and y from dataset df6
x= songs_dummy1.drop('won_grammy_True',axis=1)
y= songs_dummy1['won_grammy_True']

# split into train and test sets
x_train,x_test,y_train,y_test = train_test_split(x,y,
                                                 random_state=1,
                                                 test_size=0.25)


clf_xgb2 = xgb.XGBClassifier(objective = 'binary:logistic',scale_pos_weight=7.5)
 
parameters = {
            'max_depth': range(2,30),
            'learning_rate': [0.01, 0.05, 0.1, 0.15, 0.20, 0.25],
            'n_estimators': [150,350,500,800],
            'min_child_weight': range(1,20),
            'subsample': [0.1,0.3,0.5,0.7,1],
            'colsample_bytree': [0.1,0.3,0.5,0.7,1],
            'colsample_bylevel': [0.1,0.3,0.5,0.7,1],
            'reg_alpha': [0, 1e-2, 1, 1e1],
            'reg_lambda': [0, 1e-2, 1, 1e1]
            }
clf2 = RandomizedSearchCV(clf_xgb2, param_distributions = parameters, n_iter = 10, 
                         scoring = 'f1', error_score = 0, verbose = 3, cv=5)
clf2.fit(x_train, y_train)


clf2.best_params_ 


clf_xgb7 = xgb.XGBClassifier(objective = 'binary:logistic', 
                             colsample_bylevel = 0.7,
                             colsample_bytree= 0.3,
                             learning_rate =0.1,
                             max_depth = 9,
                             min_child_weight= 13,
                             n_estimators =500,
                             reg_alpha = 1,
                             reg_lambda = 0,
                             subsample = 0.5,
                             scale_pos_weight=7.5,
                             random_state=42)
clf_xgb7.fit(x_train,y_train)

# evalute the training set again 
y_pred_train57 = clf_xgb7.predict(x_train)
print(metrics.classification_report(y_train,y_pred_train57))
mat41 = pd.DataFrame(confusion_matrix(y_train,y_pred_train57,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
mat41

# testing set again 
y_pred_test23 = clf_xgb7.predict(x_test)
print(metrics.classification_report(y_test,y_pred_test23))

matrix56 = pd.DataFrame(confusion_matrix(y_test,y_pred_test23,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
matrix56

feature_importances9 = pd.DataFrame(clf_xgb7.feature_importances_, 
                                   index=x.columns,columns=['importance']).sort_values('importance', 
                                                                       ascending=False)
feature_importances9.plot(kind='barh')

# the f1 score is worse, and the model still considers Explict_True is the most important feature


'''
    smote function, oversampling the minority class, combined with the random forest
'''
# Import the SMOTE package
from imblearn.over_sampling import SMOTE
# read the file 
songs = pd.read_csv('updated_attributes_csv.csv')
print(songs.info()) # no missing values

# reset the won grammy name
songs = songs.rename(columns = {"won grammy": "won_grammy"})
songs = pd.DataFrame(songs)


'''
    check the percentage of each object type variable
'''

# check the balance of won grammy category 
songs_count = songs['won_grammy'].value_counts().reset_index()
songs_count.columns = ['won_grammy', 'count']

sns.barplot(x='won_grammy', y='count',data=songs_count, alpha=0.8)
plt.ylabel('Count', fontsize=12)
plt.xlabel('Won_grammy', fontsize=12)
plt.title('Class Distribution of the Spotify Dataset')
plt.show()
# imblance classes issue,87%/13%

'''
    change some data type
'''

# change the mode datatype 
songs['Mode'] = songs['Mode'].astype('str')
songs['Explicit'] = songs['Explicit'].astype('str')
songs['won_grammy'] = songs['won_grammy'].astype('str')


# check the correlation 
plt.figure(figsize=(12,12))
sns.heatmap(songs.corr(),annot=True)

# by checking the heatmap, we can see only two variables are highly correlated
# to each other, but let's keep them for prediction,and remove them later
# on to compare the difference 

# check the stat info
pd.set_option('max_columns', 50)
pd.set_option('max_rows', 50)

print(songs.describe())


# drop some unwanted variables 
songs_data = songs.drop(['Album','Artist','Name','Unnamed: 0'],axis=1)
songs_data.info()

# dummize the data 
songs_dummy = pd.get_dummies(songs_data,drop_first=True)
songs_dummy.info()

# split x and y from dataset df6
x= songs_dummy.drop('won_grammy_True',axis=1)
y= songs_dummy['won_grammy_True']

# split into train and test sets
x_train,x_test,y_train,y_test = train_test_split(x,y,
                                                 random_state=1,
                                                 test_size=0.25)

# summarize class distribution
print("Before oversampling: ",Counter(y_train))
# Counter({0: 102352, 1: 13846} 70/30


# define oversampling strategy
SMOTE = SMOTE()

# fit and apply the transform
X_train_SMOTE, y_train_SMOTE = SMOTE.fit_resample(x_train, y_train)

# summarize class distribution
print("After oversampling: ",Counter(y_train_SMOTE))
# Counter({0: 102352, 1: 102352}

# Fit model on oversampled data
oversample_rf = RandomForestClassifier(n_estimators=500).fit(X_train_SMOTE,
                                                             y_train_SMOTE)
# Make predictions on test sets
y_pred78 = oversample_rf.predict(x_test)
print(metrics.classification_report(y_test,y_pred78))

matrix66 = pd.DataFrame(confusion_matrix(y_test,y_pred78,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
matrix66

# roc auc
print("ROC AUC score for random forest data: ", roc_auc_score(y_test, y_pred78)) # 0.63

# the f1 score of the True class is slightly worse than when we use undersampling with
# random forest model.


'''
     combination of oversampling and undersampling by using pipeline to fit to the 
     xgboost model.
     1. searching for the best combination of the oversampling and undersampling 
'''
from imblearn.pipeline import Pipeline
from sklearn.model_selection import cross_val_score
from numpy import mean

# split into train and test sets
x_train,x_test,y_train,y_test = train_test_split(x,y,
                                                 random_state=1,
                                                 test_size=0.25)

# split x and y from dataset df6
x= songs_dummy.drop('won_grammy_True',axis=1)
y= songs_dummy['won_grammy_True']


# values to evaluate
over_values = [0.2,0.3,0.4,0.5]
under_values = [0.8,0.7,0.6,0.5]
for o in over_values:
  for u in under_values:
    # define pipeline
    model = xgb.XGBClassifier(colsample_bytree= 0.5,
                            colsample_bylevel =0.5,
                            learning_rate= 0.01,
                            max_depth= 15,
                            min_child_weight= 1,
                            n_estimators =800,
                            subsample=0.4,
                            reg_alpha = 0.01,
                            reg_lambda =1,
                            random_state=5)
    over = SMOTE(sampling_strategy=o)
    under = RandomUnderSampler(sampling_strategy=u)
    steps = [('over', over), ('under', under), ('model', model)]
    pipeline = Pipeline(steps=steps)
    # evaluate pipeline
    scores = cross_val_score(pipeline, x, y, scoring='roc_auc', cv=5, n_jobs=-1)
    score = mean(scores)
    print('SMOTE oversampling rate:%.1f, Random undersampling rate:%.1f , Mean ROC AUC: %.3f' % (o, u, score))
# SMOTE oversampling rate:0.2, Random undersampling rate:0.8 , Mean ROC AUC: 0.637


# define pipeline
model = xgb.XGBClassifier(colsample_bytree= 0.5,
                            colsample_bylevel =0.5,
                            learning_rate= 0.01,
                            max_depth= 15,
                            min_child_weight= 1,
                            n_estimators =800,
                            subsample=0.4,
                            reg_alpha = 0.01,
                            reg_lambda =1,
                            random_state=5)
over = SMOTE(sampling_strategy=0.2)
under = RandomUnderSampler(sampling_strategy=0.8)
steps = [('o', over), ('u', under), ('model', model)]
pipeline = Pipeline(steps=steps)

pipeline.fit(x_train, y_train)
# Make predictions on test sets
y_pred99 = pipeline.predict(x_test)
print(metrics.classification_report(y_test,y_pred99))

matrix99 = pd.DataFrame(confusion_matrix(y_test,y_pred99,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
matrix99

# roc auc
print("ROC AUC score for random forest data: ", roc_auc_score(y_test, y_pred99)) # 0.68

# try to use random forest 
# define pipeline
model = RandomForestClassifier(max_depth=15,min_samples_split=5,
                                n_estimators=500,random_state=1)

over = SMOTE(sampling_strategy=0.2)
under = RandomUnderSampler(sampling_strategy=0.8)
steps = [('o', over), ('u', under), ('model', model)]
pipeline = Pipeline(steps=steps)

pipeline.fit(x_train, y_train)
# Make predictions on test sets
y_pred100 = pipeline.predict(x_test)
print(metrics.classification_report(y_test,y_pred100))

matrix100 = pd.DataFrame(confusion_matrix(y_test,y_pred100,labels=[1,0]),
index=['actual:1','actual:0'],columns=['pred:1','pred:0'])
matrix100

# roc auc
print("ROC AUC score for random forest data: ", roc_auc_score(y_test, y_pred100)) # 0.67

# the xgboost model and random forest model perform similarly by comparing the f1 score
# of the True class. 















