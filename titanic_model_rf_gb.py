#!/usr/bin/env python

import pandas as pd
import sys
import re
import csv
import random
import bisect
import math

import scipy.stats as stats
from scipy.stats import itemfreq
import numpy as np

from sklearn import linear_model
from sklearn.datasets import load_boston
from sklearn.cross_validation import cross_val_score
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import roc_auc_score
from sklearn.metrics import mean_squared_error
from sklearn import preprocessing
from sklearn.preprocessing import Imputer

from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, GradientBoostingClassifier
from sklearn.ensemble import RandomForestRegressor, AdaBoostRegressor
from sklearn.externals import joblib
from sklearn.tree import DecisionTreeClassifier
from sklearn.tree import DecisionTreeRegressor
from sklearn import preprocessing
from sklearn import svm
from sklearn.metrics import roc_curve, auc
from sklearn import metrics
import matplotlib.pyplot as plt
from collections import Counter

NUM_OF_TREES = 100

rf = RandomForestClassifier(n_estimators=NUM_OF_TREES)
gb = GradientBoostingClassifier(n_estimators=NUM_OF_TREES)

train = pd.read_csv('/Users/fliptop/Downloads/train.csv')
test = pd.read_csv('/Users/fliptop/Downloads/test.csv')

train.columns.tolist()
train['Age_Class'] = -1
train['Title'] = -1

def Define_Age_Class(df): 
    for i, row in df.iterrows():
        if math.isnan(df['Age'][i]):
            df['Age_Class'][i] = 0 #'Not Available'
        elif df['Age'][i] <= 12:
            df['Age_Class'][i] = 1 #'Child'
        elif df['Age'][i] <= 18:
            df['Age_Class'][i] = 2 #'Young Adult'
        elif df['Age'][i] <= 29:
            df['Age_Class'][i] = 3 #'Adult'
        elif df['Age'][i] <= 39:
            df['Age_Class'][i] = 4 #'Family Man'
        elif df['Age'][i] <= 54:
            df['Age_Class'][i] = 5 #'Mid-Age Madness'
        else:
            df['Age_Class'][i] = 6 #'OLD'
    return df

def Title_Assign(df):
    for i, row in df.iterrows():
        name = df['Name'][i].strip()
        if re.search(r'''(Mr\.)''', name):
            df['Title'][i] = 1 #'Mr.'
        elif re.search(r'''(Mrs.*)''', name):
            df['Title'][i] = 2# 'Mrs.'
        elif re.search(r'''(Miss|Ms.*)''', name):
            df['Title'][i] = 3 #'Ms.'
        elif re.search(r'''(Master.*)''', name):
            df['Title'][i] = 4 #'Master.'
        elif re.search(r'''(Rev.)''', name):
            df['Title'][i] = 5 #'Rev.'
        elif re.search(r'''(Dr.*)''', name):
            df['Title'][i] = 6 #'Dr.'
        else:
            df['Title'][i] = 0 #'Other'
    return df

def factor_to_int(df):
    for i, row in df.iterrows():
        if df['Sex'][i] == 'female':
            df['Sex'][i] = 0 #Female
        elif df['Sex'][i] == 'male':
            df['Sex'][i] = 1 #Male
        if df['Embarked'][i] == 'C':
            df['Embarked'][i] = 0 #Cherbourg
        elif df['Embarked'][i] == 'Q':
            df['Embarked'][i] = 1 #Queenstown
        elif df['Embarked'][i] == 'S':
            df['Embarked'][i] = 2 #Southampton
    return df
            
train = Define_Age_Class(train)
train = Title_Assign(train)
train = factor_to_int(train)
train = train.drop(['Ticket','PassengerId','Cabin','Name'], 1)
train_1 = train[1:int(round(len(train.index)*0.75))]
test_1 = train[int(round(len(train.index)*0.75)):]

train_1 = train_1.fillna(-1)
test_1 = test_1.fillna(-1)

y_train = train_1['Survived']
y_test = test_1['Survived']

x_train = train_1.drop(['Survived'],1)
x_test = test_1.drop(['Survived'],1)

rf = rf.fit(x_train, y_train)
gb = gb.fit(x_train, y_train)

TestScore_rf = rf.predict_proba(x_test)
TestScore_gb = gb.predict_proba(x_test)

plt.subplot(1,1,1)
fpr, tpr, thresholds = roc_curve(y_test, TestScore_rf[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='RandomForest: (AUC = %0.4f)' % roc_auc,color='black')

plt.subplot(1,1,1)
fpr, tpr, thresholds = roc_curve(y_test, TestScore_gb[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='GradientBoosting: (AUC = %0.4f)' % roc_auc,color='green')

plt.plot([0, 1], [0, 1], 'k--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.0])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')

plt.legend(loc="lower right")
plt.show()