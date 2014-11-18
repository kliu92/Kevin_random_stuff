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
from sklearn.cross_validation import train_test_split
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

NUM_OF_TREES = 75

rf = RandomForestClassifier(n_estimators=NUM_OF_TREES)
gb = GradientBoostingClassifier(n_estimators=NUM_OF_TREES, subsample = 0.85, learning_rate=.01)

train = pd.read_csv('/Users/fliptop/Downloads/train.csv')
test = pd.read_csv('/Users/fliptop/Downloads/test.csv')
test_id = test['PassengerId']


#train.columns.tolist()
train['Age_Class'] = -1
train['Title'] = -1
train['Family_class_onboard'] = -1
train['Parent_Status'] = -1

test['Age_Class'] = -1
test['Title'] = -1
test['Family_class_onboard'] = -1
test['Parent_Status'] = -1


def Define_Age_Class(df): 
    for i, row in df.iterrows():
        if math.isnan(df['Age'][i]):
            df['Age'][i] = df['Age'].mean()
#            df['Age_Class'][i] = 0 #'Not Available'
        if df['Age'][i] <= 12:
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
            df['Title'][i] = 1  #'Mr.'
        elif re.search(r'''(Mrs.*)''', name):
            df['Title'][i] = 2  # 'Mrs.'
        elif re.search(r'''(Miss|Ms.*)''', name):
            df['Title'][i] = 3  #'Ms.'
        elif re.search(r'''(Master.*)''', name):
            df['Title'][i] = 4  #'Master.'
            #if df['Age_Class'][i] == 0:
            df['Age_Class'][i] = 1
            df['Age'][i] = 5
        elif re.search(r'''(Rev.)''', name):
            df['Title'][i] = 5  #'Rev.'
        elif re.search(r'''(Dr.*)''', name):
            df['Title'][i] = 6  #'Dr.'
        else:
            df['Title'][i] = 0  #'Other'
    return df

def factor_to_int(df):
    for i, row in df.iterrows():
        if df['Sex'][i] == 'female':
            df['Sex'][i] = 0  #Female
        elif df['Sex'][i] == 'male':
            df['Sex'][i] = 1  #Male

        if df['Embarked'][i] == 'C':
            df['Embarked'][i] = 0  #Cherbourg
        elif df['Embarked'][i] == 'Q':
            df['Embarked'][i] = 1  #Queenstown
        elif df['Embarked'][i] == 'S':
            df['Embarked'][i] = 2  #Southampton

        if df['Parch'][i] >= 1:
            df['Family_class_onboard'][i] = 1
            if df['Age'][i] >= 30:
                df['Parent_Status'][i] = 1
        elif df['SibSp'][i] >= 1:
            df['Family_class_onboard'][i] = 2
        else:
            df['Family_class_onboard'][i] = 0

        if df['Parent_Status'][i] != 1:
            df['Parent_Status'][i] = 0

    return df


train = Title_Assign(train)            
train = Define_Age_Class(train)
train = factor_to_int(train)
train = train.drop(['Ticket','PassengerId','Cabin','Name'], 1)
train = train.fillna(-1)
train_1 = train[1:int(round(len(train.index)*0.75))]
test_1 = train[int(round(len(train.index)*0.75)):]

y_train = train_1['Survived']
y_test = test_1['Survived']
X_train = train_1.drop(['Survived'],1)
X_test = test_1.drop(['Survived'],1)

X_train, X_test, y_train, y_test = train_test_split(train.drop(['Survived'],1), train['Survived'], test_size=0.25, random_state=0)

rf_train = rf.fit(X_train, y_train)
gb_train = gb.fit(X_train, y_train)

TestScore_rf = rf_train.predict_proba(X_test)
TestScore_gb = gb_train.predict_proba(X_test)

test = Title_Assign(test)
test = Define_Age_Class(test)
test = factor_to_int(test)
test = test.drop(['Ticket','PassengerId','Cabin','Name'], 1)
test = test.fillna(-1)

Y_train = train['Survived']
X_train = train.drop(['Survived'],1)
rf_test = rf.fit(X_train, Y_train)
gb_test = gb.fit(X_train, Y_train)

TestScore_rf_test = rf_test.predict_proba(test)
TestScore_gb_test = gb_test.predict_proba(test)

TestScore_rf_results = rf_test.predict(test)
TestScore_gb_results = gb_test.predict(test)


plt.subplot(1,1,1)
fpr, tpr, thresholds = roc_curve(y_test, TestScore_rf[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='RF train split: (AUC = %0.4f)' % roc_auc,color='black')

plt.subplot(1,1,1)
fpr, tpr, thresholds = roc_curve(y_test, TestScore_gb[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='GB train split: (AUC = %0.4f)' % roc_auc,color='green')


plt.plot([0, 1], [0, 1], 'k--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.0])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')

plt.legend(loc="lower right")
plt.show()


final_results_rf = pd.DataFrame()
final_results_rf['PassengerId'] = test_id
final_results_rf['Survived'] = TestScore_rf_results
final_results_rf.to_csv('/Users/fliptop/Kaggle_titanic_survival_rf.csv', index=False, sep=',')

final_results_gb = pd.DataFrame()
final_results_gb['PassengerId'] = test_id
final_results_gb['Survived'] = TestScore_gb_results
final_results_gb.to_csv('/Users/fliptop/Kaggle_titanic_survival_gb.csv', index=False, sep=',')

