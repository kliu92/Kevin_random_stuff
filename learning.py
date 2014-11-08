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


def displayImportance(clf_, my_cols):
    dict_var_importance = {}
    for ix, w in enumerate(clf.feature_importances_):
        #only displaying vars with importance higher than average
        if w > np.average(clf.feature_importances_):
            dict_var_importance[my_cols[ix]] = w
    
    ctr_vars = 0
    c=Counter(dict_var_importance)

    print
    print '--importance from high to low--' 
    ctr = 0
    for tpl in c.most_common(): 
        ctr += 1
        print '%0.8f,%s,%d' %(tpl[1],tpl[0],ctr)
        
        
#def displayImportance(clf_, my_cols):
#    importances = clf_.feature_importances_
#    std = np.std([tree.feature_importances_ for tree in clf_.estimators_],
#             axis=0)
#    indices = np.argsort(importances)[::-1]

#    # Print the feature ranking
#    print("Feature ranking:")
#
#    for f in range(10):
#        print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))



#---------------------------------------

NUM_OF_TREES = 100
PUBLISHER_NAME = 'Marketo'


clf = RandomForestClassifier(n_estimators=NUM_OF_TREES)

#--- model files ------------------

WORKING_DIR = "/Users/timothyliu/MarketoActNow/PRODUCTION_ROUND_1"

train = pd.read_csv('%s/train.csv' %WORKING_DIR)
test = pd.read_csv('%s/test.csv' %WORKING_DIR)

#==>re-code DV if needed<==
train['Label_Converted.'] = -1
train['Label_Converted.'][train['label_converted.']== 0] = 0
train['Label_Converted.'][train['label_converted.']!= 0] = 1
y_train = train['Label_Converted.']

test['Label_Converted.'] = -1
test['Label_Converted.'][test['label_converted.']== 0] = 0
test['Label_Converted.'][test['label_converted.']!= 0] = 1
y_test = test['Label_Converted.']

#-------------------------------------------
train_columns = list(train.columns)

#==>remove unused vars if needed <==
#train_columns.remove('X')
train_columns.remove('FixedScore_reason')
train_columns.remove('Platform_amount')
train_columns.remove('label_converted.')
train_columns.remove('Label_Converted.')
train_columns.remove('oppId')
train_columns.remove('pid')
train_columns.remove('pid.1')
train_columns.remove('pid.2')
 
#---------------------------------
my_cols = train_columns
X_train = train[my_cols]
X_test = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_allVar_Model = clf.predict_proba(X_test)  

displayImportance(clf, my_cols)
#========================================
# Case 0

my_cols = [varName for varName in train_columns if not varName.startswith('mko_')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_AllExceptMkto_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)
#========================================
# Case I
 
my_cols = [varName for varName in train_columns if varName.upper().startswith('PG')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_PG_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)
#-------------------------------------------------
# Case II
 
my_cols = [varName for varName in train_columns if varName.upper().startswith('BW')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_BW_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)
#-------------------------------------------------
# Case III
 
my_cols = [varName for varName in train_columns if varName.upper().startswith('DNB')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_DNB_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)
#-------------------------------------------------
# Case IV
 
my_cols = [varName for varName in train_columns if varName.startswith('Platform')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_Platform_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)
#-------------------------------------------------
# Case IV
 
my_cols = [varName for varName in train_columns if varName.startswith('Indeed')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_Indeed_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)
#-------------------------------------------------
# Case V
 
my_cols = [varName for varName in train_columns if varName.startswith('InboundScore')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_InboundScore_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)
#-------------------------------------------------
# Case VI
 
my_cols = [varName for varName in train_columns if varName.startswith('mko_')]

X_train = train[my_cols]
X_test  = test[my_cols]

clf = clf.fit(X_train, y_train)
TestScore_Mkto_Model = clf.predict_proba(X_test)  

#displayImportance(clf, my_cols)

#==============================================================================
# Compute ROC curves and area the curve

plt.subplot(1,1,1)
fpr, tpr, thresholds = roc_curve(y_test, TestScore_allVar_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='All Variables: (AUC = %0.4f)' % roc_auc,color='black')

plt.subplot(1,1,1)
fpr, tpr, thresholds = roc_curve(y_test, TestScore_AllExceptMkto_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='All Variables other than Marketing Activity: (AUC = %0.4f)' % roc_auc,color='orange')


fpr, tpr, thresholds = roc_curve(y_test, TestScore_Mkto_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='Marketing Activity: (AUC = %0.4f)' % roc_auc,color='brown')


fpr, tpr, thresholds = roc_curve(y_test, TestScore_Platform_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='Marketing and Personal Info: (AUC = %0.4f)' % roc_auc,color='red')

fpr, tpr, thresholds = roc_curve(y_test, TestScore_InboundScore_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='Business Social: (AUC = %0.4f)' % roc_auc,color='grey')

fpr, tpr, thresholds = roc_curve(y_test, TestScore_BW_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='Business Technology: (AUC = %0.4f)' % roc_auc,color='blue')

fpr, tpr, thresholds = roc_curve(y_test, TestScore_DNB_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='Business Info: (AUC = %0.4f)' % roc_auc,color='green')


fpr, tpr, thresholds = roc_curve(y_test, TestScore_Indeed_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='Business Hiring: (AUC = %0.4f)' % roc_auc,color='yellow')


fpr, tpr, thresholds = roc_curve(y_test, TestScore_PG_Model[:,[1]])
roc_auc = auc(fpr, tpr)
plt.plot(fpr, tpr, label='Personal Social: (AUC = %0.4f)' % roc_auc,color='purple')


plt.plot([0, 1], [0, 1], 'k--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.0])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')

plt.legend(loc="lower right")
plt.title('%s - %s' %(PUBLISHER_NAME, str(clf).split('(')[0]))
plt.show()

