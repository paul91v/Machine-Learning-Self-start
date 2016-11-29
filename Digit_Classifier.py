# -*- coding: utf-8 -*-
"""
Created on Tue Nov 29 10:04:24 2016

@author: DELL
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier

#Importing the Training and validation set
dataset = pd.read_csv('G:/Kaggle/Digit Classifier/Data/train.csv')
target = dataset[[0]].values.ravel()
train = dataset.iloc[:,1:].values
test = pd.read_csv('G:/Kaggle/Digit Classifier/Data/test.csv').values

#Training the Random Forest
rf = RandomForestClassifier(n_estimators = 100)
rf.fit(train, target)
pred = rf.predict(test)
np.savetxt('DC_RF_100.csv', np.c_[range(1,len(test)+1),pred], delimiter = ',', header = 'ImageId,Label', comments = '', fmt = '%d')