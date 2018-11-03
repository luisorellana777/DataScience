# -*- coding: utf-8 -*-
"""
Created on Wed Oct 17 15:08:50 2018

@author: Luis.Orellana.EXT

Dataset: https://www.kaggle.com/dragonheir/logistic-regression
"""

import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import train_test_split
from sklearn.metrics import classification_report
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix


class Scal:
    #Escala de -1 a 1
    scaler = MinMaxScaler(feature_range=(0, 1))
    
    def __init__(self, X):
        self.X = X
        Scal.scaler.fit(X)

    def scaling(self):
        self.X_Scaled = Scal.scaler.transform(self.X)
        return self.X_Scaled
        
    def undo_scaling(self):
        X_un_Scaled = Scal.scaler.inverse_transform(self.X_Scaled)
        return X_un_Scaled

def transform(column):
    
    wcdict = {}
    
    wcdict['Male'] = 0
    wcdict['Female'] = 1
    
    col_num = pd.DataFrame(column).applymap(lambda s: wcdict.get(s) if s in wcdict else s)
    
    return col_num

def init():
    
    X = pd.read_csv(r"C:/Users/Luis.O.A/Documents/Charla ML/DataScience/Social_Network_Ads.csv", sep=",")
    
    Y = X.Purchased
    X = X.drop('Purchased', axis=1)
    X = X.drop('User ID', axis=1)
    
    X.Gender = transform(X.Gender)

    scal = Scal(X)
    X = scal.scaling()
    #scal.undo_scaling()
    ###########################################################################################
    X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = .3, random_state=25)
    
    C_param_range = [0.001,0.01,0.1,1,10,100]
    
    for i in C_param_range:
        LogReg = LogisticRegression(penalty = 'l2', C = i,random_state = 0)
        LogReg.fit(X_train, y_train)
        
        y_pred = LogReg.predict(X_test)
        
        
        matriz_confu = confusion_matrix(y_test, y_pred)
        print("------------------------------------------------------")
        print("Parametro: ", i)
        print(matriz_confu)
        print(classification_report(y_test, y_pred))
        print(accuracy_score(y_test, y_pred))
        print("------------------------------------------------------")
        
        
    
init()