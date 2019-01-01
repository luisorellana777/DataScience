import os
import csv
import math
import numpy as np
import pandas as pd
from scipy import stats
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns
from mpl_toolkits.mplot3d import Axes3D
from sklearn.metrics import silhouette_score
from sklearn import preprocessing
from matplotlib import pyplot as plt
from scipy import signal

class Signal:
    PATH = "C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/Sujetos"
    
    def __init__(self, filePath):
        self.filePath = filePath
        
    def reesampling(self, var):
        x_resampled = signal.resample(var, math.ceil(var.shape[0]*1.33333333333333))
        return x_resampled
     
    def process(self):
        X = pd.read_csv(self.filePath, sep="	")
        
        VFSCd = self.reesampling(X['VFSCd'])
        VFSCi = self.reesampling(X['VFSCi'])
        PAMn = self.reesampling(X['PAMn'])
        
        
        reesampled_data = zip(VFSCd,VFSCi, PAMn)
        
        with open(self.filePath[:(len(self.filePath)-3)] + "csv", 'w', newline='') as f:
            writer = csv.writer(f, delimiter='\t')
            writer.writerow(["VFSCd", "VFSCi", "PAMn"])
            writer.writerows(reesampled_data)
        
def run_signal():    
    listOfFolders = os.listdir(Signal.PATH)
    for folder in listOfFolders:
        folderPath = Signal.PATH+"/"+folder
        listOfFiles = os.listdir(folderPath)
        for file in listOfFiles:
            filePath = folderPath+"/"+file
            signal = Signal(filePath)
            signal.process()


class Stair:
        
    def __init__(self, file_stair):
        self.file_stair = file_stair

    def reesampling(self, var):
        x_resampled = signal.resample(var, math.ceil(var.shape[0]*0.52977777645333333333333333333333))
        return x_resampled
     
    def process(self):
        X = pd.read_csv(self.file_stair, sep="	")
        
        escalon = self.reesampling(X['ESCALON'])

        reesampled_data = zip(escalon)
        
        with open(self.file_stair[:(len(self.file_stair)-4)] + "_04.csv", 'w', newline='') as f:
            writer = csv.writer(f, delimiter='\t')
            writer.writerow(["ESCALON"])
            writer.writerows(reesampled_data)

def run_stair():   
    file_stair = "C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/esc.csv"     
    stair = Stair(file_stair)
    stair.process()

run_stair()