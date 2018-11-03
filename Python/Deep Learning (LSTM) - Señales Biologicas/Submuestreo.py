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

PATH = "C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/Sujetos"

def reesampling(var):
    x_resampled = signal.resample(var, math.ceil(var.shape[0]*1.33333333333333))
    return x_resampled
 
def process(filePath):
    X = pd.read_csv(filePath, sep="	")
    
    VFSCd = reesampling(X['VFSCd'])
    VFSCi = reesampling(X['VFSCi'])
    PAMn = reesampling(X['PAMn'])
    
    
    reesampled_data = zip(VFSCd,VFSCi, PAMn)
    
    with open(filePath[:(len(filePath)-3)] + "csv", 'w', newline='') as f:
        writer = csv.writer(f, delimiter='\t')
        writer.writerow(["VFSCd", "VFSCi", "PAMn"])
        writer.writerows(reesampled_data)
    
    
listOfFolders = os.listdir(PATH)
for folder in listOfFolders:
    folderPath = PATH+"/"+folder
    listOfFiles = os.listdir(folderPath)
    for file in listOfFiles:
        filePath = folderPath+"/"+file
        process(filePath)