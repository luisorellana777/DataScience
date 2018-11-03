import pandas
import numpy
import seaborn
import scipy
import matplotlib.pyplot as plt
import seaborn as sns
data = pandas.read_csv(‘marscrater_pds.csv’, low_memory=False)
data['DIAM_CIRCLE_IMAGE’] = data['DIAM_CIRCLE_IMAGE’].convert_objects(convert_numeric=True)
data['DEPTH_RIMFLOOR_TOPOG’] = data['DEPTH_RIMFLOOR_TOPOG’].convert_objects(convert_numeric=True)
data['DIAM_CIRCLE_IMAGE’]=data['DIAM_CIRCLE_IMAGE’].replace(’ ’, numpy.nan)
data['DEPTH_RIMFLOOR_TOPOG’]=data['DEPTH_RIMFLOOR_TOPOG’].replace(’ ’, numpy.nan)
data_clean=data.dropna()
print ('association between urbanrate and internetuserate’)
print (scipy.stats.pearsonr(data_clean['DIAM_CIRCLE_IMAGE’], data_clean['DEPTH_RIMFLOOR_TOPOG’]))
scat1 = seaborn.regplot(x=“DIAM_CIRCLE_IMAGE”, y=“DEPTH_RIMFLOOR_TOPOG”, fit_reg=True, data=data_clean)
plt.xlabel('Crater Diameter (km)’)
plt.ylabel('Crater Elevation (km)’)
plt.title('Scatterplot for the Association Between Diameter and Elevation’)
corr = data_clean.corr()
sns.heatmap(corr, 
       xticklabels=corr.columns,
       yticklabels=corr.columns)
