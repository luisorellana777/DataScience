<<<<<<< HEAD
import numpy
import pandas
import statsmodels.formula.api as smf
import statsmodels.stats.multicomp as multi 
import matplotlib.pyplot as plt
import seaborn as sns
data = pandas.read_csv(‘addhealth_pds.csv’, low_memory=False)
data = data[['IYEAR’, 'H1GI1Y’, 'H1FS15’]].dropna()
data['AGE’]=data['IYEAR’] - data['H1GI1Y’]
data['AGE’]= data['AGE’].convert_objects(convert_numeric=True)
sub1 = data[['AGE’, 'H1FS15’]].dropna()
sub1=sub1[(sub1['AGE’] >= 13)&(sub1['AGE’] <= 17)]
print ('means for AGE by feeling tired status’)
m1= sub1.groupby('H1FS15’).mean()
print (m1)
print ('standard deviations for AGE by feeling tired status’)
sd1 = sub1.groupby('H1FS15’).std()
print (sd1)
mc1 = multi.MultiComparison(sub1['AGE’], sub1['H1FS15’])
res1 = mc1.tukeyhsd()
print(res1.summary())
sns.distplot(sub1['AGE’]);
=======
import numpy
import pandas
import statsmodels.formula.api as smf
import statsmodels.stats.multicomp as multi 
import matplotlib.pyplot as plt
import seaborn as sns
data = pandas.read_csv(‘addhealth_pds.csv’, low_memory=False)
data = data[['IYEAR’, 'H1GI1Y’, 'H1FS15’]].dropna()
data['AGE’]=data['IYEAR’] - data['H1GI1Y’]
data['AGE’]= data['AGE’].convert_objects(convert_numeric=True)
sub1 = data[['AGE’, 'H1FS15’]].dropna()
sub1=sub1[(sub1['AGE’] >= 13)&(sub1['AGE’] <= 17)]
print ('means for AGE by feeling tired status’)
m1= sub1.groupby('H1FS15’).mean()
print (m1)
print ('standard deviations for AGE by feeling tired status’)
sd1 = sub1.groupby('H1FS15’).std()
print (sd1)
mc1 = multi.MultiComparison(sub1['AGE’], sub1['H1FS15’])
res1 = mc1.tukeyhsd()
print(res1.summary())
sns.distplot(sub1['AGE’]);
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
