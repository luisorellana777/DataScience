import numpy
import pandas
import statsmodels.formula.api as smf 
import statsmodels.stats.multicomp as multi
import scipy.stats
import seaborn
import matplotlib.pyplot as plt
data = pandas.read_csv(‘addhealth_pds.csv’, low_memory=False)
data['H1FS11’]=data['H1FS11’].replace(0, numpy.nan)
data['H1ED12’]=data['H1ED12’].replace(0, numpy.nan)
data['H1FS11’] = pandas.to_numeric(data['H1FS11’], errors='coerce’)
data['H1ED12’] = pandas.to_numeric(data['H1ED12’], errors='coerce’)
ct=pandas.crosstab(data['H1FS11’], data['H1ED12’])
print (ct)
# column percentages
colsum=ct.sum(axis=0)
colpct=ct/colsum
print(colpct)
# chi-square
cs= scipy.stats.chi2_contingency(ct)
print ('chi-square value %f’%cs[0])
print ('p value %f’%cs[1])
###################Post Hoc Analysis
sub = data.copy()
for a in [1, 2, 3, 4, 5, 6, 7, 8, 9]:
   for b in range((a + 1),10):
       bb = b
       aa = a
       if b == 7:
           bb = 96
       if b == 8:
           bb = 97
       if b == 9:
           bb = 98
       if a == 7:
           aa = 96
       if a == 8:
           aa = 97
       print (’———————–’)
       recode1 = {aa: aa, bb: bb}
       sub['COMP1v2’]= data['H1ED12’].map(recode1)
       # contingency table of observed counts
       ct1=pandas.crosstab(sub['H1FS11’], sub['COMP1v2’])
       cs1= scipy.stats.chi2_contingency(ct1)
       if (cs1[0] > 3.84) & (cs1[1] < 0.003) :
           print ('chi-square value %f’%cs1[0])
           print ('p value %.2E’%cs1[1])
           print (ct1)
           # column percentages
           colsum=ct1.sum(axis=0)
           colpct=ct1/colsum
           print(colpct)
“’ The Best one
———————–
chi-square value 1150.361891
p value 4.31E-249
COMP1v2  1.0   96.0
H1FS11             
1.0       237     1
2.0       605     0
3.0       687     0
6.0         0     3
COMP1v2      1.0   96.0
H1FS11                 
1.0      0.155003  0.25
2.0      0.395683  0.00
3.0      0.449313  0.00
6.0      0.000000  0.75
———————–
”’
################### Moderation Analysis
for a in range(1, 6):
   print ('How close do you feel to your mother? = %d’%a)
   sub2=data[(data['H1WP9’]==a)]
   ct=pandas.crosstab(sub2['H1FS11’], sub2['H1ED12’])
   colsum=ct.sum(axis=0)
   colpct=ct/colsum
   # chi-square
   cs= scipy.stats.chi2_contingency(ct)
   print ('chi-square value %f’%cs[0])
   print ('p value %f’%cs[1])
