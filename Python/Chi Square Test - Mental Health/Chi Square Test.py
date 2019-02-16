<<<<<<< HEAD
import pandas
import numpy
import scipy.stats
import seaborn
import matplotlib.pyplot as plt
data = pandas.read_csv(‘addhealth_pds.csv’, low_memory=False)
data['BIO_SEX’]=data['BIO_SEX’].replace(0, numpy.nan)
data['H1GH28’]=data['H1GH28’].replace(0, numpy.nan)
data['BIO_SEX’] = pandas.to_numeric(data['BIO_SEX’], errors='coerce’)
data['H1GH28’] = pandas.to_numeric(data['H1GH28’], errors='coerce’)
#There is a six within the data
data = data[(data['BIO_SEX’] == 1) | (data['BIO_SEX’] == 2)]
ct=pandas.crosstab(data['BIO_SEX’], data['H1GH28’])
print (ct)
# column percentages
colsum=ct.sum(axis=0)
colpct=ct/colsum
print(colpct)
# chi-square
print ('chi-square value, p value, expected counts’)
cs= scipy.stats.chi2_contingency(ct)
print (cs)
# set variable types 
data[“H1GH28”] = data[“H1GH28”].astype('category’)
# new code for setting variables to numeric:
data['BIO_SEX’] = pandas.to_numeric(data['BIO_SEX’], errors='coerce’)
# graph percent with nicotine dependence within each smoking frequency group 
seaborn.factorplot(x=“H1GH28”, y=“BIO_SEX”, data=data, kind=“bar”, ci=None)
plt.xlabel('Thougts of Weight’)
plt.ylabel('Bio Sex’)


###################Post Hoc Analysis
#make a copy of my new subsetted data
sub = data.copy()
for a in [1, 2, 3, 4, 5, 6]:
   for b in range((a + 1),9):
       if b != 7:
           print (’———————–’)
           recode1 = {a: a, b: b}
           sub['COMP1v2’]= data['H1GH28’].map(recode1)
           # contingency table of observed counts
           ct1=pandas.crosstab(sub['BIO_SEX’], sub['COMP1v2’])
           cs1= scipy.stats.chi2_contingency(ct1)
           if (cs1[0] > 3.84) & (cs1[1] < 0.003) :
               print ('chi-square value %f’%cs1[0])
               print ('p value %.2E’%cs1[1])
               print (ct1)
               # column percentages
               colsum=ct1.sum(axis=0)
               colpct=ct1/colsum
               print(colpct)
=======
import pandas
import numpy
import scipy.stats
import seaborn
import matplotlib.pyplot as plt
data = pandas.read_csv(‘addhealth_pds.csv’, low_memory=False)
data['BIO_SEX’]=data['BIO_SEX’].replace(0, numpy.nan)
data['H1GH28’]=data['H1GH28’].replace(0, numpy.nan)
data['BIO_SEX’] = pandas.to_numeric(data['BIO_SEX’], errors='coerce’)
data['H1GH28’] = pandas.to_numeric(data['H1GH28’], errors='coerce’)
#There is a six within the data
data = data[(data['BIO_SEX’] == 1) | (data['BIO_SEX’] == 2)]
ct=pandas.crosstab(data['BIO_SEX’], data['H1GH28’])
print (ct)
# column percentages
colsum=ct.sum(axis=0)
colpct=ct/colsum
print(colpct)
# chi-square
print ('chi-square value, p value, expected counts’)
cs= scipy.stats.chi2_contingency(ct)
print (cs)
# set variable types 
data[“H1GH28”] = data[“H1GH28”].astype('category’)
# new code for setting variables to numeric:
data['BIO_SEX’] = pandas.to_numeric(data['BIO_SEX’], errors='coerce’)
# graph percent with nicotine dependence within each smoking frequency group 
seaborn.factorplot(x=“H1GH28”, y=“BIO_SEX”, data=data, kind=“bar”, ci=None)
plt.xlabel('Thougts of Weight’)
plt.ylabel('Bio Sex’)


###################Post Hoc Analysis
#make a copy of my new subsetted data
sub = data.copy()
for a in [1, 2, 3, 4, 5, 6]:
   for b in range((a + 1),9):
       if b != 7:
           print (’———————–’)
           recode1 = {a: a, b: b}
           sub['COMP1v2’]= data['H1GH28’].map(recode1)
           # contingency table of observed counts
           ct1=pandas.crosstab(sub['BIO_SEX’], sub['COMP1v2’])
           cs1= scipy.stats.chi2_contingency(ct1)
           if (cs1[0] > 3.84) & (cs1[1] < 0.003) :
               print ('chi-square value %f’%cs1[0])
               print ('p value %.2E’%cs1[1])
               print (ct1)
               # column percentages
               colsum=ct1.sum(axis=0)
               colpct=ct1/colsum
               print(colpct)
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
