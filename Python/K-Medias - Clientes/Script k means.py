<<<<<<< HEAD
# -*- coding: utf-8 -*-
"""
Editor de Spyder

Este es un archivo temporal

"""

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

X = pd.read_csv(r"C:/Users/Luis.O.A/Documents/Analisis Clientes/Dataset2.csv", sep=";")

min_max_scaler = preprocessing.MinMaxScaler()
df_tr = min_max_scaler.fit_transform(X)
df_tr

#Verificar Cantidad de Clusters
Nc = range(2, 15)
kmeans = [KMeans(n_clusters=i) for i in Nc]
score = [kmeans[i].fit(df_tr).score(df_tr) for i in range(len(kmeans))]

plt.plot(Nc,score)
plt.xlabel('Numero de Clusters')
plt.ylabel('Score')
plt.title('Curva del Codo')
plt.show()



silhouette_score_values = [silhouette_score(df_tr,kmeans[i].labels_ ,metric='euclidean', sample_size=None, random_state=None) for i in range(len(kmeans))]

plt.plot(Nc, silhouette_score_values)
plt.title("Silhouette score values vs Numbers of Clusters ")
plt.show()


kmeans = KMeans(n_clusters=3, random_state=0).fit(df_tr)
labels = kmeans.labels_

#Glue back to originaal data
X['clusters'] = labels

clmns = ['TIPO_EMPRESA',
         'FAMILIA',
         'CANTIDAD_EMPLEADOS',
         'CARTERA',
         'REGION',
         'ESTADO_SERVICIOS',
         'Q_ACUERDOS',
         'COMISION',
         'DEUDA',
         'Q_MESES_COMPRA',
         'MES_PRIMERA_COMPRA',
         'MES_ULTIMA_COMPRA',
         'DIAS_CREDITO',
         'CONDICION_ENTREGA',
         'PUNTOS',
         'PORCENTAJE_NO_PERSONALIZADO',
         'MESES_CON_PEDIDOS',
         'VF_PROMEDIO',
         '%_COBERTURA_SUPERMERCADOS',
         '%_COBERTURA_RESTAURANT',
         '%_COBERTURA_BV',
         '%_COBERTURA_MERCHANTS',
         '$_COBERTURA_SUPERMERCADOS']
#Add the column into our list
clmns.extend(['clusters'])

print (X[clmns].groupby(['clusters']).mean())

C = kmeans.cluster_centers_
colores=['red','green','blue','cyan','yellow']
asignar=[]
for row in labels:
    asignar.append(colores[row])

pca = PCA(n_components=2, whiten=True).fit(df_tr)
X_pca = pca.transform(df_tr)

pca_C = PCA(n_components=2, whiten=True).fit(C)
X_pca_C = pca_C.transform(C)

plt.scatter(X_pca[:, 0], X_pca[:, 1], c=asignar, s=5, cmap='viridis')

plt.scatter(X_pca_C[:, 0], X_pca_C[:, 1], c='black', s=20, alpha=0.5);


'''

pca = PCA(n_components=2, whiten=True).fit(X)
X_pca = pca.transform(X)

#Verificar Cantidad de Clusters
Nc = range(1, 10)
kmeans = [KMeans(n_clusters=i) for i in Nc]
score = [kmeans[i].fit(X_pca).score(X_pca) for i in range(len(kmeans))]
score
plt.plot(Nc,score)
plt.xlabel('Numero de Clusters')
plt.ylabel('Score')
plt.title('Curva del Codo')
plt.show()

#Aplicar optimo de Numero de Clusters
kmeans = KMeans(n_clusters=3).fit(X)
labels = kmeans.predict(X)

C = kmeans.cluster_centers_
colores=['red','green','blue','cyan','yellow']
asignar=[]
for row in labels:
    asignar.append(colores[row])

################################
plt.scatter(X_pca[:, 0], X_pca[:, 1], c=asignar, s=10, cmap='viridis')

plt.scatter(C[:, 0], C[:, 1], c='black', s=50, alpha=0.5);
################################
#Mostrar Centroides en sus dimensiones originales
centers = pca.inverse_transform(kmeans.cluster_centers_)
print(centers)

#Instancias mas cercanas a clusters
closest, _ = pairwise_distances_argmin_min(kmeans.cluster_centers_, X)
print(closest)#indices del dataset


cobertura=X['PESOS_COBERTURA_SUPERMERCADOS'].values
for row in closest:
    print(cobertura[row])


#Predecir con el modelo ya entrenado
X_in = np.array([[-1.30357739e-02,2.28489484e-01,5.18036247e-03,5.15420989e-02,3.05902670e-02,9.99349161e-01,2.60555288e-04,1.95107905e-01,2.11808226e-03,4.26917188e-02,1.58504804e-02,6.45018308e-01,6.03258340e-01,6.35717578e-01,1.29403368e-02,2.49905384e-01,5.10030389e-03,1.03674421e-02,3.62927127e-03,1.07621551e-02,9.89237856e-01,8.57144911e-01,2.27023458e-03,9.26154934e-01,8.09486133e-01,8.69697378e-01,8.21334173e-01,3.97226057e-03,6.62009214e-03]])
X_pca_in = pca.transform(X_in)

print(X_pca_in)
label_in = kmeans.predict(X_pca_in)
print(label_in)#Cluster predicho
'''

'''
CON EL DATASET SIN NORMALIZAR (Pre-Procesado Datos con Formulas.csv)
import numpy as np
import pandas as pd
from scipy import stats
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns
from mpl_toolkits.mplot3d import Axes3D
from sklearn.metrics import pairwise_distances_argmin_min
from scipy.sparse import csr_matrix
from sklearn import preprocessing


df = pd.read_csv('C:/Users/luis.orellana.ext/Documents/Analisis de Clientes/Datos Pro-Procesados.csv',sep=';')


min_max_scaler = preprocessing.MinMaxScaler()
df_tr = min_max_scaler.fit_transform(df)
df_tr

#Cluster the data
kmeans = KMeans(n_clusters=2, random_state=0).fit(df_tr)
labels = kmeans.labels_

#Glue back to originaal data
df['clusters'] = labels

clmns = ['ESTADO_EMPRESA','TIPO_EMPRESA',	'FAMILIA',	'CANTIDAD_EMPLEADOS','CARTERA']
#Add the column into our list
clmns.extend(['clusters'])

#Lets analyze the clusters
print (df[clmns].groupby(['clusters']).mean())
'''
=======
# -*- coding: utf-8 -*-
"""
Editor de Spyder

Este es un archivo temporal

"""

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

X = pd.read_csv(r"C:/Users/Luis.O.A/Documents/Analisis Clientes/Dataset2.csv", sep=";")

min_max_scaler = preprocessing.MinMaxScaler()
df_tr = min_max_scaler.fit_transform(X)
df_tr

#Verificar Cantidad de Clusters
Nc = range(2, 15)
kmeans = [KMeans(n_clusters=i) for i in Nc]
score = [kmeans[i].fit(df_tr).score(df_tr) for i in range(len(kmeans))]

plt.plot(Nc,score)
plt.xlabel('Numero de Clusters')
plt.ylabel('Score')
plt.title('Curva del Codo')
plt.show()



silhouette_score_values = [silhouette_score(df_tr,kmeans[i].labels_ ,metric='euclidean', sample_size=None, random_state=None) for i in range(len(kmeans))]

plt.plot(Nc, silhouette_score_values)
plt.title("Silhouette score values vs Numbers of Clusters ")
plt.show()


kmeans = KMeans(n_clusters=3, random_state=0).fit(df_tr)
labels = kmeans.labels_

#Glue back to originaal data
X['clusters'] = labels

clmns = ['TIPO_EMPRESA',
         'FAMILIA',
         'CANTIDAD_EMPLEADOS',
         'CARTERA',
         'REGION',
         'ESTADO_SERVICIOS',
         'Q_ACUERDOS',
         'COMISION',
         'DEUDA',
         'Q_MESES_COMPRA',
         'MES_PRIMERA_COMPRA',
         'MES_ULTIMA_COMPRA',
         'DIAS_CREDITO',
         'CONDICION_ENTREGA',
         'PUNTOS',
         'PORCENTAJE_NO_PERSONALIZADO',
         'MESES_CON_PEDIDOS',
         'VF_PROMEDIO',
         '%_COBERTURA_SUPERMERCADOS',
         '%_COBERTURA_RESTAURANT',
         '%_COBERTURA_BV',
         '%_COBERTURA_MERCHANTS',
         '$_COBERTURA_SUPERMERCADOS']
#Add the column into our list
clmns.extend(['clusters'])

print (X[clmns].groupby(['clusters']).mean())

C = kmeans.cluster_centers_
colores=['red','green','blue','cyan','yellow']
asignar=[]
for row in labels:
    asignar.append(colores[row])

pca = PCA(n_components=2, whiten=True).fit(df_tr)
X_pca = pca.transform(df_tr)

pca_C = PCA(n_components=2, whiten=True).fit(C)
X_pca_C = pca_C.transform(C)

plt.scatter(X_pca[:, 0], X_pca[:, 1], c=asignar, s=5, cmap='viridis')

plt.scatter(X_pca_C[:, 0], X_pca_C[:, 1], c='black', s=20, alpha=0.5);


'''

pca = PCA(n_components=2, whiten=True).fit(X)
X_pca = pca.transform(X)

#Verificar Cantidad de Clusters
Nc = range(1, 10)
kmeans = [KMeans(n_clusters=i) for i in Nc]
score = [kmeans[i].fit(X_pca).score(X_pca) for i in range(len(kmeans))]
score
plt.plot(Nc,score)
plt.xlabel('Numero de Clusters')
plt.ylabel('Score')
plt.title('Curva del Codo')
plt.show()

#Aplicar optimo de Numero de Clusters
kmeans = KMeans(n_clusters=3).fit(X)
labels = kmeans.predict(X)

C = kmeans.cluster_centers_
colores=['red','green','blue','cyan','yellow']
asignar=[]
for row in labels:
    asignar.append(colores[row])

################################
plt.scatter(X_pca[:, 0], X_pca[:, 1], c=asignar, s=10, cmap='viridis')

plt.scatter(C[:, 0], C[:, 1], c='black', s=50, alpha=0.5);
################################
#Mostrar Centroides en sus dimensiones originales
centers = pca.inverse_transform(kmeans.cluster_centers_)
print(centers)

#Instancias mas cercanas a clusters
closest, _ = pairwise_distances_argmin_min(kmeans.cluster_centers_, X)
print(closest)#indices del dataset


cobertura=X['PESOS_COBERTURA_SUPERMERCADOS'].values
for row in closest:
    print(cobertura[row])


#Predecir con el modelo ya entrenado
X_in = np.array([[-1.30357739e-02,2.28489484e-01,5.18036247e-03,5.15420989e-02,3.05902670e-02,9.99349161e-01,2.60555288e-04,1.95107905e-01,2.11808226e-03,4.26917188e-02,1.58504804e-02,6.45018308e-01,6.03258340e-01,6.35717578e-01,1.29403368e-02,2.49905384e-01,5.10030389e-03,1.03674421e-02,3.62927127e-03,1.07621551e-02,9.89237856e-01,8.57144911e-01,2.27023458e-03,9.26154934e-01,8.09486133e-01,8.69697378e-01,8.21334173e-01,3.97226057e-03,6.62009214e-03]])
X_pca_in = pca.transform(X_in)

print(X_pca_in)
label_in = kmeans.predict(X_pca_in)
print(label_in)#Cluster predicho
'''

'''
CON EL DATASET SIN NORMALIZAR (Pre-Procesado Datos con Formulas.csv)
import numpy as np
import pandas as pd
from scipy import stats
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns
from mpl_toolkits.mplot3d import Axes3D
from sklearn.metrics import pairwise_distances_argmin_min
from scipy.sparse import csr_matrix
from sklearn import preprocessing


df = pd.read_csv('C:/Users/luis.orellana.ext/Documents/Analisis de Clientes/Datos Pro-Procesados.csv',sep=';')


min_max_scaler = preprocessing.MinMaxScaler()
df_tr = min_max_scaler.fit_transform(df)
df_tr

#Cluster the data
kmeans = KMeans(n_clusters=2, random_state=0).fit(df_tr)
labels = kmeans.labels_

#Glue back to originaal data
df['clusters'] = labels

clmns = ['ESTADO_EMPRESA','TIPO_EMPRESA',	'FAMILIA',	'CANTIDAD_EMPLEADOS','CARTERA']
#Add the column into our list
clmns.extend(['clusters'])

#Lets analyze the clusters
print (df[clmns].groupby(['clusters']).mean())
'''
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
