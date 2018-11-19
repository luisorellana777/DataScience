# download dataset
import os
import urllib.request

os.makedirs('./Python/Regresion Logistica - Clasificacion Imagenes/data', exist_ok = True)

urllib.request.urlretrieve('http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz', filename='./Python/Regresion Logistica - Clasificacion Imagenes/data/train-images.gz')
urllib.request.urlretrieve('http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz', filename='./Python/Regresion Logistica - Clasificacion Imagenes/data/train-labels.gz')
urllib.request.urlretrieve('http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz', filename='./Python/Regresion Logistica - Clasificacion Imagenes/data/test-images.gz')
urllib.request.urlretrieve('http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz', filename='./Python/Regresion Logistica - Clasificacion Imagenes/data/test-labels.gz')

