<<<<<<< HEAD
from __future__ import print_function
import pandas as pd
import numpy
import matplotlib.pyplot as plt
import math
from keras.models import load_model, Model, Sequential
from keras.layers import Dense, Activation, Dropout, Input, LSTM, Reshape, Lambda, RepeatVector
from keras.initializers import glorot_uniform
from keras.utils import to_categorical
from keras.optimizers import Adam
from keras import backend as K
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error
from pandas import DataFrame
from pandas.plotting import table
from math import sqrt
from joblib import Parallel, delayed
import tensorflow as tf
from scipy import stats


class Entrenamiento:
    '''    
    #Por batch
    def correlation_coefficient_loss(y_true, y_pred):
        fsp = y_pred - K.mean(y_pred) #being K.mean a scalar here, it will be automatically subtracted from all elements in y_pred
        fst = y_true - K.mean(y_true)
        
        devP = K.std(y_pred)
        devT = K.std(y_true)
        
        return K.mean(fsp*fst)/(devP*devT)
        
    #Por cada ejemplo
    def correlation_coefficient_loss(y_true, y_pred):
        #original shapes: (batch, 10)
        
        fsp = y_pred - K.mean(y_pred,axis=0) #you take the mean over the batch, keeping the features separate.   
        fst = y_true - K.mean(y_true,axis=0) 
        #mean shape: (1,10)
        #fst shape keeps (batch,10)
        
        devP = K.std(y_pred,axis=0)  
        devt = K.std(y_true,axis=0)
        #dev shape: (1,10)
        #mean shape: (1,10), making all tensors in the expression be (1,10). 
        #sum is only necessary because we need a single loss value
        return K.sum(K.mean(fsp*fst,axis=0)/(devP*devT))
    '''       
    
    def correlation_coefficient_loss(y_true, y_pred):
        x = y_true
        y = y_pred
        mx = K.mean(x)
        my = K.mean(y)
        xm, ym = x-mx, y-my
        r_num = K.sum(tf.multiply(xm,ym))
        r_den = K.sqrt(tf.multiply(K.sum(K.square(xm)), K.sum(K.square(ym))))
        r = r_num / r_den
    
        r = K.maximum(K.minimum(r, 1.0), -1.0)
        return 1 - K.square(r)
    # fit an LSTM network to training data
    def fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, dropout, hidden_layers, neurons):
        
        model = Sequential()
        model.add(LSTM(neurons, batch_input_shape=(batch_size, trainX.shape[1], trainX.shape[2]), stateful=True, return_sequences=True))
        for i in range (hidden_layers-1):
            model.add(LSTM(neurons, batch_input_shape=(batch_size, trainX.shape[1], trainX.shape[2]), stateful=True))
        model.add(Dropout(dropout))
        model.add(Dense(1, activation=activation))
        model.compile(loss=Entrenamiento.correlation_coefficient_loss, optimizer=optimization)
        
        for i in range(epochs):
            model.fit(trainX, trainY, epochs=1, batch_size=batch_size, verbose=2, shuffle=False)
            model.reset_states()
    
        return model
    
    #Evaluate the Model
    def evaluate(model, X, Y, batch_size):
        
        output = model.predict(X, batch_size=batch_size)
        # invert data transforms on forecast
        yhat = list()
        for i in range(len(output)):
            # store forecast
            yhat.append(output[i,0])
        # report performance
        rmse = stats.pearsonr(Y[:,0], yhat)
        
        return rmse
    
    # Guardar en formato PNG los resultados
    def save_results(results, nombre_archivo_resultados):
        
        desc = results.describe()
        #create a subplot without frame
        plot = plt.subplot(111, frame_on=False)
        
        #remove axis
        plot.xaxis.set_visible(False) 
        plot.yaxis.set_visible(False) 
        
        #create the table plot and position it in the upper left corner
        table(plot, desc,loc='upper right')
        
        #save the plot as a png file
        plt.savefig('desc_%s.png'%(nombre_archivo_resultados))
        plt.close()
        
        results.boxplot()
        plt.savefig('boxplot_%s.png'%(nombre_archivo_resultados))
        plt.close()
    
        # run a repeated experiment
    def experiment(trainX, trainY, testX, testY, repeats, batch_size, epochs, optimization, activation, dropout, hidden_layers, neurons):
        # run experiment
        error_scores = list()
        for r in range(repeats):
            # fit the model
            lstm_model = Entrenamiento.fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, dropout, hidden_layers, neurons)
            
            lstm_model.predict(trainX, batch_size=batch_size)
            
        # report performance
        rmse = Entrenamiento.evaluate(lstm_model, testX, testY, batch_size)
        print('%d) Test RMSE: %.3f' % (r+1, rmse))
        error_scores.append(neurons)
        return error_scores
        
#QUITAR PARALEL DE ESTE METODO PARA CORRER LAS PRUEBAS

    def run_experiment(trainX, trainY, testX, testY, batch_size=[1], epochs=[1], optimization=["adam"], activation=["sigmoid"], dropout=[0], hidden_layers=[1], neurons=[1]):
        
        results = []
        results_data_frame = DataFrame()
        repeats = 1
        
        if (len(batch_size) > 1):
            for b in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, b, epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0])
        
        if (len(epochs) > 1):
            for e in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], e, optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0])

        if (len(optimization) > 1):
            for o in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], o, activation[0], dropout[0], hidden_layers[0], neurons[0])
                
        if (len(activation) > 1):
            for a in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], a, dropout[0], hidden_layers[0], neurons[0])
                
        if (len(dropout) > 1):
            for d in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], d, hidden_layers[0], neurons[0])
                
        if (len(hidden_layers) > 1):
            for h in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], h, neurons[0])
                
        if (len(neurons) > 1):
            for n in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], n)
        '''
        if (len(batch_size) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, b, epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0]) for b in batch_size)
            
        elif (len(epochs) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], e, optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0]) for e in epochs)
            
        elif (len(optimization) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], o, activation[0], dropout[0], hidden_layers[0], neurons[0]) for o in optimization)
            
        elif (len(activation) > 1):
            results = Parallel(n_jobs=4)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], a, dropout[0], hidden_layers[0], neurons[0]) for a in activation)
            
        elif (len(dropout) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], d, hidden_layers[0], neurons[0]) for d in dropout)
            
        elif (len(hidden_layers) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], h, neurons[0]) for h in hidden_layers)
            
        elif (len(neurons) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], n) for n in neurons)
        '''
        for i in range(len(results)):
            results_data_frame[str(i)] = results[i]
        
        # summarize results
        print(results);
        #print(results_data_frame.describe())
        # save boxplot
        #Entrenamiento.save_results(results_data_frame, "nombre_archivo")

###########################################################################################################    
nombre_sujeto = "AC"
nombre_postura = "ACOSTADO"
nombre_emisferio = "DERECHO"


PATH = ("C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/Sujetos/Muestreo 0.4/%s/%s-%s-VE.csv"%(nombre_sujeto, nombre_sujeto, nombre_postura))
X = pd.read_csv(PATH, sep="	")

# normalize the dataset
scaler = MinMaxScaler(feature_range=(-1, 1))

VFSCd = scaler.fit_transform(X.VFSCd.values.reshape((len(X.VFSCd.values), 1)))
VFSCi = scaler.fit_transform(X.VFSCi.values.reshape((len(X.VFSCi.values), 1)))
PAMn = scaler.fit_transform(X.PAMn.values.reshape((len(X.PAMn.values), 1)))
# fix random seed for reproducibility
numpy.random.seed(7)

#Dar formato float a las señales
PAMn, VFSCd = PAMn.astype('float32'), VFSCd.astype('float32')
PAMn, VFSCd = numpy.array(PAMn), numpy.array(VFSCd)

# Validacion Valanceada
train_size = int(len(PAMn) * 0.75)
test_size = len(PAMn) - train_size
train_PAM, train_VFSCd = PAMn[0:train_size], VFSCd[0:train_size]
test_PAM, test_VFSCd = PAMn[train_size:len(PAMn)], VFSCd[train_size:len(VFSCd)]

# Reshape segun el formato que acepta Keras
# reshape input to be [samples, time steps, features]
train_PAM = numpy.reshape(train_PAM, (train_PAM.shape[0], 1, train_PAM.shape[1]))
test_PAM = numpy.reshape(test_PAM, (test_PAM.shape[0], 1, test_PAM.shape[1]))
# create and fit the LSTM network
##############################################################################################################################################################
##Hyperparametros
numpy.random.uniform(low=0.5, high=13.3, size=(5,))
#################
nombre_archivo_resultados = nombre_sujeto+"_"+nombre_postura+"_"+nombre_emisferio
Entrenamiento.run_experiment(train_PAM, train_VFSCd, test_PAM, test_VFSCd, epochs = [2,3])
##############################################################################################################################################################

        
###########################################################################################################################
#random search
#https://github.com/llSourcell/hyperparameter_optimization_strategies/blob/master/random_search.py


Parallel(n_jobs=2)(delayed(sqrt)(i ** 2) for i in range(10))

#################








# Use scikit-learn to grid search the batch size and epochs
import pandas as pd
import numpy
import keras.backend as K
import tensorflow as tf
import math
from scipy import stats
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import GridSearchCV
from keras.models import Sequential
from keras.layers import Dense, Activation, Dropout, Input, LSTM, Reshape, Lambda, RepeatVector
from keras.wrappers.scikit_learn import KerasRegressor

    
def correlation_coefficient_loss(y_true, y_pred):
    fsp = y_pred - K.mean(y_pred,axis=0) #you take the mean over the batch, keeping the features separate.   
    fst = y_true - K.mean(y_true,axis=0) 
        #mean shape: (1,10)
        #fst shape keeps (batch,10)
    
    devP = K.std(y_pred,axis=0)  
    devT = K.std(y_true,axis=0)
        #dev shape: (1,10)
    
    return K.sum(K.mean(fsp*fst,axis=0)/(devP*devT))

# Function to create model, required for KerasClassifier
def create_model(neurons=1):
    # create model
    model = Sequential()
    #model.add(LSTM(1, return_sequences=True))
    model.add(LSTM(neurons))
    model.add(Dense(1))
    # Compile model
    model.compile(loss=correlation_coefficient_loss, optimizer='adam')
    return model

def create_dataset():
    nombre_sujeto = "AC"
    nombre_postura = "ACOSTADO"
    nombre_emisferio = "DERECHO"
    
    PATH = ("C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/Sujetos/Muestreo 0.4/%s/%s-%s-VE.csv"%(nombre_sujeto, nombre_sujeto, nombre_postura))
    X = pd.read_csv(PATH, sep="	")
    
    # normalize the dataset
    scaler = MinMaxScaler(feature_range=(-1, 1))
    
    VFSCd = scaler.fit_transform(X.VFSCd.values.reshape((len(X.VFSCd.values), 1)))
    VFSCi = scaler.fit_transform(X.VFSCi.values.reshape((len(X.VFSCi.values), 1)))
    PAMn = scaler.fit_transform(X.PAMn.values.reshape((len(X.PAMn.values), 1)))
    # fix random seed for reproducibility
    numpy.random.seed(7)
    
    #Dar formato float a las señales
    PAMn, VFSCd, VFSCi = PAMn.astype('float32'), VFSCd.astype('float32'), VFSCi.astype('float32')
    PAMn, VFSCd, VFSCi = numpy.array(PAMn), numpy.array(VFSCd), numpy.array(VFSCi)
    
    
    # Reshape segun el formato que acepta Keras
    # reshape input to be [samples, time steps, features]
    PAMn = numpy.reshape(PAMn, (PAMn.shape[0], 1, PAMn.shape[1]))
    return PAMn, VFSCd, VFSCi

def run_experiment(hyperparam_grid):
    # create model
    model = KerasRegressor(build_fn=create_model, verbose=0)

    grid = GridSearchCV(estimator=model, param_grid=hyperparam_grid, cv=5, n_jobs=1)
    grid_result = grid.fit(X, Y1)
    # summarize results
    print("Best: %f using %s" % (grid_result.best_score_, grid_result.best_params_))
    means = grid_result.cv_results_['mean_test_score']
    stds = grid_result.cv_results_['std_test_score']
    params = grid_result.cv_results_['params']
    for mean, stdev, param in zip(means, stds, params):
        print("%f (%f) with: %r" % (mean, stdev, param))
    

# fix random seed for reproducibility
seed = 7
numpy.random.seed(seed)
# load dataset
# split into input (X) and output (Y) variables
X, Y1, Y2 = create_dataset()

# define the grid search parameters
##Tuning the Number of Epochs
##Tuning the Batch Size
##Tuning the Number of Neurons
##Tuning the Number of Hidden Layers
#################################################################### batch_size
batch_size = []
for i in range(1,500):
    if (math.floor(X.shape[0] - (X.shape[0]/5))%i)==0:
        batch_size.append(i)

hyperparam_grid = dict(batch_size=batch_size)
run_experiment(hyperparam_grid)
        
######################################################################## epochs
#epochs = numpy.random.randint(low=1, high=10000, size=(10,))
epochs = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000]

hyperparam_grid = dict(epochs=epochs)
run_experiment(hyperparam_grid)

####################################################################### neurons
neurons = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

hyperparam_grid = dict(neurons=neurons)
run_experiment(hyperparam_grid)

################################################################## dropout_rate
dropout_rate = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
hyperparam_grid = dict(dropout_rate=dropout_rate)
run_experiment(hyperparam_grid)





batch_size = [1]

epochs = [100]

neurons = [10]

hyperparam_grid = dict(batch_size=batch_size, epochs=epochs, neurons=neurons)
=======
from __future__ import print_function
import pandas as pd
import numpy
import matplotlib.pyplot as plt
import math
from keras.models import load_model, Model, Sequential
from keras.layers import Dense, Activation, Dropout, Input, LSTM, Reshape, Lambda, RepeatVector
from keras.initializers import glorot_uniform
from keras.utils import to_categorical
from keras.optimizers import Adam
from keras import backend as K
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error
from pandas import DataFrame
from pandas.plotting import table
from math import sqrt
from joblib import Parallel, delayed
import tensorflow as tf
from scipy import stats


class Entrenamiento:
    '''    
    #Por batch
    def correlation_coefficient_loss(y_true, y_pred):
        fsp = y_pred - K.mean(y_pred) #being K.mean a scalar here, it will be automatically subtracted from all elements in y_pred
        fst = y_true - K.mean(y_true)
        
        devP = K.std(y_pred)
        devT = K.std(y_true)
        
        return K.mean(fsp*fst)/(devP*devT)
        
    #Por cada ejemplo
    def correlation_coefficient_loss(y_true, y_pred):
        #original shapes: (batch, 10)
        
        fsp = y_pred - K.mean(y_pred,axis=0) #you take the mean over the batch, keeping the features separate.   
        fst = y_true - K.mean(y_true,axis=0) 
        #mean shape: (1,10)
        #fst shape keeps (batch,10)
        
        devP = K.std(y_pred,axis=0)  
        devt = K.std(y_true,axis=0)
        #dev shape: (1,10)
        #mean shape: (1,10), making all tensors in the expression be (1,10). 
        #sum is only necessary because we need a single loss value
        return K.sum(K.mean(fsp*fst,axis=0)/(devP*devT))
    '''       
    
    def correlation_coefficient_loss(y_true, y_pred):
        x = y_true
        y = y_pred
        mx = K.mean(x)
        my = K.mean(y)
        xm, ym = x-mx, y-my
        r_num = K.sum(tf.multiply(xm,ym))
        r_den = K.sqrt(tf.multiply(K.sum(K.square(xm)), K.sum(K.square(ym))))
        r = r_num / r_den
    
        r = K.maximum(K.minimum(r, 1.0), -1.0)
        return 1 - K.square(r)
    # fit an LSTM network to training data
    def fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, dropout, hidden_layers, neurons):
        
        model = Sequential()
        model.add(LSTM(neurons, batch_input_shape=(batch_size, trainX.shape[1], trainX.shape[2]), stateful=True, return_sequences=True))
        for i in range (hidden_layers-1):
            model.add(LSTM(neurons, batch_input_shape=(batch_size, trainX.shape[1], trainX.shape[2]), stateful=True))
        model.add(Dropout(dropout))
        model.add(Dense(1, activation=activation))
        model.compile(loss=Entrenamiento.correlation_coefficient_loss, optimizer=optimization)
        
        for i in range(epochs):
            model.fit(trainX, trainY, epochs=1, batch_size=batch_size, verbose=2, shuffle=False)
            model.reset_states()
    
        return model
    
    #Evaluate the Model
    def evaluate(model, X, Y, batch_size):
        
        output = model.predict(X, batch_size=batch_size)
        # invert data transforms on forecast
        yhat = list()
        for i in range(len(output)):
            # store forecast
            yhat.append(output[i,0])
        # report performance
        rmse = stats.pearsonr(Y[:,0], yhat)
        
        return rmse
    
    # Guardar en formato PNG los resultados
    def save_results(results, nombre_archivo_resultados):
        
        desc = results.describe()
        #create a subplot without frame
        plot = plt.subplot(111, frame_on=False)
        
        #remove axis
        plot.xaxis.set_visible(False) 
        plot.yaxis.set_visible(False) 
        
        #create the table plot and position it in the upper left corner
        table(plot, desc,loc='upper right')
        
        #save the plot as a png file
        plt.savefig('desc_%s.png'%(nombre_archivo_resultados))
        plt.close()
        
        results.boxplot()
        plt.savefig('boxplot_%s.png'%(nombre_archivo_resultados))
        plt.close()
    
        # run a repeated experiment
    def experiment(trainX, trainY, testX, testY, repeats, batch_size, epochs, optimization, activation, dropout, hidden_layers, neurons):
        # run experiment
        error_scores = list()
        for r in range(repeats):
            # fit the model
            lstm_model = Entrenamiento.fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, dropout, hidden_layers, neurons)
            
            lstm_model.predict(trainX, batch_size=batch_size)
            
        # report performance
        rmse = Entrenamiento.evaluate(lstm_model, testX, testY, batch_size)
        print('%d) Test RMSE: %.3f' % (r+1, rmse))
        error_scores.append(neurons)
        return error_scores
        
#QUITAR PARALEL DE ESTE METODO PARA CORRER LAS PRUEBAS

    def run_experiment(trainX, trainY, testX, testY, batch_size=[1], epochs=[1], optimization=["adam"], activation=["sigmoid"], dropout=[0], hidden_layers=[1], neurons=[1]):
        
        results = []
        results_data_frame = DataFrame()
        repeats = 1
        
        if (len(batch_size) > 1):
            for b in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, b, epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0])
        
        if (len(epochs) > 1):
            for e in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], e, optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0])

        if (len(optimization) > 1):
            for o in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], o, activation[0], dropout[0], hidden_layers[0], neurons[0])
                
        if (len(activation) > 1):
            for a in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], a, dropout[0], hidden_layers[0], neurons[0])
                
        if (len(dropout) > 1):
            for d in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], d, hidden_layers[0], neurons[0])
                
        if (len(hidden_layers) > 1):
            for h in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], h, neurons[0])
                
        if (len(neurons) > 1):
            for n in batch_size:
                Entrenamiento.experiment(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], n)
        '''
        if (len(batch_size) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, b, epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0]) for b in batch_size)
            
        elif (len(epochs) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], e, optimization[0], activation[0], dropout[0], hidden_layers[0], neurons[0]) for e in epochs)
            
        elif (len(optimization) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], o, activation[0], dropout[0], hidden_layers[0], neurons[0]) for o in optimization)
            
        elif (len(activation) > 1):
            results = Parallel(n_jobs=4)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], a, dropout[0], hidden_layers[0], neurons[0]) for a in activation)
            
        elif (len(dropout) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], d, hidden_layers[0], neurons[0]) for d in dropout)
            
        elif (len(hidden_layers) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], h, neurons[0]) for h in hidden_layers)
            
        elif (len(neurons) > 1):
            results = Parallel(n_jobs=3)(delayed (Entrenamiento.experiment)(trainX, trainY, testX, testY, repeats, batch_size[0], epochs[0], optimization[0], activation[0], dropout[0], hidden_layers[0], n) for n in neurons)
        '''
        for i in range(len(results)):
            results_data_frame[str(i)] = results[i]
        
        # summarize results
        print(results);
        #print(results_data_frame.describe())
        # save boxplot
        #Entrenamiento.save_results(results_data_frame, "nombre_archivo")

###########################################################################################################    
nombre_sujeto = "AC"
nombre_postura = "ACOSTADO"
nombre_emisferio = "DERECHO"


PATH = ("C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/Sujetos/Muestreo 0.4/%s/%s-%s-VE.csv"%(nombre_sujeto, nombre_sujeto, nombre_postura))
X = pd.read_csv(PATH, sep="	")

# normalize the dataset
scaler = MinMaxScaler(feature_range=(-1, 1))

VFSCd = scaler.fit_transform(X.VFSCd.values.reshape((len(X.VFSCd.values), 1)))
VFSCi = scaler.fit_transform(X.VFSCi.values.reshape((len(X.VFSCi.values), 1)))
PAMn = scaler.fit_transform(X.PAMn.values.reshape((len(X.PAMn.values), 1)))
# fix random seed for reproducibility
numpy.random.seed(7)

#Dar formato float a las señales
PAMn, VFSCd = PAMn.astype('float32'), VFSCd.astype('float32')
PAMn, VFSCd = numpy.array(PAMn), numpy.array(VFSCd)

# Validacion Valanceada
train_size = int(len(PAMn) * 0.75)
test_size = len(PAMn) - train_size
train_PAM, train_VFSCd = PAMn[0:train_size], VFSCd[0:train_size]
test_PAM, test_VFSCd = PAMn[train_size:len(PAMn)], VFSCd[train_size:len(VFSCd)]

# Reshape segun el formato que acepta Keras
# reshape input to be [samples, time steps, features]
train_PAM = numpy.reshape(train_PAM, (train_PAM.shape[0], 1, train_PAM.shape[1]))
test_PAM = numpy.reshape(test_PAM, (test_PAM.shape[0], 1, test_PAM.shape[1]))
# create and fit the LSTM network
##############################################################################################################################################################
##Hyperparametros
numpy.random.uniform(low=0.5, high=13.3, size=(5,))
#################
nombre_archivo_resultados = nombre_sujeto+"_"+nombre_postura+"_"+nombre_emisferio
Entrenamiento.run_experiment(train_PAM, train_VFSCd, test_PAM, test_VFSCd, epochs = [2,3])
##############################################################################################################################################################

        
###########################################################################################################################
#random search
#https://github.com/llSourcell/hyperparameter_optimization_strategies/blob/master/random_search.py


Parallel(n_jobs=2)(delayed(sqrt)(i ** 2) for i in range(10))

#################








# Use scikit-learn to grid search the batch size and epochs
import pandas as pd
import numpy
import keras.backend as K
import tensorflow as tf
import math
from scipy import stats
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import GridSearchCV
from keras.models import Sequential
from keras.layers import Dense, Activation, Dropout, Input, LSTM, Reshape, Lambda, RepeatVector
from keras.wrappers.scikit_learn import KerasRegressor

    
def correlation_coefficient_loss(y_true, y_pred):
    fsp = y_pred - K.mean(y_pred,axis=0) #you take the mean over the batch, keeping the features separate.   
    fst = y_true - K.mean(y_true,axis=0) 
        #mean shape: (1,10)
        #fst shape keeps (batch,10)
    
    devP = K.std(y_pred,axis=0)  
    devT = K.std(y_true,axis=0)
        #dev shape: (1,10)
    
    return K.sum(K.mean(fsp*fst,axis=0)/(devP*devT))

# Function to create model, required for KerasClassifier
def create_model(neurons=1):
    # create model
    model = Sequential()
    #model.add(LSTM(1, return_sequences=True))
    model.add(LSTM(neurons))
    model.add(Dense(1))
    # Compile model
    model.compile(loss=correlation_coefficient_loss, optimizer='adam')
    return model

def create_dataset():
    nombre_sujeto = "AC"
    nombre_postura = "ACOSTADO"
    nombre_emisferio = "DERECHO"
    
    PATH = ("C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/Sujetos/Muestreo 0.4/%s/%s-%s-VE.csv"%(nombre_sujeto, nombre_sujeto, nombre_postura))
    X = pd.read_csv(PATH, sep="	")
    
    # normalize the dataset
    scaler = MinMaxScaler(feature_range=(-1, 1))
    
    VFSCd = scaler.fit_transform(X.VFSCd.values.reshape((len(X.VFSCd.values), 1)))
    VFSCi = scaler.fit_transform(X.VFSCi.values.reshape((len(X.VFSCi.values), 1)))
    PAMn = scaler.fit_transform(X.PAMn.values.reshape((len(X.PAMn.values), 1)))
    # fix random seed for reproducibility
    numpy.random.seed(7)
    
    #Dar formato float a las señales
    PAMn, VFSCd, VFSCi = PAMn.astype('float32'), VFSCd.astype('float32'), VFSCi.astype('float32')
    PAMn, VFSCd, VFSCi = numpy.array(PAMn), numpy.array(VFSCd), numpy.array(VFSCi)
    
    
    # Reshape segun el formato que acepta Keras
    # reshape input to be [samples, time steps, features]
    PAMn = numpy.reshape(PAMn, (PAMn.shape[0], 1, PAMn.shape[1]))
    return PAMn, VFSCd, VFSCi

def run_experiment(hyperparam_grid):
    # create model
    model = KerasRegressor(build_fn=create_model, verbose=0)

    grid = GridSearchCV(estimator=model, param_grid=hyperparam_grid, cv=5, n_jobs=1)
    grid_result = grid.fit(X, Y1)
    # summarize results
    print("Best: %f using %s" % (grid_result.best_score_, grid_result.best_params_))
    means = grid_result.cv_results_['mean_test_score']
    stds = grid_result.cv_results_['std_test_score']
    params = grid_result.cv_results_['params']
    for mean, stdev, param in zip(means, stds, params):
        print("%f (%f) with: %r" % (mean, stdev, param))
    

# fix random seed for reproducibility
seed = 7
numpy.random.seed(seed)
# load dataset
# split into input (X) and output (Y) variables
X, Y1, Y2 = create_dataset()

# define the grid search parameters
##Tuning the Number of Epochs
##Tuning the Batch Size
##Tuning the Number of Neurons
##Tuning the Number of Hidden Layers
#################################################################### batch_size
batch_size = []
for i in range(1,500):
    if (math.floor(X.shape[0] - (X.shape[0]/5))%i)==0:
        batch_size.append(i)

hyperparam_grid = dict(batch_size=batch_size)
run_experiment(hyperparam_grid)
        
######################################################################## epochs
#epochs = numpy.random.randint(low=1, high=10000, size=(10,))
epochs = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000]

hyperparam_grid = dict(epochs=epochs)
run_experiment(hyperparam_grid)

####################################################################### neurons
neurons = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

hyperparam_grid = dict(neurons=neurons)
run_experiment(hyperparam_grid)

################################################################## dropout_rate
dropout_rate = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
hyperparam_grid = dict(dropout_rate=dropout_rate)
run_experiment(hyperparam_grid)





batch_size = [1]

epochs = [100]

neurons = [10]

hyperparam_grid = dict(batch_size=batch_size, epochs=epochs, neurons=neurons)
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
run_experiment(hyperparam_grid)