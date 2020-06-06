import os
#FORZAR USO DE CPU
os.environ['CUDA_VISIBLE_DEVICES'] = '0'
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 

import time
import pandas as pd
import numpy
import matplotlib.pyplot as plt
from keras import Sequential
from keras.models import load_model, Model
from keras.layers import Dense, Activation, Dropout, Input, LSTM, Reshape, Lambda, RepeatVector
from keras.initializers import glorot_uniform
from keras.utils import to_categorical
from keras.optimizers import Adam
from keras import backend as K
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error
from pandas import DataFrame
from pandas.plotting import table
from math import sqrt, log, exp
import math
from joblib import Parallel, delayed
import tensorflow as tf
import gc
from statistics import mean,stdev
from functools import reduce
from scipy import stats
from numpy.random import seed
from tensorflow import set_random_seed

CPU_USAGE = 7
GPU_USAGE = 1

def create_dataset(nombre_sujeto, nombre_postura):
    PATH = ("C:/Users/Luis/Documents/DataScience/Tesis/Datos/SUJETOS/%s/%s-%s-VE.csv"%(nombre_sujeto, nombre_sujeto, nombre_postura))
    X = pd.read_csv(PATH, sep="	")
    
    # normalize the dataset
    scaler = MinMaxScaler(feature_range=(0, 1))
    
    VFSCd = scaler.fit_transform(X.VFSCd.values.reshape((len(X.VFSCd.values), 1)))
    VFSCi = scaler.fit_transform(X.VFSCi.values.reshape((len(X.VFSCi.values), 1)))
    PAMn = scaler.fit_transform(X.PAMn.values.reshape((len(X.PAMn.values), 1)))
    
    #Dar formato float a las señales
    PAMn, VFSCd = PAMn.astype('float32'), VFSCd.astype('float32')
    PAMn, VFSCd = numpy.array(PAMn), numpy.array(VFSCd)
    
    # Validacion Valanceada
    train_size = int(len(PAMn) * 0.5)
    test_size = len(PAMn) - train_size
    train_PAM, train_VFSCd = PAMn[0:train_size], VFSCd[0:train_size]
    test_PAM, test_VFSCd = PAMn[train_size:len(PAMn)], VFSCd[train_size:len(VFSCd)]
    
    # Reshape segun el formato que acepta Keras
    # reshape input to be [samples, time steps, features]
    train_PAM = numpy.reshape(train_PAM, (train_PAM.shape[0], 1, train_PAM.shape[1]))
    test_PAM = numpy.reshape(test_PAM, (test_PAM.shape[0], 1, test_PAM.shape[1]))
    
    return train_PAM, train_VFSCd, test_PAM, test_VFSCd
    

def correlation_coefficient_loss_metric(y_true, y_pred):
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
def fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout):
    
    ret_seq = False
    if hidden_layers > 1:
        ret_seq = True

    model = Sequential()
    model.add(LSTM(neurons, batch_input_shape=(batch_size, trainX.shape[1], trainX.shape[2]), return_sequences=ret_seq, stateful=True, recurrent_activation=activation, recurrent_dropout=dropout))
    for i in range (hidden_layers-1):
        if i == (hidden_layers-2):
            ret_seq = False
        model.add(LSTM(neurons, return_sequences=ret_seq, stateful = True, recurrent_activation=activation, recurrent_dropout=dropout))
    model.add(Dense(trainX.shape[1], activation=activation))

    model.compile(loss='mean_squared_error', optimizer=optimization)
    for i in range(epochs):
        model.fit(trainX, trainY, epochs=1, batch_size=batch_size, verbose=0, shuffle=False)
        model.reset_states()
    return model


#Evaluate the Model
def evaluate(model, X, Y, batch_size):

    output = model.predict(X, batch_size=batch_size)
    
    # invert data transforms on forecast
    # report performance
    rmse = stats.pearsonr(Y[:,0], output[:,0])
    
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
def experiment(trainX, trainY, testX, testY, repeats, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout):
    # run experiment

    lista_z = list()
    repetir_error = 0
    i = 1
    del_level_hyp = False
    while i <= repeats:
        # fit the model
        use_cpu_gpu()
        lstm_model = fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout)
        # report performance
        r = evaluate(lstm_model, testX, testY, batch_size)[0]
        
        if math.isnan(r) and repetir_error < 3:
            repetir_error = repetir_error + 1
            if repetir_error == 3:
                del_level_hyp = True
                break
        else:
            if repetir_error != 3:
                lista_z.append(fisher_transform(r))
            repetir_error = 0
            i = i + 1

        
        K.clear_session()
        del lstm_model
        gc.collect()

    if del_level_hyp:
        mean_z = float('nan')
        mean_corr = float('nan')
    else:
        mean_z = reduce(lambda x, y: x + y, lista_z) / len(lista_z)
        mean_corr = inverse_fisher_transform(mean_z)

    print('batch_size=%d, neurons=%d, hidden_layers=%d, dropout=%.1f, epochs=%d, optimization=%s, activation=%s:::::::::: RESULT CORR=%.3f, RESULT Z=%.3f' % (batch_size, neurons, hidden_layers, dropout, epochs, optimization, activation, mean_corr, mean_z))
    
    return mean_corr, mean_z

#Transfomracion de Fisher
def fisher_transform(r):
    z = 0.5*log((1+r)/(1-r))
    return z

def inverse_fisher_transform(z):
    r = (exp(2*z)-1)/(exp(2*z)+1)
    return r

def use_cpu_gpu():
    config = tf.ConfigProto(intra_op_parallelism_threads=10,
                            inter_op_parallelism_threads=10, 
                            allow_soft_placement=True,
                            device_count = {'CPU' : CPU_USAGE,
                                            'GPU' : GPU_USAGE}
                           )
    #config.gpu_options.allow_growth = True
    session = tf.Session(config=config)
    K.set_session(session)

def run_experiment(experimento, sujeto, postura, balance, trainX, trainY, testX, testY, hyperparameter, batch_size=[2], epochs=[10], optimization=["RMSprop"], activation=["tanh"], hidden_layers=[2], neurons=[10], dropout=[0.9]):
    #take time
    t0 = time.time()
    print("################################################################################### " + hyperparameter)
    
    columnas = ['batch_size','neurons','hidden_layers','dropout','epochs','optimization','activation','CORRELATION','FISHER']
    filas = len(batch_size) * len(epochs) * len(optimization) * len(activation) * len(hidden_layers) * len(neurons) * len(dropout)
    results = numpy.chararray((filas,9), itemsize=40)
    row = 0
    repeats = 10
    best_result = -1
    best_row = 0
    
    for b in batch_size:
        for e in epochs:
            for o in optimization:
                for a in activation:
                    for h in hidden_layers:
                        for n in neurons:
                            for d in dropout:

                                mean_corr, mean_z = experiment(trainX, trainY, testX, testY, repeats, b, e, o, a, h, n, d)
                                results[row][0] = b
                                results[row][1] = n
                                results[row][2] = h
                                results[row][3] = d
                                results[row][4] = e
                                results[row][5] = o
                                results[row][6] = a
                                results[row][7] = mean_corr
                                results[row][8] = mean_z
                            
                                if best_result <= mean_corr:
                                    best_result = mean_corr
                                    best_row = row

                                row = row + 1
    
    df = pd.DataFrame(results, columns=columnas)
    df = df.sort_values(by='CORRELATION', ascending=False)
    print(df)
    nombre_archivo = sujeto+'_'+postura+'/Sujeto_Posicion_'+hyperparameter+'_'+str(balance)+'.xlsx'
    writer = pd.ExcelWriter('C:/Users/Luis/Documents/DataScience/Tesis/Resultados/Fundamentacion Ajuste Modelo/Resultados_'+experimento+'/'+nombre_archivo)
    df.to_excel(writer,'Resultados')
    writer.save()
    print('################################################################################### Archivo |||'+nombre_archivo+'||| Creado')
    print ('###################################################################################',time.time() - t0, "segundos tardo")

    if hyperparameter == "epochs":
        return results[best_row][4]
    if hyperparameter == "dropout":
        return results[best_row][3]
    if hyperparameter == "activation":
        return results[best_row][6].decode("utf-8") 
    if hyperparameter == "optimization":
        return results[best_row][5].decode("utf-8")
    if hyperparameter == "neurons":
        return results[best_row][1] 
    if hyperparameter == "batch_size":
        return results[best_row][0]
    if hyperparameter == "hidden_layers":
        return results[best_row][2]
    if hyperparameter == "deep_network":
        return [int(results[best_row][2])],[int(results[best_row][1])],[str(results[best_row][6].decode("utf-8"))]
        


def run_rango (sujeto, postura, balance):
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: ' + str(balance))
    # load dataset
    # split into input (X) and output (Y) variables
    
    train_PAM, train_VFSCd, test_PAM, test_VFSCd = create_dataset(sujeto, postura)
    if balance == 2:
        train_PAM_temp = train_PAM
        train_PAM = test_PAM
        test_PAM = train_PAM_temp
        del train_PAM_temp
        train_VFSCd_temp = train_VFSCd
        train_VFSCd = test_VFSCd
        test_VFSCd = train_VFSCd_temp
        del train_VFSCd_temp
    
    ################################################################################### batch_size
    batch_size = []
    for i in range(1,train_PAM.shape[0]+1):
        if (train_PAM.shape[0]%i)==0 and i>1:
            batch_size.append(i)
    batch_size_result = [int(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="batch_size", batch_size=batch_size))]
    batch_size_result = [198]
    ################################################################################### neurons
    #neurons = [10,20,30,40,50,60,70,80,90,100]
    #neurons_result = [int(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="neurons", neurons=neurons, batch_size=batch_size_result))]
    
    ################################################################################### dropout
    dropout = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
    dropout_result = [float(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="dropout", dropout=dropout, batch_size=batch_size_result))]

    ################################################################################### deep network (hidden_layers, neurons, activation)
    hidden_layers = [2, 3, 4, 5]
    neurons = [10,20,30,40,50,60,70,80,90,100]
    activation = ['softplus', 'softsign', 'tanh', 'sigmoid', 'hard_sigmoid', 'linear']
    hidden_layers_result, neurons_result, activation_result = run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="deep_network", hidden_layers=hidden_layers, neurons=neurons, activation=activation, dropout=dropout_result, batch_size=batch_size_result)

    ################################################################################### activation
    #activation = ['softplus', 'softsign', 'tanh', 'sigmoid', 'hard_sigmoid', 'linear']
    #activation_result = [str(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="activation", activation=activation, dropout=dropout_result, neurons=neurons_result, batch_size=batch_size_result))]
    #activation_result = ['tanh']#Es el unico que puede modelar respuesta a escalones (de 2 capas)

    ################################################################################### optimization
    optimization = ['SGD', 'RMSprop', 'Adagrad', 'Adadelta', 'Adam', 'Adamax', 'Nadam']
    optimization_result = [str(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="optimization", optimization=optimization, hidden_layers=hidden_layers_result, activation=activation_result, dropout=dropout_result, neurons=neurons_result, batch_size=batch_size_result))]

    ################################################################################### epochs
    epochs = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
    epochs_result = [int(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="epochs", epochs=epochs, optimization=optimization_result, hidden_layers=hidden_layers_result, activation=activation_result, dropout=dropout_result, neurons=neurons_result, batch_size=batch_size_result))]

def run_hiperparametro (sujeto, postura, balance):
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: ' + str(balance))

    # load dataset
    # split into input (X) and output (Y) variables
    train_PAM, train_VFSCd, test_PAM, test_VFSCd = create_dataset(sujeto, postura)
    if balance == 2:
        train_PAM_temp = train_PAM
        train_PAM = test_PAM
        test_PAM = train_PAM_temp
        del train_PAM_temp
        train_VFSCd_temp = train_VFSCd
        train_VFSCd = test_VFSCd
        test_VFSCd = train_VFSCd_temp
        del train_VFSCd_temp
    
    ################################################################################### Base
    run_experiment('Hiperparametro', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="Base")
    
    ################################################################################### epochs
    epochs = [10, 20, 30, 40, 50]
    run_experiment('Hiperparametro', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="epochs", epochs=epochs)

    ################################################################################### activation
    activation = ['softplus', 'softsign', 'tanh', 'sigmoid', 'hard_sigmoid', 'linear']
    run_experiment('Hiperparametro', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="activation", activation=activation)
    
    ################################################################################### optimization
    optimization = ['SGD', 'RMSprop', 'Adagrad', 'Adadelta', 'Adam', 'Adamax', 'Nadam']
    run_experiment('Hiperparametro', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="optimization", optimization=optimization)
    
    ################################################################################### neurons
    neurons = [10,20,30,40,50,60,70,80,90,100]
    run_experiment('Hiperparametro', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="neurons", neurons=neurons)

    ################################################################################### batch_size
    batch_size = []
    for i in range(1,train_PAM.shape[0]+1):
        if (train_PAM.shape[0]%i)==0:
            batch_size.append(i)

    run_experiment('Hiperparametro', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="batch_size", batch_size=batch_size)

    ################################################################################### hidden_layers
    hidden_layers = [2, 3, 4, 5]
    run_experiment('Hiperparametro', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="hidden_layers", hidden_layers=hidden_layers)

#Repitable Experiment
seed(1)
set_random_seed(2)

run_rango(sujeto='AC', postura='ACOSTADO', balance=1) 
#run_rango(sujeto='AC', postura='ACOSTADO', balance=2) 

#run_rango(sujeto='DM', postura='PIE', balance=1) 
#run_rango(sujeto='DM', postura='PIE', balance=2) 

#run_rango(sujeto='PC', postura='SENTADO', balance=1) 
#run_rango(sujeto='PC', postura='SENTADO', balance=2) 

#run_rango(sujeto='AV', postura='ACOSTADO', balance=1) 
#run_rango(sujeto='AV', postura='ACOSTADO', balance=2) 

#run_rango(sujeto='CC', postura='PIE', balance=1) 
#run_rango(sujeto='CC', postura='PIE', balance=2) 

#run_rango(sujeto='CS', postura='SENTADO', balance=1) 
#run_rango(sujeto='CS', postura='SENTADO', balance=2) 