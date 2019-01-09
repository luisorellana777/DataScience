from __future__ import print_function
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
from joblib import Parallel, delayed
import tensorflow as tf
import gc
from statistics import mean,stdev
from functools import reduce
from scipy import stats


def create_dataset(nombre_sujeto, nombre_postura):
    PATH = ("C:/Users/Luis.O.A/Documents/USACH/Tesis/Dataset/Sujetos/Muestreo 0.4/%s/%s-%s-VE.csv"%(nombre_sujeto, nombre_sujeto, nombre_postura))
    X = pd.read_csv(PATH, sep="	")
    
    # normalize the dataset
    scaler = MinMaxScaler(feature_range=(-1, 1))
    
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
    model.add(LSTM(neurons, batch_input_shape=(batch_size, trainX.shape[1], trainX.shape[2]), return_sequences=ret_seq))
    model.add(Dropout(dropout))
    for i in range (hidden_layers-1):
        if i == (hidden_layers-2):
            ret_seq = False
        model.add(LSTM(neurons, return_sequences=ret_seq))
    model.add(Dense(trainY.shape[1], activation=activation))

    model.compile(loss='mean_squared_error', optimizer=optimization)
    
    model.fit(trainX, trainY, epochs=epochs, batch_size=batch_size, verbose=0, shuffle=False)
    
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

    # fit the model
    lstm_model = fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout)
    # report performance
    r = evaluate(lstm_model, testX, testY, batch_size)[0]

    K.clear_session()
    del lstm_model
    gc.collect()
    print('epochs=%d, dropout=%.1f, activation=%s, optimization=%s neurons=%d, batch_size=%d, hidden_layers=%d:::::::::: RESULT=%.3f' % (epochs, dropout, activation, optimization, neurons, batch_size, hidden_layers, r))
    return r

#Transfomracion de Fisher
def fisher_transform(r):
    z = 0.5*log((1+r)/(1-r))
    return z

def inverse_fisher_transform(z):
    r = (exp(2*z)-1)/(exp(2*z)+1)
    return r

def run_experiment(experimento, sujeto, postura, balance, trainX, trainY, testX, testY, hyperparameter, batch_size=[1], epochs=[10], optimization=["Adam"], activation=["linear"], hidden_layers=[3], neurons=[10], dropout=[1.0]):
    print("################################################################################### " + hyperparameter)
    
    columnas = ['epochs','dropout','activation','optimization','neurons','batch_size','hidden_layers','RESULT']
    filas = len(batch_size) * len(epochs) * len(optimization) * len(activation) * len(hidden_layers) * len(neurons) * len(dropout)
    results = numpy.chararray((filas,8), itemsize=20)
    row = 0
    repeats = 1
    best_result = -1
    best_row = 0
    
    for b in batch_size:
        for e in epochs:
            for o in optimization:
                for a in activation:
                    for h in hidden_layers:
                        for n in neurons:
                            for d in dropout:
                                result = experiment(trainX, trainY, testX, testY, repeats, b, e, o, a, h, n, d)
                                results[row][0] = e
                                results[row][1] = d
                                results[row][2] = a
                                results[row][3] = o
                                results[row][4] = n
                                results[row][5] = b
                                results[row][6] = h
                                results[row][7] = result
                            
                                if best_result < result:
                                    best_result= result
                                    best_row = row

                                row = row + 1
    
    df = pd.DataFrame(results, columns=columnas)
    df = df.sort_values(by='RESULT', ascending=False)
    print(df)
    nombre_archivo = sujeto+'_'+postura+'/Sujeto_Posicion_'+hyperparameter+'_'+str(balance)+'.xlsx'
    writer = pd.ExcelWriter('C:/Users/Luis.O.A/Documents/USACH/Tesis/Resultados_'+experimento+'/'+nombre_archivo)
    df.to_excel(writer,'Resultados')
    writer.save()
    print('################################################################################### Archivo |||'+nombre_archivo+'||| Creado')
    

    if hyperparameter == "epochs":
        return results[best_row][0]
    if hyperparameter == "dropout":
        return results[best_row][1]
    if hyperparameter == "activation":
        return results[best_row][2].decode("utf-8") 
    if hyperparameter == "optimization":
        return results[best_row][3].decode("utf-8")
    if hyperparameter == "neurons":
        return results[best_row][4] 
    if hyperparameter == "batch_size":
        return results[best_row][5]
    if hyperparameter == "hidden_layers":
        return results[best_row][6]


def run_rango (sujeto, postura, balance):
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: ' + str(balance))
    # fix random seed for reproducibility
    #seed = 7
    #numpy.random.seed(seed)
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
    
    ################################################################################### epochs
    epochs = [10, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500]
    epochs_result = [int(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="epochs", epochs=epochs))]

    ################################################################################### dropout
    dropout = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]    #   1.0 es no usar dropout
    dropout_result = [float(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="dropout", epochs=epochs_result, dropout=dropout))]
    ################################################################################### activation
    activation = ['softplus', 'softsign', 'tanh', 'sigmoid', 'hard_sigmoid', 'linear']
    activation_result = [str(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="activation", epochs=epochs_result, dropout=dropout_result, activation=activation))]
    
    ################################################################################### optimization
    optimization = ['SGD', 'RMSprop', 'Adagrad', 'Adadelta', 'Adam', 'Adamax', 'Nadam']
    optimization_result = [str(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="optimization", epochs=epochs_result, dropout=dropout_result, activation=activation_result, optimization=optimization))]
    
    ################################################################################### neurons
    neurons = [10,20,30,40,50,60,70,80,90,100]
    neurons_result = [int(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="neurons", epochs=epochs_result, dropout=dropout_result, optimization=optimization_result, activation=activation_result, neurons=neurons))]

    ################################################################################### batch_size
    batch_size = []
    for i in range(1,train_PAM.shape[0]+1):
        if (train_PAM.shape[0]%i)==0:
            batch_size.append(i)

    batch_size_result = [int(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="batch_size", epochs=epochs_result, dropout=dropout_result, optimization=optimization_result, activation=activation_result, neurons=neurons_result, batch_size=batch_size))]

    ################################################################################### hidden_layers
    hidden_layers = [2, 3, 4, 5]
    hidden_layers_result = [int(run_experiment('Rango', sujeto, postura, balance, train_PAM, train_VFSCd, test_PAM, test_VFSCd, hyperparameter="hidden_layers", epochs=epochs_result, dropout=dropout_result, optimization=optimization_result, activation=activation_result, neurons=neurons_result, batch_size=batch_size_result, hidden_layers=hidden_layers))]

def run_hiperparametro (sujeto, postura, balance):
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: ' + str(balance))
    # fix random seed for reproducibility
    #seed = 7
    #numpy.random.seed(seed)
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
    epochs = [10, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500]
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


############################################### POSTURA: ACOSTADO
run_hiperparametro(sujeto='AV', postura='ACOSTADO', balance=1)
run_hiperparametro(sujeto='AV', postura='ACOSTADO', balance=2)

run_hiperparametro(sujeto='AC', postura='ACOSTADO', balance=1)
run_hiperparametro(sujeto='AC', postura='ACOSTADO', balance=2)

############################################### POSTURA: PIE
run_hiperparametro(sujeto='CC', postura='PIE', balance=1)
run_hiperparametro(sujeto='CC', postura='PIE', balance=2)

run_hiperparametro(sujeto='DM', postura='PIE', balance=1)
run_hiperparametro(sujeto='DM', postura='PIE', balance=2)

############################################### POSTURA: SENTADO
run_hiperparametro(sujeto='PC', postura='SENTADO', balance=1)
run_hiperparametro(sujeto='PC', postura='SENTADO', balance=2)

run_hiperparametro(sujeto='CS', postura='SENTADO', balance=1)
run_hiperparametro(sujeto='CS', postura='SENTADO', balance=2)