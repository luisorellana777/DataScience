import os
#FORZAR USO DE CPU
os.environ['CUDA_VISIBLE_DEVICES'] = '0'
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 

import time
from math import sqrt, log, exp
import math
import pandas as pd
import numpy
import matplotlib.pyplot as plt
from keras import Sequential
from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, Dense
from sklearn.preprocessing import MinMaxScaler
import gc
from scipy import stats
import plotly.plotly as py
import plotly.graph_objs as go
import plotly
from numpy.random import seed
from tensorflow import set_random_seed
import tensorflow as tf
from functools import reduce



CPU_USAGE = 7
GPU_USAGE = 1
PATH_SUJETOS = ("C:/Users/Luis/Documents/DataScience/Tesis/Datos/SUJETOS/%s/%s-%s-VE.csv")
PATH_ESCALON = ("C:/Users/Luis/Documents/DataScience/Tesis/Datos/ESCALON_PRESION/ESCALON.csv")
PATH_RESULTADO = ("C:/Users/Luis/Documents/DataScience/Tesis/Resultados/Escalon/%s")
PATH_RESULTADO_ESCALON = (PATH_RESULTADO%("%s/%s_%s"))
OPTIMUM_NEURONS = 10

def create_dataset(nombre_sujeto, nombre_postura):

    X = pd.read_csv(PATH_SUJETOS%(nombre_sujeto, nombre_sujeto, nombre_postura), sep="	")
    data_escalon = pd.read_csv(PATH_ESCALON)
    
    # normalize the dataset
    scaler_VFSCd = MinMaxScaler(feature_range=(-1, 1))
    scaler_VFSCi = MinMaxScaler(feature_range=(-1, 1))
    scaler_PAMn = MinMaxScaler(feature_range=(-1, 1))
    scaler_escalon = MinMaxScaler(feature_range=(-1, 1))
    
    VFSCd = scaler_VFSCd.fit_transform(X.VFSCd.values.reshape((len(X.VFSCd.values), 1)))
    VFSCi = scaler_VFSCi.fit_transform(X.VFSCi.values.reshape((len(X.VFSCi.values), 1)))
    PAMn = scaler_PAMn.fit_transform(X.PAMn.values.reshape((len(X.PAMn.values), 1)))
    Escalon = scaler_escalon.fit_transform(data_escalon.ESCALON.values.reshape((len(data_escalon.ESCALON.values), 1)))
    
    #Dar formato float a las señales
    PAMn, VFSCd, VFSCi, Escalon = PAMn.astype('float32'), VFSCd.astype('float32'), VFSCi.astype('float32'), Escalon.astype('float32')
    PAMn, VFSCd, VFSCi, Escalon = numpy.array(PAMn), numpy.array(VFSCd), numpy.array(VFSCi), numpy.array(Escalon)
    
    # Validacion Valanceada
    train_size = int(len(PAMn) * 0.5)
    train_PAM, train_VFSCd, train_VFSCi = PAMn[0:train_size], VFSCd[0:train_size], VFSCi[0:train_size]
    test_PAM, test_VFSCd, test_VFSCi = PAMn[train_size:len(PAMn)], VFSCd[train_size:len(VFSCd)], VFSCi[train_size:len(VFSCi)]
    
    # Reshape segun el formato que acepta Keras
    # reshape input to be [samples, time steps, features]
    train_PAM = numpy.reshape(train_PAM, (train_PAM.shape[0], 1, train_PAM.shape[1]))
    test_PAM = numpy.reshape(test_PAM, (test_PAM.shape[0], 1, test_PAM.shape[1]))
    Escalon = numpy.reshape(Escalon, (Escalon.shape[0], 1, Escalon.shape[1]))
    
    return train_PAM, train_VFSCd, train_VFSCi, test_PAM, test_VFSCd, test_VFSCi, Escalon, scaler_VFSCd, scaler_VFSCi, scaler_escalon


# fit an LSTM network to training data
def fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout):
    use_cpu_gpu()
    
    ret_seq = False
    model = Sequential()
    model.add(LSTM(64, return_sequences=True, input_shape=(198, 1)))
    model.add(LSTM(128, return_sequences=True))
    model.add(LSTM(256, return_sequences=True))
    model.add(LSTM(128, return_sequences=True))
    model.add(LSTM(64, return_sequences=True))
    model.add(LSTM(2, return_sequences=True)) 

    model.compile(loss='mse', optimizer='adam')
    model.fit(trainX[0:198], trainY[0:198], epochs=1000, batch_size=1, verbose=2, validation_data=(trainX,trainY))  
    return model

def fit_lstm2(trainX, trainY, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout):
    use_cpu_gpu()
    
    ret_seq = False
    if hidden_layers > 1:
        ret_seq = True

    model = Sequential()
    model.add(LSTM(neurons, batch_input_shape=(batch_size, trainX.shape[1], trainX.shape[2]), return_sequences=ret_seq, stateful=True, recurrent_activation=activation, recurrent_dropout=dropout))
    for i in range (hidden_layers-1):
        if i == (hidden_layers-2):
            ret_seq = False
            #neurons = OPTIMUM_NEURONS
        model.add(LSTM(neurons, return_sequences=ret_seq, stateful = True, recurrent_activation=activation, recurrent_dropout=dropout))
    model.add(Dense(trainX.shape[1], activation=activation))

    model.compile(loss='mean_squared_error', optimizer=optimization)
    for i in range(epochs):
        model.fit(trainX, trainY, epochs=1, batch_size=batch_size, verbose=0, shuffle=False)
        model.reset_states()
    return model

from keras import backend as K
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
    return ((1 - K.square(r))*-1)+1

#Evaluate the Model
def evaluate(model, X, Y, batch_size):
    output = model.predict(X)
    
    # invert data transforms on forecast
    # report performance
    rmse = stats.pearsonr(Y[:,0], output[:,0])
    
    return rmse[0]

def evaluate2(model, X, Y, batch_size):
    output = model.predict(X, batch_size=batch_size)
    
    # invert data transforms on forecast
    # report performance
    rmse = stats.pearsonr(Y[:,0], output[:,0])
    
    return rmse[0]

    #Evaluate the Model
def evaluate_stair(model, Escalon, batch_size):
    output = model.predict(Escalon, batch_size=batch_size)
    
    return output

#Transfomracion de Fisher
def fisher_transform(r):
    z = 0.5*log((1+r)/(1-r))
    return z

def inverse_fisher_transform(z):
    r = (exp(2*z)-1)/(exp(2*z)+1)
    return r

# run a repeated experiment
def experiment(trainX, trainY, testX, testY, repeats, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout):
    # run experiment
    lista_z = list()
    repetir_error = 0
    i = 1
    del_level_hyp = False
    while i <= repeats:
        # fit the model
        lstm_model = fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout)
        # report performance
        r = evaluate(lstm_model, testX, testY, batch_size)

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

    print('epochs=%d, dropout=%.1f, activation=%s, optimization=%s neurons=%d, batch_size=%d, hidden_layers=%d:::::::::: RESULT=%.3f' % (epochs, dropout, activation, optimization, neurons, batch_size, hidden_layers, mean_corr))
    return mean_corr

def run_experiment(trainX, trainY, testX, testY, batch_size=[1], epochs=[65], optimization=["RMSprop"], activation=["tanh"], hidden_layers=[3], neurons=[10], dropout=[0.9]):
    
    columnas = ['epochs','dropout','activation','optimization','neurons','batch_size','hidden_layers','RESULT']
    filas = len(batch_size) * len(epochs) * len(optimization) * len(activation) * len(hidden_layers) * len(neurons) * len(dropout)
    results = numpy.chararray((filas,8), itemsize=20, unicode=True)
    row = 0
    repeats = 5
    
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

                                row = row + 1
    
    df = pd.DataFrame(results, columns=columnas)
    df = df.sort_values(by='RESULT', ascending=False)

    return df

def best_model(df_1, df_2, ruta_archivo):

    balance_extencion = "_1.csv"
    best_balance = 1
    max_df_1 = float(df_1.iat[0,7])
    max_df_2 = float(df_2.iat[0,7])

    df = df_1
    if(max_df_1<max_df_2):
        df = df_2
        best_balance = 2
        balance_extencion = "_2.csv"


    print('++++++++++++++++++++++++++++++++++++++ Mejor Balance: ')
    print(df)
    df.to_csv(ruta_archivo+balance_extencion, index=False)
    #df.to_excel(writer,'Resultados')
    #writer.save()
    print('################################################################################### Archivo |||'+ruta_archivo+'||| Creado')

    return df, best_balance


def plotting(escalon, output, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout, result_r):

    re_escalon = numpy.reshape(escalon, (escalon.shape[0], 1))
    re_output = numpy.reshape(output, (output.shape[0], 1))

    plotly.tools.set_credentials_file(username='luis.orellana777', api_key='pCEXLd20Nqi47WlLGYGk')

    trace_high = go.Scatter(
                    x=list(range(1, len(re_escalon))),
                    y=re_escalon,
                    name = "Escalon",
                    line = dict(color = '#17BECF'),
                    opacity = 0.8)

    trace_low = go.Scatter(
                    x=list(range(1, len(re_output))),
                    y=re_output,
                    name = "VFSC Respuesta Escalon",
                    line = dict(color = '#7F7F7F'),
                    opacity = 0.8)

    data = [trace_high,trace_low]

    layout = dict(

        title = ("LSTM - Respuesta a Escalon"),
        xaxis = dict(
            range = [1, len(re_output)], 
            title="Batch=%d ; Epoch=%d ; Optimization=%s ; Activation=%s ; Layers=%d ; Units=%d ; Dropout=%.2f ; Correlation=%.3f"%(batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout, result_r))
    )

    fig = dict(data=data, layout=layout)
    py.plot(fig, filename = "LSTM VFSC")


def plotting_mathplot(escalon, output, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout, result_r):
    
    re_escalon = numpy.reshape(escalon, (escalon.shape[0]))
    re_output = numpy.reshape(output, (output.shape[0]))
    
    fig, ax = plt.subplots()
    
    # Using set_dashes() to modify dashing of an existing line
    line1, = ax.plot(list(range(1, len(re_escalon)+1)), re_escalon, label='Escalon')
    
    # Using plot(..., dashes=...) to set the dashing when creating a line
    line2, = ax.plot(list(range(1, len(re_output)+1)), re_output, label='VFSC Respuesta Escalon')
    
    title="Batch=%d ; Epoch=%d ; Optimization=%s ; Activation=%s ; Layers=%d ; Units=%d ; Dropout=%.2f ; Correlation=%.3f"%(batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout, result_r)
    ax.set_xlabel(title)
    ax.set_title("LSTM - Respuesta a Escalon")
    
    ax.legend()
    plt.show()
    
def use_cpu_gpu():
    config = tf.compat.v1.ConfigProto(intra_op_parallelism_threads=10,
                            inter_op_parallelism_threads=10, 
                            device_count = {'CPU' : CPU_USAGE,
                                            'GPU' : GPU_USAGE}
                           )
    config.gpu_options.allow_growth = True
    config.gpu_options.per_process_gpu_memory_fraction = 0.9
    #config.gpu_options.allow_growth = True
    session = tf.compat.v1.Session(config=config)
    K.set_session(session)

def apply_stair(df, trainX, trainY, testX, testY, escalon, scaler_VFSC, scaler_escalon, sujeto, postura, hemisferio):

    for row in range(df.shape[0]):#Cantidad de registros en el dataframe resultados
        batch_size = int(df.iat[row,5])
        epochs = int(df.iat[row,0])
        optimization = df.iat[row,3]
        activation = df.iat[row,2]
        hidden_layers = int(df.iat[row,6])
        neurons = int(df.iat[row,4])
        dropout = float(df.iat[row,1])
        result_r = float(df.iat[row,7])
        
        if hemisferio == "Izquierdo":
            pregunta_seguir = "¿Finalizar (3)?\n"
        else:
            pregunta_seguir = "¿Seguir con el Otro Hemisferio (3)?\n"

        for dropouts in [1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0]:

            for execution in range(1,100):

                opcion = input("Ejecucion %d. \nHemisferio %s. \n¿Deseas continuar? (0) \n¿Deseas Cambiar DropOut? (1) \n¿Deseas Probar Con Otros Hiperparametros (2)?\n%s"%(execution,hemisferio,pregunta_seguir))
            
                if opcion == "1":
                    break
                elif opcion == "2":
                    break
                elif opcion == "3":
                    return

                use_cpu_gpu()

                lstm_model = fit_lstm(trainX, trainY, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout)

                output = evaluate_stair(lstm_model, escalon, batch_size)

                r = evaluate(lstm_model, testX, testY, batch_size)

                plotting_mathplot(escalon, output, batch_size, epochs, optimization, activation, hidden_layers, neurons, dropout, r)

                quedar = input("¿Te quedas con esta? \nSi(1) \nNo(0) \n")
            
                if quedar == "1":
                    save_hidden_states(lstm_model, neurons, layer=0, sujeto=sujeto, postura=postura, hemisferio=hemisferio)
                    save_hidden_states(lstm_model, neurons, layer=1, sujeto=sujeto, postura=postura, hemisferio=hemisferio)
                    save_hidden_states(lstm_model, neurons, layer=2, sujeto=sujeto, postura=postura, hemisferio=hemisferio)
                    save_signal(scaler_VFSC.inverse_transform(output), sujeto=sujeto, postura=postura, hemisferio=hemisferio)
                    save_model(lstm_model, sujeto=sujeto, postura=postura, hemisferio=hemisferio)
                    #save_hidden_output(lstm_model, escalon, sujeto, postura, hemisferio)
                K.clear_session()
                del lstm_model
                gc.collect()

                if opcion == "0":
                    continue

                seguir = input("dropout = %.1f \nSeguir si(0) no(2):"%dropout)
                if seguir == "2":
                    dropout = 1.0
                    break

            if opcion == "1":
                continue
            elif opcion == "2" or seguir == "2":

                break

def save_model(model, sujeto, postura, hemisferio):
    directory = PATH_RESULTADO%(sujeto)+'/models'
    if not os.path.exists(directory):
        os.makedirs(directory)

    model_json = model.to_json()
    with open(directory+"/model_"+str(postura)+'_'+str(hemisferio)+".json", "w") as json_file:
        json_file.write(model_json)
    # serialize weights to HDF5
    model.save_weights(directory+"/model_"+str(postura)+'_'+str(hemisferio)+".h5")

def save_signal(output, sujeto, postura, hemisferio):

    df = pd.DataFrame(output.tolist(), columns=['Output'])
    df.to_csv(PATH_RESULTADO_ESCALON%(sujeto, postura, hemisferio)+'_output.csv',index=False)

def save_hidden_output(model, escalon, sujeto, postura, hemisferio):
    intermediate_layer_model = Model(inputs=model.layers[0].input,
                                 outputs=model.layers[0].output)
    intermediate_output = intermediate_layer_model.predict(escalon)
    df = pd.DataFrame({'Output': intermediate_output})
    writer = pd.ExcelWriter(PATH_RESULTADO_ESCALON%(sujeto, postura, hemisferio)+'_output_hidden_layer.xlsx')
    df.to_excel(writer, sheet_name='Kernel')
    writer.save()


def save_hidden_states(model, units, layer, sujeto, postura, hemisferio):
    W = model.layers[layer].get_weights()[0]
    U = model.layers[layer].get_weights()[1]
    b = model.layers[layer].get_weights()[2]
    
    W_i = W[:, :units]
    W_f = W[:, units: units * 2]
    W_c = W[:, units * 2: units * 3]
    W_o = W[:, units * 3:]

    U_i = U[:, :units]
    U_f = U[:, units: units * 2]
    U_c = U[:, units * 2: units * 3]
    U_o = U[:, units * 3:]

    b_i = b[:units]
    b_f = b[units: units * 2]
    b_c = b[units * 2: units * 3]
    b_o = b[units * 3:]

    column_W = ['W_i', 'W_f', 'W_c', 'W_o']
    column_U = ['U_i', 'U_f', 'U_c', 'U_o']
    column_b = ['b_i', 'b_f', 'b_c', 'b_o']
    # Create some Pandas dataframes from some data.
    if layer == 0:

        data_W_i = list(zip(W_i.reshape(units,1).tolist(), W_f.reshape(units,1).tolist(), W_c.reshape(units,1).tolist(), W_o.reshape(units,1).tolist()))
        data_U_i = list(zip(U_i.reshape(units*units,1).tolist(), U_f.reshape(units*units,1).tolist(), U_c.reshape(units*units,1).tolist(), U_o.reshape(units*units,1).tolist()))
        data_b_i = list(zip(b_i.reshape(units,1).tolist(), b_f.reshape(units,1).tolist(), b_c.reshape(units,1).tolist(), b_o.reshape(units,1).tolist()))
    else:

        data_W_i = list(zip(W_i.reshape(units*units,1).tolist(), W_f.reshape(units*units,1).tolist(), W_c.reshape(units*units,1).tolist(), W_o.reshape(units*units,1).tolist()))
        data_U_i = list(zip(U_i.reshape(units*units,1).tolist(), U_f.reshape(units*units,1).tolist(), U_c.reshape(units*units,1).tolist(),  U_o.reshape(units*units,1).tolist()))
        data_b_i = list(zip(b_i.reshape(units,1).tolist(), b_f.reshape(units,1).tolist(), b_c.reshape(units,1).tolist(), b_o.reshape(units,1).tolist()))

    df1 = pd.DataFrame(data_W_i, columns=column_W)
    df2 = pd.DataFrame(data_U_i, columns=column_U)
    df3 = pd.DataFrame(data_b_i, columns=column_b)
    # Create a Pandas Excel writer using XlsxWriter as the engine.
    writer = pd.ExcelWriter(PATH_RESULTADO_ESCALON%(sujeto, postura, hemisferio)+'_layer_'+str(layer)+'.xlsx',index=False)

    # Write each dataframe to a different worksheet.
    df1.to_excel(writer, sheet_name='Kernel')
    df2.to_excel(writer, sheet_name='Recurrent Kernel')
    df3.to_excel(writer, sheet_name='Bias')

    # Close the Pandas Excel writer and output the Excel file.
    writer.save()


def run (sujeto, postura, proceso_escalon = False, hemisferio_derecho=True, hemisferio_izquierdo=True):
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    
    

    path = PATH_RESULTADO%(sujeto)
    if not os.path.exists(path):
        os.makedirs(path)

    train_PAM, train_VFSCd, train_VFSCi, test_PAM, test_VFSCd, test_VFSCi, Escalon, scaler_VFSCd, scaler_VFSCi, scaler_escalon = create_dataset(sujeto, postura)

    activation = ['softplus','sigmoid', 'hard_sigmoid']
    neurons = [5,10,15,20,30,40]
    hidden_layers = [3, 4, 5]
    dropout = [0.6, 0.7, 0.8, 0.9]
    optimization = ['RMSprop']
    epochs = [10]
    batch_size = [198]
    '''
    batch_size = []
    for i in range(1,train_PAM.shape[0]+1):
        if (train_PAM.shape[0]%i)==0:
            batch_size.append(199)
    '''
    if hemisferio_derecho == True:

        hemisferio = "Derecho"
        PATH_RESULTADO_CONTEXTO = PATH_RESULTADO_ESCALON%(sujeto, postura, hemisferio)

        exists_1 = os.path.isfile(PATH_RESULTADO_CONTEXTO+"_1.csv")
        exists_2 = os.path.isfile(PATH_RESULTADO_CONTEXTO+"_2.csv")

        best_balance = 0
        
        if exists_1 == False and exists_2 == False:
            ################################################################################### Balance 1
            print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: 1, Emisferio: Derecho')

            df_1 = run_experiment(train_PAM, train_VFSCd, test_PAM, test_VFSCd, batch_size=batch_size, epochs=epochs, optimization=optimization, activation=activation, hidden_layers=hidden_layers, neurons=neurons, dropout=dropout)

            ################################################################################### Balance 2
            print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: 2, Emisferio: Derecho')

            df_2 = run_experiment(test_PAM, test_VFSCd, train_PAM, train_VFSCd, batch_size=batch_size, epochs=epochs, optimization=optimization, activation=activation, hidden_layers=hidden_layers, neurons=neurons, dropout=dropout)

            df, best_balance = best_model(df_1, df_2, PATH_RESULTADO_CONTEXTO)

        if (best_balance == 1 or exists_1 == True) and proceso_escalon == True:

            df = pd.read_csv(PATH_RESULTADO_CONTEXTO+"_1.csv", dtype='S')

            apply_stair(df, train_PAM, train_VFSCd, test_PAM, test_VFSCd, Escalon, scaler_VFSCd, scaler_escalon, sujeto, postura, hemisferio)

        elif (best_balance == 2 or exists_2 == True) and proceso_escalon == True:

            df = pd.read_csv(PATH_RESULTADO_CONTEXTO+"_2.csv", dtype='S')

            apply_stair(df, test_PAM, test_VFSCd, train_PAM, train_VFSCd, Escalon, scaler_VFSCd, scaler_escalon, sujeto, postura, hemisferio)
    
    ########################################################################################## HEMISFERIO IZQUIERDO
    ################################################################################### Balance 1
    
    if hemisferio_izquierdo == True:

        hemisferio = "Izquierdo"
        PATH_RESULTADO_CONTEXTO = PATH_RESULTADO_ESCALON%(sujeto, postura, hemisferio)

        exists_1 = os.path.isfile(PATH_RESULTADO_CONTEXTO+"_1.csv")
        exists_2 = os.path.isfile(PATH_RESULTADO_CONTEXTO+"_2.csv")
        
        best_balance = 0
        
        print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: 1, Emisferio: Izquierdo')

        if exists_1 == False and exists_2 == False:
            df_1 = run_experiment(train_PAM, train_VFSCi, test_PAM, test_VFSCi, batch_size=batch_size, epochs=epochs, optimization=optimization, activation=activation, hidden_layers=hidden_layers, neurons=neurons, dropout=dropout)

            ################################################################################### Balance 2
            print('++++++++++++++++++++++++++++++++++++++ Sujeto: ' + sujeto + ' Posicion: ' + postura + ' Balance: 2, Emisferio: Izquierdo')

            df_2 = run_experiment(test_PAM, test_VFSCi, train_PAM, train_VFSCi, batch_size=batch_size, epochs=epochs, optimization=optimization, activation=activation, hidden_layers=hidden_layers, neurons=neurons, dropout=dropout)

            df, best_balance = best_model(df_1, df_2, PATH_RESULTADO_CONTEXTO)

        ########################################################################################## ENTRENAR MODELO A PARTIR DE "df" Y GRAFICAR RESPUESTA  ESCALON
        
        if (best_balance == 1 or exists_1 == True) and proceso_escalon == True:

            df = pd.read_csv(PATH_RESULTADO_CONTEXTO+"_1.csv")

            apply_stair(df, train_PAM, train_VFSCi, test_PAM, test_VFSCi, Escalon, scaler_VFSCi, scaler_escalon, sujeto, postura, hemisferio)

        elif (best_balance == 2 or exists_2 == True) and proceso_escalon == True:

            df = pd.read_csv(PATH_RESULTADO_CONTEXTO+"_2.csv")

            apply_stair(df, test_PAM, test_VFSCi, train_PAM, train_VFSCi, Escalon, scaler_VFSCi, scaler_escalon, sujeto, postura, hemisferio)


#Repitable Experiment
seed(1)
set_random_seed(2)


#run(sujeto='AP', postura='PIE', proceso_escalon=False, hemisferio_derecho=False)


#run(sujeto='AV', postura='ACOSTADO', proceso_escalon = False, hemisferio_izquierdo=False)

#run(sujeto='AV', postura='PIE', proceso_escalon = False, hemisferio_derecho=False)


#run(sujeto='CC', postura='ACOSTADO', proceso_escalon = False, hemisferio_derecho=False)

#run(sujeto='CC', postura='PIE', proceso_escalon = True)#falta emisferio ezquierdo


####################################################

#take time
t0 = time.time()

run(sujeto='AC', postura='ACOSTADO', proceso_escalon = True)

print ('###################################################################################',time.time() - t0, "segundos tardo")