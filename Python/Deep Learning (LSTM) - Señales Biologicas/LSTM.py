#Ejemplo de entrenmiento:
#https://machinelearningmastery.com/time-series-prediction-lstm-recurrent-neural-networks-python-keras/

#Elegir cantidad de hidden layesr:
#https://wiki.inf.ed.ac.uk/twiki/pub/CSTR/ListenTerm1201415/sak2.pdf

#Mejores parametros:
#https://arxiv.org/pdf/1709.05206.pdf
##Tuning the Number of Epochs
##Tuning the Batch Size
##Tuning the Number of Neurons
##Tuning the Number of Hidden Layers

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

class Entrenaiento_1:
    # evaluate the model on a dataset, returns RMSE in transformed units
    def evaluate(model, X, Y, batch_size):
        
    	output = model.predict(X, batch_size=batch_size)
    	# invert data transforms on forecast
    	yhat = list()
    	for i in range(len(output)):
    		# store forecast
    		yhat.append(output[i,0])
    	# report performance
    	rmse = math.sqrt(mean_squared_error(Y[:,0], yhat))
    	return rmse
    
    # fit an LSTM network to training data
    def fit_lstm(trainX, trainY, testX, testY, n_batch, n_epochs, n_neurons):
    	# prepare model
    	model = Sequential()
    	model.add(LSTM(n_neurons, batch_input_shape=(n_batch, trainX.shape[1], trainX.shape[2]), stateful=True, return_sequences=True))
    	model.add(LSTM(n_neurons, batch_input_shape=(n_batch, trainX.shape[1], trainX.shape[2]), stateful=True))
    	model.add(Dense(1))
    	model.compile(loss='mean_squared_error', optimizer='adam')
    	# fit model
    	train_rmse, test_rmse = list(), list()
    	for i in range(n_epochs):
    		model.fit(trainX, trainY, epochs=1, batch_size=n_batch, verbose=0, shuffle=False)
    		model.reset_states()
    		# evaluate model on train data
    		train_rmse.append(Entrenaiento_1.evaluate(model, trainX, trainY, n_batch))
    		model.reset_states()
    		# evaluate model on test data
    		test_rmse.append(Entrenaiento_1.evaluate(model, testX, testY, n_batch))
    		model.reset_states()
    	history = DataFrame()
    	history['train'], history['test'] = train_rmse, test_rmse
    	return history
    
    def run(trainX, trainY, testX, testY):
        repeats = 10
        n_batch = 2 # (n_batch%trainX.shape[0]) == 0
        n_epochs = 50
        n_neurons = 10
        # run diagnostic tests
        for i in range(repeats):
        	history = Entrenaiento_1.fit_lstm(trainX, trainY, testX, testY, n_batch, n_epochs, n_neurons)
        	plt.plot(history['train'], color='blue')
        	plt.plot(history['test'], color='orange')
        	print('%d) TrainRMSE=%f, TestRMSE=%f' % (i, history['train'].iloc[-1], history['test'].iloc[-1]))
        plt.savefig('epochs_diagnostic.png')

class Entrenaiento_2:
    # fit an LSTM network to training data
    def fit_lstm(trainX, trainY, n_batch, n_epochs, n_neurons):
        
        model = Sequential()
        model.add(LSTM(n_neurons, batch_input_shape=(n_batch, trainX.shape[1], trainX.shape[2]), stateful=True, return_sequences=True))
        model.add(LSTM(n_neurons, batch_input_shape=(n_batch, trainX.shape[1], trainX.shape[2]), stateful=True))
        model.add(Dense(1))
        model.compile(loss='mean_squared_error', optimizer='adam')
        for i in range(n_epochs):
            model.fit(trainX, trainY, epochs=1, batch_size=n_batch, verbose=0, shuffle=False)
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
    	rmse = math.sqrt(mean_squared_error(Y[:,0], yhat))
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
    def experiment(trainX, trainY, testX, testY, repeats, epochs, batch_size, n_neurons):
    	# run experiment
    	error_scores = list()
    	for r in range(repeats):
    		# fit the model
    		lstm_model = Entrenaiento_2.fit_lstm(trainX, trainY, batch_size, epochs, n_neurons)

    		lstm_model.predict(trainX, batch_size=batch_size)
    	 
    		# report performance
    		rmse = Entrenaiento_2.evaluate(lstm_model, testX, testY, batch_size)
    		print('%d) Test RMSE: %.3f' % (r+1, rmse))
    		error_scores.append(rmse)
    	return error_scores
    
    def run_epochs(trainX, trainY, testX, testY, nombre_archivo_resultados):
        nombre_experimento = "_epochs_"
        results = DataFrame()
        repeats = 2
        # experiment
        # vary training epochs
        epochs = [2, 3, 4]
        batch_size = 1
        n_neurons = 1

        for e in epochs:
        	results[str(e)] = Entrenaiento_2.experiment(trainX, trainY, testX, testY, repeats, e, batch_size, n_neurons)
        # summarize results
        print(results.describe())
        # save boxplot
        Entrenaiento_2.save_results(results, nombre_archivo_resultados+nombre_experimento)
        
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
train_size = int(len(PAMn) * 0.5)
test_size = len(PAMn) - train_size
train_PAM, train_VFSCd = PAMn[0:train_size], VFSCd[0:train_size]
test_PAM, test_VFSCd = PAMn[train_size:len(PAMn)], VFSCd[train_size:len(VFSCd)]

# Reshape segun el formato que acepta Keras
# reshape input to be [samples, time steps, features]
train_PAM = numpy.reshape(train_PAM, (train_PAM.shape[0], 1, train_PAM.shape[1]))
test_PAM = numpy.reshape(test_PAM, (test_PAM.shape[0], 1, test_PAM.shape[1]))
# create and fit the LSTM network
##############################################################################################################################################################
nombre_archivo_resultados = nombre_sujeto+"_"+nombre_postura+"_"+nombre_emisferio
Entrenaiento_2.run_epochs(train_PAM, train_VFSCd, test_PAM, test_VFSCd, nombre_archivo_resultados+"_MODELO1")
Entrenaiento_2.run_epochs(test_PAM, test_VFSCd, train_PAM, train_VFSCd, nombre_archivo_resultados+"_MODELO2")
##############################################################################################################################################################
'''
model = Sequential()
model.add(LSTM(7, input_shape=(1, 1)))
model.add(Dense(1))#activation='sigmoid'
model.compile(loss='mse', optimizer='adam')
history = model.fit(trainX, trainY, validation_data=(testX, testY), epochs=800, batch_size=50, verbose=0, shuffle=False)
# make predictions
trainPredict = model.predict(trainX)
testPredict = model.predict(testX)
# invert predictions
trainPredict = scaler.inverse_transform(trainPredict)
trainY = scaler.inverse_transform(trainY)
testPredict = scaler.inverse_transform(testPredict)
testY = scaler.inverse_transform(testY)
# calculate root mean squared error
trainScore = math.sqrt(mean_squared_error(trainY[:,0], trainPredict[:,0]))
print('Train Score: %.2f RMSE' % (trainScore))
testScore = math.sqrt(mean_squared_error(testY[:,0], testPredict[:,0]))
print('Test Score: %.2f RMSE' % (testScore))
# shift train predictions for plotting
trainPredictPlot = numpy.empty_like(trainPredict)
trainPredictPlot[:, :] = numpy.nan
trainPredictPlot = trainPredict
# shift test predictions for plotting
testPredictPlot = numpy.empty_like(testPredict)
testPredictPlot[:, :] = numpy.nan
testPredictPlot = testPredict
#plot loss function
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('model loss')
plt.ylabel('loss')
plt.xlabel('epoch')
plt.legend(['train', 'test'], loc='upper left')
plt.show()
'''
# plot baseline and predictions
'''
plt.plot(scaler.inverse_transform(VFSCd))
plt.plot(trainPredictPlot)
plt.plot(testPredictPlot)
plt.show()
'''
'''
import plotly.plotly as py
import plotly.graph_objs as go
import plotly
plotly.tools.set_credentials_file(username='luis.orellana777', api_key='pCEXLd20Nqi47WlLGYGk')


trace_high = go.Scatter(
                x=list(range(1, len(VFSCd))),
                y=scaler.inverse_transform(VFSCd),
                name = "VFSCd",
                line = dict(color = '#17BECF'),
                opacity = 0.8)

trace_low = go.Scatter(
                x=list(range(1, len(VFSCd))),
                y=numpy.concatenate((trainPredictPlot, testPredictPlot)),
                name = "VFSCd Predicted",
                line = dict(color = '#7F7F7F'),
                opacity = 0.8)

data = [trace_high,trace_low]

layout = dict(
    title = "LSTM VFSCd",
    xaxis = dict(
        range = [1, len(VFSCd)])
)

fig = dict(data=data, layout=layout)
py.iplot(fig, filename = "LSTM VFSCd")
'''