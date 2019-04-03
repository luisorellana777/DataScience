import pandas as pd
from sklearn.preprocessing import MinMaxScaler
import numpy
import matplotlib.pyplot as plt
import numpy as np
import csv
import os

PATH_SUJETOS = ("C:/Users/Luis/Documents/DataScience/Tesis/Datos/SUJETOS/%s/%s-%s-VE.csv")
PATH_ESCALON = ("C:/Users/Luis/Documents/DataScience/Tesis/Datos/ESCALON_PRESION/ESCALON.csv")
PATH_RESULTADO = ("C:/Users/Luis/Documents/DataScience/Tesis/Resultados/Escalon/%s")
PATH_RESULTADO_MFARI = ("C:/Users/Luis/Documents/DataScience/Tesis/Calculo ARI/v15.9.10/Data/%s")
PATH_RESULTADO_FORMATEADO = ("C:/Users/Luis/Documents/DataScience/Tesis/Calculo ARI/v15.9.10/Data/%s/%s-%s.txt")

PATH_RESULTADO_ESCALON = (PATH_RESULTADO%("%s/%s_%s"))

def create_scalers(nombre_sujeto, nombre_postura):

    X = pd.read_csv(PATH_SUJETOS%(nombre_sujeto, nombre_sujeto, nombre_postura), sep="	")
    data_escalon = pd.read_csv(PATH_ESCALON)
    
    # normalize the dataset
    scaler_VFSCd = MinMaxScaler(feature_range=(0, 1))
    scaler_VFSCi = MinMaxScaler(feature_range=(0, 1))
    scaler_escalon = MinMaxScaler(feature_range=(0, 1))
    
    scaler_VFSCd.fit(X.VFSCd.values.reshape((len(X.VFSCd.values), 1)))
    scaler_VFSCi.fit(X.VFSCi.values.reshape((len(X.VFSCi.values), 1)))
    Escalon = scaler_escalon.fit_transform(data_escalon.ESCALON.values.reshape((len(data_escalon.ESCALON.values), 1)))
    
    return scaler_VFSCd, scaler_VFSCi, Escalon


def read_output(nombre_sujeto, nombre_postura, hemisferio, scaler_VFSCd, scaler_VFSCi):

    X = pd.read_csv(PATH_RESULTADO_ESCALON%(nombre_sujeto, nombre_postura, hemisferio)+'_output.csv', sep="	")
    if hemisferio == "Izquierdo":
        out = scaler_VFSCi.transform(X.Output.values.reshape((len(X.Output.values), 1)))
        return out
    elif hemisferio == "Derecho":
        out = scaler_VFSCd.transform(X.Output.values.reshape((len(X.Output.values), 1)))
        return out

def plotting_mathplot(escalon, output):
    
    re_escalon = numpy.reshape(escalon, (escalon.shape[0]))
    re_output = numpy.reshape(output, (output.shape[0]))
    
    fig, ax = plt.subplots()
    
    # Using set_dashes() to modify dashing of an existing line
    line1, = ax.plot(list(range(1, len(re_escalon)+1)), re_escalon, label='Escalon')
    
    # Using plot(..., dashes=...) to set the dashing when creating a line
    line2, = ax.plot(list(range(1, len(re_output)+1)), re_output, label='VFSC Respuesta Escalon')
    
    title=""
    ax.set_xlabel(title)
    ax.set_title("LSTM - Respuesta a Escalon")
    
    ax.legend()
    plt.show()

def save_signal(sujeto, postura, escalon, output_izquierdo, output_derecho):

    time = np.round(np.arange(-78.0, 81.2, 0.4),1)

    df = list(zip(time.tolist(), escalon[:,0].tolist(), output_izquierdo[:,0].tolist(), output_derecho[:,0].tolist()))

    df1 = pd.DataFrame(df, columns=['Time', 'ABP', 'LCBFV', 'RCBFV'])

    if not os.path.exists(PATH_RESULTADO_MFARI%(sujeto)):
        os.makedirs(PATH_RESULTADO_MFARI%(sujeto))

    df1.to_csv(PATH_RESULTADO_FORMATEADO%(sujeto, sujeto, postura),index=False, sep="	", quoting=csv.QUOTE_NONE)
    
def run(sujeto, postura):

    scaler_VFSCd, scaler_VFSCi, escalon = create_scalers(sujeto, postura)
    
    output_izquierdo = read_output(sujeto, postura, "Izquierdo", scaler_VFSCd, scaler_VFSCi)
    output_derecho = read_output(sujeto, postura, "Derecho", scaler_VFSCd, scaler_VFSCi)

    save_signal(sujeto, postura, escalon, output_izquierdo, output_derecho)

    #plotting_mathplot(escalon, output_izquierdo)
    #plotting_mathplot(escalon, output_derecho)

run("AC", "ACOSTADO")
run("AC", "PIE")
run("AC", "SENTADO")

run(sujeto='AP', postura='ACOSTADO')
run(sujeto='AP', postura='PIE')
run(sujeto='AP', postura='SENTADO')

run(sujeto='AV', postura='ACOSTADO')
run(sujeto='AV', postura='PIE')
run(sujeto='AV', postura='SENTADO')

run(sujeto='CC', postura='ACOSTADO')
run(sujeto='CC', postura='PIE')
run(sujeto='CC', postura='SENTADO')

run(sujeto='CS', postura='ACOSTADO')
run(sujeto='CS', postura='PIE')
run(sujeto='CS', postura='SENTADO')

run(sujeto='DM', postura='ACOSTADO')
run(sujeto='DM', postura='PIE')
run(sujeto='DM', postura='SENTADO')

run(sujeto='DS', postura='ACOSTADO')
run(sujeto='DS', postura='PIE')
run(sujeto='DS', postura='SENTADO')

run(sujeto='GP', postura='ACOSTADO')
run(sujeto='GP', postura='PIE')
run(sujeto='GP', postura='SENTADO')
             
run(sujeto='HF', postura='ACOSTADO')
run(sujeto='HF', postura='PIE')
run(sujeto='HF', postura='SENTADO')

#run(sujeto='HS', postura='ACOSTADO')
run(sujeto='HS', postura='PIE')
run(sujeto='HS', postura='SENTADO')

run(sujeto='IH', postura='ACOSTADO')
run(sujeto='IH', postura='PIE')
run(sujeto='IH', postura='SENTADO')

run(sujeto='MM', postura='ACOSTADO')
run(sujeto='MM', postura='PIE')
run(sujeto='MM', postura='SENTADO')

run(sujeto='MR', postura='ACOSTADO')
run(sujeto='MR', postura='PIE')
run(sujeto='MR', postura='SENTADO')

run(sujeto='MV', postura='ACOSTADO')
run(sujeto='MV', postura='PIE')
run(sujeto='MV', postura='SENTADO')

run(sujeto='ND', postura='ACOSTADO')
run(sujeto='ND', postura='PIE')
run(sujeto='ND', postura='SENTADO')

run(sujeto='PC', postura='ACOSTADO')
run(sujeto='PC', postura='PIE')
run(sujeto='PC', postura='SENTADO')

run(sujeto='RO', postura='ACOSTADO')
run(sujeto='RO', postura='PIE')
run(sujeto='RO', postura='SENTADO')

run(sujeto='VT', postura='ACOSTADO')
run(sujeto='VT', postura='PIE')
run(sujeto='VT', postura='SENTADO')