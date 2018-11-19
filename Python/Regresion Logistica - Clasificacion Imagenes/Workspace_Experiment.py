import azureml
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from azureml.core import Run, Workspace
from azureml.core import Experiment

# load workspace configuration from the config.json file in the current folder.
def load_workspace():
    ws = Workspace.from_config()
    print(ws.name, ws.location, ws.resource_group, ws.location, sep = '\t')

    return ws

def load_exoeriment():
    # create a new experiment
    experiment_name = 'MNIST_Logistic_Regression'
    
    exp = Experiment(workspace=load_workspace(), name=experiment_name)

    return exp