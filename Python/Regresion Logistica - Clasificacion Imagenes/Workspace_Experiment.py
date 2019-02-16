import azureml
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from azureml.core import Run, Workspace
from azureml.core import Experiment

# load workspace configuration from the config.json file in the current folder.
ws = Workspace.from_config()

def load_workspace():
    return ws

def load_experiment():
    # create a new experiment
    experiment_name = 'MNIST_Logistic_Regression'
    
    exp = Experiment(workspace=ws, name=experiment_name)

    return exp