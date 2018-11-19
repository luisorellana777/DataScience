from utils import load_data

def get_data():
    # note we also shrink the intensity values (X) from 0-255 to 0-1. This helps the model converge faster.
    X_train = load_data('./Python/Regresion Logistica - Clasificacion Imagenes/data/train-images.gz', False) / 255.0
    y_train = load_data('./Python/Regresion Logistica - Clasificacion Imagenes/data/train-labels.gz', True).reshape(-1)

    X_test = load_data('./Python/Regresion Logistica - Clasificacion Imagenes/data/test-images.gz', False) / 255.0
    y_test = load_data('./Python/Regresion Logistica - Clasificacion Imagenes/data/test-labels.gz', True).reshape(-1)

    return X_train, y_train, X_test, y_test

