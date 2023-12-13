import numpy as np

def crps(y_true, y_pred):
    y_true = np.clip(np.cumsum(y_true, axis=1), 0, 1)
    y_pred = np.clip(np.cumsum(y_pred, axis=1), 0, 1)
    score  = ((y_true-y_pred)**2).sum(axis=1).sum(axis=0) / (y_true.shape[1]*y_true.shape[0])

    return(score)

a = np.array([range(1, 7)]).reshape(-1, 3) / 8
b = np.array([range(13, 1, -2)]).reshape(-1, 3) / 14
crps(a, b)