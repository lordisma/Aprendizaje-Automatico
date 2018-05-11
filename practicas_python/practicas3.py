#from sklearn.cross_validation import cross_validation
from sklearn.linear_model import LogisticRegressionCV
#from sklearn.feature_selection import #Que voy a necesitar#
from sklearn.linear_model import SGDClassifier
from sklearn.preprocessing import MinMaxScaler

import pandas as pd

import numpy as np

airfoil = pd.read_csv("./airfoil_self_noise.csv",header=None)
print("Shape of data: {}".format(airfoil.shape))
print("Keys of Airfoil: {}".format(airfoil.keys()))
