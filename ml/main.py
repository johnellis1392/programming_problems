import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
# import sklearn
# import tensorflow as tf

if __name__ == '__main__':
  path = os.getcwd() + '/data/ex1data1.txt'
  data = pd.read_csv(path, header=None, names=['Population', 'Profit'])
  # print(data.head())
  # print(data.describe())
  data.plot(kind='scatter', x='Population', y='Profit', figsize=(12, 8))
