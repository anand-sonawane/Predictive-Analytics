import numpy as np
import pandas as pd
import time
import os
import scipy
from scipy import io
from sklearn.cluster import AgglomerativeClustering
import threading

dir_path = os.getcwd()
feature_data = pd.read_csv("avs1.csv")
feature_data_label = pd.read_csv("labels1.csv")
feature_data = feature_data.drop(['Unnamed: 0'], axis=1)

start_time = time.time()
label = {}

def clustering(label,num_cluster):
    for i in range(2,num_cluster):
        print(i)
        start_time = time.time()
        hclustering = AgglomerativeClustering(n_clusters = i)
        hclustering.fit(feature_data)
        label[i] = list(hclustering.labels_)
    label['Category'] = feature_data_label['original']
    df = pd.DataFrame(label)
    df.to_csv(dir_path+'/CLUSTERS_Agglomerative.csv')

threads = []
for i in range(2,10):
    thread = threading.Thread(target=clustering,args=(label,i))
    threads.append(thread)

for st in threads:
    st.start()

print(time.time() - start_time)

