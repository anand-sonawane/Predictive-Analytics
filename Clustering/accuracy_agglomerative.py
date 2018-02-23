import os
import pandas as pd
import json
from collections import Counter

dir_path = os.getcwd()

df = pd.read_csv(dir_path+'/CLUSTERS_Agglomerative.csv',index_col=None)
len = df.shape[0]   # total number of rows in dataframe

df_exp=pd.DataFrame()
acc = {}
for cluster_number in range(2,60):
    df1 = df[[str(cluster_number), 'Category']]
    count = 0
    for i in range(cluster_number):
        df2 = df1.loc[df1[str(cluster_number)] == i]
        items_counts = df2['Category'].value_counts()
        max_item = items_counts[0]   ## highest value from a particular cluster
        count += max_item
    	accuracy = float(count*100)/len
    	acc[cluster_number] = accuracy
df_acc = pd.DataFrame(acc.items(), columns=['num_of_cluster', 'Accuracy'], index = None)
df_acc.to_csv(dir_path+'/ACCURACY_agglomerative.csv')


    