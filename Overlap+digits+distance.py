
# coding: utf-8

# In[1]:


# import tensorflow as tf 
#sess = tf.InteractiveSession()
from keras.datasets import mnist
# import keras.backend as K
import numpy as np
import sys

from keras.layers import Input, Dense, Dropout, Flatten, Activation
from keras.layers.normalization import BatchNormalization
from keras.models import Model

#get_ipython().magic('matplotlib inline')
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt


# In[2]:


def to_one_hot(y, classes, dtype=np.uint8):
    n = y.shape[0]
    nb_classes = len(classes)
    idx_lookup = {_class: idx for idx, _class in enumerate(classes)}
    y_idx = [idx_lookup[_class] for _class in y]
    y_one_hot = np.zeros((n, nb_classes), dtype=dtype)
    y_one_hot[range(n), y_idx] = 1
    return y_one_hot


def accuracy(T, Y):
    return K.all(K.equal(T, K.round(Y)), axis=-1)


# In[3]:


def pair(X, T, n):
    """
    Given a digits dataset, create a new dataset with paired images. 
    Returns X_output and T_output (T_output is one-hot encoded).
    """
    X_n = X.shape[0]
    T_one_hot = to_one_hot(T, range(10))
    merge_candidates = {col: np.where(np.logical_not(T_one_hot[:, col]))[0] for col in range(10)}
    X_output_list = []
    T_output_list = []

    for _ in range(n):
        x1_idx = np.random.choice(X_n)
        x1 = X[x1_idx]
        t1 = T_one_hot[x1_idx]
        x2_idx = np.random.choice(merge_candidates[T[x1_idx]])
        x2 = X[x2_idx]
        t2 = T_one_hot[x2_idx]
        x_output = np.maximum(x1, x2)
        x_output = np.expand_dims(x_output, 0)
        X_output_list.append(x_output)
        t_output = np.maximum(t1, t2)
        t_output = np.expand_dims(t_output, 0)
        T_output_list.append(t_output)

    X_output = np.concatenate(X_output_list)
    T_output = np.concatenate(T_output_list)

    return X_output, T_output


# In[4]:


np.random.seed(0)

(X_train, T_train), (X_test, T_test) = mnist.load_data()
X_train = X_train / 255.0
X_test = X_test / 255.0
  
multiple = 1
X_train, T_train = pair(X_train, T_train, X_train.shape[0] * multiple)
X_test, T_test = pair(X_test, T_test, X_test.shape[0] * multiple)
print(X_train.shape, T_train.shape, X_train.shape[0] * multiple)


# In[5]:


print(X_train.shape, T_train.shape, X_test.shape, T_test.shape)


# In[6]:



from tqdm import tqdm

def distance(i1, i2):
    return np.sqrt((i1-i2)**2)

def distance_stast(X,T,n,layer_name = "input"):
    T = T[:n,:]
    row,col = T.shape
    same_digit_list = []
    half_digit_list = []
    diff_digit_list = []
    for i in tqdm(range(row)):
        for j in range(i+1,row):
            combine = (T[i,:] + T[j,:])
            if list(combine).count(2) == 2: #same digit
                two_digit_same = np.where(combine == 2)[0]
                img1 = X_train[i,:]
                img2 = X_train[j,:]
    #             print(("Image %s and image %s have same digits of %s and %s, with distance %s")%\
    #                   (i,j,two_digit_same[0],two_digit_same[1],distance(img1,img2)))
                same_digit_list.append([i,j,two_digit_same,distance(img1,img2)])
            if list(combine).count(2) == 1: #half digit
                one_digit_same = np.where(combine == 2)[0]
                img1 = X[i,:]
                img2 = X[j,:]
    #             print(("Image %s and image %s have same digits of %s , with distance %s")%\
    #                   (i,j,one_digit_same,distance(img1,img2)))
                half_digit_list.append([i,j,one_digit_same,distance(img1,img2)])
            if list(combine).count(2) == 0: #diff digit
                img1 = X_train[i,:]
                img2 = X_train[j,:]
                diff_digit_list.append([i,j,distance(img1,img2)])

        same_digits_df = pd.DataFrame(same_digit_list,columns=["image_1","image_2","same digits","distance"])
        same_digits_df.to_csv(layer_name + '_same_digits.csv')

        half_digits_df = pd.DataFrame(half_digit_list,columns=["image_1","image_2","same digits","distance"])
        half_digits_df.to_csv(layer_name +'_half_digits.csv')

        diff_digits_df = pd.DataFrame(diff_digit_list,columns=["image_1","image_2","distance"])
        diff_digits_df.to_csv(layer_name +'_diff_digits.csv')


# In[7]:


distance_stast(X_test,T_test,500)


# In[1]:


same_digits_df = pd.read_csv('input_same_digits.csv')
grouped = same_digits_df[["same digits","distance"]].groupby('same digits')
same_digits_distance = grouped['distance'].mean()
print(same_digits_distance)
#plt.boxplot(same_digits_distance,labels = ["Same Digits"])
#plt.show()


# In[9]:


half_digits_df = pd.read_csv('input_half_digits.csv',index_col=0)
# half_digits_df.head()
grouped = half_digits_df[["same digits","distance"]].groupby('same digits')
half_digits_distance = grouped['distance'].mean()
print(half_digits_distance)


# In[10]:


diff_digits_df = pd.read_csv('input_diff_digits.csv',index_col=0)
diff_digits_df.head()
# grouped = diff_digits_df.groupby('distance')
diff_digits_distance = diff_digits_df['distance']
print(diff_digits_distance)


# In[11]:


print(np.mean(same_digits_distance),np.mean(half_digits_distance),np.mean(diff_digits_distance))


# In[12]:


data = [list(same_digits_distance),list(half_digits_distance),list(diff_digits_distance)]
plt.boxplot(data, labels=["Same","Half","Diff"])
plt.show()

