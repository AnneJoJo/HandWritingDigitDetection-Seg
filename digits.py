import tensorflow as tf 
#sess = tf.InteractiveSession()
from keras.datasets import mnist
import keras.backend as K
import numpy as np
import sys

from keras.layers import Input, Dense, Dropout, Flatten, Activation
from keras.layers.normalization import BatchNormalization
from keras.models import Model
import matplotlib.pyplot as plt


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


def pair(X, T, n):
    """
    Given a digits dataset, create a new dataset with paired images. 
    Returns X_output and T_output (T_output is one-hot encoded).
    """
    X_n = X.shape[0]
    T_one_hot = to_one_hot(T, range(10))
    merge_candidates = {col: np.where(np.logical_not(T_one_hot[:, col]))[0] for col in range(10)}
    print(merge_candidates)
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


#def main(argv):
    np.random.seed(0)

    (X_train, T_train), (X_test, T_test) = mnist.load_data()
    X_train = X_train / 255.0
    X_test = X_test / 255.0
  
    multiple = 5
    X_train, T_train = pair(X_train, T_train, X_train.shape[0] * multiple)
    X_test, T_test = pair(X_test, T_test, X_test.shape[0] * multiple)

    input_dims = X_train.shape[1:]
    output_dim = T_train.shape[-1]
    
    input_dims = X_test.shape[1:]
    output_dim = T_test.shape[-1]
    x = input = Input(shape=input_dims)
    x = Flatten()(x)

    activation = 'relu'
    dropout = 0.2

    x = Dense(256)(x)
    x = BatchNormalization()(x)
    x = Activation(activation)(x)
    x = Dropout(dropout)(x)

    x = Dense(128)(x)
    x = BatchNormalization()(x)
    x = Activation(activation)(x)
    x = Dropout(dropout)(x)

    x = Dense(output_dim, activation='sigmoid')(x)

    model = Model(input, x)
    model.summary()
    
    model.get_config()
    model.compile(optimizer='adam', loss='binary_crossentropy', metrics=[accuracy])
   
    model.fit(X_train, T_train,
              epochs=100,
              batch_size=128,
              verbose=1,
              shuffle=True)

    # evaluate returns a list. The first element has the loss, followed by the
    # metrics specified when compiling the model.
    print("train:{}").format(model.evaluate(X_train, T_train, verbose=0))
    print("test:{}").format(model.evaluate(X_test, T_test, verbose=0))
    #I don't know why I cannot run two line code above,so I revise them into:
    print(model.evaluate(X_train, T_train, verbose=1))
    print(model.evaluate(X_test, T_test, verbose=1))
    
    print(model.predict(X_test))
    T_test_2=model.predict(X_test)
    
    return 0


#if __name__ == '__main__':
    #sys.exit(main(sys.argv))
############give a .csv table for the results################
import seaborn as sns
from pandas import *
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix
#image display
newlabel = pd.read_csv('d:/new_result_label.csv')
 for i in newlabel.columns[:0]:
     print(i)
     plt.imshow(X_test[i])
 
    
 plt.imshow(X_test[19])
 plt.imshow(X_test[82])
 plt.imshow(X_test[120])
 plt.imshow(X_test[125])
 plt.imshow(X_test[147])
 plt.imshow(X_test[184])
 
def same:
    
#preformace 
#############################################################
#############section for function############################
#manipulate result into a presentable pattern################
#############################################################
##pick the two largest value as predicted digits
def labelize(d):
    for i in range(len(d)):
        t=np.max(d[i])
        for j in range(len(d[i])):  
            if d[i,j]==t:
                d[i,j]=-1
            else:continue
    for i in range(len(d)):
         t=np.max(d[i])
         for j in range(len(d[i])):  
            if d[i,j]==t:
                d[i,j]=1
            else:continue                        
    for i in range(len(d)):
         for j in range(len(d[i])):  
             if d[i,j]==-1 or d[i,j]==1:
                d[i,j]=1   
             else:d[i,j]=0    
    return d
    

def predilabel (result,d_matrix_pred): 
    for m in range(50000):
        for n in range(10):
           if d_matrix_pred[m,n]!=0 and result[m,0]!=0:
               result[m,1]=d_matrix_pred[m,n]
           elif not d_matrix_pred[m,n]==0: result[m,0]=d_matrix_pred[m,n]
                 
    for n in range(50000):
        if result[n,0]==11:
            result[n,0]=0
    return result, d_matrix_pred
         

def actulabel(result_actul,d_matrix_actul):       
    for m in range(50000):
        for n in range(10):
            if d_matrix_actul[m,n]!=0 and result_actul[m,0]!=0:
                result_actul[m,1]=d_matrix_actul[m,n]
            elif not d_matrix_actul[m,n]==0: result_actul[m,0]=d_matrix_actul[m,n]
                 
    for n in range(50000):
        if result_actul[n,0]==11:
            result_actul[n,0]=0
    return result_actul, d_matrix_actul
    
 
   ## we generate 4 situation of two digits result
   ## Both wrong-(None of two digits get right) without consideration of order
   ## Both right-(two digtis get right) without consideration of order
   ## only one got correct(we ignore the order of digits, but which one gets right is still vital)       
def stringlabel(result,result_actul):   
    d_pn=np.zeros((50000,2),dtype='uint8')#container for judge 
    for p in range(50000):
        if result[p,0]==result_actul[p,0]:
            d_pn[p,0]=1
        elif result[p,0]==result_actul[p,1]:
            d_pn[p,1]=1
        if result[p,1]==result_actul[p,0]:
            d_pn[p,0]=1
        elif result[p,1]==result_actul[p,1]:
            d_pn[p,1]=1
 
    label=np.empty((50000,1),dtype=object)
    for p in range(50000):
        if  d_pn[p,0]!=d_pn[p,1] and d_pn[p,0]==1:
            label[p]="h1"
        elif d_pn[p,0]!=d_pn[p,1] and d_pn[p,0]==0:
            label[p]="h2"
        elif d_pn[p,0]==0:
            label[p]="wrong"
        elif d_pn[p,0]==1:
            label[p]="right"
    results=np.hstack((result_actul,result,label))
    return results


#main    
    d=model.predict(X_test)
    
    d=labelize(T_test)
    
    result=np.zeros((50000,2),dtype='uint8') # container for predi-labels to manipulate   
    #d_pred=np.array(d,dtype='uint8') 
    d_pred=np.array(T_test,dtype='uint8') 
    digit=np.array([11,1,2,3,4,5,6,7,8,9],dtype='uint8')#give them a digit label
    d_matrix_pred=d_pred*digit
    
    result,d_matrix_pred=predilabel(result,d_matrix_pred)
    result_actul=np.zeros((50000,2),dtype='uint8') # container for actual-labels to manipulate             
    d_matrix_actul=T_test*digit    
    result_actul, d_matrix_actul=actulabel(result_actul,d_matrix_actul)
    results=stringlabel(result,result_actul)
    
    new_result_label=pd.DataFrame(np.array(result_actul))
    new_result_label.to_csv('d:/newresultlabel.csv',index=False, header=False)
    
    
    new_results=pd.DataFrame(np.array(results))
    
    new_results.to_csv('d:/new_results.csv',index=False, header=False)
    Act_results=pd.DataFrame(np.array(result_actul))
    Act_results.to_csv('d:/act_results.csv',index=False, header=False)
  
    te_results=pd.DataFrame(np.array(T_test_2))
    te_results.to_csv('d:/te_results.csv',index=False, header=False)
    
    inputdata=pd.DataFrame(np.array(d))
    inputdata.to_csv('d:/inputdata.csv',index=False, header=False)
    
    