import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from kmodes.kprototypes import KPrototypes # pip install kmodes
import sys
import os
import joblib 
import csv 
from csv import writer
import warnings
from datetime import datetime


warnings.filterwarnings('ignore')

gitrepo = "C:/Users/Antonio Pelluz/Desktop/Cluster3/" #importante cambiar con el directorio que se usa

import statistics
from statistics import mode

def kprot_missing(X, n_clusters, cat_index, max_iter=10):  # cat_index has to be a vector: [5], or [3,4,5] , never an integer
    """Perform K-Means clustering on data with missing values.
    Args:
      X: An [n_samples, n_features] array of data to cluster.
      n_clusters: Number of clusters to form.
      max_iter: Maximum number of EM iterations to perform.
    Returns:
      labels: An [n_samples] vector of integer labels.
      centroids: An [n_clusters, n_features] array of cluster centroids.
      X_hat: Copy of X with the missing values filled in.
    """
    # Initialize missing values to their column means
    missing = ~np.isfinite(X)
    mu = np.nanmean(X, 0, keepdims=1) # average values for each feature
    modes = np.apply_along_axis(mode, 0, X)  # mode of each feature
    mu = mu.reshape(modes.shape[0])  # this is for chaning the shape for later on (3 lines), but the values stays as they are
    num_index = list(set(range(0,X.shape[1]))^set(cat_index)) # the index of numerical features (complementary to cat_index)
    filling = mu
    filling[cat_index] = modes[cat_index]  # we create a vector with mean and mode of each feature
    X_hat = np.where(missing, filling, X)
    cls = KPrototypes(n_clusters=n_clusters, init='Cao', n_init=1, verbose=0)
    # initialize KMeans with the previous set of centroids. this is much
    # faster and makes it easier to check convergence (since labels
    # won't be permuted on every iteration), but might be more prone to
    # getting stuck in local minima.
    for i in range(max_iter):
        # perform clustering on the filled-in data
        labels = cls.fit_predict(X_hat, categorical=list(cat_index)) 
        centroids = cls._enc_cluster_centroids
        centroids_num = centroids[0]
        centroids_cat = centroids[1]
        centroids_all= np.empty([centroids_num.shape[0], centroids_num.shape[1]+centroids_cat.shape[1] ])
        centroids_all[:,cat_index] = centroids_cat
        centroids_all[:,num_index] = centroids_num
        # fill in the missing values based on their cluster centroids
        X_hat[missing] = centroids_all[labels][missing]
        # when the labels have stopped changing then we have converged
        if i > 0 and np.all(labels == prev_labels):
            break
        prev_labels = labels
        prev_centroids = centroids
        gamma = cls.gamma
        cls = KPrototypes(n_clusters=n_clusters, init=prev_centroids, verbose=0)
    return labels, centroids_all, X_hat, gamma, cls




def split_num_cat2(X, categorical):
    """Extract numerical and categorical columns.
    Convert to numpy arrays, if needed.
    :param X: Feature matrix
    :param categorical: Indices of categorical columns
    """
    Xnum = np.asanyarray(X[:, [ii for ii in range(X.shape[1])
                               if ii not in categorical]]).astype(np.float64)
    Xcat = np.asanyarray(X[:, categorical])
    return Xnum, Xcat


def labels_cost2(Xnum, Xcat, centroids, num_dissim, cat_dissim, gamma, labels, num_index,cat_index, membship=None):
    """Calculate labels and cost function given a matrix of points and
    a list of centroids for the k-prototypes algorithm.
    """
    n_points = Xnum.shape[0]
    #Xnum = check_array(Xnum)
    tot_costs = np.empty(n_points)
    for ipoint in range(n_points):
        # Numerical cost = sum of Euclidean distances
        num_costs = num_dissim(centroids[labels[ipoint]][num_index], Xnum[ipoint])
        cat_costs = cat_dissim(centroids[labels[ipoint]][cat_index], Xcat[ipoint], X=Xcat, membship=membship)
        # Gamma relates the categorical cost to the numerical cost.
        tot_costs[ipoint] = num_costs + gamma * cat_costs
    return tot_costs


def heatingtype_to_numeric(x):
    if x=='Biomass':
        return 1
    if x=='District heating':
        return 2
    if x=='District Heating':
        return 2
    if x=='Electric':
        return 3
    if x=='Electricity':
        return 3
    if x=='Gas':
        return 4
    if x=='Heat network':
        return 5
    if x=='Heat network and steam':
        return 6
    if x=='Oil':
        return 7


def industry_to_numeric(x):
    if x=='Commercial Property':
        return 1
    if x=='Commercial':
        return 1
    if x=='Education':
        return 2
    if x=='Government':
        return 3
    if x=='Multifamily':
        return 4
    if x=='Single Family':
        return 5


def rating_to_numeric(x):
    if x=='B':
        return 1
    if x=='C':
        return 2
    if x=='D':
        return 3
    if x=='E':
        return 4
    if x=='F':
        return 5
    if x=='G':
        return 6

def subindustry_to_numeric(x):
    if x=='Bank/Financial Services':
        return 1
    if x=='Business Services':
        return 2
    if x=='College/University':
        return 3
    if x=='Commercial Real Estate':
        return 4
    if x=='Corporate Office':
        return 5
    if x=='Other Government Buildings':
        return 6
    if x=='Primary/Secondary School':
        return 7
    if x=='Social Services':
        return 8

def timezone_to_numeric(x):
    if x=='America/Chicago':
        return 1
    if x=='America/Denver':
        return 2
    if x=='America/Los_Angeles':
        return 3
    if x=='America/New_York':
        return 4
    if x=='America/Phoenix':
        return 5
    if x=='Asia/Singapore':
        return 6
    if x=='Europe/London':
        return 7
    if x=='Europe/Zurich':
        return 8
    if x=='America/Texas':
        return 9


def yearbuilt_to_numeric(x):
    if x=='11th Century onwards':
        return 1200
    if x=='1862-1875':
        return 1868
    if x=='1888-1890':
        return 1889
    if x=='1898-1902':
        return 1900
    if x=='1903-1906':
        return 1904
    if x=='1913-1915':
        return 1914
    if x=='1919-1945':
        return 1932
    if x=='1945-1966':
        return 1955
    if x=='1967-1976':
        return 1971
    if x=='post 1976':
        return 1982
    if x=='Post 1976':
        return 1982
    if x=='pre 1919':
        return 1910
    if x=='Pre 1919':
        return 1910
    if x=='Post 2000':
        return 2011

def numeric_yearbuild(x):
    if x=='11th Century onwards':
        return 1200
    if x=='1862-1875':
        return 1868
    if x=='1888-1890':
        return 1889
    if x=='1898-1902':
        return 1900
    if x=='1903-1906':
        return 1904
    if x=='1913-1915':
        return 1914
    if x=='1919-1945':
        return 1932
    if x=='1945-1966':
        return 1955
    if x=='1967-1976':
        return 1971
    if x=='post 1976':
        return 1982
    if x=='Post 1976':
        return 1982
    if x=='pre 1919':
        return 1910
    if x=='Pre 1919':
        return 1910
    if x=='Post 2000':
        return 2011
    if x=='nan':
        return 0
    return(x)

def primaryspaceuse_abbrev_to_numeric(x):
    if x=='Office':
        return 1
    if x=='PrimClass':
        return 2
    if x=='UnivClass':
        return 3
    if x=='UnivDorm':
        return 4
    if x=='UnivLab':
        return 5
    if x=='UnivLib':
        return 6
    if x=='UnivDining':
        return 7

def CrearNulo(x):
    if x == 0: 
        return ""
    return x

def CrearNuloCaracter(x):
    if x == "nan": 
        return ""
    return x

def SeleccionNombre(nombre):
    if nombre == "nan": 
        return datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    return nombre

def euclidean_dissim(a, b, **_):
    """Euclidean distance dissimilarity function"""
    if np.isnan(a).any() or np.isnan(b).any():
        raise ValueError("Missing values detected in numerical columns.")
    return np.sum((a - b) ** 2)

def matching_dissim(a, b, **_):
    """Simple matching dissimilarity function"""
    return np.sum(a != b)


def kprot():
    study1 = pd.read_csv(gitrepo + 'meta_opentrynew.csv', sep=',', index_col=0)
    vbles= ["energystarscore", "heatingtype", "industry",
            "numberoffloors", "occupants", "rating", 
            "sqm", "subindustry", "timezone", "yearbuilt", "primaryspaceuse_abbrev","Electric_Savings","Gas_Savings"]
    df = study1[vbles]
    # type: num, cat, cat 
    # type: cat, num, cat
    # type: num, cat, cat, num, cat
    # cat_index = [1,2,3,5,7,8,10]
    df['heatingtype'] = df['heatingtype'].apply(heatingtype_to_numeric)
    df['industry'] = df['industry'].apply(industry_to_numeric)
    df['rating'] = df['rating'].apply(rating_to_numeric)
    df['subindustry'] = df['subindustry'].apply(subindustry_to_numeric)
    df['timezone'] = df['timezone'].apply(timezone_to_numeric)
    df['yearbuilt'] = df['yearbuilt'].apply(yearbuilt_to_numeric)
    df['primaryspaceuse_abbrev'] = df['primaryspaceuse_abbrev'].apply(primaryspaceuse_abbrev_to_numeric)
    df = df.to_numpy()
    labels, centroids, X_hat, gamma, modelF = kprot_missing(df, n_clusters=15, cat_index = [1,2,3,5,7,8,10], max_iter=1000)
    study1.to_csv('estudio.csv') #se crea un segundo csv que es donde se encuentran los datos del csv en el momento en el que se entrena para la formación de la gráfica del programa
    Xnum, Xcat = split_num_cat2(X_hat, categorical = [1,2,3,5,7,8,10])
    num_dissim=euclidean_dissim
    cat_dissim=matching_dissim
    cat_index = [1,2,3,5,7,8,10]
    num_index = list(set(range(0,X_hat.shape[1]))^set(cat_index))
    centroidsCat = centroids[cat_index]
    # distance between points and their cluster prototype
    distances = labels_cost2(Xnum, Xcat, centroids, num_dissim, cat_dissim, gamma, labels,num_index,cat_index)
    df2 = pd.DataFrame(df)
    X_hat2 = pd.DataFrame(X_hat)
    labels2dist = pd.DataFrame(labels, distances)
    centroids2 = pd.DataFrame(centroids)
    df2.to_csv("kprototypes-df-iii15.csv", sep=';')
    X_hat2.to_csv("kprototypes-X_hat-iii15.csv", sep=';')
    labels2dist.to_csv("kprototypes-labels-dist-iii15.csv", sep=';')
    centroids2.to_csv("kprototypes-centroids-iii15.csv", sep=';')
    joblib.dump(modelF, "modelF.pkl") # Guardamos el modelo.

attempts = 0
while attempts < 10: 
    try:
        kprot()
        break  
    except Exception as e:
        attempts += 1
        






