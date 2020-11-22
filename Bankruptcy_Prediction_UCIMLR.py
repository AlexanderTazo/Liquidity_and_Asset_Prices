# -*- coding: utf-8 -*-
"""
Created on Tue July 09 08:52:13 2019

@author: ATazo

Bankruptcy Prediction- UCI ML Repository Dataset

"""



# To supress warnings
import warnings
warnings.filterwarnings("ignore")
warnings.filterwarnings("ignore", category=DeprecationWarning)

    # Basic Libraries for Data organization, Statistical operations and Plotting
import numpy as np
import pandas as pd
%matplotlib inline
# For loading .arff files
from scipy.io import arff
# To analyze the type of missing data
import missingno as msno
# Library for performing k-NN and MICE imputations 
#import fancyimpute
from missingpy import KNNImputer
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
# Library to perform Expectation-Maximization (EM) imputation
import impyute as impy
# To perform mean imputation
from sklearn.preprocessing import Imputer
#To perform kFold Cross Validation
from sklearn.model_selection import KFold
# Formatted counter of class labels
from collections import Counter
# Ordered Dictionary
from collections import OrderedDict
# Library imbalanced-learn to deal with the data imbalance. To use SMOTE oversampling
from imblearn.over_sampling import SMOTE 

# Impoting classification models
from xgboost import XGBClassifier
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from imblearn.ensemble import BalancedBaggingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB

import random

from sklearn.metrics import accuracy_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_curve
from sklearn.metrics import precision_recall_curve


def load_arff_raw_data() :
    N=5
    return[arff.loadarff("data/"+ str(i+1) + "year.arff") for i in range(N)]

def load_dataframes():
    return[pd.DataFrame(data_i_year[0]) for data_i_year in load_arff_raw_data()]
    


dataframes = load_dataframes()


def set_new_headers(dataframes):
    cols = ['X' + str(i+1) for i in range(len(dataframes[0].columns)-1)]
    cols.append('Y')
    for df in dataframes:
        df.columns = cols
    
set_new_headers(dataframes)    


dataframes[0].head()


def convert_columns_type_float(dfs):
    for i in range(5):
        index = 1
        while(index<= 63):
            colname = dfs[i].columns[index]
            col = getattr(dfs[i], colname)
            dfs[i][colname] = col.astype(float)
            index+=1
            
convert_columns_type_float(dataframes)


def convert_class_label_type_int(dfs):
    for i in range(len(dfs)):
        col = getattr(dfs[i], 'Y')
        dfs[i]['Y'] = col.astype(int)
        
convert_class_label_type_int(dataframes)

def drop_nan_rows(dataframes, verbose=False):
    clean_dataframes = [df.dropna(axis=0, how='any') for df in dataframes]
    if verbose:
        for i in range(len(dataframes)):
            print(str(i+1)+'year:','Original Length=', len(dataframes[i]), '\tCleaned Length=', len(clean_dataframes[i]), '\tMissing Data=', len(dataframes[i])-len(clean_dataframes[i]))
    return clean_dataframes

nan_dropped_dataframes = drop_nan_rows(dataframes, verbose=True)
        

def generate_sparsity_matrix(dfs):
    for i in range(5):
        missing_df_i = dfs[i].columns[dfs[i].isnull().any()].tolist()
        msno.matrix(dfs[i][missing_df_i], figsize = (20,5))

generate_sparsity_matrix(dataframes)


def generate_heatmap(dfs):
    for i in range(5):
        missing_df_i = dfs[i].columns[dfs[i].isnull().any()].tolist()
        msno.heatmap(dfs[i][missing_df_i], figsize = (20,20))

generate_heatmap(dataframes)


def perform_mean_imputation(dfs):
    imputer = Imputer(missing_values = np.nan, strategy = "mean", axis = 0)
    mean_imputed_dfs = [pd.DataFrame(imputer.fit_transform(df)) for df in dfs]
    for i in range(len(dfs)):
        mean_imputed_dfs[i].columns= dfs[i].columns
    return mean_imputed_dfs
mean_imputed_dataframes = perform_mean_imputation(dataframes)


def perform_knn_imputation(dfs):
    knn_imputed_datasets = [KNNImputer(n_neighbors=100, copy = True).fit_transform(dfs[i]) for i in range(len(dfs))]
    return [pd.DataFrame(data=knn_imputed_datasets[i]) for i in range(len(dfs))]
    
knn_imputed_dataframes = perform_knn_imputation(dataframes)
set_new_headers(knn_imputed_dataframes)


def perform_EM_imputation(dfs):
    em_imputed_datasets = [impy.imputation.cs.em(dfs[i].values, loops=50, dtype='cont') for i in range(len(dfs))]
    return [pd.DataFrame(data=em_imputed_datasets[i]) for i in range(len(dfs))]
em_imputed_dataframes = perform_EM_imputation(dataframes)
set_new_headers(em_imputed_dataframes)


def perform_MICE_imputation(dfs):
    mice_imputed_datasets = [IterativeImputer(sample_posterior=True).fit_transform(dfs[i]) for i in range(len(dfs))]
    return [pd.DataFrame(data=mice_imputed_datasets[i]) for i in range(len(dfs))]

mice_imputed_dataframes = perform_MICE_imputation(dataframes)
set_new_headers(mice_imputed_dataframes)


imputed_dataframes_dictionary = OrderedDict()
imputed_dataframes_dictionary["Mean"]= mean_imputed_dataframes
imputed_dataframes_dictionary["k-NN"]= knn_imputed_dataframes
imputed_dataframes_dictionary["EM"] = em_imputed_dataframes
imputed_dataframes_dictionary["MICE"]= mice_imputed_dataframes


def check_data_imbalance(dfs):
    for i in range(len(dfs)):
        print('Dataset: '+str(i+1)+'year')
        print(dfs[i].groupby('Y').size())
        minority_percent = (dfs[i]['Y'].tolist().count(1) / len(dfs[i]['Y'].tolist()))*100
        print('Minority (label 1) percentage: '+  str(minority_percent) + '%')
        print('-'*64)
        
check_data_imbalance(dataframes)


def split_dataframes_features_labels(dfs):
    feature_dfs = [dfs[i].iloc[:,0:64] for i in range(len(dfs))]
    label_dfs = [dfs[i].iloc[:,64] for i in range(len(dfs))]
    return feature_dfs, label_dfs


def oversample_data_SMOTE(dfs, verbose = False):
    smote = SMOTE(ratio = "auto", random_state = 42, k_neighbors = 10)
    feature_dfs, label_dfs = split_dataframes_features_labels(dfs)
    resampled_feature_arrays = []
    resampled_label_arrays = []
    for i in range(len(dfs)):
        if verbose: print("Dataset:" + str(i+1)+ "year:")
        if verbose: print("Original dataset shape {}".format(Counter(label_dfs[i])))
        dfi_features_res, dfi_label_res = smote.fit_sample(feature_dfs[i], label_dfs[i])
        if verbose: print("Resampled dataset shape {}\n".format(Counter(dfi_label_res)))
        resampled_feature_arrays.append(dfi_features_res)
        resampled_label_arrays.append(dfi_label_res)
    return resampled_feature_arrays, resampled_label_arrays


def restructure_arrays_to_dataframes(feature_arrays, label_arrays):
    resampled_dfs = []
    for i in range(len(feature_arrays)):
        feature_df = pd.DataFrame(data = feature_arrays[i])
        label_df = pd.DataFrame(data = label_arrays[i])
        label_df.columns = ["Y"]
        resampled_dfs.append(feature_df.join(label_df))
    set_new_headers(resampled_dfs)
    return resampled_dfs

def perform_oversampling_on_imputed_dataframes(df_dict):
    imputed_oversampled_dataframes_dictionary = OrderedDict()
    for key, dfs in df_dict.items():
        print("SMOTE Oversampling for " + key + " imputed dataframes\n")
        smote_feature_arrays, smote_label_arrays = oversample_data_SMOTE(dfs, verbose= True)
        oversampled_dataframes = restructure_arrays_to_dataframes(smote_feature_arrays, smote_label_arrays)
        imputed_oversampled_dataframes_dictionary[key] = oversampled_dataframes
        print("-"*100)
    return imputed_oversampled_dataframes_dictionary

imputed_oversampled_dataframes_dictionary = perform_oversampling_on_imputed_dataframes(imputed_dataframes_dictionary)

def prepare_kfold_cv_data(k, X, y, verbose = False):
    X = X.values
    y = y.values
    kf = KFold(n_splits = k, shuffle = False, random_state = 42)
    X_train = []
    y_train = []
    X_test = []
    y_test = []
    
    for train_index, test_index in kf.split(X):
        X_train.append(X[train_index])
        y_train.append(y[train_index])
        X_test.append(X[test_index])
        y_test.append(y[test_index])
    return X_train, y_train, X_test, y_test

gnb_classifier = GaussianNB()
lr_classifier = LogisticRegression(penalty= "l1", random_state = 0)
dt_classifier = DecisionTreeClassifier(random_state = 42)
rf_classifier = RandomForestClassifier(n_estimators=5, criterion="entropy")
xgb_classifier = XGBClassifier()
bb_classifier = BalancedBaggingClassifier(base_estimator=RandomForestClassifier(criterion="entropy"), n_estimators=5, bootstrap=True)

models_dictionary = OrderedDict()

models_dictionary['Gaussian Naive Bayes'] = gnb_classifier
models_dictionary['Logistic Regression'] = lr_classifier
models_dictionary['Decision Tree'] = dt_classifier
models_dictionary['Extreme Gradient Boosting'] = xgb_classifier
models_dictionary['Random Forest'] = rf_classifier
models_dictionary['Balanced Bagging'] = bb_classifier



feature_dfs_mean, label_dfs_mean = split_dataframes_features_labels(imputed_oversampled_dataframes_dictionary["Mean"])

year_results = OrderedDict()

for df_index in range(5):
    X_train_list, y_train_list, X_test_list, y_test_list = prepare_kfold_cv_data(5,feature_dfs_mean[df_index], label_dfs_mean[df_index])
   
    metrics_results = OrderedDict()
    accuracy_list = np.zeros([5])
    precision_list = np.zeros([5,2])
    recall_list = np.zeros([5,2])
    TN_list = np.zeros([5])
    FP_list = np.zeros([5])
    FN_list = np.zeros([5])
    TP_list = np.zeros([5])
    
    
    for k_index in range(5):
        X_train = X_train_list[k_index]
        y_train = y_train_list[k_index]
        X_test = X_test_list[k_index]
        y_test = y_test_list[k_index] 
        
        clf = gnb_classifier.fit(X_train, y_train)
        y_test_predicted = clf.predict(X_test)
                
        accuracy_gnb = accuracy_score(y_test, y_test_predicted, normalize= True)
        accuracy_list[k_index] = accuracy_gnb
    
        recall_gnb = recall_score(y_test, y_test_predicted, average = None)
        recall_list[k_index] = recall_gnb
                    
        precision_gnb = precision_score(y_test, y_test_predicted, average = None)
        precision_list[k_index] = precision_gnb
                    
        confusion_matrix_gnb = confusion_matrix(y_test, y_test_predicted)

    
    metrics_results['Accuracy'] = np.mean(accuracy_list)
    metrics_results['Precisions'] = np.mean(precision_list, axis=0)
    metrics_results['Recalls'] = np.mean(recall_list, axis=0)
    year_results[str(df_index+1)+'year'] = metrics_results 
 


for df_index in range(5):
                X_train_list, y_train_list, X_test_list, y_test_list = prepare_kfold_cv_data(5, feature_dfs_mean[df_index], label_dfs_mean[df_index])
                
                metrics_results = OrderedDict()
                accuracy_list = np.zeros([5])
                precision_list = np.zeros([5,2])
                recall_list = np.zeros([5,2])
                TN_list = np.zeros([5])
                FP_list = np.zeros([5])
                FN_list = np.zeros([5])
                TP_list = np.zeros([5])                
                
                # Iterate over all the k-folds
                for k_index in range(5):
                    X_train = X_train_list[k_index]
                    y_train = y_train_list[k_index]
                    X_test = X_test_list[k_index]
                    y_test = y_test_list[k_index]
                    
                    # Fit the model and 
                    clf = clf.fit(X_train, y_train)
                    y_test_predicted = clf.predict(X_test)
                    
                    #code for calculating accuracy 
                    _accuracy_ = accuracy_score(y_test, y_test_predicted, normalize=True)
                    accuracy_list[k_index] = _accuracy_
                    
                    #code for calculating recall 
                    _recalls_ = recall_score(y_test, y_test_predicted, average=None)
                    recall_list[k_index] = _recalls_
                    
                    #code for calculating precision 
                    _precisions_ = precision_score(y_test, y_test_predicted, average=None)
                    precision_list[k_index] = _precisions_
                    
                    #code for calculating confusion matrix 
                    _confusion_matrix_ = confusion_matrix(y_test, y_test_predicted)
                    TN_list[k_index] = _confusion_matrix_[0][0]
                    FP_list[k_index] = _confusion_matrix_[0][1]
                    FN_list[k_index] = _confusion_matrix_[1][0]
                    TP_list[k_index] = _confusion_matrix_[1][1]
                
                # creating a metrics dictionary
                metrics_results['Accuracy'] = np.mean(accuracy_list)
                metrics_results['Precisions'] = np.mean(precision_list, axis=0)
                metrics_results['Recalls'] = np.mean(recall_list, axis=0)
                metrics_results['TN'] = np.mean(TN_list)
                metrics_results['FP'] = np.mean(FP_list)
                metrics_results['FN'] = np.mean(FN_list)
                metrics_results['TP'] = np.mean(TP_list)
            
                year_results[str(df_index+1)+'year'] = metrics_results 

    
    

def perform_data_modelling(_models_, _imputers_, verbose = False, k_folds = 5):
    model_results = OrderedDict()
    
    for model_name, clf in _models_.items():
        if verbose: print("-"*120, "\n", "Model: " + "\033[1m" + model_name + "\033[0m" + " Classifier")
            
        imputer_results = OrderedDict()
        
        for imputer_name, dataframes_list in _imputers_.items():
            if verbose: print("\tImputer Technique: " + "\033[1m" + imputer_name + "\033[0m")
            
            feature_dfs, label_dfs = split_dataframes_features_labels(dataframes_list)
            
            year_results = OrderedDict()
            
            for df_index in range(len(dataframes_list)):
                if verbose: print("\t\tDataset: " + "\033[1m" + str(df_index + 1) + "year" + "\033[0m")
                X_train_list, y_train_list, X_test_list, y_test_list = prepare_kfold_cv_data(k_folds, feature_dfs[df_index], label_dfs[df_index], verbose)
                    
                metrics_results = OrderedDict()
                accuracy_list = np.zeros([k_folds])
                precision_list = np.zeros([k_folds, 2])
                recall_list = np.zeros([k_folds, 2])
                TN_list = np.zeros([k_folds])
                FP_list = np.zeros([k_folds])
                FN_list = np.zeros([k_folds])
                TP_list = np.zeros([k_folds])
                    
                for k_index in range(k_folds):
                    X_train = X_train_list[k_index]
                    y_train = y_train_list[k_index]
                    X_test = X_test_list[k_index]
                    y_test = y_test_list[k_index]
                        
                    clf = clf.fit(X_train, y_train)
                    y_test_predicted = clf.predict(X_test)
                        
                    _accuracy_ = accuracy_score(y_test, y_test_predicted, normalize=True)
                    accuracy_list[k_index] = _accuracy_
                        
                    _recalls_ = recall_score(y_test, y_test_predicted, average=None)
                    recall_list[k_index] = _recalls_
                        
                    _precisions_ = precision_score(y_test, y_test_predicted, average=None)
                    precision_list[k_index] = _precisions_
                        
                    _confusion_matrix_ = confusion_matrix(y_test, y_test_predicted)
                    TN_list[k_index] = _confusion_matrix_[0][0]
                    FP_list[k_index] = _confusion_matrix_[0][1]
                    FN_list[k_index] = _confusion_matrix_[1][0]
                    TP_list[k_index] = _confusion_matrix_[1][1]
                        
                metrics_results["Accuracy"] = np.mean(accuracy_list)
                metrics_results["Precisions"] = np.mean(precision_list, axis = 0)
                metrics_results["Recalls"] = np.mean(recall_list, axis = 0)
                metrics_results['TN'] = np.mean(TN_list)
                metrics_results['FP'] = np.mean(FP_list)
                metrics_results['FN'] = np.mean(FN_list)
                metrics_results['TP'] = np.mean(TP_list)
                    
                if verbose:
                    print("\t\t\tAccuracy: ", metrics_results["Accuracy"])
                    print("\t\t\tPrecision: ", metrics_results["Precisions"])
                    print("\t\t\tRecall: ", metrics_results["Recalls"])
                        
                year_results[str(df_index + 1) + "year"] = metrics_results
                    
            imputer_results[imputer_name] = year_results
                
        model_results[model_name] = imputer_results
            
    return model_results
                        

results = perform_data_modelling(models_dictionary, imputed_oversampled_dataframes_dictionary, verbose = True, k_folds= 5)


# model -> imputer -> year
def perform_model_ranking(models, imputers, results):
    column_headers = ['-'] + list(imputers.keys())
    rows = []
    for model_name, model_details in results.items():
        row = [model_name]
        for imputer_name, imputer_details in model_details.items():
            mean_accuracy = 0
            for year, metrics in imputer_details.items():
                mean_accuracy += metrics['Accuracy']
            mean_accuracy = mean_accuracy/len(imputer_details)
            row.append(mean_accuracy)
        rows.append(row)
    results_df = pd.DataFrame(data=rows, columns = column_headers)
    return results_df

model_ranking = perform_model_ranking(models_dictionary, imputed_oversampled_dataframes_dictionary, results)

    



