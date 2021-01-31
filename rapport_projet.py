#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Dec 19 13:51:37 2020

@author: teriitehau
"""

# Import des packages utiles au projet
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jan 30 09:01:57 2021

@author: teriitehau
"""

# Import des packages utiles au projet
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn import preprocessing as pp
from graphviz import Source
from itertools import cycle
import seaborn as sns
from matplotlib import pyplot
from numpy import sqrt,argmax

#evaluation métrique
from sklearn.metrics import roc_auc_score,f1_score,plot_confusion_matrix
from sklearn.metrics import recall_score, precision_score,accuracy_score
from sklearn.metrics import roc_curve, auc, confusion_matrix, classification_report
from sklearn.metrics import cohen_kappa_score,matthews_corrcoef,precision_recall_curve
from sklearn.dummy import DummyClassifier
    
from sklearn.datasets import make_classification
from matplotlib import pyplot

#classifieur 
#abre de decision 
from sklearn import tree
from sklearn.tree import DecisionTreeClassifier, export_graphviz
#graphique arbre de decision 
import graphviz as gv
import pylab

#XGBOOST
import xgboost as xgb
from xgboost import plot_importance

#Fichiers à importer, à scorer et définition du répertoire pour la sauvegarde des résultats

URL_ADULT ='/Users/teriitehau/Desktop/2020-2021/Méthode_de_scoring/fraude_mobile_phone_ech/fraude_mobile_phone_ech_train.csv'
URL_ADULT_test ='/Users/teriitehau/Desktop/2020-2021/Méthode_de_scoring/fraude_mobile_phone_ech/fraude_mobile_phone_ech_test.csv'


dir_res = r'C:\Tanguy\Enseignement\IAE Nantes Master EKAP\Cours Fraude Python\results'


# Import du fichier adult.csv
df_train = pd.read_csv(URL_ADULT, skipinitialspace=True)
df_test = pd.read_csv(URL_ADULT_test, skipinitialspace=True)
stats_desc_dftrain = df_train.describe() #Description des variables quanti
stats_desc_dftest = df_test.describe() #Description des variables quanti

#vue nature des variables
print(df_train.info(), "\n")
print(df_train.shape)




#dimension des bases 
print(df_train.shape,
df_test.shape )


# Calcul du nombre de données manquantes par variable
nb_null_by_vartrain = np.sum(pd.isnull(df_train))
print(nb_null_by_vartrain)
nb_null_by_var_test = np.sum(pd.isnull(df_test))
print(nb_null_by_var_test)

#aucune valeur manquante 



#column echantillon
df_train["echantillon"] = "train"
df_test["echantillon"] = "test"

frames= [df_train,df_test]
nrw_df = pd.concat(frames,axis=0)# 0 permet d'empiler par ligne

#régénerer index 
nrw_df = nrw_df.reset_index(drop=True)
 

#creation train et test_index (recup index des individus)
train_index = list(nrw_df[(nrw_df.echantillon=='train')].index)
train_index[:100]
test_index = list(nrw_df[(nrw_df.echantillon=='test')].index)
total = len(df_train)
total_amt = df_train.groupby(['isFraud'])['amount'].sum().sum()





#creation d'une variable par parse de namedest
df_train['dest']=0
df_test['dest']= 0

df_train['dest']= df_train['nameDest'].str.split('(\d.+)', n = 0, expand = True)
df_test['dest']= df_test['nameDest'].str.split('(\d.+)', n = 0, expand = True)





#Les ind qui ont newbalanceOrig zero car la majorité de la fraude provient d'eux
#quand le destinaire est un C
df_train['Orig_newbal_zero_destC'] = 0
df_train.loc[(df_train['newbalanceOrig']==0) &
            (df_train['dest']=='C'),'Orig_newbal_zero_destC']=1

df_test['Orig_newbal_zero_destC'] = 0
df_test.loc[(df_test['newbalanceOrig']==0) &
            (df_test['dest']=='C'),'Orig_newbal_zero_destC']=1


pd.crosstab(df_train['Orig_newbal_zero_destC'], df_train['isFraud'])




#creation base erreur 
df_train['Erreur_orig']= df_train['newbalanceOrig']+df_train['amount']-df_train['oldbalanceOrg']
df_test['Erreur_orig']= df_test['newbalanceOrig']+df_test['amount']-df_test['oldbalanceOrg']


df_train['Erreur_dest']= df_train['oldbalanceDest']+df_train['amount']-df_train['newbalanceDest']
df_test['Erreur_dest']= df_test['oldbalanceDest']+df_test['amount']-df_test['newbalanceDest']



########################################################
#############Graphique - analyse de données#############
########################################################
#https://github.com/DataCampM2DSSAF/suivi-du-data-camp-zeta-team
#Abraich, Bouchouat, NGUYEN, Tounsi
#graphique isFraud
ax = sns.boxplot(x="isFraud", y="amount", 
                data=df_train)
sns.plt.title("isFraud by amount",fontsize=12);

#fonction graphique pour amount 
def graphique0(base,col): 
    df_trans = base
    df_trans['TransactionAmt']= df_trans[col]


    plt.figure(figsize=(16,12))
    plt.suptitle('Transaction Values Distribution', fontsize=22)
    plt.subplot(221)
    g = sns.distplot(df_trans[df_trans['TransactionAmt'] <= 2.226441e+05]['TransactionAmt'])
    g.set_title("Transaction Amount Distribuition <= 2.226441e+05", fontsize=18)
    g.set_xlabel("")
    g.set_ylabel("Probability", fontsize=15)

    plt.subplot(222)
    g1 = sns.distplot(np.log(df_trans['TransactionAmt']))
    g1.set_title("Transaction Amount (Log) Distribuition", fontsize=18)
    g1.set_xlabel("")
    g1.set_ylabel("Probability", fontsize=15)

    plt.figure(figsize=(16,12))


    plt.subplot(221)
    g4 = plt.scatter(range(df_trans[df_trans['isFraud'] == 0].shape[0]),
                     np.sort(df_trans[df_trans['isFraud'] == 0]['TransactionAmt'].values), 
                     label='NoFraud', alpha=.2)
    g4 = plt.scatter(range(df_trans[df_trans['isFraud'] == 1].shape[0]),
                     np.sort(df_trans[df_trans['isFraud'] == 1]['TransactionAmt'].values), 
                     label='Fraud', alpha=.2)
    g4= plt.title("ECDF \nFRAUD and NO FRAUD Transaction Amount Distribution", fontsize=18)
    g4 = plt.xlabel("Index")
    g4 = plt.ylabel("Amount Distribution", fontsize=15)
    g4 = plt.legend()

    plt.subplot(222)
    g = sns.distplot(np.log(df_trans[df_trans['isFraud'] == 0]['TransactionAmt']), label='NoFraud')
    g = sns.distplot(np.log(df_trans[df_trans['isFraud'] == 1]['TransactionAmt']), label='Fraud')
    g= plt.title("Transaction Amount (Log) Distribuition by Fraud", fontsize=18)
    g = plt.xlabel("")
    g = plt.ylabel("Probability", fontsize=15)
    g = plt.legend()

    plt.figure(figsize=(16,12))

    plt.subplot(321)
    g = plt.scatter(range(df_trans[df_trans['isFraud'] == 1].shape[0]), 
                     np.sort(df_trans[df_trans['isFraud'] == 1]['TransactionAmt'].values), 
                    label='isFraud', alpha=.4, color = 'orange')
    plt.title("FRAUD - Transaction Amount ECDF", fontsize=18)
    plt.xlabel("Index")
    plt.ylabel("Amount Distribution", fontsize=12)

    plt.subplot(322)
    g1 = plt.scatter(range(df_trans[df_trans['isFraud'] == 0].shape[0]),
                     np.sort(df_trans[df_trans['isFraud'] == 0]['TransactionAmt'].values), 
                     label='NoFraud', alpha=.2)
    g1= plt.title("NO FRAUD - Transaction Amount ECDF", fontsize=18)
    g1 = plt.xlabel("Index")
    g1 = plt.ylabel("Amount Distribution", fontsize=15)

    plt.suptitle('Individual Distribution', fontsize=22)

    plt.show()

graphique0(df_train,'amount')

#graphique pour type 
def graphique_1(base,titre) :#fonction graphique 
    titre = titre
    tmp = pd.crosstab(base['type'], base['isFraud'], normalize='index') * 100
    tmp = tmp.reset_index()
    tmp.rename(columns={0:'NoFraud', 1:'Fraud'}, inplace=True)

    plt.figure(figsize=(14,10))
    plt.suptitle(titre, fontsize=22)

    plt.subplot(221)
    g = sns.countplot(x='type', data=base)
# plt.legend(title='Fraud', loc='upper center', labels=['No', 'Yes'])
    g.set_title("Type ", fontsize=19)
    g.set_ylim(0,420000)
    g.set_xlabel("Type Category Names", fontsize=17)
    g.set_ylabel("Count", fontsize=17)
    for p in g.patches:
        height = p.get_height()
        g.text(p.get_x()+p.get_width()/2.,
                height + 3,
                '{:1.2f}%'.format(height/total*100),
                ha="center",fontsize=14) 


    plt.subplot(222)
    g1 = sns.countplot(x='type', hue='isFraud', data=base)
    plt.legend(title='Fraud', loc='best', labels=['No', 'Yes'])
    gt = g1.twinx()
    gt = sns.pointplot(x='type', y='isFraud', data=base, 
                   color='black', legend=False)
    gt.set_ylabel("% of Fraud Transactions", fontsize=16)
    g1.set_title("Type by Target(isFraud)", fontsize=19)
    g1.set_xlabel("Type Category Names", fontsize=17)
    g1.set_ylabel("Count", fontsize=17)

    plt.subplot(212)
    g3 = sns.boxenplot(x='type', y='amount', hue='isFraud', 
              data=base )
    g3.set_title("Type Distribuition by amount and Target", fontsize=20)
    g3.set_xlabel("Type Category Names", fontsize=17)
    g3.set_ylabel("Type Values", fontsize=17)

    plt.subplots_adjust(hspace = 0.6, top = 0.85)

    plt.show()
    
graphique_1(df_train, 'type train')
graphique_1(df_test, 'type test')

#graphique par step 
def ploting_cnt_amt(df, col, lim=2000):
    tmp = pd.crosstab(df[col], df['isFraud'], normalize='index') * 100
    tmp = tmp.reset_index()
    tmp.rename(columns={0:'NoFraud', 1:'Fraud'}, inplace=True)
    
    plt.figure(figsize=(16,14))    
    plt.suptitle(f'{col} Distributions ', fontsize=24)
    
    plt.subplot(211)
    g = sns.countplot( x=col,  data=df, order=list(tmp[col].values))
    gt = g.twinx()
    gt = sns.pointplot(x=col, y='Fraud', data=tmp, order=list(tmp[col].values),
                       color='black', legend=False, )
    gt.set_ylim(0,tmp['Fraud'].max()*1.1)
    gt.set_ylabel("%Fraud Transactions", fontsize=16)
    g.set_title(f"Most Frequent {col} values and % Fraud Transactions", fontsize=20)
    g.set_xlabel(f"{col} Category Names", fontsize=16)
    g.set_ylabel("Count", fontsize=17)
    g.set_xticklabels(g.get_xticklabels(),rotation=45)
    sizes = []
    for p in g.patches:
        height = p.get_height()
        sizes.append(height)
        g.text(p.get_x()+p.get_width()/2.,
                height + 3,
                '{:1.2f}%'.format(height/total*100),
                ha="center",fontsize=12) 
        
    g.set_ylim(0,max(sizes)*1.15)
    
    #########################################################################
    perc_amt = (df.groupby(['isFraud',col])['amount'].sum() \
                / df.groupby([col])['amount'].sum() * 100).unstack('isFraud')
    perc_amt = perc_amt.reset_index()
    perc_amt.rename(columns={0:'NoFraud', 1:'Fraud'}, inplace=True)
    amt = df.groupby([col])['amount'].sum().reset_index()
    perc_amt = perc_amt.fillna(0)
    plt.subplot(212)
    g1 = sns.barplot(x=col, y='amount', 
                       data=amt, 
                       order=list(tmp[col].values))
    g1t = g1.twinx()
    g1t = sns.pointplot(x=col, y='Fraud', data=perc_amt, 
                        order=list(tmp[col].values),
                       color='black', legend=False, )
    g1t.set_ylim(0,perc_amt['Fraud'].max()*1.1)
    g1t.set_ylabel("%Fraud Total Amount", fontsize=16)
    g.set_xticklabels(g.get_xticklabels(),rotation=45)
    g1.set_title(f"{col} by amount Total + %of total and %Fraud Transactions", fontsize=20)
    g1.set_xlabel(f"{col} Category Names", fontsize=16)
    g1.set_ylabel("Transaction Total Amount", fontsize=16)
    g1.set_xticklabels(g.get_xticklabels(),rotation=45)    
    
    for p in g1.patches:
        height = p.get_height()
        g1.text(p.get_x()+p.get_width()/2.,
                height + 3,
                '{:1.2f}%'.format(height/total_amt*100),
                ha="center",fontsize=12) 
        
    plt.subplots_adjust(hspace=.4, top = 0.9)
    plt.show()
ploting_cnt_amt(df_train, 'step')
ploting_cnt_amt(df_test, 'step')


#variable dummies 
dummy = pd.get_dummies(df_train['type'])
df_train = pd.concat([df_train, dummy], axis=1)

dummy = pd.get_dummies(df_train['dest'])
df_train = pd.concat([df_train, dummy], axis=1)


dummy = pd.get_dummies(df_test['type'])
df_test = pd.concat([df_test, dummy], axis=1)

dummy = pd.get_dummies(df_test['dest'])
df_test = pd.concat([df_test, dummy], axis=1)





#var qualitative
var_quali = ['CASH_OUT', 'TRANSFER','C']
df_train[var_quali] = df_train[var_quali].astype(int)
df_test[var_quali] = df_test[var_quali].astype(int)
#doit etre en int car le Xboost ne va pas fonctionner


#var_expli = ['step', 'amount','oldbalanceOrg','newbalanceOrig','oldbalanceDest','newbalanceDest',
#            'old_compte_zero_dest','new_compte_zero_dest','OrigC_zero_dest','Orig_newbal_zero_destC',
#            'CASH_OUT','TRANSFER']
var_expli = ['step','amount','oldbalanceOrg','newbalanceOrig','oldbalanceDest','newbalanceDest',
            'CASH_OUT','TRANSFER','C','Erreur_orig','Erreur_dest']

target = ['isFraud']

X_train1= df_train[var_expli]
Y_train1= df_train[target]
X_test1 = df_test[var_expli]
Y_test1= df_test[target]



#####################ajouter l'ACP si nécessaire





#fonction d'evaluation metrique 
def perf(train_y,train_x,valid_y,valid_x,clf):
    print("Train F1-score : {}".format(f1_score(train_y, clf.predict(train_x).round(), average='macro')))
    print("Valid F1-score : {}".format(f1_score(valid_y, clf.predict(valid_x).round(), average='macro')))
    
    print("score AUC train : {}".format(roc_auc_score(train_y, clf.predict(train_x).round())))
    print("score AUC test : {}".format(roc_auc_score(valid_y, clf.predict(valid_x).round())))
    
    print("score Accuracy train : {}".format(accuracy_score(train_y, clf.predict(train_x).round())))
    print("score Accuracy test : {}".format(accuracy_score(valid_y, clf.predict(valid_x).round())))
    
    print("score Recall train : {}".format(recall_score(train_y, clf.predict(train_x).round())))
    print("score Recall test : {}".format(recall_score(valid_y, clf.predict(valid_x).round())))
    
    print("score Precision train : {}".format(precision_score(train_y, clf.predict(train_x).round())))
    print("score Precision test : {}".format(precision_score(valid_y, clf.predict(valid_x).round())))
    
    print("score Cohen train : {}".format(cohen_kappa_score(train_y, clf.predict(train_x).round())))
    print("score Cohen test : {}".format(cohen_kappa_score(valid_y, clf.predict(valid_x).round())))
    
    print(" matthews_corrcoef train : {}".format(matthews_corrcoef(train_y, clf.predict(train_x).round())))
    print("matthews_corrcoef Cohen test : {}".format(matthews_corrcoef(valid_y, clf.predict(valid_x).round())))
    


#Decision Tree
clf_tree = tree.DecisionTreeClassifier()
clf_tree.fit(X_train1, Y_train1)
perf(Y_train1,X_train1,Y_test1,X_test1,clf_tree)
# DOT data
#dot_data = tree.export_graphviz(clf_tree, out_file=None, 
#                                filled=True)

# Draw graph
#graph = graphviz.Source(dot_data, format="png") 
#graph
plot_confusion_matrix(clf_tree, X_test1, Y_test1) 

#courbe ROC_AUC ET PR 
#https://machinelearningmastery.com/roc-curves-and-precision-recall-curves-for-imbalanced-classification/
ns_probs = [0 for _ in range(len(Y_test1))]
# fit a model
model = clf_tree
model.fit(X_train1, Y_train1)
# predict probabilities
lr_probs = model.predict_proba(X_test1)
# keep probabilities for the positive outcome only
lr_probs = lr_probs[:, 1]
# calculate scores
ns_auc = roc_auc_score(Y_test1, ns_probs)
lr_auc = roc_auc_score(Y_test1, lr_probs)
# summarize scores
print('No Skill: ROC AUC=%.3f' % (ns_auc))
print('DecisionTree: ROC AUC=%.3f' % (lr_auc))
# calculate roc curves
ns_fpr, ns_tpr, _ = roc_curve(Y_test1, ns_probs)
lr_fpr, lr_tpr, _ = roc_curve(Y_test1, lr_probs)
# plot the roc curve for the model
pyplot.plot(ns_fpr, ns_tpr, linestyle='--', label='No Skill')
pyplot.plot(lr_fpr, lr_tpr, marker='.', label='DecisionTree')
# axis labels
pyplot.xlabel('False Positive Rate')
pyplot.ylabel('True Positive Rate')
# show the legend
pyplot.legend()
# show the plot
pyplot.show()


#roc-PR
#https://machinelearningmastery.com/roc-curves-and-precision-recall-curves-for-imbalanced-classification/
# no skill model, stratified random class predictions
 
# plot no skill and model precision-recall curves
def plot_pr_curve(test_y, model_probs,name):
	# calculate the no skill line as the proportion of the positive class
	no_skill = len(test_y[test_y==1]) / len(test_y)
	# plot the no skill precision-recall curve
	pyplot.plot([0, 1], [no_skill, no_skill], linestyle='--', label='No Skill')
	# plot model precision-recall curve
	precision, recall, _ = precision_recall_curve(testy, model_probs)
	pyplot.plot(recall, precision, marker='.', label=name)
	# axis labels
	pyplot.xlabel('Recall')
	pyplot.ylabel('Precision')
	# show the legend
	pyplot.legend()
	# show the plot
	pyplot.show()

trainX, testX, trainy, testy = X_train1, X_test1, Y_train1, Y_test1
model = DummyClassifier(strategy='stratified')
model.fit(trainX, trainy)
yhat = model.predict_proba(testX)
naive_probs = yhat[:, 1]
# calculate the precision-recall auc
precision, recall, _ = precision_recall_curve(testy, naive_probs)
auc_score = auc(recall, precision)
print('No Skill PR AUC: %.3f' % auc_score)
# fit a model
model = clf_tree
model.fit(trainX, trainy)
yhat = model.predict_proba(testX)
model_probs = yhat[:, 1]
# calculate the precision-recall auc
precision, recall, _ = precision_recall_curve(testy, model_probs)
auc_score = auc(recall, precision)
print('decision_tree PR AUC: %.3f' % auc_score)
# plot precision-recall curves
plot_pr_curve(testy, model_probs,'decision_tree')



#Xgboost 
boost = xgb.XGBClassifier()
boost.fit(X_train1, Y_train1)
perf(Y_train1,X_train1,Y_test1,X_test1,boost)

plt.figure(figsize=(40,20))
plot_importance(boost ,max_num_features=100)
plt.show()

plot_confusion_matrix(boost, X_test1, Y_test1) 

#coubre ROC_AUC ET ROC_PR 
# predict probabilities
#https://machinelearningmastery.com/roc-curves-and-precision-recall-curves-for-imbalanced-classification/

model = boost
model.fit(X_train1, Y_train1)
yhat = model.predict_proba(X_test1)
# keep probabilities for the positive outcome only
yhat = yhat[:, 1]
# calculate roc curves
fpr, tpr, thresholds = roc_curve(Y_test1, yhat)
# calculate the g-mean for each threshold
gmeans = sqrt(tpr * (1-fpr))
# locate the index of the largest g-mean
ix = argmax(gmeans)
print('Best Threshold=%f, G-Mean=%.3f' % (thresholds[ix], gmeans[ix]))
# plot the roc curve for the model
pyplot.plot([0,1], [0,1], linestyle='--', label='No Skill')
pyplot.plot(fpr, tpr, marker='.', label='Xgboost')
pyplot.scatter(fpr[ix], tpr[ix], marker='o', color='black', label='Best')
# axis labels
pyplot.xlabel('False Positive Rate')
pyplot.ylabel('True Positive Rate')
pyplot.legend()
# show the plot
pyplot.show()

#ROC_PR
model = DummyClassifier(strategy='stratified')
model.fit(trainX, trainy)
yhat = model.predict_proba(testX)
naive_probs = yhat[:, 1]
# calculate the precision-recall auc
precision, recall, _ = precision_recall_curve(testy, naive_probs)
auc_score = auc(recall, precision)
print('No Skill PR AUC: %.3f' % auc_score)
# fit a model
model = boost
model.fit(trainX, trainy)
yhat = model.predict_proba(testX)
model_probs = yhat[:, 1]
# calculate the precision-recall auc
precision, recall, _ = precision_recall_curve(testy, model_probs)
auc_score = auc(recall, precision)
print('Xgboost PR AUC: %.3f' % auc_score)
# plot precision-recall curves
plot_pr_curve(testy, model_probs,'xgboost')



#identification des point mal classee
y_prob =boost.predict_proba(X_test1)
y_pred =boost.predict(X_test1)
y_pred = pd.DataFrame(y_pred)
y_prob = pd.DataFrame(y_prob)
df_test['Proba_boost']= y_prob[1]
matrice = confusion_matrix(Y_test1,y_pred)
print(matrice)

df_test.loc[(y_pred[0]!=Y_test1['isFraud']),['step','type','amount',
                                            'oldbalanceOrg','newbalanceOrig',
                                            'oldbalanceDest','newbalanceDest','isFraud',
                                            'Erreur_orig','Erreur_dest','Proba_boost']]



df_test.sort_values(by=['Proba_boost'], ascending = False).head(10)



#################################################
#############apprentissaage par step#############
#################################################
#modèle 1
mois1_train = X_train1.loc[(df_train['step']==7) | (df_train['step']==8)|(df_train['step']==9)|
            (df_train['step']==10)|(df_train['step']==11)|(df_train['step']==12)]
mois1_Ytrain=Y_train1.loc[(df_train['step']==7) | (df_train['step']==8)|(df_train['step']==9)|
            (df_train['step']==10)|(df_train['step']==11)|(df_train['step']==12)]

mois1_test = X_train1.loc[df_train['step']==16]
mois1_Ytest=Y_train1.loc[df_train['step']==16]

mois1_Xtrain= mois1_train[var_expli]
mois1_Xtest = mois1_test[var_expli]
boost.fit(mois1_Xtrain, mois1_Ytrain)

perf(mois1_Ytrain,mois1_Xtrain,mois1_Ytest,mois1_Xtest,boost)


#modele 2
mois1_train = X_train1.loc[(df_train['step']==8) | (df_train['step']==9)|(df_train['step']==10)|
            (df_train['step']==11)|(df_train['step']==12)|(df_train['step']==13)]
mois1_Ytrain=Y_train1.loc[(df_train['step']==8) | (df_train['step']==9)|(df_train['step']==10)|
            (df_train['step']==11)|(df_train['step']==12)|(df_train['step']==13)]
mois1_test = X_train1.loc[df_train['step']==17]
mois1_Ytest=Y_train1.loc[df_train['step']==17]
mois1_Xtrain= mois1_train[var_expli]
mois1_Xtest = mois1_test[var_expli]
boost.fit(mois1_Xtrain, mois1_Ytrain)
perf(mois1_Ytrain,mois1_Xtrain,mois1_Ytest,mois1_Xtest,boost)

#modèle 3
mois1_train = X_train1.loc[(df_train['step']==9) | (df_train['step']==10)|(df_train['step']==11)|
            (df_train['step']==12)|(df_train['step']==13)|(df_train['step']==14)]
mois1_Ytrain=Y_train1.loc[(df_train['step']==9) | (df_train['step']==10)|(df_train['step']==11)|
            (df_train['step']==12)|(df_train['step']==13)|(df_train['step']==14)]
mois1_test = X_train1.loc[df_train['step']==18]
mois1_Ytest=Y_train1.loc[df_train['step']==18]
mois1_Xtrain= mois1_train[var_expli]
mois1_Xtest = mois1_test[var_expli]
boost.fit(mois1_Xtrain, mois1_Ytrain)
perf(mois1_Ytrain,mois1_Xtrain,mois1_Ytest,mois1_Xtest,boost)


#modèle 4
mois1_train = X_train1.loc[(df_train['step']==10) | (df_train['step']==11)|(df_train['step']==12)|
            (df_train['step']==13)|(df_train['step']==14)|(df_train['step']==15)]
mois1_Ytrain=Y_train1.loc[(df_train['step']==10) | (df_train['step']==11)|(df_train['step']==12)|
            (df_train['step']==13)|(df_train['step']==14)|(df_train['step']==15)]

mois1_test = X_train1.loc[df_train['step']==19]
mois1_Ytest=Y_train1.loc[df_train['step']==19]

mois1_Xtrain= mois1_train[var_expli]
mois1_Xtest = mois1_test[var_expli]
boost.fit(mois1_Xtrain, mois1_Ytrain)

perf(mois1_Ytrain,mois1_Xtrain,mois1_Ytest,mois1_Xtest,boost)



#realiser une boucle sur tout les step de train
perf = pd.DataFrame(index=['false_no_fraud', 'true_no_fraud', 'false_fraud','true_fraud'
                          ,'false_expect'])
for i in range(0,20):
    mois1_Xtest = X_train1.loc[df_train['step']==i+1]
    mois1_Ytest=Y_train1.loc[df_train['step']==i+1]
    y_pred = boost.predict(mois1_Xtest)
    test1 = confusion_matrix(mois1_Ytest,y_pred.round())
    perf.loc['false_no_fraud',i+1]=test1[1,0]
    perf.loc['true_no_fraud',i+1] =test1[0,0]
    perf.loc['false_fraud',i+1]=test1[0,1]
    perf.loc['true_fraud',i+1] =test1[1,1]
    perf.loc['false_expect',i+1] =test1[0,1]+test1[1,0]
perf.T


#realiser une boucle sur tout les step de test
perf = pd.DataFrame(index=['false_no_fraud', 'true_no_fraud', 'false_fraud','true_fraud'
                          ,'false_expect','F1_SCORE'])

for i in range(0,7):
    mois1_Xtest= X_test1.loc[df_test['step']==i+21]
    mois1_Ytest=Y_test1.loc[df_test['step']==i+21]
    y_pred = boost.predict(mois1_Xtest)
    test2 = confusion_matrix(mois1_Ytest,y_pred.round())
    perf.loc['false_no_fraud',i+21]=test2[1,0]
    perf.loc['true_no_fraud',i+21] =test2[0,0]
    perf.loc['false_fraud',i+21]=test2[0,1]
    perf.loc['true_fraud',i+21] =test2[1,1]
    perf.loc['false_expect',i+21] =test2[0,1]+test2[1,0]
    perf.loc['F1_SCORE',i+21] =f1_score(mois1_Ytest,y_pred.round(),average='binary')
perf.T


for i in range(7,12):
    mois1_Xtest= X_test1.loc[df_test['step']==i+21]
    mois1_Ytest=Y_test1.loc[df_test['step']==i+21]
    y_pred = boost.predict(mois1_Xtest)
    test2 = confusion_matrix(mois1_Ytest,y_pred.round())
    perf.loc['false_no_fraud',i+21]=0
    perf.loc['true_no_fraud',i+21] =0
    perf.loc['false_fraud',i+21]=0
    perf.loc['true_fraud',i+21] =test2
    perf.loc['false_expect',i+21] =0
    perf.loc['F1_SCORE',i+21] =f1_score(mois1_Ytest,y_pred.round(),average='binary')
perf.T


for i in range(12,19):
    mois1_Xtest= X_test1.loc[df_test['step']==i+21]
    mois1_Ytest=Y_test1.loc[df_test['step']==i+21]
    y_pred = boost.predict(mois1_Xtest)
    test2 = confusion_matrix(mois1_Ytest,y_pred.round())
    perf.loc['false_no_fraud',i+21]=test2[1,0]
    perf.loc['true_no_fraud',i+21] =test2[0,0]
    perf.loc['false_fraud',i+21]=test2[0,1]
    perf.loc['true_fraud',i+21] =test2[1,1]
    perf.loc['false_expect',i+21] =test2[0,1]+test2[1,0]
    perf.loc['F1_SCORE',i+21] =f1_score(mois1_Ytest,y_pred.round(),average='binary')
perf.T











