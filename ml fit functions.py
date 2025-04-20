import numpy as np
from sksurv.ensemble import RandomSurvivalForest
from sksurv.metrics import concordance_index_censored
from sksurv.util import Surv
from sklearn.model_selection import train_test_split
from sksurv.metrics import brier_score
from sksurv.ensemble import GradientBoostingSurvivalAnalysis

# function to fit random survival forest model at a landmark time
def fit_rsf(X_train, X_test, y_train, y_test, eval_time=2.5, random_state=42):
    # fit rsf
    rsf = RandomSurvivalForest(n_estimators=100, random_state=random_state)
    rsf.fit(X_train, y_train)

    # predict survival function and risk scores
    surv_funcs = rsf.predict_survival_function(X_test)
    risk_scores = rsf.predict(X_test)

    # c-index
    c_index = concordance_index_censored(y_test["event"], y_test["time"], risk_scores)[0]

    # brier
    times = np.array([eval_time])
    surv_probs = np.array([fn(eval_time) for fn in surv_funcs])
    bs = brier_score(y_train, y_test, surv_probs.reshape(-1, 1), times)[1][0]
    
    return c_index, bs, surv_probs, y_test
    

# function to fit gradient boosted survival trees at a landmark time

def fit_gbst(X_train, X_test, y_train, y_test, eval_time=2.5, random_state=42):

    # fit gbst
    gbst = GradientBoostingSurvivalAnalysis(n_estimators=100, random_state=random_state)
    gbst.fit(X_train, y_train)

    # predict survival function and risk scores
    surv_funcs = gbst.predict_survival_function(X_test)
    risk_scores = gbst.predict(X_test)
    
    # surviva probability
    surv_probs = np.array([fn(eval_time) for fn in surv_funcs])

    # c-index
    c_index = concordance_index_censored(y_test["event"], y_test["time"], 1 - surv_probs)[0]

    # brier
    times = np.array([eval_time])
    bs = brier_score(y_train, y_test, surv_probs.reshape(-1, 1), times)[1][0]

    return c_index, bs, surv_probs, y_test
