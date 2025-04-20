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