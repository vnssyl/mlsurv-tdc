import pandas as pd

def prepare_landmark_data(df, landmark_time):
    df_lm = df[df["time"] <= landmark_time]

    df_snapshot = df_lm.groupby("ID").apply(lambda x: x[x["time"] <= landmark_time].iloc[-1], include_groups = False)
    df_snapshot = df_snapshot.reset_index(drop=True)
    
    df_snapshot["landmark_time"] = landmark_time
    df_snapshot["time_from_lm"] = df_snapshot["final_time"] - landmark_time
    df_snapshot = df_snapshot[df_snapshot["time_from_lm"] > 0] 
    
    drop_cols = [
        "start", "stop", "event_flag", "final_time", "landmark_time",
        "time_from_lm", "event", "time", "id_noise", "noise_component", "linpred", "hazard"
    ]
    X = df_snapshot.drop(columns=drop_cols, errors="ignore")
    y = Surv.from_arrays(
        event=df_snapshot["event"].astype(bool),
        time=df_snapshot["time_from_lm"]
    )
    
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.3, random_state=42
    )
    return X_train, X_test, y_train, y_test, df_snapshot