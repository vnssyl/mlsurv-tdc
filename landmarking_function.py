import pandas as pd
from sksurv.util import Surv
from sklearn.model_selection import train_test_split

def prepare_landmark_data(df, landmark_time):
    df_lm = df[df["time"] <= landmark_time].copy()
    df_lm = df_lm.sort_values(by=["ID", "time"])

    df_snapshot = df_lm.groupby("ID").tail(1).reset_index(drop=True)
    
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
    
    return train_test_split(X, y, test_size=0.3, random_state=42), df_snapshot