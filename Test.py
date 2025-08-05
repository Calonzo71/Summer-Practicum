#%% Packages
import pandas as pd
from pathlib import Path
from plotnine import *
#%%
df = pd.read_csv(r"C:\Users\jcalonzo\Desktop\Carlos\T_ONTIME_MARKETING8.csv")
#%%
df.shape

#%%

# Load the 12 CSVs
base_dir = Path(r"C:\Users\jcalonzo\Desktop\Carlos")

all_data = []  
                      # collect each month here
#target_cols = ["FlightDate","Origin","Dest","DepDel15","ArrDel15","Cancelled","Diverted","Operating_Airline"]
for i in range(1, 13):                
    file_path = base_dir / f"T_ONTIME_MARKETING{i}.csv"
    # Read the CSV
    df = pd.read_csv(file_path)
    
    # Track which file/month the rows came from
    #df["Month_File"] = i
    
    all_data.append(df)

"""
for i in range(1, 2):                
    file_path = base_dir / f"On_Time_Marketing_Carrier_On_Time_Performance_(Beginning_January_2018)_{i}.csv"
    # Read the CSV
    df = pd.read_csv(file_path)
    
    # Track which file/month the rows came from
    #df["Month_File"] = i
    
    all_data.append(df)
"""

# Combine every month into one DataFrame
combined_df = pd.concat(all_data, ignore_index=True)
#%%
combined_df['OP_UNIQUE_CARRIER'].value_counts()

airports_list = ['LAX', 'ATL', 'DFW', 'DEN', 'ORD', 'CLT',
                 'MCO', 'LAS', 'PHX', 'MIA']
airline_List = ['DL','AA','UA','WN','AS','B6','NK','F9','G4','HA']

combined_df2 = combined_df.loc[
    (combined_df['DEST'].isin(airports_list)) &  
    (combined_df['OP_UNIQUE_CARRIER'].isin(airline_List))      
]
combined_df.dropna()
print(combined_df2.shape)
#%%

combined_df2 = combined_df2[combined_df2['CANCELLED']==0]
combined_df2 = combined_df2[combined_df2['DIVERTED']==0]
combined_df2 = combined_df2[~combined_df2['FL_DATE'].str.contains("4/1/2023")]
print(combined_df2.shape)
#%%
combined_df2['SCHEDULED_HOUR'] = combined_df2['CRS_DEP_TIME'] // 100

#0000-0359
T1Conditions = (combined_df2['SCHEDULED_HOUR'] < 4) #| (delayedAirlines['SCHEDULED_HOUR'] >= 24 & (delayedAirlines['SCHEDULED_HOUR'] < 28))
combined_df2.loc[T1Conditions, 'TIME_OF_DAY'] = 1

#0400-0759
T2Conditions = ((combined_df2['SCHEDULED_HOUR'] >= 4) & (combined_df2['SCHEDULED_HOUR'] < 8)) #| (delayedAirlines['SCHEDULED_HOUR'] >= 28 & (delayedAirlines['SCHEDULED_HOUR'] < 32))
combined_df2.loc[T2Conditions, 'TIME_OF_DAY'] = 2

#0800-1159
T3Conditions = ((combined_df2['SCHEDULED_HOUR'] >= 8) & (combined_df2['SCHEDULED_HOUR'] < 12)) #| (delayedAirlines['SCHEDULED_HOUR'] >= 32 & (delayedAirlines['SCHEDULED_HOUR'] < 36))
combined_df2.loc[T3Conditions, 'TIME_OF_DAY'] = 3

#1200-1559
T4Conditions = ((combined_df2['SCHEDULED_HOUR'] >= 12) & (combined_df2['SCHEDULED_HOUR'] < 16)) #| (delayedAirlines['SCHEDULED_HOUR'] >= 36 & (delayedAirlines['SCHEDULED_HOUR'] < 40))
combined_df2.loc[T4Conditions, 'TIME_OF_DAY'] = 4

#1600-1959
T5Conditions = ((combined_df2['SCHEDULED_HOUR'] >= 16) & (combined_df2['SCHEDULED_HOUR'] < 20)) #| (delayedAirlines['SCHEDULED_HOUR'] >= 40.0 & (delayedAirlines['SCHEDULED_HOUR'] < 44))
combined_df2.loc[T5Conditions, 'TIME_OF_DAY'] = 5

#2000-2359
T6Conditions = ((combined_df2['SCHEDULED_HOUR'] >= 20) & (combined_df2['SCHEDULED_HOUR'] < 24)) #| (delayedAirlines['SCHEDULED_HOUR'] >= 44 & (delayedAirlines['SCHEDULED_HOUR'] < 48))
combined_df2.loc[T6Conditions, 'TIME_OF_DAY'] = 6
#%%
combined_df2['TIME_OF_DAY'].value_counts()
#%%

#combined_df2['FL_DATE'].str.contains("4/1/2023").value_counts()
#combined_df2['OP_UNIQUE_CARRIER'].value_counts()
print(combined_df2.shape)
combined_df2.to_csv(r"C:\users\jcalonzo\Desktop\Carlos\data_to_predict.csv", index=False)


#%%
master_df = pd.read_csv(r"C:\users\jcalonzo\Desktop\Carlos\group_dataset.csv")
master_df = master_df.drop('Unnamed: 119',axis= 1)
combined_df2 = master_df
#%%
combined_df2 = pd.read_csv(r"C:\users\jcalonzo\Desktop\Carlos\groupdata_filter")
#combined_df2 = combined_df2.drop('Unnamed: 119',axis= 1)
combined_df2 
#%%
combined_df2
#%%
#print(combined_df2['DepDel15'][combined_df2['SecurityDelay'] >= 20].value_counts())

#Timeblocks (Categorical), operating airline (Categorical), origin(categorical), *distance(Numerical)

#%%
print(combined_df2.shape)
print(combined_df2['Origin'].value_counts())
#%% Subset for top 10 US airports
airports_list = ['LAX', 'ATL', 'DFW', 'DEN', 'ORD', 'CLT',
                 'MCO', 'LAS', 'PHX', 'MIA']
airline_List = ['DL','AA','UA','WN','AS','B6','NK','F9','G4','HA']

combined_df2 = combined_df.loc[
    (combined_df['Origin'].isin(airports_list)) &  
    (combined_df['Operating_Airline'].isin(airline_List))      
]
print(combined_df2.shape)

combined_df2.to_csv(r"C:\users\jcalonzo\Desktop\Carlos\data_to_predict.csv", index=False)

#%% Creating new columns

combined_df2["total_delay_Mins"] = combined_df2["DepDelayMinutes"] + combined_df2["ArrDelayMinutes"]
#combined_df2["Total_Ground_Idle"]= combined_df2["TaxiOut"] + combined_df2["TaxiIn"]
combined_df2["ops_disruptions"] = combined_df2["Cancelled"]+combined_df2["Diverted"]
combined_df2["Flight_Delayed"] = (combined_df2["total_delay_Mins"] > 0).astype(int)

#%% Summary
#combined_df2.isnull().sum()
#print(combined_df2["Flight_Delayed"].value_counts())
Flights_Top10 = 1672943 #Num of flights between top 10 aiports & airlines
Total_Flights = 7065617 

Per_Cancels_Diversions = (combined_df2["ops_disruptions"].sum()/Flights_Top10)
Per_Flights_Delayed = (combined_df2["DepDel15"].sum()/Flights_Top10)
print(f"Pecent of Cancels and Diversions: {Per_Cancels_Diversions:.2%}")
print(f"Pecent of flights delayed: {Per_Flights_Delayed:.2%}") #count in only DepDel15 min column (DeptimeBlk shows time of day)
print(f"Pecent of flights disruptions: {Per_Flights_Delayed + Per_Cancels_Diversions :.2%}")
print(f"Pecent of flights for top 10 airports: {Flights_Top10/Total_Flights :.2%}")

#%% Origin of volume & and destination of volume
print(combined_df2["Origin"].value_counts())
print(combined_df2["Dest"].value_counts())
"""
-Counts remained consistent
- LAX best in west coast, DEN best in midwest, ATL best in east, 
"""
#%% Delayed flights by origin and airline
delayed_counts_Origin = (combined_df2[combined_df2['DepDel15'] == 1]   
                    .groupby('Origin')
                    .size())
print(f'flight delays {delayed_counts_Origin.sort_values()}')
delayed_counts_Airlines = (combined_df2[combined_df2['DepDel15'] == 1]   
                    .groupby(combined_df2['Operating_Airline'])
                    .size())
print(f'flight delays {delayed_counts_Airlines.sort_values()}')

#%% Imports
from sklearn.model_selection import train_test_split
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    classification_report,
    confusion_matrix,
    roc_auc_score,
    RocCurveDisplay
)
import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm

#%% 1. Build the working DataFrame
Log_df = combined_df2[['CarrierDelay', 'WeatherDelay', 'NASDelay',
                       'SecurityDelay', 'LateAircraftDelay',
                       'DepDel15', 'Origin', 'Dest']].copy()

# 2. Basic cleaning
numeric_delay_cols = ['CarrierDelay', 'WeatherDelay', 'NASDelay',
                      'SecurityDelay', 'LateAircraftDelay']

Log_df[numeric_delay_cols] = Log_df[numeric_delay_cols].fillna(0)
Log_df = Log_df.dropna(subset=['DepDel15'])

# 3. Define X (features) and y (target)
X = Log_df.drop(columns='DepDel15')
y = Log_df['DepDel15'].astype(int)   # ensure 0/1 ints

#%% 4. Train/test split
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.25, stratify=y, random_state=42
)
#%% 5. Pre‑processing pipeline
numeric_features = numeric_delay_cols
categorical_features = ['Origin', 'Dest']

preprocess = ColumnTransformer(
    transformers=[
        ('num',  StandardScaler(), numeric_features), #Scale numeric features
        ('cat',  OneHotEncoder(handle_unknown='ignore'), categorical_features)
    ] # One‑hot encode Origin and Dest
)
#%% 6. Full modeling pipeline
clf = Pipeline(steps=[
    ('preprocess', preprocess),
    ('model', LogisticRegression(
        max_iter=1000,
        solver='lbfgs',
        class_weight='balanced'       # handles class imbalance, if any
    ))
])
# 7. Fit the model
clf.fit(X_train, y_train)
clf.summary
#%% 8. Evaluate
y_pred  = clf.predict(X_test)
y_proba = clf.predict_proba(X_test)[:, 1]

print("\nClassification report:")
print(classification_report(y_test, y_pred, digits=3))

print("Confusion matrix:\n", confusion_matrix(y_test, y_pred))

auc = roc_auc_score(y_test, y_proba)
print(f"AUC = {auc:.3f}")

# Optional: ROC curve
RocCurveDisplay.from_predictions(y_test, y_proba)
plt.title("ROC curve – Logistic Regression (DepDel15)")
plt.tight_layout()
plt.show()

#The model gives higher predicted probabilities to true positives than to true negatives
#%% 1. subset df created here
Log_df = combined_df2[['CarrierDelay', 'WeatherDelay', 'NASDelay',
                       'SecurityDelay', 'LateAircraftDelay',
                       'DepDel15', 'Origin']].copy()
#%%
Log_df[['DepDel15','CarrierDelay''WeatherDelay', 'NASDelay','SecurityDelay', 'LateAircraftDelay']]
#%%

# 2. getting rid of nan values
numeric_delay_cols = ['CarrierDelay', 'WeatherDelay', 'NASDelay',
                      'SecurityDelay', 'LateAircraftDelay']

Log_df[numeric_delay_cols] = Log_df[numeric_delay_cols].fillna(0)
Log_df = Log_df.dropna(subset=['DepDel15'])
#%% Creating dummies
X = pd.get_dummies(
        Log_df.drop(columns='DepDel15'),
        columns=['Origin'],
        drop_first=True
    )

#scaler = StandardScaler()
#X[numeric_delay_cols] = scaler.fit_transform(X[numeric_delay_cols])

y = Log_df['DepDel15'].astype(int)
print(y.head())
print(X.head())
#%% 4. Train/test split
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.25, stratify=y, random_state=42
)
#%% 5. Add intercept term by adding col of 1
X_train_const = sm.add_constant(X_train, has_constant='add')
X_test_const  = sm.add_constant(X_test,  has_constant='add')
# After you already split into train/test and added the constant
X_train_const = X_train_const.astype(float)  
X_test_const  = X_test_const.astype(float)
#%%

X_test_const.to_csv(r"C:\users\jcalonzo\Desktop\Carlos\X_test_const.csv", index=False)
X_train_const.to_csv(r"C:\users\jcalonzo\Desktop\Carlos\X_train_const.csv", index=False)
y_train.to_csv(r"C:\users\jcalonzo\Desktop\Carlos\y_train.csv", index=False)
y_test.to_csv(r"C:\users\jcalonzo\Desktop\Carlos\y_test.csv", index=False)



#np.asarray(X_train_const)
#%% 6. Fit statsmodels Logit
logit_sm = sm.Logit(y_train, X_train_const)
result   = logit_sm.fit(method='lbfgs', maxiter=1000)

print(result.summary())    
#%%
print("AIC:", result.aic)
print("BIC:", result.bic)
#%% 7. Evaluate on hold‑out set
y_proba = result.predict(X_test_const)
y_pred  = (y_proba >= 0.50).astype(int)

print("\nClassification report:")
print(classification_report(y_test, y_pred, digits=3))

print("Confusion matrix:\n", confusion_matrix(y_test, y_pred))

auc = roc_auc_score(y_test, y_proba)
print(f"AUC = {auc:.3f}")

RocCurveDisplay.from_predictions(y_test, y_proba)
plt.title("ROC curve – Logit (DepDel15)")
plt.tight_layout()
plt.show()
#The model gives higher predicted probabilities to true positives than to true negatives


#%%
import matplotlib.pyplot as plt
import seaborn as sns

sample_idx = y_train.sample(n=400, random_state=42).index

fig, (axL, axR) = plt.subplots(2, figsize=(15, 15))
plt.suptitle("Logistic Regression Residual Plots \n using Seaborn Lowess line (N = 400)")

# Deviance Residuals
sns.regplot(
    x=result.fittedvalues.loc[sample_idx],
    y=result.resid_dev.loc[sample_idx],
    ax=axL,
    color="black",
    scatter_kws={"s": 5},
    line_kws={"color": "b", "alpha": 1, "lw": 2},
    lowess=True
)
axL.set_title("Deviance Residuals \n against Fitted Values")
axL.set_xlabel("Linear Predictor Values")
axL.set_ylabel("Deviance Residuals")

# Studentized Pearson Residuals
sns.regplot(
    x=result.fittedvalues.loc[sample_idx],
    y=result.resid_pearson.loc[sample_idx],
    ax=axR,
    color="black",
    scatter_kws={"s": 5},
    line_kws={"color": "g", "alpha": 1, "lw": 2},
    lowess=True
)
axR.set_title("Studentized Pearson Residuals \n against Fitted Values")
axR.set_xlabel("Linear Predictor Values")
axR.set_ylabel("Studentized Pearson Residuals")

plt.show()
#%%
"""
Next: 
-run different types of model
- Log likelyhood ratio test to compare models (LR = -2*(LLsmall - LLfull))



"""

#%%

# 1. Predicted probabilities
mu = result.predict(X_train_const)   
      # fitted probabilities
residual = y_train - mu
#%%

resid_pearson = (y_train - mu)

# 3. Leverage (diagonal of hat matrix)
leverage = influence.hat_matrix_diag

# 4. Standardized Pearson residuals
std_pearson = resid_pearson / np.sqrt(1 - leverage)

# --- Q–Q plot ---
sm.qqplot(residual, line='45', fit=True)
plt.title("Normal Q–Q Plot of Standardized Pearson Residuals")
plt.xlabel("Theoretical Quantiles")
plt.ylabel("Standardized Pearson Residuals")
plt.tight_layout()
plt.show()







#%%

p = ggplot(combined_df2,aes(x ="total_delay_Mins" )) + geom_bar(fill = "blue",bin = 10) + xlim(0, 3000) + ylim(0,10000)
p.show()
#df.max , df.min for xlim

#%%IDEAS 
"""
*subset by top 10 airlines & origin 
- forecast 2023 data (maybe: disruptions, volume of flights??)

"""

#%%


np.corrcoef()

#%%

"""
Outliers (Var to look at): 
- Delays & all continous data
- Cate. origin, depdelay
Models:
 - Clustering
 - random forest model
 
"""



