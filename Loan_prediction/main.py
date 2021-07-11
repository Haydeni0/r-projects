import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt
plt.style.use("ggplot")

data = pd.read_csv("Train.csv")

# data

encode = LabelEncoder()
data.Loan_Status = encode.fit_transform(data.Loan_Status)

data.dropna(inplace = True)

data

pd.plotting.scatter_matrix(data, c = data.Loan_Status)

plt.show()
