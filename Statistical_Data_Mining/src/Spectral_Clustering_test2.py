import os
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import SpectralClustering
from sklearn.preprocessing import StandardScaler, normalize
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_score

# Changing the working location to the location of the data
# cd "/Users/thomasserafin/Serafin_Documents/Cornell/Spring_2020/ORIE_4740_Statistical_Data_Mining/Project/ORIE4710_FINAL-master" # Tom, change to yours when you download
os.chdir(
    "/Users/thomasserafin/Serafin_Documents/Cornell/Spring_2020/ORIE_4740_Statistical_Data_Mining/Project/ORIE4710_FINAL-master")  # Loading the data

location_crime_data = pd.read_csv('Raw_DataSet/raw_crime.csv', usecols=['Lat', 'Long'])

# Handling the missing values
location_crime_data = location_crime_data.dropna()

# location_crime_data.head()
# Preprocessing the data to make it visualizable

# Scaling the Data
scaler = StandardScaler()
X_scaled = scaler.fit_transform(location_crime_data)

# Normalizing the Data
X_normalized = normalize(X_scaled)

# Converting the numpy array into a pandas DataFrame
X_normalized = pd.DataFrame(X_normalized)

# Reducing the dimensions of the data
pca = PCA(n_components=2)
X_principal = pca.fit_transform(X_normalized)
X_principal = pd.DataFrame(X_principal)
X_principal.columns = ['P1', 'P2']

# X_principal.head()

# ---- BE CAREFUL BELOW. MIGHT OR MIGHT NOT CRASH YOUR COMPUTER. HIGH CPU USAGE 
# Method B ,  affinity = ‘nearest_neighbors’
# Building the clustering model
spectral_model_nn = SpectralClustering(n_clusters=2, affinity='nearest_neighbors')

# Training the model and Storing the predicted cluster labels
labels_nn = spectral_model_nn.fit_predict(X_principal)
