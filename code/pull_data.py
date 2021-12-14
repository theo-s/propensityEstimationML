# First, run pip install folktables before running any of these cells.
import numpy as np
import pandas as pd
import folktables
from folktables import adult_filter
from folktables import ACSDataSource


print("downloading data")
data_source = ACSDataSource(survey_year='2018', horizon='1-Year', survey='person')
acs_data = data_source.get_data(states = ['CA'], download=True)
print("finished downloading data")

acs_data.head()

columns = [# More details about the future can be found in Appendix B: https://arxiv.org/pdf/2108.04884.pdf
        'AGEP', # Age
        'COW',  # Class of worker
        'SCHL', # Level of schooling
        'MAR',  # Marital status
        'OCCP', # Occupation
        'WKHP', # Work hours per week
        'POBP', # Place of birth
        'CIT',  # Citizenship status
        'DIS',  # Disability code
        'SEX',  # Gender code
        'RAC1P',# Recorded race code
        'PINCP' # Income variable
        # ... You can choose to add more features if you think they would be helpful in the causal estimation
]

df = acs_data
res = []
for feature in columns:
    res.append(df[feature].to_numpy())
res_array = np.column_stack(res)

#print(res_array)
df = pd.DataFrame(data=res_array, columns=columns)
print(df.head())
# Check NA's before filtering
print(df.isnull().sum())
print(df.shape)

# Filter all observations with missing SCHL and PINCP
df = df[df['SCHL'].notnull()]
df = df[df['PINCP'].notnull()]

# Filter all observations by adults only
df = df[df['AGEP'] > 18]

df['HED'] = (df['SCHL'] >= 21).astype(int) # Higher ed: Associate degree and above
#inc_cutoff = 750000 # The upper tail of incomes would distort our analysis
#df['PINCP'] = df['PINCP'].apply(lambda x: x if x <= inc_cutoff else inc_cutoff)
print(df.isnull().sum())
print(df.shape)

print("Average and median income for the two groups")
print(df.groupby(['HED']).agg({'PINCP':['count', 'mean', 'median']}))

df.to_csv("~/Desktop/propensityEstimationML/data/ca2018.csv", encoding='utf-8', index=False)
