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
  # ... You can choose to add more features if you think they would be helpful in the causal estimation
]

ACSIncomeNew = folktables.BasicProblem(
  features=columns,
  target='PINCP', # Income
  preprocess=adult_filter,
  postprocess=lambda x: np.nan_to_num(x, -1),
)

features, label, _ = ACSIncomeNew.df_to_numpy(acs_data)

# Create a dataframe with all the data
df = pd.DataFrame(features, columns = columns)
df['PINCP'] = label
df['HED'] = (df['SCHL'] >= 21).astype(int) # Higher ed: Associate degree and above
inc_cutoff = 150000 # The upper tail of incomes would distort our analysis
df['PINCP'] = df['PINCP'].apply(lambda x: x if x <= inc_cutoff else inc_cutoff)

df.head()

print("Average and median income for the two groups")
df.groupby(['HED']).agg({'PINCP':['count', 'mean', 'median']})

df.to_csv("~/Desktop/propensityEstimationML/data/ca2018.csv", encoding='utf-8', index=False)
