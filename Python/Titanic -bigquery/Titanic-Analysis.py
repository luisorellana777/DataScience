import pandas as pd
# to communicate with Google BigQuery
from pandas.io import gbq
import plotly.plotly as py
import plotly.graph_objs as go
from plotly.tools import FigureFactory as FF

project_id = 'titanic-231219'

top10_active_users_query = "SELECT * FROM [titanic-231219.titanic.titanic_passenger] LIMIT 10"


top10_active_users_df = gbq.read_gbq(top10_active_users_query, project_id=project_id)

print (top10_active_users_df)





