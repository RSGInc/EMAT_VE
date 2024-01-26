
# %% imports
import os

from dash import Dash, html, dash_table

import emat
from emat.analysis import display_experiments
from emat.analysis import Visualizer

import emat_verspm


# %% Load data

database_path = os.path.join(
    r"C:\Users\matt.landis\Documents\Git\EMAT_VE_Metro",
    "EMAT-VE-Database", 
    "metro-verspm-06112023-500.db"
)

db = emat.SQLiteDB(database_path, initialize=False)
model_scope = db.read_scope()
fx = emat_verspm.VERSPMModel(db=db, scope=model_scope)

design_name = "exp500"
results = fx.db.read_experiment_all(fx.scope.name, design_name=design_name)
# results.head()

# %% Display results
# display_experiments(
#   fx.scope, 
#   results, 
#   rows=fx.scope.get_measure_names()[:3]
# )

# %% Dashboard
app = Dash(__name__)

app.layout = html.Div([
    html.H1(children='Hello World', style={'textAlign':'center'}),
    dash_table.DataTable(data=results.head().to_dict('records'))
])

if __name__ == '__main__':
    app.run(debug=True)
