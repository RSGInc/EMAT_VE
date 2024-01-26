
# %%
#| label: imports
import os

# import logging
# import plotly.io; plotly.io.templates.default = "seaborn"
# import seaborn; seaborn.set_theme()

import emat

from emat.analysis import display_experiments
from emat.analysis import Visualizer
# from emat.util.loggers import log_to_stderr
# log = log_to_stderr(logging.INFO)

import emat_verspm

# %%
#| label: parameters

database_path = os.path.join(
    os.getcwd(),"EMAT-VE-Database", "metro-verspm-06112023-500.db"
)

db = emat.SQLiteDB(database_path, initialize=False)
model_scope = db.read_scope()
fx = emat_verspm.VERSPMModel(db=db, scope=model_scope)


design_name = "exp500"

results = fx.db.read_experiment_all(fx.scope.name, design_name=design_name)
# results.head()

# %%
#| label: display_results
display_experiments(
  fx.scope, 
  results, 
  rows=fx.scope.get_measure_names()[:3]
)

# %%
#| label: visualize_results
viz_results = Visualizer(scope=fx.scope, data=results)
display(viz_results.complete())

