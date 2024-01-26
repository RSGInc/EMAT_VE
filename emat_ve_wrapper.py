from genericpath import exists
import os
import numpy as np
import pandas as pd
import logging
import tempfile
# import re
import shutil
# import platform
import subprocess
# import json
from distutils.dir_util import copy_tree
from distutils.file_util import copy_file

from emat import Scope, SQLiteDB
from emat.model.core_files import FilesCoreModel
from emat.model.core_files.parsers import TableParser, MappingParser, loc, key, iloc

_logger = logging.getLogger("EMAT.VEModel")

# The demo model code is located in the same
# directory as this script file.  We can recover
# this directory name like this, even if the
# current working directory is different.
this_directory = os.path.dirname(__file__)

def scenario_input(*filename):
	"""The path to a scenario_input file."""
	return os.path.join(this_directory, 'Scenario-Inputs', *filename)

def join_norm(*args):
	"""Normalize joined paths."""
	return os.path.normpath(os.path.join(*args))

def r_join_norm(*args):
	"""Normalize joined paths."""
	return os.path.normpath(os.path.join(*args)).replace('\\','/')


class VEModel(FilesCoreModel):
	"""
	A class for using the Vision Eval RSPM as a files core model.

	Args:
		db (emat.Database, optional):
			An optional Database to store experiments and results.
			This allows this module to store results in a persistent
			manner across sessions.  If a `db` is not given, one is
			created and initialized in the temporary directory
			alongside the other model files, but it will be
			deleted automatically when the Python session ends.
		db_filename (str, default "verspm.db")
			The filename used to create a database if no existing
			database is given in `db`.
		scope (emat.Scope, optional):
			A YAML file that defines the scope for these model
			runs. If not given, the default scope stored in this
			package directly is used.
	"""

	def __init__(self, db=None, db_filename="verspm.db", scope=None):

		# Make a temporary directory for this instance.
		self.master_directory = tempfile.TemporaryDirectory(dir=join_norm(this_directory,'Temporary'))
		os.chdir(self.master_directory.name)
		_logger.warning(f"changing cwd to {self.master_directory.name}")
		cwd = self.master_directory.name


		# Housekeeping for this example:
		# Also copy the CONFIG and SCOPE files
		for i in ['ve-model-config']:
			shutil.copy2(
				join_norm(this_directory, 'EMAT-VE-Configs', f"{i}.yml"),
				join_norm(cwd, f"{i}.yml"),
			)

		if scope is None:
			scope = Scope(join_norm(this_directory, 'EMAT-VE-Configs', "ve-model-scope.yml"))
			for i in ['scope']:
				shutil.copy2(
					join_norm(this_directory, 'EMAT-VE-Configs', f"ve-model-{i}.yml"),
					join_norm(cwd, f"ve-model-{i}.yml"),
				)
		else:
			scope.dump(filename=join_norm(cwd, f"ve-model-scope.yml"))

		# Initialize a new daatabase if none was given.
		if db is None:
			if os.path.exists(db_filename):
				initialize = False
			else:
				initialize = True
			db = SQLiteDB(
				db_filename,
				initialize=initialize,
			)
		if db is False: # explicitly use no DB
			db = None
		else:
			if scope.name not in db.read_scope_names():
				db.store_scope(scope)

		# Initialize the super class (FilesCoreModel)
		super().__init__(
			configuration=join_norm(cwd, "ve-model-config.yml"),
			scope=scope,
			db=db,
			name='VEModel',
			local_directory = cwd,
		)
		if isinstance(db, SQLiteDB):
			self._sqlitedb_path = db.database_path

		# Create parameter address to indicate directory in which the scenario files are stored

		# Create a scenario input directory dictionary
		self.scenario_input_dirs = {parameter.name:parameter.address for parameter in self.scope.get_parameters()} 

		# Ensuring R Exe path is in env.
		os.environ['path'] = join_norm(self.config['r_executable'])+';'+os.environ['path']

		# Ensure that R paths are set correctly.
		r_lib = self.config['r_library_path']
		r_runtime_path = self.config['r_runtime_path']

		with open(join_norm(self.local_directory, '.Rprofile'), 'wt') as rprof:
			rprof.write(f'source(file.path("{r_runtime_path}", "VisionEval.R"), chdir=TRUE)')

		cmd = 'Rscript'
		
		self.modelname = self.config['model_type'] + '-' + self.config['model_variant']
		modelpath = r_join_norm(self.local_directory, self.modelname)
		self.model_path = modelpath
		
		# Add the model year and base year
		self.model_base_year = int(self.config['base_year'])
		self.model_future_year = int(self.config['model_year'])

		with open(join_norm(self.local_directory, 'veinstaller.R'), 'wt') as veinstaller:
			veinstaller.write(f"""
			# This model should load the base year results
			ematmodel <- installModel('{self.config['model_type']}', variant='{self.config['model_variant']}', '{modelpath}', confirm=FALSE)

			# Get the config path from the model
			model_dir <- ematmodel$setting("ModelDir")
			config_file <- file.path(model_dir, "visioneval.cnf")

			# Update the config to load the base year model
			runConfig_ls <-  list(
			Model       = "{self.config['model_type']} 3.0 MultiModal, STS Powertrain and Telework",
			Scenario    = "{self.config['model_type']} {self.config["model_year"]} METRO MultiModal, STS Powertrain, with telework",
			Description = "STS inputs",
			Region      = "METRO",
			State       = "OR",
			BaseYear    = "2010",
			Years       = c('{self.config["model_year"]}'),
			LoadModel   = '{self.config['base_model']}'
			)

			viewSetup(Param_ls = runConfig_ls)
			yaml::write_yaml(runConfig_ls, config_file)

			# Re configure the model and run
			ematmodel$configure()
			""")

		# print(arg)
		# print([cmd, '-e', arg])
		

		results = subprocess.run(
			 [cmd, 'veinstaller.R'],
		 	cwd=self.local_directory,
		 	capture_output=False)
		
		print(results)

		def read_csv_index_character(filename, index_colname, **kwargs,):
			df = pd.read_csv(filename, **kwargs)
			df = df.set_index(index_colname)
			df.index = df.index.map(str)
			return df

		for measure in scope.get_measures():
			instructions = {}			
			if measure.parser:
				if measure.parser.get('loc'):
					instructions[measure.name] = loc[(str(j) for j in measure.parser.get('loc'))]
			self.add_parser(
				TableParser(
					measure.parser.get('file'),
					instructions,
					reader_method=read_csv_index_character,
					index_colname='Measure',
				)
			)
		


	def setup(self, params: dict):
		"""
		Configure the core model with the experiment variable values.

		This method is the place where the core model set up takes place,
		including creating or modifying files as necessary to prepare
		for a VE core model run.  When running experiments, this method
		is called once for each core model experiment, where each experiment
		is defined by a set of particular values for both the exogenous
		uncertainties and the policy levers.  These values are passed to
		the experiment only here, and not in the `run` method itself.
		This facilitates debugging, as the `setup` method can potentially
		be used without the `run` method, allowing the user to manually
		inspect the prepared files and ensure they are correct before
		actually running a potentially expensive model.

		At the end of the `setup` method, a core model experiment should be
		ready to run using the `run` method.

		Args:
			params (dict):
				experiment variables including both exogenous
				uncertainty and policy levers

		Raises:
			KeyError:
				if a defined experiment variable is not supported
				by the core model
		"""
		_logger.info(f"{self.config['model_type']} SETUP...")

		for p in self.scope.get_parameters():
			if p.name not in params:
				_logger.warning(f" - for {p.name} using default value {p.default}")
				params[p.name] = p.default

		super().setup(params)

		# Set R environment path to run R and use visioneval to install model
		os.environ['path'] = join_norm(self.config['r_executable'])+';'+os.environ['path']

		# Check if we are using distributed multi-processing. If so,
		# we'll need to copy some files into a local working directory,
		# as otherwise changes in the files will over-write each other
		# when different processes are working in a common directory at
		# the same time.
		try:
			# First try to import the dask.distributed library
			# and check if this code is running on a worker.
			from dask.distributed import get_worker
			worker = get_worker()
		except (ValueError, ImportError):
			# If the library is not available, or if the code is
			# not running on a worker, then we are not running
			# in multi-processing mode, and we can just use
			# the main cwd as the working directory without
			# copying anything.
			pass
		else:
			# If we do find we are running this setup on a
			# worker, then we want to set the local directory
			# accordingly. We copy model files from the "master"
			# working directory to the worker's local directory,
			# if it is different (it should be). Depending
			# on how large your core model is, you may or may
			# not want to be copying the whole thing.
			if self.local_directory != worker.local_directory:

				# Make the archive path absolute, so all archives
				# go back to the original directory.
				# If using non-file based model using installModel function then
				# it should install model once again in the worker's local directory
				self.archive_path = os.path.abspath(self.resolved_archive_path)

				_logger.debug(f"DISTRIBUTED.COPY FROM {self.local_directory}")
				_logger.debug(f"                   TO {worker.local_directory}")
				copy_tree(
					join_norm(self.local_directory, self.modelname),
					join_norm(worker.local_directory, self.modelname),
				)
				copy_file(
					join_norm(self.local_directory, '.Rprofile'),
					join_norm(worker.local_directory, '.Rprofile'),
				)
				self.local_directory = worker.local_directory
				self.model_path = join_norm(worker.local_directory, self.modelname)

		# The process of manipulating each input file is broken out
		# into discrete sub-methods, as each step is loosely independent
		# and having separate methods makes this clearer.
		tmip_vars = [var_name.upper() for var_name in params.keys()]
		if 'LUDENSITYMIX' in tmip_vars:
			self._manipulate_ludensity(params)
		if 'INTDENSITYSCEN' in tmip_vars:
			self._manipulate_intdensity(params)
		if 'HHPOPGROWTHRATE' in tmip_vars:
			self._manipulate_population(params)
		if 'INCOMEGROWTH' in tmip_vars:
			self._manipulate_income(params)
		if 'LDVECODRVSCEN' in tmip_vars:
			self._manipulate_ldvecodrv(params)
		if 'CARSVCAVAILSCEN' in tmip_vars:
			self._manipulate_carsvcavail(params)
		if 'SHDCARSVCOCCUPRATE' in tmip_vars:
			self._manipulate_shdcarsvc(params)
		if 'DRVLESSADJSCEN' in tmip_vars:
			self._manipulate_drvlessadj(params)
		if 'DRVLESSPROPREMOTEACC' in tmip_vars and 'PROPPARKINGFEEAVOID' in tmip_vars:
			self._manipulate_drvless_param(params)
		if 'AVVEHSALESGROWTHSCEN' in tmip_vars:
			self._manipulate_drvlessvehsales(params)
		if 'CARCHARGEAVAILSCEN' in tmip_vars:
			self._manipulate_carchargeavailscen(params)
		if 'CICHANGERATESCEN' in tmip_vars:
			self._manipulate_cichange(params)
		if 'INVESTMENTSCEN' in tmip_vars:
			self._manipulate_inv(params)
		if 'SOVDIVIVERTSCEN' in tmip_vars:
			self._manipulate_sovdivert(params)
		if 'TAXSCEN' in tmip_vars:
			self._manipulate_taxes(params)
		if 'LANEMILESCEN' in tmip_vars:
			self._manipulate_mlanemiles(params)
		if 'OPSDEPLOYSCEN' in tmip_vars:
			self._manipulate_opsdeployment(params)
		if 'TRANSITSERVICESCEN' in tmip_vars:
			self._manipulate_transitservice(params)
		if 'TDMINVESTMENTSCEN' in tmip_vars:
			self._manipulate_tdmareatype(params)
		if 'TRANSITSCEN' in tmip_vars:
			self._manipulate_transitscen(params)
		if 'POWERTRAINSCEN' in tmip_vars:
			self._manipulate_powertrainscen(params)

		_logger.info(f"{self.config['model_type']} SETUP complete")


	def _manipulate_by_categorical_drop_in(self, params, cat_param, ve_scenario_dir):
		"""
		Copy in the relevant input files.

		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""
		scenario_dir = params[cat_param]
		for i in os.scandir(scenario_input(ve_scenario_dir,scenario_dir)):
			if i.is_file():
				shutil.copyfile(
					scenario_input(ve_scenario_dir,scenario_dir,i.name),
					join_norm(self.resolved_model_path, 'inputs', i.name)
				)

	def _manipulate_by_mixture(self, params, weight_param, ve_scenario_dir, no_mix_cols=('Year', 'Geo',), float_dtypes=False):
		"""
		Prepare files by interpolating parameters between two files.

		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
			weight_param:
				The name of the parameters that is generated from the
				scope file
			ve_scenario_dir:
				The name of the directory that contains the two set
				of folder/files that need to be interpolated
			no_mix_cols:
				Columns that should not be interpolated
		"""

		weight_2 = params[weight_param]
		weight_1 = 1.0-weight_2

		# Gather list of all files in directory "1", and confirm they
		# are also in directory "2"
		filenames = []
		for i in os.scandir(scenario_input(ve_scenario_dir,'1')):
			if i.is_file():
				filenames.append(i.name)
				f2 = scenario_input(ve_scenario_dir,'2', i.name)
				if not os.path.exists(f2):
					raise FileNotFoundError(f2)

		for filename in filenames:
			df1 = pd.read_csv(scenario_input(ve_scenario_dir,'1',filename))
			isna_ = (df1.isnull().values).any()
			df1.fillna(0, inplace=True)
			df2 = pd.read_csv(scenario_input(ve_scenario_dir,'2',filename))
			df2.fillna(0, inplace=True)

			float_mix_cols = list(df1.select_dtypes('float').columns)
			if float_dtypes:
				float_mix_cols = float_mix_cols+list(df1.select_dtypes('int').columns)
			for j in no_mix_cols:
				if j in float_mix_cols:
					float_mix_cols.remove(j)

			if float_mix_cols:
				df1_float = df1[float_mix_cols]
				df2_float = df2[float_mix_cols]
				df1[float_mix_cols] = df1_float * weight_1 + df2_float * weight_2

			int_mix_cols = list(df1.select_dtypes('int').columns)
			if float_dtypes:
				int_mix_cols = list()
			for j in no_mix_cols:
				if j in int_mix_cols:
					int_mix_cols.remove(j)

			if int_mix_cols:
				df1_int = df1[int_mix_cols]
				df2_int = df2[int_mix_cols]
				df_int_mix = df1_int * weight_1 + df2_int * weight_2
				df1[int_mix_cols] = np.round(df_int_mix).astype(int)

			out_filename = join_norm(
				self.resolved_model_path, 'inputs', filename
			)
			if isna_:
				df1.replace(0, np.nan, inplace=True)
			df1.to_csv(out_filename, index=False, float_format="%.5f", na_rep='NA')

	def _manipulate_by_scale(self, params, param_map, ve_scenario_dir, max_thresh=1E9):
		"""
		Prepare files by multiplying fields with the scalar value.

		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
			param_map:
				The dictionary that maps parameter to columns in the data
			ve_scenario_dir:
				The name of the directory that contains the two set
				of folder/files that need to be interpolated
			no_mix_cols:
				Columns that should not be interpolated
		"""

		# Gather list of all files in directory "1", and confirm they
		# are also in directory "2"
		filenames = []

		for i in os.scandir(scenario_input(ve_scenario_dir)):
			if i.is_file():
				filenames.append(i.name)

		for filename in filenames:
			df1 = pd.read_csv(scenario_input(ve_scenario_dir,filename))
			isna_ = (df1.isnull().values).any()
			df1.fillna(0, inplace=True)

			for param_name, column_names in param_map.items():
				if not isinstance(column_names, list):
					df1[[column_names]] = (df1[[column_names]]*params.get(param_name)).clip(lower=-max_thresh,upper=max_thresh)
				else:
					for column_name in column_names:
						df1[[column_name]] = (df1[[column_name]]*params.get(param_name)).clip(lower=-max_thresh,upper=max_thresh)

			out_filename = join_norm(
				self.resolved_model_path, 'inputs', filename
			)
			if isna_:
				df1.replace(0, np.nan, inplace=True)
			df1.to_csv(out_filename, index=False, float_format="%.5f", na_rep='NA')

	def _manipulate_by_delta(self, params, weight_param, ve_scenario_dir, no_mix_cols=('Year', 'Geo',)):
		"""
		Prepare files by interpolating parameters between two files.

		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
			weight_param:
				The name of the parameters that is generated from the
				scope file
			ve_scenario_dir:
				The name of the directory that contains the two set
				of folder/files that need to be interpolated
			no_mix_cols:
				Columns that should not be interpolated
		"""

		# weight_2 = params[weight_param]
		# weight_1 = 1.0-weight_2
		weight_ = params[weight_param]

		# Gather list of all files in directory "1", and confirm they
		# are also in directory "2"
		filenames = []
		for i in os.scandir(scenario_input(ve_scenario_dir,'1')):
			if i.is_file():
				filenames.append(i.name)
				f2 = scenario_input(ve_scenario_dir,'2', i.name)
				if not os.path.exists(f2):
					raise FileNotFoundError(f2)

		for filename in filenames:
			df1 = pd.read_csv(scenario_input(ve_scenario_dir,'1',filename))
			df2 = pd.read_csv(scenario_input(ve_scenario_dir,'2',filename))

			float_mix_cols = list(df1.select_dtypes('float').columns)
			for j in no_mix_cols:
				if j in float_mix_cols:
					float_mix_cols.remove(j)

			if float_mix_cols:
				df1_float = df1[float_mix_cols]
				df2_float = df2[float_mix_cols]
				delta_float = df2_float - df1_float
				df1[float_mix_cols] = df1_float + (delta_float * weight_)

			int_mix_cols = list(df1.select_dtypes('int').columns)
			for j in no_mix_cols:
				if j in int_mix_cols:
					int_mix_cols.remove(j)

			if int_mix_cols:
				df1_int = df1[int_mix_cols]
				df2_int = df2[int_mix_cols]
				delta_int = df2_int - df1_int
				df_int_mix = df1_int + (delta_int * weight_)
				df1[int_mix_cols] = np.round(df_int_mix).astype(int)

			out_filename = join_norm(
				self.resolved_model_path, 'inputs', filename
			)
			df1.to_csv(out_filename, index=False, float_format="%.5f")

	def _manipulate_ludensity(self, params):
		"""
		Prepare the urban mix proportion by marea
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_mixture(params, 'LUDENSITYMIX', self.scenario_input_dirs.get('LUDENSITYMIX'))

	def _manipulate_intdensity(self, params):
		"""
		Prepare the D3BPO4 adjustment factor by marea
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""
		return self._manipulate_by_mixture(params, 'INTDENSITYSCEN', self.scenario_input_dirs.get('INTDENSITYSCEN'))

	def _manipulate_population(self, params):
		"""
		Prepare the population files
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'HHPOPGROWTHRATE', self.scenario_input_dirs.get('HHPOPGROWTHRATE'))

	def _manipulate_income(self, params):
		"""
		Prepare the income input file based on a template file.
        Type: Manipulation
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		income_df = pd.read_csv(join_norm(scenario_input(self.scenario_input_dirs.get('INCOMEGROWTHRATE'),'azone_per_cap_inc.csv')))

		unique_years = income_df.Year.unique()
		base_year = self.model_base_year

		for run_year in unique_years:
			year_diff = run_year - base_year
			income_df.loc[income_df.Year == run_year,['HHIncomePC.2005', 'GQIncomePC.2005']] = \
			income_df.loc[income_df.Year == run_year,['HHIncomePC.2005', 'GQIncomePC.2005']] * (params['INCOMEGROWTHRATE'] ** year_diff)
		
		out_filename = join_norm(
			self.resolved_model_path, 'inputs', 'azone_per_cap_inc.csv'
		)
		_logger.debug(f"writing updates to: {out_filename}")
		income_df.to_csv(out_filename, index=False)
		
	def _manipulate_ldvecodrv(self, params):
		"""
		Prepate the LDV ecodrive penetration file.
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""
		return self._manipulate_by_mixture(params, 'LDVECODRVSCEN', self.scenario_input_dirs.get('LDVECODRVSCEN'))

	def _manipulate_carsvcavail(self, params):
		"""
		Prepate the car service availability file.
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'CARSVCAVAILSCEN', self.scenario_input_dirs.get('CARSVCAVAILSCEN'))

	def _manipulate_shdcarsvc(self, params):
		"""
		Prepare the shared car service occupancy rate file.
        Type: Manipulation
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		shdcarsvc_occp_df = pd.read_csv(join_norm(scenario_input(self.scenario_input_dirs.get('SHDCARSVCOCCUPRATE'),'region_carsvc_shd_occup.csv')))

		future_year = self.model_future_year

		shdcarsvc_occp_df.loc[shdcarsvc_occp_df.Year == future_year, 'ShdCarSvcAveOccup'] = params['SHDCARSVCOCCUPRATE']
		
		out_filename = join_norm(
			self.resolved_model_path, 'inputs', 'region_carsvc_shd_occup.csv'
		)
		_logger.debug(f"writing updates to: {out_filename}")
		shdcarsvc_occp_df.to_csv(out_filename, index=False)

	def _manipulate_drvlessadj(self, params):
		"""
		Prepare the delay and smoothing adjustment factor file
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'DRVLESSADJSCEN', self.scenario_input_dirs.get('DRVLESSADJSCEN'))

	def _manipulate_drvless_param(self, params):
		"""
		Prepare the driverless vehicle parameters file.
        Type: Manipulation
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		drvless_veh_param_df = pd.read_csv(join_norm(scenario_input(self.scenario_input_dirs.get('DRVLESSPROPREMOTEACC'),'region_driverless_vehicle_parameter.csv')))

		future_year = self.model_future_year

		drvless_veh_param_df.loc[drvless_veh_param_df.Year == future_year, 'PropRemoteAccess'] = params['DRVLESSPROPREMOTEACC']
		drvless_veh_param_df.loc[drvless_veh_param_df.Year == future_year, 'PropParkingFeeAvoid'] = params['PROPPARKINGFEEAVOID']
		
		out_filename = join_norm(
			self.resolved_model_path, 'inputs', 'region_driverless_vehicle_parameter.csv'
		)
		_logger.debug(f"writing updates to: {out_filename}")
		drvless_veh_param_df.to_csv(out_filename, index=False)

	def _manipulate_drvlessvehsales(self, params):
		"""
		Prepare the driverless vehicles sales file
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'AVVEHSALESGROWTHSCEN', self.scenario_input_dirs.get('AVVEHSALESGROWTHSCEN'))

	def _manipulate_carchargeavailscen(self, params):
		"""
		Prepare the azone_charging_availability.csv
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_mixture(params, 'CARCHARGEAVAILSCEN', self.scenario_input_dirs.get('CARCHARGEAVAILSCEN'))

	def _manipulate_cichange(self, params):
		"""
		Prepare the carbon emissions related files
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'CICHANGERATESCEN', self.scenario_input_dirs.get('CICHANGERATESCEN'))

	def _manipulate_inv(self, params):
		"""
		Prepare the investment related files
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'INVESTMENTSCEN', self.scenario_input_dirs.get('INVESTMENTSCEN'))

	def _manipulate_sovdivert(self, params):
		"""
		Prepare the azone_prop_sov_dvmt_diverted.csv
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_mixture(params, 'SOVDIVIVERTSCEN', self.scenario_input_dirs.get('SOVDIVIVERTSCEN'))

	def _manipulate_taxes(self, params):
		"""
		Prepare the azone_veh_use_taxes.csv file.
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_mixture(params, 'TAXSCEN', self.scenario_input_dirs.get('TAXSCEN'),  no_mix_cols=('Year', 'Geo', 'FuelTax.2005'))

	def _manipulate_mlanemiles(self, params):
		"""
		Prepare the marea_lane_miles.csv file
        Type: Manipulation
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_delta(params, 'LANEMILESCEN', self.scenario_input_dirs.get('LANEMILESCEN'))

	def _manipulate_opsdeployment(self, params):
		"""
		Prepare the marea_operations_deployment.csv file
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_mixture(params, 'OPSDEPLOYSCEN', self.scenario_input_dirs.get('OPSDEPLOYSCEN'))

	def _manipulate_transitservice(self, params):
		"""
		Prepare the marea_transit_service.csv file
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_mixture(params, 'TRANSITSERVICESCEN', self.scenario_input_dirs.get('TRANSITSERVICESCEN'))

	def _manipulate_tdmareatype(self, params):
		"""
		Prepare the marea_travel-demand-mgt_by_area-type.csv
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""
		return self._manipulate_by_mixture(params, 'TDMINVESTMENTSCEN', self.scenario_input_dirs.get('TDMINVESTMENTSCEN'))

	def _manipulate_transitscen(self, params):
		"""
		Prepare the transit mix scenario
        Type: Mixture
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_mixture(params, 'TRANSITSCEN', self.scenario_input_dirs.get('TRANSITSCEN'), float_dtypes=True)

	def _manipulate_powertrainscen(self, params):
		"""
		Prepare the run model script so that approprate PowertrainAndFuelsPackage is used
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		scenario_dir = params['POWERTRAINSCEN']
		ve_scenario_dir = self.scenario_input_dirs.get('POWERTRAINSCEN')
		for i in os.scandir(scenario_input(ve_scenario_dir,scenario_dir)):
			if i.is_file():
				shutil.copyfile(
					scenario_input(ve_scenario_dir,scenario_dir,i.name),
					join_norm(self.resolved_model_path, 'scripts', i.name)
				)

	def _manipulate_expand_roads(self, params):
		"""
		Prepare the expand road investment files
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'EXPANDROADS', self.scenario_input_dirs.get('EXPANDROADS'))

	def _manipulate_transit(self, params):
		"""
		Prepare the transit investment files
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'TRANSIT', self.scenario_input_dirs.get('TRANSIT'))

	def _manipulate_bikewalk(self, params):
		"""
		Prepare the bike and walk files
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'BIKEWALK', self.scenario_input_dirs.get('BIKEWALK'))

	def _manipulate_operations(self, params):
		"""
		Prepare the operations files
        Type: Categorical
		Args:
			params (dict):
				The parameters for this experiment, including both
				exogenous uncertainties and policy levers.
		"""

		return self._manipulate_by_categorical_drop_in(params, 'OPERATIONS', self.scenario_input_dirs.get('OPERATIONS'))


	def run(self):
		"""
		Run the core model.

		This method is the place where the RSPM core model run takes place.
		Note that this method takes no arguments; all the input
		exogenous uncertainties and policy levers are delivered to the
		core model in the `setup` method, which will be executed prior
		to calling this method. This facilitates debugging, as the `setup`
		method can potentially be used without the `run` method, allowing
		the user to manually inspect the prepared files and ensure they
		are correct before actually running a potentially expensive model.
		When running experiments, this method is called once for each core
		model experiment, after the `setup` method completes.

		Raises:
		    UserWarning: If model is not properly setup
		"""
		_logger.info(f"{self.config['model_type']} RUN ...")

		os.environ['path'] = join_norm(self.config['r_executable'])+';'+os.environ['path']
		
		cmd = 'Rscript'

		# Script that opens the model and runs it
		with open(join_norm(self.local_directory, "vemodel_runner.R"), "wt") as r_script:
			r_script.write(f"""
			thismodel <- openModel("{r_join_norm(self.local_directory, self.modelname)}")
			thismodel$run("reset")
			""")

		# The subprocess.run command runs a command line tool. The
		# name of the command line tool, plus all the command line arguments
		# for the tool, are given as a list of strings, not one string.
		# The `cwd` argument sets the current working directory from which the
		# command line tool is launched.  Setting `capture_output` to True
		# will capture both stdout and stderr from the command line tool, and
		# make these available in the result to facilitate debugging.
		self.last_run_result = subprocess.run(
			[cmd, 'vemodel_runner.R'],
			cwd=self.local_directory,
			capture_output=True,
		)
		##Add errors log
		if self.last_run_result.returncode:
			raise subprocess.CalledProcessError(
				self.last_run_result.returncode,
				self.last_run_result.args,
				self.last_run_result.stdout,
				self.last_run_result.stderr,
			)
		else:
			with open(join_norm(self.local_directory, self.modelname, 'results', 'stdout.log'), 'wb') as slog:
				slog.write(self.last_run_result.stdout)

		_logger.info(f"{self.config['model_type']} RUN complete")


	def last_run_logs(self, output=None):
		"""
		Display the logs from the last run.
		"""
		if output is None:
			output = print
		def to_out(x):
			if isinstance(x, bytes):
				output(x.decode())
			else:
				output(x)
		try:
			last_run_result = self.last_run_result
		except AttributeError:
			output("no run stored")
		else:
			if last_run_result.stdout:
				output("=== STDOUT ===")
				to_out(last_run_result.stdout)
			if last_run_result.stderr:
				output("=== STDERR ===")
				to_out(last_run_result.stderr)
			output("=== END OF LOG ===")


	def post_process(self, params=None, measure_names=None, output_path=None):
		"""
		Runs post processors associated with particular performance measures.

		This method is the place to conduct automatic post-processing
		of core model run results, in particular any post-processing that
		is expensive or that will write new output files into the core model's
		output directory.  The core model run should already have
		been completed using `setup` and `run`.  If the relevant performance
		measures do not require any post-processing to create (i.e. they
		can all be read directly from output files created during the core
		model run itself) then this method does not need to be overloaded
		for a particular core model implementation.

		Args:
			params (dict):
				Dictionary of experiment variables, with keys as variable names
				and values as the experiment settings. Most post-processing
				scripts will not need to know the particular values of the
				inputs (exogenous uncertainties and policy levers), but this
				method receives the experiment input parameters as an argument
				in case one or more of these parameter values needs to be known
				in order to complete the post-processing.  In this demo, the
				params are not needed, and the argument is optional.
			measure_names (List[str]):
				List of measures to be processed.  Normally for the first pass
				of core model run experiments, post-processing will be completed
				for all performance measures.  However, it is possible to use
				this argument to give only a subset of performance measures to
				post-process, which may be desirable if the post-processing
				of some performance measures is expensive.  Additionally, this
				method may also be called on archived model results, allowing
				it to run to generate only a subset of (probably new) performance
				measures based on these archived runs. In this demo, the
				the argument is optional; if not given, all measures will be
				post-processed.
			output_path (str, optional):
				Path to model outputs.  If this is not given (typical for the
				initial run of core model experiments) then the local/default
				model directory is used.  This argument is provided primarily
				to facilitate post-processing archived model runs to make new
				performance measures (i.e. measures that were not in-scope when
				the core model was actually run).

		Raises:
			KeyError:
				If post process is not available for specified measure
		"""
	
		extraction_script = self.config['extract_script']
		if not os.path.exists(join_norm(self.local_directory, self.modelname, extraction_script)):
			shutil.copy2(
					join_norm(this_directory, extraction_script),
					join_norm(self.local_directory, self.modelname, extraction_script),
				)

		cwd2 = join_norm(self.local_directory, self.modelname)

		# Ensuring R Exe path is in env.
		os.environ['path'] = join_norm(self.config['r_executable'])+';'+os.environ['path']

		# Ensure that R paths are set correctly.
		r_runtime_path = self.config['r_runtime_path']
		
		with open(join_norm(self.local_directory, self.modelname, '.Rprofile'), 'wt') as rprof:
			rprof.write(f'source(file.path("{r_runtime_path}", "VisionEval.R"), chdir=TRUE)')

		cmd = 'Rscript'

		### The subprocess.run command runs a command line tool.
		self.postprocess_results = subprocess.run(
			[cmd, extraction_script],
			cwd=cwd2,
			capture_output=True,
		)

				##Add errors log
		if self.postprocess_results.returncode:
			raise subprocess.CalledProcessError(
				self.postprocess_results.returncode,
				self.postprocess_results.args,
				self.postprocess_results.stdout,
				self.postprocess_results.stderr,
			)
		else:
			with open(join_norm(self.resolved_model_path, 'results', 'postprocess_stdout.log'), 'wb') as slog:
				slog.write(self.postprocess_results.stdout)


	def archive(self, params, model_results_path=None, experiment_id=None):
		"""
		Copies model outputs to archive location.

		Args:
			params (dict):
				Dictionary of experiment variables
			model_results_path (str, optional):
				The archive path to use.  If not given, a default
				archive path is constructed based on the scope name
				and the experiment_id.
			experiment_id (int, optional):
				The id number for this experiment.  Ignored if the
				`model_results_path` argument is given.

		"""
		if model_results_path is None:
			if experiment_id is None:
				db = getattr(self, 'db', None)
				if db is not None:
					experiment_id = db.get_experiment_id(self.scope.name, None, params)
			model_results_path = self.get_experiment_archive_path(experiment_id)
		zipname = os.path.join(model_results_path, 'run_archive')
		_logger.info(
			f"VERSPM ARCHIVE\n"
			f" from: {join_norm(self.local_directory, self.modelname, self.rel_output_path)}\n"
			f"   to: {zipname}.zip"
		)
		shutil.make_archive(
			zipname, 'zip',
			root_dir=join_norm(self.local_directory, self.modelname),
			base_dir=self.rel_output_path,
		)

