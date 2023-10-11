# TMIP-EMAT and VisionEval 3.0

The repository hosts the [TMIP-EMAT](https://tmip-emat.github.io/) framework that is used to run ODOT VE models built in [VisionEval 3.0](https://visioneval.org/).

## Structure
There are six directories contained in the repository:

1. **EMAT-Conda-Setup** - The directory contains the yaml file *emat_install.yml* that can be used to create an emat conda environment. To create the environment use the command
```
conda env create -f emat_install.yml
```
2. **EMAT-VE-Configs** - The directory contains two yaml files. The yaml file *ve-model-config.yml* is used to specify VE model configurations and the yaml file *odot-otp-scope.yml* is used to
specify the scope i.e. the design elements of the experiments along with the measures that should be collected from a model run.
3. **EMAT-VE-Database** - The directory will store the database that TMIP-EMAT will use to run the experiments and store the results.
4. **Scenario-Inputs** - This directory contains scenario input files in sub-directories for each experiment parameter defined by the scope in *odot-otp-scope.yml*.
5. **Temporary** - TMIP-EMAT creates a temporary directory to run experiments. This directory is used as a host for those temporary directories to make post TMIP-EMAT run cleanup easy.


In addition to the directories the repository contains following files in the root directory:
1. *emat_ve_wrapper.py* - The python script that defines how TMIP interfaces with VisionEval models, setup scenarios, run scenarios, and collect results.
2. *extract_outputs.R* - R script used with the VE model to extract the measures as defined in the scope *odot-otp-scope.yml*.
3. *ODOT-TMIP-METAMODEL.ipynb* - The jupyter python notebook used to run and visualize TMIP-EMAT experiments.
4. *metamodel_variables.csv* - This file contains a list (partial or complete) of variables collected from model runs to build the metamodel for.

## Setup Requirements

The TMIP EMAT operates in python and interfaces with VisionEval. Thus, all the software requirements needed for [TMIP-EMAT](https://tmip-emat.github.io/source/emat.install.html) and [VisionEval 3.0](https://visioneval.org/docs/getting-started.html#installation) should be met.

## Run

To run multiple scenarios/experiments in TMIP-EMAT:

1. Open Anaconda3 command prompt and activate the *emat* environment.
2. Navigate to the TMIP-EMAT directory **EMAT_VE**.
3. Enter the command `jupyter notebook` and press *Enter*. This will open a jupyter notbook in a browser and list all the files contained in the **EMAT_VE** directory.
4. Click on *ODOT-TMIP-METAMODEL.ipynb*. This will open the jupyter notbook.
5. Check the values of following parameters in the **Cell Block 2**:

    a. *run_experiments* - It's a logical value that determines whether to run multiple scenarios (**True**) or load the results from the database (**False**).
	b. *database_name* - A character value that tells the name of the database. If one doesn't exists then the notebook will create one. Note that if the notebook is creating the database then it cannot load results and the run_experiments should be set to True.
	c. *model_scope_name* - A character value that indicates the name of the model scope file that should be used to design the experiments.
	d. *num_workers* - An integer value that specifies the number of parallel processors to use to run scenarios.
	e. *num_experiments* - An integer value that specifies the number of scenarios to create.

