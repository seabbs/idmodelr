default: all
RDIR = .

all: model_details.rds parameter_details.rds

## Update incidence rate data
model_details.rds: model_details.R
			Rscript model_details.R


## Update incidence rate data
parameter_details.rds: parameter_details.R
			Rscript parameter_details.R
