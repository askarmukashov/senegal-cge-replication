Senegal CGE Model – Replication Package
This repository contains the clean and fully functional GAMS model used in the paper:
"Promoting Regional Income Equity Under Structural Transformation and Climate Change: An Economy-wide Analysis for Senegal"
Contents
•	0.data – Contains:
o	SAM matrix
o	Model parameters
o	scenarios
•	0project GAMS IDE project file that stores command lines for 1setup and 2simulation gms files (s and r)
•	1setup.gms – Initializes sets, parameters, and reads the SAM, etc.
•	2simulations.gms – Runs baseline and policy/climate change scenarios.
•	DCGEcore subfolder to which either 1setup or 2simulations refers to include .inc files
•	tmp subfolder to which either 1setup or 2simulations unloads/loads temporarily files
How to Run
1.	Open 0project GAMS IDE project file to load command lines for 1setup and 2simulation gms files
2.	Open 1setup.gms and run it to load sets and parameters.
3.	Open 2simulations.gms and run i to simulate all scenarios.
4.	Output will be written to results.gdx and can be exported to Excel. For detailization of what each outcome Z means, refer to DCGEcore/4.outcomes.gms
Notes
•	This model adapts the IFPRI standard CGE structure to the context of the study (Regional Income Equity Under Structural Transformation and Climate Change in Senegal)
•	Elasticities are sourced from standard literature (see Excel sheets for explanations).

