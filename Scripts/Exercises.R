### Exercises

# Exercise 1: Summary of the Paper and Main Findings
#Provide a brief summary of the paper you are replicating. Describe the main findings, especially
#those related to Study 1.

#Exercise 2: Data Preparation and Exploration
#Use the data file study1_data.csv to begin the replication process. Identify and describe the
#experimental variables (i.e., treatment and immigration attitudes) and provide visualizations
#of their distribution.
#Then, select up to four covariates (e.g., gender, age, etc.) and plot their distribution too.
#If necessary, clean or transform variables. Document any changes.

#Exercise 3: Covariate Balance
#Check for balance across covariates and report the results in a table and a plot.
#Explain your findings. Why would you expect randomization to lead to balance across covariates?

#Exercise 4 (additional): Estimate Treatment Effect
#This exercise is not mandatory, but it serves only to opt for the maximum grade (6).
#Estimate the average effect of the treatment on the outcome variable support conditional on
#the pre-treatment immigration attitudes imm_1. For this, use an interaction term in an OLS
#regression model. Compare your results to those in the original paper.
#Then, repeat this analysis with three iteratively smaller random samples of the treatment 
#(n = 200, n = 100, n = 10) and control groups (n = 200, n = 100, n = 10); total N = 400, 200,
#and 20, respectively. Explain your findings.
#Finally, discuss how sample size impacts the results and what this implies about the role of
#randomization for selection bias.