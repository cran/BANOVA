# BANOVA R package
V0.6 improvements:
  1. add checking missing data for independent variables
  2. Fix typos
  3. add warnings for mean centering of numeric variables
  4. print full convergence diag. for the Heidelberg and Welch diagnostic.
  5. Table of means can now print any level of interactions

V0.7 improvements (08-2016):
  1. exclude numeric variables for predictions 
  2. format change of convergence diag 
  3. table of means and prediction change (exp(mu + sigma^2/2)) for the Poisson model
  4. table of means included in the summary
  5. unify the format of digits, and names of the table of effect sizes and table of p values (round and format)
  6. median -> mean 
  7. predictors that have 3 levels or fewer will be automatically considered as factors
  
V0.8 improvements (12-2016)
  1. change the name 'table of means' to 'table of predictions'
  2. column names of the data goalstudy are changed to make more sense
  3. add floodlight analysis
  4. add mediation analysis (single mediator, multiple moderators)
  
v0.9 improvements (06-2017)
  1. BANOVA uses Stan!
  2. all two-level models are moved into a simple function call: BANOVA.run 
  3. old two-level models (e.g. BANOVA.Normal, BANOVA.Bin, etc.) are still kept, but will be removed gradually
  4. single level models are included
  
  
Backlog: check if a model has an identification issue, e.g. the number of observations of each subject is too small.
