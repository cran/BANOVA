# BANOVA R package
V0.6 improvements:
  1. add checking missing data for independent variables
  2. Fix typos
  3. add warnings for mean centering of numeric variables
  4. print full convergence diag. for the Heidelberg and Welch diagnostic.
  5. Table of means can now print any level of interactions

===========================

V0.7 improvements (08-2016):
  1. exclude numeric variables for predictions 
  2. format change of convergence diag 
  3. table of means and prediction change (exp(mu + sigma^2/2)) for the Poisson model
  4. table of means included in the summary
  5. unify the format of digits, and names of the table of effect sizes and table of p values (round and format)
  6. median -> mean 
  7. predictors that have 3 levels or fewer will be automatically considered as factors

===========================  

V0.8 improvements (12-2016):
  1. change the name 'table of means' to 'table of predictions'
  2. column names of the data goalstudy are changed to make more sense
  3. add floodlight analysis
  4. add mediation analysis (single mediator, multiple moderators)

===========================  

v0.9 improvements (06-2017):
  1. BANOVA uses Stan!
  2. all two-level models are moved into a simple function call: BANOVA.run 
  3. old two-level models (e.g. BANOVA.Normal, BANOVA.Bin, etc.) are still kept, but will be removed gradually
  4. single level models are included
  
v0.9 tasks in detail:

*1. add two functions, 
    BANOVA.model(model_name = 'Normal'): visualize the model code, can be modified, return the model code
    BANOVA.build(model_code = BANOVA.model object): stanc the model code, stan model it, return the BANOVA stan model
    BANOVA.NormalNormal needs to have the stan model argument and return the stan model
  
*2. build independent stan modules in addition to the JAGS models (create branch v10), other JAGS models stays the same, adding one level models to JAGS, BANOVA.NormalNormal needs to be changed back

*3. multinomial model related functions needs to be refined 

*4. predict functions need to be done, plus predict.BANOVA

*5. beta2_1_1 vs beta1_1 in the old jags model in the table.predictions issue needs to be fixed (use if else)

*6. ordered multinomial model issues: 
*after calling app_7 <- BANOVA.ordMultinomial (perceivedsim~1, ~progress*prodvar,  goalstudy, goalstudy$id, burnin = 1000, sample = 1000, thin = 1)
then >> app_7
coefficents table is printed twice!

*7. cut point or other unkown parameters not converged, try to use the same initials or change to single level?

*8. single level models

*9. multinomial, table.ancova needs to change for each choice, choice_intercept + variables, the assign values and corresponding estimates

10. investigate table.ancova negative values

*11. multinomial case, factors are the same across all choices, means different levels of the factor has the same probability estimation. will be cancelled in the final prob/sum

*12. add parameter:single_level to old BANOVA.~ models, so that summary/table.predictions can work

13. add loglikehood to compare various models including single level vs two level models

*14. trace.plot, conv.diag, floodlight, mediation, etc. other funcitons

*15. save compiling time: option 1: follow the methods of prophet, install.libs.R, and zzz.R; option 2: follow the brms package, remove the compiling warning message, but still compiling

*16. fix all cran check issues

*17. add BANOVA.run to all functions in  man files, retest ordMultinomial

===========================

v0.9.1 improvements:
  1. Add single level models for BANOVA.Normal and BANOVA.T
  2. remove unnecessary files (TODO) for cran publication

 
Backlog: 
10 and 13 above
check if a model has the following issue, the number of observations of each subject is too small.
