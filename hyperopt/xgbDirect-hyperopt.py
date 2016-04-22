from __future__ import print_function
import rpy2.robjects as ro
import pandas as pd
from rpy2.robjects import pandas2ri
from hyperopt import fmin, tpe, hp

def objective (params):
    eta, max_depth, subsample, col_sample_bytree, min_child_weight, gamma = params

    rParams = ro.ListVector ({
        'eta': ro.FloatVector ([eta]),
        'max_depth': ro.IntVector ([max_depth]),
        'subsample': ro.FloatVector ([subsample]),
        'col_sample_bytree': ro.FloatVector ([col_sample_bytree]),
        'min_child_weight': ro.FloatVector ([min_child_weight]),
        'gamma': ro.FloatVector ([gamma])})

    f1=open ('./hyperopt-results', "a")
    
    print ("--------------New Trial---------------")
    print ("eta,{}".format(eta))
    print ("max_depth,{}".format (max_depth))
    print ("subsample,{}".format (subsample))
    print ("col_sample_bytree,{}".format (col_sample_bytree))
    print ("min_child_weight,{}".format (min_child_weight))
    print ("gamma,{}".format (gamma))

    print ("--------------New Trial---------------", file=f1)
    print ("eta,{}".format (eta), file=f1)
    print ("max_depth,{}".format (max_depth), file=f1)
    print ("subsample,{}".format (subsample), file=f1)
    print ("col_sample_bytree,{}".format (col_sample_bytree), file=f1)
    print ("min_child_weight,{}".format (min_child_weight), file=f1)
    print ("gamma,{}".format (gamma), file=f1)

    ro.globalenv['params'] = rParams
    rmse = ro.r('fitFunc (fitFormula, trainData, params)')
    print (rmse[0])
    print ("Result is,{}".format (rmse[0]), file=f1)
    f1.close()
    return (rmse[0])
    
    
space = (
    hp.loguniform ('eta', -6, 0),
    hp.randint ('max_depth', 6) + 2,
    hp.uniform ('subsample', 0.5, 1),
    hp.uniform ('col_sample_bytree', 0.5, 1),
    hp.uniform ('min_child_weight', 0, 3), 
    hp.uniform ('gamma', 0, 3)
    )
    
pandas2ri.activate()

ro.r('source ("xgbDirect-hyperopt.R")')
       
best = fmin (objective,
             space = space,
             algo = tpe.suggest,
             max_evals= 100)

print (best)
            

