import rpy2.robjects as ro
import pandas as pd
from rpy2.robjects import pandas2ri
from hyperopt import fmin, tpe, hp

def objective (params):
    eta, max_depth, subsample, col_sample_bytree, min_child_weight, gamma = params

    print "eta: ", eta
    rParams = ro.ListVector ({
        'eta': ro.FloatVector ([eta]),
        'max_depth': ro.IntVector ([max_depth]),
        'subsample': ro.FloatVector ([subsample]),
        'col_sample_bytree': ro.FloatVector ([col_sample_bytree]),
        'min_child_weight': ro.FloatVector ([min_child_weight]),
        'gamma': ro.FloatVector ([gamma])})

    f1=open ('./hyperopt-results', "a")
    
    print "--------------New Trial---------------"
    print "eta: ", eta
    print "max_depth: ", max_depth
    print "subsample: ", subsample
    print "col_sample_bytree: ", col_sample_bytree
    print "min_child_weight: ", min_child_weight
    print "gamma: ", gamma

    print >> f1, "--------------New Trial---------------"
    print >> f1, "eta: ", eta
    print >> f1, "max_depth: ", max_depth
    print >> f1, "subsample: ", subsample
    print >> f1, "col_sample_bytree: ", col_sample_bytree
    print >> f1, "min_child_weight: ", min_child_weight
    print >> f1, "gamma: ", gamma

    ro.globalenv['params'] = rParams
    rmse = ro.r('fitFunc (fitFormula, trainData, params)')
    print (rmse[0])
    print >> f1, "Result is: ", rmse[0]
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
             max_evals= 1)

print best
            

