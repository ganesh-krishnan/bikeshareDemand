import rpy2.robjects as ro
import pandas as pd
from rpy2.robjects import pandas2ri
from hyperopt import fmin, tpe, hp
import itertools as it


def expand_grid(*args, **kwargs):
    columns = []
    lst = []
    if args:
        columns += xrange(len(args))
        lst += args
    if kwargs:
        columns += kwargs.iterkeys()
        lst += kwargs.itervalues()

    return pd.DataFrame(list(it.product(*lst)), columns=columns)
        
def objective (params):
    eta, max_depth, col_sample_bytree, min_child_weight, gamma = params

    f1=open ('./hyperopt-results', "a")
    
    print ("--------------New Trial---------------")
    print ("eta: ", eta)
    print ("max_depth: ", max_depth)
    print ("col_sample_bytree: ", col_sample_bytree)
    print ("min_child_weight: ", min_child_weight)
    print ("gamma: ", gamma)

    print >> f1, ("--------------New Trial---------------")
    print >> f1, ("eta: ", eta)
    print >> f1, ("max_depth: ", max_depth)
    print >> f1, ("col_sample_bytree: ", col_sample_bytree)
    print >> f1,  ("min_child_weight: ", min_child_weight)
    print >> f1,  ("gamma: ", gamma)

    df = expand_grid (nrounds = [50, 100, 150, 200],
                      max_depth = [max_depth],
                      col_sample_bytree = [col_sample_bytree],
                      min_child_weight = [min_child_weight],
                      gamma = [gamma])
    
    ro.globalenv['tuneGrid'] = df
    rmse = ro.r('fitFunc (fitFormula, train.df, method="xgbTree", tuneGrid=tunegrid, ctrl=ctrl)')
    print (rmse[0])
    print >>f1, ("Result is: ", rmse[0])
    f1.close()
    return (rmse[0])
    
    
space = (
    hp.loguniform ('eta', -6, 0),
    hp.randint ('max_depth', 6) + 2,
    hp.uniform ('col_sample_bytree', 0.5, 1),
    hp.uniform ('min_child_weight', 0, 3), 
    hp.uniform ('gamma', 0, 3)
    )
    
pandas2ri.activate()

ro.r('source ("xgbCaret-hyperopt.R")')

best = fmin (objective,
             space = space,
             algo = tpe.suggest,
             max_evals= 20)

print best
            

