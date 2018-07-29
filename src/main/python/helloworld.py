import sys
from pyswip import Prolog

def helloworld():
    prolog = Prolog();
    prolog.assertz("father(michael,john)")
    prolog.assertz("father(michael,gina)")
    list(prolog.query("father(michael,X)")) == [{'X': 'john'}, {'X': 'gina'}]
    for soln in prolog.query("father(X,Y)"):
        print(soln["X"], "is the father of", soln["Y"])
    
helloworld();