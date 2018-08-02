import sys
from pyswip import Prolog

def helloworld():
    prolog = Prolog();
    prolog.assertz("use_module(library(semweb/turtle))");
    prolog.assertz("use_module(library(semweb/rdf_http_plugin))");
    prolog.assertz("use_module(library(semweb/rdf_db))");
    
    for soln in prolog.query("rdf_load('https://www.dropbox.com/s/33v1zze5fpbmnzh/model.ttl?raw=1', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')])"):
        print(soln);
    
helloworld();