:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

% Base ontologies.

:- rdf_load('lib/schema/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).
:- rdf_load('lib/schema/model4I3.0.ttl', [format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).

% User ontologies, at Jena endpoint.

:- rdf_load('http://localhost:3030/drugs', [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]).
:- rdf_load('http://localhost:3030/transitions', [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]).
:- rdf_load('http://localhost:3030/beliefs', [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).

% Logic libraries

:- include('lib/logic/interactionRules').
:- include('lib/logic/interaction_graph').
