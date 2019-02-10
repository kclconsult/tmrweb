:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

% Base ontologies.

:- rdf_load('tmr/schema/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).
:- rdf_load('tmr/schema/model4I3.0.ttl', [format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).

% User ontologies, at Jena endpoint.

loadOntologies() :-
  rdf_load('http://localhost:3030/drugs', [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]),
  rdf_load('http://localhost:3030/transitions', [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]),
  rdf_load('http://localhost:3030/beliefs', [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).

unloadOntologies() :-
  rdf_unload_graph('http://anonymous.org/CareAction&DrugTypes'),
  rdf_unload_graph('http://anonymous.org/Transition&SituationTypes'),
  rdf_unload_graph('http://anonymous.org/CausationBeliefs-Nanopub').

% Logic libraries

:- include('tmr/logic/interactionRules').
:- include('tmr/logic/interaction_graph').
