:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

% Base ontologies.

:- rdf_load('tmr/schema/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).
:- rdf_load('tmr/schema/model4I3.0.ttl', [format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).

% User ontologies, at Jena endpoint.

loadOntologies() :-
  getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT),
  atom_concat(FUSEKI_HOST_PORT, "drugs", FUSEKI_DRUGS_URL),
  rdf_load(FUSEKI_DRUGS_URL, [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]),
  atom_concat(FUSEKI_HOST_PORT, "transitions", FUSEKI_TRANSITIONS_URL),
  rdf_load(FUSEKI_TRANSITIONS_URL, [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]),
  atom_concat(FUSEKI_HOST_PORT, "beliefs", FUSEKI_BELIEFS_URL),
  rdf_load(FUSEKI_BELIEFS_URL, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).

unloadOntologies() :-
  rdf_unload_graph('http://anonymous.org/CareAction&DrugTypes'),
  rdf_unload_graph('http://anonymous.org/Transition&SituationTypes'),
  rdf_unload_graph('http://anonymous.org/CausationBeliefs-Nanopub').

% Logic libraries

:- include('tmr/logic/interactionRules').
:- include('tmr/logic/interaction_graph').
