:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_load('/home/martin/CONSULT/Guidelines/dev/rapgmsbgym/schemas/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).
:- rdf_load('/home/martin/CONSULT/Guidelines/dev/rapgmsbgym/schemas/model4I3.0.ttl',[format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).
:- rdf_load('/home/martin/CONSULT/Guidelines/dev/rapgmsbgym/guidelines/CareActionDrugTypes.ttl',[format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]).
:- rdf_load('/home/martin/CONSULT/Guidelines/dev/rapgmsbgym/guidelines/TransitionSituationTypes.ttl',[format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]).
:- rdf_load('/home/martin/CONSULT/Guidelines/dev/rapgmsbgym/guidelines/CausationBeliefsNanopub.trig', [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).
:- rdf_load('/home/martin/CONSULT/Guidelines/dev/rapgmsbgym/guidelines/RegNormsNanopub.trig',[format('trig'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Reg&Norms-Nanopub')]).
:- rdf_load('/home/martin/CONSULT/Guidelines/dev/rapgmsbgym/guidelines/MergedRegNormsNanopub.trig',[format('trig'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/MergedReg&Norms-Nanopub')]).

:- include('../rapgmsbgym/lib/interactionRules').
:- include('../rapgmsbgym/lib/interaction_graph').

%:- inferInternalInteractions.

%:- guideline_recommendations(Guideline, Recommendations),maplist(recommendation_term, Recommendations, Terms),findall(interaction(Interaction,Label,Elems,External),interaction(Recommendations, Interaction, Label, Elems, External),Interactions).
