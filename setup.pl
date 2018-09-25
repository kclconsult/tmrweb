:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_load('https://www.dropbox.com/s/33v1zze5fpbmnzh/model.ttl?raw=1', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).
:- rdf_load('https://www.dropbox.com/s/7mfhm6ohlon2yj0/model4I%203.0.ttl?raw=1',[format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).
:- rdf_load('https://www.dropbox.com/s/ad95snt663lcbvc/CareAction%26DrugTypes.ttl?raw=1',[format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]).
:- rdf_load('https://www.dropbox.com/s/ayt89to51dakjsw/Transition%26SituationTypes.ttl?raw=1',[format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]).
:- rdf_load('https://www.dropbox.com/s/ofzeq00zrm1jy3h/CausationBeliefs-Nanopub.trig?raw=1 ', [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).
:- rdf_load('https://www.dropbox.com/s/fn6j5wqg8clgjfl/Reg%26Norms-Nanopub.trig?raw=1',[format('trig'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Reg&Norms-Nanopub')]).
:- rdf_load('https://www.dropbox.com/s/1ax2uerv5wzzrj5/MergedReg%26Norms-Nanopub.trig?raw=1',[format('trig'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/MergedReg&Norms-Nanopub')]).

:- include('../rapgmsbgym/lib/interactionRules').
:- include('../rapgmsbgym/lib/interaction_graph').

%:- inferInternalInteractions.

%:- guideline_recommendations(Guideline, Recommendations),maplist(recommendation_term, Recommendations, Terms),findall(interaction(Interaction,Label,Elems,External),interaction(Recommendations, Interaction, Label, Elems, External),Interactions).
