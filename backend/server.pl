:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

:- rdf_prefix(vocab, 'http://anonymous.org/vocab/').

:- http_handler(root(guidelines), get_available_guidelines, []).
:- http_handler(root(interactions), show_interactions, []).
:- http_handler(root(drug), show_drug, []).
:- http_handler(root(drugadministration), show_drug_administration, []).

:- set_prolog_flag(color_term,false).

:- consult(setup).
:- consult(util).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

get_available_guidelines(_Request) :-
        directory_files('lib/ontologies/guidelines', Files),
        format('Content-type: text/plain~n~n'),
        sort(Files, SortedFiles),
        remove_head(SortedFiles, SortedFilesWithoutHead),
        remove_head(SortedFilesWithoutHead, Guidelines),
        maplist(remove_file_extension, Guidelines, GuidelineNames),
        print_list(GuidelineNames).

show_interactions(Request) :-
        http_parameters(Request, [ guideline(GuidelineID, [ string ]) ]),
        atom_concat('http://localhost:3030/', GuidelineID, MainGuidelinePath),
        atom_concat('http://anonymous.org/', GuidelineID, GuidelineGraphPath),
        rdf_load(MainGuidelinePath, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph(GuidelineGraphPath)]),
        inferInternalInteractions,
        format('Content-type: text/plain~n~n'),
        atom_concat('data:', GuidelineID, DataGuidelineID),
        %TODO: rdf_global_id(DataGuidelineID, Guideline),
        rdf_global_id(data:'CIG-HT', Guideline),
        guideline_recommendations(Guideline, Recommendations),
        maplist(recommendation_term, Recommendations, Terms),
        findall(interaction(Interaction,Label,Elems,External), interaction(Recommendations, Interaction, Label, Elems, External), Interactions),
        print_list(Interactions),
        rdf_unload_graph(GuidelineGraphPath).

show_drug_administration(Request) :-
        member(method(post), Request),
        !,
        http_read_data(Request, Data, []),
        format('Content-type: text/plain~n~n', []),
        rdf(Data, vocab:'aboutExecutionOf', DrugAdministration),
	      rdf(DrugAdministration, vocab:'causes', Transition),
	      string_concat(' causes ', Transition, Join1),
	      string_concat(DrugAdministration, Join1, Join2),
        format(Join2).

show_drug(Request) :-
        member(method(post), Request),
        !,
        http_read_data(Request, Data, []),
        format('Content-type: text/plain~n~n', []),
	      rdf(Data, vocab:'aboutExecutionOf', DrugAdministration),
	      rdf(DrugAdministration, vocab:'administrationOf', Drug),
        format(Drug).
