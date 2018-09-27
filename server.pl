:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).

:- http_handler(root(guidelines), get_available_guidelines, []).
:- http_handler(root(interactions), show_interactions, []).
:- http_handler(root(drug), show_drug, []).
:- http_handler(root(drugadministration), show_drug_administration, []).

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
        string_concat('lib/ontologies/guidelines/', GuidelineID, GuidelinePath),
        string_concat(GuidelinePath, '.trig', MainGuidelineFile),
        string_concat('lib/ontologies/other/', GuidelineID, MergedGuidelinePath),
        string_concat(MergedGuidelinePath, '_merged.trig', MergedGuidelineFile),
        atom_concat('http://anonymous.org/', GuidelineID, GuidelineGraphPath),
        atom_concat('http://anonymous.org/merged_', GuidelineID, MergedGuidelineGraphPath),
        rdf_load(MainGuidelineFile, [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph(GuidelineGraphPath)]),
        rdf_load(MergedGuidelineFile, [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph(MergedGuidelineGraphPath)]),
        inferInternalInteractions,
        format('Content-type: text/plain~n~n'),
        rdf_global_id(data:'CIG-OA-HT-DB',Guideline),
        guideline_recommendations(Guideline, Recommendations),
        maplist(recommendation_term, Recommendations, Terms),
        findall(interaction(Interaction,Label,Elems,External), interaction(Recommendations, Interaction, Label, Elems, External), Interactions),
        print_list(Interactions).

show_drug_administration(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
        format('Content-type: text/plain~n~n', []),
        rdf(Data, vocab:'aboutExecutionOf', DrugAdministration),
	      rdf(DrugAdministration, vocab:'causes', Transition),
	      string_concat(' causes ', Transition, Join1),
	      string_concat(DrugAdministration, Join1, Join2),
        format(Join2).

show_drug(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
        format('Content-type: text/plain~n~n', []),
	      rdf(Data, vocab:'aboutExecutionOf', DrugAdministration),
	      rdf(DrugAdministration, vocab:'administrationOf', Drug),
        format(Drug).
