:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).

:- http_handler(root(interactions), show_interactions, []).     % (1)

server(Port) :-                     % (2)
        consult(setup),
        http_server(http_dispatch, [port(Port)]).

show_interactions(_Request) :-                 % (3)
 	inferInternalInteractions,
	format('Content-type: text/plain~n~n'),
	rdf_global_id(data:'CIG-OA-HT-DB',Guideline),guideline_recommendations(Guideline, Recommendations),maplist(recommendation_term, Recommendations, Terms),findall(interaction(Interaction,Label,Elems,External),interaction(Recommendations, Interaction, Label, Elems, External),Interactions),with_output_to(codes(Codes), write(Interactions)), format("~s", [Codes]).
