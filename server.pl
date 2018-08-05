:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).

:- http_handler(root(hello_world), say_hi, []).     % (1)

server(Port) :-                     % (2)
        http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-                 % (3)
        rdf_load('https://www.dropbox.com/s/33v1zze5fpbmnzh/model.ttl?raw=1', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]),
        format('Content-type: text/plain~n~n'),
        format('Hello World!~n').