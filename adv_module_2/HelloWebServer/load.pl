:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), say_hi, []).

say_hi(_) :-
    reply_html_page(
        [title('Howdy')],
        [h1('Hi'),
        p('Greetings, stranger.')]).
