:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).

server :-
    server(80).
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

user:file_search_path(assets, assets).
http:location(assets, root(assets), []).

:- html_resource(assets('style.css'), []).

:- http_handler(root(.), todo_handler(Method), [method(Method), methods([get, post])]).
:- http_handler(root(evac), evac_all_handler(post), [methods([post])]).
:- http_handler(root(evac/ItemID), evac_handler(post, ItemID), [methods([post])]).
:- http_handler(root(mute/ItemID), mute_handler(post, ItemID), [methods([post])]).
:- http_handler(root(amp/ItemID), amp_handler(post, ItemID), [methods([post])]).

todo_handler(get, _) :-
    todo_page.
todo_handler(post, Request) :-
    http_parameters(Request, [], [form_data([engram=Engram])]),
    normalize_space(atom(Trimmed), Engram),
    todo_handler_post(Trimmed).

todo_handler_post('') :-
    todo_page(i(['Some fleeting thought skitters past my mind\'s eye, just out of reach...'])),
    !.
todo_handler_post(Engram) :-
    uuid(ItemID),
    item_asserta(ItemID-f-Engram),
    todo_page(i(['A fey thread joins the chorus: ', Engram])).

evac_all_handler(post, _) :-
    item_retractall(_),
    todo_page(i(['The song escapes! I almost had it...'])).

evac_handler(post, ItemID, _) :-
    item_retractall(ItemID-_-_),
    todo_page(i(['Forgotten threads, ancient webs in dark corners. I no longer recall.'])),
    !.
evac_handler(post, _, _) :-
    session_conflict.

mute_handler(post, ItemID, _) :-
    once(item(ItemID-f-Engram)),
    item_retractall(ItemID-f-Engram),   % try a detailed match to reduce odds of removing a mysterious wrong dupe
    item_assert(ItemID-t-Engram),
    todo_page(i(['Silence, child.'])),
    !.
mute_handler(post, _, _) :-
    session_conflict.

amp_handler(post, ItemID, _) :-
    once(item(ItemID-t-Engram)),
    item_retractall(ItemID-t-Engram),   % try a detailed match to reduce odds of removing a mysterious wrong dupe
    item_asserta(ItemID-f-Engram),
    todo_page(i(['I do believe.. yes. Yes.. that\'s right.'])),
    !.
amp_handler(post, _, _) :-
    session_conflict.

session_conflict :-
    todo_page(i(['Another me? Another dimension?'])).

todo_page :-
    todo_page([]).
todo_page(Flashbag) :-
    todo_list_view(Todo),
    reply_html_page(
        [
            title('TODOlog')
        ],
        [
            \html_requires(assets('style.css')),
            h1('TODOlog'),
            div(Flashbag),
            div(Todo),
            form([action=location_by_id(todo_handler), method=post], [
                input([type=text, id=engram, name=engram]),
                input([type=submit, value='Commit to Memory'])
            ]),
            form([action=location_by_id(evac_handler), method=post], [
                input([type=submit, value='Evacuate All Notions'])
            ])
        ]
    ).
    
todo_list_view(Out) :-
    todo_list(L),
    todo_list_view(L, Out).
todo_list_view([], i('Wherefore shall I serve?')).
todo_list_view(L, ul(Out)) :-
    L = [_|_],
    maplist(todo_item_view, L, Out).

%! todo_item_view(ItemID-Muted-Item, Out) is semidet
todo_item_view(ItemID-t-Item, Out) :-
    Out = li([
        del(Item),
        form([action=location_by_id(amp_handler(ItemID)), method=post], [
            input([type=submit, value='Amplify'])
        ]),
        form([action=location_by_id(evac_handler(ItemID)), method=post], [
            input([type=submit, value='Evacuate'])
        ])
    ]).
todo_item_view(ItemID-f-Item, Out) :-
    Out = li([
        Item,
        form([action=location_by_id(mute_handler(ItemID)), method=post], [
            input([type=submit, value='Mute'])
        ]),
        form([action=location_by_id(evac_handler(ItemID)), method=post], [
            input([type=submit, value='Evacuate'])
        ])
    ]).

todo_list(L) :-    findall(X, item(X), L),    !.
todo_list([]).

item(X) :-              http_session_data(item(X)).
item_assert(X) :-       http_session_assert(item(X)).
item_asserta(X) :-      http_session_asserta(item(X)).
item_retractall(X) :-   http_session_retractall(item(X)).
