% -*- mode: prolog-mode -*- %

% swipl
% ['haversack.prolog'].
% make.
% example.

:- use_module(library(clpfd)).

%% sentence --> bagPhrase, " ", contains, " ", containsPhrase, ".".
%% bagPhrase --> bagType "bags".
%% contains --> "contain".


haversack(File):-
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, ),
    writeln(String),
    split_string(String, " contain ", "", L),
    writeln(L),
    close(Stream).

example:- haversack("example").
