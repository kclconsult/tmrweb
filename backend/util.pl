print_list(List) :-
    with_output_to(codes(Codes), write(List)), format("~s", [Codes]).

remove_head([_|Tail], Tail).

without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :-
    without_last(Xs, WithoutLast).

remove_file_extension(File, Filename) :-
    split_string(File, '.', '', FileComponents),
    nth0(0, FileComponents, Filename).
