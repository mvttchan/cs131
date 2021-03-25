tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    length(T, N),
    set_tower(N, T),
    transpose(T, TR),
    set_tower(N, TR),
    maplist(fd_labeling, T),
    final_check(T, TR, Top, Bottom, Left, Right).

%Thank you stackoverflow
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

set_tower(_, []).
set_tower(N, [H|T]) :-
    fd_unordered_range(N, H),
    set_tower(N, T).

%From TA, commented that last part out because it made it really slow, like super slow
fd_unordered_range(N, Res) :-
    length(Res, N),
    fd_domain(Res, 1, N),
    fd_all_different(Res).
    %fd_labeling(Res).

final_check(T, TR, Top, Bottom, Left, Right) :-
    check_helper(T, Left),
    maplist(reverse, T, T_reversed),
    check_helper(T_reversed, Right),

    check_helper(TR, Top),
    maplist(reverse, TR, TR_reversed),
    check_helper(TR_reversed, Bottom).

check_helper([], []).
check_helper([Row|Tail], [Count|Count_tail]) :-
    check_subhelper(Row, Count, 0, 0),
    check_helper(Tail, Count_tail).

check_subhelper([], Count, Acc, _) :-
    Count = Acc.
check_subhelper([Head|Tail], Count, Acc, Bookmark) :-
    (Bookmark < Head -> Inc is Acc + 1,
                        check_subhelper(Tail, Count, Inc, Head);
     check_subhelper(Tail, Count, Acc, Bookmark)).

%-----------------------------------------------------------------

generate_start(0, []).
generate_start(N, [Hd|Tl]) :-
    Hd = N,
    X is N - 1,
    generate_start(X, Tl).

plain_tower_constraints(_, [], [], _).
plain_tower_constraints(Canvas, [Product|Rest], [Edge|Tl], N) :-
    permutation(Canvas, Product),
    between(1, N, Edge),
    plain_tower_check(Product, 0, Edge),
    plain_tower_constraints(Canvas, Rest, Tl, N).

plain_tower_check([], _, 0).
plain_tower_check([Hd|Tl], Highest, Visible) :-
    Hd > Highest ->
        Visible_dec is Visible - 1,
        plain_tower_check(Tl, Hd, Visible_dec);
    plain_tower_check(Tl, Highest, Visible).

final_plain(Canvas, T, Top, Bottom, Left, Right, N) :-
    plain_tower_constraints(Canvas, T, Left, N),
    maplist(reverse, T, T_reversed),
    plain_tower_constraints(Canvas, T_reversed, Right, N),
    transpose(T, TR),
    plain_tower_constraints(Canvas, TR, Top, N),
    maplist(reverse, TR, TR_reversed),
    plain_tower_constraints(Canvas, TR_reversed, Bottom, N).

plain_tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    length(T, N),

    generate_start(N, Canvas),
    final_plain(Canvas, T, Top, Bottom, Left, Right, N).

%-------------------------------------------------------------

speedup(Time) :-
    test1(T),
    test2(Plain_T),
    Time is Plain_T/T.

test1(Time) :-
    statistics(cpu_time, [Begin|_]),
    tower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End|_]),
    Time is (End - Begin).

test2(Time) :-
    statistics(cpu_time, [Begin|_]),
    plain_tower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End|_]),
    Time is (End - Begin).

%-----------------------------------------

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.
