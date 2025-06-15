:- dynamic person/6.
:- dynamic married/2.


person(unknown, unknown, 'Murat Aslan', 1940, none, m).
person(unknown, unknown, 'Sedanur Aslan', 1942, 2015, f).
married('Murat Aslan', 'Sedanur Aslan').
married('Sedanur Aslan', 'Murat Aslan').

/* 
 Direct parent/child relationshipshalt.
 */

direct_parent_child('Anne', Mother, Child) :-
    person(_, Mother, Child, _, _, _).

direct_parent_child('Baba', Father, Child) :-
    person(Father, _, Child, _, _, _).

direct_parent_child('Ogul', Son, Parent) :-
    ( person(Parent, _, Son, _, _, m)
    ; person(_, Parent, Son, _, _, m)
    ).

direct_parent_child('Kiz', Daughter, Parent) :-
    ( person(Parent, _, Daughter, _, _, f)
    ; person(_, Parent, Daughter, _, _, f)
    ).

/* 
 Sibling & nephew - niece relationships
 */
 are_sibling(X, Y) :-
    person(F1, M1, X, _, _, _),
    person(F2, M2, Y, _, _, _),
    F1 \= unknown, M1 \= unknown,
    F1 == F2, M1 == M2,
    X \= Y.

sibling_relation(Label, P1, P2) :-
    person(F, M, P1, Birth1, _, Gender1),
    person(F, M, P2, Birth2, _, _),
    P1 \= P2,
    (F \= unknown ; M \= unknown),          % at least one parent known
    (  Birth1 < Birth2 ->
            ( Gender1 == m -> Label = 'Abi'
            ; Gender1== f -> Label = 'Abla' )
    ;
        Birth1 > Birth2 ->
            ( Gender1 == m -> Label = 'Erkek Kardes'
            ; Gender1 == f -> Label = 'Kiz Kardes' )
    ).

nephew_niece_relation('Yegen', P1, P2) :-
    % Sibling of P2 is parent of P1
    person(F, M, Sibling, _, _, _),
    person(F, M, P2,      _, _, _),
    Sibling \= P2,
    ( person(Sibling, _, P1, _, _, _)
    ; person(_, Sibling, P1, _, _, _)
    ).

/* 
 Spouse relationship
 */

spouse_relation('Es', P1, P2) :-
    married(P1, P2).

/* 
Aunt & Uncle relationships
 */

uncle_aunt_relation('Amca', P1, P2) :-
    person(GF, GM, Father, _, _, _),
    person(Father, _, P2, _, _, _),           % Father of P2
    person(GF, GM, P1, _, _, m),           % Male sibling of Father
    P1 \= Father.

uncle_aunt_relation('Hala', P1, P2) :-
    person(GF, GM, Father, _, _, _),
    person(Father, _, P2, _, _, _),
    person(GF, GM, P1, _, _, f),         % Female sibling of Father
    P1 \= Father.

uncle_aunt_relation('Dayi', P1, P2) :-
    person(GF, GM, Mother, _, _, _),
    person(_, Mother, P2, _, _, _),           % Mother of P2
    person(GF, GM, P1, _, _, m),           % Male sibling of Mother
    P1 \= Mother.

uncle_aunt_relation('Teyze', P1, P2) :-
    person(GF, GM, Mother, _, _, _),
    person(_, Mother, P2, _, _, _),
    person(GF, GM, P1, _, _, f),         % Female sibling of Mother
    P1 \= Mother.

/* 
Cousin relationship
 */

cousin_relation('Kuzen', P1, P2) :-
    % Find two parents that are siblings
    person(GF, GM, Parent1, _, _, _),
    person(GF, GM, Parent2, _, _, _),
    Parent1 \= Parent2,
    % Parent1 is parent of P1
    ( person(Parent1, _, P1, _, _, _)
    ; person(_, Parent1, P1, _, _, _) ),
    % Parent2 is parent of P2
    ( person(Parent2, _, P2, _, _, _)
    ; person(_, Parent2, P2, _, _, _) ),
    % Ensure we didn’t pick a direct sibling pair
    \+ are_sibling(P1, P2).

/* 
 Parent-in-law - Child-in-law relationships
 */

% Parent‑in‑law: P1 is the mother or father of P2’s spouse
parent_in_law_relation('Kayinvalide', P1, P2) :-
    married(P2, Child),
    person(_, P1, Child, _, _, _),           % P1 is mother of Child
    P1 \= P2.

parent_in_law_relation('Kayinpeder', P1, P2) :-
    married(P2, Child),
    person(P1, _, Child, _, _, _),           % P1 is father of Child
    P1 \= P2.

% child-in-law
child_in_law_relation('Gelin', P1, P2) :-
    married(P1, Child),
    ( person(P2, _, Child, _, _, _)        % P2 is father of Child
    ; person(_, P2, Child, _, _, _) ),     % or mother of Child
    person(_, _, P1, _, _, f),        % P1 must be female
    P1 \= P2.

child_in_law_relation('Damat', P1, P2) :-
    married(P1, Child),
    ( person(P2, _, Child, _, _, _)        % P2 is father of Child
    ; person(_, P2, Child, _, _, _) ),     % or mother of Child
    person(_, _, P1, _, _, m),          % P1 must be male
    P1 \= P2.

/* 
Sibling-in-law relationships
 */

% husband of someone's sister
sibling_in_law_relation('Eniste', P1, P2) :-
    person(F,M,Sis,_,_,_),
    person(F,M,P2, _,_,_),
    Sis \= P2,
    (F \= unknown ; M \= unknown),   % ensure parents are known → real siblings
    married(P1,Sis),
    person(_,_,P1, _,_, m).

% wife of someones brother  (requires at least one known parent)
sibling_in_law_relation('Yenge', P1, P2) :-
    person(F,M,Bro,_,_, m),
    person(F,M,P2, _,_,_),
    Bro \= P2,
    (F \= unknown ; M \= unknown),          % ensure real sibling link
    married(P1,Bro),
    person(_,_,P1, _,_, f).

% wife’s sister
sibling_in_law_relation('Baldiz', P1, P2) :-
    married(P2,Wife),
    person(F,M,Wife, _,_, f),
    person(F,M,P1,  _,_, f),
    P1 \= Wife.

% spouse’s brother
sibling_in_law_relation('Kayinbirader', P1, P2) :-
    married(P2,Spouse),
    person(F,M,Spouse, _,_,_),
    person(F,M,P1,    _,_, m),
    P1 \= Spouse.

% husbands of sisters
sibling_in_law_relation('Bacanak', P1, P2) :-
    person(_,_,P1, _,_, m),
    person(_,_,P2, _,_, m),
    P1 \= P2,
    married(P1,W1),
    married(P2,W2),
    person(F,M,W1, _,_, f),
    person(F,M,W2, _,_, f),
    W1 \= W2,
    \+ are_sibling(P1, P2).

% wives of brothers
sibling_in_law_relation('Elti', P1, P2) :-
    person(_,_,P1, _,_, f),
    person(_,_,P2, _,_, f),
    P1 \= P2,
    married(P1,H1),
    married(P2,H2),
    H1 \= H2,
    are_sibling(H1, H2).

/* 
   ask_relation(+P1,+P2)
*/
ask_relation(P1, P2) :-
    direct_parent_child(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    spouse_relation(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    parent_in_law_relation(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    child_in_law_relation(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    sibling_in_law_relation(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    sibling_relation(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    uncle_aunt_relation(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    nephew_niece_relation(Label, P1, P2), !,
    writeln(Label).

ask_relation(P1, P2) :-
    cousin_relation(Label, P1, P2), !,
    writeln(Label).

show_relation :-
    writeln('please type first person name:'),
    read(P1),
    writeln('please type second person name :'),
    read(P2),
    (   ask_relation(P1, P2)
    ->  true
    ;   writeln('No direct parent/child relationship found between these two individuals.')
    ).
main :-
    loop_entry.

loop_entry:-    
    writeln('1-)Ask relation'),
    writeln('2-)Add/Update person'),
    writeln('3-)Get information of any person'),
    writeln('4-)print the family tree'),
    writeln('5-)Add marriage'),
    writeln('6-)Terminate the program'),
    nl,
    writeln('Please choose an operation!'),
    read(Choose),
    (
        Choose==1->show_relation(),loop_entry; %++ %elman
        Choose==2->
        writeln('1-)Add person'),
        writeln('2-)Update person'),
        writeln('Please choose an operation!'),
        read(Choose1),
        (
            Choose1==1->
                writeln('please type the father name and surname:'),read(Father),
                writeln('please type the mother name and surname:'),read(Mother),
                writeln('please type the child name and surname:'),read(Name),
                writeln('please type the birthdate of the child (YYYY format): '),read(Birth),
                writeln('please type the death date of the child (YYYY format)(if did not died none) :'),read(Death),
                writeln('please type the child person gender(male/female):'),read(Gender),
                add_person(Father,Mother,Name,Birth,Death,Gender), %++ %esma
                loop_entry
            ;
            Choose1==2->
                writeln('1. Update the birth year of someone.'),
                writeln('2. Update the death year of someone.'),                          
                writeln('0. Cancel.'),
                read(Choose2),
                (
                    Choose2==1->
                        writeln('Enter the name and surname of person that you want to update: '),read(Name),
                        writeln('Enter the new birth year: '),read(NewBirthYear),
                        update_birth(Name,NewBirthYear), %++ %elman
                        loop_entry
                    ;

                    Choose2==2->
                        writeln('Enter the name and surname of person that you want to update: '),read(Name),
                        writeln('Enter the new death year: '),read(NewDeathYear),
                        update_death(Name,NewDeathYear), %++ %elman
                        loop_entry
                    ;

                    Choose2==0->
                        writeln('Cancelled.'),loop_entry
                    ;

                    writeln('Invalid choice! Please enter 0, 1 or 2.'),
                    loop_entry
                )
            ;
            writeln('Invalid choice! Please enter 1 or 2.'),loop_entry

        );

        Choose==3->
            writeln('please type the name and surname:'),read(Name),
            get_information(Name), %++ 
            loop_entry
        ;
            
        Choose==4->
            print_tree,loop_entry; %++ 

        Choose==5->
            writeln('name of the first person :'),read(FirstPerson),
            writeln('name of the second person :'),read(SecondPerson),
            add_marriage(FirstPerson,SecondPerson), %++ 
            loop_entry
        ;

        Choose==6-> 
            writeln('Program terminated. Goodbye!'), halt
        ;
        writeln('Invalid choice! Please enter a number between 1-6.'),loop_entry
    ).


add_person(Father, Mother, Name, Birth, Death, Gender) :-
    (   person(_, _, Name, _, _, _)               
    ->  writeln('This person already exists')
    ;   Death \= none,
        Death < Birth                          
    ->  writeln('Death year cannot be earlier than birth year')
    ;   \+ check_parents_dates(Father, Mother, Birth)
    ->  true                                      
    ;   assertz(person(Father, Mother, Name,
                       Birth, Death, Gender)),    
        writeln('Person successfully added.')
    ).


check_parents_dates(Father,Mother,Birth):-
    (Father \= unknown, person(_, _, Father, FatherBirth, FatherDeath, _) ->
        (Birth =< FatherBirth + 18 -> 
            writeln('Error: Father is too young when child was born'), fail; true),
        (FatherDeath \= none, Birth > FatherDeath ->
            writeln(' Child born after father died'), fail; true)
    ; true),
    (Mother \= unknown, person(_, _, Mother, MotherBirth, MotherDeath, _) ->
        (Birth =< MotherBirth + 18 -> 
            writeln(' Mother is too young when child was born'), fail; true),
        (MotherDeath \= none, Birth > MotherDeath ->
            writeln(' Child born after mother died'), fail; true)
    ; true).  


print_tree :-
    writeln('--- Family Tree ---'),
    findall(N, person(_,_,N,_,_,_), Ns),
    compute_all_levels(Ns, [], Map),
    print_by_levels(Map, 0).

compute_all_levels([], L, L).
compute_all_levels([N|Rest], Acc, Fin) :-
    ( member(N-_,Acc) ->
        compute_all_levels(Rest, Acc, Fin)
    ; compute_level(N,Lv),
      compute_all_levels(Rest,[N-Lv|Acc],Fin)
    ).

print_persons_couples([], _).

print_persons_couples([N|Rest], Seen) :-
    (   memberchk(N, Seen)                          % already printed
    ->  print_persons_couples(Rest, Seen)
    ;   married(N, S), N \= S,
        memberchk(S, Rest)                          % spouse also here
    ->  format('~w - ~w~n', [N, S]),                % show as a couple
        print_persons_couples(Rest, [N,S|Seen])
    ;   format('~w~n', [N]),                        % single print
        print_persons_couples(Rest, [N|Seen])
    ).

print_by_levels(Map, Gen) :-
    include(match_level(Gen), Map, Cur),
    Cur \= [], !,
    format('--- Level ~w ---~n', [Gen]),
    maplist(add_birth, Cur, BPairs),
    keysort(BPairs, Sorted),
    pairs_values(Sorted, Names),
    print_persons_couples(Names,[]),
    Next is Gen+1,
    print_by_levels(Map, Next).
print_by_levels(_, _).

match_level(G, _N-L) :- L=:=G.
add_birth(Name-_, Birth-Name) :- person(_,_,Name,Birth,_,_).

root_person(Name) :-
    person(F, M, Name, _, _, _),
    (F == unknown ; \+ person(_,_,F,_,_,_)),
    (M == unknown ; \+ person(_,_,M,_,_,_)).

% public wrapper (2-arg version kept for existing code)
compute_level(Name, Level) :- compute_level(Name, Level, []).

% (1) founder couple – both spouses also founders → level 0
compute_level(Name, 0, _) :-
    root_person(Name),
    (   \+ married(Name, _)
    ;   married(Name, Sp),
        root_person(Sp)
    ), !.

% (2) founder married to someone with known parents → use spouse’s level
compute_level(Name, Level, Vis) :-
    root_person(Name),
    married(Name, Sp),
    \+ memberchk(Name, Vis),
    \+ root_person(Sp),                 
    compute_level(Sp, Level, [Name|Vis]), !.

% (3) normal rule: max(parent levels) + 1
compute_level(Name, Level, Vis) :-
    person(F,M,Name,_,_,_),
    find_parent_level(F, L1, [Name|Vis]),
    find_parent_level(M, L2, [Name|Vis]),
    MaxP is max(L1,L2),
    Level is MaxP + 1.

% helpers 
find_parent_level(unknown, -1, _) :- !.
find_parent_level(Name, -1, _)    :- \+ person(_,_,Name,_,_,_), !.
find_parent_level(Name, L, Vis)   :- compute_level(Name, L, Vis).

% 2-arg wrapper
find_parent_level(unknown,-1) :- !.
find_parent_level(Name,-1)    :- \+ person(_,_,Name,_,_,_), !.
find_parent_level(Name,L)     :- find_parent_level(Name,L,[]).



ensure_person_exists(Name, existing) :-                     % already present
    person(_,_,Name,_,_,_), !.

ensure_person_exists(Name, fresh) :-                        % ask then assert
    writeln('Birth year (YYYY):'),         read(B),
    writeln('Death year (YYYY or none):'), read(D),
    writeln('Gender (m/f):'),      read(G),
    assertz(person(unknown, unknown, Name, B, D, G)).


rollback(fresh,  Name) :- retractall(person(_,_,Name,_,_,_)).
rollback(existing,_).


is_married_to_someone_else(Person) :-
    married(Person, Spouse),
    Spouse \= unknown.


% --- YENİ VE DÜZELTİLMİŞ add_marriage ---

% Bu kural, tüm ön kontrolleri yapar.
add_marriage(P1, P2) :-
    ensure_person_exists(P1, Flag1),
    ensure_person_exists(P2, Flag2),

    % Hata kontrolleri
    (   ( P1 == P2 ->
            Msg = 'Error: A person cannot marry themselves.'
        ;   (underage(P1,A1), A1 < 18) ->
            format(atom(Msg), 'INVALID MARRIAGE: ~w is under 18 (Age: ~w).', [P1,A1])
        ;   (underage(P2,A2), A2 < 18) ->
            format(atom(Msg), 'INVALID MARRIAGE: ~w is under 18 (Age: ~w).', [P2,A2])
        ;   (married(P1,P2) ; married(P2,P1)) ->
            Msg = 'Error: These two are already married.'
        ;   is_married_to_someone_else(P1) ->
            format(atom(Msg), 'Error: ~w is already married to someone else.', [P1])
        ;   is_married_to_someone_else(P2) ->
            format(atom(Msg), 'Error: ~w is already married to someone else.', [P2])
        ;   person(_,_,P1,_,_,G1), person(_,_,P2,_,_,G2), G1 == G2 ->
            Msg = 'Error: Spouses must be of different genders.'
        ;   forbidden_marriage_relation(P1,P2,Rel) ->
            format(atom(Msg), 'INVALID MARRIAGE: The relationship is ~w.', [Rel])
        )
    ->  % Eğer yukarıdaki kontrollerden BİRİ bile başarılı olursa (yani bir HATA VARSA):
        rollback(Flag1, P1), % Yeni eklenen kişileri geri al
        rollback(Flag2, P2),
        writeln(Msg)         % Hata mesajını yazdır
    ;   % Eğer HİÇBİR hata bulunamazsa, bu ELSE bloğu çalışır:
        perform_actual_marriage(P1, P2)
    ).

% Bu kural, sadece evliliği gerçekleştirir ve başarı mesajı verir.
perform_actual_marriage(P1, P2) :-
    assertz(married(P1, P2)),
    assertz(married(P2, P1)),
    writeln('Marriage successful!').

underage(Name, Age) :-
    person(_,_,Name, Birth, Death, _),
    current(CY),
    ( Death == none -> Age is CY - Birth ; Age is Death - Birth ).


forbidden_marriage_relation(P1,P2,'parent/child') :-
    is_parent_child(P1,P2) ; is_parent_child(P2,P1).

forbidden_marriage_relation(P1,P2,'siblings') :-
    are_siblings(P1,P2).

forbidden_marriage_relation(P1,P2,'uncle/aunt & niece/nephew') :-
    is_uncle_aunt(P1,P2) ; is_uncle_aunt(P2,P1).

forbidden_marriage_relation(P1,P2,'grandparent/grandchild') :-
    is_grandparent(P1,P2) ; is_grandparent(P2,P1).

% Relationship checking predicates
is_parent_child(Parent, Child) :-
    person(Parent, _, Child, _, _, _).
is_parent_child(Parent, Child) :-
    person(_, Parent, Child, _, _, _).

are_siblings(P1, P2) :-
    person(F, M, P1, _, _, _),
    person(F, M, P2, _, _, _),
    P1 \= P2,
    (F \= unknown ; M \= unknown).

is_uncle_aunt(P1, P2) :-
    % P1 is uncle/aunt of P2
    person(F, M, Parent, _, _, _),
    (F \= unknown ; M \= unknown),
    person(Parent, _, P2, _, _, _),
    person(F, M, P1, _, _, _),
    P1 \= Parent.
is_uncle_aunt(P1, P2) :-
    % P1 is uncle/aunt of P2 (through mothers side)
    person(F, M, Parent, _, _, _),
    (F \= unknown ; M \= unknown),
    person(_, Parent, P2, _, _, _),
    person(F, M, P1, _, _, _),
    P1 \= Parent.

is_grandparent(GP, GC) :-
    person(GP, _, Parent, _, _, _),          
    (   person(Parent, _, GC, _, _, _)       
    ;   person(_, Parent, GC, _, _, _) ).    

is_grandparent(GP, GC) :-
    person(_, GP, Parent, _, _, _),          
    (   person(Parent, _, GC, _, _, _)       
    ;   person(_, Parent, GC, _, _, _) ).



% --- Update birth year ---
update_birth(Name, NewBirthYear) :-
    person(Father, Mother, Name, OldBirth, Death, Gender),
    ( validate_birth_year(Name, NewBirthYear, Death) ->
        retract(person(Father, Mother, Name, OldBirth, Death, Gender)),
        assertz(person(Father, Mother, Name, NewBirthYear, Death, Gender)),
        format('Birth year of ~w has been updated to ~w.~n', [Name, NewBirthYear])
    ; writeln('Invalid birth year! Update failed.')
    ).
update_birth(Name, _) :-
    format('No person found with the name ~w.~n', [Name]).

% --- Update death year ---
update_death(Name, NewDeathYear) :-
    person(Father, Mother, Name, Birth, OldDeath, Gender),
    !,
    ( validate_death_year(Name, Birth, NewDeathYear) ->
        retract(person(Father, Mother, Name, Birth, OldDeath, Gender)),
        assertz(person(Father, Mother, Name, Birth, NewDeathYear, Gender)),
        format('Death year of ~w has been updated to ~w.~n', [Name, NewDeathYear])
    ; writeln('Invalid death year! Update failed.')
    ).
update_death(Name, _) :-
    format('No person found with the name ~w.~n', [Name]).

% --- Validations ---

validate_birth_year(_Name, NewBirthYear, Death) :-
    current(CY),
    NewBirthYear =< CY,
    ( Death == none ; Death > NewBirthYear ).

validate_death_year(_Name, Birth, NewDeathYear) :-
    Birth < NewDeathYear,
    current(CY),
    NewDeathYear =< CY.

current(Year) :-
    get_time(T), format_time(atom(A), '%Y', T), atom_number(A, Year).

get_information(Name) :-
    person(_,_,Name,Birth,Death,Gender),
    !,
    ( Death == none -> current(CY), Age is CY - Birth
    ; Age is Death - Birth ),
    format('Age: ~w~n', [Age]),
    find_level(Name, L), format('Level: ~w~n', [L]),
    print_num_of_child(Name),
    ( Gender == m -> writeln('Gender: m') ; writeln('Gender: f') ),
    ( Death == none -> writeln('Status: Alive')
    ; writeln('Status: Dead') ).

get_information(Name) :-
    format('No person found with the name ~w.~n', [Name]).


print_num_of_child(Name) :-
    findall(
      Child,
      ( person(Name,_,Child,_,_,_)   % baba
      ; person(_,Name,Child,_,_,_) ),% anne
      L),
    length(L, Count),
    format('Total children: ~w~n', [Count]).


print_age(Name):-
    person(_,_,Name,Birth,Death,_),
    (Death== none ->
        current(CurrentYear),
        Age is CurrentYear - Birth,
        format("Age: ~w~n", [Age])
    ;
        Age is Death - Birth,
        format("Age: ~w~n", [Age])
    ).

print_level(Name):-
    find_level(Name,Level),
    format("Level = ~w~n",[Level]).


find_level(Name, Level) :- compute_level(Name, Level).


 
