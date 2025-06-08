:- dynamic person/6.
:- dynamic married/2.

:- discontiguous direct_relationship/3.

person(unknown, unknown, 'Ahmet Arslan', 1940, none, male).
person(unknown, unknown, 'Fatma Arslan', 1945, 2015, female).
married('Ahmet Arslan', 'Fatma Arslan').
married('Fatma Arslan', 'Ahmet Arslan').
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
            get_information(Name), %++ %ahmet
            loop_entry
        ;
            
        Choose==4->
            print_tree,loop_entry; %++ %esma

        Choose==5->
            writeln('name of the first person :'),read(FirstPerson),
            writeln('name of the second person :'),read(SecondPerson),
            add_marriage(FirstPerson,SecondPerson), %++ %ahmet
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
    ->  format('~w ⇄ ~w~n', [N, S]),                % show as a couple
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

% helpers -------------------------------------------------
find_parent_level(unknown, -1, _) :- !.
find_parent_level(Name, -1, _)    :- \+ person(_,_,Name,_,_,_), !.
find_parent_level(Name, L, Vis)   :- compute_level(Name, L, Vis).

% 2-arg wrapper
find_parent_level(unknown,-1) :- !.
find_parent_level(Name,-1)    :- \+ person(_,_,Name,_,_,_), !.
find_parent_level(Name,L)     :- find_parent_level(Name,L,[]).



% marriage deneme 
add_marriage(P1, P2) :-
    % Step 1: Ensure both people exist in the database.
    % If not, ask for their details and add them.
    ensure_person_exists(P1),
    ensure_person_exists(P2),

    % Step 2: Perform marriage validations.
    % We use the (->;) operator for a clean if-then-else logic.
    (   P1 == P2 ->
        writeln('Error: A person cannot marry themselves.')
    ;   (married(P1, P2) ; married(P2, P1)) ->
        writeln('Error: These two are already married.')
    ;   is_married_to_someone_else(P1) -> % Check if P1 is married to someone else
        format('Error: ~w is already married to someone else.', [P1]), nl
    ;   is_married_to_someone_else(P2) -> % Check if P2 is married to someone else
        format('Error: ~w is already married to someone else.', [P2]), nl
    ;   person(_,_,P1,_,_,G1), person(_,_,P2,_,_,G2), G1 == G2 ->
        writeln('Error: Spouses must be of different genders.')
    ;   forbidden_marriage_relation(P1, P2, Relation) ->
        format('INVALID MARRIAGE: The relationship is ~w.', [Relation]), nl
    ;   (underage(P1, Age1), Age1 < 18) ->
        format('INVALID MARRIAGE: ~w is under 18 (Age: ~w).', [P1, Age1]), nl
    ;   (underage(P2, Age2), Age2 < 18) ->
        format('INVALID MARRIAGE: ~w is under 18 (Age: ~w).', [P2, Age2]), nl
    ;   % Step 3: If all checks pass, perform the marriage.
        assertz(married(P1, P2)),
        assertz(married(P2, P1)),
        writeln('Marriage successful!'), nl
    ).

ensure_person_exists(Name) :-
    person(_,_,Name,_,_,_), !.
ensure_person_exists(Name) :-
    writeln('Birth year (YYYY):'),           read(B),
    writeln('Death year (YYYY or none):'),   read(D),
    writeln('Gender (male/female):'),        read(G),
    assertz(person(_, _, Name, B, D, G)).


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


show_relation :-
    writeln('please type first person name:'),
    read(P1),
    writeln('please type second person name :'),
    read(P2),
    (   find_relationship(P1, P2, Relation) ->
        format('~w~n', [Relation])
    ;   writeln('Bu iki kişi arasında bir akrabalık ilişkisi bulunamadı')
    ).

% Main relationship finding 
find_relationship(P1, P2, Relation) :-
    P1 \= P2,
    (   direct_relationship(P1, P2, Relation)
    ;   indirect_relationship(P1, P2, Relation)
    ).

% Parent-child relationships
% Mother–Child
direct_relationship(P1, P2, 'Anne') :-
    person(_, P1, P2, _, _, _).

% Father–Child
direct_relationship(P1, P2, 'Baba') :-
    person(P1, _, P2, _, _, _).

% Child–Parent (Oğul)
direct_relationship(P1, P2, 'Oğul') :-
    person(P2, _, P1, _, _, male).
direct_relationship(P1, P2, 'Oğul') :-
    person(_, P2, P1, _, _, male).    

% Child–Parent (Kız)
direct_relationship(P1, P2, 'Kız') :-
    person(_, P2, P1, _, _, female).
direct_relationship(P1, P2, 'Kız') :-
    person(P2, _, P1, _, _, female).

% Sibling relationships
direct_relationship(P1, P2, SiblingType) :-
    person(Father, Mother, P1, Birth1, _, Gender1),
    person(Father, Mother, P2, Birth2, _, Gender2),
    P1 \= P2,
    Father \= unknown,
    Mother \= unknown,
    sibling_type(Gender1, Gender2, Birth1, Birth2, SiblingType).

% Yaşa ve cinsiyete göre kardeş türü
sibling_type(male,   male,   Birth1, Birth2, 'Abi')            :- Birth1 < Birth2.
sibling_type(male,   male,   Birth1, Birth2, 'Erkek Kardeş')    :- Birth1 > Birth2.
sibling_type(female, female, Birth1, Birth2, 'Abla')            :- Birth1 < Birth2.
sibling_type(female, female, Birth1, Birth2, 'Kız Kardeş')      :- Birth1 > Birth2.
sibling_type(male,   female, _,      _,      'Erkek Kardeş').          
sibling_type(female, male,   _,      _,      'Kız Kardeş').    

% Marriage relationships
direct_relationship(P1, P2, 'Eş') :-
    married(P1, P2).

% Amca (baba tarafı erkek kardeş)
indirect_relationship(P1, P2, 'Amca') :-
    person(GrandFather, GrandMother, Father, _, _, _),
    person(Father, _, P2, _, _, _),
    person(GrandFather, GrandMother, P1, _, _, male),
    P1 \= Father.

% Dayı (anne tarafı erkek kardeş)
indirect_relationship(P1, P2, 'Dayı') :-
    person(GrandFather, GrandMother, Mother, _, _, _),
    person(_, Mother, P2, _, _, _),
    person(GrandFather, GrandMother, P1, _, _, male),
    P1 \= Mother.

% Hala (baba tarafı kız kardeş)
indirect_relationship(P1, P2, 'Hala') :-
    person(GrandFather, GrandMother, Father, _, _, _),
    person(Father, _, P2, _, _, _),
    person(GrandFather, GrandMother, P1, _, _, female),
    P1 \= Father.

% Teyze (anne tarafı kız kardeş)
indirect_relationship(P1, P2, 'Teyze') :-
    person(GrandFather, GrandMother, Mother, _, _, _),
    person(_, Mother, P2, _, _, _),
    person(GrandFather, GrandMother, P1, _, _, female),
    P1 \= Mother.

indirect_relationship(P1, P2, 'Yeğen') :-
    person(Father, Mother, P2, _, _, _),
    person(Father, Mother, Sibling, _, _, _),
    Sibling \= P2,
    ( person(Sibling, _, P1, _, _, _)
    ; person(_, Sibling, P1, _, _, _) ).

indirect_relationship(P1, P2, 'Kuzen') :-
    person(GrandFather, GrandMother, Parent1, _, _, _),
    person(GrandFather, GrandMother, Parent2, _, _, _),
    Parent1 \= Parent2,
    ( person(Parent1, _, P1, _, _, _)
    ; person(_, Parent1, P1, _, _, _) ),
    ( person(Parent2, _, P2, _, _, _)
    ; person(_, Parent2, P2, _, _, _) ).

indirect_relationship(P1, P2, 'Kayınvalide') :-
    married(P2, Spouse),
    person(_, P1, Spouse, _, _, female).

indirect_relationship(P1, P2, 'Kayınpeder') :-
    married(P2, Spouse),
    person(P1, _, Spouse, _, _, male).

indirect_relationship(P1, P2, 'Gelin') :-
    ( person(P2, _, Child, _, _, _) ; person(_, P2, Child, _, _, _) ),
    married(Child, P1),
    P1 \= P2.

indirect_relationship(P1, P2, 'Damat') :-
    ( person(P2, _, Child, _, _, _) ; person(_, P2, Child, _, _, _) ),
    married(P1, Child),
    P1 \= P2.

% Sibling-in-law relationships SORUN VAR
indirect_relationship(P1, P2, 'Enişte') :-
    married(P1, Sister),
    person(Father, Mother, Sister, _, _, female),
    person(Father, Mother, P2, _, _, _),
    Sister \= P2.

indirect_relationship(P1, P2, 'Yenge') :- % SORUN VAR
    married(P1, Brother),
    person(Father, Mother, Brother, _, _, male),
    person(Father, Mother, P2, _, _, _),
    Brother \= P2.

indirect_relationship(P1, P2, 'Baldız') :-
    married(P2, Wife),
    person(Father, Mother, Wife, _, _, female),
    person(Father, Mother, P1, _, _, female),
    P1 \= Wife.

indirect_relationship(P1, P2, 'Kayınbirader') :-
    married(P2, Spouse),
    person(Father, Mother, Spouse, _, _, _),
    person(Father, Mother, P1, _, _, male),
    P1 \= Spouse.

indirect_relationship(P1, P2, 'Bacanak') :-
    person(_, _, P1, _, _, male),
    person(_, _, P2, _, _, male),
    married(P1, Wife1),
    married(P2, Wife2),
    person(Father, Mother, Wife1, _, _, female),
    person(Father, Mother, Wife2, _, _, female),
    Wife1 \= Wife2.

indirect_relationship(P1, P2, 'Elti') :-
    person(_, _, P1, _, _, female),
    person(_, _, P2, _, _, female),
    married(P1, Husband1),
    married(P2, Husband2),
    person(Father, Mother, Husband1, _, _, male),
    person(Father, Mother, Husband2, _, _, male),
    Husband1 \= Husband2.

% --- Update birth year ---
update_birth(Name, NewBirthYear) :-
    person(Father, Mother, Name, OldBirth, Death, Gender),
    ( validate_birth_year(Name, NewBirthYear, Death) ->
        retract(person(Father, Mother, Name, OldBirth, Death, Gender)),
        assertz(person(Father, Mother, Name, NewBirthYear, Death, Gender)),
        format('~w kişisinin doğum yılı ~w olarak güncellendi.~n', [Name, NewBirthYear])
    ; writeln('Geçersiz doğum yılı! Güncelleme başarısız.')
    ).
update_birth(Name, _) :-
    format('~w isimli kişi bulunamadı.~n', [Name]).

% --- Update death year ---
update_death(Name, NewDeathYear) :-
    person(Father, Mother, Name, Birth, OldDeath, Gender),
    !,
    ( validate_death_year(Name, Birth, NewDeathYear) ->
        retract(person(Father, Mother, Name, Birth, OldDeath, Gender)),
        assertz(person(Father, Mother, Name, Birth, NewDeathYear, Gender)),
        format('~w kişisinin ölüm yılı ~w olarak güncellendi.~n', [Name, NewDeathYear])
    ; writeln('Geçersiz ölüm yılı! Güncelleme başarısız.')
    ).
update_death(Name, _) :-
    format('~w isimli kişi bulunamadı.~n', [Name]).

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
    ( Gender == male -> writeln('Gender: m') ; writeln('Gender: f') ),
    ( Death == none -> writeln('Status: Alive')
    ; writeln('Status: Dead') ).
get_information(Name) :-
    format('~w isimli kişi bulunamadı.~n', [Name]).


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
    (Death=='none'->
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
