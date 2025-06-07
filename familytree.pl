:-dynamic person/6.


:-dynamic married/2.
current(2025).
person(unknown, unknown, 'Ahmet Arslan', 1940, 'none', male).
person(unknown, unknown, 'Fatma Arslan', 1945, 2015, female).
% person('Ahmet Arslan', 'Fatma Arslan', 'Murat Arslan', 1970, 'none', male).
% person('Murat Arslan', 'Mukaddes Demir', 'Zeynep Arslan', 2000, 'none', female).



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
                writeln('please type the death date of the child (YYYY format) :'),read(Death),
                writeln('please type the child person gender:'),read(Gender),
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
add_person(Father,Mother,Name,Birth,Death,Gender):-
    person(_,_,Name,_,_,_) ->
        writeln('This person already exist');
    % olum-dogum yili kiyaslamasi
    (Death \= none, Death < Birth) ->
        writeln('Death year can not be earlier than birth year');
    check_parents_dates(Father,Mother,Birth),
    assertz(person(Father,Mother,Name,Birth,Death,Gender)),
    writeln('person succesfully added.').
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
 
main :- loop_entry.

print_tree :-
    writeln('--- Family Tree ---'),
    findall(Name, person(_, _, Name, _, _, _), AllNames),
    compute_all_levels(AllNames, [], LevelMap),
    print_by_levels(LevelMap, 0).

compute_all_levels([], Levels, Levels).
compute_all_levels([Name|Rest], Acc, Final) :-
    ( member(Name-_, Acc) ->
        compute_all_levels(Rest, Acc, Final)
    ;
        compute_level(Name, Level),
        compute_all_levels(Rest, [Name-Level|Acc], Final)
    ).

compute_level(Name, Level) :-
    person(Father, Mother, Name, _, _, _),
    find_parent_level(Father, L1),
    find_parent_level(Mother, L2),
    Max is max(L1, L2),
    ( Max == -1 ->
        ( married(Name, Spouse),
          compute_level(Spouse, SpouseLevel) ->
              Level is SpouseLevel
        ;
              Level = 0 )
    ;
        Level is Max + 1
    ).

print_by_levels(LevelMap, Gen) :-
    include(match_level(Gen), LevelMap, CurrentLevel),
    CurrentLevel \= [],
    format('--- Level ~w ---~n', [Gen]),
    maplist(with_birth, CurrentLevel, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, NamesSorted),
    print_persons_couples(NamesSorted, []),
    NextGen is Gen + 1,
    print_by_levels(LevelMap, NextGen).
print_by_levels(_, _).

match_level(Target, _Name-Level) :- Level =:= Target.

with_birth(Name-_, Birth-Name) :-
    person(_, _, Name, Birth, _, _).

    

% marriage deneme 
add_marriage(P1,P2) :- 
    ensure_person_exists(P2),
    ( 
        P1=P2 ->
        writeln('a person can not marry himself/herself '), fail;
    (married(P1,P2); married(P2,P1)) -> 
        writeln('They are already married'),fail;
    forbidden_relation(P1,P2,Relation) ->
        format('They can not get married because ~w and , ~w have ~w.~n',[P1,P2,Relation]);
    underage(P1,Age1), Age1 < 18 -> 
        format('They can not get married because ~w under 18(age: ~w).~n',[P1,Age1]),fail;
    underage(P2,Age2), Age2 < 18 -> 
        format('They can not get married because ~w under 18(age: ~w).~n',[P2,Age2]),fail;
    assertz(married(P1,P2)),
    assertz(married(P2,P1))
       ).

ensure_person_exists(Name) :-
    person(_,_,Name,_,_,_), !.
ensure_person_exists(Name) :-
    format('~w does not exist. Please enter details.~n', [Name]),
    writeln('Father name (or unknown):'), read(F),
    writeln('Mother name (or unknown):'), read(M),
    writeln('Birth year (YYYY):'),           read(B),
    writeln('Death year (YYYY or none):'),   read(D),
    writeln('Gender (male/female):'),        read(G),
    assertz(person(F, M, Name, B, D, G)).


underage(Name, Age) :-
    person(_,_,Name, Birth, Death, _),
    current(CY),
    ( Death == none -> Age is CY - Birth ; Age is Death - Birth ).


forbidden_marriage_relation(P1, P2) :-
    (is_parent_child(P1, P2) ; is_parent_child(P2, P1)).

forbidden_marriage_relation(P1, P2) :-
    are_siblings(P1, P2).

forbidden_marriage_relation(P1, P2) :-
    (is_uncle_aunt(P1, P2) ; is_uncle_aunt(P2, P1)).

forbidden_marriage_relation(P1, P2) :-
    (is_grandparent(P1, P2) ; is_grandparent(P2, P1)).

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
    person(Parent, _, P2, _, _, _),
    person(F, M, P1, _, _, _),
    P1 \= Parent.
is_uncle_aunt(P1, P2) :-
    % P1 is uncle/aunt of P2 (through mothers side)
    person(F, M, Parent, _, _, _),
    person(_, Parent, P2, _, _, _),
    person(F, M, P1, _, _, _),
    P1 \= Parent.

is_grandparent(P1, P2) :-
    person(P1, _, Parent, _, _, _),
    (person(Parent, _, P2, _, _, _) ; person(_, Parent, P2, _, _, _)).

is_grandparent(P1, P2) :-
    person(_, P1, Parent, _, _, _),
    (person(Parent, _, P2, _, _, _) ; person(_, Parent, P2, _, _, _)).


get_information(Name):-
    person(_,_,Name,_,Death,_),
    print_age(Name),
    print_level(Name),
    print_num_of_child(Name),
    (
        Death=='none'->
            writeln('Alive')
        ;
            writeln('Dead')
    ).
    

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

find_level(Name,Level):-
    person(Father,Mother,Name,_,_,_),
    find_parent_level(Father,L1),
    find_parent_level(Mother,L2),
    (L1 >= L2 -> MaxLevel is L1; MaxLevel is L2),
    Level is MaxLevel + 1.

find_parent_level(unknown, -1):- !.
find_parent_level(Name, -1) :-
    \+ person(_, _, Name, _, _, _),
    !.
find_parent_level(Name, Level):-
    find_level(Name, Level).

print_num_of_child(Name):-
    person(_,_,Name,_,_,Gender),
    (
        Gender=='f'->
        findall(Child, person(_,Name,Child,_,_,_), ChildrenFromMother),
        length(ChildrenFromMother,Count),
        format("Total child: ~w~n",[Count])
    ;
        findall(Child, person(Name,_,Child,_,_,_), ChildrenFromFather),
        length(ChildrenFromFather,Count),
        format("Total child: ~w~n",[Count])
    ).
    



% Ask for relationship between two people
show_relation :-
    writeln('please type first person name and surname:'),
    read(Person1),
    writeln('please type second person name and surname:'),
    read(Person2),
    (   find_relationship(Person1, Person2, Relation) ->
        format('~w~n', [Relation])
    ;   writeln('Bu iki kişi arasında bir akrabalık ilişkisi bulunamadı')
    ).

% Main relationship finding
find_relationship(Person1, Person2, Relation) :-
    Person1 \= Person2,
    (   direct_relationship(Person1, Person2, Relation)
    ;   indirect_relationship(Person1, Person2, Relation)
    ).

% Parent-child relationships
direct_relationship(Person1, Person2, 'Anne') :-
    person(_, Mother, Person1, _, _, female),
    person(Mother, _, Person2, _, _, _),
    !.
direct_relationship(Person1, Person2, 'Baba') :-
    person(Father, _, Person1, _, _, male),
    person(_, Father, Person2, _, _, _),
    !.
direct_relationship(Person1, Person2, 'Oğul') :-
    person(_, _, Person2, _, _, male),
    person(Person2, _, Person1, _, _, _),
    !.
direct_relationship(Person1, Person2, 'Kız') :-
    person(_, _, Person2, _, _, female),
    person(Person2, _, Person1, _, _, _),
    !.

% Sibling relationships
direct_relationship(Person1, Person2, SiblingType) :-
    person(Father, Mother, Person1, Birth1, _, Gender1),
    person(Father, Mother, Person2, Birth2, _, Gender2),
    Person1 \= Person2,
    Father \= unknown, Mother \= unknown,
    sibling_type(Gender1, Gender2, Birth1, Birth2, SiblingType).

sibling_type(male, male, Birth1, Birth2, 'Abi') :- Birth1 < Birth2.
sibling_type(male, male, Birth1, Birth2, 'Erkek Kardeş') :- Birth1 > Birth2.
sibling_type(female, female, Birth1, Birth2, 'Abla') :- Birth1 < Birth2.
sibling_type(female, female, Birth1, Birth2, 'Kız Kardeş') :- Birth1 > Birth2.
sibling_type(male, female, _, _, 'Erkek Kardeş').
sibling_type(female, male, _, _, 'Kız Kardeş').

% Marriage relationships
direct_relationship(Person1, Person2, 'Eş') :-
    married(Person1, Person2).
direct_relationship(Person1, Person2, 'Eş') :-
    married(Person2, Person1).

% Indirect relationships

% Amca (father's brother)
indirect_relationship(Person1, Person2, 'Amca') :-
    person(Father, _, Person2, _, _, male),
    person(GrandFather, _, Father, _, _, male),
    person(GrandFather, _, Person1, _, _, male),
    Person1 \= Father.

% Hala (father's sister)
indirect_relationship(Person1, Person2, 'Hala') :-
    person(Father, _, Person2, _, _, _),
    person(GrandFather, _, Father, _, _, _),
    person(GrandFather, _, Person1, _, _, female),
    Person1 \= Father.

% Dayı (mother's brother)
indirect_relationship(Person1, Person2, 'Dayı') :-
    person(Mother, _, Person2, _, _, _),
    person(GrandFather, _, Mother, _, _, _),
    person(GrandFather, _, Person1, _, _, male),
    Person1 \= Mother.

% Teyze (mother's sister)
indirect_relationship(Person1, Person2, 'Teyze') :-
    person(Mother, _, Person2, _, _, _),
    person(GrandFather, _, Mother, _, _, _),
    person(GrandFather, _, Person1, _, _, female),
    Person1 \= Mother.

% Yeğen (child of sibling)
indirect_relationship(Person1, Person2, 'Yeğen') :-
    person(Sibling, _, Person1, _, _, _),
    person(Sibling, _, Person2, _, _, _),
    person(_, _, Sibling, _, _, _),
    Person1 \= Sibling.

% Kuzen (children of siblings)
indirect_relationship(Person1, Person2, 'Kuzen') :-
    person(Parent1, _, Person1, _, _, _),
    person(Parent2, _, Person2, _, _, _),
    parent_sibling(Parent1, Parent2),
    Parent1 \= Parent2.

parent_sibling(P1, P2) :-
    person(GF, GM, P1, _, _, _),
    person(GF, GM, P2, _, _, _).

% In-law relationships

% Kayınvalide (mother-in-law)
indirect_relationship(Person1, Person2, 'Kayınvalide') :-
    married(Person2, Spouse),
    person(_, _, Spouse, _, _, _),
    person(_, _, Person1, _, _, female),
    person(Person1, _, Spouse, _, _, _).

% Kayınpeder (father-in-law)
indirect_relationship(Person1, Person2, 'Kayınpeder') :-
    married(Person2, Spouse),
    person(_, _, Spouse, _, _, _),
    person(_, _, Person1, _, _, male),
    person(Person1, _, Spouse, _, _, _).

% Gelin / Damat
indirect_relationship(Person1, Person2, 'Gelin') :-
    person(_, _, Person1, _, _, female),
    married(Person1, Spouse),
    person(Person2, _, Spouse, _, _, _).

indirect_relationship(Person1, Person2, 'Damat') :-
    person(_, _, Person1, _, _, male),
    married(Person1, Spouse),
    person(Person2, _, Spouse, _, _, _).

% Bacanak, Baldız, Elti, Kayınbirader etc. can be added similarly if needed

% Married fact
married(Person1, Person2) :- marriage(Person1, Person2).
married(Person1, Person2) :- marriage(Person2, Person1).

% Update birth year
update_birth(Name, NewBirthYear) :-
    person(Father, Mother, Name, OldBirth, Death, Gender),
    validate_birth_year(Name, NewBirthYear, Death),
    retract(person(Father, Mother, Name, OldBirth, Death, Gender)),
    assert(person(Father, Mother, Name, NewBirthYear, Death, Gender)).

validate_birth_year(Name, NewBirthYear, Death) :-
    get_time(CurrentTime),
    format_time(atom(CY), '%Y', CurrentTime),
    atom_number(CY, CurrentYear),
    NewBirthYear =< CurrentYear,
    (Death == none -> true ; Death > NewBirthYear).

% Update death year
update_death(Name, NewDeathYear) :-
    person(Father, Mother, Name, Birth, OldDeath, Gender),
    validate_death_year(Birth, NewDeathYear),
    retract(person(Father, Mother, Name, Birth, OldDeath, Gender)),
    assert(person(Father, Mother, Name, Birth, NewDeathYear, Gender)).

validate_death_year(Birth, Death) :-
    Death > Birth,
    get_time(CurrentTime),
    format_time(atom(CY), '%Y', CurrentTime),
    atom_number(CY, CurrentYear),
    Death =< CurrentYear.


are_related(Person1, Person2, prohibited) :-
    (direct_relationship(Person1, Person2, _) ; indirect_relationship(Person1, Person2, _)),
    member(Relation, ['Anne', 'Baba', 'Erkek Kardeş', 'Kız Kardeş', 'Amca', 'Teyze', 'Hala', 'Dayı']),
    Relation == Relation.
