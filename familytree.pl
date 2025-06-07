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
        (ChildBirth =< FatherBirth + 20 -> 
            writeln('Error: Father is too young when child was born'), fail; true),
        (FatherDeath \= none, ChildBirth > FatherDeath ->
            writeln(' Child born after father died'), fail; true)
    ; true),
    (Mother \= unknown, person(_, _, Mother, MotherBirth, MotherDeath, _) ->
        (ChildBirth =< MotherBirth + 20 -> 
            writeln(' Mother is too young when child was born'), fail; true),
        (MotherDeath \= none, ChildBirth > MotherDeath ->
            writeln(' Child born after mother died'), fail; true)
    ; true).  
main :- loop_entry.

% kontrol icin evlilik iliskileri ve leveller eklenecek 
print_tree :-
    writeln('Family Members:'),
    listPeople.
listPeople :-
    person(_,_,Name,_,_,_),
    writeln(Name),
    fail.
listPeople.    

% marriage deneme 
add_marriage(P1,P2) :- 
    writeln('Name of first peerson: '),
    read(P1),
    ensure_person_exists(P1),
    writeln('Name of second person: ')
    read(P2),
    ensure_person_exists(P2),
    ( 
        P1=P2 ->
        writeln('a person can not marry himself/herself '), fail;
    (married(P1,P2); married(P2,P1)) -> 
        writeln('They are already married'),fail;
    forbidden_relation(P1,P2,Relation) ->
        format('They can not get married because ~w and , ~w have ~w.~n',[P1,P2,Relation]);
    underage(P1,Age1), Age1 < 18 -> 
        format('They can not get married bocause ~ not be 18(age: ~w).~n'[P1,Age1]),fail;
    underage(P2,Age2), Age2 < 18 -> 
        format('They can not get married bocause ~ not be 18(age: ~w).~n'[P2,Age2]),fail;
    assertz(married(P1,P2)),
    assertz(married(P2,P1))
       ).

ensure_person_exists(Name) :-
    birthDate(Name, _), !.  
ensure_person_exists(Name) :-
    format('~w not exists. Please give information .~n', [Name]),
    writeln('Birth date (YYYY):'), read(B),
    writeln(' Death date (none):'), read(D),
    writeln('Gender (male,female):'), read(G),
    assertz(birthDate(Name, B)),
    (D \= none -> assertz(deadDate(Name, D)) ; true),
    assertz(gender(Name, G)),
    (G == male -> assertz(male(Name)) ; G == female -> assertz(female(Name)); true).

undergrade(Name,Age) :-
   birthDate(Name, Y),
   current(CY),
   Age is CY-Y.


%eklenecek yasak akrabalık iliskileri var
forbidden_relation(P1, P2, 'babası') :- father(P1, P2).
forbidden_relation(P1, P2, 'annesi') :- mother(P1, P2).
forbidden_relation(P1, P2, 'oğlu') :- son(P1, P2).
forbidden_relation(P1, P2, 'kızı') :- daughter(P1, P2).
forbidden_relation(P1, P2, 'erkek kardeşi') :- erkekKardes(P1, P2); abi(P1, P2).
forbidden_relation(P1, P2, 'kız kardeşi') :- kizKardes(P1, P2); abla(P1, P2).
forbidden_relation(P1, P2, 'amcası') :- uncle(P1, P2).
forbidden_relation(P1, P2, 'dayısı') :- dayi(P1, P2); dayi1(P1, P2).
forbidden_relation(P1, P2, 'halası') :- hala(P1, P2).
forbidden_relation(P1, P2, 'teyzesi') :- teyze(P1, P2).
forbidden_relation(P1, P2, 'dedesi') :- grandfather(P1, P2).
forbidden_relation(P1, P2, 'ninesi') :- grandmother(P1, P2).


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
    


 

