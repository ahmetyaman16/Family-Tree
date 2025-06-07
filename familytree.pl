:-dynamic person/6.

person(unknown, unknown, 'Ahmet Arslan', 1940, 'none', male).
person(unknown, unknown, 'Fatma Arslan', 1945, 2015, female).
person('Ahmet Arslan', 'Fatma Arslan', 'Murat Arslan', 1970, 'none', male).
person('Murat Arslan', 'Mukaddes Demir', 'Zeynep Arslan', 2000, 'none', female).

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
