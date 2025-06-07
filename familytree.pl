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
    person(Person1, _, female, _, _, _, _, Children),
    member(Person2, Children).

direct_relationship(Person1, Person2, 'Baba') :-
    person(Person1, _, male, _, _, _, _, Children),
    member(Person2, Children).

direct_relationship(Person1, Person2, 'Oğul') :-
    person(Person2, _, male, _, _, _, _, Children),
    member(Person1, Children).

direct_relationship(Person1, Person2, 'Kız') :-
    person(Person2, _, female, _, _, _, _, Children),
    member(Person1, Children).

% Sibling relationships
direct_relationship(Person1, Person2, SiblingType) :-
    person(Person1, _, Gender1, Birth1, _, Father, Mother, _),
    person(Person2, _, Gender2, Birth2, _, Father, Mother, _),
    Person1 \= Person2,
    Father \= none, Mother \= none,
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

% Indirect relationships (uncles, aunts, cousins, in-laws)
indirect_relationship(Person1, Person2, 'Amca') :-
    person(Person1, _, male, _, _, _, _, _),
    person(Person2, _, _, _, _, Father, _, _),
    Father \= none,
    person(Father, _, _, _, _, GrandFather, GrandMother, _),
    person(Person1, _, _, _, _, GrandFather, GrandMother, _),
    Person1 \= Father.

indirect_relationship(Person1, Person2, 'Dayı') :-
    person(Person1, _, male, _, _, _, _, _),
    person(Person2, _, _, _, _, _, Mother, _),
    Mother \= none,
    person(Mother, _, _, _, _, GrandFather, GrandMother, _),
    person(Person1, _, _, _, _, GrandFather, GrandMother, _),
    Person1 \= Mother.

indirect_relationship(Person1, Person2, 'Hala') :-
    person(Person1, _, female, _, _, _, _, _),
    person(Person2, _, _, _, _, Father, _, _),
    Father \= none,
    person(Father, _, _, _, _, GrandFather, GrandMother, _),
    person(Person1, _, _, _, _, GrandFather, GrandMother, _),
    Person1 \= Father.

indirect_relationship(Person1, Person2, 'Teyze') :-
    person(Person1, _, female, _, _, _, _, _),
    person(Person2, _, _, _, _, _, Mother, _),
    Mother \= none,
    person(Mother, _, _, _, _, GrandFather, GrandMother, _),
    person(Person1, _, _, _, _, GrandFather, GrandMother, _),
    Person1 \= Mother.

indirect_relationship(Person1, Person2, 'Yeğen') :-
    % Person1 is yeğen of Person2 if Person1s parent is sibling of Person2
    person(Person1, _, _, _, _, Father, Mother, _),
    (   (Father \= none, 
         person(Father, _, _, _, _, GrandFather, GrandMother, _),
         person(Person2, _, _, _, _, GrandFather, GrandMother, _),
         Father \= Person2)
    ;   (Mother \= none,
         person(Mother, _, _, _, _, GrandFather, GrandMother, _), 
         person(Person2, _, _, _, _, GrandFather, GrandMother, _),
         Mother \= Person2)
    ).

indirect_relationship(Person1, Person2, 'Kuzen') :-
    person(Person1, _, _, _, _, Father1, Mother1, _),
    person(Person2, _, _, _, _, Father2, Mother2, _),
    (   (Father1 \= none, Father2 \= none,
         person(Father1, _, _, _, _, GF, GM, _),
         person(Father2, _, _, _, _, GF, GM, _),
         Father1 \= Father2)
    ;   (Mother1 \= none, Mother2 \= none,
         person(Mother1, _, _, _, _, GF, GM, _),
         person(Mother2, _, _, _, _, GF, GM, _),
         Mother1 \= Mother2)
    ).

% In-law relationships
indirect_relationship(Person1, Person2, 'Kayınvalide') :-
    married(Person1, Spouse),
    person(Person2, _, female, _, _, _, _, Children),
    member(Spouse, Children).

indirect_relationship(Person1, Person2, 'Kayınpeder') :-
    married(Person1, Spouse),
    person(Person2, _, male, _, _, _, _, Children),
    member(Spouse, Children).

indirect_relationship(Person1, Person2, 'Gelin') :-
    person(Person1, _, female, _, _, _, _, _),
    married(Person1, Spouse),
    person(Person2, _, _, _, _, _, _, Children),
    member(Spouse, Children).

indirect_relationship(Person1, Person2, 'Damat') :-
    person(Person1, _, male, _, _, _, _, _),
    married(Person1, Spouse),
    person(Person2, _, _, _, _, _, _, Children),
    member(Spouse, Children).

% Sibling-in-law relationships
indirect_relationship(Person1, Person2, 'Enişte') :-
    % Person1 is enişte (brother-in-law) of Person2 if Person1 is married to Person2's sister
    person(Person1, _, male, _, _, _, _, _),
    married(Person1, Sister),
    person(Sister, _, female, _, _, Father, Mother, _),
    person(Person2, _, _, _, _, Father, Mother, _),
    Sister \= Person2.

indirect_relationship(Person1, Person2, 'Yenge') :-
    % Person1 is yenge (sister-in-law) of Person2 if Person1 is married to Person2's brother
    person(Person1, _, female, _, _, _, _, _),
    married(Person1, Brother),
    person(Brother, _, male, _, _, Father, Mother, _),
    person(Person2, _, _, _, _, Father, Mother, _),
    Brother \= Person2.

indirect_relationship(Person1, Person2, 'Baldız') :-
    % Person1 is baldız of Person2 if Person1 is sister of Person2s wife
    person(Person1, _, female, _, _, _, _, _),
    married(Person2, Wife),
    person(Wife, _, female, _, _, Father, Mother, _),
    person(Person1, _, _, _, _, Father, Mother, _),
    Person1 \= Wife.

indirect_relationship(Person1, Person2, 'Kayınbirader') :-
    % Person1 is kayınbirader of Person2 if Person1 is brother of Person2s spouse
    person(Person1, _, male, _, _, _, _, _),
    married(Person2, Spouse),
    person(Spouse, _, _, _, _, Father, Mother, _),
    person(Person1, _, _, _, _, Father, Mother, _),
    Person1 \= Spouse.

indirect_relationship(Person1, Person2, 'Bacanak') :-
    % Person1 is bacanak of Person2 if they are married to sisters
    person(Person1, _, male, _, _, _, _, _),
    person(Person2, _, male, _, _, _, _, _),
    married(Person1, Wife1),
    married(Person2, Wife2),
    person(Wife1, _, female, _, _, Father, Mother, _),
    person(Wife2, _, female, _, _, Father, Mother, _),
    Wife1 \= Wife2.

indirect_relationship(Person1, Person2, 'Elti') :-
    % Person1 is elti of Person2 if they are married to brothers
    person(Person1, _, female, _, _, _, _, _),
    person(Person2, _, female, _, _, _, _, _),
    married(Person1, Husband1),
    married(Person2, Husband2),
    person(Husband1, _, male, _, _, Father, Mother, _),
    person(Husband2, _, male, _, _, Father, Mother, _),
    Husband1 \= Husband2.




% Update birth year
update_birth(Name, NewBirthYear) :-
    % Check if person exists
    person(Name, Surname, Gender, OldBirth, Death, Father, Mother, Children),
    
    % Validate the new birth year
    (   validate_birth_year(Name, NewBirthYear, Death, Children) ->
        % Remove old fact and add new one
        retract(person(Name, Surname, Gender, OldBirth, Death, Father, Mother, Children)),
        assert(person(Name, Surname, Gender, NewBirthYear, Death, Father, Mother, Children)),
        format('~w kişisinin doğum yılı ~w olarak güncellendi.~n', [Name, NewBirthYear])
    ;   writeln('Geçersiz doğum yılı! Güncelleme başarısız.')
    ).

update_birth(Name, _) :-
    \+ person(Name, _, _, _, _, _, _, _),
    format('~w isimli kişi bulunamadı.~n', [Name]).

% Update death year
update_death(Name, NewDeathYear) :-
    % Check if person exists
    person(Name, Surname, Gender, Birth, OldDeath, Father, Mother, Children),
    
    % Validate the new death year
    (   validate_death_year(Name, Birth, NewDeathYear, Children) ->
        % Remove old fact and add new one
        retract(person(Name, Surname, Gender, Birth, OldDeath, Father, Mother, Children)),
        assert(person(Name, Surname, Gender, Birth, NewDeathYear, Father, Mother, Children)),
        format('~w kişisinin ölüm yılı ~w olarak güncellendi.~n', [Name, NewDeathYear])
    ;   writeln('Geçersiz ölüm yılı! Güncelleme başarısız.')
    ).

update_death(Name, _) :-
    \+ person(Name, _, _, _, _, _, _, _),
    format('~w isimli kişi bulunamadı.~n', [Name]).


% Validations
% Validate birth year constraints
validate_birth_year(Name, NewBirthYear, Death, Children) :-
    % Check if birth year is reasonable (not in future)
    get_time(CurrentTime),
    format_time(atom(CurrentYear), '%Y', CurrentTime),
    atom_number(CurrentYear, CurrentYearNum),
    NewBirthYear =< CurrentYearNum,
    
    % Check death year constraint
    (   Death = none -> true
    ;   Death > NewBirthYear
    ),
    
    % Check children constraint (parent should be born at least 18 years before children)
    validate_children_ages(Children, NewBirthYear).

validate_children_ages([], _).
validate_children_ages([Child|Rest], ParentBirth) :-
    person(Child, _, _, ChildBirth, _, _, _, _),
    (   ChildBirth = none -> true
    ;   ParentBirth + 18 =< ChildBirth  % Parent should be at least 18 when child is born
    ),
    validate_children_ages(Rest, ParentBirth).

% Validate death year constraints
validate_death_year(Name, Birth, NewDeathYear, Children) :-
    % Check if death year is after birth year
    Birth < NewDeathYear,
    
    % Check if death year is not in future
    get_time(CurrentTime),
    format_time(atom(CurrentYear), '%Y', CurrentTime),
    atom_number(CurrentYear, CurrentYearNum),
    NewDeathYear =< CurrentYearNum,
    
    % Check that person didnt die before having children
    validate_death_vs_children(Children, NewDeathYear).

validate_death_vs_children([], _).
validate_death_vs_children([Child|Rest], DeathYear) :-
    person(Child, _, _, ChildBirth, _, _, _, _),
    (   ChildBirth = none -> true
    ;   ChildBirth =< DeathYear  % Child should be born before parents death
    ),
    validate_death_vs_children(Rest, DeathYear).


% helper functions 
% Check if two people are married
married(Person1, Person2) :-
    marriage(Person1, Person2).
married(Person1, Person2) :-
    marriage(Person2, Person1).

% Get current year helper
current_year(Year) :-
    get_time(CurrentTime),
    format_time(atom(YearAtom), '%Y', CurrentTime),
    atom_number(YearAtom, Year).
