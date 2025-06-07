:-dynamic person/6.
:-dynamic married/2.
current(2025).
person(unknown, unknown, 'Ahmet Arslan', 1940, 'none', male).
person(unknown, unknown, 'Fatma Arslan', 1945, 2015, female).
%person('Ahmet Arslan', 'Fatma Arslan', 'Murat Arslan', 1970, 'none', male).
%person('Murat Arslan', 'Mukaddes Demir', 'Zeynep Arslan', 2000, 'none', female).


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
    


 

