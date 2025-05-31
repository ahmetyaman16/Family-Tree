
loop_entry:-    
    writeln('1-)Ask relation'),
    writeln('2-)Add/Delete/Update person'),
    writeln('3-)Get information of any person'),
    writeln('4-)print the family tree'),
    writeln('5-)Control under 18 age marriage'),
    writeln('6-)Terminate the program'),
    nl,
    writeln('Please choose an operation!'),
    read(Choose),
    (
        Choose==1->show_relation(),loop;
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
                writeln('please type the birthdate of the child:'),read(Birth),
                writeln('please type the death date of the child:'),read(Death),
                writeln('please type the child person gender:'),read(Gender),
                add_person(Father,Mother,Name,Birth,Death,Gender),
                loop_entry.
            ;
            Choose1==2->
                writeln('Update Birth Date:'),read(UpdateBirth)
                writeln('Update Death Date'),read(UpdateDeath),
                update_person(UpdateBirth,UpdateDeath),
                loop_entry.
        );
        Choose==3->
            writeln('please type the name and surname:'),read(Name),
            get_information(Name),
            
        Choose==4->
        Choose==5->
        Choose==6->
    )


