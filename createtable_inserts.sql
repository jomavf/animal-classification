create table Animal(
	id int primary key identity not null,
	name varchar(20) not null
);

create table Caracteristica(
	id int primary key identity not null,
	name varchar(20) not null
);

create table Animal_Caracteristica(
	id int primary key identity not null,
	animal_id int references Animal(id),	
	caracteristica_id int references Caracteristica(id)
);

select * from Animal;
select * from Caracteristica;
select * from Animal_Caracteristica;

insert into Animal values ('chita');
insert into Animal values ('tigre');
insert into Animal values ('jirafa');
insert into Animal values ('zebra');
insert into Animal values ('avestruz');
insert into Animal values ('pinguino');

insert into Caracteristica values ('mamifero');
insert into Caracteristica values ('carnivoro');
insert into Caracteristica values ('color_rojizo');
insert into Caracteristica values ('manchas_oscuras');
insert into Caracteristica values ('rayas_negras');
insert into Caracteristica values ('ungulado');
insert into Caracteristica values ('cuello_largo');
insert into Caracteristica values ('piernas_largas');
insert into Caracteristica values ('pajaro');
insert into Caracteristica values ('vuela');
insert into Caracteristica values ('color_negro_y_blanco');
insert into Caracteristica values ('nadar');

insert into Animal_Caracteristica values(1,1);
insert into Animal_Caracteristica values(1,2);
insert into Animal_Caracteristica values(1,3);
insert into Animal_Caracteristica values(1,4);

insert into Animal_Caracteristica values(2,1);
insert into Animal_Caracteristica values(2,2);
insert into Animal_Caracteristica values(2,3);
insert into Animal_Caracteristica values(2,5);

insert into Animal_Caracteristica values(3,6);
insert into Animal_Caracteristica values(3,1);
insert into Animal_Caracteristica values(3,7);
insert into Animal_Caracteristica values(3,8);
insert into Animal_Caracteristica values(3,4);

insert into Animal_Caracteristica values(4,6);
insert into Animal_Caracteristica values(4,1);
insert into Animal_Caracteristica values(4,4);

insert into Animal_Caracteristica values(5,9);
insert into Animal_Caracteristica values(5,10);
insert into Animal_Caracteristica values(5,7);
insert into Animal_Caracteristica values(5,8);
insert into Animal_Caracteristica values(5,11);

insert into Animal_Caracteristica values(6,9);
insert into Animal_Caracteristica values(6,10);
insert into Animal_Caracteristica values(6,11);
insert into Animal_Caracteristica values(6,12);


select A.[name],C.[name] from Animal_Caracteristica AC join Animal A on AC.animal_id=A.id join Caracteristica C on C.id = AC.caracteristica_id;