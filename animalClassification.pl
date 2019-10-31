:- dynamic(yes/1).
:- dynamic(no/1).

:- dynamic(hipotesis/1).

conexion :- odbc_connect('prologconexion',_,[user(''),password(''),alias(bd),open(once)]) -> write('Conexion a la base de datos exitosa\n'); write('Error de conexion a la base de datos').

run_hipotesis :- odbc_query(bd, 'select name from Animal',asserta( (hipotesis(X):- X,!) )).

run_query(X,Y) :- odbc_query(bd, 'select A.[name],C.[name] from Animal_Caracteristica AC join Animal A on AC.animal_id=A.id join Caracteristica C on C.id = AC.caracteristica_id;',row(X,Y)).

run :- hipotesis(Animal),
format('Creo que el animal es: ~w',[Animal]).

hipotesis(chita) :- chita, !.
hipotesis(tigre) :- tigre, !.
hipotesis(zebra) :- zebra, !.
hipotesis(avestruz) :- avestruz, !.
hipotesis(pinguino) :- pinguino, !.
hipotesis(jirafa) :- jirafa, !.
hipotesis('Desconocido').

chita :-
    verificar(mamifero),
    verificar(carnivoro),
    verificar(color_rojizo),
    verificar(manchas_oscuras).

tigre:-
    verificar(mamifero),
    verificar(carnivoro),
    verificar(color_rojizo),
    verificar(rayas_negras).

jirafa :-
    verificar(ungulado),
    verificar(mamifero),
    verificar(cuello_largo),
    verificar(piernas_largas),
    verificar(manchas_oscuras).

zebra :-
    verificar(ungulado),
    verificar(mamifero),
    verificar(manchas_oscuras).

avestruz :- 
    verificar(pajaro),
    verificar(vuela),
    verificar(cuello_largo),
    verificar(piernas_largas),
    verificar(color_negro_y_blanco).

pinguino :-
    verificar(pajaro),
    verificar(vuela),
    verificar(nadar),
    verificar(color_negro_y_blanco).

preguntar(Pregunta) :-
    format('El animal tiene las siguientes caracteristicas-> "~w" ~s',[Pregunta,"? (y/n): "]),
    read(Respuesta),
    ((Respuesta == yes ; Respuesta == y))
    ->
    assertz(yes(Pregunta));
    assertz(no(Pregunta)),fail.

verificar(S) :-
    (yes(S)
    ->
    true ;
    (no(S)
    ->
    fail ;
    preguntar(S))
    ).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.

:- conexion.