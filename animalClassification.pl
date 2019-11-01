% working_directory(_,"E:\\animal-classification").

:- dynamic(yes/1).
:- dynamic(no/1).

:- dynamic(hipotesis/1).

% Conexion a la base de datos
conexion :- odbc_connect('prologconexion',_,[user(''),password(''),alias(bd),open(once)]) -> write('Conexion a la base de datos exitosa\n'); write('Error de conexion a la base de datos').

% Logica para insertar dinamicamente (reglas) de manera recursiva ,los datos traidos de la db.
run_insert_hipotesis([]).
run_insert_hipotesis([H|T]) :- asserta((hipotesis(H):- H,!)),format('Insertando ~w \n',[H]), run_insert_hipotesis(T).
% Consulta a la base de datos para obtener el nombre de los animales en la base de datos
run_hipotesis :- findall(X,odbc_query(bd, 'select name from Animal',row(X)),X),run_insert_hipotesis(X),!.

run_insert([]).
run_insert([H|T]) :- H = [X,Y] , format('Insertando la regla ~w, ~w \n',[X,Y]), assertz(( X :- (verificar(Y)) )), run_insert(T).

run_insert_format([]).
run_insert_format([H|T]) :- H = [X,Y] , format('Insertando la regla ~w, ~w \n',[X,Y]), assertz(( X :- (verificar(Y)) )), run_insert_format(T).

run_query :- odbc_query(bd, 'select A.[name],C.[name] from Animal_Caracteristica AC join Animal A on AC.animal_id=A.id join Caracteristica C on C.id = AC.caracteristica_id;',Lista, [ findall([Animal,Caracteristica],row(Animal,Caracteristica)) ]), format('insert ~w',[Lista]), run_insert_format(Lista).

% Comando run: comienza con las preguntas al usuario
run :- hipotesis(Animal),
format('El animal es: ~w',[Animal]).

% Si el sistema experto, no encuentra el animal que cumpla con las caracteristicas verificadas por el usuario, el valor por descarte sera desconocido
hipotesis('Desconocido').

%Reglas
% chita :-
%     verificar(mamifero),
%     verificar(carnivoro),
%     verificar(color_rojizo),
%     verificar(manchas_oscuras).

% tigre:-
%     verificar(mamifero),
%     verificar(carnivoro),
%     verificar(color_rojizo),
%     verificar(rayas_negras).

% jirafa :-
%     verificar(ungulado),
%     verificar(mamifero),
%     verificar(cuello_largo),
%     verificar(piernas_largas),
%     verificar(manchas_oscuras).

% zebra :-
%     verificar(ungulado),
%     verificar(mamifero),
%     verificar(manchas_oscuras).

% avestruz :- 
%     verificar(pajaro),
%     verificar(vuela),
%     verificar(cuello_largo),
%     verificar(piernas_largas),
%     verificar(color_negro_y_blanco).

% pinguino :-
%     verificar(pajaro),
%     verificar(vuela),
%     verificar(nadar),
%     verificar(color_negro_y_blanco).

% Logica para preguntar
preguntar(Pregunta) :-
    format('El animal tiene las siguientes caracteristicas-> "~w" ~s',[Pregunta,"? (y/n): "]),
    read(Respuesta),
    ((Respuesta == yes ; Respuesta == y))
    ->
    assertz(yes(Pregunta));
    assertz(no(Pregunta)),fail.

% Logica para verificar una caracteristica
verificar(S) :-
    (yes(S)
    ->
    true ;
    (no(S)
    ->
    fail ;
    preguntar(S))
    ).

% Undo: Todas las respuestas del usuario que se encuentran en la base de datos son borradas.
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo :- retract(hipotesis(_)),write('Reglas de hipotesis eliminados correctamente'),!.

% Ejecuta las siguientes sentencias al momento de cargar el archivo al cmd de prolog.
:- conexion.
:- run_hipotesis.