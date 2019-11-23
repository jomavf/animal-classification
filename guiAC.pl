:- use_module(library(assoc)).

:- dynamic(yes/1).
:- dynamic(no/1).
:- dynamic(hipotesis/1).

% INICIO Logica para insercion de varias reglas

    assert_characteristics(Characteristics) :-
        group_characteristics(Characteristics, GroupedCharacteristics),
        maplist(assert_condition, GroupedCharacteristics).

    % Conditions should be a conjunction, e.g., `(a, b, c)`
    assert_condition(Head-Conditions) :-
        maplist(make_check, Conditions, Checks),
        list_to_conjunction(Checks, Conjunctions),
        assertz(( Head :- Conjunctions )).

    % ex. group_characteristics([foo-f1, foo-f2, bar-b1], [foo-[f1,f2], bar-[b1]]).
    group_characteristics(AnimalCharacteristics, Grouped) :-
        empty_assoc(Assoc),
        group_characteristics(AnimalCharacteristics, Assoc, Grouped).

    % helper for group_characteristics/2
    group_characteristics([], Assoc, Grouped) :- assoc_to_list(Assoc, Grouped).
    group_characteristics([Animal-Char|Rest], Assoc0, Grouped) :-
        % Updating an existing animal with the new characteristic `Char`
        ( get_assoc(Animal, Assoc0, Chars, Assoc1, [Char|Chars]), !
        % Otherwise, the key for `Animal` isn't present yet, so add it.
        ; put_assoc(Animal, Assoc0, [Char], Assoc1) ),
        group_characteristics(Rest, Assoc1, Grouped).


    % Convert a list of predictes into a conjunction of predicates
    % ex. list_to_conjunction([a,b,c], (a, (b, (c, true)))).
    list_to_conjunction([], true).
    list_to_conjunction([P|Ps], (P, Conjuncts)) :- list_to_conjunction(Ps, Conjuncts).

    % just a helper used in assert_condition/1
    make_check(C, verificar(C)).

% FIN Logica para insercion de varias reglas

% Conexion a la base de datos
    conexion :- odbc_connect('prologconexion',_,[user(''),password(''),alias(bd),open(once)]) -> write('Conexion a la base de datos exitosa\n'); write('Error de conexion a la base de datos'). 

% Logica para insertar dinamicamente (reglas) de manera recursiva ,los datos traidos de la db.
    run_insert_hipotesis([]).
    run_insert_hipotesis([H|T]) :- asserta((hipotesis(H):- H)), run_insert_hipotesis(T).
    %format('Insertando ~w \n',[H]),

% Consulta a la base de datos para obtener el nombre de los animales en la base de datos
    run_hipotesis :- findall(X,odbc_query(bd, 'select name from Animal',row(X)),X),run_insert_hipotesis(X),!.

% Inserta las reglas para determinar el animal que cumple con determinadas caracteristicas
    run_query :- odbc_query(bd, 'select A.[name],C.[name] from Animal_Caracteristica AC join Animal A on AC.animal_id=A.id join Caracteristica C on C.id = AC.caracteristica_id;',Lista, [ findall(Animal-Caracteristica,row(Animal,Caracteristica)) ]), assert_characteristics(Lista).

% Comando run: comienza con las preguntas al usuario
    % run :- hipotesis(Animal),
    % format('El animal es: ~w',[Animal]).  

% Si el sistema experto, no encuentra el animal que cumpla con las caracteristicas verificadas por el usuario, el valor por descarte sera desconocido
    hipotesis('Desconocido').

% Logica para verificar una caracteristica
    verificar(Sintoma) :-
        (yes(Sintoma)
        ->
        true ;
        (no(Sintoma)
        ->
        fail ;
        preguntar(Sintoma,Respuesta)),
        (Respuesta == si)  
        ->
        assertz(yes(Sintoma));
        (assertz(no(Sintoma)),fail)
        ).

% ----------------  ----------------  START GUI  ----------------  ---------------- %
:- use_module(library(pce)).
:- pce_image_directory('./imagenes').
:- use_module(library(pce_style_item)).
:- dynamic color/2.

% working_directory(_,'C:\\Users\\jmart\\Documents\\animal-classification').

% Preguntar window
  preguntar(Preg,Resp):-new(Di,dialog('Colsultar Datos:')),
                        new(L2,label(texto,'Responde las siguientes preguntas:',font('times','roman',15))),
                        new(L3,label(texto,'El animal tiene la siguiente caracteristica?',font('times','roman',13))),
                        new(La,label(prob,Preg)),
                        new(B1,button(si,and(message(Di,return,si)))),
                        new(B2,button(no,and(message(Di,return,no)))),
                        send(Di, gap, size(25,25)),
                        send(Di,append(L2)),
                        send(Di,append(L3)),
                        send(Di,append(La)),
                        send(Di,append(B1)),
                        send(Di,append(B2)),
                        send(Di,default_button,'si'),
                        send(Di,open_centered),get(Di,confirm,Answer),
                        free(Di),
                        Resp=Answer.

% Preguntas
  preguntas:- borrado,
              send(@iniciar_consulta, free),
              send(@salir, free),
              hipotesis(Animal),
              send(@texto, selection('El Diagnostico a partir de los datos es:')),
              send(@resp1, selection(Animal)),
              new(@iniciar_consulta, button('Iniciar consulta',
              message(@prolog, preguntas)
              )).

% Borrado
  borrado:- send(@resp1, selection('')).

% Crear interfaz inicio
  crea_interfaz_inicio:- new(@interfaz,dialog('Bienvenido al Sistema Experto de clasificacion de animales',
  size(300,100))),

  %Imprimir label
  new(@texto, label(nombre,'Clasificador de animales apartir de caracteristicas ingresadas',font('times','roman',18))),
  new(@resp1, label(nombre,'',font('times','roman',26))),

  new(@salir,button('Salir',and(message(@interfaz,destroy),message(@interfaz,free)))),
  new(@iniciar_consulta, button('Iniciar consulta',message(@prolog, preguntas))),

  send(@interfaz, display,@texto,point(20,10)),
  send(@interfaz, display,@resp1,point(20,50)),
  send(@interfaz, display,@salir,point(300,70)),
  send(@interfaz, display,@iniciar_consulta,point(190,70)),
  send(@interfaz,open_centered).

% Inicio
:- conexion.
:- run_hipotesis, write('Datos insertados correctamente ( REGLA: hipotesis(animal ) :- animal) \n').
:- run_query, write('Datos insertados correctamente ( REGLA: animal :- verificar(caracteristica) )\n').
:-crea_interfaz_inicio.