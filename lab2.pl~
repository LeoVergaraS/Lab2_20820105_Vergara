car([H|_],H).
cdr([_|T],T).

cadr(A,X):-
    cdr(A,B),
    car(B,X).

caddr(A,X):-
    cdr(A,B),
    cdr(B,C),
    car(C,X).

cadddr(A,X):-
    cdr(A,B),
    cdr(B,C),
    cdr(C,D),
    car(D,X).

caddddr(A,X):-
    cdr(A,B),
    cdr(B,C),
    cdr(C,D),
    cdr(D,E),
    car(E,X).

cadddddr(A,X):-
    cdr(A,B),
    cdr(B,C),
    cdr(C,D),
    cdr(D,E),
    cdr(E,F),
    car(F,X).

%TDA Date: constructor
date(D,M,A,[D,M,A]):- integer(D),integer(M),integer(A),
    31>=D,1=<D,12>=M,1=<D,A>=0.

%TDA paradigmadocs: constructor y modificadores.
paradigmaDocs(Name,Date,[Name,Date,[],[],[]]):-string(Name),date(_,_,_,Date).

setSesionActiva(P1,SA,P2):-
    car(P1,Name),
    cadr(P1,Date),
    cadddr(P1,LU),
    caddddr(P1,LD),
    P2 = [Name,Date,SA,LU,LD].

setListaUsuario(P1,LU,P2):-
    car(P1,Name),
    cadr(P1,Date),
    caddr(P1,SA),
    caddddr(P1,LD),
    P2 = [Name,Date,SA,LU,LD].

setListaDocumento(P1,LD,P2):-
    car(P1,Name),
    cadr(P1,Date),
    caddr(P1,SA),
    cadddr(P1,LU),
    P2 = [Name,Date,SA,LU,LD].

%TDA usuario: constructor
usuario(Name,Pass,Date,[Name,Pass,Date]):- string(Name),string(Pass),date(_,_,_,Date).

%TDA permisos: constructor
permisos(LP,LUP,[LP,LUP]).

%TDA documento: constructor y selector
documento(Autor,Fecha,Nombre,Contenido,[Autor,Fecha,Nombre,Contenido,[],[]]):-
    string(Autor),
    date(_,_,_,Fecha),
    string(Nombre),
    string(Contenido).

setListaPermisos(Do1,LP,Do2):-
    car(Do1,Autor),
    cadr(Do1,Fecha),
    caddr(Do1,Nombre),
    cadddr(Do1,Contenido),
    cadddddr(Do1,LV),
    Do2 = [Autor,Fecha,Nombre,Contenido,LP,LV].

setListaVersiones(Do1,LV,Do2):-
    car(Do1,Autor),
    cadr(Do1,Fecha),
    caddr(Do1,Nombre),
    cadddr(Do1,Contenido),
    caddddr(Do1,LP),
    Do2 = [Autor,Fecha,Nombre,Contenido,LP,LV].

setContenido(Do1,Contenido,Do2):-
    car(Do1,Autor),
    cadr(Do1,Fecha),
    caddr(Do1,Nombre),
    caddddr(Do1,LP),
    cadddddr(Do1,LV),
    Do2 = [Autor,Fecha,Nombre,Contenido,LP,LV].

%TDA version: constructor
version(Id,Contenido,Fecha,[Id,Contenido,Fecha]):-
    integer(Id),string(Contenido),date(_,_,_,Fecha).

%Predicado pardigmaDocsRegister()
existe([],_):-fail.
existe([[H|_]|_],H):- !.
existe([_|T],Username):-
    existe(T,Username).

agregar([],X,[X]).
agregar([H|T],X,[H|L]):-agregar(T,X,L).

paradigmaDocsRegister(Sn1,Fecha,Username,Password,Sn2):-
    cadddr(Sn1,LU),
    \+(existe(LU,Username)),
    usuario(Username,Password,Fecha,U),
    agregar(LU,U,LU1),
    setListaUsuario(Sn1,LU1,Sn2).



%Predicado paradigmaDocsLogin:
esta([],_,_):- fail.
esta([[U,P,_]|_],U,P).
esta([_|LU],Username,Password):-esta(LU,Username,Password).

conectado([]).
conectado([_]):-fail.

paradigmaDocsLogin(Sn1,Username,Password,Sn2):-
    cadddr(Sn1,LU),
    esta(LU,Username,Password),
    caddr(Sn1,SA),
    conectado(SA),
    agregar(SA,Username,SA1),
    setSesionActiva(Sn1,SA1,Sn2).

%Predicado paradigmaDocsCreate
paradigmaDocsCreate(Sn1,Fecha,Nombre,Contenido,Sn2):-
    caddr(Sn1,SA),
    \+(conectado(SA)),
    car(SA,Autor),
    documento(Autor,Fecha,Nombre,Contenido,Documento),
    caddddr(Sn1,LD),
    agregar(LD,Documento,LD1),
    setListaDocumento(Sn1,LD1,PA),
    setSesionActiva(PA,[],Sn2).

%Predicado paradigmaDocsShare:
buscarElemento([H|_],0,H).
buscarElemento([_|T],I,Elemento):- I1 is I - 1,
    buscarElemento(T,I1,Elemento).

actualizarLD([],_,_,[]).
actualizarLD([_|T],0,Elemento,[Elemento|L]):-actualizarLD(T,-1,Elemento,L).
actualizarLD([H|T],Id,Elemento,[H|L]):-Id1 is Id - 1,actualizarLD(T,Id1,Elemento,L).

propietario(Documento,Username):-
    car(Documento,Autor),
    Autor == Username.

estaUsuario([],_):-fail.
estaUsuario([H|_],H).
estaUsuario([_|T],Username):-estaUsuario(T,Username).

estaPermiso([],_):-fail.
estaPermiso([H|_],H).
estaPermiso([_|T],Permiso):-estaPermiso(T,Permiso).


tienePermiso(Username,PermisosDocumento,Permiso):-
    cadr(PermisosDocumento,LUP),
    estaUsuario(LUP,Username),
    car(PermisosDocumento,LP),
    estaPermiso(LP,Permiso).

paradigmaDocsShare(Sn1,IdD,LP,LUP,Sn2):-
    % Verifica si hay una sesion activa,
    caddr(Sn1,SA),
    \+(conectado(SA)),

    % Se busca el documento y se verifica
    % si el usuario conectado tiene permiso
    % de compatir o si es propietario.
    caddddr(Sn1,LD),
    IdD1 is IdD - 1,
    buscarElemento(LD,IdD1,Documento),

    % es propietario? o tiene permiso de compartir?
    car(SA,Username),
    caddddr(Documento,PermisosDocumento),
    (propietario(Documento,Username);
    tienePermiso(Username,PermisosDocumento,"S")),

    permisos(LP,LUP,Permisos),
    setListaPermisos(Documento,Permisos,Documento1),
    actualizarLD(LD,IdD1,Documento1,LD1),
    setListaDocumento(Sn1,LD1,PA),
    setSesionActiva(PA,[],Sn2).

%Predicado paradigmaDocsAdd:
concatenar(String1,String2,String3):-
    string_chars(String1,ListaString1),
    string_chars(String2,ListaString2),
    append(ListaString1,ListaString2,LS3),
    atomics_to_string(LS3,String3).

agregarAListaVersiones([],Id,Contenido,Fecha,[VersionN]):-version(Id,Contenido,Fecha,VersionN).
agregarAListaVersiones([H|T],Id,Contenido,Fecha,[H|L]):-Id1 is Id + 1,
    agregarAListaVersiones(T,Id1,Contenido,Fecha,L).

paradigmaDocsAdd(Sn1,IdD,Fecha,ContenidoAgregar,Sn2):-
    % Verifica si hay una sesion activa,
    caddr(Sn1,SA),
    \+(conectado(SA)),

    % Se busca el documento y se verifica
    % si el usuario conectado tiene permiso
    % de compatir o si es propietario.
    caddddr(Sn1,LD),
    IdD1 is IdD - 1,
    buscarElemento(LD,IdD1,Documento),

    % es propietario? o tiene permiso de escritura?
    car(SA,Username),
    caddddr(Documento,PermisosDocumento),
    (propietario(Documento,Username);
    tienePermiso(Username,PermisosDocumento,"W")),

    cadddr(Documento,Contenido),
    cadddddr(Documento,LV),
    agregarAListaVersiones(LV,0,Contenido,Fecha,LV1),
    setListaVersiones(Documento,LV1,Documento1),
    concatenar(Contenido,ContenidoAgregar,String3),
    setContenido(Documento1,String3,Documento2),
    actualizarLD(LD,IdD1,Documento2,LD1),
    setListaDocumento(Sn1,LD1,PA),
    setSesionActiva(PA,[],Sn2).

%Predicado paradigmaDocsRestoreVersion:
paradigmaDocsRestoreVersion(Sn1,Fecha,IdD,IdV,Sn2):-
    % Verifica si hay una sesion activa,
    caddr(Sn1,SA),
    \+(conectado(SA)),

    % Se busca el documento y se verifica
    % si el usuario conectado tiene permiso
    % de compatir o si es propietario.
    caddddr(Sn1,LD),
    IdD1 is IdD - 1,
    buscarElemento(LD,IdD1,Documento),

    % es propietario?
    car(SA,Username),
    propietario(Documento,Username),

    cadddr(Documento,Contenido),
    cadddddr(Documento,LV),
    agregarAListaVersiones(LV,0,Contenido,Fecha,LV1),
    setListaVersiones(Documento,LV1,Documento1),

    buscarElemento(LV1,IdV,VersionN),
    cadr(VersionN,ContenidoVersionN),
    setContenido(Documento1,ContenidoVersionN,Documento2),

    actualizarLD(LD,IdD1,Documento2,LD1),

    setListaDocumento(Sn1,LD1,PA),
    setSesionActiva(PA,[],Sn2).










