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

%TDA documento: constructor
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
    cadddddr(Do1,LH),
    Do2 = [Autor,Fecha,Nombre,Contenido,LP,LH].

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


sol(PF):-date(12,12,2021,D1),
    date(22,12,2021,D2),
    paradigmaDocs("DuckDocs",D1,P1),
    paradigmaDocsRegister(P1,D2,"Leo","Pass1",P2),
    paradigmaDocsRegister(P2,D2,"Pedro","Pass2",P3),
    paradigmaDocsRegister(P3,D2,"Miguel","Pass3",P4),
    paradigmaDocsLogin(P4,"Pedro","Pass2",P5),
    paradigmaDocsCreate(P5,D2,"doc0","contenido doc0",PF).

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





