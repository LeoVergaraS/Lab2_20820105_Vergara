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

%TDA usuario: constructor
usuario(Name,Pass,Date,[Name,Pass,Date]):- string(Name),string(Pass),date(_,_,_,Date).

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
    paradigmaDocsLogin(P4,"Pedro","Pass2",PF).

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

