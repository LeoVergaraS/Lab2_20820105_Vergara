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



%Predicado paradigmaDocsToString:
dateToString(Date,StrOut):-
    date(D,M,A,Date),

    number_string(D,StrDia),
    concatenar(StrDia,"-",StrDia1),

    number_string(M,StrMes),
    concatenar(StrDia1,StrMes,StrMes1),
    concatenar(StrMes1,"-",StrMes2),

    number_string(A,StrAnio),
    concatenar(StrMes2,StrAnio,StrOut).

permisosLegibles([],[]).
permisosLegibles([H|T],[C|L]):-
    (H == "C" -> C = "Comentar",permisosLegibles(T,L);
     H == "W" -> C = "Escritura",permisosLegibles(T,L);
     H == "S" -> C = "Compartir",permisosLegibles(T,L);
     H == "R" -> C = "Lectura",permisosLegibles(T,L)).

listAccessesToString([],"\tNo hay informacion en la lista.\n").
listAccessesToString([LP,LUP],StrOut):-
    permisosLegibles(LP,LP1),
    atomics_to_string(LP1,", ",StrLP),atomics_to_string(LUP,", ",StrLUP),
    concatenar("\tPermisos: ",StrLP,StrLP1),concatenar("Usuarios: ",StrLUP,StrLUP1),
    concatenar(StrLP1,". ",StrAux),concatenar(StrAux,StrLUP1,StrAux1),concatenar(StrAux1,".\n",StrOut).


versionToString(Version,StrOut):-
    car(Version,Id),
    cadr(Version,Contenido),
    caddr(Version,Fecha),

    number_string(Id,StrId),concatenar(StrId,". ",StrAux1),
    concatenar("Contenido de la version: ",Contenido,StrCon),concatenar(StrCon,". ",StrAux2),
    dateToString(Fecha,StrFec),concatenar("Fecha de modificacion: ",StrFec,StrFec1),concatenar(StrFec1,".\n",StrAux3),
    concatenar(StrAux1,StrAux2,StrAux4),concatenar(StrAux4,StrAux3,StrOut).

listVersionToString([],"","\tNo hay informacion en la lista.\n").
listVersionToString([],StrAux,StrAux).
listVersionToString([H|T],StrAux,StrOut):-
    versionToString(H,StrVer),
    concatenar(StrAux,"\t",StrAux2),
    concatenar(StrAux2,StrVer,StrAux1),
    listVersionToString(T,StrAux1,StrOut).

documentsToString(Documento,StrOut):-
    car(Documento,Autor),
    cadr(Documento,Fecha),
    caddr(Documento,Nombre),
    cadddr(Documento,Contenido),
    caddddr(Documento,LP),
    cadddddr(Documento,LV),

    concatenar("Autor del documento: ",Autor,StrAut),concatenar(StrAut,". ",StrAux),
    dateToString(Fecha,StrFec),concatenar("Fecha de creacion: ",StrFec,StrFec1),concatenar(StrFec1,". ",StrAux2),
    concatenar("Nombre del documento: ",Nombre,StrName),concatenar(StrName,". ",StrAux3),
    concatenar("Contenido del documento: ",Contenido,StrCon),concatenar(StrCon,".\n\n",StrAux4),
    listAccessesToString(LP,StrLP),concatenar("\tLista Permisos:\n",StrLP,StrAux5),
    listVersionToString(LV,"",StrLV),concatenar("\n\tLista de versiones:\n",StrLV,StrAux6),

    concatenar(StrAux,StrAux2,StrFin1),concatenar(StrFin1,StrAux3,StrFin2),
    concatenar(StrFin2,StrAux4,StrFin3),concatenar(StrFin3,StrAux5,StrFin4),
    concatenar(StrFin4,StrAux6,StrOut).


listDocumentsToString([],1,"","\tNo hay informacion en la lista.\n").
listDocumentsToString([],_,StrAux,StrAux).
listDocumentsToString([H|T],Id,StrAux,StrOut):-
    number_string(Id,StrId),
    documentsToString(H,StrDoc),

    concatenar(StrAux,StrId,Str1),
    concatenar(Str1,".- ",Str2),
    concatenar(Str2,StrDoc,Str3),
    concatenar(Str3,"\n",Str4),
    Id1 is Id + 1,

    listDocumentsToString(T,Id1,Str4,StrOut).

filtrar([],_,ListaAux,ListaAux).
filtrar([DocumentoH|T],Username,ListaAux,ListaRes):-
    caddddr(DocumentoH,LP),
    %if
    ((propietario(DocumentoH,Username);tienePermiso(Username,LP,_)) ->
    %then
    agregar(ListaAux,DocumentoH,ListaAux1),filtrar(T,Username,ListaAux1,ListaRes);
    %else
    filtrar(T,Username,ListaAux,ListaRes)).

paradigmaDocsToString(Sn1,StrOut):-
    caddr(Sn1,SA),
    %car(SA,Username),
    caddddr(Sn1,LD),
    %if
   (conectado(SA)  ->
    %then
    listDocumentsToString(LD,1,"",String1),
    concatenar("Bienvenido a la plataforma DuckDocs, los documentos presente en la plataformas son:\n\n",String1,String2),
    concatenar(String2,"\n\n\n Plataforma DuckDocs creado por Leo Inaki Vergara Sepulveda.",StrOut);
    %else
    car(SA,Username),
    filtrar(LD,Username,[],LD1),listDocumentsToString(LD1,1,"",String1),concatenar("Bienvenido a la plataforma DuckDocs, ",Username,Str1),
    concatenar(Str1,". Los documentos en los que tienes permiso o son tuyos son:\n\n",Str2),concatenar(Str2,String1,Str3),
    concatenar(Str3,"\n\n\n Plataforma DuckDocs creado por Leo Inaki Vergara Sepulveda.",StrOut)).



%Predicado paradigmaDocsRevokeAllAccesses:

actualizarPermisos([],_,null,[]).
actualizarPermisos([H|T],Username,null,[C|L]):-
    %if
    (propietario(H,Username)->
    %then
    setListaPermisos(H,[],H1),C = H1,actualizarPermisos(T,Username,null,L);
    %else
    C = H, actualizarPermisos(T,Username,null,L)).

actualizarPermisos(LD,_,[],LD).
actualizarPermisos(LD,Username,[IdC|IdsT],LDRes):-
    IdC1 is IdC - 1,
    buscarElemento(LD,IdC1,Documento),
    propietario(Documento,Username),
    setListaPermisos(Documento,[],Documento1),
    actualizarLD(LD,IdC1,Documento1,LD1),
    actualizarPermisos(LD1,Username,IdsT,LDRes).

paradigmaDocsRevokeAllAccesses(Sn1,IdsD,Sn2):-
    % Verifica si hay una sesion activa,
    caddr(Sn1,SA),
    \+(conectado(SA)),

    % se revocan los accesos
    caddddr(Sn1,LD),
    car(SA,Username),
    actualizarPermisos(LD,Username,IdsD,LD1),
    setListaDocumento(Sn1,LD1,PA),
    setSesionActiva(PA,[],Sn2).

%Predicado paradigmaDocsSearch:
buscarEnVersiones([],_):-fail.
buscarEnVersiones([H|T],SearchText):-
    cadr(H,ContenidoVersion),
    (sub_string(ContenidoVersion,_,_,_,SearchText)->
    %then
    !;
    %else
    buscarEnVersiones(T,SearchText)).

filtrarSearch([],_,_,ListAux,ListAux).
filtrarSearch([H|T],Username,SearchText,ListAux,ListRes):-
    caddddr(H,LP),
    cadddr(H,Contenido),
    cadddddr(H,LV),
    %if
    (((propietario(H,Username);tienePermiso(Username,LP,"W");tienePermiso(Username,LP,"R")),
     (sub_string(Contenido,_,_,_,SearchText);buscarEnVersiones(LV,SearchText)))->
    %then
    agregar(ListAux,H,ListAux1), filtrarSearch(T,Username,SearchText,ListAux1,ListRes);
    %else
    filtrarSearch(T,Username,SearchText,ListAux,ListRes)).

paradigmaDocsSearch(Sn1,SearchText,Documents):-
    % Verifica si hay una sesion activa,
    caddr(Sn1,SA),
    \+(conectado(SA)),

    % se filtra la lista de documentos de paradigmaDocs.
    car(SA,Username),
    caddddr(Sn1,LD),
    filtrarSearch(LD,Username,SearchText,[],Documents).

%Predicado paradigmaDocsDelete:
eliminar(_,0,[]).
eliminar([H|T],Id,[H|L]):-Id1 is Id - 1, eliminar(T,Id1,L).

eliminarCaracteres(Doc,NoC,Doc1):-
    cadddr(Doc,Contenido),
    string_length(Contenido,LargoCon),
    N is LargoCon - NoC,
    %if
    (N =< 0 ->
    %then
    setContenido(Doc,"",Doc1);
    %else
    string_chars(Contenido,ListCon),
    eliminar(ListCon,N,ListCon1),
    atomics_to_string(ListCon1,Contenido1),
    setContenido(Doc,Contenido1,Doc1)).

paradigmaDocsDelete(Sn1,IdD,Date,NoC,Sn2):-
    % Verifica si hay una sesion activa,
    caddr(Sn1,SA),
    \+(conectado(SA)),

    % Se busca el documento y se verifica
    % si el usuario conectado tiene permiso
    % de escritura o si es propietario.
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
    agregarAListaVersiones(LV,0,Contenido,Date,LV1),
    setListaVersiones(Documento,LV1,Documento1),

    eliminarCaracteres(Documento1,NoC,Documento2),
    actualizarLD(LD,IdD1,Documento2,LD1),
    setListaDocumento(Sn1,LD1,PA),
    setSesionActiva(PA,[],Sn2).


%Predicado paradigmaDocsSearchAndReplace:
reemplazar("",_,_,ContAux,ContAux).
reemplazar(Contenido,SearchText,ReplaceText,ContAux,ContRes):-
    %if: si no hay mas SearchText
   (\+(sub_string(Contenido,_,_,_,SearchText))->
    %then: copio lo ultimo que queda
    concatenar(ContAux,Contenido,ContAux5),reemplazar("",SearchText,ReplaceText,ContAux5,ContRes);
    %else
    string_length(SearchText,LenST),
    sub_string(Contenido,Antes,_,Despues,SearchText),

        %if: no hay nada antes
        (Antes == 0 ->
        %then: copio el ReplaceText y continuo con el string despues del SearchText
        concatenar(ContAux,ReplaceText,ContAux1),sub_string(Contenido,LenST,_,0,Contenido1),reemplazar(Contenido1,SearchText,ReplaceText,ContAux1,ContRes);
        %else: copio lo que hay antes, el ReplaceText y continuo con el sting despues del SearchText
        Despues1 is Despues + LenST,sub_string(Contenido,0,_,Despues1,Contenido2),concatenar(ContAux,Contenido2,ContAux2),
        concatenar(ContAux2,ReplaceText,ContAux3),Antes1 is Antes + LenST,sub_string(Contenido,Antes1,_,0,Contenido3),
        reemplazar(Contenido3,SearchText,ReplaceText,ContAux3,ContRes))).

reemplazarTexto(Doc,SearchText,ReplaceText,Doc1):-
    cadddr(Doc,Contenido),
    (sub_string(Contenido,_,_,_,SearchText)->
    %then
    reemplazar(Contenido,SearchText,ReplaceText,"",Contenido1),setContenido(Doc,Contenido1,Doc1);

    %else
    write("El documento no contiene el texto buscado, no se puede reemplazar."),Doc1 = Doc).

paradigmaDocsSearchAndReplace(Sn1,IdD,Date,SearchText,ReplaceText,Sn2):-
    % Verifica si hay una sesion activa,
    caddr(Sn1,SA),
    \+(conectado(SA)),

    % Se busca el documento y se verifica
    % si el usuario conectado tiene permiso
    % de escritura o si es propietario.
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
    agregarAListaVersiones(LV,0,Contenido,Date,LV1),
    setListaVersiones(Documento,LV1,Documento1),

    reemplazarTexto(Documento1,SearchText,ReplaceText,Documento2),
    actualizarLD(LD,IdD1,Documento2,LD1),
    setListaDocumento(Sn1,LD1,PA),
    setSesionActiva(PA,[],Sn2).















% ---------------------------------------------------------------------------------------------
%                                         TEST
% ---------------------------------------------------------------------------------------------

% Test 1, paradigmaDocsRegister: 5 personas se registraron en la
% plataforma en el mismo dia,
test1(PT1):-
    date(12,12,2021,D1),
    date(22,12,2021,D2),
    paradigmaDocs("DuckDocs",D1,P1),
    paradigmaDocsRegister(P1,D2,"Leo","Pass1",P2),
    paradigmaDocsRegister(P2,D2,"Pedro","Pass2",P3),
    paradigmaDocsRegister(P3,D2,"Miguel","Pass3",P4),
    paradigmaDocsRegister(P4,D2,"Sara","Pass4",P5),
    paradigmaDocsRegister(P5,D2,"Romina","Pass5",PT1).

% Test 2, paradigmaDocsLogin y paradigmaDocsCreate: distintos usuario
% crearon 5 documentos en la plataforma en el mismo dia.
test2(PT2):-test1(PT1),
    date(24,12,2021,D1),
    paradigmaDocsLogin(PT1,"Leo","Pass1",P1),
    paradigmaDocsCreate(P1,D1,"Libro de vida","Titulo1: Ayer saque a pasear a mi perro.",P2),
    paradigmaDocsLogin(P2,"Romina","Pass5",P3),
    paradigmaDocsCreate(P3,D1,"Informe de biologia","La celulas animales:",P4),
    paradigmaDocsLogin(P4,"Miguel","Pass3",P5),
    paradigmaDocsCreate(P5,D1,"Lista del supermercado","1. Huevos. 2. Leche. ",P6),
    paradigmaDocsLogin(P6,"Sara","Pass4",P7),
    paradigmaDocsCreate(P7,D1,"Tabla de valores:","1. PlayStation 5 -> 500.000. 2. Xbox 360 -> 120.000 3. Nintendo Switch lite -> 220.000",P8),
    paradigmaDocsLogin(P8,"Pedro","Pass2",P9),
    paradigmaDocsCreate(P9,D1,"Prueba","Hola mundo!",PT2).

% Test 3, paradigmaDocsShare: el usuario Miguel compartio su archivo con
% 2 usuarios, Romina compartio su archivo con 3 usuarios, Sara compartio
% su archivo con 2 usuarios y Pedro con 1 usuario.
test3(PT3):-test2(PT2),
    paradigmaDocsLogin(PT2,"Miguel","Pass3",P1),
    paradigmaDocsShare(P1,3,["W","R","C","S"],["Leo","Sara"],P2),
    paradigmaDocsLogin(P2,"Romina","Pass5",P3),
    paradigmaDocsShare(P3,2,["W"],["Leo","Sara","Pedro"],P4),
    paradigmaDocsLogin(P4,"Sara","Pass4",P5),
    paradigmaDocsShare(P5,4,["W","R","C"],["Romina","Miguel"],P6),
    paradigmaDocsLogin(P6,"Pedro","Pass2",P7),
    paradigmaDocsShare(P7,5,["W","R"],["Leo"],PT3).

% Test 4, paradigmaDocsAdd: 3 usuario agregaron contenido al documento 2
% y 1 agrego al documento 3 en la misma fecha.
test4(PT4):-test3(PT3),
    date(26,12,2021,D1),
    paradigmaDocsLogin(PT3,"Leo","Pass1",P1),
    paradigmaDocsAdd(P1,2,D1," Es el tipo de celula eucariota. ",P2),
    paradigmaDocsLogin(P2,"Romina","Pass5",P3),
    paradigmaDocsAdd(P3,2,D1,"Es decir poseen un nucleo bien definido. ",P4),
    paradigmaDocsLogin(P4,"Pedro","Pass2",P5),
    paradigmaDocsAdd(P5,2,D1,"Partes importantes: 1. Mitocondrias, 2. Nucleo, 3. Aparato de Golgi, entre otros.",P6),
    paradigmaDocsLogin(P6,"Sara","Pass4",P7),
    paradigmaDocsAdd(P7,3,D1,"3. la Lechuga. 4. Bebida y jugos. 5. Sal",PT4).

% Test 3, paradigmaDocsRestoreVersion: se restauraron 2 versiones del
% documento 2 y 1 del documento 3 en la misma fecha.
test5(PT5):-test4(PT4),
    date(27,12,2021,D1),
    paradigmaDocsLogin(PT4,"Romina","Pass5",P1),
    paradigmaDocsRestoreVersion(P1,D1,2,2,P2),
    paradigmaDocsLogin(P2,"Miguel","Pass3",P3),
    paradigmaDocsRestoreVersion(P3,D1,3,0,P4),
    paradigmaDocsLogin(P4,"Romina","Pass5",P5),
    paradigmaDocsRestoreVersion(P5,D1,2,3,PT5).

pTs1():-test3(X),paradigmaDocsToString(X,Str),write(Str).
pTs2():-test4(X),paradigmaDocsToString(X,Str),write(Str).
pTs3():-test5(X),paradigmaDocsToString(X,Str),write(Str).

test6(PT6):-test5(PT5),
    paradigmaDocsLogin(PT5,"Romina","Pass5",P1),
    paradigmaDocsRevokeAllAccesses(P1,null,P2),
    paradigmaDocsLogin(P2,"Miguel","Pass3",P3),
    paradigmaDocsRevokeAllAccesses(P3,null,P4),
    paradigmaDocsLogin(P4,"Leo","Pass1",P5),
    paradigmaDocsRevokeAllAccesses(P5,null,PT6).

test7():-test6(PT6),
    paradigmaDocsLogin(PT6,"Romina","Pass5",P1),
    paradigmaDocsSearch(P1,"la",P2),write(P2),
    paradigmaDocsLogin(PT6,"Miguel","Pass3",P3),
    paradigmaDocsSearch(P3,"el",P4),write(P4),
    paradigmaDocsLogin(PT6,"Leo","Pass1",P5),
    paradigmaDocsSearch(P5,"hola",PT7),write(PT7).

test8(PT8):-test5(PT5),
    date(30,12,2021,D1),
    paradigmaDocsLogin(PT5,"Romina","Pass5",P1),
    paradigmaDocsDelete(P1,2,D1,10,P2),
    paradigmaDocsLogin(P2,"Miguel","Pass3",P3),
    paradigmaDocsDelete(P3,4,D1,2,P4),
    paradigmaDocsLogin(P4,"Leo","Pass1",P5),
    paradigmaDocsDelete(P5,1,D1,100,PT8).

test9(PT9):-test5(PT5),
    date(31,12,2021,D1),
    paradigmaDocsLogin(PT5,"Romina","Pass5",P1),
    paradigmaDocsSearchAndReplace(P1,2,"a","e",D1,P2),
    paradigmaDocsLogin(P2,"Miguel","Pass3",P3),
    paradigmaDocsSearchAndReplace(P3,4,"i","u",D1,P4),
    paradigmaDocsLogin(P4,"Leo","Pass1",P5),
    paradigmaDocsSearchAndReplace(P5,1,"Titulo1:","Sabado 22:",D1,PT9).








