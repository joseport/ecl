%%----------------------------------------------------------------
%% compile("/Users/mac/Downloads/CalenExames/project.ecl").
%%
%%----------------------------------------------------------------

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).
:- compile(basedados).

d :- obter_dados(Unidade_curr,Estudante,E,D,K),
    length(Unidade_curr,NDisc), length(X,NDisc),
    X #:: K,
    restrs_espacamento(E,1,X,Unidade_curr),
    restrs_sobreposicoes(Estudante,X,Unidade_curr),
    restrs_anos_consecutivos(Unidade_curr,X,L1,L2),
    %restrs_estender_epoca(X,D),
    labeling(X),
    restrs_salas(Unidade_curr,Estudante,X),

    escrever_calendario(Unidade_curr,X).

restrs_salas(Unidade_curr,Estudante,X):-
  searchRepetidos(X,L,Repetidos),
  inscritos(Unidade_curr,Estudante,I,Inscritos),
  write(Inscritos),nl.




inscritos([],Estudante,Inscritos,V):- append(Inscritos,[],V).
inscritos([H|T],Estudante,Inscritos,V):-

  C is 0,
  inscritos_(H,Estudante,C,Cont),
  vazio(Cont,Inscritos,Ins),
  inscritos(T,Estudante,Ins,V).

inscritos_(Codigo,[],C,[C]):- !.
inscritos_(Codigo,[H|T],C,Cont):-
  estudante(H,_,Cadeiras),
  (member(Codigo,Cadeiras), Cn is C+1,inscritos_(Codigo,T,Cn,Cont));
  inscritos_(Codigo,T,C,Cont).

vazio([],[],L2).
vazio([],L1,L2) :- append([],L1,L2).
vazio(L,[],L2) :- append([],L,L2).
vazio(L,L1,L2) :- append(L1,L,L2).

searchRepetidos([_],L,V):- append([],L,V).
searchRepetidos([P|Rest],L,V) :-

    search_(Rest,L1,P),
    %append(L,L1,L2),
    vazio(L1,L,L2),
    searchRepetidos(Rest,L2,V).

search_([],L,P):- !.%write(L),nl.
search_([P|Rest],L,P) :- search_([], L2, P),append(L2,[P],L).
search_([S|Rest],L,P) :- search_(Rest, L, P).


restrs_anos_consecutivos(Unidade_curr,X,L1,L2) :- 
    findall(I, unidade_curr(I,_,1,_,_),Lt1),
    findall(I, unidade_curr(I,_,2,_,_),Lt2),
    findall(I, unidade_curr(I,_,3,_,_),Lt3),
    append(Lt1,Lt2,L1),
    append(Lt2,Lt3,L2),
    seleccionar_vars(L1,Unidade_curr,X,Consec1),
    seleccionar_vars(L2,Unidade_curr,X,Consec2),
    ic_global:alldifferent(Consec1),
    ic_global:alldifferent(Consec2).

obter_dias_( d(D,_,_) ,D):- !.

obter_dias([],K):- !.
obter_dias([CHead|CRest], K) :-
    obter_dias_(CHead,D),
    obter_dias(CRest,K1),
    append(K1,[D],K).


obter_dados(Unidade_curr,Estudante,E,D,K) :-
    findall(I,unidade_curr(I,_,_,_,_),Unidade_curr),
    findall(J,estudante(J,_,_),Estudante),
    calendario(_,C),
    obter_dias(C,K),
    intervalos_anos(E,_),
    calendario(_,A), % Modificado para obter o numero de dias uteis atravez do calendario
    length(A,D).


restrs_espacamento([],_,_,_).
restrs_espacamento([Ek|RE],K,X,Unidade_curr) :-
    findall(I,unidade_curr(I,_,K,_,_),DiscAnoK), % discanok tem a lista de cadeiras por ano
    seleccionar_vars(DiscAnoK,Unidade_curr,X,XDiscAnoK),
    restrs_espacamento_k(XDiscAnoK,Ek),
    K1 is K+1, restrs_espacamento(RE,K1,X,Unidade_curr).

restrs_espacamento_k([],_).
restrs_espacamento_k([_],_).
restrs_espacamento_k([Xi,Xil|RX],Ek) :-
    restrs_espacamento_k_([Xil|RX],Xi,Ek),
    restrs_espacamento_k([Xil|RX],Ek).

restrs_espacamento_k_([],_,_).
restrs_espacamento_k_([Xil|RX],Xi,Ek) :-
    abs(Xi-Xil) #>= Ek,
    restrs_espacamento_k_(RX,Xi,Ek).

restrs_sobreposicoes([],_,_).
restrs_sobreposicoes([J|REstudante],X,Unidade_curr) :-
    estudante(J,_,DiscAlunoJ),
    seleccionar_vars(DiscAlunoJ,Unidade_curr,X,XDiscAlunoJ),
    ic_global:alldifferent(XDiscAlunoJ),
    restrs_sobreposicoes(REstudante,X,Unidade_curr).

%seleccionar_vars(DiscAnoK,Unidadse_curr,X,XDiscAnoK),
seleccionar_vars([],_,_,[]).
seleccionar_vars([I|RDisc],Unidade_curr,X,[Xi|XRDisc]) :-
    selec_elemento(1,T,Unidade_curr,I), %Retorna o index do I
    selec_elemento(1,T,X,Xi), %  Xi=X[t] Posições de memoria?
    seleccionar_vars(RDisc,Unidade_curr,X,XRDisc).

selec_elemento(T,T,[I|_],I) :- !.
selec_elemento(T0,T,[_|R],I) :- T0n is T0+1, selec_elemento(T0n,T,R,I).



escrever_calendario([],[]).
escrever_calendario([I|RDisc],[Xi|RX]) :-
    unidade_curr(I,NomeDisc,_,_,_),
    write(NomeDisc:Xi), nl,
    escrever_calendario(RDisc,RX).
