%%----------------------------------------------------------------
%% compile("/Users/mac/Downloads/CalenExames/project.ecl").
%%
%%----------------------------------------------------------------

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).
:- compile(basedados).
    /*
       basedados.pl tera predicados indicados no enunciado
            unidade_curr(CodigoDisc,Ano,NomeDisc)
            estudante(CodigoAluno,NomeAluno,ListaCodigosDiscInscrito)
            numeroDiasUteis(NumeroDiasUteis)
            espacamentosMinimos(ListaEspacamentosOrdenadaPorAnos)
    */






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


d :- obter_dados(Unidade_curr,Estudante,E,D),
    length(Unidade_curr,NDisc), length(X,NDisc),
    X #:: 1..30, Y #:: 0..29,  %Y #>= 0, Y #< 30,

    restrs_espacamento(E,1,X,Unidade_curr),
    restrs_sobreposicoes(Estudante,X,Unidade_curr),
    
    restrs_anos_consecutivos(Unidade_curr,X,L1,L2),
    
    restrs_estender_epoca(X,Y,D),
    minimize(labeling([Y|X]),Y),
    escrever_solucao(X,Y,Unidade_curr).

obter_dados(Unidade_curr,Estudante,E,D) :-
    findall(I,unidade_curr(I,_,_,_,_),Unidade_curr),
    findall(J,estudante(J,_,_),Estudante),
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


restrs_estender_epoca([],_,_).
restrs_estender_epoca([Xi|RX],Y,D) :- Xi #=< D+Y,
    restrs_estender_epoca(RX,Y,D).
%seleccionar_vars(DiscAnoK,Unidade_curr,X,XDiscAnoK),
seleccionar_vars([],_,_,[]).
seleccionar_vars([I|RDisc],Unidade_curr,X,[Xi|XRDisc]) :-
    selec_elemento(1,T,Unidade_curr,I), %Retorna o index do I
    selec_elemento(1,T,X,Xi), %  Xi=X[t] Posições de memoria?
    seleccionar_vars(RDisc,Unidade_curr,X,XRDisc).

selec_elemento(T,T,[I|_],I) :- !.
selec_elemento(T0,T,[_|R],I) :- T0n is T0+1, selec_elemento(T0n,T,R,I).

escrever_solucao(X,Y,Unidade_curr) :- Y = 0,
    nl, escrever_calendario(Unidade_curr,X).
escrever_solucao(X,Y,Unidade_curr) :- Y \= 0,
    write('Estender epoca de '), write(Y), write(' dias uteis'), nl,
    nl, escrever_calendario(Unidade_curr,X).


escrever_calendario([],[]).
escrever_calendario([I|RDisc],[Xi|RX]) :-
    unidade_curr(I,NomeDisc,_,_,_),
    write(NomeDisc:Xi), nl,
    escrever_calendario(RDisc,RX).
