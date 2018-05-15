% codigo da unidade curricular | nome | ano | duracao | hora pref
% fica a faltar hora pref
unidade_curr(1,'Introducao a Programacao',1,2,9).
unidade_curr(2,'Matematica I',1,2,10).
unidade_curr(5,'Estruturas de Dados',1,2,9).
unidade_curr(6,'Matematica II',1,2,14).
unidade_curr(3,'Estruturas Discretas',1,2,15).
unidade_curr(4,'Introducao aos Computadores',1,1,11).
unidade_curr(7,'Desenho e Analise de Algoritmos',2,3,9).
unidade_curr(8,'Probabilidades e Estatistica',2,2,14).
unidade_curr(9,'Analise Infinitesimal I',2,1,15).
unidade_curr(14,'Sistemas de Operacao',2,2,16).
unidade_curr(10,'Bases de Dados',2,2,14).
unidade_curr(11,'Metodos de Apoio a Decisao',3,2,9).
unidade_curr(12,'Computabilidade e Complexidade',3,2,11).
unidade_curr(13,'Compiladores',3,1,11).

% codigo estudante | Nome | Lista de codigos das unidades curriculares incritas
estudante(20151,'Joao',[1,3,6,7,10]).
estudante(20152,'Maria',[1,5,6,10]).
estudante(20153,'Tiago',[1,5,6,8,7]).
estudante(20121,'Mafalda',[1,2,3,5,6,9]).
estudante(20131,'Joao',[8,9,10]).
estudante(20132,'Andre',[1,5,6,3]).
estudante(20161,'Nuno',[2,3,8,12]).
estudante(20111,'jose',[12,13,1,4]).
estudante(20154,'Amorim',[2,3,4,5]).
estudante(20141,'Joao',[1,3,6,7,10]).
estudante(20142,'Maria',[1,5,6,10]).
estudante(20133,'Tiago',[1,5,6,8,7]).
estudante(20162,'Mafalda',[1,2,3,5,6,9]).
estudante(20163,'Joao',[8,9,10]).


% numero de dias uteis da epoca normal
% fica a faltar implementar a epoca de recurso
%numeroDiasUteis(10).
%calendario(Epoca,ListaOrdenadaDatasPossiveis) d(DiaMes,Mes,DiaSemana)
calendario(1,[d(4,6,2),d(5,6,3),d(6,6,4),d(7,6,5),d(8,6,6),d(11,6,2),d(12,6,3),d(13,6,4),d(14,6,5),d(15,6,6)]).

% lista espacamentos minimos por ano | intervalo min para almoco
intervalos_anos([4,3,2],[2,1,1]).

capacidade_sala(207, 5).
capacidade_sala(206, 3).
capacidade_sala(009, 2).
capacidade_sala(114, 4).
