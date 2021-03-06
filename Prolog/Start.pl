:- use_module(library(clpfd)).

red().
white().
green().
yellow().
cyan().

startGame(QuantLinhas,QuantColunas,QuantBombasLetais,QuantBombas) :-
    writeln(''),
	matrizGenerate(QuantColunas, QuantLinhas, QuantBombasLetais, QuantBombas, Matriz),
	!,
	get_time(Time),
	start(Matriz,Time),
	!.

start(Matriz,Time) :- % jogador ganhou
	traversesMatriz(win, Matriz),
	traversesMatriz(printMatrizUsuario, Matriz),
	writeln('
 __________________________________________ 
|                                          | 
|         █▀▀ █▀█ █▄ █ █ █ █▀█ █ █         | 
|         █ ▄ █▀█ █ ▀█ █▀█ █ █ █ █         |
|         ▀▀▀ ▀ ▀ ▀  ▀ ▀ ▀ ▀▀▀  ▀▀         |
|__________________________________________|'),halt.

start(Matriz,Time) :- % jogador perdeu
	\+ traversesMatriz(playing, Matriz),
	traversesMatriz(printMatrizInterna, Matriz),
	writeln('
 __________________________________________ 
|                                          |
|         █▀█ █▀▀ █▀█ █▀▄ █▀▀ █ █          |
|         █▀▀ █▀  █▀▄ █ █ █▀  █ █          |
|         ▀   ▀▀▀ ▀ ▀ ▀▀  ▀▀▀  ▀▀          |
|__________________________________________|'),halt.
	  
verify(TimeDif):-(TimeDif >= 540,
	writeln('
 __________________________________________ 
|                                          |
|          ▀█▀ █▀▀ █▀▄▀█ █▀█ █▀█           |
|           █  █▀  █ █ █ █▀▀ █ █           |
|           ▀  ▀▀▀ ▀ ▀ ▀ ▀   ▀▀▀           |
|                                          |
|     █▀▀ █▀▀ █▀▀ █▀█ ▀█▀ █▀█ █▀▄ █▀█      |
|     █▀  ▀▀█ █ ▄ █ █  █  █▀█ █ █ █ █      |
|     ▀▀▀ ▀▀▀ ▀▀▀ ▀▀▀  ▀  ▀ ▀ ▀▀  ▀▀▀      |
|__________________________________________|'),halt; TimeDif < 540).
	  
start(Matriz,Time) :- % jogador nem ganhou nem perdeu
	traversesMatriz(printMatrizUsuario, Matriz),
	writeln(''),
	writeln('Informe sua Jogada:'),
	getMove(Jogada, X, Y),
	writeln(''),
	get_time(TimeAtual),
	TimeDif is TimeAtual - Time,
	verify(TimeDif), 
	writeln(''),
	% writeln(Matriz),
	actions(Jogada, coordenada(X,Y), Matriz, MatrizRevelada),
	!,
	start(MatrizRevelada,Time).


% Cria uma nova matriz que será o campo.
matrizGenerate(QuantLinhas, QuantColunas, QuantBombasLetais, QuantBombas, campo(QuantLinhas,QuantColunas,PosicoesMapeadas)) :-
	% gera uma lista inicial de posições
	TotalPosicoes is QuantLinhas * QuantColunas,
	length(Posicoes, TotalPosicoes),

	% gera uma lista de bombas desarmáveis
	length(ListaDeNormais, QuantBombas),
	maplist(=('B'), ListaDeNormais), 
	
	% gera uma lista de bombas letais
	length(ListaDeLetais, QuantBombasLetais),
	maplist(=('L'), ListaDeLetais), 
	
	% concatena as duas listas de bombas em uma 
	append(ListaDeNormais, ListaDeLetais, ListaBombasJuntas),

	% concatena a lista de bombas com a lista inicial
	addBombs(Posicoes, ListaBombasJuntas, ListaComBombas), 

	% Permuta a lista, colocando as bombas normais e letais em posições aleatórias
	random_permutation(ListaComBombas, ListaFinal), 
	
	% converte a lista final para uma matriz
	generateMatriz(ListaFinal, QuantLinhas, QuantColunas, MatrizFinal),
	
	% writeln(MatrizFinal),

	traversesMatriz(countAdjacentLetais(campo(QuantLinhas,QuantColunas,MatrizFinal)),
	campo(QuantLinhas,QuantColunas,MatrizFinal), 
	campo(QuantLinhas,QuantColunas,PosicoesMapeadas)).
	
% Pega a jogada feita pelo usuário.
getMove(Jogada, X, Y) :-
	read_line_to_codes(user_input, In),
	maplist(char_code, InChars, In),
	phrase(typeMove(Jogada, Y, X), InChars, []). 


% Identifica tipo de jogada feita.
typeMove(alerta, X, Y) --> ['A','l','e','r','t','a'], [' '], coords(X, Y).
typeMove(desativa, X, Y) --> ['D','e','s','a','t','i','v','a','r'], [' '], coords(X, Y).
typeMove(revela, X, Y) --> ['A','b','r','i','r'], [' '], coords(X, Y).
coords(Xi, Yi) --> number_(X), { number_chars(Xi, X) }, [' '], number_(Y), { number_chars(Yi, Y) }.

number_([D|T]) --> digit(D), number_(T).
number_([D]) --> digit(D).
digit(D) --> [D], { char_type(D, digit) }. 	

% função auxiliar que percorre toda a matriz.
traversesMatriz(PosicaoFinal, Matriz) :- traversesMatriz(PosicaoFinal, Matriz, Matriz).
traversesMatriz(PosicaoFinal, campo(QuantLinhas,QuantColunas,Posicoes), campo(QuantLinhas,QuantColunas,MatrizFinal)) :-
	traverseColumns(Posicoes, 1, dim(QuantLinhas,QuantColunas), PosicaoFinal, MatrizFinal).

traverseColumns([], _, _, _, []). 
traverseColumns([QuantColunas|T], Y, Z, PosicaoFinal, [NLinha|NColuna]) :-
	traverseLines(QuantColunas, coordenada(1, Y), Z, PosicaoFinal, NLinha),
	succ(Y, Y1),
	traverseColumns(T, Y1, Z, PosicaoFinal, NColuna).

traverseLines([], _, _, _, []).
traverseLines([QuantColunas|T], coordenada(X, Y), Z, PosicaoFinal, [Posicao|L]) :-
	call(PosicaoFinal, coordenada(X, Y), Z, QuantColunas, Posicao),
	succ(X, X1),
	traverseLines(T, coordenada(X1, Y), Z, PosicaoFinal, L).	
	
printMatrizUsuario(coordenada(X,_), dim(X,_), posicao(Z,A), posicao(Z,A)) :-
	 (Z =@= 'L', colorRed2(Z); 
	  Z =@= 'B', colorGreen2(Z);
	  Z =@= 'A', colorYellow2(Z);
	  Z =@= 'D', colorCyan2(Z);
	 format(" ~w~n", Z)).
printMatrizUsuario(coordenada(X,_), dim(QuantLinhas,_), posicao(Z,A), posicao(Z,A)) :- 
	dif(X,QuantLinhas), (
		Z =@= 'L', colorRed1(Z);
		Z =@= 'B', colorGreen1(Z);
		Z =@= 'A', colorYellow1(Z);
        Z =@= 'D', colorCyan1(Z);
		format(" ~w ", Z)).

colorRed1(Z):- ansi_format([bold,fg(red)], ' ~w ', [Z]).
colorRed2(Z):- ansi_format([bold,fg(red)], ' ~w~n', [Z]).

colorGreen1(Z):- ansi_format([bold,fg(green)], ' ~w ', [Z]).
colorGreen2(Z):- ansi_format([bold,fg(green)], ' ~w~n', [Z]).

colorYellow1(Z):- ansi_format([bold,fg(yellow)], ' ~w ', [Z]).
colorYellow2(Z):- ansi_format([bold,fg(yellow)], ' ~w~n', [Z]).

colorCyan1(Z):- ansi_format([bold,fg(cyan)], ' ~w ', [Z]).
colorCyan2(Z):- ansi_format([bold,fg(cyan)], ' ~w~n', [Z]).

printMatrizInterna(coordenada(X,_), dim(X,_), posicao(Z,A), posicao(Z,A)) :- 
	 (A =@= 'L', colorRed2(A); 
	  A =@= 'B', colorGreen2(A);
	  A =@= 'A', colorYellow2(A);
      A =@= 'D', colorCyan2(A);
	 format(" ~w~n", A)).
printMatrizInterna(coordenada(X,_), dim(QuantLinhas,_), posicao(Z,A), posicao(Z,A)) :-
	dif(X,QuantLinhas), (
		A =@= 'L', colorRed1(A);
		A =@= 'B', colorGreen1(A);
		A =@= 'A', colorYellow1(A);
		A =@= 'D', colorCyan1(A);
		format(" ~w ", A)).	

% função para concatenar as bombas na lista
addBombs(P, [], P).
addBombs([_|Pt], [B|Bt], [B|L]) :-
	addBombs(Pt, Bt, L).

generateMatriz([], _, 0, []).
generateMatriz(P, QuantLinhas, QuantColunas, [Linha|PosicoesRestantes]) :-
	dif(QuantColunas, 0), succ(QuantLinhasAux, QuantColunas),
	generateLines(P, QuantLinhas, Linha, Resto),
	generateMatriz(Resto, QuantLinhas, QuantLinhasAux, PosicoesRestantes).

generateLines(T, 0, [], T).
generateLines([H|T], QuantLinhas, [H|L], Resto) :-
	dif(QuantLinhas, 0), succ(QuantColunasAux, QuantLinhas),
	generateLines(T, QuantColunasAux, L, Resto).

% calcula o valor de bombas letais adjacentes à cada posição. 
countAdjacentLetais(_, _, _, Z, posicao('*',Z)) :- Z =@= 'L'.
countAdjacentLetais(_, _, _, H, posicao('*',H)) :- H =@= 'B'.
countAdjacentLetais(Matriz, coordenada(X,Y), D, Posicao, posicao('*',NumBombasLetais)) :-
	dif(Posicao, 'L'), 
	findall(coordenada(Ax,Ay), (
		dif(Posicao, 'B'),
		adjacentsPositions(coordenada(X,Y), D, coordenada(Ax,Ay)),
		indomain(Ax), indomain(Ay),
		getCoordenada(Matriz, coordenada(Ax,Ay), Val),
		Val =@= 'L'
	), BombasLetais),
	length(BombasLetais, NumBombasLetais).

% retorna o valor na posição xy
getCoordenada(campo(_,_,Posicoes), coordenada(X,Y), Val) :- 
	nth1(Y, Posicoes, Linha), % nth1 pega a N-ésima posição da lista,
							  % no caso, a x-ésima posição em Posicoes, que é uma linha
	nth1(X, Linha, Val). % aqui a y-ésima posição é um valor

% define o valor na posicao xy
modifyMatriz(campo(QuantLinhas,QuantColunas,Posicoes), coordenada(X,Y), Val, campo(QuantLinhas,QuantColunas,NovaPosicao)) :- defineColumn(Posicoes, X, Y, Val, NovaPosicao).
defineColumn([QuantColunas|T], X, 1, Val, [Linha|T]) :- defineLine(QuantColunas, X, Val, Linha).
defineColumn([QuantColunas|T], X, Y, Val, [QuantColunas|Nova]) :- dif(Y, 0), succ(Y1, Y), defineColumn(T, X, Y1, Val, Nova).
defineLine([_|T], 1, Val, [Val|T]).
defineLine([QuantColunas|T], X, Val, [QuantColunas|Nova]) :- dif(X, 0), succ(X1, X), defineLine(T, X1, Val, Nova).

% retorna as posições adjacentes a uma posição.
adjacentsPositions(coordenada(X,Y), dim(QuantLinhas,QuantColunas), coordenada(Ax,Ay)) :-
	dif(coordenada(X,Y),coordenada(Ax,Ay)), 
	Ax in 1..QuantLinhas,
	Ymin #= X-1, Ymax #= X+1,
	Ax in Ymin..Ymax,

	Ay in 1..QuantColunas,
	Xmin #= Y-1, Xmax #= Y+1,
	Ay in Xmin..Xmax.

% Jogada que adiciona um alerta A na posição especificada.
actions(alerta, Posicao, Matriz, MatrizComNovoAlerta) :-
	getCoordenada(Matriz, Posicao, posicao(_,Z)),
	modifyMatriz(Matriz, Posicao, posicao('A',Z), MatrizComNovoAlerta).
	
% Jogada que desativa uma bomba na posição especificada.
actions(desativa, Posicao, Matriz, MatrizDesativada) :-
	getCoordenada(Matriz, Posicao, posicao(_,Z)),
	modifyMatriz(Matriz, Posicao, posicao('D',Z), MatrizDesativada).

% Jogada que abre caminho em uma posição que contem uma bomba letal, resultando no fim do jogo.
actions(revela, Posicao, Matriz, MatrizDerrota) :-
	getCoordenada(Matriz, Posicao, posicao(_,'L')),
	modifyMatriz(Matriz, Posicao, posicao('L','L'), MatrizDerrota). 

% Jogada que abre caminho e releva posições seguras da matriz.
actions(revela, Posicao, Matriz, MatrizRevelada) :-
	getCoordenada(Matriz, Posicao, posicao(_,Z)),
	dif(Z, 'L'), % dif(Z,'B'),
	modifyMatriz(Matriz, Posicao, posicao(Z,Z), MatrizAlterada),
	revealsMatriz(Posicao, MatrizAlterada, MatrizRevelada).
	
% revela recursivo
revealsMatriz(coordenada(X,Y), campo(QuantLinhas,QuantColunas,Posicoes), MatrizRevelada) :-
	findall(coordenada(Ax,Ay), (
		    adjacentsPositions(coordenada(X,Y), dim(QuantLinhas,QuantColunas), coordenada(Ax,Ay)),
		    indomain(Ax), indomain(Ay)
		), Coords),
	revealing(Coords, campo(QuantLinhas,QuantColunas,Posicoes), MatrizRevelada).

revealing([], Matriz, Matriz).

% se a posição já tiver sido revelada
revealing([QuantColunas|T], Matriz, MatrizRevelada) :- 
	getCoordenada(Matriz, QuantColunas, posicao(X,Y)),
	member(X, [Y,'L']),
	revealing(T, Matriz, MatrizRevelada).

% se a posição for uma bomba letal, não revela nenhum outro campo.
revealing([QuantColunas|T], Matriz, MatrizRevelada) :- 
	getCoordenada(Matriz, QuantColunas, posicao(_,'L')),
	revealing(T, Matriz, MatrizRevelada).

% se a posição for uma bomba normal, não revela nenhum outro campo.
revealing([QuantColunas|T], Matriz, MatrizRevelada) :- 
	getCoordenada(Matriz, QuantColunas, posicao(_,'B')),
	revealing(T, Matriz, MatrizRevelada).
	
% se a posição tem bombas ao redor
revealing([QuantColunas|T], Matriz, MatrizRevelada) :- 
	getCoordenada(Matriz, QuantColunas, posicao(_,Z)),
	integer(Z),
	Z #> 0,
	modifyMatriz(Matriz, QuantColunas, posicao(Z,Z), MatrizAlterada), 
	revealing(T, MatrizAlterada, MatrizRevelada).

% se não tem bombas ao redor, revela posições seguras de forma recursiva.
revealing([QuantColunas|T], Matriz, MatrizRevelada) :- 
	getCoordenada(Matriz, QuantColunas, posicao('*',0)),
	modifyMatriz(Matriz, QuantColunas, posicao(0,0), MatrizAlterada),
	revealsMatriz(QuantColunas, MatrizAlterada, MatrizParaRevelar),
	revealing(T, MatrizParaRevelar, MatrizRevelada).

playing(_,_,posicao(Z,_),_) :- Z \= 'L'.

win(_,_,posicao(Z,Z),_) :- integer(Z).
win(_,_,posicao('A','L'),_).
win(_,_,posicao('D','B'),_).
