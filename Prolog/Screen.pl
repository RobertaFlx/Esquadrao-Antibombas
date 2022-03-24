% padrão de pixel
emptyPxl(' ').

% largura do pixel
pixelWidth(2).

% largura do terminal
termWidth(W):- tty_size(_,W).

% altura do terminal
termHeight(H):- tty_size(H,_).

% largura da tela em pixels
width(X) :- 
    termWidth(TW),
    pixelWidth(PW), 
    X is div(TW, PW).

% altura da tela em pixels
height(Y) :- termHeight(Y).

% cria um pixel a partir de um character
pixel(C, X) :- 
    pixelWidth(PW),
    replicate(C, PW, L),
    atomic_list_concat(L, X).

% cria um buffer a partir da largura, da altura e do conteudo do pixel
createScreenBuffer(W, H, C, Buf) :-
    pixel(C, Pxl),
    replicate(Pxl, W, Row), 
    replicate(Row, H, Buf).

% retorna o tamanho da maior linha
bufferWidth(Buf, W):- 
    maplist(length, Buf, Ls), 
    max_list(Ls, W).

% retorna o numero de linhas
bufferHeight(Buf, H):- length(Buf, H).

% retorna as dimensoes de um buffer
bufferSize(Buf, W, H):- 
    bufferWidth(Buf, W), 
    bufferHeight(Buf, H).

% renderiza um buffer dentro de outro com posicao relativa ao centro
renderCentralized(Src, Tgt, RowOff, ColOff, R):-
    bufferSize(Src, SW, SH), 
    bufferSize(Tgt, TW, TH), 
    Row is TH // 2 - SH // 2 + RowOff, 
    Col is TW // 2 - SW // 2 + ColOff, 
    renderInBuffer(Src, Tgt, Row, Col, R).
   
% renderiza um buffer dentro de outro
renderInBuffer(_, [], _, _, []):- !.
renderInBuffer([], Tgt, _, _, Tgt):- !.
renderInBuffer([SRow|Scr], [TRow|Tgt], 0, Col, R):-
    renderInBufferRow(SRow, TRow, Col, RRow), 
    renderInBuffer(Scr, Tgt, 0, Col, Rest), 
    append([RRow], Rest, R), !.
renderInBuffer(Src, [TRow|Tgt], Row, Col, R):- 
    NR is Row - 1, 
    renderInBuffer(Src, Tgt, NR, Col, Rest),
    append([TRow], Rest, R), !.

renderInBufferRow(_, [], _, []):- !.
renderInBufferRow([], Tgt, _, Tgt):- !.
renderInBufferRow([SPxl|Scr], [_|Tgt], 0, R):-
    renderInBufferRow(Scr, Tgt, 0, Rest), 
    append([SPxl], Rest, R), !.
renderInBufferRow(Scr, [TPxl|Tgt], Col, R):-
    NC is Col - 1, 
    renderInBufferRow(Scr, Tgt, NC, Rest), 
    append([TPxl], Rest, R), !.

createBufferFromStringMatrix(Matrix, Buf):-
    maplist(stringToBufferRow, Matrix, Buf).

stringToBufferRow("", []):- !.
stringToBufferRow(Text, RowList):-
    pixelWidth(PW), 
    string_length(Text, TL), 
    TL >= PW, 
    sub_string(Text, 0, PW, _, Pxl), 
    sub_string(Text, PW, _, 0, StrRest), 
    stringToBufferRow(StrRest, Rest), 
    append([Pxl], Rest, RowList), !.
stringToBufferRow(Text, RowList):-
    pixelWidth(PW), 
    string_length(Text, TL), 
    Comp is PW - TL, 
    create_string(' ', Comp, CompStr), 
    atom_concat(Text, CompStr, CompText), 
    stringToBufferRow(CompText, RowList).

% imprime buffer na tela
printScreen(Buf):-
    maplist(atomic_list_concat, Buf, RowList),
    atomic_list_concat(RowList, '\n', BufStr),
    write(BufStr).

replicate(X, N, L) :-
    length(L, N),
    maplist(=(X), L).
    
create_string(X, N, Str):- 
    replicate(X, N, L),
    atomic_list_concat(L, Str).

waitKey(ValidKeys, ReturnedKey) :-
    get_single_char(X),
    char_code(Y, X),
    (member(Y, ValidKeys) ->
    ReturnedKey = Y;
    waitKey(ValidKeys, K), ReturnedKey = K).

printArrow([],_,[]).
printArrow([Row|Rest],I,R):-
    (I =:= 0, 
     string_concat(Row," <<<",X),
     string_concat(" ", X, Z),
     R = [Z|Rest];
     N is I-1,
     I >=0,
     printArrow(Rest, N, Z),
     R = [Row|Z]).
