:- initialization(main).
:- include('Screen.pl').
:- include('Credits').
:- include('Rules.pl').
:- include('Start.pl').

title([ 
    " ____________________________________________________ ",
    "|                                                    |", 
    "|                                                    |", 
    "|        █▀▀ █▀▀ █▀█ █ █ █▀█ █▀▄ █▀█ █▀█ █▀█         |", 
    "|        █▀  ▀▀█ █ █ █ █ █▀█ █ █ █▀▄ █▀█ █ █         |",
    "|        ▀▀▀ ▀▀▀ ▀▀▀▀ ▀▀ ▀ ▀ ▀▀  ▀ ▀ ▀ ▀ ▀▀▀         |",
    "|                                                    |",
    "|     █▀█ █▄ █ ▀█▀ █ █▀▀▄ █▀█ █▀▄▀█ █▀▀▄ █▀█ █▀▀     |", 
    "|     █▀█ █ ▀█  █  █ █▀▀▄ █ █ █ █ █ █▀▀▄ █▀█ ▀▀█     |",
    "|     ▀ ▀ ▀  ▀  ▀  ▀ ▀▀▀▀ ▀▀▀ ▀ ▀ ▀ ▀▀▀▀ ▀ ▀ ▀▀▀     |",
    "|                                                    |", 
    "|____________________________________________________|"]).
    

menu([ 
    "Iniciar   ", 
    "Regras    ",
    "Creditos  ", 
    "Sair      "]).

commandsTable([
    " s / w - mover cursor  ", 
    " f - selecionar        "]).
                   
printMenu(MenuTab):-

    width(Width),
    height(Height), 
    emptyPxl(EmptyPxl),
    title(Title),
    commandsTable(CommandsTable),

    createScreenBuffer(Width, Height, EmptyPxl, InitialBuffer),
    createBufferFromStringMatrix(Title, TitleBuf),
    createBufferFromStringMatrix(MenuTab, MenuBuf),
    createBufferFromStringMatrix(CommandsTable, TableBuf),

    renderCentralized(TitleBuf,InitialBuffer, (-4), 0, Tmp1),
    renderCentralized(MenuBuf, Tmp1, 10, 0, Tmp2),
    renderInBuffer(TableBuf, Tmp2, 3, 1, Tmp3),
    
    printScreen(Tmp3), !.
    
mainLoop(Index):-

    MaxIndex = 4, 
    menu(Menu),
    printArrow(Menu, Index, MenuTab),
    printMenu(MenuTab),

    waitKey(['w','s','f'], Key),  
    
    (Key = 'w', NewIndex = ((Index - 1) + MaxIndex) mod MaxIndex;
	 Key = 's', NewIndex = ((Index + 1) + MaxIndex) mod MaxIndex;
     Key = 'f', Index =:= 0, startGame(9,9,5,4); % Iniciar Jogo
     Key = 'f', Index =:= 1, rules;
     Key = 'f', Index =:= 2, credits;
     Key = 'f', Index =:= 3, halt),

    mainLoop(NewIndex).
    
main:- mainLoop(0).

