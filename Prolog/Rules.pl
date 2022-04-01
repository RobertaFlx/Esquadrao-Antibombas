textoRules([
    " _______________________________________________________________________________________________________________________________",
    "|                                                                                                                               |",
    "|   Você está sendo treinado para ser um especialista em desarmamento de bombas e foi chamado para um teste prático em campo.   |",
    "|                                                                                                                               |",
    "|   Ao chegar no lugar indicado, é informado que foram implantadas bombas comuns, que podem ser desarmadas, e bombas letais,    |",
    "|   que você ainda não tem treinamento necessário para desativar. O objetivo do jogo é conseguir andar pelo campos seguros,     |",
    "|   descobrindo onde estão as bombas e as desarmando quando possível.                                                           |",
    "|                                                                                                                               |",
    "|   >>> Se o jogador tentar abrir caminho em uma bomba letal, automaticamente, ele é reprovado no teste e o jogo termina.       |",                                                                                                        |",
    "|                                                                                                                               |",
    "|   >>> Um cronometro é iniciado junto com o início da partida, se o cronometro zerar, o jogador é reprovado e o jogo termina.  |", 
    "|                                                                                                                               |",
    "|   >>> Para ganhar, o jogador deve desativar todas as Bombas B, revelar todos os campos sem bomba e adicionar um alerta        |",
    "|       em todos os campos com suspeita de serem bombas letais L.                                                               |",                                                                |",
    "|_______________________________________________________________________________________________________________________________|"]).
    

menuRules([
    "Voltar para o menu <<<"  
    ]).

commandsTableRules([
    " f - selecionar        "]).

printRules:-

    textoRules(Texto),
    commandsTableRules(CommandsTable),
    width(Width),
    height(Height),
    emptyPxl(EmptyPxl),
    menuRules(Menu),
    createScreenBuffer(Width, Height, EmptyPxl, InitialBuffer),
    createBufferFromStringMatrix(Texto, TextoBuf),
    createBufferFromStringMatrix(Menu, MenuBuf),
    createBufferFromStringMatrix(CommandsTable, TableBuf),
    
    renderCentralized(TextoBuf,InitialBuffer, -3, 0, Tmp1),
    renderCentralized(MenuBuf, Tmp1,10, 0, Tmp2),
    renderInBuffer(TableBuf, Tmp2, 3, 1, Tmp3),
    
    printScreen(Tmp3).

rules:-

    printRules,
    waitKey(['f'], Key),  
    Key = 'f', 
    mainLoop(1).
