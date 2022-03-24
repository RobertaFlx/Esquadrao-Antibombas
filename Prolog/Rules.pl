textoRules([
    " _______________________________________________________________________________________________________________________________",
    "|                                                                                                                               |",
    "|   Você está sendo treinado para ser um especialista em desarmamento de bombas e foi chamado para um teste prático em campo.   |",
    "|                                                                                                                               |",
    "|   Ao chegar no lugar indicado, é informado que foram implantadas bombas comuns, que podem ser desarmadas, e bombas letais,    |",
    "|   que você ainda não tem treinamento necessário para desativar. O objetivo do jogo é conseguir andar pelo campos seguros,     |",
    "|   descobrindo onde estão as bombas e as desarmando quando possível.                                                           |",
    "|                                                                                                                               |",
    "|   >>> O jogador inicia o teste com nota máxima, conforme o passar do jogo, sua nota poderá diminuir de acordo com os erros    |",
    "|       cometidos no teste.                                                                                                     |",
    "|                                                                                                                               |",
    "|   >>> Se o jogador errar mais de três vezes, ficará abaixo da média e reprovará no teste, desta forma, o jogo termina.        |",
    "|                                                                                                                               |",
    "|   >>> Erros são contados apenas quando o jogador tentar desativar uma bomba em um campo que não possui bomba ou abrir um      |",
    "|       caminho já revelado.                                                                                                    |",
    "|                                                                                                                               |",
    "|   >>> Se o jogador tentar desarmar ou abrir caminho em uma bomba letal, automaticamente, ele é reprovado no teste e o         |",
    "|       jogo termina.                                                                                                           |",
    "|                                                                                                                               |",
    "|   >>> Um cronometro é iniciado junto com o início da partida, se o cronometro zerar, o jogador é reprovado e o jogo termina.  |",                                             
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
