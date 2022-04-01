:- include('Start.pl').

textoInstructions([
    " _______________________________________________________________________________________________________________________________",
    "|                                                                                                                               |",
    "|                                                                                                                               |",
    "|                                        █▀▀ █▀█ █▀▄▀█ █▀█    █ █▀█ █▀▀ █▀█ █▀█                                                 |",                                                           
    "|                                        █   █ █ █ █ █ █ █  ▄ █ █ █ █ ▄ █▀█ █▀▄                                                 |",                                                         
    "|                                        ▀▀▀ ▀▀▀ ▀ ▀ ▀ ▀▀▀  ▀▀▀ ▀▀▀ ▀▀▀ ▀ ▀ ▀ ▀                                                 |",                                                            
    "|                                                                                                                               |",
    "|                                                                                                                               |",
    "|   >>> Bombas desármáveis podem ser identificadas pela letra B, e bombas letais pela letra L.                                  |",
    "|                                                                                                                               |",
    "|   >>> Por padrão, existem 4 bombas desármáveis e 8 bombas letais escondidas pelo campo.                                       |",
    "|                                                                                                                               |",
    "|   >>> O jogador possui três opções de jogada, sendo elas: Abrir caminho, Desativar Bomba e Adicionar alerta.                  |",                                                                                                                          
    "|                                                                                                                               |",
    "|   >>> O numeral 0 indicará que a posição escolhida não possui nenhuma bomba, outros numerais indicam que ao redor deles       |",
    "|       existe uma quantidade de bombas Letais igual ao numeral. Logo, o númeral 1, por exemplo, indica que existe uma bomba    |",
    "|       letal ao seu redor.                                                                                                     |",
    "|                                                                                                                               |",
    "|   >>> Bombas que já foram desativadas podem ser identificadas pela letra D, e Alertas marcados podem ser identificados        |",
    "|       pela letra A. Após um Alerta ser marcado ele não pode ser desmarcado.                                                   |",                                                                                                   
    "|                                                                                                                               |",
    "|   >>> O jogador deve escrever a ação da jogada que quer fazer(Abrir, Desativar, Alerta), e em seguida indicar a posição       |",
    "|       da linha e da coluna, respectivamente.                                                                                  |",  
    "|                                                                                                                               |",
    "|_______________________________________________________________________________________________________________________________|"]).
    

menuInstructions([
    "Começar a partida <<<"  
    ]).

commandsTableInstructions([
    " f - selecionar        "]).

printInstructions:-

    textoInstructions(Texto),
    commandsTableInstructions(CommandsTable),
    width(Width),
    height(Height),
    emptyPxl(EmptyPxl),
    menuInstructions(Menu),
    createScreenBuffer(Width, Height, EmptyPxl, InitialBuffer),
    createBufferFromStringMatrix(Texto, TextoBuf),
    createBufferFromStringMatrix(Menu, MenuBuf),
    createBufferFromStringMatrix(CommandsTable, TableBuf),
    
    renderCentralized(TextoBuf,InitialBuffer, -3, 0, Tmp1),
    renderCentralized(MenuBuf, Tmp1,14, 0, Tmp2),
    renderInBuffer(TableBuf, Tmp2, 3, 1, Tmp3),
    
    printScreen(Tmp3).

instructions:-

    printInstructions,
    waitKey(['f'], Key),  
    Key = 'f', 
    startGame(9,9,8,4),
    mainLoop(1).
