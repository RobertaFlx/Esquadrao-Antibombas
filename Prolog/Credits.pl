texto([
    " ________________________________________________________________________________",
    "|                                                                                |",
    "|   Projeto em Prolog para disciplina Paradigmas de Linguagens de Programação    |", 
    "|                                                                                |", 
    "|   Integrantes do Grupo:                                                        |", 
    "|                                                                                |",
    "|   >>> Adísio Pereira Fialho Júnior                                             |",
    "|   >>> Sonaly Katly Garcia Nunes                                                |",
    "|   >>> Roberta Felix da Silva                                                   |",
    "|________________________________________________________________________________|"]).


menuCredit([
    "Voltar para o menu <<<"]).

commandsTableCredit([ 
    " f - selecionar        "]).

printCredits:-

    width(Width),
    height(Height),
    menuCredit(MenuCredit),
    texto(Texto),
    emptyPxl(EmptyPxl),
    commandsTableCredit(CommandsTableCredit),

    createScreenBuffer(Width, Height, EmptyPxl, InitialBuffer),
    createBufferFromStringMatrix(Texto, TextoBuf),
    createBufferFromStringMatrix(MenuCredit, MenuBuf),
    createBufferFromStringMatrix(CommandsTableCredit, TableBuf),

    renderCentralized(TextoBuf,InitialBuffer, -1, 0, Tmp1),
    renderCentralized(MenuBuf, Tmp1, 9, 0, Tmp2),
    renderInBuffer(TableBuf, Tmp2, 3, 1, Tmp3),
  
    printScreen(Tmp3).
    
credits:- 

    printCredits,
    waitKey(['f'], Key),  
    Key = 'f',
    mainLoop(2).
