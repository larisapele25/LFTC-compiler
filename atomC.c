#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include<stdlib.h>

#include "lexer.h"
#include "utils.h"
#include "parser.h"
#include "ad.h"
#include "at.h"
#include "vm.h"
#include "gc.h"

int main()
{
    char *inbuf=loadFile("tests/testgc.c");
    puts(inbuf);
    Token *tokens=tokenize(inbuf);
    
    showTokens(tokens);
    pushDomain();
    vmInit();
    parse(tokens);
    showDomain(symTable,"global");
    Instr *test =genTestProgramDouble();
    run(test);

    Symbol *symMain=findSymbolInDomain(symTable,"main");

    if(!symMain)err("missing main function");

    Instr *entryCode=NULL;
    addInstr(&entryCode,OP_CALL)->arg.instr=symMain->fn.instr;
    addInstr(&entryCode,OP_HALT);
    run(entryCode);
    dropDomain();
    free(inbuf);

    return 0;
}