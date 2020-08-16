%option noinput nounput noyywrap
%top {
/*
 * Redistribution and use in source and binary forms, with
 * or without modification, are permitted provided that the
 * following conditions are met:
 *
 * 1. Redistributions of source code must retain this list
 *    of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce this
 *    list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
#include <config.h>
}
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "thecl.h"
    #include "ecsparse.h"
    #include "util.h"
    #include "gool_ins.h"
    void yyerror(parser_state_t*, const char*);
    /* yylloc.first_column is initialized to 1. */
    #define YY_USER_INIT yylloc.first_column = 0
    /* XXX: The builtin YY_INPUT does something with the newlines that doesn't
     * work well with Wine/Windows. */
    /* TODO: Add some \r handling to the line counting stuff. */
    #define YY_INPUT(buf,result,max_size) \
    { \
        int c = getc_unlocked(yyin); \
        if (c == EOF) { \
            result = YY_NULL; \
        } else { \
            if (c == '\n') { \
                yylloc.first_line++; \
                yylloc.last_line = yylloc.first_line; \
                yylloc.first_column = 0; \
            } else { \
                yylloc.first_column++; \
            } \
            yylloc.last_column = yylloc.first_column; \
            buf[0] = c; \
            result = 1; \
        } \
    }
%x COMMENT COMMENT_SINGLE CSTRING STRING ESCAPE_CHAR P_BLOCK
%%
","        return COMMA;
"?"        return QUESTION;
";"        return SEMICOLON;
":"        return COLON;
"{"        return BRACE_OPEN;
"}"        return BRACE_CLOSE;
"["        return SQUARE_OPEN;
"]"        return SQUARE_CLOSE;
"("        return PARENTHESIS_OPEN;
")"        return PARENTHESIS_CLOSE;
"->"       return DEREFERENCE;
"=>"       return LAMBDA;
"sub"      return SUB;
"var"      return VAR;
"default"  { if (g_parser_state->current_sub) { yylval.string = strdup(yytext); return IDENTIFIER; } else return DEFAULT; }
"trans"    { if (g_parser_state->current_sub) { yylval.string = strdup(yytext); return IDENTIFIER; } else return TRANS; }
"once"     { if (g_parser_state->current_sub && g_parser_state->current_sub->is_trans && !g_parser_state->current_sub->has_once) return ONCE; else { yylval.string = strdup(yytext); return IDENTIFIER; } }
"nofirst"  { if (g_parser_state->current_sub && g_parser_state->current_sub->is_trans && !g_parser_state->current_sub->has_nofirst) return NOFIRST; else { yylval.string = strdup(yytext); return IDENTIFIER; } }
"code"     return CODE;
"event"    return EVENT;
"state"    return STATE;
"return"   return RETURN;
"return_sup" { if (g_parser_state->current_sub && g_parser_state->current_sub->is_inline) return RETURN_SUP; else { yylval.string = strdup(yytext); return IDENTIFIER; } };
"expr"     return EXPRESSION;
"array"    return ARRAY;
"inline"   return INLINE;
"mips"     return MIPS;
"__transargs"      return TRANSARGS;
"__mips"           return MOD_MIPS;
"__trans"          return MOD_TRANS;
"__args"   { if (!g_parser_state->current_state && g_parser_state->current_sub && g_parser_state->current_sub->mod_trans) return MOD_ARGS; else { yylval.string = strdup(yytext); return IDENTIFIER; } }

"goto"     return GOTO;
"unless"   return UNLESS;
"if"       return IF;
"else"     return ELSE;
"do"       return DO;
"while"    return WHILE;
"until"    return UNTIL;
"break"    return BREAK;
"continue" return CONTINUE;
"save"     return SAVE;
"@"        return AT;
"="        return ASSIGN;
"+="       return ASSIGNADD;
"-="       return ASSIGNSUB;
"*="       return ASSIGNMUL;
"/="       return ASSIGNDIV;
"%="       return ASSIGNMOD;
"^="       return ASSIGNXOR;
"|="       return ASSIGNBOR;
"&="       return ASSIGNBAND;
"<<="      return ASSIGNLSHIFT;
">>="      return ASSIGNRSHIFT;
"\\="      return ASSIGNTEST;
"+"        return ADD;
"-"        return SUBTRACT;
"*"        return MULTIPLY;
"/"        return DIVIDE;
"%"        return MODULO;
"=="       return EQUAL;
"!="       return INEQUAL;
"<"        return LT;
"<="       return LTEQ;
">"        return GT;
">="       return GTEQ;
"!"        return NOT;
"~"        return B_NOT;
"||"       return OR;
"&&"       return AND;
"^"        return XOR;
"|"        return B_OR;
"&"        return B_AND;
"<<"       return LSHIFT;
">>"       return RSHIFT;
"\\"       return TEST;
"avg"              return AVG;
"abs"              return ABS;
"seek"             return SEEK;
"degseek"          return DEGSEEK;
"degdist"          return DEGDIST;
"time"             return TIME;
"rand"             return RAND;
"randi"            return RANDI;
"loopseek"         return LOOPSEEK;
"getcolor"         return GETCOLOR;
"pad"              return PAD;
"buttonpress"      return BPRESS;
"buttonhold"       return BHOLD;
"buttonbuffer"     return BBUFFER;
"dirpress"         return DPRESS;
"dirhold"          return DHOLD;
"dirbuffer"        return DBUFFER;
"spd"              return SPD;
"sin"              return SIN;
"cos"              return COS;
"fieldval"         return FVAL;
"fieldrow"         return FROW;
"getins"           return MOVC;
"getval"           return GETVAL;
"distance"         return DISTANCE;
"atan"             return ATAN;
"atan2"            return ATAN2;
"atan2_3d"         return ATAN2_3D;
"getfield"         return GETFIELD;
"atan2_obj"        return ATAN2O;
"objectget"        return OBJGET;
"entitygetstate"   return ENTITYSTATEGET;
"gamefunc"         return GAMEFUNC;
"getvalideventobj" return GETVALIDEVENTOBJ;
"pointclip"        return POINTCLIP;
"__unk2"           return UNK2;
"tryload"          return TRYLOAD;
"getanim"          return GETANIM;
"offsetof"         return OFFSETOF;
"ntry4"            return NTRY4;
"ntry5"            return NTRY5;

[0-9]+\.[0-9]+([MmSsVv]|[Hh]z|deg)? {
    char* afternum;
    long double num = strtold(yytext, &afternum);
    if (strstr(afternum, "hz") || strstr(afternum, "Hz"))
        yylval.integer = (int)(num / 11025.0L * 0x400);
    else if (strstr(afternum, "deg"))
        yylval.integer = (int)(num / 180.0L * 2048.0L);
    else if (strchr(afternum, 'M') || strchr(afternum, 'm'))
        yylval.integer = (int)(num * 102400.0L);
    else if (strchr(afternum, 'V') || strchr(afternum, 'v'))
        yylval.integer = (int)(num * 16383.0L);
    else if (strchr(afternum, 's'))
        yylval.integer = (int)(num * (long double)g_rate);
    else if (strchr(afternum, 'S'))
        yylval.integer = (int)(num * 4096.0L);
    else
        yylval.integer = (int)(num * 256.0L);
    return GOOL_INTEGER;
}
[0-9]+([GgMmVvSs]|[Hh]z|deg)? {
    char* afternum;
    yylval.integer = strtoul(yytext, &afternum, 10);
    if (strstr(afternum, "hz") || strstr(afternum, "Hz"))
        yylval.integer = (int)(yylval.integer / 11025.0 * 0x400);
    else if (strstr(afternum, "deg"))
        yylval.integer = (int)(yylval.integer / 180.0 * 0x800);
    else if (strchr(afternum, 'M') || strchr(afternum, 'm'))
        yylval.integer *= 102400;
    else if (strchr(afternum, 'G') || strchr(afternum, 'g'))
        yylval.integer *= 0x100;
    else if (strchr(afternum, 'V') || strchr(afternum, 'v'))
        yylval.integer *= 0x3FFF;
    else if (strchr(afternum, 's'))
        yylval.integer *= g_rate;
    else if (strchr(afternum, 'S'))
        yylval.integer *= 0x1000;
    else
        return INTEGER;
    return GOOL_INTEGER;
}
[GgS]?0[xX][0-9a-fA-F]+ {
    char* txt = yytext;
    if (txt[0] == 'G' || txt[0] == 'g') {
        yylval.integer = 0x100;
        ++txt;
    }
    else if (txt[0] == 'S') {
        yylval.integer = 0x1000;
        ++txt;
    }
    else
        yylval.integer = 1;
    yylval.integer *= strtoul(txt, NULL, 16);
    return INTEGER;
}
0[bB][0-1]+ {
    bool hasSign = yytext[0] == '-' || yytext[0] == '+';
    yylval.integer = strtoul(yytext + (hasSign ? 3 : 2), NULL, 2);
    if (yytext[0] == '-') yylval.integer = -yylval.integer;
    return INTEGER;
}
[Ll]'[0-9a-zA-Z_!]' {
    int id = strchr_o(gool_ename_charmap, yytext[2]);
    if (id == -(int)gool_ename_charmap) {
        char buf[64];
        snprintf(buf, 64, "invalid level id: %c", yytext[2]);
        yyerror(NULL, buf);
        BEGIN(INITIAL);
        yyterminate();
    }
    yylval.integer = id << 8;
    return INTEGER;
}
"false" {
    /* boolean "false" */
    yylval.integer = 0;
    return GOOL_INTEGER;
}
"true" {
    /* boolean "true" */
    yylval.integer = 0x100;
    return GOOL_INTEGER;
}
"null" |
"nil" {
    return NIL;
}
[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.string = strdup(yytext);
    if (global_get(g_parser_state->version, yytext))
        return IDENTIFIER;
    if (macro_get(g_parser_state, yytext))
        return MACRO;
    gooc_array_t* array;
    list_for_each(&g_parser_state->main_ecl->arrays, array) {
        if (!strcmp(array->name, yytext)) {
            free(yylval.string);
            yylval.integer = array->start;
            return ARRAY_NAME;
        }
    }
    return IDENTIFIER;
}
\[[a-zA-Z0-9_!]{5}\] {
    yylval.string = malloc(6);
    strncpy(yylval.string, yytext + 1, 5);
    yylval.string[5] = '\0';
    return ENTRY;
}

\" {
    BEGIN(STRING);
    yylval.string = strdup("");
}
<STRING>[^\"\n\\]+ {
    yylval.string = realloc(yylval.string, strlen(yylval.string) + yyleng + 1);
    strcat(yylval.string, yytext);
}
<STRING>\\ {
    BEGIN(ESCAPE_CHAR);
}
<STRING>\n {
    free(yylval.string);
    yyerror(NULL, "unterminated string");
    BEGIN(INITIAL);
    yyterminate();
}
<STRING>\" {
    BEGIN(INITIAL);
    return TEXT;
}
<ESCAPE_CHAR>[\"\\] {
    yylval.string = realloc(yylval.string, strlen(yylval.string) + yyleng + 1);
    strcat(yylval.string, yytext);
    BEGIN(STRING);
}
<ESCAPE_CHAR>[^\"\\] {
    char buf[256];
    snprintf(buf, 256, "invalid character escape: %s", yytext);
    yyerror(NULL, buf);
    BEGIN(INITIAL);
    yyterminate();
}

"/*"               BEGIN(COMMENT);
<COMMENT>[^*]*     |
<COMMENT>\*[^*/]*  ;
<COMMENT>"*/"      BEGIN(INITIAL);

"//"                  BEGIN(COMMENT_SINGLE);
<COMMENT_SINGLE>.*    ;
<COMMENT_SINGLE>"\n"  BEGIN(INITIAL);

#ifn?reg[\t\n\r ]+\"[a-zA-Z\-]+\"[\t\n\r ]+ |
<P_BLOCK>#ifn?reg[\t\n\r ]+\"[a-zA-Z\-]+\"[\t\n\r ]+ {
    char* o_reg_text = strchr(yytext, '"') + 1;
    int len = strchr_o(o_reg_text, '"');
    char* reg_text = malloc(len + 1);
    strncpy(reg_text, o_reg_text, len);
    reg_text[len] = '\0';

    int cmp = (yytext[3] == 'n') == strcmp(reg_text, g_region);
    free(reg_text);
    g_reg_blocks = realloc(g_reg_blocks, sizeof(int) * ++g_reg_block_depth);
    g_reg_blocks[g_reg_block_depth - 1] = !cmp;
    if (!cmp) {
        BEGIN(P_BLOCK);
    }
}
#elifn?reg[\t\n\r ]+\"[a-zA-Z\-]+\"[\t\n\r ]+ |
<P_BLOCK>#elifn?reg[\t\n\r ]+\"[a-zA-Z\-]+\"[\t\n\r ]+ {
    if (g_reg_block_depth == 0 || g_reg_blocks[g_reg_block_depth - 1] & 2) {
        yyerror(NULL, "invalid #elif");
        yyterminate();
    }
    if (g_reg_blocks[g_reg_block_depth - 1] & 1) {
        char* o_reg_text = strchr(yytext, '"') + 1;
        int len = strchr_o(o_reg_text, '"');
        char* reg_text = malloc(len + 1);
        strncpy(reg_text, o_reg_text, len);
        reg_text[len] = '\0';

        int cmp = (yytext[5] == 'n') == strcmp(reg_text, g_region);
        free(reg_text);
        g_reg_blocks[g_reg_block_depth - 1] = !cmp;
        if (!cmp) {
            BEGIN(P_BLOCK);
        }
        else {
            BEGIN(INITIAL);
        }
    }
    else {
        BEGIN(P_BLOCK);
    }
}
#ifn?ver[\t\n\r ]+[0-9]+[\t\n\r ]+ |
<P_BLOCK>#ifn?ver[\t\n\r ]+[0-9]+[\t\n\r ]+ {
    int cmp = (atoi(yytext + 7) == g_parser_state->version) ^ (yytext[3] == 'n');
    g_reg_blocks = realloc(g_reg_blocks, sizeof(int) * ++g_reg_block_depth);
    g_reg_blocks[g_reg_block_depth - 1] = !cmp;
    if (!cmp) {
        BEGIN(P_BLOCK);
    }
}
#elifn?ver[\t\n\r ]+[0-9]+[\t\n\r ]+ |
<P_BLOCK>#elifn?ver[\t\n\r ]+[0-9]+[\t\n\r ]+ {
    if (g_reg_block_depth == 0 || g_reg_blocks[g_reg_block_depth - 1] & 2) {
        yyerror(NULL, "invalid #elif");
        yyterminate();
    }
    if (g_reg_blocks[g_reg_block_depth - 1] & 1) {
        int cmp = (atoi(yytext + 9) == g_parser_state->version) ^ (yytext[5] == 'n');
        g_reg_blocks[g_reg_block_depth - 1] = !cmp;
        if (!cmp) {
            BEGIN(P_BLOCK);
        }
        else {
            BEGIN(INITIAL);
        }
    }
    else {
        BEGIN(P_BLOCK);
    }
}
#ifn?lev[\t\n\r ]+[0-9a-zA-Z\_\!]+[\t\n\r ]+ |
<P_BLOCK>#ifn?lev[\t\n\r ]+[0-9a-zA-Z\_\!]+[\t\n\r ]+ {
    char* lev = strpbrk(yytext + 7, gool_ename_charmap);
    int cmp = false;
    if (lev != NULL) {
        char l = lev++[0];
        while (((l >= '0' && l <= '9') || (l >= 'a' && l <= 'z') || (l >= 'A' && l <= 'Z') || l == '_' || l == '!') && !cmp) {
            cmp |= l == g_lev;
            l = lev++[0];
        }
    }
    cmp ^= yytext[3] == 'n';
    g_reg_blocks = realloc(g_reg_blocks, sizeof(int) * ++g_reg_block_depth);
    g_reg_blocks[g_reg_block_depth - 1] = !cmp;
    if (!cmp) {
        BEGIN(P_BLOCK);
    }
}
#elifn?lev[\t\n\r ]+[0-9a-zA-Z\_\!]+[\t\n\r ]+ |
<P_BLOCK>#elifn?lev[\t\n\r ]+[0-9a-zA-Z\_\!]+[\t\n\r ]+ {
    if (g_reg_block_depth == 0 || g_reg_blocks[g_reg_block_depth - 1] & 2) {
        yyerror(NULL, "invalid #elif");
        yyterminate();
    }
    if (g_reg_blocks[g_reg_block_depth - 1] & 1) {
        char* lev = strpbrk(yytext + 9, gool_ename_charmap);
        int cmp = false;
        if (lev != NULL) {
            char l = lev++[0];
            while (((l >= '0' && l <= '9') || (l >= 'a' && l <= 'z') || (l >= 'A' && l <= 'Z') || l == '_' || l == '!') && !cmp) {
                cmp |= l == g_lev;
                l = lev++[0];
            }
        }
        cmp ^= yytext[5] == 'n';
        g_reg_blocks[g_reg_block_depth - 1] = !cmp;
        if (!cmp) {
            BEGIN(P_BLOCK);
        }
        else {
            BEGIN(INITIAL);
        }
    }
    else {
        BEGIN(P_BLOCK);
    }
}
#else |
<P_BLOCK>#else {
    if (g_reg_block_depth == 0 || g_reg_blocks[g_reg_block_depth - 1] & 2) {
        yyerror(NULL, "invalid #else");
        yyterminate();
    }
    if (g_reg_blocks[g_reg_block_depth - 1] & 1) {
        BEGIN(INITIAL);
    }
    else {
        BEGIN(P_BLOCK);
    }
    g_reg_blocks[g_reg_block_depth - 1] = !g_reg_blocks[g_reg_block_depth - 1] | 2;
}
#endif | {
<P_BLOCK>#endif {
    if (g_reg_block_depth == 0) {
        yyerror(NULL, "invalid #endif");
        yyterminate();
    }
    g_reg_blocks = realloc(g_reg_blocks, sizeof(int) * --g_reg_block_depth);
    if (g_reg_block_depth == 0) {
        BEGIN(INITIAL);
    }
    else if (g_reg_blocks[g_reg_block_depth - 1] & 1) {
        BEGIN(P_BLOCK);
    }
    else {
        BEGIN(INITIAL);
    }
}
<P_BLOCK>([\n\r]|.) ;
#purge[\t\n\r ]+[0-9a-zA-Z\_\!]+ {
    if (g_parser_state->ecl != g_parser_state->main_ecl) {
        if (g_parser_state->ecl->purge_tested) {
            char buf[6];
            yyerror(NULL, "#purge already used in module '%s'", gool_to_ename(buf, g_parser_state->ecl->eid));
        }
        else {
            g_parser_state->ecl->purge_data = !strchr(yytext+6, g_lev);
            g_parser_state->ecl->purge_tested = true;
        }
    }
    else {
        yyerror(NULL, "#purge cannot be used for main module");
    }
    yylval.string = strdup(yytext + 1);
    return DIRECTIVE;
}

#([a-zA-Z]+) {
    char* txt = yytext + 1;
    if (!strcmp(txt, "font"))
        return DIRECTIVE_FONT;
    else if (!strcmp(txt, "char"))
        return DIRECTIVE_CHAR;
    else if (!strcmp(txt, "text"))
        return DIRECTIVE_TEXT;
    else if (!strcmp(txt, "tex"))
        return DIRECTIVE_TEXTURE;
    else if (!strcmp(txt, "sprite"))
        return DIRECTIVE_SPRITE;
    else if (!strcmp(txt, "fraganim"))
        return DIRECTIVE_FANIM;
    else if (!strcmp(txt, "frag"))
        return DIRECTIVE_FRAG;
    yylval.string = strdup(yytext + 1);
    return DIRECTIVE;
}

[\t\n\r ]+ ;
. {
    char buf[256];
    snprintf(buf, 256, "illegal token: %s", yytext);
    yyerror(NULL, buf);
    return ILLEGAL_TOKEN;
}
