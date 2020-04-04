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
"code"     return CODE;
"event"    return EVENT;
"state"    return STATE;
"return"   return RETURN;
"expr"     return EXPRESSION;
"inline"   return INLINE;

"goto"     return GOTO;
"unless"   return UNLESS;
"if"       return IF;
"else"     return ELSE;
"do"       return DO;
"while"    return WHILE;
"until"    return UNTIL;
"break"    return BREAK;
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
"abs"              return ABS;
"seek"             return SEEK;
"degseek"          return DEGSEEK;
"degdist"          return DEGDIST;
"time"             return TIME;
"rand"             return RAND;
"nearseek"         return NEARSEEK;
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
"getval"                return GETVAL;
"distance"              return DISTANCE;
"atan2"                 return ATAN2;
"getfield"              return GETFIELD;
"atan2_mirrored"        return ATAN2M;
"objectget"             return OBJGET;
"entitygetstate"        return ENTITYSTATEGET;
"gamefunc"              return GAMEFUNC;
"__unk1"                return UNK1;
"iscolliding"           return ISCOLLIDING;
"__unk2"                return UNK2;

vol\([0-9]+\.[0-9]+\) {
    yylval.integer = (int)(strtod(yytext+4, NULL) * 0x3FFF);
    return INTEGER;
}
[0-9]+(\.[0-9]+)?\s*deg {
    yylval.integer = (int)(strtod(yytext, NULL) / 180.0 * 0x800);
    return GOOL_INTEGER;
}
[0-9]+\.[0-9]+([Mms]|[Hh]z)? {
    char* afternum;
    double num = strtod(yytext, &afternum);
    if (strstr(afternum, "hz") || strstr(afternum, "Hz"))
        yylval.integer = (int)(num / 11025.0 * 0x400);
    else if (strchr(afternum, 'M') || strchr(afternum, 'm'))
        yylval.integer = (int)(num * 102400);
    else if (strchr(afternum, 's'))
        yylval.integer = (int)(num * g_rate);
    else
        yylval.integer = (int)(num * 0x100);
    return GOOL_INTEGER;
}
[0-9]+([GgMms]|[Hh]z)? {
    char* afternum;
    yylval.integer = strtoul(yytext, &afternum, 10);
    if (strstr(afternum, "hz") || strstr(afternum, "Hz"))
        yylval.integer = (int)(yylval.integer / 11025.0 * 0x400);
    else if (strchr(afternum, 'M') || strchr(afternum, 'm'))
        yylval.integer *= 102400;
    else if (strchr(afternum, 'G') || strchr(afternum, 'g'))
        yylval.integer *= 0x100;
    else if (strchr(afternum, 's'))
        yylval.integer *= g_rate;
    else
        return INTEGER;
    return GOOL_INTEGER;
}
[Gg]?0x[0-9a-fA-F]+ {
    char* txt = yytext;
    if (txt[0] == 'G' || txt[0] == 'g') {
        yylval.integer = 0x100;
        ++txt;
    }
    else
        yylval.integer = 1;
    yylval.integer *= strtoul(txt, NULL, 16);
    return INTEGER;
}
0b[0-1]+ {
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
    return INTEGER;
}
"true" {
    /* boolean "true" */
    yylval.integer = 1;
    return INTEGER;
}
[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.string = strdup(yytext);
    if (global_get(g_parser_state->version, yytext))
        return IDENTIFIER;
    if (macro_get(g_parser_state, yytext))
        return MACRO;
    return IDENTIFIER;
}
\[[a-zA-Z0-9_!]{5}\] {
    yylval.string = malloc(6);
    strncpy(yylval.string, yytext + 1, 5);
    yylval.string[5] = '\0';
    return ENTRY;
}
"null" |
"nil" {
    return NIL;
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
    else if (!strcmp(txt, "endif"))
        goto CHECK_ENDIF;
    else if (!strcmp(txt, "fraganim"))
        return DIRECTIVE_FANIM;
    else if (!strcmp(txt, "frag"))
        return DIRECTIVE_FRAG;
    yylval.string = strdup(yytext + 1);
    return DIRECTIVE;
}

#ifn?reg[\t\n\r ]+\"[a-zA-Z\-]+\"[\t\n\r ]+ {
    char* o_reg_text = strchr(yytext, '"') + 1;
    int len = strchr_o(o_reg_text, '"');
    char* reg_text = malloc(len + 1);
    strncpy(reg_text, o_reg_text, len);
    reg_text[len] = '\0';

    int cmp = (yytext[3] == 'n') == strcmp(reg_text, g_region);
    g_reg_blocks = realloc(g_reg_blocks, sizeof(int) * ++g_reg_block_depth);
    if (!cmp) {
        BEGIN(P_BLOCK);
    }
    g_reg_blocks[g_reg_block_depth - 1] = !cmp;
    free(reg_text);
}
<P_BLOCK>#endif {
CHECK_ENDIF:
    if (g_reg_block_depth == 0) {
        yyerror(NULL, "invalid #endif");
        yyterminate();
    }
    g_reg_blocks = realloc(g_reg_blocks, sizeof(int) * --g_reg_block_depth);
    if (g_reg_block_depth == 0) {
        BEGIN(INITIAL);
    }
    else if (g_reg_blocks[g_reg_block_depth - 1]) {
        BEGIN(P_BLOCK);
    }
    else {
        BEGIN(INITIAL);
    }
}
<P_BLOCK>([\n\r]|.) ;

[\t\n\r ]+ ;
. {
    char buf[256];
    snprintf(buf, 256, "illegal token: %s", yytext);
    yyerror(NULL, buf);
    return ILLEGAL_TOKEN;
}
