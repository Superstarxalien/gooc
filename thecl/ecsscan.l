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
%x COMMENT COMMENT_SINGLE CSTRING STRING ESCAPE_CHAR
%%
","        return COMMA;
";"        return SEMICOLON;
"{"        return BRACE_OPEN;
"}"        return BRACE_CLOSE;
"("        return PARENTHESIS_OPEN;
")"        return PARENTHESIS_CLOSE;
"->"       return DEREFERENCE;
"sub"      return SUB;
"var"      return VAR;
"trans"    return TRANS;
"code"     return CODE;
"handles"  return HANDLES;
"event"    return EVENT;
"state"    return STATE;
"return"   return RETURN;
"statusc"  return STATUSC;
"expr"     return EXPRESSION;

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
"<<="      return ASSIGNSHIFT;
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
"||"       return OR;
"&&"       return AND;
"^"        return XOR;
"|"        return B_OR;
"&"        return B_AND;
"<<"       return SHIFT;
"\\"       return TEST;
"seek"     return SEEK;
"degseek"  return DEGSEEK;

(-|\+)?[0-9]+(\.[0-9]+)?\s*deg {
    yylval.integer = (int)(strtod(yytext, NULL) / 180.0 * 0x800);
    return GOOL_INTEGER;
}
(-|\+)?[0-9]+\.[0-9]+[Mm]? {
    char* afternum;
    double num = strtod(yytext, &afternum);
    if (strchr(afternum, 'M') || strchr(afternum, 'm'))
        yylval.integer = (int)(num * 102400);
    else
        yylval.integer = (int)(num * 0x100);
    return GOOL_INTEGER;
}
(-|\+)?[0-9]+[GgMm]? {
    char* afternum;
    yylval.integer = strtol(yytext, &afternum, 10);
    if (strchr(afternum, 'M') || strchr(afternum, 'm'))
        yylval.integer *= 102400;
    else if (strchr(afternum, 'G') || strchr(afternum, 'g'))
        yylval.integer *= 0x100;
    else
        return INTEGER;
    return GOOL_INTEGER;
}
(-|\+)?0x[0-9a-fA-F]+ {
    yylval.integer = strtol(yytext, NULL, 16);
    return INTEGER;
}
(-|\+)?0b[0-1]+ {
    bool hasSign = yytext[0] == '-' || yytext[0] == '+';
    yylval.integer = strtol(yytext + (hasSign ? 3 : 2), NULL, 2);
    if (yytext[0] == '-') yylval.integer = -yylval.integer;
    return INTEGER;
}
"false" {
    /* boolean "false" */
    yylval.integer = 0;
    return INTEGER;
}
"true" {
    /* boolean "true" */
    yylval.integer = 0x100;
    return INTEGER;
}

[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.string = strdup(yytext);
    if (gvar_get(g_parser_state->version, yytext))
        return IDENTIFIER;
    if (macro_get(g_parser_state, yytext))
        return MACRO;
    return IDENTIFIER;
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
    yylval.string = strdup(yytext + 1);
    if (!strcmp(yylval.string, "font"))
        return DIRECTIVE_FONT;
    else if (!strcmp(yylval.string, "char"))
        return DIRECTIVE_CHAR;
    else if (!strcmp(yylval.string, "text"))
        return DIRECTIVE_TEXT;
    return DIRECTIVE;
}

[\t\n\r ]+ ;
. {
    char buf[256];
    snprintf(buf, 256, "illegal token: %s", yytext);
    yyerror(NULL, buf);
    return ILLEGAL_TOKEN;
}
