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
%{
#include <config.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "expr.h"
#include "field.h"
#include "path.h"
#include "file.h"
#include "list.h"
#include "program.h"
#include "thecl.h"
#include "value.h"
#include "gool_ins.h"
#include "c1_gool.h"
#include "c2_gool.h"

typedef struct {
    char* text;
} string_t;

static list_t* string_list_add(list_t* list, char* text);
static void string_list_free(list_t* list);

static thecl_instr_t* instr_new(parser_state_t* state, uint8_t id, const char* format, ...);
static thecl_instr_t* instr_new_list(parser_state_t* state, uint8_t id, list_t* list);
static void instr_add(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_del(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_prepend(thecl_sub_t* sub, thecl_instr_t* instr);
/* Returns true if the created call was inline. */
static bool instr_create_call(parser_state_t *state, uint8_t type, char *name, list_t *params);

static expression_t* expression_load_new(const parser_state_t* state, thecl_param_t* value);
static expression_t* expression_val_new(const parser_state_t* state, int value);
static expression_t* expression_pointer_new(const parser_state_t* state, thecl_param_t* value);
static expression_t* expression_operation_new(const parser_state_t* state, const int symbol, expression_t** operands);
static expression_t* expression_ternary_new(const parser_state_t* state, expression_t* cond, expression_t* val1, expression_t* val2);

static void expression_output(parser_state_t* state, expression_t* expr);
static void expression_optimize(parser_state_t* state, expression_t* expr);
#define EXPR_5(a, A, B, C, D, E) \
    expression_operation_new(state, a, (expression_t*[]){ A, B, C, D, E, NULL })
#define EXPR_4(a, A, B, C, D) \
    expression_operation_new(state, a, (expression_t*[]){ A, B, C, D, NULL })
#define EXPR_3(a, A, B, C) \
    expression_operation_new(state, a, (expression_t*[]){ A, B, C, NULL })
#define EXPR_2(a, A, B) \
    expression_operation_new(state, a, (expression_t*[]){ A, B, NULL })
#define EXPR_1(a, A) \
    expression_operation_new(state, a, (expression_t*[]){ A, NULL })

static expression_t *expression_copy(expression_t *expr);
static void expression_create_goto(parser_state_t *state, int type, char *labelstr, expression_t* cond);
static void expression_create_goto_pop(parser_state_t *state, int type, char *labelstr, expression_t* cond, int pop);

/* Bison things. */
void yyerror(const parser_state_t*, const char*, ...);
int yylex(void);
extern FILE* yyin;

/* Parser APIs. */

/* Starts a new subroutine. */
static void sub_begin(parser_state_t* state, char* name);
/* Closes the current subroutine. */
static void sub_finish(parser_state_t* state);

/* Starts a new GOOL state. */
static void state_begin(parser_state_t* state, char* name);
/* Closes the current GOOL state. */
static void state_finish(parser_state_t* state);

/* Begins a new scope. */
static void scope_begin(parser_state_t* state);
/* Ends the most recently started scope. */
static void scope_finish(parser_state_t* state, bool pop_vars);

static expr_macro_t* macro_create(parser_state_t* state, const char* name, expression_t* expr);
/* Creates a new argument in the specified subroutine. */
static thecl_variable_t* arg_create(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Creates a new object field. */
static field_t* objfield_create(parser_state_t* state, const char* name);
/* Deletes an object field. */
static void objfield_delete(parser_state_t* state, const char* name);
/* Creates a new variable in the specified subroutine. */
static thecl_variable_t* var_create(parser_state_t* state, thecl_sub_t* sub, const char* name, bool push);
/* Creates a new variable in the specified subroutine, and assigns a value to it. */
static thecl_variable_t* var_create_assign(parser_state_t* state, thecl_sub_t* sub, const char* name, expression_t* expr);
/* Returns true if the given variable is accessible in the current scope.. */
static bool var_accessible(parser_state_t* state, thecl_variable_t* var);
/* Returns argument of the given name in the specified sub, or NULL if the argument doesn't exist */
static thecl_variable_t* arg_get(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Returns object field of the given name, or NULL if the field doesn't exist */
static field_t* objfield_get(parser_state_t* state, const char* name);
/* Returns variable of the given name in the specified sub, or NULL if the variable doesn't exist/is out of scope */
static thecl_variable_t* var_get(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Returns the stack offset of a specified variable in the specified sub. */
static int var_stack(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Returns 1 if an argument of a given name exists, and 0 if it doesn't. */
static int arg_exists(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Returns 1 if a variable of a given name exists, and 0 if it doesn't. */
static int var_exists(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Compiles a shorthand assignment operation on a given variable */
static void var_shorthand_assign(parser_state_t* state, thecl_param_t* param, expression_t* expr, int EXPR);
/* Stores a new label in the current subroutine pointing to the current offset. */
static void label_create(parser_state_t* state, char* label);
/* Returns the spawn of the given name, or NULL if it doesn't exist */
static thecl_spawn_t* spawn_get(parser_state_t* state, const char* name);

/* Creates a new param equivalent to a GOOL stack push/pop operand */
static thecl_param_t* param_sp_new(void);
/* Creates a new param equivalent to a GOOL null operand */
static thecl_param_t* param_null_new(void);
/* Creates a new param equivalent to a GOOL double stack pop operand */
static thecl_param_t* param_sp2_new(void);
/* Creates a new self->variable param with the specified value */
static thecl_param_t* param_var_new(int val);

/* Returns the result of a math operation */
static int math_preprocess(parser_state_t* state, int symbol, int val1, int val2);

/* Update the current time label. */
void set_time(parser_state_t* state, int new_time);

/* Opens and parses the file of a given name. Returns a non-zero value on error. */
static int directive_include(parser_state_t* state, char* include_path);

/* Return animation of the given name */
static gool_anim_t* anim_get(parser_state_t* state, char* name);
/* Return the offset of an animation of the given name */
static size_t anim_get_offset(parser_state_t* state, char* name);
/* Appends a new Crash 1 vertex animation to the animations list. */
static void anim_create_anim_c1(parser_state_t* state, char* name, int frames, int eid);
/* Appends a new Crash 2 vertex animation to the animations list. */
static void anim_create_anim_c2(parser_state_t* state, char* name, int frames, int eid, int interp);

int yydebug = 0;
%}

%define parse.error verbose
%locations
%parse-param {parser_state_t* state}
%debug

%union {
    /* Values from Flex: */
    int integer;
    char* string;

    /* Internal types: */
    struct thecl_param_t* param;
    struct expression_t* expression;
    struct list_t* list;
}

%token <string> IDENTIFIER "identifier"
%token <string> MACRO "macro"
%token <string> TEXT "text"
%token <integer> INTEGER "integer"
%token <integer> GOOL_INTEGER "gool integer"
%token <string> DIRECTIVE "directive"
%token <string> ENTRY "entry"
%token NIL "nil"
%token DIRECTIVE_FONT "#font"
%token DIRECTIVE_CHAR "#char"
%token DIRECTIVE_TEXT "#text"
%token DIRECTIVE_TEXTURE "#tex"
%token DIRECTIVE_SPRITE "#sprite"
%token DIRECTIVE_FANIM "#fraganim"
%token DIRECTIVE_FRAG "#frag"
%token COMMA ","
%token QUESTION "?"
%token SEMICOLON ";"
%token COLON ":"
%token SUB "sub"
%token VAR "var"
%token RETURN "return"
%token STATE "state"
%token CODE "code"
%token TRANS "trans"
%token TRANSARGS "__transargs"
%token ONCE "once"
%token NOFIRST "nofirst"
%token EVENT "event"
%token INLINE "inline"
%token DEFAULT "default"
%token BRACE_OPEN "{"
%token BRACE_CLOSE "}"
%token SQUARE_OPEN "["
%token SQUARE_CLOSE "]"
%token PARENTHESIS_OPEN "("
%token PARENTHESIS_CLOSE ")"
%token DEREFERENCE "->"
%token EXPRESSION "expr"
%token LAMBDA "=>"
%token ILLEGAL_TOKEN "illegal token"
%token END_OF_FILE 0 "end of file"

%token GOTO "goto"
%token UNLESS "unless"
%token IF "if"
%token ELSE "else"
%token DO "do"
%token WHILE "while"
%token UNTIL "until"
%token BREAK "break"
%token CONTINUE "continue"
%token CALL
%token LOAD
%token GLOAD
%token PLOAD
%token ASSIGN "="
%token GASSIGN
%token ASSIGNADD "+="
%token ASSIGNSUB "-="
%token ASSIGNMUL "*="
%token ASSIGNDIV "/="
%token ASSIGNMOD "%="
%token ASSIGNXOR "^="
%token ASSIGNBOR "|="
%token ASSIGNBAND "&="
%token ASSIGNLSHIFT "<<="
%token ASSIGNRSHIFT ">>="
%token ASSIGNTEST "\\="
%token ADD "+"
%token SUBTRACT "-"
%token MULTIPLY "*"
%token DIVIDE "/"
%token MODULO "%"
%token EQUAL "=="
%token INEQUAL "!="
%token LT "<"
%token LTEQ "<="
%token GT ">"
%token GTEQ ">="
%token NOT "!"
%token B_NOT "~"
%token AND "&&"
%token OR "||"
%token XOR "^"
%token B_OR "|"
%token B_AND "&"
%token LSHIFT "<<"
%token RSHIFT ">>"
%token TEST "\\"
%token ADDRESSOF
%token ARRL "array load"
%token ABS "abs"
%token SEEK "seek"
%token DEGSEEK "degseek"
%token DEGDIST "degdist"
%token RAND "rand"
%token NEARSEEK "nearseek"
%token TIME "time"
%token GETCOLOR "getcolor"
%token PAD "pad"
%token BPRESS "buttonpress"
%token BHOLD "buttonhold"
%token BBUFFER "buttonbuffer"
%token DPRESS "dirpress"
%token DHOLD "dirhold"
%token DBUFFER "dirbuffer"
%token SPD "spd"
%token PSIN
%token SIN "sin"
%token COS "cos"
%token MISC "misc"
%token NTRY "entry operation"
%token FVAL "fieldval"
%token FROW "fieldrow"
%token MOVC "getins"
%token GETVAL "getval"
%token DISTANCE "distance"
%token ATAN2 "atan2"
%token GETFIELD "getfield"
%token ATAN2M "atan2_mirrored"
%token OBJGET "objectget"
%token ENTITYSTATEGET "entitygetstate"
%token GAMEFUNC "gamefunc"
%token GETVALIDEVENTOBJ "getvalideventobj"
%token ISCOLLIDING "iscolliding"
%token UNK2 "__unk2"
%token TRYLOAD "tryload"
%token GETANIM "getanim"
%token OFFSETOF "offsetof"

%type <list> Instruction_Parameters_List
%type <list> Instruction_Parameters

%type <expression> Expression
%type <expression> ExpressionSubsetInstParam
%type <expression> ExpressionLoadType
%type <expression> ExpressionSubset
%type <expression> ParenExpression
%type <expression> ParenExpression2

%type <param> Instruction_Parameter
%type <param> Address
%type <param> Integer
%type <param> Entry
//%type <param> Text
%type <param> Load_Type

%type <integer> Literal_Int

%left QUESTION
%right OR
%right AND
%left B_OR
%left XOR
%left TEST
%left B_AND
%left EQUAL INEQUAL
%left LT LTEQ GT GTEQ
%left LSHIFT RSHIFT
%left ADD SUBTRACT
%left MULTIPLY DIVIDE MODULO
%precedence NOT B_NOT ADDRESSOF
%precedence ABS

%expect 0
%%

Statements:
    %empty
    | Statements Statement
    ;

Statement:
      "sub" IDENTIFIER {
        sub_begin(state, $2);
        state->current_sub->is_inline = false;
        free($2);
      }
      "(" ArgumentDeclaration ")" Subroutine_Body {
        sub_finish(state);
      }
    | "inline" "sub" IDENTIFIER {
        sub_begin(state, $3);
        state->current_sub->is_inline = true;
        if (state->ecl != state->main_ecl) {
            list_del(&state->ecl->subs, state->ecl->subs.tail);
            list_append_new(&state->main_ecl->subs, state->current_sub);
        }
        free($3);
      }
      "(" ArgumentDeclaration ")" Subroutine_Body {
        sub_finish(state);
      }
    | "state" IDENTIFIER {
        state_begin(state, $2);
        free($2);
      }
      State_Body {
        state_finish(state);
      }
    | "event" IDENTIFIER "=>" {
        const field_t* event = event_get(state->version, $2);
        if (!event) {
            yyerror(state, "unknown event: %s", $2);
            free($2);
            break;
        }

        state->current_interrupt = malloc(sizeof(thecl_interrupt_t));
        state->current_interrupt->event = malloc(sizeof(field_t));
        memcpy(state->current_interrupt->event, event, sizeof(field_t));

        free($2);
      }
      Interrupt_Body {
        list_append_new(&state->main_ecl->interrupts, state->current_interrupt);

        state->current_interrupt = NULL;
      }
    | DIRECTIVE IDENTIFIER INTEGER INTEGER {
        if (!strcmp($1, "gool")){
            state->main_ecl->eid = gool_to_eid($2);
            state->main_ecl->id = $3;
            state->main_ecl->type = $4;

            state->main_ecl->is_defined = 1;

            gool_pool_force_get_index(state->main_ecl, state->main_ecl->eid);

            /* automatically create an expression macro that translates the ename to the GOOL ID */
            macro_create(state, $2, expression_load_new(state, param_val_new($3)));
        }
        free($1);
        free($2);
      }
    | DIRECTIVE IDENTIFIER IDENTIFIER {
        if (!strcmp($1, "spawn")){
            thecl_spawn_t* spawn = malloc(sizeof(thecl_spawn_t));
            spawn->name = strdup($2);
            spawn->state_name = strdup($3);
            spawn->offset = state->spawn_count;
            list_append_new(&state->main_ecl->spawns, spawn);
            ++state->spawn_count;
        }
        free($1);
        free($2);
        free($3);
      }
    | DIRECTIVE IDENTIFIER ENTRY INTEGER {
        if (!strcmp($1, "anim")) {
            if (state->version == 1) {
                anim_create_anim_c1(state, $2, $4, gool_to_eid($3));
            }
        }
        free($1);
        free($2);
        free($3);
      }
    | DIRECTIVE IDENTIFIER ENTRY INTEGER INTEGER {
        if (!strcmp($1, "anim")) {
            if (state->version == 2) {
                anim_create_anim_c2(state, $2, $4, gool_to_eid($3), $5);
            }
        }
        free($1);
        free($2);
        free($3);
      }
    | EXPRESSION IDENTIFIER "=" Expression { /* expression macro */
        macro_create(state, $2, $4);
        free($2);
      }
    | DIRECTIVE TEXT {
        if (strcmp($1, "include") == 0) {
            if (directive_include(state, $2) != 0) {
                /* For proper syntax error displaying, this needs to return. */
                free($1);
                free($2);
                return 1;
            }
        } else {
            yyerror(state, "unknown directive: %s", $1);
        }
        free($1);
        free($2);
      }
    | DIRECTIVE ENTRY {
        if (!strcmp($1, "module")) {
            int eid = gool_to_eid($2);
            for (int i = 0; i < state->ecl_cnt; ++i) {
                if (state->ecl_stack[i]->eid == eid) {
                    state->ecl = state->ecl_stack[i];
                    goto outer_break_447;
                }
            }
            thecl_t* ecl = thecl_new();
            ecl->eid = eid;
            state->ecl = ecl;
            state->ecl_stack = realloc(state->ecl_stack, sizeof(thecl_t*) * ++state->ecl_cnt);
            state->ecl_stack[state->ecl_cnt - 1] = ecl;
        }
      outer_break_447:
        free($1);
        free($2);
      }
    | DIRECTIVE "default" {
        if (!strcmp($1, "module")) {
            state->ecl = state->main_ecl;
        }
        free($1);
      }
    | DIRECTIVE_FONT IDENTIFIER ENTRY {
        gool_anim_t* anim = malloc(sizeof(gool_anim_t));
        anim->name = strdup($2);

        if (state->version == 1) {
            c1_font_t* font = malloc(sizeof(c1_font_t));
            font->type = 3;
            font->char_count = 0;
            font->eid = gool_to_eid($3);
            anim->size = sizeof(c1_font_t);
            anim->anim = font;
        } else if (state->version == 2) {
            c2_font_t* font = malloc(sizeof(c2_font_t));
            font->type = 3;
            font->char_count = 0;
            font->eid = gool_to_eid($3);
            anim->size = sizeof(c2_font_t);
            anim->anim = font;
        }

        state->current_anim = anim;

        free($2);
        free($3);
      }
      Font_Chars {
        list_append_new(&state->main_ecl->anims, state->current_anim);
        state->current_anim = NULL;
      }
    | DIRECTIVE_SPRITE IDENTIFIER ENTRY {
        gool_anim_t* anim = malloc(sizeof(gool_anim_t));
        anim->name = strdup($2);

        if (state->version == 1) {
            anim->size = sizeof(c1_sprite_t);
            c1_sprite_t* sprite = malloc(sizeof(c1_sprite_t));
            sprite->type = 2;
            sprite->count = 0;
            sprite->eid = gool_to_eid($3);
            anim->anim = sprite;
        } else if (state->version == 2) {
            anim->size = sizeof(c2_sprite_t);
            c2_sprite_t* sprite = malloc(sizeof(c2_sprite_t));
            sprite->type = 2;
            sprite->count = 0;
            sprite->eid = gool_to_eid($3);
            anim->anim = sprite;
        }

        state->current_anim = anim;

        free($2);
        free($3);
      }
      Sprite_Frames {
        list_append_new(&state->main_ecl->anims, state->current_anim);
        state->current_anim = NULL;
      }
    | DIRECTIVE_TEXT IDENTIFIER IDENTIFIER ENTRY {
        gool_anim_t* anim = malloc(sizeof(gool_anim_t));
        anim->name = strdup($2);
        anim->size = sizeof(c1_text_t);

        c1_text_t* text = malloc(sizeof(c1_text_t));
        text->type = 4;
        text->string_count = 0;
        text->unknown = gool_to_eid($4);
        text->font = anim_get_offset(state, $3);

        anim->anim = text;
        state->current_anim = anim;

        free($2);
        free($3);
        free($4);
      }
      String_List {
        list_append_new(&state->main_ecl->anims, state->current_anim);
        state->current_anim = NULL;
      }
    | DIRECTIVE_FANIM IDENTIFIER ENTRY INTEGER {
        gool_anim_t* anim = malloc(sizeof(gool_anim_t));
        anim->name = strdup($2);
        anim->size = sizeof(c1_fraganim_t);

        c1_fraganim_t* fraganim = malloc(sizeof(c1_fraganim_t));
        fraganim->type = 5;
        fraganim->sprite_count = 0;
        fraganim->frag_count = $4;
        fraganim->eid = gool_to_eid($3);

        anim->anim = fraganim;
        state->current_anim = anim;

        free($2);
        free($3);
      }
      Frags {
        c1_fraganim_t* fraganim = state->current_anim->anim;
        ++fraganim->sprite_count;
        fraganim->sprite_count /= fraganim->frag_count;
        list_append_new(&state->main_ecl->anims, state->current_anim);
        state->current_anim = NULL;
      }
    | GlobalVarDeclaration
    ;

Subroutine_Body:
      "{" Instructions "}" {
        state->current_sub->forward_declaration = false;
      }
    ;

State_Body:
      "{" State_Instructions "}"
    ;

Interrupt_Body:
      "state" IDENTIFIER {
        state->current_interrupt->type = INTERRUPT_STATE;
        state->current_interrupt->lambda_name = strdup($2);
        free($2);
      }
    | "sub" IDENTIFIER {
        state->current_interrupt->type = INTERRUPT_SUB;
        state->current_interrupt->lambda_name = strdup($2);
        free($2);
      }
    | {
        char buf[256];
        snprintf(buf, 256, "%s_INTERRUPT_%i_%i", state->current_interrupt->event->name, yylloc.first_line, yylloc.first_column);

        state->current_interrupt->type = INTERRUPT_SUB;
        state->current_interrupt->lambda_name = strdup(buf);

        sub_begin(state, buf);
        state->current_sub->is_inline = false;
      }
      "(" ArgumentDeclaration ")" Subroutine_Body {
        sub_finish(state);
      }
    ;

GlobalVarDeclaration:
      "var" IDENTIFIER {
        if (state->declared_tempfields)
            yyerror(state, "trans arguments used before variable declaration: %s", $2);
        else
            objfield_create(state, $2);
        free($2);
      }
    | GlobalVarDeclaration "," IDENTIFIER {
        if (state->declared_tempfields)
            yyerror(state, "trans arguments used before variable declaration: %s", $3);
        else
            objfield_create(state, $3);
        free($3);
      }
    ;

Font_Chars:
    %empty
    | Font_Chars Font_Char
    ;

Font_Char:
    DIRECTIVE_CHAR INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER { /* jesus christ */
        if (state->version == 1) {
            c1_font_t* font = state->current_anim->anim;
            font = realloc(font, sizeof(c1_font_t) + sizeof(c1_char_t) * ++font->char_count);
            state->current_anim->anim = font;
            state->current_anim->size = sizeof(c1_font_t) + sizeof(c1_char_t) * font->char_count;

            c1_char_t* character = font->chars + font->char_count - 1;
            if ($7 >= 128) {
                yyerror(state, "syntax error, texture x offset is out of bounds");
            }
            int uv = 0;
            if (($9 != 4 && $9 != 8 && $9 != 16 && $9 != 32 && $9 != 64) ||
                ($10 != 4 && $10 != 8 && $10 != 16 && $10 != 32 && $10 != 64)) {
                yyerror(state, "syntax error, invalid texture width/height");
            }
            if ($9 == 4) uv = 0;
            if ($9 == 8) uv = 1;
            if ($9 == 16) uv = 2;
            if ($9 == 32) uv = 3;
            if ($9 == 64) uv = 4;
            if ($10 == 4) uv += 0;
            if ($10 == 8) uv += 5;
            if ($10 == 16) uv += 10;
            if ($10 == 32) uv += 15;
            if ($10 == 64) uv += 20;
            character->tex1 = $2 & 0xFFFFFF; /* rgb */
            character->tex1 |= ($5 & 0xF) << 24; /* clutx */
            character->tex1 |= ($4 & 0x3) << 29; /* blend */
            character->tex2 = $8 & 0x1F; /* yoff */
            character->tex2 |= ($6 & 0x7F) << 6; /* cluty */
            character->tex2 |= ($7 & 0x1F) << 13; /* xoff */
            character->tex2 |= (($7 / 0x20) & 0x3) << 18; /* segment */
            character->tex2 |= ($3 & 0x3) << 20; /* color */
            character->tex2 |= (uv & 0x3FF) << 22; /* uv */
            if (character->tex1 || character->tex2)
                character->tex1 |= 0x80000000;
            character->w = $11;
            character->h = $12;
        } else if (state->version == 2) {
            c2_font_t* font = state->current_anim->anim;
            font = realloc(font, sizeof(c2_font_t) + sizeof(c2_char_t) * ++font->char_count);
            state->current_anim->anim = font;
            state->current_anim->size = sizeof(c2_font_t) + sizeof(c2_char_t) * font->char_count;

            c2_char_t* character = font->chars + font->char_count - 1;
            int x = $7;
            int y = $8;
            int w = $9;
            int h = $10;
            int segsize = 256 >> $3;
            if ((x & 0xff) + w > 256) {
                yyerror(state, "syntax error, aligned texture is too wide");
            }
            if (y + h > 128) {
                yyerror(state, "syntax error, texture is too tall");
            }
            if (y < 0 || x < 0) {
                yyerror(state, "syntax error, invalid texture parameters");
            }
            --w;
            --h;
            character->tex.r = $2 >> 0 & 0xFF;
            character->tex.g = $2 >> 8 & 0xFF;
            character->tex.b = $2 >> 16 & 0xFF;
            character->tex.primtype = $2 == 0 ? 0 : 11;
            character->tex.unk1 = 0;
            character->tex.unk2 = 0;
            character->tex.unused1 = 0;
            character->tex.unk3 = 0;
            character->tex.additive = $4 >> 1 & 0x1;
            character->tex.unk4 = 0;
            character->tex.unk5 = 0;
            character->tex.segment = x / segsize;
            x &= segsize - 1;
            character->tex.color = $3;
            character->tex.blend = $4 & 0x1;
            character->tex.cx = $5;
            character->tex.cy = $6;
            character->tex.u1 = x;
            character->tex.v1 = y;
            character->tex.u2 = x+w;
            character->tex.v2 = y;
            character->tex.u3 = x;
            character->tex.v3 = y+h;
            character->tex.u4 = x+w;
            character->tex.v4 = y+h;
            character->w = $11;
            character->h = $12;
        }
    }
    ;

Sprite_Frames:
    %empty
    | Sprite_Frames Sprite_Frame
    ;

Sprite_Frame:
    DIRECTIVE_TEXTURE INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER { /* rgb color blend cx cy x y w h */
        if (state->version == 1) {
            c1_sprite_t* sprite = state->current_anim->anim;
            sprite = realloc(sprite, sizeof(c1_sprite_t) + sizeof(c1_frame_t) * ++sprite->count);
            state->current_anim->anim = sprite;
            state->current_anim->size = sizeof(c1_sprite_t) + sizeof(c1_frame_t) * sprite->count;

            c1_frame_t* frame = sprite->frames + sprite->count - 1;
            if ($7 >= 128) {
                yyerror(state, "syntax error, texture x offset is out of bounds");
            }
            int uv = 0;
            if (($9 != 4 && $9 != 8 && $9 != 16 && $9 != 32 && $9 != 64) ||
                ($10 != 4 && $10 != 8 && $10 != 16 && $10 != 32 && $10 != 64)) {
                yyerror(state, "syntax error, invalid texture width/height");
            }
            if ($9 == 4) uv = 0;
            if ($9 == 8) uv = 1;
            if ($9 == 16) uv = 2;
            if ($9 == 32) uv = 3;
            if ($9 == 64) uv = 4;
            if ($10 == 4) uv += 0;
            if ($10 == 8) uv += 5;
            if ($10 == 16) uv += 10;
            if ($10 == 32) uv += 15;
            if ($10 == 64) uv += 20;
            frame->tex1 = $2 & 0xFFFFFF; /* rgb */
            frame->tex1 |= ($5 & 0xF) << 24; /* clutx */
            frame->tex1 |= ($4 & 0x3) << 29; /* blend */
            frame->tex2 = $8 & 0x1F; /* yoff */
            frame->tex2 |= ($6 & 0x7F) << 6; /* cluty */
            frame->tex2 |= ($7 & 0x1F) << 13; /* xoff */
            frame->tex2 |= (($7 / 0x20) & 0x3) << 18; /* segment */
            frame->tex2 |= ($3 & 0x3) << 20; /* color */
            frame->tex2 |= (uv & 0x3FF) << 22; /* uv */
            if (frame->tex1 || frame->tex2)
                frame->tex1 |= 0x80000000;
        } else if (state->version == 2) {
            c2_sprite_t* sprite = state->current_anim->anim;
            sprite = realloc(sprite, sizeof(c2_sprite_t) + sizeof(c2_tex_t) * ++sprite->count);
            state->current_anim->anim = sprite;
            state->current_anim->size = sizeof(c2_sprite_t) + sizeof(c2_tex_t) * sprite->count;

            c2_tex_t* tex = sprite->frames + sprite->count - 1;
            int x = $7;
            int y = $8;
            int w = $9;
            int h = $10;
            int segsize = 256 >> $3;
            if ((x & 0xff) + w > 256) {
                yyerror(state, "syntax error, aligned texture is too wide");
            }
            if (y + h > 128) {
                yyerror(state, "syntax error, texture is too tall");
            }
            if (y < 0 || x < 0) {
                yyerror(state, "syntax error, invalid texture parameters");
            }
            --w;
            --h;
            tex->r = $2 >> 0 & 0xFF;
            tex->g = $2 >> 8 & 0xFF;
            tex->b = $2 >> 16 & 0xFF;
            tex->primtype = $2 == 0 ? 0 : 11;
            tex->unk1 = 0;
            tex->unk2 = 0;
            tex->unused1 = 0;
            tex->unk3 = 0;
            tex->additive = $4 >> 1 & 0x1;
            tex->unk4 = 0;
            tex->unk5 = 0;
            tex->segment = x / segsize;
            x &= segsize - 1;
            tex->color = $3;
            tex->blend = $4 & 0x1;
            tex->cx = $5;
            tex->cy = $6;
            tex->u1 = x;
            tex->v1 = y;
            tex->u2 = x+w;
            tex->v2 = y;
            tex->u3 = x;
            tex->v3 = y+h;
            tex->u4 = x+w;
            tex->v4 = y+h;
        }
    }
    ;

String_List:
    %empty
    | String_List TEXT {
        size_t stringlen = strlen($2) + 1;

        state->current_anim->anim = realloc(state->current_anim->anim, state->current_anim->size + stringlen);

        c1_text_t* text = state->current_anim->anim;
        ++text->string_count;

        char *string = (char*)state->current_anim->anim + state->current_anim->size;
        strcpy(string, $2);

        state->current_anim->size += stringlen;

        free($2);
    }
    ;

Frags:
    %empty
    | Frags Frag
    ;

Frag:
    DIRECTIVE_FRAG INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER Literal_Int Literal_Int Literal_Int Literal_Int {
        state->current_anim->anim = realloc(state->current_anim->anim, state->current_anim->size + sizeof(c1_frag_t));
        c1_fraganim_t* fraganim = state->current_anim->anim;
        c1_frag_t* frag = (char*)fraganim + state->current_anim->size;
        state->current_anim->size += sizeof(c1_frag_t);
        ++fraganim->sprite_count;

        if ($7 >= 128) {
            yyerror(state, "syntax error, texture x offset is out of bounds");
        }
        int uv = 0;
        if (($9 != 4 && $9 != 8 && $9 != 16 && $9 != 32 && $9 != 64) ||
            ($10 != 4 && $10 != 8 && $10 != 16 && $10 != 32 && $10 != 64)) {
            yyerror(state, "syntax error, invalid texture width/height");
        }
        if ($9 == 4) uv = 0;
        if ($9 == 8) uv = 1;
        if ($9 == 16) uv = 2;
        if ($9 == 32) uv = 3;
        if ($9 == 64) uv = 4;
        if ($10 == 4) uv += 0;
        if ($10 == 8) uv += 5;
        if ($10 == 16) uv += 10;
        if ($10 == 32) uv += 15;
        if ($10 == 64) uv += 20;
        frag->tex1 = $2 & 0xFFFFFF; /* rgb */
        frag->tex1 |= ($5 & 0xF) << 24; /* clutx */
        frag->tex1 |= ($4 & 0x3) << 29; /* blend */
        frag->tex2 = $8 & 0x1F; /* yoff */
        frag->tex2 |= ($6 & 0x7F) << 6; /* cluty */
        frag->tex2 |= ($7 & 0x1F) << 13; /* xoff */
        frag->tex2 |= (($7 / 0x20) & 0x3) << 18; /* segment */
        frag->tex2 |= ($3 & 0x3) << 20; /* color */
        frag->tex2 |= (uv & 0x3FF) << 22; /* uv */
        if (frag->tex1 || frag->tex2)
            frag->tex1 |= 0x80000000;
        frag->x = $11;
        frag->y = $12;
        frag->w = $13;
        frag->h = $14;
    }
    ;

VarDeclaration:
      "var" IDENTIFIER {
          var_create(state, state->current_sub, $2, true);
          free($2);
      }
    | "var" IDENTIFIER "=" Expression {
          var_create_assign(state, state->current_sub, $2, $4);
          free($2);
      }
    | VarDeclaration "," IDENTIFIER {
          var_create(state, state->current_sub, $3, true);
          free($3);
      }
    | VarDeclaration "," IDENTIFIER "=" Expression {
          var_create_assign(state, state->current_sub, $3, $5);
          free($3);
      }
    ;

ArgumentDeclaration:
    %empty
    | IDENTIFIER {
          arg_create(state, state->current_sub, $1);
          free($1);
      }
    | ArgumentDeclaration "," IDENTIFIER {
          arg_create(state, state->current_sub, $3);
          free($3);
      }
    ;

State_Instructions:
    %empty
    | State_Instructions "__transargs" {
        state->current_state->trans_args = true;
        state->declared_tempfields = true;
    }
    | State_Instructions IDENTIFIER INTEGER {
        if (!strcmp($2, "stateflag"))
            state->current_state->stateflag = $3;
        else if (!strcmp($2, "statusc"))
            state->current_state->statusc = $3;
        else {
            yyerror(state, "syntax error, unpexpected %s in state body", $2);
        }
        free($2);
      }
    | State_Instructions "trans" {
        if (state->current_state->trans)
            yyerror(state, "duplicate trans block in state: %s", state->current_state->name);
        char buf[256];
        snprintf(buf, 256, "__%s_TRANS", state->current_state->name);
        sub_begin(state, buf);
        state->current_sub->is_inline = false;
        state->current_sub->is_trans = true;
        state->current_state->trans = state->current_sub;
        
        if (state->current_state->trans_args) {
            if (!state->current_state->code) {
                yyerror(state, "cannot use trans args without code block first in state: %s", state->current_state->name);
                state->current_state->trans_args = false;
            }
            else {
                for (int a=0; a < state->current_state->code->arg_count; ++a) {
                    objfield_create(state, state->current_state->code->args[a]->name);
                }
            }
        }
      }
      Subroutine_Body {
        if (state->current_state->trans_args) {
            for (int a=state->current_state->code->arg_count-1; a >= 0; --a) {
                objfield_delete(state, state->current_state->code->args[a]->name);
            }
        }
        sub_finish(state);
      }
    | State_Instructions "code" {
        if (state->current_state->code)
            yyerror(state, "duplicate code block in state: %s", state->current_state->name);
        char buf[256];
        snprintf(buf, 256, "__%s_CODE", state->current_state->name);
        sub_begin(state, buf);
        state->current_sub->is_inline = false;
        state->current_state->code = state->current_sub;
      }
      "(" ArgumentDeclaration ")" Subroutine_Body {
        sub_finish(state);
      }
    | State_Instructions "event" {
        if (state->current_state->event)
            yyerror(state, "duplicate event block in state: %s", state->current_state->name);
        char buf[256];
        snprintf(buf, 256, "__%s_EVENT", state->current_state->name);
        sub_begin(state, buf);
        state->current_sub->is_inline = false;
        state->current_state->event = state->current_sub;
      }
      "(" ArgumentDeclaration ")" Subroutine_Body {
        sub_finish(state);
      }
    ;

Instructions:
    %empty
    | Instructions IDENTIFIER ":" { label_create(state, $2); free($2); }
    | Instructions Instruction
    | Instructions Block
    ;

BlockVarDeclaration:
      "var" IDENTIFIER {
          scope_begin(state);
          var_create(state, state->current_sub, $2, true);
          free($2);
      }
    | "var" IDENTIFIER "=" Expression {
          scope_begin(state);
          var_create_assign(state, state->current_sub, $2, $4);
          free($2);
      }
    | BlockVarDeclaration "," IDENTIFIER {
          var_create(state, state->current_sub, $3, true);
          free($3);
      }
    | BlockVarDeclaration "," IDENTIFIER "=" Expression {
          var_create_assign(state, state->current_sub, $3, $5);
          free($3);
      }
    ;

ParenExpression:
      "(" Expression ")"
        { $$ = $2; }
    ;

ParenExpression2:
      "(" BlockVarDeclaration "," Expression ")"
        { $$ = $4; }
    ;

Block:
      IfBlock
    | WhileBlock
    | CodeBlock
    | OnceBlock
    ;

OnceBlock:
    "once" {
        state->current_sub->has_once = true;
    } CodeBlock {
        const expr_t* expr = expr_get_by_symbol(state->version, ASSIGN);

        thecl_param_t* p1 = param_var_new(field_get("tpc")->offset);
        thecl_param_t* p2 = param_var_new(field_get("pc")->offset);

        instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", p1, p2));
    }
    | "nofirst" {
        state->current_sub->has_nofirst = true;

        instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, ADD)->id, "pp",
        param_var_new(field_get("pc")->offset), param_val_new(4*2)));
        instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, ASSIGN)->id, "pp",
        param_var_new(field_get("tpc")->offset), param_sp_new()));

        char labelstr[256];
        snprintf(labelstr, 256, "nofirst_%i_%i", yylloc.first_line, yylloc.first_column);
        list_prepend_new(&state->block_stack, strdup(labelstr));
        expression_create_goto(state, GOTO, labelstr, NULL);
    } CodeBlock {
        list_node_t *head = state->block_stack.head;
        label_create(state, head->data);
        state->block_stack.head = head->next;
        free(head->data);
        list_del(&state->block_stack, head);
    }
    ;

CodeBlock:
    "{" { scope_begin(state); }
      Instructions "}" { scope_finish(state, true); }
    | Instruction ";"
    ;

ContinueStatement:
      "continue" {
          list_node_t *head = state->block_stack.head;
          for(; head; head = head->next) {
              if (
                  strncmp(head->data, "do", 2) == 0 ||
                  strncmp(head->data, "while", 5) == 0 ||
                  strncmp(head->data, "until", 5) == 0
              ) {
                  char labelstr[256];
                  snprintf(labelstr, 256, "%s_continue", (char*)head->data);
                  expression_create_goto(state, GOTO, labelstr, NULL);
                  break;
              }
          }
          if(!head) {
              yyerror(state, "continue not within while or until");
              g_was_error = true;
          }
      }
      ;

BreakStatement:
      "break" {
          list_node_t *head = state->block_stack.head;
          for(; head; head = head->next) {
              if (
                  strncmp(head->data, "do", 2) == 0 ||
                  strncmp(head->data, "while", 5) == 0 ||
                  strncmp(head->data, "until", 5) == 0
              ) {
                  char labelstr[256];
                  snprintf(labelstr, 256, "%s_end", (char*)head->data);
                  expression_create_goto(state, GOTO, labelstr, NULL);
                  break;
              }
          }
          if(!head) {
              yyerror(state, "break not within while or until");
              g_was_error = true;
          }
      }
      ;

IfBlock:
    "unless" ParenExpression[cond]  /*%expect 1*/ {
          char labelstr[256];
          snprintf(labelstr, 256, "unless_%i_%i", yylloc.first_line, yylloc.first_column);
          list_prepend_new(&state->block_stack, strdup(labelstr));
          expression_create_goto(state, IF, labelstr, $cond);
          expression_free($cond);
      } CodeBlock ElseBlock {
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          state->block_stack.head = head->next;
          free(head->data);
          list_del(&state->block_stack, head);
        }
    | "unless" ParenExpression2[cond]  /*%expect 1*/ {
          char labelstr[256];
          snprintf(labelstr, 256, "unless_%i_%i", yylloc.first_line, yylloc.first_column);
          list_prepend_new(&state->block_stack, strdup(labelstr));
          expression_create_goto(state, IF, labelstr, $cond);
          expression_free($cond);
      } CodeBlock ElseBlock {
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          state->block_stack.head = head->next;
          free(head->data);
          list_del(&state->block_stack, head);
          scope_finish(state, true);
        }
    | "if" ParenExpression[cond]  /*%expect 1*/ {
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          list_prepend_new(&state->block_stack, strdup(labelstr));
          expression_create_goto(state, UNLESS, labelstr, $cond);
          expression_free($cond);
      } CodeBlock ElseBlock {
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          free(head->data);
          list_del(&state->block_stack, head);
      }
    | "if" ParenExpression2[cond]  /*%expect 1*/ {
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          list_prepend_new(&state->block_stack, strdup(labelstr));
          expression_create_goto(state, UNLESS, labelstr, $cond);
          expression_free($cond);
      } CodeBlock ElseBlock {
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          free(head->data);
          list_del(&state->block_stack, head);
          scope_finish(state, true);
      }
      ;

ElseBlock:
    %empty
    | "else"  {
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          expression_create_goto(state, GOTO, labelstr, NULL);
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          free(head->data);
          list_del(&state->block_stack, head);
          list_prepend_new(&state->block_stack, strdup(labelstr));
    } CodeBlock
    | "else" {
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          expression_create_goto(state, GOTO, labelstr, NULL);
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          free(head->data);
          list_del(&state->block_stack, head);
          list_prepend_new(&state->block_stack, strdup(labelstr));
    } IfBlock
      ;

WhileBlock:
      "while" ParenExpression[cond] {
          if ($cond->type == EXPRESSION_VAL && !$cond->value->stack && !$cond->value->value.val.S) {
              ++state->ignore_block;
              expression_free($cond);
              break;
          }
          char labelstr[256];
          snprintf(labelstr, 256, "while_%i_%i", yylloc.first_line, yylloc.first_column);
          char labelstr_st[256];
          char labelstr_end[256];
          snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
          snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);

          list_prepend_new(&state->block_stack, strdup(labelstr));
          label_create(state, labelstr_st);
          expression_create_goto(state, UNLESS, labelstr_end, $cond);
          expression_free($cond);
      } CodeBlock {
          if (state->ignore_block) {
              --state->ignore_block;
              break;
          }
          char labelstr_st[256];
          char labelstr_end[256];
          char labelstr_continue[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);
          snprintf(labelstr_continue, 256, "%s_continue", (char*)head->data);

          label_create(state, labelstr_continue);
          expression_create_goto(state, GOTO, labelstr_st, NULL);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
      }
    | "while" ParenExpression2[cond] {
          if ($cond->type == EXPRESSION_VAL && !$cond->value->stack && !$cond->value->value.val.S) {
              ++state->ignore_block;
              expression_free($cond);
              break;
          }
          char labelstr[256];
          snprintf(labelstr, 256, "while_%i_%i", yylloc.first_line, yylloc.first_column);
          char labelstr_st[256];
          char labelstr_end[256];
          snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
          snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);

          list_prepend_new(&state->block_stack, strdup(labelstr));
          label_create(state, labelstr_st);
          expression_create_goto(state, UNLESS, labelstr_end, $cond);
          expression_free($cond);
      } CodeBlock {
          if (state->ignore_block) {
              --state->ignore_block;
              scope_finish(state, true);
              break;
          }
          char labelstr_st[256];
          char labelstr_end[256];
          char labelstr_continue[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);
          snprintf(labelstr_continue, 256, "%s_continue", (char*)head->data);

          label_create(state, labelstr_continue);
          expression_create_goto(state, GOTO, labelstr_st, NULL);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
          scope_finish(state, true);
      }
    | "until" ParenExpression[cond] {
          if ($cond->type == EXPRESSION_VAL && !$cond->value->stack && $cond->value->value.val.S) {
              ++state->ignore_block;
              expression_free($cond);
              break;
          }
          char labelstr[256];
          snprintf(labelstr, 256, "until_%i_%i", yylloc.first_line, yylloc.first_column);
          char labelstr_st[256];
          char labelstr_end[256];
          snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
          snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);

          list_prepend_new(&state->block_stack, strdup(labelstr));
          label_create(state, labelstr_st);
          expression_create_goto(state, IF, labelstr_end, $cond);
          expression_free($cond);
      } CodeBlock {
          if (state->ignore_block) {
              --state->ignore_block;
              break;
          }
          char labelstr_st[256];
          char labelstr_end[256];
          char labelstr_continue[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);
          snprintf(labelstr_continue, 256, "%s_continue", (char*)head->data);

          label_create(state, labelstr_continue);
          expression_create_goto(state, GOTO, labelstr_st, NULL);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
      }
    | "until" ParenExpression2[cond] {
          if ($cond->type == EXPRESSION_VAL && !$cond->value->stack && $cond->value->value.val.S) {
              ++state->ignore_block;
              expression_free($cond);
              scope_finish(state, true);
              break;
          }
          char labelstr[256];
          snprintf(labelstr, 256, "until_%i_%i", yylloc.first_line, yylloc.first_column);
          char labelstr_st[256];
          char labelstr_end[256];
          snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
          snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);

          list_prepend_new(&state->block_stack, strdup(labelstr));
          label_create(state, labelstr_st);
          expression_create_goto(state, IF, labelstr_end, $cond);
          expression_free($cond);
          scope_finish(state, true);
      } CodeBlock {
          if (state->ignore_block) {
              --state->ignore_block;
              break;
          }
          char labelstr_st[256];
          char labelstr_end[256];
          char labelstr_continue[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);
          snprintf(labelstr_continue, 256, "%s_continue", (char*)head->data);

          label_create(state, labelstr_continue);
          expression_create_goto(state, GOTO, labelstr_st, NULL);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
      }
    | "do" {
          char labelstr[256];
          snprintf(labelstr, 256, "do_%i_%i", yylloc.first_line, yylloc.first_column);
          char labelstr_st[256];
          char labelstr_end[256];
          snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
          snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);

          list_prepend_new(&state->block_stack, strdup(labelstr));
          label_create(state, labelstr_st);
    } DoBlock
    | "do" "(" BlockVarDeclaration ")" {
          char labelstr[256];
          snprintf(labelstr, 256, "do_%i_%i", yylloc.first_line, yylloc.first_column);
          char labelstr_st[256];
          char labelstr_end[256];
          snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
          snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);

          list_prepend_new(&state->block_stack, strdup(labelstr));
          label_create(state, labelstr_st);
    } DoBlock { scope_finish(state, true); }
    ;
DoBlock:
      CodeBlock "while" ParenExpression[cond]  {
          char labelstr_st[256];
          char labelstr_end[256];
          char labelstr_continue[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);
          snprintf(labelstr_continue, 256, "%s_continue", (char*)head->data);

          label_create(state, labelstr_continue);
          expression_create_goto(state, IF, labelstr_st, $cond);
          expression_free($cond);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
    }
    | CodeBlock "until" ParenExpression[cond]  {
          char labelstr_st[256];
          char labelstr_end[256];
          char labelstr_continue[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);
          snprintf(labelstr_continue, 256, "%s_continue", (char*)head->data);

          label_create(state, labelstr_continue);
          expression_create_goto(state, UNLESS, labelstr_st, $cond);
          expression_free($cond);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
    }
    ;

Instruction:
      IDENTIFIER "(" Instruction_Parameters ")" {
        const gool_ins_t* gool_ins = gool_ins_get_by_name(state->version, $1);
        if (gool_ins) {
            if (gool_ins->varargs) {
                list_t* param_list = list_new();
                list_t* arg_list = list_new();

                int argc = gool_ins->param_count;
                if ($3 != NULL) {
                    list_node_t* node, *next_node;
                    list_for_each_node_safe($3, node, next_node) {
                        if (argc-- > 0)
                            list_append_new(param_list, node->data);
                        else if (gool_ins->reverse_args)
                            list_prepend_new(arg_list, node->data);
                        else
                            list_append_new(arg_list, node->data);
                    }
                }

                thecl_param_t* param;
                expression_t* late_expr = NULL;
                thecl_param_t* late_param = NULL;
                if (gool_ins->late_param >= 0) {
                    int i = 0;
                    list_node_t* e = state->expressions.tail;
                    list_for_each(param_list, param) {
                        if (i++ == gool_ins->late_param && param->is_expression_param) {
                            late_expr = e->data;
                            late_param = param;
                            list_del(&state->expressions, e);
                            break;
                        }
                        else if (param->is_expression_param) {
                            e = e->prev;
                        }
                    }
                }

                list_node_t* expr_node;
                if (gool_ins->reverse_args)
                    expr_node = state->expressions.head;
                else
                    expr_node = state->expressions.tail;

                if (!gool_ins->reverse_args) {
                    list_for_each(param_list, param) {
                        if (param->is_expression_param && param != late_param) {
                            expression_t* expression = expr_node->data;
                            expression_output(state, expression);
                            expression_free(expression);
                            if (gool_ins->reverse_args) {
                                expr_node = expr_node->next;
                            }
                            else {
                                expr_node = expr_node->prev;
                            }
                        }
                    }
                }
                list_for_each(arg_list, param) {
                    if (!(param->stack && param->object_link == 0 && param->value.val.S == 0x1F)) { /* argument is already on the stack */
                        if (param->object_link == -3) { /* TODO */
                            const expr_t* expr = expr_get_by_symbol(state->version, PLOAD);
                            instr_add(state, state->current_sub, instr_new(state, expr->id, "p", param));
                        }
                        else {
                            const expr_t* expr = expr_get_by_symbol(state->version, LOAD);
                            instr_add(state, state->current_sub, instr_new(state, expr->id, "p", param));
                        }
                    }
                    else if (param->is_expression_param) {
                        expression_t* expression = expr_node->data;
                        expression_output(state, expression);
                        expression_free(expression);
                        if (gool_ins->reverse_args) {
                            expr_node = expr_node->next;
                        }
                        else {
                            expr_node = expr_node->prev;
                        }
                    }
                }
                if (gool_ins->reverse_args) {
                    list_for_each(param_list, param) {
                        if (param->is_expression_param) {
                            expression_t* expression = expr_node->data;
                            expression_output(state, expression, 1);
                            expression_free(expression);
                            if (gool_ins->reverse_args) {
                                expr_node = expr_node->next;
                            }
                            else {
                                expr_node = expr_node->prev;
                            }
                        }
                    }
                }
                list_free_nodes(&state->expressions);

                if (late_expr) {
                    if (late_expr->type != EXPRESSION_VAL || late_param->stack != 1 || late_param->object_link != 0 || late_param->value.val.S < 0 || late_param->value.val.S > 0x3F) {
                        expression_output(state, late_expr);
                        expression_free(late_expr);
                        late_param->stack = 1;
                        late_param->object_link = 0;
                        late_param->value.val.S = 0x1F;
                    }
                }

                instr_add(state, state->current_sub, instr_new_list(state, gool_ins->id, gool_ins->param_list_validate(param_list, list_count(arg_list))));

                if (gool_ins->pop_args) {
                    if (argc = list_count(arg_list)) {
                        const expr_t* expr = expr_get_by_symbol(state->version, GOTO);
                        instr_add(state, state->current_sub, instr_new(state, expr->id, "SSSSS", 0, argc, 0x25, 0, 0));
                    }
                }

                list_free_nodes(param_list);
                free(param_list);

                list_free_nodes(arg_list);
                free(arg_list);
            }
            else {
                expression_t* expression;
                list_for_each(&state->expressions, expression) {
                    expression_output(state, expression);
                    expression_free(expression);
                }
                list_free_nodes(&state->expressions);

                if (gool_ins->late_param >= 0) {
                    thecl_param_t* param = NULL;
                    int i = 0;
                    list_for_each($3, param) {
                        if (i++ == gool_ins->late_param && !param->is_expression_param) {
                            break;
                        }
                        param = NULL;
                    }

                    if (param && (param->stack != 1 || param->object_link != 0 || param->value.val.S < 0 || param->value.val.S > 0x3F)) {
                        expression_t* expression = expression_load_new(state, param_copy(param));
                        expression_output(state, expression);
                        expression_free(expression);
                        param->stack = 1;
                        param->object_link = 0;
                        param->value.val.S = 0x1F;
                    }
                }
                if (!$3) {
                    $3 = list_new();
                }
                instr_add(state, state->current_sub, instr_new_list(state, gool_ins->id, gool_ins->param_list_validate($3, 0)));
            }
        }
        else {
            const expr_t* expr = expr_get_by_symbol(state->version, CALL);
            instr_create_call(state, expr->id, $1, $3);
        }
        if ($3 != NULL) {
            list_free_nodes($3);
            free($3);
        }
      }
    /*| "goto" IDENTIFIER {
        expression_create_goto(state, GOTO, $2, NULL);
    }*/
    | Assignment
    | VarDeclaration
    | BreakStatement
    | ContinueStatement
    | "return" {
        if (state->current_sub->is_inline)
            expression_create_goto(state, GOTO, "inline_end", NULL);
        else {
            int pop = 0;
            thecl_variable_t* var;
            for (int v=0; v < state->current_sub->var_count; ++v)
                for (int s=1; s < state->scope_cnt; ++s)
                    if (state->current_sub->vars[v]->scope == state->scope_stack[s])
                        ++pop;
            if (pop > 0) {
                char buf[512];
                snprintf(buf, 512, "@%s_sub_end", state->current_sub->name);
                expression_create_goto_pop(state, GOTO, buf, NULL, pop);
            }
            else {
                const expr_t* expr = expr_get_by_symbol(state->version, RETURN);
                instr_add(state, state->current_sub, instr_new(state, expr->id, "SSSSS", 0, 0, 0x25, 0, 2));
            }
        }
    }
    ;

Assignment:
      Address "=" Expression {
        if ($1->type == 'o') {
            yyerror(state, "invalid address: %s", $1->value.val.z);
            return 0;
        }
        const expr_t* expr = expr_get_by_id(state->version, $3->id);
        thecl_param_t* src_param = expr != NULL && expr->is_unary ? ((expression_t*)$3->children.head->data)->value : NULL;
        if (($1->stack == 1 && src_param && src_param->value.val.S == 0x1F && src_param->stack == 1 && src_param->object_link == 0)) {
            src_param->value.val.S = $1->value.val.S;
            if ($1->object_link == -1 && $1->value.val.S >= 3) {
                state->current_sub->vars[$1->value.val.S - 3]->is_written = true;
            }
            else if ($1->object_link == -1 && $1->value.val.S < 0) {
                state->current_sub->args[-$1->value.val.S - 1]->is_written = true;
            }
            param_free($1);
            expression_output(state, $3);
            expression_free($3);
            break;
        }

        if ($3->type == EXPRESSION_VAL && ($1->stack != 2 || ($1->stack == 2 && !expr->is_unary))) {
            src_param = $3->value;
        } else {
            expression_output(state, $3);
            src_param = param_sp_new();
        }
        expression_free($3);
        if ($1->stack != 2) {
            expr = expr_get_by_symbol(state->version, ASSIGN);

            instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", $1, src_param));

            if ($1->object_link == -1 && $1->value.val.S >= 3) {
                state->current_sub->vars[$1->value.val.S - 3]->is_written = true;
            }
            else if ($1->object_link == -1 && $1->value.val.S < 0) {
                state->current_sub->args[-$1->value.val.S - 1]->is_written = true;
            }
        } else { /* WGL */
            expr = expr_get_by_symbol(state->version, GASSIGN);

            instr_add(state, state->current_sub, instr_new(state, expr->id, "Sp", $1->value.val.S, src_param));
        }
      }
    | Address "+=" Expression { var_shorthand_assign(state, $1, $3, ADD); }
    | Address "-=" Expression { var_shorthand_assign(state, $1, $3, SUBTRACT); }
    | Address "*=" Expression { var_shorthand_assign(state, $1, $3, MULTIPLY); }
    | Address "/=" Expression { var_shorthand_assign(state, $1, $3, DIVIDE); }
    | Address "%=" Expression { var_shorthand_assign(state, $1, $3, MODULO); }
    | Address "^=" Expression { var_shorthand_assign(state, $1, $3, XOR); }
    | Address "|=" Expression { var_shorthand_assign(state, $1, $3, B_OR); }
    | Address "&=" Expression { var_shorthand_assign(state, $1, $3, B_AND); }
    | Address "<<=" Expression { var_shorthand_assign(state, $1, $3, LSHIFT); }
    | Address ">>=" Expression { var_shorthand_assign(state, $1, EXPR_2(SUBTRACT, expression_val_new(state, 0), $3), LSHIFT); }
    | Address "\\=" Expression { var_shorthand_assign(state, $1, $3, TEST); }
    ;

Instruction_Parameters:
    %empty { $$ = NULL; }
    | Instruction_Parameters_List
    ;

Instruction_Parameters_List:
      Instruction_Parameter {
        $$ = list_new();
        list_append_new($$, $1);
      }
    | Instruction_Parameters_List "," Instruction_Parameter {
        $$ = $1;
        list_append_new($$, $3);
      }
    ;

Instruction_Parameter:
      Load_Type
    | ExpressionSubsetInstParam {
          if ($1->type == EXPRESSION_VAL) {
              $$ = param_copy($1->value);
          } else {
              list_prepend_new(&state->expressions, $1);

              $$ = param_new('S');
              $$->stack = 1;
              $$->object_link = 0;
              $$->is_expression_param = 1;
              $$->value.val.S = 0x1F;
          }
      }
    ;

Expression:
      ExpressionLoadType
    | ExpressionSubset
    ;

ExpressionSubsetInstParam:
      ExpressionSubset
    ;

ExpressionLoadType:
      Load_Type                      { $$ = expression_load_new(state, $1); }
    ;

/* This is the lowest common denominator between expression-instructions and expression-parameters */
ExpressionSubset:
      MACRO { $$ = expression_copy(macro_get(state, $1)->expr); }
    |             "(" Expression ")" { $$ = $2; }
    | Expression "+"   Expression { $$ = EXPR_2(ADD,      $1, $3); }
    | Expression "-"   Expression { $$ = EXPR_2(SUBTRACT, $1, $3); }
    | Expression "*"   Expression { $$ = EXPR_2(MULTIPLY, $1, $3); }
    | Expression "/"   Expression { $$ = EXPR_2(DIVIDE,   $1, $3); }
    | Expression "%"   Expression { $$ = EXPR_2(MODULO,   $1, $3); }
    | Expression "=="  Expression { $$ = EXPR_2(EQUAL,    $1, $3); }
    | Expression "<"   Expression { $$ = EXPR_2(LT,       $1, $3); }
    | Expression "<="  Expression { $$ = EXPR_2(LTEQ,     $1, $3); }
    | Expression ">"   Expression { $$ = EXPR_2(GT,       $1, $3); }
    | Expression ">="  Expression { $$ = EXPR_2(GTEQ,     $1, $3); }
    | "!" Expression              { $$ = EXPR_2(NOT,      expression_load_new(state, param_sp_new()), $2); }
    | "~" Expression              { $$ = EXPR_2(B_NOT,    expression_load_new(state, param_sp_new()), $2); }
    | Expression "||"  Expression { $$ = EXPR_2(OR,       $1, $3); }
    | Expression "&&"  Expression { $$ = EXPR_2(AND,      $1, $3); }
    | Expression "^"   Expression { $$ = EXPR_2(XOR,      $1, $3); }
    | Expression "|"   Expression { $$ = EXPR_2(B_OR,     $1, $3); }
    | Expression "&"   Expression { $$ = EXPR_2(B_AND,    $1, $3); }
    | Expression "<<"  Expression { $$ = EXPR_2(LSHIFT,   $1, $3); }
    | Expression ">>"  Expression { $$ = EXPR_2(LSHIFT,   $1, EXPR_2(SUBTRACT, expression_val_new(state, 0), $3)); }
    | Expression "\\"  Expression { $$ = EXPR_2(TEST,     $1, $3); }
    | Expression "!="  Expression {
        $$ = EXPR_2(INEQUAL,  $1, $3);
        $$ = EXPR_2(NOT,      expression_load_new(state, param_sp_new()), $$);
      }
    | "+" Expression              { $$ = $2; }
    | "-" Expression              { $$ = EXPR_2(SUBTRACT, expression_val_new(state, 0), $2); }
    | "&" Expression              { $$ = EXPR_2(ADDRESSOF,expression_load_new(state, param_sp_new()), $2); }
    | "abs" "(" Expression ")"    { $$ = EXPR_2(ABS,      expression_load_new(state, param_sp_new()), $3); }
    | "getanim" "(" Expression ")"{ $$ = EXPR_2(GETANIM,  expression_load_new(state, param_sp_new()), EXPR_2(LSHIFT, $3, expression_val_new(state, 8))); }
    | "seek" "(" Expression "," Expression "," Expression ")"     { $$ = EXPR_3(SEEK, $3, $5, $7); }
    | "seek" "(" Expression "," Expression ")"                    { $$ = EXPR_2(SEEK, $3, $5); }
    | "degseek" "(" Expression "," Expression "," Expression ")"  { $$ = EXPR_3(DEGSEEK, $3, $5, $7); }
    | "degseek" "(" Expression "," Expression ")"                 { $$ = EXPR_2(DEGSEEK, $3, $5); }
    | "degdist" "(" Expression "," Expression ")"                 { $$ = EXPR_2(DEGDIST, $3, $5); }
    | "rand" "(" Expression ")"                                   { $$ = EXPR_2(RAND, expression_val_new(state, 0), $3); }
    | "rand" "(" Expression "," Expression ")"                    { $$ = EXPR_2(RAND, $3, $5); }
    | "nearseek" "(" Expression "," Expression "," Expression ")" { $$ = EXPR_3(NEARSEEK, $3, $5, $7); }
    | "nearseek" "(" Expression "," Expression ")"                { $$ = EXPR_2(NEARSEEK, $3, $5); }
    | "time" "(" Expression "," Expression ")"                    { $$ = EXPR_2(TIME, $3, $5); }
    | "time" "(" Expression ")"                                   { $$ = EXPR_2(TIME, $3, expression_val_new(state, 0)); }
    | "getcolor" "(" Expression "," Expression ")"                { $$ = EXPR_2(GETCOLOR, $3, $5); }
    | "getcolor" "(" Expression ")"                               { $$ = EXPR_2(GETCOLOR, expression_val_new(state, 0), $3); }
    | "pad" "(" Expression "," Expression "," Expression "," Expression "," Expression ")" { $$ = EXPR_5(PAD, $3, $5, $7, $9, $11); }
    | "buttonpress" "(" Expression ")"                            { $$ = EXPR_5(PAD, $3, expression_val_new(state, 1), expression_val_new(state, 0), expression_val_new(state, 8), expression_val_new(state, 0)); }
    | "buttonhold" "(" Expression ")"                             { $$ = EXPR_5(PAD, $3, expression_val_new(state, 2), expression_val_new(state, 0), expression_val_new(state, 8), expression_val_new(state, 0)); }
    | "buttonbuffer" "(" Expression ")"                           { $$ = EXPR_5(PAD, $3, expression_val_new(state, 3), expression_val_new(state, 0), expression_val_new(state, 8), expression_val_new(state, 0)); }
    | "buttonpress" "(" Expression "," Expression ")"             { $$ = EXPR_5(PAD, $3, expression_val_new(state, 1), expression_val_new(state, 0), expression_val_new(state, 8), $5); }
    | "buttonhold" "(" Expression "," Expression ")"              { $$ = EXPR_5(PAD, $3, expression_val_new(state, 2), expression_val_new(state, 0), expression_val_new(state, 8), $5); }
    | "buttonbuffer" "(" Expression "," Expression ")"            { $$ = EXPR_5(PAD, $3, expression_val_new(state, 3), expression_val_new(state, 0), expression_val_new(state, 8), $5); }
    | "dirpress" "(" Expression ")"                               { $$ = EXPR_5(PAD, expression_val_new(state, 0), expression_val_new(state, 0), expression_val_new(state, 1), $3, expression_val_new(state, 0)); }
    | "dirhold" "(" Expression ")"                                { $$ = EXPR_5(PAD, expression_val_new(state, 0), expression_val_new(state, 0), expression_val_new(state, 2), $3, expression_val_new(state, 0)); }
    | "dirbuffer" "(" Expression ")"                              { $$ = EXPR_5(PAD, expression_val_new(state, 0), expression_val_new(state, 0), expression_val_new(state, 3), $3, expression_val_new(state, 0)); }
    | "dirpress" "(" Expression "," Expression ")"                { $$ = EXPR_5(PAD, expression_val_new(state, 0), expression_val_new(state, 0), expression_val_new(state, 1), $3, $5); }
    | "dirhold" "(" Expression "," Expression ")"                 { $$ = EXPR_5(PAD, expression_val_new(state, 0), expression_val_new(state, 0), expression_val_new(state, 2), $3, $5); }
    | "dirbuffer" "(" Expression "," Expression ")"               { $$ = EXPR_5(PAD, expression_val_new(state, 0), expression_val_new(state, 0), expression_val_new(state, 3), $3, $5); }
    | "spd" "(" Expression "," Expression ")"                     { $$ = EXPR_2(SPD, $3, $5); }
    | "spd" "(" Expression ")"                                    { $$ = EXPR_2(SPD, expression_val_new(state, 0), $3); }
    | "sin" "(" Expression "," Expression ")"                     { $$ = EXPR_2(PSIN, $3, $5); }
    | "sin" "(" Expression ")"                                    { $$ = EXPR_2(SIN, expression_load_new(state, param_sp_new()), $3); }
    | "cos" "(" Expression ")"                                    { $$ = EXPR_2(COS, expression_load_new(state, param_sp_new()), $3); }
    | "fieldval" "(" Expression ")"                               { $$ = EXPR_2(FVAL, expression_load_new(state, param_sp_new()), $3); }
    | "fieldrow" "(" Expression "," Expression ")"                { $$ = EXPR_2(FROW, $3, $5); }
    | Address "[" Expression "]"                                  { if (state->version == 1) $$ = EXPR_4(MISC, expression_load_new(state, $1), expression_val_new(state, 5), $3, expression_val_new(state, 0));
                                                                    else if (state->version == 2) $$ = EXPR_2(ARRL, expression_load_new(state, $1), $3);
                                                                  }
    | "getval" "(" Expression "," Expression ")"                  { if (state->version == 1) $$ = EXPR_4(MISC, $3, expression_val_new(state, 5), $5, expression_val_new(state, 0)); }
    | "distance" "(" Expression "," Expression ")"                { $$ = EXPR_4(MISC, expression_load_new(state, param_null_new()), $3, $5, expression_val_new(state, 1)); }
    | "atan2" "(" Expression "," Expression ")"                   { $$ = EXPR_4(MISC, $5, $3, expression_val_new(state, 0), expression_val_new(state, 2)); }
    | "getfield" "(" Expression "," Expression ")"                { $$ = EXPR_4(MISC, $5, $3, expression_val_new(state, 0), expression_val_new(state, 3)); }

    | "atan2_mirrored" "(" Expression ")"                         { if (state->version == 1) $$ = EXPR_4(MISC, expression_load_new(state, param_null_new()), $3, expression_val_new(state, 0), expression_val_new(state, 5)); }
    | "distance" "(" Expression "," Expression "," Expression ")" { $$ = EXPR_4(MISC, $3, $5, $7, expression_val_new(state, 6)); }
    | "objectget" "(" Expression ")"                              { $$ = EXPR_4(MISC, $3, expression_val_new(state, 5), expression_val_new(state, 0), expression_val_new(state, 7)); }

    | "entitygetstate" "(" Expression "," Expression ")"          { $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 11)); }
//  | "gamefunc" "(" Expression "," Expression ")"                { if (state->version == 1) $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 12)); }
    | "getvalideventobj" "(" Expression "," Expression "," Expression ")"   { $$ = EXPR_4(MISC, $3, $5, $7, expression_val_new(state, 13)); }
    | "getvalideventobj" "(" Expression "," Expression ")"        { $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 13)); }
    | "iscolliding" "(" Expression "," Expression ")"             { if (state->version == 1) $$ = EXPR_4(MISC, $3, $5, expression_val_new(state, 0), expression_val_new(state, 14)); }
//  | "__unk2" "(" Expression "," Expression ")"                  { if (state->version == 1) $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 15)); }

    | "tryload" "(" Expression ")"                                { $$ = EXPR_2(NTRY, $3, expression_val_new(state, 3)); }
    
    | "getins" "(" Expression ")"                                 { $$ = EXPR_3(MOVC, $3, expression_val_new(state, 0), expression_val_new(state, 0x1F)); }

    /* Custom expressions. */

    | Expression "?" Expression ":" Expression  %prec QUESTION    { $$ = expression_ternary_new(state, $1, $3, $5); }
    | "offsetof" "(" Expression ")"                               {
        if ($3->type != EXPRESSION_VAL) {
            yyerror(state, "syntax error, offsetof parameter must be value expression");
            exit(2);
        }
        if ($3->value->stack != 1 || $3->value->object_link > 7 || $3->value->object_link < 0) {
            yyerror(state, "syntax error, offsetof parameter is an invalid expression");
            exit(2);
        }
        $$ = expression_val_new(state, $3->value->value.val.S << 8);
      }
    ;

Address:
      IDENTIFIER {
        thecl_variable_t* arg;
        const field_t* global;
        thecl_spawn_t* spawn;
        const field_t* field;
        const field_t* event;
        size_t anim_offset;
        field_t* objfield;
        if (var_exists(state, state->current_sub, $1)) {
            $$ = param_new('S');
            $$->stack = 1;
            $$->value.val.S = var_stack(state, state->current_sub, $1);
        } else if (arg = arg_get(state, state->current_sub, $1)) {
            $$ = param_new('S');
            $$->stack = 1;
            $$->value.val.S = arg->stack;
        } else if (global = global_get(state->version, $1)) {
            $$ = param_new('S');
            $$->stack = 2;
            $$->value.val.S = global->offset << 8;
        } else if (field = field_get($1)) {
            $$ = param_new('S');
            $$->stack = 1;
            $$->value.val.S = field->offset;
            $$->object_link = 0;
        } else if (event = event_get(state->version, $1)) {
            $$ = param_new('S');
            $$->value.val.S = event->offset << 8;
        } else if (objfield = objfield_get(state, $1)) {
            $$ = param_new('S');
            $$->stack = 1;
            $$->value.val.S = objfield->offset + 64;
            $$->object_link = 0;
        } else if (spawn = spawn_get(state, $1)) {
            $$ = param_new('S');
            $$->value.val.S = spawn->offset;
        } else if ((anim_offset = anim_get_offset(state, $1)) != 0xFFFF) {
            $$ = param_new('S');
            $$->value.val.S = anim_offset;
        } else {
            $$ = param_new('o');
            $$->value.type = 'z';
            $$->value.val.z = strdup($1);
        }
        free($1);
      }
    | IDENTIFIER "->" IDENTIFIER {
        const field_t* link = field_get($1);
        const field_t* field = field_get($3);
        if (link == NULL) {
            yyerror(state, "object link not found: %s", $1);
            free($1);
            free($3);
            return 1;
        }
        if (field == NULL) {
            yyerror(state, "object field not found: %s", $3);
            free($1);
            free($3);
            return 1;
        }
        if (link->offset >= 8 || link->offset < 0) {
            yyerror(state, "invalid object link: %s", $1);
        }
        if (field->offset > 0x3F || field->offset < 0) {
            yyerror(state, "invalid object field: %s", $1);
        }
        $$ = param_new('S');
        $$->stack = 1;
        $$->value.val.S = field->offset;
        $$->object_link = link->offset;
        free($1);
        free($3);
      }
    | NIL {
        $$ = param_new('S');
        $$->stack = 1;
        $$->value.val.S = 0;
        $$->object_link = -2;
    }
    ;

Integer:
      INTEGER {
        $$ = param_new('S');
        $$->value.val.S = $1;
      }
    | GOOL_INTEGER {
        $$ = param_new('S');
        $$->value.val.S = $1;
      }
    ;

Entry:
      ENTRY {
        $$ = param_new('S');
        $$->value.val.S = gool_to_eid($1);
        gool_pool_force_get_index(state->ecl, $$->value.val.S);
        free($1);
      }
    ;

Load_Type:
      Address
    | Integer
    | Entry
    ;

Literal_Int:
      INTEGER
    | "-" INTEGER { $$ = -$2; }
    | "+" INTEGER { $$ = +$2; }
    ;
%%

static list_t*
string_list_add(
    list_t* list,
    char* text)
{
    string_t* s = malloc(sizeof(string_t));
    s->text = text;
    list_append_new(list, s);
    return list;
}

static void
string_list_free(
    list_t* list)
{
    string_t* s;
    list_for_each(list, s) {
        free(s->text);
        free(s);
    }
    list_free_nodes(list);
    free(list);
}

static thecl_instr_t*
instr_init(
    parser_state_t* state)
{
    thecl_instr_t* instr = thecl_instr_new();
    instr->flags = state->instr_flags;
    return instr;
}

static thecl_instr_t*
instr_new(
    parser_state_t* state,
    uint8_t id,
    const char* format,
    ...)
{
    va_list ap;
    thecl_instr_t* instr = instr_init(state);
    instr->id = id;

    va_start(ap, format);
    while (*format) {
        thecl_param_t* param;
        if (*format == 'p') {
            param = va_arg(ap, thecl_param_t*);
        } else if (*format == 'S') {
            param = param_new('S');
            param->value.val.S = va_arg(ap, int32_t);
        } else {
            param = NULL;
        }
        list_append_new(&instr->params, param);
        ++instr->param_count;
        ++format;
    }
    va_end(ap);

    return instr;
}

static thecl_instr_t*
instr_new_list(
    parser_state_t* state,
    uint8_t id,
    list_t* list)
{
    thecl_instr_t* instr = instr_init(state);
    thecl_param_t* param;

    instr->id = id;
    if (list) {
        list_for_each(list, param) {
            ++instr->param_count;
            list_append_new(&instr->params, param);
        }
        list_free_nodes(list);
    }

    return instr;
}

static void
instr_add(
    parser_state_t* state,
    thecl_sub_t* sub,
    thecl_instr_t* instr)
{
    if (state->ignore_block) {
        thecl_instr_free(instr);
        return;
    }
    const expr_t* load_expr = expr_get_by_symbol(state->version, LOAD);
    if (state->block_bound) {
        if (instr->id == load_expr->id && instr->param_count == 1) {
            thecl_param_t* load_param = list_head(&instr->params);
            if (load_param->value.val.S == 0x1f && load_param->object_link == 0 && load_param->stack) {
                thecl_instr_free(instr);
                return;
            }
        }
        state->block_bound = 0;
        goto NO_OPTIM;
    }
    const expr_t* expr = expr_get_by_symbol(state->version, PLOAD);
    const expr_t* bra_expr = expr_get_by_symbol(state->version, GOTO);
    const expr_t* beqz_expr = expr_get_by_symbol(state->version, UNLESS);
    const expr_t* bnez_expr = expr_get_by_symbol(state->version, IF);
    /* push optimization */
    if (instr->id == load_expr->id) {
        if (instr->param_count == 1) {
            thecl_param_t* load_param = list_head(&instr->params);
            if (load_param->value.val.S == 0x1f && load_param->object_link == 0 && load_param->stack) {
                thecl_instr_free(instr);
                return;
            }
        }
        thecl_instr_t* last_ins = list_tail(&sub->instrs);
        if (last_ins != NULL) {
            thecl_label_t* tmp_label;
            list_for_each(&sub->labels, tmp_label) {
                if (tmp_label->offset == sub->offset)
                    goto NO_OPTIM;
            }

            while (last_ins->id == load_expr->id && last_ins->param_count < 2 && instr->param_count > 0) {
                ++last_ins->param_count;
                list_append_new(&last_ins->params, list_head(&instr->params));
                list_del(&instr->params, instr->params.head);
                if (--instr->param_count == 0) {
                    thecl_instr_free(instr);
                    instr->offset = sub->offset;
                    return;
                }
            }
        }
    }
    /* pointer push optimization */
    else if (instr->id == expr->id) {
        thecl_instr_t* last_ins = list_tail(&sub->instrs);
        if (last_ins != NULL) {
            thecl_label_t* tmp_label;
            list_for_each(&sub->labels, tmp_label) {
                if (tmp_label->offset == sub->offset)
                    goto NO_OPTIM;
            }

            while (last_ins->id == expr->id && last_ins->param_count < 2 && instr->param_count > 0) {
                ++last_ins->param_count;
                list_append_new(&last_ins->params, list_head(&instr->params));
                list_del(&instr->params, instr->params.head);
                if (--instr->param_count == 0) {
                    thecl_instr_free(instr);
                    instr->offset = sub->offset;
                    return;
                }
            }
        }
    }
    /* branch optimization */
    else if ((instr->id == beqz_expr->id || instr->id == bnez_expr->id) && instr->param_count == 5 &&
        ((state->version != 1) ||
        ((state->version == 1) &&
        ((thecl_param_t*)instr->params.tail->data)->value.val.S == 0 &&
        ((thecl_param_t*)instr->params.tail->prev->data)->value.val.S != 0))) {
        thecl_instr_t* last_ins = list_tail(&sub->instrs);
        if (last_ins != NULL) {
            thecl_label_t* tmp_label;
            list_for_each(&sub->labels, tmp_label) {
                if (tmp_label->offset == sub->offset)
                    goto NO_OPTIM;
            }

            expr = expr_get_by_symbol(state->version, NOT);
            if (last_ins->id == expr->id && last_ins->param_count == 2) {
                thecl_param_t* param = list_head(&last_ins->params);
                if (param->value.val.S != 0x1F || !param->stack || param->object_link != 0)
                    goto NO_OPTIM;

                if (state->version == 1) {
                    param = instr->params.tail->prev->data;
                    param->value.val.S = param->value.val.S == 1 ? 2 : 1;
                } else if (state->version == 2) {
                    instr->id = instr->id == beqz_expr->id ? bnez_expr->id : beqz_expr->id;
                }

                param = list_tail(&last_ins->params);

                if (param->value.val.S <= 0x3F && param->value.val.S >= 0 && param->stack == 1 && param->object_link == 0) {
                    thecl_param_t* reg_param = instr->params.head->next->next->data;
                    reg_param->value.val.S = param->value.val.S;
                    list_del(&sub->instrs, sub->instrs.tail);
                    thecl_instr_free(last_ins);
                    --sub->offset;
                }
                else {
                    expr = expr_get_by_symbol(state->version, LOAD);
                    last_ins->id = expr->id;
                    param = list_head(&last_ins->params);
                    list_del(&last_ins->params, last_ins->params.head);
                    param_free(param);
                }
            } else { /* optimize if literal conditions */
                expr = expr_get_by_symbol(state->version, LOAD);
                if (last_ins->id == expr->id && last_ins->param_count > 0) {
                    thecl_param_t* param = list_tail(&last_ins->params);
                    if (!param->stack) {
                        list_del(&last_ins->params, last_ins->params.tail);
                        thecl_param_t* branch_type_param = (thecl_param_t*)instr->params.tail->prev->data;
                        int branch_type = state->version == 2 ? instr->id - bra_expr->id - 1 : branch_type_param->value.val.S - 1;
                        if (--last_ins->param_count == 0) {
                            list_del(&sub->instrs, sub->instrs.tail);
                            thecl_instr_free(last_ins);
                            --sub->offset;
                        }
                        if (!!param->value.val.S == branch_type) { /* will never branch */
                            thecl_instr_free(instr);
                            param_free(param);
                            return;
                        } else { /* will always branch */
                            param_free(param);
                            branch_type_param->value.val.S = 0;
                            branch_type_param = (thecl_param_t*)instr->params.tail->prev->prev->data;
                            branch_type_param->value.val.S = 0x25;
                            instr->id = bra_expr->id;
                        }
                    }
                }
            }
        }
    }
    NO_OPTIM:;
    list_append_new(&sub->instrs, instr);
    instr->offset = sub->offset;
    ++sub->offset;
}

static void
instr_del(
    parser_state_t* state,
    thecl_sub_t* sub,
    thecl_instr_t* instr)
{
    int found = 0;
    list_node_t* node, *next;
    list_for_each_node_safe(&sub->instrs, node, next) {
        if (node->data == instr) {
            list_del(&sub->instrs, node);
            thecl_label_t* label;
            list_for_each(&sub->labels, label) {
                if (label->offset > instr->offset) {
                    --label->offset;
                }
            }
            --sub->offset;
            found = 1;
        }
        else if (found) {
            thecl_instr_t* this_inst = node->data;
            if (this_inst->offset > instr->offset)
                --this_inst->offset;
        }
    }
}

static void
instr_prepend(
    thecl_sub_t* sub,
    thecl_instr_t* instr)
{
    list_prepend_new(&sub->instrs, instr);
    instr->offset = sub->start_offset;
    ++sub->offset;

    thecl_instr_t* tmp_instr;
    list_for_each(&sub->instrs, tmp_instr) {
        ++tmp_instr->offset;
    }

    thecl_label_t* tmp_label;
    list_for_each(&sub->labels, tmp_label) {
        ++tmp_label->offset;
    }
}

static thecl_instr_t*
instr_copy(thecl_instr_t* instr) {
    thecl_instr_t* new_instr = malloc(sizeof(thecl_instr_t));
    memcpy(new_instr, instr, sizeof(thecl_instr_t));
    if (instr->string)
        new_instr->string = strdup(instr->string);
    else
        new_instr->string = NULL;
    list_init(&new_instr->params);
    thecl_param_t* param;
    list_for_each(&instr->params, param) {
        thecl_param_t* new_param = param_copy(param);
        list_append_new(&new_instr->params, new_param);
    }
    return new_instr;
}

static void instr_create_inline_call(
    parser_state_t* state,
    thecl_sub_t* sub,
    list_t* params_org
) {
    /*   INLINE SUBS: how do they work?
     * Generally the main concept is that the sub that's inline gets parsed normally
     * (with some exceptions, like the return statement jumping to the end of the sub instead)
     * and then, when called, all insructions from the inline sub get copied into the caller,
     * with some instr parameters being replaced by the values provided as inline sub parameters,
     * stack variables being recreated, labels being adjusted etc.
     * 
     * So, how do parameters actually get passed? This part is a bit tricky, since it depends from
     * what the parameter actually is, and what the inline sub does with the parameter inside:
     * - For example, if we pass the RAND variable as a parameter, it needs to be copied into
     * a new variable, in order to keep the same value when being read multiple times.
     * - On the other hand, when passing a static value, creating a variable to store it in
     * would be a waste of time, and as such all occurences of the parameter are replaced
     * by the static value directly instead.
     * - When the parameter is an expression, it needs to be stored in a variable too,
     * since 1. evaluating the expression every time would be stupid, 2. expression does
     * not neccesairly have to result in the same value every time (thanks to variables like RAND).
     * - But, there still is another thing to keep in mind - the parameter could be written to
     * inside of the inline sub! In this case, creating a variable to store the parameter in
     * is absolutely necessary. */

    /* An inline sub can't call itself for obvious reasons. */
    if (strcmp(sub->name, state->current_sub->name) == 0) {
        yyerror(state, "an inline sub is not allowed to call itself");
        return;
    }
    char buf[256];

    /* A new variable is created in order to know whether the list was passed or created here later. */
    list_t* params = params_org == NULL ? list_new() : params_org;

    /* Verify parameter count before doing anything else. */
    size_t i = list_count(params);
    thecl_param_t* param;
    if (i > sub->arg_count) {
        yyerror(state, "too many paramters for inline sub \"%s\"", sub->name);
        list_for_each(params, param)
            param_free(param);
        return;
    }
    if (i < sub->arg_count) {
        yyerror(state, "not enough parameters for inline sub \"%s\"", sub->name);
        list_for_each(params, param)
            param_free(param);
        return;
    }

    /* After making sure that everything is correct, we can now create the inline sub scope. */
    scope_begin(state);

    /* This string will be prepended to label names/var names etc. */
    char name[256];
    snprintf(name, 256, "%s_%d_%d_%s_", state->current_sub->name, yylloc.first_line, yylloc.first_column, sub->name);

    /* It's time to setup the param replacements.
     * As mentioned earlier, it is necessary to create vars if the param ever
     * gets written to, or the passed parameter is an expression.
     * We will use a param_replace array to replace all argument variable references from the code of copied inline sub. */
    thecl_param_t** param_replace = calloc(sub->arg_count, sizeof(thecl_param_t*));
    thecl_variable_t* arg;
    i = 0;

    list_for_each(params, param) { /* It has alredy been verified that param amount is correct. */
        arg = sub->args[i];

        if (param->value.val.S >= 0 && (arg->is_written || param->is_expression_param || (param->stack && param->object_link == -1))) {
            /* Non-static param value or the param is written to, need to create var. */
            strcpy(buf, name);
            strcat(buf, arg->name);
            thecl_param_t* new_param = param_new(param->type);
            new_param->stack = 1;

            thecl_variable_t* var = var_create_assign(state, state->current_sub, buf, expression_load_new(state, param_copy(param)));
            new_param->value.val.S = var->stack;
            param_replace[i] = new_param;
        } else {
            param_replace[i] = param_copy(param);
        }
        ++i;
    }

    /* Create non-param variables that the inline sub uses.. */
    thecl_variable_t** stack_replace = malloc(sizeof(thecl_variable_t*) * sub->var_count);
    for (i = 0; i < sub->var_count; ++i) {
        thecl_variable_t* var = sub->vars[i];
        snprintf(buf, 256, "%s%s", name, var->name);
        thecl_variable_t* var_new = var_create(state, state->current_sub, buf, false);
        stack_replace[i] = var_new;
        state->current_sub->vars = realloc(state->current_sub->vars, --state->current_sub->var_count * sizeof(thecl_variable_t*));
    }

    /* Create labels that the inline sub uses (with changed offsets) */
    thecl_label_t* label;
    list_for_each(&sub->labels, label) {
        snprintf(buf, 256, "%s%s", name, label->name);
        thecl_label_t* new_label = malloc(sizeof(thecl_label_t) + strlen(buf) + 1);
        new_label->offset = label->offset + state->current_sub->offset;
        strcpy(new_label->name, buf);
        list_append_new(&state->current_sub->labels, new_label);
    }

    /* And finally, copy the instructions. */

    thecl_instr_t* instr;
    list_for_each(&sub->instrs, instr) {
        thecl_instr_t* new_instr = instr_copy(instr);

        list_node_t* param_node;
        list_for_each_node(&new_instr->params, param_node) {
            /* Still reusing the same param variable as earlier. */
            param = (thecl_param_t*)param_node->data;
            if (param->stack && param->object_link == -1) {
                if (param->value.type == 'S') {
                    if (param->value.val.S < 0) {
                        /* Parameter. */
                        param_node->data = param_copy(param_replace[sub->arg_count + param->value.val.S]);
                        param_free(param);
                    } else if (param->value.val.S >= 3) {
                        /* Regular stack variable, needs adjusting the offset. */
                        param->value.val.S = stack_replace[param->value.val.S - 3]->stack;
                    }
                }
            } else if (param->stack && param->object_link == 0 && param->value.val.S == 0x1F) {
                thecl_instr_t* last_ins = list_tail(&state->current_sub->instrs);
                const expr_t* last_expr = last_ins ? expr_get_by_id(state->version, last_ins->id) : NULL;
                if (last_expr && last_expr->allow_optim) {
                    thecl_param_t* p1 = last_ins->params.head->data;
                    thecl_param_t* p2 = last_ins->params.head->next->data;
                    if (p1->stack || p2->stack) continue;
                    int val1 = p1->value.val.S;
                    int val2 = p2->value.val.S;
                    param->value.val.S = math_preprocess(state, last_expr->symbol, val1, val2);
                    param->stack = 0;
                    param->object_link = -1;
                    instr_del(state, state->current_sub, last_ins);
                    thecl_instr_free(last_ins);
                }
            } else if (param->type == 'o' && label_find(sub, param->value.val.z)) {
                /* We also have to make sure that all jumps are correct. */
                snprintf(buf, 256, "%s%s", name, param->value.val.z);
                free(param->value.val.z);
                param->value.val.z = strdup(buf);
            }
        }
        instr_add(state, state->current_sub, new_instr);
    }

    scope_finish(state, true);

    /* We have to mark variables that were marked as unused in the inline sub
     * as unused in the current sub as well. */
    for (size_t v=0; v<sub->var_count; ++v) {
        stack_replace[v]->is_unused = sub->vars[v]->is_unused;
    }

    /* Free stuff. */
    /* Still the same variables used here */
    i = 0;
    list_for_each(params, param) {
        param_free(param);
        param_free(param_replace[i++]);
    }
    /* Only free this list if it was created here.
     * It's empty, so no need to free nodes. */
    if (params_org == NULL)
        free(params);
    free(param_replace);
    free(stack_replace);
}

static bool
instr_create_call(
    parser_state_t *state,
    uint8_t type,
    char *name,
    list_t *params)
{
    size_t param_count = params ? list_count(params) : 0;
    /* First, check if the called sub is inline. */
    thecl_sub_t* sub;
    list_for_each(&state->main_ecl->subs, sub) {
        if (sub->is_inline && !strcmp(sub->name, name) && sub->arg_count == param_count) {
            instr_create_inline_call(state, sub, params);
            free(name);
            return true;
        }
    }

    /* Instr name */
    thecl_param_t *name_param = param_new('z');
    name_param->value.type = 'z';
    name_param->value.val.z = name;

    int argc = 0;

    /* Add parameter casts */
    if (params != NULL) {
        list_node_t* node_expr = state->expressions.tail;
        thecl_param_t *param;
        list_for_each(params, param) {
            if (param->is_expression_param) {
                expression_t* current_expr = (expression_t*)node_expr->data;
                list_node_t* last_node = node_expr;
                node_expr = node_expr->prev;

                if (current_expr->type == EXPRESSION_VAL) {
                    const expr_t* expr = expr_get_by_id(state->version, current_expr->id);
                    if (expr->symbol == LOAD) {
                        param_free(param);
                        param = current_expr->value;
                        param->is_expression_param = 0;
                        list_del(&state->expressions, last_node);
                        expression_free(current_expr);
                    }
                } else if (current_expr->type == EXPRESSION_OP) {
                    expression_output(state, current_expr);
                    list_del(&state->expressions, last_node);
                    expression_free(current_expr);
                }
            }

            instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, LOAD)->id, "p", param));

            ++argc;
        }
    }

    instr_add(state, state->current_sub, instr_new(state, type, "pSS", name_param, 38, argc));
    return false;
}

static expression_t*
expression_load_new(
    const parser_state_t* state,
    thecl_param_t* value)
{
    expression_t* ret = malloc(sizeof(expression_t));
    const expr_t* expr;
    if (value->stack == 2) {
        expr = expr_get_by_symbol(state->version, GLOAD);
        ret->type = EXPRESSION_GLOBAL;
    }
    else {
        expr = expr_get_by_symbol(state->version, LOAD);
        ret->type = EXPRESSION_VAL;
    }
    ret->id = expr->id;
    ret->value = value;
    return ret;
}

static expression_t*
expression_val_new(
    const parser_state_t* state,
    int value)
{
    return expression_load_new(state, param_val_new(value));
}

static expression_t*
expression_pointer_new(
    const parser_state_t* state,
    thecl_param_t* value)
{
    expression_t* ret = malloc(sizeof(expression_t));
    const expr_t* expr = expr_get_by_symbol(state->version, PLOAD);
    ret->type = EXPRESSION_VAL;
    ret->id = expr->id;
    ret->value = value;
    return ret;
}

static expression_t*
expression_operation_new(
    const parser_state_t* state,
    const int symbol,
    expression_t** operands)
{
    const expr_t* expr = expr_get_by_symbol(state->version, symbol);

    expression_t* ret = malloc(sizeof(expression_t));
    ret->type = EXPRESSION_OP;
    ret->id = expr->id;
    ret->value = NULL;
    list_init(&ret->children);
    for (size_t o = 0; o < expr->stack_arity; ++o) {
        if (!operands[o] && (!expr->has_double_param || (expr->has_double_param && o != expr->stack_arity - 1))) {
            yyerror(state, "not enough params for operation %d", symbol);
        }
        else if (!operands[o] && expr->has_double_param && o == expr->stack_arity - 1)
            break;
        list_append_new(&ret->children, operands[o]);
    }

    expression_optimize(state, ret);

    return ret;
}

static expression_t*
expression_ternary_new(
    const parser_state_t* state,
    expression_t* cond,
    expression_t* val1,
    expression_t* val2
 ) {
    expression_t* expr = malloc(sizeof(expression_t));
    expr->type = EXPRESSION_TERNARY;
    list_init(&expr->children);
    list_append_new(&expr->children, cond);
    list_append_new(&expr->children, val1);
    list_append_new(&expr->children, val2);
    return expr;
}

static expression_t *
expression_copy(
    expression_t *expr)
{
    expression_t *copy = malloc(sizeof(expression_t));
    memcpy(copy, expr, sizeof(expression_t));
    expression_t* child_expr;
    list_init(&copy->children);
    if (expr->type == EXPRESSION_OP || expr->type == EXPRESSION_TERNARY) {
        list_for_each(&expr->children, child_expr)
            list_append_new(&copy->children, expression_copy(child_expr));
    } else if (expr->type == EXPRESSION_VAL) {
        thecl_param_t *param = malloc(sizeof(thecl_param_t));
        memcpy(param, expr->value, sizeof(thecl_param_t));
        copy->value = param;
    }
    return copy;
}

static void
expression_create_goto(
    parser_state_t *state,
    int type,
    char *labelstr,
    expression_t* cond)
{
    expression_create_goto_pop(state, type, labelstr, cond, 0);
}

static void
expression_create_goto_pop(
    parser_state_t *state,
    int type,
    char *labelstr,
    expression_t* cond,
    int pop)
{
    const expr_t* expr = expr_get_by_symbol(state->version, type);
    thecl_param_t *p1 = param_new('o');
    p1->value.type = 'z';
    p1->value.val.z = strdup(labelstr);
    thecl_param_t *pcond;
    if (type == GOTO || cond == NULL) {
        pcond = param_new('S');
        pcond->stack = 1;
        pcond->object_link = 0;
        pcond->value.val.S = field_get("misc")->offset;
    }
    else {
        if (cond->type != EXPRESSION_VAL || cond->value->stack != 1 || cond->value->object_link != 0 || cond->value->value.val.S < 0 || cond->value->value.val.S > 0x3F) {
            expression_output(state, cond);
            pcond = param_sp_new();
        }
        else {
            pcond = cond->value;
        }
    }
    instr_add(state, state->current_sub, instr_new(state, expr->id, "pSpSS", p1, pop, pcond, state->version == 1 ? (type == GOTO ? 0 : (type == IF ? 1 : (type == UNLESS ? 2 : 3))) : 0, 0));
}

static void
expression_output(
    parser_state_t* state,
    expression_t* expr)
{
    if (expr_get_by_id(state->version, expr->id)->symbol < 0) {
        yyerror(state, "error, cannot output non-compileable expression %d", expr->id);
        exit(2);
    }
    
    if (expr->type == EXPRESSION_VAL) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "p", expr->value));
    } else if (expr->type == EXPRESSION_GLOBAL) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "S", expr->value->value.val.S));
    } else if (expr->type == EXPRESSION_OP) {
        const expr_t* expression = expr_get_by_id(state->version, expr->id);
        expression_t* child_expr;
        list_t* param_list = list_new();
        int c = 0, lc = list_count(&expr->children);
        
        list_for_each(&expr->children, child_expr) {
            ++c;
            if (child_expr->type == EXPRESSION_VAL && (!expression->has_double_param || (expression->has_double_param && lc <= 2) || (expression->has_double_param && lc > 2 && c == 1))) {
                list_append_new(param_list, child_expr->value);
            }
            else {
                expression_output(state, child_expr);
                list_append_new(param_list, param_sp_new());
            }
        }
        
        if (expression->has_double_param && lc == 3) {
            param_free(param_list->tail->data);
            param_free(param_list->tail->prev->data);
            list_del(param_list, param_list->tail);
            list_del(param_list, param_list->tail);
            list_append_new(param_list, param_sp2_new());
        }
        
        instr_add(state, state->current_sub, instr_new_list(state, expr->id, param_list));
    } else if (expr->type == EXPRESSION_TERNARY) {
        char labelstr_unless[256];
        char labelstr_end[256];

        snprintf(labelstr_unless, 256, "ternary_unless_%d_%d", yylloc.first_line, yylloc.first_column);
        snprintf(labelstr_end, 256, "ternary_end_%d_%d", yylloc.first_line, yylloc.first_column);

        int i = 0;
        expression_t* child_expr;
        list_for_each(&expr->children, child_expr) {
            if (i == 0) {
                expression_create_goto(state, UNLESS, labelstr_unless, child_expr);
            } else if (i == 1) {
                expression_output(state, child_expr);
                expression_create_goto(state, GOTO, labelstr_end, NULL);
                label_create(state, labelstr_unless);
            } else {
                expression_output(state, child_expr);
                label_create(state, labelstr_end);
            }
            ++i;
        }
    }
}

static void
expression_optimize(
    parser_state_t* state,
    expression_t* expression)
{
    if (expression->type != EXPRESSION_OP) return;

    int child_cnt = 0;
    expression_t* child_expr_1;
    expression_t* child_expr_2;
    expression_t* child_expr;
    list_for_each(&expression->children, child_expr) {
        if (child_expr->type == EXPRESSION_OP) {
            expression_optimize(state, child_expr);
        }

        if (child_cnt == 0) {
            child_expr_1 = child_expr;
        } else if (child_cnt == 1) {
            child_expr_2 = child_expr;
        }
        ++child_cnt;
    }

    const expr_t* tmp_expr = expr_get_by_id(state->version, expression->id);

    /* TODO: handle some single-child expressions, such as sin or cos */
    if (tmp_expr->stack_arity != 2 || !tmp_expr->allow_optim) return;

    if (   !tmp_expr->is_unary && (
           child_expr_1->type != EXPRESSION_VAL
        || child_expr_2->type != EXPRESSION_VAL
        || child_expr_1->value->stack /* Variables are not acceptable, obviously. */
        || child_expr_2->value->stack
        || child_expr_1->value->type != 'S'
        || child_expr_2->value->type != 'S'
      ) || tmp_expr->is_unary && (
           child_expr_2->type != EXPRESSION_VAL
        || child_expr_2->value->stack
        || child_expr_1->value->value.val.S != 0x1F
        || child_expr_1->value->object_link != 0
        || !child_expr_1->value->stack
        || child_expr_1->value->type != 'S'
        || child_expr_2->value->type != 'S'
      )
    ) return;

    thecl_param_t* param = param_new('S');

    int val1 = child_expr_1->value->value.val.S;
    int val2 = child_expr_2->value->value.val.S;

    param->value.val.S = math_preprocess(state, tmp_expr->symbol, val1, val2);

    expression->value = param;
    expression->type = EXPRESSION_VAL;
    tmp_expr = expr_get_by_symbol(state->version, LOAD);
    expression->id = tmp_expr->id;

    param_free(child_expr_1->value);
    param_free(child_expr_2->value);
    expression_free(child_expr_1);
    expression_free(child_expr_2);
    list_free_nodes(&expression->children);
}

#define PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062
static int
math_preprocess(
    parser_state_t* state,
    int symbol,
    int val1,
    int val2)
{
    switch(symbol) {
        case ADD:      return val1 + val2;
        case SUBTRACT: return val1 - val2;
        case MULTIPLY: return val1 * val2;
        case DIVIDE:   return val1 / val2;
        case MODULO:   return val1 % val2;
        case EQUAL:    return val1 == val2;
        case INEQUAL:  return val1 != val2;
        case LT:       return val1 < val2;
        case LTEQ:     return val1 <= val2;
        case GT:       return val1 > val2;
        case GTEQ:     return val1 >= val2;
        case OR:       return val1 || val2;
        case AND:      return val1 && val2;
        case XOR:      return val1 ^ val2;
        case B_OR:     return val1 | val2;
        case B_AND:    return val1 & val2;
        case LSHIFT:   return val2 >= 0 ? val1 << val2 : val1 >> -val2;
        case TEST:     return (val1 & val2) == val2;
        case NOT:      return !val2;
        case B_NOT:    return ~val2;
        case ABS:      return val2 < 0 ? -val2 : val2;
        case PSIN:     return (sin((val1 * PI) / val2 - PI / 2) + 1.0) * val2 / 2.0; /* casts to int on return */
        case SIN:      return lround(sin(val2 / 2048.0 * PI) * 0x1000); /* casts to int on return */
        case COS:      return lround(cos(val2 / 2048.0 * PI) * 0x1000); /* casts to int on return */
        default:
            /* Since the cases above cover all existing expressions there is no possibility of this ever hapenning.
               Just putting this error message in case someone adds new expressions and forgets about handling them here... */
            yyerror(state, "Math preprocessing error!");
    }
}
#undef PI

static void
state_begin(
    parser_state_t* state,
    char* name)
{
    thecl_state_t* iter_state;
    list_for_each(&state->main_ecl->states, iter_state) {
        if(!strcmp(name, iter_state->name)) {
            yyerror(state, "duplicate state: %s", name);
            g_was_error = true;
            break;
        }
    }

    thecl_state_t* gstate = malloc(sizeof(thecl_state_t));

    gstate->name = strdup(name);
    gstate->code = NULL;
    gstate->trans = NULL;
    gstate->event = NULL;
    gstate->exe_eid = state->ecl->eid;
    gstate->stateflag = 1;
    gstate->statusc = 2;
    gstate->trans_args = false;
    gstate->index = state->state_count++;
    list_append_new(&state->main_ecl->states, gstate);

    state->current_state = gstate;
}

static void
state_finish(
    parser_state_t* state)
{
    state->current_state = NULL;
}

static void
sub_begin(
    parser_state_t* state,
    char* name)
{
    scope_begin(state);

    thecl_sub_t* sub = malloc(sizeof(thecl_sub_t));

    sub->name = strdup(name);
    list_init(&sub->instrs);
    sub->stack = 3;
    sub->var_count = 0;
    sub->arg_count = 0;
    sub->vars = NULL;
    sub->args = NULL;
    sub->start_offset = state->ecl->ins_offset;
    sub->offset = 0;
    sub->instr_data = NULL;
    sub->deleted = false;
    sub->is_trans = false;
    sub->has_once = false;
    sub->has_nofirst = false;
    list_init(&sub->labels);

    list_append_new(&state->ecl->subs, sub);

    state->current_sub = sub;
}

static void
sub_finish(
    parser_state_t* state)
{
    thecl_sub_t* iter_sub;
    list_for_each(&state->ecl->subs, iter_sub) {
        if(state->current_sub != iter_sub && !strcmp(state->current_sub->name, iter_sub->name) && iter_sub->arg_count == state->current_sub->arg_count && !iter_sub->forward_declaration) {
            yyerror(state, "duplicate sub: %s", state->current_sub->name);
            g_was_error = true;
            break;
        }
    }

    if (state->current_sub->is_inline) {
        thecl_instr_t* last_ins = list_tail(&state->current_sub->instrs);
        const expr_t* tmp = expr_get_by_symbol(state->version, GOTO);
        if (last_ins != NULL && last_ins->id == tmp->id && ((state->version == 1 && ((thecl_param_t*)last_ins->params.tail->data)->value.val.S == 2) || state->version != 1)) {
            thecl_param_t* label_param = list_head(&last_ins->params);
            if (strcmp(label_param->value.val.z, "inline_end") == 0) {
                /* Remove useless goto. */
                list_del(&state->current_sub->instrs, state->current_sub->instrs.tail);
                --state->current_sub->offset;
                thecl_instr_free(last_ins);
            }
        }
        label_create(state, "inline_end");
        scope_finish(state, false);
    }
    else {
        int bb = state->block_bound;
        char buf[512];
        snprintf(buf, 512, "@%s_sub_end", state->current_sub->name);
        label_create(state, buf);
        scope_finish(state, true);
        thecl_instr_t* last_ins = state->version == 1 ? NULL : list_tail(&state->current_sub->instrs);
        const expr_t* expr = expr_get_by_symbol(state->version, RETURN);
        if (bb || last_ins == NULL || last_ins->id != expr->id) {
            instr_add(state, state->current_sub, instr_new(state, expr->id, "SSSSS", 0, 0, 0x25, 0, 2));
        }
        state->ecl->ins_offset += state->current_sub->offset;
    }

    state->current_sub = NULL;
}

static void
scope_begin(
    parser_state_t* state
) {
    ++state->scope_cnt;
    state->scope_stack = realloc(state->scope_stack, sizeof(int)*state->scope_cnt);
    state->scope_stack[state->scope_cnt - 1] = state->scope_id++;
    state->block_bound = 1;
}

static void
scope_finish(
    parser_state_t* state,
    bool pop_vars
) {
    --state->scope_cnt;

    /* pop GOOL stack variables */
    int pop = 0;
    thecl_variable_t* var;
    for (int v=0; v < state->current_sub->var_count; ++v)
        if (state->current_sub->vars[v]->scope == state->scope_stack[state->scope_cnt]) {
            state->current_sub->vars[v]->is_unused = true;
            ++pop;
        }
    if (pop > 0 && pop_vars) {
        const expr_t* expr = expr_get_by_symbol(state->version, GOTO);
        instr_add(state, state->current_sub, instr_new(state, expr->id, "SSSSS", 0, pop, 0x25, 0, 0));
    }

    state->scope_stack = realloc(state->scope_stack, sizeof(int)*state->scope_cnt);
    state->block_bound = 1;
}

static bool
var_stack_used(
    parser_state_t* state,
    thecl_sub_t* sub,
    int stack
) {
    for (size_t v=0; v<sub->var_count; ++v) {
        if (sub->vars[v]->stack == stack && !sub->vars[v]->is_unused)
            return true;
    }
    return false;
}

static int
var_get_new_stack(
    parser_state_t* state,
    thecl_sub_t* sub
) {
    int stack = 3;
    while(1) {
        if (!var_stack_used(state, sub, stack))
            return stack;
        ++stack;
    }
}

static expr_macro_t*
macro_create(
    parser_state_t* state,
    const char* name,
    expression_t* expr)
{
    expr_macro_t* macro = malloc(sizeof(expr_macro_t));
    macro->name = strdup(name);
    macro->expr = expr;
    list_append_new(&state->expr_macros, macro);
    return macro;
}

static thecl_variable_t*
arg_create(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name)
{
    if (arg_exists(state, sub, name)) {
        yyerror(state, "duplicate argument: %s", name);
    }

    thecl_variable_t* arg = malloc(sizeof(thecl_variable_t));
    arg->name = strdup(name);
    arg->is_unused = false;
    arg->is_written = false;

    ++sub->arg_count;
    sub->args = realloc(sub->args, sub->arg_count * sizeof(thecl_variable_t*));
    sub->args[sub->arg_count - 1] = arg;

    for (int i = 0; i < sub->arg_count; ++i) {
        sub->args[i]->stack = -sub->arg_count + i;
    }

    return arg;
}

static field_t*
objfield_create(
    parser_state_t* state,
    const char* name)
{
    if (objfield_get(state, name) != NULL) {
        yyerror(state, "redeclaration of object variable: %s", name);
    }

    field_t* var = malloc(sizeof(field_t));
    var->name = strdup(name);
    var->offset = state->main_ecl->var_count++;

    state->main_ecl->vars = realloc(state->main_ecl->vars, state->main_ecl->var_count * sizeof(field_t*));
    state->main_ecl->vars[state->main_ecl->var_count - 1] = var;

    return var;
}

static void
objfield_delete(
    parser_state_t* state,
    const char* name)
{
    field_t* field = objfield_get(state, name);
    if (field == NULL) {
        yyerror(state, "nonexistant object variable: %s", name);
    }

    free(field);
    state->main_ecl->vars = realloc(state->main_ecl->vars, --state->main_ecl->var_count * sizeof(field_t*));
}

static thecl_variable_t*
var_create(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name,
    bool push)
{
    if (var_exists(state, sub, name)) {
        yyerror(state, "redeclaration of variable: %s", name);
    }

    thecl_variable_t* var = malloc(sizeof(thecl_variable_t));
    var->name = strdup(name);
    var->stack = var_get_new_stack(state, sub);
    var->is_unused = false;
    var->is_written = false;
    var->scope = state->scope_stack[state->scope_cnt - 1];

    ++sub->var_count;
    sub->vars = realloc(sub->vars, sub->var_count * sizeof(thecl_variable_t*));
    sub->vars[sub->var_count - 1] = var;

    if (var->stack == sub->stack) /* Only increment the stack if the variable uses a new offset. */
        ++sub->stack;

    if (push) {
        const expr_t* expr = expr_get_by_symbol(state->version, LOAD);
        instr_add(state, sub, instr_new(state, expr->id, "S", 0));
    }

    return var;
}

static thecl_variable_t*
var_create_assign(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name,
    expression_t* expr)
{
    thecl_variable_t* var = var_create(state, sub, name, false);
    var->is_written = true;

    thecl_param_t* param;
    if (expr->type == EXPRESSION_VAL) {
        param = expr->value;
    } else {
        expression_output(state, expr);
        param = NULL;
    }
    expression_free(expr);

    if (param != NULL) { /* if param is NULL, then an expression was pushed to stack, which is enough */
        const expr_t* expr_load = expr_get_by_symbol(state->version, LOAD);
        instr_add(state, state->current_sub, instr_new(state, expr_load->id, "p", param));
    }

    return var;
}

static bool
var_accessible(
    parser_state_t* state,
    thecl_variable_t* var
) {
    for (int scope_state=0; scope_state<state->scope_cnt; ++scope_state) {
        if (state->scope_stack[scope_state] == var->scope)
            return true;
    }
    return false;
}

static thecl_variable_t*
arg_get(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name
) {
    if (!sub) return NULL;

    for (size_t i = 0; i < sub->arg_count; ++i) {
        if (!strcmp(name, sub->args[i]->name))
            return sub->args[i];
    }
    return NULL;
}

static field_t*
objfield_get(
    parser_state_t* state,
    const char* name
) {
    for (size_t i = 0; i < state->main_ecl->var_count; ++i) {
        if (!strcmp(name, state->main_ecl->vars[i]->name))
            return state->main_ecl->vars[i];
    }
    return NULL;
}

static thecl_variable_t*
var_get(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name
) {
    for (size_t i = 0; i < sub->var_count; ++i) {
        if (strcmp(name, sub->vars[i]->name) == 0 && var_accessible(state, sub->vars[i]))
            return sub->vars[i];
    }
    return NULL;
}

static int
var_stack(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name)
{
    thecl_variable_t* var = var_get(state, sub, name);
    if (var != NULL)
        return var->stack;

    yyerror(state, "variable not found: %s", name);
    return 0;
}

static int
arg_exists(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name)
{
    if (sub == NULL)
        yyerror(state, "attempted to find argument outside sub scope: %s", name);

    return arg_get(state, sub, name) != NULL;
}

static int
var_exists(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name)
{
    if (sub == NULL) return 0; /* we are outside of sub scope, no point in searching for variables */

    return var_get(state, sub, name) != NULL;
}

static void
var_shorthand_assign(
    parser_state_t* state,
    thecl_param_t* param,
    expression_t* expr_assign,
    int EXPR)
{
    /* Can't use the same param twice, so a copy is created. */
    thecl_param_t* param_clone = malloc(sizeof(thecl_param_t));
    memcpy(param_clone, param, sizeof(thecl_param_t));

    expression_t* expr_load = expression_load_new(state, param_clone);
    expression_t* expr_main = EXPR_2(EXPR, expr_load, expr_assign);
    expression_output(state, expr_main);
    expression_free(expr_main);
    /* No need to free expr_load or expr, since they both got freed as children of expr_main. */

    if (param->stack != 2) {
        const expr_t* expr = expr_get_by_symbol(state->version, ASSIGN);

        instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", param, param_sp_new()));

        if (param->object_link == -1 && param->value.val.S >= 3) {
            state->current_sub->vars[param->value.val.S - 3]->is_written = true;
        }
        else if (param->object_link == -1 && param->value.val.S < 0) {
            state->current_sub->args[-param->value.val.S - 1]->is_written = true;
        }
    } else { /* WGL */
        const expr_t* expr = expr_get_by_symbol(state->version, GASSIGN);

        thecl_instr_t* last_ins = state->current_sub->instrs.tail->prev->data;

        thecl_param_t* last_param = param_copy(list_tail(&last_ins->params));

        instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", last_param, param_sp_new()));
    }
}

static void
label_create(
    parser_state_t* state,
    char* name)
{
    thecl_label_t* label = malloc(sizeof(thecl_label_t) + strlen(name) + 1);
    list_prepend_new(&state->current_sub->labels, label);
    label->offset = state->current_sub->offset;
    strcpy(label->name, name);
}

static thecl_spawn_t*
spawn_get(
    parser_state_t* state,
    const char* name)
{
    thecl_spawn_t* spawn;
    list_for_each(&state->main_ecl->spawns, spawn) {
        if (!strcmp(name, spawn->name))
            return spawn;
    }
    return NULL;
}

static int
directive_include(
    parser_state_t* state,
    char* include_path)
{
    char* path = path_get_full(state, include_path);
    FILE* include_file = fopen(path, "rb");

    if (include_file != NULL) {

        FILE* in_org = yyin;
        YYLTYPE loc_org = yylloc;
        const char* input_org = current_input;

        current_input = include_path;
        yyin = include_file;
        yylloc.first_line = 1;
        yylloc.first_column = 1;
        yylloc.last_line = 1;
        yylloc.last_column = 1;

        path_add(state, path);

        int err = yyparse(state);

        fclose(include_file);
        path_remove(state);

        if (err) {
            free(path);
            return 1;
        }

        yyin = in_org;
        yylloc = loc_org;
        current_input = input_org;
    } else {
        yyerror(state, "#include error: couldn't open %s for reading", path);
        return 1;
    }
    free(path);
    return 0;
}

static gool_anim_t*
anim_get(
    parser_state_t* state,
    char* name)
{
    gool_anim_t* anim;
    list_for_each(&state->main_ecl->anims, anim) {
        if (!strcmp(name, anim->name))
            return anim;
    }
    return NULL;
}

static size_t
anim_get_offset(
    parser_state_t* state,
    char* name)
{
    size_t offset = 0;

    gool_anim_t* anim;
    list_for_each(&state->main_ecl->anims, anim) {
        if (!strcmp(name, anim->name))
            return offset / 4;
        offset += anim->size;
        if (offset % 4)
            offset += 4 - (offset % 4);
    }
    return 0xFFFF; /* this is never a valid offset within context */
}

static void
anim_create_anim_c1(
    parser_state_t* state,
    char* name,
    uint16_t frames,
    int eid)
{
    size_t anim_size = sizeof(c1_anim_t);
    c1_anim_t *anim = malloc(anim_size);
    anim->type = 1;
    anim->frames = frames;
    anim->eid = eid;

    gool_anim_t* anim_header = malloc(sizeof(gool_anim_t));
    anim_header->name = strdup(name);
    anim_header->size = anim_size;
    anim_header->anim = anim;
    list_append_new(&state->main_ecl->anims, anim_header);
}

static void
anim_create_anim_c2(
    parser_state_t* state,
    char* name,
    uint16_t frames,
    int eid,
    int interp)
{
    size_t anim_size = sizeof(c2_anim_t);
    c2_anim_t *anim = malloc(anim_size);
    anim->type = 1;
    anim->frames = frames;
    anim->eid = eid;
    anim->interp = interp;

    gool_anim_t* anim_header = malloc(sizeof(gool_anim_t));
    anim_header->name = strdup(name);
    anim_header->size = anim_size;
    anim_header->anim = anim;
    list_append_new(&state->main_ecl->anims, anim_header);
}

static thecl_param_t*
param_sp_new(void)
{
    thecl_param_t* param_sp = param_new('S');
    param_sp->stack = 1;
    param_sp->object_link = 0;
    param_sp->value.val.S = 0x1F;
    return param_sp;
}

static thecl_param_t*
param_null_new(void)
{
    thecl_param_t* param_sp = param_new('S');
    param_sp->stack = 1;
    param_sp->object_link = -2;
    param_sp->value.val.S = 0;
    return param_sp;
}

static thecl_param_t*
param_sp2_new(void)
{
    thecl_param_t* param_sp = param_new('S');
    param_sp->stack = 1;
    param_sp->object_link = -2;
    param_sp->value.val.S = 1;
    return param_sp;
}

static thecl_param_t*
param_var_new(
    int val)
{
    thecl_param_t* param_sp = param_new('S');
    param_sp->stack = 1;
    param_sp->object_link = 0;
    param_sp->value.val.S = val;
    return param_sp;
}

void
yyerror(
    const parser_state_t* state,
    const char* format,
    ...)
{
    /* TODO: Research standard row and column range formats. */
    if (yylloc.first_line == yylloc.last_line) {
        if (yylloc.first_column == yylloc.last_column) {
            fprintf(stderr,
                    "%s:%s:%d,%d: ",
                    argv0, current_input,
                    yylloc.first_line, yylloc.first_column);
        } else {
            fprintf(stderr,
                    "%s:%s:%d,%d-%d: ",
                    argv0, current_input, yylloc.first_line,
                    yylloc.first_column, yylloc.last_column);
        }
    } else {
        fprintf(stderr,
                "%s:%s:%d,%d-%d,%d: ",
                argv0, current_input, yylloc.first_line,
                yylloc.first_column, yylloc.last_line, yylloc.last_column);
    }

    va_list ap;
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);

    fputc('\n', stderr);
}
