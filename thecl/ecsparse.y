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

typedef struct {
    char* text;
} string_t;

static list_t* string_list_add(list_t* list, char* text);
static void string_list_free(list_t* list);

static thecl_instr_t* instr_new(parser_state_t* state, uint8_t id, const char* format, ...);
static thecl_instr_t* instr_new_list(parser_state_t* state, uint8_t id, list_t* list);
static void instr_add(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_prepend(thecl_sub_t* sub, thecl_instr_t* instr);
/* Returns true if the created call was inline. */
static bool instr_create_call(parser_state_t *state, uint8_t type, char *name, list_t *params, bool needs_ret);

static expression_t* expression_load_new(const parser_state_t* state, thecl_param_t* value);
static expression_t* expression_pointer_new(const parser_state_t* state, thecl_param_t* value);
static expression_t* expression_operation_new(const parser_state_t* state, const int symbol, expression_t** operands);
static expression_t* expression_ternary_new(/*const parser_state_t* state, */expression_t* condition, expression_t* val1, expression_t* val2);

static void expression_output(parser_state_t* state, expression_t* expr, int has_no_parents);
static void expression_optimize(parser_state_t* state, expression_t* expr);
#define EXPR_5(a, A, B, C, D, E) \
    expression_operation_new(state, a, (expression_t*[]){ A, B, C, D, E, NULL })
#define EXPR_3(a, A, B, C) \
    expression_operation_new(state, a, (expression_t*[]){ A, B, C, NULL })
#define EXPR_2(a, A, B) \
    expression_operation_new(state, a, (expression_t*[]){ A, B, NULL })
#define EXPR_1(a, A) \
    expression_operation_new(state, a, (expression_t*[]){ A, NULL })

static expression_t *expression_copy(expression_t *expr);
static void expression_create_goto(parser_state_t *state, int type, char *labelstr);

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
static void scope_finish(parser_state_t* state);

static expr_macro_t* macro_create(parser_state_t* state, const char* name, expression_t* expr);
/* Creates a new argument in the specified subroutine. */
static thecl_variable_t* arg_create(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Creates a new global variable. */
static thecl_globalvar_t* globalvar_create(parser_state_t* state, const char* name);
/* Creates a new variable in the specified subroutine. */
static thecl_variable_t* var_create(parser_state_t* state, thecl_sub_t* sub, const char* name, bool push);
/* Creates a new variable in the specified subroutine, and assigns a value to it. */
static thecl_variable_t* var_create_assign(parser_state_t* state, thecl_sub_t* sub, const char* name, expression_t* expr);
/* Returns true if the given variable is accessible in the current scope.. */
static bool var_accessible(parser_state_t* state, thecl_variable_t* var);
/* Returns argument of the given name in the specified sub, or NULL if the argument doesn't exist */
static thecl_variable_t* arg_get(parser_state_t* state, thecl_sub_t* sub, const char* name);
/* Returns global variable of the given name, or NULL if the variable doesn't exist */
static thecl_globalvar_t* globalvar_get(parser_state_t* state, const char* name);
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
static thecl_variable_t* spawn_get(parser_state_t* state, const char* name);

/* Creates a new param equivalent to a GOOL stack push/pop operand */
static thecl_param_t* param_sp_new(void);
/* Creates a new param equivalent to a GOOL double stack pop operand */
static thecl_param_t* param_sp2_new(void);

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
%token <string> DIRECTIVE_FONT "#font"
%token <string> DIRECTIVE_CHAR "#char"
%token <string> DIRECTIVE_TEXT "#text"
%token <string> ENTRY "entry"
%token COMMA ","
%token SEMICOLON ";"
%token SUB "sub"
%token VAR "var"
%token RETURN "return"
%token STATE "state"
%token CODE "code"
%token TRANS "trans"
%token EVENT "event"
%token HANDLES "handles"
%token BRACE_OPEN "{"
%token BRACE_CLOSE "}"
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
%token KILL
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
%token ADDRESSOF "#"
%token ABS "abs"
%token SEEK "seek"
%token DEGSEEK "degseek"
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

%type <list> Instruction_Parameters_List
%type <list> Instruction_Parameters

%type <expression> Expression
%type <expression> ExpressionSubsetInstParam
%type <expression> ExpressionSubsetInstruction
%type <expression> ExpressionLoadType
%type <expression> ExpressionSubset
//%type <expression> Expression_Safe
%type <expression> ParenExpression

%type <param> Instruction_Parameter
%type <param> Address
%type <param> Integer
%type <param> Entry
//%type <param> Text
%type <param> Load_Type
%type <param> Pointer_Type

%left QUESTION
%left OR
%left AND
%left B_OR
%left XOR
%right TEST
%left B_AND
%left EQUAL INEQUAL
%left LT LTEQ GT GTEQ
%left LSHIFT RSHIFT
%left ADD SUBTRACT
%left MULTIPLY DIVIDE MODULO
%precedence NOT B_NOT ADDRESSOF
%precedence ABS

%expect 1
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
        state->current_interrupt->event = event;

        free($2);
      }
      Interrupt_Body {
        list_append_new(&state->ecl->interrupts, state->current_interrupt);

        state->current_interrupt = NULL;
      }
    | DIRECTIVE IDENTIFIER INTEGER INTEGER {
        if (!strcmp($1, "gool")){
            state->ecl->eid = gool_to_eid($2);
            state->ecl->id = $3;
            state->ecl->type = $4;

            state->ecl->is_defined = 1;

            /* automatically create an expression macro that translates the ename to the GOOL ID */
            thecl_param_t* param = param_new('S');
            param->value.val.S = $3;
            macro_create(state, $2, expression_load_new(state, param));
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
            list_append_new(&state->ecl->spawns, spawn);
            ++state->spawn_count;
        }
        free($1);
        free($2);
        free($3);
      }
    | DIRECTIVE IDENTIFIER IDENTIFIER INTEGER {
        if (!strcmp($1, "anim")){
            anim_create_anim_c1(state, $2, $4, gool_to_eid($3));
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
    | DIRECTIVE_FONT IDENTIFIER IDENTIFIER {
        gool_anim_t* anim = malloc(sizeof(gool_anim_t));
        anim->name = strdup($2);
        anim->size = sizeof(c1_font_t);

        c1_font_t* font = malloc(sizeof(c1_font_t));
        font->type = 3;
        font->char_count = 0;
        font->eid = gool_to_eid($3);

        anim->anim = font;
        state->current_anim = anim;

        free($1);
        free($2);
        free($3);
      }
      Font_Chars {
        list_append_new(&state->ecl->anims, state->current_anim);
        state->current_anim = NULL;
      }
    | DIRECTIVE_TEXT IDENTIFIER IDENTIFIER INTEGER {
        gool_anim_t* anim = malloc(sizeof(gool_anim_t));
        anim->name = strdup($2);
        anim->size = sizeof(c1_text_t);

        c1_text_t* text = malloc(sizeof(c1_text_t));
        text->type = 4;
        text->string_count = 0;
        text->unknown = $4;
        text->font = anim_get_offset(state, $3);

        anim->anim = text;
        state->current_anim = anim;

        free($1);
        free($2);
        free($3);
      }
      String_List {
        list_append_new(&state->ecl->anims, state->current_anim);
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
        globalvar_create(state, $2);
        free($2);
      }
    | GlobalVarDeclaration "," IDENTIFIER {
        globalvar_create(state, $3);
        free($3);
      }
    ;

Font_Chars:
    %empty
    | Font_Chars Font_Char
    ;

Font_Char:
    DIRECTIVE_CHAR INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER INTEGER { /* jesus christ */
        free($1);

        c1_font_t* font = state->current_anim->anim;
        font = realloc(font, sizeof(c1_font_t) + sizeof(c1_char_t) * ++font->char_count);
        state->current_anim->anim = font;
        state->current_anim->size = sizeof(c1_font_t) + sizeof(c1_char_t) * font->char_count;

        c1_char_t* character = font->chars + font->char_count - 1;
        character->tex1 = $2 & 0xFFFFFF;
        character->tex1 |= ($5 & 0xF) << 24;
        character->tex1 |= ($4 & 0x3) << 29;
        character->tex2 = $9 & 0x1F;
        character->tex2 |= ($6 & 0x7F) << 6;
        character->tex2 |= ($8 & 0x1F) << 13;
        character->tex2 |= ($7 & 0x3) << 18;
        character->tex2 |= ($3 & 0x3) << 20;
        character->tex2 |= ($10 & 0x3FF) << 22;
        character->w = $11;
        character->h = $12;
    }
    ;

String_List:
    %empty
    | String_List TEXT {
        size_t stringlen = strlen($2) + 1;

        state->current_anim->anim = realloc(state->current_anim->anim, state->current_anim->size + stringlen);

        c1_text_t* text = state->current_anim->anim;

        char *string = (char*)state->current_anim->anim + state->current_anim->size;
        strcpy(string, $2);

        state->current_anim->size += stringlen;

        free($2);
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
    | State_Instructions IDENTIFIER INTEGER
      {
        if (!strcmp($2, "stateflag"))
            state->current_state->stateflag = $3;
        else if (!strcmp($2, "statusc"))
            state->current_state->statusc = $3;
        else {
            yyerror("syntax error, unpexpected %s in state body", $2);
        }
        free($2);
      }
    | State_Instructions "trans" {
        if (state->current_state->trans)
            yyerror(state, "duplicate trans block in state: %s", state->current_state->name);
        char buf[256];
        snprintf(buf, 256, "%s_TRANS_%i_%i", state->current_state->name, yylloc.first_line, yylloc.first_column);
        sub_begin(state, buf);
        state->current_sub->is_inline = false;
        state->current_state->trans = state->current_sub;
      }
      Subroutine_Body {
        sub_finish(state);
      }
    | State_Instructions "code" {
        if (state->current_state->code)
            yyerror(state, "duplicate code block in state: %s", state->current_state->name);
        char buf[256];
        snprintf(buf, 256, "%s_CODE_%i_%i", state->current_state->name, yylloc.first_line, yylloc.first_column);
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
        snprintf(buf, 256, "%s_EVENT_%i_%i", state->current_state->name, yylloc.first_line, yylloc.first_column);
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

ParenExpression:
      "(" Expression ")"
        { $$ = $2; }
    ;

Block:
      IfBlock
    | WhileBlock
    | CodeBlock
    ;

CodeBlock:
      "{" {
          scope_begin(state);
      } Instructions "}" {
          scope_finish(state);
      }
    | Instruction ";"
    ;

BreakStatement:
      "break" {
          list_node_t *head = state->block_stack.head;
          for(; head; head = head->next) {
              if (
                  strncmp(head->data, "while", 5) == 0 ||
                  strncmp(head->data, "until", 5) == 0
              ) {
                  char labelstr[256];
                  snprintf(labelstr, 256, "%s_end", (char*)head->data);
                  expression_create_goto(state, GOTO, labelstr);
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
          expression_output(state, $cond, 1);
          expression_free($cond);
          expression_create_goto(state, IF, labelstr);
      } CodeBlock ElseBlock {
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          state->block_stack.head = head->next;
          free(head->data);
          list_del(&state->block_stack, head);
        }
    | "if" ParenExpression[cond]  /*%expect 1*/ {
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          list_prepend_new(&state->block_stack, strdup(labelstr));
          expression_output(state, $cond, 1);
          expression_free($cond);
          expression_create_goto(state, UNLESS, labelstr);
      } CodeBlock ElseBlock {
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          free(head->data);
          list_del(&state->block_stack, head);
      }
      ;

ElseBlock:
    %empty
    | "else"  {
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          expression_create_goto(state, GOTO, labelstr);
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          free(head->data);
          list_del(&state->block_stack, head);
          list_prepend_new(&state->block_stack, strdup(labelstr));
    } CodeBlock
    | "else" {
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          expression_create_goto(state, GOTO, labelstr);
          list_node_t *head = state->block_stack.head;
          label_create(state, head->data);
          free(head->data);
          list_del(&state->block_stack, head);
          list_prepend_new(&state->block_stack, strdup(labelstr));
      } IfBlock
      ;

WhileBlock:
      "while" ParenExpression[cond] {
          expression_optimize(state, $cond);
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
          expression_output(state, $cond, 0);
          expression_free($cond);
          expression_create_goto(state, UNLESS, labelstr_end);
      } CodeBlock {
          if (state->ignore_block) {
              --state->ignore_block;
              break;
          }
          char labelstr_st[256];
          char labelstr_end[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);

          expression_create_goto(state, GOTO, labelstr_st);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
      }
    | "until" ParenExpression[cond] {
          expression_optimize(state, $cond);
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
          expression_output(state, $cond, 0);
          expression_free($cond);
          expression_create_goto(state, IF, labelstr_end);
      } CodeBlock {
          if (state->ignore_block) {
              --state->ignore_block;
              break;
          }
          char labelstr_st[256];
          char labelstr_end[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);

          expression_create_goto(state, GOTO, labelstr_st);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
      }
    | "do"  {
          char labelstr[256];
          snprintf(labelstr, 256, "do_%i_%i", yylloc.first_line, yylloc.first_column);
          char labelstr_st[256];
          char labelstr_end[256];
          snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
          snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);

          list_prepend_new(&state->block_stack, strdup(labelstr));
          label_create(state, labelstr_st);
    } DoBlock
    ;
DoBlock:
      CodeBlock "while" ParenExpression[cond]  {
          char labelstr_st[256];
          char labelstr_end[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);

          expression_output(state, $cond, 1);
          expression_free($cond);
          expression_create_goto(state, IF, labelstr_st);
          label_create(state, labelstr_end);

          free(head->data);
          list_del(&state->block_stack, head);
    }
    | CodeBlock "until" ParenExpression[cond]  {
          char labelstr_st[256];
          char labelstr_end[256];
          list_node_t *head = state->block_stack.head;
          snprintf(labelstr_st, 256, "%s_st", (char*)head->data);
          snprintf(labelstr_end, 256, "%s_end", (char*)head->data);

          expression_output(state, $cond, 1);
          expression_free($cond);
          expression_create_goto(state, UNLESS, labelstr_st);
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
                if (gool_ins->late_param >= 0) {
                    int i = 0;
                    list_node_t* e = state->expressions.tail;
                    list_for_each(param_list, param) {
                        if (i++ == gool_ins->late_param && param->is_expression_param) {
                            late_expr = e->data;
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
                    int i = 0;
                    list_for_each(param_list, param) {
                        if (param->is_expression_param && i != gool_ins->late_param) {
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
                        ++i;
                    }
                }
                list_for_each(arg_list, param) {
                    if (!(param->stack && param->object_link == 0 && param->value.val.S == 0x1F)) { /* argument is already on the stack */
                        if (param->object_link == -3) {
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
                    expression_output(state, late_expr, 1);
                    expression_free(late_expr);
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
                    expression_output(state, expression, 1);
                    expression_free(expression);
                }
                list_free_nodes(&state->expressions);
                instr_add(state, state->current_sub, instr_new_list(state, gool_ins->id, gool_ins->param_list_validate($3, 0)));
            }
        }
        else {
            expression_t* expression;
            list_for_each(&state->expressions, expression) {
                expression_output(state, expression, 1);
                expression_free(expression);
            }
            list_free_nodes(&state->expressions);
            instr_create_call(state, state->ins_jal, $1, $3, false);
        }
        if ($3 != NULL) {
            list_free_nodes($3);
            free($3);
        }
      }
    | "goto" IDENTIFIER {
        expression_create_goto(state, GOTO, $2);
    }
    | Assignment
    | ExpressionSubsetInstruction {
        expression_output(state, $1, 1);
        expression_free($1);
      }
    | VarDeclaration
    | BreakStatement
    | "return" {
        if (state->current_sub->is_inline)
            expression_create_goto(state, GOTO, "inline_end");
        else 
            instr_add(state, state->current_sub, instr_new(state, state->ins_ret, "SSSSS", 0, 0, 0x25, 0, 2));
    }
    ;

Assignment:
      Address "=" Expression {
        expression_optimize(state, $3);
        thecl_param_t* src_param;
        const expr_t* expr = expr_get_by_id(state->version, $3->id);
        if ($3->type == EXPRESSION_VAL && ($1->stack != 2 || ($1->stack == 2 && !expr->is_unary))) {
            src_param = $3->value;
        } else {
            expression_output(state, $3, 0);
            src_param = param_sp_new();
        }
        expression_free($3);
        if ($1->stack != 2) {
            expr = expr_get_by_symbol(state->version, ASSIGN);

            instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", $1, src_param));
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
    | Address ">>=" Expression { thecl_param_t* param = param_new('S'); param->value.val.S = 0; var_shorthand_assign(state, $1, EXPR_2(SUBTRACT, expression_load_new(state, param), $3), LSHIFT); }
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
    | Pointer_Type
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

ExpressionSubsetInstruction:
      ExpressionLoadType
    | ExpressionSubset
    ;

ExpressionLoadType:
      Load_Type                      { $$ = expression_load_new(state, $1); }
    | Pointer_Type                   { $$ = expression_pointer_new(state, $1); }
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
    | Expression ">>"  Expression { thecl_param_t* param = param_new('S'); param->value.val.S = 0; $$ = EXPR_2(LSHIFT,   EXPR_2(SUBTRACT, expression_load_new(state, param), $3), $3); }
    | Expression "\\"  Expression { $$ = EXPR_2(TEST,     $1, $3); }
    | "#" Expression              { $$ = EXPR_2(ADDRESSOF,expression_load_new(state, param_sp_new()), $2); }
    | "abs" "(" Expression ")"    { $$ = EXPR_2(ABS,    expression_load_new(state, param_sp_new()), $3); }
    | "seek" "(" Expression "," Expression "," Expression ")"     { $$ = EXPR_3(SEEK, $3, $5, $7); }
    | "seek" "(" Expression "," Expression ")"                    { $$ = EXPR_2(SEEK, $3, $5); }
    | "degseek" "(" Expression "," Expression "," Expression ")"  { $$ = EXPR_3(DEGSEEK, $3, $5, $7); }
    | "degseek" "(" Expression "," Expression ")"                 { $$ = EXPR_2(DEGSEEK, $3, $5); }
    | "rand" "(" Expression "," Expression ")"                    { $$ = EXPR_2(RAND, $3, $5); }
    | "nearseek" "(" Expression "," Expression "," Expression ")" { $$ = EXPR_3(NEARSEEK, $3, $5, $7); }
    | "nearseek" "(" Expression "," Expression ")"                { $$ = EXPR_2(NEARSEEK, $3, $5); }
    | "time" "(" Expression "," Expression ")"                    { $$ = EXPR_2(TIME, $3, $5); }
    | "time" "(" Expression ")" { thecl_param_t* param = param_new('S'); param->value.val.S = 0; $$ = EXPR_2(TIME, $3, expression_load_new(state, param)); }
    | "getcolor" "(" Expression "," Expression ")" { $$ = EXPR_2(GETCOLOR, $3, $5); }
    | "getcolor" "(" Expression ")" { thecl_param_t* param = param_new('S'); param->value.val.S = 0; $$ = EXPR_2(GETCOLOR, expression_load_new(state, param), $3); }
    | "pad" "(" Expression "," Expression "," Expression "," Expression "," Expression ")" { $$ = EXPR_5(PAD, $3, $5, $7, $9, $11); }
    | "buttonpress" "(" Expression ")" {
        thecl_param_t *p1, *p2, *p3, *p4;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S'); p4 = param_new('S');
        p1->value.val.S = 1; p2->value.val.S = 0; p3->value.val.S = 0; p4->value.val.S = 0;
        $$ = EXPR_5(PAD, $3, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), expression_load_new(state, p4));
      }
    | "buttonhold" "(" Expression ")" {
        thecl_param_t *p1, *p2, *p3, *p4;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S'); p4 = param_new('S');
        p1->value.val.S = 2; p2->value.val.S = 0; p3->value.val.S = 0; p4->value.val.S = 0;
        $$ = EXPR_5(PAD, $3, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), expression_load_new(state, p4));
      }
    | "buttonbuffer" "(" Expression ")" {
        thecl_param_t *p1, *p2, *p3, *p4;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S'); p4 = param_new('S');
        p1->value.val.S = 3; p2->value.val.S = 0; p3->value.val.S = 0; p4->value.val.S = 0;
        $$ = EXPR_5(PAD, $3, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), expression_load_new(state, p4));
      }
    | "buttonpress" "(" Expression "," Expression ")" {
        thecl_param_t *p1, *p2, *p3;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S');
        p1->value.val.S = 1; p2->value.val.S = 0; p3->value.val.S = 0;
        $$ = EXPR_5(PAD, $3, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $5);
      }
    | "buttonhold" "(" Expression "," Expression ")" {
        thecl_param_t *p1, *p2, *p3;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S');
        p1->value.val.S = 2; p2->value.val.S = 0; p3->value.val.S = 0;
        $$ = EXPR_5(PAD, $3, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $5);
      }
    | "buttonbuffer" "(" Expression "," Expression ")" {
        thecl_param_t *p1, *p2, *p3;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S');
        p1->value.val.S = 3; p2->value.val.S = 0; p3->value.val.S = 0;
        $$ = EXPR_5(PAD, $3, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $5);
      }
    | "dirpress" "(" Expression ")" {
        thecl_param_t *p1, *p2, *p3, *p4;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S'); p4 = param_new('S');
        p1->value.val.S = 0; p2->value.val.S = 0; p3->value.val.S = 1; p4->value.val.S = 0;
        $$ = EXPR_5(PAD, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $3, expression_load_new(state, p4));
      }
    | "dirhold" "(" Expression ")" {
        thecl_param_t *p1, *p2, *p3, *p4;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S'); p4 = param_new('S');
        p1->value.val.S = 0; p2->value.val.S = 0; p3->value.val.S = 2; p4->value.val.S = 0;
        $$ = EXPR_5(PAD, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $3, expression_load_new(state, p4));
      }
    | "dirbuffer" "(" Expression ")" {
        thecl_param_t *p1, *p2, *p3, *p4;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S'); p4 = param_new('S');
        p1->value.val.S = 0; p2->value.val.S = 0; p3->value.val.S = 3; p4->value.val.S = 0;
        $$ = EXPR_5(PAD, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $3, expression_load_new(state, p4));
      }
    | "dirpress" "(" Expression "," Expression ")" {
        thecl_param_t *p1, *p2, *p3;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S');
        p1->value.val.S = 0; p2->value.val.S = 0; p3->value.val.S = 1;
        $$ = EXPR_5(PAD, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $3, $5);
      }
    | "dirhold" "(" Expression "," Expression ")" {
        thecl_param_t *p1, *p2, *p3;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S');
        p1->value.val.S = 0; p2->value.val.S = 0; p3->value.val.S = 2;
        $$ = EXPR_5(PAD, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $3, $5);
      }
    | "dirbuffer" "(" Expression "," Expression ")" {
        thecl_param_t *p1, *p2, *p3;
        p1 = param_new('S'); p2 = param_new('S'); p3 = param_new('S');
        p1->value.val.S = 0; p2->value.val.S = 0; p3->value.val.S = 3;
        $$ = EXPR_5(PAD, expression_load_new(state, p1), expression_load_new(state, p2), expression_load_new(state, p3), $3, $5);
      }
    | Expression "!="  Expression {
        $$ = EXPR_2(INEQUAL,  $1, $3);
        $$ = EXPR_2(NOT,      expression_load_new(state, param_sp_new()), $$);
      }

    /* Custom expressions. */
    /*
    | Expression "?" Expression_Safe ":" Expression_Safe  %prec QUESTION
                                  { $$ = expression_ternary_new(/*state, $1, $3, $5); }
    ;*/

/* 
   The purpose of this is to be used in places that contain certain tokens
   that could be a part of an expression too, to prevent such tokens from
   mistakenly being parsed as expressions.
   An example of such situation is the ':' from "case 1:" being parsed as a part of
   the rank switch expression.
   Of course, this still allows any expression to be put in - it just requires it to
   be in brackets (unless it's a literal), which prevents any bad things from happening.
*//*
Expression_Safe:
      Load_Type                      { $$ = expression_load_new(state, $1); }
    |             "(" Expression ")" { $$ = $2; }
    ;*/

Address:
      IDENTIFIER {
        thecl_variable_t* arg;
        const field_t* gvar;
        thecl_spawn_t* spawn;
        const field_t* field;
        const field_t* event;
        if (var_exists(state, state->current_sub, $1)) {
            $$ = param_new('S');
            $$->stack = 1;
            $$->value.val.S = var_stack(state, state->current_sub, $1);
        } else if (arg = arg_get(state, state->current_sub, $1)) {
            $$ = param_new('S');
            $$->stack = 1;
            $$->value.val.S = arg->stack;
        } else if (gvar = gvar_get(state->version, $1)) {
            $$ = param_new('S');
            $$->stack = 2;
            $$->value.val.S = gvar->offset << 8;
        } else if (spawn = spawn_get(state, $1)) {
            $$ = param_new('S');
            $$->value.val.S = spawn->offset;
        } else if (field = field_get($1)) {
            $$ = param_new('S');
            $$->stack = 1;
            $$->value.val.S = field->offset;
            $$->object_link = 0;
        } else if (event = event_get(state->version, $1)) {
            $$ = param_new('S');
            $$->value.val.S = event->offset << 8;
        } else {
            thecl_globalvar_t* globalvar = globalvar_get(state, $1);
            size_t anim_offset;
            if (globalvar) {
                $$ = param_new('S');
                $$->stack = 1;
                $$->value.val.S = globalvar->offset + 64;
                $$->object_link = 0;
            } else if ((anim_offset = anim_get_offset(state, $1)) != 0xFFFF) {
                $$ = param_new('S');
                $$->value.val.S = anim_offset;
            } else {
                if ((state->current_sub == NULL || strncmp($1, state->current_sub->name, strlen(state->current_sub->name)) != 0)
                ) {
                    yyerror(state, "warning: %s not found as a variable, treating like a label or state instead.", $1);
                }
                $$ = param_new('o');
                $$->value.type = 'z';
                $$->value.val.z = strdup($1);
            }
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
        $$->object_link = -3;
        free($1);
      }
    ;

Load_Type:
      Address
    | Integer
    ;

Pointer_Type:
      Entry
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
    const expr_t* expr = expr_get_by_symbol(state->version, PLOAD);
    /* push optimization */
    if (instr->id == 22) {
        thecl_instr_t* last_ins = list_tail(&sub->instrs);
        if (last_ins != NULL) {
            thecl_label_t* tmp_label;
            list_for_each(&sub->labels, tmp_label) {
                if (tmp_label->offset == sub->offset)
                    goto NO_OPTIM;
            }

            while (last_ins->id == 22 && last_ins->param_count < 2 && instr->param_count > 0) {
                ++last_ins->param_count;
                list_append_new(&last_ins->params, list_head(&instr->params));
                list_del(&instr->params, instr->params.head);
                if (--instr->param_count == 0) {
                    thecl_instr_free(instr);
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
                    return;
                }
            }
        }
    }
    /* branch optimization */
    else if (instr->id == state->ins_bra && instr->param_count == 5 &&
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
                thecl_param_t* param;
                list_for_each(&last_ins->params, param) {
                    if (param->value.val.S != 0x1F || !param->stack || param->object_link)
                        goto NO_OPTIM;
                }
                param = instr->params.tail->prev->data;
                param->value.val.S = param->value.val.S == 1 ? 2 : 1;

                list_del(&sub->instrs, sub->instrs.tail);
                thecl_instr_free(last_ins);
                --sub->offset;
            } else { /* optimize if literal conditions */
                expr = expr_get_by_symbol(state->version, LOAD);
                if (last_ins->id == expr->id && last_ins->param_count > 0) {
                    thecl_param_t* param = list_tail(&last_ins->params);
                    if (!param->stack) {
                        thecl_param_t* branch_type_param = (thecl_param_t*)instr->params.tail->prev->data;
                        int branch_type = branch_type_param->value.val.S;
                        --branch_type;
                        list_del(&last_ins->params, last_ins->params.tail);
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
    new_instr->string = strdup(instr->string);
    list_init(&new_instr->params);
    thecl_param_t* param;
    list_for_each(&instr->params, param) {
        thecl_param_t* new_param = param_copy(param);
        list_append_new(&new_instr->params, new_param);
    }
    return new_instr;
}
static bool
instr_create_call(
    parser_state_t *state,
    uint8_t type,
    char *name,
    list_t *params,
    bool needs_ret)
{
    /* First, check if the called sub is inline. */
    /* thecl_sub_t* sub;
    list_for_each(&state->ecl->subs, sub) {
        if (sub->is_inline && !strcmp(sub->name, name)) {
            instr_create_inline_call(state, sub, params, needs_ret);
            free(name);
            return true;
        }
    } */

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

                if (current_expr->type == EXPRESSION_OP)
                    expression_optimize(state, current_expr);
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
                    expression_output(state, current_expr, 0);
                    list_del(&state->expressions, last_node);
                    expression_free(current_expr);
                }
            }

            instr_add(state, state->current_sub, instr_new(state, 22, "p", param));

            ++argc;
        }
    }

    /* Output expressions from parameters. */
    expression_t* expr;
    list_for_each(&state->expressions, expr) {
        expression_output(state, expr, 0);
        expression_free(expr);
    }
    list_free_nodes(&state->expressions);

    instr_add(state, state->current_sub, instr_new(state, type, "pS", name_param, argc));
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
        ret->type = EXPRESSION_GVAR;
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

    return ret;
}

static expression_t*
expression_ternary_new(
    //const parser_state_t* state,
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
    char *labelstr)
{
    const expr_t* expr = expr_get_by_symbol(state->version, type);
    thecl_param_t *p1 = param_new('o');
    p1->value.type = 'z';
    p1->value.val.z = strdup(labelstr);
    instr_add(state, state->current_sub, instr_new(state, expr->id, "pSSSS", p1, 0, 0x1F, state->version == 1 ? (type == GOTO ? 0 : (type == IF ? 1 : (type == UNLESS ? 2 : 3))) : 0, 0));
}

static void
expression_output(
    parser_state_t* state,
    expression_t* expr,
    int has_no_parents)
{
    if (has_no_parents)
        /* Since expression_optimize is already done recursively for children, it shouldn't be called for child expressions. */
        expression_optimize(state, expr);

    if (expr->type == EXPRESSION_VAL) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "p", expr->value));
    } else if (expr->type == EXPRESSION_GVAR) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "S", expr->value->value.val.S));
    } else if (expr->type == EXPRESSION_OP) {
        const expr_t* expression = expr_get_by_id(state->version, expr->id);
        expression_t* child_expr;
        thecl_param_t* val_param1 = NULL;
        thecl_param_t* val_param2 = NULL;
        list_t* push_list = list_new();
        int c = 0;
        list_node_t* child_node;
        list_for_each_node(&expr->children, child_node) {
            ++c;
            child_expr = child_node->data;
            if (child_expr->type == EXPRESSION_VAL && expression->symbol != PAD) {
                if (c == 1) {
                    val_param1 = child_expr->value;
                    continue;
                } else if (c == 2) {
                    if ((expression->has_double_param && !child_node->next) || !expression->has_double_param) {
                        val_param2 = child_expr->value;
                        continue;
                    }
                }
            }
            list_append_new(push_list, child_expr);
        }
        if (expression->symbol == PAD) {
            instr_add(state, state->current_sub, instr_new(state, expr->id, "ppppp",
            ((expression_t*)push_list->head->data)->value,
            ((expression_t*)push_list->head->next->data)->value,
            ((expression_t*)push_list->head->next->next->data)->value,
            ((expression_t*)push_list->head->next->next->next->data)->value,
            ((expression_t*)push_list->head->next->next->next->next->data)->value
            ));
            return;
        }
        list_for_each(push_list, child_expr) {
            expression_output(state, child_expr, 0);
        }
        list_free_nodes(push_list);
        free(push_list);
        if (val_param1 == NULL) {
            val_param1 = param_sp_new();
        }
        if (c == 3 && expression->has_double_param) {
            val_param2 = param_sp2_new();
        } else if (val_param2 == NULL) {
            val_param2 = param_sp_new();
        }

        instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", val_param1, val_param2));
    } else if (expr->type == EXPRESSION_TERNARY) {
        char labelstr_unless[256];
        char labelstr_end[256];

        snprintf(labelstr_unless, 256, "ternary_unless_%d_%d", yylloc.first_line, yylloc.first_column);
        snprintf(labelstr_end, 256, "ternary_end_%d_%d", yylloc.first_line, yylloc.first_column);

        int i = 0;
        expression_t* child_expr;
        list_for_each(&expr->children, child_expr) {
            expression_output(state, child_expr, 1);
            if (i == 0) {
                expression_create_goto(state, UNLESS, labelstr_unless);
            } else if (i == 1) {
                expression_create_goto(state, GOTO, labelstr_end);
                label_create(state, labelstr_unless);
            } else {
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

    if (   child_expr_1->type != EXPRESSION_VAL
        || child_expr_2->type != EXPRESSION_VAL
        || child_expr_1->value->stack /* Variables are not acceptable, obviously. */
        || child_expr_2->value->stack
    ) return;

    thecl_param_t* param = param_new('S');

    int val1 = child_expr_1->value->value.val.S;
    int val2 = child_expr_2->value->value.val.S;

    switch(tmp_expr->symbol) {
        case ADD:
            param->value.val.S = val1 + val2;
        break;
        case SUBTRACT:
            param->value.val.S = val1 - val2;
        break;
        case MULTIPLY:
            param->value.val.S = val1 * val2;
        break;
        case DIVIDE:
            param->value.val.S = val1 / val2;
        break;
        case MODULO:
            param->value.val.S = val1 % val2;
        break;
        case EQUAL:
            param->value.val.S = val1 == val2;
        break;
        case INEQUAL:
            param->value.val.S = val1 != val2;
        break;
        case LT:
            param->value.val.S = val1 < val2;
        break;
        case LTEQ:
            param->value.val.S = val1 <= val2;
        break;
        case GT:
            param->value.val.S = val1 > val2;
        break;
        case GTEQ:
            param->value.val.S = val1 >= val2;
        break;
        case OR:
            param->value.val.S = val1 || val2;
        break;
        case AND:
            param->value.val.S = val1 && val2;
        break;
        case XOR:
            param->value.val.S = val1 ^ val2;
        break;
        case B_OR:
            param->value.val.S = val1 | val2;
        break;
        case B_AND:
            param->value.val.S = val1 & val2;
        break;
        case LSHIFT:
            param->value.val.S = val1 << val2;
        break;
        case TEST:
            param->value.val.S = (val1 & val2) == val2;
        break;
        default:
            /* Since the cases above cover all existing 2-parameter expressions there is no possibility of this ever hapenning.
               Just putting this error message in case someone adds new expressions and forgets about handling them here... */
            yyerror(state, "Math preprocessing error!");
    }

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

static void
state_begin(
    parser_state_t* state,
    char* name)
{
    thecl_state_t* iter_state;
    list_for_each(&state->ecl->states, iter_state) {
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
    gstate->index = state->state_count++;
    list_append_new(&state->ecl->states, gstate);

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
    thecl_sub_t* iter_sub;
    list_for_each(&state->ecl->subs, iter_sub) {
        if(!strcmp(name, iter_sub->name) && !iter_sub->forward_declaration) {
            yyerror(state, "duplicate sub: %s", name);
            g_was_error = true;
            break;
        }
    }

    scope_begin(state);

    thecl_sub_t* sub = malloc(sizeof(thecl_sub_t));

    sub->name = strdup(name);
    list_init(&sub->instrs);
    sub->stack = 3;
    sub->var_count = 0;
    sub->arg_count = 0;
    sub->vars = NULL;
    sub->args = NULL;
    sub->start_offset = state->ins_offset;
    sub->offset = 0;
    sub->instr_data = NULL;
    list_init(&sub->labels);

    list_append_new(&state->ecl->subs, sub);

    state->current_sub = sub;
}

static void
sub_finish(
    parser_state_t* state)
{
    scope_finish(state);

    if (state->current_sub->is_inline) {
        thecl_instr_t* last_ins = list_tail(&state->current_sub->instrs);
        const expr_t* tmp = expr_get_by_symbol(state->version, GOTO);
        if (last_ins != NULL && last_ins->id == tmp->id) {
            thecl_param_t* label_param = list_head(&last_ins->params);
            if (strcmp(label_param->value.val.z, "inline_end") == 0) {
                /* Remove useless goto. */
                list_del(&state->current_sub->instrs, state->current_sub->instrs.tail);
                --state->current_sub->offset;
                thecl_instr_free(last_ins);
            }
        }
        label_create(state, "inline_end");
    }
    else {
        thecl_instr_t* last_ins = state->version == 1 ? NULL : list_tail(&state->current_sub->instrs);
        if (last_ins == NULL || last_ins->id != state->ins_ret) {
            instr_add(state, state->current_sub, instr_new(state, state->ins_ret, "SSSSS", 0, 0, 0x25, 0, 2));
        }
    }

    state->ins_offset += state->current_sub->offset;

    state->current_sub = NULL;
}

static void
scope_begin(
    parser_state_t* state
) {
    ++state->scope_cnt;
    state->scope_stack = realloc(state->scope_stack, sizeof(int)*state->scope_cnt);
    state->scope_stack[state->scope_cnt - 1] = state->scope_id++;
}

static void
scope_finish(
    parser_state_t* state
) {
    --state->scope_cnt;

    /* pop GOOL stack variables */
    int pop = 0;
    thecl_variable_t* var;
    for (int v=0; v < state->current_sub->var_count; ++v)
        if (state->current_sub->vars[v]->scope == state->scope_stack[state->scope_cnt]) {
            memmove(state->current_sub->vars + v, state->current_sub->vars + (v + 1), (--state->current_sub->var_count - v) * sizeof(int));
            --v;
            ++pop;
        }
    if (pop > 0) {
        const expr_t* expr = expr_get_by_symbol(state->version, GOTO);
        instr_add(state, state->current_sub, instr_new(state, expr->id, "SSSSS", 0, pop, 0x25, 0, 0));
    }

    state->scope_stack = realloc(state->scope_stack, sizeof(int)*state->scope_cnt);
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
    expression_optimize(state, expr);
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

    ++sub->arg_count;
    sub->args = realloc(sub->args, sub->arg_count * sizeof(thecl_variable_t*));
    sub->args[sub->arg_count - 1] = arg;

    for (int i = 0; i < sub->arg_count; ++i) {
        sub->args[i]->stack = -sub->arg_count + i;
    }

    return arg;
}

static thecl_globalvar_t*
globalvar_create(
    parser_state_t* state,
    const char* name)
{
    if (globalvar_get(state, name) != NULL) {
        yyerror(state, "redeclaration of global variable: %s", name);
    }

    thecl_globalvar_t* var = malloc(sizeof(thecl_globalvar_t));
    var->name = strdup(name);
    var->offset = state->ecl->var_count++;

    state->ecl->vars = realloc(state->ecl->vars, state->ecl->var_count * sizeof(thecl_globalvar_t*));
    state->ecl->vars[state->ecl->var_count - 1] = var;

    return var;
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
    var->scope = state->scope_stack[state->scope_cnt - 1];

    ++sub->var_count;
    sub->vars = realloc(sub->vars, sub->var_count * sizeof(thecl_variable_t*));
    sub->vars[sub->var_count - 1] = var;

    if (var->stack == sub->stack) /* Only increment the stack if the variable uses aa new offset. */
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

    thecl_param_t* param;
    expression_optimize(state, expr);
    if (expr->type == EXPRESSION_VAL) {
        param = expr->value;
    } else {
        expression_output(state, expr, 0);
        param = NULL;
    }
    expression_free(expr);

    if (param != NULL) { /* if param is NULL, then an expression was pushed to stack, which is enough */
        const expr_t* expr_assign = expr_get_by_symbol(state->version, ASSIGN);
        instr_add(state, state->current_sub, instr_new(state, expr_assign->id, "pp", param_sp_new(), param));
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

static thecl_globalvar_t*
globalvar_get(
    parser_state_t* state,
    const char* name
) {
    for (size_t i = 0; i < state->ecl->var_count; ++i) {
        if (!strcmp(name, state->ecl->vars[i]->name))
            return state->ecl->vars[i];
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
    expression_output(state, expr_main, 1);
    expression_free(expr_main);
    /* No need to free expr_load or expr, since they both got freed as children of expr_main. */

    if (param->stack != 2) {
        const expr_t* expr = expr_get_by_symbol(state->version, ASSIGN);

        instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", param, param_sp_new()));
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

static thecl_variable_t*
spawn_get(
    parser_state_t* state,
    const char* name)
{
    thecl_spawn_t* spawn;
    list_for_each(&state->ecl->spawns, spawn) {
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
    list_for_each(&state->ecl->anims, anim) {
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
    list_for_each(&state->ecl->anims, anim) {
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
    list_append_new(&state->ecl->anims, anim_header);
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
param_sp2_new(void)
{
    thecl_param_t* param_sp = param_new('S');
    param_sp->stack = 1;
    param_sp->object_link = -2;
    param_sp->value.val.S = 1;
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
