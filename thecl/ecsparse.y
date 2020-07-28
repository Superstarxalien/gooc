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
#include "gmips.h"

static thecl_instr_t* instr_new(parser_state_t* state, uint8_t id, const char* format, ...);
static thecl_instr_t* instr_new_list(parser_state_t* state, uint8_t id, list_t* list);
static void mips_stack_adjust(parser_state_t* state, thecl_sub_t* sub);
static thecl_instr_t* mips_instr_new(parser_state_t* state, const char* name, int imm, int shamt, int rd, int rt, int rs, int addr);
static thecl_instr_t* mips_instr_branch_new(parser_state_t* state, const char* name, int rt, int rs, const char* label);
#define MIPS_INSTR_SHIFT(name, shamt, rd, rt) \
    mips_instr_new(state, name, 0, shamt, rd, rt, 0, 0)
#define MIPS_INSTR_ALU_R(name, rd, rt, rs) \
    mips_instr_new(state, name, 0, 0, rd, rt, rs, 0)
#define MIPS_INSTR_MULT(rt, rs) \
    mips_instr_new(state, "mult", 0, 0, 0, rt, rs, 0)
#define MIPS_INSTR_DIV(rt, rs) \
    mips_instr_new(state, "div", 0, 0, 0, rt, rs, 0)
#define MIPS_INSTR_MFLO(rd) \
    mips_instr_new(state, "mflo", 0, 0, rd, 0, 0, 0)
#define MIPS_INSTR_MFHI(rd) \
    mips_instr_new(state, "mfhi", 0, 0, rd, 0, 0, 0)
#define MIPS_INSTR_MTLO(rs) \
    mips_instr_new(state, "mtlo", 0, 0, 0, 0, rs, 0)
#define MIPS_INSTR_MTHI(rs) \
    mips_instr_new(state, "mthi", 0, 0, 0, 0, rs, 0)
#define MIPS_INSTR_JR(rs) \
    mips_instr_new(state, "jr", 0, 0, 0, 0, rs, 0)
#define MIPS_INSTR_JALR(rd, rs) \
    mips_instr_new(state, "jalr", 0, 0, rd, 0, rs, 0)
#define MIPS_INSTR_I(name, imm, rt, rs) \
    mips_instr_new(state, name, imm, 0, 0, rt, rs, 0)
#define MIPS_INSTR_BEQ(label, rt, rs) \
    mips_instr_branch_new(state, "beq", rt, rs, label)
#define MIPS_INSTR_BNE(label, rt, rs) \
    mips_instr_branch_new(state, "bne", rt, rs, label)
#define MIPS_INSTR_BEQZ(label, rs) \
    MIPS_INSTR_BEQ(label, 0, rs)
#define MIPS_INSTR_BNEZ(label, rs) \
    MIPS_INSTR_BNE(label, 0, rs)
#define MIPS_INSTR_NOP() \
    mips_instr_new(state, "nop", 0, 0, 0, 0, 0, 0)
#define MIPS_INSTR_MOVE(rd, rt) \
    mips_instr_new(state, "addu", 0, 0, rd, rt, 0, 0)
static void mips_instr_new_store(parser_state_t* state, thecl_param_t* value);
static void instr_return_mips(parser_state_t* state, thecl_sub_t* sub);
static void instr_add(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_add_delay_slot(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_del(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_prepend(thecl_sub_t* sub, thecl_instr_t* instr);
/* Returns true if the created call was inline. */
static void instr_create_call(parser_state_t *state, uint8_t type, char *name, list_t *params);
static void instr_create_inline_call(parser_state_t *state, thecl_sub_t *sub, list_t *params);
static void instr_create_gool_ins(parser_state_t *state, const gool_ins_t *gool_ins, list_t *params);

static list_t* convert_expr_list_to_params(parser_state_t *state, list_t *expr_list);

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
#define EXPR_VAL(val) \
    expression_val_new(state, val)
#define EXPR_SP() \
    expression_load_new(state, param_sp_new())
#define EXPR_NULL() \
    expression_load_new(state, param_null_new())

static expression_t *expression_copy(expression_t *expr);
static void expression_create_goto(parser_state_t *state, int type, char *labelstr, expression_t* cond);
static void expression_create_goto_pop(parser_state_t *state, int type, char *labelstr, expression_t* cond, int pop);

/* macros for expression_mips_operation */
#define OutputExprToReg(EXPR, OP) \
    if (EXPR->type == EXPRESSION_VAL && EXPR->value->val_type == PARAM_LITERAL && EXPR->value->value.val.S == 0) OP = get_reg(state->reg_block, "zr"); else { expression_output(state, EXPR); OP = state->top_reg; }
#define SetUsedReg(OP) \
    if (OP->status != MREG_STATUS_RESERVED) OP->status = MREG_STATUS_USED;
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
/* Deletes an argument in the specified subroutine. */
static void arg_delete(parser_state_t* state, thecl_sub_t* sub, const char* name);
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
/* Compiles an assignment operation on a given variable */
static void var_assign(parser_state_t* state, thecl_param_t* param, expression_t* expr);
/* Compiles a shorthand assignment operation on a given variable */
static void var_shorthand_assign(parser_state_t* state, thecl_param_t* param, expression_t* expr, int EXPR);
/* Stores a new label in the current subroutine pointing to the current offset. */
static void label_create(parser_state_t* state, char* label);
/* Returns the spawn of the given name, or NULL if it doesn't exist */
static thecl_spawn_t* spawn_get(parser_state_t* state, const char* name);

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
static void anim_create_anim_c1(parser_state_t* state, char* name, uint16_t frames, int eid);
/* Appends a new Crash 2 vertex animation to the animations list. */
static void anim_create_anim_c2(parser_state_t* state, char* name, uint16_t frames, int eid, int interp);

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
%token <integer> ARRAY_NAME "array name"
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
%token MOD_MIPS "__mips"
%token MOD_TRANS "__trans"
%token MOD_ARGS "__args"
%token ONCE "once"
%token NOFIRST "nofirst"
%token EVENT "event"
%token INLINE "inline"
%token DEFAULT "default"
%token MIPS "mips"
%token BRACE_OPEN "{"
%token BRACE_CLOSE "}"
%token SQUARE_OPEN "["
%token SQUARE_CLOSE "]"
%token PARENTHESIS_OPEN "("
%token PARENTHESIS_CLOSE ")"
%token DEREFERENCE "->"
%token EXPRESSION "expr"
%token ARRAY "array"
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
%token SAVE "save"
%token EVHA "accev"
%token EVHR "rejev"
%token AT "@"
%token CALL
%token LOAD
%token GLOAD
%token PLOAD
%token CLOAD
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
%token PASSIGN
%token CASSIGN
%token ARRL "array load"
%token AVG "avg"
%token ABS "abs"
%token SEEK "seek"
%token DEGSEEK "degseek"
%token DEGDIST "degdist"
%token RAND "rand"
%token RANDI "randi"
%token LOOPSEEK "loopseek"
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
%token ATAN "atan"
%token ATAN2 "atan2"
%token ATAN2_3D "atan2_3d"
%token GETFIELD "getfield"
%token ATAN2O "atan2_obj"
%token OBJGET "objectget"
%token ENTITYSTATEGET "entitygetstate"
%token GAMEFUNC "gamefunc"
%token GETVALIDEVENTOBJ "getvalideventobj"
%token POINTCLIP "pointclip"
%token UNK2 "__unk2"
%token TRYLOAD "tryload"
%token GETANIM "getanim"
%token OFFSETOF "offsetof"
%token NTRY4 "ntry4"
%token NTRY5 "ntry5"

%type <list> Address_List
%type <list> Expression_List
%type <list> ParenExpressionList
%type <list> ParenExpressionListNoScope

%type <expression> Expression
%type <expression> ExpressionLoadType
%type <expression> ExpressionSubset

%type <param> Address
%type <param> Integer
%type <param> Entry
%type <param> Load_Type
%type <param> Pointer_Type

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
%right ADD SUBTRACT
%left MULTIPLY DIVIDE MODULO
%left UADD USUBTRACT
%precedence NOT B_NOT

%expect 0
%%

Statements:
    %empty
    | Statements Statement
    ;

Statement:
      "sub" IDENTIFIER Pre_Subroutine_Modifiers {
        sub_begin(state, $2);
        state->current_sub->is_inline = false;
        free($2);
      } "(" ArgumentDeclaration ")" Subroutine_Modifiers Subroutine_Body {
        if (state->current_sub->mod_trans) {
            for (int a=state->current_sub->mod_trans_count-1; a >= 0; --a) {
                objfield_delete(state, state->main_ecl->vars[state->main_ecl->var_count - 1]->name);
            }
        }
        sub_finish(state);
      }
    | "inline" "sub" IDENTIFIER {
        sub_begin(state, $3);
        state->current_sub->is_inline = true;
        if (state->ecl != state->main_ecl) {
            list_del_tail(&state->ecl->subs);
            list_append_new(&state->main_ecl->subs, state->current_sub);
        }
        free($3);
      } "(" ArgumentDeclaration ")" Subroutine_Modifiers Subroutine_Body {
        sub_finish(state);
      }
    | "state" IDENTIFIER {
        state_begin(state, $2);
        free($2);
      } State_Body {
        state_finish(state);
      }
    | "event" IDENTIFIER "=>" {
        const field_t* event = event_get(state->version, $2);
        if (!event) {
            yyerror(state, "unknown event: %s", $2);
            free($2);
            break;
        }

        state->current_interrupt = calloc(sizeof(thecl_interrupt_t), 1);
        state->current_interrupt->event = malloc(sizeof(field_t));
        memcpy(state->current_interrupt->event, event, sizeof(field_t));

        free($2);
      } Interrupt_Body {
        list_append_new(&state->main_ecl->interrupts, state->current_interrupt);

        state->current_interrupt = NULL;
      }
    | "expr" IDENTIFIER "=" Expression { /* expression macro */
        macro_create(state, $2, $4);
        free($2);
      }
    | "array" IDENTIFIER "=" { /* const array */
        state->current_array = malloc(sizeof(gooc_array_t));
        state->current_array->name = strdup($2);
        state->current_array->start = state->main_ecl->const_count;
        free($2);
      } "{" Array_Entries "}" {
        state->current_array->end = state->main_ecl->const_count;
        list_append_new(&state->main_ecl->arrays, state->current_array);
        state->current_array = NULL;
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
            gool_pool_force_get_index(state->main_ecl, eid);
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
    | DIRECTIVE {
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
      } Font_Chars {
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
      } Sprite_Frames {
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
      } String_List {
        list_append_new(&state->main_ecl->anims, state->current_anim);
        state->current_anim = NULL;
      }
    | DIRECTIVE_FANIM IDENTIFIER ENTRY INTEGER {
        gool_anim_t* anim = malloc(sizeof(gool_anim_t));
        anim->name = strdup($2);

        if (state->version == 1) {
            anim->size = sizeof(c1_fraganim_t);
            c1_fraganim_t* fraganim = malloc(sizeof(c1_fraganim_t));
            fraganim->type = 5;
            fraganim->sprite_count = 0;
            fraganim->frag_count = $4;
            fraganim->eid = gool_to_eid($3);
            anim->anim = fraganim;
        }
        else {
            anim->size = sizeof(c2_fraganim_t);
            c2_fraganim_t* fraganim = malloc(sizeof(c2_fraganim_t));
            fraganim->type = 5;
            fraganim->sprite_count = 0;
            fraganim->frag_count = $4;
            fraganim->eid = gool_to_eid($3);
            anim->anim = fraganim;
        }

        state->current_anim = anim;

        free($2);
        free($3);
      } Frags {
        if (state->version == 1) {
            c1_fraganim_t* fraganim = state->current_anim->anim;
            ++fraganim->sprite_count;
            fraganim->sprite_count /= fraganim->frag_count;
        }
        else {
            c2_fraganim_t* fraganim = state->current_anim->anim;
            ++fraganim->sprite_count;
            fraganim->sprite_count /= fraganim->frag_count;
        }

        list_append_new(&state->main_ecl->anims, state->current_anim);
        state->current_anim = NULL;
      }
    | GlobalVarDeclaration
    ;

Pre_Subroutine_Modifiers:
    %empty
    | Pre_Subroutine_Modifiers Pre_Subroutine_Modifier
    ;

Pre_Subroutine_Modifier:
      "__mips" {
        if (is_post_c2(state->version)) {
            state->stack_adjust = 0;
            state->mips_mode = true;
            state->force_mips = true;
        }
        else {
            yyerror(state, "mips mode is not supported for this game");
        }
      }
    ;

Subroutine_Modifiers:
    %empty
    | Subroutine_Modifiers Subroutine_Modifier
    ;

Subroutine_Modifier:
      "__trans" {
        state->current_sub->mod_trans_count = state->current_sub->arg_count;
        for (int i=0;i<state->current_sub->arg_count;++i)
            objfield_create(state, state->current_sub->args[i]->name);
        while (state->current_sub->arg_count)
            arg_delete(state, state->current_sub, state->current_sub->args[state->current_sub->arg_count-1]->name);
        state->current_sub->mod_trans = true;
        state->current_sub->is_trans = true;
      }
    | "__args" "(" ArgumentDeclaration ")"
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
      } "(" ArgumentDeclaration ")" Subroutine_Body {
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

Texture_Info:
      INTEGER[rgb] INTEGER[color] INTEGER[blend] INTEGER[clutx] INTEGER[cluty] INTEGER[x] INTEGER[y] INTEGER[w] INTEGER[h] {
        if (state->version == 1) {
            c1_tex_t* tex = state->current_tex;

            if ($x >= 128) {
                yyerror(state, "%s:texture x offset is out of bounds", state->current_anim->name);
            }
            int uv = 0;
            if (($w != 0 && $w != 4 && $w != 8 && $w != 16 && $w != 32 && $w != 64) ||
                ($h != 0 && $h != 4 && $h != 8 && $h != 16 && $h != 32 && $h != 64)) {
                yyerror(state, "%s:invalid texture width/height", state->current_anim->name);
            }
            const uint64_t zero = 0;
            if ($w == 0 && $w == 4) uv = 0;
            if ($w == 8) uv = 1;
            if ($w == 16) uv = 2;
            if ($w == 32) uv = 3;
            if ($w == 64) uv = 4;
            if ($h == 0 && $h == 4) uv += 0;
            if ($h == 8) uv += 5;
            if ($h == 16) uv += 10;
            if ($h == 32) uv += 15;
            if ($h == 64) uv += 20;
            tex->r = $rgb >> 0 & 0xFF;
            tex->g = $rgb >> 8 & 0xFF;
            tex->b = $rgb >> 16 & 0xFF;
            tex->color = $color & 0x3;
            tex->blend = $blend & 0x3;
            tex->cx = $clutx & 0xF;
            tex->cy = $cluty & 0x7F;
            tex->x = $x & 0x1F;
            tex->y = $y & 0x1F;
            tex->segment = ($x / 0x20) & 0x3;
            tex->uv = uv & 0x3FF;
            tex->unk1 = 0;
            tex->unk2 = 0;
            if (memcmp(tex, &zero, 8))
                tex->textured = 1;
        }
        else if (state->version == 2) {
            c2_tex_t* tex = state->current_tex;

            int x = $x;
            int y = $y;
            int w = $w;
            int h = $h;
            int segsize = 256 >> $color;
            if ((x & 0xff) + w > 256) {
                yyerror(state, "%s: aligned texture is too wide", state->current_anim->name);
            }
            if (y + h > 128) {
                yyerror(state, "%s: texture is too tall", state->current_anim->name);
            }
            if (y < 0 || x < 0) {
                yyerror(state, "%s: invalid texture parameters", state->current_anim->name);
            }
            --w;
            --h;
            tex->r = $rgb >> 0 & 0xFF;
            tex->g = $rgb >> 8 & 0xFF;
            tex->b = $rgb >> 16 & 0xFF;
            tex->primtype = $rgb == 0 ? 0 : 11;
            tex->unk1 = 0;
            tex->unk2 = 0;
            tex->unused1 = 0;
            tex->unk3 = 0;
            tex->additive = $blend >> 1 & 0x1;
            tex->unk4 = 0;
            tex->unk5 = 0;
            tex->segment = x / segsize;
            x &= segsize - 1;
            tex->color = $color;
            tex->blend = $blend & 0x1;
            tex->cx = $clutx;
            tex->cy = $cluty;
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
    | INTEGER[rgb] INTEGER[color] INTEGER[blend] INTEGER[clutx] INTEGER[cluty] INTEGER[x] INTEGER[y] INTEGER[w] INTEGER[h] "!" INTEGER[wind] INTEGER[flip] {
        if (state->version == 1) {
            c1_tex_t* tex = state->current_tex;

            if ($x >= 128) {
                yyerror(state, "%s:texture x offset is out of bounds", state->current_anim->name);
            }
            int uv = 0;
            if (($w != 0 && $w != 4 && $w != 8 && $w != 16 && $w != 32 && $w != 64) ||
                ($h != 0 && $h != 4 && $h != 8 && $h != 16 && $h != 32 && $h != 64)) {
                yyerror(state, "%s:invalid texture width/height", state->current_anim->name);
            }
            const uint64_t zero = 0;
            if ($w == 0 && $w == 4) uv = 0;
            if ($w == 8) uv = 1;
            if ($w == 16) uv = 2;
            if ($w == 32) uv = 3;
            if ($w == 64) uv = 4;
            if ($h == 0 && $h == 4) uv += 0;
            if ($h == 8) uv += 5;
            if ($h == 16) uv += 10;
            if ($h == 32) uv += 15;
            if ($h == 64) uv += 20;
            uv += ($wind % 6) * 25;
            uv += ($flip % 4) * 150;
            tex->r = $rgb >> 0 & 0xFF;
            tex->g = $rgb >> 8 & 0xFF;
            tex->b = $rgb >> 16 & 0xFF;
            tex->color = $color & 0x3;
            tex->blend = $blend & 0x3;
            tex->cx = $clutx & 0xF;
            tex->cy = $cluty & 0x7F;
            tex->x = $x & 0x1F;
            tex->y = $y & 0x1F;
            tex->segment = ($x / 0x20) & 0x3;
            tex->uv = uv & 0x3FF;
            tex->unk1 = 0;
            tex->unk2 = 0;
            if (memcmp(tex, &zero, 8))
                tex->textured = 1;
        }
        else if (state->version == 2) {
            c2_tex_t* tex = state->current_tex;

            int x = $x;
            int y = $y;
            int w = $w;
            int h = $h;
            int segsize = 256 >> $color;
            if ((x & 0xff) + w > 256) {
                yyerror(state, "%s: aligned texture is too wide", state->current_anim->name);
            }
            if (y + h > 128) {
                yyerror(state, "%s: texture is too tall", state->current_anim->name);
            }
            if (y < 0 || x < 0) {
                yyerror(state, "%s: invalid texture parameters", state->current_anim->name);
            }
            --w;
            --h;
            tex->r = $rgb >> 0 & 0xFF;
            tex->g = $rgb >> 8 & 0xFF;
            tex->b = $rgb >> 16 & 0xFF;
            tex->primtype = $rgb == 0 ? 0 : 11;
            tex->unk1 = 0;
            tex->unk2 = 0;
            tex->unused1 = 0;
            tex->unk3 = 0;
            tex->additive = $blend >> 1 & 0x1;
            tex->unk4 = 0;
            tex->unk5 = 0;
            tex->segment = x / segsize;
            x &= segsize - 1;
            tex->color = $color;
            tex->blend = $blend & 0x1;
            tex->cx = $clutx;
            tex->cy = $cluty;
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

Font_Chars:
    %empty
    | Font_Chars Font_Char
    ;

Font_Char:
      DIRECTIVE_CHAR {
        if (state->version == 1) {
            c1_font_t* font = state->current_anim->anim;
            state->current_anim->anim = font = realloc(font, sizeof(c1_font_t) + sizeof(c1_char_t) * ++font->char_count);
            state->current_anim->size = sizeof(c1_font_t) + sizeof(c1_char_t) * font->char_count;

            c1_char_t* character = font->chars + font->char_count - 1;
            state->current_tex = &character->tex;
        }
        else if (state->version == 2) {
            c2_font_t* font = state->current_anim->anim;
            state->current_anim->anim = font = realloc(font, sizeof(c2_font_t) + sizeof(c2_char_t) * ++font->char_count);
            state->current_anim->size = sizeof(c2_font_t) + sizeof(c2_char_t) * font->char_count;

            c2_char_t* character = font->chars + font->char_count - 1;
            state->current_tex = &character->tex;
        }
      } Texture_Info INTEGER[w] INTEGER[h] {
        if (state->version == 1) {
            c1_font_t* font = state->current_anim->anim;
            c1_char_t* character = font->chars + font->char_count - 1;
            character->w = $w;
            character->h = $h;
        } else if (state->version == 2) {
            c2_font_t* font = state->current_anim->anim;
            c2_char_t* character = font->chars + font->char_count - 1;
            character->w = $w;
            character->h = $h;
        }
      }
    ;

Sprite_Frames:
    %empty
    | Sprite_Frames Sprite_Frame
    ;

Sprite_Frame:
      DIRECTIVE_TEXTURE {
        if (state->version == 1) {
            c1_sprite_t* sprite = state->current_anim->anim;
            state->current_anim->anim = sprite = realloc(sprite, sizeof(c1_sprite_t) + sizeof(c1_tex_t) * ++sprite->count);
            state->current_anim->size = sizeof(c1_sprite_t) + sizeof(c1_tex_t) * sprite->count;

            c1_tex_t* tex = sprite->frames + sprite->count - 1;
            state->current_tex = tex;
        } else if (state->version == 2) {
            c2_sprite_t* sprite = state->current_anim->anim;
            state->current_anim->anim = sprite = realloc(sprite, sizeof(c2_sprite_t) + sizeof(c2_tex_t) * ++sprite->count);
            state->current_anim->size = sizeof(c2_sprite_t) + sizeof(c2_tex_t) * sprite->count;

            c2_tex_t* tex = sprite->frames + sprite->count - 1;
            state->current_tex = tex;
        }
      } Texture_Info
    ;

String_List:
    %empty
    | String_List TEXT {
        char* newstring = convert_extended_string(state->version, $2);
        size_t stringlen = strlen(newstring) + 1;

        state->current_anim->anim = realloc(state->current_anim->anim, state->current_anim->size + stringlen);

        c1_text_t* text = state->current_anim->anim;
        ++text->string_count;

        char *string = (char*)state->current_anim->anim + state->current_anim->size;
        strcpy(string, newstring);

        state->current_anim->size += stringlen;

        free($2);
        free(newstring);
      }
    ;

Frags:
    %empty
    | Frags Frag
    ;

Frag:
      DIRECTIVE_FRAG {
        if (state->version == 1) {
            state->current_anim->anim = realloc(state->current_anim->anim, state->current_anim->size + sizeof(c1_frag_t));
            c1_fraganim_t* fraganim = state->current_anim->anim;
            c1_frag_t* frag = (char*)fraganim + state->current_anim->size;
            state->current_tex = &frag->tex;
        }
        else if (state->version == 2) {
            state->current_anim->anim = realloc(state->current_anim->anim, state->current_anim->size + sizeof(c2_frag_t));
            c2_fraganim_t* fraganim = state->current_anim->anim;
            c2_frag_t* frag = (char*)fraganim + state->current_anim->size;
            state->current_tex = &frag->tex;
        }
      } Texture_Info Literal_Int[x] Literal_Int[y] Literal_Int[w] Literal_Int[h] {
        if (state->version == 1) {
            c1_fraganim_t* fraganim = state->current_anim->anim;
            c1_frag_t* frag = (char*)fraganim + state->current_anim->size;
            frag->x = $x;
            frag->y = $y;
            frag->w = $w;
            frag->h = $h;
            state->current_anim->size += sizeof(c1_frag_t);
            ++fraganim->sprite_count;
        }
        else if (state->version == 2) {
            c2_fraganim_t* fraganim = state->current_anim->anim;
            c2_frag_t* frag = (char*)fraganim + state->current_anim->size;
            frag->x = $x;
            frag->y = $y;
            frag->w = $w;
            frag->h = $h;
            state->current_anim->size += sizeof(c2_frag_t);
            ++fraganim->sprite_count;
        }
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
    | State_Instructions DIRECTIVE {
        free($2);
      }
    | State_Instructions "__transargs" {
        if (state->current_state->trans && state->current_state->event)
            yyerror(state, "useless modifier __transargs, trans and event blocks already defined");
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
    | State_Instructions "trans" "=>" {
        state->current_state->trans = calloc(sizeof(thecl_state_sub_t), 1);
        state->current_state_sub = state->current_state->trans;
      } State_Subroutine_Identifier {
        state->current_state_sub = NULL;
      }
    | State_Instructions "code" "=>" {
        state->current_state->code = calloc(sizeof(thecl_state_sub_t), 1);
        state->current_state_sub = state->current_state->code;
      } State_Subroutine_Identifier {
        state->current_state_sub = NULL;
      }
    | State_Instructions "event" "=>" {
        state->current_state->event = calloc(sizeof(thecl_state_sub_t), 1);
        state->current_state_sub = state->current_state->event;
      } State_Subroutine_Identifier {
        state->current_state_sub = NULL;
      }
    | State_Instructions "trans" Pre_Subroutine_Modifiers {
        if (state->current_state->trans) {
            yyerror(state, "duplicate trans block in state: %s", state->current_state->name);
            exit(2);
        }
        char buf[256];
        snprintf(buf, 256, "__%s_TRANS", state->current_state->name);
        sub_begin(state, buf);
        state->current_sub->is_inline = false;
        state->current_sub->is_trans = true;
        state->current_state->trans = calloc(sizeof(thecl_state_sub_t), 1);
        state->current_state->trans->lambda_name = strdup(buf);
        state->current_state->trans->type = INTERRUPT_SUB;

        if (state->current_state->trans_args) {
            if (!state->current_state->code) {
                yyerror(state, "cannot use trans args without code block first in state: %s", state->current_state->name);
                state->current_state->trans_args = false;
            }
            else {
                thecl_sub_t* sub_code = state->find_state_sub(state->main_ecl, state->ecl != state->main_ecl ? state->ecl : NULL, state->current_state->code, STATE_SUB_CODE);
                for (int a=0; a < sub_code->arg_count; ++a) {
                    objfield_create(state, sub_code->args[a]->name);
                }
            }
        }
      } Subroutine_Body {
        if (state->current_state->trans_args) {
            thecl_sub_t* sub_code = state->find_state_sub(state->main_ecl, state->ecl != state->main_ecl ? state->ecl : NULL, state->current_state->code, STATE_SUB_CODE);
            for (int a=sub_code->arg_count-1; a >= 0; --a) {
                objfield_delete(state, sub_code->args[a]->name);
            }
        }
        sub_finish(state);
      }
    | State_Instructions "code" Pre_Subroutine_Modifiers {
        if (state->current_state->code) {
            yyerror(state, "duplicate code block in state: %s", state->current_state->name);
            exit(2);
        }
        char buf[256];
        snprintf(buf, 256, "__%s_CODE", state->current_state->name);
        sub_begin(state, buf);
        state->current_sub->is_inline = false;
        state->current_state->code = calloc(sizeof(thecl_state_sub_t), 1);
        state->current_state->code->lambda_name = strdup(buf);
        state->current_state->code->type = INTERRUPT_SUB;
      }
      "(" ArgumentDeclaration ")" Subroutine_Body {
        sub_finish(state);
      }
    | State_Instructions "event" Pre_Subroutine_Modifiers {
        if (state->current_state->event) {
            yyerror(state, "duplicate event block in state: %s", state->current_state->name);
            exit(2);
        }
        char buf[256];
        snprintf(buf, 256, "__%s_EVENT", state->current_state->name);
        sub_begin(state, buf);
        state->current_sub->is_inline = false;
        state->current_state->event = calloc(sizeof(thecl_state_sub_t), 1);
        state->current_state->event->lambda_name = strdup(buf);
        state->current_state->event->type = INTERRUPT_SUB;

        if (state->current_state->trans_args) {
            if (!state->current_state->code) {
                yyerror(state, "cannot use trans args without code block first in state: %s", state->current_state->name);
                state->current_state->trans_args = false;
            }
            else {
                thecl_sub_t* sub_code = state->find_state_sub(state->main_ecl, state->ecl != state->main_ecl ? state->ecl : NULL, state->current_state->code, STATE_SUB_CODE);
                for (int a=0; a < sub_code->arg_count; ++a) {
                    objfield_create(state, sub_code->args[a]->name);
                }
            }
        }
      } "(" ArgumentDeclaration ")" Subroutine_Body {
        if (state->current_state->trans_args) {
            thecl_sub_t* sub_code = state->find_state_sub(state->main_ecl, state->ecl != state->main_ecl ? state->ecl : NULL, state->current_state->code, STATE_SUB_CODE);
            for (int a=sub_code->arg_count-1; a >= 0; --a) {
                objfield_delete(state, sub_code->args[a]->name);
            }
        }
        sub_finish(state);
      }
    ;

State_Subroutine_Identifier:
    "state" IDENTIFIER { state->current_state_sub->type = INTERRUPT_STATE; state->current_state_sub->lambda_name = strdup($2); free($2); }
    | "sub" IDENTIFIER { state->current_state_sub->type = INTERRUPT_SUB; state->current_state_sub->lambda_name = strdup($2); free($2); }
    ;

Instructions:
    %empty
    /*| Instructions IDENTIFIER ":" { label_create(state, $2); free($2); }*/
    | Instructions Instruction
    | Instructions Block
    ;

ParenExpressionList:
      "(" { scope_begin(state); } Expression_List[list] ")"
        { $$ = $list; }
    | "(" { scope_begin(state); } VarDeclaration ";" Expression_List[list] ")"
        { $$ = $list; }
    ;

ParenExpressionListNoScope:
      "(" Expression_List[list] ")"
        { $$ = $list; }
    ;

Block:
      IfBlock
    | WhileBlock
    | CodeBlock
    | OnceBlock
    | SaveBlock
    | MipsBlock
    ;

MipsBlock:
    "mips" {
        if (state->current_sub->is_inline) {
            yyerror(state, "mips mode is currently not supported for inline subs");
        }
        else {
            if (is_post_c2(state->version)) {
                if (state->mips_mode) {
                    yyerror(state, "invalid mips block");
                    exit(2);
                }
                state->stack_adjust = 0;
                state->mips_mode = true;
            }
            else {
                yyerror(state, "mips mode is not supported for this game");
            }
        }
      } CodeBlock {
        state->mips_mode = false;
      }
    ;

SaveBlock:
    "save" "(" Address_List ")" {
        if (state->current_sub->is_inline) {
            list_append_new(&state->current_sub->lines, line_make_save_start($3));
        }
        else {
            const expr_t* local_expr = expr_get_by_symbol(state->version, LOAD);
            const expr_t* global_expr = expr_get_by_symbol(state->version, GLOAD);
            const expr_t* color_expr = expr_get_by_symbol(state->version, CLOAD);
            if ($3 == NULL)
                $3 = list_new();
            list_node_t *n, *x;
            list_for_each_node_safe($3, n, x) {
                list_append(&state->addresses, n);
                thecl_param_t* param = n->data;
                switch (param->val_type) {
                    default: instr_add(state, state->current_sub, instr_new(state, local_expr->id, "p", param_copy(param))); break;
                    case PARAM_GLOBAL: instr_add(state, state->current_sub, instr_new(state, global_expr->id, "S", param->value.val.S)); break;
                    case PARAM_COLOR: instr_add(state, state->current_sub, instr_new(state, color_expr->id, "SS", param->object_link, param->value.val.S)); break;
                }
            }
            int count = list_count($3);
            state->current_sub->stack_offset += count;
            list_append_new(&state->addresses, count);
            free($3);
        }
      } CodeBlock {
        if (state->current_sub->is_inline) {
            list_append_new(&state->current_sub->lines, line_make_save_end());
        }
        else {
            int m = list_tail(&state->addresses);
            list_del_tail(&state->addresses);

            const expr_t* local_expr = expr_get_by_symbol(state->version, ASSIGN);
            const expr_t* global_expr = expr_get_by_symbol(state->version, GASSIGN);
            const expr_t* color_expr = expr_get_by_symbol(state->version, CASSIGN);
            for (int i=0; i<m; ++i) {
                thecl_param_t* param = list_tail(&state->addresses);
                switch (param->val_type) {
                    default: instr_add(state, state->current_sub, instr_new(state, local_expr->id, "pp", param_copy(param), param_sp_new())); break;
                    case PARAM_GLOBAL: instr_add(state, state->current_sub, instr_new(state, global_expr->id, "Sp", param->value.val.S, param_sp_new())); break;
                    case PARAM_COLOR: instr_add(state, state->current_sub, instr_new(state, color_expr->id, "pSS", param_sp_new(), param->object_link, param->value.val.S)); break;
                }
                param_free(param);
                list_del_tail(&state->addresses);
            }
            state->current_sub->stack_offset -= m;
        }
      }
    ;

OnceBlock:
    "once" {
        state->current_sub->has_once = true;
      } CodeBlock {
        var_assign(state, param_var_new("tpc"), expression_load_new(state, param_var_new("pc")));
      }
    | "nofirst" {
        state->current_sub->has_nofirst = true;

        var_assign(state, param_var_new("tpc"), EXPR_2(ADD, param_var_new("pc"), EXPR_VAL(4*2)));

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
    "{" { scope_begin(state); } Instructions "}" { scope_finish(state, true); }
    | Instruction ";"
    ;

IfBlock:
      "unless" ParenExpressionList[cond] {
        if ($cond == NULL) {
            yyerror(state, "warning: empty conditional");
            break;
        }
        char labelstr[256];
        snprintf(labelstr, 256, "unless_%i_%i", yylloc.first_line, yylloc.first_column);
        list_prepend_new(&state->block_stack, strdup(labelstr));
        expression_t* expr;
        list_for_each($cond, expr) {
            expression_create_goto(state, IF, labelstr, expr);
            expression_free(expr);
        }
        list_free_nodes($cond);
        free($cond);
      } CodeBlock ElseBlock {
        list_node_t *head = state->block_stack.head;
        label_create(state, head->data);
        state->block_stack.head = head->next;
        free(head->data);
        list_del(&state->block_stack, head);
        scope_finish(state, true);
      }
    | "if" ParenExpressionList[cond] {
          if ($cond == NULL) {
              yyerror(state, "warning: empty conditional");
              break;
          }
          char labelstr[256];
          snprintf(labelstr, 256, "if_%i_%i", yylloc.first_line, yylloc.first_column);
          list_prepend_new(&state->block_stack, strdup(labelstr));
          expression_t* expr;
          list_for_each($cond, expr) {
              expression_create_goto(state, UNLESS, labelstr, expr);
              expression_free(expr);
          }
          list_free_nodes($cond);
          free($cond);
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
      "while" ParenExpressionList[cond] {
        if ($cond == NULL) {
            yyerror(state, "warning: empty conditional");
            break;
        }
        expression_t* expr;
        list_node_t *node, *next;
        list_for_each_node_safe($cond, node, next) {
            expr = node->data;
            if (expr->type == EXPRESSION_VAL && expr->value->val_type == PARAM_LITERAL && !expr->value->value.val.S) {
                expression_free(expr);
                list_del($cond, node);
            }
            else {
                break;
            }
        }
          if (list_empty($cond)) {
            free($cond);
            ++state->ignore_block;
            break;
        }

        char labelstr[256];
        snprintf(labelstr, 256, "while_%i_%i", yylloc.first_line, yylloc.first_column);
        char labelstr_st[256];
        char labelstr_end[256];
        char labelstr_continue[256];
        snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
        snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);
        snprintf(labelstr_continue, 256, "%s_continue", (char*)labelstr);

        expression_create_goto(state, GOTO, labelstr_continue, NULL);
        label_create(state, labelstr_st);

        list_prepend_new(&state->block_stack, strdup(labelstr));
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
        expression_t* expr;
        if (list_count($cond) == 1) {
            expr = list_head($cond);
            expression_create_goto(state, IF, labelstr_st, expr);
            expression_free(expr);
        }
        else {
            list_for_each($cond, expr) {
                expression_create_goto(state, UNLESS, labelstr_end, expr);
                expression_free(expr);
            }
            expression_create_goto(state, GOTO, labelstr_st, NULL);
        }
        list_free_nodes($cond);
        label_create(state, labelstr_end);

        free(head->data);
        list_del(&state->block_stack, head);
        scope_finish(state, true);
      }
    | "until" ParenExpressionList[cond] {
        if ($cond == NULL) {
            yyerror(state, "warning: empty conditional");
            break;
        }
        expression_t* expr;
        list_node_t *node, *next;
        list_for_each_node_safe($cond, node, next) {
            expr = node->data;
            if (expr->type == EXPRESSION_VAL && expr->value->val_type == PARAM_LITERAL && expr->value->value.val.S) {
                expression_free(expr);
                list_del($cond, node);
            }
            else {
                break;
            }
        }
        if (list_empty($cond)) {
            free($cond);
            ++state->ignore_block;
            break;
        }

        char labelstr[256];
        snprintf(labelstr, 256, "until_%i_%i", yylloc.first_line, yylloc.first_column);
        char labelstr_st[256];
        char labelstr_end[256];
        char labelstr_continue[256];
        snprintf(labelstr_st, 256, "%s_st", (char*)labelstr);
        snprintf(labelstr_end, 256, "%s_end", (char*)labelstr);
        snprintf(labelstr_continue, 256, "%s_continue", (char*)labelstr);

        expression_create_goto(state, GOTO, labelstr_continue, NULL);
        label_create(state, labelstr_st);

        list_prepend_new(&state->block_stack, strdup(labelstr));
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
        expression_t* expr;
        if (list_count($cond) == 1) {
            expr = list_head($cond);
            expression_create_goto(state, UNLESS, labelstr_st, expr);
            expression_free(expr);
        }
        else {
            list_for_each($cond, expr) {
                expression_create_goto(state, IF, labelstr_end, expr);
                expression_free(expr);
            }
            expression_create_goto(state, GOTO, labelstr_st, NULL);
        }
        list_free_nodes($cond);
        label_create(state, labelstr_end);

        free(head->data);
        list_del(&state->block_stack, head);
        scope_finish(state, true);
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
    | "do" "(" { scope_begin(state); } VarDeclaration ")" {
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
      CodeBlock "while" ParenExpressionListNoScope[cond]  {
        if ($cond == NULL) {
            yyerror(state, "warning: empty conditional");
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
        expression_t* expr;
        if (list_count($cond) == 1) {
            expr = list_head($cond);
            expression_create_goto(state, IF, labelstr_st, expr);
            expression_free(expr);
        }
        else {
            list_for_each($cond, expr) {
                expression_create_goto(state, UNLESS, labelstr_end, expr);
                expression_free(expr);
            }
            expression_create_goto(state, GOTO, labelstr_st, NULL);
        }
        list_free_nodes($cond);
        label_create(state, labelstr_end);

        free(head->data);
        list_del(&state->block_stack, head);
      }
    | CodeBlock "until" ParenExpressionListNoScope[cond]  {
        if ($cond == NULL) {
            yyerror(state, "warning: empty conditional");
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
        expression_t* expr;
        if (list_count($cond) == 1) {
            expr = list_head($cond);
            expression_create_goto(state, UNLESS, labelstr_st, expr);
            expression_free(expr);
        }
        else {
            list_for_each($cond, expr) {
                expression_create_goto(state, IF, labelstr_end, expr);
                expression_free(expr);
            }
            expression_create_goto(state, GOTO, labelstr_st, NULL);
        }
        list_free_nodes($cond);
        label_create(state, labelstr_end);

        free(head->data);
        list_del(&state->block_stack, head);
      }
    ;

Instruction:
      IDENTIFIER "(" Expression_List ")" {
        bool free_expr = true;
        thecl_sub_t* sub = state->find_sub_overload(state->main_ecl, $1, $3 ? list_count($3) : 0);
        if (sub && sub != state->current_sub && sub->is_inline) {
            instr_create_inline_call(state, sub, $3);
        }
        else {
            if (state->current_sub->is_inline) {
                list_append_new(&state->current_sub->lines, line_make_call($1, $3));
                free_expr = false;
            }
            else {
                const gool_ins_t* gool_ins = gool_ins_get_by_name(state->version, $1);
                if (gool_ins) {
                    instr_create_gool_ins(state, gool_ins, $3);
                }
                else {
                    instr_create_call(state, expr_get_by_symbol(state->version, CALL)->id, strdup($1), $3);
                }
            }
        }
        if ($3 && free_expr) {
            expression_t* expr;
            list_for_each($3, expr) {
                expression_free(expr);
            }
            list_free_nodes($3);
            free($3);
        }
        free($1);
      }
    /*| "goto" IDENTIFIER {
        expression_create_goto(state, GOTO, $2, NULL);
    }*/
    | Assignment
    | VarDeclaration
    | "break" {
        if (state->current_sub->is_inline) {
            list_append_new(&state->current_sub->lines, line_make(LINE_BREAK));
        }
        else {
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
      }
    | "continue" {
        if (state->current_sub->is_inline) {
            list_append_new(&state->current_sub->lines, line_make(LINE_CONTINUE));
        }
        else {
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
      }
    | "return" {
        if (state->current_sub->is_inline) {
            list_append_new(&state->current_sub->lines, line_make(LINE_RETURN));
        }
        else {
            /*
            int pop = 0;
            for (int v=0; v < state->current_sub->var_count; ++v)
                for (int s=1; s < state->scope_cnt; ++s)
                    if (state->current_sub->vars[v]->scope == state->scope_stack[s])
                        ++pop;
            if (pop > 0) {
                char buf[512];
                snprintf(buf, 512, "@%s_sub_end", state->current_sub->name);
                expression_create_goto_pop(state, GOTO, buf, NULL, pop);
            }
            else */{
                if (state->scope_stack[state->scope_cnt-1].mips) {
                    instr_return_mips(state, state->current_sub);
                }
                else {
                    instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, RETURN)->id, "SSSSS", 0, 0, 0x25, 0, 2));
                }
            }
            //state->scope_stack[state->scope_cnt-1].returned = true;
        }
      }
    ;

Assignment:
      Address "=" Expression {
        if ($1->type == 'o') {
            yyerror(state, "invalid address: %s", $1->value.val.z);
            return 0;
        }
        var_assign(state, $1, $3);
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
    | Address ">>=" Expression { var_shorthand_assign(state, $1, $3, RSHIFT); }
    | Address "\\=" Expression { var_shorthand_assign(state, $1, $3, TEST); }
    ;

Address_List:
    %empty { $$ = NULL; }
    | Address { $$ = list_new(); list_append_new($$, $1); }
    | Address_List "," Address { $$ = $1; list_append_new($$, $3); }
    ;

Expression_List:
    %empty { $$ = NULL; }
    | Expression { $$ = list_new(); list_append_new($$, $1); }
    | Expression_List "," Expression { $$ = $1; list_append_new($$, $3); }
    ;

Expression:
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
    |          "(" Expression ")" { $$ = $2; }
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
    | "!" Expression              { $$ = EXPR_2(NOT,      EXPR_SP(), $2); }
    | "~" Expression              { $$ = EXPR_2(B_NOT,    EXPR_SP(), $2); }
    | Expression "||"  Expression { $$ = EXPR_2(OR,       $1, $3); }
    | Expression "&&"  Expression { $$ = EXPR_2(AND,      $1, $3); }
    | Expression "^"   Expression { $$ = EXPR_2(XOR,      $1, $3); }
    | Expression "|"   Expression { $$ = EXPR_2(B_OR,     $1, $3); }
    | Expression "&"   Expression { $$ = EXPR_2(B_AND,    $1, $3); }
    | Expression "<<"  Expression { $$ = EXPR_2(LSHIFT,   $1, $3); }
    | Expression ">>"  Expression { $$ = EXPR_2(RSHIFT,   $1, $3); }
    | Expression "\\"  Expression { $$ = EXPR_2(TEST,     $1, $3); }
    | Expression "!="  Expression {
        $$ = EXPR_2(EQUAL,  $1, $3);
        $$ = EXPR_2(NOT,      EXPR_SP(), $$);
      }
    | "+" Expression %prec UADD      { $$ = $2; }
    | "-" Expression %prec USUBTRACT { $$ = EXPR_2(SUBTRACT, EXPR_VAL(0), $2); }
    | "abs" "(" Expression ")"    { $$ = EXPR_2(ABS,      EXPR_SP(), $3); }
    | "getanim" "(" Expression ")"{ $$ = EXPR_2(GETANIM,  EXPR_SP(), EXPR_2(LSHIFT, $3, EXPR_VAL(8))); }
    | "seek" "(" Expression "," Expression "," Expression ")"     { $$ = EXPR_3(SEEK, $3, $5, $7); }
    | "seek" "(" Expression "," Expression ")"                    { $$ = EXPR_2(SEEK, $3, $5); }
    | "degseek" "(" Expression "," Expression "," Expression ")"  { $$ = EXPR_3(DEGSEEK, $3, $5, $7); }
    | "degseek" "(" Expression "," Expression ")"                 { $$ = EXPR_2(DEGSEEK, $3, $5); }
    | "degdist" "(" Expression "," Expression ")"                 { $$ = EXPR_2(DEGDIST, $3, $5); }
    | "rand" "(" Expression ")"                                   { $$ = EXPR_2(RAND, EXPR_VAL(0), $3); }
    | "rand" "(" Expression "," Expression ")"                    { $$ = EXPR_2(RAND, $3, $5); }
    | "randi" "(" Expression  ")"                                 { $$ = EXPR_2(RAND, EXPR_VAL(0), EXPR_2(ADD, $3, EXPR_VAL(1))); }
    | "randi" "(" Expression "," Expression ")"                   { $$ = EXPR_2(RAND, $3,          EXPR_2(ADD, $5, EXPR_VAL(1))); }
    | "loopseek" "(" Address "," Expression "," Expression ")"    { $$ = EXPR_3(LOOPSEEK, expression_load_new(state, $3), $5, $7); }
    | "loopseek" "(" Address "," Expression ")"                   { $$ = EXPR_2(LOOPSEEK, expression_load_new(state, $3), $5); }
    | "time" "(" Expression "," Expression ")"                    { $$ = EXPR_2(TIME, $3, $5); }
    | "time" "(" Expression ")"                                   { $$ = EXPR_2(TIME, $3, EXPR_VAL(0)); }
    | "getcolor" "(" Expression "," Expression ")"                { if (g_warn_deprecate_getcolor) { yyerror(state, "getcolor: deprecate expression. use as address instead (i.e. 'color + 64')"); g_warn_deprecate_getcolor = false; }; $$ = EXPR_2(GETCOLOR, $3, $5); }
    | "getcolor" "(" Expression ")"                               { if (g_warn_deprecate_getcolor) { yyerror(state, "getcolor: deprecate expression. use as address instead (i.e. 'color + 64')"); g_warn_deprecate_getcolor = false; }; $$ = EXPR_2(GETCOLOR, EXPR_VAL(0), $3); }
    | "pad" "(" Expression "," Expression "," Expression "," Expression "," Expression ")" { $$ = EXPR_5(PAD, $3, $5, $7, $9, $11); }
    | "buttonpress" "(" Expression ")"                            { $$ = EXPR_5(PAD, $3, EXPR_VAL(1), EXPR_VAL(0), EXPR_VAL(8), EXPR_VAL(0)); }
    | "buttonhold" "(" Expression ")"                             { $$ = EXPR_5(PAD, $3, EXPR_VAL(2), EXPR_VAL(0), EXPR_VAL(8), EXPR_VAL(0)); }
    | "buttonbuffer" "(" Expression ")"                           { $$ = EXPR_5(PAD, $3, EXPR_VAL(3), EXPR_VAL(0), EXPR_VAL(8), EXPR_VAL(0)); }
    | "buttonpress" "(" Expression "," Expression ")"             { $$ = EXPR_5(PAD, $3, EXPR_VAL(1), EXPR_VAL(0), EXPR_VAL(8), $5); }
    | "buttonhold" "(" Expression "," Expression ")"              { $$ = EXPR_5(PAD, $3, EXPR_VAL(2), EXPR_VAL(0), EXPR_VAL(8), $5); }
    | "buttonbuffer" "(" Expression "," Expression ")"            { $$ = EXPR_5(PAD, $3, EXPR_VAL(3), EXPR_VAL(0), EXPR_VAL(8), $5); }
    | "dirpress" "(" Expression ")"                               { $$ = EXPR_5(PAD, EXPR_VAL(0), EXPR_VAL(0), EXPR_VAL(1), $3, EXPR_VAL(0)); }
    | "dirhold" "(" Expression ")"                                { $$ = EXPR_5(PAD, EXPR_VAL(0), EXPR_VAL(0), EXPR_VAL(2), $3, EXPR_VAL(0)); }
    | "dirbuffer" "(" Expression ")"                              { $$ = EXPR_5(PAD, EXPR_VAL(0), EXPR_VAL(0), EXPR_VAL(3), $3, EXPR_VAL(0)); }
    | "dirpress" "(" Expression "," Expression ")"                { $$ = EXPR_5(PAD, EXPR_VAL(0), EXPR_VAL(0), EXPR_VAL(1), $3, $5); }
    | "dirhold" "(" Expression "," Expression ")"                 { $$ = EXPR_5(PAD, EXPR_VAL(0), EXPR_VAL(0), EXPR_VAL(2), $3, $5); }
    | "dirbuffer" "(" Expression "," Expression ")"               { $$ = EXPR_5(PAD, EXPR_VAL(0), EXPR_VAL(0), EXPR_VAL(3), $3, $5); }
    | "spd" "(" Expression "," Expression ")"                     { $$ = EXPR_2(SPD, $3, $5); }
    | "spd" "(" Expression ")"                                    { $$ = EXPR_2(SPD, EXPR_VAL(0), $3); }
    | "sin" "(" Expression "," Expression ")"                     { $$ = EXPR_2(PSIN,$3, $5); }
    | "sin" "(" Expression ")"                                    { $$ = EXPR_2(SIN, EXPR_SP(), $3); }
    | "cos" "(" Expression ")"                                    { $$ = EXPR_2(COS, EXPR_SP(), $3); }
    | "fieldval" "(" Expression ")"                               { $$ = EXPR_2(FVAL,EXPR_SP(), $3); }
    | "fieldrow" "(" Expression "," Expression ")"                { $$ = EXPR_2(FROW,$3, $5); }
    | Address "[" Expression "]"                                  { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, expression_load_new(state, $1), EXPR_VAL(5), $3, EXPR_VAL(0));
                                                                    else $$ = EXPR_2(ARRL, expression_load_new(state, $1), $3);
                                                                  }
    | "getval" "(" Expression "," Expression ")"                  { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, EXPR_VAL(5), $5, EXPR_VAL(0));
                                                                    else $$ = EXPR_2(ARRL, expression_load_new(state, $3), $5);
                                                                  }
    | "distance" "(" Expression ")"                               { $$ = EXPR_4(MISC, EXPR_NULL(), $3, EXPR_VAL(0), EXPR_VAL(1)); }
    | "distance" "(" Expression "," Expression ")"                { $$ = EXPR_4(MISC, EXPR_NULL(), $3, $5, EXPR_VAL(1)); }
    | "atan" "(" Expression "," Expression ")"                    { $$ = EXPR_2(ATAN, $3, $5); }
    | "atan2" "(" Expression "," Expression ")"                   { $$ = EXPR_4(MISC, $5, $3, EXPR_VAL(0), EXPR_VAL(2)); }
    | "atan2" "(" Expression ")"                                  { $$ = EXPR_4(MISC, $3, EXPR_VAL(0), EXPR_VAL(0), EXPR_VAL(2)); }
    | "getfield" "(" Expression "," Expression ")" {
        if (expression_is_number($3) && g_warn_deprecate_setfield) {
            fprintf(stderr, "%s:%s:getfield: deprecate use of literals as field offset. use as address instead (i.e. 'parent->ObjVar1')\n", argv0, current_input);
            g_warn_deprecate_setfield = false;
        }
        $$ = EXPR_4(MISC, $5, $3, EXPR_VAL(0), EXPR_VAL(3));
      }
    | "atan2_obj" "(" Expression ")"                              { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, EXPR_NULL(), $3, EXPR_VAL(0), EXPR_VAL(5)); }
    | "distance" "(" Expression "," Expression "," Expression ")" { $$ = EXPR_4(MISC, $3, $5, $7, EXPR_VAL(6)); }
    | "objectget" "(" Expression ")"                              { $$ = EXPR_4(MISC, $3, EXPR_VAL(5), EXPR_VAL(0), EXPR_VAL(7)); }

    | "entitygetstate" "(" Expression ")"                         { $$ = EXPR_4(MISC, expression_load_new(state, param_var_new("id")), EXPR_VAL(0), $3, EXPR_VAL(11)); }
    | "entitygetstate" "(" Expression "," Expression ")"          { $$ = EXPR_4(MISC, $3, EXPR_VAL(0), $5, EXPR_VAL(11)); }
//  | "gamefunc" "(" Expression "," Expression ")"                { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, EXPR_VAL(0), $5, EXPR_VAL(12)); }
    | "atan2_3d" "(" Expression "," Expression ")"                { $$ = EXPR_4(MISC, $5, $3, EXPR_VAL(8), EXPR_VAL(12)); }
    | "getvalideventobj" "(" Expression "," Expression "," Expression ")"   { $$ = EXPR_4(MISC, $3, $5, $7, EXPR_VAL(13)); }
    | "getvalideventobj" "(" Expression "," Expression ")"        { $$ = EXPR_4(MISC, $3, EXPR_VAL(0), $5, EXPR_VAL(13)); }
    | "pointclip" "(" Expression "," Expression ")"               { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, $5, EXPR_VAL(0), EXPR_VAL(14)); }
//  | "__unk2" "(" Expression "," Expression ")"                  { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, EXPR_VAL(0), $5, EXPR_VAL(15)); }

    | "tryload" "(" Expression ")"                                { $$ = EXPR_2(NTRY, $3, EXPR_VAL(3)); }
    | "ntry5" "(" Expression_List ")" {
        if ($3 != NULL) {
            $$ = EXPR_2(NTRY, EXPR_VAL(list_count($3)), EXPR_VAL(5));
            expression_t* expr;
            list_for_each($3, expr) {
                list_append_new(&$$->children, expr);
            }
            list_free_nodes($3);
            free($3);
        }
        else {
            $$ = EXPR_2(NTRY, EXPR_VAL(0), EXPR_VAL(5));
        }
      }
    | "ntry4" "(" ")"                                             { $$ = EXPR_2(NTRY, EXPR_NULL(), EXPR_VAL(4)); }

    | "getins" "(" Expression ")"                                 { $$ = EXPR_3(MOVC, $3, EXPR_VAL(0), EXPR_VAL(0x1F)); }

    /* Custom expressions. */

    | "avg" "(" Expression "," Expression ")"                     { $$ = EXPR_2(RSHIFT, EXPR_2(ADD, $3, $5), EXPR_VAL(1)); }
    | Expression "?" Expression ":" Expression  %prec QUESTION    { $$ = expression_ternary_new(state, $1, $3, $5); }
    | "offsetof" "(" Expression ")"                               {
        if ($3->type != EXPRESSION_VAL) {
            yyerror(state, "syntax error, offsetof parameter must be value expression");
            exit(2);
        }
        if (!($3->value->val_type == PARAM_FIELD && $3->value->object_link >= 0 && $3->value->object_link <= 7)) {
            yyerror(state, "syntax error, offsetof parameter is an invalid expression");
            exit(2);
        }
        $$ = EXPR_VAL($3->value->value.val.S << 8);
      }
    ;

Address:
      IDENTIFIER {
        thecl_variable_t* arg;
        thecl_spawn_t* spawn;
        const field_t* field;
        size_t anim_offset;
        if (var_exists(state, state->current_sub, $1)) {
            $$ = param_new('S');
            $$->val_type = PARAM_FIELD;
            $$->value.val.S = var_stack(state, state->current_sub, $1) + state->current_sub->stack_offset;
        } else if (arg = arg_get(state, state->current_sub, $1)) {
            $$ = param_new('S');
            $$->val_type = PARAM_FIELD;
            $$->value.val.S = arg->stack;
        } else if (field = global_get(state->version, $1)) {
            $$ = param_new('S');
            $$->val_type = PARAM_GLOBAL;
            $$->value.val.S = field->offset << 8;
        } else if (field = color_get(state->version, $1)) {
            $$ = param_new('S');
            $$->val_type = PARAM_COLOR;
            $$->value.val.S = field->offset;
            $$->object_link = 0;
        } else if (field = field_get($1)) {
            $$ = param_new('S');
            $$->val_type = PARAM_FIELD;
            $$->value.val.S = field->offset;
            $$->object_link = 0;
        } else if (field = event_get(state->version, $1)) {
            $$ = param_new('S');
            $$->value.val.S = field->offset << 8;
        } else if (field = objfield_get(state, $1)) {
            $$ = param_new('S');
            $$->val_type = PARAM_FIELD;
            $$->value.val.S = field->offset + 64;
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
        const field_t* color = NULL, *objfield = NULL;
        const field_t* link = field_get($1);
        const field_t* field = field_get($3);
        if (field == NULL) objfield = field = objfield_get(state, $3);
        if (field == NULL) color = field = color_get(state->version, $3);
        if (link == NULL) {
            yyerror(state, "object link not found: %s", $1);
            free($1);
            free($3);
            return 1;
        }
        if (field == NULL) {
            yyerror(state, "object field/color not found: %s", $3);
            free($1);
            free($3);
            return 1;
        }
        if (link->offset >= 8 || link->offset < 0) {
            yyerror(state, "invalid object link: %s", $1);
        }
        int field_off = 0, field_type = PARAM_FIELD;
        if (field == objfield) { field_off = 64; }
        else if (field == color) { field_type = PARAM_COLOR; }
        $$ = param_new('S');
        $$->val_type = field_type;
        $$->value.val.S = field->offset + field_off;
        $$->object_link = link->offset;
        free($1);
        free($3);
      }
    | NIL {
        $$ = param_new('S');
        $$->val_type = PARAM_FIELD;
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
        free($1);
      }
    ;

Load_Type:
      Address
    | Integer
    | Entry
    ;

Pointer_Type:
      "&" Load_Type {
        $$ = $2;
        $$->val_type = PARAM_POINTER;
      }
    | ARRAY_NAME {
        $$ = param_new('S');
        $$->val_type = PARAM_POINTER;
        $$->value.val.S = $1;
        $$->object_link = -3;
      }
    ;

Literal_Int:
      INTEGER
    | "-" INTEGER { $$ = -$2; }
    | "+" INTEGER { $$ = +$2; }
    ;

Array_Entries:
    %empty {
        yyerror(state, "array '%s' has no values", state->current_array->name);
      }
    | Array_Entry
    | Array_Entries "," Array_Entry
    ;

Array_Entry:
      Literal_Int { gool_pool_force_make_index(state->main_ecl, $1); }
    | ENTRY { gool_pool_force_make_index(state->main_ecl, gool_to_eid($1)); free($1); }
    ;
%%

static bool
int_has_bit(
    uint32_t v)
{
    for (int i=0; i<32; ++i) {
        if (v == (1 << i)) return true;
    }
    return false;
}

static bool
var_is_valid_field_ref(
    parser_state_t* state,
    thecl_param_t* param)
{
    return param->val_type == PARAM_FIELD && ((param->object_link == 0 && param->value.val.S <= 0x1FF && param->value.val.S >= 0) || (param->object_link >= 1 && param->object_link <= 7 && param->value.val.S <= 0x3F && param->value.val.S >= 0) || param->object_link == -1 || param->object_link == -2 || param->object_link == -3);
}

static thecl_instr_t*
instr_init(
    parser_state_t* state)
{
    thecl_instr_t* instr = thecl_instr_new();
    instr->string = NULL;
    return instr;
}

static thecl_instr_t*
mips_instr_new(
    parser_state_t* state,
    const char* name,
    int imm,
    int shamt,
    int rd,
    int rt,
    int rs,
    int addr)
{
    thecl_instr_t* instr = instr_init(state);
    instr->mips = true;
    instr->ins.ins = mips_instr_init(name, imm, shamt, rd, rt, rs, addr);
    uint64_t regs = mips_instr_getregs(name, &instr->ins);
    instr->reg_used = regs & 0xFFFFFFFF;
    instr->reg_stalled = regs >> 32 & 0xFFFFFFFF;

    return instr;
}

static thecl_instr_t*
mips_instr_branch_new(
    parser_state_t* state,
    const char* name,
    int rt,
    int rs,
    const char* label)
{
    thecl_instr_t* instr = mips_instr_new(state, name, 0, 0, 0, rt, rs, 0);
    instr->string = label;

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
mips_stack_adjust(
    parser_state_t* state,
    thecl_sub_t* sub)
{
    if (state->stack_adjust != 0) {
        instr_add(state, state->current_sub, MIPS_INSTR_I("addiu", state->stack_adjust, get_reg(state->reg_block, "s6")->index, get_reg(state->reg_block, "s6")->index));
        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", field_get("sp")->offset * 4 + get_obj_proc_offset(state->version), get_reg(state->reg_block, "s6")->index, get_reg(state->reg_block, "s0")->index));
        state->stack_adjust = 0;
    }
}

static void
kill_delay_slots(
    parser_state_t* state,
    thecl_sub_t* sub)
{
    thecl_instr_t* last_ins = sub ? list_tail(&sub->instrs) : NULL;
    gooc_delay_slot_t* slot;
    list_for_each(&state->delay_slots, slot) {
        thecl_instr_t* ins = slot->slot->data;
        if (slot->optional && sub) {
            for (int i=0; i<32; ++i) {
                if (state->reg_block->regs[i].last_used >= ins->offset) {
                    --state->reg_block->regs[i].last_used;
                }
            }
            instr_del(state, sub, ins);
        }
        else if (last_ins == ins) {
            instr_del(state, sub, ins);
        }
        free(slot);
    }
    list_free_nodes(&state->delay_slots);
}

static gooc_delay_slot_t*
make_delay_slot(
    list_node_t* slot,
    thecl_instr_t* owner)
{
    gooc_delay_slot_t* delay_slot = calloc(1, sizeof(gooc_delay_slot_t));
    delay_slot->slot = slot;
    delay_slot->owner = owner;
    delay_slot->optional = false;
    return delay_slot;
}

static void
make_optional_delay_slots(
    parser_state_t* state,
    int count,
    thecl_instr_t* owner)
{
    for (int i=0; i<count; ++i) {
        instr_add(state, state->current_sub, MIPS_INSTR_NOP()); /* DELAY SLOT */
        gooc_delay_slot_t* delay_slot = calloc(1, sizeof(gooc_delay_slot_t));
        delay_slot->slot = state->current_sub->instrs.tail;
        delay_slot->owner = owner;
        delay_slot->optional = true;
        list_append_new(&state->delay_slots, delay_slot);
    }
}

static void
instr_start_mips(
    parser_state_t* state,
    thecl_sub_t* sub)
{
    kill_delay_slots(state, sub);
    instr_add(state, sub, instr_new(state, 73, ""));
    sub->last_ins = NULL;
    sub->secondlast_ins = NULL;
    state->scope_bound = sub->offset;
}

static void
instr_end_mips(
    parser_state_t* state,
    thecl_sub_t* sub)
{
    mips_stack_adjust(state, sub);
    kill_delay_slots(state, sub);
    instr_add(state, sub, MIPS_INSTR_JALR(get_reg(state->reg_block, "s5")->index, get_reg(state->reg_block, "ra")->index));
    kill_delay_slots(state, sub);
    instr_add(state, sub, MIPS_INSTR_NOP());
    clean_regs(state->reg_block);
    sub->last_ins = NULL;
    sub->secondlast_ins = NULL;
}

static void
instr_return_mips(
    parser_state_t* state,
    thecl_sub_t* sub)
{
    mips_stack_adjust(state, sub);
    kill_delay_slots(state, sub);
    instr_add(state, sub, MIPS_INSTR_JR(get_reg(state->reg_block, "ra")->index));
    kill_delay_slots(state, sub);
    instr_add(state, sub, MIPS_INSTR_I("ori", 0, get_reg(state->reg_block, "s5")->index, 0));
    clean_regs(state->reg_block);
    sub->last_ins = NULL;
    sub->secondlast_ins = NULL;
}

static void
instr_add_delay_slot(
    parser_state_t* state,
    thecl_sub_t* sub,
    thecl_instr_t* instr)
{
    instr_add(state, sub, instr);
    if (instr->offset == sub->offset - 1) {
        instr_add(state, state->current_sub, MIPS_INSTR_NOP()); /* DELAY SLOT */
        list_append_new(&state->delay_slots, make_delay_slot(sub->instrs.tail, instr));
    }
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
    if (!sub->mips_dirty && ((sub->offset > 0 && state->scope_cnt > 0 && instr->mips != state->scope_stack[state->scope_cnt-1].mips) || (instr->mips && state->force_mips))) {
        sub->mips_dirty = true;
        if (instr->mips) {
            instr_start_mips(state, sub);
        }
        else {
            instr_end_mips(state, sub);
        }
        if (state->force_mips) state->force_mips = false;
        sub->mips_dirty = false;
    }
    if (state->scope_cnt > 0) state->scope_stack[state->scope_cnt-1].mips = instr->mips;
    if (instr->mips) {
        if (instr->ins.ins != 0) {
            if (mips_instr_is_multdiv(&instr->ins)) {
                list_node_t* alu_node, *alu_next;
                list_for_each_node_safe(&state->delay_slots, alu_node, alu_next) {
                    gooc_delay_slot_t* alu_slot = alu_node->data;
                    if (alu_slot->optional) {
                        instr_del(state, sub, alu_slot->slot->data);
                        list_del(&state->delay_slots, alu_node);
                        free(alu_slot);
                    }
                }
            }
            bool ret = false;
            thecl_instr_t* tail_ins = list_tail(&sub->instrs);
            list_node_t* node;
            list_for_each_node(&state->delay_slots, node) {
                gooc_delay_slot_t* delay_slot = node->data;
                thecl_instr_t* slot_ins = delay_slot->slot->data;
                if (slot_ins != tail_ins && mips_instr_is_branch(&instr->ins)) continue;
                if ((delay_slot->optional || slot_ins->offset <= sub->multdiv_offset) && mips_instr_is_hilo(&instr->ins)) continue;
                if (delay_slot && (delay_slot->owner->reg_stalled & instr->reg_used) == 0) {
                    for (int i=0; i<32; ++i) {
                        if ((1 << i) & instr->reg_used && slot_ins->offset < state->reg_block->regs[i].last_used) {
                            slot_ins = NULL;
                            break;
                        }
                    }
                    if (slot_ins) {
                        instr->offset = slot_ins->offset;
                        thecl_instr_free(slot_ins);
                        delay_slot->slot->data = instr;
                        if (delay_slot->optional && instr->reg_stalled) {
                            int i = 1;
                            list_node_t* alu_node, *alu_next;
                            list_for_each_node_safe(&state->delay_slots, alu_node, alu_next) {
                                if (node == alu_node) continue;
                                if (i >= 6) break;
                                gooc_delay_slot_t* alu_slot = alu_node->data;
                                if (alu_slot->optional && alu_slot->slot_id == delay_slot->slot_id) {
                                    instr_del(state, sub, alu_slot->slot->data);
                                    list_del(&state->delay_slots, alu_node);
                                    free(alu_slot);
                                    i += 1;
                                }
                            }
                        }
                        list_del(&state->delay_slots, node);
                        free(delay_slot);
                        ret = true;
                        break;
                    }
                }
            }
            if (!ret && tail_ins && tail_ins->mips && (instr->reg_used & tail_ins->reg_stalled || ((mips_instr_is_branch(&instr->ins) || mips_instr_is_store(&instr->ins)) && mips_instr_is_branch(&tail_ins->ins)))) {
                instr_add(state, state->current_sub, MIPS_INSTR_NOP()); /* DELAY SLOT */
                list_append_new(&state->delay_slots, make_delay_slot(sub->instrs.tail, instr));
            }
            if (ret) {
                sub->last_ins = instr;
                for (int i=0; i<32; ++i) {
                    if (instr->reg_used & (1 << i)) {
                        state->reg_block->regs[i].last_used = instr->offset;
                    }
                }
                return;
            }
        }
        goto NO_OPTIM;
    }
    const expr_t* load_expr = expr_get_by_symbol(state->version, LOAD);
    const expr_t* ptr_expr = expr_get_by_symbol(state->version, PLOAD);
    if (instr->id == ptr_expr->id) {
        load_expr = ptr_expr;
    }
    if (state->block_bound) {
        if (instr->id == load_expr->id && instr->param_count == 1) {
            thecl_param_t* load_param = list_head(&instr->params);
            if (load_param->value.val.S == 0x1f && load_param->object_link == 0 && load_param->val_type == PARAM_FIELD) {
                thecl_instr_free(instr);
                return;
            }
        }
        state->block_bound = 0;
        goto NO_OPTIM;
    }
    const expr_t* bra_expr = expr_get_by_symbol(state->version, GOTO);
    const expr_t* beqz_expr = expr_get_by_symbol(state->version, UNLESS);
    const expr_t* bnez_expr = expr_get_by_symbol(state->version, IF);
    /* push optimization */
    if (instr->id == load_expr->id) {
        if (instr->param_count == 1) {
            thecl_param_t* load_param = list_head(&instr->params);
            if (load_param->value.val.S == 0x1f && load_param->object_link == 0 && load_param->val_type == PARAM_FIELD) {
                thecl_instr_free(instr);
                return;
            }
        }
        if (sub->last_ins != NULL) {
            thecl_label_t* tmp_label;
            list_for_each(&sub->labels, tmp_label) {
                if (tmp_label->offset == sub->offset)
                    goto NO_OPTIM;
            }

            while (sub->last_ins->id == instr->id && sub->last_ins->param_count < 2 && instr->param_count > 0) {
                ++sub->last_ins->param_count;
                list_append_new(&sub->last_ins->params, list_head(&instr->params));
                list_del_head(&instr->params);
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
        (is_post_c2(state->version) ||
        (!is_post_c2(state->version) &&
        ((thecl_param_t*)instr->params.tail->data)->value.val.S == 0 &&
        ((thecl_param_t*)instr->params.tail->prev->data)->value.val.S != 0))) {
        if (sub->last_ins != NULL) {
            thecl_label_t* tmp_label;
            list_for_each(&sub->labels, tmp_label) {
                if (tmp_label->offset == sub->offset)
                    goto NO_OPTIM;
            }

            const expr_t* expr = expr_get_by_symbol(state->version, NOT);
            if (sub->last_ins->id == expr->id && sub->last_ins->param_count == 2) {
                thecl_param_t* param = list_head(&sub->last_ins->params);
                if (param->value.val.S != 0x1F || param->val_type != PARAM_FIELD || param->object_link != 0)
                    goto NO_OPTIM;

                if (!is_post_c2(state->version)) {
                    param = instr->params.tail->prev->data;
                    param->value.val.S = param->value.val.S == 1 ? 2 : 1;
                } else {
                    instr->id = instr->id == beqz_expr->id ? bnez_expr->id : beqz_expr->id;
                }

                param = list_tail(&sub->last_ins->params);

                if (param->value.val.S <= 0x3F && param->value.val.S >= 0 && param->val_type == PARAM_FIELD && param->object_link == 0) {
                    thecl_param_t* reg_param = instr->params.head->next->next->data;
                    reg_param->value.val.S = param->value.val.S;
                    list_del_tail(&sub->instrs);
                    thecl_instr_free(sub->last_ins);
                    --sub->offset;
                }
                else {
                    expr = expr_get_by_symbol(state->version, LOAD);
                    sub->last_ins->id = expr->id;
                    param = list_head(&sub->last_ins->params);
                    list_del_head(&sub->last_ins->params);
                    param_free(param);
                }
            } else { /* optimize if literal conditions */
                expr = expr_get_by_symbol(state->version, LOAD);
                if (sub->last_ins->id == expr->id && sub->last_ins->param_count > 0) {
                    thecl_param_t* param = list_tail(&sub->last_ins->params);
                    if (param->val_type == PARAM_LITERAL) {
                        list_del_tail(&sub->last_ins->params);
                        thecl_param_t* branch_type_param = (thecl_param_t*)instr->params.tail->prev->data;
                        int branch_type = state->version == 2 ? instr->id - bra_expr->id - 1 : branch_type_param->value.val.S - 1;
                        if (--sub->last_ins->param_count == 0) {
                            list_del_tail(&sub->instrs);
                            thecl_instr_free(sub->last_ins);
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
    instr->offset = sub->offset++;
    sub->last_ins = instr;
    if (instr->mips) {
        for (int i=0; i<32; ++i) {
            if (instr->reg_used & (1 << i)) {
                state->reg_block->regs[i].last_used = instr->offset;
            }
        }
    }
}

static void
instr_del(
    parser_state_t* state,
    thecl_sub_t* sub,
    thecl_instr_t* instr)
{
    bool found = false;
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
            if (instr == sub->last_ins) {
                sub->last_ins = list_tail(&sub->instrs);
            }
            --sub->offset;
            found = true;
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

static list_t*
convert_expr_list_to_params(
    parser_state_t* state,
    list_t* expr_list)
{
    list_t* param_list = expr_list ? list_new() : NULL;

    if (expr_list) {
        thecl_param_t* param;
        expression_t* expr;
        list_for_each(expr_list, expr) {
            param = expr->value;
            if (expr->type == EXPRESSION_VAL && param->val_type != PARAM_POINTER) {
                //param = param_copy(expr->value);
            } else if (expr->type == EXPRESSION_COLOR || expr->type == EXPRESSION_GLOBAL
                 || (expr->type == EXPRESSION_VAL && param->val_type == PARAM_POINTER)) {
                param = param_new('S');
                param->val_type = PARAM_FIELD;
                param->object_link = 0;
                param->is_expression_param = 1;
                param->value.val.S = 0x1F;
            } else if (expr->type == EXPRESSION_VAL) {
                //param = param_copy(expr->value);
            } else {
                param = param_new('S');
                param->val_type = PARAM_FIELD;
                param->object_link = 0;
                param->is_expression_param = 1;
                param->value.val.S = 0x1F;
            }
            list_append_new(param_list, param);
        }
    }

    return param_list;
}

static void
expression_replace_var(
    parser_state_t* state,
    expression_t* expression,
    int var_stack,
    expression_t* replace)
{
    list_node_t* node;
    list_for_each_node(&expression->children, node) {
        expression = node->data;
        if (expression->type == EXPRESSION_VAL && expression->value->val_type == PARAM_FIELD && expression->value->object_link == -1 && expression->value->value.val.S == var_stack) {
            expression_free(expression);
            node->data = expression_copy(replace);
        }
        else {
            expression_replace_var(state, expression, var_stack, replace);
        }
    }
}

static expression_t*
expression_replace_var_start(
    parser_state_t* state,
    expression_t* expression,
    int var_stack,
    expression_t* replace)
{
    if (expression->type == EXPRESSION_VAL && expression->value->val_type == PARAM_FIELD && expression->value->object_link == -1 && expression->value->value.val.S == var_stack) {
        expression_free(expression);
        return expression_copy(replace); /* no point in continuing */
    }

    expression_replace_var(state, expression, var_stack, replace);

    return expression;
}

static expression_t*
inline_call_replace_params(
    parser_state_t* state,
    expression_t* expr,
    list_t* params)
{
    expr = expression_copy(expr);

    int arg_stack = -1;
    if (params) {
        expression_t* param_expr;
        list_for_each_back(params, param_expr) {
            expr = expression_replace_var_start(state, expr, arg_stack, param_expr);
            --arg_stack;
        }
    }

    expression_optimize(state, expr);

    return expr;
}

static list_t*
inline_expression_list_copy(
    parser_state_t* state,
    list_t* list,
    list_t* params)
{
    if (list) {
        list_t* new_list = list_new();
        expression_t* e;
        list_for_each(list, e) {
            list_append_new(new_list, inline_call_replace_params(state, e, params));
        }
        return new_list;
    }
    else {
        return NULL;
    }
}

static void
instr_create_inline_call(
    parser_state_t* state,
    thecl_sub_t* sub,
    list_t* params_org
) {
    /* An inline sub can't call itself for obvious reasons. */
    if (strcmp(sub->name, state->current_sub->name) == 0 && sub->arg_count == state->current_sub->arg_count) {
        yyerror(state, "an inline sub is not allowed to call itself");
        return;
    }

    scope_begin(state);

    /* This string will be prepended to label names/var names etc. */
    char name[256], buf[512];
    snprintf(name, 256, "%s_%d_%d_%s_", state->current_sub->name, yylloc.first_line, yylloc.first_column, sub->name);

    thecl_line_t* line;
    list_for_each(&sub->lines, line) {
        switch (line->type) {
            default: yyerror(state, "invalid line type in sub '%s'", state->current_sub->name); break;
            case LINE_ASSIGNMENT: {
                var_assign(state, param_copy(line->ass.address), inline_call_replace_params(state, line->ass.expr, params_org));
            } break;
            case LINE_LABEL: {
                snprintf(buf, 512, "%s%s", name, line->name);
                label_create(state, buf);
            } break;
            case LINE_INSTRUCTION:
            case LINE_CALL: {
                if (state->current_sub->is_inline) {
                    list_append_new(&state->current_sub->lines, line_make_call(line->call.name, inline_expression_list_copy(state, line->call.expr_list, params_org)));
                }
                else {
                    list_t *expr_list;
                    if (line->call.expr_list) {
                        expr_list = list_new();
                        expression_t* expr;
                        list_for_each(line->call.expr_list, expr) {
                            list_append_new(expr_list, inline_call_replace_params(state, expr, params_org));
                        }
                    }
                    else {
                        expr_list = NULL;
                    }
                    expression_t* expr;
                    const gool_ins_t* gool_ins = gool_ins_get_by_name(state->version, line->call.name);
                    if (gool_ins) {
                        instr_create_gool_ins(state, gool_ins, expr_list);
                    }
                    else {
                        instr_create_call(state, expr_get_by_symbol(state->version, CALL)->id, strdup(line->call.name), expr_list);
                    }

                    if (expr_list) {
                        list_for_each(expr_list, expr) {
                            expression_free(expr);
                        }
                        list_free_nodes(expr_list);
                        free(expr_list);
                    }
                }
            } break;
            case LINE_SCOPE_START: {
                scope_begin(state);
            } break;
            case LINE_SCOPE_END: {
                scope_finish(state, true);
            } break;
            case LINE_VAR_DECL: {
                var_create(state, state->current_sub, line->var.name, true);
            } break;
            case LINE_VAR_DECL_ASSIGN: {
                var_create_assign(state, state->current_sub, line->var.name, inline_call_replace_params(state, line->var.expr, params_org));
            } break;
            case LINE_BREAK: {
                if (state->current_sub->is_inline) {
                    list_append_new(&state->current_sub->lines, line_make(LINE_BREAK));
                }
                else {
                    list_node_t *head = state->block_stack.head;
                    for(; head; head = head->next) {
                        if (strncmp(head->data, "do", 2) == 0 ||
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
            } break;
            case LINE_CONTINUE: {
                if (state->current_sub->is_inline) {
                    list_append_new(&state->current_sub->lines, line_make(LINE_CONTINUE));
                }
                else {
                    list_node_t *head = state->block_stack.head;
                    for(; head; head = head->next) {
                        if (strncmp(head->data, "do", 2) == 0 ||
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
                        yyerror(state, "break not within while or until");
                        g_was_error = true;
                    }
                }
            } break;
            case LINE_RETURN: {
                if (state->current_sub->is_inline) {
                    list_append_new(&state->current_sub->lines, line_make(LINE_RETURN));
                }
                else {
                    snprintf(buf, 512, "%s%s", name, "inline_end");
                    expression_create_goto(state, GOTO, buf, NULL);
                }
            } break;
            case LINE_SAVE_START: {
                if (state->current_sub->is_inline) {
                    list_append_new(&state->current_sub->lines, line_make_save_start(line->list));
                }
                else {
                    const expr_t* local_expr = expr_get_by_symbol(state->version, LOAD);
                    const expr_t* global_expr = expr_get_by_symbol(state->version, GLOAD);
                    const expr_t* color_expr = expr_get_by_symbol(state->version, CLOAD);
                    thecl_param_t *param;
                    list_for_each(line->list, param) {
                        param = param_copy(param);
                        list_append_new(&state->addresses, param);
                        switch (param->val_type) {
                            default: instr_add(state, state->current_sub, instr_new(state, local_expr->id, "p", param)); break;
                            case PARAM_GLOBAL: instr_add(state, state->current_sub, instr_new(state, global_expr->id, "S", param->value.val.S)); break;
                            case PARAM_COLOR: instr_add(state, state->current_sub, instr_new(state, color_expr->id, "SS", param->object_link, param->value.val.S)); break;
                        }
                    }
                    int count = list_count(line->list);
                    state->current_sub->stack_offset += count;
                    list_append_new(&state->addresses, count);
                }
            } break;
            case LINE_SAVE_END: {
                if (state->current_sub->is_inline) {
                    list_append_new(&state->current_sub->lines, line_make_save_end());
                }
                else {
                    int m = list_tail(&state->addresses);
                    list_del_tail(&state->addresses);

                    const expr_t* local_expr = expr_get_by_symbol(state->version, ASSIGN);
                    const expr_t* global_expr = expr_get_by_symbol(state->version, GASSIGN);
                    const expr_t* color_expr = expr_get_by_symbol(state->version, CASSIGN);
                    for (int i=0; i<m; ++i) {
                        thecl_param_t* param = list_tail(&state->addresses);
                        switch (param->val_type) {
                            default: instr_add(state, state->current_sub, instr_new(state, local_expr->id, "pp", param_copy(param), param_sp_new())); break;
                            case PARAM_GLOBAL: instr_add(state, state->current_sub, instr_new(state, global_expr->id, "Sp", param->value.val.S, param_sp_new())); break;
                            case PARAM_COLOR: instr_add(state, state->current_sub, instr_new(state, color_expr->id, "pSS", param_sp_new(), param->object_link, param->value.val.S)); break;
                        }
                        list_del_tail(&state->addresses);
                    }
                    state->current_sub->stack_offset -= m;
                }
            } break;
            case LINE_GOTO: {
                snprintf(buf, 512, "%s%s", name, line->go.label);
                expression_create_goto(state, line->go.type, buf, line->go.expr ? inline_call_replace_params(state, line->go.expr, params_org) : NULL);
            } break;
        }
    }

    scope_finish(state, true);
}

static void
instr_create_call(
    parser_state_t *state,
    uint8_t type,
    char *name,
    list_t *params)
{
    /* Instr name */
    thecl_param_t *name_param = param_new('z');
    name_param->value.type = 'z';
    name_param->value.val.z = name;

    int argc = 0;
    if (params != NULL) {
        expression_t* expr;
        list_for_each(params, expr) {
            expression_output(state, expr);
            ++argc;
        }
    }

    if (!strcmp(state->current_sub->name, name)) {
        state->current_sub->self_reference = true;
    }

    instr_add(state, state->current_sub, instr_new(state, type, "pSS", name_param, 38, argc));
}

static void
instr_create_gool_ins(
    parser_state_t* state,
    const gool_ins_t* gool_ins,
    list_t* params)
{
    list_t* param_list = list_new();
    list_t* arg_list = list_new();

    /* Split expression list into instruction params and extra args */
    list_node_t* late_node = NULL;
    if (params != NULL) {
        int i = 0;
        list_node_t* node, *next_node;
        list_for_each_node_safe(params, node, next_node) {
            if (gool_ins->varargs <= 0 || (i < gool_ins->varargs)) {
                list_append_new(param_list, node->data);
                if (i == gool_ins->late_param)
                    late_node = param_list->tail;
            }
            else if (gool_ins->reverse_args)
                list_prepend_new(arg_list, node->data);
            else
                list_append_new(arg_list, node->data);
            ++i;
        }
    }

    /* Output the varargs */
    expression_t* expr;
    list_for_each(arg_list, expr) {
        expression_output(state, expr);
        if (state->top_reg) {
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
            state->stack_adjust += 4;
        }
    }

    /* Set the late expr to NULL if it does not need to be output */
    expression_t* late_expr = late_node ? late_node->data : NULL;
    thecl_param_t* late_param = (late_expr && late_expr->type == EXPRESSION_VAL) ? late_expr->value : NULL;
    if (late_expr && (!late_param || (late_param && !((late_param->val_type == PARAM_FIELD && late_param->object_link == 0 && late_param->value.val.S >= 0 && late_param->value.val.S <= 0x3F) || late_param->val_type == PARAM_LITERAL)))) {
        late_node->data = EXPR_SP();
    }
    else {
        late_expr = NULL;
    }

    /* Output instruction parameters */
    list_t* ins_params = convert_expr_list_to_params(state, param_list);
    list_node_t* expr_node = params ? params->head : NULL;
    thecl_param_t* param;
    list_for_each(ins_params, param) {
        if (param->is_expression_param) {
            expression_output(state, expr_node->data);
            if (state->top_reg) {
                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                state->stack_adjust += 4;
            }
        }
        expr_node = expr_node->next;
    }

    /* Output the late parameter */
    if (late_expr) {
        expression_output(state, late_expr);
        if (state->top_reg) {
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
            state->stack_adjust += 4;
        }
        expression_free(late_node->data);
    }
    instr_add(state, state->current_sub, instr_new_list(state, gool_ins->id, gool_ins->param_list_validate(ins_params, list_count(arg_list))));
    list_free_nodes(ins_params);
    free(ins_params);

    if (gool_ins->pop_args) {
        int argc = list_count(arg_list);
        if (argc) {
            if (state->mips_mode) {
                state->stack_adjust -= argc * 4;
                mips_stack_adjust(state, state->current_sub);
            }
            else {
                instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, GOTO)->id, "SSSSS", 0, argc, 0x25, 0, 0));
            }
        }
    }

    list_free_nodes(param_list);
    free(param_list);
    list_free_nodes(arg_list);
    free(arg_list);
}

static mips_reg_t*
request_reg(
    parser_state_t* state,
    expression_t* expr)
{
    mips_reg_t* reg = get_usable_reg(state->reg_block);
    if (reg) {
        reg->status = MREG_STATUS_IN_USE;
        if (expr) {
            reg->saved_expr = expression_copy(expr);
            reg->saved_param = expr->type == EXPRESSION_VAL ? expr->value : NULL;
        }
    }
    else {
        yyerror(state, "no available registers for mips mode");
        exit(2);
    }
    return reg;
}

static void
verify_reg_load(
    parser_state_t* state,
    mips_reg_t** reg_loc,
    expression_t* expr)
{
    mips_reg_t* reg = *reg_loc;
    if (reg == NULL) {
        reg = request_reg(state, expr);
        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", -4, reg->index, get_reg(state->reg_block, "s6")->index));
        state->stack_adjust -= 4;
    }
    *reg_loc = reg;
}

static expression_t*
expression_load_new(
    const parser_state_t* state,
    thecl_param_t* value)
{
    expression_t* ret = malloc(sizeof(expression_t));
    const expr_t* expr;
    if (value->val_type == PARAM_GLOBAL) {
        expr = expr_get_by_symbol(state->version, GLOAD);
        ret->type = EXPRESSION_GLOBAL;
    }
    else if (value->val_type == PARAM_COLOR) {
        expr = expr_get_by_symbol(state->version, CLOAD);
        ret->type = EXPRESSION_COLOR;
    }
    else if (value->val_type == PARAM_FIELD && !var_is_valid_field_ref(state, value)) {
        expr = expr_get_by_symbol(state->version, MISC);
        ret->type = EXPRESSION_XVAL;
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
    expression_t* val2)
{
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
    } else if (expr->type == EXPRESSION_VAL || expr->type == EXPRESSION_XVAL) {
        copy->value = param_copy(expr->value);
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
    if (state->current_sub->is_inline) {
        list_append_new(&state->current_sub->lines, line_make_goto(type, strdup(labelstr), cond ? expression_copy(cond) : NULL));
        return;
    }
    if (state->mips_mode && false) {
        mips_stack_adjust(state, state->current_sub);
        if (type != GOTO) {
            expression_output(state, cond);
            verify_reg_load(state, &state->top_reg, cond);
            state->top_reg->status = MREG_STATUS_USED;
        }
        switch (type) {
            case IF: instr_add(state, state->current_sub, MIPS_INSTR_BNEZ(strdup(labelstr), state->top_reg->index)); break;
            case UNLESS: instr_add(state, state->current_sub, MIPS_INSTR_BEQZ(strdup(labelstr), state->top_reg->index)); break;
            case GOTO: default: instr_add(state, state->current_sub, MIPS_INSTR_BEQZ(strdup(labelstr), 0)); instr_add(state, state->current_sub, MIPS_INSTR_NOP()); break;
        }
    }
    else {
        thecl_param_t *p1 = param_new('o');
        p1->value.type = 'z';
        p1->value.val.z = strdup(labelstr);
        thecl_param_t *pcond;
        if (type == GOTO || cond == NULL) {
            pcond = param_new('S');
            pcond->val_type = PARAM_FIELD;
            pcond->object_link = 0;
            pcond->value.val.S = field_get("misc")->offset;
        }
        else {
            if (cond->type != EXPRESSION_VAL || cond->value->val_type != PARAM_FIELD || cond->value->object_link != 0 || cond->value->value.val.S < 0 || cond->value->value.val.S > 0x3F) {
                expression_output(state, cond);
                pcond = param_sp_new();
            }
            else {
                pcond = cond->value;
            }
        }
        instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, type)->id, "pSpSS", p1, pop, pcond, type == GOTO ? 0 : (type == IF ? 1 : (type == UNLESS ? 2 : 3)), 0));
    }
}

static void
expression_mips_load(
    parser_state_t* state,
    expression_t* expr)
{
    thecl_param_t* param = expr->value;
    mips_reg_t* reg = NULL;
    if (!((param->val_type == PARAM_FIELD && !(param->object_link >= -1 && param->object_link <= 7)) || param->val_type == PARAM_POINTER)) {
        reg = request_reg(state, expr);
    }
    int val = param->value.val.S;
    if (param->val_type == PARAM_LITERAL) { /* number */
        if (val == 0) {
            instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("addu", reg->index, 0, 0));
        }
        else if (val >= 0 && val <= 0xFFFF) {
            instr_add(state, state->current_sub, MIPS_INSTR_I("ori", val & 0xFFFF, reg->index, 0));
        }
        else if (val >= -0x8000 && val <= 0x7FFF) {
            instr_add(state, state->current_sub, MIPS_INSTR_I("addiu", val & 0xFFFF, reg->index, 0));
        }
        else {
            instr_add(state, state->current_sub, MIPS_INSTR_I("lui", val >> 16 & 0xFFFF, reg->index, 0));
            instr_add(state, state->current_sub, MIPS_INSTR_I("ori", val & 0xFFFF, reg->index, reg->index));
        }
    }
    else if (param->val_type == PARAM_FIELD) { /* object field, stack */
        if (param->object_link == 0) { /* object field */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", val * 4 + get_obj_proc_offset(state->version), reg->index, get_reg(state->reg_block, "s0")->index));
        }
        else if (param->object_link >= 1 && param->object_link <= 7) { /* linked object field */
            mips_reg_t* link_reg = get_usable_reg(state->reg_block);
            if (!link_reg) link_reg = reg;
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", param->object_link * 4 + get_obj_proc_offset(state->version), link_reg->index, get_reg(state->reg_block, "s0")->index));
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", val * 4 + get_obj_proc_offset(state->version), reg->index, link_reg->index));
            if (link_reg != reg) {
                link_reg->status = MREG_STATUS_USED;
            }
        }
        else if (param->object_link == -1) { /* stack */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", val * 4, reg->index, get_reg(state->reg_block, "s7")->index));
        }
        else if (expr->type == EXPRESSION_XVAL) {
            instr_add(state, state->current_sub, instr_new(state, expr->id, "SSSS", param->value.val.S << 8, param->object_link, 0, 3));
        }
        else { /* other */
            instr_add(state, state->current_sub, instr_new(state, expr->id, "p", param));
        }
    }
    else if (param->val_type == PARAM_GLOBAL) { /* global */
        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", 0x58, reg->index, get_reg(state->reg_block, "s8")->index));
        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", (val >> 8) * 4, reg->index, reg->index));
    }
    else if (param->val_type == PARAM_COLOR) { /* color field */
        if (param->object_link >= 1 && param->object_link <= 7) { /* linked color field */
            mips_reg_t* link_reg = get_usable_reg(state->reg_block);
            if (!link_reg) link_reg = reg;
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", param->object_link * 4 + get_obj_proc_offset(state->version), link_reg->index, get_reg(state->reg_block, "s0")->index));
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lh", val * 2 + 0x20, reg->index, link_reg->index));
            if (link_reg != reg) {
                link_reg->status = MREG_STATUS_USED;
            }
        }
        else {
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lh", val * 2 + 0x20, reg->index, get_reg(state->reg_block, "s0")->index));
        }
    }
    else if (param->val_type == PARAM_POINTER) { /* pointer */
        instr_add(state, state->current_sub, instr_new(state, expr->id, "p", param));
    }
    state->top_reg = reg;
}

static void
expression_mips_operation(
    parser_state_t* state,
    expression_t* expr)
{
    mips_reg_t* ret = NULL, *op1, *op2, *temp;
    expression_t* val_expr, *var_expr = NULL, *child_expr1 = NULL, *child_expr2 = NULL, *child_expr3 = NULL;
    int i = 0;
    list_for_each(&expr->children, val_expr) {
        ++i;
        if (i == 1) {
            child_expr1 = val_expr;
        }
        else if (i == 2) {
            child_expr2 = val_expr;
        }
        else if (i == 3) {
            child_expr3 = val_expr;
        }
    }
    val_expr = NULL;
    int symbol = expr_get_by_id(state->version, expr->id)->symbol;
    switch (symbol) {
        case ADD:
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S >= -0x8000 && child_expr1->value->value.val.S <= 0x7FFF) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S >= -0x8000 && child_expr2->value->value.val.S <= 0x7FFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr && !(var_expr->type == EXPRESSION_VAL && var_expr->value->val_type == PARAM_LITERAL && var_expr->value->value.val.S == 0)) {
                OutputExprToReg(var_expr, op1);
                verify_reg_load(state, &op1, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_I("addiu", val_expr->value->value.val.S, ret->index, op1->index));
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("addu", ret->index, op2->index, op1->index));
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            break;
        case SUBTRACT:
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S == 0) {
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("subu", ret->index, op2->index, 0));
                SetUsedReg(op2);
            }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S >= -0x7FFF && child_expr2->value->value.val.S <= 0x8000) {
                OutputExprToReg(child_expr2, op1);
                verify_reg_load(state, &op1, child_expr2);
                ret = request_reg(state, expr);
                if (child_expr2->value->value.val.S == 0) {
                    instr_add(state, state->current_sub, MIPS_INSTR_MOVE(ret->index, op1->index));
                }
                else {
                    instr_add(state, state->current_sub, MIPS_INSTR_I("addiu", -child_expr2->value->value.val.S, ret->index, op1->index));
                }
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("subu", ret->index, op2->index, op1->index));
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            break;
        case XOR:
        case OR:
        case B_AND:
        case B_OR:
        case TEST:
            const char* oprname = NULL, *opiname = NULL;
            switch (symbol) {
                case XOR: oprname = "xor"; opiname = "xori"; break;
                case OR: case B_OR: oprname = "or"; opiname = "ori"; break;
                case B_AND: case TEST: oprname = "and"; opiname = "andi"; break;
            }
            if (symbol != TEST && child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S >= 0 && child_expr1->value->value.val.S <= 0xFFFF) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S >= 0 && child_expr2->value->value.val.S <= 0xFFFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr && !(var_expr->type == EXPRESSION_VAL && var_expr->value->val_type == PARAM_LITERAL && var_expr->value->value.val.S == 0)) {
                OutputExprToReg(var_expr, op1);
                verify_reg_load(state, &op1, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_I(opiname, val_expr->value->value.val.S, ret->index, op1->index));
                if (symbol == TEST) {
                    instr_add(state, state->current_sub, MIPS_INSTR_I("xori", val_expr->value->value.val.S, ret->index, ret->index));
                }
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R(oprname, ret->index, op2->index, op1->index));
                if (symbol == TEST) {
                    instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("xor", ret->index, op2->index, ret->index));
                }
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            if (symbol == TEST) {
                instr_add(state, state->current_sub, MIPS_INSTR_I("sltiu", 1, ret->index, ret->index));
            }
            break;
        case EQUAL:
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S >= 0 && child_expr1->value->value.val.S <= 0xFFFF) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S >= 0 && child_expr2->value->value.val.S <= 0xFFFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr && !(var_expr->type == EXPRESSION_VAL && var_expr->value->val_type == PARAM_LITERAL && var_expr->value->value.val.S == 0)) {
                OutputExprToReg(var_expr, op1);
                verify_reg_load(state, &op1, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_I("xori", val_expr->value->value.val.S, ret->index, op1->index));
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("xor", ret->index, op2->index, op1->index));
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            instr_add(state, state->current_sub, MIPS_INSTR_I("sltiu", 1, ret->index, ret->index));
            break;
        case AND:
            OutputExprToReg(child_expr1, op1);
            OutputExprToReg(child_expr2, op2);
            verify_reg_load(state, &op2, child_expr2);
            verify_reg_load(state, &op1, child_expr1);
            ret = request_reg(state, expr);
            char buf[512];
            snprintf(buf, 512, "@!%s_MipsOp_AND_%p_%p", state->current_sub->name, child_expr1, child_expr2);
            instr_add(state, state->current_sub, MIPS_INSTR_BEQZ(strdup(buf), op2->index));
            instr_add(state, state->current_sub, MIPS_INSTR_MOVE(ret->index, op2->index));
            instr_add(state, state->current_sub, MIPS_INSTR_MOVE(ret->index, op1->index));
            label_create(state, buf);
            SetUsedReg(op1);
            SetUsedReg(op2);
            break;
        case NOT:
            OutputExprToReg(child_expr2, op1);
            verify_reg_load(state, &op1, child_expr2);
            ret = request_reg(state, expr);
            instr_add(state, state->current_sub, MIPS_INSTR_I("sltiu", 1, ret->index, op1->index));
            SetUsedReg(op1);
            break;
        case LSHIFT:
        case RSHIFT:
            if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S < 0) {
                if (symbol == LSHIFT) symbol = RSHIFT; else symbol = LSHIFT;
                child_expr2->value->value.val.S = -child_expr2->value->value.val.S;
            }
            if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL) {
                OutputExprToReg(child_expr1, op1);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_SHIFT(symbol == LSHIFT ? "sll" : "sra", child_expr2->value->value.val.S & 0x1F, ret->index, op1->index));
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R(symbol == LSHIFT ? "sllv" : "srav", ret->index, op1->index, op2->index));
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            break;
        case LT:
        case GT:
        case GTEQ:
            if (symbol == GT) {
                val_expr = child_expr1;
                child_expr1 = child_expr2;
                child_expr2 = val_expr;
                val_expr = NULL;
            }
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S >= -0xFFFF && child_expr1->value->value.val.S <= 0) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S >= 0 && child_expr2->value->value.val.S <= 0xFFFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr == child_expr1 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->val_type == PARAM_LITERAL && var_expr->value->value.val.S == 0)) { /* imm < b --> -b < -imm */
                OutputExprToReg(var_expr, op2);
                verify_reg_load(state, &op2, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("subu", ret->index, op2->index, 0));
                instr_add(state, state->current_sub, MIPS_INSTR_I("slti", -val_expr->value->value.val.S, ret->index, ret->index));
                SetUsedReg(op2);
            }
            else if (val_expr == child_expr2 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->val_type == PARAM_LITERAL && var_expr->value->value.val.S == 0)) { /* a < imm */
                OutputExprToReg(var_expr, op1);
                verify_reg_load(state, &op1, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_I("slti", val_expr->value->value.val.S, ret->index, op1->index));
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("slt", ret->index, op2->index, op1->index));
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            if (symbol == GTEQ) {
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("sltiu", 1, ret->index, ret->index));
            }
            break;
        case LTEQ:
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S >= -0xFFFE && child_expr1->value->value.val.S <= 1) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S >= 1 && child_expr2->value->value.val.S <= 0xFFFE) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr == child_expr1 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->val_type == PARAM_LITERAL && var_expr->value->value.val.S == 0)) { /* imm <= b --> imm < b+1 --> imm-1 < b --> -b < -(imm-1) */
                OutputExprToReg(var_expr, op2);
                verify_reg_load(state, &op2, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("subu", ret->index, op2->index, 0));
                instr_add(state, state->current_sub, MIPS_INSTR_I("slti", -(val_expr->value->value.val.S-1), ret->index, ret->index));
                SetUsedReg(op2);
            }
            else if (val_expr == child_expr2 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->val_type == PARAM_LITERAL && var_expr->value->value.val.S == 0)) { /* a <= imm --> a < imm+1*/
                OutputExprToReg(var_expr, op1);
                verify_reg_load(state, &op1, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_I("slti", val_expr->value->value.val.S+1, ret->index, op1->index));
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_I("addiu", 1, ret->index, op2->index));
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("slt", ret->index, ret->index, op1->index));
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            break;
        case MULTIPLY:
            OutputExprToReg(child_expr1, op1);
            OutputExprToReg(child_expr2, op2);
            verify_reg_load(state, &op2, child_expr2);
            verify_reg_load(state, &op1, child_expr1);
            ret = request_reg(state, expr);
            instr_add(state, state->current_sub, MIPS_INSTR_MULT(op2->index, op1->index));
            state->current_sub->multdiv_offset = state->current_sub->last_ins->offset;
            make_optional_delay_slots(state, 13, state->current_sub->last_ins);
            instr_add(state, state->current_sub, MIPS_INSTR_MFLO(ret->index));
            SetUsedReg(op1);
            SetUsedReg(op2);
            break;
        case DIVIDE:
            OutputExprToReg(child_expr1, op1);
            OutputExprToReg(child_expr2, op2);
            verify_reg_load(state, &op2, child_expr2);
            verify_reg_load(state, &op1, child_expr1);
            ret = request_reg(state, expr);
            instr_add(state, state->current_sub, MIPS_INSTR_DIV(op2->index, op1->index));
            state->current_sub->multdiv_offset = state->current_sub->last_ins->offset;
            make_optional_delay_slots(state, 36, state->current_sub->last_ins);
            instr_add(state, state->current_sub, MIPS_INSTR_MFLO(ret->index));
            SetUsedReg(op1);
            SetUsedReg(op2);
            break;
        case MODULO:
            OutputExprToReg(child_expr1, op1);
            OutputExprToReg(child_expr2, op2);
            verify_reg_load(state, &op2, child_expr2);
            verify_reg_load(state, &op1, child_expr1);
            ret = request_reg(state, expr);
            instr_add(state, state->current_sub, MIPS_INSTR_DIV(op2->index, op1->index));
            state->current_sub->multdiv_offset = state->current_sub->last_ins->offset;
            make_optional_delay_slots(state, 36, state->current_sub->last_ins);
            instr_add(state, state->current_sub, MIPS_INSTR_MFHI(ret->index));
            SetUsedReg(op1);
            SetUsedReg(op2);
            break;
        case SPD:
            temp = request_reg(state, expr);
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", 0x54, temp->index, get_reg(state->reg_block, "s8")->index));
            if (!(child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S == 0)) {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
            }
            else {
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
            }
            ret = request_reg(state, expr);
            instr_add(state, state->current_sub, MIPS_INSTR_MULT(temp->index, op2->index));
            state->current_sub->multdiv_offset = state->current_sub->last_ins->offset;
            make_optional_delay_slots(state, 13, state->current_sub->last_ins);
            instr_add(state, state->current_sub, MIPS_INSTR_MFLO(ret->index));
            instr_add(state, state->current_sub, MIPS_INSTR_SHIFT("sra", 10, ret->index, ret->index));
            if (!(child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S == 0)) {
                if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->val_type == PARAM_LITERAL && child_expr1->value->value.val.S >= -0x7FFF && child_expr1->value->value.val.S <= 0x8000) {
                    instr_add(state, state->current_sub, MIPS_INSTR_I("addiu", child_expr1->value->value.val.S, ret->index, ret->index));
                }
                else {
                    instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("addu", ret->index, op1->index, ret->index));
                }
                SetUsedReg(op1);
            }
            SetUsedReg(op2);
            free_reg(temp);
            break;
        case TIME:
            temp = request_reg(state, expr);
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", 0x50, temp->index, get_reg(state->reg_block, "s8")->index));
            if (!(child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S == 0)) {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->val_type == PARAM_LITERAL && child_expr2->value->value.val.S >= -0x7FFF && child_expr2->value->value.val.S <= 0x8000) {
                    instr_add(state, state->current_sub, MIPS_INSTR_I("addiu", child_expr2->value->value.val.S, temp->index, temp->index));
                }
                else {
                    instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("addu", temp->index, op2->index, temp->index));
                }
                SetUsedReg(op2);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                verify_reg_load(state, &op1, child_expr1);
            }
            ret = request_reg(state, expr);
            instr_add(state, state->current_sub, MIPS_INSTR_DIV(op1->index, temp->index));
            state->current_sub->multdiv_offset = state->current_sub->last_ins->offset;
            make_optional_delay_slots(state, 36, state->current_sub->last_ins);
            instr_add(state, state->current_sub, MIPS_INSTR_MFHI(ret->index));
            SetUsedReg(op1);
            free_reg(temp);
            break;
        default:
            {
            /* a normal GOOL instruction will destroy the registers
             * s5, v0, v1, a0 and a1 for sure, not to mention any
             * other register used by any functions called from it */
            list_t* saved_regs = list_new();
            for (int i=0; i<34; ++i) {
                if (state->reg_block->regs[i].status == MREG_STATUS_IN_USE) {
                    state->reg_block->regs[i].status = MREG_STATUS_RESERVED;
                    list_prepend_new(saved_regs, &state->reg_block->regs[i]);
                    if (i<32) {
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->reg_block->regs[i].index, get_reg(state->reg_block, "s6")->index));
                    }
                    else {
                        temp = request_reg(state, NULL);
                        if (i == get_reg(state->reg_block, "lo")->index) {
                            instr_add(state, state->current_sub, MIPS_INSTR_MFLO(temp->index));
                        }
                        else if (i == get_reg(state->reg_block, "hi")->index) {
                            instr_add(state, state->current_sub, MIPS_INSTR_MFHI(temp->index));
                        }
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, temp->index, get_reg(state->reg_block, "s6")->index));
                        free_reg(temp);
                    }
                    state->stack_adjust += 4;
                }
            }

            if (!state->scope_stack[state->scope_cnt - 1].mips) mips_stack_adjust(state, state->current_sub);
            const expr_t* expression = expr_get_by_id(state->version, expr->id);
            int c = 0, lc = list_count(&expr->children);
            list_t* param_list = list_new();

            expression_t* child_expr;
            list_node_t* child_node;
            list_node_t* child_next;
            list_for_each_node_safe(&expr->children, child_node, child_next) {
                ++c;
                child_expr = child_node->data;
                if (child_expr->type == EXPRESSION_VAL && (child_expr->value->val_type == PARAM_LITERAL || child_expr->value->val_type == PARAM_FIELD) && (!expression->has_double_param || (expression->has_double_param && lc <= 2) || (expression->has_double_param && lc > 2 && c == 1))) {
                    list_append_new(param_list, child_expr->value);
                }
                else {
                    expression_output(state, child_expr);
                    if (state->top_reg) {
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                        state->stack_adjust += 4;
                    }
                    if (child_expr->type == EXPRESSION_VAL && child_expr->value->val_type == PARAM_POINTER) {
                        list_del(&expr->children, child_node);
                        expression_free(child_expr);
                        if (c <= expression->stack_arity) {
                            list_append_new(param_list, param_sp_new());
                        }
                    }
                    else {
                        list_append_new(param_list, param_sp_new());
                    }
                }
            }

            if (expression->has_double_param && lc == 3) {
                param_free(param_list->tail->data);
                param_free(param_list->tail->prev->data);
                list_del_tail(param_list);
                list_del_tail(param_list);
                list_append_new(param_list, param_sp2_new());
            }

            instr_add(state, state->current_sub, instr_new_list(state, expr->id, param_list));

            mips_reg_t* saved_reg;
            list_for_each(saved_regs, saved_reg) {
                saved_reg->status = MREG_STATUS_IN_USE;
                state->stack_adjust -= 4;
                if (i<32) {
                    instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", state->stack_adjust-4, saved_reg->index, get_reg(state->reg_block, "s6")->index));
                }
                else {
                    temp = request_reg(state, NULL);
                    instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", state->stack_adjust-4, temp->index, get_reg(state->reg_block, "s6")->index));
                    if (i == get_reg(state->reg_block, "lo")->index) {
                        instr_add(state, state->current_sub, MIPS_INSTR_MTLO(temp->index));
                    }
                    else if (i == get_reg(state->reg_block, "hi")->index) {
                        instr_add(state, state->current_sub, MIPS_INSTR_MTHI(temp->index));
                    }
                    free_reg(temp);
                }
            }
            list_free_nodes(saved_regs);
            free(saved_regs);
            }
            break;
    }
    state->top_reg = ret;
}

static void
expression_output_mips(
    parser_state_t* state,
    expression_t* expr)
{
    if (expr->type == EXPRESSION_VAL || expr->type == EXPRESSION_XVAL || expr->type == EXPRESSION_GLOBAL || expr->type == EXPRESSION_COLOR) {
        expression_mips_load(state, expr);
    }
    else if (expr->type == EXPRESSION_OP) {
        expression_mips_operation(state, expr);
    }
    else if (expr->type == EXPRESSION_TERNARY) {
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
expression_output(
    parser_state_t* state,
    expression_t* expr)
{
    if (expr->type != EXPRESSION_TERNARY && expr_get_by_id(state->version, expr->id)->id < -2) {
        yyerror(state, "error, cannot output non-compileable expression %d", expr->id);
        exit(2);
    }

    if (state->mips_mode) {
        expression_output_mips(state, expr);
        return;
    }
    state->top_reg = NULL;

    if (expr->type == EXPRESSION_VAL) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "p", expr->value));
    } else if (expr->type == EXPRESSION_XVAL) {
        instr_add (state, state->current_sub, instr_new(state, expr->id, "SSSS", expr->value->value.val.S << 8, expr->value->object_link, 0, 3));
    } else if (expr->type == EXPRESSION_GLOBAL) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "S", expr->value->value.val.S));
    } else if (expr->type == EXPRESSION_COLOR) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "SS", expr->value->object_link, expr->value->value.val.S));
    } else if (expr->type == EXPRESSION_OP) {
        const expr_t* expression = expr_get_by_id(state->version, expr->id);
        int c = 0, lc = list_count(&expr->children);

        if (expression->symbol == RSHIFT && lc == 2) { /* special case handling, as GOOL does not have a native >> operation */
            expression_t* shamt = list_tail(&expr->children);
            list_del_tail(&expr->children);
            list_append_new(&expr->children, EXPR_2(SUBTRACT, EXPR_VAL(0), expression_copy(shamt)));
            expression_free(shamt);
            expr->id = expr_get_by_symbol(state->version, LSHIFT)->id;
        }

        list_t* param_list = list_new();

        expression_t* child_expr;
        list_node_t* child_node;
        list_node_t* child_next;
        list_for_each_node_safe(&expr->children, child_node, child_next) {
            ++c;
            child_expr = child_node->data;
            if (child_expr->type == EXPRESSION_VAL && (child_expr->value->val_type == PARAM_LITERAL || child_expr->value->val_type == PARAM_FIELD) && (!expression->has_double_param || (expression->has_double_param && lc <= 2) || (expression->has_double_param && lc > 2 && c == 1))) {
                list_append_new(param_list, child_expr->value);
            }
            else {
                expression_output(state, child_expr);
                if (child_expr->type == EXPRESSION_VAL && child_expr->value->val_type == PARAM_POINTER) {
                    list_del(&expr->children, child_node);
                    expression_free(child_expr);
                    if (c <= expression->stack_arity) {
                        list_append_new(param_list, param_sp_new());
                    }
                }
                else {
                    list_append_new(param_list, param_sp_new());
                }
            }
        }

        if (expression->has_double_param && lc == 3) {
            param_free(param_list->tail->data);
            param_free(param_list->tail->prev->data);
            list_del_tail(param_list);
            list_del_tail(param_list);
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
    expression_t* child_expr_1 = NULL;
    expression_t* child_expr_2 = NULL;
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

    const expr_t* expr = expr_get_by_id(state->version, expression->id);

    if (expr->stack_arity != 2 || !expr->allow_optim) return;

    if ((child_expr_1->type == EXPRESSION_VAL && child_expr_1->value->type != 'S') || (child_expr_2->type == EXPRESSION_VAL && child_expr_2->value->type != 'S')) return;

    if (   !expr->is_unary && (
           !expression_is_number(child_expr_1) /* Variables are not acceptable, obviously. */
        || !expression_is_number(child_expr_2)
      ) || expr->is_unary && (
           !expression_is_number(child_expr_2)
        || child_expr_1->value->value.val.S != 0x1F
        || child_expr_1->value->object_link != 0
        || child_expr_1->value->val_type != PARAM_FIELD
      )
    ) {
        /* Partial expression optimization */
        expr = expr_get_by_id(state->version, expression->id);
        if (expr->symbol == ADD) {
            expression_t* zero_expr = expression_is_number(child_expr_1) && child_expr_1->value->value.val.S == 0 ? child_expr_1 : (expression_is_number(child_expr_2) && child_expr_2->value->value.val.S == 0 ? child_expr_2 : NULL);
            if (zero_expr) {
                expression_t* value_expr = zero_expr == child_expr_1 ? child_expr_2 : child_expr_1;
                expression_t* new_expr = expression_copy(value_expr);
                memcpy(expression, new_expr, sizeof(expression_t));
                free(new_expr);

                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                return;
            }

            expr = expr_get_by_symbol(state->version, RAND);
            expression_t* rand_expr = child_expr_1->id == expr->id ? child_expr_1 : (child_expr_2->id == expr->id ? child_expr_2 : NULL);
            expression_t* head_expr = rand_expr ? list_head(&rand_expr->children) : NULL;
            expression_t* tail_expr = rand_expr ? list_tail(&rand_expr->children) : NULL;
            expression_t* numb_expr = rand_expr == child_expr_1 ? child_expr_2 : child_expr_1;
            if (rand_expr && expression_is_number(numb_expr) && expression_is_number(head_expr) && expression_is_number(tail_expr)) {
                head_expr = expression_copy(head_expr);
                tail_expr = expression_copy(tail_expr);
                numb_expr = expression_copy(numb_expr);

                expression->id = expr->id;
                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                list_free_nodes(&expression->children);

                list_append_new(&expression->children, EXPR_2(ADD, expression_copy(numb_expr), head_expr));
                list_append_new(&expression->children, EXPR_2(ADD, expression_copy(numb_expr), tail_expr));
                return;
            }

            expr = expr_get_by_symbol(state->version, SUBTRACT);
            expression_t* sub_expr = child_expr_1->id == expr->id ? child_expr_1 : (child_expr_2->id == expr->id ? child_expr_2 : NULL);
                         head_expr = sub_expr ? list_head(&sub_expr->children) : NULL;
                         numb_expr = sub_expr == child_expr_1 ? child_expr_2 : child_expr_1;
            if (sub_expr && expression_is_number(head_expr) && head_expr->value->value.val.S == 0) {
                sub_expr = expression_copy(list_tail(&sub_expr->children));
                numb_expr = expression_copy(numb_expr);

                expression->id = expr->id;
                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                list_free_nodes(&expression->children);

                list_append_new(&expression->children, numb_expr);
                list_append_new(&expression->children, sub_expr);
                return;
            }
        }
        else if (expr->symbol == SUBTRACT) {
            if (expression_is_number(child_expr_2) && child_expr_2->value->value.val.S == 0) {
                expression_t* new_expr = expression_copy(child_expr_1);
                memcpy(expression, new_expr, sizeof(expression_t));
                free(new_expr);

                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
            }
        }
        else if (expr->symbol == MULTIPLY) {
            expression_t* neutral_expr = expression_is_number(child_expr_1) && child_expr_1->value->value.val.S == 1 ? child_expr_1 : (expression_is_number(child_expr_2) && child_expr_2->value->value.val.S == 1 ? child_expr_2 : NULL);
            if (neutral_expr) {
                expression_t* value_expr = neutral_expr == child_expr_1 ? child_expr_2 : child_expr_1;
                expression_t* new_expr = expression_copy(value_expr);
                memcpy(expression, new_expr, sizeof(expression_t));
                free(new_expr);

                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                return;
            }
            expression_t* number_expr = expression_is_number(child_expr_1) && int_has_bit(child_expr_1->value->value.val.S) ? child_expr_1 : (expression_is_number(child_expr_2) && int_has_bit(child_expr_2->value->value.val.S) ? child_expr_2 : NULL);
            if (number_expr) {
                expression_t* other_expr = number_expr == child_expr_1 ? child_expr_2 : child_expr_1;
                int i = 0;
                while (true) if (number_expr->value->value.val.S == (1 << i++)) break;
                other_expr = expression_copy(other_expr);
                number_expr = EXPR_VAL(i-1);

                expression->id = expr_get_by_symbol(state->version, LSHIFT)->id;

                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                list_free_nodes(&expression->children);

                list_append_new(&expression->children, other_expr);
                list_append_new(&expression->children, number_expr);
                return;
            }
        }
        else if (expr->symbol == DIVIDE) {
            if (expression_is_number(child_expr_2) && child_expr_2->value->value.val.S == 1) {
                expression_t* new_expr = expression_copy(child_expr_1);
                memcpy(expression, new_expr, sizeof(expression_t));
                free(new_expr);

                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                return;
            }
            if (expression_is_number(child_expr_2) && int_has_bit(child_expr_2->value->value.val.S)) {
                int i = 0;
                while (true) if (child_expr_2->value->value.val.S == (1 << i++)) break;
                expression_t* other_expr = expression_copy(child_expr_1);
                expression_t* number_expr = EXPR_VAL(i-1);

                expression->id = expr_get_by_symbol(state->version, RSHIFT)->id;

                if (child_expr_1->type == EXPRESSION_VAL) param_free(child_expr_1->value);
                if (child_expr_2->type == EXPRESSION_VAL) param_free(child_expr_2->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                list_free_nodes(&expression->children);

                list_append_new(&expression->children, other_expr);
                list_append_new(&expression->children, number_expr);
                return;
            }
        }
        else if (expr->symbol == EQUAL) {
            if ((child_expr_1->type == EXPRESSION_VAL && child_expr_1->value->value.val.S == 0) ||
            (child_expr_2->type == EXPRESSION_VAL && child_expr_2->value->value.val.S == 0)) {
                expression_t* zero_expr = (child_expr_1->type == EXPRESSION_VAL && child_expr_1->value->value.val.S == 0) ? child_expr_1 : child_expr_2;

                param_free(zero_expr->value);
                expression_free(zero_expr);
                list_free_nodes(&expression->children);

                expression->id = expr_get_by_symbol(state->version, NOT)->id;
                list_append_new(&expression->children, EXPR_SP());
                list_append_new(&expression->children, zero_expr == child_expr_1 ? child_expr_2 : child_expr_1);
            }
        }
        else if (expr->symbol == MODULO) {
            if (expression_is_number(child_expr_2) && int_has_bit(child_expr_2->value->value.val.S)) {
                expression->id = expr_get_by_symbol(state->version, B_AND)->id;
                child_expr_2->value->value.val.S--;
            }
        }
        else if (expr->symbol == TEST) {
            if (expression_is_number(child_expr_2) && int_has_bit(child_expr_2->value->value.val.S)) {
                expression->id = expr_get_by_symbol(state->version, B_AND)->id;
            }
        }
        return;
    }

    thecl_param_t* param = param_new('S');

    int val1 = child_expr_1->value->value.val.S;
    int val2 = child_expr_2->value->value.val.S;

    param->value.val.S = math_preprocess(state, expr->symbol, val1, val2);

    expression->value = param;
    expression->type = EXPRESSION_VAL;
    expression->id = expr_get_by_symbol(state->version, LOAD)->id;

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
        case RSHIFT:   return val2 >= 0 ? val1 >> val2 : val1 << -val2;
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
    gstate->exe = state->ecl;
    gstate->external = state->ecl != state->main_ecl;
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
    thecl_sub_t* sub = malloc(sizeof(thecl_sub_t));

    sub->name = strdup(name);
    list_init(&sub->lines);
    list_init(&sub->instrs);
    sub->stack = 3;
    sub->stack_offset = 0;
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
    sub->self_reference = false;
    sub->mod_trans = false;
    sub->is_external = state->main_ecl != state->ecl;
    sub->last_ins = NULL;
    sub->secondlast_ins = NULL;
    sub->mips_dirty = false;
    list_init(&sub->labels);

    list_append_new(&state->ecl->subs, sub);

    state->current_sub = sub;
    state->scope_bound = 0;

    scope_begin(state);
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
        label_create(state, "inline_end");
        scope_finish(state, false);
    }
    else {
        int bb = state->block_bound;
        bool m = state->scope_stack[state->scope_cnt-1].mips;
        bool r = state->scope_stack[state->scope_cnt-1].returned;
        char buf[512];
        snprintf(buf, 512, "@%s_sub_end", state->current_sub->name);
        label_create(state, buf);
        scope_finish(state, true);
        if (bb || !r) {
            if (!state->mips_mode) {
                if (m) {
                    instr_end_mips(state, state->current_sub);
                }
                instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, RETURN)->id, "SSSSS", 0, 0, 0x25, 0, 2));
            }
            else {
                if (!m) {
                    instr_start_mips(state, state->current_sub);
                }
                instr_return_mips(state, state->current_sub);
            }
        }
        state->ecl->ins_offset += state->current_sub->offset;
    }

    state->current_sub = NULL;
    state->mips_mode = false;
}

static void
scope_begin(
    parser_state_t* state
) {
    if (state->current_sub && state->current_sub->is_inline && state->scope_cnt > 0) {
        list_append_new(&state->current_sub->lines, line_make(LINE_SCOPE_START));
        return;
    }

    kill_delay_slots(state, state->current_sub);

    state->scope_stack = realloc(state->scope_stack, sizeof(thecl_scope_t)*(state->scope_cnt+1));
    state->scope_stack[state->scope_cnt].id = state->scope_id++;
    state->scope_stack[state->scope_cnt].mips = state->scope_cnt >= 1 ? state->scope_stack[state->scope_cnt - 1].mips : state->mips_mode;
    state->scope_stack[state->scope_cnt].returned = false;
    ++state->scope_cnt;
    state->block_bound = 1;
}

static void
scope_finish(
    parser_state_t* state,
    bool pop_vars
) {
    if (state->current_sub && state->current_sub->is_inline && pop_vars) {
        list_append_new(&state->current_sub->lines, line_make(LINE_SCOPE_END));
        return;
    }

    /* pop GOOL stack variables */
    int pop = 0;
    for (int v=0; v < state->current_sub->var_count; ++v)
        if (state->current_sub->vars[v]->scope == state->scope_stack[state->scope_cnt-1].id) {
            state->current_sub->vars[v]->is_unused = true;
            ++pop;
        }
    if (pop > 0 && pop_vars) {
        if (state->mips_mode) {
            state->stack_adjust -= pop * 4;
            mips_stack_adjust(state, state->current_sub);
        }
        else {
            instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, GOTO)->id, "SSSSS", 0, pop, 0x25, 0, 0));
        }
    }

    if (state->scope_cnt > 1 && state->scope_stack[state->scope_cnt-2].mips != state->scope_stack[state->scope_cnt-1].mips) {
        if (state->scope_stack[state->scope_cnt-2].mips) {
            instr_start_mips(state, state->current_sub);
        }
        else {
            instr_end_mips(state, state->current_sub);
        }
    }

    kill_delay_slots(state, state->current_sub);

    state->scope_bound = state->current_sub->offset;

    state->scope_stack = realloc(state->scope_stack, sizeof(thecl_scope_t)*--state->scope_cnt);
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

    sub->args = realloc(sub->args, ++sub->arg_count * sizeof(thecl_variable_t*));
    sub->args[sub->arg_count - 1] = arg;

    for (int i = 0; i < sub->arg_count; ++i) {
        sub->args[i]->stack = -sub->arg_count + i;
    }

    return arg;
}

static void
arg_delete(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name)
{
    if (!arg_exists(state, sub, name)) {
        yyerror(state, "nonexistent argument: %s", name);
        return;
    }

    int i;
    for (i=0;i<sub->arg_count;++i) {
        if (!strcmp(sub->args[i]->name, name))
            break;
    }

    free(sub->args[i]);
    memmove(sub->args+i, sub->args+i+1, sub->arg_count-i-1);
    sub->args = realloc(sub->args, --sub->arg_count * sizeof(thecl_variable_t*));

    for (i = 0; i < sub->arg_count; ++i) {
        sub->args[i]->stack = -sub->arg_count + i;
    }
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
    if (objfield_get(state, name) == NULL) {
        yyerror(state, "nonexistant object variable: %s", name);
    }

    int i;
    for (i=0;i<state->main_ecl->var_count;++i) {
        if (!strcmp(state->main_ecl->vars[i]->name, name))
            break;
    }

    free(state->main_ecl->vars[i]);
    memmove(state->main_ecl->vars+i, state->main_ecl->vars+i+1, state->main_ecl->var_count-i-1);
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
    var->scope = state->scope_stack[state->scope_cnt - 1].id;

    ++sub->var_count;
    sub->vars = realloc(sub->vars, sub->var_count * sizeof(thecl_variable_t*));
    sub->vars[sub->var_count - 1] = var;

    if (var->stack == sub->stack) /* Only increment the stack if the variable uses a new offset. */
        ++sub->stack;

    if (sub->is_inline && push) { /* do not make instructions for inline subs */
        list_append_new(&state->current_sub->lines, line_make_var_decl(name));
        return var;
    }

    if (state->mips_mode) {
        state->stack_adjust += 4;
    }
    else if (push) {
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

    if (sub->is_inline) { /* do not make instructions for inline subs */
        list_append_new(&state->current_sub->lines, line_make_var_decl_assign(name, expr));
        return var;
    }

    if (state->mips_mode) {
        thecl_param_t* param = param_new('S');
        param->val_type = PARAM_FIELD;
        param->value.val.S = var->stack > 0 ? var->stack + sub->stack_offset : var->stack;
        param->object_link = -1;
        var_assign(state, param, expr);
    }
    else {
        if (expr->type == EXPRESSION_VAL) {
            thecl_param_t* param = expr->value;
            const expr_t* expr_load = expr_get_by_symbol(state->version, param->val_type == PARAM_POINTER ? PLOAD : LOAD);
            instr_add(state, state->current_sub, instr_new(state, expr_load->id, "p", param));
        } else {
            expression_output(state, expr);
        }
        expression_free(expr);
    }

    return var;
}

static bool
var_accessible(
    parser_state_t* state,
    thecl_variable_t* var)
{
    for (int scope_state=0; scope_state<state->scope_cnt; ++scope_state) {
        if (state->scope_stack[scope_state].id == var->scope)
            return true;
    }
    return false;
}

static thecl_variable_t*
arg_get(
    parser_state_t* state,
    thecl_sub_t* sub,
    const char* name)
{
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
    const char* name)
{
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
    const char* name)
{
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
var_assign(
    parser_state_t* state,
    thecl_param_t* param,
    expression_t* expr_assign)
{
    if (state->current_sub->is_inline) {
        if (param->val_type == PARAM_FIELD && param->object_link == -1 && param->value.val.S < 0) {
            yyerror(state, "inline subs cannot write to their arguments");
            exit(2);
        }
        list_append_new(&state->current_sub->lines, line_make_assignment(param, expr_assign));
        return;
    }
    const expr_t* expr = expr_get_by_id(state->version, expr_assign->id);
    thecl_param_t* src_param = expr != NULL && expr->is_unary ? ((expression_t*)list_head(&expr_assign->children))->value : NULL;
    if (var_is_valid_field_ref(state, param) && src_param && src_param->value.val.S == 0x1F && src_param->val_type == PARAM_FIELD && src_param->object_link == 0) {
        src_param->value.val.S = param->value.val.S;
        src_param->object_link = param->object_link;
        if (param->object_link == -1 && param->value.val.S >= 3) {
            state->current_sub->vars[param->value.val.S - 3]->is_written = true;
        }
        else if (param->object_link == -1 && param->value.val.S < 0) {
            state->current_sub->args[-param->value.val.S - 1]->is_written = true;
        }
        param_free(param);
        expression_output(state, expr_assign);
        expression_free(expr_assign);
        return;
    }
    else if (expr && expr->symbol == MOVC
    && ((expression_t*)expr_assign->children.tail->data)->value->value.val.S == 0x1F
    && param->val_type == PARAM_FIELD && param->object_link == 0 && param->value.val.S >= 0 && param->value.val.S <= 0x3F) {
        ((expression_t*)expr_assign->children.tail->data)->value->value.val.S = param->value.val.S;
        param_free(param);
        expression_output(state, expr_assign);
        expression_free(expr_assign);
        return;
    }

    if (state->mips_mode) {
        expression_output(state, expr_assign);
        expression_free(expr_assign);
        mips_instr_new_store(state, param);
        if (param->object_link == -1 && param->value.val.S >= 3) {
            state->current_sub->vars[param->value.val.S - 3]->is_written = true;
        }
        else if (param->object_link == -1 && param->value.val.S < 0) {
            state->current_sub->args[-param->value.val.S - 1]->is_written = true;
        }
    }
    else {
        if (!var_is_valid_field_ref(state, param) && param->val_type == PARAM_FIELD) {
            expression_output(state, expr_assign);
            instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, MISC)->id, "SSSS", param->value.val.S << 8, param->object_link, 0, 4));
        }
        else if (expr_assign->type == EXPRESSION_VAL && (param->val_type == PARAM_FIELD || ((param->val_type == PARAM_GLOBAL || param->val_type == PARAM_COLOR) && !expr->is_unary))) {
            src_param = expr_assign->value;
        } else {
            expression_output(state, expr_assign);
            src_param = param_sp_new();
        }
        expression_free(expr_assign);
        if (var_is_valid_field_ref(state, param)) {
            instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, src_param->val_type == PARAM_POINTER ? PASSIGN : ASSIGN)->id, "pp", param, src_param));

            if (param->object_link == -1 && param->value.val.S >= 3) {
                state->current_sub->vars[param->value.val.S - 3]->is_written = true;
            }
            else if (param->object_link == -1 && param->value.val.S < 0) {
                state->current_sub->args[-param->value.val.S - 1]->is_written = true;
            }
        } else if (param->val_type == PARAM_GLOBAL) { /* WGL */
            instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, GASSIGN)->id, "Sp", param->value.val.S, src_param));
        } else if (param->val_type == PARAM_COLOR) { /* CVMW */
            instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, CASSIGN)->id, "pSS", src_param, param->object_link, param->value.val.S));
        }
    }
}

static void
var_shorthand_assign(
    parser_state_t* state,
    thecl_param_t* param,
    expression_t* expr_assign,
    int EXPR)
{
    /* Can't use the same param twice, so a copy is created. */
    expression_t* expr_main = EXPR_2(EXPR, expression_load_new(state, param_copy(param)), expr_assign);
    var_assign(state, param, expr_main);
}

static void
mips_instr_new_store(
    parser_state_t* state,
    thecl_param_t* value)
{
    verify_reg_load(state, &state->top_reg, NULL);
    int val = value->value.val.S;
    if (value->val_type == PARAM_FIELD) { /* object field, stack */
        if (value->object_link == 0) { /* object field */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", val * 4 + get_obj_proc_offset(state->version), state->top_reg->index, get_reg(state->reg_block, "s0")->index));
        }
        else if (value->object_link >= 1 && value->object_link <= 7) { /* linked object field */
            mips_reg_t* link_reg = get_usable_reg(state->reg_block);
            if (link_reg) {
                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", value->object_link * 4 + get_obj_proc_offset(state->version), link_reg->index, get_reg(state->reg_block, "s0")->index));
                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", val * 4 + get_obj_proc_offset(state->version), state->top_reg->index, link_reg->index));
                link_reg->status = MREG_STATUS_USED;
            }
            else {
                yyerror(state, "no available registers for mips mode");
                exit(2);
            }
        }
        else if (value->object_link == -1) { /* stack */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", val * 4, state->top_reg->index, get_reg(state->reg_block, "s7")->index));
        }
    }
    else if (value->val_type == PARAM_GLOBAL) { /* global */
        mips_reg_t* reg = get_usable_reg(state->reg_block);
        if (reg) {
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", 0x58, reg->index, get_reg(state->reg_block, "s8")->index));
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", (val >> 8) * 4, state->top_reg->index, reg->index));
            reg->status = MREG_STATUS_USED;
        }
        else {
            yyerror(state, "no available registers for mips mode");
            exit(2);
        }
    }
    else if (value->val_type == PARAM_COLOR) { /* color field */
        if (value->object_link == 0) { /* color field */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sh", val * 2 + 0x20, state->top_reg->index, get_reg(state->reg_block, "s0")->index));
        }
        else if (value->object_link >= 1 && value->object_link <= 7) { /* linked color field */
            mips_reg_t* link_reg = get_usable_reg(state->reg_block);
            if (link_reg) {
                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", value->object_link * 4 + get_obj_proc_offset(state->version), link_reg->index, get_reg(state->reg_block, "s0")->index));
                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sh", val * 2 + 0x20, state->top_reg->index, link_reg->index));
                link_reg->status = MREG_STATUS_USED;
            }
            else {
                yyerror(state, "no available registers for mips mode");
                exit(2);
            }
        }
    }
    SetUsedReg(state->top_reg);
}

static void
label_create(
    parser_state_t* state,
    char* name)
{
    if (state->current_sub->is_inline) {
        list_append_new(&state->current_sub->lines, line_make_label(strdup(name)));
        return;
    }
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
