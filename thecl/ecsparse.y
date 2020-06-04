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
static void instr_add(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_add_delay_slot(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_del(parser_state_t* state, thecl_sub_t* sub, thecl_instr_t* instr);
static void instr_prepend(thecl_sub_t* sub, thecl_instr_t* instr);
/* Returns true if the created call was inline. */
static bool instr_create_call(parser_state_t *state, uint8_t type, char *name, list_t *params);
/*static bool instr_create_inline_call(parser_state_t *state, thecl_sub_t *sub, list_t *params);*/

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

/* macros for expression_mips_operation */
#define OutputExprToReg(EXPR, OP) \
    if (EXPR->type == EXPRESSION_VAL && EXPR->value->stack == 0 && EXPR->value->value.val.S == 0) OP = get_reg(state->reg_block, "zr"); else { expression_output(state, EXPR); OP = state->top_reg; }
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
%token AT "@"
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
%token PASSIGN
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
%token ATAN "atan"
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
%token NTRY4 "ntry4"
%token NTRY5 "ntry5"

%type <list> Address_List
%type <list> Expression_List
%type <list> Instruction_Parameters_List
%type <list> Instruction_Parameters
%type <list> ParenExpressionList
%type <list> ParenExpressionListNoScope

%type <expression> Expression
%type <expression> ExpressionSubsetInstParam
%type <expression> ExpressionLoadType
%type <expression> ExpressionSubset

%type <param> Instruction_Parameter
%type <param> Address
%type <param> Integer
%type <param> Entry
//%type <param> Text
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
%left ADD SUBTRACT
%left MULTIPLY DIVIDE MODULO
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
      }
      "(" ArgumentDeclaration ")" Subroutine_Modifiers Subroutine_Body {
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

        state->current_interrupt = calloc(sizeof(thecl_interrupt_t), 1);
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
      }
      Subroutine_Body {
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
      }
      "(" ArgumentDeclaration ")" Subroutine_Body {
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
    | Instructions DIRECTIVE {
        free($2);
    }
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
    } CodeBlock {
        state->mips_mode = false;
    }

SaveBlock:
    "save" "(" Address_List ")" {
        const expr_t* local_expr = expr_get_by_symbol(state->version, LOAD);
        const expr_t* global_expr = expr_get_by_symbol(state->version, GLOAD);
        if ($3 == NULL)
            $3 = list_new();
        list_node_t *n, *x;
        list_for_each_node_safe($3, n, x) {
            list_append(&state->addresses, n);
            thecl_param_t* param = n->data;
            if (param->stack == 2) {
                instr_add(state, state->current_sub, instr_new(state, global_expr->id, "S", param->value.val.S));
            }
            else {
                instr_add(state, state->current_sub, instr_new(state, local_expr->id, "p", param));
            }
        }
        list_append_new(&state->addresses, list_count($3));
        free($3);
    } CodeBlock {
        int m = list_tail(&state->addresses);
        list_del_tail(&state->addresses);

        const expr_t* local_expr = expr_get_by_symbol(state->version, ASSIGN);
        const expr_t* global_expr = expr_get_by_symbol(state->version, GASSIGN);
        for (int i=0; i<m; ++i) {
            thecl_param_t* param = list_tail(&state->addresses);
            if (param->stack == 2) {
                instr_add(state, state->current_sub, instr_new(state, global_expr->id, "Sp", param->value.val.S, param_sp_new()));
                param_free(param);
            }
            else {
                instr_add(state, state->current_sub, instr_new(state, local_expr->id, "pp", param_copy(param), param_sp_new()));
            }
            list_del_tail(&state->addresses);
        }
    }

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
              if (expr->type == EXPRESSION_VAL && !expr->value->stack && !expr->value->value.val.S) {
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
              if (expr->type == EXPRESSION_VAL && !expr->value->stack && expr->value->value.val.S) {
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
      IDENTIFIER "(" Instruction_Parameters ")" {
        const gool_ins_t* gool_ins = gool_ins_get_by_name(state->version, $1);
        if (gool_ins) {
            if (gool_ins->varargs != 0) {
                list_t* param_list = list_new();
                list_t* arg_list = list_new();

                int argc = gool_ins->varargs;
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
                        else if (i - 1 == gool_ins->late_param) {
                            late_param = param;
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
                            if (state->top_reg) {
                                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                                state->stack_adjust += 4;
                            }
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
                        if (param->stack == 3) {
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
                        if (state->top_reg) {
                            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                            state->stack_adjust += 4;
                        }
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
                            expression_output(state, expression);
                            if (state->top_reg) {
                                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                                state->stack_adjust += 4;
                            }
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

                if (late_param) {
                    if ((late_expr && late_expr->type != EXPRESSION_VAL) || (late_param->stack == 1 && late_param->object_link != 0) || (late_param->stack == 0 && late_param->object_link != -1) || (late_param->stack != 0 && late_param->stack != 1) || late_param->value.val.S < 0 || late_param->value.val.S > 0x3F) {
                        if (late_expr) {
                            expression_output(state, late_expr);
                            if (state->top_reg) {
                                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                                state->stack_adjust += 4;
                            }
                            expression_free(late_expr);
                        }
                        late_param->stack = 1;
                        late_param->object_link = 0;
                        late_param->value.val.S = 0x1F;
                    }
                }

                instr_add(state, state->current_sub, instr_new_list(state, gool_ins->id, gool_ins->param_list_validate(param_list, list_count(arg_list))));

                if (gool_ins->pop_args) {
                    if (argc = list_count(arg_list)) {
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
            else {
                expression_t* expression;
                list_for_each(&state->expressions, expression) {
                    expression_output(state, expression);
                    if (state->top_reg) {
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                        state->stack_adjust += 4;
                    }
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
                        if (state->top_reg) {
                            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                            state->stack_adjust += 4;
                        }
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
    /* | "@" IDENTIFIER "(" Instruction_Parameters ")" {
        const expr_t* expr = expr_get_by_symbol(state->version, CALL);
        size_t param_count = $4 ? list_count($4) : 0;
        thecl_sub_t* sub = state->current_sub;
        list_for_each(&state->main_ecl->subs, sub) {
            if (sub != state->current_sub && !sub->self_reference && !strcmp(sub->name, $2) && sub->arg_count == param_count) {
                instr_create_inline_call(state, sub, $4);
                sub = NULL;
            }
        }
        if (sub) {
            yyerror(state, "no valid inline call for %s", $2);
        }
        free($2);
        if ($4 != NULL) {
            list_free_nodes($4);
            free($4);
        }
      } */
    /*| "goto" IDENTIFIER {
        expression_create_goto(state, GOTO, $2, NULL);
    }*/
    | Assignment
    | VarDeclaration
    | BreakStatement
    | ContinueStatement
    | "return" {
        state->scope_stack[state->scope_cnt-1].returned = true;
        if (state->current_sub->is_inline)
            expression_create_goto(state, GOTO, "inline_end", NULL);
        else {/*
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
      Load_Type {
          if ($1->stack == 2) {
              list_prepend_new(&state->expressions, expression_load_new(state, $1));

              $$ = param_new('S');
              $$->stack = 1;
              $$->object_link = 0;
              $$->is_expression_param = 1;
              $$->value.val.S = 0x1F;
          }
      }
    | Pointer_Type {
          list_prepend_new(&state->expressions, expression_pointer_new(state, $1));

          $$ = param_new('S');
          $$->stack = 1;
          $$->object_link = 0;
          $$->is_expression_param = 1;
          $$->value.val.S = 0x1F;
      }
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

Expression_List:
    %empty { $$ = NULL; }
    | Expression { $$ = list_new(); list_append_new($$, $1); }
    | Expression_List "," Expression { $$ = $1; list_append_new($$, $3); }

Expression:
      ExpressionLoadType
    | ExpressionSubset
    ;

ExpressionSubsetInstParam:
      ExpressionSubset
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
    | "!" Expression              { $$ = EXPR_2(NOT,      expression_load_new(state, param_sp_new()), $2); }
    | "~" Expression              { $$ = EXPR_2(B_NOT,    expression_load_new(state, param_sp_new()), $2); }
    | Expression "||"  Expression { $$ = EXPR_2(OR,       $1, $3); }
    | Expression "&&"  Expression { $$ = EXPR_2(AND,      $1, $3); }
    | Expression "^"   Expression { $$ = EXPR_2(XOR,      $1, $3); }
    | Expression "|"   Expression { $$ = EXPR_2(B_OR,     $1, $3); }
    | Expression "&"   Expression { $$ = EXPR_2(B_AND,    $1, $3); }
    | Expression "<<"  Expression { $$ = EXPR_2(LSHIFT,   $1, $3); }
    | Expression ">>"  Expression { $$ = EXPR_2(RSHIFT,   $1, $3); }
    | Expression "\\"  Expression { $$ = EXPR_2(TEST,     $1, $3); }
    | Expression "!="  Expression {
        $$ = EXPR_2(INEQUAL,  $1, $3);
        $$ = EXPR_2(NOT,      expression_load_new(state, param_sp_new()), $$);
      }
    | "+" Expression              { $$ = $2; }
    | "-" Expression              { $$ = EXPR_2(SUBTRACT, expression_val_new(state, 0), $2); }
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
    | Address "[" Expression "]"                                  { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, expression_load_new(state, $1), expression_val_new(state, 5), $3, expression_val_new(state, 0));
                                                                    else $$ = EXPR_2(ARRL, expression_load_new(state, $1), $3);
                                                                  }
    | "getval" "(" Expression "," Expression ")"                  { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, expression_val_new(state, 5), $5, expression_val_new(state, 0)); }
    | "distance" "(" Expression "," Expression ")"                { $$ = EXPR_4(MISC, expression_load_new(state, param_null_new()), $3, $5, expression_val_new(state, 1)); }
    | "atan" "(" Expression "," Expression ")"                    { $$ = EXPR_2(ATAN, $3, $5); }
    | "atan2" "(" Expression "," Expression ")"                   { $$ = EXPR_4(MISC, $5, $3, expression_val_new(state, 0), expression_val_new(state, 2)); }
    | "getfield" "(" Expression "," Expression ")"                { $$ = EXPR_4(MISC, $5, $3, expression_val_new(state, 0), expression_val_new(state, 3)); }

    | "atan2_mirrored" "(" Expression ")"                         { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, expression_load_new(state, param_null_new()), $3, expression_val_new(state, 0), expression_val_new(state, 5)); }
    | "distance" "(" Expression "," Expression "," Expression ")" { $$ = EXPR_4(MISC, $3, $5, $7, expression_val_new(state, 6)); }
    | "objectget" "(" Expression ")"                              { $$ = EXPR_4(MISC, $3, expression_val_new(state, 5), expression_val_new(state, 0), expression_val_new(state, 7)); }

    | "entitygetstate" "(" Expression ")"                         { $$ = EXPR_4(MISC, expression_load_new(state, param_var_new(field_get("id")->offset)), expression_val_new(state, 0), $3, expression_val_new(state, 11)); }
    | "entitygetstate" "(" Expression "," Expression ")"          { $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 11)); }
//  | "gamefunc" "(" Expression "," Expression ")"                { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 12)); }
    | "getvalideventobj" "(" Expression "," Expression "," Expression ")"   { $$ = EXPR_4(MISC, $3, $5, $7, expression_val_new(state, 13)); }
    | "getvalideventobj" "(" Expression "," Expression ")"        { $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 13)); }
    | "iscolliding" "(" Expression "," Expression ")"             { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, $5, expression_val_new(state, 0), expression_val_new(state, 14)); }
//  | "__unk2" "(" Expression "," Expression ")"                  { if (!is_post_c2(state->version)) $$ = EXPR_4(MISC, $3, expression_val_new(state, 0), $5, expression_val_new(state, 15)); }

    | "tryload" "(" Expression ")"                                { $$ = EXPR_2(NTRY, $3, expression_val_new(state, 3)); }
    | "ntry5" "(" Expression_List ")" {
        if ($3 != NULL) {
            $$ = EXPR_2(NTRY, expression_val_new(state, list_count($3)), expression_val_new(state, 5));
            expression_t* expr;
            list_for_each($3, expr) {
                list_append_new(&$$->children, expr);
            }
            list_free_nodes($3);
            free($3);
        }
        else {
            $$ = EXPR_2(NTRY, expression_val_new(state, 0), expression_val_new(state, 5));
        }
      }
    | "ntry4" "(" ")"                                             { $$ = EXPR_2(NTRY, expression_load_new(state, param_null_new()), expression_val_new(state, 4)); }

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
    | NIL {
        $$ = param_new('S');
        $$->stack = 1;
        $$->value.val.S = 0;
        $$->object_link = -2;
    }
    ;

Entry:
      ENTRY {
        $$ = param_new('S');
        $$->value.val.S = gool_to_eid($1);
        gool_pool_force_get_index(state->main_ecl, $$->value.val.S);
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
          if ($2->stack == 0) {
              $2->object_link = -3;
          }
          $2->stack = 3;
          $$ = $2;
      }
    ;

Literal_Int:
      INTEGER
    | "-" INTEGER { $$ = -$2; }
    | "+" INTEGER { $$ = +$2; }
    ;
%%

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
    if (state->ignore_block || (state->scope_cnt > 0 && state->scope_stack[state->scope_cnt-1].returned)) {
        thecl_instr_free(instr);
        return;
    }
    if (!sub->mips_dirty && ((state->scope_cnt > 0 && instr->mips != state->scope_stack[state->scope_cnt-1].mips) || (instr->mips && state->force_mips))) {
        sub->mips_dirty = true;
        state->scope_stack[state->scope_cnt-1].mips = instr->mips;
        if (instr->mips) {
            instr_start_mips(state, sub);
        }
        else {
            instr_end_mips(state, sub);
        }
        if (state->force_mips) state->force_mips = false;
        sub->mips_dirty = false;
    }
    if (instr->mips) {
        if (instr->ins.ins != 0) {
            bool ret = false;
            thecl_instr_t* last_ins;
            if (!mips_instr_is_branch(&instr->ins)) {
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
                list_node_t* node;
                list_for_each_node(&state->delay_slots, node) {
                    gooc_delay_slot_t* delay_slot = node->data;
                    thecl_instr_t* slot_ins = delay_slot->slot->data;
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
                if (!ret && instr->reg_stalled && sub->last_ins && sub->last_ins->mips && sub->last_ins->ins.ins != 0 && (sub->last_ins->reg_used & instr->reg_used) == 0) { /* swap with previous instruction */
                    list_prepend_to(&sub->instrs, instr, sub->instrs.tail);
                    instr->offset = sub->last_ins->offset;
                    sub->last_ins->offset = sub->offset++;
                    ret = true;
                }
                else if (sub->offset > state->scope_bound && ret && instr->reg_stalled) {
                    list_node_t *instr_node = NULL;
                    list_for_each_node(&sub->instrs, node) {
                        if (node->data == instr) {
                            instr_node = node;
                            break;
                        }
                    }
                    if (instr_node && instr_node->prev) {
                        list_node_t *last_node = instr_node->prev;
                        last_ins = last_node->data;
                        if (last_ins && last_ins->mips && last_ins->ins.ins != 0 && !mips_instr_is_branch(&last_ins->ins) && (last_ins->reg_used & instr->reg_used) == 0) {
                            last_node->data = instr;
                            instr_node->data = last_ins;
                            int temp_off = last_ins->offset;
                            last_ins->offset = instr->offset;
                            instr->offset = temp_off;
                            ret = true;
                        }
                    }
                }
            }
            last_ins = list_tail(&sub->instrs);
            if (!ret && last_ins && last_ins->mips && (instr->reg_used & last_ins->reg_stalled || ((mips_instr_is_branch(&instr->ins) || mips_instr_is_store(&instr->ins)) && mips_instr_is_branch(&last_ins->ins)))) {
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
            if (load_param->value.val.S == 0x1f && load_param->object_link == 0 && load_param->stack) {
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
            if (load_param->value.val.S == 0x1f && load_param->object_link == 0 && load_param->stack) {
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
                if (param->value.val.S != 0x1F || !param->stack || param->object_link != 0)
                    goto NO_OPTIM;

                if (!is_post_c2(state->version)) {
                    param = instr->params.tail->prev->data;
                    param->value.val.S = param->value.val.S == 1 ? 2 : 1;
                } else {
                    instr->id = instr->id == beqz_expr->id ? bnez_expr->id : beqz_expr->id;
                }

                param = list_tail(&sub->last_ins->params);

                if (param->value.val.S <= 0x3F && param->value.val.S >= 0 && param->stack == 1 && param->object_link == 0) {
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
                    if (!param->stack) {
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

static void
instr_create_inline_call(
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
            if (param->is_expression_param) {
                expression_output(state, state->expressions.head->data);
                expression_free(state->expressions.head->data);
                list_del_head(&state->expressions);
            }

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
                thecl_instr_t* last_ins = state->current_sub->last_ins;
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
                } else if (current_expr->type == EXPRESSION_OP || current_expr->type == EXPRESSION_GLOBAL) {
                    expression_output(state, current_expr);
                    if (state->top_reg) {
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                        state->stack_adjust += 4;
                    }
                    list_del(&state->expressions, last_node);
                    expression_free(current_expr);
                }
            }

            instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, LOAD)->id, "p", param));

            ++argc;
        }
    }

    if (!strcmp(state->current_sub->name, name)) {
        state->current_sub->self_reference = true;
    }

    instr_add(state, state->current_sub, instr_new(state, type, "pSS", name_param, 38, argc));
    return false;
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
            reg->saved_param = expr->type == EXPRESSION_VAL ? param_copy(expr->value) : NULL;
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
    if (state->mips_mode) {
        if (type != GOTO) {
            expression_output(state, cond);
            verify_reg_load(state, &state->top_reg, cond);
            state->top_reg->status = MREG_STATUS_USED;
        }
        switch (type) {
            case IF: instr_add(state, state->current_sub, MIPS_INSTR_BNEZ(strdup(labelstr), state->top_reg->index)); break;
            case UNLESS: instr_add(state, state->current_sub, MIPS_INSTR_BEQZ(strdup(labelstr), state->top_reg->index)); break;
            case GOTO: default: instr_add(state, state->current_sub, MIPS_INSTR_BEQZ(strdup(labelstr), 0)); break;
        }
    }
    else {
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
        instr_add(state, state->current_sub, instr_new(state, expr_get_by_symbol(state->version, type)->id, "pSpSS", p1, pop, pcond, state->version == 1 ? (type == GOTO ? 0 : (type == IF ? 1 : (type == UNLESS ? 2 : 3))) : 0, 0));
    }
}

static void
expression_mips_load(
    parser_state_t* state,
    expression_t* expr)
{
    thecl_param_t* param = expr->value;
    mips_reg_t* reg = NULL;
    if (!((param->stack == 1 && !(param->object_link >= -1 && param->object_link <= 7)) || param->stack == 3)) {
        reg = request_reg(state, expr);
    }
    if (param->stack == 0) { /* number */
        if (param->value.val.S == 0) {
            instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("addu", reg->index, 0, 0));
        }
        else if (param->value.val.S > 0x7FFF || param->value.val.S < -0x8000) {
            instr_add(state, state->current_sub, MIPS_INSTR_I("lui", param->value.val.S >> 16 & 0xFFFF, reg->index, 0));
            instr_add(state, state->current_sub, MIPS_INSTR_I("ori", param->value.val.S & 0xFFFF, reg->index, reg->index));
        }
        else {
            instr_add(state, state->current_sub, MIPS_INSTR_I("ori", param->value.val.S & 0xFFFF, reg->index, 0));
        }
    }
    else if (param->stack == 1) { /* object field, stack */
        if (param->object_link == 0) { /* object field */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", param->value.val.S * 4 + get_obj_proc_offset(state->version), reg->index, get_reg(state->reg_block, "s0")->index));
        }
        else if (param->object_link >= 1 && param->object_link <= 7) { /* linked object field */
            mips_reg_t* link_reg = get_usable_reg(state->reg_block);
            if (!link_reg) link_reg = reg;
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", param->object_link * 4 + get_obj_proc_offset(state->version), link_reg->index, get_reg(state->reg_block, "s0")->index));
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", param->value.val.S * 4 + get_obj_proc_offset(state->version), reg->index, link_reg->index));
            if (link_reg != reg) {
                link_reg->status = MREG_STATUS_USED;
            }
        }
        else if (param->object_link == -1) { /* stack */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", param->value.val.S * 4, reg->index, get_reg(state->reg_block, "s7")->index));
        }
        else { /* other */
            instr_add(state, state->current_sub, instr_new(state, expr->id, "p", expr->value));
        }
    }
    else if (param->stack == 2) { /* global */
        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", 0x58, reg->index, get_reg(state->reg_block, "s8")->index));
        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", (param->value.val.S >> 8) * 4, reg->index, reg->index));
    }
    else if (param->stack == 3) { /* pointer */
        instr_add(state, state->current_sub, instr_new(state, expr->id, "p", expr->value));
    }
    state->top_reg = reg;
}

static void
expression_mips_operation(
    parser_state_t* state,
    expression_t* expr)
{
    mips_reg_t* ret = NULL, *op1, *op2;
    expression_t* val_expr, *var_expr, *child_expr1, *child_expr2, *child_expr3;
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
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->stack == 0 && child_expr1->value->value.val.S >= -0x8000 && child_expr1->value->value.val.S <= 0x7FFF) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->stack == 0 && child_expr2->value->value.val.S >= -0x8000 && child_expr2->value->value.val.S <= 0x7FFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr && !(var_expr->type == EXPRESSION_VAL && var_expr->value->stack == 0 && var_expr->value->value.val.S == 0)) {
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
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->stack == 0 && child_expr1->value->value.val.S == 0) {
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("subu", ret->index, op2->index, 0));
                SetUsedReg(op2);
            }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->stack == 0 && child_expr2->value->value.val.S >= -0x7FFF && child_expr2->value->value.val.S <= 0x8000) {
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
        case AND:
        case B_OR:
            const char* oprname, *opiname;
            switch (symbol) {
                case XOR: oprname = "xor"; opiname = "xori"; break;
                case OR: case B_OR: oprname = "or"; opiname = "ori"; break;
                case AND: oprname = "and"; opiname = "andi"; break;
            }
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->stack == 0 && child_expr1->value->value.val.S >= -0x8000 && child_expr1->value->value.val.S <= 0x7FFF) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->stack == 0 && child_expr2->value->value.val.S >= -0x8000 && child_expr2->value->value.val.S <= 0x7FFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr && !(var_expr->type == EXPRESSION_VAL && var_expr->value->stack == 0 && var_expr->value->value.val.S == 0)) {
                OutputExprToReg(var_expr, op1);
                verify_reg_load(state, &op1, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_I(opiname, val_expr->value->value.val.S, ret->index, op1->index));
                SetUsedReg(op1);
            }
            else {
                OutputExprToReg(child_expr1, op1);
                OutputExprToReg(child_expr2, op2);
                verify_reg_load(state, &op2, child_expr2);
                verify_reg_load(state, &op1, child_expr1);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R(oprname, ret->index, op2->index, op1->index));
                SetUsedReg(op1);
                SetUsedReg(op2);
            }
            break;
        case NOT:
            OutputExprToReg(child_expr2, op1);
            verify_reg_load(state, &op1, child_expr2);
            ret = request_reg(state, expr);
            instr_add(state, state->current_sub, MIPS_INSTR_I("sltiu", 1, ret->index, op1->index));
            SetUsedReg(op1);
            break;
        case EQUAL:
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->stack == 0 && child_expr1->value->value.val.S >= -0x8000 && child_expr1->value->value.val.S <= 0x7FFF) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->stack == 0 && child_expr2->value->value.val.S >= -0x8000 && child_expr2->value->value.val.S <= 0x7FFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr && !(var_expr->type == EXPRESSION_VAL && var_expr->value->stack == 0 && var_expr->value->value.val.S == 0)) {
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
        case LT:
        case GT:
        case GTEQ:
            if (symbol == GT) {
                val_expr = child_expr1;
                child_expr1 = child_expr2;
                child_expr2 = val_expr;
                val_expr = NULL;
            }
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->stack == 0 && child_expr1->value->value.val.S >= -0x7FFF && child_expr1->value->value.val.S <= 0x8000) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->stack == 0 && child_expr2->value->value.val.S >= -0x8000 && child_expr2->value->value.val.S <= 0x7FFF) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr == child_expr1 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->stack == 0 && var_expr->value->value.val.S == 0)) { /* imm < b --> -b < -imm */
                OutputExprToReg(var_expr, op2);
                verify_reg_load(state, &op2, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("subu", ret->index, op2->index, 0));
                instr_add(state, state->current_sub, MIPS_INSTR_I("slti", -val_expr->value->value.val.S, ret->index, ret->index));
                SetUsedReg(op2);
            }
            else if (val_expr == child_expr2 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->stack == 0 && var_expr->value->value.val.S == 0)) { /* a < imm */
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
            if (child_expr1->type == EXPRESSION_VAL && child_expr1->value->stack == 0 && child_expr1->value->value.val.S >= -0x7FFF && child_expr1->value->value.val.S <= 0x8000) { val_expr = child_expr1; var_expr = child_expr2; }
            else if (child_expr2->type == EXPRESSION_VAL && child_expr2->value->stack == 0 && child_expr2->value->value.val.S >= -0x8001 && child_expr2->value->value.val.S <= 0x7FFE) { val_expr = child_expr2; var_expr = child_expr1; }
            if (val_expr == child_expr1 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->stack == 0 && var_expr->value->value.val.S == 0)) { /* imm <= b --> imm < b+1 --> imm-1 < b --> -b < -(imm-1) */
                OutputExprToReg(var_expr, op2);
                verify_reg_load(state, &op2, var_expr);
                ret = request_reg(state, expr);
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("subu", ret->index, op2->index, 0));
                instr_add(state, state->current_sub, MIPS_INSTR_I("slti", -(val_expr->value->value.val.S-1), ret->index, ret->index));
                SetUsedReg(op2);
            }
            else if (val_expr == child_expr2 && !(var_expr->type == EXPRESSION_VAL && var_expr->value->stack == 0 && var_expr->value->value.val.S == 0)) { /* a <= imm --> a < imm+1*/
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
                instr_add(state, state->current_sub, MIPS_INSTR_ALU_R("addiu", 1, ret->index, op2->index));
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
        default:
            {
            /* a normal GOOL instruction will destroy the registers
             * s5, v0, v1, a0 and a1 for sure, not to mention any
             * other register used by any functions called from it */
            list_t* saved_regs = list_new();
            mips_reg_t* temp_reg;
            for (int i=0; i<34; ++i) {
                if (state->reg_block->regs[i].status == MREG_STATUS_IN_USE) {
                    state->reg_block->regs[i].status = MREG_STATUS_RESERVED;
                    list_prepend_new(saved_regs, &state->reg_block->regs[i]);
                    if (i<32) {
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->reg_block->regs[i].index, get_reg(state->reg_block, "s6")->index));
                    }
                    else {
                        temp_reg = request_reg(state, NULL);
                        if (i == get_reg(state->reg_block, "lo")->index) {
                            instr_add(state, state->current_sub, MIPS_INSTR_MFLO(temp_reg->index));
                        }
                        else if (i == get_reg(state->reg_block, "hi")->index) {
                            instr_add(state, state->current_sub, MIPS_INSTR_MFHI(temp_reg->index));
                        }
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, temp_reg->index, get_reg(state->reg_block, "s6")->index));
                        free_reg(temp_reg);
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
                if (child_expr->type == EXPRESSION_VAL && child_expr->value->stack != 3 && (!expression->has_double_param || (expression->has_double_param && lc <= 2) || (expression->has_double_param && lc > 2 && c == 1))) {
                    list_append_new(param_list, child_expr->value);
                }
                else {
                    expression_output(state, child_expr);
                    if (state->top_reg) {
                        instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", state->stack_adjust, state->top_reg->index, get_reg(state->reg_block, "s6")->index));
                        state->stack_adjust += 4;
                    }
                    if (child_expr->type == EXPRESSION_VAL && child_expr->value->stack == 3) {
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
                    mips_reg_t* temp_reg = request_reg(state, NULL);
                    instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", state->stack_adjust-4, temp_reg->index, get_reg(state->reg_block, "s6")->index));
                    if (i == get_reg(state->reg_block, "lo")->index) {
                        instr_add(state, state->current_sub, MIPS_INSTR_MTLO(temp_reg->index));
                    }
                    else if (i == get_reg(state->reg_block, "hi")->index) {
                        instr_add(state, state->current_sub, MIPS_INSTR_MTHI(temp_reg->index));
                    }
                    free_reg(temp_reg);
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
    if (expr->type == EXPRESSION_VAL) {
        expression_mips_load(state, expr);
    }
    else if (expr->type == EXPRESSION_OP) {
        expression_mips_operation(state, expr);
    }
}

static void
expression_output(
    parser_state_t* state,
    expression_t* expr)
{
    if (expr->type != EXPRESSION_TERNARY && expr_get_by_id(state->version, expr->id)->id < -1) {
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
    } else if (expr->type == EXPRESSION_GLOBAL) {
        instr_add(state, state->current_sub, instr_new(state, expr->id, "S", expr->value->value.val.S));
    } else if (expr->type == EXPRESSION_OP) {
        const expr_t* expression = expr_get_by_id(state->version, expr->id);
        int c = 0, lc = list_count(&expr->children);

        if (expression->symbol == RSHIFT && lc == 2) { /* special case handling, as GOOL does not have a native >> operation */
            expression_t* shamt = list_tail(&expr->children);
            list_del_tail(&expr->children);
            list_append_new(&expr->children, EXPR_2(SUBTRACT, expression_val_new(state, 0), expression_copy(shamt)));
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
            if (child_expr->type == EXPRESSION_VAL && child_expr->value->stack != 3 && (!expression->has_double_param || (expression->has_double_param && lc <= 2) || (expression->has_double_param && lc > 2 && c == 1))) {
                list_append_new(param_list, child_expr->value);
            }
            else {
                expression_output(state, child_expr);
                if (child_expr->type == EXPRESSION_VAL && child_expr->value->stack == 3) {
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
    ) {
        /* Partial expression optimization */
        if ((tmp_expr = expr_get_by_symbol(state->version, ADD))->id == expression->id) {
            tmp_expr = expr_get_by_symbol(state->version, RAND);
            if ((child_expr_1->id == tmp_expr->id && child_expr_2->type == EXPRESSION_VAL && ((expression_t*)child_expr_1->children.head->data)->type == EXPRESSION_VAL && ((expression_t*)child_expr_1->children.head->data)->value->value.val.S == 0) ||
            (child_expr_2->id == tmp_expr->id && child_expr_1->type == EXPRESSION_VAL && ((expression_t*)child_expr_2->children.head->data)->type == EXPRESSION_VAL && ((expression_t*)child_expr_2->children.head->data)->value->value.val.S == 0)) {
                expression_t* rand_expr = child_expr_1->id == tmp_expr->id ? child_expr_1 : child_expr_2;
                expression_t* numb_expr = child_expr_1->id == tmp_expr->id ? child_expr_2 : child_expr_1;
                rand_expr = expression_copy(list_tail(&rand_expr->children));
                numb_expr = expression_copy(numb_expr);

                expression->id = tmp_expr->id;
                param_free((child_expr_1->id == tmp_expr->id ? child_expr_2 : child_expr_1)->value);
                expression_free(child_expr_1);
                expression_free(child_expr_2);
                list_free_nodes(&expression->children);

                list_append_new(&expression->children, numb_expr);
                list_append_new(&expression->children, EXPR_2(ADD, expression_copy(numb_expr), rand_expr));
                return;
            }
        }
        else if ((tmp_expr = expr_get_by_symbol(state->version, EQUAL))->id == expression->id) {
            if ((child_expr_1->type == EXPRESSION_VAL && child_expr_1->value->value.val.S == 0) ||
            (child_expr_2->type == EXPRESSION_VAL && child_expr_2->value->value.val.S == 0)) {
                expression_t* zero_expr = (child_expr_1->type == EXPRESSION_VAL && child_expr_1->value->value.val.S == 0) ? child_expr_1 : child_expr_2;

                param_free(zero_expr->value);
                expression_free(zero_expr);
                list_free_nodes(&expression->children);

                tmp_expr = expr_get_by_symbol(state->version, NOT);
                expression->id = tmp_expr->id;
                list_append_new(&expression->children, expression_load_new(state, param_sp_new()));
                list_append_new(&expression->children, zero_expr == child_expr_1 ? child_expr_2 : child_expr_1);
                return;
            }
        }
        return;
    }

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
        thecl_instr_t* last_ins = state->current_sub->last_ins;
        const expr_t* tmp = expr_get_by_symbol(state->version, GOTO);
        if (last_ins != NULL && last_ins->id == tmp->id && ((thecl_param_t*)last_ins->params.tail->data)->value.val.S == 2) {
            thecl_param_t* label_param = list_head(&last_ins->params);
            if (strcmp(label_param->value.val.z, "inline_end") == 0) {
                /* Remove useless goto. */
                list_del_tail(&state->current_sub->instrs);
                --state->current_sub->offset;
                thecl_instr_free(last_ins);
            }
        }
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
        const expr_t* expr = expr_get_by_symbol(state->version, RETURN);
        if (bb || !r) {
            if (!state->mips_mode) {
                if (m) {
                    instr_end_mips(state, state->current_sub);
                }
                instr_add(state, state->current_sub, instr_new(state, expr->id, "SSSSS", 0, 0, 0x25, 0, 2));
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

    if (state->scope_cnt > 1 && !state->scope_stack[state->scope_cnt-1].returned && state->scope_stack[state->scope_cnt-2].mips != state->scope_stack[state->scope_cnt-1].mips) {
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

    if (state->mips_mode) {
        thecl_param_t* param = param_new('S');
        param->stack = 1;
        param->value.val.S = var->stack;
        param->object_link = -1;
        var_assign(state, param, expr);
    }
    else {
        if (expr->type == EXPRESSION_VAL) {
            thecl_param_t* param = expr->value;
            const expr_t* expr_load = expr_get_by_symbol(state->version, param->stack == 3 ? PLOAD : LOAD);
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
    const expr_t* expr = expr_get_by_id(state->version, expr_assign->id);
    thecl_param_t* src_param = expr != NULL && expr->is_unary ? ((expression_t*)list_head(&expr_assign->children))->value : NULL;
    if (param->stack == 1 && src_param && src_param->value.val.S == 0x1F && src_param->stack == 1 && src_param->object_link == 0) {
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
    && param->stack == 1 && param->object_link == 0 && param->value.val.S >= 0 && param->value.val.S <= 0x3F) {
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
        if (expr_assign->type == EXPRESSION_VAL && (param->stack == 1 || (param->stack == 2 && !expr->is_unary))) {
            src_param = expr_assign->value;
        } else {
            expression_output(state, expr_assign);
            src_param = param_sp_new();
        }
        expression_free(expr_assign);
        if (param->stack == 1) {
            expr = expr_get_by_symbol(state->version, src_param->stack == 3 ? PASSIGN : ASSIGN);

            instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", param, src_param));

            if (param->object_link == -1 && param->value.val.S >= 3) {
                state->current_sub->vars[param->value.val.S - 3]->is_written = true;
            }
            else if (param->object_link == -1 && param->value.val.S < 0) {
                state->current_sub->args[-param->value.val.S - 1]->is_written = true;
            }
        } else if (param->stack == 2) { /* WGL */
            expr = expr_get_by_symbol(state->version, GASSIGN);

            instr_add(state, state->current_sub, instr_new(state, expr->id, "Sp", param->value.val.S, src_param));
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
    thecl_param_t* param_clone = malloc(sizeof(thecl_param_t));
    memcpy(param_clone, param, sizeof(thecl_param_t));

    expression_t* expr_load = expression_load_new(state, param_clone);
    expression_t* expr_main = EXPR_2(EXPR, expr_load, expr_assign);
    expression_output(state, expr_main);
    expression_free(expr_main);
    /* No need to free expr_load or expr, since they both got freed as children of expr_main. */

    if (param->stack == 1) {
        if (param->object_link == -1 && param->value.val.S >= 3) {
            state->current_sub->vars[param->value.val.S - 3]->is_written = true;
        }
        else if (param->object_link == -1 && param->value.val.S < 0) {
            state->current_sub->args[-param->value.val.S - 1]->is_written = true;
        }
    }
    if (state->mips_mode) {
        mips_instr_new_store(state, param);
    }
    else {
        if (param->stack == 1) {
            const expr_t* expr = expr_get_by_symbol(state->version, ASSIGN);

            instr_add(state, state->current_sub, instr_new(state, expr->id, "pp", param, param_sp_new()));
        } else if (param->stack == 2) { /* WGL */
            const expr_t* expr = expr_get_by_symbol(state->version, GASSIGN);

            instr_add(state, state->current_sub, instr_new(state, expr->id, "Sp", param->value.val.S, param_sp_new()));
        }
    }
}

static void
mips_instr_new_store(
    parser_state_t* state,
    thecl_param_t* value)
{
    verify_reg_load(state, &state->top_reg, NULL);
    if (value->stack == 1) { /* object field, stack */
        if (value->object_link == 0) { /* object field */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", value->value.val.S * 4 + get_obj_proc_offset(state->version), state->top_reg->index, get_reg(state->reg_block, "s0")->index));
        }
        else if (value->object_link >= 1 && value->object_link <= 7) { /* linked object field */
            mips_reg_t* link_reg = get_usable_reg(state->reg_block);
            if (link_reg) {
                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", value->object_link * 4 + get_obj_proc_offset(state->version), link_reg->index, get_reg(state->reg_block, "s0")->index));
                instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", value->value.val.S * 4 + get_obj_proc_offset(state->version), state->top_reg->index, link_reg->index));
                link_reg->status = MREG_STATUS_USED;
            }
            else {
                yyerror(state, "no available registers for mips mode");
                exit(2);
            }
        }
        else if (value->object_link == -1) { /* stack */
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("sw", value->value.val.S * 4, state->top_reg->index, get_reg(state->reg_block, "s7")->index));
        }
    }
    else if (value->stack == 2) { /* global */
        mips_reg_t* reg = get_usable_reg(state->reg_block);
        if (reg) {
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", 0x58, reg->index, get_reg(state->reg_block, "s8")->index));
            instr_add_delay_slot(state, state->current_sub, MIPS_INSTR_I("lw", (value->value.val.S >> 8) * 4, state->top_reg->index, reg->index));
            reg->status = MREG_STATUS_USED;
        }
        else {
            yyerror(state, "no available registers for mips mode");
            exit(2);
        }
    }
    SetUsedReg(state->top_reg);
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
