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
#ifndef THECL_H_
#define THECL_H_

#include <config.h>
#include <stdbool.h>
#include <stdio.h>
#include "list.h"
#include "value.h"
#include "util.h"
#include "field.h"
#include "gmips.h"

extern const char* gool_ename_charmap;
extern const int gool_null_eid;
extern const char* gool_null_ename;

extern short sine_table[];
int sin_psx(
    int r);

typedef enum {
    THECL_INSTR_INSTR,
    THECL_INSTR_LABEL
} thecl_instr_type;

typedef enum {
    PARAM_LITERAL = 0,
    PARAM_FIELD,
    PARAM_GLOBAL,
    PARAM_POINTER,
    PARAM_COLOR,
    PARAM_SPEC
} gooc_value_type;

typedef struct thecl_param_t {
    int type;
    value_t value;
    gooc_value_type val_type;
    char object_link;
    char is_expression_param; /* Temporary variable for ecsparse.y */
} thecl_param_t;

typedef struct thecl_instr_t {
    thecl_instr_type type;
    char* string;

    /* THECL_INSTR_INSTR: */
    bool mips;
    uint8_t id;
    size_t param_count;
    list_t params;
    mips_ins_t ins;
    uint32_t reg_used;
    uint32_t reg_stalled;

    /* Etc.: */
    unsigned int offset;
} thecl_instr_t;

typedef struct {
    char* name;
    int start;
    int end;
} gooc_array_t;

typedef struct {
    unsigned int version;

    int is_defined;
    bool purge_data;
    bool purge_tested;

    uint32_t eid;
    int id;
    int type;

    list_t anims;

    size_t const_count;
    uint32_t* consts;
    list_t arrays;

    size_t var_count;
    field_t** vars;

    list_t interrupts;
    list_t spawns;

    list_t states;
    list_t subs;

    bool no_warn;

    int ins_offset;
} thecl_t;

typedef struct {
    char* name;
    int stack;
    int scope;
    bool is_unused;
    bool is_written;
} thecl_variable_t;

typedef struct {
    int32_t offset;
    char name[];
} thecl_label_t;

typedef struct {
    int was_written;
    uint32_t data[];
} gool_sub_t;

enum expression_type {
    EXPRESSION_OP,
    EXPRESSION_VAL,
    EXPRESSION_XVAL,
    EXPRESSION_GLOBAL,
    EXPRESSION_COLOR,
    EXPRESSION_TERNARY
};

typedef struct expression_t {
    /* General things. */
    enum expression_type type;
    int id;
    /* For values: The value. */
    thecl_param_t* value;
    /* For operators: The child expressions. */
    list_t children;
} expression_t;

typedef struct {
    char* name;
    expression_t* expr;
} expr_macro_t;

enum thecl_line_type {
    LINE_INVALID = 0,
    LINE_LOAD,
    LINE_INSTRUCTION,
    LINE_CALL,
    LINE_LABEL,
    LINE_ASSIGNMENT,
    LINE_VAR_DECL,
    LINE_VAR_DECL_ASSIGN,
    LINE_GOTO,
    LINE_SCOPE_START,
    LINE_SCOPE_END,
    LINE_BREAK,
    LINE_CONTINUE,
    LINE_RETURN,

    LINE_SAVE_START,
    LINE_SAVE_END
};

typedef struct {
    enum thecl_line_type type;
    union {
        thecl_label_t* label;
        list_t* list;
        const char* name;
        expression_t* expression;
        struct {
            thecl_param_t* address;
            expression_t* expr;
        } ass;
        struct {
            const char* name;
            list_t* expr_list;
        } call;
        struct {
            const char* name;
            expression_t* expr;
        } var;
        struct {
            int type;
            const char* label;
            expression_t* expr;
        } go;
    };
} thecl_line_t;

typedef struct {
    char* name;
    bool forward_declaration;
    bool is_inline;
    bool is_external;

    size_t stack;
    int stack_offset;
    size_t var_count;
    thecl_variable_t** vars;
    int arg_count;
    thecl_variable_t** args;

    list_t instrs;
    list_t labels;
    list_t lines;

    uint16_t offset;
    uint16_t start_offset;

    thecl_instr_t* last_ins;
    thecl_instr_t* secondlast_ins;
    bool mips_dirty;
    int multdiv_offset;

    bool is_trans;
    bool has_once;
    bool has_nofirst;
    bool self_reference;

    bool mod_trans;
    int mod_trans_count;

    bool deleted;
    gool_sub_t* instr_data;
} thecl_sub_t;

typedef enum {
    INTERRUPT_SUB,
    INTERRUPT_STATE
} thecl_interrupt_type;

typedef enum {
    STATE_SUB_CODE,
    STATE_SUB_EVENT,
    STATE_SUB_TRANS
} thecl_state_sub_type;

typedef struct {
    enum thecl_interrupt_type type;
    char* lambda_name;
} thecl_state_sub_t;

typedef struct {
    char* name;

    thecl_state_sub_t* code;
    thecl_state_sub_t* trans;
    thecl_state_sub_t* event;

    thecl_t* exe;
    bool external;

    uint32_t stateflag;
    uint32_t statusc;

    bool trans_args;

    uint16_t index;
} thecl_state_t;

typedef struct {
    char* name;
    char* state_name;
    size_t offset;
} thecl_spawn_t;

typedef struct {
    const field_t* event;
    enum thecl_interrupt_type type;
    char* lambda_name;
} thecl_interrupt_t;

typedef struct {
    char* name;
    size_t size; /* sizeof anim member */
    void* anim; /* pointer to anim data */
} gool_anim_t;

typedef struct {
    list_node_t* slot;
    thecl_instr_t* owner;
    bool optional;
    int slot_id;
} gooc_delay_slot_t;

typedef struct {
    int id;
    bool mips;
    bool returned;
} thecl_scope_t;

typedef struct {
    unsigned int version;
    bool mips_mode;
    int stack_adjust;
    mips_reg_block_t* reg_block;
    mips_reg_t* top_reg;
    list_t delay_slots;
    bool force_mips; /* Fuck this. */
    list_t expressions;
    list_t addresses;
    list_t block_stack;
    thecl_scope_t* scope_stack;
    int scope_cnt;
    int scope_id;
    thecl_sub_t* current_sub;
    thecl_state_t* current_state;
    thecl_interrupt_t* current_interrupt;
    thecl_state_sub_t* current_state_sub;
    gool_anim_t* current_anim;
    gooc_array_t* current_array;
    void* current_tex;
    bool declared_tempfields;
    thecl_t* main_ecl;
    thecl_t* ecl;
    thecl_t** ecl_stack;
    int ecl_cnt;
    int path_cnt;
    char** path_stack;
    thecl_sub_t* (*find_state_sub)(thecl_t* ecl, thecl_t* ecl_ext, thecl_state_sub_t* state_sub, thecl_state_sub_type type);
    thecl_sub_t* (*find_sub)(thecl_t* ecl, const char* name);
    thecl_sub_t* (*find_sub_overload)(thecl_t* ecl, const char* name, int argc);

    uint16_t state_count;
    size_t spawn_count;

    list_t expr_macros;

    int ignore_block;
    int block_bound;
    int scope_bound;
} parser_state_t;

typedef struct {
    thecl_t* (*open)(FILE* stream, unsigned int ver);
    /* Translates the data to a more general format. */
    /* TODO: Return it instead. */
    void (*trans)(thecl_t* ecl);
    void (*dump)(const thecl_t* ecl, FILE* stream);

    parser_state_t* (*parse)(FILE* stream, char* filename, unsigned int ver);
    int (*compile)(const parser_state_t* ecl, FILE* stream);

    void (*create_header)(const thecl_t* ecl, FILE* stream);
} thecl_module_t;

/* Param functions */
thecl_param_t* param_new(
    int type);
thecl_param_t* param_copy(
    thecl_param_t* param);
void param_free(
    thecl_param_t* param);

thecl_param_t* param_sp_new(void);
thecl_param_t* param_sp2_new(void);
thecl_param_t* param_val_new(int val);
thecl_param_t* param_var_new(char* var);
thecl_param_t* param_null_new(void);

/* Miscellaneous version functions */

int get_obj_proc_offset(
    unsigned int version);

bool is_post_c2(
    unsigned int version);

/* Instruction functions */

thecl_instr_t* thecl_instr_new(void);

thecl_instr_t* thecl_instr_label(
    unsigned int offset);

void thecl_instr_free(
    thecl_instr_t* instr);

/* TODO: Move label creation functions here. */

void thecl_variable_free(
    thecl_variable_t* var);

thecl_label_t*
label_find(
    thecl_sub_t* sub,
    const char* name);

int32_t
label_offset(
    thecl_sub_t* sub,
    const char* name);

/* TODO: Subroutine creation and deletion functions. */

thecl_t* thecl_new(void);

/* GOOL functions */

char* gool_to_ename(
    char* ename, int eid);

int gool_to_eid(
    const char* ename);

int gool_pool_get_index(
    thecl_t* ecl,
    uint32_t val);

int gool_pool_force_get_index(
    thecl_t* ecl,
    uint32_t val);

int gool_pool_force_make_index(
    thecl_t* ecl,
    uint32_t val);

/* Line system functions */

thecl_line_t* line_make(
    enum thecl_line_type type);

thecl_line_t* line_make_var_decl(
    const char* name);

thecl_line_t* line_make_var_decl_assign(
    const char* name,
    expression_t* expression);

thecl_line_t* line_make_assignment(
    thecl_param_t* address,
    expression_t* expression);

thecl_line_t* line_make_call(
    const char* name,
    list_t* expr_list);

thecl_line_t* line_make_label(
    const char* label_name);

thecl_line_t* line_make_goto(
    int type,
    const char* label_name,
    expression_t* expression);

thecl_line_t* line_make_save_start(
    list_t* address_list);

thecl_line_t* line_make_save_end(void);

/* Globals */

char* convert_extended_string(
    unsigned int version,
    char* old_string);

extern parser_state_t* g_parser_state;
extern int g_rate;
extern char* g_region;
extern char g_lev;
extern int g_reg_block_depth;
extern int* g_reg_blocks;
extern char* g_module_fmt;

extern bool g_warn_deprecate_getcolor;
extern bool g_warn_deprecate_setcolor;
extern bool g_warn_deprecate_setfield;
extern bool g_warn_deprecate_getfield;

/* Expression functions */

void expression_free(expression_t* expr);
bool expression_is_number(expression_t* expr);

/* Returns macro of the given name, or NULL if the macro doesn't exist */
expr_macro_t* macro_get(parser_state_t* state, const char* name);

/* TODO: Deletion and creation functions for parser state. */

extern FILE* yyin;
extern int yyparse(parser_state_t*);

extern bool g_was_error;

typedef struct {
PACK_BEGIN
    uint32_t magic;
    uint32_t id;
    uint32_t type;
    uint32_t count;
    uint32_t offsets[];
PACK_END
} PACK_ATTRIBUTE entry_header_t;

typedef struct {
PACK_BEGIN
    uint32_t id;
    uint32_t type;
    uint32_t exe_type;
    uint32_t stack_start;
    uint32_t interrupt_count;
    uint32_t stack_depth;
PACK_END
} PACK_ATTRIBUTE gool_header_t;

typedef struct {
PACK_BEGIN
    uint32_t stateflag;
    uint32_t statusc;
    int16_t exe_off;
    uint16_t epc;
    uint16_t tpc;
    uint16_t cpc;
PACK_END
} PACK_ATTRIBUTE state_t;

#define strncmpa(Str1, Str2)        strncmp(Str1, Str2, strlen(Str2))

#endif
