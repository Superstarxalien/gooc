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

extern const char* gool_ename_charmap;
extern const int gool_null_eid;
extern const char* gool_null_ename;

extern int sine_table[];
int sin_psx(
    int r);

typedef enum {
    THECL_INSTR_INSTR,
    THECL_INSTR_LABEL
} thecl_instr_type;

typedef struct thecl_param_t {
    int type;
    value_t value;
    int stack; /* Determines if value is a literal or a variable */
    char object_link;
    char is_expression_param; /* Temporary variable for ecsparse.y */
} thecl_param_t;

thecl_param_t* param_new(
    int type);
thecl_param_t* param_copy(
    thecl_param_t* param);
void param_free(
    thecl_param_t* param);
thecl_param_t* param_val_new(
    int val);

typedef struct thecl_instr_t {
    thecl_instr_type type;
    char* string;

    /* THECL_INSTR_INSTR: */
    uint8_t id;
    size_t param_count;
    list_t params;
    int op_type;

    /* Etc.: */
    unsigned int offset;

    /* Used by ecsparse.y, not present anywhere in the compiled ECL files. */
    unsigned int flags;
} thecl_instr_t;

thecl_instr_t* thecl_instr_new(
    void);

thecl_instr_t* thecl_instr_label(
    unsigned int offset);

void thecl_instr_free(
    thecl_instr_t* instr);

typedef struct {
    int32_t offset;
    char name[];
} thecl_label_t;

/* TODO: Move label creation functions here. */

typedef struct {
    char* name;
    int stack;
    int scope;
    bool is_unused;
    bool is_written;
} thecl_variable_t;

void thecl_variable_free(
    thecl_variable_t* var);

typedef struct {
    int was_written;
    uint32_t data[];
} gool_sub_t;

typedef struct {
    char* name;
    bool forward_declaration;
    bool is_inline;

    size_t stack;
    size_t var_count;
    thecl_variable_t** vars;
    int arg_count;
    thecl_variable_t** args;

    list_t instrs;
    list_t labels;

    uint16_t offset;
    uint16_t start_offset;

    bool is_trans;
    bool has_once;

    gool_sub_t* instr_data;
} thecl_sub_t;

typedef struct {
    char* name;
    size_t offset;
} thecl_globalvar_t;

typedef struct {
    char* name;

    thecl_sub_t* code;
    thecl_sub_t* trans;
    thecl_sub_t* event;

    uint32_t exe_eid;

    uint32_t stateflag;
    uint32_t statusc;

    uint16_t index;
} thecl_state_t;

typedef struct {
    char* name;
    char* state_name;
    size_t offset;
} thecl_spawn_t;

typedef enum {
    INTERRUPT_SUB,
    INTERRUPT_STATE
} thecl_interrupt_type;

typedef struct {
    const field_t* event;
    enum thecl_interrupt_type type;
    char* lambda_name;
} thecl_interrupt_t;

thecl_label_t*
label_find(
    thecl_sub_t* sub,
    const char* name);

int32_t
label_offset(
    thecl_sub_t* sub,
    const char* name);

/* TODO: Subroutine creation and deletion functions. */

typedef struct {
    char name[256];
    list_t instrs;
} thecl_timeline_t;

typedef struct {
    unsigned int version;
    /* TODO: Make local data. */
    int is_defined;

    uint32_t eid;
    int id;
    int type;

    list_t anims;

    size_t const_count;
    uint32_t* consts;

    size_t var_count;
    thecl_globalvar_t** vars;

    list_t interrupts;
    list_t spawns;

    list_t states;
    list_t subs;

    bool no_warn;

    int ins_offset;
} thecl_t;

typedef struct {
    char* name;
    size_t size; /* sizeof anim member */
    void* anim; /* pointer to anim data */
} gool_anim_t;

thecl_t* thecl_new(
    void);

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

typedef struct {
    int instr_flags; /* Special flags that are copied to instr->flags, used by ecsparse.y */
    unsigned int version;
    bool has_mips;
    list_t expressions;
    list_t block_stack;
    int* scope_stack;
    int scope_cnt;
    int scope_id;
    thecl_sub_t* current_sub;
    thecl_state_t* current_state;
    thecl_interrupt_t* current_interrupt;
    gool_anim_t* current_anim;
    thecl_t* main_ecl;
    thecl_t* ecl;
    thecl_t** ecl_stack;
    int ecl_cnt;
    int path_cnt;
    char** path_stack;
    const char* (*instr_format)(unsigned int version, unsigned int id);

    uint16_t state_count;
    size_t spawn_count;

    list_t expr_macros;

    int ignore_block;
    int block_bound;
} parser_state_t;

extern parser_state_t* g_parser_state;
extern int g_rate;
extern char* g_region;
extern int g_reg_block_depth;
extern int* g_reg_blocks;
extern char* g_module_fmt;

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

enum expression_type {
    EXPRESSION_OP,
    EXPRESSION_VAL,
    EXPRESSION_GVAR,
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
    char *name;
    expression_t* expr;
} expr_macro_t;

void expression_free(expression_t* expr);

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

#endif
