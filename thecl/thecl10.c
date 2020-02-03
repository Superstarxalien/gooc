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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "expr.h"
#include "path.h"
#include "file.h"
#include "program.h"
#include "thecl.h"
#include "util.h"
#include "value.h"

#define GOOL_JAL_OP           (0x86)
#define GOOL_RET_OP           (0x82)

typedef struct {
PACK_BEGIN
    uint32_t magic;
    uint32_t id;
    uint32_t type;
    uint32_t count;
    uint32_t offsets[];
PACK_END
} PACK_ATTRIBUTE c1_entry_header_t;

typedef struct {
PACK_BEGIN
    uint32_t id;
    uint32_t type;
    uint32_t exe_type;
    uint32_t stack_start;
    uint32_t interrupt_count;
    uint32_t stack_depth;
PACK_END
} PACK_ATTRIBUTE c1_gool_header_t;

typedef struct {
PACK_BEGIN
    uint32_t stateflag;
    uint32_t statusc;
    int16_t exe_off;
    uint16_t epc;
    uint16_t tpc;
    uint16_t cpc;
PACK_END
} PACK_ATTRIBUTE c1_state_t;

static const id_format_pair_t th10_fmts[] = {
    { 0, "RR" },
    { 1, "RR" },
    { 2, "RR" },
    { 3, "RR" },
    { 4, "RR" },
    { 5, "RR" },
    { 6, "RR" },
    { 7, "RR" },
    { 8, "RR" },
    { 9, "RR" },
    { 10, "RR" },
    { 11, "RR" },
    { 12, "RR" },
    { 13, "RR" },
    { 14, "RR" },
    { 15, "RR" },

    { 17, "RR" },

    { 22, "RR" },

    { 34, "RR" },

    { 39, "RR" },

    { 0x82, "I10I4I6I2I2" },
    { 0x83, "I7I9I6I2" },
    { 0x84, "RN4I6I2" },

    { 0x86, "I14N6I4" },
    { 0, NULL }
};

/* NEWHU: */

static const char*
th10_find_format(
    unsigned int version,
    unsigned int id)
{
    const char* ret = NULL;

    switch (version) {
    /* Intentional fallthroughs, obviously */
    /* NEWHU: */
    case 1:
        if (!ret) ret = find_format(th10_fmts, id);
        break;
    default:
        fprintf(stderr, "%s: unsupported version: %u\n", argv0, version);
        return NULL;
    }

    if (!ret)
        fprintf(stderr, "%s: id %d was not found in the format table\n", argv0, id);

    return ret;
}

static const thecl_sub_t*
th10_find_sub(
    thecl_t* ecl,
    const char* name)
{
    const thecl_sub_t* sub;
    list_for_each(&ecl->subs, sub) {
        if (!strcmp(sub->name, name))
            return sub;
    }
    return NULL;
}

static const thecl_state_t*
c1_find_state(
    thecl_t* ecl,
    char* name)
{
    const thecl_state_t* state;
    list_for_each(&ecl->states, state) {
        if (!strcmp(state->name, name))
            return state;
    }
    return NULL;
}

static thecl_t*
c1_parse(
    FILE* in,
    char* filename,
    unsigned int version)
{
    parser_state_t state;

    state.version = version;
    list_init(&state.expressions);
    list_init(&state.block_stack);
    list_init(&state.expr_macros);
    state.scope_stack = NULL;
    state.scope_cnt = 0;
    state.scope_id = 0;
    state.current_sub = NULL;
    state.ecl = thecl_new();
    state.ecl->version = version;
    state.instr_format = th10_find_format;
    state.ins_offset = 0;

    state.path_cnt = 0;
    state.path_stack = NULL;
    path_add(&state, filename);

    state.has_mips = false;
    state.state_count = 0;
    state.ins_ret = GOOL_RET_OP;
    state.ins_jal = GOOL_JAL_OP;

    g_parser_state = &state;
    yyin = in;

    if (yyparse(&state) != 0)
        return 0;

    free(state.scope_stack);
    g_parser_state = NULL;

    expr_macro_t* macro;
    list_for_each(&state.expr_macros, macro) {
        free(macro->name);
        expression_free(macro->expr);
        free(macro);
    }
    list_free_nodes(&state.expr_macros);

    return state.ecl;
}

static void
c1_set_opcode(
    int* ins,
    uint8_t opcode)
{
    *ins = *ins & 0x00FFFFFF;
    *ins |= opcode << 24;
}

static int
c1_instr_serialize(
    thecl_t* ecl,
    thecl_sub_t* sub,
    thecl_instr_t* instr)
{
    int ret = 0;
    thecl_param_t* param;

    const char* format = th10_find_format(ecl->version, instr->id);

    c1_set_opcode(&ret, instr->id);

    if (format == NULL) {
        fprintf(stderr, "%s:c1_instr_serialize: in sub %s: instruction with id %d is not known to exist in version %d\n", argv0, sub->name, instr->id, ecl->version);
        return ret;
    }

    if (instr->id == GOOL_JAL_OP) {
        /* Validate sub call parameters. */
        list_node_t* node = instr->params.head;
        thecl_param_t* sub_name_param = node->data;
        char* sub_name = sub_name_param->value.val.z;
        thecl_param_t* sub_argc_param = node->next->data;
        const thecl_sub_t* called_sub = th10_find_sub(ecl, sub_name);
        if (!called_sub) {
            fprintf(stderr, "%s:c1_instr_serialize: in sub %s: unknown sub call \"%s\"\n",
                    argv0, sub->name, sub_name);
        } else {
            if (sub_argc_param->value.val.S > called_sub->arg_count) {
                fprintf(stderr, "%s:c1_instr_serialize: in sub %s: too many parameters when calling sub \"%s\" (expected %d)\n",
                    argv0, sub->name, sub_name, called_sub->arg_count);
            }
            else if (sub_argc_param->value.val.S < called_sub->arg_count)
                fprintf(stderr, "%s:c1_instr_serialize: in sub %s: not enough parameters when calling sub %s (expected %d)\n",
                        argv0, sub->name, sub_name, called_sub->arg_count);
            free(sub_name_param->value.val.z);
            sub_name_param->type = 'S';
            sub_name_param->value.type = 'S';
            sub_name_param->value.val.S = called_sub->start_offset;
        }
    }

    int total_bits = 0;
    int i = 0;
    char op;
    list_for_each(&instr->params, param) {
        int p = param->value.val.S;
        if (param->type == 'o') {
            /* This calculates the relative offset from the current instruction. */
            p = label_offset(sub, param->value.val.z) - (instr->offset + 1);
        }
    RECHECK_PARAM:;
        while ((op = format[i++]) == ' ');
        int bits = total_bits;
        int val = 0;
        if (op == 'R') {
            if (param->stack) {
                if (param->object_link == 0) {
                    /* reg ref */
                    val = 0xE00;
                    val |= p
                        & 0x1FF;
                }
                else if (param->object_link == -1) {
                    /* stack ref */
                    val = 0xB00;
                    val |= p
                        & 0x7F;
                }
                else if (param->object_link > 0 && param->object_link <= 7) {
                    /* ireg ref */
                    val = 0xC00;
                    val |= p
                        & 0x3F;
                    val |= param->object_link
                        << 6;
                }
                else if (param->object_link == -2) {
                    /* sp-double ref OR null ref */
                    val = 0xBE0;
                    val |= param->value.val.S
                        << 4;
                }
            }
            else {
                if (!(p % 0x100) && p >= -256 * 0x100 && p <= 255 * 0x100) {
                    /* frac ref */
                    val = 0x800;
                    val |= (p / 0x100)
                        & 0x1FF;
                }
                else if (!(p % 0x10) && p >= -128 * 0x10 && p <= 127 * 0x10) {
                    /* int ref */
                    val = 0xA00;
                    val |= (p / 0x10) & 0xFF;
                }
                else {
                    /* pool ref */
                    val = 0x000;
                    val |= gool_pool_force_get_index(ecl, p);
                }
            }
            total_bits += 12;
        }
        else if (op == 'I') {
            const char* next_format;
            uint32_t b = strtol(format + i, &next_format, 10);
            i = next_format - format;

            val = p;

            val &= 0xFFFFFFFFU >> (32 - b);
            total_bits += b;
        }
        else if (op == 'N') {
            const char* next_format;
            uint32_t b = strtol(format + i, &next_format, 10);
            i = next_format - format;
            total_bits += b;
            goto RECHECK_PARAM;
        }
        ret |= val << bits;
    }
    while (i < strlen(format)) {
        while ((op = format[i++]) == ' ');
        int bits = total_bits;
        int val = 0;
        if (op == 'R') {
            /* null ref */
            val = 0xBE0;
            total_bits += 12;
        }
        else if (op == 'I') {
            const char* next_format;
            uint32_t b = strtol(format + i, &next_format, 10);
            i = next_format - format;
            total_bits += b;
        }
        else if (op == 'N') {
            const char* next_format;
            uint32_t b = strtol(format + i, &next_format, 10);
            i = next_format - format;
            total_bits += b;
        }
        ret |= val << bits;
    }

    if (total_bits > 24) {
        fprintf(stderr, "%s:c1_instr_serialize: in sub %s: gool instruction %d format overflow\n", argv0, sub->name, instr->id);
    }

    return ret;
}

static int
c1_compile(
    const thecl_t* ecl,
    FILE* out)
{
    c1_entry_header_t entry_header = { 0x100FFFFU, ecl->eid, 11U, 6U, { 0, 0, 0, 0, 0, 0, 0 } };
    c1_gool_header_t header = { ecl->id, ecl->type << 8, 1, ecl->var_count + 0x40, 0, 8 };
    thecl_sub_t* sub;
    thecl_state_t* state;
    gool_anim_t* anim;

    /* write GOOL EIDs to const pool before anything else */
    list_for_each(&ecl->states, state) {
        gool_pool_force_get_index(ecl, state->exe_eid);
    }

    if (!file_seekable(out)) {
        fprintf(stderr, "%s: output is not seekable\n", argv0);
        return 0;
    }

    if (!file_write(out, &entry_header, sizeof(c1_entry_header_t) + 7 * sizeof(uint32_t)))
        return 0;

    entry_header.offsets[0] = file_tell(out);

    if (!file_write(out, &header, sizeof(c1_gool_header_t)))
        return 0;

    entry_header.offsets[1] = file_tell(out);

    list_for_each(&ecl->subs, sub) {
        if (sub->forward_declaration || sub->is_inline)
            continue;

        thecl_instr_t* instr;
        sub->offset = file_tell(out);

        list_for_each(&sub->instrs, instr) {
            int data = c1_instr_serialize(ecl, sub, instr);
            if (!file_write(out, &data, sizeof(data)))
                return 0;
        }
    }

    entry_header.offsets[2] = file_tell(out);

    if (!file_write(out, ecl->consts, sizeof(int) * ecl->const_count))
        return 0;

    entry_header.offsets[3] = file_tell(out);

    /* write interrupts here */

    thecl_spawn_t* spawn;
    list_for_each(&ecl->spawns, spawn) {
        state = c1_find_state(ecl, spawn->state_name);
        uint16_t state_id = 255;
        if (state == NULL) {
            if (!file_write(out, &state_id, sizeof(uint16_t)))
                return 0;
        }
        else {
            if (!file_write(out, &state->index, sizeof(uint16_t)))
                return 0;
        }
    }
    size_t pos = file_tell(out) % 4;
    if (pos != 0) {
        uint32_t a = 0;
        if (!file_write(out, &a, 4 - pos))
            return 0;
    }

    entry_header.offsets[4] = file_tell(out);

    list_for_each(&ecl->states, state) {
        c1_state_t gstate = { state->stateflag, state->statusc, gool_pool_force_get_index(ecl, state->exe_eid),
            0x3FFFU,
            state->trans == NULL ? 0x3FFFU : state->trans->start_offset,
            state->code == NULL ? 0x3FFFU : state->code->start_offset };

        if (!file_write(out, &gstate, sizeof(gstate)))
            return 0;
    }

    entry_header.offsets[5] = file_tell(out);

    list_for_each(&ecl->anims, anim) {
        if (!file_write(out, anim->anim, anim->size))
            return 0;
        pos = file_tell(out) % 4;
        if (pos != 0) {
            uint32_t a = 0;
            if (!file_write(out, &a, 4 - pos))
                return 0;
        }
    }

    entry_header.offsets[6] = file_tell(out);

    if (!file_seek(out, 0))
        return 0;
    if (!file_write(out, &entry_header, sizeof(c1_entry_header_t) + 7 * sizeof(uint32_t)))
        return 0;

    return 1;
}

const thecl_module_t c1_gool = {
    .open = NULL,
    .trans = NULL,
    .dump = NULL,
    .parse = c1_parse,
    .compile = c1_compile,
    .create_header = NULL
};
