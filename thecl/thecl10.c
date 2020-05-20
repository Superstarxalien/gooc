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
    { 16, "RR" },
    { 17, "RR" },
    { 18, "RR" },
    { 19, "RR" },
    { 20, "RR" },
    { 21, "RR" },
    { 22, "RR" },
    { 23, "RR" },
    { 24, "I14I6" },
    { 25, "RR" },
    { 26, "I12I2I2I4I1" },
    { 27, "RR" },
    { 28, "RI3I5I4" },
    { 29, "RR" },
    { 30, "RR" },
    { 31, "RR" },
    { 32, "RR" },
    { 33, "RR" },
    { 34, "RR" },
    { 35, "N12I3I9" },
    { 36, "RI3I9" },
    { 37, "RR" },
    { 38, "RR" },
    { 39, "RR" },
    { 0x80, "RR" },
    { 0x81, "RR" },
    { 0x82, "I10I4I6I2I2" },
    { 0x83, "I7I9I6I2" },
    { 0x84, "RN4I6I2" },
    { 0x85, "RI3I3I3I3" },
    { 0x86, "I14I6I4" },
    { 0x87, "RI6I3I3" },
    { 0x88, "I10I4I6I2I2" },
    { 0x89, "I10I4I6I2I2" },
    { 0x8A, "I6I6I8I4" },
    { 0x8B, "RR" },
    { 0x8C, "RR" },
    { 0x8D, "RI6I2I4" },
    { 0x8E, "RI3I3I3I3" },
    { 0x8F, "RI6I3I3" },
    { 0x90, "RI6I3I3" },
    { 0x91, "I6I6I8I4" },
    { 0, NULL }
};

static const id_format_pair_t c2_fmts[] = {
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
    { 16, "RR" },
    { 17, "RR" },
    { 18, "RR" },
    { 19, "RR" },
    { 20, "RR" },
    { 21, "RR" },
    { 22, "RR" },
    { 23, "RR" },
    { 24, "I14I1N3I6" },
    { 25, "RR" },
    { 26, "I12I2I2I4I1" },
    { 27, "RR" },
    { 28, "RI3I5I4" },
    { 29, "RR" },
    { 30, "RR" },
    { 31, "RR" },
    { 32, "RR" },
    { 33, "RR" },
    { 34, "RR" },
    { 35, "N12I3I9" },
    { 36, "RI3I9" },
    { 37, "RR" },
    { 38, "RR" },
    { 39, "RR" },
    { 40, "RR" },
    { 41, "RR" },
    { 42, "RR" },
    { 43, "RR" },
    { 44, "RR" },
    { 45, "RR" },
    { 46, "RR" },
    { 47, "RR" },
    { 48, "RR" },
    { 49, "I10I4I6I2I2" },
    { 50, "I10I4I6I2I2" },
    { 51, "I10I4I6I2I2" },
    { 52, "I10I4I6I2I2" },
    { 53, "I10I4I6I2I2" },
    { 54, "I10I4I6I2I2" },
    { 55, "I10I4I6I2I2" },
    { 56, "I7I9I6I2" },
    { 57, "RN4I6I2" },
    { 58, "RI3I3I3I3" },
    { 59, "I14I1N5I4" },
    { 60, "RI6I3I3" },
    { 61, "I10I4I6I2I2" },
    { 62, "I10I4I6I2I2" },
    { 63, "I6I6I8I4" },
    { 64, "RR" },
    { 65, "RR" },
    { 66, "RI6I2I4" },
    { 67, "RI3I3I3I3" },
    { 68, "RI6I3I3" },
    { 69, "RI6I3I3" },
    { 70, "I6I6I8I4" },
    { 71, "RR" },
    { 72, "RR" },
    { 73, "RR" },
    { 74, "RR" },
    { 75, "RR" },
    { 76, "RR" },
    { 77, "RR" },
    { 78, "RR" },
    { 0, NULL }
};

static const id_format_pair_t c3_fmts[] = {
    { 79, "RR" },
    { 80, "RR" },
    { 81, "RR" },
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
    case 3:
        if (!ret) ret = find_format(c3_fmts, id);
    case 2:
        if (!ret) ret = find_format(c2_fmts, id);
        break;
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

static const thecl_sub_t*
th10_find_sub_overload(
    thecl_t* ecl,
    const char* name,
    int argc)
{
    const thecl_sub_t* sub;
    list_for_each(&ecl->subs, sub) {
        if (!strcmp(sub->name, name) && sub->arg_count == argc)
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

static thecl_sub_t*
c1_find_state_sub(
    thecl_t* ecl,
    thecl_t* ecl_ext,
    thecl_state_sub_t* state_sub,
    thecl_state_sub_type type)
{
    if (state_sub->type == INTERRUPT_STATE) {
        thecl_state_t* state = c1_find_state(ecl, state_sub->lambda_name);
        if (state == NULL) {
            return NULL;
        }
        else {
            switch (type) {
            case STATE_SUB_CODE: return state->code ? c1_find_state_sub(ecl, ecl_ext, state->code, STATE_SUB_CODE) : NULL;
            case STATE_SUB_EVENT: return state->event ? c1_find_state_sub(ecl, ecl_ext, state->event, STATE_SUB_EVENT) : NULL;
            case STATE_SUB_TRANS: return state->trans ? c1_find_state_sub(ecl, ecl_ext, state->trans, STATE_SUB_TRANS) : NULL;
            }
        }
    }
    else if (state_sub->type == INTERRUPT_SUB) {
        thecl_sub_t* sub = th10_find_sub(ecl, state_sub->lambda_name);
        if (!sub && ecl_ext) {
            sub = th10_find_sub(ecl_ext, state_sub->lambda_name);
        }
        return sub;
    }
    return NULL;
}

static parser_state_t*
c1_parse(
    FILE* in,
    char* filename,
    unsigned int version)
{
    parser_state_t *state = malloc(sizeof(parser_state_t));

    state->version = version;
    list_init(&state->delay_slots);
    list_init(&state->expressions);
    list_init(&state->addresses);
    list_init(&state->block_stack);
    list_init(&state->expr_macros);
    state->scope_stack = NULL;
    state->scope_cnt = 0;
    state->scope_id = 0;
    state->current_sub = NULL;
    state->ecl = thecl_new();
    state->ecl->version = version;
    state->main_ecl = state->ecl;
    state->ecl_stack = malloc(0);
    state->ecl_cnt = 0;
    state->find_state_sub = c1_find_state_sub;

    state->path_cnt = 0;
    state->path_stack = NULL;
    path_add(state, filename);

    state->mips_mode = false;
    state->stack_adjust = 0;
    state->reg_block = mips_reg_block_new();
    state->declared_tempfields = false;
    state->state_count = 0;
    state->spawn_count = 0;

    state->ignore_block = 0;

    g_parser_state = state;
    yyin = in;

    g_was_error = yyparse(state) != 0;

    free(state->scope_stack);
    g_parser_state = NULL;

    expr_macro_t* macro;
    list_for_each(&state->expr_macros, macro) {
        free(macro->name);
        expression_free(macro->expr);
        free(macro);
    }
    list_free_nodes(&state->expr_macros);
    free(state->reg_block);

    return state;
}

static int
c1_ins_init(
    uint8_t opcode)
{
    return opcode << 24;
}

static int
c1_make_ref_reg(int reg)
{
    return 0xE00 | (reg & 0x1FF);
}

static int
c1_make_ref_stack(int offset)
{
    return 0xB00 | (offset & 0x7F);
}

static int
c1_make_ref_ireg(int link, int reg)
{
    return 0xC00 | ((link & 0x7) << 6) | (reg & 0x3F);
}

static int
c1_make_ref_null(void)
{
    return 0xBE0;
}

static int
c1_make_ref_sp2(void)
{
    return 0xBF0;
}

static int
c1_make_ref_special(int s)
{
    return s == 1 ? c1_make_ref_sp2() : c1_make_ref_null();
}

static int
c1_make_ref_int(int val)
{
    return 0x800 | (val & 0x1FF);
}

static int
c1_make_ref_frac(int val)
{
    return 0xA00 | (val & 0xFF);
}

static int
c1_make_ref_pool(int offset)
{
    return 0x000 | (offset & 0x3FF);
}

static int
c1_make_ref_extpool(int offset)
{
    return 0x400 | (offset & 0x3FF);
}

static int
c1_instr_serialize(
    thecl_t* ecl,
    thecl_t* ecl_ext,
    thecl_sub_t* sub,
    thecl_instr_t* instr,
    bool ignore_error)
{
    const thecl_param_t* param;

    const char* format = th10_find_format(ecl->version, instr->id);

    int ret = c1_ins_init(instr->id);

    if (format == NULL) {
        fprintf(stderr, "%s:c1_instr_serialize: in sub %s: instruction with id %d is not known to exist in version %d\n", argv0, sub->name, instr->id, ecl->version);
        return ret;
    }

    bool was_error = false;

    if (instr->id == 0x86) {
        /* Validate sub call parameters. */
        list_node_t* node = instr->params.head;
        const thecl_param_t* sub_name_param = node->data;
        char* sub_name = sub_name_param->value.val.z;
        const thecl_sub_t* called_sub = th10_find_sub(ecl, sub_name);
        if (!called_sub && ecl_ext) {
            called_sub = th10_find_sub(ecl_ext, sub_name);
        }
        if (!called_sub && !ignore_error) {
            fprintf(stderr, "%s:c1_instr_serialize: in sub %s: unknown sub call \"%s\"\n",
                    argv0, sub->name, sub_name);
            was_error = true;
        } else {
            const thecl_param_t* sub_argc_param = node->next->next->data;
            called_sub = th10_find_sub_overload(ecl, sub_name, sub_argc_param->value.val.S);
            if (!called_sub && ecl_ext) {
                called_sub = th10_find_sub(ecl_ext, sub_name);
            }
            if (!called_sub && !ignore_error) {
                fprintf(stderr, "%s:c1_instr_serialize: in sub %s: no suitable parameter count for sub \"%s\"\n",
                    argv0, sub->name, sub_name);
                was_error = true;
            }
            else if (called_sub && called_sub->is_external && !ignore_error) {
                fprintf(stderr, "%s:c1_instr_serialize: in sub %s: cannot call subs in external gool module: \"%s\"\n",
                    argv0, sub->name, sub_name);
                was_error = true;
            }
        }
    }

    int total_bits = 0;
    int i = 0;
    char op;
    list_for_each(&instr->params, param) {
        if (total_bits >= 24 && !ignore_error) {
            fprintf(stderr, "%s:c1_instr_serialize: in sub %s: gool instruction %d parameter overflow\n", argv0, sub->name, instr->id);
            break;
        }
        int p;
        if (param->type == 'o' || param->type == 'z') {
            /* This calculates the relative offset from the current instruction. */
            thecl_label_t* label = param->type == 'z' ? NULL :label_find(sub, param->value.val.z);
            if (label) {
                p = label->offset - (instr->offset + 1);
            }
            else {
                const thecl_state_t* called_state;
                int o = 0;
                list_for_each(&ecl->states, called_state) {
                    if (!strcmp(called_state->name, param->value.val.z))
                        break;
                    ++o;
                    called_state = NULL;
                }
                if (!called_state) {
                    /* Validate sub call parameters. */
                    char* sub_name = param->value.val.z;
                    const thecl_sub_t* called_sub = th10_find_sub(ecl, sub_name);
                    if (!called_sub && ecl_ext) {
                        called_sub = th10_find_sub(ecl_ext, sub_name);
                    }
                    if (called_sub) {
                        if (instr->id == 59) {
                            const thecl_param_t* sub_argc_param = node->next->next->data;
                            called_sub = th10_find_sub_overload(ecl, sub_name, sub_argc_param->value.val.S);
                            if (!called_sub && ecl_ext) {
                                called_sub = th10_find_sub(ecl_ext, sub_name);
                            }
                        }
                    }
                    if (!called_sub) {
                        if (!was_error && !ignore_error) {
                            fprintf(stderr, "%s:c1_instr_serialize: in sub %s: sub/state/label not found: %s\n", argv0, sub->name, param->value.val.z);
                        }
                        p = o;
                    }
                    else {
                        p = called_sub->start_offset;
                    }
                }
                else
                    p = o;
            }
        }
        else
            p = param->value.val.S;
    RECHECK_PARAM:;
        while ((op = format[i++]) == ' ');
        int bits = total_bits;
        int val = 0;
        if (op == 'R') {
            if (param->stack) {
                if (param->object_link == 0) {
                    val = c1_make_ref_reg(p); /* reg ref */
                }
                else if (param->object_link == -1) {
                    val = c1_make_ref_stack(p); /* stack ref */
                }
                else if (param->object_link > 0 && param->object_link <= 7) {
                    val = c1_make_ref_ireg(param->object_link, p); /* ireg ref */
                }
                else if (param->object_link == -2) {
                    val = c1_make_ref_special(param->value.val.S); /* sp-double ref OR null ref */
                }
                else if (param->object_link == -3) {
                    val = c1_make_ref_pool(gool_pool_force_make_index(ecl, p)); /* pool ref */
                }
            }
            else {
                if (!(p % 0x100) && p >= -256 * 0x100 && p <= 255 * 0x100) {
                    val = c1_make_ref_int(p / 0x100); /* int ref */
                }
                else if (!(p % 0x10) && p >= -128 * 0x10 && p <= 127 * 0x10) {
                    val = c1_make_ref_frac(p / 0x10); /* frac ref */
                }
                else {
                    if (ecl_ext && ecl_ext->purge_data) {
                        val = c1_make_ref_null(); /* null ref */
                    }
                    else {
                        val = c1_make_ref_pool(gool_pool_force_get_index(ecl, p)); /* pool ref */
                    }
                    /* DOES NOT WORK IN-GAME! */
                    //if (!ecl_ext || (ecl_ext && gool_pool_get_index(ecl, p) != -1)) {
                    //    val = c1_make_ref_pool(gool_pool_force_get_index(ecl, p)); /* pool ref */
                    //}
                    //else {
                    //    val = c1_make_ref_extpool(gool_pool_force_get_index(ecl_ext, p)); /* pool ref */
                    //}
                }
            }
            total_bits += 12;
        }
        else if (op == 'I') {
            const char* next_format;
            uint32_t b = strtol(format + i, &next_format, 10);
            i = next_format - format;

            val = p;

            int s = 32 - b;
            int sm = s-1;
            val &= 0xFFFFFFFFU >> s;
            if (p != ((val << s) >> s) && p != ((val << sm) >> sm) && !ignore_error) {
                fprintf(stderr, "%s:c1_instr_serialize: in sub %s: parameter out of bounds for instruction %d (%u bits)\n", argv0, sub->name, instr->id, b);
            }
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
            val = c1_make_ref_null(); /* null ref */
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

    if (total_bits > 24 && !ignore_error) {
        fprintf(stderr, "%s:c1_instr_serialize: in sub %s: gool instruction %d format overflow\n", argv0, sub->name, instr->id);
    }

    return ret;
}

static int
c1_write_gool(
    thecl_t* ecl,
    entry_header_t* entry_header,
    gool_header_t* header,
    FILE* out)
{
    thecl_sub_t* sub;
    thecl_state_t* state;
    gool_anim_t* anim;

    if (!file_write(out, entry_header, sizeof(entry_header_t) + (entry_header->count + 1) * sizeof(uint32_t))) return 0;
    entry_header->offsets[0] = file_tell(out);
    if (!file_write(out, header, sizeof(gool_header_t))) return 0;
    entry_header->offsets[1] = file_tell(out);

    list_for_each(&ecl->subs, sub) {
        if (sub->forward_declaration || sub->is_inline)
            continue;

        if (sub->instr_data->was_written || sub->deleted)
            continue;
        sub->instr_data->was_written = 1;

        if (!file_write(out, &sub->instr_data->data, sizeof(uint32_t) * sub->offset)) return 0;
    }

    entry_header->offsets[2] = file_tell(out);
    if (!file_write(out, ecl->consts, sizeof(int) * ecl->const_count)) return 0;
    entry_header->offsets[3] = file_tell(out);

    size_t pos = file_tell(out);
    thecl_interrupt_t* interrupt;
    list_for_each(&ecl->interrupts, interrupt) {
        while (header->interrupt_count <= interrupt->event->offset) {
            ++header->interrupt_count;
            uint16_t state_id = 255;
            if (!file_write(out, &state_id, sizeof(uint16_t))) return 0;
        }
        if (!file_seek(out, pos + interrupt->event->offset * 2)) return 0;
        uint16_t interrupt_val = 255;
        if (interrupt->type == INTERRUPT_STATE) {
            state = c1_find_state(ecl, interrupt->lambda_name);
            if (state == NULL) {
                fprintf(stderr, "%s: state for interrupt %s not found: %s\n", argv0, interrupt->event->name, interrupt->lambda_name);
            }
            else {
                interrupt_val = state->index;
                if (state->index == 255) { /* lol */
                    fprintf(stderr, "%s: state '%s' for interrupt %s has index 255 and will be ignored. consider using less states.\n",
                        argv0, interrupt->lambda_name, interrupt->event->name);
                }
            }
        }
        else if (interrupt->type == INTERRUPT_SUB) {
            sub = th10_find_sub(ecl, interrupt->lambda_name);
            if (sub) {
                interrupt_val = 0x8000 | sub->start_offset;
            }
        }
        if (!file_write(out, &interrupt_val, sizeof(uint16_t))) return 0;
        if (!file_seek(out, pos + header->interrupt_count * 2)) return 0;
    }

    thecl_spawn_t* spawn;
    list_for_each(&ecl->spawns, spawn) {
        state = c1_find_state(ecl, spawn->state_name);
        uint16_t state_id = 255;
        if (state == NULL) {
            if (!file_write(out, &state_id, sizeof(uint16_t))) return 0;
        }
        else {
            if (!file_write(out, &state->index, sizeof(uint16_t))) return 0;
        }
    }
    pos = file_tell(out) % 4;
    if (pos != 0) {
        uint32_t a = 0;
        if (!file_write(out, &a, 4 - pos)) return 0;
    }

    entry_header->offsets[4] = file_tell(out);

    list_for_each(&ecl->states, state) {
        thecl_t* ecl_ext = state->exe != ecl ? state->exe : NULL;
        thecl_sub_t* sub_c = state->code ? c1_find_state_sub(ecl, ecl_ext, state->code, STATE_SUB_CODE) : NULL;
        thecl_sub_t* sub_e = state->event ? c1_find_state_sub(ecl, ecl_ext, state->event, STATE_SUB_EVENT) : NULL;
        thecl_sub_t* sub_t = state->trans ? c1_find_state_sub(ecl, ecl_ext, state->trans, STATE_SUB_TRANS) : NULL;
        if (!sub_c) {
            fprintf(stderr, "%s: warning: state %s has no code block\n", argv0, state->name);
        }
        if (sub_e) {
            if (sub_e->arg_count != 2) {
                fprintf(stderr, "%s: warning: state %s event block does not have 2 arguments\n", argv0, state->name);
            }
        }
        state_t gstate = { state->stateflag, state->statusc, gool_pool_force_get_index(ecl, state->exe->eid),
            state->event == NULL ? 0x3FFFU : sub_e->start_offset | (sub_e->is_external ? 0x4000 : 0),
            state->trans == NULL ? 0x3FFFU : sub_t->start_offset | (sub_t->is_external ? 0x4000 : 0),
            state->code == NULL ? 0x3FFFU : sub_c->start_offset | (sub_c->is_external ? 0x4000 : 0) };

        if (!file_write(out, &gstate, sizeof(gstate))) return 0;
    }

    entry_header->offsets[5] = file_tell(out);

    list_for_each(&ecl->anims, anim) {
        if (!file_write(out, anim->anim, anim->size)) return 0;
        pos = file_tell(out) % 4;
        if (pos != 0) {
            uint32_t a = 0;
            if (!file_write(out, &a, 4 - pos)) return 0;
        }
    }

    entry_header->offsets[6] = file_tell(out);

    if (!file_seek(out, entry_header->offsets[0])) return 0;
    if (!file_write(out, header, sizeof(gool_header_t))) return 0;

    if (!file_seek(out, 0)) return 0;
    if (!file_write(out, entry_header, sizeof(entry_header_t) + (entry_header->count + 1) * sizeof(uint32_t))) return 0;

    return 1;
}

static int
c1_compile(
    const parser_state_t* parser,
    FILE* out)
{
    if (!file_seekable(out)) {
        fprintf(stderr, "%s: output is not seekable\n", argv0);
        return 0;
    }

    entry_header_t entry_header = { 0x100FFFFU, parser->main_ecl->eid, 11U, 6U, { 0, 0, 0, 0, 0, 0, 0 } };
    gool_header_t header = { parser->main_ecl->id, parser->main_ecl->type << 8, 1, parser->main_ecl->var_count + 0x40, 0, 8 };
    thecl_sub_t* sub;
    thecl_instr_t* instr;

    thecl_t* ecl = parser->main_ecl;

    const thecl_state_t* state;

    /* write GOOL EIDs to const pool before anything else */
    list_for_each(&ecl->states, state) {
        gool_pool_force_get_index(ecl, state->exe->eid);
    }

    /* compile all subs */
    for (int i = 0; i < parser->ecl_cnt + 1; ++i) {
        ecl = i == 0 ? parser->main_ecl : parser->ecl_stack[i - 1];

        list_for_each(&ecl->subs, sub) {
            if (sub->forward_declaration || sub->is_inline || sub->deleted)
                continue;

            sub->instr_data = malloc(sizeof(gool_sub_t) + sizeof(uint32_t) * sub->offset);
            sub->instr_data->was_written = 0;

            int j = 0;
            list_for_each(&sub->instrs, instr) {
                sub->instr_data->data[j++] = c1_instr_serialize(parser->main_ecl, i > 0 ? ecl : NULL, sub, instr, true);
            }
        }
    }
    /* remove duplicates */
    for (int i = 0; i < parser->ecl_cnt + 1; ++i) {
        ecl = i == 0 ? parser->main_ecl : parser->ecl_stack[i - 1];

        list_for_each(&ecl->subs, sub) {
            if (sub->forward_declaration || sub->is_inline || sub->deleted)
                continue;

            for (int e = 0; e < parser->ecl_cnt + 1; ++e) {
                thecl_t* comp_ecl = e == 0 ? parser->main_ecl : parser->ecl_stack[e - 1];
                if ((i == 0 && e > 0) || (i > 0 && (e != i && e > 0))) continue; /* main checks self; ext checks main+self */
                thecl_sub_t* comp_sub;
                list_for_each(&comp_ecl->subs, comp_sub) {
                    if (comp_sub->deleted || comp_sub == sub || !comp_sub->instr_data || comp_sub->offset != sub->offset || (comp_sub->start_offset >= sub->start_offset && i == e))
                        continue;

                    for (int j = 0; j < sub->offset; ++j) {
                        if (sub->instr_data->data[j] != comp_sub->instr_data->data[j])
                            goto different_subs;
                    }
                    free(sub->instr_data);
                    sub->instr_data = comp_sub->instr_data;
                    sub->deleted = true;

                    thecl_sub_t* sub2;
                    list_for_each(&ecl->subs, sub2) {
                        if (sub2->start_offset > sub->start_offset && sub2->is_external == sub->is_external)
                            sub2->start_offset -= sub->offset;
                    }

                    sub->is_external = comp_sub->is_external;
                    sub->start_offset = comp_sub->start_offset;

                    goto double_break;
                    different_subs:;
                }
            }
            double_break:;
        }
    }
    /* re-compile with new offsets */
    for (int i = 0; i < parser->ecl_cnt + 1; ++i) {
        ecl = i == 0 ? parser->main_ecl : parser->ecl_stack[i - 1];

        list_for_each(&ecl->subs, sub) {
            if (sub->forward_declaration || sub->is_inline || sub->deleted)
                continue;

            int j = 0;
            list_for_each(&sub->instrs, instr) {
                sub->instr_data->data[j++] = c1_instr_serialize(parser->main_ecl, i > 0 ? ecl : NULL, sub, instr, false);
            }
        }
    }
    ecl = parser->main_ecl;

    /* write file contents */
    if (!c1_write_gool(ecl, &entry_header, &header, out)) return 0;

    if (parser->ecl_cnt && !g_module_fmt) {
        fprintf(stderr, "%s: external module name format not specified\n", argv0);
    }
    else {
        entry_header.count = 3;
        for (int i = 0; i < parser->ecl_cnt; ++i) {
            ecl = parser->ecl_stack[i];

            char buf[_MAX_PATH + 1];
            char ename[6];
            snprintf(buf, _MAX_PATH + 1, g_module_fmt, gool_to_ename(ename, parser->ecl_stack[i]->eid));
            FILE* out = fopen(buf, "wb");

            if (!file_seekable(out)) {
                fprintf(stderr, "%s: output is not seekable\n", argv0);
                return 0;
            }

            entry_header.id = ecl->eid;
            header.exe_type = 0;

            if (!file_write(out, &entry_header, sizeof(entry_header_t) + (entry_header.count + 1) * sizeof(uint32_t))) return 0;
            entry_header.offsets[0] = file_tell(out);
            if (!file_write(out, &header, sizeof(gool_header_t))) return 0;
            entry_header.offsets[1] = file_tell(out);

            list_for_each(&ecl->subs, sub) {
                if (sub->forward_declaration || sub->is_inline)
                    continue;

                if (sub->instr_data->was_written || sub->deleted)
                    continue;
                sub->instr_data->was_written = 1;

                if (!file_write(out, &sub->instr_data->data, sizeof(uint32_t) * sub->offset)) return 0;
            }

            entry_header.offsets[2] = file_tell(out);
            if (!file_write(out, ecl->consts, sizeof(int) * ecl->const_count)) return 0;
            entry_header.offsets[3] = file_tell(out);

            if (!file_seek(out, 0)) return 0;
            if (!file_write(out, &entry_header, sizeof(entry_header_t) + (entry_header.count + 1) * sizeof(uint32_t))) return 0;
            
            fclose(out);
        }
    }

    return 1;
}

static int
c1_create_header(
    const thecl_t* gool,
    FILE* out)
{
    thecl_spawn_t* spawn;
    char gool_name[6];

    fprintf(out, "expr %s = %u\n\n", gool_to_ename(gool_name, gool->eid), gool->id);

    list_for_each(&gool->spawns, spawn) {
        fprintf(out, "expr %s_%s = %u\n", gool_name, spawn->name, (int)spawn->offset);
    }

    return 1;
}

static int
c2_instr_serialize(
    thecl_t* ecl,
    thecl_t* ecl_ext,
    thecl_sub_t* sub,
    thecl_instr_t* instr,
    bool ignore_error)
{
    if (instr->mips) {
        if (instr->label_name) {
            thecl_label_t* label = label_find(sub, instr->label_name);
            if (label) {
                instr->ins.i.imm = label->offset - (instr->offset + 1);
            }
            else {
                fprintf(stderr, "%s:c2_instr_serialize: in sub %s: label not found '%s'", argv0, sub->name, instr->label_name);
            }
        }
        return instr->ins.ins;
    }

    const thecl_param_t* param;

    const char* format = th10_find_format(ecl->version, instr->id);

    int ret = c1_ins_init(instr->id);

    if (format == NULL) {
        fprintf(stderr, "%s:c2_instr_serialize: in sub %s: instruction with id %d is not known to exist in version %d\n", argv0, sub->name, instr->id, ecl->version);
        return ret;
    }

    bool was_error = false;

    if (instr->id == 59 || instr->id == 24) {
        /* Validate sub call parameters. */
        list_node_t* node = instr->params.head;
        const thecl_param_t* sub_name_param = node->data;
        char* sub_name = sub_name_param->value.val.z;
        const thecl_sub_t* called_sub = th10_find_sub(ecl, sub_name);
        if (!called_sub && ecl_ext) {
            called_sub = th10_find_sub(ecl_ext, sub_name);
        }
        if (!called_sub && !ignore_error) {
            fprintf(stderr, "%s:c2_instr_serialize: in sub %s: unknown sub call \"%s\"\n",
                    argv0, sub->name, sub_name);
            was_error = true;
        } else {
            if (instr->id == 59) {
                const thecl_param_t* sub_argc_param = node->next->next->data;
                called_sub = th10_find_sub_overload(ecl, sub_name, sub_argc_param->value.val.S);
                if (!called_sub && ecl_ext) {
                    called_sub = th10_find_sub(ecl_ext, sub_name);
                }
                if (!called_sub && !ignore_error) {
                    fprintf(stderr, "%s:c2_instr_serialize: in sub %s: no suitable parameter count for sub \"%s\"\n",
                        argv0, sub->name, sub_name);
                    was_error = true;
                }
            }
            thecl_param_t* sub_ext_param = instr->params.head->next->data;
            sub_ext_param->value.val.S = called_sub ? called_sub->is_external : 0;
        }
    }

    int total_bits = 0;
    int i = 0;
    char op;
    list_for_each(&instr->params, param) {
        if (total_bits >= 24 && !ignore_error) {
            fprintf(stderr, "%s:c2_instr_serialize: in sub %s: gool instruction %d parameter overflow\n", argv0, sub->name, instr->id);
            break;
        }
        int p;
        if (param->type == 'o' || param->type == 'z') {
            /* This calculates the relative offset from the current instruction. */
            thecl_label_t* label = param->type == 'z' ? NULL : label_find(sub, param->value.val.z);
            if (label) {
                p = label->offset - (instr->offset + 1);
            }
            else {
                const thecl_state_t* called_state;
                int o = 0;
                list_for_each(&ecl->states, called_state) {
                    if (!strcmp(called_state->name, param->value.val.z))
                        break;
                    ++o;
                    called_state = NULL;
                }
                if (!called_state) {
                    /* Validate sub call parameters. */
                    char* sub_name = param->value.val.z;
                    const thecl_sub_t* called_sub = th10_find_sub(ecl, sub_name);
                    if (!called_sub && ecl_ext) {
                        called_sub = th10_find_sub(ecl_ext, sub_name);
                    }
                    if (called_sub) {
                        if (instr->id == 59) {
                            const thecl_param_t* sub_argc_param = node->next->next->data;
                            called_sub = th10_find_sub_overload(ecl, sub_name, sub_argc_param->value.val.S);
                            if (!called_sub && ecl_ext) {
                                called_sub = th10_find_sub(ecl_ext, sub_name);
                            }
                        }
                    }
                    if (!called_sub) {
                        if (!was_error && !ignore_error) {
                            fprintf(stderr, "%s:c2_instr_serialize: in sub %s: sub/state/label not found: %s\n", argv0, sub->name, param->value.val.z);
                        }
                        p = o;
                    }
                    else {
                        p = called_sub->start_offset;
                    }
                }
                else
                    p = o;
            }
        }
        else
            p = param->value.val.S;
    RECHECK_PARAM:;
        while ((op = format[i++]) == ' ');
        int bits = total_bits;
        int val = 0;
        if (op == 'R') {
            if (param->stack) {
                if (param->object_link == 0) {
                    val = c1_make_ref_reg(p); /* reg ref */
                }
                else if (param->object_link == -1) {
                    val = c1_make_ref_stack(p); /* stack ref */
                }
                else if (param->object_link > 0 && param->object_link <= 7) {
                    val = c1_make_ref_ireg(param->object_link, p); /* ireg ref */
                }
                else if (param->object_link == -2) {
                    val = c1_make_ref_special(param->value.val.S); /* sp-double ref OR null ref */
                }
                else if (param->object_link == -3) {
                    val = c1_make_ref_pool(gool_pool_force_make_index(ecl, p)); /* pool ref */
                }
            }
            else {
                if (!(p % 0x100) && p >= -256 * 0x100 && p <= 255 * 0x100) {
                    val = c1_make_ref_int(p / 0x100); /* int ref */
                }
                else if (!(p % 0x10) && p >= -128 * 0x10 && p <= 127 * 0x10) {
                    val = c1_make_ref_frac(p / 0x10); /* frac ref */
                }
                else {
                    if (ecl_ext && ecl_ext->purge_data) {
                        val = c1_make_ref_null(); /* null ref */
                    }
                    else {
                        val = c1_make_ref_pool(gool_pool_force_get_index(ecl, p)); /* pool ref */
                    }
                    /* DOES NOT WORK IN-GAME! */
                    //if (!ecl_ext || (ecl_ext && gool_pool_get_index(ecl, p) != -1)) {
                    //    val = c1_make_ref_pool(gool_pool_force_get_index(ecl, p)); /* pool ref */
                    //}
                    //else {
                    //    val = c1_make_ref_extpool(gool_pool_force_get_index(ecl_ext, p)); /* pool ref */
                    //}
                }
            }
            total_bits += 12;
        }
        else if (op == 'I') {
            const char* next_format;
            uint32_t b = strtol(format + i, &next_format, 10);
            i = next_format - format;

            val = p;

            int s = 32 - b;
            int sm = s-1;
            val &= 0xFFFFFFFFU >> s;
            if (p != ((val << s) >> s) && p != ((val << sm) >> sm) && !ignore_error) {
                fprintf(stderr, "%s:c2_instr_serialize: in sub %s: parameter out of bounds for instruction %d (%u bits)\n", argv0, sub->name, instr->id, b);
            }
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
            val = c1_make_ref_null(); /* null ref */
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

    if (total_bits > 24 && !ignore_error) {
        fprintf(stderr, "%s:c2_instr_serialize: in sub %s: gool instruction %d format overflow\n", argv0, sub->name, instr->id);
    }

    return ret;
}

static int
c2_compile(
    const parser_state_t* parser,
    FILE* out)
{
    if (!file_seekable(out)) {
        fprintf(stderr, "%s: output is not seekable\n", argv0);
        return 0;
    }

    entry_header_t entry_header = { 0x100FFFFU, parser->main_ecl->eid, 11U, 6U, { 0, 0, 0, 0, 0, 0, 0 } };
    gool_header_t header = { parser->main_ecl->id, parser->main_ecl->type << 8, 1, parser->main_ecl->var_count + 0x40, 0, 8 };
    thecl_sub_t* sub;
    const thecl_state_t* state;
    gool_anim_t* anim;
    thecl_instr_t* instr;

    thecl_t* ecl = parser->main_ecl;

    /* write GOOL EIDs to const pool before anything else */
    list_for_each(&ecl->states, state) {
        gool_pool_force_get_index(ecl, state->exe->eid);
    }

    /* compile all subs */
    for (int i = 0; i < parser->ecl_cnt + 1; ++i) {
        ecl = i == 0 ? parser->main_ecl : parser->ecl_stack[i - 1];

        list_for_each(&ecl->subs, sub) {
            if (sub->forward_declaration || sub->is_inline || sub->deleted)
                continue;

            sub->instr_data = malloc(sizeof(gool_sub_t) + sizeof(uint32_t) * sub->offset);
            sub->instr_data->was_written = 0;

            int j = 0;
            list_for_each(&sub->instrs, instr) {
                sub->instr_data->data[j++] = c2_instr_serialize(parser->main_ecl, i > 0 ? ecl : NULL, sub, instr, true);
            }
        }
    }
    /* remove duplicates */
    for (int i = 0; i < parser->ecl_cnt + 1; ++i) {
        ecl = i == 0 ? parser->main_ecl : parser->ecl_stack[i - 1];

        list_for_each(&ecl->subs, sub) {
            if (sub->forward_declaration || sub->is_inline || sub->deleted)
                continue;

            for (int e = 0; e < parser->ecl_cnt + 1; ++e) {
                thecl_t* comp_ecl = e == 0 ? parser->main_ecl : parser->ecl_stack[e - 1];
                if ((i == 0 && e > 0) || (i > 0 && (e != i && e > 0))) continue; /* main checks self; ext checks main+self */
                thecl_sub_t* comp_sub;
                list_for_each(&comp_ecl->subs, comp_sub) {
                    if (comp_sub->deleted || comp_sub == sub || !comp_sub->instr_data || comp_sub->offset != sub->offset || (comp_sub->start_offset >= sub->start_offset && i == e))
                        continue;

                    for (int j = 0; j < sub->offset; ++j) {
                        if (sub->instr_data->data[j] != comp_sub->instr_data->data[j])
                            goto different_subs;
                    }
                    free(sub->instr_data);
                    sub->instr_data = comp_sub->instr_data;
                    sub->deleted = true;

                    thecl_sub_t* sub2;
                    list_for_each(&ecl->subs, sub2) {
                        if (sub2->start_offset > sub->start_offset && sub2->is_external == sub->is_external)
                            sub2->start_offset -= sub->offset;
                    }

                    sub->is_external = comp_sub->is_external;
                    sub->start_offset = comp_sub->start_offset;

                    goto double_break;
                    different_subs:;
                }
            }
            double_break:;
        }
    }
    /* re-compile with new offsets */
    for (int i = 0; i < parser->ecl_cnt + 1; ++i) {
        ecl = i == 0 ? parser->main_ecl : parser->ecl_stack[i - 1];

        list_for_each(&ecl->subs, sub) {
            if (sub->forward_declaration || sub->is_inline || sub->deleted)
                continue;

            int j = 0;
            list_for_each(&sub->instrs, instr) {
                sub->instr_data->data[j++] = c2_instr_serialize(parser->main_ecl, i > 0 ? ecl : NULL, sub, instr, false);
            }
        }
    }
    ecl = parser->main_ecl;

    /* write file contents */
    if (!c1_write_gool(ecl, &entry_header, &header, out)) return 0;

    if (parser->ecl_cnt && !g_module_fmt) {
        fprintf(stderr, "%s: external module name format not specified\n", argv0);
    }
    else {
        entry_header.count = 3;
        for (int i = 0; i < parser->ecl_cnt; ++i) {
            ecl = parser->ecl_stack[i];

            char buf[_MAX_PATH + 1];
            char ename[6];
            snprintf(buf, _MAX_PATH + 1, g_module_fmt, gool_to_ename(ename, parser->ecl_stack[i]->eid));
            FILE* out = fopen(buf, "wb");

            if (!file_seekable(out)) {
                fprintf(stderr, "%s: output is not seekable\n", argv0);
                return 0;
            }

            entry_header.id = ecl->eid;
            header.exe_type = 0;

            if (!file_write(out, &entry_header, sizeof(entry_header_t) + (entry_header.count + 1) * sizeof(uint32_t))) return 0;
            entry_header.offsets[0] = file_tell(out);
            if (!file_write(out, &header, sizeof(gool_header_t))) return 0;
            entry_header.offsets[1] = file_tell(out);

            list_for_each(&ecl->subs, sub) {
                if (sub->forward_declaration || sub->is_inline)
                    continue;

                if (sub->instr_data->was_written || sub->deleted)
                    continue;
                sub->instr_data->was_written = 1;

                if (!file_write(out, &sub->instr_data->data, sizeof(uint32_t) * sub->offset)) return 0;
            }

            entry_header.offsets[2] = file_tell(out);
            if (!file_write(out, ecl->consts, sizeof(int) * ecl->const_count)) return 0;
            entry_header.offsets[3] = file_tell(out);

            if (!file_seek(out, 0)) return 0;
            if (!file_write(out, &entry_header, sizeof(entry_header_t) + (entry_header.count + 1) * sizeof(uint32_t))) return 0;
            
            fclose(out);
        }
    }

    return 1;
}

const thecl_module_t c1_gool = {
    .open = NULL,
    .trans = NULL,
    .dump = NULL,
    .parse = c1_parse,
    .compile = c1_compile,
    .create_header = c1_create_header
};

const thecl_module_t c2_gool = {
    .open = NULL,
    .trans = NULL,
    .dump = NULL,
    .parse = c1_parse,
    .compile = c2_compile,
    .create_header = c1_create_header
};
