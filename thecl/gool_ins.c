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
#include <stddef.h>
#include "thecl.h"
#include "field.h"
#include "program.h"
#include "gool_ins.h"

static list_t*
c1_gool_ins_playframe_params(
    list_t* params)
{
    thecl_param_t* param;
    if (!params) {
        list_t* ret = list_new();

        param = param_new('S');
        param->stack = 1;
        param->object_link = 0;
        param->value.val.S = field_get("animframe")->offset;
        list_append_new(ret, param);

        param = param_new('S');
        param->value.val.S = 50;
        list_append_new(ret, param);

        param = param_new('S');
        param->value.val.S = 3;
        list_append_new(ret, param);

        return ret;
    }
    else {
        size_t c = list_count(params);
        if (c == 3) {
            return params;
        }
        else {
            fprintf(stderr, "%s: playframe: wrong number of arguments (expected 3, got %zu)\n", argv0, c);
            return NULL;
        }
    }
}

static list_t*
c1_gool_ins_anim_params(
    list_t* params)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        param = list_head(params);

        if (!param->stack) {
            param->value.val.S <<= 8;
        }

        param = param_new('S');
        param->stack = 1;
        param->object_link = 0;
        param->value.val.S = field_get("animseq")->offset;
        list_prepend_new(params, param);
        return params;
    }
    else if (c == 2) {
        return params;
    }
    else {
        fprintf(stderr, "%s: anim: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, c);
        return NULL;
    }
}

static list_t*
c1_gool_ins_playtext_params(
    list_t* params)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2) {
        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 3;
        list_append_new(params, param);

        return params;
    }
    else {
        fprintf(stderr, "%s: anim: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, c);
        return NULL;
    }
}

static const gool_ins_t
c1_gool_ins[] = {
     /* NAME               TYPE           ID          VALIDATE */
     { "playtext",  GOOL_INS_PLAYTEXT,  0x83,  true,  true,  2, c1_gool_ins_playtext_params },
     { "anim",      GOOL_INS_ANIM,        39, false, false, -1, c1_gool_ins_anim_params },
     { "playframe", GOOL_INS_PLAYFRAME, 0x84, false, false, -1, c1_gool_ins_playframe_params },
     { NULL, 0, 0, NULL }
};

static const gool_ins_t*
gool_ins_get_by_name_from_table(
    const gool_ins_t* table,
    char* name)
{
    while (table->name) {
        if (!strcmp(table->name, name))
            return table;
        ++table;
    }

    return NULL;
}

const gool_ins_t*
gool_ins_get_by_name(
    unsigned int version,
    char* name)
{
    const gool_ins_t* ret = NULL;

    if (version == 1 && !ret) ret = gool_ins_get_by_name_from_table(c1_gool_ins, name);

    return ret;
}

static const gool_ins_t*
gool_ins_get_by_id_from_table(
    const gool_ins_t* table,
    uint8_t id)
{
    while (table->name) {
        if (table->id == id)
            return table;
        ++table;
    }

    return NULL;
}

const gool_ins_t*
gool_ins_get_by_id(
    unsigned int version,
    uint8_t id)
{
    const gool_ins_t* ret = NULL;

    if (version == 1 && !ret) ret = gool_ins_get_by_id_from_table(c1_gool_ins, id);

    return ret;
}
