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
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    if (!params)
        params = list_new();
    size_t c = list_count(params);
    if (c == 0) {
        param = param_new('S');
        param->stack = 1;
        param->object_link = 0;
        param->value.val.S = field_get("animframe")->offset;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 3;
        list_append_new(params, param);
    }
    else if (c == 1) {
        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 3;
        list_append_new(params, param);
    }
    else if (c == 2) {
        param = param_new('S');
        param->value.val.S = 3;
        list_append_new(params, param);
    }
    return params;
}

static list_t*
c1_gool_ins_anim_params(
    list_t* params,
    int argc)
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
c1_gool_ins_playanim_params(
    list_t* params,
    int argc)
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
    }
    else if (c == 3) {
        param = param_new('S');
        param->value.val.S = 3;
        list_append_new(params, param);
    }
    else if (c != 4) {
        fprintf(stderr, "%s: playanim: wrong number of arguments (expected 2, 3 or 4, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_playtext_params(
    list_t* params,
    int argc)
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
        fprintf(stderr, "%s: playtext: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, c);
        return NULL;
    }
}

static list_t*
c1_gool_ins_changestate_params(
    list_t* params,
    int argc,
    int type)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_node_t* node = params->head;

        param = param_new('S');
        param->value.val.S = argc;
        list_append_to(params, param, node);
        node = node->next;

        param = param_new('S');
        param->value.val.S = 0x25;
        list_append_to(params, param, node);
        node = node->next;

        param = param_new('S');
        param->value.val.S = type;
        list_append_to(params, param, node);
        node = node->next;

        param = param_new('S');
        param->value.val.S = 1;
        list_append_to(params, param, node);
    }
    else if (c == 2) {
        param = param_new('S');
        param->value.val.S = argc;
        list_append_to(params, param, params->head);

        param = param_new('S');
        param->value.val.S = type;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: changestate: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, c);
        return NULL;
    }

    return params;
}

static list_t*
c1_gool_ins_state_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_changestate_params(params, argc, 0);
}

static list_t*
c1_gool_ins_stateif_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_changestate_params(params, argc, 1);
}

static list_t*
c1_gool_ins_stateifn_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_changestate_params(params, argc, 2);
}

static list_t*
c1_gool_ins_setcolor_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 3) {
    }
    else if (c == 2) {
        list_node_t* node = params->head;

        param = param_new('S');
        param->value.val.S = 0;
        list_append_to(params, param, node);
        node = node->next;
    }
    else {
        fprintf(stderr, "%s: setcolor: wrong number of arguments (expected 3, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_nop_params(
    list_t* params,
    int argc)
{
    return params;
}

static list_t*
c1_gool_ins_spawn_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 3 || c == 2) {
        if (c == 2) {
            param = param_new('S');
            param->value.val.S = 1;
            list_append_new(params, param);
        }

        list_node_t *node, *next;
        list_for_each_node_safe(params, node, next) {
            list_prepend_new(params, node->data);
            list_del(params, node);
        }

        param = param_new('S');
        param->value.val.S = argc;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: spawn: wrong number of arguments (expected 2 or 3, got %zu)\n", argv0, c);
        return NULL;
    }

    return params;
}

static list_t*
c1_gool_ins_sendevent_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2) {
        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_to(params, param, params->tail);

        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_to(params, param, params->tail);
    }
    else if (c == 3) {
        thecl_param_t* receiver = params->head->next->data;
        list_del(params, params->head->next);

        param = param_new('S');
        param->value.val.S = argc;
        list_append_new(params, param);

        list_append_new(params, receiver);
    }
    else {
        fprintf(stderr, "%s: sendevent: wrong number of arguments (expected at least 3, got %zu)\n", argv0, c);
        return NULL;
    }

    return params;
}

static list_t*
c1_gool_ins_eventstatus_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    if (!params)
        params = list_new();
    size_t c = list_count(params);
    if (c == 0) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0x25;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);
    }
    else if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: accept/rejectevent: wrong number of arguments (expected 0 or 1, got %zu)\n", argv0, c);
        return NULL;
    }

    return params;
}

static list_t*
c1_gool_ins_eventstatusreturn_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    if (!params)
        params = list_new();
    size_t c = list_count(params);
    if (c == 0) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0x25;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 2;
        list_append_new(params, param);

        return params;
    }
    else if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 2;
        list_append_new(params, param);

        return params;
    }
    else {
        fprintf(stderr, "%s: accept/rejectevent: wrong number of arguments (expected 0 or 1, got %zu)\n", argv0, c);
        return NULL;
    }
}

static list_t*
c1_gool_ins_eventstatusstate_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0x25;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        return params;
    }
    else if (c == 2) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_to(params, param, params->head);

        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 1;
        list_append_new(params, param);

        return params;
    }
    else {
        fprintf(stderr, "%s: accept/rejectevent: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, c);
        return NULL;
    }
}

static list_t*
c1_gool_ins_onexit_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        param = param_new('S');
        param->value.val.S = field_get("hpc")->offset;
        list_append_new(params, param);

        return params;
    }
    else {
        fprintf(stderr, "%s: onstateexit: wrong number of arguments (expected 0, got %zu)\n", argv0, c);
        return NULL;
    }
}

static list_t*
c1_gool_ins_setfield_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2 && argc == 1) {
        list_append_new(params, params->head->data);
        list_del(params, params->head);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 4;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: setfield: wrong number of arguments (expected 3, got %zu)\n", argv0, c + argc);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_movetozoneinposition_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2) {
        list_append_new(params, params->head->data);
        list_del(params, params->head);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 9;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: movetozoneinposition: wrong number of arguments (expected 2, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_entitysetspawn_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        param = param_new('S');
        param->value.val.S = field_get("player")->offset;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = field_get("id")->offset;
        param->stack = 1;
        param->object_link = 0;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = 8;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: entitysetspawn: wrong number of arguments (expected 1, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_entitysetstate_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2) {
        param = param_new('S');
        param->value.val.S = 5;
        list_append_to(params, param, params->head);

        param = param_new('S');
        param->value.val.S = 10;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: entitysetstate: wrong number of arguments (expected 2, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_getvert_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 3) {
        list_prepend_new(params, params->tail->data);
        list_del(params, params->tail);

        param = param_new('S');
        param->value.val.S = 5;
        list_prepend_to(params, param, params->tail);

        param = param_new('S');
        param->value.val.S = 6;
        list_prepend_to(params, param, params->tail);

        param = params->head->next->data;
        param->value.val.S -= 8;
        param->value.val.S /= 3;
    }
    else {
        fprintf(stderr, "%s: getvert: wrong number of arguments (expected 3, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_calcpath_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    if (!params)
        params = list_new();
    size_t c = list_count(params);
    if (c == 0) {
        param = param_new('S');
        param->value.val.S = field_get("pathprog")->offset;
        param->object_link = 0;
        param->stack = 1;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 5;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);
    }
    else if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 5;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);
    }
    else if (c == 2) {

        param = param_new('S');
        param->value.val.S = 5;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: getvert: wrong number of arguments (expected 0, 1 or 2, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_playsound_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c != 2) {
        fprintf(stderr, "%s: playsound: wrong number of arguments (expected 2, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_setupsound_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 3) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_to(params, param, params->head);
    }
    else if (c == 4) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_to(params, param, params->head);
    }
    else {
        fprintf(stderr, "%s: soundsetup: wrong number of arguments (expected 3 or 4, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_loadlevel_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 9;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 12;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: loadlevel: wrong number of arguments (expected 1, got %zu)\n", argv0, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_soundspec_params(
    list_t* params,
    int type,
    const char* name)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_append_new(params, param);

        param = param_new('S');
        param->value.val.S = type;
        list_append_new(params, param);
    }
    else if (c == 2) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_to(params, param, params->head);

        param = param_new('S');
        param->value.val.S = type;
        list_append_new(params, param);
    }
    else if (c == 3) {
        param = param_new('S');
        param->value.val.S = type;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: %s: wrong number of arguments (expected 1, 2 or 3, got %zu)\n", argv0, name, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_soundpitch_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_soundspec_params(params, 1, "soundpitch");
}

static list_t*
c1_gool_ins_soundcount_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_soundspec_params(params, 4, "soundcount");
}

static list_t*
c1_gool_ins_sounddelay_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_soundspec_params(params, 7, "sounddelay");
}

static list_t*
c1_gool_ins_sounddecay_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_soundspec_params(params, 12, "sounddecay");
}

static list_t*
c1_gool_ins_ntry_params(
    list_t* params,
    int type,
    const char* name)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        param = param_new('S');
        param->value.val.S = type;
        list_append_new(params, param);
    }
    else {
        fprintf(stderr, "%s: %s: wrong number of arguments (expected 1, got %zu)\n", argv0, name, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_loadfile_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_ntry_params(params, 1, "loadfile");
}

static list_t*
c1_gool_ins_deloadfile_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_ntry_params(params, 2, "deloadfile");
}

static const gool_ins_t
c1_gool_ins[] = {
     /* NAME                        ID  VA POP R   L   C              VALIDATE */
     { "onstateexit",                24, 0, 0, 0, -1,  1, c1_gool_ins_onexit_params },
     { "setfield",                   28, 1, 0, 0, -1,  2, c1_gool_ins_setfield_params },
     { "entitysetspawn",             28, 0, 0, 0, -1,  1, c1_gool_ins_entitysetspawn_params },
     { "movetozoneinposition",       28, 0, 0, 0, -1,  2, c1_gool_ins_movetozoneinposition_params },
     { "entitysetstate",             28, 0, 0, 0, -1,  2, c1_gool_ins_entitysetstate_params },
     { "loadlevel",                  28, 0, 0, 0, -1,  1, c1_gool_ins_loadlevel_params },
     { "setcolor",                   36, 0, 0, 0, -1,  3, c1_gool_ins_setcolor_params },
     { "anim",                       39, 0, 0, 0, -1,  2, c1_gool_ins_anim_params },
     { "nop",                      0x81, 0, 0, 0, -1,  0, c1_gool_ins_nop_params },
     { "changestate",              0x82, 0, 0, 0, -1,  1, c1_gool_ins_state_params },
     { "changestateif",            0x82, 0, 0, 0, -1,  2, c1_gool_ins_stateif_params },
     { "changestateifn",           0x82, 0, 0, 0, -1,  2, c1_gool_ins_stateifn_params },
     { "playanim",                 0x83, 1, 1, 1, -1,  4, c1_gool_ins_playanim_params },
     { "playtext",                 0x83, 1, 1, 1, -1,  2, c1_gool_ins_playtext_params },
     { "playframe",                0x84, 1, 1, 1, -1,  3, c1_gool_ins_playframe_params },
     { "calcpath",                 0x85, 0, 0, 0, -1,  3, c1_gool_ins_calcpath_params },
     { "getvert",                  0x85, 0, 0, 0, -1,  3, c1_gool_ins_getvert_params },
     { "sendevent",                0x87, 1, 0, 0,  2,  3, c1_gool_ins_sendevent_params },
     { "rejectevent",              0x88, 0, 0, 0, -1,  1, c1_gool_ins_eventstatus_params },
     { "acceptevent",              0x89, 0, 0, 0, -1,  1, c1_gool_ins_eventstatus_params },
     { "rejecteventandreturn",     0x88, 0, 0, 0, -1,  1, c1_gool_ins_eventstatusreturn_params },
     { "accepteventandreturn",     0x89, 0, 0, 0, -1,  1, c1_gool_ins_eventstatusreturn_params },
     { "rejecteventandchangestate",0x88, 0, 0, 0, -1,  2, c1_gool_ins_eventstatusstate_params },
     { "accepteventandchangestate",0x89, 0, 0, 0, -1,  2, c1_gool_ins_eventstatusstate_params },
     { "spawn",                    0x8A, 1, 0, 0, -1,  3, c1_gool_ins_spawn_params },
     { "loadfile",                 0x8B, 0, 0, 0, -1,  1, c1_gool_ins_loadfile_params },
     { "deloadfile",               0x8B, 0, 0, 0, -1,  1, c1_gool_ins_deloadfile_params },
     { "soundplay",                0x8C, 0, 0, 0, -1,  2, c1_gool_ins_playsound_params },
     { "soundsetup",               0x8D, 0, 0, 0, -1,  2, c1_gool_ins_setupsound_params },
     { "soundpitch",               0x8D, 0, 0, 0, -1,  2, c1_gool_ins_soundpitch_params },
     { "soundcount",               0x8D, 0, 0, 0, -1,  2, c1_gool_ins_soundcount_params },
     { "sounddelay",               0x8D, 0, 0, 0, -1,  2, c1_gool_ins_sounddelay_params },
     { "sounddecay",               0x8D, 0, 0, 0, -1,  2, c1_gool_ins_sounddecay_params },
     { "broadcastevent",           0x8F, 1, 0, 0,  2,  3, c1_gool_ins_sendevent_params },
     { "cascadeevent",             0x90, 1, 0, 0,  2,  3, c1_gool_ins_sendevent_params },
     { "tryspawn",                 0x91, 1, 0, 0, -1,  3, c1_gool_ins_spawn_params },
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
