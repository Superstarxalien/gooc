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
#include <string.h>
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
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_var_new("animframe"));
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(3));
    }
    else if (c == 1) {
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(3));
    }
    else if (c == 2) {
        list_append_new(params, param_val_new(3));
    }
    return params;
}

static list_t*
c1_gool_ins_anim_params(
    list_t* params,
    int argc)
{
    if (g_warn_deprecate_anim) {
        fprintf(stderr, "%s:%s:anim: deprecate function. use getanim instead (i.e. 'animseq = getanim(AnimName)')\n", argv0, current_input);
        g_warn_deprecate_anim = false;
    }
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_prepend_new(params, param_var_new("animseq"));
    }
    else if (c == 2) {
    }
    else {
        fprintf(stderr, "%s:%s:anim: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_playanim_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2) {
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(3));
    }
    else if (c == 3) {
        list_append_new(params, param_val_new(3));
    }
    else if (c != 4) {
        fprintf(stderr, "%s:%s:playanim: wrong number of arguments (expected 2, 3 or 4, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    param = params->head->next->data;
    if (param->val_type == PARAM_LITERAL && param->value.type == 'S') {
        param->value.val.S >>= 8;
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
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(3));
    }
    else {
        fprintf(stderr, "%s:%s:playtext: wrong number of arguments (expected 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    param = params->head->next->data;
    if (param->val_type == PARAM_LITERAL && param->value.type == 'S') {
        param->value.val.S >>= 8;
    }
    return params;
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
        list_append_new(params, param_val_new(argc));
        list_append_new(params, param_val_new(0x25));
        list_append_new(params, param_val_new(type));
        list_append_new(params, param_val_new(1));
    }
    else if (c == 2) {
        param = param_new('S');
        param->value.val.S = argc;
        list_append_to(params, param, params->head);
        list_append_new(params, param_val_new(type));
        list_append_new(params, param_val_new(1));
    }
    else {
        fprintf(stderr, "%s:%s:%s:wrong number of arguments (expected at least %d, got %zu)\n", argv0, current_input, type == 0 ? "changestate" : (type == 1 ? "changestateif" : "changestateifn"), 1 + (type == 1 || type == 2), c+argc);
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
    size_t c = list_count(params);
    if (c != 2) {
        fprintf(stderr, "%s:%s:changestateif: wrong number of arguments (expected 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return c1_gool_ins_changestate_params(params, argc, 1);
}

static list_t*
c1_gool_ins_stateifn_params(
    list_t* params,
    int argc)
{
    size_t c = list_count(params);
    if (c != 2) {
        fprintf(stderr, "%s:%s:changestateifn: wrong number of arguments (expected 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return c1_gool_ins_changestate_params(params, argc, 2);
}

static list_t*
c1_gool_ins_setcolor_params(
    list_t* params,
    int argc)
{
    if (g_warn_deprecate_setcolor) {
        fprintf(stderr, "%s:%s:setcolor: deprecate function. use as address instead (i.e. 'color = 255')\n", argv0, current_input);
        g_warn_deprecate_setcolor = false;
    }
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
        fprintf(stderr, "%s:%s:setcolor: wrong number of arguments (expected 3 or 2, got %zu)\n", argv0, current_input, c);
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
            list_append_new(params, param_val_new(1));
        }

        list_node_t *node, *next;
        list_for_each_node_safe(params, node, next) {
            list_prepend_new(params, node->data);
            list_del(params, node);
        }

        list_append_new(params, param_val_new(argc));
    }
    else {
        fprintf(stderr, "%s:%s:spawn: wrong number of arguments (expected 2 or 3, got %zu)\n", argv0, current_input, c);
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
        list_prepend_to(params, param_val_new(0), params->tail);
        list_prepend_to(params, param_val_new(argc), params->tail);
    }
    else {
        fprintf(stderr, "%s:%s:sendevent: wrong number of arguments (expected at least 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_sendeventif_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 3) {
        list_append_new(params, params->head->next->data);
        list_del(params, params->head->next);
        list_prepend_to(params, param_val_new(argc), params->tail);
    }
    else {
        fprintf(stderr, "%s:%s:sendeventif: wrong number of arguments (expected at least 3, got %zu)\n", argv0, current_input, c);
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
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0x25));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:accept/rejectevent: wrong number of arguments (expected 0 or 1, got %zu)\n", argv0, current_input, c);
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
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0x25));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(2));
    }
    else if (c == 1) {
        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);

        param = param_new('S');
        param->value.val.S = 0;
        list_prepend_new(params, param);
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(2));
    }
    else {
        fprintf(stderr, "%s:%s:accept/rejectevent: wrong number of arguments (expected 0 or 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_eventstatusstate_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0x25));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(1));
    }
    else if (c == 2) {
        param = param_new('S');
        param->value.val.S = 0;
        list_append_to(params, param, params->head);
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(1));
    }
    else {
        fprintf(stderr, "%s:%s:accept/rejectevent: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_onexit_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(field_get("hpc")->offset));
    }
    else {
        fprintf(stderr, "%s:%s:onstateexit: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_settrans_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(field_get("tpc")->offset));
    }
    else {
        fprintf(stderr, "%s:%s:settrans: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
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
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(4));
        param = list_head(params);
        if (param->val_type == PARAM_LITERAL && g_warn_deprecate_setfield) {
            fprintf(stderr, "%s:%s:setfield: deprecate use of literals as field offset. use as address instead (i.e. 'ObjVar1 = 10.5')\n", argv0, current_input);
            g_warn_deprecate_setfield = false;
        }
    }
    else {
        fprintf(stderr, "%s:%s:setfield: wrong number of arguments (expected 3, got %zu)\n", argv0, current_input, c + argc);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_movetozone_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_null_new());
        ++c;
    }
    if (c == 2) {
        list_append_new(params, params->head->data);
        list_del(params, params->head);
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(9));
    }
    else {
        fprintf(stderr, "%s:%s:movetozone: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, current_input, c);
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
        list_prepend_new(params, param_val_new(field_get("player")->offset));
        list_prepend_new(params, param_var_new("id"));
        list_append_new(params, param_val_new(8));
    }
    else {
        fprintf(stderr, "%s:%s:entitysetspawn: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
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
    if (c == 1) {
        list_prepend_new(params, param_var_new("id"));
        c = 2;
    }
    if (c == 2) {
        list_append_to(params, param_val_new(5), params->head);
        list_append_new(params, param_val_new(10));
    }
    else {
        fprintf(stderr, "%s:%s:entitysetstate: wrong number of arguments (expected 2, got %zu)\n", argv0, current_input, c);
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

        list_prepend_to(params, param_val_new(5), params->tail);
        list_prepend_to(params, param_val_new(6), params->tail);

        param = params->head->next->data;
        if (param->value.val.S >= 8) {
            param->value.val.S -= 8;
            param->value.val.S /= 3;
        }
    }
    else {
        fprintf(stderr, "%s:%s:getvert: wrong number of arguments (expected 3, got %zu)\n", argv0, current_input, c);
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
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_var_new("pathprog"));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 1) {
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 2) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:calcpath: wrong number of arguments (expected 0, 1 or 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_soundplay_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c != 2) {
        fprintf(stderr, "%s:%s:soundplay: wrong number of arguments (expected 2, got %zu)\n", argv0, current_input, c);
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
    if (c == 2) {
        list_append_to(params, param_val_new(0), params->head);
        list_prepend_to(params, param_val_new(0), params->tail);
    }
    else if (c == 3) {
        list_prepend_to(params, param_val_new(0), params->tail);
    }
    else if (c == 4) {
    }
    else {
        fprintf(stderr, "%s:%s:soundsetup: wrong number of arguments (expected 2, 3 or 4, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_soundcheck_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_prepend_new(params, param_null_new());
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(13));
    }
    else if (c == 1) {
        list_prepend_new(params, param_null_new());
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(13));
    }
    else if (c == 2) {
        list_prepend_new(params, param_null_new());
        list_append_new(params, param_val_new(13));
    }
    else {
        fprintf(stderr, "%s:%s:soundcheck: wrong number of arguments (expected 0, 1 or 2, got %zu)\n", argv0, current_input, c);
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
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(type));
    }
    else if (c == 2) {
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(type));
    }
    else if (c == 3) {
        list_append_new(params, param_val_new(type));
    }
    else {
        fprintf(stderr, "%s:%s:%s:wrong number of arguments (expected 1, 2 or 3, got %zu)\n", argv0, current_input, name, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_soundfadevol_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_soundspec_params(params, 0, "soundfadev");
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
c1_gool_ins_soundfadetime_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_soundspec_params(params, 6, "soundfadet");
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
c1_gool_ins_soundset_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_soundspec_params(params, 13, "soundset");
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
        list_append_new(params, param_val_new(type));
    }
    else {
        fprintf(stderr, "%s:%s:%s:wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, name, c);
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

static list_t*
c1_gool_ins_loadfile2_params(
    list_t* params,
    int argc)
{
    return c1_gool_ins_ntry_params(params, 6, "loadfile2");
}

static list_t*
c1_gool_ins_calclight_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_var_new("v0"));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(4));
        list_append_new(params, param_val_new(6));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:calclight: wrong number of arguments (expected 0, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_projzone_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 2) {
        list_append_to(params, param_val_new(5), params->head);
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:projectzoneshadow: wrong number of arguments (expected 1 or 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_projobj_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(3));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:projectobjshadow: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_savecheckpoint_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_null_new());
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:savecheckpoint: wrong number of arguments (expected 0, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_loadcheckpoint_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_null_new());
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:loadcheckpoint: wrong number of arguments (expected 0, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_movetolist_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(2));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:movetolist: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_debugfunc_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(3));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:debugfunc: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_gamefunc4_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(4));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:gamefunc4: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_soundstop_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_null_new());
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:soundstop: wrong number of arguments (expected 0, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_seqplay_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(6));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:seqplay: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
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
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(9));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:loadlevel: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_gamefunc10_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(10));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:gamefunc10: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_startgame_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_null_new());
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(11));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:startgame: wrong number of arguments (expected 0, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_unkget_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(12));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:unkget: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_unkset_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(13));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:unkset: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_moveto2d_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_var_new("scale"));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 2) {
        param = params->head->data;
        if (param->value.val.S >= 8) {
            param->value.val.S -= 8;
            param->value.val.S /= 3;
        }
        param = params->head->next->data;
        if (param->value.val.S >= 8) {
            param->value.val.S -= 8;
            param->value.val.S /= 3;
        }

        list_prepend_new(params, param_var_new("scale"));
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 3) {
        param = params->head->next->data;
        if (param->value.val.S >= 8) {
            param->value.val.S -= 8;
            param->value.val.S /= 3;
        }
        param = params->head->next->next->data;
        if (param->value.val.S >= 8) {
            param->value.val.S -= 8;
            param->value.val.S /= 3;
        }

        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:moveto2d: wrong number of arguments (expected 0, 2 or 3, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_setvel_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_var_new("speed"));
        list_append_new(params, param_val_new(3));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(2));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 1) {
        list_append_new(params, param_val_new(3));
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(2));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 2) {
        param = params->head->next->data;
        if (param->value.val.S >= 8) {
            param->value.val.S -= 8;
            param->value.val.S /= 3;
        }

        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(2));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:setvel: wrong number of arguments (expected 0, 1 or 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

/* Format is: trans, out, z, x, y */
static list_t*
c1_gool_ins_vectransf_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 3 && argc == 2) {
        list_prepend_new(params, params->tail->data);
        list_del(params, params->tail);
        list_append_new(params, param_val_new(4));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:vectransf: wrong number of arguments (expected 5, got %zu)\n", argv0, current_input, c+argc);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_vectransf2_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 3 && argc == 2) {
        list_prepend_new(params, params->tail->data);
        list_del(params, params->tail);
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:vectransf2: wrong number of arguments (expected 5, got %zu)\n", argv0, current_input, c + argc);
        return NULL;
    }
    return params;
}

static list_t*
c1_gool_ins_checkzonecollision_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_prepend_new(params, param_val_new(3));
        list_prepend_new(params, param_null_new());
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 2) {
        list_prepend_new(params, param_val_new(3));
        list_prepend_new(params, params->tail->data);
        list_del_tail(params);
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
    }
    else if (c == 3) {
        list_prepend_new(params, params->tail->data);
        list_del_tail(params);
        list_prepend_new(params, params->tail->data);
        list_del_tail(params);
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(0));
    }
    else {
        fprintf(stderr, "%s:%s:checkzonecollision: wrong number of arguments (expected 1, 2 or 3, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static const gool_ins_t
c1_gool_ins[] = {
     /* NAME                        ID VA POP R   L              VALIDATE */
    { "onstateexit",                24, 0, 0, 0, -1, c1_gool_ins_onexit_params },
    { "settrans",                   24, 0, 0, 0, -1, c1_gool_ins_settrans_params },
    { "setfield",                   28, 2, 0, 0, -1, c1_gool_ins_setfield_params },
    { "entitysetspawn",             28, 0, 0, 0, -1, c1_gool_ins_entitysetspawn_params },
    { "movetozone",                 28, 0, 0, 0, -1, c1_gool_ins_movetozone_params },
    { "entitysetstate",             28, 0, 0, 0, -1, c1_gool_ins_entitysetstate_params },
    { "savecheckpoint",             28, 0, 0, 0, -1, c1_gool_ins_savecheckpoint_params },
    { "loadcheckpoint",             28, 0, 0, 0, -1, c1_gool_ins_loadcheckpoint_params },
    { "movetolist",                 28, 0, 0, 0, -1, c1_gool_ins_movetolist_params },
    { "debugfunc",                  28, 0, 0, 0, -1, c1_gool_ins_debugfunc_params },
    { "gamefunc4",                  28, 0, 0, 0, -1, c1_gool_ins_gamefunc4_params },
    { "soundstop",                  28, 0, 0, 0, -1, c1_gool_ins_soundstop_params },
    { "seqplay",                    28, 0, 0, 0, -1, c1_gool_ins_seqplay_params },
    { "loadlevel",                  28, 0, 0, 0, -1, c1_gool_ins_loadlevel_params },
    { "gamefunc10",                 28, 0, 0, 0, -1, c1_gool_ins_gamefunc10_params },
    { "startgame",                  28, 0, 0, 0, -1, c1_gool_ins_startgame_params },
    { "unkget",                     28, 0, 0, 0, -1, c1_gool_ins_unkget_params },
    { "unkset",                     28, 0, 0, 0, -1, c1_gool_ins_unkset_params },
    { "setcolor",                   36, 0, 0, 0, -1, c1_gool_ins_setcolor_params },
    { "anim",                       39, 0, 0, 0, -1, c1_gool_ins_anim_params },
    { "nop",                      0x81, 0, 0, 0, -1, c1_gool_ins_nop_params },
    { "changestate",              0x82, 1, 0, 0, -1, c1_gool_ins_state_params },
    { "changestateif",            0x82, 2, 0, 0,  1, c1_gool_ins_stateif_params },
    { "changestateifn",           0x82, 2, 0, 0,  1, c1_gool_ins_stateifn_params },
    { "playanim",                 0x83, 4, 1, 1, -1, c1_gool_ins_playanim_params },
    { "playtext",                 0x83, 2, 1, 1, -1, c1_gool_ins_playtext_params },
    { "playframe",                0x84, 3, 1, 1, -1, c1_gool_ins_playframe_params },
    { "calcpath",                 0x85, 0, 0, 0, -1, c1_gool_ins_calcpath_params },
    { "getvert",                  0x85, 0, 0, 0, -1, c1_gool_ins_getvert_params },
    { "moveto2d",                 0x85, 0, 0, 0, -1, c1_gool_ins_moveto2d_params },
    { "setvel",                   0x85, 0, 0, 0, -1, c1_gool_ins_setvel_params },
    { "vectransf",                0x85, 3, 0, 0, -1, c1_gool_ins_vectransf_params },
    { "vectransf2",               0x85, 3, 0, 0, -1, c1_gool_ins_vectransf2_params },
    { "sendevent",                0x87, 2, 0, 0, -1, c1_gool_ins_sendevent_params },
    { "sendeventif",              0x87, 3, 0, 0,  2, c1_gool_ins_sendeventif_params },
    { "rejectevent",              0x88, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "acceptevent",              0x89, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "rejecteventandreturn",     0x88, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "accepteventandreturn",     0x89, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "rejecteventandchangestate",0x88, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "accepteventandchangestate",0x89, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "rejev",                    0x88, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "accev",                    0x89, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "rejevret",                 0x88, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "accevret",                 0x89, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "rejevcstate",              0x88, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "accevcstate",              0x89, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "spawn",                    0x8A, 3, 0, 0, -1, c1_gool_ins_spawn_params },
    { "loadfile",                 0x8B, 0, 0, 0, -1, c1_gool_ins_loadfile_params },
    { "deloadfile",               0x8B, 0, 0, 0, -1, c1_gool_ins_deloadfile_params },
    { "loadfile2",                0x8B, 0, 0, 0, -1, c1_gool_ins_loadfile2_params },
    { "soundplay",                0x8C, 0, 0, 0, -1, c1_gool_ins_soundplay_params },
    { "soundsetup",               0x8D, 0, 0, 0,  1, c1_gool_ins_setupsound_params },
    { "soundfadev",               0x8D, 0, 0, 0,  1, c1_gool_ins_soundfadevol_params },
    { "soundpitch",               0x8D, 0, 0, 0,  1, c1_gool_ins_soundpitch_params },
    { "soundcount",               0x8D, 0, 0, 0,  1, c1_gool_ins_soundcount_params },
    { "soundfadet",               0x8D, 0, 0, 0,  1, c1_gool_ins_soundfadetime_params },
    { "sounddelay",               0x8D, 0, 0, 0,  1, c1_gool_ins_sounddelay_params },
    { "sounddecay",               0x8D, 0, 0, 0,  1, c1_gool_ins_sounddecay_params },
    { "soundset",                 0x8D, 0, 0, 0,  1, c1_gool_ins_soundset_params },
    { "soundcheck",               0x8D, 0, 0, 0,  1, c1_gool_ins_soundcheck_params },
    { "checkzonecollision",       0x8E, 0, 0, 0, -1, c1_gool_ins_checkzonecollision_params },
    { "calclight",                0x8E, 0, 0, 0, -1, c1_gool_ins_calclight_params },
    { "projectobjshadow",         0x8E, 0, 0, 0, -1, c1_gool_ins_projobj_params },
    { "projectzoneshadow",        0x8E, 0, 0, 0, -1, c1_gool_ins_projzone_params },
    { "broadcastevent",           0x8F, 2, 0, 0, -1, c1_gool_ins_sendevent_params },
    { "broadcasteventif",         0x8F, 3, 0, 0,  2, c1_gool_ins_sendeventif_params },
    { "cascadeevent",             0x90, 2, 0, 0, -1, c1_gool_ins_sendevent_params },
    { "cascadeeventif",           0x90, 3, 0, 0,  2, c1_gool_ins_sendeventif_params },
    { "spawn2",                   0x91, 3, 0, 0, -1, c1_gool_ins_spawn_params },
    { NULL, 0, 0, 0, 0, 0, NULL }
};

static list_t*
c2_gool_ins_onexit_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(field_get("hpc")->offset));
    }
    else {
        fprintf(stderr, "%s:%s:onstateexit: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c2_gool_ins_settrans_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(field_get("tpc")->offset));
    }
    else {
        fprintf(stderr, "%s:%s:settrans: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c2_gool_ins_call_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(argc << 8));
    }
    else {
        fprintf(stderr, "%s:%s:call: wrong number of arguments (expected at least 1, got %zu)\n", argv0, current_input, c+argc);
        return NULL;
    }
    return params;
}

static list_t*
c2_gool_ins_savecheckpoint_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(0));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:savecheckpoint: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c2_gool_ins_loadcheckpoint_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 1) {
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(1));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:loadcheckpoint: wrong number of arguments (expected 1, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c2_gool_ins_ins46_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2) {
    }
    else {
        fprintf(stderr, "%s:%s:ins46: wrong number of arguments (expected 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c2_gool_ins_ins72_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 2) {
    }
    else {
        fprintf(stderr, "%s:%s:ins72: wrong number of arguments (expected 2, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static list_t*
c2_gool_ins_killvictims_params(
    list_t* params,
    int argc)
{
    thecl_param_t* param;
    size_t c = list_count(params);
    if (c == 0) {
        list_append_new(params, param_null_new());
        list_append_new(params, param_val_new(5));
        list_append_new(params, param_val_new(18));
        list_append_new(params, param_val_new(12));
    }
    else {
        fprintf(stderr, "%s:%s:killvictims: wrong number of arguments (expected 0, got %zu)\n", argv0, current_input, c);
        return NULL;
    }
    return params;
}

static const gool_ins_t
c2_gool_ins[] = {
     /* NAME                        ID VA POP R   L              VALIDATE */
    { "onstateexit",                24, 0, 0, 0, -1, c2_gool_ins_onexit_params },
    { "settrans",                   24, 0, 0, 0, -1, c2_gool_ins_settrans_params },
    { "setfield",                   28, 2, 0, 0, -1, c1_gool_ins_setfield_params },
    { "entitysetspawn",             28, 0, 0, 0, -1, c1_gool_ins_entitysetspawn_params },
    { "entitysetstate",             28, 0, 0, 0, -1, c1_gool_ins_entitysetstate_params },
    { "savecheckpoint",             28, 0, 0, 0, -1, c2_gool_ins_savecheckpoint_params },
    { "loadcheckpoint",             28, 0, 0, 0, -1, c2_gool_ins_loadcheckpoint_params },
    { "movetolist",                 28, 0, 0, 0, -1, c1_gool_ins_movetolist_params },
    { "debugfunc",                  28, 0, 0, 0, -1, c1_gool_ins_debugfunc_params },
    { "gamefunc4",                  28, 0, 0, 0, -1, c1_gool_ins_gamefunc4_params },
    { "soundstop",                  28, 0, 0, 0, -1, c1_gool_ins_soundstop_params },
    { "seqplay",                    28, 0, 0, 0, -1, c1_gool_ins_seqplay_params },
    { "loadlevel",                  28, 0, 0, 0, -1, c1_gool_ins_loadlevel_params },
    { "gamefunc10",                 28, 0, 0, 0, -1, c1_gool_ins_gamefunc10_params },
    { "startgame",                  28, 0, 0, 0, -1, c1_gool_ins_startgame_params },
    { "unkget",                     28, 0, 0, 0, -1, c1_gool_ins_unkget_params },
    { "unkset",                     28, 0, 0, 0, -1, c1_gool_ins_unkset_params },
    { "killvictims",                28, 0, 0, 0, -1, c2_gool_ins_killvictims_params },
    { "setcolor",                   36, 0, 0, 0, -1, c1_gool_ins_setcolor_params },
    { "anim",                       39, 0, 0, 0, -1, c1_gool_ins_anim_params },
    { "nop",                        47, 0, 0, 0, -1, c1_gool_ins_nop_params },
    { "changestate",                53, 1, 0, 0, -1, c1_gool_ins_state_params },
    { "changestateif",              54, 2, 0, 0,  1, c1_gool_ins_stateif_params },
    { "changestateifn",             55, 2, 0, 0,  1, c1_gool_ins_stateifn_params },
    { "playanim",                   56, 4, 1, 1, -1, c1_gool_ins_playanim_params },
    { "playtext",                   56, 2, 1, 1, -1, c1_gool_ins_playtext_params },
    { "playframe",                  57, 3, 1, 1, -1, c1_gool_ins_playframe_params },
    { "calcpath",                   58, 0, 0, 0, -1, c1_gool_ins_calcpath_params },
    { "moveto2d",                   58, 0, 0, 0, -1, c1_gool_ins_moveto2d_params },
    { "getvert",                    58, 0, 0, 0, -1, c1_gool_ins_getvert_params },
    { "setvel",                     58, 0, 0, 0, -1, c1_gool_ins_setvel_params },
    { "vectransf",                  58, 3, 0, 0,  2, c1_gool_ins_vectransf_params },
    { "vectransf2",                 58, 3, 0, 0,  2, c1_gool_ins_vectransf2_params },
    { "sendevent",                  60, 2, 0, 0, -1, c1_gool_ins_sendevent_params },
    { "sendeventif",                60, 3, 0, 0,  2, c1_gool_ins_sendeventif_params },
    { "rejectevent",                61, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "acceptevent",                62, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "rejecteventandreturn",       61, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "accepteventandreturn",       62, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "rejecteventandchangestate",  61, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "accepteventandchangestate",  62, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "rejev",                      61, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "accev",                      62, 0, 0, 0,  0, c1_gool_ins_eventstatus_params },
    { "rejevret",                   61, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "accevret",                   62, 0, 0, 0,  0, c1_gool_ins_eventstatusreturn_params },
    { "rejevcstate",                61, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "accevcstate",                62, 0, 0, 0,  1, c1_gool_ins_eventstatusstate_params },
    { "spawn",                      63, 3, 0, 0, -1, c1_gool_ins_spawn_params },
    { "loadfile",                   64, 0, 0, 0, -1, c1_gool_ins_loadfile_params },
    { "deloadfile",                 64, 0, 0, 0, -1, c1_gool_ins_deloadfile_params },
    { "loadfile2",                  64, 0, 0, 0, -1, c1_gool_ins_loadfile2_params },
    { "soundplay",                  65, 0, 0, 0, -1, c1_gool_ins_soundplay_params },
    { "soundsetup",                 66, 0, 0, 0,  1, c1_gool_ins_setupsound_params },
    { "soundfadev",                 66, 0, 0, 0,  1, c1_gool_ins_soundfadevol_params },
    { "soundpitch",                 66, 0, 0, 0,  1, c1_gool_ins_soundpitch_params },
    { "soundcount",                 66, 0, 0, 0,  1, c1_gool_ins_soundcount_params },
    { "soundfadet",                 66, 0, 0, 0,  1, c1_gool_ins_soundfadetime_params },
    { "sounddelay",                 66, 0, 0, 0,  1, c1_gool_ins_sounddelay_params },
    { "sounddecay",                 66, 0, 0, 0,  1, c1_gool_ins_sounddecay_params },
    { "soundset",                   66, 0, 0, 0,  1, c1_gool_ins_soundset_params },
    { "soundcheck",                 66, 0, 0, 0,  1, c1_gool_ins_soundcheck_params },
    { "checkzonecollision",         67, 0, 0, 0, -1, c1_gool_ins_checkzonecollision_params },
    { "calclight",                  67, 0, 0, 0, -1, c1_gool_ins_calclight_params },
    { "projectobjshadow",           67, 0, 0, 0, -1, c1_gool_ins_projobj_params },
    { "projectzoneshadow",          67, 0, 0, 0, -1, c1_gool_ins_projzone_params },
    { "broadcastevent",             68, 2, 0, 0, -1, c1_gool_ins_sendevent_params },
    { "broadcasteventif",           68, 3, 0, 0,  2, c1_gool_ins_sendeventif_params },
    { "cascadeevent",               69, 2, 0, 0, -1, c1_gool_ins_sendevent_params },
    { "cascadeeventif",             69, 3, 0, 0,  2, c1_gool_ins_sendeventif_params },
    { "spawn2",                     70, 3, 0, 0, -1, c1_gool_ins_spawn_params },
    { "call",                       71, 1, 0, 0, -1, c2_gool_ins_call_params },
    { "ins46",                      46, 0, 0, 0, -1, c2_gool_ins_ins46_params },
    { "ins72",                      72, 0, 0, 0, -1, c2_gool_ins_ins72_params },
    { NULL, 0, 0, 0, 0, 0, NULL }
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
    const gool_ins_t* table = NULL;

    switch (version)
    {
    case 2: table = c2_gool_ins; break;
    case 1: table = c1_gool_ins; break;
    }

    ret = gool_ins_get_by_name_from_table(table, name);

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
    const gool_ins_t* table = NULL;

    switch (version)
    {
    case 2: table = c2_gool_ins; break;
    case 1: table = c1_gool_ins; break;
    }

    ret = gool_ins_get_by_id_from_table(table, id);

    return ret;
}
