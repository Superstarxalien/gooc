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
#ifndef GOOL_INS_H_
#define GOOL_INS_H_

#include <config.h>
#include <stddef.h>
#include "list.h"

typedef enum {
    GOOL_INS_STATE,
    GOOL_INS_PLAYTEXT,
    GOOL_INS_SET_COLOR,
    GOOL_INS_ANIM,
    GOOL_INS_PLAYFRAME
} gool_ins_type;

typedef struct {
    char* name;
    enum gool_ins_type type;
    uint8_t id;
    bool varargs;
    bool pop_args;
    int param_count;

    list_t* (*param_list_validate)(list_t* params);
} gool_ins_t;

/* Returns an expression by its symbol. */
const gool_ins_t* gool_ins_get_by_name(unsigned int version, char* name);

/* Returns an expression by its id. */
const gool_ins_t* gool_ins_get_by_id(unsigned int version, uint8_t id);

#endif
