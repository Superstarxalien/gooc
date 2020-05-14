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
#include <stdlib.h>
#include <string.h>
#include "thecl.h"
#include "ecsparse.h"
#include "gmips.h"

static const char*
mips_registers[] = {
    "zr",
    "at",
    "v0", "v1",
    "a0", "a1", "a2", "a3",
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "t8", "t9",
    "k0", "k1",
    "gp",
    "sp",
    "s8",
    "ra"
};

static const mips_ins_fmt_t
mips_instructions[] = {
    { "nop",   'R', "0",  "0", "0",  "0",  "0",  "0" },
    { "jr",    'R', "9",  "0", "rd", "0",  "rs", "0" },
    { "addu",  'R', "33", "0", "rd", "rt", "rs", "0" },
    { "subu",  'R', "35", "0", "rd", "rt", "rs", "0" },
    { "and",   'R', "36", "0", "rd", "rt", "rs", "0" },
    { "or",    'R', "37", "0", "rd", "rt", "rs", "0" },
    { "xor",   'R', "38", "0", "rd", "rt", "rs", "0" },
    { "nor",   'R', "39", "0", "rd", "rt", "rs", "0" }
};

mips_reg_block_t*
mips_reg_block_new(void)
{
    mips_reg_block_t* new_block = calloc(1, sizeof(mips_reg_block_t));
    for (int i=0; i<32; ++i) {
        new_block->regs[i].index = i;
        new_block->regs[i].name = mips_registers[i];
        new_block->regs[i].saved_expr = NULL;
        new_block->regs[i].saved_param = NULL;
        new_block->regs[i].status = MREG_STATUS_FREE;
    }
    new_block->regs[0].status = MREG_STATUS_RESERVED; /* zr */
    new_block->regs[1].status = MREG_STATUS_RESERVED; /* at */
    new_block->regs[16].status = MREG_STATUS_RESERVED; /* s0 */
    new_block->regs[17].status = MREG_STATUS_RESERVED; /* s1 */
    new_block->regs[18].status = MREG_STATUS_RESERVED; /* s2 */
    new_block->regs[19].status = MREG_STATUS_RESERVED; /* s3 */
    new_block->regs[20].status = MREG_STATUS_RESERVED; /* s4 */
    new_block->regs[21].status = MREG_STATUS_RESERVED; /* s5 - actually usable but doesn't matter right now */
    new_block->regs[22].status = MREG_STATUS_RESERVED; /* s6 */
    new_block->regs[23].status = MREG_STATUS_RESERVED; /* s7 */
    new_block->regs[26].status = MREG_STATUS_RESERVED; /* k0 */
    new_block->regs[27].status = MREG_STATUS_RESERVED; /* k1 */
    new_block->regs[28].status = MREG_STATUS_RESERVED; /* gp */
    new_block->regs[29].status = MREG_STATUS_RESERVED; /* sp */
    new_block->regs[30].status = MREG_STATUS_RESERVED; /* s8 - actually usable but doesn't matter right now */
    new_block->regs[31].status = MREG_STATUS_RESERVED; /* ra */
    return new_block;
}

mips_reg_t*
get_usable_reg(mips_reg_block_t* block)
{
    for (int i=0; i<32; ++i) {
        if (block->regs[i].status == MREG_STATUS_FREE || block->regs[i].status == MREG_STATUS_USED) {
            return &block->regs[i];
        }
    }
    return NULL;
}

thecl_param_t*
reg_get_param(mips_reg_t* reg)
{
    if (reg->status == MREG_STATUS_FREE) {
        return NULL;
    }
    else if (reg->status == MREG_STATUS_USED) {
        return reg->saved_param;
    }
    else return NULL;
}

expression_t*
reg_get_expr(mips_reg_t* reg)
{
    if (reg->status == MREG_STATUS_FREE) {
        return NULL;
    }
    else if (reg->status == MREG_STATUS_USED) {
        return reg->saved_expr;
    }
    else return NULL;
}
