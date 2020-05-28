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
#include "program.h"
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
    { "jr",    'R', "8",  "0", "0",  "0",  "rs", "0" },
    { "jalr",  'R', "9",  "0", "rd", "0",  "rs", "0" },

    { "mfhi",  'R', "16", "0", "rd", "0", "0", "0" },
    { "mflo",  'R', "18", "0", "rd", "0", "0", "0" },
    { "mult",  'R', "24", "0", "0", "rt", "rs", "0" },
    { "div",   'R', "26", "0", "0", "rt", "rs", "0" },

    { "addu",  'R', "33", "0", "rd", "rt", "rs", "0" },
    { "subu",  'R', "35", "0", "rd", "rt", "rs", "0" },
    { "and",   'R', "36", "0", "rd", "rt", "rs", "0" },
    { "or",    'R', "37", "0", "rd", "rt", "rs", "0" },
    { "xor",   'R', "38", "0", "rd", "rt", "rs", "0" },
    { "nor",   'R', "39", "0", "rd", "rt", "rs", "0" },

    { "beq",   'I', "imm", "rt", "rs",  "4", NULL, NULL },
    { "bne",   'I', "imm", "rt", "rs",  "5", NULL, NULL },
    { "blez",  'I', "imm", "rt", "rs",  "6", NULL, NULL },
    { "bgtz",  'I', "imm", "rt", "rs",  "7", NULL, NULL },

    { "addiu", 'I', "imm", "rt", "rs",  "9", NULL, NULL },
    { "slti",  'I', "imm", "rt", "rs", "10", NULL, NULL },
    { "sltiu", 'I', "imm", "rt", "rs", "11", NULL, NULL },
    { "andi",  'I', "imm", "rt", "rs", "12", NULL, NULL },
    { "ori",   'I', "imm", "rt", "rs", "13", NULL, NULL },
    { "xori",  'I', "imm", "rt", "rs", "14", NULL, NULL },
    { "lui",   'I', "imm", "rt", "0",  "15", NULL, NULL },

    { "lb",    'I', "imm", "rt", "rs", "32", NULL, NULL },
    { "lh",    'I', "imm", "rt", "rs", "33", NULL, NULL },
    { "lwl",   'I', "imm", "rt", "rs", "34", NULL, NULL },
    { "lw",    'I', "imm", "rt", "rs", "35", NULL, NULL },
    { "lbu",   'I', "imm", "rt", "rs", "36", NULL, NULL },
    { "lhu",   'I', "imm", "rt", "rs", "37", NULL, NULL },
    { "lwr",   'I', "imm", "rt", "rs", "38", NULL, NULL },
    { "sb",    'I', "imm", "rt", "rs", "40", NULL, NULL },
    { "sh",    'I', "imm", "rt", "rs", "41", NULL, NULL },
    { "swl",   'I', "imm", "rt", "rs", "42", NULL, NULL },
    { "sw",    'I', "imm", "rt", "rs", "43", NULL, NULL },
    { "swr",   'I', "imm", "rt", "rs", "46", NULL, NULL },
    { NULL,    0, NULL, NULL, NULL, NULL, NULL, NULL }
};

mips_reg_block_t*
mips_reg_block_new(void)
{
    mips_reg_block_t* new_block = calloc(1, sizeof(mips_reg_block_t));
    new_block->reg_index = 0;
    for (int i=0; i<32; ++i) {
        new_block->regs[i].index = i;
        new_block->regs[i].name = mips_registers[i];
        new_block->regs[i].saved_expr = NULL;
        new_block->regs[i].saved_param = NULL;
        new_block->regs[i].last_used = 0;
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
get_reg(mips_reg_block_t* block, const char* name)
{
    for (int i=0; i<32; ++i) {
        if (!strcmp(name, block->regs[i].name)) {
            return &block->regs[i];
        }
    }
    return NULL;
}

mips_reg_t*
get_usable_reg(mips_reg_block_t* block)
{
    for (int i=0; i<32; block->reg_index = ++block->reg_index % 32, ++i) {
        if (block->regs[block->reg_index].status == MREG_STATUS_FREE) {
            return &block->regs[block->reg_index];
        }
    }
    for (int i=0; i<32; block->reg_index = ++block->reg_index % 32, ++i) {
        if (block->regs[block->reg_index].status == MREG_STATUS_USED) {
            return &block->regs[block->reg_index];
        }
    }
    return NULL;
}

void
free_reg(mips_reg_t* reg)
{
    if (reg->status == MREG_STATUS_IN_USE || reg->status == MREG_STATUS_USED) {
        reg->status = MREG_STATUS_FREE;
        if (reg->saved_expr) {
            expression_free(reg->saved_expr);
        }
        if (reg->saved_param) {
            param_free(reg->saved_param);
        }
    }
}

void
clean_regs(mips_reg_block_t* block)
{
    for (int i = 0; i < 32; ++i) {
        free_reg(&block->regs[i]);
    }
    block->reg_index = 0;
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

mips_ins_fmt_t*
mips_find_format(const char* name)
{
    const mips_ins_fmt_t* format = mips_instructions;
    while (format->name) {
        if (!strcmp(format->name, name)) {
            return format;
        }
        ++format;
    }
    return NULL;
}

int
mips_instr_init(
    const char* name,
    int imm,
    int shamt,
    int rd,
    int rt,
    int rs,
    int addr)
{
    const mips_ins_fmt_t* fmt = mips_find_format(name);
    if (fmt) {
        mips_ins_t ins;
        ins.ins = 0;
        switch (fmt->fmt) {
        case 'R':
            ins.r.funct = atoi(fmt->ops[0]);
            ins.r.shamt = !strcmp(fmt->ops[1], "shamt") ? shamt : atoi(fmt->ops[1]);
            ins.r.rd = !strcmp(fmt->ops[2], "rd") ? rd : atoi(fmt->ops[2]);
            ins.r.rt = !strcmp(fmt->ops[3], "rt") ? rt : atoi(fmt->ops[3]);
            ins.r.rs = !strcmp(fmt->ops[4], "rs") ? rs : atoi(fmt->ops[4]);
            ins.r.opcode = atoi(fmt->ops[5]);
            break;
        case 'I':
            ins.i.imm = !strcmp(fmt->ops[0], "imm") ? imm : atoi(fmt->ops[0]);
            ins.i.rt = !strcmp(fmt->ops[1], "rt") ? rt : atoi(fmt->ops[1]);
            ins.i.rs = !strcmp(fmt->ops[2], "rs") ? rs : atoi(fmt->ops[2]);
            ins.i.opcode = atoi(fmt->ops[3]);
            break;
        case 'J':
            ins.j.addr = !strcmp(fmt->ops[0], "addr") ? addr : atoi(fmt->ops[0]);
            ins.j.opcode = atoi(fmt->ops[1]);
            break;
        default:
            fprintf(stdout, "%s:mips_instr_init: error: invalid mips instruction type %c", argv0, fmt->fmt);
            exit(2);
        }
        return ins.ins;
    }
    else {
        fprintf(stdout, "%s:mips_instr_init: mips instruction not foun: %s", argv0, name);
    }
    return 0;
}

uint64_t
mips_instr_getregs(const char* name, mips_ins_t *ins)
{
    const mips_ins_fmt_t* fmt = mips_find_format(name);
    uint64_t reg = 0;
    if (fmt) {
        switch (fmt->fmt) {
        case 'R':
            if (!strcmp(fmt->ops[2], "rd")) reg |= 1ULL << ins->r.rd;
            if (!strcmp(fmt->ops[3], "rt")) reg |= 1ULL << ins->r.rt;
            if (!strcmp(fmt->ops[4], "rs")) reg |= 1ULL << ins->r.rs;
            break;
        case 'I':
            if (!strcmp(fmt->ops[1], "rt")) reg |= 1ULL << ins->i.rt;
            if (!strcmp(fmt->ops[2], "rs")) reg |= 1ULL << ins->i.rs;
            break;
        case 'J':
            break;
        default:
            fprintf(stdout, "%s:mips_instr_init: error: invalid mips instruction type %c", argv0, fmt->fmt);
            exit(2);
        }
        if ((ins->i.opcode >= 32 && ins->i.opcode <= 38) || (ins->i.opcode >= 40 && ins->i.opcode <= 46)) {
            if (!strcmp(fmt->ops[1], "rt")) reg |= 0x100000000 << ins->i.rt;
        }
    }
    else {
        fprintf(stdout, "%s:mips_instr_init: mips instruction not foun: %s", argv0, name);
    }
    return reg;
}

int
mips_instr_is_branch(mips_ins_t* ins)
{
    return (ins->r.opcode == 0 && ins->r.funct == 8)
        || (ins->r.opcode == 0 && ins->r.funct == 9)
        || (ins->i.opcode == 1 && ins->i.rt == 0)
        || (ins->i.opcode == 1 && ins->i.rt == 1)
        || (ins->i.opcode == 1 && ins->i.rt == 16)
        || (ins->i.opcode == 1 && ins->i.rt == 17)
        || (ins->j.opcode == 2)
        || (ins->j.opcode == 3)
        || (ins->i.opcode == 4)
        || (ins->i.opcode == 5)
        || (ins->i.opcode == 6 && ins->i.rt == 0)
        || (ins->i.opcode == 7 && ins->i.rt == 0)
        ;
}
