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
#include "ecsparse.h"
#include "expr.h"

static const expr_t
global_expressions[] = {
    /* The program checks against the number of params, as well as the
     * requested stack depth, and does the replacements. */
    /* p0 is the first param, p1 the second ... */
    /* s0 is the previous instruction, s1 the one previous to s0 ... */

    /*SYM        ID  A SP2 O  U */
    { LOAD,      22, 0, 0, 0, 0 }, /* p0 */
    { GLOAD,     31, 0, 0, 0, 0 }, /* p0 */
    { CLOAD,     35, 0, 0, 0, 0 }, /* p0 */
    { PLOAD,     38, 0, 0, 0, 0 }, /* &p0 */
    { ASSIGN,    17, 1, 0, 0, 0 }, /* p0 = s0 */
    { PASSIGN,   20, 2, 0, 0, 1 }, /* p0 = &s0 */
    { GASSIGN,   32, 1, 0, 0, 0 }, /* p0 = s0 */
    { CASSIGN,   36, 1, 0, 0, 0 }, /* p0 = s0 */

    { ADD,        0, 2, 0, 1, 0 }, /* s1 + s0 */
    { SUBTRACT,   1, 2, 0, 1, 0 }, /* s1 - s0 */
    { MULTIPLY,   2, 2, 0, 1, 0 }, /* s1 * s0 */
    { DIVIDE,     3, 2, 0, 1, 0 }, /* s1 / s0 */
    { EQUAL,      4, 2, 0, 1, 0 }, /* s1 == s0 */
    { INEQUAL,   -2, 2, 0, 1, 0 }, /* GOOL does not have this instruction, use NOT + CEQ instead /* s1 != s0 */
    { AND,        5, 2, 0, 1, 0 }, /* s1 && s0 */
    { OR,         6, 2, 0, 1, 0 }, /* s1 || s0 */
    { B_AND,      7, 2, 0, 1, 0 }, /* s1 & s0 */
    { B_OR,       8, 2, 0, 1, 0 }, /* s1 | s0 */
    { LT,         9, 2, 0, 1, 0 }, /* s1 < s0 */
    { LTEQ,      10, 2, 0, 1, 0 }, /* s1 <= s0 */
    { GT,        11, 2, 0, 1, 0 }, /* s1 > s0 */
    { GTEQ,      12, 2, 0, 1, 0 }, /* s1 >= s0 */
    { MODULO,    13, 2, 0, 1, 0 }, /* s1 % s0 */
    { XOR,       14, 2, 0, 1, 0 }, /* s1 ^ s0 */
    { TEST,      15, 2, 0, 1, 0 }, /* s1 \ s0 */
    { RAND,      16, 2, 0, 0, 0 }, /* rand(s0, s1) */
    { NOT,       18, 2, 0, 1, 1 }, /* !s0 */
    { LOOPSEEK,  19, 3, 1, 0, 0 }, /* loopseek(s0, s1, s2) */
    { LSHIFT,    21, 2, 0, 1, 0 }, /* s1 << s0 */
    { RSHIFT,    -1, 2, 0, 1, 0 }, /* s1 >> s0 */
    { B_NOT,     23, 2, 0, 1, 1 }, /* ~s0 */
    { ABS,       25, 2, 0, 1, 1 }, /* abs(s0) */
    { PAD,       26, 5, 0, 0, 0 }, /* pad(s0, s1, s2, s3, s4) */
    { SPD,       27, 2, 0, 0, 0 }, /* spd(s0, s1) */
    { MISC,      28, 4, 0, 0, 0 }, /* misc(s0, s1, s2, s3) */
    { PSIN,      29, 2, 0, 1, 0 }, /* sin(s0, s1) */
    { TIME,      30, 2, 0, 0, 0 }, /* time(s0, s1) */
    { DEGDIST,   33, 2, 0, 0, 0 }, /* degdist(s0, s1) */
    { SEEK,      34, 3, 1, 0, 0 }, /* seek(s0, s1, s2) */
    { GETCOLOR,  35, 2, 0, 0, 0 }, /* getcolor(s0, s1) */
    { DEGSEEK,   37, 3, 1, 0, 0 }, /* degseek(s0, s1, s2) */
    { GETANIM,   39, 2, 0, 0, 1 }, /* getanim(s0) */
    { 0,          0, 0, 0, 0, 0 }
};

static const expr_t
c1_expressions[] = {
    /* The program checks against the number of params, as well as the
     * requested stack depth, and does the replacements. */
    /* p0 is the first param, p1 the second ... */
    /* s0 is the previous instruction, s1 the one previous to s0 ... */

    /*SYM        ID     P  A SP2 O  U */
    { CALL,    0x86, 0, 0, 0, 0 }, /* return */
    { RETURN,  0x82, 0, 0, 0, 0 }, /* return */

    { GOTO,    0x82, 0, 0, 0, 0 }, /* goto p0 */
    { UNLESS,  0x82, 1, 0, 0, 0 }, /* unless (s0) goto p0 */
    { IF,      0x82, 1, 0, 0, 0 }, /* if (s0) goto p0 */

    { EVHA,    0x89, 5, 0, 0, 0 }, /* accept event */
    { EVHR,    0x8A, 5, 0, 0, 0 }, /* reject event */
    { NTRY,    0x8B, 2, 0, 0, 0 }, /* entry operation */
    { 0,          0, 0, 0, 0, 0 }
};

static const expr_t
c2_expressions[] = {
    /* The program checks against the number of params, as well as the
     * requested stack depth, and does the replacements. */
    /* p0 is the first param, p1 the second ... */
    /* s0 is the previous instruction, s1 the one previous to s0 ... */

    /*SYM        ID  A SP2 O  U */
    { CALL,      59, 0, 0, 0, 0 }, /* return */
    { RETURN,    49, 0, 0, 0, 0 }, /* return */

    { GOTO,      50, 0, 0, 0, 0 }, /* goto p0 */
    { IF,        51, 1, 0, 0, 0 }, /* if (s0) goto p0 */
    { UNLESS,    52, 1, 0, 0, 0 }, /* unless (s0) goto p0 */

    { MOVC,      24, 3, 0, 0, 0 }, /* getins(s0) */
    { FROW,      40, 2, 0, 0, 0 }, /* fieldrow(s0, s1) */
    { FVAL,      41, 2, 0, 0, 1 }, /* fieldval(s0) */
    { ARRL,      42, 2, 0, 0, 0 }, /* s0[s1] */
    { SIN,       43, 2, 0, 1, 1 }, /* sin(s0) */
    { COS,       44, 2, 0, 1, 1 }, /* cos(s0) */
    { ATAN,      45, 2, 0, 0, 0 }, /* atan(s1, s0) */

    { EVHA,      62, 5, 0, 0, 0 }, /* accept event */
    { EVHR,      63, 5, 0, 0, 0 }, /* reject event */
    { NTRY,      64, 2, 0, 0, 0 }, /* entry operation */
    { 0,          0, 0, 0, 0, 0 }
};

static const expr_t*
expr_get_by_symbol_from_table(
    const expr_t* table,
    int symbol)
{
    while (table->symbol) {
        if (table->symbol == symbol)
            return table;
        ++table;
    }

    return NULL;
}

const expr_t*
expr_get_by_symbol(
    unsigned int version,
    int symbol)
{
    const expr_t* ret = NULL;

    ret = expr_get_by_symbol_from_table(global_expressions, symbol);
    if (!ret && version == 1) ret = expr_get_by_symbol_from_table(c1_expressions, symbol);
    if (!ret && version == 2) ret = expr_get_by_symbol_from_table(c2_expressions, symbol);

    return ret;
}

static const expr_t*
expr_get_by_id_from_table(
    const expr_t* table,
    int id)
{
    while (table->symbol) {
        if (table->id == id)
            return table;
        ++table;
    }

    return NULL;
}

const expr_t*
expr_get_by_id(
    unsigned int version,
    int id)
{
    const expr_t* ret = NULL;

    ret = expr_get_by_id_from_table(global_expressions, id);
    if (!ret && version == 1) ret = expr_get_by_id_from_table(c1_expressions, id);
    if (!ret && version == 2) ret = expr_get_by_id_from_table(c2_expressions, id);

    return ret;
}
