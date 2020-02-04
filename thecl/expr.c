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
c1_expressions[] = {
    /* The program checks against the number of params, as well as the
     * requested stack depth, and does the replacements. */
    /* p0 is the first param, p1 the second ... */
    /* s0 is the previous instruction, s1 the one previous to s0 ... */

    /*SYM        ID     P  A    S    */
    { RETURN,  0x82, NULL, 0, 0,  NULL, }, /* return */

    { GOTO,    0x82,  "o", 0, 0,  "S" }, /* goto p0 */
    { UNLESS,  0x82,  "o", 1, 0,  "S" }, /* unless (s0) goto p0 */
    { IF,      0x82,  "o", 1, 0,  "S" }, /* if (s0) goto p0 */

    { GLOAD,     31,  "S", 0, 0, NULL }, /* p0 */
    { LOAD,      22,  "S", 0, 0, NULL }, /* p0 */
    { GASSIGN,   32,  "S", 1, 0,  "S" }, /* p0 = s0 */
    { ASSIGN,    17,  "S", 1, 0,  "S" }, /* p0 = s0 */

    { ADD,        0, NULL, 2, 0, "SS" }, /* s1 + s0 */
    { SUBTRACT,   1, NULL, 2, 0, "SS" }, /* s1 - s0 */
    { MULTIPLY,   2, NULL, 2, 0, "SS" }, /* s1 * s0 */
    { DIVIDE,     3, NULL, 2, 0, "SS" }, /* s1 / s0 */
    { MODULO,    13, NULL, 2, 0, "SS" }, /* s1 % s0 */
    { EQUAL,      4, NULL, 2, 0, "SS" }, /* s1 == s0 */
    { INEQUAL,    4, NULL, 2, 0, "SS" }, /* GOOL does not have this instruction, use NOT + CEQ instead /* s1 != s0 */
    { LT,         9, NULL, 2, 0, "SS" }, /* s1 < s0 */
    { LTEQ,      10, NULL, 2, 0, "SS" }, /* s1 <= s0 */
    { GT,        11, NULL, 2, 0, "SS" }, /* s1 > s0 */
    { GTEQ,      12, NULL, 2, 0, "SS" }, /* s1 >= s0 */
    { NOT,       18, NULL, 2, 0,  "S" }, /* !s0 */
    { OR,         6, NULL, 2, 0, "SS" }, /* s1 || s0 */
    { AND,        5, NULL, 2, 0, "SS" }, /* s1 && s0 */
    { XOR,       14, NULL, 2, 0, "SS" }, /* s1 ^ s0 */
    { B_OR,       8, NULL, 2, 0, "SS" }, /* s1 | s0 */
    { B_AND,      7, NULL, 2, 0, "SS" }, /* s1 & s0 */
    { SHIFT,     21, NULL, 2, 0, "SS" }, /* s1 << s0 */
    { TEST,      15, NULL, 2, 0, "SS" }, /* s1 has s0 */
    { SEEK,      34, NULL, 3, 1, "SSS" }, /* seek(s0, s1, s2) */
    { DEGSEEK,   37, NULL, 3, 1, "SSS" }, /* degseek(s0, s1, s2) */
    { 0,          0, NULL, 0, 0, NULL }
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

    if (!ret) ret = expr_get_by_symbol_from_table(c1_expressions, symbol);

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

    if (!ret) ret = expr_get_by_id_from_table(c1_expressions, id);

    return ret;
}
