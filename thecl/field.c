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
#include <string.h>
#include "field.h"

static const field_t
gool_fields[] = {
    { "self",          0 },
    { "parent",        1 },
    { "sibling",       2 },
    { "child",         3 },
    { "creator",       4 },
    { "player",        5 },
    { "collider",      6 },
    { "interrupter",   7 },
    { "x",             8 },
    { "y",             9 },
    { "z",            10 },
    { "rotx",         11 },
    { "roty",         12 },
    { "rotz",         13 },
    { "scalex",       14 },
    { "scaley",       15 },
    { "scalez",       16 },
    { "velx",         17 },
    { "vely",         18 },
    { "velz",         19 },
    { "trotx",        20 },
    { "troty",        21 },
    { "trotz",        22 },
    { "modea",        23 },
    { "modeb",        24 },
    { "modec",        25 },
    { "statusa",      26 },
    { "statusb",      27 },
    { "statusc",      28 },
    { "spawn"  ,      29 },
    { "id",           30 },
    { "sp",           31 },
    { "pc",           32 },
    { "fp",           33 },
    { "tpc",          34 },
    { "epc",          35 },
    { "hpc",          36 },
    { "misc",         37 },
    { "field_38",     38 },
    { "frametime",    39 },
    { "statetime",    40 },
    { "animlag",      41 },
    { "animseq",      42 },
    { "animframe",    43 },
    { "entity",       44 },
    { "pathprog",     45 },
    { "pathlen",      46 },
    { "groundy",      47 },
    { "stateflag",    48 },
    { "speed",        49 },
    { "mode",         50 },
    { "field_51",     51 },
    { "groundtime",   52 },
    { "groundvel",    53 },
    { "zindex",       54 },
    { "event",        55 },
    { "camzoom",      56 },
    { "yzapproach",   57 },
    { "density",      58 },
    { "field_59",     59 },
    { "field_60",     60 },
    { "field_61",     61 },
    { "field_62",     62 },
    { "field_63",     63 },
    { NULL, 0 }
};

const field_t*
field_get(
    char* name)
{
    const field_t* table = gool_fields;

    while (table->name != NULL) {
        if (!strcmp(table->name, name))
            return table;
        ++table;
    }

    return NULL;
}
