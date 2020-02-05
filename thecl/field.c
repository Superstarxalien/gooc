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
#include <string.h>
#include <stdio.h>
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
    { "eventreceived",55 },
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

static char gvar_name_buf[10];
static field_t gvar_buf = { gvar_name_buf, 0 };

static const field_t
c1_gool_globals[] = {
    { "LEVEL",         0 },

    { "FRUITCOUNTER",  6 },
    { "LIFECOUNTER",   7 },

    { "PAUSEMENU",    12 },

    { "DOCTOR",       16 },

    { "GAMEPROGRESS", 20 },

    { "CAMTRANSX",    37 },
    { "CAMTRANSY",    38 },
    { "CAMTRANSZ",    39 },
    { "CAMROTX",      40 },
    { "CAMROTY",      41 },
    { "CAMROTZ",      42 },
    { "FRAMETIME",    43 },

    { "BONUSROUND",   60 },

    { "BOXCOUNT",     62 },

    { "DEBUG",        68 },
    { "CHECKPOINTID", 69 },

    { "SPAWNTRANSX", 102 },
    { "SPAWNTRANSY", 103 },
    { "SPAWNTRANSZ", 104 },

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

static const field_t*
gvar_get_from_table(
    const field_t* table,
    char* name)
{
    while (table->name) {
        if (!strcmp(table->name, name))
            return table;
        ++table;
    }

    return NULL;
}

const field_t*
gvar_get(
    unsigned int version,
    char* name)
{
    const field_t* ret = NULL;

    switch (version) {
    case 1:
        ret = gvar_get_from_table(c1_gool_globals, name);
        break;
    }

    if (!ret) {
        int gvar_id;
        if (sscanf(name, "GVAR_%d", &gvar_id)) {
            snprintf(gvar_name_buf, 10, "GVAR_%d", gvar_id);
            gvar_buf.offset = gvar_id;
            return &gvar_buf;
        }
    }

    return ret;
}

static const field_t
c1_gool_events[] = {
    { "Event00",         0 },
    { "Event01",         1 },
    { "Event02",         2 },
    { "Event03",         3 },
    { "Event04",         4 },
    { "Event05",         5 },
    { "Event06",         6 },
    { "Event07",         7 },
    { "Event08",         8 },
    { "Event09",         9 },
    { "Event10",        10 },
    { "Event11",        11 },
    { "Event12",        12 },
    { "Event13",        13 },
    { "Event14",        14 },
    { "Event15",        15 },
    { "Event16",        16 },
    { "Event17",        17 },
    { "Event18",        18 },
    { "Event19",        19 },
    { "Event20",        20 },
    { "Event21",        21 },
    { "Event22",        22 },
    { "Event23",        23 },
    { "Event24",        24 },
    { "Event25",        25 },
    { "Event26",        26 },
    { "Event27",        27 },
    { "Event28",        28 },
    { "Event29",        29 },
    { "Event30",        30 },
    { "Event31",        31 },
    { "Event32",        32 },
    { "Event33",        33 },
    { "Event34",        34 },
    { "Event35",        35 },
    { "Event36",        36 },
    { "Event37",        37 },
    { "Event38",        38 },
    { "Event39",        39 },
    { "Event40",        40 },
    { "Event41",        41 },
    { "Event42",        42 },
    { "Event43",        43 },
    { "Event44",        44 },

    { NULL, 0 }
};

static const field_t*
event_get_from_table(
    const field_t* table,
    char* name)
{
    while (table->name) {
        if (!strcmp(table->name, name))
            return table;
        ++table;
    }

    return NULL;
}

const field_t*
event_get(
    unsigned int version,
    char* name)
{
    const field_t* ret = NULL;

    switch (version) {
    case 1:
        ret = event_get_from_table(c1_gool_events, name);
        break;
    }

    return ret;
}
