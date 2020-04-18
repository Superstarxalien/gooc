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

static char global_name_buf[10];
static field_t global_buf = { global_name_buf, 0 };

static char event_name_buf[8];
static field_t event_buf = { event_name_buf, 0 };

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
    { "eventaccepted",37 },
    { "v0",           38 },
    { "frametime",    39 },
    { "statetime",    40 },
    { "stalltime",    41 },
    { "health",       41 },
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
    { "vfx",          56 },
    { "yzapproach",   57 },
    { "density",      58 },
    { "field_59",     59 },
    { "voice",        59 },
    { "field_60",     60 },
    { "field_61",     61 },
    { "field_62",     62 },
    { "field_63",     63 },
    { "trans",         8 },
    { "rot",          11 },
    { "vel",          14 },
    { "scale",        17 },
    { "trot",         20 },
    { "vec",          23 },
    { "vtrans",        0 },
    { "vrot",          1 },
    { "vvel",          2 },
    { "vscale",        3 },
    { "vtrot",         4 },
    { "vvec",          5 },
    { "vecx",         23 },
    { "vecy",         24 },
    { "vecz",         25 },
    { NULL, 0 }
};

static const field_t
c1_globals[] = {
    { "LEVEL",             0 },

    { "RESPAWNCOUNT",      5 },
    { "FRUITCOUNTER",      6 },
    { "LIFECOUNTER",       7 },

    { "PAUSEMENU",        12 },
    { "LIFEICONTRANSX",   13 },

    { "DOCTOR",           16 },

    { "GAMEPROGRESS",     20 },

    { "LIFECOUNT",        24 },

    { "ZONEFLAG",         30 },
    { "STARTLIVES",       31 },

    { "SFXVOL",           34 },
    { "MUSVOL",           35 },

    { "CAMTRANSX",        37 },
    { "CAMTRANSY",        38 },
    { "CAMTRANSZ",        39 },
    { "CAMROTX",          40 },
    { "CAMROTY",          41 },
    { "CAMROTZ",          42 },
    { "FRAMETIME",        43 },

    { "LEVELCOUNT",       46 },

    { "PERCENTCOMPLETE",  58 },

    { "BONUSROUND",       60 },

    { "BOXCOUNT",         62 },
    { "GAMEITEMPOOL1",    63 },

    { "DEBUG",            68 },
    { "CHECKPOINTID",     69 },
    { "PREVBOXCOUNT",     70 },
    { "PREVLEVEL",        71 },
    { "GAMEITEMPOOL2",    72 },

    { "DEMOTEXTOBJ",      76 },

    { "GEMCOUNT",         97 },
    { "KEYCOUNT",         98 },

    { "SPAWNTRANSX",     102 },
    { "SPAWNTRANSY",     103 },
    { "SPAWNTRANSZ",     104 },

    { "DEATHCOUNT",      108 },

    { NULL, 0 }
};

static const field_t
c1_events[] = {
    { "EventJumpedOn",         0 },

    { "EventHit",              3 },
    { "EventAttacked",         4 },

    { "EventTriggered",        8 },

    { "EventHitInvincible",   10 },

    { "EventRespawn",         19 },

    { "EventWin",             22 },

    { "EventDespawn",         26 },

    { "EventFling",           29 },

    { "EventLevelEnd",        41 },

    { "EventPlayerDamage",    43 },

    { NULL, 0 }
};

static const field_t
c2_globals[] = {
    { "LEVEL",             0 },

    { "RESPAWNCOUNT",      5 },
    { "FRUITCOUNTER",      6 },
    { "LIFECOUNTER",       7 },

    { "PAUSEMENU",        12 },

    { "DOCTOR",           16 },

    { "GAMEPROGRESS",     20 },

    { "LIFECOUNT",        24 },
    { "HEALTH",           25 },

    { "BOXCOUNTER",       29 },
    { "ZONEFLAG",         30 },
    { "STARTLIVES",       31 },

    { "SFXVOL",           34 },
    { "MUSVOL",           35 },

    { "CAMTRANSX",        37 },
    { "CAMTRANSY",        38 },
    { "CAMTRANSZ",        39 },
    { "CAMROTX",          40 },
    { "CAMROTY",          41 },
    { "CAMROTZ",          42 },
    { "FRAMETIME",        43 },

    { "GEMPOOL1",         48 },
    { "GEMPOOL2",         49 },

    { "CRYSTALPOOL1",     56 },
    { "CRYSTALPOOL2",     57 },

    { "BOXCOUNT",         62 },

    { "DEBUG",            68 },
    { "CHECKPOINTID",     69 },
    { "PREVBOXCOUNT",     70 },
    { "PREVLEVEL",        71 },

    { "DEMOTEXTOBJ",      76 },

    { "GEMCOUNT",         97 },
    { "KEYCOUNT",         98 },

    { "SPAWNTRANSX",     102 },
    { "SPAWNTRANSY",     103 },
    { "SPAWNTRANSZ",     104 },

    { "DEATHCOUNT",      108 },

    { "LEVELBOXCOUNT",   148 },

    { "BOXBELOW",        160 },
    { "BOXSTACKOFFSET",  161 },
    { "BOXENTITYBELOW",  162 },

    { NULL, 0 }
};

static const field_t
c2_events[] = {
    { "EventJumpedOn",         0 },

    { "EventHit",              3 },
    { "EventAttacked",         4 },

    { "EventTriggered",        8 },

    { "EventHitInvincible",   10 },

    { "EventRespawn",         19 },

    { "EventWin",             22 },

    { "EventDespawn",         26 },

    { "EventFling",           29 },

    { "EventLevelEnd",        41 },

    { "EventPlayerDamage",    43 },

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
field_get_from_table(
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
global_get(
    unsigned int version,
    char* name)
{
    const field_t* ret = NULL;

    switch (version) {
    case 2: ret = field_get_from_table(c2_globals, name); break;
    case 1: ret = field_get_from_table(c1_globals, name); break;
    }

    if (!ret) {
        int global_id;
        if (sscanf(name, "GLOBAL_%d", &global_id)) {
            snprintf(global_name_buf, 10, "GLOBAL_%d", global_id);
            global_buf.offset = global_id;
            return &global_buf;
        }
    }

    return ret;
}

const field_t*
event_get(
    unsigned int version,
    char* name)
{
    const field_t* ret = NULL;

    switch (version) {
    case 2: ret = field_get_from_table(c2_events, name); break;
    case 1: ret = field_get_from_table(c1_events, name); break;
    }

    if (!ret) {
        int event_id;
        if (sscanf(name, "Event%02d", &event_id)) {
            snprintf(event_name_buf, 8, "Event%02d", event_id);
            event_buf.offset = event_id;
            return &event_buf;
        }
    }

    return ret;
}
