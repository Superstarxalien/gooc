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
    { "self",            0 },
    { "parent",          1 },
    { "sibling",         2 },
    { "child",           3 },
    { "creator",         4 },
    { "player",          5 },
    { "collider",        6 },
    { "interrupter",     7 },
    { "x",               8 },
    { "y",               9 },
    { "z",              10 },
    { "rotx",           11 },
    { "roty",           12 },
    { "rotz",           13 },
    { "scalex",         14 },
    { "scaley",         15 },
    { "scalez",         16 },
    { "velx",           17 },
    { "vely",           18 },
    { "velz",           19 },
    { "trotx",          20 },
    { "troty",          21 },
    { "trotz",          22 },
    { "modea",          23 },
    { "modeb",          24 },
    { "modec",          25 },
    { "statusa",        26 },
    { "statusb",        27 },
    { "statusc",        28 },
    { "spawn",          29 },
    { "id",             30 },
    { "sp",             31 },
    { "pc",             32 },
    { "fp",             33 },
    { "tpc",            34 },
    { "epc",            35 },
    { "hpc",            36 },
    { "misc",           37 },
    { "eventaccepted",  37 },
    { "objectspawned",  37 },
    { "v0",             38 },
    { "var0",           38 },
    { "broadcastcaught",38 },
    { "frametime",      39 },
    { "statetime",      40 },
    { "stalltime",      41 },
    { "health",         41 },
    { "animseq",        42 },
    { "animframe",      43 },
    { "entity",         44 },
    { "pathprog",       45 },
    { "pathlen",        46 },
    { "groundy",        47 },
    { "stateflag",      48 },
    { "speed",          49 },
    { "invincible",     50 },
    { "invincibletime", 51 },
    { "groundtime",     52 },
    { "groundvel",      53 },
    { "zindex",         54 },
    { "eventreceived",  55 },
    { "vfx",            56 },
    { "camdist",        56 },
    { "yzapproach",     57 },
    { "density",        58 },
    { "voice",          59 },
    { "field_60",       60 },
    { "field_61",       61 },
    { "var1"    ,       61 },
    { "pathdist",       61 },
    { "field_62",       62 },
    { "field_63",       63 },
    { "trans",           8 },
    { "rot",            11 },
    { "scale",          14 },
    { "vel",            17 },
    { "trot",           20 },
    { "vec",            23 },
    { "vtrans",          0 },
    { "vrot",            1 },
    { "vscale",          2 },
    { "vvel",            3 },
    { "vtrot",           4 },
    { "vvec",            5 },
    { "vecx",           23 },
    { "vecy",           24 },
    { "vecz",           25 },
    { NULL, 0 }
};

static const field_t
c1_colors[] = {
    { "src11",     0 },
    { "src12",     1 },
    { "src13",     2 },
    { "src21",     3 },
    { "src22",     4 },
    { "src23",     5 },
    { "src31",     6 },
    { "src32",     7 },
    { "src33",     8 },
    { "amb1",      9 },
    { "amb2",     10 },
    { "amb3",     11 },
    { "colr1",    12 },
    { "colg1",    13 },
    { "colb1",    14 },
    { "colr2",    15 },
    { "colg2",    16 },
    { "colb2",    17 },
    { "colr3",    18 },
    { "colg3",    19 },
    { "colb3",    20 },
    { "colra",    21 },
    { "colga",    22 },
    { "colba",    23 },
    { "colr4",    21 },
    { "colg4",    22 },
    { "colb4",    23 },
    { NULL, 0 }
};

static const field_t
c2_colors[] = {
    { "moda",      0 },
    { "modb",      1 },
    { "modc",      2 },
    { "modd",      3 },
    { "colr",      4 },
    { "colg",      5 },
    { "colb",      6 },
    { "colr1",     4 },
    { "colg1",     5 },
    { "colb1",     6 },
    { "colr2",     7 },
    { "colg2",     8 },
    { "colb2",     9 },
    { "ldirx",     7 },
    { "ldiry",     8 },
    { "ldirz",     9 },
    { "colr3",    10 },
    { "colg3",    11 },
    { "colb3",    12 },
    { "colra",    13 },
    { "colga",    14 },
    { "colba",    15 },
    { "finalr",   13 },
    { "finalg",   14 },
    { "finalb",   15 },
    { "colr4",    13 },
    { "colg4",    14 },
    { "colb4",    15 },
    { NULL, 0 }
};

static const field_t
c1_globals[] = {
    { "LEVEL",               0 },
    { "GLOBALVAL",           1 },
    { "SHAKEY",              2 },
    { "GLOBALOBJ",           3 },
    { "GAMEFLAGS",           4 },
    { "RESPAWNCOUNT",        5 },
    { "FRUITDISPLAY",        6 },
    { "LIFEDISPLAY",         7 },

    { "PREVGAMEFLAGS",       9 },

    { "PAUSEMENU",          12 },
    { "LIFEICONTRANSX",     13 },
    { "PICKUPDISPLAY",      14 },
    { "GAMEDIR",            15 },
    { "DOCTOR",             16 },

    { "GAMEPROGRESS",       20 },

    { "LIFECOUNT",          24 },
    { "HEALTH",             25 },
    { "FRUITCOUNT",         26 },
    { "CORTEXCOUNT",        27 },
    { "BRIOCOUNT",          28 },
    { "TAWNACOUNT",         29 },
    { "ZONEFLAGS",          30 },
    { "STARTLIVES",         31 },

    { "MONOSOUND",          33 },
    { "SFXVOL",             34 },
    { "MUSVOL",             35 },

    { "CAMTRANSX",          37 },
    { "CAMTRANSY",          38 },
    { "CAMTRANSZ",          39 },
    { "CAMROTX",            40 },
    { "CAMROTY",            41 },
    { "CAMROTZ",            42 },
    { "FRAMETIME",          43 },

    { "LEVELCOUNT",         46 },

    { "PERCENTCOMPLETE",    58 },

    { "BONUSROUND",         60 },

    { "BOXCOUNT",           62 },
    { "ITEMPOOL1",          63 },

    { "GEMTIME",            65 },

    { "FIRSTZONE",          67 },
    { "DEBUG",              68 },
    { "CHECKPOINTID",       69 },
    { "PREVBOXCOUNT",       70 },
    { "PREVLEVEL",          71 },
    { "ITEMPOOL2",          72 },

    { "DEMOTEXTOBJ",        76 },

    { "GAMETICK",           79 },

    { "CARDBLOCKDATA00",    82 },
    { "CARDBLOCKDATA01",    83 },
    { "CARDBLOCKDATA02",    84 },
    { "CARDBLOCKDATA03",    85 },
    { "CARDBLOCKDATA04",    86 },
    { "CARDBLOCKDATA05",    87 },
    { "CARDBLOCKDATA06",    88 },
    { "CARDBLOCKDATA07",    89 },
    { "CARDBLOCKDATA08",    90 },
    { "CARDBLOCKDATA09",    91 },
    { "CARDBLOCKDATA10",    92 },
    { "CARDBLOCKDATA11",    93 },
    { "CARDBLOCKDATA12",    94 },
    { "CARDBLOCKDATA13",    95 },
    { "CARDBLOCKDATA14",    96 },
    { "GEMCOUNT",           97 },
    { "KEYCOUNT",           98 },
    { "SAVETYPE",           99 },
    { "SAVEDITEMPOOL1",    100 },
    { "SAVEDITEMPOOL2",    101 },
    { "SPAWNTRANSX",       102 },
    { "SPAWNTRANSY",       103 },
    { "SPAWNTRANSZ",       104 },

    { "FADECONTROL",       106 },
    { "FADEAMOUNT",        107 },
    { "DEATHCOUNT",        108 },

    { "AUTOPASSWORD",      110 },
    { "AUTOPASSWORDINPUT1",111 },
    { "AUTOPASSWORDINPUT2",112 },
    { "SAVEDLEVELCOUNT",   113 },
    { "DEMOID",            114 },
    { "OPTIONSCHANGED",    115 },

    /* NTSC-J only */

    { "DOCTORHELPCOUNT",   116 },
    { "CARDBIOSTEXTBUF",   117 },

    { NULL, 0 }
};

static const field_t
c1_events[] = {
    { "EventJumpedOn",           0 },

    { "EventHit",                3 },
    { "EventSpinHit",            4 },

    { "EventTriggered",          8 },
    { "EventFallKill",           9 },
    { "EventBoxStackBreak",      9 },
    { "EventHitInvincible",     10 },

    { "EventStatus",            15 },
    { "EventCombo",             16 },

    { "EventRespawn",           19 },
    { "EventEat",               20 },
    { "EventBounce",            21 },
    { "EventWarp",              22 },

    { "EventSquash",            25 },
    { "EventDespawn",           26 },

    { "EventBossWin",           29 },
    { "EventFling",             29 },
    { "EventExplode",           30 },
    { "EventBurn",              31 },

    { "EventDrown",             33 },

    { "EventShock",             35 },
    { "EventHitFence",          35 },

    { "EventBoulderSquash",     37 },

    { "EventLevelEnd",          41 },

    { "EventPlayerDamage",      43 },

    { NULL, 0 }
};

static const field_t
c2_globals[] = {
    { "LEVEL",               0 },

    { "SHAKEY",              2 },

    { "GAMEFLAGS",           4 },
    { "RESPAWNCOUNT",        5 },
    { "FRUITDISPLAY",        6 },
    { "LIFEDISPLAY",         7 },

    { "PREVGAMEFLAGS",       9 },

    { "PAUSEMENU",          12 },
    { "LIFEICONTRANSX",     13 },
    { "PICKUPDISPLAY",      14 },
    { "GAMEDIR",            15 },
    { "DOCTOR",             16 },

    { "GAMEPROGRESS",       20 },

    { "BONUSDEATHTIME",     22 },

    { "LIFECOUNT",          24 },
    { "HEALTH",             25 },
    { "FRUITCOUNT",         26 },

    { "BOXCOUNTER",         29 },
    { "ZONEFLAGS",          30 },
    { "STARTLIVES",         31 },

    { "MONOSOUND",          33 },
    { "SFXVOL",             34 },
    { "MUSVOL",             35 },
    { "CHECKPOINTCOUNT",    36 },
    { "CAMTRANSX",          37 },
    { "CAMTRANSY",          38 },
    { "CAMTRANSZ",          39 },
    { "CAMROTX",            40 },
    { "CAMROTY",            41 },
    { "CAMROTZ",            42 },
    { "FRAMETIME",          43 },

    { "SAVEDGEMPOOL1",      46 },
    { "SAVEDGEMPOOL2",      47 },
    { "GEMPOOL1",           48 },
    { "GEMPOOL2",           49 },

    { "CRYSTALPOOL1",       56 },
    { "CRYSTALPOOL2",       57 },

    { "BOXCOUNT",           62 },

    { "DEBUG",              68 },
    { "CHECKPOINTID",       69 },
    { "PREVBOXCOUNT",       70 },
    { "PREVLEVEL",          71 },

    { "DEMOTEXTOBJ",        76 },

    { "GAMETICK",           79 },

    { "SPAWNTRANSX",       102 },
    { "SPAWNTRANSY",       103 },
    { "SPAWNTRANSZ",       104 },

    { "FADECONTROL",       106 },
    { "FADEAMOUNT",        107 },
    { "DEATHCOUNT",        108 },

    { "ANALOGAMT",         124 },
    { "ANALOGDIR",         125 },

    { "BONUSSTATE",        132 },
    { "BONUSFRUITCOUNTER", 133 },
    { "BONUSLIFECOUNTER",  134 },
    { "BONUSBOXCOUNTER",   135 },

    { "SAVEDCRYSTALPOOL1", 141 },
    { "SAVEDCRYSTALPOOL2", 142 },

    { "SECTION",           145 },
    { "SECTIONDEATHCOUNT", 146 },

    { "LEVELBOXCOUNT",     148 },

    { NULL, 0 }
};

static const field_t
c2_events[] = {
    { "EventJumpedOn",           0 },

    { "EventHit",                3 },
    { "EventSpinHit",            4 },

    { "EventTriggered",          8 },
    { "EventFallKill",           9 },
    { "EventBoxStackBreak",      9 },
    { "EventHitInvincible",     10 },

    { "EventRespawn",           19 },

    { "EventBounce",            21 },
    { "EventWarp",              22 },

    { "EventDespawn",           26 },

    { "EventFling",             29 },
    { "EventExplode",           30 },
    { "EventBurn",              31 },

    { "EventLevelEnd",          41 },

    { "EventPlayerDamage",      43 },

    { "EventSlideHit",          45 },
    { "EventSlamHit",           46 },

    { "EventBearStart",         65 },

    { "EventBoxBreak",          72 },

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
color_get(
    unsigned int version,
    char* name)
{
    const field_t* ret = NULL;

    switch (version) {
    case 2: ret = field_get_from_table(c2_colors, name); break;
    case 1: ret = field_get_from_table(c1_colors, name); break;
    }

    return ret;
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
