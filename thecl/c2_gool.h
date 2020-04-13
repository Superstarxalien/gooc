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
#ifndef C2_GOOL_H_
#define C2_GOOL_H_

#include <stdint.h>
#include "c1_gool.h"
#include "thecl.h"

typedef struct {
    uint16_t type; /* 1 */
    uint16_t frames;
    uint32_t eid;
    uint32_t interp;
} c2_anim_t;

typedef struct {
PACK_BEGIN
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t unk1:1;
    uint8_t blend:1;
    uint8_t primtype:6;
    uint8_t u1;
    uint8_t v1;
    uint16_t cx:4;
    uint16_t unk2:2;
    uint16_t cy:7;
    uint16_t unused1:3;
    uint8_t u2;
    uint8_t v2;
    uint16_t segment:2;
    uint16_t unk3:3;
    uint16_t additive:1;
    uint16_t unk4:1;
    uint16_t color:2;
    uint16_t unk5:7;
    uint8_t u3;
    uint8_t v3;
    uint8_t u4;
    uint8_t v4;
PACK_END
} PACK_ATTRIBUTE c2_tex_t;

typedef struct {
    uint16_t type; /* 2 */
    uint16_t count;
    uint32_t eid;
    c2_tex_t frames[];
} c2_sprite_t;

typedef struct {
    c2_tex_t tex;
    uint16_t w;
    uint16_t h;
} c2_char_t;

typedef struct {
    uint16_t type; /* 3 */
    uint16_t char_count;
    uint32_t eid;
    c2_char_t chars[];
} c2_font_t;

typedef c1_text_t c2_text_t;

#endif
