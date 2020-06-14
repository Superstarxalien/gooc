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
#ifndef C1_GOOL_H_
#define C1_GOOL_H_

#include <stdint.h>
#include "thecl.h"

typedef struct {
    uint16_t type; /* 1 */
    uint16_t frames;
    uint32_t eid;
} c1_anim_t;

typedef struct {
PACK_BEGIN
    uint32_t r:8;
    uint32_t g:8;
    uint32_t b:8;
    uint32_t cx:4;
    uint32_t unk1:1;
    uint32_t blend:2;
    uint32_t textured:1;
    uint32_t y:5;
    uint32_t unk2:1;
    uint32_t cy:7;
    uint32_t x:5;
    uint32_t segment:2;
    uint32_t color:2;
    uint32_t uv:10;
PACK_END
} PACK_ATTRIBUTE c1_tex_t;

typedef struct {
    uint16_t type; /* 2 */
    uint16_t count;
    uint32_t eid;
    c1_tex_t frames[];
} c1_sprite_t;

typedef struct {
    c1_tex_t tex;
    uint16_t w;
    uint16_t h;
} c1_char_t;

typedef struct {
    uint16_t type; /* 3 */
    uint16_t char_count;
    uint32_t eid;
    c1_char_t chars[];
} c1_font_t;

typedef struct {
    uint16_t type; /* 4 */
    uint16_t string_count;
    uint32_t unknown;
    uint32_t font;
    char strings[];
} c1_text_t;

typedef struct {
    c1_tex_t tex;
    int16_t x;
    int16_t y;
    int16_t w;
    int16_t h;
} c1_frag_t;

typedef struct {
    uint16_t type; /* 5 */
    uint16_t sprite_count;
    uint32_t eid;
    uint32_t frag_count;
    c1_frag_t frags[];
} c1_fraganim_t;

#endif
