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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "program.h"
#include "thecl.h"
#include "util.h"
#include "mygetopt.h"

const char* gool_ename_charmap = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_!";
const int gool_null_eid = 0x6396347F;
const char* gool_null_ename = "NONE!";

parser_state_t* g_parser_state = NULL;

extern const thecl_module_t c1_gool;

bool g_was_error = false;

thecl_t*
thecl_new(
    void)
{
    thecl_t* ecl = malloc(sizeof(thecl_t));
    list_init(&ecl->subs);
    list_init(&ecl->states);
    list_init(&ecl->anims);
    list_init(&ecl->spawns);
    ecl->vars = malloc(0);
    ecl->consts = malloc(0);
    ecl->var_count = 0;
    ecl->const_count = 0;
    ecl->no_warn = false;
    ecl->eid = gool_null_eid;
    ecl->id = 0;
    ecl->type = 0;
    ecl->is_defined = 0;
    return ecl;
}

static void
thecl_free(
    thecl_t* ecl)
{
    free(ecl->consts);

    for (size_t v = 0; v < ecl->var_count; ++v)
        free(ecl->vars[v]);
    free(ecl->vars);

    thecl_sub_t* sub;
    list_for_each(&ecl->subs, sub) {
        free(sub->name);

        thecl_instr_t* instr;
        list_for_each(&sub->instrs, instr)
            thecl_instr_free(instr);
        list_free_nodes(&sub->instrs);

        for (size_t v = 0; v < sub->var_count; ++v)
            thecl_variable_free(sub->vars[v]);
        free(sub->vars);

        thecl_label_t* label;
        list_for_each(&sub->labels, label)
            free(label);
        list_free_nodes(&sub->labels);

        free(sub);
    }
    list_free_nodes(&ecl->subs);

    thecl_state_t* state;
    list_for_each(&ecl->states, state) {
        free(state->name);
    }
    list_free_nodes(&ecl->states);

    gool_anim_t* anim;
    list_for_each(&ecl->anims, anim) {
        free(anim->name);

        free(anim->anim);
    }
    list_free_nodes(&ecl->anims);

    free(ecl);
}

int gool_to_eid(
    char* ename)
{
    if (strlen(ename) != 5) {
        fprintf(stderr, "%s: ename is not 5 characters long: %s\n", argv0, ename);
        return gool_null_eid;
    }
    int i = 5;
    int eid = 1;
    while (i--) {
        int c = strchr_o(gool_ename_charmap, ename[i]);
        if (c == -(int)gool_ename_charmap) {
            fprintf(stderr, "%s: ename has invalid character '%c': %s\n", argv0, ename[i], ename);
            return gool_null_eid;
        }
        eid |= c << (1 + 6*(4-i));
    }
    return eid;
}

int gool_pool_force_get_index(
    thecl_t* ecl,
    uint32_t val)
{
    for (int i = 0; i < ecl->const_count; ++i) {
        if (ecl->consts[i] == val)
            return i;
    }
    ecl->consts = realloc(ecl->consts, (ecl->const_count + 1) * sizeof(uint32_t));
    ecl->consts[ecl->const_count] = val;
    return ecl->const_count++;
}

thecl_instr_t*
thecl_instr_new(void)
{
    thecl_instr_t* instr = calloc(1, sizeof(thecl_instr_t));
    instr->type = THECL_INSTR_INSTR;
    instr->flags = 0;
    list_init(&instr->params);
    return instr;
}

thecl_instr_t*
thecl_instr_label(unsigned int offset)
{
    thecl_instr_t* instr = thecl_instr_new();
    instr->type = THECL_INSTR_LABEL;
    instr->offset = offset;
    return instr;
}

void
thecl_instr_free(thecl_instr_t* instr)
{
    free(instr->string);

    thecl_param_t* param;
    list_for_each(&instr->params, param) {
        value_free(&param->value);
        free(param);
    }
    list_free_nodes(&instr->params);

    free(instr);
}

void
thecl_variable_free(thecl_variable_t* var) {
    free(var->name);
    free(var);
}

thecl_param_t*
param_new(
    int type)
{
    thecl_param_t* param = malloc(sizeof(thecl_param_t));
    param->type = type;
    param->value.type = type;
    param->is_expression_param = 0;
    param->object_link = -1;
    param->stack = 0;
    return param;
}

thecl_param_t*
param_copy(
    thecl_param_t* param)
{
    thecl_param_t* copy = malloc(sizeof(thecl_param_t));
    memcpy(copy, param, sizeof(thecl_param_t));
    /* Handle possible pointers in the value. */
    if (copy->value.type == 'z')
        copy->value.val.z = strdup(param->value.val.z);
    return copy;
}

void
param_free(
    thecl_param_t* param)
{
    value_free(&param->value);
    free(param);
}

int32_t
label_offset(
    thecl_sub_t* sub,
    const char* name)
{
    thecl_label_t* label;
    list_for_each(&sub->labels, label) {
        if (strcmp(label->name, name) == 0)
            return label->offset;
    }
    fprintf(stderr, "%s: label not found: %s\n", argv0, name);
    return 0;
}

void
expression_free(
    expression_t* expr)
{
    expression_t* child_expr;
    if (expr->type == EXPRESSION_OP || expr->type == EXPRESSION_TERNARY) {
        list_for_each(&expr->children, child_expr)
            expression_free(child_expr);
        list_free_nodes(&expr->children);
    }
    free(expr);
}

expr_macro_t*
macro_get(
    parser_state_t* state,
    const char* name
) {
    expr_macro_t* macro;
    list_for_each(&state->expr_macros, macro) {
        if (!strcmp(name, macro->name))
            return macro;
    }
    return NULL;
}

static void
free_globals(void)
{
}

static void
print_usage(void)
{
    printf("Usage: %s [-V] [[-c] VERSION]... [INPUT [OUTPUT]]\n"
           "Options:\n"
           "  -c  create ECL file\n"
           "  -V  display version information and exit\n"
           "VERSION can be:\n"
           "  1\n"
           /* NEWHU: */
           "Report bugs to <" PACKAGE_BUGREPORT ">.\n", argv0);
}

int
main(int argc, char* argv[])
{
    FILE* in = stdin;
    FILE* out = stdout;
    unsigned int version = 0;
    int mode = -1;
    const thecl_module_t* module = NULL;

    current_input = "(stdin)";
    current_output = "(stdout)";

    atexit(free_globals);

    argv0 = util_shortname(argv[0]);
    int opt;
    int ind=0;
    while(argv[util_optind]) {
        switch(opt = util_getopt(argc, argv, ":c:V")) {
        case 'c':
            if(mode != -1) {
                fprintf(stderr,"%s: More than one mode specified\n", argv0);
                print_usage();
                exit(1);
            }
            mode = opt;
            version = parse_version(util_optarg);
            break;
        default:
            util_getopt_default(&ind,argv,opt,print_usage);
        }
    }
    argc = ind;
    argv[argc] = NULL;

    switch (version) {
    case 1:
        module = &c1_gool;
        break;
    default:
        if (mode == 'c') {
            if (version == -1)
                fprintf(stderr, "%s: version must be specified\n", argv0);
            else
                fprintf(stderr, "%s: version %u is unsupported\n", argv0, version);
            exit(1);
        }
    }

    switch (mode)
    {
    case 'c': {
        if (0 < argc) {
            current_input = argv[0];
            in = fopen(argv[0], "rb");
            if (!in) {
                fprintf(stderr, "%s: couldn't open %s for reading: %s\n",
                    argv0, argv[0], strerror(errno));
                exit(1);
            }
            if (1 < argc) {
                current_output = argv[1];
                out = fopen(argv[1], "wb");
                if (!out) {
                    fprintf(stderr, "%s: couldn't open %s for writing: %s\n",
                        argv0, argv[1], strerror(errno));
                    fclose(in);
                    exit(1);
                }
            }
        }

        if (mode == 'c') {
#ifdef WIN32
            (void)_setmode(fileno(stdout), _O_BINARY);
#endif
            thecl_t* gool = module->parse(in, argv[0], version);
            if (gool) {
                if (gool->is_defined) {
                    module->compile(gool, out);
                }
                thecl_free(gool);
            }
        }
        fclose(in);
        fclose(out);

        if(g_was_error) {
          printf("%s: %s: there were errors.\n", argv0, argv[0]);
          exit(1);
        }
        exit(0);
    }
    default:
        print_usage();
        exit(1);
    }
}
