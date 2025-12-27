// main.c
// DS_bighw: Parse design_*.v netlists and greedily pair LUTs.
//
// Requirements implemented:
// - Parse design_*.v files in current directory.
// - Recognize only module instances whose cell/module name matches GTP_LUT<digits>
//   but exclude GTP_LUT6CARRY.
// - For each recognized LUT instance, collect input nets from ports .I<number>(net)
//   ignoring .Z (and other ports).
// - Greedily pair LUTs such that the combined set of unique input nets has size <= 6.
//   Each LUT used at most once.
// - Write output file design_x_syn.res per testcase in expected format:
//     <pair_count>\n
//     <inst1> <inst2>\n   for each pair
//     (If odd LUT remains unpaired, it is ignored; pair_count counts only pairs.)
// - Print per-testcase summary including run time.
//
// Notes/assumptions:
// - This is a lightweight parser intended for typical synthesized Verilog netlists:
//     GTP_LUT6 u1 ( .I0(n1), .I1(n2), .Z(nout) );
//   Instance name is taken as the identifier after the cell name.
// - Nets can be simple identifiers or escaped identifiers (\\name) and may include
//   bit selects like bus[3]. We capture text inside parentheses up to matching ')'
//   (no nested parentheses expected).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif

typedef struct {
    char *inst;      // instance name
    char **nets;     // unique input nets (strings)
    int net_count;
    int net_cap;
    int used;
} Lut;

static void *xmalloc(size_t n) {
    void *p = malloc(n);
    if (!p) { fprintf(stderr, "OOM\n"); exit(1); }
    return p;
}

static char *xstrdup(const char *s) {
    size_t n = strlen(s);
    char *p = (char*)xmalloc(n + 1);
    memcpy(p, s, n + 1);
    return p;
}

static void lut_add_net_unique(Lut *l, const char *net) {
    // trim spaces
    while (*net && isspace((unsigned char)*net)) net++;
    if (*net == '\0') return;

    // copy trimmed end
    const char *end = net + strlen(net);
    while (end > net && isspace((unsigned char)end[-1])) end--;
    if (end <= net) return;
    size_t len = (size_t)(end - net);

    // check existing
    for (int i = 0; i < l->net_count; i++) {
        if (strncmp(l->nets[i], net, len) == 0 && l->nets[i][len] == '\0') return;
    }

    if (l->net_count == l->net_cap) {
        l->net_cap = l->net_cap ? l->net_cap * 2 : 8;
        l->nets = (char**)realloc(l->nets, (size_t)l->net_cap * sizeof(char*));
        if (!l->nets) { fprintf(stderr, "OOM\n"); exit(1); }
    }

    char *s = (char*)xmalloc(len + 1);
    memcpy(s, net, len);
    s[len] = '\0';
    l->nets[l->net_count++] = s;
}

static int is_gtp_lut_cell(const char *cell) {
    // must be exactly GTP_LUT<digits>
    if (strcmp(cell, "GTP_LUT6CARRY") == 0) return 0;
    const char *p = cell;
    if (strncmp(p, "GTP_LUT", 7) != 0) return 0;
    p += 7;
    if (!isdigit((unsigned char)*p)) return 0;
    while (*p) {
        if (!isdigit((unsigned char)*p)) return 0;
        p++;
    }
    return 1;
}

static int is_ident_char(int c) {
    return isalnum((unsigned char)c) || c == '_' || c == '$';
}

static void skip_spaces(const char **pp) {
    const char *p = *pp;
    while (*p && isspace((unsigned char)*p)) p++;
    *pp = p;
}

static int starts_with(const char *p, const char *lit) {
    while (*lit) {
        if (*p++ != *lit++) return 0;
    }
    return 1;
}

static char *parse_identifier(const char **pp) {
    // Parses Verilog identifier or escaped identifier.
    // Escaped identifier: \\...<space>
    const char *p = *pp;
    skip_spaces(&p);

    if (*p == '\\') {
        const char *s = p;
        p++; // consume backslash
        while (*p && !isspace((unsigned char)*p)) p++;
        size_t len = (size_t)(p - s);
        char *out = (char*)xmalloc(len + 1);
        memcpy(out, s, len);
        out[len] = '\0';
        *pp = p;
        return out;
    }

    if (!is_ident_char((unsigned char)*p)) return NULL;
    const char *s = p;
    while (*p && is_ident_char((unsigned char)*p)) p++;
    // allow hierarchical names a/b? In netlists, instance names may be simple.
    // We'll stop at first non-ident.
    size_t len = (size_t)(p - s);
    char *out = (char*)xmalloc(len + 1);
    memcpy(out, s, len);
    out[len] = '\0';
    *pp = p;
    return out;
}

static char *read_entire_file(const char *path, long *out_len) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return NULL; }
    long n = ftell(f);
    if (n < 0) { fclose(f); return NULL; }
    rewind(f);
    char *buf = (char*)xmalloc((size_t)n + 1);
    size_t rd = fread(buf, 1, (size_t)n, f);
    fclose(f);
    buf[rd] = '\0';
    if (out_len) *out_len = (long)rd;
    return buf;
}

static void strip_line_comments(char *s) {
    // Removes // ... to end of line (in-place). Doesn’t handle strings; good enough.
    char *p = s;
    while (*p) {
        if (p[0] == '/' && p[1] == '/') {
            while (*p && *p != '\n') *p++ = ' ';
        } else {
            p++;
        }
    }
}

static void strip_block_comments(char *s) {
    char *p = s;
    while (*p) {
        if (p[0] == '/' && p[1] == '*') {
            *p++ = ' ';
            *p++ = ' ';
            while (*p) {
                if (p[0] == '*' && p[1] == '/') {
                    *p++ = ' ';
                    *p++ = ' ';
                    break;
                }
                *p++ = ' ';
            }
        } else {
            p++;
        }
    }
}

static int parse_luts_from_buffer(const char *buf_in, Lut **out_luts, int *out_count) {
    // Work on a modifiable copy to strip comments.
    char *buf = xstrdup(buf_in);
    strip_block_comments(buf);
    strip_line_comments(buf);

    const char *p = buf;
    Lut *luts = NULL;
    int n_luts = 0, cap_luts = 0;

    while (*p) {
        // parse potential cell name
        const char *save = p;
        char *cell = parse_identifier(&p);
        if (!cell) { p = save + 1; continue; }

        if (!is_gtp_lut_cell(cell)) {
            free(cell);
            continue;
        }

        // instance name
        char *inst = parse_identifier(&p);
        if (!inst) {
            free(cell);
            continue;
        }

        skip_spaces(&p);
        if (*p != '(') {
            free(cell);
            free(inst);
            continue;
        }

        // parse port connections until matching ")" then ";"
        p++; // consume '('
        Lut lut = {0};
        lut.inst = inst;
        lut.nets = NULL;
        lut.net_count = 0;
        lut.net_cap = 0;
        lut.used = 0;

        int depth = 1;
        while (*p && depth > 0) {
            skip_spaces(&p);
            if (*p == ')') { depth--; p++; break; }
            if (*p == '(') { depth++; p++; continue; }

            // expecting .PortName(net)
            if (*p != '.') {
                p++;
                continue;
            }
            p++; // '.'
            // port identifier
            char *port = parse_identifier(&p);
            if (!port) {
                continue;
            }
            skip_spaces(&p);
            if (*p != '(') {
                free(port);
                continue;
            }
            p++; // '('

            // capture net expression until ')'
            const char *net_start = p;
            while (*p && *p != ')') p++;
            const char *net_end = p;
            if (*p == ')') p++;

            // Decide whether to record
            // Only .I<number>(net)
            int record = 0;
            if (port[0] == 'I') {
                int ok = 1;
                for (int i = 1; port[i]; i++) {
                    if (!isdigit((unsigned char)port[i])) { ok = 0; break; }
                }
                if (ok && port[1] != '\0') record = 1; // must have digits
            }
            if (record) {
                // substring
                size_t len = (size_t)(net_end - net_start);
                char *tmp = (char*)xmalloc(len + 1);
                memcpy(tmp, net_start, len);
                tmp[len] = '\0';
                lut_add_net_unique(&lut, tmp);
                free(tmp);
            }

            free(port);
        }

        // advance to semicolon if present
        while (*p && *p != ';' && *p != '\n') p++;
        if (*p == ';') p++;

        free(cell);

        // store lut
        if (n_luts == cap_luts) {
            cap_luts = cap_luts ? cap_luts * 2 : 64;
            luts = (Lut*)realloc(luts, (size_t)cap_luts * sizeof(Lut));
            if (!luts) { fprintf(stderr, "OOM\n"); exit(1); }
        }
        luts[n_luts++] = lut;
    }

    free(buf);
    *out_luts = luts;
    *out_count = n_luts;
    return 1;
}

static int union_unique_count_le6(const Lut *a, const Lut *b) {
    int count = a->net_count;
    for (int j = 0; j < b->net_count; j++) {
        int found = 0;
        for (int i = 0; i < a->net_count; i++) {
            if (strcmp(a->nets[i], b->nets[j]) == 0) { found = 1; break; }
        }
        if (!found) {
            count++;
            if (count > 6) return 0;
        }
    }
    return count <= 6;
}

static void free_luts(Lut *luts, int n) {
    for (int i = 0; i < n; i++) {
        free(luts[i].inst);
        for (int k = 0; k < luts[i].net_count; k++) free(luts[i].nets[k]);
        free(luts[i].nets);
    }
    free(luts);
}

static int ends_with(const char *s, const char *suffix) {
    size_t n = strlen(s), m = strlen(suffix);
    if (m > n) return 0;
    return strcmp(s + (n - m), suffix) == 0;
}

static int match_design_v(const char *name, int *out_idx) {
    // match design_<digits>.v exactly
    if (!starts_with(name, "design_")) return 0;
    if (!ends_with(name, ".v")) return 0;
    const char *p = name + 7;
    if (!isdigit((unsigned char)*p)) return 0;
    int val = 0;
    while (*p && *p != '.') {
        if (!isdigit((unsigned char)*p)) return 0;
        val = val * 10 + (*p - '0');
        p++;
    }
    if (strcmp(p, ".v") != 0) return 0;
    if (out_idx) *out_idx = val;
    return 1;
}

static int compare_ints(const void *a, const void *b) {
    int ia = *(const int*)a, ib = *(const int*)b;
    return (ia > ib) - (ia < ib);
}

static void run_one(const char *infile, int idx) {
    clock_t t0 = clock();

    long len = 0;
    char *buf = read_entire_file(infile, &len);
    if (!buf) {
        fprintf(stderr, "Failed to read %s\n", infile);
        return;
    }

    Lut *luts = NULL;
    int n_luts = 0;
    parse_luts_from_buffer(buf, &luts, &n_luts);
    free(buf);

    // Greedy pairing
    int *pair_a = (int*)xmalloc((size_t)n_luts * sizeof(int));
    int *pair_b = (int*)xmalloc((size_t)n_luts * sizeof(int));
    int n_pairs = 0;

    for (int i = 0; i < n_luts; i++) {
        if (luts[i].used) continue;
        int best = -1;
        for (int j = i + 1; j < n_luts; j++) {
            if (luts[j].used) continue;
            if (union_unique_count_le6(&luts[i], &luts[j])) { best = j; break; }
        }
        if (best != -1) {
            luts[i].used = 1;
            luts[best].used = 1;
            pair_a[n_pairs] = i;
            pair_b[n_pairs] = best;
            n_pairs++;
        }
    }

    char outfile[256];
    snprintf(outfile, sizeof(outfile), "design_%d_syn.res", idx);
    FILE *out = fopen(outfile, "wb");
    if (!out) {
        fprintf(stderr, "Failed to write %s\n", outfile);
    } else {
        fprintf(out, "%d\n", n_pairs);
        for (int k = 0; k < n_pairs; k++) {
            fprintf(out, "%s %s\n", luts[pair_a[k]].inst, luts[pair_b[k]].inst);
        }
        fclose(out);
    }

    double secs = (double)(clock() - t0) / (double)CLOCKS_PER_SEC;
    printf("%s: LUTs=%d pairs=%d time=%.3f s -> %s\n", infile, n_luts, n_pairs, secs, outfile);

    free(pair_a);
    free(pair_b);
    free_luts(luts, n_luts);
}

int main(int argc, char **argv) {
    // If specific files are provided, run only those.
    // Otherwise, search for design_*.v by trying a reasonable range.

    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            int idx = -1;
            if (!match_design_v(argv[i], &idx)) {
                fprintf(stderr, "Skipping (not design_*.v): %s\n", argv[i]);
                continue;
            }
            run_one(argv[i], idx);
        }
        return 0;
    }

    // No directory listing allowed portably without dirent; use dirent if available.
    // We'll use dirent on POSIX; fallback to trying design_0..design_9999.

#if defined(__unix__) || defined(__APPLE__)
    #include <dirent.h>
    DIR *d = opendir(".");
    if (!d) {
        fprintf(stderr, "Cannot open current directory\n");
        return 1;
    }
    int idxs_cap = 64, idxs_n = 0;
    int *idxs = (int*)xmalloc((size_t)idxs_cap * sizeof(int));
    struct dirent *de;
    while ((de = readdir(d)) != NULL) {
        int idx;
        if (match_design_v(de->d_name, &idx)) {
            if (idxs_n == idxs_cap) {
                idxs_cap *= 2;
                idxs = (int*)realloc(idxs, (size_t)idxs_cap * sizeof(int));
                if (!idxs) { fprintf(stderr, "OOM\n"); exit(1); }
            }
            idxs[idxs_n++] = idx;
        }
    }
    closedir(d);

    qsort(idxs, (size_t)idxs_n, sizeof(int), compare_ints);
    for (int i = 0; i < idxs_n; i++) {
        char name[256];
        snprintf(name, sizeof(name), "design_%d.v", idxs[i]);
        run_one(name, idxs[i]);
    }
    free(idxs);
#else
    for (int idx = 0; idx <= 9999; idx++) {
        char name[256];
        snprintf(name, sizeof(name), "design_%d.v", idx);
        FILE *f = fopen(name, "rb");
        if (!f) continue;
        fclose(f);
        run_one(name, idx);
    }
#endif

    return 0;
}
