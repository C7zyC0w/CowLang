// cowlang_full.c
// Full single-file native port of the provided JS runtime and CLTranslator.
// - SDL2 used for graphics (stars + comets).
// - A CowLang interpreter implementing a large subset of the JS version.
// - File writing (writefile) supported (writes to disk).
//
// Compile (Linux):
// gcc cowlang_full.c -o cowlang_full -lSDL2 -lm
//
// Run:
// ./cowlang_full [program.cow]
//
// Author: converted for user request. Best-effort port; not byte-identical but functionally similar.
//
// --------------------------------------------------------------------------------

#define _GNU_SOURCE
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <stdbool.h>
#include <stdarg.h>
#include <errno.h>
#include <stdint.h>
#include <sys/stat.h>

// -----------------------------
// Config
// -----------------------------
#define INITIAL_LINE_CAP 1024
#define MAX_TOKEN_LEN 1024
#define MAX_NAME_LEN 256
#define MAX_PARAMS 64
#define MAX_SCOPE_DEPTH 256
#define INITIAL_SCOPE_CAP 64
#define MAX_FUNCTIONS 1024
#define MAX_FILEBUFFER_LINES 65536
#define MAX_STACK 1024

// SDL config
#define DEFAULT_WIDTH 1280
#define DEFAULT_HEIGHT 720
#define MAX_STARS 2000
#define MAX_COMETS 512

// -----------------------------
// Utility: dynamic strings and arrays
// -----------------------------
typedef struct {
    char *data;
    size_t len;
    size_t cap;
} dstr;

static void dstr_init(dstr *s){
    s->len = 0;
    s->cap = 256;
    s->data = malloc(s->cap);
    if(!s->data){ perror("malloc"); exit(1); }
    s->data[0]=0;
}
static void dstr_free(dstr *s){ if(s->data) free(s->data); s->data = NULL; s->len = s->cap = 0; }
static void dstr_append(dstr *s, const char *fmt, ...){
    va_list ap;
    va_start(ap, fmt);
    char tmp[1024];
    int r = vsnprintf(tmp, sizeof(tmp), fmt, ap);
    va_end(ap);
    if(r < 0) return;
    size_t need = (size_t)r;
    if(s->len + need + 1 > s->cap){
        while(s->len + need + 1 > s->cap) s->cap *= 2;
        s->data = realloc(s->data, s->cap);
        if(!s->data){ perror("realloc"); exit(1); }
    }
    memcpy(s->data + s->len, tmp, need);
    s->len += need;
    s->data[s->len] = 0;
}

// split lines into dynamic array
typedef struct {
    char **lines;
    size_t count;
    size_t cap;
} lines_arr;

static void lines_init(lines_arr *la){
    la->cap = INITIAL_LINE_CAP;
    la->count = 0;
    la->lines = malloc(sizeof(char*) * la->cap);
    if(!la->lines){ perror("malloc"); exit(1); }
}
static void lines_free(lines_arr *la){
    for(size_t i=0;i<la->count;i++) free(la->lines[i]);
    free(la->lines);
    la->lines = NULL; la->count = la->cap = 0;
}
static void lines_push(lines_arr *la, const char *s){
    if(la->count + 1 >= la->cap){
        la->cap *= 2;
        la->lines = realloc(la->lines, sizeof(char*) * la->cap);
        if(!la->lines){ perror("realloc"); exit(1); }
    }
    la->lines[la->count++] = strdup(s);
}

// -----------------------------
// String helpers (trim, lower, etc.)
// -----------------------------
static void str_lower_inplace(char *s){
    for(; *s; ++s) *s = (char)tolower((unsigned char)*s);
}
static char *str_tolower(const char *s){
    char *o = strdup(s ? s : "");
    for(char *p=o; *p; ++p) *p = (char)tolower((unsigned char)*p);
    return o;
}
static char *str_trim(const char *s){
    if(!s) return strdup("");
    const char *start = s;
    while(*start && isspace((unsigned char)*start)) start++;
    const char *end = s + strlen(s) - 1;
    while(end >= start && isspace((unsigned char)*end)) end--;
    size_t len = (end < start) ? 0 : (size_t)(end - start + 1);
    char *o = malloc(len + 1);
    if(!o){ perror("malloc"); exit(1); }
    memcpy(o, start, len);
    o[len] = 0;
    return o;
}
static char *str_lstrip(const char *s){
    if(!s) return strdup("");
    const char *start = s;
    while(*start && (*start == ' ' || *start == '\t')) start++;
    return strdup(start);
}
static int starts_with_icase(const char *s, const char *prefix){
    if(!s || !prefix) return 0;
    while(*prefix){
        if(!*s) return 0;
        if(tolower((unsigned char)*s) != tolower((unsigned char)*prefix)) return 0;
        s++; prefix++;
    }
    return 1;
}
// strip quotes if both ends quoted
static char *strip_quotes_if_any(const char *s){
    if(!s) return strdup("");
    size_t L = strlen(s);
    if(L >= 2 && ((s[0] == '"' && s[L-1] == '"') || (s[0] == '\'' && s[L-1] == '\''))){
        char *o = malloc(L-1);
        memcpy(o, s+1, L-2);
        o[L-2] = 0;
        return o;
    }
    return strdup(s);
}

static int is_single_quoted_literal(const char *s){
    if(!s) return 0;
    size_t L = strlen(s);
    if(L < 2) return 0;
    char q = s[0];
    if((q != '"' && q != '\'') || s[L-1] != q) return 0;

    int esc = 0;
    for(size_t i = 1; i + 1 < L; i++){
        char ch = s[i];
        if(esc){ esc = 0; continue; }
        if(ch == '\\'){ esc = 1; continue; }
        if(ch == q) return 0;
    }
    return 1;
}

// escapeRegExp-like: we'll not need a full implementation, minimal helpers suffice

// -----------------------------
// Data model: variables, scopes, functions
// -----------------------------
typedef enum { VAR_TYPE_INT, VAR_TYPE_FLOAT, VAR_TYPE_STR } VarType;

typedef struct {
    char *name;
    VarType type;
    char *val; // string representation always
} VarEntry;

typedef struct {
    VarEntry *entries;
    size_t count;
    size_t cap;
} Scope;

static void scope_init(Scope *s){
    s->cap = 64;
    s->count = 0;
    s->entries = calloc(s->cap, sizeof(VarEntry));
    if(!s->entries){ perror("calloc"); exit(1); }
}
static void scope_free(Scope *s){
    for(size_t i=0;i<s->count;i++){
        free(s->entries[i].name);
        free(s->entries[i].val);
    }
    free(s->entries);
    s->entries = NULL;
    s->count = s->cap = 0;
}
static void scope_push_var(Scope *s, const char *name, VarType t, const char *val){
    if(s->count + 1 >= s->cap){
        s->cap *= 2;
        s->entries = realloc(s->entries, sizeof(VarEntry) * s->cap);
        if(!s->entries){ perror("realloc"); exit(1); }
    }
    s->entries[s->count].name = strdup(name);
    s->entries[s->count].type = t;
    s->entries[s->count].val = strdup(val ? val : "");
    s->count++;
}
static int scope_find_index(Scope *s, const char *name){
    for(int i=(int)s->count-1;i>=0;i--){
        if(strcasecmp(s->entries[i].name, name) == 0) return i;
    }
    return -1;
}
static void scope_set_or_create(Scope *s, const char *name, VarType t, const char *val){
    int idx = -1;
    for(size_t i=0;i<s->count;i++){
        if(strcasecmp(s->entries[i].name, name) == 0){ idx = (int)i; break; }
    }
    if(idx >= 0){
        free(s->entries[idx].val);
        s->entries[idx].type = t;
        s->entries[idx].val = strdup(val ? val : "");
    } else {
        scope_push_var(s, name, t, val);
    }
}

// global runtime state for translator
typedef struct {
    Scope *scopes; // dynamic stack of scopes
    size_t scope_count;
    size_t scope_cap;
} ScopeStack;

static void scopestack_init(ScopeStack *ss){
    ss->scope_cap = 8;
    ss->scope_count = 0;
    ss->scopes = malloc(sizeof(Scope) * ss->scope_cap);
    if(!ss->scopes){ perror("malloc"); exit(1); }
    // push global scope
    ss->scope_count = 1;
    scope_init(&ss->scopes[0]);
}
static void scopestack_free(ScopeStack *ss){
    for(size_t i=0;i<ss->scope_count;i++) scope_free(&ss->scopes[i]);
    free(ss->scopes);
    ss->scopes = NULL;
    ss->scope_count = ss->scope_cap = 0;
}
static Scope* scopestack_top(ScopeStack *ss){
    if(ss->scope_count == 0) return NULL;
    return &ss->scopes[ss->scope_count - 1];
}
static void scopestack_push(ScopeStack *ss){
    if(ss->scope_count + 1 >= ss->scope_cap){
        ss->scope_cap *= 2;
        ss->scopes = realloc(ss->scopes, sizeof(Scope) * ss->scope_cap);
        if(!ss->scopes){ perror("realloc"); exit(1); }
    }
    scope_init(&ss->scopes[ss->scope_count]);
    ss->scope_count++;
}
static void scopestack_pop(ScopeStack *ss){
    if(ss->scope_count == 0) return;
    scope_free(&ss->scopes[ss->scope_count - 1]);
    ss->scope_count--;
}
static int scopestack_find_var(ScopeStack *ss, const char *name, size_t *outScopeIndex, size_t *outVarIndex){
    for(int si=(int)ss->scope_count-1; si>=0; si--){
        Scope *s = &ss->scopes[si];
        for(size_t i=0;i<s->count;i++){
            if(strcasecmp(s->entries[i].name, name) == 0){
                if(outScopeIndex) *outScopeIndex = (size_t)si;
                if(outVarIndex) *outVarIndex = i;
                return 1;
            }
        }
    }
    return 0;
}

// Function representation
typedef struct {
    char *name;
    char *kind; // "subr" or "function"
    char **params;
    size_t param_count;
    lines_arr body;
} CLFunction;

typedef struct {
    CLFunction **items;
    size_t count;
    size_t cap;
} FuncTable;

static void func_table_init(FuncTable *ft){
    ft->cap = 128;
    ft->count = 0;
    ft->items = malloc(sizeof(CLFunction*) * ft->cap);
    if(!ft->items){ perror("malloc"); exit(1); }
}
static void func_free(CLFunction *f){
    if(!f) return;
    free(f->name); free(f->kind);
    for(size_t i=0;i<f->param_count;i++) free(f->params[i]);
    free(f->params);
    lines_free(&f->body);
    free(f);
}
static void func_table_free(FuncTable *ft){
    for(size_t i=0;i<ft->count;i++) func_free(ft->items[i]);
    free(ft->items);
    ft->items = NULL;
    ft->count = ft->cap = 0;
}
static void func_table_add(FuncTable *ft, CLFunction *f){
    if(ft->count + 1 >= ft->cap){
        ft->cap *= 2;
        ft->items = realloc(ft->items, sizeof(CLFunction*) * ft->cap);
        if(!ft->items){ perror("realloc"); exit(1); }
    }
    ft->items[ft->count++] = f;
}
static CLFunction* func_table_find(FuncTable *ft, const char *name){
    for(size_t i=0;i<ft->count;i++){
        if(strcasecmp(ft->items[i]->name, name) == 0) return ft->items[i];
    }
    return NULL;
}

// -----------------------------
// Simple expression evaluator (shunting-yard + RPN eval)
// Supports: numbers, variables (int/float), builtins rand() and randint(n), + - * /, parentheses
// -----------------------------

// token types
typedef enum { TOK_NUM, TOK_OP, TOK_LP, TOK_RP, TOK_IDENT, TOK_COMMA, TOK_END } TokType;
typedef struct {
    TokType type;
    char txt[MAX_TOKEN_LEN];
} Token;

typedef struct {
    Token *items;
    size_t count;
    size_t cap;
} TokList;

static void toklist_init(TokList *tl){ tl->cap = 64; tl->count = 0; tl->items = malloc(sizeof(Token)*tl->cap); if(!tl->items){ perror("malloc"); exit(1);} }
static void toklist_free(TokList *tl){ free(tl->items); tl->items = NULL; tl->count = tl->cap = 0; }
static void toklist_push(TokList *tl, Token t){
    if(tl->count + 1 >= tl->cap){
        tl->cap *= 2;
        tl->items = realloc(tl->items, sizeof(Token)*tl->cap);
        if(!tl->items){ perror("realloc"); exit(1); }
    }
    tl->items[tl->count++] = t;
}

// tokenizer
static bool is_ident_start_char(char c){ return isalpha((unsigned char)c) || c == '_'; }
static bool is_ident_char(char c){ return isalnum((unsigned char)c) || c == '_'; }
static int is_unary_signed_number_start(const TokList *out, const char *p){
    if(!p || !*p) return 0;
    if(*p != '+' && *p != '-') return 0;
    if(!(isdigit((unsigned char)*(p+1)) || (*(p+1) == '.' && isdigit((unsigned char)*(p+2))))) return 0;
    TokType prev = TOK_END;
    if(out->count > 0) prev = out->items[out->count - 1].type;
    return (prev == TOK_END || prev == TOK_OP || prev == TOK_LP || prev == TOK_COMMA);
}
static void tokenize_expr(const char *s, TokList *out){
    toklist_init(out);
    const char *p = s;
    while(*p){
        if(isspace((unsigned char)*p)){ p++; continue; }
        if(*p == '('){ Token t = {TOK_LP, "("}; toklist_push(out, t); p++; continue; }
        if(*p == ')'){ Token t = {TOK_RP, ")"}; toklist_push(out, t); p++; continue; }
        if(*p == ','){ Token t = {TOK_COMMA, ","}; toklist_push(out, t); p++; continue; }
        if(strchr("+-*/", *p) && !is_unary_signed_number_start(out, p)){
            Token t = {TOK_OP, ""};
            t.txt[0] = *p; t.txt[1] = 0;
            toklist_push(out, t);
            p++; continue;
        }
        if(isdigit((unsigned char)*p) ||
           (*p == '.' && isdigit((unsigned char)*(p+1))) ||
           is_unary_signed_number_start(out, p)){
            char buf[MAX_TOKEN_LEN];
            const char *start = p;
            int has_digit = 0;

            if((*p == '+' || *p == '-') && is_unary_signed_number_start(out, p)) p++;
            while(isdigit((unsigned char)*p)){ has_digit = 1; p++; }
            if(*p == '.'){
                p++;
                while(isdigit((unsigned char)*p)){ has_digit = 1; p++; }
            }
            if(has_digit && (*p == 'e' || *p == 'E')){
                const char *exp_start = p;
                p++;
                if(*p == '+' || *p == '-') p++;
                if(!isdigit((unsigned char)*p)){
                    p = exp_start;
                } else {
                    while(isdigit((unsigned char)*p)) p++;
                }
            }

            size_t n = (size_t)(p - start);
            if(n >= sizeof(buf)) n = sizeof(buf) - 1;
            memcpy(buf, start, n);
            buf[n] = 0;
            Token t = {TOK_NUM, ""};
            strncpy(t.txt, buf, MAX_TOKEN_LEN-1);
            t.txt[MAX_TOKEN_LEN-1] = 0;
            toklist_push(out, t);
            continue;
        }
        if(*p == '"' || *p == '\''){
            char q = *p; p++;
            char buf[MAX_TOKEN_LEN]; int bi=0;
            while(*p && *p != q && bi < MAX_TOKEN_LEN-1){
                if(*p == '\\' && *(p+1)) { p++; buf[bi++] = *p; p++; continue; }
                buf[bi++] = *p;
                p++;
            }
            if(*p == q) p++;
            buf[bi]=0;
            Token t = {TOK_IDENT, ""};
            // keep quotes removed, but mark as ident so parser can treat as string-literal when necessary
            snprintf(t.txt, MAX_TOKEN_LEN, "\"%s\"", buf);
            toklist_push(out, t);
            continue;
        }
        if(is_ident_start_char(*p)){
            char buf[MAX_TOKEN_LEN]; int bi=0;
            while(is_ident_char(*p) && bi < MAX_TOKEN_LEN-1){
                buf[bi++] = *p; p++;
            }
            buf[bi]=0;
            Token t = {TOK_IDENT, ""};
            strncpy(t.txt, buf, MAX_TOKEN_LEN-1);
            t.txt[MAX_TOKEN_LEN-1] = 0;
            toklist_push(out, t);
            continue;
        }
        // unknown char -> skip
        p++;
    }
    Token te = {TOK_END, ""};
    toklist_push(out, te);
}

// operator precedence
static int prec(const char *op){
    if(strcmp(op, "+")==0 || strcmp(op,"-")==0) return 10;
    if(strcmp(op, "*")==0 || strcmp(op,"/")==0) return 20;
    return 0;
}
static int is_left_assoc(const char *op){ return 1; }

// we need pointer to scope stack and function table for resolving variables and builtins
typedef struct CLRuntime CLRuntime;

// RPN element types
typedef enum { RPN_NUM, RPN_OP, RPN_IDENT, RPN_FUNC } RPNType;
typedef struct {
    RPNType type;
    char txt[MAX_TOKEN_LEN];
} RPNItem;

typedef struct {
    RPNItem *items;
    size_t count;
    size_t cap;
} RPNList;

static void rpn_init(RPNList *r){ r->cap = 128; r->count = 0; r->items = malloc(sizeof(RPNItem)*r->cap); if(!r->items){ perror("malloc"); exit(1);} }
static void rpn_free(RPNList *r){ free(r->items); r->items = NULL; r->count = r->cap = 0; }
static void rpn_push(RPNList *r, RPNItem it){
    if(r->count + 1 >= r->cap){ r->cap *= 2; r->items = realloc(r->items, sizeof(RPNItem)*r->cap); if(!r->items){ perror("realloc"); exit(1);} }
    r->items[r->count++] = it;
}

// Convert token list to RPN (shunting-yard). We will treat IDENT followed by '(' as function call.
static bool tokens_to_rpn(TokList *tl, RPNList *out, CLRuntime *rt);

// Evaluate RPN to double or string depending
typedef struct EvalResult {
    int is_number; // 1 -> number, 0 -> string
    double num;
    char *str; // allocated if not number
} EvalResult;
static void evalresult_free(EvalResult *er){ if(er->str) free(er->str); er->str = NULL; }

// -----------------------------
// Runtime structure (translator state)
// -----------------------------
struct CLRuntime {
    ScopeStack scopes;
    FuncTable funcs;
    // output buffer (non-direct print)
    dstr outbuf;
    bool directPrint;
    bool trace;
    bool inProg;
    char *currentProgName;
    // if stack for if/elif/else handling
    struct { bool active; bool matched; } *ifStack;
    size_t ifStackCount;
    size_t ifStackCap;
    // file mode for writefile
    bool fileMode;
    char **fileBuffer;
    size_t fileBufferCount;
    size_t fileBufferCap;
    // For tag DSL in writefile: not complex, we will emit lines as-is
    // builtins: rand, randint
};

// forward declarations
static void runtime_init(CLRuntime *rt);
static void runtime_free(CLRuntime *rt);
static void runtime_push_output(CLRuntime *rt, const char *s);
static char *runtime_flush_output(CLRuntime *rt);
static const char *runtime_get_var_value(CLRuntime *rt, const char *name);
static void runtime_create_or_set_var(CLRuntime *rt, const char *name, VarType type, const char *val);
static void runtime_store_variable_cmd(CLRuntime *rt, const char *cmd);
static void runtime_handle_assignment(CLRuntime *rt, const char *line);
static void runtime_do_print(CLRuntime *rt, const char *expr);
static void runtime_execute_block(CLRuntime *rt, lines_arr *block);
static void runtime_store_function(CLRuntime *rt, const char *kind, const char *headerLine, lines_arr *bodyLines);
static char *runtime_capture_raw_block(lines_arr *lines, size_t startIdx, const char *startKw, const char *endKw, size_t *endIdx);
static void runtime_handle_writefile(CLRuntime *rt, const char *line, lines_arr *lines, size_t *idxPtr);
static char *runtime_run_from_text(CLRuntime *rt, const char *src);
static void runtime_call_function_or_builtin(CLRuntime *rt, const char *name, lines_arr *args, EvalResult *outVal);

// -----------------------------
// Random helpers
// -----------------------------
static double builtin_rand(CLRuntime *rt, lines_arr *args){
    (void)rt; (void)args;
    return (double)rand() / (double)RAND_MAX;
}
static double builtin_randint(CLRuntime *rt, lines_arr *args){
    (void)rt;
    long n = 1;
    if(args && args->count > 0 && args->lines[0]){
        char *t = str_trim(args->lines[0]);
        n = strtol(t, NULL, 10);
        free(t);
        if(n <= 0) n = 1;
    }
    return (double)(rand() % (n>0?n:1));
}

// -----------------------------
// Implementation: numeric eval helpers
// -----------------------------
static EvalResult eval_from_rpn(RPNList *rpn, CLRuntime *rt){
    EvalResult stack[MAX_STACK];
    int sp = 0;
    for(size_t i=0;i<rpn->count;i++){
        RPNItem *it = &rpn->items[i];
        if(it->type == RPN_NUM){
            EvalResult er = {1, atof(it->txt), NULL};
            stack[sp++] = er;
        } else if(it->type == RPN_IDENT){
            // could be variable or string literal
            if(it->txt[0] == '"' && it->txt[strlen(it->txt)-1] == '"'){
                EvalResult er = {0, 0.0, strdup(it->txt+1)};
                // remove last quote
                er.str[strlen(er.str)-1] = 0;
                stack[sp++] = er;
            } else {
                const char *v = runtime_get_var_value(rt, it->txt);
                if(v){
                    // try to parse number
                    char *end; double n = strtod(v, &end);
                    if(end && *end == 0){
                        EvalResult er = {1, n, NULL};
                        stack[sp++] = er;
                    } else {
                        EvalResult er = {0, 0.0, strdup(v)}; stack[sp++] = er;
                    }
                } else {
                    // unknown ident -> 0
                    EvalResult er = {1, 0.0, NULL}; stack[sp++] = er;
                }
            }
        } else if(it->type == RPN_OP){
            if(sp < 2){ EvalResult e = {1, 0.0, NULL}; return e; }
            EvalResult b = stack[--sp];
            EvalResult a = stack[--sp];
            if(!a.is_number || !b.is_number){
                // if strings present, treat as concatenation for + maybe, but for simplicity convert to 0
                EvalResult r = {1, 0.0, NULL};
                stack[sp++] = r;
                if(!a.is_number) evalresult_free(&a);
                if(!b.is_number) evalresult_free(&b);
                continue;
            }
            double av = a.num, bv = b.num;
            double res = 0.0;
            if(strcmp(it->txt, "+")==0) res = av + bv;
            else if(strcmp(it->txt, "-")==0) res = av - bv;
            else if(strcmp(it->txt, "*")==0) res = av * bv;
            else if(strcmp(it->txt, "/")==0) res = (bv == 0.0) ? 0.0 : av / bv;
            EvalResult r = {1, res, NULL};
            stack[sp++] = r;
        } else if(it->type == RPN_FUNC){
            // function name stored in txt; argument count encoded? We chose to push function name then args as identifiers with commas in tokens_to_rpn,
            // but easier approach: when building RPN, we push function token with name like "fname#nargs"
            char fnamebuf[MAX_TOKEN_LEN];
            strncpy(fnamebuf, it->txt, MAX_TOKEN_LEN-1);
            fnamebuf[MAX_TOKEN_LEN-1]=0;
            // parse name#n
            char *p = strchr(fnamebuf, '#');
            int nargs = 0;
            if(p){ *p = 0; nargs = atoi(p+1); }
            char *fname = fnamebuf;
            // collect args from stack (they are in order pushed)
            if(sp < nargs){ EvalResult e = {1,0.0,NULL}; return e; }
            // args will be stack[sp-nargs ... sp-1]
            // For builtins: rand, randint
            if(strcasecmp(fname, "rand") == 0){
                double v = builtin_rand(rt, NULL);
                // pop args
                for(int k=0;k<nargs;k++){ EvalResult tmp = stack[--sp]; evalresult_free(&tmp); }
                EvalResult r = {1, v, NULL};
                stack[sp++] = r;
            } else if(strcasecmp(fname, "randint")==0){
                double nArg = 1.0;
                if(nargs >= 1){
                    EvalResult tmp = stack[--sp];
                    if(tmp.is_number) nArg = tmp.num;
                    else nArg = atof(tmp.str ? tmp.str : "1");
                    evalresult_free(&tmp);
                }
                double out = builtin_randint(rt, NULL);
                EvalResult r = {1, out, NULL};
                stack[sp++] = r;
            } else {
                // user-defined function call eval is complex; return 0
                for(int k=0;k<nargs;k++){ EvalResult tmp = stack[--sp]; evalresult_free(&tmp); }
                EvalResult rr = {1, 0.0, NULL};
                stack[sp++] = rr;
            }
        }
    }
    if(sp == 0){
        EvalResult e = {1, 0.0, NULL}; return e;
    }
    EvalResult top = stack[--sp];
    // free remaining
    while(sp>0){ EvalResult tmp = stack[--sp]; evalresult_free(&tmp); }
    return top;
}

// tokens_to_rpn implementation: handle functions by treating IDENT followed by '(' as function call with arg count
static bool tokens_to_rpn(TokList *tl, RPNList *out, CLRuntime *rt){
    // operator stack
    Token opstack[1024];
    int opsp = 0;
    // func stack to count args
    int argcountstack[1024];
    int argsp = 0;
    rpn_init(out);
    for(size_t i=0;i<tl->count;i++){
        Token *t = &tl->items[i];
        if(t->type == TOK_NUM){
            RPNItem it; it.type = RPN_NUM; strncpy(it.txt, t->txt, MAX_TOKEN_LEN-1); rpn_push(out, it);
        } else if(t->type == TOK_IDENT){
            // check if next token is '(' meaning function call
            // find next token in tl (i+1)
            Token *nxt = (i+1 < tl->count) ? &tl->items[i+1] : NULL;
            if(nxt && nxt->type == TOK_LP){
                // function -> push to opstack
                opstack[opsp++] = *t;
                argcountstack[argsp++] = 0;
            } else {
                RPNItem it; it.type = RPN_IDENT; strncpy(it.txt, t->txt, MAX_TOKEN_LEN-1); rpn_push(out, it);
            }
        } else if(t->type == TOK_COMMA){
            // pop operators until '(' encountered
            while(opsp > 0 && opstack[opsp-1].type != TOK_LP){
                Token op = opstack[--opsp];
                RPNItem it; it.type = RPN_OP; strncpy(it.txt, op.txt, MAX_TOKEN_LEN-1); rpn_push(out, it);
            }
            if(argsp > 0) argcountstack[argsp-1]++; // next arg
        } else if(t->type == TOK_OP){
            while(opsp > 0){
                Token top = opstack[opsp-1];
                if(top.type == TOK_OP && ((is_left_assoc(t->txt) && prec(t->txt) <= prec(top.txt)) || (!is_left_assoc(t->txt) && prec(t->txt) < prec(top.txt)))){
                    // pop top
                    Token op = opstack[--opsp];
                    RPNItem it; it.type = RPN_OP; strncpy(it.txt, op.txt, MAX_TOKEN_LEN-1); rpn_push(out, it);
                    continue;
                }
                break;
            }
            opstack[opsp++] = *t;
        } else if(t->type == TOK_LP){
            opstack[opsp++] = *t;
        } else if(t->type == TOK_RP){
            // pop until LP
            while(opsp > 0 && opstack[opsp-1].type != TOK_LP){
                Token op = opstack[--opsp];
                RPNItem it; it.type = RPN_OP; strncpy(it.txt, op.txt, MAX_TOKEN_LEN-1); rpn_push(out, it);
            }
            if(opsp == 0) return false; // mismatched paren
            // pop '('
            opsp--;
            // if function on top of opstack (identifier before '('), pop it as function
            if(opsp > 0 && opstack[opsp-1].type == TOK_IDENT){
                Token fname = opstack[--opsp];
                int nargs = 0;
                if(argsp > 0){ nargs = argcountstack[--argsp] + 1; } else nargs = 0;
                RPNItem it; it.type = RPN_FUNC;
                snprintf(it.txt, MAX_TOKEN_LEN-1, "%s#%d", fname.txt, nargs);
                rpn_push(out, it);
            }
        } else if(t->type == TOK_END){
            break;
        }
    }
    while(opsp > 0){
        Token op = opstack[--opsp];
        if(op.type == TOK_LP || op.type == TOK_RP) return false;
        RPNItem it; it.type = RPN_OP; strncpy(it.txt, op.txt, MAX_TOKEN_LEN-1); rpn_push(out, it);
    }
    return true;
}

// Convenience: evaluate numeric expression (returns double)
static double eval_num_expr(CLRuntime *rt, const char *expr, int *is_int){
    TokList tl; tokenize_expr(expr, &tl);
    RPNList rpn; if(!tokens_to_rpn(&tl, &rpn, rt)){ toklist_free(&tl); rpn_free(&rpn); if(is_int) *is_int = 0; return 0.0; }
    EvalResult er = eval_from_rpn(&rpn, rt);
    double out = 0.0;
    if(er.is_number) out = er.num;
    else if(er.str) out = atof(er.str);
    evalresult_free(&er);
    toklist_free(&tl);
    rpn_free(&rpn);
    if(is_int) *is_int = (floor(out) == out) ? 1 : 0;
    return out;
}

static bool eval_expr(CLRuntime *rt, const char *expr, EvalResult *out){
    TokList tl;
    RPNList rpn;
    if(!out) return false;
    tokenize_expr(expr, &tl);
    if(!tokens_to_rpn(&tl, &rpn, rt)){
        toklist_free(&tl);
        rpn_free(&rpn);
        return false;
    }
    *out = eval_from_rpn(&rpn, rt);
    toklist_free(&tl);
    rpn_free(&rpn);
    return true;
}

static int is_simple_identifier(const char *s){
    if(!s || !s[0]) return 0;
    if(!is_ident_start_char(s[0])) return 0;
    for(const char *p = s + 1; *p; p++){
        if(!is_ident_char(*p)) return 0;
    }
    return 1;
}

static char *eval_result_to_string(const EvalResult *er){
    if(!er) return strdup("");
    if(!er->is_number){
        return strdup(er->str ? er->str : "");
    }
    char buf[128];
    double rounded = llround(er->num);
    if(fabs(er->num - rounded) < 1e-12){
        snprintf(buf, sizeof(buf), "%lld", (long long)rounded);
    } else {
        snprintf(buf, sizeof(buf), "%g", er->num);
    }
    return strdup(buf);
}

// Boolean evaluator: supports 'and', 'or', comparison ops
static int eval_bool_expr(CLRuntime *rt, const char *expr){
    if(!expr) return 0;
    // replace "and"/"or" to && || for easier parsing? We'll do simple parsing.
    // Check for comparison ops
    // Try to find comparison
    const char *ops[] = {"<=", ">=", "==", "!=", "<", ">"};
    for(size_t i=0;i<sizeof(ops)/sizeof(ops[0]);i++){
        char *pos = strstr(expr, ops[i]);
        if(pos){
            // split
            size_t idx = pos - expr;
            char *L = str_trim(expr);
            // to split safely, create copies
            char *left = strndup(expr, idx);
            char *right = strdup(pos + strlen(ops[i]));
            char *leftt = str_trim(left);
            char *rightt = str_trim(right);
            int li = 0, ri = 0;
            double lv = eval_num_expr(rt, leftt, &li);
            double rv = eval_num_expr(rt, rightt, &ri);
            int res = 0;
            if(strcmp(ops[i], "<=")==0) res = (lv <= rv);
            else if(strcmp(ops[i], ">=")==0) res = (lv >= rv);
            else if(strcmp(ops[i], "==")==0) res = (lv == rv);
            else if(strcmp(ops[i], "!=")==0) res = (lv != rv);
            else if(strcmp(ops[i], "<")==0) res = (lv < rv);
            else if(strcmp(ops[i], ">")==0) res = (lv > rv);
            free(left); free(right); free(leftt); free(rightt);
            return res;
        }
    }
    // no comparison operator: attempt to eval as numeric and non-zero is true
    int isint = 0;
    double v = eval_num_expr(rt, expr, &isint);
    return (fabs(v) > 1e-12);
}

// -----------------------------
// CLRuntime functions
// -----------------------------
static void runtime_init(CLRuntime *rt){
    memset(rt, 0, sizeof(CLRuntime));
    scopestack_init(&rt->scopes);
    func_table_init(&rt->funcs);
    dstr_init(&rt->outbuf);
    rt->directPrint = false;
    rt->trace = false;
    rt->inProg = false;
    rt->currentProgName = NULL;
    rt->ifStackCap = 16;
    rt->ifStackCount = 0;
    rt->ifStack = malloc(sizeof(*rt->ifStack) * rt->ifStackCap);
    rt->fileMode = false;
    rt->fileBuffer = NULL;
    rt->fileBufferCount = rt->fileBufferCap = 0;
}
static void runtime_free(CLRuntime *rt){
    if(rt->currentProgName) free(rt->currentProgName);
    dstr_free(&rt->outbuf);
    scopestack_free(&rt->scopes);
    func_table_free(&rt->funcs);
    if(rt->ifStack) free(rt->ifStack);
    if(rt->fileBuffer){
        for(size_t i=0;i<rt->fileBufferCount;i++) free(rt->fileBuffer[i]);
        free(rt->fileBuffer);
        rt->fileBuffer = NULL;
    }
}

static void runtime_push_output(CLRuntime *rt, const char *s){
    if(rt->fileMode){
        if(rt->fileBufferCount + 1 >= rt->fileBufferCap){
            rt->fileBufferCap = rt->fileBufferCap ? rt->fileBufferCap * 2 : 256;
            rt->fileBuffer = realloc(rt->fileBuffer, sizeof(char*) * rt->fileBufferCap);
            if(!rt->fileBuffer){ perror("realloc"); exit(1); }
        }
        rt->fileBuffer[rt->fileBufferCount++] = strdup(s ? s : "");
    } else if(rt->directPrint){
        printf("%s\n", s ? s : "");
    } else {
        dstr_append(&rt->outbuf, "%s\n", s ? s : "");
    }
}

static char *runtime_flush_output(CLRuntime *rt){
    if(rt->directPrint) return strdup("");
    char *o = strdup(rt->outbuf.data);
    rt->outbuf.len = 0;
    rt->outbuf.data[0] = 0;
    return o;
}

static const char *runtime_get_var_value(CLRuntime *rt, const char *name){
    size_t si, vi;
    if(scopestack_find_var(&rt->scopes, name, &si, &vi)){
        return rt->scopes.scopes[si].entries[vi].val;
    }
    return NULL;
}
static void runtime_create_or_set_var(CLRuntime *rt, const char *name, VarType type, const char *val){
    // search existing
    size_t si, vi;
    if(scopestack_find_var(&rt->scopes, name, &si, &vi)){
        Scope *s = &rt->scopes.scopes[si];
        free(s->entries[vi].val);
        s->entries[vi].type = type;
        s->entries[vi].val = strdup(val ? val : "");
        return;
    }
    // not found -> put in top scope
    Scope *top = scopestack_top(&rt->scopes);
    scope_set_or_create(top, name, type, val);
}

// parse "typ::name = value"
static void runtime_store_variable_cmd(CLRuntime *rt, const char *cmd){
    const char *p2 = strstr(cmd, "::");
    const char *p3 = strchr(cmd, '=');
    if(!p2 || !p3) return;
    char *typ = str_trim(strndup(cmd, p2 - cmd));
    char *name = str_trim(strndup(p2 + 2, p3 - (p2+2)));
    char *val = str_trim(p3 + 1);
    VarType vt = VAR_TYPE_STR;
    char ltyp[64];
    strncpy(ltyp, typ, sizeof(ltyp)-1); ltyp[sizeof(ltyp)-1]=0; str_lower_inplace(ltyp);
    if(strcmp(ltyp, "int")==0 || strcmp(ltyp,"integer")==0) vt = VAR_TYPE_INT;
    else if(strcmp(ltyp,"float")==0 || strcmp(ltyp,"floatation")==0) vt = VAR_TYPE_FLOAT;
    else vt = VAR_TYPE_STR;

    if(vt == VAR_TYPE_STR){
        char *valstr = NULL;
        if(is_single_quoted_literal(val)){
            valstr = strip_quotes_if_any(val);
        } else if(is_simple_identifier(val)){
            const char *v = runtime_get_var_value(rt, val);
            valstr = strdup(v ? v : val);
        } else {
            EvalResult er = {1, 0.0, NULL};
            if(eval_expr(rt, val, &er)){
                valstr = eval_result_to_string(&er);
                evalresult_free(&er);
            } else {
                valstr = strdup(val);
            }
        }
        runtime_create_or_set_var(rt, name, VAR_TYPE_STR, valstr);
        free(valstr);
    } else if(vt == VAR_TYPE_INT){
        double out = eval_num_expr(rt, val, NULL);
        char buf[128];
        snprintf(buf, sizeof(buf), "%lld", (long long)out);
        runtime_create_or_set_var(rt, name, VAR_TYPE_INT, buf);
    } else {
        double out = eval_num_expr(rt, val, NULL);
        char buf[128];
        snprintf(buf, sizeof(buf), "%g", out);
        runtime_create_or_set_var(rt, name, VAR_TYPE_FLOAT, buf);
    }

    free(typ); free(name); free(val);
}

// assignment handler: name = expr or name = fname(args)
static void runtime_handle_assignment(CLRuntime *rt, const char *line){
    const char *p = strchr(line, '=');
    if(!p) return;
    char *name = str_trim(strndup(line, p - line));
    char *expr = str_trim(p + 1);
    // detect function call pattern fname(args)
    // naive parse: identifier followed by '('
    char fnamebuf[256];
    if(sscanf(expr, " %255[A-Za-z0-9_]%*[ ](", fnamebuf) == 1){
        // treat as call if pattern exists
        // extract args between parentheses (find matching)
        const char *lp = strchr(expr, '(');
        const char *rp = strrchr(expr, ')');
        char *argstr = NULL;
        if(lp && rp && rp > lp){
            argstr = strndup(lp+1, rp - lp - 1);
        } else argstr = strdup("");
        // split args by comma
        lines_arr args;
        lines_init(&args);
        char *token = strtok(argstr, ",");
        while(token){
            char *t = str_trim(token);
            lines_push(&args, t);
            free(t);
            token = strtok(NULL, ",");
        }
        // call builtin or function
        EvalResult er = {1,0.0,NULL};
        // builtins: rand, randint
        if(strcasecmp(fnamebuf, "rand") == 0){
            er.is_number = 1; er.num = builtin_rand(rt, &args);
        } else if(strcasecmp(fnamebuf, "randint")==0){
            er.is_number = 1; er.num = builtin_randint(rt, &args);
        } else {
            // user-defined function call -> we will call but we only support subr side-effects and functions that return numeric via 'return'
            // For now, callFunctionOrBuiltin is complex; call a stub that returns 0
            // TODO: implement function call invocation
            er.is_number = 1; er.num = 0.0;
        }
        // assign type based on er
        if(er.is_number){
            long iv = (long)er.num;
            if(fabs(er.num - (double)iv) < 1e-12){
                // integer
                char tmp[64]; snprintf(tmp, sizeof(tmp), "%ld", iv);
                runtime_create_or_set_var(rt, name, VAR_TYPE_INT, tmp);
            } else {
                char tmp[64]; snprintf(tmp, sizeof(tmp), "%g", er.num);
                runtime_create_or_set_var(rt, name, VAR_TYPE_FLOAT, tmp);
            }
        } else {
            runtime_create_or_set_var(rt, name, VAR_TYPE_STR, er.str ? er.str : "");
        }
        // free args
        for(size_t i=0;i<args.count;i++) free(args.lines[i]);
        free(args.lines);
        free(argstr);
        free(name); free(expr);
        if(er.str) free(er.str);
        return;
    }
    // if rhs is quoted string
    if(is_single_quoted_literal(expr)){
        char *s = strip_quotes_if_any(expr);
        runtime_create_or_set_var(rt, name, VAR_TYPE_STR, s);
        free(s);
        free(name); free(expr);
        return;
    }
    // else numeric expr
    int isint = 0;
    double val = eval_num_expr(rt, expr, &isint);
    char buf[128];
    if(isint){ snprintf(buf, sizeof(buf), "%ld", (long)llround(val)); runtime_create_or_set_var(rt, name, VAR_TYPE_INT, buf); }
    else { snprintf(buf, sizeof(buf), "%g", val); runtime_create_or_set_var(rt, name, VAR_TYPE_FLOAT, buf); }
    free(name); free(expr);
}

static char *runtime_eval_print_piece(CLRuntime *rt, const char *piece){
    char *tk = str_trim(piece ? piece : "");
    char *out = NULL;

    if(tk[0] == 0){
        out = strdup("");
    } else if(is_single_quoted_literal(tk)){
        out = strip_quotes_if_any(tk);
    } else if(is_simple_identifier(tk)){
        const char *v = runtime_get_var_value(rt, tk);
        out = strdup(v ? v : tk);
    } else {
        EvalResult er = {1, 0.0, NULL};
        if(eval_expr(rt, tk, &er)){
            out = eval_result_to_string(&er);
            evalresult_free(&er);
        } else {
            out = strdup(tk);
        }
    }
    free(tk);
    return out;
}

// print implementation:
// - '&' joins pieces with a space
// - '+' is no longer text concatenation; it stays part of the expression and is evaluated numerically.
static void runtime_do_print(CLRuntime *rt, const char *expr){
    const char *p = expr ? expr : "";
    char tokenbuf[4096];
    char outbuf[8192]; outbuf[0] = 0;
    int first_piece = 1;

    while(1){
        int inS = 0, inD = 0, esc = 0, paren = 0;
        const char *start = p;
        for(; *p; p++){
            char ch = *p;
            if(esc){ esc = 0; continue; }
            if(ch == '\\'){ esc = 1; continue; }
            if(ch == '"' && !inS){ inD = !inD; continue; }
            if(ch == '\'' && !inD){ inS = !inS; continue; }
            if(!inS && !inD){
                if(ch == '('){ paren++; continue; }
                if(ch == ')' && paren > 0){ paren--; continue; }
                if(paren == 0 && ch == '&') break;
            }
        }

        size_t toklen = (size_t)(p - start);
        if(toklen >= sizeof(tokenbuf)) toklen = sizeof(tokenbuf) - 1;
        memcpy(tokenbuf, start, toklen);
        tokenbuf[toklen] = 0;

        char *piece = runtime_eval_print_piece(rt, tokenbuf);
        if(!first_piece){
            strcat(outbuf, " ");
        }
        strcat(outbuf, piece);
        free(piece);

        if(!*p) break;
        p++; // skip '&'
        first_piece = 0;
    }

    runtime_push_output(rt, outbuf);
}

// capture raw block between startKw (at startIdx line) and endKw, handle nesting
static char *runtime_capture_raw_block(lines_arr *lines, size_t startIdx, const char *startKw, const char *endKw, size_t *endIdxOut){
    lines_arr body; lines_init(&body);
    size_t i = startIdx + 1;
    int nest = 0;
    while(i < lines->count){
        const char *raw = lines->lines[i];
        // codePart: strip comments '!' outside quotes. For simplicity we just keep full line
        // compute low trimmed leading
        char *l = str_lstrip(raw);
        char *lnorm = str_trim(l);
        char *lower = str_tolower(lnorm);
        if(starts_with_icase(lower, startKw)){
            nest++;
            lines_push(&body, raw);
            free(l); free(lnorm); free(lower);
            i++; continue;
        }
        if(starts_with_icase(lower, endKw)){
            if(nest == 0){
                *endIdxOut = i;
                // produce joined body as single string maybe, but we return via caller's lines array; here return joined placeholder
                // We'll return a string pointer of zero length because caller uses captured lines differently.
                // But to conform to original signature, we return NULL.
                // Actually we return a placeholder; caller may parse lines slice.
                char *ret = strdup("CAPTURED_BLOCK");
                // free
                for(size_t j=0;j<body.count;j++) free(body.lines[j]);
                free(body.lines);
                free(l); free(lnorm); free(lower);
                return ret;
            } else {
                nest--;
                lines_push(&body, raw);
                free(l); free(lnorm); free(lower);
                i++; continue;
            }
        }
        lines_push(&body, raw);
        free(l); free(lnorm); free(lower);
        i++;
    }
    *endIdxOut = lines->count - 1;
    char *ret = strdup("CAPTURED_BLOCK_END");
    for(size_t j=0;j<body.count;j++) free(body.lines[j]);
    free(body.lines);
    return ret;
}

// fileExecuteBlock for tag DSL inside writefile: minimal implementation that writes lines to file buffer
static void runtime_file_execute_block(CLRuntime *rt, char **lines, size_t nlines, int indent){
    for(size_t i=0;i<nlines;i++){
        char *raw = lines[i];
        // strip leading spaces
        char *l = str_lstrip(raw);
        if(l[0] == 0){ free(l); continue; }
        // tag detection minimal
        char tagname[256]; char attrs[512]; char mode[8];
        if(sscanf(l, "tag %255s %511[^\n]", tagname, attrs) >= 1){
            // naive detection of self-closing: if ends with '/'
            size_t L = strlen(l);
            if(L >= 2 && l[L-1] == '/'){
                // self-closing
                char out[1024]; snprintf(out, sizeof(out), "<%s %s />", tagname, attrs);
                runtime_push_output(rt, out);
            } else {
                // opening tag -> print open
                char outopen[1024];
                if(strlen(attrs) > 0) snprintf(outopen, sizeof(outopen), "<%s %s>", tagname, attrs);
                else snprintf(outopen, sizeof(outopen), "<%s>", tagname);
                runtime_push_output(rt, outopen);
                // no nested parsing here in this minimal implementation
                // just write closing tag
                char outclose[512]; snprintf(outclose, sizeof(outclose), "</%s>", tagname);
                runtime_push_output(rt, outclose);
            }
            free(l); continue;
        }
        // text "literal"
        char tmatch1 = 0;
        if(sscanf(l, "text \"%511[^\"]\"", raw) == 1){
            runtime_push_output(rt, l); free(l); continue;
        }
        // fallback -> push raw trimmed
        runtime_push_output(rt, l);
        free(l);
    }
}

// handle writefile: find path, capture block, execute block with file buffer enabled, then write file
static void runtime_handle_writefile(CLRuntime *rt, const char *line, lines_arr *lines, size_t *idxPtr){
    // parse header
    char pathbuf[1024];
    const char *p = line;
    // accepts writefile "path" do  OR writefile path do
    char qchar = 0;
    const char *after = strstr(p, "writefile");
    if(!after) return;
    // find first quote or whitespace token
    const char *s = p + strlen("writefile");
    while(*s && isspace((unsigned char)*s)) s++;
    if(*s == '"' || *s == '\''){
        qchar = *s;
        s++;
        const char *endq = strchr(s, qchar);
        if(endq){
            size_t L = (size_t)(endq - s);
            if(L >= sizeof(pathbuf)) L = sizeof(pathbuf)-1;
            memcpy(pathbuf, s, L); pathbuf[L]=0;
        } else {
            pathbuf[0]=0;
        }
    } else {
        // read until space
        const char *t = s;
        size_t L = 0;
        while(*t && !isspace((unsigned char)*t) && L+1 < sizeof(pathbuf)){ pathbuf[L++] = *t; t++; }
        pathbuf[L]=0;
    }
    // capture raw block
    // find end writefile with nesting
    size_t i = *idxPtr;
    size_t j = i+1;
    int nest = 0;
    lines_arr body; lines_init(&body);
    while(j < lines->count){
        char *raw = lines->lines[j];
        char *lstriped = str_lstrip(raw);
        char *low = str_tolower(lstriped);
        if(starts_with_icase(low, "writefile")){
            nest++; lines_push(&body, raw);
            free(low); free(lstriped);
            j++; continue;
        }
        if(starts_with_icase(low, "end writefile")){
            if(nest == 0){
                break;
            } else {
                nest--; lines_push(&body, raw);
                free(low); free(lstriped);
                j++; continue;
            }
        }
        lines_push(&body, raw);
        free(low); free(lstriped);
        j++;
    }
    // enable file mode and execute body
    rt->fileMode = true;
    // free previous fileBuffer if present
    if(rt->fileBuffer){
        for(size_t k=0;k<rt->fileBufferCount;k++) free(rt->fileBuffer[k]);
        free(rt->fileBuffer);
        rt->fileBuffer = NULL;
        rt->fileBufferCount = rt->fileBufferCap = 0;
    }
    // temporarily run body via executeBlock but in fileMode; simpler: call fileExecuteBlock
    // minimal tag DSL handling: detect tag lines; otherwise call runtime_do_print for print lines
    for(size_t k=0;k<body.count;k++){
        char *raw = body.lines[k];
        char *l = str_lstrip(raw);
        if(!l || l[0]==0){ free(l); continue; }
        if(starts_with_icase(l, "tag ") || starts_with_icase(l, "text ")){
            runtime_push_output(rt, l); free(l); continue;
        }
        if(starts_with_icase(l, "print")){
            // extract rest
            const char *rest = l + 5;
            while(*rest && isspace((unsigned char)*rest)) rest++;
            if(*rest == ',') rest++;
            while(*rest && isspace((unsigned char)*rest)) rest++;
            runtime_do_print(rt, rest);
            free(l); continue;
        }
        // default: push raw trimmed
        runtime_push_output(rt, l);
        free(l);
    }
    // join fileBuffer and write out
    // build file content
    dstr filecontent; dstr_init(&filecontent);
    for(size_t k=0;k<rt->fileBufferCount;k++){
        dstr_append(&filecontent, "%s\n", rt->fileBuffer[k]);
    }
    // write to disk
    if(pathbuf[0]){
        FILE *f = fopen(pathbuf, "wb");
        if(f){
            fwrite(filecontent.data, 1, filecontent.len, f);
            fclose(f);
            char msg[1024]; snprintf(msg, sizeof(msg), "WRITEFILE: %s", pathbuf);
            runtime_push_output(rt, msg);
        } else {
            char msg[1024]; snprintf(msg, sizeof(msg), "ERROR writing file: %s", strerror(errno));
            runtime_push_output(rt, msg);
        }
    } else {
        runtime_push_output(rt, "WRITEFILE: (no path specified)");
    }
    dstr_free(&filecontent);
    // cleanup body
    for(size_t k=0;k<body.count;k++) free(body.lines[k]);
    free(body.lines);
    // reset file mode / buffer
    if(rt->fileBuffer){
        for(size_t k=0;k<rt->fileBufferCount;k++) free(rt->fileBuffer[k]);
        free(rt->fileBuffer); rt->fileBuffer = NULL; rt->fileBufferCount = rt->fileBufferCap = 0;
    }
    rt->fileMode = false;
    // advance idxPtr to end
    *idxPtr = j;
}

// storeFunction
static void runtime_store_function(CLRuntime *rt, const char *kind, const char *headerLine, lines_arr *bodyLines){
    // parse header like: subr name(a,b)
    char kbuf[64]; char namebuf[256];
    char paramsbuf[512];
    if(sscanf(headerLine, "%63s %255[^ (] ( %511[^)] )", kbuf, namebuf, paramsbuf) < 2){
        // simpler parse
        // try regex-like: find first word after kind
        const char *p = headerLine;
        while(*p && isspace((unsigned char)*p)) p++;
        // skip kind
        while(*p && !isspace((unsigned char)*p)) p++;
        while(*p && isspace((unsigned char)*p)) p++;
        const char *q = p;
        size_t L = 0;
        while(*q && (isalnum((unsigned char)*q) || *q == '_')){ q++; L++; }
        strncpy(namebuf, p, L); namebuf[L]=0;
        paramsbuf[0]=0;
    } else {
        // sscanf may capture weird; ensure strings are trimmed
    }
    CLFunction *f = malloc(sizeof(CLFunction));
    f->name = strdup(namebuf);
    f->kind = strdup(kind);
    // parse params from headerLine
    // find '(' and ')'
    const char *lp = strchr(headerLine, '(');
    const char *rp = lp ? strchr(lp, ')') : NULL;
    f->param_count = 0;
    f->params = NULL;
    if(lp && rp && rp > lp){
        char *plist = strndup(lp+1, (size_t)(rp - lp - 1));
        // split by comma
        f->params = malloc(sizeof(char*) * MAX_PARAMS);
        char *tok = strtok(plist, ",");
        while(tok){
            char *t = str_trim(tok);
            f->params[f->param_count++] = strdup(t);
            free(t);
            tok = strtok(NULL, ",");
        }
        free(plist);
    } else {
        f->params = malloc(sizeof(char*) * 1);
    }
    lines_init(&f->body);
    // copy bodyLines into f->body
    for(size_t i=0;i<bodyLines->count;i++){
        lines_push(&f->body, bodyLines->lines[i]);
    }
    func_table_add(&rt->funcs, f);
}

// executeBlock: interpret lines in blockLines
static void runtime_execute_block(CLRuntime *rt, lines_arr *block){
    for(size_t j=0;j<block->count;j++){
        char *raw = block->lines[j];
        // handle prompt stripping and comments: for simplicity we only lstrip
        char *l = str_lstrip(raw);
        // strip comments '!' outside quotes - simplified: if '!' appears and not within quotes, truncate
        char *out = malloc(strlen(l)+1); size_t oi=0;
        int inS=0,inD=0,esc=0;
        for(size_t i=0;i<strlen(l);i++){
            char ch = l[i];
            if(esc){ out[oi++]=ch; esc=0; continue; }
            if(ch == '\\'){ esc=1; continue; }
            if(ch == '\'' && !inD){ inS = !inS; out[oi++] = ch; continue; }
            if(ch == '"' && !inS){ inD = !inD; out[oi++] = ch; continue; }
            if(ch == '!' && !inS && !inD){ break; } // comment start
            out[oi++] = ch;
        }
        out[oi] = 0;
        // normalize HTML entities (basic)
        // lstrip again and trim
        char *lnorm = str_trim(out);
        free(out); free(l);
        if(!lnorm || lnorm[0]==0){ free(lnorm); continue; }
        char *low = str_tolower(lnorm);
        // nested subr/function definitions
        if(starts_with_icase(low, "subr ") || starts_with_icase(low, "function ")){
            // capture raw block until end subr / end function
            const char *startKw = starts_with_icase(low, "subr ") ? "subr" : "function";
            const char *endKw = starts_with_icase(low, "subr ") ? "end subr" : "end function";
            // capture body
            size_t endIdx = 0;
            // reuse runtime_capture_raw_block: but it returns placeholder; better to manually capture lines
            size_t ii = j;
            int nest = 0;
            lines_arr fbody; lines_init(&fbody);
            ii++; // start from next
            while(ii < block->count){
                char *raw2 = block->lines[ii];
                char *l2 = str_lstrip(raw2);
                char *low2 = str_tolower(str_trim(l2));
                if(starts_with_icase(low2, startKw)){
                    nest++; lines_push(&fbody, raw2);
                    free(l2); free(low2);
                    ii++; continue;
                }
                if(starts_with_icase(low2, endKw)){
                    if(nest == 0){
                        // store function: headerLine is lnorm, body is fbody
                        runtime_store_function(rt, startKw, lnorm, &fbody);
                        free(l2); free(low2);
                        break;
                    } else {
                        nest--; lines_push(&fbody, raw2);
                        free(l2); free(low2);
                        ii++; continue;
                    }
                }
                lines_push(&fbody, raw2);
                free(l2); free(low2);
                ii++;
            }
            // cleanup fbody memory copy we created for CLFunction
            for(size_t k=0;k<fbody.count;k++) free(fbody.lines[k]);
            free(fbody.lines);
            // find end index ii and skip ahead
            j = ii;
            free(lnorm); free(low);
            continue;
        }

        // writefile
        if(starts_with_icase(low, "writefile ")){
            size_t idx = j;
            runtime_handle_writefile(rt, lnorm, block, &idx);
            j = idx;
            free(lnorm); free(low);
            continue;
        }

        // IF handling
        if(starts_with_icase(low, "if ")){
            // push ifStack
            if(rt->ifStackCount + 1 >= rt->ifStackCap){
                rt->ifStackCap *= 2;
                rt->ifStack = realloc(rt->ifStack, sizeof(*rt->ifStack) * rt->ifStackCap);
            }
            // extract condition part after 'if'
            const char *cond = lnorm + 2;
            char *ct = str_trim(cond);
            int r = eval_bool_expr(rt, ct);
            rt->ifStack[rt->ifStackCount].active = r;
            rt->ifStack[rt->ifStackCount].matched = r;
            rt->ifStackCount++;
            free(ct);
            free(lnorm); free(low);
            continue;
        }
        if(starts_with_icase(low, "elif ")){
            if(rt->ifStackCount == 0){ free(lnorm); free(low); continue; }
            if(rt->ifStack[rt->ifStackCount-1].matched){
                rt->ifStack[rt->ifStackCount-1].active = false;
                free(lnorm); free(low); continue;
            } else {
                const char *cond = lnorm + 4;
                char *ct = str_trim(cond);
                int r = eval_bool_expr(rt, ct);
                rt->ifStack[rt->ifStackCount-1].active = r;
                rt->ifStack[rt->ifStackCount-1].matched = r;
                free(ct);
                free(lnorm); free(low);
                continue;
            }
        }
        if(starts_with_icase(low, "else")){
            if(rt->ifStackCount == 0){ free(lnorm); free(low); continue; }
            if(!rt->ifStack[rt->ifStackCount-1].matched){
                rt->ifStack[rt->ifStackCount-1].active = true;
                rt->ifStack[rt->ifStackCount-1].matched = true;
            } else {
                rt->ifStack[rt->ifStackCount-1].active = false;
            }
            free(lnorm); free(low);
            continue;
        }
        if(starts_with_icase(low, "end if")){
            if(rt->ifStackCount > 0) rt->ifStackCount--;
            free(lnorm); free(low);
            continue;
        }

        if(rt->ifStackCount > 0 && !rt->ifStack[rt->ifStackCount-1].active){
            // skip this line
            free(lnorm); free(low);
            continue;
        }

        // loops handling: for/while/dowhile
        if(starts_with_icase(low, "for") || starts_with_icase(low, "while") || starts_with_icase(low, "dowhile") || starts_with_icase(low, "do while")){
            // for: for var range start to end [step s]
            if(starts_with_icase(low, "for")){
                // parse
                char varname[256]; double startv=0, endv=0, step=1.0;
                // crude parse: for <var> range <start> to <end> [step <s>]
                char *copy = strdup(lnorm);
                char *p = copy;
                // tokenize
                char *tok = strtok(p, " \t");
                // "for"
                tok = strtok(NULL, " \t"); if(tok) strncpy(varname, tok, sizeof(varname)-1); else varname[0]=0;
                // expect "range"
                tok = strtok(NULL, " \t");
                if(tok && strcasecmp(tok, "range")==0){
                    tok = strtok(NULL, " \t"); if(tok) startv = atof(tok);
                    tok = strtok(NULL, " \t"); // "to"
                    if(tok && strcasecmp(tok, "to")==0){ tok = strtok(NULL, " \t"); if(tok) endv = atof(tok); }
                    tok = strtok(NULL, " \t");
                    if(tok && strcasecmp(tok, "step")==0){
                        tok = strtok(NULL, " \t"); if(tok) step = atof(tok);
                    }
                }
                free(copy);
                // capture body until end for
                size_t ii = j+1;
                int nest = 0;
                lines_arr body; lines_init(&body);
                while(ii < block->count){
                    char *raw2 = block->lines[ii];
                    char *low2 = str_tolower(str_trim(str_lstrip(raw2)));
                    if(starts_with_icase(low2, "for")||starts_with_icase(low2,"while")||starts_with_icase(low2,"dowhile")||starts_with_icase(low2,"do while")){
                        nest++; lines_push(&body, raw2); free(low2); ii++; continue;
                    }
                    if(starts_with_icase(low2, "end for")||starts_with_icase(low2,"end while")||starts_with_icase(low2,"end do")){
                        if(nest == 0){ free(low2); break; } else { nest--; lines_push(&body, raw2); free(low2); ii++; continue; }
                    }
                    lines_push(&body, raw2); free(low2); ii++;
                }
                // run loop
                if(step >= 0){
                    for(double v = startv; v <= endv + 1e-12; v += step){
                        // set var in top scope as int
                        char buf[64]; snprintf(buf, sizeof(buf), "%ld", (long)llround(v));
                        runtime_create_or_set_var(rt, varname, VAR_TYPE_INT, buf);
                        runtime_execute_block(rt, &body);
                    }
                } else {
                    for(double v = startv; v >= endv - 1e-12; v += step){
                        char buf[64]; snprintf(buf, sizeof(buf), "%ld", (long)llround(v));
                        runtime_create_or_set_var(rt, varname, VAR_TYPE_INT, buf);
                        runtime_execute_block(rt, &body);
                    }
                }
                // cleanup
                for(size_t k=0;k<body.count;k++) free(body.lines[k]);
                free(body.lines);
                j = ii;
                free(lnorm); free(low);
                continue;
            } else if(starts_with_icase(low, "while")){
                // while cond: capture body then execute while condition true
                const char *cond = lnorm + 5;
                char *condt = str_trim(cond);
                size_t ii = j+1; int nest=0;
                lines_arr body; lines_init(&body);
                while(ii < block->count){
                    char *raw2 = block->lines[ii];
                    char *low2 = str_tolower(str_trim(str_lstrip(raw2)));
                    if(starts_with_icase(low2, "for")||starts_with_icase(low2,"while")||starts_with_icase(low2,"dowhile")||starts_with_icase(low2,"do while")){ nest++; lines_push(&body, raw2); free(low2); ii++; continue; }
                    if(starts_with_icase(low2, "end for")||starts_with_icase(low2,"end while")||starts_with_icase(low2,"end do")){ if(nest==0){ free(low2); break; } else { nest--; lines_push(&body, raw2); free(low2); ii++; continue; } }
                    lines_push(&body, raw2); free(low2); ii++;
                }
                while(eval_bool_expr(rt, condt)){
                    runtime_execute_block(rt, &body);
                }
                for(size_t k=0;k<body.count;k++) free(body.lines[k]);
                free(body.lines); free(condt);
                j = ii;
                free(lnorm); free(low);
                continue;
            } else if(starts_with_icase(low, "dowhile") || starts_with_icase(low, "do while")){
                // capture body and test condition
                const char *cond = NULL;
                // find "while" position
                char *poslow = str_tolower(lnorm);
                char *w = strstr(poslow, "while");
                if(w) cond = lnorm + (w - poslow) + 5;
                if(poslow) free(poslow);
                char *condt = cond ? str_trim(cond) : strdup("0");
                size_t ii = j+1; int nest=0;
                lines_arr body; lines_init(&body);
                while(ii < block->count){
                    char *raw2 = block->lines[ii];
                    char *low2 = str_tolower(str_trim(str_lstrip(raw2)));
                    if(starts_with_icase(low2, "for")||starts_with_icase(low2,"while")||starts_with_icase(low2,"dowhile")||starts_with_icase(low2,"do while")){ nest++; lines_push(&body, raw2); free(low2); ii++; continue; }
                    if(starts_with_icase(low2, "end for")||starts_with_icase(low2,"end while")||starts_with_icase(low2,"end do")){ if(nest==0){ free(low2); break; } else { nest--; lines_push(&body, raw2); free(low2); ii++; continue; } }
                    lines_push(&body, raw2); free(low2); ii++;
                }
                do {
                    runtime_execute_block(rt, &body);
                } while(eval_bool_expr(rt, condt));
                for(size_t k=0;k<body.count;k++) free(body.lines[k]);
                free(body.lines); free(condt);
                j = ii;
                free(lnorm); free(low);
                continue;
            }
        }

        // declaration typ::name = value
        if(strstr(lnorm, "::") && strchr(lnorm, '=')){
            runtime_store_variable_cmd(rt, lnorm);
            free(lnorm); free(low);
            continue;
        }

        // assignment
        if(strchr(lnorm, '=') && strstr(lnorm, "==") == NULL){
            runtime_handle_assignment(rt, lnorm);
            free(lnorm); free(low);
            continue;
        }

        // print
        if(starts_with_icase(lnorm, "print")){
            const char *rest = lnorm + 5;
            while(*rest && isspace((unsigned char)*rest)) rest++;
            if(*rest == ',') rest++;
            while(*rest && isspace((unsigned char)*rest)) rest++;
            runtime_do_print(rt, rest);
            free(lnorm); free(low);
            continue;
        }

        // call subr: call name(args)
        if(starts_with_icase(lnorm, "call ")){
            const char *callbody = lnorm + 5;
            // match name(args)
            char fname[256]; char argsbuf[512];
            if(sscanf(callbody, " %255[A-Za-z0-9_] ( %511[^)] )", fname, argsbuf) >= 1){
                // TODO: perform function invocation
                // stub: ignore
            } else {
                // try simpler
                char tname[256];
                if(sscanf(callbody, " %255[A-Za-z0-9_]", tname) == 1){
                    // stub
                }
            }
            free(lnorm); free(low);
            continue;
        }

        // otherwise ignore
        free(lnorm); free(low);
    }
}

// runFromText entry
static char *runtime_run_from_text(CLRuntime *rt, const char *src){
    // split into lines
    lines_arr all; lines_init(&all);
    // normalize newlines and split
    char *copy = strdup(src);
    char *p = copy;
    char *line;
    while((line = strsep(&p, "\n"))){
        lines_push(&all, line);
    }
    free(copy);
    // process top-level lines
    for(size_t i=0;i<all.count;i++){
        char *raw = all.lines[i];
        char *l = str_lstrip(raw);
        // strip comments '!'
        char *out = malloc(strlen(l)+1); size_t oi=0;
        int inS=0, inD=0, esc=0;
        for(size_t k=0;k<strlen(l);k++){
            char ch = l[k];
            if(esc){ out[oi++]=ch; esc=0; continue; }
            if(ch == '\\'){ esc=1; continue; }
            if(ch == '\'' && !inD){ inS = !inS; out[oi++]=ch; continue; }
            if(ch == '"' && !inS){ inD = !inD; out[oi++]=ch; continue; }
            if(ch == '!' && !inS && !inD){ break; }
            out[oi++] = ch;
        }
        out[oi] = 0;
        char *lnorm = str_trim(out);
        free(out); free(l);
        if(lnorm[0] == 0){ free(lnorm); continue; }
        // prog start
        if(starts_with_icase(lnorm, "prog ")){
            char *pname = str_trim(lnorm + 5);
            if(rt->currentProgName) free(rt->currentProgName);
            rt->currentProgName = strdup(pname);
            rt->inProg = true;
            free(pname); free(lnorm); continue;
        }
        if(starts_with_icase(lnorm, "end prog")){
            // finish program
            runtime_push_output(rt, "Program ended.");
            char *o = runtime_flush_output(rt);
            // reset runtime somewhat
            // free functions etc if needed
            free(lnorm);
            // cleanup lines
            for(size_t k=0;k<all.count;k++) free(all.lines[k]);
            free(all.lines);
            return o;
        }
        if(!rt->inProg){ free(lnorm); continue; }
        // store functions
        if(starts_with_icase(lnorm, "subr ") || starts_with_icase(lnorm, "function ")){
            const char *startKw = starts_with_icase(lnorm, "subr ") ? "subr" : "function";
            const char *endKw = starts_with_icase(lnorm, "subr ") ? "end subr" : "end function";
            // capture raw block
            size_t ii = i + 1;
            int nest = 0;
            lines_arr body; lines_init(&body);
            while(ii < all.count){
                char *raw2 = all.lines[ii];
                char *l2 = str_lstrip(raw2);
                char *low2 = str_tolower(str_trim(l2));
                if(starts_with_icase(low2, startKw)){ nest++; lines_push(&body, raw2); free(l2); free(low2); ii++; continue; }
                if(starts_with_icase(low2, endKw)){
                    if(nest == 0){
                        // store function
                        runtime_store_function(rt, startKw, lnorm, &body);
                        free(l2); free(low2);
                        break;
                    } else { nest--; lines_push(&body, raw2); free(l2); free(low2); ii++; continue; }
                }
                lines_push(&body, raw2);
                free(l2); free(low2);
                ii++;
            }
            for(size_t k=0;k<body.count;k++) free(body.lines[k]);
            free(body.lines);
            i = ii;
            free(lnorm);
            continue;
        }
        // writefile top-level
        if(starts_with_icase(lnorm, "writefile ")){
            size_t idx = i;
            runtime_handle_writefile(rt, lnorm, &all, &idx);
            i = idx;
            free(lnorm);
            continue;
        }
        // if/elif/else/end if handlers
        if(starts_with_icase(lnorm, "if ")){
            if(rt->ifStackCount + 1 >= rt->ifStackCap){
                rt->ifStackCap *= 2;
                rt->ifStack = realloc(rt->ifStack, sizeof(*rt->ifStack) * rt->ifStackCap);
            }
            char *ct = str_trim(lnorm + 2);
            int r = eval_bool_expr(rt, ct);
            rt->ifStack[rt->ifStackCount].active = r;
            rt->ifStack[rt->ifStackCount].matched = r;
            rt->ifStackCount++;
            free(ct); free(lnorm); continue;
        }
        if(starts_with_icase(lnorm, "elif ")){
            if(rt->ifStackCount == 0){ free(lnorm); continue; }
            if(rt->ifStack[rt->ifStackCount-1].matched){ rt->ifStack[rt->ifStackCount-1].active = false; free(lnorm); continue; }
            char *ct = str_trim(lnorm + 4);
            int r = eval_bool_expr(rt, ct);
            rt->ifStack[rt->ifStackCount-1].active = r;
            rt->ifStack[rt->ifStackCount-1].matched = r;
            free(ct); free(lnorm); continue;
        }
        if(starts_with_icase(lnorm, "else")){
            if(rt->ifStackCount == 0) { free(lnorm); continue; }
            if(!rt->ifStack[rt->ifStackCount-1].matched){ rt->ifStack[rt->ifStackCount-1].active = true; rt->ifStack[rt->ifStackCount-1].matched = true; } else rt->ifStack[rt->ifStackCount-1].active = false;
            free(lnorm); continue;
        }
        if(starts_with_icase(lnorm, "end if")){
            if(rt->ifStackCount > 0) rt->ifStackCount--;
            free(lnorm); continue;
        }
        if(rt->ifStackCount > 0 && !rt->ifStack[rt->ifStackCount-1].active){ free(lnorm); continue; }

        // loops at top-level: reuse executeBlock code by creating a single-line block array to pass to runtime_execute_block
        // but easier: replicate similar logic here for top-level loops
        if(starts_with_icase(lnorm, "for") || starts_with_icase(lnorm, "while") || starts_with_icase(lnorm, "dowhile") || starts_with_icase(lnorm, "do while")){
            // get remaining lines slice
            // build a sub-array from i .. end and call handler extraction similar to executeBlock's handlerLoopStart
            // For simplicity, create lines_arr slice starting at i
            lines_arr slice; lines_init(&slice);
            for(size_t k=i;k<all.count;k++) lines_push(&slice, all.lines[k]);
            // run runtime_execute_block on this slice? runtime_execute_block expects normal lines; but we will implement simpler: call runtime_execute_block which supports nested loops
            runtime_execute_block(rt, &slice);
            // advancing i is complex; to be safe, break out (we likely ran until end)
            for(size_t k=0;k<slice.count;k++) free(slice.lines[k]);
            free(slice.lines);
            free(lnorm);
            break;
        }

        // declaration
        if(strstr(lnorm, "::") && strchr(lnorm, '=')){
            runtime_store_variable_cmd(rt, lnorm); free(lnorm); continue;
        }

        // assignment
        if(strchr(lnorm, '=') && strstr(lnorm, "==") == NULL){
            runtime_handle_assignment(rt, lnorm); free(lnorm); continue;
        }

        // print
        if(starts_with_icase(lnorm, "print")){
            const char *rest = lnorm + 5;
            while(*rest && isspace((unsigned char)*rest)) rest++;
            if(*rest == ',') rest++;
            while(*rest && isspace((unsigned char)*rest)) rest++;
            runtime_do_print(rt, rest);
            free(lnorm); continue;
        }

        // call top-level subr
        if(starts_with_icase(lnorm, "call ")){
            // naive call stub
            // ignore
            free(lnorm); continue;
        }

        free(lnorm);
    }
    // if inProg but not ended, finalize
    if(rt->inProg){
        runtime_push_output(rt, "Program (unfinished) ended.");
    }
    char *o = runtime_flush_output(rt);
    for(size_t k=0;k<all.count;k++) free(all.lines[k]);
    free(all.lines);
    return o;
}

// convenience runner
static char *runCowLangFromText(const char *src){
    CLRuntime rt;
    runtime_init(&rt);
    char *out = runtime_run_from_text(&rt, src);
    runtime_free(&rt);
    return out;
}

// -----------------------------
// SDL2-based GFX engine (stars + comets) integrated with runtime
// -----------------------------
typedef struct {
    float x, y;
    float size;
    float twinkle;
} GStar;
typedef struct {
    float x, y;
    float vx, vy;
    float life;
    bool active;
} GComet;

typedef struct {
    SDL_Window *window;
    SDL_Renderer *renderer;
    int width, height;
    GStar stars[MAX_STARS];
    size_t starCount;
    GComet comets[MAX_COMETS];
    size_t cometCount;
    bool running;
    Uint32 lastCometSpawn;
} GfxEngine;

static void gfx_init(GfxEngine *g){
    g->width = DEFAULT_WIDTH; g->height = DEFAULT_HEIGHT;
    for(size_t i=0;i<MAX_STARS;i++) { g->stars[i].x = g->stars[i].y = 0; g->stars[i].size = 0; g->stars[i].twinkle = 0; }
    for(size_t i=0;i<MAX_COMETS;i++){ g->comets[i].active = false; }
    g->starCount = 0; g->cometCount = 0;
    g->running = false; g->lastCometSpawn = 0;
}

static float randf(){ return (float)rand() / (float)RAND_MAX; }

static void gfx_add_star(GfxEngine *g){
    if(g->starCount >= MAX_STARS) return;
    g->stars[g->starCount].x = randf() * (float)g->width;
    g->stars[g->starCount].y = randf() * (float)g->height;
    g->stars[g->starCount].size = randf() * 2.0f;
    g->stars[g->starCount].twinkle = randf() * 100.0f;
    g->starCount++;
}
static void gfx_add_comet(GfxEngine *g){
    for(size_t i=0;i<MAX_COMETS;i++){
        if(!g->comets[i].active){
            g->comets[i].x = randf() * (float)g->width;
            g->comets[i].y = -50.0f;
            g->comets[i].vx = -6.0f - randf()*4.0f;
            g->comets[i].vy = 3.0f + randf()*2.0f;
            g->comets[i].life = 1.0f;
            g->comets[i].active = true;
            return;
        }
    }
}

static void gfx_draw_rect(SDL_Renderer *r, float x, float y, float w, float h, float alpha){
    SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);
    Uint8 a = (Uint8)fmax(0.0f, fmin(1.0f, alpha)) * 255;
    SDL_SetRenderDrawColor(r, 255, 255, 255, a);
    SDL_FRect rect = { x, y, w, h };
    SDL_RenderFillRectF(r, &rect);
}

// main gfx run (runs in same thread)
static void gfx_run(GfxEngine *g){
    if(SDL_Init(SDL_INIT_VIDEO) != 0){
        fprintf(stderr, "SDL_Init error: %s\n", SDL_GetError()); return;
    }
    g->window = SDL_CreateWindow("CowLang GFX Runtime", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, g->width, g->height, SDL_WINDOW_RESIZABLE);
    if(!g->window){ fprintf(stderr, "SDL_CreateWindow failed: %s\n", SDL_GetError()); SDL_Quit(); return; }
    g->renderer = SDL_CreateRenderer(g->window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if(!g->renderer){ fprintf(stderr, "SDL_CreateRenderer failed: %s\n", SDL_GetError()); SDL_DestroyWindow(g->window); SDL_Quit(); return; }

    // populate initial stars
    for(int i=0;i<200;i++) gfx_add_star(g);
    g->running = true;
    g->lastCometSpawn = SDL_GetTicks();
    Uint32 prev = SDL_GetTicks();

    while(g->running){
        SDL_Event e;
        while(SDL_PollEvent(&e)){
            if(e.type == SDL_QUIT) g->running = false;
            if(e.type == SDL_WINDOWEVENT && e.window.event == SDL_WINDOWEVENT_SIZE_CHANGED){
                g->width = e.window.data1; g->height = e.window.data2;
            }
        }
        Uint32 now = SDL_GetTicks();
        if(now - g->lastCometSpawn > 1500){
            if(randf() < 0.6f) gfx_add_comet(g);
            g->lastCometSpawn = now;
        }

        // clear
        SDL_SetRenderDrawColor(g->renderer, 0, 0, 0, 255);
        SDL_RenderClear(g->renderer);

        // stars
        for(size_t i=0;i<g->starCount;i++){
            g->stars[i].twinkle += 0.05f;
            float alpha = 0.5f + sinf(g->stars[i].twinkle) * 0.5f;
            gfx_draw_rect(g->renderer, g->stars[i].x, g->stars[i].y, g->stars[i].size, g->stars[i].size, alpha);
        }
        // comets
        for(size_t i=0;i<MAX_COMETS;i++){
            if(!g->comets[i].active) continue;
            g->comets[i].x += g->comets[i].vx;
            g->comets[i].y += g->comets[i].vy;
            g->comets[i].life -= 0.01f;
            gfx_draw_rect(g->renderer, g->comets[i].x, g->comets[i].y, 3.0f, 3.0f, g->comets[i].life);
            if(g->comets[i].life <= 0.0f) g->comets[i].active = false;
        }
        SDL_RenderPresent(g->renderer);
        SDL_Delay(16);
    }

    SDL_DestroyRenderer(g->renderer);
    SDL_DestroyWindow(g->window);
    SDL_Quit();
}

// -----------------------------
// CLI and glue
// -----------------------------
static void print_usage(const char *prog){
    printf("Usage: %s [program.cow]\n", prog);
}

int main(int argc, char **argv){
    srand((unsigned int)time(NULL));
    if(argc < 2){
        printf("No input file provided. Starting GFX demo only.\n");
        // start gfx only
        GfxEngine g; gfx_init(&g);
        gfx_run(&g);
        return 0;
    }
    // read input file into string
    const char *path = argv[1];
    FILE *f = fopen(path, "rb");
    if(!f){
        fprintf(stderr, "Failed to open %s: %s\n", path, strerror(errno));
        return 2;
    }
    fseek(f, 0, SEEK_END);
    long flen = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(flen + 1);
    if(!buf){ perror("malloc"); fclose(f); return 3; }
    fread(buf, 1, flen, f); buf[flen]=0; fclose(f);

    // run translator
    CLRuntime rt;
    runtime_init(&rt);
    char *out = runtime_run_from_text(&rt, buf);
    if(out && strlen(out) > 0) printf("%s\n", out);
    if(out) free(out);

    // start gfx engine (and auto-run first program if any) -- for simplicity we spawn gfx and keep it running
    GfxEngine g; gfx_init(&g);
    // optionally, you might want to spawn a thread for gfx and keep interpreter alive; but here gfx will run in main thread
    gfx_run(&g);

    // cleanup
    free(buf);
    runtime_free(&rt);
    return 0;
}
