#include "cell.h"

static Cell *syntax_pattern_match(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env, Cell *md);
static Cell *syntax_template_expand(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx);

static Cell *mk_closure_expr(Cell *ctx, Cell *expr, Cell *env) {
    Cell *c = closure_expr_new(ctx);
    closure_expr_expr(c) = expr;
    closure_expr_env(c) = env;
    return c;
}

static Cell *mk_literal_matcher(Cell *ctx, Cell *name, Cell *env) {
    Cell *c = matcher_new(ctx);
    matcher_type(c) = MatcherLiteral;
    matcher_name(c) = name;
    matcher_value(c) = env;
    return c;
}

static Cell *mk_variable_matcher(Cell *ctx, Cell *name) {
    Cell *c = matcher_new(ctx);
    matcher_type(c) = MatcherVariable;
    matcher_name(c) = name;
    return c;
}

static Cell *mk_rest_matcher(Cell *ctx, Cell *mt) {
    Cell *c = matcher_new(ctx);
    matcher_type(c) = MatcherRest;
    matcher_value(c) = mt;
    return c;
}

static Cell *mk_underscore_matcher(Cell *ctx, Cell *name) {
    Cell *c = matcher_new(ctx);
    matcher_type(c) = MatcherUnderscore;
    matcher_name(c) = name;
    return c;
}

static Cell *mk_constant_matcher(Cell *ctx, Cell *value) {
    Cell *c = matcher_new(ctx);
    matcher_type(c) = MatcherConstant;
    matcher_value(c) = value;
    return c;
}

static Cell *mk_sequence_matcher(Cell *ctx) {
    Cell *c = matcher_new(ctx);
    matcher_type(c) = MatcherSequence;
    matcher_value(c) = NULL;
    return c;
}

static Cell *mk_constant_expander(Cell *ctx, Cell *v) {
    Cell *c = expander_new(ctx);
    expander_type(c) = ExpanderConstant;
    expander_n(c) = 0;
    expander_value(c) = v;
    return c;
}

static Cell *mk_variable_expander(Cell *ctx, Cell *n) {
    Cell *c = expander_new(ctx);
    expander_type(c) = ExpanderVariable;
    expander_n(c) = 0;
    expander_name(c) = n;
    return c;
}

static Cell *mk_sequence_expander(Cell *ctx) {
    Cell *c = expander_new(ctx);
    expander_type(c) = ExpanderSequence;
    return c;
}

static Cell *sequence_matcher_add(Cell *ctx, Cell *seq, Cell *sub) {
    if (matcher_value(seq) == NULL) {
        matcher_value(seq) = cons(ctx, sub, CELL_NIL);
        return CELL_TRUE;
    }
    list_add(ctx, matcher_value(seq), sub);
    return CELL_TRUE;
}

static Cell *sequence_expander_add(Cell *ctx, Cell *seq, Cell *sub) {
    if (expander_name(seq) == NULL) {
        if (expander_type(sub) == ExpanderVariable)
            expander_name(seq) = cons(ctx, expander_name(sub), CELL_NIL);
        else if (expander_type(sub) == ExpanderSequence)
            expander_name(seq) = expander_name(sub);
    } else {
        if (expander_type(sub) == ExpanderVariable)
            list_add(ctx, expander_name(seq), expander_name(sub));
        else if (expander_type(sub) == ExpanderSequence)
            list_extend(ctx, expander_name(seq), expander_name(sub));
    }

    if (expander_value(seq) == NULL) {
        expander_value(seq) = cons(ctx, sub, CELL_NIL);
    } else {
        list_add(ctx, expander_value(seq), sub);
    }
}

static bool syntax_pattern_match_literal(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env) {
    if (!is_symbol(expr)) {
        return FALSE;
    }
    Cell *c1 = find_env(expr_env, expr);
    Cell *c2 = find_env(matcher_value(mt), matcher_name(mt));
    if (c1 == c2) {
        return TRUE;
    }
    return FALSE;
}

static Cell *syntax_pattern_match_sequence(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env, Cell *md) {
    for (Cell *ls=matcher_value(mt); is_pair(ls); ls=cdr(ls)) {
        expr = syntax_pattern_match(ctx, car(ls), expr, expr_env, md);
        if (expr == CELL_ERR) {
            break;
        }
    }
    return expr;
}

Cell *syntax_pattern_match(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env, Cell *md) {
    switch (matcher_type(mt)) {
    case MatcherLiteral:
        if (matcher_repeat(mt)) {
            while (is_pair(expr)) {
                if (!syntax_pattern_match_literal(ctx, mt, car(expr), expr_env)) break;
                expr = cdr(expr);
            }
            return expr;
        }
        if (!syntax_pattern_match_literal(ctx, mt, car(expr), expr_env)) {
            return CELL_ERR;
        }
        return cdr(expr);
    case MatcherConstant:
        if (!is_pair(expr) || !equal(car(expr), matcher_value(mt))){
            return CELL_ERR;
        }
        if (matcher_repeat(mt)) {
            expr = cdr(expr);
            while (is_pair(expr) && equal(car(expr), matcher_value(mt)))
                expr = cdr(expr);
            return expr;
        }
        return cdr(expr);
    case MatcherVariable: {
        gc_var2(rt, tmp);
        gc_preserve2(ctx, rt, tmp);
        if (matcher_repeat(mt)) {
            if (!is_pair(expr)) {
                gc_release(ctx);
                return expr;
            }
            rt = cons(ctx, CELL_NIL, CELL_NIL);
            while (is_pair(expr)) {
                list_add(ctx, rt, car(expr));
                expr = cdr(expr);
            }
            rt = cdr(rt);
        } else {
            if (!is_pair(expr)) {
                gc_release(ctx);
                return CELL_ERR;
            }
            rt = car(expr);
            expr = cdr(expr);
            if (is_eof(rt)) {
                gc_release(ctx);
                return expr;
            }
        }
        alist_append(ctx, md, tmp = cons(ctx, matcher_name(mt), rt));
        gc_release(ctx);
        return expr;
    }
    case MatcherUnderscore:
        if (matcher_repeat(mt)) {
            while (is_pair(expr)) expr = cdr(expr);
        } else {
            if (!is_pair(expr)) {
                return CELL_ERR;
            }
            expr = cdr(expr);
        }
        return expr;
    case MatcherRest: {
        gc_var1(tmp);
        gc_preserve1(ctx, tmp);
        if (is_nil(expr)) {
            expr = CELL_EOF;
        }
        tmp = syntax_pattern_match(ctx, matcher_value(mt), tmp = cons(ctx, expr, CELL_NIL), expr_env, md);
        gc_release(ctx);
        return tmp;
    }
    case MatcherSequence:
        if (matcher_repeat(mt)) {
            gc_var1(tmp_md);
            tmp_md = cons(ctx, CELL_NIL, CELL_NIL);
            gc_preserve1(ctx, tmp_md);
            while (is_pair(expr)) {
                if (syntax_pattern_match_sequence(ctx, mt, car(expr), expr_env, tmp_md) != CELL_NIL) break;
                expr = cdr(expr);
            }
            alist_update(ctx, md, cdr(tmp_md));
            gc_release(ctx);
            return expr;
        }
        if (!is_pair(expr)) {
            return CELL_ERR;
        }
        if (syntax_pattern_match_sequence(ctx, mt, car(expr), expr_env, md) != CELL_NIL) {
            return CELL_ERR;
        }
        return cdr(expr);
    }
    return CELL_ERR;
}

static Cell *_variable_expand(Cell *ctx, Cell *v) {
    gc_var1(ret);
    gc_preserve1(ctx, ret);
    ret = cons(ctx, CELL_NIL, CELL_NIL);
    for (; is_pair(v); v=cdr(v)) {
        list_add(ctx, ret, car(v));
    }
    gc_release(ctx);
    return cdr(ret);
}

static inline bool is_internal_symbol(Cell *ctx, Cell *c) {
    if (is_quote(ctx, c) || is_qquote(ctx, c) || is_unquote(ctx, c) ||
            is_unquotes(ctx, c)) {
        return true;
    }
    return false;
}

static Cell *_sequence_expand0(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx) {
    gc_var2(ret, c);
    gc_preserve2(ctx, ret, c);
    ret = cons(ctx, CELL_NIL, CELL_NIL);
    for (Cell *ls=expander_value(expd), *t; is_pair(ls); ls=cdr(ls)) {
        t = car(ls);
        c = syntax_template_expand(ctx, t, md, env, idx);
        if (!is_error(c) && !is_eof(c)) {
            if (expander_n(t) > 0) {
                if (is_closure_expr(c) && is_pair(closure_expr_expr(c))) {
                    Cell *env = closure_expr_env(c);
                    c = closure_expr_expr(c);
                    for (Cell *ls=c, *t; is_pair(ls); ls=cdr(ls)) {
                        t = car(ls);
                        if (is_pair(t) || (is_symbol(t) && !is_internal_symbol(ctx, t))) {
                            rplaca(ls, mk_closure_expr(ctx, t, env));
                        }
                    }
                }
                list_extend(ctx, ret, c);
            } else {
                list_add(ctx, ret, c);
            }
        }
    }
    gc_release(ctx);
    return cdr(ret);
}

static Cell *_sequence_expand(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx, int expd_n) {
    if (expd_n == 0) return _sequence_expand0(ctx, expd, md, env, idx);
    int len=0;
    for (Cell *c=expander_name(expd); is_pair(c); c=cdr(c)) {
        Cell *var = assq(car(c), md);
        if (is_nil(var)) continue;
        var = cdr(var);
        for (Cell *ls=cdr(idx); is_pair(var) && is_pair(ls) && !is_nil(car(ls)); ls=cdr(ls)) {
            if (is_pair(var)) {
                var = list_ref(var, car(ls));
            }
        }
        if (is_pair(var)) {
            if (len == 0) {
                len = length(var);
            } else if (length(var) < len) {
                len = length(var);
            }
        } else if (len == 0) {
            len = 1;
        }
    }
    if (len > 0) {
        gc_var3(ret, v1, v2);
        gc_preserve3(ctx, ret, v1, v2);
        list_add(ctx, idx, v1 = mk_long(ctx, 0));
        ret = cons(ctx, CELL_NIL, CELL_NIL);
        for (int i=0; i<len; i++) {
            list_set(cdr(idx), v1 = mk_long(ctx, length(cdr(idx)) - 1), v2 = mk_long(ctx, i));
            v1 = _sequence_expand(ctx, expd, md, env, idx, expd_n - 1);
            if (!is_eof(v1)) {
                list_add(ctx, ret, v1);
            }
        }
        list_pop(idx);
        gc_release(ctx);
        return cdr(ret);
    }
    return CELL_EOF;
}

static Cell *_transform_closure_expr(Cell *ctx, Cell *expr, Cell *env) {
    if (is_pair(expr) || (is_symbol(expr) && !is_internal_symbol(ctx, expr))) {
        return mk_closure_expr(ctx, expr, env);
    }
    return expr;
}

static Cell *syntax_template_expand(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx) {
    switch (expander_type(expd)) {
    case ExpanderConstant:
        return expander_value(expd);
    case ExpanderVariable: {
        Cell *var = assq(expander_name(expd), md);
        if (is_nil(var)) return CELL_EOF;
        var = cdr(var);
        for (Cell *ls=cdr(idx); is_pair(ls) && !is_nil(car(ls)) && is_pair(var); ls=cdr(ls)) {
            var = list_ref(var, car(ls));
        }
        uint n = expander_n(expd);
        while (n > 0) {
            var = _variable_expand(ctx, var);
            if (var == CELL_ERR) {
                return mk_exception(ctx, SyntaxError, mk_string(ctx, "too many ellipsis for variable:"), expander_name(expd), NULL);
            }
            --n;
        }
        return _transform_closure_expr(ctx, var, env);
    }
    case ExpanderSequence:
        return _sequence_expand(ctx, expd, md, env, idx, expander_n(expd));
    }
    return CELL_ERR;
}

static Cell *_syntax_pattern_analyze(Cell *ctx, Cell *lit, Cell *pat, Cell *syn_env, Cell *pat_vars) {
    if (is_pair(pat)) {
        bool ellipsis = FALSE;
        gc_var2(mt, sub);
        gc_preserve2(ctx, mt, sub);
        mt = mk_sequence_matcher(ctx);
        while (is_pair(pat)) {
            sub = _syntax_pattern_analyze(ctx, lit, car(pat), syn_env, pat_vars);
            if (is_exception(sub)) return sub;
            sequence_matcher_add(ctx, mt, sub);
            pat = cdr(pat);
            if (is_pair(pat) && is_ellipsis(car(pat))) {
                if (ellipsis == TRUE) return mk_exception(ctx, SyntaxError, mk_string(ctx, "misplaced ellipsis in pattern"), NULL, NULL);
                ellipsis = TRUE;
                matcher_repeat(sub) = TRUE;
                pat = cdr(pat);
            }
        }
        if (!is_nil(pat)) {
            sub = _syntax_pattern_analyze(ctx, lit, pat, syn_env, pat_vars);
            sequence_matcher_add(ctx, mt, sub = mk_rest_matcher(ctx, sub));
        }
        gc_release(ctx);
        return mt;
    } else if (is_symbol(pat)) {
        if (is_contains(lit, pat)) {
            return mk_literal_matcher(ctx, pat, syn_env);
        } else if (!strcmp(symbol_data(pat), "_")) {
            return mk_underscore_matcher(ctx, pat);
        } else if (is_contains(pat_vars, pat)) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "duplicated pattern variable:"), pat, NULL);
        }
        list_add(ctx, pat_vars, pat);
        return mk_variable_matcher(ctx, pat);
    }
    return mk_constant_matcher(ctx, pat);
}

static Cell *syntax_pattern_analyze(Cell *ctx, Cell *lit, Cell *pattern, Cell *syn_env, Cell *pat_vars) {
    if (!is_pair(pattern) || !is_symbol(car(pattern))) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax pattern format:"), pattern, NULL);
    }
    return _syntax_pattern_analyze(ctx, lit, cdr(pattern), syn_env, pat_vars);
}

static Cell *syntax_template_analyze(Cell *ctx, Cell *tmpl, Cell *pat_vars) {
    if (is_pair(tmpl)) {
        gc_var2(seq, sub);
        gc_preserve2(ctx, seq, sub);
        seq = mk_sequence_expander(ctx);
        while (is_pair(tmpl)) {
            sub = syntax_template_analyze(ctx, car(tmpl), pat_vars);
            tmpl = cdr(tmpl);
            while (is_pair(tmpl) && is_ellipsis(car(tmpl))) {
                expander_n(sub) += 1;
                tmpl = cdr(tmpl);
            }
            sequence_expander_add(ctx, seq, sub);
        }
        if (!is_nil(tmpl)) {
            sub = syntax_template_analyze(ctx, tmpl, pat_vars);
            sequence_expander_add(ctx, seq, sub);
        }
        gc_release(ctx);
        return seq;
    } else if (is_symbol(tmpl)) {
        if (is_contains(pat_vars, tmpl)) {
            return mk_variable_expander(ctx, tmpl);
        }
    }
    return mk_constant_expander(ctx, tmpl);
}

Cell *macro_analyze(Cell *ctx, Cell *lit, Cell *matches, Cell *syn_env) {
    if (!is_pair(matches)) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax format in syntax rules:"), matches, NULL);
    }

    Cell *t1=NULL, *t2=NULL, *t3=NULL, *t4=NULL;
    gc_var6(machers, pat_vars, pattern, tmpl, t5, t6);
    gc_preserve4(ctx, machers, pat_vars, pattern, tmpl);
    machers = cons(ctx, syn_env, CELL_NIL);
    for (Cell *ls=matches, *macher; is_pair(ls); ls=cdr(ls)) {
        pat_vars = cons(ctx, CELL_NIL, CELL_NIL);
        macher = car(ls);
        if (!is_pair(macher) || !is_pair(car(macher)) || !is_pair(cdr(macher)) || !is_nil(cddr(macher))) {
            gc_release(ctx);
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax format in syntax rules:"), macher, NULL);
        }
        pattern = syntax_pattern_analyze(ctx, lit, car(macher), syn_env, pat_vars);
        if (is_exception(pattern)) {
            gc_release(ctx);
            return pattern;
        }
        tmpl = syntax_template_analyze(ctx, cadr(macher), cdr(pat_vars));
        if (is_exception(tmpl)) {
            gc_release(ctx);
            return tmpl;
        }
        macher = cons(ctx, pattern, tmpl);
        list_add(ctx, machers, macher);
    }
    gc_release(ctx);
    gc_preserve6(ctx, machers, t1, t2, t3, t4, t5);
    t6 = mk_proc(ctx, CELL_NIL, t1=mk_closure(ctx, t2=cons(ctx, CELL_NIL, t3=cons(ctx, t4=cons(ctx, ctx_quote(ctx), t5=cons(ctx, machers, CELL_NIL)), CELL_NIL)), syn_env));
    gc_release(ctx);
    return t6;
}

Cell *macro_transform(Cell *ctx, Cell *machers, Cell *syn_env, Cell *expr, Cell *expr_env) {
    Cell *tmpl = NULL;
    gc_var2(md, tmp);
    gc_preserve2(ctx, md, tmp);
    Cell *ls = machers;
    for (; is_pair(ls); ls=cdr(ls)) {
        md = cons(ctx, CELL_NIL, CELL_NIL);
        Cell *rt = syntax_pattern_match(ctx, caar(ls), tmp = cons(ctx, expr, CELL_NIL), expr_env, md);
        if (rt == CELL_NIL) {
            break;
        }
    }
    if (is_pair(ls) && is_pair(car(ls))) tmpl = cdar(ls);
    if (!tmpl) {
        gc_release(ctx);
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "unmatched pattern"), NULL, NULL);
    }
    #ifdef MACRO_DEBUG
    write_string(ctx, ctx_stdoutport(ctx), "\n*Macro match dict*\n");
    print_cell(ctx, ctx_stdoutport(ctx), cdr(md));
    write_char(ctx, ctx_stdoutport(ctx), '\n');
    #endif
    tmpl = syntax_template_expand(ctx, tmpl, cdr(md), expr_env, tmp = cons(ctx, CELL_NIL, CELL_NIL));
    gc_release(ctx);
    return tmpl;
}
