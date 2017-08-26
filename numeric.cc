#include <math.h>
#include "cell.h"

Cell *mk_long(Cell *ctx, long l) {
    Cell *num = number_new(ctx);
    number_type(num) = NUMBER_LONG;
    number_long(num) = l;
    return num;
}

Cell *mk_double(Cell *ctx, double d) {
    Cell *num = number_new(ctx);
    number_type(num) = NUMBER_DOUBLE;
    number_double(num) = d;
    return num;
}

long num_gcd(long bg, long sm)
{
    if (bg < 0) bg = -bg;
    if (sm < 0) sm = -sm;
    if (bg < sm)
    {
        return num_gcd(sm, bg);
    }

    if(sm == 0) return bg;
    long res = bg % sm;
    while (res != 0)
    {
        bg = sm;
        sm = res ;
        res = bg % sm;
    }
    return sm;
}

Cell *mk_fraction(Cell *ctx, long nr, long dr) {
    char s = 1;
    long gcd = num_gcd(nr, dr);
    gc_var1(num);
    gc_preserve1(ctx, num);
    num = number_new(ctx);
    if (dr < 0) s = -1;
    number_type(num) = NUMBER_FRACTION;
    number_fn_nr(num) = number_new(ctx);
    number_type(number_fn_nr(num)) = NUMBER_LONG;
    number_long(number_fn_nr(num)) = s * nr / gcd;

    number_fn_dr(num) = number_new(ctx);
    number_type(number_fn_dr(num)) = NUMBER_LONG;
    number_long(number_fn_dr(num)) = s * dr / gcd;
    gc_release(ctx);
    return num;
}

Cell *mk_complex(Cell *ctx, Cell *rl, Cell *im) {
    Cell *num = number_new(ctx);
    number_type(num) = NUMBER_COMPLEX;
    number_cx_rl(num) = rl;
    number_cx_im(num) = im;
    return num;
}

Cell *num_etoi(Cell *ctx, Cell *num) {
    switch (number_type(num)) {
    case NUMBER_LONG:
        number_type(num) = NUMBER_DOUBLE;
        number_double(num) = number_long(num);
        break;
    case NUMBER_DOUBLE:
        return num;
    case NUMBER_FRACTION:
    {
        number_type(num) = NUMBER_DOUBLE;
        Cell *nr = number_fn_nr(num);
        Cell *dr = number_fn_dr(num);
        number_double(num) = (double)number_long(nr) / (double)number_long(dr);
        break;
    }
    case NUMBER_COMPLEX:
        num_etoi(ctx, number_cx_rl(num));
        num_etoi(ctx, number_cx_im(num));
        break;
    }
    return num;
}

Cell *num_itoe(Cell *ctx, Cell *num) {
    switch (number_type(num)) {
    case NUMBER_LONG:
        return num;
    case NUMBER_DOUBLE:
    {
        int n = 0;
        double decimal = 0.0, inter = 0;
        number_type(num) = NUMBER_FRACTION;
        decimal = modf(number_double(num), &inter);
        if (decimal > 0.0) {
            for(;;) {
                decimal *= 10;
                ++n;
                if (decimal - (long)decimal < 0.000001) {
                    break;
                }
            }
        }
        long bg = pow(10, n);
        long divisor = num_gcd(bg, decimal);
        number_fn_dr(num) = number_new(ctx);
        number_type(number_fn_dr(num)) = NUMBER_LONG;
        number_long(number_fn_dr(num)) = bg / divisor;

        number_fn_nr(num) = number_new(ctx);
        number_type(number_fn_nr(num)) = NUMBER_LONG;
        number_long(number_fn_nr(num)) = decimal / divisor + number_long(number_fn_dr(num)) * inter;
        break;
    }
    case NUMBER_FRACTION:
        return num;
    case NUMBER_COMPLEX:
        number_cx_rl(num) = num_itoe(ctx, number_cx_rl(num));
        number_cx_im(num) = num_itoe(ctx, number_cx_im(num));
        break;
    }
    return num;
}

bool num_equal(Cell *a, Cell *b) {
    if (!a || !b) return FALSE;
    if (number_type(a) != number_type(b)) return FALSE;
    switch (number_type(a)) {
    case NUMBER_LONG:
        return number_long(a) == number_long(b);
    case NUMBER_DOUBLE:
    {
        double sub = number_double(a) - number_double(b);
        return sub > -0.000001 && sub < 0.000001;
    }
    case NUMBER_FRACTION:
        return (number_long(number_fn_nr(a)) == number_long(number_fn_nr(b)) && number_long(number_fn_dr(a)) == number_long(number_fn_dr(b)));
    case NUMBER_COMPLEX:
        return num_equal(number_cx_rl(a), number_cx_rl(b)) && num_equal(number_cx_im(a), number_cx_im(b));
    }
    return FALSE;
}

double num_real_compare(Cell *a, Cell *b) {
    double d1=0, d2=0;
    switch (number_type(a)) {
    case NUMBER_LONG:
        d1 = number_long(a);
        break;
    case NUMBER_DOUBLE:
        d1 = number_double(a);
        break;
    case NUMBER_FRACTION:
        d1 = (double)number_long(number_fn_nr(a)) / (double)number_long(number_fn_dr(a));
        break;
    }
    switch (number_type(b)) {
    case NUMBER_LONG:
        d2 = number_long(b);
        break;
    case NUMBER_DOUBLE:
        d2 = number_double(b);
        break;
    case NUMBER_FRACTION:
        d2 = number_long(number_fn_nr(b)) / number_long(number_fn_dr(b));
        break;
    }
    return d1 - d2;
}

static Cell *_num_calcu(Cell *ctx, int op, Cell *a, Cell *b) {
    gc_var7(num, rl, im, nr1, dr1, v1, v2);
    gc_preserve7(ctx, num, rl, im, nr1, dr1, v1, v2);
    long nr, dr, gcd;
    switch (number_type(a)) {
    case NUMBER_LONG:
        switch (number_type(b)) {
        case NUMBER_LONG:
            switch (op) {
            case OP_ADD:
                num = mk_long(ctx, number_long(a) + number_long(b));
                break;
            case OP_SUB:
                num = mk_long(ctx, number_long(a) - number_long(b));
                break;
            case OP_MULTI:
                num = mk_long(ctx, number_long(a) * number_long(b));
                break;
            case OP_DIV:
            {
                gcd = num_gcd(number_long(a), number_long(b));
                if (gcd == number_long(b)) {
                    num = mk_long(ctx, number_long(a) / gcd);
                } else {
                    num = mk_fraction(ctx, number_long(a), number_long(b));
                }
                break;
            }}
            break;
        case NUMBER_DOUBLE:
            num = _num_calcu(ctx, op, v1 = num_etoi(ctx, a), b);
            break;
        case NUMBER_FRACTION:
        {
            nr = number_long(number_fn_nr(b));
            dr = number_long(number_fn_dr(b));
            switch (op) {
            case OP_ADD:
                nr = nr + number_long(a) * dr;
                num = mk_fraction(ctx, nr, dr);
                break;
            case OP_SUB:
                nr = number_long(a) * dr - nr;
                num = mk_fraction(ctx, nr, dr);
                break;
            case OP_MULTI:
            {
                nr = number_long(a) * nr;
                gcd = num_gcd(number_long(a) * number_long(number_fn_nr(b)), dr);
                if (gcd == dr)
                    num = mk_long(ctx, nr / dr);
                else
                    num = mk_fraction(ctx, nr, dr);
                break;
            }
            case OP_DIV:
            {
                nr = number_long(a) * dr;
                dr = number_long(number_fn_nr(b));
                gcd = num_gcd(nr, dr);
                if (gcd == nr)
                    num = mk_long(ctx, nr / dr);
                else
                    num = mk_fraction(ctx, nr, dr);
                break;
            }}
            break;
        }
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, a, number_cx_rl(b)), number_cx_im(b));
                break;
            case OP_MULTI:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, a, number_cx_rl(b)), v2 = _num_calcu(ctx, op, a, number_cx_im(b)));
                break;
            case OP_DIV:
            {
                nr1 = _num_calcu(ctx, OP_MULTI, a, number_cx_rl(b));
                dr1 = _num_calcu(ctx, OP_ADD, v1 = _num_calcu(ctx, OP_MULTI, number_cx_rl(b), number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, number_cx_im(b), number_cx_im(b)));
                rl = _num_calcu(ctx, OP_DIV, nr1, dr1);
                
                nr1 = _num_calcu(ctx, OP_MULTI, v1 = mk_long(ctx, -1), v2 = _num_calcu(ctx, OP_MULTI, a, number_cx_im(b)));
                im = _num_calcu(ctx, OP_DIV, nr1, dr1);
                num = mk_complex(ctx, rl, im);
                break;
            }}
            break;
        }
        break;
    case NUMBER_DOUBLE:
        switch (number_type(b)) {
        case NUMBER_LONG:
        case NUMBER_FRACTION:
            num = _num_calcu(ctx, op, a, v1 = num_etoi(ctx, b));
            break;
        case NUMBER_DOUBLE:
            switch (op) {
            case OP_ADD:
                num = mk_double(ctx, number_double(a) + number_double(b));
                break;
            case OP_SUB:
                num = mk_double(ctx, number_double(a) - number_double(b));
                break;
            case OP_MULTI:
                num = mk_double(ctx, number_double(a) * number_double(b));
                break;
            case OP_DIV:
                num = mk_double(ctx, number_double(a) / number_double(b));
            }
            break;
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, a, number_cx_rl(b)), number_cx_im(b));
                break;
            case OP_MULTI:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, a, number_cx_rl(b)), v2 = _num_calcu(ctx, op, a, number_cx_im(b)));
                break;
            case OP_DIV:
            {
                double dr = pow(number_double(number_cx_rl(b)), 2) + pow(number_double(number_cx_im(b)), 2);
                num = mk_complex(ctx, v1 = mk_double(ctx, number_double(a) * number_double(number_cx_rl(b)) / dr), v2 = mk_double(ctx, -1 * number_double(a) * number_double(number_cx_im(b)) / dr));
                break;
            }}
            break;
        }
        break;
    case NUMBER_FRACTION:
        switch (number_type(b)) {
        case NUMBER_LONG:
            switch (op) {
            case OP_ADD:
            case OP_MULTI:
                num = _num_calcu(ctx, op, b, a);
                break;
            case OP_SUB:
                nr = number_long(number_fn_nr(a));
                dr = number_long(number_fn_dr(a));
                nr = nr - number_long(b) * dr;
                num = mk_fraction(ctx, nr, dr);
                break;
            case OP_DIV:
                nr = number_long(number_fn_nr(a));
                dr = number_long(number_fn_nr(a)) * number_long(b);
                gcd = num_gcd(nr, dr);
                if (gcd == nr)
                    num = mk_long(ctx, nr / dr);
                else
                    num = mk_fraction(ctx, nr, dr);
                break;
            }
            break;
        case NUMBER_DOUBLE:
            num = _num_calcu(ctx, op, v1 = num_etoi(ctx, a), b);
            break;
        case NUMBER_FRACTION:
        {
            switch (op) {
            case OP_ADD:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_dr(b)) + number_long(number_fn_dr(a)) * number_long(number_fn_nr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_dr(b));
                break;
            case OP_SUB:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_dr(b)) - number_long(number_fn_dr(a)) * number_long(number_fn_nr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_dr(b));
                break;
            case OP_MULTI:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_nr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_dr(b));
                break;
            case OP_DIV:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_dr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_nr(b));
                break;
            }
            gcd = num_gcd(nr, dr);
            if (gcd == dr)
                num = mk_long(ctx, nr / dr);
            else
                num = mk_fraction(ctx, nr, dr);
            break;
        }
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, a, number_cx_rl(b)), number_cx_im(b));
                break;
            case OP_MULTI:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, OP_MULTI, a, number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, a, number_cx_im(b)));
                break;
            case OP_DIV:
            {
                nr1 = _num_calcu(ctx, OP_MULTI, a, number_cx_rl(b));
                dr1 = _num_calcu(ctx, OP_ADD, v1 = _num_calcu(ctx, OP_MULTI, number_cx_rl(b), number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, number_cx_im(b), number_cx_im(b)));
                rl = _num_calcu(ctx, OP_DIV, nr1, dr1);
                
                nr1 = _num_calcu(ctx, OP_MULTI, v1 = mk_long(ctx, -1), v2 = _num_calcu(ctx, OP_MULTI, a, number_cx_im(b)));
                im = _num_calcu(ctx, OP_DIV, nr1, dr1);
                num = mk_complex(ctx, rl, im);
                break;
            }}
        }
        break;
    case NUMBER_COMPLEX:
        switch (number_type(b)) {
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, number_cx_rl(a), number_cx_rl(b)),
                                      v2 = _num_calcu(ctx, op, number_cx_im(a), number_cx_im(b)));
                break;
            case OP_MULTI:
                rl = _num_calcu(ctx, OP_SUB, v1 = _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_im(b)));
                im = _num_calcu(ctx, OP_ADD, v1 = _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_im(b)));
                num = mk_complex(ctx, rl, im);
                break;
            case OP_DIV:
            {
                nr1 = _num_calcu(ctx, OP_ADD, v1 = _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_im(b)));
                dr1 = _num_calcu(ctx, OP_ADD, v1 = _num_calcu(ctx, OP_MULTI, number_cx_rl(b), number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, number_cx_im(b), number_cx_im(b)));
                rl = _num_calcu(ctx, OP_DIV, nr1, dr1);

                nr1 = _num_calcu(ctx, OP_SUB, v1 = _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_rl(b)), v2 = _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_im(b)));
                im = _num_calcu(ctx, OP_DIV, nr1, dr1);
                num = mk_complex(ctx, rl, im);
                break;
            }}
            break;
        default:
            switch (op) {
            case OP_ADD:
            case OP_MULTI:
                num = _num_calcu(ctx, op, b, a);
                break;
            case OP_SUB:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, number_cx_rl(a), b), number_cx_im(a));
                break;
            case OP_DIV:
                num = mk_complex(ctx, v1 = _num_calcu(ctx, op, number_cx_rl(a), b), v2 = _num_calcu(ctx, op, number_cx_im(a), b));
                break;
            }
        }
        break;
    }
    gc_release(ctx);
    return num;
}

Cell *num_calcu(Cell *ctx, int op, Cell *a, Cell *b) {
    gc_var2(c1, c2);
    gc_preserve2(ctx, c1, c2);
    c1 = a;
    c2 = b;
    if (number_type(a) == NUMBER_DOUBLE ||
        (number_type(a) == NUMBER_COMPLEX && 
            (number_type(number_cx_rl(a)) == NUMBER_DOUBLE ||
             number_type(number_cx_im(a)) == NUMBER_DOUBLE))) {
        if (number_type(b) == NUMBER_LONG || number_type(b) == NUMBER_FRACTION ||
            (number_type(b) == NUMBER_COMPLEX && number_type(number_cx_rl(b)) != NUMBER_DOUBLE))
        c2 = num_etoi(ctx, b);
    } else {
        if (number_type(b) == NUMBER_DOUBLE ||
            (number_type(b) == NUMBER_COMPLEX && 
                (number_type(number_cx_rl(b)) == NUMBER_DOUBLE ||
                 number_type(number_cx_im(b)) == NUMBER_DOUBLE))) {
            c1 = num_etoi(ctx, a);
        }
    }
    c1 = _num_calcu(ctx, op, c1, c2);
    gc_release(ctx);
    return c1;
}
