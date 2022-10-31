#include "Rversion.h"
#include "cpp11/doubles.hpp"
#include "cpp11/int64.hpp"
#include "cpp11/strings.hpp"

#include <testthat.h>
#include <cstdint>

context("integers-C++") {
  test_that("as_integers(doubles)") {
    // TYPEOF(x) == INTSXP
    cpp11::writable::int64s y;
    y.push_back(10.00);
    cpp11::writable::int64s i(cpp11::as_int64s(y));
    expect_true(i[0] == 10);
    expect_true(TYPEOF(i) == REALSXP);

    cpp11::writable::doubles x;
    x.push_back(10.01);
    expect_error(cpp11::as_int64s(x));

    cpp11::writable::strings e;
    e.push_back("a");
    e.push_back("b");
    expect_error(cpp11::as_int64s(e));

    cpp11::writable::doubles z;
    z.push_back(10);
    z.push_back(1000);
    z.push_back(100000);
    z.push_back(100000.00);

    cpp11::writable::int64s t((cpp11::as_int64s(z)));
    expect_true(t[0] == 10);
    expect_true(t[1] == 1000);
    expect_true(t[2] == 100000);
    expect_true(t[3] == 100000);
    expect_true(TYPEOF(t) == REALSXP);
  }

  test_that("int64s.push_back()") {
    cpp11::writable::int64s x;
    x.push_back(1);
    x.push_back(2);

    expect_true(x.size() == 2);
    expect_true(x[0] == 1);
    expect_true(x[1] == 2);
  }
  test_that("int64s.resize()") {
    cpp11::writable::int64s x;
    x.resize(2);
    x[0] = 1;
    x[1] = 2;

    expect_true(x.size() == 2);
    expect_true(x[0] == 1);
    expect_true(x[1] == 2);
  }
  test_that("int64s.at()") {
    cpp11::writable::int64s x;

    expect_error(x.at(-1));

    expect_error(x.at(0));

    x.push_back(1);
    expect_true(x.at(0) == 1);
    expect_error(x.at(1));
  }
  test_that("int64s.pop_back()") {
    cpp11::writable::int64s x;

    x.push_back(1);
    x.push_back(2);
    x.pop_back();

    expect_true(x.size() == 1);
    expect_true(x[0] == 1);

    expect_error(x.at(1));
  }
  test_that("int64s.insert()") {
    cpp11::writable::int64s x;

    x.insert(0, 1);
    x.insert(0, 2);
    x.insert(1, 3);
    expect_true(x.size() == 3);

    expect_true(x[0] == 2);
    expect_true(x[1] == 3);
    expect_true(x[2] == 1);
  }
  test_that("int64s.erase()") {
    cpp11::writable::int64s x;

    x.push_back(1);
    x.push_back(2);
    x.push_back(3);
    x.push_back(4);
    x.push_back(5);

    expect_true(x.size() == 5);

    x.erase(0);

    expect_true(x.size() == 4);
    expect_true(x[0] == 2);
    expect_true(x[1] == 3);
    expect_true(x[2] == 4);
    expect_true(x[3] == 5);

    x.erase(2);

    expect_true(x.size() == 3);
    expect_true(x[0] == 2);
    expect_true(x[1] == 3);
    expect_true(x[2] == 5);
  }
  test_that("int64s.iterator* = ") {
    cpp11::writable::int64s x;
    x.push_back(1);
    x.push_back(2);
    x.push_back(3);
    auto it = x.begin() + 1;
    *it = 3;
    ++it;
    *it = 4;

    expect_true(x.size() == 3);
    expect_true(x[0] == 1);
    expect_true(x[1] == 3);
    expect_true(x[2] == 4);
  }

  test_that("writable::int64s(SEXP)") {
    SEXP x = PROTECT(Rf_allocVector(REALSXP, 5));

    REAL(x)[0] = 1.0;
    REAL(x)[1] = 2.0;
    REAL(x)[2] = 3.0;
    REAL(x)[3] = 4.0;
    REAL(x)[4] = 5.0;

    cpp11::writable::int64s y(x);
    y[0] = std::bit_cast<int64_t>(-1.0);

    expect_true(x != y.data());

    expect_true(REAL(x)[0] == 1.0);
    expect_true(y[0] == std::bit_cast<int64_t>(-1.0));

    cpp11::writable::int64s z(y);
    z[0] = std::bit_cast<int64_t>(-2.0);

    expect_true(z.data() != y.data());

    expect_true(REAL(x)[0] == 1.0);
    expect_true(y[0] == std::bit_cast<int64_t>(-1.0));
    expect_true(z[0] == std::bit_cast<int64_t>(-2.0));

    UNPROTECT(1);
  }

#if defined(__APPLE__) && defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
  test_that("writable::int64s(ALTREP_SEXP)") {

    SEXP x = PROTECT(Rf_coerceVector(R_compact_intrange(1, 5), REALSXP));
    // Need to find (or create) an altrep class that implements duplicate.

    cpp11::writable::int64s y(x);
    y[0] = std::bit_cast<int64_t>(-1.0);

    expect_true(x != y.data());

    expect_true(REAL_ELT(x, 0) == std::bit_cast<1.0);
    expect_true(y[0] == std::bit_cast<int64_t>(-1.0));

    cpp11::writable::int64s z(y);
    z[0] = std::bit_cast<int64_t>(-2.0);

    expect_true(z.data() != y.data());

    expect_true(INTEGER_ELT(x, 0) == 1);
    expect_true(y[0] == std::bit_cast<int64_t>(-1.0));
    expect_true(z[0] == std::bit_cast<int64_t>(-2.0));

    z.push_back(std::bit_cast<int64_t>(6.0));
    expect_true(z[5] == std::bit_cast<int64_t>(6.0));

    UNPROTECT(1);
  }
#endif

  test_that("is_na(integer)") {
    int64_t x = 0;
    expect_true(!cpp11::is_na(x));

    int64_t y = std::bit_cast<int64_t>(NA_REAL);
    expect_true(cpp11::is_na(y));
  }
}
