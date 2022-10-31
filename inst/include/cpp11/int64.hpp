// cpp11 version: 0.4.2
// vendored on: 2022-03-29
#pragma once

#include <algorithm>         // for min
#include <array>             // for array
#include <cstdint>
#include <initializer_list>  // for initializer_list

#include "R_ext/Arith.h"        // for ISNA
#include "cpp11/R.hpp"          // for SEXP, SEXPREC, Rf_allocVector, REAL
#include "cpp11/as.hpp"         // for as_sexp
#include "cpp11/named_arg.hpp"  // for named_arg
#include "cpp11/protect.hpp"    // for SEXP, SEXPREC, REAL_ELT, R_Preserve...
#include "cpp11/r_vector.hpp"   // for vector, vector<>::proxy, vector<>::...
#include "cpp11/sexp.hpp"       // for sexp
#include <bit>
// Specializations for int64_ts

namespace cpp11 {

template <>
inline SEXP r_vector<int64_t>::valid_type(SEXP data) {
  if (data == nullptr) {
    throw type_error(REALSXP, NILSXP);
  }
  if (TYPEOF(data) != REALSXP) {
    throw type_error(REALSXP, TYPEOF(data));
  }
  return data;
}

template <>
inline int64_t r_vector<int64_t>::operator[](const R_xlen_t pos) const {
  // NOPROTECT: likely too costly to unwind protect every elt
  return std::bit_cast<int64_t>(is_altrep_ ? REAL_ELT(data_, pos) : data_p_[pos]);
}

template <>
inline int64_t* r_vector<int64_t>::get_p(bool is_altrep, SEXP data) {
  if (is_altrep) {
    return nullptr;
  } else {
    return std::bit_cast<int64_t*>(REAL(data));
  }
}

template <>
inline void r_vector<int64_t>::const_iterator::fill_buf(R_xlen_t pos) {
  length_ = std::min(64_xl, data_->size() - pos);
  auto tbuff = std::bit_cast<double*>(buf_.data());
  REAL_GET_REGION(data_->data_, pos, length_, tbuff);
  block_start_ = pos;
}

typedef r_vector<int64_t> int64s;

namespace writable {

template <>
inline typename r_vector<int64_t>::proxy& r_vector<int64_t>::proxy::operator=(
    const int64_t& rhs) {
  if (is_altrep_) {
    // NOPROTECT: likely too costly to unwind protect every set elt
    SET_REAL_ELT(data_, index_, std::bit_cast<double>(rhs));
  } else {
    *p_ = rhs;
  }
  return *this;
}

template <>
inline r_vector<int64_t>::proxy::operator int64_t() const {
  if (p_ == nullptr) {
    // NOPROTECT: likely too costly to unwind protect every elt
    return std::bit_cast<int64_t>(REAL_ELT(data_, index_));
  } else {
    return std::bit_cast<int64_t>(*p_);
  }
}

template <>
inline r_vector<int64_t>::r_vector(std::initializer_list<int64_t> il)
    : cpp11::r_vector<int64_t>(as_sexp(il)), capacity_(il.size()) {}

template <>
inline r_vector<int64_t>::r_vector(std::initializer_list<named_arg> il)
    : cpp11::r_vector<int64_t>(safe[Rf_allocVector](REALSXP, il.size())),
      capacity_(il.size()) {
  protect_ = preserved.insert(data_);
  int n_protected = 0;

  try {
    unwind_protect([&] {
      Rf_setAttrib(data_, R_NamesSymbol, Rf_allocVector(STRSXP, capacity_));
      SEXP names = PROTECT(Rf_getAttrib(data_, R_NamesSymbol));
      ++n_protected;
      auto it = il.begin();
      for (R_xlen_t i = 0; i < capacity_; ++i, ++it) {
        data_p_[i] = std::bit_cast<int64_t>(REAL_ELT(it->value(), 0));
        SET_STRING_ELT(names, i, Rf_mkCharCE(it->name(), CE_UTF8));
      }
      UNPROTECT(n_protected);
    });
  } catch (const unwind_exception& e) {
    preserved.release(protect_);
    UNPROTECT(n_protected);
    throw e;
  }
}

template <>
inline void r_vector<int64_t>::reserve(R_xlen_t new_capacity) {
  data_ = data_ == R_NilValue ? safe[Rf_allocVector](REALSXP, new_capacity)
                              : safe[Rf_xlengthgets](data_, new_capacity);
  SEXP old_protect = protect_;
  protect_ = preserved.insert(data_);
  preserved.release(old_protect);
  data_p_ = std::bit_cast<int64_t*>(REAL(data_));
  capacity_ = new_capacity;
}

template <>
inline void r_vector<int64_t>::push_back(int64_t value) {
  while (length_ >= capacity_) {
    reserve(capacity_ == 0 ? 1 : capacity_ *= 2);
  }
  if (is_altrep_) {
    SET_REAL_ELT(data_, length_, std::bit_cast<double>(value));
  } else {
    data_p_[length_] = std::bit_cast<double>(value);
  }
  ++length_;
}

typedef r_vector<int64_t> int64s;

}  // namespace writable

typedef r_vector<int64_t> int64s;

inline int64s as_int64s(sexp x) {
  if (TYPEOF(x) == REALSXP) {
    return as_cpp<int64s>(x);
  }

  else if (TYPEOF(x) == INTSXP) {
    int64s xn = as_cpp<int64s>(x);
    size_t len = xn.size();
    writable::int64s ret;
    for (size_t i = 0; i < len; ++i) {
      ret.push_back(static_cast<int64_t>(xn[i]));
    }
    return ret;
  }

  throw type_error(REALSXP, TYPEOF(x));
}

template <>
inline int64_t na() {
  return std::bit_cast<int64_t>(NA_REAL);
}

template <>
inline bool is_na(const int64_t& x) {
  return ISNA(std::bit_cast<double>(x));
}
}  // namespace cpp11
