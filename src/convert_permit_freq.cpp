#include <fstream>
#include "Kmer.h"
#include "Rcpp.h"
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]
//
// [[Rcpp::export]]
List cpp_get_permit_freq_info(String fn) {

    // open the binary file in C++
    // NOTE: this function does *not* check if the file exists
    // that means it should be be wrapped in R in an appropriate
    // caller that does the relevant checks.
    const char* c_fn = fn.get_cstring();
    std::ifstream ifile(c_fn, std::ios::binary);

    uint64_t ver;
    ifile.read(reinterpret_cast<char*>(&ver), sizeof(ver));

    uint64_t bc_len;
    ifile.read(reinterpret_cast<char*>(&bc_len), sizeof(bc_len));

    uint64_t num_elem;
    ifile.read(reinterpret_cast<char*>(&num_elem), sizeof(num_elem));

    StringVector bc(num_elem);
    NumericVector counts(num_elem);

    uint64_t bc_integer;
    uint64_t bc_count;

    combinelib::kmers::Kmer<32, 0> k;
    k.k(static_cast<uint16_t>(bc_len));

    for (size_t i = 0; i < num_elem; ++i) {
        ifile.read(reinterpret_cast<char*>(&bc_integer), sizeof(bc_integer));
        k.word__(0) = bc_integer;
        ifile.read(reinterpret_cast<char*>(&bc_count), sizeof(bc_count));

        bc[i] = String(k.toStr());
        counts[i] = static_cast<uint32_t>(bc_count);
    }

    List L = List::create(bc, counts);
    return L;
}
