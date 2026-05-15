#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <cmath>
#include <numeric>
#include <climits>
#include <cctype>

using namespace Rcpp;

// ============================================================
// String distance helpers — native C++ implementations that
// replicate the behaviour of stringdist::stringdist() for
// method = "jw"  (p = 0.1, matching stringdist default)
//         "soundex"
//         "cosine" (q = 1, character unigrams)
// ============================================================

// Jaro similarity in [0, 1]
static double jaro_sim(const std::string& s1, const std::string& s2) {
    int n1 = (int)s1.size();
    int n2 = (int)s2.size();

    if (n1 == 0 && n2 == 0) return 1.0;
    if (n1 == 0 || n2 == 0) return 0.0;

    int win = std::max(n1, n2) / 2 - 1;
    if (win < 0) win = 0;

    std::vector<bool> m1(n1, false), m2(n2, false);
    int m = 0;

    for (int i = 0; i < n1; ++i) {
        int lo = std::max(0, i - win);
        int hi = std::min(n2 - 1, i + win);
        for (int j = lo; j <= hi; ++j) {
            if (!m2[j] && s1[i] == s2[j]) {
                m1[i] = m2[j] = true;
                ++m;
                break;
            }
        }
    }

    if (m == 0) return 0.0;

    int t = 0, k = 0;
    for (int i = 0; i < n1; ++i) {
        if (!m1[i]) continue;
        while (!m2[k]) ++k;
        if (s1[i] != s2[k]) ++t;
        ++k;
    }

    return ((double)m / n1 + (double)m / n2 + (double)(m - t / 2) / m) / 3.0;
}

// Jaro-Winkler distance in [0, 1] (p = 0.1 — matches stringdist default)
static double jaro_winkler_dist(const std::string& s1, const std::string& s2,
                                double p = 0.1) {
    if (s1.empty() && s2.empty()) return 0.0;
    if (s1.empty() || s2.empty()) return 1.0;

    double js = jaro_sim(s1, s2);

    // common prefix length, capped at 4
    int l = 0;
    int cap = (int)std::min({ s1.size(), s2.size(), (size_t)4 });
    for (int i = 0; i < cap; ++i) {
        if (s1[i] == s2[i]) ++l;
        else break;
    }

    return 1.0 - (js + p * l * (1.0 - js));
}

// American Soundex: returns a 4-character code (letter + 3 digits)
static std::string soundex_encode(const std::string& s) {
    if (s.empty()) return "0000";

    auto digit_of = [](char c) -> char {
        switch ((char)toupper((unsigned char)c)) {
            case 'B': case 'F': case 'P': case 'V': return '1';
            case 'C': case 'G': case 'J': case 'K': case 'Q':
            case 'S': case 'X': case 'Z':             return '2';
            case 'D': case 'T':                       return '3';
            case 'L':                                 return '4';
            case 'M': case 'N':                       return '5';
            case 'R':                                 return '6';
            default:                                  return '0';
        }
    };

    std::string code;
    code += (char)toupper((unsigned char)s[0]);
    char prev = digit_of(s[0]);

    for (int i = 1; i < (int)s.size() && (int)code.size() < 4; ++i) {
        char uc = (char)toupper((unsigned char)s[i]);
        // H and W are transparent in American Soundex
        if (uc == 'H' || uc == 'W') continue;
        char cur = digit_of(s[i]);
        if (cur != '0' && cur != prev) code += cur;
        prev = cur;
    }

    while ((int)code.size() < 4) code += '0';
    return code;
}

// Soundex distance: 0.0 if same code, 1.0 if different
static double soundex_dist(const std::string& s1, const std::string& s2) {
    if (s1.empty() || s2.empty()) return 1.0;
    return (soundex_encode(s1) == soundex_encode(s2)) ? 0.0 : 1.0;
}

// Cosine distance on character unigrams (q = 1 — matches stringdist default)
static double cosine_dist_q1(const std::string& s1, const std::string& s2) {
    if (s1.empty() && s2.empty()) return 0.0;
    if (s1.empty() || s2.empty()) return 1.0;

    std::unordered_map<unsigned char, int> f1, f2;
    for (unsigned char c : s1) ++f1[c];
    for (unsigned char c : s2) ++f2[c];

    double dot = 0.0, norm1 = 0.0, norm2 = 0.0;
    for (const auto& kv : f1) {
        norm1 += (double)kv.second * kv.second;
        auto it = f2.find(kv.first);
        if (it != f2.end()) dot += (double)kv.second * it->second;
    }
    for (const auto& kv : f2) {
        norm2 += (double)kv.second * kv.second;
    }

    if (norm1 <= 0.0 || norm2 <= 0.0) return 1.0;
    double sim = dot / (std::sqrt(norm1) * std::sqrt(norm2));
    // clamp against floating-point rounding
    if (sim > 1.0) sim = 1.0;
    return 1.0 - sim;
}

// ============================================================
// Main exported function
// ============================================================

//' Match school names across two datasets using vectorised C++ distances
//'
//' Replaces the per-row R loop in \code{match_schools_names()}.  For every
//' row in \code{data1} it finds candidate rows in \code{data2} that share
//' the same group key (typically \code{county_std}), then classifies each
//' data1 row as:
//' \describe{
//'   \item{0}{No data2 rows share the same group key.}
//'   \item{1}{Unmatched: all JW distances exceed \code{threshold_jw_min}.}
//'   \item{2}{Exact: at least one JW distance ≤ \code{exact_jw}. The
//'     returned candidates are all rows with JW ≤ \code{exact_jw}.}
//'   \item{3}{Non-exact candidates: after filtering by
//'     \code{(jw <= threshold_jw) | (soundex == 0 & cosine <= soundex_cosine_thresh)},
//'     the top-10 (by JW) are returned for R-level detailed scoring.}
//' }
//'
//' @param names1  Character vector — \code{school_name_std} for all data1 rows.
//' @param groups1 Character vector — composite group key (lowercase) for data1.
//' @param names2  Character vector — \code{school_name_std} for all data2 rows.
//' @param groups2 Character vector — composite group key (lowercase) for data2.
//' @param threshold_jw_min Numeric. Rows where all JW distances exceed this
//'   value are classified as unmatched (status 1).
//' @param threshold_jw Numeric. JW threshold for candidate selection.
//' @param exact_jw Numeric. JW threshold for classifying a match as exact
//'   (status 2).
//' @param soundex_cosine_thresh Numeric. Cosine threshold used alongside
//'   \code{soundex == 0} in the fallback candidate filter (default 0.25).
//'
//' @return A named list with elements:
//' \describe{
//'   \item{status}{Integer vector (length = \code{length(names1)}).}
//'   \item{candidates_idx}{List of integer vectors: 1-based indices into
//'     \code{names2}.}
//'   \item{candidates_jw}{List of numeric vectors: JW distances.}
//'   \item{candidates_soundex}{List of numeric vectors: Soundex distances
//'     (0 or 1). \code{NULL} for exact-match rows.}
//'   \item{candidates_cosine}{List of numeric vectors: Cosine distances.
//'     \code{NULL} for exact-match rows.}
//' }
//' @keywords internal
// [[Rcpp::export]]
List match_schools_batch_cpp(
        CharacterVector names1,
        CharacterVector groups1,
        CharacterVector names2,
        CharacterVector groups2,
        double threshold_jw_min,
        double threshold_jw,
        double exact_jw,
        double soundex_cosine_thresh = 0.25) {

    int n1 = names1.size();
    int n2 = names2.size();

    // Build group -> data2 row-index map
    std::unordered_map<std::string, std::vector<int>> g2idx;
    g2idx.reserve((size_t)n2);
    for (int j = 0; j < n2; ++j) {
        if (CharacterVector::is_na(groups2[j])) continue;
        g2idx[as<std::string>(groups2[j])].push_back(j);
    }

    IntegerVector status(n1, 0);
    List cand_idx(n1), cand_jw(n1), cand_sd(n1), cand_cd(n1);

    for (int i = 0; i < n1; ++i) {

        if (CharacterVector::is_na(groups1[i]) || CharacterVector::is_na(names1[i])) {
            status[i] = 1;  // treat missing group/name as unmatched
            continue;
        }

        std::string g1  = as<std::string>(groups1[i]);
        auto it = g2idx.find(g1);
        if (it == g2idx.end()) {
            status[i] = 0;  // no matching group in data2
            continue;
        }

        const std::vector<int>& idxs = it->second;
        int nc = (int)idxs.size();
        std::string nm1 = as<std::string>(names1[i]);

        // --- Step 1: Jaro-Winkler to all data2 rows in the same group --------
        std::vector<double> jw_v(nc);
        std::vector<bool>   na_v(nc, false);
        for (int k = 0; k < nc; ++k) {
            if (CharacterVector::is_na(names2[idxs[k]])) {
                na_v[k] = true;
                jw_v[k] = 1.0;
            } else {
                jw_v[k] = jaro_winkler_dist(nm1, as<std::string>(names2[idxs[k]]));
            }
        }

        // --- Step 2: All JW > threshold_jw_min → unmatched (status 1) --------
        bool any_below = false;
        for (int k = 0; k < nc; ++k) {
            if (!na_v[k] && jw_v[k] <= threshold_jw_min) { any_below = true; break; }
        }
        if (!any_below) { status[i] = 1; continue; }

        // --- Step 3: Any JW ≤ exact_jw → exact match (status 2) -------------
        bool has_exact = false;
        for (int k = 0; k < nc; ++k) {
            if (!na_v[k] && jw_v[k] <= exact_jw) { has_exact = true; break; }
        }

        if (has_exact) {
            // Collect all rows with JW ≤ exact_jw; R handles tie-breaking
            std::vector<int>    ei;
            std::vector<double> ej;
            for (int k = 0; k < nc; ++k) {
                if (!na_v[k] && jw_v[k] <= exact_jw) {
                    ei.push_back(idxs[k] + 1);  // 1-based R index
                    ej.push_back(jw_v[k]);
                }
            }
            status[i]    = 2;
            cand_idx[i]  = wrap(ei);
            cand_jw[i]   = wrap(ej);
            cand_sd[i]   = R_NilValue;  // not needed for exact path
            cand_cd[i]   = R_NilValue;

        } else {
            // --- Step 4: Filter + top-10 (status 3) -------------------------
            // Keep rows where JW ≤ threshold_jw  OR  (soundex==0 AND cosine≤thresh)
            std::vector<int>    fi;
            std::vector<double> fj, fs, fc;

            for (int k = 0; k < nc; ++k) {
                if (na_v[k]) continue;
                std::string nm2 = as<std::string>(names2[idxs[k]]);
                bool jw_pass    = jw_v[k] <= threshold_jw;
                double sd       = soundex_dist(nm1, nm2);
                double cd       = cosine_dist_q1(nm1, nm2);

                if (jw_pass || (sd == 0.0 && cd <= soundex_cosine_thresh)) {
                    fi.push_back(idxs[k]);
                    fj.push_back(jw_v[k]);
                    fs.push_back(sd);
                    fc.push_back(cd);
                }
            }

            if (fi.empty()) { status[i] = 1; continue; }

            // Sort by JW ascending and keep top 10
            int nf    = (int)fi.size();
            int top_n = std::min(10, nf);
            std::vector<int> ord(nf);
            std::iota(ord.begin(), ord.end(), 0);
            std::partial_sort(ord.begin(), ord.begin() + top_n, ord.end(),
                              [&fj](int a, int b) { return fj[a] < fj[b]; });

            std::vector<int>    oi(top_n);
            std::vector<double> oj(top_n), os(top_n), oc(top_n);
            for (int k = 0; k < top_n; ++k) {
                int o  = ord[k];
                oi[k]  = fi[o] + 1;   // 1-based R index
                oj[k]  = fj[o];
                os[k]  = fs[o];
                oc[k]  = fc[o];
            }

            status[i]   = 3;
            cand_idx[i] = wrap(oi);
            cand_jw[i]  = wrap(oj);
            cand_sd[i]  = wrap(os);
            cand_cd[i]  = wrap(oc);
        }
    }

    return List::create(
        Named("status")            = status,
        Named("candidates_idx")    = cand_idx,
        Named("candidates_jw")     = cand_jw,
        Named("candidates_soundex")= cand_sd,
        Named("candidates_cosine") = cand_cd
    );
}
