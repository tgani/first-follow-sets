#include "firstsets.h"

using namespace std::string_literals;

enum struct Tok {
    none,
    eof,
    id,
    colon,
    semi,
    alt,
    lbrace,
    rbrace,
    lbrack,
    rbrack,
    lparen,
    rparen,
    literal,
    epsilon
};

struct Bad_token_error {
};

struct Syntax_error {
};

struct Scanner {
    FILE* fp_; // input source.
    std::string line_; // current line
    Tok la_; // look ahead
    size_t pos_; // read pos in line
    std::string alfa_; // value of identifier for la_ == Tok::id
    std::string literal_; // value of literal string
public:
    Scanner(FILE* fp)
        : fp_ { fp }
        , la_ { Tok::none }
        , pos_ { 0 }
    {
        assert(fp != nullptr && !feof(fp));
        get();
    }
    Tok token() const { return la_; }

    std::string curr_token_str() const
    {
        using namespace std::string_literals;
        switch (la_) {
        default:
            assert(false && "unreached");
            return "Tok::none";
        case Tok::alt:
            return "|";
        case Tok::colon:
            return ":";
        case Tok::lbrace:
            return "{";
        case Tok::rbrace:
            return "}";
        case Tok::lbrack:
            return "[";
        case Tok::rbrack:
            return "]";
        case Tok::lparen:
            return "(";
        case Tok::rparen:
            return ")";
        case Tok::eof:
            return "END-OF-FILE";
        case Tok::epsilon:
            return "EPSILON";
        case Tok::id:
            return "ID("s + alfa_ + ")"s;
        case Tok::literal:
            return "LITERAL("s + literal_ + ")"s;
        case Tok::semi:
            return ";";
        }
    }

    std::string identifier() const
    {
        assert(la_ == Tok::id);
        return alfa_;
    }
    std::string literal() const
    {
        assert(la_ == Tok::literal);
        return literal_;
    }

    void getline()
    {
        int c;
        line_.clear();
        for (;;) {
            c = fgetc(fp_);
            if (c == EOF || feof(fp_))
                break;
            line_ += static_cast<char>(c);
            if (c == '\n')
                break;
        }
        pos_ = 0;
    }
    void skip_white_space()
    {
        while (pos_ < line_.size() && isspace(line_[pos_]))
            ++pos_;
    }
    void show_line()
    {
        const unsigned spaces_per_tab = 8;
        unsigned col = 0;
        for (auto c : line_) {
            if (c == '\t') {
                unsigned n = spaces_per_tab - (col % spaces_per_tab);
                col += n;
                while (n-- > 0)
                    putchar(' ');
            } else {
                putchar(c);
                col += 1;
            }
        }
        putchar('\n');
        while (col-- > 1)
            putchar(' ');
        putchar('^');
        putchar('\n');
    }
    void bad_token()
    {
        show_line();
        printf("error: bad token\n");
        throw Bad_token_error {};
    }
    void scan_id()
    {
        alfa_.clear();
        assert(isalpha(line_[pos_]));
        alfa_ += line_[pos_++];
        while (pos_ < line_.size() && isalpha(line_[pos_]))
            alfa_ += line_[pos_++];
        if (alfa_ == "EPSILON")
            la_ = Tok::epsilon;
        else
            la_ = Tok::id;
    }
    void scan_literal()
    {
        assert(line_[pos_] == '"');
        literal_.clear();
        literal_ += line_[pos_++];
        for (;;) {
            if (pos_ >= line_.size()) {
                bad_token();
            }
            literal_ += line_[pos_++];
            if (line_[pos_ - 1] == '"')
                break;
        }
        la_ = Tok::literal;
    }
    void get()
    {
        if (la_ == Tok::eof)
            return;
        skip_white_space();
        if (pos_ >= line_.size()) {
            getline();
            if (line_.size() == 0)
                la_ = Tok::eof;
            return get();
        }
        char c = line_[pos_];
        switch (c) {
        case '|':
            la_ = Tok::alt;
            ++pos_;
            break;
        case ':':
            la_ = Tok::colon;
            ++pos_;
            break;
        case ';':
            la_ = Tok::semi;
            ++pos_;
            break;
        case '{':
            la_ = Tok::lbrace;
            ++pos_;
            break;
        case '}':
            la_ = Tok::rbrace;
            ++pos_;
            break;
        case '[':
            la_ = Tok::lbrack;
            ++pos_;
            break;
        case ']':
            la_ = Tok::rbrack;
            ++pos_;
            break;
        case '(':
            la_ = Tok::lparen;
            ++pos_;
            break;
        case ')':
            la_ = Tok::rparen;
            ++pos_;
            break;
        case '"':
            return scan_literal();
        default:
            if (isalpha(c)) {
                return scan_id();
            } else {
                bad_token();
            }
        }
    }
};

// Type representing a single terminal. E.g, "foo" that is to be matched exactly.
struct Terminal {
    std::string name; // Also the value.
    Terminal(const std::string& n)
        : name { n }
    {
    }
};

// Type representing the empty production "ε"
struct Epsilon {
};

// Type representing a non-terminal, which appears on the LHS of productions.
struct NonTerminal {
    std::string name; // Name of the non-terminal.
    NonTerminal(const std::string& n)
        : name { n }
    {
    }
};

// Forward decls.
struct ProductionItem;
struct ProductionAlternative;

using ProductionItemSeq = std::vector<ProductionItem>;
using ProductionAlternatives = std::vector<ProductionAlternative>;

// Type representing a clause such as
//	{ "+" Term }            contents occur 0 or more times
//	[ "-" Expression ]      contents occur 0 or 1 time
//  ( "-" | "+" )           contents occur exactly 1 time
struct ProductionItemSeqClause {
    Tok clause_kind_;

public:
    Tok kind() const { return clause_kind_; }
    ProductionItemSeqClause(Tok t)
        : clause_kind_ { t }
    {
        assert(t == Tok::lbrace || t == Tok::lbrack || t == Tok::lparen);
    }
    ProductionItemSeq sequence;
};

// Type representing an element of the production's RHS. A production's RHS is composed of a
// sequence of these.
struct ProductionItem {
    std::variant<Terminal, NonTerminal, Epsilon, ProductionItemSeqClause, ProductionAlternatives>
        item;

public:
    ProductionItem(const Terminal& terminal)
        : item { terminal }
    {
    }
    ProductionItem(const NonTerminal& nonterminal)
        : item { nonterminal }
    {
    }
    ProductionItem(const ProductionItemSeqClause& seq_clause)
        : item { seq_clause }
    {
    }
    ProductionItem(Epsilon)
        : item { Epsilon {} }
    {
    }
    ProductionItem(const ProductionAlternatives& alts)
        : item { alts }
    {
    }
    template <typename VariantType>
    VariantType* only_if()
    {
        return std::get_if<VariantType>(&item);
    }
    template <typename VariantType>
    const VariantType* only_if() const
    {
        return std::get_if<VariantType>(&item);
    }
};

// Type representing a production's right hand side for a single alternative.
struct ProductionAlternative {
    ProductionItemSeq sequence;
};

// Type representing a single production.
struct Production {
    NonTerminal lhs;
    std::vector<ProductionAlternative> alternatives; // Must be >= 1.
    Production(const NonTerminal& nt, std::vector<ProductionAlternative>&& list)
        : lhs { nt }
        , alternatives { list }
    {
    }
};

// Type representing a normalized production.
struct NormalizedProduction {
    NonTerminal lhs;
    ProductionItemSeq seq;

    NormalizedProduction(const NonTerminal& lhs, const ProductionItemSeq& seq)
        : lhs { lhs }
        , seq { seq }
    {
    }
};

// Type representing a list of productions in the grammar.
using ProductionList = std::vector<Production>;

// Type representing a list of normalized production.
using NormalizedProductionList = std::vector<NormalizedProduction>;

// Grammar parser. Parses productions and stores them in EBNF form. Then simplifies them
// to simple CFG on request.
struct Parser {
    Scanner& scn_; // The scanner being used
    ProductionList prods_; // The list of productions parsed
public:
    Parser(Scanner& scn)
        : scn_ { scn }
    {
    }

    const ProductionList& productions() const { return prods_; }

    void syntax_error()
    {
        scn_.show_line();
        printf("syntax error\n");
        throw Syntax_error {};
    }

    void require(Tok t)
    {
        if (scn_.token() == t)
            scn_.get();
        else
            syntax_error();
    }

    bool next_is(Tok t) const { return scn_.token() == t; }

    Tok token() const { return scn_.token(); }

    void gettoken() { scn_.get(); }

    ProductionItem scan_name_or_symbol()
    {
        if (next_is(Tok::id)) {
            auto name = scn_.identifier();
            gettoken();
            return ProductionItem { NonTerminal { name } };
        } else if (next_is(Tok::literal)) {
            auto lit = scn_.literal();
            scn_.get();
            return ProductionItem { Terminal { lit } };
        } else {
            syntax_error();
            return ProductionItem { Epsilon {} }; // not reached.
        }
    }
    ProductionItemSeqClause scan_production_item_seq_clause()
    {
        ProductionItemSeqClause seq_clause { token() };
        gettoken();
        auto alts = scan_alternatives();
        if (alts.size() > 1)
            seq_clause.sequence.push_back(alts);
        else {
            // Just a single "alternative", i.e., | was not used.
            assert(alts.size() == 1);
            seq_clause.sequence = std::move(alts[0].sequence);
        }
        require(seq_clause.kind() == Tok::lbrace ? Tok::rbrace : seq_clause.kind() == Tok::lbrack ? Tok::rbrack : Tok::rparen);
        return seq_clause;
    }

    ProductionItem scan_production_item()
    {
        if (next_is(Tok::lbrace) || next_is(Tok::lbrack) || next_is(Tok::lparen)) {
            return ProductionItem { scan_production_item_seq_clause() };
        }
        return ProductionItem { scan_name_or_symbol() };
    }

    ProductionItemSeq scan_production_item_seq()
    {
        ProductionItemSeq seq;
        ProductionItem item = scan_production_item();
        seq.push_back(item);
        while (next_is(Tok::id)
            || next_is(Tok::literal)
            || next_is(Tok::lbrace)
            || next_is(Tok::lbrack)
            || next_is(Tok::lparen)) {
            item = scan_alternatives();
            seq.push_back(item);
        }
        return seq;
    }

    ProductionAlternative scan_single_alternative()
    {
        if (next_is(Tok::epsilon)) {
            ProductionItem item { Epsilon {} };
            ProductionAlternative prodalt;
            prodalt.sequence.push_back(item);
            gettoken();
            return prodalt;
        }
        ProductionAlternative prodalt;
        prodalt.sequence = scan_production_item_seq();
        return prodalt;
    }

    std::vector<ProductionAlternative> scan_alternatives()
    {
        std::vector<ProductionAlternative> result;
        auto prodalt = scan_single_alternative();
        result.push_back(prodalt);
        while (next_is(Tok::alt)) {
            gettoken();
            prodalt = scan_single_alternative();
            result.push_back(prodalt);
        }
        return result;
    }

    Production scan_production()
    {
        auto nonterm
            = scn_.token() == Tok::id ? scn_.identifier() : ""; // guard against erroneous input
        require(Tok::id);
        require(Tok::colon);
        auto altlist = scan_alternatives();
        require(Tok::semi);
        return Production { nonterm, std::move(altlist) };
    }

    void parse()
    {
        // The grammar file is in the EBNF format, specified below in EBNF:
        //      Productions = Production { Production }
        //      Production = NonTerminal ':' Alternatives ';'
        //      NonTerminal = Identifier
        //      Alternatives = SingleAlternative { '|' SingleAlternative }
        //      SingleAlternative = ProductionItemSeq | 'EPSILON'
        //      ProductionItemSeq = ProductionItem { ProductionItem }
        //      ProductionItem = NameOrSymbol | ProductionItemSeqClause
        //      ProductionItemSeqClause = '{' Alternatives '}' | '[' Alternatives ']' | '(' Alternatives ')'
        //      NameOrSymbol = Identifier | LiteralInQuotes
        auto prod = scan_production();
        prods_.push_back(prod);
        while (next_is(Tok::id)) {
            prod = scan_production();
            prods_.push_back(prod);
        }
    }

    void test_lexer()
    {
        for (;;) {
            printf("%s\n", scn_.curr_token_str().c_str());
            if (scn_.token() == Tok::eof)
                break;
            scn_.get();
        }
    }

    static std::string production_item_text(const ProductionItem& proditem)
    {
        std::string buf;
        if (proditem.only_if<Epsilon>()) {
            buf += "EPSILON ";
        } else if (auto p = proditem.only_if<NonTerminal>()) {
            buf += p->name;
            buf += ' ';
        } else if (auto p = proditem.only_if<Terminal>()) {
            buf += p->name;
            buf += ' ';
        } else if (auto p = proditem.only_if<ProductionItemSeqClause>()) {
            buf += production_item_seq_clause_text(*p);
        } else if (auto p = proditem.only_if<ProductionAlternatives>()) {
            buf += production_alternatives_text(*p);
        } else {
            assert(false && "unreached");
        }
        return buf;
    }

    static std::string production_item_seq_text(const ProductionItemSeq& seq)
    {
        std::string buf;
        for (auto&& item : seq) {
            buf += production_item_text(item);
        }
        return buf;
    }

    static std::string production_item_seq_clause_text(const ProductionItemSeqClause& clause)
    {
        std::string buf;
        buf += (clause.kind() == Tok::lbrace ? '{' : clause.kind() == Tok::lbrack ? '[' : '(');
        buf += production_item_seq_text(clause.sequence);
        buf += (clause.kind() == Tok::lbrace ? '}' : clause.kind() == Tok::rbrack ? ']' : ')');
        buf += ' ';
        return buf;
    }

    static std::string production_alternatives_text(const ProductionAlternatives& prodalts)
    {
        std::string buf;
        bool first = true;
        for (auto& item : prodalts) {
            if (!first) {
                buf += '|';
                buf += ' ';
            }
            buf += production_item_seq_text(item.sequence);
            first = false;
        }
        return buf;
    }

    static std::string productions_text(const ProductionList& prods)
    {
        std::string buf;
        for (auto prod : prods) {
            buf += prod.lhs.name;
            buf += ' ';
            buf += ':';
            buf += ' ';
            buf += production_alternatives_text(prod.alternatives);
            buf += '\n';
        }
        return buf;
    }
    static std::string productions_text(const NormalizedProductionList& prods)
    {
        std::string buf;
        for (auto prod : prods) {
            buf += prod.lhs.name;
            buf += ' ';
            buf += ':';
            buf += ' ';
            buf += production_item_seq_text(prod.seq);
            buf += '\n';
        }
        return buf;
    }
};

std::pair<std::vector<ProductionItemSeqClause>, std::vector<unsigned>>
find_sequence_or_optional_clause(const NormalizedProduction& prod)
{
    std::vector<ProductionItemSeqClause> clauses;
    std::vector<unsigned> locations;
    unsigned loc = 0;
    for (auto&& item : prod.seq) {
        if (auto p = item.only_if<ProductionItemSeqClause>()) {
            clauses.push_back(*p);
            locations.push_back(loc);
        }
        ++loc;
    }
    return { clauses, locations };
}

std::pair<ProductionItemSeq, std::vector<NonTerminal>>
replace_sequence_or_optional_clauses(const ProductionItemSeq& prodseq,
    const std::vector<unsigned>& locations,
    unsigned name_seq)
{
    if (locations.size() == 0)
        return { {}, {} };
    ProductionItemSeq itemseq;
    std::vector<NonTerminal> invented_nonterminals;
    unsigned i = 0;
    for (auto loc : locations) {
        while (i < loc) {
            itemseq.push_back(prodseq[i]);
            ++i;
        }
        // Create new non-terminal N_k and replace sequence-or-optional clause with it.
        NonTerminal nt { "N"s + std::to_string(name_seq) };
        name_seq += 1;
        itemseq.push_back(nt);
        invented_nonterminals.push_back(nt);
        ++i;
    }
    // Copy any remaining.
    while (i < prodseq.size())
        itemseq.push_back(prodseq[i++]);
    return { std::move(itemseq), std::move(invented_nonterminals) };
}

void generate_invented_productions(const std::vector<NonTerminal>& nt_list,
    const std::vector<ProductionItemSeqClause>& seq_clause_list,
    NormalizedProductionList& prod_accum)
{
    unsigned i = 0;
    assert(nt_list.size() == seq_clause_list.size());
    for (auto& seq_clause : seq_clause_list) {
        // Add N_k -> EPSILON, if not ()
        if (seq_clause.kind() != Tok::lparen) {

            ProductionItemSeq epsilon_seq { Epsilon {} };
            prod_accum.emplace_back(nt_list[i], epsilon_seq);
        }
        // Add N_k -> <seq> N_k; for {}
        //     N_k -> <seq> for [] and ()
        prod_accum.emplace_back(nt_list[i], seq_clause.sequence);
        if (seq_clause.kind() == Tok::lbrace) {
            prod_accum.back().seq.push_back(nt_list[i]);
        }
    }
}

NormalizedProductionList normalize(const ProductionList& prodlist)
{
    // Step 1: convert A: a1| a2 | ... aN to
    //      A -> a1
    //      A -> a2
    // ...
    NormalizedProductionList result;

    for (auto&& prod : prodlist) {
        for (auto i = 0U; i < prod.alternatives.size(); ++i) {
            NormalizedProduction nprod { prod.lhs, prod.alternatives[i].sequence };
            result.push_back(nprod);
        }
    }

    // Step 2:
    // Convert all productions involving [] and {} in this fashion:
    // Suppose:
    //      A -> <alpha> { <beta> } <gamma>
    // Then we can convert this to:
    //      A -> <alpha> N <gamma>
    //      N -> EPSILON
    //      N -> <beta> N
    // Similarly, for
    //      A -> <alpha> [ <beta> ] <gamma>
    // can be converted to
    //      A -> <alpha> N <gamma>
    //      N -> EPSILON
    //      N -> <beta>
    // Similarlar, for
    //      A -> <alpha> ( <beta> ) <gamma>
    // can be converted to
    //      A -> <alpha> N <gamma>
    //      N -> <beta>
    // For each sequence-or-optional-clause on the RHS we replace by invented non-terminals and
    // for each such non-terminal add the above productions. Since <beta> in the above cases itself
    // can contain an alternative clause, we need to do this iteratively till there are
    // no such clauses left.
    bool something_to_do = true;
    unsigned name_seq = 1;
    while (something_to_do) {
        NormalizedProductionList invented_productions;
        for (auto& normprod : result) {
            // clause_items: list of all sequence or optional clauses
            // locations: list of all the indices in normprod they occur at
            auto [clause_items, locations] = find_sequence_or_optional_clause(normprod);
            assert(clause_items.size() == locations.size());
            auto [replaced_seq, invented_nonterminals]
                = replace_sequence_or_optional_clauses(normprod.seq, locations, name_seq);
            if (invented_nonterminals.size() > 0)
                normprod.seq = std::move(replaced_seq);
            generate_invented_productions(
                invented_nonterminals, clause_items, invented_productions);
            name_seq += invented_nonterminals.size();
        }
        something_to_do = invented_productions.size() > 0;
        result.insert(result.end(), invented_productions.begin(), invented_productions.end());
    }

    return result;
}

void test_lexer(FILE* fp)
{
    Scanner scn { fp };
    Parser p { scn };
    p.test_lexer();
}

ProductionList read_grammar(FILE* fp)
{
    Scanner scn { fp };
    Parser p { scn };
    p.parse();
    return p.productions();
}

struct CmpTerminal {
    bool operator()(const Terminal& lhs, const Terminal& rhs) const { return lhs.name < rhs.name; }
};

struct CmpNonTerminal {
    bool operator()(const NonTerminal& lhs, const NonTerminal& rhs) const
    {
        return lhs.name < rhs.name;
    }
};

using GrammarElement = std::variant<Terminal, NonTerminal>;

struct CmpGrammarElement {
    bool operator()(const GrammarElement& lhs, const GrammarElement& rhs) const
    {
        const std::string& lhs_name
            = lhs.index() == 0 ? std::get<0>(lhs).name : std::get<1>(lhs).name;
        const std::string& rhs_name
            = rhs.index() == 0 ? std::get<0>(rhs).name : std::get<1>(rhs).name;
        return lhs_name < rhs_name;
    }
};

// Type representing function from terminal/non-terminals -> set of terminals, used in computing
// FIRST and FOLLOW
using TerminalSet = std::set<Terminal, CmpTerminal>;
using NonTerminalSet = std::set<NonTerminal, CmpNonTerminal>;
using GrammarElementToTerminalSetMap = std::map<GrammarElement, TerminalSet*, CmpGrammarElement>;

struct TopDownParsingSets {
    NonTerminalSet epsilon_;
    GrammarElementToTerminalSetMap predict_, follow_;
    bool any_changes_ = false;
    const NormalizedProductionList& prods_;

    TopDownParsingSets(const NormalizedProductionList& prods)
        : prods_ { prods }
    {
    }

    // For a production A -> N1 N2 ...
    // return true iff all of N1 N2 ... derive epsilon. Note that
    // due to the current knowledge of which of N1 N2 ... derive epsilon
    // we may return a false negative but that's ok due to the iterative
    // nature of the process.
    bool is_seq_nullable(const ProductionItemSeq& seq)
    {
        for (auto&& elem : seq) {
            // This is not a non-terminal. Sequence cannot be nullable.
            if (!elem.only_if<NonTerminal>())
                return false;

            if (epsilon_.find(std::get<NonTerminal>(elem.item)) == epsilon_.end())
                return false;
        }
        return true;
    }
    // EPSILON contains:
    //  - Any non-terminal that has an epsilon production
    //  - Any non-terminal that has a production containing only non-terminals that are in EPSILON
    void do_epsilon()
    {
        for (auto&& P : prods_) {
            if (P.seq.size() == 1 && P.seq[0].only_if<Epsilon>()) {
                auto [_, inserted] = epsilon_.insert(P.lhs);
                any_changes_ |= inserted;
            }
            if (is_seq_nullable(P.seq)) {
                auto [_, inserted] = epsilon_.insert(P.lhs);
                any_changes_ |= inserted;
            }
        }
    }

    void insert_into_set(GrammarElementToTerminalSetMap& mapref,
        const GrammarElement& gelem,
        const Terminal& t)
    {
        auto iter = mapref.find(gelem);
        if (iter == mapref.end()) {
            auto ptr = new TerminalSet;
            ptr->insert(t);
            mapref[gelem] = ptr;
            any_changes_ = true;
        } else {
            auto [_, inserted] = (*iter).second->insert(t);
            any_changes_ |= inserted;
        }
    }

    void insert_into_predict_set(const GrammarElement& gelem, const Terminal& t)
    {
        insert_into_set(predict_, gelem, t);
    }

    void insert_into_follow_set(const GrammarElement& gelem, const Terminal& t)
    {
        insert_into_set(follow_, gelem, t);
    }

    static TerminalSet& get_set_for(GrammarElementToTerminalSetMap& mapref, const NonTerminal& nt)
    {
        auto iter = mapref.find(nt);

        if (iter == mapref.end()) {
            auto ptr = new TerminalSet;
            mapref[nt] = ptr;
            return *ptr;
        }
        return *(*iter).second;
    }

    TerminalSet& predict_set_for(const NonTerminal& nt) { return get_set_for(predict_, nt); }

    TerminalSet& follow_set_for(const NonTerminal& nt) { return get_set_for(follow_, nt); }

    // PREDICT(A) contains:
    //  - A terminal that appears leftmost in a production for A
    //  - For every non-terminal B that appears leftmost in a production for A,
    //    every terminal in PREDICT(B)
    //  - if A =>* epsilon, i.e., A is in EPSILON, the every token in FOLLOW(A).
    void do_predict()
    {
        for (auto&& P : prods_) {
            auto&& first_elem = P.seq[0];
            if (auto elem = first_elem.only_if<Terminal>()) {
                insert_into_predict_set(P.lhs, *elem);
            } else if (auto elem = first_elem.only_if<NonTerminal>()) {
                for (auto&& terminal : predict_set_for(*elem)) {
                    insert_into_predict_set(P.lhs, terminal);
                }
            }
            if (epsilon_.find(P.lhs) != epsilon_.end()) {
                for (auto&& terminal : follow_set_for(P.lhs)) {
                    insert_into_predict_set(P.lhs, terminal);
                }
            }
        }
    }

    // FOLLOW(A) contains:
    //  - Any terminal that immediately follows A in any production
    //  - For any non-terminal B that immediately follows A in any production,
    //    all the tokens in PREDICT(B)
    //  - For any non-terminal B such that A appears rightmost in a production for
    //    B, every token in FOLLOW(B)
    void do_follow()
    {
        for (auto&& prod : prods_) {
            for (auto it = prod.seq.begin(); it != prod.seq.end(); ++it) {
                if (auto* A = (*it).only_if<NonTerminal>()) {
                    if (it + 1 != prod.seq.end()) {
                        // What follows A?
                        auto it2 = it + 1;
                        if (auto* tptr = (*it2).only_if<Terminal>())
                            insert_into_follow_set(*A, *tptr);
                        else if (auto* B = (*it2).only_if<NonTerminal>()) {
                            for (auto t : predict_set_for(*B)) {
                                insert_into_follow_set(*A, t);
                            }
                        } else {
                            assert(false && "unreached");
                        }
                    } else {
                        // A is rightmost in the production
                        for (auto t : follow_set_for(prod.lhs)) {
                            insert_into_follow_set(*A, t);
                        }
                    }
                }
            }
        }
    }

    void compute()
    {
        do {
            any_changes_ = false;
            do_epsilon();
            do_predict();
            do_follow();
        } while (any_changes_);
    }

    std::string epsilon_set_text()
    {
        std::string buf = "EPSILON = {\n";
        for (auto nt : epsilon_) {
            buf += nt.name;
            buf += ",\n";
        }
        buf += "}\n";
        return buf;
    }

    static std::string text_for_set(const char* name, const GrammarElementToTerminalSetMap& mapref)
    {
        std::string buf;
        for (auto&& entry : mapref) {
            if (auto ptr = std::get_if<NonTerminal>(&entry.first)) {
                buf += name;
                buf += "(";
                buf += ptr->name;
                buf += ") = { ";
                for (auto t : *entry.second) {
                    buf += t.name;
                    buf += ' ';
                }
                buf += "}\n";
            }
        }
        return buf;
    }

    std::string predict_set_text() { return text_for_set("PREDICT", predict_); }

    std::string follow_set_text() { return text_for_set("FOLLOW", follow_); }
};

void compute_first_sets(const ProductionList& prods)
{
    printf("%lu productions\n", prods.size());
    printf("%s\n", Parser::productions_text(prods).c_str());
    printf("after normalizing:\n");
    NormalizedProductionList normprods = normalize(prods);
    printf("%lu productions\n", normprods.size());
    printf("%s\n", Parser::productions_text(normprods).c_str());

    TopDownParsingSets parsing_sets { normprods };
    parsing_sets.compute();
    printf("%s%s%s",
        parsing_sets.epsilon_set_text().c_str(),
        parsing_sets.predict_set_text().c_str(),
        parsing_sets.follow_set_text().c_str());
}

int usage(const char* pname)
{
    fprintf(stderr, "usage: %s GRAMMAR-FILE\n", pname);
    return 2;
}

int main(int argc, char* argv[])
{
    if (argc < 2)
        exit(usage(argv[0]));
    FILE* fp = fopen(argv[1], "r");
    if (fp == nullptr) {
        perror(("can't open: "s + argv[1]).c_str());
        exit(2);
    }
    try {
        // test_lexer(fp);
        compute_first_sets(read_grammar(fp));
    } catch (Bad_token_error) {
    } catch (Syntax_error) {
    }
    fclose(fp);
}