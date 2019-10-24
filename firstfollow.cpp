#include "firstfollow.h"

static constexpr bool Debug = false;

using namespace std::string_literals;

// Global values set by options
const char* grammar_file_name;
bool        dump_normalized_grammar;
bool        dump_epsilon_sets;
bool        dump_first_sets;
bool        dump_follow_sets;

enum struct Tok
{
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

struct Bad_token_error
{
};

struct Syntax_error
{
};

struct Scanner
{
    FILE*       fp_;       // input source.
    const char* filename_; // Input file name.
    std::string line_;     // current line
    Tok         la_;       // look ahead
    size_t      la_pos_;   // position of the look ahead token.
    size_t      pos_;      // read pos in line
    size_t      lineno_;   // Line number
    std::string alfa_;     // value of identifier for la_ == Tok::id
    std::string literal_;  // value of literal string
public:
    Scanner(FILE* fp, const char* filename)
        : fp_ { fp }
        , filename_ { filename }
        , la_ { Tok::none }
        , la_pos_ { 0 }
        , pos_ { 0 }
        , lineno_ { 0 }
    {
        assert(fp != nullptr && !feof(fp));
        get();
    }
    Tok         token() const { return la_; }
    const char* filename() const { return filename_; }
    size_t      lineno() const { return lineno_; }

    std::string curr_token_str() const
    {
        using namespace std::string_literals;
        switch (la_)
        {
        default: assert(false && "unreached"); return "Tok::none";
        case Tok::alt: return "|";
        case Tok::colon: return ":";
        case Tok::lbrace: return "{";
        case Tok::rbrace: return "}";
        case Tok::lbrack: return "[";
        case Tok::rbrack: return "]";
        case Tok::lparen: return "(";
        case Tok::rparen: return ")";
        case Tok::eof: return "END-OF-FILE";
        case Tok::epsilon: return "EPSILON";
        case Tok::id: return "ID("s + alfa_ + ")"s;
        case Tok::literal: return "LITERAL("s + literal_ + ")"s;
        case Tok::semi: return ";";
        }
    }

    static const char* string_for_token(Tok t)
    {
        switch (t)
        {
        default: assert(false && "unreached"); return "Tok::none";
        case Tok::alt: return "|";
        case Tok::colon: return ":";
        case Tok::lbrace: return "{";
        case Tok::rbrace: return "}";
        case Tok::lbrack: return "[";
        case Tok::rbrack: return "]";
        case Tok::lparen: return "(";
        case Tok::rparen: return ")";
        case Tok::eof: return "end-of-file";
        case Tok::epsilon: return "EPSILON";
        case Tok::id: return "identifier";
        case Tok::literal: return "terminal-literal";
        case Tok::semi: return ";";
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
        for (;;)
        {
            c = fgetc(fp_);
            if (c == EOF || feof(fp_)) break;
            line_ += static_cast<char>(c);
            if (c == '\n')
            {
                ++lineno_;
                break;
            }
        }
        pos_ = 0;
    }
    void skip_white_space()
    {
        while (pos_ < line_.size() && isspace(line_[pos_])) ++pos_;
    }
    void show_line()
    {
        const unsigned spaces_per_tab = 8;
        unsigned       charpos        = 0;
        unsigned       col            = 0;
        for (auto c : line_)
        {
            if (c == '\t')
            {
                unsigned n = spaces_per_tab - (col % spaces_per_tab);
                col += (charpos < la_pos_ ? n : 0);
                while (n-- > 0) putchar(' ');
            }
            else
            {
                putchar(c);
                col += (charpos < la_pos_ ? 1 : 0);
            }
            ++charpos;
        }
        for (auto i = 0U; i < col; ++i) putchar(' ');
        putchar('^');
        putchar('\n');
        for (auto i = 0U; i < col; ++i) putchar(' ');
        putchar('|');
        putchar('\n');
    }
    void bad_token()
    {
        show_line();
        printf("%s(%zu): error: bad token\n", filename_, lineno_);
        throw Bad_token_error {};
    }
    void scan_id()
    {
        alfa_.clear();
        assert(isalpha(line_[pos_]));
        alfa_ += line_[pos_++];
        while (pos_ < line_.size()
               && (isalnum(line_[pos_]) || line_[pos_] == '-' || line_[pos_] == '_'))
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
        for (;;)
        {
            if (pos_ >= line_.size()) { bad_token(); }
            literal_ += line_[pos_++];
            if (line_[pos_ - 1] == '"') break;
        }
        la_ = Tok::literal;
    }
    void get()
    {
        if (la_ == Tok::eof) return;
        skip_white_space();
        la_pos_ = pos_;
        if (pos_ >= line_.size())
        {
            getline();
            if (line_.size() == 0) la_ = Tok::eof;
            return get();
        }
        char c = line_[pos_];
        switch (c)
        {
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
        case '"': return scan_literal();
        default:
            if (isalpha(c)) { return scan_id(); }
            else
            {
                bad_token();
            }
        }
    }
};

enum class ItemKind
{
    none,
    terminal,     // Anything in "..."
    nonterminal,  // Any identifiers
    epsilon,      // EPSILON
    alternatives, // alt1 | alt2 | ...
    sequence,     // RHS of A -> a1 a2... where a1 a2.. can be terminals, non terminals or grouped
    grouped_sequence // { a1 a2... } or [ a1 a2... ] or ( a1 a2 ...)
};

struct Item
{
    ItemKind    kind_; // The kind of the item. Currenty not used as we dynamic_cast<>
    std::string name_; // The name/identifier/literal string associated with the item.
                       // Some don't have this or have just a description string.

    Item(ItemKind k, const std::string& name)
        : kind_ { k }
        , name_ { name }
    {
    }

    ItemKind           kind() const { return kind_; }
    const std::string& name() const { return name_; }

    virtual std::string to_string() const { return name(); }

    template <typename T> T* as()
    {
        static_assert(std::is_base_of_v<Item, T>);
        assert(is<T>());
        return dynamic_cast<T*>(this);
    }

    template <typename T> const T* as() const
    {
        static_assert(std::is_base_of_v<Item, T>);
        assert(is<T>());
        return dynamic_cast<const T*>(this);
    }

    template <typename T> const T* only_if() const { return dynamic_cast<const T*>(this); }

    template <typename T> T* only_if() { return dynamic_cast<T*>(this); }

    template <typename T> bool is() const { return this->only_if<T>() != nullptr; }
};

using ItemList = std::vector<Item*>;

// Type representing a single terminal. E.g, "foo" that is to be matched exactly.
struct Terminal : Item
{
    Terminal(const std::string& n)
        : Item { ItemKind::terminal, n }
    {
    }
};

// Type representing the empty production "ε"
struct Epsilon : Item
{
    Epsilon()
        : Item { ItemKind::epsilon, "ε" }
    {
    }
};

struct NormalizedProduction; // Forward decl.

// Type representing a non-terminal, which appears on the LHS of productions.
struct NonTerminal : Item
{
    NonTerminal(const std::string& n)
        : Item { ItemKind::nonterminal, n }
    {
    }
};

// Type representing a sequence of "items" in a single production rule.
struct ItemSequence : Item
{
    ItemSequence()
        : Item { ItemKind::sequence, "<item-sequence>" }
    {
    }

    virtual std::string to_string() const override
    {
        std::string buf;
        bool        first = true;
        for (auto item : sequence)
        {
            if (!first) buf += ' ';
            buf += item->to_string();
            first = false;
        }
        return buf;
    }

    ItemList sequence; // sequence of items on the RHS of a production.
};

// Type representing a list of alternatives in a production. This type will not be present
// in normalized grammars.
struct Alternatives : Item
{
    Alternatives()
        : Item { ItemKind::alternatives, "<alternatives>" }
    {
    }

    virtual std::string to_string() const override
    {
        std::string buf;
        bool        first = true;
        for (auto alt : alternatives)
        {
            if (!first) buf += "| ";
            buf += alt->to_string();
            first = false;
        }
        return buf;
    }

    ItemList alternatives; // list of alternatives.
};

// Type representing a clause such as
//  { "+" Term }            contents occur 0 or more times
//  [ "-" Expression ]      contents occur 0 or 1 time
//  ( "-" | "+" )           contents occur exactly 1 time
struct GroupedItemSequence : Item
{
    Tok group_kind_; // What kind of grouping is it?

public:
    Tok group_kind() const { return group_kind_; }
    GroupedItemSequence(Tok t)
        : Item { ItemKind::grouped_sequence, "<grouped-item-sequence>" }
        , group_kind_ { t }
    {
        assert(t == Tok::lbrace || t == Tok::lbrack || t == Tok::lparen);
    }

    virtual std::string to_string() const override
    {
        std::string buf { Scanner::string_for_token(group_kind_) };
        buf += contents->to_string();
        switch (group_kind_)
        {
        case Tok::lparen: buf += ')'; break;
        case Tok::lbrack: buf += ']'; break;
        case Tok::lbrace: buf += '}'; break;
        }
        return buf;
    }

    Item* contents; // Can be any arbitrary construct.
};

// Type representing a location. So far, only line numbers are tracked.
struct Location
{
    size_t line_ = 0;

    size_t line() const { return line_; }

    Location() = default;

    static constexpr size_t none = 0xffffffff;

    Location(size_t l)
        : line_ { l }
    {
        assert(l != 0 && "line numbers begin with 1");
    }
};

// Type representing a production. "rhs" can be Epsilon, ItemSequence or Alternatives.
struct Production
{
    NonTerminal* lhs_ = nullptr; // LHS of the production
    Item*        rhs_ = nullptr; // RHS of the production in full EBNF form
    Location     loc_;           // Location of the LHS of the production

    Production(NonTerminal* lhs, Item* rhs, const Location& loc)
        : lhs_ { lhs }
        , rhs_ { rhs }
        , loc_ { loc }
    {
    }

    NonTerminal*       lhs() { return lhs_; }
    const NonTerminal* lhs() const { return lhs_; }
    Item*              rhs() { return rhs_; }
    const Item*        rhs() const { return rhs_; }

    const Location& location() const { return loc_; }

    void set_rhs(Item* item)
    {
        assert(item->is<Epsilon>() || item->is<ItemSequence>() || item->is<Alternatives>());
        assert(rhs_ == nullptr);
        rhs_ = item;
    }

    std::string to_string() const
    {
        std::string buf;

        buf += lhs_->to_string();
        buf += " : ";
        buf += rhs_->to_string();
        return buf;
    }
};

// Type representing a normalized production. "rhs" can be Epsilon or ItemSequence
struct NormalizedProduction
{
    NonTerminal* lhs_ = nullptr; // LHS of the production
    Item*        rhs_ = nullptr; // RHS of the production. Either EPSILON or an nonterminal
                                 // sequence in final normalized form.
    Location loc_;               // Location of LHS of the production.

    NormalizedProduction(NonTerminal* lhs, const Location& loc)
        : lhs_ { lhs }
        , loc_ { loc }
    {
        assert(lhs_->is<NonTerminal>());
    }

    const Item*        rhs() const { return rhs_; }
    Item*              rhs() { return rhs_; }
    const NonTerminal* lhs() const { return lhs_; }
    NonTerminal*       lhs() { return lhs_; }
    void               set_rhs(Item* item) { rhs_ = item; }
    const Location&    location() const { return loc_; }

    std::string to_string() const
    {
        std::string buf;

        buf += lhs_->to_string();
        buf += " : ";
        buf += rhs_->to_string();
        return buf;
    }
};

// Type representing a list of productions in the grammar.
using ProductionList = std::vector<Production*>;

// Type representing a list of normalized production.
using NormalizedProductionList = std::vector<NormalizedProduction*>;

// Comparsion for Item. We compare simply by name.
struct CmpItem
{
    bool operator()(const Item* lhs, const Item* rhs) const { return lhs->name() < rhs->name(); }
};
// Type representing function from terminal/non-terminals -> set of terminals, used in
// computing FIRST and FOLLOW
using TerminalSet          = std::set<const Terminal*, CmpItem>;
using NonTerminalSet       = std::set<const NonTerminal*, CmpItem>;
using ItemToTerminalSetMap = std::map<const Item*, TerminalSet*, CmpItem>;

// Grammar parser. Parses productions and stores them in EBNF form. Then simplifies them
// to simple CFG on request.
struct Parser
{
    Scanner&       scn_;   // The scanner being used
    ProductionList prods_; // The list of productions parsed
public:
    Parser(Scanner& scn)
        : scn_ { scn }
    {
    }

    const ProductionList& productions() const { return prods_; }

    void syntax_error(const std::string& msg)
    {
        scn_.show_line();
        printf("%s(%zu): error: %s\n", scn_.filename(), scn_.lineno(), msg.c_str());
        throw Syntax_error {};
    }

    void require(Tok t)
    {
        if (scn_.token() == t)
            scn_.get();
        else
        {
            std::string msgbuf;
            msgbuf += "expected ";
            msgbuf += Scanner::string_for_token(t);
            msgbuf += " but got ";
            msgbuf += scn_.curr_token_str();
            syntax_error(msgbuf);
        }
    }

    bool next_is(Tok t) const { return scn_.token() == t; }

    Tok token() const { return scn_.token(); }

    void gettoken() { scn_.get(); }

    // Parse a single terminal or non-terminal
    Item* scan_name_or_symbol()
    {
        if (next_is(Tok::id))
        {
            auto name = scn_.identifier();
            gettoken();
            return new NonTerminal { name };
        }
        else if (next_is(Tok::literal))
        {
            auto lit = scn_.literal();
            scn_.get();
            return new Terminal { lit };
        }
        else
        {
            syntax_error("unexpected token "s + scn_.curr_token_str());
            return new Epsilon {}; // not reached.
        }
    }

    // Parse { ... } ( ... ) or [ ... ]
    GroupedItemSequence* scan_grouped_items()
    {
        auto group_clause = new GroupedItemSequence { token() };
        gettoken();
        group_clause->contents = scan_alternatives();
        require(group_clause->group_kind() == Tok::lbrace
                    ? Tok::rbrace
                    : group_clause->group_kind() == Tok::lbrack ? Tok::rbrack : Tok::rparen);
        return group_clause;
    }

    // Parse a general item on the RHS of a production.
    Item* scan_production_item()
    {
        if (next_is(Tok::lbrace) || next_is(Tok::lbrack) || next_is(Tok::lparen))
        { return scan_grouped_items(); }
        return scan_name_or_symbol();
    }

    // Parse a sequence of items. If more than 1 item exists build an ItemSequence.
    // Else, return just the item.
    Item* scan_production_item_seq()
    {
        auto item = scan_production_item();
        if (!(next_is(Tok::id) || next_is(Tok::literal) || next_is(Tok::lbrace)
              || next_is(Tok::lbrack) || next_is(Tok::lparen)))
        { return item; }
        auto result = new ItemSequence;
        result->sequence.push_back(item);
        while (next_is(Tok::id) || next_is(Tok::literal) || next_is(Tok::lbrace)
               || next_is(Tok::lbrack) || next_is(Tok::lparen))
        {
            item = scan_production_item();
            if (auto items = item->only_if<ItemSequence>())
            {
                // Concatenate sequences.
                result->sequence.insert(
                    result->sequence.end(), items->sequence.begin(), items->sequence.end());
            }
            else
            {
                result->sequence.push_back(item);
            }
        }
        return result;
    }

    // Parse one of alt1 | alt2 ...
    Item* scan_single_alternative()
    {
        if (next_is(Tok::epsilon))
        {
            auto item = new Epsilon {};
            gettoken();
            return item;
        }
        auto seq = scan_production_item_seq();
        return seq;
    }
    // Parse <alpha>| <beta> | ...
    // If there is a single alternative, build and ItemSequence, otherwise build
    // Alternatives.
    Item* scan_alternatives()
    {
        auto prodalt = scan_single_alternative();
        if (!next_is(Tok::alt)) { return prodalt; }
        Alternatives* alt = new Alternatives;
        alt->alternatives.push_back(prodalt);
        while (next_is(Tok::alt))
        {
            gettoken();
            prodalt = scan_single_alternative();
            alt->alternatives.push_back(prodalt);
        }
        return alt;
    }

    // Parse a production in EBNF form.
    Production* scan_production()
    {
        auto nonterm
            = scn_.token() == Tok::id ? scn_.identifier() : ""; // guard against erroneous input
        auto line = scn_.lineno();
        require(Tok::id);
        require(Tok::colon);
        auto altlist = scan_alternatives();
        require(Tok::semi);
        return new Production { new NonTerminal { nonterm }, altlist, Location { line } };
    }

    // Parse the entire grammar file.
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
        //      ProductionItemSeqClause = '{' Alternatives '}' | '[' Alternatives ']' |
        //      '(' Alternatives ')' NameOrSymbol = Identifier | LiteralInQuotes
        auto prod = scan_production();
        prods_.push_back(prod);
        while (next_is(Tok::id))
        {
            prod = scan_production();
            prods_.push_back(prod);
        }
    }

    void test_lexer()
    {
        for (;;)
        {
            printf("%s\n", scn_.curr_token_str().c_str());
            if (scn_.token() == Tok::eof) break;
            scn_.get();
        }
    }

    template<typename List>
    static std::string to_string(const List& prodlist)
    {
        std::string buf;
        for (auto prod : prodlist)
        {
            buf += prod->to_string();
            buf += '\n';
        }
        return buf;
    }
};

// Given a production without alternatives (it may not be completely normalized yet),
// find all the places where {} [] or () occur and return all such grouped clauses and
// their indices in the ItemSequence. If an epsilon is on the RHS, it will be the only
// item.
//
// E.g., A -> B C D { E F } G [H I] will return std::pair<first={E F} [H I], second=0,
// 5>
//
std::pair<ItemList, std::vector<unsigned>>
find_sequence_or_optional_clause(const NormalizedProduction* prod)
{
    ItemList              clauses;
    std::vector<unsigned> locations;
    unsigned              loc = 0;
    auto                  rhs = prod->rhs();
    assert(rhs->is<Epsilon>() || rhs->is<ItemSequence>());
    if (rhs->is<Epsilon>()) { return { {}, {} }; }
    for (auto seqitem : rhs->as<ItemSequence>()->sequence)
    {
        if (auto p = seqitem->only_if<GroupedItemSequence>())
        {
            clauses.push_back(p);
            locations.push_back(loc);
        }
        ++loc;
    }
    return { clauses, locations };
}

// Replace grouped clauses in productions by invented non-terminals. Return the new
// production and the list of invented non-terminals. E.g., for the above example, we
// shall return
//    std::pair<first=B C D N1 G N2, second=N1, N2>
std::pair<ItemList, std::vector<NonTerminal*>>
replace_grouped_clauses(const Item* rhs, const std::vector<unsigned>& locations, unsigned name_seq)
{
    if (locations.size() == 0) return { {}, {} };
    ItemList                  itemseq;
    auto&&                    prodseq = rhs->as<ItemSequence>()->sequence;
    std::vector<NonTerminal*> invented_nonterminals;
    unsigned                  i = 0;
    for (auto loc : locations)
    {
        while (i < loc)
        {
            itemseq.push_back(prodseq[i]);
            ++i;
        }
        // Create new non-terminal N_k and replace sequence-or-optional clause with it.
        auto nt = new NonTerminal { "N"s + std::to_string(name_seq) };
        name_seq += 1;
        itemseq.push_back(nt);
        invented_nonterminals.push_back(nt);
        ++i;
    }
    // Copy any remaining.
    while (i < prodseq.size()) itemseq.push_back(prodseq[i++]);
    return { std::move(itemseq), std::move(invented_nonterminals) };
}

// Add new productions for the invented nonterminals. E.g., for the above example, we
// shall return with
//      prod_accum=
//              N1 -> EPSILON
//              N1 -> B C N1
//              N2 -> EPSILON
//              N2 -> H I
void generate_invented_productions(const std::vector<NonTerminal*>& nt_list,
                                   const ItemList&                  seq_clause_list,
                                   NormalizedProductionList&        prod_accum)
{
    unsigned clause_index = 0;
    assert(nt_list.size() == seq_clause_list.size());
    for (auto& item : seq_clause_list)
    {
        assert(item->is<GroupedItemSequence>());
        auto group_clause = item->as<GroupedItemSequence>();

        switch (group_clause->group_kind())
        {
        case Tok::lparen:
        {

            auto newprod = new NormalizedProduction { nt_list[clause_index], Location::none };
            newprod->set_rhs(group_clause->contents);
            prod_accum.push_back(newprod);
            break;
        }
        case Tok::lbrack:
        {
            // Add  N_k -> EPSILON
            //      N_k -> <seq>
            auto newprod = new NormalizedProduction { nt_list[clause_index], Location::none };
            newprod->set_rhs(new Epsilon);
            prod_accum.push_back(newprod);
            newprod = new NormalizedProduction { nt_list[clause_index], Location::none };
            newprod->set_rhs(group_clause->contents);
            prod_accum.push_back(newprod);
            break;
        }
        case Tok::lbrace:
        {
            // Add  N_k -> EPSILON
            //      N_k -> <seq> N_k
            auto newprod = new NormalizedProduction { nt_list[clause_index], Location::none };
            newprod->set_rhs(new Epsilon);
            prod_accum.push_back(newprod);
            newprod  = new NormalizedProduction { nt_list[clause_index], Location::none };
            auto seq = new ItemSequence;
            if (auto gcseq = group_clause->contents->only_if<ItemSequence>())
            {
                for (auto item : gcseq->sequence) { seq->sequence.push_back(item); }
            }
            else
            {
                seq->sequence.push_back(group_clause->contents);
            }
            seq->sequence.push_back(nt_list[clause_index]);
            newprod->set_rhs(seq);
            prod_accum.push_back(newprod);
            break;
        }
        default: assert(false && "unreached");
        }
        ++clause_index;
    }
}
// convert A: a1| a2 | ... aN to
//      A -> a1
//      A -> a2
// ...
// Also, make sure Epsilon or ItemSeq are the only two type on RHS, even for single
// items. This simplifies things in the rest of normalization.
void remove_alternatives(const NormalizedProductionList& prodlist, NormalizedProductionList& result)
{

    for (auto prod : prodlist)
    {
        if (Debug) printf("*** removing alternatives for %s\n", prod->to_string().c_str());

        if (prod->rhs()->is<Alternatives>())
        {
            auto alternatives = prod->rhs()->as<Alternatives>()->alternatives;
            for (auto i = 0U; i < alternatives.size(); ++i)
            {
                auto nprod = new NormalizedProduction { prod->lhs(), prod->location() };
                if (alternatives[i]->is<ItemSequence>() || alternatives[i]->is<Epsilon>())
                { nprod->set_rhs(alternatives[i]); }
                else
                {
                    auto seq = new ItemSequence;
                    seq->sequence.push_back(alternatives[i]);
                    nprod->set_rhs(seq);
                }
                result.push_back(nprod);
            }
        }
        else
        {
            auto nprod = new NormalizedProduction { prod->lhs(), prod->location() };
            if (prod->rhs()->is<ItemSequence>() || prod->rhs()->is<Epsilon>())
            { nprod->set_rhs(prod->rhs()); }
            else
            {
                // Single item: normalize by creating an item sequence of 1 element.
                auto seq = new ItemSequence;
                seq->sequence.push_back(prod->rhs());
                nprod->set_rhs(seq);
            }
            result.push_back(nprod);
        }
    }
}

using NormalizedProductionMap
    = std::multimap<const NonTerminal*, const NormalizedProduction*, CmpItem>;

using NormalizedProductionSet
    = std::set<const NormalizedProduction*>; // note: uses pointer comparison for keys.

// Given a production, walk its non-terminals recursively recording all visited
// productions in "visited_set".
void walk_productions(const NormalizedProduction*    prod,
                      const NormalizedProductionMap& prodmap,
                      NormalizedProductionSet&       visited)
{
    if (visited.find(prod) != visited.end()) return;
    visited.insert(prod);
    if (auto itemseq = prod->rhs()->only_if<ItemSequence>())
        for (auto item : itemseq->sequence)
            if (auto nt = item->only_if<NonTerminal>())
                if (auto iter = prodmap.find(nt); iter != prodmap.end())
                    while (iter != prodmap.end() && iter->first->name() == nt->name())
                    {
                        walk_productions(iter->second, prodmap, visited);
                        ++iter;
                    }
}

// Check for undefined names and unreachable productions. Return false iff any such
// cases are found and issue appropriate error messages.
bool check_productions(const NormalizedProductionList& prodlist)
{
    // Map of non-terminals to their productions.
    std::multimap<const NonTerminal*, const NormalizedProduction*, CmpItem> prodmap;

    // Pass 1: Map non-terminals to their productions.
    for (auto prod : prodlist) prodmap.emplace(prod->lhs(), prod);

    bool result = true;

    // Pass 2: Go through all productions and look up non-terminals; diagnose ones for
    // where there are no productions.
    for (auto prod : prodlist)
    {
        if (prod->rhs()->is<Epsilon>()) continue;
        for (auto item : prod->rhs()->as<ItemSequence>()->sequence)
            if (auto nt = item->only_if<NonTerminal>())
                // Find the production for the non-terminal; there might be multiple but
                // we record only one in the map above since we compare by name.
                if (auto iter = prodmap.find(nt); iter == prodmap.end())
                {
                    printf("%s(%zu): error: undefined name '%s'\n",
                           grammar_file_name,
                           prod->location().line(),
                           nt->name().c_str());
                    result = false;
                }
    }
    // Step 3. Starting from the first production symbol, recursively walk the
    // productions and diagnose productions which are not reached.
    NormalizedProductionSet visited_set;
    walk_productions(prodlist[0], prodmap, visited_set);
    for (auto prod : prodlist)
        if (visited_set.find(prod) == visited_set.end())
        {
            if (prod->location().line()
                != Location::none) // invented nonterms have this line number
                printf("%s(%zu): warning: unreachable production '%s'\n",
                       grammar_file_name,
                       prod->location().line(),
                       prod->lhs()->name().c_str());
        }
    return result;
}

// Given the read-in list of productions in EBNF form, normalize the productions
// to BNF form where,
//      - No production alternatives
//      - No production has () [] or {} forms
//      - Only right recursion is used.
// Return pair<normalized-productions, flag> where flag is true iff no errors are
// detected.
std::pair<NormalizedProductionList, bool> normalize(const ProductionList& prodlist)
{
    NormalizedProductionList result;

    for (auto& orig_prod : prodlist)
    {
        auto nprod = new NormalizedProduction { orig_prod->lhs(), orig_prod->location() };
        nprod->set_rhs(orig_prod->rhs());
        result.push_back(nprod);
    }

    bool     something_to_do = true;
    unsigned name_seq        = 1;
    while (something_to_do)
    {
        // Step 1: remove alternatives and replace by expanded productions.
        NormalizedProductionList loc_result;
        remove_alternatives(result, loc_result);
        std::swap(result, loc_result);
        bool alternatives_removed = (result.size() != loc_result.size());

        if (Debug)
            printf("*** after removing alternatives:\n%s\n", Parser::to_string(result).c_str());

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
        // For each sequence-or-optional-clause on the RHS we replace by invented
        // non-terminals and for each such non-terminal add the above productions. Since
        // <beta> in the above cases itself can contain an alternative clause, we need
        // to do this iteratively till there are no such clauses left.
        NormalizedProductionList invented_productions;
        for (auto normprod : result)
        {
            // clause_items: list of all sequence or optional clauses
            // locations: list of all the indices in normprod they occur at
            auto [clause_items, locations] = find_sequence_or_optional_clause(normprod);
            assert(clause_items.size() == locations.size());
            auto [new_seq, invented_nonterminals]
                = replace_grouped_clauses(normprod->rhs(), locations, name_seq);
            if (invented_nonterminals.size() > 0)
            {
                auto new_item_seq      = new ItemSequence;
                new_item_seq->sequence = std::move(new_seq);
                normprod->set_rhs(new_item_seq);
            }
            generate_invented_productions(
                invented_nonterminals, clause_items, invented_productions);
            name_seq += invented_nonterminals.size();
        }
        something_to_do = invented_productions.size() > 0 || alternatives_removed;
        result.insert(result.end(), invented_productions.begin(), invented_productions.end());
        if (Debug)
            printf("*** after transforming sequences:\n%s\n", Parser::to_string(result).c_str());
    }

    // Final checks.
    for (auto prod : result)
    { assert(prod->rhs()->is<Epsilon>() || prod->rhs()->is<ItemSequence>()); }

    if (!check_productions(result)) return { result, false };

    return { result, true };
}

// Read in the grammar file and return the list of productions.
ProductionList read_grammar(const char* fname)
{
    FILE* fp = fopen(fname, "r");
    if (fp == nullptr)
    {
        perror(("can't open: "s + fname).c_str());
        exit(2);
    }
    Scanner scn { fp, fname };
    Parser  p { scn };
    p.parse();
    return p.productions();
}

struct TopDownParsingSets
{
    NonTerminalSet                  epsilon_;
    ItemToTerminalSetMap            predict_, follow_;
    bool                            any_changes_ = false;
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
    bool is_seq_nullable(const ItemList& seq)
    {
        for (auto&& elem : seq)
        {
            // This is not a non-terminal. Sequence cannot be nullable.
            if (!elem->only_if<NonTerminal>()) return false;

            if (epsilon_.find(elem->as<NonTerminal>()) == epsilon_.end()) return false;
        }
        return true;
    }
    // EPSILON contains:
    //  - Any non-terminal that has an epsilon production
    //  - Any non-terminal that has a production containing only non-terminals that are
    //  in EPSILON
    void do_epsilon()
    {
        for (auto&& P : prods_)
        {
            if (P->rhs()->is<Epsilon>())
            {
                auto [_, inserted] = epsilon_.insert(P->lhs());
                any_changes_ |= inserted;
            }
            else if (is_seq_nullable(P->rhs()->as<ItemSequence>()->sequence))
            {
                auto [_, inserted] = epsilon_.insert(P->lhs());
                any_changes_ |= inserted;
            }
        }
    }

    void insert_into_set(ItemToTerminalSetMap& mapref, const Item* item, const Terminal* t)
    {
        auto iter = mapref.find(item);
        if (iter == mapref.end())
        {
            auto ptr = new TerminalSet;
            ptr->insert(t);
            mapref[item] = ptr;
            any_changes_ = true;
        }
        else
        {
            auto [_, inserted] = (*iter).second->insert(t);
            any_changes_ |= inserted;
        }
    }

    void insert_into_predict_set(const Item* item, const Terminal* t)
    {
        insert_into_set(predict_, item, t);
    }

    void insert_into_follow_set(const Item* item, const Terminal* t)
    {
        insert_into_set(follow_, item, t);
    }

    static TerminalSet& get_set_for(ItemToTerminalSetMap& mapref, const NonTerminal* nt)
    {
        auto iter = mapref.find(nt);

        if (iter == mapref.end())
        {
            auto ptr   = new TerminalSet;
            mapref[nt] = ptr;
            return *ptr;
        }
        return *(*iter).second;
    }

    TerminalSet& predict_set_for(const NonTerminal* nt) { return get_set_for(predict_, nt); }

    TerminalSet& follow_set_for(const NonTerminal* nt) { return get_set_for(follow_, nt); }

    // PREDICT(A) contains:
    //  - A terminal that appears leftmost in a production for A
    //  - For every non-terminal B that appears leftmost in a production for A,
    //    every terminal in PREDICT(B)
    //  - if A =>* epsilon, i.e., A is in EPSILON, every token in FOLLOW(A).
    void do_predict()
    {
        for (auto&& P : prods_)
        {
            if (Debug) printf("*** predict: prod'n: %s\n", P->to_string().c_str());

            if (P->rhs()->is<Epsilon>()) continue;
            auto&& first_elem = P->rhs()->as<ItemSequence>()->sequence[0];
            if (auto t = first_elem->only_if<Terminal>()) { insert_into_predict_set(P->lhs(), t); }
            else if (auto elem = first_elem->only_if<NonTerminal>())
            {
                for (auto&& t : predict_set_for(elem)) { insert_into_predict_set(P->lhs(), t); }
            }
            if (epsilon_.find(P->lhs()) != epsilon_.end())
            {
                for (auto&& terminal : follow_set_for(P->lhs()))
                { insert_into_predict_set(P->lhs(), terminal); }
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
        for (auto&& P : prods_)
        {
            if (Debug) printf("*** follow: prod'n: %s\n", P->to_string().c_str());

            if (P->rhs()->is<Epsilon>()) continue;
            auto&& items = P->rhs()->as<ItemSequence>()->sequence;
            for (auto it = items.begin(); it != items.end(); ++it)
            {
                if (auto* A = (*it)->only_if<NonTerminal>())
                {
                    if (it + 1 != items.end())
                    {
                        // What follows A?
                        auto it2 = it + 1;
                        if (auto* tptr = (*it2)->only_if<Terminal>())
                            insert_into_follow_set(A, tptr);
                        else if (auto* B = (*it2)->only_if<NonTerminal>())
                        {
                            for (auto t : predict_set_for(B)) { insert_into_follow_set(A, t); }
                        }
                        else
                        {
                            assert(false && "unreached");
                        }
                    }
                    else
                    {
                        // A is rightmost in the production
                        for (auto t : follow_set_for(P->lhs())) { insert_into_follow_set(A, t); }
                    }
                }
            }
        }
    }

    // The end of file symbol is in the follow set of the grammar start symbol which
    // we take as the LHS of the first production.
    void do_follow_special_rule()
    {
        auto first_prod  = prods_[0];
        auto end_of_file = new Terminal { "end-of-file" };
        insert_into_follow_set(first_prod->lhs(), end_of_file);
    }

    void compute()
    {
        do_follow_special_rule();
        do
        {
            any_changes_ = false;
            do_epsilon();
            do_predict();
            do_follow();
        } while (any_changes_);
    }

    std::string epsilon_set_text()
    {
        std::string buf = "EPSILON = {\n";
        for (auto nt : epsilon_)
        {
            buf += nt->name();
            buf += ",\n";
        }
        buf += "}\n";
        return buf;
    }

    static std::string text_for_set(const char* name, const ItemToTerminalSetMap& mapref)
    {
        std::string buf;
        for (auto&& entry : mapref)
        {
            if (auto ptr = entry.first->only_if<NonTerminal>())
            {
                buf += name;
                buf += "(";
                buf += ptr->name();
                buf += ") = { ";
                for (auto t : *entry.second)
                {
                    buf += t->name();
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
    if (Debug) printf("%s\n", Parser::to_string(prods).c_str());
    auto [normprods, success] = normalize(prods);
    if (!success) return;
    if (dump_normalized_grammar)
    {
        printf("after normalizing:\n");
        printf("%lu productions\n", normprods.size());
        printf("%s\n", Parser::to_string(normprods).c_str());
    }
    TopDownParsingSets parsing_sets { normprods };
    parsing_sets.compute();
    if (dump_epsilon_sets) printf("%s", parsing_sets.epsilon_set_text().c_str());
    if (dump_first_sets) printf("%s", parsing_sets.predict_set_text().c_str());
    if (dump_follow_sets) printf("%s", parsing_sets.follow_set_text().c_str());
}

int usage(const char* pname)
{
    fprintf(
        stderr, "usage: %s [--normalized] [--predict] [--follow] [--all] GRAMMAR-FILE\n", pname);
    return 2;
}

void opts(int argc, char* argv[])
{
    for (int i = 1; i < argc; ++i)
    {
        if ("--normalized"s == argv[i])
            dump_normalized_grammar = true;
        else if ("--epsilon"s == argv[i])
            dump_epsilon_sets = true;
        else if ("--predict"s == argv[i])
            dump_first_sets = true;
        else if ("--follow"s == argv[i])
            dump_follow_sets = true;
        else if ("--all"s == argv[i])
        {
            dump_normalized_grammar = dump_epsilon_sets = dump_first_sets = dump_follow_sets = true;
        }
        else if (grammar_file_name != nullptr)
            exit(usage(argv[0]));
        else
            grammar_file_name = argv[i];
    }
    if (grammar_file_name == nullptr) exit(usage(argv[0]));
}

int main(int argc, char* argv[])
{
    if (argc < 2) exit(usage(argv[0]));
    try
    {
        opts(argc, argv);
        compute_first_sets(read_grammar(grammar_file_name));
    }
    catch (Bad_token_error)
    {
    }
    catch (Syntax_error)
    {
    }
}