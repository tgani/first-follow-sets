#pragma once

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
        : Item { ItemKind::epsilon, "EPSILON" }
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
