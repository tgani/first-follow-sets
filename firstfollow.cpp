#include "firstfollow.h"

static constexpr bool Debug = false;

using namespace std::string_literals;

// Global values set by options
const char* grammar_file_name;
bool        dump_normalized_grammar;
bool        dump_epsilon_sets;
bool        dump_predict_sets;
bool        dump_first_sets;
bool        dump_follow_sets;
bool        dump_conflicts;

// Comparsion for Item. We compare simply by name.
struct CmpItem
{
    bool operator()(const Item* lhs, const Item* rhs) const { return lhs->name() < rhs->name(); }
};
// Type representing function from terminal/non-terminals -> set of terminals, used in
// computing FIRST and FOLLOW
using TerminalSet          = std::set<const Item*, CmpItem>; // Epsilon or Terminals
using NonTerminalSet       = std::set<const NonTerminal*, CmpItem>;
using ItemToTerminalSetMap = std::map<const Item*, TerminalSet*, CmpItem>;

// Given a production without alternatives (it may not be completely normalized yet),
// find all the places where {} [] or () occur and return all such grouped clauses and
// their indices in the ItemSequence. If an epsilon is on the RHS, it will be the only
// item.
//
// E.g., A -> B C D { E F } G [H I] will return std::pair<first={E F} [H I], second=0,
// 5>
//
std::pair<ItemList, std::vector<size_t>>
find_sequence_or_optional_clause(const NormalizedProduction* prod)
{
    ItemList            clauses;
    std::vector<size_t> locations;
    size_t              loc = 0;
    auto                rhs = prod->rhs();
    assert(rhs->is<Epsilon>() || rhs->is<ItemSequence>());
    if (rhs->is<Epsilon>())
    {
        return { {}, {} };
    }
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
replace_grouped_clauses(const Item* rhs, const std::vector<size_t>& locations, size_t name_seq)
{
    if (locations.size() == 0)
        return { {}, {} };
    ItemList                  itemseq;
    auto&&                    prodseq = rhs->as<ItemSequence>()->sequence;
    std::vector<NonTerminal*> invented_nonterminals;
    size_t                    i = 0;
    for (auto loc : locations)
    {
        while (i < loc)
        {
            itemseq.push_back(prodseq[i]);
            ++i;
        }
        // Create new non-terminal N_k and replace sequence-or-optional clause with it.
        auto nt = new NonTerminal{ "N"s + std::to_string(name_seq) };
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

// Add new productions for the invented nonterminals. E.g., for the above example, we
// shall return with
//      prod_accum=
//              N1 -> EPSILON
//              N1 -> B C N1
//              N2 -> EPSILON
//              N2 -> H I
void generate_invented_productions(const std::vector<NonTerminal*>& nt_list,
    const ItemList&                                                 seq_clause_list,
    NormalizedProductionList&                                       prod_accum)
{
    size_t clause_index = 0;
    assert(nt_list.size() == seq_clause_list.size());
    for (auto& item : seq_clause_list)
    {
        assert(item->is<GroupedItemSequence>());
        auto group_clause = item->as<GroupedItemSequence>();

        switch (group_clause->group_kind())
        {
            case Tok::lparen:
            {

                auto newprod = new NormalizedProduction{ nt_list[clause_index], Location::none };
                newprod->set_rhs(group_clause->contents);
                prod_accum.push_back(newprod);
                break;
            }
            case Tok::lbrack:
            {
                // Add  N_k -> EPSILON
                //      N_k -> <seq>
                auto newprod = new NormalizedProduction{ nt_list[clause_index], Location::none };
                newprod->set_rhs(new Epsilon);
                prod_accum.push_back(newprod);
                newprod = new NormalizedProduction{ nt_list[clause_index], Location::none };
                newprod->set_rhs(group_clause->contents);
                prod_accum.push_back(newprod);
                break;
            }
            case Tok::lbrace:
            {
                // Add  N_k -> EPSILON
                //      N_k -> <seq> N_k
                auto newprod = new NormalizedProduction{ nt_list[clause_index], Location::none };
                newprod->set_rhs(new Epsilon);
                prod_accum.push_back(newprod);
                newprod  = new NormalizedProduction{ nt_list[clause_index], Location::none };
                auto seq = new ItemSequence;
                if (auto gcseq = group_clause->contents->only_if<ItemSequence>())
                {
                    for (auto item : gcseq->sequence)
                    {
                        seq->sequence.push_back(item);
                    }
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
            default:
                assert(false && "unreached");
        }
        ++clause_index;
    }
}
// convert A: a1| a2 | ... aN to
//      A -> a1
//      A -> a2
// ...
// Also, make sure Epsilon or ItemSeq are the only two types on RHS, even for single
// items. This simplifies things in the rest of normalization.
void remove_alternatives(const NormalizedProductionList& prodlist, NormalizedProductionList& result)
{

    for (auto prod : prodlist)
    {
        if (Debug)
            printf("*** removing alternatives for %s\n", prod->to_string().c_str());

        if (prod->rhs()->is<Alternatives>())
        {
            auto alternatives = prod->rhs()->as<Alternatives>()->alternatives;
            for (auto i = 0U; i < alternatives.size(); ++i)
            {
                auto nprod = new NormalizedProduction{ prod->lhs(), prod->location() };
                if (alternatives[i]->is<ItemSequence>() || alternatives[i]->is<Epsilon>())
                {
                    nprod->set_rhs(alternatives[i]);
                }
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
            auto nprod = new NormalizedProduction{ prod->lhs(), prod->location() };
            if (prod->rhs()->is<ItemSequence>() || prod->rhs()->is<Epsilon>())
            {
                nprod->set_rhs(prod->rhs());
            }
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

using NormalizedProductionMap = std::multimap<const NonTerminal*, const NormalizedProduction*, CmpItem>;

using NormalizedProductionSet = std::set<const NormalizedProduction*>; // note: uses pointer comparison for keys.

// Given a production, walk its non-terminals recursively recording all visited
// productions in "visited_set".
void walk_productions(const NormalizedProduction* prod,
    const NormalizedProductionMap&                prodmap,
    NormalizedProductionSet&                      visited)
{
    if (visited.find(prod) != visited.end())
        return;
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
    for (auto prod : prodlist)
        prodmap.emplace(prod->lhs(), prod);

    bool result = true;

    // Pass 2: Go through all productions and look up non-terminals; diagnose ones for
    // where there are no productions.
    for (auto prod : prodlist)
    {
        if (prod->rhs()->is<Epsilon>())
            continue;
        bool first_item = true;
        for (auto item : prod->rhs()->as<ItemSequence>()->sequence)
        {
            if (auto nt = item->only_if<NonTerminal>(); first_item && nt != nullptr && nt->name() == prod->lhs()->name())
            {
                printf("%s(%zu): warning: production '%s' is left recursive\n",
                    grammar_file_name,
                    prod->location().line(),
                    prod->lhs()->name().c_str());
            }
            first_item = false;
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
    }
    // Pass 3. Starting from the first production symbol, recursively walk the
    // productions and diagnose productions which are not reached.
    NormalizedProductionSet visited_set;
    walk_productions(prodlist[0], prodmap, visited_set);
    for (auto prod : prodlist)
        if (visited_set.find(prod) == visited_set.end())
            if (prod->location().line() != Location::none) // invented nonterms have this line number
                printf("%s(%zu): warning: unreachable production '%s'\n",
                    grammar_file_name,
                    prod->location().line(),
                    prod->lhs()->name().c_str());

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
        auto nprod = new NormalizedProduction{ orig_prod->lhs(), orig_prod->location() };
        nprod->set_rhs(orig_prod->rhs());
        result.push_back(nprod);
    }

    bool   something_to_do = true;
    size_t name_seq        = 1;
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
            auto [new_seq, invented_nonterminals] = replace_grouped_clauses(normprod->rhs(), locations, name_seq);
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
    {
        assert(prod->rhs()->is<Epsilon>() || prod->rhs()->is<ItemSequence>());
    }

    if (!check_productions(result))
        return { result, false };

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
    Scanner scn{ fp, fname };
    Parser  p{ scn };
    p.parse();
    return p.productions();
}

struct TopDownParsingSets
{
    NonTerminalSet           epsilon_;
    ItemToTerminalSetMap     predict_, follow_, first_;
    bool                     any_changes_ = false;
    NormalizedProductionList prods_;

    TopDownParsingSets(const NormalizedProductionList& prods)
        : prods_{ prods }
    {
    }

    bool is_member_of_epsilon(const Item* item)
    {
        if (item->is<Terminal>())
        {
            return false;
        }
        return epsilon_.find(item->as<NonTerminal>()) != epsilon_.end();
    }

    // For a production A -> N1 N2 ...  return true iff all of N1 N2 ... derive
    // epsilon. Note that due to the current knowledge of which of N1 N2
    // ... derive epsilon we may return a false negative but that's ok due to
    // the iterative nature of the process.
    bool is_seq_nullable(const ItemList& seq, size_t limit)
    {
        assert(limit <= seq.size());
        for (size_t i = 0; i < limit; ++i)
        {
            auto& elem = seq[i];
            // This is not a non-terminal. Sequence cannot be nullable.
            if (!elem->is<NonTerminal>())
                return false;

            if (!is_member_of_epsilon(elem))
                return false;
        }
        return true;
    }

    bool is_seq_nullable(const ItemList& seq)
    {
        return is_seq_nullable(seq, seq.size());
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

    void insert_into_set(ItemToTerminalSetMap& mapref, const Item* item, const Item* t)
    {
        assert(item->is<NonTerminal>());
        assert(t->is<Epsilon>() || t->is<Terminal>());
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

    void insert_into_predict_set(const Item* item, const Item* t)
    {
        insert_into_set(predict_, item, t);
    }

    void insert_into_first_set(const Item* item, const Item* t)
    {
        insert_into_set(first_, item, t);
    }

    void insert_into_follow_set(const Item* item, const Item* t)
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
    TerminalSet& first_set_for(const NonTerminal* nt) { return get_set_for(first_, nt); }
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
            if (Debug)
                printf("*** predict: prod'n: %s\n", P->to_string().c_str());

            if (P->rhs()->is<Epsilon>())
                continue;
            auto&& first_elem = P->rhs()->as<ItemSequence>()->sequence[0];
            if (auto t = first_elem->only_if<Terminal>())
            {
                insert_into_predict_set(P->lhs(), t);
            }
            else if (auto elem = first_elem->only_if<NonTerminal>())
            {
                for (auto&& t : predict_set_for(elem))
                {
                    insert_into_predict_set(P->lhs(), t);
                }
            }
            if (is_member_of_epsilon(P->lhs()))
            {
                for (auto&& terminal : follow_set_for(P->lhs()))
                {
                    insert_into_predict_set(P->lhs(), terminal);
                }
            }
        }
    }

    // First set S of a sequence Y_1 Y_2 ... Y_k is computed as:
    //  - Add all the non-epsilon members of FIRST(Y_1) to S
    //  - If epsilon is in FIRST(Y_1), add all the members of FIRST(Y_2) to S
    //  - etc.
    // If all of Y_1 Y_2 ... Y_k contains epsilon, add epsilon to S
    TerminalSet first_set_of_seq(const ItemList& seq, size_t start_index)
    {
        // Examine a non-terminal sym and return true iff FIRST(sym) contains
        // epsilon. Add non-epsilon symbols of sym to FIRST(P->lhs()) as a
        // side effect.
        TerminalSet result;
        auto        collect_first_set_of_symbol = [&result, this](const Item* sym) -> bool {
            if (sym->is<Terminal>())
            {
                result.insert(sym);
                return false;
            }
            auto& first_Y1         = first_set_for(sym->as<NonTerminal>());
            bool  contains_epsilon = false;
            for (auto&& item : first_Y1)
            {
                if (item->is<Epsilon>())
                {
                    contains_epsilon = true;
                    continue;
                }
                assert(item->is<Terminal>());
                result.insert(item);
            }
            return contains_epsilon;
        };

        bool prior_contains_epsilon = collect_first_set_of_symbol(seq[start_index]);
        for (size_t i = start_index + 1; i < seq.size(); ++i)
        {
            if (!prior_contains_epsilon)
                break;
            prior_contains_epsilon = collect_first_set_of_symbol(seq[i]);
        }
        if (prior_contains_epsilon)
            result.insert(new Epsilon);
        return result; // prior_contains_epsilon is true here IFF all contain epsilon
    }

    // FIRST(A) is defined as:
    //  - if A is a terminal FIRST(A) = { A }
    //  - if A -> epsilon is a production, then add epsilon to FIRST(A)
    //  - if A is a non-terminal A -> Y1 Y2 ... Yk is a production and
    //    all of Y1 Y2 ... Yk are in EPSILON, then add epsilon to FIRST(A)
    //  - if A is a non-terminal and A -> Y1 Y2 ... Yk is a production then
    //    add a to FIRST(A) if a is in FIRST(Yj) j < k and epsilon is in all
    //    of FIRST(Yi) for 1 <= i < j.
    // We shall not implement the first bullet since this is implicit.
    void do_first()
    {
        for (auto&& P : prods_)
        {
            if (Debug)
                printf("*** first : prod'n: %s\n", P->to_string().c_str());
            if (P->rhs()->is<Epsilon>())
            {
                insert_into_first_set(P->lhs(), P->rhs());
                continue;
            }
            auto& seq = P->rhs()->as<ItemSequence>()->sequence;
            // Compute S = FIRST(Y_1 Y_2 ... Y_k) as follows:
            //  - Add all the non-epsilon symbols of FIRST(X1) to S
            //  - for all i from 2 to k do
            //      if FIRST(Y_{i-1}) contains epsilon then
            //          add all non-epsilon symbols of Y_i to S
            //  - if all of FIRST(Y_1) to FIRST(Y_k) contain epsilon then
            //      add epsilon to S
            // Finally, FIRST(A) = FIRST(A) union S, which we do simply by
            // using FIRST(A) for S.

            for (auto&& item : first_set_of_seq(seq, 0))
                insert_into_first_set(P->lhs(), item);
        }
    }

    // FOLLOW(A) contains:
    //  - Any terminal that immediately follows A in any production
    //  - For any non-terminal B that immediately follows A in any production,
    //    all the tokens in PREDICT(B)
    //  - For any non-terminal B such that A appears rightmost in a production for
    //    B, every token in FOLLOW(B)

    // FOLLOW(A) is defined as:
    //  - the end of input marker is in FOLLOW(S), the start symbol
    //  - if there is a production A -> alpha B beta, then everything in FIRST(beta) is
    //    in FOLLOW(B)
    //  - if there is a producton A -> alpha B or a production A -> alpha B beta where
    //    FIRST(beta) contains epsilon, then everything in FOLLOW(A) is in FOLLOW(B)
    void do_follow()
    {
#if 0
        for (auto&& P : prods_)
        {
            if (Debug)
                printf("*** follow: prod'n: %s\n", P->to_string().c_str());

            if (P->rhs()->is<Epsilon>())
                continue;
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
                            for (auto t : predict_set_for(B))
                            {
                                insert_into_follow_set(A, t);
                            }
                        }
                        else
                        {
                            assert(false && "unreached");
                        }
                    }
                    else
                    {
                        // A is rightmost in the production
                        for (auto t : follow_set_for(P->lhs()))
                        {
                            insert_into_follow_set(A, t);
                        }
                    }
                }
            }
        }
#else
        for (auto&& P : prods_)
        {
            auto A = P->lhs();
            if (P->rhs()->is<Epsilon>())
                continue;
            auto& seq = P->rhs()->as<ItemSequence>()->sequence;

            for (size_t i = 0; i < seq.size(); ++i)
            {
                if (auto&& B = seq[i]->only_if<NonTerminal>())
                {
                    bool contains_epsilon = false;
                    if (i < seq.size() - 1)
                    {
                        for (auto&& item : first_set_of_seq(seq, i + 1))
                        {
                            if (item->is<Epsilon>())
                            {
                                contains_epsilon = true;
                                continue;
                            }
                            insert_into_follow_set(B, item);
                        }
                    }
                    if (contains_epsilon || i == seq.size() - 1)
                    {
                        auto& follow_lhs = follow_set_for(A);
                        for (auto&& item : follow_lhs)
                        {
                            insert_into_follow_set(B, item);
                        }
                    }
                }
            }
        }
#endif
    }

    // The end of file symbol is in the follow set of the grammar start symbol which
    // we take as the LHS of the first production.
    void do_follow_special_rule()
    {
        auto first_prod  = prods_[0];
        auto end_of_file = new Terminal{ "end-of-file" };
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
            do_first();
            do_follow();
            if (Debug)
            {
                printf("****\n");
                printf("%s", epsilon_set_text().c_str());
                printf("%s", predict_set_text().c_str());
                printf("%s", first_set_text().c_str());
                printf("%s", follow_set_text().c_str());
                printf("+++++\n");
            }
        } while (any_changes_);
    }

    const TerminalSet find_first_set(const NormalizedProduction* normprod)
    {
        NonTerminal* nt{};

        bool first = true;

        if (normprod->rhs()->is<Epsilon>())
        {
            return TerminalSet{};
        }

        for (auto&& item : normprod->rhs()->as<ItemSequence>()->sequence)
        {
            assert(!item->is<Epsilon>());
            if (first && item->is<Terminal>())
            {
                // A -> a <alpha>; first set is { a }
                TerminalSet result;
                result.insert(item->as<Terminal>());
                return result;
            }
            if (item->is<NonTerminal>())
            {
                // A -> B <alpha>
                if (is_member_of_epsilon(item->as<NonTerminal>()))
                {
                    return follow_set_for(item->as<NonTerminal>());
                }
                else
                {
                    return predict_set_for(item->as<NonTerminal>());
                }
            }
            first = false;
        }
        assert(false && "unreached");
        return TerminalSet{};
    }

    TerminalSet intersection(const TerminalSet& lhs, const TerminalSet& rhs)
    {
        TerminalSet result;
        for (auto&& item : lhs)
        {
            if (rhs.find(item) != rhs.end())
            {
                result.insert(item);
            }
        }
        return result;
    }

    // Check if the grammar is LL(1), that is parseable by recursive descent with 1-token of look-ahead. This is
    // done by computing predict sets for RHS of all productions for the same non-terminal
    // and looking for common elements.
    void check_LL1()
    {
        std::vector<const NormalizedProduction*> current_prod;

        struct CmpProds
        {
            bool operator()(const NormalizedProduction* lhs, const NormalizedProduction* rhs) const
            {
                return lhs->lhs()->name() < rhs->lhs()->name();
            }
        };

        std::sort(prods_.begin(), prods_.end(), CmpProds{});

        int num_conflicts = 0;

        for (auto&& P : prods_)
        {
            if (Debug)
            {
                puts("---");
                printf("P:\n\t%s\n", P->to_string().c_str());
                printf("Current prods:\n");
                for (auto&& PP : current_prod)
                {
                    printf("\t%s\n", PP->to_string().c_str());
                }
                puts("+++");
            }
            if (current_prod.empty())
            {
                if (!P->rhs()->is<Epsilon>())
                    current_prod.push_back(P);
                continue;
            }
            if (current_prod.back()->lhs()->name() != P->lhs()->name())
            {
                current_prod.clear();
                if (!P->rhs()->is<Epsilon>())
                    current_prod.push_back(P);
                continue;
            }
            // Names match. Check P against all productions with the same name seen so far
            auto first_set_of_P = find_first_set(P);
            for (auto&& P2 : current_prod)
            {
                auto first_set_of_P2 = find_first_set(P2);
                auto common_elems    = intersection(first_set_of_P, first_set_of_P2);
                if (!common_elems.empty())
                {
                    printf("conflict:\n\t%s\n\t%s\n", P->to_string().c_str(), P2->to_string().c_str());
                    printf("common elements: { ");
                    bool first = true;
                    for (auto elem : common_elems)
                    {
                        if (!first)
                            putchar(',');
                        printf("%s", elem->to_string().c_str());
                        first = false;
                    }
                    printf(" }\n\n");
                    ++num_conflicts;
                }
            }
            current_prod.push_back(P);
        }
        if (num_conflicts > 0)
            printf("%d conflicts\n", num_conflicts);
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
    std::string first_set_text() { return text_for_set("FIRST", first_); }
    std::string follow_set_text() { return text_for_set("FOLLOW", follow_); }
};

void compute_first_sets(const ProductionList& prods)
{
    printf("%zu productions\n", prods.size());
    if (Debug)
        printf("%s\n", Parser::to_string(prods).c_str());
    auto [normprods, success] = normalize(prods);
    if (!success)
        return;
    if (dump_normalized_grammar)
    {
        printf("after normalizing:\n");
        printf("%zu productions\n", normprods.size());
        printf("%s\n", Parser::to_string(normprods).c_str());
    }
    TopDownParsingSets parsing_sets{ normprods };
    parsing_sets.compute();
    if (dump_epsilon_sets)
        printf("%s", parsing_sets.epsilon_set_text().c_str());
    if (dump_predict_sets)
        printf("%s", parsing_sets.predict_set_text().c_str());
    if (dump_first_sets)
        printf("%s", parsing_sets.first_set_text().c_str());
    if (dump_follow_sets)
        printf("%s", parsing_sets.follow_set_text().c_str());
    if (dump_conflicts)
        parsing_sets.check_LL1();
}

int usage(const char* pname)
{
    fprintf(
        stderr, "usage: %s [--normalized] [--predict] [--first] [--follow] [--all] GRAMMAR-FILE\n", pname);
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
            dump_predict_sets = true;
        else if ("--first"s == argv[i])
            dump_first_sets = true;
        else if ("--follow"s == argv[i])
            dump_follow_sets = true;
        else if ("--conflict"s == argv[i])
            dump_conflicts = true;
        else if ("--all"s == argv[i])
        {
            dump_normalized_grammar = dump_epsilon_sets = dump_predict_sets =
                dump_first_sets = dump_follow_sets = dump_conflicts = true;
        }
        else if (grammar_file_name != nullptr)
            exit(usage(argv[0]));
        else
            grammar_file_name = argv[i];
    }
    if (grammar_file_name == nullptr)
        exit(usage(argv[0]));
}

int main(int argc, char* argv[])
{
    if (argc < 2)
        exit(usage(argv[0]));
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
