#pragma once

struct Syntax_error
{
};

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
			using namespace std::literals;
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

    template <typename List> 
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
