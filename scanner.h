#pragma once
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
