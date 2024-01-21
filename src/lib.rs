pub mod grammar;
pub mod parse;

#[macro_export]
macro_rules! next_tkn {
    ($l:expr) => {
        $l.next().ok_or(Error::Invalid)?
    };
}

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,

    Create,
    Table,

    // Should be Type(<type>)
    Int,

    Select,
    Insert,
    Update,
    Delete,
    Into,
    Values,
    From,
    Where,
    Join,
    On,
    Using,
    As,
    Conjunction,
    Disjunction,
    Negation,
    Null,
    Semicolon,
    Comma,
    All,
    In,
    Between,
    Is,

    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    StringLiteral(String),
    IntegerLiteral(u64),

    TableAndColumnReference(String, String),
    TableOrColumnReference(String),
}

impl Token {
    fn teq(&self, other: &Self) -> bool {
        match self {
            // Ignore inner
            Token::StringLiteral(_) => match other {
                Token::StringLiteral(_) => true,
                _ => false,
            },
            Token::IntegerLiteral(_) => match other {
                Token::IntegerLiteral(_) => true,
                _ => false,
            },
            Token::TableAndColumnReference(_, _) => match other {
                Token::TableAndColumnReference(_, _) => true,
                _ => false,
            },
            Token::TableOrColumnReference(_) => match other {
                Token::TableOrColumnReference(_) => true,
                _ => false,
            },

            t @ Token::Is
            | t @ Token::Between
            | t @ Token::In
            | t @ Token::LParen
            | t @ Token::RParen
            | t @ Token::Create
            | t @ Token::Table
            | t @ Token::Int
            | t @ Token::Select
            | t @ Token::Insert
            | t @ Token::Update
            | t @ Token::Delete
            | t @ Token::Into
            | t @ Token::Values
            | t @ Token::From
            | t @ Token::Where
            | t @ Token::Join
            | t @ Token::On
            | t @ Token::Using
            | t @ Token::As
            | t @ Token::Conjunction
            | t @ Token::Disjunction
            | t @ Token::Negation
            | t @ Token::Null
            | t @ Token::Semicolon
            | t @ Token::Comma
            | t @ Token::All
            | t @ Token::Eq
            | t @ Token::Neq
            | t @ Token::Lt
            | t @ Token::Le
            | t @ Token::Gt
            | t @ Token::Ge => t == other,
        }
    }
}

impl Into<Token> for &str {
    fn into(self) -> Token {
        let lower = self.to_lowercase();

        match &lower[..] {
            "(" => Token::LParen,
            ")" => Token::RParen,

            "create" => Token::Create,
            "table" => Token::Table,
            "int" => Token::Int,

            "select" => Token::Select,
            "insert" => Token::Insert,
            "update" => Token::Update,
            "delete" => Token::Delete,
            "into" => Token::Into,
            "values" => Token::Values,
            "from" => Token::From,
            "where" => Token::Where,
            "join" => Token::Join,
            "on" => Token::On,
            "using" => Token::Using,
            "as" => Token::As,
            "and" => Token::Conjunction,
            "or" => Token::Disjunction,
            "not" => Token::Disjunction,
            "null" => Token::Null,
            ";" => Token::Semicolon,
            "," => Token::Comma,
            "*" => Token::All,
            "in" => Token::In,
            "between" => Token::Between,
            "is" => Token::Is,

            "=" => Token::Eq,
            "!=" => Token::Neq,
            ">" => Token::Gt,
            ">=" => Token::Ge,
            "<" => Token::Lt,
            "<=" => Token::Le,

            s => {
                // Try integer
                if let Ok(n) = s.parse::<u64>() {
                    return Token::IntegerLiteral(n);
                }

                // Try string
                let mut cs = s.chars();
                if cs.next() == Some('"') && cs.next_back() == Some('"') {
                    return Token::StringLiteral(cs.collect());
                }

                // Try reference
                match s.split_once('.') {
                    Some((table, column)) => {
                        Token::TableAndColumnReference(table.into(), column.into())
                    }
                    None => Token::TableOrColumnReference(s.into()),
                }
            }
        }
    }
}

pub struct Lexer<'a> {
    src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src }
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.src.is_empty() {
            return None;
        }

        while !self.src.is_empty() {
            let s = chop(self.src);
            if s.is_empty() {
                self.src = &self.src[1..];
                continue;
            }

            self.src = &self.src[s.len()..];
            return Some(s.into());
        }

        None
    }

    pub fn to_vec(mut self) -> Vec<Token> {
        let mut tkns = Vec::new();
        while let Some(t) = self.next() {
            tkns.push(t);
        }

        tkns
    }
}

pub fn tokenise(mut src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();

    while !src.is_empty() {
        let s = chop(src);
        if s.is_empty() {
            src = &src[1..];
            continue;
        }

        tokens.push(s.into());

        src = &src[s.len()..];
    }

    tokens
}

macro_rules! check_str {
    ($str:ident, $i:expr, [ $($char:expr),* ]) => {
        (
            $(
                &$str[$i..$i+1] == $char ||
            )*
            false
        )
    };

    ($str:ident, $i:expr, $char:expr) => {
        (
            &$str[$i..$i+1] == $char
        )
    };
}

macro_rules! check {
    ($c:expr, [ $($char:expr),* ]) => {
        (
            $(
               $c == $char ||
            )*
            false
        )
    };
}

fn chop(src: &str) -> &str {
    let mut i = 0;

    if src.is_empty() {
        return "";
    }

    match &src[0..1] {
        c @ "," => return c,
        c @ "(" => return c,
        c @ ")" => return c,
        c @ ";" => return c,
        _ => {}
    };

    let mut iq = false;
    while i < src.len() {
        if !iq && check_str!(src, i, [" ", "\n", "(", ")", ","]) {
            break;
        }

        let q = check_str!(src, i, "\"");
        if q {
            iq = !iq
        }

        i += 1
    }

    let tmp = &src[0..i];
    if i != 0 && check_str!(tmp, tmp.len() - 1, [",", ")", ";"]) && tmp[0..i].len() > 1 {
        i -= 1
    }

    &src[0..i]
}

// Keep to compare later
#[allow(unused)]
fn chopc(src: &str) -> String {
    let cs: Vec<char> = src.chars().collect();

    let mut i = 0;

    if cs.is_empty() {
        return String::new();
    }

    match cs[0] {
        c @ ',' => return c.into(),
        c @ '(' => return c.into(),
        c @ ')' => return c.into(),
        c @ ';' => return c.into(),
        _ => {}
    };

    while i < cs.len() {
        if check!(cs[i], [' ', '\n', '(', ')', ',']) {
            break;
        }

        i += 1
    }

    let tmp = &cs[0..i];
    if i != 0 && check!(tmp[tmp.len() - 1], [',', ')', ';']) && tmp[0..i].len() > 1 {
        i -= 1
    }

    cs[0..i].iter().collect()
}

#[cfg(test)]
mod test {
    use crate::Lexer;

    use super::{chop, Token};

    #[test]
    fn test_tokenise() {
        struct TestCase {
            input: &'static str,
            want: Vec<Token>,
        }

        let tcs = [
            TestCase {
                input: "",
                want: vec![],
            },
            TestCase {
                input: "select update delete",
                want: vec![Token::Select, Token::Update, Token::Delete],
            },
            TestCase {
                input: "    select update delete",
                want: vec![Token::Select, Token::Update, Token::Delete],
            },
            TestCase {
                input: "select update delete    ",
                want: vec![Token::Select, Token::Update, Token::Delete],
            },
            TestCase {
                input: "select    update    delete",
                want: vec![Token::Select, Token::Update, Token::Delete],
            },
            TestCase {
                input: "select, update, delete",
                want: vec![
                    Token::Select,
                    Token::Comma,
                    Token::Update,
                    Token::Comma,
                    Token::Delete,
                ],
            },
            TestCase {
                input: "select   ,   update   ,   delete",
                want: vec![
                    Token::Select,
                    Token::Comma,
                    Token::Update,
                    Token::Comma,
                    Token::Delete,
                ],
            },
            TestCase {
                input: ")(",
                want: vec![Token::RParen, Token::LParen],
            },
            TestCase {
                input: ";,)(",
                want: vec![Token::Semicolon, Token::Comma, Token::RParen, Token::LParen],
            },
            TestCase {
                input: "\";,)(\n\"\nand",
                want: vec![Token::StringLiteral(";,)(\n".into()), Token::Conjunction],
            },
            TestCase {
                input: "into(table.columna,table.columnb)values(1,2);",
                want: vec![
                    Token::Into,
                    Token::LParen,
                    Token::TableAndColumnReference("table".into(), "columna".into()),
                    Token::Comma,
                    Token::TableAndColumnReference("table".into(), "columnb".into()),
                    Token::RParen,
                    Token::Values,
                    Token::LParen,
                    Token::IntegerLiteral(1),
                    Token::Comma,
                    Token::IntegerLiteral(2),
                    Token::RParen,
                    Token::Semicolon,
                ],
            },
            TestCase {
                input: "select * from tablea;",
                want: vec![
                    Token::Select,
                    Token::All,
                    Token::From,
                    Token::TableOrColumnReference("tablea".into()),
                    Token::Semicolon,
                ],
            },
            TestCase {
                input: "select tablea.columnA, tablea.columnB from tablea",
                want: vec![
                    Token::Select,
                    Token::TableAndColumnReference("tablea".into(), "columna".into()),
                    Token::Comma,
                    Token::TableAndColumnReference("tablea".into(), "columnb".into()),
                    Token::From,
                    Token::TableOrColumnReference("tablea".into()),
                ],
            },
            TestCase {
                input:
                    "select tablea.columnA, tablea.columnB from tablea where tablea.columnA = \"1234\"
                    and tablea.columnB > 1234",
                want: vec![
                    Token::Select,
                    Token::TableAndColumnReference("tablea".into(), "columna".into()),
                    Token::Comma,
                    Token::TableAndColumnReference("tablea".into(), "columnb".into()),
                    Token::From,
                    Token::TableOrColumnReference("tablea".into()),
                    Token::Where,
                    Token::TableAndColumnReference("tablea".into(), "columna".into()),
                    Token::Eq,
                    Token::StringLiteral("1234".into()),
                    Token::Conjunction,
                    Token::TableAndColumnReference("tablea".into(), "columnb".into()),
                    Token::Gt,
                    Token::IntegerLiteral(1234),
                ],
            },
            TestCase {
                input: "select tablea.columnA, tablea.columnB from tablea
                    join tableB on (tablea.columnA = tableB.columnA)
                    where tablea.columnA = \"1234\" and tablea.columnB > 1234",
                want: vec![
                    Token::Select,
                    Token::TableAndColumnReference("tablea".into(), "columna".into()),
                    Token::Comma,
                    Token::TableAndColumnReference("tablea".into(), "columnb".into()),
                    Token::From,
                    Token::TableOrColumnReference("tablea".into()),
                    Token::Join,
                    Token::TableOrColumnReference("tableb".into()),
                    Token::On,
                    Token::LParen,
                    Token::TableAndColumnReference("tablea".into(), "columna".into()),
                    Token::Eq,
                    Token::TableAndColumnReference("tableb".into(), "columna".into()),
                    Token::RParen,
                    Token::Where,
                    Token::TableAndColumnReference("tablea".into(), "columna".into()),
                    Token::Eq,
                    Token::StringLiteral("1234".into()),
                    Token::Conjunction,
                    Token::TableAndColumnReference("tablea".into(), "columnb".into()),
                    Token::Gt,
                    Token::IntegerLiteral(1234),
                ],
            },
            TestCase {
                input: "insert into tablea (columnA, columnB, columnC, columnD) values (\"a\", 1, \"b\", 2)",
                want: vec![
                    Token::Insert,
                    Token::Into,
                    Token::TableOrColumnReference("tablea".into()),
                    Token::LParen,
                    Token::TableOrColumnReference("columna".into()),
                    Token::Comma,
                    Token::TableOrColumnReference("columnb".into()),
                    Token::Comma,
                    Token::TableOrColumnReference("columnc".into()),
                    Token::Comma,
                    Token::TableOrColumnReference("columnd".into()),
                    Token::RParen,
                    Token::Values,
                    Token::LParen,
                    Token::StringLiteral("a".into()),
                    Token::Comma,
                    Token::IntegerLiteral(1),
                    Token::Comma,
                    Token::StringLiteral("b".into()),
                    Token::Comma,
                    Token::IntegerLiteral(2),
                    Token::RParen,
                ]
            },
            TestCase {
                input: "create table tablea (
                           columnA int,
                           columnB int
                           );",
                want: vec![
                    Token::Create,
                    Token::Table,
                    Token::TableOrColumnReference("tablea".into()),
                    Token::LParen,
                    Token::TableOrColumnReference("columna".into()),
                    Token::Int,
                    Token::Comma,
                    Token::TableOrColumnReference("columnb".into()),
                    Token::Int,
                    Token::RParen,
                    Token::Semicolon
                ]
            }
        ];

        for TestCase { input, want } in tcs {
            let have = Lexer::new(input).to_vec();
            assert!(want == have, "\nWant: {:?}\nHave: {:?}", want, have);
        }
    }

    #[test]
    fn test_chop() {
        struct TestCase {
            input: &'static str,
            want: &'static str,
        }

        let tcs = [
            TestCase {
                input: "",
                want: "",
            },
            TestCase {
                input: "select",
                want: "select",
            },
            TestCase {
                input: "select, ",
                want: "select",
            },
            TestCase {
                input: ", update",
                want: ",",
            },
            TestCase {
                input: ",",
                want: ",",
            },
            TestCase {
                input: "(table",
                want: "(",
            },
            TestCase {
                input: "table)",
                want: "table",
            },
        ];

        for TestCase { input, want } in tcs {
            let have = chop(input);
            assert!(want == have, "Want: {:?}, Have: {:?}", want, have);
        }
    }
}
