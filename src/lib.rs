#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,

    Select,
    Insert,
    Update,
    Delete,
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

    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    StringConst(String),
    NumericConst(u64),

    TableAndColumnReference(String, String),
    TableOrColumnReference(String),
}

impl Into<Token> for &str {
    fn into(self) -> Token {
        let lower = self.to_lowercase();

        match &lower[..] {
            "(" => Token::LParen,
            ")" => Token::RParen,

            "select" => Token::Select,
            "insert" => Token::Insert,
            "update" => Token::Update,
            "delete" => Token::Delete,
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

            "=" => Token::Eq,
            "!=" => Token::Neq,
            ">" => Token::Gt,
            ">=" => Token::Ge,
            "<" => Token::Lt,
            "<=" => Token::Le,

            s => {
                // Try numeric
                if let Ok(n) = s.parse::<u64>() {
                    return Token::NumericConst(n);
                }

                // Try string
                let mut cs = s.chars();
                if cs.next() == Some('"') && cs.next_back() == Some('"') {
                    return Token::StringConst(cs.collect());
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

pub fn tokenise(mut src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();

    while src.len() > 0 {
        let s = chop(src);
        if s.len() == 0 {
            src = &src[1..];
            continue;
        }

        dbg!(s);
        tokens.push(s.into());

        src = &src[s.len()..];
    }

    tokens
}

fn chop<'a>(src: &'a str) -> &'a str {
    let mut i = 0;

    if src.len() == 0 {
        return "";
    }

    match &src[0..1] {
        c @ "," => return c,
        c @ "(" => return c,
        c @ ")" => return c,
        _ => {}
    };

    while i < src.len() {
        if &src[i..i + 1] == " " || &src[i..i + 1] == "\n" {
            break;
        }

        i += 1
    }

    let tmp = &src[0..i];
    if i != 0
        && (&tmp[tmp.len() - 1..tmp.len()] == "," || &tmp[tmp.len() - 1..tmp.len()] == ")")
        && tmp[0..i].len() > 1
    {
        i -= 1
    }

    &src[0..i]
}

#[cfg(test)]
mod test {
    use super::{chop, tokenise, Token};

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
                input: "select * from table",
                want: vec![
                    Token::Select,
                    Token::All,
                    Token::From,
                    Token::TableOrColumnReference("table".into()),
                ],
            },
            TestCase {
                input: "select table.columnA, table.columnB from table",
                want: vec![
                    Token::Select,
                    Token::TableAndColumnReference("table".into(), "columna".into()),
                    Token::Comma,
                    Token::TableAndColumnReference("table".into(), "columnb".into()),
                    Token::From,
                    Token::TableOrColumnReference("table".into()),
                ],
            },
            TestCase {
                input:
                    "select table.columnA, table.columnB from table where table.columnA = \"1234\"
                    and table.columnB > 1234",
                want: vec![
                    Token::Select,
                    Token::TableAndColumnReference("table".into(), "columna".into()),
                    Token::Comma,
                    Token::TableAndColumnReference("table".into(), "columnb".into()),
                    Token::From,
                    Token::TableOrColumnReference("table".into()),
                    Token::Where,
                    Token::TableAndColumnReference("table".into(), "columna".into()),
                    Token::Eq,
                    Token::StringConst("1234".into()),
                    Token::Conjunction,
                    Token::TableAndColumnReference("table".into(), "columnb".into()),
                    Token::Gt,
                    Token::NumericConst(1234),
                ],
            },
            TestCase {
                input: "select table.columnA, table.columnB from table
                    join tableB on (table.columnA = tableB.columnA)
                    where table.columnA = \"1234\" and table.columnB > 1234",
                want: vec![
                    Token::Select,
                    Token::TableAndColumnReference("table".into(), "columna".into()),
                    Token::Comma,
                    Token::TableAndColumnReference("table".into(), "columnb".into()),
                    Token::From,
                    Token::TableOrColumnReference("table".into()),
                    Token::Join,
                    Token::TableOrColumnReference("tableb".into()),
                    Token::On,
                    Token::LParen,
                    Token::TableAndColumnReference("table".into(), "columna".into()),
                    Token::Eq,
                    Token::TableAndColumnReference("tableb".into(), "columna".into()),
                    Token::RParen,
                    Token::Where,
                    Token::TableAndColumnReference("table".into(), "columna".into()),
                    Token::Eq,
                    Token::StringConst("1234".into()),
                    Token::Conjunction,
                    Token::TableAndColumnReference("table".into(), "columnb".into()),
                    Token::Gt,
                    Token::NumericConst(1234),
                ],
            },
        ];

        for TestCase { input, want } in tcs {
            let have = tokenise(input);
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
