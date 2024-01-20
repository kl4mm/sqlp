use crate::{Lexer, Token};

#[derive(PartialEq, Debug)]
pub enum ColumnType {
    Int,
}

#[derive(PartialEq, Debug)]
struct ColumnDef {
    name: String,
    t: ColumnType,
}

// TODO: look at rust AST in macro
#[derive(PartialEq, Debug)]
pub enum Node {
    // Statements
    CreateStatement {
        rel_name: String,
        col_defs: Vec<Node>, // Of ColumnDef
    },

    SelectStatement {
        all: bool,
        targets: Vec<Node>, // Of ResTarget

        from_clause: Vec<Node>,
        where_clause: Box<Node>,
        group_clause: Vec<Node>,
        sort_clause: Vec<Node>,
        limit_count: Box<Node>,
        limit_offset: Box<Node>,
    },
    InsertStatement {},
    DeleteStatement {},
    UpdateStatement {},

    // Parse tree nodes
    ColumnDef {
        t: ColumnType,
        name: String,
        var_size: u8,
    },

    // PGResTarget
    ResTarget {
        name: Option<String>, // Name of column if any
        value: String,
    },

    ColumnRef {
        table: Option<String>,
        column: String,
    },

    TableRef {
        table: String,
    },

    Invalid,
}

#[derive(Debug)]
pub enum Error {
    Invalid,

    UnexpectedToken,
}

macro_rules! next_tkn {
    ($lexer:expr) => {
        $lexer.next().ok_or(Error::Invalid)?
    };
}

macro_rules! ptr {
    ($e:expr) => {
        Box::into_raw(Box::new($e))
    };
}

#[derive(Debug)]
enum GrammarTag {
    Targets,
    Table,
    None,
}

#[derive(Debug)]
struct GrammarNode {
    token: Token,
    adjacent: Vec<*mut GrammarNode>,
    tag: GrammarTag,
}

impl GrammarNode {
    pub fn select_stmt() -> *const Self {
        static mut SELECT_STMT: *const GrammarNode = std::ptr::null_mut();

        unsafe {
            if !SELECT_STMT.is_null() {
                return SELECT_STMT;
            }

            let end = ptr!(Self {
                token: Token::Semicolon,
                adjacent: vec![],
                tag: GrammarTag::None,
            });

            let where_clause = ptr!(Self {
                token: Token::Where,
                adjacent: vec![],
                tag: GrammarTag::None,
            });

            let from_clause = ptr!(Self {
                token: Token::From,
                adjacent: vec![ptr!(GrammarNode {
                    token: Token::TableOrColumnReference(String::new()),
                    adjacent: vec![where_clause, end],
                    tag: GrammarTag::Table,
                })],
                tag: GrammarTag::None,
            });

            let col_refs_1 = ptr!(GrammarNode {
                token: Token::TableOrColumnReference(String::new()),
                adjacent: vec![from_clause],
                tag: GrammarTag::Targets,
            });

            let col_refs_2 = ptr!(GrammarNode {
                token: Token::TableAndColumnReference(String::new(), String::new()),
                adjacent: vec![from_clause],
                tag: GrammarTag::None,
            });

            // Cycle
            let comma = ptr!(GrammarNode {
                token: Token::Comma,
                adjacent: vec![col_refs_1, col_refs_2, from_clause],
                tag: GrammarTag::None,
            });
            (*col_refs_1).adjacent.push(comma);
            (*col_refs_2).adjacent.push(comma);

            SELECT_STMT = ptr!(Self {
                token: Token::Select,
                adjacent: vec![
                    ptr!(Self {
                        token: Token::All,
                        adjacent: vec![from_clause],
                        tag: GrammarTag::None,
                    }),
                    col_refs_1,
                    col_refs_2,
                ],
                tag: GrammarTag::None,
            });

            SELECT_STMT
        }
    }
}

pub fn parse_stmt_g(l: &mut Lexer<'_>) -> Result<Node, Error> {
    let tkn = next_tkn!(l);

    Ok(match tkn {
        Token::Create => todo!(),
        Token::Select => parse_select_stmt_g(l)?,
        Token::Insert => todo!(),
        Token::Update => todo!(),
        Token::Delete => todo!(),
        _ => Err(Error::Invalid)?,
    })
}

fn parse_select_stmt_g(l: &mut Lexer<'_>) -> Result<Node, Error> {
    let mut all = false;
    let mut targets = Vec::new();
    let mut from_clause = vec![];
    let where_clause = Box::new(Node::Invalid);
    let group_clause = vec![];
    let sort_clause = vec![];
    let limit_count = Box::new(Node::Invalid);
    let limit_offset = Box::new(Node::Invalid);

    unsafe {
        let mut cur = GrammarNode::select_stmt();

        loop {
            let node = &(*cur);
            if node.adjacent.is_empty() {
                break;
            }

            let tkn = next_tkn!(l);
            let mut m = false;
            'adj: for n in &node.adjacent {
                if tkn.teq(&(**n).token) {
                    cur = *n;
                    m = true;

                    match tkn {
                        Token::Table
                        | Token::Select
                        | Token::From
                        | Token::Comma
                        | Token::Semicolon
                        | Token::Where => break 'adj,

                        Token::All => {
                            all = true;
                            break 'adj;
                        }

                        Token::TableOrColumnReference(r) => match (**n).tag {
                            GrammarTag::Targets => {
                                targets.push(Node::ColumnRef {
                                    table: None,
                                    column: r,
                                });
                                break 'adj;
                            }
                            GrammarTag::Table => {
                                from_clause.push(Node::TableRef { table: r });
                                break 'adj;
                            }
                            _ => unreachable!(),
                        },

                        Token::TableAndColumnReference(t, c) => {
                            targets.push(Node::ColumnRef {
                                table: Some(t),
                                column: c,
                            });
                            break 'adj;
                        }

                        _ => Err(Error::UnexpectedToken)?,
                    }
                }
            }

            if !m {
                Err(Error::UnexpectedToken)?;
            }
        }
    }

    Ok(Node::SelectStatement {
        all,
        targets,
        from_clause,
        where_clause,
        group_clause,
        sort_clause,
        limit_count,
        limit_offset,
    })
}

#[test]
fn test_parse_select_stmt_g() {
    struct TestCase {
        input: &'static str,
        want: Node,
    }

    let tcs = [TestCase {
        input: "select columna, tablea.columna from tablea;",
        want: Node::SelectStatement {
            all: false,
            targets: vec![
                Node::ColumnRef {
                    table: None,
                    column: "columna".into(),
                },
                Node::ColumnRef {
                    table: Some("tablea".into()),
                    column: "columna".into(),
                },
            ],
            from_clause: vec![Node::TableRef {
                table: "tablea".into(),
            }],
            where_clause: Box::new(Node::Invalid),
            group_clause: vec![],
            sort_clause: vec![],
            limit_count: Box::new(Node::Invalid),
            limit_offset: Box::new(Node::Invalid),
        },
    }];

    for TestCase { input, want } in tcs {
        let have = parse_stmt_g(&mut Lexer::new(input)).unwrap();
        assert!(want == have, "\nWant: {:?}\nHave: {:?}\n", want, have);
    }
}

// Keeping for reference
#[allow(unused)]
mod old {
    use super::*;

    #[derive(PartialEq, Debug, Copy, Clone)]
    enum State {
        ExpectLParen,
        ExpectRParen,

        ExpectTable,
        ExpectFrom,

        ExpectTableReference,
        ExpectColumnReference,

        ExpectTableOrColumnReference,
        ExpectColumnDef,

        ExpectSemicolon,
        ExpectComma,

        ExpectWhereClause,

        Done,
    }

    fn parse_select_stmt(l: &mut Lexer<'_>) -> Result<Node, Error> {
        let mut state = State::ExpectColumnReference;

        let mut all = false;
        let mut targets = Vec::new();
        let mut table = String::new();

        while state != State::Done {
            let tkn = next_tkn!(l);

            match state {
                State::ExpectColumnReference => match tkn {
                    Token::All => {
                        all = true;
                        state = State::ExpectFrom
                    }
                    Token::TableOrColumnReference(r) => {
                        targets.push(Node::ColumnRef {
                            table: None,
                            column: r,
                        });

                        state = State::ExpectComma;
                    }
                    Token::TableAndColumnReference(tr, cr) => {
                        targets.push(Node::ColumnRef {
                            table: Some(tr),
                            column: cr,
                        });

                        state = State::ExpectComma;
                    }
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectFrom => match tkn {
                    Token::From => state = State::ExpectTableReference,
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectTableReference => match tkn {
                    Token::TableOrColumnReference(r) => {
                        table = r;
                        state = State::ExpectWhereClause;
                    }
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectComma => match tkn {
                    Token::Comma => state = State::ExpectColumnReference,
                    Token::From => state = State::ExpectFrom,
                    _ => Err(Error::UnexpectedToken)?,
                },

                _ => todo!(),
            }
        }

        todo!()
    }

    fn parse_stmt(tkns: &mut Lexer<'_>) -> Result<Node, Error> {
        let tkn = tkns.next().ok_or(Error::Invalid)?;

        Ok(match tkn {
            Token::Create => parse_create_stmt(tkns)?,
            Token::Select => todo!(),
            Token::Insert => todo!(),
            Token::Update => todo!(),
            Token::Delete => todo!(),
            _ => Err(Error::Invalid)?,
        })
    }

    fn parse_create_stmt(tkns: &mut Lexer<'_>) -> Result<Node, Error> {
        let mut state = State::ExpectTable;

        let mut rel_name = String::new();
        let mut col_defs = Vec::new();

        while state != State::Done {
            let tkn = next_tkn!(tkns);

            match state {
                State::ExpectTable => match tkn {
                    Token::Table => state = State::ExpectTableOrColumnReference,
                    _ => Err(Error::Invalid)?,
                },

                State::ExpectTableOrColumnReference => match tkn {
                    Token::TableOrColumnReference(r) => {
                        rel_name = r;
                        state = State::ExpectLParen;
                    }
                    _ => Err(Error::Invalid)?,
                },

                State::ExpectLParen => match tkn {
                    Token::LParen => state = State::ExpectColumnDef,
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectRParen => match tkn {
                    Token::RParen => state = State::ExpectSemicolon,
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectColumnDef => match tkn {
                    Token::TableOrColumnReference(r) => {
                        // Get the type and build column def
                        let tkn = next_tkn!(tkns);

                        match tkn {
                            Token::Int => col_defs.push(Node::ColumnDef {
                                name: r,
                                t: ColumnType::Int,
                                var_size: 0,
                            }),
                            _ => Err(Error::UnexpectedToken)?,
                        }

                        state = State::ExpectComma;
                    }
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectComma => match tkn {
                    Token::Comma => state = State::ExpectColumnDef,
                    Token::RParen => state = State::ExpectSemicolon,
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectSemicolon => match tkn {
                    Token::Semicolon => state = State::Done,
                    _ => Err(Error::UnexpectedToken)?,
                },

                State::ExpectFrom
                | State::ExpectTableReference
                | State::ExpectColumnReference
                | State::ExpectWhereClause
                | State::Done => unreachable!(),
            }
        }

        Ok(Node::CreateStatement { rel_name, col_defs })
    }

    #[test]
    fn test_parse_create_stmt() {
        struct TestCase {
            input: &'static str,
            want: Node,
        }

        let tcs = [TestCase {
            input: "create table tablea (
                columna int,
                columnb int
            );",
            want: Node::CreateStatement {
                rel_name: "tablea".into(),
                col_defs: vec![
                    Node::ColumnDef {
                        name: "columna".into(),
                        t: ColumnType::Int,
                        var_size: 0,
                    },
                    Node::ColumnDef {
                        name: "columnb".into(),
                        t: ColumnType::Int,
                        var_size: 0,
                    },
                ],
            },
        }];

        for TestCase { input, want } in tcs {
            let have = parse_stmt(&mut Lexer::new(input)).unwrap();
            assert!(want == have, "\nWant: {:?}\nHave: {:?}\n", want, have);
        }
    }
}
