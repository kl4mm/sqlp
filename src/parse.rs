use std::vec;

use crate::{Lexer, Token};

#[derive(PartialEq, Debug)]
pub enum ColumnType {
    Int,
}

#[derive(PartialEq, Debug)]
pub enum Node {
    /* Statements */
    CreateStatement {
        rel_name: String,
        col_defs: Vec<Node>, // Of ColumnDef
    },

    SelectStatement {
        all: bool,
        targets: Vec<Node>, // Of ColumnRef

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

    /* Parse nodes */
    ColumnDef {
        t: ColumnType,
        name: String,
        var_size: Option<u8>,
    },

    ColumnRef {
        alias: Option<String>,
        family: Option<String>,
        name: String,
    },

    TableRef {
        name: String,
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
    Defs,
    Table,
    None,
}

#[derive(Debug)]
struct GrammarNode {
    token: Token,
    tag: GrammarTag,
    adjacent: Vec<*mut GrammarNode>,
}

impl GrammarNode {
    pub fn create_stmt() -> *const Self {
        static mut CREATE_STMT: *const GrammarNode = std::ptr::null_mut();

        unsafe {
            if !CREATE_STMT.is_null() {
                return CREATE_STMT;
            }

            let end = ptr!(Self {
                token: Token::RParen,
                tag: GrammarTag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::Semicolon,
                    tag: GrammarTag::None,
                    adjacent: vec![]
                })],
            });

            // Cycle
            // <c> INT [,|);]
            let col_def = ptr!(Self {
                token: Token::TableOrColumnReference(String::new()),
                tag: GrammarTag::Defs,
                adjacent: vec![ptr!(Self {
                    token: Token::Int,
                    tag: GrammarTag::None,
                    adjacent: vec![
                        ptr!(Self {
                            token: Token::Comma,
                            tag: GrammarTag::None,
                            adjacent: vec![]
                        }),
                        end,
                    ]
                })]
            });
            (*(*(*col_def).adjacent[0]).adjacent[0])
                .adjacent
                .push(col_def);

            // CREATE TABLE <t> ( (col_def),* );
            CREATE_STMT = ptr!(Self {
                token: Token::Create,
                tag: GrammarTag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::Table,
                    tag: GrammarTag::None,
                    adjacent: vec![ptr!(Self {
                        token: Token::TableOrColumnReference(String::new()),
                        tag: GrammarTag::Table,
                        adjacent: vec![ptr!(Self {
                            token: Token::LParen,
                            tag: GrammarTag::None,
                            adjacent: vec![col_def]
                        })]
                    })],
                })],
            });

            CREATE_STMT
        }
    }

    pub fn select_stmt() -> *const Self {
        static mut SELECT_STMT: *const GrammarNode = std::ptr::null_mut();

        unsafe {
            if !SELECT_STMT.is_null() {
                return SELECT_STMT;
            }

            let end = ptr!(Self {
                token: Token::Semicolon,
                tag: GrammarTag::None,
                adjacent: vec![],
            });

            // TODO: vecs labeled 'Next clause' will have next token of either AND conditions, OR conditions,
            // or next clause

            // TODO: adjacent should probably be Rc lol
            let rhs_operands = vec![
                ptr!(Self {
                    token: Token::TableAndColumnReference(String::new(), String::new()),
                    tag: GrammarTag::None,
                    adjacent: vec![] // Next clause
                }),
                ptr!(Self {
                    token: Token::TableOrColumnReference(String::new()),
                    tag: GrammarTag::None,
                    adjacent: vec![] // Next clause
                }),
                ptr!(Self {
                    token: Token::StringLiteral(String::new()),
                    tag: GrammarTag::None,
                    adjacent: vec![] // Next clause
                }),
                ptr!(Self {
                    token: Token::IntegerLiteral(0),
                    tag: GrammarTag::None,
                    adjacent: vec![] // Next clause
                }),
            ];

            let operators = vec![
                ptr!(Self {
                    token: Token::Eq,
                    tag: GrammarTag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Neq,
                    tag: GrammarTag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Gt,
                    tag: GrammarTag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Ge,
                    tag: GrammarTag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Lt,
                    tag: GrammarTag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Le,
                    tag: GrammarTag::None,
                    adjacent: rhs_operands
                }),
            ];

            let string_list = ptr!(Self {
                token: Token::StringLiteral(String::new()),
                tag: GrammarTag::None,
                adjacent: vec![
                    ptr!(Self {
                        token: Token::Comma,
                        tag: GrammarTag::None,
                        adjacent: vec![], // Cycle
                    }),
                    ptr!(Self {
                        token: Token::RParen,
                        tag: GrammarTag::None,
                        adjacent: vec![], // Next clause
                    })
                ],
            });
            (*(*string_list).adjacent[0]).adjacent.push(string_list);

            let int_list = ptr!(Self {
                token: Token::IntegerLiteral(0),
                tag: GrammarTag::None,
                adjacent: vec![
                    ptr!(Self {
                        token: Token::Comma,
                        tag: GrammarTag::None,
                        adjacent: vec![], // Cycle
                    }),
                    ptr!(Self {
                        token: Token::RParen,
                        tag: GrammarTag::None,
                        adjacent: vec![], // Next clause
                    })
                ],
            });
            (*(*int_list).adjacent[0]).adjacent.push(int_list);

            let in_expr = ptr!(Self {
                token: Token::In,
                tag: GrammarTag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::LParen,
                    tag: GrammarTag::None,
                    adjacent: vec![string_list, int_list]
                })]
            });

            let between_expr = ptr!(Self {
                token: Token::Between,
                tag: GrammarTag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::IntegerLiteral(0),
                    tag: GrammarTag::None,
                    adjacent: vec![ptr!(Self {
                        token: Token::Conjunction,
                        tag: GrammarTag::None,
                        adjacent: vec![ptr!(Self {
                            token: Token::IntegerLiteral(0),
                            tag: GrammarTag::None,
                            adjacent: vec![] // Next clause
                        })]
                    })]
                })]
            });

            let null = ptr!(Self {
                token: Token::Null,
                tag: GrammarTag::None,
                adjacent: vec![] // Next clause
            });
            let is_expr = ptr!(Self {
                token: Token::Is,
                tag: GrammarTag::None,
                adjacent: vec![
                    null,
                    ptr!(Self {
                        token: Token::Negation,
                        tag: GrammarTag::None,
                        adjacent: vec![null]
                    })
                ]
            });

            let not_expr = ptr!(Self {
                token: Token::Negation,
                tag: GrammarTag::None,
                adjacent: vec![in_expr, between_expr, is_expr]
            });

            // TODO: add support for summands and factors in future. keeping it simple for now
            // TODO: add grammar for term (value, function, col_ref), which should also be used in
            // place of col_refs. sticking to col_refs and literals for now
            // [<t>.<c>|<c>|StringLiteral|IntegerLiteral]
            let lhs_operands = vec![
                ptr!(Self {
                    token: Token::TableAndColumnReference(String::new(), String::new()),
                    tag: GrammarTag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
                ptr!(Self {
                    token: Token::TableOrColumnReference(String::new()),
                    tag: GrammarTag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
                ptr!(Self {
                    token: Token::StringLiteral(String::new()),
                    tag: GrammarTag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
                ptr!(Self {
                    token: Token::IntegerLiteral(0),
                    tag: GrammarTag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
            ];

            // lhs_operand [
            //   [=|!=|>|>=|<|<=] rhs_operand
            //   | [NOT|] IN ( (constOperand),* )
            //   | [NOT|] BETWEEN IntegerLiteral AND IntegerLiteral
            //   | IS [NOT|] NULL
            // ]
            // | NOT expr
            // | ( expr )
            let conditions = lhs_operands.into_iter().chain(vec![
                ptr!(Self {
                    token: Token::Negation,
                    tag: GrammarTag::None,
                    adjacent: vec![] // expr
                }),
                // ( expr )
            ]);

            // condition ([AND condition|OR condition|])*
            let andCondition = ptr!(Self {
                token: todo!(),
                tag: GrammarTag::None,
                adjacent: vec![],
            });

            // // andCondition ([OR andCondition|])*
            // let expr = ptr!(Self {
            //     token: todo!(),
            //     tag: GrammarTag::None,
            //     adjacent: vec![],
            // });

            // WHERE expr
            let where_clause = ptr!(Self {
                token: Token::Where,
                tag: GrammarTag::None,
                adjacent: conditions.collect(),
            });

            // FROM <t> [;|where_clause]
            let from_clause = ptr!(Self {
                token: Token::From,
                tag: GrammarTag::None,
                adjacent: vec![ptr!(GrammarNode {
                    token: Token::TableOrColumnReference(String::new()),
                    tag: GrammarTag::Table,
                    adjacent: vec![where_clause, end],
                })],
            });

            // <c> [,|from_clause]
            let col_refs_1 = ptr!(GrammarNode {
                token: Token::TableOrColumnReference(String::new()),
                tag: GrammarTag::Targets,
                adjacent: vec![from_clause],
            });

            // <t>.<c> [,|from_clause]
            let col_refs_2 = ptr!(GrammarNode {
                token: Token::TableAndColumnReference(String::new(), String::new()),
                tag: GrammarTag::None,
                adjacent: vec![from_clause],
            });

            // Cycle
            let comma = ptr!(GrammarNode {
                token: Token::Comma,
                tag: GrammarTag::None,
                adjacent: vec![col_refs_1, col_refs_2, from_clause],
            });
            (*col_refs_1).adjacent.push(comma);
            (*col_refs_2).adjacent.push(comma);

            // SELECT [*|(col_ref),*]
            SELECT_STMT = ptr!(Self {
                token: Token::Select,
                tag: GrammarTag::None,
                adjacent: vec![
                    ptr!(Self {
                        token: Token::All,
                        adjacent: vec![from_clause],
                        tag: GrammarTag::None,
                    }),
                    col_refs_1,
                    col_refs_2,
                ],
            });

            SELECT_STMT
        }
    }
}

pub fn parse_stmt(l: &mut Lexer<'_>) -> Result<Node, Error> {
    let tkn = next_tkn!(l);

    Ok(match tkn {
        Token::Create => parse_create_stmt(l)?,
        Token::Select => parse_select_stmt(l)?,
        Token::Insert => todo!(),
        Token::Update => todo!(),
        Token::Delete => todo!(),
        _ => Err(Error::Invalid)?,
    })
}

fn parse_create_stmt(l: &mut Lexer<'_>) -> Result<Node, Error> {
    let mut rel_name = String::new();
    let mut col_defs = Vec::new();

    unsafe {
        let mut cur = GrammarNode::create_stmt();

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
                        | Token::Create
                        | Token::Comma
                        | Token::LParen
                        | Token::RParen
                        | Token::Semicolon => break 'adj,

                        Token::TableOrColumnReference(r) => {
                            match (**n).tag {
                                GrammarTag::Defs => {
                                    // Look ahead for type
                                    // Advance grammar cursor
                                    let ntkn = next_tkn!(l);
                                    cur = *(**n)
                                        .adjacent
                                        .iter()
                                        .find(|&&x| (*x).token.teq(&Token::Int))
                                        .expect("infallible");

                                    match ntkn {
                                        Token::Int => {
                                            col_defs.push(Node::ColumnDef {
                                                t: ColumnType::Int,
                                                name: r,
                                                var_size: None,
                                            });
                                            break 'adj;
                                        }
                                        _ => Err(Error::UnexpectedToken)?,
                                    }

                                    // Why does the compiler think it can reach this?
                                    unreachable!()
                                }
                                GrammarTag::Table => {
                                    rel_name = r;
                                    break 'adj;
                                }
                                _ => unreachable!(),
                            }
                        }

                        // We look ahead to find the type so this shouldn't be expected here
                        Token::Int => Err(Error::UnexpectedToken)?,

                        _ => Err(Error::UnexpectedToken)?,
                    }
                }
            }

            if !m {
                Err(Error::UnexpectedToken)?;
            }
        }
    }

    Ok(Node::CreateStatement { rel_name, col_defs })
}

fn parse_select_stmt(l: &mut Lexer<'_>) -> Result<Node, Error> {
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
                                    alias: None,
                                    family: None,
                                    name: r,
                                });
                                break 'adj;
                            }
                            GrammarTag::Table => {
                                from_clause.push(Node::TableRef { name: r });
                                break 'adj;
                            }
                            _ => unreachable!(),
                        },

                        Token::TableAndColumnReference(t, c) => {
                            targets.push(Node::ColumnRef {
                                alias: None,
                                family: Some(t),
                                name: c,
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

#[cfg(test)]
mod test {
    use super::*;

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
                        var_size: None,
                    },
                    Node::ColumnDef {
                        name: "columnb".into(),
                        t: ColumnType::Int,
                        var_size: None,
                    },
                ],
            },
        }];

        for TestCase { input, want } in tcs {
            let have = parse_stmt(&mut Lexer::new(input)).unwrap();
            assert!(want == have, "\nWant: {:?}\nHave: {:?}\n", want, have);
        }
    }

    #[test]
    fn test_parse_select_stmt() {
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
                        alias: None,
                        family: None,
                        name: "columna".into(),
                    },
                    Node::ColumnRef {
                        alias: None,
                        family: Some("tablea".into()),
                        name: "columna".into(),
                    },
                ],
                from_clause: vec![Node::TableRef {
                    name: "tablea".into(),
                }],
                where_clause: Box::new(Node::Invalid),
                group_clause: vec![],
                sort_clause: vec![],
                limit_count: Box::new(Node::Invalid),
                limit_offset: Box::new(Node::Invalid),
            },
        }];

        for TestCase { input, want } in tcs {
            let have = parse_stmt(&mut Lexer::new(input)).unwrap();
            assert!(want == have, "\nWant: {:?}\nHave: {:?}\n", want, have);
        }
    }
}
