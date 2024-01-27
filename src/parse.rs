use crate::{
    grammar::{self, Node as Grammar, Tag},
    next_tkn, Lexer, Token,
};

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
        let mut cur = Grammar::create_stmt();

        loop {
            let node = grammar::NODES[cur];
            if node.adjacent.is_empty() {
                break;
            }

            let tkn = next_tkn!(l);
            let mut m = false;
            'adj: for n in node.adjacent.iter() {
                if tkn == grammar::NODES[n].token {
                    cur = n;
                    m = true;

                    let node = grammar::NODES[n];

                    match tkn {
                        Token::Table
                        | Token::Create
                        | Token::Comma
                        | Token::LParen
                        | Token::RParen
                        | Token::Semicolon => break 'adj,

                        Token::TableOrColumnReference(r) => {
                            match node.tag {
                                Tag::Defs => {
                                    // Look ahead for type
                                    // Advance grammar cursor
                                    let ntkn = next_tkn!(l);
                                    cur = node
                                        .adjacent
                                        .iter()
                                        .find(|&j| Token::Int == grammar::NODES[j].token)
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
                                Tag::Table => {
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
        let mut cur = Grammar::select_stmt();

        loop {
            let node = grammar::NODES[cur];
            if node.adjacent.is_empty() {
                break;
            }

            let tkn = next_tkn!(l);
            let mut m = false;
            'adj: for n in node.adjacent {
                if tkn == grammar::NODES[n].token {
                    cur = n;
                    m = true;

                    let node = grammar::NODES[n];

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

                        Token::TableOrColumnReference(r) => match node.tag {
                            Tag::Targets => {
                                targets.push(Node::ColumnRef {
                                    alias: None,
                                    family: None,
                                    name: r,
                                });
                                break 'adj;
                            }
                            Tag::Table => {
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
