use crate::{Lexer, Token};

use crate::{
    check_next,
    parse::{Node, Op, Result, Unexpected},
};

pub fn select(l: &mut Lexer) -> Result<Node> {
    assert_eq!(l.next(), Token::Select);

    let columns = fields(l)?;
    assert_eq!(l.next(), Token::From);
    let table = Box::new(table_expr(l)?);

    let mut r#where = None;
    let mut group = vec![];
    let mut order = vec![];
    let mut joins = vec![];
    let mut limit = None;

    loop {
        match l.peek() {
            Token::Join => {
                if r#where.is_some() || !group.is_empty() || !order.is_empty() || limit.is_some() {
                    Err(Unexpected(l.next()))?
                }

                joins.push(join_expr(l)?);
            }
            Token::Where => {
                if r#where.is_some() || !group.is_empty() || !order.is_empty() || limit.is_some() {
                    Err(Unexpected(l.next()))?
                }

                r#where = Some(Box::new(where_expr(l)?))
            }
            Token::Group => {
                if !group.is_empty() || !order.is_empty() || limit.is_some() {
                    Err(Unexpected(l.next()))?
                }

                group = group_expr(l)?;
            }
            Token::Order => {
                if !order.is_empty() || limit.is_some() {
                    Err(Unexpected(l.next()))?
                }

                order = order_expr(l)?;
            }
            Token::Limit => {
                if limit.is_some() {
                    Err(Unexpected(l.next()))?
                }

                limit = Some(Box::new(limit_expr(l)?));
            }
            _ => break, // `parens` will check RParen, `select` will check Semicolon
        };
    }

    Ok(Node::Select {
        columns,
        table,
        r#where,
        group,
        order,
        joins,
        limit,
    })
}

pub fn insert(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Insert);
    check_next!(l, Token::Into);

    let table = match l.next() {
        Token::TableOrColumnReference(table) => Box::new(Node::TableRef(table)),
        t => Err(Unexpected(t))?,
    };

    let columns = match l.peek() {
        Token::LParen => parens(l, column_list)?,
        _ => vec![],
    };

    check_next!(l, Token::Values);

    let inserts = list_list(l, parens2(list))?;

    Ok(Node::Insert {
        columns,
        table,
        inserts,
    })
}

fn fields(l: &mut Lexer) -> Result<Vec<Node>> {
    match l.peek() {
        Token::All => {
            l.next();
            return Ok(vec![Node::All]);
        }

        _ => column_list(l),
    }
}

fn column_list(l: &mut Lexer) -> Result<Vec<Node>> {
    #[derive(PartialEq, Debug)]
    enum State {
        Comma,
        Column,
    }

    fn column_list(l: &mut Lexer, mut list: Vec<Node>, state: State) -> Result<Vec<Node>> {
        match l.peek() {
            Token::Comma if state == State::Comma => {
                l.next();
                column_list(l, list, State::Column)
            }
            Token::TableOrColumnReference(_) | Token::TableAndColumnReference(_, _)
                if state == State::Column =>
            {
                list.push(column_ref(l)?);
                column_list(l, list, State::Comma)
            }

            _ if state == State::Comma => return Ok(list),
            t => Err(Unexpected(t)),
        }
    }

    column_list(l, Vec::new(), State::Column)
}

fn list(l: &mut Lexer) -> Result<Vec<Node>> {
    #[derive(PartialEq, Debug)]
    enum State {
        Comma,
        Item,
    }

    fn _list(l: &mut Lexer, mut list: Vec<Node>, state: State) -> Result<Vec<Node>> {
        match l.peek() {
            Token::Comma if state == State::Comma => {
                l.next();
                _list(l, list, State::Item)
            }
            Token::StringLiteral(s) if state == State::Item => {
                l.next();
                list.push(Node::StringLiteral(s));
                _list(l, list, State::Comma)
            }
            Token::IntegerLiteral(i) if state == State::Item => {
                l.next();
                list.push(Node::IntegerLiteral(i));
                _list(l, list, State::Comma)
            }

            _ if state == State::Comma => return Ok(list),
            t => Err(Unexpected(t)),
        }
    }

    _list(l, Vec::new(), State::Item)
}

#[allow(unused)]
fn literal(l: &mut Lexer) -> Result<Node> {
    match l.peek() {
        Token::StringLiteral(s) => {
            l.next();
            Ok(Node::StringLiteral(s))
        }
        Token::IntegerLiteral(i) => {
            l.next();
            Ok(Node::IntegerLiteral(i))
        }
        t => Err(Unexpected(t)),
    }
}

fn list_list<T, F: Fn(&mut Lexer) -> Result<T>>(l: &mut Lexer, f: F) -> Result<Vec<T>> {
    #[derive(PartialEq, Debug)]
    enum State {
        Comma,
        Item,
    }

    fn list_list<T>(
        l: &mut Lexer,
        f: impl Fn(&mut Lexer) -> Result<T>,
        mut list: Vec<T>,
        state: State,
    ) -> Result<Vec<T>> {
        match l.peek() {
            Token::Comma if state == State::Comma => {
                l.next();
                list_list(l, f, list, State::Item)
            }
            _ if state == State::Comma => return Ok(list),
            _ => {
                list.push(f(l)?);
                list_list(l, f, list, State::Comma)
            }
        }
    }

    list_list(l, f, Vec::new(), State::Item)
}

fn column_ref(l: &mut Lexer) -> Result<Node> {
    match l.next() {
        Token::TableOrColumnReference(column) => Ok(Node::ColumnRef {
            table: None,
            column,
            alias: None,
        }),

        Token::TableAndColumnReference(table, column) => Ok(Node::ColumnRef {
            table: Some(table),
            column,
            alias: None,
        }),

        t => Err(Unexpected(t)),
    }
}

fn table_expr(l: &mut Lexer) -> Result<Node> {
    match l.peek() {
        Token::TableOrColumnReference(table) => {
            l.next();
            Ok(Node::TableRef(table))
        }
        Token::LParen => parens(l, select),
        t => Err(Unexpected(t)),
    }
}

fn join_expr(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Join);

    let table = Box::new(table_expr(l)?);

    match l.next() {
        Token::Using => Ok(Node::JoinUsing {
            table,
            columns: parens(l, column_list)?,
        }),
        Token::On => Ok(Node::JoinOn {
            table,
            expr: Box::new(parens(l, expr)?),
        }),
        t => Err(Unexpected(t)),
    }
}

fn where_expr(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Where);

    expr(l)
}

fn group_expr(l: &mut Lexer) -> Result<Vec<Node>> {
    check_next!(l, Token::Group);
    check_next!(l, Token::By);

    column_list(l)
}

fn order_expr(l: &mut Lexer) -> Result<Vec<Node>> {
    check_next!(l, Token::Order);
    check_next!(l, Token::By);

    column_list(l)
}

fn limit_expr(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Limit);

    match l.next() {
        Token::IntegerLiteral(i) => Ok(Node::IntegerLiteral(i)),
        t => Err(Unexpected(t)),
    }
}

fn parens<T, F: Fn(&mut Lexer) -> Result<T>>(l: &mut Lexer, f: F) -> Result<T> {
    check_next!(l, Token::LParen);
    let n = f(l)?;
    check_next!(l, Token::RParen);

    Ok(n)
}

fn parens2<T>(f: impl Fn(&mut Lexer) -> Result<T>) -> impl Fn(&mut Lexer) -> Result<T> {
    move |l: &mut Lexer| -> Result<T> {
        check_next!(l, Token::LParen);
        let n = f(l)?;
        check_next!(l, Token::RParen);

        Ok(n)
    }
}

fn expr(l: &mut Lexer) -> Result<Node> {
    fn expr_bp(l: &mut Lexer, min_bp: u8) -> Result<Node> {
        let mut lhs: Node = match l.next() {
            Token::TableAndColumnReference(table, column) => Node::ColumnRef {
                table: Some(table),
                column,
                alias: None,
            },
            Token::TableOrColumnReference(column) => Node::ColumnRef {
                table: None,
                column,
                alias: None,
            },
            Token::StringLiteral(s) => Node::StringLiteral(s),
            Token::IntegerLiteral(i) => Node::IntegerLiteral(i),
            Token::Null => Node::Null,
            t if is_prefix(&t) => {
                let op = t.into();
                match op {
                    Op::Between => between(l)?,
                    Op::In => Node::Expr(op, parens(l, list)?),
                    _ => {
                        let ((), r_bp) = prefix_bp(&op);
                        let rhs = expr_bp(l, r_bp)?;
                        Node::Expr(op, vec![rhs]);
                        unreachable!()
                    }
                }
            }
            t => Err(Unexpected(t))?,
        };

        loop {
            let op = match l.peek() {
                t if is_infix(&t) => t.into(),
                _ => break,
            };

            let (l_bp, r_bp) = infix_bp(&op);
            if l_bp < min_bp {
                break;
            }

            l.next();
            let rhs = expr_bp(l, r_bp)?;

            lhs = Node::Expr(op, vec![lhs, rhs]);
        }

        Ok(lhs)
    }

    expr_bp(l, 0)
}

fn between(l: &mut Lexer) -> Result<Node> {
    let from = match l.next() {
        Token::IntegerLiteral(i) => Node::IntegerLiteral(i),
        t => Err(Unexpected(t))?,
    };

    check_next!(l, Token::Conjunction);

    let to = match l.next() {
        Token::IntegerLiteral(i) => Node::IntegerLiteral(i),
        t => Err(Unexpected(t))?,
    };

    Ok(Node::Expr(Op::Between, vec![from, to]))
}

fn is_infix(t: &Token) -> bool {
    match t {
        Token::Negation
        | Token::Is
        | Token::Conjunction
        | Token::Disjunction
        | Token::Eq
        | Token::Neq
        | Token::Lt
        | Token::Le
        | Token::Gt
        | Token::Ge => true,
        _ => false,
    }
}

fn is_prefix(t: &Token) -> bool {
    match t {
        Token::Between | Token::In => true,
        _ => false,
    }
}

fn infix_bp(op: &Op) -> (u8, u8) {
    match op {
        Op::Conjunction | Op::Disjunction => (1, 2),
        Op::Eq | Op::Neq | Op::Lt | Op::Le | Op::Gt | Op::Ge => (3, 4),
        Op::Negation | Op::Is => (3, 4),
        _ => unreachable!(),
    }
}

fn prefix_bp(op: &Op) -> ((), u8) {
    match op {
        Op::In | Op::Between => unreachable!("IN and BETWEEN are parsed without binding power"),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_expr() -> Result<()> {
        struct Test {
            input: &'static str,
            want: &'static str,
        }

        let tcs = [
            Test {
                input: "12 = 12",
                want: "(= 12 12)",
            },
            Test {
                input: "12 < 14 AND 14 > 12",
                want: "(AND (< 12 14) (> 14 12))",
            },
            Test {
                input: "12 < 14 AND 14 > 12 OR name != \"bob\"",
                want: "(OR (AND (< 12 14) (> 14 12)) (!= name \"bob\"))",
            },
            Test {
                input: "NULL",
                want: "NULL",
            },
            Test {
                input: "columna IS NULL",
                want: "(IS columna NULL)",
            },
            Test {
                input: "columna NOT NULL",
                want: "(NOT columna NULL)",
            },
            Test {
                input: "columna NOT BETWEEN 100 AND 200",
                want: "(NOT columna (BETWEEN 100 200))",
            },
            Test {
                input: "columna NOT BETWEEN 100 AND 200 AND 1 < 2",
                want: "(AND (NOT columna (BETWEEN 100 200)) (< 1 2))",
            },
            Test {
                input: "columna NOT IN (1, 2, 3, 4)",
                want: "(NOT columna (IN 1 2 3 4))",
            },
            Test {
                input: "columna NOT IN (1, 2, 3, 4) AND columna = columnb OR IN (6, 7, 8)",
                want: "(OR (AND (NOT columna (IN 1 2 3 4)) (= columna columnb)) (IN 6 7 8))",
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = expr(&mut l)?.to_string();
            assert_eq!(want, have);
        }

        Ok(())
    }

    struct Test {
        input: &'static str,
        want: Result<Node>,
    }

    #[test]
    fn test_parse_select() -> Result<()> {
        let tcs = [
            Test {
                input: "select * from tablea;",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![],
                    limit: None,
                }),
            },
            Test {
                input: "select columna, columnb from tablea;",
                want: Ok(Node::Select {
                    columns: vec![
                        Node::ColumnRef {
                            table: None,
                            column: "columna".into(),
                            alias: None,
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnb".into(),
                            alias: None,
                        },
                    ],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![],
                    limit: None,
                }),
            },
            Test {
                input: "select columna, columnb, columnc from tablea;",
                want: Ok(Node::Select {
                    columns: vec![
                        Node::ColumnRef {
                            table: None,
                            column: "columna".into(),
                            alias: None,
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnb".into(),
                            alias: None,
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnc".into(),
                            alias: None,
                        },
                    ],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![],
                    limit: None,
                }),
            },
            Test {
                input: "select * from tablea join tableb using (columna, columnb);",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![Node::JoinUsing {
                        table: Box::new(Node::TableRef("tableb".into())),
                        columns: vec![
                            Node::ColumnRef {
                                table: None,
                                column: "columna".into(),
                                alias: None,
                            },
                            Node::ColumnRef {
                                table: None,
                                column: "columnb".into(),
                                alias: None,
                            },
                        ],
                    }],
                    limit: None,
                }),
            },
            Test {
                input: "select * from tablea join (select * from tableb) using (columna, columnb);",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![Node::JoinUsing {
                        table: Box::new(Node::Select {
                            columns: vec![Node::All],
                            table: Box::new(Node::TableRef("tableb".into())),
                            r#where: None,
                            group: vec![],
                            order: vec![],
                            joins: vec![],
                            limit: None,
                        }),
                        columns: vec![
                            Node::ColumnRef {
                                table: None,
                                column: "columna".into(),
                                alias: None,
                            },
                            Node::ColumnRef {
                                table: None,
                                column: "columnb".into(),
                                alias: None,
                            },
                        ],
                    }],
                    limit: None,
                }),
            },
            Test {
                input: "select * from tablea join tableb on (tablea.columna = tableb.columna);",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![Node::JoinOn {
                        table: Box::new(Node::TableRef("tableb".into())),
                        expr: Box::new(Node::Expr(
                            Op::Eq,
                            vec![
                                Node::ColumnRef {
                                    table: Some("tablea".into()),
                                    column: "columna".into(),
                                    alias: None,
                                },
                                Node::ColumnRef {
                                    table: Some("tableb".into()),
                                    column: "columna".into(),
                                    alias: None,
                                },
                            ],
                        )),
                    }],
                    limit: None,
                }),
            },
            Test {
                input: "select * from tablea
                    join tableb on (tablea.columna = tableb.columna)
                    join tablec on (tablea.columna = tablec.columna);",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![
                        Node::JoinOn {
                            table: Box::new(Node::TableRef("tableb".into())),
                            expr: Box::new(Node::Expr(
                                Op::Eq,
                                vec![
                                    Node::ColumnRef {
                                        table: Some("tablea".into()),
                                        column: "columna".into(),
                                        alias: None,
                                    },
                                    Node::ColumnRef {
                                        table: Some("tableb".into()),
                                        column: "columna".into(),
                                        alias: None,
                                    },
                                ],
                            )),
                        },
                        Node::JoinOn {
                            table: Box::new(Node::TableRef("tablec".into())),
                            expr: Box::new(Node::Expr(
                                Op::Eq,
                                vec![
                                    Node::ColumnRef {
                                        table: Some("tablea".into()),
                                        column: "columna".into(),
                                        alias: None,
                                    },
                                    Node::ColumnRef {
                                        table: Some("tablec".into()),
                                        column: "columna".into(),
                                        alias: None,
                                    },
                                ],
                            )),
                        },
                    ],
                    limit: None,
                }),
            },
            Test {
                input: "select * from tablea where columna not null;",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: Some(Box::new(Node::Expr(
                        Op::Negation,
                        vec![
                            Node::ColumnRef {
                                table: None,
                                column: "columna".into(),
                                alias: None,
                            },
                            Node::Null,
                        ],
                    ))),
                    group: vec![],
                    order: vec![],
                    joins: vec![],
                    limit: None,
                }),
            },
            Test {
                input: "select * from tablea where columna not null group by columna, columnb order by columnb;",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: Some(Box::new(Node::Expr(
                        Op::Negation,
                        vec![
                            Node::ColumnRef {
                                table: None,
                                column: "columna".into(),
                                alias: None,
                            },
                            Node::Null,
                        ],
                    ))),
                    group: vec![
                        Node::ColumnRef {
                            table: None,
                            column: "columna".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnb".into(),
                            alias: None
                        }
                    ],
                    order: vec![Node::ColumnRef { table: None, column: "columnb".into(), alias: None }],
                    joins: vec![],
                    limit: None,
                }),
            },
            Test {
                input: "select * from tablea
                    join tableb using (columna)
                    where columna > 1000 and columnb not in (1, 2, 3, 4)
                    group by columna, columnb
                    order by columnb
                    limit 100;",
                want: Ok(Node::Select {
                    columns: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: Some(Box::new(Node::Expr(
                        Op::Conjunction,
                        vec![
                            Node::Expr(
                                Op::Gt,
                                vec![
                                    Node::ColumnRef {
                                        table: None,
                                        column: "columna".into(),
                                        alias: None
                                    },
                                    Node::IntegerLiteral(1000)
                                ]),
                            Node::Expr(
                                Op::Negation,
                                vec![
                                    Node::ColumnRef {
                                        table: None,
                                        column: "columnb".into(),
                                        alias: None
                                    },
                                    Node::Expr(
                                        Op::In,
                                        vec![
                                            Node::IntegerLiteral(1),
                                            Node::IntegerLiteral(2),
                                            Node::IntegerLiteral(3),
                                            Node::IntegerLiteral(4)
                                        ]
                                    )
                                ]
                            )
                        ]
                    ))),
                    group: vec![
                        Node::ColumnRef {
                            table: None,
                            column: "columna".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnb".into(),
                            alias: None
                        }
                    ],
                    order: vec![Node::ColumnRef { table: None, column: "columnb".into(), alias: None }],
                    joins: vec![
                        Node::JoinUsing {
                            table: Box::new(Node::TableRef("tableb".into())),
                            columns: vec![
                                Node::ColumnRef {
                                    table: None,
                                    column: "columna".into(),
                                    alias: None
                                }
                            ]
                        }
                    ],
                    limit: Some(Box::new(Node::IntegerLiteral(100))),
                }),
            },
            Test {
                input: "select * from tablea where columna > 1000 join tableb using (columna);",
                want: Err(Unexpected(Token::Join)),
            },
            Test {
                input: "select * from tablea join tableb using (columna) group by columna join tableb using (columna);",
                want: Err(Unexpected(Token::Join)),
            },
            Test {
                input: "select * from tablea join tableb using (columna) group by columna order by columna join tableb using (columna);",
                want: Err(Unexpected(Token::Join)),
            },
            Test {
                input: "select * from tablea join tableb using (columna) order by columna group by columna;",
                want: Err(Unexpected(Token::Group)),
            },
            Test {
                input: "select * from tablea join tableb using (columna) limit 100 group by columna;",
                want: Err(Unexpected(Token::Group)),
            },
            Test {
                input: "select columna, columnb, from tablea;",
                want: Err(Unexpected(Token::From)),
            },
            Test {
                input: "select columna, * from tablea;",
                want: Err(Unexpected(Token::All)),
            },
            Test {
                input: "select , columna, columnb from tablea;",
                want: Err(Unexpected(Token::Comma)),
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = select(&mut l);

            assert_eq!(want, have);
        }

        Ok(())
    }

    #[test]
    fn test_parse_insert() -> Result<()> {
        let tcs = [
            Test {
                input: "insert into tablea values (\"test\", 1, \"insert\", 2);",
                want: Ok(Node::Insert {
                    columns: vec![],
                    table: Box::new(Node::TableRef("tablea".into())),
                    inserts: vec![vec![
                        Node::StringLiteral("test".into()),
                        Node::IntegerLiteral(1),
                        Node::StringLiteral("insert".into()),
                        Node::IntegerLiteral(2),
                    ]],
                })
            },
            Test {
                input: "insert into tablea (columna, columnb, columnc, columnd) values (\"test\", 1, \"insert\", 2);",
                want: Ok(Node::Insert {
                    columns: vec![
                        Node::ColumnRef {
                            table: None,
                            column: "columna".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnb".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnc".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnd".into(),
                            alias: None
                        },
                    ],
                    table: Box::new(Node::TableRef("tablea".into())),
                    inserts: vec![vec![
                        Node::StringLiteral("test".into()),
                        Node::IntegerLiteral(1),
                        Node::StringLiteral("insert".into()),
                        Node::IntegerLiteral(2),
                    ]],
                })
            },
            Test {
                input: "insert into tablea (columna, columnb, columnc, columnd)
                    values (\"test\", 1, \"insert\", 2),
                    (\"insert\", 2, \"test\", 1);",
                want: Ok(Node::Insert {
                    columns: vec![
                        Node::ColumnRef {
                            table: None,
                            column: "columna".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnb".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnc".into(),
                            alias: None
                        },
                        Node::ColumnRef {
                            table: None,
                            column: "columnd".into(),
                            alias: None
                        },
                    ],
                    table: Box::new(Node::TableRef("tablea".into())),
                    inserts: vec![
                        vec![
                            Node::StringLiteral("test".into()),
                            Node::IntegerLiteral(1),
                            Node::StringLiteral("insert".into()),
                            Node::IntegerLiteral(2),
                        ],
                        vec![
                            Node::StringLiteral("insert".into()),
                            Node::IntegerLiteral(2),
                            Node::StringLiteral("test".into()),
                            Node::IntegerLiteral(1),
                        ]
                    ],
                })
            }
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = insert(&mut l);

            assert_eq!(want, have);
        }

        Ok(())
    }
}
