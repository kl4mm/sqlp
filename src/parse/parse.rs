use crate::{
    check_next,
    parse::{Node, Op, Result, Type, Unexpected},
    Lexer, Token,
};

pub fn select(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Select);

    let columns = fields(l)?;
    check_next!(l, Token::From);
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
    check_next!(l, [Token::Insert, :Token::Into]);

    let table = match l.next() {
        Token::TableOrColumnReference(table) => Box::new(Node::TableRef(table)),
        t => Err(Unexpected(t))?,
    };

    let columns = match l.peek() {
        Token::LParen => parens(list(column_ref))(l)?,
        _ => vec![],
    };

    check_next!(l, Token::Values);

    let inserts = list(parens(list(literal)))(l)?;

    Ok(Node::Insert {
        columns,
        table,
        inserts,
    })
}

pub fn delete(l: &mut Lexer) -> Result<Node> {
    check_next!(l, [Token::Delete, Token::From]);

    let table = match l.next() {
        Token::TableOrColumnReference(table) => table,
        t => Err(Unexpected(t))?,
    };

    let mut r#where = None;
    if let Token::Where = l.peek() {
        r#where = Some(Box::new(where_expr(l)?));
    }

    let mut limit = None;
    if let Token::Limit = l.peek() {
        limit = Some(Box::new(limit_expr(l)?));
    }

    Ok(Node::Delete {
        table,
        r#where,
        limit,
    })
}

pub fn update(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Update);

    let table = match l.next() {
        Token::TableOrColumnReference(table) => table,
        t => Err(Unexpected(t))?,
    };

    check_next!(l, Token::Set);

    let assignments = list(assignment)(l)?;

    let mut r#where = None;
    if let Token::Where = l.peek() {
        r#where = Some(Box::new(where_expr(l)?));
    }

    Ok(Node::Update {
        table,
        assignments,
        r#where,
    })
}

fn assignment(l: &mut Lexer) -> Result<Node> {
    let column = match l.next() {
        Token::TableOrColumnReference(column) => column,
        t => Err(Unexpected(t))?,
    };

    check_next!(l, Token::Eq);

    let value = Box::new(literal(l)?);

    Ok(Node::Assignment { column, value })
}

pub fn create(l: &mut Lexer) -> Result<Node> {
    check_next!(l, [Token::Create, Token::Table]);

    let table = match l.next() {
        Token::TableOrColumnReference(table) => table,
        t => Err(Unexpected(t))?,
    };

    let columns = parens(list(column_def))(l)?;

    Ok(Node::Create { table, columns })
}

fn fields(l: &mut Lexer) -> Result<Vec<Node>> {
    match l.peek() {
        Token::All => {
            l.next();
            return Ok(vec![Node::All]);
        }

        _ => list(column_ref)(l),
    }
}

fn list<T>(
    f: impl Fn(&mut Lexer) -> Result<T> + Copy,
) -> impl Fn(&mut Lexer) -> Result<Vec<T>> + Copy {
    move |l: &mut Lexer| -> Result<Vec<T>> {
        #[derive(PartialEq, Debug)]
        enum State {
            Comma,
            Item,
        }

        fn _list<T>(
            l: &mut Lexer,
            f: impl Fn(&mut Lexer) -> Result<T>,
            mut list: Vec<T>,
            state: State,
        ) -> Result<Vec<T>> {
            match l.peek() {
                Token::Comma if state == State::Comma => {
                    l.next();
                    _list(l, f, list, State::Item)
                }
                _ if state == State::Comma => return Ok(list),
                _ => {
                    list.push(f(l)?);
                    _list(l, f, list, State::Comma)
                }
            }
        }

        _list(l, f, Vec::new(), State::Item)
    }
}

fn integer(l: &mut Lexer) -> Result<Node> {
    match l.next() {
        Token::IntegerLiteral(i) => Ok(Node::IntegerLiteral(i)),
        t => Err(Unexpected(t)),
    }
}

fn string(l: &mut Lexer) -> Result<Node> {
    match l.next() {
        Token::StringLiteral(s) => Ok(Node::StringLiteral(s)),
        t => Err(Unexpected(t)),
    }
}

fn literal(l: &mut Lexer) -> Result<Node> {
    match l.peek() {
        Token::StringLiteral(_) => string(l),
        Token::IntegerLiteral(_) => integer(l),
        t => Err(Unexpected(t)),
    }
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
        Token::LParen => parens(select)(l),
        t => Err(Unexpected(t)),
    }
}

fn join_expr(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Join);

    let table = Box::new(table_expr(l)?);

    match l.next() {
        Token::Using => Ok(Node::JoinUsing {
            table,
            columns: parens(list(column_ref))(l)?,
        }),
        Token::On => Ok(Node::JoinOn {
            table,
            expr: Box::new(parens(expr)(l)?),
        }),
        t => Err(Unexpected(t)),
    }
}

fn where_expr(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Where);

    expr(l)
}

fn group_expr(l: &mut Lexer) -> Result<Vec<Node>> {
    check_next!(l, [Token::Group, Token::By]);

    list(column_ref)(l)
}

fn order_expr(l: &mut Lexer) -> Result<Vec<Node>> {
    check_next!(l, [Token::Order, Token::By]);

    list(column_ref)(l)
}

fn limit_expr(l: &mut Lexer) -> Result<Node> {
    check_next!(l, Token::Limit);

    match l.next() {
        Token::IntegerLiteral(i) => Ok(Node::IntegerLiteral(i)),
        t => Err(Unexpected(t)),
    }
}

fn parens<T>(
    f: impl Fn(&mut Lexer) -> Result<T> + Copy,
) -> impl Fn(&mut Lexer) -> Result<T> + Copy {
    move |l: &mut Lexer| -> Result<T> {
        check_next!(l, Token::LParen);
        let n = f(l)?;
        check_next!(l, Token::RParen);

        Ok(n)
    }
}

fn column_def(l: &mut Lexer) -> Result<Node> {
    let column = match l.next() {
        Token::TableOrColumnReference(column) => column,
        t => Err(Unexpected(t))?,
    };

    let mut ty = l.next().try_into()?;
    if let Type::Varchar(n) = &mut ty {
        *n.as_mut() = parens(integer)(l)?
    }

    Ok(Node::ColumnDef { column, ty })
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
                    Op::In => Node::Expr(op, parens(list(literal))(l)?),
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

    use crate::parse::Type;

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
            },
            Test {
                input: "insert tablea (columna, columnb) values (1, 2)",
                want: Ok(Node::Insert{
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
                    ],
                    table: Box::new(Node::TableRef("tablea".into())),
                    inserts: vec![
                        vec![
                            Node::IntegerLiteral(1),
                            Node::IntegerLiteral(2),
                        ],
                    ],
                }),
            }
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = insert(&mut l);

            assert_eq!(want, have);
        }

        Ok(())
    }

    #[test]
    fn test_parse_create() {
        let tcs = [
            Test {
                input: "create table tablea (
                    columna int
                )",
                want: Ok(Node::Create {
                    table: "tablea".into(),
                    columns: vec![Node::ColumnDef {
                        column: "columna".into(),
                        ty: Type::Int,
                    }],
                }),
            },
            Test {
                input: "create table tablea (
                    columna int,
                    columnb int
                )",
                want: Ok(Node::Create {
                    table: "tablea".into(),
                    columns: vec![
                        Node::ColumnDef {
                            column: "columna".into(),
                            ty: Type::Int,
                        },
                        Node::ColumnDef {
                            column: "columnb".into(),
                            ty: Type::Int,
                        },
                    ],
                }),
            },
            Test {
                input: "create table tablea (
                    columna int,
                    columnb varchar(255),
                    columnc int
                )",
                want: Ok(Node::Create {
                    table: "tablea".into(),
                    columns: vec![
                        Node::ColumnDef {
                            column: "columna".into(),
                            ty: Type::Int,
                        },
                        Node::ColumnDef {
                            column: "columnb".into(),
                            ty: Type::Varchar(Box::new(Node::IntegerLiteral(255))),
                        },
                        Node::ColumnDef {
                            column: "columnc".into(),
                            ty: Type::Int,
                        },
                    ],
                }),
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = create(&mut l);

            assert_eq!(want, have)
        }
    }

    #[test]
    fn test_parse_delete() {
        let tcs = [
            Test {
                input: "delete from tablea",
                want: Ok(Node::Delete {
                    table: "tablea".into(),
                    r#where: None,
                    limit: None,
                }),
            },
            Test {
                input: "delete from tablea where 1 = 1",
                want: Ok(Node::Delete {
                    table: "tablea".into(),
                    r#where: Some(Box::new(Node::Expr(
                        Op::Eq,
                        vec![Node::IntegerLiteral(1), Node::IntegerLiteral(1)],
                    ))),
                    limit: None,
                }),
            },
            Test {
                input: "delete from tablea where 1 = 1 limit 1000",
                want: Ok(Node::Delete {
                    table: "tablea".into(),
                    r#where: Some(Box::new(Node::Expr(
                        Op::Eq,
                        vec![Node::IntegerLiteral(1), Node::IntegerLiteral(1)],
                    ))),
                    limit: Some(Box::new(Node::IntegerLiteral(1000))),
                }),
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = delete(&mut l);

            assert_eq!(want, have)
        }
    }

    #[test]
    fn test_parse_update() {
        let tcs = [
            Test {
                input: "update tablea set columna = 10",
                want: Ok(Node::Update {
                    table: "tablea".into(),
                    assignments: vec![Node::Assignment {
                        column: "columna".into(),
                        value: Box::new(Node::IntegerLiteral(10)),
                    }],
                    r#where: None,
                }),
            },
            Test {
                input: "update tablea set columna = 10, columnb = \"test\" where 1 = 1",
                want: Ok(Node::Update {
                    table: "tablea".into(),
                    assignments: vec![
                        Node::Assignment {
                            column: "columna".into(),
                            value: Box::new(Node::IntegerLiteral(10)),
                        },
                        Node::Assignment {
                            column: "columnb".into(),
                            value: Box::new(Node::StringLiteral("test".into())),
                        },
                    ],
                    r#where: Some(Box::new(Node::Expr(
                        Op::Eq,
                        vec![Node::IntegerLiteral(1), Node::IntegerLiteral(1)],
                    ))),
                }),
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = update(&mut l);

            assert_eq!(want, have)
        }
    }
}
