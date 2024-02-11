use crate::{Lexer, Token};

#[derive(Debug, PartialEq)]
pub enum Op {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    Conjunction,
    Disjunction,

    Negation,
    In,
    Between,
    Is,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Eq => write!(f, "="),
            Op::Neq => write!(f, "!="),
            Op::Lt => write!(f, "<"),
            Op::Le => write!(f, "<="),
            Op::Gt => write!(f, ">"),
            Op::Ge => write!(f, ">="),
            Op::Conjunction => write!(f, "AND"),
            Op::Disjunction => write!(f, "OR"),
            Op::Negation => write!(f, "NOT"),
            Op::In => write!(f, "IN"),
            Op::Between => write!(f, "BETWEEN"),
            Op::Is => write!(f, "IS"),
        }
    }
}

impl From<Token> for Op {
    fn from(t: Token) -> Self {
        match t {
            Token::Conjunction => Op::Conjunction,
            Token::Disjunction => Op::Disjunction,
            Token::Eq => Op::Eq,
            Token::Neq => Op::Neq,
            Token::Lt => Op::Lt,
            Token::Le => Op::Le,
            Token::Gt => Op::Gt,
            Token::Ge => Op::Ge,
            Token::Negation => Op::Negation,
            Token::In => Op::In,
            Token::Between => Op::Between,
            Token::Is => Op::Is,

            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Select {
        fields: Vec<Node>,
        table: Box<Node>,           // TableRef or Select
        r#where: Option<Box<Node>>, // Expr
        group: Vec<Node>,
        order: Vec<Node>,
        joins: Vec<Node>,
        limit: Option<Box<Node>>,
    },

    Expr(Op, Vec<Node>),

    ColumnRef {
        table: Option<String>,
        column: String,
        alias: Option<String>,
    },

    TableRef(String),

    JoinUsing {
        table: Box<Node>,
        columns: Vec<Node>, // Of ColumnRef
    },

    JoinOn {
        table: Box<Node>,
        expr: Box<Node>, // Expr
    },

    StringLiteral(String),
    IntegerLiteral(u64),

    All,
    Null,

    None,
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Select {
                fields,
                table,
                r#where,
                group,
                order,
                joins,
                limit,
            } => todo!(),
            Node::Expr(operator, operands) => {
                write!(f, "({}", operator)?;
                for o in operands {
                    write!(f, " {}", o)?;
                }
                write!(f, ")")
            }
            Node::ColumnRef {
                table,
                column,
                alias,
            } => {
                if table.is_some() {
                    write!(f, "{}", table.as_ref().unwrap())?;
                }

                write!(f, "{column}")?;

                if alias.is_some() {
                    write!(f, "AS {}", alias.as_ref().unwrap())?;
                }

                Ok(())
            }
            Node::TableRef(t) => write!(f, "{t}"),
            Node::JoinUsing { table, columns } => todo!(),
            Node::JoinOn { table, expr } => todo!(),
            Node::StringLiteral(s) => write!(f, "\"{s}\""),
            Node::IntegerLiteral(i) => write!(f, "{i}"),
            Node::All => write!(f, "*"),
            Node::Null => write!(f, "NULL"),
            Node::None => todo!(),
        }
    }
}

pub struct Unexpected(Token);

impl std::fmt::Display for Unexpected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unexpected token: {:?}", self.0)
    }
}

impl std::fmt::Debug for Unexpected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unexpected token: {:?}", self.0)
    }
}

impl std::error::Error for Unexpected {}

type Result<T> = std::result::Result<T, Unexpected>;

macro_rules! check_next {
    ($l:ident, $want:path) => {
        match $l.next() {
            $want => {}
            t => Err(Unexpected(t))?,
        }
    };
}

pub fn query(l: &mut Lexer) -> Result<Node> {
    let q = match l.peek() {
        Token::Select => select(l),
        t => Err(Unexpected(t)),
    };

    match l.next() {
        Token::Semicolon => q,
        t => Err(Unexpected(t)),
    }
}

fn select(l: &mut Lexer) -> Result<Node> {
    assert_eq!(l.next(), Token::Select);

    let fields = fields(l)?;
    assert_eq!(l.next(), Token::From);
    let table = Box::new(table_expr(l)?);

    let mut r#where = None;
    let mut group = vec![];
    let mut order = vec![];
    let mut joins = vec![];
    let mut limit = None;

    // Need to make sure each clause is parsed in the order in which they're allowed to appear
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
        fields,
        table,
        r#where,
        group,
        order,
        joins,
        limit,
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

    fn integer_list(l: &mut Lexer, mut list: Vec<Node>, state: State) -> Result<Vec<Node>> {
        match l.peek() {
            Token::Comma if state == State::Comma => {
                l.next();
                integer_list(l, list, State::Item)
            }
            Token::IntegerLiteral(i) if state == State::Item => {
                l.next();
                list.push(Node::IntegerLiteral(i));
                integer_list(l, list, State::Comma)
            }

            _ if state == State::Comma => return Ok(list),
            t => Err(Unexpected(t)),
        }
    }

    fn string_list(l: &mut Lexer, mut list: Vec<Node>, state: State) -> Result<Vec<Node>> {
        match l.peek() {
            Token::Comma if state == State::Comma => {
                l.next();
                string_list(l, list, State::Item)
            }
            Token::StringLiteral(s) if state == State::Item => {
                l.next();
                list.push(Node::StringLiteral(s));
                string_list(l, list, State::Comma)
            }

            _ if state == State::Comma => return Ok(list),
            t => Err(Unexpected(t)),
        }
    }

    match l.peek() {
        Token::IntegerLiteral(_) => integer_list(l, Vec::new(), State::Item),
        Token::StringLiteral(_) => string_list(l, Vec::new(), State::Item),
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
        Token::LParen => parens(l, select),
        t => Err(Unexpected(t)), // TODO: parse select
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
    fn test_parse_select() -> Result<()> {
        struct Test {
            input: &'static str,
            want: Node,
        }

        let tcs = [
            Test {
                input: "select * from tablea;",
                want: Node::Select {
                    fields: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![],
                    limit: None,
                },
            },
            Test {
                input: "select columna, columnb from tablea;",
                want: Node::Select {
                    fields: vec![
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
                },
            },
            Test {
                input: "select columna, columnb, columnc from tablea;",
                want: Node::Select {
                    fields: vec![
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
                },
            },
            Test {
                input: "select * from tablea join tableb using (columna, columnb);",
                want: Node::Select {
                    fields: vec![Node::All],
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
                },
            },
            Test {
                input: "select * from tablea join (select * from tableb) using (columna, columnb);",
                want: Node::Select {
                    fields: vec![Node::All],
                    table: Box::new(Node::TableRef("tablea".into())),
                    r#where: None,
                    group: vec![],
                    order: vec![],
                    joins: vec![Node::JoinUsing {
                        table: Box::new(Node::Select {
                            fields: vec![Node::All],
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
                },
            },
            Test {
                input: "select * from tablea join tableb on (tablea.columna = tableb.columna);",
                want: Node::Select {
                    fields: vec![Node::All],
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
                },
            },
            Test {
                input: "select * from tablea
                    join tableb on (tablea.columna = tableb.columna)
                    join tablec on (tablea.columna = tablec.columna);",
                want: Node::Select {
                    fields: vec![Node::All],
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
                },
            },
            Test {
                input: "select * from tablea where columna not null;",
                want: Node::Select {
                    fields: vec![Node::All],
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
                },
            },
            Test {
                input: "select * from tablea where columna not null group by columna, columnb order by columnb;",
                want: Node::Select {
                    fields: vec![Node::All],
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
                },
            },
            Test {
                input: "select * from tablea
                    join tableb using (columna)
                    where columna > 1000 and columnb not in (1, 2, 3, 4)
                    group by columna, columnb
                    order by columnb
                    limit 100;",
                want: Node::Select {
                    fields: vec![Node::All],
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
                },
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = query(&mut l)?;

            assert_eq!(want, have);
        }

        Ok(())
    }

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
}
