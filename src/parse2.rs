use crate::{Lexer, Token};

#[derive(Debug, PartialEq)]
pub enum Node {
    Select {
        fields: Vec<Node>,
        table_expr: Box<Node>,         // TableRef or Select
        where_expr: Option<Box<Node>>, // WhereExpr
        group_expr: Option<Box<Node>>,
        order_expr: Option<Box<Node>>,
        join_expr: Option<Box<Node>>,
        limit: Option<u64>,
    },

    WhereExpr(Box<Node>, Vec<Node>), // Could just be type Op

    ColumnRef {
        table: Option<String>,
        column: String,
        alias: Option<String>,
    },

    TableRef(String),

    All,

    None,
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

pub fn query(l: &mut Lexer) -> Result<Node> {
    match l.next() {
        Token::Select => select(l),
        t => Err(Unexpected(t))?,
    }
}

fn select(l: &mut Lexer) -> Result<Node> {
    let f = fields(l)?;
    let t = table_expr(l)?;

    Ok(Node::Select {
        fields: f,
        table_expr: Box::new(t),
        where_expr: None,
        group_expr: None,
        order_expr: None,
        join_expr: None,
        limit: None,
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
            Token::From if state == State::Comma => return Ok(list),
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

            t => Err(Unexpected(t)),
        }
    }

    column_list(l, Vec::new(), State::Column)
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
    if l.next() != Token::From {
        panic!("unexpected token");
    }

    match l.next() {
        Token::TableOrColumnReference(table) => Ok(Node::TableRef(table)),
        t => Err(Unexpected(t)), // TODO: parse select
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
                    table_expr: Box::new(Node::TableRef("tablea".into())),
                    where_expr: None,
                    group_expr: None,
                    order_expr: None,
                    join_expr: None,
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
                    table_expr: Box::new(Node::TableRef("tablea".into())),
                    where_expr: None,
                    group_expr: None,
                    order_expr: None,
                    join_expr: None,
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
                    table_expr: Box::new(Node::TableRef("tablea".into())),
                    where_expr: None,
                    group_expr: None,
                    order_expr: None,
                    join_expr: None,
                    limit: None,
                },
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let have = query(&mut l)?;

            assert!(want == have, "Want: {:?}\nHave: {:?}", want, have);
        }

        Ok(())
    }
}
