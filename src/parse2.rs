use crate::{next_tkn, Lexer, Token};

macro_rules! next_tkn {
    ($l:ident) => {
        $l.next().expect("expected token")
    };
}

struct ListCell {
    value: Node,
    next: Option<Box<ListCell>>,
}

impl ListCell {
    fn new(value: Node) -> Self {
        Self { value, next: None }
    }

    fn next(mut self, value: Node) -> Self {
        self.next = Some(Box::new(Self::new(value)));

        self
    }
}

struct List {
    head: Option<Box<ListCell>>,
}

impl List {
    fn new() -> Self {
        Self { head: None }
    }

    fn insert(mut self, value: Node) -> Self {
        let mut cur = &mut self.head;

        while let Some(cell) = cur {
            cur = &mut cell.next;
        }

        *cur = Some(Box::new(ListCell::new(value)));

        self
    }
}

enum Node {
    Select {
        fields: List,
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

fn query(l: &mut Lexer) -> Node {
    match next_tkn!(l) {
        Token::Select => select(l),
        _ => Node::None,
    }
}

fn select(l: &mut Lexer) -> Node {
    let f = fields(l);
    let t = table_expr(l);

    Node::Select {
        fields: f,
        table_expr: Box::new(t),
        where_expr: None,
        group_expr: None,
        order_expr: None,
        join_expr: None,
        limit: None,
    }
}

fn fields(l: &mut Lexer) -> List {
    match l.peek() {
        Token::All => {
            l.next();
            return List::new().insert(Node::All);
        }

        _ => column_list(l),
    }
}

fn column_list(l: &mut Lexer) -> List {
    #[derive(PartialEq, Debug)]
    enum State {
        Comma,
        Column,
    }

    fn column_list(l: &mut Lexer, mut list: List, state: State) -> List {
        match l.peek() {
            Token::From if state == State::Comma => return list,
            Token::Comma if state == State::Comma => {
                l.next();
                column_list(l, list, State::Column)
            }
            Token::TableOrColumnReference(_) | Token::TableAndColumnReference(_, _)
                if state == State::Column =>
            {
                list = list.insert(column_ref(l));
                column_list(l, list, State::Comma)
            }

            t => {
                panic!("unexpected column/state");
            }
        }
    }

    column_list(l, List::new(), State::Column)
}

fn column_ref(l: &mut Lexer) -> Node {
    match l.next().unwrap() {
        Token::TableOrColumnReference(column) => Node::ColumnRef {
            table: None,
            column,
            alias: None,
        },

        Token::TableAndColumnReference(table, column) => Node::ColumnRef {
            table: Some(table),
            column,
            alias: None,
        },

        _ => panic!("unexpected token"),
    }
}

fn table_expr(l: &mut Lexer) -> Node {
    if l.next().unwrap() != Token::From {
        panic!("unexpected token");
    }

    match l.next().unwrap() {
        Token::TableOrColumnReference(table) => Node::TableRef(table),
        _ => panic!("unexpected token"), // TODO: parse select
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_select() {
        struct Test {
            input: &'static str,
            want: Node,
        }

        let tcs = [
            Test {
                input: "select * from tablea;",
                want: Node::None,
            },
            Test {
                input: "select columna, columnb from tablea;",
                want: Node::None,
            },
            Test {
                input: "select columna, columnb, columnc from tablea;",
                want: Node::None,
            },
        ];

        for Test { input, want } in tcs {
            let mut l = Lexer::new(input);
            let q = query(&mut l);
        }
    }
}
