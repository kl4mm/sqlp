use crate::{Lexer, Token};

pub mod parse;

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
        columns: Vec<Node>,
        table: Box<Node>,           // TableRef or Select
        r#where: Option<Box<Node>>, // Expr
        group: Vec<Node>,
        order: Vec<Node>,
        joins: Vec<Node>,
        limit: Option<Box<Node>>,
    },

    Insert {
        columns: Vec<Node>, // ColumnRef
        table: Box<Node>,
        inserts: Vec<Vec<Node>>, // List of list of StringLiteral or IntegerLiteral
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
                    write!(f, "{}.", table.as_ref().unwrap())?;
                }

                write!(f, "{column}")?;

                if alias.is_some() {
                    write!(f, "AS {}", alias.as_ref().unwrap())?;
                }

                Ok(())
            }
            Node::TableRef(t) => write!(f, "{t}"),
            Node::JoinUsing { table, columns } => {
                write!(f, "JOIN {table} USING (")?;
                for c in columns {
                    write!(f, " {c}")?;
                }
                write!(f, ")")
            }
            Node::JoinOn { table, expr } => {
                write!(f, "JOIN {table} ON {expr}")
            }
            Node::StringLiteral(s) => write!(f, "\"{s}\""),
            Node::IntegerLiteral(i) => write!(f, "{i}"),
            Node::All => write!(f, "ALL"),
            Node::Null => write!(f, "NULL"),
            Node::None => write!(f, "NONE"),
            _ => unimplemented!(),
        }
    }
}

#[derive(PartialEq)]
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

#[macro_export]
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
        Token::Select => self::parse::select(l),
        Token::Insert => self::parse::insert(l),
        t => Err(Unexpected(t)),
    };

    match l.next() {
        Token::Semicolon => q,
        t => Err(Unexpected(t)),
    }
}
