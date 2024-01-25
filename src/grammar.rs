use std::ops::Index;

use crate::Token;

#[derive(Copy, Clone, Debug)]
pub enum Tag {
    Targets,
    Defs,
    Table,
    None,
}

#[derive(Debug)]
pub struct Node {
    pub token: Token,
    pub tag: Tag,
    pub adjacent: Vec<*mut Node>,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum State {
    LParen,
    RParen,

    Create,
    Table,

    // Should be Type(<type>)
    Int,

    Select,
    Insert,
    Update,
    Delete,
    Into,
    Values,
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
    In,
    Between,
    Is,

    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    StringLiteral,
    IntegerLiteral,

    TableAndColumnReference,
    TableOrColumnReference,

    Invalid,
}

impl PartialEq<Token> for State {
    fn eq(&self, other: &Token) -> bool {
        match self {
            State::LParen => match other {
                Token::LParen => true,
                _ => false,
            },
            State::RParen => match other {
                Token::RParen => true,
                _ => false,
            },
            State::Create => match other {
                Token::Create => true,
                _ => false,
            },
            State::Table => match other {
                Token::Table => true,
                _ => false,
            },
            State::Int => match other {
                Token::Int => true,
                _ => false,
            },
            State::Select => match other {
                Token::Select => true,
                _ => false,
            },
            State::Insert => match other {
                Token::Insert => true,
                _ => false,
            },
            State::Update => match other {
                Token::Update => true,
                _ => false,
            },
            State::Delete => match other {
                Token::Delete => true,
                _ => false,
            },
            State::Into => match other {
                Token::Into => true,
                _ => false,
            },
            State::Values => match other {
                Token::Values => true,
                _ => false,
            },
            State::From => match other {
                Token::From => true,
                _ => false,
            },
            State::Where => match other {
                Token::Where => true,
                _ => false,
            },
            State::Join => match other {
                Token::Join => true,
                _ => false,
            },
            State::On => match other {
                Token::On => true,
                _ => false,
            },
            State::Using => match other {
                Token::Using => true,
                _ => false,
            },
            State::As => match other {
                Token::As => true,
                _ => false,
            },
            State::Conjunction => match other {
                Token::Conjunction => true,
                _ => false,
            },
            State::Disjunction => match other {
                Token::Disjunction => true,
                _ => false,
            },
            State::Negation => match other {
                Token::Negation => true,
                _ => false,
            },
            State::Null => match other {
                Token::Null => true,
                _ => false,
            },
            State::Semicolon => match other {
                Token::Semicolon => true,
                _ => false,
            },
            State::Comma => match other {
                Token::Comma => true,
                _ => false,
            },
            State::All => match other {
                Token::All => true,
                _ => false,
            },
            State::In => match other {
                Token::In => true,
                _ => false,
            },
            State::Between => match other {
                Token::Between => true,
                _ => false,
            },
            State::Is => match other {
                Token::Is => true,
                _ => false,
            },
            State::Eq => match other {
                Token::Eq => true,
                _ => false,
            },
            State::Neq => match other {
                Token::Neq => true,
                _ => false,
            },
            State::Lt => match other {
                Token::Lt => true,
                _ => false,
            },
            State::Le => match other {
                Token::Le => true,
                _ => false,
            },
            State::Gt => match other {
                Token::Gt => true,
                _ => false,
            },
            State::Ge => match other {
                Token::Ge => true,
                _ => false,
            },
            State::StringLiteral => match other {
                Token::StringLiteral(_) => true,
                _ => false,
            },
            State::IntegerLiteral => match other {
                Token::IntegerLiteral(_) => true,
                _ => false,
            },
            State::TableAndColumnReference => match other {
                Token::TableAndColumnReference(_, _) => true,
                _ => false,
            },
            State::TableOrColumnReference => match other {
                Token::TableOrColumnReference(_) => true,
                _ => false,
            },
            State::Invalid => false,
        }
    }
}

#[derive(Copy, Clone)]
pub struct SNode {
    pub token: State,
    pub tag: Tag,
    pub adjacent: SArray,
}

macro_rules! array {
    ($($e:expr),*) => {
        {
            #[allow(unused_mut)]
            let mut a = SArray {
                inner: [0; 32],
                len: 0,
            };

            $(
                a.push($e);
            )*

            a
        }
    };
}

#[derive(Copy, Clone)]
pub struct SArray {
    inner: [usize; 32],
    len: usize,
}

pub struct Iter<'a> {
    inner: &'a SArray,
    i: usize,
}

pub struct IntoIter {
    inner: SArray,
    i: usize,
}

impl Iterator for IntoIter {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.inner.len() {
            return None;
        }

        self.i += 1;
        Some(self.inner[self.i - 1])
    }
}

impl Index<usize> for SArray {
    type Output = usize;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[index]
    }
}

impl IntoIterator for SArray {
    type Item = usize;

    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { inner: self, i: 0 }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.inner.len() {
            return None;
        }

        self.i += 1;
        Some(self.inner[self.i - 1])
    }
}

impl Extend<usize> for SArray {
    fn extend<T: IntoIterator<Item = usize>>(&mut self, iter: T) {
        for i in iter {
            self.push(i);
        }
    }
}

impl SArray {
    pub fn push(&mut self, x: usize) {
        self.inner[self.len] = x;
        self.len += 1;
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn iter<'a>(&'a self) -> Iter<'a> {
        Iter { inner: self, i: 0 }
    }
}

static mut NODES_IDX: usize = 0;
pub static mut NODES: [SNode; 128] = [SNode {
    token: State::Invalid,
    tag: Tag::None,
    adjacent: array!(),
}; 128];

macro_rules! node {
    () => {
            let i = NODES_IDX;
            NODES_IDX += 1;
            i
    };
    ($token:expr) => {
        {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;
            i
        }
    };
    ($token:expr, [[$a:ident]]) => {
         {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;
            n.adjacent.extend($a);

            i
        }
    };
    ($token:expr, [[$a:ident]], [$($i:expr),*]) => {
         {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;
            n.adjacent.extend($a);

            $(
                n.adjacent.push($i);
            )*

            i
        }
    };
    ($token:expr, [$($i:expr),*]) => {
         {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;

            $(
                n.adjacent.push($i);
            )*

            i
        }
    };
    ($ns:ident $token:expr, [$($i:expr),*]) => {
         {
            let $ns = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[$ns];
            n.token = $token;

            $(
                n.adjacent.push($i);
            )*

            $ns
        }
    };
    ($token:expr, $tag:expr) => {
        {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;
            n.tag = $tag;

            i
        }
    };
    ($token:expr, $tag:expr, [$($i:expr),*]) => {
        {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;
            n.tag = $tag;

            $(
                n.adjacent.push($i);
            )*

            i
        }
    };
    ($ns:ident $token:expr, $tag:expr, [$($i:expr),*]) => {
        {
            let $ns = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[$ns];

            n.token = $token;
            n.tag = $tag;

            $(
                n.adjacent.push($i);
            )*

            $ns
        }
    };
}

impl Node {
    pub fn create_stmt() -> usize {
        static mut CREATE_STMT: isize = -1;

        unsafe {
            if CREATE_STMT >= 0 {
                return CREATE_STMT as usize;
            }

            CREATE_STMT = node!(
                State::Create,
                [node!(
                    State::Table,
                    [node!(
                        State::TableOrColumnReference,
                        Tag::Table,
                        [node!(
                            State::LParen,
                            [node!(
                                col_def
                                State::TableOrColumnReference,
                                Tag::Defs,
                                [
                                    node!(
                                        State::Int,
                                        [
                                            node!(State::Comma, [col_def]), // Cycle
                                            node!(State::RParen, [node!(State::Semicolon)])
                                        ]
                                    )
                                ]
                            )]
                        )]
                    )]
                )]
            ) as isize;

            CREATE_STMT as usize
        }
    }

    pub fn select_stmt() -> usize {
        static mut SELECT_STMT: isize = -1;

        unsafe {
            if SELECT_STMT >= 0 {
                return SELECT_STMT as usize;
            }

            let e = node!(State::Semicolon);

            // [(AND condition)*|(OR condition)*|;]
            let conn = array![node!(State::Conjunction), node!(State::Disjunction), e];

            // [<t>.<c>|<c>|StringLiteral|IntegerLiteral] c
            let rhs = array![
                node!(State::TableAndColumnReference, [[conn]]),
                node!(State::TableOrColumnReference, [[conn]]),
                node!(State::StringLiteral, [[conn]]),
                node!(State::IntegerLiteral, [[conn]])
            ];

            // StringLiteral [,*|)]
            let sl = node!(sl State::StringLiteral, [
                node!(State::Comma, [sl]), // Cycle
                node!(State::RParen, [[conn]])
            ]);

            // IntegerLiteral [,*|)]
            let il = node!(il State::IntegerLiteral, [
                node!(State::Comma, [il]),
                node!(State::RParen, [[conn]])
            ]);

            // IN ([string_list|int_list])
            let inexpr = node!(State::In, [node!(State::LParen, [sl, il])]);

            // BETWEEN IntegerLiteral AND IntegerLiteral
            let btwexpr = node!(
                State::Between,
                [node!(
                    State::IntegerLiteral,
                    [node!(
                        State::Conjunction,
                        [node!(State::IntegerLiteral, [[conn]])]
                    )]
                )]
            );

            let null = node!(State::Null, [[conn]]);

            // IS [NOT|] NULL
            let isexpr = node!(State::Is, [null, node!(State::Negation, [null])]);

            // NOT [inexpr|btwexpr|isexpr]
            let notexpr = node!(State::Negation, [inexpr, btwexpr, isexpr]);

            let o = array![
                node!(State::Eq, [[rhs]]),
                node!(State::Neq, [[rhs]]),
                node!(State::Gt, [[rhs]]),
                node!(State::Ge, [[rhs]]),
                node!(State::Lt, [[rhs]]),
                node!(State::Le, [[rhs]])
            ];

            // TODO: add support for summands and factors in future. keeping it simple for now
            // TODO: add grammar for term (value, function, col_ref), which should also be used in
            // place of col_refs. sticking to col_refs and literals for now
            // lhs_operand = [<t>.<c>|<c>|StringLiteral|IntegerLiteral]
            // lhs_operand [
            //   [=|!=|>|>=|<|<=] rhs_operand
            //   | [NOT|] IN ( (constOperand),* )
            //   | [NOT|] BETWEEN IntegerLiteral AND IntegerLiteral
            //   | IS [NOT|] NULL
            // ]
            // | NOT expr
            let cond = array![
                node!(
                    State::TableAndColumnReference,
                    [[o]],
                    [notexpr, inexpr, btwexpr, isexpr]
                ),
                node!(
                    State::TableOrColumnReference,
                    [[o]],
                    [notexpr, inexpr, btwexpr, isexpr]
                ),
                node!(
                    State::StringLiteral,
                    [[o]],
                    [notexpr, inexpr, btwexpr, isexpr]
                ),
                node!(
                    State::IntegerLiteral,
                    [[o]],
                    [notexpr, inexpr, btwexpr, isexpr]
                ),
                node!(State::Negation)
            ];
            NODES[conn[0]].adjacent.extend(cond);
            NODES[conn[1]].adjacent.extend(cond);
            NODES[cond[4]].adjacent.extend(cond);

            let f = node!(
                State::From,
                [node!(
                    State::TableOrColumnReference,
                    Tag::Table,
                    [e, node!(State::Where, [[cond]])]
                )]
            );

            // [<c>|<t>.<c>] [,|from_clause]
            let cr1 = node!(State::TableOrColumnReference, Tag::Targets, [f]);
            let cr2 = node!(State::TableAndColumnReference, Tag::Targets, [f]);
            let c = node!(State::Comma, [cr1, cr2]);
            NODES[cr1].adjacent.push(c);
            NODES[cr2].adjacent.push(c);

            SELECT_STMT = node!(State::Select, [node!(State::All, [f]), cr1, cr2]) as isize;

            SELECT_STMT as usize
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{next_tkn, parse::Error, Lexer};

    use super::*;

    #[test]
    fn test_select_grammar() -> Result<(), Error> {
        const ONLY: bool = false;

        struct TestCase {
            input: &'static str,
            matches: bool,
            only: bool,
        }

        let tcs = [
            TestCase {
                input: "select * from tablea;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, * from tablea;",
                matches: false,
                only: false,
            },
            TestCase {
                input: "select *, columna from tablea;",
                matches: false,
                only: false,
            },
            TestCase {
                input: "select tablea.columna, columna from tablea;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea where columna > columnb;",
                matches: true,
                only: false,
            },
            TestCase {
                input:
                    "select columna, columnb from tablea where columna > columnb and columnc = columnd;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna > columnb \
                    and columnc = columnd \
                    or columna > columnb;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where not columna > columnb;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna between 100 and 200;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna not between 100 and 200;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna in (1, 2, 3);",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna not in (1, 2, 3);",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna is null;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna is not null;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna > columnb \
                    and columnc in (1, 2, 3) \
                    or columnd between 100 and 200;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where not columna > columnb \
                    and columnc not in (1, 2, 3) \
                    or columnd not between 100 and 200;",
                matches: true,
                only: false,
            },
            // TODO: look at this later
            TestCase {
                input: "select columna, columnb from tablea \
                    where not not not columna > columnb \
                    and columnc not in (1, 2, 3) \
                    or columnd not between 100 and 200
                    and columne is not null;",
                matches: true,
                only: false,
            },
        ];

        for TestCase {
            input,
            matches,
            only,
        } in tcs
        {
            if ONLY && !only {
                continue;
            }

            let mut l = Lexer::new(input);
            let mut cur = Node::select_stmt();

            assert!(next_tkn!(l) == Token::Select);

            unsafe {
                let mut m = false;
                'l: loop {
                    let node = NODES[cur];
                    if node.adjacent.is_empty() {
                        break 'l;
                    }

                    let tkn = l.next().unwrap();

                    m = false;
                    'adj: for n in node.adjacent.iter() {
                        if NODES[n].token == tkn {
                            cur = n;
                            m = true;
                            break 'adj;
                        };
                    }

                    if !m {
                        break 'l;
                    }
                }

                if !m && !matches {
                    continue;
                }

                if !m && matches {
                    panic!("Expected input to match\ninput: {:?}", input);
                }

                if m && !matches {
                    panic!("Expected input to not match");
                }
            }
        }

        Ok(())
    }

    #[test]
    fn test_create_grammar() -> Result<(), Error> {
        struct TestCase {
            input: &'static str,
            matches: bool,
        }

        let tcs = [
            TestCase {
                input: "create table tablea (
                       columna int
                    );",
                matches: true,
            },
            TestCase {
                input: "create table tablea (
                       columna int,
                       columnb int
                    );",
                matches: true,
            },
            TestCase {
                input: "create table tablea (
                       columna int,
                       columnb int,
                    );",
                matches: false,
            },
        ];

        for TestCase { input, matches } in tcs {
            let mut l = Lexer::new(input);
            let mut cur = Node::create_stmt();

            assert!(next_tkn!(l) == Token::Create);

            unsafe {
                let mut m = false;
                'l: loop {
                    let node = NODES[cur];
                    if node.adjacent.is_empty() {
                        break 'l;
                    }

                    let tkn = l.next().unwrap();

                    m = false;
                    'adj: for n in node.adjacent.iter() {
                        if NODES[n].token == tkn {
                            cur = n;
                            m = true;
                            break 'adj;
                        };
                    }

                    if !m {
                        break 'l;
                    }
                }

                if !m && !matches {
                    continue;
                }

                if !m && matches {
                    panic!("Expected input to match\ninput: {:?}", input);
                }

                if m && !matches {
                    panic!("Expected input to not match");
                }
            }
        }

        Ok(())
    }
}
