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
    Group,
    By,
    Order,
    Asc,
    Desc,

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

macro_rules! match_state {
    ($self:ident, $other:ident, [$($t1:tt $(=> $t2:tt)?),*] ) => {
        match $self {
            $(
                Token::$t1 $($t2)? => match $other {
                    State::$t1 => true,
                    _ => false
                },
            )*
        }
    };
}

impl PartialEq<State> for Token {
    fn eq(&self, other: &State) -> bool {
        match_state!(
            self,
            other,
            [
                LParen,
                RParen,
                Create,
                Table,
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
                Group,
                By,
                Order,
                Asc,
                Desc,
                Eq,
                Neq,
                Lt,
                Le,
                Gt,
                Ge,
                StringLiteral => (_),
                IntegerLiteral => (_),
                TableAndColumnReference => (_, _),
                TableOrColumnReference => (_)
            ]
        )
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

    pub fn pop(&mut self) -> usize {
        let ret = self.inner[self.len - 1];
        self.len -= 1;
        ret
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
pub static mut NODES: [SNode; 256] = [SNode {
    token: State::Invalid,
    tag: Tag::None,
    adjacent: array!(),
}; 256];

macro_rules! node {
    () => {{
            let i = NODES_IDX;
            NODES_IDX += 1;
            i
    }};
    ($token:expr) => {
        {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;
            i
        }
    };
    ($token:expr, [[$a:expr]]) => {
         {
            let i = NODES_IDX;
            NODES_IDX += 1;

            let n = &mut NODES[i];
            n.token = $token;
            n.adjacent.extend($a);

            i
        }
    };
    ($token:expr, [[$a:expr]], [$($i:expr),*]) => {
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

fn copy_grammar(i: usize) -> usize {
    use std::collections::HashSet;

    fn copy_grammar(i: usize, visited: &mut HashSet<usize>) -> usize {
        unsafe {
            if visited.contains(&i) {
                return i;
            }

            visited.insert(i);

            let n = NODES[i];
            let j = node!(n.token, n.tag);

            for k in n.adjacent {
                NODES[j].adjacent.push(copy_grammar(k, visited));
            }

            j
        }
    }

    copy_grammar(i, &mut HashSet::new())
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

    fn condition(conn: SArray) -> SArray {
        unsafe {
            // [<t>.<c>|<c>|StringLiteral|IntegerLiteral] c
            let rhs = array![
                node!(State::TableAndColumnReference, [[conn]]),
                node!(State::TableOrColumnReference, [[conn]]),
                node!(State::StringLiteral, [[conn]]),
                node!(State::IntegerLiteral, [[conn]])
            ];

            let o = array![
                node!(State::Eq, [[rhs]]),
                node!(State::Neq, [[rhs]]),
                node!(State::Gt, [[rhs]]),
                node!(State::Ge, [[rhs]]),
                node!(State::Lt, [[rhs]]),
                node!(State::Le, [[rhs]])
            ];

            // IN ([StringLiteral [,*|)]|IntegerLiteral [,*|)]])
            let inexpr = node!(
                State::In,
                [node!(
                    State::LParen,
                    [
                        node!(sl State::StringLiteral, [
                            node!(State::Comma, [sl]), // Cycle
                            node!(State::RParen, [[conn]])
                        ]),
                        node!(il State::IntegerLiteral, [
                            node!(State::Comma, [il]), // Cycle
                            node!(State::RParen, [[conn]])
                        ])
                    ]
                )]
            );

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
            array![
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
            ]
        }
    }

    pub fn select_stmt() -> usize {
        static mut SELECT_STMT: isize = -1;

        unsafe {
            if SELECT_STMT >= 0 {
                return SELECT_STMT as usize;
            }

            // ;
            let e = node!(State::Semicolon);

            // ORDER BY [<c>|<t>.<c>] [ASC|DESC]
            let o = node!(
                State::Order,
                [node!(
                    State::By,
                    [
                        node!(
                            State::TableOrColumnReference,
                            [node!(State::Asc, [e]), node!(State::Desc, [e]), e]
                        ),
                        node!(
                            State::TableAndColumnReference,
                            [node!(State::Asc, [e]), node!(State::Desc, [e]), e]
                        )
                    ]
                )]
            );

            // GROUP BY [<c>|<t>.<c>]
            let g = node!(
                State::Group,
                [node!(
                    State::By,
                    [[NODES[node!(c State::Comma, [
                        node!(State::TableOrColumnReference, [c, o, e]),
                        node!(State::TableAndColumnReference, [c, o, e])
                    ])]
                    .adjacent]]
                )]
            );

            // [(AND condition)*|(OR condition)*|;]
            let conn = array![
                node!(State::Conjunction),
                node!(State::Disjunction),
                o,
                g,
                e
            ];

            let cond = Self::condition(conn);
            NODES[conn[0]].adjacent.extend(cond);
            NODES[conn[1]].adjacent.extend(cond);
            NODES[cond[4]].adjacent.extend(cond);

            let w = node!(State::Where, [[cond]]);

            let jconn = array![
                node!(State::Conjunction),
                node!(State::Disjunction),
                node!(State::RParen, [o, g, e, w])
            ];

            let jcond = Self::condition(jconn);
            NODES[jconn[0]].adjacent.extend(jcond);
            NODES[jconn[1]].adjacent.extend(jcond);
            NODES[cond[4]].adjacent.extend(jcond);

            let j = node!(
                State::Join,
                [
                    node!(State::TableOrColumnReference, [
                        node!(State::On, [
                              node!(State::LParen, [[jcond]])
                        ]),
                        node!(
                            State::Using,
                            [node!(State::LParen, [[
                                NODES[node!(c State::Comma, [
                                    node!(State::TableAndColumnReference, [c, node!(State::RParen, [o, g, e, w])]),
                                    node!(State::TableOrColumnReference, [c, node!(State::RParen, [o, g, e, w])])
                                ])].adjacent
                            ]])]
                        )
                    ])
                ]
            );

            let f = node!(
                State::From,
                [node!(
                    State::TableOrColumnReference,
                    Tag::Table,
                    [e, g, w, j]
                )]
            );

            // SELECT [*| <c>|<t>.<c>] [,|from_clause]
            SELECT_STMT = node!(
                State::Select,
                [[NODES[node!(c State::Comma, [
                    node!(State::TableOrColumnReference, Tag::Targets, [c, f]),
                    node!(State::TableAndColumnReference, Tag::Targets, [c, f])
                ])]
                .adjacent]],
                [node!(State::All, [f])]
            ) as isize;

            SELECT_STMT as usize
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{next_tkn, parse::Error, Lexer};

    use super::*;

    #[test]
    fn test_copy_grammar() {
        struct Testcase {
            g: usize,
            input: &'static str,
        }

        unsafe {
            let tcs = [
                Testcase {
                    g: node!(
                        State::Invalid,
                        [node!(
                            State::Select,
                            [node!(State::All, [node!(State::From)])]
                        )]
                    ),
                    input: "select * from",
                },
                Testcase {
                    g: node!(
                        State::Invalid,
                        [[NODES[node!(c State::Comma, [
                            node!(State::IntegerLiteral, [c, node!(State::Semicolon)]),
                            node!(State::StringLiteral, [c, node!(State::Semicolon)])
                        ])]
                        .adjacent]]
                    ),
                    input: "1, 2, 3, 4, 5;",
                },
            ];

            for Testcase { g, input } in tcs {
                let mut l = Lexer::new(input);
                let mut lc = Lexer::new(input);
                let gc = copy_grammar(g);

                assert!(test(&mut l, g));
                assert!(test(&mut lc, gc));

                // Modify the copy and ensure the original remains the same
                NODES[gc]
                    .adjacent
                    .push(node!(State::Semicolon, [NODES[gc].adjacent[0]]));
                let ninput = format!("; {input}");
                let mut l = Lexer::new(&ninput);
                let mut lc = Lexer::new(&ninput);

                assert!(!test(&mut l, g));
                assert!(test(&mut lc, gc));
            }
        }
    }

    fn test(l: &mut Lexer<'_>, mut cur: usize) -> bool {
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
                    if tkn == NODES[n].token {
                        cur = n;
                        m = true;
                        break 'adj;
                    };
                }

                if !m {
                    break 'l;
                }
            }

            m
        }
    }

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
            TestCase {
                input: "select columna, columnb from tablea \
                    group by columna;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna < columnb
                    group by columna;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where not columna < columnb
                    group by columna;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    where columna in (\"1\", \"2\")
                    group by columna;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    join tableb on (tablea.columna = tableb.columnb)
                    where columna = 1
                    group by columna;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    join tableb on (tablea.columna = tableb.columna and tablea.columbb = tableb.columnb)
                    where columna = 1
                    group by columna;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    join tableb using (columna, columnb)
                    where columna = 1
                    group by columna;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    join tableb using (columna, columnb)
                    where columna = 1
                    group by columna
                    order by columna desc;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    join tableb using (columna, columnb)
                    order by columna asc;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "select columna, columnb from tablea \
                    join tableb using (columna, columnb)
                    group by columna;",
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
            let cur = Node::select_stmt();

            assert!(next_tkn!(l) == Token::Select);

            let m = test(&mut l, cur);

            if !m && !matches {
                continue;
            }

            if !m && matches {
                panic!(
                    "Expected input to match\ninput: {:?}\nremaiming tokens: {:?}",
                    input,
                    l.to_vec()
                );
            }

            if m && !matches {
                panic!("Expected input to not match");
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
            let cur = Node::create_stmt();

            assert!(next_tkn!(l) == Token::Create);

            let m = test(&mut l, cur);

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

        Ok(())
    }
}
