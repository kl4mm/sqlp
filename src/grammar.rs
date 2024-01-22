use crate::Token;

macro_rules! ptr {
    ($e:expr) => {
        Box::into_raw(Box::new($e))
    };
}

#[derive(Debug)]
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

impl Node {
    pub fn create_stmt() -> *const Self {
        static mut CREATE_STMT: *const Node = std::ptr::null_mut();

        unsafe {
            if !CREATE_STMT.is_null() {
                return CREATE_STMT;
            }

            let end = ptr!(Self {
                token: Token::RParen,
                tag: Tag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::Semicolon,
                    tag: Tag::None,
                    adjacent: vec![]
                })],
            });

            // Cycle
            // <c> INT [,|);]
            let col_def = ptr!(Self {
                token: Token::TableOrColumnReference(String::new()),
                tag: Tag::Defs,
                adjacent: vec![ptr!(Self {
                    token: Token::Int,
                    tag: Tag::None,
                    adjacent: vec![
                        ptr!(Self {
                            token: Token::Comma,
                            tag: Tag::None,
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
                tag: Tag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::Table,
                    tag: Tag::None,
                    adjacent: vec![ptr!(Self {
                        token: Token::TableOrColumnReference(String::new()),
                        tag: Tag::Table,
                        adjacent: vec![ptr!(Self {
                            token: Token::LParen,
                            tag: Tag::None,
                            adjacent: vec![col_def]
                        })]
                    })],
                })],
            });

            CREATE_STMT
        }
    }

    pub fn select_stmt() -> *const Self {
        static mut SELECT_STMT: *const Node = std::ptr::null_mut();

        unsafe {
            if !SELECT_STMT.is_null() {
                return SELECT_STMT;
            }

            let end = ptr!(Self {
                token: Token::Semicolon,
                tag: Tag::None,
                adjacent: vec![],
            });

            // [(AND condition)*|(OR condition)*|;]
            let mut connector = vec![
                ptr!(Self {
                    token: Token::Conjunction,
                    tag: Tag::None,
                    adjacent: vec![] // conditions
                }),
                ptr!(Self {
                    token: Token::Disjunction,
                    tag: Tag::None,
                    adjacent: vec![] // conditions
                }),
                end,
                // TODO: Next clause
            ];

            // [<t>.<c>|<c>|StringLiteral|IntegerLiteral] connector
            let rhs_operands = vec![
                ptr!(Self {
                    token: Token::TableAndColumnReference(String::new(), String::new()),
                    tag: Tag::None,
                    adjacent: connector.clone(),
                }),
                ptr!(Self {
                    token: Token::TableOrColumnReference(String::new()),
                    tag: Tag::None,
                    adjacent: connector.clone(),
                }),
                ptr!(Self {
                    token: Token::StringLiteral(String::new()),
                    tag: Tag::None,
                    adjacent: connector.clone(),
                }),
                ptr!(Self {
                    token: Token::IntegerLiteral(0),
                    tag: Tag::None,
                    adjacent: connector.clone(),
                }),
            ];

            // [=|!=|>|>=|<|<=]
            let operators = vec![
                ptr!(Self {
                    token: Token::Eq,
                    tag: Tag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Neq,
                    tag: Tag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Gt,
                    tag: Tag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Ge,
                    tag: Tag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Lt,
                    tag: Tag::None,
                    adjacent: rhs_operands.clone(),
                }),
                ptr!(Self {
                    token: Token::Le,
                    tag: Tag::None,
                    adjacent: rhs_operands
                }),
            ];

            // StringLiteral [,*|)]
            let string_list = ptr!(Self {
                token: Token::StringLiteral(String::new()),
                tag: Tag::None,
                adjacent: vec![
                    ptr!(Self {
                        token: Token::Comma,
                        tag: Tag::None,
                        adjacent: vec![], // Cycle
                    }),
                    ptr!(Self {
                        token: Token::RParen,
                        tag: Tag::None,
                        adjacent: connector.clone(),
                    })
                ],
            });
            (*(*string_list).adjacent[0]).adjacent.push(string_list);

            // IntegerLiteral [,*|)]
            let int_list = ptr!(Self {
                token: Token::IntegerLiteral(0),
                tag: Tag::None,
                adjacent: vec![
                    ptr!(Self {
                        token: Token::Comma,
                        tag: Tag::None,
                        adjacent: vec![], // Cycle
                    }),
                    ptr!(Self {
                        token: Token::RParen,
                        tag: Tag::None,
                        adjacent: connector.clone(),
                    })
                ],
            });
            (*(*int_list).adjacent[0]).adjacent.push(int_list);

            // IN ([string_list|int_list])
            let in_expr = ptr!(Self {
                token: Token::In,
                tag: Tag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::LParen,
                    tag: Tag::None,
                    adjacent: vec![string_list, int_list]
                })]
            });

            let between_expr = ptr!(Self {
                token: Token::Between,
                tag: Tag::None,
                adjacent: vec![ptr!(Self {
                    token: Token::IntegerLiteral(0),
                    tag: Tag::None,
                    adjacent: vec![ptr!(Self {
                        token: Token::Conjunction,
                        tag: Tag::None,
                        adjacent: vec![ptr!(Self {
                            token: Token::IntegerLiteral(0),
                            tag: Tag::None,
                            adjacent: connector.clone()
                        })]
                    })]
                })]
            });

            let null = ptr!(Self {
                token: Token::Null,
                tag: Tag::None,
                adjacent: connector.clone()
            });
            let is_expr = ptr!(Self {
                token: Token::Is,
                tag: Tag::None,
                adjacent: vec![
                    null,
                    ptr!(Self {
                        token: Token::Negation,
                        tag: Tag::None,
                        adjacent: vec![null]
                    })
                ]
            });

            let not_expr = ptr!(Self {
                token: Token::Negation,
                tag: Tag::None,
                adjacent: vec![in_expr, between_expr, is_expr]
            });

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
            let mut condition = vec![
                // LHS operands
                ptr!(Self {
                    token: Token::TableAndColumnReference(String::new(), String::new()),
                    tag: Tag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
                ptr!(Self {
                    token: Token::TableOrColumnReference(String::new()),
                    tag: Tag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
                ptr!(Self {
                    token: Token::StringLiteral(String::new()),
                    tag: Tag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
                ptr!(Self {
                    token: Token::IntegerLiteral(0),
                    tag: Tag::None,
                    adjacent: operators
                        .clone()
                        .into_iter()
                        .chain(vec![not_expr, in_expr, between_expr, is_expr])
                        .collect(),
                }),
                // NOT expression
                ptr!(Self {
                    token: Token::Negation,
                    tag: Tag::None,
                    adjacent: vec![] // Cycle
                }),
            ];

            // Connector cycles
            (*connector[0]).adjacent.extend(condition.clone());
            (*connector[1]).adjacent.extend(condition.clone());
            // NOT expression (probably can't have more than one in a query)
            (*condition[4]).adjacent.extend(condition.clone());

            // WHERE condition
            let where_clause = ptr!(Self {
                token: Token::Where,
                tag: Tag::None,
                adjacent: condition,
            });

            // FROM <t> [;|where_clause]
            let from_clause = ptr!(Self {
                token: Token::From,
                tag: Tag::None,
                adjacent: vec![ptr!(Node {
                    token: Token::TableOrColumnReference(String::new()),
                    tag: Tag::Table,
                    adjacent: vec![where_clause, end],
                })],
            });

            // <c> [,|from_clause]
            let col_refs_1 = ptr!(Node {
                token: Token::TableOrColumnReference(String::new()),
                tag: Tag::Targets,
                adjacent: vec![from_clause],
            });

            // <t>.<c> [,|from_clause]
            let col_refs_2 = ptr!(Node {
                token: Token::TableAndColumnReference(String::new(), String::new()),
                tag: Tag::None,
                adjacent: vec![from_clause],
            });

            // Cycle
            let comma = ptr!(Node {
                token: Token::Comma,
                tag: Tag::None,
                adjacent: vec![col_refs_1, col_refs_2, from_clause],
            });
            (*col_refs_1).adjacent.push(comma);
            (*col_refs_2).adjacent.push(comma);

            // SELECT [*|(col_ref),*]
            SELECT_STMT = ptr!(Self {
                token: Token::Select,
                tag: Tag::None,
                adjacent: vec![
                    ptr!(Self {
                        token: Token::All,
                        adjacent: vec![from_clause],
                        tag: Tag::None,
                    }),
                    col_refs_1,
                    col_refs_2,
                ],
            });

            SELECT_STMT
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
                input: "* from tablea;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, * from tablea;",
                matches: false,
                only: false,
            },
            TestCase {
                input: "*, columna from tablea;",
                matches: false,
                only: false,
            },
            TestCase {
                input: "tablea.columna, columna from tablea;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea where columna > columnb;",
                matches: true,
                only: false,
            },
            TestCase {
                input:
                    "columna, columnb from tablea where columna > columnb and columnc = columnd;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna > columnb \
                    and columnc = columnd \
                    or columna > columnb;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where not columna > columnb;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna between 100 and 200;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna not between 100 and 200;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna in (1, 2, 3);",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna not in (1, 2, 3);",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna is null;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna is not null;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where columna > columnb \
                    and columnc in (1, 2, 3) \
                    or columnd between 100 and 200;",
                matches: true,
                only: false,
            },
            TestCase {
                input: "columna, columnb from tablea \
                    where not columna > columnb \
                    and columnc not in (1, 2, 3) \
                    or columnd not between 100 and 200;",
                matches: true,
                only: false,
            },
            // TODO: look at this later
            TestCase {
                input: "columna, columnb from tablea \
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

            unsafe {
                let mut m = false;
                'l: loop {
                    let node = &(*cur);
                    if node.adjacent.is_empty() {
                        break 'l;
                    }

                    let tkn = next_tkn!(l);

                    m = false;
                    'adj: for n in &node.adjacent {
                        if tkn.teq(&(**n).token) {
                            cur = *n;
                            m = true;
                            break 'adj;
                        }
                    }

                    if !m {
                        break 'l;
                    }
                }

                if !m && !matches {
                    continue;
                }

                if !m && matches {
                    panic!("Expected input to match");
                }

                if m && !matches {
                    panic!("Expected input to not match");
                }
            }
        }

        Ok(())
    }
}
