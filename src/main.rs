mod grammar {
    // TODO: generalize the symbol types
    pub type TerminalSymbolType = String;
    pub type NonterminalSymbolType = String;

    pub enum Expression {
        // Atomic
        EmptyString,
        TerminalSymbol(TerminalSymbolType),
        NonterminalSymbol(NonterminalSymbolType),

        // Complex
        Sequence(Box<Expression>, Box<Expression>),
        OrderedChoice(Box<Expression>, Box<Expression>),
        ZeroOrMore(Box<Expression>),
        OneOrMore(Box<Expression>),
        Optional(Box<Expression>),
        AndPredicate(Box<Expression>),
        NotPredicate(Box<Expression>),
    }

    pub struct Rule {
        pub symbol: TerminalSymbolType,
        pub expression: Box<Expression>,
    }

    pub struct Grammar {
        pub starting_symbol: TerminalSymbolType,
        pub rules: Vec<Rule>,
    }

    fn validate(_grammar: Grammar) -> bool {
        // TODO: check if the version has indirect left recursion (probably just a toposort)
        return true
    }
}

pub mod helpers {
    pub mod expression_builders {
        use super::super::grammar::*;

        pub fn empty_string() -> Box<Expression> {
            return Box::new(Expression::EmptyString);
        }

        pub fn terminal_symbol(symbol: &str) -> Box<Expression> {
            return Box::new(Expression::TerminalSymbol(String::from(symbol)))
        }

        pub fn nonterminal_symbol(symbol: &str) -> Box<Expression> {
            return Box::new(Expression::NonterminalSymbol(String::from(symbol)))
        }

        pub fn sequence(e1: Box<Expression>, e2: Box<Expression>) -> Box<Expression> {
            return Box::new(Expression::Sequence(e1, e2));
        }

        pub fn multiple_sequence(head: Box<Expression>, tail: Vec<Box<Expression>>) -> Box<Expression> {
            let mut ret: Box<Expression> = head;
            for x in tail.into_iter() {
                ret = Box::new(Expression::Sequence(ret, x));
            }
            return ret;
        }

        pub fn ordered_choice(e1: Box<Expression>, e2: Box<Expression>) -> Box<Expression> {
            return Box::new(Expression::OrderedChoice(e1, e2));
        }

        pub fn multiple_ordered_choice(head: Box<Expression>, tail: Vec<Box<Expression>>) -> Box<Expression> {
            let mut ret: Box<Expression> = head;
            for x in tail.into_iter() {
                ret = Box::new(Expression::OrderedChoice(ret, x));
            }
            return ret;
        }

        pub fn zero_or_more(e: Box<Expression>) -> Box<Expression> {
            return Box::new(Expression::ZeroOrMore(e));
        }

        pub fn one_or_more(e: Box<Expression>) -> Box<Expression> {
            return Box::new(Expression::OneOrMore(e));
        }

        pub fn optional(e: Box<Expression>) -> Box<Expression> {
            return Box::new(Expression::Optional(e));
        }

        pub fn and_predicate(e: Box<Expression>) -> Box<Expression> {
            return Box::new(Expression::AndPredicate(e));
        }

        pub fn not_predicate(e: Box<Expression>) -> Box<Expression> {
            return Box::new(Expression::NotPredicate(e));
        }
    }
}

pub mod parsers {
    pub mod naive {

    }

    pub mod packrat {

    }
}

mod examples {
    mod grammars {
        use super::super::grammar::*;
        use super::super::helpers::expression_builders::*;

        // Source: https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples
        pub fn arithmetic() -> Grammar {
            Grammar {
                starting_symbol: String::from("Expr"),
                rules: vec! [
                    Rule {
                        symbol: String::from("Expr"),
                        expression: nonterminal_symbol("Sum"),
                    },
                    Rule {
                        symbol: String::from("Sum"),
                        expression: sequence(
                            nonterminal_symbol("Product"),
                            zero_or_more(
                                sequence(
                                    ordered_choice(
                                        terminal_symbol("+"),
                                        terminal_symbol("-"),
                                    ),
                                    nonterminal_symbol("Product")
                                )
                            )
                        )
                    },
                    Rule {
                        symbol: String::from("Product"),
                        expression: sequence(
                            nonterminal_symbol("Power"),
                            zero_or_more(
                                sequence(
                                    ordered_choice(
                                        terminal_symbol("*"),
                                        terminal_symbol("/"),
                                    ),
                                    nonterminal_symbol("Power")
                                )
                            )
                        )
                    },
                    Rule {
                        symbol: String::from("Power"),
                        expression: sequence(
                            nonterminal_symbol("Value"),
                            optional(
                                sequence(
                                    terminal_symbol("^"),
                                    nonterminal_symbol("Power")
                                )
                            )
                        )
                    },
                    Rule {
                        symbol: String::from("Value"),
                        expression: ordered_choice(
                            one_or_more(
                                multiple_ordered_choice(
                                    nonterminal_symbol("0"),
                                    (1..9).into_iter().map(|i| terminal_symbol(&i.to_string())).collect()
                                ),
                            ),
                            multiple_sequence(
                                terminal_symbol("("),
                                vec! [ nonterminal_symbol("Expr"), terminal_symbol(")"), ]
                            )
                        )
                    },
                ]
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
