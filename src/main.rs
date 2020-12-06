mod grammar {
    use std::collections::HashMap;

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

    /*pub struct Rule {
        pub symbol: TerminalSymbolType,
        pub expression: Box<Expression>,
    }*/

    pub struct Grammar {
        pub starting_symbol: TerminalSymbolType,
        pub rules: HashMap<TerminalSymbolType, Box<Expression>>,
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
    pub enum ParsingResult {
        Success(i64), // i64: number of bytes consumed
        Failure,
    }

    pub mod naive {
        use super::ParsingResult;
        use super::super::grammar::{Grammar,Expression};

        // TODO: generalize input so that it does not need to be a string (it could be an many-times-iterator of TerminalSymbolType, perhaps)
        //pub fn parse(grammar: &Grammar, current_symbol: &NonterminalSymbolType, input: &str) -> ParsingResult {
        pub fn parse(grammar: &Grammar, expression: &Expression, input: &str) -> ParsingResult {
            match expression {
                // Atomic
                Expression::EmptyString => ParsingResult::Success(0),
                Expression::TerminalSymbol(terminal) => ParsingResult::Failure, // TODO: implement
                Expression::NonterminalSymbol(next_symbol) => {
                    if let Some(next_expression) = grammar.rules.get(next_symbol) {
                        return parse(grammar, &next_expression, input);
                    } else {
                        return ParsingResult::Failure;
                    }
                }

                // Complex
                Expression::Sequence(e1, e2) => {
                    match parse(grammar, &e1, input) {
                        ParsingResult::Success(consumed_chars) => {
                            return ParsingResult::Failure; // TODO: fix
                        }
                        ParsingResult::Failure => ParsingResult::Failure,
                    }
                },
                _ => ParsingResult::Failure,
            }
        }
    }

    pub mod packrat {

    }
}

mod examples {
    mod grammars {
        use std::collections::HashMap;
        use super::super::grammar::{Grammar};
        use super::super::helpers::expression_builders::*;

        // Source: https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples
        pub fn arithmetic() -> Grammar {
            let mut grammar = Grammar {
                starting_symbol: String::from("Expr"),
                rules: HashMap::new(),
            };
            grammar.rules.insert(String::from("Expr"),
                nonterminal_symbol("Sum"),
            );
            grammar.rules.insert(String::from("Sum"),
                sequence(
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
            );
            grammar.rules.insert(String::from("Product"),
                sequence(
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
            );
            grammar.rules.insert(String::from("Power"),
                sequence(
                    nonterminal_symbol("Value"),
                    optional(
                        sequence(
                            terminal_symbol("^"),
                            nonterminal_symbol("Power")
                        )
                    )
                )
            );
            grammar.rules.insert(String::from("Value"),
                ordered_choice(
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
            );
            return grammar;
        }
    }
}

fn main() {
    println!("Hello, world!");
}
