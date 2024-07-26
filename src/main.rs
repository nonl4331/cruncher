use miette::{LabeledSpan, Result, SourceSpan};
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    OpenParen,
    ClosedParen,
    Literal(f64),
}
struct Prefix(fn(f64) -> f64, u32);
struct Infix(fn(f64, f64) -> f64, u32, u32);
struct Postfix(fn(f64) -> f64, u32);

impl Token {
    pub fn prefix(&self) -> Option<Prefix> {
        Some(match self {
            Self::Sub => Prefix(|a| -a, 10),
            Self::OpenParen => Prefix(|a| a, 0),
            _ => return None,
        })
    }
    pub fn infix(&self) -> Option<Infix> {
        Some(match self {
            Self::Add => Infix(|a, b| a + b, 1, 2),
            Self::Sub => Infix(|a, b| a - b, 1, 2),
            Self::Mul => Infix(|a, b| a * b, 3, 4),
            Self::Div => Infix(|a, b| a / b, 3, 4),
            Self::Exp => Infix(|a, b| a.powf(b), 5, 6),
            _ => return None,
        })
    }
    pub fn postfix(&self) -> Option<Postfix> {
        None
    }
}

#[derive(Debug)]
struct Lexer {
    src: String,
    tokens: Vec<(Token, SourceSpan)>,
}

impl Lexer {
    pub fn new(input: &str) -> Result<Self> {
        let mut chars = input.chars().peekable();
        let src = input.to_owned();
        let mut tokens = Vec::new();

        let mut number_buffer = String::new();
        let mut char_pos = 0;
        while let Some(c) = chars.next() {
            if c.is_ascii_digit()
                || (!number_buffer.is_empty()
                    && c == '.'
                    && !number_buffer.contains('.')
                    && chars.peek().is_some_and(|v| v.is_ascii_digit()))
            {
                number_buffer.push(c);
                char_pos += 1;
                continue;
            }

            if !number_buffer.is_empty() {
                tokens.push((
                    Token::Literal(number_buffer.parse().unwrap()),
                    SourceSpan::new(char_pos.into(), 1),
                ));
                number_buffer.clear();
            }

            match c {
                '(' => tokens.push((Token::OpenParen, SourceSpan::new(char_pos.into(), 1))),
                ')' => tokens.push((Token::ClosedParen, SourceSpan::new(char_pos.into(), 1))),
                '+' => tokens.push((Token::Add, SourceSpan::new(char_pos.into(), 1))),
                '-' => tokens.push((Token::Sub, SourceSpan::new(char_pos.into(), 1))),
                '*' => tokens.push((Token::Mul, SourceSpan::new(char_pos.into(), 1))),
                '/' => tokens.push((Token::Div, SourceSpan::new(char_pos.into(), 1))),
                '^' => tokens.push((Token::Exp, SourceSpan::new(char_pos.into(), 1))),
                'π' => tokens.push((
                    Token::Literal(std::f64::consts::PI),
                    SourceSpan::new(char_pos.into(), 1),
                )),
                'τ' => tokens.push((
                    Token::Literal(std::f64::consts::TAU),
                    SourceSpan::new(char_pos.into(), 1),
                )),
                'e' => tokens.push((
                    Token::Literal(std::f64::consts::E),
                    SourceSpan::new(char_pos.into(), 1),
                )),
                ' ' | '\t' => {}
                _ => {
                    return Err(miette::miette!(
                        code = "Lexer Error",
                        labels = vec![LabeledSpan::at_offset(char_pos, "here")],
                        "Unknown character \"{c}\""
                    )
                    .with_source_code(input.to_owned()));
                }
            };
            char_pos += c.len_utf8();
        }
        if !number_buffer.is_empty() {
            tokens.push((
                Token::Literal(number_buffer.parse().unwrap()),
                SourceSpan::new(char_pos.into(), 1),
            ));
        }

        tokens.reverse();
        Ok(Self { tokens, src })
    }
    pub fn next(&mut self) -> Option<(Token, SourceSpan)> {
        self.tokens.pop()
    }
    pub fn peek(&mut self) -> Option<(Token, SourceSpan)> {
        self.tokens.last().cloned()
    }
}

fn evaluate_expr(lexer: &mut Lexer, bp: u32) -> Result<f64> {
    let Some(tok) = lexer.next() else {
        unreachable!();
    };

    let mut lhs;

    if let Token::Literal(v) = tok.0 {
        lhs = v;
    } else if let Some(Prefix(op, r_bp)) = tok.0.prefix() {
        lhs = op(evaluate_expr(lexer, r_bp)?);
        if tok.0 == Token::OpenParen {
            assert_eq!(lexer.next().map(|v| v.0), Some(Token::ClosedParen));
        };
    } else {
        return Err(miette::miette!(
            code = "Evaluation Error",
            labels = vec![LabeledSpan::at(tok.1, "here")],
            "Missing lhs"
        )
        .with_source_code(lexer.src.to_owned()));
    }

    loop {
        let Some(tok) = lexer.peek() else {
            break;
        };

        if let Some(Infix(op, l_bp, r_bp)) = tok.0.infix() {
            if l_bp < bp {
                break;
            }
            lexer.next();
            lhs = op(lhs, evaluate_expr(lexer, r_bp)?);
            continue;
        };

        if let Some(Postfix(op, l_bp)) = tok.0.postfix() {
            if l_bp < bp {
                break;
            }
            lexer.next();
            lhs = op(lhs);
            continue;
        }
        break;
    }
    Ok(lhs)
}

fn evaluate(s: &str) -> Result<Option<f64>> {
    let mut lexer = Lexer::new(s.trim())?;
    if lexer.peek().is_none() {
        return Ok(None);
    }

    evaluate_expr(&mut lexer, 0).map(|v| Some(v))
}

fn main() -> miette::Result<()> {
    let stdin = std::io::stdin();
    let mut buf = String::new();
    loop {
        print!(">");
        use std::io::Write;
        let _ = std::io::stdout().flush();
        stdin.read_line(&mut buf).unwrap();
        match evaluate(&buf) {
            Ok(None) => {}
            Ok(Some(v)) => println!("{v}"),
            Err(e) => println!("{e:?}"),
        }
        buf.clear();
    }
}
