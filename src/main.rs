#![feature(box_patterns)]

use std::{
    collections::{HashMap, HashSet},
    fmt,
    ops::{Add, Deref, DerefMut},
};

use color_eyre::{
    eyre::{eyre, ContextCompat},
    Result,
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::{eof, map, map_res, opt, value},
    error::Error,
    multi::separated_list1,
    sequence::{delimited, pair, separated_pair, terminated},
    Finish, IResult,
};
use rustyline::error::ReadlineError;

#[derive(Clone)]
enum Command {
    Assign(Name, Expression),
    Context,
    Evaluate(Expression),
    Quit,
}

struct Context(HashMap<Name, Expression>);

impl Context {
    fn environment(&self, xs: HashSet<Name>) -> Result<Environment> {
        let mut env = Environment::new();

        for x in xs {
            let e = self.get(&x).wrap_err_with(|| eyre!("{} not found", x))?;

            env.insert(
                x,
                e.evaluate(
                    &self.environment(e.variables().map(|x| x.name.clone()).collect())?,
                    true,
                ),
            );
        }

        Ok(env)
    }

    fn new() -> Self {
        Self(HashMap::new())
    }
}

impl Deref for Context {
    type Target = HashMap<Name, Expression>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Context {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

type Environment = HashMap<Name, Value>;

#[derive(Clone)]
enum Expression {
    Addition(Box<Expression>, Box<Expression>),
    Literal(Literal),
    Variable(Variable),
}

impl Expression {
    fn contains(&self, x: &Variable, context: &Context) -> bool {
        match self {
            Self::Addition(box lhs, box rhs) => {
                lhs.contains(x, context) || rhs.contains(x, context)
            }
            Self::Literal(_) => false,
            Self::Variable(y) => {
                y == x
                    || y.mutable
                        && if let Some(e) = context.get(&y.name) {
                            e.contains(x, context)
                        } else {
                            false
                        }
            }
        }
    }

    fn evaluate(&self, environment: &Environment, mutable: bool) -> Value {
        match self {
            Self::Addition(box lhs, box rhs) => {
                lhs.evaluate(environment, mutable) + rhs.evaluate(environment, mutable)
            }
            &Self::Literal(literal) => Value::Literal(literal),
            Self::Variable(x) => if mutable || !x.mutable {
                environment.get(&x.name).cloned()
            } else {
                None
            }
            .unwrap_or_else(|| Value::Neutral(Box::new(Neutral::Variable(x.to_owned())))),
        }
    }

    fn variables(&self) -> Variables {
        Variables::new(self)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Addition(lhs, rhs) => write!(f, "{} + {}", lhs, rhs),
            Self::Literal(n) => n.fmt(f),
            Self::Variable(x) => x.fmt(f),
        }
    }
}

impl Add for Expression {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Addition(Box::new(self), Box::new(rhs))
    }
}

type Literal = u64;

type Name = String;

#[derive(Clone, Debug)]
enum Neutral {
    Addition(Box<Neutral>, Value),
    Variable(Variable),
}

impl Neutral {
    fn quote(self) -> Expression {
        match self {
            Self::Addition(n, v) => Expression::Addition(Box::new(n.quote()), Box::new(v.quote())),
            Self::Variable(x) => Expression::Variable(x),
        }
    }
}

impl Add<Value> for Neutral {
    type Output = Self;

    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Self::Addition(box n, v_1), v_2) => n + (v_1 + v_2),
            (n, v) => Self::Addition(Box::new(n), v),
        }
    }
}

#[derive(Clone, Debug)]
enum Value {
    Literal(Literal),
    Neutral(Box<Neutral>),
}

impl Value {
    fn quote(self) -> Expression {
        match self {
            Self::Literal(literal) => Expression::Literal(literal),
            Self::Neutral(neutral) => neutral.quote(),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Literal(lhs), Self::Literal(rhs)) => Self::Literal(lhs + rhs),
            (Self::Neutral(box neutral), value) | (value, Self::Neutral(box neutral)) => {
                Self::Neutral(Box::new(neutral + value))
            }
        }
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
struct Variable {
    mutable: bool,
    name: Name,
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.mutable {
            '&'.fmt(f)?;
        }

        self.name.fmt(f)
    }
}

struct Variables<'a> {
    expressions: Vec<&'a Expression>,
}

impl<'a> Variables<'a> {
    fn new(expression: &'a Expression) -> Self {
        Self {
            expressions: vec![expression],
        }
    }
}

impl<'a> Iterator for Variables<'a> {
    type Item = &'a Variable;

    fn next(&mut self) -> Option<Self::Item> {
        let e = self.expressions.pop()?;

        match e {
            Expression::Addition(box lhs, box rhs) => {
                self.expressions.push(rhs);
                self.expressions.push(lhs);
                self.next()
            }
            Expression::Literal(_) => None,
            Expression::Variable(x) => Some(x),
        }
    }
}

fn parse_name(input: &str) -> IResult<&str, Name> {
    map(alpha1, str::to_owned)(input)
}

fn parse_variable(input: &str) -> IResult<&str, Variable> {
    map(
        pair(map(opt(char('&')), |o| o.is_some()), parse_name),
        |(mutable, name)| Variable { mutable, name },
    )(input)
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    let atom = alt((
        parens(ws(parse_expression)),
        map(map_res(digit1, |s: &str| s.parse()), |n| {
            Expression::Literal(n)
        }),
        map(parse_variable, Expression::Variable),
    ));

    map(separated_list1(ws(char('+')), atom), |atoms| {
        atoms.into_iter().reduce(Expression::add).unwrap()
    })(input)
}

fn parse_command(input: &str) -> IResult<&str, Command> {
    alt((
        value(Command::Context, tag("context")),
        value(Command::Quit, tag("quit")),
        map(
            separated_pair(parse_name, ws(char('=')), parse_expression),
            |(x, e)| Command::Assign(x, e),
        ),
        map(parse_expression, Command::Evaluate),
    ))(input)
}

fn parens<'a, O, F: 'a>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(char('('), inner, char(')'))
}

fn ws<'a, O, F: 'a>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

fn main() -> Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut context = Context::new();

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                let result: Result<_> = (|| {
                    let (_, cmd) = terminated(ws(parse_command), eof)(&line)
                        .finish()
                        .map_err(|e| Error::new(e.input.to_owned(), e.code))?;

                    match cmd {
                        Command::Assign(x, e) => {
                            if e.contains(
                                &Variable {
                                    mutable: true,
                                    name: x.to_owned(),
                                },
                                &context,
                            ) {
                                return Err(eyre!("cannot self-reference {}", x));
                            }

                            let xs = e
                                .variables()
                                .filter(|x| !x.mutable)
                                .map(|x| x.name.clone())
                                .collect();

                            let env = context.environment(xs)?;
                            let e = e.evaluate(&env, false).quote();
                            context.insert(x, e);
                            Ok(true)
                        }
                        Command::Context => {
                            for (x, e) in context.iter() {
                                println!("{} = {}", x, e);
                            }

                            Ok(true)
                        }
                        Command::Evaluate(e) => {
                            let xs = e.variables().map(|x| x.name.clone()).collect();
                            let env = context.environment(xs)?;
                            let e = e.evaluate(&env, true).quote();
                            println!("{}", e);
                            Ok(true)
                        }
                        Command::Quit => {
                            println!("Bye bye!");
                            Ok(false)
                        }
                    }
                })();

                rl.add_history_entry(line)?;

                match result {
                    Ok(true) => continue,
                    Ok(false) => break,
                    Err(e) => eprintln!("{e}"),
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                break;
            }
            Err(e) => eprintln!("Error: {e}"),
        }
    }

    Ok(())
}
