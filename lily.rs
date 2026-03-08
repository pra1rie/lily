use std::collections::HashMap;
use std::process;
use std::fmt;
use std::env;
use std::fs;

const CHAR_OPERATORS: &str = "(){}[];:,.+-*/%!=<>&|^~";
const OPERATORS: [&str; 8] = ["==", "!=", "<=", ">=", "&&", "||", "<<", ">>"];
const KEYWORDS: [&str; 10] = ["write", "var", "fun", "end", "nil", "if", "else", "while", "return", "break"];

#[derive(PartialEq, Clone, Debug)]
enum Token {
    Eof,
    Number(f64),
    String(String),
    Identifier(String),
    Keyword(String),
    Operator(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Eof => write!(f, "<EOF>"),
            Token::Number(n) => write!(f, "number \'{}\'", n),
            Token::String(s) => write!(f, "string \'{}\'", s),
            Token::Identifier(s) => write!(f, "identifier \'{}\'", s),
            Token::Keyword(s) => write!(f, "keyword \'{}\'", s),
            Token::Operator(s) => write!(f, "operator \'{}\'", s),
        }
    }
}

struct Lexer {
    text: String,
    pos: usize,
}

impl Lexer {
    fn new(text: String) -> Lexer {
        Self { text: text, pos: 0 }
    }

    fn peek(&mut self, amount: i32) -> Option<char> {
        self.text.chars().nth((self.pos as i32 + amount) as usize)
    }

    fn cur(&mut self) -> Option<char> {
        self.peek(0)
    }

    fn skip_space(&mut self) {
        while self.cur().is_some_and(|c| c.is_whitespace()) {
            self.pos += 1;
        }
        if self.cur().is_some_and(|c| c == '/') && self.peek(1).is_some_and(|c| c == '/') {
            while self.cur().is_some_and(|c| c != '\n') {
                self.pos += 1;
            }
            self.skip_space();
        } else if self.cur().is_some_and(|c| c == '/') && self.peek(1).is_some_and(|c| c == '*') {
            self.pos += 2;
            while self.cur().is_some() {
                if self.cur().is_some_and(|c| c == '*') && self.peek(1).is_some_and(|c| c == '/') {
                    break;
                }
                self.pos += 1;
            }
            self.pos += 2;
            self.skip_space();
        }
    }

    fn is_char_operator(&self, c: char) -> bool {
        CHAR_OPERATORS.find(c).is_some()
    }

    fn is_operator(&self, s: String) -> bool {
        if s.len() == 1 {
            self.is_char_operator(s.chars().nth(0).unwrap())
        } else {
            OPERATORS.iter().find(|op| **op == s).is_some()
        }
    }

    fn is_keyword(&self, s: String) -> bool {
        KEYWORDS.iter().find(|kwrd| **kwrd == s).is_some()
    }

    fn is_word(&self, c: char) -> bool {
        !(c.is_whitespace() || self.is_char_operator(c) || "\"\'".find(c).is_some())
    }

    fn next_token(&mut self) -> Option<Token> {
        let mut tok: String = String::new();
        self.skip_space();
        if self.cur().is_none() {
            return Some(Token::Eof);
        }
        let cur = self.cur().unwrap();
        if cur.is_numeric() {
            while self.cur().is_some_and(|c| c.is_numeric() || c == '.') {
                tok.push(self.cur().unwrap());
                self.pos += 1;
            }
            let num = tok.parse::<f64>().ok();
            match num {
                Some(n) => Some(Token::Number(n)),
                None => {
                    eprintln!("error: could not parse number '{}'", tok);
                    None
                }
            }
        } else if "\'\"".find(cur).is_some() {
            let quote = cur;
            self.pos += 1;
            while self.cur().is_some_and(|c| c != quote) {
                tok.push(self.cur().unwrap());
                self.pos += 1;
            }
            self.pos += 1;
            Some(Token::String(tok.clone()))
        } else if self.is_char_operator(cur) {
            tok.push(cur);
            self.pos += 1;
            if self.cur().is_some_and(|c| self.is_operator([cur, c].iter().collect())) {
                tok.push(self.cur().unwrap());
                self.pos += 1;
            }
            Some(Token::Operator(tok.clone()))
        } else if self.is_word(cur) {
            while self.cur().is_some_and(|c| self.is_word(c)) {
                tok.push(self.cur().unwrap());
                self.pos += 1;
            }
            if self.is_keyword(tok.clone()) {
                Some(Token::Keyword(tok.clone()))
            } else {
                Some(Token::Identifier(tok.clone()))
            }
        } else {
            eprintln!("error: unexpected '{}'", self.cur().unwrap());
            None
        }
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut toks = Vec::<Token>::new();
        while self.pos < self.text.len() {
            let tok = self.next_token();
            match tok {
                None => process::exit(1),
                Some(t) => toks.push(t.clone()),
            }
        }
        toks
    }
}

#[derive(PartialEq, Clone, Debug)]
#[allow(dead_code)]
enum UnaryOp {
    Negate,
    Plus,
    Minus,
}

// this bitchass compiler complains if i don't derive these,
// but if i add them it complains that it doesn't use them.
#[derive(PartialEq, Clone, Debug)]
#[allow(dead_code)]
enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    NotEqual,
    And,
    Or,
}

#[derive(PartialEq, Clone, Debug)]
struct Condition {
    condition: Box<Expr>,
    body: Vec<Stmt>,
}

#[derive(PartialEq, Clone, Debug)]
enum Expr {
    LitNil,
    LitString(String),
    LitNumber(f64),
    GetVar(String),
    SetVar(String, Box<Expr>),
    SetGlobal(String, Box<Expr>),
    CallFun(String, Vec<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
}

#[derive(PartialEq, Clone, Debug)]
enum Stmt {
    Write(Vec<Expr>),
    DefineFun(Function),
    If(Vec<Condition>, Vec<Stmt>),
    While(Condition),
    Return(Option<Expr>),
    Break,
    Expr(Expr),
}

#[derive(PartialEq, Clone, Debug)]
struct Function {
    name: String,
    args: Vec<String>,
    body: Vec<Stmt>,
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    ast: Vec<Stmt>,
}

impl Parser {
    fn new(toks: Vec<Token>) -> Parser {
        Self { tokens: toks, pos: 0, ast: Vec::<Stmt>::new() }
    }

    fn peek(&mut self, amount: i32) -> Option<Token> {
        let pos = (self.pos as i32 + amount) as usize;
        if pos < self.tokens.len() {
            Some(self.tokens[pos].clone())
        } else {
            None
        }
    }

    fn cur(&mut self) -> Option<Token> {
        self.peek(0)
    }

    fn parse_unary(&mut self, op: String) -> Expr {
        match op.as_str() {
            "(" => {
                let res = self.parse_expr();
                if !self.cur().is_some_and(|c| c == Token::Operator(")".to_string())) {
                    eprintln!("error: missing ')'");
                    process::exit(1);
                }
                self.pos += 1;
                res
            },
            "!" => Expr::Unary(UnaryOp::Negate, Box::new(self.parse_expr())),
            "-" => Expr::Unary(UnaryOp::Minus, Box::new(self.parse_expr())),
            "+" => Expr::Unary(UnaryOp::Plus, Box::new(self.parse_expr())),
            _ => {
                eprintln!("error: unexpected {}", op);
                process::exit(1);
            }
        }
    }

    fn parse_funcall(&mut self, id: String) -> Expr {
        let mut args = Vec::new();
        self.pos += 1;
        if self.cur() != Some(Token::Operator(")".to_string())) {
            args.push(self.parse_expr());
            while self.cur().is_some_and(|c| c == Token::Operator(",".to_string())) {
                self.pos += 1;
                args.push(self.parse_expr());
            }
        }

        if self.cur() != Some(Token::Operator(")".to_string())) {
            eprintln!("error: expected ')', got {}", self.cur().unwrap());
            process::exit(1);
        }
        self.pos += 1;
        Expr::CallFun(id, args)
    }

    fn parse_identifier(&mut self, id: String) -> Expr {
        if self.cur().is_some_and(|c| c == Token::Operator("=".to_string())) {
            self.pos += 1;
            let expr = self.parse_expr();
            Expr::SetVar(id, Box::new(expr))
        } else if self.cur().is_some_and(|c| c == Token::Operator("(".to_string())) {
            self.parse_funcall(id)
        } else {
            Expr::GetVar(id)
        }
    }

    fn parse_term(&mut self) -> Expr {
        let tok = self.cur().unwrap();
        self.pos += 1;
        match tok {
            Token::Number(n) => Expr::LitNumber(n),
            Token::String(s) => Expr::LitString(s),
            Token::Operator(o) => self.parse_unary(o),
            Token::Identifier(i) => self.parse_identifier(i),
            Token::Keyword(ref k) => {
                if *k != "nil" {
                    eprintln!("error: unexpected {}", tok);
                    process::exit(1);
                } else {
                    Expr::LitNil
                }
            },
            _ => {
                eprintln!("error: unexpected {}", tok);
                process::exit(1);
            }
        }
    }

    // XXX: merge all these binary operation parsing functions into a single recursive monster
    fn parse_muldiv(&mut self) -> Expr {
        let mut left = self.parse_term();
        loop {
            match self.cur().unwrap() {
                Token::Operator(op) => {
                    match op.as_str() {
                        "*" => {
                            self.pos += 1;
                            let right = self.parse_term();
                            left = Expr::Binary(BinaryOp::Multiply, Box::new(left), Box::new(right));
                        },
                        "/" => {
                            self.pos += 1;
                            let right = self.parse_term();
                            left = Expr::Binary(BinaryOp::Divide, Box::new(left), Box::new(right));
                        },
                        "%" => {
                            self.pos += 1;
                            let right = self.parse_term();
                            left = Expr::Binary(BinaryOp::Modulo, Box::new(left), Box::new(right));
                        },
                        _ => break,
                    }
                },
                _ => break,
            }
        }
        left
    }

    fn parse_addsub(&mut self) -> Expr {
        let mut left = self.parse_muldiv();
        loop {
            match self.cur().unwrap() {
                Token::Operator(op) => {
                    match op.as_str() {
                        "+" => {
                            self.pos += 1;
                            let right = self.parse_muldiv();
                            left = Expr::Binary(BinaryOp::Add, Box::new(left), Box::new(right));
                        },
                        "-" => {
                            self.pos += 1;
                            let right = self.parse_muldiv();
                            left = Expr::Binary(BinaryOp::Subtract, Box::new(left), Box::new(right));
                        },
                        _ => break,
                    }
                },
                _ => break,
            }
        }
        left
    }

    fn parse_comparisons(&mut self) -> Expr {
        let mut left = self.parse_addsub();
        loop {
            match self.cur().unwrap() {
                Token::Operator(op) => {
                    match op.as_str() {
                        "<" => {
                            self.pos += 1;
                            let right = self.parse_addsub();
                            left = Expr::Binary(BinaryOp::Less, Box::new(left), Box::new(right));
                        },
                        ">" => {
                            self.pos += 1;
                            let right = self.parse_addsub();
                            left = Expr::Binary(BinaryOp::Greater, Box::new(left), Box::new(right));
                        },
                        "<=" => {
                            self.pos += 1;
                            let right = self.parse_addsub();
                            left = Expr::Binary(BinaryOp::LessEqual, Box::new(left), Box::new(right));
                        },
                        ">=" => {
                            self.pos += 1;
                            let right = self.parse_addsub();
                            left = Expr::Binary(BinaryOp::GreaterEqual, Box::new(left), Box::new(right));
                        },
                        "==" => {
                            self.pos += 1;
                            let right = self.parse_addsub();
                            left = Expr::Binary(BinaryOp::Equal, Box::new(left), Box::new(right));
                        },
                        "!=" => {
                            self.pos += 1;
                            let right = self.parse_addsub();
                            left = Expr::Binary(BinaryOp::NotEqual, Box::new(left), Box::new(right));
                        },
                        _ => break,
                    }
                },
                _ => break,
            }
        }
        left
    }

    fn parse_andor(&mut self) -> Expr {
        let mut left = self.parse_comparisons();
        loop {
            match self.cur().unwrap() {
                Token::Operator(op) => {
                    match op.as_str() {
                        "&&" => {
                            self.pos += 1;
                            let right = self.parse_comparisons();
                            left = Expr::Binary(BinaryOp::And, Box::new(left), Box::new(right));
                        },
                        "||" => {
                            self.pos += 1;
                            let right = self.parse_comparisons();
                            left = Expr::Binary(BinaryOp::Or, Box::new(left), Box::new(right));
                        },
                        _ => break,
                    }
                },
                _ => break,
            }
        }
        left
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_andor()
    }

    fn get_identifier(&mut self) -> String {
        match self.cur() {
            Some(Token::Identifier(s)) => s.clone(),
            _ => {
                eprintln!("error: unexpected {}", self.cur().unwrap());
                process::exit(1);
            },
        }
    }

    fn parse_keyword_var(&mut self, is_global: bool) -> Option<Stmt> {
        let name = self.get_identifier();
        self.pos += 1;
        Some(Stmt::Expr(if self.cur().is_some_and(|c| c == Token::Operator("=".to_string())) {
            self.pos += 1;
            let expr = self.parse_expr();
            if is_global {
                let res = Expr::SetGlobal(name, Box::new(expr));
                if !self.cur().is_some_and(|tok| tok == Token::Operator(";".to_string())) {
                    eprintln!("error: missing ';'");
                    process::exit(1);
                }
                self.pos += 1;
                res
            } else {
                Expr::SetVar(name, Box::new(expr))
            }
        } else {
            if is_global {
                eprintln!("error: missing value for global constant");
                process::exit(1);
            }
            Expr::SetVar(name, Box::new(Expr::LitNil))
        }))
    }

    fn parse_body(&mut self, is_if: bool) -> Vec<Stmt> {
        let mut body = Vec::new();
        while self.cur().is_some_and(|c| c != Token::Keyword("end".to_string())) {
            if is_if && self.cur().is_some_and(|c| c == Token::Keyword("else".to_string())) {
                self.pos -= 1;
                break;
            }
            let stmt = self.parse_stmt();
            match stmt {
                Some(s) => body.push(s),
                None => break,
            }
        }
        self.pos += 1;
        body
    }

    fn parse_keyword_fun(&mut self) -> Option<Stmt> {
        let name = self.get_identifier();
        let mut args = Vec::new();
        self.pos += 1;

        if self.cur() != Some(Token::Operator("(".to_string())) {
            eprintln!("error: expected '(', got {}", self.cur().unwrap());
            return None;
        }
        self.pos += 1;

        if self.cur() != Some(Token::Operator(")".to_string())) {
            args.push(self.get_identifier());
            self.pos += 1;
            while self.cur().is_some_and(|c| c == Token::Operator(",".to_string())) {
                self.pos += 1;
                args.push(self.get_identifier());
                self.pos += 1;
            }
        }

        if self.cur() != Some(Token::Operator(")".to_string())) {
            eprintln!("error: expected ')', got {}", self.cur().unwrap());
            return None;
        }
        self.pos += 1;
        let body = self.parse_body(false);
        Some(Stmt::DefineFun(Function{ name: name.clone(), args: args, body: body }))
    }

    fn parse_keyword_if(&mut self) -> Option<Stmt> {
        let mut conditions = Vec::<Condition>::new();
        let mut else_condition = Vec::<Stmt>::new();

        let if_condition = self.parse_expr();
        let if_body = self.parse_body(true);
        conditions.push(Condition{ condition: Box::new(if_condition), body: if_body });
        while self.cur().is_some_and(|c| c == Token::Keyword("else".to_string())) {
            self.pos += 1;
            if self.cur().is_some_and(|c| c == Token::Keyword("if".to_string())) {
                self.pos += 1;
                let expr = self.parse_expr();
                let body = self.parse_body(true);
                conditions.push(Condition{ condition: Box::new(expr), body: body });
            } else {
                else_condition = self.parse_body(true);
                break;
            }
        }

        Some(Stmt::If(conditions, else_condition))
    }

    fn parse_keyword_while(&mut self) -> Option<Stmt> {
        let cond = self.parse_expr();
        let body = self.parse_body(false);
        Some(Stmt::While(Condition{ condition: Box::new(cond), body: body }))
    }

    fn parse_keyword_write(&mut self) -> Option<Stmt> {
        let mut vec = Vec::<Expr>::new();
        vec.push(self.parse_expr());
        while self.cur().is_some_and(|c| c == Token::Operator(",".to_string())) {
            self.pos += 1;
            vec.push(self.parse_expr());
        }
        Some(Stmt::Write(vec))
    }

    fn parse_keyword_break(&mut self) -> Option<Stmt> {
        Some(Stmt::Break)
    }

    fn parse_keyword_return(&mut self) -> Option<Stmt> {
        if self.cur().is_some_and(|c| c == Token::Operator(";".to_string())) {
            return Some(Stmt::Return(None));
        }
        Some(Stmt::Return(Some(self.parse_expr())))
    }

    fn parse_keyword(&mut self, keyword: String) -> Option<Stmt> {
        self.pos += 1;
        match keyword.as_str() {
            "var" => self.parse_keyword_var(false),
            "write" => self.parse_keyword_write(),
            "if" => self.parse_keyword_if(),
            "while" => self.parse_keyword_while(),
            "break" => self.parse_keyword_break(),
            "return" => self.parse_keyword_return(),
            _ => {
                eprintln!("error: unexpected keyword '{}'", keyword);
                None
            }
        }
    }

    fn parse_decl(&mut self) -> Option<Stmt> {
        if self.cur().is_some() {
            let tok = self.cur().unwrap();
            self.pos += 1;
            match tok {
                Token::Keyword(keyword) => match keyword.as_str() {
                    "fun" => self.parse_keyword_fun(),
                    "var" => self.parse_keyword_var(true),
                    _ => {
                        eprintln!("error: unexpected keyword '{}'", keyword);
                        None
                    }
                },
                _ => {
                    eprintln!("error: unexpected {}", tok);
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        let stmt = match self.cur().unwrap() {
            Token::Eof => None,
            Token::Keyword(k) => self.parse_keyword(k),
            Token::Identifier(_) => Some(Stmt::Expr(self.parse_expr())),
            s => {
                eprintln!("error: unexpected {}", s);
                None
            }
        };
        match stmt.clone() {
            None => {
                process::exit(1);
            },
            Some(Stmt::If(_, _)) => {},
            Some(Stmt::While(_)) => {},
            _ => {
                if !self.cur().is_some_and(|tok| tok == Token::Operator(";".to_string())) {
                    eprintln!("error: missing ';'");
                    process::exit(1);
                }
                self.pos += 1;
            }
        }
        stmt
    }

    fn parse(&mut self) -> Vec<Stmt> {
        while self.cur().is_some() {
            if self.cur().is_some_and(|c| c == Token::Eof) {
                break;
            }
            let decl = self.parse_decl();
            match decl {
                Some(d) => self.ast.push(d),
                None => process::exit(1),
            }
        }
        self.ast.clone()
    }
}

#[derive(PartialEq, Clone)]
enum Value {
    Nil,
    Number(f64),
    String(String),
}

impl Value {
    fn is_truthy(v: Value) -> bool {
        match v {
            Value::Nil => false,
            Value::Number(n) => n != (0 as f64),
            Value::String(s) => s.len() != 0,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil       => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

enum Return {
    NoReturn,
    Break,
    Return(Value),
}

#[derive(Clone)]
struct Global {
    funs: HashMap<String, Function>,
    vars: HashMap<String, Value>,
}

struct Interpreter {
    code: Vec<Stmt>,
    pos: usize,
    global: Box<Global>,
    vars: HashMap<String, Value>,
}

impl Interpreter {
    fn new(global: Box<Global>, code: Vec<Stmt>) -> Interpreter {
        Self {
            code: code,
            pos: 0,
            global: global,
            vars: HashMap::new(),
        }
    }

    fn cur(&mut self) -> Option<Stmt> {
        if self.pos < self.code.len() {
            Some(self.code[self.pos].clone())
        } else {
            None
        }
    }

    fn escape_string(&self, string: String) -> String {
        string.replace("\\n", "\n")
              .replace("\\r", "\r")
              .replace("\\t", "\t")
              .replace("\\0", "\0")
    }

    fn get_var(&mut self, id: String) -> Value {
        if !self.vars.contains_key(&id) {
            if self.global.vars.contains_key(&id) {
                return self.global.vars.get(&id).unwrap().clone();
            }
            eprintln!("error: undefined variable '{}'", id);
            process::exit(1);
        }
        self.vars.get(&id).unwrap().clone()
    }

    fn set_var(&mut self, id: String, val: Value) -> Value {
        self.vars.insert(id, val.clone());
        val
    }

    fn call_fun(&mut self, id: String, args: Vec<Value>) -> Option<Value> {
        if !self.global.funs.contains_key(&id) {
            eprintln!("error: undefined function '{}'", id);
            process::exit(1);
        }
        let fun = self.global.funs.get(&id).unwrap();
        let mut int = Interpreter::new(self.global.clone(), fun.body.clone());
        if args.len() != fun.args.len() {
            let arg_str = if fun.args.len() == 1 { "argument" } else { "arguments" };
            eprintln!("error: function '{}' expects {} {}, got {}",
                id, fun.args.len(), arg_str, args.len());
            process::exit(1);
        }
        for i in 0..args.len() {
            int.set_var(fun.args[i].clone(), args[i].clone());
        }
        int.interpret()
    }

    fn get_num(&mut self, val: Value) -> f64 {
        match val {
            Value::Number(n) => n,
            other => {
                eprintln!("error: expected number, got '{}'", other);
                process::exit(1);
            }
        }
    }

    fn get_nums(&mut self, a: Value, b: Value) -> (f64, f64) {
        let num_a = self.get_num(a);
        let num_b = self.get_num(b);
        (num_a, num_b)
    }

    fn exec_binary(&mut self, op: BinaryOp, a: Value, b: Value) -> Value {
        match op {
            BinaryOp::Add => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number(num_a + num_b)
            },
            BinaryOp::Subtract => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number(num_a - num_b)
            },
            BinaryOp::Multiply => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number(num_a * num_b)
            },
            BinaryOp::Divide => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number(num_a / num_b)
            },
            BinaryOp::Modulo => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number(num_a % num_b)
            },
            BinaryOp::Less => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number((num_a < num_b) as i64 as f64)
            },
            BinaryOp::Greater => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number((num_a > num_b) as i64 as f64)
            },
            BinaryOp::LessEqual => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number((num_a <= num_b) as i64 as f64)
            },
            BinaryOp::GreaterEqual => {
                let (num_a, num_b) = self.get_nums(a, b);
                Value::Number((num_a <= num_b) as i64 as f64)
            },
            BinaryOp::Equal => {
                Value::Number((a == b) as i64 as f64)
            },
            BinaryOp::NotEqual => {
                Value::Number((a != b) as i64 as f64)
            },
            BinaryOp::And => {
                Value::Number((Value::is_truthy(a) && Value::is_truthy(b)) as i64 as f64)
            },
            BinaryOp::Or => {
                Value::Number((Value::is_truthy(a) || Value::is_truthy(b)) as i64 as f64)
            },
        }
    }

    fn exec_unary(&mut self, op: UnaryOp, val: Value) -> Value {
        match op {
            UnaryOp::Negate => {
                Value::Number(Value::is_truthy(val) as i64 as f64)
            },
            UnaryOp::Minus => {
                Value::Number(-self.get_num(val))
            },
            UnaryOp::Plus => {
                Value::Number(self.get_num(val).abs())
            },
        }
    }

    fn exec_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::LitNil          => Value::Nil,
            Expr::LitNumber(n)    => Value::Number(n),
            Expr::LitString(s)    => Value::String(self.escape_string(s)),
            Expr::GetVar(n)       => self.get_var(n),
            Expr::SetVar(n, e)    => {
                let expr = self.exec_expr(*e);
                self.set_var(n, expr)
            },
            Expr::SetGlobal(n, e) => {
                let expr = self.exec_expr(*e);
                self.global.vars.insert(n, expr);
                Value::Nil
            },
            Expr::CallFun(n, a) => {
                let mut args = Vec::new();
                for arg in a {
                    args.push(self.exec_expr(arg));
                }
                match self.call_fun(n, args) {
                    None => Value::Nil,
                    Some(v) => v
                }
            },
            Expr::Binary(o, a, b) => {
                let val_a = self.exec_expr(*a);
                let val_b = self.exec_expr(*b);
                self.exec_binary(o, val_a, val_b)
            },
            Expr::Unary(o, v) => {
                let val = self.exec_expr(*v);
                self.exec_unary(o, val)
            },
        }
    }

    fn exec_stmt(&mut self, stmt: Stmt) -> Return {
        match stmt {
            Stmt::DefineFun(fun) => {
                self.global.funs.insert(fun.name.clone(), fun);
                Return::NoReturn
            },
            Stmt::Write(body) => {
                for expr in body.iter() {
                    print!("{}", self.exec_expr(expr.clone()));
                }
                Return::NoReturn
            },
            Stmt::If(c, e) => {
                for if_cond in c {
                    let res = self.exec_expr(*if_cond.condition);
                    if Value::is_truthy(res) {
                        for stmt in if_cond.body {
                            let s = self.exec_stmt(stmt);
                            match s {
                                Return::NoReturn => {},
                                _ => return s,
                            }
                        }
                        return Return::NoReturn;
                    }
                }
                for stmt in e {
                    let s = self.exec_stmt(stmt);
                    match s {
                        Return::Return(_) => return s,
                        _ => {},
                    }
                }
                Return::NoReturn
            },
            Stmt::While(c) => {
                'cond: loop {
                    let res = self.exec_expr(*c.clone().condition);
                    if !Value::is_truthy(res) { break; }
                    for stmt in &c.body {
                        let s = self.exec_stmt(stmt.clone());
                        match s {
                            Return::Break => break 'cond,
                            Return::Return(_) => return s,
                            _ => {},
                        }
                    }
                }
                Return::NoReturn
            },
            Stmt::Break => Return::Break,
            Stmt::Return(val) => {
                match val {
                    None => Return::NoReturn,
                    Some(e) => Return::Return(self.exec_expr(e)),
                }
            },
            Stmt::Expr(expr) => {
                _ = self.exec_expr(expr);
                Return::NoReturn
            },
        }
    }

    fn interpret(&mut self) -> Option<Value> {
        while self.cur().is_some() {
            match self.cur() {
                None => {},
                Some(s) => {
                    let stmt = self.exec_stmt(s);
                    match stmt {
                        Return::Return(v) => return Some(v),
                        _ => {},
                    }
                }
            }
            self.pos += 1;
        }
        None
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} <file>", args[0]);
        process::exit(0);
    }

    let path = &args[1];
    let text_res = fs::read_to_string(path);

    if text_res.is_err() {
        eprintln!("error: could not open '{}'", path);
        process::exit(1);
    }

    let text = text_res.ok().unwrap();
    let mut lexer = Lexer::new(text);
    let tokens = lexer.tokenize();
    // for token in tokens.clone().into_iter() {
    //     println!("{}", token);
    // }
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    // for branch in ast.clone().into_iter() {
    //     println!("{:?}", branch);
    // }
    let global = Global { funs: HashMap::new(), vars: HashMap::new() };
    let mut interpreter = Interpreter::new(Box::new(global), ast);
    interpreter.interpret();
    if (*interpreter.global).funs.contains_key("main") {
        interpreter.call_fun("main".to_string(), Vec::new());
    }
}
