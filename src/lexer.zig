const std = @import("std");

pub const Lexer = struct {
    const Self = @This();
    pub const Error = error{
        UnterminatedString,
        UnexpectedChar,
    };

    const TokenMap = std.StringHashMap(Token);

    keywords: TokenMap,

    curr: usize,
    start: usize,
    line: usize,
    str: []const u8,

    pub fn new(str: []const u8, alloc: std.mem.Allocator) !Self {
        var keywords = TokenMap.init(alloc);

        try keywords.put("and", .And);
        try keywords.put("class", .Class);
        try keywords.put("else", .Else);
        try keywords.put("false", .False);
        try keywords.put("for", .For);
        try keywords.put("fn", .Fn);
        try keywords.put("if", .If);
        try keywords.put("None", .None);
        try keywords.put("or", .Or);
        try keywords.put("return", .Return);
        try keywords.put("super", .Super);
        try keywords.put("self", .Self);
        try keywords.put("true", .True);
        try keywords.put("let", .Let);
        try keywords.put("while", .While);
        try keywords.put("break", .Break);
        try keywords.put("continue", .Continue);

        try keywords.put("print", .Print);

        return .{ .start = 0, .curr = 0, .line = 1, .str = str, .keywords = keywords };
    }

    pub fn code(self: *Self, str: []const u8) void {
        self.str = str;
        self.curr = 0;
        self.start = 0;
    }

    pub fn deinit(self: *Self) void {
        self.keywords.deinit();
    }

    pub fn next(self: *Self) !?TokenInfo {
        if (self.str.len < self.curr) {
            return null;
        } else if (self.str.len == self.curr) {
            self.curr += 1;
            return .{ .tok = .Eof, .line = self.line };
        }

        const c = self.str[self.curr];
        self.curr += 1;

        var t: TokenInfo = undefined;
        t.line = self.line;

        switch (c) {
            '(' => t.tok = .LeftParen,
            ')' => t.tok = .RightParen,
            '{' => t.tok = .LeftBrace,
            '}' => t.tok = .RightBrace,
            ',' => t.tok = .Comma,
            '.' => t.tok = .Dot,
            '-' => t.tok = .Dash,
            '+' => t.tok = .Plus,
            ';' => t.tok = .Semicolon,
            '*' => t.tok = .Star,
            '!' => t.tok = if (self.match("=")) .BangEqual else .Bang,
            '=' => t.tok = if (self.match("=")) .DoubleEqual else .Equal,
            '<' => t.tok = if (self.match("=")) .Lte else .Lt,
            '>' => t.tok = if (self.match("=")) .Lte else .Gt,
            '/' => {
                if (self.match("/")) {
                    while (self.str.len > self.curr and self.str[self.curr] != '\n') {
                        self.curr += 1;
                    }
                    return self.next();
                } else {
                    t.tok = .Slash;
                }
            },
            ' ', '\r', '\t' => return self.next(),
            '\n' => {
                self.line += 1;
                return self.next();
            },
            '"' => {
                var len: usize = 0;
                var end = false;
                while (self.str.len > self.curr + len) {
                    if (self.str[self.curr + len] == '"') {
                        end = true;
                        break;
                    } else if (self.str[self.curr + len] == '\n') {
                        self.line += 1;
                    }
                    len += 1;
                }
                if (!end) {
                    return error.UnterminatedString;
                }

                // TODO: unescaped string
                t.tok = .{ .String = self.str[self.curr .. self.curr + len] };
                self.curr += len + 1;
            },
            '0'...'9' => {
                var len: usize = 0;
                while (self.str.len > self.curr + len) {
                    switch (self.str[self.curr + len]) {
                        '0'...'9' => len += 1,
                        else => break,
                    }
                }
                if (self.str.len > self.curr + len and self.str[self.curr + len] == '.') {
                    var decimal = false;
                    while (self.str.len > self.curr + len + 1) {
                        switch (self.str[self.curr + len + 1]) {
                            '0'...'9' => {
                                decimal = true;
                                len += 1;
                            },
                            else => break,
                        }
                    }
                    if (decimal) {
                        len += 1;
                    }
                }
                t.tok = .{ .Number = self.str[self.curr - 1 .. self.curr + len] };
                self.curr += len;
            },
            'a'...'z', 'A'...'Z', '_' => {
                self.curr -= 1;
                var len: usize = 0;
                while (self.str.len > self.curr + len) {
                    switch (self.str[self.curr + len]) {
                        'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                            len += 1;
                        },
                        else => break,
                    }
                }
                const ident = self.str[self.curr .. self.curr + len];
                t.tok = if (self.keywords.get(ident)) |tok| tok else .{ .Identifier = ident };
                self.curr += len;
            },
            else => return error.UnexpectedChar,
        }

        return t;
    }

    fn match(self: *Self, str: []const u8) bool {
        if (self.curr + str.len > self.str.len) {
            return false;
        }
        if (std.mem.eql(u8, self.str[self.curr .. self.curr + str.len], str)) {
            self.curr += str.len;
            return true;
        } else {
            return false;
        }
    }
};

pub const Token = union(enum) {
    pub const Type = std.meta.Tag(Token);

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Dash,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    DoubleEqual,
    Gt,
    Gte,
    Lt,
    Lte,

    Identifier: []const u8,
    String: []const u8,
    Number: []const u8,

    And,
    Class,
    Else,
    False,
    Fn,
    For,
    If,
    None,
    Or,
    Return,
    Super,
    Self,
    True,
    Let,
    While,
    Break,
    Continue,

    Print,

    Eof,
};

pub const TokenInfo = struct {
    const Self = @This();
    tok: Token,
    line: u64,

    fn match(self: *Self, others: []const Token.Type) bool {
        for (others) |typ| {
            if (@as(Token.Type, self.tok) == typ) {
                return true;
            }
        }
        return false;
    }
};
