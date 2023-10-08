use std::{collections::BTreeSet, ops::Deref};

use crate::parser::{
    ast::{Block, Expr, ExprKind, File, Ident, Item},
    span::Spanned,
};
use stack_graphs::{
    arena::Handle,
    graph::{Node, NodeID, StackGraph, Symbol},
    partial::PartialPaths,
    serde::NoFilter,
    stitching::{Database, ForwardPartialPathStitcher, GraphEdges},
    NoCancellation,
};

pub struct Graph {
    graph: StackGraph,
    file: Handle<stack_graphs::graph::File>,
}

impl<'s, 'a> Graph {
    pub fn new(file_name: &str) -> Self {
        let mut graph = StackGraph::new();
        let file = graph.get_or_create_file(file_name);
        Self { graph, file }
    }

    fn new_node_id(&mut self) -> NodeID {
        self.graph.new_node_id(self.file)
    }

    fn new_scope(&mut self, is_exported: bool) -> Handle<Node> {
        let node_id = self.new_node_id();
        // Unwrap is fine because we just made the node id
        self.graph.add_scope_node(node_id, is_exported).unwrap()
    }

    fn new_definition(&mut self, symbol: Handle<Symbol>) -> Handle<Node> {
        let node_id = self.new_node_id();
        // Unwrap is fine because we just made the node id
        self.graph
            .add_pop_symbol_node(node_id, symbol, true)
            .unwrap()
    }

    fn new_reference(&mut self, symbol: Handle<Symbol>) -> Handle<Node> {
        let node_id = self.new_node_id();
        // Unwrap is fine because we just made the node id
        self.graph
            .add_push_symbol_node(node_id, symbol, true)
            .unwrap()
    }

    fn add_symbol<S: AsRef<str> + ?Sized>(&mut self, s: &S) -> Handle<Symbol> {
        self.graph.add_symbol(s)
    }

    fn edge(&mut self, from: Handle<Node>, to: Handle<Node>, precedence: i32) {
        self.graph.add_edge(from, to, precedence)
    }

    pub fn resolve(&mut self, ast: &File<'s, 'a>) {
        let root = StackGraph::root_node();

        let module_scope = self.new_scope(false);
        self.edge(module_scope, root, 0);

        for item in ast.items {
            self.item(module_scope, item);
        }

        self.print()
    }

    fn item(&mut self, module: Handle<Node>, item: &Item<'s, 'a>) {
        let Item::Function(f) = item;
        let f = f.deref();

        let symbol = self.add_symbol(f.name.deref().string);
        let item_def = self.new_definition(symbol);
        self.edge(module, item_def, 0);

        let internal_scope = self.new_scope(false);
        self.edge(internal_scope, module, 0);

        for param in f.parameters {
            let symbol = self.add_symbol(param.name.deref().string);
            let param_def = self.new_definition(symbol);
            self.edge(internal_scope, param_def, 0);
            self.block(internal_scope, f.block);
        }
    }

    fn block(&mut self, scope: Handle<Node>, block: &Spanned<Block<'s, 'a>>) {
        let block = block.deref();
        let block_scope = self.new_scope(false);
        self.edge(block_scope, scope, 0);

        // TODO: stmts

        if let Some(expr) = &block.last {
            self.expr(block_scope, expr);
        }
    }

    fn expr(&mut self, scope: Handle<Node>, expr: &Expr<'s, 'a>) {
        match expr.deref() {
            ExprKind::Ident(Ident { string }) => {
                let symbol = self.add_symbol(string);
                let ident_ref = self.new_reference(symbol);
                self.edge(ident_ref, scope, 0);
            }

            ExprKind::Lit(_) => {}
            ExprKind::If(cond, then_block, option_else_block) => {
                self.expr(scope, cond);
                self.block(scope, then_block);
                if let Some(else_block) = option_else_block {
                    self.block(scope, else_block);
                }
            }
            ExprKind::Block(block) => self.block(scope, block),
            ExprKind::Neg(expr) | ExprKind::Paren(expr) | ExprKind::Not(expr) => {
                self.expr(scope, expr)
            }
            ExprKind::Mul(left, right)
            | ExprKind::Div(left, right)
            | ExprKind::Add(left, right)
            | ExprKind::Sub(left, right)
            | ExprKind::LogicalOr(left, right)
            | ExprKind::LogicalAnd(left, right)
            | ExprKind::Gt(left, right)
            | ExprKind::Gte(left, right)
            | ExprKind::Lt(left, right)
            | ExprKind::Lte(left, right)
            | ExprKind::Eq(left, right)
            | ExprKind::Neq(left, right) => {
                self.expr(scope, left);
                self.expr(scope, right);
            }
            ExprKind::Tuple(exprs) => {
                for expr in *exprs {
                    self.expr(scope, expr);
                }
            }
            ExprKind::Call(func, args) => {
                self.expr(scope, func);
                for arg in *args.deref() {
                    self.expr(scope, arg);
                }
            }
        }
    }

    fn print(&self) {
        let mut paths = PartialPaths::new();
        let mut results = BTreeSet::new();
        let references = self
            .graph
            .iter_nodes()
            .filter(|handle| self.graph[*handle].is_reference());

        ForwardPartialPathStitcher::find_all_complete_partial_paths(
            &self.graph,
            &mut paths,
            &mut GraphEdges(None),
            references,
            &NoCancellation,
            |graph, paths, path| {
                results.insert(path.display(graph, paths).to_string());
            },
        )
        .unwrap();

        for path in results {
            eprintln!("{}", path);
        }
        print!(
            "{}",
            self.graph
                .to_html_string("", &mut paths, &mut Database::new(), &NoFilter)
                .unwrap()
        );
    }
}
