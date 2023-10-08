use std::{collections::BTreeMap, ops::Deref};

use crate::parser::{
    ast::{Block, Expr, ExprKind, File, Ident, Item, Statement},
    span::{NodeId, Spanned},
};
use stack_graphs::{
    arena::Handle,
    graph::{Node, NodeID as GraphId, StackGraph, Symbol},
    partial::{PartialPath, PartialPaths},
    serde::NoFilter,
    stitching::{Database, ForwardPartialPathStitcher, GraphEdges},
    NoCancellation,
};

pub struct Graph {
    /// The stack graph for this file
    graph: StackGraph,
    /// The handle to the file (used for creating new node ids)
    file: Handle<stack_graphs::graph::File>,
    /// Maps from stack graph id to AST id
    id_map: BTreeMap<GraphId, NodeId>,
}

impl<'s, 'a> Graph {
    pub fn new(file_name: &str) -> Self {
        let mut graph = StackGraph::new();
        let file = graph.get_or_create_file(file_name);
        let id_map = BTreeMap::new();
        Self {
            graph,
            file,
            id_map,
        }
    }

    /// Get a fresh node id
    fn new_node_id(&mut self) -> GraphId {
        self.graph.new_node_id(self.file)
    }

    /// Add a new scope node to the stack-graph
    ///
    /// This only creates a node. It is does not connect it to the graph.
    fn new_scope(&mut self, is_exported: bool) -> Handle<Node> {
        let node_id = self.new_node_id();
        // Unwrap is fine because we just made the node id
        self.graph.add_scope_node(node_id, is_exported).unwrap()
    }

    /// Add a new definition to the stack-graph
    ///
    /// This only creates a node. It is does not connect it to the graph.
    fn new_definition(&mut self, ident: &Spanned<Ident<'s>>) -> Handle<Node> {
        let symbol = self.add_symbol(ident.deref().string);
        let node_id = self.new_node_id();

        self.id_map.insert(node_id, ident.span);

        // Unwrap is fine because we just made the node id
        self.graph
            .add_pop_symbol_node(node_id, symbol, true)
            .unwrap()
    }

    /// Add a new reference to the stack-graph
    ///
    /// This only creates a node. It is does not connect it to the graph.
    fn new_reference(&mut self, ident: &Spanned<Ident<'s>>) -> Handle<Node> {
        let symbol = self.add_symbol(ident.deref().string);
        let node_id = self.new_node_id();
        self.id_map.insert(node_id, ident.span);

        // Unwrap is fine because we just made the node id
        self.graph
            .add_push_symbol_node(node_id, symbol, true)
            .unwrap()
    }

    /// Add a symbol to the stack-graph
    ///
    /// A symbol is an interned string in the graph. A symbol can be reused
    /// to reduce the time needed to recompute symbols for recurring strings.
    fn add_symbol<S: AsRef<str> + ?Sized>(&mut self, s: &S) -> Handle<Symbol> {
        self.graph.add_symbol(s)
    }

    fn edge(&mut self, from: Handle<Node>, to: Handle<Node>, precedence: i32) {
        self.graph.add_edge(from, to, precedence)
    }

    /// Resolve all names in the AST of a file.
    pub fn resolve(&mut self, ast: &File<'s, 'a>) -> Vec<(NodeId, NodeId)> {
        let root = StackGraph::root_node();

        let module_scope = self.new_scope(false);
        self.edge(module_scope, root, 0);

        for item in ast.items {
            self.item(module_scope, item);
        }

        // Find all reference nodes
        let references = self
            .graph
            .iter_nodes()
            .filter(|handle| self.graph[*handle].is_reference());

        // Find **all** paths from the reference nodes to some definition.
        // This includes shadowed paths.
        let mut results = Vec::<PartialPath>::new();
        let mut paths = PartialPaths::new();
        ForwardPartialPathStitcher::find_all_complete_partial_paths(
            &self.graph,
            &mut paths,
            &mut GraphEdges(None),
            references,
            &NoCancellation,
            |_graph, _paths, path| {
                results.push(path.clone());
            },
        )
        .unwrap();

        // Remove all the shadowed paths from the list
        //
        // This bit is based on how the tree-sitter CLI of stack-graphs works.
        // It feels slow, but maybe it's not so bad. A possible optimization is
        // to store the paths per reference and only compare those. However,
        // that is also the first check that `shadows` does, so it remains to
        // be seen whether that actually improves performance. The code in the
        // CLI uses a BTreeMap, which might also help.
        //
        // We also put in our own node ids here, so we get spans. The
        // stack-graphs has spans too, which come from the lsp_positions crate.
        // However, those require line numbers and columns, which area bit
        // complicated to compute.
        let mut no_shadow_results = Vec::new();
        for res in &results {
            if results.iter().all(|other| !other.shadows(&mut paths, res)) {
                let start = self.id_map.get(&self.graph[res.start_node].id()).unwrap();
                let end = self.id_map.get(&self.graph[res.end_node].id()).unwrap();
                no_shadow_results.push((*start, *end))
            }
        }

        no_shadow_results
    }

    fn item(&mut self, module: Handle<Node>, item: &Item<'s, 'a>) {
        let Item::Function(f) = item;
        let f = f.deref();

        let item_def = self.new_definition(&f.name);
        self.edge(module, item_def, 0);

        let internal_scope = self.new_scope(false);
        self.edge(internal_scope, module, 0);

        for param in f.parameters {
            // The edge from the parameter to the definition has a higher precedence,
            // because the parameter should shadow previous bindings.
            let param_def = self.new_definition(&param.name);
            self.edge(internal_scope, param_def, 1);
            self.block(internal_scope, f.block);
        }
    }

    fn block(&mut self, scope: Handle<Node>, block: &Spanned<Block<'s, 'a>>) {
        let block = block.deref();
        let block_scope = self.new_scope(false);
        self.edge(block_scope, scope, 0);

        // This is the last scope we created, which starts as the block scope
        // and is updated to the scope of any `let` we encounter in this block.
        let mut last_scope = block_scope;

        for stmt in block.stmts {
            match stmt {
                Statement::Expr(expr) => self.expr(last_scope, expr),
                Statement::Let(ident, _, expr) => {
                    // Expr in the scope _before_ the let
                    self.expr(last_scope, expr);

                    // Update the scope
                    //
                    // The edge from the let to the definition has a higher precedence,
                    // because the let should shadow previous bindings.
                    let let_scope = self.new_scope(false);
                    self.edge(let_scope, last_scope, 0);
                    let def_id = self.new_definition(ident);
                    self.edge(let_scope, def_id, 1);
                    last_scope = let_scope;
                }
            }
        }

        if let Some(expr) = &block.last {
            self.expr(last_scope, expr);
        }
    }

    fn expr(&mut self, scope: Handle<Node>, expr: &Expr<'s, 'a>) {
        match expr.deref() {
            // Identifiers in expressions are references
            ExprKind::Ident(ident) => {
                let ident_ref = self.new_reference(ident);
                self.edge(ident_ref, scope, 0);
            }

            // Lit contains no further expressions, so we're done
            ExprKind::Lit(_) => {}

            // Identifiers and blocks might be in these variants, so recurse
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

    pub fn print(&self) {
        print!(
            "{}",
            self.graph
                .to_html_string(
                    "",
                    &mut PartialPaths::new(),
                    &mut Database::new(),
                    &NoFilter
                )
                .unwrap()
        );
    }
}
