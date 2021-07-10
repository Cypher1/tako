use specs::prelude::*;
use specs::World;
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use directories::ProjectDirs;

use crate::ast::{path_to_string, Node, Path, PathRef, Root, Symbol, Visitor};
use crate::cli_options::Options;
use crate::components::*;
use crate::errors::TError;
use crate::externs::get_externs;
use crate::externs::{Extern, Semantic};
use crate::primitives::Val;
use crate::symbol_table::Table;

fn to_file_path(context: PathRef) -> Path {
    let mut module = context.to_vec();
    loop {
        match module.last() {
            None => panic!(
                "Couldn't find a file associated with symbol at {}",
                path_to_string(context)
            ),
            Some(Symbol::Anon()) => {}                   // Skip anons
            Some(Symbol::Named(_, None)) => {}           // Skip regular symbols
            Some(Symbol::Named(_, Some(_ext))) => break, // Found the file
        }
        module.pop();
    }
    module
}

pub struct DBStorage {
    world: World,
    project_dirs: Option<ProjectDirs>,
    pub options: Options,
    ast_to_entity: HashMap<AstNode, Entity>,
    defined_at: HashMap<Entity, HashSet<Loc>>,
    // refers_to: HashMap<Loc, Entity>,
    instance_at: HashMap<Entity, HashSet<Loc>>,
    file_contents: HashMap<String, Arc<String>>,
}

macro_rules! define_components {
    ( $($component:ty),* ) => {
        /// Register all components with the world.
        fn register_components(world: &mut World) {
            $( world.register::<$component>(); )*
        }

        /// Print all the components that are associated with an entity.
        fn format_entity(world: &World, entity: Entity) -> String {
            let mut out = format!("Entity {}:", entity.id());
            $(
                if let Some(component) = world.read_storage::<$component>().get(entity) {
                    out = format!("{}\n - {:?}", out, component);
                }
            )*
            out
        }

        fn print_entity(world: &World, entity: Entity) {
            print!("{}", format_entity(world, entity));
        }
    }
}

define_components!(
    AtLoc,
    HasArguments,
    HasChildren,
    HasErrors,
    HasInner,
    HasSymbol,
    HasValue,
    IsDefinition,
    IsSymbol,
    Token,
    Typed,
    Untyped
);

impl Default for DBStorage {
    fn default() -> Self {
        let mut world = World::new();
        register_components(&mut world);

        let project_dirs = ProjectDirs::from("systems", "mimir", "tako");
        Self {
            world,
            project_dirs,
            options: Options::default(),
            file_contents: HashMap::default(),
            ast_to_entity: HashMap::default(),
            defined_at: HashMap::default(),
            // refers_to: HashMap::default(),
            instance_at: HashMap::default(),
        }
    }
}

struct DebugSystem<'a, T> {
    f: &'a dyn Fn(Entity) -> T,
    results: Vec<T>,
}

impl<'a, T> System<'a> for DebugSystem<'a, T> {
    type SystemData = (Entities<'a>, ReadStorage<'a, AtLoc>);

    fn run(&mut self, (entities, at_loc_storage): Self::SystemData) {
        for (ent, _) in (&*entities, &at_loc_storage).join() {
            self.results.push((self.f)(ent));
        }
    }
}

use crate::location::Loc;
impl DBStorage {
    pub fn print_entity(&self, entity: Entity) {
        print_entity(&self.world, entity);
    }

    pub fn format_entity(&self, entity: Entity) -> String {
        format_entity(&self.world, entity)
    }

    pub fn format_entities(&self) -> String {
        let f = |entity| format_entity(&self.world, entity);
        let mut mapper = DebugSystem::<String> {
            f: &f,
            results: Vec::new(),
        };
        mapper.run_now(&self.world);
        // self.world.maintain(); // Nah?
        mapper.results.join("\n")
    }

    pub fn config_dir(&self) -> PathBuf {
        if let Some(project_dirs) = &self.project_dirs {
            project_dirs.config_dir().to_path_buf()
        } else {
            PathBuf::new()
        }
    }

    pub fn history_file(&self) -> PathBuf {
        self.config_dir().join("tako_history")
    }

    pub fn debug_level(&self) -> i32 {
        self.options.debug_level
    }

    pub fn file(&mut self, filename: &str) -> Option<&Arc<String>> {
        self.file_contents.get(filename)
    }

    pub fn set_file(&mut self, filename: &str, contents: String) {
        self.file_contents
            .insert(filename.to_owned(), Arc::new(contents));
    }

    pub fn module_name(&self, filename: String) -> Path {
        filename
            .replace("\\", "/")
            .split('/')
            .map(|part| {
                let name: Vec<&str> = part.split('.').collect();
                Symbol::Named(
                    name[0].to_owned(),
                    if name.len() > 1 {
                        Some(name[1..].join("."))
                    } else {
                        None
                    },
                )
            })
            .collect()
    }

    pub fn filename(&self, module: Path) -> String {
        let parts: Vec<String> = module
            .iter()
            .map(|sym| match sym {
                Symbol::Named(sym, None) => sym.to_owned(),
                Symbol::Named(sym, Some(ext)) => format!("{}.{}", sym, ext),
                Symbol::Anon() => "?".to_owned(),
            })
            .collect();
        let file_name = parts.join("/");
        if self.debug_level() > 0 {
            eprintln!(
                "Getting filename for {}, {}",
                path_to_string(&module),
                file_name
            );
        }
        file_name
    }

    pub fn get_externs(&self) -> Result<HashMap<String, Extern>, TError> {
        get_externs()
    }

    pub fn get_extern_names(&self) -> Result<Vec<String>, TError> {
        Ok(get_externs()?.keys().cloned().collect())
    }

    pub fn get_extern(&self, name: String) -> Result<Option<Extern>, TError> {
        Ok(get_externs()?.get(&name).cloned())
    }

    pub fn get_extern_operator(&self, name: String) -> Result<Semantic, TError> {
        Ok(self
            .get_extern(name)?
            .map(|x| x.semantic)
            .unwrap_or(Semantic::Func))
    }

    pub fn parse_string(&mut self, module: Path, contents: Arc<String>) -> Result<Node, TError> {
        use crate::passes::parser;
        Ok(parser::parse_string(self, &module, &contents)?.0)
    }

    pub fn parse_str(&mut self, module: Path, contents: &'static str) -> Result<Node, TError> {
        self.parse_string(module, Arc::new(contents.to_string()))
    }

    pub fn parse_file(&mut self, module: Path) -> Result<Node, TError> {
        if self.debug_level() > 0 {
            eprintln!("parsing file... {}", path_to_string(&module));
        }
        let filename = self.filename(module.clone());
        let contents = if let Some(contents) = self.file_contents.get(&filename) {
            contents.clone()
        } else {
            // Load the file
            let raw_contents = std::fs::read_to_string(&filename)?;
            let contents = Arc::new(raw_contents);
            self.file_contents.insert(filename, contents.clone());
            contents
        };
        self.parse_string(module, contents)
    }
    pub fn infer(&mut self, expr: Node, env: Val) -> Result<Val, TError> {
        use crate::passes::type_checker::infer;
        if self.debug_level() > 0 {
            eprintln!("infering type for ... {}", &expr);
        }
        infer(self, &expr, &env)
    }

    pub fn look_up_definitions(&mut self, context: Path) -> Result<Root, TError> {
        use crate::passes::definition_finder::DefinitionFinder;
        let module = to_file_path(&context);
        if self.debug_level() > 0 {
            eprintln!("look up definitions >> {}", path_to_string(&module));
        }
        DefinitionFinder::process(&module, self)
    }

    pub fn compile_to_cpp(&mut self, module: Path) -> Result<(String, HashSet<String>), TError> {
        use crate::passes::to_cpp::CodeGenerator;
        if self.debug_level() > 0 {
            eprintln!("generating code for file ... {}", path_to_string(&module));
        }
        CodeGenerator::process(&module, self)
    }

    pub fn build_with_gpp(&mut self, module: Path) -> Result<String, TError> {
        let (res, flags) = self.compile_to_cpp(module.clone())?;
        if self.debug_level() > 0 {
            eprintln!("building file with g++ ... {}", path_to_string(&module));
        }

        let name: String = module
            .iter()
            .map(|s| s.to_name())
            .collect::<Vec<String>>()
            .join("_");

        let outf = format!("build/{}.cc", name);
        let execf = format!("build/{}", name);
        let destination = std::path::Path::new(&outf);
        std::fs::create_dir_all("build").expect("could not create build directory");
        let mut f = std::fs::File::create(&destination).expect("could not open output file");
        write!(f, "{}", res).expect("couldn't write to file");

        let mut cmd = Command::new("g++");
        for arg in flags.iter() {
            cmd.arg(arg);
        }
        let output = cmd
            .arg("-std=c++14")
            .arg("-Wall")
            .arg("-Werror")
            .arg("-Wfatal-errors")
            .arg("-O3")
            .arg(outf)
            .arg("-o")
            .arg(execf)
            .output()
            .expect("could not run g++");
        if !output.status.success() {
            let s = String::from_utf8(output.stderr)
                .expect("Illegal utf8 stderr from backend compiler");
            use crate::ast::Info;
            return Err(TError::CppCompilerError(
                s,
                output.status.code(),
                Info::default(),
            ));
        }
        let s =
            String::from_utf8(output.stdout).expect("Illegal utf8 stdout from backend compiler");
        eprintln!("{}", s);
        Ok(res)
    }

    pub fn find_symbol_uses(&mut self, path: Path) -> Result<HashSet<Path>, TError> {
        if let Some(symb) = self.find_symbol(path.clone(), Vec::new())? {
            return Ok(symb.value.uses);
        }
        use crate::ast::Info;
        Err(TError::UnknownSymbol(
            path_to_string(&path),
            Info::default(),
            "".to_string(),
        ))
    }

    pub fn build_symbol_table(&mut self, module: Path) -> Result<Root, TError> {
        use crate::passes::symbol_table_builder::SymbolTableBuilder;
        SymbolTableBuilder::process(&module, self)
    }

    pub fn find_symbol(&mut self, mut context: Path, path: Path) -> Result<Option<Table>, TError> {
        if self.debug_level() > 1 {
            eprintln!(
                ">>> looking for symbol {} in {}",
                path_to_string(&path),
                path_to_string(&context)
            );
        }
        let table = self.look_up_definitions(context.clone())?.table;
        loop {
            if let Some(Symbol::Anon()) = context.last() {
                context.pop(); // Cannot look inside an 'anon'.
            }
            let mut search: Vec<Symbol> = context.clone();
            search.extend(path.clone());
            if let Some(node) = table.find(&search) {
                if self.debug_level() > 1 {
                    eprintln!(
                        "FOUND INSIDE {} {}",
                        path_to_string(&context),
                        path_to_string(&search)
                    );
                }
                return Ok(Some(node.clone()));
            }
            if self.debug_level() > 1 {
                eprintln!(
                    "   not found {} at {}",
                    path_to_string(&path),
                    path_to_string(&search)
                );
            }
            if context.is_empty() {
                eprintln!(
                    "   not found {} at {}",
                    path_to_string(&path),
                    path_to_string(&search)
                );
                return Ok(None);
            }
            context.pop(); // Up one, go again.
        }
    }

    fn entity_for_ast(&self, node: &AstNode) -> Option<Entity> {
        self.ast_to_entity.get(node).cloned()
    }

    fn set_entity_for_ast(&mut self, node: AstNode, entity: Entity) {
        self.ast_to_entity.insert(node, entity);
    }

    fn add_location_for_entity(&mut self, loc: Loc, entity: Entity) {
        self.instance_at
            .entry(entity)
            .or_insert_with(HashSet::new)
            .insert(loc);
    }

    fn add_location_for_definition(&mut self, loc: Loc, entity: Entity) {
        self.defined_at
            .entry(entity)
            .or_insert_with(HashSet::new)
            .insert(loc);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum AstNode {
    Value(Val),
    Symbol(String),
    Chain(Vec<Entity>), // TODO: Inline the vec somehow?
    Apply {
        inner: Entity,
        children: Vec<Entity>,
    },
    Definition {
        name: String,
        args: Option<Vec<Entity>>,
        implementations: Vec<Entity>,
    },
}

impl AstNode {
    pub fn into_data(self, loc: Loc) -> AstNodeData {
        AstNodeData { node: self, loc }
    }
}

#[derive(Debug, Clone)]
pub struct AstNodeData {
    pub node: AstNode,
    pub loc: Loc,
}

impl DBStorage {
    pub fn store_node_set(&mut self, node: AstNodeData) -> Vec<Entity> {
        match node.node {
            AstNode::Chain(args) => args,
            _ => vec![self.store_node(AstNodeData {
                node: node.node,
                loc: node.loc,
            })], // TODO
        }
    }

    pub fn store_node(&mut self, node: AstNodeData) -> Entity {
        let lookup: Option<Entity> = self.entity_for_ast(&node.node);
        let entity = if let Some(entity) = lookup {
            entity
        } else {
            let entity = {
                let mut entity = self.world.create_entity();
                let entity = match node.node.clone() {
                    AstNode::Definition {
                        name,
                        args,
                        implementations,
                    } => entity
                        .with(HasSymbol(name))
                        .with(HasArguments(args))
                        .with(HasChildren(implementations))
                        .with(IsDefinition),
                    AstNode::Value(value) => entity.with(HasValue(value)),
                    AstNode::Symbol(name) => entity.with(HasSymbol(name)).with(IsSymbol),
                    AstNode::Apply { inner, children } => {
                        if !children.is_empty() {
                            entity = entity.with(HasChildren(children));
                        }
                        entity.with(HasInner(inner))
                    }
                    AstNode::Chain(children) => {
                        // TODO: We assume this is a tuple, review this.
                        entity.with(HasChildren(children))
                    }
                };
                entity.with(AtLoc(node.loc.clone())).build()
            };
            if let AstNode::Definition { .. } = &node.node {
                self.add_location_for_definition(node.loc.clone(), entity);
            }
            self.set_entity_for_ast(node.node, entity);
            entity
        };
        self.add_location_for_entity(node.loc, entity);
        entity
    }
}
