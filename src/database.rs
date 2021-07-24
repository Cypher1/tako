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
            Some(Symbol::Named(_, Some(_ext))) => break, // Found the file
            _ => {}                                      // Skip anons and regular symbols
        }
        module.pop();
    }
    module
}

pub struct DBStorage {
    pub world: World,
    project_dirs: Option<ProjectDirs>,
    pub options: Options,
    ast_to_entity: HashMap<AstTerm, Entity>,
    pub path_to_entity: HashMap<Path, Entity>,
    file_contents: HashMap<String, Arc<String>>,
    // TODO: Make entities & components
    defined_at: HashMap<Entity, HashSet<Loc>>,
    instance_at: HashMap<Entity, HashSet<Loc>>,
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
    Call, DefinedAt, Definition, HasErrors, HasType, HasValue, Sequence, SymbolRef, Token, Untyped
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
            path_to_entity: HashMap::default(),
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
    type SystemData = Entities<'a>;

    fn run(&mut self, entities: Self::SystemData) {
        for ent in (&*entities).join() {
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
        let parts: Vec<String> = module.iter().map(|sym| format!("{:?}", sym)).collect();
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

        let out_file = format!("build/{}.cc", name);
        let exec_file = format!("build/{}", name);
        let destination = std::path::Path::new(&out_file);
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
            .arg(out_file)
            .arg("-o")
            .arg(exec_file)
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
        if let Some(symbol) = self.find_symbol(path.clone(), Vec::new())? {
            return Ok(symbol.value.uses);
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
            if let Some(Symbol::Anon) = context.last() {
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

    fn entity_for_ast(&self, node: &AstTerm) -> Option<Entity> {
        self.ast_to_entity.get(node).cloned()
    }

    fn set_entity_for_ast(&mut self, term: AstTerm, entity: Entity) {
        self.ast_to_entity.insert(term, entity);
    }

    fn entity_for_path(&self, path: PathRef) -> Option<Entity> {
        self.path_to_entity.get(path).cloned()
    }

    fn set_entity_for_path(&mut self, path: PathRef, entity: Entity) {
        if let Some(_old_entity) = self.entity_for_path(path) {
            // panic!("Internal error, conflicting entities for path: {:?}. {:?} and {:?}", &path, &entity, &old_entity);
        }
        self.path_to_entity.insert(path.to_vec(), entity);
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
pub struct DefinitionHead {
    pub name: Path,
    pub params: Option<Vec<Entity>>, // TODO: Restrict to valid def-args (variables and functions with optional default values)
    pub path: Path,
}

impl DefinitionHead {
    pub fn into_call(self, storage: &mut DBStorage, loc: Loc, ty: Option<Entity>) -> AstNode {
        let name = AstTerm::Symbol {
            name: self.name,
            context: self.path,
        }
        .into_node(loc.clone(), None);
        if let Some(children) = self.params {
            let inner = storage.store_node(name);
            AstTerm::Call { inner, children }.into_node(loc, ty)
        } else {
            name
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum AstTerm {
    Value(Val),
    Symbol {
        name: Path,
        context: Path,
    },
    Sequence(Vec<Entity>), // TODO: Inline the vec somehow? Use a non empty vec?
    Call {
        inner: Entity,
        children: Vec<Entity>,
    },
    Definition {
        head: DefinitionHead,
        implementations: Vec<Entity>,
    },
    DefinitionHead(DefinitionHead),
}

impl AstTerm {
    pub fn into_node(self, loc: Loc, ty: Option<Entity>) -> AstNode {
        AstNode {
            term: self,
            loc,
            ty,
        }
    }

    pub fn into_definition(
        self,
        _storage: &mut DBStorage,
        right: Entity,
        loc: Loc,
    ) -> Result<AstNode, TError> {
        Ok(match self {
            AstTerm::Symbol { name, context } => AstTerm::Definition {
                head: DefinitionHead {
                    name,
                    params: None,
                    path: context,
                },
                implementations: vec![right],
            },
            AstTerm::DefinitionHead(head) => AstTerm::Definition {
                head,
                implementations: vec![right],
            },
            _ => {
                return Err(TError::ParseError(
                    format!("Cannot assign to {:?}", self),
                    loc.get_info(),
                ));
            }
        }
        .into_node(loc, None))
    }
}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub term: AstTerm,
    pub loc: Loc,
    pub ty: Option<Entity>,
}

impl AstNode {
    pub fn into_definition(
        self,
        storage: &mut DBStorage,
        right: Entity,
        loc: Loc,
    ) -> Result<AstNode, TError> {
        Ok(AstNode {
            ty: self.ty,
            ..self.term.into_definition(storage, right, loc)?
        })
    }
}

impl DBStorage {
    pub fn store_node_set(&mut self, node: AstNode) -> Vec<Entity> {
        match node.term {
            AstTerm::Sequence(args) => args,
            _ => vec![self.store_node(node)],
        }
    }

    pub fn store_node(&mut self, entry: AstNode) -> Entity {
        let lookup: Option<Entity> = self.entity_for_ast(&entry.term);
        let entity = if let Some(entity) = lookup {
            entity
        } else {
            if let AstTerm::DefinitionHead(head) = entry.term {
                let call = head.into_call(self, entry.loc, entry.ty);
                return self.store_node(call);
            }
            let entity = {
                let mut entity = self.world.create_entity();
                if let Some(ty) = entry.ty {
                    entity = entity.with(HasType(ty));
                }
                let entity = match entry.term.clone() {
                    AstTerm::DefinitionHead(_) => unreachable!(),
                    AstTerm::Definition {
                        head,
                        implementations,
                    } => entity.with(Definition {
                        implementations,
                        names: vec![head.name],
                        params: head.params,
                        path: head.path,
                    }),
                    AstTerm::Value(value) => entity.with(HasValue(value)),
                    AstTerm::Symbol { name, context } => entity
                        .with(SymbolRef { name, context })
                        .with(DefinedAt(None)),
                    AstTerm::Call { inner, children } => entity.with(Call(inner, children)),
                    AstTerm::Sequence(children) => {
                        // TODO: We assume this is a tuple, review this.
                        entity.with(Sequence(children))
                    }
                };
                entity.build()
            };
            if let AstTerm::Definition { head, .. } = &entry.term {
                self.add_location_for_definition(entry.loc.clone(), entity);
                let mut sub_path = head.path.clone();
                sub_path.extend(head.name.clone());
                self.set_entity_for_path(&sub_path, entity);
            }
            self.set_entity_for_ast(entry.term, entity);
            entity
        };
        self.add_location_for_entity(entry.loc, entity);
        entity
    }
}
