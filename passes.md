# Passes

Passes are implemented as queries.

## Raw Passes

```dot
digraph {
    {
        subgraph cluster_codegen {
            label="Code Generation";
            style="dotted";
            codegen_all[style=boxed, label="codegen_all(name): IO", shape=boxed];
            enumerate_binaries [label="enumerate_binaries(name): binary_info[]", shape=boxed];
            codegen[style=boxed, label="codegen(name): IO", shape=boxed];
            optimize [label="optimize([src]node): [optimized,lowered,typed]ast, [gen]node"];
            lower [label="lower([src]node): [lowered,typed]ast, [gen]node"];
            sourcemap_gen[style=boxed, label="sourcemap_gen(name): IO", shape=boxed];
        }
        subgraph cluster_semantic_analysis {
            label="Semantic Analysis";
            style="dotted";
            type_at[style=boxed, label="type_at(src_pos): type_info", shape=boxed];
            typecheck[label="typecheck(name): [typed]ast", shape=boxed];

            get_type[label="get_type([src]node): type_info"];
            check_proofs[label="check_proofs([gen]node): [typed]ast, errors[]"];
        }
        subgraph cluster_parsing {
            label="Parsing";
            style="dotted";
            find_node [label="find_node(src_pos): [src]node"];
            location [label="location([src]node): src_pos"];
            parse[label="parse(name): [partially_typed]ast, [src]node", shape=boxed];
            find_definition[label="find_definition(name): [src]node"];
            eval_front_matter[label="eval_front_matter(name): operator[], macro[], [partial] ast, token[]"];
            parse_front_matter[label="parse_front_matter(name): [partial] ast, token[]"];
            macro_expand[label="macro_expand(name): [partial]ast, [gen]node, shape=boxed"];
            handle_import[label="handle_import(name): [partial]ast"];
            lex[label="lex(name): token[]"];
        }
        subgraph cluster_loading {
            label="Loading";
            style="dotted";
            load [label="load(name, version?): string"];
            load_local_file [style=dashed, label="load_local_file(path): string with IO"];
            download_dependencies[label="download_dependencies(name, version): string with IO"];
        }
        subgraph cluster_devtools {
            label="Dev Tools";

            style="dotted";
            format[label="pretty_print(ast, node): string", shape=boxed];
            pretty_print[label="pretty_print(ast, node): string"];
            interpret [label="interpret(name): IO", shape=boxed];
            eval[label="eval(string): value with IO", shape=boxed];
            eval_node[label="eval_node([src]node): value with IO"];
        }
        subgraph cluster_error_reporting {
            label="Error Reporting";
            style="dotted";
            errors[style=boxed, label="errors(name): (src_pos, error)[]", shape=boxed];
            errors_at[style=boxed, label="errors_at(src_pos): error[]", shape=boxed];
            errors_for_node[style=boxed, label="errors_for_node([src]node): error[]"];
        }
    }

   {
    concentrate=true;

    interpret -> eval;
    eval -> eval_node;
    interpret -> find_definition;
    eval_node -> lower;
    eval_node -> location;
    codegen_all -> enumerate_binaries;
    codegen_all -> codegen;
    enumerate_binaries -> eval_node [weight=0];
    codegen -> optimize;
    optimize -> lower;
    lower -> get_type;
    codegen -> location [label="sourcemap?"];
    codegen -> errors;
    optimize -> get_type;
    errors_at -> errors_for_node;
    errors -> errors_for_node;
    errors -> location;
    errors_for_node -> check_proofs;
    check_proofs -> typecheck;
    get_type -> typecheck [label="is_annotated?"];
    get_type -> parse;
    type_at ->  get_type;
    typecheck -> macro_expand;
    errors_at -> find_node;
    type_at -> find_node;
    find_node -> parse;
    macro_expand -> parse;
    handle_import -> eval_front_matter;
    location -> lex;
    find_definition -> macro_expand;
    eval_front_matter -> parse_front_matter;
    eval_front_matter -> eval_node [weight=0];
    parse -> handle_import;
    parse_front_matter -> lex;
    lex -> load;
    load -> load_local_file;
    load -> download_dependencies;
    format -> parse;
    format -> pretty_print;
    }
}
```

> ### Legend
> 
> - Each square box is a user interface (commandline, web, interactive terminal, language server protocol API - like for VSCode)
> - Each box or circle is a query with parameters in parens and the result after the colon.
> - arrows are a dependency where one query should call another query
> - `T with Eff` is a value of type `T` but has some effect `Eff`
> - `T[]` is vector/list/multi-set of values of type `T` (these are intentionally left to the implementation to define)
> - Dashed circles are 'volatile' and might change without notifying anyone (so they have to be checked / watched)


## Notes

- A file update should not require **re-parsing** the contents of other files, unless:
  - A macro is updated and used in the other file.
  - An operator is updated and used in the other file.

- A file change should not require **re-type-checking** other files, unless:
  - A type is changed that is depended on in another file.
  - TODO: This requires a missing file / ast-diffing system.

- **Moving** a file/directory should not require re-parsing it
  - TODO: Names should be contextual / based on references?

- **Reordering** non-sequenced items should only require remapping `location` information into outputs.

- **Renaming** items (to an unused name) should only require remapping `name` information into outputs.
  - TODO: This is currently broken by all things used names as query keys.
  - TODO: This is in tension with the reordering principle as 


- Renaming to a name that doesn't currently exist should be a single string-interner lookup.


I also want to make re-ordering things in a file (where the order isn't a sequence) only require updating the locations of error messages and things like that...
But it is, in-practice, hard to work out how to do that... 
It'd also be nice if changing something in one file (that doesn't change the type / names of things / parsing relevant stuff) didn't require reprocessing every other file... 
That's also WIP
