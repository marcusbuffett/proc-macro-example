#[macro_use]
extern crate quote;

extern crate proc_macro;

extern crate proc_macro2;

use convert_case::{Case, Casing};
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use petgraph::Graph;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

#[derive(Clone, Debug)]
struct CensusField {
    string: String,
    field_name: String,
    field_type: FieldType,
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum FieldType {
    I32,
    FullStruct(String),
}

impl FieldType {
    fn to_syn(&self) -> TokenStream {
        match self {
            FieldType::I32 => {
                quote! {  Option<i32> }
            }
            FieldType::FullStruct(struct_name) => {
                let struct_name = format_ident!("{}", struct_name);
                quote! {#struct_name}
            }
        }
    }
}

fn get_indent_level(line: String) -> usize {
    line.as_bytes()
        .iter()
        .take_while(|s| s.is_ascii_whitespace())
        .count() as usize
}

fn get_ancestor_chain(node_idx: NodeIndex<u32>, tree: &Graph<InputLine, &str>) -> Vec<InputLine> {
    let mut nodes = vec![tree.node_weight(node_idx).unwrap().clone()];
    let mut node_idx = node_idx;
    loop {
        let edges = tree.edges_directed(node_idx, Direction::Incoming).next();
        match edges {
            Some(i) => {
                node_idx = i.source();
                if (tree.node_weight(i.source())).unwrap().indent_level != 0 {
                    nodes.push(tree.node_weight(i.source()).unwrap().clone())
                }
            }
            None => break,
        }
    }
    nodes
}

fn has_children(node_idx: NodeIndex<u32>, tree: &Graph<InputLine, &str>) -> bool {
    tree.edges_directed(node_idx, Direction::Outgoing)
        .next()
        .is_some()
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct InputLine {
    string: String,
    indent_level: usize,
}

fn generate_census_structs_inner(item: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input: Vec<TokenTree> = item.into_iter().collect();
    let literal = match &input.get(0) {
        Some(proc_macro2::TokenTree::Literal(literal)) => literal.to_string(),
        _ => panic!(),
    };

    let lines: Vec<InputLine> = literal
        .lines()
        .map(String::from)
        .filter(|s| !s.trim().is_empty() && s.trim() != "\"")
        .map(|s| InputLine {
            string: s.to_string(),
            indent_level: get_indent_level(s),
        })
        .collect();

    let mut tree = Graph::<InputLine, &str>::new();
    fn line_visitor(
        parent: Option<NodeIndex<u32>>,
        i: usize,
        lines: &[InputLine],
        mut tree: &mut Graph<InputLine, &str>,
    ) -> Vec<InputLine> {
        let line = lines.get(i).unwrap();
        let node = tree.add_node(line.clone());
        if let Some(parent) = parent {
            tree.add_edge(parent, node, "");
        }
        let sibling_lines: Vec<InputLine> = (0..lines.len())
            .skip(i + 1)
            .take_while(|i| lines.get(*i).unwrap().indent_level > line.indent_level)
            .filter(|i| lines.get(*i).unwrap().indent_level == line.indent_level + 4)
            .map(|i| {
                // dbg!(&i);
                line_visitor(Some(node), i, &lines, &mut tree);
                lines.get(i).unwrap().clone()
            })
            .collect();

        sibling_lines
    }
    let minimum_indent = lines.iter().map(|l| l.indent_level).min().unwrap();
    let root_lines = lines
        .iter()
        .enumerate()
        .filter(|(_, l)| l.indent_level == minimum_indent)
        .map(|(i, _)| i);
    for root_line in root_lines {
        line_visitor(None, root_line, &lines, &mut tree);
    }
    let mut generated_structs: Vec<TokenStream> = vec![];
    for node_idx in tree.node_indices() {
        let node = tree.node_weight(node_idx).unwrap();
        let edges = tree.edges_directed(node_idx, Direction::Outgoing);
        let fields = edges
            .map(|edge| {
                let child_idx = edge.target();
                let child_node = tree.node_weight(edge.target()).unwrap();
                return CensusField {
                    string: child_node.string.trim().to_string(),
                    field_name: child_node.string.to_case(Case::Snake),
                    field_type: if has_children(child_idx, &tree) {
                        FieldType::FullStruct(child_node.string.to_case(Case::Pascal))
                    } else {
                        FieldType::I32
                    },
                };
            })
            .collect::<Vec<CensusField>>();
        if fields.is_empty() {
            continue;
        }
        let field_names = fields
            .iter()
            .map(|f| format_ident!("{}", f.field_name.to_string()));
        let field_constructors = fields
            .iter()
            .map(|f| {
                let mut ancestors = get_ancestor_chain(node_idx, &tree)
                    .iter()
                    .map(|l| l.string.trim().to_string())
                    .collect::<Vec<String>>();
                ancestors.reverse();
                if f.field_type == FieldType::I32 {
                    let field_name = format_ident!("{}", f.field_name.clone());
                    ancestors.push(f.string.clone());
                    let csv_header = format!("Estimate!!{}", ancestors.join("!!"));

                    return quote! {
                        #field_name: read_field(#csv_header),
                    };
                } else {
                    let field_name = to_ident(&f.field_name);
                    let field_struct_name = to_ident(&f.field_name.to_case(Case::Pascal));
                    return quote! {
                        #field_name: #field_struct_name::new(read_field.clone()),
                    };
                }
            })
            .collect::<Vec<TokenStream>>();
        let struct_name = to_ident(&node.string.to_case(Case::Pascal));

        let field_types: Vec<TokenStream> = fields.iter().map(|f| f.field_type.to_syn()).collect();
        generated_structs.push(quote! {
            #[derive(GraphQLObject, Copy, Clone, Debug)]
            struct #struct_name {
               #( #field_names: #field_types ),*
            }

            impl #struct_name {
                pub fn new<F>(read_field: F) -> Self
                    where F: Fn(&str) -> Option<i32> + Clone {
                    Self {
                        #( #field_constructors )*
                    }
                }
            }
        })
    }
    let generated = quote! {
           #( #generated_structs )*
    };
    generated
}

fn to_ident(node: &str) -> proc_macro2::Ident {
    format_ident!("{}", node)
}

#[proc_macro]
pub fn generate_census_structs(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    generate_census_structs_inner(proc_macro2::TokenStream::from(item)).into()
}

#[test]
fn census_gen_test() {
    use std::{fs::File, io::Write};
    let generated = generate_census_structs_inner(
        quote!(
            "
dp03
    DEMOGRAPHICS
        Male
        Female
    INCOME
        Less than 50 k
        More than 50 k
                    "
        )
        .into(),
    );
    let mut file = File::create("test.rs").unwrap();
    file.write_all(format!("{}", generated).as_bytes()).unwrap();
}
