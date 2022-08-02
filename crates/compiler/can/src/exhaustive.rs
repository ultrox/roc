use crate::expr::{self, IntValue, WhenBranch};
use crate::pattern::DestructType;
use roc_collections::all::HumanIndex;
use roc_collections::VecSet;
use roc_error_macros::internal_error;
use roc_exhaustive::{
    is_useful, Ctor, CtorName, Error, Guard, Literal, Pattern, RenderAs, TagId, Union,
};
use roc_module::ident::{TagIdIntType, TagName};
use roc_module::symbol::{Interns, ModuleId};
use roc_region::all::{Loc, Region};
use roc_types::subs::{Content, FlatType, RedundantMark, Subs, SubsFmtContent, Variable};
use roc_types::types::{AliasKind, RecordStructure, TagUnionStructure};

use ven_pretty::{Arena, DocAllocator, DocBuilder};

pub use roc_exhaustive::Context as ExhaustiveContext;

pub const GUARD_CTOR: &str = "#Guard";
pub const NONEXHAUSIVE_CTOR: &str = "#Open";

pub struct ExhaustiveSummary {
    pub errors: Vec<Error>,
    pub exhaustive: bool,
    pub redundancies: Vec<RedundantMark>,
}

pub fn check(
    subs: &Subs,
    sketched_rows: SketchedRows,
    context: ExhaustiveContext,
) -> ExhaustiveSummary {
    let overall_region = sketched_rows.overall_region;
    let mut all_errors = Vec::with_capacity(1);

    let NonRedundantSummary {
        non_redundant_rows,
        errors,
        redundancies,
    } = sketched_rows.reify_to_non_redundant(subs);
    all_errors.extend(errors);

    let exhaustive = match roc_exhaustive::check(overall_region, context, non_redundant_rows) {
        Ok(()) => true,
        Err(errors) => {
            all_errors.extend(errors);
            false
        }
    };

    ExhaustiveSummary {
        errors: all_errors,
        exhaustive,
        redundancies,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum SketchedPattern {
    Anything,
    Literal(Literal),
    Ctor(Variable, TagName, Vec<SketchedPattern>),
    KnownCtor(Union, TagId, Vec<SketchedPattern>),
}

impl SketchedPattern {
    fn reify(self, subs: &Subs) -> Pattern {
        match self {
            Self::Anything => Pattern::Anything,
            Self::Literal(lit) => Pattern::Literal(lit),
            Self::KnownCtor(union, tag_id, patterns) => Pattern::Ctor(
                union,
                tag_id,
                patterns.into_iter().map(|pat| pat.reify(subs)).collect(),
            ),
            Self::Ctor(var, tag_name, patterns) => {
                let (union, tag_id) = convert_tag(subs, var, &tag_name);
                Pattern::Ctor(
                    union,
                    tag_id,
                    patterns.into_iter().map(|pat| pat.reify(subs)).collect(),
                )
            }
        }
    }

    fn to_doc<'a>(&'a self, interns: &'a Interns, f: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        match self {
            SketchedPattern::Anything => f.text("_"),
            SketchedPattern::Literal(lit) => match lit {
                Literal::Int(n) => f.text(i128::from_ne_bytes(*n).to_string()),
                Literal::U128(n) => f.text(u128::from_ne_bytes(*n).to_string()),
                Literal::Bit(b) => f.text(b.to_string()),
                Literal::Byte(b) => f.text(b.to_string()),
                Literal::Float(n) => f.text(f64::from_bits(*n).to_string()),
                Literal::Decimal(d) => f.text(roc_std::RocDec::from_ne_bytes(*d).to_string()),
                Literal::Str(s) => f.text(&**s),
            },
            SketchedPattern::Ctor(_, tag, args) => {
                if args.is_empty() {
                    f.text(tag.0.as_str())
                } else {
                    f.text(tag.0.as_str())
                        .append(f.space())
                        .append(
                            f.intersperse(args.iter().map(|arg| arg.to_doc(interns, f)), f.space()),
                        )
                        .group()
                }
            }
            SketchedPattern::KnownCtor(union, _, args) => match &union.render_as {
                RenderAs::Opaque => {
                    let opaque_name = union.alternatives[0].name.to_str(interns);
                    f.text("@")
                        .append(f.text(opaque_name))
                        .append(f.space())
                        .append(
                            f.intersperse(args.iter().map(|arg| arg.to_doc(interns, f)), f.space()),
                        )
                        .group()
                }
                RenderAs::Record(fields) => f
                    .reflow("{")
                    .append(
                        f.concat(fields.iter().zip(args).map(|(name, destruct)| {
                            let field = f
                                .text(name.as_str())
                                .append(f.reflow(": "))
                                .append(destruct.to_doc(interns, f))
                                .nest(2)
                                .group();
                            f.line().append(field).append(",")
                        }))
                        .nest(2)
                        .group(),
                    )
                    .append(f.line())
                    .append(f.text("}"))
                    .group(),

                RenderAs::Tag | RenderAs::Guard => internal_error!(),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct SketchedRow {
    patterns: Vec<SketchedPattern>,
    region: Region,
    guard: Guard,
    redundant_mark: RedundantMark,
}

impl SketchedRow {
    fn to_doc<'a>(&'a self, interns: &'a Interns, f: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        f.intersperse(
            self.patterns.iter().map(|pat| pat.to_doc(interns, f)),
            f.reflow(" | "),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SketchedRows {
    rows: Vec<SketchedRow>,
    overall_region: Region,
}

impl SketchedRows {
    fn reify_to_non_redundant(self, subs: &Subs) -> NonRedundantSummary {
        to_nonredundant_rows(subs, self)
    }

    pub fn overall_region(&self) -> Region {
        self.overall_region
    }

    pub fn to_doc(&self, interns: &Interns) -> String {
        let f = Arena::new();
        f.intersperse(
            self.rows.iter().map(|row| row.to_doc(interns, &f)),
            f.hardline(),
        )
        .1
        .pretty(80)
        .to_string()
    }
}

fn sketch_pattern(var: Variable, pattern: &crate::pattern::Pattern) -> SketchedPattern {
    use crate::pattern::Pattern::*;
    use SketchedPattern as SP;

    match pattern {
        &NumLiteral(_, _, IntValue::I128(n), _) | &IntLiteral(_, _, _, IntValue::I128(n), _) => {
            SP::Literal(Literal::Int(n))
        }
        &NumLiteral(_, _, IntValue::U128(n), _) | &IntLiteral(_, _, _, IntValue::U128(n), _) => {
            SP::Literal(Literal::U128(n))
        }
        &FloatLiteral(_, _, _, f, _) => SP::Literal(Literal::Float(f64::to_bits(f))),
        StrLiteral(v) => SP::Literal(Literal::Str(v.clone())),
        &SingleQuote(c) => SP::Literal(Literal::Byte(c as u8)),
        RecordDestructure { destructs, .. } => {
            let tag_id = TagId(0);
            let mut patterns = std::vec::Vec::with_capacity(destructs.len());
            let mut field_names = std::vec::Vec::with_capacity(destructs.len());

            for Loc {
                value: destruct,
                region: _,
            } in destructs
            {
                field_names.push(destruct.label.clone());

                match &destruct.typ {
                    DestructType::Required | DestructType::Optional(..) => {
                        patterns.push(SP::Anything)
                    }
                    DestructType::Guard(_, guard) => {
                        patterns.push(sketch_pattern(destruct.var, &guard.value))
                    }
                }
            }

            let union = Union {
                render_as: RenderAs::Record(field_names),
                alternatives: vec![Ctor {
                    name: CtorName::Tag(TagName("#Record".into())),
                    tag_id,
                    arity: destructs.len(),
                }],
            };

            SP::KnownCtor(union, tag_id, patterns)
        }

        AppliedTag {
            tag_name,
            arguments,
            ..
        } => {
            let simplified_args: std::vec::Vec<_> = arguments
                .iter()
                .map(|(var, arg)| sketch_pattern(*var, &arg.value))
                .collect();

            SP::Ctor(var, tag_name.clone(), simplified_args)
        }

        UnwrappedOpaque {
            opaque, argument, ..
        } => {
            let (arg_var, argument) = &(**argument);

            let tag_id = TagId(0);

            let union = Union {
                render_as: RenderAs::Opaque,
                alternatives: vec![Ctor {
                    name: CtorName::Opaque(*opaque),
                    tag_id,
                    arity: 1,
                }],
            };

            SP::KnownCtor(
                union,
                tag_id,
                vec![sketch_pattern(*arg_var, &argument.value)],
            )
        }

        // Treat this like a literal so we mark it as non-exhaustive
        MalformedPattern(..) => SP::Literal(Literal::Byte(1)),

        Underscore
        | Identifier(_)
        | AbilityMemberSpecialization { .. }
        | Shadowed(..)
        | OpaqueNotInScope(..)
        | UnsupportedPattern(..) => SP::Anything,
    }
}

pub fn sketch_when_branches(
    target_var: Variable,
    region: Region,
    patterns: &[expr::WhenBranch],
) -> SketchedRows {
    let mut rows: Vec<SketchedRow> = Vec::with_capacity(patterns.len());

    // If any of the branches has a guard, e.g.
    //
    // when x is
    //      y if y < 10 -> "foo"
    //      _ -> "bar"
    //
    // then we treat it as a pattern match on the pattern and a boolean, wrapped in the #Guard
    // constructor. We can use this special constructor name to generate better error messages.
    // This transformation of the pattern match only works because we only report exhaustiveness
    // errors: the Pattern created in this file is not used for code gen.
    //
    // when x is
    //      #Guard y True -> "foo"
    //      #Guard _ _    -> "bar"
    let any_has_guard = patterns.iter().any(|branch| branch.guard.is_some());

    use SketchedPattern as SP;
    for WhenBranch {
        patterns,
        guard,
        value: _,
        redundant,
    } in patterns
    {
        let guard = if guard.is_some() {
            Guard::HasGuard
        } else {
            Guard::NoGuard
        };

        for loc_pat in patterns {
            // Decompose each pattern in the branch into its own row.

            let patterns = if any_has_guard {
                let guard_pattern = match guard {
                    Guard::HasGuard => SP::Literal(Literal::Bit(true)),
                    Guard::NoGuard => SP::Anything,
                };

                let tag_id = TagId(0);

                let union = Union {
                    render_as: RenderAs::Guard,
                    alternatives: vec![Ctor {
                        tag_id,
                        name: CtorName::Tag(TagName(GUARD_CTOR.into())),
                        arity: 2,
                    }],
                };

                vec![SP::KnownCtor(
                    union,
                    tag_id,
                    // NB: ordering the guard pattern first seems to be better at catching
                    // non-exhaustive constructors in the second argument; see the paper to see if
                    // there is a way to improve this in general.
                    vec![
                        guard_pattern,
                        sketch_pattern(target_var, &loc_pat.pattern.value),
                    ],
                )]
            } else {
                // Simple case
                vec![sketch_pattern(target_var, &loc_pat.pattern.value)]
            };

            let row = SketchedRow {
                patterns,
                region: loc_pat.pattern.region,
                guard,
                redundant_mark: *redundant,
            };
            rows.push(row);
        }
    }

    SketchedRows {
        rows,
        overall_region: region,
    }
}

pub fn sketch_pattern_to_rows(
    target_var: Variable,
    region: Region,
    pattern: &crate::pattern::Pattern,
) -> SketchedRows {
    let row = SketchedRow {
        patterns: vec![sketch_pattern(target_var, pattern)],
        region,
        // A single row cannot be redundant!
        redundant_mark: RedundantMark::known_non_redundant(),
        guard: Guard::NoGuard,
    };
    SketchedRows {
        rows: vec![row],
        overall_region: region,
    }
}

/// REDUNDANT PATTERNS

struct NonRedundantSummary {
    non_redundant_rows: Vec<Vec<Pattern>>,
    redundancies: Vec<RedundantMark>,
    errors: Vec<Error>,
}

/// INVARIANT: Produces a list of rows where (forall row. length row == 1)
fn to_nonredundant_rows(subs: &Subs, rows: SketchedRows) -> NonRedundantSummary {
    let SketchedRows {
        rows,
        overall_region,
    } = rows;
    let mut checked_rows = Vec::with_capacity(rows.len());

    let mut redundancies = vec![];
    let mut errors = vec![];

    for SketchedRow {
        patterns,
        guard,
        region,
        redundant_mark,
    } in rows.into_iter()
    {
        let next_row: Vec<Pattern> = patterns
            .into_iter()
            .map(|pattern| pattern.reify(subs))
            .collect();

        if matches!(guard, Guard::HasGuard) || is_useful(checked_rows.clone(), next_row.clone()) {
            checked_rows.push(next_row);
        } else {
            redundancies.push(redundant_mark);
            errors.push(Error::Redundant {
                overall_region,
                branch_region: region,
                index: HumanIndex::zero_based(checked_rows.len()),
            });
        }
    }

    NonRedundantSummary {
        non_redundant_rows: checked_rows,
        redundancies,
        errors,
    }
}

fn convert_tag(subs: &Subs, whole_var: Variable, this_tag: &TagName) -> (Union, TagId) {
    let content = subs.get_content_without_compacting(whole_var);

    use {Content::*, FlatType::*};

    match dealias_tag(subs, content) {
        Structure(TagUnion(tags, ext) | RecursiveTagUnion(_, tags, ext)) => {
            let (sorted_tags, ext) = tags.sorted_iterator_and_ext(subs, *ext);

            let mut num_tags = sorted_tags.len();

            // DEVIATION: model openness by attaching a #Open constructor, that can never
            // be matched unless there's an `Anything` pattern.
            let opt_openness_tag = match subs.get_content_without_compacting(ext) {
                FlexVar(_) | RigidVar(_) => {
                    let openness_tag = TagName(NONEXHAUSIVE_CTOR.into());
                    num_tags += 1;
                    Some((openness_tag, &[] as _))
                }
                Structure(EmptyTagUnion) => None,
                // Anything else is erroneous and we ignore
                _ => None,
            };

            // High tag ID if we're out-of-bounds.
            let mut my_tag_id = TagId(num_tags as TagIdIntType);

            let mut alternatives = Vec::with_capacity(num_tags);
            let alternatives_iter = sorted_tags.into_iter().chain(opt_openness_tag.into_iter());

            for (index, (tag, args)) in alternatives_iter.enumerate() {
                let tag_id = TagId(index as TagIdIntType);
                if this_tag == &tag {
                    my_tag_id = tag_id;
                }
                alternatives.push(Ctor {
                    name: CtorName::Tag(tag),
                    tag_id,
                    arity: args.len(),
                });
            }

            let union = Union {
                alternatives,
                render_as: RenderAs::Tag,
            };

            (union, my_tag_id)
        }
        _ => internal_error!(
            "Content is not a tag union: {:?}",
            SubsFmtContent(content, subs)
        ),
    }
}

pub fn dealias_tag<'a>(subs: &'a Subs, content: &'a Content) -> &'a Content {
    use Content::*;
    let mut result = content;
    loop {
        match result {
            Alias(_, _, real_var, AliasKind::Structural)
            | RecursionVar {
                structure: real_var,
                ..
            } => result = subs.get_content_without_compacting(*real_var),
            _ => return result,
        }
    }
}

/// Sketches rows synthetically from a type variable.
pub fn sketch_variable(subs: &Subs, var: Variable, region: Region) -> SketchedRows {
    let mut ctx = Ctx {
        subs,
        seen_recursion_vars: Default::default(),
    };
    let patterns = sketch_variable_help(&mut ctx, var);
    let rows = patterns
        .into_iter()
        .map(|pat| SketchedRow {
            patterns: vec![pat],
            region,
            guard: Guard::NoGuard,
            redundant_mark: RedundantMark::known_non_redundant(),
        })
        .collect();

    SketchedRows {
        rows,
        overall_region: region,
    }
}

struct Ctx<'a> {
    subs: &'a Subs,
    seen_recursion_vars: VecSet<Variable>,
}

/// Given a list of [`args`], build a table of patterns that exhausts them.
/// For example, if args = [ [A, B], [C, D] ], then we build the rows
///   A C
///   A D
///   B C
///   B D
fn build_args_opts(
    ctx: &mut Ctx,
    args: impl IntoIterator<Item = Variable>,
    build_pattern: impl Fn(Vec<SketchedPattern>) -> SketchedPattern,
) -> impl Iterator<Item = SketchedPattern> {
    let base = vec![vec![]];

    let args_opts = args.into_iter().fold(base, |args_opts, arg| {
        let next_args_opts = sketch_variable_help(ctx, arg);
        let all_args_opts = next_args_opts
            .into_iter()
            .flat_map(|next_arg| {
                let mut args_opts = args_opts.clone();
                args_opts
                    .iter_mut()
                    .for_each(|arg_seq| arg_seq.push(next_arg.clone()));

                args_opts
            })
            .collect();

        all_args_opts
    });

    args_opts.into_iter().map(move |args| build_pattern(args))
}

fn sketch_variable_help(ctx: &mut Ctx, var: Variable) -> Vec<SketchedPattern> {
    match *ctx.subs.get_content_without_compacting(var) {
        Content::FlexVar(_)
        | Content::RigidVar(_)
        | Content::FlexAbleVar(_, _)
        | Content::RigidAbleVar(_, _)
        | Content::Error
        | Content::RangedNumber(_) => vec![SketchedPattern::Anything],
        Content::RecursionVar {
            structure,
            opt_name: _,
        } => sketch_variable_help(ctx, structure),
        Content::Alias(symbol, _, arg_var, AliasKind::Opaque) => {
            let arg_options = sketch_variable_help(ctx, arg_var);

            let tag_id = TagId(0);
            let union = Union {
                render_as: RenderAs::Opaque,
                alternatives: vec![Ctor {
                    name: CtorName::Opaque(symbol),
                    tag_id,
                    arity: 1,
                }],
            };

            arg_options
                .into_iter()
                .map(|arg| SketchedPattern::KnownCtor(union.clone(), tag_id, vec![arg]))
                .collect()
        }
        Content::Alias(symbol, _, real_var, AliasKind::Structural) => {
            if symbol.module_id() == ModuleId::NUM {
                vec![SketchedPattern::Anything]
            } else {
                sketch_variable_help(ctx, real_var)
            }
        }
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(_, _) => vec![SketchedPattern::Anything],
            FlatType::Func(_, _, _) => vec![SketchedPattern::Anything],
            FlatType::Record(fields, ext) => {
                let RecordStructure { fields, ext: _ } =
                    match roc_types::types::gather_fields(ctx.subs, fields, ext) {
                        Ok(record_structure) => record_structure,
                        Err(_) => return vec![SketchedPattern::Anything],
                    };

                let tag_id = TagId(0);
                let union = Union {
                    render_as: RenderAs::Record(
                        fields.iter().map(|(field, _)| field.clone()).collect(),
                    ),
                    alternatives: vec![Ctor {
                        name: CtorName::Tag(TagName("#Record".into())),
                        tag_id,
                        arity: 0,
                    }],
                };

                build_args_opts(
                    ctx,
                    fields.iter().map(|(_, var)| *var.as_inner()),
                    |fields| SketchedPattern::KnownCtor(union.clone(), tag_id, fields),
                )
                .collect()
            }
            FlatType::TagUnion(tags, ext) => {
                let TagUnionStructure { fields, ext: _ } =
                    match roc_types::types::gather_tags(ctx.subs, tags, ext) {
                        Ok(tag_structure) => tag_structure,
                        Err(_) => return vec![SketchedPattern::Anything],
                    };

                fields
                    .into_iter()
                    .flat_map(|(tag, args)| {
                        build_args_opts(ctx, args.iter().copied(), move |args| {
                            SketchedPattern::Ctor(var, tag.clone(), args)
                        })
                    })
                    .collect()
            }
            FlatType::RecursiveTagUnion(rec, tags, ext) => {
                if ctx.seen_recursion_vars.contains(&rec) {
                    return vec![SketchedPattern::Anything];
                }

                ctx.seen_recursion_vars.insert(rec);

                let TagUnionStructure { fields, ext: _ } =
                    match roc_types::types::gather_tags(ctx.subs, tags, ext) {
                        Ok(tag_structure) => tag_structure,
                        Err(_) => return vec![SketchedPattern::Anything],
                    };

                fields
                    .into_iter()
                    .flat_map(|(tag, args)| {
                        build_args_opts(ctx, args.iter().copied(), move |args| {
                            SketchedPattern::Ctor(var, tag.clone(), args)
                        })
                    })
                    .collect()
            }
            FlatType::FunctionOrTagUnion(tag, _, ext) => {
                let ctor_pattern = SketchedPattern::Ctor(var, ctx.subs[tag].clone(), vec![]);
                match ctx.subs.get_content_without_compacting(ext) {
                    Content::Structure(FlatType::TagUnion(tags, _)) if tags.is_empty() => {
                        vec![ctor_pattern, SketchedPattern::Anything]
                    }
                    Content::Structure(FlatType::EmptyTagUnion) => {
                        vec![ctor_pattern, SketchedPattern::Anything]
                    }
                    _ => vec![ctor_pattern],
                }
            }
            FlatType::EmptyRecord => {
                let tag_id = TagId(0);
                let union = Union {
                    render_as: RenderAs::Record(vec![]),
                    alternatives: vec![Ctor {
                        name: CtorName::Tag(TagName("#Record".into())),
                        tag_id,
                        arity: 0,
                    }],
                };
                vec![SketchedPattern::KnownCtor(union, tag_id, vec![])]
            }
            FlatType::Erroneous(_) => vec![SketchedPattern::Anything],
            FlatType::EmptyTagUnion => internal_error!("unreachable"),
        },
        Content::LambdaSet(_) => internal_error!("unreachable"),
    }
}

#[cfg(test)]
mod test {
    use roc_module::symbol::{IdentIds, Interns, ModuleIds};
    use roc_region::all::Region;
    use roc_types::{
        subs::{Content, Subs, Variable},
        synth::synth_var,
        v,
    };

    use super::sketch_variable;

    fn print_variable_sketch(synth: impl FnOnce(&mut Subs) -> Variable) -> String {
        let mut subs = Subs::default();
        let var = synth(&mut subs);
        let rows = sketch_variable(&subs, var, Region::zero());
        let interns = Interns {
            module_ids: ModuleIds::default(),
            all_ident_ids: IdentIds::exposed_builtins(0),
        };
        rows.to_doc(&interns)
    }

    #[test]
    fn sketch_flex_var() {
        insta::assert_snapshot!(
            print_variable_sketch(|subs: &mut Subs| synth_var(subs, Content::FlexVar(None))),
            @"_"
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_tag_union() {
        insta::assert_snapshot!(
            print_variable_sketch(v!([ A v!(U8), B v!(U16) ])),
            @r###"
        A _
        B _
        "###
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_tag_union_with_flex_ext() {
        insta::assert_snapshot!(
            print_variable_sketch(v!([ A v!(U8), B v!(U16) ]*)),
            @r###"
        A _
        B _
        "###
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_tag_union_no_payloads() {
        insta::assert_snapshot!(
            print_variable_sketch(v!([ A, B ])),
            @r###"
        A
        B
        "###
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_tag_union_no_payloads_with_flex_ext() {
        insta::assert_snapshot!(
            print_variable_sketch(v!([ A, B ]*)),
            @r###"
        A
        B
        "###
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_tag_union_with_branching_payloads() {
        insta::assert_snapshot!(
            print_variable_sketch(v!([ A v!([B, C]) v!([D, E]), F v!([G, H, I]) ]*)),
            @r###"
        A B D
        A C D
        A B E
        A C E
        F G
        F H
        F I
        "###
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_recursive_tag_union() {
        insta::assert_snapshot!(
            print_variable_sketch(v!([Nil, Cons v!(U8) v!(^lst) ] as lst)),
            @r###"
        Cons _ _
        Nil 
        "###
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_record() {
        insta::assert_snapshot!(
            print_variable_sketch(v!({ a: v!(U8), b: v!(U16), })),
            @"{ a: _, b: _, }"
        );
    }

    #[test]
    #[allow(non_snake_case)]
    fn sketch_record_with_branching_value_types() {
        insta::assert_snapshot!(
            print_variable_sketch(v!({ a: v!([A v!([B, C]) v!([D, E])]), b: v!([F v!([G, H, I])]), })),
            @r###"
        { a: A B D, b: F G, }
        { a: A C D, b: F G, }
        { a: A B E, b: F G, }
        { a: A C E, b: F G, }
        { a: A B D, b: F H, }
        { a: A C D, b: F H, }
        { a: A B E, b: F H, }
        { a: A C E, b: F H, }
        { a: A B D, b: F I, }
        { a: A C D, b: F I, }
        { a: A B E, b: F I, }
        { a: A C E, b: F I, }
        "###
        );
    }
}
