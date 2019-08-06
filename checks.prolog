:- module(checks,
    [ unflexible_type/1
    , unflexible_instance/1
    , no_unbound_variables/1
    , mentioned_classes_exist/2
    , class_is_declared_once/2
    , unflexible_constraint/1
    , well_formed_class_decl/1
    , superclass_instances_exist/3

    , check_file/2
    ]).

:- use_module(parse).
:- use_module(syntax).
:- use_module(resolution).

%! unflexible_type(Ty).
%
% True if `Ty` is a type of the form `C $ t_1 $ ... $ t_n`, where
% `C` is a concrete type, and each `t_i` is a distinct type variable.
unflexible_type(Ty) :-
    application_to_list(Ty, [ty(_) | Args]),
    maplist([tyvar(_)] >> true, Args),
    is_set(Args).

unflexible_instance(instance(_, _ @ Type, _)) :-
    unflexible_type(Type).

%! no_unbound_tyvars(Declaration).
%
% True if all variables in the context are bound in the head, i.e. all occur
% in the head.
no_unbound_variables(Decl) :-
    (Decl = instance(Ctx, Head, _) ; Decl = class(Ctx, Head, _)),
    tyvars(Ctx, CVars),
    tyvars(Head, HVars),
    subset(CVars, HVars).

%! mentioned_classes_exist(Classes, Declaration).
%
% True if all classes mentioned in the head or context of `Declaration` are
% defined in `Classes`.
mentioned_classes_exist(Classes, instance(Ctx, Class @ _, _)) :-
    member(class(_, Class @ _, _), Classes),
    maplist({Classes}/[C @ _] >> member(class(_, C @ _, _), Classes), Ctx).
mentioned_classes_exist(Classes, class(Ctx, _ @ _, _)) :-
    maplist({Classes}/[C @ _] >> member(class(_, C @ _, _), Classes), Ctx).

%! class_is_declared_once(Classes, ClassDecl).
%
% True if `ClassDecl` declares a class that is not declared in `Classes`.
class_is_declared_once(Classes, ClassDecl) :-
    ClassDecl = class(_, Class @ _, _),
    nth0(_, Classes, ClassDecl, Rest),
    \+ member(class(_, Class @ _, _), Rest).

%! unflexible_constraint(Constraint).
%
% True if `Constraint` is of the form `_ @ tyvar(_)`.
unflexible_constraint(_ @ tyvar(_)).

%! well_formed_class_decl(ClassDecl).
well_formed_class_decl(class(Ctx, _ @ tyvar(_), _)) :-
    maplist(unflexible_constraint, Ctx).

%! superclass_instances_exist(Clases, Instances, Instance).
%
% True if `Instances` declares an instance for `Class @ Type` such that
% there is an instance `SClass @ Type` for every superclass of `Class`.
superclass_instances_exist(Classes, Instances, instance(_, Class @ Type, _)) :-
    member(class(Superclasses, Class @ _, _), Classes),
    maplist([class(_, SClass @ _, _)] >> has_instance(Instances, SClass @ Type), Superclasses).

check_file(File, Check) :-
    parse_file(File, mod(Cs, Is)),
    call(Check, Cs, Is).
