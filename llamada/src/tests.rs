macro_rules! tests {
    ($ty: ty) => {
        #[test]
        fn id_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let prev = expr.get_last_id();
            let abs = expr.add(Term::abs(prev));
            *expr.root_mut() = (abs);
            expr.set_print_meta(false);
            assert_eq!(format!("{}", expr), "(a => a)");
            expr.reduce();
            assert_eq!(format!("{}", expr), "(a => a)");
            //assert_eq!(
            //format!("{:?}", &expr),
            //"DenseRepr { terms: [(Var(1), Empty), (Abs(0), Empty)], root: 1, print_meta: false }"
            //);
            expr.set_print_meta(true);
            //assert_eq!(
            //format!("{:?}", &expr),
            //"DenseRepr { terms: [(Var(1), Empty), (Abs(0), Empty)], root: 1, print_meta: true }"
            //);
            assert_eq!(format!("{}", &expr), "(a => a: Empty): Empty");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => a: Empty): Empty");
        }

        #[test]
        fn true_expr() {
            let mut expr = <$ty>::new(Term::Var(2), Empty);
            let prev = expr.get_last_id();
            let abs1 = expr.add(Term::abs(prev));
            let abs2 = expr.add(Term::abs(abs1));
            *expr.root_mut() = (abs2);
            assert_eq!(format!("{}", &expr), "(a => (b => a))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => (b => a))");
        }

        #[test]
        fn false_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let prev = expr.get_last_id();
            let abs1 = expr.add(Term::abs(prev));
            let abs2 = expr.add(Term::abs(abs1));
            *expr.root_mut() = (abs2);
            assert_eq!(format!("{}", &expr), "(a => (b => b))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => (b => b))");
        }

        #[test]
        fn id_a_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let prev = expr.get_last_id();
            let abs1 = expr.add(Term::abs(prev));
            let a = expr.add(Term::Var(1));
            let app1 = expr.add(Term::App(abs1, a));
            let abs2 = expr.add(Term::abs(app1));
            *expr.root_mut() = (abs2);
            assert_eq!(format!("{}", &expr), "(a => ((b => b) a))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => a)");
        }

        #[test]
        fn not_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let true_case = expr.get_last_id();
            let false_case = expr.add(Term::Var(2));
            let cond_case = expr.add(Term::Var(3));
            let app1 = expr.add(Term::App(cond_case, false_case));
            let app2 = expr.add(Term::App(app1, true_case));
            let abs1 = expr.add(Term::abs(app2));
            let abs2 = expr.add(Term::abs(abs1));
            let abs3 = expr.add(Term::abs(abs2));
            *expr.root_mut() = (abs3);
            assert_eq!(format!("{}", &expr), "(a => (b => (c => ((a b) c))))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => (b => (c => ((a b) c))))");
        }

        #[test]
        fn not_true_false_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let inner = {
                let true_case = expr.get_last_id();
                let false_case = expr.add(Term::Var(2));
                let cond_case = expr.add(Term::Var(3));
                let app1 = expr.add(Term::App(cond_case, false_case));
                let app2 = expr.add(Term::App(app1, true_case));
                let abs1 = expr.add(Term::abs(app2));
                let abs2 = expr.add(Term::abs(abs1));
                expr.add(Term::abs(abs2))
            };
            let _true_v = {
                let true_case = expr.add(Term::Var(2));
                let abs1 = expr.add(Term::abs(true_case));
                expr.add(Term::abs(abs1))
            };
            let false_v = {
                let false_case = expr.add(Term::Var(1));
                let abs1 = expr.add(Term::abs(false_case));
                expr.add(Term::abs(abs1))
            };
            let app1 = expr.add(Term::App(inner, false_v));
            *expr.root_mut() = (app1);
            assert_eq!(
                format!("{}", expr),
                "((a => (b => (c => ((a b) c)))) (a => (b => b)))"
            );
            expr.reduce();
            assert_eq!(format!("{}", expr), "(a => (b => b))");
        }

        #[test]
        fn zero_expr() {
            let mut church_test = <$ty>::new(Term::Var(1), Empty);
            let church = church_test.to_church(0);
            *church_test.root_mut() = (church);
            assert_eq!(format!("{}", &church_test), "(a => (b => b))");

            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let prev = expr.get_last_id();
            let abs1 = expr.add(Term::abs(prev));
            let abs2 = expr.add(Term::abs(abs1));
            *expr.root_mut() = (abs2);
            assert_eq!(format!("{}", &expr), "(a => (b => b))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => (b => b))");
            assert_eq!(expr.as_church(expr.root()), Some(0));
        }

        #[test]
        fn one_expr() {
            let mut church_test = <$ty>::new(Term::Var(1), Empty);
            let church = church_test.to_church(1);
            *church_test.root_mut() = (church);
            assert_eq!(format!("{}", &church_test), "(a => (b => (a b)))");

            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let prev = expr.get_last_id();
            let a = expr.add(Term::Var(2));
            let app1 = expr.add(Term::App(a, prev));
            let abs1 = expr.add(Term::abs(app1));
            let abs2 = expr.add(Term::abs(abs1));
            *expr.root_mut() = (abs2);
            assert_eq!(format!("{}", &expr), "(a => (b => (a b)))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => (b => (a b)))");
            assert_eq!(expr.as_church(expr.root()), Some(1));
        }

        #[test]
        fn two_expr() {
            let mut church_test = <$ty>::new(Term::Var(1), Empty);
            let church = church_test.to_church(2);
            *church_test.root_mut() = (church);
            assert_eq!(format!("{}", &church_test), "(a => (b => (a (a b))))");

            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let prev = expr.get_last_id();
            let a = expr.add(Term::Var(2));
            let app1 = expr.add(Term::App(a.clone(), prev));
            let app2 = expr.add(Term::App(a, app1));
            let abs1 = expr.add(Term::abs(app2));
            let abs2 = expr.add(Term::abs(abs1));
            *expr.root_mut() = (abs2);
            assert_eq!(format!("{}", &expr), "(a => (b => (a (a b))))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(a => (b => (a (a b))))");
            assert_eq!(expr.as_church(expr.root()), Some(2));
        }

        #[test]
        fn plus_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let x = expr.get_last_id();

            let f = expr.add(Term::Var(2));
            let m = expr.add(Term::Var(3));
            let n = expr.add(Term::Var(4));
            let nf = expr.add(Term::App(n.clone(), f.clone()));
            let mf = expr.add(Term::App(m.clone(), f.clone()));
            let mfx = expr.add(Term::App(mf, x.clone()));
            let nfmfx = expr.add(Term::App(nf.clone(), mfx.clone()));
            let abs1_nfmfx = expr.add(Term::abs(nfmfx));
            let abs2_nfmfx = expr.add(Term::abs(abs1_nfmfx));
            let abs3_nfmfx = expr.add(Term::abs(abs2_nfmfx));
            let plus = expr.add(Term::abs(abs3_nfmfx));
            *expr.root_mut() = (plus.clone());

            assert_eq!(
                format!("{}", &expr),
                "(a => (b => (c => (d => ((a c) ((b c) d))))))"
            );

            for n in 0..10 {
                for m in 0..10 {
                    let church_n = expr.to_church(n);
                    let church_m = expr.to_church(m);

                    let plus_m = expr.add(Term::App(plus.clone(), church_m));
                    let plus_n_m = expr.add(Term::App(plus_m, church_n));
                    *expr.root_mut() = (plus_n_m);
                    expr.reduce();
                    let result = expr.as_church(expr.root());
                    eprintln!("{n:?} + {m:?} = {result:?}");
                    assert_eq!(result, Some(n + m));
                }
            }
        }

        #[test]
        fn mul_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty);
            let x = expr.get_last_id();

            let f = expr.add(Term::Var(2));
            let m = expr.add(Term::Var(3));
            let n = expr.add(Term::Var(4));
            let mf = expr.add(Term::App(m.clone(), f.clone()));
            let nmf = expr.add(Term::App(n.clone(), mf));
            let nmfx = expr.add(Term::App(nmf, x.clone()));
            let abs1_nmfx = expr.add(Term::abs(nmfx));
            let abs2_nmfx = expr.add(Term::abs(abs1_nmfx));
            let abs3_nmfx = expr.add(Term::abs(abs2_nmfx));
            let mul = expr.add(Term::abs(abs3_nmfx));
            *expr.root_mut() = (mul.clone());

            assert_eq!(
                format!("{}", &expr),
                "(a => (b => (c => (d => ((a (b c)) d)))))"
            );

            for n in 0..10 {
                for m in 0..10 {
                    let church_n = expr.to_church(n);
                    let church_m = expr.to_church(m);

                    let mul_m = expr.add(Term::App(mul.clone(), church_m));
                    let mul_n_m = expr.add(Term::App(mul_m, church_n));
                    *expr.root_mut() = (mul_n_m);
                    expr.reduce();
                    let result = expr.as_church(expr.root());
                    eprintln!("{n:?} * {m:?} = {result:?}");
                    assert_eq!(result, Some(n * m));
                }
            }
        }
    };
}
