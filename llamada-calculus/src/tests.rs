macro_rules! tests {
    ($ty: ty) => {
        #[test]
        fn id_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty {});
            let prev = expr.get_last_id();
            let abs = expr.push(Term::Abs(prev), Empty {});
            expr.set_root(abs);
            expr.print_meta = false;
            assert_eq!(format!("{}", expr), "(\\a. a)");
            expr.reduce();
            assert_eq!(format!("{}", expr), "(\\a. a)");
            //assert_eq!(
                //format!("{:?}", &expr),
                //"DenseRepr { terms: [(Var(1), Empty), (Abs(0), Empty)], root: 1, print_meta: false }"
            //);
            expr.print_meta = true;
            //assert_eq!(
                //format!("{:?}", &expr),
                //"DenseRepr { terms: [(Var(1), Empty), (Abs(0), Empty)], root: 1, print_meta: true }"
            //);
            assert_eq!(format!("{}", &expr), "(\\a. a: Empty): Empty");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(\\a. a: Empty): Empty");
        }

        #[test]
        fn true_expr() {
            let mut expr = <$ty>::new(Term::Var(2), Empty {});
            let prev = expr.get_last_id();
            let abs1 = expr.push(Term::Abs(prev), Empty {});
            let abs2 = expr.push(Term::Abs(abs1), Empty {});
            expr.set_root(abs2);
            assert_eq!(format!("{}", &expr), "(\\a. (\\b. a))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(\\a. (\\b. a))");
        }

        #[test]
        fn false_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty {});
            let prev = expr.get_last_id();
            let abs1 = expr.push(Term::Abs(prev), Empty {});
            let abs2 = expr.push(Term::Abs(abs1), Empty {});
            expr.set_root(abs2);
            assert_eq!(format!("{}", &expr), "(\\a. (\\b. b))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(\\a. (\\b. b))");
        }

        #[test]
        fn not_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty {});
            let true_case = expr.get_last_id();
            let false_case = expr.push(Term::Var(2), Empty {});
            let cond_case = expr.push(Term::Var(3), Empty {});
            let app1 = expr.push(Term::App(cond_case, false_case), Empty {});
            let app2 = expr.push(Term::App(app1, true_case), Empty {});
            let abs1 = expr.push(Term::Abs(app2), Empty {});
            let abs2 = expr.push(Term::Abs(abs1), Empty {});
            let abs3 = expr.push(Term::Abs(abs2), Empty {});
            expr.set_root(abs3);
            assert_eq!(format!("{}", &expr), "(\\a. (\\b. (\\c. a b c)))");
            expr.reduce();
            assert_eq!(format!("{}", &expr), "(\\a. (\\b. (\\c. a b c)))");
        }

        #[test]
        fn not_true_false_expr() {
            let mut expr = <$ty>::new(Term::Var(1), Empty {});
            let inner = {
                let true_case = expr.get_last_id();
                let false_case = expr.push(Term::Var(2), Empty {});
                let cond_case = expr.push(Term::Var(3), Empty {});
                let app1 = expr.push(Term::App(cond_case, false_case), Empty {});
                let app2 = expr.push(Term::App(app1, true_case), Empty {});
                let abs1 = expr.push(Term::Abs(app2), Empty {});
                let abs2 = expr.push(Term::Abs(abs1), Empty {});
                expr.push(Term::Abs(abs2), Empty {})
            };
            let _true_v = {
                let true_case = expr.push(Term::Var(2), Empty {});
                let abs1 = expr.push(Term::Abs(true_case), Empty {});
                expr.push(Term::Abs(abs1), Empty {})
            };
            let false_v = {
                let false_case = expr.push(Term::Var(1), Empty {});
                let abs1 = expr.push(Term::Abs(false_case), Empty {});
                expr.push(Term::Abs(abs1), Empty {})
            };
            let app1 = expr.push(Term::App(inner, false_v), Empty {});
            expr.set_root(app1);
            assert_eq!(format!("{}", expr), "(\\a. (\\b. (\\c. a b c))) (\\a. (\\b. b))");
            expr.reduce();
            assert_eq!(format!("{}", expr), "(\\a. (\\b. b))");
        }
    }
}
