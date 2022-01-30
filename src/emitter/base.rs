{
    macro_rules! action {
        (
            $id: literal,
            $block: literal,
            $action: literal,
            $void: literal,
            $args: expr
        ) => {
            Action {
                id: $id.to_string(),
                block: $block.to_string(),
                action: $action.to_string(),
                extra: HashMap::new(),
                void: $void,
                args: Items {
                    items: $args,
                    extra: HashMap::new(),
                    slots: Vec::new(),
                    curr: 0
                }
            }
        }
    }
    
    let mut map = HashMap::new();
    
    macro_rules! arith {
        ($x: literal) => {
            map.insert($x.to_string(), action!("block", "set_var", $x, false, vec![]));
        };
    }

    macro_rules! ctrl {
        ($x: literal, $y: literal) => {
            map.insert($x.to_string(), action!("block", "control", $y, false, vec![]));
        };
    }
    
    macro_rules! comp {
        ("==") => {
            map.insert("==".to_string(), action!("block", "if_var", "=", false, vec![]));
        };
        ($x: literal) => {
            map.insert($x.to_string(), action!("block", "if_var", $x, false, vec![]));
        };
    }

    arith!("+");
    arith!("-");
    arith!("*");
    arith!("/");
    arith!("%");
    arith!("=");
    arith!("+=");
    arith!("-=");

    ctrl!("?ReturnNTimes", "ReturnNTimes");
    ctrl!("?SkipIteration", "SkipIteration");
    ctrl!("?StopRepeat", "StopRepeat");
    ctrl!("?End", "End");
    ctrl!("?Wait", "Wait");

    comp!("==");
    comp!("!=");
    comp!(">");
    comp!("<");
    comp!(">=");
    comp!("<=");

    map
}