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
                    slots: Some(Vec::new()),
                    curr: 0
                }
            }
        }
    }
    
    let mut map = HashMap::new();
    
    macro_rules! arith {
        ($x: literal, $v: expr) => {
            map.insert($x.to_string(), action!("block", "set_var", $x, $v, vec![]));
        };
    }

    macro_rules! var {
        ($x: literal, $y: literal, $v: expr) => {
            map.insert($x.to_string(), action!("block", "set_var", $y, $v, vec![]));
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

    macro_rules! repeat {
        ($x: literal, $y: literal) => {
            map.insert($x.to_string(), action!("block", "repeat", $y, false, vec![]));
        };
    }

    arith!("+", false);
    arith!("-", false);
    arith!("*", false);
    arith!("/", false);
    arith!("%", false);

    arith!("=", true);
    arith!("+=", true);
    arith!("-=", true);

    var!("assign", "=", true);

    var!("create_list", "CreateList", false);
    var!("push", "AppendValue", true);

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

    repeat!("?Forever", "Forever");
    repeat!("?Range", "Range");
    repeat!("?ForEach", "ForEach");
    repeat!("?ForEachEntry", "ForEachEntry");
    repeat!("?Adjacent", "Adjacent");
    repeat!("?Grid", "Grid");
    repeat!("?Path", "Path");
    repeat!("?Sphere", "Sphere");

    map
}