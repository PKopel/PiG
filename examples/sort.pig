sort = (a) => {
    if a then {
        p = >- a;
        ls = [];
        gt = [];
        while a do {
            c = >- a;
            if c > p then gt = gt # c else ls = ls # c;
        };
        x = sort(ls) # p # sort(gt); 
    } else x = [];
    return x;
};