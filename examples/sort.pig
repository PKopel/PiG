sort = (a) => {
    if a then {
        p = >- a;
        ls = [];
        gt = [];
        while a do {
            c = >- a;
            if c > p then gt = gt # c else ls = ls # c;
        };
        x = sort(ls, l-1) # p # sort(gt, l-1); 
    } else x = [];
    return x;
};