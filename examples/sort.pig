length = (a) => {
    i = 0;
    while a: {
        i = i + 1;
        >- a;
    }
    i
};

quick = (a) => {
    if a: {
        p = >- a;
        ls = gt = [];
        while a: {
            c = >- a;
            if c > p: gt = gt <> c else: ls = ls <> c;
        }
        x = quick(ls) <> p <> quick(gt); 
    } else: x = [];
    x
};

bubble = (a) => {
    n = length(a);
    i = 0;
    while i < n: {
        j = 0;
        while j < (n-i-1): {
            p = a(j);
            if p > a(j+1): {
                a(j) = a(j+1);
                a(j+1) = p;
            }
            j = j + 1;
        }
        i = i + 1;
    };
    a
}