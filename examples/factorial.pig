factorial = (n) => {
    if n == 0 then x = 1 else {
        y = 1;
        x = 1;
        while -(y > n) do {
            x = x * y;
            y = y + 1;
        };
    }
    return x;
}