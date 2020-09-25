factorial = (n) => {
    if n == 0 do x = 1 else {
        y = x = 1;
        while -(y > n) do {
            x = x * y;
            y = y + 1;
        };
    }
    x;
}