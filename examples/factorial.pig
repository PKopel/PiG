factorial = (n) => {
    y = x = 1;
    while ~(y > n) do {
        x = x * y;
        y = y + 1;
    }
    x;
}