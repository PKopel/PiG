:load "ex2.pig";

fun = (a) => a + 1;

main = () => {
    x = 1;
    y = fun(x);
    z = fun2(x,y);
    t = fun3(x,y,z);
    x + y + z + t;
};

main();