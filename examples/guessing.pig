guessing = () => {
    print("think of a natural number between 0 and 100");
    h = 100;
    l = 0;
    while (ans != "yes") && (ans != "stop") do {
        if (c = even(h - l)/2) == 0 then c = 1;
        x = l + c;
        print("is your number equal ", x, "?");
        ans = read;
        if ans == "less" then h = x; 
        if ans == "more" then l = x;
    };
    if ans == "yes" then print("your number is ", x , "!");
};

even = (a) => {
    x = 0;
    if a > 0 
        then while x < a do x = x + 2 
        else while x > a do x = x - 2;
    if x > a then x - 2 else x;
};

guessing()