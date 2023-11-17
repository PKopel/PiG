guessing = () => {
    print("think of a natural number between 0 and 100\n");
    print("answer 'less', 'more', 'yes' or 'stop'\n");
    h = 100;
    l = 0;
    while (ans != "stop"): {
        if (c = even(h - l)/2) == 0: c = 1;
        x = l + c;
        print("is your number equal ", x, "?\n");
        ans = read();
        if ans == "less": h = x
        elif ans == "more": l = x
        elif ans == "yes": { 
            print("your number is ", x , "!\n");
            return;
        }
        elif ans != "stop": print("answer 'less', 'more', 'yes' or 'stop'\n");
    }
};

even = (a) => {
    if a % 2 == 0: a
    else: a - 1;
};

guessing();

return 0;