guessing = () => {
    print("think of a natural number between 0 and 100\n");
    h = 100;
    l = 0;
    while (ans != "yes") && (ans != "stop") do {
        if (c = even(h - l)/2) == 0 do c = 1;
        x = l + c;
        print("is your number equal ", x, "?\n");
        ans = read();
        if ans == "less" do h = x
        elif ans == "more" do l = x
        elif ans == "yes" do print("your number is ", x , "!\n");
    };
};

even = (a) => {
    if a % 2 == 0 do a 
    else a - 1;
};

guessing();

exit();