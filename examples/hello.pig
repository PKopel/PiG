#! ./pig -l

hello = () =>{
    print("What is your name?\n");
    name = read();
    print("Hello, ",name,'\n');
};

hello();

return 0;