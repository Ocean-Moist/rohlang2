# rohlang2

## a statically typed pure functional language. 

type Animal {
    dog,
    cat,
    monkey,
}

type Sound {
    bark,
    meow,
    growl,
}

fn a2s[Animal -> Sound](x) {
    dog => bark,
    cat => meow,
    monkey => growl
}

