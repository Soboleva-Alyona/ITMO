"use strict";

//cnst, variable, add, subtract, multiply, divide, negate

const apply = f => args => f(...args);

const someOperation = operation =>
    (...args) =>
        // :NOTE: плохой тон использовать arguments - вроде DONE
        (...params) => {
            let vals = [];
            for (let i = 0; i < args.length; i++) {
                vals.push(args[i](...params));
            }
            return apply(operation)(vals);
        }


const add = someOperation((a, b) => a + b);
const subtract = someOperation((a, b) => a - b);
const multiply = someOperation((a, b) => a * b);
const divide = someOperation((a, b) => a / b);
const negate = someOperation((a) => -a);

const variables = {
    'x': 0,
    'y': 1,
    'z': 2
};

const cnst = value => () => value;
const variable = (symbol) => {
    return function () {
        return arguments[variables[symbol]];
    }
}

const one = cnst(1);
const two = cnst(2);

const constants = {
    'one': one,
    'two': two
}

// :NOTE: обобщить с бинарными операциями - вроде тоже DONE
const max3 = someOperation((...operands) => {
    return Math.max(...operands);
});

const min5 = someOperation((...operands) => {
    return Math.min(...operands);

});

const operations = {
    '+': add,
    '-': subtract,
    '*': multiply,
    '/': divide,
    'negate': negate,
    'max3': max3,
    'min5': min5
};

const arn = {
    '+': 2,
    '-': 2,
    '/': 2,
    '*': 2,
    'negate': 1,
    'max3': 3,
    'min5': 5
}

const parse = function (str) {
    let s = str.split(' ');
    let args = s.filter(function (len) {
        return len.length > 0;
    });

    let nums = [];
    for (let i = 0; i < args.length; i++) {
        if (args[i] in variables) {
            nums.push(variable(args[i]));
            // :NOTE: Array.slice Array.splice
        } else if (args[i] in operations) {
            let arr = [];
            for (let j = 0; j < arn[args[i]]; j++) {
                arr[j] = nums.pop();
            }
            arr.reverse();

            nums.push(operations[args[i]](...arr));
        } else if (args[i] in constants) {
            nums.push(constants[args[i]]);
        } else {
            nums.push(cnst(Number(args[i])));
        }
    }
    return nums.pop();
}


