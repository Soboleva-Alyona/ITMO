"use strict";

// Const, Variable, Add, Subtract, Multiply, Divide, Negate
//+ diff ("x")

const operations = {};

function AbstractOperation(...args) {
    this.args = args;
}

AbstractOperation.prototype = {
	evaluate: function (...values) {
		return this.someOp(...this.args.map(o => o.evaluate(...values)));
	},
	diff: function (x) {
		return this.d(x, ...this.args);
	},
	toString: function () {
		return this.args.map(o => o.toString()).join(" ") + " " + this.sign;
	}
};

function operationFactory(sign, someOp, d) {
	let res = function(...args) {
		AbstractOperation.call(this, ...args);
	};
	res.prototype = Object.create(AbstractOperation.prototype);
	res.prototype.sign = sign;
	res.prototype.someOp = someOp;
	res.prototype.d = d;
	res.arity = someOp.length;
	operations[sign] = res;
	
	return res;
}


//const
function Const(val) {
	this.val = val;
}

Const.prototype.evaluate = function() {
	return this.val;
};
Const.prototype.diff = () => new Const(0);

Const.prototype.toString = function () {
	return this.val.toString();
};


const names = [ "x", 
				"y", 
				"z"];

function Variable(perem) {
    this.perem = perem;
    this.num = names.indexOf(perem);
}

Variable.prototype.evaluate = function (...values) {
	return values[this.num]
};
Variable.prototype.diff = function (x) {
	// :NOTE: new Const(1)
	return x === this.perem ? new Const(1) : new Const(0)
};
Variable.prototype.toString = function () {
	return this.perem;
};

//neg

const Negate = operationFactory(
	"negate",
	a => -a,
	(x, expression) => new Negate(expression.diff(x))
);

//add

const Add = operationFactory(
	"+",
	(a, b) => a + b,
	// :NOTE: .diff(x)
	(x, a, b) => new Add(a.diff(x), b.diff(x))
);

//sub

const Subtract = operationFactory(
	"-",
	(a, b) => a - b,
	(x, a, b) => new Subtract(a.diff(x), b.diff(x))
);


//mult

const Multiply = operationFactory(
	"*",
	function (a, b) {
		return a * b;
	},
	function (x, a, b) {
		return 	new Add(new Multiply(a.diff(x), b), new Multiply(a, b.diff(x)));
	}
);

//div

const Divide = operationFactory (
	"/",
	function (a, b) {
		return a / b;
	},
	function (x, a, b) {
		return new Divide(new Subtract(
			new Multiply(a.diff(x), b), new Multiply(a, b.diff(x))),
			new Multiply(b, b));
    }
);

//parser

const parse = function (str) {
    let s = str.split(' ');
    let args = s.filter(function (len) {
        return len.length > 0;
    });

    let nums = [];
    for (let i = 0; i < args.length; i++) {
        if (args[i] in operations) {
			let Operation = operations[args[i]];
            nums.push(new Operation(...nums.splice(-Operation.arity)));
        } else if (names.includes(args[i])) {
            nums.push(new Variable(args[i]));
        } else {
            nums.push(new Const(parseInt(args[i])));
        }
    }
    return nums.pop();
};

const Cube = operationFactory(
	"cube", 
	(a) => Math.pow(a, 3),
	(x, a) => new Multiply(new Multiply(new Const(3), new Multiply(a, a)), a.diff(x))
);

const Cbrt = operationFactory(
	"cbrt",
	(a) => Math.cbrt(a),
	function (x, a) {
		return new Multiply(new Multiply(new Const(1/3), new Divide(new Const(1), new Multiply(new Cbrt(a), new Cbrt(a)))), a.diff(x));
	}
);
