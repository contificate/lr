

Split(['#left', '#middle', '#right'], {
    sizes: [20, 50, 30]
});

Split(['#mid-1', '#mid-2'], {
    direction: 'vertical',
    sizes: [60, 40]
});

// hacky globals
var gid = 0;
var actionsTbl;
var gotoTbl;

// store alternative in case of conflicting action entry
var alts = {};
function updateAlt(key, value) {
    alts[key] = unescape(value);
    parseInput();
};

function parseAction(obj, key) {
    if (obj == undefined) return '';
    if (obj.s !== undefined) { return 's' + obj.s; }
    if (obj.r !== undefined) {
	let r = [...obj.r];
	let s = r[0];
	s += ' → ';
	r.shift();
	r.forEach(sym => {
	    s += sym + ' ';
	});
	return s;
    }

    if (obj.c !== undefined) {
	let combo = '';
	let unique = Math.random();
	combo += '<div class="alt">';
	
	for (let i = 0; i < obj.c.length; i++) {
	    let ch = obj.c[i];
	    let first = (i == 0);
	    let encoded = escape(JSON.stringify(ch));
	    if (first) {
		updateAlt(key, encoded);
	    }
	    combo += '<input onchange="updateAlt(\'' + key + '\', \'' + escape(JSON.stringify(ch)) + '\')" id="' + gid + '" type="radio" id="" name="' + unique +'" ' + (first ? 'checked' : '') + ' />';
	    combo += '<label for="' + gid + '">' + parseAction(ch) + '</label>';
	    gid++;
	}

	combo += '</div>';
	return combo;
    }

    
    return 'accept';
};

function parseGoto(obj) {
    if (obj == undefined) return '';
    return obj;
};

function populateTable(result) {
    let actions = {};
    let goto = {};

    result.action.forEach(act => {
	actions[[act[0], act[1]]] = act[2];
    });


    result.goto.forEach(go => {
	goto[[go[0], go[1]]] = go[2];
    });

    console.log(actions);
    console.log(goto);

    let tbl = document.getElementById("tbl");
    tbl.innerHTML = '';

    let terminals = result.terminals;
    terminals.push("$");

    let nonterminals = result.nonterminals;

    let entries = '<tr><th colspan="1">#</th><th colspan="' + terminals.length +'">ACTION</th><th colspan="' + nonterminals.length + '">GOTO</th></tr>';
    entries += '<tr>'
    entries += '<th></th>';
    terminals.forEach(t => entries += '<th>' + t + '</th>');
    nonterminals.forEach(t => entries += '<th>' + t + '</th>');
    entries += '</tr>';

    for (let i = 0; i < result.count; i++) {
	entries += '<tr>';

	// state number
	entries += '<td>' + i + '</td>';

	terminals.forEach(t => {
	    entries += '<td>' + parseAction(actions[[i,t]], [i,t])  + '</td>';
	});

	nonterminals.forEach(t => {
	    entries += '<td>' + parseGoto(goto[[i,t]]) + '</td>';
	});

	entries += '</tr>';
    }
    
    tbl.innerHTML = entries;
    actionsTbl = actions;
    gotoTbl = goto;
};


function parseGrammar() {
    let viz = new Viz();
    let bnf = document.getElementById("grammar").value;
    let result = JSON.parse(lr.construct(bnf));
    populateTable(result);
    viz.renderSVGElement(result.automaton).then(function(svg) {
	console.log(svg);
	document.getElementById("automaton").innerHTML = ''; // lol
	document.getElementById("automaton").appendChild(svg);
	svgPanZoom(svg);

    }).catch(e => alert("dot: " + e));
};

var fresh = 0;
var nodes = [];
var arrows = new Map();

function constructTree(arr) {
    let head = arr.shift();

    let x = 'x' + fresh++;
    nodes.push({ name: x, label: String(head) });

    let lhs = arr.map(a => {
	if (typeof(a) == 'object') {
	    let i = constructTree(a);
	    return i;
	}
	
	let f = 'x' + fresh++;
	nodes.push({ name: f, label: String(a) });
	return f;
    });
    
    lhs.forEach(l => {
	if (arrows.get(x) !== undefined) {
	    let current = arrows.get(x);
	    current.push(l);
	    arrows.set(x, current);
	} else {
	    arrows.set(x, [l]);
	}

    });

    return x;
}

function drawParseTree(result) {
    fresh = 0;
    nodes = [];
    arrows = new Map();

    let tree = constructTree(result);
    let g = 'digraph { node [shape=circle]; bgcolor = "#f9f9f9";';
    nodes.forEach(n => {
	g += n.name + '[label="' + n.label + '"];';
    });
    
    arrows.forEach((v, k) => {
	if (v.length > 1) {
	    g += "subgraph { { rank = same " + v.join(' ')  + " } " + v.join(' -> ') + " [color=invis, arrowhead=none]; }";
	    v.forEach(vp => { g += k + ' -> ' + vp + ';'; });
	} else {
	    g += k + ' -> ' + v + ';';
	}
    });

    g += '}';

    let viz = new Viz();
    viz.renderSVGElement(g).then(function(svg) {
	console.log(svg);
	document.getElementById("tree").innerHTML = ''; // lol
	document.getElementById("tree").appendChild(svg);
	svgPanZoom(svg);
    }).catch(e => alert("dot: " + e));
};

function parseInput() {
    // ensure tables are constructed
    if (actionsTbl === undefined || gotoTbl === undefined) return;

    // split and trim user's input
    let input = document.getElementById("src").value;
    let stream = input.split(' ').map(t => t.trim()).filter(t => t !== '');

    // create a polling lexer
    let i = 0;
    let lex = function() {
	return (i >= stream.length) ? "$" : stream[i];
    };

    // initial state
    let stack = [0];

    while (true) {
	// get next token and current state
	let current = lex();
	let state = stack[stack.length - 1];

	// check alternative table (user-defined overload)
	let key = [state, current];
	let action = alts[key];
	if (action !== undefined)
	    action = JSON.parse(action);

	// if no alternative, check the action table
	if (action === undefined)
	    action = actionsTbl[key];

	// accept
	if (action == "a") {
	    stack.pop();
	    drawParseTree(stack.pop());
	    break;
	}

	// shift token onto stack
	if (action.s !== undefined) {
	    stack.push(current);
	    stack.push(action.s);
	    i++;
	    continue;
	}

	// reduce
	if (action.r !== undefined) {
	    let rule = action.r;
	    let arity = rule.length - 1;

	    // E -> xs..., get xs
	    let body = [];
	    
	    for (let i = 0; i < arity; i++) {
		stack.pop();
		// get symbol
		let s = stack.pop();
		let tree = s;
		body.unshift(tree);
	    }

	    body.unshift(rule[0]);

	    // state for goto
	    let st = stack[stack.length - 1];
	    stack.push(body);
	    stack.push(gotoTbl[[st, rule[0]]]);
	    continue;
	}
	

	if (current == '$') break;
	
    }    
    
}
