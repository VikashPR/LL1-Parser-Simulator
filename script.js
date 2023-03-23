// LL(1) parser code

let getStartSymbol = ([start, rules]) => start;

let getNonterminals = ([start, rules]) => 
	rules.reduce((a, [lhs, rhs]) => [...a, lhs], []);

let getInitFirstSets = (g) => 
	getNonterminals(g).reduce((m, nt) => m.set(nt, new Set()), new Map());

let getInitFollowSets = (g) => 
	getInitFirstSets(g).set(getStartSymbol(g), new Set(['eof']));

let computeFirstSet = (firstSet, [h, ...t]) => {
	let first = mapClone(firstSet);
	if (h == undefined) return new Set(['eps']);
	if (first.get(h)) {
		let h_first = first.get(h);
		if (h_first.has('eps')) {
			h_first.delete('eps');
			return new Set([...computeFirstSet(first, t), ...h_first]);
		} else return h_first;
	}
	return new Set([h]);
};

let recurseFirstSets = ([start, rules], firstSet, firstFunc) => {
	let first = mapClone(firstSet);
	return rules.reduce(
		(map, [lhs, rhs]) => map.set(
			lhs, new Set([...firstFunc(first, rhs), ...first.get(lhs)])),
		first
	);
};

let mapClone = (m) => {
	let clone = new Map();
	for (key of m.keys()) {
		clone.set(key, new Set(m.get(key)));
	}
	return clone;
};

let getFirstSets = (g, first, firstFunc) => {
	let setEquals = (s1, s2) => (s1.size == s2.size) && [...s1].every(v=>s2.has(v));
	let mapEquals = (m1, m2) => [...m1.keys()].every(key => setEquals(m1.get(key), m2.get(key)));
	let updated = recurseFirstSets(g, first, firstFunc);
	if (mapEquals(first, updated)) {
		return updated;
	} else {
		return getFirstSets(g, updated, firstFunc);
	}
};

let updateFollowSet = (firstSet, followSet, nt, [h, ...t]) => {
	// JS pass by reference, MUST deep clone params for immutability principal
	let first = mapClone(firstSet);
	let follow = mapClone(followSet);
	if (h == undefined) return follow;
	if (first.get(h)) {
		if (t.length > 0) {
			let tail_first = computeFirstSet(first, t);
			if (tail_first.has('eps')) {				
				tail_first.delete('eps');
				let updated = follow.set(h, new Set(
					[...follow.get(h), ...tail_first, ...follow.get(nt)]
				));
				return updateFollowSet(first, updated, nt, t);
			} else {
				let updated = follow.set(h, new Set(
					[...follow.get(h), ...tail_first]
				));
				return updateFollowSet(first, updated, nt, t);
			}
		}
		return follow.set(h, new Set(
			[...follow.get(h), ...follow.get(nt)]
		));		
	}
	return updateFollowSet(first, follow, nt, t);
};

let recurseFollowSets = ([start, rules], first, follow, followFunc) =>
rules.reduce((map, [lhs, rhs]) => followFunc(first, map, lhs, rhs), follow);

let getFollowSets = (g, first, follow, followFunc) => {
	let setEquals = (s1, s2) => 
		(s1.siz == s2.size) && [...s1].every(v=>s2.has(v));
	let mapEquals = (m1, m2) => 
		[...m1.keys()].every(key => setEquals(m1.get(key), m2.get(key)));
	let updated = recurseFollowSets(g, first, follow, followFunc);
	if (mapEquals(follow, updated)) 
		return getFollowSets(g, first, updated, followFunc);
	else 
		return updated;
};

let getPredictSets = ([start, rules], first, follow, firstFunc) =>
rules.reduce((l, [lhs, rhs]) => {
	let rhs_first = firstFunc(first, rhs);
	if (rhs_first.has('eps')) {
		rhs_first.delete('eps');
		return [
			[[lhs, rhs], new Set([...rhs_first, ...follow.get(lhs)])],
			...l
		];			
	} else {
		return [
			[[lhs, rhs], firstFunc(first, rhs)],
			...l
		]; 
	}
}, []);

let getPredictMap = predictSets => predictSets.reduce(
	(map, [[lhs, rhs], symbols]) => [...symbols].reduce((pmap, symbol) =>pmap.set(lhs+'|||'+symbol, rhs), map), 
	new Map()
);

let derive = (first, predict, [ph, ...pt], [ih, ...it]) => {
	if (ph == undefined) {
		if (ih == undefined) return true;
		return false;
	} else {
		if (first.get(ph)) {
			if (ih == undefined) return false;
			let expanded = predict.get(ph+'|||'+ih);
			if (expanded == undefined) return false;
			return derive(first, predict, [...expanded, ...pt], [ih, ...it]);
		} else {
			if (ph == ih) return derive(first, predict, pt, it);
			return false;
		}
	}
};

let tryDerive = (g, inputStr) => {
	let ifs = getInitFirstSets(g); 
	let fs = getFirstSets(g, ifs, computeFirstSet);
	let ifl = getInitFollowSets(g);
	let ls = getFollowSets(g, fs, ifl, updateFollowSet);
	let pset = getPredictSets(g, fs, ls, computeFirstSet);
	let pmap = getPredictMap(pset);
	return derive(fs, pmap, [getStartSymbol(g), "eof"], [...inputStr, 'eof']);
};

let deriveTree = (first, predict, symbol, input) => {
	if (first.get(symbol)) {
		let [ih, ...it] = input;
		if (ih == undefined) {
			throw new Error("Invalid input");	
		} else {
			let expanded = predict.get(symbol+'|||'+ih);
			if (expanded == undefined) {
				throw new Error("Invalid input");	
			} else {
				if (expanded.length == 0) {
					return [
						{ text: { name: symbol }, children: [ { text: {name: 'eps'} } ] }, 	
						input
					];
				} else {
					let [eh, ...et] = expanded;
					let createTree = ([subTree, input], symbol) => {
						let [childTree, newInput] = deriveTree(first, predict, symbol, input);
						return [[...subTree, childTree], newInput];
					};
					let [expandedTree, newInput] = expanded.reduce(
						createTree, [[], input]);
					return [
						{ text: {name: symbol}, children:  expandedTree },
						newInput
					];
				}
			}		
		}
	} else {
		let [ih, ...it] = input;
		if (ih == undefined) {
			throw new Error("Invalid input");	
		} else {
			return [{ text: { name: ih } }, it];
		}
	}	
};

let tryDeriveTree = (g, inputStr) => {
	let ifs = getInitFirstSets(g); 
	let fs = getFirstSets(g, ifs, computeFirstSet);
	let ifl = getInitFollowSets(g);
	let ls = getFollowSets(g, fs, ifl, updateFollowSet);
	let pset = getPredictSets(g, fs, ls, computeFirstSet);
	let pmap = getPredictMap(pset);
	let [tree, input] = deriveTree(fs, pmap, getStartSymbol(g), [...inputStr, 'eof']);
	return tree;
};

// UI Code

var rules = [
	{lhs: 'S', rhs: 'AB' },
	{lhs: 'A', rhs: 'aA' },
	{lhs: 'A', rhs: '' },
	{lhs: 'B', rhs: 'bB' },
	{lhs: 'B', rhs: '' }
];

var cases = [
	{ case: 'aaabb', result: '' }
];

var rules_tbl = new Tabulator('#rules_tbl', {
	layout: 'fitColumns',
	data: rules,
	reactiveData: true,
	addRowPos: 'bottom',
	columns:[
		{title: 'LHS', field: 'lhs', editor: true},
		{title: 'RHS', field: 'rhs', editor: true}
	],
});

var tests_tbl = new Tabulator('#tests_tbl', {
	layout: 'fitColumns',
	data: cases,
	reactiveData: true,
	addRowPos: 'bottom',
	columns:[
		{ title: 'Input String', field: 'case', editor:true },
		{ title: 'Deriving Result', field: 'result' }
	],
});

var sets_tbl = new Tabulator('#fsls_tbl', {
	layout: 'fitColumns',
	reactiveData: true,
	columns:[
		{ title: 'NT', field: 'nt' },
		{ title: 'First Set', field: 'first' },
		{ title: 'Follow Set', field: 'follow' }
	],
});

var ps_tbl = new Tabulator('#ps_tbl', {
	layout: 'fitColumns',
	reactiveData: true,
	columns:[
		{ title: 'Rule', field: 'rule' },
		{ title: 'Predict Set', field: 'predicts' }
	],
});

let getGrammar = () => {
	let g = [];
	let rows = rules_tbl.getRows();
	rows.forEach(r=> {
		if (r.getData().lhs != undefined) {
			if (r.getData().rhs == undefined)
				g.push([r.getData().lhs, []]);
			else
				g.push([r.getData().lhs, [...r.getData().rhs]]);
		}
	});
	return ['S', g];
};

let getTests = () => {
	let tests= [];
	for (r of tests_tbl.getRows()) {
		if (r.getData().case == undefined)
			tests.push({case: '', result: ''});
		else
			tests.push(r.getData());
	}
	return tests;
}

let generateSets = () => {
	let g = getGrammar();
	let ifs = getInitFirstSets(g); 
	let fs = getFirstSets(g, ifs, computeFirstSet);
	let ifl = getInitFollowSets(g);
	let ls = getFollowSets(g, fs, ifl, updateFollowSet);
	let pset = getPredictSets(g, fs, ls, computeFirstSet);

	let fsls_data = [];
	for (nt of fs.keys()) {
		fsls_data.push({nt: nt, first: [...fs.get(nt)].join(', '), follow: [...ls.get(nt)].join(', ')});
	}
	sets_tbl.setData(fsls_data);

	let ps_data = [];
	for (r of pset) {
		ps_data.push({rule: r[0][0]+" :== "+[...r[0][1]].join(''), predicts: [...r[1]].join(', ')});
	}
	ps_tbl.setData(ps_data);
};

function parse() {
	let g = getGrammar();
	let cases= getTests();
	for (let i=0; i<cases.length; i++) {
		cases[i].result = tryDerive(g, [...cases[i].case]);
	}
	tests_tbl.setData(cases);

	// generate tree graph ONLY when first test case is valid input
	let tree_config = {
		chart: {
			container: "#tree",
			levelSeparation: 20,
			siblingSeparation: 15,
			subTeeSeparation:  15,
			rootOrientation: "NORTH"
		},
		nodeStructure: {}
	};	
	if (cases[0].result) {
		tree_config.nodeStructure = 
			tryDeriveTree(g, [...cases[0].case]);
	} else {
		tree_config.nodeStructure = {};		
	}
	// draw tree
	new Treant( tree_config );			
};

let addRule = () => { rules_tbl.addRow(); };
let addTest = () => { tests_tbl.addRow(); };

generateSets();
parse();
setTimeout(function(){ parse(); },5000);