clojure = {
  JS: {
    global: this,
    merge: function( t, s ) {
      for( var i in s ) {
        t[ i ] = s[ i ];
      }
      return t;
    }
  },
  lang: {
    Namespace: function( m ) { clojure.JS.merge( this, m || {} ); }
  }
};

clojure = new clojure.lang.Namespace({
  in_ns: function(s) {
    var nsparts = s.substring(1).split('.');
    var base = clojure.JS.global;
    for( var i = 0; i < nsparts.length; ++i ) {
      if( ! base[nsparts[i]] ) {
        base[nsparts[i]] = new clojure.lang.Namespace();
      }
      base = base[nsparts[i]];
    }
  },
  refer: function(s) {},
  seq: function(coll){
    if( coll === null ) return null;
    else if( coll.seq ) return coll.seq();
    //else if( coll.constructor === String )
    //  return clojure.lang.StringSeq.create(coll);
    else if( typeof coll.length == typeof 0 )
      return clojure.lang.ArraySeq.create(coll);
    else if( typeof coll === typeof {} )
      return clojure.JS.ObjSeq.create(coll);
    throw ("Don't know how to create ISeq from: " +
        (typeof coll) + " " + coll.constructor.name);
  },
  apply: function( f ) {
    if( f.isVariadic ) {
      // lazy
      var i, args = [];
      var eagercount = Math.min( f.arity, arguments.length - 2 );
      for( i = 0; i < eagercount; ++i ) {
        args.push( arguments[ i + 1 ] );
      }
      if( eagercount == f.arity ) {
        if( arguments.length - eagercount < 3 ) {
          args.push( clojure.seq( arguments[ arguments.length - 1 ] ) );
        }
        else {
          args.push( clojure.concat(
                  new clojure.lang.ArraySeq(
                      null, arguments, eagercount + 1, arguments.length - 1 ),
                  arguments[ arguments.length - 1 ] ) );
        }
      }
      else {
        var s = clojure.seq( arguments[ arguments.length - 1 ] );
        for( ; s && args.length < f.arity; s = s.rest() ) {
          args.push( s.first() );
        }
        if( s )
          args.push( s );
      }
      return f.apply( clojure.JS.variadic_sentinel, args );
    }
    else {
      // non-lazy
      var args = [];
      for( var i = 1; i < arguments.length - 1; ++i ) {
        args.push( arguments[ i ] );
      }
      for( var s = arguments[ arguments.length - 1]; s; s = s.rest()) {
        args.push( s.first() );
      }
      return f.apply( null, args );
    }
  },
  first: function(x) {
    if( x.first ) return x.first();
    var seq = clojure.seq( x );
    if( seq === null ) return null;
    return seq.first();
  },
  rest: function(x) {
    if( x.rest ) return x.rest();
    var seq = clojure.seq( x );
    if( seq === null ) return null;
    return seq.rest();
  },
  second: function(x) { return clojure.first(clojure.rest(x)); },
  instance_QMARK_: function( c, o ) {
    return clojure.JS.instanceq( c, o );
  },
  find: function(coll, key) {
    if( coll == null )
      return null;
    else if( clojure.JS.instanceq( java.util.Map, coll ) ) {
      if( coll.containsKey( key ) )
        return new clojure.lang.MapEntry( key, coll.get( key ) );
      return null;
    }
    return coll.entryAt( key );
  },
  get: function(coll, key, notFound ) {
    if( coll === null )
      return null;
    if( coll.valAt )
      return coll.valAt( key, notFound );
    if( coll.containsKey ) {
      if( notFound === undefined || coll.containsKey( key ) )
        return coll.get( key );
      return notFound;
    }
    if( coll.contains ) {
      if( notFound === undefined || coll.contains( key ) )
        return coll.get( key );
      return notFound;
    }
    if( notFound === undefined || key in coll )
      return coll[ key ];
    return notFound;
  },
  contains_QMARK_: function(coll, key) {
    if( coll === null )
      return false;
    if( coll.containsKey )
      return coll.containsKey( key ) ? true : false;
    if( coll.contains )
      return coll.contains( key ) ? true : false;
    return key in coll;
  },
  hash_map: function() {
    return clojure.lang.PersistentHashMap.create( arguments );
  },
  hash_set: function() {
    return clojure.lang.PersistentHashSet.create( arguments );
  },
  keyword: function(a,b) {
    if( b === undefined )
      return clojure.lang.Keyword.intern( "", a );
    return clojure.lang.Keyword.intern( a, b );
  },
  assoc: function( coll, key, val ) {
    if( coll === null )
      return new clojure.lang.PersistentArrayMap([key, val]);
    return coll.assoc( key, val );
  },
  count: function(x) {
    if( x === null ) return 0;
    if( x.count ) return x.count();
    if( x.length != undefined ) return x.length;
    throw ("count not supported on: " + (typeof x) + " " + x.constructor);
  },
  class_: function(o) {
    if( o === null )
      return null;
    return o.constructor || typeof o;
  },
  import_: function() {
    // do nothing
  },
  identical_QMARK_: function( a, b ) {
    return a === b;
  },
  keys: function(coll) {
    return clojure.lang.APersistentMap.KeySeq.create(clojure.seq(coll));
  },
  vals: function(coll) {
    return clojure.lang.APersistentMap.ValSeq.create(clojure.seq(coll));
  },
  JS: {
    merge: clojure.JS.merge,
    global: clojure.JS.global,
    variadic: function( arity, f ) {
      f.arity = arity;
      f.isVariadic = true;
      return f;
    },
    resolveVar: function( sym, ctxns ) {
      return ctxns[ sym ] || clojure[ sym ] || clojure.JS.global[ sym ];
    },
    def: function( ns, name, init ) {
      var v = new clojure.lang.Var( ns, name );
      ns["_var_" + name] = v;
      v.push( init );
      return v;
    },
    variadic_sentinel: {},
    rest_args: function( varflag, args, i ) {
      if( varflag === clojure.JS.variadic_sentinel )
        return args[ args.length - 1 ];
      return new clojure.lang.ArraySeq( null, args, i );
    },
    invoke: function( obj, methodname, args ) {
      return obj[ methodname ].apply( obj, args );
    },
    lit_list: function( a ) {
      return new clojure.lang.ArraySeq( null, a, 0 );
    },
    implement: function( cls, name, extend, implement ) {
      cls.classname = name;
      cls.classset = {};
      cls.hashCode = function() { return clojure.lang.Util.hash( name ); };
      cls.getSuperclass = function() { return extend || null; };
      cls.getInterfaces = function() { return implement || null; };
      cls.classset[ name ] = true;
      if( implement ) {
        for( var i = 0; i < implement.length; ++i ) {
          if( ! implement[ i ] )
            throw "Can't implement null: " + name;
          clojure.JS.merge( cls.classset, implement[ i ].classset );
        }
      }
    },
    definterface: function( pkg, name, implement ) {
      var cls = pkg[ name ] = {};
      clojure.JS.implement( cls, name, implement );
      return cls;
    },
    defclass: function( pkg, name, opts ) {
      var cls = pkg[ name ] = opts.init || function() {};
      clojure.JS.implement( cls, name, opts.extend, opts.implement );
      if( 'extend' in opts ) {
        cls.prototype = new opts.extend;
        cls.prototype.constructor = cls;
        clojure.JS.merge( cls.classset, opts.extend.classset );
      }
      if( opts.statics ) { clojure.JS.merge( cls, opts.statics ); }
      if( opts.methods ) { clojure.JS.merge( cls.prototype, opts.methods ); }
      return cls;
    },
    instanceq: function( c, o ){
      if( o === null || o.getClass === null )
        return false;
      if( o.constructor === c )
        return true;
      if( ! o.constructor.classset )
        return false; // builtin class that doesn't match?
      return o.constructor.classset[ c.classname ];
    },
    bitcount: function(n){
      var rtn = 0;
      for( ; n; n >>= 1) {
        rtn += n & 1;
      }
      return rtn;
    },
    ObjSeq: {
      create: function( obj ) {
        var pairs = [];
        for( var i in obj ) {
          pairs.push( [i, obj[i]] );
        }
        return clojure.lang.ArraySeq.create( pairs );
      }
    }
  },
  lang: {
    Namespace: clojure.lang.Namespace,
    Numbers: {
      isPos: function(x) { return x > 0; },
      lt: function(x,y) { return x < y; },
      gt: function(x,y) { return x > y; },
      inc: function(x) { return x + 1; },
      dec: function(x) { return x - 1; },
      unchecked_inc: function(x) { return x + 1; }
    },
    Util: {
      hash: function(o){
        if( o === null )
          return 0;
        if( typeof o == typeof "" ) {
          var ret = 0;
          for( var i = 0; i < o.length; ++i ) {
            ret ^= o.charCodeAt(i) << ((i % 4) * 8);
          }
          return ret;
        }
        if( o == String )
          return clojure.lang.Util.hash("stringtype");
        if( o == Number )
          return clojure.lang.Util.hash("numbertype");
        if( o == RegExp )
          return clojure.lang.Util.hash("regexptype");
        if( o == Object )
          return clojure.lang.Util.hash("objecttype");
        return o.hashCode();
      },
      equal: function(x,y) { return x == y; },
      isInteger: function(x) { return typeof x == typeof 0; }
    },
    RT: {
      EMPTY_ARRAY: [],
      conj: function( coll, x ) {
        if( coll === null )
          return new clojure.lang.PersistentList( null, x );
        return coll.cons( x );
      },
      cons: function( x, coll ) {
        var y = clojure.seq( coll );
        if( y === null )
          return new clojure.lang.PersistentList( null, x );
        return y.cons( x );
      },
      seqToArray: function(s) {
        var ret = new Array( clojure.count( s ) );
        for( var i = 0; s !== null; ++i, s = s.rest() )
          ret[ i ] = s.first();
        return ret;
      },
      intCast: function(i) {
        return parseInt(i);
      },
    }
  }
});

java = { util: { regex: {} }, lang: {}, math: {} };
clojure.JS.definterface( java.util, "Map" );
clojure.JS.definterface( java.util, "Collection" );
clojure.JS.definterface( java.util.regex, "Pattern" );
clojure.JS.definterface( java.lang, "Character" );
clojure.JS.definterface( java.lang, "Class" );
clojure.JS.definterface( java.math, "BigDecimal", [Number] );
clojure.JS.definterface( java.math, "BigInteger", [Number] );
clojure.JS.defclass( java.lang, "StringBuilder", {
  init: function( x ) { this.a = [ x ]; },
  methods: {
    append: function( x ) { this.a.push( x ); return this; },
    toString: function() { return this.a.join(''); }
  }
});

clojure.JS.defclass( java.lang, "String", {
  init: function(s) {
    this.s = s;
    this.length = s.length;
  },
  methods: {
    charAt: function(x) { return this.s.charAt(x); },
    toString: function() { return this.s; }
  }
});

clojure.JS.definterface( clojure.lang, "IObj" );

clojure.JS.defclass( clojure.lang, "Obj", {
  implement: [ clojure.lang.IObj ],
  init: function(_meta) { this._meta = _meta; },
  methods: {
    meta: function() { return this._meta; }
  }
});

clojure.JS.definterface( clojure.lang, "IReduce" );

clojure.JS.definterface( clojure.lang, "IPersistentCollection" );

clojure.JS.definterface( clojure.lang, "ISeq",
    [clojure.lang.IPersistentCollection] );

clojure.JS.definterface( clojure.lang, "IndexedSeq",
    [clojure.lang.ISeq] );

clojure.JS.defclass( clojure.lang, "ASeq", {
  methods: {
    equals: function( obj ) {
      var ms = obj.seq();
      for( var s = this.seq(); s !== null; s = s.rest(), ms = ms.rest() ) {
        if( ms === null || !clojure.lang.Util.equal( s.first(), ms.first() ))
          return false;
      }
      if( ms !== null )
        return false;
      return true;
    },
    hashCode: function() { throw "not yet implemented"; },
    count: function() {
      var i = 1;
      for( var s = this.rest(); s; s = s.rest() )
        i += 1;
      return i;
    },
    seq: function(){ return this; },
    cons: function(o){ return new clojure.lang.Cons( null, o, this ); },
    toArray: function(){ return clojure.lang.RT.seqToArray( this.seq() ); },
    containsAll: function(c){ throw "not yet implemented"; },
    size: function(){ return this.count(); },
    isEmpty: function(){ return this.count() == 0; },
    contains: function(c){ throw "not yet implemented"; }
  }
});

clojure.JS.defclass( clojure.lang, "Cons", {
  extend: clojure.lang.ASeq,
  init: function( _meta, _first, _rest ) {
    this._meta = _meta;
    this._first = _first;
    this._rest = _rest;
  },
  methods: {
    first: function(){ return this._first; },
    rest: function(){ return this._rest; },
    count: function(){ return 1 + clojure.count( this._rest ); },
    seq: function(){ return this; },
    withMeta: function(_meta){
      return new clojure.lang.Cons( _meta, this._first, this._rest );
    }
  }
});

clojure.JS.defclass( clojure.lang, "ArraySeq", {
  extend: clojure.lang.ASeq,
  implement: [clojure.lang.IndexedSeq, clojure.lang.IReduce],
  init: function( _meta, a, i, len ) {
    this._meta = _meta;
    this.a = a;
    this.i = i;
    this.len = (len === undefined) ? a.length : len;
  },
  statics: {
    create: function( a ) {
      if( a && a.length ) {
        return new clojure.lang.ArraySeq( null, a, 0 );
      }
      else {
        return null;
      }
    }
  },
  methods: {
    first: function() { return this.a[this.i]; },
    rest: function() {
      if( this.i + 1 < this.len )
        return new clojure.lang.ArraySeq(
            this._meta, this.a, this.i + 1, this.len);
      return null;
    },
    count: function() { return this.len - this.i; },
    index: function() { return this.i; },
    withMeta: function( _meta ) {
      return new clojure.lang.ArraySeq( _meta, this.array, this.i, this.len );
    },
    reduce: function( fn, start ) {
      var ret = (start === undefined) ? this.a[0] : fn(start, this.a[0]);
      for( var x = this.i + 1; x < this.len; ++x ) {
        ret = fn( ret, this.a[x] );
      }
      return ret;
    },
    seq: function() { return this; }
  }
});


clojure.JS.defclass( clojure.lang, "LazyCons", {
  init: function(f,_first,_rest) {
    this.f = f;
    this._first = _first === undefined ? clojure.lang.LazyCons.sentinel :_first;
    this._rest  = _rest  === undefined ? clojure.lang.LazyCons.sentinel :_rest;
  },
  statics: {
    sentinel: {}
  },
  methods: {
    first: function() {
      if( this._first === clojure.lang.LazyCons.sentinel )
        this._first = this.f();
      return this._first;
    },
    rest: function() {
      if( this._rest === clojure.lang.LazyCons.sentinel ) {
        if( this._first === clojure.lang.LazyCons.sentinel ) {
          this.first();
        }
        this._rest = clojure.seq( this.f(null) );
        this.f = null;
      }
      return this._rest;
    },
    withMeta: function(_meta) {
      if( _meta == this.meta() )
        return this;
      //force before copying
      this.rest();
      return new clojure.lang.LazyCons( _meta, this._first, this._rest );
    },
    seq: function() { return this; }
  }
});


clojure.JS.defclass( clojure.lang, "Var", {
  init: function( ns, name ) {
    this.ns = ns;
    this.name = name;
    this.stack = [];
  },
  statics: {
    stack: [],
    pushThreadBindings: function( m ) {
      var vars=[], b;
      for( var bs = m.seq(); bs; bs = bs.rest()) {
        e = bs.first();
        vars.push( e.key() );
        e.key().push( e.val() );
      }
      clojure.lang.Var.stack.push( vars );
    },
    popThreadBindings: function() {
      var vars = clojure.lang.Var.stack.pop();
      for( var i = 0; i < vars.length; ++i ) {
        vars[i].pop();
      }
    }
  },
  methods: {
    push: function( val ) {
      this.stack.push( val );
      this.ns[ this.name ] = val;
    },
    pop: function() {
      this.stack.pop();
      this.ns[ this.name ] = this.stack[ this.stack.length - 1 ];
    },
    set: function( val ) {
      this.stack.pop();
      this.push( val );
    },
    hasRoot: function() { return this.stack.length > 0; },
    hashCode: function() {
      return clojure.lang.Util.hash( this.ns + "/" + this.name );
    }
  }
});

clojure.JS.definterface( clojure.lang, "IFn" );

clojure.JS.defclass( clojure.lang, "AFn", {
  extend: clojure.lang.Obj,
  implement: [clojure.lang.IFn],
  methods: {
    apply: function( obj, args ){
      return this.invoke.apply( this, args );
    }
  }
});

clojure.JS.definterface( clojure.lang, "IPersistentStack",
    [clojure.lang.IPersistentCollection] );

clojure.JS.definterface( clojure.lang, "Sequential" );

clojure.JS.definterface( clojure.lang, "Reversible" );

clojure.JS.definterface( clojure.lang, "Named" );

clojure.JS.defclass( clojure.lang, "Keyword", {
  extend: clojure.lang.AFn,
  implement: [clojure.lang.Named],
  init: function( ns, name ) {
    this._ns = ns;
    this._name = name;
  },
  statics: {
    table: {},
    intern: function( ns, name ) {
      var key = ns + "/" + name;
      var obj = clojure.lang.Keyword.table[ key ];
      if( obj )
        return obj;
      return clojure.lang.Keyword.table[ key ] =
        new clojure.lang.Keyword( ns, name );
    }
  },
  methods: {
    toString: function() {
      return ":" + (this.ns ? this.ns+"/" : "") + this._name;
    },
    compareTo: function(o) {
      if( this == o )
        return 0;
      if( this._ns === null && o._ns !== null )
        return -1;
      if( this._ns !== null ) {
        if( o._ns === null )
          return 1;
        var nsc = clojure.JS.compare(this._ns, o._ns);
        if( nsc !== 0 )
          return nsc;
      }
      return clojure.JS.compare(this._name, o._name);
    },
    getNamespace: function() { return this._ns; },
    getName: function() { return this._name; },
    hashCode: function() {
      return clojure.lang.Util.hash( this._ns + "/" + this._name );
    },
    invoke: function(coll, notFound) { return clojure.get( coll,this,notFound);}
  }
});


clojure.JS.definterface( clojure.lang, "IPersistentList",
    [clojure.lang.Sequential, clojure.lang.IPersistentStack] );

clojure.JS.defclass( clojure.lang, "EmptyList", {
  extend: clojure.lang.Obj,
  implement: [clojure.lang.IPersistentList, java.util.Collection],
  init: function( _meta ) { this._meta = _meta; },
  methods: {
    cons: function(o) {
      return new clojure.lang.PersistentList( this.meta(), o );
    },
    empty: function() { return this; },
    withMeta: function(m) {
      if( m != this.meta() )
        return new clojure.lang.EmptyList( m );
      return this;
    },
    peek: function() { return null; },
    pop: function() { throw "Can't pop empty list"; },
    count: function() { return 0; },
    seq: function() { return null; },
    size: function() { return 0; },
    isEmpty: function() { return true; },
    contains: function() { return false; },
    toArray: function() { return clojure.lang.RT.EMPTY_ARRAY; },
    containsAll: function( coll ) { return coll.isEmpty(); }
  }
});

clojure.JS.definterface( clojure.lang, "IMapEntry" );

clojure.JS.definterface( clojure.lang, "Associative",
    [ clojure.lang.IPersistentCollection ] );

clojure.JS.definterface( clojure.lang, "IPersistentVector",
    [ clojure.lang.Associative, clojure.lang.Sequential,
      clojure.lang.IPersistentStack, clojure.lang.Reversible ]);

clojure.JS.defclass( clojure.lang, "AMapEntry", {
  implement: [ clojure.lang.IMapEntry, clojure.lang.IPersistentVector ],
  methods: {
    empty: function(){ return null; },
    equals: function(o){
      return clojure.lang.APersistentVector.doEquals(this,o);
    },
    hashCode: function(){ throw "not implemented yet"; },
    toString: function(){
      return this.key() + " " + this.val();
      var sw = new java.io.StringWriter();
      clojure.lang.RT.print( this, sw );
      return sw.toString();
    },
    length: function(){ return 2; },
    nth: function(i){
      switch(i){
        case 0: return this.key();
        case 1: return this.val();
        default: throw "Index out of bounds";
      }
    },
    asVector: function(){
      return clojure.lang.LazilyPersistentVector.createOwning(
          this.key(), this.val() );
    },
    assocN: function(i,v){ return this.asVector().assocN(i,v); },
    count: function(){ return 2; },
    seq: function(){ return this.asVector().seq(); },
    cons: function(o){ return this.asVector().cons(o); },
    containsKey: function(k){ return this.asVector().containsKey(k); },
    entryAt: function(k){ return this.asVector().entryAt(k); },
    assoc: function(k,v){ return this.asVector().assoc(k,v); },
    valAt: function(k,notFound){ return this.asVector().valAt(k,notFound); },
    peek: function(){ return this.val(); },
    pop: function(){
      return clojure.lang.LazilyPersistentVector.createOwning( this.key() );
    },
    rseq: function(){ return this.asVector().rseq(); }
  }
});

clojure.JS.defclass( clojure.lang, "MapEntry", {
  extend: clojure.lang.AMapEntry,
  init: function(k,v){
    this._key = k;
    this._val = v;
  },
  methods: {
    key: function(){ return this._key; },
    val: function(){ return this._val; },
    getKey: function(){ return this._key; },
    getValue: function(){ return this._val; }
  }
});

clojure.JS.defclass( clojure.lang, "PersistentList", {
  extend: clojure.lang.ASeq,
  implement: [clojure.lang.IPersistentList, clojure.lang.IReduce],
  init: function( _meta, _first, _rest, _count ) {
    this._meta = _meta || null;
    this._first = _first;
    this._rest = _rest || null;
    this._count = _count || 1;
  },
  statics: {
    creator: function() {
      var real = clojure.lang.PersistentList.creator;
      if( real == arguments.callee ) {
        throw "Not yet implemented: clojure.lang.PersistentList.creator";
      }
      return real.apply( arguments );
    },
    EMPTY: new clojure.lang.EmptyList(null)
  },
  methods: {
    first: function(){ return this._first; },
    rest: function(){
      if( this._count == 1 )
        return null;
      return this._rest;
    },
    peek: function(){ return this.first; },
    pop: function(){
      if( this._rest === null )
        return this.empty();
      return this._rest;
    },
    count: function(){ return this._count; },
    cons: function(o){
      return new clojure.lang.PersistentList(
          this._meta, o, this, this._count + 1 );
    },
    empty: function(){
      return clojure.lang.PersistentList.EMPTY.withMeta( this._meta );
    },
    withMeta: function( _meta ){
      if( _meta != this._meta )
        return new clojure.lang.PersistentList(
            this._meta, this._first, this._rest, this._count );
      return this;
    },
    reduce: function( f, start ){
      var ret = (start === undefined) ? this.first() : f( start, this.first() );
      for( var s = this.rest(); s !== null; s = s.rest() )
        ret = f( ret, s.first() );
      return ret;
    }
  }
});

clojure.JS.defclass( clojure.lang, "APersistentVector", {
  init: function( _meta ) { this._meta = _meta; },
  methods: {
    meta: function() { return this._meta; },
    peek: function() {
      if( this.count() > 0 )
        return this.nth( this.count() - 1 );
      return null;
    },
    seq: function() {
      if( this.count() > 0 )
        return new clojure.lang.APersistentVector.Seq( null, this, 0 );
      return null;
    },
    rseq: function() {
      if( this.count() > 0 )
        return new clojure.lang.APersistentVector.RSeq( this, this.count() - 1);
      return null;
    },
    equals: function() { throw "not implemented yet"; },
    hashCode: function() { throw "not implemented yet"; },
    get: function(i) { return this.nth(i); },
    indexOf: function( o ){
      var len = this.count();
      for( var i = 0; i < len; ++i )
        if( clojure.lang.Util.equal( this.nth( i ), o ) )
          return i;
      return -1;
    },
    lastIndexOf: function( o ){
      for( var i = this.count() - 1; i >= 0; --i )
        if( clojure.lang.Util.equal( this.nth( i ), o ) )
          return i;
      return -1;
    },
    subList: function( fromi, toi ) {
      return clojure.lang.RT.subvec( this, fromi, toi );
    },
    invoke: function( i ) {
      if( clojure.lang.Util.isInteger(i) )
        return this.nth( parseInt( i ) );
      throw "Key must be integer";
    },
    peek: function() {
      if( this.count() > 0 )
        return this.nth( this.count() - 1 );
      return null
    },
    constainsKey: function(k){
      if( ! clojure.lang.Util.isInteger( k ) )
        return false;
      var i = parseInt(k);
      return i >= 0 && i < this.count();
    },
    entryAt: function(k){
      if( clojure.lang.Util.isInteger( k ) ) {
        var i = parseInt(k);
        if( i >= 0 && i < this.count() )
          return new clojure.lang.MapEntry( k, this.nth(i) );
      }
      return null;
    },
    assoc: function(k,v){
      if( clojure.lang.Util.isInteger( k ) ) {
        var i = parseInt(k);
        return this.assocN(i,v);
      }
      throw "Key must be integer";
    },
    valAt: function(k, notFound){
      if( clojure.lang.Util.isInteger( k ) ) {
        var i = parseInt(k);
        if( i >= 0 && i < this.count() )
          return this.nth(i);
      }
      if( notFound === undefined )
        return null;
      return notFound;
    },
    toArray: function(){ return clojure.lang.RT.seqToArray( this.seq() ); },
    containsAll: function(){ throw "not implemented yet"; },
    size: function(){ return this.count(); },
    isEmpty: function(){ return this.count() === 0; },
    contains: function(o){
      for( var s = this.seq(); s !== null; s = s.rest() ) {
        if( clojure.lang.Util.equal( s.first(), o ) )
          return true;
      }
      return false;
    },
    length: function(){ return this.count(); },
    compareTo: function(v){
      var c, len = this.count();
      if( len < v.count() )
        return -1;
      else if( len > v.count() )
        return 1;
      for( var i = 0; i < len; ++i ) {
        c = this.nth(i).compareTo( v.nth(i) );
        if( c != 0 )
          return c;
      }
      return 0;
    }
  }
});

clojure.JS.defclass( clojure.lang.APersistentVector, "Seq", {
  init: function( _meta, v, i){
    this._meta = _meta;
    this.v = v;
    this.i = i;
  },
  methods: {
    first: function(){ return this.v.nth(this.i); },
    rest: function(){
      if( this.i + 1 < this.v.count() )
        return new clojure.lang.APersistentVector.Seq(
            this._meta, this.v, this.i + 1 );
      return null;
    },
    index: function(){ return this.i; },
    count: function(){ return this.v.count() - this.i; },
    withMeta: function(_meta){
      return new clojure.lang.APersistentVector.Seq( _meta, this.v, this.i );
    },
    reduce: function( fn, start ) {
      var ret = (start === undefined) ?
                this.v.nth(this.i) : fn(start,this.v.nth(this.i));
      for( var x = this.i + 1; x < this.count(); ++x ) {
        ret = fn( ret, this.v.nth(x) );
      }
      return ret;
    }
  }
});

clojure.JS.defclass( clojure.lang.APersistentVector, "RSeq", {
  init: function( _meta, v, i){
    this._meta = _meta;
    this.v = v;
    this.i = i;
  },
  methods: {
    first: function(){ return this.v.nth(this.i); },
    rest: function(){
      if( this.i > 0 )
        return new clojure.lang.APersistentVector.RSeq( this.v, this.i - 1 );
      return null;
    },
    index: function(){ return this.i; },
    count: function(){ return this.i + 1; },
    withMeta: function(_meta){
      return new clojure.lang.APersistentVector.RSeq( _meta, this.v, this.i );
    }
  }
});

clojure.JS.defclass( clojure.lang, "PersistentVector", {
  extend: clojure.lang.APersistentVector,
  init: function( _meta, cnt, shift, root, tail ) {
    clojure.lang.APersistentVector.call( this, _meta );
    this.cnt = cnt;
    this.shift = shift;
    this.root = root;
    this.tail = tail;
  },
  statics: {
    create: function( items ) {
      var ret = clojure.lang.PersistentVector.EMPTY;
      for( var i = 0; i < items.length; ++i ) {
        ret = ret.cons( items[ i ] );
      }
      return ret;
    }
  },
  methods: {
    tailoff: function() { return this.cnt - this.tail.length; },
    nth: function( i ) {
      if( i >= 0 && i < this.cnt ) {
        if( i >= this.tailoff() ) {
          return this.tail[ i & 0x01f ];
        }
        var arr = this.root;
        for( var level = this.shift; level > 0; level -= 5 ) {
          arr = arr[ (i >>> level) & 0x01f ];
        }
        return arr[ i & 0x01f ];
      }
      throw "IndexOutOfBoundsException";
    },
    assocN: function( i, val ) {
      if( i >= 0 && i < this.cnt ) {
        if( i >= this.tailoff() ) {
          var newTail = this.tail.slice( 0 );
          newTail[ i & 0x01f ] = val;
          return new clojure.lang.PersistentVector(
              this.meta(), this.cnt, this.shift, this.root, newTail );
        }
        return new clojure.lang.PersistentVector(
            this.meta(), this.cnt, this.shift,
            this.doAssoc( this.shift, this.root, i, val), this.tail );
      }
      if( i == this.cnt ) {
        return this.cons( val );
      }
      throw "IndexOutOfBoundsException";
    },
    doAssoc: function( level, arr, i, val ) {
      var ret = arr.slice( 0 );
      if( level == 0 ) {
        ret[ i & 0x01f ] = val;
      }
      else {
        var subidx = (i >>> level) & 0x01f;
        ret[ subidx ] = this.doAssoc( level - 5, arr[ subidx ], i, val );
      }
      return ret;
    },
    count: function() { return this.cnt; },
    withMeta: function( _meta ) {
      return new clojure.lang.PersistentVector(
          _meta, this.cnt, this.shift, this.root, this.tail );
    },
    cons: function( val ) {
      if( this.tail.length < 32 ) {
        var newTail = this.tail.concat( [val] );
        return new clojure.lang.PersistentVector(
            this.meta(), this.cnt + 1, this.shift, this.root, newTail );
      }
      var expansion = [null];
      var newroot = this.pushTail(
          this.shift - 5, this.root, this.tail, expansion);
      var newshift = this.shift;
      if( expansion[0] != null ) {
        newroot = [newroot, expansion[0]];
        newshift += 5;
      }
      return new clojure.lang.PersistentVector(
          this.meta(), this.cnt+1, newshift, newroot, [val] );
    },
    empty: function() {
      return clojure.lang.PersistentVector.EMPTY.withMeta( this.meta() );
    },
    pushTail: function( level, arr, tailNode, expansion)
    {
      var newchild;
      if( level == 0 ) {
        newchild = tailNode;
      }
      else {
        newchild = this.pushTail(
            level - 5, arr[arr.length - 1], tailNode, expansion);
        if( expansion[0] == null ) {
          var ret = arr.slice( 0 );
          ret[ arr.length - 1 ] = newchild;
          return ret;
        }
        else {
          newchild = expansion[0];
        }
      }
      //expansion
      if( arr.length == 32 ) {
        expansion[0] = [newchild];
        return arr;
      }
      expansion[0] = null;
      return arr.concat([newchild]);
    },
    pop: function() {
      if( this.cnt == 0 ) {
        throw "IllegalStateException: Can't pop empty vector";
      }
      if( this.cnt == 1 ) {
        return clojure.lang.PersistentVector.EMPTY.withMeta( this.meta() );
      }
      if( this.tail.length > 1 ) {
        var newTail = this.tail.slice( 0, this.tail.length - 1 );
        return new clojure.lang.PersistentVector(
            this.meta(), this.cnt - 1, this.shift, this.root, newTail );
      }
      var ptail = [null];
      var newroot = this.popTail( this.shift - 5, this.root, ptail );
      var newshift = this.shift;
      if( newroot == null ) {
        newroot = clojure.lang.RT.EMPTY_ARRAY;
      }
      if( this.shift > 5 && newroot.length == 1 ) {
        newroot = newroot[0];
        newshift -= 5;
      }
      return new clojure.lang.PersistentVector(
          this.meta(), this.cnt - 1, newshift, newroot, ptail[0] );
    },
    popTail: function( shift, arr, ptail ) {
      if( shift > 0 ) {
        var newchild = this.popTail( shift - 5, arr[ arr.length - 1 ], ptail );
        if( newchild != null ) {
          var ret = arr.slice( 0 );
          ret[ arr.length - 1 ] = newchild;
          return ret;
        }
      }
      if( shift == 0 ) {
        ptail[0] = arr[ arr.length - 1 ];
      }
      //contraction
      if( arr.length == 1 ) {
        return null;
      }
      return arr.slice( 0, arr.length - 1 );
    }
  }
});

clojure.lang.PersistentVector.EMPTY =
  new clojure.lang.PersistentVector(
      {}, 0, 5, clojure.lang.RT.EMPTY_ARRAY, clojure.lang.RT.EMPTY_ARRAY );

clojure.JS.definterface( clojure.lang, "IPersistentMap",
    [clojure.lang.Associative]);

clojure.JS.defclass( clojure.lang, "APersistentMap", {
  extend: clojure.lang.AFn,
  implement: [clojure.lang.IPersistentMap, java.util.Collection],
  init: function(_meta) {
    this._meta = _meta;
    this._hash = -1;
  },
  methods: {
    cons: function(o){
      if( clojure.JS.instanceq( clojure.lang.IPersistentVector, o ) ) {
        if( o.count() != 2 )
          throw "Vector arg to map conj must be a pair";
        return this.assoc( o.nth(0), o.nth(1) );
      }
      var e, ret = this;
      for( var es = clojure.seq( o ); es; es = es.rest() ) {
        e = es.first();
        ret = ret.assoc( e.getKey(), e.getValue() );
      }
      return ret;
    },
    equals: function(m){
      if( ! clojure.JS.instanceq( clojure.lang.IPersistentMap, m ) )
        return false;
      if( m.count() != this.count() || m.hashCode() != this.hashCode() )
        return false;
      var e, me;
      for( var s = this.seq(); s; s = s.rest() ) {
        e = s.first();
        me = m.entryAt( e.getKey() );
        if( me === null
            || ! clojure.lang.Util.equal( e.getValue(), me.getValue() ))
        {
          return false;
        }
      }
      return true;
    },
    hashCode: function(){
      if( this._hash == -1 ) {
        var e, hash = this.count();
        for( s = this.seq(); s; s = s.rest() ) {
          e = s.first();
          hash ^= clojure.lang.Util.hashCombine(
              clojure.lang.Util.hash( e.getKey() ),
              clojure.lang.Util.hash( e.getValue() ) );
        }
        this._hash = hash;
      }
      return _hash;
    },
    containsAll: function(){ throw "not implemented yet"; },
    invoke: function(k,notFound){ return this.valAt(k,notFound); },
    toArray: function(){ return clojure.lang.RT.seqToArray( this.seq() ); },
    size: function(){ return this.count(); },
    isEmpty: function(){ return this.count() === 0; },
    contains: function(e){
      if( clojure.JS.instanceq( clojure.lang.MapEntry, e ) ) {
        var v = this.entryAt( e.getKey() );
        return (v!==null && clojure.lang.Util.equal(v.getValue(),e.getValue()));
      }
      return false;
    }
  }
});

clojure.JS.defclass( clojure.lang.APersistentMap, "KeySeq", {
  extend: clojure.lang.ASeq,
  init: function(_meta, _seq) {
    this._meta = _meta;
    this._seq = _seq;
  },
  statics: {
    create: function(seq){
      if(seq === null)
        return null;
      return new clojure.lang.APersistentMap.KeySeq(seq);
    }
  },
  methods: {
    first: function(){ return this._seq.first().getKey(); },
    rest: function(){
      return clojure.lang.APersistentMap.KeySeq.create( this._seq.rest() );
    },
    withMeta: function(_meta){
      return new clojure.lang.APersistentMap.KeySeq( _meta, this._seq );
    }
  }
});

clojure.JS.defclass( clojure.lang.APersistentMap, "ValSeq", {
  extend: clojure.lang.ASeq,
  init: function(_meta, _seq) {
    this._meta = _meta;
    this._seq = _seq;
  },
  statics: {
    create: function(seq){
      if(seq === null)
        return null;
      return new clojure.lang.APersistentMap.ValSeq(seq);
    }
  },
  methods: {
    first: function(){ return this._seq.first().getValue(); },
    rest: function(){
      return clojure.lang.APersistentMap.ValSeq.create( this._seq.rest() );
    },
    withMeta: function(_meta){
      return new clojure.lang.APersistentMap.ValSeq( _meta, this._seq );
    }
  }
});

clojure.JS.defclass( clojure.lang, "PersistentHashMap", {
  extend: clojure.lang.APersistentMap,
  init: function(_meta, _count, _root){
    this._meta = _meta;
    this._count = _count;
    this._root = _root;
  },
  statics: {
    create: function(init){
      var ret = clojure.lang.PersistentHashMap.EMPTY;
      for( var s = clojure.seq( init ); s; s=s.rest().rest() ){
        if( s.rest() === null )
          throw "No value supplied for key: " + s.first();
        ret = ret.assoc( s.first(), clojure.second( s ) );
      }
      return ret;
    },
    mask: function(hash, shift){ return (hash >>> shift) & 0x01f; }
  },
  methods:{
    containsKey: function(key){ return this.entryAt(key) !== null; },
    entryAt: function(k){ return this._root.find(clojure.lang.Util.hash(k),k);},
    assoc: function(k,v){
      var addedLeaf=[null];
      var newroot = this._root.assoc(
          0, clojure.lang.Util.hash(k), k, v, addedLeaf );
      if( newroot == this._root )
        return this;
      return new clojure.lang.PersistentHashMap(
          this._meta, this._count + (addedLeaf[0] === null ? 0 : 1), newroot );
    },
    valAt: function(k, notFound){
      var e = this.entryAt(k);
      if( e !== null )
        return e.val();
      if( notFound === undefined )
        return null;
      return notFound;
    },
    assocEx: function(k,v){
      if(this.containsKey(k))
        throw "Key already present";
      return this.assoc(k,v);
    },
    without: function(k){
      var newroot = this._root.without( clojure.lang.Util.hash(k), k );
      if( newroot == this._root )
        return this;
      if( newroot == null )
        return this.empty();
      return new cloljure.lang.PersistentHashMap(
          this._meta, this._count-1, newroot );
    },
    count: function(){ return this._count; },
    seq: function(){ return this._root.nodeSeq(); },
    empty: function(){
      return clojure.lang.PersistentHashMap.EMPTY.withMeta( this._meta );
    },
    withMeta: function(_meta){
      return new clojure.lang.PersistentHashMap( _meta, this._count,this._root);
    }
  }
});

clojure.JS.defclass( clojure.lang.PersistentHashMap, "EmptyNode", {
  methods: {
    assoc: function(shift, hash, key, val, addedLeaf){
      var ret = new clojure.lang.PersistentHashMap.LeafNode( hash, key, val );
      addedLeaf[0] = ret;
      return ret;
    },
    without: function(h,k){ return this; },
    find:    function(h,k){ return null; },
    nodeSeq: function(){ return null;},
    getHash: function(){ return 0;}
  }
});

clojure.lang.PersistentHashMap.EMPTY = new clojure.lang.PersistentHashMap(
    null, 0, new clojure.lang.PersistentHashMap.EmptyNode() );

clojure.JS.defclass( clojure.lang.PersistentHashMap, "FullNode", {
  init: function(nodes, shift){
    this._nodes = nodes;
    this._shift = shift;
    this._hash = nodes[0].getHash();
  },
  statics: {
    bitpos: function(hash, shift) {
      return 1 << clojure.lang.PersistentHashMap.mask( hash, shift );
    }
  },
  methods: {
    assoc: function( levelShift, hash, key, val, addedLeaf ) {
      var PHM = clojure.lang.PersistentHashMap;
      var idx = PHM.mask( hash, this._shift );
      var n = this._nodes[idx].assoc( this._shift+5, hash, key, val, addedLeaf);
      if( n == this._nodes[idx] )
        return this;
      else {
        var newnodes = nodes.slice();
        newnodes[idx] = n;
        return new PHM.FullNode( newnodes, this._shift );
      }
    },
    without: function(hash, key){
      var PHM = clojure.lang.PersistentHashMap;
      var idx = PHM.mask( hash, this._shift );
      var n = this._nodes[idx].without( hash, key );
      if( n != this._nodes[idx] ) {
        var newnodes = nodes.slice();
        if( n == null ) {
          nodes.splice( idx, 1 );
          return new PHM.BitmapIndexedNode(
              ~PHM.FullNode.bitpos(hash,this._shift), newnodes, this._shift );
        }
        newnodes[ idx ] = n;
        return new PHM.FullNode( newnodes, this._shift );
      }
      return this;
    },
    find: function(hash, key) {
      return (nodes[clojure.lang.PersistentHashMap.mask( hash, this._shift )]
        .find( hash, key ));
    },
    nodeSeq: function(){
      return clojure.lang.PersistentHashMap.FullNode.Seq.create( this, 0 );
    },
    getHash: function(){ return this._hash; }
  }
});

clojure.JS.defclass( clojure.lang.PersistentHashMap.FullNode, "Seq", {
  extend: clojure.lang.ASeq,
  init: function( _meta, s, i, node ) {
    this._meta = _meta;
    this.s = s;
    this.i = i;
    this.node = node;
  },
  statics: {
    create: function(node, i){
      if( i >= node.nodes.length )
        return null;
      return new clojure.lang.PersistentHashMap.FullNode.Seq(
          null, node.nodes[i].nodeSeq(), i, node );
    }
  },
  methods: {
    first: function(){ return this.s.first(); },
    rest: function(){
      var Seq = clojure.lang.PersistentHashMap.FullNode.Seq;
      var nexts = this.s.rest();
      if( nexts )
        return new Seq( null, nexts, this.i, this.node );
      return Seq.create( node,this.i+1);
    },
    withMeta: function(_meta){
      return new clojure.lang.PersistentHashMap.FullNode.Seq(
          _meta, this.s, this.i, this.node );
    }
  }
});

clojure.JS.defclass( clojure.lang.PersistentHashMap, "BitmapIndexedNode", {
  init: function(bitmap, nodes, shift) {
    this.bitmap = bitmap;
    this.nodes = nodes;
    this.shift = shift;
    this._hash = nodes[0].getHash();
  },
  statics: {
    bitpos: function(hash, shift) {
      return 1 << clojure.lang.PersistentHashMap.mask( hash, shift );
    },
    createA: function(bitmap, nodes, shift){
      var PHM = clojure.lang.PersistentHashMap;
      if(bitmap == -1)
        return new PHM.FullNode( nodes, shift );
      return new PHM.BitmapIndexedNode( bitmap, nodes, shift );
    },
    createB: function(shift, branch, hash, key, val, addedLeaf){
      var PHM = clojure.lang.PersistentHashMap;
      return (new PHM.BitmapIndexedNode(
                PHM.BitmapIndexedNode.bitpos(branch.getHash(), shift),
                [branch],
                shift)).assoc( shift, hash, key, val, addedLeaf );
    }
  },
  methods: {
    index: function(bit) { return clojure.JS.bitcount(this.bitmap & (bit-1) );},
    assoc: function(levelShift, hash, key, val, addedLeaf){
      var BIN = clojure.lang.PersistentHashMap.BitmapIndexedNode;
      var bit = BIN.bitpos( hash, this.shift );
      var idx = this.index( bit );
      if((this.bitmap & bit) != 0) {
        var n = this.nodes[idx].assoc( this.shift+5, hash, key, val, addedLeaf);
        if( n == this.nodes[idx] )
          return this;
        else {
          var newnodes = this.nodes.slice();
          newnodes[idx] = n;
          return new BIN( this.bitmap, newnodes, this.shift );
        }
      }
      else {
        addedLeaf[0]= new clojure.lang.PersistentHashMap.LeafNode(hash,key,val);
        var newnodes = this.nodes.slice();
        newnodes.splice( idx, 0, addedLeaf[0] );
        return BIN.createA( this.bitmap | bit, newnodes, this.shift );
      }
    },
    without: function( hash, key ) {
      var BIN = clojure.lang.PersistentHashMap.BitmapIndexedNode;
      var bit = BIN.bitpos( hash, this.shift );
      if((this.bitmap & bit) !== 0) {
        var idx = this.index( bit );
        var n = this.nodes[ idx ].without( hash, key );
        if( n != this.nodes[ idx ] ) {
          if( n === null ) {
            if( this.bitmap == bit )
              return null;
            var newnodes = this.nodes.slice();
            newnodes.splice( idx, 1 );
            return new BIN( bitmap & ~bit, newnodes, this.shift );
          }
          var newnodes = this.nodes.slice();
          newnodes[ idx ] = n;
          return new BIN( bitmap, newnodes, this.shift );
        }
      }
      return this;
    },
    find: function( hash, key ) {
      var BIN = clojure.lang.PersistentHashMap.BitmapIndexedNode;
      var bit = BIN.bitpos( hash, this.shift );
      if((this.bitmap & bit) !== 0)
        return this.nodes[ this.index(bit) ].find( hash, key );
      return null;
    },
    getHash: function(){ return this._hash; },
    nodeSeq: function(){
      return clojure.lang.PersistentHashMap.BitmapIndexedNode.Seq.create(
          this, 0 );
    }
  }
});

clojure.JS.defclass( clojure.lang.PersistentHashMap.BitmapIndexedNode, "Seq", {
  extend: clojure.lang.ASeq,
  init: function( _meta, s, i, node ) {
    this._meta = _meta;
    this.s = s;
    this.i = i;
    this.node = node;
  },
  statics: {
    create: function(node, i){
      if( i >= node.nodes.length )
        return null;
      return new clojure.lang.PersistentHashMap.BitmapIndexedNode.Seq(
          null, node.nodes[i].nodeSeq(), i, node );
    }
  },
  methods: {
    first: function(){ return this.s.first(); },
    rest: function(){
      var Seq = clojure.lang.PersistentHashMap.BitmapIndexedNode.Seq;
      var nexts = this.s.rest();
      if( nexts )
        return new Seq( null, nexts, this.i, this.node );
      return Seq.create( this.node, this.i+1 );
    },
    withMeta: function(_meta){
      return new clojure.lang.PersistentHashMap.BitmapIndexedNode.Seq(
          _meta, this.s, this.i, this.node );
    }
  }
});

clojure.JS.defclass( clojure.lang.PersistentHashMap, "LeafNode", {
  extend: clojure.lang.AMapEntry,
  init: function( hash, key, val ) {
    this.hash = hash;
    this._key = key;
    this._val = val;
  },
  methods: {
    assoc: function(shift, hash, key, val, addedLeaf) {
      var PHM = clojure.lang.PersistentHashMap;
      if( hash == this.hash ) {
        if( clojure.lang.Util.equal( key, this._key ) ) {
          if( val == this._val )
            return this;
          return new PHM.LeafNode( hash, key, val );
        }
        var newLeaf = new PHM.LeafNode( hash, key, val );
        addedLeaf[0] = newLeaf;
        return new PHM.HashCollisionNode( hash, [this, newLeaf] );
      }
      return PHM.BitmapIndexedNode.createB(
          shift, this, hash, key, val, addedLeaf );
    },
    without: function(hash, key){
      if(hash == this.hash && clojure.lang.Util.equal( key, this._key ))
        return null;
      return this;
    },
    find: function(hash, key){
      if(hash == this.hash && clojure.lang.Util.equal( key, this._key ))
        return this;
      return null;
    },
    nodeSeq: function(){ return clojure.cons( this, null ); },
    getHash: function(){ return this.hash; },
    key: function(){ return this._key; },
    val: function(){ return this._val; },
    getKey: function(){ return this._key; },
    getValue: function(){ return this._val; }
  }
});

clojure.JS.defclass( clojure.lang.PersistentHashMap, "HashCollisionNode", {
  init: function(hash, leaves){
    this.hash = hash;
    this.leaves = leaves;
  },
  methods: {
    assoc: function(shift, hash, key, val, addedLeaf) {
      var PHM = clojure.lang.PersistentHashMap;
      if( hash == this.hash ) {
        var idx = this.findIndex( hash, key );
        if( idx != -1 ) {
          if( this.leaves[idx].val == val )
            return this;
          var newLeaves = this.leaves.slice();
          newLeaves[idx] = new PHM.LeafNode( hash, key, val );
          return new PHM.HashCollisionNode( has, newLeaves );
        }
        addedLeaf[0] = new PHM.LeafNode( hash, key, val );
        var newLeaves = this.leaves.concat( addedLeaf );
        return new PHM.HashCollisionNode( hash, newLeaves );
      }
      return PHM.BitmapIndexedNode.createB(shift,this,hash,key,val,addedLeaf);
    },
    without: function(hash, key){
      var idx = this.findIndex( hash, key );
      if( idx != -1 )
        return leaves[ idx ];
      return null;
    },
    nodeSeq: function(){
      return clojure.lang.ArraySeq.create(this.leaves);
    },
    findIndex: function(hash, key){
      for( var i = 0; i < this.leaves.length; ++i ) {
        if( this.leaves[i].find( hash, key ) != null )
          return i;
      }
      return -1;
    },
    getHash: function(){ return this.hash; }
  }
});

clojure.JS.definterface( clojure.lang, "IPersistentSet",
    [ clojure.lang.IPersistentCollection ] );

clojure.JS.defclass( clojure.lang, "APersistentSet", {
  extend: clojure.lang.AFn,
  implement: [ clojure.lang.IPersistentSet ],
  init: function( meta, impl ) {
    this._meta = meta;
    this.impl = impl;
    this._hash = -1;
  },
  methods: {
    contains: function(key){ return this.impl.containsKey(key); },
    get: function(key){ return this.impl.valAt(key); },
    count: function(){ return this.impl.count(); },
    seq: function(){ return clojure.keys( this.impl ); },
    invoke: function(key){ return this.get(key); },
    equals: function(m) {
      if( ! clojure.instanceq( clojure.lang.IPersistentSet ) )
        return false;
      if( m.count() != this.count() || m.hashCode() != this.hashCode() )
        return false;
      for( var s = this.seq(); s; s = s.rest() ) {
        if( ! m.contains( s.first() ) )
          return false;
      }
      return true;
    },
    hashCode: function() {
      if( this._hash == -1 ) {
        var hash = this.count();
        for( var s = this.seq(); s; s = s.rest() ) {
          hash = clojure.lang.Util.hashCombine(
              hash, clojure.lang.Util.hash( s.first() ) );
        }
        this._hash = hash;
      }
      return this._hash;
    },
    toArray: function(){ return clojure.lang.RT.seqToArray( this.seq() ); },
    containsAll: function(c){ throw "not yet implemented"; },
    size: function(){ return this.count(); },
    isEmpty: function(){ return this.count() == 0; }
  }
});

clojure.JS.defclass( clojure.lang, "PersistentHashSet", {
  extend: clojure.lang.APersistentSet,
  init: function( meta, impl ) {
    clojure.lang.APersistentSet.call( this, meta, impl );
  },
  statics: {
    create: function(init){
      var ret = clojure.lang.PersistentHashSet.EMPTY;
      for( var s = clojure.seq( init ); s; s=s.rest() ){
        ret = ret.cons( s.first() );
      }
      return ret;
    }
  },
  methods: {
    disjoin: function(key) {
      if( this.contains(key) )
        return new clojure.lang.PersistentHashSet(
            this._meta, this.impl.without(key));
      return this;
    },
    cons: function(o) {
      if( this.contains(o) )
        return this;
      return new clojure.lang.PersistentHashSet(
          this._meta, this.impl.assoc(o));
    },
    empty: function(){
      return clojure.lang.PersistentHashSet.EMPTY.withMeta( this._meta );
    },
    withMeta: function(_meta){
      return new clojure.lang.PersistentHashSet( _meta, this.impl );
    }
  }
});

clojure.lang.PersistentHashSet.EMPTY = new clojure.lang.PersistentHashSet(
    null, clojure.lang.PersistentHashMap.EMPTY );

clojure.JS.defclass( clojure.lang, "MultiFn", {
  extend: clojure.lang.AFn,
  init: function( dispatchFn, defaultDispatchVal ) {
    this.dispatchFn = dispatchFn;
    this.defaultDispatchVal = defaultDispatchVal;
    this.methodTable = clojure.lang.PersistentHashMap.EMPTY;
    this.methodCache = clojure.lang.PersistentHashMap.EMPTY;
    this.preferTable = clojure.lang.PersistentHashMap.EMPTY;
    this.cachedHierarchy = null;
  },
  methods: {
    addMethod: function( dispatchVal, method ){
      this.methodTable = this.methodTable.assoc( dispatchVal, method );
      this.resetCache();
      return this;
    },
    removeMethod: function( dispatchVal ){
      this.methodTable = this.methodTable.without( dispatchVal );
      this.resetCache();
      return this;
    },
    preferMethod: function( dispatchValX, dispatchValY ){
      if( this.prefers( dispatchValY, dispatchValX ) )
        throw ("Preference conflict: " + dispatchValY +
            " is already preferred to" + dispatchValX);
      var oldset = clojure.get(
          this.preferTable, dispatchValX, clojure.lang.PersistentHashSet.EMPTY);
      this.preferTable = this.preferTable.assoc(
          dispatchValX, clojure.conj( oldset, dispatchValY ) );
      this.resetCache();
      return this;
    },
    prefers: function(x,y) {
      var xprefs = this.preferTable.valAt(x);
      return xprefs && xprefs.contains(y);
    },
    isA: function(x,y) { return clojure.isa_QMARK_( x, y ); },
    dominates: function(x,y) { return this.prefers(x,y) || this.isA(x,y); },
    resetCache: function() {
      this.methodCache = this.methodTable;
      this.cachedHierarchy = clojure.global_hierarchy;
      return this.methodCache;
    },
    getFn: function(dispatchVal) {
      if( this.cachedHierarchy != clojure.global_hierarchy )
        this.resetCache();
      var targetFn =
        this.methodCache.valAt( dispatchVal ) ||
        this.findAndCacheBestMethod( dispatchVal ) ||
        this.methodTable.valAt( this.defaultDispatchVal );
      if( targetFn === null )
        throw "No method for dispatch value: " + dispatchVal;
      return targetFn;
    },
    findAndCacheBestMethod: function( dispatchVal ) {
      var e, bestEntry = null;
      for( var s = this.methodTable.seq(); s; s = s.rest() ) {
        e = s.first();
        if( this.isA( dispatchVal, e.getKey() ) ) {
          if( bestEntry===null || this.dominates(e.getKey(),bestEntry.getKey()))
            bestEntry = e;
          if( ! this.dominates( bestEntry.getKey(), e.getKey() ) )
            throw ["Multiple methods match dispatch value:", dispatchVal,
                  "->", e.getKey(), "and", bestEntry.getKey(),
                  "and neither is preferred"].join(' ');
        }
      }
      if( bestEntry === null )
        return null;
      // skip multi-threading protection
      this.methodCache = this.methodCache.assoc(
          dispatchVal, bestEntry.getValue());
      return bestEntry.getValue();
    },
    invoke: function() {
      return (this.getFn( this.dispatchFn.apply( null, arguments ) )
        .apply( null, arguments ));
    }
  }
});

clojure.print_method = new clojure.lang.MultiFn(
  function (x, writer){ return clojure.class_(x); },
  clojure.keyword("","default"));

clojure.print_method.addMethod( String, function(s,w) {
  return clojure.print_method.apply( null, [new java.lang.String(s), w] );
});

clojure.JS.def(clojure,"_STAR_print_readably_STAR_",true);

clojure.lang.Namespace.find = function( s ) {
  return clojure.JS.global[ s.substring(1) ];
};

clojure.lang.Namespace.prototype.getMappings = function() {
  return this;
};

(function() {
  var buf = [];
  function write(s) {
    s = s.toString();
    var parts = s.split(/\n/);
    if( parts.length == 1 ) {
      buf.push(s);
    }
    else {
      var last = parts.pop();
      print( buf.join('') + parts.join('\n') );
      buf = [ last ];
    }
  }
  clojure._STAR_out_STAR_ = { append: write, write: write };
})();
