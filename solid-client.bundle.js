var SolidClient = (function (exports) {
  'use strict';

  var COMPATIBLE_ENCODING_PATTERN = /^utf-?8|ascii|utf-?16-?le|ucs-?2|base-?64|latin-?1$/i;
  var WS_TRIM_PATTERN = /^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g;
  var WS_CHAR_PATTERN = /\s|\uFEFF|\xA0/;
  var WS_FOLD_PATTERN = /\r?\n[\x20\x09]+/g;
  var DELIMITER_PATTERN = /[;,"]/;
  var WS_DELIMITER_PATTERN = /[;,"]|\s/;

  /**
   * Token character pattern
   * @type {RegExp}
   * @see https://tools.ietf.org/html/rfc7230#section-3.2.6
   */
  var TOKEN_PATTERN = /^[!#$%&'*+\-\.^_`|~\da-zA-Z]+$/;

  var STATE = {
    IDLE: 1 << 0,
    URI: 1 << 1,
    ATTR: 1 << 2,
  };

  function trim( value ) {
    return value.replace( WS_TRIM_PATTERN, '' )
  }

  function hasWhitespace( value ) {
    return WS_CHAR_PATTERN.test( value )
  }

  function skipWhitespace( value, offset ) {
    while( hasWhitespace( value[offset] ) ) {
      offset++;
    }
    return offset
  }

  function needsQuotes( value ) {
    return WS_DELIMITER_PATTERN.test( value ) ||
      !TOKEN_PATTERN.test( value )
  }

  class Link {

    /**
     * Link
     * @constructor
     * @param {String} [value]
     * @returns {Link}
     */
    constructor( value ) {

      /** @type {Array} URI references */
      this.refs = [];

      if( value ) {
        this.parse( value );
      }

    }

    /**
     * Get refs with given relation type
     * @param {String} value
     * @returns {Array<Object>}
     */
    rel( value ) {

      var links = [];
      var type = value.toLowerCase();

      for( var i = 0; i < this.refs.length; i++ ) {
        if( this.refs[ i ].rel.toLowerCase() === type ) {
          links.push( this.refs[ i ] );
        }
      }

      return links

    }

    /**
     * Get refs where given attribute has a given value
     * @param {String} attr
     * @param {String} value
     * @returns {Array<Object>}
     */
    get( attr, value ) {

      attr = attr.toLowerCase();

      var links = [];

      for( var i = 0; i < this.refs.length; i++ ) {
        if( this.refs[ i ][ attr ] === value ) {
          links.push( this.refs[ i ] );
        }
      }

      return links

    }

    set( link ) {
      this.refs.push( link );
      return this
    }

    has( attr, value ) {

      attr = attr.toLowerCase();

      for( var i = 0; i < this.refs.length; i++ ) {
        if( this.refs[ i ][ attr ] === value ) {
          return true
        }
      }

      return false

    }

    parse( value, offset ) {

      offset = offset || 0;
      value = offset ? value.slice( offset ) : value;

      // Trim & unfold folded lines
      value = trim( value ).replace( WS_FOLD_PATTERN, '' );

      var state = STATE.IDLE;
      var length = value.length;
      var offset = 0;
      var ref = null;

      while( offset < length ) {
        if( state === STATE.IDLE ) {
          if( hasWhitespace( value[offset] ) ) {
            offset++;
            continue
          } else if( value[offset] === '<' ) {
            if( ref != null ) {
              ref.rel != null ?
                this.refs.push( ...Link.expandRelations( ref ) ) :
                this.refs.push( ref );
            }
            var end = value.indexOf( '>', offset );
            if( end === -1 ) throw new Error( 'Expected end of URI delimiter at offset ' + offset )
            ref = { uri: value.slice( offset + 1, end ) };
            // this.refs.push( ref )
            offset = end;
            state = STATE.URI;
          } else {
            throw new Error( 'Unexpected character "' + value[offset] + '" at offset ' + offset )
          }
          offset++;
        } else if( state === STATE.URI ) {
          if( hasWhitespace( value[offset] ) ) {
            offset++;
            continue
          } else if( value[offset] === ';' ) {
            state = STATE.ATTR;
            offset++;
          } else if( value[offset] === ',' ) {
            state = STATE.IDLE;
            offset++;
          } else {
            throw new Error( 'Unexpected character "' + value[offset] + '" at offset ' + offset )
          }
        } else if( state === STATE.ATTR ) {
          if( value[offset] ===';' || hasWhitespace( value[offset] ) ) {
            offset++;
            continue
          }
          var end = value.indexOf( '=', offset );
          if( end === -1 ) throw new Error( 'Expected attribute delimiter at offset ' + offset )
          var attr = trim( value.slice( offset, end ) ).toLowerCase();
          var attrValue = '';
          offset = end + 1;
          offset = skipWhitespace( value, offset );
          if( value[offset] === '"' ) {
            offset++;
            while( offset < length ) {
              if( value[offset] === '"' ) {
                offset++; break
              }
              if( value[offset] === '\\' ) {
                offset++;
              }
              attrValue += value[offset];
              offset++;
            }
          } else {
            var end = offset + 1;
            while( !DELIMITER_PATTERN.test( value[end] ) && end < length ) {
              end++;
            }
            attrValue = value.slice( offset, end );
            offset = end;
          }
          if( ref[ attr ] && Link.isSingleOccurenceAttr( attr ) ) ; else if( attr[ attr.length - 1 ] === '*' ) {
            ref[ attr ] = Link.parseExtendedValue( attrValue );
          } else {
            attrValue = attr === 'type' ?
              attrValue.toLowerCase() : attrValue;
            if( ref[ attr ] != null ) {
              if( Array.isArray( ref[ attr ] ) ) {
                ref[ attr ].push( attrValue );
              } else {
                ref[ attr ] = [ ref[ attr ], attrValue ];
              }
            } else {
              ref[ attr ] = attrValue;
            }
          }
          switch( value[offset] ) {
            case ',': state = STATE.IDLE; break
            case ';': state = STATE.ATTR; break
          }
          offset++;
        } else {
          throw new Error( 'Unknown parser state "' + state + '"' )
        }
      }

      if( ref != null ) {
        ref.rel != null ?
          this.refs.push( ...Link.expandRelations( ref ) ) :
          this.refs.push( ref );
      }

      ref = null;

      return this

    }

    toString() {

      var refs = [];
      var link = '';
      var ref = null;

      for( var i = 0; i < this.refs.length; i++ ) {
        ref = this.refs[i];
        link = Object.keys( this.refs[i] ).reduce( function( link, attr ) {
          if( attr === 'uri' ) return link
          return link + '; ' + Link.formatAttribute( attr, ref[ attr ] )
        }, '<' + ref.uri + '>' );
        refs.push( link );
      }

      return refs.join( ', ' )

    }

  }

  /**
   * Determines whether an encoding can be
   * natively handled with a `Buffer`
   * @param {String} value
   * @returns {Boolean}
   */
  Link.isCompatibleEncoding = function( value ) {
    return COMPATIBLE_ENCODING_PATTERN.test( value )
  };

  Link.parse = function( value, offset ) {
    return new Link().parse( value, offset )
  };

  Link.isSingleOccurenceAttr = function( attr ) {
    return attr === 'rel' || attr === 'type' || attr === 'media' ||
      attr === 'title' || attr === 'title*'
  };

  Link.isTokenAttr = function( attr ) {
    return attr === 'rel' || attr === 'type' || attr === 'anchor'
  };

  Link.escapeQuotes = function( value ) {
    return value.replace( /"/g, '\\"' )
  };

  Link.expandRelations = function( ref ) {
    var rels = ref.rel.split( ' ' );
    return rels.map( function( rel ) {
      var value = Object.assign( {}, ref );
      value.rel = rel;
      return value
    })
  };

  /**
   * Parses an extended value and attempts to decode it
   * @internal
   * @param {String} value
   * @return {Object}
   */
  Link.parseExtendedValue = function( value ) {
    var parts = /([^']+)?(?:'([^']+)')?(.+)/.exec( value );
    return {
      language: parts[2].toLowerCase(),
      encoding: Link.isCompatibleEncoding( parts[1] ) ?
        null : parts[1].toLowerCase(),
      value: Link.isCompatibleEncoding( parts[1] ) ?
        decodeURIComponent( parts[3] ) : parts[3]
    }
  };

  /**
   * Format a given extended attribute and it's value
   * @param {String} attr
   * @param {Object} data
   * @return {String}
   */
  Link.formatExtendedAttribute = function( attr, data ) {

    var encoding = ( data.encoding || 'utf-8' ).toUpperCase();
    var language = data.language || 'en';

    var encodedValue = '';

    if( Buffer.isBuffer( data.value ) && Link.isCompatibleEncoding( encoding ) ) {
      encodedValue = data.value.toString( encoding );
    } else if( Buffer.isBuffer( data.value ) ) {
      encodedValue = data.value.toString( 'hex' )
        .replace( /[0-9a-f]{2}/gi, '%$1' );
    } else {
      encodedValue = encodeURIComponent( data.value );
    }

    return attr + '=' + encoding + '\'' +
      language + '\'' + encodedValue

  };

  /**
   * Format a given attribute and it's value
   * @param {String} attr
   * @param {String|Object} value
   * @return {String}
   */
  Link.formatAttribute = function( attr, value ) {

    if( Array.isArray( value ) ) {
      return value.map(( item ) => {
        return Link.formatAttribute( attr, item )
      }).join( '; ' )
    }

    if( attr[ attr.length - 1 ] === '*' || typeof value !== 'string' ) {
      return Link.formatExtendedAttribute( attr, value )
    }

    if( Link.isTokenAttr( attr ) ) {
      value = needsQuotes( value ) ?
        '"' + Link.escapeQuotes( value ) + '"' :
        Link.escapeQuotes( value );
    } else if( needsQuotes( value ) ) {
      value = encodeURIComponent( value );
      // We don't need to escape <SP> <,> <;> within quotes
      value = value
        .replace( /%20/g, ' ' )
        .replace( /%2C/g, ',' )
        .replace( /%3B/g, ';' );

      value = '"' + value + '"';
    }

    return attr + '=' + value

  };

  var link = Link;

  class BlankNode$1 {
    constructor (id) {
      this.value = id || ('b' + (++BlankNode$1.nextId));
    }

    equals (other) {
      return !!other && other.termType === this.termType && other.value === this.value
    }
  }

  BlankNode$1.prototype.termType = 'BlankNode';

  BlankNode$1.nextId = 0;

  var BlankNode_1 = BlankNode$1;

  class DefaultGraph$1 {
    equals (other) {
      return !!other && other.termType === this.termType
    }
  }

  DefaultGraph$1.prototype.termType = 'DefaultGraph';
  DefaultGraph$1.prototype.value = '';

  var DefaultGraph_1 = DefaultGraph$1;

  function fromTerm$1 (original) {
    if (!original) {
      return null
    }

    if (original.termType === 'BlankNode') {
      return this.blankNode(original.value)
    }

    if (original.termType === 'DefaultGraph') {
      return this.defaultGraph()
    }

    if (original.termType === 'Literal') {
      return this.literal(original.value, original.language || this.namedNode(original.datatype.value))
    }

    if (original.termType === 'NamedNode') {
      return this.namedNode(original.value)
    }

    if (original.termType === 'Quad') {
      const subject = this.fromTerm(original.subject);
      const predicate = this.fromTerm(original.predicate);
      const object = this.fromTerm(original.object);
      const graph = this.fromTerm(original.graph);

      return this.quad(subject, predicate, object, graph)
    }

    if (original.termType === 'Variable') {
      return this.variable(original.value)
    }

    throw new Error(`unknown termType ${original.termType}`)
  }

  var fromTerm_1 = fromTerm$1;

  class NamedNode$1 {
    constructor (iri) {
      this.value = iri;
    }

    equals (other) {
      return !!other && other.termType === this.termType && other.value === this.value
    }
  }

  NamedNode$1.prototype.termType = 'NamedNode';

  var NamedNode_1 = NamedNode$1;

  class Literal$1 {
    constructor (value, language, datatype) {
      this.value = value;
      this.datatype = Literal$1.stringDatatype;
      this.language = '';

      if (language) {
        this.language = language;
        this.datatype = Literal$1.langStringDatatype;
      } else if (datatype) {
        this.datatype = datatype;
      }
    }

    equals (other) {
      return !!other && other.termType === this.termType && other.value === this.value &&
        other.language === this.language && other.datatype.equals(this.datatype)
    }
  }

  Literal$1.prototype.termType = 'Literal';

  Literal$1.langStringDatatype = new NamedNode_1('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString');
  Literal$1.stringDatatype = new NamedNode_1('http://www.w3.org/2001/XMLSchema#string');

  var Literal_1 = Literal$1;

  class Quad$1 {
    constructor (subject, predicate, object, graph) {
      this.subject = subject;
      this.predicate = predicate;
      this.object = object;

      if (graph) {
        this.graph = graph;
      } else {
        this.graph = new DefaultGraph_1();
      }
    }

    equals (other) {
      // `|| !other.termType` is for backwards-compatibility with old factories without RDF* support.
      return !!other && (other.termType === 'Quad' || !other.termType) &&
        other.subject.equals(this.subject) && other.predicate.equals(this.predicate) &&
        other.object.equals(this.object) && other.graph.equals(this.graph)
    }
  }

  Quad$1.prototype.termType = 'Quad';
  Quad$1.prototype.value = '';

  var Quad_1 = Quad$1;

  class Variable$1 {
    constructor (name) {
      this.value = name;
    }

    equals (other) {
      return !!other && other.termType === this.termType && other.value === this.value
    }
  }

  Variable$1.prototype.termType = 'Variable';

  var Variable_1 = Variable$1;

  function namedNode$1 (value) {
    return new NamedNode_1(value)
  }

  function blankNode$1 (value) {
    return new BlankNode_1(value)
  }

  function literal$1 (value, languageOrDatatype) {
    if (typeof languageOrDatatype === 'string') {
      if (languageOrDatatype.indexOf(':') === -1) {
        return new Literal_1(value, languageOrDatatype)
      }

      return new Literal_1(value, null, DataFactory$2.namedNode(languageOrDatatype))
    }

    return new Literal_1(value, null, languageOrDatatype)
  }

  function variable$1 (value) {
    return new Variable_1(value)
  }

  function defaultGraph$1 () {
    return DataFactory$2.defaultGraphInstance
  }

  function triple (subject, predicate, object) {
    return DataFactory$2.quad(subject, predicate, object)
  }

  function quad$1 (subject, predicate, object, graph) {
    return new Quad_1(subject, predicate, object, graph || DataFactory$2.defaultGraphInstance)
  }

  function fromTerm (original) {
    return fromTerm_1.call(DataFactory$2, original)
  }

  function fromQuad (original) {
    return fromTerm_1.call(DataFactory$2, original)
  }

  const DataFactory$2 = {
    namedNode: namedNode$1,
    blankNode: blankNode$1,
    literal: literal$1,
    variable: variable$1,
    defaultGraph: defaultGraph$1,
    triple,
    quad: quad$1,
    fromTerm,
    fromQuad,
    defaultGraphInstance: new DefaultGraph_1()
  };

  var DataFactory_1 = DataFactory$2;

  var dataModel = DataFactory_1;

  function isString$1 (s) {
    return typeof s === 'string' || s instanceof String
  }

  const xsdString = 'http://www.w3.org/2001/XMLSchema#string';

  function termToId$1 (term) {
    if (typeof term === 'string') {
      return term
    }

    if (!term) {
      return ''
    }

    if (typeof term.id !== 'undefined' && term.termType !== 'Quad') {
      return term.id
    }

    let subject, predicate, object, graph;

    // Term instantiated with another library
    switch (term.termType) {
      case 'NamedNode':
        return term.value

      case 'BlankNode':
        return `_:${term.value}`

      case 'Variable':
        return `?${term.value}`

      case 'DefaultGraph':
        return ''

      case 'Literal':
        if (term.language) {
          return `"${term.value}"@${term.language}`
        }

        return `"${term.value}"${term.datatype && term.datatype.value !== xsdString ? `^^${term.datatype.value}` : ''}`

      case 'Quad':
        // To identify RDF* quad components, we escape quotes by doubling them.
        // This avoids the overhead of backslash parsing of Turtle-like syntaxes.
        subject = escapeQuotes$1(termToId$1(term.subject));
        predicate = escapeQuotes$1(termToId$1(term.predicate));
        object = escapeQuotes$1(termToId$1(term.object));
        graph = term.graph.termType === 'DefaultGraph' ? '' : ` ${termToId$1(term.graph)}`;

        return `<<${subject} ${predicate} ${object}${graph}>>`

      default:
        throw new Error(`Unexpected termType: ${term.termType}`)
    }
  }

  const escapedLiteral$1 = /^"(.*".*)(?="[^"]*$)/;

  function escapeQuotes$1 (id) {
    return id.replace(escapedLiteral$1, (_, quoted) => `"${quoted.replace(/"/g, '""')}`)
  }

  class DatasetCore {
    constructor (quads) {
      // The number of quads is initially zero
      this._size = 0;
      // `_graphs` contains subject, predicate, and object indexes per graph
      this._graphs = Object.create(null);
      // `_ids` maps entities such as `http://xmlns.com/foaf/0.1/name` to numbers,
      // saving memory by using only numbers as keys in `_graphs`
      this._id = 0;
      this._ids = Object.create(null);
      this._ids['><'] = 0; // dummy entry, so the first actual key is non-zero
      this._entities = Object.create(null); // inverse of `_ids`

      this._quads = new Map();

      // Add quads if passed
      if (quads) {
        for (const quad of quads) {
          this.add(quad);
        }
      }
    }

    get size () {
      // Return the quad count if if was cached
      let size = this._size;

      if (size !== null) {
        return size
      }

      // Calculate the number of quads by counting to the deepest level
      size = 0;
      const graphs = this._graphs;
      let subjects, subject;

      for (const graphKey in graphs) {
        for (const subjectKey in (subjects = graphs[graphKey].subjects)) {
          for (const predicateKey in (subject = subjects[subjectKey])) {
            size += Object.keys(subject[predicateKey]).length;
          }
        }
      }

      this._size = size;

      return this._size
    }

    add (quad) {
      // Convert terms to internal string representation
      let subject = termToId$1(quad.subject);
      let predicate = termToId$1(quad.predicate);
      let object = termToId$1(quad.object);
      const graph = termToId$1(quad.graph);

      // Find the graph that will contain the triple
      let graphItem = this._graphs[graph];
      // Create the graph if it doesn't exist yet
      if (!graphItem) {
        graphItem = this._graphs[graph] = { subjects: {}, predicates: {}, objects: {} };
        // Freezing a graph helps subsequent `add` performance,
        // and properties will never be modified anyway
        Object.freeze(graphItem);
      }

      // Since entities can often be long IRIs, we avoid storing them in every index.
      // Instead, we have a separate index that maps entities to numbers,
      // which are then used as keys in the other indexes.
      const ids = this._ids;
      const entities = this._entities;
      subject = ids[subject] || (ids[entities[++this._id] = subject] = this._id);
      predicate = ids[predicate] || (ids[entities[++this._id] = predicate] = this._id);
      object = ids[object] || (ids[entities[++this._id] = object] = this._id);

      this._addToIndex(graphItem.subjects, subject, predicate, object);
      this._addToIndex(graphItem.predicates, predicate, object, subject);
      this._addToIndex(graphItem.objects, object, subject, predicate);

      this._setQuad(subject, predicate, object, graph, quad);

      // The cached quad count is now invalid
      this._size = null;

      return this
    }

    delete (quad) {
      // Convert terms to internal string representation
      let subject = termToId$1(quad.subject);
      let predicate = termToId$1(quad.predicate);
      let object = termToId$1(quad.object);
      const graph = termToId$1(quad.graph);

      // Find internal identifiers for all components
      // and verify the quad exists.
      const ids = this._ids;
      const graphs = this._graphs;
      let graphItem, subjects, predicates;

      if (!(subject = ids[subject]) || !(predicate = ids[predicate]) ||
        !(object = ids[object]) || !(graphItem = graphs[graph]) ||
        !(subjects = graphItem.subjects[subject]) ||
        !(predicates = subjects[predicate]) ||
        !(object in predicates)
      ) {
        return this
      }

      // Remove it from all indexes
      this._removeFromIndex(graphItem.subjects, subject, predicate, object);
      this._removeFromIndex(graphItem.predicates, predicate, object, subject);
      this._removeFromIndex(graphItem.objects, object, subject, predicate);

      if (this._size !== null) {
        this._size--;
      }

      this._deleteQuad(subject, predicate, object, graph);

      // Remove the graph if it is empty
      for (subject in graphItem.subjects) { // eslint-disable-line no-unreachable-loop
        return this
      }

      delete graphs[graph];

      return this
    }

    has (quad) {
      // Convert terms to internal string representation
      const subject = termToId$1(quad.subject);
      const predicate = termToId$1(quad.predicate);
      const object = termToId$1(quad.object);
      const graph = termToId$1(quad.graph);

      const graphItem = this._graphs[graph];

      if (!graphItem) {
        return false
      }

      const ids = this._ids;
      let subjectId, predicateId, objectId;

      // Translate IRIs to internal index keys.
      if (
        (isString$1(subject) && !(subjectId = ids[subject])) ||
        (isString$1(predicate) && !(predicateId = ids[predicate])) ||
        (isString$1(object) && !(objectId = ids[object]))
      ) {
        return false
      }

      return this._countInIndex(graphItem.objects, objectId, subjectId, predicateId) === 1
    }

    match (subject, predicate, object, graph) {
      return this._createDataset(this._match(subject, predicate, object, graph))
    }

    [Symbol.iterator] () {
      return this._match()[Symbol.iterator]()
    }

    // ## Private methods

    // ### `_addToIndex` adds a quad to a three-layered index.
    // Returns if the index has changed, if the entry did not already exist.
    _addToIndex (index0, key0, key1, key2) {
      // Create layers as necessary
      const index1 = index0[key0] || (index0[key0] = {});
      const index2 = index1[key1] || (index1[key1] = {});
      // Setting the key to _any_ value signals the presence of the quad
      const existed = key2 in index2;

      if (!existed) {
        index2[key2] = null;
      }

      return !existed
    }

    // ### `_removeFromIndex` removes a quad from a three-layered index
    _removeFromIndex (index0, key0, key1, key2) {
      // Remove the quad from the index
      const index1 = index0[key0];
      const index2 = index1[key1];
      delete index2[key2];

      // Remove intermediary index layers if they are empty
      for (const key in index2) { // eslint-disable-line no-unreachable-loop
        return
      }

      delete index1[key1];

      for (const key in index1) { // eslint-disable-line no-unreachable-loop
        return
      }

      delete index0[key0];
    }

    // ### `_findInIndex` finds a set of quads in a three-layered index.
    // The index base is `index0` and the keys at each level are `key0`, `key1`, and `key2`.
    // Any of these keys can be undefined, which is interpreted as a wildcard.
    // `name0`, `name1`, and `name2` are the names of the keys at each level,
    // used when reconstructing the resulting quad
    // (for instance: _subject_, _predicate_, and _object_).
    // Finally, `graph` will be the graph of the created quads.
    // If `callback` is given, each result is passed through it
    // and iteration halts when it returns truthy for any quad.
    // If instead `array` is given, each result is added to the array.
    _findInIndex (index0, key0, key1, key2, name0, name1, name2, graph, callback, array) {
      let tmp, index1, index2;

      // If a key is specified, use only that part of index 0.
      if (key0) {
        (tmp = index0, index0 = {})[key0] = tmp[key0];
      }

      for (const value0 in index0) {
        index1 = index0[value0];

        if (index1) {
          // If a key is specified, use only that part of index 1.
          if (key1) {
            (tmp = index1, index1 = {})[key1] = tmp[key1];
          }

          for (const value1 in index1) {
            index2 = index1[value1];

            if (index2) {
              // If a key is specified, use only that part of index 2, if it exists.
              const values = key2 ? (key2 in index2 ? [key2] : []) : Object.keys(index2);
              // Create quads for all items found in index 2.
              for (let l = 0; l < values.length; l++) {
                const parts = {
                  [name0]: value0,
                  [name1]: value1,
                  [name2]: values[l]
                };

                const quad = this._getQuad(parts.subject, parts.predicate, parts.object, graph);

                if (array) {
                  array.push(quad);
                } else if (callback(quad)) {
                  return true
                }
              }
            }
          }
        }
      }

      return array
    }

    // ### `_countInIndex` counts matching quads in a three-layered index.
    // The index base is `index0` and the keys at each level are `key0`, `key1`, and `key2`.
    // Any of these keys can be undefined, which is interpreted as a wildcard.
    _countInIndex (index0, key0, key1, key2) {
      let count = 0;
      let tmp, index1, index2;

      // If a key is specified, count only that part of index 0
      if (key0) {
        (tmp = index0, index0 = {})[key0] = tmp[key0];
      }

      for (const value0 in index0) {
        index1 = index0[value0];

        if (index1) {
          // If a key is specified, count only that part of index 1
          if (key1) {
            (tmp = index1, index1 = {})[key1] = tmp[key1];
          }

          for (const value1 in index1) {
            index2 = index1[value1];

            if (index2) {
              if (key2) {
                // If a key is specified, count the quad if it exists
                (key2 in index2) && count++;
              } else {
                // Otherwise, count all quads
                count += Object.keys(index2).length;
              }
            }
          }
        }
      }

      return count
    }

    // ### `_getGraphs` returns an array with the given graph,
    // or all graphs if the argument is null or undefined.
    _getGraphs (graph) {
      if (!isString$1(graph)) {
        return this._graphs
      }

      return {
        [graph]: this._graphs[graph]
      }
    }

    _match (subject, predicate, object, graph) {
      // Convert terms to internal string representation
      subject = subject && termToId$1(subject);
      predicate = predicate && termToId$1(predicate);
      object = object && termToId$1(object);
      graph = graph && termToId$1(graph);

      const quads = [];
      const graphs = this._getGraphs(graph);
      const ids = this._ids;
      let content, subjectId, predicateId, objectId;

      // Translate IRIs to internal index keys.
      if (
        (isString$1(subject) && !(subjectId = ids[subject])) ||
        (isString$1(predicate) && !(predicateId = ids[predicate])) ||
        (isString$1(object) && !(objectId = ids[object]))
      ) {
        return quads
      }

      for (const graphId in graphs) {
        content = graphs[graphId];

        // Only if the specified graph contains triples, there can be results
        if (content) {
          // Choose the optimal index, based on what fields are present
          if (subjectId) {
            if (objectId) {
              // If subject and object are given, the object index will be the fastest
              this._findInIndex(content.objects, objectId, subjectId, predicateId, 'object', 'subject', 'predicate', graphId, null, quads);
            } else {
              // If only subject and possibly predicate are given, the subject index will be the fastest
              this._findInIndex(content.subjects, subjectId, predicateId, null, 'subject', 'predicate', 'object', graphId, null, quads);
            }
          } else if (predicateId) {
            // if only predicate and possibly object are given, the predicate index will be the fastest
            this._findInIndex(content.predicates, predicateId, objectId, null, 'predicate', 'object', 'subject', graphId, null, quads);
          } else if (objectId) {
            // If only object is given, the object index will be the fastest
            this._findInIndex(content.objects, objectId, null, null, 'object', 'subject', 'predicate', graphId, null, quads);
          } else {
            // If nothing is given, iterate subjects and predicates first
            this._findInIndex(content.subjects, null, null, null, 'subject', 'predicate', 'object', graphId, null, quads);
          }
        }
      }

      return quads
    }

    _getQuad (subjectId, predicateId, objectId, graphId) {
      return this._quads.get(this._toId(subjectId, predicateId, objectId, graphId))
    }

    _setQuad (subjectId, predicateId, objectId, graphId, quad) {
      this._quads.set(this._toId(subjectId, predicateId, objectId, graphId), quad);
    }

    _deleteQuad (subjectId, predicateId, objectId, graphId) {
      this._quads.delete(this._toId(subjectId, predicateId, objectId, graphId));
    }

    _createDataset (quads) {
      return new this.constructor(quads)
    }

    _toId (subjectId, predicateId, objectId, graphId) {
      return `${subjectId}:${predicateId}:${objectId}:${graphId}`
    }
  }

  var DatasetCore_1 = DatasetCore;

  function dataset (quads) {
    return new DatasetCore_1(quads)
  }

  var dataset_1 = Object.assign({ dataset }, dataModel);

  var commonjsGlobal = typeof globalThis !== 'undefined' ? globalThis : typeof window !== 'undefined' ? window : typeof global !== 'undefined' ? global : typeof self !== 'undefined' ? self : {};

  function getDefaultExportFromCjs (x) {
  	return x && x.__esModule && Object.prototype.hasOwnProperty.call(x, 'default') ? x['default'] : x;
  }

  function getAugmentedNamespace(n) {
  	if (n.__esModule) return n;
  	var a = Object.defineProperty({}, '__esModule', {value: true});
  	Object.keys(n).forEach(function (k) {
  		var d = Object.getOwnPropertyDescriptor(n, k);
  		Object.defineProperty(a, k, d.get ? d : {
  			enumerable: true,
  			get: function () {
  				return n[k];
  			}
  		});
  	});
  	return a;
  }

  function createCommonjsModule(fn) {
    var module = { exports: {} };
  	return fn(module, module.exports), module.exports;
  }

  var browserPonyfill = createCommonjsModule(function (module, exports) {
  var global = typeof self !== 'undefined' ? self : commonjsGlobal;
  var __self__ = (function () {
  function F() {
  this.fetch = false;
  this.DOMException = global.DOMException;
  }
  F.prototype = global;
  return new F();
  })();
  (function(self) {

  ((function (exports) {

    var support = {
      searchParams: 'URLSearchParams' in self,
      iterable: 'Symbol' in self && 'iterator' in Symbol,
      blob:
        'FileReader' in self &&
        'Blob' in self &&
        (function() {
          try {
            new Blob();
            return true
          } catch (e) {
            return false
          }
        })(),
      formData: 'FormData' in self,
      arrayBuffer: 'ArrayBuffer' in self
    };

    function isDataView(obj) {
      return obj && DataView.prototype.isPrototypeOf(obj)
    }

    if (support.arrayBuffer) {
      var viewClasses = [
        '[object Int8Array]',
        '[object Uint8Array]',
        '[object Uint8ClampedArray]',
        '[object Int16Array]',
        '[object Uint16Array]',
        '[object Int32Array]',
        '[object Uint32Array]',
        '[object Float32Array]',
        '[object Float64Array]'
      ];

      var isArrayBufferView =
        ArrayBuffer.isView ||
        function(obj) {
          return obj && viewClasses.indexOf(Object.prototype.toString.call(obj)) > -1
        };
    }

    function normalizeName(name) {
      if (typeof name !== 'string') {
        name = String(name);
      }
      if (/[^a-z0-9\-#$%&'*+.^_`|~]/i.test(name)) {
        throw new TypeError('Invalid character in header field name')
      }
      return name.toLowerCase()
    }

    function normalizeValue(value) {
      if (typeof value !== 'string') {
        value = String(value);
      }
      return value
    }

    // Build a destructive iterator for the value list
    function iteratorFor(items) {
      var iterator = {
        next: function() {
          var value = items.shift();
          return {done: value === undefined, value: value}
        }
      };

      if (support.iterable) {
        iterator[Symbol.iterator] = function() {
          return iterator
        };
      }

      return iterator
    }

    function Headers(headers) {
      this.map = {};

      if (headers instanceof Headers) {
        headers.forEach(function(value, name) {
          this.append(name, value);
        }, this);
      } else if (Array.isArray(headers)) {
        headers.forEach(function(header) {
          this.append(header[0], header[1]);
        }, this);
      } else if (headers) {
        Object.getOwnPropertyNames(headers).forEach(function(name) {
          this.append(name, headers[name]);
        }, this);
      }
    }

    Headers.prototype.append = function(name, value) {
      name = normalizeName(name);
      value = normalizeValue(value);
      var oldValue = this.map[name];
      this.map[name] = oldValue ? oldValue + ', ' + value : value;
    };

    Headers.prototype['delete'] = function(name) {
      delete this.map[normalizeName(name)];
    };

    Headers.prototype.get = function(name) {
      name = normalizeName(name);
      return this.has(name) ? this.map[name] : null
    };

    Headers.prototype.has = function(name) {
      return this.map.hasOwnProperty(normalizeName(name))
    };

    Headers.prototype.set = function(name, value) {
      this.map[normalizeName(name)] = normalizeValue(value);
    };

    Headers.prototype.forEach = function(callback, thisArg) {
      for (var name in this.map) {
        if (this.map.hasOwnProperty(name)) {
          callback.call(thisArg, this.map[name], name, this);
        }
      }
    };

    Headers.prototype.keys = function() {
      var items = [];
      this.forEach(function(value, name) {
        items.push(name);
      });
      return iteratorFor(items)
    };

    Headers.prototype.values = function() {
      var items = [];
      this.forEach(function(value) {
        items.push(value);
      });
      return iteratorFor(items)
    };

    Headers.prototype.entries = function() {
      var items = [];
      this.forEach(function(value, name) {
        items.push([name, value]);
      });
      return iteratorFor(items)
    };

    if (support.iterable) {
      Headers.prototype[Symbol.iterator] = Headers.prototype.entries;
    }

    function consumed(body) {
      if (body.bodyUsed) {
        return Promise.reject(new TypeError('Already read'))
      }
      body.bodyUsed = true;
    }

    function fileReaderReady(reader) {
      return new Promise(function(resolve, reject) {
        reader.onload = function() {
          resolve(reader.result);
        };
        reader.onerror = function() {
          reject(reader.error);
        };
      })
    }

    function readBlobAsArrayBuffer(blob) {
      var reader = new FileReader();
      var promise = fileReaderReady(reader);
      reader.readAsArrayBuffer(blob);
      return promise
    }

    function readBlobAsText(blob) {
      var reader = new FileReader();
      var promise = fileReaderReady(reader);
      reader.readAsText(blob);
      return promise
    }

    function readArrayBufferAsText(buf) {
      var view = new Uint8Array(buf);
      var chars = new Array(view.length);

      for (var i = 0; i < view.length; i++) {
        chars[i] = String.fromCharCode(view[i]);
      }
      return chars.join('')
    }

    function bufferClone(buf) {
      if (buf.slice) {
        return buf.slice(0)
      } else {
        var view = new Uint8Array(buf.byteLength);
        view.set(new Uint8Array(buf));
        return view.buffer
      }
    }

    function Body() {
      this.bodyUsed = false;

      this._initBody = function(body) {
        this._bodyInit = body;
        if (!body) {
          this._bodyText = '';
        } else if (typeof body === 'string') {
          this._bodyText = body;
        } else if (support.blob && Blob.prototype.isPrototypeOf(body)) {
          this._bodyBlob = body;
        } else if (support.formData && FormData.prototype.isPrototypeOf(body)) {
          this._bodyFormData = body;
        } else if (support.searchParams && URLSearchParams.prototype.isPrototypeOf(body)) {
          this._bodyText = body.toString();
        } else if (support.arrayBuffer && support.blob && isDataView(body)) {
          this._bodyArrayBuffer = bufferClone(body.buffer);
          // IE 10-11 can't handle a DataView body.
          this._bodyInit = new Blob([this._bodyArrayBuffer]);
        } else if (support.arrayBuffer && (ArrayBuffer.prototype.isPrototypeOf(body) || isArrayBufferView(body))) {
          this._bodyArrayBuffer = bufferClone(body);
        } else {
          this._bodyText = body = Object.prototype.toString.call(body);
        }

        if (!this.headers.get('content-type')) {
          if (typeof body === 'string') {
            this.headers.set('content-type', 'text/plain;charset=UTF-8');
          } else if (this._bodyBlob && this._bodyBlob.type) {
            this.headers.set('content-type', this._bodyBlob.type);
          } else if (support.searchParams && URLSearchParams.prototype.isPrototypeOf(body)) {
            this.headers.set('content-type', 'application/x-www-form-urlencoded;charset=UTF-8');
          }
        }
      };

      if (support.blob) {
        this.blob = function() {
          var rejected = consumed(this);
          if (rejected) {
            return rejected
          }

          if (this._bodyBlob) {
            return Promise.resolve(this._bodyBlob)
          } else if (this._bodyArrayBuffer) {
            return Promise.resolve(new Blob([this._bodyArrayBuffer]))
          } else if (this._bodyFormData) {
            throw new Error('could not read FormData body as blob')
          } else {
            return Promise.resolve(new Blob([this._bodyText]))
          }
        };

        this.arrayBuffer = function() {
          if (this._bodyArrayBuffer) {
            return consumed(this) || Promise.resolve(this._bodyArrayBuffer)
          } else {
            return this.blob().then(readBlobAsArrayBuffer)
          }
        };
      }

      this.text = function() {
        var rejected = consumed(this);
        if (rejected) {
          return rejected
        }

        if (this._bodyBlob) {
          return readBlobAsText(this._bodyBlob)
        } else if (this._bodyArrayBuffer) {
          return Promise.resolve(readArrayBufferAsText(this._bodyArrayBuffer))
        } else if (this._bodyFormData) {
          throw new Error('could not read FormData body as text')
        } else {
          return Promise.resolve(this._bodyText)
        }
      };

      if (support.formData) {
        this.formData = function() {
          return this.text().then(decode)
        };
      }

      this.json = function() {
        return this.text().then(JSON.parse)
      };

      return this
    }

    // HTTP methods whose capitalization should be normalized
    var methods = ['DELETE', 'GET', 'HEAD', 'OPTIONS', 'POST', 'PUT'];

    function normalizeMethod(method) {
      var upcased = method.toUpperCase();
      return methods.indexOf(upcased) > -1 ? upcased : method
    }

    function Request(input, options) {
      options = options || {};
      var body = options.body;

      if (input instanceof Request) {
        if (input.bodyUsed) {
          throw new TypeError('Already read')
        }
        this.url = input.url;
        this.credentials = input.credentials;
        if (!options.headers) {
          this.headers = new Headers(input.headers);
        }
        this.method = input.method;
        this.mode = input.mode;
        this.signal = input.signal;
        if (!body && input._bodyInit != null) {
          body = input._bodyInit;
          input.bodyUsed = true;
        }
      } else {
        this.url = String(input);
      }

      this.credentials = options.credentials || this.credentials || 'same-origin';
      if (options.headers || !this.headers) {
        this.headers = new Headers(options.headers);
      }
      this.method = normalizeMethod(options.method || this.method || 'GET');
      this.mode = options.mode || this.mode || null;
      this.signal = options.signal || this.signal;
      this.referrer = null;

      if ((this.method === 'GET' || this.method === 'HEAD') && body) {
        throw new TypeError('Body not allowed for GET or HEAD requests')
      }
      this._initBody(body);
    }

    Request.prototype.clone = function() {
      return new Request(this, {body: this._bodyInit})
    };

    function decode(body) {
      var form = new FormData();
      body
        .trim()
        .split('&')
        .forEach(function(bytes) {
          if (bytes) {
            var split = bytes.split('=');
            var name = split.shift().replace(/\+/g, ' ');
            var value = split.join('=').replace(/\+/g, ' ');
            form.append(decodeURIComponent(name), decodeURIComponent(value));
          }
        });
      return form
    }

    function parseHeaders(rawHeaders) {
      var headers = new Headers();
      // Replace instances of \r\n and \n followed by at least one space or horizontal tab with a space
      // https://tools.ietf.org/html/rfc7230#section-3.2
      var preProcessedHeaders = rawHeaders.replace(/\r?\n[\t ]+/g, ' ');
      preProcessedHeaders.split(/\r?\n/).forEach(function(line) {
        var parts = line.split(':');
        var key = parts.shift().trim();
        if (key) {
          var value = parts.join(':').trim();
          headers.append(key, value);
        }
      });
      return headers
    }

    Body.call(Request.prototype);

    function Response(bodyInit, options) {
      if (!options) {
        options = {};
      }

      this.type = 'default';
      this.status = options.status === undefined ? 200 : options.status;
      this.ok = this.status >= 200 && this.status < 300;
      this.statusText = 'statusText' in options ? options.statusText : 'OK';
      this.headers = new Headers(options.headers);
      this.url = options.url || '';
      this._initBody(bodyInit);
    }

    Body.call(Response.prototype);

    Response.prototype.clone = function() {
      return new Response(this._bodyInit, {
        status: this.status,
        statusText: this.statusText,
        headers: new Headers(this.headers),
        url: this.url
      })
    };

    Response.error = function() {
      var response = new Response(null, {status: 0, statusText: ''});
      response.type = 'error';
      return response
    };

    var redirectStatuses = [301, 302, 303, 307, 308];

    Response.redirect = function(url, status) {
      if (redirectStatuses.indexOf(status) === -1) {
        throw new RangeError('Invalid status code')
      }

      return new Response(null, {status: status, headers: {location: url}})
    };

    exports.DOMException = self.DOMException;
    try {
      new exports.DOMException();
    } catch (err) {
      exports.DOMException = function(message, name) {
        this.message = message;
        this.name = name;
        var error = Error(message);
        this.stack = error.stack;
      };
      exports.DOMException.prototype = Object.create(Error.prototype);
      exports.DOMException.prototype.constructor = exports.DOMException;
    }

    function fetch(input, init) {
      return new Promise(function(resolve, reject) {
        var request = new Request(input, init);

        if (request.signal && request.signal.aborted) {
          return reject(new exports.DOMException('Aborted', 'AbortError'))
        }

        var xhr = new XMLHttpRequest();

        function abortXhr() {
          xhr.abort();
        }

        xhr.onload = function() {
          var options = {
            status: xhr.status,
            statusText: xhr.statusText,
            headers: parseHeaders(xhr.getAllResponseHeaders() || '')
          };
          options.url = 'responseURL' in xhr ? xhr.responseURL : options.headers.get('X-Request-URL');
          var body = 'response' in xhr ? xhr.response : xhr.responseText;
          resolve(new Response(body, options));
        };

        xhr.onerror = function() {
          reject(new TypeError('Network request failed'));
        };

        xhr.ontimeout = function() {
          reject(new TypeError('Network request failed'));
        };

        xhr.onabort = function() {
          reject(new exports.DOMException('Aborted', 'AbortError'));
        };

        xhr.open(request.method, request.url, true);

        if (request.credentials === 'include') {
          xhr.withCredentials = true;
        } else if (request.credentials === 'omit') {
          xhr.withCredentials = false;
        }

        if ('responseType' in xhr && support.blob) {
          xhr.responseType = 'blob';
        }

        request.headers.forEach(function(value, name) {
          xhr.setRequestHeader(name, value);
        });

        if (request.signal) {
          request.signal.addEventListener('abort', abortXhr);

          xhr.onreadystatechange = function() {
            // DONE (success or failure)
            if (xhr.readyState === 4) {
              request.signal.removeEventListener('abort', abortXhr);
            }
          };
        }

        xhr.send(typeof request._bodyInit === 'undefined' ? null : request._bodyInit);
      })
    }

    fetch.polyfill = true;

    if (!self.fetch) {
      self.fetch = fetch;
      self.Headers = Headers;
      self.Request = Request;
      self.Response = Response;
    }

    exports.Headers = Headers;
    exports.Request = Request;
    exports.Response = Response;
    exports.fetch = fetch;

    Object.defineProperty(exports, '__esModule', { value: true });

    return exports;

  })({}));
  })(__self__);
  __self__.fetch.ponyfill = true;
  // Remove "polyfill" property added by whatwg-fetch
  delete __self__.fetch.polyfill;
  // Choose between native implementation (global) or custom implementation (__self__)
  // var ctx = global.fetch ? global : __self__;
  var ctx = __self__; // this line disable service worker support temporarily
  exports = ctx.fetch; // To enable: import fetch from 'cross-fetch'
  exports.default = ctx.fetch; // For TypeScript consumers without esModuleInterop.
  exports.fetch = ctx.fetch; // To enable: import {fetch} from 'cross-fetch'
  exports.Headers = ctx.Headers;
  exports.Request = ctx.Request;
  exports.Response = ctx.Response;
  module.exports = exports;
  });

  var browserPonyfill$1 = /*@__PURE__*/getDefaultExportFromCjs(browserPonyfill);

  var crossFetch = /*#__PURE__*/Object.freeze(/*#__PURE__*/Object.assign(/*#__PURE__*/Object.create(null), browserPonyfill, {
    'default': browserPonyfill$1
  }));

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Verify whether a given SolidDataset includes metadata about where it was sent to.
   *
   * @param dataset A [[SolidDataset]] that may have metadata attached about the Resource it was retrieved from.
   * @returns True if `dataset` includes metadata about the Resource it was sent to, false if not.
   * @since 0.2.0
   */
  function hasResourceInfo(resource) {
      const potentialResourceInfo = resource;
      return (typeof potentialResourceInfo === "object" &&
          typeof potentialResourceInfo.internal_resourceInfo === "object");
  }
  /**
   * Verify whether a given SolidDataset includes metadata about where it was retrieved from.
   *
   * @param dataset A [[SolidDataset]] that may have metadata attached about the Resource it was retrieved from.
   * @returns True if `dataset` includes metadata about the Resource it was retrieved from, false if not.
   * @since 0.6.0
   */
  function hasServerResourceInfo(resource) {
      const potentialResourceInfo = resource;
      return (typeof potentialResourceInfo === "object" &&
          typeof potentialResourceInfo.internal_resourceInfo === "object" &&
          typeof potentialResourceInfo.internal_resourceInfo.linkedResources ===
              "object");
  }
  /** @internal */
  function hasChangelog(dataset) {
      const potentialChangeLog = dataset;
      return (typeof potentialChangeLog.internal_changeLog === "object" &&
          Array.isArray(potentialChangeLog.internal_changeLog.additions) &&
          Array.isArray(potentialChangeLog.internal_changeLog.deletions));
  }
  /**
   * Errors thrown by solid-client extend this class, and can thereby be distinguished from errors
   * thrown in lower-level libraries.
   * @since 1.2.0
   */
  class SolidClientError extends Error {
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /** @internal */
  function internal_toIriString(iri) {
      return typeof iri === "string" ? iri : iri.value;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * @ignore Internal fallback for when no fetcher is provided; not to be used downstream.
   */
  const fetch = async (resource, init) => {
      /* istanbul ignore if: `require` is always defined in the unit test environment */
      if (typeof window === "object" && typeof require !== "function") {
          return await window.fetch(resource, init);
      }
      /* istanbul ignore if: `require` is always defined in the unit test environment */
      if (typeof require !== "function") {
          // When using Node.js with ES Modules, require is not defined:
          const crossFetchModule = await Promise.resolve().then(function () { return crossFetch; });
          const fetch = crossFetchModule.default;
          return fetch(resource, init);
      }
      // Implementation note: it's up to the client application to resolve these module names to the
      // respective npm packages. At least one commonly used tool (Webpack) is only able to do that if
      // the module names are literal strings.
      // Additionally, Webpack throws a warning in a way that halts compilation for at least Next.js
      // when using native Javascript dynamic imports (`import()`), whereas `require()` just logs a
      // warning. Since the use of package names instead of file names requires a bundles anyway, this
      // should not have any practical consequences. For more background, see:
      // https://github.com/webpack/webpack/issues/7713
      let fetch;
      // Unfortunately solid-client-authn-browser does not support a default session yet.
      // Once it does, we can auto-detect if it is available and use it as follows:
      // try {
      //   fetch = require("solid-client-authn-browser").fetch;
      // } catch (e) {
      // When enabling the above, make sure to add a similar try {...} catch block using `import`
      // statements in the elseif above.
      // eslint-disable-next-line prefer-const
      fetch = require("cross-fetch");
      // }
      return await fetch(resource, init);
  };

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * @internal
   */
  function internal_parseResourceInfo(response) {
      var _a, _b, _c;
      const contentTypeParts = (_b = (_a = response.headers.get("Content-Type")) === null || _a === void 0 ? void 0 : _a.split(";")) !== null && _b !== void 0 ? _b : [];
      // If the server offers a Turtle or JSON-LD serialisation on its own accord,
      // that tells us whether it is RDF data that the server can understand
      // (and hence can be updated with a PATCH request with SPARQL INSERT and DELETE statements),
      // in which case our SolidDataset-related functions should handle it.
      // For more context, see https://github.com/inrupt/solid-client-js/pull/214.
      const isSolidDataset = contentTypeParts.length > 0 &&
          ["text/turtle", "application/ld+json"].includes(contentTypeParts[0]);
      const resourceInfo = {
          sourceIri: response.url,
          isRawData: !isSolidDataset,
          contentType: (_c = response.headers.get("Content-Type")) !== null && _c !== void 0 ? _c : undefined,
          linkedResources: {},
      };
      const linkHeader = response.headers.get("Link");
      if (linkHeader) {
          const parsedLinks = link.parse(linkHeader);
          // Set ACL link
          const aclLinks = parsedLinks.get("rel", "acl");
          if (aclLinks.length === 1) {
              resourceInfo.aclUrl = new URL(aclLinks[0].uri, resourceInfo.sourceIri).href;
          }
          // Parse all link headers and expose them in a standard way
          // (this can replace the parsing of the ACL link above):
          resourceInfo.linkedResources = parsedLinks.refs.reduce((rels, ref) => {
              var _a;
              var _b;
              (_a = rels[_b = ref.rel]) !== null && _a !== void 0 ? _a : (rels[_b] = []);
              rels[ref.rel].push(new URL(ref.uri, resourceInfo.sourceIri).href);
              return rels;
          }, resourceInfo.linkedResources);
      }
      const wacAllowHeader = response.headers.get("WAC-Allow");
      if (wacAllowHeader) {
          resourceInfo.permissions = parseWacAllowHeader(wacAllowHeader);
      }
      return resourceInfo;
  }
  /**
   * Parse a WAC-Allow header into user and public access booleans.
   *
   * @param wacAllowHeader A WAC-Allow header in the format `user="read append write control",public="read"`
   * @see https://github.com/solid/solid-spec/blob/cb1373a369398d561b909009bd0e5a8c3fec953b/api-rest.md#wac-allow-headers
   */
  function parseWacAllowHeader(wacAllowHeader) {
      function parsePermissionStatement(permissionStatement) {
          const permissions = permissionStatement.split(" ");
          const writePermission = permissions.includes("write");
          return writePermission
              ? {
                  read: permissions.includes("read"),
                  append: true,
                  write: true,
                  control: permissions.includes("control"),
              }
              : {
                  read: permissions.includes("read"),
                  append: permissions.includes("append"),
                  write: false,
                  control: permissions.includes("control"),
              };
      }
      function getStatementFor(header, scope) {
          const relevantEntries = header
              .split(",")
              .map((rawEntry) => rawEntry.split("="))
              .filter((parts) => parts.length === 2 && parts[0].trim() === scope);
          // There should only be one statement with the given scope:
          if (relevantEntries.length !== 1) {
              return "";
          }
          const relevantStatement = relevantEntries[0][1].trim();
          // The given statement should be wrapped in double quotes to be valid:
          if (relevantStatement.charAt(0) !== '"' ||
              relevantStatement.charAt(relevantStatement.length - 1) !== '"') {
              return "";
          }
          // Return the statment without the wrapping quotes, e.g.: read append write control
          return relevantStatement.substring(1, relevantStatement.length - 1);
      }
      return {
          user: parsePermissionStatement(getStatementFor(wacAllowHeader, "user")),
          public: parsePermissionStatement(getStatementFor(wacAllowHeader, "public")),
      };
  }
  /** @hidden Used to instantiate a separate instance from input parameters */
  function internal_cloneResource(resource) {
      let clonedResource;
      if (typeof resource.slice === "function") {
          // If given Resource is a File:
          clonedResource = Object.assign(resource.slice(), Object.assign({}, resource));
      }
      else {
          // If it is just a plain object containing metadata:
          clonedResource = Object.assign({}, resource);
      }
      return clonedResource;
  }
  /** @internal */
  function internal_isUnsuccessfulResponse(response) {
      return !response.ok;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  // TODO: These should be replaced by auto-generated constants,
  //       if we can ensure that unused constants will be excluded from bundles.
  /** @hidden */
  const acl = {
      Authorization: "http://www.w3.org/ns/auth/acl#Authorization",
      AuthenticatedAgent: "http://www.w3.org/ns/auth/acl#AuthenticatedAgent",
      accessTo: "http://www.w3.org/ns/auth/acl#accessTo",
      agent: "http://www.w3.org/ns/auth/acl#agent",
      agentGroup: "http://www.w3.org/ns/auth/acl#agentGroup",
      agentClass: "http://www.w3.org/ns/auth/acl#agentClass",
      default: "http://www.w3.org/ns/auth/acl#default",
      defaultForNew: "http://www.w3.org/ns/auth/acl#defaultForNew",
      mode: "http://www.w3.org/ns/auth/acl#mode",
      origin: "http://www.w3.org/ns/auth/acl#origin",
  };
  /** @hidden */
  const rdf$2 = {
      type: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
  };
  /** @hidden */
  const ldp = {
      BasicContainer: "http://www.w3.org/ns/ldp#BasicContainer",
      Container: "http://www.w3.org/ns/ldp#Container",
      Resource: "http://www.w3.org/ns/ldp#Resource",
      contains: "http://www.w3.org/ns/ldp#contains",
  };
  /** @hidden */
  const foaf = {
      Agent: "http://xmlns.com/foaf/0.1/Agent",
  };
  /** @hidden */
  const acp = {
      AccessControlResource: "http://www.w3.org/ns/solid/acp#AccessControlResource",
      Policy: "http://www.w3.org/ns/solid/acp#Policy",
      AccessControl: "http://www.w3.org/ns/solid/acp#AccessControl",
      Read: "http://www.w3.org/ns/solid/acp#Read",
      Append: "http://www.w3.org/ns/solid/acp#Append",
      Write: "http://www.w3.org/ns/solid/acp#Write",
      /** @deprecated Removed from the ACP proposal, to be replaced by Matchers. */
      Rule: "http://www.w3.org/ns/solid/acp#Rule",
      Matcher: "http://www.w3.org/ns/solid/acp#Matcher",
      accessControl: "http://www.w3.org/ns/solid/acp#accessControl",
      apply: "http://www.w3.org/ns/solid/acp#apply",
      applyMembers: "http://www.w3.org/ns/solid/acp#applyMembers",
      allow: "http://www.w3.org/ns/solid/acp#allow",
      deny: "http://www.w3.org/ns/solid/acp#deny",
      allOf: "http://www.w3.org/ns/solid/acp#allOf",
      anyOf: "http://www.w3.org/ns/solid/acp#anyOf",
      noneOf: "http://www.w3.org/ns/solid/acp#noneOf",
      access: "http://www.w3.org/ns/solid/acp#access",
      accessMembers: "http://www.w3.org/ns/solid/acp#accessMembers",
      agent: "http://www.w3.org/ns/solid/acp#agent",
      group: "http://www.w3.org/ns/solid/acp#group",
      client: "http://www.w3.org/ns/solid/acp#client",
      PublicAgent: "http://www.w3.org/ns/solid/acp#PublicAgent",
      AuthenticatedAgent: "http://www.w3.org/ns/solid/acp#AuthenticatedAgent",
      CreatorAgent: "http://www.w3.org/ns/solid/acp#CreatorAgent",
  };
  /** @hidden */
  const solid = {
      PublicOidcClient: "http://www.w3.org/ns/solid/terms#PublicOidcClient",
  };

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /** @ignore For internal use only. */
  const internal_defaultFetchOptions = {
      fetch: fetch,
  };
  /**
   * Retrieve the information about a resource (e.g. access permissions) without
   * fetching the resource itself.
   *
   * @param url URL to fetch Resource metadata from.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch#Parameters).
   * @returns Promise resolving to the metadata describing the given Resource, or rejecting if fetching it failed.
   * @since 0.4.0
   */
  async function getResourceInfo(url, options = internal_defaultFetchOptions) {
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const response = await config.fetch(url, { method: "HEAD" });
      return responseToResourceInfo(response);
  }
  /**
   * Parse Solid metadata from a Response obtained by fetching a Resource from a Solid Pod,
   *
   * @param response A Fetch API Response. See {@link https://developer.mozilla.org/en-US/docs/Web/API/Response MDN}.
   * @returns Resource metadata readable by functions such as [[getSourceUrl]].
   * @hidden This interface is not exposed yet until we've tried it out in practice.
   */
  function responseToResourceInfo(response) {
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Fetching the metadata of the Resource at [${response.url}] failed: [${response.status}] [${response.statusText}].`, response);
      }
      const resourceInfo = internal_parseResourceInfo(response);
      return { internal_resourceInfo: resourceInfo };
  }
  /**
   * @param resource Resource for which to check whether it is a Container.
   * @returns Whether `resource` is a Container.
   */
  function isContainer(resource) {
      const containerUrl = hasResourceInfo(resource)
          ? getSourceUrl(resource)
          : internal_toIriString(resource);
      return containerUrl.endsWith("/");
  }
  /**
   * This function will tell you whether a given Resource contains raw data, or a SolidDataset.
   *
   * @param resource Resource for which to check whether it contains raw data.
   * @return Whether `resource` contains raw data.
   */
  function isRawData(resource) {
      return resource.internal_resourceInfo.isRawData;
  }
  /**
   * @param resource Resource for which to determine the Content Type.
   * @returns The Content Type, if known, or null if not known.
   */
  function getContentType$1(resource) {
      var _a;
      return (_a = resource.internal_resourceInfo.contentType) !== null && _a !== void 0 ? _a : null;
  }
  function getSourceUrl(resource) {
      if (hasResourceInfo(resource)) {
          return resource.internal_resourceInfo.sourceIri;
      }
      return null;
  }
  /** @hidden Alias of getSourceUrl for those who prefer to use IRI terminology. */
  const getSourceIri = getSourceUrl;
  /**
   * Given a Resource that exposes information about the owner of the Pod it is in, returns the WebID of that owner.
   *
   * Data about the owner of the Pod is exposed when the following conditions hold:
   * - The Pod server supports exposing the Pod owner
   * - The current user is allowed to see who the Pod owner is.
   *
   * If one or more of those conditions are false, this function will return `null`.
   *
   * @param resource A Resource that contains information about the owner of the Pod it is in.
   * @returns The WebID of the owner of the Pod the Resource is in, if provided, or `null` if not.
   * @since 0.6.0
   */
  function getPodOwner(resource) {
      var _a;
      if (!hasServerResourceInfo(resource)) {
          return null;
      }
      const podOwners = (_a = getLinkedResourceUrlAll(resource)["http://www.w3.org/ns/solid/terms#podOwner"]) !== null && _a !== void 0 ? _a : [];
      return podOwners.length === 1 ? podOwners[0] : null;
  }
  /**
   * Given a WebID and a Resource that exposes information about the owner of the Pod it is in, returns whether the given WebID is the owner of the Pod.
   *
   * Data about the owner of the Pod is exposed when the following conditions hold:
   * - The Pod server supports exposing the Pod owner
   * - The current user is allowed to see who the Pod owner is.
   *
   * If one or more of those conditions are false, this function will return `null`.
   *
   * @param webId The WebID of which to check whether it is the Pod Owner's.
   * @param resource A Resource that contains information about the owner of the Pod it is in.
   * @returns Whether the given WebID is the Pod Owner's, if the Pod Owner is exposed, or `null` if it is not exposed.
   * @since 0.6.0
   */
  function isPodOwner(webId, resource) {
      const podOwner = getPodOwner(resource);
      if (typeof podOwner !== "string") {
          return null;
      }
      return podOwner === webId;
  }
  /**
   * Get the URLs of Resources linked to the given Resource.
   *
   * Solid servers can link Resources to each other. For example, in servers
   * implementing Web Access Control, Resources can have an Access Control List
   * Resource linked to it via the `acl` relation.
   *
   * @param resource A Resource fetched from a Solid Pod.
   * @returns The URLs of Resources linked to the given Resource, indexed by the key that links them.
   * @since 1.7.0
   */
  function getLinkedResourceUrlAll(resource) {
      return resource.internal_resourceInfo.linkedResources;
  }
  /**
   * Get what access the current user has to the given Resource.
   *
   * This function can tell you what access the current user has for the given
   * Resource, allowing you to e.g. determine that changes to it will be rejected
   * before attempting to do so.
   * Additionally, for servers adhering to the Web Access Control specification,
   * it will tell you what access unauthenticated users have to the given Resource.
   *
   * @param resource A Resource fetched from a Solid Pod.
   * @returns What access the current user and, if supported by the server, unauthenticated users have to the given Resource.
   * @since 1.7.0
   */
  function getEffectiveAccess(resource) {
      var _a, _b, _c, _d, _e, _f, _g;
      if (typeof resource.internal_resourceInfo.permissions === "object") {
          return {
              user: {
                  read: resource.internal_resourceInfo.permissions.user.read,
                  append: resource.internal_resourceInfo.permissions.user.append,
                  write: resource.internal_resourceInfo.permissions.user.write,
              },
              public: {
                  read: resource.internal_resourceInfo.permissions.public.read,
                  append: resource.internal_resourceInfo.permissions.public.append,
                  write: resource.internal_resourceInfo.permissions.public.write,
              },
          };
      }
      const linkedResourceUrls = getLinkedResourceUrlAll(resource);
      return {
          user: {
              read: (_b = (_a = linkedResourceUrls[acp.allow]) === null || _a === void 0 ? void 0 : _a.includes(acp.Read)) !== null && _b !== void 0 ? _b : false,
              append: (_e = (((_c = linkedResourceUrls[acp.allow]) === null || _c === void 0 ? void 0 : _c.includes(acp.Append)) ||
                  ((_d = linkedResourceUrls[acp.allow]) === null || _d === void 0 ? void 0 : _d.includes(acp.Write)))) !== null && _e !== void 0 ? _e : false,
              write: (_g = (_f = linkedResourceUrls[acp.allow]) === null || _f === void 0 ? void 0 : _f.includes(acp.Write)) !== null && _g !== void 0 ? _g : false,
          },
      };
  }
  /**
   * Extends the regular JavaScript error object with access to the status code and status message.
   * @since 1.2.0
   */
  class FetchError extends SolidClientError {
      constructor(message, errorResponse) {
          super(message);
          this.response = errorResponse;
      }
      get statusCode() {
          return this.response.status;
      }
      get statusText() {
          return this.response.statusText;
      }
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const defaultGetFileOptions = {
      fetch: fetch,
  };
  const RESERVED_HEADERS = ["Slug", "If-None-Match", "Content-Type"];
  /**
   * Some of the headers must be set by the library, rather than directly.
   */
  function containsReserved(header) {
      return RESERVED_HEADERS.some((reserved) => header[reserved] !== undefined);
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Retrieves a file from a URL and returns the file as a blob.
   *
   * For example:
   *
   * ```
   * const fileBlob = await getFile("https://pod.example.com/some/file", { fetch: fetch });
   * ```
   *
   * For additional examples, see
   * [Read/Write Files](https://docs.inrupt.com/developer-tools/javascript/client-libraries/tutorial/read-write-files/#retrieve-a-file).
   *
   * @param url The URL of the file to return
   * @param options Fetching options: a custom fetcher and/or headers.
   * @returns The file as a blob.
   */
  async function getFile(input, options = defaultGetFileOptions) {
      const config = Object.assign(Object.assign({}, defaultGetFileOptions), options);
      const url = internal_toIriString(input);
      const response = await config.fetch(url, config.init);
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Fetching the File failed: [${response.status}] [${response.statusText}].`, response);
      }
      const resourceInfo = internal_parseResourceInfo(response);
      const data = await response.blob();
      const fileWithResourceInfo = Object.assign(data, {
          internal_resourceInfo: resourceInfo,
      });
      return fileWithResourceInfo;
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   * Deletes a file at a given URL.
   *
   * For example:
   *
   * ```
   * await deleteFile( "https://pod.example.com/some/file", { fetch: fetch });
   * ```
   *
   * For additional examples, see
   * [Read/Write Files](https://docs.inrupt.com/developer-tools/javascript/client-libraries/tutorial/read-write-files/#delete-a-file).
   *
   * @param file The URL of the file to delete
   */
  async function deleteFile(file, options = defaultGetFileOptions) {
      const config = Object.assign(Object.assign({}, defaultGetFileOptions), options);
      const url = hasResourceInfo(file)
          ? internal_toIriString(getSourceIri(file))
          : internal_toIriString(file);
      const response = await config.fetch(url, Object.assign(Object.assign({}, config.init), { method: "DELETE" }));
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Deleting the file at [${url}] failed: [${response.status}] [${response.statusText}].`, response);
      }
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Saves a file in an existing folder/Container associated with the given URL.
   *
   * For example:
   *
   * ```
   * const savedFile = await saveFileInContainer(
   *   "https://pod.example.com/some/existing/container/",
   *   new Blob(["This is a plain piece of text"], { type: "plain/text" }),
   *   { slug: "suggestedFileName.txt", contentType: "text/plain", fetch: fetch }
   * );
   * ```
   *
   * For additional example, see
   * [Read/Write Files](https://docs.inrupt.com/developer-tools/javascript/client-libraries/tutorial/read-write-files/#save-a-file-into-an-existing-container).
   *
   * In the `options` parameter,
   *
   * - You can suggest a file name in the `slug` field.  However, the Solid
   *   Server may or may not use the suggested `slug` as the file name.
   *
   * - *Recommended:* You can specify the [media type](https://developer.mozilla.org/en-US/docs/Glossary/MIME_type)
   *   of the file in the `contentType`.  If unspecified, the function uses the default type of
   *   `application/octet-stream`, indicating a binary data file.
   *
   * The function saves a file into an *existing* Container. If the
   * Container does not exist, either:
   * - Create the Container first using [[createContainerAt]], and then
   *   use the function, or
   * - Use [[overwriteFile]] to save the file. [[overwriteFile]] creates
   *   the Containers in the saved file path as needed.
   *
   * Users who only have `Append` but not `Write` access to a Container
   * can use [[saveFileInContainer]] to save new files to the Container.
   * That is, [[saveFileInContainer]] is useful in situations where users
   * can add new files to a Container but not change existing files in
   * the Container, such as users given access to send notifications to
   * another's Pod but not to view or delete existing notifications in that Pod.
   *
   * Users with `Write` access to the given folder/Container may prefer to
   * use [[overwriteFile]].
   *
   * @param folderUrl The URL of an existing folder where the new file is saved.
   * @param file The file to be written.
   * @param options Additional parameters for file creation (e.g. a slug).
   * @returns A Promise that resolves to the saved file, if available, or `null` if the current user does not have Read access to the newly-saved file. It rejects if saving fails.
   */
  async function saveFileInContainer(folderUrl, file, options = defaultGetFileOptions) {
      const folderUrlString = internal_toIriString(folderUrl);
      const response = await writeFile(folderUrlString, file, "POST", options);
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Saving the file in [${folderUrl}] failed: [${response.status}] [${response.statusText}].`, response);
      }
      const locationHeader = response.headers.get("Location");
      if (locationHeader === null) {
          throw new Error("Could not determine the location of the newly saved file.");
      }
      const fileIri = new URL(locationHeader, new URL(folderUrlString).origin).href;
      const blobClone = internal_cloneResource(file);
      const resourceInfo = {
          internal_resourceInfo: {
              isRawData: true,
              sourceIri: fileIri,
              contentType: getContentType(file, options.contentType),
          },
      };
      return Object.assign(blobClone, resourceInfo);
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Saves a file at a given URL. If a file already exists at the URL,
   * the function overwrites the existing file.
   *
   * For example:
   *
   * ```
   * const savedFile = await overwriteFile(
   *   "https://pod.example.com/some/container/myFile.txt",
   *   new Blob(["This is a plain piece of text"], { type: "plain/text" }),
   *   { contentType: "text/plain", fetch: fetch }
   * );
   * ```
   *
   * For additional example, see
   * [Read/Write Files](https://docs.inrupt.com/developer-tools/javascript/client-libraries/tutorial/read-write-files/#write-a-file-to-a-specific-url).
   *
   * *Recommended:* In the `options` parameter, you can specify the
   * [media type](https://developer.mozilla.org/en-US/docs/Glossary/MIME_type)
   * of the file in the `contentType`.  If unspecified, the function uses the default type of
   * `application/octet-stream`, indicating a binary data file.
   *
   * When saving a file with [[overwriteFile]], the Solid server creates any
   * intermediary Containers as needed; i.e., the Containers do not
   * need to be created in advance. For example, when saving a file to the target URL of
   * https://example.pod/container/resource, if https://example.pod/container/ does not exist,
   * the container is created as part of the save.
   *
   * @param fileUrl The URL where the file is saved.
   * @param file The file to be written.
   * @param options Additional parameters for file creation (e.g., media type).
   */
  async function overwriteFile(fileUrl, file, options = defaultGetFileOptions) {
      const fileUrlString = internal_toIriString(fileUrl);
      const response = await writeFile(fileUrlString, file, "PUT", options);
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Overwriting the file at [${fileUrlString}] failed: [${response.status}] [${response.statusText}].`, response);
      }
      const blobClone = internal_cloneResource(file);
      const resourceInfo = internal_parseResourceInfo(response);
      resourceInfo.sourceIri = fileUrlString;
      resourceInfo.isRawData = true;
      return Object.assign(blobClone, { internal_resourceInfo: resourceInfo });
  }
  function isHeadersArray(headers) {
      return Array.isArray(headers);
  }
  /**
   * The return type of this function is misleading: it should ONLY be used to check
   * whether an object has a forEach method that returns <key, value> pairs.
   *
   * @param headers A headers object that might have a forEach
   */
  function hasHeadersObjectForEach(headers) {
      return typeof headers.forEach === "function";
  }
  /**
   * @hidden
   * This function feels unnecessarily complicated, but is required in order to
   * have Headers according to type definitions in both Node and browser environments.
   * This might require a fix upstream to be cleaned up.
   *
   * @param headersToFlatten A structure containing headers potentially in several formats
   */
  function flattenHeaders(headersToFlatten) {
      if (typeof headersToFlatten === "undefined") {
          return {};
      }
      let flatHeaders = {};
      if (isHeadersArray(headersToFlatten)) {
          headersToFlatten.forEach(([key, value]) => {
              flatHeaders[key] = value;
          });
          // Note that the following line must be a elsif, because string[][] has a forEach,
          // but it returns string[] instead of <key, value>
      }
      else if (hasHeadersObjectForEach(headersToFlatten)) {
          headersToFlatten.forEach((value, key) => {
              flatHeaders[key] = value;
          });
      }
      else {
          // If the headers are already a Record<string, string>,
          // they can directly be returned.
          flatHeaders = headersToFlatten;
      }
      return flatHeaders;
  }
  /**
   * Internal function that performs the actual write HTTP query, either POST
   * or PUT depending on the use case.
   *
   * @param fileUrl The URL where the file is saved
   * @param file The file to be written
   * @param method The HTTP method
   * @param options Additional parameters for file creation (e.g. a slug, or media type)
   */
  async function writeFile(targetUrl, file, method, options) {
      var _a, _b;
      const config = Object.assign(Object.assign({}, defaultGetFileOptions), options);
      const headers = flattenHeaders((_b = (_a = config.init) === null || _a === void 0 ? void 0 : _a.headers) !== null && _b !== void 0 ? _b : {});
      if (containsReserved(headers)) {
          throw new Error(`No reserved header (${RESERVED_HEADERS.join(", ")}) should be set in the optional RequestInit.`);
      }
      // If a slug is in the parameters, set the request headers accordingly
      if (config.slug !== undefined) {
          headers["Slug"] = config.slug;
      }
      headers["Content-Type"] = getContentType(file, options.contentType);
      const targetUrlString = internal_toIriString(targetUrl);
      return await config.fetch(targetUrlString, Object.assign(Object.assign({}, config.init), { headers,
          method, body: file }));
  }
  function getContentType(file, contentTypeOverride) {
      if (typeof contentTypeOverride === "string") {
          return contentTypeOverride;
      }
      const fileType = typeof file === "object" &&
          file !== null &&
          typeof file.type === "string" &&
          file.type.length > 0
          ? file.type
          : undefined;
      return fileType !== null && fileType !== void 0 ? fileType : "application/octet-stream";
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  dataset_1.dataset;
  const localNodeSkolemPrefix = "https://inrupt.com/.well-known/sdk-local-node/";
  /**
   * Runtime freezing might be too much overhead;
   * if so, this function allows us to replace it by a function
   * that merely marks its input as Readonly<> for static analysis.
   */
  const freeze = Object.freeze;
  function isLocalNodeIri(iri) {
      return (iri.substring(0, localNodeSkolemPrefix.length) === localNodeSkolemPrefix);
  }
  function getLocalNodeName(localNodeIri) {
      return localNodeIri.substring(localNodeSkolemPrefix.length);
  }
  function getLocalNodeIri(localNodeName) {
      return `${localNodeSkolemPrefix}${localNodeName}`;
  }
  function isBlankNodeId(value) {
      return typeof value === "string" && value.substring(0, 2) === "_:";
  }
  function getBlankNodeValue(blankNodeId) {
      return blankNodeId.substring(2);
  }
  function getBlankNodeId(blankNode) {
      return `_:${blankNode.value}`;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * IRIs of the XML Schema data types we support
   * @internal
   */
  const xmlSchemaTypes = {
      boolean: "http://www.w3.org/2001/XMLSchema#boolean",
      dateTime: "http://www.w3.org/2001/XMLSchema#dateTime",
      date: "http://www.w3.org/2001/XMLSchema#date",
      time: "http://www.w3.org/2001/XMLSchema#time",
      decimal: "http://www.w3.org/2001/XMLSchema#decimal",
      integer: "http://www.w3.org/2001/XMLSchema#integer",
      string: "http://www.w3.org/2001/XMLSchema#string",
      langString: "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString",
  };
  /**
   * @internal
   * @param value Value to serialise.
   * @returns String representation of `value`.
   * @see https://www.w3.org/TR/xmlschema-2/#boolean-lexical-representation
   */
  function serializeBoolean(value) {
      return value ? "true" : "false";
  }
  /**
   * @internal
   * @param value Value to deserialise.
   * @returns Deserialized boolean, or null if the given value is not a valid serialised boolean.
   * @see https://www.w3.org/TR/xmlschema-2/#boolean-lexical-representation
   */
  function deserializeBoolean(value) {
      if (value === "true" || value === "1") {
          return true;
      }
      else if (value === "false" || value === "0") {
          return false;
      }
      else {
          return null;
      }
  }
  /**
   * @internal
   * @param value Value to serialise.
   * @returns String representation of `value` in UTC.
   * @see https://www.w3.org/TR/xmlschema-2/#time-lexical-repr
   */
  function serializeTime(value) {
      let millisecondString;
      let timezoneString;
      if (value.millisecond) {
          if (value.millisecond < 10) {
              millisecondString = "00" + value.millisecond;
          }
          else if (value.millisecond < 100) {
              millisecondString = "0" + value.millisecond;
          }
          else {
              millisecondString = value.millisecond;
          }
      }
      if (typeof value.timezoneHourOffset === "number") {
          const timezoneFormatted = Math.abs(value.timezoneHourOffset) < 10
              ? "0" + Math.abs(value.timezoneHourOffset)
              : Math.abs(value.timezoneHourOffset);
          timezoneString =
              value.timezoneHourOffset >= 0
                  ? "+" + timezoneFormatted
                  : "-" + timezoneFormatted;
          if (value.timezoneMinuteOffset) {
              timezoneString =
                  timezoneString +
                      ":" +
                      (value.timezoneMinuteOffset < 10
                          ? "0" + value.timezoneMinuteOffset
                          : value.timezoneMinuteOffset);
          }
          else {
              timezoneString = timezoneString + ":00";
          }
      }
      return ((value.hour < 10 ? "0" + value.hour : value.hour) +
          ":" +
          (value.minute < 10 ? "0" + value.minute : value.minute) +
          ":" +
          (value.second < 10 ? "0" + value.second : value.second) +
          (value.millisecond ? "." + millisecondString : "") +
          (timezoneString ? timezoneString : ""));
  }
  /**
   * @internal
   * @param literalString Value to deserialise.
   * @returns Deserialized time, or null if the given value is not a valid serialised datetime.
   * @see https://www.w3.org/TR/xmlschema-2/#time-lexical-repr
   */
  function deserializeTime(literalString) {
      // Time in the format described at
      // https://www.w3.org/TR/xmlschema-2/#time-lexical-repr
      // \d\d:\d\d:\d\d - Two digits for the hour, minute and second, respectively, separated by a `:`.
      //                  Example: "13:37:42".
      // (\.\d+)? - Optionally a `.` followed by one or more digits representing milliseconds.
      //            Example: ".1337".
      // (Z|(\+|-)\d\d:\d\d) - The letter Z indicating UTC, or a `+` or `-` followed by two digits for
      //                       the hour offset and two for the minute offset, separated by a `:`.
      //                       Example: "+13:37".
      const timeRegEx = /\d\d:\d\d:\d\d(\.\d+)?(Z|(\+|-)\d\d:\d\d)?/;
      if (!timeRegEx.test(literalString)) {
          return null;
      }
      const [timeString, timezoneString] = splitTimeFromTimezone(literalString);
      const [hourString, minuteString, timeRest] = timeString.split(":");
      let utcHours = Number.parseInt(hourString, 10);
      let utcMinutes = Number.parseInt(minuteString, 10);
      const [secondString, optionalMillisecondString] = timeRest.split(".");
      const utcSeconds = Number.parseInt(secondString, 10);
      const utcMilliseconds = optionalMillisecondString
          ? Number.parseInt(optionalMillisecondString, 10)
          : undefined;
      if (utcMinutes >= 60) {
          utcHours = utcHours + 1;
          utcMinutes = utcMinutes - 60;
      }
      const deserializedTime = {
          hour: utcHours,
          minute: utcMinutes,
          second: utcSeconds,
      };
      if (typeof utcMilliseconds === "number") {
          deserializedTime.millisecond = utcMilliseconds;
      }
      if (typeof timezoneString === "string") {
          const [hourOffset, minuteOffset] = getTimezoneOffsets(timezoneString);
          if (typeof hourOffset !== "number" ||
              hourOffset > 24 ||
              typeof minuteOffset !== "number" ||
              minuteOffset > 59) {
              return null;
          }
          deserializedTime.timezoneHourOffset = hourOffset;
          deserializedTime.timezoneMinuteOffset = minuteOffset;
      }
      return deserializedTime;
  }
  /**
   * @internal
   * @param value Value to serialise.
   * @returns String representation of `value`.
   * @see https://www.w3.org/TR/xmlschema-2/#dateTime-lexical-representation
   */
  function serializeDatetime(value) {
      // Although the XML Schema DateTime is not _exactly_ an ISO 8601 string
      // (see https://www.w3.org/TR/xmlschema-2/#deviantformats),
      // the deviations only affect the parsing, not the serialisation.
      // Therefore, we can just use .toISOString():
      return value.toISOString();
  }
  /**
   * @internal
   * @param value Value to deserialise.
   * @returns Deserialized datetime, or null if the given value is not a valid serialised datetime.
   * @see https://www.w3.org/TR/xmlschema-2/#dateTime-lexical-representation
   */
  function deserializeDatetime(literalString) {
      // DateTime in the format described at
      // https://www.w3.org/TR/xmlschema-2/#dateTime-lexical-representation
      // (without constraints on the value).
      // -? - An optional leading `-`.
      // \d{4,}- - Four or more digits followed by a `-` representing the year. Example: "3000-".
      // \d\d-\d\d - Two digits representing the month and two representing the day of the month,
      //             separated by a `-`. Example: "11-03".
      // T - The letter T, separating the date from the time.
      // \d\d:\d\d:\d\d - Two digits for the hour, minute and second, respectively, separated by a `:`.
      //                  Example: "13:37:42".
      // (\.\d+)? - Optionally a `.` followed by one or more digits representing milliseconds.
      //            Example: ".1337".
      // (Z|(\+|-)\d\d:\d\d) - The letter Z indicating UTC, or a `+` or `-` followed by two digits for
      //                       the hour offset and two for the minute offset, separated by a `:`.
      //                       Example: "+13:37".
      const datetimeRegEx = /-?\d{4,}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?(Z|(\+|-)\d\d:\d\d)?/;
      if (!datetimeRegEx.test(literalString)) {
          return null;
      }
      const [signedDateString, rest] = literalString.split("T");
      // The date string can optionally be prefixed with `-`,
      // in which case the year is negative:
      const [yearMultiplier, dateString] = signedDateString.charAt(0) === "-"
          ? [-1, signedDateString.substring(1)]
          : [1, signedDateString];
      const [yearString, monthString, dayString] = dateString.split("-");
      const utcFullYear = Number.parseInt(yearString, 10) * yearMultiplier;
      const utcMonth = Number.parseInt(monthString, 10) - 1;
      const utcDate = Number.parseInt(dayString, 10);
      const [timeString, timezoneString] = splitTimeFromTimezone(rest);
      const [hourOffset, minuteOffset] = typeof timezoneString === "string"
          ? getTimezoneOffsets(timezoneString)
          : [0, 0];
      const [hourString, minuteString, timeRest] = timeString.split(":");
      const utcHours = Number.parseInt(hourString, 10) + hourOffset;
      const utcMinutes = Number.parseInt(minuteString, 10) + minuteOffset;
      const [secondString, optionalMillisecondString] = timeRest.split(".");
      const utcSeconds = Number.parseInt(secondString, 10);
      const utcMilliseconds = optionalMillisecondString
          ? Number.parseInt(optionalMillisecondString, 10)
          : 0;
      const date = new Date(Date.UTC(utcFullYear, utcMonth, utcDate, utcHours, utcMinutes, utcSeconds, utcMilliseconds));
      // For the year, values from 0 to 99 map to the years 1900 to 1999. Since the serialisation
      // always writes out the years fully, we should correct this to actually map to the years 0 to 99.
      // See
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date#Individual_date_and_time_component_values
      if (utcFullYear >= 0 && utcFullYear < 100) {
          // Note that we base it on the calculated year, rather than the year that was actually read.
          // This is because the year might actually differ from the value listed in the serialisation,
          // i.e. when moving the timezone offset to UTC pushes it into a different year:
          date.setUTCFullYear(date.getUTCFullYear() - 1900);
      }
      return date;
  }
  /**
   * @internal
   * @param value Value to serialise.
   * @returns String representation of `value`.
   * @see https://www.w3.org/TR/xmlschema-2/#date-lexical-representation
   */
  function serializeDate(value) {
      const year = value.getFullYear();
      const month = value.getMonth() + 1;
      const day = value.getDate();
      const [_, timezone] = splitTimeFromTimezone(value.toISOString());
      return `${year}-${String(month).padStart(2, "0")}-${String(day).padStart(2, "0")}${timezone}`;
  }
  /**
   * @internal
   * @param value Value to deserialise.
   * @returns Deserialized datetime, or null if the given value is not a valid serialised datetime.
   * @see https://www.w3.org/TR/xmlschema-2/#date-lexical-representation
   */
  function deserializeDate(literalString) {
      // Date in the format described at
      // https://www.w3.org/TR/xmlschema-2/#date-lexical-representation
      // (without constraints on the value).
      // -? - An optional leading `-`.
      // \d{4,}- - Four or more digits followed by a `-` representing the year. Example: "3000-".
      // \d\d-\d\d - Two digits representing the month and two representing the day of the month,
      //             separated by a `-`. Example: "11-03".
      // (Z|(\+|-)\d\d:\d\d) - Optionally, the letter Z indicating UTC, or a `+` or `-` followed by two digits for
      //                       the hour offset and two for the minute offset, separated by a `:`.
      //                       Example: "+13:37".
      const dateRegEx = /-?\d{4,}-\d\d-\d\d(Z|(\+|-)\d\d:\d\d)?/;
      if (!dateRegEx.test(literalString)) {
          return null;
      }
      const signedDateString = literalString;
      // The date string can optionally be prefixed with `-`,
      // in which case the year is negative:
      const [yearMultiplier, dateString] = signedDateString.charAt(0) === "-"
          ? [-1, signedDateString.substring(1)]
          : [1, signedDateString];
      const [yearString, monthString, dayAndTimezoneString] = dateString.split("-");
      const dayString = dayAndTimezoneString.length > 2
          ? dayAndTimezoneString.substring(0, 2)
          : dayAndTimezoneString;
      const utcFullYear = Number.parseInt(yearString, 10) * yearMultiplier;
      const utcMonth = Number.parseInt(monthString, 10) - 1;
      const utcDate = Number.parseInt(dayString, 10);
      const hour = 12;
      // setting at 12:00 avoids all timezones
      const date = new Date(Date.UTC(utcFullYear, utcMonth, utcDate, hour));
      // For the year, values from 0 to 99 map to the years 1900 to 1999. Since the serialisation
      // always writes out the years fully, we should correct this to actually map to the years 0 to 99.
      // See
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date#Individual_date_and_time_component_values
      if (utcFullYear >= 0 && utcFullYear < 100) {
          date.setUTCFullYear(date.getUTCFullYear() - 1900);
      }
      return date;
  }
  /**
   * @param timeString An XML Schema time string.
   * @returns A tuple [timeString, timezoneString].
   * @see https://www.w3.org/TR/xmlschema-2/#time-lexical-repr
   */
  function splitTimeFromTimezone(timeString) {
      if (timeString.endsWith("Z")) {
          return [timeString.substring(0, timeString.length - 1), "Z"];
      }
      const splitOnPlus = timeString.split("+");
      const splitOnMinus = timeString.split("-");
      if (splitOnPlus.length === 1 && splitOnMinus.length === 1) {
          return [splitOnPlus[0], undefined];
      }
      return splitOnPlus.length > splitOnMinus.length
          ? [splitOnPlus[0], "+" + splitOnPlus[1]]
          : [splitOnMinus[0], "-" + splitOnMinus[1]];
  }
  /**
   * @param timezoneString Lexical representation of a time zone in XML Schema.
   * @returns A tuple of the hour and minute offset of the time zone.
   * @see https://www.w3.org/TR/xmlschema-2/#dateTime-timezones
   */
  function getTimezoneOffsets(timezoneString) {
      if (timezoneString === "Z") {
          return [0, 0];
      }
      const multiplier = timezoneString.charAt(0) === "+" ? 1 : -1;
      const [hourString, minuteString] = timezoneString.substring(1).split(":");
      const hours = Number.parseInt(hourString, 10);
      const minutes = Number.parseInt(minuteString, 10);
      return [hours * multiplier, minutes * multiplier];
  }
  /**
   * @internal
   * @param value Value to serialise.
   * @returns String representation of `value`.
   * @see https://www.w3.org/TR/xmlschema-2/#decimal-lexical-representation
   */
  function serializeDecimal(value) {
      return value.toString();
  }
  /**
   * @internal
   * @param value Value to deserialise.
   * @returns Deserialized decimal, or null if the given value is not a valid serialised decimal.
   * @see https://www.w3.org/TR/xmlschema-2/#decimal-lexical-representation
   */
  function deserializeDecimal(literalString) {
      const deserialized = Number.parseFloat(literalString);
      if (Number.isNaN(deserialized)) {
          return null;
      }
      return deserialized;
  }
  /**
   * @internal
   * @param value Value to serialise.
   * @returns String representation of `value`.
   */
  function serializeInteger(value) {
      return value.toString();
  }
  /**
   * @internal
   * @param value Value to deserialise.
   * @returns Deserialized integer, or null if the given value is not a valid serialised integer.
   */
  function deserializeInteger(literalString) {
      const deserialized = Number.parseInt(literalString, 10);
      if (Number.isNaN(deserialized)) {
          return null;
      }
      return deserialized;
  }
  /**
   * @internal
   * @param locale Locale to transform into a consistent format.
   */
  function normalizeLocale(locale) {
      return locale.toLowerCase();
  }
  /**
   * @internal Library users shouldn't need to be exposed to raw NamedNodes.
   * @param value The value that might or might not be a Named Node.
   * @returns Whether `value` is a Named Node.
   */
  function isNamedNode$1(value) {
      return isTerm(value) && value.termType === "NamedNode";
  }
  /**
   * @internal Library users shouldn't need to be exposed to raw Literals.
   * @param value The value that might or might not be a Literal.
   * @returns Whether `value` is a Literal.
   */
  function isLiteral$1(value) {
      return isTerm(value) && value.termType === "Literal";
  }
  /**
   * @internal Library users shouldn't need to be exposed to raw Terms.
   * @param value The value that might or might not be a Term.
   * @returns Whether `value` is a Term.
   */
  function isTerm(value) {
      return (value !== null &&
          typeof value === "object" &&
          typeof value.termType === "string" &&
          typeof value.value === "string" &&
          typeof value.equals === "function");
  }
  /**
   * @internal Library users shouldn't need to be exposed to LocalNodes.
   * @param value The value that might or might not be a Node with no known IRI yet.
   * @returns Whether `value` is a Node with no known IRI yet.
   */
  function isLocalNode(value) {
      return isNamedNode$1(value) && isLocalNodeIri(value.value);
  }
  /**
   * Ensure that a given value is a valid URL.
   *
   * @internal Library users shouldn't need to be exposed to raw URLs.
   * @param iri The value of which to verify that it is a valid URL.
   */
  function internal_isValidUrl(iri) {
      const iriString = internal_toIriString(iri);
      // If the runtime environment supports URL, instantiate one.
      // If the given IRI is not a valid URL, it will throw an error.
      // See: https://developer.mozilla.org/en-US/docs/Web/API/URL
      /* istanbul ignore if [URL is available in our testing environment, so we cannot test the alternative] */
      if (typeof URL !== "function") {
          // If we can't validate the URL, do not throw an error:
          return true;
      }
      try {
          new URL(iriString);
      }
      catch (_a) {
          return false;
      }
      return true;
  }
  /**
   * @internal Utility method; library users should not need to interact with LocalNodes directly.
   * @param localNode The LocalNode to resolve to a NamedNode.
   * @param resourceIri The Resource in which the Node will be saved.
   */
  function resolveIriForLocalNode(localNode, resourceIri) {
      return DataFactory$1.namedNode(resolveLocalIri(getLocalNodeName(localNode.value), resourceIri));
  }
  /**
   * @internal API for internal use only.
   * @param name The name identifying a Thing.
   * @param resourceIri The Resource in which the Thing can be found.
   */
  function resolveLocalIri(name, resourceIri) {
      /* istanbul ignore if [The URL interface is available in the testing environment, so we cannot test this] */
      if (typeof URL !== "function") {
          throw new Error("The URL interface is not available, so an IRI cannot be determined.");
      }
      const thingIri = new URL(resourceIri);
      thingIri.hash = name;
      return thingIri.href;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const DataFactory$1 = dataModel;
  function addRdfJsQuadToDataset(dataset, quad, quadParseOptions = {}) {
      var _a;
      const supportedGraphTypes = [
          "NamedNode",
          "DefaultGraph",
      ];
      if (!supportedGraphTypes.includes(quad.graph.termType)) {
          throw new Error(`Cannot parse Quads with nodes of type [${quad.graph.termType}] as their Graph node.`);
      }
      const graphId = quad.graph.termType === "DefaultGraph" ? "default" : quad.graph.value;
      const graph = (_a = dataset.graphs[graphId]) !== null && _a !== void 0 ? _a : {};
      return freeze(Object.assign(Object.assign({}, dataset), { graphs: freeze(Object.assign(Object.assign({}, dataset.graphs), { [graphId]: addRdfJsQuadToGraph(graph, quad, quadParseOptions) })) }));
  }
  function addRdfJsQuadToGraph(graph, quad, quadParseOptions) {
      var _a;
      const supportedSubjectTypes = [
          "NamedNode",
          "BlankNode",
      ];
      if (!supportedSubjectTypes.includes(quad.subject.termType)) {
          throw new Error(`Cannot parse Quads with nodes of type [${quad.subject.termType}] as their Subject node.`);
      }
      const subjectIri = quad.subject.termType === "BlankNode"
          ? `_:${quad.subject.value}`
          : quad.subject.value;
      const subject = (_a = graph[subjectIri]) !== null && _a !== void 0 ? _a : {
          type: "Subject",
          url: subjectIri,
          predicates: {},
      };
      return freeze(Object.assign(Object.assign({}, graph), { [subjectIri]: addRdfJsQuadToSubject(subject, quad, quadParseOptions) }));
  }
  function addRdfJsQuadToSubject(subject, quad, quadParseOptions) {
      return freeze(Object.assign(Object.assign({}, subject), { predicates: addRdfJsQuadToPredicates(subject.predicates, quad, quadParseOptions) }));
  }
  function addRdfJsQuadToPredicates(predicates, quad, quadParseOptions) {
      var _a;
      const supportedPredicateTypes = [
          "NamedNode",
      ];
      if (!supportedPredicateTypes.includes(quad.predicate.termType)) {
          throw new Error(`Cannot parse Quads with nodes of type [${quad.predicate.termType}] as their Predicate node.`);
      }
      const predicateIri = quad.predicate.value;
      const objects = (_a = predicates[predicateIri]) !== null && _a !== void 0 ? _a : {};
      return freeze(Object.assign(Object.assign({}, predicates), { [predicateIri]: addRdfJsQuadToObjects(objects, quad, quadParseOptions) }));
  }
  function addRdfJsQuadToObjects(objects, quad, quadParseOptions) {
      var _a, _b, _c, _d, _e, _f, _g, _h;
      if (quad.object.termType === "NamedNode") {
          const namedNodes = freeze([
              ...((_a = objects.namedNodes) !== null && _a !== void 0 ? _a : []),
              quad.object.value,
          ]);
          return freeze(Object.assign(Object.assign({}, objects), { namedNodes: namedNodes }));
      }
      if (quad.object.termType === "Literal") {
          if (quad.object.datatype.value === xmlSchemaTypes.langString) {
              const locale = quad.object.language.toLowerCase();
              const thisLocaleStrings = freeze([
                  ...((_c = (_b = objects.langStrings) === null || _b === void 0 ? void 0 : _b[locale]) !== null && _c !== void 0 ? _c : []),
                  quad.object.value,
              ]);
              const langStrings = freeze(Object.assign(Object.assign({}, ((_d = objects.langStrings) !== null && _d !== void 0 ? _d : {})), { [locale]: thisLocaleStrings }));
              return freeze(Object.assign(Object.assign({}, objects), { langStrings: langStrings }));
          }
          // If the Object is a non-langString Literal
          const thisTypeValues = freeze([
              ...((_f = (_e = objects.literals) === null || _e === void 0 ? void 0 : _e[quad.object.datatype.value]) !== null && _f !== void 0 ? _f : []),
              quad.object.value,
          ]);
          const literals = freeze(Object.assign(Object.assign({}, ((_g = objects.literals) !== null && _g !== void 0 ? _g : {})), { [quad.object.datatype.value]: thisTypeValues }));
          return freeze(Object.assign(Object.assign({}, objects), { literals: literals }));
      }
      if (quad.object.termType === "BlankNode") {
          const blankNodePredicates = getPredicatesForBlankNode(quad.object, quadParseOptions);
          const blankNodes = freeze([
              ...((_h = objects.blankNodes) !== null && _h !== void 0 ? _h : []),
              blankNodePredicates,
          ]);
          return freeze(Object.assign(Object.assign({}, objects), { blankNodes: blankNodes }));
      }
      throw new Error(`Objects of type [${quad.object.termType}] are not supported.`);
  }
  function getPredicatesForBlankNode(node, quadParseOptions) {
      var _a, _b;
      const chainBlankNodes = (_a = quadParseOptions.chainBlankNodes) !== null && _a !== void 0 ? _a : [];
      if (chainBlankNodes.find((chainBlankNode) => chainBlankNode.equals(node)) ===
          undefined) {
          // If this Blank Node is not used to provide nested values for another Subject,
          // just return its identifier.
          // That identifier will also be listed among the Subjects in the Graph.
          return getBlankNodeId(node);
      }
      /* istanbul ignore next: If there are chain nodes, there will always be other Quads, so the `?? []` can't be reached: */
      const quads = (_b = quadParseOptions.otherQuads) !== null && _b !== void 0 ? _b : [];
      const quadsWithNodeAsSubject = quads.filter((quad) => quad.subject.equals(node));
      // First add the Quads with regular Objects
      const predicates = quadsWithNodeAsSubject
          .filter((quad) => !isBlankNode$1(quad.object))
          .reduce((predicatesAcc, quad) => {
          var _a;
          const supportedPredicateTypes = [
              "NamedNode",
          ];
          if (!supportedPredicateTypes.includes(quad.predicate.termType)) {
              throw new Error(`Cannot parse Quads with nodes of type [${quad.predicate.termType}] as their Predicate node.`);
          }
          const objects = (_a = predicatesAcc[quad.predicate.value]) !== null && _a !== void 0 ? _a : {};
          return freeze(Object.assign(Object.assign({}, predicatesAcc), { [quad.predicate.value]: addRdfJsQuadToObjects(objects, quad, quadParseOptions) }));
      }, {});
      // And then also add the Quads that have another Blank Node as the Object
      // in addition to the Blank Node `node` as the Subject:
      const blankNodeObjectQuads = quadsWithNodeAsSubject.filter((quad) => isBlankNode$1(quad.object));
      return blankNodeObjectQuads.reduce((predicatesAcc, quad) => {
          var _a, _b;
          const supportedPredicateTypes = [
              "NamedNode",
          ];
          if (!supportedPredicateTypes.includes(quad.predicate.termType)) {
              throw new Error(`Cannot parse Quads with nodes of type [${quad.predicate.termType}] as their Predicate node.`);
          }
          /* istanbul ignore next: The `?? {}` doesn't get hit; presumably it's initialised above. */
          const objects = (_a = predicatesAcc[quad.predicate.value]) !== null && _a !== void 0 ? _a : {};
          /* istanbul ignore next: The `?? []` doesn't get hit; presumably it's initialised above. */
          const blankNodes = (_b = objects.blankNodes) !== null && _b !== void 0 ? _b : [];
          return freeze(Object.assign(Object.assign({}, predicatesAcc), { 
              // The BlankNode assertions are valid because we filtered on BlankNodes above:
              [quad.predicate.value]: Object.assign(Object.assign({}, objects), { blankNodes: [
                      ...blankNodes,
                      getPredicatesForBlankNode(quad.object, quadParseOptions),
                  ] }) }));
      }, predicates);
  }
  /**
   * Given an array of Quads, returns all Blank Nodes that are used in a single chain of Nodes.
   *
   * This allows you to obtain which Blank Nodes are involved in e.g. RDF lists.
   * This is useful because those can be represented as nested data that will have
   * a deterministic structure, whereas a representation of Blank Nodes that
   * create a cycle or are re-used will need ad-hoc, non-deterministic identifiers
   * to allow for representation without inifinite nesting.
   */
  function getChainBlankNodes(quads) {
      // All Blank Nodes that occur in Subject position:
      const blankNodeSubjects = quads
          .map((quad) => quad.subject)
          .filter(isBlankNode$1);
      // All Blank Nodes that occur in Object position:
      const blankNodeObjects = quads.map((quad) => quad.object).filter(isBlankNode$1);
      // Makes sure that all given Nodes are the same,
      // which will be used to verify that a set of Quads all have the same Subject:
      function everyNodeTheSame(nodes) {
          // This could potentially be made more performant by mapping every term
          // to their value and using native JS comparisons, assuming every node is
          // either a Blank or a Named Node.
          return nodes.every((otherNode) => nodes.every((anotherNode) => otherNode.equals(anotherNode)));
      }
      // Get all Blank Nodes that are part of a cycle in the graph:
      const cycleBlankNodes = [];
      blankNodeObjects.forEach((blankNodeObject) => {
          cycleBlankNodes.push(...getCycleBlankNodes(blankNodeObject, quads));
      });
      // Get Blank Nodes that are used to provide nested values for a single Subject,
      // which we'll represent as nested values as well
      // (this allows us to avoid generating a non-deterministic, ad-hoc identifier
      // for those Blank Nodes).
      // We'll do this by taking all Blank Nodes in the given Quads...
      const chainBlankNodes = blankNodeSubjects
          .concat(blankNodeObjects)
          .filter((blankNode) => {
          // ....removing those Blank Nodes that are part of a cycle...
          if (cycleBlankNodes.some((cycleBlankNode) => cycleBlankNode.equals(blankNode))) {
              return false;
          }
          // ...and then returning only those Blank Nodes that only occur in the
          // Object position for a single Subject, i.e. that are part of a single
          // chain:
          const subjectsWithThisNodeAsObject = quads
              .filter((quad) => quad.object.equals(blankNode))
              .map((quad) => quad.subject);
          return (subjectsWithThisNodeAsObject.length > 0 &&
              everyNodeTheSame(subjectsWithThisNodeAsObject));
      });
      return chainBlankNodes;
  }
  function toRdfJsQuads(dataset, options = {}) {
      var _a;
      const quads = [];
      const dataFactory = (_a = options.dataFactory) !== null && _a !== void 0 ? _a : dataModel;
      Object.keys(dataset.graphs).forEach((graphIri) => {
          const graph = dataset.graphs[graphIri];
          const graphNode = graphIri === "default"
              ? dataFactory.defaultGraph()
              : dataFactory.namedNode(graphIri);
          Object.keys(graph).forEach((subjectIri) => {
              const predicates = graph[subjectIri].predicates;
              const subjectNode = isBlankNodeId(subjectIri)
                  ? dataFactory.blankNode(getBlankNodeValue(subjectIri))
                  : dataFactory.namedNode(subjectIri);
              quads.push(...subjectToRdfJsQuads(predicates, subjectNode, graphNode, options));
          });
      });
      return quads;
  }
  function subjectToRdfJsQuads(predicates, subjectNode, graphNode, options = {}) {
      var _a;
      const quads = [];
      const dataFactory = (_a = options.dataFactory) !== null && _a !== void 0 ? _a : dataModel;
      Object.keys(predicates).forEach((predicateIri) => {
          var _a, _b, _c, _d;
          const predicateNode = dataFactory.namedNode(predicateIri);
          const langStrings = (_a = predicates[predicateIri].langStrings) !== null && _a !== void 0 ? _a : {};
          const namedNodes = (_b = predicates[predicateIri].namedNodes) !== null && _b !== void 0 ? _b : [];
          const literals = (_c = predicates[predicateIri].literals) !== null && _c !== void 0 ? _c : {};
          const blankNodes = (_d = predicates[predicateIri].blankNodes) !== null && _d !== void 0 ? _d : [];
          const literalTypes = Object.keys(literals);
          literalTypes.forEach((typeIri) => {
              const typeNode = dataFactory.namedNode(typeIri);
              const literalValues = literals[typeIri];
              literalValues.forEach((value) => {
                  const literalNode = dataFactory.literal(value, typeNode);
                  quads.push(dataFactory.quad(subjectNode, predicateNode, literalNode, graphNode));
              });
          });
          const locales = Object.keys(langStrings);
          locales.forEach((locale) => {
              const localeValues = langStrings[locale];
              localeValues.forEach((value) => {
                  const langStringNode = dataFactory.literal(value, locale);
                  quads.push(dataFactory.quad(subjectNode, predicateNode, langStringNode, graphNode));
              });
          });
          namedNodes.forEach((namedNodeIri) => {
              const node = dataFactory.namedNode(namedNodeIri);
              quads.push(dataFactory.quad(subjectNode, predicateNode, node, graphNode));
          });
          blankNodes.forEach((blankNodeIdOrPredicates) => {
              if (isBlankNodeId(blankNodeIdOrPredicates)) {
                  const blankNode = dataFactory.blankNode(getBlankNodeValue(blankNodeIdOrPredicates));
                  quads.push(dataFactory.quad(subjectNode, predicateNode, blankNode, graphNode));
              }
              else {
                  const node = dataFactory.blankNode();
                  const blankNodeObjectQuad = dataFactory.quad(subjectNode, predicateNode, node, graphNode);
                  const blankNodeSubjectQuads = subjectToRdfJsQuads(blankNodeIdOrPredicates, node, graphNode);
                  quads.push(blankNodeObjectQuad);
                  quads.push(...blankNodeSubjectQuads);
              }
          });
      });
      return quads;
  }
  /**
   * A recursive function that finds all Blank Nodes in an array of Quads that create a cycle in the graph.
   *
   * This function will traverse the graph starting from `currentNode`, keeping
   * track of all the Blank Nodes it encounters twice while doing so, and
   * returning those.
   */
  function getCycleBlankNodes(currentNode, quads, traversedBlankNodes = []) {
      // If we've encountered `currentNode` before, all the Blank Nodes we've
      // encountered so far are part of a cycle. Return those.
      if (traversedBlankNodes.find((traversedBlankNode) => traversedBlankNode.equals(currentNode)) !== undefined) {
          return traversedBlankNodes;
      }
      // Find all Blank Nodes that are connected to `currentNode`:
      const blankNodeObjects = quads
          .filter((quad) => quad.subject.equals(currentNode) && isBlankNode$1(quad.object))
          .map((quad) => quad.object);
      // If no Blank Nodes are connected to `currentNode`, and `currentNode` is not
      // part of a cycle, we're done; the currently traversed Nodes do not form a
      // cycle:
      if (blankNodeObjects.length === 0) {
          return [];
      }
      // Store that we've traversed `currentNode`, then move on to all the Blank
      // Nodes connected to it, which will then take up the role of `currentNode`:
      const nextTraversedNodes = [...traversedBlankNodes, currentNode];
      const cycleBlankNodeArrays = blankNodeObjects.map((nextNode) => getCycleBlankNodes(nextNode, quads, nextTraversedNodes));
      // Collect all the cycle Blank Nodes found in those traverals,
      // then return them:
      const allCycleBlankNodes = [];
      for (const cycleBlankNodes of cycleBlankNodeArrays) {
          allCycleBlankNodes.push(...cycleBlankNodes);
      }
      return allCycleBlankNodes;
  }
  function isBlankNode$1(term) {
      return term.termType === "BlankNode";
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const getTurtleParser = () => {
      const onQuadCallbacks = [];
      const onCompleteCallbacks = [];
      const onErrorCallbacks = [];
      return {
          onQuad: (callback) => {
              onQuadCallbacks.push(callback);
          },
          onError: (callback) => {
              onErrorCallbacks.push(callback);
          },
          onComplete: (callback) => {
              onCompleteCallbacks.push(callback);
          },
          parse: async (source, resourceInfo) => {
              const parser = await getParser(getSourceUrl(resourceInfo));
              parser.parse(source, (error, quad, _prefixes) => {
                  if (error) {
                      onErrorCallbacks.forEach((callback) => callback(error));
                  }
                  else if (quad) {
                      onQuadCallbacks.every((callback) => callback(quad));
                  }
                  else {
                      onCompleteCallbacks.every((callback) => callback());
                  }
              });
          },
      };
  };
  async function getParser(baseIri) {
      const n3 = await loadN3();
      return new n3.Parser({ format: "text/turtle", baseIRI: baseIri });
  }
  /**
   * @param quads Triples that should be serialised to Turtle
   * @internal Utility method for internal use; not part of the public API.
   */
  async function triplesToTurtle(quads) {
      const n3 = await loadN3();
      const format = "text/turtle";
      const writer = new n3.Writer({ format: format });
      // Remove any potentially lingering references to Named Graphs in Quads;
      // they'll be determined by the URL the Turtle will be sent to:
      const triples = quads.map((quad) => DataFactory$1.quad(quad.subject, quad.predicate, quad.object, undefined));
      writer.addQuads(triples);
      const writePromise = new Promise((resolve, reject) => {
          writer.end((error, result) => {
              /* istanbul ignore if [n3.js doesn't actually pass an error nor a result, apparently: https://github.com/rdfjs/N3.js/blob/62682e48c02d8965b4d728cb5f2cbec6b5d1b1b8/src/N3Writer.js#L290] */
              if (error) {
                  return reject(error);
              }
              resolve(result);
          });
      });
      const rawTurtle = await writePromise;
      return rawTurtle;
  }
  async function loadN3() {
      // When loaded via Webpack or another bundler that looks at the `modules` field in package.json,
      // N3 serves up ES modules with named exports.
      // However, when it is loaded in Node, it serves up a CommonJS module, which, when imported from
      // a Node ES module, is in the shape of a default export that is an object with all the named
      // exports as its properties.
      // This means that if we were to import the default module, our code would fail in Webpack,
      // whereas if we imported the named exports, our code would fail in Node.
      // As a workaround, we use a dynamic import. This way, we can use the same syntax in every
      // environment, where the differences between the environments are in whether the returned object
      // includes a `default` property that contains all exported functions, or whether those functions
      // are available on the returned object directly. We can then respond to those different
      // situations at runtime.
      // Unfortunately, that does mean that tree shaking will not work until N3 also provides ES modules
      // for Node, or adds a default export for Webpack. See
      // https://github.com/rdfjs/N3.js/issues/196
      const n3Module = await Promise.resolve().then(function () { return index; });
      /* istanbul ignore if: the package provides named exports in the unit test environment */
      if (typeof n3Module.default !== "undefined") {
          return n3Module.default;
      }
      return n3Module;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /** @hidden For internal use only. */
  function internal_getReadableValue(value) {
      var _a, _b, _c, _d, _e, _f, _g, _h;
      if (isNamedNode$1(value)) {
          return `<${value.value}> (URL)`;
      }
      if (isLiteral$1(value)) {
          /* istanbul ignore if: thingAsMarkdown always instantiates a NamedNode, so we can't hit this code path in tests. */
          if (!isNamedNode$1(value.datatype)) {
              return `[${value.value}] (RDF/JS Literal of unknown type)`;
          }
          let val;
          switch (value.datatype.value) {
              case xmlSchemaTypes.boolean:
                  val =
                      (_b = (_a = deserializeBoolean(value.value)) === null || _a === void 0 ? void 0 : _a.valueOf()) !== null && _b !== void 0 ? _b : `Invalid data: \`${value.value}\``;
                  return val + " (boolean)";
              case xmlSchemaTypes.dateTime:
                  val =
                      (_d = (_c = deserializeDatetime(value.value)) === null || _c === void 0 ? void 0 : _c.toUTCString()) !== null && _d !== void 0 ? _d : `Invalid data: \`${value.value}\``;
                  return val + " (datetime)";
              case xmlSchemaTypes.decimal:
                  val =
                      (_f = (_e = deserializeDecimal(value.value)) === null || _e === void 0 ? void 0 : _e.toString()) !== null && _f !== void 0 ? _f : `Invalid data: \`${value.value}\``;
                  return val + " (decimal)";
              case xmlSchemaTypes.integer:
                  val =
                      (_h = (_g = deserializeInteger(value.value)) === null || _g === void 0 ? void 0 : _g.toString()) !== null && _h !== void 0 ? _h : `Invalid data: \`${value.value}\``;
                  return val + " (integer)";
              case xmlSchemaTypes.langString:
                  return `"${value.value}" (${value.language} string)`;
              case xmlSchemaTypes.string:
                  return `"${value.value}" (string)`;
              default:
                  return `[${value.value}] (RDF/JS Literal of type: \`${value.datatype.value}\`)`;
          }
      }
      /* istanbul ignore else: thingAsMarkdown doesn't generate other Nodes, so we can't hit this path in tests. */
      if (value.termType === "BlankNode") {
          return `[${value.value}] (RDF/JS BlankNode)`;
      }
      /* istanbul ignore next: thingAsMarkdown doesn't generate Quad Nodes, so we can't hit this path in tests. */
      if (value.termType === "Quad") {
          return `??? (nested RDF* Quad)`;
      }
      /* istanbul ignore else: The if statements are exhaustive; if not, TypeScript will complain. */
      /* istanbul ignore next: thingAsMarkdown doesn't generate Variable Nodes, so we can't hit this path in tests. */
      if (value.termType === "Variable") {
          return `?${value.value} (RDF/JS Variable)`;
      }
      /* istanbul ignore next: The if statements are exhaustive; if not, TypeScript will complain. */
      return value;
  }
  /**
   * @hidden
   */
  function internal_throwIfNotThing(thing) {
      if (!isThing(thing)) {
          throw new ThingExpectedError(thing);
      }
  }
  /**
   * @hidden
   * @param solidDataset
   */
  function internal_addAdditionsToChangeLog(solidDataset, additions) {
      const changeLog = hasChangelog(solidDataset)
          ? solidDataset.internal_changeLog
          : /* istanbul ignore next: This function always gets called after addDeletionsToChangeLog, so the ChangeLog always already exists in tests: */
              { additions: [], deletions: [] };
      const [newAdditions, newDeletions] = additions
          .filter((addition) => !containsBlankNode(addition))
          .reduce(([additionsAcc, deletionsAcc], addition) => {
          const existingDeletion = deletionsAcc.find((deletion) => deletion.equals(addition));
          if (typeof existingDeletion !== "undefined") {
              return [
                  additionsAcc,
                  deletionsAcc.filter((deletion) => !deletion.equals(addition)),
              ];
          }
          return [additionsAcc.concat(addition), deletionsAcc];
      }, [changeLog.additions, changeLog.deletions]);
      return freeze(Object.assign(Object.assign({}, solidDataset), { internal_changeLog: {
              additions: newAdditions,
              deletions: newDeletions,
          } }));
  }
  /**
   * @hidden
   * @param solidDataset
   */
  function internal_addDeletionsToChangeLog(solidDataset, deletions) {
      const changeLog = hasChangelog(solidDataset)
          ? solidDataset.internal_changeLog
          : { additions: [], deletions: [] };
      const [newAdditions, newDeletions] = deletions
          .filter((deletion) => !containsBlankNode(deletion))
          .reduce(([additionsAcc, deletionsAcc], deletion) => {
          const existingAddition = additionsAcc.find((addition) => addition.equals(deletion));
          if (typeof existingAddition !== "undefined") {
              return [
                  additionsAcc.filter((addition) => !addition.equals(deletion)),
                  deletionsAcc,
              ];
          }
          return [additionsAcc, deletionsAcc.concat(deletion)];
      }, [changeLog.additions, changeLog.deletions]);
      return freeze(Object.assign(Object.assign({}, solidDataset), { internal_changeLog: {
              additions: newAdditions,
              deletions: newDeletions,
          } }));
  }
  /**
   * Enforces the presence of a Changelog for a given dataset. If a changelog is
   * already present, it is unchanged. Otherwise, an empty changelog is created.
   * @hidden
   * @param solidDataset
   */
  function internal_withChangeLog(solidDataset) {
      const newSolidDataset = hasChangelog(solidDataset)
          ? solidDataset
          : freeze(Object.assign(Object.assign({}, solidDataset), { internal_changeLog: { additions: [], deletions: [] } }));
      return newSolidDataset;
  }
  /**
   * We don't currently support reading and writing Blank Nodes, so this function can be used to skip those Quads.
   *
   * This is needed because we cannot reconcile Blank Nodes in additions and
   * deletions. Down the road, we should do a diff before saving a SolidDataset
   * against a saved copy of the originally-fetched one, based on our own data
   * structures, which should make it easier to reconcile.
   */
  function containsBlankNode(quad) {
      return (quad.subject.termType === "BlankNode" ||
          quad.object.termType === "BlankNode");
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Returns the URLs of all Properties that the given [[Thing ]]has values for.b
   *
   * @param thing The [[Thing]] of which to get that Property URLs that have a value.
   * @returns The URLs of the Properties for which values are defined for the given Thing.
   * @hidden This is an advanced API that should not be needed for most Solid use cases. If you do find yourself needing this, please file a feature request sharing your use case.
   */
  function getPropertyAll(thing) {
      return Object.keys(thing.predicates).filter((predicate) => getTerm(thing, predicate) !== null);
  }
  /**
   * Returns the URL value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type URL, returns null.
   * If the Property has multiple URL values, returns one of its URL values.
   *
   * @param thing The [[Thing]] to read a URL value from.
   * @param property The Property whose URL value to return.
   * @returns A URL value for the given Property if present, or null if the Property is not present or the value is not of type URL.
   */
  function getUrl(thing, property) {
      var _a, _b, _c;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateUrl = internal_toIriString(property);
      const firstUrl = (_c = (_b = (_a = thing.predicates[predicateUrl]) === null || _a === void 0 ? void 0 : _a.namedNodes) === null || _b === void 0 ? void 0 : _b[0]) !== null && _c !== void 0 ? _c : null;
      if (firstUrl === null) {
          return null;
      }
      return isLocalNodeIri(firstUrl) ? `#${getLocalNodeName(firstUrl)}` : firstUrl;
  }
  /** @hidden Alias of [[getUrl]] for those who prefer IRI terminology. */
  const getIri = getUrl;
  /**
   * Returns the URL values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type URL, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the URL values from.
   * @param property The Property whose URL values to return.
   * @returns An array of URL values for the given Property.
   */
  function getUrlAll(thing, property) {
      var _a, _b, _c;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateUrl = internal_toIriString(property);
      return ((_c = (_b = (_a = thing.predicates[predicateUrl]) === null || _a === void 0 ? void 0 : _a.namedNodes) === null || _b === void 0 ? void 0 : _b.map((iri) => isLocalNodeIri(iri) ? `#${getLocalNodeName(iri)}` : iri)) !== null && _c !== void 0 ? _c : []);
  }
  /** @hidden Alias of [[getUrlAll]] for those who prefer IRI terminology. */
  const getIriAll = getUrlAll;
  /**
   * Returns the boolean value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type boolean, returns null.
   * If the Property has multiple boolean values, returns one of its values.
   *
   * @param thing The [[Thing]] to read a boolean value from.
   * @param property The Property whose boolean value to return.
   * @returns A boolean value for the given Property if present, or null if the Property is not present or the value is not of type boolean.
   */
  function getBoolean(thing, property) {
      internal_throwIfNotThing(thing);
      const literalString = getLiteralOfType(thing, property, xmlSchemaTypes.boolean);
      if (literalString === null) {
          return null;
      }
      return deserializeBoolean(literalString);
  }
  /**
   * Returns the boolean values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type boolean, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the boolean values from.
   * @param property The Property whose boolean values to return.
   * @returns An array of boolean values for the given Property.
   */
  function getBooleanAll(thing, property) {
      internal_throwIfNotThing(thing);
      const literalStrings = getLiteralAllOfType(thing, property, xmlSchemaTypes.boolean);
      return literalStrings
          .map(deserializeBoolean)
          .filter((possibleBoolean) => possibleBoolean !== null);
  }
  /**
   * Returns the datetime value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type datetime, returns null.
   * If the Property has multiple datetime values, returns one of its values.
   *
   * @param thing The [[Thing]] to read a datetime value from.
   * @param property The Property whose datetime value to return.
   * @returns A datetime value for the given Property if present, or null if the Property is not present or the value is not of type datetime.
   */
  function getDatetime(thing, property) {
      internal_throwIfNotThing(thing);
      const literalString = getLiteralOfType(thing, property, xmlSchemaTypes.dateTime);
      if (literalString === null) {
          return null;
      }
      return deserializeDatetime(literalString);
  }
  /**
   * Returns the datetime values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type datetime, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the datetime values from.
   * @param property The Property whose datetime values to return.
   * @returns An array of datetime values for the given Property.
   */
  function getDatetimeAll(thing, property) {
      internal_throwIfNotThing(thing);
      const literalStrings = getLiteralAllOfType(thing, property, xmlSchemaTypes.dateTime);
      return literalStrings
          .map(deserializeDatetime)
          .filter((potentialDatetime) => potentialDatetime !== null);
  }
  /**
   * Returns the date value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type date, returns null.
   * If the Property has multiple date values, returns one of its values.
   *
   * @param thing The [[Thing]] to read a date value from.
   * @param property The Property whose date value to return.
   * @returns A date value for the given Property if present, or null if the Property is not present or the value is not of type date.
   * @since 1.10.0
   */
  function getDate(thing, property) {
      internal_throwIfNotThing(thing);
      const literalString = getLiteralOfType(thing, property, xmlSchemaTypes.date);
      if (literalString === null) {
          return null;
      }
      return deserializeDate(literalString);
  }
  /**
   * Returns the date values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type date, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the date values from.
   * @param property The Property whose date values to return.
   * @returns An array of date values for the given Property.
   * @since 1.10.0
   */
  function getDateAll(thing, property) {
      internal_throwIfNotThing(thing);
      const literalStrings = getLiteralAllOfType(thing, property, xmlSchemaTypes.date);
      return literalStrings
          .map(deserializeDate)
          .filter((potentialDate) => potentialDate !== null);
  }
  /**
   * Returns the time value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type time, returns null.
   * If the Property has multiple time values, returns one of its values.
   *
   * @param thing The [[Thing]] to read a time value from.
   * @param property The Property whose time value to return.
   * @returns A time value for the given Property if present, or null if the Property is not present or the value is not of type time.
   * @since 1.10.0
   */
  function getTime(thing, property) {
      internal_throwIfNotThing(thing);
      const literalString = getLiteralOfType(thing, property, xmlSchemaTypes.time);
      if (literalString === null) {
          return null;
      }
      return deserializeTime(literalString);
  }
  /**
   * Returns the time values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type time, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the time values from.
   * @param property The Property whose time values to return.
   * @returns An array of time values for the given Property.
   * @since 1.10.0
   */
  function getTimeAll(thing, property) {
      internal_throwIfNotThing(thing);
      const literalStrings = getLiteralAllOfType(thing, property, xmlSchemaTypes.time);
      return literalStrings
          .map(deserializeTime)
          .filter((potentialTime) => potentialTime !== null);
  }
  /**
   * Returns the decimal value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type decimal, returns null.
   * If the Property has multiple decimal values, returns one of its values.
   *
   * @param thing The [[Thing]] to read a decimal value from.
   * @param property The Property whose decimal value to return.
   * @returns A decimal value for the given Property if present, or null if the Property is not present or the value is not of type decimal.
   */
  function getDecimal(thing, property) {
      internal_throwIfNotThing(thing);
      const literalString = getLiteralOfType(thing, property, xmlSchemaTypes.decimal);
      if (literalString === null) {
          return null;
      }
      return deserializeDecimal(literalString);
  }
  /**
   * Returns the decimal values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type decimal, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the decimal values from.
   * @param property The Property whose decimal values to return.
   * @returns An array of decimal values for the given Property.
   */
  function getDecimalAll(thing, property) {
      internal_throwIfNotThing(thing);
      const literalStrings = getLiteralAllOfType(thing, property, xmlSchemaTypes.decimal);
      return literalStrings
          .map((literalString) => deserializeDecimal(literalString))
          .filter((potentialDecimal) => potentialDecimal !== null);
  }
  /**
   * Returns the integer value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type integer, returns null.
   * If the Property has multiple integer values, returns one of its values.
   *
   * @param thing The [[Thing]] to read an integer value from.
   * @param property The Property whose integer value to return.
   * @returns A integer value for the given Property if present, or null if the Property is not present or the value is not of type datetime.
   */
  function getInteger(thing, property) {
      internal_throwIfNotThing(thing);
      const literalString = getLiteralOfType(thing, property, xmlSchemaTypes.integer);
      if (literalString === null) {
          return null;
      }
      return deserializeInteger(literalString);
  }
  /**
   * Returns the integer values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type integer, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the integer values from.
   * @param property The Property whose integer values to return.
   * @returns An array of integer values for the given Property.
   */
  function getIntegerAll(thing, property) {
      internal_throwIfNotThing(thing);
      const literalStrings = getLiteralAllOfType(thing, property, xmlSchemaTypes.integer);
      return literalStrings
          .map((literalString) => deserializeInteger(literalString))
          .filter((potentialInteger) => potentialInteger !== null);
  }
  /**
   * Returns the localized string value of the specified Property from a [[Thing]].
   * If the Property is not present as a string in the specified locale, returns null.
   * If the Property has multiple string values for the specified locale, returns one of its values.
   *
   * @param thing The [[Thing]] to read a localised string value from.
   * @param property The Property whose localised string value to return.
   * @param locale The desired locale for the string value.
   * @returns A localised string value for the given Property if present in the specified `locale`, or null otherwise.
   */
  function getStringWithLocale(thing, property, locale) {
      var _a, _b;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const langStrings = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.langStrings) !== null && _b !== void 0 ? _b : {};
      const existingLocales = Object.keys(langStrings);
      const matchingLocale = existingLocales.find((existingLocale) => existingLocale.toLowerCase() === locale.toLowerCase() &&
          Array.isArray(langStrings[existingLocale]) &&
          langStrings[existingLocale].length > 0);
      return typeof matchingLocale === "string"
          ? langStrings[matchingLocale][0]
          : null;
  }
  /**
   * Returns the localized string values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not a string of the specified locale, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the localised string values from.
   * @param property The Property whose localised string values to return.
   * @param locale The desired locale for the string values.
   * @returns An array of localised string values for the given Property.
   */
  function getStringWithLocaleAll(thing, property, locale) {
      var _a, _b;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const langStrings = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.langStrings) !== null && _b !== void 0 ? _b : {};
      const existingLocales = Object.keys(langStrings);
      const matchingLocale = existingLocales.find((existingLocale) => existingLocale.toLowerCase() === locale.toLowerCase() &&
          Array.isArray(langStrings[existingLocale]) &&
          langStrings[existingLocale].length > 0);
      return typeof matchingLocale === "string"
          ? [...langStrings[matchingLocale]]
          : [];
  }
  /**
   * Returns all localized string values mapped by the locales for the specified property from the
   * specified [[Thing]] (explicitly filters out non-language string literals).
   *
   * @param thing The [[Thing]] to read the localised string values from.
   * @param property The Property whose localised string values to return.
   * @returns A Map of objects, keyed on locale with the value an array of string values (for that locale).
   */
  function getStringByLocaleAll(thing, property) {
      var _a, _b;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const stringsByLocale = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.langStrings) !== null && _b !== void 0 ? _b : {};
      return new Map(Object.entries(stringsByLocale).map(([locale, values]) => [
          locale,
          [...values],
      ]));
  }
  /**
   * Returns the string value of the specified Property from a [[Thing]].
   * If the Property is not present or its value is not of type string, returns null.
   * If the Property has multiple string values, returns one of its values.
   *
   * @param thing The [[Thing]] to read a string value from.
   * @param property The Property whose string value to return.
   * @returns A string value for the given Property if present, or null if the Property is not present or the value is not of type string.
   */
  function getStringNoLocale(thing, property) {
      internal_throwIfNotThing(thing);
      const literalString = getLiteralOfType(thing, property, xmlSchemaTypes.string);
      return literalString;
  }
  /**
   * Returns the string values of the specified Property from a [[Thing]].
   * If the Property is not present, returns an empty array.
   * If the Property's value is not of type string, omits that value in the array.
   *
   * @param thing The [[Thing]] to read the string values from.
   * @param property The Property whose string values to return.
   * @returns An array of string values for the given Property.
   */
  function getStringNoLocaleAll(thing, property) {
      internal_throwIfNotThing(thing);
      const literalStrings = getLiteralAllOfType(thing, property, xmlSchemaTypes.string);
      return literalStrings;
  }
  /**
   * @param thing The [[Thing]] to read a NamedNode value from.
   * @param property The given Property for which you want the NamedNode value.
   * @returns A NamedNode value for the given Property, if present, or null otherwise.
   * @ignore This should not be needed due to the other get*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @see https://rdf.js.org/data-model-spec/#namednode-interface
   */
  function getNamedNode(thing, property) {
      const iriString = getIri(thing, property);
      if (iriString === null) {
          return null;
      }
      return DataFactory$1.namedNode(iriString);
  }
  /**
   * @param thing The [[Thing]] to read the NamedNode values from.
   * @param property The given Property for which you want the NamedNode values.
   * @returns The NamedNode values for the given Property.
   * @ignore This should not be needed due to the other get*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @see https://rdf.js.org/data-model-spec/#namednode-interface
   */
  function getNamedNodeAll(thing, property) {
      const iriStrings = getIriAll(thing, property);
      return iriStrings.map((iriString) => DataFactory$1.namedNode(iriString));
  }
  /**
   * @param thing The [[Thing]] to read a Literal value from.
   * @param property The given Property for which you want the Literal value.
   * @returns A Literal value for the given Property, if present, or null otherwise.
   * @ignore This should not be needed due to the other get*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @see https://rdf.js.org/data-model-spec/#literal-interface
   */
  function getLiteral(thing, property) {
      var _a, _b, _c, _d;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const langStrings = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.langStrings) !== null && _b !== void 0 ? _b : {};
      const locales = Object.keys(langStrings);
      if (locales.length > 0) {
          const nonEmptyLocale = locales.find((locale) => Array.isArray(langStrings[locale]) && langStrings[locale].length > 0);
          if (typeof nonEmptyLocale === "string") {
              return DataFactory$1.literal(langStrings[nonEmptyLocale][0], nonEmptyLocale);
          }
      }
      const otherLiterals = (_d = (_c = thing.predicates[predicateIri]) === null || _c === void 0 ? void 0 : _c.literals) !== null && _d !== void 0 ? _d : {};
      const dataTypes = Object.keys(otherLiterals);
      if (dataTypes.length > 0) {
          const nonEmptyDataType = dataTypes.find((dataType) => Array.isArray(otherLiterals[dataType]) &&
              otherLiterals[dataType].length > 0);
          if (typeof nonEmptyDataType === "string") {
              return DataFactory$1.literal(otherLiterals[nonEmptyDataType][0], DataFactory$1.namedNode(nonEmptyDataType));
          }
      }
      return null;
  }
  /**
   * @param thing The [[Thing]] to read the Literal values from.
   * @param property The given Property for which you want the Literal values.
   * @returns The Literal values for the given Property.
   * @ignore This should not be needed due to the other get*All() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @see https://rdf.js.org/data-model-spec/#literal-interface
   */
  function getLiteralAll(thing, property) {
      var _a, _b, _c, _d;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      let literals = [];
      const langStrings = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.langStrings) !== null && _b !== void 0 ? _b : {};
      const locales = Object.keys(langStrings);
      for (const locale of locales) {
          const stringsInLocale = langStrings[locale];
          const localeLiterals = stringsInLocale.map((langString) => DataFactory$1.literal(langString, locale));
          literals = literals.concat(localeLiterals);
      }
      const otherLiterals = (_d = (_c = thing.predicates[predicateIri]) === null || _c === void 0 ? void 0 : _c.literals) !== null && _d !== void 0 ? _d : {};
      const dataTypes = Object.keys(otherLiterals);
      for (const dataType of dataTypes) {
          const values = otherLiterals[dataType];
          const typeNode = DataFactory$1.namedNode(dataType);
          const dataTypeLiterals = values.map((value) => DataFactory$1.literal(value, typeNode));
          literals = literals.concat(dataTypeLiterals);
      }
      return literals;
  }
  /**
   * @param thing The [[Thing]] to read a raw RDF/JS value from.
   * @param property The given Property for which you want the raw value.
   * @returns A Term for the given Property, if present, or null otherwise.
   * @ignore This should not be needed due to the other get*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @see https://rdf.js.org/data-model-spec/
   * @since 0.3.0
   */
  function getTerm(thing, property) {
      var _a, _b;
      internal_throwIfNotThing(thing);
      const namedNode = getNamedNode(thing, property);
      if (namedNode !== null) {
          return namedNode;
      }
      const literal = getLiteral(thing, property);
      if (literal !== null) {
          return literal;
      }
      const predicateIri = internal_toIriString(property);
      const blankNodes = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.blankNodes) !== null && _b !== void 0 ? _b : [];
      if (blankNodes.length > 0) {
          const blankNodeValue = isBlankNodeId(blankNodes[0])
              ? getBlankNodeValue(blankNodes[0])
              : undefined;
          return DataFactory$1.blankNode(blankNodeValue);
      }
      return null;
  }
  /**
   * @param thing The [[Thing]] to read the raw RDF/JS values from.
   * @param property The given Property for which you want the raw values.
   * @returns The Terms for the given Property.
   * @ignore This should not be needed due to the other get*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @see https://rdf.js.org/data-model-spec/
   * @since 0.3.0
   */
  function getTermAll(thing, property) {
      var _a, _b;
      internal_throwIfNotThing(thing);
      const namedNodes = getNamedNodeAll(thing, property);
      const literals = getLiteralAll(thing, property);
      const predicateIri = internal_toIriString(property);
      const blankNodeValues = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.blankNodes) !== null && _b !== void 0 ? _b : [];
      const blankNodes = blankNodeValues.map((rawBlankNode) => {
          const blankNodeName = isBlankNodeId(rawBlankNode)
              ? getBlankNodeValue(rawBlankNode)
              : undefined;
          return DataFactory$1.blankNode(blankNodeName);
      });
      const terms = namedNodes
          .concat(literals)
          .concat(blankNodes);
      return terms;
  }
  /**
   * @param thing The [Thing]] to read a Literal of the given type from.
   * @param property The given Property for which you want the Literal value.
   * @param literalType Set type of the Literal data.
   * @returns The stringified value for the given Property and type, if present, or null otherwise.
   */
  function getLiteralOfType(thing, property, literalType) {
      var _a, _b, _c, _d;
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      return (_d = (_c = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.literals) === null || _b === void 0 ? void 0 : _b[literalType]) === null || _c === void 0 ? void 0 : _c[0]) !== null && _d !== void 0 ? _d : null;
  }
  /**
   * @param thing The [Thing]] to read the Literals of the given type from.
   * @param property The given Property for which you want the Literal values.
   * @param literalType Set type of the Literal data.
   * @returns The stringified values for the given Property and type.
   */
  function getLiteralAllOfType(thing, property, literalType) {
      var _a, _b, _c;
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const literalsOfType = (_c = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.literals) === null || _b === void 0 ? void 0 : _b[literalType]) !== null && _c !== void 0 ? _c : [];
      return [...literalsOfType];
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Extract Quads with a given Subject from a [[SolidDataset]] into a [[Thing]].
   *
   * @param solidDataset The [[SolidDataset]] to extract the [[Thing]] from.
   * @param thingUrl The URL of the desired [[Thing]].
   * @param options Not yet implemented.
   */
  function getThing(solidDataset, thingUrl, options = {}) {
      var _a;
      if (!internal_isValidUrl(thingUrl)) {
          throw new ValidThingUrlExpectedError(thingUrl);
      }
      const graph = typeof options.scope !== "undefined"
          ? internal_toIriString(options.scope)
          : "default";
      const thingsByIri = (_a = solidDataset.graphs[graph]) !== null && _a !== void 0 ? _a : {};
      const thingIri = internal_toIriString(thingUrl);
      const resolvedThingIri = isLocalNodeIri(thingIri) && hasServerResourceInfo(solidDataset)
          ? resolveLocalIri(getLocalNodeName(thingIri), getSourceUrl(solidDataset))
          : thingIri;
      const thing = thingsByIri[resolvedThingIri];
      if (typeof thing === "undefined") {
          return null;
      }
      return thing;
  }
  /**
   * Get all [[Thing]]s about which a [[SolidDataset]] contains Quads.
   *
   * @param solidDataset The [[SolidDataset]] to extract the [[Thing]]s from.
   * @param options Not yet implemented.
   */
  function getThingAll(solidDataset, options = {}) {
      var _a;
      const graph = typeof options.scope !== "undefined"
          ? internal_toIriString(options.scope)
          : "default";
      const thingsByIri = (_a = solidDataset.graphs[graph]) !== null && _a !== void 0 ? _a : {};
      return Object.values(thingsByIri).filter((thing) => !isBlankNodeId(thing.url));
  }
  /**
   * Insert a [[Thing]] into a [[SolidDataset]], replacing previous instances of that Thing.
   *
   * @param solidDataset The SolidDataset to insert a Thing into.
   * @param thing The Thing to insert into the given SolidDataset.
   * @returns A new SolidDataset equal to the given SolidDataset, but with the given Thing.
   */
  function setThing(solidDataset, thing) {
      var _a;
      const thingIri = isThingLocal(thing) && hasServerResourceInfo(solidDataset)
          ? resolveLocalIri(getLocalNodeName(thing.url), getSourceUrl(solidDataset))
          : thing.url;
      const defaultGraph = solidDataset.graphs.default;
      const updatedDefaultGraph = freeze(Object.assign(Object.assign({}, defaultGraph), { [thingIri]: freeze(Object.assign(Object.assign({}, thing), { url: thingIri })) }));
      const updatedGraphs = freeze(Object.assign(Object.assign({}, solidDataset.graphs), { default: updatedDefaultGraph }));
      const subjectNode = DataFactory$1.namedNode(thingIri);
      const deletedThingPredicates = (_a = solidDataset.graphs.default[thingIri]) === null || _a === void 0 ? void 0 : _a.predicates;
      const deletions = typeof deletedThingPredicates !== "undefined"
          ? subjectToRdfJsQuads(deletedThingPredicates, subjectNode, DataFactory$1.defaultGraph())
          : [];
      const additions = subjectToRdfJsQuads(thing.predicates, subjectNode, DataFactory$1.defaultGraph());
      return internal_addAdditionsToChangeLog(internal_addDeletionsToChangeLog(freeze(Object.assign(Object.assign({}, solidDataset), { graphs: updatedGraphs })), deletions), additions);
  }
  /**
   * Remove a Thing from a SolidDataset.
   *
   * @param solidDataset The SolidDataset to remove a Thing from.
   * @param thing The Thing to remove from `solidDataset`.
   * @returns A new [[SolidDataset]] equal to the input SolidDataset, excluding the given Thing.
   */
  function removeThing(solidDataset, thing) {
      var _a;
      let thingIri;
      if (isNamedNode$1(thing)) {
          thingIri = thing.value;
      }
      else if (typeof thing === "string") {
          thingIri =
              isLocalNodeIri(thing) && hasServerResourceInfo(solidDataset)
                  ? resolveLocalIri(getLocalNodeName(thing), getSourceUrl(solidDataset))
                  : thing;
      }
      else if (isThingLocal(thing)) {
          thingIri = thing.url;
      }
      else {
          thingIri = asIri(thing);
      }
      const defaultGraph = solidDataset.graphs.default;
      const updatedDefaultGraph = Object.assign({}, defaultGraph);
      delete updatedDefaultGraph[thingIri];
      const updatedGraphs = freeze(Object.assign(Object.assign({}, solidDataset.graphs), { default: freeze(updatedDefaultGraph) }));
      const subjectNode = DataFactory$1.namedNode(thingIri);
      const deletedThingPredicates = (_a = solidDataset.graphs.default[thingIri]) === null || _a === void 0 ? void 0 : _a.predicates;
      const deletions = typeof deletedThingPredicates !== "undefined"
          ? subjectToRdfJsQuads(deletedThingPredicates, subjectNode, DataFactory$1.defaultGraph())
          : [];
      return internal_addDeletionsToChangeLog(freeze(Object.assign(Object.assign({}, solidDataset), { graphs: updatedGraphs })), deletions);
  }
  function createThing(options = {}) {
      var _a;
      if (typeof options.url !== "undefined") {
          const url = options.url;
          if (!internal_isValidUrl(url)) {
              throw new ValidThingUrlExpectedError(url);
          }
          const thing = freeze({
              type: "Subject",
              predicates: freeze({}),
              url: url,
          });
          return thing;
      }
      const name = (_a = options.name) !== null && _a !== void 0 ? _a : generateName();
      const localNodeIri = getLocalNodeIri(name);
      const thing = freeze({
          type: "Subject",
          predicates: freeze({}),
          url: localNodeIri,
      });
      return thing;
  }
  /**
   * @param input An value that might be a [[Thing]].
   * @returns Whether `input` is a Thing.
   * @since 0.2.0
   */
  function isThing(input) {
      return (typeof input === "object" &&
          input !== null &&
          typeof input.type === "string" &&
          input.type === "Subject");
  }
  function asUrl(thing, baseUrl) {
      if (isThingLocal(thing)) {
          if (typeof baseUrl === "undefined") {
              throw new Error("The URL of a Thing that has not been persisted cannot be determined without a base URL.");
          }
          return resolveLocalIri(getLocalNodeName(thing.url), baseUrl);
      }
      return thing.url;
  }
  /** @hidden Alias of [[asUrl]] for those who prefer IRI terminology. */
  const asIri = asUrl;
  /**
   * Gets a human-readable representation of the given Thing to aid debugging.
   *
   * Note that changes to the exact format of the return value are not considered a breaking change;
   * it is intended to aid in debugging, not as a serialisation method that can be reliably parsed.
   *
   * @param thing The Thing to get a human-readable representation of.
   * @since 0.3.0
   */
  function thingAsMarkdown(thing) {
      let thingAsMarkdown = "";
      if (isThingLocal(thing)) {
          thingAsMarkdown += `## Thing (no URL yet — identifier: \`#${getLocalNodeName(thing.url)}\`)\n`;
      }
      else {
          thingAsMarkdown += `## Thing: ${thing.url}\n`;
      }
      const predicateIris = Object.keys(thing.predicates);
      if (predicateIris.length === 0) {
          thingAsMarkdown += "\n<empty>\n";
      }
      else {
          for (const predicate of predicateIris) {
              thingAsMarkdown += `\nProperty: ${predicate}\n`;
              const values = getTermAll(thing, predicate);
              values.forEach((value) => {
                  thingAsMarkdown += `- ${internal_getReadableValue(value)}\n`;
              });
          }
      }
      return thingAsMarkdown;
  }
  /**
   * @param thing The [[Thing]] of which a URL might or might not be known.
   * @return `true` if `thing` has no known URL yet.
   * @since 1.7.0
   */
  function isThingLocal(thing) {
      return isLocalNodeIri(thing.url);
  }
  /**
   * This error is thrown when a function expected to receive a [[Thing]] but received something else.
   * @since 1.2.0
   */
  class ThingExpectedError extends SolidClientError {
      constructor(receivedValue) {
          const message = `Expected a Thing, but received: [${receivedValue}].`;
          super(message);
          this.receivedValue = receivedValue;
      }
  }
  /**
   * This error is thrown when a function expected to receive a valid URL to identify a property but received something else.
   */
  class ValidPropertyUrlExpectedError extends SolidClientError {
      constructor(receivedValue) {
          const value = isNamedNode$1(receivedValue)
              ? receivedValue.value
              : receivedValue;
          const message = `Expected a valid URL to identify a property, but received: [${value}].`;
          super(message);
          this.receivedProperty = value;
      }
  }
  /**
   * This error is thrown when a function expected to receive a valid URL value but received something else.
   */
  class ValidValueUrlExpectedError extends SolidClientError {
      constructor(receivedValue) {
          const value = isNamedNode$1(receivedValue)
              ? receivedValue.value
              : receivedValue;
          const message = `Expected a valid URL value, but received: [${value}].`;
          super(message);
          this.receivedValue = value;
      }
  }
  /**
   * This error is thrown when a function expected to receive a valid URL to identify a [[Thing]] but received something else.
   */
  class ValidThingUrlExpectedError extends SolidClientError {
      constructor(receivedValue) {
          const value = isNamedNode$1(receivedValue)
              ? receivedValue.value
              : receivedValue;
          const message = `Expected a valid URL to identify a Thing, but received: [${value}].`;
          super(message);
          this.receivedValue = value;
      }
  }
  /**
   * Generate a string that can be used as the unique identifier for a Thing
   *
   * This function works by starting with a date string (so that Things can be
   * sorted chronologically), followed by a random number generated by taking a
   * random number between 0 and 1, and cutting off the `0.`.
   *
   * @internal
   * @returns An string that's likely to be unique
   */
  const generateName = () => {
      return (Date.now().toString() + Math.random().toString().substring("0.".length));
  };

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * This function normalizes IRIs as managed by the server to ease accurate comparison.
   * @param iri
   * @hidden
   */
  function normalizeServerSideIri(iri) {
      const iriObj = new URL(iri);
      iriObj.hash = "";
      return iriObj.href;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Initialise a new [[SolidDataset]] in memory.
   *
   * @returns An empty [[SolidDataset]].
   */
  function createSolidDataset() {
      return freeze({
          type: "Dataset",
          graphs: {
              default: {},
          },
      });
  }
  /**
   * @hidden This interface is not exposed yet until we've tried it out in practice.
   */
  async function responseToSolidDataset(response, parseOptions = {}) {
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Fetching the SolidDataset at [${response.url}] failed: [${response.status}] [${response.statusText}].`, response);
      }
      const resourceInfo = responseToResourceInfo(response);
      const parsers = Object.assign({ "text/turtle": getTurtleParser() }, parseOptions.parsers);
      const contentType = getContentType$1(resourceInfo);
      if (contentType === null) {
          throw new Error(`Could not determine the content type of the Resource at [${getSourceUrl(resourceInfo)}].`);
      }
      const mimeType = contentType.split(";")[0];
      const parser = parsers[mimeType];
      if (typeof parser === "undefined") {
          throw new Error(`The Resource at [${getSourceUrl(resourceInfo)}] has a MIME type of [${mimeType}], but the only parsers available are for the following MIME types: [${Object.keys(parsers).join(", ")}].`);
      }
      const data = await response.text();
      const parsingPromise = new Promise((resolve, reject) => {
          let solidDataset = freeze({
              graphs: freeze({ default: freeze({}) }),
              type: "Dataset",
          });
          // While Quads without Blank Nodes can be added to the SolidDataset as we
          // encounter them, to parse Quads with Blank Nodes, we'll have to wait until
          // we've seen all the Quads, so that we can reconcile equal Blank Nodes.
          const quadsWithBlankNodes = [];
          const allQuads = [];
          parser.onError((error) => {
              reject(new Error(`Encountered an error parsing the Resource at [${getSourceUrl(resourceInfo)}] with content type [${contentType}]: ${error}`));
          });
          parser.onQuad((quad) => {
              allQuads.push(quad);
              if (quad.subject.termType === "BlankNode" ||
                  quad.object.termType === "BlankNode") {
                  // Quads with Blank Nodes will be parsed when all Quads are known,
                  // so that equal Blank Nodes can be reconciled:
                  quadsWithBlankNodes.push(quad);
              }
              else {
                  solidDataset = addRdfJsQuadToDataset(solidDataset, quad);
              }
          });
          parser.onComplete(async () => {
              // If a Resource contains more than this number of Blank Nodes,
              // we consider the detection of chains (O(n^2), I think) to be too
              // expensive, and just incorporate them as regular Blank Nodes with
              // non-deterministic, ad-hoc identifiers into the SolidDataset:
              const maxBlankNodesToDetectChainsFor = 20;
              // Some Blank Nodes only serve to use a set of Quads as the Object for a
              // single Subject. Those Quads will be added to the SolidDataset when
              // their Subject's Blank Node is encountered in the Object position.
              const chainBlankNodes = quadsWithBlankNodes.length <= maxBlankNodesToDetectChainsFor
                  ? getChainBlankNodes(quadsWithBlankNodes)
                  : [];
              const quadsWithoutChainBlankNodeSubjects = quadsWithBlankNodes.filter((quad) => chainBlankNodes.every((chainBlankNode) => !chainBlankNode.equals(quad.subject)));
              solidDataset = quadsWithoutChainBlankNodeSubjects.reduce((datasetAcc, quad) => addRdfJsQuadToDataset(datasetAcc, quad, {
                  otherQuads: allQuads,
                  chainBlankNodes: chainBlankNodes,
              }), solidDataset);
              const solidDatasetWithResourceInfo = freeze(Object.assign(Object.assign({}, solidDataset), resourceInfo));
              resolve(solidDatasetWithResourceInfo);
          });
          parser.parse(data, resourceInfo);
      });
      return await parsingPromise;
  }
  /**
   * Fetch a SolidDataset from the given URL. Currently requires the SolidDataset to be available as [Turtle](https://www.w3.org/TR/turtle/).
   *
   * @param url URL to fetch a [[SolidDataset]] from.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns Promise resolving to a [[SolidDataset]] containing the data at the given Resource, or rejecting if fetching it failed.
   */
  async function getSolidDataset(url, options = internal_defaultFetchOptions) {
      var _a;
      url = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const parserContentTypes = Object.keys((_a = options.parsers) !== null && _a !== void 0 ? _a : {});
      const acceptedContentTypes = parserContentTypes.length > 0
          ? parserContentTypes.join(", ")
          : "text/turtle";
      const response = await config.fetch(url, {
          headers: {
              Accept: acceptedContentTypes,
          },
      });
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Fetching the Resource at [${url}] failed: [${response.status}] [${response.statusText}].`, response);
      }
      const solidDataset = await responseToSolidDataset(response, options);
      return solidDataset;
  }
  /**
   * Create a SPARQL UPDATE Patch request from a [[SolidDataset]] with a changelog.
   * @param solidDataset the [[SolidDataset]] that has been locally updated, and that should be persisted.
   * @returns an HTTP PATCH request configuration object, aligned with the [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters), containing a SPARQL UPDATE.
   * @hidden
   */
  async function prepareSolidDatasetUpdate(solidDataset) {
      const deleteStatement = solidDataset.internal_changeLog.deletions.length > 0
          ? `DELETE DATA {${(await triplesToTurtle(solidDataset.internal_changeLog.deletions.map(getNamedNodesForLocalNodes))).trim()}};`
          : "";
      const insertStatement = solidDataset.internal_changeLog.additions.length > 0
          ? `INSERT DATA {${(await triplesToTurtle(solidDataset.internal_changeLog.additions.map(getNamedNodesForLocalNodes))).trim()}};`
          : "";
      return {
          method: "PATCH",
          body: `${deleteStatement} ${insertStatement}`,
          headers: {
              "Content-Type": "application/sparql-update",
          },
      };
  }
  /**
   * Create a Put request to write a locally created [[SolidDataset]] to a Pod.
   * @param solidDataset the [[SolidDataset]] that has been locally updated, and that should be persisted.
   * @returns an HTTP PUT request configuration object, aligned with the [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters), containing a serialization of the [[SolidDataset]].
   * @hidden
   */
  async function prepareSolidDatasetCreation(solidDataset) {
      return {
          method: "PUT",
          body: await triplesToTurtle(toRdfJsQuads(solidDataset).map(getNamedNodesForLocalNodes)),
          headers: {
              "Content-Type": "text/turtle",
              "If-None-Match": "*",
              Link: `<${ldp.Resource}>; rel="type"`,
          },
      };
  }
  /**
   * Given a SolidDataset, store it in a Solid Pod (overwriting the existing data at the given URL).
   *
   * A SolidDataset keeps track of the data changes compared to the data in the Pod; i.e.,
   * the changelog tracks both the old value and new values of the property being modified. This
   * function applies the changes to the current SolidDataset. If the old value specified in the
   * changelog does not correspond to the value currently in the Pod, this function will throw an
   * error.
   * The SolidDataset returned by this function will contain the data sent to the Pod, and a ChangeLog
   * up-to-date with the saved data. Note that if the data on the server was modified in between the
   * first fetch and saving it, the updated data will not be reflected in the returned SolidDataset.
   * To make sure you have the latest data, call [[getSolidDataset]] again after saving the data.
   *
   * The Solid server will create any intermediary Containers that do not exist yet, so they do not
   * need to be created in advance. For example, if the target URL is
   * https://example.pod/container/resource and https://example.pod/container/ does not exist yet,
   * it will exist after this function resolves successfully.
   *
   * @param url URL to save `solidDataset` to.
   * @param solidDataset The [[SolidDataset]] to save.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A Promise resolving to a [[SolidDataset]] containing the stored data, or rejecting if saving it failed.
   */
  async function saveSolidDatasetAt(url, solidDataset, options = internal_defaultFetchOptions) {
      url = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const datasetWithChangelog = internal_withChangeLog(solidDataset);
      const requestInit = isUpdate(datasetWithChangelog, url)
          ? await prepareSolidDatasetUpdate(datasetWithChangelog)
          : await prepareSolidDatasetCreation(datasetWithChangelog);
      const response = await config.fetch(url, requestInit);
      if (internal_isUnsuccessfulResponse(response)) {
          const diagnostics = isUpdate(datasetWithChangelog, url)
              ? "The changes that were sent to the Pod are listed below.\n\n" +
                  changeLogAsMarkdown(datasetWithChangelog)
              : "The SolidDataset that was sent to the Pod is listed below.\n\n" +
                  solidDatasetAsMarkdown(datasetWithChangelog);
          throw new FetchError(`Storing the Resource at [${url}] failed: [${response.status}] [${response.statusText}].\n\n` +
              diagnostics, response);
      }
      const resourceInfo = Object.assign(Object.assign({}, internal_parseResourceInfo(response)), { isRawData: false });
      const storedDataset = freeze(Object.assign(Object.assign({}, solidDataset), { internal_changeLog: { additions: [], deletions: [] }, internal_resourceInfo: resourceInfo }));
      const storedDatasetWithResolvedIris = resolveLocalIrisInSolidDataset(storedDataset);
      return storedDatasetWithResolvedIris;
  }
  /**
   * Deletes the SolidDataset at a given URL.
   *
   * If operating on a container, the container must be empty otherwise a 409 CONFLICT will be raised.
   *
   * @param file The (URL of the) SolidDataset to delete
   * @since 0.6.0
   */
  async function deleteSolidDataset(solidDataset, options = internal_defaultFetchOptions) {
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const url = hasResourceInfo(solidDataset)
          ? internal_toIriString(getSourceUrl(solidDataset))
          : internal_toIriString(solidDataset);
      const response = await config.fetch(url, { method: "DELETE" });
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Deleting the SolidDataset at [${url}] failed: [${response.status}] [${response.statusText}].`, response);
      }
  }
  /**
   * Create an empty Container at the given URL.
   *
   * Throws an error if creating the Container failed, e.g. because the current user does not have
   * permissions to, or because the Container already exists.
   *
   * Note that a Solid server will automatically create the necessary Containers when storing a
   * Resource; i.e. there is no need to call this function if it is immediately followed by
   * [[saveSolidDatasetAt]] or [[overwriteFile]].
   *
   * @param url URL of the empty Container that is to be created.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @since 0.2.0
   */
  async function createContainerAt(url, options = internal_defaultFetchOptions) {
      url = internal_toIriString(url);
      url = url.endsWith("/") ? url : url + "/";
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const response = await config.fetch(url, {
          method: "PUT",
          headers: {
              Accept: "text/turtle",
              "Content-Type": "text/turtle",
              "If-None-Match": "*",
              // This header should not be required to create a Container,
              // but ESS currently expects it:
              Link: `<${ldp.BasicContainer}>; rel="type"`,
          },
      });
      if (internal_isUnsuccessfulResponse(response)) {
          if (response.status === 409 &&
              response.statusText === "Conflict" &&
              (await response.text()).trim() ===
                  internal_NSS_CREATE_CONTAINER_SPEC_NONCOMPLIANCE_DETECTION_ERROR_MESSAGE_TO_WORKAROUND_THEIR_ISSUE_1465) {
              return createContainerWithNssWorkaroundAt(url, options);
          }
          throw new FetchError(`Creating the empty Container at [${url}] failed: [${response.status}] [${response.statusText}].`, response);
      }
      const resourceInfo = internal_parseResourceInfo(response);
      const containerDataset = freeze(Object.assign(Object.assign({}, createSolidDataset()), { internal_changeLog: { additions: [], deletions: [] }, internal_resourceInfo: resourceInfo }));
      return containerDataset;
  }
  /**
   * Unfortunately Node Solid Server does not confirm to the Solid spec when it comes to Container
   * creation. When we make the (valid, according to the Solid protocol) request to create a
   * Container, NSS responds with the following exact error message. Thus, when we encounter exactly
   * this message, we use an NSS-specific workaround ([[createContainerWithNssWorkaroundAt]]). Both
   * this constant and that workaround should be removed once the NSS issue has been fixed and
   * no versions of NSS with the issue are in common use/supported anymore.
   *
   * @see https://github.com/solid/node-solid-server/issues/1465
   * @internal
   */
  const internal_NSS_CREATE_CONTAINER_SPEC_NONCOMPLIANCE_DETECTION_ERROR_MESSAGE_TO_WORKAROUND_THEIR_ISSUE_1465 = "Can't write file: PUT not supported on containers, use POST instead";
  /**
   * Unfortunately Node Solid Server does not confirm to the Solid spec when it comes to Container
   * creation. As a workaround, we create a dummy file _inside_ the desired Container (which should
   * create the desired Container on the fly), and then delete it again.
   *
   * @see https://github.com/solid/node-solid-server/issues/1465
   */
  const createContainerWithNssWorkaroundAt = async (url, options) => {
      url = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      let existingContainer;
      try {
          existingContainer = await getResourceInfo(url, options);
      }
      catch (e) {
          // To create the Container, we'd want it to not exist yet. In other words, we'd expect to get
          // a 404 error here in the happy path - so do nothing if that's the case.
          if (!(e instanceof FetchError) || e.statusCode !== 404) {
              // (But if we get an error other than a 404, just throw that error like we usually would.)
              throw e;
          }
      }
      if (typeof existingContainer !== "undefined") {
          throw new Error(`The Container at [${url}] already exists, and therefore cannot be created again.`);
      }
      const dummyUrl = url + ".dummy";
      const createResponse = await config.fetch(dummyUrl, {
          method: "PUT",
          headers: {
              Accept: "text/turtle",
              "Content-Type": "text/turtle",
          },
      });
      if (internal_isUnsuccessfulResponse(createResponse)) {
          throw new FetchError(`Creating the empty Container at [${url}] failed: [${createResponse.status}] [${createResponse.statusText}].`, createResponse);
      }
      await config.fetch(dummyUrl, { method: "DELETE" });
      const containerInfoResponse = await config.fetch(url, { method: "HEAD" });
      const resourceInfo = internal_parseResourceInfo(containerInfoResponse);
      const containerDataset = freeze(Object.assign(Object.assign({}, createSolidDataset()), { internal_changeLog: { additions: [], deletions: [] }, internal_resourceInfo: resourceInfo }));
      return containerDataset;
  };
  function isSourceIriEqualTo(dataset, iri) {
      return (normalizeServerSideIri(dataset.internal_resourceInfo.sourceIri) ===
          normalizeServerSideIri(iri));
  }
  function isUpdate(solidDataset, url) {
      return (hasChangelog(solidDataset) &&
          hasResourceInfo(solidDataset) &&
          typeof solidDataset.internal_resourceInfo.sourceIri === "string" &&
          isSourceIriEqualTo(solidDataset, url));
  }
  /**
   * Given a SolidDataset, store it in a Solid Pod in a new Resource inside a Container.
   *
   * The Container at the given URL should already exist; if it does not, you can initialise it first
   * using [[createContainerAt]], or directly save the SolidDataset at the desired location using
   * [[saveSolidDatasetAt]].
   *
   * This function is primarily useful if the current user does not have access to change existing files in
   * a Container, but is allowed to add new files; in other words, they have Append, but not Write
   * access to a Container. This is useful in situations where someone wants to allow others to,
   * for example, send notifications to their Pod, but not to view or delete existing notifications.
   * You can pass a suggestion for the new Resource's name, but the server may decide to give it
   * another name — for example, if a Resource with that name already exists inside the given
   * Container.
   * If the user does have access to write directly to a given location, [[saveSolidDatasetAt]]
   * will do the job just fine, and does not require the parent Container to exist in advance.
   *
   * @param containerUrl URL of the Container in which to create a new Resource.
   * @param solidDataset The [[SolidDataset]] to save to a new Resource in the given Container.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A Promise resolving to a [[SolidDataset]] containing the saved data. The Promise rejects if the save failed.
   */
  async function saveSolidDatasetInContainer(containerUrl, solidDataset, options = internal_defaultFetchOptions) {
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      containerUrl = internal_toIriString(containerUrl);
      const rawTurtle = await triplesToTurtle(toRdfJsQuads(solidDataset).map(getNamedNodesForLocalNodes));
      const headers = {
          "Content-Type": "text/turtle",
          Link: `<${ldp.Resource}>; rel="type"`,
      };
      if (options.slugSuggestion) {
          headers.slug = options.slugSuggestion;
      }
      const response = await config.fetch(containerUrl, {
          method: "POST",
          body: rawTurtle,
          headers: headers,
      });
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Storing the Resource in the Container at [${containerUrl}] failed: [${response.status}] [${response.statusText}].\n\n` +
              "The SolidDataset that was sent to the Pod is listed below.\n\n" +
              solidDatasetAsMarkdown(solidDataset), response);
      }
      const locationHeader = response.headers.get("Location");
      if (locationHeader === null) {
          throw new Error("Could not determine the location of the newly saved SolidDataset.");
      }
      const resourceIri = new URL(locationHeader, response.url).href;
      const resourceInfo = {
          internal_resourceInfo: {
              isRawData: false,
              sourceIri: resourceIri,
          },
      };
      const resourceWithResourceInfo = freeze(Object.assign(Object.assign({}, solidDataset), resourceInfo));
      const resourceWithResolvedIris = resolveLocalIrisInSolidDataset(resourceWithResourceInfo);
      return resourceWithResolvedIris;
  }
  /**
   * Create an empty Container inside the Container at the given URL.
   *
   * Throws an error if creating the Container failed, e.g. because the current user does not have
   * permissions to.
   *
   * The Container in which to create the new Container should itself already exist.
   *
   * This function is primarily useful if the current user does not have access to change existing files in
   * a Container, but is allowed to add new files; in other words, they have Append, but not Write
   * access to a Container. This is useful in situations where someone wants to allow others to,
   * for example, send notifications to their Pod, but not to view or delete existing notifications.
   * You can pass a suggestion for the new Resource's name, but the server may decide to give it
   * another name — for example, if a Resource with that name already exists inside the given
   * Container.
   * If the user does have access to write directly to a given location, [[createContainerAt]]
   * will do the job just fine, and does not require the parent Container to exist in advance.
   *
   * @param containerUrl URL of the Container in which the empty Container is to be created.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @since 0.2.0
   */
  async function createContainerInContainer(containerUrl, options = internal_defaultFetchOptions) {
      containerUrl = internal_toIriString(containerUrl);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const headers = {
          "Content-Type": "text/turtle",
          Link: `<${ldp.BasicContainer}>; rel="type"`,
      };
      if (options.slugSuggestion) {
          headers.slug = options.slugSuggestion;
      }
      const response = await config.fetch(containerUrl, {
          method: "POST",
          headers: headers,
      });
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Creating an empty Container in the Container at [${containerUrl}] failed: [${response.status}] [${response.statusText}].`, response);
      }
      const locationHeader = response.headers.get("Location");
      if (locationHeader === null) {
          throw new Error("Could not determine the location of the newly created Container.");
      }
      const resourceIri = new URL(locationHeader, response.url).href;
      const resourceInfo = {
          internal_resourceInfo: {
              isRawData: false,
              sourceIri: resourceIri,
          },
      };
      const resourceWithResourceInfo = freeze(Object.assign(Object.assign({}, createSolidDataset()), resourceInfo));
      return resourceWithResourceInfo;
  }
  /**
   * Deletes the Container at a given URL.
   *
   * @param file The (URL of the) Container to delete
   * @since 0.6.0
   */
  async function deleteContainer(container, options = internal_defaultFetchOptions) {
      const url = hasResourceInfo(container)
          ? internal_toIriString(getSourceUrl(container))
          : internal_toIriString(container);
      if (!isContainer(container)) {
          throw new Error(`You're trying to delete the Container at [${url}], but Container URLs should end in a \`/\`. Are you sure this is a Container?`);
      }
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const response = await config.fetch(url, { method: "DELETE" });
      if (internal_isUnsuccessfulResponse(response)) {
          throw new FetchError(`Deleting the Container at [${url}] failed: [${response.status}] [${response.statusText}].`, response);
      }
  }
  /**
   * Given a [[SolidDataset]] representing a Container (see [[isContainer]]), fetch the URLs of all
   * contained resources.
   * If the solidDataset given is not a container, or is missing resourceInfo, throw an error.
   *
   * @param solidDataset The container from which to fetch all contained Resource URLs.
   * @returns A list of URLs, each of which points to a contained Resource of the given SolidDataset.
   * @since 1.3.0
   */
  function getContainedResourceUrlAll(solidDataset) {
      const container = getThing(solidDataset, getSourceUrl(solidDataset));
      // See https://www.w3.org/TR/2015/REC-ldp-20150226/#h-ldpc-http_post:
      // > a containment triple MUST be added to the state of the LDPC whose subject is the LDPC URI,
      // > whose predicate is ldp:contains and whose object is the URI for the newly created document
      return container !== null ? getIriAll(container, ldp.contains) : [];
  }
  /**
   * Gets a human-readable representation of the given SolidDataset to aid debugging.
   *
   * Note that changes to the exact format of the return value are not considered a breaking change;
   * it is intended to aid in debugging, not as a serialisation method that can be reliably parsed.
   *
   * @param solidDataset The [[SolidDataset]] to get a human-readable representation of.
   * @since 0.3.0
   */
  function solidDatasetAsMarkdown(solidDataset) {
      let readableSolidDataset = "";
      if (hasResourceInfo(solidDataset)) {
          readableSolidDataset += `# SolidDataset: ${getSourceUrl(solidDataset)}\n`;
      }
      else {
          readableSolidDataset += `# SolidDataset (no URL yet)\n`;
      }
      const things = getThingAll(solidDataset);
      if (things.length === 0) {
          readableSolidDataset += "\n<empty>\n";
      }
      else {
          things.forEach((thing) => {
              readableSolidDataset += "\n" + thingAsMarkdown(thing);
              if (hasChangelog(solidDataset)) {
                  readableSolidDataset +=
                      "\n" + getReadableChangeLogSummary(solidDataset, thing) + "\n";
              }
          });
      }
      return readableSolidDataset;
  }
  /**
   * Gets a human-readable representation of the local changes to a Resource to aid debugging.
   *
   * Note that changes to the exact format of the return value are not considered a breaking change;
   * it is intended to aid in debugging, not as a serialisation method that can be reliably parsed.
   *
   * @param solidDataset The Resource of which to get a human-readable representation of the changes applied to it locally.
   * @since 0.3.0
   */
  function changeLogAsMarkdown(solidDataset) {
      if (!hasResourceInfo(solidDataset)) {
          return "This is a newly initialized SolidDataset, so there is no source to compare it to.";
      }
      if (!hasChangelog(solidDataset) ||
          (solidDataset.internal_changeLog.additions.length === 0 &&
              solidDataset.internal_changeLog.deletions.length === 0)) {
          return (`## Changes compared to ${getSourceUrl(solidDataset)}\n\n` +
              `This SolidDataset has not been modified since it was fetched from ${getSourceUrl(solidDataset)}.\n`);
      }
      let readableChangeLog = `## Changes compared to ${getSourceUrl(solidDataset)}\n`;
      const changeLogsByThingAndProperty = sortChangeLogByThingAndProperty(solidDataset);
      Object.keys(changeLogsByThingAndProperty).forEach((thingUrl) => {
          readableChangeLog += `\n### Thing: ${thingUrl}\n`;
          const changeLogByProperty = changeLogsByThingAndProperty[thingUrl];
          Object.keys(changeLogByProperty).forEach((propertyUrl) => {
              readableChangeLog += `\nProperty: ${propertyUrl}\n`;
              const deleted = changeLogByProperty[propertyUrl].deleted;
              const added = changeLogByProperty[propertyUrl].added;
              if (deleted.length > 0) {
                  readableChangeLog += "- Removed:\n";
                  deleted.forEach((deletedValue) => (readableChangeLog += `  - ${internal_getReadableValue(deletedValue)}\n`));
              }
              if (added.length > 0) {
                  readableChangeLog += "- Added:\n";
                  added.forEach((addedValue) => (readableChangeLog += `  - ${internal_getReadableValue(addedValue)}\n`));
              }
          });
      });
      return readableChangeLog;
  }
  function sortChangeLogByThingAndProperty(solidDataset) {
      const changeLogsByThingAndProperty = {};
      solidDataset.internal_changeLog.deletions.forEach((deletion) => {
          var _a, _b;
          var _c;
          const subjectNode = isLocalNode(deletion.subject)
              ? /* istanbul ignore next: Unsaved deletions should be removed from the additions list instead, so this code path shouldn't be hit: */
                  resolveIriForLocalNode(deletion.subject, getSourceUrl(solidDataset))
              : deletion.subject;
          if (!isNamedNode$1(subjectNode) || !isNamedNode$1(deletion.predicate)) {
              return;
          }
          const thingUrl = internal_toIriString(subjectNode);
          const propertyUrl = internal_toIriString(deletion.predicate);
          (_a = changeLogsByThingAndProperty[thingUrl]) !== null && _a !== void 0 ? _a : (changeLogsByThingAndProperty[thingUrl] = {});
          (_b = (_c = changeLogsByThingAndProperty[thingUrl])[propertyUrl]) !== null && _b !== void 0 ? _b : (_c[propertyUrl] = {
              added: [],
              deleted: [],
          });
          changeLogsByThingAndProperty[thingUrl][propertyUrl].deleted.push(deletion.object);
      });
      solidDataset.internal_changeLog.additions.forEach((addition) => {
          var _a, _b;
          var _c;
          const subjectNode = isLocalNode(addition.subject)
              ? /* istanbul ignore next: setThing already resolves local Subjects when adding them, so this code path should never be hit. */
                  resolveIriForLocalNode(addition.subject, getSourceUrl(solidDataset))
              : addition.subject;
          if (!isNamedNode$1(subjectNode) || !isNamedNode$1(addition.predicate)) {
              return;
          }
          const thingUrl = internal_toIriString(subjectNode);
          const propertyUrl = internal_toIriString(addition.predicate);
          (_a = changeLogsByThingAndProperty[thingUrl]) !== null && _a !== void 0 ? _a : (changeLogsByThingAndProperty[thingUrl] = {});
          (_b = (_c = changeLogsByThingAndProperty[thingUrl])[propertyUrl]) !== null && _b !== void 0 ? _b : (_c[propertyUrl] = {
              added: [],
              deleted: [],
          });
          changeLogsByThingAndProperty[thingUrl][propertyUrl].added.push(addition.object);
      });
      return changeLogsByThingAndProperty;
  }
  function getReadableChangeLogSummary(solidDataset, thing) {
      const subject = DataFactory$1.namedNode(thing.url);
      const nrOfAdditions = solidDataset.internal_changeLog.additions.reduce((count, addition) => (addition.subject.equals(subject) ? count + 1 : count), 0);
      const nrOfDeletions = solidDataset.internal_changeLog.deletions.reduce((count, deletion) => (deletion.subject.equals(subject) ? count + 1 : count), 0);
      const additionString = nrOfAdditions === 1
          ? "1 new value added"
          : nrOfAdditions + " new values added";
      const deletionString = nrOfDeletions === 1 ? "1 value removed" : nrOfDeletions + " values removed";
      return `(${additionString} / ${deletionString})`;
  }
  function getNamedNodesForLocalNodes(quad) {
      const subject = isNamedNode$1(quad.subject)
          ? getNamedNodeFromLocalNode(quad.subject)
          : /* istanbul ignore next: We don't allow non-NamedNodes as the Subject, so this code path should never be hit: */
              quad.subject;
      const object = isNamedNode$1(quad.object)
          ? getNamedNodeFromLocalNode(quad.object)
          : quad.object;
      return DataFactory$1.quad(subject, quad.predicate, object, quad.graph);
  }
  function getNamedNodeFromLocalNode(node) {
      if (isLocalNodeIri(node.value)) {
          return DataFactory$1.namedNode("#" + getLocalNodeName(node.value));
      }
      return node;
  }
  function resolveLocalIrisInSolidDataset(solidDataset) {
      const resourceIri = getSourceUrl(solidDataset);
      const defaultGraph = solidDataset.graphs.default;
      const thingIris = Object.keys(defaultGraph);
      const updatedDefaultGraph = thingIris.reduce((graphAcc, thingIri) => {
          const resolvedThing = resolveLocalIrisInThing(graphAcc[thingIri], resourceIri);
          const resolvedThingIri = isLocalNodeIri(thingIri)
              ? `${resourceIri}#${getLocalNodeName(thingIri)}`
              : thingIri;
          const updatedGraph = Object.assign({}, graphAcc);
          delete updatedGraph[thingIri];
          updatedGraph[resolvedThingIri] = resolvedThing;
          return freeze(updatedGraph);
      }, defaultGraph);
      const updatedGraphs = freeze(Object.assign(Object.assign({}, solidDataset.graphs), { default: updatedDefaultGraph }));
      return freeze(Object.assign(Object.assign({}, solidDataset), { graphs: updatedGraphs }));
  }
  function resolveLocalIrisInThing(thing, baseIri) {
      const predicateIris = Object.keys(thing.predicates);
      const updatedPredicates = predicateIris.reduce((predicatesAcc, predicateIri) => {
          var _a;
          const namedNodes = (_a = predicatesAcc[predicateIri].namedNodes) !== null && _a !== void 0 ? _a : [];
          if (namedNodes.every((namedNode) => !isLocalNodeIri(namedNode))) {
              // This Predicate has no local node Objects, so return it unmodified:
              return predicatesAcc;
          }
          const updatedNamedNodes = freeze(namedNodes.map((namedNode) => isLocalNodeIri(namedNode)
              ? `${baseIri}#${getLocalNodeName(namedNode)}`
              : namedNode));
          const updatedPredicate = freeze(Object.assign(Object.assign({}, predicatesAcc[predicateIri]), { namedNodes: updatedNamedNodes }));
          return freeze(Object.assign(Object.assign({}, predicatesAcc), { [predicateIri]: updatedPredicate }));
      }, thing.predicates);
      return freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates, url: isLocalNodeIri(thing.url)
              ? `${baseIri}#${getLocalNodeName(thing.url)}`
              : thing.url }));
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const { Response } = crossFetch;
  /**
   * ```{warning}
   * Do not use this function in production code. For use in **unit tests**.
   * ```
   *
   * This function initialises a new [[SolidDataset]] with metadata as though the
   * SolidDataset has been retrieved from the given URL. The mock SolidDataset can be used in
   * unit tests that require persisted SolidDatasets; e.g., unit tests that call [[getSourceUrl]].
   *
   * @param url The URL from which the returned SolidDataset appears to be retrieved.
   * @returns A mock SolidDataset that appears to be retrieved from the `url`.
   * @since 0.2.0
   */
  function mockSolidDatasetFrom(url) {
      const solidDataset = createSolidDataset();
      const solidDatasetWithResourceInfo = Object.assign(Object.assign({}, solidDataset), { internal_resourceInfo: {
              sourceIri: internal_toIriString(url),
              isRawData: false,
              contentType: "text/turtle",
              linkedResources: {},
          } });
      return solidDatasetWithResourceInfo;
  }
  /**
   * ```{warning}
   * Do not use this function in production code. For use in **unit tests**.
   * ```
   *
   * This function initialises a new Container [[SolidDataset]] with metadata as though the
   * Container has been retrieved from the given URL. The mock SolidDataset can be used in
   * unit tests that require persisted Containers; e.g., unit tests that call [[isContainer]].
   *
   * @param url The URL from which the returned Container appears to be retrieved. The `url` must end in a slash.
   * @returns A mock SolidDataset that appears to be retrieved from the `url`.
   * @since 0.2.0
   */
  function mockContainerFrom(url) {
      const sourceIri = internal_toIriString(url);
      if (!sourceIri.endsWith("/")) {
          throw new Error("A Container's URL should end in a slash. Please update your tests.");
      }
      return mockSolidDatasetFrom(sourceIri);
  }
  /**
   * ```{warning}
   * Do not use this function in production code. For use in **unit tests**.
   * ```
   *
   * This function initialises a new File with metadata as though the
   * File has been retrieved from the given URL. The mock File can be used in
   * unit tests that require persisted Files; e.g. unit tests that call [[getSourceUrl]].
   *
   * @param url The URL from which the returned File appears to be retrieved.
   * @returns A mock File that appears to be retrieved from the `url`.
   * @since 0.2.0
   */
  function mockFileFrom(url, options) {
      const file = new Blob();
      const fileWithResourceInfo = Object.assign(file, {
          internal_resourceInfo: {
              sourceIri: internal_toIriString(url),
              isRawData: true,
              contentType: options === null || options === void 0 ? void 0 : options.contentType,
              linkedResources: {},
          },
      });
      return fileWithResourceInfo;
  }
  /**
   * ```{warning}
   * Do not use this function in production code. For use in **unit tests**.
   * ```
   *
   * This function initialises a new Error object with metadata as though the
   * it was the result of getting a 404 when trying to fetch the Resource at the
   * given URL. The mock Error can be used in unit tests that require functions
   * that fetch Resources (like [[getSolidDataset]]) to fail.
   *
   * @param url The URL of the Resource that could not be fetched according to the error.
   * @param statusCode Optional status code (defaults to 404) that caused the error.
   * @returns A mock Error that represents not having been able to fetch the Resource at `url` due to a 404 Response.
   * @since 1.1.0
   */
  function mockFetchError(fetchedUrl, statusCode = 404) {
      const failedResponse = new Response(undefined, {
          status: statusCode,
      });
      return new FetchError(`Fetching the Resource at [${fetchedUrl}] failed: [${failedResponse.status}] [${failedResponse.statusText}].`, failedResponse);
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Create a new Thing with a URL added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setUrl]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add a URL value to.
   * @param property Property for which to add the given URL value.
   * @param url URL to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  const addUrl = (thing, property, url) => {
      var _a, _b;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      if (!isThing(url) && !internal_isValidUrl(url)) {
          throw new ValidValueUrlExpectedError(url);
      }
      const predicateIri = internal_toIriString(property);
      const existingPredicate = (_a = thing.predicates[predicateIri]) !== null && _a !== void 0 ? _a : {};
      const existingNamedNodes = (_b = existingPredicate.namedNodes) !== null && _b !== void 0 ? _b : [];
      let iriToAdd;
      if (isNamedNode$1(url)) {
          iriToAdd = url.value;
      }
      else if (typeof url === "string") {
          iriToAdd = url;
      }
      else if (isThingLocal(url)) {
          iriToAdd = url.url;
      }
      else {
          iriToAdd = asIri(url);
      }
      const updatedNamedNodes = freeze(existingNamedNodes.concat(internal_toIriString(iriToAdd)));
      const updatedPredicate = freeze(Object.assign(Object.assign({}, existingPredicate), { namedNodes: updatedNamedNodes }));
      const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
      const updatedThing = freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
      return updatedThing;
  };
  /** @hidden Alias for [[addUrl]] for those who prefer IRI terminology. */
  const addIri = addUrl;
  /**
   * Create a new Thing with a boolean added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setBoolean]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add a boolean value to.
   * @param property Property for which to add the given boolean value.
   * @param value Boolean to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  const addBoolean = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addLiteralOfType(thing, property, serializeBoolean(value), xmlSchemaTypes.boolean);
  };
  /**
   * Create a new Thing with a datetime added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setDatetime]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add a datetime value to.
   * @param property Property for which to add the given datetime value.
   * @param value Datetime to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  const addDatetime = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addLiteralOfType(thing, property, serializeDatetime(value), xmlSchemaTypes.dateTime);
  };
  /**
   * Create a new Thing with a date added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setDate]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add a date value to.
   * @param property Property for which to add the given date value.
   * @param value Date to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   * @since 1.10.0
   */
  const addDate = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addLiteralOfType(thing, property, serializeDate(value), xmlSchemaTypes.date);
  };
  /**
   * Create a new Thing with a time added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setDatetime]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add a datetime value to.
   * @param property Property for which to add the given datetime value.
   * @param value time to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   * @since 1.10.0
   */
  const addTime = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addLiteralOfType(thing, property, serializeTime(value), xmlSchemaTypes.time);
  };
  /**
   * Create a new Thing with a decimal added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setDecimal]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add a decimal value to.
   * @param property Property for which to add the given decimal value.
   * @param value Decimal to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  const addDecimal = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addLiteralOfType(thing, property, serializeDecimal(value), xmlSchemaTypes.decimal);
  };
  /**
   * Create a new Thing with an integer added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setInteger]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add an integer value to.
   * @param property Property for which to add the given integer value.
   * @param value Integer to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  const addInteger = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addLiteralOfType(thing, property, serializeInteger(value), xmlSchemaTypes.integer);
  };
  /**
   * Create a new Thing with a localised string added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setStringWithLocale]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add a localised string value to.
   * @param property Property for which to add the given string value.
   * @param value String to add to `thing` for the given `property`.
   * @param locale Locale of the added string.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  function addStringWithLocale(thing, property, value, locale) {
      var _a, _b, _c;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const normalizedLocale = normalizeLocale(locale);
      const existingPredicate = (_a = thing.predicates[predicateIri]) !== null && _a !== void 0 ? _a : {};
      const existingLangStrings = (_b = existingPredicate.langStrings) !== null && _b !== void 0 ? _b : {};
      const existingStringsInLocale = (_c = existingLangStrings[normalizedLocale]) !== null && _c !== void 0 ? _c : [];
      const updatedStringsInLocale = freeze(existingStringsInLocale.concat(value));
      const updatedLangStrings = freeze(Object.assign(Object.assign({}, existingLangStrings), { [normalizedLocale]: updatedStringsInLocale }));
      const updatedPredicate = freeze(Object.assign(Object.assign({}, existingPredicate), { langStrings: updatedLangStrings }));
      const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
      const updatedThing = freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
      return updatedThing;
  }
  /**
   * Create a new Thing with an unlocalised string added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setStringNoLocale]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to add an unlocalised string value to.
   * @param property Property for which to add the given string value.
   * @param value String to add to `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  const addStringNoLocale = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addLiteralOfType(thing, property, value, xmlSchemaTypes.string);
  };
  /**
   * Create a new Thing with a Named Node added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setNamedNode]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @ignore This should not be needed due to the other add*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing The [[Thing]] to add a Named Node to.
   * @param property Property for which to add a value.
   * @param value The Named Node to add.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  function addNamedNode(thing, property, value) {
      return addUrl(thing, property, value.value);
  }
  /**
   * Create a new Thing with a Literal added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setLiteral]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @ignore This should not be needed due to the other add*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing The [[Thing]] to add a Literal to.
   * @param property Property for which to add a value.
   * @param value The Literal to add.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   */
  function addLiteral(thing, property, value) {
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const typeIri = value.datatype.value;
      if (typeIri === xmlSchemaTypes.langString) {
          return addStringWithLocale(thing, property, value.value, value.language);
      }
      return addLiteralOfType(thing, property, value.value, value.datatype.value);
  }
  /**
   * Creates a new Thing with a Term added for a Property.
   *
   * This preserves existing values for the given Property. To replace them, see [[setTerm]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @ignore This should not be needed due to the other add*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing The [[Thing]] to add a Term to.
   * @param property Property for which to add a value.
   * @param value The Term to add.
   * @returns A new Thing equal to the input Thing with the given value added for the given Property.
   * @since 0.3.0
   */
  function addTerm(thing, property, value) {
      var _a, _b;
      if (value.termType === "NamedNode") {
          return addNamedNode(thing, property, value);
      }
      if (value.termType === "Literal") {
          return addLiteral(thing, property, value);
      }
      if (value.termType === "BlankNode") {
          internal_throwIfNotThing(thing);
          if (!internal_isValidUrl(property)) {
              throw new ValidPropertyUrlExpectedError(property);
          }
          const predicateIri = internal_toIriString(property);
          const existingPredicate = (_a = thing.predicates[predicateIri]) !== null && _a !== void 0 ? _a : {};
          const existingBlankNodes = (_b = existingPredicate.blankNodes) !== null && _b !== void 0 ? _b : [];
          const updatedBlankNodes = freeze(existingBlankNodes.concat(getBlankNodeId(value)));
          const updatedPredicate = freeze(Object.assign(Object.assign({}, existingPredicate), { blankNodes: updatedBlankNodes }));
          const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
          const updatedThing = freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
          return updatedThing;
      }
      throw new Error(`Term type [${value.termType}] is not supported by @inrupt/solid-client.`);
  }
  function addLiteralOfType(thing, property, value, type) {
      var _a, _b, _c;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const existingPredicate = (_a = thing.predicates[predicateIri]) !== null && _a !== void 0 ? _a : {};
      const existingLiterals = (_b = existingPredicate.literals) !== null && _b !== void 0 ? _b : {};
      const existingValuesOfType = (_c = existingLiterals[type]) !== null && _c !== void 0 ? _c : [];
      const updatedValuesOfType = freeze(existingValuesOfType.concat(value));
      const updatedLiterals = freeze(Object.assign(Object.assign({}, existingLiterals), { [type]: updatedValuesOfType }));
      const updatedPredicate = freeze(Object.assign(Object.assign({}, existingPredicate), { literals: updatedLiterals }));
      const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
      const updatedThing = freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
      return updatedThing;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  function removeAll(thing, property) {
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const newPredicates = Object.assign({}, thing.predicates);
      delete newPredicates[predicateIri];
      return freeze(Object.assign(Object.assign({}, thing), { predicates: freeze(newPredicates) }));
  }
  /**
   * Create a new Thing with the given URL removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove a URL value from.
   * @param property Property for which to remove the given URL value.
   * @param value URL to remove from `thing` for the given `Property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  const removeUrl = (thing, property, value) => {
      var _a, _b, _c;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      if (!isThing(value) && !internal_isValidUrl(value)) {
          throw new ValidValueUrlExpectedError(value);
      }
      const iriToRemove = isNamedNode$1(value)
          ? value.value
          : typeof value === "string"
              ? value
              : asIri(value);
      const updatedNamedNodes = freeze((_c = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.namedNodes) === null || _b === void 0 ? void 0 : _b.filter((namedNode) => namedNode.toLowerCase() !== iriToRemove.toLowerCase())) !== null && _c !== void 0 ? _c : []);
      const updatedPredicate = freeze(Object.assign(Object.assign({}, thing.predicates[predicateIri]), { namedNodes: updatedNamedNodes }));
      const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
      return freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
  };
  /** @hidden Alias of [[removeUrl]] for those who prefer IRI terminology. */
  const removeIri = removeUrl;
  /**
   * Create a new Thing with the given boolean removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove a boolean value from.
   * @param property Property for which to remove the given boolean value.
   * @param value Boolean to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  const removeBoolean = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return removeLiteralMatching(thing, property, xmlSchemaTypes.boolean, (foundBoolean) => deserializeBoolean(foundBoolean) === value);
  };
  /**
   * Create a new Thing with the given datetime removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove a datetime value from.
   * @param property Property for which to remove the given datetime value.
   * @param value Datetime to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  const removeDatetime = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return removeLiteralMatching(thing, property, xmlSchemaTypes.dateTime, (foundDatetime) => { var _a; return ((_a = deserializeDatetime(foundDatetime)) === null || _a === void 0 ? void 0 : _a.getTime()) === value.getTime(); });
  };
  /**
   * Create a new Thing with the given date removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove a date value from.
   * @param property Property for which to remove the given date value.
   * @param value Date to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   * @since 1.10.0
   */
  const removeDate = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return removeLiteralMatching(thing, property, xmlSchemaTypes.date, function (foundDate) {
          const deserializedDate = deserializeDate(foundDate);
          if (deserializedDate) {
              return (deserializedDate.getFullYear() === value.getFullYear() &&
                  deserializedDate.getMonth() === value.getMonth() &&
                  deserializedDate.getDate() === value.getDate());
          }
          else {
              return false;
          }
      });
  };
  /**
   * Create a new Thing with the given datetime removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove a datetime value from.
   * @param property Property for which to remove the given datetime value.
   * @param value Time to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   * @since 1.10.0
   */
  const removeTime = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return removeLiteralMatching(thing, property, xmlSchemaTypes.time, function (foundTime) {
          const deserializedTime = deserializeTime(foundTime);
          if (deserializedTime) {
              return (deserializedTime.hour === value.hour &&
                  deserializedTime.minute === value.minute &&
                  deserializedTime.second === value.second &&
                  deserializedTime.millisecond === value.millisecond &&
                  deserializedTime.timezoneHourOffset === value.timezoneHourOffset &&
                  deserializedTime.timezoneMinuteOffset === value.timezoneMinuteOffset);
          }
          else {
              return false;
          }
      });
  };
  /**
   * Create a new Thing with the given decimal removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove a decimal value from.
   * @param property Property for which to remove the given decimal value.
   * @param value Decimal to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  const removeDecimal = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return removeLiteralMatching(thing, property, xmlSchemaTypes.decimal, (foundDecimal) => deserializeDecimal(foundDecimal) === value);
  };
  /**
   * Create a new Thing with the given integer removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove an integer value from.
   * @param property Property for which to remove the given integer value.
   * @param value Integer to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  const removeInteger = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return removeLiteralMatching(thing, property, xmlSchemaTypes.integer, (foundInteger) => deserializeInteger(foundInteger) === value);
  };
  /**
   * Create a new Thing with the given localised string removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove a localised string value from.
   * @param property Property for which to remove the given localised string value.
   * @param value String to remove from `thing` for the given `property`.
   * @param locale Locale of the string to remove.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  function removeStringWithLocale(thing, property, value, locale) {
      var _a, _b;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const existingLangStrings = (_b = (_a = thing.predicates[predicateIri]) === null || _a === void 0 ? void 0 : _a.langStrings) !== null && _b !== void 0 ? _b : {};
      const matchingLocale = Object.keys(existingLangStrings).find((existingLocale) => normalizeLocale(existingLocale) === normalizeLocale(locale) &&
          Array.isArray(existingLangStrings[existingLocale]) &&
          existingLangStrings[existingLocale].length > 0);
      if (typeof matchingLocale !== "string") {
          // Nothing to remove.
          return thing;
      }
      const existingStringsInLocale = existingLangStrings[matchingLocale];
      const updatedStringsInLocale = freeze(existingStringsInLocale.filter((existingString) => existingString !== value));
      const updatedLangStrings = freeze(Object.assign(Object.assign({}, existingLangStrings), { [matchingLocale]: updatedStringsInLocale }));
      const updatedPredicate = freeze(Object.assign(Object.assign({}, thing.predicates[predicateIri]), { langStrings: updatedLangStrings }));
      const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
      return freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
  }
  /**
   * Create a new Thing with the given unlocalised string removed for the given Property.
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to remove an unlocalised string value from.
   * @param property Property for which to remove the given string value.
   * @param value String to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  const removeStringNoLocale = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return removeLiteralMatching(thing, property, xmlSchemaTypes.string, (foundString) => foundString === value);
  };
  /**
   * @ignore This should not be needed due to the other remove*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing Thing to remove a NamedNode value from.
   * @param property Property for which to remove the given NamedNode value.
   * @param value NamedNode to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  function removeNamedNode(thing, property, value) {
      return removeUrl(thing, property, value.value);
  }
  /**
   * @ignore This should not be needed due to the other remove*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing Thing to remove a Literal value from.
   * @param property Property for which to remove the given Literal value.
   * @param value Literal to remove from `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with the given value removed for the given Property.
   */
  function removeLiteral(thing, property, value) {
      var _a, _b, _c;
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const typeIri = value.datatype.value;
      if (typeIri === xmlSchemaTypes.langString) {
          return removeStringWithLocale(thing, property, value.value, value.language);
      }
      const predicateIri = internal_toIriString(property);
      const existingPredicateValues = (_a = thing.predicates[predicateIri]) !== null && _a !== void 0 ? _a : {};
      const existingLiterals = (_b = existingPredicateValues.literals) !== null && _b !== void 0 ? _b : {};
      const existingValuesOfType = (_c = existingLiterals[typeIri]) !== null && _c !== void 0 ? _c : [];
      const updatedValues = freeze(existingValuesOfType.filter((existingValue) => existingValue !== value.value));
      const updatedLiterals = freeze(Object.assign(Object.assign({}, existingLiterals), { [typeIri]: updatedValues }));
      const updatedPredicate = freeze(Object.assign(Object.assign({}, existingPredicateValues), { literals: updatedLiterals }));
      const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
      const updatedThing = freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
      return updatedThing;
  }
  /**
   * @param thing Thing to remove a Literal value from.
   * @param property Property for which to remove the given Literal value.
   * @param type Data type that the Literal should be stored as.
   * @param matcher Function that returns true if the given value is an equivalent serialisation of the value to remove. For example, when removing a `false` boolean, the matcher should return true for both "0" and "false".
   */
  function removeLiteralMatching(thing, property, type, matcher) {
      var _a, _b, _c;
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      const predicateIri = internal_toIriString(property);
      const existingPredicateValues = (_a = thing.predicates[predicateIri]) !== null && _a !== void 0 ? _a : {};
      const existingLiterals = (_b = existingPredicateValues.literals) !== null && _b !== void 0 ? _b : {};
      const existingValuesOfType = (_c = existingLiterals[type]) !== null && _c !== void 0 ? _c : [];
      const updatedValues = freeze(existingValuesOfType.filter((existingValue) => !matcher(existingValue)));
      const updatedLiterals = freeze(Object.assign(Object.assign({}, existingLiterals), { [type]: updatedValues }));
      const updatedPredicate = freeze(Object.assign(Object.assign({}, existingPredicateValues), { literals: updatedLiterals }));
      const updatedPredicates = freeze(Object.assign(Object.assign({}, thing.predicates), { [predicateIri]: updatedPredicate }));
      const updatedThing = freeze(Object.assign(Object.assign({}, thing), { predicates: updatedPredicates }));
      return updatedThing;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Create a new Thing with existing values replaced by the given URL for the given Property.
   *
   * To preserve existing values, see [[addUrl]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set a URL value on.
   * @param property Property for which to set the given URL value.
   * @param url URL to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  const setUrl = (thing, property, url) => {
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      if (!isThing(url) && !internal_isValidUrl(url)) {
          throw new ValidValueUrlExpectedError(url);
      }
      return addUrl(removeAll(thing, property), property, url);
  };
  /** @hidden Alias of [[setUrl]] for those who prefer IRI terminology. */
  const setIri = setUrl;
  /**
   * Create a new Thing with existing values replaced by the given boolean for the given Property.
   *
   * To preserve existing values, see [[addBoolean]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set a boolean value on.
   * @param property Property for which to set the given boolean value.
   * @param value Boolean to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  const setBoolean = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addBoolean(removeAll(thing, property), property, value);
  };
  /**
   * Create a new Thing with existing values replaced by the given datetime for the given Property.
   *
   * To preserve existing values, see [[addDatetime]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set an datetime value on.
   * @param property Property for which to set the given datetime value.
   * @param value Datetime to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  const setDatetime = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addDatetime(removeAll(thing, property), property, value);
  };
  /**
   * Create a new Thing with existing values replaced by the given date for the given Property.
   *
   * To preserve existing values, see [[addDate]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set an date value on.
   * @param property Property for which to set the given date value.
   * @param value Date to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   * @since 1.10.0
   */
  const setDate = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addDate(removeAll(thing, property), property, value);
  };
  /**
   * Create a new Thing with existing values replaced by the given time for the given Property.
   *
   * To preserve existing values, see [[addTime]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set an time value on.
   * @param property Property for which to set the given time value.
   * @param value time to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   * @since 1.10.0
   */
  const setTime = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addTime(removeAll(thing, property), property, value);
  };
  /**
   * Create a new Thing with existing values replaced by the given decimal for the given Property.
   *
   * To preserve existing values, see [[addDecimal]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set a decimal value on.
   * @param property Property for which to set the given decimal value.
   * @param value Decimal to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  const setDecimal = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addDecimal(removeAll(thing, property), property, value);
  };
  /**
   * Create a new Thing with existing values replaced by the given integer for the given Property.
   *
   * To preserve existing values, see [[addInteger]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set an integer value on.
   * @param property Property for which to set the given integer value.
   * @param value Integer to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  const setInteger = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addInteger(removeAll(thing, property), property, value);
  };
  /**
   * Create a new Thing with existing values replaced by the given localised string for the given Property.
   *
   * To preserve existing values, see [[addStringWithLocale]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set a localised string value on.
   * @param property Property for which to set the given localised string value.
   * @param value Localised string to set on `thing` for the given `property`.
   * @param locale Locale of the added string.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  function setStringWithLocale(thing, property, value, locale) {
      internal_throwIfNotThing(thing);
      return addStringWithLocale(removeAll(thing, property), property, value, locale);
  }
  /**
   * Create a new Thing with existing values replaced by the given unlocalised string for the given Property.
   *
   * To preserve existing values, see [[addStringNoLocale]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @param thing Thing to set an unlocalised string value on.
   * @param property Property for which to set the given unlocalised string value.
   * @param value Unlocalised string to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  const setStringNoLocale = (thing, property, value) => {
      internal_throwIfNotThing(thing);
      return addStringNoLocale(removeAll(thing, property), property, value);
  };
  /**
   * Create a new Thing with existing values replaced by the given Named Node for the given Property.
   *
   * This replaces existing values for the given Property. To preserve them, see [[addNamedNode]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @ignore This should not be needed due to the other set*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing The [[Thing]] to set a NamedNode on.
   * @param property Property for which to set the value.
   * @param value The NamedNode to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  function setNamedNode(thing, property, value) {
      internal_throwIfNotThing(thing);
      return addNamedNode(removeAll(thing, property), property, value);
  }
  /**
   * Create a new Thing with existing values replaced by the given Literal for the given Property.
   *
   * This replaces existing values for the given Property. To preserve them, see [[addLiteral]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @ignore This should not be needed due to the other set*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing The [[Thing]] to set a Literal on.
   * @param property Property for which to set the value.
   * @param value The Literal to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   */
  function setLiteral(thing, property, value) {
      internal_throwIfNotThing(thing);
      return addLiteral(removeAll(thing, property), property, value);
  }
  /**
   * Creates a new Thing with existing values replaced by the given Term for the given Property.
   *
   * This replaces existing values for the given Property. To preserve them, see [[addTerm]].
   *
   * The original `thing` is not modified; this function returns a cloned Thing with updated values.
   *
   * @ignore This should not be needed due to the other set*() functions. If you do find yourself needing it, please file a feature request for your use case.
   * @param thing The [[Thing]] to set a Term on.
   * @param property Property for which to set the value.
   * @param value The raw RDF/JS value to set on `thing` for the given `property`.
   * @returns A new Thing equal to the input Thing with existing values replaced by the given value for the given Property.
   * @since 0.3.0
   */
  function setTerm(thing, property, value) {
      internal_throwIfNotThing(thing);
      if (!internal_isValidUrl(property)) {
          throw new ValidPropertyUrlExpectedError(property);
      }
      return addTerm(removeAll(thing, property), property, value);
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Create or modify a [[Thing]], setting multiple properties in a single expresssion.
   *
   * For example, you can create a new Thing and initialise several properties as follows:
   *
   *     const me = buildThing()
   *       .addUrl(rdf.type, schema.Person)
   *       .addStringNoLocale(schema.givenName, "Vincent")
   *       .build();
   *
   * Take note of the final call to `.build()` to obtain the actual Thing.
   *
   * @param init Optionally pass an existing [[Thing]] to modify the properties of. If left empty, `buildThing` will initialise a new Thing.
   * @returns a [[ThingBuilder]], a Fluent API that allows you to set multiple properties in a single expression.
   * @since 1.9.0
   */
  function buildThing(init = createThing()) {
      let thing = isThing(init) ? init : createThing(init);
      function getAdder(adder) {
          return (property, value) => {
              thing = adder(thing, property, value);
              return builder;
          };
      }
      function getSetter(setter) {
          return (property, value) => {
              thing = setter(thing, property, value);
              return builder;
          };
      }
      function getRemover(remover) {
          return (property, value) => {
              thing = remover(thing, property, value);
              return builder;
          };
      }
      const builder = {
          build: () => thing,
          addUrl: getAdder(addUrl),
          addIri: getAdder(addIri),
          addBoolean: getAdder(addBoolean),
          addDatetime: getAdder(addDatetime),
          addDate: getAdder(addDate),
          addTime: getAdder(addTime),
          addDecimal: getAdder(addDecimal),
          addInteger: getAdder(addInteger),
          addStringNoLocale: getAdder(addStringNoLocale),
          addStringWithLocale: (property, value, locale) => {
              thing = addStringWithLocale(thing, property, value, locale);
              return builder;
          },
          addNamedNode: getAdder(addNamedNode),
          addLiteral: getAdder(addLiteral),
          addTerm: getAdder(addTerm),
          setUrl: getSetter(setUrl),
          setIri: getSetter(setIri),
          setBoolean: getSetter(setBoolean),
          setDatetime: getSetter(setDatetime),
          setDate: getSetter(setDate),
          setTime: getSetter(setTime),
          setDecimal: getSetter(setDecimal),
          setInteger: getSetter(setInteger),
          setStringNoLocale: getSetter(setStringNoLocale),
          setStringWithLocale: (property, value, locale) => {
              thing = setStringWithLocale(thing, property, value, locale);
              return builder;
          },
          setNamedNode: getSetter(setNamedNode),
          setLiteral: getSetter(setLiteral),
          setTerm: getSetter(setTerm),
          removeAll: (property) => {
              thing = removeAll(thing, property);
              return builder;
          },
          removeUrl: getRemover(removeUrl),
          removeIri: getRemover(removeIri),
          removeBoolean: getRemover(removeBoolean),
          removeDatetime: getRemover(removeDatetime),
          removeDate: getRemover(removeDate),
          removeTime: getRemover(removeTime),
          removeDecimal: getRemover(removeDecimal),
          removeInteger: getRemover(removeInteger),
          removeStringNoLocale: getRemover(removeStringNoLocale),
          removeStringWithLocale: (property, value, locale) => buildThing(removeStringWithLocale(thing, property, value, locale)),
          removeNamedNode: getRemover(removeNamedNode),
          removeLiteral: getRemover(removeLiteral),
      };
      return builder;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Function for use in unit tests to mock a [[Thing]] with a given URL.
   *
   * Warning: do not use this function in actual production code.
   * This function initialises a new empty Thing and sets its URL to a given URL.
   * This is useful to mock a Thing in tests of code that call e.g.
   * [[asUrl]].
   *
   * @param url The URL that the mocked Thing pretends identifies it.
   * @returns A new Thing, pretending to be identified by the given URL.
   * @since 0.2.0
   */
  function mockThingFrom(url) {
      const iri = internal_toIriString(url);
      const thing = {
          type: "Subject",
          predicates: {},
          url: iri,
      };
      return thing;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * @param linkedAccessResource A Resource exposed via the Link header of another Resource with rel="acl".
   * @returns Whether that Resource is an ACP ACR or not (in which case it's likely a WAC ACL).
   */
  function isAcr(linkedAccessResource) {
      const relTypeLinks = getLinkedResourceUrlAll(linkedAccessResource)["type"];
      return (Array.isArray(relTypeLinks) &&
          relTypeLinks.includes(acp.AccessControlResource));
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * This (currently internal) function fetches the ACL indicated in the [[WithServerResourceInfo]]
   * attached to a resource.
   *
   * @internal
   * @param resourceInfo The Resource info with the ACL URL
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch#Parameters).
   */
  async function internal_fetchAcl(resourceInfo, options = internal_defaultFetchOptions) {
      if (!hasAccessibleAcl(resourceInfo)) {
          return {
              resourceAcl: null,
              fallbackAcl: null,
          };
      }
      try {
          const resourceAcl = await internal_fetchResourceAcl(resourceInfo, options);
          const acl = resourceAcl === null
              ? {
                  resourceAcl: null,
                  fallbackAcl: await internal_fetchFallbackAcl(resourceInfo, options),
              }
              : { resourceAcl: resourceAcl, fallbackAcl: null };
          return acl;
      }
      catch (e) {
          /* istanbul ignore else: fetchResourceAcl swallows all non-AclIsAcrErrors */
          if (e instanceof AclIsAcrError) {
              return {
                  resourceAcl: null,
                  fallbackAcl: null,
              };
          }
          /* istanbul ignore next: fetchResourceAcl swallows all non-AclIsAcrErrors */
          throw e;
      }
  }
  /** @internal */
  async function internal_fetchResourceAcl(dataset, options = internal_defaultFetchOptions) {
      if (!hasAccessibleAcl(dataset)) {
          return null;
      }
      try {
          const aclSolidDataset = await getSolidDataset(dataset.internal_resourceInfo.aclUrl, options);
          if (isAcr(aclSolidDataset)) {
              throw new AclIsAcrError(dataset, aclSolidDataset);
          }
          return freeze(Object.assign(Object.assign({}, aclSolidDataset), { internal_accessTo: getSourceUrl(dataset) }));
      }
      catch (e) {
          if (e instanceof AclIsAcrError) {
              throw e;
          }
          // Since a Solid server adds a `Link` header to an ACL even if that ACL does not exist,
          // failure to fetch the ACL is expected to happen - we just return `null` and let callers deal
          // with it.
          return null;
      }
  }
  /** @internal */
  async function internal_fetchFallbackAcl(resource, options = internal_defaultFetchOptions) {
      const resourceUrl = new URL(getSourceUrl(resource));
      const resourcePath = resourceUrl.pathname;
      // Note: we're currently assuming that the Origin is the root of the Pod. However, it is not yet
      //       set in stone that that will always be the case. We might need to check the Container's
      //       metadata at some point in time to check whether it is actually the root of the Pod.
      //       See: https://github.com/solid/specification/issues/153#issuecomment-624630022
      if (resourcePath === "/") {
          // We're already at the root, so there's no Container we can retrieve:
          return null;
      }
      const containerPath = internal_getContainerPath(resourcePath);
      const containerIri = new URL(containerPath, resourceUrl.origin).href;
      const containerInfo = await getResourceInfo(containerIri, options);
      if (!hasAccessibleAcl(containerInfo)) {
          // If the current user does not have access to this Container's ACL,
          // we cannot determine whether its ACL is the one that applies. Thus, return null:
          return null;
      }
      const containerAcl = await internal_fetchResourceAcl(containerInfo, options);
      if (containerAcl === null) {
          return internal_fetchFallbackAcl(containerInfo, options);
      }
      return containerAcl;
  }
  /**
   * Given the path to a Resource, get the URL of the Container one level up in the hierarchy.
   * @param resourcePath The path of the Resource of which we need to determine the Container's path.
   * @hidden For internal use only.
   */
  function internal_getContainerPath(resourcePath) {
      const resourcePathWithoutTrailingSlash = resourcePath.substring(resourcePath.length - 1) === "/"
          ? resourcePath.substring(0, resourcePath.length - 1)
          : resourcePath;
      const containerPath = resourcePath.substring(0, resourcePathWithoutTrailingSlash.lastIndexOf("/")) + "/";
      return containerPath;
  }
  /** @internal */
  function internal_getAclRules(aclDataset) {
      const things = getThingAll(aclDataset);
      return things.filter(isAclRule);
  }
  function isAclRule(thing) {
      return getIriAll(thing, rdf$2.type).includes(acl.Authorization);
  }
  /** @internal */
  function internal_getResourceAclRulesForResource(aclRules, resource) {
      return aclRules.filter((rule) => appliesToResource(rule, resource));
  }
  function appliesToResource(aclRule, resource) {
      return getIriAll(aclRule, acl.accessTo).includes(resource);
  }
  /** @internal */
  function internal_getDefaultAclRulesForResource(aclRules, resource) {
      return aclRules.filter((rule) => isDefaultForResource(rule, resource));
  }
  function isDefaultForResource(aclRule, resource) {
      return (getIriAll(aclRule, acl.default).includes(resource) ||
          getIriAll(aclRule, acl.defaultForNew).includes(resource));
  }
  /** @internal */
  function internal_getAccess(rule) {
      const ruleAccessModes = getIriAll(rule, acl.mode);
      const writeAccess = ruleAccessModes.includes(internal_accessModeIriStrings.write);
      return writeAccess
          ? {
              read: ruleAccessModes.includes(internal_accessModeIriStrings.read),
              append: true,
              write: true,
              control: ruleAccessModes.includes(internal_accessModeIriStrings.control),
          }
          : {
              read: ruleAccessModes.includes(internal_accessModeIriStrings.read),
              append: ruleAccessModes.includes(internal_accessModeIriStrings.append),
              write: false,
              control: ruleAccessModes.includes(internal_accessModeIriStrings.control),
          };
  }
  /** @internal */
  function internal_combineAccessModes(modes) {
      return modes.reduce((accumulator, current) => {
          const writeAccess = accumulator.write || current.write;
          return writeAccess
              ? {
                  read: accumulator.read || current.read,
                  append: true,
                  write: true,
                  control: accumulator.control || current.control,
              }
              : {
                  read: accumulator.read || current.read,
                  append: accumulator.append || current.append,
                  write: false,
                  control: accumulator.control || current.control,
              };
      }, { read: false, append: false, write: false, control: false });
  }
  /** @internal */
  function internal_removeEmptyAclRules(aclDataset) {
      const aclRules = internal_getAclRules(aclDataset);
      const aclRulesToRemove = aclRules.filter(isEmptyAclRule);
      // Is this too clever? It iterates over aclRulesToRemove, one by one removing them from aclDataset.
      const updatedAclDataset = aclRulesToRemove.reduce(removeThing, aclDataset);
      return updatedAclDataset;
  }
  function isEmptyAclRule(aclRule) {
      // If there are Quads in there unrelated to Access Control,
      // this is not an empty ACL rule that can be deleted:
      if (subjectToRdfJsQuads(aclRule.predicates, DataFactory$1.namedNode(aclRule.url), DataFactory$1.defaultGraph()).some((quad) => !isAclQuad(quad))) {
          return false;
      }
      // If the rule does not apply to any Resource, it is no longer working:
      if (getIri(aclRule, acl.accessTo) === null &&
          getIri(aclRule, acl.default) === null &&
          getIri(aclRule, acl.defaultForNew) === null) {
          return true;
      }
      // If the rule does not specify Access Modes, it is no longer working:
      if (getIri(aclRule, acl.mode) === null) {
          return true;
      }
      // If the rule does not specify whom it applies to, it is no longer working:
      if (getIri(aclRule, acl.agent) === null &&
          getIri(aclRule, acl.agentGroup) === null &&
          getIri(aclRule, acl.agentClass) === null) {
          return true;
      }
      return false;
  }
  function isAclQuad(quad) {
      const predicate = quad.predicate;
      const object = quad.object;
      if (predicate.equals(DataFactory$1.namedNode(rdf$2.type)) &&
          object.equals(DataFactory$1.namedNode(acl.Authorization))) {
          return true;
      }
      if (predicate.equals(DataFactory$1.namedNode(acl.accessTo)) ||
          predicate.equals(DataFactory$1.namedNode(acl.default)) ||
          predicate.equals(DataFactory$1.namedNode(acl.defaultForNew))) {
          return true;
      }
      if (predicate.equals(DataFactory$1.namedNode(acl.mode)) &&
          Object.values(internal_accessModeIriStrings).some((mode) => object.equals(DataFactory$1.namedNode(mode)))) {
          return true;
      }
      if (predicate.equals(DataFactory$1.namedNode(acl.agent)) ||
          predicate.equals(DataFactory$1.namedNode(acl.agentGroup)) ||
          predicate.equals(DataFactory$1.namedNode(acl.agentClass))) {
          return true;
      }
      if (predicate.equals(DataFactory$1.namedNode(acl.origin))) {
          return true;
      }
      return false;
  }
  /**
   * IRIs of potential Access Modes
   * @internal
   */
  const internal_accessModeIriStrings = {
      read: "http://www.w3.org/ns/auth/acl#Read",
      append: "http://www.w3.org/ns/auth/acl#Append",
      write: "http://www.w3.org/ns/auth/acl#Write",
      control: "http://www.w3.org/ns/auth/acl#Control",
  };
  /** @internal
   * This function finds, among a set of ACL rules, the ones granting access to a given entity (the target)
   * and identifying it with a specific property (`acl:agent` or `acl:agentGroup`).
   * @param aclRules The set of rules to filter
   * @param targetIri The IRI of the target
   * @param targetType The property linking the rule to the target
   */
  function internal_getAclRulesForIri(aclRules, targetIri, targetType) {
      return aclRules.filter((rule) => getIriAll(rule, targetType).includes(targetIri));
  }
  /** @internal
   * This function transforms a given set of rules into a map associating the IRIs
   * of the entities to which permissions are granted by these rules, and the permissions
   * granted to them. Additionally, it filters these entities based on the predicate
   * that refers to them in the rule.
   */
  function internal_getAccessByIri(aclRules, targetType) {
      const targetIriAccess = {};
      aclRules.forEach((rule) => {
          const ruleTargetIri = getIriAll(rule, targetType);
          const access = internal_getAccess(rule);
          // A rule might apply to multiple agents. If multiple rules apply to the same agent, the Access
          // Modes granted by those rules should be combined:
          ruleTargetIri.forEach((targetIri) => {
              targetIriAccess[targetIri] =
                  typeof targetIriAccess[targetIri] === "undefined"
                      ? access
                      : internal_combineAccessModes([targetIriAccess[targetIri], access]);
          });
      });
      return targetIriAccess;
  }
  /**
   * Initialises a new ACL Rule that grants some access - but does not yet specify to whom.
   *
   * @hidden This is an internal utility function that should not be used directly by downstreams.
   * @param access Access mode that this Rule will grant
   */
  function internal_initialiseAclRule(access) {
      let newRule = createThing();
      newRule = setIri(newRule, rdf$2.type, acl.Authorization);
      if (access.read) {
          newRule = addIri(newRule, acl.mode, internal_accessModeIriStrings.read);
      }
      if (access.append && !access.write) {
          newRule = addIri(newRule, acl.mode, internal_accessModeIriStrings.append);
      }
      if (access.write) {
          newRule = addIri(newRule, acl.mode, internal_accessModeIriStrings.write);
      }
      if (access.control) {
          newRule = addIri(newRule, acl.mode, internal_accessModeIriStrings.control);
      }
      return newRule;
  }
  /**
   * Creates a new ACL Rule with the same ACL values as the input ACL Rule, but having a different IRI.
   *
   * Note that non-ACL values will not be copied over.
   *
   * @hidden This is an internal utility function that should not be used directly by downstreams.
   * @param sourceRule ACL rule to duplicate.
   */
  function internal_duplicateAclRule(sourceRule) {
      let targetRule = createThing();
      targetRule = setIri(targetRule, rdf$2.type, acl.Authorization);
      function copyIris(inputRule, outputRule, predicate) {
          return getIriAll(inputRule, predicate).reduce((outputRule, iriTarget) => addIri(outputRule, predicate, iriTarget), outputRule);
      }
      targetRule = copyIris(sourceRule, targetRule, acl.accessTo);
      targetRule = copyIris(sourceRule, targetRule, acl.default);
      targetRule = copyIris(sourceRule, targetRule, acl.defaultForNew);
      targetRule = copyIris(sourceRule, targetRule, acl.agent);
      targetRule = copyIris(sourceRule, targetRule, acl.agentGroup);
      targetRule = copyIris(sourceRule, targetRule, acl.agentClass);
      targetRule = copyIris(sourceRule, targetRule, acl.origin);
      targetRule = copyIris(sourceRule, targetRule, acl.mode);
      return targetRule;
  }
  function internal_setAcl(resource, acl) {
      return Object.assign(internal_cloneResource(resource), { internal_acl: acl });
  }
  const supportedActorPredicates = [
      acl.agent,
      acl.agentClass,
      acl.agentGroup,
      acl.origin,
  ];
  /**
   * Given an ACL Rule, returns two new ACL Rules that cover all the input Rule's use cases,
   * except for giving the given Actor access to the given Resource.
   *
   * @param rule The ACL Rule that should no longer apply for a given Actor to a given Resource.
   * @param actor The Actor that should be removed from the Rule for the given Resource.
   * @param resourceIri The Resource to which the Rule should no longer apply for the given Actor.
   * @returns A tuple with the original ACL Rule without the given Actor, and a new ACL Rule for the given Actor for the remaining Resources, respectively.
   */
  function internal_removeActorFromRule(rule, actor, actorPredicate, resourceIri, ruleType) {
      // If the existing Rule does not apply to the given Actor, we don't need to split up.
      // Without this check, we'd be creating a new rule for the given Actor (ruleForOtherTargets)
      // that would give it access it does not currently have:
      if (!getIriAll(rule, actorPredicate).includes(actor)) {
          const emptyRule = internal_initialiseAclRule({
              read: false,
              append: false,
              write: false,
              control: false,
          });
          return [rule, emptyRule];
      }
      // The existing rule will keep applying to Actors other than the given one:
      const ruleWithoutActor = removeIri(rule, actorPredicate, actor);
      // The actor might have been given other access in the existing rule, so duplicate it...
      let ruleForOtherTargets = internal_duplicateAclRule(rule);
      // ...but remove access to the original Resource...
      ruleForOtherTargets = removeIri(ruleForOtherTargets, ruleType === "resource" ? acl.accessTo : acl.default, resourceIri);
      // Prevents the legacy predicate 'acl:defaultForNew' to lead to privilege escalation
      if (ruleType === "default") {
          ruleForOtherTargets = removeIri(ruleForOtherTargets, acl.defaultForNew, resourceIri);
      }
      // ...and only apply the new Rule to the given Actor (because the existing Rule covers the others):
      ruleForOtherTargets = setIri(ruleForOtherTargets, actorPredicate, actor);
      supportedActorPredicates
          .filter((predicate) => predicate !== actorPredicate)
          .forEach((predicate) => {
          ruleForOtherTargets = removeAll(ruleForOtherTargets, predicate);
      });
      return [ruleWithoutActor, ruleForOtherTargets];
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   * Modifies the resource ACL (Access Control List) to set the Access Modes for the given Agent.
   * Specifically, the function returns a new resource ACL initialised with the given ACL and
   * new rules for the Actor's access.
   *
   * If rules for Actor's access already exist in the given ACL, in the returned ACL,
   * they are replaced by the new rules.
   *
   * This function does not modify:
   *
   * - Access Modes granted indirectly to Actors through other ACL rules, e.g., public or group-specific permissions.
   * - Access Modes granted to Actors for the child Resources if the associated Resource is a Container.
   * - The original ACL.
   *
   * @param aclDataset The SolidDataset that contains Access-Control List rules.
   * @param actor The Actor to grant specific Access Modes.
   * @param access The Access Modes to grant to the Actor for the Resource.
   * @returns A new resource ACL initialised with the given `aclDataset` and `access` for the `agent`.
   */
  function internal_setActorAccess$2(aclDataset, access, actorPredicate, accessType, actor) {
      // First make sure that none of the pre-existing rules in the given ACL SolidDataset
      // give the Agent access to the Resource:
      let filteredAcl = aclDataset;
      getThingAll(aclDataset).forEach((aclRule) => {
          // Obtain both the Rule that no longer includes the given Actor,
          // and a new Rule that includes all ACL Quads
          // that do not pertain to the given Actor-Resource combination.
          // Note that usually, the latter will no longer include any meaningful statements;
          // we'll clean them up afterwards.
          const [filteredRule, remainingRule] = internal_removeActorFromRule(aclRule, actor, actorPredicate, aclDataset.internal_accessTo, accessType);
          filteredAcl = setThing(filteredAcl, filteredRule);
          filteredAcl = setThing(filteredAcl, remainingRule);
      });
      // Create a new Rule that only grants the given Actor the given Access Modes:
      let newRule = internal_initialiseAclRule(access);
      newRule = setIri(newRule, accessType === "resource" ? acl.accessTo : acl.default, aclDataset.internal_accessTo);
      newRule = setIri(newRule, actorPredicate, actor);
      const updatedAcl = setThing(filteredAcl, newRule);
      // Remove any remaining Rules that do not contain any meaningful statements:
      return internal_removeEmptyAclRules(updatedAcl);
  }
  function internal_setResourceAcl(resource, acl) {
      const newAcl = {
          resourceAcl: acl,
          fallbackAcl: null,
      };
      return internal_setAcl(resource, newAcl);
  }
  function internal_getResourceAcl(resource) {
      return resource.internal_acl.resourceAcl;
  }
  /**
   * This error indicates that, if we're following a Link with rel="acl",
   * it does not result in a WAC ACL, but in an ACP ACR.
   */
  class AclIsAcrError extends Error {
      constructor(sourceResource, aclResource) {
          super(`[${getSourceIri(sourceResource)}] is governed by Access Control Policies in [${getSourceIri(aclResource)}] rather than by Web Access Control.`);
      }
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Verify whether a given SolidDataset was fetched together with its Access Control List.
   *
   * @param dataset A [[SolidDataset]] that may have its ACLs attached.
   * @returns True if `dataset` was fetched together with its ACLs.
   */
  function hasAcl(dataset) {
      const potentialAcl = dataset;
      return typeof potentialAcl.internal_acl === "object";
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Verifies whether the given Resource has a resource ACL (Access Control List) attached.
   *
   * The [[hasResourceAcl]] function checks that:
   * - a given Resource has a resource ACL attached, and
   * - the user calling [[hasResourceAcl]] has Control access to the Resource.
   *
   * To retrieve a Resource with its ACLs, see [[getSolidDatasetWithAcl]].
   *
   * @param resource A Resource that might have an ACL attached.
   * @returns `true` if the Resource has a resource ACL attached that is accessible by the user.
   */
  function hasResourceAcl(resource) {
      return (resource.internal_acl.resourceAcl !== null &&
          getSourceUrl(resource) ===
              resource.internal_acl.resourceAcl.internal_accessTo &&
          resource.internal_resourceInfo.aclUrl ===
              getSourceUrl(resource.internal_acl.resourceAcl));
  }
  /**
   * Experimental: fetch a SolidDataset and its associated Access Control List.
   *
   * This is an experimental function that fetches both a Resource, the linked ACL Resource (if
   * available), and the ACL that applies to it if the linked ACL Resource is not available. This can
   * result in many HTTP requests being executed, in lieu of the Solid spec mandating servers to
   * provide this info in a single request. Therefore, and because this function is still
   * experimental, prefer [[getSolidDataset]] instead.
   *
   * If the Resource does not advertise the ACL Resource (because the authenticated user does not have
   * access to it), the `acl` property in the returned value will be null. `acl.resourceAcl` will be
   * undefined if the Resource's linked ACL Resource could not be fetched (because it does not exist),
   * and `acl.fallbackAcl` will be null if the applicable Container's ACL is not accessible to the
   * authenticated user.
   *
   * @param url URL of the SolidDataset to fetch.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A SolidDataset and the ACLs that apply to it, if available to the authenticated user.
   */
  async function getSolidDatasetWithAcl(url, options = internal_defaultFetchOptions) {
      const solidDataset = await getSolidDataset(url, options);
      const acl = await internal_fetchAcl(solidDataset, options);
      return internal_setAcl(solidDataset, acl);
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Retrieves a file, its resource ACL (Access Control List) if available,
   * and its fallback ACL from a URL and returns them as a blob.
   *
   * If the user calling the function does not have access to the file's resource ACL,
   * [[hasAccessibleAcl]] on the returned blob returns false.
   * If the user has access to the file's resource ACL but the resource ACL does not exist,
   * [[getResourceAcl]] on the returned blob returns null.
   * If the fallback ACL is inaccessible by the user,
   * [[getFallbackAcl]] on the returned blob returns null.
   *
   * ```{tip}
   * To retrieve the ACLs, the function results in multiple HTTP requests rather than a single
   * request as mandated by the Solid spec. As such, prefer [[getFile]] instead if you do not need the ACL.
   * ```
   *
   * @param url The URL of the fetched file
   * @param options Fetching options: a custom fetcher and/or headers.
   * @returns A file and its ACLs, if available to the authenticated user, as a blob.
   * @since 0.2.0
   */
  async function getFileWithAcl(input, options = internal_defaultFetchOptions) {
      const file = await getFile(input, options);
      const acl = await internal_fetchAcl(file, options);
      return internal_setAcl(file, acl);
  }
  /**
   * Experimental: fetch a Resource's metadata and its associated Access Control List.
   *
   * This is an experimental function that fetches both a Resource's metadata, the linked ACL Resource (if
   * available), and the ACL that applies to it if the linked ACL Resource is not available (if accessible). This can
   * result in many HTTP requests being executed, in lieu of the Solid spec mandating servers to
   * provide this info in a single request.
   *
   * If the Resource's linked ACL Resource could not be fetched (because it does not exist, or because
   * the authenticated user does not have access to it), `acl.resourceAcl` will be `null`. If the
   * applicable Container's ACL is not accessible to the authenticated user, `acl.fallbackAcl` will be
   * `null`.
   *
   * @param url URL of the SolidDataset to fetch.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A Resource's metadata and the ACLs that apply to the Resource, if available to the authenticated user.
   */
  async function getResourceInfoWithAcl(url, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfo(url, options);
      const acl = await internal_fetchAcl(resourceInfo, options);
      return internal_setAcl(resourceInfo, acl);
  }
  function getResourceAcl(resource) {
      if (!hasResourceAcl(resource)) {
          return null;
      }
      return resource.internal_acl.resourceAcl;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Verifies whether the given Resource has a fallback ACL (Access Control List) attached.
   *
   * A fallback ACL for a Resource is inherited from the Resource's parent Container
   * (or another of its ancestor Containers) and applies if the Resource does
   * not have its own resource ACL.
   *
   * The [[hasFallbackAcl]] function checks that:
   * - a given Resource has a fallback ACL attached, and
   * - the user calling [[hasFallbackAcl]] has Control access to the Container
   * from which the Resource inherits its ACL.
   *
   * To retrieve a Resource with its ACLs, see [[getSolidDatasetWithAcl]].
   *
   * @param resource A [[SolidDataset]] that might have a fallback ACL attached.
   *
   * @returns `true` if the Resource has a fallback ACL attached that is accessible to the user.
   */
  function hasFallbackAcl(resource) {
      return resource.internal_acl.fallbackAcl !== null;
  }
  function getFallbackAcl(dataset) {
      if (!hasFallbackAcl(dataset)) {
          return null;
      }
      return dataset.internal_acl.fallbackAcl;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Creates an empty resource ACL (Access Control List) for a given Resource.
   *
   * @param targetResource A Resource that does not have its own ACL yet (see [[hasResourceAcl]]).
   * @returns An empty resource ACL for the given Resource.
   */
  function createAcl(targetResource) {
      const emptyResourceAcl = freeze(Object.assign(Object.assign({}, createSolidDataset()), { internal_accessTo: getSourceUrl(targetResource), internal_resourceInfo: {
              sourceIri: targetResource.internal_resourceInfo.aclUrl,
              isRawData: false,
              linkedResources: {},
          } }));
      return emptyResourceAcl;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Creates a resource ACL (Access Control List), initialised from the fallback ACL
   * inherited from the given Resource's Container (or another of its ancestor Containers).
   * That is, the new ACL has the same rules/entries as the fallback ACL that currently
   * applies to the Resource.
   *
   * @param resource A Resource without its own resource ACL (see [[hasResourceAcl]]) but with an accessible fallback ACL (see [[hasFallbackAcl]]).
   * @returns A resource ACL initialised with the rules/entries from the Resource's fallback ACL.
   */
  function createAclFromFallbackAcl(resource) {
      const emptyResourceAcl = createAcl(resource);
      const fallbackAclRules = internal_getAclRules(resource.internal_acl.fallbackAcl);
      const defaultAclRules = internal_getDefaultAclRulesForResource(fallbackAclRules, resource.internal_acl.fallbackAcl.internal_accessTo);
      const newAclRules = defaultAclRules.map((rule) => {
          rule = removeAll(rule, acl.default);
          rule = removeAll(rule, acl.defaultForNew);
          rule = setIri(rule, acl.accessTo, getSourceUrl(resource));
          rule = setIri(rule, acl.default, getSourceUrl(resource));
          return rule;
      });
      // Iterate over every ACL Rule we want to import, inserting them into `emptyResourceAcl` one by one:
      const initialisedResourceAcl = newAclRules.reduce(setThing, emptyResourceAcl);
      return initialisedResourceAcl;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Saves the resource ACL for a Resource.
   *
   * @param resource The Resource to which the given resource ACL applies.
   * @param resourceAcl An [[AclDataset]] whose ACL Rules will apply to `resource`.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   */
  async function saveAclFor(resource, resourceAcl, options = internal_defaultFetchOptions) {
      if (!hasAccessibleAcl(resource)) {
          throw new Error(`Could not determine the location of the ACL for the Resource at [${getSourceUrl(resource)}]; possibly the current user does not have Control access to that Resource. Try calling \`hasAccessibleAcl()\` before calling \`saveAclFor()\`.`);
      }
      const savedDataset = await saveSolidDatasetAt(resource.internal_resourceInfo.aclUrl, resourceAcl, options);
      const savedAclDataset = Object.assign(Object.assign({}, savedDataset), { internal_accessTo: getSourceUrl(resource) });
      return savedAclDataset;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes the resource ACL (Access Control List) from a Resource.
   *
   * Once the resource ACL is removed from the Resource, the Resource relies on the
   * fallback ACL inherited from the Resource's parent Container (or another of its ancestor Containers).
   *
   * @param resource The Resource for which you want to delete the ACL.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   */
  async function deleteAclFor(resource, options = internal_defaultFetchOptions) {
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const response = await config.fetch(resource.internal_resourceInfo.aclUrl, {
          method: "DELETE",
      });
      if (!response.ok) {
          throw new Error(`Deleting the ACL of the Resource at [${getSourceUrl(resource)}] failed: [${response.status}] [${response.statusText}].`);
      }
      const storedResource = Object.assign(internal_cloneResource(resource), {
          acl: {
              resourceAcl: null,
          },
      });
      return storedResource;
  }
  /**
   * Given a [[SolidDataset]], verify whether its Access Control List is accessible to the current user.
   *
   * This should generally only be true for SolidDatasets fetched by
   * [[getSolidDatasetWithAcl]].
   *
   * Please note that the Web Access Control specification is not yet finalised, and hence, this
   * function is still experimental and can change in a non-major release.
   *
   * @param dataset A [[SolidDataset]].
   * @returns Whether the given `dataset` has a an ACL that is accessible to the current user.
   */
  function hasAccessibleAcl(dataset) {
      return typeof dataset.internal_resourceInfo.aclUrl === "string";
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns an Agent's explicitly-granted Access Modes for the given Resource.
   *
   * The function does not return Access Modes granted indirectly to the Agent through other
   * ACL rules, e.g., public or group-specific permissions.
   *
   * @param resourceInfo Information about the Resource to which the given Agent may have been granted access.
   * @param agent WebID of the Agent for which to retrieve what access it has to the Resource.
   * @returns Access Modes that have been explicitly granted to the Agent for the given Resource, or `null` if it could not be determined (e.g. because the current user does not have Control access to a given Resource or its Container).
   */
  function getAgentAccess$3(resourceInfo, agent) {
      if (hasResourceAcl(resourceInfo)) {
          return getAgentResourceAccess(resourceInfo.internal_acl.resourceAcl, agent);
      }
      if (hasFallbackAcl(resourceInfo)) {
          return getAgentDefaultAccess(resourceInfo.internal_acl.fallbackAcl, agent);
      }
      return null;
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   * Returns all explicitly-granted Access Modes per Agent for the given Resource.
   *
   * The function does not return Access Modes granted indirectly to Agents through other
   * ACL rules, e.g., public or group-specific permissions.
   *
   * @param resourceInfo Information about the Resource to which Agents may have been granted access.
   * @returns Access Modes per Agent that have been explicitly granted for the given Resource, or `null` if it could not be determined (e.g. because the current user does not have Control access to a given Resource or its Container).
   */
  function getAgentAccessAll$3(resourceInfo) {
      if (hasResourceAcl(resourceInfo)) {
          const resourceAcl = getResourceAcl(resourceInfo);
          return getAgentResourceAccessAll(resourceAcl);
      }
      if (hasFallbackAcl(resourceInfo)) {
          const fallbackAcl = getFallbackAcl(resourceInfo);
          return getAgentDefaultAccessAll(fallbackAcl);
      }
      return null;
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the Access Modes explicitly granted to an Agent for the Resource
   * associated with an ACL (Access ControlList).
   *
   * The function does not return:
   *
   * - Access Modes granted indirectly to the Agent through other ACL rules, e.g., public or group-specific permissions.
   * - Access Modes granted to the Agent for the child Resources if the associated Resource is a Container (see [[getAgentDefaultAccess]] instead).
   *
   * @param aclDataset The SolidDataset that contains ACL rules.
   * @param agent WebID of the Agent for which to retrieve what access it has to the Resource.
   * @returns Access Modes that have been explicitly granted to an Agent for the Resource associated with an ACL SolidDataset.
   */
  function getAgentResourceAccess(aclDataset, agent) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getResourceAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const agentResourceRules = getAgentAclRulesForAgent(resourceRules, agent);
      const agentAccessModes = agentResourceRules.map(internal_getAccess);
      return internal_combineAccessModes(agentAccessModes);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the explicitly granted Access Modes per Agent for the Resource associated
   * with an ACL (Access Control List).
   *
   * The function does not return:
   *
   * - Access Modes granted indirectly to Agents through other ACL rules, e.g., public or group-specific permissions.
   * - Access Modes granted to Agents for the child Resources if the associated Resource is a Container.
   *
   * @param aclDataset The SolidDataset that contains ACL rules.
   * @returns Access Modes per Agent that have been explicitly granted for the Resource associated with an ACL SolidDataset.
   */
  function getAgentResourceAccessAll(aclDataset) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getResourceAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const agentResourceRules = getAgentAclRules(resourceRules);
      return getAccessByAgent(agentResourceRules);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   * Modifies the resource ACL (Access Control List) to set the Access Modes for the given Agent.
   * Specifically, the function returns a new resource ACL initialised with the given ACL and
   * new rules for the Agent's access.
   *
   * If rules for Agent's access already exist in the given ACL, in the returned ACL,
   * they are replaced by the new rules.
   *
   * This function does not modify:
   *
   * - Access Modes granted indirectly to Agents through other ACL rules, e.g., public or group-specific permissions.
   * - Access Modes granted to Agents for the child Resources if the associated Resource is a Container.
   * - The original ACL.
   *
   * @param aclDataset The SolidDataset that contains Access-Control List rules.
   * @param agent The Agent to grant specific Access Modes.
   * @param access The Access Modes to grant to the Agent for the Resource.
   * @returns A new resource ACL initialised with the given `aclDataset` and `access` for the `agent`.
   */
  function setAgentResourceAccess$1(aclDataset, agent, access) {
      return internal_setActorAccess$2(aclDataset, access, acl.agent, "resource", agent);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns an Agent's Access Modes explicitly granted for the children of the
   * Container associated with the given ACL (Access Control List).
   *
   * The function does not return:
   * - Access Modes granted indirectly to the Agent through other ACL rules, e.g. public or group-specific permissions.
   * - Access Modes granted to the Agent for the Container Resource itself (see [[getAgentResourceAccess]] instead).
   *
   * @param aclDataset The SolidDataset that contains Access-Control List rules for a certain Container.
   * @param agent WebID of the Agent for which to retrieve what access it has to the Container's children.
   * @returns Access Modes that have been explicitly granted to an Agent for the children of the Container associated with the given ACL.
   */
  function getAgentDefaultAccess(aclDataset, agent) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getDefaultAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const agentResourceRules = getAgentAclRulesForAgent(resourceRules, agent);
      const agentAccessModes = agentResourceRules.map(internal_getAccess);
      return internal_combineAccessModes(agentAccessModes);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the Access Modes, per Agent, that have been explicitly granted for the children
   * of the Container associated with the given ACL (Access Control List).
   *
   * The function does not return:
   *
   * - Access Modes granted indirectly to the Agents through other ACL rules, e.g. public or group-specific permissions.
   * - Access Modes granted to the Agents for the Container Resource itself (see [[getAgentResourceAccessAll]] instead).
   *
   * @param aclDataset The SolidDataset that contains Access-Control List rules.
   * @returns Access Modes, per Agent, that have been explicitly granted for the children of the Container associated with the given ACL.
   */
  function getAgentDefaultAccessAll(aclDataset) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getDefaultAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const agentResourceRules = getAgentAclRules(resourceRules);
      return getAccessByAgent(agentResourceRules);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Modifies the default ACL (Access Control List) to set an Agent's Access Modes for the Container's children.
   * Specifically, the function returns a new default ACL initialised with the given ACL and
   * new rules for the Agent's access.
   *
   * If rules already exist for the Agent in the given ACL, in the returned ACL, they are replaced by the new rules.
   *
   * This function does not modify:
   * - Access Modes granted indirectly to the Agent through other ACL rules, e.g., public or group-specific permissions.
   * - Access Modes granted to the Agent for the Container Resource itself.
   * - The original ACL.
   *
   * @param aclDataset The SolidDataset that contains Access-Control List rules.
   * @param agent The Agent to grant specific Access Modes.
   * @param access The Access Modes to grant to the Agent.
   * @returns A new default ACL initialised with the given `aclDataset` and `access` for the `agent`.
   */
  function setAgentDefaultAccess(aclDataset, agent, access) {
      return internal_setActorAccess$2(aclDataset, access, acl.agent, "default", agent);
  }
  function getAgentAclRulesForAgent(aclRules, agent) {
      return internal_getAclRulesForIri(aclRules, agent, acl.agent);
  }
  function getAgentAclRules(aclRules) {
      return aclRules.filter(isAgentAclRule);
  }
  function isAgentAclRule(aclRule) {
      return getIri(aclRule, acl.agent) !== null;
  }
  function getAccessByAgent(aclRules) {
      return internal_getAccessByIri(aclRules, acl.agent);
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   * Returns a Group's explicity-granted Access Modes for a given Resource.
   *
   * The function does not return Access Modes granted indirectly to the Group through other
   * ACL rules, e.g., public permissions.
   *
   * @param resourceInfo Information about the Resource to which the given Group may have been granted access.
   * @param group URL of the Group for which to retrieve what access it has to the Resource.
   * @returns Access Modes that have been explicitly granted to the `group` for the given Resource, or `null` if it could not be determined (e.g. because the current user does not have Control Access to a given Resource or its Container).
   */
  function getGroupAccess$2(resourceInfo, group) {
      if (hasResourceAcl(resourceInfo)) {
          return getGroupResourceAccess(resourceInfo.internal_acl.resourceAcl, group);
      }
      if (hasFallbackAcl(resourceInfo)) {
          return getGroupDefaultAccess(resourceInfo.internal_acl.fallbackAcl, group);
      }
      return null;
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns all explicitly-granted Access Modes per Group for the given Resource.
   *
   * The function does not return Access Modes granted indirectly to the Group through other
   * ACL rules, e.g., public permissions.
   *
   * @param resourceInfo Information about the Resource to which the given Group may have been granted access.
   * @returns Access Modes per Group that have been explicitly granted for the given Resource, or `null` if it could not be determined (e.g. because the current user does not have Control Access to a given Resource or its Container).
   */
  function getGroupAccessAll$2(resourceInfo) {
      if (hasResourceAcl(resourceInfo)) {
          const resourceAcl = getResourceAcl(resourceInfo);
          return getGroupResourceAccessAll(resourceAcl);
      }
      if (hasFallbackAcl(resourceInfo)) {
          const fallbackAcl = getFallbackAcl(resourceInfo);
          return getGroupDefaultAccessAll(fallbackAcl);
      }
      return null;
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the Access Modes explicitly granted to a Group for the Resource
   * associated with an ACL (Access Control List).
   *
   * The function does not return:
   * - Access Modes granted indirectly to the Group through other ACL rules, e.g., public permissions.
   * - Access Modes granted to the Group for the child Resources if the associated Resource is a Container
   *   (see [[getGroupDefaultAccess]] instead).
   *
   * @param aclDataset The SolidDataset that contains Access Control List rules.
   * @param group URL of the Group for which to retrieve what access it has to the Resource.
   * @returns Access Modes explicitly granted to a Group for the Resource associated with an ACL.
   */
  function getGroupResourceAccess(aclDataset, group) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getResourceAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const groupResourceRules = getGroupAclRuleForGroup(resourceRules, group);
      const groupAccessModes = groupResourceRules.map(internal_getAccess);
      return internal_combineAccessModes(groupAccessModes);
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the explicitly granted Access Modes per Group for the Resource associated
   * with an ACL (Access Control List).
   *
   * The function does not return:
   * - Access Modes granted indirectly to the Group through other ACL rules, e.g., public permissions.
   * - Access Modes granted to Groups for the child Resources if the associated Resource is a Container.
   *
   * @param aclDataset The SolidDataset that contains Access Control List rules.
   * @returns Access Modes per Group that have been explicitly granted for the Resource associated with an ACL.
   */
  function getGroupResourceAccessAll(aclDataset) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getResourceAclRulesForResource(allRules, aclDataset.internal_accessTo);
      return getAccessByGroup(resourceRules);
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns a Group's Access Modes explicitly granted for the children of the
   * Container associated with an ACL (Access ControlList).
   *
   * The function does not return:
   * - Access Modes granted indirectly to the Group through other ACL rules, e.g. public permissions.
   * - Access Modes granted to the Group for the Container Resource itself (see [[getGroupResourceAccess]] instead).
   *
   * @param aclDataset The SolidDataset that contains ACL rules for a certain Container.
   * @param group URL of the Group for which to retrieve what access it has to the child Resources of the given Container.
   * @returns Access Modes that have been explicitly granted to the Group for the children of the Container associated with the given ACL.
   */
  function getGroupDefaultAccess(aclDataset, group) {
      const allRules = internal_getAclRules(aclDataset);
      const defaultRules = internal_getDefaultAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const groupDefaultRules = getGroupAclRuleForGroup(defaultRules, group);
      const groupAccessModes = groupDefaultRules.map(internal_getAccess);
      return internal_combineAccessModes(groupAccessModes);
  }
  /**
   * ```{note} This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the Access Modes, per Group, that have been explicitly granted for the children
   * of the Container associated with the given ACL (Access Control List).
   *
   * The function does not return:
   * - Access Modes granted indirectly to the Groups through other ACL rules, e.g. public permissions.
   * - Access Modes granted to the Groups for the Container Resource itself (see [[getGroupResourceAccessAll]] instead).
   *
   * @param aclDataset The SolidDataset that contains Access Control List rules for a certain Container.
   * @returns Access Modes per Group that have been explicitly granted for the children of the Container associated with the given ACL SolidDataset.
   */
  function getGroupDefaultAccessAll(aclDataset) {
      const allRules = internal_getAclRules(aclDataset);
      const defaultRules = internal_getDefaultAclRulesForResource(allRules, aclDataset.internal_accessTo);
      return getAccessByGroup(defaultRules);
  }
  function getGroupAclRuleForGroup(rules, group) {
      return internal_getAclRulesForIri(rules, group, acl.agentGroup);
  }
  function getAccessByGroup(aclRules) {
      return internal_getAccessByIri(aclRules, acl.agentGroup);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   * Modifies the resource ACL (Access Control List) to set the Access Modes for the given Group.
   * Specifically, the function returns a new resource ACL initialised with the given ACL and
   * new rules for the Group's access.
   *
   * If rules for Groups's access already exist in the given ACL, in the returned ACL,
   * they are replaced by the new rules.
   *
   * This function does not modify:
   *
   * - Access Modes granted indirectly to Groups through other ACL rules, e.g., public or Agent-specific permissions.
   * - Access Modes granted to Groups for the child Resources if the associated Resource is a Container.
   * - The original ACL.
   *
   * @param aclDataset The SolidDataset that contains Access-Control List rules.
   * @param group The Group to grant specific Access Modes.
   * @param access The Access Modes to grant to the Group for the Resource.
   * @returns A new resource ACL initialised with the given `aclDataset` and `access` for the `group`.
   * @since 1.4.0
   */
  function setGroupResourceAccess$1(aclDataset, group, access) {
      return internal_setActorAccess$2(aclDataset, access, acl.agentGroup, "resource", group);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Modifies the default ACL (Access Control List) to set a Group's Access Modes for the Container's children.
   * Specifically, the function returns a new default ACL initialised with the given ACL and
   * new rules for the Group's access.
   *
   * If rules already exist for the Group in the given ACL, in the returned ACL, they are replaced by the new rules.
   *
   * This function does not modify:
   * - Access Modes granted indirectly to the Group through other ACL rules, e.g., public or Agent-specific permissions.
   * - Access Modes granted to the Group for the Container Resource itself.
   * - The original ACL.
   *
   * @param aclDataset The SolidDataset that contains Access-Control List rules.
   * @param group The Group to grant specific Access Modes.
   * @param access The Access Modes to grant to the Group.
   * @returns A new default ACL initialised with the given `aclDataset` and `access` for the `group`.
   * @since 1.4.0
   */
  function setGroupDefaultAccess(aclDataset, group, access) {
      return internal_setActorAccess$2(aclDataset, access, acl.agentGroup, "default", group);
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the Access Modes granted to the public in general for a Resource.
   *
   * This function does not return Access Modes granted to specific Agents
   * through other ACL (Access Control List) rules, e.g., agent- or group-specific permissions.
   *
   * @param resourceInfo Information about the Resource to which the given Agent may have been granted access.
   * @returns Access Modes granted to the public in general for the Resource, or `null` if it could not be determined (e.g. because the current user does not have Control Access to a given Resource or its Container).
   */
  function getPublicAccess$3(resourceInfo) {
      if (hasResourceAcl(resourceInfo)) {
          return getPublicResourceAccess(resourceInfo.internal_acl.resourceAcl);
      }
      if (hasFallbackAcl(resourceInfo)) {
          return getPublicDefaultAccess(resourceInfo.internal_acl.fallbackAcl);
      }
      return null;
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the Access Modes granted to the public in general for the Resource
   * associated with an ACL (Access Control List).
   *
   * This function does not return:
   * - Access Modes granted to specific Agents through other ACL rules, e.g., agent- or group-specific permissions.
   * - Access Modes to child Resources if the associated Resource is a Container (see [[getPublicDefaultAccess]] instead).
   *
   * @param aclDataset The SolidDataset that contains Access Control List rules.
   * @returns Access Modes granted to the public in general for the Resource associated with the `aclDataset`.
   */
  function getPublicResourceAccess(aclDataset) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getResourceAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const publicResourceRules = getClassAclRulesForClass(resourceRules, foaf.Agent);
      const publicAccessModes = publicResourceRules.map(internal_getAccess);
      return internal_combineAccessModes(publicAccessModes);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Returns the Access Modes granted to the public in general for the child Resources
   * of the Container associated with an ACL (Access Control List).
   *
   * This function does not return:
   * - Access Modes granted to Agents through other ACL rules, e.g., agent- or group-specific permissions.
   * - Access Modes to the Container Resource itself (see [[getPublicResourceAccess]] instead).
   *
   * @param aclDataset The SolidDataset that contains Access Control List rules for a certain Container.
   * @returns Access Modes granted to the public in general for the children of the Container associated with the given `aclDataset`.
   */
  function getPublicDefaultAccess(aclDataset) {
      const allRules = internal_getAclRules(aclDataset);
      const resourceRules = internal_getDefaultAclRulesForResource(allRules, aclDataset.internal_accessTo);
      const publicResourceRules = getClassAclRulesForClass(resourceRules, foaf.Agent);
      const publicAccessModes = publicResourceRules.map(internal_getAccess);
      return internal_combineAccessModes(publicAccessModes);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Modifies the resource ACL (Access Control List) to set the Access Modes for the public.
   * Specifically, the function returns a new resource ACL (Access Control List) initialised
   * with the given resource ACL and new rules for the given public access.
   *
   * If rules for public access already exist in the given ACL, in the *returned* ACL,
   * they are replaced by the new rules.
   *
   * This function does not modify:
   * - Access Modes granted to Agents through other ACL rules, e.g., agent- or group-specific permissions.
   * - Access Modes to child Resources if the associated Resource is a Container.
   * - The original ACL.
   *
   * @param aclDataset The SolidDataset that contains Access Control List rules.
   * @param access The Access Modes to grant to the public.
   * @returns A new resource ACL initialised with the given `aclDataset` and public `access`.
   */
  function setPublicResourceAccess$1(aclDataset, access) {
      return internal_setActorAccess$2(aclDataset, access, acl.agentClass, "resource", foaf.Agent);
  }
  /**
   * ```{note}
   * This function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Modifies the default ACL (Access Control List) to set the public's default Access Modes
   * to child resources. Specifically, the function returns a new default ACL initialised
   * with the given default ACL and new rules for the given public access.
   *
   * If rules for public access already exist in the given ACL, in the *returned* ACL,
   * they are replaced by the new rules.
   *
   * This function does not modify:
   * - Access Modes granted to Agents through other ACL rules, e.g., agent- or group-specific permissions.
   * - Access Modes to Container Resource itself.
   * - The original ACL.
   *
   * @param aclDataset The SolidDataset that contains Access Control List rules.
   * @param access The Access Modes to grant to the public.
   * @returns A new default ACL initialised with the given `aclDataset` and public `access`.
   */
  function setPublicDefaultAccess(aclDataset, access) {
      return internal_setActorAccess$2(aclDataset, access, acl.agentClass, "default", foaf.Agent);
  }
  function getClassAclRulesForClass(aclRules, agentClass) {
      return aclRules.filter((rule) => appliesToClass(rule, agentClass));
  }
  function appliesToClass(aclRule, agentClass) {
      return getIriAll(aclRule, acl.agentClass).includes(agentClass);
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  function setMockAclUrl(resource, aclUrl) {
      const resourceWithAclUrl = Object.assign(internal_cloneResource(resource), {
          internal_resourceInfo: Object.assign(Object.assign({}, resource.internal_resourceInfo), { aclUrl: aclUrl }),
      });
      return resourceWithAclUrl;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * ```{warning}
   * Do not use this function in production code. For use in **unit tests** that
   * require a [[SolidDataset]] with a resource ACL (Access Control List).
   * ```
   *
   * Initialises a new empty ACL and attaches it to a given [[SolidDataset]] for use
   * in **unit tests**; e.g., unit tests that call [[getResourceAcl]].
   *
   * @param resource The Resource to mock up with a new resource ACL.
   * @returns The input Resource with an empty resource ACL attached.
   * @since 0.2.0
   */
  function addMockResourceAclTo(resource) {
      var _a;
      const aclUrl = (_a = resource.internal_resourceInfo.aclUrl) !== null && _a !== void 0 ? _a : "https://your.pod/mock-acl.ttl";
      const resourceWithAclUrl = Object.assign(internal_cloneResource(resource), {
          internal_resourceInfo: Object.assign(Object.assign({}, resource.internal_resourceInfo), { aclUrl: aclUrl }),
      });
      const aclDataset = createAcl(resourceWithAclUrl);
      const resourceWithResourceAcl = internal_setAcl(resourceWithAclUrl, {
          resourceAcl: aclDataset,
          fallbackAcl: null,
      });
      return resourceWithResourceAcl;
  }
  /**
   *
   * ```{warning}
   * Do not use this function in production code.  For use in **unit tests** that require a
   * [[SolidDataset]] with a fallback ACL (Access Control List).
   * ```
   *
   * Initialises a new empty fallback ACL and attaches it to a given [[SolidDataset]] for use
   * in **unit tests**; e.g., unit tests that call [[getFallbackAcl]].
   *
   * @param resource The Resource to mock up with new fallback ACL.
   * @returns The input Resource with an empty fallback ACL attached.
   * @since 0.2.0
   */
  function addMockFallbackAclTo(resource) {
      const containerUrl = internal_getContainerPath(getSourceIri(resource));
      const aclUrl = containerUrl + ".acl";
      const mockContainer = setMockAclUrl(mockContainerFrom(containerUrl), aclUrl);
      const aclDataset = createAcl(mockContainer);
      const resourceWithFallbackAcl = internal_setAcl(internal_cloneResource(resource), {
          resourceAcl: null,
          fallbackAcl: aclDataset,
      });
      return resourceWithFallbackAcl;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const rdfJsDataset = dataset_1.dataset;
  /**
   * Convert an RDF/JS Dataset into a [[SolidDataset]]
   *
   * Parse an RDF/JS
   * {@link https://rdf.js.org/dataset-spec/#datasetcore-interface DatasetCore},
   * into a [[SolidDataset]]. Note that, when saving the returned SolidDataset to
   * a Solid Pod, only Quads in the Default Graph will be stored.
   *
   * @param rdfJsDataset The source RDF/JS Dataset.
   * @returns A [[SolidDataset]] containing the same data as the given RDF/JS Dataset.
   * @since 1.9.0
   */
  function fromRdfJsDataset(rdfJsDataset) {
      const dataset = {
          graphs: { default: {} },
          type: "Dataset",
      };
      const quads = Array.from(rdfJsDataset);
      const chainBlankNodes = getChainBlankNodes(quads);
      // Quads with chain Blank Nodes as their Subject will be parsed when those
      // Blank Nodes are referred to in an Object. See `addRdfJsQuadToObjects`.
      const quadsWithoutChainBlankNodeSubjects = quads.filter((quad) => chainBlankNodes.every((chainBlankNode) => !chainBlankNode.equals(quad.subject)));
      return quadsWithoutChainBlankNodeSubjects.reduce((datasetAcc, quad) => addRdfJsQuadToDataset(datasetAcc, quad, {
          otherQuads: quads,
          chainBlankNodes: chainBlankNodes,
      }), dataset);
  }
  /**
   * Convert a [[SolidDataset]] into an RDF/JS Dataset
   *
   * Export a [[SolidDataset]] into an RDF/JS
   * {@link https://rdf.js.org/dataset-spec/#datasetcore-interface DatasetCore}.
   *
   * @param set A [[SolidDataset]] to export into an RDF/JS Dataset.
   * @param options Optional parameter that allows you to pass in your own RDF/JS DataFactory or DatasetCoreFactory.
   * @returns An RDF/JS Dataset containing the data from the given SolidDataset.
   * @since 1.9.0
   */
  function toRdfJsDataset(set, options = {}) {
      var _a, _b;
      const datasetFactory = (_b = (_a = options.datasetFactory) === null || _a === void 0 ? void 0 : _a.dataset) !== null && _b !== void 0 ? _b : rdfJsDataset;
      return datasetFactory(toRdfJsQuads(set, options));
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /** @hidden */
  function internal_getAcr(resource) {
      if (!hasAccessibleAcr(resource)) {
          throw new Error(`An Access Control Resource for [${getSourceUrl(resource)}] is not available. This could be because the current user is not allowed to see it, or because their Pod Server does not support Access Control Resources.`);
      }
      return resource.internal_acp.acr;
  }
  /** @hidden */
  function internal_setAcr(resource, acr) {
      return Object.assign(internal_cloneResource(resource), {
          internal_acp: Object.assign(Object.assign({}, resource.internal_acp), { acr: acr }),
      });
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Initialise a new [[Control]].
   * @hidden Developers don't need to care about initialising Controls - they can just add Policies directly.
   * @deprecated
   */
  function internal_createControl(options) {
      let control = createThing(options);
      control = setIri(control, rdf$2.type, acp.AccessControl);
      return control;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Find an [[Control]] with a given URL in a given Resource with an Access Control Resource.
   *
   * @returns The requested Access Control, or `null` if it could not be found.
   * @hidden Developers don't need to care about initialising Controls - they can just add Policies directly.
   * @deprecated
   */
  function internal_getControl(withAccessControlResource, url, options) {
      const acr = internal_getAcr(withAccessControlResource);
      const foundThing = getThing(acr, url, options);
      if (foundThing === null ||
          !getIriAll(foundThing, rdf$2.type).includes(acp.AccessControl)) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get all [[Control]]s in the Access Control Resource of a given Resource.
   * @hidden Developers don't need to care about initialising Controls - they can just add Policies directly.
   * @deprecated
   */
  function internal_getControlAll(withAccessControlResource, options) {
      const acr = internal_getAcr(withAccessControlResource);
      const foundThings = getThingAll(acr, options);
      return foundThings.filter((foundThing) => getIriAll(foundThing, rdf$2.type).includes(acp.AccessControl));
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert an [[Control]] into the [[AccessControlResource]] of a Resource, replacing previous
   * instances of that Access Control.
   *
   * @param withAccessControlResource A Resource with the Access Control Resource into which to insert an Access Control.
   * @param control The Control to insert into the Access Control Resource.
   * @returns The given Resource with a new Access Control Resource equal to the original Access Control Resource, but with the given Access Control.
   * @hidden Developers don't need to care about initialising Controls - they can just add Policies directly.
   * @deprecated
   */
  function internal_setControl(withAccessControlResource, control) {
      const acr = internal_getAcr(withAccessControlResource);
      const updatedAcr = setThing(acr, control);
      const updatedResource = internal_setAcr(withAccessControlResource, updatedAcr);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a [[Policy]] to an [[Control]] such that that Policy applies to the Resource to which
   * the [[Control]] is linked.
   *
   * @param accessControl The [[Control]] to which the Policy should be added.
   * @param policyUrl URL of the Policy that should apply to the Resource to which the [[Control]] is linked.
   * @returns A new [[Control]] equal to the given [[Control]], but with the given policy added to it.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_addPolicyUrl(accessControl, policyUrl) {
      return addIri(accessControl, acp.apply, policyUrl);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get all Policies that apply to the Resource to which the given [[Control]] is linked, and
   * which can be removed by anyone with Write access to the Access Control Resource that contains the
   * [[Control]].
   *
   * @param accessControl The [[Control]] of which to get the Policies.
   * @returns The Policies that are listed in this [[Control]] as applying to the Resource it is linked to, and as removable by anyone with Write access to the Access Control Resource.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_getPolicyUrlAll(accessControl) {
      return getIriAll(accessControl, acp.apply);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove a given Policy that applies to the Resource to which the given [[Control]] is linked,
   * and which can be removed by anyone with Write access to the Access Control Resource that contains
   * the Access Control.
   *
   * @param accessControl The [[Control]] of which to remove the Policies.
   * @param policyUrl URL of the Policy that should no longer apply to the Resource to which the [[Control]] is linked.
   * @returns A new [[Control]] equal to the given [[Control]], but with the given Policy removed from it.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_removePolicyUrl(accessControl, policyUrl) {
      return removeIri(accessControl, acp.apply, policyUrl);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove all Policies that apply to the Resource to which the given [[Control]] is linked, and
   * which can be removed by anyone with Write access to the Access Control Resource that contains the
   * [[Control]].
   *
   * @param accessControl The [[Control]] of which to remove the Policies.
   * @returns A new [[Control]] equal to the given [[Control]], but with all Policies removed from it.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_removePolicyUrlAll(accessControl) {
      return removeAll(accessControl, acp.apply);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a [[Policy]] to an [[Control]] such that that Policy applies to the children of the
   * Resource to which the [[Control]] is linked.
   *
   * @param accessControl The [[Control]] to which the Policy should be added.
   * @param policyUrl URL of the Policy that should apply to the children of the Resource to which the [[Control]] is linked.
   * @returns A new [[Control]] equal to the given [[Control]], but with the given policy added to it as a Member Policy.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_addMemberPolicyUrl(accessControl, policyUrl) {
      return addIri(accessControl, acp.applyMembers, policyUrl);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get all Policies that apply to the children of the Resource to which the given [[Control]] is
   * linked, and which can be removed by anyone with Write access to the Access Control Resource that
   * contains the [[Control]].
   *
   * @param accessControl The [[Control]] of which to get the Policies.
   * @returns The Policies that are listed in this [[Control]] as applying to the children of the Resource it is linked to, and as removable by anyone with Write access to the Access Control Resource.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_getMemberPolicyUrlAll(accessControl) {
      return getIriAll(accessControl, acp.applyMembers);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove a given Policy that applies to the children of the Resource to which the given Access
   * Control is linked, and which can be removed by anyone with Write access to the Access Control
   * Resource that contains the Access Control.
   *
   * @param accessControl The [[Control]] of which to remove the Member Policy.
   * @param policyUrl URL of the Member Policy that should no longer apply to the Resource to which the [[Control]] is linked.
   * @returns A new [[Control]] equal to the given [[Control]], but with the given Member Policy removed from it.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_removeMemberPolicyUrl(accessControl, policyUrl) {
      return removeIri(accessControl, acp.applyMembers, policyUrl);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove all Policies that apply to the children of the Resource to which the given Access Control
   * is linked, and which can be removed by anyone with Write access to the Access Control Resource
   * that contains the Access Control.
   *
   * @param accessControl The [[Control]] of which to remove the Member Policies.
   * @returns A new [[Control]] equal to the given [[Control]], but with all Member Policies removed from it.
   * @hidden Developers don't need to care about working with Controls - they can just add Policies to the Resource directly.
   * @deprecated
   */
  function internal_removeMemberPolicyUrlAll(accessControl) {
      return removeAll(accessControl, acp.applyMembers);
  }
  function internal_getInitialisedControl(resourceWithAcr) {
      const allControls = internal_getControlAll(resourceWithAcr);
      return allControls.length === 0 ? internal_createControl() : allControls[0];
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a Resource, check whether it is governed by Access Policies.
   * (Specifically, a Resource that is governed by Access Policies will refer to exactly one Access
   * Control Resource, and expose that to users who are allowed to see or modify access to the given
   * Resource.)
   *
   * @param resource Resource which may or may not be governed by Access Policies.
   * @returns True if the Resource refers to an Access Control Resource and is hence governed by Access Policies, or false if it does not.
   * @since 1.6.0
   */
  function hasLinkedAcr(resource) {
      return (hasServerResourceInfo(resource) &&
          Array.isArray(resource.internal_resourceInfo.linkedResources[acp.accessControl]) &&
          resource.internal_resourceInfo.linkedResources[acp.accessControl].length ===
              1);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a [[Policy]] to an Access Control Resource such that that [[Policy]] applies to the Access
   * Control Resource itself, rather than the Resource it governs.
   *
   * @param resourceWithAcr The Resource with an Access Control Resource to which the ACR Policy should be added.
   * @param policyUrl URL of the Policy that should apply to the given Access Control Resource.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but with the given ACR Policy added to it.
   * @since 1.6.0
   */
  function addAcrPolicyUrl(resourceWithAcr, policyUrl) {
      var _a;
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      let acrThing = (_a = getThing(acr, acrUrl)) !== null && _a !== void 0 ? _a : createThing({ url: acrUrl });
      acrThing = addIri(acrThing, acp.access, policyUrl);
      const updatedAcr = setThing(acr, acrThing);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a [[Policy]] to a Resource's Access Control Resource such that that
   * Policy applies to the Access Control Resources of child Resources.
   *
   * @param resourceWithAcr The Resource with an Access Control Resource to which the ACR Policy should be added.
   * @param policyUrl URL of the Policy that should apply to the given Access Control Resources of children of the Resource.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but with the given ACR Policy added to it.
   * @since 1.6.0
   */
  function addMemberAcrPolicyUrl(resourceWithAcr, policyUrl) {
      var _a;
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      let acrThing = (_a = getThing(acr, acrUrl)) !== null && _a !== void 0 ? _a : createThing({ url: acrUrl });
      acrThing = addIri(acrThing, acp.accessMembers, policyUrl);
      const updatedAcr = setThing(acr, acrThing);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the URLs of the Access Policies that apply to an Access Control Resource itself, rather than
   * to the Resource it governs.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource of which to get the URLs of the Policies that govern access to it.
   * @returns URLs of the Policies that govern access to the given Access Control Resource.
   * @since 1.6.0
   */
  function getAcrPolicyUrlAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const acrThing = getThing(acr, acrUrl);
      if (acrThing === null) {
          return [];
      }
      return getIriAll(acrThing, acp.access);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the URLs of the Access Policies that apply to the Access Control Resources of the Resource's
   * children.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource of which to get the URLs of the Policies that govern access to its children.
   * @returns URLs of the Policies that govern access to the Access Control Resources of the given Resource's children.
   * @since 1.6.0
   */
  function getMemberAcrPolicyUrlAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const acrThing = getThing(acr, acrUrl);
      if (acrThing === null) {
          return [];
      }
      return getIriAll(acrThing, acp.accessMembers);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop the URL of a given [[Policy]] from applying to an Access Control Resource itself.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource to which the given URL of a Policy should no longer apply.
   * @param policyUrl The URL of the Policy that should no longer apply.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but with the given ACR Policy removed from it.
   * @since 1.6.0
   */
  function removeAcrPolicyUrl(resourceWithAcr, policyUrl) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const acrThing = getThing(acr, acrUrl);
      if (acrThing === null) {
          return resourceWithAcr;
      }
      const updatedAcrThing = removeIri(acrThing, acp.access, policyUrl);
      const updatedAcr = setThing(acr, updatedAcrThing);
      return internal_setAcr(resourceWithAcr, updatedAcr);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop the URL of a given [[Policy]] from applying to the Access Control Resources of the
   * Resource's children.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource to whose children's ACRs the given URL of a Policy should no longer apply.
   * @param policyUrl The URL of the Policy that should no longer apply.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but with the given member ACR Policy removed from it.
   * @since 1.6.0
   */
  function removeMemberAcrPolicyUrl(resourceWithAcr, policyUrl) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const acrThing = getThing(acr, acrUrl);
      if (acrThing === null) {
          return resourceWithAcr;
      }
      const updatedAcrThing = removeIri(acrThing, acp.accessMembers, policyUrl);
      const updatedAcr = setThing(acr, updatedAcrThing);
      return internal_setAcr(resourceWithAcr, updatedAcr);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop all URL of Access Policies from applying to an Access Control Resource itself.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource to which no more Policies should apply.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but without any Policy applying to it.
   * @since 1.6.0
   */
  function removeAcrPolicyUrlAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const acrThing = getThing(acr, acrUrl);
      if (acrThing === null) {
          return resourceWithAcr;
      }
      const updatedAcrThing = removeAll(acrThing, acp.access);
      const updatedAcr = setThing(acr, updatedAcrThing);
      return internal_setAcr(resourceWithAcr, updatedAcr);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop all URL of Access Policies from applying to the Access Control Resources of the Resource's
   * children.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource that should no longer apply Policies to its children's ACRs.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but without any Policy applying to its children's ACRs.
   * @since 1.6.0
   */
  function removeMemberAcrPolicyUrlAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const acrThing = getThing(acr, acrUrl);
      if (acrThing === null) {
          return resourceWithAcr;
      }
      const updatedAcrThing = removeAll(acrThing, acp.accessMembers);
      const updatedAcr = setThing(acr, updatedAcrThing);
      return internal_setAcr(resourceWithAcr, updatedAcr);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a [[Policy]] to an Access Control Resource such that that [[Policy]] applies to that Resource.
   *
   * @param resourceWithAcr The Resource to which the ACR Policy should be added.
   * @param policyUrl URL of the Policy that should apply to the given Resource.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but with the given Policy added to it.
   * @since 1.6.0
   */
  function addPolicyUrl(resourceWithAcr, policyUrl) {
      const control = internal_getInitialisedControl(resourceWithAcr);
      const updatedControl = internal_addPolicyUrl(control, policyUrl);
      const updatedResource = internal_setControl(resourceWithAcr, updatedControl);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a [[Policy]] to a Resource's Access Control Resource such that that
   * Policy applies to that Resource's children.
   *
   * @param resourceWithAcr The Resource to whose Access Control Resource the Policy should be added.
   * @param policyUrl URL of the Policy that should apply to the given Resource's children.
   * @returns A new Resource equal to the given Resource, but with the given Member Policy added to its Access Control Resource.
   * @since 1.6.0
   */
  function addMemberPolicyUrl(resourceWithAcr, policyUrl) {
      const control = internal_getInitialisedControl(resourceWithAcr);
      const updatedControl = internal_addMemberPolicyUrl(control, policyUrl);
      const updatedResource = internal_setControl(resourceWithAcr, updatedControl);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the URLs of the Access Policies that apply to a Resource.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource of which to get the URLs of the Policies that govern access to it.
   * @returns URLs of the Policies that govern access to the given Resource.
   * @since 1.6.0
   */
  function getPolicyUrlAll(resourceWithAcr) {
      const controls = internal_getControlAll(resourceWithAcr);
      const policyUrlsByControl = controls.map((control) => internal_getPolicyUrlAll(control));
      const uniquePolicyUrls = new Set();
      policyUrlsByControl.forEach((policyUrls) => {
          policyUrls.forEach((url) => uniquePolicyUrls.add(url));
      });
      return Array.from(uniquePolicyUrls);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the URLs of the Access Policies that apply to a Resource's children.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource of which to get the URLs of the Policies that govern access to its children.
   * @returns URLs of the Policies that govern access to the given Resource's children.
   * @since 1.6.0
   */
  function getMemberPolicyUrlAll(resourceWithAcr) {
      const controls = internal_getControlAll(resourceWithAcr);
      const memberPolicyUrlsByControl = controls.map((control) => internal_getMemberPolicyUrlAll(control));
      const uniquePolicyUrls = new Set();
      memberPolicyUrlsByControl.forEach((policyUrls) => {
          policyUrls.forEach((url) => uniquePolicyUrls.add(url));
      });
      return Array.from(uniquePolicyUrls);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop the URL of a given [[Policy]] from applying to a Resource.
   *
   * @param resourceWithAcr The Resource, with its Access Control Resource, to which the given URL of a Policy should no longer apply.
   * @param policyUrl The URL of the Policy that should no longer apply.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but with the given Policy removed from it.
   * @since 1.6.0
   */
  function removePolicyUrl(resourceWithAcr, policyUrl) {
      const controls = internal_getControlAll(resourceWithAcr);
      const updatedControls = controls.map((control) => internal_removePolicyUrl(control, policyUrl));
      const updatedResource = updatedControls.reduce(internal_setControl, resourceWithAcr);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop the URL of a given [[Policy]] from applying to the Resource's children.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource to whose children the given URL of a Policy should no longer apply.
   * @param policyUrl The URL of the Policy that should no longer apply.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but with the given Member Policy removed from it.
   * @since 1.6.0
   */
  function removeMemberPolicyUrl(resourceWithAcr, policyUrl) {
      const controls = internal_getControlAll(resourceWithAcr);
      const updatedControls = controls.map((control) => internal_removeMemberPolicyUrl(control, policyUrl));
      const updatedResource = updatedControls.reduce(internal_setControl, resourceWithAcr);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop all URL of Access Policies from applying to a Resource.
   *
   * @param resourceWithAcr The Resource, with its Access Control Resource, to which no more Policies should apply.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but without any Policy applying to the Resource.
   * @since 1.6.0
   */
  function removePolicyUrlAll(resourceWithAcr) {
      const controls = internal_getControlAll(resourceWithAcr);
      const updatedControls = controls.map((control) => internal_removePolicyUrlAll(control));
      const updatedResource = updatedControls.reduce(internal_setControl, resourceWithAcr);
      return updatedResource;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Stop all URL of Access Policies from applying to the Resource's children.
   *
   * @param resourceWithAcr The Resource with the Access Control Resource that should no longer apply Policies to its children.
   * @returns A Resource with a new Access Control Resource equal to the original ACR, but without any Policy applying to the Resource's children.
   * @since 1.6.0
   */
  function removeMemberPolicyUrlAll(resourceWithAcr) {
      const controls = internal_getControlAll(resourceWithAcr);
      const updatedControls = controls.map((control) => internal_removeMemberPolicyUrlAll(control));
      const updatedResource = updatedControls.reduce(internal_setControl, resourceWithAcr);
      return updatedResource;
  }
  /**
   * Gets a human-readable representation of the given [[Control]] to aid debugging.
   *
   * Note that changes to the exact format of the return value are not considered a breaking change;
   * it is intended to aid in debugging, not as a serialisation method that can be reliably parsed.
   *
   * @param resourceWithAcr The Resource with an attached Access Control Resource of which you want to get a human-readable representation.
   * @since 1.6.0
   */
  function acrAsMarkdown(resourceWithAcr) {
      let markdown = `# Access controls for ${getSourceUrl(resourceWithAcr)}\n`;
      const policyUrls = getPolicyUrlAll(resourceWithAcr);
      const memberPolicyUrls = getMemberPolicyUrlAll(resourceWithAcr);
      const acrPolicyUrls = getAcrPolicyUrlAll(resourceWithAcr);
      const memberAcrPolicyUrls = getMemberAcrPolicyUrlAll(resourceWithAcr);
      if (policyUrls.length === 0 &&
          memberPolicyUrls.length === 0 &&
          acrPolicyUrls.length === 0 &&
          memberAcrPolicyUrls.length === 0) {
          markdown += "\n<no policies specified yet>\n";
      }
      if (policyUrls.length > 0) {
          markdown += "\nThe following policies apply to this resource:\n- ";
          markdown += policyUrls.join("\n- ") + "\n";
      }
      if (acrPolicyUrls.length > 0) {
          markdown +=
              "\nThe following policies apply to the access control resource for this resource:\n- ";
          markdown += acrPolicyUrls.join("\n- ") + "\n";
      }
      if (memberPolicyUrls.length > 0) {
          markdown +=
              "\nThe following policies apply to the children of this resource:\n- ";
          markdown += memberPolicyUrls.join("\n- ") + "\n";
      }
      if (memberAcrPolicyUrls.length > 0) {
          markdown +=
              "\nThe following policies apply to the access control resources for children of this resource:\n- ";
          markdown += memberAcrPolicyUrls.join("\n- ") + "\n";
      }
      return markdown;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Fetch a SolidDataset and its associated Access Control Resource (if available to the current user).
   *
   * @param url URL of the SolidDataset to fetch.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A SolidDataset and the ACR that applies to it, if available to the authenticated user.
   * @since 1.6.0
   */
  async function getSolidDatasetWithAcr(url, options = internal_defaultFetchOptions) {
      const urlString = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const solidDataset = await getSolidDataset(urlString, config);
      const acp = await fetchAcr(solidDataset, config);
      return Object.assign(Object.assign({}, solidDataset), acp);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Fetch a file and its associated Access Control Resource (if available to the current user).
   *
   * @param url URL of the file to fetch.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A file and the ACR that applies to it, if available to the authenticated user.
   * @since 1.6.0
   */
  async function getFileWithAcr(url, options = internal_defaultFetchOptions) {
      const urlString = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const file = await getFile(urlString, config);
      const acp = await fetchAcr(file, config);
      return Object.assign(file, acp);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Retrieve information about a Resource and its associated Access Control Resource (if available to
   * the current user), without fetching the Resource itself.
   *
   * @param url URL of the Resource about which to fetch its information.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns Metadata describing a Resource, and the ACR that applies to it, if available to the authenticated user.
   * @since 1.6.0
   */
  async function getResourceInfoWithAcr(url, options = internal_defaultFetchOptions) {
      const urlString = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const resourceInfo = await getResourceInfo(urlString, config);
      const acp = await fetchAcr(resourceInfo, config);
      return Object.assign(Object.assign({}, resourceInfo), acp);
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Fetch a SolidDataset, and:
   * - if the Resource is governed by an ACR: its associated Access Control Resource (if available to
   *                                          the current user), and all the Access Control Policies
   *                                          referred to therein, if available to the current user.
   * - if the Resource is governed by an ACL: its associated Resource ACL (if available to the current
   *                                          user), or its Fallback ACL if it does not exist.
   *
   * @param url URL of the SolidDataset to fetch.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A SolidDataset and either the ACL access data or the ACR access data, if available to the current user.
   * @since 1.6.0
   */
  async function getSolidDatasetWithAccessDatasets(url, options = internal_defaultFetchOptions) {
      const urlString = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const solidDataset = await getSolidDataset(urlString, config);
      if (hasAccessibleAcl(solidDataset)) {
          const acl = await internal_fetchAcl(solidDataset, config);
          return internal_setAcl(solidDataset, acl);
      }
      else {
          const acr = await fetchAcr(solidDataset, config);
          return Object.assign(Object.assign({}, solidDataset), acr);
      }
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Fetch a File, and:
   * - if the Resource is governed by an ACR: its associated Access Control Resource (if available to
   *                                          the current user), and all the Access Control Policies
   *                                          referred to therein, if available to the current user.
   * - if the Resource is governed by an ACL: its associated Resource ACL (if available to the current
   *                                          user), or its Fallback ACL if it does not exist.
   *
   * @param url URL of the File to fetch.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A File and either the ACL access data or the ACR access data, if available to the current user.
   * @since 1.6.0
   */
  async function getFileWithAccessDatasets(url, options = internal_defaultFetchOptions) {
      const urlString = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const file = await getFile(urlString, config);
      if (hasAccessibleAcl(file)) {
          const acl = await internal_fetchAcl(file, config);
          return internal_setAcl(file, acl);
      }
      else {
          const acr = await fetchAcr(file, config);
          return Object.assign(file, acr);
      }
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Fetch information about a Resource, and:
   * - if the Resource is governed by an ACR: its associated Access Control Resource (if available to
   *                                          the current user), and all the Access Control Policies
   *                                          referred to therein, if available to the current user.
   * - if the Resource is governed by an ACL: its associated Resource ACL (if available to the current
   *                                          user), or its Fallback ACL if it does not exist.
   *
   * @param url URL of the Resource information about which to fetch.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns Information about a Resource and either the ACL access data or the ACR access data, if available to the current user.
   * @since 1.6.0
   */
  async function getResourceInfoWithAccessDatasets(url, options = internal_defaultFetchOptions) {
      const urlString = internal_toIriString(url);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const resourceInfo = await getResourceInfo(urlString, config);
      if (hasAccessibleAcl(resourceInfo)) {
          const acl = await internal_fetchAcl(resourceInfo, config);
          return internal_setAcl(resourceInfo, acl);
      }
      else {
          const acr = await fetchAcr(resourceInfo, config);
          return Object.assign(Object.assign({}, resourceInfo), acr);
      }
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Save a Resource's Access Control Resource.
   *
   * @param resource Resource with an Access Control Resource that should be saved.
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @since 1.6.0
   */
  async function saveAcrFor(resource, options = internal_defaultFetchOptions) {
      const acr = internal_getAcr(resource);
      const config = Object.assign(Object.assign({}, internal_defaultFetchOptions), options);
      const savedAcr = await saveSolidDatasetAt(getSourceUrl(acr), acr, config);
      return internal_setAcr(resource, savedAcr);
  }
  /**
   * @param resource Resource of which to check whether it has an Access Control Resource attached.
   * @returns Boolean representing whether the given Resource has an Access Control Resource attached for use in e.g. [[getPolicyUrlAll]].
   * @since 1.6.0
   */
  function hasAccessibleAcr(resource) {
      return (typeof resource.internal_acp === "object" &&
          resource.internal_acp !== null &&
          typeof resource.internal_acp.acr === "object" &&
          resource.internal_acp.acr !== null);
  }
  async function fetchAcr(resource, options) {
      let acrUrl = undefined;
      if (hasLinkedAcr(resource)) {
          // Whereas a Resource can generally have multiple linked Resources for the same relation,
          // it can only have one Access Control Resource for that ACR to be valid.
          // Hence the accessing of [0] directly:
          acrUrl =
              resource.internal_resourceInfo.linkedResources[acp.accessControl][0];
      }
      else if (hasAccessibleAcl(resource)) {
          // The ACP proposal will be updated to expose the Access Control Resource
          // via a Link header with rel="acl", just like WAC. That means that if
          // an ACL is advertised, we can still fetch its metadata — if that indicates
          // that it's actually an ACP Access Control Resource, then we can fetch that
          // instead.
          const aclResourceInfo = await getResourceInfo(resource.internal_resourceInfo.aclUrl, options);
          if (isAcr(aclResourceInfo)) {
              acrUrl = getSourceUrl(aclResourceInfo);
          }
      }
      // If the Resource doesn't advertise an ACR via the old Link header,
      // nor via a rel="acl" header, then return, indicating that no ACR could be
      // fetched:
      if (typeof acrUrl !== "string") {
          return {
              internal_acp: {
                  acr: null,
              },
          };
      }
      let acr;
      try {
          acr = await getSolidDataset(acrUrl, options);
      }
      catch (e) {
          return {
              internal_acp: {
                  acr: null,
              },
          };
      }
      const acrDataset = Object.assign(Object.assign({}, acr), { accessTo: getSourceUrl(resource) });
      const acpInfo = {
          internal_acp: {
              acr: acrDataset,
          },
      };
      return acpInfo;
  }
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * To make it easy to fetch all the relevant Access Policy Resources,
   * this function returns all referenced Access Policy Resources referenced in an
   * Access Control Resource.
   * In other words, if Access Controls refer to different Policies in the same
   * Access Policy Resource, this function will only return that Access Policy
   * Resource's URL once.
   *
   * @param withAcr A Resource with an Access Control Resource attached.
   * @returns List of all unique Access Policy Resources that are referenced in the given Access Control Resource.
   * @since 1.6.0
   */
  function getReferencedPolicyUrlAll(withAcr) {
      const policyUrls = getPolicyUrlAll(withAcr)
          .map(normalizeServerSideIri)
          .concat(getMemberPolicyUrlAll(withAcr).map(normalizeServerSideIri))
          .concat(getAcrPolicyUrlAll(withAcr).map(normalizeServerSideIri))
          .concat(getMemberAcrPolicyUrlAll(withAcr).map(normalizeServerSideIri));
      const uniqueUrls = Array.from(new Set(policyUrls));
      return uniqueUrls;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * NOTE: Don't export for now (i.e. if exported, should this be `isAcpRule()` so
   * as not to clash with `isAclRule()`.
   *
   * @param thing the [[Thing]] to check to see if it's an ACP rule or not
   */
  function isRule(thing) {
      return getIriAll(thing, rdf$2.type).includes(acp.Rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a rule that refines the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is **not** present in **any** of the "All Of" rules,
   * they will not be granted access.
   *
   * Also see [[addAnyOfRuleUrl]] and [[addNoneOfRuleUrl]].
   *
   * @param policy The [[Policy]] to which the rule should be added.
   * @param rule The rule to add to the policy.
   * @returns A new [[Policy]] clone of the original one, with the new rule added.
   * @since 1.6.0
   */
  function addAllOfRuleUrl(policy, rule) {
      return addIri(policy, acp.allOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes a rule that refines the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is **not** present in **any** of the "All Of" rules,
   * they will not be granted access.
   * @param policy The [[Policy]] from which the rule should be removed.
   * @param rule The rule to remove from the policy.
   * @returns A new [[Policy]] clone of the original one, with the rule removed.
   * @since 1.6.0
   */
  function removeAllOfRuleUrl(policy, rule) {
      return removeIri(policy, acp.allOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrites the rule refining the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is **not** present in **any** of the "All Of" rules,
   * they will not be granted access.
   * @param policy The [[Policy]] to which the rule should be added.
   * @param rules The rules the policy requires.
   * @returns A new [[Policy]] clone of the original one, with the "All Of" rules replaced.
   * @since 1.6.0
   */
  function setAllOfRuleUrl(policy, rule) {
      return setIri(policy, acp.allOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the "All Of" [[Rule]]s for the given [[Policy]]
   * @param policy The [[policy]] from which the rules should be read.
   * @returns A list of the "All Of" [[Rule]]s
   * @since 1.6.0
   */
  function getAllOfRuleUrlAll(policy) {
      return getIriAll(policy, acp.allOf);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a rule that extends the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the "Any Of" rules,
   * they will be granted access.
   *
   * Also see [[addAllOfRuleUrl]] and [[addNoneOfRuleUrl]].
   *
   * @param policy The [[Policy]] to which the rule should be added.
   * @param rule The rule to add to the policy.
   * @returns A new [[Policy]] clone of the original one, with the new rule added.
   * @since 1.6.0
   */
  function addAnyOfRuleUrl(policy, rule) {
      return addIri(policy, acp.anyOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes a rule that extends the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the "Any Of" rules,
   * they will be granted access.
   * @param policy The [[Policy]] from which the rule should be removed.
   * @param rule The rule to remove from the policy.
   * @returns A new [[Policy]] clone of the original one, with the rule removed.
   * @since 1.6.0
   */
  function removeAnyOfRuleUrl(policy, rule) {
      return removeIri(policy, acp.anyOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrite the rule extending the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the "Any Of" rules,
   * they will be granted access.
   * @param policy The [[Policy]] to which the rule should be added.
   * @param rules The rules the policy accepts.
   * @returns A new [[Policy]] clone of the original one, with the "Any Of" rules replaced.
   * @since 1.6.0
   */
  function setAnyOfRuleUrl(policy, rule) {
      return setIri(policy, acp.anyOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the "Any Of" [[Rule]]s for the given [[Policy]]
   * @param policy The [[policy]] from which the rules should be read.
   * @returns A list of the "Any Of" [[Rule]]s
   * @since 1.6.0
   */
  function getAnyOfRuleUrlAll(policy) {
      return getIriAll(policy, acp.anyOf);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a rule that restricts the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the forbidden rules,
   * they will **not** be granted access.
   *
   * Also see [[addAllOfRuleUrl]] and [[addAnyOfRuleUrl]].
   *
   * @param policy The [[Policy]] to which the rule should be added.
   * @param rule The rule to add to the policy.
   * @returns A new [[Policy]] clone of the original one, with the new rule added.
   * @since 1.6.0
   */
  function addNoneOfRuleUrl(policy, rule) {
      return addIri(policy, acp.noneOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes a rule that restricts the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the forbidden rules,
   * they will **not** be granted access.
   * @param policy The [[Policy]] from which the rule should be removed.
   * @param rule The rule to remove from the policy.
   * @returns A new [[Policy]] clone of the original one, with the rule removed.
   * @since 1.6.0
   */
  function removeNoneOfRuleUrl(policy, rule) {
      return removeIri(policy, acp.noneOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set the rules restrincting the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the "None Of" rules,
   * they will not be granted access.
   * @param policy The [[Policy]] to which the rule should be added.
   * @param rules The rules the policy accepts.
   * @returns A new [[Policy]] clone of the original one, with the "Any Of" rules replaced.
   * @since 1.6.0
   */
  function setNoneOfRuleUrl(policy, rule) {
      return setIri(policy, acp.noneOf, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the "None Of" [[Rule]]s for the given [[Policy]]
   * @param policy The [[policy]] from which the rules should be read.
   * @returns A list of the forbidden [[Rule]]s
   * @since 1.6.0
   */
  function getNoneOfRuleUrlAll(policy) {
      return getIriAll(policy, acp.noneOf);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Initialise a new, empty [[Rule]].
   *
   * @param url URL that identifies this [[Rule]].
   * @since 1.6.0
   */
  function createRule(url) {
      const stringUrl = internal_toIriString(url);
      let ruleThing = createThing({ url: stringUrl });
      ruleThing = setUrl(ruleThing, rdf$2.type, acp.Rule);
      return ruleThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Initialise a new, empty [[ResourceRule]] for the given Resource.
   *
   * @param resourceWithAcr The Resource to which the new Rule is to apply.
   * @param name Name that identifies this [[Rule]].
   * @since 1.6.0
   */
  function createResourceRuleFor(resourceWithAcr, name) {
      const acr = internal_getAcr(resourceWithAcr);
      const url = new URL(getSourceUrl(acr));
      url.hash = `#${name}`;
      let ruleThing = createThing({ url: url.href });
      ruleThing = setUrl(ruleThing, rdf$2.type, acp.Rule);
      return ruleThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the [[Rule]] with the given URL from an [[SolidDataset]].
   *
   * @param ruleResource The Resource that contains the given [[Rule]].
   * @param url URL that identifies this [[Rule]].
   * @returns The requested [[Rule]], if it exists, or `null` if it does not.
   * @since 1.6.0
   */
  function getRule(ruleResource, url) {
      const foundThing = getThing(ruleResource, url);
      if (foundThing === null || getUrl(foundThing, rdf$2.type) !== acp.Rule) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the [[ResourceRule]] with the given name from an Resource's Access Control
   * Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains the given [[ResourceRule]].
   * @param name Name that identifies this [[ResourceRule]].
   * @returns The requested [[ResourceRule]], if it exists, or `null` if it does not.
   * @since 1.6.0
   */
  function getResourceRule(resourceWithAcr, name) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const url = new URL(acrUrl);
      url.hash = `#${name}`;
      const foundThing = getThing(acr, url.href);
      if (foundThing === null || !isRule(foundThing)) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Gets the [[Rule]]s from a [[SolidDataset]].
   *
   * @param ruleResource The Resource that contains (zero or more) [[Rule]]s.
   * @returns The [[Rule]]s contained in this resource.
   * @since 1.6.0
   */
  function getRuleAll(ruleResource) {
      const things = getThingAll(ruleResource);
      return things.filter(isRule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Gets the [[ResourceRule]]s from a Resource's Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains (zero or more) [[ResourceRule]]s.
   * @returns The [[ResourceRule]]s contained in this Resource's Access Control Resource.
   * @since 1.6.0
   */
  function getResourceRuleAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const things = getThingAll(acr);
      return things.filter(isRule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes the given [[Rule]] from the given [[SolidDataset]].
   *
   * @param ruleResource The Resource that contains (zero or more) [[Rule]]s.
   * @returns A new SolidDataset equal to the given Rule Resource, but without the given Rule.
   * @since 1.6.0
   */
  function removeRule(ruleResource, rule) {
      return removeThing(ruleResource, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes the given [[ResourceRule]] from the given Resource's Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains (zero or more) [[ResourceRule]]s.
   * @returns A new Resource equal to the given Resource, but without the given Rule in its ACR.
   * @since 1.6.0
   */
  function removeResourceRule(resourceWithAcr, rule) {
      const acr = internal_getAcr(resourceWithAcr);
      let ruleToRemove;
      if (typeof rule === "string") {
          try {
              new URL(rule);
              ruleToRemove = rule;
          }
          catch (e) {
              // If the given Rule to remove is the name of the Rule,
              // resolve it to its full URL — developers usually refer to either the
              // Rule itself, or by its name, as they do not have access to the ACR
              // directly.
              const ruleUrl = new URL(getSourceUrl(acr));
              ruleUrl.hash = `#${rule}`;
              ruleToRemove = ruleUrl.href;
          }
      }
      else if (isNamedNode$1(rule)) {
          ruleToRemove = internal_toIriString(rule);
      }
      else {
          ruleToRemove = asUrl(rule);
      }
      // Check whether the actual Rule (i.e. with the Rule type) exists:
      const matchingRule = getResourceRule(resourceWithAcr, new URL(ruleToRemove).hash.substring(1));
      if (matchingRule === null) {
          // No such Rule exists yet, so return the Resource+ACR unchanged:
          return resourceWithAcr;
      }
      const updatedAcr = removeThing(acr, matchingRule);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return updatedResource;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert the given [[Rule]] into the given [[SolidDataset]], replacing previous
   * instances of that Rule.
   *
   * @param ruleResource The Resource that contains (zero or more) [[Rule]]s.
   * @returns A new SolidDataset equal to the given Rule Resource, but with the given Rule.
   * @since 1.6.0
   */
  function setRule(ruleResource, rule) {
      return setThing(ruleResource, rule);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert the given [[ResourceRule]] into the given Resource's Access Control Resource,
   * replacing previous instances of that Rule.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains (zero or more) [[ResourceRule]]s.
   * @returns A new Resource equal to the given Resource, but with the given Rule in its ACR.
   * @since 1.6.0
   */
  function setResourceRule(resourceWithAcr, rule) {
      const acr = internal_getAcr(resourceWithAcr);
      const updatedAcr = setThing(acr, rule);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return updatedResource;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * List all the agents a [[Rule]] applies **directly** to. This will not include agents
   * that are part of a group the [[Rule]] applies to, nor will it include specific agent
   * classes, such as authenticated or public agents.
   *
   * @param rule The rule from which agents are read.
   * @returns A list of the WebIDs of agents included in the rule.
   * @since 1.6.0
   */
  function getAgentAll$1(rule) {
      return getIriAll(rule, acp.agent).filter((agent) => agent !== acp.PublicAgent &&
          agent !== acp.AuthenticatedAgent &&
          agent !== acp.CreatorAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrite the agents the [[Rule]] applies to with the provided agents.
   *
   * @param rule The rule for which agents are set.
   * @param agent The agent the rule should apply to.
   * @returns A copy of the input rule, applying to a different set of agents.
   * @since 1.6.0
   */
  function setAgent$1(rule, agent) {
      // Preserve the special agent classes authenticated and public, which we
      // don't want to overwrite with this function.
      const isPublic = hasPublic$1(rule);
      const isAuthenticated = hasAuthenticated$1(rule);
      const isCreator = hasCreator$1(rule);
      let result = setIri(rule, acp.agent, agent);
      // Restore public and authenticated
      if (isPublic) {
          result = setPublic$1(result);
      }
      if (isAuthenticated) {
          result = setAuthenticated$1(result);
      }
      if (isCreator) {
          result = setCreator$1(result);
      }
      return result;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Apply the [[Rule]] to an additional agent.
   *
   * @param rule The [[Rule]] to be applied to an additional agent.
   * @param agent The agent the [[Rule]] should apply to.
   * @returns A copy of the [[Rule]], applying to an additional agent.
   * @since 1.6.0
   */
  function addAgent$1(rule, agent) {
      return addIri(rule, acp.agent, agent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Prevent the [[Rule]] from applying to a given agent directly. This will not
   * remove the agent from any groups the rule applies to.
   *
   * @param rule The [[Rule]] that should no longer apply to a given agent.
   * @param agent The agent the rule should no longer apply to.
   * @returns A copy of the rule, not applying to the given agent.
   * @since 1.6.0
   */
  function removeAgent$1(rule, agent) {
      return removeIri(rule, acp.agent, agent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Lists all the groups a [[Rule]] applies to.
   *
   * @param rule The rule from which groups are read.
   * @returns A list of the [[URL]]'s of groups included in the rule.
   * @since 1.6.0
   * @deprecated Access Control Policies will no longer support vcard:Group. You can re-use a Rule listing multiple Agents to get the same functionality.
   */
  function getGroupAll(rule) {
      return getIriAll(rule, acp.group);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrite the groups the [[Rule]] applies to with the provided groups.
   *
   * @param rule The rule for which groups are set.
   * @param group The group the rule should apply to.
   * @returns A copy of the input rule, applying to a different set of groups.
   * @since 1.6.0
   * @deprecated Access Control Policies will no longer support vcard:Group. You can re-use a Rule listing multiple Agents to get the same functionality.
   */
  function setGroup(rule, group) {
      return setIri(rule, acp.group, group);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Apply the [[Rule]] to an additional group.
   *
   * @param rule The [[Rule]] to be applied to an additional group.
   * @param agent The group the [[Rule]] should apply to.
   * @returns A copy of the [[Rule]], applying to an additional group.
   * @since 1.6.0
   * @deprecated Access Control Policies will no longer support vcard:Group. You can re-use a Rule listing multiple Agents to get the same functionality.
   */
  function addGroup(rule, group) {
      return addIri(rule, acp.group, group);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Prevent the [[Rule]] from applying to a given group.
   *
   * @param rule The [[Rule]] that should no longer apply to a given group.
   * @param agent The group the rule should no longer apply to.
   * @returns A copy of the rule, not applying to the given group.
   * @since 1.6.0
   * @deprecated Access Control Policies will no longer support vcard:Group. You can re-use a Rule listing multiple Agents to get the same functionality.
   */
  function removeGroup(rule, group) {
      return removeIri(rule, acp.group, group);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the rule applies to any agent.
   *
   * @param rule The rule checked for public access.
   * @returns Whether the rule applies to any agent or not.
   * @since 1.6.0
   */
  function hasPublic$1(rule) {
      return (getIriAll(rule, acp.agent).filter((agent) => agent === acp.PublicAgent)
          .length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Rule to apply to any Agent.
   *
   * @param rule The rule being modified.
   * @returns A copy of the rule, updated to apply to any agent.
   * @since 1.6.0
   */
  function setPublic$1(rule) {
      // The second argument should not be part of the function signature,
      // so it's not in the parameter list:
      // eslint-disable-next-line prefer-rest-params
      if (typeof arguments === "object" && typeof arguments[1] === "boolean") {
          throw new Error("The function `setPublic` no longer takes a second parameter. It is now used together with `removePublic` instead.");
      }
      return addIri(rule, acp.agent, acp.PublicAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Rule to no longer apply to any Agent.
   *
   * @param rule The rule being modified.
   * @returns A copy of the rule, updated to no longer apply to any agent.
   * @since 1.6.0
   */
  function removePublic$1(rule) {
      return removeIri(rule, acp.agent, acp.PublicAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the rule applies to any authenticated agent.
   *
   * @param rule The rule checked for authenticated access.
   * @returns Whether the rule applies to any authenticated agent or not.
   * @since 1.6.0
   */
  function hasAuthenticated$1(rule) {
      return (getIriAll(rule, acp.agent).filter((agent) => agent === acp.AuthenticatedAgent).length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Rule to apply to any authenticated Agent.
   *
   * @param rule The rule being modified.
   * @returns A copy of the rule, updated to apply to any authenticated Agent.
   * @since 1.6.0
   */
  function setAuthenticated$1(rule) {
      // The second argument should not be part of the function signature,
      // so it's not in the parameter list:
      // eslint-disable-next-line prefer-rest-params
      if (typeof arguments === "object" && typeof arguments[1] === "boolean") {
          throw new Error("The function `setAuthenticated` no longer takes a second parameter. It is now used together with `removeAuthenticated` instead.");
      }
      return addIri(rule, acp.agent, acp.AuthenticatedAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Rule to no longer apply to any authenticated Agent.
   *
   * @param rule The rule being modified.
   * @returns A copy of the rule, updated to apply/not apply to any authenticated agent.
   * @since 1.6.0
   */
  function removeAuthenticated$1(rule) {
      return removeIri(rule, acp.agent, acp.AuthenticatedAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the rule applies to the creator of the Resource.
   *
   * @param rule The rule checked for authenticated access.
   * @returns Whether the rule applies to the creator of the Resource or not.
   * @since 1.6.0
   */
  function hasCreator$1(rule) {
      return (getIriAll(rule, acp.agent).filter((agent) => agent === acp.CreatorAgent)
          .length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Rule to apply to the creator of a Resource.
   *
   * @param rule The rule being modified.
   * @returns A copy of the rule, updated to apply to the creator of a Resource.
   * @since 1.6.0
   */
  function setCreator$1(rule) {
      // The second argument should not be part of the function signature,
      // so it's not in the parameter list:
      // eslint-disable-next-line prefer-rest-params
      if (typeof arguments === "object" && typeof arguments[1] === "boolean") {
          throw new Error("The function `setCreator` no longer takes a second parameter. It is now used together with `removeCreator` instead.");
      }
      return addIri(rule, acp.agent, acp.CreatorAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Rule to no longer apply to the creator of a Resource.
   *
   * @param rule The rule being modified.
   * @returns A copy of the rule, updated to apply/not apply to the creator of a Resource.
   * @since 1.6.0
   */
  function removeCreator$1(rule) {
      return removeIri(rule, acp.agent, acp.CreatorAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * List all the clients a [[Rule]] applies **directly** to. This will not include
   * specific client classes, such as public clients.
   *
   * @param rule The rule from which clients are read.
   * @returns A list of the WebIDs of clients included in the rule.
   * @since 1.6.0
   */
  function getClientAll$1(rule) {
      return getIriAll(rule, acp.client).filter((client) => client !== solid.PublicOidcClient);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrite the clients the [[Rule]] applies to with the provided Client.
   *
   * @param rule The rule for which clients are set.
   * @param client The Client the rule should apply to.
   * @returns A copy of the input rule, applying to a different set of Clients.
   * @since 1.6.0
   */
  function setClient$1(rule, client) {
      // Preserve the special "any client" class, which we
      // don't want to overwrite with this function.
      const anyClientEnabled = hasAnyClient$1(rule);
      let result = setIri(rule, acp.client, client);
      // Restore the "any client" class
      if (anyClientEnabled) {
          result = setAnyClient$1(result);
      }
      return result;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Apply the [[Rule]] to an additional Client.
   *
   * @param rule The [[Rule]] to be applied to an additional Client.
   * @param client The Client the [[Rule]] should apply to.
   * @returns A copy of the [[Rule]], applying to an additional Client.
   * @since 1.6.0
   */
  function addClient$1(rule, client) {
      return addIri(rule, acp.client, client);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Prevent the [[Rule]] from applying to a given Client directly.
   *
   * @param rule The [[Rule]] that should no longer apply to a given Client.
   * @param client The Client the rule should no longer apply to.
   * @returns A copy of the rule, not applying to the given Client.
   * @since 1.6.0
   */
  function removeClient$1(rule, client) {
      return removeIri(rule, acp.client, client);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the rule applies to any client, i.e. all the applications
   * regardless of their identifier.
   *
   * @param rule The rule checked for authenticated access.
   * @returns Whether the rule applies to public clients.
   * @since 1.6.0
   */
  function hasAnyClient$1(rule) {
      return (getIriAll(rule, acp.client).filter((client) => client === solid.PublicOidcClient).length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Make the [[Rule]] apply to any client application.
   *
   * @param rule The rule for which clients are set.
   * @returns A copy of the rule, updated to apply to any client
   * @since 1.6.0
   */
  function setAnyClient$1(rule) {
      return addIri(rule, acp.client, solid.PublicOidcClient);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Make the [[Rule]] no longer apply to any client application.
   *
   * @param rule The rule for which clients are set.
   * @returns A copy of the rule, updated to no longer apply to any client
   * @since 1.6.0
   */
  function removeAnyClient$1(rule) {
      return removeIri(rule, acp.client, solid.PublicOidcClient);
  }
  /**
   * Gets a human-readable representation of the given [[Rule]] to aid debugging.
   *
   * Note that changes to the exact format of the return value are not considered a breaking change;
   * it is intended to aid in debugging, not as a serialisation method that can be reliably parsed.
   *
   * @param rule The Rule to get a human-readable representation of.
   * @since 1.6.0
   */
  function ruleAsMarkdown(rule) {
      let markdown = `## Rule: ${asUrl(rule)}\n\n`;
      let targetEnumeration = "";
      if (hasPublic$1(rule)) {
          targetEnumeration += "- Everyone\n";
      }
      if (hasAuthenticated$1(rule)) {
          targetEnumeration += "- All authenticated agents\n";
      }
      if (hasCreator$1(rule)) {
          targetEnumeration += "- The creator of this resource\n";
      }
      if (hasAnyClient$1(rule)) {
          targetEnumeration += "- Users of any client application\n";
      }
      const targetAgents = getAgentAll$1(rule);
      if (targetAgents.length > 0) {
          targetEnumeration += "- The following agents:\n  - ";
          targetEnumeration += targetAgents.join("\n  - ") + "\n";
      }
      const targetGroups = getGroupAll(rule);
      if (targetGroups.length > 0) {
          targetEnumeration += "- Members of the following groups:\n  - ";
          targetEnumeration += targetGroups.join("\n  - ") + "\n";
      }
      const targetClients = getClientAll$1(rule);
      if (targetClients.length > 0) {
          targetEnumeration += "- Users of the following client applications:\n  - ";
          targetEnumeration += targetClients.join("\n  - ") + "\n";
      }
      markdown +=
          targetEnumeration.length > 0
              ? "This rule applies to:\n" + targetEnumeration
              : "<empty>\n";
      return markdown;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * @param thing the [[Thing]] to check to see if it's an ACP Policy or not
   */
  function isPolicy(thing) {
      return getIriAll(thing, rdf$2.type).includes(acp.Policy);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Initialise a new, empty [[Policy]].
   *
   * @param url URL that identifies this Policy.
   * @since 1.6.0
   */
  function createPolicy(url) {
      const stringUrl = internal_toIriString(url);
      let policyThing = createThing({ url: stringUrl });
      policyThing = setUrl(policyThing, rdf$2.type, acp.Policy);
      return policyThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the [[Policy]] with the given URL from an [[SolidDataset]].
   *
   * @param policyResource The Resource that contains the given Policy.
   * @param url URL that identifies this Policy.
   * @returns The requested Policy, if it exists, or `null` if it does not.
   * @since 1.6.0
   */
  function getPolicy(policyResource, url) {
      const foundThing = getThing(policyResource, url);
      if (foundThing === null || !isPolicy(foundThing)) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get all [[Policy]]'s in a given [[SolidDataset]].
   *
   * @param policyResource The Resource that contains Access Policies.
   * @since 1.6.0
   */
  function getPolicyAll(policyResource) {
      const foundThings = getThingAll(policyResource);
      const foundPolicies = foundThings.filter((thing) => !isThingLocal(thing) && isPolicy(thing));
      return foundPolicies;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove the given [[Policy]] from the given [[SolidDataset]].
   *
   * @param policyResource The Resource that contains Access Policies.
   * @param policy The Policy to remove from the resource.
   * @since 1.6.0
   */
  function removePolicy(policyResource, policy) {
      return removeThing(policyResource, policy);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert the given [[Policy]] into the given [[SolidDataset]], replacing previous instances of that Policy.
   *
   * @param policyResource The Resource that contains Access Policies.
   * @param policy The Policy to insert into the Resource.
   * @returns A new dataset equal to the given resource, but with the given Policy.
   * @since 1.6.0
   */
  function setPolicy(policyResource, policy) {
      return setThing(policyResource, policy);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]] and a set of [[AccessModes]], return a new Policy based on the given
   * Policy, but with the given Access Modes allowed on it.
   *
   * @param policy The Policy on which to set the modes to allow.
   * @param modes Modes to allow for this Policy.
   * @since Not released yet.
   */
  function setAllowModesV2(policy, modes) {
      let newPolicy = removeAll(policy, acp.allow);
      if (modes.read === true) {
          newPolicy = addIri(newPolicy, acp.allow, internal_accessModeIriStrings.read);
      }
      if (modes.append === true) {
          newPolicy = addIri(newPolicy, acp.allow, internal_accessModeIriStrings.append);
      }
      if (modes.write === true) {
          newPolicy = addIri(newPolicy, acp.allow, internal_accessModeIriStrings.write);
      }
      return newPolicy;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]] and a set of [[AccessModes]], return a new Policy based on the given
   * Policy, but with the given Access Modes allowed on it.
   *
   * @param policy The Policy on which to set the modes to allow.
   * @param modes Modes to allow for this Policy.
   * @since 1.6.0
   * @deprecated The Access Control Policies proposal will be updated to use a different vocabulary for allow- and deny-modes. To be compatible with servers that implement that, use [[setAllowModesV2]].
   */
  function setAllowModesV1(policy, modes) {
      let newPolicy = removeAll(policy, acp.allow);
      if (modes.read === true) {
          newPolicy = addIri(newPolicy, acp.allow, acp.Read);
      }
      if (modes.append === true) {
          newPolicy = addIri(newPolicy, acp.allow, acp.Append);
      }
      if (modes.write === true) {
          newPolicy = addIri(newPolicy, acp.allow, acp.Write);
      }
      return newPolicy;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]], return which [[AccessModes]] it allows.
   *
   * @param policy The Policy for which you want to know the Access Modes it allows.
   * @since Not released yet.
   */
  function getAllowModesV2(policy) {
      const allowedModes = getIriAll(policy, acp.allow);
      return {
          read: allowedModes.includes(internal_accessModeIriStrings.read),
          append: allowedModes.includes(internal_accessModeIriStrings.append),
          write: allowedModes.includes(internal_accessModeIriStrings.write),
      };
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]], return which [[AccessModes]] it allows.
   *
   * @param policy The Policy for which you want to know the Access Modes it allows.
   * @since 1.6.0
   * @deprecated The Access Control Policies proposal will be updated to use a different vocabulary for allow- and deny-modes. To be compatible with servers that implement that, use [[getAllowModesV2]].
   */
  function getAllowModesV1(policy) {
      const allowedModes = getIriAll(policy, acp.allow);
      return {
          read: allowedModes.includes(acp.Read),
          append: allowedModes.includes(acp.Append),
          write: allowedModes.includes(acp.Write),
      };
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]] and a set of [[AccessModes]], return a new Policy based on the given
   * Policy, but with the given Access Modes disallowed on it.
   *
   * @param policy The Policy on which to set the modes to disallow.
   * @param modes Modes to disallow for this Policy.
   * @since Not released yet.
   */
  function setDenyModesV2(policy, modes) {
      let newPolicy = removeAll(policy, acp.deny);
      if (modes.read === true) {
          newPolicy = addIri(newPolicy, acp.deny, internal_accessModeIriStrings.read);
      }
      if (modes.append === true) {
          newPolicy = addIri(newPolicy, acp.deny, internal_accessModeIriStrings.append);
      }
      if (modes.write === true) {
          newPolicy = addIri(newPolicy, acp.deny, internal_accessModeIriStrings.write);
      }
      return newPolicy;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]] and a set of [[AccessModes]], return a new Policy based on the given
   * Policy, but with the given Access Modes disallowed on it.
   *
   * @param policy The Policy on which to set the modes to disallow.
   * @param modes Modes to disallow for this Policy.
   * @since 1.6.0
   * @deprecated The Access Control Policies proposal will be updated to use a different vocabulary for allow- and deny-modes. To be compatible with servers that implement that, use [[setDenyModesV2]].
   */
  function setDenyModesV1(policy, modes) {
      let newPolicy = removeAll(policy, acp.deny);
      if (modes.read === true) {
          newPolicy = addIri(newPolicy, acp.deny, acp.Read);
      }
      if (modes.append === true) {
          newPolicy = addIri(newPolicy, acp.deny, acp.Append);
      }
      if (modes.write === true) {
          newPolicy = addIri(newPolicy, acp.deny, acp.Write);
      }
      return newPolicy;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]], return which [[AccessModes]] it disallows.
   *
   * @param policy The Policy on which you want to know the Access Modes it disallows.
   * @since Not released yet.
   */
  function getDenyModesV2(policy) {
      const deniedModes = getIriAll(policy, acp.deny);
      return {
          read: deniedModes.includes(internal_accessModeIriStrings.read),
          append: deniedModes.includes(internal_accessModeIriStrings.append),
          write: deniedModes.includes(internal_accessModeIriStrings.write),
      };
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Given a [[Policy]], return which [[AccessModes]] it disallows.
   *
   * @param policy The Policy on which you want to know the Access Modes it disallows.
   * @since 1.6.0
   * @deprecated The Access Control Policies proposal will be updated to use a different vocabulary for allow- and deny-modes. To be compatible with servers that implement that, use [[getDenyModesV2]].
   */
  function getDenyModesV1(policy) {
      const deniedModes = getIriAll(policy, acp.deny);
      return {
          read: deniedModes.includes(acp.Read),
          append: deniedModes.includes(acp.Append),
          write: deniedModes.includes(acp.Write),
      };
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Initialise a new, empty [[ResourcePolicy]] for the given Resource.
   *
   * @param resourceWithAcr The Resource to which the Policy is to apply.
   * @param name The name that identifies this Policy.
   * @since 1.6.0
   */
  function createResourcePolicyFor(resourceWithAcr, name) {
      const acr = internal_getAcr(resourceWithAcr);
      const url = new URL(getSourceUrl(acr));
      url.hash = `#${name}`;
      let policyThing = createThing({ url: url.href });
      policyThing = setUrl(policyThing, rdf$2.type, acp.Policy);
      return policyThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the [[ResourcePolicy]] with the given name that applies to a Resource
   * from its Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose ACR contains the given Policy.
   * @param name The name that identifies this Policy.
   * @returns The requested Policy, if it exists and applies to the given Resource, or `null` if it does not.
   * @since 1.6.0
   */
  function getResourcePolicy(resourceWithAcr, name) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const url = new URL(acrUrl);
      url.hash = `#${name}`;
      const foundThing = getThing(acr, url.href);
      if (!getPolicyUrlAll(resourceWithAcr).includes(url.href) ||
          foundThing === null ||
          !isPolicy(foundThing)) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the [[ResourcePolicy]] with the given name that applies to a Resource's
   * Access Control Resource from that Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose ACR contains the given Policy.
   * @param name The name that identifies this Policy.
   * @returns The requested Policy, if it exists and applies to the Resource's ACR, or `null` if it does not.
   * @since 1.6.0
   */
  function getResourceAcrPolicy(resourceWithAcr, name) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const url = new URL(acrUrl);
      url.hash = `#${name}`;
      const foundThing = getThing(acr, url.href);
      if (!getAcrPolicyUrlAll(resourceWithAcr).includes(url.href) ||
          foundThing === null ||
          !isPolicy(foundThing)) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get all [[ResourcePolicy]]'s that apply to a Resource in its Access Control
   * Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains Access Policies applying to it.
   * @since 1.6.0
   */
  function getResourcePolicyAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const policyUrls = getPolicyUrlAll(resourceWithAcr);
      const foundThings = policyUrls.map((policyUrl) => getThing(acr, policyUrl));
      const foundPolicies = foundThings.filter((thing) => thing !== null && isPolicy(thing));
      return foundPolicies;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get all [[ResourcePolicy]]'s that apply to a given Resource's Access Control
   * Resource from that Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains Access Policies.
   * @since 1.6.0
   */
  function getResourceAcrPolicyAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const policyUrls = getAcrPolicyUrlAll(resourceWithAcr);
      const foundThings = policyUrls.map((policyUrl) => getThing(acr, policyUrl));
      const foundPolicies = foundThings.filter((thing) => thing !== null && isPolicy(thing));
      return foundPolicies;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove the given [[ResourcePolicy]] from the given Resource's Access Control
   * Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains Access Policies.
   * @param policy The Policy to remove from the Resource's Access Control Resource.
   * @since 1.6.0
   */
  function removeResourcePolicy(resourceWithAcr, policy) {
      const acr = internal_getAcr(resourceWithAcr);
      let policyToRemove = policy;
      if (typeof policyToRemove === "string") {
          try {
              new URL(policyToRemove);
          }
          catch (e) {
              // If the given Policy to remove is the name of the Policy,
              // resolve it to its full URL — developers usually refer to either the
              // Policy itself, or by its name, as they do not have access to the ACR
              // directly.
              const policyUrl = new URL(getSourceUrl(acr));
              policyUrl.hash = `#${policy}`;
              policyToRemove = policyUrl.href;
          }
      }
      let policyUrlString;
      if (typeof policyToRemove === "string") {
          policyUrlString = policyToRemove;
      }
      else if (isNamedNode$1(policyToRemove)) {
          policyUrlString = internal_toIriString(policyToRemove);
      }
      else {
          policyUrlString = asUrl(policyToRemove, getSourceUrl(acr));
      }
      // Check whether the actual Policy (i.e. with the Policy type) exists:
      const matchingRule = getResourcePolicy(resourceWithAcr, new URL(policyUrlString).hash.substring(1));
      if (matchingRule === null) {
          // No such Policy exists yet, so return the Resource+ACR unchanged:
          return resourceWithAcr;
      }
      const updatedAcr = removeThing(acr, policyToRemove);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return removePolicyUrl(updatedResource, policyUrlString);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove the given [[ResourcePolicy]] that applies to a given Resource's Access
   * Control Resource from that Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains Access Policies.
   * @param policy The ACR Policy to remove from the Resource's Access Control Resource.
   * @since 1.6.0
   */
  function removeResourceAcrPolicy(resourceWithAcr, policy) {
      const acr = internal_getAcr(resourceWithAcr);
      let policyToRemove = policy;
      if (typeof policyToRemove === "string") {
          try {
              new URL(policyToRemove);
          }
          catch (e) {
              // If the given Policy to remove is the name of the Policy,
              // resolve it to its full URL — developers usually refer to either the
              // Policy itself, or by its name, as they do not have access to the ACR
              // directly.
              const policyUrl = new URL(getSourceUrl(acr));
              policyUrl.hash = `#${policy}`;
              policyToRemove = policyUrl.href;
          }
      }
      let policyUrlString;
      if (typeof policyToRemove === "string") {
          policyUrlString = policyToRemove;
      }
      else if (isNamedNode$1(policyToRemove)) {
          policyUrlString = internal_toIriString(policyToRemove);
      }
      else {
          policyUrlString = asUrl(policyToRemove, getSourceUrl(acr));
      }
      // Check whether the actual Policy (i.e. with the Policy type) exists:
      const matchingRule = getResourceAcrPolicy(resourceWithAcr, new URL(policyUrlString).hash.substring(1));
      if (matchingRule === null) {
          // No such Policy exists yet, so return the Resource+ACR unchanged:
          return resourceWithAcr;
      }
      const updatedAcr = removeThing(acr, policyToRemove);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return removeAcrPolicyUrl(updatedResource, policyUrlString);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert the given [[ResourcePolicy]] into the given Resource's Acccess Control
   * Resource, replacing previous instances of that Policy.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains Access Policies.
   * @param policy The Policy to insert into the Resource's Access Control Resource.
   * @returns A new Resource equal to the given Resource, but with the given Policy in its Access Control Resource.
   * @since 1.6.0
   */
  function setResourcePolicy(resourceWithAcr, policy) {
      const acr = internal_getAcr(resourceWithAcr);
      const updatedAcr = setThing(acr, policy);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      const policyUrl = asUrl(policy, getSourceUrl(acr));
      return addPolicyUrl(updatedResource, policyUrl);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert the given [[ResourcePolicy]] into the given Resource's Acccess Control
   * Resource, replacing previous instances of that Policy, to apply to the Access
   * Control Resource itself.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains Access Policies.
   * @param policy The Policy to insert into the Resource's Access Control Resource.
   * @returns A new Resource equal to the given Resource, but with the given Policy in its Access Control Resource, applying to that Access Control Resource.
   * @since 1.6.0
   */
  function setResourceAcrPolicy(resourceWithAcr, policy) {
      const acr = internal_getAcr(resourceWithAcr);
      const updatedAcr = setThing(acr, policy);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      const policyUrl = asUrl(policy, getSourceUrl(acr));
      return addAcrPolicyUrl(updatedResource, policyUrl);
  }
  /**
   * Gets a human-readable representation of the given [[Policy]] to aid debugging.
   *
   * Note that changes to the exact format of the return value are not considered a breaking change;
   * it is intended to aid in debugging, not as a serialisation method that can be reliably parsed.
   *
   * @param policy The Policy to get a human-readable representation of.
   * @since 1.6.0
   */
  function policyAsMarkdown(policy) {
      function getStatus(allow, deny) {
          if (deny) {
              return "denied";
          }
          if (allow) {
              return "allowed";
          }
          return "unspecified";
      }
      const allowModes = getAllowModesV1(policy);
      const denyModes = getDenyModesV1(policy);
      let markdown = `## Policy: ${asUrl(policy)}\n\n`;
      markdown += `- Read: ${getStatus(allowModes.read, denyModes.read)}\n`;
      markdown += `- Append: ${getStatus(allowModes.append, denyModes.append)}\n`;
      markdown += `- Write: ${getStatus(allowModes.write, denyModes.write)}\n`;
      const allOfRules = getAllOfRuleUrlAll(policy);
      const anyOfRules = getAnyOfRuleUrlAll(policy);
      const noneOfRules = getNoneOfRuleUrlAll(policy);
      if (allOfRules.length === 0 &&
          anyOfRules.length === 0 &&
          noneOfRules.length === 0) {
          markdown += "\n<no rules specified yet>\n";
      }
      if (allOfRules.length > 0) {
          markdown += "\nAll of these rules should match:\n";
          markdown += "- " + allOfRules.join("\n- ") + "\n";
      }
      if (anyOfRules.length > 0) {
          markdown += "\nAt least one of these rules should match:\n";
          markdown += "- " + anyOfRules.join("\n- ") + "\n";
      }
      if (noneOfRules.length > 0) {
          markdown += "\nNone of these rules should match:\n";
          markdown += "- " + noneOfRules.join("\n- ") + "\n";
      }
      return markdown;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   *
   * ```{warning}
   * Do not use this function in production code.  For use in **unit tests** that require a
   * [[AccessControlResource]].
   * ```
   *
   * Initialises a new empty Access Control Resource for a given Resource for use
   * in **unit tests**.
   *
   * @param resourceUrl The URL of the Resource to which the mocked ACR should apply.
   * @returns The mocked empty Access Control Resource for the given Resource.
   * @since 1.6.0
   */
  function mockAcrFor(resourceUrl) {
      const acrUrl = new URL("access-control-resource", resourceUrl).href;
      const acr = Object.assign(Object.assign({}, mockSolidDatasetFrom(acrUrl)), { accessTo: resourceUrl });
      return acr;
  }
  /**
   * ```{warning}
   * Do not use this function in production code.  For use in **unit tests** that require a
   * Resource with an [[AccessControlResource]].
   * ```
   *
   * Attaches an Access Control Resource to a given [[SolidDataset]] for use
   * in **unit tests**; e.g., unit tests that call [[getPolicyUrlAll]].
   *
   * @param resource The Resource to mock up with a new resource ACL.
   * @param accessControlResource The Access Control Resource to attach to the given Resource.
   * @returns The input Resource with an empty resource ACL attached.
   * @since 1.6.0
   */
  function addMockAcrTo(resource, accessControlResource = mockAcrFor(getSourceUrl(resource))) {
      const resourceWithAcr = Object.assign(internal_cloneResource(resource), {
          internal_acp: {
              acr: accessControlResource,
              aprs: {},
          },
      });
      return resourceWithAcr;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const v2AcpFunctions = {
      getFileWithAccessDatasets,
      getFileWithAcr,
      getReferencedPolicyUrlAll,
      getResourceInfoWithAccessDatasets,
      getResourceInfoWithAcr,
      getSolidDatasetWithAccessDatasets,
      getSolidDatasetWithAcr,
      hasAccessibleAcr,
      saveAcrFor,
  };
  const v2ControlFunctions = {
      acrAsMarkdown,
      addAcrPolicyUrl,
      addMemberAcrPolicyUrl,
      addMemberPolicyUrl,
      addPolicyUrl,
      getAcrPolicyUrlAll,
      getMemberAcrPolicyUrlAll,
      getMemberPolicyUrlAll,
      getPolicyUrlAll,
      hasLinkedAcr,
      removeAcrPolicyUrl,
      removeAcrPolicyUrlAll,
      removeMemberAcrPolicyUrl,
      removeMemberAcrPolicyUrlAll,
      removeMemberPolicyUrl,
      removeMemberPolicyUrlAll,
      removePolicyUrl,
      removePolicyUrlAll,
  };
  const v2PolicyFunctions = {
      createPolicy,
      getAllowModes: getAllowModesV1,
      getDenyModes: getDenyModesV1,
      getPolicy,
      getPolicyAll,
      policyAsMarkdown,
      removePolicy,
      setAllowModes: setAllowModesV1,
      setDenyModes: setDenyModesV1,
      setPolicy,
  };
  const v2RuleFunctions = {
      addAgent: addAgent$1,
      addForbiddenRuleUrl: addNoneOfRuleUrl,
      addGroup,
      addOptionalRuleUrl: addAnyOfRuleUrl,
      addRequiredRuleUrl: addAllOfRuleUrl,
      createRule,
      getAgentAll: getAgentAll$1,
      getForbiddenRuleUrlAll: getNoneOfRuleUrlAll,
      getGroupAll,
      getOptionalRuleUrlAll: getAnyOfRuleUrlAll,
      getRequiredRuleUrlAll: getAllOfRuleUrlAll,
      getRule,
      getRuleAll,
      hasAuthenticated: hasAuthenticated$1,
      hasCreator: hasCreator$1,
      hasPublic: hasPublic$1,
      removeAgent: removeAgent$1,
      removeForbiddenRuleUrl: removeNoneOfRuleUrl,
      removeGroup,
      removeOptionalRuleUrl: removeAnyOfRuleUrl,
      removeRequiredRuleUrl: removeAllOfRuleUrl,
      removeRule,
      ruleAsMarkdown,
      setAgent: setAgent$1,
      setForbiddenRuleUrl: setNoneOfRuleUrl,
      setGroup,
      setOptionalRuleUrl: setAnyOfRuleUrl,
      setRequiredRuleUrl: setAllOfRuleUrl,
      setRule,
  };
  const v2MockFunctions = {
      addMockAcrTo,
      mockAcrFor,
  };
  /* istanbul ignore next Not a supported public API: */
  /** @deprecated Replaced by [[setPublic]] */
  function previousSetPublicSignature(rule, enable) {
      return enable ? setPublic$1(rule) : removePublic$1(rule);
  }
  /* istanbul ignore next Not a supported public API: */
  /** @deprecated Replaced by [[setAuthenticated]] */
  function previousSetAuthenticatedSignature(rule, enable) {
      return enable ? setAuthenticated$1(rule) : removeAuthenticated$1(rule);
  }
  /* istanbul ignore next Not a supported public API: */
  /** @deprecated Replaced by [[setCreator]] */
  function previousSetCreatorSignature(rule, enable) {
      return enable ? setCreator$1(rule) : removeCreator$1(rule);
  }
  const deprecatedFunctions$1 = {
      /** @deprecated This misspelling was included accidentally. The correct function is [[getForbiddenRuleUrlAll]]. */
      getForbiddenRuleurlAll: getNoneOfRuleUrlAll,
      setPublic: previousSetPublicSignature,
      setAuthenticated: previousSetAuthenticatedSignature,
      setCreator: previousSetCreatorSignature,
  };
  /**
   * @hidden
   * @deprecated Replaced by [[acp_v3]].
   */
  const acp_v2 = Object.assign(Object.assign(Object.assign(Object.assign(Object.assign(Object.assign({}, v2AcpFunctions), v2ControlFunctions), v2PolicyFunctions), v2RuleFunctions), v2MockFunctions), deprecatedFunctions$1);

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const v1AcpFunctions = {
      getFileWithAccessDatasets,
      getFileWithAcr,
      getReferencedPolicyUrlAll,
      getResourceInfoWithAccessDatasets,
      getResourceInfoWithAcr,
      getSolidDatasetWithAccessDatasets,
      getSolidDatasetWithAcr,
      hasAccessibleAcr,
      saveAcrFor,
  };
  const v1PolicyFunctions = {
      createPolicy,
      getAllowModes: getAllowModesV1,
      getDenyModes: getDenyModesV1,
      getPolicy,
      getPolicyAll,
      policyAsMarkdown,
      removePolicy,
      setAllowModes: setAllowModesV1,
      setDenyModes: setDenyModesV1,
      setPolicy,
  };
  const v1RuleFunctions = {
      addAgent: addAgent$1,
      addForbiddenRuleUrl: addNoneOfRuleUrl,
      addGroup,
      addOptionalRuleUrl: addAnyOfRuleUrl,
      addRequiredRuleUrl: addAllOfRuleUrl,
      createRule,
      getAgentAll: getAgentAll$1,
      getForbiddenRuleUrlAll: getNoneOfRuleUrlAll,
      getGroupAll,
      getOptionalRuleUrlAll: getAnyOfRuleUrlAll,
      getRequiredRuleUrlAll: getAllOfRuleUrlAll,
      getRule,
      getRuleAll,
      hasAuthenticated: hasAuthenticated$1,
      hasCreator: hasCreator$1,
      hasPublic: hasPublic$1,
      removeAgent: removeAgent$1,
      removeForbiddenRuleUrl: removeNoneOfRuleUrl,
      removeGroup,
      removeOptionalRuleUrl: removeAnyOfRuleUrl,
      removeRequiredRuleUrl: removeAllOfRuleUrl,
      removeRule,
      ruleAsMarkdown,
      setAgent: setAgent$1,
      setForbiddenRuleUrl: setNoneOfRuleUrl,
      setGroup,
      setOptionalRuleUrl: setAnyOfRuleUrl,
      setRequiredRuleUrl: setAllOfRuleUrl,
      setRule,
  };
  const v1MockFunctions = {
      addMockAcrTo,
      mockAcrFor,
  };
  const v1ControlFunctions = {
      hasLinkedAcr,
      addAcrPolicyUrl,
      addMemberAcrPolicyUrl,
      getAcrPolicyUrlAll,
      getMemberAcrPolicyUrlAll,
      removeAcrPolicyUrl,
      removeAcrPolicyUrlAll,
      removeMemberAcrPolicyUrl,
      removeMemberAcrPolicyUrlAll,
  };
  const deprecatedFunctions = {
      createControl: internal_createControl,
      getControl: internal_getControl,
      getAllControl: internal_getControlAll,
      getControlAll: internal_getControlAll,
      setControl: internal_setControl,
      removeControl: removeControl,
      addPolicyUrl: internal_addPolicyUrl,
      getPolicyUrlAll: internal_getPolicyUrlAll,
      removePolicyUrl: internal_removePolicyUrl,
      removePolicyUrlAll: internal_removePolicyUrlAll,
      addMemberPolicyUrl: internal_addMemberPolicyUrl,
      getMemberPolicyUrlAll: internal_getMemberPolicyUrlAll,
      removeMemberPolicyUrl: internal_getMemberPolicyUrlAll,
      removeMemberPolicyUrlAll: internal_removeMemberPolicyUrlAll,
      /** @deprecated This misspelling was included accidentally. The correct function is [[getForbiddenRuleUrlAll]]. */
      getForbiddenRuleurlAll: getNoneOfRuleUrlAll,
      setPublic: previousSetPublicSignature,
      setAuthenticated: previousSetAuthenticatedSignature,
      setCreator: previousSetCreatorSignature,
  };
  /**
   * @hidden
   * @deprecated Replaced by [[acp_v2]].
   */
  const acp_v1 = Object.assign(Object.assign(Object.assign(Object.assign(Object.assign(Object.assign({}, v1AcpFunctions), v1PolicyFunctions), v1RuleFunctions), v1MockFunctions), v1ControlFunctions), deprecatedFunctions);
  /**
   * ```{note} The Web Access Control specification is not yet finalised. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Remove an [[Control]] from the [[AccessControlResource]] of a Resource.
   *
   * @param withAccessControlResource A Resource with the Access Control Resource from which to remove an Access Control.
   * @param control The [[Control]] to remove from the given Access Control Resource.
   * @returns The given Resource with a new Access Control Resource equal to the original Access Control Resource, excluding the given Access Control.
   * @hidden Developers don't need to care about initialising Controls - they can just add Policies directly.
   * @deprecated
   */
  function removeControl(withAccessControlResource, control) {
      const acr = internal_getAcr(withAccessControlResource);
      const updatedAcr = removeThing(acr, control);
      const updatedResource = internal_setAcr(withAccessControlResource, updatedAcr);
      return updatedResource;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const v3AcpFunctions = {
      getFileWithAccessDatasets,
      getFileWithAcr,
      getReferencedPolicyUrlAll,
      getResourceInfoWithAccessDatasets,
      getResourceInfoWithAcr,
      getSolidDatasetWithAccessDatasets,
      getSolidDatasetWithAcr,
      hasAccessibleAcr,
      saveAcrFor,
  };
  const v3ControlFunctions = {
      acrAsMarkdown,
      addAcrPolicyUrl,
      addMemberAcrPolicyUrl,
      addMemberPolicyUrl,
      addPolicyUrl,
      getAcrPolicyUrlAll,
      getMemberAcrPolicyUrlAll,
      getMemberPolicyUrlAll,
      getPolicyUrlAll,
      hasLinkedAcr,
      removeAcrPolicyUrl,
      removeAcrPolicyUrlAll,
      removeMemberAcrPolicyUrl,
      removeMemberAcrPolicyUrlAll,
      removeMemberPolicyUrl,
      removeMemberPolicyUrlAll,
      removePolicyUrl,
      removePolicyUrlAll,
  };
  const v3PolicyFunctions = {
      createPolicy,
      getAllowModes: getAllowModesV1,
      getDenyModes: getDenyModesV1,
      getPolicy,
      getPolicyAll,
      policyAsMarkdown,
      removePolicy,
      setAllowModes: setAllowModesV1,
      setDenyModes: setDenyModesV1,
      setPolicy,
      createResourcePolicyFor,
      getResourceAcrPolicy,
      getResourceAcrPolicyAll,
      getResourcePolicy,
      getResourcePolicyAll,
      removeResourceAcrPolicy,
      removeResourcePolicy,
      setResourceAcrPolicy,
      setResourcePolicy,
  };
  const v3RuleFunctions = {
      addAgent: addAgent$1,
      addGroup,
      createRule,
      getAgentAll: getAgentAll$1,
      getGroupAll,
      getRule,
      getRuleAll,
      removeAgent: removeAgent$1,
      removeGroup,
      removeRule,
      ruleAsMarkdown,
      setAgent: setAgent$1,
      setGroup,
      setRule,
      addClient: addClient$1,
      getClientAll: getClientAll$1,
      hasAnyClient: hasAnyClient$1,
      removeClient: removeClient$1,
      setAnyClient: setAnyClient$1,
      setClient: setClient$1,
      removeAnyClient: removeAnyClient$1,
      hasAuthenticated: hasAuthenticated$1,
      hasCreator: hasCreator$1,
      hasPublic: hasPublic$1,
      setAuthenticated: setAuthenticated$1,
      setCreator: setCreator$1,
      setPublic: setPublic$1,
      removeAuthenticated: removeAuthenticated$1,
      removeCreator: removeCreator$1,
      removePublic: removePublic$1,
      getAnyOfRuleUrlAll,
      addAnyOfRuleUrl,
      removeAnyOfRuleUrl,
      setAnyOfRuleUrl,
      getAllOfRuleUrlAll,
      addAllOfRuleUrl,
      removeAllOfRuleUrl,
      setAllOfRuleUrl,
      getNoneOfRuleUrlAll,
      addNoneOfRuleUrl,
      removeNoneOfRuleUrl,
      setNoneOfRuleUrl,
      createResourceRuleFor,
      getResourceRule,
      getResourceRuleAll,
      removeResourceRule,
      setResourceRule,
  };
  const v3MockFunctions = {
      addMockAcrTo,
      mockAcrFor,
  };
  /**
   * @hidden
   * @deprecated Please import directly from the "acp/*" modules.
   */
  const acp_v3 = Object.assign(Object.assign(Object.assign(Object.assign(Object.assign({}, v3AcpFunctions), v3ControlFunctions), v3PolicyFunctions), v3RuleFunctions), v3MockFunctions);

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * @param thing the [[Thing]] to check to see if it's an ACP Matcher or not
   */
  function isMatcher(thing) {
      return getIriAll(thing, rdf$2.type).includes(acp.Matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a Matcher that refines the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is **not** present in **any** of the "All Of" Matchers,
   * they will not be granted access.
   *
   * Also see [[addAnyOfMatcherUrl]] and [[addNoneOfMatcherUrl]].
   *
   * @param policy The [[Policy]] to which the Matcher should be added.
   * @param matcher The Matcher to add to the policy.
   * @returns A new [[Policy]] clone of the original one, with the new Matcher added.
   * @since Not released yet.
   */
  function addAllOfMatcherUrl(policy, matcher) {
      return addIri(policy, acp.allOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes a Matcher that refines the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is **not** present in **any** of the "All Of" Matchers,
   * they will not be granted access.
   * @param policy The [[Policy]] from which the Matcher should be removed.
   * @param matcher The Matcher to remove from the policy.
   * @returns A new [[Policy]] clone of the original one, with the Matcher removed.
   * @since Not released yet.
   */
  function removeAllOfMatcherUrl(policy, matcher) {
      return removeIri(policy, acp.allOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrites the Matcher refining the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is **not** present in **any** of the "All Of" Matchers,
   * they will not be granted access.
   * @param policy The [[Policy]] to which the Matcher should be added.
   * @param matcher The Matcher to set for the Policy.
   * @returns A new [[Policy]] clone of the original one, with the "All Of" Matchers replaced.
   * @since Not released yet.
   */
  function setAllOfMatcherUrl(policy, matcher) {
      return setIri(policy, acp.allOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the "All Of" [[Matcher]]s for the given [[Policy]]
   * @param policy The [[policy]] from which the Matchers should be read.
   * @returns A list of the "All Of" [[Matcher]]s
   * @since Not released yet.
   */
  function getAllOfMatcherUrlAll(policy) {
      return getIriAll(policy, acp.allOf);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a Matcher that extends the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the "Any Of" Matchers,
   * they will be granted access.
   *
   * Also see [[addAllOfMatcherUrl]] and [[addNoneOfMatcherUrl]].
   *
   * @param policy The [[Policy]] to which the Matcher should be added.
   * @param matcher The Matcher to add to the policy.
   * @returns A new [[Policy]] clone of the original one, with the new Matcher added.
   * @since Not released yet.
   */
  function addAnyOfMatcherUrl(policy, matcher) {
      return addIri(policy, acp.anyOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes a Matcher that extends the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the "Any Of" Matchers,
   * they will be granted access.
   * @param policy The [[Policy]] from which the Matcher should be removed.
   * @param matcher The Matcher to remove from the policy.
   * @returns A new [[Policy]] clone of the original one, with the Matcher removed.
   * @since Not released yet.
   */
  function removeAnyOfMatcherUrl(policy, matcher) {
      return removeIri(policy, acp.anyOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrite the Matcher extending the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is present in **any** of the "Any Of" Matchers,
   * they will be granted access.
   * @param policy The [[Policy]] to which the Matcher should be added.
   * @param matcher The Matcher to set for the Policy.
   * @returns A new [[Policy]] clone of the original one, with the "Any Of" Matchers replaced.
   * @since Not released yet.
   */
  function setAnyOfMatcherUrl(policy, matcher) {
      return setIri(policy, acp.anyOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the "Any Of" [[Matcher]]s for the given [[Policy]]
   * @param policy The [[policy]] from which the Matchers should be read.
   * @returns A list of the "Any Of" [[Matcher]]s
   * @since Not released yet.
   */
  function getAnyOfMatcherUrlAll(policy) {
      return getIriAll(policy, acp.anyOf);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Add a Matcher that restricts the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is matched by another Matcher, but **also**
   * by the given Matcher, they will **not** be granted access.
   *
   * Also see [[addAllOfMatcherUrl]] and [[addAnyOfMatcherUrl]].
   *
   * @param policy The [[Policy]] to which the Matcher should be added.
   * @param matcher The Matcher to add to the policy.
   * @returns A new [[Policy]] clone of the original one, with the new Matcher added.
   * @since Not released yet.
   */
  function addNoneOfMatcherUrl(policy, matcher) {
      return addIri(policy, acp.noneOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes a Matcher that restricts the scope of a given the [[Policy]]. If an agent
   * requesting access to a resource is matched by another Matcher, but **also**
   * in any of the "None Of" Matchers, they will **not** be granted access.
   *
   * @param policy The [[Policy]] from which the Matcher should be removed.
   * @param matcher The Matcher to remove from the policy.
   * @returns A new [[Policy]] clone of the original one, with the Matcher removed.
   * @since Not released yet.
   */
  function removeNoneOfMatcherUrl(policy, matcher) {
      return removeIri(policy, acp.noneOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set the Matchers restricting the scope of a given [[Policy]]. If an agent
   * requesting access to a resource is matched by another Matcher, but **also**
   * by any of the "None Of" Matchers, they will not be granted access.
   *
   * @param policy The [[Policy]] to which the Matcher should be added.
   * @param matcher The Matcher to set for the Policy.
   * @returns A new [[Policy]] clone of the original one, with the "None Of" Matchers replaced.
   * @since Not released yet.
   */
  function setNoneOfMatcherUrl(policy, matcher) {
      return setIri(policy, acp.noneOf, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the "None Of" [[Matcher]]s for the given [[Policy]]
   * @param policy The [[policy]] from which the Matchers should be read.
   * @returns A list of the forbidden [[Matcher]]s
   * @since Not released yet.
   */
  function getNoneOfMatcherUrlAll(policy) {
      return getIriAll(policy, acp.noneOf);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Initialise a new, empty [[Matcher]].
   *
   * @param url URL that identifies this [[Matcher]].
   * @since Not released yet.
   */
  function createMatcher(url) {
      const stringUrl = internal_toIriString(url);
      let matcherThing = createThing({ url: stringUrl });
      matcherThing = setUrl(matcherThing, rdf$2.type, acp.Matcher);
      return matcherThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Initialise a new, empty [[ResourceMatcher]] for the given Resource.
   *
   * @param resourceWithAcr The Resource to which the new Matcher is to apply.
   * @param name Name that identifies this [[Matcher]].
   * @since Not released yet.
   */
  function createResourceMatcherFor(resourceWithAcr, name) {
      const acr = internal_getAcr(resourceWithAcr);
      const url = new URL(getSourceUrl(acr));
      url.hash = `#${name}`;
      let matcherThing = createThing({ url: url.href });
      matcherThing = setUrl(matcherThing, rdf$2.type, acp.Matcher);
      return matcherThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the [[Matcher]] with the given URL from an [[SolidDataset]].
   *
   * @param matcherResource The Resource that contains the given [[Matcher]].
   * @param url URL that identifies this [[Matcher]].
   * @returns The requested [[Matcher]], if it exists, or `null` if it does not.
   * @since Not released yet.
   */
  function getMatcher(matcherResource, url) {
      const foundThing = getThing(matcherResource, url);
      if (foundThing === null || !isMatcher(foundThing)) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Get the [[ResourceMatcher]] with the given name from an Resource's Access Control
   * Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains the given [[ResourceMatcher]].
   * @param name Name that identifies this [[ResourceMatcher]].
   * @returns The requested [[ResourceMatcher]], if it exists, or `null` if it does not.
   * @since Not released yet.
   */
  function getResourceMatcher(resourceWithAcr, name) {
      const acr = internal_getAcr(resourceWithAcr);
      const acrUrl = getSourceUrl(acr);
      const url = new URL(acrUrl);
      url.hash = `#${name}`;
      const foundThing = getThing(acr, url.href);
      if (foundThing === null || !isMatcher(foundThing)) {
          return null;
      }
      return foundThing;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Gets the [[Matcher]]s from a [[SolidDataset]].
   *
   * @param matcherResource The Resource that contains (zero or more) [[Matcher]]s.
   * @returns The [[Matcher]]s contained in this resource.
   * @since Not released yet.
   */
  function getMatcherAll(matcherResource) {
      const things = getThingAll(matcherResource);
      return things.filter(isMatcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Gets the [[ResourceMatcher]]s from a Resource's Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains (zero or more) [[ResourceMatcher]]s.
   * @returns The [[ResourceMatcher]]s contained in this Resource's Access Control Resource.
   * @since Not released yet.
   */
  function getResourceMatcherAll(resourceWithAcr) {
      const acr = internal_getAcr(resourceWithAcr);
      const things = getThingAll(acr);
      return things.filter(isMatcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes the given [[Matcher]] from the given [[SolidDataset]].
   *
   * @param matcherResource The Resource that contains (zero or more) [[Matcher]]s.
   * @returns A new SolidDataset equal to the given Matcher Resource, but without the given Matcher.
   * @since Not released yet.
   */
  function removeMatcher(matcherResource, matcher) {
      return removeThing(matcherResource, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Removes the given [[ResourceMatcher]] from the given Resource's Access Control Resource.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains (zero or more) [[ResourceMatcher]]s.
   * @returns A new Resource equal to the given Resource, but without the given Matcher in its ACR.
   * @since Not released yet.
   */
  function removeResourceMatcher(resourceWithAcr, matcher) {
      const acr = internal_getAcr(resourceWithAcr);
      let matcherToRemove;
      if (typeof matcher === "string") {
          try {
              new URL(matcher);
              matcherToRemove = matcher;
          }
          catch (e) {
              // If the given Matcher to remove is the name of the Matcher,
              // resolve it to its full URL — developers usually refer to either the
              // Matcher itself, or by its name, as they do not have access to the ACR
              // directly.
              const matcherUrl = new URL(getSourceUrl(acr));
              matcherUrl.hash = `#${matcher}`;
              matcherToRemove = matcherUrl.href;
          }
      }
      else if (isNamedNode$1(matcher)) {
          matcherToRemove = internal_toIriString(matcher);
      }
      else {
          matcherToRemove = asUrl(matcher);
      }
      // Check whether the actual Matcher (i.e. with the Matcher type) exists:
      const matchingMatcher = getResourceMatcher(resourceWithAcr, new URL(matcherToRemove).hash.substring(1));
      if (matchingMatcher === null) {
          // No such Matcher exists yet, so return the Resource+ACR unchanged:
          return resourceWithAcr;
      }
      const updatedAcr = removeThing(acr, matchingMatcher);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return updatedResource;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert the given [[Matcher]] into the given [[SolidDataset]], replacing previous
   * instances of that Matcher.
   *
   * @param matcherResource The Resource that contains (zero or more) [[Matcher]]s.
   * @returns A new SolidDataset equal to the given Matcher Resource, but with the given Matcher.
   * @since Not released yet.
   */
  function setMatcher(matcherResource, matcher) {
      return setThing(matcherResource, matcher);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Insert the given [[ResourceMatcher]] into the given Resource's Access Control Resource,
   * replacing previous instances of that Matcher.
   *
   * @param resourceWithAcr The Resource whose Access Control Resource contains (zero or more) [[ResourceMatcher]]s.
   * @returns A new Resource equal to the given Resource, but with the given Matcher in its ACR.
   * @since Not released yet.
   */
  function setResourceMatcher(resourceWithAcr, matcher) {
      const acr = internal_getAcr(resourceWithAcr);
      const updatedAcr = setThing(acr, matcher);
      const updatedResource = internal_setAcr(resourceWithAcr, updatedAcr);
      return updatedResource;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * List all the agents a [[Matcher]] applies **directly** to. This will not include agents
   * that are matched on a property other than their WebID.
   *
   * @param matcher The matcher from which agents are read.
   * @returns A list of the WebIDs of agents included in the matcher.
   * @since Not released yet.
   */
  function getAgentAll(matcher) {
      return getIriAll(matcher, acp.agent).filter((agent) => agent !== acp.PublicAgent &&
          agent !== acp.AuthenticatedAgent &&
          agent !== acp.CreatorAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrite the agents the [[Matcher]] applies to with the provided agents.
   *
   * @param matcher The matcher for which agents are set.
   * @param agent The agent the matcher should apply to.
   * @returns A copy of the input matcher, applying to a different set of agents.
   * @since Not released yet.
   */
  function setAgent(matcher, agent) {
      // Preserve the special agent classes authenticated and public, which we
      // don't want to overwrite with this function.
      const isPublic = hasPublic(matcher);
      const isAuthenticated = hasAuthenticated(matcher);
      const isCreator = hasCreator(matcher);
      let result = setIri(matcher, acp.agent, agent);
      // Restore public and authenticated
      if (isPublic) {
          result = setPublic(result);
      }
      if (isAuthenticated) {
          result = setAuthenticated(result);
      }
      if (isCreator) {
          result = setCreator(result);
      }
      return result;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Apply the [[Matcher]] to an additional agent.
   *
   * @param matcher The [[Matcher]] to be applied to an additional agent.
   * @param agent The agent the [[Matcher]] should apply to.
   * @returns A copy of the [[Matcher]], applying to an additional agent.
   * @since Not released yet.
   */
  function addAgent(matcher, agent) {
      return addIri(matcher, acp.agent, agent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Prevent the [[Matcher]] from applying to a given agent directly. This will not
   * prevent the agent from matching on other properties than its WebID.
   *
   * @param matcher The [[Matcher]] that should no longer apply to a given agent.
   * @param agent The agent the Matcher should no longer apply to.
   * @returns A copy of the Matcher, not applying to the given agent.
   * @since Not released yet.
   */
  function removeAgent(matcher, agent) {
      return removeIri(matcher, acp.agent, agent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the Matcher applies to any agent.
   *
   * @param matcher The Matcher checked for public access.
   * @returns Whether the Matcher applies to any agent or not.
   * @since Not released yet.
   */
  function hasPublic(matcher) {
      return (getIriAll(matcher, acp.agent).filter((agent) => agent === acp.PublicAgent)
          .length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Matcher to apply to any Agent.
   *
   * @param matcher The Matcher being modified.
   * @returns A copy of the Matcher, updated to apply to any agent.
   * @since Not released yet.
   */
  function setPublic(matcher) {
      // The second argument should not be part of the function signature,
      // so it's not in the parameter list:
      // eslint-disable-next-line prefer-rest-params
      if (typeof arguments === "object" && typeof arguments[1] === "boolean") {
          throw new Error("The function `setPublic` no longer takes a second parameter. It is now used together with `removePublic` instead.");
      }
      return addIri(matcher, acp.agent, acp.PublicAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Matcher to no longer apply to any Agent.
   *
   * @param matcher The Matcher being modified.
   * @returns A copy of the Matcher, updated to no longer apply to any agent.
   * @since Not released yet.
   */
  function removePublic(matcher) {
      return removeIri(matcher, acp.agent, acp.PublicAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the Matcher applies to any authenticated agent.
   *
   * @param matcher The Matcher checked for authenticated access.
   * @returns Whether the Matcher applies to any authenticated agent or not.
   * @since Not released yet.
   */
  function hasAuthenticated(matcher) {
      return (getIriAll(matcher, acp.agent).filter((agent) => agent === acp.AuthenticatedAgent).length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Matcher to apply to any authenticated Agent.
   *
   * @param matcher The Matcher being modified.
   * @returns A copy of the Matcher, updated to apply to any authenticated Agent.
   * @since Not released yet.
   */
  function setAuthenticated(matcher) {
      // The second argument should not be part of the function signature,
      // so it's not in the parameter list:
      // eslint-disable-next-line prefer-rest-params
      if (typeof arguments === "object" && typeof arguments[1] === "boolean") {
          throw new Error("The function `setAuthenticated` no longer takes a second parameter. It is now used together with `removeAuthenticated` instead.");
      }
      return addIri(matcher, acp.agent, acp.AuthenticatedAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Matcher to no longer apply to any authenticated Agent.
   *
   * @param matcher The Matcher being modified.
   * @returns A copy of the Matcher, updated to apply/not apply to any authenticated agent.
   * @since Not released yet.
   */
  function removeAuthenticated(matcher) {
      return removeIri(matcher, acp.agent, acp.AuthenticatedAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the Matcher applies to the creator of the Resource.
   *
   * @param matcher The Matcher checked for authenticated access.
   * @returns Whether the Matcher applies to the creator of the Resource or not.
   * @since Not released yet.
   */
  function hasCreator(matcher) {
      return (getIriAll(matcher, acp.agent).filter((agent) => agent === acp.CreatorAgent)
          .length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Matcher to apply to the creator of a Resource.
   *
   * @param matcher The Matcher being modified.
   * @returns A copy of the Matcher, updated to apply to the creator of a Resource.
   * @since Not released yet.
   */
  function setCreator(matcher) {
      // The second argument should not be part of the function signature,
      // so it's not in the parameter list:
      // eslint-disable-next-line prefer-rest-params
      if (typeof arguments === "object" && typeof arguments[1] === "boolean") {
          throw new Error("The function `setCreator` no longer takes a second parameter. It is now used together with `removeCreator` instead.");
      }
      return addIri(matcher, acp.agent, acp.CreatorAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Set a Matcher to no longer apply to the creator of a Resource.
   *
   * @param matcher The Matcher being modified.
   * @returns A copy of the Matcher, updated to apply/not apply to the creator of a Resource.
   * @since Not released yet.
   */
  function removeCreator(matcher) {
      return removeIri(matcher, acp.agent, acp.CreatorAgent);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * List all the clients a [[Matcher]] applies **directly** to. This will not include
   * specific client classes, such as public clients.
   *
   * @param matcher The Matcher from which clients are read.
   * @returns A list of the WebIDs of clients included in the Matcher.
   * @since Not released yet.
   */
  function getClientAll(matcher) {
      return getIriAll(matcher, acp.client).filter((client) => client !== solid.PublicOidcClient);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Overwrite the clients the [[Matcher]] applies to with the provided Client.
   *
   * @param matcher The Matcher for which clients are set.
   * @param client The Client the Matcher should apply to.
   * @returns A copy of the input Matcher, applying to a different set of Clients.
   * @since Not released yet.
   */
  function setClient(matcher, client) {
      // Preserve the special "any client" class, which we
      // don't want to overwrite with this function.
      const anyClientEnabled = hasAnyClient(matcher);
      let result = setIri(matcher, acp.client, client);
      // Restore the "any client" class
      if (anyClientEnabled) {
          result = setAnyClient(result);
      }
      return result;
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Apply the [[Matcher]] to an additional Client.
   *
   * @param matcher The [[Matcher]] to be applied to an additional Client.
   * @param client The Client the [[Matcher]] should apply to.
   * @returns A copy of the [[Matcher]], applying to an additional Client.
   * @since Not released yet.
   */
  function addClient(matcher, client) {
      return addIri(matcher, acp.client, client);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Prevent the [[Matcher]] from applying to a given Client directly.
   *
   * @param matcher The [[Matcher]] that should no longer apply to a given Client.
   * @param client The Client the Matcher should no longer apply to.
   * @returns A copy of the Matcher, not applying to the given Client.
   * @since Not released yet.
   */
  function removeClient(matcher, client) {
      return removeIri(matcher, acp.client, client);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Check if the Matcher applies to any client, i.e. all the applications
   * regardless of their identifier.
   *
   * @param matcher The Matcher checked for authenticated access.
   * @returns Whether the Matcher applies to public clients.
   * @since Not released yet.
   */
  function hasAnyClient(matcher) {
      return (getIriAll(matcher, acp.client).filter((client) => client === solid.PublicOidcClient).length > 0);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Make the [[Matcher]] apply to any client application.
   *
   * @param matcher The Matcher for which clients are set.
   * @returns A copy of the Matcher, updated to apply to any client
   * @since Not released yet.
   */
  function setAnyClient(matcher) {
      return addIri(matcher, acp.client, solid.PublicOidcClient);
  }
  /**
   * ```{note} There is no Access Control Policies specification yet. As such, this
   * function is still experimental and subject to change, even in a non-major release.
   * ```
   *
   * Make the [[Matcher]] no longer apply to any client application.
   *
   * @param matcher The Matcher for which clients are set.
   * @returns A copy of the Matcher, updated to no longer apply to any client
   * @since Not released yet.
   */
  function removeAnyClient(matcher) {
      return removeIri(matcher, acp.client, solid.PublicOidcClient);
  }
  /**
   * Gets a human-readable representation of the given [[Matcher]] to aid debugging.
   *
   * Note that changes to the exact format of the return value are not considered a breaking change;
   * it is intended to aid in debugging, not as a serialisation method that can be reliably parsed.
   *
   * @param matcher The Matcher to get a human-readable representation of.
   * @since Not released yet.
   */
  function matcherAsMarkdown(matcher) {
      let markdown = `## Matcher: ${asUrl(matcher)}\n\n`;
      let targetEnumeration = "";
      if (hasPublic(matcher)) {
          targetEnumeration += "- Everyone\n";
      }
      if (hasAuthenticated(matcher)) {
          targetEnumeration += "- All authenticated agents\n";
      }
      if (hasCreator(matcher)) {
          targetEnumeration += "- The creator of this resource\n";
      }
      if (hasAnyClient(matcher)) {
          targetEnumeration += "- Users of any client application\n";
      }
      const targetAgents = getAgentAll(matcher);
      if (targetAgents.length > 0) {
          targetEnumeration += "- The following agents:\n  - ";
          targetEnumeration += targetAgents.join("\n  - ") + "\n";
      }
      const targetClients = getClientAll(matcher);
      if (targetClients.length > 0) {
          targetEnumeration += "- Users of the following client applications:\n  - ";
          targetEnumeration += targetClients.join("\n  - ") + "\n";
      }
      markdown +=
          targetEnumeration.length > 0
              ? "This Matcher matches:\n" + targetEnumeration
              : "<empty>\n";
      return markdown;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const v4AcpFunctions = {
      getFileWithAccessDatasets,
      getFileWithAcr,
      getReferencedPolicyUrlAll,
      getResourceInfoWithAccessDatasets,
      getResourceInfoWithAcr,
      getSolidDatasetWithAccessDatasets,
      getSolidDatasetWithAcr,
      hasAccessibleAcr,
      saveAcrFor,
  };
  const v4ControlFunctions = {
      acrAsMarkdown,
      addAcrPolicyUrl,
      addMemberAcrPolicyUrl,
      addMemberPolicyUrl,
      addPolicyUrl,
      getAcrPolicyUrlAll,
      getMemberAcrPolicyUrlAll,
      getMemberPolicyUrlAll,
      getPolicyUrlAll,
      hasLinkedAcr,
      removeAcrPolicyUrl,
      removeAcrPolicyUrlAll,
      removeMemberAcrPolicyUrl,
      removeMemberAcrPolicyUrlAll,
      removeMemberPolicyUrl,
      removeMemberPolicyUrlAll,
      removePolicyUrl,
      removePolicyUrlAll,
  };
  const v4PolicyFunctions = {
      createPolicy,
      getAllowModes: getAllowModesV2,
      getDenyModes: getDenyModesV2,
      getPolicy,
      getPolicyAll,
      policyAsMarkdown,
      removePolicy,
      setAllowModes: setAllowModesV2,
      setDenyModes: setDenyModesV2,
      setPolicy,
      createResourcePolicyFor,
      getResourceAcrPolicy,
      getResourceAcrPolicyAll,
      getResourcePolicy,
      getResourcePolicyAll,
      removeResourceAcrPolicy,
      removeResourcePolicy,
      setResourceAcrPolicy,
      setResourcePolicy,
  };
  const v4MatcherFunctions = {
      addAgent,
      createMatcher,
      getAgentAll,
      getMatcher,
      getMatcherAll,
      removeAgent,
      removeMatcher,
      matcherAsMarkdown,
      setAgent,
      setMatcher,
      addClient,
      getClientAll,
      hasAnyClient,
      removeClient,
      setAnyClient,
      setClient,
      removeAnyClient,
      hasAuthenticated,
      hasCreator,
      hasPublic,
      setAuthenticated,
      setCreator,
      setPublic,
      removeAuthenticated,
      removeCreator,
      removePublic,
      getAnyOfMatcherUrlAll,
      addAnyOfMatcherUrl,
      removeAnyOfMatcherUrl,
      setAnyOfMatcherUrl,
      getAllOfMatcherUrlAll,
      addAllOfMatcherUrl,
      removeAllOfMatcherUrl,
      setAllOfMatcherUrl,
      getNoneOfMatcherUrlAll,
      addNoneOfMatcherUrl,
      removeNoneOfMatcherUrl,
      setNoneOfMatcherUrl,
      createResourceMatcherFor,
      getResourceMatcher,
      getResourceMatcherAll,
      removeResourceMatcher,
      setResourceMatcher,
  };
  const v4MockFunctions = {
      addMockAcrTo,
      mockAcrFor,
  };
  /**
   * @hidden
   * @deprecated Please import directly from the "acp/*" modules.
   */
  const acp_v4 = Object.assign(Object.assign(Object.assign(Object.assign(Object.assign({}, v4AcpFunctions), v4ControlFunctions), v4PolicyFunctions), v4MatcherFunctions), v4MockFunctions);

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const knownActorRelations$1 = [acp.agent];
  /**
   * Get an overview of what access is defined for a given actor in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Matcher applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to this actor.
   *
   * Additionally, this only considers access given _explicitly_ to the given actor, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setActorAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @param actorRelation What type of actor (e.g. acp:agent) you want to get the access for.
   * @param actor Which instance of the given actor type you want to get the access for.
   * @returns What Access modes are granted to the given actor explicitly, or null if it could not be determined.
   */
  function internal_getActorAccess$1(acpData, actorRelation, actor) {
      if (acpData.inaccessibleUrls.length > 0) {
          // If we can't see all access data,
          // we can't reliably determine what access the given actor has:
          return null;
      }
      const applicableAcrPolicies = acpData.acrPolicies.filter((policy) => policyAppliesTo$1(policy, actorRelation, actor, acpData));
      const applicablePolicies = acpData.policies.filter((policy) => policyAppliesTo$1(policy, actorRelation, actor, acpData));
      const initialAccess = {
          read: false,
          append: false,
          write: false,
          controlRead: false,
          controlWrite: false,
      };
      // All allowed reading and writing defined in ACR policies
      // determines whether the `controlRead` and `controlWrite` statuses are `true`.
      const allowedAcrAccess = applicableAcrPolicies.reduce((acc, policy) => {
          const allAllowedAccess = Object.assign({}, acc);
          const allowModes = getAllowModesV2(policy);
          if (allowModes.read) {
              allAllowedAccess.controlRead = true;
          }
          if (allowModes.write) {
              allAllowedAccess.controlWrite = true;
          }
          return allAllowedAccess;
      }, initialAccess);
      // Then allowed reading, appending and writing in regular policies
      // determines whether the respective status is `true`.
      const withAllowedAccess = applicablePolicies.reduce((acc, policy) => {
          const allAllowedAccess = Object.assign({}, acc);
          const allowModes = getAllowModesV2(policy);
          if (allowModes.read) {
              allAllowedAccess.read = true;
          }
          if (allowModes.append) {
              allAllowedAccess.append = true;
          }
          if (allowModes.write) {
              allAllowedAccess.write = true;
          }
          return allAllowedAccess;
      }, allowedAcrAccess);
      // At this point, everything that has been explicitly allowed is true.
      // However, it could still be overridden by access that is explicitly denied.
      // Starting with `controlRead` and `controlWrite`,
      // by inspecting denied reading and writing defined in the ACR policies.
      const withAcrDeniedAccess = applicableAcrPolicies.reduce((acc, policy) => {
          const allDeniedAccess = Object.assign({}, acc);
          const denyModes = getDenyModesV2(policy);
          if (denyModes.read === true) {
              allDeniedAccess.controlRead = false;
          }
          if (denyModes.write === true) {
              allDeniedAccess.controlWrite = false;
          }
          return allDeniedAccess;
      }, withAllowedAccess);
      // And finally, we set to `false` those access modes that are explicitly denied
      // in the regular policies:
      const withDeniedAccess = applicablePolicies.reduce((acc, policy) => {
          const allDeniedAccess = Object.assign({}, acc);
          const denyModes = getDenyModesV2(policy);
          if (denyModes.read === true) {
              allDeniedAccess.read = false;
          }
          if (denyModes.append === true) {
              allDeniedAccess.append = false;
          }
          if (denyModes.write === true) {
              allDeniedAccess.write = false;
          }
          return allDeniedAccess;
      }, withAcrDeniedAccess);
      return withDeniedAccess;
  }
  /**
   * Get an overview of what access is defined for a given Agent in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Matcher applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to this Agent.
   *
   * Additionally, this only considers access given _explicitly_ to the given Agent, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setAgentAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @param webId WebID of the Agent you want to get the access for.
   * @returns What Access modes are granted to the given Agent explicitly, or null if it could not be determined.
   */
  function internal_getAgentAccess$1(acpData, webId) {
      return internal_getActorAccess$1(acpData, acp.agent, webId);
  }
  /**
   * Get an overview of what access is defined for everybody in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Matcher applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to everybody.
   *
   * Additionally, this only considers access given _explicitly_ to everybody, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setPublicAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @returns What Access modes are granted to everyone explicitly, or null if it could not be determined.
   */
  function internal_getPublicAccess$1(acpData) {
      return internal_getActorAccess$1(acpData, acp.agent, acp.PublicAgent);
  }
  function policyAppliesTo$1(policy, actorRelation, actor, acpData) {
      const allowModes = getIriAll(policy, acp.allow);
      const denyModes = getIriAll(policy, acp.deny);
      if (allowModes.length + denyModes.length === 0) {
          // A Policy that does not specify access modes does not do anything:
          return false;
      }
      // Note: the non-null assertions (`!`) here should be valid because
      //       the caller of `policyAppliesTo` should already have validated that
      //       the return value of internal_getPoliciesAndMatchers() did not have any
      //       inaccessible URLs, so we should be able to find every Matcher.
      const allOfMatchers = getAllOfMatcherUrlAll(policy).map((matcherUrl) => acpData.matchers.find((matcher) => asIri(matcher) === matcherUrl));
      const anyOfMatchers = getAnyOfMatcherUrlAll(policy).map((matcherUrl) => acpData.matchers.find((matcher) => asIri(matcher) === matcherUrl));
      const noneOfMatchers = getNoneOfMatcherUrlAll(policy).map((matcherUrl) => acpData.matchers.find((matcher) => asIri(matcher) === matcherUrl));
      // We assume that this Policy applies if this specific actor is mentioned
      // and no further restrictions are in place.
      // (In other words, the Policy may apply to others *in addition to* this
      // actor, but if it applies to this actor *unless* some other condition holds,
      // we cannot be sure whether it will apply to this actor.)
      // This means that:
      return (
      // Every existing allOf Matcher explicitly applies explicitly to this given actor:
      allOfMatchers.every((matcher) => matcherAppliesTo(matcher, actorRelation, actor)) &&
          // If there are anyOf Matchers, at least one applies explicitly to this actor:
          (anyOfMatchers.length === 0 ||
              anyOfMatchers.some((matcher) => matcherAppliesTo(matcher, actorRelation, actor))) &&
          // There is at least one allOf or anyOf Matcher:
          allOfMatchers.length + anyOfMatchers.length > 0 &&
          // No further restrictions are in place that make this sometimes not apply
          // to the given actor:
          noneOfMatchers.length === 0);
  }
  function policyConflictsWith$1(policy, otherAccess) {
      const allowModes = getIriAll(policy, acp.allow);
      const denyModes = getIriAll(policy, acp.deny);
      return ((otherAccess.read === true &&
          denyModes.includes(internal_accessModeIriStrings.read)) ||
          (otherAccess.read === false &&
              allowModes.includes(internal_accessModeIriStrings.read) &&
              !denyModes.includes(internal_accessModeIriStrings.read)) ||
          (otherAccess.append === true &&
              denyModes.includes(internal_accessModeIriStrings.append)) ||
          (otherAccess.append === false &&
              allowModes.includes(internal_accessModeIriStrings.append) &&
              !denyModes.includes(internal_accessModeIriStrings.append)) ||
          (otherAccess.write === true &&
              denyModes.includes(internal_accessModeIriStrings.write)) ||
          (otherAccess.write === false &&
              allowModes.includes(internal_accessModeIriStrings.write) &&
              !denyModes.includes(internal_accessModeIriStrings.write)));
  }
  function matcherAppliesTo(matcher, actorRelation, actor) {
      return getIriAll(matcher, actorRelation).includes(actor);
  }
  /**
   * Get a set of all actors mentioned in an ACR by active Matchers (i.e. that are
   * referenced by Policies referenced by the ACR Control, and therefore that
   * effectively apply).
   *
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @param actorRelation
   */
  function internal_findActorAll$1(acpData, actorRelation) {
      const actors = new Set();
      // This code could be prettier using flat(), which isn't supported by nodeJS 10.
      // If you read this comment after April 2021, feel free to refactor.
      acpData.matchers.forEach((matcher) => {
          getIriAll(matcher, actorRelation)
              .filter((iri) => ![
              acp.PublicAgent,
              acp.CreatorAgent,
              acp.AuthenticatedAgent,
          ].includes(iri) || actorRelation != acp.agent)
              .forEach((iri) => actors.add(iri));
      });
      return actors;
  }
  /**
   * Iterate through all the actors active for an ACR, and list all of their access.
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @param actorRelation The type of actor we want to list access for
   * @returns A map with each actor access indexed by their URL, or null if some
   * external policies are referenced.
   */
  function internal_getActorAccessAll$1(acpData, actorRelation) {
      if (acpData.inaccessibleUrls.length > 0) {
          // If we can't see all access data,
          // we can't reliably determine what access actors of the given type have:
          return null;
      }
      const result = {};
      const actors = internal_findActorAll$1(acpData, actorRelation);
      actors.forEach((iri) => {
          // The type assertion holds, because if internal_getActorAccess were null,
          // we would have returned {} already.
          const access = internal_getActorAccess$1(acpData, actorRelation, iri);
          result[iri] = access;
      });
      return result;
  }
  /**
   * Get an overview of what access are defined for all Agents in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Matcher applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to the mentionned
   * Agents.
   *
   * Additionally, this only considers access given _explicitly_ to individual Agents, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setAgentAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @returns A map with each Agent's access indexed by their WebID, or null if some
   * external policies are referenced.
   */
  function internal_getAgentAccessAll$1(acpData) {
      return internal_getActorAccessAll$1(acpData, acp.agent);
  }
  /**
   * Set access to a Resource for a specific actor.
   *
   * This function adds the relevant Access Control Policies and Matchers to a
   * Resource's Access Control Resource to define the given access for the given
   * actor specifically. In other words, it can, for example, add Policies that
   * give the general Public Read access to the Resource. However, if other
   * Policies specify that everyone is *denied* Read access *except* for a
   * particular Agent, then that will be left intact.
   * This means that, unless *only* this module's functions are used to manipulate
   * access to this Resource, the set access might not be equal to the effective
   * access for an agent matching the given actor.
   *
   * There are a number of preconditions that have to be fulfilled for this
   * function to work:
   * - Access to the Resource is determined via an Access Control Resource.
   * - The Resource's Access Control Resource does not refer to (Policies or Matchers
   *   in) other Resources.
   * - The current user has access to the Resource's Access Control Resource.
   *
   * If those conditions do not hold, this function will return `null`.
   *
   * Additionally, take note that the given access will only be applied to the
   * given Resource; if that Resource is a Container, access will have to be set
   * for its contained Resources independently.
   *
   * @param resource Resource that was fetched together with its linked Access Control Resource.
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @param actorRelation What type of actor (e.g. acp:agent) you want to set the access for.
   * @param actor Which instance of the given actor type you want to set the access for.
   * @param access What access (read, append, write, controlRead, controlWrite) to set for the given actor. `true` to allow, `false` to deny, and `undefined` to leave unchanged.
   * @returns The Resource with the updated Access Control Resource attached, if updated successfully, or `null` if not.
   */
  function internal_setActorAccess$1(resource, acpData, actorRelation, actor, access) {
      var _a, _b, _c, _d, _e;
      if (!hasAccessibleAcr(resource) || acpData.inaccessibleUrls.length > 0) {
          return null;
      }
      // Get the access that currently applies to the given actor
      const existingAccess = internal_getActorAccess$1(acpData, actorRelation, actor);
      /* istanbul ignore if: It returns null if the ACR has inaccessible Policies, which should happen since we already check for that above. */
      if (existingAccess === null) {
          return null;
      }
      // Get all Policies that apply specifically to the given actor
      const applicableAcrPolicies = acpData.acrPolicies.filter((policy) => policyAppliesTo$1(policy, actorRelation, actor, acpData));
      const applicablePolicies = acpData.policies.filter((policy) => policyAppliesTo$1(policy, actorRelation, actor, acpData));
      // We only need to override Policies that define access other than what we want:
      const conflictingAcrPolicies = applicableAcrPolicies.filter((policy) => policyConflictsWith$1(policy, {
          read: access.controlRead,
          write: access.controlWrite,
      }));
      const conflictingPolicies = applicablePolicies.filter((policy) => policyConflictsWith$1(policy, {
          read: access.read,
          append: access.append,
          write: access.write,
      }));
      // For every Policy that applies specifically to the given Actor, but _also_
      // to another actor (i.e. that applies using an anyOf Matcher, or a Matcher that
      // mentions both the given and another actor)...
      const otherActorAcrPolicies = conflictingAcrPolicies.filter((acrPolicy) => policyHasOtherActors$1(acrPolicy, actorRelation, actor, acpData));
      const otherActorPolicies = conflictingPolicies.filter((policy) => policyHasOtherActors$1(policy, actorRelation, actor, acpData));
      // ...check what access the current actor would have if we removed them...
      const acpDataWithPoliciesExcluded = Object.assign(Object.assign({}, acpData), { acrPolicies: acpData.acrPolicies.filter((acrPolicy) => !otherActorAcrPolicies.includes(acrPolicy)), policies: acpData.policies.filter((policy) => !otherActorPolicies.includes(policy)) });
      const remainingAccess = internal_getActorAccess$1(acpDataWithPoliciesExcluded, actorRelation, actor);
      /* istanbul ignore if: It returns null if the ACR has inaccessible Policies, which should happen since we already check for that at the start. */
      if (remainingAccess === null) {
          return null;
      }
      // ...add copies of those Policies and their Matchers, but excluding the given actor...
      let updatedResource = resource;
      otherActorAcrPolicies.forEach((acrPolicy) => {
          const [policyCopy, matcherCopies] = copyPolicyExcludingActor$1(acrPolicy, resource, acpData, actorRelation, actor);
          updatedResource = setResourceAcrPolicy(updatedResource, policyCopy);
          updatedResource = matcherCopies.reduce(setResourceMatcher, updatedResource);
      });
      otherActorPolicies.forEach((policy) => {
          const [policyCopy, matcherCopies] = copyPolicyExcludingActor$1(policy, resource, acpData, actorRelation, actor);
          updatedResource = setResourcePolicy(updatedResource, policyCopy);
          updatedResource = matcherCopies.reduce(setResourceMatcher, updatedResource);
      });
      // ...add a new Policy that applies the given access,
      // and the previously applying access for access modes that were undefined...
      const newMatcherName = `matcher_${encodeURIComponent(`${actorRelation}_${actor}`)}`;
      let newMatcher = createResourceMatcherFor(resource, newMatcherName);
      newMatcher = setIri(newMatcher, actorRelation, actor);
      const newControlReadAccess = (_a = access.controlRead) !== null && _a !== void 0 ? _a : existingAccess.controlRead;
      const newControlWriteAccess = (_b = access.controlWrite) !== null && _b !== void 0 ? _b : existingAccess.controlWrite;
      let acrPoliciesToUnapply = otherActorAcrPolicies;
      // Only replace existing Policies if the defined access actually changes:
      if (newControlReadAccess !== remainingAccess.controlRead ||
          newControlWriteAccess !== remainingAccess.controlWrite) {
          const newAcrPolicyName = `acr_policy` +
              `_${encodeURIComponent(`${actorRelation}_${actor}`)}` +
              `_${Date.now()}_${Math.random()}`;
          let newAcrPolicy = createResourcePolicyFor(resource, newAcrPolicyName);
          newAcrPolicy = setAllowModesV2(newAcrPolicy, {
              read: newControlReadAccess === true,
              append: false,
              write: newControlWriteAccess === true,
          });
          newAcrPolicy = addIri(newAcrPolicy, acp.allOf, newMatcher);
          updatedResource = setResourceAcrPolicy(updatedResource, newAcrPolicy);
          updatedResource = setResourceMatcher(updatedResource, newMatcher);
          // If we don't have to set new access, we only need to unapply the
          // ACR Policies that applied to both the given actor and other actors
          // (because they have been replaced by clones not mentioning the given
          // actor). Hence `policiesToUnApply` is initialised to `otherActorPolicies`.
          // However, if we're in this if branch, that means we also had to replace
          // Policies that defined access for just this actor, so we'll have to remove
          // all Policies mentioning this actor:
          acrPoliciesToUnapply = conflictingAcrPolicies;
      }
      const newReadAccess = (_c = access.read) !== null && _c !== void 0 ? _c : existingAccess.read;
      const newAppendAccess = (_d = access.append) !== null && _d !== void 0 ? _d : existingAccess.append;
      const newWriteAccess = (_e = access.write) !== null && _e !== void 0 ? _e : existingAccess.write;
      let policiesToUnapply = otherActorPolicies;
      // Only replace existing Policies if the defined access actually changes:
      if (newReadAccess !== remainingAccess.read ||
          newAppendAccess !== remainingAccess.append ||
          newWriteAccess !== remainingAccess.write) {
          const newPolicyName = `policy` +
              `_${encodeURIComponent(`${actorRelation}_${actor}`)}` +
              `_${Date.now()}_${Math.random()}`;
          let newPolicy = createResourcePolicyFor(resource, newPolicyName);
          newPolicy = setAllowModesV2(newPolicy, {
              read: newReadAccess === true,
              append: newAppendAccess === true,
              write: newWriteAccess === true,
          });
          newPolicy = addIri(newPolicy, acp.allOf, newMatcher);
          updatedResource = setResourcePolicy(updatedResource, newPolicy);
          updatedResource = setResourceMatcher(updatedResource, newMatcher);
          // If we don't have to set new access, we only need to unapply the
          // Policies that applied to both the given actor and other actors (because
          // they have been replaced by clones not mentioning the given actor). Hence
          // `policiesToUnApply` is initialised to `otherActorPolicies`.
          // However, if we're in this if branch, that means we also had to replace
          // Policies that defined access for just this actor, so we'll have to remove
          // all Policies mentioning this actor:
          policiesToUnapply = conflictingPolicies;
      }
      // ...then remove existing Policy URLs that mentioned both the given actor
      // and other actors from the given Resource and apply the new ones (but do not
      // remove the actual old Policies - they might still apply to other Resources!).
      acrPoliciesToUnapply.forEach((previouslyApplicableAcrPolicy) => {
          updatedResource = removeAcrPolicyUrl(updatedResource, asIri(previouslyApplicableAcrPolicy));
      });
      policiesToUnapply.forEach((previouslyApplicablePolicy) => {
          updatedResource = removePolicyUrl(updatedResource, asIri(previouslyApplicablePolicy));
      });
      return updatedResource;
  }
  /**
   * Set access to a Resource for a specific Agent.
   *
   * This function adds the relevant Access Control Policies and Matchers to a
   * Resource's Access Control Resource to define the given access for the given
   * Agent specifically. In other words, it can, for example, add Policies that
   * give a particular Agent Read access to the Resource. However, if other
   * Policies specify that that Agent is *denied* Read access *except* if they
   * match on some other characteristic, then that will be left intact.
   * This means that, unless *only* this function is used to manipulate access to
   * this Resource, the set access might not be equal to the effective access for
   * the given Agent.
   *
   * There are a number of preconditions that have to be fulfilled for this
   * function to work:
   * - Access to the Resource is determined via an Access Control Resource.
   * - The Resource's Access Control Resource does not refer to (Policies or Matchers
   *   in) other Resources.
   * - The current user has access to the Resource's Access Control Resource.
   *
   * If those conditions do not hold, this function will return `null`.
   *
   * Additionally, take note that the given access will only be applied to the
   * given Resource; if that Resource is a Container, access will have to be set
   * for its contained Resources independently.
   *
   * @param resource Resource that was fetched together with its linked Access Control Resource.
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @param webId Which Agent you want to set the access for.
   * @param access What access (read, append, write, controlRead, controlWrite) to set for the given Agent. `true` to allow, `false` to deny, and `undefined` to leave unchanged.
   * @returns The Resource with the updated Access Control Resource attached, if updated successfully, or `null` if not.
   */
  function internal_setAgentAccess$1(resource, acpData, webId, access) {
      return internal_setActorAccess$1(resource, acpData, acp.agent, webId, access);
  }
  /**
   * Set access to a Resource for everybody.
   *
   * This function adds the relevant Access Control Policies and Matchers to a
   * Resource's Access Control Resource to define the given access for everybody
   * specifically. In other words, it can, for example, add Policies that
   * give everybody Read access to the Resource. However, if other
   * Policies specify that everybody is *denied* Read access *except* if they're
   * a particular Agent, then that will be left intact.
   * This means that, unless *only* this module's functions are used to manipulate
   * access to this Resource, the set access might not be equal to the effective
   * access for a particular Agent.
   *
   * There are a number of preconditions that have to be fulfilled for this
   * function to work:
   * - Access to the Resource is determined via an Access Control Resource.
   * - The Resource's Access Control Resource does not refer to (Policies or Matchers
   *   in) other Resources.
   * - The current user has access to the Resource's Access Control Resource.
   *
   * If those conditions do not hold, this function will return `null`.
   *
   * Additionally, take note that the given access will only be applied to the
   * given Resource; if that Resource is a Container, access will have to be set
   * for its contained Resources independently.
   *
   * @param resource Resource that was fetched together with its linked Access Control Resource.
   * @param acpData All Access Control Policies and Matchers that apply to a particular Resource.
   * @param access What access (read, append, write, controlRead, controlWrite) to set for everybody. `true` to allow, `false` to deny, and `undefined` to leave unchanged.
   * @returns The Resource with the updated Access Control Resource attached, if updated successfully, or `null` if not.
   */
  function internal_setPublicAccess$1(resource, acpData, access) {
      return internal_setActorAccess$1(resource, acpData, acp.agent, acp.PublicAgent, access);
  }
  function policyHasOtherActors$1(policy, actorRelation, actor, acpData) {
      // Note: the non-null assertions (`!`) here should be valid because
      //       the caller of `policyHasOtherActors` should already have validated
      //       that the return value of internal_getPoliciesAndMatchers() did not have
      //       any inaccessible URLs, so we should be able to find every Matcher.
      const allOfMatchers = getIriAll(policy, acp.allOf).map((matcherUrl) => acpData.matchers.find((matcher) => asIri(matcher) === matcherUrl));
      const allOfMatchersHaveOtherActors = allOfMatchers.some((matcher) => {
          return matcherHasOtherActors(matcher, actorRelation, actor);
      });
      const anyOfMatchers = getIriAll(policy, acp.anyOf).map((matcherUrl) => acpData.matchers.find((matcher) => asIri(matcher) === matcherUrl));
      const anyOfMatchersHaveOtherActors = anyOfMatchers.some((matcher) => {
          return matcherHasOtherActors(matcher, actorRelation, actor);
      });
      /* istanbul ignore next This function only gets called after policyAppliesTo, which already filters out all noneOf Matchers */
      const noneOfMatchers = getIriAll(policy, acp.noneOf).map((matcherUrl) => acpData.matchers.find((matcher) => asIri(matcher) === matcherUrl));
      /* istanbul ignore next This function only gets called after policyAppliesTo, which already filters out all noneOf Matchers */
      const noneOfMatchersHaveOtherActors = noneOfMatchers.some((matcher) => {
          return matcherHasOtherActors(matcher, actorRelation, actor);
      });
      return (allOfMatchersHaveOtherActors ||
          anyOfMatchersHaveOtherActors ||
          noneOfMatchersHaveOtherActors);
  }
  function matcherHasOtherActors(matcher, actorRelation, actor) {
      const otherActors = [];
      knownActorRelations$1.forEach((knownActorRelation) => {
          const otherActorsWithThisRelation = getIriAll(matcher, knownActorRelation).filter((applicableActor) => applicableActor !== actor || knownActorRelation !== actorRelation);
          // Unfortunately Node 10 does not support `.flat()` yet, hence the use of `push`:
          otherActors.push(...otherActorsWithThisRelation);
      });
      return otherActors.length > 0;
  }
  function copyPolicyExcludingActor$1(inputPolicy, resourceWithAcr, acpData, actorRelationToExclude, actorToExclude) {
      const newIriSuffix = "_copy_without" +
          `_${encodeURIComponent(actorRelationToExclude)}_${actorToExclude}` +
          `_${Date.now()}_${Math.random()}`;
      // Create new Matchers for the Policy, excluding the given Actor
      const newAllOfMatchers = copyMatchersExcludingActor(getIriAll(inputPolicy, acp.allOf), resourceWithAcr, acpData, newIriSuffix, actorRelationToExclude, actorToExclude);
      const newAnyOfMatchers = copyMatchersExcludingActor(getIriAll(inputPolicy, acp.anyOf), resourceWithAcr, acpData, newIriSuffix, actorRelationToExclude, actorToExclude);
      const newNoneOfMatchers = copyMatchersExcludingActor(getIriAll(inputPolicy, acp.noneOf), resourceWithAcr, acpData, newIriSuffix, actorRelationToExclude, actorToExclude);
      // Create a new Policy with the new Matchers
      let newPolicy = createResourcePolicyFor(resourceWithAcr, encodeURI(asIri(inputPolicy)) + newIriSuffix);
      getIriAll(inputPolicy, acp.allow).forEach((allowMode) => {
          newPolicy = addIri(newPolicy, acp.allow, allowMode);
      });
      getIriAll(inputPolicy, acp.deny).forEach((denyMode) => {
          newPolicy = addIri(newPolicy, acp.deny, denyMode);
      });
      newAllOfMatchers.forEach((newMatcher) => {
          newPolicy = addIri(newPolicy, acp.allOf, newMatcher);
      });
      newAnyOfMatchers.forEach((newMatcher) => {
          newPolicy = addIri(newPolicy, acp.anyOf, newMatcher);
      });
      /* istanbul ignore next Policies listing noneOf Matchers are left alone (because they do not unambiguously apply to the given actor always), so there will usually not be any noneOf Matchers to copy. */
      newNoneOfMatchers.forEach((newMatcher) => {
          newPolicy = addIri(newPolicy, acp.noneOf, newMatcher);
      });
      return [
          newPolicy,
          newAllOfMatchers.concat(newAnyOfMatchers).concat(newNoneOfMatchers),
      ];
  }
  /** Creates clones of all the Matchers identified by `matcherIris` in `acpData`, excluding the given Actor */
  function copyMatchersExcludingActor(matcherIris, resourceWithAcr, acpData, iriSuffix, actorRelationToExclude, actorToExclude) {
      return matcherIris
          .map((matcherIri) => {
          const matcher = acpData.matchers.find((matcher) => asIri(matcher) === matcherIri);
          /* istanbul ignore if: getPoliciesAndMatchers should already have fetched all referenced Matchers, so this should never be true: */
          if (typeof matcher === "undefined") {
              return null;
          }
          let newMatcher = createResourceMatcherFor(resourceWithAcr, encodeURI(asIri(matcher)) + iriSuffix);
          let listsOtherActors = false;
          knownActorRelations$1.forEach((knownActorRelation) => {
              getIriAll(matcher, knownActorRelation).forEach((targetActor) => {
                  if (knownActorRelation === actorRelationToExclude &&
                      targetActor === actorToExclude) {
                      return;
                  }
                  listsOtherActors = true;
                  newMatcher = addIri(newMatcher, knownActorRelation, targetActor);
              });
          });
          return listsOtherActors ? newMatcher : null;
      })
          .filter(isNotNull$1);
  }
  function isNotNull$1(value) {
      return value !== null;
  }
  async function internal_getPoliciesAndMatchers(resource, options = internal_defaultFetchOptions) {
      const acrPolicyUrls = getAcrPolicyUrlAll(resource);
      const policyUrls = getPolicyUrlAll(resource);
      const allPolicyResourceUrls = getResourceUrls$1(acrPolicyUrls).concat(getResourceUrls$1(policyUrls));
      const policyResources = await getResources$1(allPolicyResourceUrls, options);
      const acrPolicies = getThingsFromResources$1(acrPolicyUrls, policyResources).filter(isNotNull$1);
      const policies = getThingsFromResources$1(policyUrls, policyResources).filter(isNotNull$1);
      const matcherUrlSet = new Set();
      acrPolicies.forEach((acrPolicy) => {
          const referencedMatcherUrls = getReferencedMatcherUrls(acrPolicy);
          referencedMatcherUrls.forEach((matcherUrl) => {
              matcherUrlSet.add(matcherUrl);
          });
      });
      policies.forEach((policy) => {
          const referencedMatcherUrls = getReferencedMatcherUrls(policy);
          referencedMatcherUrls.forEach((matcherUrl) => {
              matcherUrlSet.add(matcherUrl);
          });
      });
      const matcherUrls = Array.from(matcherUrlSet);
      const matcherResourceUrls = matcherUrls.map((matcherUrl) => getResourceUrl$1(matcherUrl));
      const unfetchedMatcherResourceUrls = matcherResourceUrls.filter((matcherResourceUrl) => !allPolicyResourceUrls.includes(matcherResourceUrl));
      const matcherResources = await getResources$1(unfetchedMatcherResourceUrls, options);
      const allResources = Object.assign(Object.assign({}, policyResources), matcherResources);
      const matchers = getThingsFromResources$1(matcherUrls, allResources).filter(isNotNull$1);
      const inaccessibleUrls = Object.keys(allResources).filter((resourceUrl) => allResources[resourceUrl] === null);
      return {
          inaccessibleUrls: inaccessibleUrls,
          acrPolicies: acrPolicies,
          policies: policies,
          matchers: matchers,
      };
  }
  function getResourceUrl$1(thingUrl) {
      const thingUrlObject = new URL(thingUrl);
      thingUrlObject.hash = "";
      return thingUrlObject.href;
  }
  function getResourceUrls$1(thingUrls) {
      const resourceUrls = [];
      thingUrls.forEach((thingUrl) => {
          const resourceUrl = getResourceUrl$1(thingUrl);
          if (!resourceUrls.includes(resourceUrl)) {
              resourceUrls.push(resourceUrl);
          }
      });
      return resourceUrls;
  }
  async function getResources$1(resourceUrls, options) {
      const uniqueResourceUrls = Array.from(new Set(resourceUrls));
      const resources = {};
      await Promise.all(uniqueResourceUrls.map(async (resourceUrl) => {
          try {
              const resource = await getSolidDataset(resourceUrl, options);
              resources[resourceUrl] = resource;
          }
          catch (e) {
              resources[resourceUrl] = null;
          }
      }));
      return resources;
  }
  function getThingsFromResources$1(thingUrls, resources) {
      return thingUrls.map((thingUrl) => {
          const resourceUrl = getResourceUrl$1(thingUrl);
          const resource = resources[resourceUrl];
          if (!resource) {
              return null;
          }
          return getThing(resource, thingUrl);
      });
  }
  function getReferencedMatcherUrls(policy) {
      return getAllOfMatcherUrlAll(policy)
          .concat(getAnyOfMatcherUrlAll(policy))
          .concat(getNoneOfMatcherUrlAll(policy));
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  function universalAccessToAcl(newAccess, previousAccess) {
      var _a, _b, _c, _d;
      // Universal access is aligned on ACP, which means there is a distinction between
      // controlRead and controlWrite. This split doesn't exist in WAC, which is why
      // the type for the input variable of this function is a restriction on the
      // universal Access type.
      if (newAccess.controlRead !== newAccess.controlWrite) {
          throw new Error("For Pods using Web Access Control, controlRead and controlWrite must be equal.");
      }
      return {
          read: (_a = newAccess.read) !== null && _a !== void 0 ? _a : previousAccess.read,
          append: (_b = newAccess.append) !== null && _b !== void 0 ? _b : previousAccess.append,
          write: (_c = newAccess.write) !== null && _c !== void 0 ? _c : previousAccess.write,
          control: (_d = newAccess.controlRead) !== null && _d !== void 0 ? _d : previousAccess.control,
      };
  }
  function aclAccessToUniversal(access) {
      // In ACL, denying access to an actor is a notion that doesn't exist, so an
      // access is either granted or not for a given mode.
      // This creates a misalignment with the ACP notion of an access being granted,
      // denied, or simply not mentioned. Here, we convert the boolean vision of
      // ACL into the boolean or undefined vision of ACP.
      return {
          read: access.read,
          write: access.write,
          append: access.append,
          controlRead: access.control,
          controlWrite: access.control,
      };
  }
  async function getActorAccess(resource, actor, accessEvaluationCallback, options) {
      const resourceAcl = await internal_fetchAcl(resource, options);
      const wacAccess = accessEvaluationCallback(internal_setAcl(resource, resourceAcl), actor);
      if (wacAccess === null) {
          return null;
      }
      return aclAccessToUniversal(wacAccess);
  }
  async function getActorClassAccess(resource, accessEvaluationCallback, options) {
      const resourceAcl = await internal_fetchAcl(resource, options);
      const wacAccess = accessEvaluationCallback(internal_setAcl(resource, resourceAcl));
      if (wacAccess === null) {
          return null;
      }
      return aclAccessToUniversal(wacAccess);
  }
  async function getActorAccessAll(resource, accessEvaluationCallback, options) {
      const resourceAcl = await internal_fetchAcl(resource, options);
      const wacAgentAccess = accessEvaluationCallback(internal_setAcl(resource, resourceAcl));
      if (wacAgentAccess === null) {
          return null;
      }
      const result = {};
      for (const [webId, wacAccess] of Object.entries(wacAgentAccess)) {
          result[webId] = aclAccessToUniversal(wacAccess);
      }
      return result;
  }
  /**
   * For a given Resource, look up its metadata, and read the Access permissions
   * granted to the given Agent.
   *
   * Note that this only lists permissions granted to the given Agent individually,
   * and will not exhaustively list modes the given Agent may have access to because
   * they apply to everyone, or because they apply to the Agent through a group for
   * instance.
   *
   * @param resource The URL of the Resource for which we want to list Access
   * @param agent The Agent for which the Access is granted
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns What Access modes are granted to the given Agent explicitly, or null if it could not be determined.
   */
  function getAgentAccess$2(resource, agent, options = internal_defaultFetchOptions) {
      return getActorAccess(resource, agent, getAgentAccess$3, options);
  }
  /**
   * For a given Resource, look up its metadata, and read the Access permissions
   * granted to the given Group.
   *
   * Note that this only lists permissions granted to the given Group individually,
   * and will not exhaustively list modes the given Group may have access to because
   * they apply to everyone, or because they apply to the Group through another
   * Group that may contain it for instance.
   *
   * @param resource The URL of the Resource for which we want to list Access
   * @param group The Group for which the Access is granted
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns What Access modes are granted to the given Group explicitly, or null if it could not be determined.
   */
  function getGroupAccess$1(resource, group, options = internal_defaultFetchOptions) {
      return getActorAccess(resource, group, getGroupAccess$2, options);
  }
  /**
   * For a given Resource, look up its metadata, and read the Access permissions
   * granted to everyone.
   *
   * Note that this only lists permissions explicitly granted to everyone as a whole,
   * and will not exhaustively list modes any individual Agent or Group may have
   * access to because they specifically apply to them only.
   *
   * @param resource The URL of the Resource for which we want to list public Access
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns What Access modes are granted to the everyone explicitly, or null if it could not be determined.
   */
  function getPublicAccess$2(resource, options = internal_defaultFetchOptions) {
      return getActorClassAccess(resource, getPublicAccess$3, options);
  }
  /**
   * For a given Resource, look up its metadata, and read the Access permissions
   * granted explicitly to each individual Agent.
   *
   * Note that this only lists permissions granted to each Agent individually,
   * and will not exhaustively list modes any Agent may have access to because
   * they apply to everyone, or because they apply to an Agent through a group for
   * instance.
   *
   * @param resource The URL of the Resource for which we want to list Agents Access
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A map of Agent WebIDs and the access granted to them, or null if it could not be determined.
   */
  function getAgentAccessAll$2(resource, options = internal_defaultFetchOptions) {
      return getActorAccessAll(resource, getAgentAccessAll$3, options);
  }
  /**
   * For a given Resource, look up its metadata, and read the Access permissions
   * granted explicitly to each individual Group.
   *
   * Note that this only lists permissions granted to each Group individually,
   * and will not exhaustively list modes any Group may have access to because
   * they apply individually to all of the Agents in the Group, or to everyone
   * for instance.
   *
   * @param resource The URL of the Resource for which we want to list Agents Access
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns A map of Group URLs and the access granted to them, or null if it could not be determined.
   */
  function getGroupAccessAll$1(resource, options = internal_defaultFetchOptions) {
      return getActorAccessAll(resource, getGroupAccessAll$2, options);
  }
  async function prepareResourceAcl(resource, options) {
      if (!hasAccessibleAcl(resource)) {
          return null;
      }
      const acl = await internal_fetchAcl(resource, options);
      const resourceWithAcl = internal_setAcl(resource, acl);
      let resourceAcl;
      if (hasResourceAcl(resourceWithAcl)) {
          // This is the simple case, where the Resource ACL we need to update already
          // exists.
          resourceAcl = internal_getResourceAcl(resourceWithAcl);
      }
      else if (hasFallbackAcl(resourceWithAcl)) {
          // In this case, the Resource ACL needs to be created first, and then updated.
          resourceAcl = createAclFromFallbackAcl(resourceWithAcl);
      }
      else {
          return null;
      }
      return internal_setResourceAcl(resourceWithAcl, resourceAcl);
  }
  async function saveUpdatedAcl(resource, acl, options) {
      let savedAcl = null;
      try {
          savedAcl = await saveAclFor(resource, acl, options);
          return internal_setResourceAcl(resource, savedAcl);
      }
      catch (e) {
          return null;
      }
  }
  async function setActorClassAccess(resource, access, getAccess, setAccess, options) {
      const resourceWithOldAcl = await prepareResourceAcl(resource, options);
      if (resourceWithOldAcl === null) {
          return null;
      }
      const resourceAcl = getResourceAcl(resourceWithOldAcl);
      const currentAccess = getAccess(resourceWithOldAcl);
      const wacAccess = universalAccessToAcl(access, currentAccess);
      const updatedResourceAcl = setAccess(resourceAcl, wacAccess);
      return await saveUpdatedAcl(resourceWithOldAcl, updatedResourceAcl, options);
  }
  async function setActorAccess(resource, actor, access, getAccess, setAccess, options) {
      const resourceWithOldAcl = await prepareResourceAcl(resource, options);
      if (resourceWithOldAcl === null) {
          return null;
      }
      const currentAccess = getAccess(resourceWithOldAcl, actor);
      const resourceAcl = getResourceAcl(resourceWithOldAcl);
      const wacAccess = universalAccessToAcl(access, currentAccess);
      const updatedResourceAcl = setAccess(resourceAcl, actor, wacAccess);
      return await saveUpdatedAcl(resourceWithOldAcl, updatedResourceAcl, options);
  }
  /**
   * Set the Access modes for a given Agent to a given Resource.
   *
   * Important note: if the target resource did not have a Resource ACL, and its
   * Access was regulated by its Fallback ACL, said Fallback ACL is copied to create
   * a new Resource ACL. This has the side effect that the next time the Fallback
   * ACL is updated, the changes _will not impact_ the target resource.
   *
   * If the target Resource's Access mode cannot be determined, e.g. the user does
   * not have Read and Write access to the target Resource's ACL, or to its
   * fallback ACL if it does not have a Resource ACL, then `null` is returned.
   *
   * @param resource The Resource for which Access is being set
   * @param agent The Agent for whom Access is being set
   * @param access The Access being set
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns The Resource, with its ACL updated, or null if the new Access could not
   * be set.
   */
  async function setAgentResourceAccess(resource, agent, access, options = internal_defaultFetchOptions) {
      return await setActorAccess(resource, agent, access, getAgentAccess$3, setAgentResourceAccess$1, options);
  }
  /**
   * Set the Access modes for a given Group to a given Resource.
   *
   * Important note: if the target resource did not have a Resource ACL, and its
   * Access was regulated by its Fallback ACL, said Fallback ACL is copied to create
   * a new Resource ACL. This has the side effect that the next time the Fallback
   * ACL is updated, the changes _will not impact_ the target resource.
   *
   * If the target Resource's Access mode cannot be determined, e.g. the user does
   * not have Read and Write access to the target Resource's ACL, or to its
   * fallback ACL if it does not have a Resource ACL, then `null` is returned.
   *
   * @param resource The Resource for which Access is being set
   * @param agent The Group for which Access is being set
   * @param access The Access being set
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns The Resource, with its ACL updated, or null if the new Access could not
   * be set.
   */
  async function setGroupResourceAccess(resource, group, access, options = internal_defaultFetchOptions) {
      return await setActorAccess(resource, group, access, getGroupAccess$2, setGroupResourceAccess$1, options);
  }
  /**
   * Set the Access modes for everyone to a given Resource.
   *
   * Important note: if the target resource did not have a Resource ACL, and its
   * Access was regulated by its Fallback ACL, said Fallback ACL is copied to create
   * a new Resource ACL. This has the side effect that the next time the Fallback
   * ACL is updated, the changes _will not impact_ the target resource.
   *
   * If the target Resource's Access mode cannot be determined, e.g. the user does
   * not have Read and Write access to the target Resource's ACL, or to its
   * fallback ACL if it does not have a Resource ACL, then `null` is returned.
   *
   * @param resource The Resource for which Access is being set
   * @param access The Access being set
   * @param options Optional parameter `options.fetch`: An alternative `fetch` function to make the HTTP request, compatible with the browser-native [fetch API](https://developer.mozilla.org/docs/Web/API/WindowOrWorkerGlobalScope/fetch#parameters).
   * @returns The Resource, with its ACL updated, or null if the new Access could not
   * be set.
   */
  async function setPublicResourceAccess(resource, access, options = internal_defaultFetchOptions) {
      return await setActorClassAccess(resource, access, getPublicAccess$3, setPublicResourceAccess$1, options);
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  const knownActorRelations = [acp.agent, acp.group];
  /**
   * Get an overview of what access is defined for a given actor in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Rule applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to this actor.
   *
   * Additionally, this only considers access given _explicitly_ to the given actor, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setActorAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param actorRelation What type of actor (e.g. acp:agent or acp:group) you want to get the access for.
   * @param actor Which instance of the given actor type you want to get the access for.
   * @returns What Access modes are granted to the given actor explicitly, or null if it could not be determined.
   */
  function internal_getActorAccess(acpData, actorRelation, actor) {
      if (acpData.inaccessibleUrls.length > 0) {
          // If we can't see all access data,
          // we can't reliably determine what access the given actor has:
          return null;
      }
      const applicableAcrPolicies = acpData.acrPolicies.filter((policy) => policyAppliesTo(policy, actorRelation, actor, acpData));
      const applicablePolicies = acpData.policies.filter((policy) => policyAppliesTo(policy, actorRelation, actor, acpData));
      const initialAccess = {
          read: false,
          append: false,
          write: false,
          controlRead: false,
          controlWrite: false,
      };
      // All allowed reading and writing defined in ACR policies
      // determines whether the `controlRead` and `controlWrite` statuses are `true`.
      const allowedAcrAccess = applicableAcrPolicies.reduce((acc, policy) => {
          const allAllowedAccess = Object.assign({}, acc);
          const allowModes = getAllowModesV1(policy);
          if (allowModes.read) {
              allAllowedAccess.controlRead = true;
          }
          if (allowModes.write) {
              allAllowedAccess.controlWrite = true;
          }
          return allAllowedAccess;
      }, initialAccess);
      // Then allowed reading, appending and writing in regular policies
      // determines whether the respective status is `true`.
      const withAllowedAccess = applicablePolicies.reduce((acc, policy) => {
          const allAllowedAccess = Object.assign({}, acc);
          const allowModes = getAllowModesV1(policy);
          if (allowModes.read) {
              allAllowedAccess.read = true;
          }
          if (allowModes.append) {
              allAllowedAccess.append = true;
          }
          if (allowModes.write) {
              allAllowedAccess.write = true;
          }
          return allAllowedAccess;
      }, allowedAcrAccess);
      // At this point, everything that has been explicitly allowed is true.
      // However, it could still be overridden by access that is explicitly denied.
      // Starting with `controlRead` and `controlWrite`,
      // by inspecting denied reading and writing defined in the ACR policies.
      const withAcrDeniedAccess = applicableAcrPolicies.reduce((acc, policy) => {
          const allDeniedAccess = Object.assign({}, acc);
          const denyModes = getDenyModesV1(policy);
          if (denyModes.read === true) {
              allDeniedAccess.controlRead = false;
          }
          if (denyModes.write === true) {
              allDeniedAccess.controlWrite = false;
          }
          return allDeniedAccess;
      }, withAllowedAccess);
      // And finally, we set to `false` those access modes that are explicitly denied
      // in the regular policies:
      const withDeniedAccess = applicablePolicies.reduce((acc, policy) => {
          const allDeniedAccess = Object.assign({}, acc);
          const denyModes = getDenyModesV1(policy);
          if (denyModes.read === true) {
              allDeniedAccess.read = false;
          }
          if (denyModes.append === true) {
              allDeniedAccess.append = false;
          }
          if (denyModes.write === true) {
              allDeniedAccess.write = false;
          }
          return allDeniedAccess;
      }, withAcrDeniedAccess);
      return withDeniedAccess;
  }
  /**
   * Get an overview of what access is defined for a given Agent in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Rule applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to this Agent.
   *
   * Additionally, this only considers access given _explicitly_ to the given Agent, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setAgentAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param webId WebID of the Agent you want to get the access for.
   * @returns What Access modes are granted to the given Agent explicitly, or null if it could not be determined.
   */
  function internal_getAgentAccess(acpData, webId) {
      return internal_getActorAccess(acpData, acp.agent, webId);
  }
  /**
   * Get an overview of what access is defined for a given Group in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Rule applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to this Group.
   *
   * Additionally, this only considers access given _explicitly_ to the given Group, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setGroupAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param groupUrl URL of the Group you want to get the access for.
   * @returns What Access modes are granted to the given Group explicitly, or null if it could not be determined.
   */
  function internal_getGroupAccess(acpData, groupUrl) {
      return internal_getActorAccess(acpData, acp.group, groupUrl);
  }
  /**
   * Get an overview of what access is defined for everybody in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Rule applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to everybody.
   *
   * Additionally, this only considers access given _explicitly_ to everybody, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setPublicAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @returns What Access modes are granted to everyone explicitly, or null if it could not be determined.
   */
  function internal_getPublicAccess(acpData) {
      return internal_getActorAccess(acpData, acp.agent, acp.PublicAgent);
  }
  function policyAppliesTo(policy, actorRelation, actor, acpData) {
      const allowModes = getIriAll(policy, acp.allow);
      const denyModes = getIriAll(policy, acp.deny);
      if (allowModes.length + denyModes.length === 0) {
          // A Policy that does not specify access modes does not do anything:
          return false;
      }
      // Note: the non-null assertions (`!`) here should be valid because
      //       the caller of `policyAppliesTo` should already have validated that
      //       the return value of internal_getPoliciesAndRules() did not have any
      //       inaccessible URLs, so we should be able to find every Rule.
      const allOfRules = getAllOfRuleUrlAll(policy).map((ruleUrl) => acpData.rules.find((rule) => asIri(rule) === ruleUrl));
      const anyOfRules = getAnyOfRuleUrlAll(policy).map((ruleUrl) => acpData.rules.find((rule) => asIri(rule) === ruleUrl));
      const noneOfRules = getNoneOfRuleUrlAll(policy).map((ruleUrl) => acpData.rules.find((rule) => asIri(rule) === ruleUrl));
      // We assume that this Policy applies if this specific actor is mentioned
      // and no further restrictions are in place.
      // (In other words, the Policy may apply to others *in addition to* this
      // actor, but if it applies to this actor *unless* some other condition holds,
      // we cannot be sure whether it will apply to this actor.)
      // This means that:
      return (
      // Every existing allOf Rule explicitly applies explicitly to this given actor:
      allOfRules.every((rule) => ruleAppliesTo(rule, actorRelation, actor)) &&
          // If there are anyOf Rules, at least one applies explicitly to this actor:
          (anyOfRules.length === 0 ||
              anyOfRules.some((rule) => ruleAppliesTo(rule, actorRelation, actor))) &&
          // No further restrictions are in place that make this sometimes not apply
          // to the given actor:
          noneOfRules.length === 0);
  }
  function policyConflictsWith(policy, otherAccess) {
      const allowModes = getIriAll(policy, acp.allow);
      const denyModes = getIriAll(policy, acp.deny);
      return ((otherAccess.read === true && denyModes.includes(acp.Read)) ||
          (otherAccess.read === false &&
              allowModes.includes(acp.Read) &&
              !denyModes.includes(acp.Read)) ||
          (otherAccess.append === true && denyModes.includes(acp.Append)) ||
          (otherAccess.append === false &&
              allowModes.includes(acp.Append) &&
              !denyModes.includes(acp.Append)) ||
          (otherAccess.write === true && denyModes.includes(acp.Write)) ||
          (otherAccess.write === false &&
              allowModes.includes(acp.Write) &&
              !denyModes.includes(acp.Write)));
  }
  function ruleAppliesTo(rule, actorRelation, actor) {
      // A Rule that does not list *any* actor matches for everyone:
      let isEmpty = true;
      knownActorRelations.forEach((knownActorRelation) => {
          isEmpty && (isEmpty = getIri(rule, knownActorRelation) === null);
      });
      return isEmpty || getIriAll(rule, actorRelation).includes(actor);
  }
  /**
   * Get a set of all actors mentioned in an ACR by active Rules (i.e. that are
   * referenced by Policies referenced by the ACR Control, and therefore that
   * effectively apply).
   *
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param actorRelation
   */
  function internal_findActorAll(acpData, actorRelation) {
      const actors = new Set();
      // This code could be prettier using flat(), which isn't supported by nodeJS 10.
      // If you read this comment after April 2021, feel free to refactor.
      acpData.rules.forEach((rule) => {
          getIriAll(rule, actorRelation)
              .filter((iri) => ![
              acp.PublicAgent,
              acp.CreatorAgent,
              acp.AuthenticatedAgent,
          ].includes(iri) || actorRelation != acp.agent)
              .forEach((iri) => actors.add(iri));
      });
      return actors;
  }
  /**
   * Iterate through all the actors active for an ACR, and list all of their access.
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param actorRelation The type of actor we want to list access for
   * @returns A map with each actor access indexed by their URL, or null if some
   * external policies are referenced.
   */
  function internal_getActorAccessAll(acpData, actorRelation) {
      if (acpData.inaccessibleUrls.length > 0) {
          // If we can't see all access data,
          // we can't reliably determine what access actors of the given type have:
          return null;
      }
      const result = {};
      const actors = internal_findActorAll(acpData, actorRelation);
      actors.forEach((iri) => {
          // The type assertion holds, because if internal_getActorAccess were null,
          // we would have returned {} already.
          const access = internal_getActorAccess(acpData, actorRelation, iri);
          result[iri] = access;
      });
      return result;
  }
  /**
   * Get an overview of what access are defined for all Groups in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Rule applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to the mentionned
   * Groups.
   *
   * Additionally, this only considers access given _explicitly_ to individual Groups, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setAgentAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @returns A map with each Group's access indexed by their URL, or null if some
   * external policies are referenced.
   */
  function internal_getGroupAccessAll(acpData) {
      return internal_getActorAccessAll(acpData, acp.group);
  }
  /**
   * Get an overview of what access are defined for all Agents in a Resource's Access Control Resource.
   *
   * This will only return a value if all relevant access is defined in just the Resource's Access
   * Control Resource; in other words, if an Access Policy or Access Rule applies that is re-used for
   * other Resources, this function will not be able to determine the access relevant to the mentionned
   * Agents.
   *
   * Additionally, this only considers access given _explicitly_ to individual Agents, i.e. without
   * additional conditions.
   *
   * In other words, this function will generally understand and return the access as set by
   * [[internal_setAgentAccess]], but not understand more convoluted Policies.
   *
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @returns A map with each Agent's access indexed by their WebID, or null if some
   * external policies are referenced.
   */
  function internal_getAgentAccessAll(acpData) {
      return internal_getActorAccessAll(acpData, acp.agent);
  }
  /**
   * Set access to a Resource for a specific actor.
   *
   * This function adds the relevant Access Control Policies and Rules to a
   * Resource's Access Control Resource to define the given access for the given
   * actor specifically. In other words, it can, for example, add Policies that
   * give a particular Group Read access to the Resource. However, if other
   * Policies specify that everyone in that Group is *denied* Read access *except*
   * for a particular Agent, then that will be left intact.
   * This means that, unless *only* this module's functions are used to manipulate
   * access to this Resource, the set access might not be equal to the effective
   * access for an agent matching the given actor.
   *
   * There are a number of preconditions that have to be fulfilled for this
   * function to work:
   * - Access to the Resource is determined via an Access Control Resource.
   * - The Resource's Access Control Resource does not refer to (Policies or Rules
   *   in) other Resources.
   * - The current user has access to the Resource's Access Control Resource.
   *
   * If those conditions do not hold, this function will return `null`.
   *
   * Additionally, take note that the given access will only be applied to the
   * given Resource; if that Resource is a Container, access will have to be set
   * for its contained Resources independently.
   *
   * @param resource Resource that was fetched together with its linked Access Control Resource.
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param actorRelation What type of actor (e.g. acp:agent or acp:group) you want to set the access for.
   * @param actor Which instance of the given actor type you want to set the access for.
   * @param access What access (read, append, write, controlRead, controlWrite) to set for the given actor. `true` to allow, `false` to deny, and `undefined` to leave unchanged.
   * @returns The Resource with the updated Access Control Resource attached, if updated successfully, or `null` if not.
   */
  function internal_setActorAccess(resource, acpData, actorRelation, actor, access) {
      var _a, _b, _c, _d, _e;
      if (!hasAccessibleAcr(resource) || acpData.inaccessibleUrls.length > 0) {
          return null;
      }
      // Get the access that currently applies to the given actor
      const existingAccess = internal_getActorAccess(acpData, actorRelation, actor);
      /* istanbul ignore if: It returns null if the ACR has inaccessible Policies, which should happen since we already check for that above. */
      if (existingAccess === null) {
          return null;
      }
      // Get all Policies that apply specifically to the given actor
      const applicableAcrPolicies = acpData.acrPolicies.filter((policy) => policyAppliesTo(policy, actorRelation, actor, acpData));
      const applicablePolicies = acpData.policies.filter((policy) => policyAppliesTo(policy, actorRelation, actor, acpData));
      // We only need to override Policies that define access other than what we want:
      const conflictingAcrPolicies = applicableAcrPolicies.filter((policy) => policyConflictsWith(policy, {
          read: access.controlRead,
          write: access.controlWrite,
      }));
      const conflictingPolicies = applicablePolicies.filter((policy) => policyConflictsWith(policy, {
          read: access.read,
          append: access.append,
          write: access.write,
      }));
      // For every Policy that applies specifically to the given Actor, but _also_
      // to another actor (i.e. that applies using an anyOf Rule, or a Rule that
      // mentions both the given and another actor)...
      const otherActorAcrPolicies = conflictingAcrPolicies.filter((acrPolicy) => policyHasOtherActors(acrPolicy, actorRelation, actor, acpData));
      const otherActorPolicies = conflictingPolicies.filter((policy) => policyHasOtherActors(policy, actorRelation, actor, acpData));
      // ...check what access the current actor would have if we removed them...
      const acpDataWithPoliciesExcluded = Object.assign(Object.assign({}, acpData), { acrPolicies: acpData.acrPolicies.filter((acrPolicy) => !otherActorAcrPolicies.includes(acrPolicy)), policies: acpData.policies.filter((policy) => !otherActorPolicies.includes(policy)) });
      const remainingAccess = internal_getActorAccess(acpDataWithPoliciesExcluded, actorRelation, actor);
      /* istanbul ignore if: It returns null if the ACR has inaccessible Policies, which should happen since we already check for that at the start. */
      if (remainingAccess === null) {
          return null;
      }
      // ...add copies of those Policies and their Rules, but excluding the given actor...
      let updatedResource = resource;
      otherActorAcrPolicies.forEach((acrPolicy) => {
          const [policyCopy, ruleCopies] = copyPolicyExcludingActor(acrPolicy, resource, acpData, actorRelation, actor);
          updatedResource = setResourceAcrPolicy(updatedResource, policyCopy);
          updatedResource = ruleCopies.reduce(setResourceRule, updatedResource);
      });
      otherActorPolicies.forEach((policy) => {
          const [policyCopy, ruleCopies] = copyPolicyExcludingActor(policy, resource, acpData, actorRelation, actor);
          updatedResource = setResourcePolicy(updatedResource, policyCopy);
          updatedResource = ruleCopies.reduce(setResourceRule, updatedResource);
      });
      // ...add a new Policy that applies the given access,
      // and the previously applying access for access modes that were undefined...
      const newRuleName = `rule_${encodeURIComponent(`${actorRelation}_${actor}`)}`;
      let newRule = createResourceRuleFor(resource, newRuleName);
      newRule = setIri(newRule, actorRelation, actor);
      const newControlReadAccess = (_a = access.controlRead) !== null && _a !== void 0 ? _a : existingAccess.controlRead;
      const newControlWriteAccess = (_b = access.controlWrite) !== null && _b !== void 0 ? _b : existingAccess.controlWrite;
      let acrPoliciesToUnapply = otherActorAcrPolicies;
      // Only replace existing Policies if the defined access actually changes:
      if (newControlReadAccess !== remainingAccess.controlRead ||
          newControlWriteAccess !== remainingAccess.controlWrite) {
          const newAcrPolicyName = `acr_policy` +
              `_${encodeURIComponent(`${actorRelation}_${actor}`)}` +
              `_${Date.now()}_${Math.random()}`;
          let newAcrPolicy = createResourcePolicyFor(resource, newAcrPolicyName);
          newAcrPolicy = setAllowModesV1(newAcrPolicy, {
              read: newControlReadAccess === true,
              append: false,
              write: newControlWriteAccess === true,
          });
          newAcrPolicy = addIri(newAcrPolicy, acp.allOf, newRule);
          updatedResource = setResourceAcrPolicy(updatedResource, newAcrPolicy);
          updatedResource = setResourceRule(updatedResource, newRule);
          // If we don't have to set new access, we only need to unapply the
          // ACR Policies that applied to both the given actor and other actors
          // (because they have been replaced by clones not mentioning the given
          // actor). Hence `policiesToUnApply` is initialised to `otherActorPolicies`.
          // However, if we're in this if branch, that means we also had to replace
          // Policies that defined access for just this actor, so we'll have to remove
          // all Policies mentioning this actor:
          acrPoliciesToUnapply = conflictingAcrPolicies;
      }
      const newReadAccess = (_c = access.read) !== null && _c !== void 0 ? _c : existingAccess.read;
      const newAppendAccess = (_d = access.append) !== null && _d !== void 0 ? _d : existingAccess.append;
      const newWriteAccess = (_e = access.write) !== null && _e !== void 0 ? _e : existingAccess.write;
      let policiesToUnapply = otherActorPolicies;
      // Only replace existing Policies if the defined access actually changes:
      if (newReadAccess !== remainingAccess.read ||
          newAppendAccess !== remainingAccess.append ||
          newWriteAccess !== remainingAccess.write) {
          const newPolicyName = `policy` +
              `_${encodeURIComponent(`${actorRelation}_${actor}`)}` +
              `_${Date.now()}_${Math.random()}`;
          let newPolicy = createResourcePolicyFor(resource, newPolicyName);
          newPolicy = setAllowModesV1(newPolicy, {
              read: newReadAccess === true,
              append: newAppendAccess === true,
              write: newWriteAccess === true,
          });
          newPolicy = addIri(newPolicy, acp.allOf, newRule);
          updatedResource = setResourcePolicy(updatedResource, newPolicy);
          updatedResource = setResourceRule(updatedResource, newRule);
          // If we don't have to set new access, we only need to unapply the
          // Policies that applied to both the given actor and other actors (because
          // they have been replaced by clones not mentioning the given actor). Hence
          // `policiesToUnApply` is initialised to `otherActorPolicies`.
          // However, if we're in this if branch, that means we also had to replace
          // Policies that defined access for just this actor, so we'll have to remove
          // all Policies mentioning this actor:
          policiesToUnapply = conflictingPolicies;
      }
      // ...then remove existing Policy URLs that mentioned both the given actor
      // and other actors from the given Resource and apply the new ones (but do not
      // remove the actual old Policies - they might still apply to other Resources!).
      acrPoliciesToUnapply.forEach((previouslyApplicableAcrPolicy) => {
          updatedResource = removeAcrPolicyUrl(updatedResource, asIri(previouslyApplicableAcrPolicy));
      });
      policiesToUnapply.forEach((previouslyApplicablePolicy) => {
          updatedResource = removePolicyUrl(updatedResource, asIri(previouslyApplicablePolicy));
      });
      return updatedResource;
  }
  /**
   * Set access to a Resource for a specific Agent.
   *
   * This function adds the relevant Access Control Policies and Rules to a
   * Resource's Access Control Resource to define the given access for the given
   * Agent specifically. In other words, it can, for example, add Policies that
   * give a particular Agent Read access to the Resource. However, if other
   * Policies specify that that Agent is *denied* Read access *except* if they're
   * in a particular Group, then that will be left intact.
   * This means that, unless *only* this function is used to manipulate access to
   * this Resource, the set access might not be equal to the effective access for
   * the given Agent.
   *
   * There are a number of preconditions that have to be fulfilled for this
   * function to work:
   * - Access to the Resource is determined via an Access Control Resource.
   * - The Resource's Access Control Resource does not refer to (Policies or Rules
   *   in) other Resources.
   * - The current user has access to the Resource's Access Control Resource.
   *
   * If those conditions do not hold, this function will return `null`.
   *
   * Additionally, take note that the given access will only be applied to the
   * given Resource; if that Resource is a Container, access will have to be set
   * for its contained Resources independently.
   *
   * @param resource Resource that was fetched together with its linked Access Control Resource.
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param webId Which Agent you want to set the access for.
   * @param access What access (read, append, write, controlRead, controlWrite) to set for the given Agent. `true` to allow, `false` to deny, and `undefined` to leave unchanged.
   * @returns The Resource with the updated Access Control Resource attached, if updated successfully, or `null` if not.
   */
  function internal_setAgentAccess(resource, acpData, webId, access) {
      return internal_setActorAccess(resource, acpData, acp.agent, webId, access);
  }
  /**
   * Set access to a Resource for a specific Group.
   *
   * This function adds the relevant Access Control Policies and Rules to a
   * Resource's Access Control Resource to define the given access for the given
   * Group specifically. In other words, it can, for example, add Policies that
   * give a particular Group Read access to the Resource. However, if other
   * Policies specify that it is *denied* Read access *except* if they're a
   * particular Agent, then that will be left intact.
   * This means that, unless *only* this module's functions are used to manipulate
   * access to this Resource, the set access might not be equal to the effective
   * access for Agents in the given Group.
   *
   * There are a number of preconditions that have to be fulfilled for this
   * function to work:
   * - Access to the Resource is determined via an Access Control Resource.
   * - The Resource's Access Control Resource does not refer to (Policies or Rules
   *   in) other Resources.
   * - The current user has access to the Resource's Access Control Resource.
   *
   * If those conditions do not hold, this function will return `null`.
   *
   * Additionally, take note that the given access will only be applied to the
   * given Resource; if that Resource is a Container, access will have to be set
   * for its contained Resources independently.
   *
   * @param resource Resource that was fetched together with its linked Access Control Resource.
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param groupUrl Which Group you want to set the access for.
   * @param access What access (read, append, write, controlRead, controlWrite) to set for the given Group. `true` to allow, `false` to deny, and `undefined` to leave unchanged.
   * @returns The Resource with the updated Access Control Resource attached, if updated successfully, or `null` if not.
   */
  function internal_setGroupAccess(resource, acpData, groupUrl, access) {
      return internal_setActorAccess(resource, acpData, acp.group, groupUrl, access);
  }
  /**
   * Set access to a Resource for everybody.
   *
   * This function adds the relevant Access Control Policies and Rules to a
   * Resource's Access Control Resource to define the given access for everybody
   * specifically. In other words, it can, for example, add Policies that
   * give everybody Read access to the Resource. However, if other
   * Policies specify that everybody is *denied* Read access *except* if they're
   * in a particular Group, then that will be left intact.
   * This means that, unless *only* this module's functions are used to manipulate
   * access to this Resource, the set access might not be equal to the effective
   * access for a particular Agent.
   *
   * There are a number of preconditions that have to be fulfilled for this
   * function to work:
   * - Access to the Resource is determined via an Access Control Resource.
   * - The Resource's Access Control Resource does not refer to (Policies or Rules
   *   in) other Resources.
   * - The current user has access to the Resource's Access Control Resource.
   *
   * If those conditions do not hold, this function will return `null`.
   *
   * Additionally, take note that the given access will only be applied to the
   * given Resource; if that Resource is a Container, access will have to be set
   * for its contained Resources independently.
   *
   * @param resource Resource that was fetched together with its linked Access Control Resource.
   * @param acpData All Access Control Policies and Rules that apply to a particular Resource.
   * @param access What access (read, append, write, controlRead, controlWrite) to set for everybody. `true` to allow, `false` to deny, and `undefined` to leave unchanged.
   * @returns The Resource with the updated Access Control Resource attached, if updated successfully, or `null` if not.
   */
  function internal_setPublicAccess(resource, acpData, access) {
      return internal_setActorAccess(resource, acpData, acp.agent, acp.PublicAgent, access);
  }
  function policyHasOtherActors(policy, actorRelation, actor, acpData) {
      // Note: the non-null assertions (`!`) here should be valid because
      //       the caller of `policyHasOtherActors` should already have validated
      //       that the return value of internal_getPoliciesAndRules() did not have
      //       any inaccessible URLs, so we should be able to find every Rule.
      const allOfRules = getIriAll(policy, acp.allOf).map((ruleUrl) => acpData.rules.find((rule) => asIri(rule) === ruleUrl));
      const allOfRulesHaveOtherActors = allOfRules.some((rule) => {
          return ruleHasOtherActors(rule, actorRelation, actor);
      });
      const anyOfRules = getIriAll(policy, acp.anyOf).map((ruleUrl) => acpData.rules.find((rule) => asIri(rule) === ruleUrl));
      const anyOfRulesHaveOtherActors = anyOfRules.some((rule) => {
          return ruleHasOtherActors(rule, actorRelation, actor);
      });
      /* istanbul ignore next This function only gets called after policyAppliesTo, which already filters out all noneOf Rules */
      const noneOfRules = getIriAll(policy, acp.noneOf).map((ruleUrl) => acpData.rules.find((rule) => asIri(rule) === ruleUrl));
      /* istanbul ignore next This function only gets called after policyAppliesTo, which already filters out all noneOf Rules */
      const noneOfRulesHaveOtherActors = noneOfRules.some((rule) => {
          return ruleHasOtherActors(rule, actorRelation, actor);
      });
      return (allOfRulesHaveOtherActors ||
          anyOfRulesHaveOtherActors ||
          noneOfRulesHaveOtherActors);
  }
  function ruleHasOtherActors(rule, actorRelation, actor) {
      const otherActors = [];
      knownActorRelations.forEach((knownActorRelation) => {
          const otherActorsWithThisRelation = getIriAll(rule, knownActorRelation).filter((applicableActor) => applicableActor !== actor || knownActorRelation !== actorRelation);
          // Unfortunately Node 10 does not support `.flat()` yet, hence the use of `push`:
          otherActors.push(...otherActorsWithThisRelation);
      });
      return otherActors.length > 0;
  }
  function copyPolicyExcludingActor(inputPolicy, resourceWithAcr, acpData, actorRelationToExclude, actorToExclude) {
      const newIriSuffix = "_copy_without" +
          `_${encodeURIComponent(actorRelationToExclude)}_${actorToExclude}` +
          `_${Date.now()}_${Math.random()}`;
      // Create new Rules for the Policy, excluding the given Actor
      const newAllOfRules = copyRulesExcludingActor(getIriAll(inputPolicy, acp.allOf), resourceWithAcr, acpData, newIriSuffix, actorRelationToExclude, actorToExclude);
      const newAnyOfRules = copyRulesExcludingActor(getIriAll(inputPolicy, acp.anyOf), resourceWithAcr, acpData, newIriSuffix, actorRelationToExclude, actorToExclude);
      const newNoneOfRules = copyRulesExcludingActor(getIriAll(inputPolicy, acp.noneOf), resourceWithAcr, acpData, newIriSuffix, actorRelationToExclude, actorToExclude);
      // Create a new Policy with the new Rules
      let newPolicy = createResourcePolicyFor(resourceWithAcr, encodeURI(asIri(inputPolicy)) + newIriSuffix);
      getIriAll(inputPolicy, acp.allow).forEach((allowMode) => {
          newPolicy = addIri(newPolicy, acp.allow, allowMode);
      });
      getIriAll(inputPolicy, acp.deny).forEach((denyMode) => {
          newPolicy = addIri(newPolicy, acp.deny, denyMode);
      });
      newAllOfRules.forEach((newRule) => {
          newPolicy = addIri(newPolicy, acp.allOf, newRule);
      });
      newAnyOfRules.forEach((newRule) => {
          newPolicy = addIri(newPolicy, acp.anyOf, newRule);
      });
      /* istanbul ignore next Policies listing noneOf Rules are left alone (because they do not unambiguously apply to the given actor always), so there will usually not be any noneOf Rules to copy. */
      newNoneOfRules.forEach((newRule) => {
          newPolicy = addIri(newPolicy, acp.noneOf, newRule);
      });
      return [
          newPolicy,
          newAllOfRules.concat(newAnyOfRules).concat(newNoneOfRules),
      ];
  }
  /** Creates clones of all the Rules identified by `ruleIris` in `acpData`, excluding the given Actor */
  function copyRulesExcludingActor(ruleIris, resourceWithAcr, acpData, iriSuffix, actorRelationToExclude, actorToExclude) {
      return ruleIris
          .map((ruleIri) => {
          const rule = acpData.rules.find((rule) => asIri(rule) === ruleIri);
          /* istanbul ignore if: getPoliciesAndRules should already have fetched all referenced Rules, so this should never be true: */
          if (typeof rule === "undefined") {
              return null;
          }
          let newRule = createResourceRuleFor(resourceWithAcr, encodeURI(asIri(rule)) + iriSuffix);
          let listsOtherActors = false;
          knownActorRelations.forEach((knownActorRelation) => {
              getIriAll(rule, knownActorRelation).forEach((targetActor) => {
                  if (knownActorRelation === actorRelationToExclude &&
                      targetActor === actorToExclude) {
                      return;
                  }
                  listsOtherActors = true;
                  newRule = addIri(newRule, knownActorRelation, targetActor);
              });
          });
          return listsOtherActors ? newRule : null;
      })
          .filter(isNotNull);
  }
  function isNotNull(value) {
      return value !== null;
  }
  async function internal_getPoliciesAndRules(resource, options = internal_defaultFetchOptions) {
      const acrPolicyUrls = getAcrPolicyUrlAll(resource);
      const policyUrls = getPolicyUrlAll(resource);
      const allPolicyResourceUrls = getResourceUrls(acrPolicyUrls).concat(getResourceUrls(policyUrls));
      const policyResources = await getResources(allPolicyResourceUrls, options);
      const acrPolicies = getThingsFromResources(acrPolicyUrls, policyResources).filter(isNotNull);
      const policies = getThingsFromResources(policyUrls, policyResources).filter(isNotNull);
      const ruleUrlSet = new Set();
      acrPolicies.forEach((acrPolicy) => {
          const referencedRuleUrls = getReferencedRuleUrls(acrPolicy);
          referencedRuleUrls.forEach((ruleUrl) => {
              ruleUrlSet.add(ruleUrl);
          });
      });
      policies.forEach((policy) => {
          const referencedRuleUrls = getReferencedRuleUrls(policy);
          referencedRuleUrls.forEach((ruleUrl) => {
              ruleUrlSet.add(ruleUrl);
          });
      });
      const ruleUrls = Array.from(ruleUrlSet);
      const ruleResourceUrls = ruleUrls.map((ruleUrl) => getResourceUrl(ruleUrl));
      const unfetchedRuleResourceUrls = ruleResourceUrls.filter((ruleResourceUrl) => !allPolicyResourceUrls.includes(ruleResourceUrl));
      const ruleResources = await getResources(unfetchedRuleResourceUrls, options);
      const allResources = Object.assign(Object.assign({}, policyResources), ruleResources);
      const rules = getThingsFromResources(ruleUrls, allResources).filter(isNotNull);
      const inaccessibleUrls = Object.keys(allResources).filter((resourceUrl) => allResources[resourceUrl] === null);
      return {
          inaccessibleUrls: inaccessibleUrls,
          acrPolicies: acrPolicies,
          policies: policies,
          rules: rules,
      };
  }
  function getResourceUrl(thingUrl) {
      const thingUrlObject = new URL(thingUrl);
      thingUrlObject.hash = "";
      return thingUrlObject.href;
  }
  function getResourceUrls(thingUrls) {
      const resourceUrls = [];
      thingUrls.forEach((thingUrl) => {
          const resourceUrl = getResourceUrl(thingUrl);
          if (!resourceUrls.includes(resourceUrl)) {
              resourceUrls.push(resourceUrl);
          }
      });
      return resourceUrls;
  }
  async function getResources(resourceUrls, options) {
      const uniqueResourceUrls = Array.from(new Set(resourceUrls));
      const resources = {};
      await Promise.all(uniqueResourceUrls.map(async (resourceUrl) => {
          try {
              const resource = await getSolidDataset(resourceUrl, options);
              resources[resourceUrl] = resource;
          }
          catch (e) {
              resources[resourceUrl] = null;
          }
      }));
      return resources;
  }
  function getThingsFromResources(thingUrls, resources) {
      return thingUrls.map((thingUrl) => {
          const resourceUrl = getResourceUrl(thingUrl);
          const resource = resources[resourceUrl];
          if (!resource) {
              return null;
          }
          return getThing(resource, thingUrl);
      });
  }
  function getReferencedRuleUrls(policy) {
      return getAllOfRuleUrlAll(policy)
          .concat(getAnyOfRuleUrlAll(policy))
          .concat(getNoneOfRuleUrlAll(policy));
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Get an overview of what access is defined for a given Agent.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for the given Agent. If
   *   additional restrictions are set up to apply to the given Agent in a
   *   particular situation, those will not be reflected in the return value of
   *   this function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @param webId WebID of the Agent you want to get the access for.
   * @since 1.5.0
   */
  async function getAgentAccess$1(resourceUrl, webId, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          return internal_getAgentAccess(acpData, webId);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getAgentAccess$2(resourceInfo, webId, options);
      }
      return null;
  }
  /**
   * Set access to a Resource for a specific Agent.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably setting access, in which case it will
   *   resolve to `null`.
   * - It will only set access explicitly for the given Agent. In other words,
   *   additional restrictions could be present that further restrict or loosen
   *   what access the given Agent has in particular circumstances.
   * - The provided access will only apply to the given Resource. In other words,
   *   if the Resource is a Container, the configured Access may not apply to
   *   contained Resources.
   * - If the current user does not have permission to view or change access for
   *   the given Resource, this function will resolve to `null`.
   *
   * Additionally, two caveats apply to users with a Pod server that uses WAC:
   * - If the Resource did not have an ACL yet, a new one will be initialised.
   *   This means that changes to the ACL of a parent Container can no longer
   *   affect access people have to this Resource, although existing access will
   *   be preserved.
   * - Setting different values for `controlRead` and `controlWrite` is not
   *   supported, and **will throw an error**. If you expect (some of) your users
   *   to have Pods implementing WAC, be sure to pass the same value for both.
   *
   * @param resourceUrl URL of the Resource you want to change the Agent's access to.
   * @param webId WebID of the Agent you want to set access for.
   * @param access What access permissions you want to set for the given Agent to the given Resource. Possible properties are `read`, `append`, `write`, `controlRead` and `controlWrite`: set to `true` to allow, to `false` to stop allowing, or `undefined` to leave unchanged. Take note that `controlRead` and `controlWrite` can not have distinct values for a Pod server implementing Web Access Control; trying this will throw an error.
   * @returns What access has been set for the given Agent explicitly.
   * @since 1.5.0
   */
  async function setAgentAccess$1(resourceUrl, webId, access, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          const updatedResource = internal_setAgentAccess(resourceInfo, acpData, webId, access);
          if (updatedResource) {
              try {
                  await saveAcrFor(updatedResource, options);
                  return await getAgentAccess$1(resourceUrl, webId, options);
              }
              catch (e) {
                  return null;
              }
          }
          return null;
      }
      if (hasAccessibleAcl(resourceInfo)) {
          if (access.controlRead != access.controlWrite) {
              throw new Error(`When setting access for a Resource in a Pod implementing Web Access Control (i.e. [${getSourceIri(resourceInfo)}]), ` + "`controlRead` and `controlWrite` should have the same value.");
          }
          const wacAccess = access;
          await setAgentResourceAccess(resourceInfo, webId, wacAccess, options);
          return await getAgentAccess$2(resourceInfo, webId, options);
      }
      return null;
  }
  /**
   * Get an overview of what access is defined for all Agents with respect to a given
   * Resource.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for the returned Agents. If
   *   additional restrictions are set up to apply to the listed Agents in a
   *   particular situation, those will not be reflected in the return value of
   *   this function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @returns The access information to the Resource, grouped by Agent.
   * @since 1.5.0
   */
  async function getAgentAccessAll$1(resourceUrl, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          return internal_getAgentAccessAll(acpData);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getAgentAccessAll$2(resourceInfo, options);
      }
      return null;
  }
  /**
   * Get an overview of what access is defined for a given Group.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for the given Group. If
   *   additional restrictions are set up to apply to the given Group in a
   *   particular situation, those will not be reflected in the return value of
   *   this function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @param webId WebID of the Group you want to get the access for.
   * @since 1.5.0
   * @deprecated Access Control Policies will no longer support vcard:Group. Use the mechanism-specific access API's if you want to define access for groups of people.
   */
  async function getGroupAccess(resourceUrl, webId, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          return internal_getGroupAccess(acpData, webId);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getGroupAccess$1(resourceInfo, webId, options);
      }
      return null;
  }
  /**
   * Get an overview of what access is defined for all Groups with respect to a given
   * Resource.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for the returned Groups. If
   *   additional restrictions are set up to apply to the listed Groups in a
   *   particular situation, those will not be reflected in the return value of
   *   this function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @returns The access information to the Resource, sorted by Group.
   * @since 1.5.0
   * @deprecated Access Control Policies will no longer support vcard:Group. Use the mechanism-specific access API's if you want to define access for groups of people.
   */
  async function getGroupAccessAll(resourceUrl, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          return internal_getGroupAccessAll(acpData);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getGroupAccessAll$1(resourceInfo, options);
      }
      return null;
  }
  /**
   * Set access to a Resource for a specific Group.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably setting access, in which case it will
   *   resolve to `null`.
   * - It will only set access explicitly for the given Group. In other words,
   *   additional restrictions could be present that further restrict or loosen
   *   what access the given Group has in particular circumstances.
   * - The provided access will only apply to the given Resource. In other words,
   *   if the Resource is a Container, the configured Access may not apply to
   *   contained Resources.
   * - If the current user does not have permission to view or change access for
   *   the given Resource, this function will resolve to `null`.
   *
   * Additionally, two caveats apply to users with a Pod server that uses WAC:
   * - If the Resource did not have an ACL yet, a new one will be initialised.
   *   This means that changes to the ACL of a parent Container can no longer
   *   affect access people have to this Resource, although existing access will
   *   be preserved.
   * - Setting different values for `controlRead` and `controlWrite` is not
   *   supported, and **will throw an error**. If you expect (some of) your users
   *   to have Pods implementing WAC, be sure to pass the same value for both.
   *
   * @param resourceUrl URL of the Resource you want to change the Group's access to.
   * @param groupUrl URL of the Group you want to set access for.
   * @param access What access permissions you want to set for the given Group to the given Resource. Possible properties are `read`, `append`, `write`, `controlRead` and `controlWrite`: set to `true` to allow, to `false` to stop allowing, or `undefined` to leave unchanged. Take note that `controlRead` and `controlWrite` can not have distinct values for a Pod server implementing Web Access Control; trying this will throw an error.
   * @returns What access has been set for the given Group explicitly.
   * @since 1.5.0
   * @deprecated Access Control Policies will no longer support vcard:Group. Use the mechanism-specific access API's if you want to define access for groups of people.
   */
  async function setGroupAccess(resourceUrl, groupUrl, access, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          const updatedResource = internal_setGroupAccess(resourceInfo, acpData, groupUrl, access);
          if (updatedResource) {
              try {
                  await saveAcrFor(updatedResource, options);
                  return getGroupAccess(resourceUrl, groupUrl, options);
              }
              catch (e) {
                  return null;
              }
          }
          return null;
      }
      if (hasAccessibleAcl(resourceInfo)) {
          if (access.controlRead != access.controlWrite) {
              throw new Error(`When setting access for a Resource in a Pod implementing Web Access Control (i.e. [${getSourceIri(resourceInfo)}]), ` + "`controlRead` and `controlWrite` should have the same value.");
          }
          const wacAccess = access;
          await setGroupResourceAccess(resourceInfo, groupUrl, wacAccess, options);
          return await getGroupAccess$1(resourceInfo, groupUrl, options);
      }
      return null;
  }
  /**
   * Get an overview of what access is defined for everyone.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for everyone. If
   *   additional restrictions are set up to apply to users in a particular
   *   situation, those will not be reflected in the return value of this
   *   function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @since 1.5.0
   */
  async function getPublicAccess$1(resourceUrl, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          return internal_getPublicAccess(acpData);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getPublicAccess$2(resourceInfo, options);
      }
      return null;
  }
  /**
   * Set access to a Resource for everybody.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably setting access, in which case it will
   *   resolve to `null`.
   * - It will only set access explicitly for everybody. In other words,
   *   additional restrictions could be present that further restrict or loosen
   *   what access a user has in particular circumstances.
   * - The provided access will only apply to the given Resource. In other words,
   *   if the Resource is a Container, the configured Access may not apply to
   *   contained Resources.
   * - If the current user does not have permission to view or change access for
   *   the given Resource, this function will resolve to `null`.
   *
   * Additionally, two caveats apply to users with a Pod server that uses WAC:
   * - If the Resource did not have an ACL yet, a new one will be initialised.
   *   This means that changes to the ACL of a parent Container can no longer
   *   affect access people have to this Resource, although existing access will
   *   be preserved.
   * - Setting different values for `controlRead` and `controlWrite` is not
   *   supported, and **will throw an error**. If you expect (some of) your users
   *   to have Pods implementing WAC, be sure to pass the same value for both.
   *
   * @param resourceUrl URL of the Resource you want to change public access to.
   * @param access What access permissions you want to set for everybody to the given Resource. Possible properties are `read`, `append`, `write`, `controlRead` and `controlWrite`: set to `true` to allow, to `false` to stop allowing, or `undefined` to leave unchanged. Take note that `controlRead` and `controlWrite` can not have distinct values for a Pod server implementing Web Access Control; trying this will throw an error.
   * @returns What access has been set for everybody explicitly.
   * @since 1.5.0
   */
  async function setPublicAccess$1(resourceUrl, access, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndRules(resourceInfo, options);
          const updatedResource = internal_setPublicAccess(resourceInfo, acpData, access);
          if (updatedResource) {
              try {
                  await saveAcrFor(updatedResource, options);
                  return getPublicAccess$1(resourceUrl, options);
              }
              catch (e) {
                  return null;
              }
          }
          return null;
      }
      if (hasAccessibleAcl(resourceInfo)) {
          if (access.controlRead != access.controlWrite) {
              throw new Error(`When setting access for a Resource in a Pod implementing Web Access Control (i.e. [${getSourceIri(resourceInfo)}]), ` + "`controlRead` and `controlWrite` should have the same value.");
          }
          const wacAccess = access;
          await setPublicResourceAccess(resourceInfo, wacAccess, options);
          return await getPublicAccess$2(resourceInfo, options);
      }
      return null;
  }

  var universal_v1 = /*#__PURE__*/Object.freeze({
    __proto__: null,
    getAgentAccess: getAgentAccess$1,
    setAgentAccess: setAgentAccess$1,
    getAgentAccessAll: getAgentAccessAll$1,
    getGroupAccess: getGroupAccess,
    getGroupAccessAll: getGroupAccessAll,
    setGroupAccess: setGroupAccess,
    getPublicAccess: getPublicAccess$1,
    setPublicAccess: setPublicAccess$1,
    getAccessFor: getAccessFor,
    getAccessForAll: getAccessForAll,
    setAccessFor: setAccessFor
  });

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */

  var universal = /*#__PURE__*/Object.freeze({
    __proto__: null,
    getAccessFor: getAccessFor,
    getAccessForAll: getAccessForAll,
    setAccessFor: setAccessFor,
    getAgentAccess: getAgentAccess$1,
    setAgentAccess: setAgentAccess$1,
    getAgentAccessAll: getAgentAccessAll$1,
    getGroupAccess: getGroupAccess,
    getGroupAccessAll: getGroupAccessAll,
    setGroupAccess: setGroupAccess,
    getPublicAccess: getPublicAccess$1,
    setPublicAccess: setPublicAccess$1
  });

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  async function getAccessFor(resourceUrl, actorType, actor = internal_defaultFetchOptions, options = internal_defaultFetchOptions) {
      if (actorType === "agent") {
          if (typeof actor !== "string") {
              throw new Error("When reading Agent-specific access, the given agent cannot be left undefined.");
          }
          return await getAgentAccess$1(resourceUrl, actor, options);
      }
      if (actorType === "group") {
          if (typeof actor !== "string") {
              throw new Error("When reading Group-specific access, the given group cannot be left undefined.");
          }
          return await getGroupAccess(resourceUrl, actor, options);
      }
      if (actorType === "public") {
          if (typeof actor === "string") {
              throw new Error(`When reading public access, no actor type should be specified (here [${actor}]).`);
          }
          return await getPublicAccess$1(resourceUrl, actor);
      }
      return null;
  }
  /**
   * Get an overview of what access is defined for a given set of actors: all Agents
   * or all Groups.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for the given actor (Agent
   *   or Group). If additional restrictions are set up to apply to the given Agent
   *   in a particular situation, those will not be reflected in the return value
   *   of this function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @param actorType type of actor whose access is being read.
   * @returns What access is set for the given resource, grouped by resp. Agent or Group.
   * @since 1.5.0
   */
  async function getAccessForAll(resourceUrl, actorType, options = internal_defaultFetchOptions) {
      if (actorType === "agent") {
          return await getAgentAccessAll$1(resourceUrl, options);
      }
      if (actorType === "group") {
          return await getGroupAccessAll(resourceUrl, options);
      }
      return null;
  }
  async function setAccessFor(resourceUrl, actorType, access, actor = internal_defaultFetchOptions, options = internal_defaultFetchOptions) {
      if (actorType === "agent") {
          if (typeof actor !== "string") {
              throw new Error("When writing Agent-specific access, the given agent cannot be left undefined.");
          }
          return await setAgentAccess$1(resourceUrl, actor, access, options);
      }
      if (actorType === "group") {
          if (typeof actor !== "string") {
              throw new Error("When writing Group-specific access, the given group cannot be left undefined.");
          }
          return await setGroupAccess(resourceUrl, actor, access, options);
      }
      if (actorType === "public") {
          if (typeof actor === "string") {
              throw new Error(`When writing public access, no actor type should be specified (here [${actor}]).`);
          }
          return await setPublicAccess$1(resourceUrl, access, actor);
      }
      return null;
  }

  /**
   * Copyright 2021 Inrupt Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal in
   * the Software without restriction, including without limitation the rights to use,
   * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
   * Software, and to permit persons to whom the Software is furnished to do so,
   * subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
   * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */
  /**
   * Get an overview of what access is defined for a given Agent.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for the given Agent. If
   *   additional restrictions are set up to apply to the given Agent in a
   *   particular situation, those will not be reflected in the return value of
   *   this function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @param webId WebID of the Agent you want to get the access for.
   * @since 1.5.0
   */
  async function getAgentAccess(resourceUrl, webId, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndMatchers(resourceInfo, options);
          return internal_getAgentAccess$1(acpData, webId);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getAgentAccess$2(resourceInfo, webId, options);
      }
      return null;
  }
  /**
   * Set access to a Resource for a specific Agent.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably setting access, in which case it will
   *   resolve to `null`.
   * - It will only set access explicitly for the given Agent. In other words,
   *   additional restrictions could be present that further restrict or loosen
   *   what access the given Agent has in particular circumstances.
   * - The provided access will only apply to the given Resource. In other words,
   *   if the Resource is a Container, the configured Access may not apply to
   *   contained Resources.
   * - If the current user does not have permission to view or change access for
   *   the given Resource, this function will resolve to `null`.
   *
   * Additionally, two caveats apply to users with a Pod server that uses WAC:
   * - If the Resource did not have an ACL yet, a new one will be initialised.
   *   This means that changes to the ACL of a parent Container can no longer
   *   affect access people have to this Resource, although existing access will
   *   be preserved.
   * - Setting different values for `controlRead` and `controlWrite` is not
   *   supported, and **will throw an error**. If you expect (some of) your users
   *   to have Pods implementing WAC, be sure to pass the same value for both.
   *
   * @param resourceUrl URL of the Resource you want to change the Agent's access to.
   * @param webId WebID of the Agent you want to set access for.
   * @param access What access permissions you want to set for the given Agent to the given Resource. Possible properties are `read`, `append`, `write`, `controlRead` and `controlWrite`: set to `true` to allow, to `false` to stop allowing, or `undefined` to leave unchanged. Take note that `controlRead` and `controlWrite` can not have distinct values for a Pod server implementing Web Access Control; trying this will throw an error.
   * @returns What access has been set for the given Agent explicitly.
   * @since 1.5.0
   */
  async function setAgentAccess(resourceUrl, webId, access, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndMatchers(resourceInfo, options);
          const updatedResource = internal_setAgentAccess$1(resourceInfo, acpData, webId, access);
          if (updatedResource) {
              try {
                  await saveAcrFor(updatedResource, options);
                  return await getAgentAccess(resourceUrl, webId, options);
              }
              catch (e) {
                  return null;
              }
          }
          return null;
      }
      if (hasAccessibleAcl(resourceInfo)) {
          if (access.controlRead != access.controlWrite) {
              throw new Error(`When setting access for a Resource in a Pod implementing Web Access Control (i.e. [${getSourceIri(resourceInfo)}]), ` + "`controlRead` and `controlWrite` should have the same value.");
          }
          const wacAccess = access;
          await setAgentResourceAccess(resourceInfo, webId, wacAccess, options);
          return await getAgentAccess$2(resourceInfo, webId, options);
      }
      return null;
  }
  /**
   * Get an overview of what access is defined for all Agents with respect to a given
   * Resource.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for the returned Agents. If
   *   additional restrictions are set up to apply to the listed Agents in a
   *   particular situation, those will not be reflected in the return value of
   *   this function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @returns The access information to the Resource, grouped by Agent.
   * @since 1.5.0
   */
  async function getAgentAccessAll(resourceUrl, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndMatchers(resourceInfo, options);
          return internal_getAgentAccessAll$1(acpData);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getAgentAccessAll$2(resourceInfo, options);
      }
      return null;
  }
  /**
   * Get an overview of what access is defined for everyone.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably reading access, in which case it will
   *   resolve to `null`.
   * - It will only return access specified explicitly for everyone. If
   *   additional restrictions are set up to apply to users in a particular
   *   situation, those will not be reflected in the return value of this
   *   function.
   * - It will only return access specified explicitly for the given Resource.
   *   In other words, if the Resource is a Container, the returned Access may not
   *   apply to contained Resources.
   * - If the current user does not have permission to view access for the given
   *   Resource, this function will resolve to `null`.
   *
   * @param resourceUrl URL of the Resource you want to read the access for.
   * @since 1.5.0
   */
  async function getPublicAccess(resourceUrl, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndMatchers(resourceInfo, options);
          return internal_getPublicAccess$1(acpData);
      }
      if (hasAccessibleAcl(resourceInfo)) {
          return await getPublicAccess$2(resourceInfo, options);
      }
      return null;
  }
  /**
   * Set access to a Resource for everybody.
   *
   * This function works with Solid Pods that implement either the Web Access
   * Control spec or the Access Control Policies proposal, with some caveats:
   *
   * - If access to the given Resource has been set using anything other than the
   *   functions in this module, it is possible that it has been set in a way that
   *   prevents this function from reliably setting access, in which case it will
   *   resolve to `null`.
   * - It will only set access explicitly for everybody. In other words,
   *   additional restrictions could be present that further restrict or loosen
   *   what access a user has in particular circumstances.
   * - The provided access will only apply to the given Resource. In other words,
   *   if the Resource is a Container, the configured Access may not apply to
   *   contained Resources.
   * - If the current user does not have permission to view or change access for
   *   the given Resource, this function will resolve to `null`.
   *
   * Additionally, two caveats apply to users with a Pod server that uses WAC:
   * - If the Resource did not have an ACL yet, a new one will be initialised.
   *   This means that changes to the ACL of a parent Container can no longer
   *   affect access people have to this Resource, although existing access will
   *   be preserved.
   * - Setting different values for `controlRead` and `controlWrite` is not
   *   supported, and **will throw an error**. If you expect (some of) your users
   *   to have Pods implementing WAC, be sure to pass the same value for both.
   *
   * @param resourceUrl URL of the Resource you want to change public access to.
   * @param access What access permissions you want to set for everybody to the given Resource. Possible properties are `read`, `append`, `write`, `controlRead` and `controlWrite`: set to `true` to allow, to `false` to stop allowing, or `undefined` to leave unchanged. Take note that `controlRead` and `controlWrite` can not have distinct values for a Pod server implementing Web Access Control; trying this will throw an error.
   * @returns What access has been set for everybody explicitly.
   * @since 1.5.0
   */
  async function setPublicAccess(resourceUrl, access, options = internal_defaultFetchOptions) {
      const resourceInfo = await getResourceInfoWithAcr(resourceUrl, options);
      if (hasAccessibleAcr(resourceInfo)) {
          const acpData = await internal_getPoliciesAndMatchers(resourceInfo, options);
          const updatedResource = internal_setPublicAccess$1(resourceInfo, acpData, access);
          if (updatedResource) {
              try {
                  await saveAcrFor(updatedResource, options);
                  return getPublicAccess(resourceUrl, options);
              }
              catch (e) {
                  return null;
              }
          }
          return null;
      }
      if (hasAccessibleAcl(resourceInfo)) {
          if (access.controlRead != access.controlWrite) {
              throw new Error(`When setting access for a Resource in a Pod implementing Web Access Control (i.e. [${getSourceIri(resourceInfo)}]), ` + "`controlRead` and `controlWrite` should have the same value.");
          }
          const wacAccess = access;
          await setPublicResourceAccess(resourceInfo, wacAccess, options);
          return await getPublicAccess$2(resourceInfo, options);
      }
      return null;
  }

  var universal_v2 = /*#__PURE__*/Object.freeze({
    __proto__: null,
    getAgentAccess: getAgentAccess,
    setAgentAccess: setAgentAccess,
    getAgentAccessAll: getAgentAccessAll,
    getPublicAccess: getPublicAccess,
    setPublicAccess: setPublicAccess,
    getAccessFor: getAccessFor,
    getAccessForAll: getAccessForAll,
    setAccessFor: setAccessFor
  });

  const RDF  = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
      XSD  = 'http://www.w3.org/2001/XMLSchema#',
      SWAP = 'http://www.w3.org/2000/10/swap/';

  var namespaces = {
    xsd: {
      decimal: `${XSD}decimal`,
      boolean: `${XSD}boolean`,
      double:  `${XSD}double`,
      integer: `${XSD}integer`,
      string:  `${XSD}string`,
    },
    rdf: {
      type:       `${RDF}type`,
      nil:        `${RDF}nil`,
      first:      `${RDF}first`,
      rest:       `${RDF}rest`,
      langString: `${RDF}langString`,
    },
    owl: {
      sameAs: 'http://www.w3.org/2002/07/owl#sameAs',
    },
    r: {
      forSome: `${SWAP}reify#forSome`,
      forAll:  `${SWAP}reify#forAll`,
    },
    log: {
      implies: `${SWAP}log#implies`,
    },
  };

  /*! queue-microtask. MIT License. Feross Aboukhadijeh <https://feross.org/opensource> */

  let promise;

  var queueMicrotask_1 = typeof queueMicrotask === 'function'
    ? queueMicrotask.bind(typeof window !== 'undefined' ? window : commonjsGlobal)
    // reuse resolved promise, and allocate it lazily
    : cb => (promise || (promise = Promise.resolve()))
      .then(cb)
      .catch(err => setTimeout(() => { throw err }, 0));

  var queueMicrotask$1 = queueMicrotask_1;

  // **N3Lexer** tokenizes N3 documents.

  const { xsd: xsd$2 } = namespaces;

  // Regular expression and replacement string to escape N3 strings
  const escapeSequence = /\\u([a-fA-F0-9]{4})|\\U([a-fA-F0-9]{8})|\\([^])/g;
  const escapeReplacements = {
    '\\': '\\', "'": "'", '"': '"',
    'n': '\n', 'r': '\r', 't': '\t', 'f': '\f', 'b': '\b',
    '_': '_', '~': '~', '.': '.', '-': '-', '!': '!', '$': '$', '&': '&',
    '(': '(', ')': ')', '*': '*', '+': '+', ',': ',', ';': ';', '=': '=',
    '/': '/', '?': '?', '#': '#', '@': '@', '%': '%',
  };
  const illegalIriChars = /[\x00-\x20<>\\"\{\}\|\^\`]/;

  const lineModeRegExps = {
    _iri: true,
    _unescapedIri: true,
    _simpleQuotedString: true,
    _langcode: true,
    _blank: true,
    _newline: true,
    _comment: true,
    _whitespace: true,
    _endOfFile: true,
  };
  const invalidRegExp = /$0^/;

  // ## Constructor
  class N3Lexer {
    constructor(options) {
      // ## Regular expressions
      // It's slightly faster to have these as properties than as in-scope variables
      this._iri = /^<((?:[^ <>{}\\]|\\[uU])+)>[ \t]*/; // IRI with escape sequences; needs sanity check after unescaping
      this._unescapedIri = /^<([^\x00-\x20<>\\"\{\}\|\^\`]*)>[ \t]*/; // IRI without escape sequences; no unescaping
      this._simpleQuotedString = /^"([^"\\\r\n]*)"(?=[^"])/; // string without escape sequences
      this._simpleApostropheString = /^'([^'\\\r\n]*)'(?=[^'])/;
      this._langcode = /^@([a-z]+(?:-[a-z0-9]+)*)(?=[^a-z0-9\-])/i;
      this._prefix = /^((?:[A-Za-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])(?:\.?[\-0-9A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])*)?:(?=[#\s<])/;
      this._prefixed = /^((?:[A-Za-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])(?:\.?[\-0-9A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])*)?:((?:(?:[0-:A-Z_a-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff]|%[0-9a-fA-F]{2}|\\[!#-\/;=?\-@_~])(?:(?:[\.\-0-:A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff]|%[0-9a-fA-F]{2}|\\[!#-\/;=?\-@_~])*(?:[\-0-:A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff]|%[0-9a-fA-F]{2}|\\[!#-\/;=?\-@_~]))?)?)(?:[ \t]+|(?=\.?[,;!\^\s#()\[\]\{\}"'<>]))/;
      this._variable = /^\?(?:(?:[A-Z_a-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])(?:[\-0-:A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])*)(?=[.,;!\^\s#()\[\]\{\}"'<>])/;
      this._blank = /^_:((?:[0-9A-Z_a-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])(?:\.?[\-0-9A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])*)(?:[ \t]+|(?=\.?[,;:\s#()\[\]\{\}"'<>]))/;
      this._number = /^[\-+]?(?:(\d+\.\d*|\.?\d+)[eE][\-+]?|\d*(\.)?)\d+(?=\.?[,;:\s#()\[\]\{\}"'<>])/;
      this._boolean = /^(?:true|false)(?=[.,;\s#()\[\]\{\}"'<>])/;
      this._keyword = /^@[a-z]+(?=[\s#<:])/i;
      this._sparqlKeyword = /^(?:PREFIX|BASE|GRAPH)(?=[\s#<])/i;
      this._shortPredicates = /^a(?=[\s#()\[\]\{\}"'<>])/;
      this._newline = /^[ \t]*(?:#[^\n\r]*)?(?:\r\n|\n|\r)[ \t]*/;
      this._comment = /#([^\n\r]*)/;
      this._whitespace = /^[ \t]+/;
      this._endOfFile = /^(?:#[^\n\r]*)?$/;
      options = options || {};

      // In line mode (N-Triples or N-Quads), only simple features may be parsed
      if (this._lineMode = !!options.lineMode) {
        this._n3Mode = false;
        // Don't tokenize special literals
        for (const key in this) {
          if (!(key in lineModeRegExps) && this[key] instanceof RegExp)
            this[key] = invalidRegExp;
        }
      }
      // When not in line mode, enable N3 functionality by default
      else {
        this._n3Mode = options.n3 !== false;
      }
      // Don't output comment tokens by default
      this._comments = !!options.comments;
      // Cache the last tested closing position of long literals
      this._literalClosingPos = 0;
    }

    // ## Private methods

    // ### `_tokenizeToEnd` tokenizes as for as possible, emitting tokens through the callback
    _tokenizeToEnd(callback, inputFinished) {
      // Continue parsing as far as possible; the loop will return eventually
      let input = this._input;
      const outputComments = this._comments;
      while (true) {
        // Count and skip whitespace lines
        let whiteSpaceMatch, comment;
        while (whiteSpaceMatch = this._newline.exec(input)) {
          // Try to find a comment
          if (outputComments && (comment = this._comment.exec(whiteSpaceMatch[0])))
            callback(null, { line: this._line, type: 'comment', value: comment[1], prefix: '' });
          // Advance the input
          input = input.substr(whiteSpaceMatch[0].length, input.length);
          this._line++;
        }
        // Skip whitespace on current line
        if (!whiteSpaceMatch && (whiteSpaceMatch = this._whitespace.exec(input)))
          input = input.substr(whiteSpaceMatch[0].length, input.length);

        // Stop for now if we're at the end
        if (this._endOfFile.test(input)) {
          // If the input is finished, emit EOF
          if (inputFinished) {
            // Try to find a final comment
            if (outputComments && (comment = this._comment.exec(input)))
              callback(null, { line: this._line, type: 'comment', value: comment[1], prefix: '' });
            callback(input = null, { line: this._line, type: 'eof', value: '', prefix: '' });
          }
          return this._input = input;
        }

        // Look for specific token types based on the first character
        const line = this._line, firstChar = input[0];
        let type = '', value = '', prefix = '',
            match = null, matchLength = 0, inconclusive = false;
        switch (firstChar) {
        case '^':
          // We need at least 3 tokens lookahead to distinguish ^^<IRI> and ^^pre:fixed
          if (input.length < 3)
            break;
          // Try to match a type
          else if (input[1] === '^') {
            this._previousMarker = '^^';
            // Move to type IRI or prefixed name
            input = input.substr(2);
            if (input[0] !== '<') {
              inconclusive = true;
              break;
            }
          }
          // If no type, it must be a path expression
          else {
            if (this._n3Mode) {
              matchLength = 1;
              type = '^';
            }
            break;
          }
          // Fall through in case the type is an IRI
        case '<':
          // Try to find a full IRI without escape sequences
          if (match = this._unescapedIri.exec(input))
            type = 'IRI', value = match[1];
          // Try to find a full IRI with escape sequences
          else if (match = this._iri.exec(input)) {
            value = this._unescape(match[1]);
            if (value === null || illegalIriChars.test(value))
              return reportSyntaxError(this);
            type = 'IRI';
          }
          // Try to find a nested triple
          else if (input.length > 1 && input[1] === '<')
            type = '<<', matchLength = 2;
          // Try to find a backwards implication arrow
          else if (this._n3Mode && input.length > 1 && input[1] === '=')
            type = 'inverse', matchLength = 2, value = '>';
          break;

        case '>':
          if (input.length > 1 && input[1] === '>')
            type = '>>', matchLength = 2;
          break;

        case '_':
          // Try to find a blank node. Since it can contain (but not end with) a dot,
          // we always need a non-dot character before deciding it is a blank node.
          // Therefore, try inserting a space if we're at the end of the input.
          if ((match = this._blank.exec(input)) ||
              inputFinished && (match = this._blank.exec(`${input} `)))
            type = 'blank', prefix = '_', value = match[1];
          break;

        case '"':
          // Try to find a literal without escape sequences
          if (match = this._simpleQuotedString.exec(input))
            value = match[1];
          // Try to find a literal wrapped in three pairs of quotes
          else {
            ({ value, matchLength } = this._parseLiteral(input));
            if (value === null)
              return reportSyntaxError(this);
          }
          if (match !== null || matchLength !== 0) {
            type = 'literal';
            this._literalClosingPos = 0;
          }
          break;

        case "'":
          if (!this._lineMode) {
            // Try to find a literal without escape sequences
            if (match = this._simpleApostropheString.exec(input))
              value = match[1];
            // Try to find a literal wrapped in three pairs of quotes
            else {
              ({ value, matchLength } = this._parseLiteral(input));
              if (value === null)
                return reportSyntaxError(this);
            }
            if (match !== null || matchLength !== 0) {
              type = 'literal';
              this._literalClosingPos = 0;
            }
          }
          break;

        case '?':
          // Try to find a variable
          if (this._n3Mode && (match = this._variable.exec(input)))
            type = 'var', value = match[0];
          break;

        case '@':
          // Try to find a language code
          if (this._previousMarker === 'literal' && (match = this._langcode.exec(input)))
            type = 'langcode', value = match[1];
          // Try to find a keyword
          else if (match = this._keyword.exec(input))
            type = match[0];
          break;

        case '.':
          // Try to find a dot as punctuation
          if (input.length === 1 ? inputFinished : (input[1] < '0' || input[1] > '9')) {
            type = '.';
            matchLength = 1;
            break;
          }
          // Fall through to numerical case (could be a decimal dot)

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case '+':
        case '-':
          // Try to find a number. Since it can contain (but not end with) a dot,
          // we always need a non-dot character before deciding it is a number.
          // Therefore, try inserting a space if we're at the end of the input.
          if (match = this._number.exec(input) ||
              inputFinished && (match = this._number.exec(`${input} `))) {
            type = 'literal', value = match[0];
            prefix = (typeof match[1] === 'string' ? xsd$2.double :
                      (typeof match[2] === 'string' ? xsd$2.decimal : xsd$2.integer));
          }
          break;

        case 'B':
        case 'b':
        case 'p':
        case 'P':
        case 'G':
        case 'g':
          // Try to find a SPARQL-style keyword
          if (match = this._sparqlKeyword.exec(input))
            type = match[0].toUpperCase();
          else
            inconclusive = true;
          break;

        case 'f':
        case 't':
          // Try to match a boolean
          if (match = this._boolean.exec(input))
            type = 'literal', value = match[0], prefix = xsd$2.boolean;
          else
            inconclusive = true;
          break;

        case 'a':
          // Try to find an abbreviated predicate
          if (match = this._shortPredicates.exec(input))
            type = 'abbreviation', value = 'a';
          else
            inconclusive = true;
          break;

        case '=':
          // Try to find an implication arrow or equals sign
          if (this._n3Mode && input.length > 1) {
            type = 'abbreviation';
            if (input[1] !== '>')
              matchLength = 1, value = '=';
            else
              matchLength = 2, value = '>';
          }
          break;

        case '!':
          if (!this._n3Mode)
            break;
        case ',':
        case ';':
        case '[':
        case ']':
        case '(':
        case ')':
        case '{':
        case '}':
          if (!this._lineMode) {
            matchLength = 1;
            type = firstChar;
          }
          break;

        default:
          inconclusive = true;
        }

        // Some first characters do not allow an immediate decision, so inspect more
        if (inconclusive) {
          // Try to find a prefix
          if ((this._previousMarker === '@prefix' || this._previousMarker === 'PREFIX') &&
              (match = this._prefix.exec(input)))
            type = 'prefix', value = match[1] || '';
          // Try to find a prefixed name. Since it can contain (but not end with) a dot,
          // we always need a non-dot character before deciding it is a prefixed name.
          // Therefore, try inserting a space if we're at the end of the input.
          else if ((match = this._prefixed.exec(input)) ||
                   inputFinished && (match = this._prefixed.exec(`${input} `)))
            type = 'prefixed', prefix = match[1] || '', value = this._unescape(match[2]);
        }

        // A type token is special: it can only be emitted after an IRI or prefixed name is read
        if (this._previousMarker === '^^') {
          switch (type) {
          case 'prefixed': type = 'type';    break;
          case 'IRI':      type = 'typeIRI'; break;
          default:         type = '';
          }
        }

        // What if nothing of the above was found?
        if (!type) {
          // We could be in streaming mode, and then we just wait for more input to arrive.
          // Otherwise, a syntax error has occurred in the input.
          // One exception: error on an unaccounted linebreak (= not inside a triple-quoted literal).
          if (inputFinished || (!/^'''|^"""/.test(input) && /\n|\r/.test(input)))
            return reportSyntaxError(this);
          else
            return this._input = input;
        }

        // Emit the parsed token
        const token = { line: line, type: type, value: value, prefix: prefix };
        callback(null, token);
        this.previousToken = token;
        this._previousMarker = type;
        // Advance to next part to tokenize
        input = input.substr(matchLength || match[0].length, input.length);
      }

      // Signals the syntax error through the callback
      function reportSyntaxError(self) { callback(self._syntaxError(/^\S*/.exec(input)[0])); }
    }

    // ### `_unescape` replaces N3 escape codes by their corresponding characters
    _unescape(item) {
      let invalid = false;
      const replaced = item.replace(escapeSequence, (sequence, unicode4, unicode8, escapedChar) => {
        // 4-digit unicode character
        if (typeof unicode4 === 'string')
          return String.fromCharCode(Number.parseInt(unicode4, 16));
        // 8-digit unicode character
        if (typeof unicode8 === 'string') {
          let charCode = Number.parseInt(unicode8, 16);
          return charCode <= 0xFFFF ? String.fromCharCode(Number.parseInt(unicode8, 16)) :
            String.fromCharCode(0xD800 + ((charCode -= 0x10000) >> 10), 0xDC00 + (charCode & 0x3FF));
        }
        // fixed escape sequence
        if (escapedChar in escapeReplacements)
          return escapeReplacements[escapedChar];
        // invalid escape sequence
        invalid = true;
        return '';
      });
      return invalid ? null : replaced;
    }

    // ### `_parseLiteral` parses a literal into an unescaped value
    _parseLiteral(input) {
      // Ensure we have enough lookahead to identify triple-quoted strings
      if (input.length >= 3) {
        // Identify the opening quote(s)
        const opening = input.match(/^(?:"""|"|'''|'|)/)[0];
        const openingLength = opening.length;

        // Find the next candidate closing quotes
        let closingPos = Math.max(this._literalClosingPos, openingLength);
        while ((closingPos = input.indexOf(opening, closingPos)) > 0) {
          // Count backslashes right before the closing quotes
          let backslashCount = 0;
          while (input[closingPos - backslashCount - 1] === '\\')
            backslashCount++;

          // An even number of backslashes (in particular 0)
          // means these are actual, non-escaped closing quotes
          if (backslashCount % 2 === 0) {
            // Extract and unescape the value
            const raw = input.substring(openingLength, closingPos);
            const lines = raw.split(/\r\n|\r|\n/).length - 1;
            const matchLength = closingPos + openingLength;
            // Only triple-quoted strings can be multi-line
            if (openingLength === 1 && lines !== 0 ||
                openingLength === 3 && this._lineMode)
              break;
            this._line += lines;
            return { value: this._unescape(raw), matchLength };
          }
          closingPos++;
        }
        this._literalClosingPos = input.length - openingLength + 1;
      }
      return { value: '', matchLength: 0 };
    }

    // ### `_syntaxError` creates a syntax error for the given issue
    _syntaxError(issue) {
      this._input = null;
      const err = new Error(`Unexpected "${issue}" on line ${this._line}.`);
      err.context = {
        token: undefined,
        line: this._line,
        previousToken: this.previousToken,
      };
      return err;
    }

    // ### Strips off any starting UTF BOM mark.
    _readStartingBom(input) {
      return input.startsWith('\ufeff') ? input.substr(1) : input;
    }

    // ## Public methods

    // ### `tokenize` starts the transformation of an N3 document into an array of tokens.
    // The input can be a string or a stream.
    tokenize(input, callback) {
      this._line = 1;

      // If the input is a string, continuously emit tokens through the callback until the end
      if (typeof input === 'string') {
        this._input = this._readStartingBom(input);
        // If a callback was passed, asynchronously call it
        if (typeof callback === 'function')
          queueMicrotask$1(() => this._tokenizeToEnd(callback, true));
        // If no callback was passed, tokenize synchronously and return
        else {
          const tokens = [];
          let error;
          this._tokenizeToEnd((e, t) => e ? (error = e) : tokens.push(t), true);
          if (error) throw error;
          return tokens;
        }
      }
      // Otherwise, the input must be a stream
      else {
        this._pendingBuffer = null;
        if (typeof input.setEncoding === 'function')
          input.setEncoding('utf8');
        // Adds the data chunk to the buffer and parses as far as possible
        input.on('data', data => {
          if (this._input !== null && data.length !== 0) {
            // Prepend any previous pending writes
            if (this._pendingBuffer) {
              data = Buffer.concat([this._pendingBuffer, data]);
              this._pendingBuffer = null;
            }
            // Hold if the buffer ends in an incomplete unicode sequence
            if (data[data.length - 1] & 0x80) {
              this._pendingBuffer = data;
            }
            // Otherwise, tokenize as far as possible
            else {
              // Only read a BOM at the start
              if (typeof this._input === 'undefined')
                this._input = this._readStartingBom(typeof data === 'string' ? data : data.toString());
              else
                this._input += data;
              this._tokenizeToEnd(callback, false);
            }
          }
        });
        // Parses until the end
        input.on('end', () => {
          if (typeof this._input === 'string')
            this._tokenizeToEnd(callback, true);
        });
        input.on('error', callback);
      }
    }
  }

  // **N3Util** provides N3 utility functions.

  // Tests whether the given term represents an IRI
  function isNamedNode(term) {
    return !!term && term.termType === 'NamedNode';
  }

  // Tests whether the given term represents a blank node
  function isBlankNode(term) {
    return !!term && term.termType === 'BlankNode';
  }

  // Tests whether the given term represents a literal
  function isLiteral(term) {
    return !!term && term.termType === 'Literal';
  }

  // Tests whether the given term represents a variable
  function isVariable(term) {
    return !!term && term.termType === 'Variable';
  }

  // Tests whether the given term represents the default graph
  function isDefaultGraph(term) {
    return !!term && term.termType === 'DefaultGraph';
  }

  // Tests whether the given quad is in the default graph
  function inDefaultGraph(quad) {
    return isDefaultGraph(quad.graph);
  }

  // Creates a function that prepends the given IRI to a local name
  function prefix(iri, factory) {
    return prefixes({ '': iri }, factory)('');
  }

  // Creates a function that allows registering and expanding prefixes
  function prefixes(defaultPrefixes, factory) {
    // Add all of the default prefixes
    const prefixes = Object.create(null);
    for (const prefix in defaultPrefixes)
      processPrefix(prefix, defaultPrefixes[prefix]);
    // Set the default factory if none was specified
    factory = factory || N3DataFactory;

    // Registers a new prefix (if an IRI was specified)
    // or retrieves a function that expands an existing prefix (if no IRI was specified)
    function processPrefix(prefix, iri) {
      // Create a new prefix if an IRI is specified or the prefix doesn't exist
      if (typeof iri === 'string') {
        // Create a function that expands the prefix
        const cache = Object.create(null);
        prefixes[prefix] = local => {
          return cache[local] || (cache[local] = factory.namedNode(iri + local));
        };
      }
      else if (!(prefix in prefixes)) {
        throw new Error(`Unknown prefix: ${prefix}`);
      }
      return prefixes[prefix];
    }
    return processPrefix;
  }

  var N3Util = /*#__PURE__*/Object.freeze({
    __proto__: null,
    isNamedNode: isNamedNode,
    isBlankNode: isBlankNode,
    isLiteral: isLiteral,
    isVariable: isVariable,
    isDefaultGraph: isDefaultGraph,
    inDefaultGraph: inDefaultGraph,
    prefix: prefix,
    prefixes: prefixes
  });

  // N3.js implementations of the RDF/JS core data types
  const { rdf: rdf$1, xsd: xsd$1 } = namespaces;

  // eslint-disable-next-line prefer-const
  let DEFAULTGRAPH$1;
  let _blankNodeCounter = 0;

  const escapedLiteral = /^"(.*".*)(?="[^"]*$)/;
  const quadId = /^<<("(?:""|[^"])*"[^ ]*|[^ ]+) ("(?:""|[^"])*"[^ ]*|[^ ]+) ("(?:""|[^"])*"[^ ]*|[^ ]+) ?("(?:""|[^"])*"[^ ]*|[^ ]+)?>>$/;

  // ## DataFactory singleton
  const DataFactory = {
    namedNode,
    blankNode,
    variable,
    literal,
    defaultGraph,
    quad,
    triple: quad,
  };
  var N3DataFactory = DataFactory;

  // ## Term constructor
  class Term {
    constructor(id) {
      this.id = id;
    }

    // ### The value of this term
    get value() {
      return this.id;
    }

    // ### Returns whether this object represents the same term as the other
    equals(other) {
      // If both terms were created by this library,
      // equality can be computed through ids
      if (other instanceof Term)
        return this.id === other.id;
      // Otherwise, compare term type and value
      return !!other && this.termType === other.termType &&
                        this.value    === other.value;
    }

    // ### Returns a plain object representation of this term
    toJSON() {
      return {
        termType: this.termType,
        value:    this.value,
      };
    }
  }


  // ## NamedNode constructor
  class NamedNode extends Term {
    // ### The term type of this term
    get termType() {
      return 'NamedNode';
    }
  }

  // ## Literal constructor
  class Literal extends Term {
    // ### The term type of this term
    get termType() {
      return 'Literal';
    }

    // ### The text value of this literal
    get value() {
      return this.id.substring(1, this.id.lastIndexOf('"'));
    }

    // ### The language of this literal
    get language() {
      // Find the last quotation mark (e.g., '"abc"@en-us')
      const id = this.id;
      let atPos = id.lastIndexOf('"') + 1;
      // If "@" it follows, return the remaining substring; empty otherwise
      return atPos < id.length && id[atPos++] === '@' ? id.substr(atPos).toLowerCase() : '';
    }

    // ### The datatype IRI of this literal
    get datatype() {
      return new NamedNode(this.datatypeString);
    }

    // ### The datatype string of this literal
    get datatypeString() {
      // Find the last quotation mark (e.g., '"abc"^^http://ex.org/types#t')
      const id = this.id, dtPos = id.lastIndexOf('"') + 1;
      const char = dtPos < id.length ? id[dtPos] : '';
      // If "^" it follows, return the remaining substring
      return char === '^' ? id.substr(dtPos + 2) :
             // If "@" follows, return rdf:langString; xsd:string otherwise
             (char !== '@' ? xsd$1.string : rdf$1.langString);
    }

    // ### Returns whether this object represents the same term as the other
    equals(other) {
      // If both literals were created by this library,
      // equality can be computed through ids
      if (other instanceof Literal)
        return this.id === other.id;
      // Otherwise, compare term type, value, language, and datatype
      return !!other && !!other.datatype &&
                        this.termType === other.termType &&
                        this.value    === other.value    &&
                        this.language === other.language &&
                        this.datatype.value === other.datatype.value;
    }

    toJSON() {
      return {
        termType: this.termType,
        value:    this.value,
        language: this.language,
        datatype: { termType: 'NamedNode', value: this.datatypeString },
      };
    }
  }

  // ## BlankNode constructor
  class BlankNode extends Term {
    constructor(name) {
      super(`_:${name}`);
    }

    // ### The term type of this term
    get termType() {
      return 'BlankNode';
    }

    // ### The name of this blank node
    get value() {
      return this.id.substr(2);
    }
  }

  class Variable extends Term {
    constructor(name) {
      super(`?${name}`);
    }

    // ### The term type of this term
    get termType() {
      return 'Variable';
    }

    // ### The name of this variable
    get value() {
      return this.id.substr(1);
    }
  }

  // ## DefaultGraph constructor
  class DefaultGraph extends Term {
    constructor() {
      super('');
      return DEFAULTGRAPH$1 || this;
    }

    // ### The term type of this term
    get termType() {
      return 'DefaultGraph';
    }

    // ### Returns whether this object represents the same term as the other
    equals(other) {
      // If both terms were created by this library,
      // equality can be computed through strict equality;
      // otherwise, compare term types.
      return (this === other) || (!!other && (this.termType === other.termType));
    }
  }

  // ## DefaultGraph singleton
  DEFAULTGRAPH$1 = new DefaultGraph();


  // ### Constructs a term from the given internal string ID
  function termFromId(id, factory) {
    factory = factory || DataFactory;

    // Falsy value or empty string indicate the default graph
    if (!id)
      return factory.defaultGraph();

    // Identify the term type based on the first character
    switch (id[0]) {
    case '?':
      return factory.variable(id.substr(1));
    case '_':
      return factory.blankNode(id.substr(2));
    case '"':
      // Shortcut for internal literals
      if (factory === DataFactory)
        return new Literal(id);
      // Literal without datatype or language
      if (id[id.length - 1] === '"')
        return factory.literal(id.substr(1, id.length - 2));
      // Literal with datatype or language
      const endPos = id.lastIndexOf('"', id.length - 1);
      return factory.literal(id.substr(1, endPos - 1),
              id[endPos + 1] === '@' ? id.substr(endPos + 2)
                                     : factory.namedNode(id.substr(endPos + 3)));
    case '<':
      const components = quadId.exec(id);
      return factory.quad(
        termFromId(unescapeQuotes(components[1]), factory),
        termFromId(unescapeQuotes(components[2]), factory),
        termFromId(unescapeQuotes(components[3]), factory),
        components[4] && termFromId(unescapeQuotes(components[4]), factory)
      );
    default:
      return factory.namedNode(id);
    }
  }

  // ### Constructs an internal string ID from the given term or ID string
  function termToId(term) {
    if (typeof term === 'string')
      return term;
    if (term instanceof Term && term.termType !== 'Quad')
      return term.id;
    if (!term)
      return DEFAULTGRAPH$1.id;

    // Term instantiated with another library
    switch (term.termType) {
    case 'NamedNode':    return term.value;
    case 'BlankNode':    return `_:${term.value}`;
    case 'Variable':     return `?${term.value}`;
    case 'DefaultGraph': return '';
    case 'Literal':      return `"${term.value}"${
    term.language ? `@${term.language}` :
      (term.datatype && term.datatype.value !== xsd$1.string ? `^^${term.datatype.value}` : '')}`;
    case 'Quad':
      // To identify RDF* quad components, we escape quotes by doubling them.
      // This avoids the overhead of backslash parsing of Turtle-like syntaxes.
      return `<<${
        escapeQuotes(termToId(term.subject))
      } ${
        escapeQuotes(termToId(term.predicate))
      } ${
        escapeQuotes(termToId(term.object))
      }${
        (isDefaultGraph(term.graph)) ? '' : ` ${termToId(term.graph)}`
      }>>`;
    default: throw new Error(`Unexpected termType: ${term.termType}`);
    }
  }


  // ## Quad constructor
  class Quad extends Term {
    constructor(subject, predicate, object, graph) {
      super('');
      this._subject   = subject;
      this._predicate = predicate;
      this._object    = object;
      this._graph     = graph || DEFAULTGRAPH$1;
    }

    // ### The term type of this term
    get termType() {
      return 'Quad';
    }

    get subject() {
      return this._subject;
    }

    get predicate() {
      return this._predicate;
    }

    get object() {
      return this._object;
    }

    get graph() {
      return this._graph;
    }

    // ### Returns a plain object representation of this quad
    toJSON() {
      return {
        termType:  this.termType,
        subject:   this._subject.toJSON(),
        predicate: this._predicate.toJSON(),
        object:    this._object.toJSON(),
        graph:     this._graph.toJSON(),
      };
    }

    // ### Returns whether this object represents the same quad as the other
    equals(other) {
      return !!other && this._subject.equals(other.subject)     &&
                        this._predicate.equals(other.predicate) &&
                        this._object.equals(other.object)       &&
                        this._graph.equals(other.graph);
    }
  }

  // ### Escapes the quotes within the given literal
  function escapeQuotes(id) {
    return id.replace(escapedLiteral, (_, quoted) => `"${quoted.replace(/"/g, '""')}`);
  }

  // ### Unescapes the quotes within the given literal
  function unescapeQuotes(id) {
    return id.replace(escapedLiteral, (_, quoted) => `"${quoted.replace(/""/g, '"')}`);
  }

  // ### Creates an IRI
  function namedNode(iri) {
    return new NamedNode(iri);
  }

  // ### Creates a blank node
  function blankNode(name) {
    return new BlankNode(name || `n3-${_blankNodeCounter++}`);
  }

  // ### Creates a literal
  function literal(value, languageOrDataType) {
    // Create a language-tagged string
    if (typeof languageOrDataType === 'string')
      return new Literal(`"${value}"@${languageOrDataType.toLowerCase()}`);

    // Automatically determine datatype for booleans and numbers
    let datatype = languageOrDataType ? languageOrDataType.value : '';
    if (datatype === '') {
      // Convert a boolean
      if (typeof value === 'boolean')
        datatype = xsd$1.boolean;
      // Convert an integer or double
      else if (typeof value === 'number') {
        if (Number.isFinite(value))
          datatype = Number.isInteger(value) ? xsd$1.integer : xsd$1.double;
        else {
          datatype = xsd$1.double;
          if (!Number.isNaN(value))
            value = value > 0 ? 'INF' : '-INF';
        }
      }
    }

    // Create a datatyped literal
    return (datatype === '' || datatype === xsd$1.string) ?
      new Literal(`"${value}"`) :
      new Literal(`"${value}"^^${datatype}`);
  }

  // ### Creates a variable
  function variable(name) {
    return new Variable(name);
  }

  // ### Returns the default graph
  function defaultGraph() {
    return DEFAULTGRAPH$1;
  }

  // ### Creates a quad
  function quad(subject, predicate, object, graph) {
    return new Quad(subject, predicate, object, graph);
  }

  // **N3Parser** parses N3 documents.

  let blankNodePrefix = 0;

  // ## Constructor
  class N3Parser {
    constructor(options) {
      this._contextStack = [];
      this._graph = null;

      // Set the document IRI
      options = options || {};
      this._setBase(options.baseIRI);
      options.factory && initDataFactory(this, options.factory);

      // Set supported features depending on the format
      const format = (typeof options.format === 'string') ?
                   options.format.match(/\w*$/)[0].toLowerCase() : '',
          isTurtle = /turtle/.test(format), isTriG = /trig/.test(format),
          isNTriples = /triple/.test(format), isNQuads = /quad/.test(format),
          isN3 = this._n3Mode = /n3/.test(format),
          isLineMode = isNTriples || isNQuads;
      if (!(this._supportsNamedGraphs = !(isTurtle || isN3)))
        this._readPredicateOrNamedGraph = this._readPredicate;
      // Support triples in other graphs
      this._supportsQuads = !(isTurtle || isTriG || isNTriples || isN3);
      // Support nesting of triples
      this._supportsRDFStar = format === '' || /star|\*$/.test(format);
      // Disable relative IRIs in N-Triples or N-Quads mode
      if (isLineMode)
        this._resolveRelativeIRI = iri => { return null; };
      this._blankNodePrefix = typeof options.blankNodePrefix !== 'string' ? '' :
                                options.blankNodePrefix.replace(/^(?!_:)/, '_:');
      this._lexer = options.lexer || new N3Lexer({ lineMode: isLineMode, n3: isN3 });
      // Disable explicit quantifiers by default
      this._explicitQuantifiers = !!options.explicitQuantifiers;
    }

    // ## Static class methods

    // ### `_resetBlankNodePrefix` restarts blank node prefix identification
    static _resetBlankNodePrefix() {
      blankNodePrefix = 0;
    }

    // ## Private methods

    // ### `_setBase` sets the base IRI to resolve relative IRIs
    _setBase(baseIRI) {
      if (!baseIRI) {
        this._base = '';
        this._basePath = '';
      }
      else {
        // Remove fragment if present
        const fragmentPos = baseIRI.indexOf('#');
        if (fragmentPos >= 0)
          baseIRI = baseIRI.substr(0, fragmentPos);
        // Set base IRI and its components
        this._base = baseIRI;
        this._basePath   = baseIRI.indexOf('/') < 0 ? baseIRI :
                           baseIRI.replace(/[^\/?]*(?:\?.*)?$/, '');
        baseIRI = baseIRI.match(/^(?:([a-z][a-z0-9+.-]*:))?(?:\/\/[^\/]*)?/i);
        this._baseRoot   = baseIRI[0];
        this._baseScheme = baseIRI[1];
      }
    }

    // ### `_saveContext` stores the current parsing context
    // when entering a new scope (list, blank node, formula)
    _saveContext(type, graph, subject, predicate, object) {
      const n3Mode = this._n3Mode;
      this._contextStack.push({
        subject: subject, predicate: predicate, object: object,
        graph: graph, type: type,
        inverse: n3Mode ? this._inversePredicate : false,
        blankPrefix: n3Mode ? this._prefixes._ : '',
        quantified: n3Mode ? this._quantified : null,
      });
      // The settings below only apply to N3 streams
      if (n3Mode) {
        // Every new scope resets the predicate direction
        this._inversePredicate = false;
        // In N3, blank nodes are scoped to a formula
        // (using a dot as separator, as a blank node label cannot start with it)
        this._prefixes._ = (this._graph ? `${this._graph.id.substr(2)}.` : '.');
        // Quantifiers are scoped to a formula
        this._quantified = Object.create(this._quantified);
      }
    }

    // ### `_restoreContext` restores the parent context
    // when leaving a scope (list, blank node, formula)
    _restoreContext() {
      const context = this._contextStack.pop(), n3Mode = this._n3Mode;
      this._subject   = context.subject;
      this._predicate = context.predicate;
      this._object    = context.object;
      this._graph     = context.graph;
      // The settings below only apply to N3 streams
      if (n3Mode) {
        this._inversePredicate = context.inverse;
        this._prefixes._ = context.blankPrefix;
        this._quantified = context.quantified;
      }
    }

    // ### `_readInTopContext` reads a token when in the top context
    _readInTopContext(token) {
      switch (token.type) {
      // If an EOF token arrives in the top context, signal that we're done
      case 'eof':
        if (this._graph !== null)
          return this._error('Unclosed graph', token);
        delete this._prefixes._;
        return this._callback(null, null, this._prefixes);
      // It could be a prefix declaration
      case 'PREFIX':
        this._sparqlStyle = true;
      case '@prefix':
        return this._readPrefix;
      // It could be a base declaration
      case 'BASE':
        this._sparqlStyle = true;
      case '@base':
        return this._readBaseIRI;
      // It could be a graph
      case '{':
        if (this._supportsNamedGraphs) {
          this._graph = '';
          this._subject = null;
          return this._readSubject;
        }
      case 'GRAPH':
        if (this._supportsNamedGraphs)
          return this._readNamedGraphLabel;
      // Otherwise, the next token must be a subject
      default:
        return this._readSubject(token);
      }
    }

    // ### `_readEntity` reads an IRI, prefixed name, blank node, or variable
    _readEntity(token, quantifier) {
      let value;
      switch (token.type) {
      // Read a relative or absolute IRI
      case 'IRI':
      case 'typeIRI':
        const iri = this._resolveIRI(token.value);
        if (iri === null)
          return this._error('Invalid IRI', token);
        value = this._namedNode(iri);
        break;
      // Read a prefixed name
      case 'type':
      case 'prefixed':
        const prefix = this._prefixes[token.prefix];
        if (prefix === undefined)
          return this._error(`Undefined prefix "${token.prefix}:"`, token);
        value = this._namedNode(prefix + token.value);
        break;
      // Read a blank node
      case 'blank':
        value = this._blankNode(this._prefixes[token.prefix] + token.value);
        break;
      // Read a variable
      case 'var':
        value = this._variable(token.value.substr(1));
        break;
      // Everything else is not an entity
      default:
        return this._error(`Expected entity but got ${token.type}`, token);
      }
      // In N3 mode, replace the entity if it is quantified
      if (!quantifier && this._n3Mode && (value.id in this._quantified))
        value = this._quantified[value.id];
      return value;
    }

    // ### `_readSubject` reads a quad's subject
    _readSubject(token) {
      this._predicate = null;
      switch (token.type) {
      case '[':
        // Start a new quad with a new blank node as subject
        this._saveContext('blank', this._graph,
                          this._subject = this._blankNode(), null, null);
        return this._readBlankNodeHead;
      case '(':
        // Start a new list
        this._saveContext('list', this._graph, this.RDF_NIL, null, null);
        this._subject = null;
        return this._readListItem;
      case '{':
        // Start a new formula
        if (!this._n3Mode)
          return this._error('Unexpected graph', token);
        this._saveContext('formula', this._graph,
                          this._graph = this._blankNode(), null, null);
        return this._readSubject;
      case '}':
         // No subject; the graph in which we are reading is closed instead
        return this._readPunctuation(token);
      case '@forSome':
        if (!this._n3Mode)
          return this._error('Unexpected "@forSome"', token);
        this._subject = null;
        this._predicate = this.N3_FORSOME;
        this._quantifier = this._blankNode;
        return this._readQuantifierList;
      case '@forAll':
        if (!this._n3Mode)
          return this._error('Unexpected "@forAll"', token);
        this._subject = null;
        this._predicate = this.N3_FORALL;
        this._quantifier = this._variable;
        return this._readQuantifierList;
      case 'literal':
        if (!this._n3Mode)
          return this._error('Unexpected literal', token);

        if (token.prefix.length === 0) {
          this._literalValue = token.value;
          return this._completeSubjectLiteral;
        }
        else
          this._subject = this._literal(token.value, this._namedNode(token.prefix));

        break;
      case '<<':
        if (!this._supportsRDFStar)
          return this._error('Unexpected RDF* syntax', token);
        this._saveContext('<<', this._graph, null, null, null);
        this._graph = null;
        return this._readSubject;
      default:
        // Read the subject entity
        if ((this._subject = this._readEntity(token)) === undefined)
          return;
        // In N3 mode, the subject might be a path
        if (this._n3Mode)
          return this._getPathReader(this._readPredicateOrNamedGraph);
      }

      // The next token must be a predicate,
      // or, if the subject was actually a graph IRI, a named graph
      return this._readPredicateOrNamedGraph;
    }

    // ### `_readPredicate` reads a quad's predicate
    _readPredicate(token) {
      const type = token.type;
      switch (type) {
      case 'inverse':
        this._inversePredicate = true;
      case 'abbreviation':
        this._predicate = this.ABBREVIATIONS[token.value];
        break;
      case '.':
      case ']':
      case '}':
        // Expected predicate didn't come, must have been trailing semicolon
        if (this._predicate === null)
          return this._error(`Unexpected ${type}`, token);
        this._subject = null;
        return type === ']' ? this._readBlankNodeTail(token) : this._readPunctuation(token);
      case ';':
        // Additional semicolons can be safely ignored
        return this._predicate !== null ? this._readPredicate :
               this._error('Expected predicate but got ;', token);
      case 'blank':
        if (!this._n3Mode)
          return this._error('Disallowed blank node as predicate', token);
      default:
        if ((this._predicate = this._readEntity(token)) === undefined)
          return;
      }
      // The next token must be an object
      return this._readObject;
    }

    // ### `_readObject` reads a quad's object
    _readObject(token) {
      switch (token.type) {
      case 'literal':
        // Regular literal, can still get a datatype or language
        if (token.prefix.length === 0) {
          this._literalValue = token.value;
          return this._readDataTypeOrLang;
        }
        // Pre-datatyped string literal (prefix stores the datatype)
        else
          this._object = this._literal(token.value, this._namedNode(token.prefix));
        break;
      case '[':
        // Start a new quad with a new blank node as subject
        this._saveContext('blank', this._graph, this._subject, this._predicate,
                          this._subject = this._blankNode());
        return this._readBlankNodeHead;
      case '(':
        // Start a new list
        this._saveContext('list', this._graph, this._subject, this._predicate, this.RDF_NIL);
        this._subject = null;
        return this._readListItem;
      case '{':
        // Start a new formula
        if (!this._n3Mode)
          return this._error('Unexpected graph', token);
        this._saveContext('formula', this._graph, this._subject, this._predicate,
                          this._graph = this._blankNode());
        return this._readSubject;
      case '<<':
        if (!this._supportsRDFStar)
          return this._error('Unexpected RDF* syntax', token);
        this._saveContext('<<', this._graph, this._subject, this._predicate, null);
        this._graph = null;
        return this._readSubject;
      default:
        // Read the object entity
        if ((this._object = this._readEntity(token)) === undefined)
          return;
        // In N3 mode, the object might be a path
        if (this._n3Mode)
          return this._getPathReader(this._getContextEndReader());
      }
      return this._getContextEndReader();
    }

    // ### `_readPredicateOrNamedGraph` reads a quad's predicate, or a named graph
    _readPredicateOrNamedGraph(token) {
      return token.type === '{' ? this._readGraph(token) : this._readPredicate(token);
    }

    // ### `_readGraph` reads a graph
    _readGraph(token) {
      if (token.type !== '{')
        return this._error(`Expected graph but got ${token.type}`, token);
      // The "subject" we read is actually the GRAPH's label
      this._graph = this._subject, this._subject = null;
      return this._readSubject;
    }

    // ### `_readBlankNodeHead` reads the head of a blank node
    _readBlankNodeHead(token) {
      if (token.type === ']') {
        this._subject = null;
        return this._readBlankNodeTail(token);
      }
      else {
        this._predicate = null;
        return this._readPredicate(token);
      }
    }

    // ### `_readBlankNodeTail` reads the end of a blank node
    _readBlankNodeTail(token) {
      if (token.type !== ']')
        return this._readBlankNodePunctuation(token);

      // Store blank node quad
      if (this._subject !== null)
        this._emit(this._subject, this._predicate, this._object, this._graph);

      // Restore the parent context containing this blank node
      const empty = this._predicate === null;
      this._restoreContext();
      // If the blank node was the subject, continue reading the predicate
      if (this._object === null)
        // If the blank node was empty, it could be a named graph label
        return empty ? this._readPredicateOrNamedGraph : this._readPredicateAfterBlank;
      // If the blank node was the object, restore previous context and read punctuation
      else
        return this._getContextEndReader();
    }

    // ### `_readPredicateAfterBlank` reads a predicate after an anonymous blank node
    _readPredicateAfterBlank(token) {
      switch (token.type) {
      case '.':
      case '}':
        // No predicate is coming if the triple is terminated here
        this._subject = null;
        return this._readPunctuation(token);
      default:
        return this._readPredicate(token);
      }
    }

    // ### `_readListItem` reads items from a list
    _readListItem(token) {
      let item = null,                      // The item of the list
          list = null,                      // The list itself
          next = this._readListItem;        // The next function to execute
      const previousList = this._subject,   // The previous list that contains this list
          stack = this._contextStack,       // The stack of parent contexts
          parent = stack[stack.length - 1]; // The parent containing the current list

      switch (token.type) {
      case '[':
        // Stack the current list quad and start a new quad with a blank node as subject
        this._saveContext('blank', this._graph,
                          list = this._blankNode(), this.RDF_FIRST,
                          this._subject = item = this._blankNode());
        next = this._readBlankNodeHead;
        break;
      case '(':
        // Stack the current list quad and start a new list
        this._saveContext('list', this._graph,
                          list = this._blankNode(), this.RDF_FIRST, this.RDF_NIL);
        this._subject = null;
        break;
      case ')':
        // Closing the list; restore the parent context
        this._restoreContext();
        // If this list is contained within a parent list, return the membership quad here.
        // This will be `<parent list element> rdf:first <this list>.`.
        if (stack.length !== 0 && stack[stack.length - 1].type === 'list')
          this._emit(this._subject, this._predicate, this._object, this._graph);
        // Was this list the parent's subject?
        if (this._predicate === null) {
          // The next token is the predicate
          next = this._readPredicate;
          // No list tail if this was an empty list
          if (this._subject === this.RDF_NIL)
            return next;
        }
        // The list was in the parent context's object
        else {
          next = this._getContextEndReader();
          // No list tail if this was an empty list
          if (this._object === this.RDF_NIL)
            return next;
        }
        // Close the list by making the head nil
        list = this.RDF_NIL;
        break;
      case 'literal':
        // Regular literal, can still get a datatype or language
        if (token.prefix.length === 0) {
          this._literalValue = token.value;
          next = this._readListItemDataTypeOrLang;
        }
        // Pre-datatyped string literal (prefix stores the datatype)
        else {
          item = this._literal(token.value, this._namedNode(token.prefix));
          next = this._getContextEndReader();
        }
        break;
      case '{':
        // Start a new formula
        if (!this._n3Mode)
          return this._error('Unexpected graph', token);
        this._saveContext('formula', this._graph, this._subject, this._predicate,
                          this._graph = this._blankNode());
        return this._readSubject;
      default:
        if ((item = this._readEntity(token)) === undefined)
          return;
      }

       // Create a new blank node if no item head was assigned yet
      if (list === null)
        this._subject = list = this._blankNode();

      // Is this the first element of the list?
      if (previousList === null) {
        // This list is either the subject or the object of its parent
        if (parent.predicate === null)
          parent.subject = list;
        else
          parent.object = list;
      }
      else {
        // Continue the previous list with the current list
        this._emit(previousList, this.RDF_REST, list, this._graph);
      }
      // If an item was read, add it to the list
      if (item !== null) {
        // In N3 mode, the item might be a path
        if (this._n3Mode && (token.type === 'IRI' || token.type === 'prefixed')) {
          // Create a new context to add the item's path
          this._saveContext('item', this._graph, list, this.RDF_FIRST, item);
          this._subject = item, this._predicate = null;
          // _readPath will restore the context and output the item
          return this._getPathReader(this._readListItem);
        }
        // Output the item
        this._emit(list, this.RDF_FIRST, item, this._graph);
      }
      return next;
    }

    // ### `_readDataTypeOrLang` reads an _optional_ datatype or language
    _readDataTypeOrLang(token) {
      return this._completeObjectLiteral(token, false);
    }


    // ### `_readListItemDataTypeOrLang` reads an _optional_ datatype or language in a list
    _readListItemDataTypeOrLang(token) {
      return this._completeObjectLiteral(token, true);
    }

    // ### `_completeLiteral` completes a literal with an optional datatype or language
    _completeLiteral(token) {
      // Create a simple string literal by default
      let literal = this._literal(this._literalValue);

      switch (token.type) {
      // Create a datatyped literal
      case 'type':
      case 'typeIRI':
        const datatype = this._readEntity(token);
        if (datatype === undefined) return; // No datatype means an error occurred
        literal = this._literal(this._literalValue, datatype);
        token = null;
        break;
      // Create a language-tagged string
      case 'langcode':
        literal = this._literal(this._literalValue, token.value);
        token = null;
        break;
      }

      return { token, literal };
    }

    // Completes a literal in subject position
    _completeSubjectLiteral(token) {
      this._subject = this._completeLiteral(token).literal;
      return this._readPredicateOrNamedGraph;
    }

    // Completes a literal in object position
    _completeObjectLiteral(token, listItem) {
      const completed = this._completeLiteral(token);
      if (!completed)
        return;
      this._object = completed.literal;

      // If this literal was part of a list, write the item
      // (we could also check the context stack, but passing in a flag is faster)
      if (listItem)
        this._emit(this._subject, this.RDF_FIRST, this._object, this._graph);
      // If the token was consumed, continue with the rest of the input
      if (completed.token === null)
        return this._getContextEndReader();
      // Otherwise, consume the token now
      else {
        this._readCallback = this._getContextEndReader();
        return this._readCallback(completed.token);
      }
    }

    // ### `_readFormulaTail` reads the end of a formula
    _readFormulaTail(token) {
      if (token.type !== '}')
        return this._readPunctuation(token);

      // Store the last quad of the formula
      if (this._subject !== null)
        this._emit(this._subject, this._predicate, this._object, this._graph);

      // Restore the parent context containing this formula
      this._restoreContext();
      // If the formula was the subject, continue reading the predicate.
      // If the formula was the object, read punctuation.
      return this._object === null ? this._readPredicate : this._getContextEndReader();
    }

    // ### `_readPunctuation` reads punctuation between quads or quad parts
    _readPunctuation(token) {
      let next, graph = this._graph;
      const subject = this._subject, inversePredicate = this._inversePredicate;
      switch (token.type) {
      // A closing brace ends a graph
      case '}':
        if (this._graph === null)
          return this._error('Unexpected graph closing', token);
        if (this._n3Mode)
          return this._readFormulaTail(token);
        this._graph = null;
      // A dot just ends the statement, without sharing anything with the next
      case '.':
        this._subject = null;
        next = this._contextStack.length ? this._readSubject : this._readInTopContext;
        if (inversePredicate) this._inversePredicate = false;
        break;
      // Semicolon means the subject is shared; predicate and object are different
      case ';':
        next = this._readPredicate;
        break;
      // Comma means both the subject and predicate are shared; the object is different
      case ',':
        next = this._readObject;
        break;
      default:
        // An entity means this is a quad (only allowed if not already inside a graph)
        if (this._supportsQuads && this._graph === null && (graph = this._readEntity(token)) !== undefined) {
          next = this._readQuadPunctuation;
          break;
        }
        return this._error(`Expected punctuation to follow "${this._object.id}"`, token);
      }
      // A quad has been completed now, so return it
      if (subject !== null) {
        const predicate = this._predicate, object = this._object;
        if (!inversePredicate)
          this._emit(subject, predicate, object,  graph);
        else
          this._emit(object,  predicate, subject, graph);
      }
      return next;
    }

      // ### `_readBlankNodePunctuation` reads punctuation in a blank node
    _readBlankNodePunctuation(token) {
      let next;
      switch (token.type) {
      // Semicolon means the subject is shared; predicate and object are different
      case ';':
        next = this._readPredicate;
        break;
      // Comma means both the subject and predicate are shared; the object is different
      case ',':
        next = this._readObject;
        break;
      default:
        return this._error(`Expected punctuation to follow "${this._object.id}"`, token);
      }
      // A quad has been completed now, so return it
      this._emit(this._subject, this._predicate, this._object, this._graph);
      return next;
    }

    // ### `_readQuadPunctuation` reads punctuation after a quad
    _readQuadPunctuation(token) {
      if (token.type !== '.')
        return this._error('Expected dot to follow quad', token);
      return this._readInTopContext;
    }

    // ### `_readPrefix` reads the prefix of a prefix declaration
    _readPrefix(token) {
      if (token.type !== 'prefix')
        return this._error('Expected prefix to follow @prefix', token);
      this._prefix = token.value;
      return this._readPrefixIRI;
    }

    // ### `_readPrefixIRI` reads the IRI of a prefix declaration
    _readPrefixIRI(token) {
      if (token.type !== 'IRI')
        return this._error(`Expected IRI to follow prefix "${this._prefix}:"`, token);
      const prefixNode = this._readEntity(token);
      this._prefixes[this._prefix] = prefixNode.value;
      this._prefixCallback(this._prefix, prefixNode);
      return this._readDeclarationPunctuation;
    }

    // ### `_readBaseIRI` reads the IRI of a base declaration
    _readBaseIRI(token) {
      const iri = token.type === 'IRI' && this._resolveIRI(token.value);
      if (!iri)
        return this._error('Expected valid IRI to follow base declaration', token);
      this._setBase(iri);
      return this._readDeclarationPunctuation;
    }

    // ### `_readNamedGraphLabel` reads the label of a named graph
    _readNamedGraphLabel(token) {
      switch (token.type) {
      case 'IRI':
      case 'blank':
      case 'prefixed':
        return this._readSubject(token), this._readGraph;
      case '[':
        return this._readNamedGraphBlankLabel;
      default:
        return this._error('Invalid graph label', token);
      }
    }

    // ### `_readNamedGraphLabel` reads a blank node label of a named graph
    _readNamedGraphBlankLabel(token) {
      if (token.type !== ']')
        return this._error('Invalid graph label', token);
      this._subject = this._blankNode();
      return this._readGraph;
    }

    // ### `_readDeclarationPunctuation` reads the punctuation of a declaration
    _readDeclarationPunctuation(token) {
      // SPARQL-style declarations don't have punctuation
      if (this._sparqlStyle) {
        this._sparqlStyle = false;
        return this._readInTopContext(token);
      }

      if (token.type !== '.')
        return this._error('Expected declaration to end with a dot', token);
      return this._readInTopContext;
    }

    // Reads a list of quantified symbols from a @forSome or @forAll statement
    _readQuantifierList(token) {
      let entity;
      switch (token.type) {
      case 'IRI':
      case 'prefixed':
        if ((entity = this._readEntity(token, true)) !== undefined)
          break;
      default:
        return this._error(`Unexpected ${token.type}`, token);
      }
      // Without explicit quantifiers, map entities to a quantified entity
      if (!this._explicitQuantifiers)
        this._quantified[entity.id] = this._quantifier(this._blankNode().value);
      // With explicit quantifiers, output the reified quantifier
      else {
        // If this is the first item, start a new quantifier list
        if (this._subject === null)
          this._emit(this._graph || this.DEFAULTGRAPH, this._predicate,
                     this._subject = this._blankNode(), this.QUANTIFIERS_GRAPH);
        // Otherwise, continue the previous list
        else
          this._emit(this._subject, this.RDF_REST,
                     this._subject = this._blankNode(), this.QUANTIFIERS_GRAPH);
        // Output the list item
        this._emit(this._subject, this.RDF_FIRST, entity, this.QUANTIFIERS_GRAPH);
      }
      return this._readQuantifierPunctuation;
    }

    // Reads punctuation from a @forSome or @forAll statement
    _readQuantifierPunctuation(token) {
      // Read more quantifiers
      if (token.type === ',')
        return this._readQuantifierList;
      // End of the quantifier list
      else {
        // With explicit quantifiers, close the quantifier list
        if (this._explicitQuantifiers) {
          this._emit(this._subject, this.RDF_REST, this.RDF_NIL, this.QUANTIFIERS_GRAPH);
          this._subject = null;
        }
        // Read a dot
        this._readCallback = this._getContextEndReader();
        return this._readCallback(token);
      }
    }

    // ### `_getPathReader` reads a potential path and then resumes with the given function
    _getPathReader(afterPath) {
      this._afterPath = afterPath;
      return this._readPath;
    }

    // ### `_readPath` reads a potential path
    _readPath(token) {
      switch (token.type) {
      // Forward path
      case '!': return this._readForwardPath;
      // Backward path
      case '^': return this._readBackwardPath;
      // Not a path; resume reading where we left off
      default:
        const stack = this._contextStack, parent = stack.length && stack[stack.length - 1];
        // If we were reading a list item, we still need to output it
        if (parent && parent.type === 'item') {
          // The list item is the remaining subejct after reading the path
          const item = this._subject;
          // Switch back to the context of the list
          this._restoreContext();
          // Output the list item
          this._emit(this._subject, this.RDF_FIRST, item, this._graph);
        }
        return this._afterPath(token);
      }
    }

    // ### `_readForwardPath` reads a '!' path
    _readForwardPath(token) {
      let subject, predicate;
      const object = this._blankNode();
      // The next token is the predicate
      if ((predicate = this._readEntity(token)) === undefined)
        return;
      // If we were reading a subject, replace the subject by the path's object
      if (this._predicate === null)
        subject = this._subject, this._subject = object;
      // If we were reading an object, replace the subject by the path's object
      else
        subject = this._object,  this._object  = object;
      // Emit the path's current quad and read its next section
      this._emit(subject, predicate, object, this._graph);
      return this._readPath;
    }

    // ### `_readBackwardPath` reads a '^' path
    _readBackwardPath(token) {
      const subject = this._blankNode();
      let predicate, object;
      // The next token is the predicate
      if ((predicate = this._readEntity(token)) === undefined)
        return;
      // If we were reading a subject, replace the subject by the path's subject
      if (this._predicate === null)
        object = this._subject, this._subject = subject;
      // If we were reading an object, replace the subject by the path's subject
      else
        object = this._object,  this._object  = subject;
      // Emit the path's current quad and read its next section
      this._emit(subject, predicate, object, this._graph);
      return this._readPath;
    }

    // ### `_readRDFStarTailOrGraph` reads the graph of a nested RDF* quad or the end of a nested RDF* triple
    _readRDFStarTailOrGraph(token) {
      if (token.type !== '>>') {
        // An entity means this is a quad (only allowed if not already inside a graph)
        if (this._supportsQuads && this._graph === null && (this._graph = this._readEntity(token)) !== undefined)
          return this._readRDFStarTail;
        return this._error(`Expected >> to follow "${this._object.id}"`, token);
      }
      return this._readRDFStarTail(token);
    }

    // ### `_readRDFStarTail` reads the end of a nested RDF* triple
    _readRDFStarTail(token) {
      if (token.type !== '>>')
        return this._error(`Expected >> but got ${token.type}`, token);
      // Read the quad and restore the previous context
      const quad = this._quad(this._subject, this._predicate, this._object,
        this._graph || this.DEFAULTGRAPH);
      this._restoreContext();
      // If the triple was the subject, continue by reading the predicate.
      if (this._subject === null) {
        this._subject = quad;
        return this._readPredicate;
      }
      // If the triple was the object, read context end.
      else {
        this._object = quad;
        return this._getContextEndReader();
      }
    }

    // ### `_getContextEndReader` gets the next reader function at the end of a context
    _getContextEndReader() {
      const contextStack = this._contextStack;
      if (!contextStack.length)
        return this._readPunctuation;

      switch (contextStack[contextStack.length - 1].type) {
      case 'blank':
        return this._readBlankNodeTail;
      case 'list':
        return this._readListItem;
      case 'formula':
        return this._readFormulaTail;
      case '<<':
        return this._readRDFStarTailOrGraph;
      }
    }

    // ### `_emit` sends a quad through the callback
    _emit(subject, predicate, object, graph) {
      this._callback(null, this._quad(subject, predicate, object, graph || this.DEFAULTGRAPH));
    }

    // ### `_error` emits an error message through the callback
    _error(message, token) {
      const err = new Error(`${message} on line ${token.line}.`);
      err.context = {
        token: token,
        line: token.line,
        previousToken: this._lexer.previousToken,
      };
      this._callback(err);
      this._callback = noop$2;
    }

    // ### `_resolveIRI` resolves an IRI against the base path
    _resolveIRI(iri) {
      return /^[a-z][a-z0-9+.-]*:/i.test(iri) ? iri : this._resolveRelativeIRI(iri);
    }

    // ### `_resolveRelativeIRI` resolves an IRI against the base path,
    // assuming that a base path has been set and that the IRI is indeed relative
    _resolveRelativeIRI(iri) {
      // An empty relative IRI indicates the base IRI
      if (!iri.length)
        return this._base;
      // Decide resolving strategy based in the first character
      switch (iri[0]) {
      // Resolve relative fragment IRIs against the base IRI
      case '#': return this._base + iri;
      // Resolve relative query string IRIs by replacing the query string
      case '?': return this._base.replace(/(?:\?.*)?$/, iri);
      // Resolve root-relative IRIs at the root of the base IRI
      case '/':
        // Resolve scheme-relative IRIs to the scheme
        return (iri[1] === '/' ? this._baseScheme : this._baseRoot) + this._removeDotSegments(iri);
      // Resolve all other IRIs at the base IRI's path
      default:
        // Relative IRIs cannot contain a colon in the first path segment
        return (/^[^/:]*:/.test(iri)) ? null : this._removeDotSegments(this._basePath + iri);
      }
    }

    // ### `_removeDotSegments` resolves './' and '../' path segments in an IRI as per RFC3986
    _removeDotSegments(iri) {
      // Don't modify the IRI if it does not contain any dot segments
      if (!/(^|\/)\.\.?($|[/#?])/.test(iri))
        return iri;

      // Start with an imaginary slash before the IRI in order to resolve trailing './' and '../'
      const length = iri.length;
      let result = '', i = -1, pathStart = -1, segmentStart = 0, next = '/';

      while (i < length) {
        switch (next) {
        // The path starts with the first slash after the authority
        case ':':
          if (pathStart < 0) {
            // Skip two slashes before the authority
            if (iri[++i] === '/' && iri[++i] === '/')
              // Skip to slash after the authority
              while ((pathStart = i + 1) < length && iri[pathStart] !== '/')
                i = pathStart;
          }
          break;
        // Don't modify a query string or fragment
        case '?':
        case '#':
          i = length;
          break;
        // Handle '/.' or '/..' path segments
        case '/':
          if (iri[i + 1] === '.') {
            next = iri[++i + 1];
            switch (next) {
            // Remove a '/.' segment
            case '/':
              result += iri.substring(segmentStart, i - 1);
              segmentStart = i + 1;
              break;
            // Remove a trailing '/.' segment
            case undefined:
            case '?':
            case '#':
              return result + iri.substring(segmentStart, i) + iri.substr(i + 1);
            // Remove a '/..' segment
            case '.':
              next = iri[++i + 1];
              if (next === undefined || next === '/' || next === '?' || next === '#') {
                result += iri.substring(segmentStart, i - 2);
                // Try to remove the parent path from result
                if ((segmentStart = result.lastIndexOf('/')) >= pathStart)
                  result = result.substr(0, segmentStart);
                // Remove a trailing '/..' segment
                if (next !== '/')
                  return `${result}/${iri.substr(i + 1)}`;
                segmentStart = i + 1;
              }
            }
          }
        }
        next = iri[++i];
      }
      return result + iri.substring(segmentStart);
    }

    // ## Public methods

    // ### `parse` parses the N3 input and emits each parsed quad through the callback
    parse(input, quadCallback, prefixCallback) {
      // The read callback is the next function to be executed when a token arrives.
      // We start reading in the top context.
      this._readCallback = this._readInTopContext;
      this._sparqlStyle = false;
      this._prefixes = Object.create(null);
      this._prefixes._ = this._blankNodePrefix ? this._blankNodePrefix.substr(2)
                                               : `b${blankNodePrefix++}_`;
      this._prefixCallback = prefixCallback || noop$2;
      this._inversePredicate = false;
      this._quantified = Object.create(null);

      // Parse synchronously if no quad callback is given
      if (!quadCallback) {
        const quads = [];
        let error;
        this._callback = (e, t) => { e ? (error = e) : t && quads.push(t); };
        this._lexer.tokenize(input).every(token => {
          return this._readCallback = this._readCallback(token);
        });
        if (error) throw error;
        return quads;
      }

      // Parse asynchronously otherwise, executing the read callback when a token arrives
      this._callback = quadCallback;
      this._lexer.tokenize(input, (error, token) => {
        if (error !== null)
          this._callback(error), this._callback = noop$2;
        else if (this._readCallback)
          this._readCallback = this._readCallback(token);
      });
    }
  }

  // The empty function
  function noop$2() {}

  // Initializes the parser with the given data factory
  function initDataFactory(parser, factory) {
    // Set factory methods
    const namedNode = factory.namedNode;
    parser._namedNode   = namedNode;
    parser._blankNode   = factory.blankNode;
    parser._literal     = factory.literal;
    parser._variable    = factory.variable;
    parser._quad        = factory.quad;
    parser.DEFAULTGRAPH = factory.defaultGraph();

    // Set common named nodes
    parser.RDF_FIRST  = namedNode(namespaces.rdf.first);
    parser.RDF_REST   = namedNode(namespaces.rdf.rest);
    parser.RDF_NIL    = namedNode(namespaces.rdf.nil);
    parser.N3_FORALL  = namedNode(namespaces.r.forAll);
    parser.N3_FORSOME = namedNode(namespaces.r.forSome);
    parser.ABBREVIATIONS = {
      'a': namedNode(namespaces.rdf.type),
      '=': namedNode(namespaces.owl.sameAs),
      '>': namedNode(namespaces.log.implies),
    };
    parser.QUANTIFIERS_GRAPH = namedNode('urn:n3:quantifiers');
  }
  initDataFactory(N3Parser.prototype, N3DataFactory);

  // **N3Writer** writes N3 documents.

  const DEFAULTGRAPH = N3DataFactory.defaultGraph();

  const { rdf, xsd } = namespaces;

  // Characters in literals that require escaping
  const escape    = /["\\\t\n\r\b\f\u0000-\u0019\ud800-\udbff]/,
      escapeAll = /["\\\t\n\r\b\f\u0000-\u0019]|[\ud800-\udbff][\udc00-\udfff]/g,
      escapedCharacters = {
        '\\': '\\\\', '"': '\\"', '\t': '\\t',
        '\n': '\\n', '\r': '\\r', '\b': '\\b', '\f': '\\f',
      };

  // ## Placeholder class to represent already pretty-printed terms
  class SerializedTerm extends Term {
    // Pretty-printed nodes are not equal to any other node
    // (e.g., [] does not equal [])
    equals() {
      return false;
    }
  }

  // ## Constructor
  class N3Writer {
    constructor(outputStream, options) {
      // ### `_prefixRegex` matches a prefixed name or IRI that begins with one of the added prefixes
      this._prefixRegex = /$0^/;

      // Shift arguments if the first argument is not a stream
      if (outputStream && typeof outputStream.write !== 'function')
        options = outputStream, outputStream = null;
      options = options || {};
      this._lists = options.lists;

      // If no output stream given, send the output as string through the end callback
      if (!outputStream) {
        let output = '';
        this._outputStream = {
          write(chunk, encoding, done) { output += chunk; done && done(); },
          end: done => { done && done(null, output); },
        };
        this._endStream = true;
      }
      else {
        this._outputStream = outputStream;
        this._endStream = options.end === undefined ? true : !!options.end;
      }

      // Initialize writer, depending on the format
      this._subject = null;
      if (!(/triple|quad/i).test(options.format)) {
        this._lineMode = false;
        this._graph = DEFAULTGRAPH;
        this._baseIRI = options.baseIRI;
        this._prefixIRIs = Object.create(null);
        options.prefixes && this.addPrefixes(options.prefixes);
      }
      else {
        this._lineMode = true;
        this._writeQuad = this._writeQuadLine;
      }
    }

    // ## Private methods

    // ### Whether the current graph is the default graph
    get _inDefaultGraph() {
      return DEFAULTGRAPH.equals(this._graph);
    }

    // ### `_write` writes the argument to the output stream
    _write(string, callback) {
      this._outputStream.write(string, 'utf8', callback);
    }

    // ### `_writeQuad` writes the quad to the output stream
    _writeQuad(subject, predicate, object, graph, done) {
      try {
        // Write the graph's label if it has changed
        if (!graph.equals(this._graph)) {
          // Close the previous graph and start the new one
          this._write((this._subject === null ? '' : (this._inDefaultGraph ? '.\n' : '\n}\n')) +
                      (DEFAULTGRAPH.equals(graph) ? '' : `${this._encodeIriOrBlank(graph)} {\n`));
          this._graph = graph;
          this._subject = null;
        }
        // Don't repeat the subject if it's the same
        if (subject.equals(this._subject)) {
          // Don't repeat the predicate if it's the same
          if (predicate.equals(this._predicate))
            this._write(`, ${this._encodeObject(object)}`, done);
          // Same subject, different predicate
          else
            this._write(`;\n    ${
                      this._encodePredicate(this._predicate = predicate)} ${
                      this._encodeObject(object)}`, done);
        }
        // Different subject; write the whole quad
        else
          this._write(`${(this._subject === null ? '' : '.\n') +
                    this._encodeSubject(this._subject = subject)} ${
                    this._encodePredicate(this._predicate = predicate)} ${
                    this._encodeObject(object)}`, done);
      }
      catch (error) { done && done(error); }
    }

    // ### `_writeQuadLine` writes the quad to the output stream as a single line
    _writeQuadLine(subject, predicate, object, graph, done) {
      // Write the quad without prefixes
      delete this._prefixMatch;
      this._write(this.quadToString(subject, predicate, object, graph), done);
    }

    // ### `quadToString` serializes a quad as a string
    quadToString(subject, predicate, object, graph) {
      return  `${this._encodeSubject(subject)} ${
            this._encodeIriOrBlank(predicate)} ${
            this._encodeObject(object)
            }${graph && graph.value ? ` ${this._encodeIriOrBlank(graph)} .\n` : ' .\n'}`;
    }

    // ### `quadsToString` serializes an array of quads as a string
    quadsToString(quads) {
      return quads.map(t => {
        return this.quadToString(t.subject, t.predicate, t.object, t.graph);
      }).join('');
    }

    // ### `_encodeSubject` represents a subject
    _encodeSubject(entity) {
      return entity.termType === 'Quad' ?
        this._encodeQuad(entity) : this._encodeIriOrBlank(entity);
    }

    // ### `_encodeIriOrBlank` represents an IRI or blank node
    _encodeIriOrBlank(entity) {
      // A blank node or list is represented as-is
      if (entity.termType !== 'NamedNode') {
        // If it is a list head, pretty-print it
        if (this._lists && (entity.value in this._lists))
          entity = this.list(this._lists[entity.value]);
        return 'id' in entity ? entity.id : `_:${entity.value}`;
      }
      let iri = entity.value;
      // Use relative IRIs if requested and possible
      if (this._baseIRI && iri.startsWith(this._baseIRI))
        iri = iri.substr(this._baseIRI.length);
      // Escape special characters
      if (escape.test(iri))
        iri = iri.replace(escapeAll, characterReplacer);
      // Try to represent the IRI as prefixed name
      const prefixMatch = this._prefixRegex.exec(iri);
      return !prefixMatch ? `<${iri}>` :
             (!prefixMatch[1] ? iri : this._prefixIRIs[prefixMatch[1]] + prefixMatch[2]);
    }

    // ### `_encodeLiteral` represents a literal
    _encodeLiteral(literal) {
      // Escape special characters
      let value = literal.value;
      if (escape.test(value))
        value = value.replace(escapeAll, characterReplacer);

      // Write a language-tagged literal
      if (literal.language)
        return `"${value}"@${literal.language}`;

      // Write dedicated literals per data type
      if (this._lineMode) {
        // Only abbreviate strings in N-Triples or N-Quads
        if (literal.datatype.value === xsd.string)
          return `"${value}"`;
      }
      else {
        // Use common datatype abbreviations in Turtle or TriG
        switch (literal.datatype.value) {
        case xsd.string:
          return `"${value}"`;
        case xsd.boolean:
          if (value === 'true' || value === 'false')
            return value;
          break;
        case xsd.integer:
          if (/^[+-]?\d+$/.test(value))
            return value;
          break;
        case xsd.decimal:
          if (/^[+-]?\d*\.\d+$/.test(value))
            return value;
          break;
        case xsd.double:
          if (/^[+-]?(?:\d+\.\d*|\.?\d+)[eE][+-]?\d+$/.test(value))
            return value;
          break;
        }
      }

      // Write a regular datatyped literal
      return `"${value}"^^${this._encodeIriOrBlank(literal.datatype)}`;
    }

    // ### `_encodePredicate` represents a predicate
    _encodePredicate(predicate) {
      return predicate.value === rdf.type ? 'a' : this._encodeIriOrBlank(predicate);
    }

    // ### `_encodeObject` represents an object
    _encodeObject(object) {
      switch (object.termType) {
      case 'Quad':
        return this._encodeQuad(object);
      case 'Literal':
        return this._encodeLiteral(object);
      default:
        return this._encodeIriOrBlank(object);
      }
    }

    // ### `_encodeQuad` encodes an RDF* quad
    _encodeQuad({ subject, predicate, object, graph }) {
      return `<<${
      this._encodeSubject(subject)} ${
      this._encodePredicate(predicate)} ${
      this._encodeObject(object)}${
      isDefaultGraph(graph) ? '' : ` ${this._encodeIriOrBlank(graph)}`}>>`;
    }

    // ### `_blockedWrite` replaces `_write` after the writer has been closed
    _blockedWrite() {
      throw new Error('Cannot write because the writer has been closed.');
    }

    // ### `addQuad` adds the quad to the output stream
    addQuad(subject, predicate, object, graph, done) {
      // The quad was given as an object, so shift parameters
      if (object === undefined)
        this._writeQuad(subject.subject, subject.predicate, subject.object, subject.graph, predicate);
      // The optional `graph` parameter was not provided
      else if (typeof graph === 'function')
        this._writeQuad(subject, predicate, object, DEFAULTGRAPH, graph);
      // The `graph` parameter was provided
      else
        this._writeQuad(subject, predicate, object, graph || DEFAULTGRAPH, done);
    }

    // ### `addQuads` adds the quads to the output stream
    addQuads(quads) {
      for (let i = 0; i < quads.length; i++)
        this.addQuad(quads[i]);
    }

    // ### `addPrefix` adds the prefix to the output stream
    addPrefix(prefix, iri, done) {
      const prefixes = {};
      prefixes[prefix] = iri;
      this.addPrefixes(prefixes, done);
    }

    // ### `addPrefixes` adds the prefixes to the output stream
    addPrefixes(prefixes, done) {
      // Ignore prefixes if not supported by the serialization
      if (!this._prefixIRIs)
        return done && done();

      // Write all new prefixes
      let hasPrefixes = false;
      for (let prefix in prefixes) {
        let iri = prefixes[prefix];
        if (typeof iri !== 'string')
          iri = iri.value;
        hasPrefixes = true;
        // Finish a possible pending quad
        if (this._subject !== null) {
          this._write(this._inDefaultGraph ? '.\n' : '\n}\n');
          this._subject = null, this._graph = '';
        }
        // Store and write the prefix
        this._prefixIRIs[iri] = (prefix += ':');
        this._write(`@prefix ${prefix} <${iri}>.\n`);
      }
      // Recreate the prefix matcher
      if (hasPrefixes) {
        let IRIlist = '', prefixList = '';
        for (const prefixIRI in this._prefixIRIs) {
          IRIlist += IRIlist ? `|${prefixIRI}` : prefixIRI;
          prefixList += (prefixList ? '|' : '') + this._prefixIRIs[prefixIRI];
        }
        IRIlist = IRIlist.replace(/[\]\/\(\)\*\+\?\.\\\$]/g, '\\$&');
        this._prefixRegex = new RegExp(`^(?:${prefixList})[^\/]*$|` +
                                       `^(${IRIlist})([a-zA-Z][\\-_a-zA-Z0-9]*)$`);
      }
      // End a prefix block with a newline
      this._write(hasPrefixes ? '\n' : '', done);
    }

    // ### `blank` creates a blank node with the given content
    blank(predicate, object) {
      let children = predicate, child, length;
      // Empty blank node
      if (predicate === undefined)
        children = [];
      // Blank node passed as blank(Term("predicate"), Term("object"))
      else if (predicate.termType)
        children = [{ predicate: predicate, object: object }];
      // Blank node passed as blank({ predicate: predicate, object: object })
      else if (!('length' in predicate))
        children = [predicate];

      switch (length = children.length) {
      // Generate an empty blank node
      case 0:
        return new SerializedTerm('[]');
      // Generate a non-nested one-triple blank node
      case 1:
        child = children[0];
        if (!(child.object instanceof SerializedTerm))
          return new SerializedTerm(`[ ${this._encodePredicate(child.predicate)} ${
                                  this._encodeObject(child.object)} ]`);
      // Generate a multi-triple or nested blank node
      default:
        let contents = '[';
        // Write all triples in order
        for (let i = 0; i < length; i++) {
          child = children[i];
          // Write only the object is the predicate is the same as the previous
          if (child.predicate.equals(predicate))
            contents += `, ${this._encodeObject(child.object)}`;
          // Otherwise, write the predicate and the object
          else {
            contents += `${(i ? ';\n  ' : '\n  ') +
                      this._encodePredicate(child.predicate)} ${
                      this._encodeObject(child.object)}`;
            predicate = child.predicate;
          }
        }
        return new SerializedTerm(`${contents}\n]`);
      }
    }

    // ### `list` creates a list node with the given content
    list(elements) {
      const length = elements && elements.length || 0, contents = new Array(length);
      for (let i = 0; i < length; i++)
        contents[i] = this._encodeObject(elements[i]);
      return new SerializedTerm(`(${contents.join(' ')})`);
    }

    // ### `end` signals the end of the output stream
    end(done) {
      // Finish a possible pending quad
      if (this._subject !== null) {
        this._write(this._inDefaultGraph ? '.\n' : '\n}\n');
        this._subject = null;
      }
      // Disallow further writing
      this._write = this._blockedWrite;

      // Try to end the underlying stream, ensuring done is called exactly one time
      let singleDone = done && ((error, result) => { singleDone = null, done(error, result); });
      if (this._endStream) {
        try { return this._outputStream.end(singleDone); }
        catch (error) { /* error closing stream */ }
      }
      singleDone && singleDone();
    }
  }

  // Replaces a character by its escaped version
  function characterReplacer(character) {
    // Replace a single character by its escaped version
    let result = escapedCharacters[character];
    if (result === undefined) {
      // Replace a single character with its 4-bit unicode escape sequence
      if (character.length === 1) {
        result = character.charCodeAt(0).toString(16);
        result = '\\u0000'.substr(0, 6 - result.length) + result;
      }
      // Replace a surrogate pair with its 8-bit unicode escape sequence
      else {
        result = ((character.charCodeAt(0) - 0xD800) * 0x400 +
                   character.charCodeAt(1) + 0x2400).toString(16);
        result = '\\U00000000'.substr(0, 10 - result.length) + result;
      }
    }
    return result;
  }

  // Copyright Joyent, Inc. and other Node contributors.

  var R = typeof Reflect === 'object' ? Reflect : null;
  var ReflectApply = R && typeof R.apply === 'function'
    ? R.apply
    : function ReflectApply(target, receiver, args) {
      return Function.prototype.apply.call(target, receiver, args);
    };

  var ReflectOwnKeys;
  if (R && typeof R.ownKeys === 'function') {
    ReflectOwnKeys = R.ownKeys;
  } else if (Object.getOwnPropertySymbols) {
    ReflectOwnKeys = function ReflectOwnKeys(target) {
      return Object.getOwnPropertyNames(target)
        .concat(Object.getOwnPropertySymbols(target));
    };
  } else {
    ReflectOwnKeys = function ReflectOwnKeys(target) {
      return Object.getOwnPropertyNames(target);
    };
  }

  function ProcessEmitWarning(warning) {
    if (console && console.warn) console.warn(warning);
  }

  var NumberIsNaN = Number.isNaN || function NumberIsNaN(value) {
    return value !== value;
  };

  function EventEmitter() {
    EventEmitter.init.call(this);
  }
  var events = EventEmitter;
  var once_1 = once$2;

  // Backwards-compat with node 0.10.x
  EventEmitter.EventEmitter = EventEmitter;

  EventEmitter.prototype._events = undefined;
  EventEmitter.prototype._eventsCount = 0;
  EventEmitter.prototype._maxListeners = undefined;

  // By default EventEmitters will print a warning if more than 10 listeners are
  // added to it. This is a useful default which helps finding memory leaks.
  var defaultMaxListeners = 10;

  function checkListener(listener) {
    if (typeof listener !== 'function') {
      throw new TypeError('The "listener" argument must be of type Function. Received type ' + typeof listener);
    }
  }

  Object.defineProperty(EventEmitter, 'defaultMaxListeners', {
    enumerable: true,
    get: function() {
      return defaultMaxListeners;
    },
    set: function(arg) {
      if (typeof arg !== 'number' || arg < 0 || NumberIsNaN(arg)) {
        throw new RangeError('The value of "defaultMaxListeners" is out of range. It must be a non-negative number. Received ' + arg + '.');
      }
      defaultMaxListeners = arg;
    }
  });

  EventEmitter.init = function() {

    if (this._events === undefined ||
        this._events === Object.getPrototypeOf(this)._events) {
      this._events = Object.create(null);
      this._eventsCount = 0;
    }

    this._maxListeners = this._maxListeners || undefined;
  };

  // Obviously not all Emitters should be limited to 10. This function allows
  // that to be increased. Set to zero for unlimited.
  EventEmitter.prototype.setMaxListeners = function setMaxListeners(n) {
    if (typeof n !== 'number' || n < 0 || NumberIsNaN(n)) {
      throw new RangeError('The value of "n" is out of range. It must be a non-negative number. Received ' + n + '.');
    }
    this._maxListeners = n;
    return this;
  };

  function _getMaxListeners(that) {
    if (that._maxListeners === undefined)
      return EventEmitter.defaultMaxListeners;
    return that._maxListeners;
  }

  EventEmitter.prototype.getMaxListeners = function getMaxListeners() {
    return _getMaxListeners(this);
  };

  EventEmitter.prototype.emit = function emit(type) {
    var args = [];
    for (var i = 1; i < arguments.length; i++) args.push(arguments[i]);
    var doError = (type === 'error');

    var events = this._events;
    if (events !== undefined)
      doError = (doError && events.error === undefined);
    else if (!doError)
      return false;

    // If there is no 'error' event listener then throw.
    if (doError) {
      var er;
      if (args.length > 0)
        er = args[0];
      if (er instanceof Error) {
        // Note: The comments on the `throw` lines are intentional, they show
        // up in Node's output if this results in an unhandled exception.
        throw er; // Unhandled 'error' event
      }
      // At least give some kind of context to the user
      var err = new Error('Unhandled error.' + (er ? ' (' + er.message + ')' : ''));
      err.context = er;
      throw err; // Unhandled 'error' event
    }

    var handler = events[type];

    if (handler === undefined)
      return false;

    if (typeof handler === 'function') {
      ReflectApply(handler, this, args);
    } else {
      var len = handler.length;
      var listeners = arrayClone(handler, len);
      for (var i = 0; i < len; ++i)
        ReflectApply(listeners[i], this, args);
    }

    return true;
  };

  function _addListener(target, type, listener, prepend) {
    var m;
    var events;
    var existing;

    checkListener(listener);

    events = target._events;
    if (events === undefined) {
      events = target._events = Object.create(null);
      target._eventsCount = 0;
    } else {
      // To avoid recursion in the case that type === "newListener"! Before
      // adding it to the listeners, first emit "newListener".
      if (events.newListener !== undefined) {
        target.emit('newListener', type,
                    listener.listener ? listener.listener : listener);

        // Re-assign `events` because a newListener handler could have caused the
        // this._events to be assigned to a new object
        events = target._events;
      }
      existing = events[type];
    }

    if (existing === undefined) {
      // Optimize the case of one listener. Don't need the extra array object.
      existing = events[type] = listener;
      ++target._eventsCount;
    } else {
      if (typeof existing === 'function') {
        // Adding the second element, need to change to array.
        existing = events[type] =
          prepend ? [listener, existing] : [existing, listener];
        // If we've already got an array, just append.
      } else if (prepend) {
        existing.unshift(listener);
      } else {
        existing.push(listener);
      }

      // Check for listener leak
      m = _getMaxListeners(target);
      if (m > 0 && existing.length > m && !existing.warned) {
        existing.warned = true;
        // No error code for this since it is a Warning
        // eslint-disable-next-line no-restricted-syntax
        var w = new Error('Possible EventEmitter memory leak detected. ' +
                            existing.length + ' ' + String(type) + ' listeners ' +
                            'added. Use emitter.setMaxListeners() to ' +
                            'increase limit');
        w.name = 'MaxListenersExceededWarning';
        w.emitter = target;
        w.type = type;
        w.count = existing.length;
        ProcessEmitWarning(w);
      }
    }

    return target;
  }

  EventEmitter.prototype.addListener = function addListener(type, listener) {
    return _addListener(this, type, listener, false);
  };

  EventEmitter.prototype.on = EventEmitter.prototype.addListener;

  EventEmitter.prototype.prependListener =
      function prependListener(type, listener) {
        return _addListener(this, type, listener, true);
      };

  function onceWrapper() {
    if (!this.fired) {
      this.target.removeListener(this.type, this.wrapFn);
      this.fired = true;
      if (arguments.length === 0)
        return this.listener.call(this.target);
      return this.listener.apply(this.target, arguments);
    }
  }

  function _onceWrap(target, type, listener) {
    var state = { fired: false, wrapFn: undefined, target: target, type: type, listener: listener };
    var wrapped = onceWrapper.bind(state);
    wrapped.listener = listener;
    state.wrapFn = wrapped;
    return wrapped;
  }

  EventEmitter.prototype.once = function once(type, listener) {
    checkListener(listener);
    this.on(type, _onceWrap(this, type, listener));
    return this;
  };

  EventEmitter.prototype.prependOnceListener =
      function prependOnceListener(type, listener) {
        checkListener(listener);
        this.prependListener(type, _onceWrap(this, type, listener));
        return this;
      };

  // Emits a 'removeListener' event if and only if the listener was removed.
  EventEmitter.prototype.removeListener =
      function removeListener(type, listener) {
        var list, events, position, i, originalListener;

        checkListener(listener);

        events = this._events;
        if (events === undefined)
          return this;

        list = events[type];
        if (list === undefined)
          return this;

        if (list === listener || list.listener === listener) {
          if (--this._eventsCount === 0)
            this._events = Object.create(null);
          else {
            delete events[type];
            if (events.removeListener)
              this.emit('removeListener', type, list.listener || listener);
          }
        } else if (typeof list !== 'function') {
          position = -1;

          for (i = list.length - 1; i >= 0; i--) {
            if (list[i] === listener || list[i].listener === listener) {
              originalListener = list[i].listener;
              position = i;
              break;
            }
          }

          if (position < 0)
            return this;

          if (position === 0)
            list.shift();
          else {
            spliceOne(list, position);
          }

          if (list.length === 1)
            events[type] = list[0];

          if (events.removeListener !== undefined)
            this.emit('removeListener', type, originalListener || listener);
        }

        return this;
      };

  EventEmitter.prototype.off = EventEmitter.prototype.removeListener;

  EventEmitter.prototype.removeAllListeners =
      function removeAllListeners(type) {
        var listeners, events, i;

        events = this._events;
        if (events === undefined)
          return this;

        // not listening for removeListener, no need to emit
        if (events.removeListener === undefined) {
          if (arguments.length === 0) {
            this._events = Object.create(null);
            this._eventsCount = 0;
          } else if (events[type] !== undefined) {
            if (--this._eventsCount === 0)
              this._events = Object.create(null);
            else
              delete events[type];
          }
          return this;
        }

        // emit removeListener for all listeners on all events
        if (arguments.length === 0) {
          var keys = Object.keys(events);
          var key;
          for (i = 0; i < keys.length; ++i) {
            key = keys[i];
            if (key === 'removeListener') continue;
            this.removeAllListeners(key);
          }
          this.removeAllListeners('removeListener');
          this._events = Object.create(null);
          this._eventsCount = 0;
          return this;
        }

        listeners = events[type];

        if (typeof listeners === 'function') {
          this.removeListener(type, listeners);
        } else if (listeners !== undefined) {
          // LIFO order
          for (i = listeners.length - 1; i >= 0; i--) {
            this.removeListener(type, listeners[i]);
          }
        }

        return this;
      };

  function _listeners(target, type, unwrap) {
    var events = target._events;

    if (events === undefined)
      return [];

    var evlistener = events[type];
    if (evlistener === undefined)
      return [];

    if (typeof evlistener === 'function')
      return unwrap ? [evlistener.listener || evlistener] : [evlistener];

    return unwrap ?
      unwrapListeners(evlistener) : arrayClone(evlistener, evlistener.length);
  }

  EventEmitter.prototype.listeners = function listeners(type) {
    return _listeners(this, type, true);
  };

  EventEmitter.prototype.rawListeners = function rawListeners(type) {
    return _listeners(this, type, false);
  };

  EventEmitter.listenerCount = function(emitter, type) {
    if (typeof emitter.listenerCount === 'function') {
      return emitter.listenerCount(type);
    } else {
      return listenerCount.call(emitter, type);
    }
  };

  EventEmitter.prototype.listenerCount = listenerCount;
  function listenerCount(type) {
    var events = this._events;

    if (events !== undefined) {
      var evlistener = events[type];

      if (typeof evlistener === 'function') {
        return 1;
      } else if (evlistener !== undefined) {
        return evlistener.length;
      }
    }

    return 0;
  }

  EventEmitter.prototype.eventNames = function eventNames() {
    return this._eventsCount > 0 ? ReflectOwnKeys(this._events) : [];
  };

  function arrayClone(arr, n) {
    var copy = new Array(n);
    for (var i = 0; i < n; ++i)
      copy[i] = arr[i];
    return copy;
  }

  function spliceOne(list, index) {
    for (; index + 1 < list.length; index++)
      list[index] = list[index + 1];
    list.pop();
  }

  function unwrapListeners(arr) {
    var ret = new Array(arr.length);
    for (var i = 0; i < ret.length; ++i) {
      ret[i] = arr[i].listener || arr[i];
    }
    return ret;
  }

  function once$2(emitter, name) {
    return new Promise(function (resolve, reject) {
      function errorListener(err) {
        emitter.removeListener(name, resolver);
        reject(err);
      }

      function resolver() {
        if (typeof emitter.removeListener === 'function') {
          emitter.removeListener('error', errorListener);
        }
        resolve([].slice.call(arguments));
      }
      eventTargetAgnosticAddListener(emitter, name, resolver, { once: true });
      if (name !== 'error') {
        addErrorHandlerIfEventEmitter(emitter, errorListener, { once: true });
      }
    });
  }

  function addErrorHandlerIfEventEmitter(emitter, handler, flags) {
    if (typeof emitter.on === 'function') {
      eventTargetAgnosticAddListener(emitter, 'error', handler, flags);
    }
  }

  function eventTargetAgnosticAddListener(emitter, name, listener, flags) {
    if (typeof emitter.on === 'function') {
      if (flags.once) {
        emitter.once(name, listener);
      } else {
        emitter.on(name, listener);
      }
    } else if (typeof emitter.addEventListener === 'function') {
      // EventTarget does not have `error` event semantics like Node
      // EventEmitters, we do not listen for `error` events here.
      emitter.addEventListener(name, function wrapListener(arg) {
        // IE does not have builtin `{ once: true }` support so we
        // have to do it manually.
        if (flags.once) {
          emitter.removeEventListener(name, wrapListener);
        }
        listener(arg);
      });
    } else {
      throw new TypeError('The "emitter" argument must be of type EventEmitter. Received type ' + typeof emitter);
    }
  }
  events.once = once_1;

  var streamBrowser = events.EventEmitter;

  var byteLength_1 = byteLength;
  var toByteArray_1 = toByteArray;
  var fromByteArray_1 = fromByteArray;

  var lookup = [];
  var revLookup = [];
  var Arr = typeof Uint8Array !== 'undefined' ? Uint8Array : Array;

  var code = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  for (var i = 0, len = code.length; i < len; ++i) {
    lookup[i] = code[i];
    revLookup[code.charCodeAt(i)] = i;
  }

  // Support decoding URL-safe base64 strings, as Node.js does.
  // See: https://en.wikipedia.org/wiki/Base64#URL_applications
  revLookup['-'.charCodeAt(0)] = 62;
  revLookup['_'.charCodeAt(0)] = 63;

  function getLens (b64) {
    var len = b64.length;

    if (len % 4 > 0) {
      throw new Error('Invalid string. Length must be a multiple of 4')
    }

    // Trim off extra bytes after placeholder bytes are found
    // See: https://github.com/beatgammit/base64-js/issues/42
    var validLen = b64.indexOf('=');
    if (validLen === -1) validLen = len;

    var placeHoldersLen = validLen === len
      ? 0
      : 4 - (validLen % 4);

    return [validLen, placeHoldersLen]
  }

  // base64 is 4/3 + up to two characters of the original data
  function byteLength (b64) {
    var lens = getLens(b64);
    var validLen = lens[0];
    var placeHoldersLen = lens[1];
    return ((validLen + placeHoldersLen) * 3 / 4) - placeHoldersLen
  }

  function _byteLength (b64, validLen, placeHoldersLen) {
    return ((validLen + placeHoldersLen) * 3 / 4) - placeHoldersLen
  }

  function toByteArray (b64) {
    var tmp;
    var lens = getLens(b64);
    var validLen = lens[0];
    var placeHoldersLen = lens[1];

    var arr = new Arr(_byteLength(b64, validLen, placeHoldersLen));

    var curByte = 0;

    // if there are placeholders, only get up to the last complete 4 chars
    var len = placeHoldersLen > 0
      ? validLen - 4
      : validLen;

    var i;
    for (i = 0; i < len; i += 4) {
      tmp =
        (revLookup[b64.charCodeAt(i)] << 18) |
        (revLookup[b64.charCodeAt(i + 1)] << 12) |
        (revLookup[b64.charCodeAt(i + 2)] << 6) |
        revLookup[b64.charCodeAt(i + 3)];
      arr[curByte++] = (tmp >> 16) & 0xFF;
      arr[curByte++] = (tmp >> 8) & 0xFF;
      arr[curByte++] = tmp & 0xFF;
    }

    if (placeHoldersLen === 2) {
      tmp =
        (revLookup[b64.charCodeAt(i)] << 2) |
        (revLookup[b64.charCodeAt(i + 1)] >> 4);
      arr[curByte++] = tmp & 0xFF;
    }

    if (placeHoldersLen === 1) {
      tmp =
        (revLookup[b64.charCodeAt(i)] << 10) |
        (revLookup[b64.charCodeAt(i + 1)] << 4) |
        (revLookup[b64.charCodeAt(i + 2)] >> 2);
      arr[curByte++] = (tmp >> 8) & 0xFF;
      arr[curByte++] = tmp & 0xFF;
    }

    return arr
  }

  function tripletToBase64 (num) {
    return lookup[num >> 18 & 0x3F] +
      lookup[num >> 12 & 0x3F] +
      lookup[num >> 6 & 0x3F] +
      lookup[num & 0x3F]
  }

  function encodeChunk (uint8, start, end) {
    var tmp;
    var output = [];
    for (var i = start; i < end; i += 3) {
      tmp =
        ((uint8[i] << 16) & 0xFF0000) +
        ((uint8[i + 1] << 8) & 0xFF00) +
        (uint8[i + 2] & 0xFF);
      output.push(tripletToBase64(tmp));
    }
    return output.join('')
  }

  function fromByteArray (uint8) {
    var tmp;
    var len = uint8.length;
    var extraBytes = len % 3; // if we have 1 byte left, pad 2 bytes
    var parts = [];
    var maxChunkLength = 16383; // must be multiple of 3

    // go through the array every three bytes, we'll deal with trailing stuff later
    for (var i = 0, len2 = len - extraBytes; i < len2; i += maxChunkLength) {
      parts.push(encodeChunk(uint8, i, (i + maxChunkLength) > len2 ? len2 : (i + maxChunkLength)));
    }

    // pad the end with zeros, but make sure to not forget the extra bytes
    if (extraBytes === 1) {
      tmp = uint8[len - 1];
      parts.push(
        lookup[tmp >> 2] +
        lookup[(tmp << 4) & 0x3F] +
        '=='
      );
    } else if (extraBytes === 2) {
      tmp = (uint8[len - 2] << 8) + uint8[len - 1];
      parts.push(
        lookup[tmp >> 10] +
        lookup[(tmp >> 4) & 0x3F] +
        lookup[(tmp << 2) & 0x3F] +
        '='
      );
    }

    return parts.join('')
  }

  var base64Js = {
  	byteLength: byteLength_1,
  	toByteArray: toByteArray_1,
  	fromByteArray: fromByteArray_1
  };

  /*! ieee754. BSD-3-Clause License. Feross Aboukhadijeh <https://feross.org/opensource> */
  var read = function (buffer, offset, isLE, mLen, nBytes) {
    var e, m;
    var eLen = (nBytes * 8) - mLen - 1;
    var eMax = (1 << eLen) - 1;
    var eBias = eMax >> 1;
    var nBits = -7;
    var i = isLE ? (nBytes - 1) : 0;
    var d = isLE ? -1 : 1;
    var s = buffer[offset + i];

    i += d;

    e = s & ((1 << (-nBits)) - 1);
    s >>= (-nBits);
    nBits += eLen;
    for (; nBits > 0; e = (e * 256) + buffer[offset + i], i += d, nBits -= 8) {}

    m = e & ((1 << (-nBits)) - 1);
    e >>= (-nBits);
    nBits += mLen;
    for (; nBits > 0; m = (m * 256) + buffer[offset + i], i += d, nBits -= 8) {}

    if (e === 0) {
      e = 1 - eBias;
    } else if (e === eMax) {
      return m ? NaN : ((s ? -1 : 1) * Infinity)
    } else {
      m = m + Math.pow(2, mLen);
      e = e - eBias;
    }
    return (s ? -1 : 1) * m * Math.pow(2, e - mLen)
  };

  var write = function (buffer, value, offset, isLE, mLen, nBytes) {
    var e, m, c;
    var eLen = (nBytes * 8) - mLen - 1;
    var eMax = (1 << eLen) - 1;
    var eBias = eMax >> 1;
    var rt = (mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0);
    var i = isLE ? 0 : (nBytes - 1);
    var d = isLE ? 1 : -1;
    var s = value < 0 || (value === 0 && 1 / value < 0) ? 1 : 0;

    value = Math.abs(value);

    if (isNaN(value) || value === Infinity) {
      m = isNaN(value) ? 1 : 0;
      e = eMax;
    } else {
      e = Math.floor(Math.log(value) / Math.LN2);
      if (value * (c = Math.pow(2, -e)) < 1) {
        e--;
        c *= 2;
      }
      if (e + eBias >= 1) {
        value += rt / c;
      } else {
        value += rt * Math.pow(2, 1 - eBias);
      }
      if (value * c >= 2) {
        e++;
        c /= 2;
      }

      if (e + eBias >= eMax) {
        m = 0;
        e = eMax;
      } else if (e + eBias >= 1) {
        m = ((value * c) - 1) * Math.pow(2, mLen);
        e = e + eBias;
      } else {
        m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen);
        e = 0;
      }
    }

    for (; mLen >= 8; buffer[offset + i] = m & 0xff, i += d, m /= 256, mLen -= 8) {}

    e = (e << mLen) | m;
    eLen += mLen;
    for (; eLen > 0; buffer[offset + i] = e & 0xff, i += d, e /= 256, eLen -= 8) {}

    buffer[offset + i - d] |= s * 128;
  };

  var ieee754 = {
  	read: read,
  	write: write
  };

  /*!
   * The buffer module from node.js, for the browser.
   *
   * @author   Feross Aboukhadijeh <https://feross.org>
   * @license  MIT
   */

  var buffer = createCommonjsModule(function (module, exports) {



  const customInspectSymbol =
    (typeof Symbol === 'function' && typeof Symbol['for'] === 'function') // eslint-disable-line dot-notation
      ? Symbol['for']('nodejs.util.inspect.custom') // eslint-disable-line dot-notation
      : null;

  exports.Buffer = Buffer;
  exports.SlowBuffer = SlowBuffer;
  exports.INSPECT_MAX_BYTES = 50;

  const K_MAX_LENGTH = 0x7fffffff;
  exports.kMaxLength = K_MAX_LENGTH;

  /**
   * If `Buffer.TYPED_ARRAY_SUPPORT`:
   *   === true    Use Uint8Array implementation (fastest)
   *   === false   Print warning and recommend using `buffer` v4.x which has an Object
   *               implementation (most compatible, even IE6)
   *
   * Browsers that support typed arrays are IE 10+, Firefox 4+, Chrome 7+, Safari 5.1+,
   * Opera 11.6+, iOS 4.2+.
   *
   * We report that the browser does not support typed arrays if the are not subclassable
   * using __proto__. Firefox 4-29 lacks support for adding new properties to `Uint8Array`
   * (See: https://bugzilla.mozilla.org/show_bug.cgi?id=695438). IE 10 lacks support
   * for __proto__ and has a buggy typed array implementation.
   */
  Buffer.TYPED_ARRAY_SUPPORT = typedArraySupport();

  if (!Buffer.TYPED_ARRAY_SUPPORT && typeof console !== 'undefined' &&
      typeof console.error === 'function') {
    console.error(
      'This browser lacks typed array (Uint8Array) support which is required by ' +
      '`buffer` v5.x. Use `buffer` v4.x if you require old browser support.'
    );
  }

  function typedArraySupport () {
    // Can typed array instances can be augmented?
    try {
      const arr = new Uint8Array(1);
      const proto = { foo: function () { return 42 } };
      Object.setPrototypeOf(proto, Uint8Array.prototype);
      Object.setPrototypeOf(arr, proto);
      return arr.foo() === 42
    } catch (e) {
      return false
    }
  }

  Object.defineProperty(Buffer.prototype, 'parent', {
    enumerable: true,
    get: function () {
      if (!Buffer.isBuffer(this)) return undefined
      return this.buffer
    }
  });

  Object.defineProperty(Buffer.prototype, 'offset', {
    enumerable: true,
    get: function () {
      if (!Buffer.isBuffer(this)) return undefined
      return this.byteOffset
    }
  });

  function createBuffer (length) {
    if (length > K_MAX_LENGTH) {
      throw new RangeError('The value "' + length + '" is invalid for option "size"')
    }
    // Return an augmented `Uint8Array` instance
    const buf = new Uint8Array(length);
    Object.setPrototypeOf(buf, Buffer.prototype);
    return buf
  }

  /**
   * The Buffer constructor returns instances of `Uint8Array` that have their
   * prototype changed to `Buffer.prototype`. Furthermore, `Buffer` is a subclass of
   * `Uint8Array`, so the returned instances will have all the node `Buffer` methods
   * and the `Uint8Array` methods. Square bracket notation works as expected -- it
   * returns a single octet.
   *
   * The `Uint8Array` prototype remains unmodified.
   */

  function Buffer (arg, encodingOrOffset, length) {
    // Common case.
    if (typeof arg === 'number') {
      if (typeof encodingOrOffset === 'string') {
        throw new TypeError(
          'The "string" argument must be of type string. Received type number'
        )
      }
      return allocUnsafe(arg)
    }
    return from(arg, encodingOrOffset, length)
  }

  Buffer.poolSize = 8192; // not used by this implementation

  function from (value, encodingOrOffset, length) {
    if (typeof value === 'string') {
      return fromString(value, encodingOrOffset)
    }

    if (ArrayBuffer.isView(value)) {
      return fromArrayView(value)
    }

    if (value == null) {
      throw new TypeError(
        'The first argument must be one of type string, Buffer, ArrayBuffer, Array, ' +
        'or Array-like Object. Received type ' + (typeof value)
      )
    }

    if (isInstance(value, ArrayBuffer) ||
        (value && isInstance(value.buffer, ArrayBuffer))) {
      return fromArrayBuffer(value, encodingOrOffset, length)
    }

    if (typeof SharedArrayBuffer !== 'undefined' &&
        (isInstance(value, SharedArrayBuffer) ||
        (value && isInstance(value.buffer, SharedArrayBuffer)))) {
      return fromArrayBuffer(value, encodingOrOffset, length)
    }

    if (typeof value === 'number') {
      throw new TypeError(
        'The "value" argument must not be of type number. Received type number'
      )
    }

    const valueOf = value.valueOf && value.valueOf();
    if (valueOf != null && valueOf !== value) {
      return Buffer.from(valueOf, encodingOrOffset, length)
    }

    const b = fromObject(value);
    if (b) return b

    if (typeof Symbol !== 'undefined' && Symbol.toPrimitive != null &&
        typeof value[Symbol.toPrimitive] === 'function') {
      return Buffer.from(value[Symbol.toPrimitive]('string'), encodingOrOffset, length)
    }

    throw new TypeError(
      'The first argument must be one of type string, Buffer, ArrayBuffer, Array, ' +
      'or Array-like Object. Received type ' + (typeof value)
    )
  }

  /**
   * Functionally equivalent to Buffer(arg, encoding) but throws a TypeError
   * if value is a number.
   * Buffer.from(str[, encoding])
   * Buffer.from(array)
   * Buffer.from(buffer)
   * Buffer.from(arrayBuffer[, byteOffset[, length]])
   **/
  Buffer.from = function (value, encodingOrOffset, length) {
    return from(value, encodingOrOffset, length)
  };

  // Note: Change prototype *after* Buffer.from is defined to workaround Chrome bug:
  // https://github.com/feross/buffer/pull/148
  Object.setPrototypeOf(Buffer.prototype, Uint8Array.prototype);
  Object.setPrototypeOf(Buffer, Uint8Array);

  function assertSize (size) {
    if (typeof size !== 'number') {
      throw new TypeError('"size" argument must be of type number')
    } else if (size < 0) {
      throw new RangeError('The value "' + size + '" is invalid for option "size"')
    }
  }

  function alloc (size, fill, encoding) {
    assertSize(size);
    if (size <= 0) {
      return createBuffer(size)
    }
    if (fill !== undefined) {
      // Only pay attention to encoding if it's a string. This
      // prevents accidentally sending in a number that would
      // be interpreted as a start offset.
      return typeof encoding === 'string'
        ? createBuffer(size).fill(fill, encoding)
        : createBuffer(size).fill(fill)
    }
    return createBuffer(size)
  }

  /**
   * Creates a new filled Buffer instance.
   * alloc(size[, fill[, encoding]])
   **/
  Buffer.alloc = function (size, fill, encoding) {
    return alloc(size, fill, encoding)
  };

  function allocUnsafe (size) {
    assertSize(size);
    return createBuffer(size < 0 ? 0 : checked(size) | 0)
  }

  /**
   * Equivalent to Buffer(num), by default creates a non-zero-filled Buffer instance.
   * */
  Buffer.allocUnsafe = function (size) {
    return allocUnsafe(size)
  };
  /**
   * Equivalent to SlowBuffer(num), by default creates a non-zero-filled Buffer instance.
   */
  Buffer.allocUnsafeSlow = function (size) {
    return allocUnsafe(size)
  };

  function fromString (string, encoding) {
    if (typeof encoding !== 'string' || encoding === '') {
      encoding = 'utf8';
    }

    if (!Buffer.isEncoding(encoding)) {
      throw new TypeError('Unknown encoding: ' + encoding)
    }

    const length = byteLength(string, encoding) | 0;
    let buf = createBuffer(length);

    const actual = buf.write(string, encoding);

    if (actual !== length) {
      // Writing a hex string, for example, that contains invalid characters will
      // cause everything after the first invalid character to be ignored. (e.g.
      // 'abxxcd' will be treated as 'ab')
      buf = buf.slice(0, actual);
    }

    return buf
  }

  function fromArrayLike (array) {
    const length = array.length < 0 ? 0 : checked(array.length) | 0;
    const buf = createBuffer(length);
    for (let i = 0; i < length; i += 1) {
      buf[i] = array[i] & 255;
    }
    return buf
  }

  function fromArrayView (arrayView) {
    if (isInstance(arrayView, Uint8Array)) {
      const copy = new Uint8Array(arrayView);
      return fromArrayBuffer(copy.buffer, copy.byteOffset, copy.byteLength)
    }
    return fromArrayLike(arrayView)
  }

  function fromArrayBuffer (array, byteOffset, length) {
    if (byteOffset < 0 || array.byteLength < byteOffset) {
      throw new RangeError('"offset" is outside of buffer bounds')
    }

    if (array.byteLength < byteOffset + (length || 0)) {
      throw new RangeError('"length" is outside of buffer bounds')
    }

    let buf;
    if (byteOffset === undefined && length === undefined) {
      buf = new Uint8Array(array);
    } else if (length === undefined) {
      buf = new Uint8Array(array, byteOffset);
    } else {
      buf = new Uint8Array(array, byteOffset, length);
    }

    // Return an augmented `Uint8Array` instance
    Object.setPrototypeOf(buf, Buffer.prototype);

    return buf
  }

  function fromObject (obj) {
    if (Buffer.isBuffer(obj)) {
      const len = checked(obj.length) | 0;
      const buf = createBuffer(len);

      if (buf.length === 0) {
        return buf
      }

      obj.copy(buf, 0, 0, len);
      return buf
    }

    if (obj.length !== undefined) {
      if (typeof obj.length !== 'number' || numberIsNaN(obj.length)) {
        return createBuffer(0)
      }
      return fromArrayLike(obj)
    }

    if (obj.type === 'Buffer' && Array.isArray(obj.data)) {
      return fromArrayLike(obj.data)
    }
  }

  function checked (length) {
    // Note: cannot use `length < K_MAX_LENGTH` here because that fails when
    // length is NaN (which is otherwise coerced to zero.)
    if (length >= K_MAX_LENGTH) {
      throw new RangeError('Attempt to allocate Buffer larger than maximum ' +
                           'size: 0x' + K_MAX_LENGTH.toString(16) + ' bytes')
    }
    return length | 0
  }

  function SlowBuffer (length) {
    if (+length != length) { // eslint-disable-line eqeqeq
      length = 0;
    }
    return Buffer.alloc(+length)
  }

  Buffer.isBuffer = function isBuffer (b) {
    return b != null && b._isBuffer === true &&
      b !== Buffer.prototype // so Buffer.isBuffer(Buffer.prototype) will be false
  };

  Buffer.compare = function compare (a, b) {
    if (isInstance(a, Uint8Array)) a = Buffer.from(a, a.offset, a.byteLength);
    if (isInstance(b, Uint8Array)) b = Buffer.from(b, b.offset, b.byteLength);
    if (!Buffer.isBuffer(a) || !Buffer.isBuffer(b)) {
      throw new TypeError(
        'The "buf1", "buf2" arguments must be one of type Buffer or Uint8Array'
      )
    }

    if (a === b) return 0

    let x = a.length;
    let y = b.length;

    for (let i = 0, len = Math.min(x, y); i < len; ++i) {
      if (a[i] !== b[i]) {
        x = a[i];
        y = b[i];
        break
      }
    }

    if (x < y) return -1
    if (y < x) return 1
    return 0
  };

  Buffer.isEncoding = function isEncoding (encoding) {
    switch (String(encoding).toLowerCase()) {
      case 'hex':
      case 'utf8':
      case 'utf-8':
      case 'ascii':
      case 'latin1':
      case 'binary':
      case 'base64':
      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return true
      default:
        return false
    }
  };

  Buffer.concat = function concat (list, length) {
    if (!Array.isArray(list)) {
      throw new TypeError('"list" argument must be an Array of Buffers')
    }

    if (list.length === 0) {
      return Buffer.alloc(0)
    }

    let i;
    if (length === undefined) {
      length = 0;
      for (i = 0; i < list.length; ++i) {
        length += list[i].length;
      }
    }

    const buffer = Buffer.allocUnsafe(length);
    let pos = 0;
    for (i = 0; i < list.length; ++i) {
      let buf = list[i];
      if (isInstance(buf, Uint8Array)) {
        if (pos + buf.length > buffer.length) {
          if (!Buffer.isBuffer(buf)) buf = Buffer.from(buf);
          buf.copy(buffer, pos);
        } else {
          Uint8Array.prototype.set.call(
            buffer,
            buf,
            pos
          );
        }
      } else if (!Buffer.isBuffer(buf)) {
        throw new TypeError('"list" argument must be an Array of Buffers')
      } else {
        buf.copy(buffer, pos);
      }
      pos += buf.length;
    }
    return buffer
  };

  function byteLength (string, encoding) {
    if (Buffer.isBuffer(string)) {
      return string.length
    }
    if (ArrayBuffer.isView(string) || isInstance(string, ArrayBuffer)) {
      return string.byteLength
    }
    if (typeof string !== 'string') {
      throw new TypeError(
        'The "string" argument must be one of type string, Buffer, or ArrayBuffer. ' +
        'Received type ' + typeof string
      )
    }

    const len = string.length;
    const mustMatch = (arguments.length > 2 && arguments[2] === true);
    if (!mustMatch && len === 0) return 0

    // Use a for loop to avoid recursion
    let loweredCase = false;
    for (;;) {
      switch (encoding) {
        case 'ascii':
        case 'latin1':
        case 'binary':
          return len
        case 'utf8':
        case 'utf-8':
          return utf8ToBytes(string).length
        case 'ucs2':
        case 'ucs-2':
        case 'utf16le':
        case 'utf-16le':
          return len * 2
        case 'hex':
          return len >>> 1
        case 'base64':
          return base64ToBytes(string).length
        default:
          if (loweredCase) {
            return mustMatch ? -1 : utf8ToBytes(string).length // assume utf8
          }
          encoding = ('' + encoding).toLowerCase();
          loweredCase = true;
      }
    }
  }
  Buffer.byteLength = byteLength;

  function slowToString (encoding, start, end) {
    let loweredCase = false;

    // No need to verify that "this.length <= MAX_UINT32" since it's a read-only
    // property of a typed array.

    // This behaves neither like String nor Uint8Array in that we set start/end
    // to their upper/lower bounds if the value passed is out of range.
    // undefined is handled specially as per ECMA-262 6th Edition,
    // Section 13.3.3.7 Runtime Semantics: KeyedBindingInitialization.
    if (start === undefined || start < 0) {
      start = 0;
    }
    // Return early if start > this.length. Done here to prevent potential uint32
    // coercion fail below.
    if (start > this.length) {
      return ''
    }

    if (end === undefined || end > this.length) {
      end = this.length;
    }

    if (end <= 0) {
      return ''
    }

    // Force coercion to uint32. This will also coerce falsey/NaN values to 0.
    end >>>= 0;
    start >>>= 0;

    if (end <= start) {
      return ''
    }

    if (!encoding) encoding = 'utf8';

    while (true) {
      switch (encoding) {
        case 'hex':
          return hexSlice(this, start, end)

        case 'utf8':
        case 'utf-8':
          return utf8Slice(this, start, end)

        case 'ascii':
          return asciiSlice(this, start, end)

        case 'latin1':
        case 'binary':
          return latin1Slice(this, start, end)

        case 'base64':
          return base64Slice(this, start, end)

        case 'ucs2':
        case 'ucs-2':
        case 'utf16le':
        case 'utf-16le':
          return utf16leSlice(this, start, end)

        default:
          if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
          encoding = (encoding + '').toLowerCase();
          loweredCase = true;
      }
    }
  }

  // This property is used by `Buffer.isBuffer` (and the `is-buffer` npm package)
  // to detect a Buffer instance. It's not possible to use `instanceof Buffer`
  // reliably in a browserify context because there could be multiple different
  // copies of the 'buffer' package in use. This method works even for Buffer
  // instances that were created from another copy of the `buffer` package.
  // See: https://github.com/feross/buffer/issues/154
  Buffer.prototype._isBuffer = true;

  function swap (b, n, m) {
    const i = b[n];
    b[n] = b[m];
    b[m] = i;
  }

  Buffer.prototype.swap16 = function swap16 () {
    const len = this.length;
    if (len % 2 !== 0) {
      throw new RangeError('Buffer size must be a multiple of 16-bits')
    }
    for (let i = 0; i < len; i += 2) {
      swap(this, i, i + 1);
    }
    return this
  };

  Buffer.prototype.swap32 = function swap32 () {
    const len = this.length;
    if (len % 4 !== 0) {
      throw new RangeError('Buffer size must be a multiple of 32-bits')
    }
    for (let i = 0; i < len; i += 4) {
      swap(this, i, i + 3);
      swap(this, i + 1, i + 2);
    }
    return this
  };

  Buffer.prototype.swap64 = function swap64 () {
    const len = this.length;
    if (len % 8 !== 0) {
      throw new RangeError('Buffer size must be a multiple of 64-bits')
    }
    for (let i = 0; i < len; i += 8) {
      swap(this, i, i + 7);
      swap(this, i + 1, i + 6);
      swap(this, i + 2, i + 5);
      swap(this, i + 3, i + 4);
    }
    return this
  };

  Buffer.prototype.toString = function toString () {
    const length = this.length;
    if (length === 0) return ''
    if (arguments.length === 0) return utf8Slice(this, 0, length)
    return slowToString.apply(this, arguments)
  };

  Buffer.prototype.toLocaleString = Buffer.prototype.toString;

  Buffer.prototype.equals = function equals (b) {
    if (!Buffer.isBuffer(b)) throw new TypeError('Argument must be a Buffer')
    if (this === b) return true
    return Buffer.compare(this, b) === 0
  };

  Buffer.prototype.inspect = function inspect () {
    let str = '';
    const max = exports.INSPECT_MAX_BYTES;
    str = this.toString('hex', 0, max).replace(/(.{2})/g, '$1 ').trim();
    if (this.length > max) str += ' ... ';
    return '<Buffer ' + str + '>'
  };
  if (customInspectSymbol) {
    Buffer.prototype[customInspectSymbol] = Buffer.prototype.inspect;
  }

  Buffer.prototype.compare = function compare (target, start, end, thisStart, thisEnd) {
    if (isInstance(target, Uint8Array)) {
      target = Buffer.from(target, target.offset, target.byteLength);
    }
    if (!Buffer.isBuffer(target)) {
      throw new TypeError(
        'The "target" argument must be one of type Buffer or Uint8Array. ' +
        'Received type ' + (typeof target)
      )
    }

    if (start === undefined) {
      start = 0;
    }
    if (end === undefined) {
      end = target ? target.length : 0;
    }
    if (thisStart === undefined) {
      thisStart = 0;
    }
    if (thisEnd === undefined) {
      thisEnd = this.length;
    }

    if (start < 0 || end > target.length || thisStart < 0 || thisEnd > this.length) {
      throw new RangeError('out of range index')
    }

    if (thisStart >= thisEnd && start >= end) {
      return 0
    }
    if (thisStart >= thisEnd) {
      return -1
    }
    if (start >= end) {
      return 1
    }

    start >>>= 0;
    end >>>= 0;
    thisStart >>>= 0;
    thisEnd >>>= 0;

    if (this === target) return 0

    let x = thisEnd - thisStart;
    let y = end - start;
    const len = Math.min(x, y);

    const thisCopy = this.slice(thisStart, thisEnd);
    const targetCopy = target.slice(start, end);

    for (let i = 0; i < len; ++i) {
      if (thisCopy[i] !== targetCopy[i]) {
        x = thisCopy[i];
        y = targetCopy[i];
        break
      }
    }

    if (x < y) return -1
    if (y < x) return 1
    return 0
  };

  // Finds either the first index of `val` in `buffer` at offset >= `byteOffset`,
  // OR the last index of `val` in `buffer` at offset <= `byteOffset`.
  //
  // Arguments:
  // - buffer - a Buffer to search
  // - val - a string, Buffer, or number
  // - byteOffset - an index into `buffer`; will be clamped to an int32
  // - encoding - an optional encoding, relevant is val is a string
  // - dir - true for indexOf, false for lastIndexOf
  function bidirectionalIndexOf (buffer, val, byteOffset, encoding, dir) {
    // Empty buffer means no match
    if (buffer.length === 0) return -1

    // Normalize byteOffset
    if (typeof byteOffset === 'string') {
      encoding = byteOffset;
      byteOffset = 0;
    } else if (byteOffset > 0x7fffffff) {
      byteOffset = 0x7fffffff;
    } else if (byteOffset < -0x80000000) {
      byteOffset = -0x80000000;
    }
    byteOffset = +byteOffset; // Coerce to Number.
    if (numberIsNaN(byteOffset)) {
      // byteOffset: it it's undefined, null, NaN, "foo", etc, search whole buffer
      byteOffset = dir ? 0 : (buffer.length - 1);
    }

    // Normalize byteOffset: negative offsets start from the end of the buffer
    if (byteOffset < 0) byteOffset = buffer.length + byteOffset;
    if (byteOffset >= buffer.length) {
      if (dir) return -1
      else byteOffset = buffer.length - 1;
    } else if (byteOffset < 0) {
      if (dir) byteOffset = 0;
      else return -1
    }

    // Normalize val
    if (typeof val === 'string') {
      val = Buffer.from(val, encoding);
    }

    // Finally, search either indexOf (if dir is true) or lastIndexOf
    if (Buffer.isBuffer(val)) {
      // Special case: looking for empty string/buffer always fails
      if (val.length === 0) {
        return -1
      }
      return arrayIndexOf(buffer, val, byteOffset, encoding, dir)
    } else if (typeof val === 'number') {
      val = val & 0xFF; // Search for a byte value [0-255]
      if (typeof Uint8Array.prototype.indexOf === 'function') {
        if (dir) {
          return Uint8Array.prototype.indexOf.call(buffer, val, byteOffset)
        } else {
          return Uint8Array.prototype.lastIndexOf.call(buffer, val, byteOffset)
        }
      }
      return arrayIndexOf(buffer, [val], byteOffset, encoding, dir)
    }

    throw new TypeError('val must be string, number or Buffer')
  }

  function arrayIndexOf (arr, val, byteOffset, encoding, dir) {
    let indexSize = 1;
    let arrLength = arr.length;
    let valLength = val.length;

    if (encoding !== undefined) {
      encoding = String(encoding).toLowerCase();
      if (encoding === 'ucs2' || encoding === 'ucs-2' ||
          encoding === 'utf16le' || encoding === 'utf-16le') {
        if (arr.length < 2 || val.length < 2) {
          return -1
        }
        indexSize = 2;
        arrLength /= 2;
        valLength /= 2;
        byteOffset /= 2;
      }
    }

    function read (buf, i) {
      if (indexSize === 1) {
        return buf[i]
      } else {
        return buf.readUInt16BE(i * indexSize)
      }
    }

    let i;
    if (dir) {
      let foundIndex = -1;
      for (i = byteOffset; i < arrLength; i++) {
        if (read(arr, i) === read(val, foundIndex === -1 ? 0 : i - foundIndex)) {
          if (foundIndex === -1) foundIndex = i;
          if (i - foundIndex + 1 === valLength) return foundIndex * indexSize
        } else {
          if (foundIndex !== -1) i -= i - foundIndex;
          foundIndex = -1;
        }
      }
    } else {
      if (byteOffset + valLength > arrLength) byteOffset = arrLength - valLength;
      for (i = byteOffset; i >= 0; i--) {
        let found = true;
        for (let j = 0; j < valLength; j++) {
          if (read(arr, i + j) !== read(val, j)) {
            found = false;
            break
          }
        }
        if (found) return i
      }
    }

    return -1
  }

  Buffer.prototype.includes = function includes (val, byteOffset, encoding) {
    return this.indexOf(val, byteOffset, encoding) !== -1
  };

  Buffer.prototype.indexOf = function indexOf (val, byteOffset, encoding) {
    return bidirectionalIndexOf(this, val, byteOffset, encoding, true)
  };

  Buffer.prototype.lastIndexOf = function lastIndexOf (val, byteOffset, encoding) {
    return bidirectionalIndexOf(this, val, byteOffset, encoding, false)
  };

  function hexWrite (buf, string, offset, length) {
    offset = Number(offset) || 0;
    const remaining = buf.length - offset;
    if (!length) {
      length = remaining;
    } else {
      length = Number(length);
      if (length > remaining) {
        length = remaining;
      }
    }

    const strLen = string.length;

    if (length > strLen / 2) {
      length = strLen / 2;
    }
    let i;
    for (i = 0; i < length; ++i) {
      const parsed = parseInt(string.substr(i * 2, 2), 16);
      if (numberIsNaN(parsed)) return i
      buf[offset + i] = parsed;
    }
    return i
  }

  function utf8Write (buf, string, offset, length) {
    return blitBuffer(utf8ToBytes(string, buf.length - offset), buf, offset, length)
  }

  function asciiWrite (buf, string, offset, length) {
    return blitBuffer(asciiToBytes(string), buf, offset, length)
  }

  function base64Write (buf, string, offset, length) {
    return blitBuffer(base64ToBytes(string), buf, offset, length)
  }

  function ucs2Write (buf, string, offset, length) {
    return blitBuffer(utf16leToBytes(string, buf.length - offset), buf, offset, length)
  }

  Buffer.prototype.write = function write (string, offset, length, encoding) {
    // Buffer#write(string)
    if (offset === undefined) {
      encoding = 'utf8';
      length = this.length;
      offset = 0;
    // Buffer#write(string, encoding)
    } else if (length === undefined && typeof offset === 'string') {
      encoding = offset;
      length = this.length;
      offset = 0;
    // Buffer#write(string, offset[, length][, encoding])
    } else if (isFinite(offset)) {
      offset = offset >>> 0;
      if (isFinite(length)) {
        length = length >>> 0;
        if (encoding === undefined) encoding = 'utf8';
      } else {
        encoding = length;
        length = undefined;
      }
    } else {
      throw new Error(
        'Buffer.write(string, encoding, offset[, length]) is no longer supported'
      )
    }

    const remaining = this.length - offset;
    if (length === undefined || length > remaining) length = remaining;

    if ((string.length > 0 && (length < 0 || offset < 0)) || offset > this.length) {
      throw new RangeError('Attempt to write outside buffer bounds')
    }

    if (!encoding) encoding = 'utf8';

    let loweredCase = false;
    for (;;) {
      switch (encoding) {
        case 'hex':
          return hexWrite(this, string, offset, length)

        case 'utf8':
        case 'utf-8':
          return utf8Write(this, string, offset, length)

        case 'ascii':
        case 'latin1':
        case 'binary':
          return asciiWrite(this, string, offset, length)

        case 'base64':
          // Warning: maxLength not taken into account in base64Write
          return base64Write(this, string, offset, length)

        case 'ucs2':
        case 'ucs-2':
        case 'utf16le':
        case 'utf-16le':
          return ucs2Write(this, string, offset, length)

        default:
          if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
          encoding = ('' + encoding).toLowerCase();
          loweredCase = true;
      }
    }
  };

  Buffer.prototype.toJSON = function toJSON () {
    return {
      type: 'Buffer',
      data: Array.prototype.slice.call(this._arr || this, 0)
    }
  };

  function base64Slice (buf, start, end) {
    if (start === 0 && end === buf.length) {
      return base64Js.fromByteArray(buf)
    } else {
      return base64Js.fromByteArray(buf.slice(start, end))
    }
  }

  function utf8Slice (buf, start, end) {
    end = Math.min(buf.length, end);
    const res = [];

    let i = start;
    while (i < end) {
      const firstByte = buf[i];
      let codePoint = null;
      let bytesPerSequence = (firstByte > 0xEF)
        ? 4
        : (firstByte > 0xDF)
            ? 3
            : (firstByte > 0xBF)
                ? 2
                : 1;

      if (i + bytesPerSequence <= end) {
        let secondByte, thirdByte, fourthByte, tempCodePoint;

        switch (bytesPerSequence) {
          case 1:
            if (firstByte < 0x80) {
              codePoint = firstByte;
            }
            break
          case 2:
            secondByte = buf[i + 1];
            if ((secondByte & 0xC0) === 0x80) {
              tempCodePoint = (firstByte & 0x1F) << 0x6 | (secondByte & 0x3F);
              if (tempCodePoint > 0x7F) {
                codePoint = tempCodePoint;
              }
            }
            break
          case 3:
            secondByte = buf[i + 1];
            thirdByte = buf[i + 2];
            if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80) {
              tempCodePoint = (firstByte & 0xF) << 0xC | (secondByte & 0x3F) << 0x6 | (thirdByte & 0x3F);
              if (tempCodePoint > 0x7FF && (tempCodePoint < 0xD800 || tempCodePoint > 0xDFFF)) {
                codePoint = tempCodePoint;
              }
            }
            break
          case 4:
            secondByte = buf[i + 1];
            thirdByte = buf[i + 2];
            fourthByte = buf[i + 3];
            if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80 && (fourthByte & 0xC0) === 0x80) {
              tempCodePoint = (firstByte & 0xF) << 0x12 | (secondByte & 0x3F) << 0xC | (thirdByte & 0x3F) << 0x6 | (fourthByte & 0x3F);
              if (tempCodePoint > 0xFFFF && tempCodePoint < 0x110000) {
                codePoint = tempCodePoint;
              }
            }
        }
      }

      if (codePoint === null) {
        // we did not generate a valid codePoint so insert a
        // replacement char (U+FFFD) and advance only 1 byte
        codePoint = 0xFFFD;
        bytesPerSequence = 1;
      } else if (codePoint > 0xFFFF) {
        // encode to utf16 (surrogate pair dance)
        codePoint -= 0x10000;
        res.push(codePoint >>> 10 & 0x3FF | 0xD800);
        codePoint = 0xDC00 | codePoint & 0x3FF;
      }

      res.push(codePoint);
      i += bytesPerSequence;
    }

    return decodeCodePointsArray(res)
  }

  // Based on http://stackoverflow.com/a/22747272/680742, the browser with
  // the lowest limit is Chrome, with 0x10000 args.
  // We go 1 magnitude less, for safety
  const MAX_ARGUMENTS_LENGTH = 0x1000;

  function decodeCodePointsArray (codePoints) {
    const len = codePoints.length;
    if (len <= MAX_ARGUMENTS_LENGTH) {
      return String.fromCharCode.apply(String, codePoints) // avoid extra slice()
    }

    // Decode in chunks to avoid "call stack size exceeded".
    let res = '';
    let i = 0;
    while (i < len) {
      res += String.fromCharCode.apply(
        String,
        codePoints.slice(i, i += MAX_ARGUMENTS_LENGTH)
      );
    }
    return res
  }

  function asciiSlice (buf, start, end) {
    let ret = '';
    end = Math.min(buf.length, end);

    for (let i = start; i < end; ++i) {
      ret += String.fromCharCode(buf[i] & 0x7F);
    }
    return ret
  }

  function latin1Slice (buf, start, end) {
    let ret = '';
    end = Math.min(buf.length, end);

    for (let i = start; i < end; ++i) {
      ret += String.fromCharCode(buf[i]);
    }
    return ret
  }

  function hexSlice (buf, start, end) {
    const len = buf.length;

    if (!start || start < 0) start = 0;
    if (!end || end < 0 || end > len) end = len;

    let out = '';
    for (let i = start; i < end; ++i) {
      out += hexSliceLookupTable[buf[i]];
    }
    return out
  }

  function utf16leSlice (buf, start, end) {
    const bytes = buf.slice(start, end);
    let res = '';
    // If bytes.length is odd, the last 8 bits must be ignored (same as node.js)
    for (let i = 0; i < bytes.length - 1; i += 2) {
      res += String.fromCharCode(bytes[i] + (bytes[i + 1] * 256));
    }
    return res
  }

  Buffer.prototype.slice = function slice (start, end) {
    const len = this.length;
    start = ~~start;
    end = end === undefined ? len : ~~end;

    if (start < 0) {
      start += len;
      if (start < 0) start = 0;
    } else if (start > len) {
      start = len;
    }

    if (end < 0) {
      end += len;
      if (end < 0) end = 0;
    } else if (end > len) {
      end = len;
    }

    if (end < start) end = start;

    const newBuf = this.subarray(start, end);
    // Return an augmented `Uint8Array` instance
    Object.setPrototypeOf(newBuf, Buffer.prototype);

    return newBuf
  };

  /*
   * Need to make sure that buffer isn't trying to write out of bounds.
   */
  function checkOffset (offset, ext, length) {
    if ((offset % 1) !== 0 || offset < 0) throw new RangeError('offset is not uint')
    if (offset + ext > length) throw new RangeError('Trying to access beyond buffer length')
  }

  Buffer.prototype.readUintLE =
  Buffer.prototype.readUIntLE = function readUIntLE (offset, byteLength, noAssert) {
    offset = offset >>> 0;
    byteLength = byteLength >>> 0;
    if (!noAssert) checkOffset(offset, byteLength, this.length);

    let val = this[offset];
    let mul = 1;
    let i = 0;
    while (++i < byteLength && (mul *= 0x100)) {
      val += this[offset + i] * mul;
    }

    return val
  };

  Buffer.prototype.readUintBE =
  Buffer.prototype.readUIntBE = function readUIntBE (offset, byteLength, noAssert) {
    offset = offset >>> 0;
    byteLength = byteLength >>> 0;
    if (!noAssert) {
      checkOffset(offset, byteLength, this.length);
    }

    let val = this[offset + --byteLength];
    let mul = 1;
    while (byteLength > 0 && (mul *= 0x100)) {
      val += this[offset + --byteLength] * mul;
    }

    return val
  };

  Buffer.prototype.readUint8 =
  Buffer.prototype.readUInt8 = function readUInt8 (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 1, this.length);
    return this[offset]
  };

  Buffer.prototype.readUint16LE =
  Buffer.prototype.readUInt16LE = function readUInt16LE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 2, this.length);
    return this[offset] | (this[offset + 1] << 8)
  };

  Buffer.prototype.readUint16BE =
  Buffer.prototype.readUInt16BE = function readUInt16BE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 2, this.length);
    return (this[offset] << 8) | this[offset + 1]
  };

  Buffer.prototype.readUint32LE =
  Buffer.prototype.readUInt32LE = function readUInt32LE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 4, this.length);

    return ((this[offset]) |
        (this[offset + 1] << 8) |
        (this[offset + 2] << 16)) +
        (this[offset + 3] * 0x1000000)
  };

  Buffer.prototype.readUint32BE =
  Buffer.prototype.readUInt32BE = function readUInt32BE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 4, this.length);

    return (this[offset] * 0x1000000) +
      ((this[offset + 1] << 16) |
      (this[offset + 2] << 8) |
      this[offset + 3])
  };

  Buffer.prototype.readBigUInt64LE = defineBigIntMethod(function readBigUInt64LE (offset) {
    offset = offset >>> 0;
    validateNumber(offset, 'offset');
    const first = this[offset];
    const last = this[offset + 7];
    if (first === undefined || last === undefined) {
      boundsError(offset, this.length - 8);
    }

    const lo = first +
      this[++offset] * 2 ** 8 +
      this[++offset] * 2 ** 16 +
      this[++offset] * 2 ** 24;

    const hi = this[++offset] +
      this[++offset] * 2 ** 8 +
      this[++offset] * 2 ** 16 +
      last * 2 ** 24;

    return BigInt(lo) + (BigInt(hi) << BigInt(32))
  });

  Buffer.prototype.readBigUInt64BE = defineBigIntMethod(function readBigUInt64BE (offset) {
    offset = offset >>> 0;
    validateNumber(offset, 'offset');
    const first = this[offset];
    const last = this[offset + 7];
    if (first === undefined || last === undefined) {
      boundsError(offset, this.length - 8);
    }

    const hi = first * 2 ** 24 +
      this[++offset] * 2 ** 16 +
      this[++offset] * 2 ** 8 +
      this[++offset];

    const lo = this[++offset] * 2 ** 24 +
      this[++offset] * 2 ** 16 +
      this[++offset] * 2 ** 8 +
      last;

    return (BigInt(hi) << BigInt(32)) + BigInt(lo)
  });

  Buffer.prototype.readIntLE = function readIntLE (offset, byteLength, noAssert) {
    offset = offset >>> 0;
    byteLength = byteLength >>> 0;
    if (!noAssert) checkOffset(offset, byteLength, this.length);

    let val = this[offset];
    let mul = 1;
    let i = 0;
    while (++i < byteLength && (mul *= 0x100)) {
      val += this[offset + i] * mul;
    }
    mul *= 0x80;

    if (val >= mul) val -= Math.pow(2, 8 * byteLength);

    return val
  };

  Buffer.prototype.readIntBE = function readIntBE (offset, byteLength, noAssert) {
    offset = offset >>> 0;
    byteLength = byteLength >>> 0;
    if (!noAssert) checkOffset(offset, byteLength, this.length);

    let i = byteLength;
    let mul = 1;
    let val = this[offset + --i];
    while (i > 0 && (mul *= 0x100)) {
      val += this[offset + --i] * mul;
    }
    mul *= 0x80;

    if (val >= mul) val -= Math.pow(2, 8 * byteLength);

    return val
  };

  Buffer.prototype.readInt8 = function readInt8 (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 1, this.length);
    if (!(this[offset] & 0x80)) return (this[offset])
    return ((0xff - this[offset] + 1) * -1)
  };

  Buffer.prototype.readInt16LE = function readInt16LE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 2, this.length);
    const val = this[offset] | (this[offset + 1] << 8);
    return (val & 0x8000) ? val | 0xFFFF0000 : val
  };

  Buffer.prototype.readInt16BE = function readInt16BE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 2, this.length);
    const val = this[offset + 1] | (this[offset] << 8);
    return (val & 0x8000) ? val | 0xFFFF0000 : val
  };

  Buffer.prototype.readInt32LE = function readInt32LE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 4, this.length);

    return (this[offset]) |
      (this[offset + 1] << 8) |
      (this[offset + 2] << 16) |
      (this[offset + 3] << 24)
  };

  Buffer.prototype.readInt32BE = function readInt32BE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 4, this.length);

    return (this[offset] << 24) |
      (this[offset + 1] << 16) |
      (this[offset + 2] << 8) |
      (this[offset + 3])
  };

  Buffer.prototype.readBigInt64LE = defineBigIntMethod(function readBigInt64LE (offset) {
    offset = offset >>> 0;
    validateNumber(offset, 'offset');
    const first = this[offset];
    const last = this[offset + 7];
    if (first === undefined || last === undefined) {
      boundsError(offset, this.length - 8);
    }

    const val = this[offset + 4] +
      this[offset + 5] * 2 ** 8 +
      this[offset + 6] * 2 ** 16 +
      (last << 24); // Overflow

    return (BigInt(val) << BigInt(32)) +
      BigInt(first +
      this[++offset] * 2 ** 8 +
      this[++offset] * 2 ** 16 +
      this[++offset] * 2 ** 24)
  });

  Buffer.prototype.readBigInt64BE = defineBigIntMethod(function readBigInt64BE (offset) {
    offset = offset >>> 0;
    validateNumber(offset, 'offset');
    const first = this[offset];
    const last = this[offset + 7];
    if (first === undefined || last === undefined) {
      boundsError(offset, this.length - 8);
    }

    const val = (first << 24) + // Overflow
      this[++offset] * 2 ** 16 +
      this[++offset] * 2 ** 8 +
      this[++offset];

    return (BigInt(val) << BigInt(32)) +
      BigInt(this[++offset] * 2 ** 24 +
      this[++offset] * 2 ** 16 +
      this[++offset] * 2 ** 8 +
      last)
  });

  Buffer.prototype.readFloatLE = function readFloatLE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 4, this.length);
    return ieee754.read(this, offset, true, 23, 4)
  };

  Buffer.prototype.readFloatBE = function readFloatBE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 4, this.length);
    return ieee754.read(this, offset, false, 23, 4)
  };

  Buffer.prototype.readDoubleLE = function readDoubleLE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 8, this.length);
    return ieee754.read(this, offset, true, 52, 8)
  };

  Buffer.prototype.readDoubleBE = function readDoubleBE (offset, noAssert) {
    offset = offset >>> 0;
    if (!noAssert) checkOffset(offset, 8, this.length);
    return ieee754.read(this, offset, false, 52, 8)
  };

  function checkInt (buf, value, offset, ext, max, min) {
    if (!Buffer.isBuffer(buf)) throw new TypeError('"buffer" argument must be a Buffer instance')
    if (value > max || value < min) throw new RangeError('"value" argument is out of bounds')
    if (offset + ext > buf.length) throw new RangeError('Index out of range')
  }

  Buffer.prototype.writeUintLE =
  Buffer.prototype.writeUIntLE = function writeUIntLE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset >>> 0;
    byteLength = byteLength >>> 0;
    if (!noAssert) {
      const maxBytes = Math.pow(2, 8 * byteLength) - 1;
      checkInt(this, value, offset, byteLength, maxBytes, 0);
    }

    let mul = 1;
    let i = 0;
    this[offset] = value & 0xFF;
    while (++i < byteLength && (mul *= 0x100)) {
      this[offset + i] = (value / mul) & 0xFF;
    }

    return offset + byteLength
  };

  Buffer.prototype.writeUintBE =
  Buffer.prototype.writeUIntBE = function writeUIntBE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset >>> 0;
    byteLength = byteLength >>> 0;
    if (!noAssert) {
      const maxBytes = Math.pow(2, 8 * byteLength) - 1;
      checkInt(this, value, offset, byteLength, maxBytes, 0);
    }

    let i = byteLength - 1;
    let mul = 1;
    this[offset + i] = value & 0xFF;
    while (--i >= 0 && (mul *= 0x100)) {
      this[offset + i] = (value / mul) & 0xFF;
    }

    return offset + byteLength
  };

  Buffer.prototype.writeUint8 =
  Buffer.prototype.writeUInt8 = function writeUInt8 (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 1, 0xff, 0);
    this[offset] = (value & 0xff);
    return offset + 1
  };

  Buffer.prototype.writeUint16LE =
  Buffer.prototype.writeUInt16LE = function writeUInt16LE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0);
    this[offset] = (value & 0xff);
    this[offset + 1] = (value >>> 8);
    return offset + 2
  };

  Buffer.prototype.writeUint16BE =
  Buffer.prototype.writeUInt16BE = function writeUInt16BE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0);
    this[offset] = (value >>> 8);
    this[offset + 1] = (value & 0xff);
    return offset + 2
  };

  Buffer.prototype.writeUint32LE =
  Buffer.prototype.writeUInt32LE = function writeUInt32LE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0);
    this[offset + 3] = (value >>> 24);
    this[offset + 2] = (value >>> 16);
    this[offset + 1] = (value >>> 8);
    this[offset] = (value & 0xff);
    return offset + 4
  };

  Buffer.prototype.writeUint32BE =
  Buffer.prototype.writeUInt32BE = function writeUInt32BE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0);
    this[offset] = (value >>> 24);
    this[offset + 1] = (value >>> 16);
    this[offset + 2] = (value >>> 8);
    this[offset + 3] = (value & 0xff);
    return offset + 4
  };

  function wrtBigUInt64LE (buf, value, offset, min, max) {
    checkIntBI(value, min, max, buf, offset, 7);

    let lo = Number(value & BigInt(0xffffffff));
    buf[offset++] = lo;
    lo = lo >> 8;
    buf[offset++] = lo;
    lo = lo >> 8;
    buf[offset++] = lo;
    lo = lo >> 8;
    buf[offset++] = lo;
    let hi = Number(value >> BigInt(32) & BigInt(0xffffffff));
    buf[offset++] = hi;
    hi = hi >> 8;
    buf[offset++] = hi;
    hi = hi >> 8;
    buf[offset++] = hi;
    hi = hi >> 8;
    buf[offset++] = hi;
    return offset
  }

  function wrtBigUInt64BE (buf, value, offset, min, max) {
    checkIntBI(value, min, max, buf, offset, 7);

    let lo = Number(value & BigInt(0xffffffff));
    buf[offset + 7] = lo;
    lo = lo >> 8;
    buf[offset + 6] = lo;
    lo = lo >> 8;
    buf[offset + 5] = lo;
    lo = lo >> 8;
    buf[offset + 4] = lo;
    let hi = Number(value >> BigInt(32) & BigInt(0xffffffff));
    buf[offset + 3] = hi;
    hi = hi >> 8;
    buf[offset + 2] = hi;
    hi = hi >> 8;
    buf[offset + 1] = hi;
    hi = hi >> 8;
    buf[offset] = hi;
    return offset + 8
  }

  Buffer.prototype.writeBigUInt64LE = defineBigIntMethod(function writeBigUInt64LE (value, offset = 0) {
    return wrtBigUInt64LE(this, value, offset, BigInt(0), BigInt('0xffffffffffffffff'))
  });

  Buffer.prototype.writeBigUInt64BE = defineBigIntMethod(function writeBigUInt64BE (value, offset = 0) {
    return wrtBigUInt64BE(this, value, offset, BigInt(0), BigInt('0xffffffffffffffff'))
  });

  Buffer.prototype.writeIntLE = function writeIntLE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) {
      const limit = Math.pow(2, (8 * byteLength) - 1);

      checkInt(this, value, offset, byteLength, limit - 1, -limit);
    }

    let i = 0;
    let mul = 1;
    let sub = 0;
    this[offset] = value & 0xFF;
    while (++i < byteLength && (mul *= 0x100)) {
      if (value < 0 && sub === 0 && this[offset + i - 1] !== 0) {
        sub = 1;
      }
      this[offset + i] = ((value / mul) >> 0) - sub & 0xFF;
    }

    return offset + byteLength
  };

  Buffer.prototype.writeIntBE = function writeIntBE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) {
      const limit = Math.pow(2, (8 * byteLength) - 1);

      checkInt(this, value, offset, byteLength, limit - 1, -limit);
    }

    let i = byteLength - 1;
    let mul = 1;
    let sub = 0;
    this[offset + i] = value & 0xFF;
    while (--i >= 0 && (mul *= 0x100)) {
      if (value < 0 && sub === 0 && this[offset + i + 1] !== 0) {
        sub = 1;
      }
      this[offset + i] = ((value / mul) >> 0) - sub & 0xFF;
    }

    return offset + byteLength
  };

  Buffer.prototype.writeInt8 = function writeInt8 (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 1, 0x7f, -0x80);
    if (value < 0) value = 0xff + value + 1;
    this[offset] = (value & 0xff);
    return offset + 1
  };

  Buffer.prototype.writeInt16LE = function writeInt16LE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000);
    this[offset] = (value & 0xff);
    this[offset + 1] = (value >>> 8);
    return offset + 2
  };

  Buffer.prototype.writeInt16BE = function writeInt16BE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000);
    this[offset] = (value >>> 8);
    this[offset + 1] = (value & 0xff);
    return offset + 2
  };

  Buffer.prototype.writeInt32LE = function writeInt32LE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000);
    this[offset] = (value & 0xff);
    this[offset + 1] = (value >>> 8);
    this[offset + 2] = (value >>> 16);
    this[offset + 3] = (value >>> 24);
    return offset + 4
  };

  Buffer.prototype.writeInt32BE = function writeInt32BE (value, offset, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000);
    if (value < 0) value = 0xffffffff + value + 1;
    this[offset] = (value >>> 24);
    this[offset + 1] = (value >>> 16);
    this[offset + 2] = (value >>> 8);
    this[offset + 3] = (value & 0xff);
    return offset + 4
  };

  Buffer.prototype.writeBigInt64LE = defineBigIntMethod(function writeBigInt64LE (value, offset = 0) {
    return wrtBigUInt64LE(this, value, offset, -BigInt('0x8000000000000000'), BigInt('0x7fffffffffffffff'))
  });

  Buffer.prototype.writeBigInt64BE = defineBigIntMethod(function writeBigInt64BE (value, offset = 0) {
    return wrtBigUInt64BE(this, value, offset, -BigInt('0x8000000000000000'), BigInt('0x7fffffffffffffff'))
  });

  function checkIEEE754 (buf, value, offset, ext, max, min) {
    if (offset + ext > buf.length) throw new RangeError('Index out of range')
    if (offset < 0) throw new RangeError('Index out of range')
  }

  function writeFloat (buf, value, offset, littleEndian, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) {
      checkIEEE754(buf, value, offset, 4);
    }
    ieee754.write(buf, value, offset, littleEndian, 23, 4);
    return offset + 4
  }

  Buffer.prototype.writeFloatLE = function writeFloatLE (value, offset, noAssert) {
    return writeFloat(this, value, offset, true, noAssert)
  };

  Buffer.prototype.writeFloatBE = function writeFloatBE (value, offset, noAssert) {
    return writeFloat(this, value, offset, false, noAssert)
  };

  function writeDouble (buf, value, offset, littleEndian, noAssert) {
    value = +value;
    offset = offset >>> 0;
    if (!noAssert) {
      checkIEEE754(buf, value, offset, 8);
    }
    ieee754.write(buf, value, offset, littleEndian, 52, 8);
    return offset + 8
  }

  Buffer.prototype.writeDoubleLE = function writeDoubleLE (value, offset, noAssert) {
    return writeDouble(this, value, offset, true, noAssert)
  };

  Buffer.prototype.writeDoubleBE = function writeDoubleBE (value, offset, noAssert) {
    return writeDouble(this, value, offset, false, noAssert)
  };

  // copy(targetBuffer, targetStart=0, sourceStart=0, sourceEnd=buffer.length)
  Buffer.prototype.copy = function copy (target, targetStart, start, end) {
    if (!Buffer.isBuffer(target)) throw new TypeError('argument should be a Buffer')
    if (!start) start = 0;
    if (!end && end !== 0) end = this.length;
    if (targetStart >= target.length) targetStart = target.length;
    if (!targetStart) targetStart = 0;
    if (end > 0 && end < start) end = start;

    // Copy 0 bytes; we're done
    if (end === start) return 0
    if (target.length === 0 || this.length === 0) return 0

    // Fatal error conditions
    if (targetStart < 0) {
      throw new RangeError('targetStart out of bounds')
    }
    if (start < 0 || start >= this.length) throw new RangeError('Index out of range')
    if (end < 0) throw new RangeError('sourceEnd out of bounds')

    // Are we oob?
    if (end > this.length) end = this.length;
    if (target.length - targetStart < end - start) {
      end = target.length - targetStart + start;
    }

    const len = end - start;

    if (this === target && typeof Uint8Array.prototype.copyWithin === 'function') {
      // Use built-in when available, missing from IE11
      this.copyWithin(targetStart, start, end);
    } else {
      Uint8Array.prototype.set.call(
        target,
        this.subarray(start, end),
        targetStart
      );
    }

    return len
  };

  // Usage:
  //    buffer.fill(number[, offset[, end]])
  //    buffer.fill(buffer[, offset[, end]])
  //    buffer.fill(string[, offset[, end]][, encoding])
  Buffer.prototype.fill = function fill (val, start, end, encoding) {
    // Handle string cases:
    if (typeof val === 'string') {
      if (typeof start === 'string') {
        encoding = start;
        start = 0;
        end = this.length;
      } else if (typeof end === 'string') {
        encoding = end;
        end = this.length;
      }
      if (encoding !== undefined && typeof encoding !== 'string') {
        throw new TypeError('encoding must be a string')
      }
      if (typeof encoding === 'string' && !Buffer.isEncoding(encoding)) {
        throw new TypeError('Unknown encoding: ' + encoding)
      }
      if (val.length === 1) {
        const code = val.charCodeAt(0);
        if ((encoding === 'utf8' && code < 128) ||
            encoding === 'latin1') {
          // Fast path: If `val` fits into a single byte, use that numeric value.
          val = code;
        }
      }
    } else if (typeof val === 'number') {
      val = val & 255;
    } else if (typeof val === 'boolean') {
      val = Number(val);
    }

    // Invalid ranges are not set to a default, so can range check early.
    if (start < 0 || this.length < start || this.length < end) {
      throw new RangeError('Out of range index')
    }

    if (end <= start) {
      return this
    }

    start = start >>> 0;
    end = end === undefined ? this.length : end >>> 0;

    if (!val) val = 0;

    let i;
    if (typeof val === 'number') {
      for (i = start; i < end; ++i) {
        this[i] = val;
      }
    } else {
      const bytes = Buffer.isBuffer(val)
        ? val
        : Buffer.from(val, encoding);
      const len = bytes.length;
      if (len === 0) {
        throw new TypeError('The value "' + val +
          '" is invalid for argument "value"')
      }
      for (i = 0; i < end - start; ++i) {
        this[i + start] = bytes[i % len];
      }
    }

    return this
  };

  // CUSTOM ERRORS
  // =============

  // Simplified versions from Node, changed for Buffer-only usage
  const errors = {};
  function E (sym, getMessage, Base) {
    errors[sym] = class NodeError extends Base {
      constructor () {
        super();

        Object.defineProperty(this, 'message', {
          value: getMessage.apply(this, arguments),
          writable: true,
          configurable: true
        });

        // Add the error code to the name to include it in the stack trace.
        this.name = `${this.name} [${sym}]`;
        // Access the stack to generate the error message including the error code
        // from the name.
        this.stack; // eslint-disable-line no-unused-expressions
        // Reset the name to the actual name.
        delete this.name;
      }

      get code () {
        return sym
      }

      set code (value) {
        Object.defineProperty(this, 'code', {
          configurable: true,
          enumerable: true,
          value,
          writable: true
        });
      }

      toString () {
        return `${this.name} [${sym}]: ${this.message}`
      }
    };
  }

  E('ERR_BUFFER_OUT_OF_BOUNDS',
    function (name) {
      if (name) {
        return `${name} is outside of buffer bounds`
      }

      return 'Attempt to access memory outside buffer bounds'
    }, RangeError);
  E('ERR_INVALID_ARG_TYPE',
    function (name, actual) {
      return `The "${name}" argument must be of type number. Received type ${typeof actual}`
    }, TypeError);
  E('ERR_OUT_OF_RANGE',
    function (str, range, input) {
      let msg = `The value of "${str}" is out of range.`;
      let received = input;
      if (Number.isInteger(input) && Math.abs(input) > 2 ** 32) {
        received = addNumericalSeparator(String(input));
      } else if (typeof input === 'bigint') {
        received = String(input);
        if (input > BigInt(2) ** BigInt(32) || input < -(BigInt(2) ** BigInt(32))) {
          received = addNumericalSeparator(received);
        }
        received += 'n';
      }
      msg += ` It must be ${range}. Received ${received}`;
      return msg
    }, RangeError);

  function addNumericalSeparator (val) {
    let res = '';
    let i = val.length;
    const start = val[0] === '-' ? 1 : 0;
    for (; i >= start + 4; i -= 3) {
      res = `_${val.slice(i - 3, i)}${res}`;
    }
    return `${val.slice(0, i)}${res}`
  }

  // CHECK FUNCTIONS
  // ===============

  function checkBounds (buf, offset, byteLength) {
    validateNumber(offset, 'offset');
    if (buf[offset] === undefined || buf[offset + byteLength] === undefined) {
      boundsError(offset, buf.length - (byteLength + 1));
    }
  }

  function checkIntBI (value, min, max, buf, offset, byteLength) {
    if (value > max || value < min) {
      const n = typeof min === 'bigint' ? 'n' : '';
      let range;
      if (byteLength > 3) {
        if (min === 0 || min === BigInt(0)) {
          range = `>= 0${n} and < 2${n} ** ${(byteLength + 1) * 8}${n}`;
        } else {
          range = `>= -(2${n} ** ${(byteLength + 1) * 8 - 1}${n}) and < 2 ** ` +
                  `${(byteLength + 1) * 8 - 1}${n}`;
        }
      } else {
        range = `>= ${min}${n} and <= ${max}${n}`;
      }
      throw new errors.ERR_OUT_OF_RANGE('value', range, value)
    }
    checkBounds(buf, offset, byteLength);
  }

  function validateNumber (value, name) {
    if (typeof value !== 'number') {
      throw new errors.ERR_INVALID_ARG_TYPE(name, 'number', value)
    }
  }

  function boundsError (value, length, type) {
    if (Math.floor(value) !== value) {
      validateNumber(value, type);
      throw new errors.ERR_OUT_OF_RANGE(type || 'offset', 'an integer', value)
    }

    if (length < 0) {
      throw new errors.ERR_BUFFER_OUT_OF_BOUNDS()
    }

    throw new errors.ERR_OUT_OF_RANGE(type || 'offset',
                                      `>= ${type ? 1 : 0} and <= ${length}`,
                                      value)
  }

  // HELPER FUNCTIONS
  // ================

  const INVALID_BASE64_RE = /[^+/0-9A-Za-z-_]/g;

  function base64clean (str) {
    // Node takes equal signs as end of the Base64 encoding
    str = str.split('=')[0];
    // Node strips out invalid characters like \n and \t from the string, base64-js does not
    str = str.trim().replace(INVALID_BASE64_RE, '');
    // Node converts strings with length < 2 to ''
    if (str.length < 2) return ''
    // Node allows for non-padded base64 strings (missing trailing ===), base64-js does not
    while (str.length % 4 !== 0) {
      str = str + '=';
    }
    return str
  }

  function utf8ToBytes (string, units) {
    units = units || Infinity;
    let codePoint;
    const length = string.length;
    let leadSurrogate = null;
    const bytes = [];

    for (let i = 0; i < length; ++i) {
      codePoint = string.charCodeAt(i);

      // is surrogate component
      if (codePoint > 0xD7FF && codePoint < 0xE000) {
        // last char was a lead
        if (!leadSurrogate) {
          // no lead yet
          if (codePoint > 0xDBFF) {
            // unexpected trail
            if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
            continue
          } else if (i + 1 === length) {
            // unpaired lead
            if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
            continue
          }

          // valid lead
          leadSurrogate = codePoint;

          continue
        }

        // 2 leads in a row
        if (codePoint < 0xDC00) {
          if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
          leadSurrogate = codePoint;
          continue
        }

        // valid surrogate pair
        codePoint = (leadSurrogate - 0xD800 << 10 | codePoint - 0xDC00) + 0x10000;
      } else if (leadSurrogate) {
        // valid bmp char, but last char was a lead
        if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
      }

      leadSurrogate = null;

      // encode utf8
      if (codePoint < 0x80) {
        if ((units -= 1) < 0) break
        bytes.push(codePoint);
      } else if (codePoint < 0x800) {
        if ((units -= 2) < 0) break
        bytes.push(
          codePoint >> 0x6 | 0xC0,
          codePoint & 0x3F | 0x80
        );
      } else if (codePoint < 0x10000) {
        if ((units -= 3) < 0) break
        bytes.push(
          codePoint >> 0xC | 0xE0,
          codePoint >> 0x6 & 0x3F | 0x80,
          codePoint & 0x3F | 0x80
        );
      } else if (codePoint < 0x110000) {
        if ((units -= 4) < 0) break
        bytes.push(
          codePoint >> 0x12 | 0xF0,
          codePoint >> 0xC & 0x3F | 0x80,
          codePoint >> 0x6 & 0x3F | 0x80,
          codePoint & 0x3F | 0x80
        );
      } else {
        throw new Error('Invalid code point')
      }
    }

    return bytes
  }

  function asciiToBytes (str) {
    const byteArray = [];
    for (let i = 0; i < str.length; ++i) {
      // Node's code seems to be doing this and not & 0x7F..
      byteArray.push(str.charCodeAt(i) & 0xFF);
    }
    return byteArray
  }

  function utf16leToBytes (str, units) {
    let c, hi, lo;
    const byteArray = [];
    for (let i = 0; i < str.length; ++i) {
      if ((units -= 2) < 0) break

      c = str.charCodeAt(i);
      hi = c >> 8;
      lo = c % 256;
      byteArray.push(lo);
      byteArray.push(hi);
    }

    return byteArray
  }

  function base64ToBytes (str) {
    return base64Js.toByteArray(base64clean(str))
  }

  function blitBuffer (src, dst, offset, length) {
    let i;
    for (i = 0; i < length; ++i) {
      if ((i + offset >= dst.length) || (i >= src.length)) break
      dst[i + offset] = src[i];
    }
    return i
  }

  // ArrayBuffer or Uint8Array objects from other contexts (i.e. iframes) do not pass
  // the `instanceof` check but they should be treated as of that type.
  // See: https://github.com/feross/buffer/issues/166
  function isInstance (obj, type) {
    return obj instanceof type ||
      (obj != null && obj.constructor != null && obj.constructor.name != null &&
        obj.constructor.name === type.name)
  }
  function numberIsNaN (obj) {
    // For IE11 support
    return obj !== obj // eslint-disable-line no-self-compare
  }

  // Create lookup table for `toString('hex')`
  // See: https://github.com/feross/buffer/issues/219
  const hexSliceLookupTable = (function () {
    const alphabet = '0123456789abcdef';
    const table = new Array(256);
    for (let i = 0; i < 16; ++i) {
      const i16 = i * 16;
      for (let j = 0; j < 16; ++j) {
        table[i16 + j] = alphabet[i] + alphabet[j];
      }
    }
    return table
  })();

  // Return not function with Error if BigInt not supported
  function defineBigIntMethod (fn) {
    return typeof BigInt === 'undefined' ? BufferBigIntNotDefined : fn
  }

  function BufferBigIntNotDefined () {
    throw new Error('BigInt not supported')
  }
  });

  var _nodeResolve_empty = {};

  var _nodeResolve_empty$1 = /*#__PURE__*/Object.freeze({
    __proto__: null,
    'default': _nodeResolve_empty
  });

  var debugUtil = /*@__PURE__*/getAugmentedNamespace(_nodeResolve_empty$1);

  function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

  function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty$1(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

  function _defineProperty$1(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

  function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

  function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

  function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

  var Buffer$4 = buffer.Buffer;

  var inspect = debugUtil.inspect;

  var custom = inspect && inspect.custom || 'inspect';

  function copyBuffer(src, target, offset) {
    Buffer$4.prototype.copy.call(src, target, offset);
  }

  var buffer_list =
  /*#__PURE__*/
  function () {
    function BufferList() {
      _classCallCheck(this, BufferList);

      this.head = null;
      this.tail = null;
      this.length = 0;
    }

    _createClass(BufferList, [{
      key: "push",
      value: function push(v) {
        var entry = {
          data: v,
          next: null
        };
        if (this.length > 0) this.tail.next = entry;else this.head = entry;
        this.tail = entry;
        ++this.length;
      }
    }, {
      key: "unshift",
      value: function unshift(v) {
        var entry = {
          data: v,
          next: this.head
        };
        if (this.length === 0) this.tail = entry;
        this.head = entry;
        ++this.length;
      }
    }, {
      key: "shift",
      value: function shift() {
        if (this.length === 0) return;
        var ret = this.head.data;
        if (this.length === 1) this.head = this.tail = null;else this.head = this.head.next;
        --this.length;
        return ret;
      }
    }, {
      key: "clear",
      value: function clear() {
        this.head = this.tail = null;
        this.length = 0;
      }
    }, {
      key: "join",
      value: function join(s) {
        if (this.length === 0) return '';
        var p = this.head;
        var ret = '' + p.data;

        while (p = p.next) {
          ret += s + p.data;
        }

        return ret;
      }
    }, {
      key: "concat",
      value: function concat(n) {
        if (this.length === 0) return Buffer$4.alloc(0);
        var ret = Buffer$4.allocUnsafe(n >>> 0);
        var p = this.head;
        var i = 0;

        while (p) {
          copyBuffer(p.data, ret, i);
          i += p.data.length;
          p = p.next;
        }

        return ret;
      } // Consumes a specified amount of bytes or characters from the buffered data.

    }, {
      key: "consume",
      value: function consume(n, hasStrings) {
        var ret;

        if (n < this.head.data.length) {
          // `slice` is the same for buffers and strings.
          ret = this.head.data.slice(0, n);
          this.head.data = this.head.data.slice(n);
        } else if (n === this.head.data.length) {
          // First chunk is a perfect match.
          ret = this.shift();
        } else {
          // Result spans more than one buffer.
          ret = hasStrings ? this._getString(n) : this._getBuffer(n);
        }

        return ret;
      }
    }, {
      key: "first",
      value: function first() {
        return this.head.data;
      } // Consumes a specified amount of characters from the buffered data.

    }, {
      key: "_getString",
      value: function _getString(n) {
        var p = this.head;
        var c = 1;
        var ret = p.data;
        n -= ret.length;

        while (p = p.next) {
          var str = p.data;
          var nb = n > str.length ? str.length : n;
          if (nb === str.length) ret += str;else ret += str.slice(0, n);
          n -= nb;

          if (n === 0) {
            if (nb === str.length) {
              ++c;
              if (p.next) this.head = p.next;else this.head = this.tail = null;
            } else {
              this.head = p;
              p.data = str.slice(nb);
            }

            break;
          }

          ++c;
        }

        this.length -= c;
        return ret;
      } // Consumes a specified amount of bytes from the buffered data.

    }, {
      key: "_getBuffer",
      value: function _getBuffer(n) {
        var ret = Buffer$4.allocUnsafe(n);
        var p = this.head;
        var c = 1;
        p.data.copy(ret);
        n -= p.data.length;

        while (p = p.next) {
          var buf = p.data;
          var nb = n > buf.length ? buf.length : n;
          buf.copy(ret, ret.length - n, 0, nb);
          n -= nb;

          if (n === 0) {
            if (nb === buf.length) {
              ++c;
              if (p.next) this.head = p.next;else this.head = this.tail = null;
            } else {
              this.head = p;
              p.data = buf.slice(nb);
            }

            break;
          }

          ++c;
        }

        this.length -= c;
        return ret;
      } // Make sure the linked list only shows the minimal necessary information.

    }, {
      key: custom,
      value: function value(_, options) {
        return inspect(this, _objectSpread({}, options, {
          // Only inspect one level.
          depth: 0,
          // It should not recurse.
          customInspect: false
        }));
      }
    }]);

    return BufferList;
  }();

  function destroy(err, cb) {
    var _this = this;

    var readableDestroyed = this._readableState && this._readableState.destroyed;
    var writableDestroyed = this._writableState && this._writableState.destroyed;

    if (readableDestroyed || writableDestroyed) {
      if (cb) {
        cb(err);
      } else if (err) {
        if (!this._writableState) {
          process.nextTick(emitErrorNT, this, err);
        } else if (!this._writableState.errorEmitted) {
          this._writableState.errorEmitted = true;
          process.nextTick(emitErrorNT, this, err);
        }
      }

      return this;
    } // we set destroyed to true before firing error callbacks in order
    // to make it re-entrance safe in case destroy() is called within callbacks


    if (this._readableState) {
      this._readableState.destroyed = true;
    } // if this is a duplex stream mark the writable part as destroyed as well


    if (this._writableState) {
      this._writableState.destroyed = true;
    }

    this._destroy(err || null, function (err) {
      if (!cb && err) {
        if (!_this._writableState) {
          process.nextTick(emitErrorAndCloseNT, _this, err);
        } else if (!_this._writableState.errorEmitted) {
          _this._writableState.errorEmitted = true;
          process.nextTick(emitErrorAndCloseNT, _this, err);
        } else {
          process.nextTick(emitCloseNT, _this);
        }
      } else if (cb) {
        process.nextTick(emitCloseNT, _this);
        cb(err);
      } else {
        process.nextTick(emitCloseNT, _this);
      }
    });

    return this;
  }

  function emitErrorAndCloseNT(self, err) {
    emitErrorNT(self, err);
    emitCloseNT(self);
  }

  function emitCloseNT(self) {
    if (self._writableState && !self._writableState.emitClose) return;
    if (self._readableState && !self._readableState.emitClose) return;
    self.emit('close');
  }

  function undestroy() {
    if (this._readableState) {
      this._readableState.destroyed = false;
      this._readableState.reading = false;
      this._readableState.ended = false;
      this._readableState.endEmitted = false;
    }

    if (this._writableState) {
      this._writableState.destroyed = false;
      this._writableState.ended = false;
      this._writableState.ending = false;
      this._writableState.finalCalled = false;
      this._writableState.prefinished = false;
      this._writableState.finished = false;
      this._writableState.errorEmitted = false;
    }
  }

  function emitErrorNT(self, err) {
    self.emit('error', err);
  }

  function errorOrDestroy$2(stream, err) {
    // We have tests that rely on errors being emitted
    // in the same tick, so changing this is semver major.
    // For now when you opt-in to autoDestroy we allow
    // the error to be emitted nextTick. In a future
    // semver major update we should change the default to this.
    var rState = stream._readableState;
    var wState = stream._writableState;
    if (rState && rState.autoDestroy || wState && wState.autoDestroy) stream.destroy(err);else stream.emit('error', err);
  }

  var destroy_1 = {
    destroy: destroy,
    undestroy: undestroy,
    errorOrDestroy: errorOrDestroy$2
  };

  function _inheritsLoose(subClass, superClass) { subClass.prototype = Object.create(superClass.prototype); subClass.prototype.constructor = subClass; subClass.__proto__ = superClass; }

  var codes = {};

  function createErrorType(code, message, Base) {
    if (!Base) {
      Base = Error;
    }

    function getMessage(arg1, arg2, arg3) {
      if (typeof message === 'string') {
        return message;
      } else {
        return message(arg1, arg2, arg3);
      }
    }

    var NodeError =
    /*#__PURE__*/
    function (_Base) {
      _inheritsLoose(NodeError, _Base);

      function NodeError(arg1, arg2, arg3) {
        return _Base.call(this, getMessage(arg1, arg2, arg3)) || this;
      }

      return NodeError;
    }(Base);

    NodeError.prototype.name = Base.name;
    NodeError.prototype.code = code;
    codes[code] = NodeError;
  } // https://github.com/nodejs/node/blob/v10.8.0/lib/internal/errors.js


  function oneOf(expected, thing) {
    if (Array.isArray(expected)) {
      var len = expected.length;
      expected = expected.map(function (i) {
        return String(i);
      });

      if (len > 2) {
        return "one of ".concat(thing, " ").concat(expected.slice(0, len - 1).join(', '), ", or ") + expected[len - 1];
      } else if (len === 2) {
        return "one of ".concat(thing, " ").concat(expected[0], " or ").concat(expected[1]);
      } else {
        return "of ".concat(thing, " ").concat(expected[0]);
      }
    } else {
      return "of ".concat(thing, " ").concat(String(expected));
    }
  } // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith


  function startsWith(str, search, pos) {
    return str.substr(!pos || pos < 0 ? 0 : +pos, search.length) === search;
  } // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/endsWith


  function endsWith(str, search, this_len) {
    if (this_len === undefined || this_len > str.length) {
      this_len = str.length;
    }

    return str.substring(this_len - search.length, this_len) === search;
  } // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/includes


  function includes(str, search, start) {
    if (typeof start !== 'number') {
      start = 0;
    }

    if (start + search.length > str.length) {
      return false;
    } else {
      return str.indexOf(search, start) !== -1;
    }
  }

  createErrorType('ERR_INVALID_OPT_VALUE', function (name, value) {
    return 'The value "' + value + '" is invalid for option "' + name + '"';
  }, TypeError);
  createErrorType('ERR_INVALID_ARG_TYPE', function (name, expected, actual) {
    // determiner: 'must be' or 'must not be'
    var determiner;

    if (typeof expected === 'string' && startsWith(expected, 'not ')) {
      determiner = 'must not be';
      expected = expected.replace(/^not /, '');
    } else {
      determiner = 'must be';
    }

    var msg;

    if (endsWith(name, ' argument')) {
      // For cases like 'first argument'
      msg = "The ".concat(name, " ").concat(determiner, " ").concat(oneOf(expected, 'type'));
    } else {
      var type = includes(name, '.') ? 'property' : 'argument';
      msg = "The \"".concat(name, "\" ").concat(type, " ").concat(determiner, " ").concat(oneOf(expected, 'type'));
    }

    msg += ". Received type ".concat(typeof actual);
    return msg;
  }, TypeError);
  createErrorType('ERR_STREAM_PUSH_AFTER_EOF', 'stream.push() after EOF');
  createErrorType('ERR_METHOD_NOT_IMPLEMENTED', function (name) {
    return 'The ' + name + ' method is not implemented';
  });
  createErrorType('ERR_STREAM_PREMATURE_CLOSE', 'Premature close');
  createErrorType('ERR_STREAM_DESTROYED', function (name) {
    return 'Cannot call ' + name + ' after a stream was destroyed';
  });
  createErrorType('ERR_MULTIPLE_CALLBACK', 'Callback called multiple times');
  createErrorType('ERR_STREAM_CANNOT_PIPE', 'Cannot pipe, not readable');
  createErrorType('ERR_STREAM_WRITE_AFTER_END', 'write after end');
  createErrorType('ERR_STREAM_NULL_VALUES', 'May not write null values to stream', TypeError);
  createErrorType('ERR_UNKNOWN_ENCODING', function (arg) {
    return 'Unknown encoding: ' + arg;
  }, TypeError);
  createErrorType('ERR_STREAM_UNSHIFT_AFTER_END_EVENT', 'stream.unshift() after end event');
  var codes_1 = codes;

  var errorsBrowser = {
  	codes: codes_1
  };

  var ERR_INVALID_OPT_VALUE = errorsBrowser.codes.ERR_INVALID_OPT_VALUE;

  function highWaterMarkFrom(options, isDuplex, duplexKey) {
    return options.highWaterMark != null ? options.highWaterMark : isDuplex ? options[duplexKey] : null;
  }

  function getHighWaterMark$2(state, options, duplexKey, isDuplex) {
    var hwm = highWaterMarkFrom(options, isDuplex, duplexKey);

    if (hwm != null) {
      if (!(isFinite(hwm) && Math.floor(hwm) === hwm) || hwm < 0) {
        var name = isDuplex ? duplexKey : 'highWaterMark';
        throw new ERR_INVALID_OPT_VALUE(name, hwm);
      }

      return Math.floor(hwm);
    } // Default value


    return state.objectMode ? 16 : 16 * 1024;
  }

  var state = {
    getHighWaterMark: getHighWaterMark$2
  };

  var inherits_browser = createCommonjsModule(function (module) {
  if (typeof Object.create === 'function') {
    // implementation from standard node.js 'util' module
    module.exports = function inherits(ctor, superCtor) {
      if (superCtor) {
        ctor.super_ = superCtor;
        ctor.prototype = Object.create(superCtor.prototype, {
          constructor: {
            value: ctor,
            enumerable: false,
            writable: true,
            configurable: true
          }
        });
      }
    };
  } else {
    // old school shim for old browsers
    module.exports = function inherits(ctor, superCtor) {
      if (superCtor) {
        ctor.super_ = superCtor;
        var TempCtor = function () {};
        TempCtor.prototype = superCtor.prototype;
        ctor.prototype = new TempCtor();
        ctor.prototype.constructor = ctor;
      }
    };
  }
  });

  /**
   * Module exports.
   */

  var browser = deprecate;

  /**
   * Mark that a method should not be used.
   * Returns a modified function which warns once by default.
   *
   * If `localStorage.noDeprecation = true` is set, then it is a no-op.
   *
   * If `localStorage.throwDeprecation = true` is set, then deprecated functions
   * will throw an Error when invoked.
   *
   * If `localStorage.traceDeprecation = true` is set, then deprecated functions
   * will invoke `console.trace()` instead of `console.error()`.
   *
   * @param {Function} fn - the function to deprecate
   * @param {String} msg - the string to print to the console when `fn` is invoked
   * @returns {Function} a new "deprecated" version of `fn`
   * @api public
   */

  function deprecate (fn, msg) {
    if (config('noDeprecation')) {
      return fn;
    }

    var warned = false;
    function deprecated() {
      if (!warned) {
        if (config('throwDeprecation')) {
          throw new Error(msg);
        } else if (config('traceDeprecation')) {
          console.trace(msg);
        } else {
          console.warn(msg);
        }
        warned = true;
      }
      return fn.apply(this, arguments);
    }

    return deprecated;
  }

  /**
   * Checks `localStorage` for boolean values for the given `name`.
   *
   * @param {String} name
   * @returns {Boolean}
   * @api private
   */

  function config (name) {
    // accessing global.localStorage can trigger a DOMException in sandboxed iframes
    try {
      if (!commonjsGlobal.localStorage) return false;
    } catch (_) {
      return false;
    }
    var val = commonjsGlobal.localStorage[name];
    if (null == val) return false;
    return String(val).toLowerCase() === 'true';
  }

  var require$$2 = _stream_duplex;

  var _stream_writable = Writable;
  // there will be only 2 of these for each stream


  function CorkedRequest(state) {
    var _this = this;

    this.next = null;
    this.entry = null;

    this.finish = function () {
      onCorkedFinish(_this, state);
    };
  }
  /* </replacement> */

  /*<replacement>*/


  var Duplex$2;
  /*</replacement>*/

  Writable.WritableState = WritableState;
  /*<replacement>*/

  var internalUtil = {
    deprecate: browser
  };
  /*</replacement>*/

  /*<replacement>*/


  /*</replacement>*/


  var Buffer$3 = buffer.Buffer;

  var OurUint8Array$1 = commonjsGlobal.Uint8Array || function () {};

  function _uint8ArrayToBuffer$1(chunk) {
    return Buffer$3.from(chunk);
  }

  function _isUint8Array$1(obj) {
    return Buffer$3.isBuffer(obj) || obj instanceof OurUint8Array$1;
  }



  var getHighWaterMark$1 = state.getHighWaterMark;

  var _require$codes$3 = errorsBrowser.codes,
      ERR_INVALID_ARG_TYPE$1 = _require$codes$3.ERR_INVALID_ARG_TYPE,
      ERR_METHOD_NOT_IMPLEMENTED$2 = _require$codes$3.ERR_METHOD_NOT_IMPLEMENTED,
      ERR_MULTIPLE_CALLBACK$1 = _require$codes$3.ERR_MULTIPLE_CALLBACK,
      ERR_STREAM_CANNOT_PIPE = _require$codes$3.ERR_STREAM_CANNOT_PIPE,
      ERR_STREAM_DESTROYED$1 = _require$codes$3.ERR_STREAM_DESTROYED,
      ERR_STREAM_NULL_VALUES = _require$codes$3.ERR_STREAM_NULL_VALUES,
      ERR_STREAM_WRITE_AFTER_END = _require$codes$3.ERR_STREAM_WRITE_AFTER_END,
      ERR_UNKNOWN_ENCODING = _require$codes$3.ERR_UNKNOWN_ENCODING;

  var errorOrDestroy$1 = destroy_1.errorOrDestroy;

  inherits_browser(Writable, streamBrowser);

  function nop() {}

  function WritableState(options, stream, isDuplex) {
    Duplex$2 = Duplex$2 || require$$2;
    options = options || {}; // Duplex streams are both readable and writable, but share
    // the same options object.
    // However, some cases require setting options to different
    // values for the readable and the writable sides of the duplex stream,
    // e.g. options.readableObjectMode vs. options.writableObjectMode, etc.

    if (typeof isDuplex !== 'boolean') isDuplex = stream instanceof Duplex$2; // object stream flag to indicate whether or not this stream
    // contains buffers or objects.

    this.objectMode = !!options.objectMode;
    if (isDuplex) this.objectMode = this.objectMode || !!options.writableObjectMode; // the point at which write() starts returning false
    // Note: 0 is a valid value, means that we always return false if
    // the entire buffer is not flushed immediately on write()

    this.highWaterMark = getHighWaterMark$1(this, options, 'writableHighWaterMark', isDuplex); // if _final has been called

    this.finalCalled = false; // drain event flag.

    this.needDrain = false; // at the start of calling end()

    this.ending = false; // when end() has been called, and returned

    this.ended = false; // when 'finish' is emitted

    this.finished = false; // has it been destroyed

    this.destroyed = false; // should we decode strings into buffers before passing to _write?
    // this is here so that some node-core streams can optimize string
    // handling at a lower level.

    var noDecode = options.decodeStrings === false;
    this.decodeStrings = !noDecode; // Crypto is kind of old and crusty.  Historically, its default string
    // encoding is 'binary' so we have to make this configurable.
    // Everything else in the universe uses 'utf8', though.

    this.defaultEncoding = options.defaultEncoding || 'utf8'; // not an actual buffer we keep track of, but a measurement
    // of how much we're waiting to get pushed to some underlying
    // socket or file.

    this.length = 0; // a flag to see when we're in the middle of a write.

    this.writing = false; // when true all writes will be buffered until .uncork() call

    this.corked = 0; // a flag to be able to tell if the onwrite cb is called immediately,
    // or on a later tick.  We set this to true at first, because any
    // actions that shouldn't happen until "later" should generally also
    // not happen before the first write call.

    this.sync = true; // a flag to know if we're processing previously buffered items, which
    // may call the _write() callback in the same tick, so that we don't
    // end up in an overlapped onwrite situation.

    this.bufferProcessing = false; // the callback that's passed to _write(chunk,cb)

    this.onwrite = function (er) {
      onwrite(stream, er);
    }; // the callback that the user supplies to write(chunk,encoding,cb)


    this.writecb = null; // the amount that is being written when _write is called.

    this.writelen = 0;
    this.bufferedRequest = null;
    this.lastBufferedRequest = null; // number of pending user-supplied write callbacks
    // this must be 0 before 'finish' can be emitted

    this.pendingcb = 0; // emit prefinish if the only thing we're waiting for is _write cbs
    // This is relevant for synchronous Transform streams

    this.prefinished = false; // True if the error was already emitted and should not be thrown again

    this.errorEmitted = false; // Should close be emitted on destroy. Defaults to true.

    this.emitClose = options.emitClose !== false; // Should .destroy() be called after 'finish' (and potentially 'end')

    this.autoDestroy = !!options.autoDestroy; // count buffered requests

    this.bufferedRequestCount = 0; // allocate the first CorkedRequest, there is always
    // one allocated and free to use, and we maintain at most two

    this.corkedRequestsFree = new CorkedRequest(this);
  }

  WritableState.prototype.getBuffer = function getBuffer() {
    var current = this.bufferedRequest;
    var out = [];

    while (current) {
      out.push(current);
      current = current.next;
    }

    return out;
  };

  (function () {
    try {
      Object.defineProperty(WritableState.prototype, 'buffer', {
        get: internalUtil.deprecate(function writableStateBufferGetter() {
          return this.getBuffer();
        }, '_writableState.buffer is deprecated. Use _writableState.getBuffer ' + 'instead.', 'DEP0003')
      });
    } catch (_) {}
  })(); // Test _writableState for inheritance to account for Duplex streams,
  // whose prototype chain only points to Readable.


  var realHasInstance;

  if (typeof Symbol === 'function' && Symbol.hasInstance && typeof Function.prototype[Symbol.hasInstance] === 'function') {
    realHasInstance = Function.prototype[Symbol.hasInstance];
    Object.defineProperty(Writable, Symbol.hasInstance, {
      value: function value(object) {
        if (realHasInstance.call(this, object)) return true;
        if (this !== Writable) return false;
        return object && object._writableState instanceof WritableState;
      }
    });
  } else {
    realHasInstance = function realHasInstance(object) {
      return object instanceof this;
    };
  }

  function Writable(options) {
    Duplex$2 = Duplex$2 || require$$2; // Writable ctor is applied to Duplexes, too.
    // `realHasInstance` is necessary because using plain `instanceof`
    // would return false, as no `_writableState` property is attached.
    // Trying to use the custom `instanceof` for Writable here will also break the
    // Node.js LazyTransform implementation, which has a non-trivial getter for
    // `_writableState` that would lead to infinite recursion.
    // Checking for a Stream.Duplex instance is faster here instead of inside
    // the WritableState constructor, at least with V8 6.5

    var isDuplex = this instanceof Duplex$2;
    if (!isDuplex && !realHasInstance.call(Writable, this)) return new Writable(options);
    this._writableState = new WritableState(options, this, isDuplex); // legacy.

    this.writable = true;

    if (options) {
      if (typeof options.write === 'function') this._write = options.write;
      if (typeof options.writev === 'function') this._writev = options.writev;
      if (typeof options.destroy === 'function') this._destroy = options.destroy;
      if (typeof options.final === 'function') this._final = options.final;
    }

    streamBrowser.call(this);
  } // Otherwise people can pipe Writable streams, which is just wrong.


  Writable.prototype.pipe = function () {
    errorOrDestroy$1(this, new ERR_STREAM_CANNOT_PIPE());
  };

  function writeAfterEnd(stream, cb) {
    var er = new ERR_STREAM_WRITE_AFTER_END(); // TODO: defer error events consistently everywhere, not just the cb

    errorOrDestroy$1(stream, er);
    process.nextTick(cb, er);
  } // Checks that a user-supplied chunk is valid, especially for the particular
  // mode the stream is in. Currently this means that `null` is never accepted
  // and undefined/non-string values are only allowed in object mode.


  function validChunk(stream, state, chunk, cb) {
    var er;

    if (chunk === null) {
      er = new ERR_STREAM_NULL_VALUES();
    } else if (typeof chunk !== 'string' && !state.objectMode) {
      er = new ERR_INVALID_ARG_TYPE$1('chunk', ['string', 'Buffer'], chunk);
    }

    if (er) {
      errorOrDestroy$1(stream, er);
      process.nextTick(cb, er);
      return false;
    }

    return true;
  }

  Writable.prototype.write = function (chunk, encoding, cb) {
    var state = this._writableState;
    var ret = false;

    var isBuf = !state.objectMode && _isUint8Array$1(chunk);

    if (isBuf && !Buffer$3.isBuffer(chunk)) {
      chunk = _uint8ArrayToBuffer$1(chunk);
    }

    if (typeof encoding === 'function') {
      cb = encoding;
      encoding = null;
    }

    if (isBuf) encoding = 'buffer';else if (!encoding) encoding = state.defaultEncoding;
    if (typeof cb !== 'function') cb = nop;
    if (state.ending) writeAfterEnd(this, cb);else if (isBuf || validChunk(this, state, chunk, cb)) {
      state.pendingcb++;
      ret = writeOrBuffer(this, state, isBuf, chunk, encoding, cb);
    }
    return ret;
  };

  Writable.prototype.cork = function () {
    this._writableState.corked++;
  };

  Writable.prototype.uncork = function () {
    var state = this._writableState;

    if (state.corked) {
      state.corked--;
      if (!state.writing && !state.corked && !state.bufferProcessing && state.bufferedRequest) clearBuffer(this, state);
    }
  };

  Writable.prototype.setDefaultEncoding = function setDefaultEncoding(encoding) {
    // node::ParseEncoding() requires lower case.
    if (typeof encoding === 'string') encoding = encoding.toLowerCase();
    if (!(['hex', 'utf8', 'utf-8', 'ascii', 'binary', 'base64', 'ucs2', 'ucs-2', 'utf16le', 'utf-16le', 'raw'].indexOf((encoding + '').toLowerCase()) > -1)) throw new ERR_UNKNOWN_ENCODING(encoding);
    this._writableState.defaultEncoding = encoding;
    return this;
  };

  Object.defineProperty(Writable.prototype, 'writableBuffer', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._writableState && this._writableState.getBuffer();
    }
  });

  function decodeChunk(state, chunk, encoding) {
    if (!state.objectMode && state.decodeStrings !== false && typeof chunk === 'string') {
      chunk = Buffer$3.from(chunk, encoding);
    }

    return chunk;
  }

  Object.defineProperty(Writable.prototype, 'writableHighWaterMark', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._writableState.highWaterMark;
    }
  }); // if we're already writing something, then just put this
  // in the queue, and wait our turn.  Otherwise, call _write
  // If we return false, then we need a drain event, so set that flag.

  function writeOrBuffer(stream, state, isBuf, chunk, encoding, cb) {
    if (!isBuf) {
      var newChunk = decodeChunk(state, chunk, encoding);

      if (chunk !== newChunk) {
        isBuf = true;
        encoding = 'buffer';
        chunk = newChunk;
      }
    }

    var len = state.objectMode ? 1 : chunk.length;
    state.length += len;
    var ret = state.length < state.highWaterMark; // we must ensure that previous needDrain will not be reset to false.

    if (!ret) state.needDrain = true;

    if (state.writing || state.corked) {
      var last = state.lastBufferedRequest;
      state.lastBufferedRequest = {
        chunk: chunk,
        encoding: encoding,
        isBuf: isBuf,
        callback: cb,
        next: null
      };

      if (last) {
        last.next = state.lastBufferedRequest;
      } else {
        state.bufferedRequest = state.lastBufferedRequest;
      }

      state.bufferedRequestCount += 1;
    } else {
      doWrite(stream, state, false, len, chunk, encoding, cb);
    }

    return ret;
  }

  function doWrite(stream, state, writev, len, chunk, encoding, cb) {
    state.writelen = len;
    state.writecb = cb;
    state.writing = true;
    state.sync = true;
    if (state.destroyed) state.onwrite(new ERR_STREAM_DESTROYED$1('write'));else if (writev) stream._writev(chunk, state.onwrite);else stream._write(chunk, encoding, state.onwrite);
    state.sync = false;
  }

  function onwriteError(stream, state, sync, er, cb) {
    --state.pendingcb;

    if (sync) {
      // defer the callback if we are being called synchronously
      // to avoid piling up things on the stack
      process.nextTick(cb, er); // this can emit finish, and it will always happen
      // after error

      process.nextTick(finishMaybe, stream, state);
      stream._writableState.errorEmitted = true;
      errorOrDestroy$1(stream, er);
    } else {
      // the caller expect this to happen before if
      // it is async
      cb(er);
      stream._writableState.errorEmitted = true;
      errorOrDestroy$1(stream, er); // this can emit finish, but finish must
      // always follow error

      finishMaybe(stream, state);
    }
  }

  function onwriteStateUpdate(state) {
    state.writing = false;
    state.writecb = null;
    state.length -= state.writelen;
    state.writelen = 0;
  }

  function onwrite(stream, er) {
    var state = stream._writableState;
    var sync = state.sync;
    var cb = state.writecb;
    if (typeof cb !== 'function') throw new ERR_MULTIPLE_CALLBACK$1();
    onwriteStateUpdate(state);
    if (er) onwriteError(stream, state, sync, er, cb);else {
      // Check if we're actually ready to finish, but don't emit yet
      var finished = needFinish(state) || stream.destroyed;

      if (!finished && !state.corked && !state.bufferProcessing && state.bufferedRequest) {
        clearBuffer(stream, state);
      }

      if (sync) {
        process.nextTick(afterWrite, stream, state, finished, cb);
      } else {
        afterWrite(stream, state, finished, cb);
      }
    }
  }

  function afterWrite(stream, state, finished, cb) {
    if (!finished) onwriteDrain(stream, state);
    state.pendingcb--;
    cb();
    finishMaybe(stream, state);
  } // Must force callback to be called on nextTick, so that we don't
  // emit 'drain' before the write() consumer gets the 'false' return
  // value, and has a chance to attach a 'drain' listener.


  function onwriteDrain(stream, state) {
    if (state.length === 0 && state.needDrain) {
      state.needDrain = false;
      stream.emit('drain');
    }
  } // if there's something in the buffer waiting, then process it


  function clearBuffer(stream, state) {
    state.bufferProcessing = true;
    var entry = state.bufferedRequest;

    if (stream._writev && entry && entry.next) {
      // Fast case, write everything using _writev()
      var l = state.bufferedRequestCount;
      var buffer = new Array(l);
      var holder = state.corkedRequestsFree;
      holder.entry = entry;
      var count = 0;
      var allBuffers = true;

      while (entry) {
        buffer[count] = entry;
        if (!entry.isBuf) allBuffers = false;
        entry = entry.next;
        count += 1;
      }

      buffer.allBuffers = allBuffers;
      doWrite(stream, state, true, state.length, buffer, '', holder.finish); // doWrite is almost always async, defer these to save a bit of time
      // as the hot path ends with doWrite

      state.pendingcb++;
      state.lastBufferedRequest = null;

      if (holder.next) {
        state.corkedRequestsFree = holder.next;
        holder.next = null;
      } else {
        state.corkedRequestsFree = new CorkedRequest(state);
      }

      state.bufferedRequestCount = 0;
    } else {
      // Slow case, write chunks one-by-one
      while (entry) {
        var chunk = entry.chunk;
        var encoding = entry.encoding;
        var cb = entry.callback;
        var len = state.objectMode ? 1 : chunk.length;
        doWrite(stream, state, false, len, chunk, encoding, cb);
        entry = entry.next;
        state.bufferedRequestCount--; // if we didn't call the onwrite immediately, then
        // it means that we need to wait until it does.
        // also, that means that the chunk and cb are currently
        // being processed, so move the buffer counter past them.

        if (state.writing) {
          break;
        }
      }

      if (entry === null) state.lastBufferedRequest = null;
    }

    state.bufferedRequest = entry;
    state.bufferProcessing = false;
  }

  Writable.prototype._write = function (chunk, encoding, cb) {
    cb(new ERR_METHOD_NOT_IMPLEMENTED$2('_write()'));
  };

  Writable.prototype._writev = null;

  Writable.prototype.end = function (chunk, encoding, cb) {
    var state = this._writableState;

    if (typeof chunk === 'function') {
      cb = chunk;
      chunk = null;
      encoding = null;
    } else if (typeof encoding === 'function') {
      cb = encoding;
      encoding = null;
    }

    if (chunk !== null && chunk !== undefined) this.write(chunk, encoding); // .end() fully uncorks

    if (state.corked) {
      state.corked = 1;
      this.uncork();
    } // ignore unnecessary end() calls.


    if (!state.ending) endWritable(this, state, cb);
    return this;
  };

  Object.defineProperty(Writable.prototype, 'writableLength', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._writableState.length;
    }
  });

  function needFinish(state) {
    return state.ending && state.length === 0 && state.bufferedRequest === null && !state.finished && !state.writing;
  }

  function callFinal(stream, state) {
    stream._final(function (err) {
      state.pendingcb--;

      if (err) {
        errorOrDestroy$1(stream, err);
      }

      state.prefinished = true;
      stream.emit('prefinish');
      finishMaybe(stream, state);
    });
  }

  function prefinish$1(stream, state) {
    if (!state.prefinished && !state.finalCalled) {
      if (typeof stream._final === 'function' && !state.destroyed) {
        state.pendingcb++;
        state.finalCalled = true;
        process.nextTick(callFinal, stream, state);
      } else {
        state.prefinished = true;
        stream.emit('prefinish');
      }
    }
  }

  function finishMaybe(stream, state) {
    var need = needFinish(state);

    if (need) {
      prefinish$1(stream, state);

      if (state.pendingcb === 0) {
        state.finished = true;
        stream.emit('finish');

        if (state.autoDestroy) {
          // In case of duplex streams we need a way to detect
          // if the readable side is ready for autoDestroy as well
          var rState = stream._readableState;

          if (!rState || rState.autoDestroy && rState.endEmitted) {
            stream.destroy();
          }
        }
      }
    }

    return need;
  }

  function endWritable(stream, state, cb) {
    state.ending = true;
    finishMaybe(stream, state);

    if (cb) {
      if (state.finished) process.nextTick(cb);else stream.once('finish', cb);
    }

    state.ended = true;
    stream.writable = false;
  }

  function onCorkedFinish(corkReq, state, err) {
    var entry = corkReq.entry;
    corkReq.entry = null;

    while (entry) {
      var cb = entry.callback;
      state.pendingcb--;
      cb(err);
      entry = entry.next;
    } // reuse the free corkReq.


    state.corkedRequestsFree.next = corkReq;
  }

  Object.defineProperty(Writable.prototype, 'destroyed', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      if (this._writableState === undefined) {
        return false;
      }

      return this._writableState.destroyed;
    },
    set: function set(value) {
      // we ignore the value if the stream
      // has not been initialized yet
      if (!this._writableState) {
        return;
      } // backward compatibility, the user is explicitly
      // managing destroyed


      this._writableState.destroyed = value;
    }
  });
  Writable.prototype.destroy = destroy_1.destroy;
  Writable.prototype._undestroy = destroy_1.undestroy;

  Writable.prototype._destroy = function (err, cb) {
    cb(err);
  };

  var require$$0 = _stream_readable;

  /*<replacement>*/

  var objectKeys = Object.keys || function (obj) {
    var keys = [];

    for (var key in obj) {
      keys.push(key);
    }

    return keys;
  };
  /*</replacement>*/


  var _stream_duplex = Duplex$1;





  inherits_browser(Duplex$1, require$$0);

  {
    // Allow the keys array to be GC'ed.
    var keys = objectKeys(_stream_writable.prototype);

    for (var v = 0; v < keys.length; v++) {
      var method = keys[v];
      if (!Duplex$1.prototype[method]) Duplex$1.prototype[method] = _stream_writable.prototype[method];
    }
  }

  function Duplex$1(options) {
    if (!(this instanceof Duplex$1)) return new Duplex$1(options);
    require$$0.call(this, options);
    _stream_writable.call(this, options);
    this.allowHalfOpen = true;

    if (options) {
      if (options.readable === false) this.readable = false;
      if (options.writable === false) this.writable = false;

      if (options.allowHalfOpen === false) {
        this.allowHalfOpen = false;
        this.once('end', onend);
      }
    }
  }

  Object.defineProperty(Duplex$1.prototype, 'writableHighWaterMark', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._writableState.highWaterMark;
    }
  });
  Object.defineProperty(Duplex$1.prototype, 'writableBuffer', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._writableState && this._writableState.getBuffer();
    }
  });
  Object.defineProperty(Duplex$1.prototype, 'writableLength', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._writableState.length;
    }
  }); // the no-half-open enforcer

  function onend() {
    // If the writable side ended, then we're ok.
    if (this._writableState.ended) return; // no more data can be written.
    // But allow more writes to happen in this tick.

    process.nextTick(onEndNT, this);
  }

  function onEndNT(self) {
    self.end();
  }

  Object.defineProperty(Duplex$1.prototype, 'destroyed', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      if (this._readableState === undefined || this._writableState === undefined) {
        return false;
      }

      return this._readableState.destroyed && this._writableState.destroyed;
    },
    set: function set(value) {
      // we ignore the value if the stream
      // has not been initialized yet
      if (this._readableState === undefined || this._writableState === undefined) {
        return;
      } // backward compatibility, the user is explicitly
      // managing destroyed


      this._readableState.destroyed = value;
      this._writableState.destroyed = value;
    }
  });

  /*! safe-buffer. MIT License. Feross Aboukhadijeh <https://feross.org/opensource> */

  var safeBuffer = createCommonjsModule(function (module, exports) {
  /* eslint-disable node/no-deprecated-api */

  var Buffer = buffer.Buffer;

  // alternative to using Object.keys for old browsers
  function copyProps (src, dst) {
    for (var key in src) {
      dst[key] = src[key];
    }
  }
  if (Buffer.from && Buffer.alloc && Buffer.allocUnsafe && Buffer.allocUnsafeSlow) {
    module.exports = buffer;
  } else {
    // Copy properties from require('buffer')
    copyProps(buffer, exports);
    exports.Buffer = SafeBuffer;
  }

  function SafeBuffer (arg, encodingOrOffset, length) {
    return Buffer(arg, encodingOrOffset, length)
  }

  SafeBuffer.prototype = Object.create(Buffer.prototype);

  // Copy static methods from Buffer
  copyProps(Buffer, SafeBuffer);

  SafeBuffer.from = function (arg, encodingOrOffset, length) {
    if (typeof arg === 'number') {
      throw new TypeError('Argument must not be a number')
    }
    return Buffer(arg, encodingOrOffset, length)
  };

  SafeBuffer.alloc = function (size, fill, encoding) {
    if (typeof size !== 'number') {
      throw new TypeError('Argument must be a number')
    }
    var buf = Buffer(size);
    if (fill !== undefined) {
      if (typeof encoding === 'string') {
        buf.fill(fill, encoding);
      } else {
        buf.fill(fill);
      }
    } else {
      buf.fill(0);
    }
    return buf
  };

  SafeBuffer.allocUnsafe = function (size) {
    if (typeof size !== 'number') {
      throw new TypeError('Argument must be a number')
    }
    return Buffer(size)
  };

  SafeBuffer.allocUnsafeSlow = function (size) {
    if (typeof size !== 'number') {
      throw new TypeError('Argument must be a number')
    }
    return buffer.SlowBuffer(size)
  };
  });

  /*<replacement>*/

  var Buffer$2 = safeBuffer.Buffer;
  /*</replacement>*/

  var isEncoding = Buffer$2.isEncoding || function (encoding) {
    encoding = '' + encoding;
    switch (encoding && encoding.toLowerCase()) {
      case 'hex':case 'utf8':case 'utf-8':case 'ascii':case 'binary':case 'base64':case 'ucs2':case 'ucs-2':case 'utf16le':case 'utf-16le':case 'raw':
        return true;
      default:
        return false;
    }
  };

  function _normalizeEncoding(enc) {
    if (!enc) return 'utf8';
    var retried;
    while (true) {
      switch (enc) {
        case 'utf8':
        case 'utf-8':
          return 'utf8';
        case 'ucs2':
        case 'ucs-2':
        case 'utf16le':
        case 'utf-16le':
          return 'utf16le';
        case 'latin1':
        case 'binary':
          return 'latin1';
        case 'base64':
        case 'ascii':
        case 'hex':
          return enc;
        default:
          if (retried) return; // undefined
          enc = ('' + enc).toLowerCase();
          retried = true;
      }
    }
  }
  // Do not cache `Buffer.isEncoding` when checking encoding names as some
  // modules monkey-patch it to support additional encodings
  function normalizeEncoding(enc) {
    var nenc = _normalizeEncoding(enc);
    if (typeof nenc !== 'string' && (Buffer$2.isEncoding === isEncoding || !isEncoding(enc))) throw new Error('Unknown encoding: ' + enc);
    return nenc || enc;
  }

  // StringDecoder provides an interface for efficiently splitting a series of
  // buffers into a series of JS strings without breaking apart multi-byte
  // characters.
  var StringDecoder_1 = StringDecoder$1;
  function StringDecoder$1(encoding) {
    this.encoding = normalizeEncoding(encoding);
    var nb;
    switch (this.encoding) {
      case 'utf16le':
        this.text = utf16Text;
        this.end = utf16End;
        nb = 4;
        break;
      case 'utf8':
        this.fillLast = utf8FillLast;
        nb = 4;
        break;
      case 'base64':
        this.text = base64Text;
        this.end = base64End;
        nb = 3;
        break;
      default:
        this.write = simpleWrite;
        this.end = simpleEnd;
        return;
    }
    this.lastNeed = 0;
    this.lastTotal = 0;
    this.lastChar = Buffer$2.allocUnsafe(nb);
  }

  StringDecoder$1.prototype.write = function (buf) {
    if (buf.length === 0) return '';
    var r;
    var i;
    if (this.lastNeed) {
      r = this.fillLast(buf);
      if (r === undefined) return '';
      i = this.lastNeed;
      this.lastNeed = 0;
    } else {
      i = 0;
    }
    if (i < buf.length) return r ? r + this.text(buf, i) : this.text(buf, i);
    return r || '';
  };

  StringDecoder$1.prototype.end = utf8End;

  // Returns only complete characters in a Buffer
  StringDecoder$1.prototype.text = utf8Text;

  // Attempts to complete a partial non-UTF-8 character using bytes from a Buffer
  StringDecoder$1.prototype.fillLast = function (buf) {
    if (this.lastNeed <= buf.length) {
      buf.copy(this.lastChar, this.lastTotal - this.lastNeed, 0, this.lastNeed);
      return this.lastChar.toString(this.encoding, 0, this.lastTotal);
    }
    buf.copy(this.lastChar, this.lastTotal - this.lastNeed, 0, buf.length);
    this.lastNeed -= buf.length;
  };

  // Checks the type of a UTF-8 byte, whether it's ASCII, a leading byte, or a
  // continuation byte. If an invalid byte is detected, -2 is returned.
  function utf8CheckByte(byte) {
    if (byte <= 0x7F) return 0;else if (byte >> 5 === 0x06) return 2;else if (byte >> 4 === 0x0E) return 3;else if (byte >> 3 === 0x1E) return 4;
    return byte >> 6 === 0x02 ? -1 : -2;
  }

  // Checks at most 3 bytes at the end of a Buffer in order to detect an
  // incomplete multi-byte UTF-8 character. The total number of bytes (2, 3, or 4)
  // needed to complete the UTF-8 character (if applicable) are returned.
  function utf8CheckIncomplete(self, buf, i) {
    var j = buf.length - 1;
    if (j < i) return 0;
    var nb = utf8CheckByte(buf[j]);
    if (nb >= 0) {
      if (nb > 0) self.lastNeed = nb - 1;
      return nb;
    }
    if (--j < i || nb === -2) return 0;
    nb = utf8CheckByte(buf[j]);
    if (nb >= 0) {
      if (nb > 0) self.lastNeed = nb - 2;
      return nb;
    }
    if (--j < i || nb === -2) return 0;
    nb = utf8CheckByte(buf[j]);
    if (nb >= 0) {
      if (nb > 0) {
        if (nb === 2) nb = 0;else self.lastNeed = nb - 3;
      }
      return nb;
    }
    return 0;
  }

  // Validates as many continuation bytes for a multi-byte UTF-8 character as
  // needed or are available. If we see a non-continuation byte where we expect
  // one, we "replace" the validated continuation bytes we've seen so far with
  // a single UTF-8 replacement character ('\ufffd'), to match v8's UTF-8 decoding
  // behavior. The continuation byte check is included three times in the case
  // where all of the continuation bytes for a character exist in the same buffer.
  // It is also done this way as a slight performance increase instead of using a
  // loop.
  function utf8CheckExtraBytes(self, buf, p) {
    if ((buf[0] & 0xC0) !== 0x80) {
      self.lastNeed = 0;
      return '\ufffd';
    }
    if (self.lastNeed > 1 && buf.length > 1) {
      if ((buf[1] & 0xC0) !== 0x80) {
        self.lastNeed = 1;
        return '\ufffd';
      }
      if (self.lastNeed > 2 && buf.length > 2) {
        if ((buf[2] & 0xC0) !== 0x80) {
          self.lastNeed = 2;
          return '\ufffd';
        }
      }
    }
  }

  // Attempts to complete a multi-byte UTF-8 character using bytes from a Buffer.
  function utf8FillLast(buf) {
    var p = this.lastTotal - this.lastNeed;
    var r = utf8CheckExtraBytes(this, buf);
    if (r !== undefined) return r;
    if (this.lastNeed <= buf.length) {
      buf.copy(this.lastChar, p, 0, this.lastNeed);
      return this.lastChar.toString(this.encoding, 0, this.lastTotal);
    }
    buf.copy(this.lastChar, p, 0, buf.length);
    this.lastNeed -= buf.length;
  }

  // Returns all complete UTF-8 characters in a Buffer. If the Buffer ended on a
  // partial character, the character's bytes are buffered until the required
  // number of bytes are available.
  function utf8Text(buf, i) {
    var total = utf8CheckIncomplete(this, buf, i);
    if (!this.lastNeed) return buf.toString('utf8', i);
    this.lastTotal = total;
    var end = buf.length - (total - this.lastNeed);
    buf.copy(this.lastChar, 0, end);
    return buf.toString('utf8', i, end);
  }

  // For UTF-8, a replacement character is added when ending on a partial
  // character.
  function utf8End(buf) {
    var r = buf && buf.length ? this.write(buf) : '';
    if (this.lastNeed) return r + '\ufffd';
    return r;
  }

  // UTF-16LE typically needs two bytes per character, but even if we have an even
  // number of bytes available, we need to check if we end on a leading/high
  // surrogate. In that case, we need to wait for the next two bytes in order to
  // decode the last character properly.
  function utf16Text(buf, i) {
    if ((buf.length - i) % 2 === 0) {
      var r = buf.toString('utf16le', i);
      if (r) {
        var c = r.charCodeAt(r.length - 1);
        if (c >= 0xD800 && c <= 0xDBFF) {
          this.lastNeed = 2;
          this.lastTotal = 4;
          this.lastChar[0] = buf[buf.length - 2];
          this.lastChar[1] = buf[buf.length - 1];
          return r.slice(0, -1);
        }
      }
      return r;
    }
    this.lastNeed = 1;
    this.lastTotal = 2;
    this.lastChar[0] = buf[buf.length - 1];
    return buf.toString('utf16le', i, buf.length - 1);
  }

  // For UTF-16LE we do not explicitly append special replacement characters if we
  // end on a partial character, we simply let v8 handle that.
  function utf16End(buf) {
    var r = buf && buf.length ? this.write(buf) : '';
    if (this.lastNeed) {
      var end = this.lastTotal - this.lastNeed;
      return r + this.lastChar.toString('utf16le', 0, end);
    }
    return r;
  }

  function base64Text(buf, i) {
    var n = (buf.length - i) % 3;
    if (n === 0) return buf.toString('base64', i);
    this.lastNeed = 3 - n;
    this.lastTotal = 3;
    if (n === 1) {
      this.lastChar[0] = buf[buf.length - 1];
    } else {
      this.lastChar[0] = buf[buf.length - 2];
      this.lastChar[1] = buf[buf.length - 1];
    }
    return buf.toString('base64', i, buf.length - n);
  }

  function base64End(buf) {
    var r = buf && buf.length ? this.write(buf) : '';
    if (this.lastNeed) return r + this.lastChar.toString('base64', 0, 3 - this.lastNeed);
    return r;
  }

  // Pass bytes on through for single-byte encodings (e.g. ascii, latin1, hex)
  function simpleWrite(buf) {
    return buf.toString(this.encoding);
  }

  function simpleEnd(buf) {
    return buf && buf.length ? this.write(buf) : '';
  }

  var string_decoder = {
  	StringDecoder: StringDecoder_1
  };

  var ERR_STREAM_PREMATURE_CLOSE = errorsBrowser.codes.ERR_STREAM_PREMATURE_CLOSE;

  function once$1(callback) {
    var called = false;
    return function () {
      if (called) return;
      called = true;

      for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }

      callback.apply(this, args);
    };
  }

  function noop$1() {}

  function isRequest$1(stream) {
    return stream.setHeader && typeof stream.abort === 'function';
  }

  function eos$1(stream, opts, callback) {
    if (typeof opts === 'function') return eos$1(stream, null, opts);
    if (!opts) opts = {};
    callback = once$1(callback || noop$1);
    var readable = opts.readable || opts.readable !== false && stream.readable;
    var writable = opts.writable || opts.writable !== false && stream.writable;

    var onlegacyfinish = function onlegacyfinish() {
      if (!stream.writable) onfinish();
    };

    var writableEnded = stream._writableState && stream._writableState.finished;

    var onfinish = function onfinish() {
      writable = false;
      writableEnded = true;
      if (!readable) callback.call(stream);
    };

    var readableEnded = stream._readableState && stream._readableState.endEmitted;

    var onend = function onend() {
      readable = false;
      readableEnded = true;
      if (!writable) callback.call(stream);
    };

    var onerror = function onerror(err) {
      callback.call(stream, err);
    };

    var onclose = function onclose() {
      var err;

      if (readable && !readableEnded) {
        if (!stream._readableState || !stream._readableState.ended) err = new ERR_STREAM_PREMATURE_CLOSE();
        return callback.call(stream, err);
      }

      if (writable && !writableEnded) {
        if (!stream._writableState || !stream._writableState.ended) err = new ERR_STREAM_PREMATURE_CLOSE();
        return callback.call(stream, err);
      }
    };

    var onrequest = function onrequest() {
      stream.req.on('finish', onfinish);
    };

    if (isRequest$1(stream)) {
      stream.on('complete', onfinish);
      stream.on('abort', onclose);
      if (stream.req) onrequest();else stream.on('request', onrequest);
    } else if (writable && !stream._writableState) {
      // legacy streams
      stream.on('end', onlegacyfinish);
      stream.on('close', onlegacyfinish);
    }

    stream.on('end', onend);
    stream.on('finish', onfinish);
    if (opts.error !== false) stream.on('error', onerror);
    stream.on('close', onclose);
    return function () {
      stream.removeListener('complete', onfinish);
      stream.removeListener('abort', onclose);
      stream.removeListener('request', onrequest);
      if (stream.req) stream.req.removeListener('finish', onfinish);
      stream.removeListener('end', onlegacyfinish);
      stream.removeListener('close', onlegacyfinish);
      stream.removeListener('finish', onfinish);
      stream.removeListener('end', onend);
      stream.removeListener('error', onerror);
      stream.removeListener('close', onclose);
    };
  }

  var endOfStream = eos$1;

  var _Object$setPrototypeO;

  function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }



  var kLastResolve = Symbol('lastResolve');
  var kLastReject = Symbol('lastReject');
  var kError = Symbol('error');
  var kEnded = Symbol('ended');
  var kLastPromise = Symbol('lastPromise');
  var kHandlePromise = Symbol('handlePromise');
  var kStream = Symbol('stream');

  function createIterResult(value, done) {
    return {
      value: value,
      done: done
    };
  }

  function readAndResolve(iter) {
    var resolve = iter[kLastResolve];

    if (resolve !== null) {
      var data = iter[kStream].read(); // we defer if data is null
      // we can be expecting either 'end' or
      // 'error'

      if (data !== null) {
        iter[kLastPromise] = null;
        iter[kLastResolve] = null;
        iter[kLastReject] = null;
        resolve(createIterResult(data, false));
      }
    }
  }

  function onReadable(iter) {
    // we wait for the next tick, because it might
    // emit an error with process.nextTick
    process.nextTick(readAndResolve, iter);
  }

  function wrapForNext(lastPromise, iter) {
    return function (resolve, reject) {
      lastPromise.then(function () {
        if (iter[kEnded]) {
          resolve(createIterResult(undefined, true));
          return;
        }

        iter[kHandlePromise](resolve, reject);
      }, reject);
    };
  }

  var AsyncIteratorPrototype = Object.getPrototypeOf(function () {});
  var ReadableStreamAsyncIteratorPrototype = Object.setPrototypeOf((_Object$setPrototypeO = {
    get stream() {
      return this[kStream];
    },

    next: function next() {
      var _this = this;

      // if we have detected an error in the meanwhile
      // reject straight away
      var error = this[kError];

      if (error !== null) {
        return Promise.reject(error);
      }

      if (this[kEnded]) {
        return Promise.resolve(createIterResult(undefined, true));
      }

      if (this[kStream].destroyed) {
        // We need to defer via nextTick because if .destroy(err) is
        // called, the error will be emitted via nextTick, and
        // we cannot guarantee that there is no error lingering around
        // waiting to be emitted.
        return new Promise(function (resolve, reject) {
          process.nextTick(function () {
            if (_this[kError]) {
              reject(_this[kError]);
            } else {
              resolve(createIterResult(undefined, true));
            }
          });
        });
      } // if we have multiple next() calls
      // we will wait for the previous Promise to finish
      // this logic is optimized to support for await loops,
      // where next() is only called once at a time


      var lastPromise = this[kLastPromise];
      var promise;

      if (lastPromise) {
        promise = new Promise(wrapForNext(lastPromise, this));
      } else {
        // fast path needed to support multiple this.push()
        // without triggering the next() queue
        var data = this[kStream].read();

        if (data !== null) {
          return Promise.resolve(createIterResult(data, false));
        }

        promise = new Promise(this[kHandlePromise]);
      }

      this[kLastPromise] = promise;
      return promise;
    }
  }, _defineProperty(_Object$setPrototypeO, Symbol.asyncIterator, function () {
    return this;
  }), _defineProperty(_Object$setPrototypeO, "return", function _return() {
    var _this2 = this;

    // destroy(err, cb) is a private API
    // we can guarantee we have that here, because we control the
    // Readable class this is attached to
    return new Promise(function (resolve, reject) {
      _this2[kStream].destroy(null, function (err) {
        if (err) {
          reject(err);
          return;
        }

        resolve(createIterResult(undefined, true));
      });
    });
  }), _Object$setPrototypeO), AsyncIteratorPrototype);

  var createReadableStreamAsyncIterator$1 = function createReadableStreamAsyncIterator(stream) {
    var _Object$create;

    var iterator = Object.create(ReadableStreamAsyncIteratorPrototype, (_Object$create = {}, _defineProperty(_Object$create, kStream, {
      value: stream,
      writable: true
    }), _defineProperty(_Object$create, kLastResolve, {
      value: null,
      writable: true
    }), _defineProperty(_Object$create, kLastReject, {
      value: null,
      writable: true
    }), _defineProperty(_Object$create, kError, {
      value: null,
      writable: true
    }), _defineProperty(_Object$create, kEnded, {
      value: stream._readableState.endEmitted,
      writable: true
    }), _defineProperty(_Object$create, kHandlePromise, {
      value: function value(resolve, reject) {
        var data = iterator[kStream].read();

        if (data) {
          iterator[kLastPromise] = null;
          iterator[kLastResolve] = null;
          iterator[kLastReject] = null;
          resolve(createIterResult(data, false));
        } else {
          iterator[kLastResolve] = resolve;
          iterator[kLastReject] = reject;
        }
      },
      writable: true
    }), _Object$create));
    iterator[kLastPromise] = null;
    endOfStream(stream, function (err) {
      if (err && err.code !== 'ERR_STREAM_PREMATURE_CLOSE') {
        var reject = iterator[kLastReject]; // reject if we are waiting for data in the Promise
        // returned by next() and store the error

        if (reject !== null) {
          iterator[kLastPromise] = null;
          iterator[kLastResolve] = null;
          iterator[kLastReject] = null;
          reject(err);
        }

        iterator[kError] = err;
        return;
      }

      var resolve = iterator[kLastResolve];

      if (resolve !== null) {
        iterator[kLastPromise] = null;
        iterator[kLastResolve] = null;
        iterator[kLastReject] = null;
        resolve(createIterResult(undefined, true));
      }

      iterator[kEnded] = true;
    });
    stream.on('readable', onReadable.bind(null, iterator));
    return iterator;
  };

  var async_iterator = createReadableStreamAsyncIterator$1;

  var fromBrowser = function () {
    throw new Error('Readable.from is not available in the browser')
  };

  var _stream_readable = Readable;
  /*<replacement>*/

  var Duplex;
  /*</replacement>*/

  Readable.ReadableState = ReadableState;

  var EElistenerCount = function EElistenerCount(emitter, type) {
    return emitter.listeners(type).length;
  };
  /*</replacement>*/

  /*<replacement>*/



  /*</replacement>*/


  var Buffer$1 = buffer.Buffer;

  var OurUint8Array = commonjsGlobal.Uint8Array || function () {};

  function _uint8ArrayToBuffer(chunk) {
    return Buffer$1.from(chunk);
  }

  function _isUint8Array(obj) {
    return Buffer$1.isBuffer(obj) || obj instanceof OurUint8Array;
  }
  /*<replacement>*/




  var debug;

  if (debugUtil && debugUtil.debuglog) {
    debug = debugUtil.debuglog('stream');
  } else {
    debug = function debug() {};
  }
  /*</replacement>*/






  var getHighWaterMark = state.getHighWaterMark;

  var _require$codes$2 = errorsBrowser.codes,
      ERR_INVALID_ARG_TYPE = _require$codes$2.ERR_INVALID_ARG_TYPE,
      ERR_STREAM_PUSH_AFTER_EOF = _require$codes$2.ERR_STREAM_PUSH_AFTER_EOF,
      ERR_METHOD_NOT_IMPLEMENTED$1 = _require$codes$2.ERR_METHOD_NOT_IMPLEMENTED,
      ERR_STREAM_UNSHIFT_AFTER_END_EVENT = _require$codes$2.ERR_STREAM_UNSHIFT_AFTER_END_EVENT; // Lazy loaded to improve the startup performance.


  var StringDecoder;
  var createReadableStreamAsyncIterator;
  var from;

  inherits_browser(Readable, streamBrowser);

  var errorOrDestroy = destroy_1.errorOrDestroy;
  var kProxyEvents = ['error', 'close', 'destroy', 'pause', 'resume'];

  function prependListener(emitter, event, fn) {
    // Sadly this is not cacheable as some libraries bundle their own
    // event emitter implementation with them.
    if (typeof emitter.prependListener === 'function') return emitter.prependListener(event, fn); // This is a hack to make sure that our error handler is attached before any
    // userland ones.  NEVER DO THIS. This is here only because this code needs
    // to continue to work with older versions of Node.js that do not include
    // the prependListener() method. The goal is to eventually remove this hack.

    if (!emitter._events || !emitter._events[event]) emitter.on(event, fn);else if (Array.isArray(emitter._events[event])) emitter._events[event].unshift(fn);else emitter._events[event] = [fn, emitter._events[event]];
  }

  function ReadableState(options, stream, isDuplex) {
    Duplex = Duplex || require$$2;
    options = options || {}; // Duplex streams are both readable and writable, but share
    // the same options object.
    // However, some cases require setting options to different
    // values for the readable and the writable sides of the duplex stream.
    // These options can be provided separately as readableXXX and writableXXX.

    if (typeof isDuplex !== 'boolean') isDuplex = stream instanceof Duplex; // object stream flag. Used to make read(n) ignore n and to
    // make all the buffer merging and length checks go away

    this.objectMode = !!options.objectMode;
    if (isDuplex) this.objectMode = this.objectMode || !!options.readableObjectMode; // the point at which it stops calling _read() to fill the buffer
    // Note: 0 is a valid value, means "don't call _read preemptively ever"

    this.highWaterMark = getHighWaterMark(this, options, 'readableHighWaterMark', isDuplex); // A linked list is used to store data chunks instead of an array because the
    // linked list can remove elements from the beginning faster than
    // array.shift()

    this.buffer = new buffer_list();
    this.length = 0;
    this.pipes = null;
    this.pipesCount = 0;
    this.flowing = null;
    this.ended = false;
    this.endEmitted = false;
    this.reading = false; // a flag to be able to tell if the event 'readable'/'data' is emitted
    // immediately, or on a later tick.  We set this to true at first, because
    // any actions that shouldn't happen until "later" should generally also
    // not happen before the first read call.

    this.sync = true; // whenever we return null, then we set a flag to say
    // that we're awaiting a 'readable' event emission.

    this.needReadable = false;
    this.emittedReadable = false;
    this.readableListening = false;
    this.resumeScheduled = false;
    this.paused = true; // Should close be emitted on destroy. Defaults to true.

    this.emitClose = options.emitClose !== false; // Should .destroy() be called after 'end' (and potentially 'finish')

    this.autoDestroy = !!options.autoDestroy; // has it been destroyed

    this.destroyed = false; // Crypto is kind of old and crusty.  Historically, its default string
    // encoding is 'binary' so we have to make this configurable.
    // Everything else in the universe uses 'utf8', though.

    this.defaultEncoding = options.defaultEncoding || 'utf8'; // the number of writers that are awaiting a drain event in .pipe()s

    this.awaitDrain = 0; // if true, a maybeReadMore has been scheduled

    this.readingMore = false;
    this.decoder = null;
    this.encoding = null;

    if (options.encoding) {
      if (!StringDecoder) StringDecoder = string_decoder.StringDecoder;
      this.decoder = new StringDecoder(options.encoding);
      this.encoding = options.encoding;
    }
  }

  function Readable(options) {
    Duplex = Duplex || require$$2;
    if (!(this instanceof Readable)) return new Readable(options); // Checking for a Stream.Duplex instance is faster here instead of inside
    // the ReadableState constructor, at least with V8 6.5

    var isDuplex = this instanceof Duplex;
    this._readableState = new ReadableState(options, this, isDuplex); // legacy

    this.readable = true;

    if (options) {
      if (typeof options.read === 'function') this._read = options.read;
      if (typeof options.destroy === 'function') this._destroy = options.destroy;
    }

    streamBrowser.call(this);
  }

  Object.defineProperty(Readable.prototype, 'destroyed', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      if (this._readableState === undefined) {
        return false;
      }

      return this._readableState.destroyed;
    },
    set: function set(value) {
      // we ignore the value if the stream
      // has not been initialized yet
      if (!this._readableState) {
        return;
      } // backward compatibility, the user is explicitly
      // managing destroyed


      this._readableState.destroyed = value;
    }
  });
  Readable.prototype.destroy = destroy_1.destroy;
  Readable.prototype._undestroy = destroy_1.undestroy;

  Readable.prototype._destroy = function (err, cb) {
    cb(err);
  }; // Manually shove something into the read() buffer.
  // This returns true if the highWaterMark has not been hit yet,
  // similar to how Writable.write() returns true if you should
  // write() some more.


  Readable.prototype.push = function (chunk, encoding) {
    var state = this._readableState;
    var skipChunkCheck;

    if (!state.objectMode) {
      if (typeof chunk === 'string') {
        encoding = encoding || state.defaultEncoding;

        if (encoding !== state.encoding) {
          chunk = Buffer$1.from(chunk, encoding);
          encoding = '';
        }

        skipChunkCheck = true;
      }
    } else {
      skipChunkCheck = true;
    }

    return readableAddChunk(this, chunk, encoding, false, skipChunkCheck);
  }; // Unshift should *always* be something directly out of read()


  Readable.prototype.unshift = function (chunk) {
    return readableAddChunk(this, chunk, null, true, false);
  };

  function readableAddChunk(stream, chunk, encoding, addToFront, skipChunkCheck) {
    debug('readableAddChunk', chunk);
    var state = stream._readableState;

    if (chunk === null) {
      state.reading = false;
      onEofChunk(stream, state);
    } else {
      var er;
      if (!skipChunkCheck) er = chunkInvalid(state, chunk);

      if (er) {
        errorOrDestroy(stream, er);
      } else if (state.objectMode || chunk && chunk.length > 0) {
        if (typeof chunk !== 'string' && !state.objectMode && Object.getPrototypeOf(chunk) !== Buffer$1.prototype) {
          chunk = _uint8ArrayToBuffer(chunk);
        }

        if (addToFront) {
          if (state.endEmitted) errorOrDestroy(stream, new ERR_STREAM_UNSHIFT_AFTER_END_EVENT());else addChunk(stream, state, chunk, true);
        } else if (state.ended) {
          errorOrDestroy(stream, new ERR_STREAM_PUSH_AFTER_EOF());
        } else if (state.destroyed) {
          return false;
        } else {
          state.reading = false;

          if (state.decoder && !encoding) {
            chunk = state.decoder.write(chunk);
            if (state.objectMode || chunk.length !== 0) addChunk(stream, state, chunk, false);else maybeReadMore(stream, state);
          } else {
            addChunk(stream, state, chunk, false);
          }
        }
      } else if (!addToFront) {
        state.reading = false;
        maybeReadMore(stream, state);
      }
    } // We can push more data if we are below the highWaterMark.
    // Also, if we have no data yet, we can stand some more bytes.
    // This is to work around cases where hwm=0, such as the repl.


    return !state.ended && (state.length < state.highWaterMark || state.length === 0);
  }

  function addChunk(stream, state, chunk, addToFront) {
    if (state.flowing && state.length === 0 && !state.sync) {
      state.awaitDrain = 0;
      stream.emit('data', chunk);
    } else {
      // update the buffer info.
      state.length += state.objectMode ? 1 : chunk.length;
      if (addToFront) state.buffer.unshift(chunk);else state.buffer.push(chunk);
      if (state.needReadable) emitReadable(stream);
    }

    maybeReadMore(stream, state);
  }

  function chunkInvalid(state, chunk) {
    var er;

    if (!_isUint8Array(chunk) && typeof chunk !== 'string' && chunk !== undefined && !state.objectMode) {
      er = new ERR_INVALID_ARG_TYPE('chunk', ['string', 'Buffer', 'Uint8Array'], chunk);
    }

    return er;
  }

  Readable.prototype.isPaused = function () {
    return this._readableState.flowing === false;
  }; // backwards compatibility.


  Readable.prototype.setEncoding = function (enc) {
    if (!StringDecoder) StringDecoder = string_decoder.StringDecoder;
    var decoder = new StringDecoder(enc);
    this._readableState.decoder = decoder; // If setEncoding(null), decoder.encoding equals utf8

    this._readableState.encoding = this._readableState.decoder.encoding; // Iterate over current buffer to convert already stored Buffers:

    var p = this._readableState.buffer.head;
    var content = '';

    while (p !== null) {
      content += decoder.write(p.data);
      p = p.next;
    }

    this._readableState.buffer.clear();

    if (content !== '') this._readableState.buffer.push(content);
    this._readableState.length = content.length;
    return this;
  }; // Don't raise the hwm > 1GB


  var MAX_HWM = 0x40000000;

  function computeNewHighWaterMark(n) {
    if (n >= MAX_HWM) {
      // TODO(ronag): Throw ERR_VALUE_OUT_OF_RANGE.
      n = MAX_HWM;
    } else {
      // Get the next highest power of 2 to prevent increasing hwm excessively in
      // tiny amounts
      n--;
      n |= n >>> 1;
      n |= n >>> 2;
      n |= n >>> 4;
      n |= n >>> 8;
      n |= n >>> 16;
      n++;
    }

    return n;
  } // This function is designed to be inlinable, so please take care when making
  // changes to the function body.


  function howMuchToRead(n, state) {
    if (n <= 0 || state.length === 0 && state.ended) return 0;
    if (state.objectMode) return 1;

    if (n !== n) {
      // Only flow one buffer at a time
      if (state.flowing && state.length) return state.buffer.head.data.length;else return state.length;
    } // If we're asking for more than the current hwm, then raise the hwm.


    if (n > state.highWaterMark) state.highWaterMark = computeNewHighWaterMark(n);
    if (n <= state.length) return n; // Don't have enough

    if (!state.ended) {
      state.needReadable = true;
      return 0;
    }

    return state.length;
  } // you can override either this method, or the async _read(n) below.


  Readable.prototype.read = function (n) {
    debug('read', n);
    n = parseInt(n, 10);
    var state = this._readableState;
    var nOrig = n;
    if (n !== 0) state.emittedReadable = false; // if we're doing read(0) to trigger a readable event, but we
    // already have a bunch of data in the buffer, then just trigger
    // the 'readable' event and move on.

    if (n === 0 && state.needReadable && ((state.highWaterMark !== 0 ? state.length >= state.highWaterMark : state.length > 0) || state.ended)) {
      debug('read: emitReadable', state.length, state.ended);
      if (state.length === 0 && state.ended) endReadable(this);else emitReadable(this);
      return null;
    }

    n = howMuchToRead(n, state); // if we've ended, and we're now clear, then finish it up.

    if (n === 0 && state.ended) {
      if (state.length === 0) endReadable(this);
      return null;
    } // All the actual chunk generation logic needs to be
    // *below* the call to _read.  The reason is that in certain
    // synthetic stream cases, such as passthrough streams, _read
    // may be a completely synchronous operation which may change
    // the state of the read buffer, providing enough data when
    // before there was *not* enough.
    //
    // So, the steps are:
    // 1. Figure out what the state of things will be after we do
    // a read from the buffer.
    //
    // 2. If that resulting state will trigger a _read, then call _read.
    // Note that this may be asynchronous, or synchronous.  Yes, it is
    // deeply ugly to write APIs this way, but that still doesn't mean
    // that the Readable class should behave improperly, as streams are
    // designed to be sync/async agnostic.
    // Take note if the _read call is sync or async (ie, if the read call
    // has returned yet), so that we know whether or not it's safe to emit
    // 'readable' etc.
    //
    // 3. Actually pull the requested chunks out of the buffer and return.
    // if we need a readable event, then we need to do some reading.


    var doRead = state.needReadable;
    debug('need readable', doRead); // if we currently have less than the highWaterMark, then also read some

    if (state.length === 0 || state.length - n < state.highWaterMark) {
      doRead = true;
      debug('length less than watermark', doRead);
    } // however, if we've ended, then there's no point, and if we're already
    // reading, then it's unnecessary.


    if (state.ended || state.reading) {
      doRead = false;
      debug('reading or ended', doRead);
    } else if (doRead) {
      debug('do read');
      state.reading = true;
      state.sync = true; // if the length is currently zero, then we *need* a readable event.

      if (state.length === 0) state.needReadable = true; // call internal read method

      this._read(state.highWaterMark);

      state.sync = false; // If _read pushed data synchronously, then `reading` will be false,
      // and we need to re-evaluate how much data we can return to the user.

      if (!state.reading) n = howMuchToRead(nOrig, state);
    }

    var ret;
    if (n > 0) ret = fromList(n, state);else ret = null;

    if (ret === null) {
      state.needReadable = state.length <= state.highWaterMark;
      n = 0;
    } else {
      state.length -= n;
      state.awaitDrain = 0;
    }

    if (state.length === 0) {
      // If we have nothing in the buffer, then we want to know
      // as soon as we *do* get something into the buffer.
      if (!state.ended) state.needReadable = true; // If we tried to read() past the EOF, then emit end on the next tick.

      if (nOrig !== n && state.ended) endReadable(this);
    }

    if (ret !== null) this.emit('data', ret);
    return ret;
  };

  function onEofChunk(stream, state) {
    debug('onEofChunk');
    if (state.ended) return;

    if (state.decoder) {
      var chunk = state.decoder.end();

      if (chunk && chunk.length) {
        state.buffer.push(chunk);
        state.length += state.objectMode ? 1 : chunk.length;
      }
    }

    state.ended = true;

    if (state.sync) {
      // if we are sync, wait until next tick to emit the data.
      // Otherwise we risk emitting data in the flow()
      // the readable code triggers during a read() call
      emitReadable(stream);
    } else {
      // emit 'readable' now to make sure it gets picked up.
      state.needReadable = false;

      if (!state.emittedReadable) {
        state.emittedReadable = true;
        emitReadable_(stream);
      }
    }
  } // Don't emit readable right away in sync mode, because this can trigger
  // another read() call => stack overflow.  This way, it might trigger
  // a nextTick recursion warning, but that's not so bad.


  function emitReadable(stream) {
    var state = stream._readableState;
    debug('emitReadable', state.needReadable, state.emittedReadable);
    state.needReadable = false;

    if (!state.emittedReadable) {
      debug('emitReadable', state.flowing);
      state.emittedReadable = true;
      process.nextTick(emitReadable_, stream);
    }
  }

  function emitReadable_(stream) {
    var state = stream._readableState;
    debug('emitReadable_', state.destroyed, state.length, state.ended);

    if (!state.destroyed && (state.length || state.ended)) {
      stream.emit('readable');
      state.emittedReadable = false;
    } // The stream needs another readable event if
    // 1. It is not flowing, as the flow mechanism will take
    //    care of it.
    // 2. It is not ended.
    // 3. It is below the highWaterMark, so we can schedule
    //    another readable later.


    state.needReadable = !state.flowing && !state.ended && state.length <= state.highWaterMark;
    flow(stream);
  } // at this point, the user has presumably seen the 'readable' event,
  // and called read() to consume some data.  that may have triggered
  // in turn another _read(n) call, in which case reading = true if
  // it's in progress.
  // However, if we're not ended, or reading, and the length < hwm,
  // then go ahead and try to read some more preemptively.


  function maybeReadMore(stream, state) {
    if (!state.readingMore) {
      state.readingMore = true;
      process.nextTick(maybeReadMore_, stream, state);
    }
  }

  function maybeReadMore_(stream, state) {
    // Attempt to read more data if we should.
    //
    // The conditions for reading more data are (one of):
    // - Not enough data buffered (state.length < state.highWaterMark). The loop
    //   is responsible for filling the buffer with enough data if such data
    //   is available. If highWaterMark is 0 and we are not in the flowing mode
    //   we should _not_ attempt to buffer any extra data. We'll get more data
    //   when the stream consumer calls read() instead.
    // - No data in the buffer, and the stream is in flowing mode. In this mode
    //   the loop below is responsible for ensuring read() is called. Failing to
    //   call read here would abort the flow and there's no other mechanism for
    //   continuing the flow if the stream consumer has just subscribed to the
    //   'data' event.
    //
    // In addition to the above conditions to keep reading data, the following
    // conditions prevent the data from being read:
    // - The stream has ended (state.ended).
    // - There is already a pending 'read' operation (state.reading). This is a
    //   case where the the stream has called the implementation defined _read()
    //   method, but they are processing the call asynchronously and have _not_
    //   called push() with new data. In this case we skip performing more
    //   read()s. The execution ends in this method again after the _read() ends
    //   up calling push() with more data.
    while (!state.reading && !state.ended && (state.length < state.highWaterMark || state.flowing && state.length === 0)) {
      var len = state.length;
      debug('maybeReadMore read 0');
      stream.read(0);
      if (len === state.length) // didn't get any data, stop spinning.
        break;
    }

    state.readingMore = false;
  } // abstract method.  to be overridden in specific implementation classes.
  // call cb(er, data) where data is <= n in length.
  // for virtual (non-string, non-buffer) streams, "length" is somewhat
  // arbitrary, and perhaps not very meaningful.


  Readable.prototype._read = function (n) {
    errorOrDestroy(this, new ERR_METHOD_NOT_IMPLEMENTED$1('_read()'));
  };

  Readable.prototype.pipe = function (dest, pipeOpts) {
    var src = this;
    var state = this._readableState;

    switch (state.pipesCount) {
      case 0:
        state.pipes = dest;
        break;

      case 1:
        state.pipes = [state.pipes, dest];
        break;

      default:
        state.pipes.push(dest);
        break;
    }

    state.pipesCount += 1;
    debug('pipe count=%d opts=%j', state.pipesCount, pipeOpts);
    var doEnd = (!pipeOpts || pipeOpts.end !== false) && dest !== process.stdout && dest !== process.stderr;
    var endFn = doEnd ? onend : unpipe;
    if (state.endEmitted) process.nextTick(endFn);else src.once('end', endFn);
    dest.on('unpipe', onunpipe);

    function onunpipe(readable, unpipeInfo) {
      debug('onunpipe');

      if (readable === src) {
        if (unpipeInfo && unpipeInfo.hasUnpiped === false) {
          unpipeInfo.hasUnpiped = true;
          cleanup();
        }
      }
    }

    function onend() {
      debug('onend');
      dest.end();
    } // when the dest drains, it reduces the awaitDrain counter
    // on the source.  This would be more elegant with a .once()
    // handler in flow(), but adding and removing repeatedly is
    // too slow.


    var ondrain = pipeOnDrain(src);
    dest.on('drain', ondrain);
    var cleanedUp = false;

    function cleanup() {
      debug('cleanup'); // cleanup event handlers once the pipe is broken

      dest.removeListener('close', onclose);
      dest.removeListener('finish', onfinish);
      dest.removeListener('drain', ondrain);
      dest.removeListener('error', onerror);
      dest.removeListener('unpipe', onunpipe);
      src.removeListener('end', onend);
      src.removeListener('end', unpipe);
      src.removeListener('data', ondata);
      cleanedUp = true; // if the reader is waiting for a drain event from this
      // specific writer, then it would cause it to never start
      // flowing again.
      // So, if this is awaiting a drain, then we just call it now.
      // If we don't know, then assume that we are waiting for one.

      if (state.awaitDrain && (!dest._writableState || dest._writableState.needDrain)) ondrain();
    }

    src.on('data', ondata);

    function ondata(chunk) {
      debug('ondata');
      var ret = dest.write(chunk);
      debug('dest.write', ret);

      if (ret === false) {
        // If the user unpiped during `dest.write()`, it is possible
        // to get stuck in a permanently paused state if that write
        // also returned false.
        // => Check whether `dest` is still a piping destination.
        if ((state.pipesCount === 1 && state.pipes === dest || state.pipesCount > 1 && indexOf(state.pipes, dest) !== -1) && !cleanedUp) {
          debug('false write response, pause', state.awaitDrain);
          state.awaitDrain++;
        }

        src.pause();
      }
    } // if the dest has an error, then stop piping into it.
    // however, don't suppress the throwing behavior for this.


    function onerror(er) {
      debug('onerror', er);
      unpipe();
      dest.removeListener('error', onerror);
      if (EElistenerCount(dest, 'error') === 0) errorOrDestroy(dest, er);
    } // Make sure our error handler is attached before userland ones.


    prependListener(dest, 'error', onerror); // Both close and finish should trigger unpipe, but only once.

    function onclose() {
      dest.removeListener('finish', onfinish);
      unpipe();
    }

    dest.once('close', onclose);

    function onfinish() {
      debug('onfinish');
      dest.removeListener('close', onclose);
      unpipe();
    }

    dest.once('finish', onfinish);

    function unpipe() {
      debug('unpipe');
      src.unpipe(dest);
    } // tell the dest that it's being piped to


    dest.emit('pipe', src); // start the flow if it hasn't been started already.

    if (!state.flowing) {
      debug('pipe resume');
      src.resume();
    }

    return dest;
  };

  function pipeOnDrain(src) {
    return function pipeOnDrainFunctionResult() {
      var state = src._readableState;
      debug('pipeOnDrain', state.awaitDrain);
      if (state.awaitDrain) state.awaitDrain--;

      if (state.awaitDrain === 0 && EElistenerCount(src, 'data')) {
        state.flowing = true;
        flow(src);
      }
    };
  }

  Readable.prototype.unpipe = function (dest) {
    var state = this._readableState;
    var unpipeInfo = {
      hasUnpiped: false
    }; // if we're not piping anywhere, then do nothing.

    if (state.pipesCount === 0) return this; // just one destination.  most common case.

    if (state.pipesCount === 1) {
      // passed in one, but it's not the right one.
      if (dest && dest !== state.pipes) return this;
      if (!dest) dest = state.pipes; // got a match.

      state.pipes = null;
      state.pipesCount = 0;
      state.flowing = false;
      if (dest) dest.emit('unpipe', this, unpipeInfo);
      return this;
    } // slow case. multiple pipe destinations.


    if (!dest) {
      // remove all.
      var dests = state.pipes;
      var len = state.pipesCount;
      state.pipes = null;
      state.pipesCount = 0;
      state.flowing = false;

      for (var i = 0; i < len; i++) {
        dests[i].emit('unpipe', this, {
          hasUnpiped: false
        });
      }

      return this;
    } // try to find the right one.


    var index = indexOf(state.pipes, dest);
    if (index === -1) return this;
    state.pipes.splice(index, 1);
    state.pipesCount -= 1;
    if (state.pipesCount === 1) state.pipes = state.pipes[0];
    dest.emit('unpipe', this, unpipeInfo);
    return this;
  }; // set up data events if they are asked for
  // Ensure readable listeners eventually get something


  Readable.prototype.on = function (ev, fn) {
    var res = streamBrowser.prototype.on.call(this, ev, fn);
    var state = this._readableState;

    if (ev === 'data') {
      // update readableListening so that resume() may be a no-op
      // a few lines down. This is needed to support once('readable').
      state.readableListening = this.listenerCount('readable') > 0; // Try start flowing on next tick if stream isn't explicitly paused

      if (state.flowing !== false) this.resume();
    } else if (ev === 'readable') {
      if (!state.endEmitted && !state.readableListening) {
        state.readableListening = state.needReadable = true;
        state.flowing = false;
        state.emittedReadable = false;
        debug('on readable', state.length, state.reading);

        if (state.length) {
          emitReadable(this);
        } else if (!state.reading) {
          process.nextTick(nReadingNextTick, this);
        }
      }
    }

    return res;
  };

  Readable.prototype.addListener = Readable.prototype.on;

  Readable.prototype.removeListener = function (ev, fn) {
    var res = streamBrowser.prototype.removeListener.call(this, ev, fn);

    if (ev === 'readable') {
      // We need to check if there is someone still listening to
      // readable and reset the state. However this needs to happen
      // after readable has been emitted but before I/O (nextTick) to
      // support once('readable', fn) cycles. This means that calling
      // resume within the same tick will have no
      // effect.
      process.nextTick(updateReadableListening, this);
    }

    return res;
  };

  Readable.prototype.removeAllListeners = function (ev) {
    var res = streamBrowser.prototype.removeAllListeners.apply(this, arguments);

    if (ev === 'readable' || ev === undefined) {
      // We need to check if there is someone still listening to
      // readable and reset the state. However this needs to happen
      // after readable has been emitted but before I/O (nextTick) to
      // support once('readable', fn) cycles. This means that calling
      // resume within the same tick will have no
      // effect.
      process.nextTick(updateReadableListening, this);
    }

    return res;
  };

  function updateReadableListening(self) {
    var state = self._readableState;
    state.readableListening = self.listenerCount('readable') > 0;

    if (state.resumeScheduled && !state.paused) {
      // flowing needs to be set to true now, otherwise
      // the upcoming resume will not flow.
      state.flowing = true; // crude way to check if we should resume
    } else if (self.listenerCount('data') > 0) {
      self.resume();
    }
  }

  function nReadingNextTick(self) {
    debug('readable nexttick read 0');
    self.read(0);
  } // pause() and resume() are remnants of the legacy readable stream API
  // If the user uses them, then switch into old mode.


  Readable.prototype.resume = function () {
    var state = this._readableState;

    if (!state.flowing) {
      debug('resume'); // we flow only if there is no one listening
      // for readable, but we still have to call
      // resume()

      state.flowing = !state.readableListening;
      resume(this, state);
    }

    state.paused = false;
    return this;
  };

  function resume(stream, state) {
    if (!state.resumeScheduled) {
      state.resumeScheduled = true;
      process.nextTick(resume_, stream, state);
    }
  }

  function resume_(stream, state) {
    debug('resume', state.reading);

    if (!state.reading) {
      stream.read(0);
    }

    state.resumeScheduled = false;
    stream.emit('resume');
    flow(stream);
    if (state.flowing && !state.reading) stream.read(0);
  }

  Readable.prototype.pause = function () {
    debug('call pause flowing=%j', this._readableState.flowing);

    if (this._readableState.flowing !== false) {
      debug('pause');
      this._readableState.flowing = false;
      this.emit('pause');
    }

    this._readableState.paused = true;
    return this;
  };

  function flow(stream) {
    var state = stream._readableState;
    debug('flow', state.flowing);

    while (state.flowing && stream.read() !== null) {
    }
  } // wrap an old-style stream as the async data source.
  // This is *not* part of the readable stream interface.
  // It is an ugly unfortunate mess of history.


  Readable.prototype.wrap = function (stream) {
    var _this = this;

    var state = this._readableState;
    var paused = false;
    stream.on('end', function () {
      debug('wrapped end');

      if (state.decoder && !state.ended) {
        var chunk = state.decoder.end();
        if (chunk && chunk.length) _this.push(chunk);
      }

      _this.push(null);
    });
    stream.on('data', function (chunk) {
      debug('wrapped data');
      if (state.decoder) chunk = state.decoder.write(chunk); // don't skip over falsy values in objectMode

      if (state.objectMode && (chunk === null || chunk === undefined)) return;else if (!state.objectMode && (!chunk || !chunk.length)) return;

      var ret = _this.push(chunk);

      if (!ret) {
        paused = true;
        stream.pause();
      }
    }); // proxy all the other methods.
    // important when wrapping filters and duplexes.

    for (var i in stream) {
      if (this[i] === undefined && typeof stream[i] === 'function') {
        this[i] = function methodWrap(method) {
          return function methodWrapReturnFunction() {
            return stream[method].apply(stream, arguments);
          };
        }(i);
      }
    } // proxy certain important events.


    for (var n = 0; n < kProxyEvents.length; n++) {
      stream.on(kProxyEvents[n], this.emit.bind(this, kProxyEvents[n]));
    } // when we try to consume some more bytes, simply unpause the
    // underlying stream.


    this._read = function (n) {
      debug('wrapped _read', n);

      if (paused) {
        paused = false;
        stream.resume();
      }
    };

    return this;
  };

  if (typeof Symbol === 'function') {
    Readable.prototype[Symbol.asyncIterator] = function () {
      if (createReadableStreamAsyncIterator === undefined) {
        createReadableStreamAsyncIterator = async_iterator;
      }

      return createReadableStreamAsyncIterator(this);
    };
  }

  Object.defineProperty(Readable.prototype, 'readableHighWaterMark', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._readableState.highWaterMark;
    }
  });
  Object.defineProperty(Readable.prototype, 'readableBuffer', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._readableState && this._readableState.buffer;
    }
  });
  Object.defineProperty(Readable.prototype, 'readableFlowing', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._readableState.flowing;
    },
    set: function set(state) {
      if (this._readableState) {
        this._readableState.flowing = state;
      }
    }
  }); // exposed for testing purposes only.

  Readable._fromList = fromList;
  Object.defineProperty(Readable.prototype, 'readableLength', {
    // making it explicit this property is not enumerable
    // because otherwise some prototype manipulation in
    // userland will fail
    enumerable: false,
    get: function get() {
      return this._readableState.length;
    }
  }); // Pluck off n bytes from an array of buffers.
  // Length is the combined lengths of all the buffers in the list.
  // This function is designed to be inlinable, so please take care when making
  // changes to the function body.

  function fromList(n, state) {
    // nothing buffered
    if (state.length === 0) return null;
    var ret;
    if (state.objectMode) ret = state.buffer.shift();else if (!n || n >= state.length) {
      // read it all, truncate the list
      if (state.decoder) ret = state.buffer.join('');else if (state.buffer.length === 1) ret = state.buffer.first();else ret = state.buffer.concat(state.length);
      state.buffer.clear();
    } else {
      // read part of list
      ret = state.buffer.consume(n, state.decoder);
    }
    return ret;
  }

  function endReadable(stream) {
    var state = stream._readableState;
    debug('endReadable', state.endEmitted);

    if (!state.endEmitted) {
      state.ended = true;
      process.nextTick(endReadableNT, state, stream);
    }
  }

  function endReadableNT(state, stream) {
    debug('endReadableNT', state.endEmitted, state.length); // Check that we didn't get one last unshift.

    if (!state.endEmitted && state.length === 0) {
      state.endEmitted = true;
      stream.readable = false;
      stream.emit('end');

      if (state.autoDestroy) {
        // In case of duplex streams we need a way to detect
        // if the writable side is ready for autoDestroy as well
        var wState = stream._writableState;

        if (!wState || wState.autoDestroy && wState.finished) {
          stream.destroy();
        }
      }
    }
  }

  if (typeof Symbol === 'function') {
    Readable.from = function (iterable, opts) {
      if (from === undefined) {
        from = fromBrowser;
      }

      return from(Readable, iterable, opts);
    };
  }

  function indexOf(xs, x) {
    for (var i = 0, l = xs.length; i < l; i++) {
      if (xs[i] === x) return i;
    }

    return -1;
  }

  var _stream_transform = Transform;

  var _require$codes$1 = errorsBrowser.codes,
      ERR_METHOD_NOT_IMPLEMENTED = _require$codes$1.ERR_METHOD_NOT_IMPLEMENTED,
      ERR_MULTIPLE_CALLBACK = _require$codes$1.ERR_MULTIPLE_CALLBACK,
      ERR_TRANSFORM_ALREADY_TRANSFORMING = _require$codes$1.ERR_TRANSFORM_ALREADY_TRANSFORMING,
      ERR_TRANSFORM_WITH_LENGTH_0 = _require$codes$1.ERR_TRANSFORM_WITH_LENGTH_0;



  inherits_browser(Transform, require$$2);

  function afterTransform(er, data) {
    var ts = this._transformState;
    ts.transforming = false;
    var cb = ts.writecb;

    if (cb === null) {
      return this.emit('error', new ERR_MULTIPLE_CALLBACK());
    }

    ts.writechunk = null;
    ts.writecb = null;
    if (data != null) // single equals check for both `null` and `undefined`
      this.push(data);
    cb(er);
    var rs = this._readableState;
    rs.reading = false;

    if (rs.needReadable || rs.length < rs.highWaterMark) {
      this._read(rs.highWaterMark);
    }
  }

  function Transform(options) {
    if (!(this instanceof Transform)) return new Transform(options);
    require$$2.call(this, options);
    this._transformState = {
      afterTransform: afterTransform.bind(this),
      needTransform: false,
      transforming: false,
      writecb: null,
      writechunk: null,
      writeencoding: null
    }; // start out asking for a readable event once data is transformed.

    this._readableState.needReadable = true; // we have implemented the _read method, and done the other things
    // that Readable wants before the first _read call, so unset the
    // sync guard flag.

    this._readableState.sync = false;

    if (options) {
      if (typeof options.transform === 'function') this._transform = options.transform;
      if (typeof options.flush === 'function') this._flush = options.flush;
    } // When the writable side finishes, then flush out anything remaining.


    this.on('prefinish', prefinish);
  }

  function prefinish() {
    var _this = this;

    if (typeof this._flush === 'function' && !this._readableState.destroyed) {
      this._flush(function (er, data) {
        done(_this, er, data);
      });
    } else {
      done(this, null, null);
    }
  }

  Transform.prototype.push = function (chunk, encoding) {
    this._transformState.needTransform = false;
    return require$$2.prototype.push.call(this, chunk, encoding);
  }; // This is the part where you do stuff!
  // override this function in implementation classes.
  // 'chunk' is an input chunk.
  //
  // Call `push(newChunk)` to pass along transformed output
  // to the readable side.  You may call 'push' zero or more times.
  //
  // Call `cb(err)` when you are done with this chunk.  If you pass
  // an error, then that'll put the hurt on the whole operation.  If you
  // never call cb(), then you'll never get another chunk.


  Transform.prototype._transform = function (chunk, encoding, cb) {
    cb(new ERR_METHOD_NOT_IMPLEMENTED('_transform()'));
  };

  Transform.prototype._write = function (chunk, encoding, cb) {
    var ts = this._transformState;
    ts.writecb = cb;
    ts.writechunk = chunk;
    ts.writeencoding = encoding;

    if (!ts.transforming) {
      var rs = this._readableState;
      if (ts.needTransform || rs.needReadable || rs.length < rs.highWaterMark) this._read(rs.highWaterMark);
    }
  }; // Doesn't matter what the args are here.
  // _transform does all the work.
  // That we got here means that the readable side wants more data.


  Transform.prototype._read = function (n) {
    var ts = this._transformState;

    if (ts.writechunk !== null && !ts.transforming) {
      ts.transforming = true;

      this._transform(ts.writechunk, ts.writeencoding, ts.afterTransform);
    } else {
      // mark that we need a transform, so that any data that comes in
      // will get processed, now that we've asked for it.
      ts.needTransform = true;
    }
  };

  Transform.prototype._destroy = function (err, cb) {
    require$$2.prototype._destroy.call(this, err, function (err2) {
      cb(err2);
    });
  };

  function done(stream, er, data) {
    if (er) return stream.emit('error', er);
    if (data != null) // single equals check for both `null` and `undefined`
      stream.push(data); // TODO(BridgeAR): Write a test for these two error cases
    // if there's nothing in the write buffer, then that means
    // that nothing more will ever be provided

    if (stream._writableState.length) throw new ERR_TRANSFORM_WITH_LENGTH_0();
    if (stream._transformState.transforming) throw new ERR_TRANSFORM_ALREADY_TRANSFORMING();
    return stream.push(null);
  }

  var _stream_passthrough = PassThrough;



  inherits_browser(PassThrough, _stream_transform);

  function PassThrough(options) {
    if (!(this instanceof PassThrough)) return new PassThrough(options);
    _stream_transform.call(this, options);
  }

  PassThrough.prototype._transform = function (chunk, encoding, cb) {
    cb(null, chunk);
  };

  var eos;

  function once(callback) {
    var called = false;
    return function () {
      if (called) return;
      called = true;
      callback.apply(void 0, arguments);
    };
  }

  var _require$codes = errorsBrowser.codes,
      ERR_MISSING_ARGS = _require$codes.ERR_MISSING_ARGS,
      ERR_STREAM_DESTROYED = _require$codes.ERR_STREAM_DESTROYED;

  function noop(err) {
    // Rethrow the error if it exists to avoid swallowing it
    if (err) throw err;
  }

  function isRequest(stream) {
    return stream.setHeader && typeof stream.abort === 'function';
  }

  function destroyer(stream, reading, writing, callback) {
    callback = once(callback);
    var closed = false;
    stream.on('close', function () {
      closed = true;
    });
    if (eos === undefined) eos = endOfStream;
    eos(stream, {
      readable: reading,
      writable: writing
    }, function (err) {
      if (err) return callback(err);
      closed = true;
      callback();
    });
    var destroyed = false;
    return function (err) {
      if (closed) return;
      if (destroyed) return;
      destroyed = true; // request.destroy just do .end - .abort is what we want

      if (isRequest(stream)) return stream.abort();
      if (typeof stream.destroy === 'function') return stream.destroy();
      callback(err || new ERR_STREAM_DESTROYED('pipe'));
    };
  }

  function call(fn) {
    fn();
  }

  function pipe(from, to) {
    return from.pipe(to);
  }

  function popCallback(streams) {
    if (!streams.length) return noop;
    if (typeof streams[streams.length - 1] !== 'function') return noop;
    return streams.pop();
  }

  function pipeline() {
    for (var _len = arguments.length, streams = new Array(_len), _key = 0; _key < _len; _key++) {
      streams[_key] = arguments[_key];
    }

    var callback = popCallback(streams);
    if (Array.isArray(streams[0])) streams = streams[0];

    if (streams.length < 2) {
      throw new ERR_MISSING_ARGS('streams');
    }

    var error;
    var destroys = streams.map(function (stream, i) {
      var reading = i < streams.length - 1;
      var writing = i > 0;
      return destroyer(stream, reading, writing, function (err) {
        if (!error) error = err;
        if (err) destroys.forEach(call);
        if (reading) return;
        destroys.forEach(call);
        callback(error);
      });
    });
    return streams.reduce(pipe);
  }

  var pipeline_1 = pipeline;

  var readableBrowser = createCommonjsModule(function (module, exports) {
  exports = module.exports = require$$0;
  exports.Stream = exports;
  exports.Readable = exports;
  exports.Writable = _stream_writable;
  exports.Duplex = require$$2;
  exports.Transform = _stream_transform;
  exports.PassThrough = _stream_passthrough;
  exports.finished = endOfStream;
  exports.pipeline = pipeline_1;
  });

  // **N3Store** objects store N3 quads by graph in memory.

  // ## Constructor
  class N3Store {
    constructor(quads, options) {
      // The number of quads is initially zero
      this._size = 0;
      // `_graphs` contains subject, predicate, and object indexes per graph
      this._graphs = Object.create(null);
      // `_ids` maps entities such as `http://xmlns.com/foaf/0.1/name` to numbers,
      // saving memory by using only numbers as keys in `_graphs`
      this._id = 0;
      this._ids = Object.create(null);
      this._ids['><'] = 0; // dummy entry, so the first actual key is non-zero
      this._entities = Object.create(null); // inverse of `_ids`
      // `_blankNodeIndex` is the index of the last automatically named blank node
      this._blankNodeIndex = 0;

      // Shift parameters if `quads` is not given
      if (!options && quads && !quads[0])
        options = quads, quads = null;
      options = options || {};
      this._factory = options.factory || N3DataFactory;

      // Add quads if passed
      if (quads)
        this.addQuads(quads);
    }

    // ## Public properties

    // ### `size` returns the number of quads in the store
    get size() {
      // Return the quad count if if was cached
      let size = this._size;
      if (size !== null)
        return size;

      // Calculate the number of quads by counting to the deepest level
      size = 0;
      const graphs = this._graphs;
      let subjects, subject;
      for (const graphKey in graphs)
        for (const subjectKey in (subjects = graphs[graphKey].subjects))
          for (const predicateKey in (subject = subjects[subjectKey]))
            size += Object.keys(subject[predicateKey]).length;
      return this._size = size;
    }

    // ## Private methods

    // ### `_addToIndex` adds a quad to a three-layered index.
    // Returns if the index has changed, if the entry did not already exist.
    _addToIndex(index0, key0, key1, key2) {
      // Create layers as necessary
      const index1 = index0[key0] || (index0[key0] = {});
      const index2 = index1[key1] || (index1[key1] = {});
      // Setting the key to _any_ value signals the presence of the quad
      const existed = key2 in index2;
      if (!existed)
        index2[key2] = null;
      return !existed;
    }

    // ### `_removeFromIndex` removes a quad from a three-layered index
    _removeFromIndex(index0, key0, key1, key2) {
      // Remove the quad from the index
      const index1 = index0[key0], index2 = index1[key1];
      delete index2[key2];

      // Remove intermediary index layers if they are empty
      for (const key in index2) return;
      delete index1[key1];
      for (const key in index1) return;
      delete index0[key0];
    }

    // ### `_findInIndex` finds a set of quads in a three-layered index.
    // The index base is `index0` and the keys at each level are `key0`, `key1`, and `key2`.
    // Any of these keys can be undefined, which is interpreted as a wildcard.
    // `name0`, `name1`, and `name2` are the names of the keys at each level,
    // used when reconstructing the resulting quad
    // (for instance: _subject_, _predicate_, and _object_).
    // Finally, `graph` will be the graph of the created quads.
    // If `callback` is given, each result is passed through it
    // and iteration halts when it returns truthy for any quad.
    // If instead `array` is given, each result is added to the array.
    _findInIndex(index0, key0, key1, key2, name0, name1, name2, graph, callback, array) {
      let tmp, index1, index2;
      // Depending on the number of variables, keys or reverse index are faster
      const varCount = !key0 + !key1 + !key2,
          entityKeys = varCount > 1 ? Object.keys(this._ids) : this._entities;

      // If a key is specified, use only that part of index 0.
      if (key0) (tmp = index0, index0 = {})[key0] = tmp[key0];
      for (const value0 in index0) {
        const entity0 = entityKeys[value0];

        if (index1 = index0[value0]) {
          // If a key is specified, use only that part of index 1.
          if (key1) (tmp = index1, index1 = {})[key1] = tmp[key1];
          for (const value1 in index1) {
            const entity1 = entityKeys[value1];

            if (index2 = index1[value1]) {
              // If a key is specified, use only that part of index 2, if it exists.
              const values = key2 ? (key2 in index2 ? [key2] : []) : Object.keys(index2);
              // Create quads for all items found in index 2.
              for (let l = 0; l < values.length; l++) {
                const parts = { subject: null, predicate: null, object: null };
                parts[name0] = termFromId(entity0, this._factory);
                parts[name1] = termFromId(entity1, this._factory);
                parts[name2] = termFromId(entityKeys[values[l]], this._factory);
                const quad = this._factory.quad(
                  parts.subject, parts.predicate, parts.object, termFromId(graph, this._factory));
                if (array)
                  array.push(quad);
                else if (callback(quad))
                  return true;
              }
            }
          }
        }
      }
      return array;
    }

    // ### `_loop` executes the callback on all keys of index 0
    _loop(index0, callback) {
      for (const key0 in index0)
        callback(key0);
    }

    // ### `_loopByKey0` executes the callback on all keys of a certain entry in index 0
    _loopByKey0(index0, key0, callback) {
      let index1, key1;
      if (index1 = index0[key0]) {
        for (key1 in index1)
          callback(key1);
      }
    }

    // ### `_loopByKey1` executes the callback on given keys of all entries in index 0
    _loopByKey1(index0, key1, callback) {
      let key0, index1;
      for (key0 in index0) {
        index1 = index0[key0];
        if (index1[key1])
          callback(key0);
      }
    }

    // ### `_loopBy2Keys` executes the callback on given keys of certain entries in index 2
    _loopBy2Keys(index0, key0, key1, callback) {
      let index1, index2, key2;
      if ((index1 = index0[key0]) && (index2 = index1[key1])) {
        for (key2 in index2)
          callback(key2);
      }
    }

    // ### `_countInIndex` counts matching quads in a three-layered index.
    // The index base is `index0` and the keys at each level are `key0`, `key1`, and `key2`.
    // Any of these keys can be undefined, which is interpreted as a wildcard.
    _countInIndex(index0, key0, key1, key2) {
      let count = 0, tmp, index1, index2;

      // If a key is specified, count only that part of index 0
      if (key0) (tmp = index0, index0 = {})[key0] = tmp[key0];
      for (const value0 in index0) {
        if (index1 = index0[value0]) {
          // If a key is specified, count only that part of index 1
          if (key1) (tmp = index1, index1 = {})[key1] = tmp[key1];
          for (const value1 in index1) {
            if (index2 = index1[value1]) {
              // If a key is specified, count the quad if it exists
              if (key2) (key2 in index2) && count++;
              // Otherwise, count all quads
              else count += Object.keys(index2).length;
            }
          }
        }
      }
      return count;
    }

    // ### `_getGraphs` returns an array with the given graph,
    // or all graphs if the argument is null or undefined.
    _getGraphs(graph) {
      if (!isString(graph))
        return this._graphs;
      const graphs = {};
      graphs[graph] = this._graphs[graph];
      return graphs;
    }

    // ### `_uniqueEntities` returns a function that accepts an entity ID
    // and passes the corresponding entity to callback if it hasn't occurred before.
    _uniqueEntities(callback) {
      const uniqueIds = Object.create(null);
      return id => {
        if (!(id in uniqueIds)) {
          uniqueIds[id] = true;
          callback(termFromId(this._entities[id], this._factory));
        }
      };
    }

    // ## Public methods

    // ### `add` adds the specified quad to the dataset.
    // Returns the dataset instance it was called on.
    // Existing quads, as defined in Quad.equals, will be ignored.
    add(quad) {
      this.addQuad(quad);
      return this;
    }

    // ### `addQuad` adds a new quad to the store.
    // Returns if the quad index has changed, if the quad did not already exist.
    addQuad(subject, predicate, object, graph) {
      // Shift arguments if a quad object is given instead of components
      if (!predicate)
        graph = subject.graph, object = subject.object,
          predicate = subject.predicate, subject = subject.subject;

      // Convert terms to internal string representation
      subject = termToId(subject);
      predicate = termToId(predicate);
      object = termToId(object);
      graph = termToId(graph);

      // Find the graph that will contain the triple
      let graphItem = this._graphs[graph];
      // Create the graph if it doesn't exist yet
      if (!graphItem) {
        graphItem = this._graphs[graph] = { subjects: {}, predicates: {}, objects: {} };
        // Freezing a graph helps subsequent `add` performance,
        // and properties will never be modified anyway
        Object.freeze(graphItem);
      }

      // Since entities can often be long IRIs, we avoid storing them in every index.
      // Instead, we have a separate index that maps entities to numbers,
      // which are then used as keys in the other indexes.
      const ids = this._ids;
      const entities = this._entities;
      subject   = ids[subject]   || (ids[entities[++this._id] = subject]   = this._id);
      predicate = ids[predicate] || (ids[entities[++this._id] = predicate] = this._id);
      object    = ids[object]    || (ids[entities[++this._id] = object]    = this._id);

      const changed = this._addToIndex(graphItem.subjects,   subject,   predicate, object);
      this._addToIndex(graphItem.predicates, predicate, object,    subject);
      this._addToIndex(graphItem.objects,    object,    subject,   predicate);

      // The cached quad count is now invalid
      this._size = null;
      return changed;
    }

    // ### `addQuads` adds multiple quads to the store
    addQuads(quads) {
      for (let i = 0; i < quads.length; i++)
        this.addQuad(quads[i]);
    }

    // ### `delete` removes the specified quad from the dataset.
    // Returns the dataset instance it was called on.
    delete(quad) {
      this.removeQuad(quad);
      return this;
    }

    // ### `has` determines whether a dataset includes a certain quad.
    // Returns true or false as appropriate.
    has(quad) {
      const quads = this.getQuads(quad.subject, quad.predicate, quad.object, quad.graph);
      return quads.length !== 0;
    }

    // ### `import` adds a stream of quads to the store
    import(stream) {
      stream.on('data', quad => { this.addQuad(quad); });
      return stream;
    }

    // ### `removeQuad` removes a quad from the store if it exists
    removeQuad(subject, predicate, object, graph) {
      // Shift arguments if a quad object is given instead of components
      if (!predicate)
        graph = subject.graph, object = subject.object,
          predicate = subject.predicate, subject = subject.subject;

      // Convert terms to internal string representation
      subject = termToId(subject);
      predicate = termToId(predicate);
      object = termToId(object);
      graph = termToId(graph);

      // Find internal identifiers for all components
      // and verify the quad exists.
      const ids = this._ids, graphs = this._graphs;
      let graphItem, subjects, predicates;
      if (!(subject    = ids[subject]) || !(predicate = ids[predicate]) ||
          !(object     = ids[object])  || !(graphItem = graphs[graph])  ||
          !(subjects   = graphItem.subjects[subject]) ||
          !(predicates = subjects[predicate]) ||
          !(object in predicates))
        return false;

      // Remove it from all indexes
      this._removeFromIndex(graphItem.subjects,   subject,   predicate, object);
      this._removeFromIndex(graphItem.predicates, predicate, object,    subject);
      this._removeFromIndex(graphItem.objects,    object,    subject,   predicate);
      if (this._size !== null) this._size--;

      // Remove the graph if it is empty
      for (subject in graphItem.subjects) return true;
      delete graphs[graph];
      return true;
    }

    // ### `removeQuads` removes multiple quads from the store
    removeQuads(quads) {
      for (let i = 0; i < quads.length; i++)
        this.removeQuad(quads[i]);
    }

    // ### `remove` removes a stream of quads from the store
    remove(stream) {
      stream.on('data', quad => { this.removeQuad(quad); });
      return stream;
    }

    // ### `removeMatches` removes all matching quads from the store
    // Setting any field to `undefined` or `null` indicates a wildcard.
    removeMatches(subject, predicate, object, graph) {
      const stream = new readableBrowser.Readable({ objectMode: true });

      stream._read = () => {
        for (const quad of this.getQuads(subject, predicate, object, graph))
          stream.push(quad);
        stream.push(null);
      };

      return this.remove(stream);
    }

    // ### `deleteGraph` removes all triples with the given graph from the store
    deleteGraph(graph) {
      return this.removeMatches(null, null, null, graph);
    }

    // ### `getQuads` returns an array of quads matching a pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    getQuads(subject, predicate, object, graph) {
      // Convert terms to internal string representation
      subject = subject && termToId(subject);
      predicate = predicate && termToId(predicate);
      object = object && termToId(object);
      graph = graph && termToId(graph);

      const quads = [], graphs = this._getGraphs(graph), ids = this._ids;
      let content, subjectId, predicateId, objectId;

      // Translate IRIs to internal index keys.
      if (isString(subject)   && !(subjectId   = ids[subject])   ||
          isString(predicate) && !(predicateId = ids[predicate]) ||
          isString(object)    && !(objectId    = ids[object]))
        return quads;

      for (const graphId in graphs) {
        // Only if the specified graph contains triples, there can be results
        if (content = graphs[graphId]) {
          // Choose the optimal index, based on what fields are present
          if (subjectId) {
            if (objectId)
              // If subject and object are given, the object index will be the fastest
              this._findInIndex(content.objects, objectId, subjectId, predicateId,
                                'object', 'subject', 'predicate', graphId, null, quads);
            else
              // If only subject and possibly predicate are given, the subject index will be the fastest
              this._findInIndex(content.subjects, subjectId, predicateId, null,
                                'subject', 'predicate', 'object', graphId, null, quads);
          }
          else if (predicateId)
            // If only predicate and possibly object are given, the predicate index will be the fastest
            this._findInIndex(content.predicates, predicateId, objectId, null,
                              'predicate', 'object', 'subject', graphId, null, quads);
          else if (objectId)
            // If only object is given, the object index will be the fastest
            this._findInIndex(content.objects, objectId, null, null,
                              'object', 'subject', 'predicate', graphId, null, quads);
          else
            // If nothing is given, iterate subjects and predicates first
            this._findInIndex(content.subjects, null, null, null,
                              'subject', 'predicate', 'object', graphId, null, quads);
        }
      }
      return quads;
    }

    // ### `match` returns a new dataset that is comprised of all quads in the current instance matching the given arguments.
    // The logic described in Quad Matching is applied for each quad in this dataset to check if it should be included in the output dataset.
    // Note: This method always returns a new DatasetCore, even if that dataset contains no quads.
    // Note: Since a DatasetCore is an unordered set, the order of the quads within the returned sequence is arbitrary.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    // For backwards compatibility, the object return also implements the Readable stream interface.
    match(subject, predicate, object, graph) {
      return new DatasetCoreAndReadableStream(this, subject, predicate, object, graph);
    }

    // ### `countQuads` returns the number of quads matching a pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    countQuads(subject, predicate, object, graph) {
      // Convert terms to internal string representation
      subject = subject && termToId(subject);
      predicate = predicate && termToId(predicate);
      object = object && termToId(object);
      graph = graph && termToId(graph);

      const graphs = this._getGraphs(graph), ids = this._ids;
      let count = 0, content, subjectId, predicateId, objectId;

      // Translate IRIs to internal index keys.
      if (isString(subject)   && !(subjectId   = ids[subject])   ||
          isString(predicate) && !(predicateId = ids[predicate]) ||
          isString(object)    && !(objectId    = ids[object]))
        return 0;

      for (const graphId in graphs) {
        // Only if the specified graph contains triples, there can be results
        if (content = graphs[graphId]) {
          // Choose the optimal index, based on what fields are present
          if (subject) {
            if (object)
              // If subject and object are given, the object index will be the fastest
              count += this._countInIndex(content.objects, objectId, subjectId, predicateId);
            else
              // If only subject and possibly predicate are given, the subject index will be the fastest
              count += this._countInIndex(content.subjects, subjectId, predicateId, objectId);
          }
          else if (predicate) {
            // If only predicate and possibly object are given, the predicate index will be the fastest
            count += this._countInIndex(content.predicates, predicateId, objectId, subjectId);
          }
          else {
            // If only object is possibly given, the object index will be the fastest
            count += this._countInIndex(content.objects, objectId, subjectId, predicateId);
          }
        }
      }
      return count;
    }

    // ### `forEach` executes the callback on all quads.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    forEach(callback, subject, predicate, object, graph) {
      this.some(quad => {
        callback(quad);
        return false;
      }, subject, predicate, object, graph);
    }

    // ### `every` executes the callback on all quads,
    // and returns `true` if it returns truthy for all them.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    every(callback, subject, predicate, object, graph) {
      let some = false;
      const every = !this.some(quad => {
        some = true;
        return !callback(quad);
      }, subject, predicate, object, graph);
      return some && every;
    }

    // ### `some` executes the callback on all quads,
    // and returns `true` if it returns truthy for any of them.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    some(callback, subject, predicate, object, graph) {
      // Convert terms to internal string representation
      subject = subject && termToId(subject);
      predicate = predicate && termToId(predicate);
      object = object && termToId(object);
      graph = graph && termToId(graph);

      const graphs = this._getGraphs(graph), ids = this._ids;
      let content, subjectId, predicateId, objectId;

      // Translate IRIs to internal index keys.
      if (isString(subject)   && !(subjectId   = ids[subject])   ||
          isString(predicate) && !(predicateId = ids[predicate]) ||
          isString(object)    && !(objectId    = ids[object]))
        return false;

      for (const graphId in graphs) {
        // Only if the specified graph contains triples, there can be results
        if (content = graphs[graphId]) {
          // Choose the optimal index, based on what fields are present
          if (subjectId) {
            if (objectId) {
            // If subject and object are given, the object index will be the fastest
              if (this._findInIndex(content.objects, objectId, subjectId, predicateId,
                                    'object', 'subject', 'predicate', graphId, callback, null))
                return true;
            }
            else
              // If only subject and possibly predicate are given, the subject index will be the fastest
              if (this._findInIndex(content.subjects, subjectId, predicateId, null,
                                    'subject', 'predicate', 'object', graphId, callback, null))
                return true;
          }
          else if (predicateId) {
            // If only predicate and possibly object are given, the predicate index will be the fastest
            if (this._findInIndex(content.predicates, predicateId, objectId, null,
                                  'predicate', 'object', 'subject', graphId, callback, null)) {
              return true;
            }
          }
          else if (objectId) {
            // If only object is given, the object index will be the fastest
            if (this._findInIndex(content.objects, objectId, null, null,
                                  'object', 'subject', 'predicate', graphId, callback, null)) {
              return true;
            }
          }
          else
          // If nothing is given, iterate subjects and predicates first
          if (this._findInIndex(content.subjects, null, null, null,
                                'subject', 'predicate', 'object', graphId, callback, null)) {
            return true;
          }
        }
      }
      return false;
    }

    // ### `getSubjects` returns all subjects that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    getSubjects(predicate, object, graph) {
      const results = [];
      this.forSubjects(s => { results.push(s); }, predicate, object, graph);
      return results;
    }

    // ### `forSubjects` executes the callback on all subjects that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    forSubjects(callback, predicate, object, graph) {
      // Convert terms to internal string representation
      predicate = predicate && termToId(predicate);
      object = object && termToId(object);
      graph = graph && termToId(graph);

      const ids = this._ids, graphs = this._getGraphs(graph);
      let content, predicateId, objectId;
      callback = this._uniqueEntities(callback);

      // Translate IRIs to internal index keys.
      if (isString(predicate) && !(predicateId = ids[predicate]) ||
          isString(object)    && !(objectId    = ids[object]))
        return;

      for (graph in graphs) {
        // Only if the specified graph contains triples, there can be results
        if (content = graphs[graph]) {
          // Choose optimal index based on which fields are wildcards
          if (predicateId) {
            if (objectId)
              // If predicate and object are given, the POS index is best.
              this._loopBy2Keys(content.predicates, predicateId, objectId, callback);
            else
              // If only predicate is given, the SPO index is best.
              this._loopByKey1(content.subjects, predicateId, callback);
          }
          else if (objectId)
            // If only object is given, the OSP index is best.
            this._loopByKey0(content.objects, objectId, callback);
          else
            // If no params given, iterate all the subjects
            this._loop(content.subjects, callback);
        }
      }
    }

    // ### `getPredicates` returns all predicates that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    getPredicates(subject, object, graph) {
      const results = [];
      this.forPredicates(p => { results.push(p); }, subject, object, graph);
      return results;
    }

    // ### `forPredicates` executes the callback on all predicates that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    forPredicates(callback, subject, object, graph) {
      // Convert terms to internal string representation
      subject = subject && termToId(subject);
      object = object && termToId(object);
      graph = graph && termToId(graph);

      const ids = this._ids, graphs = this._getGraphs(graph);
      let content, subjectId, objectId;
      callback = this._uniqueEntities(callback);

      // Translate IRIs to internal index keys.
      if (isString(subject) && !(subjectId = ids[subject]) ||
          isString(object)  && !(objectId  = ids[object]))
        return;

      for (graph in graphs) {
        // Only if the specified graph contains triples, there can be results
        if (content = graphs[graph]) {
          // Choose optimal index based on which fields are wildcards
          if (subjectId) {
            if (objectId)
              // If subject and object are given, the OSP index is best.
              this._loopBy2Keys(content.objects, objectId, subjectId, callback);
            else
              // If only subject is given, the SPO index is best.
              this._loopByKey0(content.subjects, subjectId, callback);
          }
          else if (objectId)
            // If only object is given, the POS index is best.
            this._loopByKey1(content.predicates, objectId, callback);
          else
            // If no params given, iterate all the predicates.
            this._loop(content.predicates, callback);
        }
      }
    }

    // ### `getObjects` returns all objects that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    getObjects(subject, predicate, graph) {
      const results = [];
      this.forObjects(o => { results.push(o); }, subject, predicate, graph);
      return results;
    }

    // ### `forObjects` executes the callback on all objects that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    forObjects(callback, subject, predicate, graph) {
      // Convert terms to internal string representation
      subject = subject && termToId(subject);
      predicate = predicate && termToId(predicate);
      graph = graph && termToId(graph);

      const ids = this._ids, graphs = this._getGraphs(graph);
      let content, subjectId, predicateId;
      callback = this._uniqueEntities(callback);

      // Translate IRIs to internal index keys.
      if (isString(subject)   && !(subjectId   = ids[subject]) ||
          isString(predicate) && !(predicateId = ids[predicate]))
        return;

      for (graph in graphs) {
        // Only if the specified graph contains triples, there can be results
        if (content = graphs[graph]) {
          // Choose optimal index based on which fields are wildcards
          if (subjectId) {
            if (predicateId)
              // If subject and predicate are given, the SPO index is best.
              this._loopBy2Keys(content.subjects, subjectId, predicateId, callback);
            else
              // If only subject is given, the OSP index is best.
              this._loopByKey1(content.objects, subjectId, callback);
          }
          else if (predicateId)
            // If only predicate is given, the POS index is best.
            this._loopByKey0(content.predicates, predicateId, callback);
          else
            // If no params given, iterate all the objects.
            this._loop(content.objects, callback);
        }
      }
    }

    // ### `getGraphs` returns all graphs that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    getGraphs(subject, predicate, object) {
      const results = [];
      this.forGraphs(g => { results.push(g); }, subject, predicate, object);
      return results;
    }

    // ### `forGraphs` executes the callback on all graphs that match the pattern.
    // Setting any field to `undefined` or `null` indicates a wildcard.
    forGraphs(callback, subject, predicate, object) {
      for (const graph in this._graphs) {
        this.some(quad => {
          callback(quad.graph);
          return true; // Halt iteration of some()
        }, subject, predicate, object, graph);
      }
    }

    // ### `createBlankNode` creates a new blank node, returning its name
    createBlankNode(suggestedName) {
      let name, index;
      // Generate a name based on the suggested name
      if (suggestedName) {
        name = suggestedName = `_:${suggestedName}`, index = 1;
        while (this._ids[name])
          name = suggestedName + index++;
      }
      // Generate a generic blank node name
      else {
        do { name = `_:b${this._blankNodeIndex++}`; }
        while (this._ids[name]);
      }
      // Add the blank node to the entities, avoiding the generation of duplicates
      this._ids[name] = ++this._id;
      this._entities[this._id] = name;
      return this._factory.blankNode(name.substr(2));
    }

    // ### `extractLists` finds and removes all list triples
    // and returns the items per list.
    extractLists({ remove = false, ignoreErrors = false } = {}) {
      const lists = {}; // has scalar keys so could be a simple Object
      const onError = ignoreErrors ? (() => true) :
                    ((node, message) => { throw new Error(`${node.value} ${message}`); });

      // Traverse each list from its tail
      const tails = this.getQuads(null, namespaces.rdf.rest, namespaces.rdf.nil, null);
      const toRemove = remove ? [...tails] : [];
      tails.forEach(tailQuad => {
        const items = [];             // the members found as objects of rdf:first quads
        let malformed = false;      // signals whether the current list is malformed
        let head;                   // the head of the list (_:b1 in above example)
        let headPos;                // set to subject or object when head is set
        const graph = tailQuad.graph; // make sure list is in exactly one graph

        // Traverse the list from tail to end
        let current = tailQuad.subject;
        while (current && !malformed) {
          const objectQuads = this.getQuads(null, null, current, null);
          const subjectQuads = this.getQuads(current, null, null, null);
          let quad, first = null, rest = null, parent = null;

          // Find the first and rest of this list node
          for (let i = 0; i < subjectQuads.length && !malformed; i++) {
            quad = subjectQuads[i];
            if (!quad.graph.equals(graph))
              malformed = onError(current, 'not confined to single graph');
            else if (head)
              malformed = onError(current, 'has non-list arcs out');

            // one rdf:first
            else if (quad.predicate.value === namespaces.rdf.first) {
              if (first)
                malformed = onError(current, 'has multiple rdf:first arcs');
              else
                toRemove.push(first = quad);
            }

            // one rdf:rest
            else if (quad.predicate.value === namespaces.rdf.rest) {
              if (rest)
                malformed = onError(current, 'has multiple rdf:rest arcs');
              else
                toRemove.push(rest = quad);
            }

            // alien triple
            else if (objectQuads.length)
              malformed = onError(current, 'can\'t be subject and object');
            else {
              head = quad; // e.g. { (1 2 3) :p :o }
              headPos = 'subject';
            }
          }

          // { :s :p (1 2) } arrives here with no head
          // { (1 2) :p :o } arrives here with head set to the list.
          for (let i = 0; i < objectQuads.length && !malformed; ++i) {
            quad = objectQuads[i];
            if (head)
              malformed = onError(current, 'can\'t have coreferences');
            // one rdf:rest
            else if (quad.predicate.value === namespaces.rdf.rest) {
              if (parent)
                malformed = onError(current, 'has incoming rdf:rest arcs');
              else
                parent = quad;
            }
            else {
              head = quad; // e.g. { :s :p (1 2) }
              headPos = 'object';
            }
          }

          // Store the list item and continue with parent
          if (!first)
            malformed = onError(current, 'has no list head');
          else
            items.unshift(first.object);
          current = parent && parent.subject;
        }

        // Don't remove any quads if the list is malformed
        if (malformed)
          remove = false;
        // Store the list under the value of its head
        else if (head)
          lists[head[headPos].value] = items;
      });

      // Remove list quads if requested
      if (remove)
        this.removeQuads(toRemove);
      return lists;
    }

    // ### Store is an iterable.
    // Can be used where iterables are expected: for...of loops, array spread operator,
    // `yield*`, and destructuring assignment (order is not guaranteed).
    *[Symbol.iterator]() {
      yield* this.getQuads();
    }
  }

  // Determines whether the argument is a string
  function isString(s) {
    return typeof s === 'string' || s instanceof String;
  }

  /**
   * A class that implements both DatasetCore and Readable.
   */
  class DatasetCoreAndReadableStream extends readableBrowser.Readable {
    constructor(n3Store, subject, predicate, object, graph) {
      super({ objectMode: true });
      Object.assign(this, { n3Store, subject, predicate, object, graph });
    }

    get filtered() {
      if (!this._filtered) {
        const { n3Store, graph, object, predicate, subject } = this;
        const quads = n3Store.getQuads(subject, predicate, object, graph);
        this._filtered = new N3Store(quads, { factory: n3Store._factory });
      }
      return this._filtered;
    }
    get size() {
      return this.filtered.size;
    }

    _read() {
      for (const quad of this.filtered.getQuads())
        this.push(quad);
      this.push(null);
    }

    add(quad) {
      return this.filtered.add(quad);
    }

    delete(quad) {
      return this.filtered.delete(quad);
    }

    has(quad) {
      return this.filtered.has(quad);
    }

    match(subject, predicate, object, graph) {
      return new DatasetCoreAndReadableStream(this.filtered, subject, predicate, object, graph);
    }

    *[Symbol.iterator]() {
      yield* this.filtered.getQuads();
    }
  }

  // **N3StreamParser** parses a text stream into a quad stream.

  // ## Constructor
  class N3StreamParser extends readableBrowser.Transform {
    constructor(options) {
      super({ decodeStrings: true });
      this._readableState.objectMode = true;

      // Set up parser with dummy stream to obtain `data` and `end` callbacks
      const parser = new N3Parser(options);
      let onData, onEnd;
      parser.parse({
        on: (event, callback) => {
          switch (event) {
          case 'data': onData = callback; break;
          case 'end':   onEnd = callback; break;
          }
        },
      },
        // Handle quads by pushing them down the pipeline
        (error, quad) => { error && this.emit('error', error) || quad && this.push(quad); },
        // Emit prefixes through the `prefix` event
        (prefix, uri) => { this.emit('prefix', prefix, uri); }
      );

      // Implement Transform methods through parser callbacks
      this._transform = (chunk, encoding, done) => { onData(chunk); done(); };
      this._flush = done => { onEnd(); done(); };
    }

    // ### Parses a stream of strings
    import(stream) {
      stream.on('data',  chunk => { this.write(chunk); });
      stream.on('end',   ()      => { this.end(); });
      stream.on('error', error => { this.emit('error', error); });
      return this;
    }
  }

  // **N3StreamWriter** serializes a quad stream into a text stream.

  // ## Constructor
  class N3StreamWriter extends readableBrowser.Transform {
    constructor(options) {
      super({ encoding: 'utf8', writableObjectMode: true });

      // Set up writer with a dummy stream object
      const writer = this._writer = new N3Writer({
        write: (quad, encoding, callback) => { this.push(quad); callback && callback(); },
        end: callback => { this.push(null); callback && callback(); },
      }, options);

      // Implement Transform methods on top of writer
      this._transform = (quad, encoding, done) => { writer.addQuad(quad, done); };
      this._flush = done => { writer.end(done); };
    }

  // ### Serializes a stream of quads
    import(stream) {
      stream.on('data',   quad => { this.write(quad); });
      stream.on('end',    () => { this.end(); });
      stream.on('error',  error => { this.emit('error', error); });
      stream.on('prefix', (prefix, iri) => { this._writer.addPrefix(prefix, iri); });
      return this;
    }
  }

  var index = /*#__PURE__*/Object.freeze({
    __proto__: null,
    Lexer: N3Lexer,
    Parser: N3Parser,
    Writer: N3Writer,
    Store: N3Store,
    StreamParser: N3StreamParser,
    StreamWriter: N3StreamWriter,
    Util: N3Util,
    DataFactory: N3DataFactory,
    Term: Term,
    NamedNode: NamedNode,
    Literal: Literal,
    BlankNode: BlankNode,
    Variable: Variable,
    DefaultGraph: DefaultGraph,
    Quad: Quad,
    Triple: Quad,
    termFromId: termFromId,
    termToId: termToId
  });

  exports.FetchError = FetchError;
  exports.SolidClientError = SolidClientError;
  exports.ThingExpectedError = ThingExpectedError;
  exports.access = universal;
  exports.access_v1 = universal_v1;
  exports.access_v2 = universal_v2;
  exports.acp_v1 = acp_v1;
  exports.acp_v2 = acp_v2;
  exports.acp_v3 = acp_v3;
  exports.acp_v4 = acp_v4;
  exports.addBoolean = addBoolean;
  exports.addDate = addDate;
  exports.addDatetime = addDatetime;
  exports.addDecimal = addDecimal;
  exports.addInteger = addInteger;
  exports.addIri = addIri;
  exports.addLiteral = addLiteral;
  exports.addMockFallbackAclTo = addMockFallbackAclTo;
  exports.addMockResourceAclTo = addMockResourceAclTo;
  exports.addNamedNode = addNamedNode;
  exports.addStringNoLocale = addStringNoLocale;
  exports.addStringWithLocale = addStringWithLocale;
  exports.addTerm = addTerm;
  exports.addTime = addTime;
  exports.addUrl = addUrl;
  exports.asIri = asIri;
  exports.asUrl = asUrl;
  exports.buildThing = buildThing;
  exports.changeLogAsMarkdown = changeLogAsMarkdown;
  exports.createAcl = createAcl;
  exports.createAclFromFallbackAcl = createAclFromFallbackAcl;
  exports.createContainerAt = createContainerAt;
  exports.createContainerInContainer = createContainerInContainer;
  exports.createSolidDataset = createSolidDataset;
  exports.createThing = createThing;
  exports.deleteAclFor = deleteAclFor;
  exports.deleteContainer = deleteContainer;
  exports.deleteFile = deleteFile;
  exports.deleteSolidDataset = deleteSolidDataset;
  exports.fromRdfJsDataset = fromRdfJsDataset;
  exports.getAgentAccess = getAgentAccess$3;
  exports.getAgentAccessAll = getAgentAccessAll$3;
  exports.getAgentDefaultAccess = getAgentDefaultAccess;
  exports.getAgentDefaultAccessAll = getAgentDefaultAccessAll;
  exports.getAgentResourceAccess = getAgentResourceAccess;
  exports.getAgentResourceAccessAll = getAgentResourceAccessAll;
  exports.getBoolean = getBoolean;
  exports.getBooleanAll = getBooleanAll;
  exports.getContainedResourceUrlAll = getContainedResourceUrlAll;
  exports.getContentType = getContentType$1;
  exports.getDate = getDate;
  exports.getDateAll = getDateAll;
  exports.getDatetime = getDatetime;
  exports.getDatetimeAll = getDatetimeAll;
  exports.getDecimal = getDecimal;
  exports.getDecimalAll = getDecimalAll;
  exports.getEffectiveAccess = getEffectiveAccess;
  exports.getFallbackAcl = getFallbackAcl;
  exports.getFile = getFile;
  exports.getFileWithAcl = getFileWithAcl;
  exports.getGroupAccess = getGroupAccess$2;
  exports.getGroupAccessAll = getGroupAccessAll$2;
  exports.getGroupDefaultAccess = getGroupDefaultAccess;
  exports.getGroupDefaultAccessAll = getGroupDefaultAccessAll;
  exports.getGroupResourceAccess = getGroupResourceAccess;
  exports.getGroupResourceAccessAll = getGroupResourceAccessAll;
  exports.getInteger = getInteger;
  exports.getIntegerAll = getIntegerAll;
  exports.getIri = getIri;
  exports.getIriAll = getIriAll;
  exports.getLinkedResourceUrlAll = getLinkedResourceUrlAll;
  exports.getLiteral = getLiteral;
  exports.getLiteralAll = getLiteralAll;
  exports.getNamedNode = getNamedNode;
  exports.getNamedNodeAll = getNamedNodeAll;
  exports.getPodOwner = getPodOwner;
  exports.getPropertyAll = getPropertyAll;
  exports.getPublicAccess = getPublicAccess$3;
  exports.getPublicDefaultAccess = getPublicDefaultAccess;
  exports.getPublicResourceAccess = getPublicResourceAccess;
  exports.getResourceAcl = getResourceAcl;
  exports.getResourceInfo = getResourceInfo;
  exports.getResourceInfoWithAcl = getResourceInfoWithAcl;
  exports.getSolidDataset = getSolidDataset;
  exports.getSolidDatasetWithAcl = getSolidDatasetWithAcl;
  exports.getSourceIri = getSourceIri;
  exports.getSourceUrl = getSourceUrl;
  exports.getStringByLocaleAll = getStringByLocaleAll;
  exports.getStringNoLocale = getStringNoLocale;
  exports.getStringNoLocaleAll = getStringNoLocaleAll;
  exports.getStringWithLocale = getStringWithLocale;
  exports.getStringWithLocaleAll = getStringWithLocaleAll;
  exports.getTerm = getTerm;
  exports.getTermAll = getTermAll;
  exports.getThing = getThing;
  exports.getThingAll = getThingAll;
  exports.getTime = getTime;
  exports.getTimeAll = getTimeAll;
  exports.getUrl = getUrl;
  exports.getUrlAll = getUrlAll;
  exports.hasAccessibleAcl = hasAccessibleAcl;
  exports.hasAcl = hasAcl;
  exports.hasFallbackAcl = hasFallbackAcl;
  exports.hasResourceAcl = hasResourceAcl;
  exports.hasResourceInfo = hasResourceInfo;
  exports.hasServerResourceInfo = hasServerResourceInfo;
  exports.isContainer = isContainer;
  exports.isPodOwner = isPodOwner;
  exports.isRawData = isRawData;
  exports.isThing = isThing;
  exports.isThingLocal = isThingLocal;
  exports.mockContainerFrom = mockContainerFrom;
  exports.mockFetchError = mockFetchError;
  exports.mockFileFrom = mockFileFrom;
  exports.mockSolidDatasetFrom = mockSolidDatasetFrom;
  exports.mockThingFrom = mockThingFrom;
  exports.overwriteFile = overwriteFile;
  exports.removeAll = removeAll;
  exports.removeBoolean = removeBoolean;
  exports.removeDate = removeDate;
  exports.removeDatetime = removeDatetime;
  exports.removeDecimal = removeDecimal;
  exports.removeInteger = removeInteger;
  exports.removeIri = removeIri;
  exports.removeLiteral = removeLiteral;
  exports.removeNamedNode = removeNamedNode;
  exports.removeStringNoLocale = removeStringNoLocale;
  exports.removeStringWithLocale = removeStringWithLocale;
  exports.removeThing = removeThing;
  exports.removeTime = removeTime;
  exports.removeUrl = removeUrl;
  exports.responseToResourceInfo = responseToResourceInfo;
  exports.responseToSolidDataset = responseToSolidDataset;
  exports.saveAclFor = saveAclFor;
  exports.saveFileInContainer = saveFileInContainer;
  exports.saveSolidDatasetAt = saveSolidDatasetAt;
  exports.saveSolidDatasetInContainer = saveSolidDatasetInContainer;
  exports.setAgentDefaultAccess = setAgentDefaultAccess;
  exports.setAgentResourceAccess = setAgentResourceAccess$1;
  exports.setBoolean = setBoolean;
  exports.setDate = setDate;
  exports.setDatetime = setDatetime;
  exports.setDecimal = setDecimal;
  exports.setGroupDefaultAccess = setGroupDefaultAccess;
  exports.setGroupResourceAccess = setGroupResourceAccess$1;
  exports.setInteger = setInteger;
  exports.setIri = setIri;
  exports.setLiteral = setLiteral;
  exports.setNamedNode = setNamedNode;
  exports.setPublicDefaultAccess = setPublicDefaultAccess;
  exports.setPublicResourceAccess = setPublicResourceAccess$1;
  exports.setStringNoLocale = setStringNoLocale;
  exports.setStringWithLocale = setStringWithLocale;
  exports.setTerm = setTerm;
  exports.setThing = setThing;
  exports.setTime = setTime;
  exports.setUrl = setUrl;
  exports.solidDatasetAsMarkdown = solidDatasetAsMarkdown;
  exports.thingAsMarkdown = thingAsMarkdown;
  exports.toRdfJsDataset = toRdfJsDataset;

  Object.defineProperty(exports, '__esModule', { value: true });

  return exports;

}({}));
