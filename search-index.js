var searchIndex = {};
searchIndex["toml_document"] = {"doc":"","items":[[3,"Parser","toml_document","Parser for converting a string to a TOML `Value` instance.",null,null],[12,"errors","","A list of all errors which have occurred during parsing.",0,null],[3,"ParserError","","A structure representing a parse error.",null,null],[12,"lo","","The low byte at which this error is pointing at.",1,null],[12,"hi","","One byte beyond the last character at which this error is pointing at.",1,null],[12,"desc","","A human-readable description explaining what the error is.",1,null],[3,"DirectChildren","","",null,null],[3,"Containers","","",null,null],[3,"DirectChild","","",null,null],[3,"ContainerKeysMarkup","","",null,null],[3,"BoolValue","","",null,null],[3,"StringValue","","",null,null],[3,"FloatValue","","",null,null],[3,"IntegerValue","","",null,null],[3,"DatetimeValue","","",null,null],[3,"InlineArray","","",null,null],[3,"InlineArrayMarkup","","",null,null],[3,"Values","","",null,null],[3,"TableEntry","","",null,null],[3,"ArrayEntry","","",null,null],[3,"InlineTable","","",null,null],[3,"TableKeyMarkup","","",null,null],[3,"KeyMarkup","","",null,null],[3,"Document","","",null,null],[3,"ValueMarkup","","",null,null],[3,"Container","","",null,null],[4,"ValueRef","","",null,null],[13,"String","","",2,null],[13,"Integer","","",2,null],[13,"Float","","",2,null],[13,"Boolean","","",2,null],[13,"Datetime","","",2,null],[13,"Array","","",2,null],[13,"Table","","",2,null],[4,"EntryRef","","",null,null],[13,"String","","",3,null],[13,"Integer","","",3,null],[13,"Float","","",3,null],[13,"Boolean","","",3,null],[13,"Datetime","","",3,null],[13,"Array","","",3,null],[13,"Table","","",3,null],[4,"TableValue","","",null,null],[13,"Inline","","",4,null],[13,"Implicit","","",4,null],[13,"Explicit","","",4,null],[4,"ArrayValue","","",null,null],[13,"Inline","","",5,null],[13,"OfTables","","",5,null],[4,"ContainerKind","","",null,null],[13,"Table","","",6,null],[13,"ArrayMember","","",6,null],[11,"fmt","","",1,{"inputs":[{"name":"parsererror"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"new","","Creates a new parser for a string.",0,{"inputs":[{"name":"str"}],"output":{"name":"parser"}}],[11,"to_linecol","","Converts a byte offset from an error message to a (line, column) pair",0,null],[11,"parse","","TODO: write something here",0,{"inputs":[{"name":"parser"}],"output":{"name":"option"}}],[11,"description","","",1,{"inputs":[{"name":"parsererror"}],"output":{"name":"str"}}],[11,"fmt","","",1,{"inputs":[{"name":"parsererror"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"new","","",7,{"inputs":[],"output":{"name":"document"}}],[11,"get","","",7,{"inputs":[{"name":"document"},{"name":"str"}],"output":{"name":"option"}}],[11,"len","","",7,{"inputs":[{"name":"document"}],"output":{"name":"usize"}}],[11,"iter","","",7,{"inputs":[{"name":"document"}],"output":{"name":"box"}}],[11,"get_child","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":{"name":"directchild"}}],[11,"len_children","","",7,{"inputs":[{"name":"document"}],"output":{"name":"usize"}}],[11,"iter_children","","",7,{"inputs":[{"name":"document"}],"output":{"name":"directchildren"}}],[11,"get_container","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":{"name":"container"}}],[11,"len_containers","","",7,{"inputs":[{"name":"document"}],"output":{"name":"usize"}}],[11,"iter_containers","","",7,{"inputs":[{"name":"document"}],"output":{"name":"containers"}}],[11,"get_trailing_trivia","","",7,{"inputs":[{"name":"document"}],"output":{"name":"str"}}],[11,"insert_string","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"stringvalue"}}],[11,"insert_integer","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"i64"}],"output":{"name":"integervalue"}}],[11,"insert_float","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"f64"}],"output":{"name":"floatvalue"}}],[11,"insert_boolean","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"bool"}],"output":{"name":"boolvalue"}}],[11,"insert_datetime","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"datetimevalue"}}],[11,"insert_array","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinearray"}}],[11,"insert_inline_table","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinetable"}}],[11,"insert_container","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"k"},{"name":"containerkind"}],"output":{"name":"container"}}],[11,"find","","",7,{"inputs":[{"name":"document"},{"name":"t"}],"output":{"name":"option"}}],[11,"remove","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":null}],[11,"remove_preserve_trivia","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":null}],[11,"next","","",8,{"inputs":[{"name":"directchildren"}],"output":{"name":"option"}}],[11,"next","","",9,{"inputs":[{"name":"containers"}],"output":{"name":"option"}}],[11,"key","","",10,{"inputs":[{"name":"directchild"}],"output":{"name":"keymarkup"}}],[11,"value","","",10,{"inputs":[{"name":"directchild"}],"output":{"name":"valueref"}}],[11,"clone","","",2,{"inputs":[{"name":"valueref"}],"output":{"name":"valueref"}}],[11,"to_entry","","",2,{"inputs":[{"name":"valueref"}],"output":{"name":"entryref"}}],[11,"kind","","",11,{"inputs":[{"name":"container"}],"output":{"name":"containerkind"}}],[11,"get","","",11,{"inputs":[{"name":"container"},{"name":"str"}],"output":{"name":"option"}}],[11,"len_entries","","",11,{"inputs":[{"name":"container"}],"output":{"name":"usize"}}],[11,"iter_entries","","",11,{"inputs":[{"name":"container"}],"output":{"name":"box"}}],[11,"get_child","","",11,{"inputs":[{"name":"container"},{"name":"usize"}],"output":{"name":"directchild"}}],[11,"len_children","","",11,{"inputs":[{"name":"container"}],"output":{"name":"usize"}}],[11,"iter_children","","",11,{"inputs":[{"name":"container"}],"output":{"name":"directchildren"}}],[11,"keys","","",11,{"inputs":[{"name":"container"}],"output":{"name":"containerkeysmarkup"}}],[11,"insert_string","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"stringvalue"}}],[11,"insert_integer","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"i64"}],"output":{"name":"integervalue"}}],[11,"insert_float","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"f64"}],"output":{"name":"floatvalue"}}],[11,"insert_boolean","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"bool"}],"output":{"name":"boolvalue"}}],[11,"insert_datetime","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"datetimevalue"}}],[11,"insert_array","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinearray"}}],[11,"insert_inline_table","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinetable"}}],[11,"find","","",11,{"inputs":[{"name":"container"},{"name":"t"}],"output":{"name":"option"}}],[11,"remove","","",11,{"inputs":[{"name":"container"},{"name":"usize"}],"output":null}],[11,"to_entry","","",11,{"inputs":[{"name":"container"}],"output":{"name":"entryref"}}],[11,"get_leading_trivia","","",12,{"inputs":[{"name":"containerkeysmarkup"}],"output":{"name":"str"}}],[11,"markup","","",12,null],[11,"get_trailing_trivia","","",12,{"inputs":[{"name":"containerkeysmarkup"}],"output":{"name":"str"}}],[11,"clone","","",3,{"inputs":[{"name":"entryref"}],"output":{"name":"entryref"}}],[11,"is_child","","",3,{"inputs":[{"name":"entryref"}],"output":{"name":"bool"}}],[11,"markup","","",13,{"inputs":[{"name":"boolvalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",13,{"inputs":[{"name":"boolvalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",13,{"inputs":[{"name":"boolvalue"}],"output":{"name":"bool"}}],[11,"set","","",13,{"inputs":[{"name":"boolvalue"},{"name":"bool"}],"output":null}],[11,"to_entry","","",13,{"inputs":[{"name":"boolvalue"}],"output":{"name":"entryref"}}],[11,"markup","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"str"}}],[11,"set","","",14,{"inputs":[{"name":"stringvalue"},{"name":"string"}],"output":null}],[11,"raw","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"str"}}],[11,"to_entry","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"entryref"}}],[11,"markup","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"f64"}}],[11,"set","","",15,{"inputs":[{"name":"floatvalue"},{"name":"f64"}],"output":null}],[11,"raw","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"str"}}],[11,"to_entry","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"entryref"}}],[11,"markup","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"i64"}}],[11,"set","","",16,{"inputs":[{"name":"integervalue"},{"name":"i64"}],"output":null}],[11,"raw","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"str"}}],[11,"to_entry","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"entryref"}}],[11,"markup","","",17,{"inputs":[{"name":"datetimevalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",17,{"inputs":[{"name":"datetimevalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",17,{"inputs":[{"name":"datetimevalue"}],"output":{"name":"str"}}],[11,"set","","",17,{"inputs":[{"name":"datetimevalue"},{"name":"string"}],"output":null}],[11,"to_entry","","",17,{"inputs":[{"name":"datetimevalue"}],"output":{"name":"entryref"}}],[11,"markup","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"inlinearraymarkup"}}],[11,"markup_mut","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"inlinearraymarkup"}}],[11,"len","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"usize"}}],[11,"get","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"}],"output":{"name":"valueref"}}],[11,"iter","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"values"}}],[11,"insert_string","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"},{"name":"s"}],"output":{"name":"stringvalue"}}],[11,"insert_integer","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"},{"name":"i64"}],"output":{"name":"integervalue"}}],[11,"insert_float","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"},{"name":"f64"}],"output":{"name":"floatvalue"}}],[11,"insert_boolean","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"},{"name":"bool"}],"output":{"name":"boolvalue"}}],[11,"insert_datetime","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"},{"name":"s"}],"output":{"name":"datetimevalue"}}],[11,"insert_array","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"}],"output":{"name":"inlinearray"}}],[11,"insert_inline_table","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"}],"output":{"name":"inlinetable"}}],[11,"remove","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"}],"output":null}],[11,"find","","",18,{"inputs":[{"name":"inlinearray"},{"name":"t"}],"output":{"name":"option"}}],[11,"to_entry","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"entryref"}}],[11,"get_leading_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"},{"name":"string"}],"output":null}],[11,"get_comma_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"}],"output":{"name":"str"}}],[11,"next","","",20,{"inputs":[{"name":"values"}],"output":{"name":"option"}}],[11,"clone","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"tableentry"}}],[11,"get","","",21,{"inputs":[{"name":"tableentry"},{"name":"str"}],"output":{"name":"option"}}],[11,"len_children","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"usize"}}],[11,"iter_children","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"directchildren"}}],[11,"len","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"usize"}}],[11,"iter","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"box"}}],[11,"to_value","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"tablevalue"}}],[11,"to_entry","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"entryref"}}],[11,"clone","","",4,{"inputs":[{"name":"tablevalue"}],"output":{"name":"tablevalue"}}],[11,"clone","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"arrayentry"}}],[11,"clone","","",5,{"inputs":[{"name":"arrayvalue"}],"output":{"name":"arrayvalue"}}],[11,"to_value","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"arrayvalue"}}],[11,"len","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"usize"}}],[11,"get","","",22,{"inputs":[{"name":"arrayentry"},{"name":"usize"}],"output":{"name":"entryref"}}],[11,"iter","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"box"}}],[11,"to_entry","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"entryref"}}],[11,"get","","",23,{"inputs":[{"name":"inlinetable"},{"name":"str"}],"output":{"name":"option"}}],[11,"len","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"usize"}}],[11,"iter","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"directchildren"}}],[11,"get_child","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"}],"output":{"name":"directchild"}}],[11,"markup","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"valuemarkup"}}],[11,"get_comma_trivia","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"str"}}],[11,"insert_string","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"stringvalue"}}],[11,"insert_integer","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"i64"}],"output":{"name":"integervalue"}}],[11,"insert_float","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"f64"}],"output":{"name":"floatvalue"}}],[11,"insert_boolean","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"bool"}],"output":{"name":"boolvalue"}}],[11,"insert_datetime","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"datetimevalue"}}],[11,"insert_array","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinearray"}}],[11,"insert_inline_table","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinetable"}}],[11,"find","","",23,{"inputs":[{"name":"inlinetable"},{"name":"t"}],"output":{"name":"option"}}],[11,"remove","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"}],"output":null}],[11,"to_entry","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"tableentry"}}],[11,"get","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"get_leading_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"},{"name":"string"}],"output":null}],[11,"raw","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"get","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"get_leading_trivia","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",25,{"inputs":[{"name":"keymarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",25,{"inputs":[{"name":"keymarkup"},{"name":"string"}],"output":null}],[11,"raw","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"get_leading_trivia","","",26,{"inputs":[{"name":"valuemarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",26,{"inputs":[{"name":"valuemarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",26,{"inputs":[{"name":"valuemarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",26,{"inputs":[{"name":"valuemarkup"},{"name":"string"}],"output":null}],[11,"ptr","","",2,{"inputs":[{"name":"valueref"}],"output":{"name":"usize"}}],[11,"fmt","","",7,{"inputs":[{"name":"document"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",10,{"inputs":[{"name":"directchild"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",2,{"inputs":[{"name":"valueref"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",14,{"inputs":[{"name":"stringvalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",16,{"inputs":[{"name":"integervalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",13,{"inputs":[{"name":"boolvalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",17,{"inputs":[{"name":"datetimevalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",15,{"inputs":[{"name":"floatvalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",18,{"inputs":[{"name":"inlinearray"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",23,{"inputs":[{"name":"inlinetable"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",25,{"inputs":[{"name":"keymarkup"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",24,{"inputs":[{"name":"tablekeymarkup"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",11,{"inputs":[{"name":"container"},{"name":"formatter"}],"output":{"name":"result"}}],[8,"InternalNode","","",null,null],[11,"ptr","","",27,{"inputs":[{"name":"internalnode"}],"output":{"name":"usize"}}],[11,"clone","","",6,{"inputs":[{"name":"containerkind"}],"output":{"name":"containerkind"}}],[11,"hash","","",6,null],[11,"eq","","",6,{"inputs":[{"name":"containerkind"},{"name":"containerkind"}],"output":{"name":"bool"}}],[11,"ne","","",6,{"inputs":[{"name":"containerkind"},{"name":"containerkind"}],"output":{"name":"bool"}}],[11,"ptr","","",27,{"inputs":[{"name":"internalnode"}],"output":{"name":"usize"}}]],"paths":[[3,"Parser"],[3,"ParserError"],[4,"ValueRef"],[4,"EntryRef"],[4,"TableValue"],[4,"ArrayValue"],[4,"ContainerKind"],[3,"Document"],[3,"DirectChildren"],[3,"Containers"],[3,"DirectChild"],[3,"Container"],[3,"ContainerKeysMarkup"],[3,"BoolValue"],[3,"StringValue"],[3,"FloatValue"],[3,"IntegerValue"],[3,"DatetimeValue"],[3,"InlineArray"],[3,"InlineArrayMarkup"],[3,"Values"],[3,"TableEntry"],[3,"ArrayEntry"],[3,"InlineTable"],[3,"TableKeyMarkup"],[3,"KeyMarkup"],[3,"ValueMarkup"],[8,"InternalNode"]]};
searchIndex['toml_document'] = {"items":[[3,"Parser","toml_document","Parser for converting a string to a TOML `Value` instance.",null,null],[12,"errors","","A list of all errors which have occurred during parsing.",0,null],[3,"ParserError","","A structure representing a parse error.",null,null],[12,"lo","","The low byte at which this error is pointing at.",1,null],[12,"hi","","One byte beyond the last character at which this error is pointing at.",1,null],[12,"desc","","A human-readable description explaining what the error is.",1,null],[3,"DirectChildren","","",null,null],[3,"Containers","","",null,null],[3,"DirectChild","","",null,null],[3,"ContainerKeysMarkup","","",null,null],[3,"BoolValue","","",null,null],[3,"StringValue","","",null,null],[3,"FloatValue","","",null,null],[3,"IntegerValue","","",null,null],[3,"DatetimeValue","","",null,null],[3,"InlineArray","","",null,null],[3,"InlineArrayMarkup","","",null,null],[3,"Values","","",null,null],[3,"TableEntry","","",null,null],[3,"ArrayEntry","","",null,null],[3,"InlineTable","","",null,null],[3,"TableKeyMarkup","","",null,null],[3,"KeyMarkup","","",null,null],[3,"Document","","",null,null],[3,"ValueMarkup","","",null,null],[3,"Container","","",null,null],[4,"ValueRef","","",null,null],[13,"String","","",2,null],[13,"Integer","","",2,null],[13,"Float","","",2,null],[13,"Boolean","","",2,null],[13,"Datetime","","",2,null],[13,"Array","","",2,null],[13,"Table","","",2,null],[4,"EntryRef","","",null,null],[13,"String","","",3,null],[13,"Integer","","",3,null],[13,"Float","","",3,null],[13,"Boolean","","",3,null],[13,"Datetime","","",3,null],[13,"Array","","",3,null],[13,"Table","","",3,null],[4,"TableValue","","",null,null],[13,"Inline","","",4,null],[13,"Implicit","","",4,null],[13,"Explicit","","",4,null],[4,"ArrayValue","","",null,null],[13,"Inline","","",5,null],[13,"OfTables","","",5,null],[4,"ContainerKind","","",null,null],[13,"Table","","",6,null],[13,"ArrayMember","","",6,null],[11,"fmt","","",1,{"inputs":[{"name":"parsererror"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"new","","Creates a new parser for a string.",0,{"inputs":[{"name":"parser"},{"name":"str"}],"output":{"name":"parser"}}],[11,"to_linecol","","Converts a byte offset from an error message to a (line, column) pair",0,null],[11,"parse","","TODO: write something here",0,{"inputs":[{"name":"parser"}],"output":{"name":"option"}}],[11,"description","","",1,{"inputs":[{"name":"parsererror"}],"output":{"name":"str"}}],[11,"fmt","","",1,{"inputs":[{"name":"parsererror"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"new","","",7,{"inputs":[{"name":"document"}],"output":{"name":"document"}}],[11,"get","","",7,{"inputs":[{"name":"document"},{"name":"str"}],"output":{"name":"option"}}],[11,"len","","",7,{"inputs":[{"name":"document"}],"output":{"name":"usize"}}],[11,"iter","","",7,{"inputs":[{"name":"document"}],"output":{"name":"box"}}],[11,"get_child","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":{"name":"directchild"}}],[11,"len_children","","",7,{"inputs":[{"name":"document"}],"output":{"name":"usize"}}],[11,"iter_children","","",7,{"inputs":[{"name":"document"}],"output":{"name":"directchildren"}}],[11,"get_container","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":{"name":"container"}}],[11,"len_containers","","",7,{"inputs":[{"name":"document"}],"output":{"name":"usize"}}],[11,"iter_containers","","",7,{"inputs":[{"name":"document"}],"output":{"name":"containers"}}],[11,"get_trailing_trivia","","",7,{"inputs":[{"name":"document"}],"output":{"name":"str"}}],[11,"insert_string","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"stringvalue"}}],[11,"insert_integer","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"i64"}],"output":{"name":"integervalue"}}],[11,"insert_float","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"f64"}],"output":{"name":"floatvalue"}}],[11,"insert_boolean","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"bool"}],"output":{"name":"boolvalue"}}],[11,"insert_datetime","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"datetimevalue"}}],[11,"insert_array","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinearray"}}],[11,"insert_inline_table","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinetable"}}],[11,"insert_container","","",7,{"inputs":[{"name":"document"},{"name":"usize"},{"name":"k"},{"name":"containerkind"}],"output":{"name":"container"}}],[11,"find","","",7,{"inputs":[{"name":"document"},{"name":"t"}],"output":{"name":"option"}}],[11,"remove","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":null}],[11,"remove_preserve_trivia","","",7,{"inputs":[{"name":"document"},{"name":"usize"}],"output":null}],[11,"next","","",8,{"inputs":[{"name":"directchildren"}],"output":{"name":"option"}}],[11,"next","","",9,{"inputs":[{"name":"containers"}],"output":{"name":"option"}}],[11,"key","","",10,{"inputs":[{"name":"directchild"}],"output":{"name":"keymarkup"}}],[11,"value","","",10,{"inputs":[{"name":"directchild"}],"output":{"name":"valueref"}}],[11,"clone","","",2,{"inputs":[{"name":"valueref"}],"output":{"name":"valueref"}}],[11,"to_entry","","",2,{"inputs":[{"name":"valueref"}],"output":{"name":"entryref"}}],[11,"kind","","",11,{"inputs":[{"name":"container"}],"output":{"name":"containerkind"}}],[11,"get","","",11,{"inputs":[{"name":"container"},{"name":"str"}],"output":{"name":"option"}}],[11,"len_entries","","",11,{"inputs":[{"name":"container"}],"output":{"name":"usize"}}],[11,"iter_entries","","",11,{"inputs":[{"name":"container"}],"output":{"name":"box"}}],[11,"get_child","","",11,{"inputs":[{"name":"container"},{"name":"usize"}],"output":{"name":"directchild"}}],[11,"len_children","","",11,{"inputs":[{"name":"container"}],"output":{"name":"usize"}}],[11,"iter_children","","",11,{"inputs":[{"name":"container"}],"output":{"name":"directchildren"}}],[11,"keys","","",11,{"inputs":[{"name":"container"}],"output":{"name":"containerkeysmarkup"}}],[11,"insert_string","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"stringvalue"}}],[11,"insert_integer","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"i64"}],"output":{"name":"integervalue"}}],[11,"insert_float","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"f64"}],"output":{"name":"floatvalue"}}],[11,"insert_boolean","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"bool"}],"output":{"name":"boolvalue"}}],[11,"insert_datetime","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"datetimevalue"}}],[11,"insert_array","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinearray"}}],[11,"insert_inline_table","","",11,{"inputs":[{"name":"container"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinetable"}}],[11,"find","","",11,{"inputs":[{"name":"container"},{"name":"t"}],"output":{"name":"option"}}],[11,"remove","","",11,{"inputs":[{"name":"container"},{"name":"usize"}],"output":null}],[11,"to_entry","","",11,{"inputs":[{"name":"container"}],"output":{"name":"entryref"}}],[11,"get_leading_trivia","","",12,{"inputs":[{"name":"containerkeysmarkup"}],"output":{"name":"str"}}],[11,"markup","","",12,null],[11,"get_trailing_trivia","","",12,{"inputs":[{"name":"containerkeysmarkup"}],"output":{"name":"str"}}],[11,"clone","","",3,{"inputs":[{"name":"entryref"}],"output":{"name":"entryref"}}],[11,"is_child","","",3,{"inputs":[{"name":"entryref"}],"output":{"name":"bool"}}],[11,"markup","","",13,{"inputs":[{"name":"boolvalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",13,{"inputs":[{"name":"boolvalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",13,{"inputs":[{"name":"boolvalue"}],"output":{"name":"bool"}}],[11,"set","","",13,{"inputs":[{"name":"boolvalue"},{"name":"bool"}],"output":null}],[11,"markup","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"str"}}],[11,"set","","",14,{"inputs":[{"name":"stringvalue"},{"name":"string"}],"output":null}],[11,"raw","","",14,{"inputs":[{"name":"stringvalue"}],"output":{"name":"str"}}],[11,"markup","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"f64"}}],[11,"set","","",15,{"inputs":[{"name":"floatvalue"},{"name":"f64"}],"output":null}],[11,"raw","","",15,{"inputs":[{"name":"floatvalue"}],"output":{"name":"str"}}],[11,"markup","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"i64"}}],[11,"set","","",16,{"inputs":[{"name":"integervalue"},{"name":"i64"}],"output":null}],[11,"raw","","",16,{"inputs":[{"name":"integervalue"}],"output":{"name":"str"}}],[11,"markup","","",17,{"inputs":[{"name":"datetimevalue"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",17,{"inputs":[{"name":"datetimevalue"}],"output":{"name":"valuemarkup"}}],[11,"get","","",17,{"inputs":[{"name":"datetimevalue"}],"output":{"name":"str"}}],[11,"set","","",17,{"inputs":[{"name":"datetimevalue"},{"name":"string"}],"output":null}],[11,"markup","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"inlinearraymarkup"}}],[11,"markup_mut","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"inlinearraymarkup"}}],[11,"len","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"usize"}}],[11,"get","","",18,{"inputs":[{"name":"inlinearray"},{"name":"usize"}],"output":{"name":"valueref"}}],[11,"iter","","",18,{"inputs":[{"name":"inlinearray"}],"output":{"name":"values"}}],[11,"get_leading_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"},{"name":"string"}],"output":null}],[11,"get_comma_trivia","","",19,{"inputs":[{"name":"inlinearraymarkup"}],"output":{"name":"str"}}],[11,"next","","",20,{"inputs":[{"name":"values"}],"output":{"name":"option"}}],[11,"clone","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"tableentry"}}],[11,"get","","",21,{"inputs":[{"name":"tableentry"},{"name":"str"}],"output":{"name":"option"}}],[11,"len_children","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"usize"}}],[11,"iter_children","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"directchildren"}}],[11,"len","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"usize"}}],[11,"iter","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"box"}}],[11,"to_value","","",21,{"inputs":[{"name":"tableentry"}],"output":{"name":"tablevalue"}}],[11,"clone","","",4,{"inputs":[{"name":"tablevalue"}],"output":{"name":"tablevalue"}}],[11,"clone","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"arrayentry"}}],[11,"clone","","",5,{"inputs":[{"name":"arrayvalue"}],"output":{"name":"arrayvalue"}}],[11,"to_value","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"arrayvalue"}}],[11,"len","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"usize"}}],[11,"get","","",22,{"inputs":[{"name":"arrayentry"},{"name":"usize"}],"output":{"name":"entryref"}}],[11,"iter","","",22,{"inputs":[{"name":"arrayentry"}],"output":{"name":"box"}}],[11,"get","","",23,{"inputs":[{"name":"inlinetable"},{"name":"str"}],"output":{"name":"option"}}],[11,"len","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"usize"}}],[11,"iter","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"directchildren"}}],[11,"get_child","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"}],"output":{"name":"directchild"}}],[11,"markup","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"valuemarkup"}}],[11,"markup_mut","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"valuemarkup"}}],[11,"get_comma_trivia","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"str"}}],[11,"insert_string","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"stringvalue"}}],[11,"insert_integer","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"i64"}],"output":{"name":"integervalue"}}],[11,"insert_float","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"f64"}],"output":{"name":"floatvalue"}}],[11,"insert_boolean","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"bool"}],"output":{"name":"boolvalue"}}],[11,"insert_datetime","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"},{"name":"s"}],"output":{"name":"datetimevalue"}}],[11,"insert_array","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinearray"}}],[11,"insert_inline_table","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"},{"name":"s"}],"output":{"name":"inlinetable"}}],[11,"find","","",23,{"inputs":[{"name":"inlinetable"},{"name":"t"}],"output":{"name":"option"}}],[11,"remove","","",23,{"inputs":[{"name":"inlinetable"},{"name":"usize"}],"output":null}],[11,"to_entry","","",23,{"inputs":[{"name":"inlinetable"}],"output":{"name":"tableentry"}}],[11,"get","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"get_leading_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",24,{"inputs":[{"name":"tablekeymarkup"},{"name":"string"}],"output":null}],[11,"raw","","",24,{"inputs":[{"name":"tablekeymarkup"}],"output":{"name":"str"}}],[11,"get","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"get_leading_trivia","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",25,{"inputs":[{"name":"keymarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",25,{"inputs":[{"name":"keymarkup"},{"name":"string"}],"output":null}],[11,"raw","","",25,{"inputs":[{"name":"keymarkup"}],"output":{"name":"str"}}],[11,"get_leading_trivia","","",26,{"inputs":[{"name":"valuemarkup"}],"output":{"name":"str"}}],[11,"set_leading_trivia","","",26,{"inputs":[{"name":"valuemarkup"},{"name":"string"}],"output":null}],[11,"get_trailing_trivia","","",26,{"inputs":[{"name":"valuemarkup"}],"output":{"name":"str"}}],[11,"set_trailing_trivia","","",26,{"inputs":[{"name":"valuemarkup"},{"name":"string"}],"output":null}],[11,"ptr","","",2,{"inputs":[{"name":"valueref"}],"output":{"name":"usize"}}],[11,"fmt","","",7,{"inputs":[{"name":"document"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",10,{"inputs":[{"name":"directchild"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",2,{"inputs":[{"name":"valueref"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",14,{"inputs":[{"name":"stringvalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",16,{"inputs":[{"name":"integervalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",13,{"inputs":[{"name":"boolvalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",17,{"inputs":[{"name":"datetimevalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",15,{"inputs":[{"name":"floatvalue"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",18,{"inputs":[{"name":"inlinearray"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",23,{"inputs":[{"name":"inlinetable"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",25,{"inputs":[{"name":"keymarkup"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",24,{"inputs":[{"name":"tablekeymarkup"},{"name":"formatter"}],"output":{"name":"result"}}],[11,"fmt","","",11,{"inputs":[{"name":"container"},{"name":"formatter"}],"output":{"name":"result"}}],[8,"InternalNode","","",null,null],[11,"ptr","","",27,{"inputs":[{"name":"internalnode"}],"output":{"name":"usize"}}],[11,"clone","","",6,{"inputs":[{"name":"containerkind"}],"output":{"name":"containerkind"}}],[11,"hash","","",6,null],[11,"eq","","",6,{"inputs":[{"name":"containerkind"},{"name":"containerkind"}],"output":{"name":"bool"}}],[11,"ne","","",6,{"inputs":[{"name":"containerkind"},{"name":"containerkind"}],"output":{"name":"bool"}}],[11,"ptr","","",27,{"inputs":[{"name":"internalnode"}],"output":{"name":"usize"}}]],"paths":[[3,"Parser"],[3,"ParserError"],[4,"ValueRef"],[4,"EntryRef"],[4,"TableValue"],[4,"ArrayValue"],[4,"ContainerKind"],[3,"Document"],[3,"DirectChildren"],[3,"Containers"],[3,"DirectChild"],[3,"Container"],[3,"ContainerKeysMarkup"],[3,"BoolValue"],[3,"StringValue"],[3,"FloatValue"],[3,"IntegerValue"],[3,"DatetimeValue"],[3,"InlineArray"],[3,"InlineArrayMarkup"],[3,"Values"],[3,"TableEntry"],[3,"ArrayEntry"],[3,"InlineTable"],[3,"TableKeyMarkup"],[3,"KeyMarkup"],[3,"ValueMarkup"],[8,"InternalNode"]]};
initSearch(searchIndex);
