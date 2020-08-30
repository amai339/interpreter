fun isPus c = String.isPrefix "push" c;
fun isSwa c = String.isPrefix "swap" c;
fun isQui c = String.isPrefix "quit" c;
fun isPop c = String.isPrefix "pop" c;
fun isAdd c = String.isPrefix "add" c;
fun isSub c = String.isPrefix "sub" c;
fun isMul c = String.isPrefix "mul" c;
fun isDiv c = String.isPrefix "div" c;
fun isRem c = String.isPrefix "rem" c;
fun isNeg c = String.isPrefix "neg" c;
fun isCat c = String.isPrefix "cat" c;
fun isAnd c = String.isPrefix "and" c;
fun isNot c = String.isPrefix "not" c;
fun isEql c = String.isPrefix "equa" c;
fun isLss c = String.isPrefix "less" c;
fun isBnd c = String.isPrefix "bind" c;
fun isOrr c = String.isPrefix "or" c;
fun isIf c = String.isPrefix "if" c;
fun isLet c = String.isPrefix "let" c;
fun isEnd c = String.isPrefix "end" c;
fun isFun c = String.isPrefix "fun" c;
fun isOut c = String.isPrefix "inOutFun" c;
fun isCal c = String.isPrefix "call" c;
fun isEmpty l =
    case l of
	[] => true
      | _ => false;
datatype t = I of int | S of string | B of string | N of string | U of string | E | LET;
val t = ":true:"
val f = ":false:"
val u = ":unit:"						   
fun isInt c =
    case c of
	I c=> true
      | _ => false;
fun isStr c =
    case c of
	S c=> true
      | _ => false;
fun isBool c =
    case c of
       B c => true
     | _ => false;
fun isName c =
    case c of
	N c => true
      | _ => false;
fun binded c =
    case c of
	E => false
      | _ => true; 
fun cName (N c) = c;
fun cInt (I c) = c;
fun cStr (S c) = c;
fun cBool (B c) = c;
fun findMap (c, []) = NONE
  | findMap (c,(x,y)::l) =
	if c = x then SOME(y)
	else findMap(c,l);
fun find (c,[]) = c
  | find (c, x::l) = 
   case (findMap(c,x)) of
	     NONE => find(c,l)
	   | SOME(value) => value
fun letPOP(LET::rest) = rest
   |letPOP(h::l) = letPOP(l)
   |letPOP ([]) =[]; 

fun eval ([],map,stack) = stack  
  | eval (str::l,map,stack)  =
    if isPus str then
	if substring(str,5,1) = "\""then eval(l,map,S(String.substring(str,6,size(str)-8))::stack)	    
	else if Char.isDigit(String.sub(str,5)) then
	    if String.isSubstring "." str then eval(l,map,E::stack)
	    else 
		 let
		     val SOME num = Int.fromString(String.extract(str,5,NONE))
		 in
		     eval(l,map,I(num)::stack)		 
		 end
	else if String.substring(str,5,1) = "-" andalso substring(str,6,1) <> "0" then
	    if String.isSubstring "." str then eval(l,map,E::stack)				   
	    else if Char.isDigit(String.sub(str,6)) then
		let
		    val SOME num =Int.fromString("~"^String.extract(str,6,NONE))
		in
		    eval(l,map,I(num)::stack)
		end
	    else eval(l,map,E::stack)
	else if String.substring(str,5,2) = "-0" then
	    let
		val SOME zero = Int.fromString("0")
	    in
		eval (l,map,I(zero)::stack)
	    end
	 else if String.isSubstring ":true:" str orelse String.isSubstring ":false:" str then
	    eval (l,map,B(String.substring(str,5,size(str)-6))::stack)	 
	else if String.isSubstring ":error:" str then eval(l,map,E::stack)
	else eval(l,map,N(String.substring(str,5,size(str)-6))::stack)
    else if isPop str then   
	if isEmpty stack then eval(l,map,E::stack)
	else let
	    val newStack = tl stack
	in
	    eval(l,map,newStack)
	end
    else if isAdd str then
	if length(stack) <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd(tl stack)
	    val rest = tl(tl stack)
			 
	in
	    case(one,two) of
		(I one, I two) =>eval(l,map,I(two + one)::rest)
	      | (N one, I two) =>
		if isInt(find(N one, map)) then eval(l,map,I(two + cInt(find(N one,map)))::rest)
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		if isInt(find(N two, map)) then eval(l,map,I(cInt(find(N two,map)) + one)::rest)	    
		else  eval(l,map,E::stack)
	      | (N one, N two) =>
		if isInt(find(N one,map)) andalso isInt(find(N one,map)) then
		    eval(l,map,I(cInt(find(N two, map)) + cInt(find(N one, map)))::rest)	
		else eval(l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack) 
	end		 
    else if isSub str then
	if length(stack) <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd(tl stack)
	    val rest = tl(tl stack)
	in
	    case(one,two) of
		(I one, I two) =>eval(l,map,I(two - one)::rest)
	      | (N one, I two) =>
		if isInt(find(N one, map)) then eval(l,map,I(two - cInt(find(N one,map)))::rest)
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		if isInt(find(N two, map)) then eval(l,map,I(cInt(find(N two,map)) - one)::rest)	    
		else  eval(l,map,E::stack)
	      | (N one, N two) =>
		if isInt(find(N one,map)) andalso isInt(find(N one,map)) then
		    eval(l,map,I(cInt(find(N two, map)) - cInt(find(N one, map)))::rest)	
		else eval(l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack) 
	end		 
    else if isMul str then
	if length(stack) <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd(tl stack)
	    val rest = tl(tl stack)
	in
	    case(one,two) of
		(I one, I two) =>eval(l,map,I(two * one)::rest)
	      | (N one, I two) =>
		if isInt(find(N one, map)) then eval(l,map,I(two * cInt(find(N one,map)))::rest)
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		if isInt(find(N two, map)) then eval(l,map,I(cInt(find(N two,map)) * one)::rest)	    
		else  eval(l,map,E::stack)
	      | (N one, N two) =>
		if isInt(find(N one,map)) andalso isInt(find(N one,map)) then
		    eval(l,map,I(cInt(find(N two, map)) * cInt(find(N one, map)))::rest)	
		else eval(l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack) 
	end		 
    else if isDiv str then
	if length stack <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	    case (one,two) of
		(I one, I two) =>
		if one = 0 then eval(l,map,E::stack)
		else eval(l,map,I(two div one)::rest)
	      | (N one, I two) =>
		if isInt(find(N one,map)) andalso cInt(find(N one,map)) <> 0 then
		    eval(l,map,I(two div cInt(find(N one, map)))::rest)
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		if isInt(find(N two,map)) andalso one  <> 0 then
		    eval(l,map,I(cInt(find(N two,map)) div one)::rest)
		else eval(l,map,E::stack)
	      | (N one, N two) =>
		if isInt(find(N one,map)) andalso isInt(find(N two,map)) andalso cInt(find(N one,map)) <> 0 then
		    eval(l,map,I(cInt(find(N two, map)) div cInt(find(N one, map)))::rest)
		else eval(l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack)
	end
    else if isRem str then
		if length stack <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	  case (one,two) of
	      (I one, I two) =>
		if one = 0 then eval(l,map,E::stack)
		else eval(l,map,I(two mod one)::rest)
	      | (N one, I two) =>
		if isInt(find(N one,map)) andalso cInt(find(N one,map)) <> 0 then
		    eval(l,map,I(two mod cInt(find(N one, map)))::rest)
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		if isInt(find(N two,map)) andalso one <> 0 then
		    eval(l,map,I(cInt(find(N two,map)) mod one)::rest)
		else eval(l,map,E::stack)
	      | (N one, N two) =>
		if isInt(find(N one,map)) andalso isInt(find(N two,map)) andalso cInt(find(N one,map)) <> 0 then
		    eval(l,map,I(cInt(find(N two, map)) mod cInt(find(N one, map)))::rest)
		else eval(l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack)
	end
    else if isNeg str then
	if isEmpty stack then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val rest = tl stack
	in
	    case one of
		(I one) => eval(l,map,I(one * ~1)::rest)
	      | (N one) =>
		if isInt(find(N one,map)) then eval(l,map,I(cInt(find(N one, map)) * ~1)::rest)
		else eval(l,map,E::stack)
	      | (_) => eval(l,map,E::stack)
	end		 
    else if isSwa str then
	if length stack <= 1 then eval (l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	    case (one,two) of
		(_,_) => eval(l,map,two::one::rest)
	end
    else if isCat str then
	if length stack <= 1 then eval (l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	    case (one,two) of
		(S one, S two) => eval(l,map,S(two ^ one)::rest)
	      | (S one, N two) =>
		if isStr(find(N two,map)) then eval(l,map,S(cStr(find(N two,map))^one)::rest)
		else eval(l,map,E::stack)
	      | (N one, S two) =>
		if isStr(find(N one,map)) then eval(l,map,S(two^cStr(find(N one,map)))::rest)
		else eval(l,map,E::stack)
	      | (N one, N two) =>
		if isStr(find(N one,map)) andalso isStr(find(N two,map)) then
		    eval(l,map,S(cStr(find(N two,map)) ^ cStr(find(N one,map)))::rest)
		else eval (l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack)
	end
    else if isAnd str then
	if length stack <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	    case (one,two) of
		(B one, B two) =>
		if one = t andalso two = t then eval (l,map,B(t)::rest)
		else eval (l,map,B(f)::rest)
	      | (B one, N two) =>
		if isBool(find(N two,map)) then
		    if cBool(find(N two,map)) = t andalso one = t then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)						
	      | (N one, B two) =>
		if isBool(find(N one,map)) then
		    if cBool(find(N one,map)) = t andalso two = t then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)
	      | (N one, N two) =>
		if isBool(find(N one,map)) andalso isBool(find(N two,map)) then
		    if cBool(find(N one,map)) = t andalso cBool(find(N two,map)) = t then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)											
		else eval (l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack) 
	end
    else if isOrr str then
	if length stack <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	     case (one,two) of
		(B one, B two) =>
		if one = f andalso two = f then eval (l,map,B(f)::rest)
		else eval (l,map,B(t)::rest)
	      | (B one, N two) =>
		if isBool(find(N two,map)) then
		    if cBool(find(N two,map)) = f andalso one = f then eval(l,map,B(f)::rest)
		    else eval(l,map,B(t)::rest)
		else eval(l,map,E::stack)						
	      | (N one, B two) =>
		if isBool(find(N one,map)) then
		    if cBool(find(N one,map)) = f andalso two = f then eval(l,map,B(f)::rest)
		    else eval(l,map,B(t)::rest)
		else eval(l,map,E::stack)
	      | (N one, N two) =>
		if isBool(find(N one,map)) andalso isBool(find(N two,map)) then
		    if cBool(find(N one,map)) = f andalso cBool(find(N two,map)) = f then eval(l,map,B(f)::rest)
		    else eval(l,map,B(t)::rest)											
		else eval (l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack) 
	end
    else if isNot str then
	if isEmpty stack then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val rest = tl stack
	in
	    case (one) of
		(B one) =>
		if one = t then eval(l,map,B(f)::rest)
		else eval(l,map,B(t)::rest)
	      | (N one) =>
		if isBool(find(N one,map)) then
		    if cBool(find(N one,map)) = t then eval(l,map,B(f)::rest)
		    else eval(l,map,B(t)::rest)
		else eval (l,map,E::stack)
	      | (_) => eval(l,map,E::stack) 
	end
    else if isEql str then
	if length stack <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	    case(one,two) of
	   	(I one, I two) =>
		if one = two then eval (l,map,B(t)::rest)
		else eval(l,map,B(f)::rest)
	      | (N one, I two) =>
		if isInt(find(N one, map)) then
		    if cInt(find(N one,map)) = two then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		if isInt(find(N two, map)) then
		    if cInt(find(N two,map)) = one then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)
	      | (N one, N two) =>
		if isInt(find(N one,map)) andalso isInt(find(N two,map)) then
		    if cInt(find(N one,map)) = cInt(find(N two,map)) then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack)
	end
    else if isLss str then
	if length stack <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd (tl stack)
	    val rest = tl(tl stack)
	in
	    case (one,two) of
		(I one, I two) =>
		if one > two then eval (l,map,B(t)::rest)
		else eval(l,map,B(f)::rest)
	      | (N one, I two) =>
		if isInt(find(N one, map)) then
		    if cInt(find(N one,map)) > two then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		if isInt(find(N two, map)) then
		    if cInt(find(N two,map)) < one then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)
	      | (N one, N two) =>
		if isInt(find(N one,map)) andalso isInt(find(N two,map)) then
		    if cInt(find(N one,map)) > cInt(find(N two,map)) then eval(l,map,B(t)::rest)
		    else eval(l,map,B(f)::rest)
		else eval(l,map,E::stack)
	      | (_,_) => eval(l,map,E::stack)
	end
    else if isBnd str then
	if length stack <= 1 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd(tl stack)
	    val rest = tl(tl stack)
	in
	    case (one,two) of
		(N one, N two) =>
		if binded(find(N one, map)) then
		    let
			val newBind = [(N two, find(N one,map))]
		    in
			eval(l,newBind::map,U(u)::rest)
		    end
		else eval(l,map,E::stack)
	      | (I one, N two) =>
		let
		    val newBind = [(N two, I one)]
		in
		    eval(l,newBind::map,U(u)::rest)
		end
	      | (B one, N two) =>
		let
		    val newBind = [(N two, B one)]
		in
		    eval(l,newBind::map,U(u)::rest)
		end
	      | (S one, N two) =>
		let
		    val newBind = [(N two, S one)]
		in
		    eval(l,newBind::map,U(u)::rest)
		end
	      | (U one, N two) =>
		let
		    val newBind = [(N two, U one)]
		in
		    eval(l,newBind::map,U(u)::rest)
		end
	      | (_,_) => eval(l,map,E::stack)
	end
    else if isIf str then
	if length stack <= 2 then eval(l,map,E::stack)
	else let
	    val one = hd stack
	    val two = hd(tl stack)
	    val three = hd(tl(tl stack))
	    val rest = tl(tl(tl stack))
	in
	    case (three) of
		(B three) =>
		    if three = t then eval(l,map,two::rest)
		    else eval(l,map,one::rest) 
	      | (N three) =>
		if isBool(find(N three,map)) then
		    if cBool(find(N three,map)) = t then eval(l,map,two::rest)
		    else eval(l,map,one::rest)
		else eval(l,map,E::stack)
	      | (_) => eval(l,map,E::stack) 
	end
    else if isLet str then
	let
	    val letMap = hd(map)::map
	in
	    eval(l,letMap,LET::stack)
	end
    else if isEnd str then
	let
	    val one = hd stack
	    val rest = tl stack
	    val letMap = tl(map)
	in
	    eval(l,letMap,one::letPOP(rest))
	end	    
    else if isQui str then
	eval([],map,stack)	    
    else stack
fun ifN c =
    if String.isPrefix "~" c then ("-" ^ String.substring(c,1,size(c)-1))
    else c;  
fun read c =
    let
	val ins = TextIO.openIn c
	fun loop ins =
		 case TextIO.inputLine ins of
		     SOME line => line :: loop ins
		   | NONE => []   
    in
	loop ins before TextIO.closeIn ins
    end
			
fun interpreter(inFile: string, outFile: string) =
    let
	val ins = read(inFile)
	val out = TextIO.openOut outFile
	val map = [[]]
	val stack = eval(ins,map,[])
	fun helper ([]) = (TextIO.closeOut out)
	  | helper (line::stack2) =
	    case line of
		I line => (TextIO.output(out,ifN(Int.toString(line)) ^ "\n");
			  helper(stack2))	   
	      | S line => (TextIO.output(out,line ^ "\n");
			   helper(stack2))
	      | B line => (TextIO.output(out,line ^ "\n");
			   helper(stack2))
	      | N line => (TextIO.output(out,line ^ "\n");
			   helper(stack2))
	      | U line => (TextIO.output(out,line ^"\n");
			  helper(stack2))
	      | E  => (TextIO.output(out,":error:" ^ "\n");
		       helper(stack2))
			 
					
    in
	helper(stack)
    end
