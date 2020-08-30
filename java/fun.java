package d;

import java.util.HashMap;
import java.util.Stack;

public class fun {
	String funType;
	String funName;
	String funArgu;
	Stack<String> instructions;
	HashMap<String,Object> binds;
	fun(String type, String name, String argu) {
		funType = type;
		funName = name;
		funArgu = argu;
		 instructions = new Stack<String>();
		 binds = new HashMap<String, Object>();
	}

}
