package d;
import java.io.*;
import java.util.HashMap;
import java.util.Stack;

public class interpreter {
    public static customType push(String line, customType q) {
        if (line.charAt(5) == ('"')) 
            q.stack.push(line.substring(5, line.length() - 1));    
        else if (Character.isDigit(line.charAt(5))) {
            if (line.contains("."))
                q.stack.push(":error:");    
            else 
                q.stack.push(line.substring(5));                
        }        
        else if (line.contains("-") && line.charAt(6) != '0') {    
            if (Character.isDigit(line.charAt(6))) 
                q.stack.push(line.substring(5));    
            else 
                q.stack.push(":error:");    
        }
        else if (line.contains("-0")) 
            q.stack.push("0");    
        else 
            q.stack.push(line.substring(5));    
        return q;
    }
    public static customType pop(customType q) {
        if (q.stack.isEmpty()) 
            q.stack.push(":error:");
        else 
            q.stack.pop();
        return q;
    }
    public static customType add(customType q) {
        if (q.stack.size() <= 1) 
            q.stack.push(":error:");
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) 
                first = (String) q.map.get(first);
            if (q.map.containsKey(second)) 
                second = (String) q.map.get(second);
            if (isInt(first) && isInt(second)) {
                int one = Integer.parseInt(first);
                int two = Integer.parseInt(second);
                q.stack.pop();
                q.stack.pop();
                q.stack.push(Integer.toString(two + one));
            }
            else 
                q.stack.push(":error:");            
        }
        return q;
    }
    public static customType sub(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (isInt(first) && isInt(second)) {
                int one = Integer.parseInt(first);
                int two = Integer.parseInt(second);
                q.stack.pop();
                q.stack.pop();
                q.stack.push(Integer.toString(two - one));
            }
            else {
                q.stack.push(":error:");
            }

        }
        return q;
    }
    public static customType mul(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (isInt(first) && isInt(second)) {
                int one = Integer.parseInt(first);
                int two = Integer.parseInt(second);
                q.stack.pop();
                q.stack.pop();
                q.stack.push(Integer.toString(two * one));
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType div(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (isInt(first) && isInt(second)) {
                int one = Integer.parseInt(first);
                int two = Integer.parseInt(second);
                if (one != 0) {
                    q.stack.pop();
                    q.stack.pop();
                    q.stack.push(Integer.toString(two / one));
                }
                else {
                    q.stack.push(":error:");            
                }
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType rem(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        String first = q.stack.peek();
        String second = q.stack.get(q.stack.size()-2);
        if (q.map.containsKey(first)) {
            first = (String) q.map.get(first);
        }
        if (q.map.containsKey(second)) {
            second = (String) q.map.get(second);
        }
        if (isInt(first) && isInt(second)) {
            int one = Integer.parseInt(first);
            int two = Integer.parseInt(second);
            if (one != 0) {
                q.stack.pop();
                q.stack.pop();
                q.stack.push(Integer.toString(two % one));
            }
            else {
                q.stack.push(":error:");            
            }
        }
        else {
            q.stack.push(":error:");
        }
        return q;
    }
    public static customType neg(customType q) {
        if (q.stack.isEmpty()) {
            q.stack.push(":error:");
        }
        else {
            String x = q.stack.peek() ;
            if (q.map.containsKey(x)) {
                x = (String) q.map.get(x);
            }
            if (isInt(x)) {
                q.stack.pop();
                int one = Integer.parseInt(x);
                q.stack.push(Integer.toString(one * -1));
            }
            else  {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType swap(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String x = q.stack.pop();
            String y = q.stack.pop();
            q.stack.push(x);
            q.stack.push(y);
        }
        return q;
    }
    public static customType cat(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (first.startsWith("\"") && second.startsWith("\"")) {
                q.stack.pop();
                q.stack.pop();
                q.stack.push(second + first.substring(1));
                }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType and(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (isBool(first) && isBool(second)) {
                q.stack.pop();
                q.stack.pop();
                if (first.equals(":true:") && second.equals(":true:")) {
                    q.stack.push(":true:");
                }
                else {
                    q.stack.push(":false:");
                }
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType or(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (isBool(first) && isBool(second)) {
                q.stack.pop();
                q.stack.pop();
                if (first.equals(":false:") && second.equals(":false:")) {
                    q.stack.push(":false:");
                }
                else {
                    q.stack.push(":true:");
                }
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType not(customType q) {
        if (q.stack.isEmpty()) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (isBool(first)) {
                q.stack.pop();
                if (first.equals(":true:")) {
                    q.stack.push(":false:");
                }
                else {
                    q.stack.push(":true:");
                }
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType equal(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (isInt(first) && isInt(second)) {
                q.stack.pop();
                q.stack.pop();
                int one = Integer.parseInt(first);
                int two = Integer.parseInt(second);
                if (one == two) {
                    q.stack.push(":true:");
                }
                else {
                    q.stack.push(":false:");
                }
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType lessThan(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }
        else {
            String first = q.stack.peek();
            String second = q.stack.get(q.stack.size()-2);
            if (q.map.containsKey(first)) {
                first = (String) q.map.get(first);
            }
            if (q.map.containsKey(second)) {
                second = (String) q.map.get(second);
            }
            if (isInt(first) && isInt(second)) {
                q.stack.pop();
                q.stack.pop();
                int one = Integer.parseInt(first);
                int two = Integer.parseInt(second);
                if (one > two) {
                    q.stack.push(":true:");
                }
                else {
                    q.stack.push(":false:");
                }
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType bind(customType q) {
        if (q.stack.size() <= 1) {
            q.stack.push(":error:");
        }    
        else {
            String x = q.stack.peek();
            String name = q.stack.get(q.stack.size()-2);
            if (isName(name) && !x.equals(":error:")) {
                q.stack.pop();
                q.stack.pop();
                q.stack.push(":unit:");
                String first = (String) q.map.get(x);
                if (first != null) 
                    q.map.put(name,first);
                else 
                	q.map.put(name,x);
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static customType _if(customType q) {
        if (q.stack.size() < 3) {
            q.stack.push(":error:");
        }
        else {
            String one = q.stack.peek();
            String two = q.stack.get(q.stack.size()-2);
            String three = q.stack.get(q.stack.size()-3);
            if (q.map.containsKey(three)) {
                three = (String) q.map.get(three);
            }
            if (isBool(three)) {
                q.stack.pop();
                q.stack.pop();
                q.stack.pop();
                if (three.equals(":true:")) {
                    q.stack.push(two);
                }
                else {
                    q.stack.push(one);
                }
            }
            else {
                q.stack.push(":error:");
            }
        }
        return q;
    }
    public static String let(Stack<String> allLines, customType q) {
        customType letQ = new customType();
        letQ.map = new HashMap<String,Object>(q.map);
            String line = allLines.pop();
            while(!line.equals("end")){
                if(line.equals("let")) {
                    letQ.stack.push(let(allLines,letQ));
                }
                else {
                    letQ = commands(line, letQ, allLines);    
                }
                line = allLines.pop();
            }
        return letQ.stack.peek();
    }
    public static customType fun(String fun,customType q, Stack<String> allLines) {
        String[] fParts = fun.split("\\s+");
        String type = fParts[0];
        String name = fParts[1];
        String argu = fParts[2];
        fun f = new fun(type,name,argu);
        while(!allLines.isEmpty() && !fun.equals("funEnd")) {
            fun = allLines.pop();
            f.instructions.add(0, fun);
        }
        q.map.put(name, f);
        q.stack.push(":unit:");
        f.binds = new HashMap<String,Object> (q.map);
        return q;
    }
    public static customType call(customType q) {
        if (q.stack.size() < 2) 
            q.stack.push(":error:");
        else {
        	boolean done = false;
            String argu = q.stack.peek();
            String name = q.stack.get(q.stack.size()-2);
            Object newArgu = argu;
            fun f = null;
            if (q.map.containsKey(argu) && isName(argu)) {
            	newArgu = q.map.get(argu);
            }
            if (newArgu.equals(":error:")) {
        		q.stack.push(":error:");
        		done = true;
        	}
            if (!done) {
	            if (!q.map.containsKey(name) || !(q.map.get(name) instanceof fun)) {
	            	q.stack.push(":error:");
	            	done = true;
	            }
	            else {
	            	if(isName(name)) {
	            		f = (fun) q.map.get(name);
	            	}
	            }
            }
            if (!done){
                  q.stack.pop();
                  q.stack.pop();    
                  customType funQ = new customType();
                  funQ.map = new HashMap<String, Object>(f.binds);
                  funQ.map.put(f.funArgu, newArgu);
                  Stack<String> instructions = new Stack<String>();
                  instructions.addAll(f.instructions);
                  String instruction = instructions.pop();
                  while(!instructions.isEmpty() && !instruction.equals("return")) {
                      funQ = commands(instruction,funQ,instructions);
                      instruction = instructions.pop();
                  }
                  if (f.funType.equals("inOutFun")) 
                      q.map.put(argu, funQ.map.get(f.funArgu));
                  String top = funQ.stack.pop();
                  if (funQ.map.containsKey(top)) 
                      top = (String)funQ.map.get(top);
                  if (instruction.equals("return"))
                	  q.stack.push(top);
            }
           
        }
        return q;
    }
    public static customType commands(String line, customType q, Stack<String> allLines) {
        if (line.startsWith("push")) 
            push(line,q);
        if (line.equals("pop"))
            pop(q);
        if (line.equals("add")) 
            add(q);
        if (line.equals("sub"))
            sub(q);
        if (line.equals("mul"))
            mul(q);
        if (line.equals("div"))
            div(q);
        if (line.equals("rem"))
            rem(q);
        if (line.equals("neg"))
            neg(q);
        if (line.equals("swap"))
            swap(q);
        if (line.equals("cat"))
            cat(q);
        if (line.equals("and"))
            and(q);
        if (line.equals("or"))
            or(q);
        if (line.equals("not"))
            not(q);
        if (line.equals("equal"))
            equal(q);
        if (line.equals("lessThan"))
            lessThan(q);
        if (line.equals("bind"))
            bind(q);
        if (line.equals("if"))
            _if(q);
        if (line.startsWith("fun ")) 
            fun(line,q,allLines);
        if (line.startsWith("inOutFun")) 
            fun(line,q,allLines);
        if (line.equals("call")) 
            call(q);
        return q;
    }
    public static  void interpreter(String inFile, String outFile) throws IOException {
        BufferedReader read = new BufferedReader(new FileReader(inFile));
        BufferedWriter write = new BufferedWriter(new FileWriter(outFile));
        Stack<String> inputs = new Stack<>();
        String input;    
        customType q = new customType();
        while ((input = read.readLine()) != null) {
            inputs.add(0,input);
        }
        while (!inputs.isEmpty()) {
            String line = inputs.pop();
            if (line.equals("let")) q.stack.push(let(inputs,q));
            else if (line.equals("quit")) {
            	while (!q.stack.isEmpty()) {
                    if (q.stack.peek().startsWith("\"")) {
                         write.write(q.stack.pop().substring(1) + "\n");
                    }
                    else {
                    	write.write(q.stack.pop() + "\n");
                    }
                }
            }
            else {
            	commands(line,q,inputs);
            }
        }
        write.close();
        read.close();
    }
    public static boolean hasSymbol(Character c) {
        return (c == '-' || c == '"' || c == ':') ;
    }
    public static boolean isName(String c) {
        return  (!hasSymbol(c.charAt(0)) && !Character.isDigit(c.charAt(0)));
    }
    public static boolean isInt(String c) {
        try {
            Integer.parseInt(c);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }
    public static boolean isBool(String c) {
        return (c.equals(":true:") || c.equals(":false:"));
    }
}