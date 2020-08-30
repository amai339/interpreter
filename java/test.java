package d;

import java.io.IOException;

public class d {
	public static void main(String[] args) throws IOException {
//		interpreter.interpreter("input1.txt", "output1.txt");
//		interpreter.interpreter("input2.txt", "output2.txt");
//		interpreter.interpreter("input4.txt", "output4.txt");
//		interpreter.interpreter("input5.txt", "output5.txt");
//		interpreter.interpreter("input6.txt", "output6.txt");
//		interpreter.interpreter("input9.txt", "output9.txt");
//		interpreter.interpreter("input10.txt", "output10.txt");
//		interpreter.interpreter("input12.txt", "output12.txt");
//		interpreter.interpreter("input15.txt", "output15.txt");
//		interpreter.interpreter("input20.txt", "output20.txt");
//		interpreter.interpreter("input19.txt", "o.txt");
//		i.interpreter("input15.txt", "o.txt");
		for (int i = 1; i < 21; i++) {
			interpreter.interpreter("input" + i + ".txt", "output"+i+".txt");
		}
//		interpreter.interpreter("input3.txt", "o.txt");

	}
	
}
