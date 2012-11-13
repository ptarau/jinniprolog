package prolog.kernel;

import prolog.logic.Interact;

import java.io.*;
import java.util.*;

public class PWriter extends PrologWriter {
	private boolean trouble = false;

	//private Formatter formatter;

	public PWriter(TextSink textSink) {
	   super(textSink);
	}

	public void flush() {
	}

	public void close() {
	}

	public boolean checkError() {
		return trouble;
	}

	protected void setError() {
		trouble = true;
	}

	protected void clearError() {
		trouble = false;
	}

	public void write(int c) {
		write("" + (char)c);
	}

	public void write(char buf[], int off, int len) {
		write(new String(buf, off, len));
	}

	public void write(char buf[]) {
		write(buf, 0, buf.length);
	}

	public void write(String s, int off, int len) {
		write(s.substring(off, off + len));
	}

	public void write(String s) {
		textSink.append_text(s);
	}

	private void newLine() {
		write(Interact.NL);
	}

	/* Methods that do not terminate lines */

	public void print(boolean b) {
		write(b ? "true" : "false");
	}

	public void print(char c) {
		write(c);
	}

	public void print(int i) {
		write(String.valueOf(i));
	}

	public void print(long l) {
		write(String.valueOf(l));
	}

	public void print(float f) {
		write(String.valueOf(f));
	}

	public void print(double d) {
		write(String.valueOf(d));
	}

	public void print(char s[]) {
		write(s);
	}

	public void print(String s) {
		if (s == null) {
			s = "null";
		}
		write(s);
	}

	public void print(Object obj) {
		write(String.valueOf(obj));
	}

	public void println() {
		newLine();
	}

	public void println(boolean x) {
		print(x);
		println();
	}

	public void println(char x) {
	  print(x);
	  println();
	}

	public void println(int x) {
		print(x);
		println();
	}

	public void println(long x) {
		print(x);
		println();
	}

	public void println(float x) {
		print(x);
		println();
	}

	public void println(double x) {
		print(x);
		println();
	}

	public void println(char x[]) {
		print(x);
		println();
	}

	public void println(String x) {
		print(x);
		println();
	}

	public void println(Object x) {
		String s = String.valueOf(x);
		print(s);
		println();
	}

  /*
	public VWriter printf(String format, Object... args) {
		return format(format, args);
	}

	public VWriter printf(Locale l, String format, Object... args) {
		return format(l, format, args);
	}

	public VWriter format(String format, Object... args) {
		if ((formatter == null) || (formatter.locale() != Locale.getDefault()))
			formatter = new Formatter(this);
		formatter.format(Locale.getDefault(), format, args);
		return this;
	}

	public VWriter format(Locale l, String format, Object... args) {
		if ((formatter == null) || (formatter.locale() != l))
			formatter = new Formatter(this, l);
		formatter.format(l, format, args);
		return this;
	}

	public VWriter append(CharSequence csq) {
		if (csq == null)
			write("null");
		else
			write(csq.toString());
		return this;
	}

	public VWriter append(CharSequence csq, int start, int end) {
		CharSequence cs = (csq == null ? "null" : csq);
		write(cs.subSequence(start, end).toString());
		return this;
	}
	
		public PWriter append(char c) {
		write(c);
		return this;
	}
  */


}
