package agentgui;

import prolog.kernel.*;
import prolog.logic.Interact;

import java.io.*;
import java.util.*;

public class VWriter extends PWriter {
	private Formatter formatter;

	public VWriter(TextSink textSink) {
	   super(textSink);
	}

  
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

	public VWriter append(char c) {
		write(c);
		return this;
	}
}
