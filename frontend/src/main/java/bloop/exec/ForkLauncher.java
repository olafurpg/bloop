package bloop.exec;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

/** A zero-dependency Java class that launches a classpath and main class from an args file. */
public class ForkLauncher {

  public static void main(String[] args) throws Throwable {
    if (args.length != 1) {
      exit("invalid arguments: expected 1 argument, obtained " + Arrays.toString(args));
    }
    Path argsFile = Paths.get(args[0]);
    List<String> lines = Files.readAllLines(argsFile);
    String classpath = lines.get(0);
    String mainClassName = lines.get(1);

    Thread thread = Thread.currentThread();
    ClassLoader contextClassLoader = thread.getContextClassLoader();

    String[] mainArgs = new String[lines.size() - 2];
    if (mainArgs.length > 0) {
      mainArgs = lines.subList(2, lines.size()).toArray(mainArgs);
    }

    String[] classpathEntries = classpath.split(File.pathSeparator);
    URL[] urls = new URL[classpathEntries.length];
    for (int i = 0; i < classpathEntries.length; i++) {
      URL url = Paths.get(classpathEntries[i]).toUri().toURL();
      urls[i] = url;
    }
    URLClassLoader classLoader = new URLClassLoader(urls, null);

    Class<?> mainClass = null;
    Method mainMethod = null;

    try {
      mainClass = classLoader.loadClass(mainClassName);
    } catch (ClassNotFoundException ex) {
      exit("Error: class " + mainClassName + " not found");
    }

    try {
      Class[] params = {String[].class};
      mainMethod = mainClass.getMethod("main", params);
    } catch (NoSuchMethodException ex) {
      exit("Error: main method not found in class " + mainClassName);
    }

    thread.setContextClassLoader(classLoader);
    try {
      mainMethod.invoke(null, (Object) mainArgs);
    } catch (InvocationTargetException ex) {
      trimStackTrace(ex, ForkLauncher.class.getName(), mainClassName);
      throw ex.getCause();
    }
  }

  // Removes stack trace elements that reference the reflective invocation in TestLauncher.
  private static void trimStackTrace(Throwable ex, String fromClassName, String toClassName) {
    Throwable cause = ex.getCause();
    while (cause != null) {
      StackTraceElement[] stackTrace = cause.getStackTrace();
      int end = stackTrace.length - 1;
      StackTraceElement last = stackTrace[end];
      if (last.getClassName().equals(fromClassName)) {
        for (int i = 0; end >= 0; end--) {
          StackTraceElement e = stackTrace[end];
          if (e.getClassName().equals(toClassName)) {
            break;
          }
        }
        StackTraceElement[] newStackTrace = Arrays.copyOfRange(stackTrace, 0, end + 1);
        cause.setStackTrace(newStackTrace);
      }
      cause = cause.getCause();
    }
  }

  private static void exit(String message) {
    System.err.println(message);
    System.exit(255);
  }
}
