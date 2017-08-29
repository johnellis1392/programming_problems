package ctci.practice.notes;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;

import java.util.ArrayList;
import java.util.List;


public class ReflectionNotes {

    public static List<String> getParameterNames(Method method) {
        Parameter[] parameters = method.getParameters();
        List<String> result = new ArrayList<>();
        for (Parameter parameter : parameters) {
            result.add(parameter.getName())
        }
        return result;
    }


    public static List<String> getParameterNamesByMethod(Class clazz, String methodName) {
        Method[] declaredMethods = clazz.getDeclaredMethods();
        List<String> result = null;
        for (Method declaredMethod : declaredMethods) {
            if (declaredMethod.getName().equals(methodName)) {
                result = getParameterNames(declaredMethod);
                break;
            }
        }
        return result;
    }


    public static List<String> getDeclaredMethods(Class clazz) {
        Method[] declaredMethods = clazz.getDeclaredMethods();
        List<String> result = new ArrayList<>();
        for (Method declaredMethod : declaredMethods) {
            result.add(declaredMethod.getName());
        }
        return result;
    }

}
