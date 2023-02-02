package lengine.functions;

public interface LengineLambda3<R, T1, T2, T3> extends LengineLambdaCommon {
    R invoke(T1 v1, T2 v2, T3 v3);
}
