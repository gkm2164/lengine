package lengine.types.functions;

public interface LengineLambda4<R, T1, T2, T3, T4> extends LengineLambdaCommon {
    R invoke(T1 v1, T2 v2, T3 v3, T4 v4);
}
