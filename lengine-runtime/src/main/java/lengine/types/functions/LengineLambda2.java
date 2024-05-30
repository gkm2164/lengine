package lengine.types.functions;

public interface LengineLambda2<R, T1, T2> extends LengineLambdaCommon {
    R invoke(T1 v1, T2 v2);
}
