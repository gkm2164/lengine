package lengine.runtime;

import org.objectweb.asm.*;

import java.util.Optional;

import static org.objectweb.asm.Opcodes.ASM4;

public class ClassPrinter extends ClassVisitor {
    public ClassPrinter() {
        super(ASM4);
    }

    public void visit(int version, int access, String name,
                      String signature, String superName, String[] interfaces) {
        System.out.println(name + " extends " + superName + " {");
    }

    public void visitSource(String source, String debug) {
    }

    public void visitOuterClass(String owner, String name, String desc) {
    }

    public AnnotationVisitor visitAnnotation(String desc,
                                             boolean visible) {
        System.out.println(desc + " " + visible);

        return null;
    }

    public void visitAttribute(Attribute attr) {
        System.out.println(attr);

    }

    public void visitInnerClass(String name, String outerName,
                                String innerName, int access) {
    }

    public FieldVisitor visitField(int access, String name, String desc,
                                   String signature, Object value) {
        FieldVisitor ret = super.visitField(access, name, desc, signature, value);
        System.out.println(access + " " + name + " " + desc + " " + signature + " " + Optional.ofNullable(value));
        return ret;
    }

    public MethodVisitor visitMethod(int access, String name,
                                     String desc, String signature, String[] exceptions) {
        MethodVisitor ret = super.visitMethod(access, name, desc, signature, exceptions);
        System.out.println(access + " " + name + " " + desc + " " + signature + " " + Optional.ofNullable(exceptions));
        return ret;
    }

    public void visitEnd() {
        System.out.println("}");
    }
}