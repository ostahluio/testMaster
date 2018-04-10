/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.types.*;

/* Class ABSObjectMirror
 * 
 * The builtin ABS function reflect(o) creates an object mirror for
 * the given object o. Here we (dynamically) define the class of the meta-object,
 * including its interface, i.e. the meta-object protocol for object mirrors.
 */
public class ObjectMirror {
    private static ABSDynamicClass thisClass;

    /* 
     * Create the class object for object mirrors. This only happens once.
     */
    public static ABSDynamicClass singleton() {
        if (thisClass == null) {
            thisClass = new ABSDynamicClass();
            setupAPI();
        }
        return thisClass;
    }
    
    public static void setupAPI() {
        thisClass.setName("ObjectMirror");
        
        /*
         * getClassName: get name of object's class
         */
        thisClass.addMethod(/*ABSString*/ "getClassName", new ABSClosure() {
            @Override
            public ABSString exec(ABSDynamicObject t, ABSValue... params) {
                ABSString name;
                name = ABSString.fromString(((ABSDynamicObject)t.dispatch("getObject")).getClassName());
                return name;
            }
        });
        
        /*
         * getClass: get class of object
         */
        thisClass.addMethod(/*ABSDynamicObject<Clazz>*/ "getClass", new ABSClosure() {
            @Override
            public ABSDynamicObject exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicClass cls = ((ABSDynamicObject)t.dispatch("getObject")).getClazz();
                ABSDynamicObject o = new ABSDynamicObject(Clazz.singleton());
                o.setFieldValue("class", cls);
                return o;
            }
        });
        
        /*
         * setClass: set new class for object
         */
        thisClass.addMethod(/*Unit*/ "setClass", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicClass cls = (ABSDynamicClass)((ABSDynamicObject)params[0]).getFieldValue_Internal("class");
                ((ABSDynamicObject)t.dispatch("getObject")).setClazz(cls);
                return ABSUnit.UNIT;
            }
        });

        /*
         * respondsTo: find out whether object responds to given method
         */
        thisClass.addMethod(/*ABSBool*/ "respondsTo", new ABSClosure() {
            @Override
            public ABSBool exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicClass cls = (ABSDynamicClass)((ABSDynamicObject)t.dispatch("getObject")).getClazz();
                return ABSBool.fromBoolean(cls.hasMethod(((ABSString)params[0]).getString()));
            }
        });
        
                
        // TODO: getFieldValue(), setFieldValue()

        thisClass.addMethod(/*ABSDynamicObject<COG>*/ "getCog", new ABSClosure() {
            @Override
            public ABSDynamicObject exec(ABSDynamicObject t, ABSValue... params) {
                COG cog = ((ABSDynamicObject)t.dispatch("getObject")).getCOG();
                ABSDynamicObject o = new ABSDynamicObject(Cog.singleton());
                o.setFieldValue("cog", cog);
                return o;
            }
        });
        
        thisClass.addMethod(/*Unit*/ "setCog", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ((ABSDynamicObject)t.dispatch("getObject")).setCOG((COG)params[0]);
                return ABSUnit.UNIT;
            }
        });

        /*
         * getObject: obtain the mirrored object
         */
        thisClass.addMethod(/*ABSDynamicObject*/ "getObject", new ABSClosure() {
            @Override
            public ABSDynamicObject exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicObject object;
                object = (ABSDynamicObject)t.getFieldValue_Internal("object");
                return object;
            }
        });
    }


}
