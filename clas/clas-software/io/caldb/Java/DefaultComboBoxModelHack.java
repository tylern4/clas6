//********************************************************/
//* H.Avagyan
//* Date  : 2001-04-03
//********************************************************/
import java.beans.*;
import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

import javax.swing.event.*;
import javax.swing.plaf.*;
import javax.swing.border.*;

import javax.accessibility.*;

/********************************************************
* used in JCaldbview with swing item support
********************************************************/    
public class DefaultComboBoxModelHack extends AbstractListModel implements MutableComboBoxModel, Serializable {
    Vector objects;
    Object selectedObject;

    /**
     * Constructs an empty DefaultComboBoxModel object.
     */
    public DefaultComboBoxModelHack() {
        objects = new Vector();
    }

    /**
     * Constructs a DefaultComboBoxModel object initialized with
     * an array of objects.
     *
     * @param items  an array of Object objects
     */
    public DefaultComboBoxModelHack(final Object items[]) {
        objects = new Vector();
	objects.ensureCapacity( items.length );

        int i,c;
	for ( i=0,c=items.length;i<c;i++ )
	    objects.addElement(items[i]);

	if ( getSize() > 0 ) {
	    selectedObject = getElementAt( 0 );
	}
    }

    /**
     * Constructs a DefaultComboBoxModel object initialized with
     * a vector.
     *
     * @param v  a Vector object ...
     */
    public DefaultComboBoxModelHack(Vector v) {
	objects = v;

	if ( getSize() > 0 ) {
	    selectedObject = getElementAt( 0 );
	}
    }

    // implements javax.swing.ComboBoxModel
    public void setSelectedItem(Object anObject) {
        selectedObject = anObject;
	fireContentsChanged(this, -1, -1);
    }

    // implements javax.swing.ComboBoxModel
    public Object getSelectedItem() {
        return selectedObject;
    }

    // implements javax.swing.ListModel
    public int getSize() {
        return objects.size();
    }

    // implements javax.swing.ListModel
    public Object getElementAt(int index) {
        if ( index >= 0 && index < objects.size() )
	    return objects.elementAt(index);
	else
	    return null;
    }

    /**
     * Returns the index-position of the specified object in the list.
     *
     * @param anObject  
     * @return an int representing the index position, where 0 is 
     *         the first position
     */
    public int getIndexOf(Object anObject) {
        return objects.indexOf(anObject);
    }

    // implements javax.swing.MutableComboBoxModel
    public void addElement(Object anObject) {
        objects.addElement(anObject);
	fireIntervalAdded(this,objects.size()-1, objects.size()-1);
        if ( objects.size() == 1 && selectedObject == null && anObject != null ) {
	    setSelectedItem( anObject );
	}
    }

    // implements javax.swing.MutableComboBoxModel
    public void insertElementAt(Object anObject,int index) {
        objects.insertElementAt(anObject,index);
	fireIntervalAdded(this, index, index);
    }

    // implements javax.swing.MutableComboBoxModel
    public void removeElementAt(int index) {
        if ( getElementAt( index ) == selectedObject ) {
	    if ( index == 0 ) {
	        setSelectedItem( getSize() == 1 ? null : getElementAt( index + 1 ) );
	    }
	    else {
	        setSelectedItem( getElementAt( index - 1 ) );
	    }
	}

	objects.removeElementAt(index);

	fireIntervalRemoved(this, index, index);
    }

    // implements javax.swing.MutableComboBoxModel
    public void removeElement(Object anObject) {
        int index = objects.indexOf(anObject);
	if ( index != -1 ) {
	    removeElementAt(index);
	}
    }

    /**
     * Empties the list.
     */
    public void removeAllElements() {
        int firstIndex = 0;
	int lastIndex = objects.size()-1;
	objects.removeAllElements();
	selectedObject = null;
	fireIntervalRemoved(this, firstIndex, lastIndex);
    }
    
    public void setObjects(Vector v) {
        objects = v;
        selectedObject = null;
        fireIntervalAdded(this,0, objects.size()-1);
    }
    
    public void setObjects(String items[]) {
        objects = new Vector();
	    objects.ensureCapacity( items.length );
        int i,c;
	    for ( i=0,c=items.length;i<c;i++ )
	                objects.addElement(items[i]);
	    if ( getSize() > 0 ) {
	       selectedObject = getElementAt( 0 );
	    }
    }
    
    
}
