package com.github.noobymatze.concurrency;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author noobymatze
 */
public class ChannelTest {
    
    public ChannelTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
    }
    
    @AfterClass
    public static void tearDownClass() {
    }
    
    @Before
    public void setUp() {
    }
    
    @After
    public void tearDown() {
    }

    /**
     * Test of read method, of class Channel.
     */
    @Test
    public void testRead() throws Exception {
        System.out.println("read");
        Channel instance = new Channel();
        Object expResult = null;
        Object result = instance.read();
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of write method, of class Channel.
     */
    @Test
    public void testWrite() throws Exception {
        System.out.println("write");
        Object o = null;
        Channel instance = new Channel();
        instance.write(o);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of isEmpty method, of class Channel.
     */
    @Test
    public void testIsEmpty() throws Exception {
        System.out.println("isEmpty");
        Channel instance = new Channel();
        boolean expResult = false;
        boolean result = instance.isEmpty();
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of unGet method, of class Channel.
     */
    @Test
    public void testUnGet() throws Exception {
        System.out.println("unGet");
        Object o = null;
        Channel instance = new Channel();
        instance.unGet(o);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }
    
}
