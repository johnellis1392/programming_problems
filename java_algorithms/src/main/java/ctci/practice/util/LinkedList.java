package ctci.practice.util;

import java.util.ArrayList;


public class LinkedList<V> {

	public Node<V> head;

	public LinkedList() {
		this.head = null;
	}

	public LinkedList<V> add(V value) {
		if (this.head == null) {
				this.head = new Node(value);
			} else {
				this.head.add(value);
			}
		return this;
	}

	public int size() {
		int i = 0;
		for (Node<V> n = this.head; n != null; n = n.next, i++);
		return i;
	}

	public V[] toArray() {
		final int n = this.size();
        ArrayList<V> result = new ArrayList<>();

		Node<V> node = this.head;
		for (int i = 0; i < n; i++, node = node.next)
			result.add(node.value);

		// return result.toArray(new V[result.size()]);
		return (V[])result.toArray();
	}

    public boolean equals(LinkedList<V> other) {
        Node<V> n1 = this.head;
        Node<V> n2 = other.head;

        while (n1 != null && n2 != null) {
            if (n1.value != n2.value) {
                return false;
            }
        }

        if (n1 != null || n2 != null)
            return false;
        else
            return true;
    }


    public static <V> LinkedList<V> fromArray(V[] values) {
        LinkedList<V> list = new LinkedList<V>();
        for (V value : values) list.add(value);
        return list;
    }


	public class Node<V> {
		public Node<V> next;
		public V value;

		public Node(final V value) {
			this.value = value;
			this.next = null;
		}

		public void add(V value) {
			if (this.next == null) {
					this.next = new Node<V>(value);
				} else {
					this.next.add(value);
				}
		}
	}

}
