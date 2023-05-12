#!/usr/bin/env ts-node

class ListNode {
  val: number;
  next: ListNode | null;
  constructor(val?: number, next?: ListNode | null) {
    this.val = val === undefined ? 0 : val;
    this.next = next === undefined ? null : next;
  }

  static from(ns: number[]): ListNode | null {
    if (ns.length === 0) return null;
    let head = new ListNode(ns[0]);
    head.next = ListNode.from(ns.slice(1));
    return head;
  }

  equals(o: ListNode | null): boolean {
    if (o === null) return false;
    else if (this.val !== o.val) return false;
    else if (this.next === null && o.next === null) return true;
    else if (this.next === null || o.next === null) return false;
    else return this.next.equals(o.next);
  }

  toString(): string {
    let curr: ListNode | null = this;
    let v: number[] = [];
    while (curr !== null) {
      v.push(curr.val);
      curr = curr.next;
    }
    return `[${v.join(', ')}]`;
  }
}

function mergeTwolists(list1: ListNode | null, list2: ListNode | null): ListNode | null {
  let a = list1, b = list2;
  let v: ListNode[] = [];
  while (a !== null && b !== null) {
    if (a.val < b.val) {
      v.push(new ListNode(a.val));
      a = a.next;
    } else {
      v.push(new ListNode(b.val));
      b = b.next;
    }
  }
  while (a !== null) {
    v.push(new ListNode(a.val));
    a = a.next;
  }
  while (b !== null) {
    v.push(new ListNode(b.val));
    b = b.next;
  }
  
  let res: ListNode | null = null;
  while (v.length !== 0) {
    let temp = v.pop();
    if (temp === undefined) break;
    temp.next = res;
    res = temp;
  }

  return res;
}

class Test {
  input: {
    list1: ListNode | null;
    list2: ListNode | null;
  };
  exp: ListNode | null;
  constructor(list1: number[], list2: number[], exp: number[]) {
    this.input = {
      list1: ListNode.from(list1),
      list2: ListNode.from(list2),
    };
    this.exp = ListNode.from(exp);
  }
}

const tests = [
  new Test([1,2,4], [1,3,4], [1,1,2,3,4,4]),
  new Test([], [], []),
  new Test([], [0], [0]),
]

for (const { input: { list1, list2 }, exp } of tests) {
  const actual = mergeTwolists(list1, list2);
  if ((actual === null && exp === null) || actual?.equals(exp)) {
    console.log(`SUCCESS`);
  } else {
    console.log(`FAILURE: '${(actual || '').toString()}' !== '${(exp || '').toString()}'`)
  }
}