#!/usr/bin/env ts-node

class ListNode {
  val: number;
  next: ListNode | null;
  constructor(val?: number, next?: ListNode | null) {
    this.val = val === undefined ? 0 : val;
    this.next = next === undefined ? null : next;
  }

  static from(n: number[]): ListNode | null {
    if (n.length === 0) return null;
    let head = new ListNode(n[0]);
    head.next = ListNode.from(n.slice(1));
    return head;
  }

  equals(o: ListNode): boolean {
    if (this.next === null && o.next === null)
      return this.val === o.val;
    else if (this.next === null || o.next === null)
      return false;
    else return this.val === o.val && this.next.equals(o.next);
  }

  toArray(): number[] {
    let res: number[] = [];
    let cur: ListNode | null = this;
    while (cur !== null) {
      res.push(cur.val);
      cur = cur.next;
    }
    return res;
  }
}

function equalsListNode(l1?: ListNode | null, l2?: ListNode | null): boolean {
  if (l1 === null && l2 === null) return true;
  else if (l1 === null || l2 === null) return false;
  else return l1!.equals(l2!);
}

function addTwoNumbers(l1: ListNode | null, l2: ListNode | null): ListNode | null {
  let carry: number = 0;
  let a = l1, b = l2;
  let stack: ListNode[] = [];
  while (a !== null && b !== null) {
    stack.push(new ListNode((a.val + b.val + carry) % 10));
    carry = Math.floor((a.val + b.val + carry) / 10);
    a = a.next;
    b = b.next;
  }
  while ( a !== null) {
    stack.push(new ListNode((a.val + carry) % 10));
    carry = Math.floor((a.val + carry) / 10);
    a = a.next;
  }
  while (b !== null) {
    stack.push(new ListNode((b.val + carry) % 10));
    carry = Math.floor((b.val + carry) / 10);
    b = b.next
  }
  if (carry > 0) stack.push(new ListNode(carry));

  let res: ListNode | null = null;
  while (stack.length !== 0) {
    let temp = stack.pop();
    if (temp === undefined) break;
    temp.next = res;
    res = temp;
  }
  return res;
}

class Test {
  input: {
    l1: ListNode | null;
    l2: ListNode | null;
  };
  exp: ListNode | null;
  constructor(_l1: number[], _l2: number[], exp: number[]) {
    const l1 = ListNode.from(_l1);
    const l2 = ListNode.from(_l2);
    this.input = { l1, l2 };
    this.exp = ListNode.from(exp);
  }
};

function runTest({ input: { l1, l2 }, exp }: Test) {
  const actual = addTwoNumbers(l1, l2);
  if (equalsListNode(exp, actual)) {
    console.log('PASSED');
  } else {
    console.log(`FAILED: ${JSON.stringify(exp)} !== ${JSON.stringify(actual)}`);
  }
}

const tests: Test[] = [
  new Test([2,4,3], [5,6,4], [7,0,8]),
  new Test([0], [0], [0]),
  new Test([9,9,9,9,9,9,9], [9,9,9,9], [8,9,9,9,0,0,0,1]),
];

for (const t of tests) {
  runTest(t);
}