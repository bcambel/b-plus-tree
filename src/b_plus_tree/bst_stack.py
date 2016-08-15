


class Node (object):
    def __init__(self, data):
        self.data = data
        self.left = None
        self.right = None


def inorder(root):
    st = []
    done = 0
    current = root
    iteration = 0
    while(not done and iteration < 100):
        if current is not None:
            st.append(current)
            print "has %s elements " % len(st)
            current = current.left
        else:
            if len(st)>0:
                current = st.pop()
                print current.data
                # if current.right is not None:
                current = current.right
            else:
                done = 1
        iteration += 1


n = Node(1)
n.left = Node(2)
n.right = Node(3)
n.left.left = Node(4)
n.left.left.left = Node(5)
n.left.left.right = Node(7)
n.right.right = Node(6)
n.right.right.left = Node(8)
n.right.right.right = Node(9)
inorder(n)
