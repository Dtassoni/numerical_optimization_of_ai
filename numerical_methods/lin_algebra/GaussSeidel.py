import numpy as np

def GaussSeidel(A, b, x0, delta, max_it):
    """
    A program implementing the Gauss-Seidel iteration method to solve
    the linear system Ax=b.

    Input:
        A: square coefficient matrix
        b: right side vector
        x0: initial guess
        delta: error tolerance for the relative difference between
               two consecutive iterates
        max_it: maximum number of iterations to be allowed

    Output:
        x: numerical solution vector
        iflag: 1 if a numerical solution satisfying the error
                 tolerance is found within max_it iterations
               -1 if the program fails to produce a numerical
                  solution in max_it iterations
        itnum: the number of iterations used to compute x
    """

    n = len(b)
    iflag = 1
    k = 0
    x = x0.copy()
    
    while k < max_it:
        k = k + 1

        # Update x(1), the first component of the solution
        x[0] = (b[0] - np.dot(A[0,1:], x[1:])) / A[0,0]
        for i in range(1, n):
            if i < n - 1:
                # Update x(i), the ith component of the solution
                x[i] = (b[i] - np.dot(A[i,:i], x[:i]) - np.dot(A[i,i+1:], x[i+1:])) / A[i,i]
            else:
                # Update x(n), the last component of the solution
                x[n-1] = (b[n-1] - np.dot(A[n-1,:n-1], x[:n-1])) / A[n-1,n-1]

        # compute relative error
        relerr = np.linalg.norm(x-x0, np.inf)/(np.linalg.norm(x, np.inf)+np.finfo(np.float).eps)
        x0 = x.copy()
        if relerr < delta:
            break
            
    itnum = k
    if itnum == max_it:
        iflag = -1
        
    return x, iflag, itnum


if __name__ == "__main__":
    A = np.array([[10, 2, 1], [1, 5, 1], [2, 3, 10]])
    b = np.array([7, -8, 6])
    x0 = np.zeros(3)
    delta = 1e-6
    max_it = 1000

    x, iflag, itnum = GaussSeidel(A, b, x0, delta, max_it)
    print("Solution vector: ", x)

    expected_x = np.linalg.solve(A, b)
    tolerance = 1e-6
    
    if np.allclose(x, expected_x, rtol=tolerance, atol=tolerance):
        print("Test case passed: the solution is correct.")
    else:
        print("Test case failed: the solution is not correct.") 