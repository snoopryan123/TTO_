
n = 5;
A = diag(ones(n-1,1),-1);
I = eye(n);

Sig_inv = (I-A)' * (I-A);
Sig = inv(Sig_inv);

Sig_inv
Sig

gamma = inv(I-A) * eye(n,1);
gamma


% syms p real
% 
% A = [0, p, 1 - p, 0; 1 - p, 0,     0, p;    0, p, 1 - p, 0; 1 - p, 0, 0, p];
% 
% s = svd(A);
% 
% 
% B = eye(4) - A;

% C = zeros(4);
% N = 14;
% for k = 1:N
%     %disp(k)
%     C = C + A^k;
%     D = simplify(C);
%     e = D(1,1)+D(1,2)+D(2,1)+D(2,2);
%     e = simplify(e);
%     disp(["k = ", k]);
%     disp(e);
%     disp("");
% end


