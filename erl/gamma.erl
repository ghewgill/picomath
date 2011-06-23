-module(gamma).
-export([gamma/1, log_gamma/1]).

% Visit http://www.johndcook.com/stand_alone_code.html for the source of this code and more like it.

% Note that the functions Gamma and LogGamma are mutually dependent.

gamma(X) when X > 0 ->

    % Split the function domain into three intervals:
    % (0, 0.001), [0.001, 12), and (12, infinity)

    if

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % First interval: (0, 0.001)
            %
            % For small x, 1/Gamma(x) has power series x + gamma x^2  - ...
            % So in this range, 1/Gamma(x) = x + gamma x^2 with error on the order of x^3.
            % The relative error over this interval is less than 6e-7.

        X < 0.001 ->
            Gamma = 0.577215664901532860606512090, % Euler's gamma constant
            1.0/(X*(1.0 + Gamma*X));

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Second interval: [0.001, 12)

        X < 12.0 ->
            % The algorithm directly approximates gamma over (1,2) and uses
            % reduction identities to reduce other arguments to this interval.
            
            Arg_was_less_than_one = (X < 1.0),

            % Add or subtract integers as necessary to bring y into (1,2)
            % Will correct for this below
            [Y, N] = if
                Arg_was_less_than_one ->
                    [X + 1.0, 0];
                true ->
                    N1 = trunc(X) - 1,
                    [X - N1, N1]
            end,

            % numerator coefficients for approximation over the interval (1,2)
            P =
            [
                -1.71618513886549492533811E+0,
                 2.47656508055759199108314E+1,
                -3.79804256470945635097577E+2,
                 6.29331155312818442661052E+2,
                 8.66966202790413211295064E+2,
                -3.14512729688483675254357E+4,
                -3.61444134186911729807069E+4,
                 6.64561438202405440627855E+4
            ],

            % denominator coefficients for approximation over the interval (1,2)
            Q =
            [
                -3.08402300119738975254353E+1,
                 3.15350626979604161529144E+2,
                -1.01515636749021914166146E+3,
                -3.10777167157231109440444E+3,
                 2.25381184209801510330112E+4,
                 4.75584627752788110767815E+3,
                -1.34659959864969306392456E+5,
                -1.15132259675553483497211E+5
            ],

            Z = Y - 1,
            [Num, Den] = gamma_iter(Z, 0.0, 1.0, P, Q),
            Result = Num/Den + 1.0,

            % Apply correction if argument was not initially in (1,2)
            if
                Arg_was_less_than_one ->
                    % Use identity gamma(z) = gamma(z+1)/z
                    % The variable "result" now holds gamma of the original y + 1
                    % Thus we use y-1 to get back the orginal y.
                    Result / (Y - 1.0);
                true ->
                    % Use the identity gamma(z+n) = z*(z+1)* ... *(z+n-1)*gamma(z)
                    gamma_z_n(Result, Y, N)
            end;

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Third interval: [12, infinity)

        X < 171.624 ->
            math:exp(log_gamma(X));

        true ->
            999.0e299

    end.

gamma_iter(_Z, Num, Den, [], []) ->
    [Num, Den];
gamma_iter(Z, Num, Den, [P|Ps], [Q|Qs]) ->
    NewNum = (Num + P) * Z,
    NewDen = Den * Z + Q,
    gamma_iter(Z, NewNum, NewDen, Ps, Qs).

gamma_z_n(Result, _Y, 0) -> Result;
gamma_z_n(Result, Y, N) ->
    gamma_z_n(Result * Y, Y + 1, N - 1).
    

log_gamma(X) when X > 0 ->

    if
        X < 12.0 ->
            math:log(abs(gamma(X)));

        true ->

            % Abramowitz and Stegun 6.1.41
            % Asymptotic series should be good to at least 11 or 12 figures
            % For error analysis, see Whittiker and Watson
            % A Course in Modern Analysis (1927), page 252

            C = lists:reverse(
            [
                         1.0/12.0,
                        -1.0/360.0,
                        1.0/1260.0,
                        -1.0/1680.0,
                        1.0/1188.0,
                        -691.0/360360.0,
                        1.0/156.0,
                        -3617.0/122400.0
            ]),
            Z = 1.0/(X*X),
            Sum = log_gamma_iter(Z, 0, C),
            Series = Sum/X,

            HalfLogTwoPi = 0.91893853320467274178032973640562,
            LogGamma = (X - 0.5)*math:log(X) - X + HalfLogTwoPi + Series,    
            LogGamma
    end.

log_gamma_iter(_Z, Sum, []) ->
    Sum;
log_gamma_iter(Z, Sum, [C|Cs]) ->
    S = (Sum * Z) + C,
    log_gamma_iter(Z, S, Cs).
