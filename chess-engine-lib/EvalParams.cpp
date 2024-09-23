#include "EvalParams.h"

#include "Piece.h"

#include <ranges>
#include <sstream>

namespace {

constexpr EvalParamArray kDefaultParams = {
        0.204272,    1.000000,    1.892669,    3.755643,   8.460752,    0.000000,    92.850517,
        68.652824,   368.726654,  252.147614,  363.879700, 267.517059,  484.593170,  481.669312,
        1446.973267, 589.160645,  0.000000,    0.000000,   0.000000,    0.000000,    0.000000,
        0.000000,    0.000000,    0.000000,    0.000000,   0.000000,    0.000000,    0.000000,
        0.000000,    0.000000,    0.000000,    0.000000,   0.000000,    0.000000,    -31.174021,
        15.620411,   -21.528234,  4.575360,    -22.942211, 5.043776,    -9.321198,   -103.212936,
        -5.029473,   -55.292133,  18.310398,   -2.620764,  29.126871,   -14.567578,  -10.183174,
        -6.947440,   -24.370165,  15.548706,   -13.697376, -3.419611,   -9.813615,   1.100000,
        -3.726930,   0.309168,    -3.104637,   -6.380008,  14.387559,   -0.248001,   33.527275,
        -17.797285,  5.663171,    -1.205455,   -19.497143, 12.130645,   -7.373277,   7.715571,
        -0.255919,   -5.726687,   20.445147,   -23.248016, 9.108029,    -11.727152,  16.927080,
        -4.666277,   -5.729686,   -1.442896,   -15.458315, 0.627814,    -11.173035,  40.590462,
        -7.867746,   22.736113,   -7.849900,   10.413094,  12.710978,   -4.011596,   25.723038,
        -13.404277,  7.537780,    -1.117337,   6.225163,   7.557686,    -13.737175,  11.426500,
        -4.377852,   47.994541,   14.341170,   13.434806,  -2.094087,   31.855835,   15.801701,
        -5.862200,   56.380089,   -27.355938,  71.227478,  -25.577745,  130.024948,  -8.889548,
        76.011261,   -25.622309,  161.540298,  66.446175,  172.938950,  51.648365,   30.985737,
        73.055199,   111.816635,  48.027958,   76.641266,  60.758663,   59.762897,   62.667416,
        -39.601128,  122.645714,  -43.012379,  98.158653,  0.000000,    0.000000,    0.000000,
        0.000000,    0.000000,    0.000000,    0.000000,   0.000000,    0.000000,    0.000000,
        0.000000,    0.000000,    0.000000,    0.000000,   0.000000,    0.000000,    -159.717987,
        -39.976707,  -22.139252,  -102.938599, -40.871887, -19.524343,  -23.928074,  -32.352665,
        -25.273823,  -45.562359,  -1.475660,   -36.320889, -19.646049,  -37.446167,  -187.870697,
        -15.704204,  -33.238274,  -44.082787,  -65.896530, 4.795214,    -38.179161,  -24.257608,
        -4.446566,   -6.412634,   -10.859138,  -10.318894, 11.710410,   -20.728537,  -30.059710,
        -40.129341,  -53.821308,  -6.818171,   -36.746300, -41.234215,  -14.571259,  -11.530311,
        -1.920183,   5.393561,    9.787270,    -2.519352,  32.324150,   3.808261,    4.040315,
        -17.788927,  12.701643,   -19.693329,  -43.549774, 23.548393,   -15.472121,  -21.909206,
        -3.732498,   7.866762,    9.810354,    17.578354,  1.620839,    26.048155,   21.028625,
        14.005260,   8.705103,    20.232824,   15.263408,  2.920270,    0.132724,    -7.203104,
        -18.422443,  -14.555144,  -2.559250,   19.445166,  35.623066,   15.187867,   62.734364,
        24.435469,   10.402157,   30.403658,   43.791264,  23.862823,   -16.611359,  29.775434,
        5.565150,    -0.272906,   27.404095,   -20.446239, 19.401272,   3.961670,    64.763840,
        7.855962,    81.554199,   19.237938,   110.135208, -5.048810,   95.446564,   -1.641494,
        -16.267725,  20.083328,   -8.168764,   4.044863,   -133.472214, 34.167397,   -38.121029,
        2.720846,    -14.383992,  14.582592,   62.232655,  18.820850,   3.485823,    0.715811,
        -1.618361,   6.367585,    -29.068472,  -15.862311, 6.795691,    0.144997,    -163.229370,
        -60.127087,  -98.523613,  -44.040348,  -12.538207, -16.729385,  -668.350830, 80.285004,
        -104.816429, 24.513363,   -262.087799, 36.182205,  188.298706,  -36.811142,  -249.805664,
        -52.347698,  -42.614269,  3.109259,    16.614861,  -36.672199,  3.495228,    -40.757160,
        -23.271677,  -5.890242,   -2.325786,   -24.675312, -12.558681,  -41.218800,  -7.528711,
        -26.227688,  -8.237523,   -16.045322,  16.903183,  -42.710968,  6.488977,    -24.010078,
        10.372691,   -15.632936,  1.758694,    -4.581180,  -3.675340,   -8.914783,   14.425355,
        -7.877569,   15.899960,   -30.292892,  -30.838434, -52.227448,  1.456661,    -9.551099,
        13.761917,   5.429126,    5.951515,    4.621200,   0.912350,    6.559043,    -0.130031,
        11.494069,   -1.109864,   -2.368840,   9.471880,   -9.923359,   -10.981688,  -27.763638,
        11.991713,   -5.471097,   -4.844075,   -0.065069,  -10.430633,  35.941372,   37.342598,
        7.423053,    7.224199,    7.656149,    -9.619376,  10.783309,   -16.068075,  10.450485,
        0.663714,    1.036648,    -7.569953,   -18.182905, -14.516216,  22.912184,   4.050784,
        8.057623,    8.037179,    24.899954,   38.953171,  11.869343,   -27.457041,  20.529974,
        -28.549288,  27.035606,   -60.680172,  17.423597,  -23.475977,  13.671439,   -9.408860,
        25.546246,   22.492222,   18.288750,   22.821234,  -2.454067,   24.535799,   5.324658,
        100.066383,  13.684052,   78.519722,   -10.860083, 24.205654,   10.714814,   16.361622,
        -21.683119,  -37.488796,  -1.961998,   -6.608864,  15.611027,   -10.460078,  32.009716,
        -1.675324,   4.834644,    -59.406601,  30.779467,  -27.665230,  15.273802,   -73.923935,
        10.343891,   -141.318970, 52.860046,   -54.962833, 15.512593,   -39.033211,  -0.657061,
        -69.170181,  22.090792,   26.671135,   12.181579,  -127.248703, 23.368895,   107.009827,
        4.836750,    22.062433,   -11.613850,  17.910763,  -29.086699,  9.842690,    -41.074745,
        -1.415848,   -15.349563,  2.093328,    -16.215265, -6.128570,   -19.742260,  -5.126172,
        -15.484481,  -15.533269,  -22.508341,  -9.613687,  -44.109573,  -43.921207,  -4.647410,
        -31.069357,  -12.827367,  -28.606249,  -8.307733,  -21.903107,  -6.368104,   -10.389291,
        -23.983999,  -23.252777,  1.036865,    -38.138786, -10.412560,  -54.118282,  -20.742708,
        -16.072693,  -11.348067,  -37.467800,  4.247413,   -28.186008,  2.162458,    -26.672047,
        -1.972177,   -29.817778,  1.215684,    -29.176085, 2.000663,    -7.380586,   -16.237986,
        -45.339935,  1.917714,    -41.644230,  20.962296,  -45.995686,  27.860744,   -37.635983,
        24.286863,   -19.329540,  10.985421,   -45.021378, 24.986156,   -18.399130,  12.598496,
        -25.590639,  0.057430,    -24.525047,  7.788298,   -34.386223,  29.376245,   -44.943512,
        45.944988,   -18.882753,  36.188042,   -17.069098, 31.444687,   -78.537209,  50.346588,
        1.983026,    26.437529,   -25.612274,  19.497786,  -72.601509,  29.041779,   -10.601735,
        29.731508,   -21.597883,  34.177601,   -9.763717,  31.542448,   5.892037,    19.116625,
        13.325093,   25.859320,   45.281895,   9.550442,   82.195877,   5.233504,    -30.452303,
        24.724728,   -21.896141,  37.494778,   12.832337,  28.148399,   6.174506,    37.505245,
        11.515768,   32.320332,   60.137978,   21.489351,  77.449623,   15.195263,   41.785606,
        21.543221,   32.943245,   20.111118,   31.599796,  -2.462487,   27.596285,   17.193798,
        45.804279,   16.507229,   20.649185,   28.460861,  51.119762,   21.032026,   49.533562,
        31.364079,   97.560890,   8.636324,    25.491199,  18.198544,   49.567501,   -103.615387,
        8.715677,    -12.870006,  19.129728,   -82.435966, 32.624657,   -44.464897,  37.320137,
        -88.685844,  12.353897,   -78.759956,  -3.197917,  -46.151306,  -75.910660,  5.486372,
        -53.623768,  18.425428,   -25.247263,  10.634206,  13.366770,   -46.390274,  6.682643,
        -18.428988,  5.543037,    -17.350206,  17.506258,  -52.465523,  2.008842,    -94.677628,
        -4.136578,   -132.035812, -38.085377,  44.617569,  -27.303837,  -15.393298,  -30.000694,
        15.130064,   -18.105444,  -17.488558,  -11.060791, -7.459143,   -12.894396,  -20.470747,
        -13.208329,  6.548760,    -25.933662,  -0.583760,  -31.590929,  16.678139,   -30.959211,
        18.311417,   -57.041275,  31.919044,   -48.249428, 25.640905,   -36.589016,  4.839212,
        -23.015808,  -6.449313,   -37.974754,  6.551178,   -37.542397,  17.391556,   -24.752085,
        10.857609,   -44.286976,  35.423233,   -52.431507, 41.663574,   -74.397522,  56.443447,
        -63.522327,  31.531399,   -55.227455,  36.150425,  -64.485687,  67.506416,   -41.621017,
        26.030371,   -14.731697,  -5.030943,   -34.468067, 43.019680,   -63.049213,  79.543610,
        -65.249893,  58.667221,   -29.584036,  18.005606,  3.903122,    10.008021,   48.057266,
        -62.313309,  -56.378933,  72.551163,   -6.801068,  -13.275840,  -61.723415,  90.223015,
        -56.248844,  67.878708,   -47.848339,  33.624676,  -73.040207,  70.889977,   -53.193501,
        52.910976,   -29.859171,  15.403592,   11.200954,  -20.874086,  -10.323639,  45.922035,
        5.563303,    -1.292042,   -94.978859,  88.859680,  -48.580597,  57.540920,   31.333851,
        -6.271765,   57.292641,   1.664169,    -85.739975, 85.389732,   43.252018,   -29.792028,
        -44.616165,  5.234781,    41.102707,   -46.741875, -0.354228,   -13.630446,  -118.547409,
        20.647120,   -11.929351,  -21.143417,  -51.354733, -6.254351,   6.572605,    -24.378542,
        -20.547714,  -41.731155,  -29.096624,  -9.121011,  -42.782196,  -3.964876,   -20.232227,
        -10.321869,  -25.963263,  13.255975,   -18.540543, 8.361788,    20.167025,   -0.757847,
        26.840252,   -2.985000,   18.251963,   -31.405653, -104.029678, -10.244731,  4.953716,
        -20.900009,  -1.882142,   4.452606,    -4.169506,  16.277441,   0.150557,    23.428253,
        12.948064,   12.575319,   -10.292176,  2.150050,   -20.027216,  -8.325878,   2.357754,
        -11.413137,  -24.227701,  7.029200,    20.663597,  16.988417,   -20.564013,  34.965893,
        -13.024124,  30.906092,   28.374720,   19.855885,  -34.825893,  25.639248,   -61.187332,
        -2.312115,   39.798172,   -1.884259,   -63.047325, 49.764160,   35.231873,   25.183968,
        -109.751221, 46.971252,   25.027172,   34.014450,  -13.922676,  46.629581,   11.288679,
        40.714485,   -23.667856,  24.899189,   49.915764,  -4.804743,   50.399658,   32.395660,
        29.487961,   43.634228,   -84.085297,  46.631252,  -133.262482, 60.381214,   -71.066360,
        61.008591,   -119.762497, 80.968620,   -41.207279, 33.504177,   1.870638,    -13.069745,
        31.048359,   30.926186,   43.644882,   18.726419,  30.032343,   25.630205,   -77.626335,
        50.159119,   -55.484085,  36.604408,   -32.859913, 36.559032,   -141.964920, 28.810904,
        120.878235,  -58.242538,  104.701370,  -14.912907, 94.515587,   -24.477774,  47.021717,
        -40.306866,  166.913727,  -65.966721,  26.174913,  0.777718,    9.716515,    0.855177,
        -102.569344, -59.489193,  0.000000,    0.000000,   90.000000,   90.000000,   63.790215,
        98.839149,   21.682446,   61.679153,   -1.745439,  41.314713,   -1.705235,   10.679426,
        7.555842,    12.998089,   21.681740,   9.506799,   12.456367,   8.200947,    23.396000,
        45.452812,   13.399749,   19.810282,   11.276615,  11.825466,   2.515200,    6.154844,
        0.000000,    0.000000,    -3.230076,   -13.421757, -14.690777,  -25.398605,  -17.757435,
        -16.425425,  -29.442120,  -93.614372,  10.435486,  83.072273,   3.089101,    1.171130,
        -11.848592,  10.223253,   14.603731,   20.750046,  36.462086,   -1.395877,   -300.787903,
        -21.074299,  33.001122,   -38.845772,  -21.101246, -8.660446,   0.864362,    0.125961,
        0.000000,    0.000000,    0.692462,    11.774322,  4.049002,    26.710598,   2.760495,
        41.864609,   -0.358524,   84.301590,   268.242584, -130.089752, 59.528042,   -48.353264,
        -12.271854,  -9.176639,   -6.300391,   -3.147504,  0.000000,    0.000000,    -5.170364,
        7.212988,    -6.210545,   18.300640,   -1.235747,  16.176132,   -0.028395,   20.554834,
        2.202382,    1.871919,    0.000000,    0.000000,   -1.214350,   2.011967,    2.465370,
        3.417135,    3.168167,    2.114452,    0.980166,   4.464880,    0.000000,    0.000000,
        0.000000,    0.000000,    4.977820,    -4.227230,  3.250489,    -3.995589,   6.564210,
        -5.525754,   21.497625,   4.532190,    0.000000,   0.000000};

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

void writeSquareTable(const SquareTable& squareTable, std::ostream& out) {
    for (int rank = kRanks - 1; rank >= 0; --rank) {
        out << "\t\t";
        for (int file = 0; file < kFiles; ++file) {
            const auto& value = squareTable[rank * kFiles + file];
            out << std::format("{}, ", taperedTermToString(value));
        }
        out << "\n";
    }
}

void writePieceSquareTables(const PieceSquareTables& pieceSquareTables, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        out << "\t" << pieceToString((Piece)pieceIdx) << ": {\n";
        writeSquareTable(pieceSquareTables[pieceIdx], out);
        out << "\t}\n";
    }
}

template <std::size_t N>
std::string arrayToString(const std::array<TaperedTerm, N>& valueArray) {
    std::string inner = valueArray | std::ranges::views::transform(taperedTermToString)
                      | std::ranges::views::join_with(std ::string(", "))
                      | std::ranges::to<std::string>();
    return "{" + inner + "}";
}

void writeTaperedTermPerPiece(
        const std::array<TaperedTerm, kNumPieceTypes>& terms, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n", pieceToString((Piece)pieceIdx), taperedTermToString(terms[pieceIdx]));
    }
}

}  // namespace

EvalParams EvalParams::getEmptyParams() {
    return EvalParams{};
}

EvalParams EvalParams::getDefaultParams() {
    return evalParamsFromArray(kDefaultParams);
}

EvalParamArray evalParamsToArray(const EvalParams& params) {
    EvalParamArray array;
    std::memcpy(array.data(), &params, sizeof(EvalParams));
    return array;
}

EvalParams evalParamsFromArray(const EvalParamArray& array) {
    EvalParams params = EvalParams::getEmptyParams();
    std::memcpy(&params, array.data(), sizeof(EvalParams));
    return params;
}

std::string evalParamsToString(const EvalParams& params) {
    std::ostringstream oss;

    oss << "phaseMaterialValues: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {:>6.1f}\n",
                pieceToString((Piece)pieceIdx),
                params.phaseMaterialValues[pieceIdx]);
    }
    oss << "}\n";

    oss << "\npieceValues: {\n";
    writeTaperedTermPerPiece(params.pieceValues, oss);
    oss << "}\n";

    oss << "\npieceSquareTablesWhite: {\n";
    writePieceSquareTables(params.pieceSquareTablesWhite, oss);
    oss << "}\n";

    oss << std::format("\npassedPawnBonus:\n\t{}", arrayToString(params.passedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

    oss << std::format(
            "\nbishopPawnSameColorBonus:\n\t{}", arrayToString(params.bishopPawnSameColorBonus));

    oss << std::format("\nbishopPairBonus:\n\t{}", taperedTermToString(params.bishopPairBonus));

    oss << std::format("\nknightPairBonus:\n\t{}", taperedTermToString(params.knightPairBonus));

    oss << std::format("\nrookPairBonus:\n\t{}", taperedTermToString(params.rookPairBonus));

    oss << std::format(
            "\nrookSemiOpenFileBonus:\n\t{}", taperedTermToString(params.rookSemiOpenFileBonus));

    oss << std::format("\nrookOpenFileBonus:\n\t{}", taperedTermToString(params.rookOpenFileBonus));

    oss << std::format("\nknightPawnAdjustment:\n\t{}", arrayToString(params.knightPawnAdjustment));

    oss << std::format("\nrookPawnAdjustment:\n\t{}", arrayToString(params.rookPawnAdjustment));

    oss << std::format(
            "\nkingVirtualMobilityPenalty:\n\t{}",
            taperedTermToString(params.kingVirtualMobilityPenalty));

    oss << "\n\nmobilityBonus: {\n";
    writeTaperedTermPerPiece(params.mobilityBonus, oss);
    oss << "}\n";

    oss << "\nkingTropismBonus: {\n";
    writeTaperedTermPerPiece(params.kingTropismBonus, oss);
    oss << "}";

    return oss.str();
}
