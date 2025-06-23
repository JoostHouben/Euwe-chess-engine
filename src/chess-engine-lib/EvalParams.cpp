#include "EvalParams.h"

#include "Piece.h"
#include "RangePatches.h"

#include <format>
#include <ranges>
#include <sstream>

#include <cstring>

namespace {

constexpr EvalParamArray kDefaultParams = {
        0.209377f,    1.000000f,    0.762991f,    1.679964f,    6.320265f,    0.000000f,
        0.209377f,    1.076343f,    0.309144f,    0.159447f,    0.308911f,    1.000000f,
        -0.000000f,   -0.000000f,   -0.018201f,   0.000614f,    30.835546f,   -28.175819f,
        134.784225f,  333.002930f,  350.032654f,  320.528534f,  396.017242f,  445.457642f,
        676.588928f,  1168.050659f, 1083.137329f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -43.670414f,  131.691971f,  -64.575798f,
        128.254547f,  -63.305969f,  135.806595f,  -75.990936f,  134.941147f,  -66.496971f,
        154.844818f,  -22.098846f,  137.513382f,  -12.300807f,  127.168762f,  -33.507637f,
        126.636147f,  -34.951668f,  132.901520f,  -48.350025f,  124.368591f,  -44.977379f,
        134.551178f,  -49.180782f,  145.940491f,  -41.056190f,  142.734726f,  -18.859812f,
        130.818359f,  -15.568997f,  124.915802f,  -19.365114f,  128.248825f,  -19.137735f,
        139.816467f,  -21.936922f,  137.055878f,  -22.697222f,  142.708221f,  -19.325073f,
        136.988220f,  -10.523364f,  126.824913f,  -4.982469f,   138.956863f,  -15.858902f,
        142.221512f,  -19.401283f,  139.516159f,  -10.545951f,  162.843094f,  -1.673787f,
        139.686157f,  -9.725768f,   148.459106f,  -4.594862f,   137.963135f,  8.226544f,
        136.944763f,  18.634293f,   129.879654f,  3.383830f,    145.353149f,  -8.617970f,
        149.310394f,  11.604570f,   167.268112f,  4.044479f,    160.296555f,  21.767403f,
        171.491470f,  12.469229f,   127.362671f,  37.635342f,   124.509598f,  76.140938f,
        169.151367f,  94.687439f,   158.472977f,  33.821404f,   168.117264f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    283.666443f,  280.697144f,  295.444061f,  314.250092f,  291.644623f,
        347.638367f,  298.111511f,  350.105072f,  295.622131f,  347.117981f,  305.583282f,
        328.146271f,  283.704193f,  319.395203f,  246.807404f,  325.000549f,  292.200470f,
        325.362640f,  319.742950f,  334.986237f,  304.204071f,  342.420837f,  325.813782f,
        349.596954f,  321.445618f,  348.360443f,  309.893646f,  360.088745f,  300.916412f,
        358.345581f,  299.621918f,  314.154236f,  300.202393f,  325.828278f,  328.681671f,
        345.423828f,  331.235291f,  338.829224f,  346.486755f,  362.613922f,  349.966858f,
        359.412048f,  335.515015f,  335.158722f,  338.214569f,  321.543030f,  301.480713f,
        323.977295f,  334.924377f,  359.080963f,  368.982635f,  343.451752f,  368.024170f,
        352.849792f,  352.704468f,  356.889648f,  361.645477f,  367.546509f,  355.338257f,
        359.753021f,  341.391449f,  360.119965f,  332.657776f,  348.126099f,  343.001038f,
        359.589874f,  354.365540f,  364.359863f,  373.465302f,  360.899689f,  371.416321f,
        369.076752f,  372.252594f,  357.476440f,  381.510742f,  355.664185f,  356.428955f,
        352.945862f,  360.022614f,  351.478882f,  331.281250f,  368.483307f,  360.740082f,
        375.132538f,  391.550842f,  348.966492f,  390.631531f,  350.284698f,  435.862030f,
        342.157837f,  401.924683f,  349.560516f,  386.285797f,  353.282288f,  318.808044f,
        354.341217f,  312.058777f,  375.190002f,  339.587097f,  363.598480f,  366.568115f,
        358.223358f,  357.557648f,  365.712402f,  381.454681f,  361.663147f,  384.495270f,
        339.277527f,  357.301880f,  352.026123f,  325.009338f,  362.255493f,  185.920776f,
        323.885040f,  273.810425f,  373.780182f,  285.584351f,  398.362183f,  339.491272f,
        378.483276f,  323.602386f,  375.505188f,  477.343109f,  364.276489f,  185.199677f,
        377.056183f,  146.887131f,  213.361755f,  272.518951f,  384.330719f,  319.394379f,
        375.492920f,  297.304718f,  386.405701f,  290.577545f,  390.643311f,  290.732849f,
        384.939301f,  296.342712f,  383.457092f,  312.072601f,  381.924561f,  303.389679f,
        336.133545f,  317.160736f,  389.160583f,  305.819000f,  375.910004f,  323.791321f,
        393.568726f,  306.787201f,  393.720398f,  318.626984f,  394.633575f,  326.794006f,
        387.124878f,  335.523315f,  379.891266f,  329.346222f,  345.635834f,  307.083588f,
        370.323090f,  327.382324f,  392.708405f,  324.206238f,  397.755493f,  328.860474f,
        396.003418f,  330.523804f,  407.279633f,  329.241333f,  394.986481f,  330.831055f,
        381.800812f,  321.092743f,  392.964142f,  321.877686f,  390.525452f,  332.622498f,
        390.909851f,  324.970947f,  405.612213f,  341.929565f,  406.125183f,  344.766815f,
        405.367676f,  330.487457f,  400.364655f,  346.080383f,  391.422821f,  332.185608f,
        392.427582f,  310.218597f,  411.118225f,  323.520325f,  407.347900f,  340.516052f,
        409.318024f,  361.351227f,  399.819916f,  355.308838f,  408.473145f,  344.970398f,
        407.055908f,  328.249847f,  407.003754f,  313.617767f,  399.721771f,  320.916321f,
        407.216309f,  327.883820f,  413.352966f,  333.594879f,  402.981750f,  362.304443f,
        399.102386f,  350.781586f,  414.050415f,  430.991516f,  381.402008f,  375.165802f,
        399.769287f,  366.673157f,  385.557251f,  301.331024f,  399.855865f,  321.390472f,
        411.830261f,  334.043610f,  417.803894f,  302.569763f,  408.339508f,  367.498657f,
        390.332703f,  330.245483f,  406.072937f,  363.715851f,  387.582611f,  312.035217f,
        372.154114f,  284.223877f,  410.661133f,  303.680267f,  409.853943f,  297.052002f,
        408.483704f,  309.148560f,  408.481201f,  291.589172f,  414.874603f,  255.019241f,
        409.991699f,  314.512268f,  375.388641f,  371.468292f,  378.846069f,  456.329529f,
        652.732849f,  448.905853f,  654.833923f,  444.774811f,  664.643799f,  444.385529f,
        660.428162f,  445.860779f,  656.410522f,  438.775116f,  666.629578f,  430.123108f,
        663.461975f,  432.259583f,  644.528931f,  447.189941f,  657.494080f,  438.907227f,
        655.935059f,  432.125092f,  667.720581f,  437.027618f,  667.113098f,  431.884705f,
        670.456909f,  439.814270f,  663.065063f,  432.585358f,  654.882202f,  389.984802f,
        674.188538f,  425.578339f,  676.852539f,  437.536469f,  662.262817f,  425.637695f,
        677.641296f,  428.791840f,  672.632080f,  422.941986f,  673.486755f,  410.734772f,
        682.759644f,  417.676208f,  681.263733f,  431.354889f,  661.613525f,  434.053070f,
        687.191284f,  425.867889f,  693.864563f,  426.561188f,  693.289246f,  428.126984f,
        689.209656f,  425.606262f,  695.283020f,  420.256134f,  695.123291f,  443.118958f,
        693.389954f,  415.086395f,  697.834290f,  439.450500f,  702.802551f,  431.184235f,
        705.092651f,  450.203278f,  693.417175f,  469.768707f,  687.015564f,  453.898254f,
        695.633423f,  460.483887f,  696.326477f,  421.771881f,  704.970581f,  444.701324f,
        697.248169f,  429.238251f,  709.799255f,  465.224243f,  695.223816f,  466.953979f,
        691.250916f,  481.773071f,  687.455505f,  489.761810f,  682.707581f,  491.799591f,
        689.257629f,  497.640839f,  682.416138f,  495.054352f,  693.047546f,  451.391296f,
        705.669922f,  463.570312f,  707.231689f,  489.701904f,  692.945129f,  503.434204f,
        693.583862f,  475.374390f,  699.827026f,  504.336212f,  687.402710f,  466.926025f,
        700.709656f,  469.946930f,  697.701538f,  473.461365f,  683.163574f,  466.531738f,
        695.999756f,  480.378479f,  691.993408f,  457.228363f,  699.361450f,  445.310760f,
        705.468201f,  447.058472f,  709.458252f,  425.312866f,  717.572083f,  513.773987f,
        687.658386f,  1183.217773f, 1003.520081f, 1170.152222f, 1018.907288f, 1161.305176f,
        1032.499023f, 1165.787354f, 1016.738586f, 1173.752075f, 1007.283447f, 1138.725464f,
        1011.728271f, 1150.764893f, 918.984558f,  1130.744995f, 1007.475952f, 1175.330566f,
        1054.052246f, 1181.246216f, 1015.433594f, 1182.516479f, 1033.214111f, 1176.052734f,
        1036.969971f, 1170.606812f, 1044.801636f, 1170.738647f, 1008.788330f, 1169.706299f,
        961.332520f,  1128.355347f, 1078.370605f, 1177.566040f, 1055.412598f, 1171.980347f,
        1071.461182f, 1166.215942f, 1071.686279f, 1165.232422f, 1076.862671f, 1167.650024f,
        1071.807739f, 1166.598145f, 1069.816650f, 1173.179810f, 1044.111694f, 1161.671387f,
        1068.643799f, 1169.432739f, 1102.494141f, 1165.401123f, 1095.391235f, 1165.702881f,
        1118.036133f, 1151.686646f, 1125.122559f, 1168.616333f, 1105.922485f, 1164.079712f,
        1123.446167f, 1157.611938f, 1111.910522f, 1153.007812f, 1149.979370f, 1178.233887f,
        1079.371582f, 1161.773193f, 1118.323364f, 1143.408569f, 1179.875366f, 1142.492065f,
        1150.904785f, 1148.321899f, 1165.349854f, 1155.802368f, 1165.833252f, 1142.982178f,
        1178.587891f, 1160.412842f, 1141.377808f, 1180.779175f, 1103.406494f, 1150.661255f,
        1131.157104f, 1122.403809f, 1197.142334f, 1144.161743f, 1177.541016f, 1131.638550f,
        1215.773560f, 1197.190308f, 1152.689819f, 1218.687744f, 1079.691284f, 1158.967285f,
        1172.418579f, 1155.563354f, 1132.486206f, 1140.860840f, 1146.876831f, 1168.397827f,
        1145.816040f, 1150.165649f, 1166.420166f, 1119.632446f, 1221.454712f, 1140.164307f,
        1168.500122f, 1171.822510f, 1130.183960f, 1194.060425f, 1118.186035f, 1165.686035f,
        1118.123901f, 1197.096436f, 1084.151855f, 1159.493286f, 1163.344849f, 1131.103149f,
        1183.542114f, 1119.267090f, 1220.805176f, 1194.668213f, 1154.421753f, 1196.129395f,
        1138.566528f, 1214.910645f, 1127.286621f, 60.497967f,   -52.333466f,  24.659317f,
        -20.656544f,  -38.433231f,  5.593589f,    -51.811565f,  -17.066416f,  -36.729187f,
        -16.553537f,  -55.879925f,  11.533537f,   25.890749f,   -4.740532f,   47.966785f,
        -17.791256f,  24.039148f,   -36.914936f,  -24.151079f,  -5.972700f,   -82.335213f,
        5.472371f,    -101.890244f, 2.705032f,    -93.907768f,  4.267921f,    -62.174641f,
        1.926889f,    -6.069955f,   -10.172329f,  22.829382f,   -28.162569f,  -29.952635f,
        -27.253571f,  -41.743561f,  -10.847655f,  -114.205154f, 4.438155f,    -111.131859f,
        5.097299f,    -118.611526f, 10.033728f,   -110.874268f, 11.237903f,   -59.590500f,
        -11.192346f,  -36.367626f,  -26.122469f,  -11.924559f,  -29.934464f,  -14.713767f,
        -16.637247f,  -58.696697f,  1.241709f,    -118.740860f, 10.387526f,   -86.545296f,
        4.703752f,    -133.548126f, 13.750587f,   -71.732872f,  -12.499822f,  -85.629288f,
        -35.386448f,  16.444880f,   -27.563299f,  0.790669f,    -4.043749f,   -17.235216f,
        0.108446f,    -58.961285f,  12.916663f,   -45.501743f,  11.807032f,   -10.968052f,
        -1.952624f,   -26.521362f,  -16.715263f,  -59.046341f,  -21.096157f,  14.872725f,
        9.358932f,    -2.811833f,   25.521675f,   6.393890f,    15.475238f,   9.190863f,
        6.165630f,    75.529373f,   -11.687627f,  39.658962f,   25.516819f,   57.337067f,
        11.508721f,   16.779005f,   -12.723372f,  -0.163468f,   -7.710348f,   102.824234f,
        1.161809f,    58.440613f,   8.633333f,    74.277496f,   -2.023672f,   -20.886360f,
        5.397202f,    93.873665f,   -19.146259f,  17.711723f,   41.779102f,   159.528488f,
        -88.011086f,  65.946312f,   -52.597733f,  238.169235f,  -90.537727f,  114.485237f,
        -7.225312f,   75.861649f,   -13.172987f,  21.184538f,   -33.338104f,  185.321686f,
        -57.415382f,  -77.524254f,  58.325890f,   154.186203f,  -140.607986f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    22.192846f,   68.036522f,   30.387445f,
        38.427437f,   24.794334f,   60.410831f,   32.114731f,   137.495331f,  40.610096f,
        102.703430f,  102.022408f,  41.791847f,   118.356697f,  39.975430f,   78.438522f,
        63.311634f,   50.239746f,   74.424271f,   15.573187f,   78.857262f,   28.638649f,
        96.329880f,   55.844452f,   52.590691f,   31.947510f,   94.303741f,   74.521629f,
        75.711571f,   114.879517f,  31.676477f,   80.038094f,   54.808636f,   67.751152f,
        132.867783f,  50.048012f,   119.484673f,  59.012135f,   123.113434f,  76.771133f,
        97.548019f,   97.658913f,   97.159233f,   110.276016f,  104.020050f,  96.802322f,
        96.757484f,   96.743889f,   83.274963f,   58.669041f,   248.571152f,  83.213425f,
        230.950485f,  99.070015f,   181.986099f,  122.746735f,  150.163513f,  137.741043f,
        123.572098f,  156.303101f,  139.429413f,  151.526154f,  102.332458f,  193.830048f,
        108.996651f,  126.502846f,  330.256470f,  65.430511f,   314.834381f,  76.388000f,
        284.768005f,  87.154610f,   276.125580f,  208.160736f,  180.379440f,  185.021225f,
        159.044983f,  192.521576f,  156.579605f,  151.919785f,  186.753784f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -37.138573f,
        69.890282f,   -87.890945f,  80.104897f,   -64.917419f,  67.908859f,   -81.565018f,
        30.331308f,   -75.263824f,  85.281487f,   -20.727854f,  47.236420f,   -16.717436f,
        34.654049f,   -40.316113f,  54.977158f,   -18.865240f,  82.032509f,   -49.292080f,
        70.705658f,   -45.544621f,  79.312447f,   -66.112686f,  62.762207f,   -60.265591f,
        74.432755f,   -10.094207f,  67.775696f,   10.915551f,   46.133934f,   3.786294f,
        55.973782f,   49.251762f,   118.216499f,  35.489113f,   103.445961f,  0.799871f,
        101.026733f,  22.183954f,   81.093338f,   -2.743112f,   89.311142f,   9.319393f,
        104.573242f,  2.746618f,    125.528183f,  22.172199f,   108.672356f,  115.692070f,
        193.909286f,  103.340904f,  171.570694f,  90.518059f,   146.379883f,  105.593163f,
        106.269966f,  120.251862f,  109.140930f,  120.681847f,  121.107635f,  34.286156f,
        165.595169f,  63.704998f,   157.127670f,  221.609512f,  266.432495f,  245.315216f,
        260.218079f,  262.835022f,  213.038361f,  220.111755f,  188.596329f,  289.239594f,
        155.950043f,  336.945648f,  143.246948f,  233.787292f,  187.624939f,  153.379105f,
        216.806274f,  454.684998f,  251.379944f,  428.376251f,  304.974487f,  392.434204f,
        301.597778f,  432.370819f,  224.504868f,  405.635712f,  207.011719f,  261.034241f,
        223.121780f,  184.910950f,  238.756912f,  275.411865f,  242.942932f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    9.611371f,    7.552176f,    24.745626f,
        57.350967f,   29.931295f,   -7.754506f,   3.587147f,    16.012697f,   -0.382377f,
        -0.349474f,   -0.260405f,   -0.177891f,   -0.185941f,   0.088075f,    33.962818f,
        11.502635f,   35.946716f,   1.686979f,    23.628216f,   3.598700f,    14.712154f,
        3.008033f,    0.000000f,    0.000000f,    -12.867014f,  5.342281f,    -24.840012f,
        2.595601f,    -37.584305f,  41.883293f,   -22.187584f,  37.200111f,   27.956545f,
        0.160129f,    29.398392f,   -10.266085f,  14.641242f,   1.648668f,    10.136114f,
        0.500849f,    0.000000f,    0.000000f,    -11.082203f,  1.462289f,    -19.992998f,
        1.672261f,    -37.637119f,  43.066681f,   -40.222874f,  25.934147f,   32.309818f,
        49.782314f,   3.198841f,    -10.176712f,  -22.429455f,  -25.115929f,  19.742918f,
        0.484678f,    36.480381f,   -0.696144f,   18.037666f,   19.863699f,   28.075094f,
        -29.834280f,  15.601559f,   -66.703003f,  -2.849609f,   20.188295f,   -118.686874f,
        46.872963f,   -150.051819f, 46.191238f,   4.352944f,    -22.463495f,  -1.033312f,
        -5.173254f,   0.000000f,    0.000000f,    -5.330787f,   14.771686f,   -11.873743f,
        40.495911f,   -17.412880f,  65.304337f,   -20.686541f,  94.378258f,   -245.749451f,
        105.017151f,  -141.864914f, 37.995777f,   32.239548f,   -35.669777f,  28.819813f,
        -29.293623f,  0.000000f,    0.000000f,    -2.785247f,   21.456179f,   -21.256414f,
        55.555870f,   -39.478054f,  100.164177f,  -30.261133f,  85.228996f,   57.167435f,
        -121.778374f, 170.892197f,  -218.276367f, -114.011757f, 29.205793f,   -83.548187f,
        29.096075f,   0.000000f,    0.000000f,    63.232601f,   -8.837706f,   177.242905f,
        -123.278198f, 281.796387f,  -186.082733f, 335.618835f,  -162.491776f, -0.394650f,
        2.023039f,    4.745181f,    0.952537f,    3.574932f,    2.180880f,    1.971969f,
        1.109196f,    -7.309593f,   -40.608654f,  -5.297132f,   -17.088663f,  -3.873729f,
        -5.012810f,   -2.048473f,   -2.339617f,   0.000000f,    0.000000f,    1.858290f,
        0.682794f,    7.589122f,    3.056426f,    13.426835f,   -2.289895f,   3.565325f,
        -12.564706f,  6.923994f,    -32.267017f,  6.142135f,    -10.777932f,  7.001954f,
        -7.990814f,   3.367688f,    -1.197416f,   0.000000f,    0.000000f,    -3.252933f,
        2.016539f,    -8.036234f,   3.005650f,    -9.303345f,   2.740649f,    6.253469f,
        -3.248604f,   11.174699f,   -38.954693f,  10.915802f,   -15.151732f,  7.935377f,
        -5.344041f,   5.753987f,    -2.668450f,   0.000000f,    0.000000f,    -1.128397f,
        0.756456f,    -4.902527f,   4.901671f,    -5.978880f,   2.029822f,    1.153422f,
        -6.012890f,   -15.887448f,  -119.401260f, 2.715208f,    -83.535515f,  3.654662f,
        -59.962715f,  0.400850f,    -13.472029f,  0.000000f,    0.000000f,    -2.900813f,
        9.919512f,    -7.700551f,   24.242998f,   -6.925254f,   24.948944f,   -6.576272f,
        27.005304f,   5.812469f,    -1.251217f,   1.854200f,    1.756948f,    1.073219f,
        -1.052294f,   2.514389f,    -1.018627f,   2.998088f,    -1.443085f,   6.046733f,
        -1.821000f,   4.735244f,    0.899675f,    6.741263f,    10.914034f,   -5.610143f,
        7.727580f,    -2.128415f,   -2.959538f,   0.672455f,    -0.236887f,   -3.313035f,
        -0.235588f,   4.782048f,    -5.598070f,   3.698187f,    -6.348732f,   -0.329259f,
        -3.970757f,   -0.552893f,   -4.420693f,   -8.039852f,   -5.383222f,   -0.980172f,
        -4.803372f,   20.000000f,   10.000000f,   -0.611637f,   -6.393401f,   -7.534572f,
        -38.492363f,  8.147134f,    -16.332958f,  8.166889f,    1.113768f,    -32.767685f,
        -28.857096f,  -0.201051f,   -13.858440f,  2.543847f,    12.446363f,   -40.854439f,
        -28.046751f,  -12.891953f,  -5.364527f,   6.165899f,    8.607063f,    -55.018314f,
        -6.606330f,   -5.381958f,   -2.544815f,   0.841929f,    35.019051f,   -62.108437f,
        -33.893993f,  -43.197983f,  -1.065528f,   0.000000f,    0.000000f,    -9.217955f,
        -8.806442f,   -15.131871f,  -5.026141f,   -7.461194f,   -5.958033f,   -1.172975f,
        -8.561230f,   21.260677f,   -15.099375f,  59.572430f,   -24.893797f,  128.136154f,
        -36.075912f,  267.792725f,  -77.145020f,  -0.105051f,   -1.225978f,   71.462654f,
        -0.806285f,   38.971088f,   -2.024508f,   44.575966f,   3.234535f,    37.145603f,
        4.539487f,    0.238898f,    -5.743175f,   0.372742f,    -1.985200f,   0.447514f,
        -0.757045f,   0.588294f,    0.340316f,    0.824367f,    0.363600f,    1.000000f,
        1.000000f,    1.078527f,    8.652808f,    -10.802936f,  5.309541f,    -18.539080f,
        -49.407139f,  -26.752542f,  -40.478836f,  -57.307976f,  -22.006254f,  -29.276777f,
        -9.649570f,   -7.780735f,   6.815238f,    -3.855479f,   0.118124f,    -19.382906f,
        27.236959f,   0.000000f,    0.000000f,    -1.855030f,   -3.258111f,   -8.215003f,
        -10.097983f,  -26.808825f,  -7.125984f,   -39.301952f,  -1.622966f,   -73.710587f,
        30.228552f,   -111.132095f, 71.972931f,   0.000000f,    0.000000f,    -21.944426f,
        -4.974192f,   -55.179420f,  -5.306221f,   -92.059280f,  4.476170f,    -153.792557f,
        67.051430f,   -225.775101f, 147.273605f,  -288.326477f, 199.777740f,  -5.652398f,
        2.029198f,    0.363233f,    -3.830095f,   -5.666118f,   -6.789839f,   -5.585466f,
        0.110149f,    26.761368f,   6.061390f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kCandidatePassedPawnPstIdx) {
        return "Candidate";
    } else if (idx == EvalParams::kPassedPawnPstIdx) {
        return "Passed pawn";
    } else {
        return pieceToString((Piece)idx);
    }
}

std::string getTropismPieceTypeName(int idx) {
    if (idx == EvalParams::kDoubledPawnTropismIdx) {
        return "Doubled pawn";
    } else if (idx == EvalParams::kIsolatedPawnTropismIdx) {
        return "Isolated pawn";
    } else if (idx == EvalParams::kPassedPawnTropismIdx) {
        return "Passed pawn";
    } else if (idx == EvalParams::kCandidatePassedPawnTropismIdx) {
        return "Candidate";
    } else {
        return pieceToString((Piece)idx);
    }
}

void writeSquareTable(const EvalParams::SquareTable& squareTable, std::ostream& out) {
    for (int rank = kRanks - 1; rank >= 0; --rank) {
        out << "\t\t";
        for (int file = 0; file < kFiles; ++file) {
            const auto& value = squareTable[rank * kFiles + file];
            out << std::format("{}, ", taperedTermToString(value));
        }
        out << "\n";
    }
}

void writePieceSquareTables(
        const EvalParams::PieceSquareTables& pieceSquareTables, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)pieceSquareTables.size(); ++pieceIdx) {
        out << "\t" << getPstPieceTypeName(pieceIdx) << ": {\n";
        writeSquareTable(pieceSquareTables[pieceIdx], out);
        out << "\t}\n";
    }
}

void writeTropismTable(
        const std::array<TaperedTerm, EvalParams::kNumTropismPieceTypes>& tropism,
        std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumTropismPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n",
                getTropismPieceTypeName(pieceIdx),
                taperedTermToString(tropism[pieceIdx]));
    }
}

template <std::size_t N>
std::string arrayToString(const std::array<TaperedTerm, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(taperedTermToString) | joinToString(", ");
    return "{" + inner + "}";
}

template <std::size_t N>
std::string arrayToString(const std::array<EvalCalcT, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(evalCalcTToString) | joinToString(", ");
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
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumPstPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {:>6.1f}\n",
                getPstPieceTypeName(pieceIdx),
                params.phaseMaterialValues[pieceIdx]);
    }
    oss << "}\n";

    oss << std::format(
            "\noppositeColoredBishopFactor:\n\t{}",
            arrayToString(params.oppositeColoredBishopFactor));

    oss << std::format("\nsingleMinorFactor:\n\t{}", evalCalcTToString(params.singleMinorFactor));
    oss << std::format("\ntwoKnightsFactor:\n\t{}", evalCalcTToString(params.twoKnightsFactor));
    oss << std::format("\nrookVsMinorFactor:\n\t{}", evalCalcTToString(params.rookVsMinorFactor));
    oss << std::format(
            "\nrookAndMinorVsRookFactor:\n\t{}",
            evalCalcTToString(params.rookAndMinorVsRookFactor));

    oss << std::format("\nhasUnstoppablePawn:\n\t{}", evalCalcTToString(params.hasUnstoppablePawn));

    oss << "\npieceValues: {\n";
    writeTaperedTermPerPiece(params.pieceValues, oss);
    oss << "}\n";

    oss << "\npieceSquareTables: {\n";
    writePieceSquareTables(params.pieceSquareTables, oss);
    oss << "}\n";

    oss << std::format(
            "\nprotectedPawnBonus:\n\t{}", taperedTermToString(params.protectedPawnBonus));

    oss << std::format(
            "\nconnectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.connectedPassedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

    oss << std::format(
            "\npasserMechanicalObstructionFactor:\n\t{}",
            taperedTermToString(params.passerMechanicalObstructionFactor));

    oss << std::format(
            "\npasserDynamicObstructionFactor:\n\t{}",
            taperedTermToString(params.passerDynamicObstructionFactor));

    oss << std::format(
            "\npasserSacrificialObstructionFactor:\n\t{}",
            taperedTermToString(params.passerSacrificialObstructionFactor));

    oss << std::format(
            "\nbishopPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopPawnSameColorAdjustment));

    oss << std::format(
            "\nbishopEnemyPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopEnemyPawnSameColorAdjustment));

    oss << std::format("\nbishopPairBonus:\n\t{}", taperedTermToString(params.bishopPairBonus));

    oss << std::format("\nknightPairBonus:\n\t{}", taperedTermToString(params.knightPairBonus));

    oss << std::format("\nrookPairBonus:\n\t{}", taperedTermToString(params.rookPairBonus));

    oss << std::format(
            "\nrookSemiOpenFileBonus:\n\t{}", taperedTermToString(params.rookSemiOpenFileBonus));

    oss << std::format("\nrookOpenFileBonus:\n\t{}", taperedTermToString(params.rookOpenFileBonus));

    oss << std::format(
            "\nownRookBehindPassedPawn:\n\t{}",
            taperedTermToString(params.ownRookBehindPassedPawn));

    oss << std::format(
            "\nownRookInFrontOfPassedPawn:\n\t{}",
            taperedTermToString(params.ownRookInFrontOfPassedPawn));

    oss << std::format(
            "\nenemyRookBehindPassedPawn:\n\t{}",
            taperedTermToString(params.enemyRookBehindPassedPawn));

    oss << std::format(
            "\nenemyRookInFrontOfPassedPawn:\n\t{}",
            taperedTermToString(params.enemyRookInFrontOfPassedPawn));

    oss << std::format("\nknightPawnAdjustment:\n\t{}", arrayToString(params.knightPawnAdjustment));

    oss << std::format("\nrookPawnAdjustment:\n\t{}", arrayToString(params.rookPawnAdjustment));

    oss << std::format("\nqueenPawnAdjustment:\n\t{}", arrayToString(params.queenPawnAdjustment));

    oss << "\n\nmobilityBonus: {\n";
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.mobilityBonus[pieceIdx - 1]));
    }
    oss << "}\n";

    oss << "\n\nsafeMobilityAdjustment: {\n";
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.safeMobilityAdjustment[pieceIdx - 1]));
    }
    oss << "}\n";

    oss << "\nownKingTropism: {\n";
    writeTropismTable(params.ownKingTropism, oss);
    oss << "}\n";

    oss << "\nenemyKingTropism: {\n";
    writeTropismTable(params.enemyKingTropism, oss);
    oss << "}\n";

    oss << std::format("\ntempoBonus:\n\t{}", taperedTermToString(params.tempoBonus));

    oss << "\n\nattackDefendAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)params.attackDefendAdjustment.size(); ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.attackDefendAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format("\ncontrolNearEnemyKing:\n\t{}", arrayToString(params.controlNearEnemyKing));

    oss << "\n\nkingAttackWeight: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.kingAttackWeight[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nattackersMinusDefendersFactor:\n\t{}",
            arrayToString(params.attackersMinusDefendersFactor));

    oss << "\n\npiecePinnedAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.piecePinnedAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nkingOpenFileAdjustment:\n\t{}", taperedTermToString(params.kingOpenFileAdjustment));
    oss << std::format(
            "\nkingFlankOpenFileAdjustment:\n\t{}",
            taperedTermToString(params.kingFlankOpenFileAdjustment));
    oss << std::format(
            "\npassedPawnOutsideKingSquare:\n\t{}",
            taperedTermToString(params.passedPawnOutsideKingSquare));

    oss << std::format(
            "\ndefendedChecksAdjustment:\n\t{}", arrayToString(params.defendedChecksAdjustment));

    oss << std::format(
            "\nundefendedChecksAdjustment:\n\t{}",
            arrayToString(params.undefendedChecksAdjustment));

    oss << std::format(
            "\ncentralPotentialHoleAdjustment:\n\t{}",
            taperedTermToString(params.centralPotentialHoleAdjustment));
    oss << std::format(
            "\nwingPotentialHoleAdjustment:\n\t{}",
            taperedTermToString(params.wingPotentialHoleAdjustment));

    oss << std::format(
            "\ncentralHoleAdjustment:\n\t{}", taperedTermToString(params.centralHoleAdjustment));
    oss << std::format(
            "\nwingHoleAdjustment:\n\t{}", taperedTermToString(params.wingHoleAdjustment));

    oss << std::format(
            "\nknightOnOutpostAdjustment:\n\t{}",
            taperedTermToString(params.knightOnOutpostAdjustment));

    return oss.str();
}
