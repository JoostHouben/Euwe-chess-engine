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
        0.209377f,    1.076343f,    0.300309f,    0.188074f,    0.312114f,    1.000000f,
        -0.011579f,   -0.000056f,   -0.015281f,   -0.003717f,   78.833328f,   -11.258627f,
        153.244583f,  332.487457f,  372.377411f,  295.615631f,  410.191895f,  458.337891f,
        691.182434f,  1118.632568f, 1133.834229f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -39.364426f,  152.761597f,  -37.848099f,
        146.401993f,  -35.872864f,  160.965485f,  -48.067471f,  162.193237f,  -40.079868f,
        162.413177f,  5.022711f,    158.791168f,  7.919129f,    148.877472f,  -20.109150f,
        137.703430f,  -31.975740f,  145.084091f,  -29.161083f,  145.714294f,  -23.944555f,
        159.468109f,  -23.848740f,  170.789581f,  -22.488323f,  160.718628f,  -1.242488f,
        155.745361f,  1.424459f,    146.900635f,  -11.037620f,  144.674393f,  -21.452978f,
        153.780212f,  -11.291446f,  154.836578f,  -6.770271f,   159.216919f,  1.342883f,
        157.939789f,  5.305670f,    152.886063f,  15.093563f,   150.668289f,  -3.627419f,
        161.932907f,  -8.863320f,   152.028458f,  1.229195f,    164.290985f,  4.496280f,
        161.994156f,  4.695171f,    157.450027f,  7.771175f,    167.183929f,  28.156599f,
        154.959991f,  29.467495f,   158.078751f,  12.473166f,   158.572449f,  2.025786f,
        154.411682f,  1.974490f,    202.716202f,  4.286104f,    217.481094f,  2.493342f,
        189.191086f,  46.991055f,   93.229286f,   61.962238f,   169.142670f,  69.783371f,
        180.203735f,  136.955399f,  176.670700f,  46.109184f,   151.468445f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    236.289276f,  339.937012f,  295.499451f,  345.569641f,  285.856384f,
        365.877411f,  294.782684f,  363.438538f,  291.570709f,  347.233398f,  304.432983f,
        352.399567f,  285.680481f,  347.361450f,  249.229340f,  325.352814f,  295.280731f,
        324.242706f,  316.224396f,  366.959106f,  309.870453f,  349.454468f,  327.711731f,
        361.928711f,  322.799377f,  358.580353f,  305.487610f,  361.099396f,  303.754974f,
        376.080566f,  310.193665f,  326.976959f,  306.551422f,  322.781830f,  322.836517f,
        372.297882f,  333.032837f,  367.083862f,  340.180389f,  381.718048f,  349.446899f,
        374.841400f,  333.423523f,  361.987762f,  331.422394f,  364.055847f,  306.458466f,
        347.305878f,  329.246216f,  363.904846f,  337.743164f,  364.120422f,  360.752533f,
        381.729675f,  354.076294f,  386.872528f,  359.615631f,  391.309692f,  353.666351f,
        382.740540f,  347.290039f,  379.508606f,  318.918976f,  376.334106f,  341.932861f,
        372.427704f,  363.278839f,  387.811188f,  367.369385f,  384.492462f,  369.208984f,
        400.066528f,  367.519012f,  380.507538f,  380.462646f,  382.210449f,  357.116272f,
        382.059296f,  346.252716f,  366.771942f,  327.330505f,  371.080597f,  366.467010f,
        376.277161f,  378.633362f,  375.485962f,  381.588989f,  387.741211f,  422.809631f,
        370.293976f,  408.819183f,  387.916290f,  383.208069f,  382.269867f,  287.134674f,
        372.664612f,  322.981445f,  400.222839f,  346.388336f,  391.382507f,  375.316437f,
        380.753082f,  410.468689f,  367.551849f,  361.294037f,  376.674713f,  377.957428f,
        377.906555f,  299.487122f,  391.473022f,  340.471710f,  352.565674f,  210.181259f,
        356.132385f,  276.253876f,  373.358795f,  344.880493f,  382.352173f,  299.598297f,
        406.111237f,  324.911652f,  392.714661f,  433.310181f,  352.852539f,  325.881866f,
        350.687988f,  159.431198f,  303.434113f,  270.311890f,  381.940277f,  265.506470f,
        392.005432f,  276.881134f,  396.219360f,  270.989014f,  401.772034f,  263.578217f,
        416.716675f,  270.609283f,  400.852356f,  269.114807f,  387.069885f,  246.637497f,
        377.028320f,  287.179626f,  388.193542f,  286.695984f,  383.765839f,  300.347229f,
        393.172455f,  283.331360f,  415.694794f,  291.340027f,  409.082397f,  305.251678f,
        405.172241f,  303.824707f,  394.034821f,  299.753754f,  346.279205f,  291.385956f,
        394.912048f,  300.979370f,  395.908173f,  302.101929f,  421.295593f,  303.122925f,
        416.656342f,  304.619751f,  422.626678f,  299.264465f,  409.724335f,  313.408600f,
        397.002625f,  292.062256f,  394.236023f,  276.481964f,  409.417725f,  299.343750f,
        409.204315f,  304.898468f,  416.302643f,  316.978546f,  420.669891f,  313.232819f,
        428.073029f,  305.691040f,  413.047302f,  312.386780f,  407.569824f,  297.601562f,
        412.944244f,  283.340881f,  420.035339f,  300.509094f,  423.469391f,  316.953674f,
        419.315674f,  339.626678f,  406.313629f,  325.224731f,  427.318115f,  318.802826f,
        423.216705f,  302.508575f,  420.072113f,  307.204712f,  418.336029f,  302.755341f,
        415.176239f,  328.228271f,  423.340729f,  327.513062f,  409.010986f,  354.001648f,
        409.920074f,  337.258972f,  413.617828f,  405.666809f,  408.068237f,  360.354706f,
        421.817749f,  329.867462f,  408.895477f,  305.577484f,  401.196136f,  315.112518f,
        415.093414f,  332.391022f,  423.328644f,  300.463074f,  423.108215f,  332.212006f,
        410.554626f,  321.006897f,  417.292664f,  354.946594f,  395.209564f,  258.104218f,
        408.864410f,  284.563293f,  413.173828f,  231.492233f,  435.083435f,  287.312073f,
        425.738098f,  277.676483f,  434.177368f,  280.838043f,  427.734192f,  244.741043f,
        426.966797f,  438.095184f,  361.739258f,  261.608185f,  421.220886f,  463.678375f,
        663.256042f,  458.383362f,  666.768616f,  456.819458f,  678.093872f,  459.588440f,
        671.821533f,  461.080902f,  673.246094f,  456.758606f,  678.777710f,  433.423706f,
        681.808228f,  441.942535f,  660.828491f,  441.587219f,  680.308594f,  441.583832f,
        681.259155f,  429.728119f,  692.634888f,  454.866699f,  674.336060f,  450.123596f,
        681.610229f,  462.954620f,  659.269348f,  452.514679f,  664.575806f,  407.513672f,
        693.692505f,  442.398254f,  687.650269f,  449.788239f,  688.423035f,  443.588898f,
        682.117004f,  443.414642f,  690.638000f,  440.697723f,  687.287903f,  428.708221f,
        693.629700f,  453.794281f,  685.367859f,  427.733032f,  682.325500f,  439.263672f,
        713.883301f,  433.224426f,  705.060669f,  446.405334f,  703.565979f,  458.967285f,
        699.672607f,  453.947083f,  712.309143f,  454.716278f,  703.294250f,  457.903748f,
        705.696472f,  423.394501f,  713.164185f,  452.852264f,  721.797302f,  448.635498f,
        716.243042f,  460.550201f,  712.694885f,  463.595612f,  714.846191f,  470.737457f,
        713.755005f,  475.489685f,  714.479248f,  469.180481f,  706.418701f,  458.302216f,
        718.349792f,  468.794006f,  723.862671f,  485.173920f,  710.187378f,  475.104828f,
        713.528320f,  489.164642f,  708.767029f,  510.670013f,  704.842896f,  537.378479f,
        693.115967f,  501.143219f,  703.452637f,  497.302551f,  703.608276f,  467.176239f,
        727.231201f,  487.843262f,  716.408264f,  486.447052f,  719.460327f,  503.988098f,
        715.389404f,  517.758240f,  709.920044f,  527.220398f,  701.895325f,  514.548584f,
        703.663696f,  506.540802f,  709.140808f,  470.234283f,  715.272888f,  490.115112f,
        708.092957f,  486.896423f,  711.552856f,  484.700958f,  708.552917f,  512.937927f,
        712.010925f,  516.700317f,  714.261658f,  447.882111f,  734.216553f,  511.421295f,
        708.015076f,  1130.432373f, 1068.223999f, 1115.358276f, 1096.471558f, 1114.827515f,
        1085.650635f, 1117.472168f, 1072.425171f, 1121.485107f, 1059.616211f, 1075.241821f,
        1112.746826f, 1052.735352f, 1165.108276f, 1053.153320f, 1150.457031f, 1123.840454f,
        1081.518677f, 1126.498413f, 1085.567627f, 1131.156616f, 1064.629028f, 1124.396240f,
        1082.897095f, 1119.045288f, 1100.646606f, 1123.582520f, 1052.842529f, 1117.961792f,
        985.769714f,  1074.583862f, 1135.175781f, 1121.257446f, 1093.200195f, 1127.704712f,
        1095.852539f, 1113.884033f, 1132.635498f, 1111.685425f, 1136.368042f, 1121.558350f,
        1117.251465f, 1113.015259f, 1147.582764f, 1131.074219f, 1090.536499f, 1109.385498f,
        1167.992188f, 1116.280518f, 1133.643555f, 1134.793579f, 1123.011108f, 1116.419678f,
        1165.717407f, 1097.088745f, 1218.324707f, 1107.099854f, 1176.435181f, 1106.349121f,
        1185.535278f, 1115.920776f, 1171.877075f, 1107.998047f, 1204.670532f, 1112.510376f,
        1188.478516f, 1115.252319f, 1152.169556f, 1101.635864f, 1211.042236f, 1114.909302f,
        1195.174438f, 1103.205444f, 1227.745605f, 1084.608032f, 1231.148682f, 1105.014526f,
        1217.508789f, 1113.101196f, 1196.111938f, 1117.362305f, 1157.655640f, 1124.833984f,
        1150.617920f, 1087.935303f, 1215.461426f, 1099.857056f, 1220.917480f, 1105.781616f,
        1263.889038f, 1128.700073f, 1215.163574f, 1157.995728f, 1137.172974f, 1111.299072f,
        1227.881592f, 1113.610596f, 1152.531372f, 1091.936523f, 1198.412354f, 1114.521729f,
        1192.373657f, 1087.629395f, 1243.763062f, 1095.989746f, 1241.672241f, 1131.105225f,
        1227.350342f, 1181.602783f, 1158.711304f, 1180.151733f, 1153.096313f, 1113.182129f,
        1155.899902f, 1134.700439f, 1140.953613f, 1124.896240f, 1151.302612f, 1123.337158f,
        1203.047485f, 1125.861084f, 1204.836304f, 1134.082397f, 1228.534912f, 1140.003174f,
        1172.190186f, 1162.209351f, 1139.036499f, 54.885303f,   -34.102699f,  22.699081f,
        -13.468034f,  -31.676893f,  1.831793f,    -54.266670f,  -12.625183f,  -26.130745f,
        -29.180407f,  -53.423546f,  2.820132f,    25.426107f,   -6.877470f,   46.177326f,
        -15.826314f,  20.700363f,   -27.299782f,  -6.045689f,   -8.794799f,   -79.146957f,
        4.317432f,    -107.248344f, 7.134123f,    -113.927422f, 13.684370f,   -58.311607f,
        -3.261746f,   -11.108522f,  -10.631699f,  19.912527f,   -28.278166f,  -12.263891f,
        -28.047470f,  -47.995403f,  -10.679333f,  -118.025749f, 2.663420f,    -119.420090f,
        4.758971f,    -158.538513f, 18.999636f,   -114.552132f, 7.762821f,    -62.423832f,
        -9.629986f,   -31.656466f,  -29.451702f,  -30.727194f,  -18.275518f,  -71.132553f,
        -7.517187f,   -47.306889f,  -12.063566f,  -139.361328f, 7.963650f,    -112.701439f,
        3.908862f,    -129.314926f, 14.108442f,   -104.544632f, -4.306887f,   -115.948700f,
        -16.860962f,  41.285732f,   -31.199654f,  -20.802452f,  4.278841f,    31.357143f,
        -11.123770f,  -63.449448f,  14.371150f,   -105.083054f, 13.949190f,   -38.594353f,
        5.612596f,    -110.898407f, 19.786041f,   -53.141670f,  -21.106461f,  1.369781f,
        -9.921801f,   63.057930f,   -9.018167f,   73.274330f,   4.998267f,    40.674129f,
        5.476141f,    43.794067f,   -7.678126f,   53.515022f,   14.662344f,   62.222939f,
        8.384925f,    63.140507f,   -23.329401f,  -18.515614f,  17.518410f,   23.147980f,
        11.684014f,   54.007835f,   9.793692f,    84.054756f,   2.252445f,    125.909172f,
        -15.320224f,  332.485413f,  -115.909561f, 156.308716f,  12.057526f,   -38.235058f,
        -29.417915f,  -133.204483f, -37.834393f,  175.349472f,  -71.534073f,  112.307961f,
        -12.736387f,  113.836266f,  -6.733713f,   -4.092426f,   -7.797377f,   102.502197f,
        1.204288f,    -130.128021f, 12.296556f,   65.016899f,   -105.613991f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -27.605059f,  110.151939f,  -9.733034f,
        74.619942f,   5.202523f,    92.557503f,   1.140539f,    140.443588f,  -9.520527f,
        121.386688f,  62.620407f,   111.541359f,  49.018673f,   87.798981f,   47.495056f,
        53.256161f,   -15.294992f,  133.808350f,  9.688534f,    101.674713f,  17.078815f,
        103.105850f,  0.542070f,    109.284035f,  12.356239f,   110.399475f,  35.481682f,
        109.781456f,  68.341888f,   73.409599f,   22.937082f,   91.888123f,   16.497324f,
        161.379211f,  34.362179f,   142.751358f,  24.479486f,   135.136490f,  43.558868f,
        126.581474f,  55.116714f,   128.841751f,  53.124233f,   141.186554f,  80.813324f,
        134.687683f,  43.807114f,   119.890816f,  40.318062f,   211.596924f,  40.047405f,
        216.804214f,  44.890747f,   236.688889f,  73.675117f,   155.192291f,  115.592545f,
        144.853271f,  127.695641f,  156.528839f,  71.150291f,   164.909027f,  103.921371f,
        165.664154f,  83.054878f,   444.615723f,  25.919779f,   347.436951f,  31.453304f,
        357.498199f,  56.276054f,   335.258911f,  159.534943f,  187.937317f,  116.126709f,
        167.388260f,  178.878815f,  186.853012f,  148.221542f,  151.864960f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -52.928677f,
        97.642578f,   -38.874733f,  91.723701f,   -51.901222f,  80.513756f,   -56.261223f,
        47.333622f,   -76.194550f,  71.439163f,   2.584544f,    54.526421f,   -39.623604f,
        61.159184f,   -65.550255f,  72.748604f,   -33.523613f,  100.749199f,  -39.148598f,
        117.887466f,  -34.597923f,  90.664406f,   -64.624481f,  104.132355f,  -71.014496f,
        113.561409f,  -53.280739f,  101.392441f,  14.704349f,   74.921143f,   -21.113958f,
        75.934639f,   34.298706f,   144.108505f,  48.538967f,   128.882553f,  8.136067f,
        118.003815f,  11.981851f,   108.542023f,  18.494844f,   107.996040f,  4.685725f,
        133.381577f,  -6.431623f,   153.432953f,  -0.222111f,   125.785789f,  108.990974f,
        209.760666f,  102.470169f,  196.360474f,  88.347885f,   171.997406f,  99.136978f,
        141.753922f,  110.628960f,  130.434937f,  72.705574f,   163.815613f,  86.428741f,
        167.153488f,  61.612202f,   169.149445f,  209.690994f,  271.740936f,  214.362625f,
        279.011536f,  222.195938f,  238.648666f,  179.189941f,  215.879745f,  194.958313f,
        174.988831f,  277.340454f,  156.476135f,  133.435898f,  234.206070f,  117.418915f,
        226.331192f,  425.407837f,  291.223572f,  431.508453f,  292.889435f,  351.296722f,
        312.487213f,  402.261078f,  242.764236f,  358.833008f,  245.576553f,  243.766846f,
        219.370453f,  127.948914f,  285.430389f,  9.193923f,    323.896881f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    9.148464f,    4.898351f,    10.218620f,
        57.807835f,   2.534246f,    -7.424938f,   4.081415f,    18.618856f,   -0.390316f,
        -0.324767f,   -0.265756f,   -0.182679f,   -0.161436f,   0.037145f,    28.284578f,
        15.551091f,   33.184464f,   -0.189119f,   19.526649f,   -1.155701f,   12.102971f,
        -10.665366f,  0.000000f,    0.000000f,    -12.826467f,  7.842223f,    -27.006115f,
        8.008929f,    -35.045895f,  0.443739f,    -79.307014f,  103.528893f,  23.664909f,
        3.257759f,    25.358213f,   -1.362269f,   12.936846f,   -0.468770f,   8.193699f,
        -4.309452f,   0.000000f,    0.000000f,    -10.309905f,  6.778462f,    -21.794573f,
        10.930808f,   -31.701712f,  36.481255f,   -51.769108f,  99.790970f,   31.149727f,
        62.104237f,   -6.472456f,   -2.270306f,   -20.367142f,  -28.260571f,  15.794759f,
        1.675089f,    32.560989f,   0.640747f,    10.585819f,   18.946560f,   29.694506f,
        -29.584278f,  4.076103f,    -65.474419f,  -2.638750f,   19.220346f,   -188.425949f,
        71.595093f,   -106.093147f, 25.413469f,   24.319952f,   -18.516558f,  -8.349381f,
        -1.789728f,   0.000000f,    0.000000f,    -12.517959f,  21.297642f,   -13.025886f,
        39.229546f,   -21.136421f,  50.071281f,   -30.729355f,  94.657204f,   -279.675354f,
        118.421890f,  -122.169258f, 39.095818f,   14.907987f,   -32.866627f,  14.969727f,
        -25.148594f,  0.000000f,    0.000000f,    -9.544469f,   19.606653f,   -17.467991f,
        43.130981f,   -41.952644f,  91.221161f,   -44.683704f,  83.291946f,   26.169987f,
        -20.205061f,  74.643410f,   -119.992218f, -95.570175f,  22.329891f,   -41.589546f,
        -0.715054f,   0.000000f,    0.000000f,    70.739525f,   -22.421286f,  138.178131f,
        -60.181030f,  252.851837f,  -168.785919f, 325.375336f,  -171.001266f, -0.421665f,
        -0.330599f,   5.044631f,    0.897018f,    3.215842f,    1.666829f,    1.806551f,
        1.150565f,    -5.452291f,   -37.374180f,  -3.364200f,   -16.941355f,  -1.939948f,
        -6.202147f,   -1.880305f,   -1.800552f,   0.000000f,    0.000000f,    3.465954f,
        -1.271690f,   4.299674f,    0.558488f,    13.688620f,   -8.617612f,   -32.037380f,
        -7.768435f,   11.442633f,   -47.157635f,  8.962883f,    -20.274223f,  5.965058f,
        -10.432220f,  3.147833f,    -4.778312f,   0.000000f,    0.000000f,    -6.970246f,
        3.383548f,    -10.198309f,  5.527332f,    -4.717467f,   1.130760f,    12.826172f,
        -7.007350f,   14.937943f,   -42.387161f,  9.056256f,    -13.630751f,  7.941477f,
        -8.042666f,   7.040500f,    -6.666960f,   0.000000f,    0.000000f,    1.106863f,
        2.039022f,    -0.657627f,   2.913470f,    -3.856459f,   4.075248f,    1.522266f,
        -0.377521f,   -4.327493f,   -62.589718f,  -0.250557f,   -66.170105f,  3.254193f,
        -41.233391f,  0.104129f,    -1.892689f,   0.000000f,    0.000000f,    -0.068739f,
        6.595133f,    -0.380087f,   9.029410f,    -2.849348f,   15.828750f,   -3.066262f,
        17.705858f,   5.465404f,    -1.471067f,   2.177879f,    1.916336f,    2.433366f,
        -0.648585f,   1.112242f,    -0.107823f,   1.920127f,    -1.053257f,   3.977021f,
        -2.202461f,   4.434365f,    1.483440f,    8.567989f,    10.582177f,   -0.206696f,
        4.017811f,    -1.754615f,   -3.802871f,   0.987416f,    -0.891250f,   -2.543555f,
        -1.615596f,   3.491727f,    -5.634190f,   3.469344f,    -9.825613f,   -1.305600f,
        -4.631076f,   -0.303363f,   -4.998104f,   -6.224533f,   -6.223646f,   -0.075135f,
        -3.461068f,   20.000000f,   10.000000f,   0.811920f,    -6.472053f,   -8.971083f,
        -40.156010f,  9.186360f,    -16.357775f,  7.351928f,    0.971132f,    -33.477146f,
        -24.555256f,  -1.577732f,   -14.609156f,  2.148418f,    12.381890f,   -41.827980f,
        -28.208597f,  -15.426064f,  -2.076015f,   6.135680f,    8.931062f,    -49.507011f,
        -7.633556f,   -4.350739f,   4.355588f,    3.301734f,    31.366968f,   -58.099190f,
        -37.997097f,  -44.146538f,  0.467588f,    0.000000f,    0.000000f,    -7.900497f,
        -5.233998f,   -7.883585f,   -4.462753f,   2.427526f,    -5.271156f,   8.256081f,
        -7.468678f,   21.580814f,   -12.031002f,  53.230423f,   -19.444939f,  78.054482f,
        -12.506415f,  241.905975f,  -61.213795f,  4.218631f,    -2.825439f,   78.998550f,
        -11.441630f,  41.333347f,   -9.772558f,   40.636261f,   12.523892f,   34.111427f,
        13.969080f,   0.136800f,    -1.376536f,   0.215187f,    -0.470185f,   0.325812f,
        -0.030416f,   0.501514f,    0.337360f,    0.761736f,    0.335368f,    1.000000f,
        1.000000f,    2.246367f,    2.536942f,    -11.177814f,  1.105956f,    -26.647831f,
        -30.982359f,  -20.657000f,  -52.873009f,  -74.642120f,  -11.480844f,  -16.778219f,
        -27.226728f,  -8.137493f,   4.495891f,    -2.764023f,   0.914992f,    -17.810612f,
        31.693523f,   0.000000f,    0.000000f,    -2.581196f,   -1.619182f,   -10.538349f,
        -6.192822f,   -19.993961f,  -8.387402f,   -32.428356f,  -12.247602f,  -62.358521f,
        21.733376f,   -101.586906f, 62.387337f,   0.000000f,    0.000000f,    -20.802696f,
        -5.681870f,   -50.873398f,  -8.792459f,   -75.808105f,  -10.080573f,  -144.876709f,
        51.487083f,   -145.514755f, 44.297035f,   -239.055969f, 157.726608f,  0.457621f,
        -2.011358f,   1.682594f,    -1.294864f,   -13.525390f,  -7.944756f,   -6.604337f,
        -8.785503f,   24.276230f,   9.283796f};

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
