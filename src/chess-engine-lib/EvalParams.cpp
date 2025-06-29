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
        0.209377f,    1.076343f,    0.293841f,    0.165412f,    0.298907f,    1.000000f,
        -0.000000f,   -0.000000f,   -0.015136f,   0.000920f,    21.258041f,   -18.209032f,
        147.037598f,  333.486450f,  352.133362f,  317.460327f,  410.590912f,  444.026733f,
        680.617432f,  1125.505127f, 1122.773682f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -41.139076f,  144.185394f,  -51.444942f,
        140.961548f,  -43.678120f,  153.111710f,  -52.681587f,  148.241837f,  -43.959034f,
        159.494568f,  -4.291888f,   151.872879f,  -0.630716f,   140.143326f,  -29.052105f,
        136.826218f,  -31.047300f,  144.664886f,  -38.818874f,  137.666565f,  -32.223534f,
        152.512344f,  -35.550991f,  160.910202f,  -27.197765f,  156.414291f,  -8.759109f,
        144.785645f,  -7.328772f,   140.329544f,  -15.819156f,  139.917542f,  -16.285135f,
        146.743454f,  -16.118986f,  145.685867f,  -12.747993f,  157.761154f,  -8.608918f,
        149.391479f,  -0.169620f,   140.497314f,  4.630014f,    149.670105f,  -8.037189f,
        152.416458f,  -15.191125f,  148.137451f,  -4.800848f,   164.976547f,  5.755758f,
        147.844391f,  0.069918f,    159.921997f,  3.749548f,    155.976852f,  18.619097f,
        154.939804f,  24.859194f,   138.934372f,  11.656124f,   155.649963f,  -0.857667f,
        153.569412f,  15.116956f,   165.261734f,  -0.274980f,   162.811142f,  15.912735f,
        182.482620f,  19.433498f,   146.187317f,  32.790668f,   138.199799f,  78.853844f,
        174.283936f,  90.972076f,   170.814926f,  41.401497f,   168.699890f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    288.100494f,  279.204926f,  292.982300f,  318.142151f,  291.165497f,
        346.821625f,  292.973877f,  350.093567f,  301.069824f,  351.530640f,  303.683228f,
        324.011017f,  283.304291f,  322.307220f,  249.772034f,  328.351501f,  298.838440f,
        322.031555f,  317.299255f,  339.046387f,  303.814911f,  343.874268f,  326.047943f,
        348.056702f,  321.888153f,  353.266663f,  310.097015f,  359.762726f,  302.235687f,
        355.984985f,  301.633331f,  322.882507f,  299.376831f,  328.955750f,  329.849915f,
        347.925873f,  329.731171f,  342.871521f,  346.668762f,  363.978638f,  352.473969f,
        358.992737f,  336.560608f,  336.734863f,  340.781860f,  320.227020f,  301.656525f,
        332.982758f,  332.296539f,  356.530426f,  359.883423f,  351.836487f,  369.923828f,
        354.995667f,  353.410248f,  362.054382f,  362.851196f,  367.048767f,  360.235962f,
        363.241913f,  349.714783f,  356.894745f,  333.197113f,  349.508453f,  340.955627f,
        362.261078f,  357.593658f,  361.203369f,  370.364868f,  366.027588f,  375.452454f,
        374.324097f,  369.566559f,  364.010162f,  384.041901f,  360.794922f,  357.496735f,
        359.982086f,  357.991547f,  347.936584f,  309.112610f,  368.939850f,  371.298431f,
        368.947998f,  399.569458f,  353.417114f,  394.992035f,  353.368866f,  443.506805f,
        343.377899f,  405.905701f,  358.750122f,  380.828033f,  353.281342f,  312.817230f,
        356.843475f,  321.583344f,  374.790710f,  353.754608f,  356.210602f,  364.055542f,
        360.924133f,  357.763611f,  371.443634f,  388.235474f,  364.025513f,  403.197449f,
        344.284302f,  338.112335f,  359.371704f,  320.455139f,  364.074158f,  178.978424f,
        371.706360f,  207.255371f,  386.848816f,  297.845703f,  391.030243f,  371.807465f,
        367.510742f,  334.990051f,  369.618530f,  456.551239f,  360.713562f,  187.481598f,
        381.341187f,  123.125168f,  230.473083f,  276.503052f,  390.703888f,  312.514709f,
        392.117615f,  294.379639f,  399.319763f,  287.846252f,  405.604828f,  295.340790f,
        396.672668f,  294.706268f,  400.674011f,  315.568573f,  393.575592f,  302.444153f,
        343.492798f,  316.249695f,  399.246521f,  303.601227f,  387.950684f,  322.444550f,
        404.053711f,  307.034149f,  406.830994f,  315.425507f,  409.571106f,  324.476074f,
        396.703949f,  331.867035f,  396.258270f,  327.790863f,  359.823883f,  305.180511f,
        384.564209f,  331.098114f,  402.084839f,  319.421387f,  408.502502f,  326.277008f,
        410.341522f,  324.792206f,  423.337646f,  324.562195f,  409.528015f,  328.010681f,
        398.351868f,  320.057251f,  404.612671f,  326.379333f,  403.156860f,  333.685425f,
        402.946899f,  322.599213f,  420.355621f,  334.544464f,  420.082764f,  339.900085f,
        421.164001f,  325.892273f,  419.854797f,  336.361206f,  412.140594f,  329.679871f,
        414.541748f,  307.620026f,  424.027130f,  323.377594f,  419.451752f,  336.721069f,
        424.868896f,  354.164612f,  421.030945f,  349.575989f,  424.268738f,  342.468567f,
        421.085968f,  323.869110f,  422.669220f,  312.752747f,  416.021332f,  318.118011f,
        416.595215f,  325.244446f,  425.602081f,  329.683228f,  421.026093f,  350.794067f,
        420.028107f,  338.511841f,  431.297668f,  424.816589f,  399.344757f,  373.236206f,
        414.399109f,  357.055481f,  401.900116f,  305.359924f,  404.459900f,  319.844849f,
        422.930145f,  323.490021f,  432.216827f,  310.072357f,  421.730042f,  366.489441f,
        407.098969f,  332.937439f,  418.030579f,  348.346405f,  401.331512f,  296.480896f,
        388.698120f,  308.594116f,  412.650513f,  296.727905f,  426.527283f,  290.498138f,
        423.619812f,  296.588226f,  418.766846f,  286.239075f,  429.334839f,  268.666626f,
        420.697144f,  360.101257f,  369.098907f,  359.701660f,  389.023132f,  455.079193f,
        656.289062f,  448.876251f,  656.479736f,  442.668762f,  667.644531f,  443.777557f,
        666.353088f,  443.412231f,  661.876953f,  438.349670f,  668.385498f,  422.130157f,
        667.429810f,  427.478851f,  652.049438f,  446.034393f,  659.885925f,  437.481995f,
        659.715942f,  427.557861f,  674.238586f,  434.004730f,  667.687927f,  428.935272f,
        671.655090f,  436.664368f,  664.221436f,  429.975830f,  657.910889f,  389.277466f,
        678.022156f,  431.392426f,  677.038574f,  437.399048f,  668.996521f,  426.689606f,
        678.889587f,  424.091858f,  678.794739f,  418.491760f,  678.275452f,  406.088318f,
        692.328308f,  411.984497f,  689.890930f,  424.062836f,  670.664307f,  437.637634f,
        690.328857f,  426.139191f,  698.956116f,  426.627930f,  698.543091f,  436.092896f,
        690.625366f,  429.729248f,  696.572021f,  426.407471f,  697.852234f,  439.946320f,
        699.517883f,  413.889435f,  703.331238f,  436.021484f,  708.526306f,  429.965942f,
        709.316650f,  441.684326f,  699.916992f,  472.173492f,  690.153015f,  453.095856f,
        699.601379f,  452.901855f,  702.834167f,  425.788940f,  707.189331f,  443.637238f,
        702.399170f,  431.425476f,  713.485901f,  466.423645f,  700.212524f,  471.360229f,
        696.175903f,  471.376251f,  696.104553f,  495.042175f,  687.370239f,  496.461395f,
        692.066101f,  502.624451f,  686.164917f,  487.748199f,  701.801941f,  443.780945f,
        713.711243f,  463.357941f,  712.081787f,  493.322327f,  695.790222f,  497.907196f,
        699.529602f,  490.025208f,  698.563171f,  496.510803f,  690.614258f,  486.715271f,
        699.915527f,  484.356995f,  699.858398f,  480.046753f,  686.649292f,  473.967834f,
        697.468933f,  484.299438f,  694.193054f,  456.797699f,  702.111145f,  463.333923f,
        705.492554f,  456.047058f,  707.627502f,  413.003540f,  727.152832f,  514.662659f,
        690.993774f,  1146.560791f, 1041.220337f, 1132.287842f, 1041.021729f, 1120.820679f,
        1076.866333f, 1123.917114f, 1058.275757f, 1130.629761f, 1055.515503f, 1095.218506f,
        1061.173340f, 1130.442749f, 934.907837f,  1072.426147f, 1072.204346f, 1140.248047f,
        1071.838867f, 1139.413330f, 1060.509521f, 1140.128174f, 1071.786377f, 1132.757568f,
        1080.764038f, 1127.353760f, 1090.113037f, 1123.996338f, 1050.819092f, 1122.767212f,
        1009.152771f, 1084.149536f, 1110.920654f, 1135.857910f, 1088.866943f, 1130.181519f,
        1107.179443f, 1125.618652f, 1108.832886f, 1121.719360f, 1123.761230f, 1120.759155f,
        1118.475220f, 1121.730835f, 1109.108154f, 1135.282349f, 1065.879150f, 1115.236084f,
        1124.823364f, 1127.340576f, 1134.454834f, 1124.584961f, 1136.998779f, 1122.937012f,
        1144.461060f, 1114.459351f, 1157.434814f, 1121.519165f, 1149.402832f, 1118.848755f,
        1160.188354f, 1115.764160f, 1143.198120f, 1114.991211f, 1192.854858f, 1136.664551f,
        1119.240112f, 1118.398682f, 1170.602295f, 1108.075684f, 1205.852173f, 1098.781860f,
        1198.235474f, 1103.268921f, 1205.716797f, 1110.947388f, 1202.580444f, 1102.344482f,
        1209.104980f, 1117.554443f, 1188.310669f, 1138.785278f, 1132.484863f, 1115.548462f,
        1155.122314f, 1082.677734f, 1232.784058f, 1098.936279f, 1213.895142f, 1096.000122f,
        1246.401978f, 1140.264893f, 1195.463745f, 1171.559814f, 1131.081299f, 1112.199219f,
        1222.109131f, 1118.439453f, 1162.159546f, 1090.626221f, 1195.910156f, 1114.495239f,
        1190.662720f, 1094.926758f, 1221.927490f, 1079.435303f, 1262.172119f, 1109.252197f,
        1196.629028f, 1126.753174f, 1180.314331f, 1149.620239f, 1151.282837f, 1127.995605f,
        1142.774536f, 1143.612793f, 1131.980591f, 1093.064331f, 1219.323242f, 1091.864624f,
        1221.323608f, 1116.564209f, 1215.406494f, 1156.411499f, 1181.719482f, 1130.586548f,
        1190.013428f, 1175.042480f, 1149.737305f, 46.023712f,   -40.291203f,  25.707066f,
        -18.998791f,  -37.760212f,  3.403279f,    -47.188366f,  -17.370321f,  -34.350704f,
        -19.015432f,  -56.047573f,  10.223591f,   26.397840f,   -5.858428f,   48.376251f,
        -13.752948f,  23.666443f,   -32.165405f,  -22.761786f,  -4.353783f,   -79.358452f,
        2.565995f,    -103.094284f, 3.053545f,    -99.662125f,  6.315495f,    -66.296829f,
        2.912871f,    -6.851523f,   -10.422608f,  22.307468f,   -28.958973f,  -42.889507f,
        -23.396820f,  -49.756062f,  -10.262282f,  -115.606499f, 3.665067f,    -118.195038f,
        5.510518f,    -131.517776f, 12.418741f,   -119.689278f, 12.342085f,   -64.174599f,
        -10.965546f,  -34.729286f,  -27.795933f,  -13.523289f,  -27.032743f,  -34.943943f,
        -14.022327f,  -66.785927f,  -0.033613f,   -112.045601f, 5.698375f,    -110.155754f,
        8.233452f,    -134.856216f, 13.020171f,   -79.852310f,  -14.093575f,  -94.688194f,
        -32.219372f,  35.715534f,   -28.327509f,  -22.189831f,  4.041836f,    -28.316479f,
        2.969167f,    -70.894653f,  10.828822f,   -35.601986f,  6.753737f,    -28.436028f,
        -1.501875f,   -29.669397f,  -14.738874f,  -43.500813f,  -27.525526f,  20.509781f,
        12.915777f,   -1.250327f,   19.352774f,   8.916017f,    10.949286f,   24.415985f,
        1.655605f,    94.827148f,   -24.774225f,  74.227608f,   17.219627f,   64.156647f,
        4.198310f,    31.383379f,   -21.153013f,  11.658829f,   -6.851491f,   103.420280f,
        5.683202f,    62.706993f,   8.961386f,    83.867836f,   -4.276930f,   -22.881542f,
        -1.618915f,   92.191193f,   -27.185440f,  48.731209f,   29.630650f,   178.576767f,
        -93.750786f,  43.416977f,   -31.881418f,  221.266006f,  -79.650612f,  130.561966f,
        -16.048185f,  128.441330f,  -43.252399f,  26.075048f,   -42.253109f,  100.584206f,
        -48.563404f,  -50.895485f,  41.071678f,   288.182159f,  -156.956696f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    9.630082f,    89.241234f,   27.361765f,
        53.789398f,   26.514662f,   73.212326f,   26.647610f,   133.297073f,  39.989765f,
        98.147232f,   99.139557f,   47.358463f,   106.958534f,  51.318268f,   60.333271f,
        62.298592f,   38.724590f,   81.672775f,   14.594078f,   92.308479f,   26.760313f,
        110.756424f,  50.500008f,   69.476837f,   25.983856f,   102.267601f,  62.952595f,
        90.295227f,   101.163780f,  42.215683f,   58.016220f,   60.273716f,   56.278725f,
        138.862366f,  47.115299f,   123.537285f,  52.483738f,   133.684875f,  72.352242f,
        100.289604f,  88.881104f,   113.258118f,  95.789459f,   111.283821f,  88.302391f,
        102.893730f,  95.376373f,   76.858307f,   45.435280f,   241.789001f,  80.443596f,
        225.744675f,  88.710571f,   187.077835f,  111.018822f,  151.853104f,  121.599556f,
        139.302292f,  147.974121f,  140.620560f,  134.356857f,  105.441269f,  177.020798f,
        113.698479f,  123.943855f,  286.026031f,  51.152637f,   327.501007f,  67.717384f,
        269.178162f,  109.476158f,  271.163391f,  206.420258f,  178.595078f,  193.378571f,
        145.081985f,  189.901855f,  154.901489f,  153.925735f,  174.659363f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -58.402885f,
        92.206772f,   -83.601959f,  107.811317f,  -55.025146f,  89.206841f,   -33.817093f,
        29.719433f,   -64.048363f,  100.298203f,  -18.401829f,  67.295204f,   -11.861612f,
        56.952404f,   -80.513893f,  80.030212f,   -30.218300f,  100.987221f,  -37.134293f,
        89.581131f,   -55.472050f,  101.563812f,  -46.553284f,  70.466393f,   -54.009655f,
        85.593895f,   -6.467272f,   82.054092f,   5.924163f,    74.561798f,   -20.547731f,
        72.006058f,   42.048912f,   130.520538f,  48.203678f,   119.416862f,  7.424225f,
        119.606644f,  27.466469f,   98.094704f,   12.885093f,   105.030159f,  1.874713f,
        122.407463f,  7.467847f,    146.723068f,  9.771024f,    121.711441f,  105.436478f,
        203.019928f,  100.568886f,  189.505371f,  92.597290f,   164.157501f,  109.541908f,
        125.852257f,  122.702980f,  133.280670f,  115.902802f,  139.067535f,  34.566261f,
        187.978210f,  47.479301f,   167.457886f,  211.097061f,  269.925842f,  233.761353f,
        263.598358f,  236.175034f,  224.529785f,  193.936356f,  196.460907f,  266.014954f,
        167.571869f,  314.249359f,  156.529800f,  199.277817f,  201.353210f,  121.389954f,
        227.351608f,  461.146484f,  255.248932f,  427.568939f,  302.858582f,  364.033997f,
        314.669464f,  399.558350f,  243.122955f,  385.210052f,  209.055466f,  237.874954f,
        231.301056f,  186.467697f,  236.698990f,  268.386444f,  242.863907f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    9.499181f,    4.729012f,    18.855597f,
        61.500931f,   16.545872f,   -0.550019f,   7.265460f,    15.571932f,   -0.407087f,
        -0.330391f,   -0.278819f,   -0.174538f,   -0.168517f,   0.073461f,    33.220619f,
        7.697248f,    34.373470f,   -0.788588f,   22.930552f,   2.152229f,    13.579879f,
        0.092207f,    0.000000f,    0.000000f,    -12.646818f,  1.587915f,    -26.591446f,
        -0.646145f,   -37.622459f,  20.532257f,   -41.364071f,  67.940971f,   30.455784f,
        -7.181870f,   29.589478f,   -15.262061f,  15.328509f,   -2.253427f,   9.631210f,
        -3.241396f,   0.000000f,    0.000000f,    -9.630488f,   -6.103816f,   -21.685396f,
        -5.791027f,   -36.790131f,  18.651922f,   -49.818310f,  46.116440f,   29.948597f,
        51.468632f,   4.512446f,    -10.265667f,  -21.159117f,  -21.326155f,  20.051836f,
        -1.398002f,   36.777905f,   -0.437783f,   18.545509f,   22.061426f,   26.496641f,
        -31.680086f,  21.653816f,   -69.810921f,  2.771482f,    15.777875f,   -147.866272f,
        49.210480f,   -150.104385f, 43.300301f,   0.133467f,    -24.145807f,  -2.690932f,
        -2.875406f,   0.000000f,    0.000000f,    -6.032296f,   15.584331f,   -12.286887f,
        41.996929f,   -17.928022f,  60.682194f,   -23.755095f,  84.891113f,   -259.597565f,
        102.336357f,  -142.517258f, 31.473053f,   24.032375f,   -35.810505f,  25.428146f,
        -30.432436f,  0.000000f,    0.000000f,    -2.246653f,   21.510952f,   -21.478857f,
        56.301056f,   -39.506245f,  99.009857f,   -48.666191f,  98.269691f,   94.310692f,
        -202.957230f, 153.196869f,  -221.604034f, -115.975128f, 16.715506f,   -83.549889f,
        19.469460f,   0.000000f,    0.000000f,    63.109631f,   -0.456589f,   179.107803f,
        -109.841400f, 284.198700f,  -164.954208f, 378.277374f,  -187.820923f, -0.359033f,
        1.590954f,    4.583269f,    0.637560f,    3.475705f,    1.953476f,    1.666804f,
        1.525692f,    -7.395854f,   -38.366306f,  -5.399721f,   -16.253723f,  -3.041335f,
        -6.664431f,   -1.716536f,   -3.448447f,   0.000000f,    0.000000f,    2.316635f,
        -0.599103f,   10.190424f,   1.075681f,    25.662663f,   1.373762f,    -0.764161f,
        -10.400259f,  4.956886f,    -34.340004f,  5.176708f,    -10.628201f,  5.087346f,
        -6.445235f,   2.054750f,    -1.029672f,   0.000000f,    0.000000f,    -2.565073f,
        2.670201f,    -6.687191f,   2.509181f,    -8.624598f,   3.264614f,    22.008724f,
        -7.148268f,   11.354022f,   -39.680176f,  10.322844f,   -16.548254f,  7.680575f,
        -5.813191f,   5.395893f,    -4.145935f,   0.000000f,    0.000000f,    -0.899536f,
        0.173062f,    -4.745577f,   5.162898f,    -6.525725f,   3.079253f,    2.492773f,
        -4.935689f,   -9.340886f,   -158.816772f, 1.806958f,    -84.421219f,  3.804840f,
        -56.065502f,  1.868464f,    -19.541983f,  0.000000f,    0.000000f,    -0.440988f,
        0.286715f,    -5.146830f,   19.843554f,   -4.609875f,   23.163853f,   -2.395236f,
        17.944456f,   5.556277f,    -1.420271f,   1.866947f,    1.620580f,    1.356347f,
        -1.535713f,   2.787455f,    -1.246161f,   2.807994f,    -1.508173f,   4.525356f,
        -1.113301f,   4.845267f,    1.385428f,    7.672036f,    10.604269f,   -4.242628f,
        7.357068f,    -2.668348f,   -3.096263f,   0.331891f,    -0.173615f,   -2.856624f,
        -0.619700f,   4.484685f,    -5.667960f,   4.176884f,    -6.956563f,   -0.948624f,
        -4.818060f,   -0.835136f,   -4.782157f,   -7.607145f,   -5.841280f,   -1.308580f,
        -4.540517f,   20.000000f,   10.000000f,   -0.024031f,   -6.640037f,   -7.868827f,
        -38.794659f,  8.444098f,    -16.570475f,  9.368288f,    0.586674f,    -33.640625f,
        -28.690416f,  -0.047625f,   -12.361497f,  3.540220f,    11.288424f,   -41.170647f,
        -29.346928f,  -13.990524f,  -2.557305f,   5.855820f,    9.669082f,    -53.408958f,
        -6.615739f,   -6.092059f,   -0.267264f,   1.279696f,    34.949139f,   -65.951660f,
        -27.849993f,  -45.678902f,  12.810072f,   0.000000f,    0.000000f,    -9.434441f,
        -7.962570f,   -14.338890f,  -3.827011f,   -9.451066f,   -4.510285f,   -2.160450f,
        -7.079933f,   19.738701f,   -14.568232f,  48.094837f,   -21.503027f,  117.811424f,
        -30.688240f,  277.101776f,  -79.092049f,  1.829898f,    -0.423722f,   70.624527f,
        -0.325220f,   41.878487f,   -1.063685f,   44.304111f,   2.298944f,    43.300282f,
        3.561805f,    0.252964f,    -8.652994f,   0.358956f,    -3.007558f,   0.438134f,
        -1.147229f,   0.563290f,    0.650776f,    0.856064f,    0.580489f,    1.000000f,
        1.000000f,    0.826508f,    11.688032f,   -11.899350f,  7.735900f,    -17.776768f,
        -48.063122f,  -32.200455f,  -44.779350f,  -64.338989f,  -13.422039f,  -40.592739f,
        -4.837716f,   -8.653460f,   6.446419f,    -3.615486f,   0.085016f,    -16.815790f,
        28.027201f,   0.000000f,    0.000000f,    -1.211410f,   -3.350837f,   -7.090258f,
        -11.442430f,  -26.925020f,  -8.114380f,   -39.630035f,  -3.654401f,   -67.752510f,
        24.158766f,   -122.100304f, 87.957863f,   0.000000f,    0.000000f,    -20.865341f,
        -6.362740f,   -57.069176f,  -5.019099f,   -98.249458f,  9.417927f,    -140.062057f,
        53.080772f,   -285.105347f, 202.382187f,  -292.736908f, 204.551880f,  -1.034215f,
        -3.275566f,   -0.212381f,   -0.331601f,   -12.236066f,  -7.615670f,   -2.359413f,
        -11.178076f,  23.991657f,   3.577039f};

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
