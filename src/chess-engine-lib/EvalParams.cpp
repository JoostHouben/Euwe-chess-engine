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
        0.209377f,    1.076343f,    0.307485f,    0.160131f,    0.308390f,    1.000000f,
        -0.000000f,   -0.000000f,   -0.028235f,   0.000572f,    34.948891f,   -24.367159f,
        145.025833f,  330.417999f,  353.795807f,  318.558777f,  397.834473f,  445.205933f,
        678.408569f,  1162.770752f, 1091.614990f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -40.848774f,  143.086533f,  -56.527046f,
        145.837692f,  -57.176800f,  149.930725f,  -70.522949f,  144.849045f,  -58.701706f,
        163.801788f,  -16.127377f,  151.098969f,  -5.859285f,   144.507721f,  -31.883945f,
        136.651398f,  -32.290432f,  141.101685f,  -42.769238f,  137.984970f,  -41.509453f,
        144.811737f,  -46.649673f,  153.338669f,  -37.754490f,  150.095795f,  -15.657163f,
        141.054092f,  -11.186348f,  138.370590f,  -17.991053f,  135.900452f,  -17.176651f,
        145.477509f,  -17.093637f,  147.341568f,  -20.797369f,  151.234268f,  -17.428209f,
        144.555069f,  -9.022434f,   134.693298f,  -2.772786f,   147.447784f,  -12.143588f,
        152.288284f,  -17.940477f,  145.230423f,  -6.940238f,   166.029129f,  2.567981f,
        146.890533f,  -6.637781f,   155.574615f,  -3.771838f,   147.558823f,  9.491655f,
        146.550751f,  20.829521f,   137.046143f,  7.287075f,    151.960419f,  -5.566237f,
        152.587296f,  11.672374f,   167.853210f,  -3.735785f,   158.342056f,  13.754975f,
        170.289703f,  -0.222715f,   124.271484f,  24.288876f,   122.441780f,  69.246895f,
        167.911621f,  88.589745f,   154.280762f,  35.321396f,   168.967941f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    278.234100f,  284.898529f,  293.162781f,  317.421814f,  289.639313f,
        350.171265f,  295.298584f,  354.446503f,  292.040771f,  351.581238f,  302.040985f,
        333.304108f,  280.862793f,  322.696991f,  245.848694f,  327.217560f,  290.079712f,
        328.024658f,  317.141815f,  338.807129f,  301.977356f,  345.163147f,  322.978851f,
        353.364563f,  318.862732f,  352.633484f,  307.169678f,  363.565094f,  298.345245f,
        362.130829f,  296.245056f,  320.553925f,  297.893616f,  330.119934f,  326.461639f,
        348.170197f,  328.948334f,  342.793610f,  343.591583f,  366.461578f,  347.438782f,
        363.231415f,  332.636383f,  339.319550f,  335.206329f,  325.687683f,  298.679901f,
        328.271179f,  332.993469f,  362.610718f,  366.213745f,  347.224365f,  366.311218f,
        356.370819f,  350.667023f,  360.377960f,  359.228699f,  371.236481f,  352.906952f,
        363.971954f,  339.049530f,  364.589844f,  329.888214f,  351.532593f,  340.777985f,
        362.498993f,  352.775757f,  367.205750f,  372.264191f,  363.987335f,  369.510681f,
        372.282715f,  370.486633f,  360.933899f,  377.678467f,  360.662994f,  354.031555f,
        356.967804f,  356.078156f,  356.701385f,  331.550018f,  369.877960f,  359.318909f,
        378.398560f,  390.128143f,  352.164062f,  389.079376f,  353.926819f,  433.186707f,
        346.533752f,  400.538757f,  352.347015f,  385.787628f,  356.222168f,  317.278687f,
        356.486755f,  309.856110f,  379.048126f,  333.738770f,  368.078094f,  365.380249f,
        361.014709f,  351.213531f,  370.098572f,  378.456726f,  365.481873f,  383.511597f,
        343.491302f,  354.914978f,  355.177979f,  323.895050f,  365.901581f,  173.297668f,
        333.090454f,  275.625702f,  376.295868f,  283.066528f,  402.013885f,  347.602600f,
        379.922180f,  324.604706f,  377.487885f,  443.175903f,  375.403503f,  180.343536f,
        380.780304f,  151.463440f,  212.407639f,  270.024658f,  385.286255f,  317.720276f,
        377.253326f,  295.777252f,  388.561005f,  288.908417f,  391.495117f,  288.744263f,
        386.296173f,  294.103485f,  385.704681f,  307.606354f,  383.732666f,  301.730499f,
        337.590179f,  314.211304f,  390.997467f,  304.030365f,  377.883545f,  321.698303f,
        395.790131f,  305.018341f,  395.467865f,  316.548553f,  396.693451f,  324.900604f,
        388.562622f,  333.208252f,  382.320312f,  327.357330f,  347.895721f,  305.467133f,
        372.430267f,  324.857239f,  394.912933f,  322.087799f,  399.317749f,  326.602997f,
        397.753143f,  328.690979f,  409.044037f,  326.945465f,  396.917389f,  328.820496f,
        383.355225f,  319.154388f,  394.932953f,  320.722992f,  391.964661f,  330.448456f,
        392.963715f,  322.555634f,  407.837219f,  339.876648f,  407.986023f,  342.955994f,
        407.708282f,  328.444733f,  401.912384f,  343.627380f,  394.022858f,  329.822815f,
        393.645508f,  308.958771f,  412.095367f,  321.574768f,  409.247009f,  340.072906f,
        410.349091f,  359.870941f,  401.510559f,  354.126862f,  410.250000f,  342.252075f,
        409.454254f,  326.351746f,  408.860199f,  312.601196f,  400.231201f,  318.393127f,
        408.230927f,  325.346497f,  415.569794f,  333.048248f,  404.303986f,  359.696991f,
        401.241241f,  351.599365f,  414.439789f,  427.917816f,  383.106506f,  372.831879f,
        400.688171f,  365.431488f,  387.115936f,  298.862244f,  401.827484f,  319.802185f,
        413.677399f,  332.365356f,  419.349243f,  298.948639f,  410.729065f,  365.486603f,
        392.447693f,  328.748901f,  407.240875f,  364.549561f,  388.015808f,  308.473053f,
        374.056641f,  286.792877f,  412.565491f,  298.783173f,  412.409027f,  295.875610f,
        410.348145f,  305.979401f,  410.009583f,  291.133179f,  415.784088f,  273.281769f,
        406.797302f,  330.209106f,  373.553894f,  374.780212f,  380.112213f,  455.858002f,
        654.651611f,  448.868591f,  656.012146f,  444.218445f,  666.829102f,  444.195435f,
        662.185913f,  445.974121f,  657.821167f,  438.602234f,  668.425293f,  430.751129f,
        664.594543f,  432.663849f,  645.942017f,  445.916931f,  659.429565f,  438.764496f,
        657.078674f,  431.869812f,  669.853760f,  436.719360f,  669.163269f,  431.820435f,
        672.413818f,  439.391235f,  664.929260f,  432.709961f,  656.120483f,  389.262421f,
        676.631104f,  423.802460f,  679.160156f,  437.093475f,  663.902405f,  424.463226f,
        680.419739f,  427.597504f,  674.669922f,  422.611511f,  675.221069f,  410.772797f,
        684.921753f,  417.479462f,  681.924866f,  432.004120f,  663.204041f,  434.063721f,
        688.904846f,  424.764648f,  696.067993f,  425.259796f,  695.439270f,  426.842621f,
        692.205811f,  424.853088f,  697.139038f,  419.207275f,  697.050964f,  443.230621f,
        694.580627f,  415.007416f,  699.533447f,  438.786835f,  704.470459f,  431.442627f,
        706.613647f,  449.150970f,  695.331665f,  469.982941f,  688.774963f,  453.148773f,
        698.100098f,  460.475372f,  698.389221f,  422.397125f,  706.175537f,  445.262146f,
        698.735352f,  429.827393f,  710.810608f,  464.842285f,  697.037537f,  466.568237f,
        693.289673f,  480.125427f,  689.743164f,  491.320984f,  684.054016f,  489.413116f,
        691.461792f,  498.716644f,  683.523071f,  491.439850f,  696.394104f,  450.376892f,
        707.647156f,  463.595001f,  708.983459f,  487.897919f,  695.121643f,  502.428986f,
        695.958435f,  475.738373f,  701.275818f,  507.715759f,  688.096497f,  467.328979f,
        702.246399f,  470.549042f,  699.739014f,  471.640472f,  685.446167f,  465.730164f,
        698.020325f,  477.909760f,  693.914001f,  455.499329f,  701.217712f,  446.240234f,
        707.059509f,  451.808228f,  710.323303f,  420.205231f,  721.087708f,  511.972931f,
        689.755188f,  1175.309814f, 1013.811462f, 1165.573975f, 1025.518555f, 1155.746094f,
        1041.679810f, 1160.431030f, 1026.832275f, 1168.264160f, 1016.984619f, 1134.604492f,
        1021.418396f, 1146.129639f, 925.340759f,  1123.823364f, 1020.047546f, 1169.471436f,
        1064.009399f, 1175.922119f, 1022.867981f, 1177.032104f, 1041.383179f, 1170.991577f,
        1045.352539f, 1165.347046f, 1053.971313f, 1165.229370f, 1017.293396f, 1163.867188f,
        969.065430f,  1123.153198f, 1083.552734f, 1171.441406f, 1063.656250f, 1166.531372f,
        1080.360229f, 1160.500244f, 1080.772095f, 1159.553345f, 1086.372314f, 1162.461792f,
        1081.528442f, 1161.634888f, 1078.087646f, 1167.492798f, 1056.276855f, 1154.802734f,
        1083.277222f, 1164.243408f, 1108.641235f, 1159.595337f, 1105.900757f, 1159.875122f,
        1126.965210f, 1146.310791f, 1133.837891f, 1163.668701f, 1114.914429f, 1158.895752f,
        1132.002563f, 1152.805786f, 1120.826416f, 1147.333008f, 1160.786621f, 1170.744995f,
        1090.823853f, 1157.490112f, 1125.565674f, 1138.289429f, 1190.073364f, 1135.522217f,
        1162.276489f, 1146.200684f, 1169.138306f, 1150.793091f, 1173.460205f, 1137.990479f,
        1186.315063f, 1154.920288f, 1149.093750f, 1176.037964f, 1109.144653f, 1149.136597f,
        1133.947876f, 1118.185059f, 1205.943359f, 1137.967773f, 1186.419434f, 1133.281860f,
        1215.438721f, 1192.697021f, 1159.381470f, 1214.877808f, 1084.727417f, 1153.252563f,
        1183.123901f, 1152.374634f, 1133.586426f, 1137.263184f, 1150.896973f, 1163.596680f,
        1154.241455f, 1144.858276f, 1176.475464f, 1119.687500f, 1225.415039f, 1137.175293f,
        1176.368408f, 1158.227295f, 1152.328857f, 1187.995483f, 1123.441406f, 1158.721313f,
        1127.446655f, 1192.201050f, 1089.572021f, 1153.646240f, 1173.177856f, 1124.750610f,
        1192.615723f, 1119.686035f, 1222.254272f, 1181.352295f, 1170.200684f, 1193.630005f,
        1141.627686f, 1210.952271f, 1131.696533f, 60.269501f,   -51.466095f,  24.558126f,
        -19.288624f,  -37.711742f,  5.857123f,    -51.974800f,  -16.124668f,  -36.563236f,
        -16.458456f,  -55.577934f,  11.937915f,   25.860817f,   -4.050913f,   47.306282f,
        -16.550007f,  24.239954f,   -36.295345f,  -24.983673f,  -5.490068f,   -82.477264f,
        5.762011f,    -100.859131f, 2.463486f,    -92.865356f,  4.008450f,    -62.456573f,
        2.210873f,    -5.738539f,   -10.130462f,  22.426437f,   -28.205437f,  -31.475460f,
        -27.261190f,  -42.567963f,  -10.571101f,  -115.175278f, 4.630921f,    -112.225639f,
        4.996130f,    -118.397163f, 9.913148f,    -110.632378f, 11.082023f,   -59.920113f,
        -11.248386f,  -36.620998f,  -26.276077f,  -10.870861f,  -31.073862f,  -18.885736f,
        -15.775042f,  -57.679729f,  0.329208f,    -116.609131f, 9.339173f,    -86.742096f,
        4.175634f,    -131.798645f, 12.784891f,   -72.143829f,  -12.838597f,  -87.553055f,
        -35.335640f,  12.721938f,   -28.173426f,  -4.445113f,   -3.192252f,   -13.227065f,
        -2.286868f,   -58.215382f,  11.019466f,   -53.535545f,  12.645816f,   -8.721332f,
        -3.663168f,   -25.785257f,  -18.174021f,  -57.864231f,  -23.354025f,  13.055112f,
        9.094538f,    -1.964344f,   23.874281f,   -5.704830f,   16.885130f,   7.121190f,
        4.885731f,    84.372955f,   -14.632096f,  39.737915f,   25.858166f,   51.599987f,
        11.307730f,   9.973145f,    -12.204410f,  -2.726063f,   -7.493908f,   97.371040f,
        0.222034f,    56.979977f,   8.850802f,    77.460793f,   -2.627180f,   -16.572882f,
        3.544284f,    104.818764f,  -21.127460f,  14.654678f,   40.018841f,   143.246262f,
        -85.163048f,  49.724964f,   -48.714573f,  249.474854f,  -92.601006f,  110.742714f,
        -7.135517f,   65.836678f,   -10.583549f,  15.712453f,   -35.408062f,  179.374634f,
        -60.176800f,  -42.108482f,  53.055370f,   174.990845f,  -145.764511f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    23.405252f,   83.449661f,   37.361279f,
        58.511257f,   28.986462f,   78.378662f,   40.196339f,   147.005890f,  48.943699f,
        110.028252f,  106.284920f,  56.173538f,   120.266701f,  60.501503f,   80.051811f,
        71.973885f,   50.751053f,   81.703735f,   16.928888f,   94.207176f,   31.707058f,
        104.916245f,  58.612259f,   58.847443f,   34.503334f,   100.180351f,  74.099609f,
        86.825951f,   115.965714f,  45.298050f,   81.103951f,   61.072372f,   70.329216f,
        138.459259f,  51.925369f,   129.769363f,  59.757820f,   130.692123f,  76.948669f,
        105.780899f,  97.840355f,   104.582764f,  111.697075f,  112.805237f,  97.374977f,
        105.692696f,  94.677780f,   88.227783f,   62.544426f,   250.820511f,  87.472572f,
        235.087723f,  100.680641f,  189.592834f,  120.660950f,  161.673676f,  134.606262f,
        133.604889f,  155.457016f,  147.267792f,  152.382126f,  108.504906f,  194.037766f,
        111.856079f,  114.003349f,  368.930450f,  58.706940f,   307.421204f,  75.685127f,
        278.754456f,  85.154816f,   262.890991f,  204.697861f,  177.000717f,  171.571823f,
        166.092850f,  197.723755f,  161.299194f,  153.078323f,  183.982986f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -38.570374f,
        83.762497f,   -74.619400f,  101.352768f,  -58.555397f,  87.204117f,   -68.318100f,
        37.039108f,   -59.849117f,  90.945580f,   -12.785834f,  64.557533f,   -3.904011f,
        55.416531f,   -48.488060f,  67.191856f,   -19.626575f,  92.138763f,   -42.431759f,
        86.332069f,   -42.678093f,  92.118698f,   -61.432549f,  67.727631f,   -62.205479f,
        80.856064f,   -9.906183f,   79.526199f,   18.780186f,   60.618160f,   1.834960f,
        62.677715f,   50.066467f,   123.858559f,  43.266895f,   113.854088f,  1.654130f,
        109.761414f,  20.865351f,   87.966293f,   -4.361179f,   96.419647f,   8.551277f,
        113.416008f,  6.760764f,    136.977615f,  20.784920f,   112.446701f,  118.917488f,
        195.330246f,  107.109550f,  179.222534f,  92.838600f,   152.650665f,  102.279663f,
        115.208626f,  117.752136f,  117.618721f,  124.030663f,  127.750435f,  33.561806f,
        174.091614f,  65.527374f,   157.626938f,  221.522919f,  263.626770f,  232.586029f,
        257.592682f,  251.748856f,  209.454391f,  197.129410f,  184.129074f,  268.423309f,
        149.558167f,  323.678070f,  139.830704f,  221.292801f,  184.839218f,  148.860428f,
        214.453491f,  451.874847f,  249.425964f,  433.383972f,  301.067444f,  393.891235f,
        299.058441f,  432.610779f,  220.761917f,  400.589111f,  206.335403f,  260.495026f,
        222.387482f,  179.778778f,  236.764954f,  274.159546f,  239.988998f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    9.893740f,    7.587157f,    24.187494f,
        60.743843f,   27.435774f,   -3.085018f,   4.239604f,    15.922560f,   -0.402697f,
        -0.335524f,   -0.273765f,   -0.169813f,   -0.181781f,   0.080394f,    33.772293f,
        10.772705f,   36.569286f,   1.234934f,    24.237898f,   3.292182f,    14.709223f,
        3.099006f,    0.000000f,    0.000000f,    -12.464130f,  4.475906f,    -24.839809f,
        2.195284f,    -35.974583f,  39.026680f,   -23.596956f,  34.841572f,   29.018780f,
        -0.088549f,   29.982567f,   -10.757598f,  15.080331f,   1.256963f,    10.033600f,
        0.565452f,    0.000000f,    0.000000f,    -10.433333f,  1.635853f,    -19.879881f,
        2.782641f,    -35.428959f,  43.367378f,   -39.731964f,  26.307274f,   32.429131f,
        49.629112f,   3.507989f,    -10.406727f,  -24.389715f,  -23.924696f,  19.900614f,
        0.497082f,    36.521210f,   -0.730804f,   18.017849f,   19.460312f,   28.399185f,
        -30.772017f,  14.037680f,   -65.523552f,  -2.306085f,   20.304148f,   -119.065559f,
        45.166317f,   -148.408737f, 44.310352f,   3.322436f,    -22.813683f,  -0.051886f,
        -6.019700f,   0.000000f,    0.000000f,    -4.957750f,   14.173476f,   -11.292377f,
        39.888943f,   -17.009026f,  63.965145f,   -20.866968f,  93.615036f,   -248.523880f,
        102.072525f,  -144.249466f, 37.141811f,   29.734119f,   -35.544071f,  28.816622f,
        -29.561953f,  0.000000f,    0.000000f,    -3.057333f,   21.770761f,   -20.706533f,
        55.032036f,   -38.074455f,  98.031532f,   -26.693020f,  80.936958f,   60.772167f,
        -133.223145f, 169.459991f,  -224.117401f, -112.644279f, 24.175007f,   -86.873070f,
        29.587992f,   0.000000f,    0.000000f,    66.402641f,   -10.993203f,  180.237854f,
        -124.567604f, 284.667633f,  -187.855148f, 335.346222f,  -158.568695f, -0.455187f,
        1.907472f,    4.729795f,    0.789845f,    3.638630f,    2.002010f,    1.943337f,
        0.974736f,    -7.841053f,   -40.950832f,  -5.711403f,   -17.459156f,  -4.146700f,
        -5.531581f,   -2.310111f,   -2.459715f,   0.000000f,    0.000000f,    1.999314f,
        0.592852f,    7.369314f,    2.795801f,    12.331390f,   -2.016572f,   4.404088f,
        -12.429721f,  6.558961f,    -32.703644f,  6.052390f,    -11.203760f,  6.905428f,
        -8.606024f,   3.377426f,    -1.548241f,   0.000000f,    0.000000f,    -3.200141f,
        1.980801f,    -7.772597f,   3.572037f,    -11.144628f,  3.675663f,    3.680743f,
        -2.063979f,   11.512866f,   -39.397167f,  11.125672f,   -15.807954f,  8.036880f,
        -5.821084f,   5.747971f,    -3.088553f,   0.000000f,    0.000000f,    -1.119080f,
        0.680175f,    -5.179493f,   5.060587f,    -6.299526f,   2.488646f,    0.901953f,
        -5.523013f,   -17.507769f,  -114.817497f, 2.289201f,    -82.320068f,  3.363483f,
        -56.546783f,  0.158826f,    -12.415951f,  0.000000f,    0.000000f,    -3.004595f,
        10.958919f,   -7.593483f,   24.999649f,   -6.502264f,   25.042034f,   -6.408082f,
        27.923727f,   5.666204f,    -1.258444f,   1.972788f,    1.603671f,    1.075287f,
        -1.082805f,   2.427335f,    -1.032021f,   2.939469f,    -1.746112f,   5.599378f,
        -2.003793f,   4.454642f,    0.750928f,    6.791845f,    11.076316f,   -5.556890f,
        7.767446f,    -2.234332f,   -3.106482f,   0.720746f,    -0.377043f,   -3.251923f,
        -0.251393f,   4.779998f,    -5.587671f,   3.749542f,    -6.454880f,   -0.347584f,
        -3.621035f,   -0.916800f,   -4.661890f,   -7.741260f,   -5.527608f,   -1.154518f,
        -5.068318f,   20.000000f,   10.000000f,   -0.642434f,   -6.388814f,   -7.288962f,
        -38.512993f,  8.260841f,    -16.436493f,  7.917581f,    1.390890f,    -34.214939f,
        -28.365042f,  -0.227144f,   -13.717808f,  2.455152f,    12.502699f,   -40.829170f,
        -28.069799f,  -13.363543f,  -5.037627f,   6.324837f,    8.446227f,    -55.292316f,
        -6.830002f,   -5.253050f,   -2.491801f,   0.797510f,    35.100372f,   -61.676628f,
        -36.466469f,  -43.076321f,  -2.845502f,   0.000000f,    0.000000f,    -9.307057f,
        -8.379256f,   -15.538545f,  -4.434106f,   -8.121918f,   -5.012593f,   -0.724365f,
        -7.953955f,   21.570354f,   -14.115604f,  59.243179f,   -23.729544f,  126.267685f,
        -34.658047f,  267.789124f,  -74.165520f,  0.582971f,    -1.075751f,   73.735245f,
        -0.614819f,   39.432968f,   -1.670199f,   44.903221f,   2.909323f,    38.388317f,
        3.999665f,    0.232452f,    -6.700560f,   0.367345f,    -2.436939f,   0.443079f,
        -0.960281f,   0.580768f,    0.340993f,    0.820154f,    0.263827f,    1.000000f,
        1.000000f,    1.085113f,    11.458601f,   -11.087776f,  5.472175f,    -18.698397f,
        -48.747517f,  -25.708513f,  -40.954956f,  -57.443211f,  -22.830490f,  -32.240822f,
        -4.754366f,   -7.495880f,   7.104698f,    -3.940165f,   0.657656f,    -19.342354f,
        27.391323f,   0.000000f,    0.000000f,    -1.879725f,   -3.402465f,   -8.193414f,
        -10.172323f,  -27.679632f,  -6.893164f,   -38.687038f,  -3.134661f,   -72.467018f,
        27.872305f,   -111.721405f, 71.623276f,   0.000000f,    0.000000f,    -21.988457f,
        -5.111017f,   -54.675182f,  -6.050581f,   -93.625336f,  4.586426f,    -150.348068f,
        63.936371f,   -235.233109f, 156.068497f,  -289.103729f, 185.692886f,  -3.890319f,
        1.396865f,    -5.089827f,   -2.134722f,   26.356346f,   6.656621f};

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
            "\npotentialHoleAdjustment:\n\t{}",
            taperedTermToString(params.potentialHoleAdjustment));
    oss << std::format("\nholeAdjustment:\n\t{}", taperedTermToString(params.holeAdjustment));
    oss << std::format(
            "\nknightOnOutpostAdjustment:\n\t{}",
            taperedTermToString(params.knightOnOutpostAdjustment));

    return oss.str();
}
