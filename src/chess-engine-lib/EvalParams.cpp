#include "EvalParams.h"

#include "Piece.h"
#include "RangePatches.h"

#include <format>
#include <ranges>
#include <sstream>

#include <cstring>

namespace {

constexpr EvalParamArray kDefaultParams = {
        0.209377f,     1.000000f,    0.762991f,    1.679964f,    6.320265f,    0.000000f,
        1.076343f,     0.265785f,    0.220929f,    0.439011f,    0.052389f,    1.000000f,
        -0.008578f,    -0.015662f,   -0.002482f,   -0.005392f,   203.419769f,  -37.537865f,
        197.999878f,   403.345978f,  339.238281f,  409.171539f,  421.797638f,  533.535583f,
        771.279236f,   1500.242920f, 1046.177124f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    -77.182846f,  193.695343f,  -83.336830f,
        197.031952f,   -71.022461f,  202.421173f,  -83.731979f,  193.499664f,  -68.235619f,
        223.425949f,   -9.957750f,   200.306610f,  -17.741337f,  187.929916f,  -53.574448f,
        179.767792f,   -63.531456f,  192.900436f,  -59.218426f,  192.130524f,  -51.682590f,
        198.788025f,   -50.255688f,  204.033234f,  -40.056923f,  206.161209f,  -20.193197f,
        194.954620f,   -20.113754f,  189.868790f,  -31.529737f,  183.639404f,  -51.803959f,
        205.590408f,   -43.470573f,  206.503082f,  -29.544676f,  202.192627f,  -22.853909f,
        204.958282f,   -24.505999f,  205.904846f,  -4.313689f,   198.551788f,  -21.890390f,
        205.283768f,   -34.621525f,  195.354492f,  -30.953503f,  222.207642f,  -30.281784f,
        230.004257f,   -14.477155f,  220.210251f,  0.402473f,    209.740311f,  18.218307f,
        211.333939f,   8.198812f,    204.179138f,  -17.769510f,  224.598709f,  -15.297308f,
        203.488464f,   0.519743f,    241.169800f,  -37.026836f,  243.855453f,  5.781870f,
        241.811111f,   11.550213f,   198.259491f,  29.024588f,   301.137726f,  98.542854f,
        224.516251f,   50.612465f,   219.784622f,  60.036579f,   212.214066f,  0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     293.862549f,  355.232819f,  359.931427f,  301.195007f,  359.305389f,
        328.757965f,   364.720184f,  340.455353f,  368.484894f,  321.459930f,  378.801636f,
        309.255371f,   348.944397f,  297.304443f,  337.800415f,  291.416901f,  363.562347f,
        312.294098f,   319.811981f,  376.848083f,  378.416229f,  340.524292f,  400.104218f,
        331.296875f,   391.507202f,  326.205597f,  401.127655f,  322.309235f,  364.546814f,
        307.981995f,   378.638611f,  328.678467f,  364.935577f,  334.099976f,  390.282043f,
        346.011566f,   397.384094f,  336.823669f,  415.047668f,  347.507233f,  426.335175f,
        341.199432f,   406.702332f,  311.643127f,  402.414978f,  314.825714f,  378.446655f,
        314.779785f,   387.987701f,  348.057373f,  418.677246f,  361.487457f,  434.728271f,
        352.153931f,   422.788635f,  358.286346f,  431.425415f,  364.254852f,  426.772339f,
        343.855988f,   425.173370f,  338.092987f,  388.052734f,  332.032715f,  410.008759f,
        358.557281f,   422.562164f,  358.342163f,  442.465729f,  349.877441f,  479.351807f,
        360.049896f,   442.222992f,  354.526306f,  462.690582f,  358.776245f,  433.322510f,
        353.998840f,   438.385284f,  339.566589f,  356.820374f,  359.283905f,  441.198608f,
        354.073914f,   470.352051f,  344.439148f,  480.762482f,  342.875854f,  525.336304f,
        339.935822f,   534.426086f,  326.061951f,  449.939056f,  339.328613f,  416.324554f,
        330.033936f,   404.608215f,  346.077576f,  391.652130f,  360.502716f,  438.127014f,
        354.103027f,   456.918854f,  353.727112f,  443.364441f,  355.745056f,  504.396118f,
        318.339386f,   416.078064f,  337.595673f,  272.847931f,  362.856018f,  220.958878f,
        317.326447f,   346.431671f,  343.164520f,  317.064301f,  366.198547f,  352.475525f,
        359.495270f,   453.636444f,  325.510406f,  341.095062f,  351.642273f,  302.489258f,
        345.732025f,   329.945892f,  222.440796f,  347.794342f,  417.937042f,  413.801208f,
        398.020691f,   378.829010f,  414.246094f,  382.896545f,  420.675629f,  387.087158f,
        415.853851f,   378.043915f,  424.645660f,  360.379974f,  430.964539f,  350.124115f,
        404.524323f,   424.372833f,  397.752289f,  394.301300f,  404.216003f,  422.735321f,
        404.520569f,   393.642029f,  428.813171f,  409.343842f,  418.597321f,  420.368378f,
        411.802887f,   434.447266f,  395.807373f,  395.740509f,  373.865997f,  386.410492f,
        410.485596f,   415.812225f,  420.317535f,  410.397064f,  429.529541f,  417.341187f,
        424.383636f,   414.400940f,  430.640015f,  424.539490f,  412.938538f,  423.810699f,
        418.131195f,   417.483643f,  414.235107f,  393.608154f,  420.663513f,  407.726593f,
        420.110626f,   411.291412f,  430.636597f,  426.170441f,  435.272949f,  441.418488f,
        422.306732f,   408.241058f,  436.156891f,  422.790222f,  429.049377f,  422.159424f,
        400.583221f,   397.539917f,  425.172974f,  408.584167f,  439.367859f,  432.437561f,
        427.251465f,   462.422760f,  420.874512f,  453.863800f,  420.590302f,  445.735718f,
        427.129150f,   417.564575f,  434.583008f,  416.011444f,  419.784698f,  412.304077f,
        426.689697f,   434.208588f,  432.007233f,  444.170135f,  427.216431f,  468.751190f,
        411.293976f,   477.466919f,  410.389160f,  505.685150f,  419.374573f,  468.550659f,
        439.776001f,   451.057770f,  409.299347f,  404.753723f,  436.621979f,  399.284119f,
        443.897034f,   437.672577f,  434.143646f,  406.400757f,  446.089569f,  416.937317f,
        440.923157f,   420.874939f,  441.228882f,  432.477661f,  425.995178f,  380.588898f,
        381.970459f,   373.003387f,  450.773865f,  423.542389f,  423.904053f,  298.531006f,
        467.850769f,   353.321045f,  454.076385f,  418.024719f,  433.159637f,  314.758820f,
        457.738739f,   462.885132f,  400.666046f,  472.359497f,  396.911926f,  547.157043f,
        755.172058f,   545.061340f,  745.289307f,  539.096069f,  755.336365f,  534.713318f,
        752.156189f,   532.833984f,  748.254517f,  525.302734f,  753.998840f,  509.727295f,
        745.902771f,   495.604858f,  751.482300f,  521.508118f,  764.411194f,  531.498352f,
        753.868835f,   527.439758f,  761.222839f,  524.335938f,  758.418762f,  522.861633f,
        757.323120f,   526.798645f,  740.300842f,  512.970764f,  751.618835f,  442.428894f,
        781.265930f,   518.703552f,  768.649048f,  524.720093f,  757.539185f,  509.775024f,
        767.793396f,   499.729065f,  779.785645f,  524.328430f,  757.270142f,  502.062103f,
        761.914124f,   523.941223f,  751.158020f,  482.613281f,  764.575317f,  525.533142f,
        785.064392f,   528.394165f,  777.925964f,  512.496582f,  786.989136f,  526.256287f,
        773.340088f,   517.777893f,  778.200378f,  505.180298f,  784.058655f,  522.170410f,
        781.725769f,   494.764313f,  780.697083f,  537.398132f,  795.712585f,  541.226135f,
        786.922913f,   563.115234f,  780.179626f,  558.919678f,  778.627808f,  543.023071f,
        781.177490f,   532.843018f,  780.603027f,  541.370789f,  790.224182f,  528.090088f,
        796.902222f,   545.163452f,  806.385620f,  559.094238f,  790.307983f,  558.577515f,
        789.091675f,   571.874817f,  778.255310f,  611.432922f,  775.379517f,  587.757690f,
        784.082886f,   589.139893f,  791.076599f,  598.532166f,  789.610474f,  561.833130f,
        808.584412f,   548.349243f,  811.413147f,  573.682312f,  805.749817f,  587.501709f,
        800.309082f,   561.620117f,  813.964294f,  610.102722f,  775.966736f,  578.884033f,
        798.356689f,   631.540161f,  783.777405f,  566.594360f,  795.944153f,  589.154419f,
        792.772095f,   587.934265f,  793.051453f,  562.773865f,  802.748718f,  561.191528f,
        801.818726f,   550.930420f,  809.877380f,  569.224792f,  811.591980f,  579.863892f,
        804.944580f,   1516.809448f, 1027.313599f, 1499.372803f, 1039.759033f, 1499.412354f,
        1015.576294f,  1488.381714f, 1039.543579f, 1490.616943f, 1018.595825f, 1467.701782f,
        1003.632935f,  1397.998047f, 1028.007568f, 1288.767212f, 1226.924927f, 1506.595947f,
        1027.662964f,  1512.410522f, 1013.173279f, 1510.703979f, 1020.111816f, 1504.450195f,
        1024.931274f,  1507.325439f, 1012.452942f, 1512.552734f, 980.568542f,  1474.364136f,
        957.273315f,   1461.526001f, 989.513367f,  1500.201538f, 1064.438965f, 1497.836426f,
        1061.279175f,  1486.177124f, 1094.779541f, 1491.862183f, 1046.994019f, 1493.764526f,
        1057.098389f,  1500.227905f, 1048.195801f, 1509.685181f, 1008.548218f, 1474.817871f,
        1096.231567f,  1501.502686f, 1064.064941f, 1506.798340f, 1047.279907f, 1498.452271f,
        1042.424072f,  1495.600952f, 1059.657959f, 1495.349487f, 1081.089600f, 1493.225098f,
        1074.072388f,  1495.284424f, 1073.241333f, 1494.164429f, 1066.446655f, 1500.186157f,
        1077.283325f,  1477.792358f, 1118.641968f, 1495.853149f, 1106.593994f, 1498.408691f,
        1082.958008f,  1513.426514f, 1049.416382f, 1511.156738f, 1076.746094f, 1512.892700f,
        1061.766113f,  1528.773438f, 1053.213623f, 1522.391602f, 1018.838623f, 1519.002197f,
        1020.922241f,  1505.596558f, 1075.163330f, 1513.358154f, 1058.332153f, 1521.940186f,
        1052.730591f,  1615.792480f, 973.381775f,  1580.565674f, 978.130249f,  1554.849609f,
        1030.722778f,  1509.204956f, 1071.707397f, 1484.656616f, 1094.856567f, 1515.450684f,
        1071.883301f,  1541.401733f, 1037.890503f, 1549.982666f, 1015.192810f, 1540.820557f,
        1062.954712f,  1577.573730f, 999.059082f,  1622.986938f, 954.494202f,  1475.162354f,
        1136.160645f,  1523.080200f, 1071.343750f, 1501.413818f, 1098.530762f, 1509.793457f,
        1070.271362f,  1519.833862f, 1074.600952f, 1541.362915f, 1066.952759f, 1563.109741f,
        1030.014893f,  1470.826294f, 1165.339600f, 41.079479f,   -7.466346f,   17.804976f,
        2.707649f,     -47.008003f,  18.226824f,   -28.906120f,  -10.687514f,  -19.398697f,
        -21.738428f,   -46.468781f,  19.576603f,   22.199034f,   16.497282f,   38.764805f,
        -3.028586f,    3.683766f,    -11.701336f,  1.374628f,    -1.023290f,   -55.203480f,
        1.962299f,     -118.640610f, 15.061852f,   -101.079979f, 7.635085f,    -63.124573f,
        5.140117f,     -10.069040f,  0.028284f,    12.842147f,   -11.396296f,  -34.443684f,
        -6.070805f,    -13.564790f,  -20.312410f,  -80.644791f,  -5.510895f,   -60.114555f,
        -8.201650f,    -81.684967f,  -5.832401f,   -100.322464f, -2.502863f,   -52.686565f,
        -16.581293f,   -54.403770f,  -25.224247f,  -23.934771f,  -7.355361f,   54.184261f,
        -36.892010f,   -1.655424f,   -22.731798f,  -49.410759f,  -12.716649f,  -42.481403f,
        -16.482059f,   -43.773815f,  -17.549574f,  -19.628851f,  -36.578293f,  -105.294243f,
        -23.139030f,   5.243654f,    -5.134918f,   17.570707f,   -9.770037f,   -26.794458f,
        2.130630f,     7.135957f,    -15.799352f,  -30.124439f,  -8.352904f,   62.405083f,
        -31.848370f,   98.765617f,   -35.621120f,  -37.089928f,  -27.176952f,  -43.614349f,
        26.739359f,    -10.349983f,  30.224173f,   29.167397f,   11.571454f,   18.770592f,
        -0.366183f,    80.494377f,   -20.779930f,  69.563019f,   -0.126282f,   61.696091f,
        15.738067f,    -38.023945f,  4.807151f,    37.186058f,   12.795383f,   -14.598790f,
        40.469734f,    49.200954f,   31.954706f,   71.115898f,   7.862585f,    64.688179f,
        -10.694366f,   -46.112755f,  0.585934f,    -62.401833f,  45.660606f,   275.545990f,
        -79.532814f,   17.228891f,   -83.353195f,  46.491520f,   5.377580f,    -15.457056f,
        38.006451f,    -3.790687f,   19.910027f,   43.935062f,   -6.110074f,   69.362259f,
        -25.006165f,   -505.125244f, 104.191254f,  -548.146851f, -21.791107f,  0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    -26.447454f,  101.774200f,  -43.807213f,
        104.890793f,   -35.120388f,  105.993240f,  -140.752121f, 161.139023f,  10.850964f,
        69.831238f,    4.209830f,    76.138412f,   -2.067813f,   69.603241f,   -21.682682f,
        70.998573f,    -17.776897f,  124.616623f,  -9.741157f,   117.345467f,  -25.361635f,
        107.375244f,   -34.850037f,  92.443504f,   -30.472071f,  101.354797f,  -24.294918f,
        109.212303f,   -5.350316f,   102.603546f,  -3.020255f,   90.823097f,   31.079697f,
        171.067139f,   25.405050f,   156.182159f,  6.049560f,    140.643677f,  11.745346f,
        130.458466f,   -0.490486f,   136.328842f,  15.063646f,   146.679443f,  3.547714f,
        167.852554f,   -5.780792f,   162.109772f,  98.542603f,   240.892471f,  88.344551f,
        237.952087f,   59.972702f,   218.824554f,  65.219688f,   181.545105f,  86.713295f,
        151.268890f,   64.042267f,   159.371338f,  44.622768f,   203.061630f,  47.588856f,
        206.403397f,   200.835739f,  324.598785f,  159.401260f,  335.700806f,  157.553604f,
        288.443573f,   135.819000f,  260.639404f,  155.356949f,  211.242645f,  199.056427f,
        189.843643f,   127.812729f,  246.127197f,  164.197525f,  249.315247f,  389.602325f,
        351.877502f,   313.121490f,  378.124054f,  273.317780f,  365.793030f,  281.219208f,
        311.125702f,   205.787704f,  291.438446f,  81.779808f,   333.075378f,  -27.852150f,
        373.950470f,   154.604767f,  302.363892f,  0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     33.874756f,   55.748863f,   5.467504f,    11.201653f,   12.893435f,
        23.013233f,    32.402924f,   16.163015f,   38.185036f,   -1.996090f,   26.524187f,
        -3.831578f,    13.268478f,   -4.202244f,   0.000000f,    0.000000f,    -19.549612f,
        2.512906f,     -33.202011f,  -9.996095f,   -39.322277f,  -76.627800f,  11.286866f,
        -244.710526f,  22.011408f,   0.041040f,    28.810085f,   -11.545979f,  17.614874f,
        -5.638155f,    9.389325f,    1.554046f,    -0.000000f,   0.000000f,    -17.891361f,
        -0.580257f,    -26.663570f,  1.819405f,    -39.659489f,  35.043892f,   26.719933f,
        157.976700f,   35.526073f,   77.334793f,   -1.748557f,   -11.463704f,  3.788727f,
        -51.867836f,   23.815552f,   11.554214f,   45.775711f,   -4.556732f,   -570.266113f,
        170.143311f,   -20.022491f,  23.464466f,   10.798115f,   -11.953511f,  5.151730f,
        -3.673504f,    0.000000f,    0.000000f,    6.251929f,    14.896225f,   -0.926446f,
        38.929058f,    -9.746672f,   67.748039f,   -31.742001f,  158.689270f,  -489.921967f,
        212.154678f,   -162.798004f, 55.485378f,   7.111509f,    -25.326628f,  21.125259f,
        -28.528589f,   0.000000f,    0.000000f,    -1.029569f,   21.583603f,   -24.306011f,
        67.240959f,    -38.811626f,  116.962601f,  -133.441483f, 217.614304f,  1022.798889f,
        -1057.490723f, -2.131278f,   -110.900024f, -77.213623f,  -65.120087f,  -73.687241f,
        -25.398508f,   0.000000f,    0.000000f,    59.588741f,   12.734435f,   192.937088f,
        -70.218590f,   313.977112f,  -145.751846f, 628.993530f,  -595.205811f, 2.121923f,
        -0.046774f,    0.000000f,    0.000000f,    1.730271f,    6.589444f,    5.818400f,
        5.445964f,     2.508672f,    4.983530f,    1.472732f,    10.665158f,   0.000000f,
        0.000000f,     7.089542f,    -1.168809f,   0.644955f,    3.602764f,    0.256214f,
        -0.346547f,    2.463957f,    0.630634f,    2.834708f,    -2.783935f,   6.576183f,
        -2.716708f,    3.745637f,    2.280537f,    6.489309f,    14.346269f,   -1.123363f,
        -5.397287f,    0.436971f,    4.228431f,    -3.892327f,   0.210593f,    6.470593f,
        -4.934759f,    1.197106f,    9.248217f,    -2.381684f,   -3.289380f,   1.888646f,
        -7.881429f,    -4.928953f,   -11.278391f,  20.000000f,   10.000000f,   3.443915f,
        -4.238783f,    -10.485598f,  -47.780296f,  14.038358f,   -17.264109f,  9.895497f,
        4.732380f,     -43.795982f,  -32.755150f,  -0.507625f,   -13.199482f,  4.737419f,
        14.885345f,    -47.011189f,  -34.901402f,  -16.484207f,  -1.672351f,   4.635645f,
        12.829166f,    -53.290440f,  -7.935896f,   -3.979453f,   5.223494f,    4.594220f,
        34.198158f,    -75.046951f,  -12.755577f,  -50.279861f,  32.635853f,   0.000000f,
        0.000000f,     -80.051773f,  -8.948077f,   -80.464088f,  -9.262197f,   -66.468498f,
        -12.585833f,   -40.781952f,  -26.631504f,  -15.807521f,  -36.082901f,  23.893013f,
        -52.108555f,   54.487347f,   -55.047840f,  147.733383f,  -94.472237f,  0.000000f,
        0.000000f,     112.853897f,  6.591867f,    136.133118f,  -7.420008f,   138.129395f,
        3.899240f,     135.483826f,  0.718514f,    130.472778f,  16.526838f,   0.000000f,
        0.000000f,     0.000000f,    0.000000f,    -55.186279f,  0.635456f,    -168.570648f,
        -2.440918f,    -259.686493f, -8.238023f,   -326.950684f, -19.271776f,  -424.886993f,
        -19.793652f,   -12.923679f,  6.041505f,    -13.879775f,  -61.416145f,  -16.720070f,
        -50.211842f,   -75.602051f,  4.183792f,    -29.643673f,  -7.621462f,   -11.405621f,
        16.847055f,    -7.451942f,   8.894794f,    -13.825138f,  24.766811f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kPassedPawnPstIdx) {
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
            "\nconnectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.connectedPassedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

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

    oss << std::format("\nknightPawnAdjustment:\n\t{}", arrayToString(params.knightPawnAdjustment));

    oss << std::format("\nrookPawnAdjustment:\n\t{}", arrayToString(params.rookPawnAdjustment));

    oss << std::format("\nqueenPawnAdjustment:\n\t{}", arrayToString(params.queenPawnAdjustment));

    oss << std::format(
            "\nkingVirtualMobilityPenalty:\n\t{}",
            taperedTermToString(params.kingVirtualMobilityPenalty));

    oss << "\n\nmobilityBonus: {\n";
    writeTaperedTermPerPiece(params.mobilityBonus, oss);
    oss << "}\n";

    oss << "\nownKingTropism: {\n";
    writeTropismTable(params.ownKingTropism, oss);
    oss << "}\n";

    oss << "\nenemyKingTropism: {\n";
    writeTropismTable(params.enemyKingTropism, oss);
    oss << "}\n";

    oss << std::format("\ntempoBonus:\n\t{}", taperedTermToString(params.tempoBonus));

    oss << "\n\nattackDefendAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.attackDefendAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format("\ncontrolNearEnemyKing:\n\t{}", arrayToString(params.controlNearEnemyKing));

    oss << "\n\nkingAttackWeight: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.kingAttackWeight[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nnumKingAttackersAdjustment:\n\t{}",
            arrayToString(params.numKingAttackersAdjustment));

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

    return oss.str();
}
