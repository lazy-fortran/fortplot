module fortplot_colormap
    !! Colormap functionality for contour plots
    !! Provides color interpolation for different colormaps like matplotlib
    !!
    !! Viridis, plasma, and inferno use 256-entry lookup tables matching
    !! matplotlib's perceptually uniform colormaps (CAM02-fitted).
    !! Coolwarm, jet, and crest use control-point linear interpolation.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_color_definitions, only: to_lowercase
    use fortplot_constants, only: EPSILON_COMPARE
    implicit none
    
    private
    public :: get_colormap_color, colormap_value_to_color, validate_colormap_name
    
    integer, parameter :: CMAP_N = 256

    ! 256-entry viridis LUT (matplotlib reference)
    real(wp), dimension(CMAP_N), private :: viridis_r
    real(wp), dimension(CMAP_N), private :: viridis_g
    real(wp), dimension(CMAP_N), private :: viridis_b
    DATA viridis_r(  1: 10) / 0.267_wp, 0.269_wp, 0.270_wp, 0.271_wp, 0.273_wp, 0.274_wp, 0.275_wp, 0.276_wp, 0.277_wp, 0.278_wp /
    DATA viridis_r( 11: 20) / 0.279_wp, 0.280_wp, 0.280_wp, 0.281_wp, 0.281_wp, 0.282_wp, 0.282_wp, 0.283_wp, 0.283_wp, 0.283_wp /
    DATA viridis_r( 21: 30) / 0.283_wp, 0.283_wp, 0.283_wp, 0.283_wp, 0.283_wp, 0.283_wp, 0.282_wp, 0.282_wp, 0.281_wp, 0.281_wp /
    DATA viridis_r( 31: 40) / 0.280_wp, 0.280_wp, 0.279_wp, 0.278_wp, 0.277_wp, 0.276_wp, 0.275_wp, 0.274_wp, 0.273_wp, 0.272_wp /
    DATA viridis_r( 41: 50) / 0.271_wp, 0.269_wp, 0.268_wp, 0.267_wp, 0.265_wp, 0.264_wp, 0.262_wp, 0.261_wp, 0.259_wp, 0.257_wp /
    DATA viridis_r( 51: 60) / 0.256_wp, 0.254_wp, 0.252_wp, 0.250_wp, 0.249_wp, 0.247_wp, 0.245_wp, 0.243_wp, 0.241_wp, 0.239_wp /
    DATA viridis_r( 61: 70) / 0.237_wp, 0.236_wp, 0.234_wp, 0.232_wp, 0.230_wp, 0.228_wp, 0.226_wp, 0.224_wp, 0.222_wp, 0.220_wp /
    DATA viridis_r( 71: 80) / 0.218_wp, 0.216_wp, 0.214_wp, 0.212_wp, 0.211_wp, 0.209_wp, 0.207_wp, 0.205_wp, 0.203_wp, 0.201_wp /
    DATA viridis_r( 81: 90) / 0.199_wp, 0.198_wp, 0.196_wp, 0.194_wp, 0.192_wp, 0.191_wp, 0.189_wp, 0.187_wp, 0.186_wp, 0.184_wp /
    DATA viridis_r( 91:100) / 0.182_wp, 0.181_wp, 0.179_wp, 0.177_wp, 0.176_wp, 0.174_wp, 0.173_wp, 0.171_wp, 0.170_wp, 0.168_wp /
    DATA viridis_r(101:110) / 0.167_wp, 0.165_wp, 0.164_wp, 0.162_wp, 0.161_wp, 0.159_wp, 0.158_wp, 0.156_wp, 0.155_wp, 0.153_wp /
    DATA viridis_r(111:120) / 0.152_wp, 0.150_wp, 0.149_wp, 0.148_wp, 0.146_wp, 0.145_wp, 0.143_wp, 0.142_wp, 0.141_wp, 0.139_wp /
    DATA viridis_r(121:130) / 0.138_wp, 0.136_wp, 0.135_wp, 0.134_wp, 0.132_wp, 0.131_wp, 0.130_wp, 0.129_wp, 0.128_wp, 0.126_wp /
    DATA viridis_r(131:140) / 0.125_wp, 0.124_wp, 0.123_wp, 0.123_wp, 0.122_wp, 0.121_wp, 0.121_wp, 0.120_wp, 0.120_wp, 0.120_wp /
    DATA viridis_r(141:150) / 0.119_wp, 0.119_wp, 0.120_wp, 0.120_wp, 0.121_wp, 0.121_wp, 0.122_wp, 0.123_wp, 0.125_wp, 0.126_wp /
    DATA viridis_r(151:160) / 0.128_wp, 0.130_wp, 0.132_wp, 0.135_wp, 0.137_wp, 0.140_wp, 0.143_wp, 0.147_wp, 0.150_wp, 0.154_wp /
    DATA viridis_r(161:170) / 0.158_wp, 0.162_wp, 0.166_wp, 0.171_wp, 0.176_wp, 0.181_wp, 0.186_wp, 0.191_wp, 0.197_wp, 0.202_wp /
    DATA viridis_r(171:180) / 0.208_wp, 0.214_wp, 0.220_wp, 0.226_wp, 0.233_wp, 0.239_wp, 0.246_wp, 0.253_wp, 0.260_wp, 0.267_wp /
    DATA viridis_r(181:190) / 0.274_wp, 0.281_wp, 0.289_wp, 0.296_wp, 0.304_wp, 0.312_wp, 0.320_wp, 0.328_wp, 0.336_wp, 0.344_wp /
    DATA viridis_r(191:200) / 0.352_wp, 0.361_wp, 0.369_wp, 0.378_wp, 0.386_wp, 0.395_wp, 0.404_wp, 0.413_wp, 0.422_wp, 0.431_wp /
    DATA viridis_r(201:210) / 0.440_wp, 0.449_wp, 0.459_wp, 0.468_wp, 0.478_wp, 0.487_wp, 0.497_wp, 0.506_wp, 0.516_wp, 0.526_wp /
    DATA viridis_r(211:220) / 0.536_wp, 0.546_wp, 0.555_wp, 0.565_wp, 0.576_wp, 0.586_wp, 0.596_wp, 0.606_wp, 0.616_wp, 0.627_wp /
    DATA viridis_r(221:230) / 0.637_wp, 0.647_wp, 0.658_wp, 0.668_wp, 0.678_wp, 0.689_wp, 0.699_wp, 0.710_wp, 0.720_wp, 0.731_wp /
    DATA viridis_r(231:240) / 0.741_wp, 0.752_wp, 0.762_wp, 0.773_wp, 0.783_wp, 0.794_wp, 0.804_wp, 0.815_wp, 0.825_wp, 0.835_wp /
    DATA viridis_r(241:250) / 0.846_wp, 0.856_wp, 0.866_wp, 0.876_wp, 0.886_wp, 0.896_wp, 0.906_wp, 0.916_wp, 0.926_wp, 0.936_wp /
    DATA viridis_r(251:256) / 0.946_wp, 0.955_wp, 0.965_wp, 0.974_wp, 0.984_wp, 0.993_wp /
    DATA viridis_g(  1: 10) / 0.005_wp, 0.010_wp, 0.015_wp, 0.020_wp, 0.026_wp, 0.031_wp, 0.038_wp, 0.044_wp, 0.050_wp, 0.056_wp /
    DATA viridis_g( 11: 20) / 0.062_wp, 0.068_wp, 0.073_wp, 0.079_wp, 0.084_wp, 0.090_wp, 0.095_wp, 0.100_wp, 0.105_wp, 0.111_wp /
    DATA viridis_g( 21: 30) / 0.116_wp, 0.121_wp, 0.126_wp, 0.131_wp, 0.136_wp, 0.141_wp, 0.146_wp, 0.151_wp, 0.156_wp, 0.161_wp /
    DATA viridis_g( 31: 40) / 0.166_wp, 0.171_wp, 0.175_wp, 0.180_wp, 0.185_wp, 0.190_wp, 0.195_wp, 0.200_wp, 0.205_wp, 0.209_wp /
    DATA viridis_g( 41: 50) / 0.214_wp, 0.219_wp, 0.224_wp, 0.228_wp, 0.233_wp, 0.238_wp, 0.242_wp, 0.247_wp, 0.252_wp, 0.256_wp /
    DATA viridis_g( 51: 60) / 0.261_wp, 0.265_wp, 0.270_wp, 0.274_wp, 0.279_wp, 0.283_wp, 0.288_wp, 0.292_wp, 0.296_wp, 0.301_wp /
    DATA viridis_g( 61: 70) / 0.305_wp, 0.310_wp, 0.314_wp, 0.318_wp, 0.322_wp, 0.327_wp, 0.331_wp, 0.335_wp, 0.339_wp, 0.343_wp /
    DATA viridis_g( 71: 80) / 0.347_wp, 0.352_wp, 0.356_wp, 0.360_wp, 0.364_wp, 0.368_wp, 0.372_wp, 0.376_wp, 0.380_wp, 0.384_wp /
    DATA viridis_g( 81: 90) / 0.388_wp, 0.392_wp, 0.395_wp, 0.399_wp, 0.403_wp, 0.407_wp, 0.411_wp, 0.415_wp, 0.419_wp, 0.422_wp /
    DATA viridis_g( 91:100) / 0.426_wp, 0.430_wp, 0.434_wp, 0.438_wp, 0.441_wp, 0.445_wp, 0.449_wp, 0.453_wp, 0.456_wp, 0.460_wp /
    DATA viridis_g(101:110) / 0.464_wp, 0.467_wp, 0.471_wp, 0.475_wp, 0.479_wp, 0.482_wp, 0.486_wp, 0.490_wp, 0.493_wp, 0.497_wp /
    DATA viridis_g(111:120) / 0.501_wp, 0.504_wp, 0.508_wp, 0.512_wp, 0.515_wp, 0.519_wp, 0.523_wp, 0.526_wp, 0.530_wp, 0.534_wp /
    DATA viridis_g(121:130) / 0.537_wp, 0.541_wp, 0.545_wp, 0.549_wp, 0.552_wp, 0.556_wp, 0.560_wp, 0.563_wp, 0.567_wp, 0.571_wp /
    DATA viridis_g(131:140) / 0.574_wp, 0.578_wp, 0.582_wp, 0.585_wp, 0.589_wp, 0.593_wp, 0.596_wp, 0.600_wp, 0.604_wp, 0.607_wp /
    DATA viridis_g(141:150) / 0.611_wp, 0.615_wp, 0.618_wp, 0.622_wp, 0.626_wp, 0.629_wp, 0.633_wp, 0.637_wp, 0.640_wp, 0.644_wp /
    DATA viridis_g(151:160) / 0.648_wp, 0.651_wp, 0.655_wp, 0.659_wp, 0.662_wp, 0.666_wp, 0.669_wp, 0.673_wp, 0.677_wp, 0.680_wp /
    DATA viridis_g(161:170) / 0.684_wp, 0.687_wp, 0.691_wp, 0.694_wp, 0.698_wp, 0.701_wp, 0.705_wp, 0.708_wp, 0.712_wp, 0.715_wp /
    DATA viridis_g(171:180) / 0.719_wp, 0.722_wp, 0.726_wp, 0.729_wp, 0.732_wp, 0.736_wp, 0.739_wp, 0.742_wp, 0.745_wp, 0.749_wp /
    DATA viridis_g(181:190) / 0.752_wp, 0.755_wp, 0.758_wp, 0.762_wp, 0.765_wp, 0.768_wp, 0.771_wp, 0.774_wp, 0.777_wp, 0.780_wp /
    DATA viridis_g(191:200) / 0.783_wp, 0.786_wp, 0.789_wp, 0.792_wp, 0.795_wp, 0.797_wp, 0.800_wp, 0.803_wp, 0.806_wp, 0.808_wp /
    DATA viridis_g(201:210) / 0.811_wp, 0.814_wp, 0.816_wp, 0.819_wp, 0.821_wp, 0.824_wp, 0.826_wp, 0.829_wp, 0.831_wp, 0.833_wp /
    DATA viridis_g(211:220) / 0.836_wp, 0.838_wp, 0.840_wp, 0.842_wp, 0.845_wp, 0.847_wp, 0.849_wp, 0.851_wp, 0.853_wp, 0.855_wp /
    DATA viridis_g(221:230) / 0.857_wp, 0.858_wp, 0.860_wp, 0.862_wp, 0.864_wp, 0.865_wp, 0.867_wp, 0.869_wp, 0.870_wp, 0.872_wp /
    DATA viridis_g(231:240) / 0.873_wp, 0.875_wp, 0.876_wp, 0.878_wp, 0.879_wp, 0.881_wp, 0.882_wp, 0.883_wp, 0.885_wp, 0.886_wp /
    DATA viridis_g(241:250) / 0.887_wp, 0.889_wp, 0.890_wp, 0.891_wp, 0.892_wp, 0.894_wp, 0.895_wp, 0.896_wp, 0.897_wp, 0.899_wp /
    DATA viridis_g(251:256) / 0.900_wp, 0.901_wp, 0.902_wp, 0.904_wp, 0.905_wp, 0.906_wp /
    DATA viridis_b(  1: 10) / 0.329_wp, 0.335_wp, 0.341_wp, 0.347_wp, 0.353_wp, 0.359_wp, 0.365_wp, 0.370_wp, 0.376_wp, 0.381_wp /
    DATA viridis_b( 11: 20) / 0.387_wp, 0.392_wp, 0.397_wp, 0.402_wp, 0.407_wp, 0.412_wp, 0.417_wp, 0.422_wp, 0.427_wp, 0.432_wp /
    DATA viridis_b( 21: 30) / 0.436_wp, 0.441_wp, 0.445_wp, 0.449_wp, 0.453_wp, 0.458_wp, 0.462_wp, 0.465_wp, 0.469_wp, 0.473_wp /
    DATA viridis_b( 31: 40) / 0.476_wp, 0.480_wp, 0.483_wp, 0.487_wp, 0.490_wp, 0.493_wp, 0.496_wp, 0.499_wp, 0.502_wp, 0.504_wp /
    DATA viridis_b( 41: 50) / 0.507_wp, 0.510_wp, 0.512_wp, 0.514_wp, 0.517_wp, 0.519_wp, 0.521_wp, 0.523_wp, 0.525_wp, 0.527_wp /
    DATA viridis_b( 51: 60) / 0.528_wp, 0.530_wp, 0.532_wp, 0.533_wp, 0.535_wp, 0.536_wp, 0.537_wp, 0.539_wp, 0.540_wp, 0.541_wp /
    DATA viridis_b( 61: 70) / 0.542_wp, 0.543_wp, 0.544_wp, 0.545_wp, 0.546_wp, 0.547_wp, 0.547_wp, 0.548_wp, 0.549_wp, 0.549_wp /
    DATA viridis_b( 71: 80) / 0.550_wp, 0.551_wp, 0.551_wp, 0.552_wp, 0.552_wp, 0.553_wp, 0.553_wp, 0.554_wp, 0.554_wp, 0.554_wp /
    DATA viridis_b( 81: 90) / 0.555_wp, 0.555_wp, 0.555_wp, 0.556_wp, 0.556_wp, 0.556_wp, 0.556_wp, 0.557_wp, 0.557_wp, 0.557_wp /
    DATA viridis_b( 91:100) / 0.557_wp, 0.557_wp, 0.557_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp /
    DATA viridis_b(101:110) / 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp, 0.558_wp /
    DATA viridis_b(111:120) / 0.558_wp, 0.557_wp, 0.557_wp, 0.557_wp, 0.557_wp, 0.557_wp, 0.556_wp, 0.556_wp, 0.556_wp, 0.555_wp /
    DATA viridis_b(121:130) / 0.555_wp, 0.554_wp, 0.554_wp, 0.554_wp, 0.553_wp, 0.552_wp, 0.552_wp, 0.551_wp, 0.551_wp, 0.550_wp /
    DATA viridis_b(131:140) / 0.549_wp, 0.548_wp, 0.547_wp, 0.547_wp, 0.546_wp, 0.545_wp, 0.544_wp, 0.543_wp, 0.541_wp, 0.540_wp /
    DATA viridis_b(141:150) / 0.539_wp, 0.538_wp, 0.536_wp, 0.535_wp, 0.533_wp, 0.532_wp, 0.530_wp, 0.529_wp, 0.527_wp, 0.525_wp /
    DATA viridis_b(151:160) / 0.523_wp, 0.522_wp, 0.520_wp, 0.518_wp, 0.516_wp, 0.513_wp, 0.511_wp, 0.509_wp, 0.507_wp, 0.504_wp /
    DATA viridis_b(161:170) / 0.502_wp, 0.499_wp, 0.497_wp, 0.494_wp, 0.491_wp, 0.488_wp, 0.485_wp, 0.482_wp, 0.479_wp, 0.476_wp /
    DATA viridis_b(171:180) / 0.473_wp, 0.470_wp, 0.466_wp, 0.463_wp, 0.459_wp, 0.456_wp, 0.452_wp, 0.448_wp, 0.444_wp, 0.441_wp /
    DATA viridis_b(181:190) / 0.437_wp, 0.433_wp, 0.428_wp, 0.424_wp, 0.420_wp, 0.416_wp, 0.411_wp, 0.407_wp, 0.402_wp, 0.397_wp /
    DATA viridis_b(191:200) / 0.393_wp, 0.388_wp, 0.383_wp, 0.378_wp, 0.373_wp, 0.368_wp, 0.363_wp, 0.357_wp, 0.352_wp, 0.346_wp /
    DATA viridis_b(201:210) / 0.341_wp, 0.335_wp, 0.330_wp, 0.324_wp, 0.318_wp, 0.312_wp, 0.306_wp, 0.300_wp, 0.294_wp, 0.288_wp /
    DATA viridis_b(211:220) / 0.282_wp, 0.276_wp, 0.269_wp, 0.263_wp, 0.256_wp, 0.250_wp, 0.243_wp, 0.237_wp, 0.230_wp, 0.223_wp /
    DATA viridis_b(221:230) / 0.217_wp, 0.210_wp, 0.203_wp, 0.196_wp, 0.190_wp, 0.183_wp, 0.176_wp, 0.169_wp, 0.163_wp, 0.156_wp /
    DATA viridis_b(231:240) / 0.150_wp, 0.143_wp, 0.137_wp, 0.131_wp, 0.125_wp, 0.120_wp, 0.115_wp, 0.110_wp, 0.106_wp, 0.103_wp /
    DATA viridis_b(241:250) / 0.100_wp, 0.097_wp, 0.096_wp, 0.095_wp, 0.095_wp, 0.096_wp, 0.098_wp, 0.101_wp, 0.104_wp, 0.108_wp /
    DATA viridis_b(251:256) / 0.113_wp, 0.118_wp, 0.124_wp, 0.130_wp, 0.137_wp, 0.144_wp /
    ! 256-entry plasma LUT (matplotlib reference)
    real(wp), dimension(CMAP_N), private :: plasma_r
    real(wp), dimension(CMAP_N), private :: plasma_g
    real(wp), dimension(CMAP_N), private :: plasma_b
    DATA plasma_r(  1: 10) / 0.050_wp, 0.064_wp, 0.075_wp, 0.086_wp, 0.096_wp, 0.106_wp, 0.115_wp, 0.124_wp, 0.132_wp, 0.141_wp /
    DATA plasma_r( 11: 20) / 0.149_wp, 0.156_wp, 0.164_wp, 0.172_wp, 0.179_wp, 0.186_wp, 0.193_wp, 0.200_wp, 0.207_wp, 0.214_wp /
    DATA plasma_r( 21: 30) / 0.221_wp, 0.228_wp, 0.235_wp, 0.241_wp, 0.248_wp, 0.255_wp, 0.261_wp, 0.268_wp, 0.274_wp, 0.281_wp /
    DATA plasma_r( 31: 40) / 0.287_wp, 0.293_wp, 0.300_wp, 0.306_wp, 0.313_wp, 0.319_wp, 0.325_wp, 0.331_wp, 0.338_wp, 0.344_wp /
    DATA plasma_r( 41: 50) / 0.350_wp, 0.356_wp, 0.363_wp, 0.369_wp, 0.375_wp, 0.381_wp, 0.387_wp, 0.393_wp, 0.399_wp, 0.406_wp /
    DATA plasma_r( 51: 60) / 0.412_wp, 0.418_wp, 0.424_wp, 0.430_wp, 0.436_wp, 0.442_wp, 0.448_wp, 0.454_wp, 0.460_wp, 0.466_wp /
    DATA plasma_r( 61: 70) / 0.471_wp, 0.477_wp, 0.483_wp, 0.489_wp, 0.495_wp, 0.501_wp, 0.506_wp, 0.512_wp, 0.518_wp, 0.524_wp /
    DATA plasma_r( 71: 80) / 0.529_wp, 0.535_wp, 0.541_wp, 0.546_wp, 0.552_wp, 0.557_wp, 0.563_wp, 0.568_wp, 0.574_wp, 0.579_wp /
    DATA plasma_r( 81: 90) / 0.584_wp, 0.590_wp, 0.595_wp, 0.600_wp, 0.605_wp, 0.611_wp, 0.616_wp, 0.621_wp, 0.626_wp, 0.631_wp /
    DATA plasma_r( 91:100) / 0.636_wp, 0.641_wp, 0.646_wp, 0.651_wp, 0.656_wp, 0.660_wp, 0.665_wp, 0.670_wp, 0.675_wp, 0.679_wp /
    DATA plasma_r(101:110) / 0.684_wp, 0.688_wp, 0.693_wp, 0.697_wp, 0.702_wp, 0.706_wp, 0.711_wp, 0.715_wp, 0.719_wp, 0.723_wp /
    DATA plasma_r(111:120) / 0.728_wp, 0.732_wp, 0.736_wp, 0.740_wp, 0.744_wp, 0.748_wp, 0.752_wp, 0.756_wp, 0.760_wp, 0.764_wp /
    DATA plasma_r(121:130) / 0.768_wp, 0.772_wp, 0.776_wp, 0.780_wp, 0.783_wp, 0.787_wp, 0.791_wp, 0.795_wp, 0.798_wp, 0.802_wp /
    DATA plasma_r(131:140) / 0.805_wp, 0.809_wp, 0.813_wp, 0.816_wp, 0.820_wp, 0.823_wp, 0.827_wp, 0.830_wp, 0.833_wp, 0.837_wp /
    DATA plasma_r(141:150) / 0.840_wp, 0.843_wp, 0.847_wp, 0.850_wp, 0.853_wp, 0.857_wp, 0.860_wp, 0.863_wp, 0.866_wp, 0.869_wp /
    DATA plasma_r(151:160) / 0.872_wp, 0.875_wp, 0.878_wp, 0.881_wp, 0.884_wp, 0.887_wp, 0.890_wp, 0.893_wp, 0.896_wp, 0.899_wp /
    DATA plasma_r(161:170) / 0.902_wp, 0.905_wp, 0.907_wp, 0.910_wp, 0.913_wp, 0.915_wp, 0.918_wp, 0.921_wp, 0.923_wp, 0.926_wp /
    DATA plasma_r(171:180) / 0.928_wp, 0.931_wp, 0.933_wp, 0.936_wp, 0.938_wp, 0.940_wp, 0.943_wp, 0.945_wp, 0.947_wp, 0.949_wp /
    DATA plasma_r(181:190) / 0.951_wp, 0.953_wp, 0.955_wp, 0.957_wp, 0.959_wp, 0.961_wp, 0.963_wp, 0.965_wp, 0.967_wp, 0.969_wp /
    DATA plasma_r(191:200) / 0.970_wp, 0.972_wp, 0.973_wp, 0.975_wp, 0.976_wp, 0.978_wp, 0.979_wp, 0.981_wp, 0.982_wp, 0.983_wp /
    DATA plasma_r(201:210) / 0.984_wp, 0.985_wp, 0.986_wp, 0.987_wp, 0.988_wp, 0.989_wp, 0.990_wp, 0.991_wp, 0.991_wp, 0.992_wp /
    DATA plasma_r(211:220) / 0.993_wp, 0.993_wp, 0.993_wp, 0.994_wp, 0.994_wp, 0.994_wp, 0.994_wp, 0.995_wp, 0.995_wp, 0.994_wp /
    DATA plasma_r(221:230) / 0.994_wp, 0.994_wp, 0.994_wp, 0.993_wp, 0.993_wp, 0.993_wp, 0.992_wp, 0.991_wp, 0.990_wp, 0.990_wp /
    DATA plasma_r(231:240) / 0.989_wp, 0.988_wp, 0.987_wp, 0.985_wp, 0.984_wp, 0.983_wp, 0.981_wp, 0.980_wp, 0.978_wp, 0.976_wp /
    DATA plasma_r(241:250) / 0.974_wp, 0.973_wp, 0.971_wp, 0.968_wp, 0.966_wp, 0.964_wp, 0.962_wp, 0.959_wp, 0.957_wp, 0.954_wp /
    DATA plasma_r(251:256) / 0.952_wp, 0.949_wp, 0.947_wp, 0.944_wp, 0.942_wp, 0.940_wp /
    DATA plasma_g(  1: 10) / 0.030_wp, 0.028_wp, 0.027_wp, 0.026_wp, 0.025_wp, 0.024_wp, 0.024_wp, 0.023_wp, 0.022_wp, 0.022_wp /
    DATA plasma_g( 11: 20) / 0.021_wp, 0.021_wp, 0.020_wp, 0.020_wp, 0.019_wp, 0.019_wp, 0.018_wp, 0.018_wp, 0.017_wp, 0.017_wp /
    DATA plasma_g( 21: 30) / 0.016_wp, 0.016_wp, 0.016_wp, 0.015_wp, 0.014_wp, 0.014_wp, 0.013_wp, 0.013_wp, 0.012_wp, 0.011_wp /
    DATA plasma_g( 31: 40) / 0.011_wp, 0.010_wp, 0.010_wp, 0.009_wp, 0.008_wp, 0.008_wp, 0.007_wp, 0.006_wp, 0.006_wp, 0.005_wp /
    DATA plasma_g( 41: 50) / 0.004_wp, 0.004_wp, 0.003_wp, 0.003_wp, 0.002_wp, 0.002_wp, 0.001_wp, 0.001_wp, 0.001_wp, 0.001_wp /
    DATA plasma_g( 51: 60) / 0.001_wp, 0.001_wp, 0.001_wp, 0.001_wp, 0.001_wp, 0.002_wp, 0.002_wp, 0.003_wp, 0.004_wp, 0.005_wp /
    DATA plasma_g( 61: 70) / 0.006_wp, 0.007_wp, 0.008_wp, 0.010_wp, 0.012_wp, 0.014_wp, 0.016_wp, 0.019_wp, 0.022_wp, 0.025_wp /
    DATA plasma_g( 71: 80) / 0.028_wp, 0.031_wp, 0.035_wp, 0.039_wp, 0.043_wp, 0.047_wp, 0.052_wp, 0.056_wp, 0.060_wp, 0.064_wp /
    DATA plasma_g( 81: 90) / 0.069_wp, 0.073_wp, 0.077_wp, 0.082_wp, 0.086_wp, 0.090_wp, 0.095_wp, 0.099_wp, 0.103_wp, 0.108_wp /
    DATA plasma_g( 91:100) / 0.112_wp, 0.116_wp, 0.121_wp, 0.125_wp, 0.130_wp, 0.134_wp, 0.139_wp, 0.143_wp, 0.147_wp, 0.152_wp /
    DATA plasma_g(101:110) / 0.156_wp, 0.161_wp, 0.165_wp, 0.170_wp, 0.174_wp, 0.178_wp, 0.183_wp, 0.187_wp, 0.192_wp, 0.196_wp /
    DATA plasma_g(111:120) / 0.201_wp, 0.205_wp, 0.209_wp, 0.214_wp, 0.218_wp, 0.223_wp, 0.227_wp, 0.232_wp, 0.236_wp, 0.240_wp /
    DATA plasma_g(121:130) / 0.245_wp, 0.249_wp, 0.254_wp, 0.258_wp, 0.263_wp, 0.267_wp, 0.271_wp, 0.276_wp, 0.280_wp, 0.285_wp /
    DATA plasma_g(131:140) / 0.289_wp, 0.293_wp, 0.298_wp, 0.302_wp, 0.307_wp, 0.311_wp, 0.316_wp, 0.320_wp, 0.325_wp, 0.329_wp /
    DATA plasma_g(141:150) / 0.334_wp, 0.338_wp, 0.343_wp, 0.347_wp, 0.352_wp, 0.356_wp, 0.361_wp, 0.365_wp, 0.370_wp, 0.374_wp /
    DATA plasma_g(151:160) / 0.379_wp, 0.383_wp, 0.388_wp, 0.393_wp, 0.397_wp, 0.402_wp, 0.406_wp, 0.411_wp, 0.416_wp, 0.420_wp /
    DATA plasma_g(161:170) / 0.425_wp, 0.430_wp, 0.435_wp, 0.439_wp, 0.444_wp, 0.449_wp, 0.454_wp, 0.458_wp, 0.463_wp, 0.468_wp /
    DATA plasma_g(171:180) / 0.473_wp, 0.478_wp, 0.483_wp, 0.488_wp, 0.493_wp, 0.498_wp, 0.503_wp, 0.508_wp, 0.513_wp, 0.518_wp /
    DATA plasma_g(181:190) / 0.523_wp, 0.528_wp, 0.533_wp, 0.538_wp, 0.543_wp, 0.549_wp, 0.554_wp, 0.559_wp, 0.564_wp, 0.570_wp /
    DATA plasma_g(191:200) / 0.575_wp, 0.580_wp, 0.586_wp, 0.591_wp, 0.597_wp, 0.602_wp, 0.608_wp, 0.613_wp, 0.619_wp, 0.624_wp /
    DATA plasma_g(201:210) / 0.630_wp, 0.635_wp, 0.641_wp, 0.647_wp, 0.652_wp, 0.658_wp, 0.664_wp, 0.670_wp, 0.675_wp, 0.681_wp /
    DATA plasma_g(211:220) / 0.687_wp, 0.693_wp, 0.699_wp, 0.705_wp, 0.711_wp, 0.717_wp, 0.723_wp, 0.729_wp, 0.735_wp, 0.741_wp /
    DATA plasma_g(221:230) / 0.747_wp, 0.753_wp, 0.759_wp, 0.765_wp, 0.772_wp, 0.778_wp, 0.784_wp, 0.791_wp, 0.797_wp, 0.803_wp /
    DATA plasma_g(231:240) / 0.810_wp, 0.816_wp, 0.822_wp, 0.829_wp, 0.835_wp, 0.842_wp, 0.848_wp, 0.855_wp, 0.861_wp, 0.868_wp /
    DATA plasma_g(241:250) / 0.875_wp, 0.881_wp, 0.888_wp, 0.895_wp, 0.901_wp, 0.908_wp, 0.915_wp, 0.921_wp, 0.928_wp, 0.935_wp /
    DATA plasma_g(251:256) / 0.942_wp, 0.948_wp, 0.955_wp, 0.962_wp, 0.969_wp, 0.975_wp /
    DATA plasma_b(  1: 10) / 0.528_wp, 0.533_wp, 0.538_wp, 0.543_wp, 0.547_wp, 0.551_wp, 0.555_wp, 0.559_wp, 0.563_wp, 0.567_wp /
    DATA plasma_b( 11: 20) / 0.571_wp, 0.574_wp, 0.577_wp, 0.581_wp, 0.584_wp, 0.587_wp, 0.590_wp, 0.593_wp, 0.596_wp, 0.599_wp /
    DATA plasma_b( 21: 30) / 0.602_wp, 0.605_wp, 0.608_wp, 0.610_wp, 0.613_wp, 0.615_wp, 0.618_wp, 0.620_wp, 0.623_wp, 0.625_wp /
    DATA plasma_b( 31: 40) / 0.627_wp, 0.629_wp, 0.632_wp, 0.634_wp, 0.636_wp, 0.638_wp, 0.640_wp, 0.641_wp, 0.643_wp, 0.645_wp /
    DATA plasma_b( 41: 50) / 0.646_wp, 0.648_wp, 0.649_wp, 0.651_wp, 0.652_wp, 0.653_wp, 0.654_wp, 0.655_wp, 0.656_wp, 0.657_wp /
    DATA plasma_b( 51: 60) / 0.658_wp, 0.658_wp, 0.659_wp, 0.659_wp, 0.660_wp, 0.660_wp, 0.660_wp, 0.660_wp, 0.660_wp, 0.660_wp /
    DATA plasma_b( 61: 70) / 0.660_wp, 0.660_wp, 0.659_wp, 0.659_wp, 0.658_wp, 0.657_wp, 0.656_wp, 0.655_wp, 0.654_wp, 0.653_wp /
    DATA plasma_b( 71: 80) / 0.652_wp, 0.650_wp, 0.649_wp, 0.647_wp, 0.645_wp, 0.643_wp, 0.642_wp, 0.639_wp, 0.637_wp, 0.635_wp /
    DATA plasma_b( 81: 90) / 0.633_wp, 0.630_wp, 0.628_wp, 0.625_wp, 0.623_wp, 0.620_wp, 0.617_wp, 0.614_wp, 0.611_wp, 0.608_wp /
    DATA plasma_b( 91:100) / 0.605_wp, 0.602_wp, 0.599_wp, 0.596_wp, 0.592_wp, 0.589_wp, 0.586_wp, 0.582_wp, 0.579_wp, 0.575_wp /
    DATA plasma_b(101:110) / 0.572_wp, 0.568_wp, 0.565_wp, 0.561_wp, 0.557_wp, 0.554_wp, 0.550_wp, 0.546_wp, 0.543_wp, 0.539_wp /
    DATA plasma_b(111:120) / 0.535_wp, 0.532_wp, 0.528_wp, 0.524_wp, 0.521_wp, 0.517_wp, 0.513_wp, 0.509_wp, 0.506_wp, 0.502_wp /
    DATA plasma_b(121:130) / 0.498_wp, 0.495_wp, 0.491_wp, 0.488_wp, 0.484_wp, 0.480_wp, 0.477_wp, 0.473_wp, 0.470_wp, 0.466_wp /
    DATA plasma_b(131:140) / 0.462_wp, 0.459_wp, 0.455_wp, 0.452_wp, 0.448_wp, 0.445_wp, 0.441_wp, 0.438_wp, 0.434_wp, 0.431_wp /
    DATA plasma_b(141:150) / 0.427_wp, 0.424_wp, 0.421_wp, 0.417_wp, 0.414_wp, 0.410_wp, 0.407_wp, 0.404_wp, 0.400_wp, 0.397_wp /
    DATA plasma_b(151:160) / 0.393_wp, 0.390_wp, 0.387_wp, 0.383_wp, 0.380_wp, 0.376_wp, 0.373_wp, 0.370_wp, 0.366_wp, 0.363_wp /
    DATA plasma_b(161:170) / 0.360_wp, 0.356_wp, 0.353_wp, 0.350_wp, 0.346_wp, 0.343_wp, 0.340_wp, 0.336_wp, 0.333_wp, 0.329_wp /
    DATA plasma_b(171:180) / 0.326_wp, 0.323_wp, 0.319_wp, 0.316_wp, 0.313_wp, 0.309_wp, 0.306_wp, 0.302_wp, 0.299_wp, 0.296_wp /
    DATA plasma_b(181:190) / 0.292_wp, 0.289_wp, 0.285_wp, 0.282_wp, 0.279_wp, 0.275_wp, 0.272_wp, 0.269_wp, 0.265_wp, 0.262_wp /
    DATA plasma_b(191:200) / 0.258_wp, 0.255_wp, 0.252_wp, 0.248_wp, 0.245_wp, 0.241_wp, 0.238_wp, 0.235_wp, 0.231_wp, 0.228_wp /
    DATA plasma_b(201:210) / 0.225_wp, 0.221_wp, 0.218_wp, 0.215_wp, 0.211_wp, 0.208_wp, 0.205_wp, 0.202_wp, 0.198_wp, 0.195_wp /
    DATA plasma_b(211:220) / 0.192_wp, 0.189_wp, 0.186_wp, 0.183_wp, 0.180_wp, 0.177_wp, 0.174_wp, 0.172_wp, 0.169_wp, 0.166_wp /
    DATA plasma_b(221:230) / 0.164_wp, 0.161_wp, 0.159_wp, 0.157_wp, 0.155_wp, 0.153_wp, 0.151_wp, 0.149_wp, 0.148_wp, 0.147_wp /
    DATA plasma_b(231:240) / 0.145_wp, 0.144_wp, 0.144_wp, 0.143_wp, 0.143_wp, 0.142_wp, 0.142_wp, 0.142_wp, 0.143_wp, 0.143_wp /
    DATA plasma_b(241:250) / 0.144_wp, 0.145_wp, 0.146_wp, 0.147_wp, 0.148_wp, 0.149_wp, 0.151_wp, 0.152_wp, 0.152_wp, 0.153_wp /
    DATA plasma_b(251:256) / 0.153_wp, 0.152_wp, 0.150_wp, 0.147_wp, 0.141_wp, 0.131_wp /
    ! 256-entry inferno LUT (matplotlib reference)
    real(wp), dimension(CMAP_N), private :: inferno_r
    real(wp), dimension(CMAP_N), private :: inferno_g
    real(wp), dimension(CMAP_N), private :: inferno_b
    DATA inferno_r(  1: 10) / 0.001_wp, 0.002_wp, 0.003_wp, 0.005_wp, 0.006_wp, 0.008_wp, 0.010_wp, 0.012_wp, 0.014_wp, 0.017_wp /
    DATA inferno_r( 11: 20) / 0.019_wp, 0.022_wp, 0.026_wp, 0.029_wp, 0.033_wp, 0.038_wp, 0.042_wp, 0.047_wp, 0.052_wp, 0.056_wp /
    DATA inferno_r( 21: 30) / 0.061_wp, 0.066_wp, 0.071_wp, 0.077_wp, 0.082_wp, 0.087_wp, 0.093_wp, 0.099_wp, 0.105_wp, 0.111_wp /
    DATA inferno_r( 31: 40) / 0.117_wp, 0.123_wp, 0.129_wp, 0.136_wp, 0.142_wp, 0.149_wp, 0.156_wp, 0.163_wp, 0.170_wp, 0.176_wp /
    DATA inferno_r( 41: 50) / 0.183_wp, 0.190_wp, 0.197_wp, 0.204_wp, 0.211_wp, 0.218_wp, 0.225_wp, 0.232_wp, 0.238_wp, 0.245_wp /
    DATA inferno_r( 51: 60) / 0.252_wp, 0.258_wp, 0.265_wp, 0.271_wp, 0.278_wp, 0.284_wp, 0.291_wp, 0.297_wp, 0.304_wp, 0.310_wp /
    DATA inferno_r( 61: 70) / 0.316_wp, 0.323_wp, 0.329_wp, 0.335_wp, 0.342_wp, 0.348_wp, 0.354_wp, 0.360_wp, 0.367_wp, 0.373_wp /
    DATA inferno_r( 71: 80) / 0.379_wp, 0.385_wp, 0.391_wp, 0.398_wp, 0.404_wp, 0.410_wp, 0.416_wp, 0.423_wp, 0.429_wp, 0.435_wp /
    DATA inferno_r( 81: 90) / 0.441_wp, 0.447_wp, 0.454_wp, 0.460_wp, 0.466_wp, 0.472_wp, 0.479_wp, 0.485_wp, 0.491_wp, 0.497_wp /
    DATA inferno_r( 91:100) / 0.503_wp, 0.510_wp, 0.516_wp, 0.522_wp, 0.528_wp, 0.535_wp, 0.541_wp, 0.547_wp, 0.553_wp, 0.560_wp /
    DATA inferno_r(101:110) / 0.566_wp, 0.572_wp, 0.578_wp, 0.585_wp, 0.591_wp, 0.597_wp, 0.603_wp, 0.609_wp, 0.616_wp, 0.622_wp /
    DATA inferno_r(111:120) / 0.628_wp, 0.634_wp, 0.640_wp, 0.646_wp, 0.652_wp, 0.658_wp, 0.665_wp, 0.671_wp, 0.677_wp, 0.683_wp /
    DATA inferno_r(121:130) / 0.689_wp, 0.695_wp, 0.701_wp, 0.707_wp, 0.712_wp, 0.718_wp, 0.724_wp, 0.730_wp, 0.736_wp, 0.741_wp /
    DATA inferno_r(131:140) / 0.747_wp, 0.753_wp, 0.758_wp, 0.764_wp, 0.770_wp, 0.775_wp, 0.781_wp, 0.786_wp, 0.791_wp, 0.797_wp /
    DATA inferno_r(141:150) / 0.802_wp, 0.807_wp, 0.812_wp, 0.817_wp, 0.822_wp, 0.827_wp, 0.832_wp, 0.837_wp, 0.842_wp, 0.847_wp /
    DATA inferno_r(151:160) / 0.851_wp, 0.856_wp, 0.861_wp, 0.865_wp, 0.869_wp, 0.874_wp, 0.878_wp, 0.882_wp, 0.886_wp, 0.890_wp /
    DATA inferno_r(161:170) / 0.894_wp, 0.898_wp, 0.902_wp, 0.906_wp, 0.909_wp, 0.913_wp, 0.916_wp, 0.920_wp, 0.923_wp, 0.926_wp /
    DATA inferno_r(171:180) / 0.930_wp, 0.933_wp, 0.936_wp, 0.939_wp, 0.942_wp, 0.944_wp, 0.947_wp, 0.950_wp, 0.952_wp, 0.955_wp /
    DATA inferno_r(181:190) / 0.957_wp, 0.959_wp, 0.961_wp, 0.963_wp, 0.965_wp, 0.967_wp, 0.969_wp, 0.971_wp, 0.973_wp, 0.974_wp /
    DATA inferno_r(191:200) / 0.976_wp, 0.977_wp, 0.978_wp, 0.980_wp, 0.981_wp, 0.982_wp, 0.983_wp, 0.984_wp, 0.985_wp, 0.985_wp /
    DATA inferno_r(201:210) / 0.986_wp, 0.987_wp, 0.987_wp, 0.987_wp, 0.988_wp, 0.988_wp, 0.988_wp, 0.988_wp, 0.988_wp, 0.988_wp /
    DATA inferno_r(211:220) / 0.987_wp, 0.987_wp, 0.987_wp, 0.986_wp, 0.986_wp, 0.985_wp, 0.984_wp, 0.983_wp, 0.982_wp, 0.981_wp /
    DATA inferno_r(221:230) / 0.980_wp, 0.979_wp, 0.977_wp, 0.976_wp, 0.975_wp, 0.973_wp, 0.971_wp, 0.970_wp, 0.968_wp, 0.966_wp /
    DATA inferno_r(231:240) / 0.964_wp, 0.963_wp, 0.961_wp, 0.959_wp, 0.957_wp, 0.955_wp, 0.953_wp, 0.952_wp, 0.950_wp, 0.949_wp /
    DATA inferno_r(241:250) / 0.948_wp, 0.947_wp, 0.946_wp, 0.946_wp, 0.947_wp, 0.948_wp, 0.950_wp, 0.952_wp, 0.955_wp, 0.958_wp /
    DATA inferno_r(251:256) / 0.962_wp, 0.966_wp, 0.971_wp, 0.977_wp, 0.982_wp, 0.988_wp /
    DATA inferno_g(  1: 10) / 0.000_wp, 0.001_wp, 0.002_wp, 0.003_wp, 0.005_wp, 0.006_wp, 0.008_wp, 0.009_wp, 0.011_wp, 0.013_wp /
    DATA inferno_g( 11: 20) / 0.015_wp, 0.017_wp, 0.019_wp, 0.022_wp, 0.024_wp, 0.026_wp, 0.028_wp, 0.030_wp, 0.032_wp, 0.035_wp /
    DATA inferno_g( 21: 30) / 0.037_wp, 0.039_wp, 0.040_wp, 0.042_wp, 0.043_wp, 0.045_wp, 0.046_wp, 0.046_wp, 0.047_wp, 0.047_wp /
    DATA inferno_g( 31: 40) / 0.048_wp, 0.048_wp, 0.047_wp, 0.047_wp, 0.046_wp, 0.045_wp, 0.045_wp, 0.044_wp, 0.042_wp, 0.041_wp /
    DATA inferno_g( 41: 50) / 0.040_wp, 0.039_wp, 0.038_wp, 0.038_wp, 0.037_wp, 0.037_wp, 0.036_wp, 0.036_wp, 0.037_wp, 0.037_wp /
    DATA inferno_g( 51: 60) / 0.038_wp, 0.039_wp, 0.040_wp, 0.041_wp, 0.042_wp, 0.044_wp, 0.046_wp, 0.047_wp, 0.049_wp, 0.051_wp /
    DATA inferno_g( 61: 70) / 0.053_wp, 0.056_wp, 0.058_wp, 0.060_wp, 0.062_wp, 0.065_wp, 0.067_wp, 0.069_wp, 0.072_wp, 0.074_wp /
    DATA inferno_g( 71: 80) / 0.076_wp, 0.079_wp, 0.081_wp, 0.083_wp, 0.086_wp, 0.088_wp, 0.090_wp, 0.093_wp, 0.095_wp, 0.097_wp /
    DATA inferno_g( 81: 90) / 0.099_wp, 0.102_wp, 0.104_wp, 0.106_wp, 0.108_wp, 0.111_wp, 0.113_wp, 0.115_wp, 0.117_wp, 0.119_wp /
    DATA inferno_g( 91:100) / 0.122_wp, 0.124_wp, 0.126_wp, 0.128_wp, 0.130_wp, 0.133_wp, 0.135_wp, 0.137_wp, 0.139_wp, 0.141_wp /
    DATA inferno_g(101:110) / 0.144_wp, 0.146_wp, 0.148_wp, 0.150_wp, 0.153_wp, 0.155_wp, 0.157_wp, 0.159_wp, 0.162_wp, 0.164_wp /
    DATA inferno_g(111:120) / 0.167_wp, 0.169_wp, 0.171_wp, 0.174_wp, 0.176_wp, 0.179_wp, 0.182_wp, 0.184_wp, 0.187_wp, 0.190_wp /
    DATA inferno_g(121:130) / 0.192_wp, 0.195_wp, 0.198_wp, 0.201_wp, 0.204_wp, 0.207_wp, 0.210_wp, 0.213_wp, 0.216_wp, 0.219_wp /
    DATA inferno_g(131:140) / 0.222_wp, 0.226_wp, 0.229_wp, 0.233_wp, 0.236_wp, 0.240_wp, 0.243_wp, 0.247_wp, 0.251_wp, 0.255_wp /
    DATA inferno_g(141:150) / 0.259_wp, 0.263_wp, 0.267_wp, 0.271_wp, 0.275_wp, 0.280_wp, 0.284_wp, 0.288_wp, 0.293_wp, 0.298_wp /
    DATA inferno_g(151:160) / 0.302_wp, 0.307_wp, 0.312_wp, 0.317_wp, 0.322_wp, 0.327_wp, 0.332_wp, 0.337_wp, 0.343_wp, 0.348_wp /
    DATA inferno_g(161:170) / 0.353_wp, 0.359_wp, 0.364_wp, 0.370_wp, 0.376_wp, 0.382_wp, 0.387_wp, 0.393_wp, 0.399_wp, 0.405_wp /
    DATA inferno_g(171:180) / 0.411_wp, 0.418_wp, 0.424_wp, 0.430_wp, 0.436_wp, 0.443_wp, 0.449_wp, 0.456_wp, 0.462_wp, 0.469_wp /
    DATA inferno_g(181:190) / 0.475_wp, 0.482_wp, 0.489_wp, 0.495_wp, 0.502_wp, 0.509_wp, 0.516_wp, 0.523_wp, 0.530_wp, 0.537_wp /
    DATA inferno_g(191:200) / 0.544_wp, 0.551_wp, 0.558_wp, 0.565_wp, 0.572_wp, 0.579_wp, 0.587_wp, 0.594_wp, 0.601_wp, 0.608_wp /
    DATA inferno_g(201:210) / 0.616_wp, 0.623_wp, 0.630_wp, 0.638_wp, 0.645_wp, 0.653_wp, 0.660_wp, 0.668_wp, 0.675_wp, 0.683_wp /
    DATA inferno_g(211:220) / 0.690_wp, 0.698_wp, 0.706_wp, 0.713_wp, 0.721_wp, 0.728_wp, 0.736_wp, 0.744_wp, 0.751_wp, 0.759_wp /
    DATA inferno_g(221:230) / 0.767_wp, 0.775_wp, 0.782_wp, 0.790_wp, 0.798_wp, 0.805_wp, 0.813_wp, 0.821_wp, 0.829_wp, 0.836_wp /
    DATA inferno_g(231:240) / 0.844_wp, 0.851_wp, 0.859_wp, 0.867_wp, 0.874_wp, 0.882_wp, 0.889_wp, 0.896_wp, 0.903_wp, 0.910_wp /
    DATA inferno_g(241:250) / 0.917_wp, 0.924_wp, 0.931_wp, 0.937_wp, 0.943_wp, 0.949_wp, 0.955_wp, 0.961_wp, 0.966_wp, 0.971_wp /
    DATA inferno_g(251:256) / 0.976_wp, 0.981_wp, 0.985_wp, 0.990_wp, 0.994_wp, 0.998_wp /
    DATA inferno_b(  1: 10) / 0.014_wp, 0.019_wp, 0.024_wp, 0.031_wp, 0.039_wp, 0.047_wp, 0.055_wp, 0.063_wp, 0.072_wp, 0.080_wp /
    DATA inferno_b( 11: 20) / 0.089_wp, 0.097_wp, 0.106_wp, 0.115_wp, 0.123_wp, 0.132_wp, 0.141_wp, 0.150_wp, 0.159_wp, 0.168_wp /
    DATA inferno_b( 21: 30) / 0.178_wp, 0.187_wp, 0.196_wp, 0.206_wp, 0.215_wp, 0.225_wp, 0.234_wp, 0.244_wp, 0.253_wp, 0.263_wp /
    DATA inferno_b( 31: 40) / 0.272_wp, 0.282_wp, 0.291_wp, 0.300_wp, 0.309_wp, 0.317_wp, 0.325_wp, 0.333_wp, 0.341_wp, 0.348_wp /
    DATA inferno_b( 41: 50) / 0.355_wp, 0.361_wp, 0.368_wp, 0.373_wp, 0.379_wp, 0.384_wp, 0.388_wp, 0.392_wp, 0.396_wp, 0.400_wp /
    DATA inferno_b( 51: 60) / 0.403_wp, 0.406_wp, 0.409_wp, 0.412_wp, 0.414_wp, 0.417_wp, 0.419_wp, 0.420_wp, 0.422_wp, 0.424_wp /
    DATA inferno_b( 61: 70) / 0.425_wp, 0.426_wp, 0.428_wp, 0.429_wp, 0.429_wp, 0.430_wp, 0.431_wp, 0.431_wp, 0.432_wp, 0.432_wp /
    DATA inferno_b( 71: 80) / 0.433_wp, 0.433_wp, 0.433_wp, 0.433_wp, 0.433_wp, 0.433_wp, 0.433_wp, 0.433_wp, 0.432_wp, 0.432_wp /
    DATA inferno_b( 81: 90) / 0.432_wp, 0.431_wp, 0.430_wp, 0.430_wp, 0.429_wp, 0.428_wp, 0.427_wp, 0.427_wp, 0.426_wp, 0.424_wp /
    DATA inferno_b( 91:100) / 0.423_wp, 0.422_wp, 0.421_wp, 0.420_wp, 0.418_wp, 0.417_wp, 0.415_wp, 0.414_wp, 0.412_wp, 0.410_wp /
    DATA inferno_b(101:110) / 0.408_wp, 0.406_wp, 0.404_wp, 0.402_wp, 0.400_wp, 0.398_wp, 0.396_wp, 0.394_wp, 0.391_wp, 0.389_wp /
    DATA inferno_b(111:120) / 0.386_wp, 0.384_wp, 0.381_wp, 0.378_wp, 0.376_wp, 0.373_wp, 0.370_wp, 0.367_wp, 0.364_wp, 0.361_wp /
    DATA inferno_b(121:130) / 0.358_wp, 0.354_wp, 0.351_wp, 0.348_wp, 0.344_wp, 0.341_wp, 0.337_wp, 0.334_wp, 0.330_wp, 0.327_wp /
    DATA inferno_b(131:140) / 0.323_wp, 0.319_wp, 0.315_wp, 0.311_wp, 0.307_wp, 0.304_wp, 0.300_wp, 0.295_wp, 0.291_wp, 0.287_wp /
    DATA inferno_b(141:150) / 0.283_wp, 0.279_wp, 0.275_wp, 0.270_wp, 0.266_wp, 0.262_wp, 0.257_wp, 0.253_wp, 0.249_wp, 0.244_wp /
    DATA inferno_b(151:160) / 0.240_wp, 0.235_wp, 0.231_wp, 0.226_wp, 0.221_wp, 0.217_wp, 0.212_wp, 0.208_wp, 0.203_wp, 0.198_wp /
    DATA inferno_b(161:170) / 0.194_wp, 0.189_wp, 0.184_wp, 0.179_wp, 0.175_wp, 0.170_wp, 0.165_wp, 0.160_wp, 0.155_wp, 0.150_wp /
    DATA inferno_b(171:180) / 0.145_wp, 0.140_wp, 0.135_wp, 0.130_wp, 0.125_wp, 0.120_wp, 0.115_wp, 0.110_wp, 0.105_wp, 0.100_wp /
    DATA inferno_b(181:190) / 0.095_wp, 0.089_wp, 0.084_wp, 0.079_wp, 0.074_wp, 0.069_wp, 0.063_wp, 0.058_wp, 0.053_wp, 0.048_wp /
    DATA inferno_b(191:200) / 0.044_wp, 0.039_wp, 0.035_wp, 0.031_wp, 0.029_wp, 0.026_wp, 0.025_wp, 0.024_wp, 0.024_wp, 0.024_wp /
    DATA inferno_b(201:210) / 0.026_wp, 0.028_wp, 0.031_wp, 0.035_wp, 0.040_wp, 0.046_wp, 0.052_wp, 0.058_wp, 0.065_wp, 0.072_wp /
    DATA inferno_b(211:220) / 0.080_wp, 0.088_wp, 0.096_wp, 0.104_wp, 0.112_wp, 0.121_wp, 0.130_wp, 0.138_wp, 0.148_wp, 0.157_wp /
    DATA inferno_b(221:230) / 0.166_wp, 0.176_wp, 0.186_wp, 0.196_wp, 0.206_wp, 0.217_wp, 0.228_wp, 0.239_wp, 0.250_wp, 0.262_wp /
    DATA inferno_b(231:240) / 0.273_wp, 0.286_wp, 0.298_wp, 0.311_wp, 0.324_wp, 0.337_wp, 0.351_wp, 0.366_wp, 0.380_wp, 0.395_wp /
    DATA inferno_b(241:250) / 0.411_wp, 0.426_wp, 0.442_wp, 0.459_wp, 0.475_wp, 0.491_wp, 0.508_wp, 0.524_wp, 0.540_wp, 0.556_wp /
    DATA inferno_b(251:256) / 0.572_wp, 0.587_wp, 0.602_wp, 0.617_wp, 0.631_wp, 0.645_wp /

contains

    subroutine get_colormap_color(value, colormap, color)
        !! Get RGB color from colormap for a normalized value [0,1]
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: colormap
        real(wp), dimension(3), intent(out) :: color
        character(len=:), allocatable :: cmap
        real(wp) :: t
        
        ! Clamp value to [0,1]
        t = max(0.0_wp, min(1.0_wp, value))

        ! Normalize colormap name (case-insensitive)
        cmap = trim(colormap)
        call to_lowercase(cmap)
        
        select case (cmap)
        case ('seaborn', 'colorblind', 'crest')
            call crest_colormap(t, color)
        case ('viridis')
            call viridis_colormap(t, color)
        case ('plasma')
            call plasma_colormap(t, color)
        case ('inferno')
            call inferno_colormap(t, color)
        case ('coolwarm')
            call coolwarm_colormap(t, color)
        case ('jet')
            call jet_colormap(t, color)
        case default
            call crest_colormap(t, color)  ! Use colorblind-friendly as default
        end select
    end subroutine get_colormap_color

    subroutine colormap_value_to_color(z_value, z_min, z_max, colormap, color)
        !! Convert data value to RGB color using colormap
        real(wp), intent(in) :: z_value, z_min, z_max
        character(len=*), intent(in) :: colormap
        real(wp), dimension(3), intent(out) :: color
        
        real(wp) :: normalized_value
        
        if (abs(z_max - z_min) < EPSILON_COMPARE) then
            normalized_value = 0.5_wp
        else
            normalized_value = (z_value - z_min) / (z_max - z_min)
        end if
        
        call get_colormap_color(normalized_value, colormap, color)
    end subroutine colormap_value_to_color

    subroutine viridis_colormap(t, color)
        !! Viridis colormap (256-entry LUT matching matplotlib)
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        call lookup_colormap(t, viridis_r, viridis_g, viridis_b, color)
    end subroutine viridis_colormap

    subroutine plasma_colormap(t, color)
        !! Plasma colormap (256-entry LUT matching matplotlib)
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        call lookup_colormap(t, plasma_r, plasma_g, plasma_b, color)
    end subroutine plasma_colormap

    subroutine inferno_colormap(t, color)
        !! Inferno colormap (256-entry LUT matching matplotlib)
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        call lookup_colormap(t, inferno_r, inferno_g, inferno_b, color)
    end subroutine inferno_colormap


    subroutine coolwarm_colormap(t, color)
        !! Cool-warm diverging colormap
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(3) :: r_points = [0.230_wp, 0.865_wp, 0.706_wp]
        real(wp), dimension(3) :: g_points = [0.299_wp, 0.865_wp, 0.016_wp]
        real(wp), dimension(3) :: b_points = [0.754_wp, 0.865_wp, 0.150_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine coolwarm_colormap

    subroutine jet_colormap(t, color)
        !! Jet colormap (classic rainbow)
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(5) :: r_points = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.5_wp]
        real(wp), dimension(5) :: g_points = [0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp]
        real(wp), dimension(5) :: b_points = [0.5_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine jet_colormap

    subroutine crest_colormap(t, color)
        !! Crest colormap - seaborn colorblind-friendly sequential colormap
        !! Light blue to dark blue, perceptually uniform and colorblind-safe
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(5) :: r_points = [0.855_wp, 0.627_wp, 0.373_wp, 0.188_wp, 0.063_wp]
        real(wp), dimension(5) :: g_points = [0.929_wp, 0.851_wp, 0.671_wp, 0.447_wp, 0.282_wp]
        real(wp), dimension(5) :: b_points = [0.941_wp, 0.918_wp, 0.847_wp, 0.698_wp, 0.502_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine crest_colormap



    subroutine lookup_colormap(t, r_lut, g_lut, b_lut, color)
        !! Lookup color from a 256-entry LUT with linear interpolation
        real(wp), intent(in) :: t
        real(wp), dimension(CMAP_N), intent(in) :: r_lut, g_lut, b_lut
        real(wp), dimension(3), intent(out) :: color
        
        integer :: idx
        real(wp) :: frac
        
        ! Scale t to [0, CMAP_N-1]
        idx = int(t * real(CMAP_N - 1, wp))
        if (idx < 0) idx = 0
        if (idx >= CMAP_N - 1) then
            color = [r_lut(CMAP_N), g_lut(CMAP_N), b_lut(CMAP_N)]
            return
        end if
        
        frac = t * real(CMAP_N - 1, wp) - real(idx, wp)
        
        color(1) = r_lut(idx + 1) * (1.0_wp - frac) + r_lut(idx + 2) * frac
        color(2) = g_lut(idx + 1) * (1.0_wp - frac) + g_lut(idx + 2) * frac
        color(3) = b_lut(idx + 1) * (1.0_wp - frac) + b_lut(idx + 2) * frac
    end subroutine lookup_colormap

    subroutine interpolate_colormap(t, r_points, g_points, b_points, color)
        !! Interpolate between color control points
        real(wp), intent(in) :: t
        real(wp), dimension(:), intent(in) :: r_points, g_points, b_points
        real(wp), dimension(3), intent(out) :: color
        
        integer :: n_points, i
        real(wp) :: dt, weight, t_scaled
        
        n_points = size(r_points)
        
        if (n_points == 1) then
            color = [r_points(1), g_points(1), b_points(1)]
            return
        end if
        
        ! Scale t to [0, n_points-1] and interpolate between adjacent control points
        t_scaled = t * real(n_points - 1, wp)

        ! If exactly at the end, return last point
        if (t_scaled >= real(n_points - 1, wp)) then
            color = [r_points(n_points), g_points(n_points), b_points(n_points)]
            return
        end if

        ! Interpolate between points i and i+1
        i = int(t_scaled) + 1
        if (i < 1) i = 1
        if (i > n_points - 1) i = n_points - 1
        dt = t_scaled - real(i - 1, wp)
        weight = dt

        color(1) = r_points(i) * (1.0_wp - weight) + r_points(i + 1) * weight
        color(2) = g_points(i) * (1.0_wp - weight) + g_points(i + 1) * weight
        color(3) = b_points(i) * (1.0_wp - weight) + b_points(i + 1) * weight
    end subroutine interpolate_colormap

    function validate_colormap_name(colormap) result(is_valid)
        !! Validate if colormap name is supported
        character(len=*), intent(in) :: colormap
        logical :: is_valid
        character(len=:), allocatable :: cmap

        cmap = trim(colormap)
        call to_lowercase(cmap)

        select case (cmap)
        case ('seaborn', 'colorblind', 'crest', 'viridis', 'plasma', 'inferno', &
              'coolwarm', 'jet')
            is_valid = .true.
        case default
            is_valid = .false.
        end select
    end function validate_colormap_name

end module fortplot_colormap
