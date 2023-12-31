CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   b   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2009-10-09T12:56:07Z creation;2015-04-24T02:28:17Z conversion to V3.1;2019-04-22T03:37:56Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  ;$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  =   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  =t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  AL   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  I�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   JL   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   SL   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   \L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  eL   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    e�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    e�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    e�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    e�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  e�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    f   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    f,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    f0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         f@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         fD   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        fH   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    fLArgo profile    3.1 1.2 19500101000000  20091009125607  20190423051518  2900548 Argo eq. HNFRI                                                  Akira Kusaka                                                    PRES            TEMP            PSAL               �A   JA  A3_030280_155                   2C  D   APEX                            1602                            070204                          846 @�Q��.� 1   @�Qæ� @C�ě��T�d
� ě�1   ARGOS   A   A   A   Primary sampling: discrete [1 Hz CTD subsampled]                                                                                                                                                                                                                   @���A  Ad��A���A�  A���B33B33B2ffBE33BY33Bn  B�  B���B�  B�  B�ffB���B�33B�  BЙ�B�ffB䙚B�ffB���C� CffCffCffC� CL�C33C$ffC)33C.�C3  C8�C=L�CA��CG�CL33CP��CV33C[  C`ffCe33Cj�Cn�fCt�Cy� C~��C��3C�&fC��3C�L�C��3C�&fC��fC�33C���C���C���C���C��3CǙ�Cь�CۦfC噚C��C��3D�3D�fD� DٚD� D�fD� D$� D)�fD.ٚD3� D8�3D=ٚDBٚDG�fDL� DQ�fDV� D[�3D`��De�3Dj�fDo�3Dt�fDy��D�fD�c3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���A  Ad��A���A�  A���B33B33B2ffBE33BY33Bn  B�  B���B�  B�  B�ffB���B�33B�  BЙ�B�ffB䙚B�ffB���C� CffCffCffC� CL�C33C$ffC)33C.�C3  C8�C=L�CA��CG�CL33CP��CV33C[  C`ffCe33Cj�Cn�fCt�Cy� C~��C��3C�&fC��3C�L�C��3C�&fC��fC�33C���C���C���C���C��3CǙ�Cь�CۦfC噚C��C��3D�3D�fD� DٚD� D�fD� D$� D)�fD.ٚD3� D8�3D=ٚDBٚDG�fDL� DQ�fDV� D[�3D`��De�3Dj�fDo�3Dt�fDy��D�fD�c3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A�r�A�`BA�E�A��+A��TA��Ai�
Ab~�AW�FAO��AJ��AHn�AE��AC��AB  A?�mA=�#A<�yA;�7A;�A:��A9�-A6��A6�jA5��A3�^A2��A0��A0��A.�uA.��A-�FA,��A+��A*n�A)��A(�!A(1A&�jA%%A#�mA#7LA!�A v�A��A�;A�9A��AbAffA�A  A�;A^5AXA�A��A~�A	C�Ax�@��m@�O�@�I�@ݩ�@�$�@�A�@��
@�+@� �@�33@��@��\@��@�l�@���@���@���@�Z@��#@�l�@�1@�V@}?}@yhs@u?}@pQ�@kdZ@g;d@co@_��@\1@XA�@Qhs@K@Dz�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A�r�A�`BA�E�A��+A��TA��Ai�
Ab~�AW�FAO��AJ��AHn�AE��AC��AB  A?�mA=�#A<�yA;�7A;�A:��A9�-A6��A6�jA5��A3�^A2��A0��A0��A.�uA.��A-�FA,��A+��A*n�A)��A(�!A(1A&�jA%%A#�mA#7LA!�A v�A��A�;A�9A��AbAffA�A  A�;A^5AXA�A��A~�A	C�Ax�@��m@�O�@�I�@ݩ�@�$�@�A�@��
@�+@� �@�33@��@��\@��@�l�@���@���@���@�Z@��#@�l�@�1@�V@}?}@yhs@u?}@pQ�@kdZ@g;d@co@_��@\1@XA�@Qhs@K@Dz�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBJBJB	7B1B+BB{B�1B�B49B<jB5?B>wBL�BXBP�BK�BN�BG�BM�BG�B\)BbNBl�B_;B~�B}�Bs�B|�Bt�B�DB|�B�7B�=B�DB�PB�7B�7B�1B�B�%B�B�B�Bz�Bt�Bq�B{�Bw�Bw�Bn�BdZBffB[#Be`BcTB_;BZBS�BH�B;dB-B�B+B��B�B�mB�TB�TB�yB�B��BJB�B&�B6FBD�BQ�BdZBs�B�B�oB��B�dBɺB�B�NB�B	B	VB	�B	&�B	1'B	<jB	P�B	dZB	w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BBB B�B�B��B�B�B��B�B.B5%B,�B4nBB�BM�BF�BA�BD�B="BCGB<�BQhBW�Bb�BTaBtTBs�BiBr�Bi�B� Bq�B~wB}B��B��B~�B~wB}VBz�B{�By�Bv+Bw�Bp�Bi�BffBq[BmCBmwBdZBY�B[�BPBZ�BX�BT�BO�BIRB>BB0�B"�B
�B��B�iB�FB��B�B��B��B��B�3BoB�BB+kB9�BF�BYKBh�BxB�zB��B�UB��B��B�?B�B��B	GB	}B	�B	&B	1AB	E�B	Y1B	l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6��<Z��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0000), deepest deltaS=0.011(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-800(dbar) is excluded in mapping; Use P>(dbar) Use P<(dbar) Use THETA<(deg.C) Use THETA>(deg.C)                                                                           Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201611102118252016111021182520161110211825201611110209332016111102093320161111020933201904221201012019042212010120190422120101  JA  ARFMdecpA3_b                                                                20091009125606  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091009125607  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091009125607  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091009125608  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091009125609  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091009125609  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091009125609  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091009125609  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091009125609  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20091009130334                      G�O�G�O�G�O�                JA  ARFMdecpA3_b                                                                20091013005327  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091013005726  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091013005727  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091013005727  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091013005729  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091013005729  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091013005729  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091013005729  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091013005729  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20091013010250                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424022817                      G�O�G�O�G�O�                JA  ARUP                                                                        20150427114513                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121825  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121825  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20161110170933  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190422030101  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190423051518                      G�O�G�O�G�O�                