CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   c   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2009-10-19T03:55:38Z creation;2015-04-24T02:28:17Z conversion to V3.1;2019-04-22T03:37:46Z update;     
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
_FillValue                  d  ;(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  =   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  =|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  ?   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  CL   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  HT   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  I�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Jp   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Sp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   \p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ep   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    e�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    e�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    e�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    e�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  f    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    f@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    fP   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    fT   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         fd   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         fh   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        fl   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    fpArgo profile    3.1 1.2 19500101000000  20091019035538  20190423051518  2900548 Argo eq. HNFRI                                                  Akira Kusaka                                                    PRES            TEMP            PSAL               �A   JA  A3_030280_156                   2C  D   APEX                            1602                            070204                          846 @�T8#u� 1   @�T8a�Q @C�bM���c�I�^51   ARGOS   A   A   A   Primary sampling: discrete [1 Hz CTD subsampled]                                                                                                                                                                                                                   @�33A��Ak33A���A�  A홚B
��B  B2��BF  BZ  Bl��B�33B�  B�  B���B�33B�33B�ffB�ffBЙ�B���B�ffB홚B���CffCL�C� C33C� CL�CffC$33C)33C.� C3  C8ffC=L�CBffCG  CLL�CQ  CU��C[ffC`L�CeL�CjL�CoffCs�3CyffC~�C��fC�@ C��fC��C��fC�@ C��fC�@ C���C�� C�� C�� C�s3Cǀ Cѳ3Cی�C噚C�fC��fDٚD��D� DٚDٚDٚD� D$�fD)� D.�fD3�3D8� D=�3DBٚDG��DL��DQٚDV�fD[� D`��DeٚDj� Do�fDt� Dy�3D�#3D�` D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��Ak33A���A�  A홚B
��B  B2��BF  BZ  Bl��B�33B�  B�  B���B�33B�33B�ffB�ffBЙ�B���B�ffB홚B���CffCL�C� C33C� CL�CffC$33C)33C.� C3  C8ffC=L�CBffCG  CLL�CQ  CU��C[ffC`L�CeL�CjL�CoffCs�3CyffC~�C��fC�@ C��fC��C��fC�@ C��fC�@ C���C�� C�� C�� C�s3Cǀ Cѳ3Cی�C噚C�fC��fDٚD��D� DٚDٚDٚD� D$�fD)� D.�fD3�3D8� D=�3DBٚDG��DL��DQٚDV�fD[� D`��DeٚDj� Do�fDt� Dy�3D�#3D�` D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�33A�=qA�33A�9XA�=qA�1'A�33A�-A��AgC�A_��AY��ATE�AOC�AG��AD�A@~�A@�DA@jA?/A=�7A<-A9t�A8A�A6 �A61A5oA4ZA3�A0��A/33A,��A*�9A)��A*z�A+|�A+33A)�;A)x�A)K�A(ȴA(1'A'A&v�A$��A${A"�RA!�A ��A�A��A�TAbA��A��AXA��An�A�
A�/A7LAZA	C�A�m@�r�@�A�@�@�|�@ҟ�@��y@�V@�M�@�Q�@�V@��@��;@��!@�Q�@�9X@��/@�X@��H@�r�@��@�@�A�@|j@v��@q��@l9X@g�w@dZ@`��@]��@Y�@O��@F��@?��@9�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�33A�=qA�33A�9XA�=qA�1'A�33A�-A��AgC�A_��AY��ATE�AOC�AG��AD�A@~�A@�DA@jA?/A=�7A<-A9t�A8A�A6 �A61A5oA4ZA3�A0��A/33A,��A*�9A)��A*z�A+|�A+33A)�;A)x�A)K�A(ȴA(1'A'A&v�A$��A${A"�RA!�A ��A�A��A�TAbA��A��AXA��An�A�
A�/A7LAZA	C�A�m@�r�@�A�@�@�|�@ҟ�@��y@�V@�M�@�Q�@�V@��@��;@��!@�Q�@�9X@��/@�X@��H@�r�@��@�@�A�@|j@v��@q��@l9X@g�w@dZ@`��@]��@Y�@O��@F��@?��@9�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B�#B�BƨB�RB�B�uBm�Bp�BJ�BW
BZBYBK�BJ�B6FB/B,B:^B?}BM�BK�BG�BF�BH�BA�BE�BjB�PB��B��B��B�B�B�B��B��B��B��B�PB�B�Bz�Bw�Bt�BjBgmBk�BhsBn�Br�Bq�Bn�B^5BN�B?}B,B�BB��B�B�B�B�B�B�`B��BBoB.B>wBK�B]/Bp�B~�B�JB��B�B��B��B�/B�B��B	
=B	{B	�B	(�B	49B	Q�B	m�B	�B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�sB�sB�mB�gB�aB��B��B�)B��B�\B�wB��B�ZB��Bc�BgRB?�BK�BO\BN�BA;B@�B+�B$�B �B/iB4�BCBA�B=<B<jB>wB6�B:*B^�B�B��B�dB��B��B��B�NB��B�OB��B��B��BzBwfBo�Bl�BjKB_�B\�B`vB]dBcnBg�Bf�Bc�BSuBD3B4�B!�BB��B�B��BߤB��B��B��BچB��B�$B�B#B3�B@�BR Be�BtB�;B��B��B�ZB��B��B�\B�B��B		7B	uB	�B	(�B	F�B	b4B	x�B	�"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<R�8<#�
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
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0000), deepest deltaS=0.011(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-800(dbar) is excluded in mapping; Use P>(dbar) Use P<(dbar) Use THETA<(deg.C) Use THETA>(deg.C)                                                                           Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201611102118312016111021183120161110211831201611110209332016111102093320161111020933201904221201052019042212010520190422120105  JA  ARFMdecpA3_b                                                                20091019035537  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091019035538  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091019035539  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091019035539  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091019035540  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091019035540  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091019035540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091019035540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091019035541  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20091019040245                      G�O�G�O�G�O�                JA  ARFMdecpA3_b                                                                20091022215450  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091022215831  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091022215831  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091022215832  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091022215833  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091022215833  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091022215833  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091022215833  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091022215833  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20091022220249                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424022817                      G�O�G�O�G�O�                JA  ARUP                                                                        20150427114513                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121831  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121831  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20161110170933  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190422030105  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190423051518                      G�O�G�O�G�O�                