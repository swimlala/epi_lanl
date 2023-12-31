CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   H   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-12-04T18:57:36Z creation;2009-03-18T05:33:03Z update;2015-06-08T18:37:02Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER       	            	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME      	            	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME       	            	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS        	               	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER      	         	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION         	         	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE       	            	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE      	            	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR      	            	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE         	         	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE         	            	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO       	            	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION      	            	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE         	            	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD      	         	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC       	         	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION         	         	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE      	         	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE         	         	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC       	         	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM        	            	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC       	         	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC       	         	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC       	         	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME      	            	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER         	         	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES      	         
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z           9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure          ;   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <(   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        <p   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature           =�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature           >�   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  @   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        @`   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity          A�   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity          B�   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  D   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        DP   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  Ep   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   F    SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   O    SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   X    SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  a    HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    a�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    a�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    a�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         a�   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         a�   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        a�   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b Argo profile    3.1 1.2 19500101000000  5900305 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20061204185736  20150617132513  A4_14061_140                    2C  D   APEX                            701                             072602                          846 @�M艫��1   @�M��:�@:b�\(���d�I�^51   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��A�  A�ffBffBD  Bj��B�  B���B���Bƙ�B�  B�33CffC� CL�C��C(�fC3  C=� CG��C[L�Co33C�s3C��fC��fC�� C�� C���C���Cǀ C�s3C۳3C�3C�3C��fD�3DٚD�fD�fD� DٚD� D$� D)� D.��D3ٚD8�fD=�3DB�fDG�3DN�DTY�DZ��D`��Dg3Dm` Ds� Dy��D�)�D�c3D�� D��fD�&fD�s3D�� D�s3D�� D�ffD���D�Y�D�ٚ111111111111111111111111111111111111111111111111111111111111111111111111@�33@�  AvffA͙�B  B5��B\ffB���B���B���B�ffB���B�  B���C�fC�3C33C%L�C/ffC9�fCD  CW�3Ck��CL�C�ٚC�ٚC��3C��3C�� C�� Cų3CϦfC��fC��fC��fC�ٚD ��D�3D
� D  D��D�3D��D#��D(��D-�fD2�3D8  D<��DA� DF��DM&fDSs3DY�fD_�fDf,�Dly�Dr��Dx�3D��fD�� D�<�D�s3D��3D�  D�|�D�  D�l�D��3D�y�D��fD�ff111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��!A��!A��9A��^A��!A���A�`BA���A��PA��A��uA�dZA��A��RA��\A�bA�;dA���A�jA��A�{AuhsAmAd~�A[|�ARVAI�TACO�A;A4$�A,�A&��A!
=AbAA��A�@���@�/@�hs@��m@�r�@��`@��@���@��@�Ĝ@���@�K�@��@�/@�  @�A�@�Ĝ@��-@�;d@���@{C�@v��@n��@d�@[�F@S�m@L�D@Dz�@6�y@+"�@   @{@E�@��@ Q�111111111111111111111111111111111111111111111111111111111111111111111111A��!A��!A��9A��^A��!A���A�`BA���A��PA��A��uA�dZA��A��RA��\A�bA�;dA���A�jA��A�{AuhsAmAd~�A[|�ARVAI�TACO�A;A4$�A,�A&��A!
=AbAA��A�@���@�/@�hs@��m@�r�@��`@��@���@��@�Ĝ@���@�K�@��@�/@�  @�A�@�Ĝ@��-@�;d@���@{C�@v��@n��@d�@[�F@S�m@L�D@Dz�@6�y@+"�@   @{@E�@��@ Q�111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB,B,B,B,B-B,B,B7LB:^B49BƨB��Bo�BO�B%B��B��By�B\B
��B
��B
VB
0!B
B	��B	��B	k�B	K�B	%�B	B�ZB��B�3B��B~�Bm�B_;BP�BG�B?}B9XB49B9XBK�BZB`BBq�B�1B��BB�BB	VB	%�B	C�B	^5B	p�B	�+B	�hB	��B	�jB	��B	�ZB	��B
  B

=B
/B
A�B
R�B
^5B
gmB
q�B
{�111111111111111111111111111111111111111111111111111111111111111111111111B!�B!�B!�B!�B"�B!�B!�B-B0!B-B��B�VBffBH�B��BĜB��Bs�B1B
ȴB
��B
M�B
'�B	��B	ŢB	�{B	cTB	D�B	�B��B�)BB�B�uBw�BffBXBI�B@�B8RB2-B,B1'BC�BQ�BXBiyB� B�{B�^B�B	%B	�B	;dB	VB	hsB	~�B	�7B	��B	�9B	ǮB	�)B	�B	��B
B
&�B
9XB
J�B
VB
_;B
iyB
s�111111111111111111111111111111111111111111111111111111111111111111111111<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADUSTED=PSAL(corrected by SSP&CTM)+deltaS, where deltaS is calculated by WJO                                                                                                                                                                               SP(NextCycle) = 3.6 dbar                                                                                                                                                                                                                                        None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0002), deepest deltaS=-0.010(+-0.006)                                                                                                                                                                                                              Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : max(combines 1x WJO uncertainty & CTM adjustment , 0.01(PSS-78))                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            WJO(2003) salinity adjustment is adopted with SeHyD1.0; large scale 8/4, small scale 4/2; Use interpolate_float_valuesnov2003.m, map_data_grid.m, map_data_grid_t.m; Use T levels <= 12C; Run Const:18;                                                         200612280546252006122805462520061228054625200701260525112007012605251120070126052511200810280000002008102800000020081028000000  JA  ARFMfmtp2.3                                                                 20061204185736  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061204185739  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20061204192521                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20061209033852  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061209033852  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20061209035943                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20061228054625  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20061228054625  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126052511  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20081028000000  CV  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081106051331  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081106071007                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312114227  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318052822  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318053303                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608183652                      G�O�G�O�G�O�                JA  ARDU                                                                        20150617132513                      G�O�G�O�G�O�                