CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-02-03T18:52:24Z creation;2009-03-18T07:19:26Z update;2015-06-09T19:37:48Z conversion to V3.1;     
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
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _8   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _<   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _@   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _D   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _H   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900649 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA  20060203185224  20150621172515  A5_23579_051                    2C  D   APEX                            1556                            013004                          846 @��/7�=1   @�ߠ�ܾ@5�-V�b����m1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��Ac33A�ffA�  A�ffB  B33B0  BD  BW��Bn  B���B�  B���B�  B�33B�ffB���Bƙ�B�  B�33B�33B�  B�  CffC� C33C� C�C33CL�C$33C)33C.L�C3  C7�fC=� CB��CG��CQL�C[33CeffCo�CyffC���C���C��fC�� C��3C�� C�� C��fC�� C�s3C���C���C��3C�C�Y�C̙�C�� Cֳ3C�� C�� C噚C��C�fC���C�s3D��D� D��D�3D�3D�fDٚD$� D)�3D.�fD3�fD8� D=� DBٚDGٚDL� DQ�3DVٚD[�fD`� De��DjٚDo��Dt��Dy��D�0 D�l�D���D��fD�0 D�l�D��3D��fD��D�i�D��3D��fD�&fD�ffDڣ3D��3D�&fD�S3D��D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA  Aa��A���A�33A陚B��B��B/��BC��BW33Bm��B�ffB���B�ffB���B�  B�33B���B�ffB���B�  B�  B���B���CL�CffC�CffC  C�C33C$�C)�C.33C2�fC7��C=ffCB� CG� CQ33C[�CeL�Co  CyL�C�� C�� C���C�s3C��fC��3C��3C���C�s3C�ffC���C�� C��fC C�L�Č�Cѳ3C֦fC۳3C�3C��C� CC��C�ffD�3DٚD�fD��D��D� D�3D$��D)��D.� D3� D8��D=ٚDB�3DG�3DL��DQ��DV�3D[� D`��De�fDj�3Do�fDt�3Dy�3D�,�D�i�D��fD��3D�,�D�i�D�� D��3D�fD�ffD�� D��3D�#3D�c3Dڠ D�� D�#3D�P D�D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A��PA��A�z�A�n�A�hsA�jA�l�A�ffA�dZA�`BA�dZA�ffA�jA���A�E�A��mA�A�A�A�+A�(�A�A�A���A�O�A��A��#A��RA�{A��A���A��/A�K�A�1A���A��A���A��wA��hA���A�ĜAv{Aln�Aj��A^JAX  AR{AE;dA>�uA4�`A333A/%A*v�A%�wA �A�/A-A5?A
9XA�A�RA �@�V@�/@�7@��@�@�r�@ˍP@�V@���@���@�E�@�$�@��@�=q@� �@�o@��
@�@�X@��j@��m@�@�~�@�9X@�v�@�?}@���@��@��/@���@�bN@~�R@vV@pbN@i�7@b=q@X  @P�`@J=q@C��@=V@4�D@/l�@(Q�@#S�@!�@�@��@V@�#@�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A���A���A��PA��A�z�A�n�A�hsA�jA�l�A�ffA�dZA�`BA�dZA�ffA�jA���A�E�A��mA�A�A�A�+A�(�A�A�A���A�O�A��A��#A��RA�{A��A���A��/A�K�A�1A���A��A���A��wA��hA���A�ĜAv{Aln�Aj��A^JAX  AR{AE;dA>�uA4�`A333A/%A*v�A%�wA �A�/A-A5?A
9XA�A�RA �@�V@�/@�7@��@�@�r�@ˍP@�V@���@���@�E�@�$�@��@�=q@� �@�o@��
@�@�X@��j@��m@�@�~�@�9X@�v�@�?}@���@��@��/@���@�bN@~�R@vV@pbN@i�7@b=q@X  @P�`@J=q@C��@=V@4�D@/l�@(Q�@#S�@!�@�@��@V@�#@�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��BɺBȴBȴBȴBɺBȴBȴBɺB��B��BB�ZB:^B1'B5?BoB�B��B�B�BbNB�B;dB6FB33B��B�jBu�BYB�BDB
�B
�-B
z�B	�B	�9B	bNB	�B	\B�;BÖB�LBo�Bl�BhsB^5Bo�B�=B�=Bs�Bn�B_;BXBZBP�BC�B9XB6FB?}B<jB,B1'BH�BYBT�BaHBq�B�7B��BǮB�fB	
=B	�B	/B	?}B	XB	q�B	}�B	�JB	��B	�B	�wB	��B	�
B	�TB	�yB	�B	�B	��B
%B
bB
�B
"�B
-B
33B
9XB
@�B
F�B
M�B
S�B
ZB
_;B
aHB
e`B
hsB
m�B
q�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��BɺBȴBȴBȴBɺBȴBȴBɺB��B��BB�ZB:^B1'B5?BoB�B��B�B�BbNB�B;dB6FB33B��B�jBu�BYB�BDB
�B
�-B
z�B	�B	�9B	bNB	�B	\B�;BÖB�LBo�Bl�BhsB^5Bo�B�=B�=Bs�Bn�B_;BXBZBP�BC�B9XB6FB?}B<jB,B1'BH�BYBT�BaHBq�B�7B��BǮB�fB	
=B	�B	/B	?}B	XB	q�B	}�B	�JB	��B	�B	�wB	��B	�
B	�TB	�yB	�B	�B	��B
%B
bB
�B
"�B
-B
33B
9XB
@�B
F�B
M�B
S�B
ZB
_;B
aHB
e`B
hsB
m�B
q�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200602170000002006021700000020060217000000200604190000002006041900000020060419000000JA  ARFMfmtp2.2                                                                 20060203185224  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060203185225  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060203190530                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060207125020  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060207125021  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060207130122                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20060217000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20060217000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060908013422  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060908013647                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120600  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071803  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318071926                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609193741                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621172515                      G�O�G�O�G�O�                