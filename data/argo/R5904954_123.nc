CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   |   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  ;`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  @8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  B(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  B�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  D�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  E   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  G    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  H�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Il   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  K\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  M�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    P�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    S�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  V�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    W$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    W(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    W,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    W0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  W4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Wt   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    W�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    W�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         W�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         W�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        W�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    W�Argo profile    3.1 1.2 19500101000000  20181005191716  20181005191716  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               {A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�#�1   @��%\�6�@4��t�j�dex���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      {A   B   A   @,��@�33@�  A   AffA@  A`  A���A�33A�  A�  A�  A�  A�  A�33B   B  B��B��B   B(  B133B6ffB?��BH  BP  BW��B_��Bh  Bp  Bw��B�  B�  B�  B�  B���B�  B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC�fC  C
  C  C  C�C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.�C0  C2  C4�C6�C8  C:�C<  C>  C@  CB  CD  CE�fCH  CJ  CL�CN  CP  CR�CT  CU�fCW�fCZ  C[�fC^  C`  Cb  Cd  Cf�Ch�Cj�Ck�fCm�fCp�Cr�Dy�D�I�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@>{@��
@ȣ�AQ�A"�RADQ�AdQ�A���A�\)A�(�A�(�A�(�A�(�A�(�A�\)B{B	{B�HB�B!{B){B2G�B7z�B@�BI{BQ{BX�B`�Bi{Bq{Bx�B��=B��=B��=B��=B�W
B��=B��pB��pB��=B��pB��=B��=B��=B��=B��=B��=B��=BĊ=B�W
B�W
BЊ=B�W
B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECEC+�C+�CEC
ECECEC^�CECECECECECECEC EC"+�C$EC&EC(EC*EC,EC.^�C0EC2EC4^�C6^�C8EC:^�C<EC>EC@ECBECDECF+�CHECJECL^�CNECPECR^�CTECV+�CX+�CZEC\+�C^EC`ECbECdECf^�Ch^�Cj^�Cl+�Cn+�Cp^�Cr^�Dy�\D�R>D�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aܕ�Aܕ�Aܗ�Aܛ�Aܙ�Aܕ�Aܙ�Aܣ�Aܥ�Aܣ�Aܕ�Aܗ�Aܛ�Aܗ�Aܙ�Aܛ�A܇+A�=qA��yA�A�A�l�A�^5A�33A�33A���A�VA��A�9XA�ƨA��FA��A��-A� �A�5?A�A�`BA�\)A��A�C�A�ZA�7LA�  A���A���A��+A�v�A�JA�VA�z�A��#A���A�(�A�ȴA�ffA���A��uA�bA��jA�K�A��9A��A�bA��hA�G�A���A�
=A��`A��7A��uA��HA�A�I�A���A���A�~�A��A��A�hsA�C�A��A��A��A�JA�I�A~��A}A{�Ay�FAwVAp��Am��Al1'Aj�yAj�+Ah��AfbAd�HAc�TAbz�AaVA`5?A^��A]?}A\(�AZĜAY|�AX�+AW�;AV��AU��AR��AQdZAP�AOANr�ALĜAJ�yAI�hAI7LAHZAG��@|'R@m��@W9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441Aܕ�Aܕ�Aܗ�Aܛ�Aܙ�Aܕ�Aܙ�Aܣ�Aܥ�Aܣ�Aܕ�Aܗ�Aܛ�Aܗ�Aܙ�Aܛ�A܇+A�=qA��yA�A�A�l�A�^5A�33A�33A���A�VA��A�9XA�ƨA��FA��A��-A� �A�5?A�A�`BA�\)A��A�C�A�ZA�7LA�  A���A���A��+A�v�A�JA�VA�z�A��#A���A�(�A�ȴA�ffA���A��uA�bA��jA�K�A��9A��A�bA��hA�G�A���A�
=A��`A��7A��uA��HA�A�I�A���A���A�~�A��A��A�hsA�C�A��A��A��A�JA�I�A~��A}A{�Ay�FAwVAp��Am��Al1'Aj�yAj�+Ah��AfbAd�HAc�TAbz�AaVA`5?A^��A]?}A\(�AZĜAY|�AX�+AW�;AV��AU��AR��AQdZAP�AOANr�ALĜAJ�yAI�hAI7LAHZAG��@|'R@m��@W9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BR�BT�BT�BT�BS�BS�BR�BN�B33B,B5?B:^BG�BP�BZBaHBcTBs�Bx�B}�B�B�+B�%B�DB�hB�hB�bB�PB�VB�7B�B� By�Bu�Bn�BgmB]/BYBR�BK�BD�B@�B<jB7LB+B�B�BoB
=B��B�;BɺB�?B��BbNBD�B2-B%�B�B�BbB
��B
�B
�)B
��B
��B
��B
�LB
��B
z�B
aHB
VB
J�B
?}B
2-B
&�B
�B
B	�TB	��B	ÖB	�dB	�FB	�B	��B	�oB	�=B	�B	v�B	p�B	hsB	aHB	[#B	T�B	N�B	I�B	E�B	@�B	:^B	.B	%�B	!�B	�B	�B	JB	B��B��B��B��B
�B
�B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BR�BT�BT�BT�BS�BS�BR�BN�B33B,B5?B:^BG�BP�BZBaHBcTBs�Bx�B}�B�B�+B�%B�DB�hB�hB�bB�PB�VB�7B�B� By�Bu�Bn�BgmB]/BYBR�BK�BD�B@�B<jB7LB+B�B�BoB
=B��B�;BɺB�?B��BbNBD�B2-B%�B�B�BbB
��B
�B
�)B
��B
��B
��B
�LB
��B
z�B
aHB
VB
J�B
?}B
2-B
&�B
�B
B	�TB	��B	ÖB	�dB	�FB	�B	��B	�oB	�=B	�B	v�B	p�B	hsB	aHB	[#B	T�B	N�B	I�B	E�B	@�B	:^B	.B	%�B	!�B	�B	�B	JB	B��B��B��B��B
�B
�B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191716                              AO  ARCAADJP                                                                    20181005191716    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191716  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191716  QCF$                G�O�G�O�G�O�8000            