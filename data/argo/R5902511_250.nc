CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  n   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  WP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  ^,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ٤   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p <   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p @�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` [�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   \X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   bX   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   hX   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T nX   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   n�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   n�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   n�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   n�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � n�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   oL   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   oh   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    op   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        o�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        o�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       o�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    o�Argo profile    3.1 1.2 19500101000000  20230721224951  20230721224951  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @��؎�B@��؎�B11  @��qʀ@��qʀ@1�p
@1�p
�d��Mj�d��Mj11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?��?�@=p�@�  @��R@�p�@�  A ��A��A   A*�HA>�RA`��A�Q�A�  A�Q�A�Q�A��A�  A��A�A��B�
B  BQ�B (�B(  B/�
B7�
B?�BH  BPQ�BX  B`  Bh(�Bp(�Bx(�B�=qB�(�B��B��
B��B�  B�  B�  B�  B��B�{B�{B�{B�(�B�(�B�  B�{B��B�  B�  B�{B�(�B�  B�  B�  B�{B�{B�{B�(�B�{B�  B�{C   C  C  C  C  C
  C  C��C��C
=C
=C  C  C
=C��C�C   C"
=C$  C&  C(  C*  C,
=C.{C0  C2  C4{C6{C8  C9��C<
=C=��C@  CA��CD
=CF
=CH  CJ  CK��CM��CP  CR  CS�CU��CX
=CZ  C\  C]��C`  Cb
=Cd{Cf
=Ch  Cj  Cl
=Cn
=Cp  Cr  Ct
=Cv
=Cx  Cz  C|
=C~  C�C�  C�C�C�  C�C�
=C�  C�C�C���C�  C�  C�  C�  C�  C���C�  C�  C�C�C���C�  C�  C���C�C�C���C���C���C���C���C���C���C�  C�  C�C�
=C�  C�C�  C�C�C�C���C���C�  C���C���C�  C���C�C�
=C�C�  C�  C�  C���C�  C�  C���C���C���C�  C�  C�C�C�  C���C�  C�C�  C���C���C���C�  C�C�C�  C�  C�C�C�C�C�C�C�  C���C���C���C�C���C���C���C�  C�  C�  C�
=C�C�  C�  C�  C�C�C�C���C�  C�C�C�C���C�  C�C�
=C�
=C�C���C���C�  C�  C���C�  C�  C�C�C�  C���C���C���D � D  D}qD  D� D  D� D�qD� D�qD� DD� D��Dz�D  D}qD�qD	z�D	�RD
� D  D}qD�D��D�D� D�D��DD��D�qD}qD�qD}qD�D� D�qD}qD  D��D  D}qD�qD��D  Dz�D  D��D��DxRD��D� D�D� D�qDz�D��D}qD�D� D  D��D D ��D!�D!� D!�qD"}qD#  D#�D$�D$�D%�D%}qD%�qD&��D'D'� D'�qD(z�D(��D)}qD*  D*� D+  D+}qD,  D,� D-  D-� D.  D.�D/�D/��D0D0�D1�D1�D2D2� D2�qD3� D3�qD4}qD4�qD5� D6  D6}qD7  D7� D7�qD8}qD9  D9��D:  D:� D;  D;� D<�D<� D=  D=� D>  D>� D>��D?}qD@�D@� DA  DA��DB�DB��DC�DC� DD  DD�DE�DE� DF  DF}qDF�qDG}qDH  DH� DH��DIxRDI��DJ� DK  DK� DL�DL��DM�DM� DM�qDN� DODO��DO�qDP}qDP�qDQ� DR  DR� DS�DS�DT  DT��DU  DU� DV�DV� DW�DW� DW�qDX��DY  DY� DZDZ�D[D[��D\�D\}qD]�D]�D^�D^z�D^��D_� D`  D`� Da�Da}qDb  Db��Db�qDc}qDc��Dd��De�De}qDe�qDfz�Df�qDg��DhDh�Di  Di� Dj  Dj��Dk�Dk�DlDl��DmDm� Dn�Dn�Do  Do� Dp  Dp}qDp�qDq}qDr  Dr� Ds�Ds�Dt  Dt}qDu  Du��DvDv� Dv��Dw��DxDx� Dy  Dy��Dz  Dz� D{  D{� D|�D|��D}  D}� D~D~�DD��D�HD�AHD�� D��HD�  D�>�D�~�D���D�  D�@ D�~�D���D���D�@ D���D��HD�  D�AHD��HD�� D���D�>�D�� D�� D�HD�AHD�� D��HD�  D�>�D�~�D��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
>�Q�?#�
?�  ?���?���?�ff@
=q@#�
@(��@=p�@Tz�@aG�@z�H@��
@��@���@�  @��@�@�p�@�ff@�33@�(�@��
@�\)@�A ��A�A	��A�RA�\A
=Ap�A ��A%A*�HA-p�A4z�A8��A<��AC�
AG�AMp�AS33AW
=A]p�Aa�AeAl��Ap  AuAy��A~�RA��\A�z�A�
=A��A��
A��RA��A��
A�ffA���A��A�ffA�G�A�(�A�A���A��A��A���A��HA���A�  A�=qA�(�A�\)A���AÅAƸRA���A�33AθRAУ�AӅA�{A�  AۅA�p�A�  A��HA���A�  A��HA���A�  A�\A�z�A�  A�=qA���B   B ��B{B�
B�B{B�B��B
{B�B��B{B\)B��B=qB33B��B{B
=B��B�B
=B��B��B\)B ��B!B#
=B$��B%�B&�HB(Q�B)�B*�HB,(�B-B.�HB0(�B1B2�RB4Q�B5p�B6�HB8Q�B9G�B:�HB<Q�B=�B>�RB@  BA�BB�\BD  BD��BF�\BG�BH��BJ�\BK\)BL��BN=qBO\)BPz�BQ�BS33BT(�BUG�BW
=BX  BY�BZ�\B[�B\��B^ffB_\)B`��Bb{Bc
=BdQ�Be�Bg33Bh(�Bip�Bj�HBk�BmG�Bn�\Bo�Bp��Br{Bs
=Bt(�Bu��Bv�HBw�Bx��Bz�\B{\)B|��B~{B
=B�(�B���B��B�  B��RB��B��B��\B�\)B��
B�ffB�33B��
B�Q�B�
=B�B�Q�B��RB��B�=qB���B��B�  B��\B�\)B��B�z�B�G�B��B�ffB��B��
B�=qB���B��B�{B��RB�p�B�(�B��\B�33B��B�z�B���B�B�ffB��HB��B�Q�B���B�\)B�=qB��RB�G�B�{B���B��B�  B���B��B��B���B��B��B�ffB�
=B���B�(�B��HB��B�  B���B�p�B�  B�z�B�33B��B�ffB�
=B��B�{B���B�p�B��B�=qB��HB�\)B��B�(�B��RB�
=B�\)B��B�=qB�z�B���B�G�B��B��
B�=qB�z�B���B���B�\)B�p�B��B�(�B�ffB��\B���B�G�B��B��B�  B�z�B���B���B�33B��B�  B�=qB�ffB��HB�33B�\)B��B�{B�Q�B�z�B���B�\)BÅB�B�(�Bď\BĸRB���B�\)B�B�  B�=qBƣ�B�
=B�\)BǅB��B�Q�Bȏ\B���B�
=B�p�B��
B�{B�=qBʣ�B�
=B�G�B�p�BˮB�{B�ffB�z�B���B�33BͅBͮB��B�Q�BθRB�
=B��Bϙ�B�  B�=qB�ffB��HB�G�B�p�BѮB�(�Bҏ\B���B���B�G�B�B�{B�Q�Bԏ\B���B�p�BծB��
B�(�B֣�B�
=B�33BׅB��B�=qB�ffB��HB�33B�p�B�B�=qB�ffBڸRB�33BۅBۮB��B�z�BܸRB��HB�33BݮB�  B�(�Bޏ\B�
=B�G�B߅B��B�ffB��B��HB�\)B�B��B�Q�B�RB��HB�G�B�B��B�(�B��B���B��B�B��
B�{B�z�B��HB�
=B�p�B��
B�{B�ffB��HB�
=B陚B��B�{B�z�B���B�33B�B�  B�=qB�\B���B�p�B홚B�  B�z�B���B���B�p�B��B�=qB�z�B��HB�G�B�B��
B�=qB�RB��HB�G�B�B�(�B�Q�B���B�33B�G�B���B�(�B�Q�B���B��B�G�B��B�{B�Q�B��\B���B�p�B��B��B�ffB��RB��HB�G�B�B��
B�=qB���B���B�G�B��B�B�(�B��\B��RB�
=B�p�B��
C 
=C (�C p�C ��C ��C �HC�C33C\)C�CC��C
=C(�CffC��C��C�HC{C(�CQ�C�\C��CC��C(�C33Cp�C��C��C�HC  C{CG�Cz�C�\C�C�C{C33CQ�CffC��C��C�HC  C=qCffC�\C��C�
C  C33CQ�Cp�C�C�HC��C	�C	\)C	�C	��C	��C

=C
=qC
Q�C
p�C
�C
�HC
=C�CG�C�C�RC�
C�C�C\)C�C��CC��C33CQ�Cz�C��C��C
=C33C\)Cz�C�C�C{C(�CffC��C�RC�HC{CQ�Cz�C��CC  C=qCffC�\C�C  C�CG�Cp�C�RC�C
=C(�CffC��C�
C��C�C\)C��CC�C{CQ�C�\CC�C{C\)C�\C��C
=C=qCffC�\C�
C�CG�CffC��C�
C�CffC��CC  CG�C�\C�RC�HC(�Cp�C�C�HC{C=qC�C��C  C33CffC��C�HC�CG�Cz�C��C
=CG�Cp�C��C��C =qC z�C �C �HC!{C!Q�C!��C!�HC"(�C"ffC"�\C"��C#
=C#=qC#z�C#C$
=C$G�C$p�C$�C$�C%=qC%�C%C%��C&(�C&\)C&��C&�C'(�C'Q�C'�C'�RC(  C(Q�C(z�C(��C(�
C){C)\)C)��C)�
C)��C*(�C*ffC*�RC*��C+(�C+\)C+�\C+��C,{C,Q�C,��C,��C-  C-=qC-�C-��C.  C.33C.ffC.��C.�
C/�C/ffC/��C/�
C0
=C0=qC0ffC0�RC1  C1G�C1z�C1�RC1�C2(�C2ffC2��C2�
C3{C3\)C3��C3�HC4{C4\)C4��C4�
C5  C533C5p�C5�RC5��C6=qC6z�C6�RC6�C7{C7G�C7�C7�RC8  C8=qC8z�C8C8��C9�C9Q�C9�\C9C9��C:(�C:ffC:��C:�
C;�C;G�C;�C;�RC;�C<(�C<\)C<z�C<�C<�C=�C=Q�C=��C=��C>{C>=qC>p�C>�RC>��C?(�C?ffC?��C?�
C@{C@Q�C@�C@�RC@�CA�CAQ�CA�\CA�RCA�HCB{CBQ�CB�CB��CC  CC33CCffCC��CCCC�CD�CDG�CDz�CD��CD�HCE{CEQ�CE�\CECE��CF(�CF\)CF�\CFCF��CG(�CGQ�CGz�CG�CG�
CH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��?�@=p�@�  @��R@�p�@�  A ��A��A   A*�HA>�RA`��A�Q�A�  A�Q�A�Q�A��A�  A��A�A��B�
B  BQ�B (�B(  B/�
B7�
B?�BH  BPQ�BX  B`  Bh(�Bp(�Bx(�B�=qB�(�B��B��
B��B�  B�  B�  B�  B��B�{B�{B�{B�(�B�(�B�  B�{B��B�  B�  B�{B�(�B�  B�  B�  B�{B�{B�{B�(�B�{B�  B�{C   C  C  C  C  C
  C  C��C��C
=C
=C  C  C
=C��C�C   C"
=C$  C&  C(  C*  C,
=C.{C0  C2  C4{C6{C8  C9��C<
=C=��C@  CA��CD
=CF
=CH  CJ  CK��CM��CP  CR  CS�CU��CX
=CZ  C\  C]��C`  Cb
=Cd{Cf
=Ch  Cj  Cl
=Cn
=Cp  Cr  Ct
=Cv
=Cx  Cz  C|
=C~  C�C�  C�C�C�  C�C�
=C�  C�C�C���C�  C�  C�  C�  C�  C���C�  C�  C�C�C���C�  C�  C���C�C�C���C���C���C���C���C���C���C�  C�  C�C�
=C�  C�C�  C�C�C�C���C���C�  C���C���C�  C���C�C�
=C�C�  C�  C�  C���C�  C�  C���C���C���C�  C�  C�C�C�  C���C�  C�C�  C���C���C���C�  C�C�C�  C�  C�C�C�C�C�C�C�  C���C���C���C�C���C���C���C�  C�  C�  C�
=C�C�  C�  C�  C�C�C�C���C�  C�C�C�C���C�  C�C�
=C�
=C�C���C���C�  C�  C���C�  C�  C�C�C�  C���C���C���D � D  D}qD  D� D  D� D�qD� D�qD� DD� D��Dz�D  D}qD�qD	z�D	�RD
� D  D}qD�D��D�D� D�D��DD��D�qD}qD�qD}qD�D� D�qD}qD  D��D  D}qD�qD��D  Dz�D  D��D��DxRD��D� D�D� D�qDz�D��D}qD�D� D  D��D D ��D!�D!� D!�qD"}qD#  D#�D$�D$�D%�D%}qD%�qD&��D'D'� D'�qD(z�D(��D)}qD*  D*� D+  D+}qD,  D,� D-  D-� D.  D.�D/�D/��D0D0�D1�D1�D2D2� D2�qD3� D3�qD4}qD4�qD5� D6  D6}qD7  D7� D7�qD8}qD9  D9��D:  D:� D;  D;� D<�D<� D=  D=� D>  D>� D>��D?}qD@�D@� DA  DA��DB�DB��DC�DC� DD  DD�DE�DE� DF  DF}qDF�qDG}qDH  DH� DH��DIxRDI��DJ� DK  DK� DL�DL��DM�DM� DM�qDN� DODO��DO�qDP}qDP�qDQ� DR  DR� DS�DS�DT  DT��DU  DU� DV�DV� DW�DW� DW�qDX��DY  DY� DZDZ�D[D[��D\�D\}qD]�D]�D^�D^z�D^��D_� D`  D`� Da�Da}qDb  Db��Db�qDc}qDc��Dd��De�De}qDe�qDfz�Df�qDg��DhDh�Di  Di� Dj  Dj��Dk�Dk�DlDl��DmDm� Dn�Dn�Do  Do� Dp  Dp}qDp�qDq}qDr  Dr� Ds�Ds�Dt  Dt}qDu  Du��DvDv� Dv��Dw��DxDx� Dy  Dy��Dz  Dz� D{  D{� D|�D|��D}  D}� D~D~�DD��D�HD�AHD�� D��HD�  D�>�D�~�D���D�  D�@ D�~�D���D���D�@ D���D��HD�  D�AHD��HD�� D���D�>�D�� D�� D�HD�AHD�� D��HD�  D�>�D�~�D��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
>�Q�?#�
?�  ?���?���?�ff@
=q@#�
@(��@=p�@Tz�@aG�@z�H@��
@��@���@�  @��@�@�p�@�ff@�33@�(�@��
@�\)@�A ��A�A	��A�RA�\A
=Ap�A ��A%A*�HA-p�A4z�A8��A<��AC�
AG�AMp�AS33AW
=A]p�Aa�AeAl��Ap  AuAy��A~�RA��\A�z�A�
=A��A��
A��RA��A��
A�ffA���A��A�ffA�G�A�(�A�A���A��A��A���A��HA���A�  A�=qA�(�A�\)A���AÅAƸRA���A�33AθRAУ�AӅA�{A�  AۅA�p�A�  A��HA���A�  A��HA���A�  A�\A�z�A�  A�=qA���B   B ��B{B�
B�B{B�B��B
{B�B��B{B\)B��B=qB33B��B{B
=B��B�B
=B��B��B\)B ��B!B#
=B$��B%�B&�HB(Q�B)�B*�HB,(�B-B.�HB0(�B1B2�RB4Q�B5p�B6�HB8Q�B9G�B:�HB<Q�B=�B>�RB@  BA�BB�\BD  BD��BF�\BG�BH��BJ�\BK\)BL��BN=qBO\)BPz�BQ�BS33BT(�BUG�BW
=BX  BY�BZ�\B[�B\��B^ffB_\)B`��Bb{Bc
=BdQ�Be�Bg33Bh(�Bip�Bj�HBk�BmG�Bn�\Bo�Bp��Br{Bs
=Bt(�Bu��Bv�HBw�Bx��Bz�\B{\)B|��B~{B
=B�(�B���B��B�  B��RB��B��B��\B�\)B��
B�ffB�33B��
B�Q�B�
=B�B�Q�B��RB��B�=qB���B��B�  B��\B�\)B��B�z�B�G�B��B�ffB��B��
B�=qB���B��B�{B��RB�p�B�(�B��\B�33B��B�z�B���B�B�ffB��HB��B�Q�B���B�\)B�=qB��RB�G�B�{B���B��B�  B���B��B��B���B��B��B�ffB�
=B���B�(�B��HB��B�  B���B�p�B�  B�z�B�33B��B�ffB�
=B��B�{B���B�p�B��B�=qB��HB�\)B��B�(�B��RB�
=B�\)B��B�=qB�z�B���B�G�B��B��
B�=qB�z�B���B���B�\)B�p�B��B�(�B�ffB��\B���B�G�B��B��B�  B�z�B���B���B�33B��B�  B�=qB�ffB��HB�33B�\)B��B�{B�Q�B�z�B���B�\)BÅB�B�(�Bď\BĸRB���B�\)B�B�  B�=qBƣ�B�
=B�\)BǅB��B�Q�Bȏ\B���B�
=B�p�B��
B�{B�=qBʣ�B�
=B�G�B�p�BˮB�{B�ffB�z�B���B�33BͅBͮB��B�Q�BθRB�
=B��Bϙ�B�  B�=qB�ffB��HB�G�B�p�BѮB�(�Bҏ\B���B���B�G�B�B�{B�Q�Bԏ\B���B�p�BծB��
B�(�B֣�B�
=B�33BׅB��B�=qB�ffB��HB�33B�p�B�B�=qB�ffBڸRB�33BۅBۮB��B�z�BܸRB��HB�33BݮB�  B�(�Bޏ\B�
=B�G�B߅B��B�ffB��B��HB�\)B�B��B�Q�B�RB��HB�G�B�B��B�(�B��B���B��B�B��
B�{B�z�B��HB�
=B�p�B��
B�{B�ffB��HB�
=B陚B��B�{B�z�B���B�33B�B�  B�=qB�\B���B�p�B홚B�  B�z�B���B���B�p�B��B�=qB�z�B��HB�G�B�B��
B�=qB�RB��HB�G�B�B�(�B�Q�B���B�33B�G�B���B�(�B�Q�B���B��B�G�B��B�{B�Q�B��\B���B�p�B��B��B�ffB��RB��HB�G�B�B��
B�=qB���B���B�G�B��B�B�(�B��\B��RB�
=B�p�B��
C 
=C (�C p�C ��C ��C �HC�C33C\)C�CC��C
=C(�CffC��C��C�HC{C(�CQ�C�\C��CC��C(�C33Cp�C��C��C�HC  C{CG�Cz�C�\C�C�C{C33CQ�CffC��C��C�HC  C=qCffC�\C��C�
C  C33CQ�Cp�C�C�HC��C	�C	\)C	�C	��C	��C

=C
=qC
Q�C
p�C
�C
�HC
=C�CG�C�C�RC�
C�C�C\)C�C��CC��C33CQ�Cz�C��C��C
=C33C\)Cz�C�C�C{C(�CffC��C�RC�HC{CQ�Cz�C��CC  C=qCffC�\C�C  C�CG�Cp�C�RC�C
=C(�CffC��C�
C��C�C\)C��CC�C{CQ�C�\CC�C{C\)C�\C��C
=C=qCffC�\C�
C�CG�CffC��C�
C�CffC��CC  CG�C�\C�RC�HC(�Cp�C�C�HC{C=qC�C��C  C33CffC��C�HC�CG�Cz�C��C
=CG�Cp�C��C��C =qC z�C �C �HC!{C!Q�C!��C!�HC"(�C"ffC"�\C"��C#
=C#=qC#z�C#C$
=C$G�C$p�C$�C$�C%=qC%�C%C%��C&(�C&\)C&��C&�C'(�C'Q�C'�C'�RC(  C(Q�C(z�C(��C(�
C){C)\)C)��C)�
C)��C*(�C*ffC*�RC*��C+(�C+\)C+�\C+��C,{C,Q�C,��C,��C-  C-=qC-�C-��C.  C.33C.ffC.��C.�
C/�C/ffC/��C/�
C0
=C0=qC0ffC0�RC1  C1G�C1z�C1�RC1�C2(�C2ffC2��C2�
C3{C3\)C3��C3�HC4{C4\)C4��C4�
C5  C533C5p�C5�RC5��C6=qC6z�C6�RC6�C7{C7G�C7�C7�RC8  C8=qC8z�C8C8��C9�C9Q�C9�\C9C9��C:(�C:ffC:��C:�
C;�C;G�C;�C;�RC;�C<(�C<\)C<z�C<�C<�C=�C=Q�C=��C=��C>{C>=qC>p�C>�RC>��C?(�C?ffC?��C?�
C@{C@Q�C@�C@�RC@�CA�CAQ�CA�\CA�RCA�HCB{CBQ�CB�CB��CC  CC33CCffCC��CCCC�CD�CDG�CDz�CD��CD�HCE{CEQ�CE�\CECE��CF(�CF\)CF�\CFCF��CG(�CGQ�CGz�CG�CG�
CH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�(�A�+A�"�A�"�A�&�A�&�A�&�A�"�A�VA�JA�%A��A��HA̼jA̲-A̮A̗�Ȧ+A̅ÃA�~�A�|�A�z�A�x�A�v�A�r�A�n�A�jA�l�A�r�A�v�A�x�ÁA̋DA̍PA̡�A̧�A�I�A�l�A�XA���A�x�A�bNA�?}A��A�{A��;A�A˺^Aʲ-AʶFAʴ9A�`BA��;A�
=A�9XA�bA�S�A��\A�JA�1'A��A���A���A�"�A�M�A�+A��yA��yA���A���A��;A�ĜA���A��-A���A�/A���A�$�A��7A��A�l�A���A�A�G�A��A�A��!A�bA�;dA���A���A���A�(�A��`A��A�v�A���A~r�Aw�7ArVAp�Ap�uAlI�AiXAf=qAb��A_"�A^ �AY�TAX�DAVjAP��AO\)ANbNAL�jAKt�AI��AH�RAF5?AC��ABVAA��AAS�A@��A>��A<�A<$�A<bA;�#A;/A:A�A9t�A7p�A6-A4�HA2��A/VA-�TA-�hA-�A,�RA,�\A,VA,{A+O�A)\)A(  A'�-A';dA&$�A$M�A#�hA"��A"^5A!�A!?}A M�A~�A��AK�A�`Av�A��AZA��A�HA�AffAƨAO�A�+AK�A-A�A+A
jA	A��A�\A�TAS�A�A��AQ�A^5An�AoA/A��A�!Av�AZAE�A�A�mA��AAjA��A ��A 1@�+@�E�@�p�@��u@��@��R@��F@���@��@�$�@��j@��;@�=q@��@�F@�n�@���@��T@��@�(�@�K�@�-@噚@��@�h@�5?@���@�Z@�b@��@��@�^@���@�S�@�w@�7@�Q�@�@�`B@�1'@�@ڟ�@�~�@�$�@؛�@���@� �@�r�@ج@��@և+@ա�@�S�@�{@щ7@�G�@��`@���@���@ЋD@� �@�dZ@�@�~�@͑h@̣�@�  @�o@�ȴ@�ff@�$�@�x�@�1'@��@�ƨ@ǝ�@�|�@�33@�o@ƸR@�=q@��@���@ź^@őh@ě�@+@�@��-@�X@��@���@�z�@�1'@��@�  @�ƨ@��P@�\)@�+@��@��!@�{@��@���@��u@�bN@���@�"�@���@�v�@�{@�x�@��@��`@��@�Q�@�  @��F@�\)@��@�$�@�G�@���@���@��m@�C�@��y@��+@��@��@��T@���@��-@��-@�x�@��@��u@���@���@��P@��P@��@�33@��y@���@��+@�$�@��^@�hs@��@�j@��@�  @��;@���@�o@���@�v�@�n�@�^5@��@���@�`B@�X@�O�@�7L@�&�@��@��9@�bN@�A�@�1'@�  @�\)@��@���@��H@���@�v�@�@���@�hs@�?}@��@���@�(�@���@�C�@��@�V@���@���@�`B@��`@�Ĝ@�r�@� �@��@��F@�dZ@�+@��y@��R@���@�V@��@���@��h@���@���@��@�Q�@��@��m@���@�;d@��@��y@��@���@�M�@�@��#@��h@�O�@�&�@���@�1'@��@��
@�C�@��@���@�~�@�E�@�J@���@�V@�Ĝ@�A�@��w@�|�@�;d@���@���@�v�@�$�@���@��7@���@���@�r�@�Q�@�I�@�I�@�A�@�1'@�A�@� �@�ƨ@��P@�l�@��y@��+@�5?@�{@��#@���@��-@�O�@�V@��j@�(�@��P@�l�@�dZ@�33@��@��!@��+@�$�@��@�@��^@���@�x�@�O�@�7L@��@�z�@� �@��w@�+@�o@�@���@��H@���@���@��\@��\@�v�@�5?@��#@���@�7L@���@��D@�I�@�  @��m@��@�
=@���@���@�n�@�^5@�=q@��@��^@��@�/@��j@�1'@��@�@�@��@l�@
=@~$�@}�@|��@|�/@|��@|Z@|(�@{�m@{t�@{"�@z��@zn�@z^5@y�#@y�@x�9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�33A�1'A�(�A�&�A�(�A�"�A�&�A�33A�+A�&�A�+A�"�A�&�A�"�A� �A�$�A� �A�"�A�&�A�"�A�(�A�-A�$�A�$�A�-A�&�A�+A�$�A��A�$�A� �A� �A�bA�1A�{A�VA�JA��A��A�A�A��A���A���A���A��A��mA��/A��A��#A��TA��;A��mA��A��A���A�A̺^A̼jA̾wA̺^A̲-A̺^A̰!A̰!A̲-A̶FA̮A̰!A̶FA̧�A̩�A̲-A̮A̮A̰!Ạ�ȂhA̍PȦ+A̍PẢ7Ȧ+A̍PȦ+Ȧ+Ȧ+ÃẢ7ÃA̅Ả7ÃA̅A̅ÁA̅ÁA�~�ÃA�~�A�|�ÁA�|�A�z�A�~�A�|�A�z�ÁA�z�A�|�A�~�A�x�A�~�A�z�A�z�A�|�A�x�A�z�A�z�A�t�A�z�A�v�A�t�A�z�A�t�A�v�A�x�A�t�A�t�A�x�A�v�A�r�A�r�A�t�A�p�A�p�A�t�A�l�A�l�A�p�A�l�A�n�A�hsA�jA�jA�ffA�n�A�n�A�hsA�l�A�n�A�l�A�p�A�r�A�n�A�t�A�r�A�r�A�v�A�p�A�r�A�v�A�v�A�r�A�v�A�x�A�t�A�v�A�z�A�x�A�x�A�|�A�x�A�x�A�~�A�~�A̅Ả7A̅A̅A̍PA̋DẢ7A̋DȀ\Ả7A̍PȀ\A̋DA̋DȀ\Ȁ\A̓uA̙�A̝�A̝�Ạ�A̮A̮A̺^A�ĜA�ĜA̶FA̛�ÃA�v�A�l�A�^5A�M�A�?}A�5?A��A��A�/A�C�A�jA�ƨA�JA�G�A�hsA�t�A�hsA�VA�K�A�7LA�&�A�oA���A��TA�A̛�Ả7A�|�A�x�A�p�A�p�A�p�A�jA�hsA�dZA�bNA�\)A�ZA�XA�S�A�O�A�S�A�Q�A�1'A�%A�VA�oA�
=A��A��A��A�"�A� �A��A�{A�bA�A�  A���A��`A��/A��HA��
A�ȴA�ĜA�ĜA�A˾wA���A�ĜA�ȴA���A���A���A�ȴA�AˋDA�-A� �A��;Aʏ\A�l�A�x�Aʇ+AʍPAʧ�Aʰ!AʸRAʼjA�ƨA�ƨAʲ-Aʕ�Aʛ�AʮA��mA�  A�oA�VA�A���A��A��A��A��yA��Aʧ�A�v�A��AɬAɗ�Aə�Aɗ�AɅAɁA�~�A�v�A�S�A�?}A�5?A�33A�+A�(�A�+A��A�oA�{A�
=A���A��/A��
A�ȴAȰ!Aȥ�Aȟ�Aș�A�x�A�I�A�(�A�  AǓuA��A��yAƩ�A�~�A�^5A�5?A�  A���A��A�ĜA�A���AŶFAţ�AőhAŉ7A�dZA�%A��A���Aĥ�A�
=Aç�A�M�A�`BA�bNA��A�1A��yA��-A���A��PA�jA�&�A��A���A��9A���A�I�A��A� �A�9XA�A�A�?}A�K�A�M�A�E�A�;dA�33A�&�A��A��9A���A�v�A�p�A�ffA�I�A�7LA�7LA�1'A� �A� �A�5?A�7LA�A�  A�  A�A�A���A���A��A��A���A���A��RA��7A�n�A�ZA�M�A� �A��
A�z�A�G�A�7LA�-A�7LA�9XA�A�A�K�A�?}A�/A�$�A�VA���A��mA�A���A�x�A�-A���A�&�A���A�x�A�5?A�%A��#A��hA�dZA�{A�p�A��+A�33A��A��RA�XA�-A���A��A�ȴA��wA���A���A��PA�n�A�XA��A���A��A��
A���A�Q�A�5?A�$�A�"�A��A�jA�&�A�  A��;A��wA���A��7A��A�l�A�\)A�Q�A�A�A�-A�&�A��A�{A�bA�
=A��A��wA�x�A�\)A�-A�$�A� �A���A���A��\A��DA��hA���A��PA��DA��7A�~�A�ffA�&�A��A���A���A�r�A�`BA�M�A�K�A�G�A�%A��;A��A���A��RA��uA��7A�p�A�VA�=qA�1A��A��;A�ƨA��RA���A���A���A���A��PA��A�r�A�bNA�O�A�=qA�1'A��A��A��A�%A���A��A��;A�ƨA���A��PA�z�A�n�A�VA�A�A�33A�(�A��A�A��A�ƨA��^A���A�VA���A�ZA�ƨA�z�A�9XA�  A���A��A�jA�`BA�\)A�ZA�XA�Q�A�I�A�1'A���A�l�A�(�A��A�1'A��A��\A�dZA�33A�1A�ƨA��A�ZA��A���A�t�A�9XA��A��A��TA���A���A��A�t�A�G�A�v�A���A��hA�K�A�-A�&�A��A�JA�1A���A��mA���A��
A�ƨA�A��RA��FA��9A��!A���A���A���A���A�n�A�1'A��
A���A�z�A�C�A���A��A���A���A���A��A�E�A�{A��A�(�A��RA���A�XA�{A��`A���A�?}A� �A�bA��A�ƨA��FA���A�ffA�C�A��A�A���A��A��yA���A�~�A�l�A�`BA�XA�O�A�?}A�1'A��A�oA�bA�A��A��A���A���A��A��DA�|�A�VA�I�A� �A��`A��-A�VA��yA���A��A�dZA�9XA�$�A�JA��A��;A�ĜA�XA���A�A�I�A��A���A��;A��^A��!A��A���A���A���A��hA��7A��A�jA�ZA�5?A���A��hA��A�|�A�x�A�x�A�x�A�v�A�jA�\)A�-A��yA���A�\)A��A�`BA�oA��A�1'A��TA���A��hA��DA�z�A�;dA��A���A�=qA��;A�n�A��^A��DA�hsA�JA��A�p�A�ZA���A�|�A�hsA�ffA�XA�Q�A�I�A�M�A��A�Q�A�M�A��A���A���A��A��/A���A���A��A�I�A�$�A�bA�JA�r�A�E�A�&�A��A���A��9A���A��A�p�A�bNA�Q�A�=qA�/A��A���A��yA��^A���A�r�A�S�A�?}A� �A�
=A�AƨA�hAdZA~��A}�mA|�A|  A{��A{�Az  AxZAw�hAv��AvjAu�Au;dAt��As�AsK�Ar��ArffAq�#Aq��Aq�hAq|�AqG�Ap��Ap�Aq
=Ap��Ap��ApȴApĜApĜApĜApĜAp�Ap��Ap�Ap��Ap�uAp�DAp�DAp�\Ap�\ApffAp �AnM�AmK�Alr�Ak�Ak`BAk�Aj�Aj�yAj�`Aj��Aj��Aj�Ajv�Ai�Ai7L1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�+A�(�A�+A�"�A�"�A�&�A�&�A�&�A�"�A�VA�JA�%A��A��HA̼jA̲-A̮A̗�Ȧ+A̅ÃA�~�A�|�A�z�A�x�A�v�A�r�A�n�A�jA�l�A�r�A�v�A�x�ÁA̋DA̍PA̡�A̧�A�I�A�l�A�XA���A�x�A�bNA�?}A��A�{A��;A�A˺^Aʲ-AʶFAʴ9A�`BA��;A�
=A�9XA�bA�S�A��\A�JA�1'A��A���A���A�"�A�M�A�+A��yA��yA���A���A��;A�ĜA���A��-A���A�/A���A�$�A��7A��A�l�A���A�A�G�A��A�A��!A�bA�;dA���A���A���A�(�A��`A��A�v�A���A~r�Aw�7ArVAp�Ap�uAlI�AiXAf=qAb��A_"�A^ �AY�TAX�DAVjAP��AO\)ANbNAL�jAKt�AI��AH�RAF5?AC��ABVAA��AAS�A@��A>��A<�A<$�A<bA;�#A;/A:A�A9t�A7p�A6-A4�HA2��A/VA-�TA-�hA-�A,�RA,�\A,VA,{A+O�A)\)A(  A'�-A';dA&$�A$M�A#�hA"��A"^5A!�A!?}A M�A~�A��AK�A�`Av�A��AZA��A�HA�AffAƨAO�A�+AK�A-A�A+A
jA	A��A�\A�TAS�A�A��AQ�A^5An�AoA/A��A�!Av�AZAE�A�A�mA��AAjA��A ��A 1@�+@�E�@�p�@��u@��@��R@��F@���@��@�$�@��j@��;@�=q@��@�F@�n�@���@��T@��@�(�@�K�@�-@噚@��@�h@�5?@���@�Z@�b@��@��@�^@���@�S�@�w@�7@�Q�@�@�`B@�1'@�@ڟ�@�~�@�$�@؛�@���@� �@�r�@ج@��@և+@ա�@�S�@�{@щ7@�G�@��`@���@���@ЋD@� �@�dZ@�@�~�@͑h@̣�@�  @�o@�ȴ@�ff@�$�@�x�@�1'@��@�ƨ@ǝ�@�|�@�33@�o@ƸR@�=q@��@���@ź^@őh@ě�@+@�@��-@�X@��@���@�z�@�1'@��@�  @�ƨ@��P@�\)@�+@��@��!@�{@��@���@��u@�bN@���@�"�@���@�v�@�{@�x�@��@��`@��@�Q�@�  @��F@�\)@��@�$�@�G�@���@���@��m@�C�@��y@��+@��@��@��T@���@��-@��-@�x�@��@��u@���@���@��P@��P@��@�33@��y@���@��+@�$�@��^@�hs@��@�j@��@�  @��;@���@�o@���@�v�@�n�@�^5@��@���@�`B@�X@�O�@�7L@�&�@��@��9@�bN@�A�@�1'@�  @�\)@��@���@��H@���@�v�@�@���@�hs@�?}@��@���@�(�@���@�C�@��@�V@���@���@�`B@��`@�Ĝ@�r�@� �@��@��F@�dZ@�+@��y@��R@���@�V@��@���@��h@���@���@��@�Q�@��@��m@���@�;d@��@��y@��@���@�M�@�@��#@��h@�O�@�&�@���@�1'@��@��
@�C�@��@���@�~�@�E�@�J@���@�V@�Ĝ@�A�@��w@�|�@�;d@���@���@�v�@�$�@���@��7@���@���@�r�@�Q�@�I�@�I�@�A�@�1'@�A�@� �@�ƨ@��P@�l�@��y@��+@�5?@�{@��#@���@��-@�O�@�V@��j@�(�@��P@�l�@�dZ@�33@��@��!@��+@�$�@��@�@��^@���@�x�@�O�@�7L@��@�z�@� �@��w@�+@�o@�@���@��H@���@���@��\@��\@�v�@�5?@��#@���@�7L@���@��D@�I�@�  @��m@��@�
=@���@���@�n�@�^5@�=q@��@��^@��@�/@��j@�1'@��@�@�@��@l�@
=@~$�@}�@|��@|�/@|��@|Z@|(�@{�m@{t�@{"�@z��@zn�@z^5@y�#@y�@x�9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�33A�1'A�(�A�&�A�(�A�"�A�&�A�33A�+A�&�A�+A�"�A�&�A�"�A� �A�$�A� �A�"�A�&�A�"�A�(�A�-A�$�A�$�A�-A�&�A�+A�$�A��A�$�A� �A� �A�bA�1A�{A�VA�JA��A��A�A�A��A���A���A���A��A��mA��/A��A��#A��TA��;A��mA��A��A���A�A̺^A̼jA̾wA̺^A̲-A̺^A̰!A̰!A̲-A̶FA̮A̰!A̶FA̧�A̩�A̲-A̮A̮A̰!Ạ�ȂhA̍PȦ+A̍PẢ7Ȧ+A̍PȦ+Ȧ+Ȧ+ÃẢ7ÃA̅Ả7ÃA̅A̅ÁA̅ÁA�~�ÃA�~�A�|�ÁA�|�A�z�A�~�A�|�A�z�ÁA�z�A�|�A�~�A�x�A�~�A�z�A�z�A�|�A�x�A�z�A�z�A�t�A�z�A�v�A�t�A�z�A�t�A�v�A�x�A�t�A�t�A�x�A�v�A�r�A�r�A�t�A�p�A�p�A�t�A�l�A�l�A�p�A�l�A�n�A�hsA�jA�jA�ffA�n�A�n�A�hsA�l�A�n�A�l�A�p�A�r�A�n�A�t�A�r�A�r�A�v�A�p�A�r�A�v�A�v�A�r�A�v�A�x�A�t�A�v�A�z�A�x�A�x�A�|�A�x�A�x�A�~�A�~�A̅Ả7A̅A̅A̍PA̋DẢ7A̋DȀ\Ả7A̍PȀ\A̋DA̋DȀ\Ȁ\A̓uA̙�A̝�A̝�Ạ�A̮A̮A̺^A�ĜA�ĜA̶FA̛�ÃA�v�A�l�A�^5A�M�A�?}A�5?A��A��A�/A�C�A�jA�ƨA�JA�G�A�hsA�t�A�hsA�VA�K�A�7LA�&�A�oA���A��TA�A̛�Ả7A�|�A�x�A�p�A�p�A�p�A�jA�hsA�dZA�bNA�\)A�ZA�XA�S�A�O�A�S�A�Q�A�1'A�%A�VA�oA�
=A��A��A��A�"�A� �A��A�{A�bA�A�  A���A��`A��/A��HA��
A�ȴA�ĜA�ĜA�A˾wA���A�ĜA�ȴA���A���A���A�ȴA�AˋDA�-A� �A��;Aʏ\A�l�A�x�Aʇ+AʍPAʧ�Aʰ!AʸRAʼjA�ƨA�ƨAʲ-Aʕ�Aʛ�AʮA��mA�  A�oA�VA�A���A��A��A��A��yA��Aʧ�A�v�A��AɬAɗ�Aə�Aɗ�AɅAɁA�~�A�v�A�S�A�?}A�5?A�33A�+A�(�A�+A��A�oA�{A�
=A���A��/A��
A�ȴAȰ!Aȥ�Aȟ�Aș�A�x�A�I�A�(�A�  AǓuA��A��yAƩ�A�~�A�^5A�5?A�  A���A��A�ĜA�A���AŶFAţ�AőhAŉ7A�dZA�%A��A���Aĥ�A�
=Aç�A�M�A�`BA�bNA��A�1A��yA��-A���A��PA�jA�&�A��A���A��9A���A�I�A��A� �A�9XA�A�A�?}A�K�A�M�A�E�A�;dA�33A�&�A��A��9A���A�v�A�p�A�ffA�I�A�7LA�7LA�1'A� �A� �A�5?A�7LA�A�  A�  A�A�A���A���A��A��A���A���A��RA��7A�n�A�ZA�M�A� �A��
A�z�A�G�A�7LA�-A�7LA�9XA�A�A�K�A�?}A�/A�$�A�VA���A��mA�A���A�x�A�-A���A�&�A���A�x�A�5?A�%A��#A��hA�dZA�{A�p�A��+A�33A��A��RA�XA�-A���A��A�ȴA��wA���A���A��PA�n�A�XA��A���A��A��
A���A�Q�A�5?A�$�A�"�A��A�jA�&�A�  A��;A��wA���A��7A��A�l�A�\)A�Q�A�A�A�-A�&�A��A�{A�bA�
=A��A��wA�x�A�\)A�-A�$�A� �A���A���A��\A��DA��hA���A��PA��DA��7A�~�A�ffA�&�A��A���A���A�r�A�`BA�M�A�K�A�G�A�%A��;A��A���A��RA��uA��7A�p�A�VA�=qA�1A��A��;A�ƨA��RA���A���A���A���A��PA��A�r�A�bNA�O�A�=qA�1'A��A��A��A�%A���A��A��;A�ƨA���A��PA�z�A�n�A�VA�A�A�33A�(�A��A�A��A�ƨA��^A���A�VA���A�ZA�ƨA�z�A�9XA�  A���A��A�jA�`BA�\)A�ZA�XA�Q�A�I�A�1'A���A�l�A�(�A��A�1'A��A��\A�dZA�33A�1A�ƨA��A�ZA��A���A�t�A�9XA��A��A��TA���A���A��A�t�A�G�A�v�A���A��hA�K�A�-A�&�A��A�JA�1A���A��mA���A��
A�ƨA�A��RA��FA��9A��!A���A���A���A���A�n�A�1'A��
A���A�z�A�C�A���A��A���A���A���A��A�E�A�{A��A�(�A��RA���A�XA�{A��`A���A�?}A� �A�bA��A�ƨA��FA���A�ffA�C�A��A�A���A��A��yA���A�~�A�l�A�`BA�XA�O�A�?}A�1'A��A�oA�bA�A��A��A���A���A��A��DA�|�A�VA�I�A� �A��`A��-A�VA��yA���A��A�dZA�9XA�$�A�JA��A��;A�ĜA�XA���A�A�I�A��A���A��;A��^A��!A��A���A���A���A��hA��7A��A�jA�ZA�5?A���A��hA��A�|�A�x�A�x�A�x�A�v�A�jA�\)A�-A��yA���A�\)A��A�`BA�oA��A�1'A��TA���A��hA��DA�z�A�;dA��A���A�=qA��;A�n�A��^A��DA�hsA�JA��A�p�A�ZA���A�|�A�hsA�ffA�XA�Q�A�I�A�M�A��A�Q�A�M�A��A���A���A��A��/A���A���A��A�I�A�$�A�bA�JA�r�A�E�A�&�A��A���A��9A���A��A�p�A�bNA�Q�A�=qA�/A��A���A��yA��^A���A�r�A�S�A�?}A� �A�
=A�AƨA�hAdZA~��A}�mA|�A|  A{��A{�Az  AxZAw�hAv��AvjAu�Au;dAt��As�AsK�Ar��ArffAq�#Aq��Aq�hAq|�AqG�Ap��Ap�Aq
=Ap��Ap��ApȴApĜApĜApĜApĜAp�Ap��Ap�Ap��Ap�uAp�DAp�DAp�\Ap�\ApffAp �AnM�AmK�Alr�Ak�Ak`BAk�Aj�Aj�yAj�`Aj��Aj��Aj�Ajv�Ai�Ai7L1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�B	�OB	��B	��B	��B	�}B	�}B	��B	��B	��B	��B	��B	��B	�B	��B	�XB	�jB	�B	�wB	�B	��B	��B	�'B	�-B	��B	�gB	�mB	��B	�B	��B	�0B	�jB	�BB	�TB	�[B	�B
A�B
� B
�tBVBd�B_;Bi�B��B�'B�B�)BیB��B�vB)�B�B��B�8B�zB��BT�BW?B\]B�B�nB��B�B��B{�B^�BK^B:�BB�BLdBR�BS�BY�BD�B1[BFB��B�TB�B�B��B��B~(BtTBkBW�BB[B(�B!B"B
�fB
��B
��B
iDB
J�B
-B
(B
B	�`B	�EB	�?B	�$B	��B	��B	��B	v�B	q�B	\�B	VB	K�B	<B	6�B	)*B	1B	�B	\B	fB	�B�]B�DB��B�B�>B��B�&B�BޞBخB�yBרB�
B҉B��B��B�zBB��B��B��B�*B��B�B�RB��B��B��B�6B��B�tB�FB�OB��BбB��BרB�BרBںBیB��BیBٴB�mB�B��B��B�RB�FB��B� B�.B��B�B�=B�	B��B��B��B�B��B�1B��B��B�4B��B��B�+B�uB��B��B��B˒B�BΥB�}BѷB�,B��B�9BԕB�#BߤB��B��B�,B�ZB�B�>B��B�&B��B��B��B�DB�,B�sB�)B�GB�%B��B	 �B	hB	�B	_B	�B	7B	!�B	1�B	0!B	0UB	2aB	0!B	1[B	6�B	8RB	FtB	NB	R�B	M�B	N<B	LdB	J�B	LdB	OvB	QNB	R�B	S�B	T,B	W
B	X�B	_;B	`B	_�B	`BB	_pB	]dB	^B	^jB	aB	b�B	c�B	f�B	qvB	v�B	t�B	s�B	r|B	p�B	n�B	poB	q�B	t�B	v�B	|B	zDB	y�B	zB	|�B	��B	�AB	��B	��B	�B	�MB	��B	��B	��B	�1B	�PB	�"B	��B	��B	��B	�FB	�$B	��B	�1B	�B	��B	�=B	��B	�IB	�OB	��B	�nB	��B	��B	��B	��B	�kB	��B	��B	��B	��B	��B	�-B	��B	�aB	��B	��B	��B	��B	�*B	��B	�BB	��B	�B	�[B	ĜB	��B	�B	�B	��B	��B	��B	�dB	��B	�B	͟B	�B	�HB	��B	��B	бB	�NB	ӏB	��B	�,B	��B	�gB	�?B	�?B	�EB	��B	�B	�B	�B	��B	�)B	یB	�WB	یB	��B	�5B	��B	�pB	�BB	�BB	��B	��B	�B	�B	�`B	��B	�DB	��B	�B	�B	��B	�B	�]B	�B	�B	�]B	��B	��B	�cB	��B	�;B	�B	�B	�B	��B	�+B	��B	�B	��B	��B	�>B	�DB	��B	�JB	��B	��B	��B	�VB	�VB	�]B	�.B	�]B	�.B	�cB	�cB
 iB
;B
B
B
�B
uB
AB
�B
B
�B
SB
B
�B
�B
	B

=B
	�B

	B
	7B
	7B

=B

=B

	B

rB
�B
�B
�B
�B
�B
(B
�B
�B
�B
4B
hB
hB
�B
�B
uB
uB
B
FB
FB
{B
�B
�B
�B
YB
�B
�B
kB
�B
�B
kB
�B
xB
IB
B
B
B
�B
�B
�B
�B
B
�B
 'B
 \B
 �B
 �B
!�B
!�B
"hB
"�B
#B
#nB
#nB
#�B
#�B
"�B
"�B
#:B
#�B
#�B
#�B
#�B
$@B
$�B
%zB
%zB
%zB
%�B
&B
%�B
&�B
'�B
'�B
(XB
'�B
(�B
(XB
)*B
)�B
)�B
)�B
)�B
)�B
*eB
+B
+�B
,=B
,=B
,�B
,qB
,B
,B
,B
,=B
,B
,B
-�B
.B
.}B
.}B
/�B
/OB
/B
/�B
0UB
0�B
0�B
1'B
1'B
2aB
2�B
33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A³hA³�B	�UB	�UB	��B	��B	��B	��B	��B	�'B	�[B	�wB	��B	�}B	��B	�UB	�IB	�!B	��B	�B	��B	�wB	�B	��B	��B	�=B	��B	�B	�IB	��B	�B	�B	��B	�-B	��B	��B	�B	�'B	�B	��B	��B	��B	��B	��B	�'B	��B	�9B	��B	�B	��B	�B	��B	�9B	��B	�!B	�B	��B	��B	��B	�RB	�FB	�B	�dB	�FB	��B	�$B	��B	��B	��B	��B	�RB	�^B	��B	�LB	�B	��B	��B	��B	��B	�B	�wB	�B	��B	�BB	��B	��B	��B	�BB	�HB	�B	�HB	�wB	�qB	�}B	��B	�BB	�B	�B	�HB	�OB	��B	�B	�OB	��B	�OB	� B	�HB	��B	�UB	�}B	�'B	�UB	��B	��B	�UB	�'B	��B	�UB	ÖB	�[B	�[B	�gB	�[B	��B	�3B	B	��B	ÖB	ÖB	��B	ĜB	�aB	��B	�mB	�9B	�aB	�B	�mB	��B	�?B	�mB	�3B	��B	�B	�B	�tB	�?B	ǮB	ƨB	��B	ɺB	ǮB	��B	��B	�B	�#B	�B	ɺB	̘B	�dB	��B	̘B	��B	�)B	�0B	�jB	�dB	��B	��B	͟B	�dB	�jB	��B	�0B	�pB	�pB	�dB	ϫB	�B	уB	� B	� B	��B	� B	��B	�[B	� B	�aB	ҽB	�[B	ӏB	��B	уB	�2B	�9B	�EB	��B	�&B	�B	�B	�PB
�B
 B
�B
A�B
e�B
x8B
|�B
��B
�lB
��B
��B
��B
�-B
�-B
��B
�$B
�&B
�B	lB2-BL�B\�Bf2BiBf�BgmBg8Bd�Bc�Be�Bd&B`�B^�B^jB]�B_�B`BB`�BbNBcTBe,Bg8Bl�BpBw�B�B��B��B��B��B��B�_B��B��B��B��B�qB�BB�tBɺB��B�<B�KB��BچB�#B��BچB�/B��B��B�WB��BܒB��BںB�B�NB�|B�B�B��B� B��B�B�B�B�B�B�BB	B\BSB�B)_B/OB6B4B4�B0�B[WB`�BbNBz�B}�B~�B�iB�oB��B�B��B��B��B��B��B�zB��B��BɺB�B�pB�[B��B��B�B��B�B��B��B��B�B�fB��B��B�yB�B�B�B�8B�B��B��B��B��B��B��BѷB�'B��B�UB��B�CB��B�\B��B��B�~B�4B��B��B�RB��B��B��B�YB��B��B�B~�B��B~�Br�BC�BB�BH�BEBB�B>BBE�BPBV�BU�BM�BS�Ba�BOvBJ�BOBXyBZBX�B^�B_�B`�B^�B\]Bc�Bd�B^BV9BT,B^B^�BW�BZ�B\�B^jBY�Bh
BjB�.B��B�hB��B��B��B��B�B�OB��B�7B��B�wB�B��B��B�VB�*B��B�B�B�7B�kB�bB�hB��B�6B��B��B��B�6B�XB�qB��B�B�wB�tB�6B��B�'B��B��B��B�RB�!B�B��B��B��B��B��B��B��B�DB�B�"B�YB��B��B��B��B��BzxBy�BqABqvBo5Bg8BaB[#Bd&Bo�Bk�B`B[WBZ�BW?BU�BS�BP}BQNBP}BN<BM6BMjBIBI�BI�BF�BD�BIRBJ�BD3B=B<jB6�B3hB9�B:^B7�B8RB9�B9$B;�B9XB8�B@BE�BE�B?BE9BK�BF�B@�BEBC-BD�BUgBIRBH�BI�BK�BJ�BI�BL�BK)BP�BR�BPHBS�BS�BQNBQ�BR�BQ�BQBS�BS[BS�BS�BTaBO�BR BS�BN�BPBT�BR�BP�BR�BVmBW
BXEBV�BY�BY�BZ�BYBWsBYBXEBXyBVBT,BX�BWsB_�Bk�BS�BK^BHKBGEBO�BD�B<�B=<B<�B9�B8�B6FB7�B7BG�B4�B3�B5?BCaB0UB�B#B�B!BB�B�B#nB�B�B�BBDBfB_BBB��BSB�B��B�PB�B�WB�B�>B�ZB�
B�B��B��B�HB�&B�B�B�B�5B��B�;B�#B�WBچB��B�jB��B�B�&B��B�BȀB�jB��B��B�tB��B��B��B��B�'B�'B��B��B��B��B��B��B�VB��B��B�lB��B��B�xB��B�GB�B�;B~�B��B�7Bz�B{By>By�Bw2Bx8Bv�Bu�BtBsMBu�Bq�Bo5Bm]Br|Bq�Bm�Bl"BffBgBkQBd&Bt�BZ�BYBWsB[�BPBK)BM�BIRBK^BH�BP�BD�B;�BK�B5B*eB49B.�B'RB$B&�B&�B(�B#�B#nB#�B&�B \B�B%zB%BB�B B�BxB	lBxB	�B"B	�B+6B
�B
�B
��B
�B
�mB
�B
ںB
�6B
ȀB
�B
ŢB
��B
�3B
��B
��B
�RB
�IB
��B
�VB
��B
�SB
��B
t�B
}VB
��B
��B
f�B
ffB
bB
l�B
k�B
l�B
yrB
]dB
V�B
U�B
I�B
GB
GEB
JXB
A B
H�B
K�B
1'B
DgB
4�B
(XB
UgB
�B
!B
"4B
{B
�B
B
.B
�B
JB
B
	7B
�B
�B
�B
�B
B
{B
B
 4B	��B
 �B	��B	�B	��B	�2B	�B	��B
�B	�	B	�sB	�`B	�|B	�B	�WB	��B	�B	�)B	�#B	�BB	��B	�B	�B	�<B	��B	�6B	�kB	�B	�IB	��B	�B	�B	��B	�eB	�B	��B	�$B	��B	�zB	�tB	��B	�B	�B	��B	�4B	��B	�'B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�+B	��B	�B	��B	��B	�_B	�{B	��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                        44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B	�B	�RB	��B	�B	��B	�6B	��B	��B	�B	�BB	��B	��B	� B	��B	�mB	�B	��B	��B	�^B	��B	�dB	�6B	�B	�wB	�}B	�NB	��B	��B	�,B	�mB	�B	ȀB	ɺB	˒B	ΤB	ϫB	�TB
>BB
�PB
��BRTB`�B[�Be�B�B�wB�QB�yB��B�GB��B%�B�oB�&B�B��B�BQBS�BX�B�eB��B��B�RB�Bx7BZ�BG�B7B>�BH�BOBPHBVBA B-�B�B�1BߤB�mB�^B��B��BzxBp�BglBS�B>�B%BqB
rB
�B
��B
��B
e�B
GB
)^B
xB
 iB	�B	ԕB	��B	�tB	�B	��B	�B	sB	n.B	YKB	RTB	HB	8RB	33B	%zB	�B	�B	�B	�B	 4B��B��B�AB��B�B�B�vB�iB��B��B��B��B�ZB��B�/B�B��B��B�NB�0B��B�zB�EB�mB��B�B�B�)B��B��B��B��B��B�B�B�B��B�gB��B�
B��B�EB��B�BҽB�TB��B��B��B��B��B�PB�~B��B�kB��B�YB��B�+B��B�SB�B��B��B�=B��B�B�B�{B��B�.B��B�B��B�WB��B��B�B�|B�NB҉B��B�sB��B�AB�B�|B�B��B�B�>B�vB�JB�B��B�B�|B��B�yB�B�uB�MB�"B	�B	�B	�B	�B	�B	�B	-�B	,qB	,�B	.�B	,qB	-�B	2�B	4�B	B�B	JWB	OBB	I�B	J�B	H�B	GEB	H�B	K�B	M�B	OB	PB	P|B	SZB	T�B	[�B	\]B	\)B	\�B	[�B	Y�B	ZQB	Z�B	]cB	_;B	`B	b�B	m�B	sB	p�B	p;B	n�B	l�B	j�B	l�B	n.B	qB	sMB	xlB	v�B	v+B	v`B	y	B	}"B	~�B	~(B	.B	�iB	��B	��B	�:B	��B	��B	��B	�rB	��B	��B	�!B	��B	�tB	�B	��B	�RB	�$B	��B	��B	��B	��B	�CB	��B	�B	�FB	��B	�FB	��B	�0B	�B	�6B	��B	�B	�}B	�IB	��B	�B	�OB	�-B	��B	�zB	�B	��B	��B	�dB	��B	��B	� B	�[B	�gB	�9B	�?B	�EB	ȴB	�KB	�QB	��B	�^B	̘B	�5B	�5B	�B	͞B	��B	�B	�|B	�B	ѷB	ӏB	ӏB	ԕB	�8B	�mB	�mB	�mB	�>B	�yB	��B	קB	��B	�B	څB	�B	��B	ܒB	ܒB	�/B	�/B	�B	��B	�B	�%B	�B	�2B	�B	�fB	�8B	��B	�B	��B	��B	�B	�JB	�JB	�B	�"B	�B	�cB	�iB	�B	�GB	�{B	��B	�SB	��B	��B	��B	��B	��B	��B	�B	�7B	�>B	��B	��B	��B	�~B	��B	�~B	��B	��B	��B	��B	�VB	�VB	��B	��B	��B
  B
oB
B
�B
oB
�B
�B
SB
�B
�B
YB
�B
�B
�B
�B
YB
�B
	B

	B

=B

	B
	�B
xB
IB
IB
�B
�B
�B
�B
�B
�B
�B
�B
\B
�B
�B
�B
�B
4B
B
�B
B
�B
�B
$B
�B
�B
*B
�B
�B
eB
eB
kB
B
B
0B
7B
kB
�B
wB
�B
�B
B
OB
OB
�B
!B
UB
�B
�B
�B
 'B
�B
�B
�B
�B
�B
 'B
 'B
 �B
!-B
!�B
!�B
!�B
"3B
"hB
"3B
"�B
#�B
$@B
$�B
$@B
$�B
$�B
%zB
&LB
&B
&B
&LB
&LB
&�B
'RB
'�B
(�B
(�B
)*B
(�B
(XB
(XB
(XB
(�B
(XB
(XB
)�B
*dB
*�B
*�B
+�B
+�B
+kB
+�B
,�B
-B
-BB
-wB
-wB
.�B
.�B
/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A¬	A¬qB	��B	��B	��B	�6B	�$B	�B	�0B	�wB	��B	��B	�B	��B	�B	��B	��B	�qB	�B	�dB	��B	��B	�dB	��B	�6B	��B	��B	�dB	��B	��B	�^B	�kB	��B	�}B	�B	��B	�kB	�wB	�dB	�6B	�'B	��B	��B	��B	�wB	�OB	��B	�OB	�gB	�BB	�aB	�-B	��B	�B	�qB	�UB	�KB	�-B	�9B	��B	��B	�mB	��B	��B	�B	�tB	�B	�B	�B	�B	��B	��B	�B	��B	�RB	�?B	��B	��B	�B	�XB	��B	�RB	��B	��B	��B	��B	�)B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	�jB	�^B	��B	��B	��B	�jB	��B	�0B	��B	�pB	��B	��B	��B	��B	�wB	��B	��B	�HB	��B	�wB	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	� B	��B	��B	� B	��B	��B	�B	��B	��B	��B	�UB	��B	�B	B	��B	��B	�,B	�[B	�gB	��B	B	��B	��B	�3B	�
B	��B	�9B	�B	�mB	�sB	�QB	�
B	��B	ȴB	�EB	��B	�B	�yB	ȀB	ɺB	ȴB	�B	�#B	��B	ȴB	ɺB	�#B	ȀB	��B	��B	ȴB	��B	�WB	��B	�pB	�pB	�/B	�pB	�HB	ϫB	�pB	бB	�B	ϫB	��B	�HB	��B	тB	҉B	ԕB	�KB	�vB	��B	�]B	��B
B
PB
B
>B
bB
t�B
x�B
�B
��B
��B
�:B
��B
�}B
�}B
�B
�tB
�vB
�gB�B.}BH�BYKBb�Be`BcBc�Bc�BaB`ABbB`vB\�BZ�BZ�BZB\)B\�B\�B^�B_�Ba|Bc�BiBlWBs�B�oB��B��B�B�B�B��B�B��B�IB�KB��B��B��B�
B�BʌB՛B�8B��B�sB�EB��B�B�B�BקB�B��B�EB�
B�mBޞB��B�iB��B�AB�PB�GB��B�oB��B�`B��B�SB�bBSB�B�B=B%�B+�B2aB0UB0�B-BW�B]/B^�Bw1By�Bz�B|�B}�B}"B}VB�B�1B��B�B�IB��B�3B�#B�
B�QB��BϫB�GB�2B��B�B��B�B�NB�%B��B�B�%B�,B��B��B��B��B�B�TB�NB�2B�8B�,B�AB�GB�B�wB�0B��B�B��B�FB��B��B��B��B��B�B��B��B��B��B�B��B��B�B�hB{JB}"Bz�Bo B?�B?BEBAUB?B:�BA�BLdBS&BR BJ#BO�B^5BK�BGEBK^BT�BVmBU2BZ�B[�B\�B[#BX�B_�BaGBZQBR�BP|BZQB[#BT,BW>BYKBZ�BVBdZBf�B�~B�B��B�IB�B�B�B�UB��B�3B��B�7B��B�UB�B�!B��B�zB��B�kB�eB��B��B��B��B�3B��B��B�LB��B��B��B��B�*B�kB��B��B��B�$B�wB�3B��B��B��B�qB�[B��B�B��B��B�B��B�!B��B�SB�rB��B� B��B�B�:B�Bv�Bv+Bm�Bm�Bk�Bc�B]cBWsB`vBl"Bg�B\]BW�BW>BS�BR BPHBL�BM�BL�BJ�BI�BI�BEmBE�BE�BC,BA BE�BGEB@�B9XB8�B33B/�B6B6�B3�B4�B5�B5tB8B5�B5?B<jBB&BA�B;dBA�BG�BB�B=<BAUB?}B@�BQ�BE�BE9BF?BG�BF�BE�BIBGyBM5BN�BL�BO�BPBM�BM�BN�BM�BMjBPHBO�BO�BPBP�BL/BNpBO�BK)BLdBP�BOBM5BOBBR�BSZBT�BS&BVBVBV�BUgBS�BU�BT�BT�BRTBP|BT�BS�B[�Bh>BPBG�BD�BC�BL/BA B9#B9�B8�B6EB5?B2�B4B3gBC�B1'B0 B1�B?�B,�B�BUB�BqB_BFB�B�BBBBeB�B�B�B	kB�VB��B�BB�MB��B��B�B�TB�B�B�ZB��B�>B�2BݘB�vB�B�cB�]BڅB�KBۋB�sBקB��B�)BںB�KB�dB�vB�<B�`B��B��B��B��B��B�B�'B�B�)B�wB�wB�B��B�B�B�4B��B��B�	B��B��B�!B�+B��B�B�B}VB}�Bz�B~�B��Bv�Bw�Bu�Bu�Bs�Bt�BsMBq�BpoBo�BrBn.Bk�Bi�Bn�Bm�Bi�BhrBb�BcTBg�B`vBqABW>BUgBS�BXEBLdBGyBJ#BE�BG�BEBMB@�B7�BHKB1[B&�B0�B+B#�B [B#9B#9B$�B�B�B�B#9B�B=B!�B!bB\B!BPB	�B�B�B�B%B
rB�B'�B
�B
��B
��B
��B
�B
��B
�
B
ɆB
��B
�UB
��B
�,B
��B
��B
�KB
��B
��B
�B
��B
��B
��B
��B
p�B
y�B
.B
�B
cB
b�B
^iB
iB
h
B
iDB
u�B
Y�B
S&B
Q�B
F?B
CaB
C�B
F�B
=pB
D�B
HKB
-wB
@�B
1'B
$�B
Q�B
�B
qB
�B
�B
�B
bB
~B
�B
�B
eB
�B
B
B
�B
 �B
eB	��B	�bB	��B	�B	�"B	�1B	�fB	�%B	�B	�B	��B
GB	�YB	��B	�B	��B	�
B	�B	�#B	�QB	�yB	�sB	˒B	� B	�XB	�XB	��B	�IB	��B	��B	�hB	��B	��B	�hB	�RB	��B	��B	�nB	�B	�tB	��B	��B	��B	��B	�hB	�UB	�'B	��B	��B	�wB	�OB	�YB	�B	�B	�EB	�FB	��B	�@B	�LB	�B	�YB	�{B	�GB	�SB	�MB	��B	��B	��B	��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                        44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224951                            20230721224951AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495120230721224951  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495120230721224951QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495120230721224951QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               