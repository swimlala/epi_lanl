CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:12Z creation      
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
resolution        =���   axis      Z        @  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  X�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p0   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  v    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ޠ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @ �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 0    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   0`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   6`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   <`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T B`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   B�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   B�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   B�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   B�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � B�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   CT   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Cp   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Cx   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        C�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        C�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       C�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    C�Argo profile    3.1 1.2 19500101000000  20230721225012  20230721225012  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�8e���F@�8e���F11  @�8f-��0@�8f-��0@1���@1����d����;��d����;�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?u?��H@@  @�  @��R@�  @�  A   A  A ��A,(�A?\)A`  A\)A�Q�A���A�  A�  A�  A�  A��B Q�B  B�B�B�
B'�
B0(�B8z�B@Q�BG�
BO�BX  B_�
Bh  Bp  Bw�
B�  B��B�  B��B��B��B�  B�  B�{B�{B��B��B��B�  B��B�  B�{B�  B��B��
B��
B�  B�{B�  B�  B�{B�  B��
B��
B�  B�(�B�{C   C  C��C�HC�C	��C��C��C  C
=C��C  C  C�C��C�C   C"
=C$  C&
=C(  C*  C,
=C.
=C0
=C2  C4  C5��C8  C:  C<  C>  C@  CB
=CD{CF
=CH
=CJ
=CL
=CM��CO�HCQ�CT  CU��CX
=CY��C\  C^
=C_�Ca��Cd  Ce��Ch  Cj  Cl
=Cn{Cp
=Cr  Ct  Cu��Cx  Cz
=C|  C}��C�  C�  C���C�  C���C�C���C�  C�  C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�C���C���C�  C�  C���C�
=C�  C���C�  C�C���C�C�C���C�C�
=C�  C�  C�
=C�  C�C���C���C���C�  C�  C�  C�  C�C�C�  C���C�C�C�  C��C�  C�  C���C���C�C���C���C���C�  C�  C�  C�C�  C�C�  C���C�C�C���C�  C�  C�C�  C���C�  C�C�C�C�  C���C�  C�  C���C���C���C���C�  C���C���C���C�  C�C�\C�
=C�  C��C��C��C���C���C���C���C���C�  C�  C�  C�  C���C���C���C���C���C�  C���C���C���C���C�  C�C�D �D ��D  D}qD�qD��D  D��D�D� D  D� D�D}qD�qD� D�D��D	  D	� D
�D
��D�D��D�D}qD  D��D�qD}qD  D� D�qD� DD��D  D� D�D� D��Dz�D�qD� DD��D  D��DD�D  D� D  D� D�D�D�D� D�qD}qD�D�D  D� D   D � D!  D!� D!�qD"� D"�qD#z�D$  D$��D%�D%� D%�qD&z�D&�qD'��D(�D(��D)�D)� D)�qD*��D+D+� D+��D,}qD-�D-� D-�qD.��D/�D/� D0�D0��D1  D1}qD1�qD2}qD2��D3� D4  D4}qD5  D5� D5�qD6}qD7  D7� D8  D8� D9  D9� D:�D:��D;�D;��D<  D<� D<�qD=}qD>  D>� D?�D?� D@  D@�DA�DA}qDA��DBz�DC  DC}qDD  DD}qDD��DE� DFDF��DG�DG}qDG��DHz�DH�qDI� DJ  DJ� DK  DK� DL  DL}qDM  DM� DN  DN� DO  DO� DO��DP}qDQ�DQ��DR�DR}qDR��DS}qDT  DT� DU�DU��DV�DV�DW  DW� DX�DX��DY�DY��DZ�DZ� DZ��D[}qD[��D\z�D]  D]��D^  D^��D_�D_� D`  D`� D`�qDa� Db  Db� Dc�Dc}qDd�Dd��De  De}qDe�qDf}qDg�Dg��Dh  Dh�Di  Diz�Dj  Dj�Dk  Dk}qDl  Dl��Dm�Dm}qDm��Dnz�Dn�qDo��Dp�Dp� Dq�Dq��Dr  Dr��Dr�qDs}qDt  Dt��Du�Du}qDu�qDv��Dw�Dw��Dw�qDx� DyDy��Dz�Dz��D{  D{� D|  D|}qD}  D}� D}�qD~}qD  D� D�qD�AHD���D�� D�HD�>�D�� D��HD�HD�B�D��HD�D�HD�@ D�� D��HD�HD�@ D��HD�� D�  D�AHD�~�D���D���D�@ D��HD�D�HD�>�D��HD��HD�HD�B�D��HD�� D��D�AHD�� D���D���D�>�D�� D�� D���D�>�D�� D�� D�HD�AHD�� D���D�HD�AHD��HD�� D�  D�@ D�� D��HD�  D�@ D�~�D���D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�EG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?\)?W
=?�=q?�{?��?��@��@(�@.{@B�\@O\)@fff@z�H@��\@���@�Q�@�p�@��@��@�Q�@\@˅@��@�p�@��@���@�Q�A   A33A��A(�A  A�A��A��A"�\A%A*=qA0��A2�\A7�A<��A@  AE�AI��AL��AR�\AUAZ�HA^�RAb�\AhQ�Aj�HAp  Au�AxQ�A|��A�G�A��\A�A��A���A�z�A�A���A��\A��A�\)A���A��
A��A��A��A�33A�{A�\)A�G�A��A��A�\)A���A�33A�(�A��RA�Q�A��A�(�A�p�A�
=A��A��HA�p�A�
=Aȣ�A�33A�z�AθRAУ�A�=qA���A�Aأ�Aڏ\A��
A޸RA�  A��A�z�A�A�A�\A�(�A�{A��A��A�(�A�{A��A�=qA�z�A�p�B Q�B ��B{B33B�
B��B=qB�HB  B	�B	B
=B  B��B{B�HB�B�BB�RB  B��BB
=B�Bz�BBffB�B��BG�BffB�B (�B!p�B"=qB#
=B$z�B%�B&{B'\)B(  B)�B*=qB*�HB,(�B-�B-B/
=B0  B0��B1B2�HB3�B4z�B5�B6ffB7�B8z�B9�B:�\B;33B<  B=G�B=�B>�RB@  B@��BAp�BB�HBC�BDQ�BE��BF�\BG33BHz�BI��BJ{BK\)BLz�BM�BNffBO�BPQ�BQp�BR�\BS33BT��BUG�BV=qBW�BX��BY�BZ�\B[\)B\(�B]��B^ffB_33B`��Ba�Bb=qBc�BdQ�BeG�Bf�\Bg�Bh(�Bi��Bj�\Bk\)Bl��BmBnffBo�
Bp��Bqp�Br�RBt  Bt��Bu�Bv�HBw�Bx��By�Bz�RB{�
B|��B}p�B~�HB�
B�Q�B���B��B��
B�ffB�
=B�\)B��B��\B��HB��B�(�B�z�B�
=B��B�(�B��\B�33B��B�{B���B�G�B���B�=qB���B��B��B�Q�B���B��B��
B�(�B���B�G�B��B�{B���B�33B���B�=qB���B��B��B�ffB���B�33B��B�z�B���B�\)B��B��\B��HB�p�B�{B�z�B��HB���B�(�B�z�B���B��B�{B�ffB��B���B�  B��\B�33B��B�  B��RB��B��B�=qB���B���B���B�=qB��RB��B��B�Q�B���B�33B�B�ffB���B�33B��
B�z�B���B�33B��
B�ffB���B�33B��
B�ffB���B�33B��B�z�B���B�G�B��B�ffB���B�p�B�{B�z�B��HB�p�B�  B�ffB���B���B�  B�Q�B���B��B��B�ffB��B���B��B�ffB��B��B��
B\B���B�\)B��
B�z�B��HB�G�B��B�z�B��HB�G�B�  B�z�B��HB�p�B�{B�z�B��HB˅B�=qḄ�B�
=BͮB�=qBΣ�B�
=B�B�=qBУ�B�33B��B�ffBҸRB�p�B��B�Q�B���BՅB��B�ffB��B׮B�  Bأ�B�G�B�B�{Bڣ�B�\)B�B�(�B��HB�p�B�B�ffB���B�G�B��
B�z�B�
=B�p�B��B�z�B��B�B��B��B�G�B�B�(�B��B�\)B��B�Q�B���B�B�  B�ffB�
=B�B�{B�\B�33B��
B�=qB�RB�\)B�  B�ffB���B�B�  B�z�B�33B��
B�=qB��RB��B��B��\B�G�B��B�=qB���B�p�B��B�z�B�33B���B�=qB��HB�\)B��
B��\B�
=B�p�C {C \)C �\C �HC33C\)C�RC
=C33C�C�
C{CQ�C�C  C=qCz�C�HC�CQ�C�C
=C33Cz�C�
C(�CffC�C
=C\)C��C�HC	33C	�C	C
{C
p�C
��C  CQ�C�C�C33C��C�C(�Cz�C�
C(�CffC�C
=C\)C��C�HC=qC�\C��C
=C\)CC��C33C�\C�HC{Cp�C��C  CQ�C�C  C=qCz�C�HC33Cz�C�RC  C\)C�C�C(�C�C�
C�C\)C�RC{CQ�C�\C�HC33C�\C�
C{CffCC
=CG�C�C��C33Cz�C�HC33Cp�C�RC 
=C p�C ��C �C!G�C!��C!�HC"�C"�C"�
C#{C#Q�C#�C$
=C$\)C$��C$�HC%G�C%�\C%C&{C&p�C&�RC&��C'33C'��C'�HC({C(ffC(C)
=C)G�C)�C)��C*(�C*z�C*�RC*��C+33C+�\C+�HC,33C,ffC,��C,�C-G�C-��C-�HC.{C.\)C.�C/  C/Q�C/�\C/��C0
=C0ffC0�C1  C1=qC1p�C1�C2
=C2\)C2�\C2��C3�C3ffC3��C3�
C4(�C4�C4��C5
=C5G�C5��C5��C633C6p�C6�RC7{C7ffC7C8
=C8\)C8��C8�HC933C9�C9�HC:33C:z�C:��C;  C;G�C;��C;�C<=qC<�\C<��C={C=\)C=�C>
=C>Q�C>��C>�C?(�C?p�C?�RC@
=C@ffC@�RC@��CAQ�CA��CA�HCB(�CBz�CB��CC�CCz�CCCD
=CDQ�CD��CD�
CE�CEp�CECF�CFQ�CF��CF�HCG=qCG��CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?u?��H@@  @�  @��R@�  @�  A   A  A ��A,(�A?\)A`  A\)A�Q�A���A�  A�  A�  A�  A��B Q�B  B�B�B�
B'�
B0(�B8z�B@Q�BG�
BO�BX  B_�
Bh  Bp  Bw�
B�  B��B�  B��B��B��B�  B�  B�{B�{B��B��B��B�  B��B�  B�{B�  B��B��
B��
B�  B�{B�  B�  B�{B�  B��
B��
B�  B�(�B�{C   C  C��C�HC�C	��C��C��C  C
=C��C  C  C�C��C�C   C"
=C$  C&
=C(  C*  C,
=C.
=C0
=C2  C4  C5��C8  C:  C<  C>  C@  CB
=CD{CF
=CH
=CJ
=CL
=CM��CO�HCQ�CT  CU��CX
=CY��C\  C^
=C_�Ca��Cd  Ce��Ch  Cj  Cl
=Cn{Cp
=Cr  Ct  Cu��Cx  Cz
=C|  C}��C�  C�  C���C�  C���C�C���C�  C�  C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�C���C���C�  C�  C���C�
=C�  C���C�  C�C���C�C�C���C�C�
=C�  C�  C�
=C�  C�C���C���C���C�  C�  C�  C�  C�C�C�  C���C�C�C�  C��C�  C�  C���C���C�C���C���C���C�  C�  C�  C�C�  C�C�  C���C�C�C���C�  C�  C�C�  C���C�  C�C�C�C�  C���C�  C�  C���C���C���C���C�  C���C���C���C�  C�C�\C�
=C�  C��C��C��C���C���C���C���C���C�  C�  C�  C�  C���C���C���C���C���C�  C���C���C���C���C�  C�C�D �D ��D  D}qD�qD��D  D��D�D� D  D� D�D}qD�qD� D�D��D	  D	� D
�D
��D�D��D�D}qD  D��D�qD}qD  D� D�qD� DD��D  D� D�D� D��Dz�D�qD� DD��D  D��DD�D  D� D  D� D�D�D�D� D�qD}qD�D�D  D� D   D � D!  D!� D!�qD"� D"�qD#z�D$  D$��D%�D%� D%�qD&z�D&�qD'��D(�D(��D)�D)� D)�qD*��D+D+� D+��D,}qD-�D-� D-�qD.��D/�D/� D0�D0��D1  D1}qD1�qD2}qD2��D3� D4  D4}qD5  D5� D5�qD6}qD7  D7� D8  D8� D9  D9� D:�D:��D;�D;��D<  D<� D<�qD=}qD>  D>� D?�D?� D@  D@�DA�DA}qDA��DBz�DC  DC}qDD  DD}qDD��DE� DFDF��DG�DG}qDG��DHz�DH�qDI� DJ  DJ� DK  DK� DL  DL}qDM  DM� DN  DN� DO  DO� DO��DP}qDQ�DQ��DR�DR}qDR��DS}qDT  DT� DU�DU��DV�DV�DW  DW� DX�DX��DY�DY��DZ�DZ� DZ��D[}qD[��D\z�D]  D]��D^  D^��D_�D_� D`  D`� D`�qDa� Db  Db� Dc�Dc}qDd�Dd��De  De}qDe�qDf}qDg�Dg��Dh  Dh�Di  Diz�Dj  Dj�Dk  Dk}qDl  Dl��Dm�Dm}qDm��Dnz�Dn�qDo��Dp�Dp� Dq�Dq��Dr  Dr��Dr�qDs}qDt  Dt��Du�Du}qDu�qDv��Dw�Dw��Dw�qDx� DyDy��Dz�Dz��D{  D{� D|  D|}qD}  D}� D}�qD~}qD  D� D�qD�AHD���D�� D�HD�>�D�� D��HD�HD�B�D��HD�D�HD�@ D�� D��HD�HD�@ D��HD�� D�  D�AHD�~�D���D���D�@ D��HD�D�HD�>�D��HD��HD�HD�B�D��HD�� D��D�AHD�� D���D���D�>�D�� D�� D���D�>�D�� D�� D�HD�AHD�� D���D�HD�AHD��HD�� D�  D�@ D�� D��HD�  D�@ D�~�D���D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�EG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?\)?W
=?�=q?�{?��?��@��@(�@.{@B�\@O\)@fff@z�H@��\@���@�Q�@�p�@��@��@�Q�@\@˅@��@�p�@��@���@�Q�A   A33A��A(�A  A�A��A��A"�\A%A*=qA0��A2�\A7�A<��A@  AE�AI��AL��AR�\AUAZ�HA^�RAb�\AhQ�Aj�HAp  Au�AxQ�A|��A�G�A��\A�A��A���A�z�A�A���A��\A��A�\)A���A��
A��A��A��A�33A�{A�\)A�G�A��A��A�\)A���A�33A�(�A��RA�Q�A��A�(�A�p�A�
=A��A��HA�p�A�
=Aȣ�A�33A�z�AθRAУ�A�=qA���A�Aأ�Aڏ\A��
A޸RA�  A��A�z�A�A�A�\A�(�A�{A��A��A�(�A�{A��A�=qA�z�A�p�B Q�B ��B{B33B�
B��B=qB�HB  B	�B	B
=B  B��B{B�HB�B�BB�RB  B��BB
=B�Bz�BBffB�B��BG�BffB�B (�B!p�B"=qB#
=B$z�B%�B&{B'\)B(  B)�B*=qB*�HB,(�B-�B-B/
=B0  B0��B1B2�HB3�B4z�B5�B6ffB7�B8z�B9�B:�\B;33B<  B=G�B=�B>�RB@  B@��BAp�BB�HBC�BDQ�BE��BF�\BG33BHz�BI��BJ{BK\)BLz�BM�BNffBO�BPQ�BQp�BR�\BS33BT��BUG�BV=qBW�BX��BY�BZ�\B[\)B\(�B]��B^ffB_33B`��Ba�Bb=qBc�BdQ�BeG�Bf�\Bg�Bh(�Bi��Bj�\Bk\)Bl��BmBnffBo�
Bp��Bqp�Br�RBt  Bt��Bu�Bv�HBw�Bx��By�Bz�RB{�
B|��B}p�B~�HB�
B�Q�B���B��B��
B�ffB�
=B�\)B��B��\B��HB��B�(�B�z�B�
=B��B�(�B��\B�33B��B�{B���B�G�B���B�=qB���B��B��B�Q�B���B��B��
B�(�B���B�G�B��B�{B���B�33B���B�=qB���B��B��B�ffB���B�33B��B�z�B���B�\)B��B��\B��HB�p�B�{B�z�B��HB���B�(�B�z�B���B��B�{B�ffB��B���B�  B��\B�33B��B�  B��RB��B��B�=qB���B���B���B�=qB��RB��B��B�Q�B���B�33B�B�ffB���B�33B��
B�z�B���B�33B��
B�ffB���B�33B��
B�ffB���B�33B��B�z�B���B�G�B��B�ffB���B�p�B�{B�z�B��HB�p�B�  B�ffB���B���B�  B�Q�B���B��B��B�ffB��B���B��B�ffB��B��B��
B\B���B�\)B��
B�z�B��HB�G�B��B�z�B��HB�G�B�  B�z�B��HB�p�B�{B�z�B��HB˅B�=qḄ�B�
=BͮB�=qBΣ�B�
=B�B�=qBУ�B�33B��B�ffBҸRB�p�B��B�Q�B���BՅB��B�ffB��B׮B�  Bأ�B�G�B�B�{Bڣ�B�\)B�B�(�B��HB�p�B�B�ffB���B�G�B��
B�z�B�
=B�p�B��B�z�B��B�B��B��B�G�B�B�(�B��B�\)B��B�Q�B���B�B�  B�ffB�
=B�B�{B�\B�33B��
B�=qB�RB�\)B�  B�ffB���B�B�  B�z�B�33B��
B�=qB��RB��B��B��\B�G�B��B�=qB���B�p�B��B�z�B�33B���B�=qB��HB�\)B��
B��\B�
=B�p�C {C \)C �\C �HC33C\)C�RC
=C33C�C�
C{CQ�C�C  C=qCz�C�HC�CQ�C�C
=C33Cz�C�
C(�CffC�C
=C\)C��C�HC	33C	�C	C
{C
p�C
��C  CQ�C�C�C33C��C�C(�Cz�C�
C(�CffC�C
=C\)C��C�HC=qC�\C��C
=C\)CC��C33C�\C�HC{Cp�C��C  CQ�C�C  C=qCz�C�HC33Cz�C�RC  C\)C�C�C(�C�C�
C�C\)C�RC{CQ�C�\C�HC33C�\C�
C{CffCC
=CG�C�C��C33Cz�C�HC33Cp�C�RC 
=C p�C ��C �C!G�C!��C!�HC"�C"�C"�
C#{C#Q�C#�C$
=C$\)C$��C$�HC%G�C%�\C%C&{C&p�C&�RC&��C'33C'��C'�HC({C(ffC(C)
=C)G�C)�C)��C*(�C*z�C*�RC*��C+33C+�\C+�HC,33C,ffC,��C,�C-G�C-��C-�HC.{C.\)C.�C/  C/Q�C/�\C/��C0
=C0ffC0�C1  C1=qC1p�C1�C2
=C2\)C2�\C2��C3�C3ffC3��C3�
C4(�C4�C4��C5
=C5G�C5��C5��C633C6p�C6�RC7{C7ffC7C8
=C8\)C8��C8�HC933C9�C9�HC:33C:z�C:��C;  C;G�C;��C;�C<=qC<�\C<��C={C=\)C=�C>
=C>Q�C>��C>�C?(�C?p�C?�RC@
=C@ffC@�RC@��CAQ�CA��CA�HCB(�CBz�CB��CC�CCz�CCCD
=CDQ�CD��CD�
CE�CEp�CECF�CFQ�CF��CF�HCG=qCG��CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�%A�%A�
=A�JA�
=A�%A�A�1A�A�%A�JA�VA�JA�bA�oA�VA�JA�JA�VA�
=A�1A�1A�A���A�ƨA؏\A�~�A�`BA�"�A�hsA֋DA�^5A���A�C�A��mA��mA�ƨA��Aч+A�t�A�AϓuA·+A�bNA�K�AͶFA�ĜA���A�A��A̅A��Aˡ�A�bAʋDA�5?A���Aƛ�AœuA��A�ƨA� �A���A�x�A�~�A�"�A��A��FA�G�A�{A�{A�9XA��A���A�v�A��jA��uA��\A�VA�&�A�dZA�  A�7LA��A���A�|�A�S�A�M�A��DA�ZA�  A�5?A�bNA�A��
A��-A��/A�A���A���A��
A��jA�=qA��A�A�A�bA�-A��TA�
=A�=qA���A���A�Q�A�;dA���A}�wAz�+Ax=qAu�AqC�Ao��AoK�Am�7AjJAd=qA_hsA\ĜAY�PAX�AVffAUG�ASK�AO&�AK7LAH��AFE�AD��AD�AB��AAoA?`BA=|�A;XA:  A8~�A7�-A6�A5�A4�A2�A1�
A0ĜA/
=A-C�A,��A+�FA)�A(z�A&A�A%�A$�yA#O�A!�A��A{Al�AdZA�9AA�A�yA�-A�+AI�A��A�A(�Al�AjA5?A7LA;dA?}A��AM�AAoAjA9XA�A?}A
ffA
1A	�^AAn�AQ�A$�A�!AA�A?}A A�@�
=@�$�@��#@��h@�1@�{@��@���@�Z@�P@��H@�{@�@��@� �@�@�u@�t�@��@�@���@�~�@��@��/@�33@�ȴ@��H@�!@��@�"�@��@�@�v�@��@��@�O�@�/@���@��`@��@���@�p�@�`B@�7L@�`B@���@�^@��@�5?@�z�@���@�bN@�|�@�\)@�33@���@��@٩�@؋D@��@�ƨ@�|�@�33@��@�J@�?}@�p�@�@Ցh@Ԭ@�ƨ@�dZ@�$�@�V@Л�@��@ϝ�@�\)@Ϯ@��@�I�@�I�@˥�@ʰ!@ɩ�@�%@ȼj@�Ĝ@�A�@ǝ�@ư!@�x�@ă@��@Ý�@�
=@���@�Ĝ@�1'@��m@�@�5?@���@��T@��#@�x�@�j@�;d@�o@�33@��@���@�ȴ@���@�ȴ@��R@�-@���@�`B@�/@��/@���@�1'@��@��y@�^5@��@���@��@���@�b@��w@���@��@�K�@�+@�K�@�K�@�K�@�o@��!@�$�@��@�@�x�@�V@���@�  @��P@�dZ@�
=@���@�M�@�@��#@���@�?}@��@��@��`@�z�@�1'@��@�  @��@��@�S�@���@��+@��+@�{@��T@���@��@���@� �@��F@�|�@�K�@�o@��@�^5@��T@�X@��`@���@��u@�I�@�  @�ƨ@���@�l�@�33@��@���@�ff@�-@���@���@�`B@��/@���@�j@�Q�@�A�@�1'@��@���@�+@��!@�v�@�^5@�=q@��@��7@�?}@��`@�r�@�b@�S�@�o@�ȴ@��@���@�X@��@��j@�A�@��m@��
@���@�t�@�S�@��@��y@��\@�{@�@��@�%@���@��j@��@��@��u@��@���@�
=@���@��\@�ff@�=q@��T@��@��@���@��9@�j@�A�@��;@��F@�|�@�K�@�33@�
=@�ȴ@�~�@�^5@���@��^@���@�?}@���@��@�r�@�bN@�Q�@�I�@�9X@�1'@�b@�ƨ@�dZ@�C�@���@��+@�V@�-@��@�@��@��#@���@��@��@�I�@� �@�b@���@���@��;@�S�@��@��y@��@���@�ȴ@���@�ff@�-@��^@�&�@�Ĝ@��@���@�
=@�
=@���@��@���@���@���@��@��@��`@���@���@���@�Z@�b@K�@~5?@~{@~@}@}p�@}V@|�@|j@{�m@{�@{C�@{o@zn�@xĜ@w��@wK�@v�@u�T@uO�@u/@u�@u�@uV@t��@t�@t�@st�@r��@rn�@rn�@r-@p��@o�;@o��@o�;@q�@p�`@pA�@o�w@o
=@n�R@n�+@nV@n5?@n{@m��@l�/@lz�@l�@k��@kC�@j��@j~�@j-@i��@i�7@i&�@h�`@h��@hr�@h1'@g�@g�w@gl�@g
=@f�@f��@f��@fV@e��@ep�@eV@d��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�  A���A���A�A�%A�JA�1A�  A�1A�%A�
=A�JA�
=A�
=A�VA�1A�JA�JA�1A�VA�JA�A�
=A�%A�A�1A�A�A�
=A�1A�%A�1A�1A�A�1A�A�A�JA�1A�JA�JA�
=A�JA�VA�JA�oA�JA�bA�VA�JA�bA�
=A�JA�VA�1A�JA�VA�1A�JA�1A�VA�{A�VA�{A�VA�oA�oA�JA�{A�VA�oA�{A�bA��A�bA�bA�
=A�1A�
=A�JA��A�bA�bA�VA�JA�oA�JA�
=A�JA�1A�JA�1A�1A�JA�1A�JA�VA�JA�bA�
=A�bA�VA�VA�oA�VA�JA�VA�JA�
=A�bA�JA�1A�
=A�%A�%A�JA�1A�JA�JA�
=A�
=A�1A�JA�%A�%A�A�1A�1A�
=A�
=A�A�1A�%A�A�1A�A�A�%A�A�A�%A�  A�  A���A���A��A���A���A��yA��`A���A�ȴA�ƨA���A���Aش9Aز-Aة�Aؕ�A؋DA؏\A؋DA؇+A؋DA؃A؅A؇+A؅A؃A؅A�~�A�v�A�r�A�p�A�p�A�r�A�n�A�ZA�XA�S�A�\)A�VA�\)A�M�A�C�A�(�A�+A�(�A��A�
=A��`A��/A���Aי�A׃A�O�A�+A�bA��AֶFA֛�A֗�A֑hA�t�A�r�A�p�A�n�A�p�A�hsA�ffA�dZA�\)A�Q�A�VA�M�A�$�A��A�
=A���A��TA�ĜA�hsA�K�A�  Aԙ�A�I�A��A�
=A�A���A��A��A��A��mA��yA��mA��TA��mA��yA��TA��A��yA��`A��yA��yA��`A��A��yA��`A��yA��TA�ȴAӕ�Aӕ�Aӕ�AӍPA�`BA�/A�bA��yA�|�A��A��A�ȴAѥ�A�n�A�jA�dZA�dZA�dZA�`BA�dZA�hsA�jA�v�AэPAыDA�x�A�p�A�r�A�=qA�
=A��A�AиRAЗ�A�t�A�XA�VA�jA�?}A�=qA�1A���AΛ�AΑhA΋DAΉ7A΁A�t�A�t�A�r�A�n�A�bNA�dZA�ffA�^5A�ZA�XA�XA�Q�A�Q�A�S�A�Q�A�M�A�G�A�C�A�$�A��A�%AͰ!AͲ-AͼjA�jA�ZA�C�A�$�A���A�ȴA̍PA̅A�|�ÁA̩�A̼jA���A��HA��A��A��TA��`A��A���A���A�  A�
=A�
=A�bA�{A�bA�%A�%A���A��yA���A�ȴA�ĜA̼jA̴9A̧�Ȧ+A�p�A�O�A�E�A�7LA�-A�+A� �A�{A�oA�bA�%A���A���A���A��TA˸RA˅A�C�A�=qA�=qA�A�A�33A�"�A� �A�{A�  A��#Aʺ^Aʰ!Aʩ�Aʟ�Aʏ\A�|�A�jA�`BA�S�A�1'A�\)A��yA���A���AȼjA���Aȴ9AȓuA�^5A��A���A�|�A�O�A�&�A�
=A��;AƼjAư!Aƣ�AƇ+A��A���A���AżjAť�AœuAŅA�jA�G�A�33A�+A�$�A��A�{A�oA�JA�A���A��A��/A���AĴ9AĲ-Aĕ�AāA�^5A�;dA�&�A�oA�JA�A���A��A��yA��HA���A�ƨA�A���Aú^Aò-AîAÝ�A�z�A�ffA�7LA��`A��Aº^APA�z�A�dZA�G�A�7LA�{A���A�z�A�A�A�  A���A�~�A�ZA�  A��A��A��A���A��A�x�A�7LA��`A��A�|�A��\A��\A��PA��hA��DA��A�ZA�5?A��A���A��DA�hsA�E�A��A��A�A��DA�5?A���A�dZA��A��\A�t�A�ffA�\)A�M�A�I�A�G�A�/A�oA���A�A�l�A�  A���A���A��+A�1'A���A�ffA��A���A���A�5?A��A��jA��7A�\)A�I�A�C�A�$�A��A��jA��A��A��A���A��A��!A��A���A���A��hA�K�A� �A���A���A��9A��DA�/A��yA��wA���A��+A�Q�A�"�A��A���A�r�A�=qA�A�A�  A���A���A��yA���A��RA�G�A��^A��7A�bA�dZA�1'A���A���A�&�A��A��HA���A��A�?}A���A�9XA�jA���A��A��#A���A���A���A�r�A�A�A��;A�K�A�"�A��A�
=A���A��mA���A�\)A��A��A��-A�jA�VA�M�A�E�A�7LA�JA�~�A�bNA�S�A�5?A�5?A�33A�VA��TA��RA���A�t�A�I�A�7LA��A��A�A�XA�bA��`A���A��A��A���A�\)A�&�A��!A�bA���A�`BA�M�A�A�A�33A�$�A�
=A��A���A�Q�A���A�JA���A�\)A�+A��A��A�p�A���A�l�A�O�A�{A��A��`A���A�M�A�n�A���A�l�A��A� �A��
A��+A�`BA�ZA�VA�S�A�I�A�(�A��TA���A�t�A�VA�I�A�9XA�oA��A�ȴA��RA��-A���A���A���A���A��hA�x�A�jA�O�A�7LA�"�A��A���A�(�A��A�A�A���A���A�x�A�XA��A�A��A�;dA��A���A��hA�l�A�I�A�?}A�;dA�7LA�1'A�+A�+A�(�A�&�A�"�A��A���A��/A��^A�t�A�9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�%A�%A�
=A�JA�
=A�%A�A�1A�A�%A�JA�VA�JA�bA�oA�VA�JA�JA�VA�
=A�1A�1A�A���A�ƨA؏\A�~�A�`BA�"�A�hsA֋DA�^5A���A�C�A��mA��mA�ƨA��Aч+A�t�A�AϓuA·+A�bNA�K�AͶFA�ĜA���A�A��A̅A��Aˡ�A�bAʋDA�5?A���Aƛ�AœuA��A�ƨA� �A���A�x�A�~�A�"�A��A��FA�G�A�{A�{A�9XA��A���A�v�A��jA��uA��\A�VA�&�A�dZA�  A�7LA��A���A�|�A�S�A�M�A��DA�ZA�  A�5?A�bNA�A��
A��-A��/A�A���A���A��
A��jA�=qA��A�A�A�bA�-A��TA�
=A�=qA���A���A�Q�A�;dA���A}�wAz�+Ax=qAu�AqC�Ao��AoK�Am�7AjJAd=qA_hsA\ĜAY�PAX�AVffAUG�ASK�AO&�AK7LAH��AFE�AD��AD�AB��AAoA?`BA=|�A;XA:  A8~�A7�-A6�A5�A4�A2�A1�
A0ĜA/
=A-C�A,��A+�FA)�A(z�A&A�A%�A$�yA#O�A!�A��A{Al�AdZA�9AA�A�yA�-A�+AI�A��A�A(�Al�AjA5?A7LA;dA?}A��AM�AAoAjA9XA�A?}A
ffA
1A	�^AAn�AQ�A$�A�!AA�A?}A A�@�
=@�$�@��#@��h@�1@�{@��@���@�Z@�P@��H@�{@�@��@� �@�@�u@�t�@��@�@���@�~�@��@��/@�33@�ȴ@��H@�!@��@�"�@��@�@�v�@��@��@�O�@�/@���@��`@��@���@�p�@�`B@�7L@�`B@���@�^@��@�5?@�z�@���@�bN@�|�@�\)@�33@���@��@٩�@؋D@��@�ƨ@�|�@�33@��@�J@�?}@�p�@�@Ցh@Ԭ@�ƨ@�dZ@�$�@�V@Л�@��@ϝ�@�\)@Ϯ@��@�I�@�I�@˥�@ʰ!@ɩ�@�%@ȼj@�Ĝ@�A�@ǝ�@ư!@�x�@ă@��@Ý�@�
=@���@�Ĝ@�1'@��m@�@�5?@���@��T@��#@�x�@�j@�;d@�o@�33@��@���@�ȴ@���@�ȴ@��R@�-@���@�`B@�/@��/@���@�1'@��@��y@�^5@��@���@��@���@�b@��w@���@��@�K�@�+@�K�@�K�@�K�@�o@��!@�$�@��@�@�x�@�V@���@�  @��P@�dZ@�
=@���@�M�@�@��#@���@�?}@��@��@��`@�z�@�1'@��@�  @��@��@�S�@���@��+@��+@�{@��T@���@��@���@� �@��F@�|�@�K�@�o@��@�^5@��T@�X@��`@���@��u@�I�@�  @�ƨ@���@�l�@�33@��@���@�ff@�-@���@���@�`B@��/@���@�j@�Q�@�A�@�1'@��@���@�+@��!@�v�@�^5@�=q@��@��7@�?}@��`@�r�@�b@�S�@�o@�ȴ@��@���@�X@��@��j@�A�@��m@��
@���@�t�@�S�@��@��y@��\@�{@�@��@�%@���@��j@��@��@��u@��@���@�
=@���@��\@�ff@�=q@��T@��@��@���@��9@�j@�A�@��;@��F@�|�@�K�@�33@�
=@�ȴ@�~�@�^5@���@��^@���@�?}@���@��@�r�@�bN@�Q�@�I�@�9X@�1'@�b@�ƨ@�dZ@�C�@���@��+@�V@�-@��@�@��@��#@���@��@��@�I�@� �@�b@���@���@��;@�S�@��@��y@��@���@�ȴ@���@�ff@�-@��^@�&�@�Ĝ@��@���@�
=@�
=@���@��@���@���@���@��@��@��`@���@���@���@�Z@�b@K�@~5?@~{@~@}@}p�@}V@|�@|j@{�m@{�@{C�@{o@zn�@xĜ@w��@wK�@v�@u�T@uO�@u/@u�@u�@uV@t��@t�@t�@st�@r��@rn�@rn�@r-@p��@o�;@o��@o�;@q�@p�`@pA�@o�w@o
=@n�R@n�+@nV@n5?@n{@m��@l�/@lz�@l�@k��@kC�@j��@j~�@j-@i��@i�7@i&�@h�`@h��@hr�@h1'@g�@g�w@gl�@g
=@f�@f��@f��@fV@e��@ep�@eV@d��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�  A���A���A�A�%A�JA�1A�  A�1A�%A�
=A�JA�
=A�
=A�VA�1A�JA�JA�1A�VA�JA�A�
=A�%A�A�1A�A�A�
=A�1A�%A�1A�1A�A�1A�A�A�JA�1A�JA�JA�
=A�JA�VA�JA�oA�JA�bA�VA�JA�bA�
=A�JA�VA�1A�JA�VA�1A�JA�1A�VA�{A�VA�{A�VA�oA�oA�JA�{A�VA�oA�{A�bA��A�bA�bA�
=A�1A�
=A�JA��A�bA�bA�VA�JA�oA�JA�
=A�JA�1A�JA�1A�1A�JA�1A�JA�VA�JA�bA�
=A�bA�VA�VA�oA�VA�JA�VA�JA�
=A�bA�JA�1A�
=A�%A�%A�JA�1A�JA�JA�
=A�
=A�1A�JA�%A�%A�A�1A�1A�
=A�
=A�A�1A�%A�A�1A�A�A�%A�A�A�%A�  A�  A���A���A��A���A���A��yA��`A���A�ȴA�ƨA���A���Aش9Aز-Aة�Aؕ�A؋DA؏\A؋DA؇+A؋DA؃A؅A؇+A؅A؃A؅A�~�A�v�A�r�A�p�A�p�A�r�A�n�A�ZA�XA�S�A�\)A�VA�\)A�M�A�C�A�(�A�+A�(�A��A�
=A��`A��/A���Aי�A׃A�O�A�+A�bA��AֶFA֛�A֗�A֑hA�t�A�r�A�p�A�n�A�p�A�hsA�ffA�dZA�\)A�Q�A�VA�M�A�$�A��A�
=A���A��TA�ĜA�hsA�K�A�  Aԙ�A�I�A��A�
=A�A���A��A��A��A��mA��yA��mA��TA��mA��yA��TA��A��yA��`A��yA��yA��`A��A��yA��`A��yA��TA�ȴAӕ�Aӕ�Aӕ�AӍPA�`BA�/A�bA��yA�|�A��A��A�ȴAѥ�A�n�A�jA�dZA�dZA�dZA�`BA�dZA�hsA�jA�v�AэPAыDA�x�A�p�A�r�A�=qA�
=A��A�AиRAЗ�A�t�A�XA�VA�jA�?}A�=qA�1A���AΛ�AΑhA΋DAΉ7A΁A�t�A�t�A�r�A�n�A�bNA�dZA�ffA�^5A�ZA�XA�XA�Q�A�Q�A�S�A�Q�A�M�A�G�A�C�A�$�A��A�%AͰ!AͲ-AͼjA�jA�ZA�C�A�$�A���A�ȴA̍PA̅A�|�ÁA̩�A̼jA���A��HA��A��A��TA��`A��A���A���A�  A�
=A�
=A�bA�{A�bA�%A�%A���A��yA���A�ȴA�ĜA̼jA̴9A̧�Ȧ+A�p�A�O�A�E�A�7LA�-A�+A� �A�{A�oA�bA�%A���A���A���A��TA˸RA˅A�C�A�=qA�=qA�A�A�33A�"�A� �A�{A�  A��#Aʺ^Aʰ!Aʩ�Aʟ�Aʏ\A�|�A�jA�`BA�S�A�1'A�\)A��yA���A���AȼjA���Aȴ9AȓuA�^5A��A���A�|�A�O�A�&�A�
=A��;AƼjAư!Aƣ�AƇ+A��A���A���AżjAť�AœuAŅA�jA�G�A�33A�+A�$�A��A�{A�oA�JA�A���A��A��/A���AĴ9AĲ-Aĕ�AāA�^5A�;dA�&�A�oA�JA�A���A��A��yA��HA���A�ƨA�A���Aú^Aò-AîAÝ�A�z�A�ffA�7LA��`A��Aº^APA�z�A�dZA�G�A�7LA�{A���A�z�A�A�A�  A���A�~�A�ZA�  A��A��A��A���A��A�x�A�7LA��`A��A�|�A��\A��\A��PA��hA��DA��A�ZA�5?A��A���A��DA�hsA�E�A��A��A�A��DA�5?A���A�dZA��A��\A�t�A�ffA�\)A�M�A�I�A�G�A�/A�oA���A�A�l�A�  A���A���A��+A�1'A���A�ffA��A���A���A�5?A��A��jA��7A�\)A�I�A�C�A�$�A��A��jA��A��A��A���A��A��!A��A���A���A��hA�K�A� �A���A���A��9A��DA�/A��yA��wA���A��+A�Q�A�"�A��A���A�r�A�=qA�A�A�  A���A���A��yA���A��RA�G�A��^A��7A�bA�dZA�1'A���A���A�&�A��A��HA���A��A�?}A���A�9XA�jA���A��A��#A���A���A���A�r�A�A�A��;A�K�A�"�A��A�
=A���A��mA���A�\)A��A��A��-A�jA�VA�M�A�E�A�7LA�JA�~�A�bNA�S�A�5?A�5?A�33A�VA��TA��RA���A�t�A�I�A�7LA��A��A�A�XA�bA��`A���A��A��A���A�\)A�&�A��!A�bA���A�`BA�M�A�A�A�33A�$�A�
=A��A���A�Q�A���A�JA���A�\)A�+A��A��A�p�A���A�l�A�O�A�{A��A��`A���A�M�A�n�A���A�l�A��A� �A��
A��+A�`BA�ZA�VA�S�A�I�A�(�A��TA���A�t�A�VA�I�A�9XA�oA��A�ȴA��RA��-A���A���A���A���A��hA�x�A�jA�O�A�7LA�"�A��A���A�(�A��A�A�A���A���A�x�A�XA��A�A��A�;dA��A���A��hA�l�A�I�A�?}A�;dA�7LA�1'A�+A�+A�(�A�&�A�"�A��A���A��/A��^A�t�A�9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
ΥB
�<B
ΥB
ΥB
�pB
�<B
ΥB
�B
�<B
�<B
��B
ΥB
ΥB
�<B
��B
�jB
͟B
͟B
��B
��B
�jB
̘B
��B
�aB
�B
�B
�XB
��B
��B
�*B
��B
ӏB
��B
�,B
�B �B�B+BFBoB�B�B"�B'�B-�Bb�Bw�B�7B�$B��B�B�#B�+B&�B-B/B4B7�B>BRTBVmB\)Be�B�B�B�dBΥBƨB��B�B��B�{Bk�BbNB^jBW�BQ�BQ�B`BBQ�B4nB�B_BuB��B�"B�B�)B��B��B�B.Bf�BJXBK^B@�B8RB3hB�B�B
�B
�B
�B
�pB
�B
�_B
�B
��B
x�B
_;B
R B
NpB
H�B
;dB
"hB
�B
�B	��B	�TB	��B	��B	ɺB	�OB	��B	y>B	jKB	ZQB	P}B	F�B	B�B	-wB	�B	�B	�B�xB�%B�B�QB�B�2B�[B��B��B�B�3B��B�HB��B�XB��B��B�B�B��B�7B�7B��B��B��B�B��BcB��B��B�_B��B��B��B~�BxBv�Bv�Bt�Br�Bs�Bm�Bj�Bn/Bl"Bb�B_�B`�B`BB\]B[�BX�BZ�B\�BZ�B^�Bk�Bg8BcTBb�Bd&Bl"BjBrGBxlBy�B|�B~�B}"B|�B~�B{�B�B��B��B�7B�B�B��B�bB�hB�SB�xB��B�OB��B�}B��B��B��B��B�$B��B�XB�vB�B��B��B�B��B�fB�cB��B	�B	�B	�B	\B	{B	FB	�B	�B	.IB	/�B	49B	6�B	:�B	DgB	I�B	J#B	J�B	MjB	R B	S&B	UgB	YKB	Y�B	[#B	\�B	^jB	^�B	a�B	e,B	h�B	m�B	n�B	p;B	o�B	qAB	p;B	m]B	m�B	l"B	i�B	h�B	kQB	l�B	kB	l�B	k�B	f�B	e,B	h
B	q�B	t�B	uZB	z�B	|�B	cB	��B	�AB	�AB	�B	��B	��B	�B	��B	�\B	��B	�B	��B	��B	��B	��B	��B	�=B	�OB	�VB	��B	�hB	��B	�$B	�XB	�6B	�wB	�IB	��B	��B	��B	��B	��B	�B	�FB	�FB	��B	��B	�*B	��B	��B	��B	�BB	�HB	�3B	ȀB	ȀB	�KB	ɆB	��B	��B	��B	�#B	�#B	��B	�)B	̘B	�B	�B	�<B	ΥB	��B	ϫB	��B	��B	҉B	҉B	�,B	�B	��B	��B	�B	ٴB	�WB	��B	�dB	��B	ߤB	ߤB	�B	�HB	�|B	�B	�B	� B	�NB	�B	�HB	�B	�vB	�vB	�BB	�|B	�NB	�B	�B	�,B	�2B	��B	�B	�B	��B	��B	��B	��B	�WB	��B	��B	��B	�AB	�B	�B	�MB	�MB	�MB	�B	��B	��B	�+B	��B	�2B	�fB	��B	�8B	��B	�lB	�>B	�B	�B	�JB	�B	�VB	��B	��B	��B	�.B	��B
  B	��B
 4B
 iB
oB
;B
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
fB

�B
�B
B
JB
�B
�B
"B
VB
�B
(B
\B
�B
�B
�B
�B
�B
bB
.B
:B
�B
uB
�B
�B
B
B
FB
�B
{B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
�B
7B
B
7B
7B
kB
7B
B
B
�B
�B
CB
B
B
B
B
xB
�B
B
xB
�B
!B
!B
!B
!B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"4B
"�B
#B
$�B
&�B
&B
&LB
&�B
'�B
(�B
)*B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*eB
)�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+kB
,=B
,qB
,�B
-B
,�B
,�B
.IB
.�B
/B
.�B
.IB
.IB
.�B
/OB
/�B
/�B
0!B
0!B
0UB
0�B
0�B
1�B
1�B
1�B
2aB
2�B
2�B
2�B
3hB
3�B
3�B
49B
49B
4nB
4�B
4�B
5B
5B
5tB
5�B
5�B
5�B
5�B
6B
6zB
6�B
7B
7LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
͟B
��B
��B
��B
�HB
��B
�pB
�jB
�pB
�B
�jB
�vB
��B
�jB
ΥB
�BB
�jB
�BB
͟B
�pB
�B
͟B
�pB
ϫB
�B
�pB
�B
͟B
��B
�BB
�B
��B
�BB
�<B
�B
��B
͟B
�B
ΥB
̘B
ϫB
��B
�B
�vB
�B
͟B
ϫB
�jB
��B
͟B
�<B
�pB
��B
ϫB
�B
̘B
�BB
�jB
�6B
�B
�6B
��B
��B
��B
��B
��B
��B
�B
ΥB
�pB
͟B
ϫB
�pB
�B
ϫB
�jB
��B
�B
͟B
�jB
�B
ϫB
��B
�vB
ϫB
�BB
��B
�6B
�B
�jB
�0B
�B
�0B
��B
�B
�dB
�B
�6B
�6B
��B
�dB
��B
�B
�6B
͟B
̘B
�B
��B
��B
�pB
��B
��B
��B
͟B
��B
�<B
͟B
�^B
ϫB
�jB
��B
�BB
�<B
͟B
ΥB
��B
��B
�B
��B
�BB
��B
�6B
�BB
�<B
�6B
�6B
͟B
��B
�jB
�dB
�pB
�jB
�0B
�<B
�B
�<B
˒B
�6B
��B
��B
�B
�XB
��B
�B
�EB
ȴB
ȴB
�RB
�EB
ǮB
�?B
�B
��B
�aB
�aB
� B
��B
��B
�B
�[B
��B
��B
�'B
��B
��B
�B
��B
�qB
�}B
��B
��B
�B
��B
�jB
��B
�OB
�^B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�OB
�B
��B
��B
��B
�*B
��B
��B
�B
��B
��B
�aB
�9B
��B
�nB
�?B
��B
�<B
��B
�BB
�B
�#B
�tB
��B
ɆB
��B
��B
خB
�BB
�yB
�)B
�B
�B
ѷB
��B
ϫB
�[B
ҽB
҉B
�aB
�&B
��B
�2B
��B
�[B
�gB
�&B
��B
՛B
��B
��B
��B
�&B
�[B
�,B
ҽB
�[B
��B
�B
�B
�B
�>B
��BGB �BSB�B�B�B�B
	BoB
�.B iB
��B
��B �B  B
�.BABAB�BkB�B{B�BqBB:B�B�B�BbBJBMB�B
=B�B�B�B�BoBBSB�BCBCBCBB �B �B�B�B �B �BB�B"�B!�B"4B#�B �B#�B&�B!�B%�B.}B�B$�B0�B'B%FB+�B'B,qB'B1�B7�B5tB6zBRTBg�BkBn�BpBqvBrGBp;Bv+Bv+Bw2Bw2BzDB.B� B��B�B�B��B��B��B�\B�VB�(B� B�FB�B�eB�VB��B�!B��B��B�B��B�@B��B�_B�hB��BBȀB��BΥB�vB͟B�B�<B�9B��BخB��BݘB��B��B�B�]B�B�B��B�cBB�BB=�B-wB-wB,=B-B*�B/�B2aB3�B1[B.}B*eB$B!�B#:B1�B.}B-B.�B49B7�B1'B3�B2-B1�B1�B3hB5�B9XB9XB8B6�B8B8�B6�B7LB8�B9$B9$B;�B=�B>B>BG�BIBNBOvBPHBWsBS&BT,BS�BU�BV9BU�BW?BWsBVmBU2BU�BW
BWsBYKB\�B]dBf�Be`B`BBe`Bi�Be,Bf�BgmBe,BqB��B�B��B�OB��B�1B�qB�nB��B��B�VB��B�B�jB��B��B��B�zB�?B̘B�jB�)B��B�pB�jB�pBیBɆB�tB˒B�BĜB�BÖBǮB�pB�B�KB��B��B�wB��B��B�zB�B�\B�B��B�xB�VB�B�!B��B�JB�lB�B�lB��B�By>BzB��Bp�Bv`Bm]BkQBh�Bd�Bh�BlWBh�B`B^B_B_�B^5B\]B]dB]dB^BYKB^jB`BBZ�BZ�BP�BXEBS�BU2BT,BN�BM�BQ�BR�BR�BVBS[BWsBN�BOBOBP}BOBBVmB_;B]�Br|Bc B[WBe`B_�BK�BJ�BL�BMB7LB33B4B5�B1�B4nB1[B.B&BFBSB��B�xB�JB��B{B�BB�BB�B �B�B�B+BoB��B 4B�"B�B�GB�oB�oB��B��B�B�"B�B�B�B��B�yB�BܒB�8B�B�TBҽB��B� B�
B��B��B�B�6B��B��B��B�aBȴB�[B�B��B�bB��B��B��B�OB�	B�qB��B��B�IB��B� B��B��B��B��B��BrGBp�Bp�Bh
Bc�BbNBx8Bw�Bb�BV�BdZBI�BE�BN<BF�BB�BC�BD3BJ#BM�BXEBJ�BH�BFtBC�BC-BC�B?�B>wB9�B:�B:�B8�B8B6B7�B6�B6�B7B2-B/�B49B1�B.�B*0B2-B"�B�B4B@BhB@BYBfB!�B�B
��B
�B
�B
�B
�B
�B
�B
�>B
�B
�B
��B
�TB
��B
�>B
��B
�B
�B
�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                         444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B
�)B
�#B
�)B
��B
ʌB
��B
��B
��B
ʌB
��B
�WB
ʌB
ʌB
�#B
��B
��B
ʌB
�#B
ɺB
��B
��B
�#B
�#B
ɺB
��B
�9B
��B
�jB
�XB
��B
��B
�<B
�zB
�B
��B
�B
�|B
��B
��B
��B{B�B�B�B�B�B$@B)�B_BtB��B�tB��B�mB�sB�{B"�B)^B+kB0UB49B:^BN�BR�BXyBa�B�bB�UB��B��B��B��B�UB�@B�Bh>B^�BZ�BS�BM�BN<B\�BM�B0�B	B�B��B�B�rB�WB�yB�BB��B�_B{~BcBF�BG�B<�B4�B/�BBB
�fB
��B
�QB
��B
�aB
��B
�bB
� B
t�B
[�B
NpB
J�B
E9B
7�B
�B
!B
MB	�B	ߤB	�B	�B	�
B	��B	�B	u�B	f�B	V�B	L�B	C,B	?HB	)�B	$B	B	@B��B�uB��B�B�WBтB��B�B�B�dB��B��B��B��B��B�OB��B�nB�bB�B��B��B�LB�CB��B�_B~�B{�B}"B}"B��B��B��B� B{BtSBsBr�BqABo5Bo�BjBg8BjBhrB_B\)B]/B\�BX�BW�BU2BW
BX�BW
B[#Bh>Bc�B_�B_;B`vBhrBffBn�Bt�Bu�Bx�B{JByrBy>B{BxB�iB�B�+B��B�SB�eB��B��B��B��B��B��B��B��B��B�3B�B�B�3B�tB�&BƨB��B�mB�;B�8B��B�B�B��B�B�(B�(B	  B	�B	�B	�B	@B	4B	*�B	+�B	0�B	2�B	6�B	@�B	F?B	FsB	GEB	I�B	NpB	OvB	Q�B	U�B	VB	WsB	YB	Z�B	Z�B	^5B	a|B	d�B	i�B	kB	l�B	l"B	m�B	l�B	i�B	i�B	hrB	f2B	d�B	g�B	iDB	glB	iDB	g�B	cB	a|B	dZB	m�B	qAB	q�B	w1B	y	B	{�B	~(B	~�B	~�B	�iB	�B	�MB	�_B	�CB	��B	��B	�VB	��B	��B	�B	�*B	��B	��B	��B	��B	��B	��B	�9B	�tB	��B	��B	��B	��B	�B	�B	�B	�'B	��B	�gB	��B	��B	�3B	�B	�zB	�B	�#B	��B	��B	��B	��B	��B	��B	ěB	��B	�?B	�B	�B	�sB	�sB	�B	�yB	��B	�QB	�QB	ʌB	��B	�)B	��B	�5B	�<B	��B	��B	�|B	�TB	�,B	�2B	�gB	�B	קB	�B	ٴB	�)B	��B	��B	�cB	ݘB	��B	�B	�B	�pB	ޞB	�B	ݘB	��B	��B	��B	ܒB	��B	ޞB	�B	��B	�|B	�B	�B	�TB	�TB	�%B	�,B	�2B	�8B	�B	�B	�B	�B	�B	�cB	�iB	�B	�B	�B	�B	�B	�B	�{B	�B	�B	�B	�B	�B	�B	��B	��B	�`B	�fB	��B	��B	��B	�B	�DB	��B	�~B	��B	�PB	�B	��B	��B	��B	��B	��B	��B	�.B	�.B
  B
B
B
B
B
B
oB
B
�B
�B
B
MB
B
B
�B
+B
�B
eB
�B

	B

=B

rB

�B
CB
xB
�B
�B
B
IB
IB
B
�B
~B
�B
�B
�B
.B
.B
bB
bB
�B
.B
�B
hB
:B
B
B
B
�B
�B
FB
FB
B
FB
{B
�B
�B
RB
�B
�B
�B
�B
RB
_B
*B
*B
�B
_B
_B
_B
_B
�B
0B
eB
�B
0B
qB
qB
qB
qB
B
=B
=B
IB
B
OB
OB
B
B
�B
!B
UB
 �B
"�B
"hB
"�B
#9B
$B
%FB
%zB
%�B
&LB
&LB
&B
%�B
&B
&B
&�B
&LB
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)*B
)^B
)*B
(�B
*�B
+6B
+kB
+6B
*�B
*�B
+6B
+�B
,<B
,<B
,qB
,qB
,�B
,�B
-BB
-�B
.B
.IB
.�B
.�B
/OB
/OB
/�B
/�B
0 B
0�B
0�B
0�B
0�B
1'B
1[B
1[B
1�B
1�B
2-B
2-B
1�B
2aB
2�B
33B
3gB
3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�#B
�#B
�)B
̘B
�B
��B
ɺB
��B
�dB
ɺB
��B
�)B
ɺB
��B
˒B
ɺB
˒B
��B
��B
�dB
��B
��B
��B
�QB
��B
�^B
��B
�)B
˒B
�QB
�)B
˒B
ʌB
�WB
�)B
��B
�^B
��B
��B
��B
�#B
�QB
��B
�WB
��B
��B
ɺB
�)B
��B
ʌB
��B
�B
��B
�WB
��B
˒B
ɺB
ɆB
�^B
ɆB
�#B
�B
�#B
�/B
�#B
�)B
�WB
��B
��B
��B
��B
��B
�WB
��B
ɺB
�/B
�^B
��B
ɺB
�QB
��B
�#B
��B
��B
˒B
�)B
ɆB
�^B
ɺB
ȀB
�WB
ȀB
�#B
�WB
ȴB
�WB
ɆB
ɆB
�#B
ȴB
�)B
�QB
ɆB
��B
��B
�WB
�)B
�B
��B
�)B
�B
�#B
��B
�KB
ʌB
��B
ǮB
��B
ɺB
�B
˒B
ʌB
��B
��B
�B
�#B
�^B
�B
˒B
�)B
ɆB
˒B
ʌB
ɆB
ɆB
��B
�#B
ɺB
ȴB
��B
ɺB
ȀB
ʌB
�WB
ʌB
��B
ɆB
�EB
�B
�QB
ƨB
�EB
�WB
ÕB
�B
�B
ŢB
ÕB
��B
B
�UB
��B
��B
��B
�pB
�HB
�B
�jB
��B
�B
�B
�wB
�B
�0B
�dB
�B
��B
��B
��B
��B
�RB
�KB
��B
��B
��B
��B
��B
�gB
�3B
��B
��B
�B
��B
�'B
��B
��B
�mB
�LB
�B
��B
�zB
�B
��B
�^B
� B
�B
��B
��B
�OB
��B
��B
�-B
��B
�B
��B
�jB
�sB
��B
�9B
��B
�KB
�/B
��B
˒B
��B
�yB
��B
�WB
�B
�/B
��B
ϫB
�B
��B
бB
�vB
�HB
тB
�HB
ϫB
ѷB
�vB
�HB
��B
�B
�HB
�NB
�vB
ϫB
�|B
�B
ϫB
�EB
��B
�B
�B
�B
�MB
��B
��B�BB
=B�B�BYB
��B
�~B
��B
�B
��B
�"B
�PB
�~B
��B
��B@B�BB�B�B�BhB�BB
�B�B�B�B�BB�BIB4B$B�B�BhB�BLB�B�B�BeBB�BBCBB�BkBB!B�B�B 'BB 'B#9BOB"3B*�BCB �B-BB#nB!�B($B#nB(�B#nB-�B3�B1�B2�BN�Bc�BglBkBlWBm�Bn�Bl�Br{Br{Bs�Bs�Bv�B{~B|PB|�BbB�oB�%B��B��B��B��B�xB�PB��B�nB��B��B�=B�qB�B�IB�UB�!B��B��B��B��B�0B��B��B�9B��B��B��B�dBʌB҉B�,B��B�EB��B�B�8B�
B�B��B��B��B��B iBB	kB9�B)�B)�B(�B)^B'B,<B.�B/�B-�B*�B&�B [B�B�B.B*�B)^B+6B0�B49B-wB/�B.}B.IB.IB/�B2-B5�B5�B4mB33B4mB4�B33B3�B5?B5tB5tB8B9�B:^B:^BC�BEmBJWBK�BL�BS�BOvBP|BPHBQ�BR�BQ�BS�BS�BR�BQ�BQ�BSZBS�BU�BYBY�BcBa�B\�Ba�Bf2Ba|Bb�Bc�Ba|Bm]B��B�kB��B��B�B��B��B��B��B�B��B��B�[B��B�B��B�LB��BB��BɺB�yB�B��BɺB��B��B��B��B��B�mB��B�[B��B��B��B�^BěB�KB��B��B�B�-B��B�UB��B�UB�IB��B��B�UB�qB�1B��B��B�hB��B�4B|Bu�Bv`B~�Bm(Br�Bi�Bg�Bd�BaGBd�Bh�Bd�B\]BZQB[WB[�BZ�BX�BY�BY�BZQBU�BZ�B\�BV�BV�BM5BT�BPHBQ�BP|BK)BJ#BM�BOBOBRTBO�BS�BK)BK^BK^BL�BK�BR�B[�BZBn�B_pBW�Ba�B\)BHBGEBIBIQB3�B/�B0UB1�B.IB0�B-�B*dB"hB�B�B�>B��B��B��B��B@BhBB�bB��B��B��BB{B��B��B��B�rB��B�B�B�B��B�%B��B�rB��B��B��B�B��B�TB��B�B��BΤB�B�BB�pB�ZB� B�9B�^B��B��B�6B�<B��B�B��B�[B�$B��B�CB�B��B��B�YB��B��B�B��B��B�PB�B~�B�:B�'B��Bn�Bm(Bl�BdZB_�B^�Bt�Bs�B_;BS&B`�BE�BB&BJ�BB�B>�B@NB@�BFsBI�BT�BGBEBB�B@B?}B@B<B:�B5�B7KB7KB4�B4mB2aB49B33B2�B3gB.}B,<B0�B.IB+B&�B.}B!B�B�B�B�B�B�B�BB �B
�B
��B
��B
�`B
��B
�`B
�`B
�B
�TB
��B
�GB
ߤB
�AB
�B
�NB
�iB
�TB
�m4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                         444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225012                            20230721225012AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122501220230721225012  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501220230721225012QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501220230721225012QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             