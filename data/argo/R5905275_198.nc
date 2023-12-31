CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-11T09:01:04Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ѭ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � gx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230611090104  20230611090104  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�/���@�/���11  @�/�8�0@�/�8�0@-;e���@-;e����c�����c����11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?u@   @@  @z�H@�  @�G�@�  A ��A��A   A,(�A@��AaG�A�Q�A��A�  A�  A�  A�Q�A�  A�  B   B�B�
B  B�
B(  B0(�B8(�B@Q�BHQ�BP  BW�B_�
Bh  Bp(�Bx(�B�  B��B�{B�{B�  B�{B�{B��B��B�{B�{B�  B�  B�{B�  B�  B��B�  B�(�B�(�B�{B�  B�{B�{B�  B�{B�{B�  B��B�  B�  B�{C 
=C��C  C  C��C
  C  C��C  C  C  C��C  C��C�C  C 
=C"  C$  C%��C'��C)��C+��C.  C0
=C2
=C4
=C6  C8  C9��C;��C>  C@{CB
=CC��CF  CH  CJ  CL  CN  CP  CR
=CT  CU��CW��CY��C\  C^  C`  Cb
=Cd
=Ce��Cg�Ci�Ck��Cn  Co��Cr  Ct
=Cv  Cx  Cz  C|  C~  C�  C�C�  C���C���C���C���C�  C�
=C�C���C���C�C�
=C�C�  C���C�  C�
=C�C�C�  C�  C���C���C�  C�
=C�
=C�C�C�  C�  C�  C�  C���C�C�\C�
=C�C�  C���C���C�  C�  C���C���C���C�  C���C���C�C�C���C�  C�C�  C�  C�C�C�C�C�C�C�C�C�C�  C���C���C�  C�C�C�  C���C��C���C���C�C���C���C���C���C�C�C�
=C�C���C�  C�C�C���C���C���C�  C���C���C�  C�C�
=C�C�  C�C�
=C�C�  C�  C�C�
=C�C�  C�
=C�
=C�C�  C���C���C���C���C���C�  C�C�  C���C�  C�C�
=C�C�  D �D �D�D��D�qD� D  D��D  D� D  D� D�qD� D  D� D  D}qD�qD	z�D
  D
��D�D� D  D}qD�qD� D  Dz�D�qD}qD�qD}qD  D� D  D��D�D� D�qD��D�qDz�D  D}qD�qD}qD��D� DD�D�D��D�D� D  D��D�D��D�D��D  D� D �D � D �qD!� D"�D"��D#�D#� D$�D$� D%  D%� D&  D&� D&�qD'� D(D(��D)  D)}qD*  D*� D+  D+��D,�D,� D,�qD-� D.  D.z�D.�qD/��D0D0��D1�D1� D1�qD2� D3  D3� D4�D4��D5  D5}qD6  D6��D7  D7� D8  D8� D9�D9��D:  D:� D;  D;� D;�qD<� D=�D=� D>  D>� D?  D?��D@  D@� DA  DA� DB�DB� DB��DCz�DD�DD��DE  DE� DF  DF}qDF��DG}qDH�DH��DI�DI��DJ�DJ��DK�DK� DL�DL��DM  DM}qDN  DN��DO  DO��DP  DP}qDQ�DQ� DR  DR� DR�qDSz�DS�qDT��DU  DUz�DU�qDV}qDW  DW� DX  DX� DY�DY� DY�qDZ}qD[  D[� D\�D\��D]�D]��D^  D^��D_�D_}qD`�D`�DaDa�Db  Db}qDc  Dc� Dd  Dd� Dd�qDe}qDe�qDf� Dg  Dg� Dg�qDh}qDi  Di��Dj�Dj� Dk  Dk��DlDl��Dl�qDm� Dn  Dn}qDn�qDo� Dp  Dp��Dq�Dq�Dr  Dr� Ds  Ds}qDt  Dt� Dt�qDu� Dv  Dv��Dw  Dw}qDw�qDx}qDy�Dy��Dz  Dz��D{�D{� D|  D|}qD|�qD}}qD~D~�D�D��D�HD�AHD���D��HD��qD�>�D�� D��HD��D�AHD�� D�� D���D�@ D��HD��HD�HD�>�D�~�D���D���D�=qD�~�D���D�  D�@ D�~�D���D�  D�AHD���D�D�HD�@ D�~�D�� D���D�@ D�� D�� D���D�@ D�� D���D�  D�AHD���D�D�  D�>�D�� D���D��qD�=qD�~�D���D�  D�AHD��HD�� D�  D�AHD�~�D��HD�  D�@ D�� D��qD��qD�@ D�� D���D���D�=qD�~�D�� D�HD�C�D���D��HD�HD�@ D�� D�� D�  D�AHD�� D�� D��D�AHD�}qD���D�  D�AHD���D��HD�  D�@ D��HD�D�HD�@ D��HD�D�HD�AHD�� D���D�HD�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�� D���D��qD�@ D�~�D���D�HD�@ D�}qD��qD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D��HD���D�>�D�� D�� D�  D�@ D��HD�� D��qD�>�D�� D��qD���D�@ D�~�D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�D��HD��HD�HD�AHD���D�� D�  D�@ D�� D�D�HD�@ D��HD��qD��qD�=qD�� D��HD��D�@ D�~�D�� D�HD�AHD�� D��HD�  D�@ D���D��HD�HD�AHD�� D�� D���D�AHD�~�D��qD�  D�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D��HD�HD�>�D�~�D�� D�HD�@ D��HD��HD�HD�B�D��HD���D�  D�@ D�}qD���D�  D�>�D�~�D�D��D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D��HD�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�AHDHD�� D�  D�@ DÀ Dþ�D���D�>�D�~�D�� D�  D�@ DŁHD�� D�  D�@ Dƀ D�� D�HD�@ Dǀ D�� D��qD�>�DȁHD��HD�  D�@ Dɀ D�� D�  D�@ DʁHDʾ�D�  D�AHDˁHD��HD�  D�>�D�~�D̾�D���D�>�D́HD�D�HD�AHD�~�Dξ�D�  D�>�DρHD�� D�  D�B�DЁHD��HD�  D�@ D�~�DѾ�D��qD�=qD�~�D��HD�HD�AHDӀ DӾ�D�  D�AHDԀ DԾ�D��qD�>�D�~�Dվ�D���D�@ Dր D�� D���D�@ DׁHD�� D�  D�@ D؀ Dؾ�D���D�>�Dـ D�� D�HD�B�Dڀ Dھ�D���D�@ Dۀ D۾�D�  D�AHD܀ D�� D�  D�>�D�~�D�� D�  D�@ Dހ D�� D���D�@ D߁HD��HD�HD�>�D�~�D��HD�HD�@ D�HD�� D���D�@ D� D⾸D���D�>�D�~�D�� D���D�@ D� D侸D��qD�>�D�~�D�qD�  D�AHD� D�� D���D�>�D� D��HD�HD�AHD� D�� D�  D�@ D�HD��HD�HD�@ D�~�D꾸D���D�@ D�HD�� D�  D�@ D�HD�� D�  D�>�D� D�� D���D�@ D� D��HD���D�@ D�HD�� D���D�>�D�� D�D���D�>�D�~�D�D�  D�@ D� D�� D�  D�@ D�~�D�� D�  D�@ D�HD��HD��D�AHD�~�D�� D�  D�@ D��HD���D���D�>�D�� D���D���D�@ D�� D�� D���D�=qD�}qD���D�  D�B�D�h�D��?\)?W
=?�\)?�{?��@   @��@�R@333@G�@Tz�@fff@z�H@��@���@�Q�@��\@���@�@��R@Ǯ@�33@�(�@��@���@�A ��AffA
�HA\)Az�A=qA\)A#�
A(��A.�RA3�
A8��A>{AB�\AHQ�AN{AS33AW�A]p�Ab�\Ag�Al(�Aq�Aw�A|��A���A��A�ffA���A��A�{A�G�A�(�A�ffA���A��
A�
=A�G�A��
A�
=A���A��A�ffA�G�A�(�A�ffA���A�(�A�
=A�G�A��
AƸRA��A�(�AθRAљ�A�z�A�
=Aٙ�A��
A޸RAᙚA��
A�{A��A�A�{A�Q�A��HA�{A���A�33A�B Q�BB�HB  B��B�HB�
B	�B
�\B  B�BffB�
Bp�B�RB�
B�B�RB  BG�B�RB(�Bp�B�\B�
B!�B"�\B#�
B$��B&{B'\)B(��B)�B+
=B,(�B-��B.�HB/�
B1�B2=qB3�B4z�B5p�B6�RB8  B9�B:{B;33B<z�B=B>�HB@  BA�BBffBC�BD��BE�BF�HBHQ�BI��BJ�HBK�
BM�BNffBO�BP��BR{BS\)BT��BU�BW33BXQ�BYp�BZ�HB\(�B]p�B^�\B_�B`��Bb=qBc�Bd��Bf{Bg33Bh��Bi�Bk33BlQ�Bmp�Bn�HBp(�Bqp�Br�\Bs�Bt��Bv=qBw�Bx��By�B{
=B|(�B}��B~�HB�{B���B��B�B�ffB��B��B�=qB���B�p�B�{B��RB�\)B��B�ffB��B�B�Q�B���B�p�B�  B��\B�G�B��B��\B��B���B�=qB��HB��B�(�B��RB�G�B��
B�z�B��B�B�Q�B���B�p�B�  B���B�G�B��B��\B��B���B�(�B���B�p�B�{B���B�33B�B�Q�B���B���B�=qB��HB�\)B��B�z�B�
=B��B�Q�B��HB�\)B��B�z�B��B��B�(�B��RB�33B��
B�z�B�
=B���B�{B���B�33B�B�ffB�
=B�B�ffB���B���B�(�B���B��B�=qB��HB��B�=qB��HB��B�{B��RB�p�B�(�B���B��B�(�B���B�p�B�=qB���B��B�ffB�33B��Bģ�B�\)B�  Bƣ�B�\)B�{B���BɮB�ffB��B��B̏\B�G�B�  BθRB�p�B�=qB���B�Bҏ\B�\)B�{B���BՅB�(�B���B�p�B�{Bأ�B�G�BٮB�=qBڸRB��BۅB��B�=qBܣ�B���B�G�Bݙ�B��
B�(�Bޏ\B��HB�33Bߙ�B�  B�Q�B��B���B�G�BᙚB�  B�Q�B�RB�
=B�p�B�B�(�B�\B���B�G�B噚B��B�=qB�\B��HB�G�B�B�  B�z�B���B�33B�p�B��
B�(�B�z�B���B��B�B��B�=qB��B�
=B�\)B�B�{B�ffB�RB��B�\)B�B�{B�z�B�RB��B�B��
B�=qB��B�
=B�p�B�B�(�B�z�B��HB�33B���B��B�=qB���B���B�\)B�B�(�B�z�B��HB�G�B�B�{B�z�B��HB�33B���B��
B�=qB��\B��HB�33B��B�  B�ffB���B��B�p�B��
C �C G�C �C �C �
C
=C=qC\)C��CC�C�CQ�C�C�C�HC
=C=qCp�C��C�
C
=C=qCffC�\CC��C(�CQ�C�C�RC�C{C=qCz�C��C�
C  C(�CQ�C�\C�RC�HC�CG�Cz�C�C�
C	
=C	=qC	ffC	��C	C	��C
(�C
Q�C
�C
�C
�HC{C=qCp�C��C��C��C(�CQ�C�C�C�HC
=C=qCp�C��C��C
=C33Cp�C��C�
C  C=qCp�C��C�
C{CG�Cz�C�C�C�CQ�C�\CC  C(�CffC��C�
C
=CG�Cz�C�C�HC�C\)C�\C��C  C=qCp�C�C�HC{CQ�C�C�RC��C(�C\)C��C�
C{CQ�C��C�
C{C\)C��C�
C{CQ�C�\C��C{C\)C��C�C33C�C��C
=CQ�C��C�
C�CffC�C  CG�C�\C�
C �C ffC ��C �C!33C!z�C!��C"�C"ffC"�C#  C#G�C#�C#�
C$�C$p�C$�RC%
=C%\)C%�C&  C&=qC&�C&��C'�C'z�C'C({C(Q�C(��C(�C)33C)�C)�
C*�C*p�C*�RC+
=C+Q�C+��C+�C,33C,z�C,C-
=C-Q�C-��C-�C.33C.�C.�
C/�C/p�C/�C/��C033C0z�C0��C1{C1ffC1�RC2  C2G�C2�C2��C3{C3\)C3��C3��C4G�C4��C4�
C5�C5\)C5��C5��C6=qC6�\C6��C7{C7Q�C7��C7�HC8(�C8�C8C9{C9Q�C9��C9�
C:�C:p�C:C;  C;G�C;�C;��C<
=C<Q�C<��C<�C=33C=p�C=�RC>  C>Q�C>��C>�
C?{C?\)C?��C?��C@=qC@z�C@�RCA  CAG�CA�\CA�HCB�CB\)CB��CB�HCC�CCz�CC�RCD  CD=qCDz�CD��CE{CEQ�CE�\CE��CF{CFffCF�CF��CG33CGp�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           ?u@   @@  @z�H@�  @�G�@�  A ��A��A   A,(�A@��AaG�A�Q�A��A�  A�  A�  A�Q�A�  A�  B   B�B�
B  B�
B(  B0(�B8(�B@Q�BHQ�BP  BW�B_�
Bh  Bp(�Bx(�B�  B��B�{B�{B�  B�{B�{B��B��B�{B�{B�  B�  B�{B�  B�  B��B�  B�(�B�(�B�{B�  B�{B�{B�  B�{B�{B�  B��B�  B�  B�{C 
=C��C  C  C��C
  C  C��C  C  C  C��C  C��C�C  C 
=C"  C$  C%��C'��C)��C+��C.  C0
=C2
=C4
=C6  C8  C9��C;��C>  C@{CB
=CC��CF  CH  CJ  CL  CN  CP  CR
=CT  CU��CW��CY��C\  C^  C`  Cb
=Cd
=Ce��Cg�Ci�Ck��Cn  Co��Cr  Ct
=Cv  Cx  Cz  C|  C~  C�  C�C�  C���C���C���C���C�  C�
=C�C���C���C�C�
=C�C�  C���C�  C�
=C�C�C�  C�  C���C���C�  C�
=C�
=C�C�C�  C�  C�  C�  C���C�C�\C�
=C�C�  C���C���C�  C�  C���C���C���C�  C���C���C�C�C���C�  C�C�  C�  C�C�C�C�C�C�C�C�C�C�  C���C���C�  C�C�C�  C���C��C���C���C�C���C���C���C���C�C�C�
=C�C���C�  C�C�C���C���C���C�  C���C���C�  C�C�
=C�C�  C�C�
=C�C�  C�  C�C�
=C�C�  C�
=C�
=C�C�  C���C���C���C���C���C�  C�C�  C���C�  C�C�
=C�C�  D �D �D�D��D�qD� D  D��D  D� D  D� D�qD� D  D� D  D}qD�qD	z�D
  D
��D�D� D  D}qD�qD� D  Dz�D�qD}qD�qD}qD  D� D  D��D�D� D�qD��D�qDz�D  D}qD�qD}qD��D� DD�D�D��D�D� D  D��D�D��D�D��D  D� D �D � D �qD!� D"�D"��D#�D#� D$�D$� D%  D%� D&  D&� D&�qD'� D(D(��D)  D)}qD*  D*� D+  D+��D,�D,� D,�qD-� D.  D.z�D.�qD/��D0D0��D1�D1� D1�qD2� D3  D3� D4�D4��D5  D5}qD6  D6��D7  D7� D8  D8� D9�D9��D:  D:� D;  D;� D;�qD<� D=�D=� D>  D>� D?  D?��D@  D@� DA  DA� DB�DB� DB��DCz�DD�DD��DE  DE� DF  DF}qDF��DG}qDH�DH��DI�DI��DJ�DJ��DK�DK� DL�DL��DM  DM}qDN  DN��DO  DO��DP  DP}qDQ�DQ� DR  DR� DR�qDSz�DS�qDT��DU  DUz�DU�qDV}qDW  DW� DX  DX� DY�DY� DY�qDZ}qD[  D[� D\�D\��D]�D]��D^  D^��D_�D_}qD`�D`�DaDa�Db  Db}qDc  Dc� Dd  Dd� Dd�qDe}qDe�qDf� Dg  Dg� Dg�qDh}qDi  Di��Dj�Dj� Dk  Dk��DlDl��Dl�qDm� Dn  Dn}qDn�qDo� Dp  Dp��Dq�Dq�Dr  Dr� Ds  Ds}qDt  Dt� Dt�qDu� Dv  Dv��Dw  Dw}qDw�qDx}qDy�Dy��Dz  Dz��D{�D{� D|  D|}qD|�qD}}qD~D~�D�D��D�HD�AHD���D��HD��qD�>�D�� D��HD��D�AHD�� D�� D���D�@ D��HD��HD�HD�>�D�~�D���D���D�=qD�~�D���D�  D�@ D�~�D���D�  D�AHD���D�D�HD�@ D�~�D�� D���D�@ D�� D�� D���D�@ D�� D���D�  D�AHD���D�D�  D�>�D�� D���D��qD�=qD�~�D���D�  D�AHD��HD�� D�  D�AHD�~�D��HD�  D�@ D�� D��qD��qD�@ D�� D���D���D�=qD�~�D�� D�HD�C�D���D��HD�HD�@ D�� D�� D�  D�AHD�� D�� D��D�AHD�}qD���D�  D�AHD���D��HD�  D�@ D��HD�D�HD�@ D��HD�D�HD�AHD�� D���D�HD�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�� D���D��qD�@ D�~�D���D�HD�@ D�}qD��qD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D��HD���D�>�D�� D�� D�  D�@ D��HD�� D��qD�>�D�� D��qD���D�@ D�~�D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�D��HD��HD�HD�AHD���D�� D�  D�@ D�� D�D�HD�@ D��HD��qD��qD�=qD�� D��HD��D�@ D�~�D�� D�HD�AHD�� D��HD�  D�@ D���D��HD�HD�AHD�� D�� D���D�AHD�~�D��qD�  D�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D��HD�HD�>�D�~�D�� D�HD�@ D��HD��HD�HD�B�D��HD���D�  D�@ D�}qD���D�  D�>�D�~�D�D��D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D��HD�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�AHDHD�� D�  D�@ DÀ Dþ�D���D�>�D�~�D�� D�  D�@ DŁHD�� D�  D�@ Dƀ D�� D�HD�@ Dǀ D�� D��qD�>�DȁHD��HD�  D�@ Dɀ D�� D�  D�@ DʁHDʾ�D�  D�AHDˁHD��HD�  D�>�D�~�D̾�D���D�>�D́HD�D�HD�AHD�~�Dξ�D�  D�>�DρHD�� D�  D�B�DЁHD��HD�  D�@ D�~�DѾ�D��qD�=qD�~�D��HD�HD�AHDӀ DӾ�D�  D�AHDԀ DԾ�D��qD�>�D�~�Dվ�D���D�@ Dր D�� D���D�@ DׁHD�� D�  D�@ D؀ Dؾ�D���D�>�Dـ D�� D�HD�B�Dڀ Dھ�D���D�@ Dۀ D۾�D�  D�AHD܀ D�� D�  D�>�D�~�D�� D�  D�@ Dހ D�� D���D�@ D߁HD��HD�HD�>�D�~�D��HD�HD�@ D�HD�� D���D�@ D� D⾸D���D�>�D�~�D�� D���D�@ D� D侸D��qD�>�D�~�D�qD�  D�AHD� D�� D���D�>�D� D��HD�HD�AHD� D�� D�  D�@ D�HD��HD�HD�@ D�~�D꾸D���D�@ D�HD�� D�  D�@ D�HD�� D�  D�>�D� D�� D���D�@ D� D��HD���D�@ D�HD�� D���D�>�D�� D�D���D�>�D�~�D�D�  D�@ D� D�� D�  D�@ D�~�D�� D�  D�@ D�HD��HD��D�AHD�~�D�� D�  D�@ D��HD���D���D�>�D�� D���D���D�@ D�� D�� D���D�=qD�}qD���D�  D�B�D�h�D��?\)?W
=?�\)?�{?��@   @��@�R@333@G�@Tz�@fff@z�H@��@���@�Q�@��\@���@�@��R@Ǯ@�33@�(�@��@���@�A ��AffA
�HA\)Az�A=qA\)A#�
A(��A.�RA3�
A8��A>{AB�\AHQ�AN{AS33AW�A]p�Ab�\Ag�Al(�Aq�Aw�A|��A���A��A�ffA���A��A�{A�G�A�(�A�ffA���A��
A�
=A�G�A��
A�
=A���A��A�ffA�G�A�(�A�ffA���A�(�A�
=A�G�A��
AƸRA��A�(�AθRAљ�A�z�A�
=Aٙ�A��
A޸RAᙚA��
A�{A��A�A�{A�Q�A��HA�{A���A�33A�B Q�BB�HB  B��B�HB�
B	�B
�\B  B�BffB�
Bp�B�RB�
B�B�RB  BG�B�RB(�Bp�B�\B�
B!�B"�\B#�
B$��B&{B'\)B(��B)�B+
=B,(�B-��B.�HB/�
B1�B2=qB3�B4z�B5p�B6�RB8  B9�B:{B;33B<z�B=B>�HB@  BA�BBffBC�BD��BE�BF�HBHQ�BI��BJ�HBK�
BM�BNffBO�BP��BR{BS\)BT��BU�BW33BXQ�BYp�BZ�HB\(�B]p�B^�\B_�B`��Bb=qBc�Bd��Bf{Bg33Bh��Bi�Bk33BlQ�Bmp�Bn�HBp(�Bqp�Br�\Bs�Bt��Bv=qBw�Bx��By�B{
=B|(�B}��B~�HB�{B���B��B�B�ffB��B��B�=qB���B�p�B�{B��RB�\)B��B�ffB��B�B�Q�B���B�p�B�  B��\B�G�B��B��\B��B���B�=qB��HB��B�(�B��RB�G�B��
B�z�B��B�B�Q�B���B�p�B�  B���B�G�B��B��\B��B���B�(�B���B�p�B�{B���B�33B�B�Q�B���B���B�=qB��HB�\)B��B�z�B�
=B��B�Q�B��HB�\)B��B�z�B��B��B�(�B��RB�33B��
B�z�B�
=B���B�{B���B�33B�B�ffB�
=B�B�ffB���B���B�(�B���B��B�=qB��HB��B�=qB��HB��B�{B��RB�p�B�(�B���B��B�(�B���B�p�B�=qB���B��B�ffB�33B��Bģ�B�\)B�  Bƣ�B�\)B�{B���BɮB�ffB��B��B̏\B�G�B�  BθRB�p�B�=qB���B�Bҏ\B�\)B�{B���BՅB�(�B���B�p�B�{Bأ�B�G�BٮB�=qBڸRB��BۅB��B�=qBܣ�B���B�G�Bݙ�B��
B�(�Bޏ\B��HB�33Bߙ�B�  B�Q�B��B���B�G�BᙚB�  B�Q�B�RB�
=B�p�B�B�(�B�\B���B�G�B噚B��B�=qB�\B��HB�G�B�B�  B�z�B���B�33B�p�B��
B�(�B�z�B���B��B�B��B�=qB��B�
=B�\)B�B�{B�ffB�RB��B�\)B�B�{B�z�B�RB��B�B��
B�=qB��B�
=B�p�B�B�(�B�z�B��HB�33B���B��B�=qB���B���B�\)B�B�(�B�z�B��HB�G�B�B�{B�z�B��HB�33B���B��
B�=qB��\B��HB�33B��B�  B�ffB���B��B�p�B��
C �C G�C �C �C �
C
=C=qC\)C��CC�C�CQ�C�C�C�HC
=C=qCp�C��C�
C
=C=qCffC�\CC��C(�CQ�C�C�RC�C{C=qCz�C��C�
C  C(�CQ�C�\C�RC�HC�CG�Cz�C�C�
C	
=C	=qC	ffC	��C	C	��C
(�C
Q�C
�C
�C
�HC{C=qCp�C��C��C��C(�CQ�C�C�C�HC
=C=qCp�C��C��C
=C33Cp�C��C�
C  C=qCp�C��C�
C{CG�Cz�C�C�C�CQ�C�\CC  C(�CffC��C�
C
=CG�Cz�C�C�HC�C\)C�\C��C  C=qCp�C�C�HC{CQ�C�C�RC��C(�C\)C��C�
C{CQ�C��C�
C{C\)C��C�
C{CQ�C�\C��C{C\)C��C�C33C�C��C
=CQ�C��C�
C�CffC�C  CG�C�\C�
C �C ffC ��C �C!33C!z�C!��C"�C"ffC"�C#  C#G�C#�C#�
C$�C$p�C$�RC%
=C%\)C%�C&  C&=qC&�C&��C'�C'z�C'C({C(Q�C(��C(�C)33C)�C)�
C*�C*p�C*�RC+
=C+Q�C+��C+�C,33C,z�C,C-
=C-Q�C-��C-�C.33C.�C.�
C/�C/p�C/�C/��C033C0z�C0��C1{C1ffC1�RC2  C2G�C2�C2��C3{C3\)C3��C3��C4G�C4��C4�
C5�C5\)C5��C5��C6=qC6�\C6��C7{C7Q�C7��C7�HC8(�C8�C8C9{C9Q�C9��C9�
C:�C:p�C:C;  C;G�C;�C;��C<
=C<Q�C<��C<�C=33C=p�C=�RC>  C>Q�C>��C>�
C?{C?\)C?��C?��C@=qC@z�C@�RCA  CAG�CA�\CA�HCB�CB\)CB��CB�HCC�CCz�CC�RCD  CD=qCDz�CD��CE{CEQ�CE�\CE��CF{CFffCF�CF��CG33CGp�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�=qA�9XA�;dA�9XA�9XA�7LA�5?A�=qA�A�A�A�A�C�A�C�A�C�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�G�A�E�A�/A�bA�
=A��yA�n�A�ZA�S�A�O�A�K�A�A�A�7LA��A�  A�ƨA·+A�r�A�^5A���A�K�A���A̅Aɏ\A��A�5?A�S�A�VA��A��A�VA��7A�$�A�A���A��9A�x�A��TA�XA���A��-A�dZA�%A�9XA���A�\)A��A�1'A�
=A�%A��7A� �A��7A�(�A��yA�p�A��Au�An�An1Al$�Ae��Ab�`A`�HA^�!AZ�AWhsAT�9AS
=ANĜAL9XAJ�AF�AD��ACO�AA7LA?A=�7A<1'A:�A9�wA8�uA5�wA4�!A3��A3�-A4�!A5
=A3�mA3�hA2�9A3&�A3
=A2$�A/&�A-|�A-�
A.��A-�TA*JA(A�A(  A(  A'dZA'�A&�!A%�-A$��A$��A$r�A%t�A'�A&��A#��A#�A$A$(�A$ZA$A#G�A#VA"�yA"��A" �A!�
A!��A �yA n�A M�A��AAS�A�A��A{A\)A&�A��AM�A�-AVA��A1'A�AƨA;dA�HA�jA�\A=qAA��At�AXAC�A?}A33AAz�A �A��Ax�A�`An�A5?AJA��Ax�AS�A�A�A�uA9XA�;A��A"�A�A+A�A��A��An�A�FAG�AXAK�A7LA�A��AbNA�#A�PAS�AXA33AA�9A(�A�AA��A\)A
��A
��A
ffA	�;A	��A	K�A�jAn�A1'A1A�-Ap�A�A^5A1'A1A�#A��A�7AC�A�!AffA-A  A�^Ax�A+A%A��A~�A{A�#A��A�A �!A r�A z�A I�A �@��@��@�v�@��T@�?}@�Z@��@�@�M�@���@��@���@�S�@��@��+@�V@���@��-@��u@�l�@�o@�o@�E�@�O�@�r�@��
@��@�~�@��@�O�@�Ĝ@�I�@�P@ꗍ@�-@�V@�z�@�1@�l�@�!@�@�?}@�@���@�\)@◍@�ff@�J@��@�%@�r�@��
@ߕ�@�33@�5?@�@�7L@ܓu@��
@�o@�E�@�-@�$�@���@�/@أ�@ץ�@�K�@�ff@ՙ�@ԋD@��@Ӯ@�l�@Ұ!@�=q@��@с@�%@�bN@��
@ϕ�@�S�@�C�@��@ͩ�@�&�@�bN@��
@�l�@�C�@�o@�ȴ@ʏ\@�E�@�@ə�@�O�@�%@�Z@��;@�;d@�
=@ư!@�=q@��@�@š�@�hs@ě�@Ý�@�\)@�"�@�@��@¸R@���@��h@�hs@�/@���@�r�@�S�@��\@�=q@�$�@��@�{@��#@�`B@���@���@��w@���@���@��P@�o@��H@���@��R@�n�@�=q@��@��#@���@�hs@�%@��j@��@�"�@���@�ff@�E�@�$�@���@���@���@��h@�p�@�`B@�G�@�&�@��@�bN@���@���@�\)@�C�@�+@��@��!@�n�@�J@��@���@��7@�O�@���@���@� �@��
@���@�S�@��@���@���@�~�@�-@��^@��/@�Q�@�9X@���@�ƨ@���@�\)@�@�v�@���@�`B@�?}@��@��`@��D@�1'@��
@���@�t�@��y@���@��!@���@�V@�M�@�$�@���@�p�@�%@�Ĝ@��@�I�@�b@�  @��@���@�t�@��@���@�ff@�@�p�@��@��D@�I�@���@�K�@�@�ff@���@�`B@��@���@���@��D@��;@�l�@�S�@�33@�+@�@��H@��!@�-@�@���@���@�G�@��@��`@���@��m@�l�@�33@���@���@�E�@�J@���@�7L@���@��@��@���@�S�@�o@�ȴ@�^5@���@���@�p�@�7L@���@��j@���@��u@�r�@�I�@��;@���@��@���@�v�@��@���@�O�@���@��@�Ĝ@��@��@�A�@���@�|�@�l�@�t�@�l�@�l�@�dZ@�S�@�S�@�K�@�C�@�33@�
=@��y@���@�M�@��@��T@��-@��h@��@���@��@�|�@�C�@��@��y@���@�~�@�$�@���@��^@���@��h@�p�@�O�@�&�@��`@��@��u@��D@�j@�I�@��@���@�l�@��H@�ȴ@��!@��\@�~�@�^5@�5?@��@�x�@�V@���@��u@�Q�@�@�@|�@
=@~ȴ@~v�@}�@}��@}V@|z�@|1@z�H@zJ@y�^@yx�@y&�@x��@x�`@x�u@xA�@w�;@wl�@w+@w
=@v�@v��@v{@u��@up�@u/@t�@t1@s"�@r��@r^5@q�#@qx�@q�@p�`@p�u@pQ�@p �@o�@n��@nV@n{@m�-@mp�@m?}@lj@k��@j�H@j�\@j�@i�^@h��@g�;@gl�@g�@f��@fȴ@fff@fE�@e�T@e�h@eO�@e/@d�@d�D@d�D@d�D@d�D@d9X@c��@c"�@b��@bn�@bM�@b=q@b�@a��@a��@a�@`Q�@_�;@_|�@_�@^�@]@\��@\�j@\�D@\z�@\z�@\Z@\1@[�
@["�@Z�@Yx�@YG�@Y7L@Y7L@Y&�@Y%@X�9@W�@W\)@V��@V��@V�+@VE�@U@U�@Tz�@T9X@T1@S�
@S33@S@R��@R^5@Q�#@Q�7@P��@P�9@P �@O��@O+@N��@Nȴ@N�R@NV@N@M`B@L��@L�@L9X@KdZ@J��@I�@Ix�@IG�@I%@H�u@HA�@G�@G�P@G\)@GK�@G�@FV@E�T@E?}@D�@D�D@D�@C�
@C�F@C�@Ct�@CdZ@CdZ@C33@C"�@B�@B�!@B��@BM�@A��@A�@A��@A&�@@�9@@1'@@ �@@  @?��@?l�@?+@?�@>�y@>�R@>��@>��@>�+@>ff@>5?@=��@=�@=`B@=�@<�D@<�@;��@;�F@:�@:��@:��@:^5@:J@97L@8��@8��@8Q�@7��@7+@6�@6v�@65?@5�@5/@4��@4I�@3�m@3��@3S�@3o@2��@2^5@2-@1�#@1G�@0��@0�u@0�u@0��@0�u@01'@/��@/
=@.��@.$�@.{@-`B@,��@-/@-?}@-V@,�D@,I�@,9X@+�
@+S�@+"�@*��@*M�@)�^@)G�@(Ĝ@(�u@(�@(�@(�@(r�@(b@'�@'��@'�@&�@&v�@&v�@&E�@&{@%��@%O�@$��@$�@$j@#�m@#�
@#ƨ@#�@#o@"�H@"n�@"M�@"-@!�@!��@!��@!�7@!X@!7L@!�@!%@ ��@ �@ r�@ A�@  �@�@�@�P@K�@��@��@v�@5?@@�T@@@�-@��@p�@V@�/@�D@I�@1@�m@ƨ@��@t�@C�@@��@�!@^5@-@�@�#@�7@x�@G�@Ĝ@��@r�@  @��@�w@�@�P@�@ȴ@�R@��@ff@E�@@O�@V@�@�D@�
@ƨ@�F@��@�@dZ@33@33@o@o@o@o@o@�@��@��@��@n�@=q@J@��@G�@��@��@�9@�@bN@A�@b@�@�w@��@��@|�@K�@;d@+A�/A�33A�;dA�;dA�?}A�?}A�;dA�;dA�;dA�9XA�7LA�7LA�?}A�;dA�7LA�9XA�9XA�;dA�9XA�5?A�9XA�9XA�7LA�5?A�5?A�5?A�7LA�9XA�9XA�;dA�?}A�A�A�A�A�?}A�A�A�E�A�C�A�A�A�?}A�A�A�E�A�C�A�?}A�A�A�E�A�C�A�A�A�C�A�G�A�E�A�A�A�C�A�G�A�G�A�C�A�A�A�E�A�G�A�E�A�C�A�E�A�I�A�G�A�C�A�G�A�I�A�E�A�E�A�I�A�I�A�G�A�E�A�I�A�K�A�I�A�E�A�I�A�K�A�K�A�G�A�G�A�K�A�M�A�I�A�G�A�I�A�M�A�K�A�I�A�I�A�M�A�M�A�I�A�I�A�I�A�O�A�K�A�I�A�M�A�O�A�K�A�I�A�K�A�M�A�I�A�I�A�K�A�O�A�K�A�G�A�K�A�K�A�M�A�K�A�G�A�K�A�M�A�K�A�K�A�O�A�O�A�K�A�I�A�K�A�M�A�M�A�K�A�G�A�E�A�G�A�G�A�C�A�A�A�E�A�G�A�A�A�C�A�E�A�E�A�E�A�A�A�?}A�A�A�?}A�7LA��A���A�
=A�bA�bA�bA�{A�bA�VA�A�A�A�
=A�oA��A�oA�{A���A���A��
A���Aϥ�AϑhA�t�A�ffA�bNA�bNA�bNA�`BA�\)A�ZA�XA�ZA�ZA�XA�S�A�Q�A�S�A�VA�VA�Q�A�O�A�Q�A�Q�A�Q�A�M�A�K�A�K�A�K�A�M�A�M�A�K�A�G�A�E�A�E�A�G�A�E�A�A�A�?}A�=qA�=qA�?}A�;dA�5?A�33A�33A�1'A�(�A�$�A��A��A��A�{A�bA�1A�A�  A�  A���A���A���A��yA���A���AμjAζFAάAΡ�AΙ�A΍PA΁A΁A�~�A�z�A�v�A�t�A�r�A�t�A�r�A�p�A�l�A�hsA�dZA�^5A�^5A�\)A�Q�A�;dA�bA��;A���AͼjAͩ�A̓A�dZA�ZA�O�A�I�A�G�A�E�A�=qA�-A�{A�  A���A��A��`A��
A���A�ƨA̮A̓uÁA�ffA�$�A���A�ȴA�t�A�I�A�A���AȸRA�hsA�S�A�hsA��;A�$�A��!A�9XA��/A�z�A�{A�ƨA�Q�A�/A�ĜA���A�E�A�bA���A��#A��DA��wA�G�A�
=A�ffA��A�+A��A��A���A��A��A���A��yA��TA��A�hsA�oA���A�VA���A�G�A��A���A��A�1'A��TA��hA�ffA�VA��A�r�A��A���A�S�A��HA���A�`BA�G�A�;dA�33A�(�A�bA���A��TA�ȴA���A�r�A�G�A�9XA�33A��A��HA���A���A��jA��9A���A���A��PA�/A�ȴA�A��9A��A���A���A�x�A�dZA�O�A�;dA�&�A��A�JA�  A���A���A��A��`A��;A���A�ȴA���A��RA��!A���A���A���A�ffA�S�A�C�A� �A��A��A��A�oA�
=A��A���A��!A��\A�z�A�jA�bNA�VA�9XA�
=A���A��`A��
A���A���A��A���A��A��A��A�x�A�^5A� �A���A��7A�M�A�(�A�oA��A���A��jA��uA�ffA�Q�A�=qA�"�A�VA���A���A���A��FA��!A���A�|�A�n�A�XA�I�A�A�A�1'A�(�A� �A�{A���A��
A��RA�|�A�E�A�-A�A�ĜA�G�A���A��A���A�hsA��A��HA���A�S�A�$�A���A�ƨA��wA��RA��9A��-A��!A���A���A��uA���A��DA�x�A�XA�+A��A���A���A��yA���A�~�A�bNA��A���A���A�A�G�A��A���A�+A���A���A��\A�|�A�l�A�ZA�E�A�/A��A��/A��+A�;dA���A���A�^5A�;dA�-A�JA���A��`A�ĜA���A�t�A�1A�\)A�x�A��+A���A��A�A��DA�`BA�5?A��A��/A���A��9A�x�A�VA�;dA�
=A��A���A�\)A�1A��;A��^A���A�n�A�K�A�33A�+A�$�A�
=A��A��
A��9A��A�dZA�M�A�;dA�A���A��PA�1A�l�A��FA��^A�jA�jA�`BA�C�A�M�A�9XA��A�z�A��A�  A�oA~ �A{�A{Az��Az=qAx��Au+Ar�RAq|�Ap�/Ao��Ao��AoS�An��An�RAnr�AnI�An(�An{An�An�An�AnbAnAm�-AmC�Al�yAl�\AlM�AlAk�Ai�;AhI�Af��Ae��Ae33Ad��Ad��AdVAc�;AcK�Ab�HAb=qAa��Aa�FAa`BA`��A`�A`�A`��A`�DA`M�A_�mA_hsA^��A^E�A^bA]�FA]/A\n�A[��AZ��AZAY��AY�FAY\)AX�HAXbNAW��AV��AU��AU�wAUt�AU+AT�HAT�9AT��ATn�AT �ATAS��AS�AS;dASAR��AQ��AQ33AP=qAO��AN��AN �AM�AMAL�!AL�ALbNAL9XAL{ALAK�#AK�wAK��AK�7AK\)AK�AJ��AIl�AG��AGK�AGVAF�RAF �AE�-AEt�AE"�AEVAE%AD��AD��AD�AD�`AD�HAD�AD{AC�AB��AB��AB�9AB�AB��ABz�ABAA��AA&�A@=qA?�mA?��A?x�A?+A?A?A>�A>��A>jA>E�A=�mA=�A=XA=;dA=/A=&�A=VA<�/A<��A<I�A<1A;�#A;�hA;XA;?}A;&�A;VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           A�7LA�=qA�9XA�;dA�9XA�9XA�7LA�5?A�=qA�A�A�A�A�C�A�C�A�C�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�G�A�E�A�/A�bA�
=A��yA�n�A�ZA�S�A�O�A�K�A�A�A�7LA��A�  A�ƨA·+A�r�A�^5A���A�K�A���A̅Aɏ\A��A�5?A�S�A�VA��A��A�VA��7A�$�A�A���A��9A�x�A��TA�XA���A��-A�dZA�%A�9XA���A�\)A��A�1'A�
=A�%A��7A� �A��7A�(�A��yA�p�A��Au�An�An1Al$�Ae��Ab�`A`�HA^�!AZ�AWhsAT�9AS
=ANĜAL9XAJ�AF�AD��ACO�AA7LA?A=�7A<1'A:�A9�wA8�uA5�wA4�!A3��A3�-A4�!A5
=A3�mA3�hA2�9A3&�A3
=A2$�A/&�A-|�A-�
A.��A-�TA*JA(A�A(  A(  A'dZA'�A&�!A%�-A$��A$��A$r�A%t�A'�A&��A#��A#�A$A$(�A$ZA$A#G�A#VA"�yA"��A" �A!�
A!��A �yA n�A M�A��AAS�A�A��A{A\)A&�A��AM�A�-AVA��A1'A�AƨA;dA�HA�jA�\A=qAA��At�AXAC�A?}A33AAz�A �A��Ax�A�`An�A5?AJA��Ax�AS�A�A�A�uA9XA�;A��A"�A�A+A�A��A��An�A�FAG�AXAK�A7LA�A��AbNA�#A�PAS�AXA33AA�9A(�A�AA��A\)A
��A
��A
ffA	�;A	��A	K�A�jAn�A1'A1A�-Ap�A�A^5A1'A1A�#A��A�7AC�A�!AffA-A  A�^Ax�A+A%A��A~�A{A�#A��A�A �!A r�A z�A I�A �@��@��@�v�@��T@�?}@�Z@��@�@�M�@���@��@���@�S�@��@��+@�V@���@��-@��u@�l�@�o@�o@�E�@�O�@�r�@��
@��@�~�@��@�O�@�Ĝ@�I�@�P@ꗍ@�-@�V@�z�@�1@�l�@�!@�@�?}@�@���@�\)@◍@�ff@�J@��@�%@�r�@��
@ߕ�@�33@�5?@�@�7L@ܓu@��
@�o@�E�@�-@�$�@���@�/@أ�@ץ�@�K�@�ff@ՙ�@ԋD@��@Ӯ@�l�@Ұ!@�=q@��@с@�%@�bN@��
@ϕ�@�S�@�C�@��@ͩ�@�&�@�bN@��
@�l�@�C�@�o@�ȴ@ʏ\@�E�@�@ə�@�O�@�%@�Z@��;@�;d@�
=@ư!@�=q@��@�@š�@�hs@ě�@Ý�@�\)@�"�@�@��@¸R@���@��h@�hs@�/@���@�r�@�S�@��\@�=q@�$�@��@�{@��#@�`B@���@���@��w@���@���@��P@�o@��H@���@��R@�n�@�=q@��@��#@���@�hs@�%@��j@��@�"�@���@�ff@�E�@�$�@���@���@���@��h@�p�@�`B@�G�@�&�@��@�bN@���@���@�\)@�C�@�+@��@��!@�n�@�J@��@���@��7@�O�@���@���@� �@��
@���@�S�@��@���@���@�~�@�-@��^@��/@�Q�@�9X@���@�ƨ@���@�\)@�@�v�@���@�`B@�?}@��@��`@��D@�1'@��
@���@�t�@��y@���@��!@���@�V@�M�@�$�@���@�p�@�%@�Ĝ@��@�I�@�b@�  @��@���@�t�@��@���@�ff@�@�p�@��@��D@�I�@���@�K�@�@�ff@���@�`B@��@���@���@��D@��;@�l�@�S�@�33@�+@�@��H@��!@�-@�@���@���@�G�@��@��`@���@��m@�l�@�33@���@���@�E�@�J@���@�7L@���@��@��@���@�S�@�o@�ȴ@�^5@���@���@�p�@�7L@���@��j@���@��u@�r�@�I�@��;@���@��@���@�v�@��@���@�O�@���@��@�Ĝ@��@��@�A�@���@�|�@�l�@�t�@�l�@�l�@�dZ@�S�@�S�@�K�@�C�@�33@�
=@��y@���@�M�@��@��T@��-@��h@��@���@��@�|�@�C�@��@��y@���@�~�@�$�@���@��^@���@��h@�p�@�O�@�&�@��`@��@��u@��D@�j@�I�@��@���@�l�@��H@�ȴ@��!@��\@�~�@�^5@�5?@��@�x�@�V@���@��u@�Q�@�@�@|�@
=@~ȴ@~v�@}�@}��@}V@|z�@|1@z�H@zJ@y�^@yx�@y&�@x��@x�`@x�u@xA�@w�;@wl�@w+@w
=@v�@v��@v{@u��@up�@u/@t�@t1@s"�@r��@r^5@q�#@qx�@q�@p�`@p�u@pQ�@p �@o�@n��@nV@n{@m�-@mp�@m?}@lj@k��@j�H@j�\@j�@i�^@h��@g�;@gl�@g�@f��@fȴ@fff@fE�@e�T@e�h@eO�@e/@d�@d�D@d�D@d�D@d�D@d9X@c��@c"�@b��@bn�@bM�@b=q@b�@a��@a��@a�@`Q�@_�;@_|�@_�@^�@]@\��@\�j@\�D@\z�@\z�@\Z@\1@[�
@["�@Z�@Yx�@YG�@Y7L@Y7L@Y&�@Y%@X�9@W�@W\)@V��@V��@V�+@VE�@U@U�@Tz�@T9X@T1@S�
@S33@S@R��@R^5@Q�#@Q�7@P��@P�9@P �@O��@O+@N��@Nȴ@N�R@NV@N@M`B@L��@L�@L9X@KdZ@J��@I�@Ix�@IG�@I%@H�u@HA�@G�@G�P@G\)@GK�@G�@FV@E�T@E?}@D�@D�D@D�@C�
@C�F@C�@Ct�@CdZ@CdZ@C33@C"�@B�@B�!@B��@BM�@A��@A�@A��@A&�@@�9@@1'@@ �@@  @?��@?l�@?+@?�@>�y@>�R@>��@>��@>�+@>ff@>5?@=��@=�@=`B@=�@<�D@<�@;��@;�F@:�@:��@:��@:^5@:J@97L@8��@8��@8Q�@7��@7+@6�@6v�@65?@5�@5/@4��@4I�@3�m@3��@3S�@3o@2��@2^5@2-@1�#@1G�@0��@0�u@0�u@0��@0�u@01'@/��@/
=@.��@.$�@.{@-`B@,��@-/@-?}@-V@,�D@,I�@,9X@+�
@+S�@+"�@*��@*M�@)�^@)G�@(Ĝ@(�u@(�@(�@(�@(r�@(b@'�@'��@'�@&�@&v�@&v�@&E�@&{@%��@%O�@$��@$�@$j@#�m@#�
@#ƨ@#�@#o@"�H@"n�@"M�@"-@!�@!��@!��@!�7@!X@!7L@!�@!%@ ��@ �@ r�@ A�@  �@�@�@�P@K�@��@��@v�@5?@@�T@@@�-@��@p�@V@�/@�D@I�@1@�m@ƨ@��@t�@C�@@��@�!@^5@-@�@�#@�7@x�@G�@Ĝ@��@r�@  @��@�w@�@�P@�@ȴ@�R@��@ff@E�@@O�@V@�@�D@�
@ƨ@�F@��@�@dZ@33@33@o@o@o@o@o@�@��@��@��@n�@=q@J@��@G�@��@��@�9@�@bN@A�@b@�@�w@��@��@|�@K�@;d@+A�/A�33A�;dA�;dA�?}A�?}A�;dA�;dA�;dA�9XA�7LA�7LA�?}A�;dA�7LA�9XA�9XA�;dA�9XA�5?A�9XA�9XA�7LA�5?A�5?A�5?A�7LA�9XA�9XA�;dA�?}A�A�A�A�A�?}A�A�A�E�A�C�A�A�A�?}A�A�A�E�A�C�A�?}A�A�A�E�A�C�A�A�A�C�A�G�A�E�A�A�A�C�A�G�A�G�A�C�A�A�A�E�A�G�A�E�A�C�A�E�A�I�A�G�A�C�A�G�A�I�A�E�A�E�A�I�A�I�A�G�A�E�A�I�A�K�A�I�A�E�A�I�A�K�A�K�A�G�A�G�A�K�A�M�A�I�A�G�A�I�A�M�A�K�A�I�A�I�A�M�A�M�A�I�A�I�A�I�A�O�A�K�A�I�A�M�A�O�A�K�A�I�A�K�A�M�A�I�A�I�A�K�A�O�A�K�A�G�A�K�A�K�A�M�A�K�A�G�A�K�A�M�A�K�A�K�A�O�A�O�A�K�A�I�A�K�A�M�A�M�A�K�A�G�A�E�A�G�A�G�A�C�A�A�A�E�A�G�A�A�A�C�A�E�A�E�A�E�A�A�A�?}A�A�A�?}A�7LA��A���A�
=A�bA�bA�bA�{A�bA�VA�A�A�A�
=A�oA��A�oA�{A���A���A��
A���Aϥ�AϑhA�t�A�ffA�bNA�bNA�bNA�`BA�\)A�ZA�XA�ZA�ZA�XA�S�A�Q�A�S�A�VA�VA�Q�A�O�A�Q�A�Q�A�Q�A�M�A�K�A�K�A�K�A�M�A�M�A�K�A�G�A�E�A�E�A�G�A�E�A�A�A�?}A�=qA�=qA�?}A�;dA�5?A�33A�33A�1'A�(�A�$�A��A��A��A�{A�bA�1A�A�  A�  A���A���A���A��yA���A���AμjAζFAάAΡ�AΙ�A΍PA΁A΁A�~�A�z�A�v�A�t�A�r�A�t�A�r�A�p�A�l�A�hsA�dZA�^5A�^5A�\)A�Q�A�;dA�bA��;A���AͼjAͩ�A̓A�dZA�ZA�O�A�I�A�G�A�E�A�=qA�-A�{A�  A���A��A��`A��
A���A�ƨA̮A̓uÁA�ffA�$�A���A�ȴA�t�A�I�A�A���AȸRA�hsA�S�A�hsA��;A�$�A��!A�9XA��/A�z�A�{A�ƨA�Q�A�/A�ĜA���A�E�A�bA���A��#A��DA��wA�G�A�
=A�ffA��A�+A��A��A���A��A��A���A��yA��TA��A�hsA�oA���A�VA���A�G�A��A���A��A�1'A��TA��hA�ffA�VA��A�r�A��A���A�S�A��HA���A�`BA�G�A�;dA�33A�(�A�bA���A��TA�ȴA���A�r�A�G�A�9XA�33A��A��HA���A���A��jA��9A���A���A��PA�/A�ȴA�A��9A��A���A���A�x�A�dZA�O�A�;dA�&�A��A�JA�  A���A���A��A��`A��;A���A�ȴA���A��RA��!A���A���A���A�ffA�S�A�C�A� �A��A��A��A�oA�
=A��A���A��!A��\A�z�A�jA�bNA�VA�9XA�
=A���A��`A��
A���A���A��A���A��A��A��A�x�A�^5A� �A���A��7A�M�A�(�A�oA��A���A��jA��uA�ffA�Q�A�=qA�"�A�VA���A���A���A��FA��!A���A�|�A�n�A�XA�I�A�A�A�1'A�(�A� �A�{A���A��
A��RA�|�A�E�A�-A�A�ĜA�G�A���A��A���A�hsA��A��HA���A�S�A�$�A���A�ƨA��wA��RA��9A��-A��!A���A���A��uA���A��DA�x�A�XA�+A��A���A���A��yA���A�~�A�bNA��A���A���A�A�G�A��A���A�+A���A���A��\A�|�A�l�A�ZA�E�A�/A��A��/A��+A�;dA���A���A�^5A�;dA�-A�JA���A��`A�ĜA���A�t�A�1A�\)A�x�A��+A���A��A�A��DA�`BA�5?A��A��/A���A��9A�x�A�VA�;dA�
=A��A���A�\)A�1A��;A��^A���A�n�A�K�A�33A�+A�$�A�
=A��A��
A��9A��A�dZA�M�A�;dA�A���A��PA�1A�l�A��FA��^A�jA�jA�`BA�C�A�M�A�9XA��A�z�A��A�  A�oA~ �A{�A{Az��Az=qAx��Au+Ar�RAq|�Ap�/Ao��Ao��AoS�An��An�RAnr�AnI�An(�An{An�An�An�AnbAnAm�-AmC�Al�yAl�\AlM�AlAk�Ai�;AhI�Af��Ae��Ae33Ad��Ad��AdVAc�;AcK�Ab�HAb=qAa��Aa�FAa`BA`��A`�A`�A`��A`�DA`M�A_�mA_hsA^��A^E�A^bA]�FA]/A\n�A[��AZ��AZAY��AY�FAY\)AX�HAXbNAW��AV��AU��AU�wAUt�AU+AT�HAT�9AT��ATn�AT �ATAS��AS�AS;dASAR��AQ��AQ33AP=qAO��AN��AN �AM�AMAL�!AL�ALbNAL9XAL{ALAK�#AK�wAK��AK�7AK\)AK�AJ��AIl�AG��AGK�AGVAF�RAF �AE�-AEt�AE"�AEVAE%AD��AD��AD�AD�`AD�HAD�AD{AC�AB��AB��AB�9AB�AB��ABz�ABAA��AA&�A@=qA?�mA?��A?x�A?+A?A?A>�A>��A>jA>E�A=�mA=�A=XA=;dA=/A=&�A=VA<�/A<��A<I�A<1A;�#A;�hA;XA;?}A;&�A;VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�9B	�
B	�?B	�
B	��B	�?B	�
B	�
B	��B	֡B	�
B	�
B	�?B	�sB	�sB	�?B	�?B	�?B	�?B	�?B	�?B	�?B	��B	��B	��B	�B	�KB	�B	��B	�jB	�jB	�B	�sB	�mB	�8B	�8B	�8B	�B	�mB	�sB	��B	�"B	��B	�iB	�;B	��B	�B	�/B	��B
�B
|�B
o�B
^�B
e�B
0!B
*�B
[�B
AUB
B�B
A�B
@B
W�B
g�B
f�B
o�B
{�B
�PB
��B
��B
xlB
n�B
}�B
zDB
c�B
3�B
B
�B	�rB	��B	�B	�B	�BB	�jB	�0B	�SB	��B	�B	�;B	q�B	e�B	dZB	]�B	_;B	XEB	R�B	S�B	K�B	OBB	Y�B	T�B	[#B	`�B	e�B	k�B	tB	w�B	|B	��B	�+B	��B	��B	�B	��B
B	��B
FB
�B
'�B
5�B
.}B
#B

�B
@B
'RB
4B
�B
 iB	�(B
�B
B
	�B
B
�B
DB
\B
�B
�B
=�B
F�B
/�B
)�B
5�B
B�B
JXB
O�B
O�B
P�B
S&B
VB
TaB
U2B
S�B
TaB
Q�B
RTB
S�B
S�B
YB
Y�B
]dB
`B
X�B
Z�B
b�B
c�B
_pB
[�B
Z�B
Y�B
[WB
Z�B
]�B
Z�B
Z�B
Z�B
Z�B
Z�B
\]B
[�B
\)B
\]B
\�B
]�B
_;B
^B
\�B
ZB
Z�B
\)B
W�B
ZQB
[�B
\)B
\�B
\�B
]/B
^5B
^B
^�B
]dB
\]B
^B
]�B
^�B
_�B
_B
]�B
]/B
\�B
Z�B
\�B
^�B
_�B
b�B
b�B
bNB
_B
\�B
Z�B
\]B
[�B
\�B
]dB
\)B
Z�B
ZB
Y�B
YKB
X�B
W?B
V�B
W?B
U�B
VB
VB
S�B
R�B
R B
Q�B
Q�B
P}B
MjB
J�B
K)B
J�B
J�B
I�B
K�B
J�B
I�B
I�B
IB
IB
H�B
H�B
H�B
H�B
H�B
HB
GzB
G�B
IB
E9B
B�B
EB
D�B
EB
B�B
C�B
B[B
A�B
@�B
AUB
>�B
>�B
?}B
=<B
=<B
>wB
;dB
;dB
9$B
8�B
7�B
5tB
4�B
33B
1�B
4�B
3hB
2�B
/�B
.B
+�B
(�B
'�B
($B
&B
%FB
$�B
%FB
#�B
$B
"hB
"hB
!�B
!�B
!bB
 �B
�B
VB
�B
�B
�B
�B
�B
~B
�B
IB
�B
B
CB
=B
qB
	B
�B
	B
7B
B
�B
�B
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
{B
{B
�B
�B
SB
�B
�B
�B
�B
�B
�B
�B
FB
�B
�B
�B
{B
B
�B
YB
�B
�B
�B
�B
YB
$B
�B
�B
�B
�B
eB
�B
kB
	B
qB
IB
�B
�B
B
�B
�B
�B
�B
~B
~B
IB
B
�B
�B
B
�B
�B
�B
qB
	B
	B
CB
�B
B
�B
�B
IB
�B
 \B
 �B
 �B
 �B
 �B
!�B
 'B
 �B
 'B
�B
�B
 �B
!�B
"hB
!�B
!�B
!�B
!�B
"4B
"�B
$B
#�B
#�B
#�B
$B
$@B
$tB
$tB
$�B
%B
$�B
$�B
$tB
$tB
$�B
%FB
%FB
$�B
$@B
$@B
#:B
"�B
"4B
!�B
!-B
!�B
#B
"hB
"hB
"�B
"�B
"�B
#:B
#:B
#nB
$�B
$@B
$@B
$B
$�B
$@B
$@B
$@B
#�B
#�B
$�B
$tB
$tB
$�B
&�B
'RB
'RB
'RB
($B
(�B
($B
(XB
(�B
'�B
($B
'�B
($B
(�B
)_B
)�B
)*B
)_B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
,B
+�B
+�B
+�B
+�B
-wB
-�B
-�B
-�B
-wB
-�B
-�B
-�B
.}B
.IB
.IB
.�B
/B
/OB
/�B
/�B
1'B
0�B
1'B
1'B
0�B
1[B
0�B
1'B
1�B
0�B
0�B
1'B
1'B
1[B
1�B
1�B
2�B
2�B
2�B
33B
3hB
3hB
3�B
3�B
3�B
3hB
3�B
49B
3�B
5B
5B
5?B
5�B
5tB
7LB
7B
7LB
7�B
7�B
8B
9XB
:^B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
9�B
:*B
:*B
:*B
:�B
:�B
:�B
;dB
:�B
=qB
<�B
=<B
=�B
=�B
=�B
>BB
?}B
?�B
?�B
@B
@B
@�B
@�B
@�B
A B
A B
@�B
A B
@�B
A B
@�B
@�B
B�B
B[B
B[B
B�B
B�B
B[B
B[B
B�B
B�B
D3B
E9B
EmB
E�B
F?B
GB
F�B
GB
GzB
GEB
G�B
G�B
G�B
HKB
H�B
H�B
J#B
J#B
J#B
J#B
JXB
JXB
JXB
J�B
J�B
K)B
K�B
K)B
K�B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
M�B
M�B
M�B
NB
NpB
N�B
N�B
N�B
OBB
OvB
OBB
PB
P}B
PB
P}B
P�B
P�B
P}B
Q�B
Q�B
R B
Q�B
R B
R B
S[B
S�B
T,B
S�B
S�B
T,B
TaB
TaB
T�B
T�B
T�B
T�B
UgB
U�B
UgB
U�B
UgB
U�B
V9B
V�B
V�B
VmB
VmB
VmB
VmB
V9B
V�B
W�B
W�B
W�B
XEB
XEB
XyB
Y�B
Y�B
Y�B
Y�B
YKB
YKB
YB
YKB
X�B
Y�B
Y�B
ZB
ZB
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[WB
[�B
\)B
\]B
\�B
\�B
]dB
]�B
^5B
^�B
^5B
^�B
^�B
^5B
^�B
_;B
_;B
_;B
_pB
_�B
`B
_�B
`�B
`B
`vB
`vB
a|B
a�B
bB
bB
bB
bNB
b�B
b�B
b�B
cTB
cTB
c B
cTB
c�B
d&B
d�B
d�B
d�B
e�B
e�B
e�B
f2B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
ffB
f�B
f�B
f�B
f�B
g8B
gmB
h>B
h
B
h
B
h>B
h�B
h�B
h�B
h�B
h�B
iB
iB
h�B
iB
iB
iyB
i�B
i�B
i�B
jB
jB
jB
j�B
kB
kB
kB
kB
k�B
k�B
k�B
lWB
l�B
l�B
m]B
m)B
m)B
m]B
n�B
n�B
o B
o5B
o�B
o�B
o�B
o�B
poB
qB
qB
qAB
q�B
qvB
q�B
q�B
rB
rB
rGB
r�B
rB
rGB
q�B
q�B
rB
q�B
sB
sMB
tB
t�B
u%B
t�B
uZB
u%B
u%B
uZB
u�B
u�B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x8B
xlB
x�B
yrB
yrB
y�B
y�B
zB
zDB
zxB
zxB
zxB
z�B
z�B
{JB
{B
{B
{�B
{�B
|B
|B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}"B
}"B
}VB
}VB
}�B
}�B
}�B
~]B
~�B
.B
.B
� B
� B
�iB
�iB
�4B
�iB
�iB
��B
�;B
�;B
��B
��B
�B
�AB
�uB
��B
��B
�B
�{B
��B
��B
�B
��B
��B
�B
��B
�SB
��B
�%B
�%B
��B
�+B
��B
��B
�+B
�+B
��B
��B
�1B
�1B
�fB
�fB
�B
�7B
�7B
�7B
�=B
�rB
��B
��B
��B
��B
��B
�B
��B
�DB
�DB
�DB
�B
�B
�DB
�DB
�DB
�xB
�xB
��B
��B
�B
�~B
��B
��B
��B
�B
��B
�B
�PB
�PB
��B
��B
��B
�VB
��B
�VB
��B	ӏB	�,B	רB	�sB	֡B	�mB	רB	��B	��B	֡B	רB	�B	�B	��B	��B	�B	�mB	��B	��B	��B	֡B	֡B	��B	�B	�sB	��B	�9B	֡B	�9B	�sB	�9B	՛B	�sB	רB	�?B	�9B	֡B	רB	�EB	�?B	�B	��B	�yB	רB	�9B	��B	�B	�?B	�9B	�
B	�EB	�B	��B	֡B	��B	خB	�
B	�9B	�?B	�B	�sB	�9B	֡B	�yB	��B	֡B	�sB	�B	֡B	�9B	רB	�B	֡B	�mB	�?B	خB	�sB	�9B	֡B	�B	�B	��B	�9B	�
B	�B	רB	�mB	�
B	��B	�sB	�mB	��B	�sB	�EB	רB	��B	�
B	�B	�
B	�mB	��B	خB	�sB	��B	�EB	خB	�sB	��B	רB	��B	רB	�sB	�
B	�EB	�KB	��B	�?B	�yB	�EB	��B	�
B	�EB	�B	�EB	�
B	�sB	��B	ٴB	ٴB	�B	��B	�B	��B	�B	خB	چB	�B	��B	��B	�B	�QB	�#B	�B	�B	ܒB	�|B	�B	��B	یB	��B	�B	��B	��B	��B	�vB	�B	�vB	�dB	�#B	ޞB	��B	�B	�TB	�vB	�B	�B	�B	�B	��B	�B	�B	��B	�B	�fB	��B	�
B	�
B	�mB	�B	��B	��B	�>B	�8B	�B	�B	�B	�sB	�mB	��B	�fB	��B	�
B	��B	�8B	�2B	�2B	��B	�>B	�>B	��B	�2B	�2B	�B	��B	��B	�B	�2B	�B	�>B	�>B	�mB	�B	�B	�B	�>B	��B	�B	��B	�>B	�sB	�DB	�B	�yB	�DB	�B	�
B	�B	��B	��B	��B	�B	��B	�B	��B	�5B	�B	�]B	�]B	��B	��B	�5B	�;B	�iB	��B	�iB	�B	�B	�AB	�B	�;B	�B	�iB	�|B	��B	��B	�MB	��B	�B	�B	�oB	�B	�AB	�vB	�B	�B	� B	�/B	�B	��B	��B	�B	�"B	��B	�B	��B	�5B	�iB	�"B	��B	�B	�%B
"hB
uB
�B
_B
B
�B
�B
��B
��B
��B
�SB
y>B
uZB
o�B
o�B
gB
r�B
xlB
iyB
`�B
S�B
VB
TaB
i�B
m�B
R B
r|B
��B
P}B
D�B
1�B
,�B
(�B
�B
&B
X�B
-CB
)*B
($B
%B
"�B
@�B
L�B
��B
\�B
LdB
JXB
B'B
>B
DgB
=�B
9�B
EB
9�B
9�B
A�B
Q�B
D�B
?HB
HKB
CaB
?�B
@B
:�B
8B
8RB
8�B
<B
<�B
<�B
>BB
B�B
K)B
K^B
I�B
K�B
NB
W
B
TaB
S�B
U�B
UgB
W
B
WsB
Z�B
t�B
j�B
gB
g�B
h
B
gB
i�B
iDB
ffB
g8B
f�B
h
B
g�B
f2B
h
B
f2B
f2B
f2B
f2B
e�B
gB
gB
gB
g�B
iB
h>B
h
B
jB
s�B
m�B
s�B
r|B
s�B
r|B
sMB
r�B
r�B
yrB
w�B
cB
|�B
~�B
~]B
}VB
~(B
��B
��B
�B
��B
��B
��B
�xB
�VB
�oB
�4B
��B
��B
��B
��B
��B
��B
��B
�4B
��B
�=B
��B
��B
�YB
�1B
��B
�uB
��B
�SB
�SB
��B
�=B
�B
�B
�iB
�B
.B
}�B
z�B
y�B
v`B
v�B
tTB
tTB
sB
s�B
sMB
p�B
z�B
jB
i�B
iB
k�B
q�B
n�B
f�B
h
B
k�B
l�B
v�B
��B
�rB
�SB
��B
��B
�iB
��B
� B
~]B
}�B
~�B
{�B
y>B
v�B
{JB
z�B
zxB
y>B
p;B
u�B
xB
}�B
b�B
YB
X�B
YB
[�B
f�B
P�B
DgB
X�B
A�B
C�B
0�B
+�B
&�B
&�B
%FB
%B
#�B
!�B
!-B
%FB
$�B
"�B
!B
$�B
�B
�B
�B
�B
�B
�B
(B
B
VB
!-B
�B
2�B
�B
YB
�B	�B	�B	�B	�DB	��B	�B	� B	�B	��B	��B	�JB	�rB	�B
�B

�B	�]B	�PB	�B	��B	��B	��B	��B	��B	��B	��B	�rB	�	B	�B	�rB	�TB	�B	�B	�TB	�cB	�iB	��B	��B	�]B	�B	�ZB	�#B	�B	ܒB	�B	�]B	�B	�B	��B	�<B	یB
B	�gB	�}B	��B	�OB	�&B	��B	�B	��B	�CB	��B	�SB	�	B	�\B	��B	��B	�:B	�.B	��B	�JB	��B	��B	��B	�	B	��B	�(B	��B	�xB	�+B	��B	��B	��B	�4B	�B	�B	{B	t�B	p�B	v`B	w�B	rB	s�B	r�B	iDB	j�B	kB	i�B	b�B	aB	e,B	e�B	dZB	e�B	jB	l"B	`�B	]/B	a�B	bNB	c�B	d�B	b�B	\)B	QB	Q�B	Y�B	XB	`B	b�B	i�B	h�B	YB	]/B	WsB	X�B	VmB	U�B	W?B	VmB	R�B	UgB	R�B	O�B	QB	OBB	S�B	T�B	Q�B	P}B	T,B	YB	S�B	RTB	L0B	J�B	K�B	MB	K�B	I�B	MB	G�B	H�B	H�B	G�B	J�B	QB	f2B	c�B	Y�B	R�B	[#B	\)B	YB	WsB	Y�B	T�B	T�B	T�B	S�B	T�B	UgB	R�B	Q�B	p;B	_;B	ZQB	X�B	VB	VmB	T�B	W�B	bB	^jB	c�B	o5B	aHB	e�B	a�B	f�B	a|B	a�B	f�B	jB	h�B	h�B	o�B	kB	m)B	h�B	k�B	n�B	oiB	p�B	t�B	s�B	s�B	v+B	w�B	t�B	v�B	v�B	wfG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           B	��B	мB	��B	мB	ЈB	��B	мB	мB	ЈB	�SB	мB	мB	��B	�%B	�%B	��B	��B	��B	��B	��B	��B	��B	юB	юB	юB	��B	��B	�1B	�xB	�B	�B	�iB	�%B	�B	��B	��B	��B	�B	�B	�%B	�B	��B	�B	�B	��B	�B	�B	��B	�~B
3B
v�B
iPB
XPB
_{B
)�B
$�B
UrB
;B
<AB
;pB
9�B
Q�B
aSB
`MB
i�B
u�B
�B
��B
}�B
rB
hJB
w�B
s�B
]�B
-�B
�B
�B	�$B	�wB	��B	�:B	��B	�B	��B	�B	�^B	��B	z�B	k\B	_{B	^B	W~B	X�B	Q�B	L;B	MuB	EDB	H�B	S�B	N�B	T�B	Z�B	_GB	elB	m�B	q�B	u�B	�FB	��B	�RB	�9B	��B	�sB	��B	�kB
�B
tB
!mB
/ZB
(/B
�B
XB
�B
!B
-�B
�B	�B	��B
�B
�B
RB
�B	�nB
�B
	B
tB
6B
7�B
@ZB
)jB
#�B
/ZB
<vB
D
B
I]B
I]B
JcB
L�B
O�B
NB
N�B
M�B
NB
KiB
LB
MAB
MAB
R�B
SfB
WB
Y�B
R�B
T�B
\�B
]�B
Y"B
U�B
T8B
SfB
U	B
T8B
W~B
T8B
TlB
T8B
T�B
T8B
VB
U>B
U�B
VB
VxB
W~B
X�B
W�B
VDB
S�B
T8B
U�B
QZB
TB
U>B
U�B
VDB
V�B
V�B
W�B
W�B
X�B
WB
VB
W�B
WJB
XPB
YVB
X�B
WJB
V�B
VxB
T8B
V�B
XPB
Y�B
\4B
\�B
\ B
X�B
VDB
T8B
VB
UrB
V�B
WB
U�B
T8B
S�B
S�B
R�B
R�B
P�B
P�B
P�B
OMB
O�B
O�B
MuB
L;B
K�B
K5B
K5B
J/B
GB
D�B
D�B
DsB
DsB
C�B
EDB
D�B
C�B
ClB
B�B
B�B
B2B
B�B
B2B
B2B
B�B
A�B
A,B
A`B
B�B
>�B
<vB
>�B
>�B
>�B
<vB
=�B
<B
;pB
:�B
;B
8�B
8]B
9/B
6�B
6�B
8)B
5B
5B
2�B
28B
12B
/&B
.TB
,�B
+vB
.TB
-B
,HB
)jB
'�B
%QB
"sB
!�B
!�B
�B
�B
�B
�B
�B
�B
B
B
}B
HB
B
wB
<B
B
<B
jB
�B
dB
dB
0B
dB
�B
^B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
LB
LB
�B
zB
�B
�B
?B
9B
�B
3B
gB
gB
�B
�B
aB
aB
�B
-B
-B
3B
3B
B
�B
�B
UB
UB
�B
�B
�B
�B
[B
[B
�B
-B
�B
gB
B
tB
nB
9B
?B
B
�B
nB
9B
9B
tB
B
�B
B
�B
#B
�B
dB
dB
�B
�B
dB
6B
jB
0B
0B
�B
�B
�B
^B
�B
^B
�B
�B
#B
�B
�B
�B
^B
�B
�B
�B
�B
�B
B
BB
BB
wB
�B
}B
�B
�B
�B
pB
�B
BB
}B
B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
&B
[B
�B
�B
�B
&B
&B
[B
�B
�B
�B
�B
�B
�B
NB
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
[B
�B
�B
�B
�B
�B
�B
&B
&B
�B
 gB
!B
!B
!B
!�B
"?B
!�B
"
B
"?B
!�B
!�B
!�B
!�B
"sB
#B
#EB
"�B
#B
#EB
#EB
#yB
#�B
$KB
$KB
$KB
%QB
%QB
%�B
%�B
%�B
%�B
%�B
')B
'�B
'^B
'�B
')B
'�B
'^B
'�B
(/B
'�B
'�B
(dB
(�B
)B
)5B
)jB
*�B
*<B
*�B
*�B
*�B
+B
*�B
*�B
+vB
*�B
*�B
*�B
*�B
+B
+vB
+�B
,HB
,�B
,�B
,�B
-B
-B
-�B
-NB
-NB
-B
-NB
-�B
-�B
.�B
.�B
.�B
/ZB
/&B
0�B
0�B
0�B
1�B
1gB
1�B
3
B
4B
3sB
3>B
3sB
3sB
3>B
3�B
3sB
3sB
3sB
3�B
3�B
3�B
3sB
3�B
3�B
3�B
4EB
4EB
4yB
5B
4�B
7#B
6�B
6�B
7�B
7WB
7WB
7�B
9/B
9cB
9�B
9�B
9�B
:5B
:5B
:jB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
<vB
<B
<B
<AB
<AB
<B
<B
<AB
<�B
=�B
>�B
?B
?�B
?�B
@�B
@�B
@�B
A,B
@�B
A�B
A`B
A�B
A�B
BfB
B�B
C�B
C�B
C�B
C�B
D
B
D
B
D
B
D�B
DsB
D�B
EDB
D�B
EDB
EB
EyB
E�B
E�B
E�B
E�B
E�B
G�B
GQB
G�B
G�B
H"B
H�B
HWB
HWB
H�B
I(B
H�B
I�B
J/B
I�B
J/B
JcB
JcB
J/B
K5B
K�B
K�B
K�B
K�B
K�B
MB
MuB
M�B
M�B
M�B
M�B
NB
NB
NGB
N�B
N�B
N�B
OB
OMB
OB
OMB
OB
O�B
O�B
PSB
PSB
PB
PB
PB
PB
O�B
PSB
QZB
QZB
Q�B
Q�B
Q�B
R+B
S�B
SfB
SfB
SfB
R�B
R�B
S1B
R�B
R�B
SfB
SfB
S�B
S�B
S�B
SfB
S�B
S�B
S�B
T8B
T�B
T�B
T�B
T�B
U	B
U	B
U�B
U�B
VB
VDB
VDB
WB
W~B
W�B
XPB
W�B
X�B
X�B
W�B
X�B
X�B
X�B
X�B
Y"B
YVB
Y�B
Y�B
Z�B
Y�B
Z(B
Z(B
[.B
[cB
[�B
[�B
[�B
\ B
\4B
\iB
\�B
]B
]B
\�B
]B
]oB
]�B
^uB
^AB
^�B
_GB
_{B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`MB
`�B
`MB
`�B
`�B
aB
a�B
a�B
a�B
a�B
bYB
bYB
bYB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c+B
c�B
c_B
c_B
c�B
d1B
d1B
d�B
d�B
d�B
d�B
d�B
e7B
e�B
e�B
f	B
f=B
f�B
gB
f�B
f�B
gB
hJB
h~B
h�B
h�B
iPB
iPB
i�B
i�B
j!B
j�B
j�B
j�B
k�B
k(B
k\B
k�B
k�B
k�B
k�B
l�B
k�B
k�B
k\B
k\B
k�B
k�B
l�B
l�B
m�B
nnB
n�B
n�B
oB
n�B
n�B
oB
o�B
o�B
pB
p{B
p{B
p{B
p{B
p�B
pFB
p�B
p{B
p�B
qLB
q�B
rB
r�B
s$B
s$B
sYB
sYB
s�B
s�B
t*B
t*B
t*B
t_B
t_B
t�B
u1B
u1B
ueB
u�B
u�B
u�B
vB
vB
v7B
vkB
vkB
vkB
v�B
v�B
v�B
wB
wB
wqB
wqB
wqB
xB
xwB
x�B
x�B
y�B
y�B
zB
zB
y�B
zB
zB
zOB
z�B
z�B
{UB
{�B
{�B
{�B
|'B
|\B
|�B
|�B
}-B
}�B
}�B
}�B
~3B
~hB
~�B
:B
B
�B
�B
�B
�@B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�$B
�XB
�XB
�XB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�*B
�*B
�^B
�^B
��B
�0B
�eB
�eB
�eB
��B
��B
��B
�B
�B
��B
�kB
��B
�B
�<B
�B
�qB	�AB	��B	�ZB	�%B	�SB	�B	�ZB	юB	ЈB	�SB	�ZB	��B	϶B	ЈB	юB	��B	�B	ςB	юB	ҔB	�SB	�SB	ЈB	��B	�%B	ЈB	��B	�SB	��B	�%B	��B	�MB	�%B	�ZB	��B	��B	�SB	�ZB	��B	��B	϶B	ЈB	�+B	�ZB	��B	ЈB	��B	��B	��B	мB	��B	��B	ЈB	�SB	юB	�`B	мB	��B	��B	��B	�%B	��B	�SB	�+B	ЈB	�SB	�%B	��B	�SB	��B	�ZB	��B	�SB	�B	��B	�`B	�%B	��B	�SB	��B	��B	ЈB	��B	мB	��B	�ZB	�B	мB	юB	�%B	�B	ЈB	�%B	��B	�ZB	ςB	мB	��B	мB	�B	юB	�`B	�%B	ЈB	��B	�`B	�%B	ЈB	�ZB	ҔB	�ZB	�%B	мB	��B	��B	юB	��B	�+B	��B	ЈB	мB	��B	��B	��B	мB	�%B	юB	�fB	�fB	��B	ҔB	��B	ԠB	��B	�`B	�8B	��B	ҔB	ҔB	��B	�B	��B	�1B	��B	�DB	�.B	�4B	ԠB	�>B	�~B	عB	�~B	�~B	ًB	�(B	ٿB	�(B	�B	��B	�PB	էB	׳B	�B	�(B	�GB	�AB	��B	�B	�B	�YB	��B	�B	�B	�B	�B	�B	�B	�B	�MB	��B	�B	��B	��B	�MB	�MB	�SB	�%B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�SB	�SB	��B	�B	��B	�B	��B	�%B	��B	�_B	�+B	��B	�YB	�B	��B	�rB	�B	�B	�7B	�rB	�=B	�xB	��B	�B	�B	�B	�xB	�~B	��B	��B	�B	�B	�B	�PB	�B	��B	�B	��B	�PB	�B	�.B	�B	�FB	��B	�B	�\B	�:B	�!B	�VB	��B	�(B	�PB	�JB	�B	��B	�hB	�B	�~B	�CB	��B	�B	�=B	�B	��B	�B	��B	�B	�B	��B
B
'B
	�B
B
�B

IB
XB
�UB
�yB
{�B
B
r�B
oB
i�B
iPB
`�B
l�B
rB
c+B
Z\B
MAB
O�B
NB
c_B
gCB
K�B
l.B
�NB
J/B
>�B
+�B
&�B
"?B
�B
�B
R�B
&�B
"�B
!�B
�B
�B
:�B
FB
��B
V�B
FB
D
B
;�B
7�B
>B
7WB
3sB
>�B
3�B
3>B
;pB
KiB
>�B
8�B
A�B
=B
9cB
9�B
4�B
1�B
2B
2�B
5�B
6�B
6�B
7�B
<�B
D�B
EB
ClB
EDB
G�B
P�B
NB
M�B
O�B
OB
P�B
Q%B
TlB
nnB
deB
`�B
aSB
a�B
`�B
c_B
b�B
`B
`�B
`MB
a�B
a�B
_�B
a�B
_�B
_�B
_�B
_�B
_{B
`�B
`�B
`�B
a�B
b�B
a�B
a�B
c�B
m4B
g�B
m4B
l.B
mhB
l.B
l�B
l�B
lbB
s$B
q�B
yB
vkB
xCB
xB
wB
w�B
|�B
��B
}�B
��B
�RB
��B
�*B
�B
�!B
��B
�CB
�}B
�OB
��B
��B
��B
��B
��B
��B
��B
�^B
�zB
�B
��B
:B
|'B
}bB
B
B
�@B
��B
|�B
y~B
zB
{�B
x�B
wqB
t�B
sYB
pB
pFB
nB
nB
l�B
m�B
l�B
jVB
t�B
c�B
c_B
b�B
elB
k\B
h~B
`�B
a�B
e7B
f=B
pFB
�@B
�$B
B
�LB
�B
zB
z�B
y�B
xB
w=B
x�B
u�B
r�B
p�B
t�B
t�B
t*B
r�B
i�B
o@B
q�B
wqB
\4B
S1B
R`B
S1B
U�B
`�B
J�B
>B
R�B
;�B
=|B
*�B
%�B
 gB
 3B
�B
�B
UB
}B
�B
�B
�B
�B
�B
[B
�B
?B
�B
�B

}B
qB
�B
�B
B
�B
�B
,HB
�B
B	��B	��B	�B	�1B	��B	�qB	�B	�B	�hB	�B	�B	��B	�$B	��B	��B
�B	�B	�B	��B	�7B	�7B	��B	�_B	�B	�B	��B	�$B	�B	��B	�$B	�B	��B	��B	�B	�B	�B	��B	��B	�B	��B	�B	��B	�\B	�DB	϶B	�B	��B	��B	�rB	��B	�>B	��B	�B	�/B	��B	�B	��B	�B	��B	�BB	��B	�gB	�B	��B	�B	�RB	�nB	��B	��B	�eB	��B	�XB	�XB	�LB	��B	�IB	��B	nB	�*B	��B	{�B	�9B	�kB	��B	��B	|�B	t�B	n�B	jVB	pB	qLB	k�B	m�B	l�B	b�B	deB	d�B	c_B	\iB	Z�B	^�B	_GB	^B	_{B	c�B	e�B	Z�B	V�B	[�B	\ B	]�B	^uB	\4B	U�B	J�B	K�B	SfB	Q�B	Y�B	\iB	c_B	b�B	S1B	V�B	Q%B	R`B	PB	O�B	P�B	PB	L�B	OB	L�B	I]B	J�B	H�B	M�B	NGB	K�B	J/B	M�B	S1B	MuB	LB	E�B	D�B	EyB	F�B	EDB	C8B	F�B	A�B	BfB	B�B	A`B	D>B	J�B	_�B	]oB	SfB	L�B	T�B	U�B	R�B	Q%B	SfB	N|B	N|B	N�B	M�B	NGB	OB	LoB	K�B	i�B	X�B	TB	R`B	O�B	PB	N|B	QZB	[�B	XB	]�B	h�B	Z�B	_GB	[�B	`MB	[.B	[�B	`�B	c�B	b�B	b�B	i�B	d�B	f�B	bYB	e�B	hJB	iB	jVB	n�B	m4B	m�B	o�B	qLB	n:B	p�B	p�B	qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230611090104                            20230611090104AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023061109010420230611090104  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023061109010420230611090104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023061109010420230611090104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               