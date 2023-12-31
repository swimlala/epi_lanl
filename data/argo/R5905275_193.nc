CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:33:05Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223305  20230426223305  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�#/��)e@�#/��)e11  @�#/��p@�#/��p@+^i�B��@+^i�B���c�s�g��c�s�g�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?��?��H@=p�@�  @�  @��R@޸R@��RA\)A\)A,��A@��A_\)A�Q�A�Q�A��A��A�  AУ�A�Q�A�  B Q�BQ�B  B(�B Q�B((�B/�
B8  B@(�BH(�BP  BX  B_�
Bg�
Bo�
Bw�
B�  B�  B��B�  B�  B�  B�{B�{B�  B�{B�{B�  B�  B�(�B�(�B��B��B�{B�ffB��
B�  B�{B�  B��B��B�  B��B�  B�{B�{B�  B�  C   C  C  C
=C
=C	��C��C  C  C��C��C  C
=C
=C  C��C   C!��C#��C&  C(
=C*  C,  C.  C0
=C2  C3��C6  C8  C:  C;��C>  C@  CB  CD
=CF  CG��CJ
=CL  CM��CP  CR
=CS��CU�CX  CZ
=C\
=C]��C_��Cb{Cd
=Cf  Cg��Ci�Ck��Cm��Co��Cq��Ct  Cv  Cx  Cz
=C|
=C~
=C�C�  C�C�
=C�C���C���C�C�
=C�  C���C�  C���C���C�  C�C�  C���C�C�  C�  C�  C���C���C���C�C�C���C���C���C���C���C�  C�  C�  C���C���C�
=C�C�C�  C���C�C�C�C�
=C�  C�  C�  C�  C�  C�  C�C�C�C�C�C�C�
=C�C�
=C�  C�  C�  C�  C�  C���C���C�C�C���C���C���C���C���C���C���C���C���C�  C�  C���C���C�  C�  C���C�  C���C���C�C�
=C�
=C�  C�C�  C���C���C���C�  C�C�C�C���C���C�  C�  C�C���C�  C�C���C���C���C�  C�C�
=C�  C�  C�  C���C���C�  C�
=C���C�  C�\C�\C�
=D   D z�D  D��D�qD}qD�qD}qD�qD}qD  D� D�D��DD� D  D� D	�D	��D	�qD
}qD
�qD}qD�qD� D�D� D�qD� D  Dz�D�qDz�D  D��D  D��D  D}qD�qDz�D��D��D�D�DD� D  D��D  D� DD�D  D� D�D}qD�D��D�D��D  Dz�D   D �D!D!��D"  D"}qD"�qD#z�D$  D$��D%  D%}qD%��D&z�D&�RD'}qD(  D(}qD(�qD)}qD)�qD*}qD+�D+�D,D,��D-  D-}qD-�qD.}qD/  D/�D0D0��D1�D1� D2�D2��D3D3��D4  D4� D5�D5��D6D6�D7�D7� D7�qD8z�D8�qD9��D:  D:� D:�qD;� D<D<��D=  D=z�D=�qD>� D?�D?�D@  D@}qD@�qDA� DB�DB� DB�qDC� DC�qDD� DE  DE� DF  DF��DG�DG� DH  DH��DI  DI� DI�qDJ}qDK�DK��DL  DL��DM�DM��DN�DN�DO�DO}qDO��DP}qDQ  DQ��DR�DR� DR�qDS� DT  DT}qDU  DU� DV�DV� DV�qDW� DX  DX}qDX�qDY}qDY��DZ}qD[�D[��D\�D\� D]  D]��D^  D^� D_  D_��D`�D`}qD`��Da}qDa�qDb}qDb�qDc}qDd  Dd��De�De}qDe�qDf}qDf��Dg}qDg�qDh}qDi  Di� Dj  Dj� Dj�qDk� Dl�Dl}qDl��Dm� Dn  Dn}qDn�qDo� Do�qDpz�Dq  Dq��Dr�Dr��Ds  Ds� Dt�Dt��Du  Du}qDu�qDv}qDw  Dw}qDw��Dx}qDy�Dy��Dy�qDz}qDz�qD{}qD|  D|� D}�D}� D}�qD~� D  D}qD��D�>�D��HD���D���D�>�D�� D�D��D�AHD��HD�� D�  D�AHD���D��HD�HD�@ D�~�D��HD�  D�@ D��HD�� D���D�AHD���D��HD�HD�@ D�~�D�� D�  D�AHD���D��HD�  D�>�D�}qD�� D�HD�>�D�� D��HD�  D�>�D�� D��HD�  D�AHD��HD�� D���D�@ D���D��HD�  D�@ D��HD���D���D�>�D�� D�� D��qD�=qD�� D��HD�HD�AHD�� D���D�HD�@ D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D���D�HD�B�D��HD���D���D�@ D�� D�� D�  D�AHD�� D���D�  D�AHD�~�D���D�  D�@ D��HD��HD�HD�AHD��HD�D��D�AHD��HD��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�=qD�~�D�� D���D�>�D�� D�� D���D�@ D��HD��HD�  D�>�D�}qD�� D��D�AHD��HD��HD���D�>�D�� D�� D�  D�>�D�~�D��HD�HD�@ D�~�D���D�HD�@ D�� D��HD�HD�AHD��HD���D�  D�@ D�~�D���D�  D�>�D�~�D��qD��qD�=qD�}qD��qD���D�>�D��HD���D�HD�>�D�~�D���D�  D�AHD���D�D�HD�>�D�� D���D���D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D��qD�>�D�� D��HD�HD�@ D�~�D�� D�  D�AHD���D�� D�HD�B�D��HD�� D���D�=qD�� D�� D���D�AHD�� D���D�  D�@ D�~�D��HD�  D�>�D�~�D��HD�HD�@ D�� D��qD��qD�>�D�}qD��qD��qD�=qD�~�D��qD���D�>�D�~�D�� D�HD�AHD��HD��HD��D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�  D�>�D D�� D���D�@ DÂ�D��HD���D�@ DĀ Dľ�D�  D�AHDŀ D�� D�HD�B�Dƀ Dƾ�D�  D�AHDǀ D�� D�HD�@ D�~�DȾ�D�HD�AHDɀ D�� D���D�>�DʁHD�� D���D�>�Dˀ D��HD�HD�@ D̀ D�� D�  D�@ D̀ D��HD�HD�>�D�~�D�� D�  D�@ Dπ D��HD�HD�@ DЀ D�� D�  D�@ Dр DѾ�D���D�AHD҂�D��HD�  D�@ DӀ D�� D���D�=qD�~�D�� D�HD�@ DՀ D�� D�HD�@ Dր D��HD�  D�@ DׁHD�� D�  D�@ D؂�D��HD�  D�@ Dـ D�� D�  D�@ DځHD��HD�  D�@ DہHD�� D�  D�>�D܀ D��HD�  D�@ D݁HD�� D���D�@ Dހ D޾�D���D�=qD߀ D��HD�HD�AHD�� D�qD���D�>�D� DᾸD���D�@ D�~�D�qD�  D�@ D�~�D�� D���D�>�D�HD�D�HD�AHD傏D�� D���D�@ D� D�� D�  D�@ D� D��HD�  D�AHD�HD�� D�  D�>�D� D��HD�  D�>�D� D꾸D���D�@ D� D�qD���D�AHD�HD��HD�  D�@ D� D�� D�HD�B�DD��HD���D�@ D�HDﾸD�  D�B�D��HD�D���D�AHD�D�� D���D�@ D�HD�D�  D�AHD� D�� D�HD�@ D� D���D���D�=qD�~�D�� D�  D�>�D�~�D���D���D�>�D�~�D���D�  D�AHD��HD��HD�HD�B�D�~�D���D�HD�B�D��HD��{?.{?B�\?�  ?��R?�Q�?�(�?��H@\)@!G�@.{@@  @Tz�@aG�@s33@�G�@��@�@�(�@�ff@���@��H@\@˅@�z�@�  @���@��@�Q�A�AffA
�HA\)A33AQ�A�A   A$z�A)��A.{A2�\A6ffA:=qA@  AC�
AG�AL��AQG�AUAY��A^{Ab�\Ag�Ak�Ap��Au�Az�HA�  A��A�(�A�\)A���A��
A�{A�G�A��
A�A�Q�A�33A��A�\)A��A���A�
=A�G�A��
A�ffA���A�33A�p�A�Q�A��HA���A�
=A���A�z�A�
=A���A�33A�A�Q�A�=qA�z�A�\)Aٙ�A��
A�{A��A�A�ffA�Q�A�\A�p�A�Q�A��HA���A�\)A�=qA���A�
=B ��B=qB�B��B�B33B��B	�B
�HB(�Bp�B�RB�B��B{B\)Bz�Bp�B�RB  B�B=qB�B��B�B�HB Q�B!B"�HB$  B%G�B&�\B(  B(��B*=qB+�B,��B.{B/33B0Q�B1B3
=B4(�B5G�B6�RB8  B9G�B:ffB;�B<��B>ffB?�B@��BA�BC33BD��BEBF�HBH  BIp�BJ�RBK�
BL��BN=qBO�BP��BQBR�HBT(�BU��BV�\BW�BX��BY�B[\)B\z�B]p�B^�\B`  BaG�Bb=qBc\)Bd��Bf{Bg33BhQ�Bip�Bj�HBlQ�Bm��Bn�\Bo�Bp��BrffBs�Bt��Bu�Bw\)Bx��ByBz�HB|Q�B}B33B�(�B���B��B�=qB���B��B�Q�B�
=B�B�z�B�33B��
B��\B�\)B�{B���B�p�B�{B��HB���B�Q�B��HB���B�ffB��B�B�ffB�
=B�B��\B�G�B��B�z�B�33B��B���B�G�B��
B��\B�G�B�  B���B�33B��
B��\B�G�B��
B�ffB��B��
B�z�B��B��B�ffB�
=B�B�Q�B��HB��B�=qB��HB�p�B�  B��RB�p�B�  B��\B��B��
B�z�B��B��B�=qB���B��B�Q�B���B���B�=qB���B�B�ffB�
=B��B�z�B�33B��B���B�33B��B��\B�G�B�{B��RB�p�B�{B��RB�\)B�{B���B�p�B�  Bď\B�
=BŅB�{BƏ\B��HB��B�\)BǙ�B��
B�{B�Q�B�ffB�z�Bȏ\B���B���B��B�33B�33B�\)Bə�B�B��B�  B�{B�(�B�ffBʏ\BʸRB��HB���B�
=B�33B�p�BˮB�B��
B�  B�{B�ffB̏\B���B��HB�
=B�33B�p�B͙�B��
B�  B�(�B�=qB�z�BθRB���B��B�\)B�p�Bϙ�B�B�  B�=qB�ffBУ�BиRB��HB�
=B�G�BхBѮB��B�{B�=qB�ffBҏ\BҸRB���B�33B�p�Bә�BӮB��
B�  B�(�B�z�Bԏ\BԸRB��HB���B��B�\)Bՙ�B�B��B�{B�(�B�Q�B�ffB֣�B���B�
=B�33B�G�B�\)BׅBי�B��
B�  B�=qB�ffB؏\Bأ�BظRB���B��B�\)BمBٮB��
B�  B�(�B�ffBڣ�B��HB�
=B�G�B�p�Bۙ�B��
B�{B�ffBܣ�B���B�33B�p�Bݙ�B��
B�{B�ffBޣ�B���B�G�B߅B��
B�{B�Q�B�z�B���B�
=B�\)BᙚB�  B�=qB��B��HB��B�\)B㙚B��B�=qB�z�B���B�33B�B�B�{B�Q�B�\B���B�33B�p�B�B�{B�z�B�RB�
=B�\)B陚B��
B�{B�ffB�RB��B�p�B�B�  B�Q�B�\B���B��B�\)B��B��B�=qB�\B��HB�G�B�B��B�(�B�z�B���B��B�p�B�B�  B�Q�B��B���B�\)B�B�{B�ffB���B��B��B��
B�=qB��\B��HB�33B���B��B�=qB��\B���B�G�B��B�  B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�ffB���B�33B���C 
=C =qC z�C ��C �HC{CG�Cz�C�RC�HC{CG�Cz�C�C�HC{CQ�C�C�RC��C(�C\)C��C��C
=C=qCp�C�C�C�CQ�C�\CC  C33CffC��C�HC�CQ�C�\CC	  C	33C	p�C	�C	�HC
�C
\)C
�\C
��C
=C=qCp�C�C�C�C\)C��C�
C{CG�C�CC��C33Cp�C�C�C(�C\)C��C�
C{CQ�C�CC
=CG�Cz�C�RC��C33Cp�C�C�C�C\)C��C�
C{CQ�C�\C��C
=CQ�C�CC  C=qCz�C�RC��C=qCp�C�C��C33Cp�C�RC��C33Cp�C�C��C33Cp�C�C�HC(�CffC��C�HC{C\)C��C�
C{C\)C�\C�HC{C\)C��C�HC�C\)C��C�
C 
=C Q�C �\C C!
=C!G�C!�C!��C"
=C"Q�C"�\C"��C#
=C#=qC#�C#�RC#��C$33C$p�C$��C$�HC%�C%\)C%��C%�
C&�C&\)C&��C&�
C'{C'Q�C'�\C'��C(
=C(=qC(z�C(�RC(��C)33C)p�C)C*  C*=qC*p�C*�C*�HC+�C+Q�C+�\C+��C,
=C,G�C,�C,C,��C-(�C-ffC-��C-��C.  C.33C.p�C.��C.�HC/�C/Q�C/z�C/�C/�HC0{C0Q�C0�\C0��C0��C1=qC1ffC1��C1C1��C2(�C2ffC2��C2�
C3
=C3=qC3p�C3��C3��C3��C4(�C4ffC4��C4�
C5
=C5=qC5ffC5��C5��C6
=C6=qC6z�C6�C6�HC7
=C7=qC7p�C7��C7�
C8
=C8G�C8z�C8�RC8�HC9{C9=qC9p�C9�C9�C:�C:Q�C:z�C:�RC:�HC;�C;Q�C;�\C;��C;��C<33C<ffC<��C<��C=
=C=G�C=�\C=��C>  C>=qC>p�C>�C>��C?33C?z�C?�RC?��C@(�C@ffC@��C@�HCA(�CAp�CA�CA��CB33CBffCB�CB��CC=qCC�CC��CD
=CD=qCD�CD��CE{CEffCE�CE�CF(�CFp�CFCG
=CG\)CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           ?��?��H@=p�@�  @�  @��R@޸R@��RA\)A\)A,��A@��A_\)A�Q�A�Q�A��A��A�  AУ�A�Q�A�  B Q�BQ�B  B(�B Q�B((�B/�
B8  B@(�BH(�BP  BX  B_�
Bg�
Bo�
Bw�
B�  B�  B��B�  B�  B�  B�{B�{B�  B�{B�{B�  B�  B�(�B�(�B��B��B�{B�ffB��
B�  B�{B�  B��B��B�  B��B�  B�{B�{B�  B�  C   C  C  C
=C
=C	��C��C  C  C��C��C  C
=C
=C  C��C   C!��C#��C&  C(
=C*  C,  C.  C0
=C2  C3��C6  C8  C:  C;��C>  C@  CB  CD
=CF  CG��CJ
=CL  CM��CP  CR
=CS��CU�CX  CZ
=C\
=C]��C_��Cb{Cd
=Cf  Cg��Ci�Ck��Cm��Co��Cq��Ct  Cv  Cx  Cz
=C|
=C~
=C�C�  C�C�
=C�C���C���C�C�
=C�  C���C�  C���C���C�  C�C�  C���C�C�  C�  C�  C���C���C���C�C�C���C���C���C���C���C�  C�  C�  C���C���C�
=C�C�C�  C���C�C�C�C�
=C�  C�  C�  C�  C�  C�  C�C�C�C�C�C�C�
=C�C�
=C�  C�  C�  C�  C�  C���C���C�C�C���C���C���C���C���C���C���C���C���C�  C�  C���C���C�  C�  C���C�  C���C���C�C�
=C�
=C�  C�C�  C���C���C���C�  C�C�C�C���C���C�  C�  C�C���C�  C�C���C���C���C�  C�C�
=C�  C�  C�  C���C���C�  C�
=C���C�  C�\C�\C�
=D   D z�D  D��D�qD}qD�qD}qD�qD}qD  D� D�D��DD� D  D� D	�D	��D	�qD
}qD
�qD}qD�qD� D�D� D�qD� D  Dz�D�qDz�D  D��D  D��D  D}qD�qDz�D��D��D�D�DD� D  D��D  D� DD�D  D� D�D}qD�D��D�D��D  Dz�D   D �D!D!��D"  D"}qD"�qD#z�D$  D$��D%  D%}qD%��D&z�D&�RD'}qD(  D(}qD(�qD)}qD)�qD*}qD+�D+�D,D,��D-  D-}qD-�qD.}qD/  D/�D0D0��D1�D1� D2�D2��D3D3��D4  D4� D5�D5��D6D6�D7�D7� D7�qD8z�D8�qD9��D:  D:� D:�qD;� D<D<��D=  D=z�D=�qD>� D?�D?�D@  D@}qD@�qDA� DB�DB� DB�qDC� DC�qDD� DE  DE� DF  DF��DG�DG� DH  DH��DI  DI� DI�qDJ}qDK�DK��DL  DL��DM�DM��DN�DN�DO�DO}qDO��DP}qDQ  DQ��DR�DR� DR�qDS� DT  DT}qDU  DU� DV�DV� DV�qDW� DX  DX}qDX�qDY}qDY��DZ}qD[�D[��D\�D\� D]  D]��D^  D^� D_  D_��D`�D`}qD`��Da}qDa�qDb}qDb�qDc}qDd  Dd��De�De}qDe�qDf}qDf��Dg}qDg�qDh}qDi  Di� Dj  Dj� Dj�qDk� Dl�Dl}qDl��Dm� Dn  Dn}qDn�qDo� Do�qDpz�Dq  Dq��Dr�Dr��Ds  Ds� Dt�Dt��Du  Du}qDu�qDv}qDw  Dw}qDw��Dx}qDy�Dy��Dy�qDz}qDz�qD{}qD|  D|� D}�D}� D}�qD~� D  D}qD��D�>�D��HD���D���D�>�D�� D�D��D�AHD��HD�� D�  D�AHD���D��HD�HD�@ D�~�D��HD�  D�@ D��HD�� D���D�AHD���D��HD�HD�@ D�~�D�� D�  D�AHD���D��HD�  D�>�D�}qD�� D�HD�>�D�� D��HD�  D�>�D�� D��HD�  D�AHD��HD�� D���D�@ D���D��HD�  D�@ D��HD���D���D�>�D�� D�� D��qD�=qD�� D��HD�HD�AHD�� D���D�HD�@ D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D���D�HD�B�D��HD���D���D�@ D�� D�� D�  D�AHD�� D���D�  D�AHD�~�D���D�  D�@ D��HD��HD�HD�AHD��HD�D��D�AHD��HD��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�=qD�~�D�� D���D�>�D�� D�� D���D�@ D��HD��HD�  D�>�D�}qD�� D��D�AHD��HD��HD���D�>�D�� D�� D�  D�>�D�~�D��HD�HD�@ D�~�D���D�HD�@ D�� D��HD�HD�AHD��HD���D�  D�@ D�~�D���D�  D�>�D�~�D��qD��qD�=qD�}qD��qD���D�>�D��HD���D�HD�>�D�~�D���D�  D�AHD���D�D�HD�>�D�� D���D���D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D��qD�>�D�� D��HD�HD�@ D�~�D�� D�  D�AHD���D�� D�HD�B�D��HD�� D���D�=qD�� D�� D���D�AHD�� D���D�  D�@ D�~�D��HD�  D�>�D�~�D��HD�HD�@ D�� D��qD��qD�>�D�}qD��qD��qD�=qD�~�D��qD���D�>�D�~�D�� D�HD�AHD��HD��HD��D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�  D�>�D D�� D���D�@ DÂ�D��HD���D�@ DĀ Dľ�D�  D�AHDŀ D�� D�HD�B�Dƀ Dƾ�D�  D�AHDǀ D�� D�HD�@ D�~�DȾ�D�HD�AHDɀ D�� D���D�>�DʁHD�� D���D�>�Dˀ D��HD�HD�@ D̀ D�� D�  D�@ D̀ D��HD�HD�>�D�~�D�� D�  D�@ Dπ D��HD�HD�@ DЀ D�� D�  D�@ Dр DѾ�D���D�AHD҂�D��HD�  D�@ DӀ D�� D���D�=qD�~�D�� D�HD�@ DՀ D�� D�HD�@ Dր D��HD�  D�@ DׁHD�� D�  D�@ D؂�D��HD�  D�@ Dـ D�� D�  D�@ DځHD��HD�  D�@ DہHD�� D�  D�>�D܀ D��HD�  D�@ D݁HD�� D���D�@ Dހ D޾�D���D�=qD߀ D��HD�HD�AHD�� D�qD���D�>�D� DᾸD���D�@ D�~�D�qD�  D�@ D�~�D�� D���D�>�D�HD�D�HD�AHD傏D�� D���D�@ D� D�� D�  D�@ D� D��HD�  D�AHD�HD�� D�  D�>�D� D��HD�  D�>�D� D꾸D���D�@ D� D�qD���D�AHD�HD��HD�  D�@ D� D�� D�HD�B�DD��HD���D�@ D�HDﾸD�  D�B�D��HD�D���D�AHD�D�� D���D�@ D�HD�D�  D�AHD� D�� D�HD�@ D� D���D���D�=qD�~�D�� D�  D�>�D�~�D���D���D�>�D�~�D���D�  D�AHD��HD��HD�HD�B�D�~�D���D�HD�B�D��HD��{?.{?B�\?�  ?��R?�Q�?�(�?��H@\)@!G�@.{@@  @Tz�@aG�@s33@�G�@��@�@�(�@�ff@���@��H@\@˅@�z�@�  @���@��@�Q�A�AffA
�HA\)A33AQ�A�A   A$z�A)��A.{A2�\A6ffA:=qA@  AC�
AG�AL��AQG�AUAY��A^{Ab�\Ag�Ak�Ap��Au�Az�HA�  A��A�(�A�\)A���A��
A�{A�G�A��
A�A�Q�A�33A��A�\)A��A���A�
=A�G�A��
A�ffA���A�33A�p�A�Q�A��HA���A�
=A���A�z�A�
=A���A�33A�A�Q�A�=qA�z�A�\)Aٙ�A��
A�{A��A�A�ffA�Q�A�\A�p�A�Q�A��HA���A�\)A�=qA���A�
=B ��B=qB�B��B�B33B��B	�B
�HB(�Bp�B�RB�B��B{B\)Bz�Bp�B�RB  B�B=qB�B��B�B�HB Q�B!B"�HB$  B%G�B&�\B(  B(��B*=qB+�B,��B.{B/33B0Q�B1B3
=B4(�B5G�B6�RB8  B9G�B:ffB;�B<��B>ffB?�B@��BA�BC33BD��BEBF�HBH  BIp�BJ�RBK�
BL��BN=qBO�BP��BQBR�HBT(�BU��BV�\BW�BX��BY�B[\)B\z�B]p�B^�\B`  BaG�Bb=qBc\)Bd��Bf{Bg33BhQ�Bip�Bj�HBlQ�Bm��Bn�\Bo�Bp��BrffBs�Bt��Bu�Bw\)Bx��ByBz�HB|Q�B}B33B�(�B���B��B�=qB���B��B�Q�B�
=B�B�z�B�33B��
B��\B�\)B�{B���B�p�B�{B��HB���B�Q�B��HB���B�ffB��B�B�ffB�
=B�B��\B�G�B��B�z�B�33B��B���B�G�B��
B��\B�G�B�  B���B�33B��
B��\B�G�B��
B�ffB��B��
B�z�B��B��B�ffB�
=B�B�Q�B��HB��B�=qB��HB�p�B�  B��RB�p�B�  B��\B��B��
B�z�B��B��B�=qB���B��B�Q�B���B���B�=qB���B�B�ffB�
=B��B�z�B�33B��B���B�33B��B��\B�G�B�{B��RB�p�B�{B��RB�\)B�{B���B�p�B�  Bď\B�
=BŅB�{BƏ\B��HB��B�\)BǙ�B��
B�{B�Q�B�ffB�z�Bȏ\B���B���B��B�33B�33B�\)Bə�B�B��B�  B�{B�(�B�ffBʏ\BʸRB��HB���B�
=B�33B�p�BˮB�B��
B�  B�{B�ffB̏\B���B��HB�
=B�33B�p�B͙�B��
B�  B�(�B�=qB�z�BθRB���B��B�\)B�p�Bϙ�B�B�  B�=qB�ffBУ�BиRB��HB�
=B�G�BхBѮB��B�{B�=qB�ffBҏ\BҸRB���B�33B�p�Bә�BӮB��
B�  B�(�B�z�Bԏ\BԸRB��HB���B��B�\)Bՙ�B�B��B�{B�(�B�Q�B�ffB֣�B���B�
=B�33B�G�B�\)BׅBי�B��
B�  B�=qB�ffB؏\Bأ�BظRB���B��B�\)BمBٮB��
B�  B�(�B�ffBڣ�B��HB�
=B�G�B�p�Bۙ�B��
B�{B�ffBܣ�B���B�33B�p�Bݙ�B��
B�{B�ffBޣ�B���B�G�B߅B��
B�{B�Q�B�z�B���B�
=B�\)BᙚB�  B�=qB��B��HB��B�\)B㙚B��B�=qB�z�B���B�33B�B�B�{B�Q�B�\B���B�33B�p�B�B�{B�z�B�RB�
=B�\)B陚B��
B�{B�ffB�RB��B�p�B�B�  B�Q�B�\B���B��B�\)B��B��B�=qB�\B��HB�G�B�B��B�(�B�z�B���B��B�p�B�B�  B�Q�B��B���B�\)B�B�{B�ffB���B��B��B��
B�=qB��\B��HB�33B���B��B�=qB��\B���B�G�B��B�  B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�ffB���B�33B���C 
=C =qC z�C ��C �HC{CG�Cz�C�RC�HC{CG�Cz�C�C�HC{CQ�C�C�RC��C(�C\)C��C��C
=C=qCp�C�C�C�CQ�C�\CC  C33CffC��C�HC�CQ�C�\CC	  C	33C	p�C	�C	�HC
�C
\)C
�\C
��C
=C=qCp�C�C�C�C\)C��C�
C{CG�C�CC��C33Cp�C�C�C(�C\)C��C�
C{CQ�C�CC
=CG�Cz�C�RC��C33Cp�C�C�C�C\)C��C�
C{CQ�C�\C��C
=CQ�C�CC  C=qCz�C�RC��C=qCp�C�C��C33Cp�C�RC��C33Cp�C�C��C33Cp�C�C�HC(�CffC��C�HC{C\)C��C�
C{C\)C�\C�HC{C\)C��C�HC�C\)C��C�
C 
=C Q�C �\C C!
=C!G�C!�C!��C"
=C"Q�C"�\C"��C#
=C#=qC#�C#�RC#��C$33C$p�C$��C$�HC%�C%\)C%��C%�
C&�C&\)C&��C&�
C'{C'Q�C'�\C'��C(
=C(=qC(z�C(�RC(��C)33C)p�C)C*  C*=qC*p�C*�C*�HC+�C+Q�C+�\C+��C,
=C,G�C,�C,C,��C-(�C-ffC-��C-��C.  C.33C.p�C.��C.�HC/�C/Q�C/z�C/�C/�HC0{C0Q�C0�\C0��C0��C1=qC1ffC1��C1C1��C2(�C2ffC2��C2�
C3
=C3=qC3p�C3��C3��C3��C4(�C4ffC4��C4�
C5
=C5=qC5ffC5��C5��C6
=C6=qC6z�C6�C6�HC7
=C7=qC7p�C7��C7�
C8
=C8G�C8z�C8�RC8�HC9{C9=qC9p�C9�C9�C:�C:Q�C:z�C:�RC:�HC;�C;Q�C;�\C;��C;��C<33C<ffC<��C<��C=
=C=G�C=�\C=��C>  C>=qC>p�C>�C>��C?33C?z�C?�RC?��C@(�C@ffC@��C@�HCA(�CAp�CA�CA��CB33CBffCB�CB��CC=qCC�CC��CD
=CD=qCD�CD��CE{CEffCE�CE�CF(�CFp�CFCG
=CG\)CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��A��A��A��A��A��#A���A���A��A��TA��/A��HA��mA��yA��A��A��A��`A��mA��yA��yA��yA��A��A��A��A��A��A��A���A���A���A���A���A���A�  A�A�  A�A�A���A���A���A��yA���Aϥ�A�E�AͮA�  A�\)A�\)A�^5A���A�9XA�A��PA�|�A��A��mA���A�=qA�I�A��-A�1A�  A��A�M�A��RA���A�G�A�bNA�%A�G�A��7A���A�p�A�S�A��`A��FA��A�dZA�`BA�&�A�`BA��\A�p�A��A��AwVAt�/As�
AoAg
=Aap�AXĜATQ�APȴAOoAG�AB�+A?;dA=G�A;�^A:�A8��A7S�A6JA5C�A4��A4�A3��A4z�A3�-A2�A0I�A/�A0JA/�A/t�A.VA-C�A,�9A,r�A+�A*ĜA)A)\)A)oA(�A(�A'�A'
=A&A�A&A%XA$ �A#��A#S�A"�A"�\A"�\A"Q�A!��A!K�A ��A M�A��A�RA$�A�;A�AO�A�A�!AQ�A�FA33A��A��An�AE�A�A�;A��A&�A��AE�A{A�AȴA=qA�AVA�+A �A�A��At�AK�AVAjA�
A�wA��AS�A�A�RAv�AM�AZA�Al�A/A;dA�jAVA�A�A�-A`BA��A�\A(�A�
AhsA
�`A
�\A
E�A	��A	;dA��A��An�A�A�TA�A+A%A�/A�uAffAE�AA�A`BAC�A�/AZA��A��A&�A��AI�A$�A�A;dAA �A ~�A   @��
@���@�\)@��\@���@�X@�G�@�/@�Q�@��
@��@�l�@�33@�-@���@�/@���@�Z@��@��y@�ff@�-@���@�G�@���@�Q�@���@�33@�@�M�@�h@�Ĝ@�Q�@��
@�t�@�\@�J@���@��#@�@홚@��`@�9X@�@���@��#@�`B@��@�Z@�9X@��@�@��@��H@旍@�V@�hs@�%@�j@�C�@�R@�ff@���@���@�z�@�1@߾w@߅@�l�@�K�@��@��y@ޟ�@�-@���@�x�@ܛ�@ۅ@��@�7L@ش9@�I�@��@��;@�l�@ָR@�^5@��@���@պ^@պ^@ա�@Չ7@��@ԋD@�  @ӍP@�+@�@ҟ�@�-@�hs@��@�z�@ύP@�+@��@�~�@�V@�=q@�5?@���@���@͑h@��@�1'@��m@ˮ@�t�@�dZ@��@�{@�?}@ȃ@�l�@��@��@��@���@���@�V@�x�@���@��@î@�dZ@�n�@�=q@��@���@�p�@�&�@�1'@��@�33@��y@���@���@�-@��#@�x�@�?}@��@�bN@�K�@���@��+@�^5@�M�@�E�@�5?@���@��j@�(�@���@�S�@�K�@���@�O�@�j@��m@�|�@�C�@�
=@��\@��h@��/@��u@�Z@�l�@��@��\@�M�@��@��-@�`B@���@�I�@�b@��;@�|�@��@�v�@���@�G�@�Ĝ@�z�@�bN@�1'@�  @��@���@�~�@�=q@��#@�p�@��@�1'@��F@�"�@��y@��!@�v�@�^5@�E�@���@��h@�hs@�G�@��@��j@�bN@�9X@�1@���@�33@��y@���@��\@�-@���@�p�@�V@���@��9@��9@��9@��@�I�@��m@��@�|�@�33@���@���@�5?@��@�@��h@�G�@�V@���@��j@��D@�Z@�A�@� �@���@��@�C�@���@�J@���@�x�@�/@���@���@��u@�9X@�b@��@��@�dZ@�+@��y@���@�^5@�{@��7@�?}@��/@���@�bN@� �@��m@��F@�K�@�+@��H@��R@���@��\@�$�@��T@���@��@�x�@�`B@�%@�Ĝ@��u@��@��@��F@�K�@��@���@�ȴ@��R@�~�@�E�@��@��h@�O�@���@��u@�z�@�A�@��@��F@��@�t�@�K�@�o@���@���@���@��+@�M�@�$�@�{@���@��@��#@���@�p�@�G�@��@��@�Ĝ@�bN@�b@��F@��P@�l�@�S�@�C�@�33@�
=@��@��@���@�E�@�$�@�{@�@�G�@��@���@�z�@�b@�  @�@�P@�@~��@}�@}V@|�/@|��@|�j@|�j@|��@|(�@{t�@z�!@z^5@y�^@yX@y7L@y&�@y&�@y%@xbN@x �@w�@w�@vv�@u��@u�h@u�h@uV@tj@st�@s"�@r�@r~�@pr�@p �@o�@o�@o|�@oK�@n��@n5?@n$�@m��@k�F@kdZ@kdZ@kS�@k@j�@j��@j�!@j�\@j=q@j�@i��@i�@h  @g\)@f�R@fV@f{@e�@e@e�h@e�@e�@e�@e�@eO�@dz�@c��@c@b��@ahs@`��@`A�@`  @_�;@_�w@^�R@^v�@^V@^5?@^@]�@]�T@]�h@\�@\j@\�@[ƨ@[�F@[dZ@[33@Z�H@Z�!@Z~�@Z�@Y�#@Y�7@Y%@X��@X��@XbN@X1'@W��@Vȴ@VV@V@U@U�@U�@T�j@S��@S�@SC�@S@R��@Q��@Qx�@QG�@Q�@Q%@P�`@PĜ@Pr�@P �@P  @O�@Ol�@N��@N��@NE�@N$�@M��@MO�@L�@Lz�@L(�@K�
@K�F@K33@J�@H��@Hr�@H1'@G�@G�w@GK�@F�@F��@F5?@E�T@E�-@Ep�@EO�@EV@D��@D1@C�m@C�@B��@B�@A�#@A��@A��@A�7@A%@@�@?��@?��@?l�@?;d@>��@>ȴ@>�+@>V@>$�@>{@=�@=�T@=��@<��@<(�@;�F@;t�@;dZ@;C�@;@:��@:�!@:~�@:-@9&�@8�9@8��@8Q�@7l�@7�@6�R@6�+@6{@5@5p�@5?}@5�@4�@4�@4��@4�D@4j@4j@4Z@4I�@4(�@3��@3��@3t�@3C�@333@2�@2��@2�\@1�#@1x�@1G�@1�@0�9@0r�@0  @/K�@/+@/�@.��@.�y@.�@.�R@.�R@.��@.5?@.{@.@-�T@-�@-V@,��@,��@,�D@,Z@,�@,1@+�
@+C�@+33@*�@*�\@*M�@*=q@*-@*�@)�#@)�^@)��@(�`@(�u@(A�@'�@'�;@'��@'�@'��@'�P@'K�@'K�@'
=@&E�@&{@%�@%@%�h@%`B@%/@$�@$��@$(�@#�F@#S�@"��@"=q@!��@!�#@!��@!X@!%@ r�@   @�@�;@�w@�P@l�@K�@�@��@�y@V@`B@?}@?}@/@/@�@�@�@��@j@ƨ@t�@t�@dZ@o@^5@-@J@��@hs@�@��@��@1'@�@�w@�P@\)@\)@;d@
=@ȴ@�R@�R@�@��@`B@/@V@�/@�j@�j@�@I�@��@�
@�F@��@��@�@t�@S�@o@�@��@~�@M�@-@��@�#@��@�^@��@�7@&�@�`@�u@b@�P@
=@�R@��@v�@V@@@�h@`B@?}@�@��@��@��@j@�@��@ƨ@��@�@"�@
�@
�H@
��@
�!@
�!@
��@
�\A���A���A��A��
A���A��A��#A��A��
A��A��/A��A��
A��
A��#A��#A��
A��
A��#A��/A��A��A��A��#A��#A��A��
A��A��/A��/A��A���A���A��
A���A��
A��
A��
A��
A��HA��HA��`A��TA��;A��TA��mA��`A��;A��#A��;A��HA��#A��
A��
A��/A��/A��;A��`A��yA��A��`A��`A��yA��yA��mA��mA��yA��A��yA��mA��A��yA��mA��mA��A��A��mA��yA��A��A��A��yA��A��A��A��A��A��A��A��A��yA��mA��mA��mA��TA��`A��mA��yA��mA��TA��`A��mA��yA��`A��`A��mA��yA��mA��mA��yA��A��A��mA��`A��A��yA��mA��`A��yA��A��yA��mA��yA��A��A��yA��mA��A��A��yA��yA��A��A��yA��A��A��A��A��A��A���A��A��A��A���A��A��A��A��A���A��A��A��A���A���A��A��A��A���A��A��A��A��A���A��A��A��A��A���A��A��A��A���A���A��A��A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A�  A���A�  A�A�%A�A�A�  A�A�A�%A�  A���A�  A�A�  A���A���A�A�A�A�  A�  A�A�A�  A���A���A�  A�A�  A���A���A���A���A���A���A���A��A���A�  A���A��A���A�  A�  A��`A��TA��HA��`A��TA��;A��/A���A��/A��A϶FAϲ-AϺ^Aϰ!Aϩ�Aϥ�Aχ+AσA�l�A�S�A�C�A�5?A�$�A���A�|�A�M�A�&�AͶFA�1A̍PAˏ\A�ffA��AɬAɝ�A�r�A�ĜA�$�AƉ7A�JA�bNA�p�A�AÛ�A�z�A���A¸RA�A�M�A��A�ƨA�;dA�M�A���A�~�A�1'A���A��A���A�A�A�JA��A��jA���A�|�A�\)A�M�A�G�A�;dA�1'A�-A�$�A��A��A�1A���A��A��A��A��;A���A���A��-A��A�dZA�O�A�?}A�9XA�9XA�9XA�9XA�-A�-A�&�A� �A��A�
=A���A���A�ƨA��9A���A���A��A�dZA�5?A��#A��!A�l�A�"�A�{A���A���A�ĜA��-A���A�z�A�^5A�?}A�1'A�-A�&�A��A�VA�A��A��A��`A��HA���A���A���A�ȴA���A���A���A��7A��A�r�A�p�A�p�A�n�A�l�A�hsA�S�A�E�A�=qA�9XA�33A�/A�+A�(�A�&�A�&�A�&�A�&�A�(�A�(�A�&�A�$�A�"�A�$�A�$�A��A��A��A��A��A��A�VA�A�A�A�  A���A���A��A��A��/A�A��+A�G�A�&�A��A��A��A��yA���A���A��A�S�A���A���A��hA�|�A�l�A�hsA�ffA�bNA�M�A���A���A�ZA�"�A�1A��`A��^A�E�A��A�JA��A���A�ƨA���A��7A�r�A�\)A�5?A��
A�Q�A� �A��A��A�{A�{A��
A���A�x�A�E�A��A��A��;A���A��!A���A�x�A�O�A�"�A��A���A��jA���A���A��7A��A��A�|�A�|�A�p�A�Q�A�-A��A���A��#A��\A� �A���A���A��+A�A�A��A��A�bNA��wA�9XA�dZA���A�G�A��#A��+A�-A��A���A��DA�M�A�A�A�9XA�33A�&�A��A��A���A���A��A���A�hsA�9XA��A�A�A��A�oA�%A�A���A��A��yA��`A��RA���A���A�ĜA���A��FA��!A��A���A���A���A��uA��hA��hA��hA��hA���A��-A���A���A���A�`BA��A�VA�  A��A��;A��HA�ĜA���A�l�A�O�A�I�A�G�A�C�A�&�A��`A��!A�x�A�S�A�9XA��A�  A���A�I�A�%A��^A��A�Q�A�1'A� �A�bA�{A�$�A��A��A���A��hA�jA�^5A�hsA�ZA�-A� �A�5?A�A�A�A�A�&�A�  A�ĜA���A�|�A�K�A�1A���A�hsA�;dA� �A��A�bA��TA�ĜA���A�O�A��A��A��A��A��A��A�{A�JA���A��A��;A�ȴA��\A�7LA�x�A��A��9A�v�A�XA�?}A�"�A�  A���A���A�I�A�VA��wA�z�A��A���A��hA�jA�dZA�dZA�dZA�bNA�ZA�9XA�1'A�{A�ƨA�jA�5?A�A�ȴA���A�^5A��A��9A�A�A���A���A�M�A��A���A��/A��A�K�A�;dA�n�A��;A��uA�^5A�+A��A���A�dZA�;dA�$�A���A��
A�ȴA���A���A�v�A�ZA�C�A��A���A���A��/A�  A��HA��hA�oA��#A�A���A�$�A�M�A���A��A~�DA|��Az��Ay
=Ax�+Ax{Aw`BAv�Av�AvI�AvJAu�Au��Au;dAt�At��At�9At��At��At~�AtffAtM�AtA�At�As�As��As�^As��As�AsXAr�yAq��AqC�Ap��Ap$�Ao�AodZAnQ�Al�Ak�Ai�Ahr�Ag�Ag�AgVAfZAe�^Ae7LAd��Ad�9Ac�
Ab�`Ab��Ab5?Aa��AaS�A`=qA_l�A]l�A\��A[�A[7LAZ^5AY;dAXI�AWXAV��AV5?AU��AUt�AUC�AU�AT��AT��AT��ATr�AS�#AS/ARn�AQ�wAQXAQoAP�`AP��AP�!AP��APr�APM�AP�AO�
AO|�AOG�AO/AO
=AN��AN�HAN�9ANffALz�AK"�AJ�DAI�AH�AE�^ADA�AC"�ACACVAC33ACC�AC�AC�ABZAA%A@bNA?�A?�hA?�7A?`BA?A>��A>��A>��A>~�A>r�A=�A<�9A<�DA<ffA<Q�A<M�A<1'A;�A;��A;��A;XA;�A;oA:��A:�A:��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           A���A��A��A��A��A��A��A��#A���A���A��A��TA��/A��HA��mA��yA��A��A��A��`A��mA��yA��yA��yA��A��A��A��A��A��A��A���A���A���A���A���A���A�  A�A�  A�A�A���A���A���A��yA���Aϥ�A�E�AͮA�  A�\)A�\)A�^5A���A�9XA�A��PA�|�A��A��mA���A�=qA�I�A��-A�1A�  A��A�M�A��RA���A�G�A�bNA�%A�G�A��7A���A�p�A�S�A��`A��FA��A�dZA�`BA�&�A�`BA��\A�p�A��A��AwVAt�/As�
AoAg
=Aap�AXĜATQ�APȴAOoAG�AB�+A?;dA=G�A;�^A:�A8��A7S�A6JA5C�A4��A4�A3��A4z�A3�-A2�A0I�A/�A0JA/�A/t�A.VA-C�A,�9A,r�A+�A*ĜA)A)\)A)oA(�A(�A'�A'
=A&A�A&A%XA$ �A#��A#S�A"�A"�\A"�\A"Q�A!��A!K�A ��A M�A��A�RA$�A�;A�AO�A�A�!AQ�A�FA33A��A��An�AE�A�A�;A��A&�A��AE�A{A�AȴA=qA�AVA�+A �A�A��At�AK�AVAjA�
A�wA��AS�A�A�RAv�AM�AZA�Al�A/A;dA�jAVA�A�A�-A`BA��A�\A(�A�
AhsA
�`A
�\A
E�A	��A	;dA��A��An�A�A�TA�A+A%A�/A�uAffAE�AA�A`BAC�A�/AZA��A��A&�A��AI�A$�A�A;dAA �A ~�A   @��
@���@�\)@��\@���@�X@�G�@�/@�Q�@��
@��@�l�@�33@�-@���@�/@���@�Z@��@��y@�ff@�-@���@�G�@���@�Q�@���@�33@�@�M�@�h@�Ĝ@�Q�@��
@�t�@�\@�J@���@��#@�@홚@��`@�9X@�@���@��#@�`B@��@�Z@�9X@��@�@��@��H@旍@�V@�hs@�%@�j@�C�@�R@�ff@���@���@�z�@�1@߾w@߅@�l�@�K�@��@��y@ޟ�@�-@���@�x�@ܛ�@ۅ@��@�7L@ش9@�I�@��@��;@�l�@ָR@�^5@��@���@պ^@պ^@ա�@Չ7@��@ԋD@�  @ӍP@�+@�@ҟ�@�-@�hs@��@�z�@ύP@�+@��@�~�@�V@�=q@�5?@���@���@͑h@��@�1'@��m@ˮ@�t�@�dZ@��@�{@�?}@ȃ@�l�@��@��@��@���@���@�V@�x�@���@��@î@�dZ@�n�@�=q@��@���@�p�@�&�@�1'@��@�33@��y@���@���@�-@��#@�x�@�?}@��@�bN@�K�@���@��+@�^5@�M�@�E�@�5?@���@��j@�(�@���@�S�@�K�@���@�O�@�j@��m@�|�@�C�@�
=@��\@��h@��/@��u@�Z@�l�@��@��\@�M�@��@��-@�`B@���@�I�@�b@��;@�|�@��@�v�@���@�G�@�Ĝ@�z�@�bN@�1'@�  @��@���@�~�@�=q@��#@�p�@��@�1'@��F@�"�@��y@��!@�v�@�^5@�E�@���@��h@�hs@�G�@��@��j@�bN@�9X@�1@���@�33@��y@���@��\@�-@���@�p�@�V@���@��9@��9@��9@��@�I�@��m@��@�|�@�33@���@���@�5?@��@�@��h@�G�@�V@���@��j@��D@�Z@�A�@� �@���@��@�C�@���@�J@���@�x�@�/@���@���@��u@�9X@�b@��@��@�dZ@�+@��y@���@�^5@�{@��7@�?}@��/@���@�bN@� �@��m@��F@�K�@�+@��H@��R@���@��\@�$�@��T@���@��@�x�@�`B@�%@�Ĝ@��u@��@��@��F@�K�@��@���@�ȴ@��R@�~�@�E�@��@��h@�O�@���@��u@�z�@�A�@��@��F@��@�t�@�K�@�o@���@���@���@��+@�M�@�$�@�{@���@��@��#@���@�p�@�G�@��@��@�Ĝ@�bN@�b@��F@��P@�l�@�S�@�C�@�33@�
=@��@��@���@�E�@�$�@�{@�@�G�@��@���@�z�@�b@�  @�@�P@�@~��@}�@}V@|�/@|��@|�j@|�j@|��@|(�@{t�@z�!@z^5@y�^@yX@y7L@y&�@y&�@y%@xbN@x �@w�@w�@vv�@u��@u�h@u�h@uV@tj@st�@s"�@r�@r~�@pr�@p �@o�@o�@o|�@oK�@n��@n5?@n$�@m��@k�F@kdZ@kdZ@kS�@k@j�@j��@j�!@j�\@j=q@j�@i��@i�@h  @g\)@f�R@fV@f{@e�@e@e�h@e�@e�@e�@e�@eO�@dz�@c��@c@b��@ahs@`��@`A�@`  @_�;@_�w@^�R@^v�@^V@^5?@^@]�@]�T@]�h@\�@\j@\�@[ƨ@[�F@[dZ@[33@Z�H@Z�!@Z~�@Z�@Y�#@Y�7@Y%@X��@X��@XbN@X1'@W��@Vȴ@VV@V@U@U�@U�@T�j@S��@S�@SC�@S@R��@Q��@Qx�@QG�@Q�@Q%@P�`@PĜ@Pr�@P �@P  @O�@Ol�@N��@N��@NE�@N$�@M��@MO�@L�@Lz�@L(�@K�
@K�F@K33@J�@H��@Hr�@H1'@G�@G�w@GK�@F�@F��@F5?@E�T@E�-@Ep�@EO�@EV@D��@D1@C�m@C�@B��@B�@A�#@A��@A��@A�7@A%@@�@?��@?��@?l�@?;d@>��@>ȴ@>�+@>V@>$�@>{@=�@=�T@=��@<��@<(�@;�F@;t�@;dZ@;C�@;@:��@:�!@:~�@:-@9&�@8�9@8��@8Q�@7l�@7�@6�R@6�+@6{@5@5p�@5?}@5�@4�@4�@4��@4�D@4j@4j@4Z@4I�@4(�@3��@3��@3t�@3C�@333@2�@2��@2�\@1�#@1x�@1G�@1�@0�9@0r�@0  @/K�@/+@/�@.��@.�y@.�@.�R@.�R@.��@.5?@.{@.@-�T@-�@-V@,��@,��@,�D@,Z@,�@,1@+�
@+C�@+33@*�@*�\@*M�@*=q@*-@*�@)�#@)�^@)��@(�`@(�u@(A�@'�@'�;@'��@'�@'��@'�P@'K�@'K�@'
=@&E�@&{@%�@%@%�h@%`B@%/@$�@$��@$(�@#�F@#S�@"��@"=q@!��@!�#@!��@!X@!%@ r�@   @�@�;@�w@�P@l�@K�@�@��@�y@V@`B@?}@?}@/@/@�@�@�@��@j@ƨ@t�@t�@dZ@o@^5@-@J@��@hs@�@��@��@1'@�@�w@�P@\)@\)@;d@
=@ȴ@�R@�R@�@��@`B@/@V@�/@�j@�j@�@I�@��@�
@�F@��@��@�@t�@S�@o@�@��@~�@M�@-@��@�#@��@�^@��@�7@&�@�`@�u@b@�P@
=@�R@��@v�@V@@@�h@`B@?}@�@��@��@��@j@�@��@ƨ@��@�@"�@
�@
�H@
��@
�!@
�!@
��@
�\A���A���A��A��
A���A��A��#A��A��
A��A��/A��A��
A��
A��#A��#A��
A��
A��#A��/A��A��A��A��#A��#A��A��
A��A��/A��/A��A���A���A��
A���A��
A��
A��
A��
A��HA��HA��`A��TA��;A��TA��mA��`A��;A��#A��;A��HA��#A��
A��
A��/A��/A��;A��`A��yA��A��`A��`A��yA��yA��mA��mA��yA��A��yA��mA��A��yA��mA��mA��A��A��mA��yA��A��A��A��yA��A��A��A��A��A��A��A��A��yA��mA��mA��mA��TA��`A��mA��yA��mA��TA��`A��mA��yA��`A��`A��mA��yA��mA��mA��yA��A��A��mA��`A��A��yA��mA��`A��yA��A��yA��mA��yA��A��A��yA��mA��A��A��yA��yA��A��A��yA��A��A��A��A��A��A���A��A��A��A���A��A��A��A��A���A��A��A��A���A���A��A��A��A���A��A��A��A��A���A��A��A��A��A���A��A��A��A���A���A��A��A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A�  A���A�  A�A�%A�A�A�  A�A�A�%A�  A���A�  A�A�  A���A���A�A�A�A�  A�  A�A�A�  A���A���A�  A�A�  A���A���A���A���A���A���A���A��A���A�  A���A��A���A�  A�  A��`A��TA��HA��`A��TA��;A��/A���A��/A��A϶FAϲ-AϺ^Aϰ!Aϩ�Aϥ�Aχ+AσA�l�A�S�A�C�A�5?A�$�A���A�|�A�M�A�&�AͶFA�1A̍PAˏ\A�ffA��AɬAɝ�A�r�A�ĜA�$�AƉ7A�JA�bNA�p�A�AÛ�A�z�A���A¸RA�A�M�A��A�ƨA�;dA�M�A���A�~�A�1'A���A��A���A�A�A�JA��A��jA���A�|�A�\)A�M�A�G�A�;dA�1'A�-A�$�A��A��A�1A���A��A��A��A��;A���A���A��-A��A�dZA�O�A�?}A�9XA�9XA�9XA�9XA�-A�-A�&�A� �A��A�
=A���A���A�ƨA��9A���A���A��A�dZA�5?A��#A��!A�l�A�"�A�{A���A���A�ĜA��-A���A�z�A�^5A�?}A�1'A�-A�&�A��A�VA�A��A��A��`A��HA���A���A���A�ȴA���A���A���A��7A��A�r�A�p�A�p�A�n�A�l�A�hsA�S�A�E�A�=qA�9XA�33A�/A�+A�(�A�&�A�&�A�&�A�&�A�(�A�(�A�&�A�$�A�"�A�$�A�$�A��A��A��A��A��A��A�VA�A�A�A�  A���A���A��A��A��/A�A��+A�G�A�&�A��A��A��A��yA���A���A��A�S�A���A���A��hA�|�A�l�A�hsA�ffA�bNA�M�A���A���A�ZA�"�A�1A��`A��^A�E�A��A�JA��A���A�ƨA���A��7A�r�A�\)A�5?A��
A�Q�A� �A��A��A�{A�{A��
A���A�x�A�E�A��A��A��;A���A��!A���A�x�A�O�A�"�A��A���A��jA���A���A��7A��A��A�|�A�|�A�p�A�Q�A�-A��A���A��#A��\A� �A���A���A��+A�A�A��A��A�bNA��wA�9XA�dZA���A�G�A��#A��+A�-A��A���A��DA�M�A�A�A�9XA�33A�&�A��A��A���A���A��A���A�hsA�9XA��A�A�A��A�oA�%A�A���A��A��yA��`A��RA���A���A�ĜA���A��FA��!A��A���A���A���A��uA��hA��hA��hA��hA���A��-A���A���A���A�`BA��A�VA�  A��A��;A��HA�ĜA���A�l�A�O�A�I�A�G�A�C�A�&�A��`A��!A�x�A�S�A�9XA��A�  A���A�I�A�%A��^A��A�Q�A�1'A� �A�bA�{A�$�A��A��A���A��hA�jA�^5A�hsA�ZA�-A� �A�5?A�A�A�A�A�&�A�  A�ĜA���A�|�A�K�A�1A���A�hsA�;dA� �A��A�bA��TA�ĜA���A�O�A��A��A��A��A��A��A�{A�JA���A��A��;A�ȴA��\A�7LA�x�A��A��9A�v�A�XA�?}A�"�A�  A���A���A�I�A�VA��wA�z�A��A���A��hA�jA�dZA�dZA�dZA�bNA�ZA�9XA�1'A�{A�ƨA�jA�5?A�A�ȴA���A�^5A��A��9A�A�A���A���A�M�A��A���A��/A��A�K�A�;dA�n�A��;A��uA�^5A�+A��A���A�dZA�;dA�$�A���A��
A�ȴA���A���A�v�A�ZA�C�A��A���A���A��/A�  A��HA��hA�oA��#A�A���A�$�A�M�A���A��A~�DA|��Az��Ay
=Ax�+Ax{Aw`BAv�Av�AvI�AvJAu�Au��Au;dAt�At��At�9At��At��At~�AtffAtM�AtA�At�As�As��As�^As��As�AsXAr�yAq��AqC�Ap��Ap$�Ao�AodZAnQ�Al�Ak�Ai�Ahr�Ag�Ag�AgVAfZAe�^Ae7LAd��Ad�9Ac�
Ab�`Ab��Ab5?Aa��AaS�A`=qA_l�A]l�A\��A[�A[7LAZ^5AY;dAXI�AWXAV��AV5?AU��AUt�AUC�AU�AT��AT��AT��ATr�AS�#AS/ARn�AQ�wAQXAQoAP�`AP��AP�!AP��APr�APM�AP�AO�
AO|�AOG�AO/AO
=AN��AN�HAN�9ANffALz�AK"�AJ�DAI�AH�AE�^ADA�AC"�ACACVAC33ACC�AC�AC�ABZAA%A@bNA?�A?�hA?�7A?`BA?A>��A>��A>��A>~�A>r�A=�A<�9A<�DA<ffA<Q�A<M�A<1'A;�A;��A;��A;XA;�A;oA:��A:�A:��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�KB	�B	�KB	�B	�KB	خB	�B	�KB	ٴB	�B	�sB	�sB	�EB	��B	�
B	�
B	֡B	֡B	�
B	�sB	רB	�sB	רB	��B	��B	�?B	֡B	��B	�
B	�?B	�?B	�sB	�?B	�
B	�
B	�?B	�
B	��B	֡B	��B	�9B	֡B	��B	��B	��B	ӏB	бB	��B	�'B	��B	�(B	��B	��B
fB
h>B
~�B
�B
��B
��B
ݘB
��B'�B49BC-BIB/OB
�
B
�9B
�B�B)*B5B	B
�(B
�&B
�"B iB
��B
��B
�NB
��B
�UB
�MB
��B
h�B
G�B
!-B
�B	��B	�#B	�B	��B	��B	�fB	yrB	j�B	]�B	P�B	J�B	C-B	G�B	I�B	LdB	NB	MB	M�B	dZB	poB	z�B	��B	��B	��B	�B	��B	�B	ȀB	��B	�&B	��B	�HB	��B	��B	��B	�B	��B	�B	�B	�	B	�B
 �B
�B
	B
	B
PB
�B
�B
kB
�B
�B
B
�B
CB
 �B
$�B
)�B
/OB
3�B
5B
7�B
8�B
8�B
:^B
:�B
<�B
@OB
?�B
C�B
G�B
K)B
L�B
MB
L�B
MB
MjB
NB
N<B
O�B
QB
RTB
QNB
T�B
S&B
S[B
Q�B
PHB
P}B
R B
RTB
R�B
RTB
R�B
S�B
V9B
RTB
R B
Q�B
Q�B
QB
R�B
R B
R�B
U�B
V�B
U2B
Q�B
U�B
W�B
T�B
S[B
R�B
R B
R�B
QNB
R�B
R�B
Q�B
Q�B
P�B
N�B
NpB
N�B
MjB
M�B
LdB
LdB
K�B
K^B
J�B
I�B
H�B
H�B
G�B
FtB
EmB
E�B
DgB
B�B
B�B
B�B
A�B
?}B
=�B
>wB
=qB
;0B
:�B
9�B
9�B
7�B
7LB
9�B
7�B
7B
7B
6�B
6FB
4�B
33B
2�B
2-B
4B
2�B
2�B
2�B
33B
5B
2aB
2�B
1'B
0�B
2�B
2aB
0�B
0!B
0UB
/OB
/OB
.B
-�B
-�B
,�B
+�B
+�B
*�B
)�B
)*B
)*B
)�B
'�B
'�B
'�B
'RB
&�B
(�B
'RB
&B
%�B
%�B
$B
$@B
#:B
#B
#nB
#nB
"�B
%FB
$tB
#�B
%FB
"�B
"�B
"�B
!bB
!-B
 �B
 �B
#:B
#B
"4B
!�B
 �B
!-B
!-B
 �B
!�B
!�B
 �B
 �B
�B
�B
 'B
�B
�B
�B
!B
�B
VB
B
B
�B
�B
�B
�B
!B
�B
�B
�B
B
IB
�B
�B
�B
IB
�B
B
�B
IB
�B
�B
�B
7B
�B
�B
�B
=B
7B
	B
�B
eB
�B
�B
CB
xB
�B
eB
�B
�B
�B
�B
�B
�B
1B
kB
�B
�B
eB
+B
�B
=B
B
�B
�B
�B
�B
7B
_B
1B
�B
�B
�B
qB
7B
�B
�B
�B
�B
�B
B
xB
B
�B
=B
B
IB
B
OB
OB
�B
B
B
xB
CB
CB
xB
CB
B
xB
�B
�B
�B
�B
�B
B
B
CB
xB
xB
xB
�B
IB
~B
�B
�B
�B
�B
�B
�B
 \B
!�B
"hB
"4B
"hB
!�B
!�B
"hB
"hB
"4B
!�B
!�B
"�B
"�B
#nB
#nB
#�B
#�B
#�B
#�B
%FB
%FB
%zB
%�B
&B
&�B
'B
'B
'�B
)*B
)�B
*0B
*0B
*�B
+6B
+�B
,B
,�B
-B
,�B
,�B
,qB
-B
-B
-wB
-wB
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
1�B
1[B
1�B
1�B
1�B
1�B
1[B
1�B
2-B
1�B
1[B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2-B
2-B
2-B
2aB
2-B
2�B
2�B
2�B
3�B
33B
3hB
3�B
3�B
4B
4�B
49B
5?B
4�B
4�B
5?B
5�B
6FB
6zB
6FB
6FB
6FB
7B
7B
7LB
8RB
7�B
8�B
8�B
8�B
9$B
9XB
9$B
9�B
9�B
:^B
:�B
:�B
;�B
;�B
;�B
<B
<jB
<jB
<�B
<�B
<�B
=<B
=�B
=qB
=qB
=�B
>BB
>BB
>BB
>wB
>BB
>BB
>�B
>�B
>�B
?B
>�B
?HB
?}B
@OB
@OB
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
AUB
A�B
A�B
A�B
B�B
B�B
B[B
CaB
CaB
C�B
C�B
C�B
D3B
DgB
D�B
E�B
F�B
F?B
FB
F?B
FB
FB
FtB
GEB
G�B
HB
H�B
H�B
IB
H�B
H�B
IB
I�B
I�B
I�B
J#B
K^B
K)B
J�B
J�B
K�B
LdB
MB
L�B
L�B
M�B
OvB
OB
OBB
OBB
OB
OB
O�B
OvB
OB
P}B
R B
QNB
QB
QB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
Q�B
R�B
RTB
S�B
S�B
TaB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T,B
T�B
T�B
VB
UgB
U2B
VmB
VB
VB
VmB
V9B
V�B
XEB
XEB
XEB
X�B
X�B
X�B
XyB
YB
Y�B
YB
Y�B
YB
YB
Y�B
YB
Y�B
YB
Y�B
ZB
ZB
[#B
Z�B
[#B
[�B
[WB
[WB
\�B
]/B
\�B
]/B
\�B
\�B
]/B
^B
^jB
^jB
^jB
^�B
_B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`vB
`vB
`�B
`�B
`�B
`�B
aB
`�B
aHB
aHB
aHB
aHB
a|B
aB
aB
a|B
c�B
cTB
c�B
cTB
c�B
c�B
dZB
c�B
dZB
d&B
d�B
dZB
d�B
d�B
d�B
e,B
e�B
d�B
e�B
ffB
f�B
ffB
f�B
ffB
ffB
gB
g8B
g�B
g�B
g�B
g�B
h>B
h>B
hsB
h�B
h�B
h�B
h�B
hsB
h�B
i�B
jB
jB
jB
jB
jB
j�B
j�B
j�B
j�B
kB
l�B
lWB
l"B
l�B
m�B
n/B
ncB
m�B
n/B
n/B
ncB
ncB
ncB
ncB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
o B
o5B
oiB
oiB
o�B
p;B
p�B
qB
qvB
rB
rGB
rGB
r|B
rGB
rGB
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
tTB
tTB
tTB
t�B
u%B
t�B
uZB
uZB
uZB
u�B
u�B
v`B
v�B
v�B
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
x�B
x�B
y	B
y>B
y	B
y>B
y>B
yrB
yrB
y�B
yrB
y�B
zB
zB
zDB
zDB
zxB
zDB
zxB
z�B
z�B
{JB
{B
{�B
|B
|�B
|�B
|�B
|�B
}"B
}VB
~(B
~(B
~(B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
�B
��B
�iB
��B
�iB
��B
��B
�iB
�iB
�iB
�;B
��B
��B
��B
��B
�B
�B
��B
�B
�{B
��B
��B
�B
�MB
��B
��B
��B
��B
��B
��B
��B
�B
�SB
�B
�B
��B
�%B
��B
�YB
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
��B
��B
�1B
�1B
��B
��B
��B
�7B
�7B
�7B
�lB
�lB
�7B
�lB
�lB
�lB
�	B
��B
�rB
��B
�xB
�JB
�~B
�~B
��B
��B
��B
��B
��B
�"B
�"B
�VB
�VB
��B
��B
��B
�\B
�(B
��B
�\B
��B
�bB
�bB
�bB
��B
��B
��B
��B
�hB	�B	��B	�yB	�B	چB	�B	�yB	�B	�QB	�B	�B	��B	ٴB	��B	�KB	�EB	��B	چB	�yB	��B	�EB	�KB	�KB	�B	רB	�KB	چB	�B	�EB	�yB	�QB	�QB	�yB	�EB	�B	��B	��B	רB	��B	�B	�EB	�mB	֡B	��B	��B	֡B	�mB	�yB	�B	��B	֡B	�KB	�B	�QB	�B	��B	�B	�sB	֡B	�mB	רB	�EB	�9B	�mB	�
B	��B	֡B	�B	�sB	��B	՛B	��B	��B	רB	��B	��B	�?B	��B	��B	�9B	��B	רB	�?B	��B	�9B	�
B	��B	�B	��B	֡B	�yB	�
B	�mB	�
B	�yB	�B	�
B	֡B	�sB	خB	�B	�?B	�
B	�EB	�yB	רB	֡B	�EB	خB	�?B	�9B	��B	خB	خB	��B	�
B	�B	خB	�sB	�mB	�EB	�B	�B	�
B	�?B	�EB	��B	רB	��B	�EB	�yB	�
B	�
B	�yB	��B	��B	��B	�9B	רB	�B	՛B	��B	�sB	�mB	�B	�B	�sB	��B	֡B	�B	�mB	רB	�sB	�9B	�9B	��B	�EB	�
B	�B	�?B	��B	�B	�
B	�9B	��B	�EB	��B	�
B	�B	�?B	�EB	�
B	�B	�?B	�EB	خB	�
B	�9B	֡B	�sB	�EB	רB	֡B	֡B	�sB	�yB	��B	֡B	֡B	רB	�sB	�mB	�mB	�
B	��B	�sB	�B	��B	רB	�B	רB	�B	�mB	�?B	�EB	�
B	�B	֡B	�EB	רB	�9B	��B	�mB	רB	�?B	�mB	��B	�9B	�
B	�sB	�?B	�9B	�B	�sB	�
B	՛B	՛B	��B	��B	�B	��B	��B	��B	�EB	�sB	�B	��B	�?B	�
B	֡B	�gB	�B	�aB	��B	�mB	خB	�B	��B	ԕB	�9B	�?B	�[B	��B	ԕB	�gB	ԕB	�HB	�,B	�KB	�[B	�&B	�TB	уB	��B	�[B	ӏB	�BB	ϫB	�
B	ȴB	�dB	�jB	̘B	�B	ʌB	��B	��B	ÖB	�B	�}B	�B	��B	�9B	�'B	��B	��B	��B	��B	��B	�B	�MB	��B	}�B	|�B	�B	��B	cB	�+B	�fB	��B	�B	�RB	��B	�OB	��B	��B	�gB	�B	�B
�B
{�B
TaB
^jB
j�B
e�B
h�B
n/B
x�B
r|B
t�B
{B
|�B
yrB
.B
|B
{B
}�B
~�B
.B
~�B
~�B
�B
�uB
��B
��B
�uB
�;B
��B
��B
�B
�1B
�B
��B
��B
�FB
�B
��B
� B
�\B
��B
��B
��B
�.B
�"B
�oB
��B
�{B
�B
�_B
��B
��B
��B
�~B
��B
��B
��B
��B
ÖB
�?B
��B
ȀB
��B
�XB
˒B
҉B
уB
бB
�HB
��B
� B
��B
҉B
�,B
ԕB
��B
ԕB
�gB
רB
��B
�mB
��B
�
B
�B
�KB
��B
��B
ݘB
�#B
�B
�QB
چB
��B
ޞB
ޞB
�B
�/B
�jB
�dB
ݘB
�5B
�B
��B
�jB
ݘB
��B
�/B
�dB
��B
�jB
ݘB
�)B
�B
ܒB
�B
�/B
�/B
��B
ߤB
�5B
�/B
�]B
�)B
ܒB
��B
�B
��B
�dB
�/B
�ZB
�B
��B
�5B
�JB�B
�rB
�2B
��B!�BN�B7B!�B!�B"4B!-B�BOB�B#�B'�B&LB4B)�B(�B,�B)�BGB,=B+B-wB-�B,=B3�B1�B2-B0�B2-B>�BI�B:�B8�B:^B>B?�BFBA�BC-BB[B@OBB�BD�BH�BJ�BH�BOBLdBK)BM�BIRBJ�BI�BI�BIBGBF�BFtBE�BF�BJ#BD3B?B>B;�BB�B>�B,B)*B.BB+BB�B
rBB
��B
ںB
ҽB
��B
ŢB
��B
��B
�B
�6B
�$B
�qB
�[B
�9B
�RB
�gB
�<B
�OB
�B
��B
�0B
�jB
�B
��B
�|B
�}B
ѷB
ѷB
�B
��B
�,B
��B
ܒB
�
B
�B
�B�BeB+kB+�B,B*�B)_B(�B*�B)_B)_B)�B)_B'B1[B,B;�BAUB@B<�B0�B1'B/OB.�B&�B,B&LB �BMB�B�B.B�B.B�B+B
��B
��B
��B
�2B
��B
�fB
�B
�)B
��B
�B
�pB
�B
�B
֡B
�%BB
��B�BB
�xB
��B
�B
��B
�.B
��B
�B
��BB�BSBGB  B
�PBAB
�B
�+B
��B
�KB
�B
�B
�;B
�B
��B
��B
�|B
��B
�&B
�&B
�B
��B
�B
�B
�B
� B
�B
�B
�B
�B
ߤB
�QB
�}B
�#B
��B
�<B
�XB
�XB
�0B
�RB
�nB
�B
��B
�B
��B
��B
�FB
�@B
��B
��B
�@B
��B
�bB
�4B
��B
�_B
�=B
��B
��B
�+B
�uB
cB
zDB
zB
v�B
l�B
w�B
d�B
`B
YB
T�B
OB
G�B
K)B
S�B
j�B
F�B
9�B
-B
*0B
'B
)�B
$tB
!-B
�B
YB
	B
B
hB
�B
VB
�B

rB
�B
	�B
	B
�B
�B	�B	��B	��B	��B	��B	ܒB	یB	�;B	�2B	�B	�]B	خB	��B	�8B	��B	�qB	��B	��B	�tB	�B	��B	�IB	��B	�!B	��B	�B	�+B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�rB	�@B	��B	��B	{B	�lB	{�B	��B	��B	� B	zB	�wB	}�B	t�B	q�B	t�B	v�B	s�B	j�B	kB	e,B	z�B	h�B	bB	g�B	`vB	c�B	e�B	poB	{�B	aHB	ZB	\�B	cTB	d�B	\�B	l�B	S�B	XyB	T�B	R�B	Q�B	OBB	OBB	OvB	K�B	MjB	QNB	V�B	V�B	YKB	K�B	M�B	J#B	IB	H�B	G�B	GB	HKB	E�B	G�B	H�B	E�B	C�B	@�B	<�B	>�B	;�B	@OB	sB	H�B	9�B	NpB	VmB	MjB	H�B	4nB	($B	,qB	1[B	M�B	JXB	I�B	d�B	XEB	S�B	PHB	L0B	H�B	OBB	NpB	K^B	JXB	H�B	G�B	FB	W�B	[�B	K�B	MB	K^B	J�B	M�B	NpB	L�B	M�B	O�B	I�B	JXB	GzB	I�B	S&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           B	��B	��B	��B	��B	��B	�`B	��B	��B	�fB	��B	�%B	�%B	��B	юB	мB	мB	�SB	�SB	мB	�%B	�ZB	�%B	�ZB	юB	юB	��B	�SB	ЈB	мB	��B	��B	�%B	��B	мB	мB	��B	мB	ЈB	�SB	ЈB	��B	�SB	ςB	ςB	ͪB	�AB	�cB	ħB	��B	�ZB	��B	��B	��B
B
a�B
xCB
��B
ŭB
ԠB
�JB
�=B!�B-�B<�BB�B)B
мB
��B
��B
�:B"�B.�B�B
��B
��B
��B
�B
�B
�uB
� B
áB
�B
��B
�zB
b�B
A�B
�B
qB	�@B	��B	��B	�zB	�wB	�B	s$B	d�B	WJB	J�B	D>B	<�B	A�B	C8B	FB	G�B	F�B	GQB	^B	j!B	t�B	nB	�kB	�aB	��B	�NB	��B	�2B	�B	��B	֭B	��B	߰B	�B	�B	��B	�B	�4B	�4B	�B	��B	��B
 �B
�B
�B
B
�B

}B
B
nB
�B
�B
�B
�B
wB
�B
#yB
)B
-�B
.�B
12B
2mB
2mB
4B
4yB
6QB
:B
9�B
=HB
A`B
D�B
FJB
F�B
FJB
F�B
GB
G�B
G�B
I�B
J�B
LB
K B
N|B
L�B
MB
KiB
I�B
J/B
K�B
LB
L;B
LB
LoB
MAB
O�B
LB
K�B
K5B
KiB
J�B
L;B
K�B
LoB
OMB
PSB
N�B
KiB
O�B
Q�B
N�B
MB
LoB
K�B
LoB
K B
L;B
LoB
K5B
K�B
JcB
H�B
H"B
HWB
GB
GQB
FB
FB
E�B
EB
D>B
C8B
B2B
BfB
A`B
@&B
?B
?TB
>B
<vB
<�B
<vB
;;B
9/B
7�B
8)B
7#B
4�B
4EB
3�B
3�B
1gB
0�B
3�B
12B
0�B
0�B
0�B
/�B
.�B
,�B
,HB
+�B
-�B
,�B
,|B
,|B
,�B
.�B
,B
,|B
*�B
*<B
,�B
,B
*�B
)�B
*B
)B
)B
'�B
'^B
'�B
&�B
%QB
%�B
$�B
#�B
"�B
"�B
#yB
!9B
!mB
!9B
!B
 gB
"?B
!B
�B
aB
aB
�B
�B
�B
�B
 B
 B
NB
�B
&B
�B
�B
�B
�B
NB
B
�B
BB
wB
�B
�B
�B
HB
wB
�B
�B
�B
HB
HB
�B
BB
<B
�B
�B
pB
pB
pB
�B
�B
B
�B
�B
�B
�B
jB
<B
�B
6B
�B
dB
�B
�B
^B
^B
�B
�B
^B
�B
�B
�B
�B
�B
�B
�B
�B
RB
�B
�B
�B
�B
XB
B
LB
�B
�B
*B
^B
B
RB
LB
�B
LB
LB
LB
�B
B
�B
zB
B
�B
RB
�B
�B
�B
XB
RB
�B
�B
B
�B
RB
XB
�B
#B
�B
XB
�B
�B
dB
�B
�B
*B
�B
XB
�B
�B
�B
�B
B
B
dB
�B
�B
*B
�B
�B
*B
�B
�B
*B
^B
^B
�B
�B
^B
�B
�B
�B
*B
*B
*B
dB
�B
0B
dB
�B
6B
�B
<B
<B
B
HB
B
�B
B
�B
�B
B
B
�B
�B
HB
NB
NB
 B
 B
UB
�B
UB
�B
�B
�B
,B
aB
�B
 gB
 �B
 �B
!9B
"�B
#EB
#�B
#�B
$�B
$�B
%�B
%�B
&WB
&�B
&�B
&WB
&#B
&�B
&�B
')B
')B
'^B
'^B
'^B
'�B
(�B
(dB
(dB
(�B
)jB
)jB
)�B
+BB
+B
+�B
+vB
+BB
+BB
+B
+vB
+�B
+�B
+B
+�B
+�B
+vB
+vB
+�B
+�B
+�B
+vB
,HB
+�B
+�B
+�B
,B
+�B
,�B
,|B
,|B
-NB
,�B
-B
-NB
-�B
-�B
.�B
-�B
.�B
.�B
.�B
.�B
/ZB
/�B
0,B
/�B
/�B
/�B
0�B
0�B
0�B
2B
1�B
28B
2�B
2�B
2�B
3
B
2�B
3>B
3sB
4B
4yB
4EB
5KB
5KB
5KB
5�B
6B
6B
6�B
6�B
6�B
6�B
7WB
7#B
7#B
7WB
7�B
7�B
7�B
8)B
7�B
7�B
8]B
8�B
8�B
8�B
8�B
8�B
9/B
:B
:B
:B
:jB
:5B
:jB
:5B
:�B
:jB
:jB
;B
;pB
;;B
;;B
<AB
<AB
<B
=B
=B
=�B
=�B
=|B
=�B
>B
>�B
?TB
@ZB
?�B
?�B
?�B
?�B
?�B
@&B
@�B
A`B
A�B
BfB
B�B
B�B
BfB
B�B
B�B
C�B
C�B
ClB
C�B
EB
D�B
D�B
D�B
EyB
FB
F�B
FB
FJB
GQB
I(B
H�B
H�B
H�B
H�B
H�B
I]B
I(B
H�B
J/B
K�B
K B
J�B
J�B
K5B
K5B
KiB
KiB
K�B
K�B
KiB
L;B
LB
MAB
M�B
NB
NGB
N|B
NGB
N|B
N�B
N|B
NGB
NGB
M�B
NGB
N�B
O�B
OB
N�B
PB
O�B
O�B
PB
O�B
PSB
Q�B
Q�B
Q�B
R`B
R�B
R`B
R+B
R�B
SfB
S1B
SfB
S1B
S1B
SfB
S1B
SfB
S1B
SfB
S�B
S�B
T�B
T�B
T�B
U>B
U	B
U	B
VDB
V�B
V�B
V�B
VxB
V�B
V�B
W�B
XB
XB
XB
XPB
X�B
YVB
YVB
Y�B
Y�B
Y�B
Y�B
YVB
Y�B
Z(B
Z(B
Z\B
Z\B
Z�B
Z�B
Z�B
Z\B
Z�B
Z�B
Z�B
Z�B
[.B
Z�B
Z�B
[.B
]:B
]B
]oB
]B
]�B
]:B
^B
]�B
^B
]�B
^AB
^B
^AB
^AB
^AB
^�B
_GB
^�B
_{B
`B
`MB
`B
`MB
`B
`B
`�B
`�B
aSB
aSB
a�B
a�B
a�B
a�B
b%B
bYB
bYB
bYB
bYB
b%B
bYB
c_B
c�B
d1B
d1B
d1B
d1B
d�B
d�B
deB
d�B
d�B
f�B
f	B
e�B
f=B
g�B
g�B
hB
g�B
g�B
g�B
hB
hB
hB
hB
hB
hJB
hJB
hJB
hJB
h~B
h~B
h�B
h�B
iB
iB
i�B
i�B
j�B
j�B
k(B
k�B
k�B
k�B
l.B
k�B
k�B
l�B
mhB
m�B
mhB
mhB
mhB
mhB
mhB
mhB
m�B
nB
nB
nB
nB
nnB
n�B
n�B
oB
oB
oB
ouB
ouB
pB
p{B
p{B
qB
qLB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rSB
r�B
r�B
r�B
r�B
r�B
r�B
s$B
s$B
sYB
s$B
s�B
s�B
s�B
s�B
s�B
t*B
s�B
t*B
t_B
t�B
t�B
u1B
u�B
u�B
v�B
v7B
vkB
v�B
v�B
wB
w�B
w�B
w�B
w�B
xB
xCB
xCB
xwB
xwB
xwB
xwB
y~B
z�B
zB
zOB
zB
zOB
zOB
zB
zB
zB
z�B
{�B
{UB
{UB
{UB
{�B
|�B
|�B
|�B
}-B
}bB
}�B
}�B
}�B
~3B
~hB
~3B
~hB
~hB
~hB
~�B
~�B
B
~�B
~�B
�@B
�B
�@B
�B
�@B
�tB
��B
�tB
�tB
�B
�FB
�FB
�zB
��B
�zB
��B
��B
��B
�LB
�LB
��B
��B
��B
��B
�B
�B
��B
�B
�B
�B
��B
��B
�$B
��B
�*B
��B
�0B
�0B
��B
��B
�6B
�6B
��B
��B
��B
�B
�B
�qB
�qB
��B
�B
��B
�CB
�B
�CB
�B
�B
�B
�IB
�}B
�IB
�}B
�B	��B	ӚB	�+B	�1B	�8B	��B	�+B	��B	�B	�1B	��B	ҔB	�fB	ӚB	��B	��B	ӚB	�8B	�+B	юB	��B	��B	��B	��B	�ZB	��B	�8B	��B	��B	�+B	�B	�B	�+B	��B	��B	ԠB	ҔB	�ZB	юB	��B	��B	�B	�SB	ҔB	юB	�SB	�B	�+B	�1B	юB	�SB	��B	��B	�B	��B	юB	��B	�%B	�SB	�B	�ZB	��B	��B	�B	мB	юB	�SB	϶B	�%B	юB	�MB	ЈB	юB	�ZB	ςB	ςB	��B	юB	ςB	��B	ЈB	�ZB	��B	ςB	��B	мB	ЈB	϶B	ςB	�SB	�+B	мB	�B	мB	�+B	��B	мB	�SB	�%B	�`B	��B	��B	мB	��B	�+B	�ZB	�SB	��B	�`B	��B	��B	ЈB	�`B	�`B	ЈB	мB	��B	�`B	�%B	�B	��B	��B	��B	мB	��B	��B	ҔB	�ZB	ЈB	��B	�+B	мB	мB	�+B	юB	ЈB	ЈB	��B	�ZB	϶B	�MB	ЈB	�%B	�B	϶B	϶B	�%B	юB	�SB	϶B	�B	�ZB	�%B	��B	��B	ЈB	��B	мB	϶B	��B	юB	��B	мB	��B	ЈB	��B	юB	мB	϶B	��B	��B	мB	϶B	��B	��B	�`B	мB	��B	�SB	�%B	��B	�ZB	�SB	�SB	�%B	�+B	ЈB	�SB	�SB	�ZB	�%B	�B	�B	мB	юB	�%B	϶B	ЈB	�ZB	��B	�ZB	϶B	�B	��B	��B	мB	϶B	�SB	��B	�ZB	��B	ςB	�B	�ZB	��B	�B	ςB	��B	мB	�%B	��B	��B	϶B	�%B	мB	�MB	�MB	ςB	ЈB	϶B	ςB	ςB	ςB	��B	�%B	϶B	ΰB	��B	мB	�SB	�B	϶B	�B	ΰB	�B	�`B	϶B	�uB	�GB	��B	��B	�B	ͪB	�GB	�B	�GB	��B	��B	��B	�B	��B	�B	�5B	ʗB	�B	�AB	��B	�]B	мB	�fB	�B	�B	�JB	��B	�>B	��B	áB	�HB	��B	�/B	��B	��B	��B	��B	��B	�pB	�cB	�gB	�pB	��B	}�B	��B	w=B	vkB	��B	�sB	yB	��B	�B	�NB	��B	�B	��B	�B	��B	��B	�B	��B	�bB
dB
u�B
NB
XB
deB
_GB
b�B
g�B
rSB
l.B
n:B
u1B
vkB
s$B
x�B
u�B
u1B
w�B
xCB
x�B
xwB
xwB
y~B
|'B
}bB
}bB
|'B
z�B
}�B
}bB
}�B
��B
��B
��B
�OB
��B
��B
��B
��B
�B
�UB
�CB
�}B
��B
��B
�!B
�nB
�-B
��B
�B
�LB
��B
��B
�0B
�3B
�NB
��B
�]B
�HB
��B
B
�2B
��B
�
B
�DB
�;B
�5B
�cB
��B
ʗB
��B
ΰB
�;B
��B
�GB
�uB
�GB
�B
�ZB
ЈB
�B
ςB
мB
��B
��B
�rB
�rB
�JB
��B
��B
�B
�8B
�rB
�PB
�PB
׳B
��B
�B
�B
�JB
��B
عB
؅B
�B
�JB
�xB
��B
�B
�~B
�B
�JB
��B
عB
�DB
׳B
��B
��B
֭B
�VB
��B
��B
�B
��B
�DB
�xB
׳B
�rB
�B
��B
�B
��B
ީB
��B
��B
��B
�$B
��B
�_B�BH�B0�BHB�B�B�B6BB�BUB!9B�B-�B#�B"?B&�B#yB@�B%�B$�B')B'^B%�B-NB+BB+�B*<B+�B8�BC8B4EB28B4B7�B9cB?�B;�B<�B<B:B<AB>NBB2BDsBB2BH�BFBD�BG�BCBD>BClBClBB�B@�B@�B@&B?�B@�BC�B=�B8�B7�B5KB<AB8�B%�B"�B'�B�B�B�B	CB$B
��B
�B
�lB
�oB
�sB
�TB
�KB
�5B
��B
��B
��B
�#B
�B
��B
�B
�B
��B
�B
��B
��B
��B
�B
϶B
�B
�.B
�/B
�iB
�iB
��B
ςB
��B
ԠB
�DB
�B
��B
��B @BB%B%QB%�B$�B#B"?B$�B#B#B#yB#B �B+B%�B5KB;B9�B6�B*<B*�B)B(�B gB%�B�BBB�B�B
}B	�B?B	�BFB �B
�CB
��B
�LB
��B
��B
�B
�B
��B
�B
�oB
�"B
��B
��B
�SB
��B
��B
�IB
�UB
��B
�*B
�B
�:B
��B
��B
�nB
��B
�_B
��B tB
�B
��B
��B
�B
��B
��B
��B
�B
��B
��B
׳B
��B
�AB
�~B
؅B
�.B
ݣB
��B
��B
�:B
ܝB
�4B
�iB
�iB
��B
عB
׳B
׳B
�B
�VB
�B
�/B
��B
��B
��B
�
B
�
B
��B
�B
� B
��B
�jB
��B
��B
��B
��B
��B
�3B
�3B
��B
�OB
�B
��B
�OB
�B
��B
��B
��B
��B
|'B
yB
s�B
s�B
p�B
f�B
qLB
^uB
Y�B
R�B
N|B
H�B
A�B
D�B
MuB
deB
@ZB
3sB
&�B
#�B
 �B
#�B
&B
�B
RB
B
�B
�B
B
OB
B
[B
$B
�B
�B
�B	�hB
�B	��B	�B	�B	�nB	ًB	�DB	�>B	��B	��B	��B	�B	�`B	˞B	��B	��B	�#B	�QB	��B	�&B	��B	��B	��B	�tB	��B	�HB	��B	��B	�nB	�nB	�3B	��B	�aB	�[B	��B	�UB	��B	�wB	��B	�6B	��B	�$B	��B	�wB	~�B	t�B	�B	ueB	{�B	�wB	��B	s�B	�)B	wqB	n�B	k\B	n�B	pFB	mhB	deB	d�B	^�B	t_B	b�B	[�B	aSB	Z(B	]:B	_�B	j!B	u�B	Z�B	S�B	VDB	]B	^�B	VxB	f�B	MAB	R+B	NGB	L;B	K�B	H�B	H�B	I(B	E�B	GB	K B	PSB	PSB	R�B	E�B	G�B	C�B	B�B	BfB	A`B	@�B	A�B	?TB	A`B	B2B	?�B	=�B	:5B	6QB	8�B	5B	:B	l�B	B�B	3�B	H"B	PB	GB	B2B	. B	!�B	&#B	+B	G�B	D
B	C8B	^AB	Q�B	M�B	I�B	E�B	B2B	H�B	H"B	EB	D
B	B�B	A`B	?�B	Q�B	U>B	EDB	F�B	EB	D>B	GQB	H"B	FJB	GQB	I�B	C�B	D
B	A,B	C8B	L�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223305                            20230426223305AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622330520230426223305  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330520230426223305QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330520230426223305QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               