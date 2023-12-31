CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-22T12:00:45Z creation      
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
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230522120045  20230522120045  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�*�?V&�@�*�?V&�11  @�*�l��@�*�l��@,����پ@,����پ�c��y���c��y��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?��?��H@=p�@}p�@�G�@\@�  A   AG�A   A+�A?\)A_\)A�  A�  A�\)A�\)A��A�Q�A��A�  A�\)B  B  B(�B Q�B((�B0(�B8(�B@  BG�
BO�
BX(�B`(�Bg�
Bp  Bx(�B�
B��B�  B��B��B�  B�  B�(�B�  B��B��B��
B�  B�{B�{B�(�B�{B�  B��B��B�{B�{B��B��B��
B�  B��B��B��B��B�  B��B��C  C��C  C
=C

=C  C
=C{C  C��C  C��C  C
=C��C   C"
=C$
=C%��C(  C*
=C,  C-��C0  C2
=C3��C6  C8  C:  C<
=C>  C@  CB
=CD  CE��CG��CI��CK��CN
=CP{CQ��CS�CU��CW��CZ  C\
=C^
=C`{Cb
=Cc��Ce��Cg��Ci�Ck�Cm�HCo��Cr
=Ct  Cu��Cw�Cz  C|
=C~  C��C�  C�C�C���C���C���C���C���C���C���C�  C���C�  C�  C�  C�  C�  C�C�C�  C���C�  C�  C���C���C���C�  C�C�C�  C�  C�C���C�  C�C�  C�  C�
=C�
=C�C�C�  C���C���C���C�  C�C�  C�C�C�  C�  C�  C�C�C�C�C�C�
=C�  C���C���C�  C���C���C���C���C���C���C���C�  C�  C���C�  C�
=C�C���C���C���C�C�  C�  C�  C���C���C�  C�C�  C�C�
=C�C�C���C��C��C���C���C�  C�
=C�
=C�  C�  C�C�C�C���C���C���C���C�  C���C���C�
=C�
=C�  C�  C�C���C��C���C�C�  C�  C���C�  C�
=C�  C���D � DD� D  D}qD  D��D�D�D�qD� D  D}qD�qD� D�qD}qD�qD	� D
  D
}qD
��Dz�D�qD}qD  D� D  D� D�D� D�qD� D�qD}qD�D�D�D� D  D� D  D� D  D� D  D� D  D}qD  D}qD�qD� D�qD}qD�D��D  D}qD��D� D  D� D �D ��D!�D!��D"�D"��D#�D#��D#�qD$� D%  D%� D&  D&� D'  D'� D(  D(��D)�D)}qD)�qD*� D+  D+� D,  D,��D,�qD-}qD-��D.}qD/  D/� D0  D0� D1�D1� D2�D2�D3�D3��D4�D4��D5  D5� D6  D6� D7�D7��D8  D8}qD8�qD9}qD9�qD:� D;�D;�D<�D<��D=  D=� D>  D>� D?�D?��D@  D@� DA�DA� DA�qDB}qDB�qDC� DD  DD��DD�qDEz�DE��DF}qDF�qDG� DH  DH� DI  DI��DJ�DJ}qDK  DK� DL  DL� DL�qDM� DN�DN� DO  DO}qDO�qDP��DQDQ� DR  DR� DS�DS� DT  DT�DU  DU}qDU�qDV� DW  DW}qDW��DX� DX�qDY}qDY�qDZ}qD[  D[��D[�qD\z�D]  D]��D^  D^}qD^�qD_� D`�D`��Da  Da� Db  Db� Dc  Dc� Dd  Dd� De�De��Df  Df}qDg  Dg}qDg��Dh� Di�Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDm�qDn� Do  Do� Dp  Dp�DqDq��Dr  Dr��Ds�Ds}qDt  Dt�Du  Du��Dv�Dv}qDw�Dw�Dx�Dx��DyDy� Dz�Dz��D{  D{}qD{�qD|� D}  D}� D~  D~� D  D� D�qD�>�D�� D�D�HD�@ D�~�D��HD��D�AHD�� D���D���D�@ D�� D��HD�HD�AHD��HD��HD�  D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD��D�B�D�� D��HD��D�AHD�� D�� D�  D�AHD��HD�D��D�AHD�~�D��qD��qD�>�D�� D�� D�HD�AHD�� D���D���D�@ D��HD��HD�HD�@ D��HD��HD�  D�>�D�� D�� D�  D�AHD��HD�D�HD�@ D��HD�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D��HD�  D�=qD�� D��HD�  D�@ D��HD��HD���D�>�D�� D�� D���D�>�D�~�D���D���D�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�AHD�~�D��qD���D�@ D�� D�� D���D�@ D��HD���D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�AHD�� D���D�  D�>�D�|)D��qD���D�@ D��HD�� D�  D�>�D�� D��HD�  D�@ D�~�D��HD�HD�@ D��HD��qD��qD�AHD���D�� D��qD�=qD�~�D���D�  D�AHD���D�� D���D�AHD�� D���D�  D�>�D�~�D��qD�  D�B�D��HD��HD�HD�AHD��HD�D���D�@ D�� D��qD��qD�>�D�~�D�� D�HD�AHD���D�� D��)D�@ D��HD�� D�  D�AHD���D��HD���D�@ D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�~�D��qD�  D�B�D��HD��HD�  D�@ D�~�D��qD���D�AHD��HD�� D���D�AHD���D��HD���D�@ D�� D��HD��D�AHD�~�D�� D�HD�>�D�~�D¾�D���D�AHDÁHDþ�D�  D�@ DĀ D�D��D�@ Dŀ Dž�D���D�AHDƁHD��HD��D�AHDǀ D��HD�  D�=qDȀ D��HD�  D�>�D�~�Dɾ�D�HD�@ D�~�D�� D�HD�B�Dˀ D˾�D�  D�AHD̂�D��HD���D�@ D̀ D;�D��qD�@ D�~�DνqD���D�@ Dς�D�� D���D�@ DЀ D��HD�  D�>�Dр D�� D�HD�AHDҁHD�� D�  D�AHDӀ DӾ�D���D�AHD�~�DԽqD�  D�@ D�}qDվ�D�  D�@ Dր D�� D�HD�@ D�~�D�� D�  D�@ D؀ Dؾ�D�  D�>�D�~�D�� D�  D�@ Dڀ Dھ�D���D�>�D�~�D�� D���D�>�D�~�D�� D�HD�AHD�~�Dݾ�D�  D�>�D�}qD޾�D�  D�@ D߀ D��HD�  D�@ D�~�DྸD���D�>�D�~�DᾸD�  D�AHD�~�D⾸D�HD�AHD� D�� D�HD�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�HD�>�D� D��HD�  D�@ D�~�D辸D�HD�@ D� D龸D�  D�AHD�~�D��HD�HD�@ D�~�D�� D�HD�AHD삏D��HD���D�>�D�~�D���D���D�>�D�~�DD���D�>�D� D��HD�HD�@ D�� D�� D���D�@ D� D��HD�  D�>�D�}qD�� D�HD�@ D� D�D���D�>�D�HD��HD���D�>�D�� D�� D�  D�B�D���D�� D�  D�AHD��HD�D�  D�@ D��HD��HD�HD�AHD�� D���D�HD�(�D�e?#�
?.{?u?�z�?�{?�
=?��@�@�@&ff@5@G�@Tz�@fff@z�H@��@���@�@��R@���@�\)@���@\@˅@�33@�(�@�@��@���A ��AA
�HA\)A33A�Ap�A!G�A%A)��A.�RA3�
A7�A;�A@��AEAI��AMp�AQ�AW�A\(�A`  Adz�Ai��Ao\)As�
AxQ�A}p�A���A�(�A�{A���A��
A�{A�Q�A�33A�{A�  A�=qA��A��A��A�(�A��RA�G�A��
A�A�Q�A�33A�A�  A�=qA��A�  A\A���AǮAʏ\A��AϮA�=qA�p�A�  Aڏ\A���A߮A��HA�p�A�A�\A�p�A�  A�=qA��A�  A��HA��B (�B��B�RB  Bp�B
=Bz�B	��B
�HBz�B{B\)B��B{B�B��B=qB�BG�B�\B  BG�B�HB z�B!B#
=B$z�B&{B'�B(��B*=qB+�B-G�B.�HB0  B1��B3
=B4��B6{B7\)B8��B:�\B;�
B=�B>�\B@(�BA��BB�HBDQ�BE��BG\)BH��BI�BK\)BL��BN�\BO�
BQ�BR�\BTQ�BUBW33BXz�BY�B[�B\��B^=qB_�BaG�Bb�HBd(�Bep�Bf�HBh��Bj{Bk�Bl��BnffBp  Bq��Br�HBtQ�Bu�Bw�Bx��Bz=qB{�B}G�B~�HB�(�B���B��B�Q�B��B�B�ffB��B��B���B�G�B��B��RB�p�B�(�B���B��B�Q�B���B��B�Q�B�
=B��
B��\B�33B��
B�z�B�G�B�{B���B�\)B�{B��HB��B�{B��HB��B�Q�B���B���B�Q�B�
=B�B�Q�B���B��B�z�B�
=B��B�Q�B���B��B�Q�B���B�p�B�(�B��RB�\)B��
B�z�B�33B�B�=qB��HB��B�=qB���B�\)B�{B���B�p�B�  B���B�G�B��B���B��B��B�Q�B���B���B�(�B��RB�33B�B�ffB���B��B�  B�z�B��B��B�(�B���B��B�B�ffB��HB�\)B��B��\B��B��B�(�B¸RB�G�B��Bď\B�
=B�p�B�{Bƣ�B�G�B��
B�Q�B���B�p�B�{Bʣ�B�G�B�B�Q�B��HB�p�B�{BθRB�33BϮB�{BиRB�33B�B�{B�ffBң�B��HB��B�\)BӅBә�BӮBә�BӮBӮBӮB�BӮBӮBә�Bә�BӮBӮB�BӮBә�BӮB�B�B��
B�BӮB�B��
B��B��B��B��B��
B��
B��B�  B�{B�{B�  B�{B�(�B�=qB�Q�B�=qB�=qB�Q�B�ffB�z�Bԏ\Bԣ�Bԣ�Bԣ�B���B��HB�
=B��B�33B�G�B�\)BՅBծB��
B��B�{B�{B�=qB�ffB֣�B���B���B�
=B��B�\)BׅB�B��B�{B�=qB�Q�B؏\BظRB���B�33B�G�B�p�BمBٮB��B�{B�Q�B�z�BڸRB���B���B��B�G�B�\)Bۙ�B��
B�  B�=qB�Q�B܏\Bܣ�B���B���B�33B�p�BݮB��
B�  B�=qB�ffBޏ\B޸RB���B�33B߅B߮B��B�{B�=qB�z�B�RB��HB��B�G�BᙚB��
B�{B�Q�B�z�B���B�
=B�G�B�B�B��B�{B�=qB�z�B�RB���B�33B�\)B噚B��
B�{B�=qB�ffB�\B��B��HB��B�\)B�B�B��B�(�B�ffB�\B���B�
=B�G�B�p�B�B��
B�{B�Q�B�\B�RB���B��B�\)B뙚B��
B�{B�Q�B�\B��HB��B�\)B홚B��
B�{B�Q�B��B��HB�33B�B��
B�(�B�ffB��B��HB�33B�B�B�  B�Q�B�\B��HB�33B�B��B�=qB���B���B�G�B���B��B�Q�B���B���B�G�B���B��B�=qB��\B���B�G�B���B�  B�ffB���B��B��B��
B�=qB��\B���B�\)B��B�  B�Q�B��RB�
=B�p�B�C 
=C 33C ffC ��C C ��C(�C\)C�\C�RC�C�CG�Cz�C�C�HC  C33CffC��CC��C�CQ�Cz�C�C�HC{C=qCp�C��C��C��C33C\)C�\C�RC�C�CG�Cz�C�C�HC
=C33CffC��C��C��C	(�C	Q�C	�C	�C	�HC

=C
33C
ffC
��C
C
��C�CQ�C�C�C�HC{CG�Cz�C�C�HC
=C=qCp�C��C�
C
=C=qCffC��C��C  C(�C\)C��C��C  C33CffC�\CC��C�CG�Cp�C�C��C��C(�C\)C�C�C�HC{C=qCp�C��C��C  C(�CQ�C�C�C�HC
=C=qCffC�\C�RC�HC
=C33CQ�Cz�C�C�
C  C33C\)C�\CC�HC
=C=qCffC�\C�RC�C{CG�Cz�C�C�C{CG�Cp�C��C��C  C33CffC��C��C
=C=qCp�C��C�
C
=C=qCp�C�C�C�C\)C��C�
C
=CQ�C�C�RC�C (�C \)C ��C ��C!
=C!G�C!�C!��C"  C"=qC"z�C"�RC"�C#(�C#\)C#�\C#C$  C$33C$p�C$��C$�C%�C%\)C%��C%��C&  C&=qC&p�C&�C&�C'�C'\)C'��C'�HC({C(Q�C(�C(�RC(��C)33C)p�C)�C)�C*33C*ffC*��C*��C+
=C+G�C+�\C+��C,{C,Q�C,�\C,��C-{C-G�C-�C-��C.  C.G�C.�C.C/
=C/G�C/�\C/��C0
=C0G�C0z�C0�RC0��C1(�C1p�C1�C1��C2=qC2�C2C3  C333C3p�C3�C3��C4=qC4z�C4C5  C5G�C5�C5C6
=C6G�C6�\C6�
C7�C7ffC7�C7�C8(�C8p�C8�RC9
=C9\)C9��C9�HC:(�C:ffC:�C;  C;G�C;��C;�
C<
=C<\)C<��C<��C==qC=�C=C>  C>Q�C>��C>�C?=qC?�C?��C@
=C@\)C@��C@��CAG�CA��CA�HCB33CBp�CBCC{CCffCC�RCD
=CDQ�CD��CD�CE=qCE�\CE�CF=qCF�\CF�HCG�CGp�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114144144441414411414411144444111441111441111144111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  ?��?��H@=p�@}p�@�G�@\@�  A   AG�A   A+�A?\)A_\)A�  A�  A�\)A�\)A��A�Q�A��A�  A�\)B  B  B(�B Q�B((�B0(�B8(�B@  BG�
BO�
BX(�B`(�Bg�
Bp  Bx(�B�
B��B�  B��B��B�  B�  B�(�B�  B��B��B��
B�  B�{B�{B�(�B�{B�  B��B��B�{B�{B��B��B��
B�  B��B��B��B��B�  B��B��C  C��C  C
=C

=C  C
=C{C  C��C  C��C  C
=C��C   C"
=C$
=C%��C(  C*
=C,  C-��C0  C2
=C3��C6  C8  C:  C<
=C>  C@  CB
=CD  CE��CG��CI��CK��CN
=CP{CQ��CS�CU��CW��CZ  C\
=C^
=C`{Cb
=Cc��Ce��Cg��Ci�Ck�Cm�HCo��Cr
=Ct  Cu��Cw�Cz  C|
=C~  C��C�  C�C�C���C���C���C���C���C���C���C�  C���C�  C�  C�  C�  C�  C�C�C�  C���C�  C�  C���C���C���C�  C�C�C�  C�  C�C���C�  C�C�  C�  C�
=C�
=C�C�C�  C���C���C���C�  C�C�  C�C�C�  C�  C�  C�C�C�C�C�C�
=C�  C���C���C�  C���C���C���C���C���C���C���C�  C�  C���C�  C�
=C�C���C���C���C�C�  C�  C�  C���C���C�  C�C�  C�C�
=C�C�C���C��C��C���C���C�  C�
=C�
=C�  C�  C�C�C�C���C���C���C���C�  C���C���C�
=C�
=C�  C�  C�C���C��C���C�C�  C�  C���C�  C�
=C�  C���D � DD� D  D}qD  D��D�D�D�qD� D  D}qD�qD� D�qD}qD�qD	� D
  D
}qD
��Dz�D�qD}qD  D� D  D� D�D� D�qD� D�qD}qD�D�D�D� D  D� D  D� D  D� D  D� D  D}qD  D}qD�qD� D�qD}qD�D��D  D}qD��D� D  D� D �D ��D!�D!��D"�D"��D#�D#��D#�qD$� D%  D%� D&  D&� D'  D'� D(  D(��D)�D)}qD)�qD*� D+  D+� D,  D,��D,�qD-}qD-��D.}qD/  D/� D0  D0� D1�D1� D2�D2�D3�D3��D4�D4��D5  D5� D6  D6� D7�D7��D8  D8}qD8�qD9}qD9�qD:� D;�D;�D<�D<��D=  D=� D>  D>� D?�D?��D@  D@� DA�DA� DA�qDB}qDB�qDC� DD  DD��DD�qDEz�DE��DF}qDF�qDG� DH  DH� DI  DI��DJ�DJ}qDK  DK� DL  DL� DL�qDM� DN�DN� DO  DO}qDO�qDP��DQDQ� DR  DR� DS�DS� DT  DT�DU  DU}qDU�qDV� DW  DW}qDW��DX� DX�qDY}qDY�qDZ}qD[  D[��D[�qD\z�D]  D]��D^  D^}qD^�qD_� D`�D`��Da  Da� Db  Db� Dc  Dc� Dd  Dd� De�De��Df  Df}qDg  Dg}qDg��Dh� Di�Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDm�qDn� Do  Do� Dp  Dp�DqDq��Dr  Dr��Ds�Ds}qDt  Dt�Du  Du��Dv�Dv}qDw�Dw�Dx�Dx��DyDy� Dz�Dz��D{  D{}qD{�qD|� D}  D}� D~  D~� D  D� D�qD�>�D�� D�D�HD�@ D�~�D��HD��D�AHD�� D���D���D�@ D�� D��HD�HD�AHD��HD��HD�  D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD��D�B�D�� D��HD��D�AHD�� D�� D�  D�AHD��HD�D��D�AHD�~�D��qD��qD�>�D�� D�� D�HD�AHD�� D���D���D�@ D��HD��HD�HD�@ D��HD��HD�  D�>�D�� D�� D�  D�AHD��HD�D�HD�@ D��HD�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D��HD�  D�=qD�� D��HD�  D�@ D��HD��HD���D�>�D�� D�� D���D�>�D�~�D���D���D�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�AHD�~�D��qD���D�@ D�� D�� D���D�@ D��HD���D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�AHD�� D���D�  D�>�D�|)D��qD���D�@ D��HD�� D�  D�>�D�� D��HD�  D�@ D�~�D��HD�HD�@ D��HD��qD��qD�AHD���D�� D��qD�=qD�~�D���D�  D�AHD���D�� D���D�AHD�� D���D�  D�>�D�~�D��qD�  D�B�D��HD��HD�HD�AHD��HD�D���D�@ D�� D��qD��qD�>�D�~�D�� D�HD�AHD���D�� D��)D�@ D��HD�� D�  D�AHD���D��HD���D�@ D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�~�D��qD�  D�B�D��HD��HD�  D�@ D�~�D��qD���D�AHD��HD�� D���D�AHD���D��HD���D�@ D�� D��HD��D�AHD�~�D�� D�HD�>�D�~�D¾�D���D�AHDÁHDþ�D�  D�@ DĀ D�D��D�@ Dŀ Dž�D���D�AHDƁHD��HD��D�AHDǀ D��HD�  D�=qDȀ D��HD�  D�>�D�~�Dɾ�D�HD�@ D�~�D�� D�HD�B�Dˀ D˾�D�  D�AHD̂�D��HD���D�@ D̀ D;�D��qD�@ D�~�DνqD���D�@ Dς�D�� D���D�@ DЀ D��HD�  D�>�Dр D�� D�HD�AHDҁHD�� D�  D�AHDӀ DӾ�D���D�AHD�~�DԽqD�  D�@ D�}qDվ�D�  D�@ Dր D�� D�HD�@ D�~�D�� D�  D�@ D؀ Dؾ�D�  D�>�D�~�D�� D�  D�@ Dڀ Dھ�D���D�>�D�~�D�� D���D�>�D�~�D�� D�HD�AHD�~�Dݾ�D�  D�>�D�}qD޾�D�  D�@ D߀ D��HD�  D�@ D�~�DྸD���D�>�D�~�DᾸD�  D�AHD�~�D⾸D�HD�AHD� D�� D�HD�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�HD�>�D� D��HD�  D�@ D�~�D辸D�HD�@ D� D龸D�  D�AHD�~�D��HD�HD�@ D�~�D�� D�HD�AHD삏D��HD���D�>�D�~�D���D���D�>�D�~�DD���D�>�D� D��HD�HD�@ D�� D�� D���D�@ D� D��HD�  D�>�D�}qD�� D�HD�@ D� D�D���D�>�D�HD��HD���D�>�D�� D�� D�  D�B�D���D�� D�  D�AHD��HD�D�  D�@ D��HD��HD�HD�AHD�� D���D�HD�(�D�e?#�
?.{?u?�z�?�{?�
=?��@�@�@&ff@5@G�@Tz�@fff@z�H@��@���@�@��R@���@�\)@���@\@˅@�33@�(�@�@��@���A ��AA
�HA\)A33A�Ap�A!G�A%A)��A.�RA3�
A7�A;�A@��AEAI��AMp�AQ�AW�A\(�A`  Adz�Ai��Ao\)As�
AxQ�A}p�A���A�(�A�{A���A��
A�{A�Q�A�33A�{A�  A�=qA��A��A��A�(�A��RA�G�A��
A�A�Q�A�33A�A�  A�=qA��A�  A\A���AǮAʏ\A��AϮA�=qA�p�A�  Aڏ\A���A߮A��HA�p�A�A�\A�p�A�  A�=qA��A�  A��HA��B (�B��B�RB  Bp�B
=Bz�B	��B
�HBz�B{B\)B��B{B�B��B=qB�BG�B�\B  BG�B�HB z�B!B#
=B$z�B&{B'�B(��B*=qB+�B-G�B.�HB0  B1��B3
=B4��B6{B7\)B8��B:�\B;�
B=�B>�\B@(�BA��BB�HBDQ�BE��BG\)BH��BI�BK\)BL��BN�\BO�
BQ�BR�\BTQ�BUBW33BXz�BY�B[�B\��B^=qB_�BaG�Bb�HBd(�Bep�Bf�HBh��Bj{Bk�Bl��BnffBp  Bq��Br�HBtQ�Bu�Bw�Bx��Bz=qB{�B}G�B~�HB�(�B���B��B�Q�B��B�B�ffB��B��B���B�G�B��B��RB�p�B�(�B���B��B�Q�B���B��B�Q�B�
=B��
B��\B�33B��
B�z�B�G�B�{B���B�\)B�{B��HB��B�{B��HB��B�Q�B���B���B�Q�B�
=B�B�Q�B���B��B�z�B�
=B��B�Q�B���B��B�Q�B���B�p�B�(�B��RB�\)B��
B�z�B�33B�B�=qB��HB��B�=qB���B�\)B�{B���B�p�B�  B���B�G�B��B���B��B��B�Q�B���B���B�(�B��RB�33B�B�ffB���B��B�  B�z�B��B��B�(�B���B��B�B�ffB��HB�\)B��B��\B��B��B�(�B¸RB�G�B��Bď\B�
=B�p�B�{Bƣ�B�G�B��
B�Q�B���B�p�B�{Bʣ�B�G�B�B�Q�B��HB�p�B�{BθRB�33BϮB�{BиRB�33B�B�{B�ffBң�B��HB��B�\)BӅBә�BӮBә�BӮBӮBӮB�BӮBӮBә�Bә�BӮBӮB�BӮBә�BӮB�B�B��
B�BӮB�B��
B��B��B��B��B��
B��
B��B�  B�{B�{B�  B�{B�(�B�=qB�Q�B�=qB�=qB�Q�B�ffB�z�Bԏ\Bԣ�Bԣ�Bԣ�B���B��HB�
=B��B�33B�G�B�\)BՅBծB��
B��B�{B�{B�=qB�ffB֣�B���B���B�
=B��B�\)BׅB�B��B�{B�=qB�Q�B؏\BظRB���B�33B�G�B�p�BمBٮB��B�{B�Q�B�z�BڸRB���B���B��B�G�B�\)Bۙ�B��
B�  B�=qB�Q�B܏\Bܣ�B���B���B�33B�p�BݮB��
B�  B�=qB�ffBޏ\B޸RB���B�33B߅B߮B��B�{B�=qB�z�B�RB��HB��B�G�BᙚB��
B�{B�Q�B�z�B���B�
=B�G�B�B�B��B�{B�=qB�z�B�RB���B�33B�\)B噚B��
B�{B�=qB�ffB�\B��B��HB��B�\)B�B�B��B�(�B�ffB�\B���B�
=B�G�B�p�B�B��
B�{B�Q�B�\B�RB���B��B�\)B뙚B��
B�{B�Q�B�\B��HB��B�\)B홚B��
B�{B�Q�B��B��HB�33B�B��
B�(�B�ffB��B��HB�33B�B�B�  B�Q�B�\B��HB�33B�B��B�=qB���B���B�G�B���B��B�Q�B���B���B�G�B���B��B�=qB��\B���B�G�B���B�  B�ffB���B��B��B��
B�=qB��\B���B�\)B��B�  B�Q�B��RB�
=B�p�B�C 
=C 33C ffC ��C C ��C(�C\)C�\C�RC�C�CG�Cz�C�C�HC  C33CffC��CC��C�CQ�Cz�C�C�HC{C=qCp�C��C��C��C33C\)C�\C�RC�C�CG�Cz�C�C�HC
=C33CffC��C��C��C	(�C	Q�C	�C	�C	�HC

=C
33C
ffC
��C
C
��C�CQ�C�C�C�HC{CG�Cz�C�C�HC
=C=qCp�C��C�
C
=C=qCffC��C��C  C(�C\)C��C��C  C33CffC�\CC��C�CG�Cp�C�C��C��C(�C\)C�C�C�HC{C=qCp�C��C��C  C(�CQ�C�C�C�HC
=C=qCffC�\C�RC�HC
=C33CQ�Cz�C�C�
C  C33C\)C�\CC�HC
=C=qCffC�\C�RC�C{CG�Cz�C�C�C{CG�Cp�C��C��C  C33CffC��C��C
=C=qCp�C��C�
C
=C=qCp�C�C�C�C\)C��C�
C
=CQ�C�C�RC�C (�C \)C ��C ��C!
=C!G�C!�C!��C"  C"=qC"z�C"�RC"�C#(�C#\)C#�\C#C$  C$33C$p�C$��C$�C%�C%\)C%��C%��C&  C&=qC&p�C&�C&�C'�C'\)C'��C'�HC({C(Q�C(�C(�RC(��C)33C)p�C)�C)�C*33C*ffC*��C*��C+
=C+G�C+�\C+��C,{C,Q�C,�\C,��C-{C-G�C-�C-��C.  C.G�C.�C.C/
=C/G�C/�\C/��C0
=C0G�C0z�C0�RC0��C1(�C1p�C1�C1��C2=qC2�C2C3  C333C3p�C3�C3��C4=qC4z�C4C5  C5G�C5�C5C6
=C6G�C6�\C6�
C7�C7ffC7�C7�C8(�C8p�C8�RC9
=C9\)C9��C9�HC:(�C:ffC:�C;  C;G�C;��C;�
C<
=C<\)C<��C<��C==qC=�C=C>  C>Q�C>��C>�C?=qC?�C?��C@
=C@\)C@��C@��CAG�CA��CA�HCB33CBp�CBCC{CCffCC�RCD
=CDQ�CD��CD�CE=qCE�\CE�CF=qCF�\CF�HCG�CGp�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114144144441414411414411144444111441111441111144111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A���A���A��A��#A���A��A��
A���A��#A��;A��
A��;A��HA��TA��TA��TA��mA��yA��yA��yA��`A��mA��mA��mA��mA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A���A��A��A��A��mA��mA���A�A���A͓uA̓A�K�A�^5A��A���A���A��A��jA���A�%A�;dA��yA�
=A��A�
=A�=qA���A�-A��A�33A�5?A�  A�^5A�~�A��A��A���A��hA�33A��TA��AwC�Aqt�Aj�!AfM�Ac�#Ab��Aa33A^��A\��AZ�yAY�-AX��AVQ�AR��APbNAM��AJ(�AFJAC�^ABv�AA�PA@��A@M�A??}A=�A<��A9��A8��A7�A6ĜA5��A4��A3?}A29XA1��A1��A1?}A1"�A0�A01'A/;dA.�yA. �A-�;A-�^A-�PA-+A,��A,5?A+�A++A*^5A)�FA)33A(�`A(�uA'�TA't�A'l�A'"�A&^5A&�A%��A$�A$�A$ȴA$�!A$�+A#&�A"�DA"9XA!A!�hA!+A �A n�A jA bNA M�A�-An�AQ�AA�A�AbA�TAp�A��A%A"�AbNA�AG�A��AZAjAr�A�A�A^5A��AȴAz�AbNAI�AVAI�AA�7A�/Av�AQ�A��AA�#A�7AS�AQ�Ap�A�HAȴA�A=qAbA��AhsA
=A�RAI�AJA�wA�Al�A"�A
�!A
z�A
-A	�A	��A	�7A	p�A	+A�`A��AVA$�A��A;dA��A��A�uA�`A��A�Ar�AQ�A��A��AdZA/A��A��AVA�A��A��A�PAC�A�AoA�A��A=qA��AC�A �A �A @��w@��y@�ff@��T@��@��`@�r�@��m@�l�@��@�v�@�M�@��@�%@�I�@��w@�t�@�C�@���@��@��h@��@��u@� �@���@�l�@�n�@���@� �@�t�@�~�@�J@��-@�7L@�@� �@�\)@�-@蛦@�A�@� �@��@��@�@�;d@�R@�5?@��@�O�@���@�l�@���@�+@�x�@�z�@�K�@���@�J@�%@�9X@ە�@�C�@�$�@�V@�Z@�"�@֏\@�M�@պ^@��@��/@��;@�;d@��@�V@�J@���@Ѻ^@ѡ�@�?}@϶F@�@�v�@�@���@�?}@�Q�@ˍP@���@ʟ�@ɑh@��`@ȓu@��
@��@ź^@�x�@�p�@�G�@�V@�Ĝ@�ƨ@�ȴ@+@�v�@�-@��@��@�E�@�^5@���@�`B@�Ĝ@���@��@� �@��@��w@�l�@��y@��h@��`@�Ĝ@��@��D@�bN@��@�@�~�@�=q@��#@��`@�I�@��
@�"�@���@���@�v�@�n�@�J@���@���@��@�V@��D@��m@��F@�C�@�@��!@�-@��^@�7L@��`@��D@�(�@��
@�dZ@�;d@�
=@��!@�{@�@��h@�7L@���@��`@�1'@�t�@�;d@���@��R@�ff@��T@���@��7@�G�@���@���@��@�A�@��@�l�@�dZ@�\)@�C�@�33@�@��R@�~�@�=q@�@��#@��7@�G�@��@�9X@�b@��m@��F@���@��@�dZ@�C�@�33@�+@�o@�^5@�$�@��@��^@��h@�x�@�p�@�X@�/@���@��`@�Ĝ@�z�@�Q�@�1'@���@��m@��F@�dZ@��@�~�@�@���@�7L@���@��/@��9@��D@��;@���@�|�@�\)@�+@���@��H@��!@�5?@��^@��7@�7L@��@�%@��`@���@��9@���@�j@�I�@�1'@�1@��;@��P@�K�@�33@���@�=q@���@��@�G�@���@�r�@��@�l�@�;d@�@���@��+@�v�@�^5@�=q@�$�@�{@�@��@��#@��^@�x�@��@��D@�Q�@�A�@� �@��
@���@�|�@�33@��y@��R@�V@��#@�@��-@���@���@�O�@�?}@��@�%@���@�j@�(�@� �@��m@���@�
=@��+@�M�@���@���@�7L@��/@��j@�Q�@���@��@�S�@�+@��@�^5@�J@��^@��@�hs@�hs@��@��`@��9@���@��D@�bN@�(�@�1@��@�ƨ@�dZ@�o@��R@�v�@�^5@�V@�-@���@�@�X@��@��/@���@�I�@��@��@�@~ff@}@}�h@|�@{��@{�F@{�@{S�@{C�@z�H@z-@y��@yG�@x�@w�;@wl�@w\)@v��@v��@vff@v@u�h@u`B@t�D@s�
@st�@s@r�@q��@q�^@q��@q��@q�7@qX@q�@p��@pĜ@p�u@pr�@p �@o�;@o+@nȴ@nE�@m�T@m�@m?}@m�@l�/@lZ@l�@k�m@k��@jn�@i��@h��@h��@hĜ@h��@h�@hb@gK�@fȴ@f��@f$�@e��@d��@c��@c33@b��@b~�@bJ@a��@aG�@`�`@`r�@_�;@_��@_�@^�R@^E�@]��@]��@]�@]O�@\�/@\(�@[S�@Z��@Yx�@Y&�@Y�@Y%@Xr�@W��@W�@Wl�@V�y@Vȴ@V�+@VV@VE�@VE�@V5?@V5?@U�@U��@U`B@UV@T�@Tj@T9X@T(�@S�m@SC�@S"�@R��@Rn�@Q��@Q�#@Q��@Qhs@QX@QX@QG�@Q�@PĜ@PA�@O��@Ol�@O;d@N��@N�+@N@M@M@M��@Mp�@MO�@M?}@MV@L�D@L9X@L1@K�@K"�@J^5@I��@I��@I��@I7L@H�u@HA�@G�;@G��@F�y@F��@FV@F@E�-@D�@D1@C��@C�@CS�@C33@B=q@A��@Ahs@A�@@�9@?�w@?�@>�y@>ȴ@>�+@>$�@=�h@<�j@<�@<�@<�D@<j@<1@;��@;t�@;S�@;@:=q@9�^@9�@8�@8b@7�w@6�y@6V@6$�@5��@4��@41@3dZ@2�@2M�@2J@1��@1�7@1G�@0�`@0�9@0�@0Q�@0b@/�@/�;@/�w@/��@/|�@/+@.�+@.E�@-�@-@-��@-�@-O�@,��@,�j@,��@,I�@+�
@+S�@+33@+@*�!@*�\@*^5@)��@)��@)��@)X@)&�@(�u@(Q�@(b@'�@'|�@&�+@&@%�@%��@%@%O�@$��@$�D@$9X@$1@#ƨ@#�@#C�@#"�@"�@"�!@"n�@"J@!��@ ��@ ��@ ��@ Ĝ@ �9@ r�@ A�@�;@��@;d@��@��@v�@V@@@`B@V@V@�@�@�D@�D@z�@j@Z@Z@9X@1@�
@ƨ@��@�@dZ@S�@o@�!@�\@~�@-@��@�#@��@��@hs@G�@��@1'@ �@ �@ �@ �@ �@b@  @�@��@|�@;d@+@�@��@�@�R@��@ff@E�@�@�-@`B@?}@V@�@��@�j@�@�D@j@I�@�@��@��@33@"�@o@@�@~�@^5@^5@^5@M�@�#@�7@X@G�@G�@G�@G�@7L@&�@%@�9@�u@bN@Q�@ �@�@�@�P@l�@+@��@�@��@E�@�@�T@��@�-@�-@��@p�@O�@O�A�ƨA�ƨA�A���A�ƨA���A���A���A���A���A���A�ȴA���A���A��A��A��
A��#A��#A��A��
A��A��/A��/A���A���A���A��A���A���A��;A��;A���A���A���A��
A���A���A��
A��HA��HA��#A��/A��HA��TA��/A��#A���A���A���A���A��/A��;A��/A��#A��/A��HA��;A��/A��/A��HA��HA��/A��HA��TA��HA��;A��HA��TA��HA��;A��TA��mA��mA��`A��HA��TA��`A��TA��;A��HA��`A��`A��TA��HA��yA��yA��`A��`A��mA��A��mA��`A��mA��yA��A��mA��mA��A��yA��mA��`A��yA��A��mA��mA��A��yA��`A��TA��mA��yA��mA��`A��`A��mA��yA��mA��`A��yA��yA��`A��`A��yA��yA��`A��`A��mA��yA��yA��`A��`A��yA��yA��mA��mA��mA��yA��A��mA��mA��yA��A��A��yA��A��A��A��A��A��A��A��A��yA��mA��A��A��yA��`A��yA��A��A��mA��yA��A��A��A��A��A���A���A��A��A��A���A���A��A��A��A���A��A��A��A��A��A��A��A��A���A��A��A��A��A���A��A��A��A��A���A���A��A��A��A���A��A��A��A��A���A��A��A��A���A���A��A��A���A���A���A��A��A���A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��A��yA��`A��TA��`A��yA��yA��`A��TA��`A��TA��#A���A���A���A�ȴA�ƨA�ƨA�ȴA�ȴA�ƨA���A͸RAͺ^A�A��
A���A���A���A�Aͧ�A͑hA͍PA͏\A͍PA͍PA͋DA͍PA͉7A͇+AͅA�z�A�t�A�t�A�t�A�jA�jA�XA�Q�A�A�A�  Ȁ\A�E�A���A�~�A�dZAȴ9A�\)A�5?A� �A�{A�A��
AǏ\A�`BAƟ�A�ZA��A��HAş�A�`BA�$�A��;A�jA�C�A�{A���A�Q�A�K�A�z�A��HA���A���A���A�|�A�5?A���A��A���A�\)A�9XA�&�A��A�1A��yA���A�A��wA��!A��A���A��A��A���A���A���A��hA��DA��DA��7A��hA��A�z�A�x�A�x�A��A��A��+A��+A��+A��+A��7A��PA��\A���A���A���A���A���A���A��!A��-A��RA��jA�ĜA���A���A���A���A���A���A���A�ƨA���A���A���A�ȴA�A��wA��^A��9A��A���A���A��7A�|�A�l�A�C�A�9XA� �A�oA�{A�%A��A���A��9A���A�~�A�bNA�1'A���A��HA���A�oA�VA�1A���A���A�ffA��A���A���A��-A��A�E�A�&�A�  A��A��A��jA���A��7A�v�A�^5A�1'A��A�%A��A��A�ƨA�A��yA��A���A�ZA�1A���A�~�A�ZA��A�1A�JA�bA��!A��A�^5A��A���A�=qA���A�(�A��#A��PA�XA�C�A�oA�z�A�I�A�VA�XA�ZA�ZA�\)A�^5A�bNA�dZA�dZA�bNA�^5A�ZA�S�A�S�A�VA�C�A�/A�+A�(�A�VA��A��A��A��HA��#A���A�ĜA���A�jA��A�Q�A���A���A�x�A�=qA�(�A��A��-A�p�A�\)A�I�A�=qA�5?A�-A�&�A��A�1A��yA��\A�VA��PA��A��A��A�O�A�&�A���A���A�ƨA��^A��9A���A���A��+A�jA�jA�ffA�XA�;dA���A��A���A��+A�bNA�-A��A��A�  A��A��HA��#A��#A���A��-A���A��A�n�A�^5A�O�A�E�A�-A�oA��A���A��wA��9A���A���A���A���A��A�z�A�z�A�v�A�p�A�M�A��A�1A��A��A��A��yA�
=A�K�A��\A���A���A���A��A�t�A�bNA�Q�A�K�A�M�A�5?A�bA��;A��uA�$�A�ffA��A�-A��mA���A���A�M�A� �A�bA���A�ȴA��A��A�M�A�I�A�G�A�7LA�7LA�5?A�1'A�+A�$�A� �A�{A�A��/A���A��DA�=qA���A�XA�G�A�7LA���A�"�A��A��A�A���A�n�A���A��A���A��HA��A�XA�-A��HA��+A�+A�JA��/A�A��A��uA�hsA�/A�ƨA�v�A�;dA�A���A�t�A�5?A�&�A�oA��;A���A��PA�r�A�bNA�33A��A���A�E�A�  A�33A�?}A��A�n�A��A��FA��A�`BA�$�A��A�p�A��A��\A�E�A�oA��/A���A�|�A�9XA�r�A�1A�%A�/A�t�A�$�A���A�l�A�C�A�JA���A�p�A�G�AG�A|(�Azz�Ax��Aw�TAv�\AvAu�Aut�AuoAt�yAt�DAs��AsXArĜArQ�Aq��Aq�Ap�+An��AnbAmt�Alr�Ak��Akl�Ak/Ajn�Ai�Ah�jAh�Ag�Ag|�AgdZAg�Af1'AfbAe��Ae/Ad�/Ad�Adn�Ad�Ac�AcAc��Ac�PAc�7Act�AcS�Ac;dAc�Ab�/Ab��Ab�\AbjAbA�Ab-Ab �Ab{AbAa��Aa?}A`�A`ZA`{A_�A_��A_t�A_;dA_%A^ȴA^��A^bNA]�TA]�FA]�7A]C�A]"�A]oA\��A\M�A[�
A[l�A[`BA[33A[VAZ��AZ�`AZȴAZ�9AZ�+AZn�AZ1'AZAY�
AY��AYhsAYC�AY&�AY�AX��AX�/AXĜAX��AX�DAXn�AXA�AX-AW��AW�;AWAWXAVJAT�!AT�AS��AShsAS33AR��AR��ARz�AR{AQ�#AQ��AQ|�AQXAP��AP�AO��AOhsAO;dAN�`ANĜAN��ANVAM�AMK�AL�AK�;AK�AJ��AJVAJ �AI�#AI�AH �AG33AFȴAFffAE��AE�AD��AD�9AD^5AD{AC�;AC��ACx�AC?}AB��AB��AB��AB��ABv�ABA�AB-AB  AA�TAA�
AAAA��AA/AA�A@�A@��A@��A@�A@��A@�jA@�A@�uA@r�A@^5A@=qA@�A?�;A?A?��A?;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  A�ƨA���A���A���A��A��#A���A��A��
A���A��#A��;A��
A��;A��HA��TA��TA��TA��mA��yA��yA��yA��`A��mA��mA��mA��mA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A���A��A��A��A��mA��mA���A�A���A͓uA̓A�K�A�^5A��A���A���A��A��jA���A�%A�;dA��yA�
=A��A�
=A�=qA���A�-A��A�33A�5?A�  A�^5A�~�A��A��A���A��hA�33A��TA��AwC�Aqt�Aj�!AfM�Ac�#Ab��Aa33A^��A\��AZ�yAY�-AX��AVQ�AR��APbNAM��AJ(�AFJAC�^ABv�AA�PA@��A@M�A??}A=�A<��A9��A8��A7�A6ĜA5��A4��A3?}A29XA1��A1��A1?}A1"�A0�A01'A/;dA.�yA. �A-�;A-�^A-�PA-+A,��A,5?A+�A++A*^5A)�FA)33A(�`A(�uA'�TA't�A'l�A'"�A&^5A&�A%��A$�A$�A$ȴA$�!A$�+A#&�A"�DA"9XA!A!�hA!+A �A n�A jA bNA M�A�-An�AQ�AA�A�AbA�TAp�A��A%A"�AbNA�AG�A��AZAjAr�A�A�A^5A��AȴAz�AbNAI�AVAI�AA�7A�/Av�AQ�A��AA�#A�7AS�AQ�Ap�A�HAȴA�A=qAbA��AhsA
=A�RAI�AJA�wA�Al�A"�A
�!A
z�A
-A	�A	��A	�7A	p�A	+A�`A��AVA$�A��A;dA��A��A�uA�`A��A�Ar�AQ�A��A��AdZA/A��A��AVA�A��A��A�PAC�A�AoA�A��A=qA��AC�A �A �A @��w@��y@�ff@��T@��@��`@�r�@��m@�l�@��@�v�@�M�@��@�%@�I�@��w@�t�@�C�@���@��@��h@��@��u@� �@���@�l�@�n�@���@� �@�t�@�~�@�J@��-@�7L@�@� �@�\)@�-@蛦@�A�@� �@��@��@�@�;d@�R@�5?@��@�O�@���@�l�@���@�+@�x�@�z�@�K�@���@�J@�%@�9X@ە�@�C�@�$�@�V@�Z@�"�@֏\@�M�@պ^@��@��/@��;@�;d@��@�V@�J@���@Ѻ^@ѡ�@�?}@϶F@�@�v�@�@���@�?}@�Q�@ˍP@���@ʟ�@ɑh@��`@ȓu@��
@��@ź^@�x�@�p�@�G�@�V@�Ĝ@�ƨ@�ȴ@+@�v�@�-@��@��@�E�@�^5@���@�`B@�Ĝ@���@��@� �@��@��w@�l�@��y@��h@��`@�Ĝ@��@��D@�bN@��@�@�~�@�=q@��#@��`@�I�@��
@�"�@���@���@�v�@�n�@�J@���@���@��@�V@��D@��m@��F@�C�@�@��!@�-@��^@�7L@��`@��D@�(�@��
@�dZ@�;d@�
=@��!@�{@�@��h@�7L@���@��`@�1'@�t�@�;d@���@��R@�ff@��T@���@��7@�G�@���@���@��@�A�@��@�l�@�dZ@�\)@�C�@�33@�@��R@�~�@�=q@�@��#@��7@�G�@��@�9X@�b@��m@��F@���@��@�dZ@�C�@�33@�+@�o@�^5@�$�@��@��^@��h@�x�@�p�@�X@�/@���@��`@�Ĝ@�z�@�Q�@�1'@���@��m@��F@�dZ@��@�~�@�@���@�7L@���@��/@��9@��D@��;@���@�|�@�\)@�+@���@��H@��!@�5?@��^@��7@�7L@��@�%@��`@���@��9@���@�j@�I�@�1'@�1@��;@��P@�K�@�33@���@�=q@���@��@�G�@���@�r�@��@�l�@�;d@�@���@��+@�v�@�^5@�=q@�$�@�{@�@��@��#@��^@�x�@��@��D@�Q�@�A�@� �@��
@���@�|�@�33@��y@��R@�V@��#@�@��-@���@���@�O�@�?}@��@�%@���@�j@�(�@� �@��m@���@�
=@��+@�M�@���@���@�7L@��/@��j@�Q�@���@��@�S�@�+@��@�^5@�J@��^@��@�hs@�hs@��@��`@��9@���@��D@�bN@�(�@�1@��@�ƨ@�dZ@�o@��R@�v�@�^5@�V@�-@���@�@�X@��@��/@���@�I�@��@��@�@~ff@}@}�h@|�@{��@{�F@{�@{S�@{C�@z�H@z-@y��@yG�@x�@w�;@wl�@w\)@v��@v��@vff@v@u�h@u`B@t�D@s�
@st�@s@r�@q��@q�^@q��@q��@q�7@qX@q�@p��@pĜ@p�u@pr�@p �@o�;@o+@nȴ@nE�@m�T@m�@m?}@m�@l�/@lZ@l�@k�m@k��@jn�@i��@h��@h��@hĜ@h��@h�@hb@gK�@fȴ@f��@f$�@e��@d��@c��@c33@b��@b~�@bJ@a��@aG�@`�`@`r�@_�;@_��@_�@^�R@^E�@]��@]��@]�@]O�@\�/@\(�@[S�@Z��@Yx�@Y&�@Y�@Y%@Xr�@W��@W�@Wl�@V�y@Vȴ@V�+@VV@VE�@VE�@V5?@V5?@U�@U��@U`B@UV@T�@Tj@T9X@T(�@S�m@SC�@S"�@R��@Rn�@Q��@Q�#@Q��@Qhs@QX@QX@QG�@Q�@PĜ@PA�@O��@Ol�@O;d@N��@N�+@N@M@M@M��@Mp�@MO�@M?}@MV@L�D@L9X@L1@K�@K"�@J^5@I��@I��@I��@I7L@H�u@HA�@G�;@G��@F�y@F��@FV@F@E�-@D�@D1@C��@C�@CS�@C33@B=q@A��@Ahs@A�@@�9@?�w@?�@>�y@>ȴ@>�+@>$�@=�h@<�j@<�@<�@<�D@<j@<1@;��@;t�@;S�@;@:=q@9�^@9�@8�@8b@7�w@6�y@6V@6$�@5��@4��@41@3dZ@2�@2M�@2J@1��@1�7@1G�@0�`@0�9@0�@0Q�@0b@/�@/�;@/�w@/��@/|�@/+@.�+@.E�@-�@-@-��@-�@-O�@,��@,�j@,��@,I�@+�
@+S�@+33@+@*�!@*�\@*^5@)��@)��@)��@)X@)&�@(�u@(Q�@(b@'�@'|�@&�+@&@%�@%��@%@%O�@$��@$�D@$9X@$1@#ƨ@#�@#C�@#"�@"�@"�!@"n�@"J@!��@ ��@ ��@ ��@ Ĝ@ �9@ r�@ A�@�;@��@;d@��@��@v�@V@@@`B@V@V@�@�@�D@�D@z�@j@Z@Z@9X@1@�
@ƨ@��@�@dZ@S�@o@�!@�\@~�@-@��@�#@��@��@hs@G�@��@1'@ �@ �@ �@ �@ �@b@  @�@��@|�@;d@+@�@��@�@�R@��@ff@E�@�@�-@`B@?}@V@�@��@�j@�@�D@j@I�@�@��@��@33@"�@o@@�@~�@^5@^5@^5@M�@�#@�7@X@G�@G�@G�@G�@7L@&�@%@�9@�u@bN@Q�@ �@�@�@�P@l�@+@��@�@��@E�@�@�T@��@�-@�-@��@p�@O�@O�A�ƨA�ƨA�A���A�ƨA���A���A���A���A���A���A�ȴA���A���A��A��A��
A��#A��#A��A��
A��A��/A��/A���A���A���A��A���A���A��;A��;A���A���A���A��
A���A���A��
A��HA��HA��#A��/A��HA��TA��/A��#A���A���A���A���A��/A��;A��/A��#A��/A��HA��;A��/A��/A��HA��HA��/A��HA��TA��HA��;A��HA��TA��HA��;A��TA��mA��mA��`A��HA��TA��`A��TA��;A��HA��`A��`A��TA��HA��yA��yA��`A��`A��mA��A��mA��`A��mA��yA��A��mA��mA��A��yA��mA��`A��yA��A��mA��mA��A��yA��`A��TA��mA��yA��mA��`A��`A��mA��yA��mA��`A��yA��yA��`A��`A��yA��yA��`A��`A��mA��yA��yA��`A��`A��yA��yA��mA��mA��mA��yA��A��mA��mA��yA��A��A��yA��A��A��A��A��A��A��A��A��yA��mA��A��A��yA��`A��yA��A��A��mA��yA��A��A��A��A��A���A���A��A��A��A���A���A��A��A��A���A��A��A��A��A��A��A��A��A���A��A��A��A��A���A��A��A��A��A���A���A��A��A��A���A��A��A��A��A���A��A��A��A���A���A��A��A���A���A���A��A��A���A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��A��yA��`A��TA��`A��yA��yA��`A��TA��`A��TA��#A���A���A���A�ȴA�ƨA�ƨA�ȴA�ȴA�ƨA���A͸RAͺ^A�A��
A���A���A���A�Aͧ�A͑hA͍PA͏\A͍PA͍PA͋DA͍PA͉7A͇+AͅA�z�A�t�A�t�A�t�A�jA�jA�XA�Q�A�A�A�  Ȁ\A�E�A���A�~�A�dZAȴ9A�\)A�5?A� �A�{A�A��
AǏ\A�`BAƟ�A�ZA��A��HAş�A�`BA�$�A��;A�jA�C�A�{A���A�Q�A�K�A�z�A��HA���A���A���A�|�A�5?A���A��A���A�\)A�9XA�&�A��A�1A��yA���A�A��wA��!A��A���A��A��A���A���A���A��hA��DA��DA��7A��hA��A�z�A�x�A�x�A��A��A��+A��+A��+A��+A��7A��PA��\A���A���A���A���A���A���A��!A��-A��RA��jA�ĜA���A���A���A���A���A���A���A�ƨA���A���A���A�ȴA�A��wA��^A��9A��A���A���A��7A�|�A�l�A�C�A�9XA� �A�oA�{A�%A��A���A��9A���A�~�A�bNA�1'A���A��HA���A�oA�VA�1A���A���A�ffA��A���A���A��-A��A�E�A�&�A�  A��A��A��jA���A��7A�v�A�^5A�1'A��A�%A��A��A�ƨA�A��yA��A���A�ZA�1A���A�~�A�ZA��A�1A�JA�bA��!A��A�^5A��A���A�=qA���A�(�A��#A��PA�XA�C�A�oA�z�A�I�A�VA�XA�ZA�ZA�\)A�^5A�bNA�dZA�dZA�bNA�^5A�ZA�S�A�S�A�VA�C�A�/A�+A�(�A�VA��A��A��A��HA��#A���A�ĜA���A�jA��A�Q�A���A���A�x�A�=qA�(�A��A��-A�p�A�\)A�I�A�=qA�5?A�-A�&�A��A�1A��yA��\A�VA��PA��A��A��A�O�A�&�A���A���A�ƨA��^A��9A���A���A��+A�jA�jA�ffA�XA�;dA���A��A���A��+A�bNA�-A��A��A�  A��A��HA��#A��#A���A��-A���A��A�n�A�^5A�O�A�E�A�-A�oA��A���A��wA��9A���A���A���A���A��A�z�A�z�A�v�A�p�A�M�A��A�1A��A��A��A��yA�
=A�K�A��\A���A���A���A��A�t�A�bNA�Q�A�K�A�M�A�5?A�bA��;A��uA�$�A�ffA��A�-A��mA���A���A�M�A� �A�bA���A�ȴA��A��A�M�A�I�A�G�A�7LA�7LA�5?A�1'A�+A�$�A� �A�{A�A��/A���A��DA�=qA���A�XA�G�A�7LA���A�"�A��A��A�A���A�n�A���A��A���A��HA��A�XA�-A��HA��+A�+A�JA��/A�A��A��uA�hsA�/A�ƨA�v�A�;dA�A���A�t�A�5?A�&�A�oA��;A���A��PA�r�A�bNA�33A��A���A�E�A�  A�33A�?}A��A�n�A��A��FA��A�`BA�$�A��A�p�A��A��\A�E�A�oA��/A���A�|�A�9XA�r�A�1A�%A�/A�t�A�$�A���A�l�A�C�A�JA���A�p�A�G�AG�A|(�Azz�Ax��Aw�TAv�\AvAu�Aut�AuoAt�yAt�DAs��AsXArĜArQ�Aq��Aq�Ap�+An��AnbAmt�Alr�Ak��Akl�Ak/Ajn�Ai�Ah�jAh�Ag�Ag|�AgdZAg�Af1'AfbAe��Ae/Ad�/Ad�Adn�Ad�Ac�AcAc��Ac�PAc�7Act�AcS�Ac;dAc�Ab�/Ab��Ab�\AbjAbA�Ab-Ab �Ab{AbAa��Aa?}A`�A`ZA`{A_�A_��A_t�A_;dA_%A^ȴA^��A^bNA]�TA]�FA]�7A]C�A]"�A]oA\��A\M�A[�
A[l�A[`BA[33A[VAZ��AZ�`AZȴAZ�9AZ�+AZn�AZ1'AZAY�
AY��AYhsAYC�AY&�AY�AX��AX�/AXĜAX��AX�DAXn�AXA�AX-AW��AW�;AWAWXAVJAT�!AT�AS��AShsAS33AR��AR��ARz�AR{AQ�#AQ��AQ|�AQXAP��AP�AO��AOhsAO;dAN�`ANĜAN��ANVAM�AMK�AL�AK�;AK�AJ��AJVAJ �AI�#AI�AH �AG33AFȴAFffAE��AE�AD��AD�9AD^5AD{AC�;AC��ACx�AC?}AB��AB��AB��AB��ABv�ABA�AB-AB  AA�TAA�
AAAA��AA/AA�A@�A@��A@��A@�A@��A@�jA@�A@�uA@r�A@^5A@=qA@�A?�;A?A?��A?;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�UB	��B	��B	�UB	��B	��B	��B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	�UB	��B	�UB	��B	�UB	��B	��B	��B	�OB	�B	�B	�OB	��B	�!B	�OB	�OB	��B	�[B	��B	��B	�!B	��B	�UB	�!B	�!B	�UB	��B	�}B	��B	��B	�CB	�CB	��B	�B	�=B	��B	��B	��B	ǮB	�B
(B
GB
9�B
j�B
�B
��B
��B
�B
�[B�B�BG�BJ�BS&Bu%B��BW�B0!B�B
�)B
��B
��B
sMB
WsB
�B	��B	��B	�{B	�B	sB	jKB	f�B	e,B	b�B	b�B	a�B	`B	YB	Y�B	I�B	EmB	I�B	A�B	2-B	,=B	7LB	8RB	>B	FB	R�B	YB	Z�B	h�B	�B	��B	��B	�CB	��B	�)B	��B	�yB	��B	�B	��B	��B	�B
 iB
YB
�B
�B
_B
�B
 �B
$B
*eB
/�B
1�B
6zB
8�B
<6B
@B
@�B
B�B
A�B
@�B
D3B
H�B
GEB
MB
N�B
NB
NB
MjB
NB
Q�B
OvB
P�B
P}B
QB
T�B
VmB
X�B
Y�B
Z�B
]�B
e�B
dZB
d�B
e`B
f�B
iB
k�B
iDB
h
B
l�B
t�B
q�B
o�B
p�B
lWB
m]B
q�B
u�B
t�B
t�B
o�B
iB
`BB
a�B
b�B
d�B
f�B
g�B
jKB
i�B
h�B
dZB
c B
bNB
_�B
\)B
YKB
X�B
X�B
W
B
WsB
Z�B
a|B
a|B
_;B
_;B
[�B
YKB
V�B
U2B
R�B
S�B
R B
QNB
R B
P}B
P�B
P}B
P�B
R�B
RTB
RTB
QB
PB
PHB
O�B
N�B
OBB
M�B
NB
OBB
NpB
V9B
U�B
TaB
S�B
S�B
S�B
RTB
R B
Q�B
R�B
Q�B
P�B
P�B
PHB
O�B
PB
N<B
MjB
LdB
K�B
JXB
IRB
C�B
B[B
@�B
@�B
>B
<�B
>B
=qB
<�B
?HB
?}B
?HB
?B
>BB
>BB
<�B
<jB
<�B
=B
:�B
<jB
<jB
<B
=�B
=B
<6B
<6B
;dB
:�B
9XB
9$B
9XB
7�B
5�B
6zB
4�B
49B
3�B
2�B
2�B
1�B
1�B
4B
2-B
1'B
1'B
0�B
0�B
/�B
0UB
/�B
/B
.IB
.�B
-�B
/�B
,=B
+�B
)�B
*�B
'�B
%�B
&B
$B
"�B
!-B
!�B
!�B
B
xB
qB
kB
�B
�B
1B
�B
eB
$B
�B
YB
�B
$B
�B
SB
�B
�B
MB
B
�B
FB
B
�B
FB
B
:B
�B
�B
4B
 B
4B
�B
�B
�B
.B
�B
.B
�B
�B
4B
�B
�B
�B
B
+B
7B
B
�B
~B
�B
�B
�B
�B
OB
B
B
�B
�B
�B
�B
�B
�B
OB
VB
OB
�B
�B
~B
xB
B
�B
�B
!B
OB
OB
�B
�B
�B
 \B
 �B
!�B
!bB
"4B
"hB
"�B
"hB
"�B
"hB
!�B
!-B
 �B
 �B
!-B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"4B
#�B
"�B
"�B
"�B
!�B
"�B
"�B
!�B
!�B
"�B
"�B
"hB
"hB
#nB
#�B
#nB
#nB
#�B
#�B
#�B
$B
$�B
%FB
&LB
&�B
&�B
'RB
&�B
'�B
(XB
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
($B
)_B
(�B
)_B
)_B
)�B
)_B
)_B
)�B
)�B
)_B
)_B
)�B
*eB
*0B
*�B
*0B
*0B
*0B
*0B
*�B
*�B
+kB
+�B
+�B
+�B
+�B
,qB
,qB
-�B
-CB
-CB
-CB
-�B
-�B
-wB
-�B
.B
.}B
.}B
.�B
.�B
.�B
/B
/�B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1'B
0�B
0�B
1'B
0�B
0�B
0�B
0!B
0�B
1'B
1�B
2-B
1�B
1�B
2�B
2�B
2aB
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
3�B
3�B
4�B
5tB
5tB
5�B
5�B
6�B
8�B
9XB
9$B
9$B
9�B
9�B
9�B
:^B
:^B
:^B
:*B
:�B
;0B
;0B
:�B
:�B
:�B
;�B
<jB
<6B
<�B
=B
=qB
=qB
=qB
>B
=<B
=qB
=�B
=qB
=�B
>�B
>�B
?HB
?�B
?}B
?}B
@OB
?�B
@�B
@�B
@OB
@�B
A B
AUB
AUB
A B
B�B
B�B
CaB
C�B
C�B
CaB
C�B
C�B
D3B
D�B
EB
E9B
EB
FB
E�B
F?B
F�B
GB
GB
GEB
HKB
HB
HKB
H�B
HKB
HB
H�B
IRB
IRB
I�B
JXB
J�B
J�B
JXB
K)B
K)B
K)B
K�B
K�B
K^B
L�B
L�B
L�B
MjB
NpB
N<B
N<B
N<B
N<B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
PB
O�B
P}B
PB
P�B
PHB
P}B
P}B
QNB
QB
QB
P�B
R�B
S[B
S[B
S&B
S&B
S&B
R�B
S�B
TaB
TaB
T,B
T�B
T�B
U�B
V�B
VmB
V�B
W
B
WsB
WsB
W�B
XB
XEB
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
ZQB
Z�B
[�B
[#B
[#B
Z�B
\)B
[�B
[�B
\]B
\�B
\�B
\�B
]/B
\�B
\�B
\�B
\�B
]/B
]dB
]dB
]�B
^5B
]�B
^B
]�B
^jB
_;B
^�B
^�B
_�B
_�B
_pB
_�B
`B
_�B
_�B
_�B
_�B
`BB
aB
aB
aHB
aHB
a|B
bB
b�B
bNB
bNB
b�B
b�B
b�B
b�B
b�B
c B
cTB
c B
c�B
c�B
dZB
d�B
dZB
dZB
d�B
e,B
e,B
e,B
e`B
e�B
e,B
e,B
d�B
dZB
f2B
e�B
e�B
e�B
e�B
e`B
gB
f�B
f2B
ffB
ffB
h
B
h
B
h
B
g�B
h
B
hsB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jKB
jKB
jKB
jKB
j�B
j�B
kQB
l"B
k�B
k�B
l�B
l�B
l�B
m)B
m�B
n�B
o5B
oiB
p;B
pB
poB
poB
p�B
p�B
qB
qB
qAB
q�B
qvB
qvB
q�B
q�B
q�B
rB
r�B
r�B
sB
s�B
sMB
s�B
s�B
s�B
tB
tB
t�B
t�B
u�B
uZB
uZB
u�B
u�B
u�B
v+B
v`B
v`B
v�B
v�B
wfB
w�B
w�B
xB
x�B
y�B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
{JB
{JB
{B
{B
{�B
{�B
|B
|B
|PB
|�B
}"B
~(B
}�B
~(B
}�B
~(B
~(B
~]B
~�B
~�B
.B
.B
�B
�B
�B
�4B
��B
�;B
�B
�B
�;B
�oB
�AB
�uB
��B
��B
��B
��B
�GB
�{B
�{B
��B
��B
�B
�B
��B
�MB
��B
��B
��B
�SB
��B
��B
��B
��B
��B
��B
�YB
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
��B
��B
��B
�1B
�fB
�fB
��B
��B
�B
�7B
��B
��B
�	B
�=B
�=B
�	B
�rB
�=B
�rB
��B
��B
��B
�xB
�xB
�DB
�xB
�xB
��B
�JB
�~B
�JB
�B
�~B
��B
�B
�B
�PB
�PB
�PB
�PB
��B
��B
��B
�"B
�VB
�VB
��B
��B
��B
��B
��B
�(B
�\B
��B
��B
�.B
�.B
� B
�4B
�4B
�hB
�hB
��B
�oB
�oB
�B	��B	��B	��B	��B	��B	��B	��B	�[B	��B	�!B	��B	��B	�[B	�OB	�}B	��B	��B	�UB	��B	�[B	��B	��B	��B	�UB	�9B	��B	��B	��B	��B	��B	�OB	�UB	��B	��B	�'B	�UB	�UB	��B	��B	��B	�!B	�[B	��B	��B	�B	�'B	�aB	��B	��B	��B	��B	�IB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�UB	�-B	��B	��B	��B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	�UB	�UB	��B	��B	��B	�!B	��B	�[B	��B	�OB	��B	�'B	�UB	��B	��B	��B	�[B	�!B	��B	�[B	�[B	��B	�OB	�'B	�'B	��B	��B	��B	��B	��B	��B	�'B	��B	�OB	�}B	��B	�UB	�UB	��B	��B	��B	�UB	�B	�IB	��B	��B	�}B	��B	��B	�!B	�OB	��B	�IB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	�!B	�!B	��B	�IB	�}B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�UB	�UB	�B	�IB	�}B	�'B	��B	�}B	��B	�!B	��B	��B	��B	��B	��B	�aB	�[B	�UB	��B	��B	��B	�UB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�IB	��B	�UB	��B	�'B	��B	�UB	��B	�-B	��B	��B	��B	��B	�'B	�!B	��B	��B	�[B	��B	��B	��B	��B	�'B	�UB	��B	��B	�[B	��B	��B	��B	�UB	�[B	�-B	��B	�OB	��B	��B	��B	�wB	�}B	�OB	��B	�IB	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�IB	��B	��B	�B	��B	�IB	�B	�=B	��B	�IB	��B	�B	�qB	��B	�CB	�qB	��B	�qB	�IB	��B	��B	��B	��B	��B	�IB	��B	�kB	�}B	��B	�6B	�kB	��B	��B	�_B	�*B	�6B	��B	�*B	��B	�XB	��B	�B	��B	�4B	�B	��B	��B	��B	�FB	��B	��B	�+B	��B	�dB	��B	��B	�HB	�B	��B	�RB	�&B	�B
�B	��B	�QB	��B	�WB	�MB	�B	�ZB
�B	��B
�B
�B
$�B
5?B
B�B
=�B
6�B
>wB
C�B
I�B
P�B
O�B
R�B
J�B
?�B
;dB
:�B
8�B
7�B
;0B
:�B
7�B
4�B
8B
6zB
8�B
6B
5�B
9�B
7�B
9XB
<6B
=qB
=qB
=<B
:�B
?�B
@�B
@�B
@B
;�B
=�B
<6B
<�B
=<B
=<B
=�B
=qB
=<B
;0B
8�B
:�B
:^B
9�B
8�B
7�B
7�B
5�B
7�B
5tB
4�B
3�B
4�B
7�B
6zB
6FB
7�B
6zB
3�B
49B
5�B
6FB
6zB
5�B
49B
4�B
4�B
5�B
6�B
6�B
4�B
5tB
?HB
=B
@�B
@B
<B
<6B
>�B
A�B
D3B
FB
H�B
N�B
YKB
m�B
r|B
�:B
�B
�|B
�B
�%B
�]B
��B
�DB
��B
�}B
�-B
�mB
� B
��B
��B
��B
��B
��B
�'B
�OB
�B
��B
�eB
�B
��B
�(B
��B
��B
��B
� B
�xB
�!B
�~B
��B
��B
�qB
��B
�&B
�B
��B
��B
�B
�	B
�5B
�[B
ԕB
� B
�jB
�tB
��B
��B
�B
��B
�B
̘B
��B
�dB
��B
�HB
�B
�vB
�BB
� B
�B
��B
�B
�B
�>B
�8B
�
B
�,B
�B
��B
�B
�B
ܒB
�B
��B
�HB
یB
�B
�EB
ݘB
�NB
یB
�GB
уB
��B
��B
ŢB
��B
ĜB
��B
�#B
�&B
��B
�?B
ٴB
یB
��B
ܒB
�5B
�BB
��B
�2B�B�B!-B"4BkBuBSB�B�B BB�B�B�BbBB�B�B.B�B($B)*B.}B>BB[BB'BD�BGzBK�BIBH�BGBJ�BO�BO�BN<BK�BK^BI�BHBFtBHBJXBL0BI�BJ#BM6BN�BP�BQBS�BX�BS�BS�BS[BS[BYKBU2BTaBR�BS�BZ�B]�BY�Bx�B��B��B��B��B�hB��B��B��B��B��B��B��B��B�7B��B��Bx�Bl�BOBK^BC�BHKB<�B9XB8�B9�B6�B8RB2aB,�B-CB-wB*eB)�B)�B)_B'�B%�B%B%�B'�B!�B�B�B \B�BB
��B
��BBB
��B
ںB
��B
бB
��B
��B
�B
��B
��B
�B
�kB
��B
�!B
��B
�B
�B
�VB
��B
�.B
��B
�FB
�oB
�fB
�B
�1B
�{B
�B
x�B
tTB
v`B
z�B
r�B
t�B
o�B
n/B
tB
r�B
l�B
poB
j�B
x8B
��B
h�B
D3B
JXB
E�B
:�B
6FB
5�B
6�B
6�B
5tB
"4B
 'B
SB
+B
�B
�B
{B
�B
�B
 �B
%B	�oB	�KB	�B	�WB	�B	�[B	��B	̘B	˒B	��B	��B	�tB	�LB	�aB	�B	�!B	�CB	�~B	��B	��B	��B	�CB	��B	��B	�B	��B	��B	�nB	�zB	~�B	�bB	��B	��B	�GB	z�B	�B	�;B	��B	w�B	|B	p�B	ncB	x�B	k�B	oiB	tB	rB	oiB	jKB	poB	m)B	l�B	jKB	jB	g�B	gB	g8B	gB	e�B	h>B	gmB	gB	i�B	e�B	e�B	d�B	c�B	bNB	bB	d�B	ncB	iDB	f�B	d�B	_�B	ffB	_pB	c�B	`�B	]�B	\�B	d�B	c�B	_�B	a�B	b�B	_;B	^B	h>B	_�B	lWB	`B	_B	c�B	a|B	bB	aHB	a�B	a�B	bB	`�B	c B	`BB	`vB	`BB	b�B	\�B	_;B	Z�B	\]B	[�B	Y�B	YB	W�B	W�B	W�B	VB	U�B	S&B	RTB	\�B	kB	d&B	S&B	Q�B	JXB	L�B	HKB	HKB	F�B	M�B	E�B	H�B	AUB	B[B	EmB	RTB	B�B	A B	B'B	FtB	=�B	C�B	M�B	H�B	Q�B	T�B	J�B	N<B	A�B	;dB	8�B	6zB	FB	DgB	;�B	2-B	0�B	/�B	0�B	.}B	*eB	,qB	)�B	(�B	*�B	+B	2�B	4B	2�B	2-B	33B	:�B	9�B	9�B	;�B	<�B	4nB	5�B	9XB	8B	7�B	;dB	<�B	:*B	<jB	?B	@�B	C�B	F�B	FB	C�B	DgB	E�B	JXB	H�B	MjB	U�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  B	�B	��B	�pB	�B	�pB	�<B	�BB	�<B	��B	��B	��B	�pB	��B	�pB	�pB	�<B	�B	�<B	�B	�pB	�B	��B	�jB	�5B	�B	��B	��B	�B	�5B	��B	�B	�B	�<B	�B	��B	�<B	��B	�pB	�B	��B	��B	�B	��B	�/B	��B	�^B	��B	��B	��B	��B	��B	��B	�3B	�sB	�`B	�VB
�B
@�B
3sB
d�B
��B
��B
ǅB
��B
�BeBEBA�BDsBL�Bn�B�UBQ�B)�B�B
��B
�sB
��B
l�B
Q%B
jB	ީB	�|B	�-B	|�B	l�B	c�B	`�B	^�B	\iB	\iB	[cB	Y�B	S1B	SfB	ClB	?B	C�B	;;B	+�B	%�B	0�B	2B	7�B	?�B	L;B	R�B	TlB	bYB	{�B	�tB	�6B	��B	��B	��B	ςB	�+B	֭B	�cB	ܝB	ީB	�\B	�B
 B
�B
�B
B
RB
BB
�B
$B
)jB
+�B
0,B
28B
5�B
9�B
:�B
<AB
;;B
:�B
=�B
B�B
@�B
F�B
HWB
G�B
G�B
GB
G�B
KiB
I(B
J�B
J/B
J�B
NGB
PB
R`B
S�B
TlB
W~B
_�B
^B
^uB
_B
`MB
b�B
e7B
b�B
a�B
f=B
n�B
k�B
iPB
j�B
f	B
gB
k�B
o@B
n:B
nnB
iPB
b�B
Y�B
[cB
\�B
^uB
`MB
aSB
c�B
c�B
b�B
^B
\�B
\ B
Y�B
U�B
R�B
R`B
R�B
P�B
Q%B
T8B
[.B
[.B
X�B
X�B
U�B
R�B
PSB
N�B
LoB
M�B
K�B
K B
K�B
J/B
J�B
J/B
J�B
LoB
LB
LB
J�B
I�B
I�B
I�B
HWB
H�B
G�B
G�B
H�B
H"B
O�B
O�B
NB
MuB
MAB
MAB
LB
K�B
KiB
L;B
K5B
JcB
J�B
I�B
I�B
I�B
G�B
GB
FB
E�B
D
B
CB
=HB
<B
:jB
:5B
7�B
6�B
7�B
7#B
6�B
8�B
9/B
8�B
8�B
7�B
7�B
6�B
6B
6QB
6�B
4yB
6B
6B
5�B
7�B
6�B
5�B
5�B
5B
4yB
3
B
2�B
3
B
1gB
/ZB
0,B
.TB
-�B
-�B
,HB
,�B
+vB
+vB
-�B
+�B
*�B
*�B
*�B
*pB
)jB
*B
)�B
(�B
'�B
(dB
'^B
)jB
%�B
%�B
#�B
$KB
!mB
�B
�B
�B
�B
�B
}B
HB
�B
*B
#B
B
LB
zB
�B
?B
B
�B
tB
B
?B
�B
9B
B
�B
tB
�B
�B
3B
�B
�B
�B
�B
�B
�B
�B
�B

�B

�B

�B
OB
	�B
	wB
	�B
	wB
	�B
OB

IB

�B
[B
aB
�B
�B
�B
�B
�B
�B
0B
dB
�B
6B
jB
B
�B
�B
6B
�B
jB
jB
6B
6B
B
B
B
�B
6B
0B
*B
�B
6B
6B
�B
B
B
�B
<B
<B
B
BB
HB
B
�B
B
NB
B
NB
B
}B
�B
wB
wB
�B
�B
wB
wB
�B
�B
}B
}B
�B
�B
�B
UB
�B
�B
NB
�B
�B
NB
HB
�B
NB
�B
B
B
 B
UB
 B
 B
UB
�B
�B
�B
[B
�B
�B
 �B
 �B
!B
 �B
!�B
"
B
!mB
!�B
!�B
!mB
!mB
!�B
!�B
!mB
!9B
!�B
#B
"�B
#B
#B
#EB
#B
#B
#EB
#yB
#B
#B
#yB
$B
#�B
$KB
#�B
#�B
#�B
#�B
$�B
$�B
%B
%QB
%�B
%�B
%�B
&#B
&#B
'^B
&�B
&�B
&�B
'�B
'^B
')B
'�B
'�B
(/B
(/B
(dB
(�B
(dB
(�B
)�B
*B
*pB
*pB
*<B
*<B
*<B
*<B
*�B
*pB
*<B
*�B
*�B
*pB
*pB
)�B
*pB
*�B
+BB
+�B
+�B
+vB
,|B
,HB
,B
,HB
,|B
,|B
,�B
,|B
,|B
,HB
,HB
,|B
-B
-�B
-�B
-�B
.TB
/&B
/&B
/ZB
/�B
0�B
2�B
3
B
2�B
2�B
3>B
3>B
3�B
4B
4B
4B
3�B
4yB
4�B
4�B
4�B
4yB
4yB
5B
6B
5�B
6�B
6�B
7#B
7#B
7#B
7�B
6�B
7#B
7WB
7#B
7WB
8�B
8�B
8�B
9cB
9/B
9/B
:B
9�B
:5B
:5B
:B
:jB
:�B
;B
;B
:�B
<AB
<AB
=B
=HB
=HB
=B
=|B
=|B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B2B
A�B
A�B
BfB
CB
CB
ClB
D
B
DsB
DsB
D
B
D�B
D�B
D�B
EyB
EDB
EB
FJB
FJB
FB
GB
H"B
G�B
G�B
G�B
G�B
G�B
HWB
HWB
HWB
HWB
H�B
HWB
H�B
H�B
I�B
I]B
J/B
I�B
JcB
I�B
J/B
J/B
K B
J�B
J�B
J�B
L�B
MB
MB
L�B
L�B
L�B
L�B
MAB
NB
NB
M�B
NGB
N|B
O�B
P�B
PB
PSB
P�B
Q%B
Q%B
QZB
Q�B
Q�B
R`B
R`B
R�B
R�B
S�B
S�B
S�B
SfB
SfB
SfB
TlB
TB
TlB
U�B
T�B
T�B
T�B
U�B
U�B
U�B
VB
VxB
VDB
V�B
V�B
V�B
VxB
V�B
VxB
V�B
WB
WB
W~B
W�B
W~B
W�B
W~B
XB
X�B
X�B
X�B
YVB
YVB
Y"B
YVB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[.B
[�B
\4B
\ B
\ B
\4B
\iB
\4B
\4B
\�B
\�B
]B
\�B
]:B
]oB
^B
^AB
^B
^B
^uB
^�B
^�B
^�B
_B
_{B
^�B
^�B
^�B
^B
_�B
_�B
_�B
_�B
_{B
_B
`�B
`MB
_�B
`B
`B
a�B
a�B
a�B
a�B
a�B
b%B
c�B
c�B
c�B
c�B
c�B
c_B
d1B
c�B
c�B
c�B
c�B
d�B
d�B
eB
e�B
e�B
e�B
frB
f�B
f=B
f�B
g�B
h~B
h�B
iB
i�B
i�B
j!B
j!B
jVB
j�B
j�B
j�B
j�B
k\B
k(B
k(B
k\B
k\B
k\B
k�B
l�B
l�B
l�B
m4B
l�B
m4B
mhB
m�B
m�B
m�B
n:B
nnB
o@B
oB
oB
ouB
ouB
o�B
o�B
pB
pB
pFB
pFB
qB
qLB
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t_B
t�B
t�B
t�B
u1B
u1B
u�B
ueB
u�B
u�B
vB
vkB
v�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xCB
xCB
x�B
x�B
yIB
yIB
y~B
y�B
zOB
z�B
z�B
z�B
z�B
{!B
{�B
|'B
|\B
|\B
|\B
|�B
|�B
}-B
}-B
}bB
}�B
}�B
}�B
}�B
}�B
~3B
~3B
~3B
B
:B
:B
nB
�B
nB
nB
�B
�@B
�@B
�@B
�@B
�tB
�@B
�@B
�tB
�@B
�tB
�FB
�zB
�zB
��B
��B
��B
�B
�B
��B
��B
��B
��B
�RB
�RB
��B
��B
��B
��B
�$B
��B
�$B
�XB
��B
�XB
�*B
�*B
��B
�*B
�*B
�^B
��B
�0B
��B
��B
�0B
��B
��B
��B
�B
�B
�B
�B
�kB
�6B
�6B
��B
�B
�B
�<B
�<B
�<B
�qB
��B
��B
�B
�CB
�wB
��B
��B
��B
��B
��B
�B
�B
��B
�!B
�!B
��B	��B	��B	�pB	�^B	��B	�pB	�5B	�B	�BB	��B	�pB	�HB	�B	�B	�/B	�pB	�BB	�B	��B	�B	�vB	��B	�jB	�B	��B	��B	��B	�jB	�BB	�vB	�B	�B	��B	�BB	��B	�B	�B	�HB	�jB	��B	��B	�B	�pB	�jB	��B	��B	�B	��B	��B	�BB	�vB	��B	�jB	�pB	�BB	�<B	��B	�jB	�BB	�BB	��B	�B	��B	�pB	�jB	�pB	�vB	�<B	��B	��B	�vB	�pB	��B	��B	�5B	�BB	��B	�B	�B	�vB	��B	�jB	��B	��B	�B	�jB	�B	�pB	��B	�B	�jB	�pB	��B	�B	��B	�jB	�B	�B	�jB	�B	��B	��B	�5B	�5B	�pB	�<B	�5B	��B	��B	�pB	�B	�/B	��B	�B	�B	�dB	�5B	��B	�B	��B	��B	�jB	��B	�/B	��B	�jB	��B	�B	�dB	��B	�jB	�jB	�/B	��B	�5B	�jB	�5B	��B	��B	��B	��B	��B	��B	�/B	�jB	�jB	�dB	��B	�<B	��B	�5B	��B	�jB	��B	�5B	��B	�dB	�B	�B	��B	��B	�/B	��B	�5B	�/B	�dB	��B	�vB	�vB	�<B	��B	�vB	�B	�B	�B	�<B	��B	�BB	�B	��B	��B	�BB	��B	��B	�5B	�BB	��B	�<B	��B	�<B	�B	�pB	��B	�5B	�B	�pB	��B	�pB	��B	��B	�vB	��B	��B	�^B	�5B	�B	�<B	�5B	�dB	�<B	��B	�B	�dB	��B	�B	��B	�5B	��B	�B	�B	��B	�pB	�B	�^B	��B	�dB	�)B	�/B	�B	�5B	��B	��B	��B	��B	�dB	��B	��B	��B	��B	��B	�WB	��B	��B	��B	�^B	�WB	��B	�^B	��B	��B	��B	�^B	��B	��B	��B	�#B	�WB	��B	�#B	�QB	�#B	��B	��B	�QB	��B	��B	��B	��B	�QB	�B	�/B	��B	��B	�B	�KB	�yB	�B	��B	��B	��B	��B	��B	�
B	�?B	��B	�aB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�QB	�B	��B	�;B	��B	��B	�jB	�B	��B	϶B	�3B	�xB	�B	�B	�	B	��B	�B	�B	��B	�eB	��B
�B
�B
.�B
<vB
7WB
0`B
8)B
=HB
C�B
JcB
I]B
LoB
D�B
9cB
5B
4�B
2�B
1�B
4�B
4yB
1�B
.�B
1�B
0,B
28B
/�B
/ZB
3�B
1gB
3
B
5�B
7#B
7#B
6�B
4�B
9cB
:�B
:5B
9�B
5B
7WB
5�B
6QB
6�B
6�B
7�B
7#B
6�B
4�B
2�B
4�B
4B
3>B
2mB
1�B
1gB
/�B
1�B
/&B
.TB
-�B
.�B
12B
0,B
/�B
12B
0,B
-�B
-�B
/ZB
/�B
0,B
/ZB
-�B
.TB
.TB
/ZB
0`B
0`B
.�B
/&B
8�B
6�B
:5B
9�B
5�B
5�B
8�B
;�B
=�B
?�B
B2B
H�B
R�B
gxB
l.B
��B
�VB
�.B
�VB
��B
�B
�B
��B
ԠB
�/B
��B
�B
��B
�^B
��B
��B
��B
��B
��B
�B
��B
�LB
�B
��B
��B
��B
�wB
�EB
�LB
��B
�*B
��B
�0B
�UB
�?B
�#B
�9B
��B
ƳB
؅B
�B
�:B
�B
��B
�B
�GB
��B
�B
�&B
�<B
�vB
��B
��B
��B
�JB
�WB
�B
ًB
��B
�cB
�(B
��B
��B
�:B
�uB
�MB
�B
��B
��B
�B
��B
�YB
�{B
��B
�4B
�DB
�MB
֭B
��B
�>B
عB
��B
�JB
� B
�>B
��B
�5B
�uB
��B
�TB
�vB
�NB
�|B
��B
��B
˞B
��B
�fB
�>B
էB
�DB
��B
��B
��B
��BFBjB�B�BB'BBnBzB
�B�B	wB	CB<B
B�BeB	�B	�B
}B!�B"�B(/B7�B<B;�B>NBA,BEDBB�BBfB@�BD�BI�BI]BG�BE�BEBC8BA�B@&BA�BD
BE�BClBC�BF�BH�BJcBJ�BM�BR�BM�BMABMBMBR�BN�BNBL�BMABT8BWJBS�Br�B��B�vB�HB�jB�B��B�KB�aB��B�UB�aB��B�?B��B��B��BrSBf�BH�BEB=�BA�B6�B3
B2mB3�B0`B2B,B&�B&�B')B$B#yB#�B#B!�B�B�B�B!9B}BEB�BB @B
��B
��B
�IB�B
��B
�B
�lB
ŭB
�cB
ɑB
ީB
��B
�sB
�|B
��B
�B
�}B
��B
��B
��B
��B
�B
��B
��B
�<B
��B
�!B
�B
|�B
��B
}-B
yIB
r�B
nB
pB
t_B
l�B
n�B
i�B
g�B
m�B
l�B
f=B
j!B
deB
q�B
�B
bYB
=�B
D
B
?TB
4yB
/�B
/ZB
0`B
0�B
/&B
�B
�B
B
�B
UB
�B
-B
tB
�B
BB	��B	�!B	��B	�AB	�	B	��B	�B	ީB	�JB	�DB	�xB	�~B	�&B	��B	�B	��B	��B	��B	�0B	�6B	��B	��B	��B	��B	�9B	��B	��B	�<B	� B	�,B	x�B	�B	�XB	nB	|�B	t_B	~�B	z�B	�qB	q�B	u�B	j�B	hB	r�B	e�B	iB	m�B	k�B	iB	c�B	j!B	f�B	f�B	c�B	d1B	a�B	`�B	`�B	`�B	_�B	a�B	aB	`�B	c_B	_�B	_GB	^uB	]oB	\ B	[�B	^AB	hB	b�B	`MB	^uB	Y�B	`B	Y"B	]oB	Z\B	W~B	V�B	^AB	]oB	YVB	[cB	\�B	X�B	W�B	a�B	YVB	f	B	Y�B	X�B	]�B	[.B	[�B	Z�B	[cB	[cB	[�B	Z�B	\�B	Y�B	Z(B	Y�B	\iB	V�B	X�B	T8B	VB	U�B	SfB	R�B	QZB	Q�B	QZB	O�B	OMB	L�B	LB	V�B	d�B	]�B	L�B	K�B	D
B	FB	A�B	A�B	@�B	G�B	?TB	B2B	;B	<B	?B	LB	<vB	:�B	;�B	@&B	7WB	=�B	G�B	B�B	K5B	NGB	D>B	G�B	;�B	5B	28B	0,B	?�B	>B	5KB	+�B	*<B	)�B	*�B	(/B	$B	&#B	#EB	"sB	$�B	$�B	,HB	-�B	,|B	+�B	,�B	4yB	3�B	3�B	5B	6QB	. B	/ZB	3
B	1�B	1�B	5B	6�B	3�B	6B	8�B	:jB	=�B	@ZB	?�B	=HB	>B	?TB	D
B	B2B	GB	OMG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230522120045                            20230522120045AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023052212004520230522120045  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023052212004520230522120045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023052212004520230522120045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               