CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-20T10:00:45Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230620100045  20230620100045  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�1ؙ���@�1ؙ���11  @�1���� @�1���� @0��ud0+@0��ud0+�d��L�d��L11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?k�@   @G�@��\@��R@��R@�p�A   AG�A ��A,(�A?\)A^�RA\)A�  A��A�\)A�  AУ�A�  A�  B Q�B  B  B(�B�
B(  B0(�B8(�B@  BH  BO�
BX  B`  Bh(�Bp  Bw�
B�
B��B��B��B��B�  B�{B�  B�  B�{B�{B�=qB�  B��B�(�B�  B��
B�{B�{B�  B�  B�  B�  B�  B�{B�  B�{B�  B��B�  B�{B�{C   C  C  C  C
=C

=C{C�C
=C��C�C��C  C{C  C�C   C"  C$
=C&{C'��C)�C+�HC-�C/�C1��C4  C6  C8
=C:�C<
=C>
=C@
=CB  CD  CE�CG��CI��CK��CN
=CP{CR{CT{CV
=CX  CZ{C\{C^
=C`  Ca�Cc��Cf  Ch  Cj
=Cl{Cn
=Cp
=Cr{Ct
=Cv  Cx
=Cz  C{��C~
=C��C���C�  C���C�  C�C�  C���C�C�C�C�
=C�  C�C�C���C�  C�  C���C�  C�C�C�C�  C���C���C���C���C���C���C�C�C�  C���C���C���C���C�  C�  C���C��C���C���C���C�C�
=C�
=C�C�  C���C�  C�  C�C�C���C���C�  C�
=C�
=C�  C���C��C���C���C���C�  C�  C���C�  C�
=C�C�  C�  C���C�C�  C�  C�\C�C�  C���C�
=C�
=C���C�C�
=C���C��C���C�
=C�C�C���C���C�  C�  C���C���C���C�C�C�C�
=C�  C���C�
=C�\C�C�  C���C���C���C�C�  C���C���C�  C�
=C�C�  C�
=C�  C���C�  C���C�  C�C���C���D }qD �qD� D�qD� D�D��D�D��D  D��DD��D  D� D  D��D	  D	� D
�D
� D�D��D�qD��D�D}qD�qD}qD�qD}qD�qD��D�D}qD��D}qD  Dz�D�qD��D�D}qD��D}qD��D� D�D��D�qD� D  D� D��D� DD��D  D� D  D� D�qD� D��D � D!�D!}qD"  D"� D"��D#}qD#�qD$� D%  D%� D&  D&� D'�D'}qD'�qD(��D)  D)z�D)�qD*�D+D+� D+��D,� D-�D-��D.  D.z�D.�qD/� D/�qD0� D1�D1}qD1��D2}qD3  D3��D4�D4�D5D5� D5�qD6��D7�D7�D8�D8��D9�D9��D:�D:� D;  D;� D;�qD<��D=D=�D>D>}qD?  D?�D@  D@� DA�DA��DB  DB� DB��DC� DDDD��DE�DE��DF  DF� DF�qDG}qDG�qDH}qDI  DI��DJ�DJ� DK  DK� DK�qDL}qDM  DM� DN  DN}qDO  DO� DP  DP� DQ  DQz�DQ��DRz�DR�qDS}qDS��DT� DU�DU� DU�qDV}qDW�DW}qDX  DX}qDY  DY�DZ�DZz�D[  D[��D[��D\}qD]�D]��D^�D^}qD_  D_��D`�D`��Da�Da��Db�Db�Dc�Dc� Dc�qDd}qDe  De��De�qDf}qDg  Dg� Dh  Dh��Di  Di� Dj�Dj� Dj�qDk}qDk�qDl� DmDm��Dn  Dn}qDo  Do}qDo�qDp� Dq  Dq� Dq�qDr��DsDs��Ds�qDt}qDu  Du� Dv  Dvz�Dv�qDw��Dx�Dx��Dy�Dy}qDy�qDz� D{  D{� D{�qD|}qD}  D}��D~�D~}qD~��Dz�D�  D�AHD�� D���D�HD�>�D�}qD�� D�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD���D�  D�C�D���D�D��D�C�D��HD��qD�  D�AHD���D�� D�  D�=qD��HD��HD��qD�@ D���D��HD���D�@ D�� D���D�HD�B�D�� D�� D��D�B�D�� D��)D��qD�@ D��HD��HD�HD�C�D�~�D�� D�HD�>�D�}qD�� D�  D�>�D�~�D��HD�  D�>�D���D��HD���D�AHD�� D���D��D�B�D�~�D���D��qD�@ D���D�D��D�@ D�}qD���D�  D�>�D�� D�� D�  D�B�D�~�D�� D�HD�AHD��HD���D���D�>�D�� D�D���D�>�D���D���D��qD�AHD���D�� D��qD�>�D�~�D��)D���D�B�D�� D��qD�  D�AHD�|)D�� D��D�>�D�}qD�� D�  D�AHD�~�D��qD���D�AHD��HD���D��qD�@ D�~�D��qD�HD�AHD�~�D�D�HD�AHD�� D���D��D�@ D�� D��HD��qD�>�D�~�D���D�  D�>�D��HD�� D��)D�=qD�}qD��HD�HD�B�D�}qD�� D���D�>�D��HD��HD���D�@ D�� D���D�  D�AHD�~�D��qD���D�>�D�� D��)D���D�=qD���D�� D��D�AHD�� D�� D��D�B�D���D��HD�  D�@ D���D�� D�  D�>�D�� D�� D�  D�>�D�~�D���D�HD�@ D�~�D�� D�  D�AHD�� D�� D�HD�@ D�~�D�� D��D�>�D���D���D�HD�@ D��HD���D���D�@ D��HD���D���D�@ D���D�D�HD�@ D�~�D�� D�HD�B�D�� D�� D�HD�>�D�� D��HD�HD�@ D�~�D���D�  D�>�D�� D��HD���D�@ D�~�D�� D�HD�@ D�� D��HD�  D�>�D D��HD�HD�>�D�}qD�� D��D�AHD�~�DĽqD���D�<)Dŀ D�� D���D�>�Dƀ D�� D�HD�>�DǁHDǾ�D�HD�AHDȀ DȾ�D�HD�B�Dɂ�D��HD���D�AHD�~�Dʾ�D�  D�@ D�}qD˾�D���D�=qD�~�D̼)D��qD�@ D�~�D;�D��qD�>�D΀ D�� D��qD�>�D�~�DϽqD��qD�>�D�~�Dо�D�HD�@ Dр D�D�HD�AHDҀ D�� D��D�AHDӁHDӽqD���D�>�DԀ DԾ�D�  D�@ DՁHD�� D��qD�=qDր D��HD�  D�AHD׀ D�� D�HD�>�D؁HD�� D�  D�@ D�~�D��HD�  D�AHD�~�D�D�HD�AHDہHD۾�D�  D�@ D܁HDܾ�D���D�@ D݁HD�� D���D�>�DށHD�D�HD�AHD߀ D��HD�  D�@ D�}qD�� D��D�C�D�~�D�� D���D�AHD�~�D��HD���D�>�D�~�D�� D�  D�AHD�HD�qD��D�@ D�HD��HD�  D�@ D� D�qD��qD�>�D� D�� D�  D�@ D�HD�� D��qD�=qD�~�D龸D���D�<)D�}qD��HD��D�@ D�~�D�)D���D�AHD�~�D�� D��D�@ D�HD�� D���D�@ D�~�D�qD�HD�AHD�}qD�qD�  D�@ D���D�D���D�>�D�|)D��HD���D�@ D�~�D��HD�  D�@ D� D�D�  D�@ D�~�D���D���D�@ D�� D�� D��qD�C�D�� D��HD�HD�>�D�~�D�D���D�>�D�~�D��HD�  D�>�D�� D���D�HD�B�D�l�>�?L��?u?��R?�
=?��H@
=@+�@@  @O\)@^�R@xQ�@���@�@�  @�=q@�z�@�G�@�=q@�Q�@��@��@���A�A�A��A33A=qA   A%�A)��A.�RA3�
A8��A>{ADz�AJ�HAP  AU�A[�A^{Ac33Ag�Amp�As�
Ax��A~�RA��A�(�A�ffA���A��
A�ffA���A�z�A��RA�G�A��
A�p�A�  A��\A���A�Q�A��\A�p�A�
=A���A��
A�
=A��\A�A�Q�AÅA�ffAə�A�(�A�\)Aҏ\A�ffA��A�p�A�  A��HA�ffA�G�A�z�A�Q�A�A�
=A���A��
A�
=B ��B=qB�B��BffB�Bz�B	G�B
ffB33BQ�Bp�BffB\)B  B��Bp�B=qB
=B�
B��B�B�RB�Bz�B��B��BffB33B�
B��B{B�HB�
B z�B ��B!B"�\B#�B$��B%�B&�HB'�
B(z�B)��B*�\B+�B,��B.{B/
=B0(�B0��B1B2�HB3�B4��B6{B7
=B8(�B8��B9��B:�\B;�B<��B=�B>�HB?�B@z�BAG�BB�\BC�
BD��BF{BF�RBG�BH��BJ=qBK\)BLz�BM��BNffBO\)BPz�BQp�BS
=BT(�BT��BUBV�RBX  BYG�BZffB[33B\  B\��B]B^�\B_�
Ba�Ba�Bb�HBc�BdQ�Be�Bf�\Bg�BhQ�Bi�BiBj�\Bk�Bl��BmBn�\Bo\)Bp  Bp��Bq�Bs33BtQ�Bu�BuBv�RBw�Bx��By�B{
=B{�B|z�B}p�B~ffB�
B�Q�B��HB�33B��B�(�B���B�\)B��
B�ffB��RB�33B��
B�ffB���B�p�B��
B�Q�B���B�33B�  B��\B��B�\)B��B�z�B��HB��B�=qB��RB��B���B�{B���B�\)B�B�(�B��RB�\)B��B�z�B��RB�G�B�B��\B��B��B��B�ffB�G�B��
B�=qB���B�p�B�{B���B���B���B�{B���B���B��B�z�B�
=B��
B�ffB��HB�p�B�{B���B�\)B�B�Q�B��HB�B�ffB���B�\)B��
B���B�p�B�B�ffB�33B��B�ffB��HB���B�ffB���B��B�(�B�
=B�B�(�B���B���B�Q�B��HB�\)B�  B��HB��B�  B��\B�\)B�{B�z�B��B�  B���B��B��B�z�B�G�B��B�Q�B�33B��
B�Q�B���B��BƏ\B���BǮB�z�B�33BɮB�Q�B�33B��B�Q�B�
=B��BΏ\B�
=B�B�z�B�G�B��
B�ffB�
=B��B�z�B�
=B�B֣�B��B�B�ffB�G�B�  B�ffB�
=B�  Bܣ�B�33B��
B޸RB�G�B��B��HBᙚB�{B���B�B�Q�B�
=B�B�\B�G�B��
B��B�p�B�  B��B�B�Q�B���B�p�B�ffB��BB�z�B�G�B�B��B�p�B��B���B��B�  B���B��B�=qB��HB�B�z�B���B��B��\B�
=B�B���B�G�B��C ffC ��C  C\)C�
C�CffC�HC33Cp�CC=qCz�C��CG�C��C�HCQ�C�C�CG�C�C�CQ�C�C�HC	G�C	�C	�C
G�C
�RC
��CQ�CC��CQ�CC��CffCC��CQ�CC  CQ�CC  CQ�CC
=CQ�CC{CQ�C�C
=CQ�C�C
=CG�C��C{CQ�C�C{CQ�C�RC�C\)C�RC�C\)C��C(�CffC�
C33Cp�C�HC=qC�C��CG�C�\C
=CG�C��C
=C\)C��C{C\)C�C �C ffC C!=qC!z�C!��C"G�C"�\C#
=C#Q�C#�RC$33C$p�C$�C%=qC%�\C&
=C&\)C&�C'(�C'p�C'�C(G�C(�\C){C)ffC)�
C*=qC*�C+{C+Q�C+�
C,(�C,�C-
=C-\)C-�HC.(�C.��C/{C/\)C/�HC033C0�C1{C1ffC1�C233C2�RC3
=C3p�C3��C4=qC4�RC5
=C5�\C5�
C6G�C6�RC7
=C7�C7�RC8�C8�\C8C9�C9\)C9C9�HC:33C:�C:��C:��C;
=C;Q�C;�\C;�C<
=C<�C<p�C<�C<�
C<��C=�C=z�C=��C=�HC>  C>=qC>�C>��C>��C?
=C?\)C?�\C?�C@
=C@(�C@z�C@�\C@�
CA�CA33CA�\CA�CB  CB�CBp�CB��CBCC�CC=qCC�\CC�CD  CD�CD\)CD��CDCE{CE33CEz�CE��CE�CF{CF=qCF��CF�CG  CG�CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 ?k�@   @G�@��\@��R@��R@�p�A   AG�A ��A,(�A?\)A^�RA\)A�  A��A�\)A�  AУ�A�  A�  B Q�B  B  B(�B�
B(  B0(�B8(�B@  BH  BO�
BX  B`  Bh(�Bp  Bw�
B�
B��B��B��B��B�  B�{B�  B�  B�{B�{B�=qB�  B��B�(�B�  B��
B�{B�{B�  B�  B�  B�  B�  B�{B�  B�{B�  B��B�  B�{B�{C   C  C  C  C
=C

=C{C�C
=C��C�C��C  C{C  C�C   C"  C$
=C&{C'��C)�C+�HC-�C/�C1��C4  C6  C8
=C:�C<
=C>
=C@
=CB  CD  CE�CG��CI��CK��CN
=CP{CR{CT{CV
=CX  CZ{C\{C^
=C`  Ca�Cc��Cf  Ch  Cj
=Cl{Cn
=Cp
=Cr{Ct
=Cv  Cx
=Cz  C{��C~
=C��C���C�  C���C�  C�C�  C���C�C�C�C�
=C�  C�C�C���C�  C�  C���C�  C�C�C�C�  C���C���C���C���C���C���C�C�C�  C���C���C���C���C�  C�  C���C��C���C���C���C�C�
=C�
=C�C�  C���C�  C�  C�C�C���C���C�  C�
=C�
=C�  C���C��C���C���C���C�  C�  C���C�  C�
=C�C�  C�  C���C�C�  C�  C�\C�C�  C���C�
=C�
=C���C�C�
=C���C��C���C�
=C�C�C���C���C�  C�  C���C���C���C�C�C�C�
=C�  C���C�
=C�\C�C�  C���C���C���C�C�  C���C���C�  C�
=C�C�  C�
=C�  C���C�  C���C�  C�C���C���D }qD �qD� D�qD� D�D��D�D��D  D��DD��D  D� D  D��D	  D	� D
�D
� D�D��D�qD��D�D}qD�qD}qD�qD}qD�qD��D�D}qD��D}qD  Dz�D�qD��D�D}qD��D}qD��D� D�D��D�qD� D  D� D��D� DD��D  D� D  D� D�qD� D��D � D!�D!}qD"  D"� D"��D#}qD#�qD$� D%  D%� D&  D&� D'�D'}qD'�qD(��D)  D)z�D)�qD*�D+D+� D+��D,� D-�D-��D.  D.z�D.�qD/� D/�qD0� D1�D1}qD1��D2}qD3  D3��D4�D4�D5D5� D5�qD6��D7�D7�D8�D8��D9�D9��D:�D:� D;  D;� D;�qD<��D=D=�D>D>}qD?  D?�D@  D@� DA�DA��DB  DB� DB��DC� DDDD��DE�DE��DF  DF� DF�qDG}qDG�qDH}qDI  DI��DJ�DJ� DK  DK� DK�qDL}qDM  DM� DN  DN}qDO  DO� DP  DP� DQ  DQz�DQ��DRz�DR�qDS}qDS��DT� DU�DU� DU�qDV}qDW�DW}qDX  DX}qDY  DY�DZ�DZz�D[  D[��D[��D\}qD]�D]��D^�D^}qD_  D_��D`�D`��Da�Da��Db�Db�Dc�Dc� Dc�qDd}qDe  De��De�qDf}qDg  Dg� Dh  Dh��Di  Di� Dj�Dj� Dj�qDk}qDk�qDl� DmDm��Dn  Dn}qDo  Do}qDo�qDp� Dq  Dq� Dq�qDr��DsDs��Ds�qDt}qDu  Du� Dv  Dvz�Dv�qDw��Dx�Dx��Dy�Dy}qDy�qDz� D{  D{� D{�qD|}qD}  D}��D~�D~}qD~��Dz�D�  D�AHD�� D���D�HD�>�D�}qD�� D�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD���D�  D�C�D���D�D��D�C�D��HD��qD�  D�AHD���D�� D�  D�=qD��HD��HD��qD�@ D���D��HD���D�@ D�� D���D�HD�B�D�� D�� D��D�B�D�� D��)D��qD�@ D��HD��HD�HD�C�D�~�D�� D�HD�>�D�}qD�� D�  D�>�D�~�D��HD�  D�>�D���D��HD���D�AHD�� D���D��D�B�D�~�D���D��qD�@ D���D�D��D�@ D�}qD���D�  D�>�D�� D�� D�  D�B�D�~�D�� D�HD�AHD��HD���D���D�>�D�� D�D���D�>�D���D���D��qD�AHD���D�� D��qD�>�D�~�D��)D���D�B�D�� D��qD�  D�AHD�|)D�� D��D�>�D�}qD�� D�  D�AHD�~�D��qD���D�AHD��HD���D��qD�@ D�~�D��qD�HD�AHD�~�D�D�HD�AHD�� D���D��D�@ D�� D��HD��qD�>�D�~�D���D�  D�>�D��HD�� D��)D�=qD�}qD��HD�HD�B�D�}qD�� D���D�>�D��HD��HD���D�@ D�� D���D�  D�AHD�~�D��qD���D�>�D�� D��)D���D�=qD���D�� D��D�AHD�� D�� D��D�B�D���D��HD�  D�@ D���D�� D�  D�>�D�� D�� D�  D�>�D�~�D���D�HD�@ D�~�D�� D�  D�AHD�� D�� D�HD�@ D�~�D�� D��D�>�D���D���D�HD�@ D��HD���D���D�@ D��HD���D���D�@ D���D�D�HD�@ D�~�D�� D�HD�B�D�� D�� D�HD�>�D�� D��HD�HD�@ D�~�D���D�  D�>�D�� D��HD���D�@ D�~�D�� D�HD�@ D�� D��HD�  D�>�D D��HD�HD�>�D�}qD�� D��D�AHD�~�DĽqD���D�<)Dŀ D�� D���D�>�Dƀ D�� D�HD�>�DǁHDǾ�D�HD�AHDȀ DȾ�D�HD�B�Dɂ�D��HD���D�AHD�~�Dʾ�D�  D�@ D�}qD˾�D���D�=qD�~�D̼)D��qD�@ D�~�D;�D��qD�>�D΀ D�� D��qD�>�D�~�DϽqD��qD�>�D�~�Dо�D�HD�@ Dр D�D�HD�AHDҀ D�� D��D�AHDӁHDӽqD���D�>�DԀ DԾ�D�  D�@ DՁHD�� D��qD�=qDր D��HD�  D�AHD׀ D�� D�HD�>�D؁HD�� D�  D�@ D�~�D��HD�  D�AHD�~�D�D�HD�AHDہHD۾�D�  D�@ D܁HDܾ�D���D�@ D݁HD�� D���D�>�DށHD�D�HD�AHD߀ D��HD�  D�@ D�}qD�� D��D�C�D�~�D�� D���D�AHD�~�D��HD���D�>�D�~�D�� D�  D�AHD�HD�qD��D�@ D�HD��HD�  D�@ D� D�qD��qD�>�D� D�� D�  D�@ D�HD�� D��qD�=qD�~�D龸D���D�<)D�}qD��HD��D�@ D�~�D�)D���D�AHD�~�D�� D��D�@ D�HD�� D���D�@ D�~�D�qD�HD�AHD�}qD�qD�  D�@ D���D�D���D�>�D�|)D��HD���D�@ D�~�D��HD�  D�@ D� D�D�  D�@ D�~�D���D���D�@ D�� D�� D��qD�C�D�� D��HD�HD�>�D�~�D�D���D�>�D�~�D��HD�  D�>�D�� D���D�HD�B�D�l�>�?L��?u?��R?�
=?��H@
=@+�@@  @O\)@^�R@xQ�@���@�@�  @�=q@�z�@�G�@�=q@�Q�@��@��@���A�A�A��A33A=qA   A%�A)��A.�RA3�
A8��A>{ADz�AJ�HAP  AU�A[�A^{Ac33Ag�Amp�As�
Ax��A~�RA��A�(�A�ffA���A��
A�ffA���A�z�A��RA�G�A��
A�p�A�  A��\A���A�Q�A��\A�p�A�
=A���A��
A�
=A��\A�A�Q�AÅA�ffAə�A�(�A�\)Aҏ\A�ffA��A�p�A�  A��HA�ffA�G�A�z�A�Q�A�A�
=A���A��
A�
=B ��B=qB�B��BffB�Bz�B	G�B
ffB33BQ�Bp�BffB\)B  B��Bp�B=qB
=B�
B��B�B�RB�Bz�B��B��BffB33B�
B��B{B�HB�
B z�B ��B!B"�\B#�B$��B%�B&�HB'�
B(z�B)��B*�\B+�B,��B.{B/
=B0(�B0��B1B2�HB3�B4��B6{B7
=B8(�B8��B9��B:�\B;�B<��B=�B>�HB?�B@z�BAG�BB�\BC�
BD��BF{BF�RBG�BH��BJ=qBK\)BLz�BM��BNffBO\)BPz�BQp�BS
=BT(�BT��BUBV�RBX  BYG�BZffB[33B\  B\��B]B^�\B_�
Ba�Ba�Bb�HBc�BdQ�Be�Bf�\Bg�BhQ�Bi�BiBj�\Bk�Bl��BmBn�\Bo\)Bp  Bp��Bq�Bs33BtQ�Bu�BuBv�RBw�Bx��By�B{
=B{�B|z�B}p�B~ffB�
B�Q�B��HB�33B��B�(�B���B�\)B��
B�ffB��RB�33B��
B�ffB���B�p�B��
B�Q�B���B�33B�  B��\B��B�\)B��B�z�B��HB��B�=qB��RB��B���B�{B���B�\)B�B�(�B��RB�\)B��B�z�B��RB�G�B�B��\B��B��B��B�ffB�G�B��
B�=qB���B�p�B�{B���B���B���B�{B���B���B��B�z�B�
=B��
B�ffB��HB�p�B�{B���B�\)B�B�Q�B��HB�B�ffB���B�\)B��
B���B�p�B�B�ffB�33B��B�ffB��HB���B�ffB���B��B�(�B�
=B�B�(�B���B���B�Q�B��HB�\)B�  B��HB��B�  B��\B�\)B�{B�z�B��B�  B���B��B��B�z�B�G�B��B�Q�B�33B��
B�Q�B���B��BƏ\B���BǮB�z�B�33BɮB�Q�B�33B��B�Q�B�
=B��BΏ\B�
=B�B�z�B�G�B��
B�ffB�
=B��B�z�B�
=B�B֣�B��B�B�ffB�G�B�  B�ffB�
=B�  Bܣ�B�33B��
B޸RB�G�B��B��HBᙚB�{B���B�B�Q�B�
=B�B�\B�G�B��
B��B�p�B�  B��B�B�Q�B���B�p�B�ffB��BB�z�B�G�B�B��B�p�B��B���B��B�  B���B��B�=qB��HB�B�z�B���B��B��\B�
=B�B���B�G�B��C ffC ��C  C\)C�
C�CffC�HC33Cp�CC=qCz�C��CG�C��C�HCQ�C�C�CG�C�C�CQ�C�C�HC	G�C	�C	�C
G�C
�RC
��CQ�CC��CQ�CC��CffCC��CQ�CC  CQ�CC  CQ�CC
=CQ�CC{CQ�C�C
=CQ�C�C
=CG�C��C{CQ�C�C{CQ�C�RC�C\)C�RC�C\)C��C(�CffC�
C33Cp�C�HC=qC�C��CG�C�\C
=CG�C��C
=C\)C��C{C\)C�C �C ffC C!=qC!z�C!��C"G�C"�\C#
=C#Q�C#�RC$33C$p�C$�C%=qC%�\C&
=C&\)C&�C'(�C'p�C'�C(G�C(�\C){C)ffC)�
C*=qC*�C+{C+Q�C+�
C,(�C,�C-
=C-\)C-�HC.(�C.��C/{C/\)C/�HC033C0�C1{C1ffC1�C233C2�RC3
=C3p�C3��C4=qC4�RC5
=C5�\C5�
C6G�C6�RC7
=C7�C7�RC8�C8�\C8C9�C9\)C9C9�HC:33C:�C:��C:��C;
=C;Q�C;�\C;�C<
=C<�C<p�C<�C<�
C<��C=�C=z�C=��C=�HC>  C>=qC>�C>��C>��C?
=C?\)C?�\C?�C@
=C@(�C@z�C@�\C@�
CA�CA33CA�\CA�CB  CB�CBp�CB��CBCC�CC=qCC�\CC�CD  CD�CD\)CD��CDCE{CE33CEz�CE��CE�CF{CF=qCF��CF�CG  CG�CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�  A���A���A��A���A���A���A���A���A���A���A���A�  A�  A�  A�1A�A�VA�bA�bA�oA�bA�{A��A��A��A��A��A��A� �A��A��A�
=A��AѲ-A�ĜAϲ-A��A·+A�  A�9XA�|�A�t�Aʧ�A�p�A�E�A�JA�bAȣ�A�Aǉ7A�$�A���A�|�A�33A�S�A+A���A�A��HA�dZA�n�A�K�A���A�7LA�
=A��A�O�A��PA��PA�M�A��+A���A���A��A�7LA���A�A��A�
=A�A�A�XA���A���A���A���A��A�^5A�`BA�v�A��wA�`BA�t�A�1A��uA���A�5?A��
A��A���A��A�"�A}/Ay?}As�
Ap��Ao?}AlAi�mAg��Ad�uAa�7A^ȴAZAX�AWdZAT�AR�RAP�DANĜAL��AKO�AJM�AIO�AG?}AFbACx�A@bA<�DA;VA9C�A5��A4ĜA3t�A2I�A0�+A.��A+�A)33A)K�A)��A)p�A(  A&�9A$��A#�A!��A�7AoA�7Av�A&�Az�A�FAȴAv�A�A�yA�;A�A1A�!A�A�A�9A�A��A�jA�DA~�AQ�A�A�
A�^A\)A��A��AG�AVA�AI�AjAVA�A7LA��A�7AȴAffA-A�A�#A�wA�PA�A
�RA	�A	�7A	&�AȴAE�A�PA7LA��A�!AE�A�wAt�A;dA��A�uAA�A�wA�A��A�A&�A�HA�uAM�A-A��AXA �A ��A 1@�;d@�;d@���@�v�@�`B@��@��R@�-@�`B@�b@�l�@��!@�-@��@�(�@�1@���@�F@��H@��@��@��`@�A�@�\)@��y@�~�@��^@��@웦@�ƨ@�dZ@�+@��@�O�@�G�@�7L@��@��
@�"�@�5?@��@���@�dZ@�33@�ȴ@�M�@��@�hs@��@�\)@�v�@��@�%@�1'@ە�@�S�@�ȴ@�^5@�x�@���@؛�@�Z@�1@׍P@�o@�ȴ@֏\@�@�G�@Ԭ@Ӿw@��y@҇+@��T@�O�@У�@���@�\)@�@��y@���@�-@Ͳ-@�p�@�j@ʸR@��T@�?}@��`@ȣ�@ȋD@�Z@�A�@�1@���@�
=@�~�@��@��@ź^@š�@őh@�x�@�V@Ĭ@ă@�Z@���@�S�@��y@�^5@��@�@��h@�`B@��@�9X@�t�@�+@��@���@�ff@�{@���@��^@���@��7@�`B@�&�@��@��u@�1'@�b@��m@��P@�t�@�;d@�"�@�o@��y@�ȴ@�~�@�J@��-@�hs@�7L@��@��@�z�@��@�|�@�v�@�{@��h@�?}@�?}@�?}@�/@�V@��@�Ĝ@�z�@�ƨ@�;d@���@�=q@��@���@��`@�Q�@�1@��
@�ƨ@��w@��F@��P@�@�ff@�{@���@�?}@��@��/@��@��u@�%@��j@�z�@�A�@���@���@��P@�l�@�+@��H@�~�@�-@��#@�O�@��/@��D@�bN@�(�@��
@�C�@��@��y@��+@�=q@��T@�O�@��@�%@���@���@�bN@�9X@��@�l�@�33@���@��R@�M�@���@���@��-@���@�p�@�hs@�`B@���@�j@���@��@�S�@��H@��!@�^5@�M�@�-@���@�X@��@��j@�bN@� �@���@���@�;d@���@�5?@�J@�@�x�@�V@��j@��D@�r�@��@���@��F@�t�@�"�@��@��@��+@���@�hs@���@���@�Z@�1'@�|�@��@�@��H@�^5@���@���@���@��@�p�@�X@�/@��D@�I�@��@�t�@�33@���@�5?@�$�@�{@�@��@���@��h@�X@���@�b@��;@���@�ƨ@��w@��F@��@��@�"�@��@�~�@�=q@�{@��@��#@��#@���@���@���@�p�@��@��@�A�@� �@���@��
@��F@�S�@�o@��@�ȴ@��R@���@��\@�~�@�=q@���@���@�`B@�/@�V@���@���@�r�@�Q�@�I�@�A�@�9X@���@��@�+@��@���@�~�@�n�@�{@�@�`B@�V@���@�r�@�P@
=@~ff@~$�@~@}��@}/@|�@{��@{S�@{"�@{@z��@z~�@y�^@y�@x��@x��@xQ�@x �@w��@wK�@w�@w
=@vȴ@vE�@v@u��@u��@u�h@u`B@u?}@u?}@uV@t�j@tZ@t�@t�@t1@s�
@sƨ@s�F@sS�@so@r�!@r^5@rJ@q�^@q7L@p�9@pr�@pA�@o��@o�@o�P@oK�@n�R@nV@m�@m�-@mp�@m�@l�@l9X@kƨ@kS�@k"�@j�H@j��@j~�@i��@i7L@i%@h�u@g��@gl�@g
=@f�R@e�T@d�D@c�m@ct�@c�@c33@b^5@b=q@a�7@`��@`A�@_�@_��@_+@^ff@]�@\�/@\z�@[�F@[C�@["�@[o@Z��@Z�\@Zn�@Z-@Yhs@X�`@X��@X�9@W�@V�y@V��@Vff@T��@T9X@T1@S��@S@R��@R~�@RJ@Q�@Q��@Q&�@P�u@P �@O��@O|�@O�@N�+@N$�@M�-@L�@LZ@L(�@K�
@Kt�@K33@J�@J^5@J-@I��@I��@Ix�@I7L@H��@HĜ@H��@HbN@Hb@G�@G�w@G��@G
=@F�@F��@F�+@Fff@F$�@E�@E@E��@E�@D��@Dz�@D�@Ct�@C33@B�H@B��@B=q@Ahs@A�@@��@@��@@��@@�`@@�u@@b@?�P@?;d@>ȴ@>��@>$�@=@=�h@=`B@=�@<��@<�j@<(�@;�
@;�F@;��@;S�@:��@:�\@:M�@:-@:�@9��@9x�@9hs@9&�@8��@8��@8�@8A�@7�w@7��@6��@6@5�h@5�@4��@4z�@4j@4I�@4I�@4z�@4�D@4j@4j@3��@3��@3S�@3o@2�@2��@1�@1X@0��@0�u@0bN@0A�@0  @/�P@/;d@.��@.�R@.@-�@-��@-�@,��@,j@,�@+��@+�m@+ƨ@+�F@+��@+��@+��@+t�@+C�@+33@+"�@+@*��@*�\@*~�@*n�@*�@)��@)X@)7L@)&�@(��@(�`@(��@(��@(r�@(A�@'��@'K�@'
=@&�@&��@&E�@%�T@%@%`B@%V@$��@$��@$�D@$�@#�
@#dZ@"��@"�\@"^5@"-@!��@!hs@!%@ Ĝ@ �@ Q�@ 1'@ b@   @��@��@l�@K�@
=@��@��@V@E�@�@�h@`B@��@(�@1@�F@��@�@t�@"�@�@~�@��@��@�7@&�@Ĝ@r�@A�@b@�w@|�@l�@l�@\)@+@
=@
=@�y@ȴ@��@�+@V@E�@5?@@�-@�@?}@�/@j@9X@�m@t�@�H@�!@�\@~�@=q@��@��@�7@�7@hs@&�@�`@��@Ĝ@��@1'@ �@  @�;@��@�P@l�@;d@��@�@ȴ@��@v�@V@@p�@O�@V@��@��@�@�/@�@�/@�/@�j@z�@I�@(�@(�@1@ƨ@dZ@o@@
�@
�@
��@
��@
��@
n�@
=q@	�#@	��@	hs@	hs@	G�@	�@	%@	%A���A���A�  A�A�A�  A���A���A���A�  A���A���A���A��A��A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A�A���A�  A�  A�A�A�A�A���A���A���A���A���A�  A�A�%A�1A�1A�1A�JA�1A�
=A�1A�%A�A�A�A�A�1A�bA�oA�oA�bA�JA�
=A�bA�{A�{A�{A�{A�VA�JA�VA�VA��A�oA�{A�oA�oA�oA�bA�VA�bA�bA�oA�oA�{A�oA�bA�bA�bA�oA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A��A��A��A�"�A� �A� �A��A��A��A�"�A�"�A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�VA�bA��A��A�JA�1A�%A�A�bA�VA�
=A�A���A���A���A���A��A��A��A��`A��TA��;A��;A��/A��
A���AѴ9A�~�A�=qA�A�A�
=A��A��
A���AН�AЕ�AЍPA�\)A�%A���Aϧ�Aϛ�Aϗ�Aϗ�Aϗ�AϏ\A�x�A�`BA�?}A�{A��yA��;A���AθRAήAΡ�AΕ�AΉ7A�~�A�l�A�ZA�I�A�7LA�{A�
=A��A��`A��#A���A;wA͑hA�ZA�-A��A�JA���A��
A̼jA̧�A̓uA�|�A�n�A�`BA�K�A�(�A���A˾wA�~�A�n�A�ZA�&�A���A��A��/A�ȴAʡ�A�z�A�p�A�p�A�p�A�n�A�n�A�p�A�t�A�t�A�r�A�l�A�C�A�33A�5?A�I�A�M�A�=qA�1'A�$�A�%A���A�
=A���A��/A�v�A�VA��`A��mA��A��A��TAȮAȧ�Aȣ�Aț�A�ffA�E�A� �A��A��A��#A�ȴAǰ!Aǟ�AǛ�AǕ�A�~�A�hsA�O�A�9XA�1'A�-A��A�1A���A��;A��A�
=A�1A�%A�A���A��mA�ȴA�jA�-A���A�x�A�M�A�C�A�$�A�bA��A�ƨAę�A�v�A�/A��AüjA�C�A�ƨA�\)A�?}A��A���A��/A��wA���A���A���A�Q�A���A�=qA��A��A���A�^5A�I�A�
=A��/A�ƨA��A�n�A�bA��jA�p�A���A��FA�x�A���A�|�A�VA��A��A��A�v�A�ZA�=qA�$�A���A�G�A��FA�ZA�5?A�VA�A��A��hA�A�5?A��A�A���A�{A�I�A�A��`A��!A�ffA�oA��A�XA��HA�l�A�K�A�A�A��A���A��/A���A��PA�p�A�S�A�A���A��RA���A�hsA��A��mA��DA�;dA�"�A��A���A��A�oA���A�-A��^A�`BA�bA���A���A�x�A�M�A�I�A�C�A�9XA��#A�ffA��A�G�A��-A�C�A��jA��DA��7A�~�A�|�A�dZA�-A�"�A�1A���A��A��wA��DA�n�A�dZA�bNA�ZA�;dA�bA�A��7A��A���A��A��A���A��A�VA�(�A�JA���A��A��`A�A�~�A�l�A�;dA�  A��;A��A���A�I�A�JA��yA���A���A�+A�A��yA���A�hsA�33A�bA���A��7A�M�A�A��;A�ȴA���A�bNA�{A���A���A��FA���A�t�A�dZA�\)A� �A��7A���A�O�A��jA�z�A�S�A�A�A�7LA��A��HA�~�A�bA��jA�`BA��A�l�A�&�A��yA�"�A�9XA���A�C�A�A�ĜA��\A�ZA�K�A��A��A��FA�^5A�A�A�9XA�/A�$�A�JA���A��;A�A��^A��9A���A�M�A��RA�5?A��/A��-A���A���A���A�dZA�33A��A�ȴA��A�O�A�+A�  A��;A��FA�|�A�"�A��7A���A��DA�1A��A��HA�ƨA��9A���A��A�n�A�Q�A�G�A�9XA�/A�$�A�  A��DA��TA���A�Q�A��A���A�=qA��HA�K�A���A�ƨA��A�A�A�JA��#A�r�A�{A�XAA�A~��A~(�A}��A}��A}K�A|��A|ĜA|bNA{�mA{t�A{�Az�!AzM�Ay�-Ax��Aw�;AwhsAw
=Av�uAvbAu��Atr�As�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 A���A�  A���A���A��A���A���A���A���A���A���A���A���A�  A�  A�  A�1A�A�VA�bA�bA�oA�bA�{A��A��A��A��A��A��A� �A��A��A�
=A��AѲ-A�ĜAϲ-A��A·+A�  A�9XA�|�A�t�Aʧ�A�p�A�E�A�JA�bAȣ�A�Aǉ7A�$�A���A�|�A�33A�S�A+A���A�A��HA�dZA�n�A�K�A���A�7LA�
=A��A�O�A��PA��PA�M�A��+A���A���A��A�7LA���A�A��A�
=A�A�A�XA���A���A���A���A��A�^5A�`BA�v�A��wA�`BA�t�A�1A��uA���A�5?A��
A��A���A��A�"�A}/Ay?}As�
Ap��Ao?}AlAi�mAg��Ad�uAa�7A^ȴAZAX�AWdZAT�AR�RAP�DANĜAL��AKO�AJM�AIO�AG?}AFbACx�A@bA<�DA;VA9C�A5��A4ĜA3t�A2I�A0�+A.��A+�A)33A)K�A)��A)p�A(  A&�9A$��A#�A!��A�7AoA�7Av�A&�Az�A�FAȴAv�A�A�yA�;A�A1A�!A�A�A�9A�A��A�jA�DA~�AQ�A�A�
A�^A\)A��A��AG�AVA�AI�AjAVA�A7LA��A�7AȴAffA-A�A�#A�wA�PA�A
�RA	�A	�7A	&�AȴAE�A�PA7LA��A�!AE�A�wAt�A;dA��A�uAA�A�wA�A��A�A&�A�HA�uAM�A-A��AXA �A ��A 1@�;d@�;d@���@�v�@�`B@��@��R@�-@�`B@�b@�l�@��!@�-@��@�(�@�1@���@�F@��H@��@��@��`@�A�@�\)@��y@�~�@��^@��@웦@�ƨ@�dZ@�+@��@�O�@�G�@�7L@��@��
@�"�@�5?@��@���@�dZ@�33@�ȴ@�M�@��@�hs@��@�\)@�v�@��@�%@�1'@ە�@�S�@�ȴ@�^5@�x�@���@؛�@�Z@�1@׍P@�o@�ȴ@֏\@�@�G�@Ԭ@Ӿw@��y@҇+@��T@�O�@У�@���@�\)@�@��y@���@�-@Ͳ-@�p�@�j@ʸR@��T@�?}@��`@ȣ�@ȋD@�Z@�A�@�1@���@�
=@�~�@��@��@ź^@š�@őh@�x�@�V@Ĭ@ă@�Z@���@�S�@��y@�^5@��@�@��h@�`B@��@�9X@�t�@�+@��@���@�ff@�{@���@��^@���@��7@�`B@�&�@��@��u@�1'@�b@��m@��P@�t�@�;d@�"�@�o@��y@�ȴ@�~�@�J@��-@�hs@�7L@��@��@�z�@��@�|�@�v�@�{@��h@�?}@�?}@�?}@�/@�V@��@�Ĝ@�z�@�ƨ@�;d@���@�=q@��@���@��`@�Q�@�1@��
@�ƨ@��w@��F@��P@�@�ff@�{@���@�?}@��@��/@��@��u@�%@��j@�z�@�A�@���@���@��P@�l�@�+@��H@�~�@�-@��#@�O�@��/@��D@�bN@�(�@��
@�C�@��@��y@��+@�=q@��T@�O�@��@�%@���@���@�bN@�9X@��@�l�@�33@���@��R@�M�@���@���@��-@���@�p�@�hs@�`B@���@�j@���@��@�S�@��H@��!@�^5@�M�@�-@���@�X@��@��j@�bN@� �@���@���@�;d@���@�5?@�J@�@�x�@�V@��j@��D@�r�@��@���@��F@�t�@�"�@��@��@��+@���@�hs@���@���@�Z@�1'@�|�@��@�@��H@�^5@���@���@���@��@�p�@�X@�/@��D@�I�@��@�t�@�33@���@�5?@�$�@�{@�@��@���@��h@�X@���@�b@��;@���@�ƨ@��w@��F@��@��@�"�@��@�~�@�=q@�{@��@��#@��#@���@���@���@�p�@��@��@�A�@� �@���@��
@��F@�S�@�o@��@�ȴ@��R@���@��\@�~�@�=q@���@���@�`B@�/@�V@���@���@�r�@�Q�@�I�@�A�@�9X@���@��@�+@��@���@�~�@�n�@�{@�@�`B@�V@���@�r�@�P@
=@~ff@~$�@~@}��@}/@|�@{��@{S�@{"�@{@z��@z~�@y�^@y�@x��@x��@xQ�@x �@w��@wK�@w�@w
=@vȴ@vE�@v@u��@u��@u�h@u`B@u?}@u?}@uV@t�j@tZ@t�@t�@t1@s�
@sƨ@s�F@sS�@so@r�!@r^5@rJ@q�^@q7L@p�9@pr�@pA�@o��@o�@o�P@oK�@n�R@nV@m�@m�-@mp�@m�@l�@l9X@kƨ@kS�@k"�@j�H@j��@j~�@i��@i7L@i%@h�u@g��@gl�@g
=@f�R@e�T@d�D@c�m@ct�@c�@c33@b^5@b=q@a�7@`��@`A�@_�@_��@_+@^ff@]�@\�/@\z�@[�F@[C�@["�@[o@Z��@Z�\@Zn�@Z-@Yhs@X�`@X��@X�9@W�@V�y@V��@Vff@T��@T9X@T1@S��@S@R��@R~�@RJ@Q�@Q��@Q&�@P�u@P �@O��@O|�@O�@N�+@N$�@M�-@L�@LZ@L(�@K�
@Kt�@K33@J�@J^5@J-@I��@I��@Ix�@I7L@H��@HĜ@H��@HbN@Hb@G�@G�w@G��@G
=@F�@F��@F�+@Fff@F$�@E�@E@E��@E�@D��@Dz�@D�@Ct�@C33@B�H@B��@B=q@Ahs@A�@@��@@��@@��@@�`@@�u@@b@?�P@?;d@>ȴ@>��@>$�@=@=�h@=`B@=�@<��@<�j@<(�@;�
@;�F@;��@;S�@:��@:�\@:M�@:-@:�@9��@9x�@9hs@9&�@8��@8��@8�@8A�@7�w@7��@6��@6@5�h@5�@4��@4z�@4j@4I�@4I�@4z�@4�D@4j@4j@3��@3��@3S�@3o@2�@2��@1�@1X@0��@0�u@0bN@0A�@0  @/�P@/;d@.��@.�R@.@-�@-��@-�@,��@,j@,�@+��@+�m@+ƨ@+�F@+��@+��@+��@+t�@+C�@+33@+"�@+@*��@*�\@*~�@*n�@*�@)��@)X@)7L@)&�@(��@(�`@(��@(��@(r�@(A�@'��@'K�@'
=@&�@&��@&E�@%�T@%@%`B@%V@$��@$��@$�D@$�@#�
@#dZ@"��@"�\@"^5@"-@!��@!hs@!%@ Ĝ@ �@ Q�@ 1'@ b@   @��@��@l�@K�@
=@��@��@V@E�@�@�h@`B@��@(�@1@�F@��@�@t�@"�@�@~�@��@��@�7@&�@Ĝ@r�@A�@b@�w@|�@l�@l�@\)@+@
=@
=@�y@ȴ@��@�+@V@E�@5?@@�-@�@?}@�/@j@9X@�m@t�@�H@�!@�\@~�@=q@��@��@�7@�7@hs@&�@�`@��@Ĝ@��@1'@ �@  @�;@��@�P@l�@;d@��@�@ȴ@��@v�@V@@p�@O�@V@��@��@�@�/@�@�/@�/@�j@z�@I�@(�@(�@1@ƨ@dZ@o@@
�@
�@
��@
��@
��@
n�@
=q@	�#@	��@	hs@	hs@	G�@	�@	%@	%A���A���A�  A�A�A�  A���A���A���A�  A���A���A���A��A��A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A�A���A�  A�  A�A�A�A�A���A���A���A���A���A�  A�A�%A�1A�1A�1A�JA�1A�
=A�1A�%A�A�A�A�A�1A�bA�oA�oA�bA�JA�
=A�bA�{A�{A�{A�{A�VA�JA�VA�VA��A�oA�{A�oA�oA�oA�bA�VA�bA�bA�oA�oA�{A�oA�bA�bA�bA�oA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A��A��A��A�"�A� �A� �A��A��A��A�"�A�"�A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�VA�bA��A��A�JA�1A�%A�A�bA�VA�
=A�A���A���A���A���A��A��A��A��`A��TA��;A��;A��/A��
A���AѴ9A�~�A�=qA�A�A�
=A��A��
A���AН�AЕ�AЍPA�\)A�%A���Aϧ�Aϛ�Aϗ�Aϗ�Aϗ�AϏ\A�x�A�`BA�?}A�{A��yA��;A���AθRAήAΡ�AΕ�AΉ7A�~�A�l�A�ZA�I�A�7LA�{A�
=A��A��`A��#A���A;wA͑hA�ZA�-A��A�JA���A��
A̼jA̧�A̓uA�|�A�n�A�`BA�K�A�(�A���A˾wA�~�A�n�A�ZA�&�A���A��A��/A�ȴAʡ�A�z�A�p�A�p�A�p�A�n�A�n�A�p�A�t�A�t�A�r�A�l�A�C�A�33A�5?A�I�A�M�A�=qA�1'A�$�A�%A���A�
=A���A��/A�v�A�VA��`A��mA��A��A��TAȮAȧ�Aȣ�Aț�A�ffA�E�A� �A��A��A��#A�ȴAǰ!Aǟ�AǛ�AǕ�A�~�A�hsA�O�A�9XA�1'A�-A��A�1A���A��;A��A�
=A�1A�%A�A���A��mA�ȴA�jA�-A���A�x�A�M�A�C�A�$�A�bA��A�ƨAę�A�v�A�/A��AüjA�C�A�ƨA�\)A�?}A��A���A��/A��wA���A���A���A�Q�A���A�=qA��A��A���A�^5A�I�A�
=A��/A�ƨA��A�n�A�bA��jA�p�A���A��FA�x�A���A�|�A�VA��A��A��A�v�A�ZA�=qA�$�A���A�G�A��FA�ZA�5?A�VA�A��A��hA�A�5?A��A�A���A�{A�I�A�A��`A��!A�ffA�oA��A�XA��HA�l�A�K�A�A�A��A���A��/A���A��PA�p�A�S�A�A���A��RA���A�hsA��A��mA��DA�;dA�"�A��A���A��A�oA���A�-A��^A�`BA�bA���A���A�x�A�M�A�I�A�C�A�9XA��#A�ffA��A�G�A��-A�C�A��jA��DA��7A�~�A�|�A�dZA�-A�"�A�1A���A��A��wA��DA�n�A�dZA�bNA�ZA�;dA�bA�A��7A��A���A��A��A���A��A�VA�(�A�JA���A��A��`A�A�~�A�l�A�;dA�  A��;A��A���A�I�A�JA��yA���A���A�+A�A��yA���A�hsA�33A�bA���A��7A�M�A�A��;A�ȴA���A�bNA�{A���A���A��FA���A�t�A�dZA�\)A� �A��7A���A�O�A��jA�z�A�S�A�A�A�7LA��A��HA�~�A�bA��jA�`BA��A�l�A�&�A��yA�"�A�9XA���A�C�A�A�ĜA��\A�ZA�K�A��A��A��FA�^5A�A�A�9XA�/A�$�A�JA���A��;A�A��^A��9A���A�M�A��RA�5?A��/A��-A���A���A���A�dZA�33A��A�ȴA��A�O�A�+A�  A��;A��FA�|�A�"�A��7A���A��DA�1A��A��HA�ƨA��9A���A��A�n�A�Q�A�G�A�9XA�/A�$�A�  A��DA��TA���A�Q�A��A���A�=qA��HA�K�A���A�ƨA��A�A�A�JA��#A�r�A�{A�XAA�A~��A~(�A}��A}��A}K�A|��A|ĜA|bNA{�mA{t�A{�Az�!AzM�Ay�-Ax��Aw�;AwhsAw
=Av�uAvbAu��Atr�As�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
b�B
c B
c B
c�B
c�B
cTB
c�B
cTB
c�B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
cTB
c�B
b�B
cTB
cTB
cTB
cTB
cTB
cTB
c�B
d&B
d&B
c�B
c�B
c�B
c�B
d&B
e�B
gB
o B
��B
��B
��B
�4B
��B
��B
��B
��B
�qB
�RB
�BB($BA�BD3B=qB8�BF?B\]BT�BhsB[�BPB`�B`Bv+B�$B�OB�3B�nB��B��B�xB�B�hB�@B��B��B�kB��B��B�B��B��B��B�IB�B��B��Bu�Bh�B^�B8RB-B+BGB
�B
ںB
� B
��B
�-B
�aB
��B
� B
iB
I�B
(�B
�B	�B	��B	�wB	�B	�B	�+B	��B	|B	oiB	e�B	L�B	H�B	A B	9�B	-�B	'�B	 'B	�B	B	�B	B	B	  B�VB�AB��B��B��B�B�,B�B�B�|B�B�B�B�QB		�B	_B	xB	,=B	-�B	7B	8�B	A�B	J�B	I�B	Q�B	[�B	ZQB	^�B	XEB	S&B	S�B	T�B	S&B	V�B	Y�B	j�B	cB	��B	�+B	�SB	cB	.B	~�B	�4B	��B	�GB	��B	��B	��B	��B	��B	��B	�SB	�fB	�oB	��B	��B	��B	��B	��B	�B	��B	��B	�PB	��B	�~B	�~B	�~B	�JB	�	B	�lB	�B	��B	��B	��B	��B	�bB	��B	�(B	�hB	��B	�(B	��B	��B	��B	�B	�hB	�4B	��B	�hB	��B	��B	�FB	��B	�SB	�YB	�=B	�7B	��B	�OB	��B	�@B	��B	�zB	��B	�B	�B	��B	�B	��B	�B	�$B	��B	�kB	��B	��B	�6B	�B	�'B	�aB	��B	��B	��B	�-B	�[B	��B	�B	�zB	�RB	��B	��B	�^B	�*B	�^B	��B	��B	��B	��B	��B	�<B	�jB	�0B	�jB	�jB	�wB	��B	ĜB	ĜB	�B	ÖB	��B	�qB	��B	�dB	�<B	��B	��B	�B	�}B	��B	�<B	�B	��B	�B	�B	��B	��B	��B	� B	��B	�3B	�B	��B	�B	�zB	�B	�tB	�tB	ǮB	�KB	�B	ʌB	ɺB	ɺB	�pB	�B	�vB	��B	�B	�TB	��B	�aB	��B	�mB	�?B	ںB	�B	چB	ںB	��B	�#B	��B	�]B	�B	ߤB	�pB	�B	��B	��B	�pB	�B	�|B	�B	�B	�B	��B	��B	�B	�fB	��B	��B	��B	�QB	��B	��B	��B	�B	�/B	��B	��B	� B	�iB	�5B	� B	�cB	�;B	�B	�GB	�GB	�B	�B	��B	�B	��B	�TB	�B	�B	��B	�ZB	��B	��B	�lB	��B	�	B	�8B	�8B	�8B	��B	��B	��B	��B	�>B	�DB	��B	��B	��B	��B	�B	�B	�rB	�lB	�rB	�xB	�JB	�PB	��B	�"B	��B	��B	�PB	�"B	��B	��B	�cB
�B
�B
�B
�B
fB
fB
�B
�B
	lB
	�B
	�B

	B

	B

rB
DB
B
�B
�B
�B
PB
�B
�B
"B
B
�B
�B
\B
\B
�B
�B
�B
 B
hB
oB
oB
oB
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
SB
YB
�B
+B
_B
1B
1B
�B
�B
�B
�B
eB
B
�B
�B
�B
eB
_B
�B
1B
�B
�B
�B
1B
eB
�B
7B
B
7B
B
�B
�B
=B
qB
�B
~B
B
�B
�B
�B
�B
�B
�B
VB
!B
VB
�B
�B
�B
!�B
!�B
!�B
#nB
$@B
#�B
#:B
$tB
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&B
%�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
($B
(�B
(�B
)�B
)_B
)�B
)�B
*0B
*0B
*0B
*�B
+B
*�B
+�B
,�B
+�B
,=B
,=B
,qB
,�B
-B
-B
-CB
-wB
-CB
-�B
-�B
-�B
.B
.�B
.�B
/�B
/�B
/�B
0UB
0!B
1'B
0�B
0�B
0�B
/�B
0�B
0�B
1[B
1�B
1�B
1[B
1�B
1�B
2-B
2aB
2�B
2�B
33B
4�B
4nB
4�B
5B
4�B
4�B
5tB
5�B
6zB
7B
6�B
7LB
7B
7�B
8RB
8�B
8�B
8�B
9�B
9XB
9�B
;�B
;0B
;�B
<B
<jB
<�B
<�B
=B
<�B
=<B
=<B
=B
=�B
=qB
=�B
>B
=�B
=�B
>B
>B
>B
>wB
>wB
?HB
?B
?HB
?}B
@OB
@�B
@�B
@�B
@�B
A B
A B
A B
B'B
B[B
B�B
B�B
B�B
C-B
CaB
C�B
D�B
EB
D�B
EB
E9B
D�B
E�B
F?B
E�B
GB
GzB
GEB
G�B
GB
GzB
D�B
C�B
C�B
C�B
EB
D3B
D3B
D�B
C�B
D�B
D�B
EB
F?B
F?B
F�B
E�B
F?B
GB
G�B
GzB
G�B
H�B
HB
GzB
H�B
I�B
K�B
LdB
NB
NpB
MjB
L�B
L�B
NB
L�B
MB
M�B
NB
M�B
NB
N�B
N<B
N<B
N�B
NpB
N<B
N�B
OBB
PB
O�B
O�B
P�B
Q�B
QNB
Q�B
Q�B
R�B
R�B
R�B
S&B
S[B
S[B
S�B
S�B
S�B
T�B
T,B
T,B
U�B
VB
V9B
W
B
V�B
XyB
XB
YB
X�B
YB
YB
Y�B
YKB
YB
YB
YB
Y�B
Z�B
ZB
Z�B
Z�B
YB
Z�B
Z�B
Z�B
Z�B
ZQB
ZB
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
]/B
]�B
]dB
]�B
]/B
]�B
^5B
_;B
_pB
_�B
`�B
`vB
_�B
`vB
`vB
`B
`vB
`�B
aB
a�B
bB
bNB
bB
b�B
a�B
a|B
bNB
b�B
cTB
c B
c�B
d&B
e`B
f�B
e�B
e�B
f�B
e,B
f2B
f2B
d�B
e�B
d�B
f�B
e�B
e�B
e�B
e�B
f�B
gmB
h
B
h
B
hsB
iB
hsB
h�B
iB
j�B
kQB
k�B
l"B
l�B
lWB
lWB
l�B
l�B
l�B
m]B
m�B
m�B
m]B
m�B
m�B
m�B
m�B
m�B
n�B
o B
o5B
n�B
n�B
o B
oiB
o B
o�B
o B
oiB
p;B
pB
pB
p;B
p;B
p�B
qAB
p�B
qAB
rB
qvB
q�B
q�B
rGB
rGB
r�B
s�B
sMB
s�B
sB
tB
s�B
tB
t�B
u%B
u%B
t�B
u%B
t�B
u%B
uZB
u�B
u�B
v`B
v+B
v�B
v�B
v+B
v�B
v�B
v�B
w�B
xlB
xlB
y	B
y	B
yrB
x�B
y�B
y>B
zxB
zxB
{B
z�B
{JB
{B
{�B
{�B
|PB
|�B
|�B
}"B
|�B
|�B
|�B
}"B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~]B
~�B
.B
�B
cB
�B
�4B
�B
�B
�;B
�B
�oB
�B
�B
�AB
��B
�uB
��B
��B
�B
�B
��B
��B
�{B
��B
��B
��B
�MB
��B
��B
��B
��B
��B
�SB
��B
��B
��B
��B
�_B
�+B
��B
�_B
��B
��B
�_B
��B
�+B
��B
��B
��B
�fB
��B
�1B
�fB
��B
�lB
��B
��B
��B
�	B
�=B
�=B
��B
�B
��B
��B
�B
�~B
�~B
��B
�PB
��B
a�B
c B
b�B
bNB
bB
b�B
c�B
dZB
c�B
b�B
`BB
b�B
c�B
d&B
cTB
d�B
b�B
b�B
b�B
dZB
e,B
d&B
d�B
c�B
bB
c B
c�B
c�B
dZB
dZB
d�B
c�B
cTB
b�B
b�B
cTB
e`B
d&B
d�B
d&B
c�B
c B
b�B
b�B
b�B
c�B
cTB
e`B
c�B
c�B
cTB
b�B
b�B
c�B
d�B
d&B
e,B
d�B
c�B
cTB
bB
b�B
bB
c B
c�B
c B
c�B
a|B
b�B
c�B
dZB
e�B
d&B
d�B
a�B
a|B
b�B
bNB
b�B
d�B
d�B
b�B
c B
b�B
b�B
b�B
d�B
d&B
c�B
c�B
b�B
b�B
a�B
b�B
bNB
c B
c�B
cTB
c�B
c B
bB
bB
b�B
cTB
d&B
dZB
dZB
c�B
c�B
b�B
aHB
bB
b�B
c�B
dZB
dZB
dZB
c�B
c B
b�B
bB
b�B
c�B
dZB
d�B
d�B
dZB
c B
b�B
b�B
cTB
dZB
d�B
d�B
d�B
c�B
cTB
c B
c B
c�B
d�B
d�B
d�B
c�B
c B
cTB
cTB
cTB
dZB
dZB
d�B
d�B
b�B
c B
b�B
b�B
c�B
d�B
d&B
b�B
b�B
c B
c�B
dZB
dZB
b�B
b�B
c B
d�B
dZB
e,B
dZB
d&B
c�B
b�B
cTB
c�B
d�B
d&B
d�B
c�B
e,B
e�B
e�B
b�B
c�B
g�B
e,B
e�B
e,B
b�B
e�B
f2B
hsB
e�B
e,B
ffB
g�B
g8B
f�B
f2B
g�B
g�B
hsB
h>B
h>B
iB
iB
m]B
z�B
�B
zB
��B
�lB
��B
�B
�bB
�4B
�oB
��B
��B
�!B
�-B
�'B
�'B
��B
��B
��B
��B
��B
�B
�LB
�FB
�'B
��B
��B
��B
��B
��B
�4B
��B
��B
��B
�nB
�B
��B
�tB
�zB
�zB
��B
��B
��B
�B
�IB
�B
��B
��B
��B
�-B
�'B
��B
�MB
��B
��B
��B
�B
��B
��B
�kB
�!B
��B
��B
��B
��B
��B
�LB
�_B
�IB
��B
��B
�aB
�FB
�B
�$B
��B
��B
��B
��B
�jB
�B
�KB
�#B
�<B
��B
�B
�MB�B�BoB�B�BB4B$�B�B�B)�B49B=�B>wB=�B>wBAUBJXBEmBG�BDgBB'B@�BA�B@�B?�B<6B=<B=B=<B:�B7B4�B8�B=<B8B<�B>wB>�BI�BK�BLdBOvBT�BW�B[�Be,B^�B`vBO�BI�BL�BX�B]/Ba|Bb�Bd�BiBm�Bf�Bg�BxlBg�BQ�BO�BPHBM�BOBOBPBM�BM�B]�BffBb�BXB[�B^�B^5BY�B`BB`BB`BaBbNBp�Bm]Bt�B�4B}�B�{B��B�.B��B��B�'B�B��B��B��B�B�B�qB�^B��B��B��B��B��B��B��B��B�B��B��B�aB��B��B�*B�wB�UB��B�LB��B�CB�B��B�{B�lB��B��B�"B�B�	B��B��B��B��B�!B��B�B�nB�B�B�-B�VB�'B�9B��B��B��B�*B�=B�aB��B��B�tB�@B�!B�~B�OB��B��B��B��B��B�^B��B��B�6B�kB�kB��B�CB�_B��B��B��B�B�}B��B�B��B�}B�3B�OB�3B��B��B�bB�tB�zB�B��B��B��B�6B��B�9B�B�^B�B�B��B�RB��B�qB�B��B�\B�SB��B�B��B�~B�(B��B��B��B��B�bB��BcB.Bv`BuZBq�Bx8BqABx�By�Bd�B`BB[�BW�BW�B]�BcTBn�B]/BOB<jB5tB0�B-�B,�B1�B4�B.�B%B$�B �B�BPBDB,B 4BAB
�]B
��B
�B
�B
�B
ޞB
� B
�B
�B
�pB
՛B
�[B
�[B
�TB
��B
�vB
бB
��B
ʌB
��B
�jB
�,B
��B
خB
��B
��B
��B
��B
��B
��B
�aB
�}B
��B
�=B
��B
��B
��B
��B
��B
�	B
��B
��B
�kB
��B
��B
yrB
u�B
v�B
sB
r�B
p�B
p;B
m]B
hsB
gmB
dZB
a�B
ffB
o B
ncB
S�B
W�B
WsB
I�B
LdB
HB
N�B
;dB
5B
5�B
33B
-CB
($B
+�B
/�B
+kB
 �B
{B
uB
VB
	�B
�B
�B
�B	��B
SB	��B	��B	�2B	�+B	�B	�8B	��B	��B	�B	�B	�B	�)B	�B	�B	�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 B
Z�B
[,B
[,B
[�B
[�B
[`B
[�B
[`B
[�B
[`B
[�B
[�B
[�B
[�B
[�B
[�B
[`B
[�B
Z�B
[`B
[`B
[`B
[`B
[`B
[`B
[�B
\2B
\2B
[�B
[�B
[�B
[�B
\2B
]�B
_B
gB
��B
��B
��B
�@B
��B
��B
��B
��B
�}B
�^B
� B
�+B 0B9�B<?B5}B0�B>KBTiBL�B`BTBH BX�BXBn7B�0B�[B�?B�zB�B��B��B�*B�tB�LB��B��B�wB�B��B�B��B��B��B�UB�$B��B�BnB`�BV�B0^B%B7B
�SB
��B
��B
�,B
��B
�9B
�mB
��B
xB
aB
A�B
 �B	��B	�B	��B	��B	�B	�*B	�7B	��B	t(B	guB	]�B	D�B	@�B	9,B	1�B	%�B	�B	3B	�B	B	�B	B�+B�B�bB�MB��B�B��BܛB�8B��B��BوBضBܛB�B�]B	�B	kB	�B	$IB	%�B	/#B	0�B	9�B	B�B	A�B	I�B	S�B	R]B	V�B	PQB	K2B	K�B	M
B	K2B	N�B	Q�B	b�B	woB	}�B	7B	}_B	woB	w:B	v�B	x@B	x�B	{SB	~�B	}�B	�B	~�B	{�B	{�B	}_B	�rB	y{B	|�B	��B	��B	��B	��B	�!B	��B	��B	�\B	��B	��B	��B	��B	�VB	�B	�xB	�'B	��B	��B	�B	��B	�nB	��B	�4B	�tB	��B	�4B	��B	��B	��B	�B	�tB	�@B	��B	�tB	��B	��B	�RB	��B	�_B	�eB	�IB	�CB	��B	�[B	��B	�LB	��B	��B	��B	�B	�B	��B	�*B	��B	�*B	�0B	��B	�wB	��B	��B	�BB	�B	�3B	�mB	��B	��B	��B	�9B	�gB	��B	�B	��B	�^B	�B	��B	�jB	�6B	�jB	��B	�B	��B	��B	��B	�HB	�vB	�<B	�vB	�vB	��B	��B	��B	��B	�B	��B	��B	�}B	��B	�pB	�HB	��B	��B	�B	��B	��B	�HB	�B	��B	�B	�B	��B	��B	��B	�,B	�B	�?B	�B	��B	�#B	��B	�B	��B	��B	��B	�WB	�)B	B	��B	��B	�|B	�B	ǂB	��B	�&B	�`B	��B	�mB	�
B	�yB	�KB	��B	�)B	ҒB	��B	��B	�/B	��B	�iB	�B	װB	�|B	�B	��B	��B	�|B	�B	وB	�B	�B	�B	��B	�
B	ݡB	�rB	��B	��B	��B	�]B	��B	� B	� B	�B	�;B	�B	�B	�B	�uB	�AB	�B	�oB	�GB	�B	�SB	�SB	�B	�B	��B	�B	��B	�`B	�B	��B	��B	�fB	�B	�B	�xB	��B	�B	�DB	�DB	�DB	�B	��B	�B	�B	�JB	�PB	�B	��B	��B	��B	�(B	�B	�~B	�xB	�~B	�B	�VB	�\B	��B	�.B	��B	��B	�\B	�.B	��B	��B	�oB	��B	��B	��B
 	B
 rB
 rB
 	B
 �B
xB
�B
�B
B
B
~B
PB
!B
�B
�B
�B
\B
�B
�B
.B
'B
�B
�B
hB
hB
�B
�B
�B
	B
	tB

{B

{B

{B

�B

�B
�B
�B
�B
�B
B
�B
�B
�B
_B
eB
�B
7B
kB
=B
=B
�B
�B
�B
�B
qB
B
�B
�B
B
qB
kB
�B
=B
B
�B
B
=B
qB
�B
CB
B
CB
B
�B
�B
IB
}B
�B
�B
'B
�B
�B
�B
�B
�B
�B
bB
-B
bB
�B
�B
�B
�B
�B
B
zB
LB
�B
FB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 0B
 �B
!B
!�B
!kB
!�B
"B
"<B
"<B
"<B
"�B
#B
"�B
#�B
$�B
#�B
$IB
$IB
$}B
$�B
%B
%B
%OB
%�B
%OB
%�B
%�B
%�B
& B
&�B
&�B
'�B
'�B
'�B
(aB
(-B
)3B
(�B
(�B
(�B
'�B
(�B
(�B
)gB
)�B
)�B
)gB
)�B
*B
*9B
*mB
*�B
+B
+?B
,�B
,zB
,�B
-B
,�B
,�B
-�B
-�B
.�B
/#B
.�B
/XB
/#B
/�B
0^B
0�B
0�B
0�B
1�B
1dB
2B
3�B
3<B
3�B
4B
4vB
4�B
4�B
5B
4�B
5HB
5HB
5B
5�B
5}B
5�B
6B
5�B
5�B
6B
6B
6B
6�B
6�B
7TB
7 B
7TB
7�B
8[B
8�B
8�B
8�B
8�B
9,B
9,B
9,B
:3B
:gB
:�B
;B
;B
;9B
;mB
<
B
<�B
=B
<�B
=B
=EB
<�B
=�B
>KB
=�B
?B
?�B
?QB
?�B
?B
?�B
<�B
;�B
;�B
<
B
=B
<?B
<?B
<�B
;�B
<�B
<�B
=B
>KB
>KB
>�B
=�B
>KB
?B
?�B
?�B
?�B
@�B
@#B
?�B
@�B
A�B
DB
DpB
FB
F|B
EvB
D�B
D�B
FB
D�B
EB
E�B
FB
E�B
FB
F�B
FHB
FHB
F�B
F|B
FHB
F�B
GNB
H B
G�B
G�B
H�B
I�B
IZB
I�B
I�B
J�B
J�B
J�B
K2B
KgB
KgB
LB
K�B
K�B
L�B
L8B
L8B
M�B
NB
NEB
OB
N�B
P�B
PB
Q#B
P�B
Q�B
Q�B
Q�B
QWB
Q#B
Q�B
Q�B
Q�B
R�B
R)B
R�B
R�B
Q�B
R�B
R�B
R�B
R�B
R]B
R)B
R)B
R�B
R�B
S�B
TB
TB
TB
T�B
T�B
T�B
T�B
U;B
U�B
UpB
U�B
U;B
U�B
VAB
WGB
W|B
W�B
X�B
X�B
W�B
X�B
X�B
XB
X�B
X�B
YB
Y�B
Z%B
ZZB
Z%B
Z�B
Y�B
Y�B
ZZB
Z�B
[`B
[,B
[�B
\2B
]lB
^�B
]�B
]�B
^�B
]8B
^>B
^>B
]B
]�B
]B
^�B
]�B
]�B
]�B
^
B
^�B
_yB
`B
`B
`B
aB
`B
`�B
aB
b�B
c]B
c�B
d.B
d�B
dcB
dcB
e B
d�B
d�B
eiB
fB
e�B
eiB
e�B
fB
e�B
e�B
e�B
f�B
gB
gAB
f�B
f�B
gB
guB
gB
g�B
gB
guB
hGB
hB
hB
hGB
hGB
h�B
iMB
h�B
iMB
jB
i�B
i�B
i�B
jSB
jSB
j�B
k�B
kYB
k�B
k%B
l+B
k�B
l+B
l�B
m1B
m1B
l�B
m1B
l�B
m1B
mfB
m�B
m�B
nlB
n7B
o	B
n�B
n7B
n�B
n�B
o	B
o�B
pxB
pxB
qB
qB
q~B
p�B
q�B
qJB
r�B
r�B
s"B
r�B
sVB
s�B
s�B
s�B
t\B
t�B
t�B
u.B
t�B
t�B
t�B
u.B
u.B
u.B
u�B
u�B
u�B
u�B
v B
u�B
v B
viB
viB
v�B
w:B
w�B
woB
w�B
x@B
yB
yB
yGB
yB
y{B
zB
zB
zMB
y�B
z�B
z�B
z�B
{B
{B
z�B
{�B
{�B
{�B
{�B
{�B
|YB
|�B
|�B
|�B
}�B
|�B
}_B
}�B
}�B
~�B
~�B
kB
7B
�B
kB
�B
�	B
kB
�	B
7B
�B
�	B
�	B
�rB
�B
�=B
�rB
��B
�xB
��B
��B
��B
�B
�IB
�IB
��B
�B
��B
��B
�!B
��B
��B
��B
�\B
��B
Y�B
[,B
Z�B
ZZB
Z%B
Z�B
[�B
\fB
[�B
Z�B
XNB
Z�B
[�B
\2B
[`B
\�B
Z�B
Z�B
Z�B
\fB
]8B
\2B
\�B
[�B
Z%B
[,B
[�B
[�B
\fB
\fB
\�B
[�B
[`B
Z�B
Z�B
[`B
]lB
\2B
\�B
\2B
[�B
[,B
Z�B
Z�B
Z�B
[�B
[`B
]lB
[�B
[�B
[`B
Z�B
Z�B
[�B
\�B
\2B
]8B
\�B
[�B
[`B
Z%B
Z�B
Z%B
[,B
[�B
[,B
[�B
Y�B
Z�B
[�B
\fB
]�B
\2B
\�B
Y�B
Y�B
Z�B
ZZB
Z�B
\�B
\�B
Z�B
[,B
Z�B
Z�B
Z�B
\�B
\2B
[�B
[�B
Z�B
Z�B
Y�B
Z�B
ZZB
[,B
[�B
[`B
[�B
[,B
Z%B
Z%B
Z�B
[`B
\2B
\fB
\fB
[�B
[�B
Z�B
YTB
Z%B
Z�B
[�B
\fB
\fB
\fB
[�B
[,B
Z�B
Z%B
Z�B
[�B
\fB
\�B
\�B
\fB
[,B
Z�B
Z�B
[`B
\fB
]B
\�B
]B
[�B
[`B
[,B
[,B
[�B
]B
]B
\�B
[�B
[,B
[`B
[`B
[`B
\fB
\fB
]B
\�B
Z�B
[,B
Z�B
Z�B
[�B
\�B
\2B
Z�B
Z�B
[,B
[�B
\fB
\fB
Z�B
Z�B
[,B
\�B
\fB
]8B
\fB
\2B
[�B
Z�B
[`B
[�B
\�B
\2B
\�B
[�B
]8B
]�B
]�B
Z�B
[�B
_�B
]8B
^
B
]8B
Z�B
]�B
^>B
`B
]�B
]8B
^rB
_�B
_DB
^�B
^>B
_�B
_�B
`B
`JB
`JB
aB
aB
eiB
r�B
|%B
rB
�	B
�xB
��B
�!B
�nB
�@B
�{B
��B
��B
�-B
�9B
�3B
�3B
��B
��B
��B
��B
��B
�B
�XB
�RB
�3B
��B
��B
��B
�B
��B
�@B
�B
��B
��B
�zB
�B
��B
��B
��B
��B
��B
��B
��B
�B
�UB
�B
�B
��B
��B
�9B
�3B
��B
�YB
��B
��B
��B
�B
��B
��B
�wB
�-B
��B
��B
�B
�B
��B
�XB
�kB
�UB
��B
��B
�mB
�RB
�)B
�0B
��B
��B
��B
��B
�vB
�#B
�WB
�/B
�HB
��B
�B
�YB
��B
��B
�{B
��B
�BB,B�B�B�B!�B,EB5�B6�B5�B6�B9aBBdB=yB?�B<sB:3B8�B9�B8�B7�B4BB5HB5B5HB2�B/#B,�B0�B5HB0)B4�B6�B6�BA�BC�BDpBG�BM
BO�BS�B]8BV�BX�BG�BA�BD�BP�BU;BY�BZ�B\�BaBfB^�B_�BpxB_�BI�BG�BHTBE�BGBGBH BE�BE�BU�B^rBZ�BPBS�BV�BVABQ�BXNBXNBXBYBZZBh�BeiBl�Bx@Bu�B{�B��B�:B��B��B�3B�B��B��B��B�!B�B�}B�jB��B��B��B��B��B��B��B��B�B��B��B�mB��B��B�6B��B�aB��B�XB��B�OB�By�B{�B�xB��B��B�.B�B�B��B��B��B��B�-B��B�$B�zB�B�$B�9B�bB�3B�EB��B��B��B�6B�IB�mB�B��B��B�LB�-B��B�[B��B��B��B�
B��B�jB��B��B�BB�wB�wB��B�OB�kB��B��B��B�'B��B��B�'B��B��B�?B�[B�?B��B��B�nB��B��B�B��B��B��B�BB��B�EB�B�jB�B� B��B�^B��B�}B�B��B�hB�_B��B�B��B��B�4B��B��B��B{�B�nB{�BwoBw:BnlBmfBi�BpDBiMBp�Bq�B\�BXNBS�BO�BO�BU�B[`Bf�BU;BGB4vB-�B(�B%�B$�B*B,�B&�BB�B�B�B\BPB$B
�@B
�MB
�iB
�B
߭B
�"B
�B
֪B
�,B
�B
�B
�|B
ͧB
�gB
�gB
�`B
��B
ǂB
ȽB
��B
B
��B
�vB
�8B
�B
кB
��B
��B
��B
��B
��B
��B
�mB
��B
��B
�IB
��B
��B
��B
��B
��B
�B
��B
��B
�wB
�	B
|�B
q~B
m�B
n�B
k%B
j�B
h�B
hGB
eiB
`B
_yB
\fB
Y�B
^rB
gB
foB
LB
O�B
OB
A�B
DpB
@#B
F�B
3pB
-B
-�B
+?B
%OB
 0B
#�B
'�B
#wB
�B
�B
�B
bB
�B	��B	��B	��B	��B	�_B	� B	��B	�>B	�7B	�B	�DB	��B	�	B	ݡB	�B	ܛB	�5B	�B	߭B	�`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230620100045                            20230620100045AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023062010004520230620100045  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023062010004520230620100045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023062010004520230620100045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               