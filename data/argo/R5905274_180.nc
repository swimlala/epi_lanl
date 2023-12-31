CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:18Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223218  20230426223218  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�� ���@�� ���11  @��O�@@��O�@@/�G0@9�@/�G0@9��dH��{�dH��{11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?u@   @B�\@�G�@�G�@\@�G�A   A  A   A+�A@  A`��A�Q�A��A�\)A��RA�\)A�  A�Q�A��B ��BQ�B  B  B Q�B'�
B/�
B8(�B@  BG�
BP  BX(�B`(�Bh(�Bp(�Bx(�B�{B�(�B�{B�{B�(�B�{B��B�  B�  B�  B��B��B�{B�  B�  B�{B�  B��
B��
B��B��B�  B�  B�(�B�  B��
B�{B�{B��B��B��B�  C 
=C
=C��C��C
=C
{C��C�HC  C  C  C
=C  C�C{C{C 
=C"  C$  C&  C(  C*  C,  C.
=C0
=C2
=C4
=C6  C8
=C9��C;��C>
=C@{CB
=CD
=CF
=CH  CJ  CK��CN  CP  CR  CT  CU��CX  CZ
=C\{C^
=C`
=Cb  Cd  Cf{Ch  Ci�Ck��Cn  Cp  Cq��Ct  Cv
=Cx  Cz
=C|  C}�C�C�  C���C�C�  C�C�C���C�C�  C���C�  C�  C�  C�
=C�  C���C���C���C�  C�C�C�  C�C�  C�  C�  C�C�
=C�  C�  C���C���C�  C�  C�  C�C�  C�  C�
=C�
=C�
=C�C���C���C�C�C�C���C���C���C�C�  C���C���C���C�  C�
=C�
=C�
=C�
=C�  C���C�  C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C�  C�  C���C��C���C�  C�C�  C��C���C�  C�  C�  C�  C�C�C�C�C�C�C�C�  C�C���C���C���C�C�C�C�
=C�\C�
=C�
=C�
=C�
=C�C�C�  C�  C���C�  C�  C���C�C�C�C�C�D �D � D�D�DD��D�qD}qD  D� D�qD� D  D� D�qD� DD��D�qD	}qD	��D
z�D
��Dz�D  D��D�D��D�D� D  D}qD�qD� D�D� D  D� D  D}qD�RDz�D�qD}qD�qD� DD� D�qD� D�qD� D  D}qD�qDz�D�RDz�D  Dz�D�qD� D�D� D�RD xRD �RD!xRD!��D"z�D#  D#��D$�D$�D$�qD%� D&�D&}qD&��D'� D(�D(��D)D)� D)�qD*� D+  D+}qD,D,��D-  D-z�D-�qD.�D/�D/�D0  D0��D1D1�D2�D2��D3  D3}qD4  D4��D5D5�=D6  D6xRD7  D7�D8D8z�D8�qD9��D:  D:��D;�D;}qD;�qD<��D<�qD=�D>�D>}qD>�qD?}qD?�qD@}qD@�qDA��DA�qDB}qDC�DC�DD�DD��DE�DE}qDE��DF��DGDG�DHDH�DH�qDIz�DJ  DJ��DK  DK� DK��DLz�DM  DM��DN�DN� DN�qDO}qDP  DP}qDP�qDQ}qDR�DR� DSDS��DT�DT� DU�DU� DV  DV}qDW  DW� DX�DX��DY�DY� DZ�DZ��D[�D[� D\�D\�D]�D]}qD]�qD^��D_�D_��D`  D`� D`�qDa}qDa�qDb}qDc  Dc��DdDd�De  De}qDf  Df� Dg�Dg��DhDh� Dh�qDi}qDi�qDj}qDkDk��Dl  Dl��Dl��Dm�Dm�qDn� Dn��Do� Do�qDp��Dq  Dq�Dq�qDr}qDs  Ds� DtDt}qDu�Du}qDv  Dv}qDw  Dwz�Dx  Dx��Dx�qDy� Dy��Dzz�D{  D{��D|D|�D}D}� D}��D~��D�Dz�D�  D�AHD�� D���D�  D�B�D�~�D�D�HD�@ D�~�D���D�  D�@ D��HD�� D���D�AHD��HD�� D�HD�@ D�~�D���D��qD�@ D�� D�� D���D�AHD���D��HD�  D�AHD�� D���D��qD�>�D�}qD��HD�  D�=qD�~�D��HD�  D�<)D�~�D�� D�HD�AHD��HD�� D�HD�>�D�~�D�� D�HD�<)D�|)D�� D��D�>�D�~�D�� D�HD�@ D��HD���D���D�@ D��HD��HD�  D�@ D��HD�� D���D�AHD�� D���D���D�>�D�}qD���D�  D�@ D��HD�� D��qD�>�D��HD�� D���D�=qD�� D�� D���D�@ D�~�D��HD��D�AHD�~�D���D�  D�>�D�� D��HD�  D�>�D��HD�� D�HD�AHD�� D��HD�HD�@ D��HD�� D�  D�@ D�~�D��qD�  D�@ D�� D�D�HD�AHD���D�� D���D�B�D�� D��HD�HD�@ D�~�D���D�  D�AHD���D��HD�HD�@ D�� D�� D�HD�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�HD�>�D��HD��HD���D�>�D�}qD�� D�HD�AHD�}qD�� D�HD�@ D�� D���D��)D�>�D�� D��HD��D�@ D�~�D�� D���D�AHD�� D���D���D�>�D�� D���D��qD�@ D��HD���D��)D�AHD���D�D�  D�@ D�~�D�� D��qD�>�D�}qD���D���D�@ D��HD�� D��D�AHD���D�D�  D�@ D��HD���D��qD�>�D�� D��HD�HD�@ D��HD�� D�HD�>�D�}qD��qD���D�>�D�� D��HD��D�B�D�� D���D�HD�AHD���D���D�HD�@ D�� D�� D���D�>�D�}qD�� D��D�B�D��HD��HD�  D�B�D��HD��HD��D�AHD�� D���D���D�>�DHD¾�D�  D�>�D�~�Dþ�D���D�@ D�~�D�� D�  D�=qDŀ D�D��D�AHDƁHD�� D�HD�AHDǂ�D��HD��)D�@ DȁHDȾ�D���D�@ Dɀ Dɾ�D�HD�@ Dʀ D��HD�HD�>�Dˀ D��HD�  D�>�D́HD�� D�  D�AHD̀ D��HD�HD�C�D΁HD�� D�HD�@ D�~�DϾ�D�  D�@ DЂ�D�� D�  D�@ D�~�DѾ�D�  D�>�D�~�D�� D�  D�@ DӁHD��HD��D�@ DԀ D�� D�  D�@ D�~�D�� D�  D�>�Dր D��HD�  D�AHDׁHD�� D�  D�AHD؁HD��HD�  D�@ DفHD�� D�  D�@ D�~�D�� D�  D�AHDہHD�� D���D�@ D�~�D�� D�  D�@ D�~�Dݾ�D�  D�@ D�~�D�� D�  D�>�D߀ D�� D���D�>�D�� D�� D�HD�@ D�~�D��HD�  D�@ D�~�D��HD��D�@ D�HD�� D���D�AHD� D��HD�  D�>�D� D�� D�HD�@ D�HD��HD�  D�@ D� D羸D�HD�AHD� D辸D���D�AHD� D�D���D�>�D�~�D�qD���D�>�D�~�D�qD�HD�@ D� D�D�  D�>�D�HD��HD��D�@ D�~�D�� D�  D�@ D� D�� D�HD�>�D�� D�D�  D�AHD� D�qD�  D�@ D�~�D�� D���D�@ D�D��HD�  D�@ D�}qD���D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�j=>�?k�?���?���?��H@z�@.{@B�\@Y��@s33@�ff@�33@�G�@���@��H@���@�Q�@��@�z�A ��A
=A{Az�A(�A!�A(Q�A.{A5�A;�AB�\AH��AQG�AY��A_\)AfffAl��Ar�\Ay��A�  A��A��RA�=qA�A�G�A�p�A���A�z�A��A��\A�A���A���A�  A�(�A�  A�33A�
=A���A���A�  A˅AθRA�=qAָRA��A�p�A���A�(�A�RA�=qA�p�A�G�A�z�A�  A�33A��RBp�B33B��B�HB��B
{B�BG�B�HBz�BffBQ�B{B�B�B�\B�
B��B=qB�B ��B!�B#
=B$Q�B%p�B&�\B'�B(z�B)p�B*=qB+\)B,(�B-G�B.=qB/\)B0Q�B1p�B2ffB3\)B4Q�B5p�B6ffB7�B8��B9�B:�RB;�B<��B=��B>�RB?�B@��BAp�BB=qBC33BD(�BE�BF{BG
=BH(�BIG�BJffBK�BLQ�BMp�BNffBO�BP(�BP��BQBR�RBS�BT��BUp�BV�\BW�BX��BY�BZ�RB[�
B\��B]B^�RB_�B`z�Ba�Bb{Bc
=Bc�
Bd��Be�Bg33Bh(�Bh��Bi�Bj�HBk�
Blz�Bm��Bn=qBo�Bp  Bp��Bq�Br�RBs�Bt��BuBv�RBw�Bx��By��By�Bz�HB{�
B|��B}��B~�\B�B�z�B���B�\)B��
B�Q�B���B���B��B��
B�Q�B���B�\)B��B�Q�B���B�G�B�B�  B�z�B���B�\)B��
B�Q�B���B�\)B��
B�Q�B��HB�G�B��B�  B�ffB��HB�\)B��
B�Q�B���B�p�B��B�ffB��HB��B���B�{B��\B���B���B�(�B���B��B���B��B�Q�B���B�33B��B�ffB��HB�\)B��
B�ffB���B�33B��B�  B�z�B���B���B�{B��\B�
=B��B�B�=qB���B�33B���B�{B��\B�G�B�B�=qB��RB��B��B��B�ffB��HB�\)B�{B��\B��B�\)B��
B�Q�B���B�p�B�  B��\B���B�p�B�B�=qB���B�p�B�  B�Q�B��RB��B��
B�ffB��HB�p�B�B�(�B���B��B�B�=qB���B��B���B��B�ffB���B�G�B��B�ffB���B��B���B��B�z�B��B��B��B�(�B��\B�G�B��B�(�B�ffB��HB�\)B�B�Q�B���BŅB�{BƏ\B��HB�p�B�=qB���B�p�B�B�Q�B�
=BˮB�=qB̸RB�G�B�(�B���B�G�B��
BЏ\B�G�B��
B�Q�B���BӮB�=qBԣ�B�33B�B�z�B��B�p�B�{BظRB�p�B�  B�z�B�33B��B܏\B�
=B�B�z�B��HB߅B�ffB�
=B�B�{B���B㙚B�{B�RB�B�=qB���B�\)B�(�B��HB�G�B��B�RB�p�B�B�ffB�33B��
B�=qB���B�B�ffB�RB�p�B�(�B��B�33B�{B��RB��B�B��\B�33B��B�ffB��B��B�=qB�
=B���B�=qB���B��B�=qB���B��C (�C ffC �
C33Cp�C�
C33CffC�RC33C�CC
=C�C��C
=Cz�C�
C
=C�C�HC{CffC�HC{Cp�C�HC	(�C	p�C	�HC
(�C
z�C
�C(�C�C��C33C��C  C=qC��C
=CQ�C��C�C\)C�RC(�Cp�C��CG�C�C�HC\)C�\C  Cp�C��C�Cz�C�RC(�Cp�C�RC33Cz�C�
CG�C�C�CQ�C�\C
=CG�C��C{CQ�CC33Cp�C�HCG�C�C  CG�C�C(�CffC�HC=qC�C   C =qC ��C!�C!\)C!�
C"(�C"z�C"��C#33C#�C#��C$Q�C$��C%
=C%�C%��C&=qC&�\C&�HC'\)C'��C({C(�C(��C)G�C)�\C*
=C*\)C*�C+33C+z�C+��C,Q�C,��C-�C-ffC-�HC.(�C.��C.��C/\)C/��C0{C0�\C0�
C1Q�C1�C2  C2z�C2C3G�C3z�C4  C4=qC4C5  C5z�C5�HC6(�C6��C6�C7p�C7�C8(�C8z�C8��C9=qC9�RC:  C:z�C:C;=qC;�C<
=C<G�C<C={C=z�C=�
C>=qC>��C>�HC?ffC?��C@
=C@=qC@�RC@�
CAG�CAffCA�RCA�HCB=qCBQ�CB��CBCC
=CC(�CCffCC�\CC�
CC�CD33CDQ�CD��CD�CE  CE�CEffCE�CE��CE�CF=qCFQ�CF��CFCG
=CG�CGp�CG�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     ?u@   @B�\@�G�@�G�@\@�G�A   A  A   A+�A@  A`��A�Q�A��A�\)A��RA�\)A�  A�Q�A��B ��BQ�B  B  B Q�B'�
B/�
B8(�B@  BG�
BP  BX(�B`(�Bh(�Bp(�Bx(�B�{B�(�B�{B�{B�(�B�{B��B�  B�  B�  B��B��B�{B�  B�  B�{B�  B��
B��
B��B��B�  B�  B�(�B�  B��
B�{B�{B��B��B��B�  C 
=C
=C��C��C
=C
{C��C�HC  C  C  C
=C  C�C{C{C 
=C"  C$  C&  C(  C*  C,  C.
=C0
=C2
=C4
=C6  C8
=C9��C;��C>
=C@{CB
=CD
=CF
=CH  CJ  CK��CN  CP  CR  CT  CU��CX  CZ
=C\{C^
=C`
=Cb  Cd  Cf{Ch  Ci�Ck��Cn  Cp  Cq��Ct  Cv
=Cx  Cz
=C|  C}�C�C�  C���C�C�  C�C�C���C�C�  C���C�  C�  C�  C�
=C�  C���C���C���C�  C�C�C�  C�C�  C�  C�  C�C�
=C�  C�  C���C���C�  C�  C�  C�C�  C�  C�
=C�
=C�
=C�C���C���C�C�C�C���C���C���C�C�  C���C���C���C�  C�
=C�
=C�
=C�
=C�  C���C�  C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C�  C�  C���C��C���C�  C�C�  C��C���C�  C�  C�  C�  C�C�C�C�C�C�C�C�  C�C���C���C���C�C�C�C�
=C�\C�
=C�
=C�
=C�
=C�C�C�  C�  C���C�  C�  C���C�C�C�C�C�D �D � D�D�DD��D�qD}qD  D� D�qD� D  D� D�qD� DD��D�qD	}qD	��D
z�D
��Dz�D  D��D�D��D�D� D  D}qD�qD� D�D� D  D� D  D}qD�RDz�D�qD}qD�qD� DD� D�qD� D�qD� D  D}qD�qDz�D�RDz�D  Dz�D�qD� D�D� D�RD xRD �RD!xRD!��D"z�D#  D#��D$�D$�D$�qD%� D&�D&}qD&��D'� D(�D(��D)D)� D)�qD*� D+  D+}qD,D,��D-  D-z�D-�qD.�D/�D/�D0  D0��D1D1�D2�D2��D3  D3}qD4  D4��D5D5�=D6  D6xRD7  D7�D8D8z�D8�qD9��D:  D:��D;�D;}qD;�qD<��D<�qD=�D>�D>}qD>�qD?}qD?�qD@}qD@�qDA��DA�qDB}qDC�DC�DD�DD��DE�DE}qDE��DF��DGDG�DHDH�DH�qDIz�DJ  DJ��DK  DK� DK��DLz�DM  DM��DN�DN� DN�qDO}qDP  DP}qDP�qDQ}qDR�DR� DSDS��DT�DT� DU�DU� DV  DV}qDW  DW� DX�DX��DY�DY� DZ�DZ��D[�D[� D\�D\�D]�D]}qD]�qD^��D_�D_��D`  D`� D`�qDa}qDa�qDb}qDc  Dc��DdDd�De  De}qDf  Df� Dg�Dg��DhDh� Dh�qDi}qDi�qDj}qDkDk��Dl  Dl��Dl��Dm�Dm�qDn� Dn��Do� Do�qDp��Dq  Dq�Dq�qDr}qDs  Ds� DtDt}qDu�Du}qDv  Dv}qDw  Dwz�Dx  Dx��Dx�qDy� Dy��Dzz�D{  D{��D|D|�D}D}� D}��D~��D�Dz�D�  D�AHD�� D���D�  D�B�D�~�D�D�HD�@ D�~�D���D�  D�@ D��HD�� D���D�AHD��HD�� D�HD�@ D�~�D���D��qD�@ D�� D�� D���D�AHD���D��HD�  D�AHD�� D���D��qD�>�D�}qD��HD�  D�=qD�~�D��HD�  D�<)D�~�D�� D�HD�AHD��HD�� D�HD�>�D�~�D�� D�HD�<)D�|)D�� D��D�>�D�~�D�� D�HD�@ D��HD���D���D�@ D��HD��HD�  D�@ D��HD�� D���D�AHD�� D���D���D�>�D�}qD���D�  D�@ D��HD�� D��qD�>�D��HD�� D���D�=qD�� D�� D���D�@ D�~�D��HD��D�AHD�~�D���D�  D�>�D�� D��HD�  D�>�D��HD�� D�HD�AHD�� D��HD�HD�@ D��HD�� D�  D�@ D�~�D��qD�  D�@ D�� D�D�HD�AHD���D�� D���D�B�D�� D��HD�HD�@ D�~�D���D�  D�AHD���D��HD�HD�@ D�� D�� D�HD�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�HD�>�D��HD��HD���D�>�D�}qD�� D�HD�AHD�}qD�� D�HD�@ D�� D���D��)D�>�D�� D��HD��D�@ D�~�D�� D���D�AHD�� D���D���D�>�D�� D���D��qD�@ D��HD���D��)D�AHD���D�D�  D�@ D�~�D�� D��qD�>�D�}qD���D���D�@ D��HD�� D��D�AHD���D�D�  D�@ D��HD���D��qD�>�D�� D��HD�HD�@ D��HD�� D�HD�>�D�}qD��qD���D�>�D�� D��HD��D�B�D�� D���D�HD�AHD���D���D�HD�@ D�� D�� D���D�>�D�}qD�� D��D�B�D��HD��HD�  D�B�D��HD��HD��D�AHD�� D���D���D�>�DHD¾�D�  D�>�D�~�Dþ�D���D�@ D�~�D�� D�  D�=qDŀ D�D��D�AHDƁHD�� D�HD�AHDǂ�D��HD��)D�@ DȁHDȾ�D���D�@ Dɀ Dɾ�D�HD�@ Dʀ D��HD�HD�>�Dˀ D��HD�  D�>�D́HD�� D�  D�AHD̀ D��HD�HD�C�D΁HD�� D�HD�@ D�~�DϾ�D�  D�@ DЂ�D�� D�  D�@ D�~�DѾ�D�  D�>�D�~�D�� D�  D�@ DӁHD��HD��D�@ DԀ D�� D�  D�@ D�~�D�� D�  D�>�Dր D��HD�  D�AHDׁHD�� D�  D�AHD؁HD��HD�  D�@ DفHD�� D�  D�@ D�~�D�� D�  D�AHDہHD�� D���D�@ D�~�D�� D�  D�@ D�~�Dݾ�D�  D�@ D�~�D�� D�  D�>�D߀ D�� D���D�>�D�� D�� D�HD�@ D�~�D��HD�  D�@ D�~�D��HD��D�@ D�HD�� D���D�AHD� D��HD�  D�>�D� D�� D�HD�@ D�HD��HD�  D�@ D� D羸D�HD�AHD� D辸D���D�AHD� D�D���D�>�D�~�D�qD���D�>�D�~�D�qD�HD�@ D� D�D�  D�>�D�HD��HD��D�@ D�~�D�� D�  D�@ D� D�� D�HD�>�D�� D�D�  D�AHD� D�qD�  D�@ D�~�D�� D���D�@ D�D��HD�  D�@ D�}qD���D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�j=>�?k�?���?���?��H@z�@.{@B�\@Y��@s33@�ff@�33@�G�@���@��H@���@�Q�@��@�z�A ��A
=A{Az�A(�A!�A(Q�A.{A5�A;�AB�\AH��AQG�AY��A_\)AfffAl��Ar�\Ay��A�  A��A��RA�=qA�A�G�A�p�A���A�z�A��A��\A�A���A���A�  A�(�A�  A�33A�
=A���A���A�  A˅AθRA�=qAָRA��A�p�A���A�(�A�RA�=qA�p�A�G�A�z�A�  A�33A��RBp�B33B��B�HB��B
{B�BG�B�HBz�BffBQ�B{B�B�B�\B�
B��B=qB�B ��B!�B#
=B$Q�B%p�B&�\B'�B(z�B)p�B*=qB+\)B,(�B-G�B.=qB/\)B0Q�B1p�B2ffB3\)B4Q�B5p�B6ffB7�B8��B9�B:�RB;�B<��B=��B>�RB?�B@��BAp�BB=qBC33BD(�BE�BF{BG
=BH(�BIG�BJffBK�BLQ�BMp�BNffBO�BP(�BP��BQBR�RBS�BT��BUp�BV�\BW�BX��BY�BZ�RB[�
B\��B]B^�RB_�B`z�Ba�Bb{Bc
=Bc�
Bd��Be�Bg33Bh(�Bh��Bi�Bj�HBk�
Blz�Bm��Bn=qBo�Bp  Bp��Bq�Br�RBs�Bt��BuBv�RBw�Bx��By��By�Bz�HB{�
B|��B}��B~�\B�B�z�B���B�\)B��
B�Q�B���B���B��B��
B�Q�B���B�\)B��B�Q�B���B�G�B�B�  B�z�B���B�\)B��
B�Q�B���B�\)B��
B�Q�B��HB�G�B��B�  B�ffB��HB�\)B��
B�Q�B���B�p�B��B�ffB��HB��B���B�{B��\B���B���B�(�B���B��B���B��B�Q�B���B�33B��B�ffB��HB�\)B��
B�ffB���B�33B��B�  B�z�B���B���B�{B��\B�
=B��B�B�=qB���B�33B���B�{B��\B�G�B�B�=qB��RB��B��B��B�ffB��HB�\)B�{B��\B��B�\)B��
B�Q�B���B�p�B�  B��\B���B�p�B�B�=qB���B�p�B�  B�Q�B��RB��B��
B�ffB��HB�p�B�B�(�B���B��B�B�=qB���B��B���B��B�ffB���B�G�B��B�ffB���B��B���B��B�z�B��B��B��B�(�B��\B�G�B��B�(�B�ffB��HB�\)B�B�Q�B���BŅB�{BƏ\B��HB�p�B�=qB���B�p�B�B�Q�B�
=BˮB�=qB̸RB�G�B�(�B���B�G�B��
BЏ\B�G�B��
B�Q�B���BӮB�=qBԣ�B�33B�B�z�B��B�p�B�{BظRB�p�B�  B�z�B�33B��B܏\B�
=B�B�z�B��HB߅B�ffB�
=B�B�{B���B㙚B�{B�RB�B�=qB���B�\)B�(�B��HB�G�B��B�RB�p�B�B�ffB�33B��
B�=qB���B�B�ffB�RB�p�B�(�B��B�33B�{B��RB��B�B��\B�33B��B�ffB��B��B�=qB�
=B���B�=qB���B��B�=qB���B��C (�C ffC �
C33Cp�C�
C33CffC�RC33C�CC
=C�C��C
=Cz�C�
C
=C�C�HC{CffC�HC{Cp�C�HC	(�C	p�C	�HC
(�C
z�C
�C(�C�C��C33C��C  C=qC��C
=CQ�C��C�C\)C�RC(�Cp�C��CG�C�C�HC\)C�\C  Cp�C��C�Cz�C�RC(�Cp�C�RC33Cz�C�
CG�C�C�CQ�C�\C
=CG�C��C{CQ�CC33Cp�C�HCG�C�C  CG�C�C(�CffC�HC=qC�C   C =qC ��C!�C!\)C!�
C"(�C"z�C"��C#33C#�C#��C$Q�C$��C%
=C%�C%��C&=qC&�\C&�HC'\)C'��C({C(�C(��C)G�C)�\C*
=C*\)C*�C+33C+z�C+��C,Q�C,��C-�C-ffC-�HC.(�C.��C.��C/\)C/��C0{C0�\C0�
C1Q�C1�C2  C2z�C2C3G�C3z�C4  C4=qC4C5  C5z�C5�HC6(�C6��C6�C7p�C7�C8(�C8z�C8��C9=qC9�RC:  C:z�C:C;=qC;�C<
=C<G�C<C={C=z�C=�
C>=qC>��C>�HC?ffC?��C@
=C@=qC@�RC@�
CAG�CAffCA�RCA�HCB=qCBQ�CB��CBCC
=CC(�CCffCC�\CC�
CC�CD33CDQ�CD��CD�CE  CE�CEffCE�CE��CE�CF=qCFQ�CF��CFCG
=CG�CGp�CG�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�oA�oA���A־wAֲ-Aֲ-Aְ!A֟�A֝�A֛�A֗�A֓uA֑hA֑hA֏\A։7AօAօAօAօAև+AօAօAև+Aև+Aև+Aև+Aև+Aև+A։7A։7A։7A։7A֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֍PA֍PA֋DA։7A։7A�~�A�v�A�z�AɾwA�G�AēuA��A���A���A���A��HA�bA���A���A��A���A�-A��A�jA�Q�A���A��
A�ĜA�A�A���A��
A���A��DA���A��/A��A�t�A��A�C�A���A�JA��A�{A�n�A�A�ffA�p�A�z�A�x�A���A��A�bA�9XAv�At��Ar��Ao33Akt�Ag&�Ad��Ac��Ab�A_`BA[�FAV(�AS��ASG�ARȴAQ��AP^5AO�wANVAJ�yAIt�AF��AC�#AB�/ABVAA�7A>�jA;A8��A6��A5O�A37LA1"�A/�mA/��A/XA.�\A-33A+�mA)��A'K�A%t�A$��A$ZA$JA!�-A �A��A��A�AdZA�A�!A�FA��A$�A+Av�A5?AA�FA�Al�A7LA�A+AO�AS�A�yAjA�^AO�AbAA9XAG�A�A�A�A�A�TA
bA	/A�A��A��A�jA��A$�A��A�mA��A-Ap�A��AJA�A�uA�+A��A�A �!A �uA M�@��y@�=q@�j@�1@��R@���@�O�@�%@��@�j@�33@�v�@��@���@�u@�bN@�I�@��@�1'@�@���@��@���@�Q�@��m@�ƨ@�w@�@�|�@��H@�hs@�j@���@��@�^@�x�@�hs@�O�@�%@���@�+@��@�$�@���@�x�@�&�@��@� �@�t�@��@�n�@ݡ�@���@܃@�I�@���@۾w@ۥ�@�K�@�@ڟ�@�V@�$�@ٺ^@�hs@��/@�Z@��
@�dZ@�"�@��@��`@�dZ@��H@�{@�9X@ϥ�@�K�@�~�@�p�@��@���@̬@̋D@�I�@˕�@�^5@��@��;@�=q@��@őh@�%@ģ�@�(�@öF@�l�@��@�~�@�$�@��@�@��@�G�@�hs@��@��/@�9X@�1'@�(�@� �@�1@��
@�\)@�+@��@�@��H@�=q@�$�@��-@��@�Z@�(�@�b@���@�33@���@��\@�5?@�x�@��`@�Ĝ@�Q�@�ƨ@�|�@��@�M�@��@��#@��@��-@�X@�V@�9X@�l�@�33@�o@�
=@��@�+@���@�V@��@�hs@��@���@���@�9X@�  @��F@���@�l�@�"�@���@�J@��^@��`@��j@�r�@�9X@�1@���@�t�@�C�@�
=@��R@�-@���@��7@�Ĝ@�  @��P@�K�@�"�@���@��y@���@���@�V@��@��7@�`B@�V@��9@�1@��@���@��y@���@�n�@��T@���@��7@���@��/@��u@�A�@�1@��;@��F@��@�33@�ȴ@�=q@�J@�@�hs@�O�@�/@�&�@�%@�Ĝ@�bN@� �@���@�;d@��R@��!@�n�@��^@��@���@���@�A�@��@��
@���@�|�@�+@��@�~�@�ff@�5?@�@��@��@�7L@�&�@�&�@��@��9@�A�@�9X@��@�dZ@���@�-@��@���@�p�@�G�@�&�@�&�@�&�@��@�%@���@�Ĝ@��u@�A�@�b@���@��
@���@��P@�t�@�K�@��@��@���@��+@�V@�@��T@���@��^@�p�@��/@��j@��D@�I�@�9X@���@���@�t�@�;d@��@���@�{@��-@��@��@���@�Ĝ@�j@�I�@�1'@��
@�t�@�S�@��@���@�ȴ@�~�@�{@��-@�O�@��@���@��9@��@�bN@�I�@�9X@��@���@�S�@�o@��y@��!@�ff@�=q@��T@��-@�G�@�V@���@��@��9@�r�@�;@|�@l�@;d@~�@~ȴ@~�+@}�T@|Z@|(�@{�
@{t�@z��@z�@y��@y�@yhs@y7L@x��@xbN@w�w@w\)@v��@v�R@v��@vff@vE�@u`B@t��@t�/@t�D@t9X@t1@s��@s��@sC�@r��@r��@rn�@q��@q&�@p�u@o�@o�w@o��@o�@nE�@m@mO�@l�@l(�@k�
@kt�@k"�@k@j�@j�H@j�!@j=q@ix�@h��@hr�@hb@g�w@g+@f�y@f��@f$�@e��@e?}@d��@dj@c�
@cdZ@c@b��@b-@aG�@`�u@`r�@` �@_\)@^�@^�R@^�R@^�R@^�R@^{@]`B@\��@\I�@[��@[33@[@Z�H@ZJ@YX@Y%@XĜ@XA�@W�w@W\)@W;d@V��@V�+@V$�@U��@UO�@T��@T��@T��@T9X@S�m@St�@R�H@Rn�@RM�@R-@R�@Q�@QX@P��@PA�@Pb@O��@O�@O��@O�P@Ol�@OK�@O
=@Nȴ@N��@N�+@Nv�@NE�@N$�@N@M�@M��@Mp�@M�@L�/@L�@L�@Kt�@K33@Ko@J�@J��@Jn�@I�#@Ihs@I7L@I�@HĜ@H�@HbN@G�;@G+@F�+@E�@E�-@E��@E�@D�@D��@Dj@C��@CdZ@CC�@Co@C@B��@B�!@B��@B~�@Bn�@B-@A�@A��@A7L@A�@@�`@@��@@ �@?�w@?|�@?;d@>��@>�@>v�@=�@=�-@=p�@=�@<��@<��@<j@<�@;�
@;dZ@;33@;o@;@;@:�\@:-@9�@9��@9&�@8�`@8�u@8b@7�P@7l�@7+@6�@6�R@6��@6�+@6ff@6E�@6$�@6@5p�@5�@5V@4��@4�j@4�@4I�@4�@3��@3�
@3�F@3t�@2�@2��@2~�@1��@1��@1x�@1hs@1X@17L@17L@1%@0�9@0r�@01'@0b@/�;@/K�@.�@.�@.ȴ@.�+@.5?@.$�@.@.@-��@-��@-O�@-V@,��@,��@,j@,I�@,9X@,�@+��@+t�@+@*�!@*�@)��@(��@(�9@(�@(1'@( �@(  @'�@'|�@'\)@'\)@'K�@'+@&�y@&��@%��@%�@%O�@%/@$�j@$z�@$�@#�F@#��@#�@#t�@#�@#dZ@#S�@#"�@"�!@"M�@"=q@"-@"J@!��@!�@!�#@!��@!&�@ Ĝ@ �u@ r�@ Q�@�@��@�@l�@\)@�y@�R@�+@5?@5?@{@��@��@p�@`B@?}@�@��@��@�j@�D@�m@�F@��@t�@dZ@S�@C�@"�@��@^5@J@�#@�^@��@X@Ĝ@r�@b@�;@��@�P@�P@|�@K�@��@ȴ@��@�+@v�@$�@��@�h@p�@`B@�@z�@�@��@ƨ@t�@dZ@33@�@��@��@�!@~�@�@��@��@7L@%@��@Ĝ@�u@bN@1'@��@�w@�w@�w@�P@\)@;d@
=@�R@�R@��@v�@V@E�@@��@�@O�@��@�j@��@I�@9X@�@��@�
@ƨ@�F@��@��@��@�@t�@dZ@"�@o@@@
�H@
��@
��@
n�@
=q@
J@	�#@	�^@	��@	��@	�7@	hs@	7L@	%@�`@�`@�`@��@Ĝ@Ĝ@Ĝ@�9@��@�@r�A�/A��A��A�oA�{A��A�oA�VA��A��A�
=A�1A���A��A�ƨAֲ-Aֲ-Aֲ-Aֲ-Aְ!Aֲ-A֮A֬A֝�A֝�A֝�A֝�A֝�A֝�A֟�A֝�A֙�A֗�A֗�A֕�A֓uA֕�A֕�A֕�A֕�A֓uA֓uA֓uA֑hA֑hA֑hA֏\A֑hA֏\A֑hA֏\A֑hA֑hA֍PA֋DA֋DA։7Aև+Aև+Aև+Aև+A։7Aև+AօAփAփAփAփAօAև+Aև+Aև+Aև+Aև+A։7Aև+Aև+Aև+Aև+AփAօAօAև+Aև+A։7Aև+Aև+Aև+AօAփAփAփAփAօAև+A։7A։7A։7A։7A։7Aև+AօAօAօAև+Aև+Aև+Aև+Aև+Aև+Aև+A։7A։7A։7A։7A։7A։7A։7Aև+Aև+Aև+AօAօAօAփAփAփAօAօAև+A։7A։7A։7A։7A։7A։7Aև+Aև+AօAօAօAօAօAօAև+A։7A֋DA֋DA֋DA֋DA֋DA֋DAև+Aև+Aև+Aև+AօAև+Aև+Aև+Aև+A֋DA֋DA֋DA֍PA֋DA֋DA։7A֋DA։7A։7Aև+A։7A։7A։7A։7A։7A։7A֋DA֍PA֍PA֍PA֋DA֋DA։7A։7A։7A։7A֋DA֍PA֍PA֍PA֍PA֍PA֍PA֋DA֋DA։7A։7Aև+A֋DA֍PA֍PA֍PA֍PA֍PA֋DA֋DA։7A։7A։7A֋DA֍PA֋DA֍PA֍PA֍PA֏\A֍PA։7A։7Aև+Aև+A։7A։7A֋DA֋DA֍PA֍PA֍PA֍PA֋DA֋DA։7Aև+A։7A֋DA֍PA֍PA֍PA֍PA֋DA֋DA։7A։7A։7A֋DA֍PA֍PA֍PA֍PA֋DA։7A֋DA։7A։7A։7A֋DA֍PA֍PA֏\A֍PA֋DA։7A֋DA։7A։7A֋DA֍PA֍PA֍PA֏\A֏\A֍PA֍PA։7A։7A։7A֋DA֍PA֍PA֏\A֍PA֍PA֍PA֋DA֋DA֍PA֏\A֏\A֏\A֍PA։7Aև+A։7A֋DA֍PA֏\A֏\A֋DA֋DA֍PA֏\A֏\A֍PA֍PA։7Aև+A։7A֋DA֍PA֍PA։7Aև+Aև+Aև+Aև+A։7A֋DA֋DA֋DA։7Aև+Aև+A։7A։7A։7Aև+AօAցA�~�AցAցA�~�A�r�A�bNA�bA��AՓuA��A�^5AҸRA�`BA�Aͧ�A�+A���A̓uA�v�A���A�^5A�\)A�G�A��A�x�A�I�A�1'A�ĜA��A��#A��/A�ZA�AĲ-AĬAģ�Aĕ�Aĉ7Aĉ7Aĉ7AăA�t�A�K�A�=qA��A��TA�bNA��`A�l�A�A��!A�ZA�1'A��A��A�r�A�$�A��A��7A�ZA�oA���A��uA�%A���A�v�A�{A��A���A��+A�v�A�dZA�VA�K�A�?}A�33A��A��TA�^5A�bA��A��A��A�dZA�E�A�+A���A�bNA�=qA���A��A��^A�dZA�JA���A�x�A��A��jA��TA�?}A��;A���A�5?A� �A���A�r�A�1'A�oA�A��A��9A�z�A�-A��A��mA���A���A��+A�p�A�ffA�M�A�;dA�/A���A�bNA�33A��/A��\A�-A�A��;A��!A���A���A�x�A�M�A��;A���A���A�VA�+A��A���A���A�z�A�S�A��A��!A�&�A�A�C�A��#A�~�A�(�A��uA���A��DA�=qA�A���A��FA���A��A�oA��TA���A�1'A�A���A��!A��+A�x�A�p�A�n�A�hsA�M�A���A�A�A���A�K�A��uA���A�t�A�^5A�;dA�&�A�oA���A��;A�ȴA��A��PA�~�A�r�A�n�A�\)A�M�A�=qA�&�A�JA��;A��uA��A�hsA�JA�z�A�bA��mA��A���A��!A�&�A�ƨA�XA�+A�1A��FA��A���A��mA��uA��A���A�1'A�  A�E�A��^A�"�A���A��+A�r�A�Q�A�A�A�-A��A�%A���A��A��A�ĜA���A�O�A�"�A��HA�$�A��\A�7LA�JA�A��yA��FA�t�A�VA�1'A�A�bA�ffA�ƨA�+A���A�7LA��A���A�ĜA��\A�z�A�ffA�{A�ȴA��+A�XA�33A�{A��A�ƨA�`BA���A��A�l�A}`BAx�RAw��AwoAv�Av�jAv�\Av5?Au�
Au�Au33At��Atv�AtbNAt5?At�As�mAs�As�AshsAs;dAr�Ar�ArffAr9XAq��AqXAp�jApI�Ap�Ap  AoAoK�AnȴAm�Al��Al��AljAlbAk��Ak�mAk��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     A��A��A�oA�oA���A־wAֲ-Aֲ-Aְ!A֟�A֝�A֛�A֗�A֓uA֑hA֑hA֏\A։7AօAօAօAօAև+AօAօAև+Aև+Aև+Aև+Aև+Aև+A։7A։7A։7A։7A֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֋DA֍PA֍PA֋DA։7A։7A�~�A�v�A�z�AɾwA�G�AēuA��A���A���A���A��HA�bA���A���A��A���A�-A��A�jA�Q�A���A��
A�ĜA�A�A���A��
A���A��DA���A��/A��A�t�A��A�C�A���A�JA��A�{A�n�A�A�ffA�p�A�z�A�x�A���A��A�bA�9XAv�At��Ar��Ao33Akt�Ag&�Ad��Ac��Ab�A_`BA[�FAV(�AS��ASG�ARȴAQ��AP^5AO�wANVAJ�yAIt�AF��AC�#AB�/ABVAA�7A>�jA;A8��A6��A5O�A37LA1"�A/�mA/��A/XA.�\A-33A+�mA)��A'K�A%t�A$��A$ZA$JA!�-A �A��A��A�AdZA�A�!A�FA��A$�A+Av�A5?AA�FA�Al�A7LA�A+AO�AS�A�yAjA�^AO�AbAA9XAG�A�A�A�A�A�TA
bA	/A�A��A��A�jA��A$�A��A�mA��A-Ap�A��AJA�A�uA�+A��A�A �!A �uA M�@��y@�=q@�j@�1@��R@���@�O�@�%@��@�j@�33@�v�@��@���@�u@�bN@�I�@��@�1'@�@���@��@���@�Q�@��m@�ƨ@�w@�@�|�@��H@�hs@�j@���@��@�^@�x�@�hs@�O�@�%@���@�+@��@�$�@���@�x�@�&�@��@� �@�t�@��@�n�@ݡ�@���@܃@�I�@���@۾w@ۥ�@�K�@�@ڟ�@�V@�$�@ٺ^@�hs@��/@�Z@��
@�dZ@�"�@��@��`@�dZ@��H@�{@�9X@ϥ�@�K�@�~�@�p�@��@���@̬@̋D@�I�@˕�@�^5@��@��;@�=q@��@őh@�%@ģ�@�(�@öF@�l�@��@�~�@�$�@��@�@��@�G�@�hs@��@��/@�9X@�1'@�(�@� �@�1@��
@�\)@�+@��@�@��H@�=q@�$�@��-@��@�Z@�(�@�b@���@�33@���@��\@�5?@�x�@��`@�Ĝ@�Q�@�ƨ@�|�@��@�M�@��@��#@��@��-@�X@�V@�9X@�l�@�33@�o@�
=@��@�+@���@�V@��@�hs@��@���@���@�9X@�  @��F@���@�l�@�"�@���@�J@��^@��`@��j@�r�@�9X@�1@���@�t�@�C�@�
=@��R@�-@���@��7@�Ĝ@�  @��P@�K�@�"�@���@��y@���@���@�V@��@��7@�`B@�V@��9@�1@��@���@��y@���@�n�@��T@���@��7@���@��/@��u@�A�@�1@��;@��F@��@�33@�ȴ@�=q@�J@�@�hs@�O�@�/@�&�@�%@�Ĝ@�bN@� �@���@�;d@��R@��!@�n�@��^@��@���@���@�A�@��@��
@���@�|�@�+@��@�~�@�ff@�5?@�@��@��@�7L@�&�@�&�@��@��9@�A�@�9X@��@�dZ@���@�-@��@���@�p�@�G�@�&�@�&�@�&�@��@�%@���@�Ĝ@��u@�A�@�b@���@��
@���@��P@�t�@�K�@��@��@���@��+@�V@�@��T@���@��^@�p�@��/@��j@��D@�I�@�9X@���@���@�t�@�;d@��@���@�{@��-@��@��@���@�Ĝ@�j@�I�@�1'@��
@�t�@�S�@��@���@�ȴ@�~�@�{@��-@�O�@��@���@��9@��@�bN@�I�@�9X@��@���@�S�@�o@��y@��!@�ff@�=q@��T@��-@�G�@�V@���@��@��9@�r�@�;@|�@l�@;d@~�@~ȴ@~�+@}�T@|Z@|(�@{�
@{t�@z��@z�@y��@y�@yhs@y7L@x��@xbN@w�w@w\)@v��@v�R@v��@vff@vE�@u`B@t��@t�/@t�D@t9X@t1@s��@s��@sC�@r��@r��@rn�@q��@q&�@p�u@o�@o�w@o��@o�@nE�@m@mO�@l�@l(�@k�
@kt�@k"�@k@j�@j�H@j�!@j=q@ix�@h��@hr�@hb@g�w@g+@f�y@f��@f$�@e��@e?}@d��@dj@c�
@cdZ@c@b��@b-@aG�@`�u@`r�@` �@_\)@^�@^�R@^�R@^�R@^�R@^{@]`B@\��@\I�@[��@[33@[@Z�H@ZJ@YX@Y%@XĜ@XA�@W�w@W\)@W;d@V��@V�+@V$�@U��@UO�@T��@T��@T��@T9X@S�m@St�@R�H@Rn�@RM�@R-@R�@Q�@QX@P��@PA�@Pb@O��@O�@O��@O�P@Ol�@OK�@O
=@Nȴ@N��@N�+@Nv�@NE�@N$�@N@M�@M��@Mp�@M�@L�/@L�@L�@Kt�@K33@Ko@J�@J��@Jn�@I�#@Ihs@I7L@I�@HĜ@H�@HbN@G�;@G+@F�+@E�@E�-@E��@E�@D�@D��@Dj@C��@CdZ@CC�@Co@C@B��@B�!@B��@B~�@Bn�@B-@A�@A��@A7L@A�@@�`@@��@@ �@?�w@?|�@?;d@>��@>�@>v�@=�@=�-@=p�@=�@<��@<��@<j@<�@;�
@;dZ@;33@;o@;@;@:�\@:-@9�@9��@9&�@8�`@8�u@8b@7�P@7l�@7+@6�@6�R@6��@6�+@6ff@6E�@6$�@6@5p�@5�@5V@4��@4�j@4�@4I�@4�@3��@3�
@3�F@3t�@2�@2��@2~�@1��@1��@1x�@1hs@1X@17L@17L@1%@0�9@0r�@01'@0b@/�;@/K�@.�@.�@.ȴ@.�+@.5?@.$�@.@.@-��@-��@-O�@-V@,��@,��@,j@,I�@,9X@,�@+��@+t�@+@*�!@*�@)��@(��@(�9@(�@(1'@( �@(  @'�@'|�@'\)@'\)@'K�@'+@&�y@&��@%��@%�@%O�@%/@$�j@$z�@$�@#�F@#��@#�@#t�@#�@#dZ@#S�@#"�@"�!@"M�@"=q@"-@"J@!��@!�@!�#@!��@!&�@ Ĝ@ �u@ r�@ Q�@�@��@�@l�@\)@�y@�R@�+@5?@5?@{@��@��@p�@`B@?}@�@��@��@�j@�D@�m@�F@��@t�@dZ@S�@C�@"�@��@^5@J@�#@�^@��@X@Ĝ@r�@b@�;@��@�P@�P@|�@K�@��@ȴ@��@�+@v�@$�@��@�h@p�@`B@�@z�@�@��@ƨ@t�@dZ@33@�@��@��@�!@~�@�@��@��@7L@%@��@Ĝ@�u@bN@1'@��@�w@�w@�w@�P@\)@;d@
=@�R@�R@��@v�@V@E�@@��@�@O�@��@�j@��@I�@9X@�@��@�
@ƨ@�F@��@��@��@�@t�@dZ@"�@o@@@
�H@
��@
��@
n�@
=q@
J@	�#@	�^@	��@	��@	�7@	hs@	7L@	%@�`@�`@�`@��@Ĝ@Ĝ@Ĝ@�9@��@�@r�A�/A��A��A�oA�{A��A�oA�VA��A��A�
=A�1A���A��A�ƨAֲ-Aֲ-Aֲ-Aֲ-Aְ!Aֲ-A֮A֬A֝�A֝�A֝�A֝�A֝�A֝�A֟�A֝�A֙�A֗�A֗�A֕�A֓uA֕�A֕�A֕�A֕�A֓uA֓uA֓uA֑hA֑hA֑hA֏\A֑hA֏\A֑hA֏\A֑hA֑hA֍PA֋DA֋DA։7Aև+Aև+Aև+Aև+A։7Aև+AօAփAփAփAփAօAև+Aև+Aև+Aև+Aև+A։7Aև+Aև+Aև+Aև+AփAօAօAև+Aև+A։7Aև+Aև+Aև+AօAփAփAփAփAօAև+A։7A։7A։7A։7A։7Aև+AօAօAօAև+Aև+Aև+Aև+Aև+Aև+Aև+A։7A։7A։7A։7A։7A։7A։7Aև+Aև+Aև+AօAօAօAփAփAփAօAօAև+A։7A։7A։7A։7A։7A։7Aև+Aև+AօAօAօAօAօAօAև+A։7A֋DA֋DA֋DA֋DA֋DA֋DAև+Aև+Aև+Aև+AօAև+Aև+Aև+Aև+A֋DA֋DA֋DA֍PA֋DA֋DA։7A֋DA։7A։7Aև+A։7A։7A։7A։7A։7A։7A֋DA֍PA֍PA֍PA֋DA֋DA։7A։7A։7A։7A֋DA֍PA֍PA֍PA֍PA֍PA֍PA֋DA֋DA։7A։7Aև+A֋DA֍PA֍PA֍PA֍PA֍PA֋DA֋DA։7A։7A։7A֋DA֍PA֋DA֍PA֍PA֍PA֏\A֍PA։7A։7Aև+Aև+A։7A։7A֋DA֋DA֍PA֍PA֍PA֍PA֋DA֋DA։7Aև+A։7A֋DA֍PA֍PA֍PA֍PA֋DA֋DA։7A։7A։7A֋DA֍PA֍PA֍PA֍PA֋DA։7A֋DA։7A։7A։7A֋DA֍PA֍PA֏\A֍PA֋DA։7A֋DA։7A։7A֋DA֍PA֍PA֍PA֏\A֏\A֍PA֍PA։7A։7A։7A֋DA֍PA֍PA֏\A֍PA֍PA֍PA֋DA֋DA֍PA֏\A֏\A֏\A֍PA։7Aև+A։7A֋DA֍PA֏\A֏\A֋DA֋DA֍PA֏\A֏\A֍PA֍PA։7Aև+A։7A֋DA֍PA֍PA։7Aև+Aև+Aև+Aև+A։7A֋DA֋DA֋DA։7Aև+Aև+A։7A։7A։7Aև+AօAցA�~�AցAցA�~�A�r�A�bNA�bA��AՓuA��A�^5AҸRA�`BA�Aͧ�A�+A���A̓uA�v�A���A�^5A�\)A�G�A��A�x�A�I�A�1'A�ĜA��A��#A��/A�ZA�AĲ-AĬAģ�Aĕ�Aĉ7Aĉ7Aĉ7AăA�t�A�K�A�=qA��A��TA�bNA��`A�l�A�A��!A�ZA�1'A��A��A�r�A�$�A��A��7A�ZA�oA���A��uA�%A���A�v�A�{A��A���A��+A�v�A�dZA�VA�K�A�?}A�33A��A��TA�^5A�bA��A��A��A�dZA�E�A�+A���A�bNA�=qA���A��A��^A�dZA�JA���A�x�A��A��jA��TA�?}A��;A���A�5?A� �A���A�r�A�1'A�oA�A��A��9A�z�A�-A��A��mA���A���A��+A�p�A�ffA�M�A�;dA�/A���A�bNA�33A��/A��\A�-A�A��;A��!A���A���A�x�A�M�A��;A���A���A�VA�+A��A���A���A�z�A�S�A��A��!A�&�A�A�C�A��#A�~�A�(�A��uA���A��DA�=qA�A���A��FA���A��A�oA��TA���A�1'A�A���A��!A��+A�x�A�p�A�n�A�hsA�M�A���A�A�A���A�K�A��uA���A�t�A�^5A�;dA�&�A�oA���A��;A�ȴA��A��PA�~�A�r�A�n�A�\)A�M�A�=qA�&�A�JA��;A��uA��A�hsA�JA�z�A�bA��mA��A���A��!A�&�A�ƨA�XA�+A�1A��FA��A���A��mA��uA��A���A�1'A�  A�E�A��^A�"�A���A��+A�r�A�Q�A�A�A�-A��A�%A���A��A��A�ĜA���A�O�A�"�A��HA�$�A��\A�7LA�JA�A��yA��FA�t�A�VA�1'A�A�bA�ffA�ƨA�+A���A�7LA��A���A�ĜA��\A�z�A�ffA�{A�ȴA��+A�XA�33A�{A��A�ƨA�`BA���A��A�l�A}`BAx�RAw��AwoAv�Av�jAv�\Av5?Au�
Au�Au33At��Atv�AtbNAt5?At�As�mAs�As�AshsAs;dAr�Ar�ArffAr9XAq��AqXAp�jApI�Ap�Ap  AoAoK�AnȴAm�Al��Al��AljAlbAk��Ak�mAk��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�aB
��B
�aB
��B
�mB
�[B
�[B
�[B
��B
ӏB
ӏB
�[B
�[B
�[B
�[B
�[B
�[B
�&B
�&B
�&B
�&B
�&B
�[B
�[B
ӏB
ӏB
ӏB
ӏB
��B
ӏB
��B
��B
ӏB
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
ӏB
��B
� B
ϫB
�XB
�]B�B�zB�B"hB.�BW?Ba|Br|Bq�BjBgmBk�Bd�B]�BC-B?�B7�B8�B+B)�B!�B#B�B��B�ZB�B�|B�<B�tB�=B�{B�rBd�BOvB.�B�B
�vB
՛B
�pB
�jB
��B
��B
_pB
P�B
;�B
?HB	��B	�]B	ϫB	��B	��B	��B	�AB	}�B	v�B	h�B	WsB	D�B	5B	0�B	.�B	+6B	%FB	VB		B	B	 �B�VB�cB�B�>B�B��B��B�B��B�B�mB�)B�B�sB֡B�sB֡B��B��B��BŢB�-B��B�<B�'B��B�3B�BB�OB�B�B��B�mB�#B�dBچB�B�B�KB��B�B��B	B	B	B	_B	$B	$tB	&B	(XB	,�B	-�B	"4B	&LB	 \B	OB	 �B	:�B	<�B	<�B	:�B	3�B	[�B	{B	{�B	|PB	|PB	~�B	lWB	WsB	S[B	T�B	[�B	YB	TaB	XyB	i�B	y�B	~�B	}�B	��B	�rB	�xB	�B	��B	�fB	�%B	��B	�B	�B	�;B	��B	�B	��B	�;B	�uB	�GB	�JB	�$B	�kB	�eB	�B	�6B	�qB	�B	�-B	�aB	�3B	�3B	�3B	�3B	��B	��B	��B	��B	�*B	�B	�HB	��B	��B	�BB	��B	�'B	��B	�[B	��B	�'B	�[B	B	B	�aB	�-B	�-B	�gB	�B	��B	��B	��B	�B	�B	��B	�tB	�B	��B	ɺB	�#B	ʌB	�XB	��B	˒B	��B	��B	�^B	�B	�B	ϫB	�B	�B	бB	�B	��B	уB	҉B	�gB	خB	�B	�B	خB	�B	ٴB	�sB	خB	֡B	��B	ӏB	��B	�gB	�?B	�yB	�KB	�B	�B	�B	�B	ٴB	ںB	�WB	ߤB	�B	��B	�fB	��B	�fB	��B	�2B	�B	�8B	�sB	��B	�B	��B	�sB	�DB	��B	�WB	�"B	�QB	��B	��B	�]B	�]B	��B	�cB	��B	�oB	��B	�oB	�/B	�B	� B	�B	�5B	��B	�B	�AB	�GB	�B	�B	�B	�B	�|B	��B	��B	�	B	��B	��B	�	B	��B	��B	��B	�JB	��B	��B	�B	��B	��B	��B	��B	�PB	��B	�(B	��B	��B	��B
 iB
 iB
  B
 iB	�.B
;B
oB
 �B
�B
�B
�B
�B
�B
B
GB
�B
�B
SB
�B
{B
�B
SB
�B
�B
_B
_B
�B
_B
�B
fB
�B
fB
�B
	7B
	lB
	�B
	�B

	B

rB

	B

rB

rB
DB
B
B
�B
�B
�B
�B
�B
�B
VB
�B
VB
\B
.B
�B
.B
 B
oB
B
�B
�B
FB
�B
�B
MB
�B
�B
�B
�B
B
7B
kB
�B
xB
qB
	B
	B
�B
�B
xB
IB
�B
�B
�B
�B
�B
�B
�B
 \B
 �B
 'B
 'B
 \B
 �B
 �B
!-B
!-B
!�B
"hB
"�B
#�B
$B
%�B
%FB
&B
%�B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
($B
)�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
+kB
+�B
,qB
-CB
-CB
-CB
.IB
.�B
/B
/�B
/�B
/�B
0�B
1�B
1�B
1�B
2aB
2-B
2�B
33B
3�B
4nB
3hB
4B
4�B
5tB
5�B
5�B
5?B
4�B
6FB
6zB
6�B
6�B
7B
6FB
6B
6B
6�B
7LB
6�B
7LB
8B
8�B
8RB
9XB
9�B
9$B
:*B
:*B
:*B
:�B
;�B
<jB
;�B
<B
<B
<�B
<�B
<�B
=B
=B
=<B
=�B
=<B
>�B
>wB
>BB
>�B
>�B
>�B
>�B
@OB
?HB
?}B
?HB
?�B
?HB
?HB
?�B
?�B
@OB
@B
?�B
@�B
A�B
A B
A�B
A�B
A B
A�B
A�B
B[B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
DgB
D�B
D�B
E�B
FB
FB
E�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
J#B
K)B
J�B
J�B
J�B
J�B
L0B
K^B
K)B
K�B
L�B
M6B
MB
M6B
L�B
L�B
NB
N�B
OvB
O�B
PB
O�B
O�B
O�B
P�B
P�B
P}B
P�B
QNB
R B
R�B
RTB
R B
S&B
S[B
S�B
S�B
S[B
S�B
S�B
T,B
S�B
S�B
T�B
U2B
T�B
T�B
TaB
T�B
V9B
V�B
W?B
W?B
WsB
W
B
V�B
W?B
W�B
WsB
WsB
WsB
W�B
XEB
XEB
X�B
XyB
XyB
XyB
X�B
YB
X�B
X�B
YB
YKB
ZQB
Y�B
ZB
ZB
YB
ZB
[WB
[�B
[�B
[�B
[WB
[#B
[#B
\)B
]/B
]�B
]dB
]dB
]dB
]�B
_B
^5B
^jB
_�B
_pB
_�B
`BB
`B
_�B
`BB
`B
`�B
`BB
`B
`�B
aB
aB
`�B
aB
aB
bNB
bB
bB
a�B
b�B
bB
b�B
cTB
c B
c�B
c�B
c B
cTB
c�B
c�B
dZB
d�B
e,B
e`B
e,B
d�B
e�B
e`B
e�B
f2B
f2B
e�B
e�B
f�B
f�B
g8B
g�B
h>B
h
B
g�B
g�B
g�B
h>B
hsB
h>B
iB
iDB
h�B
iyB
iyB
h�B
jB
jKB
jKB
i�B
jKB
i�B
kQB
jB
kQB
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
lWB
m]B
m)B
l�B
m)B
n�B
n/B
n/B
m�B
ncB
o B
ncB
n�B
o B
n�B
o B
o B
o�B
oiB
o�B
p;B
poB
pB
pB
o�B
p�B
qB
p�B
q�B
q�B
s�B
r|B
r�B
r|B
r|B
r�B
s�B
s�B
sMB
s�B
s�B
sMB
sMB
tB
uZB
u�B
uZB
t�B
u%B
u�B
v+B
v`B
v`B
v�B
w2B
v�B
v�B
w2B
wfB
x8B
x�B
x�B
x�B
xlB
xB
w�B
xlB
x�B
y>B
yrB
y�B
y	B
y	B
yrB
y>B
yrB
y�B
zxB
{JB
{JB
{B
{B
{JB
{B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|PB
|B
|B
|�B
}"B
}VB
}�B
}"B
}"B
}"B
}VB
}�B
~�B
~�B
.B
�B
.B
~�B
�B
�4B
��B
�iB
��B
�iB
�iB
�4B
��B
�;B
�oB
�;B
�oB
��B
�B
�uB
�uB
�uB
�uB
�B
�MB
��B
��B
��B
��B
�B
��B
��B
��B
�B
�SB
�%B
��B
��B
�YB
��B
��B
��B
�+B
��B
�1B
��B
��B
�1B
��B
�1B
�B
��B
��B
��B
�lB
�B
��B
��B
�lB
�7B
��B
��B
�=B
�=B
�DB
�DB
��B
��B
�B
�xB
��B
�B
�B
��B
��B
��B
�xB
��B
�B
�JB
��B
�JB
�B
�B
��B
�B
�PB
��B
��B
�PB
�PB
�PB
��B
�PB
��B
��B
��B
�(B
�(B
��B
��B
��B
�(B
�\B
��B
��B
��B
��B
�\B
ϫB
�B
ԕB
�B
�aB
ԕB
�B
�gB
�NB
��B
�gB
�2B
֡B
�[B
�
B
�2B
�[B
�&B
ӏB
��B
ӏB
�aB
�,B
�B
��B
��B
�&B
ҽB
�TB
�TB
��B
�[B
ӏB
��B
��B
�,B
�[B
ҽB
҉B
҉B
҉B
҉B
��B
�&B
ӏB
ԕB
��B
��B
��B
��B
��B
� B
�TB
�&B
��B
�aB
ӏB
ӏB
�&B
ҽB
ҽB
��B
ҽB
ӏB
��B
��B
��B
ӏB
ҽB
��B
��B
҉B
��B
��B
� B
ҽB
ҽB
��B
�&B
�aB
��B
ӏB
ҽB
�TB
��B
҉B
ҽB
��B
��B
��B
��B
�aB
�,B
ӏB
�&B
ҽB
҉B
҉B
҉B
��B
ӏB
�aB
�aB
��B
��B
�[B
�[B
�[B
��B
�&B
�&B
�&B
�&B
��B
҉B
�TB
ҽB
ҽB
ӏB
ӏB
ӏB
�aB
�aB
ԕB
��B
ԕB
��B
�aB
�aB
�[B
ҽB
�TB
ҽB
��B
҉B
��B
ӏB
��B
�aB
�aB
��B
ԕB
��B
�aB
��B
��B
ҽB
ҽB
ҽB
҉B
��B
�[B
�,B
�aB
ԕB
�,B
��B
ԕB
ԕB
ԕB
�aB
��B
��B
ҽB
҉B
��B
�&B
ӏB
��B
�aB
�aB
��B
�aB
ԕB
�aB
�,B
�aB
��B
�[B
�&B
��B
��B
��B
��B
�aB
��B
��B
�,B
��B
�&B
�&B
ҽB
��B
�&B
�[B
��B
�aB
��B
ԕB
��B
�,B
ӏB
�[B
�&B
�&B
�[B
�[B
��B
ԕB
ԕB
ԕB
ԕB
�[B
�,B
ӏB
ӏB
��B
��B
�&B
�aB
ԕB
��B
��B
��B
�aB
�,B
��B
�&B
�&B
��B
ӏB
��B
��B
ԕB
�2B
�aB
��B
�[B
ҽB
ҽB
��B
��B
�,B
ԕB
��B
��B
��B
ӏB
�&B
�&B
�[B
��B
�aB
�aB
��B
��B
��B
�aB
ӏB
�&B
ҽB
�[B
��B
ԕB
�aB
�2B
��B
�,B
��B
�[B
�&B
��B
��B
�[B
��B
�2B
��B
�gB
ԕB
��B
��B
��B
��B
�&B
�,B
ԕB
��B
��B
҉B
ҽB
ҽB
�[B
ԕB
��B
��B
��B
�[B
�TB
ҽB
��B
�,B
ӏB
�TB
� B
ҽB
�TB
��B
��B
�&B
�TB
ѷB
уB
� B
��B
҉B
҉B
� B
��B
�HB
ϫB
�BB
ϫB
�HB
�B
ΥB
�jB
�dB
̘B
̘B
��B
�^B
ȴB
�zB
ƨB
ȀB
��B
҉B
�B
�/B
�8B�B,=BK^B�_B�B��B�xB�1BǮB��B�?B��B��B�jB�TB��B��BߤB��B�B�BCB,qB!�B �B"�B#�B$B"�B �B!bB#B(�B%B,B,�B:�BLdBJ�BVBS[B\�BYKB`BBYKB^B`vBjB_;Bb�BdZBsMB�xBpoBo Bo5Bw�BncBcBm�BiBiDBg�Bg�Bh
BffBe�Bm)BzxBkBiBg8Be�Bd�Bd&Bb�Bq�Bg�BaB��Be,Bh�Bi�BjBaHBd�B`BB^By�BVmBVmBN�BXyBrBI�BF?BFtB@�B=�B>�BEBB�BE9B?�B9�B=�B;�B;�B8�B2aB7LB33B5?BFB9XB2aB2aB:*B1�B,�B+kB.IB%FB'�B)�B-�B$B$@B.B%�B&B#:B!�B$�B!B!�B'BB,�BB&B�B=BB$@B�B
=B�BB�VB�lB�B��B��B�ZB��B�TB�B�WB�,B�B��B�BB��B��B�B�
B� B�2B�0B�B�B��B��B��B�RB�B�LB�B�4B��B��B�kB�=B��B��B��B��B�MB�FB�uB�{B��B}�B��B�BxBb�Bb�B_�Be�Bh
BXBbBFtBF?BK�B^5B/B0UB-wB=<B �BYBL�B	lB$�B
��B
�iB
��B
�B
��B
ٴB
��B
�mB
��B
��B
��B
�TB
��B
� B
бB
�#B
�dB
ںB
�3B
�B
�OB
��B
�qB
�*B
�*B
��B
��B
��B
�tB
�hB
��B
�B
t�B
rB
[�B
ZQB
_pB
W�B
P}B
P�B
^�B
OBB
F�B
CaB
=�B
;�B
6�B
6�B
@�B
2�B
3hB
,qB
?HB
\�B	��B	�WB	��B	�B	��B	�>B	�B	�B	� B	�)B	�#B	�B	��B	��B	�2B	רB	�vB	��B	�pB	�BB	�^B	��B	�^B	�B	��B	��B	��B	��B	�6B	��B	�wB	�XB	��B	��B	�B	��B	��B	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     B
�
B
�mB
�B
�mB
��B
�yB
�gB
�gB
�gB
��B
˛B
˛B
�gB
�gB
�gB
�gB
�gB
�gB
�2B
�2B
�2B
�2B
�2B
�gB
�gB
˛B
˛B
˛B
˛B
��B
˛B
��B
��B
˛B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
��B
˛B
��B
�,B
ǷB
�dB
�iB�*B��B	�BtB&�BOKBY�Bj�Bi�Bb"B_yBc�B]BU�B;9B7�B/�B0�B#B!�BBB�B��B�fBڎBوB�HB��B�IB��B�~B\�BG�B&�B�B
؂B
ͧB
�|B
�vB
��B
��B
W|B
H�B
3�B
7TB	��B	�iB	ǷB	��B	��B	��B	zMB	u�B	n�B	`�B	OB	<�B	-B	(�B	&�B	#BB	RB	bB	B	'B��B�bB�oB��B�JBݡB��B��B�#B��B�)B�yB�5B�#B�BέB�BέB��B��B��B��B�9B��B�HB�3B�B�?B�NB�[B�B�B��B�yB�/B�pBҒB�BާB�WB��B�B��B�%B	B	
B	kB	B	�B	$B	 dB	$�B	%�B	@B	XB	hB	[B	�B	2�B	4�B	4�B	3B	+�B	S�B	s"B	s�B	t\B	t\B	v�B	dcB	OB	KgB	M
B	S�B	Q�B	LmB	P�B	a�B	q�B	wB	u�B	{�B	�~B	��B	�!B	��B	�rB	~1B	}�B	|%B	zB	yGB	|�B	{B	z�B	yGB	z�B	{SB	�VB	�0B	�wB	�qB	�!B	�BB	�}B	�'B	�9B	�mB	�?B	�?B	�?B	�?B	�B	��B	��B	��B	�6B	�B	�TB	��B	��B	�NB	��B	�3B	��B	�gB	��B	�3B	�gB	��B	��B	�mB	�9B	�9B	�sB	�B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	�/B	B	�dB	��B	ÞB	��B	��B	�jB	�B	�B	ǷB	�B	�B	ȽB	�B	��B	ɏB	ʕB	�sB	кB	�#B	�#B	кB	�)B	��B	�B	кB	έB	��B	˛B	��B	�sB	�KB	ЅB	�WB	ыB	�)B	�)B	�)B	��B	��B	�cB	װB	��B	��B	�rB	�
B	�rB	�
B	�>B	�B	�DB	�B	��B	�B	��B	�B	�PB	��B	�cB	�.B	�]B	� B	� B	�iB	�iB	��B	�oB	��B	�{B	��B	�{B	�;B	�B	�B	�B	�AB	��B	�B	�MB	�SB	�%B	�B	�%B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�VB	��B	��B	�(B	��B	��B	��B	��B	�\B	��B	�4B	��B	�B	�B	�uB	�uB	�B	�uB	�:B	�GB	�{B	��B	��B	��B	��B	��B	��B	�B	�SB	��B	��B	�_B	��B	��B	��B	�_B	��B	��B	�kB	�kB	��B	�kB	��B
 rB
 �B
 rB
 �B
CB
xB
�B
�B
B
~B
B
~B
~B
PB
!B
!B
�B
�B
�B
�B
�B
�B
bB
�B
bB
hB
:B
�B
:B
	B

{B
B

�B
�B
RB
�B
�B
YB
�B
�B
�B
�B
B
CB
wB
�B
�B
}B
B
B
�B
�B
�B
UB
�B
�B
�B
�B
�B
�B
�B
hB
�B
3B
3B
hB
�B
�B
9B
9B
�B
tB
�B
�B
B
�B
RB
$B
�B
�B
�B
�B
*B
�B
�B
�B
�B
 0B
!�B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
#wB
#�B
$}B
%OB
%OB
%OB
&UB
&�B
''B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
*mB
*9B
*�B
+?B
+�B
,zB
+tB
,B
,�B
-�B
-�B
-�B
-KB
,�B
.RB
.�B
.�B
.�B
/#B
.RB
.B
.B
.�B
/XB
.�B
/XB
0)B
0�B
0^B
1dB
1�B
10B
26B
26B
26B
2�B
3�B
4vB
3�B
4B
4B
4�B
4�B
4�B
5B
5B
5HB
5�B
5HB
6�B
6�B
6NB
6�B
6�B
6�B
6�B
8[B
7TB
7�B
7TB
7�B
7TB
7TB
7�B
7�B
8[B
8&B
7�B
8�B
9�B
9,B
9�B
9�B
9,B
9�B
9�B
:gB
;B
<
B
<
B
;�B
;�B
<
B
<�B
<
B
;�B
<sB
<�B
<�B
=�B
>B
>B
=�B
>�B
?�B
?�B
@�B
A�B
A�B
A�B
B/B
C5B
CB
B�B
CB
CB
D<B
CjB
C5B
DB
D�B
EBB
EB
EBB
D�B
D�B
FB
F�B
G�B
G�B
H B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
IZB
J,B
J�B
J`B
J,B
K2B
KgB
K�B
K�B
KgB
K�B
LB
L8B
K�B
LB
L�B
M>B
M
B
L�B
LmB
M
B
NEB
N�B
OKB
OKB
OB
OB
N�B
OKB
O�B
OB
OB
OB
O�B
PQB
PQB
P�B
P�B
P�B
P�B
P�B
Q#B
P�B
P�B
Q#B
QWB
R]B
Q�B
R)B
R)B
Q�B
R)B
ScB
S�B
S�B
S�B
ScB
S/B
S/B
T5B
U;B
U�B
UpB
UpB
UpB
U�B
WB
VAB
VvB
W�B
W|B
W�B
XNB
XB
W�B
XNB
XB
X�B
XNB
XB
X�B
YB
YB
X�B
YB
YB
ZZB
Z%B
Z%B
Y�B
Z�B
Z%B
Z�B
[`B
[,B
[�B
[�B
[,B
[`B
[�B
[�B
\fB
]B
]8B
]lB
]8B
]B
]�B
]lB
]�B
^>B
^>B
^
B
^
B
^�B
^�B
_DB
_�B
`JB
`B
_�B
_�B
_�B
`JB
`B
`JB
aB
aPB
`�B
a�B
a�B
`�B
b"B
bWB
bWB
a�B
bWB
a�B
c]B
b�B
c]B
c�B
d�B
d�B
d�B
d�B
d�B
c�B
c�B
dcB
eiB
e5B
d�B
e5B
f�B
f;B
f;B
fB
foB
gB
foB
f�B
gB
f�B
gB
gB
g�B
guB
g�B
hGB
h{B
hB
hB
g�B
h�B
iB
h�B
i�B
i�B
k�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
kYB
k�B
k�B
kYB
kYB
l+B
mfB
m�B
mfB
l�B
m1B
m�B
n7B
nlB
nlB
o	B
o>B
n�B
n�B
o>B
orB
pDB
p�B
p�B
p�B
pxB
pB
o�B
pxB
p�B
qJB
q~B
q�B
qB
qB
q~B
qJB
q~B
q�B
r�B
sVB
sVB
s�B
s�B
sVB
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t\B
t(B
t(B
t�B
u.B
ubB
u�B
u.B
u.B
u.B
ubB
v B
v�B
wB
w:B
w�B
w:B
wB
w�B
x@B
x�B
xuB
x�B
xuB
xuB
x@B
x�B
yGB
y{B
yGB
y{B
y�B
zB
z�B
z�B
z�B
z�B
{B
|YB
|�B
|�B
|�B
|�B
|%B
|�B
}�B
}�B
}+B
}_B
~1B
~�B
~�B
~eB
�B
B
�B
7B
�	B
�=B
�B
��B
�=B
��B
�=B
�B
��B
��B
��B
�xB
�B
��B
��B
�xB
�CB
��B
��B
�IB
�IB
�PB
�PB
��B
��B
�B
��B
��B
�!B
�!B
��B
��B
��B
��B
��B
�!B
�VB
��B
�VB
�!B
�!B
��B
�'B
�\B
��B
��B
�\B
�\B
�\B
��B
�\B
��B
��B
��B
�4B
�4B
��B
��B
��B
�4B
�hB
��B
��B
��B
��B
�hB
ǷB
�B
̡B
�B
�mB
̡B
�B
�sB
�ZB
��B
�sB
�>B
έB
�gB
�B
�>B
�gB
�2B
˛B
��B
˛B
�mB
�8B
�B
��B
�B
�2B
��B
�`B
�`B
��B
�gB
˛B
�B
��B
�8B
�gB
��B
ʕB
ʕB
ʕB
ʕB
��B
�2B
˛B
̡B
��B
��B
��B
��B
��B
�,B
�`B
�2B
��B
�mB
˛B
˛B
�2B
��B
��B
��B
��B
˛B
��B
�B
�B
˛B
��B
��B
��B
ʕB
��B
��B
�,B
��B
��B
��B
�2B
�mB
�B
˛B
��B
�`B
��B
ʕB
��B
��B
��B
�B
��B
�mB
�8B
˛B
�2B
��B
ʕB
ʕB
ʕB
��B
˛B
�mB
�mB
��B
�B
�gB
�gB
�gB
��B
�2B
�2B
�2B
�2B
��B
ʕB
�`B
��B
��B
˛B
˛B
˛B
�mB
�mB
̡B
�
B
̡B
��B
�mB
�mB
�gB
��B
�`B
��B
��B
ʕB
��B
˛B
�B
�mB
�mB
��B
̡B
��B
�mB
��B
��B
��B
��B
��B
ʕB
��B
�gB
�8B
�mB
̡B
�8B
��B
̡B
̡B
̡B
�mB
��B
��B
��B
ʕB
��B
�2B
˛B
��B
�mB
�mB
��B
�mB
̡B
�mB
�8B
�mB
�B
�gB
�2B
��B
��B
��B
��B
�mB
�
B
��B
�8B
�B
�2B
�2B
��B
��B
�2B
�gB
�B
�mB
��B
̡B
�
B
�8B
˛B
�gB
�2B
�2B
�gB
�gB
�B
̡B
̡B
̡B
̡B
�gB
�8B
˛B
˛B
��B
��B
�2B
�mB
̡B
��B
�
B
��B
�mB
�8B
�B
�2B
�2B
��B
˛B
��B
��B
̡B
�>B
�mB
�B
�gB
��B
��B
��B
�B
�8B
̡B
�
B
��B
�B
˛B
�2B
�2B
�gB
�B
�mB
�mB
�
B
�
B
��B
�mB
˛B
�2B
��B
�gB
�B
̡B
�mB
�>B
��B
�8B
��B
�gB
�2B
��B
��B
�gB
�B
�>B
�
B
�sB
̡B
��B
��B
��B
��B
�2B
�8B
̡B
��B
��B
ʕB
��B
��B
�gB
̡B
��B
�
B
�B
�gB
�`B
��B
��B
�8B
˛B
�`B
�,B
��B
�`B
�B
�B
�2B
�`B
��B
ɏB
�,B
��B
ʕB
ʕB
�,B
��B
�TB
ǷB
�NB
ǷB
�TB
� B
ƱB
�vB
�pB
ĤB
ĤB
��B
�jB
��B
��B
��B
��B
��B
ʕB
�B
�;B
�DB
�B$IBCjB�kB�B��B��B�=B��B��B�KB��B�B�vB�`B�B�BװB��B
�B�BOB$}B�B�B�B�BB�BBnBB!BB$B$�B2�BDpBB�BNBKgBT�BQWBXNBQWBVBX�Bb�BWGBZ�B\fBkYB��Bh{BgBgABo�BfoBwoBe�BaBaPB_�B_�B`B^rB]�Be5Br�Bc(BaB_DB]�B\�B\2BZ�Bi�B_�BYB��B]8B`�Ba�Bb"BYTB]BXNBVBq�BNyBNyBF�BP�BjBA�B>KB>�B8�B5�B6�B=B:�B=EB7�B2B5�B3�B3�B0�B*mB/XB+?B-KB>B1dB*mB*mB26B*B$�B#wB&UBRB�B"B%�BBLB& B�B$BFBB�B-B�B*BB$�B'B$B�BIBBLB�BIB�B�B�bB�xB�B��B�B�fB�B�`B�B�cB�8BݡB��B�NB��B��B۔B�B�,B�>B�<B�%B� B��B��B��B�^B�B�XB�B�@B��B��B�wB�IB��B�B��B��B�YB�RB��B��B��Bu�B�B{BpBZ�BZ�BW�B^
B`BPBZ%B>�B>KBC�BVAB''B(aB%�B5HB�BeBD�BxB�B
�B
�uB
��B
�B
��B
��B
��B
�yB
��B
��B
��B
�`B
��B
�,B
ȽB
�/B
�pB
��B
�?B
�)B
�[B
��B
�}B
�6B
�6B
��B
��B
��B
��B
�tB
}�B
yB
l�B
jB
S�B
R]B
W|B
O�B
H�B
H�B
V�B
GNB
>�B
;mB
5�B
3�B
.�B
.�B
8�B
*�B
+tB
$}B
7TB
T�B	�B	�cB	��B	ݡB	��B	�JB	�B	�%B	�,B	�5B	�/B	�B	��B	��B	�>B	ϴB	ǂB	��B	�|B	�NB	�jB	�B	�jB	�B	��B	�B	��B	��B	�BB	��B	��B	�dB	��B	��B	� B	��B	�B	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223218                            20230426223218AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622321820230426223218  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622321820230426223218QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622321820230426223218QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               