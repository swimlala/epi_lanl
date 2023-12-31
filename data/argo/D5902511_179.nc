CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-08-16T19:16:04Z creation; 2022-02-04T23:30:03Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       b�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       �0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � -�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      5   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � S   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      Z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` x�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   y   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                      SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20210816191604  20220204223516  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_179                 6810_008521_179                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٌ/h��b@ٌ/h��b11  @ٌ/���@ٌ/���@2 t�q�q@2 t�q�q�d���i�d���i11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?��H@@  @��\@��\@�  @޸R@�(�A\)A   A*=qA@  A`  A�  A�Q�A��A�  A�  A�Q�A��A�Q�B   B(�B  B(�B (�B(  B0  B8(�B@(�BG�
BO�
BX  B`(�Bh(�Bp(�Bx  B�  B�{B�  B��B�  B�  B��
B�  B�{B�  B��B�{B�  B��
B��B�  B��B��B�  B��B�  B�{B�  B��B��
B��
B�  B�  B��
B��B�  B��C   C
=C  C  C
=C

=C
=C  C  C��C��C
=C
=C
=C
=C
=C {C"
=C$
=C&{C(
=C)��C+��C-��C0
=C2  C4  C5��C8
=C:
=C<
=C>{C@
=CB
=CD{CF{CH  CI��CK��CN  CP
=CR  CS��CU��CW��CZ  C\  C^  C_��Cb  Cd  Ce��Ch
=Cj  Ck��Cn  Co��Cr  Cs��Cv  Cw��Cy�C|  C~  C�  C���C�  C�C�  C�C���C���C�C�C�C���C�  C���C�  C�  C�  C�C�  C���C���C�  C��C���C���C���C�  C�  C���C���C�  C�  C�C�  C�C���C���C�  C�C�C�C�  C���C���C���C���C���C�C�
=C�  C�C���C���C���C���C�C���C�  C�C�  C�C���C���C���C���C���C�  C���C�C�  C�
=C�\C�C���C�
=C�C�C���C�  C�C�C���C�  C���C�C���C���C���C�  C�  C�
=C�  C���C���C���C�C�
=C�C���C�  C�
=C�  C���C�  C���C�  C���C�  C�  C���C���C�  C�  C�  C�  C���C���C���C��C�C�
=C�  C�  C�C���C�C�
=C�  D �D ��D�D}qD��D� D  DxRD��D}qD�qD�DD� D  Dz�D��D}qD	�D	�D
�D
��D�D� D  D��D�D� D�qDz�D�qD}qD�qD}qD  D}qD  D��DD��DD�D�qDz�D  D��D�qDz�D�qDz�D�RD}qD��D� D  D� D�qD��DD� D  D� D  D��D D �D!�D!}qD!��D"}qD#  D#��D$�D$�D%  D%z�D%�qD&��D'  D'� D(�D(�D)D)� D*  D*}qD+  D+� D,�D,�D-  D-}qD.  D.}qD.��D/� D0�D0� D0�qD1}qD1�qD2}qD2��D3}qD4  D4� D5  D5�D6�D6� D7  D7� D8  D8��D8�qD9��D:�D:� D;  D;}qD<�D<��D=  D=��D>�D>��D?  D?� D@  D@��DA�DA��DB  DB}qDC�DC��DC�qDD}qDE  DE� DE�qDF� DF�qDG��DH�DH��DH�qDI��DJ  DJ}qDK  DK��DL  DL� DM  DM��DN  DN� DN�qDO� DP  DP��DQ�DQ� DR�DR}qDR�qDS}qDS�qDT}qDU�DU� DV  DV� DV�qDW� DX  DX}qDX�qDY}qDY�qDZ� D[�D[� D\�D\� D]  D]�D^  D^z�D^�qD_��D`�D`� Da  Da��Db�Db� Dc  Dc� Dc�qDd� De  De}qDf�Df� Df��Dg� Dh�Dh� Di�Di� Dj�Dj��Dk�Dk}qDl�Dl� Dm  Dm� Dn�Dn� Do�Do��Dp  Dp� Dq  Dq}qDr�Dr� Dr�qDs� Ds��Dt}qDu  Du}qDu�qDv}qDv�qDw}qDx  Dx� Dx�qDy��Dz�Dz� D{  D{��D{�qD|z�D|�qD}� D~�D~� D  D��D�  D�AHD�� D��HD�  D�@ D�~�D��HD�HD�@ D��HD��HD���D�>�D��HD���D���D�@ D�~�D���D�  D�AHD�� D�� D�  D�AHD��HD��HD�HD�AHD��HD���D��qD�>�D�� D�� D�  D�@ D�~�D��qD��qD�=qD�~�D�� D���D�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D�� D���D�  D�B�D��HD���D�HD�AHD�}qD�� D�HD�@ D�� D���D�HD�AHD��HD��HD���D�<)D�~�D�� D�  D�AHD��HD��HD���D�AHD�� D��qD���D�=qD�}qD��HD��D�@ D�~�D�� D�  D�@ D�~�D�� D��qD�=qD�� D��HD�HD�@ D�}qD��qD���D�>�D�� D�D�HD�B�D��HD���D���D�@ D��HD��HD��D�AHD�}qD���D�  D�AHD���D�D�  D�>�D�~�D���D�HD�@ D�~�D���D���D�@ D��HD�� D���D�@ D���D��HD���D�AHD��HD��qD�  D�B�D�� D���D���D�>�D�~�D�� D�HD�@ D�� D�D�HD�@ D��HD��HD�HD�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD���D���D�AHD��HD��HD�  D�>�D�� D���D���D�@ D��HD�� D�HD�AHD�~�D�� D�  D�B�D��HD�� D�  D�AHD�� D���D���D�@ D�� D���D���D�@ D�� D���D���D�B�D�� D�� D���D�@ D��HD�� D�  D�B�D�~�D��)D���D�AHD�� D�� D�HD�>�D�~�D��HD�HD�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D��HD��HD��D�AHD�~�D�� D�  D�@ D�� D���D���D�>�D�}qD���D�HD�AHD�~�D���D�  D�B�D�� D��qD���D�>�D�~�D�� D�  D�@ DÁHDþ�D�  D�AHDĀ D��HD��D�AHDł�D�D��D�@ Dƀ D�D��D�@ DǁHD�D�HD�AHDȂ�D��HD��qD�<)D�|)D�� D��D�AHDʁHD�� D���D�>�DˁHD˾�D��qD�@ D̂�D�D�  D�=qD�}qD�� D�HD�AHD΁HD��HD�HD�B�DρHD��HD�  D�@ DЁHD��HD�  D�>�D�~�DѾ�D���D�>�DҀ D��HD�  D�AHDӁHD�� D��D�AHDԀ D�� D�  D�AHDՁHDվ�D���D�AHDփ�D��HD�  D�AHDׁHD�� D�HD�C�D؂�D�D�HD�>�D�~�Dپ�D���D�>�Dڀ D��HD���D�@ Dۂ�D��HD�  D�@ D܀ D��HD��D�>�D�}qDݾ�D���D�>�D�~�D޾�D�  D�B�D߂�D��HD�HD�AHD�~�DྸD�HD�C�D� D�qD�  D�B�D�HD�� D��D�B�D�~�D�� D�  D�>�D� D��HD�HD�>�D� D��HD�HD�>�D�~�D澸D���D�@ D� D�� D��qD�>�D�~�D�)D��qD�=qD�~�D�� D�  D�AHD�HD�D�HD�@ D�~�D�� D�  D�@ D�HD쾸D��qD�>�D� D��HD�HD�=qD�~�D�� D��q>u>�G�?W
=?��R?\?�@z�@!G�@:�H@Tz�@fff@�G�@��@�z�@��
@�\)@�Q�@Ǯ@�33@�p�@�@�
=A ��AQ�A{A33A=qA ��A%�A,(�A333A8Q�A=p�AE�AJ=qAO\)AVffA\(�A`��Ag
=Amp�Ar�\Aw�A~�RA��\A��A��A�33A�ffA���A��
A�
=A�G�A��
A�\)A�=qA�z�A�\)A��\A�p�A�  A��\A�A�G�A�33A�{A��A���A�\)A��HA�ffAУ�A��
A׮A��HA�p�A�Q�A��
A�RA�G�A���A�  A�=qA�p�A���A��A�ffB�BffB�BG�B�RB  B	��B33BQ�Bp�B
=B��B��B
=Bz�BG�B�RB(�B��B�B33B(�B��B{B
=B�B ��B!��B"{B#33B$  B$z�B%��B&�\B'33B'�
B(��B)��B*{B+33B,(�B,��B-p�B.�\B/\)B/�
B0��B1�B2�\B3\)B4z�B5�B5B6�RB7�
B8z�B9G�B:=qB;\)B;�
B<��B=�B>�RB?33B@z�BAp�BA�BB�RBC�
BD��BE�BFffBG
=BG�BH��BI��BJ=qBK33BLQ�BL��BMBN�RBO\)BP  BQG�BR{BR�\BS�BT��BU�BV=qBW33BW�BXz�BYBZ=qB[
=B\  B\��B]p�B^ffB_\)B`  B`��BaBb�\Bc
=Bd  Bd��BeBf=qBg\)Bh  Bhz�Bip�Bj�\Bk33Bk�Bl��Bm��BnffBn�HBo�Bp��Bq�BqBr�HBs�Bt  Bt��Bu�Bv�\Bw
=Bw�
Bx��By�By�Bz�HB{�B|  B|��B}��B~ffB~�RB\)B�(�B��\B���B�
=B�p�B��B�  B�Q�B���B�33B�G�B��B�(�B�Q�B��\B�
=B�\)B��B�B�=qB��\B��RB��B��B�B��B�ffB���B�
=B�G�B��B�{B�Q�B��\B�
=B�\)B���B��B�z�B��RB�
=B�p�B��B�(�B�z�B��HB�\)B���B��B�z�B��HB��B��B�  B��\B��HB��B��B�  B�z�B��RB�
=B���B�  B�Q�B��\B��B���B��
B�{B���B��B�\)B�B�=qB���B��HB�\)B��
B�Q�B��\B���B�p�B��B�{B���B��B�p�B��B�{B���B�
=B�G�B��B�{B��\B��HB��B��B�{B�Q�B��\B��B���B��B�(�B���B��B�p�B�B�=qB��RB���B�G�B��B�Q�B��\B���B��B�  B�Q�B��\B�
=B��B��B�(�B���B�33B��B�B�Q�B���B�
=B�\)B�  B�ffB���B�G�B�B�{B�ffB���B�G�B��
B�(�B�z�B���B��B��B�(�B���B��B���B��B�=qB���B�G�B��B��B�z�B���B�33B��B�{B��\B���B�G�B���B�(�B��RB���B�\)B��
B�Q�B£�B���BÅB�  B�Q�Bģ�B�
=BŅB�  B�Q�Bƣ�B��BǮB�  B�Q�BȸRB�G�Bə�B��
B�ffB��HB��B�p�B��
B�ffB��HB�
=BͅB�  BΏ\B��HB�33Bϙ�B�(�BиRB��B�p�B�  Bҏ\B��BӅB��B�Q�B���B�p�B��
B�(�B֣�B�33B�B�{B�ffB���BمB�  B�Q�B���B�\)B�  B�Q�Bܣ�B�33B�B�=qB޸RB�
=B߅B�{B��B���B�p�B�{B�z�B���B�p�B�  B�ffB�RB�33B�B�Q�B�RB�
=B�B�(�B�RB�
=B�p�B�  B�\B��HB�\)B��B�ffB���B�33B�B�ffB�RB�33B��
B�Q�B�RB��B�B�Q�B�RB��B�B�=qB��RB��B���B�=qB��RB�
=B��B�=qB���B���B��B�{B���B�
=B��B�{B���B�
=B��B�(�B��RB�
=B��C 
=C ffC ��C ��C�CffC��C��C�CffC�C��C�C\)C�C��C�CffC�\CC�CffC��C�
C(�Cp�C��C�HC33Cz�C�RC�HC33C�C�RC��C	G�C	�C	�RC
  C
\)C
�\C
C
=C\)C��C��C
=CQ�C��C�
C
=CQ�C��C��C  CQ�C��C�HC
=CG�C��C�
C  CG�C��C�
C{CG�C��C�C{CQ�C��C�C33CQ�C��C�C(�C\)C��C�HC33Cz�C��C�C=qC�C�RC��C=qC�\C�
C
=CG�C�C�
C{CG�C�C��C�CffC�C�C�C\)C��C��C(�C\)C�C��C33C\)C��C�HC(�Cz�C�C�HC�Cp�C�RC   C =qC p�C �C!  C!G�C!z�C!�RC!�C"=qC"�\C"C"�C#=qC#�\C#��C$  C$=qC$z�C$��C%�C%\)C%��C%��C&{C&p�C&�C&�C'�C'ffC'�RC(  C(G�C(�\C(C)  C)=qC)�\C)�
C*(�C*ffC*��C*��C+G�C+�C+�RC,  C,=qC,�C,�HC-�C-\)C-��C-�
C.(�C.z�C.C.��C/=qC/�C/�
C0(�C0ffC0��C0�HC1(�C1�C1�
C2{C2Q�C2��C2��C3G�C3�C3�RC4  C4\)C4�C4�C5(�C5p�C5��C6{C6Q�C6�\C6�HC7=qC7�\C7��C8
=C8\)C8�C9  C9G�C9�C9C:
=C:ffC:�RC;  C;=qC;z�C;��C<�C<p�C<�RC<��C=33C=z�C=�
C>(�C>z�C>�RC?  C?Q�C?�RC@  C@G�C@�C@�
CA33CA�\CA�
CB(�CBp�CB�RCC  CCQ�CC�CC��CDG�CD�\CD��CE{CEp�CECF{CFffCF�CF��CG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                           ?��?��H@@  @��\@��\@�  @޸R@�(�A\)A   A*=qA@  A`  A�  A�Q�A��A�  A�  A�Q�A��A�Q�B   B(�B  B(�B (�B(  B0  B8(�B@(�BG�
BO�
BX  B`(�Bh(�Bp(�Bx  B�  B�{B�  B��B�  B�  B��
B�  B�{B�  B��B�{B�  B��
B��B�  B��B��B�  B��B�  B�{B�  B��B��
B��
B�  B�  B��
B��B�  B��C   C
=C  C  C
=C

=C
=C  C  C��C��C
=C
=C
=C
=C
=C {C"
=C$
=C&{C(
=C)��C+��C-��C0
=C2  C4  C5��C8
=C:
=C<
=C>{C@
=CB
=CD{CF{CH  CI��CK��CN  CP
=CR  CS��CU��CW��CZ  C\  C^  C_��Cb  Cd  Ce��Ch
=Cj  Ck��Cn  Co��Cr  Cs��Cv  Cw��Cy�C|  C~  C�  C���C�  C�C�  C�C���C���C�C�C�C���C�  C���C�  C�  C�  C�C�  C���C���C�  C��C���C���C���C�  C�  C���C���C�  C�  C�C�  C�C���C���C�  C�C�C�C�  C���C���C���C���C���C�C�
=C�  C�C���C���C���C���C�C���C�  C�C�  C�C���C���C���C���C���C�  C���C�C�  C�
=C�\C�C���C�
=C�C�C���C�  C�C�C���C�  C���C�C���C���C���C�  C�  C�
=C�  C���C���C���C�C�
=C�C���C�  C�
=C�  C���C�  C���C�  C���C�  C�  C���C���C�  C�  C�  C�  C���C���C���C��C�C�
=C�  C�  C�C���C�C�
=C�  D �D ��D�D}qD��D� D  DxRD��D}qD�qD�DD� D  Dz�D��D}qD	�D	�D
�D
��D�D� D  D��D�D� D�qDz�D�qD}qD�qD}qD  D}qD  D��DD��DD�D�qDz�D  D��D�qDz�D�qDz�D�RD}qD��D� D  D� D�qD��DD� D  D� D  D��D D �D!�D!}qD!��D"}qD#  D#��D$�D$�D%  D%z�D%�qD&��D'  D'� D(�D(�D)D)� D*  D*}qD+  D+� D,�D,�D-  D-}qD.  D.}qD.��D/� D0�D0� D0�qD1}qD1�qD2}qD2��D3}qD4  D4� D5  D5�D6�D6� D7  D7� D8  D8��D8�qD9��D:�D:� D;  D;}qD<�D<��D=  D=��D>�D>��D?  D?� D@  D@��DA�DA��DB  DB}qDC�DC��DC�qDD}qDE  DE� DE�qDF� DF�qDG��DH�DH��DH�qDI��DJ  DJ}qDK  DK��DL  DL� DM  DM��DN  DN� DN�qDO� DP  DP��DQ�DQ� DR�DR}qDR�qDS}qDS�qDT}qDU�DU� DV  DV� DV�qDW� DX  DX}qDX�qDY}qDY�qDZ� D[�D[� D\�D\� D]  D]�D^  D^z�D^�qD_��D`�D`� Da  Da��Db�Db� Dc  Dc� Dc�qDd� De  De}qDf�Df� Df��Dg� Dh�Dh� Di�Di� Dj�Dj��Dk�Dk}qDl�Dl� Dm  Dm� Dn�Dn� Do�Do��Dp  Dp� Dq  Dq}qDr�Dr� Dr�qDs� Ds��Dt}qDu  Du}qDu�qDv}qDv�qDw}qDx  Dx� Dx�qDy��Dz�Dz� D{  D{��D{�qD|z�D|�qD}� D~�D~� D  D��D�  D�AHD�� D��HD�  D�@ D�~�D��HD�HD�@ D��HD��HD���D�>�D��HD���D���D�@ D�~�D���D�  D�AHD�� D�� D�  D�AHD��HD��HD�HD�AHD��HD���D��qD�>�D�� D�� D�  D�@ D�~�D��qD��qD�=qD�~�D�� D���D�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D�� D���D�  D�B�D��HD���D�HD�AHD�}qD�� D�HD�@ D�� D���D�HD�AHD��HD��HD���D�<)D�~�D�� D�  D�AHD��HD��HD���D�AHD�� D��qD���D�=qD�}qD��HD��D�@ D�~�D�� D�  D�@ D�~�D�� D��qD�=qD�� D��HD�HD�@ D�}qD��qD���D�>�D�� D�D�HD�B�D��HD���D���D�@ D��HD��HD��D�AHD�}qD���D�  D�AHD���D�D�  D�>�D�~�D���D�HD�@ D�~�D���D���D�@ D��HD�� D���D�@ D���D��HD���D�AHD��HD��qD�  D�B�D�� D���D���D�>�D�~�D�� D�HD�@ D�� D�D�HD�@ D��HD��HD�HD�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD���D���D�AHD��HD��HD�  D�>�D�� D���D���D�@ D��HD�� D�HD�AHD�~�D�� D�  D�B�D��HD�� D�  D�AHD�� D���D���D�@ D�� D���D���D�@ D�� D���D���D�B�D�� D�� D���D�@ D��HD�� D�  D�B�D�~�D��)D���D�AHD�� D�� D�HD�>�D�~�D��HD�HD�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D��HD��HD��D�AHD�~�D�� D�  D�@ D�� D���D���D�>�D�}qD���D�HD�AHD�~�D���D�  D�B�D�� D��qD���D�>�D�~�D�� D�  D�@ DÁHDþ�D�  D�AHDĀ D��HD��D�AHDł�D�D��D�@ Dƀ D�D��D�@ DǁHD�D�HD�AHDȂ�D��HD��qD�<)D�|)D�� D��D�AHDʁHD�� D���D�>�DˁHD˾�D��qD�@ D̂�D�D�  D�=qD�}qD�� D�HD�AHD΁HD��HD�HD�B�DρHD��HD�  D�@ DЁHD��HD�  D�>�D�~�DѾ�D���D�>�DҀ D��HD�  D�AHDӁHD�� D��D�AHDԀ D�� D�  D�AHDՁHDվ�D���D�AHDփ�D��HD�  D�AHDׁHD�� D�HD�C�D؂�D�D�HD�>�D�~�Dپ�D���D�>�Dڀ D��HD���D�@ Dۂ�D��HD�  D�@ D܀ D��HD��D�>�D�}qDݾ�D���D�>�D�~�D޾�D�  D�B�D߂�D��HD�HD�AHD�~�DྸD�HD�C�D� D�qD�  D�B�D�HD�� D��D�B�D�~�D�� D�  D�>�D� D��HD�HD�>�D� D��HD�HD�>�D�~�D澸D���D�@ D� D�� D��qD�>�D�~�D�)D��qD�=qD�~�D�� D�  D�AHD�HD�D�HD�@ D�~�D�� D�  D�@ D�HD쾸D��qD�>�D� D��HD�HD�=qD�~�D�� G�O�>u>�G�?W
=?��R?\?�@z�@!G�@:�H@Tz�@fff@�G�@��@�z�@��
@�\)@�Q�@Ǯ@�33@�p�@�@�
=A ��AQ�A{A33A=qA ��A%�A,(�A333A8Q�A=p�AE�AJ=qAO\)AVffA\(�A`��Ag
=Amp�Ar�\Aw�A~�RA��\A��A��A�33A�ffA���A��
A�
=A�G�A��
A�\)A�=qA�z�A�\)A��\A�p�A�  A��\A�A�G�A�33A�{A��A���A�\)A��HA�ffAУ�A��
A׮A��HA�p�A�Q�A��
A�RA�G�A���A�  A�=qA�p�A���A��A�ffB�BffB�BG�B�RB  B	��B33BQ�Bp�B
=B��B��B
=Bz�BG�B�RB(�B��B�B33B(�B��B{B
=B�B ��B!��B"{B#33B$  B$z�B%��B&�\B'33B'�
B(��B)��B*{B+33B,(�B,��B-p�B.�\B/\)B/�
B0��B1�B2�\B3\)B4z�B5�B5B6�RB7�
B8z�B9G�B:=qB;\)B;�
B<��B=�B>�RB?33B@z�BAp�BA�BB�RBC�
BD��BE�BFffBG
=BG�BH��BI��BJ=qBK33BLQ�BL��BMBN�RBO\)BP  BQG�BR{BR�\BS�BT��BU�BV=qBW33BW�BXz�BYBZ=qB[
=B\  B\��B]p�B^ffB_\)B`  B`��BaBb�\Bc
=Bd  Bd��BeBf=qBg\)Bh  Bhz�Bip�Bj�\Bk33Bk�Bl��Bm��BnffBn�HBo�Bp��Bq�BqBr�HBs�Bt  Bt��Bu�Bv�\Bw
=Bw�
Bx��By�By�Bz�HB{�B|  B|��B}��B~ffB~�RB\)B�(�B��\B���B�
=B�p�B��B�  B�Q�B���B�33B�G�B��B�(�B�Q�B��\B�
=B�\)B��B�B�=qB��\B��RB��B��B�B��B�ffB���B�
=B�G�B��B�{B�Q�B��\B�
=B�\)B���B��B�z�B��RB�
=B�p�B��B�(�B�z�B��HB�\)B���B��B�z�B��HB��B��B�  B��\B��HB��B��B�  B�z�B��RB�
=B���B�  B�Q�B��\B��B���B��
B�{B���B��B�\)B�B�=qB���B��HB�\)B��
B�Q�B��\B���B�p�B��B�{B���B��B�p�B��B�{B���B�
=B�G�B��B�{B��\B��HB��B��B�{B�Q�B��\B��B���B��B�(�B���B��B�p�B�B�=qB��RB���B�G�B��B�Q�B��\B���B��B�  B�Q�B��\B�
=B��B��B�(�B���B�33B��B�B�Q�B���B�
=B�\)B�  B�ffB���B�G�B�B�{B�ffB���B�G�B��
B�(�B�z�B���B��B��B�(�B���B��B���B��B�=qB���B�G�B��B��B�z�B���B�33B��B�{B��\B���B�G�B���B�(�B��RB���B�\)B��
B�Q�B£�B���BÅB�  B�Q�Bģ�B�
=BŅB�  B�Q�Bƣ�B��BǮB�  B�Q�BȸRB�G�Bə�B��
B�ffB��HB��B�p�B��
B�ffB��HB�
=BͅB�  BΏ\B��HB�33Bϙ�B�(�BиRB��B�p�B�  Bҏ\B��BӅB��B�Q�B���B�p�B��
B�(�B֣�B�33B�B�{B�ffB���BمB�  B�Q�B���B�\)B�  B�Q�Bܣ�B�33B�B�=qB޸RB�
=B߅B�{B��B���B�p�B�{B�z�B���B�p�B�  B�ffB�RB�33B�B�Q�B�RB�
=B�B�(�B�RB�
=B�p�B�  B�\B��HB�\)B��B�ffB���B�33B�B�ffB�RB�33B��
B�Q�B�RB��B�B�Q�B�RB��B�B�=qB��RB��B���B�=qB��RB�
=B��B�=qB���B���B��B�{B���B�
=B��B�{B���B�
=B��B�(�B��RB�
=B��C 
=C ffC ��C ��C�CffC��C��C�CffC�C��C�C\)C�C��C�CffC�\CC�CffC��C�
C(�Cp�C��C�HC33Cz�C�RC�HC33C�C�RC��C	G�C	�C	�RC
  C
\)C
�\C
C
=C\)C��C��C
=CQ�C��C�
C
=CQ�C��C��C  CQ�C��C�HC
=CG�C��C�
C  CG�C��C�
C{CG�C��C�C{CQ�C��C�C33CQ�C��C�C(�C\)C��C�HC33Cz�C��C�C=qC�C�RC��C=qC�\C�
C
=CG�C�C�
C{CG�C�C��C�CffC�C�C�C\)C��C��C(�C\)C�C��C33C\)C��C�HC(�Cz�C�C�HC�Cp�C�RC   C =qC p�C �C!  C!G�C!z�C!�RC!�C"=qC"�\C"C"�C#=qC#�\C#��C$  C$=qC$z�C$��C%�C%\)C%��C%��C&{C&p�C&�C&�C'�C'ffC'�RC(  C(G�C(�\C(C)  C)=qC)�\C)�
C*(�C*ffC*��C*��C+G�C+�C+�RC,  C,=qC,�C,�HC-�C-\)C-��C-�
C.(�C.z�C.C.��C/=qC/�C/�
C0(�C0ffC0��C0�HC1(�C1�C1�
C2{C2Q�C2��C2��C3G�C3�C3�RC4  C4\)C4�C4�C5(�C5p�C5��C6{C6Q�C6�\C6�HC7=qC7�\C7��C8
=C8\)C8�C9  C9G�C9�C9C:
=C:ffC:�RC;  C;=qC;z�C;��C<�C<p�C<�RC<��C=33C=z�C=�
C>(�C>z�C>�RC?  C?Q�C?�RC@  C@G�C@�C@�
CA33CA�\CA�
CB(�CBp�CB�RCC  CCQ�CC�CC��CDG�CD�\CD��CE{CEp�CECF{CFffCF�CF��CG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                           @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A� �A� �A��A� �A� �A��A� �A� �A�"�A�"�A��A�"�A�"�A�$�A�$�A�$�A�(�A�(�A�(�A�(�A�-A�/A�1'A�1'A�33A�33A�33A�33A�33A�7LA�7LA�7LA�7LA�1'A�(�A���A���A�r�A�ZA�I�Aմ9A��AҺ^A��A�&�A�"�AΉ7A͑hA�AȍPA�1A�9XA�bAú^A�Q�A�?}A�1'A�v�A��A�x�A�oA��A�&�A���A�n�A���A�z�A�"�A�bNA��hA���A�p�A���A�{A��mA��A��#A��HA�^5A�%A���A���A�v�A�~�A�hsA�A��A��A�$�A���A��!A�O�A��`A�M�A�C�A��
A���A��A�%A�33A��A�&�A��AhsA~E�A{�TAzJAyC�Ax��AuAr�An��Ak�Ai�mAhĜAg��AgVAcK�A^��A\�9AY��AV�/ATbNAS�AQ�
AP�jAO��AN~�AMS�AK�#AI��AG/AD  AB��AA��A@z�A?�A=;dA9��A89XA7��A7VA5��A5p�A5?}A5%A4{A3�A25?A0��A/ƨA.��A-�7A,��A+�mA*�DA*A�A*bA)��A)S�A'��A&�A&M�A%�;A$��A"��A!��A!hsA �yA M�A&�A�uA�A�TAdZA�+A�-A"�A$�A��A9XAO�A1A��A�AE�A33A��Ap�A�A�A��A1AA	��A	33A��A�;A`BAffA-Av�A��A@���@���@�5?@��@���@� �@�t�@�J@�n�@���@��#@�+@�@�?}@�O�@�^5@�w@�@�A�@@�"�@@@��m@��@��`@�j@���@@�R@��^@��@�@��@�I�@�33@�o@��@��y@�V@��@�A�@�hs@��@�Q�@�X@�X@�r�@�  @ߝ�@�V@�J@�J@�
=@Դ9@��@��/@�-@͡�@�dZ@�$�@ɑh@�7L@��`@Ȭ@�b@ǝ�@�K�@�
=@�v�@�@�O�@���@ě�@�j@�bN@�Z@�bN@�x�@�G�@��/@�9X@���@�C�@�5?@�@��-@��7@�O�@�/@���@��j@�Z@���@�dZ@���@�^5@�E�@�5?@�$�@�$�@�-@�@��^@�/@��9@��u@��D@�j@�bN@�bN@�j@��@��`@�X@�@���@��\@��#@��m@���@��;@���@�;d@���@��+@�{@��7@�X@�O�@��@��u@�A�@�1@�\)@�~�@��@��u@�ƨ@��F@��D@�z�@�bN@�1@�ƨ@��
@�  @� �@�A�@�Q�@�A�@�  @��F@�l�@��@�E�@���@���@��T@��@�x�@��`@��@��@���@��P@�\)@�t�@��P@��P@�t�@�C�@���@���@�`B@��@�Ĝ@�9X@��;@�dZ@�@��@�$�@���@��h@�`B@�7L@��@���@���@���@�A�@�  @��@���@��P@�o@�^5@���@�O�@�7L@��@�z�@���@�+@��@��@��R@���@�ff@�$�@�J@���@��@��@��@��T@�x�@�X@��@�b@��P@���@��+@�$�@��7@���@��j@�j@�A�@�(�@��@��@��@�|�@�;d@�+@�+@�o@��y@�ff@��@��@�%@���@�Ĝ@���@��u@��@�r�@�j@�1@���@�l�@���@�^5@�=q@�$�@�J@���@��@�@���@�x�@�hs@�G�@�V@���@�j@���@���@�t�@�dZ@�;d@�o@��@���@�=q@��@���@��7@�`B@�%@��j@���@�Z@�A�@�A�@�9X@�1'@�b@��;@���@��@�dZ@�C�@��@��!@�n�@�{@��#@���@���@���@���@�7L@��@���@��@�Q�@�(�@��;@��@�t�@�S�@�33@��@���@��\@�ff@�=q@��@��@���@��T@��-@��7@�`B@�/@�V@��/@���@�Z@�b@��@��
@���@��F@���@���@�l�@�K�@�
=@���@�5?@���@���@�@�@��-@���@�x�@�`B@��@��@�Ĝ@��@���@�r�@�I�@�9X@�  @��
@�ƨ@��F@���@��@�l�@�K�@��@��@�ȴ@���@�$�@���@��#@���@�hs@��@��9@�bN@�(�@�  @|�@~�@~��@~{@}�-@}V@|�@|��@|I�@{�m@{t�@z�\@y�^@yX@x�`@x��@x�u@xr�@w�;@w\)@w�@v��@vE�@u@u`B@t�D@t1@s��@sS�@r�@rn�@q�^@qx�@qX@qG�@q7L@q&�@pb@o��@o�P@oK�@o
=@o�@n�+@nE�@n@m�@m@m�h@m�@l�@l(�@k��@kS�@kC�@j�@j��@j�\@jM�@i��@i�@hb@gK�@g
=@f��@fV@f@e��@e/@dj@d(�@c�
@b��@b=q@a�^@`��@`Q�@_|�@_l�@_\)@^�y@^$�@]V@\�@[S�@Z^5@Z�@Y��@Y�@Y��@YG�@X��@X�9@X�@Xb@W|�@Vȴ@V5?@V@U�@U��@U��@U��@U?}@Tz�@T9X@S�m@S�
@Sƨ@SdZ@R^5@RJ@QX@P��@P��@P�@PbN@Pb@O�;@OK�@O
=@Nȴ@N��@N�+@M@M`B@M�@M�@L��@L�@L�/@L��@Lz�@K�
@K33@J�@J�!@J�!@J�\@JM�@I��@IX@H�9@G�@GK�@G
=@F�@F��@FE�@F{@E��@E�h@E/@D��@D��@DZ@C�F@C�@C33@C@BM�@A��@AG�@@Ĝ@@ �@?�P@>�@>5?@>{@=�@=�T@=�-@=?}@<�@<Z@<1@;�m@;ƨ@;��@;dZ@;S�@;33@;@:��@:��@:�!@:�!@:�\@9��@9��@9��@9hs@97L@8��@8  @7�@7K�@7
=@6�+@6E�@65?@5�@5p�@4�@4j@49X@3��@3�F@3dZ@3"�@2~�@2^5@2=q@1��@1G�@1�@0�`@0�9@01'@/�@/�@/\)@/K�@/+@/
=@.�@.�+@.v�@.ff@.5?@-��@-�h@-`B@-`B@-V@,�@,�j@,�D@,9X@+��@+dZ@+33@*�H@*�\@*^5@*-@*J@)�@)��@)hs@(��@(bN@(1'@'�@'�w@'��@'�P@'|�@'\)@';d@'�@'
=@&ȴ@&ȴ@&�@&�@&ȴ@&ȴ@&ȴ@&��@%��@%��@%�@%�@%�@%p�@%/@$��@$�@$z�@$Z@$9X@$9X@$�@#�
@#��@#�@#33@"��@"^5@!�@!��@!�^@!G�@!7L@!�@ ��@ ��@ ��@ ��@ �@�@�@��@��@�P@l�@+@
=@�@�@��@��@��@��@�+@��@E�@�T@��@@��@�-@�-@�-@�-@�-@`B@/@V@�/@z�@�@��@�m@�m@�
@�
@�
@�F@��@��@�@dZ@33@o@�@��@~�@�@��@�@�@�#@��@��@��@��@�^@�^@x�@X@�@�@��@��@Ĝ@�9@��@�u@ �@�A�bA�bA��A��A��A��A� �A��A��A�"�A��A�"�A��A��A�"�A�"�A��A� �A�"�A��A�"�A�"�A��A�"�A��A��A��A�"�A��A� �A�$�A�"�A��A�$�A�"�A� �A�"�A�$�A��A�"�A�$�A�"�A��A��A��A��A��A� �A�"�A� �A�"�A�$�A�"�A� �A�"�A�&�A�"�A�"�A�&�A�&�A� �A�"�A�$�A�&�A�"�A�"�A�&�A�"�A� �A�$�A�$�A� �A�$�A�(�A�+A�&�A�&�A�+A�(�A�&�A�+A�-A�&�A�(�A�-A�&�A�&�A�+A�+A�$�A�+A�+A�&�A�(�A�-A�(�A�&�A�+A�1'A�+A�/A�1'A�-A�/A�1'A�-A�/A�1'A�1'A�-A�1'A�1'A�-A�33A�33A�-A�33A�1'A�/A�33A�33A�/A�/A�33A�1'A�/A�1'A�5?A�1'A�/A�5?A�33A�1'A�33A�5?A�1'A�/A�5?A�33A�/A�33A�7LA�33A�1'A�33A�7LA�33A�1'A�7LA�33A�/A�33A�5?A�1'A�1'A�5?A�33A�1'A�5?A�33A�1'A�7LA�33A�1'A�5?A�7LA�1'A�33A�7LA�33A�1'A�7LA�5?A�1'A�7LA�7LA�33A�7LA�9XA�5?A�33A�9XA�5?A�33A�7LA�9XA�5?A�7LA�9XA�5?A�33A�9XA�9XA�33A�7LA�9XA�7LA�33A�9XA�7LA�33A�7LA�9XA�9XA�33A�5?A�7LA�9XA�33A�7LA�9XA�5?A�33A�9XA�9XA�1'A�33A�5?A�33A�/A�1'A�33A�-A�/A�33A�/A�-A�-A�/A�/A�+A�(�A�(�A�+A�&�A�"�A�$�A�(�A�"�A��A��A��A�{A�{A��A�VA���A��A��Aݲ-A�x�A�5?A�$�A��A��A��A�{A�bA�bA�%A���A�dZA�`BA�%A���Aڣ�A�jA�7LAى7A�"�A���Aؕ�A؋DA�/A�&�A�{A��#Aײ-A׃A�bNA�E�A�33A�"�A�VA���A־wA֛�A�p�A�XA�I�A�A�A�5?A�+A��A��A��A�A��A��A��TA���AնFA՟�A�x�A�?}A��AԼjAԙ�A�l�A�1'A��A��HAӋDA�XA�C�A�+A�
=A���A��HAҺ^Aҡ�Aң�Aң�Aң�Aҟ�Aқ�Aҝ�Aҝ�AҍPA�p�A�33A��A�ĜAѮA�jA�(�A���Aв-AУ�AЍPA�bNA�+A���A��/AϺ^Aϧ�A�~�A�l�A�S�A�I�A�=qA�5?A�(�A�1A��A��yA��
Aκ^Aΰ!AήAΛ�AΉ7A�~�A�x�A�l�A�XA�7LA�"�A�%A�  A��A�ƨA�XA��mA̟�ȂhÃA�r�A�VA�JA�|�Aʴ9A��Aɡ�A�
=A���A���A���AȮAȗ�AȋDA�x�A�r�A�jA�S�A�-A�ĜAǉ7A�;dA���AƾwAơ�AƍPA�v�A�t�A�p�A�ffA�^5A�K�A�7LA�-A�&�A��A��Aź^Ař�AŁA�hsA�$�A���A���A���Aĺ^Aě�A�^5A�&�A�
=A��A���Aã�AÛ�AÅA�t�A�p�A�K�A�/A��A���A�A�l�A�5?A��DA��A���A��mA��^A���A�E�A��A���A��RA���A�z�A�ffA�XA�G�A�33A��A���A��A�ƨA��+A��A��A�t�A�^5A�VA�hsA��hA���A��hA��uA���A���A��+A�jA�E�A�-A�  A���A��FA���A��A�K�A���A�VA���A�+A�1A�A�A���A��`A��/A�A���A�~�A�O�A��A��yA��A��A��TA��FA�z�A�dZA�7LA���A���A��!A���A���A��DA�=qA��9A�~�A�ffA�E�A�-A�oA���A��PA�bNA�VA�A�A�(�A��A�bA���A���A��9A���A��A�\)A�%A���A��wA���A�|�A�dZA�XA�Q�A�G�A�?}A�?}A�5?A��A��A��A�oA�1A��;A��FA���A��A�dZA�?}A��A��A�ffA�oA��A���A��DA�M�A���A���A��
A��
A���A���A���A���A���A���A���A���A�A���A�ZA�%A�ȴA��A�?}A��A��A��-A��7A�p�A�XA�I�A�1'A���A���A�ƨA��+A�=qA��wA��A�"�A�A��A��yA��;A��^A�XA��A��jA�jA�  A�{A��7A�-A�VA�ƨA�|�A��A���A��7A�|�A�\)A�1'A���A��A��TA�~�A�C�A��A���A��A��`A���A�|�A�K�A�&�A�A��mA���A��-A��PA�VA��mA��uA�n�A�Q�A�/A�bA���A��
A���A�~�A�l�A�\)A�E�A��A���A��jA���A�z�A�C�A��mA��^A���A��PA��A�t�A�^5A�K�A�/A��A��A��A��+A�S�A�33A�A��/A��
A��jA���A�v�A�I�A��A��`A���A�9XA�oA���A��mA��HA��#A���A��FA���A�x�A�1'A���A�A�hsA�G�A�I�A�I�A�G�A�;dA���A�ȴA���A�S�A�?}A�33A�&�A�oA���A��A��TA��A���A��^A���A��DA�9XA��A��#A�ȴA��A���A���A���A��A�t�A�l�A�bNA�VA�9XA�$�A��A�bA���A��A��HA���A�ȴA��-A���A��A�ZA�;dA�-A�{A��A��DA��A�-A���A���A��+A�t�A�Q�A�A��#A��jA���A���A��A�E�A��A��A��
A��jA�S�A�-A���A��FA�C�A��mA�z�A�JA���A�dZA�A�bNA�I�A���A�\)A��RA���A�XA�+A��A���A��PA�|�A�E�A��A��/A���A�hsA�7LA�JA���A��A��A�9XA��A�ȴA���A��uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                           A��A��A��A� �A� �A��A� �A� �A��A� �A� �A�"�A�"�A��A�"�A�"�A�$�A�$�A�$�A�(�A�(�A�(�A�(�A�-A�/A�1'A�1'A�33A�33A�33A�33A�33A�7LA�7LA�7LA�7LA�1'A�(�A���A���A�r�A�ZA�I�Aմ9A��AҺ^A��A�&�A�"�AΉ7A͑hA�AȍPA�1A�9XA�bAú^A�Q�A�?}A�1'A�v�A��A�x�A�oA��A�&�A���A�n�A���A�z�A�"�A�bNA��hA���A�p�A���A�{A��mA��A��#A��HA�^5A�%A���A���A�v�A�~�A�hsA�A��A��A�$�A���A��!A�O�A��`A�M�A�C�A��
A���A��A�%A�33A��A�&�A��AhsA~E�A{�TAzJAyC�Ax��AuAr�An��Ak�Ai�mAhĜAg��AgVAcK�A^��A\�9AY��AV�/ATbNAS�AQ�
AP�jAO��AN~�AMS�AK�#AI��AG/AD  AB��AA��A@z�A?�A=;dA9��A89XA7��A7VA5��A5p�A5?}A5%A4{A3�A25?A0��A/ƨA.��A-�7A,��A+�mA*�DA*A�A*bA)��A)S�A'��A&�A&M�A%�;A$��A"��A!��A!hsA �yA M�A&�A�uA�A�TAdZA�+A�-A"�A$�A��A9XAO�A1A��A�AE�A33A��Ap�A�A�A��A1AA	��A	33A��A�;A`BAffA-Av�A��A@���@���@�5?@��@���@� �@�t�@�J@�n�@���@��#@�+@�@�?}@�O�@�^5@�w@�@�A�@@�"�@@@��m@��@��`@�j@���@@�R@��^@��@�@��@�I�@�33@�o@��@��y@�V@��@�A�@�hs@��@�Q�@�X@�X@�r�@�  @ߝ�@�V@�J@�J@�
=@Դ9@��@��/@�-@͡�@�dZ@�$�@ɑh@�7L@��`@Ȭ@�b@ǝ�@�K�@�
=@�v�@�@�O�@���@ě�@�j@�bN@�Z@�bN@�x�@�G�@��/@�9X@���@�C�@�5?@�@��-@��7@�O�@�/@���@��j@�Z@���@�dZ@���@�^5@�E�@�5?@�$�@�$�@�-@�@��^@�/@��9@��u@��D@�j@�bN@�bN@�j@��@��`@�X@�@���@��\@��#@��m@���@��;@���@�;d@���@��+@�{@��7@�X@�O�@��@��u@�A�@�1@�\)@�~�@��@��u@�ƨ@��F@��D@�z�@�bN@�1@�ƨ@��
@�  @� �@�A�@�Q�@�A�@�  @��F@�l�@��@�E�@���@���@��T@��@�x�@��`@��@��@���@��P@�\)@�t�@��P@��P@�t�@�C�@���@���@�`B@��@�Ĝ@�9X@��;@�dZ@�@��@�$�@���@��h@�`B@�7L@��@���@���@���@�A�@�  @��@���@��P@�o@�^5@���@�O�@�7L@��@�z�@���@�+@��@��@��R@���@�ff@�$�@�J@���@��@��@��@��T@�x�@�X@��@�b@��P@���@��+@�$�@��7@���@��j@�j@�A�@�(�@��@��@��@�|�@�;d@�+@�+@�o@��y@�ff@��@��@�%@���@�Ĝ@���@��u@��@�r�@�j@�1@���@�l�@���@�^5@�=q@�$�@�J@���@��@�@���@�x�@�hs@�G�@�V@���@�j@���@���@�t�@�dZ@�;d@�o@��@���@�=q@��@���@��7@�`B@�%@��j@���@�Z@�A�@�A�@�9X@�1'@�b@��;@���@��@�dZ@�C�@��@��!@�n�@�{@��#@���@���@���@���@�7L@��@���@��@�Q�@�(�@��;@��@�t�@�S�@�33@��@���@��\@�ff@�=q@��@��@���@��T@��-@��7@�`B@�/@�V@��/@���@�Z@�b@��@��
@���@��F@���@���@�l�@�K�@�
=@���@�5?@���@���@�@�@��-@���@�x�@�`B@��@��@�Ĝ@��@���@�r�@�I�@�9X@�  @��
@�ƨ@��F@���@��@�l�@�K�@��@��@�ȴ@���@�$�@���@��#@���@�hs@��@��9@�bN@�(�@�  @|�@~�@~��@~{@}�-@}V@|�@|��@|I�@{�m@{t�@z�\@y�^@yX@x�`@x��@x�u@xr�@w�;@w\)@w�@v��@vE�@u@u`B@t�D@t1@s��@sS�@r�@rn�@q�^@qx�@qX@qG�@q7L@q&�@pb@o��@o�P@oK�@o
=@o�@n�+@nE�@n@m�@m@m�h@m�@l�@l(�@k��@kS�@kC�@j�@j��@j�\@jM�@i��@i�@hb@gK�@g
=@f��@fV@f@e��@e/@dj@d(�@c�
@b��@b=q@a�^@`��@`Q�@_|�@_l�@_\)@^�y@^$�@]V@\�@[S�@Z^5@Z�@Y��@Y�@Y��@YG�@X��@X�9@X�@Xb@W|�@Vȴ@V5?@V@U�@U��@U��@U��@U?}@Tz�@T9X@S�m@S�
@Sƨ@SdZ@R^5@RJ@QX@P��@P��@P�@PbN@Pb@O�;@OK�@O
=@Nȴ@N��@N�+@M@M`B@M�@M�@L��@L�@L�/@L��@Lz�@K�
@K33@J�@J�!@J�!@J�\@JM�@I��@IX@H�9@G�@GK�@G
=@F�@F��@FE�@F{@E��@E�h@E/@D��@D��@DZ@C�F@C�@C33@C@BM�@A��@AG�@@Ĝ@@ �@?�P@>�@>5?@>{@=�@=�T@=�-@=?}@<�@<Z@<1@;�m@;ƨ@;��@;dZ@;S�@;33@;@:��@:��@:�!@:�!@:�\@9��@9��@9��@9hs@97L@8��@8  @7�@7K�@7
=@6�+@6E�@65?@5�@5p�@4�@4j@49X@3��@3�F@3dZ@3"�@2~�@2^5@2=q@1��@1G�@1�@0�`@0�9@01'@/�@/�@/\)@/K�@/+@/
=@.�@.�+@.v�@.ff@.5?@-��@-�h@-`B@-`B@-V@,�@,�j@,�D@,9X@+��@+dZ@+33@*�H@*�\@*^5@*-@*J@)�@)��@)hs@(��@(bN@(1'@'�@'�w@'��@'�P@'|�@'\)@';d@'�@'
=@&ȴ@&ȴ@&�@&�@&ȴ@&ȴ@&ȴ@&��@%��@%��@%�@%�@%�@%p�@%/@$��@$�@$z�@$Z@$9X@$9X@$�@#�
@#��@#�@#33@"��@"^5@!�@!��@!�^@!G�@!7L@!�@ ��@ ��@ ��@ ��@ �@�@�@��@��@�P@l�@+@
=@�@�@��@��@��@��@�+@��@E�@�T@��@@��@�-@�-@�-@�-@�-@`B@/@V@�/@z�@�@��@�m@�m@�
@�
@�
@�F@��@��@�@dZ@33@o@�@��@~�@�@��@�@�@�#@��@��@��@��@�^@�^@x�@X@�@�@��@��@Ĝ@�9@��@�u@ �G�O�A�bA�bA��A��A��A��A� �A��A��A�"�A��A�"�A��A��A�"�A�"�A��A� �A�"�A��A�"�A�"�A��A�"�A��A��A��A�"�A��A� �A�$�A�"�A��A�$�A�"�A� �A�"�A�$�A��A�"�A�$�A�"�A��A��A��A��A��A� �A�"�A� �A�"�A�$�A�"�A� �A�"�A�&�A�"�A�"�A�&�A�&�A� �A�"�A�$�A�&�A�"�A�"�A�&�A�"�A� �A�$�A�$�A� �A�$�A�(�A�+A�&�A�&�A�+A�(�A�&�A�+A�-A�&�A�(�A�-A�&�A�&�A�+A�+A�$�A�+A�+A�&�A�(�A�-A�(�A�&�A�+A�1'A�+A�/A�1'A�-A�/A�1'A�-A�/A�1'A�1'A�-A�1'A�1'A�-A�33A�33A�-A�33A�1'A�/A�33A�33A�/A�/A�33A�1'A�/A�1'A�5?A�1'A�/A�5?A�33A�1'A�33A�5?A�1'A�/A�5?A�33A�/A�33A�7LA�33A�1'A�33A�7LA�33A�1'A�7LA�33A�/A�33A�5?A�1'A�1'A�5?A�33A�1'A�5?A�33A�1'A�7LA�33A�1'A�5?A�7LA�1'A�33A�7LA�33A�1'A�7LA�5?A�1'A�7LA�7LA�33A�7LA�9XA�5?A�33A�9XA�5?A�33A�7LA�9XA�5?A�7LA�9XA�5?A�33A�9XA�9XA�33A�7LA�9XA�7LA�33A�9XA�7LA�33A�7LA�9XA�9XA�33A�5?A�7LA�9XA�33A�7LA�9XA�5?A�33A�9XA�9XA�1'A�33A�5?A�33A�/A�1'A�33A�-A�/A�33A�/A�-A�-A�/A�/A�+A�(�A�(�A�+A�&�A�"�A�$�A�(�A�"�A��A��A��A�{A�{A��A�VA���A��A��Aݲ-A�x�A�5?A�$�A��A��A��A�{A�bA�bA�%A���A�dZA�`BA�%A���Aڣ�A�jA�7LAى7A�"�A���Aؕ�A؋DA�/A�&�A�{A��#Aײ-A׃A�bNA�E�A�33A�"�A�VA���A־wA֛�A�p�A�XA�I�A�A�A�5?A�+A��A��A��A�A��A��A��TA���AնFA՟�A�x�A�?}A��AԼjAԙ�A�l�A�1'A��A��HAӋDA�XA�C�A�+A�
=A���A��HAҺ^Aҡ�Aң�Aң�Aң�Aҟ�Aқ�Aҝ�Aҝ�AҍPA�p�A�33A��A�ĜAѮA�jA�(�A���Aв-AУ�AЍPA�bNA�+A���A��/AϺ^Aϧ�A�~�A�l�A�S�A�I�A�=qA�5?A�(�A�1A��A��yA��
Aκ^Aΰ!AήAΛ�AΉ7A�~�A�x�A�l�A�XA�7LA�"�A�%A�  A��A�ƨA�XA��mA̟�ȂhÃA�r�A�VA�JA�|�Aʴ9A��Aɡ�A�
=A���A���A���AȮAȗ�AȋDA�x�A�r�A�jA�S�A�-A�ĜAǉ7A�;dA���AƾwAơ�AƍPA�v�A�t�A�p�A�ffA�^5A�K�A�7LA�-A�&�A��A��Aź^Ař�AŁA�hsA�$�A���A���A���Aĺ^Aě�A�^5A�&�A�
=A��A���Aã�AÛ�AÅA�t�A�p�A�K�A�/A��A���A�A�l�A�5?A��DA��A���A��mA��^A���A�E�A��A���A��RA���A�z�A�ffA�XA�G�A�33A��A���A��A�ƨA��+A��A��A�t�A�^5A�VA�hsA��hA���A��hA��uA���A���A��+A�jA�E�A�-A�  A���A��FA���A��A�K�A���A�VA���A�+A�1A�A�A���A��`A��/A�A���A�~�A�O�A��A��yA��A��A��TA��FA�z�A�dZA�7LA���A���A��!A���A���A��DA�=qA��9A�~�A�ffA�E�A�-A�oA���A��PA�bNA�VA�A�A�(�A��A�bA���A���A��9A���A��A�\)A�%A���A��wA���A�|�A�dZA�XA�Q�A�G�A�?}A�?}A�5?A��A��A��A�oA�1A��;A��FA���A��A�dZA�?}A��A��A�ffA�oA��A���A��DA�M�A���A���A��
A��
A���A���A���A���A���A���A���A���A�A���A�ZA�%A�ȴA��A�?}A��A��A��-A��7A�p�A�XA�I�A�1'A���A���A�ƨA��+A�=qA��wA��A�"�A�A��A��yA��;A��^A�XA��A��jA�jA�  A�{A��7A�-A�VA�ƨA�|�A��A���A��7A�|�A�\)A�1'A���A��A��TA�~�A�C�A��A���A��A��`A���A�|�A�K�A�&�A�A��mA���A��-A��PA�VA��mA��uA�n�A�Q�A�/A�bA���A��
A���A�~�A�l�A�\)A�E�A��A���A��jA���A�z�A�C�A��mA��^A���A��PA��A�t�A�^5A�K�A�/A��A��A��A��+A�S�A�33A�A��/A��
A��jA���A�v�A�I�A��A��`A���A�9XA�oA���A��mA��HA��#A���A��FA���A�x�A�1'A���A�A�hsA�G�A�I�A�I�A�G�A�;dA���A�ȴA���A�S�A�?}A�33A�&�A�oA���A��A��TA��A���A��^A���A��DA�9XA��A��#A�ȴA��A���A���A���A��A�t�A�l�A�bNA�VA�9XA�$�A��A�bA���A��A��HA���A�ȴA��-A���A��A�ZA�;dA�-A�{A��A��DA��A�-A���A���A��+A�t�A�Q�A�A��#A��jA���A���A��A�E�A��A��A��
A��jA�S�A�-A���A��FA�C�A��mA�z�A�JA���A�dZA�A�bNA�I�A���A�\)A��RA���A�XA�+A��A���A��PA�|�A�E�A��A��/A���A�hsA�7LA�JA���A��A��A�9XA��A�ȴA���A��uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                           ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�BVB�BVB�B�B"B�B"BVB�B�B�BVBVBVB"BVBVBVB�B.B.B�B.B�BB�B�BSB�B�B�B�B �B)�BI�B��B��B��B��B��B�B� B"B�B,=BK^B��B��B��B��B�B��B�#B��B�XB�BÖB��B�JB~]BzB�rB��B��B�7B��B��B~(Br�Bv+BoiBjBJ�BA�B6B(�B�B�B�B�KB�yB��B��B��B�IB�'B��B��B�By�BrBh�B[#BC-B7�B%zB�B
�B
�jB
�B
��B
�YB
�"B
��B
t�B
k�B
e`B
\�B
C�B
2�B
!�B
{B
DB
B	�PB	�B	��B	��B	�0B	��B	��B	��B	��B	�~B	�B	�B	�B	x�B	n�B	ncB	XyB	Q�B	M6B	E�B	@B	;�B	1'B	#�B	!�B	"�B	�B	xB	�B	�B	�B	:B	:B	DB		B	B	GB��B��B�B��B�2B��B�`B�TB��B�B��B�iB�B�/B�"B�B�DB��B��B�/B�vB��B�8B�B�B�B�B��B�DB�8B�sB�KB��B�;B�vB�B�B��B	�B	(B	�B	YB	B	~B	&B	(�B	#:B	($B	�B	#nB	3�B	3hB	1[B	B	�B	�B	�B	)�B	F?B	T�B	R�B	^�B	X�B	XEB	_pB	d&B	u�B	��B	�AB	y�B	x�B	}"B	�uB	�"B	��B	��B	��B	��B	��B	�B	��B	��B	�zB	��B	�xB	�=B	�kB	�xB	��B	��B	�LB	�B	��B	��B	��B	�B	�XB	�qB	��B	�6B	��B	�XB	�$B	��B	��B	��B	�IB	��B	�(B	��B	�:B	� B	�B	��B	��B	��B	�~B	��B	�'B	��B	��B	�B	��B	�=B	�B	��B	��B	�'B	�aB	��B	��B	��B	�qB	��B	��B	��B	�0B	�qB	�B	��B	��B	�UB	�'B	��B	�B	��B	�<B	�}B	уB	� B	��B	��B	��B	�B	�EB	��B	��B	یB	�#B	��B	��B	�B	��B	��B	�B	�B	�)B	�B	�%B	�rB	�B	��B	��B	��B	�JB	�8B	��B	�]B	�PB	��B	�DB	�DB	��B	��B	��B
  B	��B	�"B	�xB	��B	�lB
MB
fB
	B
	lB
	7B
	�B

rB
JB
�B
�B
�B
bB
(B
�B
�B
\B
B
�B
{B
�B
�B
SB
�B
eB
	B
�B
�B
�B
�B
 �B
"hB
#:B
#�B
&�B
&�B
&�B
'�B
&�B
&�B
&�B
'RB
,�B
*0B
(XB
)*B
(�B
)�B
*0B
+kB
/B
.�B
/B
1'B
0�B
0�B
0!B
1[B
1�B
2�B
2�B
33B
3hB
49B
5�B
6�B
6zB
6�B
6�B
7LB
7�B
7�B
7�B
8RB
8B
8B
8RB
8�B
:^B
:^B
;0B
;�B
<B
;0B
:*B
;0B
=B
>B
>wB
>�B
>�B
?}B
?�B
?�B
AUB
A�B
B�B
B�B
B�B
B[B
B�B
B�B
D3B
C�B
C�B
C�B
CaB
B�B
B�B
B�B
B�B
B'B
C�B
DgB
EB
FB
D�B
D�B
D�B
D�B
EB
D�B
EB
E9B
EmB
E9B
EmB
E�B
F�B
F�B
G�B
HKB
H�B
HKB
H�B
H�B
H�B
IRB
I�B
J�B
K^B
K�B
K�B
L�B
M6B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NpB
NpB
N�B
N�B
OB
O�B
O�B
P�B
P�B
QB
P�B
P�B
P�B
P�B
Q�B
QNB
Q�B
R�B
R�B
R�B
S[B
S�B
S&B
R�B
R�B
S[B
S[B
S�B
S�B
S�B
T,B
T,B
TaB
T,B
TaB
TaB
T�B
UgB
T�B
T�B
T�B
T�B
UgB
UgB
U2B
T�B
U2B
U2B
T�B
UgB
UgB
VB
VmB
VmB
V�B
V�B
V�B
W
B
V�B
V�B
W
B
V�B
XEB
XyB
X�B
YKB
YB
YKB
YKB
ZB
ZB
ZB
ZB
ZB
ZQB
ZQB
Z�B
Z�B
[�B
\)B
\)B
\�B
]�B
]/B
]�B
]dB
^5B
^B
_B
_pB
_B
_pB
`B
`B
`BB
`�B
aB
bB
bB
bNB
b�B
b�B
b�B
c�B
dZB
d�B
e`B
e,B
e`B
e`B
f2B
e�B
ffB
gB
gB
h
B
g�B
iDB
i�B
i�B
iyB
iDB
i�B
i�B
i�B
jB
k�B
l"B
k�B
l�B
k�B
k�B
k�B
l"B
l�B
n�B
n�B
n�B
ncB
ncB
ncB
n�B
n�B
o�B
pB
o�B
o�B
poB
poB
p;B
poB
qB
rB
t�B
t�B
tTB
t�B
t�B
u%B
uZB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
w2B
v�B
w�B
x8B
xB
xlB
w�B
w2B
u�B
u�B
uZB
u�B
u�B
u�B
v`B
wfB
wfB
w2B
v�B
v�B
w�B
y	B
y>B
y	B
y	B
y�B
y�B
y�B
zxB
zB
y�B
zB
zB
zB
z�B
z�B
z�B
{�B
|B
|B
{�B
|PB
|PB
|�B
}VB
}�B
}�B
}VB
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
�4B
�iB
��B
��B
��B
��B
��B
��B
�B
�{B
��B
��B
��B
��B
�SB
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
�fB
�fB
�B
�7B
�	B
�rB
�DB
�B
��B
�xB
��B
��B
�xB
�B
�JB
�~B
�~B
�~B
��B
��B
��B
��B
��B
�B
�B
�PB
�B
�PB
��B
��B
�"B
�"B
��B
�(B
�\B
�(B
�(B
�\B
�.B
�.B
��B
�bB
� B
��B
��B
��B
��B
�B
�:B
��B
��B
�B
��B
�{B
�FB
�B
�{B
�B
�{B
��B
��B
�B
�B
�B
�MB
�MB
��B
��B
��B
��B
�B
�SB
��B
�B
��B
��B
�$B
�$B
��B
�_B
�+B
�_B
��B
��B
�1B
��B
��B
��B
��B
�7B
��B
�	B
�	B
�qB
�qB
��B
��B
�qB
��B
��B
�CB
�xB
�B
��B
�OB
�OB
��B
��B
�'B
�-B
��B
�bB
��B
��B
��B
��B
��B
��B
�hB
�4B
��B
��B
��B
��B
�:B
�nB
�:B
�B
�B
�tB
��B
�B
�tB
�tB
�FB
�B
�FB
�B
�FB
�FB
�zB
�LB
��B
��B
�zB
�FB
�FB
�FB
��B
�FB
�zB
�zB
�FB
�FB
��B
��B
�B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�XB
�XB
�XB
�XB
�$B
��B
�XB
�XB
��B
�_B
��B
��B
�eB
�0B
�0B
�B
�kB
��B
��B
�kB
�=B
�=B
�=B
�B
�B
�=B
�=B
�=B
�=B
�=B
�=B
�qB
��B
�B
�B
�CB
�CB
�wB
�wB
�CB
��B
�wB
�}B
�B�B�B\B�B.B�B�B�B�B�B�B�B�B�B�B"B�B�BB�B�B�B�B�B�B�B"B�B(B�BPBVB(B�BVB(B"B"B�B"BPBVB�B"B�B\B(BB�B�B"B�B�B\B"BPB�B�B�BPB\B(B"BB\B�B�B�B�B"B�B�B(B"B�B\B�BPBVB(B�BB�B�BB(B(BB�B�B�B�B\B�B�B�B�B�B�BhB.B\B4B�B(B�B�B\B�BhB�B�BhB\B\B�B�B�B�B\B(BbB�B�B�B BbB�B.B�B(B\B B�B�B BhB\B�B4B.B�BbB4B.B(BbB�BVB BuBoB�BuB�B�B@BuB�B�B@B�B�B�BSB�B�B�BB�B�BBFB�B�B�B+B�B�B�B�B�B�B�B�BSB�B�BB�BeBYBYB�B�B$B�B�B�B1B�BeB�B�BkB�B�B�B�BB_B�B=B�B�BxB�B=B�B!BOB�B�B�B�B�B 'B 'B�B�B �B!�B!-B�B!bB"hB!�B�B!�B#nB �B �B#:B"�B �B#nB'�B,=B5B;dBA�BH�BB�BC�BA�B@�BB'BA B?}B>BBIBX�By>Bo Bj�Bt�Bz�B�4B�4B��B��B��B�!B�-B��B��B�B��B��B��B��B�jB�B�B�gB�0B��BҽB�B֡B��B�BٴBܒB��BیB�pB�BߤB�B��B�B��B�B��B��B�]B�
B��B�]B��B�iB�vB��B�B�2B��B�ZB�B��B�;B�jB�dB�)BݘB��BݘB��B�BB�B�B��B��B��BBGB�B	BfB_BJB@B�B�B�B4B�B�B�B�B�B+B�B �B�B �B#B(�B'�B&LB+�B-B-�B,�B.IB2�B6FB9$B<jB:�B;�BF�B]/BiDBf2Ba|Ba�Bc�Bd�Bk�B��B�YB�_B�	B��B��B��B�=B�VB�VB��B��B�~B��B�:B�_B��B�CB��B�LB��B�:B��B�@B�B�:B�tB�B��B�FB�zB��B�$B�CB��B�UB��B��B��B��B�B�nB�FB�RB�-B��B�<B�<B�-B�wB��B��B�B��B�?B��B�B��BȴB�gB�jB�;B�^B��B�[BǮB��B��B�B�EB��B�HB� B��B��B��B�XB��B��B��B��B��B��B��B��B��B�}B�qB�B��B�B� B��B�B�B��B�BƨB�B�[B��B��B��B�dB�gB��B��B�qB�bB�AB��B��B�B�B�SB~]B}�B~(B{B|�B��B�GB{BxB{JBs�B|�BzB�iB{JBx�Bv�BzB��B�-B�DB�B�B�B��B�FB�B��B��B�SB��B��B��B�7B�PB�PB��B�rB��B��B�B�1B��B��B��B��B�%B�1B��B�{B��B��B��B�B��B��B�xB�{B�oB�;B�oB~]B~�B�4B��B��BcB�Bu%B}�B}�Bt�Bq�BrGBsMBsBr�Bq�BtBrGBo5Bv�BxB{�Bz�BzDBuZBy>Bq�BncBo Bu�BkBm)Bi�Bf�Bl�BgmBp;Bz�B`BBa�Bf�B{�BR�BOvBA B?�B=qBE9B6zB>�BG�BB�BD�BZ�BG�B:^B6B:^B9�BL0B2-B.IB.�B/�B2-BAUB9�BA�B%�BBSB�BoB�B�B�B�BDBfBBGB�BBB4B��B�DB�lB�rB��B�B�|B��B�B�)B�QB�B�GB��B�2B�>B�ZB�WB�>B�;B��B��B�?B��BרB��B՛B��BʌB�<B՛B�tBȀB�KB��B��B�3B��B�[B��B��B�<B��B�UB��B��B�!B��B��B��B��B�B�'B��B�6B��B��B��B�VB��B��B�VB��B�VB��B��B�FB�:B�B�@B��B��B�B��B�B�VB�xB��B��B�~B�uB�B�uB|B|�B~�By	B{�By	By�Bx�B{BxBu�Bt�Bu%Bs�BsBpBoiBm�Bl�Bo Bk�BgmBb�Be�BdZBm)BbNBq�BQ�BP�BHKBF�BI�BPBC-BC�B>�B=B9�BGB9�B5�B4nB0UB;0B,qB1�B'B0!B~B#�B \B�B�B�B�B1B
�+B�B
��B
�B
�,B
�#B
�vB
�B
� B
�}B
�WB
�B
�gB
�BB
�XB
��B
��B
� B
�B
�0B
��B
�0B
��B
��B
�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021081619160420210816191604IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021082700010420210827000104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021082700010420210827000104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365220220126093652IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                