CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-01T10:00:58Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230601100058  20230601100058  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�-��Ȃ@�-��Ȃ11  @�-UUa�@�-UUa�@,�s�Б}@,�s�Б}�c��6e��c��6e�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?�  ?��H@=p�@}p�@��R@�G�@�\A   A  A ��A+�A?\)A`  A\)A�  A��A�  A�Q�A�Q�A�  A�B   B�
B�B�
B�B'�B/�
B8  B@Q�BHQ�BP(�BX  B`(�Bh(�Bp  Bx  B�  B�  B�{B�{B�{B�{B�  B��B�  B�  B�  B�{B�  B��B�{B�  B�{B�p�BǮB�  B�  B��B�{B�{B�  B��B��B�  B��B��B��B�  B��C  C
=C��C  C
  C  C  C��C��C  C
=C  C��C��C  C 
=C!��C#��C&  C({C*{C+��C-��C0  C2  C4
=C6  C8  C:
=C<
=C>
=C@
=CB  CC��CF
=CH{CJ  CK��CN{CP
=CQ��CT  CV  CX
=CZ  C\  C^  C`  Ca��Cc��Ce��Ch
=Cj  Ck��Cn  Co�Cq��Ct  Cv  Cx  Cz  C|  C~
=C�C�C�C�C�C�C�C�
=C�
=C�C�  C���C�C�
=C�C�C�C�C���C���C���C���C�C�C���C�  C���C���C�  C�  C�  C�
=C�C�C�C�C�C�  C���C�  C�C�  C�  C�
=C�C�  C�  C�  C���C���C�
=C�
=C���C���C�C�  C�  C�  C�  C���C���C�  C���C���C�  C�  C�  C�C�  C���C�  C�C�  C�  C�
=C�C�C�  C�  C�  C�  C�  C�  C�
=C�C���C�  C�C�C�
=C�  C���C���C�C�  C���C�C�  C���C���C�C�\C�
=C�  C���C�  C�C�  C�  C�  C�  C�C�C���C���C���C�  C�C�C�  C���C���C���C�  C�C�C�  C�D �D �D  D}qD  D�D  Dz�D��D� D  Dz�D�qD}qD�qD}qD��D}qD	  D	��D
  D
}qD  D�DD� D  D� D�D�D  D}qD�RDz�D�qD� D�D}qD  D�D�D}qD  D��D  D}qD�D��DD��D  Dz�D��DxRD  D��D  D� D��D� D�D� D�qDz�D�qD ��D!  D!� D!�qD"}qD#�D#��D$D$��D%  D%}qD&�D&�D'�D'��D(D(� D(��D)}qD*  D*��D+D+}qD+�qD,�D-D-}qD-�qD.}qD/  D/� D0  D0� D1�D1��D2  D2� D2�qD3��D4  D4}qD5  D5}qD5�qD6��D7  D7}qD8�D8� D9  D9��D:  D:� D:�qD;z�D;�qD<� D=�D=� D>D>��D?�D?� D@  D@��D@�qDA}qDB  DB� DC  DC� DC�qDD� DE�DE� DE�qDF��DG�DG}qDG��DH� DI  DI� DJ  DJ� DK�DK��DLDL�DM  DM� DN�DN� DN�qDO}qDP  DP� DQ  DQ� DQ�qDR� DR�qDSz�DS��DTz�DT�qDU� DV  DV}qDW�DW��DX  DX}qDY  DY� DZ�DZ��D[  D[� D\�D\}qD\�qD]� D^�D^��D_�D_��D_�qD`}qDa  Da}qDa��Dbz�Db��Dc� Dd�Dd��De�De��Df�Df��Dg  Dg}qDh  Dh� Di�Di� Di�qDj� Dk  Dk� Dl�Dl� Dl�qDmz�Dn  Dn��Do�Do��Dp  Dp}qDp�qDq}qDq�qDr}qDs  Ds��Dt�Dt� Dt�qDu� Dv  Dv� Dw�Dw}qDw��Dx� Dy  Dy}qDz  Dz� D{  D{}qD|  D|� D}�D}��D~  D~}qD~�qD� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�}qD�� D���D�@ D��HD��HD��D�AHD�� D���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�D�HD�@ D��HD�� D���D�AHD�� D���D���D�@ D��HD���D���D�@ D��HD��HD�HD�@ D��HD�D�HD�>�D�~�D���D���D�@ D�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D���D���D�@ D�� D��HD�HD�AHD�� D���D�HD�AHD��HD�� D���D�AHD��HD��HD�  D�@ D�� D��HD��D�@ D��HD��HD�  D�@ D��HD�� D�HD�B�D��HD�� D���D�@ D���D�D�  D�>�D�}qD�� D�HD�AHD��HD�� D���D�=qD�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD���D�� D��qD�>�D�� D��HD��D�@ D�~�D��HD�HD�@ D�~�D�� D�  D�=qD�� D��HD��D�AHD�� D��HD��D�AHD�� D��HD�  D�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�>�D�|)D��qD���D�>�D�� D��HD�  D�>�D�� D�� D�HD�@ D�}qD�� D�HD�>�D�� D��HD��D�AHD�~�D�� D�  D�@ D��HD��HD��D�B�D�~�D��qD���D�>�D�� D��HD���D�>�D���D�D��D�@ D�}qD��qD�  D�@ D�}qD���D���D�>�D�~�D���D���D�>�D�~�D�� D�  D�AHD��HD��HD�HD�>�D�~�D��HD���D�@ D��HD�� D���D�>�D�� D���D��qD�>�D�~�D�� D���D�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD�� D��HD�  D�>�D�� D�� D���D�@ DHD��HD�  D�>�D�~�Dþ�D�  D�@ D�~�D�� D�HD�@ Dŀ D�� D�HD�B�DƁHD��HD�HD�AHDǁHD��HD�  D�>�DȀ D�� D�  D�@ D�~�D�� D�  D�>�Dʀ D�� D�  D�@ Dˀ D˾�D���D�>�D̀ D�� D�  D�@ D́HD�� D�  D�AHD΁HDξ�D���D�@ Dπ D��HD�  D�>�DЁHD�� D���D�@ DсHD��HD�HD�AHDҀ DҾ�D�  D�@ DӀ D�� D�HD�@ D�}qDԾ�D�HD�AHDՁHD��HD�HD�@ Dր D��HD��D�AHD�~�D�� D���D�=qD؀ Dؾ�D��qD�>�DفHD�� D�  D�>�D�~�D�� D���D�=qD�~�D��HD�HD�AHD܂�D�� D�  D�B�D݁HD�� D�  D�@ D�~�D�� D��D�AHD߀ D߾�D���D�@ D�� D�� D�  D�@ D� D��HD���D�=qD� D�� D��qD�=qD� D��HD���D�>�D�~�D�� D�HD�B�D� D徸D���D�>�D� D�� D�HD�AHD� D羸D�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�AHD�~�D�qD�  D�AHD�HD�� D��qD�>�D� D�� D��qD�>�D�~�D�� D�HD�AHD� D��HD�  D�>�D�HD�� D���D�AHD�� D�� D�HD�B�D�HD�� D��D�B�D�HD��HD�HD�AHD�~�D�D���D�=qD� D���D���D�>�D�~�D���D���D�=qD�}qD�� D�  D�@ D�� D��HD�HD�AHD��HD���D���D�AHD�� D���D�HD�:�D��>�G�?8Q�?u?�z�?\?�(�@   @�@#�
@0��@J=q@Y��@k�@}p�@���@��@��H@��\@��@�Q�@�G�@�=q@�@�  @�ff@�33@��RA�
AQ�A�RAz�A��A�RA%�A*�HA0  A5�A;�AAG�AG
=AK�AQ�AXQ�A]p�Ab�\AhQ�An{As33AxQ�A}p�A���A�z�A�
=A���A�z�A�\)A�=qA���A�
=A�=qA��A�  A��\A��A�Q�A�33A�A�Q�A�33A�ffA�G�A��
A��RA���A�z�A�
=Aə�A���AϮAҏ\A��A׮A��HA�A�Q�A�\A�p�A�Q�A�\A���A�A�\A���A�
=A���A�z�A�
=B z�B��B
=BQ�Bp�B�\B�
B	�B
ffB33Bz�BB33B(�BG�B�RB(�Bp�B�\B  Bp�B
=BQ�Bp�B
=B ��B!B#33B$z�B%�B'�B(��B)�B+\)B,��B.{B/\)B0��B2=qB3�B5�B6=qB7�B9�B:�RB;�
B=�B>�\B@  BAG�BB�\BC�
BEG�BF�\BG�BH��BJ=qBK�BL��BM�BO\)BP��BQBS
=BTz�BU�BW\)BX��BZ{B[\)B\��B^ffB_�
BaG�Bb�\Bd  Be��Bf�HBh(�Bip�Bj�HBlQ�BmBo
=BpQ�BqBs33Btz�BuBv�HBxQ�ByB{33B|z�B}��B~�HB�=qB���B��B�{B���B��B�=qB��HB�p�B�(�B���B��B�=qB��HB���B�Q�B���B���B�=qB���B��B�Q�B��HB��B�Q�B���B���B�(�B��HB���B�=qB��HB��B�(�B��HB���B�=qB���B�p�B�(�B��HB�p�B�  B���B�\)B�{B��RB�\)B��B���B�G�B�  B���B�33B�B�z�B�33B��
B�ffB�
=B���B�ffB�
=B���B�(�B���B�p�B�(�B���B�\)B��B��\B�33B��
B�z�B�
=B���B�(�B��HB��B�(�B���B�G�B��B��\B�G�B�  B���B�33B�B�ffB��B�B�Q�B���B��B�=qB��HB��B�(�B��RB�G�B��
B�ffB�
=BÅB�  B�ffBĸRB���B�G�Bř�B�B�B��
B��B�  B�  B�  B��B��
B��
B�B��
B��
B�BŮBř�BŮB�B�B�Bř�BŮBŮB�B��
B�B�B�B�B��
B��B�  B�{B�{B�(�B�(�B�=qB�Q�B�z�BƏ\BƸRBƸRBƸRB��HB���B��B�G�B�p�B�p�BǅBǮB��
B�  B�(�B�Q�B�Q�B�z�Bȏ\BȸRB��HB��B�G�B�\)BɅBə�B�B��B�{B�Q�B�z�BʸRB���B���B�
=B�G�B�\)B˙�B�B�  B�(�B�=qB�ffB̏\B̸RB�
=B�33B�\)BͅB͙�B�B��B�{B�=qB�z�BΣ�B��HB�
=B��B�G�B�p�Bϙ�B�B�  B�(�B�Q�B�ffBУ�BиRB��HB��B�\)BхB�B��B�{B�=qB�ffBң�B��HB��B�\)Bә�B��
B�{B�=qB�z�B���B�
=B�\)BծB��B�(�B�z�BָRB���B�33BׅB�B�{B�ffBظRB�
=B�\)BٮB�  B�=qB�z�BڸRB�
=B�\)Bۙ�B��B�Q�Bܣ�B��HB��B�p�B�B�  B�Q�Bޏ\B��HB�G�Bߙ�B��B�=qB��B���B�33B�B��
B�(�B�ffB�RB�
=B�\)B�B�{B�z�B���B�
=B�p�B�B�(�B�z�B��HB�33B癚B��B�=qB�\B��HB�33B�B��B�=qB��B���B�\)B�B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�BB�  B�ffB�RB��B�p�B��
B�(�B�\B���B�G�B�B�{B�z�B��HB�G�B���B�{B�z�B��HB�G�B��B�  B�ffB��HB�G�B��B�  B�ffB���B��B��B��B�Q�B���B���B�p�B��B�{B�ffB���B��B��B��C �C Q�C �C �C �
C
=C33CffC��CC��C�CG�Cz�C��C��C  C(�CQ�Cz�C��C��C  C�CQ�Cz�C��CC�C{CG�Cp�C��C��C��C(�CQ�Cz�C�C�
C
=C33C\)C�\C�RC�C
=C=qCffC�\C�RC�HC	
=C	33C	\)C	�C	�C	�
C	��C
�C
G�C
p�C
��C
C
�C�CG�CffC��CC�C{CG�Cp�C��C��C  C(�CQ�C�\C�RC�HC{C=qCp�C��CC��C�CQ�Cz�C��C�
C
=C33CffC��C��C  C33CffC��C��C��C(�CQ�C�CC��C(�CffC��C�
C{C=qCp�C�C�HC{C=qCz�C�C�C�C\)C�\CC  C=qCp�C�C�C(�CffC�C�C(�CffC�C�HC�C\)C��C�HC�CffC��C�C33Cp�C�C�C(�CffC�C�HC33Cz�CC  CG�C�CC   C =qC �C �
C!�C!ffC!�C!�C"33C"z�C"C#  C#G�C#�\C#��C${C$\)C$�C$��C%=qC%�C%��C&{C&\)C&�C'  C'G�C'��C'�
C(�C(p�C(�RC)  C)G�C)��C)�C*G�C*�\C*�
C+�C+ffC+�C,
=C,\)C,�C-  C-G�C-�\C-�HC.33C.z�C.�
C/(�C/z�C/��C0�C0ffC0�C1  C1Q�C1�RC2
=C2\)C2�C2�C3G�C3��C3�C4G�C4��C4�C5=qC5�C5�HC633C6�\C6�C7G�C7��C7�HC833C8�\C8�C9G�C9��C:  C:Q�C:��C:��C;Q�C;��C<
=C<ffC<C={C=ffC=C>�C>�C>�
C?33C?�C?�
C@=qC@��C@��CAG�CA��CA��CB\)CB�RCC�CCp�CC��CD�CDz�CD�
CE33CE��CE��CFQ�CF��CF��CG\)CGCH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141114444441444411444141144441111414111114411111411111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                 ?�  ?��H@=p�@}p�@��R@�G�@�\A   A  A ��A+�A?\)A`  A\)A�  A��A�  A�Q�A�Q�A�  A�B   B�
B�B�
B�B'�B/�
B8  B@Q�BHQ�BP(�BX  B`(�Bh(�Bp  Bx  B�  B�  B�{B�{B�{B�{B�  B��B�  B�  B�  B�{B�  B��B�{B�  B�{B�p�BǮB�  B�  B��B�{B�{B�  B��B��B�  B��B��B��B�  B��C  C
=C��C  C
  C  C  C��C��C  C
=C  C��C��C  C 
=C!��C#��C&  C({C*{C+��C-��C0  C2  C4
=C6  C8  C:
=C<
=C>
=C@
=CB  CC��CF
=CH{CJ  CK��CN{CP
=CQ��CT  CV  CX
=CZ  C\  C^  C`  Ca��Cc��Ce��Ch
=Cj  Ck��Cn  Co�Cq��Ct  Cv  Cx  Cz  C|  C~
=C�C�C�C�C�C�C�C�
=C�
=C�C�  C���C�C�
=C�C�C�C�C���C���C���C���C�C�C���C�  C���C���C�  C�  C�  C�
=C�C�C�C�C�C�  C���C�  C�C�  C�  C�
=C�C�  C�  C�  C���C���C�
=C�
=C���C���C�C�  C�  C�  C�  C���C���C�  C���C���C�  C�  C�  C�C�  C���C�  C�C�  C�  C�
=C�C�C�  C�  C�  C�  C�  C�  C�
=C�C���C�  C�C�C�
=C�  C���C���C�C�  C���C�C�  C���C���C�C�\C�
=C�  C���C�  C�C�  C�  C�  C�  C�C�C���C���C���C�  C�C�C�  C���C���C���C�  C�C�C�  C�D �D �D  D}qD  D�D  Dz�D��D� D  Dz�D�qD}qD�qD}qD��D}qD	  D	��D
  D
}qD  D�DD� D  D� D�D�D  D}qD�RDz�D�qD� D�D}qD  D�D�D}qD  D��D  D}qD�D��DD��D  Dz�D��DxRD  D��D  D� D��D� D�D� D�qDz�D�qD ��D!  D!� D!�qD"}qD#�D#��D$D$��D%  D%}qD&�D&�D'�D'��D(D(� D(��D)}qD*  D*��D+D+}qD+�qD,�D-D-}qD-�qD.}qD/  D/� D0  D0� D1�D1��D2  D2� D2�qD3��D4  D4}qD5  D5}qD5�qD6��D7  D7}qD8�D8� D9  D9��D:  D:� D:�qD;z�D;�qD<� D=�D=� D>D>��D?�D?� D@  D@��D@�qDA}qDB  DB� DC  DC� DC�qDD� DE�DE� DE�qDF��DG�DG}qDG��DH� DI  DI� DJ  DJ� DK�DK��DLDL�DM  DM� DN�DN� DN�qDO}qDP  DP� DQ  DQ� DQ�qDR� DR�qDSz�DS��DTz�DT�qDU� DV  DV}qDW�DW��DX  DX}qDY  DY� DZ�DZ��D[  D[� D\�D\}qD\�qD]� D^�D^��D_�D_��D_�qD`}qDa  Da}qDa��Dbz�Db��Dc� Dd�Dd��De�De��Df�Df��Dg  Dg}qDh  Dh� Di�Di� Di�qDj� Dk  Dk� Dl�Dl� Dl�qDmz�Dn  Dn��Do�Do��Dp  Dp}qDp�qDq}qDq�qDr}qDs  Ds��Dt�Dt� Dt�qDu� Dv  Dv� Dw�Dw}qDw��Dx� Dy  Dy}qDz  Dz� D{  D{}qD|  D|� D}�D}��D~  D~}qD~�qD� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�}qD�� D���D�@ D��HD��HD��D�AHD�� D���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�D�HD�@ D��HD�� D���D�AHD�� D���D���D�@ D��HD���D���D�@ D��HD��HD�HD�@ D��HD�D�HD�>�D�~�D���D���D�@ D�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D���D���D�@ D�� D��HD�HD�AHD�� D���D�HD�AHD��HD�� D���D�AHD��HD��HD�  D�@ D�� D��HD��D�@ D��HD��HD�  D�@ D��HD�� D�HD�B�D��HD�� D���D�@ D���D�D�  D�>�D�}qD�� D�HD�AHD��HD�� D���D�=qD�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD���D�� D��qD�>�D�� D��HD��D�@ D�~�D��HD�HD�@ D�~�D�� D�  D�=qD�� D��HD��D�AHD�� D��HD��D�AHD�� D��HD�  D�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�>�D�|)D��qD���D�>�D�� D��HD�  D�>�D�� D�� D�HD�@ D�}qD�� D�HD�>�D�� D��HD��D�AHD�~�D�� D�  D�@ D��HD��HD��D�B�D�~�D��qD���D�>�D�� D��HD���D�>�D���D�D��D�@ D�}qD��qD�  D�@ D�}qD���D���D�>�D�~�D���D���D�>�D�~�D�� D�  D�AHD��HD��HD�HD�>�D�~�D��HD���D�@ D��HD�� D���D�>�D�� D���D��qD�>�D�~�D�� D���D�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD�� D��HD�  D�>�D�� D�� D���D�@ DHD��HD�  D�>�D�~�Dþ�D�  D�@ D�~�D�� D�HD�@ Dŀ D�� D�HD�B�DƁHD��HD�HD�AHDǁHD��HD�  D�>�DȀ D�� D�  D�@ D�~�D�� D�  D�>�Dʀ D�� D�  D�@ Dˀ D˾�D���D�>�D̀ D�� D�  D�@ D́HD�� D�  D�AHD΁HDξ�D���D�@ Dπ D��HD�  D�>�DЁHD�� D���D�@ DсHD��HD�HD�AHDҀ DҾ�D�  D�@ DӀ D�� D�HD�@ D�}qDԾ�D�HD�AHDՁHD��HD�HD�@ Dր D��HD��D�AHD�~�D�� D���D�=qD؀ Dؾ�D��qD�>�DفHD�� D�  D�>�D�~�D�� D���D�=qD�~�D��HD�HD�AHD܂�D�� D�  D�B�D݁HD�� D�  D�@ D�~�D�� D��D�AHD߀ D߾�D���D�@ D�� D�� D�  D�@ D� D��HD���D�=qD� D�� D��qD�=qD� D��HD���D�>�D�~�D�� D�HD�B�D� D徸D���D�>�D� D�� D�HD�AHD� D羸D�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�AHD�~�D�qD�  D�AHD�HD�� D��qD�>�D� D�� D��qD�>�D�~�D�� D�HD�AHD� D��HD�  D�>�D�HD�� D���D�AHD�� D�� D�HD�B�D�HD�� D��D�B�D�HD��HD�HD�AHD�~�D�D���D�=qD� D���D���D�>�D�~�D���D���D�=qD�}qD�� D�  D�@ D�� D��HD�HD�AHD��HD���D���D�AHD�� D���D�HD�:�D��>�G�?8Q�?u?�z�?\?�(�@   @�@#�
@0��@J=q@Y��@k�@}p�@���@��@��H@��\@��@�Q�@�G�@�=q@�@�  @�ff@�33@��RA�
AQ�A�RAz�A��A�RA%�A*�HA0  A5�A;�AAG�AG
=AK�AQ�AXQ�A]p�Ab�\AhQ�An{As33AxQ�A}p�A���A�z�A�
=A���A�z�A�\)A�=qA���A�
=A�=qA��A�  A��\A��A�Q�A�33A�A�Q�A�33A�ffA�G�A��
A��RA���A�z�A�
=Aə�A���AϮAҏ\A��A׮A��HA�A�Q�A�\A�p�A�Q�A�\A���A�A�\A���A�
=A���A�z�A�
=B z�B��B
=BQ�Bp�B�\B�
B	�B
ffB33Bz�BB33B(�BG�B�RB(�Bp�B�\B  Bp�B
=BQ�Bp�B
=B ��B!B#33B$z�B%�B'�B(��B)�B+\)B,��B.{B/\)B0��B2=qB3�B5�B6=qB7�B9�B:�RB;�
B=�B>�\B@  BAG�BB�\BC�
BEG�BF�\BG�BH��BJ=qBK�BL��BM�BO\)BP��BQBS
=BTz�BU�BW\)BX��BZ{B[\)B\��B^ffB_�
BaG�Bb�\Bd  Be��Bf�HBh(�Bip�Bj�HBlQ�BmBo
=BpQ�BqBs33Btz�BuBv�HBxQ�ByB{33B|z�B}��B~�HB�=qB���B��B�{B���B��B�=qB��HB�p�B�(�B���B��B�=qB��HB���B�Q�B���B���B�=qB���B��B�Q�B��HB��B�Q�B���B���B�(�B��HB���B�=qB��HB��B�(�B��HB���B�=qB���B�p�B�(�B��HB�p�B�  B���B�\)B�{B��RB�\)B��B���B�G�B�  B���B�33B�B�z�B�33B��
B�ffB�
=B���B�ffB�
=B���B�(�B���B�p�B�(�B���B�\)B��B��\B�33B��
B�z�B�
=B���B�(�B��HB��B�(�B���B�G�B��B��\B�G�B�  B���B�33B�B�ffB��B�B�Q�B���B��B�=qB��HB��B�(�B��RB�G�B��
B�ffB�
=BÅB�  B�ffBĸRB���B�G�Bř�B�B�B��
B��B�  B�  B�  B��B��
B��
B�B��
B��
B�BŮBř�BŮB�B�B�Bř�BŮBŮB�B��
B�B�B�B�B��
B��B�  B�{B�{B�(�B�(�B�=qB�Q�B�z�BƏ\BƸRBƸRBƸRB��HB���B��B�G�B�p�B�p�BǅBǮB��
B�  B�(�B�Q�B�Q�B�z�Bȏ\BȸRB��HB��B�G�B�\)BɅBə�B�B��B�{B�Q�B�z�BʸRB���B���B�
=B�G�B�\)B˙�B�B�  B�(�B�=qB�ffB̏\B̸RB�
=B�33B�\)BͅB͙�B�B��B�{B�=qB�z�BΣ�B��HB�
=B��B�G�B�p�Bϙ�B�B�  B�(�B�Q�B�ffBУ�BиRB��HB��B�\)BхB�B��B�{B�=qB�ffBң�B��HB��B�\)Bә�B��
B�{B�=qB�z�B���B�
=B�\)BծB��B�(�B�z�BָRB���B�33BׅB�B�{B�ffBظRB�
=B�\)BٮB�  B�=qB�z�BڸRB�
=B�\)Bۙ�B��B�Q�Bܣ�B��HB��B�p�B�B�  B�Q�Bޏ\B��HB�G�Bߙ�B��B�=qB��B���B�33B�B��
B�(�B�ffB�RB�
=B�\)B�B�{B�z�B���B�
=B�p�B�B�(�B�z�B��HB�33B癚B��B�=qB�\B��HB�33B�B��B�=qB��B���B�\)B�B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�BB�  B�ffB�RB��B�p�B��
B�(�B�\B���B�G�B�B�{B�z�B��HB�G�B���B�{B�z�B��HB�G�B��B�  B�ffB��HB�G�B��B�  B�ffB���B��B��B��B�Q�B���B���B�p�B��B�{B�ffB���B��B��B��C �C Q�C �C �C �
C
=C33CffC��CC��C�CG�Cz�C��C��C  C(�CQ�Cz�C��C��C  C�CQ�Cz�C��CC�C{CG�Cp�C��C��C��C(�CQ�Cz�C�C�
C
=C33C\)C�\C�RC�C
=C=qCffC�\C�RC�HC	
=C	33C	\)C	�C	�C	�
C	��C
�C
G�C
p�C
��C
C
�C�CG�CffC��CC�C{CG�Cp�C��C��C  C(�CQ�C�\C�RC�HC{C=qCp�C��CC��C�CQ�Cz�C��C�
C
=C33CffC��C��C  C33CffC��C��C��C(�CQ�C�CC��C(�CffC��C�
C{C=qCp�C�C�HC{C=qCz�C�C�C�C\)C�\CC  C=qCp�C�C�C(�CffC�C�C(�CffC�C�HC�C\)C��C�HC�CffC��C�C33Cp�C�C�C(�CffC�C�HC33Cz�CC  CG�C�CC   C =qC �C �
C!�C!ffC!�C!�C"33C"z�C"C#  C#G�C#�\C#��C${C$\)C$�C$��C%=qC%�C%��C&{C&\)C&�C'  C'G�C'��C'�
C(�C(p�C(�RC)  C)G�C)��C)�C*G�C*�\C*�
C+�C+ffC+�C,
=C,\)C,�C-  C-G�C-�\C-�HC.33C.z�C.�
C/(�C/z�C/��C0�C0ffC0�C1  C1Q�C1�RC2
=C2\)C2�C2�C3G�C3��C3�C4G�C4��C4�C5=qC5�C5�HC633C6�\C6�C7G�C7��C7�HC833C8�\C8�C9G�C9��C:  C:Q�C:��C:��C;Q�C;��C<
=C<ffC<C={C=ffC=C>�C>�C>�
C?33C?�C?�
C@=qC@��C@��CAG�CA��CA��CB\)CB�RCC�CCp�CC��CD�CDz�CD�
CE33CE��CE��CFQ�CF��CF��CG\)CGCH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141114444441444411444141144441111414111114411111411111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�+A�-A�-A�-A�-A�-A�-A�/A�/A�1'A�/A�-A�/A�/A�/A�-A�/A�1'A�1'A�1'A�/A�1'A�1'A�5?A�9XA�;dA�;dA�7LA�7LA�5?A�1'A�&�A�oA�1A��A���AϏ\A��A͕�A�/A�(�A�(�A�"�A��A�A��A���A�hsA���A�/A��A��A��yA�ZA�(�A�?}A��!A��;A���A�1'A��RA���A���A�+A���A�ffA��A��\A���A��RA�E�A�C�A�+A�v�A���A�;dA�JA���Ay�At �Ao�PAj �Af�Ab�uA_A^-AY�ASl�AL�AJ��AG7LAE�AC�AA��A@��A?ƨA>��A>Q�A=�;A>M�A?;dA?��A=`BA6�A3��A3�A2��A3�A2�+A2ffA29XA1�TA1��A1O�A1O�A0��A/�PA/�hA0Q�A0�uA/�
A/dZA.�A.n�A.A-�A,M�A+�^A*�yA*5?A)��A(��A'\)A&9XA%�-A%XA$�/A$9XA#�^A#dZA#S�A#C�A#+A"��A"�\A"Q�A!��A!�A!�A Q�A��A��Ap�A;dAVA�A�AC�AdZAS�A%A�A=qA{A�A�^A��A�A7LA%A�/A��AffAI�A1'A  AƨA`BA��A^5A�A�hA?}A%A�jAz�AbA�PAhsAC�A&�A�`A�!A9XA��A33A��A�A��AhsA7LAoA�!Ar�AA�AAA�AK�AVA��A��A�A��A��AffA�A��A�A�+A{A��AS�A
v�A	�A	C�A�`A�RA��A�uA�\AbA�^A��A"�A�!Av�AZA=qA-Al�A�A�/A�RAjA{A��A\)A+A��A1'A�Ap�A ��A ffA E�A E�A E�A =q@���@�ȴ@�n�@�E�@�J@��#@��@�?}@���@�r�@�S�@��H@�V@���@���@��h@�p�@�G�@�/@��/@��P@���@�?}@� �@�"�@�{@���@�@�(�@�F@��@��@��@�t�@�"�@��@�ȴ@@�E�@���@�hs@���@���@�9@�9@�@���@�
=@�V@��@�x�@�&�@���@�(�@���@�|�@�+@��H@�!@�V@�j@�ƨ@�@�-@���@�G�@��@��;@�o@޸R@�^5@��@ݲ-@ݩ�@�x�@܋D@���@�|�@�+@��H@�~�@��@�1'@��H@ָR@�n�@���@Ձ@�`B@��@���@�Z@�1@Ӯ@�"�@Ұ!@�-@�7L@�K�@θR@�=q@��T@͑h@�G�@��`@�9X@�dZ@��@�ȴ@ʇ+@�@ɡ�@�?}@ȣ�@�A�@ǍP@�@�5?@�@Ł@�&�@���@��@��@�Z@�Z@�b@�\)@���@§�@�J@���@�&�@�r�@�Q�@�Q�@�b@�\)@���@���@��!@��@�Q�@�ƨ@��P@�K�@�+@���@��T@�hs@�G�@�V@��`@�A�@��
@�|�@�
=@���@���@�G�@���@�bN@���@�t�@�C�@��!@�J@��-@�?}@���@��D@�Q�@�A�@�1@���@�t�@�S�@��@��\@�V@�@�x�@�&�@��`@��j@�  @��P@�"�@�@���@�n�@�J@��@��T@�@�hs@��@��D@� �@��m@��@�l�@�"�@���@��\@�~�@�M�@�{@�@�@��h@�7L@���@���@�9X@�  @�ƨ@���@�\)@�K�@�;d@��y@���@�V@���@��@�%@��j@��u@�Q�@�1@���@���@�dZ@��y@��R@��+@�E�@�J@��#@�p�@���@��@��@�r�@��m@��@�"�@��+@�=q@��#@�X@�&�@���@���@��D@�bN@�A�@��@�C�@�@�ȴ@���@��+@�V@�J@�@�/@�z�@�ƨ@��@�+@�@��H@�v�@��@��@�@�?}@��`@��@�1'@��@�b@�1@��w@�K�@���@���@���@�E�@�J@��#@�x�@�G�@��/@���@�I�@� �@��;@���@�S�@�C�@���@�V@��#@�X@���@���@���@�r�@�9X@�  @��@�|�@�K�@�
=@���@���@�-@���@��h@�`B@�&�@�Ĝ@��D@�z�@�bN@�9X@�b@���@��P@�33@��R@��\@�^5@�5?@��T@�X@�%@�Ĝ@��9@�z�@�1@�@��@��@;d@~�R@~5?@}`B@}O�@|��@|��@|j@|(�@{t�@z��@z�@y�^@y�7@yX@y7L@xr�@w�P@w�@w
=@v��@u��@u�h@u/@t�/@t��@tz�@t9X@t1@sƨ@sƨ@s��@sS�@s33@r�\@rM�@r�@q��@q�#@q��@qhs@qX@q&�@p��@p��@p�u@pr�@pbN@o�;@ol�@oK�@n��@n��@nv�@nff@nV@nV@n$�@m�-@mp�@m`B@m�@l��@lZ@k�F@k@jn�@j�@i��@ix�@iX@i�@h��@h�u@hr�@g�;@g\)@fȴ@f{@e�-@eV@d��@dI�@c�m@c"�@b��@b^5@a��@a��@aG�@a&�@`��@`Q�@`b@`  @_�;@_�@_|�@_l�@_;d@_
=@^�@^�R@^V@^@]��@]V@\j@[��@[t�@[dZ@[33@["�@["�@[o@Z~�@Y��@YG�@X��@X�u@XA�@X  @W�w@W|�@W�@Vff@U�@U��@U`B@UO�@U�@T�@Tz�@T(�@Sƨ@SdZ@S"�@R��@Q��@Q7L@P��@PĜ@P��@PA�@P  @O��@O��@O�P@Ol�@O
=@N�R@NV@N{@M�@M��@MO�@M�@L�@L9X@K��@Ko@JM�@I�#@IG�@Hr�@HbN@HbN@H1'@G��@G;d@F�@FV@E��@E@E��@E�-@E`B@Dj@C��@CC�@B�H@Bn�@B-@A�@A��@A�7@@Ĝ@?�;@?|�@?�@>�+@=@=?}@<��@<��@<�D@<(�@;��@;t�@:�H@:��@:^5@9��@9x�@9hs@9�@8��@8Ĝ@8�u@8b@7�;@7��@6�@6�+@6V@6{@5�T@5�-@5O�@4��@4Z@4(�@3�m@3ƨ@3t�@3dZ@333@2�H@2��@2~�@1�@1��@1hs@1�@0�9@0�@0Q�@01'@0  @/�w@/|�@/;d@.�@.v�@.V@.@-��@-`B@,�@,j@,I�@+�m@+��@*��@*J@)��@)�^@)��@)��@)�7@)G�@)&�@)�@(�`@(�9@(bN@( �@(  @'�;@'�@'\)@'�@'
=@&�@&�R@&�+@&5?@%�-@%�h@%�h@%�h@%�@%�@%`B@%O�@%/@$�@$�D@$j@$Z@$I�@$�@$1@#�m@#ƨ@#��@#�@"�H@"��@"�!@"�\@"-@!�#@!��@!x�@!hs@!hs@!G�@!�@ ��@ �u@  �@�w@�P@|�@|�@l�@l�@l�@\)@;d@
=@�y@ȴ@�+@E�@@��@��@`B@��@�@�@�D@j@I�@(�@�@1@��@�m@ƨ@C�@o@�H@��@�\@^5@J@�^@x�@hs@7L@�@��@�`@Ĝ@�9@�u@�@Q�@b@�@�@�w@�P@K�@��@�R@v�@v�@V@V@E�@5?@{@�T@@��@p�@`B@O�@O�@/@��@�/@�@j@Z@(�@��@��@�@S�@o@�H@��@�\@M�@=q@=qA��A��A��A�$�A�(�A�&�A�(�A�1'A�+A�+A�1'A�-A�+A�/A�/A�+A�+A�-A�/A�/A�(�A�-A�/A�+A�+A�-A�/A�+A�-A�1'A�/A�-A�-A�1'A�1'A�/A�/A�33A�33A�-A�+A�/A�/A�-A�-A�/A�/A�-A�+A�/A�1'A�/A�-A�-A�1'A�1'A�/A�-A�/A�1'A�1'A�-A�-A�1'A�1'A�-A�+A�+A�/A�1'A�-A�+A�/A�1'A�/A�-A�/A�5?A�33A�-A�/A�5?A�33A�1'A�1'A�1'A�33A�/A�-A�/A�33A�33A�/A�/A�33A�1'A�/A�-A�/A�1'A�/A�/A�/A�33A�33A�/A�-A�1'A�33A�1'A�-A�/A�33A�33A�/A�1'A�9XA�;dA�;dA�9XA�7LA�9XA�;dA�9XA�7LA�9XA�=qA�=qA�;dA�;dA�=qA�=qA�;dA�9XA�=qA�;dA�7LA�33A�5?A�9XA�;dA�7LA�5?A�9XA�;dA�7LA�33A�1'A�5?A�9XA�5?A�1'A�5?A�5?A�33A�33A�5?A�33A�1'A�1'A�&�A� �A�$�A�1'A�5?A�"�A��A�oA�JA�VA�bA�VA� �A��A�A���A�A�A��A��A��mA��yA��A��A��mA���A���A���A���A���A�ƨAϰ!Aϧ�A�x�A�t�A�dZA�$�A��TA��A��A��A��mA�ƨA��A͇+A�Q�A�A�A�;dA�9XA�1'A�-A�-A�/A�-A�(�A�&�A�+A�+A�(�A�&�A�&�A�+A�+A�(�A�$�A�$�A�&�A�&�A�$�A�"�A��A��A� �A��A�{A�oA�oA�{A�oA�1A�%A�A�A�A���A��A��A��A���A��A��A��mA��`A��HA���A̬Ḁ�A̋DA�|�A̓uÃA�^5A�=qA�oA��A���A�ȴA˾wA˰!A˧�A˙�AˑhAˋDA�t�A�/A�~�A�
=A�S�A���A���Aǝ�A�hsA���AƝ�A�`BA�JA���AŋDA�(�A���A�r�A�-A���AËDA�hsA�?}A�5?A��A���A£�A�VA��TA���A�n�A�VA�E�A�9XA�1'A�$�A��A� �A�"�A�"�A�-A�1'A�1'A�5?A�33A�33A�1'A�33A�5?A�5?A�33A�/A�+A�-A�(�A�"�A��A�
=A���A��A��HA���A��-A��!A��A��9A��-A��^A��wA��jA��RA��9A��!A���A���A��PA��+A�S�A��#A�z�A��A�p�A��A�hsA��A��A�K�A�&�A�A��HA���A���A�XA�VA�A��\A��A�|�A�z�A�z�A�v�A�r�A�n�A�hsA�bNA�1'A�hsA���A��#A���A�ƨA�ƨA��wA�ĜA�A��jA��jA��RA��!A���A��A�VA�I�A�?}A��A�
=A���A��;A��/A��
A��
A���A���A���A�ȴA���A��A���A�~�A�p�A�dZA�VA�O�A�G�A�C�A�A�A�5?A��A��A���A���A���A��+A�A�A�bNA�A��+A�G�A���A���A�1A��A�oA��wA�|�A�VA�(�A���A��hA���A��A��9A�jA�7LA�oA���A��A��`A���A���A��A�ffA�O�A�=qA�1'A�&�A�A�ƨA���A��hA��A�XA�1'A�bA��A��/A���A�ƨA��!A�v�A�5?A���A��A��A��\A�dZA�A�A�JA��TA��PA�=qA��A��9A��uA�33A��A�bA�
=A�1A�
=A�JA�1A�A���A��A�A��\A�v�A�=qA��A��A�  A��wA���A���A��hA�z�A�7LA��A�1A��A��A��FA��A���A���A���A��hA��DA��A��A�|�A�x�A�t�A�ffA�O�A�+A� �A�A���A��A��A��A��#A���A��A�O�A���A��#A�A���A���A��hA��A�z�A�v�A�t�A�n�A�ffA�`BA�ZA�O�A�A�A�33A�/A�VA��HA�n�A��\A�1A��HA��^A���A��DA�I�A��
A�"�A���A��`A���A���A���A��\A�z�A�jA�ZA�M�A�I�A�;dA�/A�-A�"�A�A���A���A�~�A�ZA�C�A�VA�VA��jA��HA� �A�;dA�C�A���A���A���A�?}A���A�Q�A�ĜA�S�A�+A���A���A���A�v�A�G�A�C�A�=qA�33A�(�A��A��A�z�A���A�Q�A�x�A� �A��mA��RA�x�A��A���A���A�|�A���A��/A��RA���A�bNA�-A�ȴA�M�A�A�33A�A��;A���A��`A��A�dZA�Q�A�&�A�ȴA�|�A�I�A��A��\A�S�A��A���A�ffA� �A���A���A�t�A�{A�I�A�|�A� �A��A~�\A|��A{&�AzjAy�mAyK�Aw��Aw"�Av��Au��Au��AuhsAuVAt��At~�At1As�As
=Ar�RArffAr{Aq�Aq
=Ap(�Ao��AnQ�Al�+AkS�Aj�Aj��Aj�\Aj�Aj9XAiAihsAh��Ag�#AgVAf�Ae��Ae�Ae/Ad�Ad��AdA�Ac�TAcS�Ab��AbAa�PAa"�A`�HA`��A`�\A_�mA_dZA_7LA_/A_�A^��A^�A^��A^�RA^�+A]��A\�+A[�mA[XAZ�HAZ�!AY�mAX��AXE�AWAW�AVv�AUx�ATQ�AQ�AN��AM�AMXAM+AL��ALĜAL��ALr�AL^5AL5?AK�AK�-AKhsAJ��AH�\AHJAGƨAG�hAG7LAF�AFv�AF9XAF  AE�AEK�AD�yADffAD�AC�mACƨAC�AC�ACS�AC/AB�yABjAA�AA��AAS�AAC�AA;dAA&�AA
=A@��A@�A@�+A@VA@ �A?��A?ƨA?��A?�A?O�A?&�A?A>ȴA>��A>�\A>�A>r�A>n�A>bNA>VA>9XA=�mA=�A>  A=�A=�;A=�-A=x�A=��A=�
A>(�A?A?G�A?C�A?O�A?dZA?K�A>�A?&�A?;dA?�A?A?��A?�A@bA?\)A=��A<��A;��A:ĜA9��A8�A6��A57LA4��A41'A4  A3�A3�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                 A��A�+A�-A�-A�-A�-A�-A�-A�/A�/A�1'A�/A�-A�/A�/A�/A�-A�/A�1'A�1'A�1'A�/A�1'A�1'A�5?A�9XA�;dA�;dA�7LA�7LA�5?A�1'A�&�A�oA�1A��A���AϏ\A��A͕�A�/A�(�A�(�A�"�A��A�A��A���A�hsA���A�/A��A��A��yA�ZA�(�A�?}A��!A��;A���A�1'A��RA���A���A�+A���A�ffA��A��\A���A��RA�E�A�C�A�+A�v�A���A�;dA�JA���Ay�At �Ao�PAj �Af�Ab�uA_A^-AY�ASl�AL�AJ��AG7LAE�AC�AA��A@��A?ƨA>��A>Q�A=�;A>M�A?;dA?��A=`BA6�A3��A3�A2��A3�A2�+A2ffA29XA1�TA1��A1O�A1O�A0��A/�PA/�hA0Q�A0�uA/�
A/dZA.�A.n�A.A-�A,M�A+�^A*�yA*5?A)��A(��A'\)A&9XA%�-A%XA$�/A$9XA#�^A#dZA#S�A#C�A#+A"��A"�\A"Q�A!��A!�A!�A Q�A��A��Ap�A;dAVA�A�AC�AdZAS�A%A�A=qA{A�A�^A��A�A7LA%A�/A��AffAI�A1'A  AƨA`BA��A^5A�A�hA?}A%A�jAz�AbA�PAhsAC�A&�A�`A�!A9XA��A33A��A�A��AhsA7LAoA�!Ar�AA�AAA�AK�AVA��A��A�A��A��AffA�A��A�A�+A{A��AS�A
v�A	�A	C�A�`A�RA��A�uA�\AbA�^A��A"�A�!Av�AZA=qA-Al�A�A�/A�RAjA{A��A\)A+A��A1'A�Ap�A ��A ffA E�A E�A E�A =q@���@�ȴ@�n�@�E�@�J@��#@��@�?}@���@�r�@�S�@��H@�V@���@���@��h@�p�@�G�@�/@��/@��P@���@�?}@� �@�"�@�{@���@�@�(�@�F@��@��@��@�t�@�"�@��@�ȴ@@�E�@���@�hs@���@���@�9@�9@�@���@�
=@�V@��@�x�@�&�@���@�(�@���@�|�@�+@��H@�!@�V@�j@�ƨ@�@�-@���@�G�@��@��;@�o@޸R@�^5@��@ݲ-@ݩ�@�x�@܋D@���@�|�@�+@��H@�~�@��@�1'@��H@ָR@�n�@���@Ձ@�`B@��@���@�Z@�1@Ӯ@�"�@Ұ!@�-@�7L@�K�@θR@�=q@��T@͑h@�G�@��`@�9X@�dZ@��@�ȴ@ʇ+@�@ɡ�@�?}@ȣ�@�A�@ǍP@�@�5?@�@Ł@�&�@���@��@��@�Z@�Z@�b@�\)@���@§�@�J@���@�&�@�r�@�Q�@�Q�@�b@�\)@���@���@��!@��@�Q�@�ƨ@��P@�K�@�+@���@��T@�hs@�G�@�V@��`@�A�@��
@�|�@�
=@���@���@�G�@���@�bN@���@�t�@�C�@��!@�J@��-@�?}@���@��D@�Q�@�A�@�1@���@�t�@�S�@��@��\@�V@�@�x�@�&�@��`@��j@�  @��P@�"�@�@���@�n�@�J@��@��T@�@�hs@��@��D@� �@��m@��@�l�@�"�@���@��\@�~�@�M�@�{@�@�@��h@�7L@���@���@�9X@�  @�ƨ@���@�\)@�K�@�;d@��y@���@�V@���@��@�%@��j@��u@�Q�@�1@���@���@�dZ@��y@��R@��+@�E�@�J@��#@�p�@���@��@��@�r�@��m@��@�"�@��+@�=q@��#@�X@�&�@���@���@��D@�bN@�A�@��@�C�@�@�ȴ@���@��+@�V@�J@�@�/@�z�@�ƨ@��@�+@�@��H@�v�@��@��@�@�?}@��`@��@�1'@��@�b@�1@��w@�K�@���@���@���@�E�@�J@��#@�x�@�G�@��/@���@�I�@� �@��;@���@�S�@�C�@���@�V@��#@�X@���@���@���@�r�@�9X@�  @��@�|�@�K�@�
=@���@���@�-@���@��h@�`B@�&�@�Ĝ@��D@�z�@�bN@�9X@�b@���@��P@�33@��R@��\@�^5@�5?@��T@�X@�%@�Ĝ@��9@�z�@�1@�@��@��@;d@~�R@~5?@}`B@}O�@|��@|��@|j@|(�@{t�@z��@z�@y�^@y�7@yX@y7L@xr�@w�P@w�@w
=@v��@u��@u�h@u/@t�/@t��@tz�@t9X@t1@sƨ@sƨ@s��@sS�@s33@r�\@rM�@r�@q��@q�#@q��@qhs@qX@q&�@p��@p��@p�u@pr�@pbN@o�;@ol�@oK�@n��@n��@nv�@nff@nV@nV@n$�@m�-@mp�@m`B@m�@l��@lZ@k�F@k@jn�@j�@i��@ix�@iX@i�@h��@h�u@hr�@g�;@g\)@fȴ@f{@e�-@eV@d��@dI�@c�m@c"�@b��@b^5@a��@a��@aG�@a&�@`��@`Q�@`b@`  @_�;@_�@_|�@_l�@_;d@_
=@^�@^�R@^V@^@]��@]V@\j@[��@[t�@[dZ@[33@["�@["�@[o@Z~�@Y��@YG�@X��@X�u@XA�@X  @W�w@W|�@W�@Vff@U�@U��@U`B@UO�@U�@T�@Tz�@T(�@Sƨ@SdZ@S"�@R��@Q��@Q7L@P��@PĜ@P��@PA�@P  @O��@O��@O�P@Ol�@O
=@N�R@NV@N{@M�@M��@MO�@M�@L�@L9X@K��@Ko@JM�@I�#@IG�@Hr�@HbN@HbN@H1'@G��@G;d@F�@FV@E��@E@E��@E�-@E`B@Dj@C��@CC�@B�H@Bn�@B-@A�@A��@A�7@@Ĝ@?�;@?|�@?�@>�+@=@=?}@<��@<��@<�D@<(�@;��@;t�@:�H@:��@:^5@9��@9x�@9hs@9�@8��@8Ĝ@8�u@8b@7�;@7��@6�@6�+@6V@6{@5�T@5�-@5O�@4��@4Z@4(�@3�m@3ƨ@3t�@3dZ@333@2�H@2��@2~�@1�@1��@1hs@1�@0�9@0�@0Q�@01'@0  @/�w@/|�@/;d@.�@.v�@.V@.@-��@-`B@,�@,j@,I�@+�m@+��@*��@*J@)��@)�^@)��@)��@)�7@)G�@)&�@)�@(�`@(�9@(bN@( �@(  @'�;@'�@'\)@'�@'
=@&�@&�R@&�+@&5?@%�-@%�h@%�h@%�h@%�@%�@%`B@%O�@%/@$�@$�D@$j@$Z@$I�@$�@$1@#�m@#ƨ@#��@#�@"�H@"��@"�!@"�\@"-@!�#@!��@!x�@!hs@!hs@!G�@!�@ ��@ �u@  �@�w@�P@|�@|�@l�@l�@l�@\)@;d@
=@�y@ȴ@�+@E�@@��@��@`B@��@�@�@�D@j@I�@(�@�@1@��@�m@ƨ@C�@o@�H@��@�\@^5@J@�^@x�@hs@7L@�@��@�`@Ĝ@�9@�u@�@Q�@b@�@�@�w@�P@K�@��@�R@v�@v�@V@V@E�@5?@{@�T@@��@p�@`B@O�@O�@/@��@�/@�@j@Z@(�@��@��@�@S�@o@�H@��@�\@M�@=q@=qA��A��A��A�$�A�(�A�&�A�(�A�1'A�+A�+A�1'A�-A�+A�/A�/A�+A�+A�-A�/A�/A�(�A�-A�/A�+A�+A�-A�/A�+A�-A�1'A�/A�-A�-A�1'A�1'A�/A�/A�33A�33A�-A�+A�/A�/A�-A�-A�/A�/A�-A�+A�/A�1'A�/A�-A�-A�1'A�1'A�/A�-A�/A�1'A�1'A�-A�-A�1'A�1'A�-A�+A�+A�/A�1'A�-A�+A�/A�1'A�/A�-A�/A�5?A�33A�-A�/A�5?A�33A�1'A�1'A�1'A�33A�/A�-A�/A�33A�33A�/A�/A�33A�1'A�/A�-A�/A�1'A�/A�/A�/A�33A�33A�/A�-A�1'A�33A�1'A�-A�/A�33A�33A�/A�1'A�9XA�;dA�;dA�9XA�7LA�9XA�;dA�9XA�7LA�9XA�=qA�=qA�;dA�;dA�=qA�=qA�;dA�9XA�=qA�;dA�7LA�33A�5?A�9XA�;dA�7LA�5?A�9XA�;dA�7LA�33A�1'A�5?A�9XA�5?A�1'A�5?A�5?A�33A�33A�5?A�33A�1'A�1'A�&�A� �A�$�A�1'A�5?A�"�A��A�oA�JA�VA�bA�VA� �A��A�A���A�A�A��A��A��mA��yA��A��A��mA���A���A���A���A���A�ƨAϰ!Aϧ�A�x�A�t�A�dZA�$�A��TA��A��A��A��mA�ƨA��A͇+A�Q�A�A�A�;dA�9XA�1'A�-A�-A�/A�-A�(�A�&�A�+A�+A�(�A�&�A�&�A�+A�+A�(�A�$�A�$�A�&�A�&�A�$�A�"�A��A��A� �A��A�{A�oA�oA�{A�oA�1A�%A�A�A�A���A��A��A��A���A��A��A��mA��`A��HA���A̬Ḁ�A̋DA�|�A̓uÃA�^5A�=qA�oA��A���A�ȴA˾wA˰!A˧�A˙�AˑhAˋDA�t�A�/A�~�A�
=A�S�A���A���Aǝ�A�hsA���AƝ�A�`BA�JA���AŋDA�(�A���A�r�A�-A���AËDA�hsA�?}A�5?A��A���A£�A�VA��TA���A�n�A�VA�E�A�9XA�1'A�$�A��A� �A�"�A�"�A�-A�1'A�1'A�5?A�33A�33A�1'A�33A�5?A�5?A�33A�/A�+A�-A�(�A�"�A��A�
=A���A��A��HA���A��-A��!A��A��9A��-A��^A��wA��jA��RA��9A��!A���A���A��PA��+A�S�A��#A�z�A��A�p�A��A�hsA��A��A�K�A�&�A�A��HA���A���A�XA�VA�A��\A��A�|�A�z�A�z�A�v�A�r�A�n�A�hsA�bNA�1'A�hsA���A��#A���A�ƨA�ƨA��wA�ĜA�A��jA��jA��RA��!A���A��A�VA�I�A�?}A��A�
=A���A��;A��/A��
A��
A���A���A���A�ȴA���A��A���A�~�A�p�A�dZA�VA�O�A�G�A�C�A�A�A�5?A��A��A���A���A���A��+A�A�A�bNA�A��+A�G�A���A���A�1A��A�oA��wA�|�A�VA�(�A���A��hA���A��A��9A�jA�7LA�oA���A��A��`A���A���A��A�ffA�O�A�=qA�1'A�&�A�A�ƨA���A��hA��A�XA�1'A�bA��A��/A���A�ƨA��!A�v�A�5?A���A��A��A��\A�dZA�A�A�JA��TA��PA�=qA��A��9A��uA�33A��A�bA�
=A�1A�
=A�JA�1A�A���A��A�A��\A�v�A�=qA��A��A�  A��wA���A���A��hA�z�A�7LA��A�1A��A��A��FA��A���A���A���A��hA��DA��A��A�|�A�x�A�t�A�ffA�O�A�+A� �A�A���A��A��A��A��#A���A��A�O�A���A��#A�A���A���A��hA��A�z�A�v�A�t�A�n�A�ffA�`BA�ZA�O�A�A�A�33A�/A�VA��HA�n�A��\A�1A��HA��^A���A��DA�I�A��
A�"�A���A��`A���A���A���A��\A�z�A�jA�ZA�M�A�I�A�;dA�/A�-A�"�A�A���A���A�~�A�ZA�C�A�VA�VA��jA��HA� �A�;dA�C�A���A���A���A�?}A���A�Q�A�ĜA�S�A�+A���A���A���A�v�A�G�A�C�A�=qA�33A�(�A��A��A�z�A���A�Q�A�x�A� �A��mA��RA�x�A��A���A���A�|�A���A��/A��RA���A�bNA�-A�ȴA�M�A�A�33A�A��;A���A��`A��A�dZA�Q�A�&�A�ȴA�|�A�I�A��A��\A�S�A��A���A�ffA� �A���A���A�t�A�{A�I�A�|�A� �A��A~�\A|��A{&�AzjAy�mAyK�Aw��Aw"�Av��Au��Au��AuhsAuVAt��At~�At1As�As
=Ar�RArffAr{Aq�Aq
=Ap(�Ao��AnQ�Al�+AkS�Aj�Aj��Aj�\Aj�Aj9XAiAihsAh��Ag�#AgVAf�Ae��Ae�Ae/Ad�Ad��AdA�Ac�TAcS�Ab��AbAa�PAa"�A`�HA`��A`�\A_�mA_dZA_7LA_/A_�A^��A^�A^��A^�RA^�+A]��A\�+A[�mA[XAZ�HAZ�!AY�mAX��AXE�AWAW�AVv�AUx�ATQ�AQ�AN��AM�AMXAM+AL��ALĜAL��ALr�AL^5AL5?AK�AK�-AKhsAJ��AH�\AHJAGƨAG�hAG7LAF�AFv�AF9XAF  AE�AEK�AD�yADffAD�AC�mACƨAC�AC�ACS�AC/AB�yABjAA�AA��AAS�AAC�AA;dAA&�AA
=A@��A@�A@�+A@VA@ �A?��A?ƨA?��A?�A?O�A?&�A?A>ȴA>��A>�\A>�A>r�A>n�A>bNA>VA>9XA=�mA=�A>  A=�A=�;A=�-A=x�A=��A=�
A>(�A?A?G�A?C�A?O�A?dZA?K�A>�A?&�A?;dA?�A?A?��A?�A@bA?\)A=��A<��A;��A:ĜA9��A8�A6��A57LA4��A41'A4  A3�A3�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�gB	�gB	��B	՛B	��B	��B	՛B	�B	�B	�2B	�B	՛B	��B	��B	��B	�gB	��B	��B	�B	�B	՛B	�gB	՛B	�2B	�gB	�2B	�gB	��B	��B	��B	ԕB	��B	�NB	� B	�B	�dB	ɆB	��B	�!B	�B	��B	�VB	��B	�OB	��B	��B	��B	��B	ĜB	רB
hB
49B
\�B
l�B
bB
_B
e�B
`vB
I�B
<6B
7�B
33B
,=B
3�B
>BB
F?B
Q�B
H�B
F�B
,=B
-wB
�B
B	�B	��B	��B	��B	ŢB	��B	��B	��B	��B	t�B	`�B	OBB	G�B	:�B	0UB��B	 �B�iB�"B� B�B��B	�B	B	_B	)�B	W
B	�%B	�kB	�-B	��B	��B	��B	�LB	�B	یB	�5B	�`B	�
B	�B	�MB
�B
B	�]B
{B
!-B
5�B
5tB
3hB
33B
2-B
1[B
33B
5�B
6B
;0B
<B
<�B
=<B
C�B
C�B
EB
E�B
GzB
J�B
M�B
NpB
M�B
M�B
M�B
N�B
PB
P�B
R�B
U2B
W�B
a�B
d�B
g�B
j�B
m�B
q�B
y	B
{B
�AB
��B
�B
��B
��B
��B
��B
�+B
��B
��B
�+B
��B
�YB
�YB
��B
��B
��B
�{B
�MB
��B
��B
��B
��B
�hB
�.B
�\B
��B
��B
�VB
�VB
�JB
��B
�=B
��B
��B
�7B
��B
��B
��B
�{B
�B
��B
��B
��B
�4B
� B
~�B
~�B
}�B
}�B
|�B
{�B
{B
zB
w2B
sB
n�B
m�B
m)B
j�B
h�B
d�B
b�B
^�B
\�B
Z�B
Z�B
U�B
V�B
V�B
U�B
U2B
T�B
T�B
T�B
Q�B
PHB
OvB
M�B
MB
L�B
L0B
K�B
Q�B
QNB
P�B
QNB
R B
QB
PB
OBB
M�B
NpB
L0B
J�B
L�B
K�B
J#B
H�B
H�B
HKB
HB
IRB
GEB
EmB
D�B
CaB
B[B
A�B
@�B
@�B
?B
<6B
;�B
<�B
<B
<B
;�B
;dB
;0B
:�B
:�B
=B
;�B
;�B
9XB
9$B
7LB
6B
7LB
7�B
6B
5tB
5tB
5?B
5tB
5?B
4�B
4�B
4�B
4nB
4�B
4nB
4nB
3�B
2�B
2�B
2�B
3hB
2�B
1�B
1[B
0�B
/�B
/�B
/B
.�B
.B
-�B
-wB
,�B
-B
.�B
*eB
)�B
'RB
%FB
$�B
!�B
"4B
�B
�B
�B
 \B
!B
�B
�B
!B
B
�B
�B
B
�B
�B
~B
�B
_B
�B
�B
�B
�B
�B
YB
�B
$B
�B
�B
SB
�B
�B
�B
@B
�B
uB
�B
B
oB
FB
FB
@B
B
@B
@B
:B
:B
�B
�B
�B
B
4B
 B
�B
�B
.B
�B
:B
�B
B
�B
B
�B
�B
$B
�B
�B
�B
_B
�B
�B
�B
�B
7B
qB
�B
�B
�B
�B
=B
=B
	B
kB
	B
CB
qB
	B
B
OB
!B
�B
�B
!-B
 'B
�B
!-B
 �B
 �B
 �B
"�B
"4B
!�B
!�B
!bB
!�B
!-B
 �B
!-B
 �B
 �B
 �B
!bB
 �B
 �B
!bB
!�B
"hB
"�B
"�B
#nB
$@B
$B
#�B
#�B
$B
$@B
$tB
$@B
$�B
$�B
$�B
%�B
&B
&B
&�B
&�B
&�B
'B
'B
'B
'RB
'B
&�B
'�B
&�B
'�B
'RB
'�B
'�B
'�B
'�B
'�B
'�B
'RB
'B
($B
($B
($B
(�B
)_B
*0B
)�B
*0B
*�B
*eB
)�B
*eB
)�B
+B
*�B
+6B
+kB
+kB
+kB
,�B
-B
-CB
,�B
,�B
-CB
,�B
-�B
-�B
-wB
.}B
.�B
/�B
0!B
0UB
0�B
0UB
0UB
0!B
1�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
2-B
2-B
1�B
2�B
1�B
1�B
2�B
2�B
2�B
2�B
4B
33B
4nB
4nB
4�B
4�B
6B
6B
6�B
6B
6zB
6zB
6FB
6�B
7�B
8RB
8�B
9�B
9$B
8�B
8�B
9XB
9�B
:*B
:^B
<B
<jB
<jB
=qB
<jB
=B
<�B
=B
=<B
=qB
=�B
>B
>B
>wB
>B
?B
?B
?}B
@B
?�B
@OB
@�B
@�B
@�B
@�B
@�B
A B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
CaB
D�B
D�B
EB
EB
E�B
FB
FB
FB
F?B
F?B
F�B
GEB
G�B
GzB
G�B
G�B
G�B
HKB
IB
I�B
J#B
I�B
J#B
J#B
I�B
K)B
K^B
J�B
J�B
K)B
L0B
K�B
LdB
L�B
L�B
L�B
L�B
M6B
M6B
MB
M6B
M6B
MjB
N<B
N<B
NB
N<B
N<B
NpB
NpB
NpB
N�B
N�B
OB
N�B
N�B
N�B
O�B
O�B
OvB
O�B
PHB
PHB
PHB
PB
PB
PHB
P�B
P�B
P}B
P�B
QB
QB
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S[B
S[B
S[B
S[B
S�B
S�B
TaB
T�B
T�B
UgB
T�B
U2B
U2B
U�B
VB
VB
VmB
VmB
W
B
V�B
W
B
W�B
W�B
W�B
W�B
W�B
W�B
XB
XB
XB
XB
XEB
XyB
XyB
X�B
YB
Y�B
Z�B
ZB
ZB
ZQB
ZQB
Y�B
Y�B
[#B
[#B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
[�B
\)B
\)B
\�B
]/B
]dB
]�B
]dB
]/B
]dB
]/B
]/B
]�B
]�B
]/B
]/B
]/B
]�B
]dB
]�B
]�B
]�B
]�B
^B
^5B
^jB
^jB
^jB
^5B
^�B
^�B
_B
_�B
_�B
`�B
aB
aB
a�B
bB
bB
bB
bNB
b�B
b�B
b�B
c B
c�B
c�B
d&B
d�B
e,B
e�B
ffB
gB
g8B
g�B
g�B
g�B
gmB
g8B
g�B
g�B
g�B
g�B
h>B
hsB
iB
iB
iB
iDB
i�B
i�B
jB
jB
jB
jB
jKB
jKB
j�B
kB
kB
kB
k�B
k�B
k�B
k�B
l"B
k�B
lWB
lWB
l�B
l�B
m]B
m�B
ncB
n�B
pB
pB
o�B
o�B
p;B
poB
p;B
poB
pB
o�B
p;B
qB
p�B
p�B
poB
p�B
p�B
qvB
qAB
qAB
r|B
rGB
r�B
r�B
sB
sMB
r�B
s�B
s�B
s�B
t�B
u�B
u�B
uZB
uZB
uZB
uZB
u�B
u�B
u�B
u�B
v`B
v+B
v�B
v�B
v�B
w2B
wfB
w�B
x8B
xB
x�B
x�B
y	B
y�B
zDB
zB
zDB
zB
zB
zDB
zDB
zDB
zxB
z�B
{JB
{JB
{JB
{JB
{�B
{�B
{�B
{�B
{�B
{�B
}"B
|�B
|�B
|�B
}VB
}�B
~(B
~(B
~(B
}�B
~(B
~]B
}�B
}�B
~�B
cB
cB
cB
cB
�B
�B
cB
�B
�B
� B
� B
�4B
�4B
��B
��B
�B
�;B
��B
�AB
�AB
��B
��B
�B
�B
�GB
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
�B
�SB
�SB
��B
��B
�%B
�%B
�%B
�YB
��B
��B
�YB
��B
�+B
�+B
�+B
�+B
�_B
��B
�1B
�1B
��B
��B
��B
��B
��B
��B
�B
�7B
�B
�7B
�7B
�lB
�lB
�lB
�lB
��B
��B
��B
�	B
�=B
�	B
�=B
�	B
�rB
��B
�B
�xB
��B
��B
�B
�JB
�~B
�B	�,B	֡B	�mB	��B	ԕB	�gB	��B	�aB	��B	�mB	�aB	՛B	�mB	�gB	��B	�B	�9B	�B	��B	��B	��B	�gB	��B	�mB	�mB	��B	ԕB	�9B	�B	��B	՛B	�9B	�mB	��B	��B	�mB	֡B	��B	�2B	�B	֡B	ԕB	��B	�mB	�mB	��B	ԕB	�B	֡B	՛B	��B	�2B	֡B	��B	��B	��B	՛B	֡B	�9B	��B	��B	�9B	֡B	�B	�gB	�B	�9B	��B	�aB	��B	�B	֡B	՛B	��B	՛B	��B	�9B	ԕB	�gB	�?B	֡B	��B	��B	�9B	�9B	�mB	��B	�9B	�
B	�mB	ԕB	�2B	�mB	�
B	�gB	��B	֡B	�9B	՛B	�,B	�2B	��B	�mB	�2B	ԕB	�gB	�mB	�gB	�aB	�gB	�9B	�9B	��B	ԕB	՛B	��B	�2B	ԕB	ԕB	�gB	�mB	��B	��B	՛B	�9B	՛B	�aB	��B	��B	��B	��B	�gB	�2B	�B	�gB	��B	�,B	�B	��B	�2B	�,B	�2B	�9B	��B	��B	�,B	�2B	՛B	ҽB	�&B	՛B	�9B	�gB	��B	�2B	�B	ӏB	�,B	�aB	ԕB	�B	�aB	��B	��B	�aB	�gB	��B	бB	ѷB	�}B	�TB	�[B	ϫB	�[B	�[B	҉B	�TB	��B	�&B	��B	�HB	��B	�^B	�0B	�jB	�BB	͟B	�dB	�0B	��B	�0B	�HB	�zB	�,B	� B	��B	�9B	�B	��B	��B	��B	�<B	�B	�B	��B	�@B	�\B	��B	��B	�B	��B	�OB	�IB	�B	�!B	��B	�B	�OB	�VB	�\B	��B	��B	�OB	�VB	�\B	��B	�!B	��B	��B	��B	�-B	�!B	�OB	�B	��B	��B	��B	��B	��B	��B	�OB	��B	�OB	�~B	��B	��B	�4B	��B	�bB	�:B	�B	�tB	��B	�FB	�$B	��B	��B	�9B	��B	�*B	�}B	�!B	�B	�<B	�qB	��B	�9B	ȴB	��B	˒B	�B	�0B	͟B	�,B	�pB	�B	�&B
hB
�B
�B
B
YB
�B
�B
"hB
-�B
:*B
@�B
QB
TaB
W?B
VmB
]dB
\]B
^B
o5B
l�B
v�B
q�B
{�B
�B
v`B
y	B
t�B
m�B
m�B
m]B
m)B
n�B
iyB
l"B
k�B
kB
jB
j�B
jB
i�B
jB
kB
k�B
k�B
jKB
iyB
iyB
j�B
j�B
h
B
gB
f�B
ffB
e`B
e,B
e�B
e�B
gmB
l�B
lWB
m�B
kB
k�B
h>B
f�B
e�B
e`B
d�B
e,B
dZB
cTB
`BB
[�B
^5B
e,B
a|B
b�B
m)B
c�B
`�B
b�B
`�B
[�B
[WB
\�B
Z�B
\]B
YB
a�B
]dB
b�B
\)B
[�B
\)B
ZQB
Y�B
YB
XyB
Y�B
YB
V�B
_�B
k�B
a�B
`vB
_;B
a|B
`�B
bB
`BB
^�B
_B
]�B
]dB
^�B
^5B
cTB
gB
bNB
aHB
b�B
cTB
dZB
iB
g�B
f�B
f�B
f�B
e`B
d�B
dZB
d�B
f�B
d�B
k�B
gB
ffB
h
B
e,B
e�B
e`B
c B
e�B
l"B
gmB
y>B
t�B
[WB
P�B
LdB
l�B
VmB
^jB
OvB
WsB
l"B
c�B
W�B
^jB
UgB
H�B
K�B
I�B
L�B
]dB
_�B
X�B
<�B
H�B
<B
?�B
;�B
;0B
<jB
=�B
@�B
@B
=qB
<�B
;0B
8RB
:^B
=<B
D�B
:�B
8�B
9XB
>BB
8B
:�B
8RB
4�B
3hB
5B
7LB
7�B
=�B
0�B
.IB
0�B
<�B
0UB
0�B
0�B
0�B
6zB
8�B
2�B
1�B
.}B
=�B
)*B
+6B
*0B
+6B
*�B
,=B
+�B
,qB
,B
+�B
7LB
0UB
+B
3�B
B[B
=qB
.B
;0B
/B
0UB
/�B
3hB
;0B
9$B
:�B
<B
<�B
A B
?B
@B
@OB
A B
CaB
C�B
B�B
CaB
CaB
C�B
C�B
FB
H�B
N<B
N<B
O�B
JXB
L0B
L�B
M�B
S�B
W�B
OBB
[#B
P}B
K�B
J#B
L0B
HKB
I�B
J#B
JXB
GzB
G�B
H�B
H�B
G�B
GzB
G�B
E�B
EmB
C-B
I�B
A�B
O�B
Y�B
1[B
)*B
,B
 �B
&�B
+6B
7�B
3hB
*eB
)_B
,�B
-B
,=B
*eB
/�B
0!B
/�B
0!B
,B
,�B
.�B
-CB
-CB
1�B
.�B
+6B
&�B
�B
OB
�B
8�B
1'B
bB
�B
�B
qB	�B
#�B
DB
\B
B
�B
�B
B
�B

�B

	B
�B
�B
  B	�rB	��B	�ZB	�8B	�B	�	B	��B
�B	��B	��B	�B	ߤB	��B	�B	�|B	��B	�B	��B	�B	��B	��B	�-B	�zB	��B	�#B	�B	ѷB	ƨB	�B	�LB	�jB	�#B	�tB	�B	�hB	��B	�dB	��B	��B	�HB	�tB	�B	��B	��B	��B	� B	��B	��B	��B	ޞB	�B	ҽB	��B	��B	�B	��B	��B	�OB	�<B	��B	�XB	��B	��B	��B	�FB	�LB	�B	�XB	��B	�9B	��B	�\B	��B	��B	�B	��B	�B	�~B	��B	�B	�FB	�:B	��B	�AB	� B	cB	��B	��B	�B	�=B	�B	��B	w2B	|B	o B	h>B	g�B	f�B	e`B	bB	h
B	e�B	aHB	\�B	[WB	VB	N�B	XEB	XB	S[B	K�B	G�B	I�B	F�B	DgB	EmB	@�B	C�B	R B	G�B	E�B	?�B	9$B	4�B	DgB	:�B	0UB	2�B	.�B	*�B	)*B	+B	\)B	:�B	VB	B	  B	 iB��B��B��B�B�B��B�rB��B	PB	�B�B��B�B�/B��B�B��B�B�B�B��B�B�B��B�ZB�B�B�B��B�B�AB��B�`B��B��B�B��B��B�(B�(B��B	B	�B	�B	
�B	�B	DB	�B	�B	�B	�B	@B	�B	B	FB	{B	�B	�B	7B	%zB	~B	$@B	+6B	4nB	5�B	8RB	:�B	T�B	J�B	_;B	�oB	~�B	�{B	��B	��B	�B	��B	�(B	��B	��B	��B	��B	�XB	�B	�BB	�3B	�EB	��B	��B	��B	� B	�!B	�hB	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                 B	϶B	�B	�B	ςB	�MB	ςB	ςB	�MB	϶B	϶B	��B	϶B	�MB	ςB	ςB	ςB	�B	ςB	ςB	϶B	϶B	�MB	�B	�MB	��B	�B	��B	�B	ΰB	�|B	�|B	�GB	�uB	� B	��B	��B	�B	�8B	�]B	��B	��B	��B	�B	�<B	�B	�6B	��B	�mB	�pB	�NB	�ZB
B
-�B
VDB
frB
[�B
X�B
_GB
Z(B
C8B
5�B
12B
,�B
%�B
-�B
7�B
?�B
K5B
B�B
@ZB
%�B
')B
dB
�B	��B	�|B	�pB	��B	�TB	�QB	�}B	�XB	~3B	nnB	Z\B	H�B	A�B	4EB	*B��B�OB�B��B��B�VB�7B	LB	�B	B	#�B	P�B	�B	�B	��B	��B	�EB	�HB	��B	ǹB	�>B	��B	�B	�B	��B	��B	��B	��B	�B	�-B
�B
/�B
/&B
-B
,�B
+�B
+B
,�B
/�B
/�B
4�B
5�B
6QB
6�B
=�B
=|B
>�B
?�B
A,B
D>B
GQB
H"B
GQB
GQB
GQB
H�B
I�B
JcB
L;B
N�B
Q�B
[cB
^�B
aSB
d�B
gxB
k\B
r�B
t�B
{�B
�XB
��B
�nB
�tB
�?B
�tB
��B
�?B
�tB
��B
��B
�B
�B
��B
��B
�3B
�-B
��B
�aB
�3B
��B
��B
�B
��B
�B
��B
��B
�B
�B
��B
��B
��B
�RB
��B
��B
��B
�zB
�@B
}-B
|�B
z�B
z�B
z�B
y�B
y�B
x�B
xwB
w�B
w�B
v7B
u�B
u1B
s�B
p�B
l�B
hJB
gxB
f�B
deB
bYB
^uB
\4B
XPB
VDB
T8B
TlB
O�B
P�B
P�B
OMB
N�B
NGB
NGB
N|B
K5B
I�B
I(B
GQB
F�B
FB
E�B
EyB
K�B
K B
JcB
K B
K�B
J�B
I�B
H�B
GQB
H"B
E�B
D�B
FJB
EDB
C�B
B�B
BfB
A�B
A�B
CB
@�B
?B
>NB
=B
<B
;�B
:jB
:jB
8�B
5�B
5B
6�B
5�B
5�B
5KB
5B
4�B
4yB
4EB
6�B
5B
5B
3
B
2�B
0�B
/�B
0�B
1�B
/�B
/&B
/&B
.�B
/&B
.�B
.�B
.TB
.TB
. B
.TB
. B
. B
-�B
,�B
,HB
,HB
-B
,|B
+vB
+B
*�B
)5B
)5B
(�B
(dB
'�B
'�B
')B
&�B
&�B
(�B
$B
#EB
!B
�B
[B
�B
�B
�B
�B
6B
B
�B
6B
jB
�B
�B
�B
�B
�B
LB
�B
0B
�B
B
zB
EB
tB
�B
�B
B
?B
�B
9B
�B
B
�B
nB
�B
�B
[B
'B
�B
�B
!B
�B
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

�B

�B

}B

IB
	�B
	wB
�B
UB
�B
aB
�B
9B
nB
�B
nB
EB
�B
B
LB
�B
XB
RB
�B
#B
XB
EB
?B
�B
�B
�B
�B
B
�B
�B
#B
�B
�B
B
�B
�B
<B
�B
�B
�B
�B
wB
wB
wB
NB
�B
HB
�B
B
HB
�B
�B
�B
�B
wB
wB
B
�B
wB
B
}B
B
NB
�B
 B
�B
�B
UB
�B
�B
�B
&B
�B
[B
[B
�B
�B
�B
�B
 3B
 gB
 gB
 �B
 �B
 �B
!B
 �B
 �B
!9B
 �B
!9B
!B
!�B
!mB
!mB
!mB
!�B
!9B
!B
 �B
!�B
!�B
!�B
"?B
#B
#�B
#�B
#�B
$KB
$B
#�B
$B
#�B
$�B
$�B
$�B
%B
%B
%B
&�B
&�B
&�B
&�B
&WB
&�B
&WB
'�B
'^B
')B
(/B
(�B
)jB
)�B
*B
*pB
*B
*B
)�B
+BB
*�B
*�B
*pB
*�B
*�B
*�B
*pB
+vB
+�B
+�B
+�B
,HB
+�B
+�B
,�B
,|B
,HB
,HB
-�B
,�B
. B
. B
.TB
.�B
/�B
/�B
0�B
/�B
0,B
0,B
/�B
0�B
1gB
2B
2mB
3sB
2�B
2�B
2�B
3
B
3�B
3�B
4B
5�B
6B
6B
7#B
6B
6�B
6QB
6�B
6�B
7#B
7�B
7�B
7�B
8)B
7�B
8�B
8�B
9/B
9�B
9�B
:B
:jB
:jB
:jB
:�B
:jB
:�B
;;B
;pB
<AB
<AB
<AB
<vB
<vB
=B
>NB
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@ZB
@�B
A`B
A,B
A�B
A�B
A`B
A�B
B�B
C8B
C�B
C�B
C�B
C�B
C�B
D�B
EB
D�B
D�B
D�B
E�B
EyB
FB
FJB
FJB
FJB
FB
F�B
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G�B
H"B
H"B
H"B
HWB
H�B
H�B
H�B
H�B
HWB
I]B
I]B
I(B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
JcB
JcB
J/B
J�B
J�B
J�B
K�B
K�B
L;B
L;B
L�B
L�B
L�B
MB
MB
MB
MB
M�B
MuB
NB
N|B
N�B
OB
N�B
N�B
N�B
O�B
O�B
O�B
PB
PB
P�B
PSB
P�B
QZB
QZB
QZB
QZB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R+B
R+B
R`B
S1B
SfB
T8B
S�B
S�B
TB
TB
S�B
S�B
T�B
T�B
U>B
U>B
UrB
U�B
U�B
UrB
UrB
U�B
VB
U�B
U�B
U�B
VDB
V�B
WB
WJB
WB
V�B
WB
V�B
V�B
WJB
WJB
V�B
V�B
V�B
W~B
WB
WJB
WJB
WJB
W~B
W�B
W�B
XB
XB
XB
W�B
X�B
X�B
X�B
YVB
Y�B
Z\B
Z�B
Z�B
[�B
[�B
[�B
[�B
\ B
\4B
\�B
\�B
\�B
]oB
]�B
]�B
^�B
^�B
_GB
`B
`�B
`�B
aSB
a�B
aSB
aB
`�B
aSB
aSB
a�B
a�B
a�B
b%B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
d1B
d1B
d1B
c�B
c�B
deB
d�B
d�B
d�B
e7B
e�B
elB
e�B
e�B
e�B
f	B
f	B
f=B
frB
gB
gxB
hB
h~B
i�B
i�B
iPB
i�B
i�B
j!B
i�B
j!B
i�B
iPB
i�B
j�B
j�B
jVB
j!B
jVB
j�B
k(B
j�B
j�B
l.B
k�B
lbB
lbB
l�B
l�B
l�B
m4B
mhB
m�B
n�B
o�B
o@B
oB
oB
oB
oB
o@B
ouB
o�B
o�B
pB
o�B
p�B
p{B
p�B
p�B
qB
q�B
q�B
q�B
rSB
rSB
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t*B
t_B
t�B
t�B
t�B
t�B
ueB
ueB
u�B
u�B
u�B
u�B
v�B
vkB
v�B
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
w�B
xwB
yB
yB
yB
yB
yIB
yIB
yB
yIB
yIB
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{UB
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}-B
}-B
}-B
}-B
}bB
~3B
~3B
~hB
~hB
~�B
~�B
B
B
:B
nB
�B
�B
�B
�B
�@B
�@B
�B
�@B
��B
��B
��B
��B
�B
�FB
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
�B
�B
�B
�B
�RB
��B
�RB
��B
��B
��B
��B
��B
�$B
�XB
��B
�*B
�^B
�^B
��B
��B
�0B
��B	��B	�SB	�B	ͪB	�GB	�B	ςB	�B	ςB	�B	�B	�MB	�B	�B	ΰB	϶B	��B	϶B	ΰB	ΰB	ЈB	�B	ΰB	�B	�B	�|B	�GB	��B	϶B	�|B	�MB	��B	�B	ΰB	ΰB	�B	�SB	ΰB	��B	��B	�SB	�GB	ΰB	�B	�B	�|B	�GB	϶B	�SB	�MB	�|B	��B	�SB	ЈB	ΰB	ΰB	�MB	�SB	��B	ΰB	ΰB	��B	�SB	϶B	�B	϶B	��B	ςB	�B	ͪB	϶B	�SB	�MB	�|B	�MB	ЈB	��B	�GB	�B	��B	�SB	ΰB	ΰB	��B	��B	�B	�|B	��B	мB	�B	�GB	��B	�B	мB	�B	ΰB	�SB	��B	�MB	��B	��B	ςB	�B	��B	�GB	�B	�B	�B	�B	�B	��B	��B	ΰB	�GB	�MB	ΰB	��B	�GB	�GB	�B	�B	�|B	�|B	�MB	��B	�MB	�B	�|B	ςB	ςB	�|B	�B	��B	϶B	�B	ΰB	��B	϶B	�|B	��B	��B	��B	��B	ΰB	�uB	��B	��B	�MB	�oB	��B	�MB	��B	�B	ΰB	��B	϶B	�AB	��B	�B	�GB	϶B	�B	˞B	ɑB	�B	�B	ςB	�cB	�iB	�/B	�B	�B	�]B	�B	�B	�;B	�B	ɑB	��B	ɑB	��B	ɑB	�B	��B	�B	��B	�QB	�B	��B	ħB	��B	��B	�,B	��B	��B	�yB	��B	��B	��B	��B	�ZB	��B	��B	��B	�dB	��B	�B	�pB	�dB	��B	��B	�B	��B	��B	��B	�<B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�pB	��B	�6B	�<B	��B	��B	��B	�B	��B	�pB	��B	��B	��B	��B	�jB	�B	�6B	�B	�0B	��B	��B	��B	��B	�B	��B	��B	�&B	�[B	��B	��B	��B	�yB	��B	�WB	��B	�/B	��B	��B	��B	�#B	��B	��B	�fB	ħB	�DB	ƳB	��B	�QB	��B	�"B	�B	��B
B
�B
eB
�B
B
RB
�B
B
'^B
3�B
:jB
J�B
NB
P�B
PB
WB
VB
W�B
h�B
f�B
p{B
k\B
ueB
{�B
pB
r�B
n�B
g�B
gxB
gB
f�B
hJB
c+B
e�B
elB
d�B
c�B
d�B
c�B
c�B
d1B
d�B
e�B
e7B
c�B
c+B
c+B
d�B
deB
a�B
`�B
`MB
`B
_B
^�B
_{B
_�B
aB
f�B
f	B
gCB
d�B
e7B
a�B
`MB
_{B
_B
^�B
^�B
^B
]B
Y�B
U>B
W�B
^�B
[.B
\4B
f�B
]:B
Z�B
\iB
Z�B
U�B
U	B
VxB
T8B
VB
S1B
[cB
WB
\iB
U�B
UrB
U�B
TB
S�B
R�B
R+B
SfB
R�B
PSB
YVB
elB
[�B
Z(B
X�B
[.B
Z\B
[�B
Y�B
XPB
X�B
WJB
WB
XPB
W�B
]B
`�B
\ B
Z�B
\4B
]B
^B
b�B
a�B
`MB
`MB
`MB
_B
^uB
^B
^uB
`�B
^�B
e7B
`�B
`B
a�B
^�B
_{B
_B
\�B
_�B
e�B
aB
r�B
nnB
U	B
JcB
FB
f=B
PB
XB
I(B
Q%B
e�B
]oB
QZB
XB
OB
BfB
EyB
C�B
FB
WB
Y�B
R`B
6�B
B2B
5�B
9cB
5KB
4�B
6B
7WB
:jB
9�B
7#B
6QB
4�B
2B
4B
6�B
>�B
4EB
2mB
3
B
7�B
1�B
4�B
2B
.�B
-B
.�B
0�B
1�B
7WB
*<B
'�B
*pB
6�B
*B
*�B
*<B
*�B
0,B
2�B
,|B
+�B
(/B
7WB
"�B
$�B
#�B
$�B
$KB
%�B
%QB
&#B
%�B
%�B
0�B
*B
$�B
-NB
<B
7#B
'�B
4�B
(�B
*B
)�B
-B
4�B
2�B
4yB
5�B
6QB
:�B
8�B
9�B
:B
:�B
=B
=HB
<�B
=B
=B
=HB
=|B
?�B
B2B
G�B
G�B
I�B
D
B
E�B
FB
GQB
M�B
QZB
H�B
T�B
J/B
EDB
C�B
E�B
A�B
C�B
C�B
D
B
A,B
A`B
BfB
BfB
A�B
A,B
A`B
?TB
?B
<�B
C8B
;;B
I�B
S�B
+B
"�B
%�B
BB
 3B
$�B
1gB
-B
$B
#B
&WB
&�B
%�B
$B
)�B
)�B
)jB
)�B
%�B
&�B
(dB
&�B
&�B
+vB
(dB
$�B
 3B
jB
B
<B
2mB
*�B

B	�:B

}B
#B	�1B
�B
�B
	B
�B	�nB
dB	��B
 @B
XB
�B
 �B
zB	��B	�$B	�7B	�B	��B	�CB	�B	�B
^B	�nB	�LB	�_B	�VB	�xB	�\B	�.B	ͪB	϶B	ɑB	�AB	��B	ħB	��B	�,B	��B	��B	��B	�iB	�ZB	��B	��B	�B	��B	�&B	��B	�B	��B	�B	��B	�`B	��B	�&B	��B	�8B	�jB	�KB	��B	��B	��B	��B	�PB	�GB	�oB	�mB	�2B	��B	̤B	�5B	�B	��B	��B	�
B	��B	�pB	�dB	��B	��B	��B	�
B	�HB	��B	��B	�B	�EB	��B	��B	��B	��B	�0B	�[B	��B	��B	��B	~�B	{�B	y�B	yB	nB	:B	yIB	��B	|�B	|�B	p�B	u�B	h�B	a�B	a�B	`�B	_B	[�B	a�B	_GB	Z�B	VDB	U	B	O�B	HWB	Q�B	Q�B	MB	E�B	A�B	C8B	@ZB	>B	?B	:�B	=�B	K�B	A�B	?TB	9cB	2�B	.TB	>B	4yB	*B	,|B	(dB	$�B	"�B	$�B	U�B	4�B	B��B��B�B��B��B��B��B��B�7B�$B�B	B		CB�PB�B�CB��B�B��B�B�CB��B��B�B�CB��BݣB�B�iB�cB��BڑB�iB��B�B�B�LB�B��B�B�YB��B��B��B��B	 �B�:B	XB	LB	�B	�B		CB		wB	�B	�B	�B	�B	�B	-B	�B	�B	�B	,B	0B	�B	$�B	. B	/�B	2B	4EB	N|B	DsB	X�B	{!B	xwB	}-B	{UB	��B	��B	�B	��B	�gB	�^B	�NB	��B	�
B	��B	��B	��B	��B	��B	�|B	�>B	��B	��B	�B	�pB	�EB	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230601100058                            20230601100058AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023060110005820230601100058  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023060110005820230601100058QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023060110005820230601100058QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               