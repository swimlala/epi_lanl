CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-01-26T14:16:47Z creation; 2023-04-26T19:14:27Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20190126141647  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               &   &AA  AOAO7316_008644_038                 7316_008644_038                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @آ��Q�@آ��Q�11  @آ�2a|@آ�2a|@*0Zp��U@*0Zp��U�c�=Ć�.�c�=Ć�.11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?��H@:�H@�  @�  @��R@޸RA   A  A ��A,��A@  A^�RA�  A�Q�A�  A�Q�A�Q�AУ�A�Q�A�\)A��B  B  B  B   B(  B0  B8(�B@  BH(�BPQ�BX(�B`  Bh  Bo�
Bw�
B�
B��B��B�  B�{B�{B�  B�{B�{B�{B�  B��B�  B�  B�  B�33B�B�{B�
=B��B��
B�  B�  B�  B�  B��B�  B�  B�  B��B��B�  B��C  C  C  C  C	��C  C  C��C��C��C  C
=C  C  C  C   C"  C$  C%��C(  C)��C,  C.
=C0  C1��C3��C6  C8
=C:  C<  C>  C@  CB  CC��CE��CH  CJ
=CL  CN
=CP  CQ��CT  CV
=CW��CY��C\  C^
=C`{Cb{Cd{Cf  Cg��Ci��Cl  Cn
=Cp
=Cr
=Ct
=Cv  Cx  Cz
=C|{C}��C��C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C�C���C�  C�  C�C�C���C�  C�C�C�  C�  C�  C�C�
=C�  C���C�  C�  C�  C�C���C���C���C���C�  C�C�C���C���C���C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�C�  C�  C�  C���C�  C�C�  C�  C�C�C�  C�  C�  C�  C���C���C���C���C�  C�C�  C���C�C�C���C�  C�C�C���C���C�  C�C�  C���C���C�  C�  C�  C���C�  C���C���C�  C�C�C�C�  C���C�  C�C�C�  D   D }qD  D�D  D}qD��D� D�D� D  D� D  D��DD��D  D� D	�D	�D
�D
� D
�qD}qD�qD� D�D��D�D�D  D}qD�qD}qD  D� D�qD}qDD�D�qD}qD  D��D�D��D  D� D  D� D�qD}qD�qD� D  D}qD�qD� D  D� D�qDz�D�qD� D �D ��D!  D!z�D!�qD"� D#  D#}qD#�qD$� D%  D%� D&  D&� D'D'� D'��D(� D)D)� D)�qD*� D+�D+��D,�D,��D-  D-z�D-��D.}qD/  D/��D0  D0}qD1�D1��D2  D2}qD2��D3� D4  D4� D4�qD5}qD6  D6}qD7  D7� D7�qD8}qD9  D9� D9��D:� D;�D;� D;�qD<� D=D=��D>  D>��D?D?��D@  D@� DA  DA� DB  DB}qDB�qDC� DD  DD� DE  DE}qDE��DF}qDF�qDG��DH�DH}qDI  DI��DJ  DJ}qDK�DK��DL  DLz�DM  DM��DN  DN}qDO�DO� DO�qDP��DQ�DQ��DR  DR� DR�qDS� DT  DTz�DU  DU��DV  DV}qDW  DW��DX  DX}qDX��DYz�DY��DZ}qD[�D[� D\  D\}qD\�qD]}qD]�qD^z�D^�qD_��D`�D`�D`�qDaz�Da�qDb� DcDc� Dc�qDd��De  De� Df�Df��Dg  Dg��Dh  Dh� Dh�qDiz�Di�qDj� Dj�qDk}qDl  Dl� Dm  Dm� Dn  Dn� Dn�qDo� Dp�Dp��Dq  Dq� Dr�Dr}qDr�qDs� Dt  Dt}qDt�qDu� Dv  Dv��Dw  Dw��Dx  Dx� Dy�Dy� Dz�Dz��D{  D{}qD|  D|� D}  D}��D~  D~� D�D� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD��HD�HD�@ D�� D��HD�HD�B�D��HD���D�HD�AHD�� D��HD�HD�>�D�~�D�� D�HD�@ D��HD�� D���D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D��HD�D�HD�@ D�� D���D���D�>�D�~�D���D���D�>�D��HD�� D�  D�@ D�� D��HD�HD�@ D�� D���D��qD�>�D�~�D���D�  D�>�D�~�D�� D�  D�>�D�}qD�� D���D�=qD�� D��HD�HD�>�D�}qD���D���D�>�D�}qD���D�  D�>�D�� D�� D��qD�@ D��HD��HD�HD�AHD�~�D��qD��qD�>�D�� D���D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�}qD���D�  D�>�D�� D��HD�  D�>�D�}qD���D�  D�@ D�~�D�� D�HD�@ D�}qD���D�  D�AHD�� D��qD���D�AHD�� D�� D���D�>�D�� D��HD�HD�@ D�� D���D��qD�=qD�� D��HD�HD�@ D�~�D�� D��D�@ D�~�D��qD�  D�AHD�� D�� D�  D�AHD���D��HD���D�@ D��HD�� D�  D�>�D�}qD���D�HD�@ D�� D�� D�HD�B�D���D�D�HD�AHD�� D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�  D�=qD�}qD�� D�HD�AHD���D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�HD�>�D�� D�� D��qD�@ D���D�� D��qD�>�D�~�D�� D�  D�AHD�~�D���D�  D�>�D�~�D�� D�  D�AHD�� D���D�  D�AHD�~�D��qD���D�AHD�� D���D�HD�@ D�� D���D��qD�@ D��HD�� D���D�@ D D¾�D�HD�B�DÁHD�� D�  D�@ DĀ DĽqD��qD�@ Dŀ D�� D�  D�@ DƁHDƾ�D���D�AHDǁHDǾ�D�  D�@ DȀ D��HD���D�@ Dɀ Dɾ�D�  D�B�Dʀ D�� D���D�@ DˁHD�� D���D�@ D̂�D�D��D�AHD́HD�� D�  D�>�D΀ D�D�HD�AHDρHD��HD�HD�AHDЁHD��HD�  D�@ Dт�D��HD�  D�>�D�~�D�� D�HD�AHDӀ DӽqD�  D�AHDԀ D�� D�  D�@ DՀ D��HD�HD�@ Dր D��HD���D�AHDׁHD׾�D��qD�@ D؀ D�� D�  D�>�D�~�DٽqD�  D�>�Dڀ D�� D���D�>�Dۀ D�� D�  D�AHD܁HD�� D�  D�@ D݁HD�� D���D�AHDށHD��HD�  D�@ D߀ D�� D�HD�@ D�� D��HD���D�@ D�HD�� D�  D�>�D� D�� D���D�>�D� D�D�HD�AHD�HD��HD�HD�AHD�HD�� D���D�>�D�HD��HD�HD�@ D�~�D�� D�HD�@ D� D�� D���D�AHD�HD�� D���D�AHD� D�� D�  D�>�D�~�D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�AHD�HDD��qD�>�D�~�D��HD�  D�>�D�� D�� D�  D�>�D�~�D�qD���D�AHD� D�� D��qD�>�D�}qD�� D�HD�AHD�HD��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�@ D��HD���D��qD�>�D�� D��HD���D�>�D�~�D�� D��D�(�>�?#�
?k�?���?�Q�?�(�@�\@��@(��@@  @W
=@k�@}p�@��@�@��R@��@�
=@�  @˅@�Q�@�G�@���@�Q�AG�AA(�A�AffA��A!�A'
=A,(�A1�A7
=A<��AC33AHQ�AL��AR�\AXQ�A\��Ab�\AhQ�Amp�Aq�Aw�A}p�A�G�A��
A�ffA���A��A�ffA���A�33A�ffA���A�33A�ffA���A�33A�{A���A��A�A���A��
A�{A���A��
A�ffA���A��
AƸRA���A��
A�
=A�G�A�(�A�\)Aٙ�A�(�A�
=A��A�(�A�RA陚A�(�A�ffA�G�A�(�A�ffA�G�A�(�A�ffB z�B�B\)Bz�BB33Bz�B	��B
=BQ�BG�B�RB  B�BffB�
B��B{B�B��BB
=BQ�Bp�B�\B   B!G�B"ffB#�B%�B&=qB'�B(��B*ffB+�B,��B.ffB/�B0z�B1�B3\)B4z�B5��B6�HB8Q�B9G�B:�\B;�
B=�B>=qB?\)B@��BB{BC
=BDQ�BEBG
=BH(�BIp�BJ�HBL  BMG�BN�HBP  BQ�BR�\BS�
BT��BU�BW\)BXz�BYp�BZ�RB[�
B\��B]�B_
=B`  B`��Bb{Bc\)BdQ�Be�Bf=qBg�Bhz�Bip�Bj�\Bk�
Bl��Bm��Bn�HBp  Bp��Bq�Bs33BtQ�Bu�Bv=qBw�Bx��By��B{
=B|(�B}�B~=qB�B�ffB��HB�\)B�(�B��RB�G�B�B�ffB�
=B��B�{B���B�G�B�B�Q�B��HB���B�{B���B�G�B��B�Q�B���B���B�(�B���B�G�B�  B�z�B�
=B��B�Q�B���B�p�B�{B��RB�G�B�B�ffB��B��B�=qB���B��B�(�B���B�G�B��B��\B�
=B��B�ffB���B�p�B�{B���B�G�B�B�z�B�
=B���B�(�B���B�p�B��B�z�B��B��B�=qB��HB��B�{B��\B�G�B��
B�Q�B���B���B�(�B���B��B��
B�ffB���B�G�B��B�z�B���B�p�B�  B��\B��B��B�  B���B��B��B�{B���B�33B���B�(�B���B�33B��B�=qB���B�G�B�B�=qB���B�\)B�B�=qB��RB�G�B��B�{B£�B��B�p�B��B�ffB��HB��B�p�B�B�(�B�Q�B�ffBƏ\BƸRB��HB���Bƣ�Bƣ�Bƣ�B�z�B�Q�B�Q�B�Q�B�=qB�  B��B�  B��B�Bř�BŅBŅB�p�B�33B�33B�33B�
=B��HB���B���BĸRBď\B�ffB�ffB�ffB�=qB�{B�  B�  B��
BîBÙ�BÙ�BÅB�\)B�33B��B��B�
=B���B£�B£�B\B�ffB�=qB�(�B�(�B�  B��
B��B��B���B�p�B�G�B�33B�33B��B���B���B���B���B��\B�ffB�ffB�ffB�=qB�{B�  B��B�  B��
B��B���B��B��B�p�B�\)B�33B��B��B�
=B���B���B��RB���B���B��\B�ffB�ffB�ffB�Q�B�(�B�(�B�(�B�{B��B�  B�  B��B��
B�B�B��
B��
B��
B��
B�B��B�  B�{B�{B�{B�=qB�z�B��\B��\B��RB��HB��B�G�B�p�B���B��
B�(�B�ffB��\B���B��B��B��B�(�B�ffB¸RB��BÅB��B�=qB�z�B���B�33BŮB��B�=qBƣ�B�
=B�p�B��B�=qBȣ�B�
=B�p�B��
B�Q�BʸRB�
=B�p�B�B�=qḄ�B�
=B�p�B�B�{B�z�B��HB�\)B�B�{B�ffB���B�G�Bљ�B�{B�ffBҸRB�
=B�p�B��
B�=qBԣ�B���B�G�Bՙ�B��B�Q�B���B��BׅB�B�(�B�z�B��HB�\)BٮB�{B�ffBڸRB��B�p�B��B�Q�B܏\B���B�G�BݮB�(�Bޏ\B��HB�33B߅B��B�ffB���B�33B�B��
B�(�B��B�
=B�p�B��
B�Q�B��B�
=B�p�B��
B�=qB��B��B癚B�{B�z�B���B�33B陚B�  B�ffB��HB�\)B��
B�Q�B�RB��B�B�  B�ffB���B�\)B��
B�Q�B�RB�33B�B�{B�z�B��HB�G�B�B�=qB��RB�33B���B�{B��\B���B�p�B��B�ffB���B�\)B��
B�ffB���B��B�  B��\B�
=B��B�{B��\B�33B�C (�C p�C C
=CG�C�\C�
C�C\)C�C  CG�C�\C�HC(�Cp�C�C  CQ�C��C��C=qC�\C�HC33C�C��C�CffC�RC	  C	G�C	��C	�HC
(�C
z�C
C{C\)C�C��CQ�C��C�C=qC�C�
C(�Cp�CC{CffC�RC  CQ�C��C�C=qC�C�
C(�Cp�CC
=CQ�C��C�C=qC�C�HC33Cz�C��C�Cp�CC{CffC�RC
=C\)C��C  CQ�C��C�C=qC�C�
C�Cp�CC{C\)C�C  CG�C�\C�HC33Cz�C��C{C\)C��C��C =qC �\C ��C!{C!\)C!�C!�C"=qC"�C"��C#{C#\)C#��C#�C$33C$z�C$�RC%
=C%Q�C%��C%�HC&(�C&z�C&C'{C'\)C'��C'�C(33C(z�C(C)
=C)\)C)��C)�C*=qC*�C*�
C+�C+ffC+�RC,
=C,Q�C,��C,�C-33C-z�C-��C.{C.\)C.�C.��C/=qC/�C/�
C0(�C0p�C0C1{C1ffC1�RC2
=C2ffC2�C3
=C3\)C3�C4  C4G�C4��C4�C533C5z�C5C6�C6p�C6C7{C7ffC7C8{C8ffC8�RC9  C9Q�C9��C9�C:=qC:�\C:�HC;33C;�\C;�HC<(�C<z�C<C={C=ffC=�C=��C>G�C>��C>�C?=qC?�C?�HC@(�C@z�C@��CA(�CAp�CA��CB�CBp�CBCC
=CCffCC�CD  CD\)CD�CE
=CE\)CE�CF  CFG�CF��CF�CG33CG�CG��CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    ?��?��H@:�H@�  @�  @��R@޸RA   A  A ��A,��A@  A^�RA�  A�Q�A�  A�Q�A�Q�AУ�A�Q�A�\)A��B  B  B  B   B(  B0  B8(�B@  BH(�BPQ�BX(�B`  Bh  Bo�
Bw�
B�
B��B��B�  B�{B�{B�  B�{B�{B�{B�  B��B�  B�  B�  B�33B�B�{B�
=B��B��
B�  B�  B�  B�  B��B�  B�  B�  B��B��B�  B��C  C  C  C  C	��C  C  C��C��C��C  C
=C  C  C  C   C"  C$  C%��C(  C)��C,  C.
=C0  C1��C3��C6  C8
=C:  C<  C>  C@  CB  CC��CE��CH  CJ
=CL  CN
=CP  CQ��CT  CV
=CW��CY��C\  C^
=C`{Cb{Cd{Cf  Cg��Ci��Cl  Cn
=Cp
=Cr
=Ct
=Cv  Cx  Cz
=C|{C}��C��C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C�C���C�  C�  C�C�C���C�  C�C�C�  C�  C�  C�C�
=C�  C���C�  C�  C�  C�C���C���C���C���C�  C�C�C���C���C���C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�C�  C�  C�  C���C�  C�C�  C�  C�C�C�  C�  C�  C�  C���C���C���C���C�  C�C�  C���C�C�C���C�  C�C�C���C���C�  C�C�  C���C���C�  C�  C�  C���C�  C���C���C�  C�C�C�C�  C���C�  C�C�C�  D   D }qD  D�D  D}qD��D� D�D� D  D� D  D��DD��D  D� D	�D	�D
�D
� D
�qD}qD�qD� D�D��D�D�D  D}qD�qD}qD  D� D�qD}qDD�D�qD}qD  D��D�D��D  D� D  D� D�qD}qD�qD� D  D}qD�qD� D  D� D�qDz�D�qD� D �D ��D!  D!z�D!�qD"� D#  D#}qD#�qD$� D%  D%� D&  D&� D'D'� D'��D(� D)D)� D)�qD*� D+�D+��D,�D,��D-  D-z�D-��D.}qD/  D/��D0  D0}qD1�D1��D2  D2}qD2��D3� D4  D4� D4�qD5}qD6  D6}qD7  D7� D7�qD8}qD9  D9� D9��D:� D;�D;� D;�qD<� D=D=��D>  D>��D?D?��D@  D@� DA  DA� DB  DB}qDB�qDC� DD  DD� DE  DE}qDE��DF}qDF�qDG��DH�DH}qDI  DI��DJ  DJ}qDK�DK��DL  DLz�DM  DM��DN  DN}qDO�DO� DO�qDP��DQ�DQ��DR  DR� DR�qDS� DT  DTz�DU  DU��DV  DV}qDW  DW��DX  DX}qDX��DYz�DY��DZ}qD[�D[� D\  D\}qD\�qD]}qD]�qD^z�D^�qD_��D`�D`�D`�qDaz�Da�qDb� DcDc� Dc�qDd��De  De� Df�Df��Dg  Dg��Dh  Dh� Dh�qDiz�Di�qDj� Dj�qDk}qDl  Dl� Dm  Dm� Dn  Dn� Dn�qDo� Dp�Dp��Dq  Dq� Dr�Dr}qDr�qDs� Dt  Dt}qDt�qDu� Dv  Dv��Dw  Dw��Dx  Dx� Dy�Dy� Dz�Dz��D{  D{}qD|  D|� D}  D}��D~  D~� D�D� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD��HD�HD�@ D�� D��HD�HD�B�D��HD���D�HD�AHD�� D��HD�HD�>�D�~�D�� D�HD�@ D��HD�� D���D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D��HD�D�HD�@ D�� D���D���D�>�D�~�D���D���D�>�D��HD�� D�  D�@ D�� D��HD�HD�@ D�� D���D��qD�>�D�~�D���D�  D�>�D�~�D�� D�  D�>�D�}qD�� D���D�=qD�� D��HD�HD�>�D�}qD���D���D�>�D�}qD���D�  D�>�D�� D�� D��qD�@ D��HD��HD�HD�AHD�~�D��qD��qD�>�D�� D���D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�}qD���D�  D�>�D�� D��HD�  D�>�D�}qD���D�  D�@ D�~�D�� D�HD�@ D�}qD���D�  D�AHD�� D��qD���D�AHD�� D�� D���D�>�D�� D��HD�HD�@ D�� D���D��qD�=qD�� D��HD�HD�@ D�~�D�� D��D�@ D�~�D��qD�  D�AHD�� D�� D�  D�AHD���D��HD���D�@ D��HD�� D�  D�>�D�}qD���D�HD�@ D�� D�� D�HD�B�D���D�D�HD�AHD�� D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�  D�=qD�}qD�� D�HD�AHD���D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�HD�>�D�� D�� D��qD�@ D���D�� D��qD�>�D�~�D�� D�  D�AHD�~�D���D�  D�>�D�~�D�� D�  D�AHD�� D���D�  D�AHD�~�D��qD���D�AHD�� D���D�HD�@ D�� D���D��qD�@ D��HD�� D���D�@ D D¾�D�HD�B�DÁHD�� D�  D�@ DĀ DĽqD��qD�@ Dŀ D�� D�  D�@ DƁHDƾ�D���D�AHDǁHDǾ�D�  D�@ DȀ D��HD���D�@ Dɀ Dɾ�D�  D�B�Dʀ D�� D���D�@ DˁHD�� D���D�@ D̂�D�D��D�AHD́HD�� D�  D�>�D΀ D�D�HD�AHDρHD��HD�HD�AHDЁHD��HD�  D�@ Dт�D��HD�  D�>�D�~�D�� D�HD�AHDӀ DӽqD�  D�AHDԀ D�� D�  D�@ DՀ D��HD�HD�@ Dր D��HD���D�AHDׁHD׾�D��qD�@ D؀ D�� D�  D�>�D�~�DٽqD�  D�>�Dڀ D�� D���D�>�Dۀ D�� D�  D�AHD܁HD�� D�  D�@ D݁HD�� D���D�AHDށHD��HD�  D�@ D߀ D�� D�HD�@ D�� D��HD���D�@ D�HD�� D�  D�>�D� D�� D���D�>�D� D�D�HD�AHD�HD��HD�HD�AHD�HD�� D���D�>�D�HD��HD�HD�@ D�~�D�� D�HD�@ D� D�� D���D�AHD�HD�� D���D�AHD� D�� D�  D�>�D�~�D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�AHD�HDD��qD�>�D�~�D��HD�  D�>�D�� D�� D�  D�>�D�~�D�qD���D�AHD� D�� D��qD�>�D�}qD�� D�HD�AHD�HD��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�@ D��HD���D��qD�>�D�� D��HD���D�>�D�~�D�� D��G�O�>�?#�
?k�?���?�Q�?�(�@�\@��@(��@@  @W
=@k�@}p�@��@�@��R@��@�
=@�  @˅@�Q�@�G�@���@�Q�AG�AA(�A�AffA��A!�A'
=A,(�A1�A7
=A<��AC33AHQ�AL��AR�\AXQ�A\��Ab�\AhQ�Amp�Aq�Aw�A}p�A�G�A��
A�ffA���A��A�ffA���A�33A�ffA���A�33A�ffA���A�33A�{A���A��A�A���A��
A�{A���A��
A�ffA���A��
AƸRA���A��
A�
=A�G�A�(�A�\)Aٙ�A�(�A�
=A��A�(�A�RA陚A�(�A�ffA�G�A�(�A�ffA�G�A�(�A�ffB z�B�B\)Bz�BB33Bz�B	��B
=BQ�BG�B�RB  B�BffB�
B��B{B�B��BB
=BQ�Bp�B�\B   B!G�B"ffB#�B%�B&=qB'�B(��B*ffB+�B,��B.ffB/�B0z�B1�B3\)B4z�B5��B6�HB8Q�B9G�B:�\B;�
B=�B>=qB?\)B@��BB{BC
=BDQ�BEBG
=BH(�BIp�BJ�HBL  BMG�BN�HBP  BQ�BR�\BS�
BT��BU�BW\)BXz�BYp�BZ�RB[�
B\��B]�B_
=B`  B`��Bb{Bc\)BdQ�Be�Bf=qBg�Bhz�Bip�Bj�\Bk�
Bl��Bm��Bn�HBp  Bp��Bq�Bs33BtQ�Bu�Bv=qBw�Bx��By��B{
=B|(�B}�B~=qB�B�ffB��HB�\)B�(�B��RB�G�B�B�ffB�
=B��B�{B���B�G�B�B�Q�B��HB���B�{B���B�G�B��B�Q�B���B���B�(�B���B�G�B�  B�z�B�
=B��B�Q�B���B�p�B�{B��RB�G�B�B�ffB��B��B�=qB���B��B�(�B���B�G�B��B��\B�
=B��B�ffB���B�p�B�{B���B�G�B�B�z�B�
=B���B�(�B���B�p�B��B�z�B��B��B�=qB��HB��B�{B��\B�G�B��
B�Q�B���B���B�(�B���B��B��
B�ffB���B�G�B��B�z�B���B�p�B�  B��\B��B��B�  B���B��B��B�{B���B�33B���B�(�B���B�33B��B�=qB���B�G�B�B�=qB���B�\)B�B�=qB��RB�G�B��B�{B£�B��B�p�B��B�ffB��HB��B�p�B�B�(�B�Q�B�ffBƏ\BƸRB��HB���Bƣ�Bƣ�Bƣ�B�z�B�Q�B�Q�B�Q�B�=qB�  B��B�  B��B�Bř�BŅBŅB�p�B�33B�33B�33B�
=B��HB���B���BĸRBď\B�ffB�ffB�ffB�=qB�{B�  B�  B��
BîBÙ�BÙ�BÅB�\)B�33B��B��B�
=B���B£�B£�B\B�ffB�=qB�(�B�(�B�  B��
B��B��B���B�p�B�G�B�33B�33B��B���B���B���B���B��\B�ffB�ffB�ffB�=qB�{B�  B��B�  B��
B��B���B��B��B�p�B�\)B�33B��B��B�
=B���B���B��RB���B���B��\B�ffB�ffB�ffB�Q�B�(�B�(�B�(�B�{B��B�  B�  B��B��
B�B�B��
B��
B��
B��
B�B��B�  B�{B�{B�{B�=qB�z�B��\B��\B��RB��HB��B�G�B�p�B���B��
B�(�B�ffB��\B���B��B��B��B�(�B�ffB¸RB��BÅB��B�=qB�z�B���B�33BŮB��B�=qBƣ�B�
=B�p�B��B�=qBȣ�B�
=B�p�B��
B�Q�BʸRB�
=B�p�B�B�=qḄ�B�
=B�p�B�B�{B�z�B��HB�\)B�B�{B�ffB���B�G�Bљ�B�{B�ffBҸRB�
=B�p�B��
B�=qBԣ�B���B�G�Bՙ�B��B�Q�B���B��BׅB�B�(�B�z�B��HB�\)BٮB�{B�ffBڸRB��B�p�B��B�Q�B܏\B���B�G�BݮB�(�Bޏ\B��HB�33B߅B��B�ffB���B�33B�B��
B�(�B��B�
=B�p�B��
B�Q�B��B�
=B�p�B��
B�=qB��B��B癚B�{B�z�B���B�33B陚B�  B�ffB��HB�\)B��
B�Q�B�RB��B�B�  B�ffB���B�\)B��
B�Q�B�RB�33B�B�{B�z�B��HB�G�B�B�=qB��RB�33B���B�{B��\B���B�p�B��B�ffB���B�\)B��
B�ffB���B��B�  B��\B�
=B��B�{B��\B�33B�C (�C p�C C
=CG�C�\C�
C�C\)C�C  CG�C�\C�HC(�Cp�C�C  CQ�C��C��C=qC�\C�HC33C�C��C�CffC�RC	  C	G�C	��C	�HC
(�C
z�C
C{C\)C�C��CQ�C��C�C=qC�C�
C(�Cp�CC{CffC�RC  CQ�C��C�C=qC�C�
C(�Cp�CC
=CQ�C��C�C=qC�C�HC33Cz�C��C�Cp�CC{CffC�RC
=C\)C��C  CQ�C��C�C=qC�C�
C�Cp�CC{C\)C�C  CG�C�\C�HC33Cz�C��C{C\)C��C��C =qC �\C ��C!{C!\)C!�C!�C"=qC"�C"��C#{C#\)C#��C#�C$33C$z�C$�RC%
=C%Q�C%��C%�HC&(�C&z�C&C'{C'\)C'��C'�C(33C(z�C(C)
=C)\)C)��C)�C*=qC*�C*�
C+�C+ffC+�RC,
=C,Q�C,��C,�C-33C-z�C-��C.{C.\)C.�C.��C/=qC/�C/�
C0(�C0p�C0C1{C1ffC1�RC2
=C2ffC2�C3
=C3\)C3�C4  C4G�C4��C4�C533C5z�C5C6�C6p�C6C7{C7ffC7C8{C8ffC8�RC9  C9Q�C9��C9�C:=qC:�\C:�HC;33C;�\C;�HC<(�C<z�C<C={C=ffC=�C=��C>G�C>��C>�C?=qC?�C?�HC@(�C@z�C@��CA(�CAp�CA��CB�CBp�CBCC
=CCffCC�CD  CD\)CD�CE
=CE\)CE�CF  CFG�CF��CF�CG33CG�CG��CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�dZA�n�A�r�A�t�A�n�A�l�A�p�A�t�A�v�A�v�A�x�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�~�A�~�AσAυAρAρAρA�~�AρA�~�AσAυAυAχ+Aχ+Aχ+Aω7AϋDAω7AϋDAχ+Aχ+Aχ+AϋDAϋDAϑhAϗ�Aϗ�Aϙ�Aϙ�Aϕ�A���Aͺ^A�-A�bNA���A�jA�p�A��A��A�VA�p�A�hsA��A�G�A�dZA�bNA�|�A�ƨA�=qA���A�A�A�XA�\)A�O�A��A���A�A���A��A��\A��A���A�O�A{��Av�HAt1An�+AjQ�Ag+Ad�!AahsA\ �AU`BASx�ARZAQVAO�TAN��AMoAJ��AIVAFbNACA@M�A<1'A;�wA;/A7C�A4-A3l�A2$�A1`BA0�/A/�A/��A/dZA/+A.E�A,ĜA+|�A)�;A'�#A&�A'K�A'��A(I�A)33A)��A)�A(�A'�mA'��A'�FA'�A'+A&�A&n�A%��A$��A$A�A#�#A#�A"ĜA"M�A!��A!��A!XA ��A �A  �A\)A�AƨA��AC�A�An�AJAhsAVA��A�\AI�A��At�A
=A��AbNA��AdZAXAG�A?}A+A�AȴAr�A�#A�wAl�A"�A�HA(�A�#Ax�A"�A�jA(�A�^A�PAx�Ax�Ap�Ap�AG�AVA�HA��AJA�FAS�A�jAI�A�AO�A/A�A��AjAbAhsA&�A%A
�A
�jA
�+A
5?A	�;A	�A	A�!A~�AffA-A�wA�hA�A`BA?}A&�A��A�+AM�A�7A��A��A�+A�AA�PAVA��A-A�A|�A ��A �A �\A ~�A ZA I�@���@�ƨ@��w@���@���@��@�t�@��@��!@�ff@�J@��^@��j@�A�@��@���@��P@�
=@���@�(�@�@��@�\@���@�@�h@�@�/@���@��D@��@��@�ȴ@�+@�$�@���@�%@�@��
@�o@ꟾ@�ff@��@�@�@��;@�;d@�
=@�n�@�?}@���@�I�@㝲@���@�\@�J@�^@�@�bN@�t�@�ȴ@ݡ�@��/@ܣ�@�(�@�l�@�;d@ڗ�@�=q@�@�`B@ج@�Q�@��;@�;d@֧�@�-@�V@��m@�33@��y@ҧ�@ҏ\@�n�@�@��@�Ĝ@�z�@�Q�@�9X@��@�S�@ΰ!@Χ�@�=q@͉7@�&�@�%@̓u@�K�@ʇ+@��#@�&�@�Ĝ@�9X@��;@�K�@Ƈ+@���@�`B@Ĵ9@�r�@�I�@�  @å�@�C�@�"�@��y@�@�{@�x�@��@��F@�;d@���@��@�~�@�J@�?}@��@�Ĝ@�bN@��m@�dZ@��@��H@��@��!@�n�@�^5@��@�?}@��9@�A�@��@�t�@�K�@��y@���@��+@��T@�x�@��`@��D@� �@�ƨ@�t�@�
=@���@���@�ff@��@���@�x�@��@�r�@��w@��@�K�@���@�n�@��@�`B@���@�I�@��;@�\)@��H@�ff@��@���@�@�?}@��@�Ĝ@�A�@�ƨ@�\)@��@��@��R@�n�@�=q@��@��@���@�O�@��/@�bN@�(�@�b@��m@��F@�|�@�@��R@��\@�V@���@���@���@��9@��u@�I�@���@���@�t�@�K�@�+@��@�
=@��y@���@���@�ff@��@��-@��@�X@�G�@�&�@���@��9@�j@�  @���@��P@��@�\)@�o@�~�@��T@���@�p�@�7L@��@���@��j@�r�@�1'@�ƨ@��P@�t�@�dZ@�@���@�5?@���@��^@�O�@�&�@���@��@�Z@�A�@�b@��w@�l�@�C�@�33@��@�@��H@���@��+@�^5@���@�@��-@���@��h@�hs@��@��`@��j@��u@��@��@���@�\)@���@��\@��@�@�hs@���@��@�A�@���@��F@�t�@�C�@�o@�V@�@�p�@�/@�%@��`@��@��@�j@�b@��@��
@�ƨ@���@�dZ@���@�v�@�$�@��T@��^@��h@�`B@�V@��/@���@��9@�I�@�ƨ@���@�;d@��@��\@�E�@���@�p�@��@�r�@�9X@� �@�b@�1@�w@�@~�R@~@}@}p�@}V@|��@|��@|j@{��@{33@{@z�H@z~�@zn�@z�@y7L@x�`@x�9@xQ�@xb@x  @w|�@w;d@v�y@vE�@u@u�@u`B@u/@t�/@tj@t(�@sƨ@sS�@r�@r�!@rn�@r=q@q��@q�^@qx�@q%@p��@pA�@o�@nv�@nE�@nE�@nE�@n5?@m�@m`B@l��@l(�@kƨ@kt�@kdZ@k33@j�H@i�@ihs@h�u@hb@g�P@gl�@gK�@g
=@f�+@fff@f@e`B@d��@d�D@dI�@d1@c��@c��@c�@b�@b��@bM�@a��@a��@aX@`��@_�;@_\)@_�@^�@]�@]p�@]V@\�@\�@\I�@[��@[ƨ@[��@[t�@Z��@Z�@Y�7@X��@XbN@X1'@X �@W�@W\)@Vȴ@V5?@U@U?}@T��@Tz�@S��@S�@R�H@R��@Rn�@R^5@R�@Q��@Qhs@Q�@P�u@Pr�@PbN@P1'@O��@O��@O|�@O;d@N�y@NV@N{@M�T@M��@M��@Mp�@Mp�@L�/@L1@K��@Kƨ@K��@K33@J��@J�\@J~�@JM�@JJ@I�7@IG�@H�`@H�9@H�u@HbN@HQ�@G�@G��@G
=@Fȴ@F��@E��@EO�@E�@D�@DZ@C�m@C��@B��@A��@@Ĝ@@�@@r�@@A�@?�@?l�@?�@>�R@>E�@=�-@=`B@=V@<��@<�D@<I�@;ƨ@;��@;dZ@;33@:�H@:�@9��@9��@97L@9%@9�@8��@8�u@81'@7�@7�P@7l�@7;d@6��@6�R@6�+@6ff@6@5�@4��@4�@4Z@3�m@3��@3dZ@3S�@333@3"�@3o@2�@2��@2=q@1�@1�^@1X@1%@0Ĝ@0bN@0  @/;d@.��@.v�@.ff@.@-�-@-�@-O�@-?}@,�@,�D@,Z@+�
@+dZ@*�\@*�@)�@)��@)��@)X@)X@)G�@)&�@)%@(��@(�u@(Q�@(b@'�;@'��@'�P@'l�@'�@&�R@&��@&��@&ff@&$�@&{@&@%�T@%��@%@%�h@%?}@$��@$�/@$��@$�@$z�@$(�@$1@#��@#�
@#�F@#t�@#33@"�H@"~�@"^5@"J@!�7@!�@ Ĝ@ �@ 1'@��@�P@|�@\)@�@�+@V@@�h@/@�@�D@Z@9X@�@�
@��@�@�H@~�@^5@=q@�@�^@��@hs@&�@��@��@�9@�@Q�@1'@  @��@K�@;d@;d@;d@;d@+@�@
=@�y@�+@�+@v�@E�@$�@@�T@@�-@�h@p�@`B@`B@O�@V@�/@�@j@�
@t�@dZ@S�@"�@@�@��@�!@n�@^5@=q@-@J@�@��@�^@��@hs@��@�`@Ĝ@�u@�@r�@bN@Q�@ �@�@�@�@�;@�;@��@��@��@�w@�P@�P@�P@|�@\)@K�@;d@�@�@ȴ@�RA�O�A�XA�`BA�ffA�^5A�bNA�ffA�ffA�ffA�r�A�r�A�p�A�r�A�t�A�r�A�r�A�v�A�r�A�hsA�l�A�n�A�l�A�l�A�t�A�n�A�r�A�x�A�r�A�t�A�x�A�x�A�r�A�v�A�x�A�t�A�x�A�z�A�v�A�v�A�z�A�x�A�v�A�x�A�z�A�v�A�v�A�|�A�x�A�v�A�z�A�x�A�x�A�z�A�x�A�v�A�z�A�z�A�v�A�x�A�z�A�v�A�v�A�z�A�z�A�v�A�x�A�|�A�x�A�v�A�z�A�z�A�x�A�x�A�|�A�z�A�x�A�~�A�z�A�x�A�|�A�z�A�v�A�z�A�~�A�z�A�x�A�|�A�~�A�z�A�|�AρA�|�A�z�A�|�A�|�A�x�A�|�A�~�A�|�A�z�A�~�AρA�z�AρAρA�z�A�|�AρA�~�A�z�A�~�AρA�~�AυAχ+AρAρAσAσAρAσAχ+AυAσAχ+AυA�|�AρAσAρA�~�AσAσA�~�A�~�AσA�~�A�|�AρAσAρAρAσAσAρA�z�A�|�AσAρA�|�AρAσAρA�~�AσA�|�A�z�A�~�A�~�A�|�A�|�AσAσA�|�AσAυAρAρAχ+AσAσAχ+AυAρAσAχ+Aχ+AσAσAχ+Aχ+AυAυAχ+Aχ+AσAχ+Aω7Aχ+AυAω7Aω7AυAσAχ+Aω7AυAυAω7Aω7AυAω7AϋDAχ+AυAω7AϋDAω7AυAω7AϋDAϋDAχ+Aω7AϋDAϋDAω7Aω7AύPAϋDAχ+Aω7AύPAϋDAυAω7Aω7AυAσAχ+AύPAϋDAύPAϑhAϋDAυAχ+Aω7Aχ+AσAσAχ+Aχ+AσAυAω7AϋDAυAσAχ+Aχ+AυAχ+Aω7Aχ+AύPAω7AύPAω7Aχ+Aχ+Aω7Aχ+AσAω7AϑhAϑhAύPAϕ�AϓuAϏ\Aω7AύPAϗ�Aϕ�Aϗ�Aϙ�Aϕ�AϓuAϙ�Aϙ�Aϗ�Aϕ�Aϙ�Aϛ�Aϗ�Aϕ�Aϙ�Aϛ�Aϗ�Aϕ�Aϙ�Aϛ�Aϛ�Aϗ�Aϗ�Aϙ�Aϛ�Aϗ�Aϗ�Aϙ�Aϛ�Aϙ�Aϗ�Aϛ�Aϙ�Aϗ�Aϙ�Aϛ�Aϛ�Aϗ�Aϗ�Aϙ�Aϛ�Aϙ�Aϗ�Aϗ�Aϝ�Aϛ�Aϗ�Aϛ�Aϝ�Aϛ�Aϙ�Aϛ�Aϝ�Aϛ�Aϗ�Aϙ�Aϛ�Aϙ�Aϕ�Aϕ�Aϙ�Aϗ�AϓuAύPAϑhAϑhAϏ\AϋDAύPAϏ\AϏ\AϏ\AϏ\AϑhAϕ�AϓuAϑhAϓuAϗ�Aϗ�Aϕ�Aϕ�Aϗ�Aϗ�AϓuAϓuAϗ�Aϗ�Aϕ�AϓuAϕ�Aϗ�Aϕ�Aϕ�Aϕ�Aϗ�Aϛ�Aϗ�Aϕ�Aϗ�Aϛ�Aϙ�Aϕ�AϓuAϙ�Aϙ�Aϕ�AϓuAϗ�Aϙ�Aϗ�Aϕ�Aϕ�Aϗ�Aϙ�Aϕ�AϓuAϕ�Aϙ�Aϗ�AϓuAϓuAϗ�Aϗ�AϓuAϏ\AϑhAϕ�Aϕ�AϑhAϓuAϗ�Aϗ�AϑhAϑhAϑhAϕ�Aϕ�AϓuAϑhAϕ�Aϕ�Aϕ�AϓuAϑhAϏ\AϓuAϕ�AϑhAύPAύPAϑhAϓuAϏ\AϑhAϓuAϓuAϓuAϏ\AϏ\Aϕ�AϓuAύPAϑhAϓuAϓuAϓuAϑhAϓuAϕ�Aϗ�Aϕ�AϓuAϑhAϓuAϗ�Aϗ�AϓuAϏ\Aω7A�`BA�-A��A�z�Aͥ�A�bNA�VA̧�A�O�A�7LA�oA˓uA�{Aʴ9A�7LA�|�A�O�A�/A�ffA��;A�^5A�ȴAŧ�A���Aĕ�A�"�AøRAÑhA�x�A�C�A�A��;A�ĜA�bNA���A�S�A���A��A��A��FA��+A��+A�hsA�dZA�XA�^5A�S�A�O�A�A�A�C�A�A�A�=qA�VA�|�A��A�|�A�=qA�"�A�  A��mA��!A���A��PA��7A�~�A�r�A�l�A�\)A�Q�A�33A�-A�-A��A�A��#A���A�t�A�^5A�9XA��A��A�ĜA�\)A�&�A�bA���A��#A��FA��hA�p�A�O�A�7LA� �A���A��/A���A���A�C�A���A���A�K�A��A���A���A���A�r�A�9XA�%A��HA��#A��
A���A���A�ȴA��uA�v�A�bA���A�E�A��A��A��DA�ffA�;dA���A�ȴA��A��A�O�A�"�A���A��
A��-A��uA�`BA�7LA�"�A�bA���A���A��9A���A��PA��A�r�A�`BA�M�A�5?A�1A���A���A�`BA��A��jA�O�A�  A��!A�r�A�33A���A�^5A��\A�7LA��mA��\A��A��+A��`A��!A��A�Q�A��A��`A�z�A�{A��uA��yA�XA��A���A�Q�A���A�|�A�^5A�oA��TA���A�`BA��9A�VA�Q�A���A��DA��A���A���A�|�A�7LA��A�ĜA���A��uA�z�A�K�A�/A�(�A�-A�/A�&�A�
=A�ƨA�=qA�l�A�x�A��RA���A��+A�VA��7A�1'A�VA��HA��A�v�A��A�A�ffA�-A���A��9A��uA�`BA�A�S�A�9XA�ƨA��mA�XA�I�A���A�A�A��A��/A�M�A��
A��A��FA��7A�oA�x�A��A�A���A��A���A��DA��A�|�A�t�A�ffA�?}A� �A��HA��A��hA�I�A�VA��/A���A��RA���A�z�A�=qA�+A��A��DA�oAS�A~n�A}A}l�A}VA|^5A{dZAz�Ax�AxM�AwƨAw/Av��Av�DAvM�AvAux�AuAt�AtQ�As�As�Ar1'Ap�`Apr�Aox�An^5AmƨAm33Al�`AlM�Ak��Ak7LAjM�Aip�AiAh��Ah-AhAg��Ag�Af��Af{Ae�#Ae��Ae�AeC�Ad�RAd{Ac�PAcx�Ab��Ab�\Aa�hA`��A`ZA`A_�hA^��A^A�A]l�AY�^AWAV�HAV$�AU�-AUx�AT��ATr�ATbASAS��AS��ASt�AS?}AS"�AR�yAR�RAR��ARn�AR(�AQ�AQ�-AQdZAQK�AQ&�AP�AP�jAP��AP��AP9XAO��AO�
AOl�AOK�AO/AO+AO�AN��AN�ANr�AM�mAM��AM�AMO�AL��ALz�AL1'AKAK�7AK/AJ�AJ1AI��AI��AI\)AH��AH�`AH�HAH��AHZAGp�AF5?AE�
AE�
AE�^AE�PAE�AC�PAB��ABM�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    A�`BA�dZA�n�A�r�A�t�A�n�A�l�A�p�A�t�A�v�A�v�A�x�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�~�A�~�AσAυAρAρAρA�~�AρA�~�AσAυAυAχ+Aχ+Aχ+Aω7AϋDAω7AϋDAχ+Aχ+Aχ+AϋDAϋDAϑhAϗ�Aϗ�Aϙ�Aϙ�Aϕ�A���Aͺ^A�-A�bNA���A�jA�p�A��A��A�VA�p�A�hsA��A�G�A�dZA�bNA�|�A�ƨA�=qA���A�A�A�XA�\)A�O�A��A���A�A���A��A��\A��A���A�O�A{��Av�HAt1An�+AjQ�Ag+Ad�!AahsA\ �AU`BASx�ARZAQVAO�TAN��AMoAJ��AIVAFbNACA@M�A<1'A;�wA;/A7C�A4-A3l�A2$�A1`BA0�/A/�A/��A/dZA/+A.E�A,ĜA+|�A)�;A'�#A&�A'K�A'��A(I�A)33A)��A)�A(�A'�mA'��A'�FA'�A'+A&�A&n�A%��A$��A$A�A#�#A#�A"ĜA"M�A!��A!��A!XA ��A �A  �A\)A�AƨA��AC�A�An�AJAhsAVA��A�\AI�A��At�A
=A��AbNA��AdZAXAG�A?}A+A�AȴAr�A�#A�wAl�A"�A�HA(�A�#Ax�A"�A�jA(�A�^A�PAx�Ax�Ap�Ap�AG�AVA�HA��AJA�FAS�A�jAI�A�AO�A/A�A��AjAbAhsA&�A%A
�A
�jA
�+A
5?A	�;A	�A	A�!A~�AffA-A�wA�hA�A`BA?}A&�A��A�+AM�A�7A��A��A�+A�AA�PAVA��A-A�A|�A ��A �A �\A ~�A ZA I�@���@�ƨ@��w@���@���@��@�t�@��@��!@�ff@�J@��^@��j@�A�@��@���@��P@�
=@���@�(�@�@��@�\@���@�@�h@�@�/@���@��D@��@��@�ȴ@�+@�$�@���@�%@�@��
@�o@ꟾ@�ff@��@�@�@��;@�;d@�
=@�n�@�?}@���@�I�@㝲@���@�\@�J@�^@�@�bN@�t�@�ȴ@ݡ�@��/@ܣ�@�(�@�l�@�;d@ڗ�@�=q@�@�`B@ج@�Q�@��;@�;d@֧�@�-@�V@��m@�33@��y@ҧ�@ҏ\@�n�@�@��@�Ĝ@�z�@�Q�@�9X@��@�S�@ΰ!@Χ�@�=q@͉7@�&�@�%@̓u@�K�@ʇ+@��#@�&�@�Ĝ@�9X@��;@�K�@Ƈ+@���@�`B@Ĵ9@�r�@�I�@�  @å�@�C�@�"�@��y@�@�{@�x�@��@��F@�;d@���@��@�~�@�J@�?}@��@�Ĝ@�bN@��m@�dZ@��@��H@��@��!@�n�@�^5@��@�?}@��9@�A�@��@�t�@�K�@��y@���@��+@��T@�x�@��`@��D@� �@�ƨ@�t�@�
=@���@���@�ff@��@���@�x�@��@�r�@��w@��@�K�@���@�n�@��@�`B@���@�I�@��;@�\)@��H@�ff@��@���@�@�?}@��@�Ĝ@�A�@�ƨ@�\)@��@��@��R@�n�@�=q@��@��@���@�O�@��/@�bN@�(�@�b@��m@��F@�|�@�@��R@��\@�V@���@���@���@��9@��u@�I�@���@���@�t�@�K�@�+@��@�
=@��y@���@���@�ff@��@��-@��@�X@�G�@�&�@���@��9@�j@�  @���@��P@��@�\)@�o@�~�@��T@���@�p�@�7L@��@���@��j@�r�@�1'@�ƨ@��P@�t�@�dZ@�@���@�5?@���@��^@�O�@�&�@���@��@�Z@�A�@�b@��w@�l�@�C�@�33@��@�@��H@���@��+@�^5@���@�@��-@���@��h@�hs@��@��`@��j@��u@��@��@���@�\)@���@��\@��@�@�hs@���@��@�A�@���@��F@�t�@�C�@�o@�V@�@�p�@�/@�%@��`@��@��@�j@�b@��@��
@�ƨ@���@�dZ@���@�v�@�$�@��T@��^@��h@�`B@�V@��/@���@��9@�I�@�ƨ@���@�;d@��@��\@�E�@���@�p�@��@�r�@�9X@� �@�b@�1@�w@�@~�R@~@}@}p�@}V@|��@|��@|j@{��@{33@{@z�H@z~�@zn�@z�@y7L@x�`@x�9@xQ�@xb@x  @w|�@w;d@v�y@vE�@u@u�@u`B@u/@t�/@tj@t(�@sƨ@sS�@r�@r�!@rn�@r=q@q��@q�^@qx�@q%@p��@pA�@o�@nv�@nE�@nE�@nE�@n5?@m�@m`B@l��@l(�@kƨ@kt�@kdZ@k33@j�H@i�@ihs@h�u@hb@g�P@gl�@gK�@g
=@f�+@fff@f@e`B@d��@d�D@dI�@d1@c��@c��@c�@b�@b��@bM�@a��@a��@aX@`��@_�;@_\)@_�@^�@]�@]p�@]V@\�@\�@\I�@[��@[ƨ@[��@[t�@Z��@Z�@Y�7@X��@XbN@X1'@X �@W�@W\)@Vȴ@V5?@U@U?}@T��@Tz�@S��@S�@R�H@R��@Rn�@R^5@R�@Q��@Qhs@Q�@P�u@Pr�@PbN@P1'@O��@O��@O|�@O;d@N�y@NV@N{@M�T@M��@M��@Mp�@Mp�@L�/@L1@K��@Kƨ@K��@K33@J��@J�\@J~�@JM�@JJ@I�7@IG�@H�`@H�9@H�u@HbN@HQ�@G�@G��@G
=@Fȴ@F��@E��@EO�@E�@D�@DZ@C�m@C��@B��@A��@@Ĝ@@�@@r�@@A�@?�@?l�@?�@>�R@>E�@=�-@=`B@=V@<��@<�D@<I�@;ƨ@;��@;dZ@;33@:�H@:�@9��@9��@97L@9%@9�@8��@8�u@81'@7�@7�P@7l�@7;d@6��@6�R@6�+@6ff@6@5�@4��@4�@4Z@3�m@3��@3dZ@3S�@333@3"�@3o@2�@2��@2=q@1�@1�^@1X@1%@0Ĝ@0bN@0  @/;d@.��@.v�@.ff@.@-�-@-�@-O�@-?}@,�@,�D@,Z@+�
@+dZ@*�\@*�@)�@)��@)��@)X@)X@)G�@)&�@)%@(��@(�u@(Q�@(b@'�;@'��@'�P@'l�@'�@&�R@&��@&��@&ff@&$�@&{@&@%�T@%��@%@%�h@%?}@$��@$�/@$��@$�@$z�@$(�@$1@#��@#�
@#�F@#t�@#33@"�H@"~�@"^5@"J@!�7@!�@ Ĝ@ �@ 1'@��@�P@|�@\)@�@�+@V@@�h@/@�@�D@Z@9X@�@�
@��@�@�H@~�@^5@=q@�@�^@��@hs@&�@��@��@�9@�@Q�@1'@  @��@K�@;d@;d@;d@;d@+@�@
=@�y@�+@�+@v�@E�@$�@@�T@@�-@�h@p�@`B@`B@O�@V@�/@�@j@�
@t�@dZ@S�@"�@@�@��@�!@n�@^5@=q@-@J@�@��@�^@��@hs@��@�`@Ĝ@�u@�@r�@bN@Q�@ �@�@�@�@�;@�;@��@��@��@�w@�P@�P@�P@|�@\)@K�@;d@�@�@ȴG�O�A�O�A�XA�`BA�ffA�^5A�bNA�ffA�ffA�ffA�r�A�r�A�p�A�r�A�t�A�r�A�r�A�v�A�r�A�hsA�l�A�n�A�l�A�l�A�t�A�n�A�r�A�x�A�r�A�t�A�x�A�x�A�r�A�v�A�x�A�t�A�x�A�z�A�v�A�v�A�z�A�x�A�v�A�x�A�z�A�v�A�v�A�|�A�x�A�v�A�z�A�x�A�x�A�z�A�x�A�v�A�z�A�z�A�v�A�x�A�z�A�v�A�v�A�z�A�z�A�v�A�x�A�|�A�x�A�v�A�z�A�z�A�x�A�x�A�|�A�z�A�x�A�~�A�z�A�x�A�|�A�z�A�v�A�z�A�~�A�z�A�x�A�|�A�~�A�z�A�|�AρA�|�A�z�A�|�A�|�A�x�A�|�A�~�A�|�A�z�A�~�AρA�z�AρAρA�z�A�|�AρA�~�A�z�A�~�AρA�~�AυAχ+AρAρAσAσAρAσAχ+AυAσAχ+AυA�|�AρAσAρA�~�AσAσA�~�A�~�AσA�~�A�|�AρAσAρAρAσAσAρA�z�A�|�AσAρA�|�AρAσAρA�~�AσA�|�A�z�A�~�A�~�A�|�A�|�AσAσA�|�AσAυAρAρAχ+AσAσAχ+AυAρAσAχ+Aχ+AσAσAχ+Aχ+AυAυAχ+Aχ+AσAχ+Aω7Aχ+AυAω7Aω7AυAσAχ+Aω7AυAυAω7Aω7AυAω7AϋDAχ+AυAω7AϋDAω7AυAω7AϋDAϋDAχ+Aω7AϋDAϋDAω7Aω7AύPAϋDAχ+Aω7AύPAϋDAυAω7Aω7AυAσAχ+AύPAϋDAύPAϑhAϋDAυAχ+Aω7Aχ+AσAσAχ+Aχ+AσAυAω7AϋDAυAσAχ+Aχ+AυAχ+Aω7Aχ+AύPAω7AύPAω7Aχ+Aχ+Aω7Aχ+AσAω7AϑhAϑhAύPAϕ�AϓuAϏ\Aω7AύPAϗ�Aϕ�Aϗ�Aϙ�Aϕ�AϓuAϙ�Aϙ�Aϗ�Aϕ�Aϙ�Aϛ�Aϗ�Aϕ�Aϙ�Aϛ�Aϗ�Aϕ�Aϙ�Aϛ�Aϛ�Aϗ�Aϗ�Aϙ�Aϛ�Aϗ�Aϗ�Aϙ�Aϛ�Aϙ�Aϗ�Aϛ�Aϙ�Aϗ�Aϙ�Aϛ�Aϛ�Aϗ�Aϗ�Aϙ�Aϛ�Aϙ�Aϗ�Aϗ�Aϝ�Aϛ�Aϗ�Aϛ�Aϝ�Aϛ�Aϙ�Aϛ�Aϝ�Aϛ�Aϗ�Aϙ�Aϛ�Aϙ�Aϕ�Aϕ�Aϙ�Aϗ�AϓuAύPAϑhAϑhAϏ\AϋDAύPAϏ\AϏ\AϏ\AϏ\AϑhAϕ�AϓuAϑhAϓuAϗ�Aϗ�Aϕ�Aϕ�Aϗ�Aϗ�AϓuAϓuAϗ�Aϗ�Aϕ�AϓuAϕ�Aϗ�Aϕ�Aϕ�Aϕ�Aϗ�Aϛ�Aϗ�Aϕ�Aϗ�Aϛ�Aϙ�Aϕ�AϓuAϙ�Aϙ�Aϕ�AϓuAϗ�Aϙ�Aϗ�Aϕ�Aϕ�Aϗ�Aϙ�Aϕ�AϓuAϕ�Aϙ�Aϗ�AϓuAϓuAϗ�Aϗ�AϓuAϏ\AϑhAϕ�Aϕ�AϑhAϓuAϗ�Aϗ�AϑhAϑhAϑhAϕ�Aϕ�AϓuAϑhAϕ�Aϕ�Aϕ�AϓuAϑhAϏ\AϓuAϕ�AϑhAύPAύPAϑhAϓuAϏ\AϑhAϓuAϓuAϓuAϏ\AϏ\Aϕ�AϓuAύPAϑhAϓuAϓuAϓuAϑhAϓuAϕ�Aϗ�Aϕ�AϓuAϑhAϓuAϗ�Aϗ�AϓuAϏ\Aω7A�`BA�-A��A�z�Aͥ�A�bNA�VA̧�A�O�A�7LA�oA˓uA�{Aʴ9A�7LA�|�A�O�A�/A�ffA��;A�^5A�ȴAŧ�A���Aĕ�A�"�AøRAÑhA�x�A�C�A�A��;A�ĜA�bNA���A�S�A���A��A��A��FA��+A��+A�hsA�dZA�XA�^5A�S�A�O�A�A�A�C�A�A�A�=qA�VA�|�A��A�|�A�=qA�"�A�  A��mA��!A���A��PA��7A�~�A�r�A�l�A�\)A�Q�A�33A�-A�-A��A�A��#A���A�t�A�^5A�9XA��A��A�ĜA�\)A�&�A�bA���A��#A��FA��hA�p�A�O�A�7LA� �A���A��/A���A���A�C�A���A���A�K�A��A���A���A���A�r�A�9XA�%A��HA��#A��
A���A���A�ȴA��uA�v�A�bA���A�E�A��A��A��DA�ffA�;dA���A�ȴA��A��A�O�A�"�A���A��
A��-A��uA�`BA�7LA�"�A�bA���A���A��9A���A��PA��A�r�A�`BA�M�A�5?A�1A���A���A�`BA��A��jA�O�A�  A��!A�r�A�33A���A�^5A��\A�7LA��mA��\A��A��+A��`A��!A��A�Q�A��A��`A�z�A�{A��uA��yA�XA��A���A�Q�A���A�|�A�^5A�oA��TA���A�`BA��9A�VA�Q�A���A��DA��A���A���A�|�A�7LA��A�ĜA���A��uA�z�A�K�A�/A�(�A�-A�/A�&�A�
=A�ƨA�=qA�l�A�x�A��RA���A��+A�VA��7A�1'A�VA��HA��A�v�A��A�A�ffA�-A���A��9A��uA�`BA�A�S�A�9XA�ƨA��mA�XA�I�A���A�A�A��A��/A�M�A��
A��A��FA��7A�oA�x�A��A�A���A��A���A��DA��A�|�A�t�A�ffA�?}A� �A��HA��A��hA�I�A�VA��/A���A��RA���A�z�A�=qA�+A��A��DA�oAS�A~n�A}A}l�A}VA|^5A{dZAz�Ax�AxM�AwƨAw/Av��Av�DAvM�AvAux�AuAt�AtQ�As�As�Ar1'Ap�`Apr�Aox�An^5AmƨAm33Al�`AlM�Ak��Ak7LAjM�Aip�AiAh��Ah-AhAg��Ag�Af��Af{Ae�#Ae��Ae�AeC�Ad�RAd{Ac�PAcx�Ab��Ab�\Aa�hA`��A`ZA`A_�hA^��A^A�A]l�AY�^AWAV�HAV$�AU�-AUx�AT��ATr�ATbASAS��AS��ASt�AS?}AS"�AR�yAR�RAR��ARn�AR(�AQ�AQ�-AQdZAQK�AQ&�AP�AP�jAP��AP��AP9XAO��AO�
AOl�AOK�AO/AO+AO�AN��AN�ANr�AM�mAM��AM�AMO�AL��ALz�AL1'AKAK�7AK/AJ�AJ1AI��AI��AI\)AH��AH�`AH�HAH��AHZAGp�AF5?AE�
AE�
AE�^AE�PAE�AC�PAB��ABM�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BGzBG�BG�BHBG�BHBHBG�BHBH�BHKBHKBHKBHBHKBH�BHKBHKBHKBHKBH�BHKBHKBH�BH�BHKBH�BHKBHKBHKBH�BHBHKBHKBH�BHKBHKBHKBHBHBHKBHBHBHBHKBHBHBH�BH�BH�BH�BH�BH�B�B�B�QB�+B��B�B��B�BBoB�B_BB�B�B��B�9B��B�{BU�B
rB
�B
�ZB
��B
zDB
@OB
'B	�B	ÖB	�$B	��B	�OB	�eB	�qB	��B	.B	o�B	^5B	MjB	@�B	49B	1�B	�B	B�cB��B�`B�oB�"B�8B�5BܒB�B�QB�?BԕB՛B��B	FB	1B	49B	CaB	Q�B	jKB	r�B	y	B	�;B	�B	��B	�$B	�=B	�oB	��B	�B	�OB	�0B	��B	�B	��B	�2B	��B	��B	��B
�B
�B
~B
:B
FB
=B
#B
$�B
)�B
,=B
/B
/�B
2-B
2�B
4nB
4�B
7�B
9�B
9XB
8�B
8�B
8�B
8RB
5�B
5?B
6�B
7B
6�B
7�B
8B
7�B
6zB
;�B
;�B
>�B
>�B
=�B
=�B
=�B
=�B
?�B
B�B
B[B
C�B
B[B
CaB
D�B
D�B
D�B
C-B
@�B
@�B
?�B
@OB
A�B
A�B
B'B
B�B
B�B
B�B
DgB
HKB
I�B
HKB
IRB
GzB
HKB
HB
G�B
F?B
GB
DgB
C�B
C-B
C�B
B'B
B�B
B�B
A�B
A�B
AUB
A�B
A B
A�B
@�B
@�B
@�B
?}B
?B
>�B
>�B
>BB
=�B
=qB
=qB
<�B
<jB
<jB
;0B
:�B
<B
8RB
8B
7�B
6�B
5?B
4nB
49B
2aB
1�B
1'B
0!B
/�B
/B
-�B
-�B
-CB
,�B
,�B
+�B
+6B
*�B
+6B
*0B
)*B
'�B
'�B
'�B
'�B
'RB
'�B
'B
'B
&�B
&�B
&LB
'RB
%�B
$B
#�B
#�B
#�B
"�B
"hB
"�B
#B
"hB
!�B
!�B
!bB
 �B
 \B
�B
�B
�B
B
�B
qB
�B
B
qB
�B
	B
�B
�B
eB
B
�B
�B
�B
�B
_B
$B
�B
SB
�B
�B
B
FB
�B
uB
:B
:B
�B
4B
 B
�B
.B
4B
hB
 B
 B
 B
�B
 B
B
�B
4B
�B
�B
bB
�B
 B
�B
�B
�B
(B
�B
(B
\B
�B
"B
VB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
"B
PB
�B
�B
B
�B
PB
B
B
~B
�B
~B
~B
�B
�B
VB
�B
�B
B
�B
"B
�B
�B
"B
VB
�B
(B
�B
�B
�B
�B
�B
VB
(B
(B
\B
�B
�B
\B
�B
�B
�B
.B
bB
bB
�B
hB
�B
hB
�B
�B
�B
oB
�B
B
�B
@B
�B
�B
�B
�B
B
{B
FB
{B
MB
B
�B
�B
B
SB
�B
�B
B
�B
YB
�B
YB
+B
+B
�B
�B
�B
1B
�B
1B
eB
1B
�B
B
�B
�B
�B
�B
�B
	B
=B
�B
�B
qB
�B
�B
xB
IB
�B
�B
B
IB
�B
B
~B
B
B
IB
IB
IB
B
IB
�B
�B
B
B
B
OB
OB
�B
OB
!B
�B
�B
�B
�B
�B
 'B
 �B
!-B
!bB
!bB
!-B
 �B
!�B
!bB
!bB
!�B
"4B
!�B
"hB
"�B
"�B
#nB
#B
#�B
#�B
#�B
$@B
$tB
$�B
$�B
$�B
%B
%B
%FB
%zB
%zB
%�B
%�B
&B
&�B
&�B
'�B
'RB
'B
'RB
'B
'RB
'�B
'�B
'�B
'�B
'�B
)*B
)*B
)�B
*�B
*�B
+kB
+�B
,�B
-B
-B
-wB
.B
.IB
.}B
.�B
.}B
0UB
0�B
0�B
0�B
1'B
1'B
1�B
1�B
1�B
2aB
2-B
2-B
1�B
1�B
1�B
3hB
3�B
49B
4nB
4nB
4�B
4nB
4�B
5B
4�B
4�B
5�B
5�B
6B
6FB
6�B
6FB
5�B
5�B
6�B
7LB
8B
7�B
7�B
7�B
7�B
7�B
8B
8B
8�B
8�B
8�B
9$B
8�B
8�B
9XB
:*B
:^B
:�B
:�B
:^B
:^B
:�B
:�B
;0B
;dB
;�B
;�B
<B
<6B
<6B
<jB
=B
=qB
=qB
=�B
=�B
>B
>BB
>wB
>�B
?B
>�B
?B
?B
?HB
?}B
?}B
?}B
?�B
?�B
@OB
A B
A�B
AUB
A B
A B
A B
A B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C-B
C-B
C�B
D3B
D�B
D�B
D�B
DgB
D�B
EB
D�B
EB
E�B
E�B
E�B
FB
F?B
E�B
E�B
F�B
F�B
F�B
GB
GEB
GB
GzB
HKB
H�B
IB
H�B
I�B
J#B
J�B
J�B
J�B
J�B
K)B
J�B
K^B
J�B
K)B
K�B
L0B
L�B
MB
M6B
M6B
MB
M6B
M�B
M�B
N<B
N�B
N�B
OB
OB
OvB
PB
PB
PHB
P}B
PHB
P}B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R B
RTB
RTB
R�B
S&B
R�B
R�B
R�B
S[B
S&B
R�B
S�B
T�B
S�B
TaB
T,B
T�B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
VB
VB
VB
V9B
U�B
V�B
VmB
W?B
V�B
W
B
XB
XB
W�B
W�B
X�B
XyB
XEB
Y�B
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
[�B
\]B
\)B
\�B
\�B
]/B
]dB
]�B
]�B
^5B
^B
^5B
^5B
^�B
_�B
_pB
_�B
`vB
`BB
`BB
`�B
aHB
aB
a|B
a|B
aB
aHB
a�B
a�B
a�B
a�B
b�B
b�B
c B
c B
c�B
c�B
d&B
dZB
d&B
d&B
dZB
dZB
dZB
d�B
e,B
e,B
d�B
e�B
e�B
f2B
f2B
ffB
gB
g�B
gmB
g�B
g�B
h>B
h
B
h�B
h>B
iB
h�B
h�B
iDB
iyB
j�B
j�B
kB
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
m)B
m)B
l�B
m�B
m�B
n/B
m�B
ncB
n�B
n�B
n�B
o B
o B
o5B
o5B
pB
poB
p�B
poB
p;B
p�B
q�B
q�B
qvB
q�B
q�B
rGB
r|B
r�B
sB
sB
r�B
s�B
tB
tB
t�B
tTB
t�B
t�B
tTB
t�B
u%B
u�B
u�B
v`B
w2B
v�B
v�B
wfB
w�B
w�B
w�B
w�B
x8B
xB
x�B
y	B
yrB
y	B
y�B
zB
zB
zxB
zxB
z�B
z�B
{B
{B
{�B
{�B
|B
|�B
}"B
}VB
}VB
}"B
}"B
}VB
}"B
}"B
}�B
~�B
~(B
~�B
~�B
.B
.B
cB
cB
�B
�B
� B
�B
�B
�B
� B
�iB
� B
�iB
�;B
�oB
�oB
�oB
��B
�oB
�B
��B
�AB
�AB
�uB
��B
��B
�B
�B
�B
�B
�B
��B
�{B
�{B
�{B
��B
�B
��B
��B
�MB
��B
�B
��B
��B
�B
��B
�MB
��B
��B
��B
�SB
��B
�SB
��B
�SB
��B
��B
�SB
�YB
�YB
�%BI�BDgBIRBEBI�BGEBF�BIBH�BG�BGzBH�BG�BGzBH�BH�BGBIBH�BG�BGBI�BF�BFtBHKBIBF�BH�BG�BG�BG�BI�BH�BG�BIBHKBGEBH�BIBGBHKBIRBHBGBIBH�BGEBHBIRBGzBHKBH�BGEBHKBIBG�BG�BIBH�BG�BH�BH�BGEBG�BIRBHKBGEBH�BIRBG�BG�BIBH�BGEBHBIRBGzBH�BIBG�BHBIRBH�BF�BHBIBHBGEBHKBI�BGBIBIRBHKBGzBIRBHKBGzBHBI�BGzBHBH�BG�BG�BI�BH�BGzBHKBJ#BHBG�BH�BH�BGzBI�BI�BG�BH�BH�BH�BG�BH�BH�BG�BHBI�BI�BGzBG�BI�BHBG�BH�BIBG�BH�BI�BHKBG�BHBI�BH�BGEBH�BIBH�BGEBHKBIBG�BGzBH�BIBG�BH�BI�BG�BGEBH�BH�BGBG�BIRBHBGzBH�BH�BGzBHKBH�BGzBG�BIBH�BG�BGEBIRBH�BG�BG�BH�BIRBGEBHBIBG�BG�BHKBH�BG�BGzBIRBIRBG�BGEBH�BH�BG�BHBIBG�BGEBH�BI�BG�BGBHBH�BHKBGBG�BIRBHBGzBG�BH�BH�BGBHKBIRBH�BGBHKBH�BGzBGEBI�BIBGEBGBIBHBGBIBIBGzBGBH�BH�BH�BGzBGzBH�BHBHKBGEBH�BH�BG�BF�BG�BH�BJ#BG�BH�BI�BG�BFBHKBG�BGBGzBIBG�BGEBI�BG�BHKBGEBJ�BH�BF�BH�BIRBH�BG�BIBI�BHBG�BH�BI�BG�BG�BIRBI�BH�BG�BIBI�BH�BHKBHKBI�BI�BH�BG�BI�BI�BH�BHKBIRBI�BHBIBJ�BI�BH�BI�BK^BK)BJXBI�BJ�BK�BK�BI�BJ#BK^BJ�BI�BJXBK^BJ#BI�BJ�BL0BK�BK)BK�BNBM�BL0BL�BN�BQNBOvBO�BPBR BQNBP�BP�BP�BQNBO�BNBOvBPBOBBM6BM6BM�BM�BL�BMjBN�BN�BMjBL�BNpBN�BNpBM6BM6BM�BN<BMBK)BL�BM6BL0BJ�BK�BL�BN<BK�BK�BMBNBL�BJ�BL0BMjBMBL0BK�BMjBN<BM6BK�BK�BMjBM�BL�BL�BN<BOBBNpBMjBMjBNpBM�BK�BK�BNBN�BNpBM�BLdBMjBM�BL0BK�BK�BM6BM�BNpBL�BK�BM�BOBBOBL�BL0BMBL�BK�BK�BK)BM6BMBJ�BK)BLdBK)BI�BI�BI�BJ�BIBG�BGBGBG�BG�BFBDgBC�BCaBD3BDgBPHB`�By	B��B�DB�B&LBT�Bp�Bv�B�B��B�`B	B	=<B	�B	�xB	�+B	�&B
fB
$B
<�B
�(B
�B
ΥB
�B
�>B
�B �B~B{B_B�B4�BF?B`By�B��B��B�hB��B�{B��B�+B�=B�B��B�YB�B��B��B��B�'B��B˒B�NB�tB�tB��BʌBΥB�pB�B��B�jB��B̘B̘B�B��B�vB�BΥBѷB՛B�WBخB��B�QBںB�B�HB�AB�KB�B�B�8B�B�]B�B��B��B�B�B�B�GB��B iBB�BBJBB{BB�B$B�B�B�B�B�B�B\B�B�B#nB!�B=B+BOB�B+BkB=B�B@B$B1B7BBeBYB�B�BuBhBbB�B�B@B B�BVB"BB
rB
rB~B�B�B%B�BVB	7B��B�B�.B��B�B�B�B��B��B��B��B��B�8B҉BѷB��B�zB��B��B��B��B�UB�tB��B��B�CB�YB�%B��B�1Bw�Bs�B��B�_BsBGEB4B6�B�B�B�B�BVB�B
��BoB
�"B
��B
�	B
��B
�B
�B
�B
�B
��B
�B
�B�B
�?B
�B
�2B
�$B
�'B
�@B
��B
бB
��B
��B
iB
NpB
R�B
K^B
C-B
D�B
;�B
;0B
9�B
LdB
)�B
%�B
7B
/�B
B
B
u%B	��B	��B
 �B	ٴB	�B	ΥB	�UB	�RB	�B	�}B	��B	�^B	�qB	��B	�0B	��B	�*B	�RB	��B	�B	��B	��B	ÖB	��B	��B	�B	��B	�-B	��B	��B	�B	��B	�B	�FB	��B	��B	�qB	�nB	��B	�~B	��B	�B	��B	�0B	��B	��B	��B	��B	��B	�MB	�B	}�B	�B	�B	�iB	��B	|�B	y	B	t�B	��B	�B	h�B	o�B	q�B	b�B	g�B	c B	f�B	_B	XB	f2B	W�B	XB	T�B	TaB	K�B	NB	M�B	l�B	I�B	?�B	9XB	>�B	MjB	@�B	=<B	7B	3�B	5�B	3�B	7LB	C�B	+�B	-�B	1�B	*�B	!�B	(XB	�uB	oB	�B	!�B	DB	�B	 B	JB	
rB	�B	�B	AB	MB	�B	 iB	�B	�B�]B��B	;B�B��B��B�B��B�DB�B�TB��B�B�B�B�B��B�5B�)B�/B��B�iB��B��B��B�B�B�B�B�]B�QB� B��B�&B�+B��B�9B�B�B��B��B��B�vB��B��BӏB��B�[B�&B�WB�"B��BҽG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    BGEBGzBG�BHBHBHBG�BG�BG�BH�BH1BHKBHKBHBHKBH�BHKBHKBHKBHKBH�BHKBHKBHfBH�BHfBH�BHKBHKBHKBH�BG�BHKBHKBH�BHKBHKBHKBHBHBHKBH1BHBHBH1BHBG�BHfBH�BH�BH�BIRBQ B�B�B	DB��B��B�FB�TB�FBPB)B&LB 'B�B�B�B
XB��B�YB�~Bd�BhB  B<B
�7B
�DB
R:B
A B	��B	��B	��B	ªB	�XB	�^B	�B	�HB	�HB	}�B	h�B	VB	LdB	F�B	F?B	
B	B	�B��B�DB��B�B��B�B�B�B�B�KB��B�TB	�B	
B	�B	6�B	ESB	UB	kkB	shB	z*B	��B	�B	�QB	��B	�B	�MB	��B	��B	��B	�RB	��B	��B	�iB	��B	�>B	�DB	�]B
�B
	B
B
{B
YB
�B
$�B
'8B
+QB
-�B
0UB
1'B
33B
4B
5�B
6�B
:xB
>wB
:*B
9rB
:*B
9�B
:B
72B
7�B
7�B
8B
7�B
8�B
9�B
9	B
8B
="B
<�B
AB
?�B
>B
=�B
=�B
>B
@�B
C-B
C�B
E�B
B�B
D�B
E�B
E�B
F�B
DMB
A�B
BAB
A;B
B[B
CB
B[B
BuB
B�B
B�B
CB
EB
IB
J=B
IRB
K^B
H�B
I�B
J#B
IlB
IB
G�B
D�B
DB
DMB
ESB
C�B
D�B
C{B
BuB
A�B
BB
B[B
B[B
B�B
BAB
B[B
BB
@4B
?}B
?�B
@OB
>�B
>(B
=�B
=�B
=<B
=�B
=�B
<6B
=�B
>wB
9$B
8�B
8�B
88B
6+B
6FB
5�B
4B
2�B
2�B
1�B
0�B
/�B
./B
./B
-�B
-�B
-B
+�B
+�B
,�B
.B
-B
*KB
(XB
(>B
(�B
(>B
)B
(�B
'mB
'�B
'mB
'�B
(XB
*B
&�B
$�B
$�B
%,B
$B
"�B
"�B
#nB
#�B
"�B
"�B
#�B
!�B
!B
!B
 �B
 �B
�B
�B
 B
]B
IB
�B
�B
�B
CB
�B
QB
�B
B
�B
�B
7B
1B
1B
B
+B
�B
�B
?B
gB
SB
B
�B
&B
�B
TB
oB
�B
�B
 B
oB
 B
�B
:B
 B
�B
&B
,B
�B
�B
NB
�B
�B
4B
 B
�B
.B
�B
\B
\B
HB
}B
�B
�B
�B
BB
�B
"B
�B
�B
<B
�B
VB
�B
<B
<B
pB
vB
<B
�B
B
jB
�B
B
�B
jB
B
PB
�B
�B
BB
pB
BB
B
�B
�B
�B
�B
VB
VB
�B
BB
vB
�B
B
�B
�B
vB
�B
BB
bB
.B
HB
�B
bB
�B
}B
.B
bB
NB
4B
�B
TB
:B
�B
 B
[B
[B
�B
�B
�B
[B
[B
FB
�B
�B
2B
,B
�B
MB
MB
�B
�B
�B
SB
�B

B
?B
YB
�B
�B
sB
�B
sB
_B
B
�B
B
B
KB
�B
B
B
�B
�B
QB
�B
�B
=B
�B
�B
=B
�B
)B
)B
B
�B
�B
�B
�B
�B
�B
~B
�B
�B
B
dB
�B
/B
IB
�B
~B
�B
�B
5B
B
OB
jB
OB
jB
�B
�B
!B
!B
�B
!B
B
B
�B
 B
!HB
!|B
!�B
!�B
!�B
!|B
!HB
"B
!�B
"4B
"4B
"hB
"4B
# B
# B
#�B
#�B
#�B
$tB
$@B
$�B
$�B
$�B
$�B
%B
%zB
%�B
%`B
%zB
%�B
%�B
&2B
&2B
&�B
'B
'mB
'�B
'�B
'RB
'mB
'mB
'�B
($B
(
B
(
B
'�B
(�B
)�B
)�B
*�B
+B
+�B
,"B
,WB
-�B
-�B
-�B
.B
.�B
.�B
.�B
/5B
/�B
1vB
1'B
1B
1AB
1vB
1�B
1�B
1�B
2GB
2�B
2aB
2aB
2GB
2aB
3B
4B
4nB
4�B
4�B
4�B
5B
5B
5?B
5?B
5%B
5�B
6�B
6FB
6�B
6�B
7�B
6�B
6zB
6�B
7�B
88B
8�B
8B
8B
7�B
8B
8RB
8�B
8�B
8�B
8�B
8�B
9rB
9$B
9>B
:*B
:�B
:�B
:�B
:�B
:xB
:�B
;dB
;JB
;dB
;�B
<B
;�B
<�B
<�B
<�B
=B
=�B
=�B
=�B
=�B
=�B
>wB
>�B
>�B
?B
?}B
?.B
?cB
?HB
?�B
?�B
?�B
?�B
@OB
@4B
AoB
A�B
A�B
AUB
A B
A;B
AoB
A�B
B[B
B�B
C-B
CGB
CB
C-B
C-B
DB
C�B
D�B
D�B
EB
D�B
D�B
D�B
ESB
E9B
E9B
E�B
F%B
F%B
E�B
FYB
FYB
E�B
FYB
G+B
F�B
G+B
G_B
GzB
G�B
H1B
IB
IlB
IlB
I7B
JXB
J�B
J�B
J�B
J�B
J�B
KxB
K)B
K�B
K)B
K�B
L~B
L�B
MPB
MjB
MjB
MPB
M6B
M�B
N<B
NpB
N�B
O(B
OBB
OvB
O�B
O�B
P�B
PbB
P}B
P�B
P�B
P�B
Q4B
Q4B
RB
RB
Q�B
Q�B
RTB
RTB
RTB
R�B
R�B
S&B
SuB
S&B
SB
S&B
S�B
S@B
S�B
T{B
T�B
T,B
T�B
T�B
U2B
U2B
UB
T�B
UMB
U�B
U�B
VB
V9B
V9B
V9B
VSB
V9B
V�B
V�B
W�B
W$B
W�B
X_B
XEB
XB
XyB
YB
X�B
YKB
Z�B
[�B
[=B
[	B
Z�B
[�B
[�B
\B
[�B
\�B
\�B
]IB
]IB
]~B
]�B
]�B
^B
^jB
^5B
^jB
^�B
_�B
_�B
_�B
`B
`�B
`BB
`vB
aB
a�B
abB
a�B
a�B
aHB
a�B
b4B
bB
bB
bNB
c�B
cB
cTB
cnB
d&B
d@B
dZB
dtB
d@B
d@B
dtB
dtB
d�B
eB
ezB
e`B
e`B
fB
fB
f�B
f�B
gB
g�B
g�B
g�B
h
B
h$B
hsB
h>B
h�B
h�B
i*B
h�B
i_B
i�B
jKB
kB
kB
k6B
kB
k�B
k�B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
l�B
mCB
m]B
mCB
nIB
nB
nIB
n/B
n�B
n�B
n�B
n�B
oB
oB
oiB
o�B
pUB
p�B
p�B
p�B
poB
p�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
sMB
sMB
shB
s3B
s�B
tnB
tnB
t�B
t�B
t�B
t�B
t�B
uB
u�B
u�B
vFB
v�B
w�B
wLB
wfB
w�B
w�B
w�B
xB
xB
xRB
x�B
y>B
y>B
y�B
yXB
zB
z*B
z^B
z�B
z�B
z�B
z�B
{JB
{�B
{�B
|B
|�B
}<B
}<B
}VB
}VB
}"B
}<B
}qB
}<B
}VB
~]B
~�B
~BB
~�B
B
HB
HB
}B
}B
�B
�B
�B
�B
�B
�B
�4B
��B
�OB
�B
��B
��B
��B
��B
��B
��B
�'B
�B
�uB
�[B
��B
��B
��B
�-B
�-B
�-B
�-B
�aB
�B
��B
��B
��B
��B
�3B
��B
��B
��B
��B
�B
��B
�B
�B
�B
�MB
��B
��B
��B
�SB
��B
�mB
�B
�mB
��B
�B
��B
�tB
�tG�O�BI�BDgBIRBEBI�BGEBF�BIBH�BG�BGzBH�BG�BGzBH�BH�BGBIBH�BG�BGBI�BF�BFtBHKBIBF�BH�BG�BG�BG�BI�BH�BG�BIBHKBGEBH�BIBGBHKBIRBHBGBIBH�BGEBHBIRBGzBHKBH�BGEBHKBIBG�BG�BIBH�BG�BH�BH�BGEBG�BIRBHKBGEBH�BIRBG�BG�BIBH�BGEBHBIRBGzBH�BIBG�BHBIRBH�BF�BHBIBHBGEBHKBI�BGBIBIRBHKBGzBIRBHKBGzBHBI�BGzBHBH�BG�BG�BI�BH�BGzBHKBJ#BHBG�BH�BH�BGzBI�BI�BG�BH�BH�BH�BG�BH�BH�BG�BHBI�BI�BGzBG�BI�BHBG�BH�BIBG�BH�BI�BHKBG�BHBI�BH�BGEBH�BIBH�BGEBHKBIBG�BGzBH�BIBG�BH�BI�BG�BGEBH�BH�BGBG�BIRBHBGzBH�BH�BGzBHKBH�BGzBG�BIBH�BG�BGEBIRBH�BG�BG�BH�BIRBGEBHBIBG�BG�BHKBH�BG�BGzBIRBIRBG�BGEBH�BH�BG�BHBIBG�BGEBH�BI�BG�BGBHBH�BHKBGBG�BIRBHBGzBG�BH�BH�BGBHKBIRBH�BGBHKBH�BGzBGEBI�BIBGEBGBIBHBGBIBIBGzBGBH�BH�BH�BGzBGzBH�BHBHKBGEBH�BH�BG�BF�BG�BH�BJ#BG�BH�BI�BG�BFBHKBG�BGBGzBIBG�BGEBI�BG�BHKBGEBJ�BH�BF�BH�BIRBH�BG�BIBI�BHBG�BH�BI�BG�BG�BIRBI�BH�BG�BIBI�BH�BHKBHKBI�BI�BH�BG�BI�BI�BH�BHKBIRBI�BHBIBJ�BI�BH�BI�BK^BK)BJXBI�BJ�BK�BK�BI�BJ#BK^BJ�BI�BJXBK^BJ#BI�BJ�BL0BK�BK)BK�BNBM�BL0BL�BN�BQNBOvBO�BPBR BQNBP�BP�BP�BQNBO�BNBOvBPBOBBM6BM6BM�BM�BL�BMjBN�BN�BMjBL�BNpBN�BNpBM6BM6BM�BN<BMBK)BL�BM6BL0BJ�BK�BL�BN<BK�BK�BMBNBL�BJ�BL0BMjBMBL0BK�BMjBN<BM6BK�BK�BMjBM�BL�BL�BN<BOBBNpBMjBMjBNpBM�BK�BK�BNBN�BNpBM�BLdBMjBM�BL0BK�BK�BM6BM�BNpBL�BK�BM�BOBBOBL�BL0BMBL�BK�BK�BK)BM6BMBJ�BK)BLdBK)BI�BI�BI�BJ�BIBG�BGBGBG�BG�BFBDgBC�BCaBD3BDgBPHB`�By	B��B�DB�B&LBT�Bp�Bv�B�B��B�`B	B	=<B	�B	�xB	�+B	�&B
fB
$B
<�B
�(B
�B
ΥB
�B
�>B
�B �B~B{B_B�B4�BF?B`By�B��B��B�hB��B�{B��B�+B�=B�B��B�YB�B��B��B��B�'B��B˒B�NB�tB�tB��BʌBΥB�pB�B��B�jB��B̘B̘B�B��B�vB�BΥBѷB՛B�WBخB��B�QBںB�B�HB�AB�KB�B�B�8B�B�]B�B��B��B�B�B�B�GB��B iBB�BBJBB{BB�B$B�B�B�B�B�B�B\B�B�B#nB!�B=B+BOB�B+BkB=B�B@B$B1B7BBeBYB�B�BuBhBbB�B�B@B B�BVB"BB
rB
rB~B�B�B%B�BVB	7B��B�B�.B��B�B�B�B��B��B��B��B��B�8B҉BѷB��B�zB��B��B��B��B�UB�tB��B��B�CB�YB�%B��B�1Bw�Bs�B��B�_BsBGEB4B6�B�B�B�B�BVB�B
��BoB
�"B
��B
�	B
��B
�B
�B
�B
�B
��B
�B
�B�B
�?B
�B
�2B
�$B
�'B
�@B
��B
бB
��B
��B
iB
NpB
R�B
K^B
C-B
D�B
;�B
;0B
9�B
LdB
)�B
%�B
7B
/�B
B
B
u%B	��B	��B
 �B	ٴB	�B	ΥB	�UB	�RB	�B	�}B	��B	�^B	�qB	��B	�0B	��B	�*B	�RB	��B	�B	��B	��B	ÖB	��B	��B	�B	��B	�-B	��B	��B	�B	��B	�B	�FB	��B	��B	�qB	�nB	��B	�~B	��B	�B	��B	�0B	��B	��B	��B	��B	��B	�MB	�B	}�B	�B	�B	�iB	��B	|�B	y	B	t�B	��B	�B	h�B	o�B	q�B	b�B	g�B	c B	f�B	_B	XB	f2B	W�B	XB	T�B	TaB	K�B	NB	M�B	l�B	I�B	?�B	9XB	>�B	MjB	@�B	=<B	7B	3�B	5�B	3�B	7LB	C�B	+�B	-�B	1�B	*�B	!�B	(XB	�uB	oB	�B	!�B	DB	�B	 B	JB	
rB	�B	�B	AB	MB	�B	 iB	�B	�B�]B��B	;B�B��B��B�B��B�DB�B�TB��B�B�B�B�B��B�5B�)B�/B��B�iB��B��B��B�B�B�B�B�]B�QB� B��B�&B�+B��B�9B�B�B��B��B��B�vB��B��BӏB��B�[B�&B�WB�"B��BҽG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<=D��<#�
<#�
<#�
<0;�<1�a<<�T<%H�<C$B<#�
<#�
<FN�<���<��<��L<�L<�o<yYe<#�
<#�
<�}�<�7�<�$�<�f<�o<�0<#�
<#�
<#�
<)��<��<w��<4��<���<f"�<.�<#�
<FN�<��w<�9�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:�l<#�
<\/�<#�
<#�
<b�\<&Z"<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019012614164720190126141647IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019020515003320190205150033QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019020515003320190205150033QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551420190521075514IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                