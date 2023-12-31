CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-12-17T21:22:31Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20181217212231  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               "   "AA  AOAO7316_008644_034                 7316_008644_034                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @ؘ�>lLY@ؘ�>lLY11  @ؘ�}Vl�@ؘ�}Vl�@*��9m@*��9m�c�\�¹N�c�\�¹N11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?�@B�\@�  @�G�@��R@�  A ��A��A   A,(�A>�RA_\)A�  A��A�  A�  A��A�\)A߮A�  B (�B  B  B  B   B(  B/�
B8  B@  BH(�BP  BW�
B_�
Bg�
BpQ�Bx��B�{B�  B�B�{B�{B�  B�  B��
B��B��B��B�  B�{B�{B�{B�  B��B��B�{B�(�B�  B�  B�  B�  B�{B�  B��B��B��B�  B��B�  C   C��C  C��C�C
  C
=C
=C  C
=C{C�C(�C  C  C��C��C!��C$  C&  C'��C*  C,  C.  C0  C1��C4  C6
=C8
=C:  C<
=C>  C@  CB  CD  CF  CH  CJ  CK��CN  CO��CQ��CS��CV  CX
=CZ  C[��C^  C_��Ca��Cc��Cf  Cg��Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cw��Cz  C|  C~  C��C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C���C���C�  C�  C���C�C�C�C�C���C�  C�C���C���C�  C�C�  C���C�  C�  C�  C�C�C�  C�  C���C���C���C���C���C���C�C�C�  C�C�C���C���C���C�  C�  C�  C�  C�  C���C�  C���C���C�C�  C�  C�C�  C�  C���C���C���C���C���C�C�C�  C���C���C�  C���C���C�  C�C�C���C�  C�  C���C�  C�C�C�C�  C�C�C�  C�  C�C�C�  C�C�  C�  C�C�C�  C���C���C�  C�  C�C�  C���C���C�  C�C���C�  C�C�  C�C�C�C�  C���C���C�  D   D ��DD��D  D� D�D��D�qD� D  D}qD�qD}qD�qD� D�qD}qD�qD	z�D	�qD
��D�D� D  D� D  D��D  D��D�D��D  D� D  D}qD�qD}qD  D��D  D� D�qD� D�D� D�qD� D�D� D�qD��D  D� DD��D��Dz�D  D� D  D� D�qD� D   D }qD �qD!}qD"�D"� D"�qD#� D$  D$}qD$�qD%}qD&�D&��D'�D'��D(�D(��D(�qD)}qD*�D*��D+  D+�D,D,�D-D-� D.  D.��D.�qD/}qD0  D0� D1�D1� D1�qD2� D3�D3� D4  D4��D5�D5�D6D6��D7�D7� D8  D8� D9  D9��D:�D:��D;  D;z�D<  D<��D=  D=��D>�D>� D>��D?}qD@  D@� DA�DA� DA�qDB� DC�DC�DD�DD� DE  DE��DF  DF}qDG  DG��DH  DH}qDI�DI� DJ  DJ� DJ�qDK��DLDL��DM  DM}qDM�qDN��DO  DO� DP  DP� DQ�DQ��DR�DR��DSDS� DT  DT��DU  DU}qDV�DV��DW  DW��DXDX��DX�qDY� DZ�DZ� D[�D[�D\�D\� D]�D]�D^�D^��D_D_��D_�qD`z�D`��Da}qDb  Db� Dc  Dc}qDc�qDd� De�De��Df�Df��DgDg�Dh  Dh� Di�Di� Dj�Dj�Dk�Dk� Dl  Dl}qDm  Dm��Dn  Dn� Do  Do� Dp  Dp� Dq�Dq� Dr  Dr� Dr�qDs}qDt  Dt��Du�Du� Dv  Dv� Dw�Dw��Dx  Dx}qDy  Dy}qDy�qDz� D{  D{��D|  D|}qD|�qD}}qD~  D~�D�D� D�  D�AHD���D�D�  D�@ D��HD��HD��D�AHD�� D�� D�HD�>�D�}qD��qD���D�AHD��HD��HD��D�AHD��HD��HD�HD�AHD�� D�� D�HD�AHD���D�� D���D�>�D�� D���D�  D�B�D��HD��HD�HD�@ D�� D�� D���D�>�D�� D��HD�HD�AHD�� D�� D�HD�AHD�� D���D�  D�AHD�� D��qD���D�@ D�~�D�� D�  D�>�D��HD�� D�  D�@ D�� D�� D���D�AHD�� D���D�  D�B�D���D�� D���D�>�D�~�D���D�  D�@ D��HD�� D���D�AHD�� D���D�HD�B�D��HD���D�  D�AHD�� D���D�  D�B�D��HD��HD�HD�@ D�� D�� D���D�>�D�}qD���D�  D�@ D�~�D��qD�  D�AHD�� D�� D���D�@ D�~�D���D�  D�@ D�� D��HD�  D�>�D�� D���D�HD�@ D�~�D��HD�  D�>�D��HD�D�HD�@ D��HD�D�  D�>�D�� D���D��qD�>�D�� D�� D�  D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�}qD���D���D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�AHD��HD�� D��D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�}qD���D�  D�AHD���D��HD���D�>�D�~�D�� D�HD�AHD�� D�D���D�>�D��HD��HD�HD�B�D�� D�� D�HD�AHD�� D��qD���D�@ D�~�D��qD�  D�>�D�~�D�� D��D�AHD�~�D��HD���D�=qD�~�D���D���D�AHD��HD��HD�HD�>�D�~�D�� D�  D�@ D�~�D���D���D�=qD�~�D�� D�  D�AHD�~�D�� D�  D�AHD��HD�� D�  D�@ D�}qD�� D�HD�@ DÀ Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D�  D�AHDʁHDʾ�D���D�B�Dˀ D�� D�  D�>�D�~�D̾�D���D�@ D́HD��HD��D�@ D�~�Dξ�D�  D�>�D�}qDϾ�D�  D�>�DЁHD��HD�  D�AHDр DѾ�D��qD�=qDҀ D��HD�  D�@ DӁHD��HD�HD�AHDԀ D��HD�  D�>�D�~�D�� D�  D�@ Dր D�� D�HD�@ D׀ D׾�D���D�>�D؀ D��HD�  D�@ Dـ Dپ�D���D�@ DځHD�� D�  D�@ Dۀ D�� D�  D�=qD�~�D�� D�  D�=qD�~�D�� D���D�@ DށHD��HD�  D�@ D�~�D�� D�HD�AHD��HD��HD�HD�AHD� DᾸD���D�>�D� D��HD�  D�>�D�HD��HD���D�AHD�HD侸D��qD�@ D�~�D徸D�  D�@ D� D澸D���D�@ D�~�D�� D�  D�@ D� D��HD�HD�@ D�~�D��HD�  D�@ D�~�D�� D��D�AHD� D뾸D���D�>�D� D�� D���D�@ D� D��HD���D�=qD�~�D�� D���D�>�D� D�� D���D�=qD�� D�� D��qD�>�D�~�D�� D�  D�AHD�D��HD�  D�@ D� D�� D���D�@ D�D�� D�  D�@ D�� D��HD���D�>�D�� D�� D�HD�AHD��HD��HD��D�@ D�~�D���D�HD�@ D�~�D���D�HD�<)D���?#�
?B�\?k�?�=q?���?�{?���?�(�@   @�@(�@+�@=p�@L��@^�R@s33@��\@��@�@��@�=q@���@��H@Ǯ@��@ٙ�@��
@�\)@�
=A   AA
�HA\)A�
A��A{A"�\A&ffA+�A0��A5�A9��A?\)ADz�AHQ�AL��AR�\AW�A\��AaG�Ag
=Al��AqG�AuA{�A���A��A�A�Q�A��
A�{A�Q�A��HA�A���A��\A��A�  A��\A���A�\)A��\A���A�\)A��A�z�A�
=A���A��A�ffA�G�AÅA�Aȣ�A��HA���A�\)A�=qA�(�AָRA�G�A��
A�{A�  A�\A�p�A�\)A陚A��
A�RA���A��HA��A��A�=qA�z�A�ffB Q�B��B�HB�B��B{B33B(�B	p�B
�RB�
B�B�B33B��BB�HB(�Bp�B�\B�B��B{B\)Bz�Bp�B
=B   B!�B"ffB#�
B$��B%�B'\)B(��B)�B*�HB,  B-p�B.�RB/�B0��B2ffB3\)B4z�B5B7
=B8Q�B9G�B:�\B<  B=�B>=qB?\)B@��BA�BC33BDQ�BEG�BF�RBH  BH��BJ{BK33BL��BMBN�HBO�
BQG�BRffBS�BT��BV{BW\)BXz�BYp�BZ�RB\(�B]G�B^=qB_�B`��Bb{Bc
=Bd  BeG�Bf�\Bg�Bh��BiBk
=BlQ�BmG�Bn=qBo33Bpz�Bq��BrffBs33Bs�
Bt��Bu��Bv{BvffBv�RBw33Bw�Bw�Bw�
Bx(�Bxz�Bxz�Bx��Bx��Byp�Byp�By��ByBzffBz�\Bz�\Bz�HB{33B{�B{�
B{�
B|Q�B|��B|��B}�B}G�B}B~{B~=qB~ffB~�RB33B�B�B�B�{B�=qB�ffB�z�B�z�B���B��HB���B�
=B��B�G�B�p�B��B��B��B��
B�  B�{B�{B�(�B�=qB�z�B�z�B�z�B��\B��RB��HB���B���B�
=B�33B�G�B�G�B�G�B�p�B��B���B���B���B��B�B��
B��B��
B��
B�  B�{B�{B�{B�{B�(�B�Q�B�Q�B�Q�B�=qB�Q�B�ffB��\B�z�B�ffB�z�B���B���B��\B��\B���B��RB��HB���B���B��HB�
=B��B��B�33B�33B�p�B��B��B�B�B��B�{B�ffB�z�B��\B��RB���B��B�\)B���B��
B�  B�=qB��\B��HB�33B��B��
B�=qB��\B���B��B�p�B��
B�=qB���B���B�G�B���B�  B�Q�B��RB��B��B��B�Q�B���B���B�G�B��B�  B�z�B���B��B�p�B�B�{B�ffB���B��B��B��B�=qB���B���B�G�B���B�  B�ffB���B��B���B�  B�ffB���B��B��B��B�=qB���B��B���B�  B�z�B��HB�G�B�B�(�B��\B���B�\)B�B�(�B��\B���B�p�B��B�Q�B���B�33B��B�{B�z�B��HB�G�B��B�{B�z�B��HB�\)B��
B�=qB��RB�33B��B�(�B���B��B��B�  B�ffB��HB�G�B��
B�Q�B��HB�\)B��
B�ffB��HB�p�B��B�z�B���B��B�  B�z�B���B��B�  B��\B�
=B���B�{B���B�33B��B�=qB��RB�G�B�B�Q�B��HB�p�B��B�z�B���B���B�{B£�B��B�B�=qB���B�G�B��
B�ffB�
=BǙ�B�(�B���B�G�B��Bʏ\B��B�B�ffB���B͙�B�=qB���BυB�{BиRB�p�B�  BҸRB�\)B�  Bԣ�B�G�B�  B֣�B�G�B��B؏\B�33B��Bڏ\B�33B��B܏\B�33B��Bޏ\B�G�B�  B��B�G�B�  B�RB�\)B�{B�RB�p�B�(�B���B�B�=qB���B陚B�Q�B��B��
B�\B�G�B�  B���B�B�=qB�
=B�B�\B�G�B�  B���B��B�Q�B�
=B�B��\B�G�B�  B��RB��B�Q�B��B��
B��RB�p�C �C z�C �HC=qC��C�CQ�C�C{Cz�C�HC=qC�\C�CQ�C��C
=CffC��C33C�\C�CQ�C�C	
=C	ffC	C
(�C
�C
�CQ�C�RC{Cp�C��C�C�C�
C33C�\C�CG�C�C
=CffCC{Cp�CC{CffCC{Cp�C��C(�C�C�
C33C�C�
C(�Cp�C�RC  C=qCz�C�C�HC
=C=qCffC�C��C�RC�
C�C
=C(�C=qCffCz�C��C�RC��C�C  C{C(�CG�CffCz�C��CC��C�C  C{C(�C=qC\)Cp�C�\C�C��C�C  C�C33CQ�CffC�C�\C�CC�HC  C�C=qCQ�CffCz�C�\C��CC�HC  C�C33CQ�C\)Cp�C�\C��C��C�C  C�C33CG�CffC�C��C�RC�
C��C 
=C �C =qC Q�C p�C �\C �RC �
C �C!{C!(�C!=qC!\)C!p�C!�\C!�C!��C!��C"{C"33C"Q�C"ffC"z�C"��C"�RC"�HC#  C#�C#=qC#\)C#p�C#�\C#��C#C#�C$
=C$(�C$Q�C$p�C$�\C$�C$C$�HC$��C%�C%=qC%ffC%�\C%�C%��C%�C&  C&�C&=qC&ffC&�\C&�RC&�
C'  C'{C'33C'Q�C'z�C'��C'C'�C({C(=qC(ffC(�C(��C(��C(�C){C)G�C)z�C)��C)C)�C*{C*33C*\)C*�\C*�RC*�C+
=C+(�C+Q�C+�C+�C+�HC,{C,=qC,\)C,�\C,�C,�
C-
=C-33C-ffC-��C-C-�C.{C.33C.ffC.��C.��C/  C/(�C/Q�C/p�C/��C/�
C0
=C0=qC0p�C0��C0C0��C1(�C1Q�C1�\C1C1�C2�C2G�C2p�C2��C2�
C3
=C3=qC3z�C3�C3�
C4
=C433C4ffC4�\C4C5  C533C5ffC5��C5C5��C6�C6G�C6z�C6�C6�HC7�C7G�C7p�C7��C7��C8
=C8=qC8p�C8��C8C8�C9(�C9\)C9�\C9�RC9�HC:{C:G�C:z�C:�RC:�C;{C;=qC;p�C;��C;�HC<{C<=qC<ffC<�\C<C<��C=33C=ffC=�\C=�RC=�HC>{C>Q�C>�C>�C>�
C?  C?(�C?\)C?�\C?��C?�C@{C@=qC@p�C@��C@�
CA  CA(�CAG�CAp�CA��CA�
CB  CB33CBQ�CBz�CB��CB�
CC  CC33CCQ�CCp�CC��CC��CC��CD(�CDG�CDp�CD�\CDCD��CE�CE=qCEffCE�CE�CE�HCF{CF33CF\)CFz�CF��CF�
CG
=CG=qCG\)CGz�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                               ?��?�@B�\@�  @�G�@��R@�  A ��A��A   A,(�A>�RA_\)A�  A��A�  A�  A��A�\)A߮A�  B (�B  B  B  B   B(  B/�
B8  B@  BH(�BP  BW�
B_�
Bg�
BpQ�Bx��B�{B�  B�B�{B�{B�  B�  B��
B��B��B��B�  B�{B�{B�{B�  B��B��B�{B�(�B�  B�  B�  B�  B�{B�  B��B��B��B�  B��B�  C   C��C  C��C�C
  C
=C
=C  C
=C{C�C(�C  C  C��C��C!��C$  C&  C'��C*  C,  C.  C0  C1��C4  C6
=C8
=C:  C<
=C>  C@  CB  CD  CF  CH  CJ  CK��CN  CO��CQ��CS��CV  CX
=CZ  C[��C^  C_��Ca��Cc��Cf  Cg��Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cw��Cz  C|  C~  C��C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C���C���C�  C�  C���C�C�C�C�C���C�  C�C���C���C�  C�C�  C���C�  C�  C�  C�C�C�  C�  C���C���C���C���C���C���C�C�C�  C�C�C���C���C���C�  C�  C�  C�  C�  C���C�  C���C���C�C�  C�  C�C�  C�  C���C���C���C���C���C�C�C�  C���C���C�  C���C���C�  C�C�C���C�  C�  C���C�  C�C�C�C�  C�C�C�  C�  C�C�C�  C�C�  C�  C�C�C�  C���C���C�  C�  C�C�  C���C���C�  C�C���C�  C�C�  C�C�C�C�  C���C���C�  D   D ��DD��D  D� D�D��D�qD� D  D}qD�qD}qD�qD� D�qD}qD�qD	z�D	�qD
��D�D� D  D� D  D��D  D��D�D��D  D� D  D}qD�qD}qD  D��D  D� D�qD� D�D� D�qD� D�D� D�qD��D  D� DD��D��Dz�D  D� D  D� D�qD� D   D }qD �qD!}qD"�D"� D"�qD#� D$  D$}qD$�qD%}qD&�D&��D'�D'��D(�D(��D(�qD)}qD*�D*��D+  D+�D,D,�D-D-� D.  D.��D.�qD/}qD0  D0� D1�D1� D1�qD2� D3�D3� D4  D4��D5�D5�D6D6��D7�D7� D8  D8� D9  D9��D:�D:��D;  D;z�D<  D<��D=  D=��D>�D>� D>��D?}qD@  D@� DA�DA� DA�qDB� DC�DC�DD�DD� DE  DE��DF  DF}qDG  DG��DH  DH}qDI�DI� DJ  DJ� DJ�qDK��DLDL��DM  DM}qDM�qDN��DO  DO� DP  DP� DQ�DQ��DR�DR��DSDS� DT  DT��DU  DU}qDV�DV��DW  DW��DXDX��DX�qDY� DZ�DZ� D[�D[�D\�D\� D]�D]�D^�D^��D_D_��D_�qD`z�D`��Da}qDb  Db� Dc  Dc}qDc�qDd� De�De��Df�Df��DgDg�Dh  Dh� Di�Di� Dj�Dj�Dk�Dk� Dl  Dl}qDm  Dm��Dn  Dn� Do  Do� Dp  Dp� Dq�Dq� Dr  Dr� Dr�qDs}qDt  Dt��Du�Du� Dv  Dv� Dw�Dw��Dx  Dx}qDy  Dy}qDy�qDz� D{  D{��D|  D|}qD|�qD}}qD~  D~�D�D� D�  D�AHD���D�D�  D�@ D��HD��HD��D�AHD�� D�� D�HD�>�D�}qD��qD���D�AHD��HD��HD��D�AHD��HD��HD�HD�AHD�� D�� D�HD�AHD���D�� D���D�>�D�� D���D�  D�B�D��HD��HD�HD�@ D�� D�� D���D�>�D�� D��HD�HD�AHD�� D�� D�HD�AHD�� D���D�  D�AHD�� D��qD���D�@ D�~�D�� D�  D�>�D��HD�� D�  D�@ D�� D�� D���D�AHD�� D���D�  D�B�D���D�� D���D�>�D�~�D���D�  D�@ D��HD�� D���D�AHD�� D���D�HD�B�D��HD���D�  D�AHD�� D���D�  D�B�D��HD��HD�HD�@ D�� D�� D���D�>�D�}qD���D�  D�@ D�~�D��qD�  D�AHD�� D�� D���D�@ D�~�D���D�  D�@ D�� D��HD�  D�>�D�� D���D�HD�@ D�~�D��HD�  D�>�D��HD�D�HD�@ D��HD�D�  D�>�D�� D���D��qD�>�D�� D�� D�  D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�}qD���D���D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�AHD��HD�� D��D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�}qD���D�  D�AHD���D��HD���D�>�D�~�D�� D�HD�AHD�� D�D���D�>�D��HD��HD�HD�B�D�� D�� D�HD�AHD�� D��qD���D�@ D�~�D��qD�  D�>�D�~�D�� D��D�AHD�~�D��HD���D�=qD�~�D���D���D�AHD��HD��HD�HD�>�D�~�D�� D�  D�@ D�~�D���D���D�=qD�~�D�� D�  D�AHD�~�D�� D�  D�AHD��HD�� D�  D�@ D�}qD�� D�HD�@ DÀ Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D�  D�AHDʁHDʾ�D���D�B�Dˀ D�� D�  D�>�D�~�D̾�D���D�@ D́HD��HD��D�@ D�~�Dξ�D�  D�>�D�}qDϾ�D�  D�>�DЁHD��HD�  D�AHDр DѾ�D��qD�=qDҀ D��HD�  D�@ DӁHD��HD�HD�AHDԀ D��HD�  D�>�D�~�D�� D�  D�@ Dր D�� D�HD�@ D׀ D׾�D���D�>�D؀ D��HD�  D�@ Dـ Dپ�D���D�@ DځHD�� D�  D�@ Dۀ D�� D�  D�=qD�~�D�� D�  D�=qD�~�D�� D���D�@ DށHD��HD�  D�@ D�~�D�� D�HD�AHD��HD��HD�HD�AHD� DᾸD���D�>�D� D��HD�  D�>�D�HD��HD���D�AHD�HD侸D��qD�@ D�~�D徸D�  D�@ D� D澸D���D�@ D�~�D�� D�  D�@ D� D��HD�HD�@ D�~�D��HD�  D�@ D�~�D�� D��D�AHD� D뾸D���D�>�D� D�� D���D�@ D� D��HD���D�=qD�~�D�� D���D�>�D� D�� D���D�=qD�� D�� D��qD�>�D�~�D�� D�  D�AHD�D��HD�  D�@ D� D�� D���D�@ D�D�� D�  D�@ D�� D��HD���D�>�D�� D�� D�HD�AHD��HD��HD��D�@ D�~�D���D�HD�@ D�~�D���D�HD�<)G�O�?#�
?B�\?k�?�=q?���?�{?���?�(�@   @�@(�@+�@=p�@L��@^�R@s33@��\@��@�@��@�=q@���@��H@Ǯ@��@ٙ�@��
@�\)@�
=A   AA
�HA\)A�
A��A{A"�\A&ffA+�A0��A5�A9��A?\)ADz�AHQ�AL��AR�\AW�A\��AaG�Ag
=Al��AqG�AuA{�A���A��A�A�Q�A��
A�{A�Q�A��HA�A���A��\A��A�  A��\A���A�\)A��\A���A�\)A��A�z�A�
=A���A��A�ffA�G�AÅA�Aȣ�A��HA���A�\)A�=qA�(�AָRA�G�A��
A�{A�  A�\A�p�A�\)A陚A��
A�RA���A��HA��A��A�=qA�z�A�ffB Q�B��B�HB�B��B{B33B(�B	p�B
�RB�
B�B�B33B��BB�HB(�Bp�B�\B�B��B{B\)Bz�Bp�B
=B   B!�B"ffB#�
B$��B%�B'\)B(��B)�B*�HB,  B-p�B.�RB/�B0��B2ffB3\)B4z�B5B7
=B8Q�B9G�B:�\B<  B=�B>=qB?\)B@��BA�BC33BDQ�BEG�BF�RBH  BH��BJ{BK33BL��BMBN�HBO�
BQG�BRffBS�BT��BV{BW\)BXz�BYp�BZ�RB\(�B]G�B^=qB_�B`��Bb{Bc
=Bd  BeG�Bf�\Bg�Bh��BiBk
=BlQ�BmG�Bn=qBo33Bpz�Bq��BrffBs33Bs�
Bt��Bu��Bv{BvffBv�RBw33Bw�Bw�Bw�
Bx(�Bxz�Bxz�Bx��Bx��Byp�Byp�By��ByBzffBz�\Bz�\Bz�HB{33B{�B{�
B{�
B|Q�B|��B|��B}�B}G�B}B~{B~=qB~ffB~�RB33B�B�B�B�{B�=qB�ffB�z�B�z�B���B��HB���B�
=B��B�G�B�p�B��B��B��B��
B�  B�{B�{B�(�B�=qB�z�B�z�B�z�B��\B��RB��HB���B���B�
=B�33B�G�B�G�B�G�B�p�B��B���B���B���B��B�B��
B��B��
B��
B�  B�{B�{B�{B�{B�(�B�Q�B�Q�B�Q�B�=qB�Q�B�ffB��\B�z�B�ffB�z�B���B���B��\B��\B���B��RB��HB���B���B��HB�
=B��B��B�33B�33B�p�B��B��B�B�B��B�{B�ffB�z�B��\B��RB���B��B�\)B���B��
B�  B�=qB��\B��HB�33B��B��
B�=qB��\B���B��B�p�B��
B�=qB���B���B�G�B���B�  B�Q�B��RB��B��B��B�Q�B���B���B�G�B��B�  B�z�B���B��B�p�B�B�{B�ffB���B��B��B��B�=qB���B���B�G�B���B�  B�ffB���B��B���B�  B�ffB���B��B��B��B�=qB���B��B���B�  B�z�B��HB�G�B�B�(�B��\B���B�\)B�B�(�B��\B���B�p�B��B�Q�B���B�33B��B�{B�z�B��HB�G�B��B�{B�z�B��HB�\)B��
B�=qB��RB�33B��B�(�B���B��B��B�  B�ffB��HB�G�B��
B�Q�B��HB�\)B��
B�ffB��HB�p�B��B�z�B���B��B�  B�z�B���B��B�  B��\B�
=B���B�{B���B�33B��B�=qB��RB�G�B�B�Q�B��HB�p�B��B�z�B���B���B�{B£�B��B�B�=qB���B�G�B��
B�ffB�
=BǙ�B�(�B���B�G�B��Bʏ\B��B�B�ffB���B͙�B�=qB���BυB�{BиRB�p�B�  BҸRB�\)B�  Bԣ�B�G�B�  B֣�B�G�B��B؏\B�33B��Bڏ\B�33B��B܏\B�33B��Bޏ\B�G�B�  B��B�G�B�  B�RB�\)B�{B�RB�p�B�(�B���B�B�=qB���B陚B�Q�B��B��
B�\B�G�B�  B���B�B�=qB�
=B�B�\B�G�B�  B���B��B�Q�B�
=B�B��\B�G�B�  B��RB��B�Q�B��B��
B��RB�p�C �C z�C �HC=qC��C�CQ�C�C{Cz�C�HC=qC�\C�CQ�C��C
=CffC��C33C�\C�CQ�C�C	
=C	ffC	C
(�C
�C
�CQ�C�RC{Cp�C��C�C�C�
C33C�\C�CG�C�C
=CffCC{Cp�CC{CffCC{Cp�C��C(�C�C�
C33C�C�
C(�Cp�C�RC  C=qCz�C�C�HC
=C=qCffC�C��C�RC�
C�C
=C(�C=qCffCz�C��C�RC��C�C  C{C(�CG�CffCz�C��CC��C�C  C{C(�C=qC\)Cp�C�\C�C��C�C  C�C33CQ�CffC�C�\C�CC�HC  C�C=qCQ�CffCz�C�\C��CC�HC  C�C33CQ�C\)Cp�C�\C��C��C�C  C�C33CG�CffC�C��C�RC�
C��C 
=C �C =qC Q�C p�C �\C �RC �
C �C!{C!(�C!=qC!\)C!p�C!�\C!�C!��C!��C"{C"33C"Q�C"ffC"z�C"��C"�RC"�HC#  C#�C#=qC#\)C#p�C#�\C#��C#C#�C$
=C$(�C$Q�C$p�C$�\C$�C$C$�HC$��C%�C%=qC%ffC%�\C%�C%��C%�C&  C&�C&=qC&ffC&�\C&�RC&�
C'  C'{C'33C'Q�C'z�C'��C'C'�C({C(=qC(ffC(�C(��C(��C(�C){C)G�C)z�C)��C)C)�C*{C*33C*\)C*�\C*�RC*�C+
=C+(�C+Q�C+�C+�C+�HC,{C,=qC,\)C,�\C,�C,�
C-
=C-33C-ffC-��C-C-�C.{C.33C.ffC.��C.��C/  C/(�C/Q�C/p�C/��C/�
C0
=C0=qC0p�C0��C0C0��C1(�C1Q�C1�\C1C1�C2�C2G�C2p�C2��C2�
C3
=C3=qC3z�C3�C3�
C4
=C433C4ffC4�\C4C5  C533C5ffC5��C5C5��C6�C6G�C6z�C6�C6�HC7�C7G�C7p�C7��C7��C8
=C8=qC8p�C8��C8C8�C9(�C9\)C9�\C9�RC9�HC:{C:G�C:z�C:�RC:�C;{C;=qC;p�C;��C;�HC<{C<=qC<ffC<�\C<C<��C=33C=ffC=�\C=�RC=�HC>{C>Q�C>�C>�C>�
C?  C?(�C?\)C?�\C?��C?�C@{C@=qC@p�C@��C@�
CA  CA(�CAG�CAp�CA��CA�
CB  CB33CBQ�CBz�CB��CB�
CC  CC33CCQ�CCp�CC��CC��CC��CD(�CDG�CDp�CD�\CDCD��CE�CE=qCEffCE�CE�CE�HCF{CF33CF\)CFz�CF��CF�
CG
=CG=qCG\)CGz�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�?}A�=qA�=qA�?}A�?}A�?}A�?}A�A�A�A�A�C�A�E�A�E�A�C�A�A�A�A�A�;dAԮA���A��TA��;A���A���A���A��A�;dA�p�A�%A�^5A�
=A�?}A�l�A�+A�/A�A�A�p�A��hA�jA��9A��\A���A�%A���A���A��jA��9A�XA�Q�A�VA��7A�I�A���A�G�A~=qAu&�Ao�TAn  AlĜAk�Ak
=AiK�Af�uAd(�A_�mA]O�AV��ANv�AH�+AF�AEG�AC�A@z�A=�TA<�yA<1'A;hsA9&�A6�+A1�;A/�mA-�A,�A,jA,9XA+�A*�jA)��A*ȴA+;dA*r�A)�#A*�DA+�A,bA+��A+A+dZA*�!A)�A(A�A(��A(�A)A( �A'|�A'�PA'`BA'K�A'/A&��A&z�A&�A%�FA%�A$��A$bNA$1A#p�A"��A"^5A!��A!;dA ��A M�A�;A��A/A�jAffAI�AE�AbAXA"�A�RAM�AbA�^A`BA��AZA=qAƨAp�AS�A�HA��A��A%A��A{A�
A�wAXA�A��A�jAn�AbA�PAK�An�AJA�A�;A��Ax�A��A�/A�9AZAƨAXA�AȴAn�A��A+A��A��A�Ar�A1A�wA��A;dA
�!A
�A
n�A
bNA
A�A
 �A
bA	�A	/A��A��A��Av�AJA��AdZAO�AK�A%A��A��An�A��A`BA�A��A�AZA�AƨA\)A
=AĜA�+A{A��AO�A�A �/A �jA ~�A A�@�l�@�-@�p�@�/@�&�@�(�@�
=@���@��T@�p�@��@�Z@��;@�C�@�v�@�p�@�@�dZ@�ȴ@�^5@�O�@�j@@�+@�+@���@�X@��`@�r�@��@�+@�$�@�h@�O�@��`@�z�@���@柾@�V@�`B@�9@�Q�@��@���@�@�X@�/@��`@�A�@�+@�$�@���@ܬ@��m@�K�@�ȴ@ڇ+@�E�@�@�p�@�G�@��@��`@�\)@�=q@թ�@Ձ@�p�@�O�@�&�@���@�j@��@�1@���@�K�@Ұ!@��@��`@��@ϕ�@���@ΰ!@Η�@�ff@���@�hs@�V@̃@��@��@�E�@��@���@�(�@ǅ@�@ƸR@�~�@�@�7L@�Q�@þw@��y@�^5@��@�X@�X@��`@�  @��
@��@��@�dZ@�33@��!@�$�@��@�Q�@�(�@�  @��;@���@�C�@�ȴ@�~�@�E�@��@�X@��@���@�z�@�1'@��w@�|�@���@�5?@�J@��T@�`B@��/@��@�r�@�1@��F@�"�@�V@��#@���@��^@�hs@�X@�7L@���@�1'@�ƨ@���@�\)@���@��-@���@�x�@�O�@�Ĝ@�9X@���@�dZ@���@�M�@�{@���@��#@�@���@��7@�x�@��`@�Ĝ@�Z@�  @���@�33@��@���@���@�V@�@��@��^@�G�@�&�@�V@���@�Ĝ@��j@��@���@�I�@�b@��;@���@��@�33@��@��\@��#@�x�@�7L@�V@���@���@���@�1@��@�;d@��@���@�n�@�J@���@��@�/@��@���@��@�Q�@��
@�@�=q@���@���@�%@���@��u@�j@�1'@��@�t�@�33@���@��y@��!@�^5@�$�@��@���@��h@�hs@���@��9@�bN@�b@���@�ƨ@���@���@��P@�\)@�33@�@���@��!@���@�-@���@�X@�V@��`@���@��9@�r�@�(�@��@��@��P@�dZ@�o@��@��!@�ff@�5?@��@��^@�x�@��@��@�j@�I�@�9X@� �@�ƨ@��P@�K�@�+@��@���@�5?@��@���@���@�hs@�?}@���@���@���@��9@�bN@�b@�1@���@���@�l�@�;d@�+@��@��y@��R@�V@�@��#@��^@��7@�G�@�V@��D@�1'@�  @��F@�t�@�C�@��@��!@�E�@�5?@�@��#@�@�X@�V@��/@�Ĝ@���@�I�@�1'@� �@�b@|�@~ȴ@~5?@}��@}��@}@|��@|Z@{�
@{t�@{S�@{C�@{"�@z�@z��@z~�@y�7@x�`@xĜ@xA�@w�@wl�@w;d@v��@vv�@vv�@vv�@vff@u��@u`B@t�@tj@s��@s�F@s�@r�H@rM�@q�@q�^@q�^@qX@p�@pb@o�w@o;d@nȴ@m�@m�@m`B@m/@l��@lz�@k�m@k�F@kC�@j��@jM�@i�#@ihs@i&�@i%@h��@hbN@h1'@g�@f�@f�+@f5?@e�T@e�h@e�@d�/@d��@dj@d(�@ct�@b��@b~�@a�^@ax�@aG�@`�u@_�w@^�R@^v�@]�@]`B@]�@\�@\�D@[�m@[dZ@Z��@Z��@Z�\@Y��@Y7L@XĜ@X�u@XQ�@X �@W�w@W
=@V�R@Vv�@V5?@U�-@U?}@T�@S�F@R�H@Rn�@R=q@Q��@Q7L@Q%@P��@PA�@Pb@O�P@O+@N��@N{@M?}@L�@L9X@L1@K�@J�@J�\@J^5@JM�@JJ@I��@IG�@H��@HĜ@Hr�@G�;@G+@F��@F$�@E�T@E�@D��@D��@DI�@D�@Cƨ@Co@B�\@BM�@A�^@A�@@r�@@b@?��@?��@?l�@?K�@?
=@>�R@>V@=�T@=�@=?}@=?}@=/@=�@<��@<�/@<��@<j@;��@;�F@;dZ@;C�@:�@:�@:~�@9��@9��@9�7@9G�@97L@9&�@9&�@8��@8A�@7�w@7|�@7�@6�@6�R@6ff@5�-@5p�@5?}@4��@4�D@4(�@3ƨ@3�@3t�@3"�@2��@2�\@2n�@2M�@2-@1��@1G�@1&�@1�@0��@0��@0Q�@0b@/�@/l�@/+@.�@.��@.ff@.$�@-��@-�-@-�-@-�h@-/@,��@,j@,Z@,I�@,9X@,(�@+��@+�
@+33@*�H@*�!@*~�@*^5@*^5@*J@)��@)X@)&�@(Ĝ@(r�@(A�@( �@(  @'�w@'�P@'l�@'K�@';d@'
=@&�R@&ff@&E�@&$�@&$�@%�T@%@%��@%O�@%�@$��@$�/@$��@$j@$j@$I�@#��@#t�@#C�@#@"��@!��@!�7@!G�@ ��@ A�@�;@�@|�@
=@�R@��@v�@V@E�@��@��@O�@�@��@z�@(�@�@1@ƨ@S�@"�@�H@�!@�\@n�@M�@J@J@�@��@�`@%@�`@�9@�u@r�@Q�@A�@  @|�@K�@ȴ@ff@$�@��@�@�@�T@�T@@�-@�h@�@�@/@�/@�@I�@1@��@��@�m@�
@�F@��@S�@o@�@�@��@�\@M�@=q@-@J@�#@��@�7@x�@x�@hs@X@�@��@��@��@�9@�@r�@bN@bN@A�@  @�@�;@�@;d@��@�y@ȴ@�+@ff@E�@5?@$�@�-@p�@?}@V@�/@�@�D@Z@�@��@��@ƨ@�F@��@��@��@��@t�@@
�!@
��@
��@
��@
�!@
�!@
�!A�5?A�33A�5?A�7LA�5?A�/A�5?A�7LA�33A�33A�7LA�5?A�33A�5?A�9XA�5?A�5?A�5?A�9XA�9XA�5?A�5?A�5?A�9XA�7LA�5?A�7LA�9XA�5?A�7LA�;dA�;dA�5?A�7LA�;dA�9XA�7LA�7LA�;dA�9XA�7LA�7LA�;dA�9XA�7LA�9XA�=qA�9XA�7LA�9XA�;dA�9XA�7LA�9XA�;dA�;dA�9XA�9XA�=qA�=qA�9XA�9XA�;dA�=qA�;dA�9XA�;dA�=qA�;dA�;dA�?}A�?}A�;dA�9XA�;dA�?}A�;dA�;dA�?}A�?}A�=qA�;dA�?}A�?}A�;dA�;dA�?}A�?}A�;dA�;dA�?}A�A�A�=qA�=qA�A�A�=qA�;dA�;dA�A�A�A�A�;dA�9XA�=qA�?}A�=qA�9XA�9XA�=qA�?}A�;dA�;dA�?}A�A�A�=qA�;dA�A�A�A�A�?}A�;dA�=qA�C�A�A�A�;dA�=qA�?}A�?}A�=qA�=qA�?}A�A�A�?}A�=qA�A�A�A�A�=qA�?}A�C�A�A�A�=qA�A�A�E�A�C�A�A�A�?}A�C�A�C�A�?}A�A�A�C�A�C�A�?}A�A�A�C�A�E�A�C�A�A�A�C�A�G�A�C�A�A�A�C�A�G�A�G�A�E�A�C�A�E�A�G�A�G�A�C�A�C�A�I�A�G�A�A�A�?}A�C�A�E�A�A�A�A�A�C�A�E�A�E�A�?}A�=qA�A�A�A�A�?}A�=qA�A�A�C�A�A�A�=qA�?}A�A�A�A�A�=qA�7LA�5?A�7LA�33A�$�A��A��AԼjAԟ�A�jA��AӓuA�E�A�"�A�VA��mA��HA��;A��A���A��
A��A���A���A���A���A���A�ƨA�A���AҴ9Aҧ�Aҩ�AҴ9AҶFAҬAҡ�Aҙ�Aҕ�AҋDA�v�A�n�A�jA�hsA�ZA�&�A�A�  A�$�A�5?A�5?A�{AѓuAщ7A�|�A�|�A�z�AсAыDAщ7AсAщ7AэPAэPAщ7AыDAэPAѓuAёhAэPAѕ�Aя\AѓuAї�Aя\AуAхAыDAхAуA�~�AуAуA�|�A�|�AхAщ7AѓuAя\Aя\AѓuAѓuAѓuAѕ�Aѕ�Aї�Aљ�Aћ�Aћ�Aѕ�AёhAщ7Aч+AсA�|�A�~�A�~�A�|�A�t�A�jA�bNA�`BA�\)A�VA�K�A�C�A�A�A�9XA�&�A��mAЕ�A�|�A�t�A��A���A�M�A�VAΙ�A�=qA�  A�~�A̛�AˑhA�XA˺^A�-A�7LA�/A�E�A��A��Aƥ�A�ffA�oAōPA�O�AÅA��TA��A�n�A��A��yA�5?A�
=A�+A��PA���A�;dA���A���A�E�A�?}A�+A��mA��9A�r�A��A��jA��A�S�A���A��+A�bNA�A�A��A��TA���A�t�A���A��hA�ZA�A�A�/A��A�A��TA���A�O�A��9A�jA��A�%A�+A��A�VA�dZA�~�A�~�A��PA���A���A���A��PA�^5A���A�~�A�
=A���A��9A�p�A�{A��A��A���A��A���A��\A�n�A�Q�A�E�A�E�A�r�A�l�A��mA���A���A��DA�dZA�/A��mA��FA��hA�l�A�O�A�33A�bA��#A��\A�=qA�JA���A�&�A�A���A�ĜA���A�jA�$�A��A�/A�1'A�5?A�5?A�33A�+A�$�A�(�A�$�A��A�1A��A��PA�"�A���A��PA�t�A�dZA�Q�A�Q�A�M�A�I�A�7LA� �A�bA��A��
A���A��A�VA�$�A���A�~�A���A���A�K�A�=qA�C�A�ffA�x�A�G�A�VA���A�ZA�%A���A�9XA���A���A��RA��-A���A�r�A��A��A���A�t�A�I�A� �A���A���A�{A��DA�JA���A���A�XA�"�A�A��A���A��FA���A�ZA��TA�~�A�9XA���A�bNA� �A�A���A�A�/A��wA�/A���A�9XA��yA���A�=qA��A��\A�1'A��A��-A�A�t�A��
A���A���A�hsA�O�A�A�A�$�A�VA�A�ƨA��A�VA�dZA�A�A���A�ƨA�1A�?}A�A��A�M�A�+A�bA��A��A���A�A�VA���A���A���A�Q�A�
=A��9A�5?A~�jA~ZA~1A|��A{�hAxz�Au�#At-Ar��Aq�Ap�Ao�
Ao��Ao�AoO�An�HAnr�AnAm�
Am�PAm?}Al��Al�/Al��Al�Al�AlbNAl-AlJAk�mAk��Akl�AkG�Ak33Ak"�Aj�Aj�RAjffAi�Ai�Ail�Ah��Ah9XAhJAg��Af�+AfbNAe�Ae��AeS�Ad�yAd$�Ac�mAc��Ac��Ac�PAcXAb��AbbNA`�jA^��A^�+A^v�A^z�A^~�A^z�A^z�A^v�A^r�A^^5A^ffA^n�A^jA^jA^n�A^ffA^ffA^Q�A^5?A]�TA\ĜA\Q�A\(�A[��A[+A[�A[AZ�yAZĜAZ��AZjAZAZ  AY�#AY�AXn�AX1AWx�AWl�AW%AV{AUl�AT�ASt�ARv�AQG�AP��AO��AO�hAOdZAO7LAO&�AOVAO%AOAN��AOAN��AN�HAN�ANZANjAM�AM��AMoAL�\AL1'AK�PAJ�+AJ{AI��AI��AIO�AI/AH��AH��AH�AHM�AH�AG�;AG��AGS�AG+AG%AF�HAFȴAFȴAF�9AF��AF~�AFVAF5?AF$�AFbAFAE�AE�AE�mAE�TAE�;AE�-AE��AE��AE��AE��AE�PAE�AE�AEp�AEXAEG�AEC�AE;dAE7LAE/AE"�AE"�AEVAE%AEAD��AD�`AD�jAD��AD��ADr�AD1'AD1AC�AC�FAC�AChsACO�AC33AC&�AB�yABjAB$�AA��AA�^AA;dA@A�A?��A?�^A?��A?�hA?x�A?\)A?7LA?�A>�A>��A>v�A>1'A=��A=�-A=��A=�7A=x�A=p�A=C�A=;dA=/A=+A=A<��A<��A=A<��A<��A<��A<��A<�uA<�+A<�DA<�DA<v�A<bNA<VA<=qA<1'A<JA;�A;�;A;��A;�
A;ƨA;�A;�A;p�A;hsA;hsA;`BA;O�A;S�A;/A:�yA:ȴA:M�A9�7A97LA8�`A8��A8�A8r�A8ZA89XA8�A7�;A7ƨA7XA6�/A6VA5�A5�-A5hsA4��A4�DA4 �A3�A2��A2�A1p�A1&�A0��A0�A0��A0�9A0��A0��A0�+A0r�A0I�A0bA/�
A/��A/&�A.�`A.��A.�!A.�DA.ffA.(�A.  A-�TA-��A-`BA-XA-XA-G�A-;dA-&�A-�A,��A,�A,�A,ȴA,��A,��A,�uA,�A,v�A,z�A,v�A,ffA,ZA,ZA,ZA,ZA,ZA,M�A,=qA,9XA,5?A,5?A,1'A,5?A,5?A,1'A,5?A,9XA,9XA,1'A,-A, �A,�A,{A,JA,A+�A+�
A+��A+dZA+&�A+%A*�A*�A*��A*�!A*�A*�!A*�!A*�A*��A*��A*ZA*5?A)�A)��A)x�A)x�A)�7A)��A)ƨA)�A)�A)��A*A)�^A)��A)�A)��A*�A*5?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                               A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�?}A�=qA�=qA�?}A�?}A�?}A�?}A�A�A�A�A�C�A�E�A�E�A�C�A�A�A�A�A�;dAԮA���A��TA��;A���A���A���A��A�;dA�p�A�%A�^5A�
=A�?}A�l�A�+A�/A�A�A�p�A��hA�jA��9A��\A���A�%A���A���A��jA��9A�XA�Q�A�VA��7A�I�A���A�G�A~=qAu&�Ao�TAn  AlĜAk�Ak
=AiK�Af�uAd(�A_�mA]O�AV��ANv�AH�+AF�AEG�AC�A@z�A=�TA<�yA<1'A;hsA9&�A6�+A1�;A/�mA-�A,�A,jA,9XA+�A*�jA)��A*ȴA+;dA*r�A)�#A*�DA+�A,bA+��A+A+dZA*�!A)�A(A�A(��A(�A)A( �A'|�A'�PA'`BA'K�A'/A&��A&z�A&�A%�FA%�A$��A$bNA$1A#p�A"��A"^5A!��A!;dA ��A M�A�;A��A/A�jAffAI�AE�AbAXA"�A�RAM�AbA�^A`BA��AZA=qAƨAp�AS�A�HA��A��A%A��A{A�
A�wAXA�A��A�jAn�AbA�PAK�An�AJA�A�;A��Ax�A��A�/A�9AZAƨAXA�AȴAn�A��A+A��A��A�Ar�A1A�wA��A;dA
�!A
�A
n�A
bNA
A�A
 �A
bA	�A	/A��A��A��Av�AJA��AdZAO�AK�A%A��A��An�A��A`BA�A��A�AZA�AƨA\)A
=AĜA�+A{A��AO�A�A �/A �jA ~�A A�@�l�@�-@�p�@�/@�&�@�(�@�
=@���@��T@�p�@��@�Z@��;@�C�@�v�@�p�@�@�dZ@�ȴ@�^5@�O�@�j@@�+@�+@���@�X@��`@�r�@��@�+@�$�@�h@�O�@��`@�z�@���@柾@�V@�`B@�9@�Q�@��@���@�@�X@�/@��`@�A�@�+@�$�@���@ܬ@��m@�K�@�ȴ@ڇ+@�E�@�@�p�@�G�@��@��`@�\)@�=q@թ�@Ձ@�p�@�O�@�&�@���@�j@��@�1@���@�K�@Ұ!@��@��`@��@ϕ�@���@ΰ!@Η�@�ff@���@�hs@�V@̃@��@��@�E�@��@���@�(�@ǅ@�@ƸR@�~�@�@�7L@�Q�@þw@��y@�^5@��@�X@�X@��`@�  @��
@��@��@�dZ@�33@��!@�$�@��@�Q�@�(�@�  @��;@���@�C�@�ȴ@�~�@�E�@��@�X@��@���@�z�@�1'@��w@�|�@���@�5?@�J@��T@�`B@��/@��@�r�@�1@��F@�"�@�V@��#@���@��^@�hs@�X@�7L@���@�1'@�ƨ@���@�\)@���@��-@���@�x�@�O�@�Ĝ@�9X@���@�dZ@���@�M�@�{@���@��#@�@���@��7@�x�@��`@�Ĝ@�Z@�  @���@�33@��@���@���@�V@�@��@��^@�G�@�&�@�V@���@�Ĝ@��j@��@���@�I�@�b@��;@���@��@�33@��@��\@��#@�x�@�7L@�V@���@���@���@�1@��@�;d@��@���@�n�@�J@���@��@�/@��@���@��@�Q�@��
@�@�=q@���@���@�%@���@��u@�j@�1'@��@�t�@�33@���@��y@��!@�^5@�$�@��@���@��h@�hs@���@��9@�bN@�b@���@�ƨ@���@���@��P@�\)@�33@�@���@��!@���@�-@���@�X@�V@��`@���@��9@�r�@�(�@��@��@��P@�dZ@�o@��@��!@�ff@�5?@��@��^@�x�@��@��@�j@�I�@�9X@� �@�ƨ@��P@�K�@�+@��@���@�5?@��@���@���@�hs@�?}@���@���@���@��9@�bN@�b@�1@���@���@�l�@�;d@�+@��@��y@��R@�V@�@��#@��^@��7@�G�@�V@��D@�1'@�  @��F@�t�@�C�@��@��!@�E�@�5?@�@��#@�@�X@�V@��/@�Ĝ@���@�I�@�1'@� �@�b@|�@~ȴ@~5?@}��@}��@}@|��@|Z@{�
@{t�@{S�@{C�@{"�@z�@z��@z~�@y�7@x�`@xĜ@xA�@w�@wl�@w;d@v��@vv�@vv�@vv�@vff@u��@u`B@t�@tj@s��@s�F@s�@r�H@rM�@q�@q�^@q�^@qX@p�@pb@o�w@o;d@nȴ@m�@m�@m`B@m/@l��@lz�@k�m@k�F@kC�@j��@jM�@i�#@ihs@i&�@i%@h��@hbN@h1'@g�@f�@f�+@f5?@e�T@e�h@e�@d�/@d��@dj@d(�@ct�@b��@b~�@a�^@ax�@aG�@`�u@_�w@^�R@^v�@]�@]`B@]�@\�@\�D@[�m@[dZ@Z��@Z��@Z�\@Y��@Y7L@XĜ@X�u@XQ�@X �@W�w@W
=@V�R@Vv�@V5?@U�-@U?}@T�@S�F@R�H@Rn�@R=q@Q��@Q7L@Q%@P��@PA�@Pb@O�P@O+@N��@N{@M?}@L�@L9X@L1@K�@J�@J�\@J^5@JM�@JJ@I��@IG�@H��@HĜ@Hr�@G�;@G+@F��@F$�@E�T@E�@D��@D��@DI�@D�@Cƨ@Co@B�\@BM�@A�^@A�@@r�@@b@?��@?��@?l�@?K�@?
=@>�R@>V@=�T@=�@=?}@=?}@=/@=�@<��@<�/@<��@<j@;��@;�F@;dZ@;C�@:�@:�@:~�@9��@9��@9�7@9G�@97L@9&�@9&�@8��@8A�@7�w@7|�@7�@6�@6�R@6ff@5�-@5p�@5?}@4��@4�D@4(�@3ƨ@3�@3t�@3"�@2��@2�\@2n�@2M�@2-@1��@1G�@1&�@1�@0��@0��@0Q�@0b@/�@/l�@/+@.�@.��@.ff@.$�@-��@-�-@-�-@-�h@-/@,��@,j@,Z@,I�@,9X@,(�@+��@+�
@+33@*�H@*�!@*~�@*^5@*^5@*J@)��@)X@)&�@(Ĝ@(r�@(A�@( �@(  @'�w@'�P@'l�@'K�@';d@'
=@&�R@&ff@&E�@&$�@&$�@%�T@%@%��@%O�@%�@$��@$�/@$��@$j@$j@$I�@#��@#t�@#C�@#@"��@!��@!�7@!G�@ ��@ A�@�;@�@|�@
=@�R@��@v�@V@E�@��@��@O�@�@��@z�@(�@�@1@ƨ@S�@"�@�H@�!@�\@n�@M�@J@J@�@��@�`@%@�`@�9@�u@r�@Q�@A�@  @|�@K�@ȴ@ff@$�@��@�@�@�T@�T@@�-@�h@�@�@/@�/@�@I�@1@��@��@�m@�
@�F@��@S�@o@�@�@��@�\@M�@=q@-@J@�#@��@�7@x�@x�@hs@X@�@��@��@��@�9@�@r�@bN@bN@A�@  @�@�;@�@;d@��@�y@ȴ@�+@ff@E�@5?@$�@�-@p�@?}@V@�/@�@�D@Z@�@��@��@ƨ@�F@��@��@��@��@t�@@
�!@
��@
��@
��@
�!@
�!G�O�A�5?A�33A�5?A�7LA�5?A�/A�5?A�7LA�33A�33A�7LA�5?A�33A�5?A�9XA�5?A�5?A�5?A�9XA�9XA�5?A�5?A�5?A�9XA�7LA�5?A�7LA�9XA�5?A�7LA�;dA�;dA�5?A�7LA�;dA�9XA�7LA�7LA�;dA�9XA�7LA�7LA�;dA�9XA�7LA�9XA�=qA�9XA�7LA�9XA�;dA�9XA�7LA�9XA�;dA�;dA�9XA�9XA�=qA�=qA�9XA�9XA�;dA�=qA�;dA�9XA�;dA�=qA�;dA�;dA�?}A�?}A�;dA�9XA�;dA�?}A�;dA�;dA�?}A�?}A�=qA�;dA�?}A�?}A�;dA�;dA�?}A�?}A�;dA�;dA�?}A�A�A�=qA�=qA�A�A�=qA�;dA�;dA�A�A�A�A�;dA�9XA�=qA�?}A�=qA�9XA�9XA�=qA�?}A�;dA�;dA�?}A�A�A�=qA�;dA�A�A�A�A�?}A�;dA�=qA�C�A�A�A�;dA�=qA�?}A�?}A�=qA�=qA�?}A�A�A�?}A�=qA�A�A�A�A�=qA�?}A�C�A�A�A�=qA�A�A�E�A�C�A�A�A�?}A�C�A�C�A�?}A�A�A�C�A�C�A�?}A�A�A�C�A�E�A�C�A�A�A�C�A�G�A�C�A�A�A�C�A�G�A�G�A�E�A�C�A�E�A�G�A�G�A�C�A�C�A�I�A�G�A�A�A�?}A�C�A�E�A�A�A�A�A�C�A�E�A�E�A�?}A�=qA�A�A�A�A�?}A�=qA�A�A�C�A�A�A�=qA�?}A�A�A�A�A�=qA�7LA�5?A�7LA�33A�$�A��A��AԼjAԟ�A�jA��AӓuA�E�A�"�A�VA��mA��HA��;A��A���A��
A��A���A���A���A���A���A�ƨA�A���AҴ9Aҧ�Aҩ�AҴ9AҶFAҬAҡ�Aҙ�Aҕ�AҋDA�v�A�n�A�jA�hsA�ZA�&�A�A�  A�$�A�5?A�5?A�{AѓuAщ7A�|�A�|�A�z�AсAыDAщ7AсAщ7AэPAэPAщ7AыDAэPAѓuAёhAэPAѕ�Aя\AѓuAї�Aя\AуAхAыDAхAуA�~�AуAуA�|�A�|�AхAщ7AѓuAя\Aя\AѓuAѓuAѓuAѕ�Aѕ�Aї�Aљ�Aћ�Aћ�Aѕ�AёhAщ7Aч+AсA�|�A�~�A�~�A�|�A�t�A�jA�bNA�`BA�\)A�VA�K�A�C�A�A�A�9XA�&�A��mAЕ�A�|�A�t�A��A���A�M�A�VAΙ�A�=qA�  A�~�A̛�AˑhA�XA˺^A�-A�7LA�/A�E�A��A��Aƥ�A�ffA�oAōPA�O�AÅA��TA��A�n�A��A��yA�5?A�
=A�+A��PA���A�;dA���A���A�E�A�?}A�+A��mA��9A�r�A��A��jA��A�S�A���A��+A�bNA�A�A��A��TA���A�t�A���A��hA�ZA�A�A�/A��A�A��TA���A�O�A��9A�jA��A�%A�+A��A�VA�dZA�~�A�~�A��PA���A���A���A��PA�^5A���A�~�A�
=A���A��9A�p�A�{A��A��A���A��A���A��\A�n�A�Q�A�E�A�E�A�r�A�l�A��mA���A���A��DA�dZA�/A��mA��FA��hA�l�A�O�A�33A�bA��#A��\A�=qA�JA���A�&�A�A���A�ĜA���A�jA�$�A��A�/A�1'A�5?A�5?A�33A�+A�$�A�(�A�$�A��A�1A��A��PA�"�A���A��PA�t�A�dZA�Q�A�Q�A�M�A�I�A�7LA� �A�bA��A��
A���A��A�VA�$�A���A�~�A���A���A�K�A�=qA�C�A�ffA�x�A�G�A�VA���A�ZA�%A���A�9XA���A���A��RA��-A���A�r�A��A��A���A�t�A�I�A� �A���A���A�{A��DA�JA���A���A�XA�"�A�A��A���A��FA���A�ZA��TA�~�A�9XA���A�bNA� �A�A���A�A�/A��wA�/A���A�9XA��yA���A�=qA��A��\A�1'A��A��-A�A�t�A��
A���A���A�hsA�O�A�A�A�$�A�VA�A�ƨA��A�VA�dZA�A�A���A�ƨA�1A�?}A�A��A�M�A�+A�bA��A��A���A�A�VA���A���A���A�Q�A�
=A��9A�5?A~�jA~ZA~1A|��A{�hAxz�Au�#At-Ar��Aq�Ap�Ao�
Ao��Ao�AoO�An�HAnr�AnAm�
Am�PAm?}Al��Al�/Al��Al�Al�AlbNAl-AlJAk�mAk��Akl�AkG�Ak33Ak"�Aj�Aj�RAjffAi�Ai�Ail�Ah��Ah9XAhJAg��Af�+AfbNAe�Ae��AeS�Ad�yAd$�Ac�mAc��Ac��Ac�PAcXAb��AbbNA`�jA^��A^�+A^v�A^z�A^~�A^z�A^z�A^v�A^r�A^^5A^ffA^n�A^jA^jA^n�A^ffA^ffA^Q�A^5?A]�TA\ĜA\Q�A\(�A[��A[+A[�A[AZ�yAZĜAZ��AZjAZAZ  AY�#AY�AXn�AX1AWx�AWl�AW%AV{AUl�AT�ASt�ARv�AQG�AP��AO��AO�hAOdZAO7LAO&�AOVAO%AOAN��AOAN��AN�HAN�ANZANjAM�AM��AMoAL�\AL1'AK�PAJ�+AJ{AI��AI��AIO�AI/AH��AH��AH�AHM�AH�AG�;AG��AGS�AG+AG%AF�HAFȴAFȴAF�9AF��AF~�AFVAF5?AF$�AFbAFAE�AE�AE�mAE�TAE�;AE�-AE��AE��AE��AE��AE�PAE�AE�AEp�AEXAEG�AEC�AE;dAE7LAE/AE"�AE"�AEVAE%AEAD��AD�`AD�jAD��AD��ADr�AD1'AD1AC�AC�FAC�AChsACO�AC33AC&�AB�yABjAB$�AA��AA�^AA;dA@A�A?��A?�^A?��A?�hA?x�A?\)A?7LA?�A>�A>��A>v�A>1'A=��A=�-A=��A=�7A=x�A=p�A=C�A=;dA=/A=+A=A<��A<��A=A<��A<��A<��A<��A<�uA<�+A<�DA<�DA<v�A<bNA<VA<=qA<1'A<JA;�A;�;A;��A;�
A;ƨA;�A;�A;p�A;hsA;hsA;`BA;O�A;S�A;/A:�yA:ȴA:M�A9�7A97LA8�`A8��A8�A8r�A8ZA89XA8�A7�;A7ƨA7XA6�/A6VA5�A5�-A5hsA4��A4�DA4 �A3�A2��A2�A1p�A1&�A0��A0�A0��A0�9A0��A0��A0�+A0r�A0I�A0bA/�
A/��A/&�A.�`A.��A.�!A.�DA.ffA.(�A.  A-�TA-��A-`BA-XA-XA-G�A-;dA-&�A-�A,��A,�A,�A,ȴA,��A,��A,�uA,�A,v�A,z�A,v�A,ffA,ZA,ZA,ZA,ZA,ZA,M�A,=qA,9XA,5?A,5?A,1'A,5?A,5?A,1'A,5?A,9XA,9XA,1'A,-A, �A,�A,{A,JA,A+�A+�
A+��A+dZA+&�A+%A*�A*�A*��A*�!A*�A*�!A*�!A*�A*��A*��A*ZA*5?A)�A)��A)x�A)x�A)�7A)��A)ƨA)�A)�A)��A*A)�^A)��A)�A)��A*�A*5?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�mB�8B�mB�B�8B�8B�8B�B�8B�B�8B�B�B�B��B�B��B�B�B�mB��B�B��B�>B�sB�sB�sB�>B�>B�B�yB�B�yB�B_BK^Bd&BzDB	[�B
_B
"hB
2�B
U�B
ںBPHB(�BB[B5B�PB��B%�B!-B�B�BAB�B�NB��B�LB��B~�Be`B@OB�B
��B
��B
��B
:�B
-CB
&�B
	7B	�jB	�RB	�SB	��B	�B	x�B	qB	`B	K�B	3�B	�B��B�B��B��B��B��B�hBǮB�HB��B�EB�KB�B�B͟B��BݘB�`B��B�yB�"B	B	O�B	w�B	}�B	�{B	�=B	��B	�B	�B	�vB	�B	�5B	�DB	�HB
1B

rB
 B
(B
"B
1B
-�B
4�B
:�B
>�B
>�B
AUB
A�B
E�B
E�B
F�B
G�B
J�B
L0B
M6B
N�B
PHB
PHB
PB
OvB
OB
O�B
N�B
M�B
M�B
M�B
QB
P�B
Q�B
V9B
UgB
T�B
S�B
T�B
S�B
S&B
Q�B
R B
O�B
OB
OvB
OB
S�B
RTB
Q�B
R�B
P�B
P�B
O�B
NB
M�B
NpB
NB
N�B
K�B
K^B
F�B
EB
E�B
E9B
D�B
D�B
B'B
C�B
EB
E�B
F?B
D�B
C�B
D�B
D3B
EB
A�B
A�B
B'B
AUB
@�B
D3B
D�B
DgB
E�B
E9B
D�B
D3B
C�B
C�B
CaB
B�B
B�B
DgB
C-B
B�B
B[B
B�B
A�B
B'B
@�B
@B
?�B
A B
@B
?}B
?�B
>wB
=�B
<jB
;�B
;0B
:�B
9�B
9�B
8�B
7�B
6�B
6FB
6zB
5?B
4�B
4nB
3�B
33B
2�B
2�B
2�B
1[B
0�B
/�B
/B
0!B
-wB
,=B
,qB
+kB
+B
*0B
)*B
(�B
'�B
'�B
&�B
%�B
$�B
$B
#:B
"�B
 �B
 �B
 'B
!B
�B
OB
�B
�B
B
�B
~B
~B
xB
�B
CB
�B
�B
7B
eB
+B
�B
�B
�B
�B
B
�B
�B
�B
MB
�B
B
@B
@B
�B
:B
B
oB
�B
4B
�B
bB
�B
�B
�B
\B
(B
�B
�B
�B
�B
�B
�B
PB
PB
�B
PB
�B
�B
�B
�B
�B
�B
B
PB
B
JB
B
�B
�B
�B
�B
B
�B
JB
�B
B
�B
B
B
�B
JB
�B
~B
PB
�B
~B
�B
�B
B
PB
�B
�B
�B
B
�B
�B
�B
"B
�B
�B
"B
�B
�B
VB
"B
VB
�B
�B
�B
�B
�B
�B
bB
hB
hB
 B
 B
�B
B
hB
�B
�B
�B
oB
�B
B
�B
�B
@B
oB
oB
�B
�B
�B
uB
�B
MB
MB
{B
{B
B
MB
�B
�B
�B
�B
$B
�B
�B
$B
�B
$B
�B
�B
_B
�B
�B
�B
�B
�B
�B
�B
1B
eB
�B
�B
�B
�B
7B
B
kB
7B
7B
B
B
�B
�B
�B
�B
�B
=B
�B
qB
CB
CB
xB
xB
CB
xB
CB
~B
B
~B
IB
B
B
�B
�B
�B
�B
OB
�B
�B
�B
�B
�B
�B
VB
!B
 'B
�B
�B
�B
�B
 �B
!bB
!�B
!�B
!�B
!�B
"�B
#B
#�B
$tB
$�B
$�B
%FB
%�B
&�B
&�B
'B
'RB
&�B
&�B
'RB
'B
'�B
($B
($B
($B
'�B
)�B
)�B
*�B
*eB
*�B
*eB
*�B
*�B
+6B
+kB
+kB
+�B
+kB
,=B
+�B
,=B
,�B
,qB
,qB
-B
-B
-�B
-�B
.IB
.IB
.B
.IB
.�B
/B
/B
/OB
/OB
/�B
0UB
0UB
0�B
0UB
0�B
0!B
1'B
1'B
0�B
0�B
1'B
1'B
1�B
1�B
2-B
2�B
2�B
2�B
2�B
2�B
33B
49B
4nB
4nB
4nB
4�B
4�B
5?B
6B
6B
6FB
6zB
6�B
6�B
6�B
6�B
5�B
6B
6�B
7B
7�B
8B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7LB
7�B
8�B
9$B
8�B
8�B
8�B
9�B
9�B
9�B
:^B
:^B
:^B
:^B
:�B
:^B
:�B
<B
;dB
;0B
;�B
;�B
<B
<B
<jB
<jB
<6B
<6B
<B
=B
=B
=<B
=qB
=�B
=�B
=�B
?HB
?HB
?}B
?HB
?B
?}B
@OB
@B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
CaB
C�B
C�B
CaB
CaB
C�B
CaB
C�B
C�B
D�B
DgB
D�B
D�B
EB
E9B
E9B
EmB
E9B
EmB
FtB
F?B
F?B
F�B
FtB
FtB
GzB
HB
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K)B
K^B
L�B
MB
L�B
MB
MB
MB
MjB
NB
M�B
M�B
NB
N<B
NB
NpB
N�B
N�B
OB
OBB
PB
P�B
P}B
P�B
QB
QB
Q�B
Q�B
Q�B
Q�B
R B
RTB
S[B
S�B
T�B
U2B
UgB
UgB
UgB
U�B
U�B
U�B
VB
VB
V9B
V�B
W
B
WsB
W�B
W�B
XB
XEB
XB
XyB
XB
XyB
YKB
YB
YB
Y�B
ZQB
Z�B
Z�B
[#B
[#B
[#B
[#B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]�B
]�B
^5B
^5B
^5B
]�B
^�B
_;B
_;B
_;B
_pB
_pB
_;B
_;B
_;B
`vB
`BB
`�B
aB
`�B
`�B
aHB
a�B
a�B
a�B
bB
b�B
b�B
c B
cTB
cTB
c�B
c�B
c�B
c�B
d&B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
f2B
f�B
f�B
gB
f�B
g8B
gB
gB
g8B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
iDB
iDB
jB
jKB
jB
jB
jKB
jKB
j�B
kQB
k�B
k�B
k�B
lWB
lWB
lWB
lWB
l�B
l�B
m)B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
ncB
n/B
ncB
o B
n�B
o5B
o5B
o5B
oiB
pB
poB
p�B
p�B
p�B
qB
qvB
rGB
rB
rGB
r|B
sMB
s�B
s�B
tTB
tTB
tB
tTB
tTB
t�B
t�B
uZB
uZB
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
w2B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
zxB
y�B
zDB
{�B
{�B
{�B
{�B
{�B
|B
|�B
|B
|�B
|�B
{B
{�B
|B
}VB
}�B
}�B
}�B
}�B
}VB
}�B
~(B
}�B
~�B
~(B
~�B
.B
�B
�B
�B
cB
�B
�B
�B
�4B
��B
�iB
��B
�iB
�B
��B
�B
�;B
�oB
��B
�AB
�B
�uB
�B
��B
�uB
��B
��B
�{B
��B
��B
��B
��B
�{B
��B
��B
�B
�B
�MB
�MB
�SB
�B
�SB
�SB
��B
��B
��B
��B
�YB
��B
��B
�+B
�+B
�_B
��B
��B
�1B
�fB
�fB
�fB
��B
��B
��B
��B
��B
��B
�B
��B
�	B
�=B
�	B
�=B
�=B
�rB
�7B�B��B�B��B�B�yB�mB�2B��B�
B�B�mB�
B�B�2B�mB��B��B�fB�fB�8B��B�8B�2B�8B�sB�B��B�B�mB�2B�B��B�B�fB��B�B��B�fB�B�sB�B�2B�B��B�B��B�B�
B�B�fB�B�
B�mB�2B�fB�mB��B�fB��B�B��B�B�B�B��B�B�2B��B�mB�2B��B�8B�
B�8B��B�8B�B�2B��B�B��B�2B�2B�
B�
B�2B�2B��B�B�2B�2B�B�B��B�8B�
B�
B�fB�2B�
B�B�B�B�8B��B�B�mB�B�>B�sB�8B�2B�
B�B�B��B�B�sB�
B�B�B�yB�B�mB�B�B��B��B�8B�
B�B��B�B�DB�B�B��B�B�sB�B��B�B�DB�>B��B�DB�B�>B��B�DB�B�
B�B�>B�DB�B�8B�B�DB�B�B�mB�>B�DB�sB�mB�mB�B�yB�8B�>B�B�B�yB�>B�B�KB�B��B��B�KB�B��B�B��B�B��B�B�DB��B�B�yB�DB�B�B��B��B�B�B�;B��BBfBB 'BCaBB'BB'BD�BK�BJ�BJ�BL0BM6BL0BJ�BL�BM6BK�BJ�BJ�BMjBL�BK^BL0BO�BOvBK�BK^BMBN�BPHBP�BR�BUgBU2BS�BP}BQ�B^�BffBd�BZ�BU�BRTBTaBp;Bo�Br�Bq�BrGBpoBn/Bo5Bq�BpBncBoiBp�Bo�BpBm]Bo BpBm�BpBn/Bm)BoiBr|Bp;Bn/BoiBo5BqABo�Bo5Bq�BrGBpBo5BlWBm�BncBm�Bm�Bl�Bl"Bl�Bl�Bk�Bj�Bj�Bl�Bm]BncBl�Bm�Bm�Bl�BlWBl"Bk�BncBo BncBm�Bm]Bo5Bo5Bl�Bk�Bi�BrB�uB{Bv�B�%B��B��B�xB�*B�kB�UB�B��B��B	AB��B�B	B	#�B	C�B	*�B	K�B	F�B	I�B	S�B	m�B	��B	��B	�FB	�HB	�vB	�"B
B	� B	��B
�B
{B
�B	��B
�B
\B
B
�B
�B
�B
uB
�B
�B
"�B
!�B
%�B
*0B
9�B
'�B
(�B
*eB
,=B
3�B
2�B
=�B
6zB
6zB
/�B
0�B
/�B
1�B
2aB
;�B
9�B
]/B
Q�B
K�B
U�B
]�B
M�B
�iB
�uB
��B
��B
��B
��B
�pB
�dB
��B4B1'B0�BN<BRTBdZBlWBa|BXyBGBB�B1�B#nB!bBxB \B~B$tB7BA�BH�BB�BC�BGEBO�BL0BEB>�B=�B:*B6FB4�B5tB4B:^B.IB.�B3hB7�B9�B^Bb�Bg8Bi�Bv�B��B��B�%B�`B�fB�8B��B�"B�B�B(B�B!bB#�B7�B,�B&�B'B&LB$@B \B#:B�B �B \B�B�B:B�B
=B�B;BB�`B�B��B��B�[BخB�fB��B�B�B��B�B�B%B iB�(B�`B�5B��B�;B�vB�B�B��B�ZB�B�9B�aBרB�B��B�gB��B��B�aB�kB�4B��B��B�CB�7B�IB�!B�MB�7B�oB�{B{�ByrBt�B��B}�BncBo BgBW
BU�BNBGB>�B?�B5B8�B2�B�B�B�B�B
�B
�aB
ѷB
�EB
̘B
��B
��B
��B
��B
��B
��B
�	B
�B
��B
ncB
iDB
MB
A�B
9$B
6B
4�B
0�B
+�B
*0B
+�B
/�B
5B
.}B
-�B
&�B
!-B
!�B
�B
@�B	�B	�2B
1B	�ZB
�B	�B	��B	��B	��B	��B	�OB	��B	��B	��B	�B	�1B	�qB	��B	�{B	��B	��B	�lB	�B	��B	��B	��B	��B	~�B	�iB	��B	~�B	~�B	w2B	zDB	x�B	s�B	y�B	m]B	m�B	q�B	v`B	m)B	c�B	l"B	^B	X�B	U�B	W�B	W
B	R B	\)B	F�B	D�B	B�B	@�B	D3B	?B	FB	_�B	?}B	'�B	'B	#�B	#nB	#:B	"�B	#�B	#�B	&B	!�B	!�B	!�B	 'B	OB	�B	�B	=B	1B	!�B	7�B	2�B	�B	:B	 B	JB	
=B		�B	�B	�B	�B	JB�(B	 �B	�B		B	�B	�B��B��B�cB�B��B�B��B�"B�B��B��B�TB�B̘B�)B��B��B��B�#BȴB˒BǮB�mBÖB��B�^B��B�NBȴBɆB��B��B�B�}B��B��B�BB��B�*B�<B�jB��B�gB�3B�BB��B��B��B��B��B��B��B�B�6B��B��B��B��B�RB�^B��B��B��B��B��B�B��B��B��B�B��B�*B��B��B��B�RB�RB�B�zB�tB��B��B��B��B�LB��B�B�FB�B��B��B��B�3B�-B�'B�!B�B��B��B��B�6B��B��B��B�9B��B�B�hB��B�?B�B�^B�qB��B��B��BǮB�jB�mB�RBȀB��B�B�B�B�B�pB�BB�vB��BΥB��B��B��B�
BԕB� B��B�2B��B��BרB��B�mB�KBרBٴB�9B՛B�BیBٴB�EB��B֡BרB��BںB�B��B� B�B�B��B�5B�B��B�/B�iB�5B�iB�)B�/B�B�B�mB�B��B�B�B� B�B��B�B��BҽB�&B� B�B�B͟B̘B��B�B��B��B�6B��B�BɺB�KB�)B�#B˒B�pB�vB�HB��B�WB��B�yB�B��BیB��B�#B�]B��BޞB�B�vB�B�,B�`B�TB�B�2B�2B�2B�B��B��B��B�fB�fB�2B�`B��B�B�B��B��B��B�B��B�sB��B�B�B�sB�sB�>B��B��B�cB�B�B��B��B�WB�B��B��B�KB�B��B��B�GB��B�B�%B��B�|B�lB�rB	�B	�B	hB	�B	B	1�B	,�B	-CB	)�B	<jB	?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                               B�B�mB�B�mB�B�8B�8B�B�B�8B�B�8B�B�B�B��B�B��B�B�B�mB��B�B��B�>B�sB�sB�sB�>B�>B�B�yB�B��B��BNBQhBoiB�B	��B
�B
3hB
G�B
b�B
��B]IB2�BLdB@iB��B�B+kB&�BsB��BJB�B��B��B��B�hB�6BsMBU2B(�B
�B
�mB
�B
>wB
3�B
;B
%�B	�iB	��B	�kB	��B	�SB	~�B	y�B	h�B	YB	>�B	2�B	SB��B�1B��B��B��B�6B�B��B�B��B�B��B��B�{BՁB�pB�B�RB�B�5B��B	NB	zB	}B	�;B	�B	��B	�B	�]B	��B	�`B	�3B	�)B	��B
�B

�B
�B
NB
<B
�B
.B
5ZB
<�B
?}B
@OB
B�B
C�B
F�B
F�B
H1B
J	B
MB
M�B
O\B
P�B
RTB
Q�B
Q�B
PbB
P�B
Q�B
O�B
N<B
NB
N�B
S[B
Q�B
S@B
W�B
VSB
VB
UB
V�B
UgB
S�B
S�B
S@B
PHB
P�B
P�B
RB
VSB
S�B
S�B
S�B
QhB
R B
P�B
N�B
N�B
O�B
OvB
PbB
M6B
N"B
G�B
EmB
E�B
E�B
FB
FtB
B�B
D�B
FYB
G�B
G�B
E�B
E9B
E�B
F�B
GEB
C{B
B�B
BuB
A�B
BuB
E9B
ESB
E�B
G�B
E�B
EB
DgB
D�B
DMB
C�B
C{B
E9B
E9B
C�B
CaB
CB
DMB
CaB
CB
AB
@OB
@�B
BB
@�B
@iB
AoB
@�B
?cB
=qB
<jB
;�B
;�B
;JB
;B
9�B
8�B
7�B
7�B
88B
6`B
5�B
5ZB
4TB
4B
4B
4�B
4�B
2�B
1B
0B
0�B
2B
.IB
-�B
-CB
,qB
,"B
+B
*eB
*B
)�B
)DB
(�B
'B
%�B
%�B
$�B
$B
!|B
!�B
!�B
�B
�B
!B
�B
�B
�B
�B
B
OB
IB
�B
OB
WB
]B
qB
B
�B
�B
	B
_B
$B
�B
?B
�B
�B
B
�B
mB
aB
,B
[B
�B
�B
B
�B
�B
hB
&B
�B
�B
B
�B
vB
BB
�B
�B
(B
"B
B
VB
�B
pB
BB
�B
�B
�B
B
�B
6B
6B
B
�B
jB
PB
�B
�B
~B
jB
�B
"B
6B
PB
�B
�B
�B
�B
�B
�B
�B
jB
VB
B
jB
.B
�B
jB
�B
6B
PB
�B
"B
�B
B
VB
pB
B
pB
�B
vB
(B
�B
�B
vB
�B
.B
B
.B
�B
bB
�B
oB
�B
hB
B
�B
oB
�B
�B
�B
�B
�B
�B
@B
�B
@B
uB
�B
�B
{B
{B
B
�B
B
�B
�B
�B
�B
B
SB
�B
�B

B
�B
�B
$B
?B
YB
$B
sB
$B
�B
�B
�B
KB
�B
�B
B
eB
KB
�B
B
�B
B
�B
�B
kB
7B
�B
QB
QB
7B
�B
=B
	B
	B
#B
qB
�B
qB
�B
�B
�B
�B
�B
�B
�B
dB
5B
B
B
�B
�B
�B
jB
B
OB
B
�B
;B
�B
 �B
!B
 �B
 �B
�B
 'B
 �B
�B
 B
 BB
 vB
!|B
!�B
"4B
!�B
!�B
"hB
#B
#�B
$&B
$�B
%B
%zB
%�B
&�B
'B
'B
'�B
'�B
'B
'B
'�B
'mB
($B
(�B
(sB
(sB
(�B
*�B
*B
+QB
*�B
*�B
*�B
+B
+6B
+�B
+�B
+�B
,B
,B
,�B
,=B
,�B
-B
,�B
-)B
-�B
.B
./B
.cB
.�B
.cB
.IB
/ B
/OB
/�B
/iB
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1vB
1AB
0�B
1'B
1�B
1AB
1�B
2-B
2�B
3hB
2�B
2�B
33B
3hB
3�B
4�B
4�B
4�B
4�B
5ZB
5ZB
6FB
6�B
6zB
6�B
6�B
7B
7B
7�B
7fB
5�B
6zB
72B
7fB
8RB
8�B
8RB
7�B
8B
8RB
8B
7�B
7�B
7�B
8�B
9	B
9�B
8�B
9	B
9rB
:*B
:DB
:^B
:xB
:xB
:xB
:�B
:�B
:�B
;B
<�B
;�B
;�B
<6B
;�B
<6B
<�B
<�B
<jB
<6B
<PB
<�B
=qB
=�B
=�B
=�B
=�B
=�B
>BB
?�B
?�B
?�B
?cB
?}B
@OB
@�B
@�B
AB
AB
A�B
B'B
A�B
A�B
A�B
BAB
CB
B�B
CB
CaB
C{B
C�B
C�B
DB
C�B
C�B
DB
C�B
DB
D�B
D�B
D�B
D�B
E9B
EmB
E�B
E�B
E�B
E�B
F%B
GB
F�B
F�B
F�B
F�B
G+B
HKB
IB
IB
IB
J	B
I�B
I�B
J#B
JXB
KB
KDB
J�B
KDB
K�B
MPB
MjB
MB
MPB
M6B
MjB
N"B
NVB
N"B
M�B
N�B
N�B
N�B
O\B
O�B
OBB
OBB
O�B
P�B
P�B
P�B
QNB
QNB
Q�B
Q�B
RB
R B
R�B
R�B
R�B
S�B
TFB
U2B
U�B
U�B
U�B
U�B
VB
V9B
VB
V9B
VmB
V�B
WYB
W�B
W�B
W�B
XB
X�B
XyB
X_B
X�B
XyB
Y1B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[=B
[WB
[WB
[WB
[qB
[�B
[�B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
]/B
]�B
]�B
]�B
^jB
^�B
^OB
^OB
_pB
_pB
_VB
_�B
_�B
_�B
_;B
_pB
_�B
`�B
`�B
aB
aHB
aB
aB
a�B
a�B
a�B
b4B
b�B
c B
cTB
cnB
cnB
c�B
d&B
c�B
dB
dB
dZB
d�B
eFB
eB
d�B
d�B
eB
eFB
eFB
e�B
e�B
fLB
f�B
gB
gB
gRB
f�B
gRB
gB
g8B
g�B
h>B
h>B
h�B
h�B
h�B
h�B
iB
iyB
i�B
jeB
jB
jKB
j�B
jeB
j�B
kQB
k�B
k�B
k�B
l=B
l�B
lqB
l�B
l�B
m)B
mB
mCB
mB
m]B
m�B
nB
nB
nB
m�B
n/B
n}B
ncB
n�B
o5B
n�B
oOB
o�B
oiB
oiB
p;B
p�B
q[B
p�B
q'B
qvB
rGB
r�B
raB
r�B
sB
s�B
s�B
tB
t�B
t�B
t9B
t�B
tnB
t�B
u%B
u�B
u�B
v`B
v�B
v�B
v�B
v�B
v�B
wB
wfB
v�B
v�B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
y>B
{B
y�B
zxB
|B
|B
|B
|B
|B
|jB
}B
|PB
}B
}"B
{�B
|6B
|B
}VB
}�B
}�B
}�B
}�B
}qB
}�B
~(B
~B
B
~]B
cB
}B
�B
�B
�B
}B
�B
�B
�B
��B
��B
�iB
��B
��B
�;B
��B
� B
�UB
��B
�B
�[B
�'B
�uB
�'B
��B
��B
��B
��B
�{B
��B
�B
��B
��B
�{B
��B
�3B
�3B
�3B
��B
��B
��B
�9B
�mB
��B
��B
��B
�B
�B
��B
�B
�+B
�_B
�_B
��B
��B
��B
�fB
��B
�fB
��B
��B
��B
��B
��B
��B
�B
�lB
�#B
�#B
�=B
�	B
�=B
�=B
�rG�O�B�B��B�B��B�B�yB�mB�2B��B�
B�B�mB�
B�B�2B�mB��B��B�fB�fB�8B��B�8B�2B�8B�sB�B��B�B�mB�2B�B��B�B�fB��B�B��B�fB�B�sB�B�2B�B��B�B��B�B�
B�B�fB�B�
B�mB�2B�fB�mB��B�fB��B�B��B�B�B�B��B�B�2B��B�mB�2B��B�8B�
B�8B��B�8B�B�2B��B�B��B�2B�2B�
B�
B�2B�2B��B�B�2B�2B�B�B��B�8B�
B�
B�fB�2B�
B�B�B�B�8B��B�B�mB�B�>B�sB�8B�2B�
B�B�B��B�B�sB�
B�B�B�yB�B�mB�B�B��B��B�8B�
B�B��B�B�DB�B�B��B�B�sB�B��B�B�DB�>B��B�DB�B�>B��B�DB�B�
B�B�>B�DB�B�8B�B�DB�B�B�mB�>B�DB�sB�mB�mB�B�yB�8B�>B�B�B�yB�>B�B�KB�B��B��B�KB�B��B�B��B�B��B�B�DB��B�B�yB�DB�B�B��B��B�B�B�;B��BBfBB 'BCaBB'BB'BD�BK�BJ�BJ�BL0BM6BL0BJ�BL�BM6BK�BJ�BJ�BMjBL�BK^BL0BO�BOvBK�BK^BMBN�BPHBP�BR�BUgBU2BS�BP}BQ�B^�BffBd�BZ�BU�BRTBTaBp;Bo�Br�Bq�BrGBpoBn/Bo5Bq�BpBncBoiBp�Bo�BpBm]Bo BpBm�BpBn/Bm)BoiBr|Bp;Bn/BoiBo5BqABo�Bo5Bq�BrGBpBo5BlWBm�BncBm�Bm�Bl�Bl"Bl�Bl�Bk�Bj�Bj�Bl�Bm]BncBl�Bm�Bm�Bl�BlWBl"Bk�BncBo BncBm�Bm]Bo5Bo5Bl�Bk�Bi�BrB�uB{Bv�B�%B��B��B�xB�*B�kB�UB�B��B��B	AB��B�B	B	#�B	C�B	*�B	K�B	F�B	I�B	S�B	m�B	��B	��B	�FB	�HB	�vB	�"B
B	� B	��B
�B
{B
�B	��B
�B
\B
B
�B
�B
�B
uB
�B
�B
"�B
!�B
%�B
*0B
9�B
'�B
(�B
*eB
,=B
3�B
2�B
=�B
6zB
6zB
/�B
0�B
/�B
1�B
2aB
;�B
9�B
]/B
Q�B
K�B
U�B
]�B
M�B
�iB
�uB
��B
��B
��B
��B
�pB
�dB
��B4B1'B0�BN<BRTBdZBlWBa|BXyBGBB�B1�B#nB!bBxB \B~B$tB7BA�BH�BB�BC�BGEBO�BL0BEB>�B=�B:*B6FB4�B5tB4B:^B.IB.�B3hB7�B9�B^Bb�Bg8Bi�Bv�B��B��B�%B�`B�fB�8B��B�"B�B�B(B�B!bB#�B7�B,�B&�B'B&LB$@B \B#:B�B �B \B�B�B:B�B
=B�B;BB�`B�B��B��B�[BخB�fB��B�B�B��B�B�B%B iB�(B�`B�5B��B�;B�vB�B�B��B�ZB�B�9B�aBרB�B��B�gB��B��B�aB�kB�4B��B��B�CB�7B�IB�!B�MB�7B�oB�{B{�ByrBt�B��B}�BncBo BgBW
BU�BNBGB>�B?�B5B8�B2�B�B�B�B�B
�B
�aB
ѷB
�EB
̘B
��B
��B
��B
��B
��B
��B
�	B
�B
��B
ncB
iDB
MB
A�B
9$B
6B
4�B
0�B
+�B
*0B
+�B
/�B
5B
.}B
-�B
&�B
!-B
!�B
�B
@�B	�B	�2B
1B	�ZB
�B	�B	��B	��B	��B	��B	�OB	��B	��B	��B	�B	�1B	�qB	��B	�{B	��B	��B	�lB	�B	��B	��B	��B	��B	~�B	�iB	��B	~�B	~�B	w2B	zDB	x�B	s�B	y�B	m]B	m�B	q�B	v`B	m)B	c�B	l"B	^B	X�B	U�B	W�B	W
B	R B	\)B	F�B	D�B	B�B	@�B	D3B	?B	FB	_�B	?}B	'�B	'B	#�B	#nB	#:B	"�B	#�B	#�B	&B	!�B	!�B	!�B	 'B	OB	�B	�B	=B	1B	!�B	7�B	2�B	�B	:B	 B	JB	
=B		�B	�B	�B	�B	JB�(B	 �B	�B		B	�B	�B��B��B�cB�B��B�B��B�"B�B��B��B�TB�B̘B�)B��B��B��B�#BȴB˒BǮB�mBÖB��B�^B��B�NBȴBɆB��B��B�B�}B��B��B�BB��B�*B�<B�jB��B�gB�3B�BB��B��B��B��B��B��B��B�B�6B��B��B��B��B�RB�^B��B��B��B��B��B�B��B��B��B�B��B�*B��B��B��B�RB�RB�B�zB�tB��B��B��B��B�LB��B�B�FB�B��B��B��B�3B�-B�'B�!B�B��B��B��B�6B��B��B��B�9B��B�B�hB��B�?B�B�^B�qB��B��B��BǮB�jB�mB�RBȀB��B�B�B�B�B�pB�BB�vB��BΥB��B��B��B�
BԕB� B��B�2B��B��BרB��B�mB�KBרBٴB�9B՛B�BیBٴB�EB��B֡BרB��BںB�B��B� B�B�B��B�5B�B��B�/B�iB�5B�iB�)B�/B�B�B�mB�B��B�B�B� B�B��B�B��BҽB�&B� B�B�B͟B̘B��B�B��B��B�6B��B�BɺB�KB�)B�#B˒B�pB�vB�HB��B�WB��B�yB�B��BیB��B�#B�]B��BޞB�B�vB�B�,B�`B�TB�B�2B�2B�2B�B��B��B��B�fB�fB�2B�`B��B�B�B��B��B��B�B��B�sB��B�B�B�sB�sB�>B��B��B�cB�B�B��B��B�WB�B��B��B�KB�B��B��B�GB��B�B�%B��B�|B�lB�rB	�B	�B	hB	�B	B	1�B	,�B	-CB	)�B	<jB	?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<#�
<<c3=��=\�<��H<��<�F�<W�(<j�<W�(<#�
<)��<>fn<#�
<#�
<#�
<#�
<4��<#�
<)��<#�
<A�g<)��<#�
<A�g<n!�<f"�<��d<�4�<#�
<��:<�8<#�
<#�
<��<�Qf<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<_a�<7��<��N<�[�<�n�<#�
<#�
<#�
<DN�<#�
<#�
<#�
<#�
<#�
<#�
<t4=<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018121721223120181217212231IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010420190106200104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010420190106200104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551320190521075513IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                