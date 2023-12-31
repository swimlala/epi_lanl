CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-10-02T00:10:20Z creation; 2023-04-26T19:01:13Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20191002001020  20230426190113  5905276 5905276 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               ?   ?AA  AOAO7317_008645_063                 7317_008645_063                 2C  2C  DD  SOLO_II                         SOLO_II                         8645                            8645                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @����.@����.11  @�����"�@�����"�@&���}�3@&���}�3�c�Z��If�c�Z��If11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @@  @}p�@��R@\@��
AG�A��A   A,(�A@��A`  A�  A�  A��A��A��AϮA߮A�B (�B��B(�B  B (�B(  B/�
B7�
B?�
BG�BP  BW�
B_�
Bhz�Bo�
Bx  B�B��B�  B��B��B�  B�  B��
B��
B��
B��
B��B�  B��B�{B�{B�{B�  B��B��B�(�B�(�B�{B��B�  B�  B�  B�{B�(�B�(�B�{B�(�C   C��C
=C
=C  C
  C��C  C�C�C  C��C{C��C��C
=C 
=C"
=C$
=C&
=C(
=C*{C,  C.
=C0{C2{C4
=C6  C8  C:  C;��C>  C@
=CA��CD  CF
=CH  CJ  CL
=CN  CP  CQ��CT  CV
=CW��CY�C[��C^{C`{Cb
=Cd
=Cf{Ch
=Cj  Cl
=Cn
=Cp  Cr  Ct
=Cu��Cx  Cz{C|  C~{C�  C���C�  C�
=C�  C�  C�C�  C�C�C�  C�C���C�  C�  C���C���C���C���C�  C���C���C���C�C�  C���C�C�
=C�C���C���C�  C�C�C�C�C�  C�C�C�C�
=C�  C���C�  C���C���C�  C�  C�C�  C�  C�  C�  C�C���C�  C�C�  C���C�  C���C���C�  C�C�
=C�C���C���C���C�  C���C�  C�  C�  C���C���C�  C�C�  C���C���C�C�C�  C�  C�C�C�
=C�C�  C�C�
=C�
=C�  C�  C�C���C���C�C�  C���C���C���C�  C�C�C���C���C�  C�  C�  C�  C�C�  C�  C�C�  C���C���C���C�  C�  C���C���C���C���C���C�  C���D }qD �qD��DD� D  D��DD� D�qD� D�qD}qD��Dz�D�qD}qD	  D	�D
�D
}qD�D�DD�DD�D�D��D  D� D��D}qD�D��D  D� D�D��DD��DD�D  D� D  D� D�D� D�qD}qD�qDz�D�qD}qD�qD}qD  D� D  D� D  D� D�qD }qD ��D!}qD"  D"}qD#  D#��D$�D$��D%  D%}qD&  D&��D&�qD'z�D'��D(}qD(�qD)� D*�D*� D+  D+}qD+�qD,� D-D-��D-�qD.}qD/�D/��D/��D0z�D1  D1�D2�D2��D2��D3}qD4  D4�D5D5��D5�qD6� D7�D7}qD8  D8��D9  D9}qD:�D:� D;  D;��D;�qD<� D=  D=}qD=�qD>� D>�qD?}qD?��D@� DA�DA��DB�DB��DC  DC��DD�DD� DE  DE��DF�DF�DG�DG��DH  DH}qDH�qDI� DJ  DJ� DKDK��DL�DL� DL�qDM}qDN�DN�DO�DO��DP  DP}qDQ�DQ��DR  DR� DSDS�DS�qDT}qDU�DU��DV�DV��DW�DW��DX  DX� DY  DY� DZ�DZ��D[�D[��D[�qD\xRD]  D]�D^D^�D_  D_z�D_��D`}qDa�Da��Da�qDb}qDc�Dc��Dc�qDd� De  De��Df�Df�Dg  Dg}qDg�qDh� Di  Di��Di�qDjz�Dj�qDk� Dl�Dl��DmDm��Dm�qDn� Dn�qDoxRDo��Dp��Dq�Dq� Dr  Dr}qDr�RDsz�Dt�Dt�Du�Du}qDu��Dvz�Dw  Dw�Dx�Dx��Dy  Dy� Dz�Dz��D{D{��D{�qD|}qD|�qD}� D}�qD~}qD�D�D�  D�>�D��HD��HD�  D�>�D�~�D��qD���D�>�D�~�D�� D�  D�@ D�~�D��qD���D�AHD��HD��HD�HD�B�D��HD��HD�HD�@ D��HD�� D��qD�@ D��HD�� D���D�>�D�}qD�� D�HD�AHD���D��HD���D�@ D��HD�� D���D�=qD�~�D�� D�  D�@ D�� D�� D�HD�@ D�� D���D���D�AHD���D��HD���D�>�D�� D�� D���D�AHD��HD���D��qD�@ D�� D�� D�HD�@ D��HD�D�  D�@ D��HD���D���D�B�D�� D���D�  D�@ D��HD�� D��qD�AHD��HD�� D�HD�@ D�~�D�� D���D�@ D�~�D��qD�HD�@ D�~�D�D�HD�@ D��HD��HD�  D�@ D�~�D���D���D�@ D�� D���D���D�>�D�� D�� D���D�@ D�� D�� D�HD�>�D�� D��HD�HD�AHD�� D���D���D�=qD�~�D��HD��qD�@ D���D�� D���D�>�D�~�D��HD�HD�@ D��HD���D��qD�@ D��HD��HD�  D�=qD�~�D��HD��D�B�D���D�� D���D�>�D�~�D��HD�HD�B�D���D�� D���D�>�D�~�D���D�HD�AHD�� D��HD��D�AHD���D���D�HD�@ D�~�D�� D�HD�>�D�}qD���D�  D�>�D�}qD���D�HD�@ D�~�D�� D�  D�@ D�~�D�� D��qD�<)D�~�D��HD�HD�AHD�� D��qD���D�@ D�~�D�� D�HD�>�D�� D���D��qD�>�D�� D���D���D�@ D���D��HD�HD�AHD��HD�D�HD�AHD��HD��HD���D�>�D�~�D���D�  D�=qD�}qD���D��)D�=qD�� D��HD�  D�>�D�~�D��HD��D�B�D��HD���D�  D�AHD�� D���D�  D�AHD�~�D���D��qD�>�D�� D�� D�HD�AHD�~�D¾�D���D�>�DÀ D��HD�  D�@ DĀ D�� D�  D�AHDŁHDž�D�  D�@ Dƀ D�� D�  D�>�Dǀ D�� D�  D�AHDȁHDȾ�D���D�@ DɁHD��HD��D�@ D�~�D�� D���D�>�D�}qD�� D�HD�@ D̀ D�� D�  D�>�D�}qDͽqD��qD�=qD�~�D�� D�  D�AHDρHD�� D�HD�B�DЂ�D�� D���D�>�D�~�D�� D�  D�AHDҁHD��HD�  D�AHDӀ DӽqD���D�@ DԀ D�� D���D�@ DՀ Dվ�D�  D�@ D�~�D־�D���D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�>�DفHD�D�  D�AHDڀ Dھ�D�  D�AHDہHD۽qD��qD�@ D܀ D�� D�  D�=qD݀ D�D�HD�@ D�~�D�� D�  D�@ D߁HD�� D�  D�AHD��HD�� D�  D�@ D�}qD�� D�  D�=qD� D��HD�  D�@ DわD��HD���D�@ D�HD侸D���D�>�D�~�D�� D�HD�AHD�HD�� D���D�=qD�~�D�� D�  D�>�D�HD�D��D�AHD� D龸D��qD�=qD�}qD�qD���D�=qD�}qD�� D��D�AHD�HD�D�  D�@ D� D�� D�HD�>�D�}qDD�  D�AHD�HD��HD��qD�<)D�}qD�� D�HD�B�D�HD�qD��qD�>�D�HD��HD��qD�=qD�HD��HD���D�>�D�~�D���D�  D�@ D�� D��HD��D�AHD�� D�� D�  D�AHD��HD�� D�HD�B�D�� D���D���D�AHD��HD�D��D�*=?\)?��?aG�?�z�?�33?�G�@�@z�@+�@@  @Y��@p��@��
@���@�(�@���@�@�G�@���@ٙ�@��
@�\)@��HA33A
=qA��A
=Ap�A#�
A)��A.�RA5�A;�AAG�AG�AMp�AS�
AZ�HAaG�AhQ�An�RAu�Az�HA�Q�A��A��RA��A�A�G�A�(�A�
=A��A��A�  A��A�
=A�=qA�p�A���A��
A��RA��A���A�  A�33A�
=Aʏ\A�AУ�AӅA�{Aٙ�A��A��A�(�A�
=A��A���A�  A�33A�
=A�=qA�p�B z�B{B�B��BffB�
B	�B
�\B  Bp�B�\B�Bz�Bp�BffB\)B��B��B�\B�BQ�B�B�B�RB�B��BB�HB�B z�B!G�B"{B"�HB#�B$��B%B&�RB'�B(z�B)�B)�B*�\B+�B,z�B-p�B.ffB/33B/�
B0��B1p�B2=qB3
=B4  B4��B5��B6=qB6�HB8  B8��B9��B:ffB:�HB;�B<Q�B=G�B>{B>�HB?�B@(�B@��BAp�BBffBC\)BD  BDz�BE�BEBF�RBG�BH(�BH��BIp�BJffBK\)BLQ�BL��BMp�BN=qBO33BP(�BP��BQ��BR=qBR�HBT  BT��BU��BV{BV�RBW�BXz�BYp�BZ{BZ�RB[\)B\Q�B\��B^{B_
=B`  B`��Bap�Bb{Bc33BdQ�BeG�Bf{Bf�RBg�BhQ�Bh��BiBj�\Bk33Bk�Bk�
Bl  Blz�Bl��BmG�Bm��BmBmBm�BnffBn�RBo
=Bo33Bo\)Bo�Bo�Bp(�Bpz�Bp��Bq�Bq�Bqp�Bq�BrffBr�RBr�HBs
=Bs\)Bs�Bt(�Bt��Bu�BuG�Bup�Bu��Bv{Bv�\Bw
=Bw\)Bw�
Bx  Bx(�Bxz�Bx��By�By��Bz{Bz�\Bz�HB{
=B{33B{\)B|(�B|z�B|z�B|��B}�B}��B}�B~ffB~ffB~�\B~�HB\)B�
B�{B�{B�(�B�z�B��RB��HB���B�
=B�G�B���B��B�  B�{B�Q�B���B��HB�33B�p�B��B��
B�(�B��\B��RB���B�33B��B��B�=qB�ffB���B���B�p�B�B�{B�=qB��\B�
=B�p�B��B��B�=qB��RB�33B��B��B�=qB���B�33B���B�  B�Q�B��HB�\)B��
B�=qB��\B��B��B�=qB���B��B��B�=qB��HB�\)B��
B�ffB��B�B�(�B��RB�p�B�(�B���B��B��
B���B�
=B���B�Q�B��B���B�(�B��RB�p�B�(�B��\B��B��B��\B���B��B�Q�B�
=B��B�  B���B�\)B�  B�z�B���B���B�Q�B���B�\)B��
B��\B�33B�B�(�B��RB��B�{B�z�B�
=B��
B�z�B���B�p�B�=qB���B�p�B��B��RB�p�B��B�z�B�G�B��B�Q�B���B��B�Q�B���B�\)B�{B��RB��B��
B�z�B��BÙ�B�{B��HB�\)B��
B�ffB��B�B�=qBȣ�B�G�B�  B�ffB��HB˅B�(�Ḅ�B��B�B�ffB��HB�G�B�  BУ�B�
=BхB�=qB���B�33B�B�ffB��HB�G�B��B�ffBָRB�\)B�  B�Q�Bأ�B�\)B��
B�(�Bڣ�B�G�BۮB�{Bܣ�B�33B݅B�{Bޏ\B��HB߅B�{B�ffB��HB�B��
B�=qB��HB�\)B�B�(�B�RB�33B�p�B�{B�\B���B�p�B�  B�Q�B��B�G�B��
B�{B��B�33B�B��B�\B��HB�G�B��B�ffB��B�G�B��
B�{B��\B�33B�B�  B�\B���B�\)B�  B�z�B��RB�33B��B�=qB��RB�G�B�B�{B���B�33B���B�=qB��RB�
=B�B�=qB��\B�33B��B�  B���B�33B���C 
=C ffC ��C ��C�Cp�C��C�CG�Cz�CC�CffC��C��CQ�C�C�C33Cp�C�
C(�CffCC�C\)C�RC�C\)C��C	�C	ffC	�
C
33C
p�C
�
CQ�C�\C��C\)C�C
=Cz�C��C(�C��C�CQ�CC{Cz�C�C=qC��C{CffC�
CG�C��C  Cp�C��C(�C��C��C\)C��C(�C�C  C\)CC33C�\C�CffCC(�C��C�CQ�C��C�C�C  CG�C�RC33C�C�C \)C �C!�C!��C!�HC"\)C"C#�C#�\C$  C$G�C$C%(�C%z�C%�C&\)C&��C'(�C'p�C'�
C(Q�C(��C)  C)p�C)C*33C*��C*�HC+\)C+C,{C,z�C,�C-=qC-��C.�C.ffC.�
C/G�C/��C/��C0p�C0�RC133C1�C1�
C2Q�C2��C2��C3p�C3C4�C4�\C4�HC5=qC5�C6  C6\)C6��C7�C7�C7��C8=qC8��C9{C9ffC9C:=qC:�C:�C;\)C;��C<{C<z�C<�RC=(�C=�\C=�
C>G�C>��C>��C?p�C?�RC@�C@�\C@�
CA=qCA��CA�CB\)CB�RCC
=CCz�CC��CD(�CD��CD�CE=qCECF{CF\)CF�
CG33CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      ?u@   @@  @}p�@��R@\@��
AG�A��A   A,(�A@��A`  A�  A�  A��A��A��AϮA߮A�B (�B��B(�B  B (�B(  B/�
B7�
B?�
BG�BP  BW�
B_�
Bhz�Bo�
Bx  B�B��B�  B��B��B�  B�  B��
B��
B��
B��
B��B�  B��B�{B�{B�{B�  B��B��B�(�B�(�B�{B��B�  B�  B�  B�{B�(�B�(�B�{B�(�C   C��C
=C
=C  C
  C��C  C�C�C  C��C{C��C��C
=C 
=C"
=C$
=C&
=C(
=C*{C,  C.
=C0{C2{C4
=C6  C8  C:  C;��C>  C@
=CA��CD  CF
=CH  CJ  CL
=CN  CP  CQ��CT  CV
=CW��CY�C[��C^{C`{Cb
=Cd
=Cf{Ch
=Cj  Cl
=Cn
=Cp  Cr  Ct
=Cu��Cx  Cz{C|  C~{C�  C���C�  C�
=C�  C�  C�C�  C�C�C�  C�C���C�  C�  C���C���C���C���C�  C���C���C���C�C�  C���C�C�
=C�C���C���C�  C�C�C�C�C�  C�C�C�C�
=C�  C���C�  C���C���C�  C�  C�C�  C�  C�  C�  C�C���C�  C�C�  C���C�  C���C���C�  C�C�
=C�C���C���C���C�  C���C�  C�  C�  C���C���C�  C�C�  C���C���C�C�C�  C�  C�C�C�
=C�C�  C�C�
=C�
=C�  C�  C�C���C���C�C�  C���C���C���C�  C�C�C���C���C�  C�  C�  C�  C�C�  C�  C�C�  C���C���C���C�  C�  C���C���C���C���C���C�  C���D }qD �qD��DD� D  D��DD� D�qD� D�qD}qD��Dz�D�qD}qD	  D	�D
�D
}qD�D�DD�DD�D�D��D  D� D��D}qD�D��D  D� D�D��DD��DD�D  D� D  D� D�D� D�qD}qD�qDz�D�qD}qD�qD}qD  D� D  D� D  D� D�qD }qD ��D!}qD"  D"}qD#  D#��D$�D$��D%  D%}qD&  D&��D&�qD'z�D'��D(}qD(�qD)� D*�D*� D+  D+}qD+�qD,� D-D-��D-�qD.}qD/�D/��D/��D0z�D1  D1�D2�D2��D2��D3}qD4  D4�D5D5��D5�qD6� D7�D7}qD8  D8��D9  D9}qD:�D:� D;  D;��D;�qD<� D=  D=}qD=�qD>� D>�qD?}qD?��D@� DA�DA��DB�DB��DC  DC��DD�DD� DE  DE��DF�DF�DG�DG��DH  DH}qDH�qDI� DJ  DJ� DKDK��DL�DL� DL�qDM}qDN�DN�DO�DO��DP  DP}qDQ�DQ��DR  DR� DSDS�DS�qDT}qDU�DU��DV�DV��DW�DW��DX  DX� DY  DY� DZ�DZ��D[�D[��D[�qD\xRD]  D]�D^D^�D_  D_z�D_��D`}qDa�Da��Da�qDb}qDc�Dc��Dc�qDd� De  De��Df�Df�Dg  Dg}qDg�qDh� Di  Di��Di�qDjz�Dj�qDk� Dl�Dl��DmDm��Dm�qDn� Dn�qDoxRDo��Dp��Dq�Dq� Dr  Dr}qDr�RDsz�Dt�Dt�Du�Du}qDu��Dvz�Dw  Dw�Dx�Dx��Dy  Dy� Dz�Dz��D{D{��D{�qD|}qD|�qD}� D}�qD~}qD�D�D�  D�>�D��HD��HD�  D�>�D�~�D��qD���D�>�D�~�D�� D�  D�@ D�~�D��qD���D�AHD��HD��HD�HD�B�D��HD��HD�HD�@ D��HD�� D��qD�@ D��HD�� D���D�>�D�}qD�� D�HD�AHD���D��HD���D�@ D��HD�� D���D�=qD�~�D�� D�  D�@ D�� D�� D�HD�@ D�� D���D���D�AHD���D��HD���D�>�D�� D�� D���D�AHD��HD���D��qD�@ D�� D�� D�HD�@ D��HD�D�  D�@ D��HD���D���D�B�D�� D���D�  D�@ D��HD�� D��qD�AHD��HD�� D�HD�@ D�~�D�� D���D�@ D�~�D��qD�HD�@ D�~�D�D�HD�@ D��HD��HD�  D�@ D�~�D���D���D�@ D�� D���D���D�>�D�� D�� D���D�@ D�� D�� D�HD�>�D�� D��HD�HD�AHD�� D���D���D�=qD�~�D��HD��qD�@ D���D�� D���D�>�D�~�D��HD�HD�@ D��HD���D��qD�@ D��HD��HD�  D�=qD�~�D��HD��D�B�D���D�� D���D�>�D�~�D��HD�HD�B�D���D�� D���D�>�D�~�D���D�HD�AHD�� D��HD��D�AHD���D���D�HD�@ D�~�D�� D�HD�>�D�}qD���D�  D�>�D�}qD���D�HD�@ D�~�D�� D�  D�@ D�~�D�� D��qD�<)D�~�D��HD�HD�AHD�� D��qD���D�@ D�~�D�� D�HD�>�D�� D���D��qD�>�D�� D���D���D�@ D���D��HD�HD�AHD��HD�D�HD�AHD��HD��HD���D�>�D�~�D���D�  D�=qD�}qD���D��)D�=qD�� D��HD�  D�>�D�~�D��HD��D�B�D��HD���D�  D�AHD�� D���D�  D�AHD�~�D���D��qD�>�D�� D�� D�HD�AHD�~�D¾�D���D�>�DÀ D��HD�  D�@ DĀ D�� D�  D�AHDŁHDž�D�  D�@ Dƀ D�� D�  D�>�Dǀ D�� D�  D�AHDȁHDȾ�D���D�@ DɁHD��HD��D�@ D�~�D�� D���D�>�D�}qD�� D�HD�@ D̀ D�� D�  D�>�D�}qDͽqD��qD�=qD�~�D�� D�  D�AHDρHD�� D�HD�B�DЂ�D�� D���D�>�D�~�D�� D�  D�AHDҁHD��HD�  D�AHDӀ DӽqD���D�@ DԀ D�� D���D�@ DՀ Dվ�D�  D�@ D�~�D־�D���D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�>�DفHD�D�  D�AHDڀ Dھ�D�  D�AHDہHD۽qD��qD�@ D܀ D�� D�  D�=qD݀ D�D�HD�@ D�~�D�� D�  D�@ D߁HD�� D�  D�AHD��HD�� D�  D�@ D�}qD�� D�  D�=qD� D��HD�  D�@ DわD��HD���D�@ D�HD侸D���D�>�D�~�D�� D�HD�AHD�HD�� D���D�=qD�~�D�� D�  D�>�D�HD�D��D�AHD� D龸D��qD�=qD�}qD�qD���D�=qD�}qD�� D��D�AHD�HD�D�  D�@ D� D�� D�HD�>�D�}qDD�  D�AHD�HD��HD��qD�<)D�}qD�� D�HD�B�D�HD�qD��qD�>�D�HD��HD��qD�=qD�HD��HD���D�>�D�~�D���D�  D�@ D�� D��HD��D�AHD�� D�� D�  D�AHD��HD�� D�HD�B�D�� D���D���D�AHD��HD�D��G�O�?\)?��?aG�?�z�?�33?�G�@�@z�@+�@@  @Y��@p��@��
@���@�(�@���@�@�G�@���@ٙ�@��
@�\)@��HA33A
=qA��A
=Ap�A#�
A)��A.�RA5�A;�AAG�AG�AMp�AS�
AZ�HAaG�AhQ�An�RAu�Az�HA�Q�A��A��RA��A�A�G�A�(�A�
=A��A��A�  A��A�
=A�=qA�p�A���A��
A��RA��A���A�  A�33A�
=Aʏ\A�AУ�AӅA�{Aٙ�A��A��A�(�A�
=A��A���A�  A�33A�
=A�=qA�p�B z�B{B�B��BffB�
B	�B
�\B  Bp�B�\B�Bz�Bp�BffB\)B��B��B�\B�BQ�B�B�B�RB�B��BB�HB�B z�B!G�B"{B"�HB#�B$��B%B&�RB'�B(z�B)�B)�B*�\B+�B,z�B-p�B.ffB/33B/�
B0��B1p�B2=qB3
=B4  B4��B5��B6=qB6�HB8  B8��B9��B:ffB:�HB;�B<Q�B=G�B>{B>�HB?�B@(�B@��BAp�BBffBC\)BD  BDz�BE�BEBF�RBG�BH(�BH��BIp�BJffBK\)BLQ�BL��BMp�BN=qBO33BP(�BP��BQ��BR=qBR�HBT  BT��BU��BV{BV�RBW�BXz�BYp�BZ{BZ�RB[\)B\Q�B\��B^{B_
=B`  B`��Bap�Bb{Bc33BdQ�BeG�Bf{Bf�RBg�BhQ�Bh��BiBj�\Bk33Bk�Bk�
Bl  Blz�Bl��BmG�Bm��BmBmBm�BnffBn�RBo
=Bo33Bo\)Bo�Bo�Bp(�Bpz�Bp��Bq�Bq�Bqp�Bq�BrffBr�RBr�HBs
=Bs\)Bs�Bt(�Bt��Bu�BuG�Bup�Bu��Bv{Bv�\Bw
=Bw\)Bw�
Bx  Bx(�Bxz�Bx��By�By��Bz{Bz�\Bz�HB{
=B{33B{\)B|(�B|z�B|z�B|��B}�B}��B}�B~ffB~ffB~�\B~�HB\)B�
B�{B�{B�(�B�z�B��RB��HB���B�
=B�G�B���B��B�  B�{B�Q�B���B��HB�33B�p�B��B��
B�(�B��\B��RB���B�33B��B��B�=qB�ffB���B���B�p�B�B�{B�=qB��\B�
=B�p�B��B��B�=qB��RB�33B��B��B�=qB���B�33B���B�  B�Q�B��HB�\)B��
B�=qB��\B��B��B�=qB���B��B��B�=qB��HB�\)B��
B�ffB��B�B�(�B��RB�p�B�(�B���B��B��
B���B�
=B���B�Q�B��B���B�(�B��RB�p�B�(�B��\B��B��B��\B���B��B�Q�B�
=B��B�  B���B�\)B�  B�z�B���B���B�Q�B���B�\)B��
B��\B�33B�B�(�B��RB��B�{B�z�B�
=B��
B�z�B���B�p�B�=qB���B�p�B��B��RB�p�B��B�z�B�G�B��B�Q�B���B��B�Q�B���B�\)B�{B��RB��B��
B�z�B��BÙ�B�{B��HB�\)B��
B�ffB��B�B�=qBȣ�B�G�B�  B�ffB��HB˅B�(�Ḅ�B��B�B�ffB��HB�G�B�  BУ�B�
=BхB�=qB���B�33B�B�ffB��HB�G�B��B�ffBָRB�\)B�  B�Q�Bأ�B�\)B��
B�(�Bڣ�B�G�BۮB�{Bܣ�B�33B݅B�{Bޏ\B��HB߅B�{B�ffB��HB�B��
B�=qB��HB�\)B�B�(�B�RB�33B�p�B�{B�\B���B�p�B�  B�Q�B��B�G�B��
B�{B��B�33B�B��B�\B��HB�G�B��B�ffB��B�G�B��
B�{B��\B�33B�B�  B�\B���B�\)B�  B�z�B��RB�33B��B�=qB��RB�G�B�B�{B���B�33B���B�=qB��RB�
=B�B�=qB��\B�33B��B�  B���B�33B���C 
=C ffC ��C ��C�Cp�C��C�CG�Cz�CC�CffC��C��CQ�C�C�C33Cp�C�
C(�CffCC�C\)C�RC�C\)C��C	�C	ffC	�
C
33C
p�C
�
CQ�C�\C��C\)C�C
=Cz�C��C(�C��C�CQ�CC{Cz�C�C=qC��C{CffC�
CG�C��C  Cp�C��C(�C��C��C\)C��C(�C�C  C\)CC33C�\C�CffCC(�C��C�CQ�C��C�C�C  CG�C�RC33C�C�C \)C �C!�C!��C!�HC"\)C"C#�C#�\C$  C$G�C$C%(�C%z�C%�C&\)C&��C'(�C'p�C'�
C(Q�C(��C)  C)p�C)C*33C*��C*�HC+\)C+C,{C,z�C,�C-=qC-��C.�C.ffC.�
C/G�C/��C/��C0p�C0�RC133C1�C1�
C2Q�C2��C2��C3p�C3C4�C4�\C4�HC5=qC5�C6  C6\)C6��C7�C7�C7��C8=qC8��C9{C9ffC9C:=qC:�C:�C;\)C;��C<{C<z�C<�RC=(�C=�\C=�
C>G�C>��C>��C?p�C?�RC@�C@�\C@�
CA=qCA��CA�CB\)CB�RCC
=CCz�CC��CD(�CD��CD�CE=qCECF{CF\)CF�
CG33CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�lG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A�t�A�S�A�"�A�RA�E�A�JA�A���A��A��A��HA���A�ƨA�A��A�PA�jA�S�A�5?A��A� �A� �A�1'A��A���A�XA盦A�C�A䝲A� �Aܧ�A�-A�-Aɺ^A×�A�VA�(�A��yA��7A��A�Q�A���A�Q�A���A�bNA�K�A�K�A��
A�;dA�%A���A�~�A�O�A�E�A��\A~1As��Am��AljAjĜAh�`Ad�uA`�A]XAZ9XAXbAT^5AS|�AQ��AO��AM�AI��AG��AF�9AFE�AEK�AD�+AC��ACC�AC7LAB��AB��AB�\AA��A@��A?�
A?7LA>��A>~�A<�A:�A8��A8�+A8��A8��A8��A8�A8��A8VA7�mA7/A6I�A5K�A4ffA3�FA2��A1��A1K�A0��A0$�A/��A/"�A.�/A.��A.r�A.{A-�A-�hA-dZA,�HA,bNA,bA+��A+��A+G�A+33A*��A*ffA)�A)G�A)"�A(��A(Q�A(1'A(1A'�^A'�A'?}A&��A&�!A&n�A&1'A%�^A%?}A$�`A$�9A$�+A$  A#G�A"��A"�A"��A"~�A"9XA!��A!�TA!�^A!�7A!K�A!�A ��A ~�A 5?A �A bA��A�7A�An�A(�A�A�7AG�A%A�A�\A~�AA�A��A��A|�A"�A�A��A�
AhsA�A��AffA(�AbA�mA�^A�hAp�A?}A��A�uA5?A�wA�A;dA��A�AjA-A��A��AG�A��A�+AQ�A �AƨA`BA"�A�`An�A5?A�FA"�A��A�HA��AffA�A��A;dA�HA�\A-A��A7LA
=A
ȴA
JA	�
A	�A	��A	�7A��AM�A�wA�AȴA��AZA �A�A��A"�A�A��A�\A  A��Ap�A��AM�A�mA�-A�A �DA -@�dZ@��H@�5?@��T@��@�&�@��j@�Q�@��w@�C�@��!@�M�@���@�p�@���@�I�@�b@��m@��@�;d@�"�@���@��-@��@�  @���@�\)@�+@�M�@���@���@���@�33@�ȴ@��T@��`@�j@���@�@��@�M�@�{@��@�?}@���@��y@���@�p�@��/@�1@�R@�@�j@��m@��
@ߕ�@�o@�=q@�@���@�/@܋D@��
@�S�@ڗ�@��@���@ش9@؛�@��@���@�{@�O�@���@�A�@�\)@���@�E�@��@��#@Ѳ-@�p�@�G�@Ь@�I�@�t�@�M�@ͺ^@��/@�(�@˅@�^5@�@��@�I�@ǶF@�|�@�S�@�o@Ə\@�^5@���@���@�7L@ļj@���@�@�~�@�x�@�Ĝ@�z�@�I�@��@��P@�l�@�\)@�"�@��\@���@���@�?}@��@��D@�(�@��;@���@��H@���@�@��@���@��9@�bN@�1@��w@���@���@�33@���@���@��@��h@�%@��9@��D@� �@�C�@��R@��\@�E�@��#@�hs@��@�j@��@��m@�l�@�33@�@���@��R@�~�@��@�p�@��j@��
@��@�|�@�C�@���@�~�@�n�@�^5@�{@���@��@���@�Z@�  @���@�S�@���@�~�@��@��@���@��@�9X@��@��@���@�v�@�E�@���@��@���@��@�Z@��@��@�dZ@���@��R@�^5@�$�@���@�`B@��`@���@�Z@�9X@� �@��@�33@�o@��\@�E�@�=q@�-@�J@�@���@��7@�x�@�p�@�hs@�V@� �@���@�33@��H@���@�ff@�V@��@��-@���@�G�@��/@���@�r�@�b@���@�+@���@��\@�=q@��@�X@�&�@��@��j@�j@�9X@� �@�b@��m@��F@�t�@�+@���@�ȴ@���@��\@�v�@�n�@�M�@�$�@��T@��7@�x�@�G�@��@��@�(�@��m@��w@�;d@��y@���@�^5@�@���@�X@��@��@��@���@�j@�(�@�  @��@��@�K�@��@��H@��!@�~�@�5?@�@�?}@��@���@�z�@�Z@�1'@�1@���@�l�@�+@��H@���@�=q@��@�@�x�@�`B@�G�@�/@��@���@�9X@�w@|�@~��@~@}��@}��@}V@|�@|z�@|j@{��@{o@z�\@z=q@y��@y��@x��@x�@xQ�@w�w@w|�@v��@v5?@u��@u�h@u`B@uV@t�@tz�@t1@sƨ@s�@s@r�!@rn�@q��@qX@q&�@p�@pb@o��@o;d@n�@nv�@m�@m?}@lZ@k�F@kC�@j��@jM�@i�#@i��@i�7@ihs@i7L@h��@h�@hQ�@h1'@hb@g��@gK�@g+@f�@f�+@fE�@e�@ep�@e?}@eV@d��@dz�@dZ@d1@c�m@c��@ct�@cC�@c"�@b�@b��@b-@a��@a�#@a��@ahs@a%@`��@`��@`Q�@`A�@` �@`b@`  @_�;@_�@_l�@_�@^��@^v�@^$�@^@]��@]�-@]V@\�/@\Z@[�m@[�@[@Z�\@Z�@Y��@Yx�@X��@XQ�@XA�@X �@W��@V�y@VE�@V{@U�@U�-@UO�@T��@T��@T�D@Tj@TI�@T1@S��@R�@RM�@R-@Q��@PbN@P1'@Pb@O��@O�@Ol�@O+@O
=@N��@N�y@N�R@M@Mp�@L��@LZ@L�@K�@KS�@K"�@J�H@JM�@IX@HĜ@H��@H�@H�u@H��@H��@H��@H�u@Hr�@HQ�@H �@G��@G+@Fv�@E�-@E?}@D��@Dz�@D�@C�m@B�@B�!@B~�@BM�@A�@Ahs@@��@@��@@A�@@  @?�w@?�@?�P@?K�@>�y@>��@>{@=@=�h@<�@<I�@<1@;ƨ@;��@;t�@;@:�\@:=q@9�#@9x�@9G�@8��@81'@7�w@7l�@7K�@7;d@7�@6ȴ@6��@6v�@6V@6@5@5�h@5/@4�@4�@4�D@4Z@4(�@3��@3t�@3C�@3"�@3"�@3@2��@2n�@2-@2J@1�@1�^@1�7@1x�@1hs@17L@0�`@0r�@0A�@0b@/�w@/l�@.��@.5?@.@-@-�h@-p�@-/@,��@,�@,�/@,��@,��@,1@+��@+�@+t�@+S�@+C�@+33@+o@*�@*��@*�!@*��@*n�@*M�@*=q@)��@)�#@)��@)x�@)X@)X@)&�@(��@(��@(�9@(Q�@( �@'�@'�;@'��@'K�@'�@&�y@&��@&ff@&{@%@%p�@%O�@%/@%�@$�@$j@$(�@#�
@#dZ@"��@"~�@"n�@"M�@"=q@"J@!�#@!�^@!x�@!G�@!&�@ ��@ bN@   @��@\)@
=@��@��@�y@�@�R@��@��@��@v�@�T@�-@p�@O�@/@��@�/@�j@j@�@ƨ@�@t�@t�@dZ@o@�!@n�@n�@^5@M�@�@�@��@�@��@Ĝ@�u@�@bN@1'@b@  @�@K�@+@+@�@�y@��@V@$�@{@$�@$�@�@��@p�@/@/@�@�@�j@��@��@�j@�D@j@j@Z@�
@�F@dZ@t�@dZ@33@o@��@�H@@@@��@�\@^5@=q@��@�^@hs@��@r�@1'@ �@bA�v�A�v�A�x�A�n�A�hsA�l�A�x�A�v�A�l�A�^5A��A�;dA��A�VA�ĜA�A�\)A�VA�9XA�{A�
=A�%A�A�  A�  A���A���A��A��A��A��A��`A��`A��TA��#A��
A���A���A���A���A���A���A�ȴA�ƨA�wA�^A�A�A�A�A��A��A��A��A��A�A��A镁A�A�A�x�A�r�A�jA�ffA�bNA�bNA�bNA�XA�S�A�K�A�C�A�A�A�9XA�7LA�5?A��A��A��A��A��A��A��A� �A�"�A�$�A�$�A�"�A��A��A��A�"�A�-A�/A�/A�33A�7LA�5?A�33A�(�A��A��A� �A�$�A�(�A�$�A��A�{A�
=A�A�  A���A��`A���A�jA�9A��A藍A�~�A�v�A�l�A�^5A�XA�M�A�?}A�/A��A��A��A�bA��A�!A�A�G�A�oA���A�9A�A�7A�ffA�E�A�1'A� �A�oA�
=A���A��A��;A�jA�jA�
=A�9A�bNA�%A���A��A�PA�hsA�9XA�$�A��A�A❲A��A�A�I�A���A�1A�XAޙ�Aݲ-A�;dA�ȴA�hsA�bA�ƨA�z�A�1'A��HAڛ�A�I�A��A٩�A�XA�%A���A�~�A���A�ȴA�E�AՅA���A�C�AҁA��A�bA�ĜA�hsA�33A̴9A˶FA��A�M�A��A��A��#A���A�+A�/A���AƲ-A�ffA�
=A�{Aď\A�ZA��A��HAìAÏ\AÁA�l�A�dZA�\)A�A�A�=qA�33A�$�A�{A�bA�A��yA°!A�jA��A��/A���A�|�A�/A���A��!A�dZA�ZA�`BA�9XA�1A��/A���A��A�/A���A��TA��^A���A�dZA���A��+A�7LA���A��#A���A��RA��hA�l�A�+A���A��FA�|�A�?}A�  A��uA�^5A�5?A���A���A���A��uA�x�A�n�A�I�A��A�bNA��A��A���A��jA��jA�(�A�A�|�A��A���A��A�A�^5A���A�dZA��HA�p�A�33A��A���A��\A�VA��A���A��A�VA�$�A��A�bA�JA���A�;dA�(�A�A�`BA�"�A���A�1'A�A��A��!A�l�A��
A�5?A��wA�`BA��;A��A�\)A�x�A��A��A���A�  A��A��A��HA�~�A�bNA��`A���A�^5A���A�JA��DA��mA���A��+A�n�A�1'A��yA��RA���A��A�ffA�5?A��A�ƨA��FA��A�l�A�=qA�ȴA��\A�|�A�jA�VA�E�A�-A��A�VA���A��A��mA��A�ĜA���A��\A�x�A�`BA�I�A�/A��A�  A���A�~�A��jA�-A���A�/A�9XA�VA�Q�A��A�VA�33A��RA�hsA��A��jA���A��PA�I�A��#A��7A�?}A���A���A� �A��A��A�ZA�"�A���A�ZA���A�33A�~�A��/A�5?A��-A�XA��;A�M�A�{A�;A�7A33A~�yA}�A|E�AzI�Aw�;AvQ�At�As?}Ar��Ar9XAq�TAp�AoG�An�AmhsAm&�Am
=Al�Al��AlȴAl��Al�uAlz�AljAlM�Al�Ak��Ak�
Ak�hAk;dAj��AjjAjM�AjAi�Ai�
Ai��Ai\)Ai�Ai�Ah��Ah�DAh�Ag%Af1Ae/Ad�/Ad�uAdM�Ad�Ac��Ac\)AcVAb��Abz�Aa�AadZA`=qA_l�A_&�A^��A^��A^ZA^�A]�mA]��A]��A\�HA\A[�hA[O�AZ��AZZAZ=qAZ$�AZ1AY�AY�^AY�AYdZAY�AX�RAXv�AX-AWx�AWAVjAU�AU+ATE�AT$�ATJATAS��AS�mAS�TAS�;AS��AS�FASx�ASS�AS?}ASoAR�/AR��AR�ARE�AQ`BAQ33AP^5AP5?AP$�AP�AO�
AO��AO��AO\)AN��AN�RANn�AM��AM�FAMC�AK�FAKAJ��AJ��AJ=qAI�AI��AI33AH��AHv�AH$�AG��AG?}AF��AF�AF��AF�jAF�9AF��AF�DAF�AFQ�AFbNAF1'AFJAE�-AEp�AEG�AE+AE&�AE�AD�`AD��ADn�ADE�ADI�AD=qAD-ADAChsACO�ACK�ACG�AC?}ACC�AC?}AC;dAC?}AC;dAC/AC/AC�AB��ABȴAB�RAB�!AB�!AB�AB��AB��AB��AB��AB��AB�uAB�+AB~�ABbNAB1'AA�mAA�7AAC�AA"�A@��A@�9A@^5A@{A?��A?�A?�
A?��A?A?��A?dZA?7LA?VA>��A>��A>��A>��A>�A>ȴA>�jA>�!A>��A>��A>M�A=�
A=x�A<�A<=qA;��A;��A;hsA;S�A:ȴA9��A8��A8�\A8~�A8�DA8��A8��A8��A8�uA8r�A8r�A8�A8��A8�`A8��A9
=A9
=A8��A8�A8��A8v�A8jA8v�A8z�A8�\A8��A8ĜA8��A8�A8��A8�A8�A8ĜA8ȴA8�9A8��A8��A8�DA8�A8n�A8Q�A8=qA8M�A8Q�A8I�A81'A7��A7�wA7�hA7`BA7K�A7?}A77LA7�A6�A6�RA6bNA6=qA6�A5�mA5��A5�hA5dZA5/A5A4�HA4�jA4�DA4r�A41'A4{A4  A3�TA3��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      A�p�A�t�A�S�A�"�A�RA�E�A�JA�A���A��A��A��HA���A�ƨA�A��A�PA�jA�S�A�5?A��A� �A� �A�1'A��A���A�XA盦A�C�A䝲A� �Aܧ�A�-A�-Aɺ^A×�A�VA�(�A��yA��7A��A�Q�A���A�Q�A���A�bNA�K�A�K�A��
A�;dA�%A���A�~�A�O�A�E�A��\A~1As��Am��AljAjĜAh�`Ad�uA`�A]XAZ9XAXbAT^5AS|�AQ��AO��AM�AI��AG��AF�9AFE�AEK�AD�+AC��ACC�AC7LAB��AB��AB�\AA��A@��A?�
A?7LA>��A>~�A<�A:�A8��A8�+A8��A8��A8��A8�A8��A8VA7�mA7/A6I�A5K�A4ffA3�FA2��A1��A1K�A0��A0$�A/��A/"�A.�/A.��A.r�A.{A-�A-�hA-dZA,�HA,bNA,bA+��A+��A+G�A+33A*��A*ffA)�A)G�A)"�A(��A(Q�A(1'A(1A'�^A'�A'?}A&��A&�!A&n�A&1'A%�^A%?}A$�`A$�9A$�+A$  A#G�A"��A"�A"��A"~�A"9XA!��A!�TA!�^A!�7A!K�A!�A ��A ~�A 5?A �A bA��A�7A�An�A(�A�A�7AG�A%A�A�\A~�AA�A��A��A|�A"�A�A��A�
AhsA�A��AffA(�AbA�mA�^A�hAp�A?}A��A�uA5?A�wA�A;dA��A�AjA-A��A��AG�A��A�+AQ�A �AƨA`BA"�A�`An�A5?A�FA"�A��A�HA��AffA�A��A;dA�HA�\A-A��A7LA
=A
ȴA
JA	�
A	�A	��A	�7A��AM�A�wA�AȴA��AZA �A�A��A"�A�A��A�\A  A��Ap�A��AM�A�mA�-A�A �DA -@�dZ@��H@�5?@��T@��@�&�@��j@�Q�@��w@�C�@��!@�M�@���@�p�@���@�I�@�b@��m@��@�;d@�"�@���@��-@��@�  @���@�\)@�+@�M�@���@���@���@�33@�ȴ@��T@��`@�j@���@�@��@�M�@�{@��@�?}@���@��y@���@�p�@��/@�1@�R@�@�j@��m@��
@ߕ�@�o@�=q@�@���@�/@܋D@��
@�S�@ڗ�@��@���@ش9@؛�@��@���@�{@�O�@���@�A�@�\)@���@�E�@��@��#@Ѳ-@�p�@�G�@Ь@�I�@�t�@�M�@ͺ^@��/@�(�@˅@�^5@�@��@�I�@ǶF@�|�@�S�@�o@Ə\@�^5@���@���@�7L@ļj@���@�@�~�@�x�@�Ĝ@�z�@�I�@��@��P@�l�@�\)@�"�@��\@���@���@�?}@��@��D@�(�@��;@���@��H@���@�@��@���@��9@�bN@�1@��w@���@���@�33@���@���@��@��h@�%@��9@��D@� �@�C�@��R@��\@�E�@��#@�hs@��@�j@��@��m@�l�@�33@�@���@��R@�~�@��@�p�@��j@��
@��@�|�@�C�@���@�~�@�n�@�^5@�{@���@��@���@�Z@�  @���@�S�@���@�~�@��@��@���@��@�9X@��@��@���@�v�@�E�@���@��@���@��@�Z@��@��@�dZ@���@��R@�^5@�$�@���@�`B@��`@���@�Z@�9X@� �@��@�33@�o@��\@�E�@�=q@�-@�J@�@���@��7@�x�@�p�@�hs@�V@� �@���@�33@��H@���@�ff@�V@��@��-@���@�G�@��/@���@�r�@�b@���@�+@���@��\@�=q@��@�X@�&�@��@��j@�j@�9X@� �@�b@��m@��F@�t�@�+@���@�ȴ@���@��\@�v�@�n�@�M�@�$�@��T@��7@�x�@�G�@��@��@�(�@��m@��w@�;d@��y@���@�^5@�@���@�X@��@��@��@���@�j@�(�@�  @��@��@�K�@��@��H@��!@�~�@�5?@�@�?}@��@���@�z�@�Z@�1'@�1@���@�l�@�+@��H@���@�=q@��@�@�x�@�`B@�G�@�/@��@���@�9X@�w@|�@~��@~@}��@}��@}V@|�@|z�@|j@{��@{o@z�\@z=q@y��@y��@x��@x�@xQ�@w�w@w|�@v��@v5?@u��@u�h@u`B@uV@t�@tz�@t1@sƨ@s�@s@r�!@rn�@q��@qX@q&�@p�@pb@o��@o;d@n�@nv�@m�@m?}@lZ@k�F@kC�@j��@jM�@i�#@i��@i�7@ihs@i7L@h��@h�@hQ�@h1'@hb@g��@gK�@g+@f�@f�+@fE�@e�@ep�@e?}@eV@d��@dz�@dZ@d1@c�m@c��@ct�@cC�@c"�@b�@b��@b-@a��@a�#@a��@ahs@a%@`��@`��@`Q�@`A�@` �@`b@`  @_�;@_�@_l�@_�@^��@^v�@^$�@^@]��@]�-@]V@\�/@\Z@[�m@[�@[@Z�\@Z�@Y��@Yx�@X��@XQ�@XA�@X �@W��@V�y@VE�@V{@U�@U�-@UO�@T��@T��@T�D@Tj@TI�@T1@S��@R�@RM�@R-@Q��@PbN@P1'@Pb@O��@O�@Ol�@O+@O
=@N��@N�y@N�R@M@Mp�@L��@LZ@L�@K�@KS�@K"�@J�H@JM�@IX@HĜ@H��@H�@H�u@H��@H��@H��@H�u@Hr�@HQ�@H �@G��@G+@Fv�@E�-@E?}@D��@Dz�@D�@C�m@B�@B�!@B~�@BM�@A�@Ahs@@��@@��@@A�@@  @?�w@?�@?�P@?K�@>�y@>��@>{@=@=�h@<�@<I�@<1@;ƨ@;��@;t�@;@:�\@:=q@9�#@9x�@9G�@8��@81'@7�w@7l�@7K�@7;d@7�@6ȴ@6��@6v�@6V@6@5@5�h@5/@4�@4�@4�D@4Z@4(�@3��@3t�@3C�@3"�@3"�@3@2��@2n�@2-@2J@1�@1�^@1�7@1x�@1hs@17L@0�`@0r�@0A�@0b@/�w@/l�@.��@.5?@.@-@-�h@-p�@-/@,��@,�@,�/@,��@,��@,1@+��@+�@+t�@+S�@+C�@+33@+o@*�@*��@*�!@*��@*n�@*M�@*=q@)��@)�#@)��@)x�@)X@)X@)&�@(��@(��@(�9@(Q�@( �@'�@'�;@'��@'K�@'�@&�y@&��@&ff@&{@%@%p�@%O�@%/@%�@$�@$j@$(�@#�
@#dZ@"��@"~�@"n�@"M�@"=q@"J@!�#@!�^@!x�@!G�@!&�@ ��@ bN@   @��@\)@
=@��@��@�y@�@�R@��@��@��@v�@�T@�-@p�@O�@/@��@�/@�j@j@�@ƨ@�@t�@t�@dZ@o@�!@n�@n�@^5@M�@�@�@��@�@��@Ĝ@�u@�@bN@1'@b@  @�@K�@+@+@�@�y@��@V@$�@{@$�@$�@�@��@p�@/@/@�@�@�j@��@��@�j@�D@j@j@Z@�
@�F@dZ@t�@dZ@33@o@��@�H@@@@��@�\@^5@=q@��@�^@hs@��@r�@1'@ �G�O�A�v�A�v�A�x�A�n�A�hsA�l�A�x�A�v�A�l�A�^5A��A�;dA��A�VA�ĜA�A�\)A�VA�9XA�{A�
=A�%A�A�  A�  A���A���A��A��A��A��A��`A��`A��TA��#A��
A���A���A���A���A���A���A�ȴA�ƨA�wA�^A�A�A�A�A��A��A��A��A��A�A��A镁A�A�A�x�A�r�A�jA�ffA�bNA�bNA�bNA�XA�S�A�K�A�C�A�A�A�9XA�7LA�5?A��A��A��A��A��A��A��A� �A�"�A�$�A�$�A�"�A��A��A��A�"�A�-A�/A�/A�33A�7LA�5?A�33A�(�A��A��A� �A�$�A�(�A�$�A��A�{A�
=A�A�  A���A��`A���A�jA�9A��A藍A�~�A�v�A�l�A�^5A�XA�M�A�?}A�/A��A��A��A�bA��A�!A�A�G�A�oA���A�9A�A�7A�ffA�E�A�1'A� �A�oA�
=A���A��A��;A�jA�jA�
=A�9A�bNA�%A���A��A�PA�hsA�9XA�$�A��A�A❲A��A�A�I�A���A�1A�XAޙ�Aݲ-A�;dA�ȴA�hsA�bA�ƨA�z�A�1'A��HAڛ�A�I�A��A٩�A�XA�%A���A�~�A���A�ȴA�E�AՅA���A�C�AҁA��A�bA�ĜA�hsA�33A̴9A˶FA��A�M�A��A��A��#A���A�+A�/A���AƲ-A�ffA�
=A�{Aď\A�ZA��A��HAìAÏ\AÁA�l�A�dZA�\)A�A�A�=qA�33A�$�A�{A�bA�A��yA°!A�jA��A��/A���A�|�A�/A���A��!A�dZA�ZA�`BA�9XA�1A��/A���A��A�/A���A��TA��^A���A�dZA���A��+A�7LA���A��#A���A��RA��hA�l�A�+A���A��FA�|�A�?}A�  A��uA�^5A�5?A���A���A���A��uA�x�A�n�A�I�A��A�bNA��A��A���A��jA��jA�(�A�A�|�A��A���A��A�A�^5A���A�dZA��HA�p�A�33A��A���A��\A�VA��A���A��A�VA�$�A��A�bA�JA���A�;dA�(�A�A�`BA�"�A���A�1'A�A��A��!A�l�A��
A�5?A��wA�`BA��;A��A�\)A�x�A��A��A���A�  A��A��A��HA�~�A�bNA��`A���A�^5A���A�JA��DA��mA���A��+A�n�A�1'A��yA��RA���A��A�ffA�5?A��A�ƨA��FA��A�l�A�=qA�ȴA��\A�|�A�jA�VA�E�A�-A��A�VA���A��A��mA��A�ĜA���A��\A�x�A�`BA�I�A�/A��A�  A���A�~�A��jA�-A���A�/A�9XA�VA�Q�A��A�VA�33A��RA�hsA��A��jA���A��PA�I�A��#A��7A�?}A���A���A� �A��A��A�ZA�"�A���A�ZA���A�33A�~�A��/A�5?A��-A�XA��;A�M�A�{A�;A�7A33A~�yA}�A|E�AzI�Aw�;AvQ�At�As?}Ar��Ar9XAq�TAp�AoG�An�AmhsAm&�Am
=Al�Al��AlȴAl��Al�uAlz�AljAlM�Al�Ak��Ak�
Ak�hAk;dAj��AjjAjM�AjAi�Ai�
Ai��Ai\)Ai�Ai�Ah��Ah�DAh�Ag%Af1Ae/Ad�/Ad�uAdM�Ad�Ac��Ac\)AcVAb��Abz�Aa�AadZA`=qA_l�A_&�A^��A^��A^ZA^�A]�mA]��A]��A\�HA\A[�hA[O�AZ��AZZAZ=qAZ$�AZ1AY�AY�^AY�AYdZAY�AX�RAXv�AX-AWx�AWAVjAU�AU+ATE�AT$�ATJATAS��AS�mAS�TAS�;AS��AS�FASx�ASS�AS?}ASoAR�/AR��AR�ARE�AQ`BAQ33AP^5AP5?AP$�AP�AO�
AO��AO��AO\)AN��AN�RANn�AM��AM�FAMC�AK�FAKAJ��AJ��AJ=qAI�AI��AI33AH��AHv�AH$�AG��AG?}AF��AF�AF��AF�jAF�9AF��AF�DAF�AFQ�AFbNAF1'AFJAE�-AEp�AEG�AE+AE&�AE�AD�`AD��ADn�ADE�ADI�AD=qAD-ADAChsACO�ACK�ACG�AC?}ACC�AC?}AC;dAC?}AC;dAC/AC/AC�AB��ABȴAB�RAB�!AB�!AB�AB��AB��AB��AB��AB��AB�uAB�+AB~�ABbNAB1'AA�mAA�7AAC�AA"�A@��A@�9A@^5A@{A?��A?�A?�
A?��A?A?��A?dZA?7LA?VA>��A>��A>��A>��A>�A>ȴA>�jA>�!A>��A>��A>M�A=�
A=x�A<�A<=qA;��A;��A;hsA;S�A:ȴA9��A8��A8�\A8~�A8�DA8��A8��A8��A8�uA8r�A8r�A8�A8��A8�`A8��A9
=A9
=A8��A8�A8��A8v�A8jA8v�A8z�A8�\A8��A8ĜA8��A8�A8��A8�A8�A8ĜA8ȴA8�9A8��A8��A8�DA8�A8n�A8Q�A8=qA8M�A8Q�A8I�A81'A7��A7�wA7�hA7`BA7K�A7?}A77LA7�A6�A6�RA6bNA6=qA6�A5�mA5��A5�hA5dZA5/A5A4�HA4�jA4�DA4r�A41'A4{A4  A3�TA3��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BUgBS�BW
BOvBXyBR BQNBPHBP�BRTBR�BT�BY�Ba�BhsBiDBn�Bl"BkQBj�Bj�Bm�BqvB~(B��B��B�gB�NB��B	�B	CB	D3B	OvB	}�B	��B	�6B	�MB

�B
&�B
B
0�B
0�B
9XB
B
B

rB
�B
B	�xB	��B	�B	�WB	�OB	��B	�XB	��B	��B	.B	poB	f�B	m)B	qvB	}"B	��B	�B	�$B	��B	�B	�B	�6B	��B	��B	�B
oB
$B
A�B
S&B
lWB
��B
��B
�nB
��B
�B
�kB
��B
�9B
�B
��B
�'B
��B
��B
� B
�RB
�OB
ʌB
�}B
�?B
�BB
��B
��B
��B
�B
�B
�B
�B
�mB
�mB
��B
�B
�B
��B
�jB
�B
��B
��B
��B
��B
�B
��B
�yB
�?B
҉B
�B
��B
�B
ʌB
��B
�#B
�XB
�EB
��B
��B
�}B
��B
�HB
�B
�}B
�}B
�B
��B
��B
�B
��B
�wB
�<B
�6B
�dB
��B
��B
��B
�FB
��B
��B
�?B
�nB
�3B
��B
��B
��B
��B
�[B
��B
�UB
��B
�wB
�CB
�=B
�qB
�*B
�RB
��B
�@B
��B
�tB
��B
��B
��B
�4B
��B
�\B
��B
�B
�B
�kB
�B
��B
��B
��B
�B
��B
��B
�.B
��B
�(B
��B
�"B
�B
�B
�DB
�rB
�7B
��B
��B
�%B
��B
��B
�B
�B
��B
� B
~�B
|�B
{�B
z�B
zxB
y>B
w�B
v�B
u�B
tTB
s�B
qvB
p;B
pB
n�B
m�B
m)B
k�B
j�B
h�B
g�B
e�B
d�B
a�B
`�B
_B
^jB
[�B
[WB
Z�B
Y�B
YB
VB
T�B
R�B
P�B
PB
O�B
N�B
M�B
L�B
K�B
I�B
IRB
IB
HB
FB
E9B
EB
B[B
A B
@OB
@OB
=�B
=B
;dB
;0B
:^B
9XB
8�B
8�B
8B
7�B
7B
6�B
5�B
5B
4�B
4�B
3�B
3�B
2�B
2�B
2�B
1�B
1'B
1�B
/OB
0�B
.IB
-�B
/B
.IB
.B
.�B
.IB
-�B
,B
*eB
*�B
(�B
(�B
'�B
'�B
'�B
&�B
%FB
$�B
%B
$�B
"4B
"4B
"hB
#B
!�B
!bB
�B
!B
OB
�B
�B
�B
�B
B
�B
�B
xB
CB
�B
�B
�B
�B
�B
7B
�B
1B
�B
$B
SB
�B
$B
�B
�B
�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
B
�B
�B
B
�B
�B
B
�B
�B
B
uB
B
MB
MB
�B
+B
�B
�B
�B
�B
�B
kB
kB
7B
�B
kB
B
�B
�B
�B
	B
�B
�B
�B
7B
7B
�B
B
�B
�B
7B
�B
+B
_B
�B
7B
�B
=B
=B
�B
=B
qB
qB
qB
�B
qB
	B
=B
�B
=B
=B
xB
�B
B
qB
�B
CB
�B
�B
xB
CB
xB
�B
CB
B
�B
�B
�B
�B
~B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
VB
�B
 'B
 'B
 'B
!bB
!�B
!-B
!bB
"4B
"�B
"�B
"hB
!�B
#�B
#�B
$B
#�B
#�B
#�B
$�B
$�B
$�B
%FB
%�B
%zB
&B
&�B
&�B
'B
'B
'B
&�B
(XB
'�B
($B
)�B
)_B
)*B
)�B
)_B
)*B
)�B
)�B
)�B
)_B
)*B
)�B
+�B
+�B
+�B
,B
,B
,=B
,B
-B
-B
,�B
-�B
-�B
.B
-�B
.}B
.}B
/�B
/�B
/�B
0!B
0!B
1'B
0�B
1'B
1'B
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
2�B
2�B
2�B
33B
33B
2�B
33B
2�B
3�B
3�B
4B
3�B
4B
4�B
5?B
5B
5B
6FB
5�B
5�B
6zB
6�B
7�B
7�B
7�B
7�B
7�B
7�B
8RB
8�B
8�B
8RB
9$B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
;0B
;�B
;�B
;�B
;�B
<B
<jB
=B
=B
=<B
=�B
>BB
>�B
>�B
?�B
?�B
?�B
?�B
?}B
@OB
?�B
AUB
A B
A B
AUB
A�B
A�B
A�B
B'B
B[B
B'B
A�B
B�B
B�B
B�B
B�B
B�B
C-B
C�B
C�B
C�B
DgB
D3B
D�B
EmB
EmB
E�B
E�B
F?B
FtB
FtB
F�B
F�B
GB
G�B
GzB
G�B
H�B
H�B
HKB
IB
H�B
I�B
I�B
I�B
I�B
J#B
JXB
K)B
K^B
K^B
K�B
L0B
L0B
L0B
LdB
LdB
LdB
L�B
MB
MB
MB
MB
M6B
MjB
MjB
M�B
NB
N<B
NpB
OB
OB
OB
OBB
OBB
OBB
OvB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P}B
P}B
P�B
P�B
QB
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S&B
R�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
U�B
UgB
UgB
U�B
VB
V�B
W
B
V�B
V�B
W
B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
XB
X�B
X�B
X�B
YB
Z�B
ZQB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[WB
\)B
[�B
\)B
\�B
\�B
\]B
\�B
]/B
^B
^5B
^jB
^jB
^jB
^5B
^B
^B
^5B
^B
^B
^B
^jB
^�B
_;B
_�B
`B
`vB
`vB
`�B
`vB
aHB
aHB
a|B
a|B
a�B
b�B
b�B
b�B
b�B
c B
c�B
c�B
c�B
cTB
d&B
dZB
d�B
d�B
d�B
e`B
e,B
e,B
e`B
e`B
e`B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
g�B
g�B
h>B
h>B
h
B
h
B
hsB
hsB
hsB
h�B
iB
iDB
iDB
iB
iDB
i�B
i�B
i�B
i�B
i�B
jKB
jB
j�B
kQB
kB
k�B
l"B
l�B
lWB
lWB
l�B
l�B
l�B
m)B
m]B
m�B
ncB
ncB
ncB
n�B
o B
o�B
p;B
p;B
poB
p�B
p�B
p�B
qB
qB
qB
qB
qB
q�B
rB
q�B
rB
rB
rGB
rGB
rGB
r|B
r|B
r�B
r�B
sB
sB
sMB
sMB
sMB
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
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
v`B
v+B
v+B
v�B
v`B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
x8B
x8B
x8B
xlB
x8B
x8B
x8B
x�B
x�B
y	B
yrB
yrB
y�B
y�B
y�B
zxB
{B
{JB
{JB
{B
{JB
|B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~(B
~(B
~(B
~(B
~�B
~�B
.B
�B
�4B
� B
��B
��B
��B
�B
�;B
�oB
�oB
�B
�AB
�uB
�AB
�AB
��B
��B
��B
�uB
��B
��B
�B
�GB
�{B
�B
��B
��B
��B
��B
��B
��B
�B
�MB
�B
��B
�B
�MB
�{B
��B
��B
��B
�B
��B
��B
��B
�B
��B
�YB
�YB
��B
�YB
��B
��B
��B
��B
��B
�%B
��B
��B
�%B
�YBT�BU2BT�BS&BZ�BT�BT�BS�BX�BT�BPBI�BI�BX�B]�BT,BQ�BLdBS�BT,BP}BPHBP�BQBP�BQNBP}BQ�BQ�BRTBR�BT,BT,BTaBV�BWsBX�BXEBYB[�B\�B^jB`BBbBd�Be�BiDBh
Bg�Bh>Bh>Bh�Bi�Bi�BiyBjKBo5Bm�Bn�Bm�Bm�Bm]Bl�Bl�BlWBkBj�Bj�Bj�Bj�BkBkBl�BiBi�BlWBi�Bk�BlWBlWBk�Bk�Bl�Bl�Bl�Bn�BpBoiBq�Bq�BrBu�BwfBw�Bx�B~(B��B�%B��B�SB��B��B�DB��B��B�B��B�B��B�tB�B�0B�-B�zB�BʌB��B� B��B�[B�2B��B�B�BخB�#BרB�B�B��B�B�ZB�yB�B�B��B��B�B�ZB��B��B��B�+B�`B��B�+B�B�B	�B	B	B	~B	SB	�B	�B	�B	B	�B	\B	�B	(B	$B	$B	'�B	B	'�B	6zB	4nB	K�B	A�B	D�B	AUB	E�B	D3B	E9B	GB	I�B	IRB	HKB	J�B	J#B	K�B	M�B	K�B	IRB	MB	WsB	d&B	YB	l�B	aB	cTB	�CB	xB	��B	��B	��B	|PB	�B	��B	��B	��B	��B	�CB	�B	�B	�HB	�aB	�B	�B	�aB	�^B	��B	ŢB	��B	�0B	�)B	�<B	�^B	�B	�B	��B	�B	� B	�<B	�jB	��B	�B	�B	�pB	��B	�gB	��B	��B	��B	�vB	�5B	��B	�B	�]B	�sB	�2B	��B	��B	�DB	��B	�cB	�AB	��B	�B	��B	�B	��B	��B	�]B
B
uB
�B
;B	��B
 �B
B
SB
�B
�B
�B
1B
�B
	7B
B

=B
JB
B
�B
	�B

	B
	lB
SB
�B
�B
�B
�B
�B
�B
-�B
+�B
�B
�B
3�B
%�B
!B
'B
2aB
-�B
/�B
(�B
.}B
&LB
#nB
�B
�B
VB
�B
B
 'B
 �B
�B
OB
�B
�B
{B
�B
-CB
D�B
:*B
.�B
%FB
?}B
-B
&LB
%zB
+6B
#�B
1�B
0�B
-CB
,�B
*�B
'�B
6FB
?}B
7B
R�B
$�B
#B
%�B
�B
$�B
��B
.IB
B
�B
$B
1'B
hB
&�B
$B
�B

=B
xB
�B
�B

=B
1B
+B

	B
	7B
�B
{B
�B
�B
 �B
"B
{B
�B
B
{B
 iB
 �B
 �B	�]B	��B	�B	�B	��B	�>B	�	B	��B	�2B	�2B	�B	�oB	�B	��B	�B	�B	�MB	�B	�>B	��B	�QB	��B
SB	�mB	��B	ŢB	ǮB	�yB	̘B	�B	�wB	��B	��B	��B	�B	��B	�RB	��B	��B	�B	�B	�$B	��B	��B	��B	�_B	��B	�B	�B	�B	��B	�CB	��B	��B	�_B	�	B	��B	�+B	�AB	�AB	��B	�@B	��B	��B	�B	�IB	}"B	t�B	n�B	jB	�B	��B	v`B	g8B	iDB	d�B	h
B	e`B	ffB	i�B	f�B	ffB	d�B	h�B	iyB	gmB	e�B	g�B	m�B	n/B	l�B	lWB	p�B	p;B	kB	q�B	p;B	p�B	jKB	m�B	tTB	pB	��B	�FB	�4B	xlB	y	B	y�B	y>B	{�B	��B	}�B	|�B	��B	��B	��B	�B	��B	�B	�DB	�~B	�B	�B	�~B	�B	�~B	�B	��B	�zB	��B	�eB	��B	�B	��B	��B	�B	��B	��B	��B	�[B	�B	�zB	�RB	�gB	��B	��B	��B	ϫB	�B	��B	�$B	��B	��B	��B	��B	�tB	��B	�XB	��B	�?B	��B	�?B	��B	��B	�B	��B	��B	�RB	�EB	�tB	�B	��B	�B	�hB	��B	�B	��B	��B	��B	�XB	��B	�
B
�B	�B	�2B	�B	�B	��B	�fB	��B
�B
�B
�B
�B
7B
"4B
 �B
#nB
#:B
#�B
%FB
)�B
*0B
2�B
B�B
R B
P�B
V9B
QB
PHB
Q�B
Q�B
[WB
bNB
g�B
pB
s�B
rB
s�B
x�B
��B
�:B
��B
��B
��B
�nB
��B
�B
�:B
��B
��B
�B
�nB
�FB
��B
�B
�kB
�B
�eB
�6B
��B
��B
��B
�6B
��B
�B
�qB
�6B
��B
�'B
��B
��B
�B
�<B
��B
�B
ɺB
ȀB
ŢB
ÖB
ĜB
�mB
�aB
��B
��B
ƨB
��B
�3B
�'B
B
��B
��B
��B
��B
��B
��B
�BB
�gB
�9B
��B
�9B
�[B
�wB
��B
��B
�zB
ɆB
�RB
�^B
��B
�FB
�B
�B
��B
��B
�BB
B
�B
��B
�UB
�B
ɺB
�pB
�0B
�B
��B
��B
ҽB
��B
��B
҉B
��B
�KB
ٴB
��B
�)B
�B
�WB
�B
��B
�B
�B
�QB
��B
�B
�yB
�yB
��B
��B
�B
�]B
�]B
��B
�AB
�AB
�B
�cB
�B
�B
�B
�)B
�B
��B
��B
�B
�B
�QB
�B
�B
�yB
�B
�yB
�B
�B
��B
�B
�"B
�B
�B
�mB
�2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      BU�BUwBYtBS�B[�BS�BQ�BP�BQ/BR�BSBUBZBb`BhzBi�BosBl�Bk�BkhBj�Bm�BqRB~�B��B�\BٮB��B�~B	�B	8PB	Y�B	v�B	�\B	��B	�B
�B
3.B
@`B
98B
G�B
P�B
Z&B
/�B
�B
TB
�B
#B	�EB	�mB
	�B	�hB	�+B	�B	�=B	��B	��B	��B	u}B	l�B	tB	�B	�kB	�\B	�XB	��B	ËB	�sB	��B	�.B	��B	�<B	��B
B
&	B
D�B
U�B
n�B
��B
��B
��B
��B
��B
�,B
��B
ǴB
�,B
�>B
��B
�AB
ǹB
�B
��B
�B
˯B
�JB
��B
�B
��B
�B
�7B
�B
�"B
�.B
�LB
�AB
�	B
��B
�JB
�B
��B
��B
��B
�tB
�B
�/B
�EB
ڒB
١B
�;B
��B
ӹB
��B
͢B
�B
�B
�B
˻B
�B
�NB
£B
B
��B
�jB
��B
�B
�LB
�zB
�B
��B
��B
�B
�[B
�B
��B
��B
�)B
��B
��B
��B
��B
�nB
��B
�=B
�?B
��B
��B
��B
��B
�lB
�B
�kB
�NB
�IB
��B
��B
��B
��B
��B
�dB
�B
��B
��B
�sB
�B
�?B
��B
�B
��B
��B
� B
�ZB
��B
��B
��B
�!B
��B
��B
��B
�xB
�5B
��B
�lB
��B
�	B
��B
�,B
��B
��B
�B
�B
��B
��B
��B
�6B
��B
�YB
��B
��B
��B
�B
}�B
|�B
|$B
{�B
z+B
x�B
x�B
v�B
v!B
u�B
rB
p�B
p�B
o�B
oB
n]B
m�B
lBB
jB
i3B
gjB
gB
b�B
a�B
a�B
_4B
\�B
[�B
Z�B
[�B
[�B
X'B
V�B
S�B
Q|B
Q-B
P�B
OB
N�B
N`B
LJB
JFB
J_B
K"B
IPB
F�B
GB
GLB
C�B
BB
BpB
B_B
?<B
>�B
<oB
<`B
; B
:B
9�B
9TB
8�B
8�B
8B
7�B
6�B
5�B
5�B
5�B
4�B
4AB
3+B
3GB
3�B
2B
2=B
3�B
0�B
2kB
.�B
.�B
0�B
.�B
/B
0�B
/�B
/B
,�B
,B
,�B
)�B
)�B
(\B
(�B
(�B
'*B
%�B
&B
'�B
&eB
#�B
#EB
#�B
$�B
$gB
#�B
!�B
 B
�B
BB
�B
 �B
hB
�B
B
�B
�B
FB
:B
 B
GB
5B
�B
�B
rB
�B
�B
B
�B
�B
KB
�B
B
=B
�B
B
�B
B
gB
�B
�B
"B
�B
lB
�B
�B
NB
^B
�B
B
B
3B
�B
aB
�B
�B
�B
B
1B
}B
qB
!B
vB
�B
B
�B
�B
VB
�B
1B
UB
�B
%B
�B
QB
vB
cB
�B
�B
VB
xB
=B
bB
hB
�B
�B
B
�B
�B
�B
qB
�B
!B
�B
�B
�B
wB
AB
�B
�B
�B
�B
�B
�B
EB
�B
�B
�B
�B
�B
�B
VB
�B
�B
�B
^B
SB
B
[B
UB
B
B
B
 B
�B
CB
�B
HB
�B
�B
B
�B
�B
 nB
 B
 �B
 �B
 �B
![B
"�B
"vB
!�B
"tB
#FB
#HB
#bB
"�B
#OB
$�B
$vB
$GB
#�B
$`B
$�B
%AB
%rB
%lB
%�B
&)B
&.B
&�B
'hB
'�B
'�B
'cB
'aB
'�B
)7B
(JB
)!B
*B
)xB
)UB
)�B
)~B
)�B
*uB
)�B
)�B
)�B
)�B
+{B
,�B
,`B
,CB
,�B
,�B
,rB
,�B
-zB
-OB
-OB
.yB
.dB
.qB
.�B
/B
/�B
0�B
/�B
0]B
0�B
18B
1�B
1/B
1�B
1�B
2'B
2,B
1�B
2B
2cB
2�B
3)B
3.B
3.B
3PB
3&B
3dB
3MB
3GB
3�B
3�B
4qB
3�B
4hB
4AB
4�B
5�B
5�B
5rB
6
B
6�B
6BB
6�B
7(B
7aB
8#B
89B
88B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:B
:)B
:`B
:ZB
:�B
;�B
;�B
<(B
<"B
<B
<%B
<cB
=5B
=qB
=�B
=�B
>tB
>�B
?/B
?ZB
@<B
@B
?�B
?�B
@B
@�B
@�B
A�B
AvB
A�B
A�B
A�B
A�B
BB
B�B
B�B
BOB
B�B
C�B
CJB
CNB
C?B
CVB
C�B
D<B
DB
DRB
D�B
D�B
E�B
E�B
E�B
FB
E�B
F�B
F�B
F�B
F�B
G(B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
IbB
I�B
I�B
I�B
JEB
J�B
K9B
K�B
K�B
K�B
LLB
L�B
LLB
LrB
L�B
L�B
L�B
M9B
M6B
M'B
M*B
MMB
M�B
M�B
M�B
N$B
NRB
N�B
N�B
OEB
OFB
OUB
O�B
OjB
O�B
O�B
O�B
PB
PB
O�B
PB
PB
P}B
QB
P�B
P�B
QB
QDB
Q5B
Q=B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R>B
RB
R9B
R�B
R�B
R�B
R�B
STB
SbB
SyB
T B
S�B
TMB
T�B
U
B
UB
UXB
UtB
VLB
U�B
U�B
V&B
V�B
W;B
W=B
W B
WB
WlB
W�B
W�B
W�B
W�B
W�B
W�B
XB
X�B
YIB
YB
Y-B
ZkB
Z�B
ZyB
ZbB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\!B
\nB
\@B
\�B
\�B
\�B
\�B
]`B
^!B
^�B
^\B
^�B
^_B
^_B
^9B
^B
^B
^YB
^(B
^9B
^bB
_B
_�B
_�B
`RB
`�B
`�B
`�B
`�B
a]B
a�B
a�B
a�B
a�B
b9B
b�B
cB
cKB
c4B
caB
c�B
c�B
c�B
c�B
d|B
d�B
eB
eB
e5B
e�B
erB
epB
e�B
e�B
e�B
fB
f B
f/B
f�B
f�B
gGB
g�B
hB
h(B
haB
hTB
h2B
h\B
h�B
h�B
h�B
h�B
iTB
i|B
i�B
iWB
i�B
i�B
i�B
i�B
i�B
j,B
j�B
j�B
j�B
kyB
k~B
k�B
lcB
l�B
l~B
l�B
l�B
l�B
mB
m`B
m�B
ngB
n�B
n�B
n�B
o#B
o}B
p^B
puB
pB
p�B
p�B
p�B
q
B
q!B
q"B
q%B
qHB
q�B
r@B
r6B
q�B
r5B
r)B
r\B
rkB
rlB
r�B
r�B
r�B
sB
s<B
s2B
s�B
ssB
s�B
s�B
s�B
s�B
s�B
tB
s�B
s�B
tIB
tVB
t�B
tnB
t�B
uB
u%B
u(B
unB
u�B
u�B
vB
vGB
vB
v�B
vGB
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
w�B
xB
xkB
x`B
x{B
x�B
xdB
x�B
x�B
y
B
y5B
yPB
y�B
y�B
y�B
y�B
y�B
z�B
{'B
{OB
{`B
{EB
{�B
|QB
|+B
|�B
|�B
|�B
|�B
|�B
}BB
}�B
~B
~4B
}�B
}�B
~B
~LB
~�B
~jB
~.B
~<B
~@B
~�B
1B
�B
�HB
�YB
�5B
��B
��B
��B
�;B
�_B
��B
��B
�hB
�eB
�zB
�XB
�wB
��B
��B
��B
��B
��B
��B
�FB
��B
��B
�RB
��B
��B
�B
��B
��B
��B
�/B
�|B
�;B
��B
�6B
��B
��B
��B
�wB
��B
�MB
�B
�B
�iB
�B
��B
�`B
��B
��B
��B
��B
��B
�
B
�NB
�4B
��B
��B
�B
�=G�O�BT�BU2BT�BS&BZ�BT�BT�BS�BX�BT�BPBI�BI�BX�B]�BT,BQ�BLdBS�BT,BP}BPHBP�BQBP�BQNBP}BQ�BQ�BRTBR�BT,BT,BTaBV�BWsBX�BXEBYB[�B\�B^jB`BBbBd�Be�BiDBh
Bg�Bh>Bh>Bh�Bi�Bi�BiyBjKBo5Bm�Bn�Bm�Bm�Bm]Bl�Bl�BlWBkBj�Bj�Bj�Bj�BkBkBl�BiBi�BlWBi�Bk�BlWBlWBk�Bk�Bl�Bl�Bl�Bn�BpBoiBq�Bq�BrBu�BwfBw�Bx�B~(B��B�%B��B�SB��B��B�DB��B��B�B��B�B��B�tB�B�0B�-B�zB�BʌB��B� B��B�[B�2B��B�B�BخB�#BרB�B�B��B�B�ZB�yB�B�B��B��B�B�ZB��B��B��B�+B�`B��B�+B�B�B	�B	B	B	~B	SB	�B	�B	�B	B	�B	\B	�B	(B	$B	$B	'�B	B	'�B	6zB	4nB	K�B	A�B	D�B	AUB	E�B	D3B	E9B	GB	I�B	IRB	HKB	J�B	J#B	K�B	M�B	K�B	IRB	MB	WsB	d&B	YB	l�B	aB	cTB	�CB	xB	��B	��B	��B	|PB	�B	��B	��B	��B	��B	�CB	�B	�B	�HB	�aB	�B	�B	�aB	�^B	��B	ŢB	��B	�0B	�)B	�<B	�^B	�B	�B	��B	�B	� B	�<B	�jB	��B	�B	�B	�pB	��B	�gB	��B	��B	��B	�vB	�5B	��B	�B	�]B	�sB	�2B	��B	��B	�DB	��B	�cB	�AB	��B	�B	��B	�B	��B	��B	�]B
B
uB
�B
;B	��B
 �B
B
SB
�B
�B
�B
1B
�B
	7B
B

=B
JB
B
�B
	�B

	B
	lB
SB
�B
�B
�B
�B
�B
�B
-�B
+�B
�B
�B
3�B
%�B
!B
'B
2aB
-�B
/�B
(�B
.}B
&LB
#nB
�B
�B
VB
�B
B
 'B
 �B
�B
OB
�B
�B
{B
�B
-CB
D�B
:*B
.�B
%FB
?}B
-B
&LB
%zB
+6B
#�B
1�B
0�B
-CB
,�B
*�B
'�B
6FB
?}B
7B
R�B
$�B
#B
%�B
�B
$�B
��B
.IB
B
�B
$B
1'B
hB
&�B
$B
�B

=B
xB
�B
�B

=B
1B
+B

	B
	7B
�B
{B
�B
�B
 �B
"B
{B
�B
B
{B
 iB
 �B
 �B	�]B	��B	�B	�B	��B	�>B	�	B	��B	�2B	�2B	�B	�oB	�B	��B	�B	�B	�MB	�B	�>B	��B	�QB	��B
SB	�mB	��B	ŢB	ǮB	�yB	̘B	�B	�wB	��B	��B	��B	�B	��B	�RB	��B	��B	�B	�B	�$B	��B	��B	��B	�_B	��B	�B	�B	�B	��B	�CB	��B	��B	�_B	�	B	��B	�+B	�AB	�AB	��B	�@B	��B	��B	�B	�IB	}"B	t�B	n�B	jB	�B	��B	v`B	g8B	iDB	d�B	h
B	e`B	ffB	i�B	f�B	ffB	d�B	h�B	iyB	gmB	e�B	g�B	m�B	n/B	l�B	lWB	p�B	p;B	kB	q�B	p;B	p�B	jKB	m�B	tTB	pB	��B	�FB	�4B	xlB	y	B	y�B	y>B	{�B	��B	}�B	|�B	��B	��B	��B	�B	��B	�B	�DB	�~B	�B	�B	�~B	�B	�~B	�B	��B	�zB	��B	�eB	��B	�B	��B	��B	�B	��B	��B	��B	�[B	�B	�zB	�RB	�gB	��B	��B	��B	ϫB	�B	��B	�$B	��B	��B	��B	��B	�tB	��B	�XB	��B	�?B	��B	�?B	��B	��B	�B	��B	��B	�RB	�EB	�tB	�B	��B	�B	�hB	��B	�B	��B	��B	��B	�XB	��B	�
B
�B	�B	�2B	�B	�B	��B	�fB	��B
�B
�B
�B
�B
7B
"4B
 �B
#nB
#:B
#�B
%FB
)�B
*0B
2�B
B�B
R B
P�B
V9B
QB
PHB
Q�B
Q�B
[WB
bNB
g�B
pB
s�B
rB
s�B
x�B
��B
�:B
��B
��B
��B
�nB
��B
�B
�:B
��B
��B
�B
�nB
�FB
��B
�B
�kB
�B
�eB
�6B
��B
��B
��B
�6B
��B
�B
�qB
�6B
��B
�'B
��B
��B
�B
�<B
��B
�B
ɺB
ȀB
ŢB
ÖB
ĜB
�mB
�aB
��B
��B
ƨB
��B
�3B
�'B
B
��B
��B
��B
��B
��B
��B
�BB
�gB
�9B
��B
�9B
�[B
�wB
��B
��B
�zB
ɆB
�RB
�^B
��B
�FB
�B
�B
��B
��B
�BB
B
�B
��B
�UB
�B
ɺB
�pB
�0B
�B
��B
��B
ҽB
��B
��B
҉B
��B
�KB
ٴB
��B
�)B
�B
�WB
�B
��B
�B
�B
�QB
��B
�B
�yB
�yB
��B
��B
�B
�]B
�]B
��B
�AB
�AB
�B
�cB
�B
�B
�B
�)B
�B
��B
��B
�B
�B
�QB
�B
�B
�yB
�B
�yB
�B
�B
��B
�B
�"B
�B
�B
�mB
�2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u'�<�{�<��r=��=.�=�Y<��4<���=!ʒ<��<�-6<��x= �W=��<��r<#�
<#�
<#�
<#�
<#�
<|=}<���<c��<pZi<�#a<�C5<�.�<�:<�Q<#�
<#�
<#�
<d <GY�<?NH<'�<#�
<@.�<#�
<#�
<#�
<#�
<(��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261901032023042619010320230426190103202304261901032023042619010320230426190103SI  SI  ARFMARFM                                                                                                                                                2019100200102020191002001020IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019101200003920191012000039QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019101200003920191012000039QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906580920200109065809IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619010520230426190105IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619010520230426190105IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619010520230426190105IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                