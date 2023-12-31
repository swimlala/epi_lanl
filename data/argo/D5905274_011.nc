CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-05-12T19:29:09Z creation; 2023-04-26T19:24:26Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180512192909  20230426192426  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7315_008643_011                 7315_008643_011                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�_����@�_����11  @�_��i�C@�_��i�C@0g���@0g����dI�i�4/�dI�i�4/11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�  @��R@�G�@�  A   A\)A\)A,��A@��A`  A�Q�A�Q�A���A���A�Q�A�  A߮A�A��B(�BQ�B(�B (�B'�
B/�B8(�B@z�BHQ�BP(�BW�
B`  BhQ�BpQ�Bx(�B�  B��
B��
B��
B��B�  B�  B�{B�=qB�(�B��B�  B�  B��
B�{B�{B�(�B�=qB�{B�{B�z�B��B�  B�{B��B��
B�  B�  B��
B��B��
B�B��
C��C��C  C��C
  C
=C  C  C{C  C��C  C  C  C��C 
=C"
=C$  C&  C'��C)��C,  C-��C/�C1�C3�C5��C8  C:
=C<
=C>  C?��CA��CD  CF{CH
=CI��CL{CN{CP
=CQ��CS��CU��CW��CZ  C\
=C^
=C_��Ca�Cd  Cf
=Ch
=Cj  Cl  Cn  Co�Cr  Ct
=Cu��Cw��Cy��C{�HC}��C�  C���C���C�  C�
=C���C���C�  C�C���C�  C�
=C���C���C�  C�C�C�{C�C�  C�C�  C�  C�C�C�  C���C�C�  C���C�C�C���C�  C�C�  C���C���C�  C���C���C���C�  C�C�C���C���C���C�  C���C���C�  C�
=C�C���C���C���C���C���C���C�
=C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�C���C��C���C�C�  C���C�C�
=C�C���C�  C�  C���C�  C�
=C�
=C�C�
=C�  C���C�C�C���C���C�
=C�
=C�  C���C�  C�  C�
=C�
=C���C���C�C�  C�  C�C�  C�  C�  C�
=C�  C���C���C�C�C�C�  C�  C�
=C�C�C�  C���C�  D �D �DD�DD�D�D� D  D� D  D}qD�qDz�D��D}qD  D}qD	  D	��D	�qD
}qD  D� D�qDz�D��D}qD  D��DD� D  D��D�D� D�qDz�D  D�D�D� D  D� D�qDz�D�D��D  Dz�D�qD�D�D}qD�D��D�RD� DD}qD�qD}qD��D� D �D ��D!D!}qD"  D"�D#  D#��D$D$� D%�D%��D%�RD&xRD&�RD'xRD'�qD(� D)  D)}qD*�D*z�D+  D+��D,D,��D-�D-� D.  D.��D/�D/��D/�qD0� D0�qD1}qD1�qD2}qD2�qD3z�D3��D4}qD5  D5}qD5�qD6� D6��D7}qD8�D8� D8�qD9��D:�D:}qD:�qD;��D;�qD<}qD=  D=� D>  D>}qD?  D?��D@  D@� DA�DA� DA�qDB}qDB�qDC� DC�qDD� DE  DEz�DE�qDF}qDG  DG� DG�qDH� DH�qDI� DJDJ��DK�DK� DK�qDL}qDL�qDM}qDN  DN�DODO��DO�qDP}qDP�qDQ��DR  DR}qDR�qDS}qDS�qDTz�DT�qDU� DV  DV��DW�DW}qDX  DX� DX�qDYz�DZ�DZ��D[�D[��D\�D\��D]�D]}qD]��D^� D_  D_� D`  D`� Da�Da� Db  Db��Dc  Dc� Dd  Dd��De  De}qDf�Df�Dg�Dg�Dh  Dh��Di  Di}qDj  Dj��Dk  Dk}qDl  Dl��Dm  Dm� Dn�Dn�Do  Do� Dp  Dp� Dp�qDq}qDr  Dr� Ds�Ds��Dt  Dt}qDu�Du� Dv  Dv� DwDw� Dx  Dx�Dy  Dy�Dz  Dz}qD{�D{}qD{�qD|}qD}  D}z�D~�D~�D  D��D�  D�=qD�~�D��HD�  D�@ D�� D���D���D�@ D��HD�D�  D�AHD��HD�� D���D�=qD�� D��HD�HD�B�D�~�D�� D�HD�=qD�� D�D�HD�@ D��HD��HD�  D�AHD�� D���D�  D�AHD���D��HD�HD�@ D�� D�� D�HD�AHD���D�D���D�@ D�� D�D�  D�=qD�� D��HD�HD�@ D�~�D��HD�  D�>�D�~�D���D���D�>�D�~�D���D�  D�<)D�� D���D��)D�=qD�� D��HD��D�AHD�� D��qD�  D�AHD�}qD��HD���D�=qD�~�D�� D��D�AHD�� D���D���D�B�D���D�� D�HD�B�D��HD��HD�HD�AHD��HD��HD��qD�=qD�� D�� D��qD�AHD���D�� D�  D�@ D�}qD���D���D�AHD���D���D��D�@ D��HD��HD��D�AHD�� D�� D��)D�<)D�}qD���D��qD�=qD�� D���D��qD�<)D�� D�� D��D�AHD�� D��)D��qD�@ D�� D���D�HD�AHD�|)D���D��D�>�D�}qD��HD�  D�AHD��HD��HD�  D�@ D��HD�� D��D�C�D��HD��HD��D�@ D�� D�D��D�AHD��HD���D��D�AHD��HD�� D�  D�>�D�~�D���D���D�@ D���D��HD���D�>�D�~�D�� D�HD�@ D��HD���D�  D�AHD���D��HD���D�AHD��HD�� D�  D�@ D�� D�� D��D�AHD�}qD��qD�  D�>�D�~�D�D��D�>�D�� D���D�  D�=qD��HD���D�  D�>�D�� D���D��D�C�D�� D���D�  D�>�D�}qD�D��D�@ D�}qD���D�  D�=qD�|)D�� D�HD�AHD�� D���D���D�=qD�� D�D���D�@ D��HD��HD��D�AHD�}qD��)D�  D�B�D��HD���D���D�AHDHD�D�HD�>�D�}qDþ�D�  D�=qDĀ D��HD�HD�=qDŀ Dž�D��)D�AHDƀ D��HD�HD�AHD�~�DǽqD�HD�>�DȀ D��HD���D�@ Dɂ�D��HD���D�@ Dʀ D�� D�HD�B�Dˀ D˾�D���D�@ D̀ D�� D�HD�@ D̀ D;�D�  D�@ D΀ D�� D�  D�@ D�~�D��HD��D�@ D�}qD�� D�HD�AHDсHDѾ�D�  D�>�D�}qD�� D�HD�AHDӀ DӾ�D�  D�@ DԂ�D��HD�HD�AHDՁHD��HD�HD�>�D�~�D��HD�HD�AHDׁHD��HD�HD�@ D�}qDؾ�D���D�@ DفHD�D��D�AHD�~�Dھ�D�  D�@ DہHD�� D�  D�>�D�}qDܾ�D�  D�@ D݀ D��HD�  D�>�Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�~�DྸD�HD�>�D� D��HD�  D�>�D� D��HD�  D�AHD�~�D�)D�HD�C�D�HD侸D�  D�@ D�}qD徸D���D�@ D�HD��HD��D�B�D炏D��HD���D�AHD�HD辸D��qD�>�D�~�D��HD�HD�>�D�~�D�� D�HD�B�D� D�� D�HD�@ D�~�D�qD���D�AHD�}qD��qD�  D�>�D�~�DD���D�>�D�HD���D�HD�>�D�~�D��HD�  D�=qD�~�D��HD�  D�>�D� D�qD�  D�@ D� D��HD��D�AHD�HD�� D�HD�B�D��HD�� D�HD�C�D���D���D���D�@ D�~�D���D�HD�AHD�}qD�� D��D�AHD�� D���D���D�B�D�w
>�Q�?.{?�  ?��R?���?��H@
=@+�@E�@^�R@s33@��@�z�@�G�@���@���@��@��@޸R@�@�
=A�AQ�A\)A�A�A"�\A(��A0  A5A<(�AC33AJ=qAP��AW�A_\)AfffAn{Au�A|(�A���A���A�\)A��\A�A�G�A���A�  A��HA�ffA���A�(�A�
=A�=qA��A�Q�A��HA�{A���A��A�A�Q�A��HA��A�\)A�G�A��HA��A�\)A�G�A�33A�p�A׮Aٙ�AۅA��A߮AᙚA��
A�{A�  A陚A�(�A�{A�Q�A�\A���A��RA���A��HA��A�\)B ��BB�HB  Bp�B�\B�B��B	B
�RB�
B��BB�RB�
B��BB�RB�B��BB�RB�B��B��B�\B�B��B��B�\B\)B Q�B!p�B"�\B#�B$z�B%G�B&ffB'\)B(Q�B)G�B*=qB+33B,  B,��B-�B.�HB0  B0��B1�B2�HB3�
B4��B5�B7
=B8  B8��B9�B:�HB<  B<��B=�B>�HB@  B@��BA�BB�HBD  BD��BE�BF�HBG�BH��BIBJ�RBK�
BL��BN{BO33BP(�BQG�BRffBS�BT��BUBV�HBW�
BX��BZ{B[33B\(�B]G�B^ffB_�B`��Ba��Bb�RBc�
BeG�BfffBg�Bh��Bi��Bj�RBk�
Bl��Bn{Bo33Bp(�Bqp�BrffBs\)BtQ�Bu��BvffBw\)Bxz�By��Bz�RB{�
B|��B}�B
=B�{B��\B��B���B�=qB���B�\)B��B�z�B�
=B���B�(�B��RB�G�B��B�ffB�
=B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B�33B��
B�Q�B��HB�p�B�  B��\B�G�B�B�ffB��HB��B�{B���B�\)B�  B��\B�33B��B�ffB���B��B�{B��RB�G�B��B�z�B�
=B��B�(�B��HB�p�B�  B��\B��B�B�Q�B��HB�p�B�  B��\B��B�B�Q�B���B�p�B�  B���B��B��B�=qB���B�\)B��B�ffB���B�p�B��B�ffB�
=B��B�  B�z�B�
=B��B�{B���B�33B�B�Q�B��HB�p�B�  B��RB�33B��
B�Q�B��HB�p�B�  B��\B�G�B��
B�ffB�
=B�B�=qB��HB��B�ffB���BÙ�B�=qB���B�\)B�  B�z�B�
=BǅB�  Bȏ\B��B�B�Q�B��HB�\)B��B�ffB���B�\)B��B�ffB��HB��BυBϮB��B�{B�=qB�ffB�Q�BиRB���B���B�
=B��B��B��B�G�B�p�Bљ�B�B�B��B�=qB�z�Bҏ\Bң�B���B���B��B�33B�G�B�G�B�p�BӅBӮB�B��B�  B�=qB�z�B���B��HB�
=B��B�G�BՅBՙ�B��
B��B�(�B�=qB�ffB�z�B�z�BָRB��HB���B��B�G�BׅB׮B�  B�{B�ffB؏\BظRB���B��B�\)B�\)B�p�Bٙ�B�B��B�{B�Q�BڸRB��HB�
=B�G�B�p�BۅBۙ�B�B��
B�{B�(�B�Q�B܏\B���B�
=B�G�B�p�Bݙ�B��
B��B�  B�  B�=qB�ffBޏ\B޸RB��B�\)B߅B߮B��
B�  B�=qB�ffB�ffB��\B���B���B��B�\)BᙚB��B�{B�Q�B�\B�\B�RB��HB��B�G�B�B�B�{B�ffB��B���B��B�G�B�p�B�B�  B�=qB�z�B�RB���B�G�B�p�B��
B�=qB�z�B�RB�
=B�\)B�p�B陚B��
B�(�B�Q�B���B��B�\)B�B��B�(�B�=qB�\B���B�
=B�G�B��B�{B�Q�B�RB�RB�
=B�33B�B�{B�ffB�RB�
=B�33B�\)B�B�  B�ffB���B��B�B��
B��B�=qB�\B���B�p�B�B�{B�(�B��\B��HB��B�B�{B�ffB��HB�
=B�G�B���B�  B��\B��HB�33B�p�B�B�(�B��\B�
=B�p�B�B�  B�ffB��RB�G�B�C {C 33C \)C �C �
C
=C=qCp�C�C�RC
=C=qCp�C�\C�RC  C=qCp�C�\C�RC  C33CffC�C�C  C33C\)Cp�C��C�HC�C\)Cz�C��C�
C�CQ�CffC��C��C{CQ�C�C��C��C	
=C	Q�C	�C	�\C	C

=C
G�C
z�C
��C
C
�CG�Cz�C�C�RC
=CG�Cz�C��C��C(�CffC�C�RC��CQ�C�C��C�
C�Cp�C�C��C  C\)C��C�C�CG�Cz�C��C�
C{CffC��CC�CQ�C�\C�C�HC33Cp�C��C��C
=C\)Cz�C�C�C=qCffC��C�C(�C=qC�C��C{C=qCp�C��C{C33CffC��C
=C33CffCC  C(�Cz�CC  C33Cp�C��C{C33Cp�C�
C{C=qC�\C�HC
=C=qC��C�C 
=C G�C �C �C!{C!G�C!�C!��C"�C"\)C"�C#  C#(�C#ffC#��C${C$33C$z�C$�
C%{C%Q�C%�C%�C&{C&\)C&C'  C'(�C'�\C'�
C'��C(G�C(��C(��C){C)p�C)�C)�
C*�C*z�C*��C*�
C+=qC+z�C+�C,  C,Q�C,p�C,�RC-�C-\)C-�\C-�C.=qC.ffC.�C/
=C/33C/�C/�
C0  C0Q�C0��C0��C1
=C1ffC1��C1��C2=qC2z�C2��C3  C3Q�C3z�C3��C4�C4=qC4��C4�C5{C5p�C5�RC5�C6G�C6�C6�C7{C7=qC7�C7�C8{C8ffC8C8�C9G�C9��C9C:{C:ffC:��C:��C;G�C;z�C;�
C<�C<Q�C<�RC<�C=(�C=�\C=��C>
=C>p�C>��C>��C?G�C?p�C?�
C@{C@=qC@��C@�HCA�CA�CA�RCB{CBG�CB�CB��CC{CCp�CC��CC��CDQ�CD�\CDCE(�CE\)CE�RCE��CF(�CF�\CF�RCF��CGffCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             ?�  @   @B�\@�  @��R@�G�@�  A   A\)A\)A,��A@��A`  A�Q�A�Q�A���A���A�Q�A�  A߮A�A��B(�BQ�B(�B (�B'�
B/�B8(�B@z�BHQ�BP(�BW�
B`  BhQ�BpQ�Bx(�B�  B��
B��
B��
B��B�  B�  B�{B�=qB�(�B��B�  B�  B��
B�{B�{B�(�B�=qB�{B�{B�z�B��B�  B�{B��B��
B�  B�  B��
B��B��
B�B��
C��C��C  C��C
  C
=C  C  C{C  C��C  C  C  C��C 
=C"
=C$  C&  C'��C)��C,  C-��C/�C1�C3�C5��C8  C:
=C<
=C>  C?��CA��CD  CF{CH
=CI��CL{CN{CP
=CQ��CS��CU��CW��CZ  C\
=C^
=C_��Ca�Cd  Cf
=Ch
=Cj  Cl  Cn  Co�Cr  Ct
=Cu��Cw��Cy��C{�HC}��C�  C���C���C�  C�
=C���C���C�  C�C���C�  C�
=C���C���C�  C�C�C�{C�C�  C�C�  C�  C�C�C�  C���C�C�  C���C�C�C���C�  C�C�  C���C���C�  C���C���C���C�  C�C�C���C���C���C�  C���C���C�  C�
=C�C���C���C���C���C���C���C�
=C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�C���C��C���C�C�  C���C�C�
=C�C���C�  C�  C���C�  C�
=C�
=C�C�
=C�  C���C�C�C���C���C�
=C�
=C�  C���C�  C�  C�
=C�
=C���C���C�C�  C�  C�C�  C�  C�  C�
=C�  C���C���C�C�C�C�  C�  C�
=C�C�C�  C���C�  D �D �DD�DD�D�D� D  D� D  D}qD�qDz�D��D}qD  D}qD	  D	��D	�qD
}qD  D� D�qDz�D��D}qD  D��DD� D  D��D�D� D�qDz�D  D�D�D� D  D� D�qDz�D�D��D  Dz�D�qD�D�D}qD�D��D�RD� DD}qD�qD}qD��D� D �D ��D!D!}qD"  D"�D#  D#��D$D$� D%�D%��D%�RD&xRD&�RD'xRD'�qD(� D)  D)}qD*�D*z�D+  D+��D,D,��D-�D-� D.  D.��D/�D/��D/�qD0� D0�qD1}qD1�qD2}qD2�qD3z�D3��D4}qD5  D5}qD5�qD6� D6��D7}qD8�D8� D8�qD9��D:�D:}qD:�qD;��D;�qD<}qD=  D=� D>  D>}qD?  D?��D@  D@� DA�DA� DA�qDB}qDB�qDC� DC�qDD� DE  DEz�DE�qDF}qDG  DG� DG�qDH� DH�qDI� DJDJ��DK�DK� DK�qDL}qDL�qDM}qDN  DN�DODO��DO�qDP}qDP�qDQ��DR  DR}qDR�qDS}qDS�qDTz�DT�qDU� DV  DV��DW�DW}qDX  DX� DX�qDYz�DZ�DZ��D[�D[��D\�D\��D]�D]}qD]��D^� D_  D_� D`  D`� Da�Da� Db  Db��Dc  Dc� Dd  Dd��De  De}qDf�Df�Dg�Dg�Dh  Dh��Di  Di}qDj  Dj��Dk  Dk}qDl  Dl��Dm  Dm� Dn�Dn�Do  Do� Dp  Dp� Dp�qDq}qDr  Dr� Ds�Ds��Dt  Dt}qDu�Du� Dv  Dv� DwDw� Dx  Dx�Dy  Dy�Dz  Dz}qD{�D{}qD{�qD|}qD}  D}z�D~�D~�D  D��D�  D�=qD�~�D��HD�  D�@ D�� D���D���D�@ D��HD�D�  D�AHD��HD�� D���D�=qD�� D��HD�HD�B�D�~�D�� D�HD�=qD�� D�D�HD�@ D��HD��HD�  D�AHD�� D���D�  D�AHD���D��HD�HD�@ D�� D�� D�HD�AHD���D�D���D�@ D�� D�D�  D�=qD�� D��HD�HD�@ D�~�D��HD�  D�>�D�~�D���D���D�>�D�~�D���D�  D�<)D�� D���D��)D�=qD�� D��HD��D�AHD�� D��qD�  D�AHD�}qD��HD���D�=qD�~�D�� D��D�AHD�� D���D���D�B�D���D�� D�HD�B�D��HD��HD�HD�AHD��HD��HD��qD�=qD�� D�� D��qD�AHD���D�� D�  D�@ D�}qD���D���D�AHD���D���D��D�@ D��HD��HD��D�AHD�� D�� D��)D�<)D�}qD���D��qD�=qD�� D���D��qD�<)D�� D�� D��D�AHD�� D��)D��qD�@ D�� D���D�HD�AHD�|)D���D��D�>�D�}qD��HD�  D�AHD��HD��HD�  D�@ D��HD�� D��D�C�D��HD��HD��D�@ D�� D�D��D�AHD��HD���D��D�AHD��HD�� D�  D�>�D�~�D���D���D�@ D���D��HD���D�>�D�~�D�� D�HD�@ D��HD���D�  D�AHD���D��HD���D�AHD��HD�� D�  D�@ D�� D�� D��D�AHD�}qD��qD�  D�>�D�~�D�D��D�>�D�� D���D�  D�=qD��HD���D�  D�>�D�� D���D��D�C�D�� D���D�  D�>�D�}qD�D��D�@ D�}qD���D�  D�=qD�|)D�� D�HD�AHD�� D���D���D�=qD�� D�D���D�@ D��HD��HD��D�AHD�}qD��)D�  D�B�D��HD���D���D�AHDHD�D�HD�>�D�}qDþ�D�  D�=qDĀ D��HD�HD�=qDŀ Dž�D��)D�AHDƀ D��HD�HD�AHD�~�DǽqD�HD�>�DȀ D��HD���D�@ Dɂ�D��HD���D�@ Dʀ D�� D�HD�B�Dˀ D˾�D���D�@ D̀ D�� D�HD�@ D̀ D;�D�  D�@ D΀ D�� D�  D�@ D�~�D��HD��D�@ D�}qD�� D�HD�AHDсHDѾ�D�  D�>�D�}qD�� D�HD�AHDӀ DӾ�D�  D�@ DԂ�D��HD�HD�AHDՁHD��HD�HD�>�D�~�D��HD�HD�AHDׁHD��HD�HD�@ D�}qDؾ�D���D�@ DفHD�D��D�AHD�~�Dھ�D�  D�@ DہHD�� D�  D�>�D�}qDܾ�D�  D�@ D݀ D��HD�  D�>�Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�~�DྸD�HD�>�D� D��HD�  D�>�D� D��HD�  D�AHD�~�D�)D�HD�C�D�HD侸D�  D�@ D�}qD徸D���D�@ D�HD��HD��D�B�D炏D��HD���D�AHD�HD辸D��qD�>�D�~�D��HD�HD�>�D�~�D�� D�HD�B�D� D�� D�HD�@ D�~�D�qD���D�AHD�}qD��qD�  D�>�D�~�DD���D�>�D�HD���D�HD�>�D�~�D��HD�  D�=qD�~�D��HD�  D�>�D� D�qD�  D�@ D� D��HD��D�AHD�HD�� D�HD�B�D��HD�� D�HD�C�D���D���D���D�@ D�~�D���D�HD�AHD�}qD�� D��D�AHD�� D���D���D�B�G�O�>�Q�?.{?�  ?��R?���?��H@
=@+�@E�@^�R@s33@��@�z�@�G�@���@���@��@��@޸R@�@�
=A�AQ�A\)A�A�A"�\A(��A0  A5A<(�AC33AJ=qAP��AW�A_\)AfffAn{Au�A|(�A���A���A�\)A��\A�A�G�A���A�  A��HA�ffA���A�(�A�
=A�=qA��A�Q�A��HA�{A���A��A�A�Q�A��HA��A�\)A�G�A��HA��A�\)A�G�A�33A�p�A׮Aٙ�AۅA��A߮AᙚA��
A�{A�  A陚A�(�A�{A�Q�A�\A���A��RA���A��HA��A�\)B ��BB�HB  Bp�B�\B�B��B	B
�RB�
B��BB�RB�
B��BB�RB�B��BB�RB�B��B��B�\B�B��B��B�\B\)B Q�B!p�B"�\B#�B$z�B%G�B&ffB'\)B(Q�B)G�B*=qB+33B,  B,��B-�B.�HB0  B0��B1�B2�HB3�
B4��B5�B7
=B8  B8��B9�B:�HB<  B<��B=�B>�HB@  B@��BA�BB�HBD  BD��BE�BF�HBG�BH��BIBJ�RBK�
BL��BN{BO33BP(�BQG�BRffBS�BT��BUBV�HBW�
BX��BZ{B[33B\(�B]G�B^ffB_�B`��Ba��Bb�RBc�
BeG�BfffBg�Bh��Bi��Bj�RBk�
Bl��Bn{Bo33Bp(�Bqp�BrffBs\)BtQ�Bu��BvffBw\)Bxz�By��Bz�RB{�
B|��B}�B
=B�{B��\B��B���B�=qB���B�\)B��B�z�B�
=B���B�(�B��RB�G�B��B�ffB�
=B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B�33B��
B�Q�B��HB�p�B�  B��\B�G�B�B�ffB��HB��B�{B���B�\)B�  B��\B�33B��B�ffB���B��B�{B��RB�G�B��B�z�B�
=B��B�(�B��HB�p�B�  B��\B��B�B�Q�B��HB�p�B�  B��\B��B�B�Q�B���B�p�B�  B���B��B��B�=qB���B�\)B��B�ffB���B�p�B��B�ffB�
=B��B�  B�z�B�
=B��B�{B���B�33B�B�Q�B��HB�p�B�  B��RB�33B��
B�Q�B��HB�p�B�  B��\B�G�B��
B�ffB�
=B�B�=qB��HB��B�ffB���BÙ�B�=qB���B�\)B�  B�z�B�
=BǅB�  Bȏ\B��B�B�Q�B��HB�\)B��B�ffB���B�\)B��B�ffB��HB��BυBϮB��B�{B�=qB�ffB�Q�BиRB���B���B�
=B��B��B��B�G�B�p�Bљ�B�B�B��B�=qB�z�Bҏ\Bң�B���B���B��B�33B�G�B�G�B�p�BӅBӮB�B��B�  B�=qB�z�B���B��HB�
=B��B�G�BՅBՙ�B��
B��B�(�B�=qB�ffB�z�B�z�BָRB��HB���B��B�G�BׅB׮B�  B�{B�ffB؏\BظRB���B��B�\)B�\)B�p�Bٙ�B�B��B�{B�Q�BڸRB��HB�
=B�G�B�p�BۅBۙ�B�B��
B�{B�(�B�Q�B܏\B���B�
=B�G�B�p�Bݙ�B��
B��B�  B�  B�=qB�ffBޏ\B޸RB��B�\)B߅B߮B��
B�  B�=qB�ffB�ffB��\B���B���B��B�\)BᙚB��B�{B�Q�B�\B�\B�RB��HB��B�G�B�B�B�{B�ffB��B���B��B�G�B�p�B�B�  B�=qB�z�B�RB���B�G�B�p�B��
B�=qB�z�B�RB�
=B�\)B�p�B陚B��
B�(�B�Q�B���B��B�\)B�B��B�(�B�=qB�\B���B�
=B�G�B��B�{B�Q�B�RB�RB�
=B�33B�B�{B�ffB�RB�
=B�33B�\)B�B�  B�ffB���B��B�B��
B��B�=qB�\B���B�p�B�B�{B�(�B��\B��HB��B�B�{B�ffB��HB�
=B�G�B���B�  B��\B��HB�33B�p�B�B�(�B��\B�
=B�p�B�B�  B�ffB��RB�G�B�C {C 33C \)C �C �
C
=C=qCp�C�C�RC
=C=qCp�C�\C�RC  C=qCp�C�\C�RC  C33CffC�C�C  C33C\)Cp�C��C�HC�C\)Cz�C��C�
C�CQ�CffC��C��C{CQ�C�C��C��C	
=C	Q�C	�C	�\C	C

=C
G�C
z�C
��C
C
�CG�Cz�C�C�RC
=CG�Cz�C��C��C(�CffC�C�RC��CQ�C�C��C�
C�Cp�C�C��C  C\)C��C�C�CG�Cz�C��C�
C{CffC��CC�CQ�C�\C�C�HC33Cp�C��C��C
=C\)Cz�C�C�C=qCffC��C�C(�C=qC�C��C{C=qCp�C��C{C33CffC��C
=C33CffCC  C(�Cz�CC  C33Cp�C��C{C33Cp�C�
C{C=qC�\C�HC
=C=qC��C�C 
=C G�C �C �C!{C!G�C!�C!��C"�C"\)C"�C#  C#(�C#ffC#��C${C$33C$z�C$�
C%{C%Q�C%�C%�C&{C&\)C&C'  C'(�C'�\C'�
C'��C(G�C(��C(��C){C)p�C)�C)�
C*�C*z�C*��C*�
C+=qC+z�C+�C,  C,Q�C,p�C,�RC-�C-\)C-�\C-�C.=qC.ffC.�C/
=C/33C/�C/�
C0  C0Q�C0��C0��C1
=C1ffC1��C1��C2=qC2z�C2��C3  C3Q�C3z�C3��C4�C4=qC4��C4�C5{C5p�C5�RC5�C6G�C6�C6�C7{C7=qC7�C7�C8{C8ffC8C8�C9G�C9��C9C:{C:ffC:��C:��C;G�C;z�C;�
C<�C<Q�C<�RC<�C=(�C=�\C=��C>
=C>p�C>��C>��C?G�C?p�C?�
C@{C@=qC@��C@�HCA�CA�CA�RCB{CBG�CB�CB��CC{CCp�CC��CC��CDQ�CD�\CDCE(�CE\)CE�RCE��CF(�CF�\CF�RCF��CGffCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�A�A�A�A�A�A�-A�+A�(�A��A��A��A� �A��A��A��A� �A� �A��A��A�{A�oA�oA�oA�oA�{A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�"�A�$�A�$�A�"�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�$�A���A��A�5?A�A�A�G�Aʏ\A�VA�VA�5?A�ĜA�9XA���A��A�E�A��wA�1A�XA�^5A�
=A�ffA��`A�A��A�(�A�bNA�JA�n�A�p�A��FA�9XA�ȴA�`BA�t�A��yA���A�dZA��A��PA��A�A�I�A�ƨA��A��PA���A�`BA��A��
A�9XA�A�A�$�A�9XA�&�A���A��+A�1'A�A���A�O�A�=qA���A��A��A��HA�VA�$�A��A�-A~��A|�Aw�#AvE�ApZAjI�Ae%A]O�AWhsATZAS"�AP��AL(�AG�-AEhsA@��A=oA<�A<��A<E�A:bNA933A7�A4 �A2Q�A1�A1�PA1C�A0��A0�A/C�A-�A,5?A+�;A+��A,  A+A*�A)l�A(��A(5?A(JA'�A'`BA&�9A%��A%p�A$ffA#��A#VA"~�A"v�A"1'A ��A��A��A�A+A�AƨA��A33A/A��A��A1A  A��A�A�-A�A�DA	��A|�A�mA�9A
=A��A1'A�
A�^A{A��A��Av�A  A�TA�^A
�HA
n�A
v�A
-A	�FA	�A�An�AJA��A��AbNA�PA�9A��A��A�A^5AbA%A��AjA��A ��A A�A 1@�\)@�"�@�hs@���@�Ĝ@�Z@�ff@�-@��@��\@���@�`B@���@�r�@�(�@���@�@�E�@�7L@�r�@��`@��@��@���@�  @�t�@���@�ff@�5?@�7L@�b@�l�@�o@���@�!@�!@��#@���@��m@���@ᙚ@���@�Z@���@ߥ�@ߕ�@�\)@އ+@���@݉7@�O�@���@ܼj@�z�@�j@܃@�A�@�\)@�M�@�V@١�@���@ج@؋D@؋D@�Q�@�1'@׍P@���@ָR@�{@��#@��@ԓu@��@Ӆ@�;d@Ұ!@�$�@��T@Ѳ-@љ�@�7L@��/@�j@�Z@� �@υ@�33@��@��@�n�@�J@ͺ^@�G�@���@�1@˾w@˝�@�\)@���@��@�/@ȼj@�9X@���@�+@Ɨ�@�$�@Ł@��@�ƨ@�@���@�X@���@���@��D@�1'@�b@��@���@�;d@��H@��+@�=q@��#@�V@���@�r�@�9X@�ƨ@��@�"�@�M�@�O�@���@�Z@�(�@���@��@��R@�-@��T@�X@���@��u@�j@�I�@��;@���@��!@�V@��^@��/@��D@�j@�I�@�1@�+@���@��@��@���@�p�@��@���@�r�@�I�@���@�S�@�33@���@�n�@�$�@���@�/@���@�Q�@��@��
@���@�dZ@�C�@��!@��@�@��^@��-@���@���@��@��`@�bN@��
@��P@�l�@�S�@�;d@���@�~�@�v�@�ff@�^5@�V@�M�@�E�@�-@�J@��@��^@�7L@��@�A�@��;@���@�K�@���@�ff@���@���@�7L@��@��@��@���@���@�j@�1'@��@�1@��@��w@�C�@��@���@���@�ff@�$�@��#@��-@�hs@��/@���@�r�@��@��P@��@��@���@��@���@�O�@�7L@�&�@���@���@�j@�  @���@��F@���@��@�K�@��@�V@��@���@���@���@��@��F@��@���@�|�@�+@���@��^@�x�@�?}@��@�%@���@��@�z�@�C�@���@�n�@�E�@��^@�x�@�%@��j@���@�I�@�b@��@�ƨ@��@��@�dZ@�S�@�C�@�33@��@���@�~�@�5?@��^@�p�@�/@��@��@�A�@��m@���@�+@�ȴ@��\@�=q@�@�?}@��@���@��9@�j@�w@�@~ȴ@~ff@~{@}�-@}O�@}V@|�/@|9X@{��@{"�@z�H@z��@z=q@zJ@y��@yX@y7L@y%@x�9@xbN@x �@w�@w��@w|�@w\)@v��@u�@uO�@t��@t�@s�@s"�@r��@r-@q�7@p��@pb@o��@o�P@oK�@n��@n��@nv�@nE�@nE�@nE�@n{@m@m�h@m�@mV@l�/@l9X@l�@k�@j�@jM�@i��@i�#@i��@i�7@ihs@i&�@hĜ@hbN@g�;@g�w@g��@f�+@e��@e��@e�h@e`B@d�j@dI�@d�@c��@c��@c��@c��@c�m@cƨ@c��@ct�@cC�@b�H@bJ@a7L@`�9@`r�@`r�@`bN@`A�@` �@`  @_�@^��@^@]p�@\�@\��@\z�@[ƨ@Z��@Zn�@Zn�@Z=q@Y��@Yhs@Y�@X��@X1'@W�P@W�@V�y@V�R@V��@Vv�@VE�@VE�@V$�@U��@U@Up�@T�D@S��@SdZ@R�H@R=q@R�@Q�#@Q�^@Q��@Q��@Qhs@Pr�@O��@O
=@Nv�@NE�@N$�@N{@N@M��@M��@M�@Mp�@Mp�@MV@L�@LI�@K��@Kƨ@K��@J�@J�\@JM�@J=q@I��@I7L@H�@H1'@G�;@G�w@G�P@Fȴ@F@EO�@D�@D�@D�D@DI�@D(�@D1@Cƨ@C��@C33@B-@AG�@A%@@�`@@�u@@b@@  @?�;@?�P@>�R@>v�@=@=��@=��@=`B@=O�@=O�@=O�@=/@=V@=V@<��@<��@<�D@<�@;ƨ@;t�@;S�@:�H@:=q@9��@9�@8�`@8Ĝ@8�9@8r�@8b@8  @7�@7�;@7�@7|�@6��@6$�@6{@5�T@5@5�-@5/@4�j@4(�@3�
@333@2��@2�@1��@1X@0��@0�@0A�@0b@/�;@/��@/K�@.ȴ@.v�@.ff@.E�@.$�@.@-�@-��@-��@-p�@-?}@-/@,��@,�@,I�@,�@+��@+�F@+dZ@+S�@+"�@*��@*^5@*�@)��@)��@)�@)�#@)�7@(��@(Ĝ@(��@(bN@( �@'��@'��@';d@&�R@&v�@&E�@&5?@%�T@%@%�h@%�@%�@%O�@%O�@%O�@%O�@%?}@$�@$z�@#�m@#��@#�@#t�@#C�@"�!@"-@!��@!�#@!��@!��@!&�@!%@ ��@ �u@ �@ A�@�@|�@�y@ȴ@�R@��@��@�+@V@{@�T@��@z�@9X@ƨ@��@dZ@�H@��@�@�#@��@hs@�`@�9@�u@A�@b@�w@|�@l�@;d@;d@+@�y@��@��@�+@��@��@V@5?@$�@�@�-@�@�j@�D@z�@j@Z@I�@�@�@�@��@�
@�@��@n�@�@��@�@��@�#@�@�#@�#@��@��@��@&�@��@ �@��@�w@�@��@�P@\)@�@�@��@�+@�+@E�@@@p�@O�@�@�/@�@�D@j@j@j@9X@��@�
@��@�@t�@dZ@S�@
�H@
n�@
n�@
n�@
M�@	�#@	��@	��@	X@	�@�`@Ĝ@�9@�@Q�@1'@ �@b@�;@�@��@�P@|�@|�@l�@l�@l�@l�@\)@\)@\)@K�@;dA�ZA�XA�O�A�S�A�O�A�?}A�?}A�C�A�C�A�?}A�E�A�E�A�7LA�(�A�+A�-A�+A�/A�1'A� �A� �A� �A�{A��A��A��A�{A��A�"�A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A�oA�oA�oA�{A�oA�oA�oA�{A�{A�{A�{A�{A��A��A��A�{A�{A��A�{A�{A�{A�{A�{A�{A�{A�oA�oA�oA�bA�bA�bA�bA�oA�bA�bA�bA�oA�oA�{A�oA�{A�{A�{A�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A� �A� �A� �A� �A� �A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A� �A� �A� �A� �A��A� �A� �A� �A� �A��A��A��A� �A��A� �A� �A� �A�"�A�$�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�&�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�(�A�(�A�(�A�&�A�(�A�&�A�&�A�&�A�&�A�"�A�{A�{A�VA�VA�1A���A���A��TA��/A��
Aҩ�A҇+A�t�A�  Aџ�AыDA�t�A��A�A�A�~�A�M�A��A�x�A��
A̗�A�v�A�ffA�E�A�"�A�
=A���A˥�A�z�A�S�A�O�A�/A���A�ȴA�ĜAʲ-A�~�A�hsA�dZA�dZA�`BA�ZA�\)A�XA�O�A�O�A�I�A�I�A�E�A�C�A�=qA�-A��#Aɥ�AɑhA�v�A�p�A�dZA�Q�A�O�A�G�A�A�A�=qA�;dA�=qA�33A�-A�"�A� �A��A�"�A� �A��A��A�bA�
=A�%A�%A�1A�A��A��A��mA���A��
A���A���A���A���A���A���A���A�ȴA�ȴAȶFAȲ-AȮAȩ�Aȩ�Aȧ�Aȝ�Aȝ�Aȕ�AȋDA�z�A�t�A�l�A�hsA�^5A�r�A�n�A�l�A�jA�bNA�XA�Q�A�VA�I�A�+A� �A�$�A��A��A�bA�JA�A���A��A��A��HA�ȴAǥ�A�r�A�`BA�+A�VA�
=A�A���A��A��A��`A���A���A�ƨA���AƸRAƮAƮAƩ�Aƥ�AƟ�AƍPAƑhAƅA�|�A�t�A�r�A�jA�S�A�G�A�E�A�;dA�;dA�5?A��A�VA�A��Aũ�Ať�AŋDA�z�A�bNA�(�A�A�A�A��#A���Aò-Aã�AÏ\A�~�A�p�A�ffA�;dA��A�ƨA�z�A�1A�ƨA�x�A�K�A��A�jA�{A��/A��A���A�|�A�r�A�bNA�^5A�\)A�VA�S�A�O�A�C�A�?}A�9XA�9XA�1'A�&�A�(�A�&�A�{A���A��A��/A�ȴA���A���A���A���A���A���A�ĜA���A�n�A�^5A�I�A�+A���A���A���A�O�A��A�-A��#A���A���A�\)A�Q�A�+A���A�|�A�\)A�O�A�K�A�9XA���A�x�A�&�A��A���A��DA�x�A�`BA�?}A��A���A���A���A��!A��A�dZA�Q�A�5?A�VA��9A�t�A�ffA�VA�E�A�/A��A���A���A��7A�v�A�ffA�M�A�+A��A�oA�oA�
=A�A��mA��#A�ƨA��A���A���A��uA��PA��A�v�A�p�A�n�A�hsA�`BA�XA�Q�A�M�A�I�A�E�A�9XA� �A�JA���A���A���A��A�p�A�jA�ffA�^5A�S�A�O�A�E�A�5?A�-A� �A��A�bA�VA�1A�A�  A���A���A��A��yA��TA��A��wA���A��\A�1'A��#A�x�A�ĜA�E�A��A��wA�ffA�7LA���A��A�5?A�%A��mA�A��^A��A���A��\A��A�x�A�l�A�^5A�VA�O�A�7LA��A�bA�JA�  A��yA��HA��HA���A��9A���A���A���A���A��A�|�A�v�A�jA�\)A�O�A�9XA�
=A��`A���A��jA���A�~�A�M�A�-A���A�ZA�C�A��A���A���A���A���A�x�A�C�A�33A�VA���A��hA�jA�O�A�1'A���A��^A��A�jA�O�A�33A�(�A�{A�  A���A���A���A���A��A��`A��TA��HA��
A��FA���A���A�ffA�?}A�{A��/A��7A�(�A���A�z�A�n�A�K�A��A�ĜA��A�5?A���A�|�A�A�A��A��#A���A�^5A�5?A��A��/A���A�n�A�$�A���A���A���A��7A�n�A�I�A�?}A�9XA�33A�(�A��A�%A���A��A�7LA��PA�VA��A�oA�
=A�%A�1A�  A���A��A��A��yA��/A��
A���A��A���A�hsA���A�v�A�?}A��HA��9A�n�A���A�z�A�C�A� �A�bA��A��HA��/A���A��A���A��uA�~�A�bNA�G�A�/A�VA��A��yA���A���A�t�A�?}A��A���A�Q�A�+A���A�l�A��A�v�A�K�A�G�A�E�A�?}A�9XA�9XA�/A�1'A�/A�(�A�(�A�&�A�$�A�&�A� �A�"�A�"�A�{A�{A��
A��A��#A��/A��#A���A��+A��A�t�A�ffA�\)A�Q�A�G�A�9XA�1'A�1'A�&�A��A�
=A��TA��PA�1'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             A�S�A�A�A�A�A�A�A�-A�+A�(�A��A��A��A� �A��A��A��A� �A� �A��A��A�{A�oA�oA�oA�oA�{A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�"�A�$�A�$�A�"�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�$�A���A��A�5?A�A�A�G�Aʏ\A�VA�VA�5?A�ĜA�9XA���A��A�E�A��wA�1A�XA�^5A�
=A�ffA��`A�A��A�(�A�bNA�JA�n�A�p�A��FA�9XA�ȴA�`BA�t�A��yA���A�dZA��A��PA��A�A�I�A�ƨA��A��PA���A�`BA��A��
A�9XA�A�A�$�A�9XA�&�A���A��+A�1'A�A���A�O�A�=qA���A��A��A��HA�VA�$�A��A�-A~��A|�Aw�#AvE�ApZAjI�Ae%A]O�AWhsATZAS"�AP��AL(�AG�-AEhsA@��A=oA<�A<��A<E�A:bNA933A7�A4 �A2Q�A1�A1�PA1C�A0��A0�A/C�A-�A,5?A+�;A+��A,  A+A*�A)l�A(��A(5?A(JA'�A'`BA&�9A%��A%p�A$ffA#��A#VA"~�A"v�A"1'A ��A��A��A�A+A�AƨA��A33A/A��A��A1A  A��A�A�-A�A�DA	��A|�A�mA�9A
=A��A1'A�
A�^A{A��A��Av�A  A�TA�^A
�HA
n�A
v�A
-A	�FA	�A�An�AJA��A��AbNA�PA�9A��A��A�A^5AbA%A��AjA��A ��A A�A 1@�\)@�"�@�hs@���@�Ĝ@�Z@�ff@�-@��@��\@���@�`B@���@�r�@�(�@���@�@�E�@�7L@�r�@��`@��@��@���@�  @�t�@���@�ff@�5?@�7L@�b@�l�@�o@���@�!@�!@��#@���@��m@���@ᙚ@���@�Z@���@ߥ�@ߕ�@�\)@އ+@���@݉7@�O�@���@ܼj@�z�@�j@܃@�A�@�\)@�M�@�V@١�@���@ج@؋D@؋D@�Q�@�1'@׍P@���@ָR@�{@��#@��@ԓu@��@Ӆ@�;d@Ұ!@�$�@��T@Ѳ-@љ�@�7L@��/@�j@�Z@� �@υ@�33@��@��@�n�@�J@ͺ^@�G�@���@�1@˾w@˝�@�\)@���@��@�/@ȼj@�9X@���@�+@Ɨ�@�$�@Ł@��@�ƨ@�@���@�X@���@���@��D@�1'@�b@��@���@�;d@��H@��+@�=q@��#@�V@���@�r�@�9X@�ƨ@��@�"�@�M�@�O�@���@�Z@�(�@���@��@��R@�-@��T@�X@���@��u@�j@�I�@��;@���@��!@�V@��^@��/@��D@�j@�I�@�1@�+@���@��@��@���@�p�@��@���@�r�@�I�@���@�S�@�33@���@�n�@�$�@���@�/@���@�Q�@��@��
@���@�dZ@�C�@��!@��@�@��^@��-@���@���@��@��`@�bN@��
@��P@�l�@�S�@�;d@���@�~�@�v�@�ff@�^5@�V@�M�@�E�@�-@�J@��@��^@�7L@��@�A�@��;@���@�K�@���@�ff@���@���@�7L@��@��@��@���@���@�j@�1'@��@�1@��@��w@�C�@��@���@���@�ff@�$�@��#@��-@�hs@��/@���@�r�@��@��P@��@��@���@��@���@�O�@�7L@�&�@���@���@�j@�  @���@��F@���@��@�K�@��@�V@��@���@���@���@��@��F@��@���@�|�@�+@���@��^@�x�@�?}@��@�%@���@��@�z�@�C�@���@�n�@�E�@��^@�x�@�%@��j@���@�I�@�b@��@�ƨ@��@��@�dZ@�S�@�C�@�33@��@���@�~�@�5?@��^@�p�@�/@��@��@�A�@��m@���@�+@�ȴ@��\@�=q@�@�?}@��@���@��9@�j@�w@�@~ȴ@~ff@~{@}�-@}O�@}V@|�/@|9X@{��@{"�@z�H@z��@z=q@zJ@y��@yX@y7L@y%@x�9@xbN@x �@w�@w��@w|�@w\)@v��@u�@uO�@t��@t�@s�@s"�@r��@r-@q�7@p��@pb@o��@o�P@oK�@n��@n��@nv�@nE�@nE�@nE�@n{@m@m�h@m�@mV@l�/@l9X@l�@k�@j�@jM�@i��@i�#@i��@i�7@ihs@i&�@hĜ@hbN@g�;@g�w@g��@f�+@e��@e��@e�h@e`B@d�j@dI�@d�@c��@c��@c��@c��@c�m@cƨ@c��@ct�@cC�@b�H@bJ@a7L@`�9@`r�@`r�@`bN@`A�@` �@`  @_�@^��@^@]p�@\�@\��@\z�@[ƨ@Z��@Zn�@Zn�@Z=q@Y��@Yhs@Y�@X��@X1'@W�P@W�@V�y@V�R@V��@Vv�@VE�@VE�@V$�@U��@U@Up�@T�D@S��@SdZ@R�H@R=q@R�@Q�#@Q�^@Q��@Q��@Qhs@Pr�@O��@O
=@Nv�@NE�@N$�@N{@N@M��@M��@M�@Mp�@Mp�@MV@L�@LI�@K��@Kƨ@K��@J�@J�\@JM�@J=q@I��@I7L@H�@H1'@G�;@G�w@G�P@Fȴ@F@EO�@D�@D�@D�D@DI�@D(�@D1@Cƨ@C��@C33@B-@AG�@A%@@�`@@�u@@b@@  @?�;@?�P@>�R@>v�@=@=��@=��@=`B@=O�@=O�@=O�@=/@=V@=V@<��@<��@<�D@<�@;ƨ@;t�@;S�@:�H@:=q@9��@9�@8�`@8Ĝ@8�9@8r�@8b@8  @7�@7�;@7�@7|�@6��@6$�@6{@5�T@5@5�-@5/@4�j@4(�@3�
@333@2��@2�@1��@1X@0��@0�@0A�@0b@/�;@/��@/K�@.ȴ@.v�@.ff@.E�@.$�@.@-�@-��@-��@-p�@-?}@-/@,��@,�@,I�@,�@+��@+�F@+dZ@+S�@+"�@*��@*^5@*�@)��@)��@)�@)�#@)�7@(��@(Ĝ@(��@(bN@( �@'��@'��@';d@&�R@&v�@&E�@&5?@%�T@%@%�h@%�@%�@%O�@%O�@%O�@%O�@%?}@$�@$z�@#�m@#��@#�@#t�@#C�@"�!@"-@!��@!�#@!��@!��@!&�@!%@ ��@ �u@ �@ A�@�@|�@�y@ȴ@�R@��@��@�+@V@{@�T@��@z�@9X@ƨ@��@dZ@�H@��@�@�#@��@hs@�`@�9@�u@A�@b@�w@|�@l�@;d@;d@+@�y@��@��@�+@��@��@V@5?@$�@�@�-@�@�j@�D@z�@j@Z@I�@�@�@�@��@�
@�@��@n�@�@��@�@��@�#@�@�#@�#@��@��@��@&�@��@ �@��@�w@�@��@�P@\)@�@�@��@�+@�+@E�@@@p�@O�@�@�/@�@�D@j@j@j@9X@��@�
@��@�@t�@dZ@S�@
�H@
n�@
n�@
n�@
M�@	�#@	��@	��@	X@	�@�`@Ĝ@�9@�@Q�@1'@ �@b@�;@�@��@�P@|�@|�@l�@l�@l�@l�@\)@\)@\)@K�G�O�A�ZA�XA�O�A�S�A�O�A�?}A�?}A�C�A�C�A�?}A�E�A�E�A�7LA�(�A�+A�-A�+A�/A�1'A� �A� �A� �A�{A��A��A��A�{A��A�"�A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A�oA�oA�oA�{A�oA�oA�oA�{A�{A�{A�{A�{A��A��A��A�{A�{A��A�{A�{A�{A�{A�{A�{A�{A�oA�oA�oA�bA�bA�bA�bA�oA�bA�bA�bA�oA�oA�{A�oA�{A�{A�{A�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A� �A� �A� �A� �A� �A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A� �A� �A� �A� �A��A� �A� �A� �A� �A��A��A��A� �A��A� �A� �A� �A�"�A�$�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�&�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�(�A�(�A�(�A�&�A�(�A�&�A�&�A�&�A�&�A�"�A�{A�{A�VA�VA�1A���A���A��TA��/A��
Aҩ�A҇+A�t�A�  Aџ�AыDA�t�A��A�A�A�~�A�M�A��A�x�A��
A̗�A�v�A�ffA�E�A�"�A�
=A���A˥�A�z�A�S�A�O�A�/A���A�ȴA�ĜAʲ-A�~�A�hsA�dZA�dZA�`BA�ZA�\)A�XA�O�A�O�A�I�A�I�A�E�A�C�A�=qA�-A��#Aɥ�AɑhA�v�A�p�A�dZA�Q�A�O�A�G�A�A�A�=qA�;dA�=qA�33A�-A�"�A� �A��A�"�A� �A��A��A�bA�
=A�%A�%A�1A�A��A��A��mA���A��
A���A���A���A���A���A���A���A�ȴA�ȴAȶFAȲ-AȮAȩ�Aȩ�Aȧ�Aȝ�Aȝ�Aȕ�AȋDA�z�A�t�A�l�A�hsA�^5A�r�A�n�A�l�A�jA�bNA�XA�Q�A�VA�I�A�+A� �A�$�A��A��A�bA�JA�A���A��A��A��HA�ȴAǥ�A�r�A�`BA�+A�VA�
=A�A���A��A��A��`A���A���A�ƨA���AƸRAƮAƮAƩ�Aƥ�AƟ�AƍPAƑhAƅA�|�A�t�A�r�A�jA�S�A�G�A�E�A�;dA�;dA�5?A��A�VA�A��Aũ�Ať�AŋDA�z�A�bNA�(�A�A�A�A��#A���Aò-Aã�AÏ\A�~�A�p�A�ffA�;dA��A�ƨA�z�A�1A�ƨA�x�A�K�A��A�jA�{A��/A��A���A�|�A�r�A�bNA�^5A�\)A�VA�S�A�O�A�C�A�?}A�9XA�9XA�1'A�&�A�(�A�&�A�{A���A��A��/A�ȴA���A���A���A���A���A���A�ĜA���A�n�A�^5A�I�A�+A���A���A���A�O�A��A�-A��#A���A���A�\)A�Q�A�+A���A�|�A�\)A�O�A�K�A�9XA���A�x�A�&�A��A���A��DA�x�A�`BA�?}A��A���A���A���A��!A��A�dZA�Q�A�5?A�VA��9A�t�A�ffA�VA�E�A�/A��A���A���A��7A�v�A�ffA�M�A�+A��A�oA�oA�
=A�A��mA��#A�ƨA��A���A���A��uA��PA��A�v�A�p�A�n�A�hsA�`BA�XA�Q�A�M�A�I�A�E�A�9XA� �A�JA���A���A���A��A�p�A�jA�ffA�^5A�S�A�O�A�E�A�5?A�-A� �A��A�bA�VA�1A�A�  A���A���A��A��yA��TA��A��wA���A��\A�1'A��#A�x�A�ĜA�E�A��A��wA�ffA�7LA���A��A�5?A�%A��mA�A��^A��A���A��\A��A�x�A�l�A�^5A�VA�O�A�7LA��A�bA�JA�  A��yA��HA��HA���A��9A���A���A���A���A��A�|�A�v�A�jA�\)A�O�A�9XA�
=A��`A���A��jA���A�~�A�M�A�-A���A�ZA�C�A��A���A���A���A���A�x�A�C�A�33A�VA���A��hA�jA�O�A�1'A���A��^A��A�jA�O�A�33A�(�A�{A�  A���A���A���A���A��A��`A��TA��HA��
A��FA���A���A�ffA�?}A�{A��/A��7A�(�A���A�z�A�n�A�K�A��A�ĜA��A�5?A���A�|�A�A�A��A��#A���A�^5A�5?A��A��/A���A�n�A�$�A���A���A���A��7A�n�A�I�A�?}A�9XA�33A�(�A��A�%A���A��A�7LA��PA�VA��A�oA�
=A�%A�1A�  A���A��A��A��yA��/A��
A���A��A���A�hsA���A�v�A�?}A��HA��9A�n�A���A�z�A�C�A� �A�bA��A��HA��/A���A��A���A��uA�~�A�bNA�G�A�/A�VA��A��yA���A���A�t�A�?}A��A���A�Q�A�+A���A�l�A��A�v�A�K�A�G�A�E�A�?}A�9XA�9XA�/A�1'A�/A�(�A�(�A�&�A�$�A�&�A� �A�"�A�"�A�{A�{A��
A��A��#A��/A��#A���A��+A��A�t�A�ffA�\)A�Q�A�G�A�9XA�1'A�1'A�&�A��A�
=A��TA��PA�1'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	{B	|�B	{B	y�B	|B	z�B	|�B	|B	zxB	{�B	z�B	{�B	{�B	{B	{B	{�B	{�B	{�B	|PB	|PB	|PB	|B	|B	|PB	|�B	|�B	|�B	|�B	|�B	|PB	|�B	|�B	|�B	}VB	}"B	}"B	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|B	{�B	z�B	y�B	w�B	�SB	�<B	�TB	�B
A�B
R�B
d�B
�CB
�'B
�B
��BYB#�BI�BTaBd&Bv�B�B��B��B�B�B��B��B�%B�rB
�B�B�B  B��B��B�B�`B�QB��BбB�pB�^B��B��B��Br�BXBRTBD�B:^B6�B2-BVB
�B�B�B 4B
��B
�iB
��B
��B
�B
�4B
qAB
\]B
,B
qB
�B	�AB	�/B	��B	چB	�$B	�B	}"B	R�B	'�B	�B��B�/B�KB�B�B�zB�HB�0B�3B�B��B��B��B�<B�mB�B��B�AB�vB�GB�JB��B	B	B	�B	�B	"�B	'B	:^B	B[B	A B	@�B	A�B	EmB	Q�B	_�B	]/B	`�B	cTB	_;B	[�B	ZQB	YKB	ZQB	wfB	~�B	�qB	��B	��B	�hB	�:B	��B	��B	��B	��B	��B	�B	�7B	�hB	��B	r|B	=B	?HB	M�B	h�B	�PB	�uB	�!B	�IB	�CB	��B	�B	��B	�-B	ƨB	�zB	�zB	ĜB	�-B	бB	�WB	�WB	�B	��B	�B	��B	�B	�KB	�B	��B	�QB	�B	�8B	�NB	��B	�DB	�KB	�B	��B	�2B	��B	�fB	�B	�B	��B	��B	��B	�;B	ޞB	��B	�dB	�;B	یB	��B	�B	��B	�dB	��B	��B	یB	�WB	�)B	�;B	��B	��B	�KB	�B	ߤB	�QB	�
B	ԕB	�[B	�&B	уB	�B	�B	ϫB	ϫB	�<B	�<B	��B	�6B	�B	�B	�HB	�B	�HB	��B	҉B	�&B	��B	��B	�mB	רB	��B	�?B	�B	�B	چB	یB	��B	ߤB	�B	�B	�B	�B	�B	�mB	�B	��B	�B	�
B	�mB	��B	�B	�B	�KB	�B	��B	�B	��B	��B	��B	�/B	��B	�B	��B	� B	�5B	��B	�B	��B	�B	��B	�B	�B	�B	�B	�KB	��B	�B	�yB	�B	�
B	�DB	��B	��B	�/B	�/B	��B	�)B	�]B	�)B	�B	�WB	�B	�B	�B	�QB	�B	�B	�B	�)B	�B	�B	�oB	�oB	�AB	��B	��B	�B	�B	�B	�B	�|B	�|B	�B	�B	��B	�B	��B	�B	�AB	�;B	�iB	��B	�iB	� B	� B	�cB	� B	��B	�iB	�B	�AB	�GB	�|B	��B	�B	�B	�B	�|B	�B	��B	�GB	�%B	��B	��B	�ZB	��B	��B	�ZB	�+B	�+B	��B	��B	��B	��B	�`B	��B	�`B	��B	�fB	��B	�8B	�B	�lB	��B	�	B	�B	��B	��B	�DB	�B	�rB	�	B	�rB	�B	��B	�JB	��B	�VB	��B	��B	�VB	��B	��B	��B	��B	�(B	�]B	��B	��B	�(B	�]B	��B	��B	��B	��B
B
;B
oB
 �B
oB
B
B
uB
{B
B
B
uB
�B
�B
B
{B
{B
�B
MB
�B
�B
�B
�B
SB
�B
�B
+B
+B
�B
	B
�B
fB
	7B

�B

�B

rB

�B
�B
JB
~B
�B
�B
PB
PB
"B
"B
�B
�B
�B
(B
�B
�B
 B
�B
hB
uB
uB
B
{B
FB
{B
B
{B
�B
$B
�B
YB
�B
$B
YB
$B
�B
1B
�B
�B
�B
�B
�B
_B
�B
_B
�B
eB
7B
�B
7B
�B
�B
=B
qB
qB
qB
CB
B
�B
B
~B
�B
�B
�B
!B
VB
�B
 \B
!-B
!-B
!�B
"�B
#B
#B
#B
#�B
#�B
$�B
&B
%�B
&�B
&�B
&�B
'B
'RB
'B
)*B
)�B
*�B
*�B
+6B
+6B
+kB
+�B
+kB
+B
+�B
+kB
+kB
+�B
+�B
+kB
+�B
+kB
+�B
,�B
,B
,�B
-B
-wB
-CB
-�B
.}B
/OB
1'B
1�B
1�B
2�B
2�B
2�B
4B
3�B
49B
49B
4B
4nB
4�B
4�B
5�B
5tB
5�B
6�B
6B
6�B
7�B
8�B
8�B
9XB
9XB
9XB
9�B
9�B
:^B
:�B
;�B
;dB
;�B
=�B
>B
=�B
=�B
>B
?B
?HB
?HB
?HB
?HB
?HB
?HB
?HB
?B
?HB
?}B
?HB
?}B
@�B
@�B
@OB
@B
@B
@B
@B
?�B
?�B
?HB
@OB
@OB
@�B
@�B
@�B
@�B
A�B
B[B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D3B
C�B
D3B
D3B
D3B
DgB
D3B
DgB
D�B
D3B
D�B
E�B
EmB
FB
F?B
GB
F�B
GB
GB
F�B
F�B
F�B
HKB
HKB
I�B
JXB
J#B
JXB
J�B
J#B
J�B
J�B
K^B
K^B
K�B
L�B
MjB
NB
M�B
N<B
NB
OBB
OBB
OvB
OBB
O�B
PHB
P�B
P�B
P�B
P�B
P�B
Q�B
R B
RTB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
RTB
R�B
S�B
T�B
S�B
T,B
TaB
U2B
T�B
T�B
T�B
VB
U�B
V�B
V9B
VmB
V�B
V9B
U�B
VmB
V�B
V�B
VmB
VmB
V�B
W
B
W?B
WsB
WsB
WsB
W�B
X�B
X�B
YB
YB
YB
YB
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
Y�B
[#B
[WB
[WB
[�B
[�B
[WB
[�B
\)B
\�B
\�B
]dB
^B
^B
^�B
^�B
_;B
_�B
`B
`B
`B
`�B
`vB
a|B
aHB
a|B
a�B
a|B
bB
a�B
a�B
bB
bNB
b�B
bNB
b�B
b�B
cTB
cTB
cTB
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
dZB
d�B
d�B
d�B
e�B
e,B
e`B
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
f�B
g8B
gB
gB
gmB
gB
gB
f�B
f�B
g8B
g�B
h>B
h>B
h>B
h>B
hsB
h�B
iDB
iDB
iDB
iyB
i�B
jKB
jKB
jKB
j�B
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
k�B
k�B
m]B
l�B
m)B
m�B
m]B
m�B
n/B
n�B
o5B
o5B
o5B
o�B
pB
o�B
o�B
pB
pB
p�B
poB
p�B
p�B
p�B
p�B
p�B
q�B
qAB
qvB
qvB
qAB
q�B
q�B
q�B
qvB
rGB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
t�B
tTB
tTB
t�B
t�B
t�B
v+B
v`B
v�B
v�B
w2B
w2B
w2B
w2B
wfB
w2B
w2B
v�B
w�B
xB
x8B
yrB
yrB
yrB
y�B
y�B
yrB
zB
zB
z�B
z�B
z�B
z�B
{JB
{JB
{�B
|B
|B
|�B
|�B
|�B
}"B
}"B
}"B
}"B
}VB
}�B
}�B
}�B
~(B
~(B
}�B
~(B
~�B
cB
.B
.B
cB
� B
�B
�B
�iB
��B
��B
�;B
�;B
�oB
��B
��B
��B
��B
�B
�AB
�AB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
��B
��B
�uB
��B
�uB	y>B	zB	|�B	z�B	|�B	|PB	y�B	zB	y�B	zxB	s�B	{�B	|�B	y�B	y�B	{B	y�B	xlB	zB	}"B	}VB	zB	{JB	{B	y�B	|PB	|B	{�B	zxB	zxB	{JB	{B	{B	{JB	{B	{�B	{�B	{�B	|B	{�B	|�B	{�B	{�B	{�B	{B	{�B	|B	|�B	|�B	}"B	|�B	|�B	|�B	|B	|�B	}�B	{�B	|�B	}"B	}"B	|�B	|PB	|B	|�B	|PB	|�B	z�B	{�B	|�B	|B	|B	{�B	|PB	{�B	{�B	z�B	|B	{�B	{�B	{�B	{B	{JB	{B	{JB	{B	{B	{B	{B	{�B	{�B	{�B	{B	{B	{�B	{�B	|�B	|�B	|�B	|�B	}"B	|�B	|�B	|�B	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|B	|�B	|�B	|B	|B	|�B	|PB	}VB	}"B	|�B	|�B	|�B	|�B	|B	|�B	|�B	|PB	|�B	|�B	|�B	}VB	}VB	}VB	|�B	}VB	}"B	}"B	|�B	}"B	}VB	}"B	}"B	}"B	}VB	}"B	}�B	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|�B	|PB	|B	{�B	{B	{�B	{�B	{B	{�B	{B	{B	{�B	|B	|PB	|B	|B	|B	|B	{�B	|B	|B	|B	|PB	|�B	|PB	|�B	|�B	|�B	}"B	}�B	}VB	}�B	~]B	}�B	~]B	}�B	~]B	~(B	~]B	}�B	}�B	}�B	}�B	}VB	}�B	|�B	|�B	|�B	|PB	|�B	}"B	|�B	{�B	|B	|�B	|PB	|�B	}VB	|PB	|�B	|�B	}"B	|�B	|�B	{B	|�B	|�B	|PB	|�B	|B	|PB	|PB	|PB	|�B	|B	{�B	{�B	|B	{�B	|�B	|�B	|B	|�B	|�B	|�B	|PB	|�B	|�B	|�B	}"B	}�B	|�B	}VB	}"B	}VB	|�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}VB	}�B	}�B	}VB	}"B	}VB	|�B	|�B	|�B	}VB	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|PB	|B	|PB	{�B	{JB	z�B	{JB	z�B	z�B	{B	z�B	z�B	z�B	yrB	~]B	zB	y>B	w�B	z�B	xB	v�B	{�B	u�B	v�B	z�B	x8B	z�B	�VB	�%B	�4B	�[B	�!B	�3B	��B	�6B	��B	��B	�/B	̘B	�B	͟B	� B	�,B	��B	�dB	�5B	� B	��B	��B	�JB
�B
)_B
.�B
5�B
K)B
PHB
PB
PB
P}B
R B
P�B
R B
S�B
S�B
T�B
T,B
T�B
T�B
V9B
ZQB
r|B
��B
��B
�	B
�xB
��B
�FB
�B
��B
��B
��B
�kB
�eB
�xB
��B
��B
�hB
��B
�bB
��B
��B
�FB
�RB
��B
��B
�B
�0B
��B
��B
��B
�?B
�<B
�dB
�<B
�B
�B
��B
��B
�}B
��B
� B
�UB
ȴB
�RB
��B
��B
�0B
��B
�BB
�B
� B
�,B
��B
�]B
�B
��B
��B
��B
��B
��B
�)B
�jB
�|B
�B
�B
�ZB
��B
��B
�B
�B
�B
��B
�B
�B
�;B
��B
�B
�B
�B
��B
��B
�xB�B
��B
�]B
��B
�.B �B
��B�B iB iBB;BB{B�B�BuB�B�B;BBBuB;BGB�B�BABGB{B�B	B�B�B
rBxB1B
	B�B�B�B	B"�B�B�B�B1B�BB�B�B#nB(XB1�B/�B;�B4nB:�B:�BA�BO�BH�BF�BJ#BHKBJ�BH�BL0BLdBL�BK�BK�BM6BN<BNBOBBN�BPBR�BQ�BR�BW?B]/BW�BW�BZQBT�BW
BX�BZQBZB\�B_�Bc B_�B`BBbNBiyBl"BqABqABr�BuZB�Bk�Bm�Bp;Br�Bm�Bt�B��B� B{JB{�B}VB��B��B�RB��B�XB��B��B�UB�B��B�zB��B�B��B��B��B�B��BÖB�mB� B�6B��B��B�B��B�B��B�9B�NBуB��B��BٴB��B��BںBیB�)B�5B� B�B�&B�B��B�`B�`B��B��B�sB�8B�8B�sB��B�DB�sB�B�>B�B��B��B��B�MB�2B�TB�B�MB�B�B�B��B��B��B��B��B��B��B�B�TB�%B�ZB�TB�B�B�B�B�MB�ZB��B�%B{BB��B�B
	B	7B�B�BGB	7B�B�B�B�B
=B{B�B�B�B�BuBGB�B�BB�BMBABB�BuB iB�cB �B�B��B��B�"B��B�]B�"B��B��B��B�JB��B�"B��B��B�+B��B��B��B��B��B
	B�B�sB�ZB��B��B�B�B�B�B�sB��B�B��B��B�B�`B�B�BٴB��B�KB��B�EB҉B҉B��B� B�NB�[BѷB��B�}B҉B�NB�BB̘BуBуB�}B҉B�B� B�jB̘B�B�&B��B�9B̘BϫB�BBB��B��B��B�<B�^B�9B�'B��B�9B��B�nB��B��B�VB�B��B��B��B��B��B��B��B��B�hB�bB�xB�4B_�B_pB_;BYBZ�BXBZB[�BW
BT�BUgBR�BRTBS�BT�BT�BS�Bd�BK�B@�BK�B8BH�BP}BGB?HB@�B=<B?B8�B8�B9XB>B6�B9$B:�B7LB9XB7LB5�B2�B1�B3hB2�B4B1'B,=B+�B9$B \B*eB+kB$B7B�BJBDB�BxB	lBxB	lB	�B
�BfB	B	B_B�B�BYB	B�B�B�B�B �B�BB
��B
��B
��B
�(B
��B
��B
�xB
��B
��B
��B
�ZB
�ZB
�B
��B
��B
�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             B	{�B	|�B	{�B	zxB	|6B	{0B	|�B	|PB	z�B	{B	z�B	{�B	{�B	{B	{�B	{�B	|B	|B	|jB	|jB	|jB	|6B	|B	|PB	|�B	|�B	|�B	|�B	|�B	|PB	|�B	|�B	|�B	}VB	}<B	}"B	|�B	}"B	}B	|�B	|�B	}B	|�B	}B	|�B	|6B	{�B	{0B	{0B	HB	��B	ǮB	�B	�8B
B�B
TaB
iyB
�;B
ňB
�WBSB?B6`BN"BY�Bt�B��B��B�XB��B��B��B�oB��B�B
�B{B�B�B�BGB+B��B��B�B�2BںB�QB�B��B��B�4Bv�B\�B_pBL~B>wB=<B>]B%B�B	�B	7B�B
�HB
�VB
�B
ÖB
�zB
��B
~(B
i�B
0;B
#B
FB	��B	�nB	��B	�B	��B	��B	�hB	eFB	@�B	B�OB��B�4B�B��B�}B�B��B��B��B��B�B՛B�B�B�sB�GB�B�B��B�"B	�B	B	?B	$B	�B	"hB	*�B	=qB	D�B	C{B	BuB	BuB	F%B	S�B	a�B	_�B	b�B	f�B	b4B	]~B	\)B	Y�B	\)B	|�B	�B	�;B	�$B	�]B	�B	��B	��B	��B	��B	��B	��B	{�B	��B	��B	��B	�uB	>BB	<B	IlB	c:B	��B	��B	�5B	��B	��B	��B	�mB	��B	��B	ƎB	ȚB	�B	�B	�3B	өB	��B	�qB	�-B	�B	��B	�B	�B	��B	�wB	�UB	�B	�B	�B	�B	��B	�_B	�kB	��B	�wB	�B	��B	��B	�RB	��B	��B	�:B	�B	�4B	ߤB	�B	�jB	�B	�CB	޸B	�FB	�B	ބB	ܒB	��B	�)B	��B	ޞB	�B	�B	�B	��B	��B	�B	�xB	ؓB	յB	ԕB	��B	� B	��B	�&B	��B	�HB	ΊB	οB	�B	��B	�B	��B	�TB	�TB	ѝB	��B	ӏB	�uB	�&B	�YB	��B	خB	רB	��B	��B	��B	�	B	ۦB	ܬB	�BB	�ZB	�RB	��B	��B	�KB	�
B	�RB	��B	�
B	�sB	�B	�B	�RB	��B	��B	��B	��B	�B	� B	�cB	��B	�5B	�}B	��B	� B	�B	��B	�B	��B	�wB	�B	�B	��B	��B	�wB	�qB	�B	�B	�B	��B	�B	�XB	��B	�)B	�}B	�B	�B	�B	�B	�B	�IB	�wB	�B	�qB	�B	�QB	��B	�B	�WB	�WB	�wB	�B	��B	�B	��B	�GB	�B	�B	�B	�B	��B	�TB	��B	��B	��B	�hB	�B	�ZB	�zB	�TB	��B	�B	��B	�B	��B	�B	�B	�iB	�B	�B	�B	��B	�B	��B	�B	�B	��B	�3B	�GB	��B	�hB	�B	��B	�+B	��B	��B	��B	��B	�`B	��B	��B	��B	�FB	�LB	�B	�FB	�B	��B	�fB	��B	�lB	�>B	��B	��B	��B	�>B	�XB	�$B	�B	�0B	�^B	�*B	��B	�$B	��B	�0B	��B	�PB	�BB	��B	��B	��B	�wB	�HB	�B	��B	��B	�BB	�wB	��B	��B	�wB	��B	�B	��B
 �B
 iB
�B
�B
B
�B
�B
B
�B
-B
�B
B
B
�B
GB
-B
{B
�B
�B
B
�B
mB
%B
B
9B
�B
?B
�B
�B
�B
�B
	lB
	7B
	B

=B
xB
B
B
�B
�B
�B
�B
B
6B
�B
"B
�B
�B
�B
�B
�B
�B
B
bB
�B
TB
�B
,B
{B
�B
�B
{B
�B
�B
�B
?B
�B
YB
�B
�B
�B
�B
�B
1B
QB
B
+B
�B
+B
�B
�B
EB
B
eB
�B
�B
�B
�B
�B
�B
qB
�B
�B
B
�B
�B
�B
�B
B
OB
�B
pB
�B
 B
 �B
!B
!�B
!�B
"�B
#�B
#TB
#:B
#�B
$@B
$�B
%zB
&fB
&LB
&�B
'B
'RB
'mB
'�B
'�B
)�B
)�B
+B
*�B
+�B
+kB
+�B
+�B
+�B
+6B
+�B
+�B
+�B
,B
+�B
+�B
+�B
+�B
,�B
-]B
,�B
-]B
-�B
-�B
-�B
.}B
/B
0;B
1�B
1�B
1�B
2�B
3B
3MB
49B
4B
49B
49B
49B
4�B
4�B
5?B
5�B
5�B
6FB
6�B
6�B
7�B
8B
8�B
8�B
9�B
9rB
9�B
9�B
9�B
:�B
;JB
;�B
;�B
<�B
>�B
>B
=�B
>B
>�B
?}B
?}B
?cB
?HB
?HB
?HB
?cB
?cB
?HB
?}B
?�B
?�B
@iB
AUB
A;B
@�B
@B
@4B
@4B
@4B
?�B
@B
@ B
A;B
@�B
AoB
@�B
A B
A�B
BuB
B�B
BB
B'B
B[B
C-B
B�B
B�B
CaB
D3B
D3B
DgB
C�B
DMB
DMB
DgB
DgB
DgB
D�B
D�B
D�B
E�B
FtB
FB
F�B
F�B
G+B
F�B
G+B
G+B
F�B
G+B
G�B
H�B
IB
J	B
J�B
J=B
JrB
J�B
JXB
J�B
KB
KxB
KxB
K�B
M6B
M�B
NVB
M�B
NpB
N�B
O�B
O�B
O�B
O�B
PbB
Q B
Q B
Q B
QB
Q B
Q�B
R:B
R�B
R�B
R�B
R�B
SB
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T,B
T{B
T�B
UMB
T�B
T�B
U�B
VSB
V�B
V�B
V9B
V�B
V�B
V9B
U�B
V�B
V�B
V�B
V�B
V�B
W$B
WsB
W�B
W�B
W�B
W�B
XyB
YKB
YeB
YKB
YKB
Y�B
YeB
ZB
Z7B
Y�B
ZB
ZB
ZB
Z�B
[�B
[qB
[�B
[�B
[�B
[�B
\xB
\�B
]/B
]dB
^B
^�B
^jB
^�B
_;B
_�B
`'B
`BB
`BB
`\B
`�B
`�B
a�B
abB
a�B
a�B
a�B
b4B
a�B
bB
bNB
b�B
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
dB
dZB
d�B
d�B
d�B
dtB
d�B
d�B
ezB
e�B
e`B
e�B
fB
fLB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
gRB
gB
gRB
gB
g8B
gmB
gB
gB
f�B
gB
g�B
h$B
h�B
hXB
hXB
hsB
iB
i_B
iyB
i_B
i_B
i�B
jKB
jB
jB
j�B
j�B
j�B
kB
kkB
lB
k�B
k�B
k�B
k�B
k�B
l"B
lqB
l=B
l�B
m�B
mB
m�B
m�B
m�B
n}B
n}B
oB
o�B
oOB
o�B
p!B
p;B
pB
o�B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
q'B
qB
q�B
q[B
qvB
qvB
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
tB
tB
tB
tB
t�B
t�B
tTB
tnB
t�B
t�B
u�B
vzB
v�B
v�B
wB
w2B
wLB
w2B
wLB
wfB
wLB
w2B
w2B
xB
xlB
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z^B
z^B
z�B
z�B
z�B
{0B
{�B
{�B
|B
|6B
|PB
|�B
|�B
}B
}<B
}"B
}"B
}VB
}�B
}�B
}�B
~B
~BB
~BB
~B
~�B
cB
cB
.B
cB
�B
�4B
�B
�B
��B
�B
��B
�UB
�oB
��B
��B
��B
��B
�B
�AB
�[B
�[B
��B
�uB
��B
�uB
�uB
�uB
��B
��B
��B
��B
��G�O�B	y>B	zB	|�B	z�B	|�B	|PB	y�B	zB	y�B	zxB	s�B	{�B	|�B	y�B	y�B	{B	y�B	xlB	zB	}"B	}VB	zB	{JB	{B	y�B	|PB	|B	{�B	zxB	zxB	{JB	{B	{B	{JB	{B	{�B	{�B	{�B	|B	{�B	|�B	{�B	{�B	{�B	{B	{�B	|B	|�B	|�B	}"B	|�B	|�B	|�B	|B	|�B	}�B	{�B	|�B	}"B	}"B	|�B	|PB	|B	|�B	|PB	|�B	z�B	{�B	|�B	|B	|B	{�B	|PB	{�B	{�B	z�B	|B	{�B	{�B	{�B	{B	{JB	{B	{JB	{B	{B	{B	{B	{�B	{�B	{�B	{B	{B	{�B	{�B	|�B	|�B	|�B	|�B	}"B	|�B	|�B	|�B	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|B	|�B	|�B	|B	|B	|�B	|PB	}VB	}"B	|�B	|�B	|�B	|�B	|B	|�B	|�B	|PB	|�B	|�B	|�B	}VB	}VB	}VB	|�B	}VB	}"B	}"B	|�B	}"B	}VB	}"B	}"B	}"B	}VB	}"B	}�B	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|�B	|PB	|B	{�B	{B	{�B	{�B	{B	{�B	{B	{B	{�B	|B	|PB	|B	|B	|B	|B	{�B	|B	|B	|B	|PB	|�B	|PB	|�B	|�B	|�B	}"B	}�B	}VB	}�B	~]B	}�B	~]B	}�B	~]B	~(B	~]B	}�B	}�B	}�B	}�B	}VB	}�B	|�B	|�B	|�B	|PB	|�B	}"B	|�B	{�B	|B	|�B	|PB	|�B	}VB	|PB	|�B	|�B	}"B	|�B	|�B	{B	|�B	|�B	|PB	|�B	|B	|PB	|PB	|PB	|�B	|B	{�B	{�B	|B	{�B	|�B	|�B	|B	|�B	|�B	|�B	|PB	|�B	|�B	|�B	}"B	}�B	|�B	}VB	}"B	}VB	|�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}VB	}�B	}�B	}VB	}"B	}VB	|�B	|�B	|�B	}VB	|�B	}"B	|�B	|�B	|�B	|�B	|�B	|PB	|B	|PB	{�B	{JB	z�B	{JB	z�B	z�B	{B	z�B	z�B	z�B	yrB	~]B	zB	y>B	w�B	z�B	xB	v�B	{�B	u�B	v�B	z�B	x8B	z�B	�VB	�%B	�4B	�[B	�!B	�3B	��B	�6B	��B	��B	�/B	̘B	�B	͟B	� B	�,B	��B	�dB	�5B	� B	��B	��B	�JB
�B
)_B
.�B
5�B
K)B
PHB
PB
PB
P}B
R B
P�B
R B
S�B
S�B
T�B
T,B
T�B
T�B
V9B
ZQB
r|B
��B
��B
�	B
�xB
��B
�FB
�B
��B
��B
��B
�kB
�eB
�xB
��B
��B
�hB
��B
�bB
��B
��B
�FB
�RB
��B
��B
�B
�0B
��B
��B
��B
�?B
�<B
�dB
�<B
�B
�B
��B
��B
�}B
��B
� B
�UB
ȴB
�RB
��B
��B
�0B
��B
�BB
�B
� B
�,B
��B
�]B
�B
��B
��B
��B
��B
��B
�)B
�jB
�|B
�B
�B
�ZB
��B
��B
�B
�B
�B
��B
�B
�B
�;B
��B
�B
�B
�B
��B
��B
�xB�B
��B
�]B
��B
�.B �B
��B�B iB iBB;BB{B�B�BuB�B�B;BBBuB;BGB�B�BABGB{B�B	B�B�B
rBxB1B
	B�B�B�B	B"�B�B�B�B1B�BB�B�B#nB(XB1�B/�B;�B4nB:�B:�BA�BO�BH�BF�BJ#BHKBJ�BH�BL0BLdBL�BK�BK�BM6BN<BNBOBBN�BPBR�BQ�BR�BW?B]/BW�BW�BZQBT�BW
BX�BZQBZB\�B_�Bc B_�B`BBbNBiyBl"BqABqABr�BuZB�Bk�Bm�Bp;Br�Bm�Bt�B��B� B{JB{�B}VB��B��B�RB��B�XB��B��B�UB�B��B�zB��B�B��B��B��B�B��BÖB�mB� B�6B��B��B�B��B�B��B�9B�NBуB��B��BٴB��B��BںBیB�)B�5B� B�B�&B�B��B�`B�`B��B��B�sB�8B�8B�sB��B�DB�sB�B�>B�B��B��B��B�MB�2B�TB�B�MB�B�B�B��B��B��B��B��B��B��B�B�TB�%B�ZB�TB�B�B�B�B�MB�ZB��B�%B{BB��B�B
	B	7B�B�BGB	7B�B�B�B�B
=B{B�B�B�B�BuBGB�B�BB�BMBABB�BuB iB�cB �B�B��B��B�"B��B�]B�"B��B��B��B�JB��B�"B��B��B�+B��B��B��B��B��B
	B�B�sB�ZB��B��B�B�B�B�B�sB��B�B��B��B�B�`B�B�BٴB��B�KB��B�EB҉B҉B��B� B�NB�[BѷB��B�}B҉B�NB�BB̘BуBуB�}B҉B�B� B�jB̘B�B�&B��B�9B̘BϫB�BBB��B��B��B�<B�^B�9B�'B��B�9B��B�nB��B��B�VB�B��B��B��B��B��B��B��B��B�hB�bB�xB�4B_�B_pB_;BYBZ�BXBZB[�BW
BT�BUgBR�BRTBS�BT�BT�BS�Bd�BK�B@�BK�B8BH�BP}BGB?HB@�B=<B?B8�B8�B9XB>B6�B9$B:�B7LB9XB7LB5�B2�B1�B3hB2�B4B1'B,=B+�B9$B \B*eB+kB$B7B�BJBDB�BxB	lBxB	lB	�B
�BfB	B	B_B�B�BYB	B�B�B�B�B �B�BB
��B
��B
��B
�(B
��B
��B
�xB
��B
��B
��B
�ZB
�ZB
�B
��B
��B
�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<=�.<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<� _<���<#�
<#�
<��<qD�<4vP<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<5�U<#�
<#�
<#�
<#�
<zp�<#�
<#�
<#�
<#�
<8�<S�<Ar�<+�6<#�
<zp�<#�
<#�
<e�<#�
<#�
<#�
<W��<#�
<#�
<#�
<#�
<#�
<#�
<p�<+�<ax</`�<#�
<ax<g��<#�
<#�
<#�
<#�
<#�
<)��<Un�<#�
<��~<��<��<��<���<;��<#�
<'xE<�`�<|a�<(��<|a�<PF�<#�
<#�
<#�
<#�
<#�
<#�
<1p�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-��<#�
<#�
<#�
<Xn<�|A<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2018051219290920180512192909IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018051222004720180512220047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018051222004720180512220047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107544320190521075443IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                