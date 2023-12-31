CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-11-09T23:31:10Z creation; 2023-04-26T19:24:30Z DMQC;      
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
_FillValue        G�O�     �  d�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � :   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � a�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � ix   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201109233110  20230426192430  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               h   hAA  AOAO7315_008643_104                 7315_008643_104                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�F9����@�F9����11  @�F9�E�@�F9�E�@1vF�V@1vF�V�e4���e4��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�@�\@=p�@z�H@�  @��R@�G�A ��A  A!�A-p�A@��A_\)A\)A�Q�A�  A�  A�Q�AУ�A߮A�  B   B(�BQ�B  B   B(Q�B0  B7�
B?�
BG�
BP(�BX  B_�Bg�
Bp(�Bx(�B�
B�  B�{B�{B�{B��B�  B�{B��B��B�  B�{B��
B�  B�  B��
B��B��
B�B�  B�=qB�  B�  B�{B�  B��B��B�  B�  B�  B�  B�{C 
=C  C��C��C  C
{C  C  C
=C
=C  C
=C  C
=C  C
=C   C"  C$
=C&  C(
=C*
=C,  C.  C0  C2
=C4
=C6  C8  C:
=C<
=C=��C@  CB  CC��CF  CH�CJ{CL
=CN  CP  CR
=CT  CU��CX  CY�HC[�HC]��C`{Cb{Cd
=Cf
=Ch
=Ci��Ck��Cn  Cp  Cr
=Ct  Cu��Cw�Cy��C|  C}��C�C�C�  C�C�\C�C���C�  C�  C�  C�  C�
=C�
=C�  C�  C�
=C�
=C�
=C�C���C�  C���C���C���C�  C���C���C�C�  C�  C�  C�  C�C�C�C�  C���C�  C�C���C���C�C�  C�  C�  C���C�  C�C�  C�C�  C���C���C�  C���C�  C���C���C�  C�  C�C�  C�  C�C�  C���C���C�  C���C���C�  C�C�C�  C���C���C���C���C�C�  C�C�C�C�  C�  C�C�C�C�C�  C���C�C�
=C�  C�  C�  C�  C�  C�  C���C�  C�\C�
=C�C�
=C�C���C���C���C���C���C���C�  C�C�C�  C�C�  C�  C�C�C���C���C���C���C���C���C�  D D � D ��D� D  D� DD� D��D}qD  D��D��D}qD�D� D�qD� D	  D	}qD	�qD
� D
�RD}qDD�D�D��D�qDxRD  D}qD��D� DD��D  D}qD�qD��D�D� D  D� D  D}qD  D�D�D� D�D� D��D� D�D� D�qD� D�D� D�qD��D�D� D�qD }qD!  D!� D!��D"��D#  D#}qD#�qD$� D%�D%��D&  D&� D&�qD'� D'�qD(}qD)  D)�D*  D*z�D*�qD+� D,  D,� D-  D-}qD-�qD.� D/�D/�D0�D0��D1  D1� D2D2��D2��D3z�D3�qD4�D5�D5��D6�D6� D6�RD7� D8D8� D9D9�D:D:�D;D;� D;��D<z�D<��D=��D>�D>�D?�D?� D@�D@��DA  DA}qDB�DB}qDC  DC��DC�qDD}qDD�qDE}qDF  DFxRDF�qDGz�DH  DHxRDH��DI��DJ�DJ��DKDK��DL  DL� DM  DM}qDN  DN� DO  DO� DP  DP��DQ�DQ��DR  DR�DS�DS��DT  DTz�DT��DUz�DV  DV��DW  DW� DX  DX� DX��DY}qDZ�DZ� D[  D[� D\�D\� D\�qD]� D^  D^}qD^��D_xRD_��D`� Da  Da��Db  Db� Dc�Dc��Dd�Dd��De�De}qDe�qDf� Dg  Dg� Dg��Dh}qDi�Di� Dj�Dj��Dk  Dkz�Dl  Dl��Dm�Dm� Dm��Dn}qDn�qDo� Do��Dp� Dq�Dq� Dr�Dr�DsDs�Dt  Dt� Du�Du��Dv  Dvz�Dw  Dw� Dx  Dx��Dy  Dy� Dz  Dz��Dz�qD{� D|�D|� D}  D}� D}�qD~� D�D��D�  D�>�D�~�D�� D���D�@ D��HD��HD���D�=qD�}qD���D�HD�B�D��HD�D�HD�>�D�}qD��qD���D�=qD�|)D���D�  D�>�D�~�D�D��D�B�D��HD�� D��D�B�D���D���D���D�>�D�~�D��qD���D�AHD���D���D��D�B�D�~�D��qD��qD�>�D�}qD��qD���D�AHD��HD�D��D�B�D��HD��HD�HD�>�D�|)D��)D���D�AHD��HD��HD�HD�B�D��HD�� D�  D�>�D�� D���D���D�>�D�� D�� D�HD�AHD�~�D���D�HD�>�D�}qD��HD�  D�>�D��HD�� D�  D�@ D�~�D�� D�HD�>�D�~�D���D���D�B�D�� D�� D�HD�@ D��HD��HD�HD�@ D���D�� D���D�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D�~�D��HD�  D�>�D�|)D���D�  D�@ D�� D�� D�HD�@ D�~�D��)D���D�AHD���D�� D�HD�@ D��HD�� D���D�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD�~�D���D��qD�AHD�� D���D���D�>�D�}qD���D�  D�@ D��HD�D�HD�AHD��HD�� D���D�>�D�� D�� D���D�>�D��HD�D�  D�=qD�� D��HD�  D�>�D�� D���D�  D�@ D�� D��HD��D�@ D�� D�D��D�B�D�� D�� D�  D�@ D�� D�� D�HD�@ D�}qD�� D���D�>�D��HD��HD�HD�B�D�� D��)D��qD�=qD�~�D�� D���D�<)D�z�D��qD�  D�=qD�}qD�� D���D�>�D�� D��qD���D�B�D���D�D��D�@ D�~�D�� D�  D�>�D��HD��HD��qD�@ D�� D��qD��qD�@ D�~�D�� D��qD�=qD�� D�D���D�>�D��HD��HD��qD�@ D�~�D½qD�  D�@ D�~�DýqD��)D�>�DĂ�D��HD�  D�AHDŀ Dž�D��D�>�D�}qD�� D�  D�B�D�~�D�� D���D�@ DȁHDȾ�D���D�>�Dɀ Dɾ�D�  D�@ D�~�D�� D���D�@ DˁHD�� D�HD�<)D̀ D�D�HD�B�D̀ D��HD��qD�<)D�~�D��HD�HD�=qD�}qD��HD�HD�@ D�|)DнqD�HD�AHDсHD�D��D�B�DҁHD��HD��D�C�DӁHD��HD�HD�@ DԀ D��HD���D�@ DՁHD�� D�  D�=qDր D��HD�HD�C�DׁHD�� D�HD�=qD�}qD�� D���D�@ Dـ D�� D�  D�@ Dڀ D��HD�  D�>�Dۀ D��HD�HD�@ D܁HD�D�HD�@ D݁HD���D�HD�@ D�~�D޾�D��qD�>�D߀ D߾�D�  D�AHD�� DྸD�  D�B�D��D���D�HD�AHD₏D��HD��D�C�DわD�� D�HD�@ D�}qD�qD�  D�AHD� D�� D�HD�@ D悏D��HD��qD�>�D炏D�� D�  D�@ D�}qD�� D���D�B�D�HD�D�  D�@ D�~�D꾸D�  D�@ D� D��HD�HD�AHD� D쾸D�HD�>�D�~�D�� D�HD�@ D� DD�HD�>�D�HD��HD�HD�B�D�� D�� D�HD�<)D� D�� D��D�@ D�HD�� D�  D�@ D�~�D�qD��qD�=qD�}qD��HD�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�D�  D�>�D��HD��HD�  D�@ D�� D�� D���D�AHD�� D���D�HD�AHD�� ?�?#�
?k�?��
?�
=@�@(�@5@Q�@k�@��\@�\)@��H@��@��@��R@�=q@�
=@�\@�\)@�(�Az�A
�HA��A
=A�RA&ffA,��A2�\A9��A?\)AEAJ�HAQG�AXQ�A^�RAe�Aj�HAq�AxQ�A~�RA�=qA�A���A���A�  A��HA�A���A��
A��RA��A��A�Q�A��A��RA��A��A�  A��HA�{A�G�A�z�A�
=Aə�A�(�A�{A�Q�A��AӅA�{A�  A��A��
A�ffA���A��HA���A�RA��A��HA�(�A�A�A��A�(�A�A���A��\A���A�ffB (�B ��BB�RB�B��B��B�\B\)B��B	B
�HB�
B��B��B�\B�BQ�BG�B�B�HB�
B��B��B
=B  B��BB�\B33B(�B�B�B
=B�
B ��B"{B"�HB#�
B$��B%B&�RB'�B(z�B)p�B*ffB+33B+�
B,��B-B.�RB/�B0z�B1p�B2ffB3�B4��B5p�B6�RB7�B8��B9��B:�\B;\)B<(�B=�B=B>�HB?�BAG�BB=qBC
=BD(�BE�BE�BF�HBG�BHQ�BIG�BJffBK\)BL��BM��BN�\BO�BP  BP��BQ�BR�HBS�
BT��BUBW
=BX(�BY�BZ{B[
=B\  B\��B]�B^�HB_�B`z�Ba��Bb�\Bc\)Bdz�Bep�Bf�\Bg\)Bhz�Bip�BjffBk�Blz�Bm��Bn�HBo�Bp��Bq�Br�HBs�
Bt��BuBv�\Bw�Bxz�Byp�Bz=qB{\)B|Q�B}p�B~ffB\)B�(�B���B�G�B�B�Q�B���B�p�B��
B�Q�B���B�p�B��B�ffB���B�\)B��B�=qB��RB�G�B��B�=qB���B�G�B�B�Q�B��HB�p�B��B�z�B�
=B��B�{B��RB�G�B��B�z�B�
=B��B�(�B���B�
=B���B�(�B��RB�G�B��B�z�B��B�B�=qB��HB�\)B�  B�z�B��B��B�=qB��HB�\)B�  B���B��B�B�=qB���B��B�=qB���B��B�{B���B�33B��B���B��B��
B�ffB�
=B��B�Q�B���B���B�=qB���B�\)B�{B��RB�p�B�  B���B�G�B�{B��\B�G�B��
B�ffB��HB���B�(�B���B��B�(�B���B�p�B�  B��RB�\)B�{B���B�\)B��B���B�\)B�  B���BÅB�{BĸRB�p�B�  BƏ\B�
=BǮB�Q�B���Bə�B�(�B���B�\)B��B�z�B���BͅB�  B�z�B���B�p�BϮB�{B�ffBЏ\B���B���B��B�\)BѮB��B�(�B�Q�Bң�B�33B�\)BӮB��
B�  B�(�B�ffBԸRB���B�33B�p�BծB��B�=qB֣�B��HB��B�\)Bי�B��
B�{B�ffB؏\B���B�
=B��B�G�Bٙ�B�B��B�Q�BڸRB��HB�
=B�p�BۅB��
B�  B�(�B�Q�Bܣ�B���B���B��B�\)Bݙ�B�B��B�=qBޏ\B��HB��B�\)Bߙ�B��
B�(�B�Q�B��\B��\B���B�
=B�G�B�p�B�B�(�B�ffB��B���B�33B�p�B�B��
B�  B�(�B�ffB��B��HB��B�p�B�B�  B�ffB��B��HB��B�\)B癚B�  B�(�B�=qB�ffB�RB�
=B�\)B�B��
B�{B�\B��HB��B�p�B�B��
B�{B�ffB��B���B�\)B�B�{B�Q�B��B�RB���B�G�B�B�  B�ffB��B���B�33B�G�B�B��B�(�B�z�B��HB�G�B�B��B�=qB�ffB�\B���B�33B��B�{B�Q�B�z�B��RB��B�\)B��
B�(�B�z�B���B��HB�G�B��B��
B�(�B��RB�
=B�p�B�B�B�(�B�z�B���B�G�B��B�  B�Q�B��\B���B�33B�p�B��
C {C \)C �\C �C C ��C�CffC��C��C�C{C=qCffC�\C�HC{C=qC\)C�C�C�HC(�C\)C�\C�C�
C
=C33Cz�C�RC�HC
=C(�CffC�\C�HC{C=qCz�C�\CC��C(�Cp�C�C�HC��C	33C	\)C	�\C	�HC
{C
G�C
ffC
��C
C
��C=qCz�C�C�HC
=C(�CffC�C�
C{CG�C�C��C��C��C33C�C�RC�
C  C=qCffC�RC��C
=C=qCp�CC��C33CG�C�C�RC
=CG�CffC�\CC{CG�C�C��C�
C
=CffC��C��C��C(�C�CC�C{CG�C�\C��C{CG�Cp�C��C�
C33CffC�\C�RC�C=qCz�C�CC��C33C�C�C��C  C33Cp�C�RC��C  C33C�CC�
C  CG�C�C�C��C��CG�C�C��C��C��CG�Cz�C�CC 
=C =qC \)C �\C ��C!
=C!=qC!\)C!�\C!�HC"�C"33C"ffC"�RC"�C#�C#G�C#p�C#C$  C${C$G�C$�\C$C%  C%�C%G�C%��C%��C%��C&�C&\)C&��C&C&�C'33C'z�C'��C'C({C(Q�C(p�C(��C(��C)(�C)Q�C)��C)�HC*
=C*=qC*�\C*��C*�C+(�C+z�C+��C+�
C,(�C,p�C,�C,C-�C-\)C-z�C-��C.
=C.33C.ffC.C.��C/(�C/�C/C/�
C0�C0z�C0��C0�
C133C1ffC1��C1�C233C2\)C2�\C2��C3�C3Q�C3�RC3��C4�C4\)C4�RC4�
C5{C5z�C5�C5�
C633C6z�C6��C7  C7G�C7ffC7�RC8{C833C8z�C8�
C9
=C9G�C9�C9�HC:�C:�C:C:��C;\)C;�C;�
C<33C<z�C<�C={C=Q�C=�C=��C>=qC>p�C>�
C?�C?Q�C?�RC@  C@33C@��C@��CA�CA�CA�
CB  CB\)CB�RCB�CCQ�CC��CC�
CD(�CD�\CD�RCE{CEz�CE��CE��CF\)CF�\CF��CG=qCGp�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       ?k�@�\@=p�@z�H@�  @��R@�G�A ��A  A!�A-p�A@��A_\)A\)A�Q�A�  A�  A�Q�AУ�A߮A�  B   B(�BQ�B  B   B(Q�B0  B7�
B?�
BG�
BP(�BX  B_�Bg�
Bp(�Bx(�B�
B�  B�{B�{B�{B��B�  B�{B��B��B�  B�{B��
B�  B�  B��
B��B��
B�B�  B�=qB�  B�  B�{B�  B��B��B�  B�  B�  B�  B�{C 
=C  C��C��C  C
{C  C  C
=C
=C  C
=C  C
=C  C
=C   C"  C$
=C&  C(
=C*
=C,  C.  C0  C2
=C4
=C6  C8  C:
=C<
=C=��C@  CB  CC��CF  CH�CJ{CL
=CN  CP  CR
=CT  CU��CX  CY�HC[�HC]��C`{Cb{Cd
=Cf
=Ch
=Ci��Ck��Cn  Cp  Cr
=Ct  Cu��Cw�Cy��C|  C}��C�C�C�  C�C�\C�C���C�  C�  C�  C�  C�
=C�
=C�  C�  C�
=C�
=C�
=C�C���C�  C���C���C���C�  C���C���C�C�  C�  C�  C�  C�C�C�C�  C���C�  C�C���C���C�C�  C�  C�  C���C�  C�C�  C�C�  C���C���C�  C���C�  C���C���C�  C�  C�C�  C�  C�C�  C���C���C�  C���C���C�  C�C�C�  C���C���C���C���C�C�  C�C�C�C�  C�  C�C�C�C�C�  C���C�C�
=C�  C�  C�  C�  C�  C�  C���C�  C�\C�
=C�C�
=C�C���C���C���C���C���C���C�  C�C�C�  C�C�  C�  C�C�C���C���C���C���C���C���C�  D D � D ��D� D  D� DD� D��D}qD  D��D��D}qD�D� D�qD� D	  D	}qD	�qD
� D
�RD}qDD�D�D��D�qDxRD  D}qD��D� DD��D  D}qD�qD��D�D� D  D� D  D}qD  D�D�D� D�D� D��D� D�D� D�qD� D�D� D�qD��D�D� D�qD }qD!  D!� D!��D"��D#  D#}qD#�qD$� D%�D%��D&  D&� D&�qD'� D'�qD(}qD)  D)�D*  D*z�D*�qD+� D,  D,� D-  D-}qD-�qD.� D/�D/�D0�D0��D1  D1� D2D2��D2��D3z�D3�qD4�D5�D5��D6�D6� D6�RD7� D8D8� D9D9�D:D:�D;D;� D;��D<z�D<��D=��D>�D>�D?�D?� D@�D@��DA  DA}qDB�DB}qDC  DC��DC�qDD}qDD�qDE}qDF  DFxRDF�qDGz�DH  DHxRDH��DI��DJ�DJ��DKDK��DL  DL� DM  DM}qDN  DN� DO  DO� DP  DP��DQ�DQ��DR  DR�DS�DS��DT  DTz�DT��DUz�DV  DV��DW  DW� DX  DX� DX��DY}qDZ�DZ� D[  D[� D\�D\� D\�qD]� D^  D^}qD^��D_xRD_��D`� Da  Da��Db  Db� Dc�Dc��Dd�Dd��De�De}qDe�qDf� Dg  Dg� Dg��Dh}qDi�Di� Dj�Dj��Dk  Dkz�Dl  Dl��Dm�Dm� Dm��Dn}qDn�qDo� Do��Dp� Dq�Dq� Dr�Dr�DsDs�Dt  Dt� Du�Du��Dv  Dvz�Dw  Dw� Dx  Dx��Dy  Dy� Dz  Dz��Dz�qD{� D|�D|� D}  D}� D}�qD~� D�D��D�  D�>�D�~�D�� D���D�@ D��HD��HD���D�=qD�}qD���D�HD�B�D��HD�D�HD�>�D�}qD��qD���D�=qD�|)D���D�  D�>�D�~�D�D��D�B�D��HD�� D��D�B�D���D���D���D�>�D�~�D��qD���D�AHD���D���D��D�B�D�~�D��qD��qD�>�D�}qD��qD���D�AHD��HD�D��D�B�D��HD��HD�HD�>�D�|)D��)D���D�AHD��HD��HD�HD�B�D��HD�� D�  D�>�D�� D���D���D�>�D�� D�� D�HD�AHD�~�D���D�HD�>�D�}qD��HD�  D�>�D��HD�� D�  D�@ D�~�D�� D�HD�>�D�~�D���D���D�B�D�� D�� D�HD�@ D��HD��HD�HD�@ D���D�� D���D�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D�~�D��HD�  D�>�D�|)D���D�  D�@ D�� D�� D�HD�@ D�~�D��)D���D�AHD���D�� D�HD�@ D��HD�� D���D�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD�~�D���D��qD�AHD�� D���D���D�>�D�}qD���D�  D�@ D��HD�D�HD�AHD��HD�� D���D�>�D�� D�� D���D�>�D��HD�D�  D�=qD�� D��HD�  D�>�D�� D���D�  D�@ D�� D��HD��D�@ D�� D�D��D�B�D�� D�� D�  D�@ D�� D�� D�HD�@ D�}qD�� D���D�>�D��HD��HD�HD�B�D�� D��)D��qD�=qD�~�D�� D���D�<)D�z�D��qD�  D�=qD�}qD�� D���D�>�D�� D��qD���D�B�D���D�D��D�@ D�~�D�� D�  D�>�D��HD��HD��qD�@ D�� D��qD��qD�@ D�~�D�� D��qD�=qD�� D�D���D�>�D��HD��HD��qD�@ D�~�D½qD�  D�@ D�~�DýqD��)D�>�DĂ�D��HD�  D�AHDŀ Dž�D��D�>�D�}qD�� D�  D�B�D�~�D�� D���D�@ DȁHDȾ�D���D�>�Dɀ Dɾ�D�  D�@ D�~�D�� D���D�@ DˁHD�� D�HD�<)D̀ D�D�HD�B�D̀ D��HD��qD�<)D�~�D��HD�HD�=qD�}qD��HD�HD�@ D�|)DнqD�HD�AHDсHD�D��D�B�DҁHD��HD��D�C�DӁHD��HD�HD�@ DԀ D��HD���D�@ DՁHD�� D�  D�=qDր D��HD�HD�C�DׁHD�� D�HD�=qD�}qD�� D���D�@ Dـ D�� D�  D�@ Dڀ D��HD�  D�>�Dۀ D��HD�HD�@ D܁HD�D�HD�@ D݁HD���D�HD�@ D�~�D޾�D��qD�>�D߀ D߾�D�  D�AHD�� DྸD�  D�B�D��D���D�HD�AHD₏D��HD��D�C�DわD�� D�HD�@ D�}qD�qD�  D�AHD� D�� D�HD�@ D悏D��HD��qD�>�D炏D�� D�  D�@ D�}qD�� D���D�B�D�HD�D�  D�@ D�~�D꾸D�  D�@ D� D��HD�HD�AHD� D쾸D�HD�>�D�~�D�� D�HD�@ D� DD�HD�>�D�HD��HD�HD�B�D�� D�� D�HD�<)D� D�� D��D�@ D�HD�� D�  D�@ D�~�D�qD��qD�=qD�}qD��HD�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�D�  D�>�D��HD��HD�  D�@ D�� D�� D���D�AHD�� D���D�HD�AHG�O�?�?#�
?k�?��
?�
=@�@(�@5@Q�@k�@��\@�\)@��H@��@��@��R@�=q@�
=@�\@�\)@�(�Az�A
�HA��A
=A�RA&ffA,��A2�\A9��A?\)AEAJ�HAQG�AXQ�A^�RAe�Aj�HAq�AxQ�A~�RA�=qA�A���A���A�  A��HA�A���A��
A��RA��A��A�Q�A��A��RA��A��A�  A��HA�{A�G�A�z�A�
=Aə�A�(�A�{A�Q�A��AӅA�{A�  A��A��
A�ffA���A��HA���A�RA��A��HA�(�A�A�A��A�(�A�A���A��\A���A�ffB (�B ��BB�RB�B��B��B�\B\)B��B	B
�HB�
B��B��B�\B�BQ�BG�B�B�HB�
B��B��B
=B  B��BB�\B33B(�B�B�B
=B�
B ��B"{B"�HB#�
B$��B%B&�RB'�B(z�B)p�B*ffB+33B+�
B,��B-B.�RB/�B0z�B1p�B2ffB3�B4��B5p�B6�RB7�B8��B9��B:�\B;\)B<(�B=�B=B>�HB?�BAG�BB=qBC
=BD(�BE�BE�BF�HBG�BHQ�BIG�BJffBK\)BL��BM��BN�\BO�BP  BP��BQ�BR�HBS�
BT��BUBW
=BX(�BY�BZ{B[
=B\  B\��B]�B^�HB_�B`z�Ba��Bb�\Bc\)Bdz�Bep�Bf�\Bg\)Bhz�Bip�BjffBk�Blz�Bm��Bn�HBo�Bp��Bq�Br�HBs�
Bt��BuBv�\Bw�Bxz�Byp�Bz=qB{\)B|Q�B}p�B~ffB\)B�(�B���B�G�B�B�Q�B���B�p�B��
B�Q�B���B�p�B��B�ffB���B�\)B��B�=qB��RB�G�B��B�=qB���B�G�B�B�Q�B��HB�p�B��B�z�B�
=B��B�{B��RB�G�B��B�z�B�
=B��B�(�B���B�
=B���B�(�B��RB�G�B��B�z�B��B�B�=qB��HB�\)B�  B�z�B��B��B�=qB��HB�\)B�  B���B��B�B�=qB���B��B�=qB���B��B�{B���B�33B��B���B��B��
B�ffB�
=B��B�Q�B���B���B�=qB���B�\)B�{B��RB�p�B�  B���B�G�B�{B��\B�G�B��
B�ffB��HB���B�(�B���B��B�(�B���B�p�B�  B��RB�\)B�{B���B�\)B��B���B�\)B�  B���BÅB�{BĸRB�p�B�  BƏ\B�
=BǮB�Q�B���Bə�B�(�B���B�\)B��B�z�B���BͅB�  B�z�B���B�p�BϮB�{B�ffBЏ\B���B���B��B�\)BѮB��B�(�B�Q�Bң�B�33B�\)BӮB��
B�  B�(�B�ffBԸRB���B�33B�p�BծB��B�=qB֣�B��HB��B�\)Bי�B��
B�{B�ffB؏\B���B�
=B��B�G�Bٙ�B�B��B�Q�BڸRB��HB�
=B�p�BۅB��
B�  B�(�B�Q�Bܣ�B���B���B��B�\)Bݙ�B�B��B�=qBޏ\B��HB��B�\)Bߙ�B��
B�(�B�Q�B��\B��\B���B�
=B�G�B�p�B�B�(�B�ffB��B���B�33B�p�B�B��
B�  B�(�B�ffB��B��HB��B�p�B�B�  B�ffB��B��HB��B�\)B癚B�  B�(�B�=qB�ffB�RB�
=B�\)B�B��
B�{B�\B��HB��B�p�B�B��
B�{B�ffB��B���B�\)B�B�{B�Q�B��B�RB���B�G�B�B�  B�ffB��B���B�33B�G�B�B��B�(�B�z�B��HB�G�B�B��B�=qB�ffB�\B���B�33B��B�{B�Q�B�z�B��RB��B�\)B��
B�(�B�z�B���B��HB�G�B��B��
B�(�B��RB�
=B�p�B�B�B�(�B�z�B���B�G�B��B�  B�Q�B��\B���B�33B�p�B��
C {C \)C �\C �C C ��C�CffC��C��C�C{C=qCffC�\C�HC{C=qC\)C�C�C�HC(�C\)C�\C�C�
C
=C33Cz�C�RC�HC
=C(�CffC�\C�HC{C=qCz�C�\CC��C(�Cp�C�C�HC��C	33C	\)C	�\C	�HC
{C
G�C
ffC
��C
C
��C=qCz�C�C�HC
=C(�CffC�C�
C{CG�C�C��C��C��C33C�C�RC�
C  C=qCffC�RC��C
=C=qCp�CC��C33CG�C�C�RC
=CG�CffC�\CC{CG�C�C��C�
C
=CffC��C��C��C(�C�CC�C{CG�C�\C��C{CG�Cp�C��C�
C33CffC�\C�RC�C=qCz�C�CC��C33C�C�C��C  C33Cp�C�RC��C  C33C�CC�
C  CG�C�C�C��C��CG�C�C��C��C��CG�Cz�C�CC 
=C =qC \)C �\C ��C!
=C!=qC!\)C!�\C!�HC"�C"33C"ffC"�RC"�C#�C#G�C#p�C#C$  C${C$G�C$�\C$C%  C%�C%G�C%��C%��C%��C&�C&\)C&��C&C&�C'33C'z�C'��C'C({C(Q�C(p�C(��C(��C)(�C)Q�C)��C)�HC*
=C*=qC*�\C*��C*�C+(�C+z�C+��C+�
C,(�C,p�C,�C,C-�C-\)C-z�C-��C.
=C.33C.ffC.C.��C/(�C/�C/C/�
C0�C0z�C0��C0�
C133C1ffC1��C1�C233C2\)C2�\C2��C3�C3Q�C3�RC3��C4�C4\)C4�RC4�
C5{C5z�C5�C5�
C633C6z�C6��C7  C7G�C7ffC7�RC8{C833C8z�C8�
C9
=C9G�C9�C9�HC:�C:�C:C:��C;\)C;�C;�
C<33C<z�C<�C={C=Q�C=�C=��C>=qC>p�C>�
C?�C?Q�C?�RC@  C@33C@��C@��CA�CA�CA�
CB  CB\)CB�RCB�CCQ�CC��CC�
CD(�CD�\CD�RCE{CEz�CE��CE��CF\)CF�\CF��CG=qCGp�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�$@�@ԉG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�-A�\)A�=qA�/A��A�A���A���A��A��A��A��yA��mA��mA��mA��`A��`A��TA��`A��TA��HA��#A�ȴA߸RAߟ�A� �A�7LA�%A��A���Aݣ�A�dZA�oA���Aܡ�Aܗ�A�n�A���Aڴ9A�ƨAכ�A֗�Aէ�A��HA���A�S�A���A̩�A��A�p�A��Aȥ�A�x�A��A��A�ȴA���A�;dA�;dA�bA�
=A�?}A�;dA��FA���A�XA���A��+A��A�/A��A��A�1'A�v�A�7LA�\)A��RA�r�A�7LA��-A�\)A�A��DA��A�z�A�~�A���A�hsA�l�A�{A�ȴA�K�A��A���A�l�A���A���A�C�A�bA�ĜA���A�VA�/A�n�A�ȴA�%A�ZA���A���A��!A�?}A���A�5?A��A�+A�&�AO�A}l�A|^5Az��Av~�Ar��AnZAj��Ag�Ae�7Aa��A_��A\{AXz�AW%AU�hAT  AQ��AO��AJ�DAE�AD^5AC`BAB�ABz�A@ȴA>v�A=l�A;K�A9�hA8=qA7��A6�A5G�A2-A0z�A/K�A-&�A,��A,1'A*bNA)�PA(bNA'"�A&E�A%�#A$bNA#�A#�FA#S�A"ĜA"JA!�wA Q�A�AAZAƨA�A�!AE�A��A�A��A�9AM�A;dA$�A��A&�A�!At�A;dAjA��A��AE�AJA�7A��A1A�hAO�A+A�\A
��A
JA	p�A�RA9XA�AZA��Ax�A+AĜA��Av�AZA1Ap�A��A  �@��H@�ff@�V@���@�=q@��h@��D@��F@�"�@���@�p�@�Z@�ƨ@�S�@��H@�@��@�@�l�@��@���@�5?@���@��@�bN@�K�@�@�%@蛦@�9X@��m@�C�@柾@��@�@�9@�bN@�1@���@߾w@߮@�t�@�"�@��y@ݡ�@�&�@���@�ȴ@�^5@��@�@�+@֏\@֏\@ՙ�@ԃ@�Z@�/@�?}@�?}@�G�@Ցh@ՙ�@Չ7@�p�@���@�j@ӶF@�33@��@��@�$�@�`B@�?}@ЋD@�ƨ@�
=@�~�@�5?@�J@���@��@�x�@̼j@̃@�r�@�I�@�9X@�1'@��@�dZ@ə�@��@ȋD@�9X@�1@ǥ�@Ɵ�@�$�@�@���@�p�@�/@���@�  @Å@�+@�@��y@���@�V@��T@�`B@��D@��;@�\)@���@�-@�`B@��/@�z�@�bN@�l�@�@���@�ff@�@��7@��9@��@�Q�@� �@�l�@��y@��R@���@�^5@��@�1'@� �@�b@�ƨ@�\)@�ff@�=q@��@��h@��@���@�Ĝ@�Ĝ@��j@���@���@���@��u@�bN@���@��;@�dZ@�+@�@���@���@�ff@��@��@���@��-@�X@��@�(�@��@�t�@�t�@�\)@�33@�
=@���@��H@���@�ff@�E�@�5?@�-@�{@��@��#@���@�hs@�7L@��`@�z�@���@��!@�ff@�^5@�=q@���@�/@���@�%@���@��@��j@�t�@���@��#@���@���@��@�hs@��@���@��9@�j@�1@�dZ@��@���@���@��@��h@�O�@��@�1@�t�@�33@��@�n�@���@�Ĝ@�Z@�9X@�1'@�1'@�1'@�(�@� �@��@�  @���@�S�@�33@�"�@��@��T@��^@���@��7@�`B@��@���@�C�@�+@���@��!@��\@�M�@���@���@���@�hs@�V@���@�Q�@��
@���@��P@��P@��P@��@�|�@�S�@�K�@�33@�@���@�-@��@��@�%@���@���@���@��u@��D@��@��@�Z@�ƨ@�l�@�+@��@��+@�@���@���@��7@��7@��@�p�@�`B@�O�@�7L@��@�j@���@��@�dZ@�+@�
=@��@�=q@��T@���@���@���@���@��@�z�@�j@�bN@�  @���@�ƨ@��F@��@��@���@���@���@��P@��@�l�@�S�@�33@�
=@���@��y@���@��R@��+@�^5@�{@���@���@��@��@��`@���@�bN@�;@;d@~�y@~v�@}�T@}`B@}?}@|��@|��@|�@|j@|I�@|9X@|�@{�F@{"�@z�H@z��@z�!@z�@yX@w�w@w�@w
=@v�y@vv�@v@u`B@t��@t�D@t1@r��@q�7@qG�@p��@p�9@pr�@pQ�@p �@oK�@n��@n��@nff@n$�@m��@m�h@m�@l�@l��@l��@l�D@lz�@lj@l9X@l�@k��@k�m@k�m@kƨ@k��@k33@j^5@i�@i�#@i�^@ix�@h��@hQ�@g��@g;d@fV@e�T@d�j@d9X@c�
@c�@c"�@a�@aX@a&�@a&�@a�@`��@`��@`Ĝ@`��@`��@`�u@`r�@`bN@`b@_�@_l�@^��@^@]V@[�
@Z��@Y�#@Yx�@YG�@X�`@XA�@W|�@Vȴ@V��@Vv�@VV@V5?@V{@U�@U�T@U�h@T�/@T��@T��@T�D@Tz�@Tj@T9X@T�@Sƨ@SC�@R�H@R~�@R=q@Q��@Q��@QG�@P�`@O��@OK�@OK�@O;d@N�y@N�y@N�@N��@Nff@N5?@M�T@MO�@MO�@M�@L�D@Lj@LI�@L�@K�
@K�@KS�@J��@J^5@J-@J�@JJ@I��@I��@I��@I�^@I%@H�@H  @F�R@F{@E�T@E�T@E��@E�T@E��@E�h@Ep�@E?}@D��@D�@C��@Cƨ@C��@C�@Ct�@Co@Bn�@B^5@B^5@B^5@BM�@B=q@B-@B�@B�@B�@BJ@A�#@A�7@A�7@A��@A��@A��@A�7@A%@@ �@?��@?�@?��@?��@?�P@?|�@?+@>�@>v�@=@=V@<j@<�@;�m@;ƨ@;ƨ@;dZ@;o@;@;@:�H@:��@:M�@:=q@9�#@9hs@9G�@9%@8Ĝ@7��@7;d@6�R@6E�@5��@5`B@4��@4��@4�D@4z�@4z�@4j@4Z@41@41@4�@3��@3�m@3�m@3ƨ@3��@3dZ@2�!@1�@17L@1%@0�`@0�u@0bN@/�@/�P@/�P@/K�@.�y@.ȴ@.��@.ff@.V@.V@.E�@.E�@.E�@.$�@.{@.@-�@-?}@,�@,��@,�D@,9X@,(�@+��@+dZ@+"�@+@*�@*�@*�H@*�\@*M�@*J@)��@)��@)7L@)%@(�`@(Ĝ@(�9@(��@(bN@(b@'�w@'K�@&ȴ@&$�@%�T@%�h@%p�@%`B@%O�@%/@$�@$�j@$��@$j@$�@#t�@"�@"^5@!�#@!x�@!x�@!x�@!X@!G�@ �`@|�@��@ȴ@v�@$�@{@@�@/@�j@j@(�@�m@��@��@�^@��@��@��@x�@7L@r�@A�@�@�w@��@l�@\)@K�@K�@K�@K�@;d@�y@�@��@�+@v�@��@��@�R@�R@ȴ@�R@��@�+@ff@ff@5?@$�@$�@$�@{@@p�@O�@�@�@�/@�/@�j@�j@�@�@�D@I�@9X@�
@�F@�@dZ@dZ@33@"�@�@��@�!@~�@=q@��@X@&�@%@�9@A�@1'@  @��@�@K�@��@ȴ@�+@E�@5?@5?@$�@��@��@O�@�@�j@�@�@��@�D@Z@I�@(�@�
@��@t�@C�A��A��A�1A���A��
A��A��A�^5A�K�A�?}A�5?A�5?A�5?A�(�A�$�A��A�JA�1A�A�A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��yA��yA��yA��mA��`A��`A��mA��mA��mA��yA��yA��yA��yA��yA��A��yA��yA��yA��mA��`A��`A��TA��TA��HA��HA��HA��TA��TA��`A��mA��mA��mA��mA��`A��`A��TA��HA��HA��HA��TA��TA��TA��`A��mA��mA��mA��`A��TA��HA��HA��;A��HA��HA��TA��TA��TA��HA��HA��HA��;A��/A��/A��
A��
A���A���A���A���A�ȴA���A�ƨA�ĜA�ĜA�A���A߸RAߴ9A߶FAߴ9A߲-A߶FA߶FA߶FAߴ9A߰!A߰!Aߩ�Aߥ�AߓuA߃A�z�A�n�A�hsA�ZA�K�A�5?A��A���A޼jAދDA�p�A�XA�=qA�+A�(�A� �A��A��A��A�VA�
=A�A���A���A���A���A���A���A��A��A��A��yA��`A��HA��HA��/A���A���A���A���A���A�AݶFAݰ!AݮAݬAݬAݩ�Aݧ�Aݥ�AݑhA݉7A݁A�x�A�r�A�hsA�\)A�I�A�;dA�9XA�33A�&�A��A�bA�VA�A���A��A��A��;A��A���A�ƨAܼjAܸRAܲ-AܬAܥ�Aܡ�Aܝ�Aܛ�Aܙ�Aܛ�Aܛ�Aܝ�Aܝ�Aܛ�Aܙ�Aܕ�Aܕ�AܓuA܍PA܉7A܁A�v�A�v�A�n�A�`BA�VA�I�A�7LA�+A��A��A۾wAۡ�A�|�A�K�A�+A�ƨAڮAڬAڥ�Aڝ�AړuAڋDA�jA�M�A��A���Aٰ!A�VA��A���A�=qA��A׬Aׇ+A�O�A�/A�A��A��HAցA�z�A�t�A�\)A�K�A�7LA��A���Aա�AՋDA�S�A�%AԮA�x�A�C�A��#A�|�A�E�A�+A��#AҴ9A�`BAѰ!A� �A���AЇ+A�XA��#A�p�A��AξwA�z�A�Q�A�G�A��A��;A͝�A͓uA�jA�/A�VA̸RA̅A�ffA�I�A�?}A�{A�1A��/A�ƨA˰!A˓uA˅A˃A�x�A�r�A�dZA�7LA��A��A�
=A�  A���Aʏ\AɼjA�VA�|�A�=qA��A�  A��;Aǩ�Aǉ7A�t�A�XA�5?A�+A�"�A��A��A�bA�1A�%A�%A�A�  A��mA���A���A�ƨA���A���A�A�ƨA���A��HA��HAƧ�A�|�A�S�A�A�A�E�A�1'A�A���Aş�AœuA�(�A��A��A��A�1A�%A�%A�bA��A�"�A�/A�E�A�E�A�p�A�r�A�x�A�z�A�v�A�t�A�|�A�t�A�z�AœuAŏ\Ař�Aŧ�AŴ9Aš�A�ffA�33A��AĸRAċDA�x�A�`BA�VA�S�A�I�A�E�A�=qA�9XA�33A�-A�$�A��A��A�bA�A���A��mA�ȴAìAÁA�l�A�E�A�/A��A�{A�{A�{A�oA�VA�JA�A�A���A��/A���A���A���A�ĜA´9A²-A�A�|�A�p�A�M�A�1A��A��A���A��A���A�{A���A�%A�A��PA��A�n�A�hsA�bNA�dZA�ffA�p�A�p�A�M�A���A���A���A��!A�ƨA��A��A��`A��
A�ȴA�ĜA�ĜA��^A���A��uA���A���A��PA�n�A�VA�C�A�$�A�VA�A���A��A��yA���A���A�ĜA��jA��RA���A��9A��!A���A��7A�r�A�`BA�O�A�=qA�=qA��A�
=A���A���A��A��A��A��`A��HA���A���A���A�ȴA�ƨA��wA��FA���A���A���A���A��\A��hA��hA��hA��\A�hsA�v�A�dZA�bNA�Q�A�bA�oA�bA��A���A��A��A��TA��/A���A�~�A�n�A�hsA�ZA�Q�A�(�A��A�oA�
=A�%A���A��;A��
A�ĜA��wA��FA���A���A�|�A�hsA�1A�Q�A��A�
=A��yA��#A��A���A���A��^A��A���A��\A�|�A�jA�XA�A�A�=qA�-A��A��HA��#A��A���A�ƨA��^A��uA�|�A�dZA�I�A�33A�"�A�oA���A��A��9A���A��\A�jA��mA�O�A���A��!A�~�A�bNA�\)A�S�A�Q�A�M�A�I�A�A�A�A�A�A�A�A�A�;dA�-A��A�ȴA�v�A�A�A�bA��A��yA��HA���A���A�p�A�XA�A�A��A���A��mA��wA��uA�bNA�A�A�?}A�$�A��mA���A�K�A���A�  A��+A�l�A�dZA�`BA�bNA�`BA�\)A�\)A�^5A�`BA�`BA�^5A�\)A�ZA�XA�I�A�5?A��A�oA�JA�A���A��yA��#A���A���A��jA���A�hsA�G�A��A�n�A�(�A��A���A��jA��9A���A��PA��7A�~�A�v�A�jA�bNA�ZA�9XA�(�A��A�bA��`A���A��DA�$�A�l�A�Q�A�G�A�E�A�$�A��A��
A��!A�ZA���A���A�S�A��A�oA�A��A��/A�ȴA��jA���A��A�ZA�+A�A��;A��-A���A�v�A�$�A���A��^A���A�`BA�G�A�A�S�A��9A�33A��yA��A�S�A�/A�A�;dA�
=A�r�A�-A�A��A��yA��TA��
A���A���A�ƨA��wA��jA��A���A���A��7A�ffA�A�A�$�A��A�
=A��A��mA�A���A��A�r�A�XA�C�A�+A�bA�A���A�ƨA��9A���A���A��DA��A�|�A�t�A�r�A�n�A�ffA�VA�G�A�C�A�5?A� �A�JA���A���A���A�t�A�G�A�JA��A���A��A�x�A�O�A�33A��A�p�A�K�A�"�A���A��/A��A�ffA�(�A��
A��9A��TA�t�A�33A���A��/A���A��RA��uA�hsA�C�A�/A�bA�A���A���A���A��A�ZA��A��A��
A��jA���A��A�Q�A�/A�JA���A��#A��RA��A���A�r�A�G�A�+A�oA���A��A��/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       A�
=A�-A�\)A�=qA�/A��A�A���A���A��A��A��A��yA��mA��mA��mA��`A��`A��TA��`A��TA��HA��#A�ȴA߸RAߟ�A� �A�7LA�%A��A���Aݣ�A�dZA�oA���Aܡ�Aܗ�A�n�A���Aڴ9A�ƨAכ�A֗�Aէ�A��HA���A�S�A���A̩�A��A�p�A��Aȥ�A�x�A��A��A�ȴA���A�;dA�;dA�bA�
=A�?}A�;dA��FA���A�XA���A��+A��A�/A��A��A�1'A�v�A�7LA�\)A��RA�r�A�7LA��-A�\)A�A��DA��A�z�A�~�A���A�hsA�l�A�{A�ȴA�K�A��A���A�l�A���A���A�C�A�bA�ĜA���A�VA�/A�n�A�ȴA�%A�ZA���A���A��!A�?}A���A�5?A��A�+A�&�AO�A}l�A|^5Az��Av~�Ar��AnZAj��Ag�Ae�7Aa��A_��A\{AXz�AW%AU�hAT  AQ��AO��AJ�DAE�AD^5AC`BAB�ABz�A@ȴA>v�A=l�A;K�A9�hA8=qA7��A6�A5G�A2-A0z�A/K�A-&�A,��A,1'A*bNA)�PA(bNA'"�A&E�A%�#A$bNA#�A#�FA#S�A"ĜA"JA!�wA Q�A�AAZAƨA�A�!AE�A��A�A��A�9AM�A;dA$�A��A&�A�!At�A;dAjA��A��AE�AJA�7A��A1A�hAO�A+A�\A
��A
JA	p�A�RA9XA�AZA��Ax�A+AĜA��Av�AZA1Ap�A��A  �@��H@�ff@�V@���@�=q@��h@��D@��F@�"�@���@�p�@�Z@�ƨ@�S�@��H@�@��@�@�l�@��@���@�5?@���@��@�bN@�K�@�@�%@蛦@�9X@��m@�C�@柾@��@�@�9@�bN@�1@���@߾w@߮@�t�@�"�@��y@ݡ�@�&�@���@�ȴ@�^5@��@�@�+@֏\@֏\@ՙ�@ԃ@�Z@�/@�?}@�?}@�G�@Ցh@ՙ�@Չ7@�p�@���@�j@ӶF@�33@��@��@�$�@�`B@�?}@ЋD@�ƨ@�
=@�~�@�5?@�J@���@��@�x�@̼j@̃@�r�@�I�@�9X@�1'@��@�dZ@ə�@��@ȋD@�9X@�1@ǥ�@Ɵ�@�$�@�@���@�p�@�/@���@�  @Å@�+@�@��y@���@�V@��T@�`B@��D@��;@�\)@���@�-@�`B@��/@�z�@�bN@�l�@�@���@�ff@�@��7@��9@��@�Q�@� �@�l�@��y@��R@���@�^5@��@�1'@� �@�b@�ƨ@�\)@�ff@�=q@��@��h@��@���@�Ĝ@�Ĝ@��j@���@���@���@��u@�bN@���@��;@�dZ@�+@�@���@���@�ff@��@��@���@��-@�X@��@�(�@��@�t�@�t�@�\)@�33@�
=@���@��H@���@�ff@�E�@�5?@�-@�{@��@��#@���@�hs@�7L@��`@�z�@���@��!@�ff@�^5@�=q@���@�/@���@�%@���@��@��j@�t�@���@��#@���@���@��@�hs@��@���@��9@�j@�1@�dZ@��@���@���@��@��h@�O�@��@�1@�t�@�33@��@�n�@���@�Ĝ@�Z@�9X@�1'@�1'@�1'@�(�@� �@��@�  @���@�S�@�33@�"�@��@��T@��^@���@��7@�`B@��@���@�C�@�+@���@��!@��\@�M�@���@���@���@�hs@�V@���@�Q�@��
@���@��P@��P@��P@��@�|�@�S�@�K�@�33@�@���@�-@��@��@�%@���@���@���@��u@��D@��@��@�Z@�ƨ@�l�@�+@��@��+@�@���@���@��7@��7@��@�p�@�`B@�O�@�7L@��@�j@���@��@�dZ@�+@�
=@��@�=q@��T@���@���@���@���@��@�z�@�j@�bN@�  @���@�ƨ@��F@��@��@���@���@���@��P@��@�l�@�S�@�33@�
=@���@��y@���@��R@��+@�^5@�{@���@���@��@��@��`@���@�bN@�;@;d@~�y@~v�@}�T@}`B@}?}@|��@|��@|�@|j@|I�@|9X@|�@{�F@{"�@z�H@z��@z�!@z�@yX@w�w@w�@w
=@v�y@vv�@v@u`B@t��@t�D@t1@r��@q�7@qG�@p��@p�9@pr�@pQ�@p �@oK�@n��@n��@nff@n$�@m��@m�h@m�@l�@l��@l��@l�D@lz�@lj@l9X@l�@k��@k�m@k�m@kƨ@k��@k33@j^5@i�@i�#@i�^@ix�@h��@hQ�@g��@g;d@fV@e�T@d�j@d9X@c�
@c�@c"�@a�@aX@a&�@a&�@a�@`��@`��@`Ĝ@`��@`��@`�u@`r�@`bN@`b@_�@_l�@^��@^@]V@[�
@Z��@Y�#@Yx�@YG�@X�`@XA�@W|�@Vȴ@V��@Vv�@VV@V5?@V{@U�@U�T@U�h@T�/@T��@T��@T�D@Tz�@Tj@T9X@T�@Sƨ@SC�@R�H@R~�@R=q@Q��@Q��@QG�@P�`@O��@OK�@OK�@O;d@N�y@N�y@N�@N��@Nff@N5?@M�T@MO�@MO�@M�@L�D@Lj@LI�@L�@K�
@K�@KS�@J��@J^5@J-@J�@JJ@I��@I��@I��@I�^@I%@H�@H  @F�R@F{@E�T@E�T@E��@E�T@E��@E�h@Ep�@E?}@D��@D�@C��@Cƨ@C��@C�@Ct�@Co@Bn�@B^5@B^5@B^5@BM�@B=q@B-@B�@B�@B�@BJ@A�#@A�7@A�7@A��@A��@A��@A�7@A%@@ �@?��@?�@?��@?��@?�P@?|�@?+@>�@>v�@=@=V@<j@<�@;�m@;ƨ@;ƨ@;dZ@;o@;@;@:�H@:��@:M�@:=q@9�#@9hs@9G�@9%@8Ĝ@7��@7;d@6�R@6E�@5��@5`B@4��@4��@4�D@4z�@4z�@4j@4Z@41@41@4�@3��@3�m@3�m@3ƨ@3��@3dZ@2�!@1�@17L@1%@0�`@0�u@0bN@/�@/�P@/�P@/K�@.�y@.ȴ@.��@.ff@.V@.V@.E�@.E�@.E�@.$�@.{@.@-�@-?}@,�@,��@,�D@,9X@,(�@+��@+dZ@+"�@+@*�@*�@*�H@*�\@*M�@*J@)��@)��@)7L@)%@(�`@(Ĝ@(�9@(��@(bN@(b@'�w@'K�@&ȴ@&$�@%�T@%�h@%p�@%`B@%O�@%/@$�@$�j@$��@$j@$�@#t�@"�@"^5@!�#@!x�@!x�@!x�@!X@!G�@ �`@|�@��@ȴ@v�@$�@{@@�@/@�j@j@(�@�m@��@��@�^@��@��@��@x�@7L@r�@A�@�@�w@��@l�@\)@K�@K�@K�@K�@;d@�y@�@��@�+@v�@��@��@�R@�R@ȴ@�R@��@�+@ff@ff@5?@$�@$�@$�@{@@p�@O�@�@�@�/@�/@�j@�j@�@�@�D@I�@9X@�
@�F@�@dZ@dZ@33@"�@�@��@�!@~�@=q@��@X@&�@%@�9@A�@1'@  @��@�@K�@��@ȴ@�+@E�@5?@5?@$�@��@��@O�@�@�j@�@�@��@�D@Z@I�@(�@�
@��@t�G�O�A��A��A�1A���A��
A��A��A�^5A�K�A�?}A�5?A�5?A�5?A�(�A�$�A��A�JA�1A�A�A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��yA��yA��yA��mA��`A��`A��mA��mA��mA��yA��yA��yA��yA��yA��A��yA��yA��yA��mA��`A��`A��TA��TA��HA��HA��HA��TA��TA��`A��mA��mA��mA��mA��`A��`A��TA��HA��HA��HA��TA��TA��TA��`A��mA��mA��mA��`A��TA��HA��HA��;A��HA��HA��TA��TA��TA��HA��HA��HA��;A��/A��/A��
A��
A���A���A���A���A�ȴA���A�ƨA�ĜA�ĜA�A���A߸RAߴ9A߶FAߴ9A߲-A߶FA߶FA߶FAߴ9A߰!A߰!Aߩ�Aߥ�AߓuA߃A�z�A�n�A�hsA�ZA�K�A�5?A��A���A޼jAދDA�p�A�XA�=qA�+A�(�A� �A��A��A��A�VA�
=A�A���A���A���A���A���A���A��A��A��A��yA��`A��HA��HA��/A���A���A���A���A���A�AݶFAݰ!AݮAݬAݬAݩ�Aݧ�Aݥ�AݑhA݉7A݁A�x�A�r�A�hsA�\)A�I�A�;dA�9XA�33A�&�A��A�bA�VA�A���A��A��A��;A��A���A�ƨAܼjAܸRAܲ-AܬAܥ�Aܡ�Aܝ�Aܛ�Aܙ�Aܛ�Aܛ�Aܝ�Aܝ�Aܛ�Aܙ�Aܕ�Aܕ�AܓuA܍PA܉7A܁A�v�A�v�A�n�A�`BA�VA�I�A�7LA�+A��A��A۾wAۡ�A�|�A�K�A�+A�ƨAڮAڬAڥ�Aڝ�AړuAڋDA�jA�M�A��A���Aٰ!A�VA��A���A�=qA��A׬Aׇ+A�O�A�/A�A��A��HAցA�z�A�t�A�\)A�K�A�7LA��A���Aա�AՋDA�S�A�%AԮA�x�A�C�A��#A�|�A�E�A�+A��#AҴ9A�`BAѰ!A� �A���AЇ+A�XA��#A�p�A��AξwA�z�A�Q�A�G�A��A��;A͝�A͓uA�jA�/A�VA̸RA̅A�ffA�I�A�?}A�{A�1A��/A�ƨA˰!A˓uA˅A˃A�x�A�r�A�dZA�7LA��A��A�
=A�  A���Aʏ\AɼjA�VA�|�A�=qA��A�  A��;Aǩ�Aǉ7A�t�A�XA�5?A�+A�"�A��A��A�bA�1A�%A�%A�A�  A��mA���A���A�ƨA���A���A�A�ƨA���A��HA��HAƧ�A�|�A�S�A�A�A�E�A�1'A�A���Aş�AœuA�(�A��A��A��A�1A�%A�%A�bA��A�"�A�/A�E�A�E�A�p�A�r�A�x�A�z�A�v�A�t�A�|�A�t�A�z�AœuAŏ\Ař�Aŧ�AŴ9Aš�A�ffA�33A��AĸRAċDA�x�A�`BA�VA�S�A�I�A�E�A�=qA�9XA�33A�-A�$�A��A��A�bA�A���A��mA�ȴAìAÁA�l�A�E�A�/A��A�{A�{A�{A�oA�VA�JA�A�A���A��/A���A���A���A�ĜA´9A²-A�A�|�A�p�A�M�A�1A��A��A���A��A���A�{A���A�%A�A��PA��A�n�A�hsA�bNA�dZA�ffA�p�A�p�A�M�A���A���A���A��!A�ƨA��A��A��`A��
A�ȴA�ĜA�ĜA��^A���A��uA���A���A��PA�n�A�VA�C�A�$�A�VA�A���A��A��yA���A���A�ĜA��jA��RA���A��9A��!A���A��7A�r�A�`BA�O�A�=qA�=qA��A�
=A���A���A��A��A��A��`A��HA���A���A���A�ȴA�ƨA��wA��FA���A���A���A���A��\A��hA��hA��hA��\A�hsA�v�A�dZA�bNA�Q�A�bA�oA�bA��A���A��A��A��TA��/A���A�~�A�n�A�hsA�ZA�Q�A�(�A��A�oA�
=A�%A���A��;A��
A�ĜA��wA��FA���A���A�|�A�hsA�1A�Q�A��A�
=A��yA��#A��A���A���A��^A��A���A��\A�|�A�jA�XA�A�A�=qA�-A��A��HA��#A��A���A�ƨA��^A��uA�|�A�dZA�I�A�33A�"�A�oA���A��A��9A���A��\A�jA��mA�O�A���A��!A�~�A�bNA�\)A�S�A�Q�A�M�A�I�A�A�A�A�A�A�A�A�A�;dA�-A��A�ȴA�v�A�A�A�bA��A��yA��HA���A���A�p�A�XA�A�A��A���A��mA��wA��uA�bNA�A�A�?}A�$�A��mA���A�K�A���A�  A��+A�l�A�dZA�`BA�bNA�`BA�\)A�\)A�^5A�`BA�`BA�^5A�\)A�ZA�XA�I�A�5?A��A�oA�JA�A���A��yA��#A���A���A��jA���A�hsA�G�A��A�n�A�(�A��A���A��jA��9A���A��PA��7A�~�A�v�A�jA�bNA�ZA�9XA�(�A��A�bA��`A���A��DA�$�A�l�A�Q�A�G�A�E�A�$�A��A��
A��!A�ZA���A���A�S�A��A�oA�A��A��/A�ȴA��jA���A��A�ZA�+A�A��;A��-A���A�v�A�$�A���A��^A���A�`BA�G�A�A�S�A��9A�33A��yA��A�S�A�/A�A�;dA�
=A�r�A�-A�A��A��yA��TA��
A���A���A�ƨA��wA��jA��A���A���A��7A�ffA�A�A�$�A��A�
=A��A��mA�A���A��A�r�A�XA�C�A�+A�bA�A���A�ƨA��9A���A���A��DA��A�|�A�t�A�r�A�n�A�ffA�VA�G�A�C�A�5?A� �A�JA���A���A���A�t�A�G�A�JA��A���A��A�x�A�O�A�33A��A�p�A�K�A�"�A���A��/A��A�ffA�(�A��
A��9A��TA�t�A�33A���A��/A���A��RA��uA�hsA�C�A�/A�bA�A���A���A���A��A�ZA��A��A��
A��jA���A��A�Q�A�/A�JA���A��#A��RA��A���A�r�A�G�A�+A�oA���A��A��/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
"�B
&�B
#:B
 �B
�B
 �B
!B
�B
�B
�B
�B
!B
�B
�B
!B
!B
�B
�B
�B
�B
�B
VB
�B
~B
�B
�B
�B
�B
�B
(B
�B
VB
�B
bB
�B
�B
�B
JB
�B
�B
YB
 �B
 �B
"hB
,�B
3�B
:�B
;0B
@�B
C�B
D�B
GzB
jB
sB
wfB
{�B
�xB
�0B
��B  B
�B
�BBd&B��B�NB�B�B��B�dB��BרB��B�fB�GB�2B�vB� B�B�TB�B�B;B��B�B��B�B��B��B�HB��B�lB��B�Bz�BuZBqABe`BU�BK^B&�B�B�B�B
�`B
�"B
�B
��B
��B
��B
�[B
�YB
��B
wfB
jB
W�B
8�B
&�B
B
JB
{B	�sB	��B	�LB	��B	��B	�B	m]B	`BB	S�B	@�B	<�B	5B	+�B	�B	FB	�B�B��B�B��B��B�B�B��B�
B՛BбB�vB�jB��B�TBɺB��B�zB��B��B�mB��B�OBĜB��B��B��B��B� B��B�-B��B�'BȴB�pBϫB�B�9B�?B�KBٴB�/B�B�B�;BߤB�&B�B�>B��B��B��B�B��B�B��B	B	YB	
�B	xB	_B	�B	{B	�B	
=B	�B	�B	+B	�B		B	OB	#�B	OB	�B	�B	eB	YB	$B	B	�B	B	�B	 B	�B	�B	�B	@B	�B	�B	B	4B	�B	�B	�B	PB		�B		lB	�B	�B	%B	�B	�B	%B	�B	YB	�B	�B	�B	
�B	(B	FB	SB	�B	�B	_B	�B	"�B	&B	�B	VB	�B	 �B	 'B	 'B	 \B	�B	!B	%B	*eB	1[B	B�B	J#B	C�B	8�B	=B	<B	;�B	@B	IB	JXB	U�B	[�B	\]B	^�B	a�B	f�B	iyB	k�B	o B	p;B	r�B	s�B	v`B	w�B	x8B	|�B	��B	��B	��B	�+B	�DB	��B	��B	��B	�4B	�oB	��B	�hB	�:B	�@B	�FB	�{B	��B	�$B	�OB	��B	��B	��B	��B	�4B	�$B	��B	��B	��B	�=B	��B	�wB	��B	��B	�9B	�nB	��B	�B	��B	�RB	��B	��B	��B	�UB	��B	�9B	��B	�EB	�B	�mB	ƨB	�?B	ǮB	�zB	�B	ȀB	�XB	�#B	�XB	��B	��B	��B	ϫB	��B	�B	՛B	��B	҉B	҉B	�&B	�,B	ٴB	ںB	�/B	�]B	�jB	�/B	�dB	�dB	��B	�B	�pB	�;B	ߤB	�B	�`B	�2B	�B	�sB	�sB	�sB	��B	��B	�KB	�KB	�B	�B	�B	�B	��B	�iB	�B	��B	��B	�5B	�B	��B	��B	�B	�cB	��B	�/B	��B	�/B	�cB	�cB	�5B	��B	��B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�fB	��B	�2B	�B	��B	��B	��B	��B	�PB	��B	�B	�VB	�"B	�VB	��B	�.B	��B
 4B	��B	�cB
�B
oB
AB
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	lB
	�B
	lB
	lB

=B
~B
~B
~B
~B
~B
�B
 B
�B
�B
�B
:B
�B
B
�B
@B
B
�B
{B
{B
�B
�B
$B
YB
$B
YB
YB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
qB
�B
�B
�B
�B
qB
�B
�B
�B
B
~B
�B
!B
�B
!B
!B
!B
!B
!B
!B
!B
!B
�B
 �B
!bB
!-B
!bB
!�B
!�B
!�B
"�B
#:B
#�B
$�B
$�B
%B
%FB
%FB
%FB
%B
&LB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
'�B
($B
(XB
)*B
)*B
)�B
)�B
*eB
*0B
*�B
+kB
+�B
,=B
,qB
,�B
,�B
-wB
-wB
-wB
-�B
-�B
-�B
.B
-�B
-�B
.}B
.�B
.�B
.�B
.}B
.�B
/�B
0�B
0�B
0�B
0�B
1�B
1�B
2aB
2aB
2aB
2�B
4B
4�B
4�B
4�B
5B
5B
5B
5?B
6B
6B
6zB
6FB
6�B
6�B
6�B
6�B
7�B
7�B
7�B
7LB
7�B
7�B
7�B
7�B
8B
7�B
7�B
7�B
7�B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;0B
;�B
<�B
<jB
>B
=�B
>B
>B
>BB
@B
?�B
?�B
?�B
?�B
?�B
@B
@B
@OB
@B
@B
@OB
@B
@�B
@�B
@OB
@�B
A B
B'B
C-B
C�B
DgB
D�B
D�B
D�B
E�B
F?B
F�B
F?B
F�B
F�B
GB
F�B
GB
GB
GzB
HB
HKB
HB
HB
HKB
HKB
HKB
HB
H�B
H�B
IRB
I�B
J#B
J#B
I�B
J#B
J�B
L0B
LdB
L0B
L0B
L�B
LdB
LdB
MB
L�B
MB
MjB
M�B
M6B
NB
N�B
NpB
NpB
N�B
N�B
OB
OB
O�B
O�B
PB
PHB
PB
P}B
PHB
PHB
PB
P}B
Q�B
Q�B
S&B
S�B
S�B
S�B
S�B
S[B
S[B
S�B
S�B
S�B
T�B
T�B
T�B
U2B
U2B
T�B
T�B
UgB
V9B
VB
V9B
V9B
V9B
VmB
V�B
V�B
W
B
V�B
V�B
WsB
W
B
W
B
W?B
V�B
W
B
V�B
WsB
YB
XEB
XyB
XyB
X�B
XEB
XEB
XyB
YKB
XyB
Y�B
ZQB
Z�B
Z�B
[#B
[�B
[#B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\)B
\�B
\�B
\�B
\�B
\�B
^�B
^B
_;B
^�B
_�B
_�B
_�B
`BB
`B
`�B
`BB
`�B
`vB
`�B
`vB
`vB
`�B
`�B
`vB
`�B
`vB
`�B
a|B
bNB
c B
b�B
b�B
c B
b�B
c�B
d&B
c�B
d&B
d�B
d�B
d�B
e,B
d�B
d�B
e,B
d�B
d�B
e,B
e,B
d�B
f2B
e�B
f2B
ffB
ffB
gB
f�B
g�B
gmB
g�B
g�B
g�B
g�B
g�B
h>B
h>B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
jKB
jKB
j�B
kQB
k�B
k�B
lWB
lWB
lWB
l"B
lWB
l�B
l�B
l�B
l�B
m)B
m)B
m]B
l�B
m�B
n�B
n�B
n�B
o B
o5B
o�B
qvB
qvB
qvB
rB
rB
rGB
r|B
r�B
r�B
s�B
s�B
s�B
s�B
u�B
u�B
v+B
v+B
u�B
v+B
u�B
v`B
w2B
w2B
w�B
w�B
wfB
xB
wfB
w�B
w�B
wfB
w�B
w�B
w�B
wfB
xB
w2B
w�B
w2B
w�B
w2B
w�B
w2B
w�B
w2B
w2B
w�B
w2B
wfB
w�B
w2B
w2B
w�B
w�B
xlB
xlB
xlB
x�B
xlB
x�B
y>B
xlB
x�B
x8B
x�B
x�B
xlB
y	B
x�B
x�B
yrB
x�B
yrB
y	B
y�B
y>B
y�B
y�B
zB
zB
z�B
{B
{�B
|B
|B
|PB
|�B
|�B
|�B
}"B
|�B
}VB
}VB
~]B
}�B
}�B
}�B
~�B
~(B
~�B
~�B
cB
� B
�B
.B
�B
��B
�oB
��B
��B
��B
�uB
��B
&�B
�B
$B
�B
%FB
$B
#�B
&LB
!�B
!�B
"hB
!-B
�B
 'B
�B
VB
�B
�B
OB
~B
�B
B
�B
B
�B
VB
 'B
�B
�B
!B
�B
�B
OB
�B
B
B
OB
B
�B
OB
�B
�B
!B
�B
�B
VB
�B
�B
OB
�B
B
OB
�B
B
OB
�B
B
!B
VB
VB
 'B
 'B
 �B
 �B
 �B
 'B
�B
!B
�B
�B
�B
!B
VB
VB
 'B
 �B
 �B
 �B
 \B
 'B
�B
�B
OB
�B
OB
�B
�B
�B
 \B
 \B
�B
 'B
!B
OB
OB
OB
�B
B
OB
OB
�B
 �B
~B
!�B
�B
�B
�B
�B
�B
�B
qB
qB
CB
CB
�B
~B
B
CB
�B
	B
B
�B
B
eB
1B
�B
kB
�B
_B
�B
�B
B
�B
�B
�B
@B
B
�B
�B
�B
B
�B
~B
�B
\B
�B
�B
\B
�B
�B
.B
�B
�B
�B
�B
�B
VB
�B
�B
�B
.B
�B
bB
�B
.B
�B
\B
�B
�B
VB
.B
�B
�B
�B
(B
"B
"B
�B
�B
�B
JB
�B
B
�B
�B
bB
hB
.B
\B
(B
�B
4B
�B
�B
�B
.B
(B
�B
(B
\B
�B
VB
�B
VB
PB
�B
�B
�B
�B
VB
"B
�B
�B
�B
�B
~B
JB
JB
xB
�B
JB
�B
�B
JB
DB
~B
�B
~B
�B
PB
JB
B
FB
�B
�B
 B
FB
B
"�B
@B
�B
:B
�B
 B
 B
{B
�B
uB
{B
eB
�B
@B
�B
1�B
�B
�B
 'B
 'B
 �B
#�B
=B
!�B
$tB
VB
VB
!�B
B
OB
!�B
%�B
#:B
�B
$tB
 �B
/�B
%zB
.�B
3hB
-wB
'�B
*eB
1'B
)*B
8�B
>wB
6FB
:�B
.IB
2�B
9�B
:�B
@B
B[B
;�B
1�B
6FB
8RB
7�B
>BB
7�B
?�B
;dB
?�B
G�B
A�B
B�B
@�B
?�B
E�B
@�B
D�B
EB
D�B
E9B
D�B
D�B
D�B
DgB
FB
HKB
IB
EB
D�B
CaB
K^B
N<B
p;B
y>B
lWB
k�B
g�B
i�B
j�B
o5B
qvB
rB
v+B
v`B
v�B
v�B
w2B
xB
yrB
y	B
x�B
x�B
y	B
yrB
~]B
�B
cB
�oB
�B
�uB
��B
��B
��B
�oB
��B
�B
��B
�-B
�FB
��B
�9B
�[B
�EB
��B
��B
��B
��B
��B
��B
��B
ɆB
�jB
��B
�QB
��B
�B
�
B
��B
�sB
�"B
��B
��B
�(B
��B
�	B
�lB
��B �B�B 4B
�B	B�BFB�BBB
��B
�TB
��B
�GB
�B
�ZB
�2B
�8B
�fB
��B
��B
�+B
�%B
�B
�B
��B
�vB
�)B
�8B
�B
�dB
��B
��B
ޞB
�|B
�B
��B
�B
�B
�B
�B
��B
�ZB
�B
��B
�mB
�yB
��B
��B
�B
�AB
�cB�B�B1B�B_B�B�B&B#nB9$B=B=<BS�B<�BVmBQNBR�BU�BY�BX�B[#Bd�Bm�Bt�BwfB}�B��B�qB��B�!B��B��BÖB��B�B�XB�vB�vBٴB�B��B�B�B�B�>B��B�|B�;B�BB�B�B�mB��B�B��B��B��B�dB��B��B�B�HB�`B�vB�B�2B�B�|B�|B�B�HB�B�jBݘB�jB��B�B�pB��B�B�;BݘB�dB�pB�BB�5B�]B��BیBںBޞB��B��B��B�BB�5BیB�WB�QB�B�#B��B�WB��B�/B��B�B��BچB�B�B��B�B�yB�mB�QB�B�9BԕB��BӏB�gB�,BԕB��B�B�,B��B�B�[B�2B�aB��BԕB�2B�?B�QB��B�jBߤB�B�B�ZB�DB�B�iB�B��B�WB�B��B�oB��B��B�B�MB�B�	B�B��B�PB��B�MB��B��B�B�B�BB�B��BޞB�;B��B�5B�;B�B� B�TB�,B�yB�B�B��B��B�B�oB�/B�WB�B��B�AB�iB��B�TB��B�MB��B��B�B��B�B�WB� B��B�>B��BB4B�B�]B�cB �B��B 4B�BAB�BB �B�BuBABBB�B�B�B�B iB�.B��B �B��B��B�DB��B�B��B�DB_B�B�B�B�WB��B�)B�/B�sB�B�sB�yB�B�B��B�mB��B�fB��B��B�"B��B�B�TB�B�B�DB�B� B�B�iB�B�]B��B�B�WB��B�]B��B�B�mB�B�EB��B�yBԕB�B�&B�BΥBбB��B�B�B�}B�OB�B�KB��B�B�LB��B��B�!B�'B��B��B��B�hB�bB�B��B��B�DB�=B�1B��B�rB��B�lB��B��B�YB�rB�7B�B��B�SB��B��B��B.B�oB}VB�ABcB|�B|PB}"BzDB�uBzxBx�Bu�By�Bx�Bv+Bw2BuZBu�Br�Bu�BsBqvBrGBqABpBp�Br�Bo�Bk�Bm]Bi�Bd�Bd&Bc�Bb�B]�B]�Ba|BS�BT�BQNBOBBJ�BOvBN�BHBL�Bt�BA�B7LB.}B-B#�B%FB$�B#�B$BVBB	B+B�B�BB{BFB�BxB1BfB+B1BSB�B
��B
�cB
�"B
��B
��B
��B
��B
�fB
��B
�;B
��B
�B
�]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       B
!B
"�B
dB
kB
�B
B
_B
B
�B
B
�B
B
�B
�B
B
B
yB
yB
�B
yB
yB
_B
EB
�B
�B
�B
TB
�B
fB
�B
	�B
�B
	�B

�B
	RB
�B
�B
�B
�B
�B
�B
�B
!B
%`B
1�B
:DB
;�B
:^B
=�B
?�B
AoB
MPB
iyB
nB
qAB
u�B
��B
�B
�dB
�}B
��B
�bB�B`vB�BޞB��BںB�1BڠB�_BٚB��B�B��B�xB�dB�5B�B��B��B�VBBB�KB�B�B�B��B�4B��B�mB�3B~(Bu�Bq�Bq�Bh$B\]BQ�B$�B�B0B
�.B
�3B
��B
��B
��B
ѷB
ǔB
��B
�KB
�B
xlB
o�B
]B
;0B
%zB
B
�B
	�B	� B	�B	�B	�UB	�(B	��B	m�B	e�B	XEB	>�B	:�B	3hB	+�B	CB	�B	jB��B�QB�fB��B��B�XB��B�1B�9B�&B�B��B��B�hB�hB�_B��B�[B�HB��B��B�.B��B��B��B��B��B��B��B��B��B�qB�iB�B�lB̈́B�BЗB�&B��B�MB��B׍B�EB�BܬB�B�B� B��B�
B�B�)B�B��B�JB�.B	oB	YB	EB	'B��B�VB�]B	�B	B		7B	�B	�B	�B	�B	VB	+B	�B	B	�B	4B	�B	}B	bB	uB	NB	�B	�B		7B		RB	B	B	�B	�B	�B	�B	�B	�B	zB	�B	GB	�B	 �B	�B	-B��B	  B	 B	 �B	 �B	;B	�B	?B	
	B	"B	.B	NB	B	�B	�B	#�B	!|B	_B	B	�B	B	eB	�B	B	�B	kB	�B	"4B	)B	="B	MB	@OB	2-B	72B	5ZB	6zB	;B	BAB	A�B	NpB	T�B	UMB	W?B	Z�B	_�B	b�B	e�B	iB	j�B	l�B	mCB	pB	r-B	r�B	u�B	z�B	~�B	~�B	�;B	��B	�B	��B	�	B	�)B	��B	�B	��B	��B	�dB	�jB	�"B	��B	�[B	�_B	��B	�7B	�7B	��B	�B	��B	�NB	� B	��B	��B	�fB	�XB	��B	�=B	��B	��B	�B	��B	��B	�aB	��B	��B	��B	�B	�"B	��B	��B	��B	�cB	�4B	��B	��B	�;B	�UB	�B	��B	��B	ÖB	��B	�SB	��B	�7B	�B	ɆB	�0B	�vB	�0B	��B	�0B	�B	�B	�B	�aB	��B	ևB	רB	�SB	�mB	ևB	�
B	�+B	�yB	�_B	�B	��B	ޞB	�B	�B	��B	�B	��B	�B	�nB	�B	�B	�B	�@B	��B	�B	�$B	�B	�B	�
B	�$B	�B	�B	�B	�mB	�B	�B	�B	�RB	�8B	�mB	�B	�B	�B	�>B	�sB	�B	��B	�cB	�}B	��B	�wB	�/B	�iB	�[B	��B	��B	��B	�B	�hB	�B	�`B	�FB	��B	��B	��B	��B	��B	�zB	��B	��B	�XB	�$B	��B	�>B	��B	��B	��B	��B	�(B	�B	�wB	�B	��B	��B
�B
�B
;B
 �B
B
 �B
 �B
 �B
 �B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
%B
�B
DB

�B

=B
)B
xB
^B
�B
JB
�B
�B
jB
<B
"B
\B
.B
HB
bB
.B
bB
}B
�B
�B
�B
NB
hB
:B
�B
aB
B
,B
aB
�B
�B
�B
�B
�B
�B
�B
SB
mB
�B
$B
EB
_B
B
EB
+B
+B
EB
EB
EB
_B
yB
KB
=B
�B
kB
�B
�B
=B
�B
�B
�B
�B
B
5B
OB
OB
jB
jB
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
�B
�B
�B
�B
 vB
 �B
 �B
 �B
!-B
!HB
!|B
!�B
"�B
"�B
"�B
#TB
#�B
#�B
$@B
%FB
%zB
%�B
%�B
&2B
&LB
&�B
&�B
&�B
&�B
'B
'B
'B
'B
'8B
(
B
'�B
'�B
'�B
(
B
(�B
*0B
*B
*B
*B
*0B
+B
+6B
+�B
+�B
+�B
,�B
./B
.B
-�B
.B
.IB
./B
.IB
/ B
/iB
/iB
/�B
/�B
0B
0!B
/�B
0oB
0�B
0�B
0�B
0UB
0�B
0�B
1B
0�B
1'B
0�B
1B
1B
1[B
2GB
33B
2�B
2�B
33B
3�B
49B
4�B
4�B
5tB
6+B
6zB
7�B
72B
7fB
7�B
8lB
9�B
9	B
8�B
8�B
9	B
9	B
9$B
9>B
9XB
9$B
9>B
9rB
9rB
9�B
9�B
9�B
:�B
;B
<PB
=<B
=�B
=�B
>B
>BB
>wB
?�B
?�B
?�B
?}B
@ B
?�B
@4B
@ B
@4B
@iB
A B
AUB
AUB
A B
A B
AUB
AoB
AoB
AoB
B[B
BAB
B�B
B�B
C�B
CGB
CGB
C�B
D�B
E�B
EmB
ESB
E�B
E�B
EmB
E�B
F?B
FB
FYB
F�B
F�B
FtB
G�B
G�B
G�B
G�B
HB
H1B
HKB
H�B
I7B
IB
IB
IRB
IRB
I�B
IRB
IlB
I�B
J	B
KDB
L0B
L�B
L�B
L�B
L�B
L~B
LdB
L�B
MB
L�B
M6B
NpB
M�B
M�B
NVB
N<B
N"B
NpB
N�B
OBB
OB
OBB
OBB
OBB
OvB
O�B
O�B
PB
O�B
PB
P�B
PB
O�B
PHB
O�B
P.B
PbB
QNB
RoB
QhB
Q�B
Q�B
Q�B
QhB
Q�B
Q�B
R�B
R B
S�B
S�B
TB
S�B
TFB
T�B
T{B
T�B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
VB
VB
U�B
VB
V�B
XB
W�B
X�B
XB
YB
X�B
Y1B
YKB
YB
Y�B
YKB
Y�B
Y�B
Y�B
YeB
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
Z�B
[=B
[�B
\CB
[�B
\CB
\]B
\]B
]IB
]/B
\�B
]~B
]�B
]�B
^B
^5B
^B
^B
^5B
^B
^B
^5B
^OB
^�B
_pB
_VB
_VB
_�B
_�B
`'B
`'B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a|B
a|B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
b�B
b�B
cnB
c�B
c�B
dtB
d�B
e,B
eFB
ezB
e`B
e`B
eFB
e�B
e�B
e�B
e�B
fB
f�B
f�B
f�B
ffB
gB
g�B
g�B
g�B
h$B
h�B
i�B
j�B
j�B
j�B
kQB
kB
k�B
k�B
lB
l"B
l�B
l�B
l�B
nB
o5B
o5B
o5B
o5B
o B
oOB
oOB
pB
poB
p�B
p�B
p�B
p�B
qB
poB
p�B
p�B
poB
p�B
p�B
p�B
p�B
qB
p;B
p�B
p;B
p�B
p;B
p�B
p;B
p�B
p;B
pUB
p�B
pUB
poB
p�B
p;B
pUB
q'B
q'B
q�B
q�B
q�B
q�B
qvB
q�B
rGB
qvB
q�B
q[B
q�B
q�B
q�B
r-B
q�B
q�B
r|B
q�B
r|B
r-B
r�B
raB
r�B
r�B
sMB
s�B
tB
t�B
u?B
u�B
u%B
utB
vB
vB
u�B
vzB
vB
v�B
v�B
wfB
v�B
v�B
v�B
w�B
w�B
w�B
xB
xlB
y	B
x�B
x8B
x�B
y�B
z�B
z�B
z�B
z�B
{�G�O�B
�B
sB
�B
�B
5B
�B
�B
;B
�B
�B
WB
B
yB
B
yB
EB
yB
sB
?B
mB
sB

B
�B

B
sB
EB
B
�B
�B
B
�B
sB
?B
�B

B

B
?B

B
�B
?B
sB
sB
B
�B
yB
EB
�B
�B
?B
�B

B
?B
sB

B
?B
sB

B
B
EB
EB
B
B
B
B
B
B
�B
B
�B
sB
sB
B
EB
EB
B
�B
B
�B
KB
B
�B
�B
?B
�B
?B
�B
�B
�B
KB
KB
�B
B
B
?B
?B
?B
sB

B
?B
?B
�B
�B
mB
�B
�B
�B
�B
�B
�B
�B
aB
aB
2B
2B
�B
mB
�B
2B
�B
�B
�B
�B
�B
TB
 B
�B
[B
�B
NB
�B
}B
B
vB
�B
�B
0B
B
�B
�B
�B
B
�B
mB
tB
KB
zB
�B
KB
�B
�B
	B
�B
�B
�B
�B
�B
EB
�B
�B
zB
	B
�B
	RB
�B
	B
	�B
KB
�B
�B
EB
	B
�B
�B
�B
B
B
B
�B
�B
�B
9B
�B
B
tB
�B
	RB

XB
	B
KB
B
	�B

#B
�B
�B
�B
	B
B
�B
B
KB
�B
EB
�B
EB
?B
�B
�B
�B
zB
EB
B
�B
�B
�B
�B
mB
9B
9B
gB
�B
9B
�B
�B
9B
3B
mB
�B
mB
�B
?B
9B
B
6B
�B
	�B
	�B
6B
B
�B
0B
�B
)B

�B
	�B
	�B
jB
pB
dB
jB
TB
�B
0B
�B
*�B
�B
�B
B
B
�B
�B
,B
�B
dB
EB
EB
�B

B
?B
�B
�B
)B
�B
dB
B
(�B
jB
'�B
,WB
&fB
 �B
#TB
*B
"B
1�B
7fB
/5B
3�B
'8B
+�B
2�B
3�B
9	B
;JB
4�B
*�B
/5B
1AB
0oB
72B
0�B
8�B
4TB
8�B
@�B
:�B
;�B
9rB
8�B
>�B
9�B
=�B
=�B
=�B
>(B
=�B
=�B
=�B
=VB
>�B
A B
BB
=�B
=�B
<PB
DMB
GB
iB
rB
e,B
d�B
`�B
b�B
c�B
h
B
jKB
j�B
o B
o5B
oiB
oiB
pB
p�B
rGB
q�B
q�B
qvB
q�B
rGB
w2B
x�B
x8B
zDB
z�B
{JB
��B
��B
�_B
�DB
��B
��B
�eB
�B
�B
��B
�B
�0B
�B
��B
�zB
ÖB
��B
�XB
��B
��B
�[B
�?B
��B
�&B
ٴB
�|B
��B
�iB
�HB
��B
��B
��B
��B
�oB
��B
�AB
�ZB
�rB
��B
�	B
��B�B_BB
�VB
��B
��B
�oB
�)B
�oB
�B
��B
�/B
�B
�B
�;B
�B
�iB
� B
��B
��B
��B
�B
�KB
��B
�B
�pB
�9B
ٴB
רB
�sB
�QB
��B
ںB
��B
��B
ܒB
�]B
��B
�/B
�jB
ߤB
�BB
�NB
��B
��B
�yB
�B
�8B
��B
��BB�B4B�B�B�BCB1�B5�B6BL�B5�BOBBJ#BK�BNpBR�BQ�BS�B]dBf�Bm]Bp;Bv�B��B�FB��B��B��B�XB�jB��B��B�-B�1B�1B�oB�=B�B�:B߾B�\B��BڠB�7B��B��B�eB�=B�'BՁB�kB׍BٚBؓB�BڠBڠB�CB�B�B�1B�xB��B�qB�7B�7B�=B�B��B�$B�SB�$BڠB��B�+B׍B��B��B�SB�B�+B��B��B�B�{B�FB�uB�YBөBԯBԯB��B��B�FB�B�B��B��BԯB�BөB��B��B��BѝB�[BּB�qB׍B��B�4B�(B�B��B�B�PB�~B�dB�<B��B�PB�~B�OB�BϫB��B�0B��B�6B��B�jB��B�B�B��B�?B�_B�=B߾B�B��B�LB�$B�RB�B�B�RB�B�*B�B��B��B�B�kB��B�9B�zB�B�B�B�hB�FB��B�@B�B�=BٴB�sB��B��B�
B��B��B��B�)B�B�4B�tB��B�B�B�qB�*B��B�B�:B�B��B�$B�IB�B�nB�B�B�[B��B�UB��B�B�B�IB��B�aB��B	�B�HB�B�B�XB�RB��B�^B��B��B��B�XB�^B�0B��B��B��B��B��B�^B�^B�$B��B�RB��B��B��B��B��B��B�hB��BB�dB�B�yB�,B�B��B�B�HB��B�HB�NB��B�vB��B�BB�B�;B��BߤB��B�B�B�)B�WBچB�B��B��B�vB�>B�B�2BޞB��B�,BԕB�2B��B��B�BB��B�B��B�NB�jB��B��B��B�zBɆB��B��B�B�RB�$B��B� BƨB�]B�!B��B�_B��B��B��B�nB�qB�=B�7B��B��B�uB�B�B�B�oB�GB��B�AB��B�iB.B�GB�B}�BzxB~(B}�ByrB~�BxBzDBv+B{Bx8BuZBu%Bu�BsB{JBsMBqvBn�Br�BqvBo BpBn/BncBk�Bn�Bk�BjKBkBjBh�BiyBk�Bh�Bd�Bf2Bb�B]dB\�B\�B[WBV�BV�BZQBL�BM�BJ#BHBC�BHKBG�B@�BE�Bm�B:�B0!B'RB%�B�BB~B�B�B+B�B�B BbB{B�BPBB�BMBB;B  BB
�(B
��B
�`B
�8B
��B
�|B
��B
�cB
��B
�;B
�B
�B
�B
�fB
�2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<IU�<#�
<#�
<4�*<Tb�<hA�<#�
<#�
<#�
<#�
<#�
<`O{<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(xs<#�
<#�
<#�
<;��<#�
<#�
<#�
<m�<#�
<#�
<'��<M�A<#�
<Qs�<7>�<:=�<���<�z0<#�
<#�
<#�
<#�
<#�
<#�
<#�
<3�<n�<l<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<P�!<,X�<'B�<#�
<#�
<Y�A<V��<)�<#�
<#�
<#�
<a�x<hEJ<i��<O<,q<#�
<P1�<#�
<Y��<Fhv<#�
<#�
<#�
<#�
<#�
<�g�<uA�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0069(+/-0.0048)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0069(+/-0.0048)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2020110923311020201109233110IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400402020210224004020QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400402020210224004020QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714015120210427140151IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                