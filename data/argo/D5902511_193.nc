CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-12-16T18:24:57Z creation; 2022-02-04T23:30:06Z DMQC;      
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 >T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 eP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20211216182457  20220204223521  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_193                 6810_008521_193                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٪��v5�@٪��v5�11  @٪��&��@٪��&��@0���Ew�@0���Ew��de������de�����11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @B�\@}p�@��R@�p�@�G�AG�AG�A!G�A,(�AAG�A`  A~�RA�\)A��A��A��A�Q�A��A�  B   B  B  B(�B�
B'�B0  B8  B@  BG�
BO�
BW�
B`  Bh(�Bp  Bw�B�  B�{B��B��B�  B�  B��B��B�  B�{B�=qB�(�B�  B��
B�{B�{B�  B�  B�{B�{B�{B�{B�  B�  B�  B��B��B�  B�{B�(�B�  B��C   C��C  C
=C��C
  C{C
=C
=C{C{C
=C  C  C  C
=C   C!�C#��C&  C(  C*
=C,
=C.
=C0
=C2  C3��C5��C7��C:
=C<  C=��C@  CB  CD
=CF
=CH
=CJ
=CL  CM��CO��CQ��CT  CV
=CX
=CZ  C[�C]�C_��Ca��Cd  Cf
=Ch
=Ci��Cl
=Cn  Cp  Cr
=Ct
=Cv  Cx  Cz  C|  C~  C�  C�C�C�C�C�C�
=C�
=C�  C�  C���C���C�C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�C�C�  C�C�C���C���C���C�  C�C�C�  C�  C�  C�C�C�C�  C�  C�  C���C�  C�  C�C�C���C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�  C���C�C�  C���C�  C�  C�  C�  C�
=C�C�
=C�  C�  C�C�C�  C���C���C���C���C���C���C���C�  C���C���C�  C�C�C�  C�  C�C�C�C���C���C���C�  C�C�
=C�C���C���C���C���C���C���C���C�  C�  C�  C�  C�C���C�  C�  C�C���C���C���C���C�  C�  D   D � D  D� D�D��D  D� D  D� D  D� D�qD��D  Dz�D�qD}qD	  D	�D
�D
�D�D}qD�qDz�D�D��D  D��D�qD� D  D}qD�qD}qD�qD��D�D��D  D� D  D}qD  D��D�D� D�D� D��D}qD�qD��D  D� D  D}qD�qD� D�qD� D  D}qD�qD }qD �qD!��D"  D"��D#�D#��D#�qD$}qD%  D%��D&�D&��D'�D'��D'�qD(��D)  D)z�D)�qD*}qD*�qD+� D+�qD,}qD-�D-� D.  D.}qD/  D/� D0  D0��D1  D1}qD1��D2� D3�D3��D4  D4��D5D5}qD5�qD6��D7�D7��D8  D8}qD9  D9� D:�D:��D;�D;� D<�D<� D<��D=� D>  D>� D>�qD?}qD@  D@� DA  DA}qDB�DB��DB�qDC}qDC�qDD}qDE  DE}qDF�DF��DF�qDG� DH�DH�DI�DI� DJ  DJ��DK  DK� DL  DL� DL��DM� DN�DN}qDO  DO� DP�DP��DQDQ�DRDR�DS�DS��DTDT��DU�DU� DV�DV�DW�DW�DX  DX}qDY  DY� DZ  DZ��D[  D[}qD[�qD\z�D]  D]��D^  D^}qD_  D_}qD_��D`}qDa�Da� Da�qDb� Dc�Dc��Dd  Dd� De  De� Df  Df}qDg�Dg��Dg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDl  Dl� Dl��Dm}qDn  Dn� Do�Do� Dp  Dp�Dq�Dq}qDq�qDr}qDr�qDs��DtDt��Dt�qDu� Dv�Dv��Dw�Dw��Dx�Dx�Dy�Dy� Dz�Dz��D{  D{z�D{�qD|��D}  D}� D}��D~z�D  D� D�  D�AHD���D�D���D�@ D�� D�� D��D�AHD��HD�� D�  D�@ D�~�D�� D�HD�>�D�� D��HD�  D�@ D��HD��HD���D�>�D�~�D�� D�HD�>�D�~�D�� D��qD�=qD�~�D�� D���D�>�D�� D���D�HD�@ D�~�D��HD�  D�>�D�}qD��)D���D�@ D�~�D��qD�  D�>�D�� D��HD���D�@ D�� D��qD�  D�AHD�� D���D�HD�@ D�|)D��qD���D�@ D���D�� D���D�AHD�� D��qD���D�AHD���D�� D�HD�AHD�� D��qD���D�@ D�~�D���D�  D�=qD�~�D�� D�  D�B�D���D�� D�  D�@ D�}qD���D�  D�AHD�� D��qD���D�@ D�~�D��qD���D�@ D�� D�D�HD�>�D�� D��HD�  D�>�D�� D���D�  D�@ D�~�D��HD�HD�@ D�� D�� D���D�@ D��HD��HD�HD�>�D��HD�� D�  D�AHD��HD��HD�  D�@ D�� D��HD��D�@ D�� D��HD�  D�@ D��HD���D���D�AHD�~�D���D���D�>�D�� D�� D�HD�>�D�~�D�� D�  D�@ D�� D�� D���D�@ D���D�D�HD�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD���D��HD�HD�B�D���D�D�HD�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D���D�HD�@ D��HD��HD�  D�B�D�� D���D�  D�AHD�� D��HD�  D�>�D�}qD��qD���D�B�D��HD�� D�  D�AHD��HD���D���D�>�D�� D��HD���D�>�D�� D�D�HD�B�D���D���D�HD�=qD�}qD�� D�  D�@ D��HD��HD�HD�>�D�� D��HD�HD�AHD�� D�� D���D�>�D�� D��HD��D�B�D�~�D¾�D�HD�B�DÂ�D�� D�  D�B�DĂ�D��HD�HD�AHD�~�Dž�D���D�@ DƁHDƾ�D��qD�=qD�~�D��HD��D�AHD�~�D�� D�  D�>�D�~�D�� D�HD�@ DʁHD��HD�  D�AHDˀ D˾�D��qD�@ D̀ D̾�D�  D�>�D�~�D��HD�  D�@ D�~�Dξ�D�  D�@ DρHD��HD���D�>�DЂ�D�D�HD�@ D�~�D�� D�HD�AHDҀ DҾ�D���D�@ DӃ�D��HD���D�>�D�~�D�� D�  D�@ DՁHD��HD�HD�@ Dր D�� D�HD�@ D�~�D��HD�HD�AHD؀ Dؾ�D�HD�@ D�~�D�� D�HD�AHDځHD�D�HD�AHDۀ D�� D�  D�=qD�~�D�� D�  D�@ D݀ D�� D��qD�>�DށHD�D��D�@ D�}qD�� D�  D�@ D�� D�� D�  D�=qD� D��HD�HD�B�D₏D��HD���D�AHDわD�� D���D�AHD� D�� D�HD�B�D傏D�� D�  D�AHD悏D��HD���D�=qD�~�D羸D���D�@ D�~�D�� D�  D�@ D� D�� D���D�AHD� D�� D�  D�@ D�~�D뾸D���D�@ D�~�D쾸D�  D�@ D�~�D�� D�  D�@ D�HD��HD�HD�@ D� D��HD�HD�B�D��HD�� D���D�>�D�~�D�� D�  D�>�D�}qD�D�  D�AHD�~�D�D�  D�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D�� D��HD�  D�AHD��HD�D��q?��?L��?k�?���?\?�@�\@��@#�
@:�H@Tz�@aG�@u@�ff@�33@�(�@�ff@���@�(�@��
@У�@�p�@�ff@�\)@�(�A33A
=Ap�A�
A�A(�A#33A'�A,(�A1G�A7�A<(�AAG�AG
=AL(�AO\)ATz�AZ�HA^�RAb�\AhQ�An{Aq�AvffA|(�A\)A��A���A�\)A���A��A�ffA�Q�A�=qA�p�A�  A���A�z�A�\)A���A��A�ffA���A��HA�{A���A��HA��A�Q�A��HA���A�  A\A�z�A�
=Aə�A�z�A�{A�G�A��
A�p�A׮A��HA��A�
=AᙚA�z�A�ffA�Q�A��HA�p�A�
=A�G�A�(�A��RA�Q�A��HA�B (�B ��BffB�
B��B{B�B��B	��B
=Bz�B��B�RB(�B��BffB�BG�B�\B�B��B=qB\)BQ�B��B33B Q�B!G�B"�RB$(�B%�B&ffB'�
B)G�B*=qB+\)B-�B.ffB/�B0��B2ffB3�
B4��B6=qB7�
B8��B:=qB;�
B=�B>{B?�BA�BB{BC33BD��BFffBG\)BH��BJffBK\)BL��BNffBO\)BP��BRffBS�BTz�BV=qBW�BX��BY�B[\)B\��B]�B_33B`��Bb{Bc
=Bdz�Be�Bg
=BhQ�Bi�Bk
=Bl(�BmBo33Bp(�Bq��Bs33BtQ�BuG�Bv�HBxQ�ByG�Bz�\B|  B}p�B~ffB\)B�z�B�33B��B�Q�B�
=B���B�{B��HB��B�  B���B�p�B��B�z�B��B��B�Q�B��HB��B�Q�B���B�\)B�(�B��RB�33B�B�z�B�33B���B�(�B��HB�\)B��B���B�G�B�B�=qB�
=B��B�{B��RB�p�B�  B�z�B��B��B�Q�B��HB���B�Q�B��RB�G�B�{B��\B��B��
B�z�B���B��B�Q�B��HB�\)B�  B���B�\)B�B�z�B�33B�B�=qB���B��B�(�B��RB�p�B��B�z�B�G�B��B�Q�B�
=B��B�Q�B��RB�G�B�{B���B��B��
B�z�B�
=B��B�{B��HB��B�  B�z�B�33B��B�Q�B��HB��B�=qB��HB�\)B��B��\B�\)B�B�=qB�
=BîB�(�Bģ�B�\)B�  B�z�B���BǙ�B�Q�B��HB�\)B��
Bʣ�B�G�B��
B�=qB���BͮB�=qBθRB�33B��BУ�B�
=Bљ�B�=qB�
=BӮB�(�BԸRB�p�B�(�B֣�B��B��B؏\B��Bٙ�B�(�B��HBۅB�(�B�z�B�33B��
B�z�B��HB�p�B�{B���B�\)B�B�Q�B�
=B�B�=qB�RB�\)B�{B��B��B�B�z�B�
=B陚B�{B���B뙚B�  B�z�B�G�B�  B�z�B���BB�ffB��HB�\)B�  B�RB�p�B�{B�\B��B��
B��\B�33B��B�(�B���B��B�(�B��\B��B��B�z�B��HB�p�B�(�B���B�33B��C (�C �C C  C=qC��C�
C{CQ�C�C�C33CffC�RC
=C\)C�C��C�Cz�C�RC�C=qC�\C�
C{CQ�C��C�C=qCz�C�C	  C	\)C	�RC
  C
=qC
��C  CG�C�\C�
C=qC��C�
C�C�C�
C{C\)CC{CG�C�\C�
C33C�CC  CG�C��C�
C  C=qC�\CC�HC�CffC�C��C�C�C(�CG�C�\C�CC�HC{CG�CG�CffC��CC�C  C�CQ�Cp�C�C��C�
C��C
=C(�CffC�C��CC  C�C33C\)C�\C��CC��C(�C=qCQ�C�C�CC�C{C33CQ�Cp�C��C�
C�C  C33C\)Cz�C�\CC��C
=C�CQ�C�C��C�C�
C
=C(�C=qCffC��C��C��C  C
=C33Cp�Cz�C��C��C�C  C33CffCz�C�C�C�HC��C 
=C =qC p�C �\C �\C C ��C!
=C!�C!G�C!�C!��C!�C!�C"{C"(�C"Q�C"�\C"��C"C"��C#
=C#(�C#ffC#�C#��C#��C#��C$  C$33C$ffC$�C$��C$��C$��C%{C%33C%ffC%��C%�RC%��C%�C&(�C&Q�C&p�C&�C&C&��C'  C'(�C'ffC'�\C'��C'C(
=C((�C(=qC(p�C(��C(��C(�HC){C)G�C)Q�C)�C)�RC)�HC*  C*�C*\)C*z�C*�\C*��C*��C+  C+=qC+p�C+�\C+��C+��C,
=C,33C,G�C,ffC,��C,��C,�HC-
=C-G�C-ffC-�C-��C-��C.  C.33C.G�C.ffC.��C.C.�HC/{C/G�C/p�C/�\C/�C/�HC0�C0G�C0\)C0�C0��C0�C1
=C1=qC1z�C1��C1�RC1��C2(�C2=qC2p�C2��C2C2�HC3�C3Q�C3ffC3�\C3�RC4  C4
=C4=qC4z�C4��C4�RC4��C5�C533C5p�C5��C5�RC5�HC6{C6G�C6ffC6z�C6�C6�C7
=C7�C7\)C7�\C7��C7�
C8
=C8(�C8=qC8p�C8��C8C8�HC9�C9G�C9ffC9�C9�RC9�C:  C:{C:Q�C:�C:�C:��C:�HC;
=C;33C;ffC;�C;��C;��C<  C<
=C<=qC<p�C<�\C<��C<�HC={C=33C=G�C=z�C=�C=�
C=��C>{C>33C>\)C>��C>��C>��C?{C?33C?ffC?��C?C?�HC@  C@(�C@ffC@�\C@��C@��CA  CA=qCA\)CAz�CA��CA�
CB
=CB(�CBG�CBp�CB��CB�
CC
=CC=qCC\)CCz�CC��CC�HCD{CD=qCDQ�CD��CDCD�HCE  CE33CEp�CE��CECE�HCF
=CFQ�CFz�CF�\CFCG  CG�CG=qCGffCG��CG�
CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                         ?u@   @B�\@}p�@��R@�p�@�G�AG�AG�A!G�A,(�AAG�A`  A~�RA�\)A��A��A��A�Q�A��A�  B   B  B  B(�B�
B'�B0  B8  B@  BG�
BO�
BW�
B`  Bh(�Bp  Bw�B�  B�{B��B��B�  B�  B��B��B�  B�{B�=qB�(�B�  B��
B�{B�{B�  B�  B�{B�{B�{B�{B�  B�  B�  B��B��B�  B�{B�(�B�  B��C   C��C  C
=C��C
  C{C
=C
=C{C{C
=C  C  C  C
=C   C!�C#��C&  C(  C*
=C,
=C.
=C0
=C2  C3��C5��C7��C:
=C<  C=��C@  CB  CD
=CF
=CH
=CJ
=CL  CM��CO��CQ��CT  CV
=CX
=CZ  C[�C]�C_��Ca��Cd  Cf
=Ch
=Ci��Cl
=Cn  Cp  Cr
=Ct
=Cv  Cx  Cz  C|  C~  C�  C�C�C�C�C�C�
=C�
=C�  C�  C���C���C�C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�C�C�  C�C�C���C���C���C�  C�C�C�  C�  C�  C�C�C�C�  C�  C�  C���C�  C�  C�C�C���C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�  C���C�C�  C���C�  C�  C�  C�  C�
=C�C�
=C�  C�  C�C�C�  C���C���C���C���C���C���C���C�  C���C���C�  C�C�C�  C�  C�C�C�C���C���C���C�  C�C�
=C�C���C���C���C���C���C���C���C�  C�  C�  C�  C�C���C�  C�  C�C���C���C���C���C�  C�  D   D � D  D� D�D��D  D� D  D� D  D� D�qD��D  Dz�D�qD}qD	  D	�D
�D
�D�D}qD�qDz�D�D��D  D��D�qD� D  D}qD�qD}qD�qD��D�D��D  D� D  D}qD  D��D�D� D�D� D��D}qD�qD��D  D� D  D}qD�qD� D�qD� D  D}qD�qD }qD �qD!��D"  D"��D#�D#��D#�qD$}qD%  D%��D&�D&��D'�D'��D'�qD(��D)  D)z�D)�qD*}qD*�qD+� D+�qD,}qD-�D-� D.  D.}qD/  D/� D0  D0��D1  D1}qD1��D2� D3�D3��D4  D4��D5D5}qD5�qD6��D7�D7��D8  D8}qD9  D9� D:�D:��D;�D;� D<�D<� D<��D=� D>  D>� D>�qD?}qD@  D@� DA  DA}qDB�DB��DB�qDC}qDC�qDD}qDE  DE}qDF�DF��DF�qDG� DH�DH�DI�DI� DJ  DJ��DK  DK� DL  DL� DL��DM� DN�DN}qDO  DO� DP�DP��DQDQ�DRDR�DS�DS��DTDT��DU�DU� DV�DV�DW�DW�DX  DX}qDY  DY� DZ  DZ��D[  D[}qD[�qD\z�D]  D]��D^  D^}qD_  D_}qD_��D`}qDa�Da� Da�qDb� Dc�Dc��Dd  Dd� De  De� Df  Df}qDg�Dg��Dg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDl  Dl� Dl��Dm}qDn  Dn� Do�Do� Dp  Dp�Dq�Dq}qDq�qDr}qDr�qDs��DtDt��Dt�qDu� Dv�Dv��Dw�Dw��Dx�Dx�Dy�Dy� Dz�Dz��D{  D{z�D{�qD|��D}  D}� D}��D~z�D  D� D�  D�AHD���D�D���D�@ D�� D�� D��D�AHD��HD�� D�  D�@ D�~�D�� D�HD�>�D�� D��HD�  D�@ D��HD��HD���D�>�D�~�D�� D�HD�>�D�~�D�� D��qD�=qD�~�D�� D���D�>�D�� D���D�HD�@ D�~�D��HD�  D�>�D�}qD��)D���D�@ D�~�D��qD�  D�>�D�� D��HD���D�@ D�� D��qD�  D�AHD�� D���D�HD�@ D�|)D��qD���D�@ D���D�� D���D�AHD�� D��qD���D�AHD���D�� D�HD�AHD�� D��qD���D�@ D�~�D���D�  D�=qD�~�D�� D�  D�B�D���D�� D�  D�@ D�}qD���D�  D�AHD�� D��qD���D�@ D�~�D��qD���D�@ D�� D�D�HD�>�D�� D��HD�  D�>�D�� D���D�  D�@ D�~�D��HD�HD�@ D�� D�� D���D�@ D��HD��HD�HD�>�D��HD�� D�  D�AHD��HD��HD�  D�@ D�� D��HD��D�@ D�� D��HD�  D�@ D��HD���D���D�AHD�~�D���D���D�>�D�� D�� D�HD�>�D�~�D�� D�  D�@ D�� D�� D���D�@ D���D�D�HD�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD���D��HD�HD�B�D���D�D�HD�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D���D�HD�@ D��HD��HD�  D�B�D�� D���D�  D�AHD�� D��HD�  D�>�D�}qD��qD���D�B�D��HD�� D�  D�AHD��HD���D���D�>�D�� D��HD���D�>�D�� D�D�HD�B�D���D���D�HD�=qD�}qD�� D�  D�@ D��HD��HD�HD�>�D�� D��HD�HD�AHD�� D�� D���D�>�D�� D��HD��D�B�D�~�D¾�D�HD�B�DÂ�D�� D�  D�B�DĂ�D��HD�HD�AHD�~�Dž�D���D�@ DƁHDƾ�D��qD�=qD�~�D��HD��D�AHD�~�D�� D�  D�>�D�~�D�� D�HD�@ DʁHD��HD�  D�AHDˀ D˾�D��qD�@ D̀ D̾�D�  D�>�D�~�D��HD�  D�@ D�~�Dξ�D�  D�@ DρHD��HD���D�>�DЂ�D�D�HD�@ D�~�D�� D�HD�AHDҀ DҾ�D���D�@ DӃ�D��HD���D�>�D�~�D�� D�  D�@ DՁHD��HD�HD�@ Dր D�� D�HD�@ D�~�D��HD�HD�AHD؀ Dؾ�D�HD�@ D�~�D�� D�HD�AHDځHD�D�HD�AHDۀ D�� D�  D�=qD�~�D�� D�  D�@ D݀ D�� D��qD�>�DށHD�D��D�@ D�}qD�� D�  D�@ D�� D�� D�  D�=qD� D��HD�HD�B�D₏D��HD���D�AHDわD�� D���D�AHD� D�� D�HD�B�D傏D�� D�  D�AHD悏D��HD���D�=qD�~�D羸D���D�@ D�~�D�� D�  D�@ D� D�� D���D�AHD� D�� D�  D�@ D�~�D뾸D���D�@ D�~�D쾸D�  D�@ D�~�D�� D�  D�@ D�HD��HD�HD�@ D� D��HD�HD�B�D��HD�� D���D�>�D�~�D�� D�  D�>�D�}qD�D�  D�AHD�~�D�D�  D�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D�� D��HD�  D�AHD��HD�G�O�?��?L��?k�?���?\?�@�\@��@#�
@:�H@Tz�@aG�@u@�ff@�33@�(�@�ff@���@�(�@��
@У�@�p�@�ff@�\)@�(�A33A
=Ap�A�
A�A(�A#33A'�A,(�A1G�A7�A<(�AAG�AG
=AL(�AO\)ATz�AZ�HA^�RAb�\AhQ�An{Aq�AvffA|(�A\)A��A���A�\)A���A��A�ffA�Q�A�=qA�p�A�  A���A�z�A�\)A���A��A�ffA���A��HA�{A���A��HA��A�Q�A��HA���A�  A\A�z�A�
=Aə�A�z�A�{A�G�A��
A�p�A׮A��HA��A�
=AᙚA�z�A�ffA�Q�A��HA�p�A�
=A�G�A�(�A��RA�Q�A��HA�B (�B ��BffB�
B��B{B�B��B	��B
=Bz�B��B�RB(�B��BffB�BG�B�\B�B��B=qB\)BQ�B��B33B Q�B!G�B"�RB$(�B%�B&ffB'�
B)G�B*=qB+\)B-�B.ffB/�B0��B2ffB3�
B4��B6=qB7�
B8��B:=qB;�
B=�B>{B?�BA�BB{BC33BD��BFffBG\)BH��BJffBK\)BL��BNffBO\)BP��BRffBS�BTz�BV=qBW�BX��BY�B[\)B\��B]�B_33B`��Bb{Bc
=Bdz�Be�Bg
=BhQ�Bi�Bk
=Bl(�BmBo33Bp(�Bq��Bs33BtQ�BuG�Bv�HBxQ�ByG�Bz�\B|  B}p�B~ffB\)B�z�B�33B��B�Q�B�
=B���B�{B��HB��B�  B���B�p�B��B�z�B��B��B�Q�B��HB��B�Q�B���B�\)B�(�B��RB�33B�B�z�B�33B���B�(�B��HB�\)B��B���B�G�B�B�=qB�
=B��B�{B��RB�p�B�  B�z�B��B��B�Q�B��HB���B�Q�B��RB�G�B�{B��\B��B��
B�z�B���B��B�Q�B��HB�\)B�  B���B�\)B�B�z�B�33B�B�=qB���B��B�(�B��RB�p�B��B�z�B�G�B��B�Q�B�
=B��B�Q�B��RB�G�B�{B���B��B��
B�z�B�
=B��B�{B��HB��B�  B�z�B�33B��B�Q�B��HB��B�=qB��HB�\)B��B��\B�\)B�B�=qB�
=BîB�(�Bģ�B�\)B�  B�z�B���BǙ�B�Q�B��HB�\)B��
Bʣ�B�G�B��
B�=qB���BͮB�=qBθRB�33B��BУ�B�
=Bљ�B�=qB�
=BӮB�(�BԸRB�p�B�(�B֣�B��B��B؏\B��Bٙ�B�(�B��HBۅB�(�B�z�B�33B��
B�z�B��HB�p�B�{B���B�\)B�B�Q�B�
=B�B�=qB�RB�\)B�{B��B��B�B�z�B�
=B陚B�{B���B뙚B�  B�z�B�G�B�  B�z�B���BB�ffB��HB�\)B�  B�RB�p�B�{B�\B��B��
B��\B�33B��B�(�B���B��B�(�B��\B��B��B�z�B��HB�p�B�(�B���B�33B��C (�C �C C  C=qC��C�
C{CQ�C�C�C33CffC�RC
=C\)C�C��C�Cz�C�RC�C=qC�\C�
C{CQ�C��C�C=qCz�C�C	  C	\)C	�RC
  C
=qC
��C  CG�C�\C�
C=qC��C�
C�C�C�
C{C\)CC{CG�C�\C�
C33C�CC  CG�C��C�
C  C=qC�\CC�HC�CffC�C��C�C�C(�CG�C�\C�CC�HC{CG�CG�CffC��CC�C  C�CQ�Cp�C�C��C�
C��C
=C(�CffC�C��CC  C�C33C\)C�\C��CC��C(�C=qCQ�C�C�CC�C{C33CQ�Cp�C��C�
C�C  C33C\)Cz�C�\CC��C
=C�CQ�C�C��C�C�
C
=C(�C=qCffC��C��C��C  C
=C33Cp�Cz�C��C��C�C  C33CffCz�C�C�C�HC��C 
=C =qC p�C �\C �\C C ��C!
=C!�C!G�C!�C!��C!�C!�C"{C"(�C"Q�C"�\C"��C"C"��C#
=C#(�C#ffC#�C#��C#��C#��C$  C$33C$ffC$�C$��C$��C$��C%{C%33C%ffC%��C%�RC%��C%�C&(�C&Q�C&p�C&�C&C&��C'  C'(�C'ffC'�\C'��C'C(
=C((�C(=qC(p�C(��C(��C(�HC){C)G�C)Q�C)�C)�RC)�HC*  C*�C*\)C*z�C*�\C*��C*��C+  C+=qC+p�C+�\C+��C+��C,
=C,33C,G�C,ffC,��C,��C,�HC-
=C-G�C-ffC-�C-��C-��C.  C.33C.G�C.ffC.��C.C.�HC/{C/G�C/p�C/�\C/�C/�HC0�C0G�C0\)C0�C0��C0�C1
=C1=qC1z�C1��C1�RC1��C2(�C2=qC2p�C2��C2C2�HC3�C3Q�C3ffC3�\C3�RC4  C4
=C4=qC4z�C4��C4�RC4��C5�C533C5p�C5��C5�RC5�HC6{C6G�C6ffC6z�C6�C6�C7
=C7�C7\)C7�\C7��C7�
C8
=C8(�C8=qC8p�C8��C8C8�HC9�C9G�C9ffC9�C9�RC9�C:  C:{C:Q�C:�C:�C:��C:�HC;
=C;33C;ffC;�C;��C;��C<  C<
=C<=qC<p�C<�\C<��C<�HC={C=33C=G�C=z�C=�C=�
C=��C>{C>33C>\)C>��C>��C>��C?{C?33C?ffC?��C?C?�HC@  C@(�C@ffC@�\C@��C@��CA  CA=qCA\)CAz�CA��CA�
CB
=CB(�CBG�CBp�CB��CB�
CC
=CC=qCC\)CCz�CC��CC�HCD{CD=qCDQ�CD��CDCD�HCE  CE33CEp�CE��CECE�HCF
=CFQ�CFz�CF�\CFCG  CG�CG=qCGffCG��CG�
CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                         @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��;A��#A��TA��TA��HA��/A��HA��TA��mA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A���A���A���A���A���A���A���A���A���A���A���A�  A�1A�
=A�{A��A��A�5?A�S�A�`BA�dZA�jA�z�AуAщ7AэPAѣ�AѮAѰ!AѬAѴ9AѴ9AэPA�n�A�jA�ZA�+A���A�r�A�5?A�oA��A�p�A��/A��mAˏ\A�VAŲ-A�ƨA�r�A�&�A��yA��A���A��mA�M�A��yA�?}A��A���A���A�~�A��A�JA���A�A�+A�K�A���A�K�A�I�A�jA�I�A�1'A��+A��^A���A�
=A��A�ZA�ĜA��A��A���A�5?A��A�1A���A�x�A��HA��A��A�JA���A��uA�z�A��-A���A��A}/Ax �Av�!At��Ap�Am�Ak�AkG�AkoAhv�Ab��A]�#AX1'AU��AR��AM�AL�DAK�TAJ��AH�DAFv�AE��ADȴAB-A@�A;A8�RA7�A6I�A4A�A2A0I�A/`BA-�A*��A)�A)C�A(1'A%\)A!��A�TAv�Al�A�`AVA�A1A;dA|�A"�A�/A�A�\AA&�AS�A�yA�uAƨA^5A�-AVAffA��A
=A?}A��A�AI�AbA�Ax�A�A~�AM�A5?A5?Ar�A��A�jAVAt�A�
A-A9XA��AffAffA1A��A��A�7A;dA/A;dA+An�A-A�PA�AhsA
�A
v�A	�A	��A�`AVA  A;dA��AA�A�/A�DA�TA%A{A?}A Z@���@�33@�~�@���@�l�@��+@���@���@��@�=q@���@�O�@�C�@�!@�~�@�M�@���@�&�@�u@�1@��-@�Z@��@�l�@�;d@�!@�h@���@�+@��@�X@�z�@�Q�@�1@���@���@���@��
@㕁@�+@���@��@���@޸R@�J@ݲ-@݁@�%@ܛ�@܋D@�Z@��H@�v�@ٙ�@���@؋D@ؓu@�1@ם�@�dZ@�33@��H@�M�@Ցh@���@�C�@�bN@�l�@θR@�O�@�V@���@��`@��/@̬@�r�@�b@�"�@���@ʰ!@�ff@�@�G�@�I�@�33@ƸR@�~�@�p�@��/@ċD@�S�@¸R@§�@§�@��7@�(�@�;d@�n�@���@��h@�%@��9@�/@���@��`@�V@���@��`@�`B@�@���@�K�@�"�@��@��!@�^5@�J@��@�p�@���@��w@��@��y@���@�V@���@��u@�r�@�Z@�  @�l�@�;d@���@���@�`B@�V@��/@��D@�bN@�A�@�(�@��m@��@���@�M�@�V@�V@�M�@�X@���@��@���@��@�|�@�o@��R@�ff@�=q@���@�/@���@�Z@��@���@���@�S�@��@���@�v�@��@���@�/@��9@�I�@��@��;@��F@��@�K�@�o@��@���@�x�@�/@�%@��/@�z�@��@��@��
@��w@���@�l�@�+@��\@�=q@��^@�`B@�Ĝ@�j@��@�
=@��H@���@�ȴ@���@�V@���@�O�@��@���@��@�9X@���@�dZ@�+@���@�ff@��T@��^@��7@�G�@��@�Ĝ@��9@��@� �@���@�K�@��@���@�~�@�v�@�M�@�J@�@���@��7@��@�O�@�%@�Ĝ@�bN@�b@��
@�|�@�C�@��@��!@�ff@��@�hs@��@�j@��@�  @��
@��@�dZ@��@�ȴ@��\@�5?@�J@��@���@��h@�%@��;@��F@��@�S�@��y@���@�v�@�V@�5?@�{@��@���@���@���@��h@��@�O�@��@�Ĝ@��u@� �@�\)@��@�ȴ@���@�E�@��#@���@��7@�`B@�?}@��`@���@���@�A�@� �@�  @��
@���@�t�@�"�@��y@��@��\@�ff@�J@��@��T@�hs@���@��9@���@�r�@�A�@�b@�@K�@~�R@~��@~��@~��@~��@~ff@~$�@~$�@~{@}�@}@}�@}?}@|�@|Z@|Z@|�@{��@{t�@{33@z�!@y�#@x�9@w�;@w;d@v��@u�@u�h@u/@t�D@sƨ@s��@st�@s"�@r��@r=q@qx�@q7L@p�`@p�u@pA�@o|�@n�y@nȴ@n5?@m/@l��@l�@l1@k�m@kdZ@j�@j��@j~�@j-@i�@ihs@h��@hA�@h �@g�@g�P@gl�@g
=@fE�@e�@e�h@e/@d��@d�@dj@dI�@c��@ct�@cC�@cC�@b��@b=q@a�7@a�@`��@`��@`��@`�u@_��@^�R@^�+@^V@^5?@^5?@^{@]�@]@]�-@]��@]��@]�@]�@\(�@[ƨ@[��@[dZ@[C�@[@Z^5@Yx�@XĜ@W�;@WK�@V�R@VE�@V@U��@Up�@U�@T�j@Tj@T�@S��@SdZ@SC�@S33@R�@R~�@Q��@Qx�@Q7L@PĜ@O��@OK�@N�y@Nv�@NE�@N$�@M�T@M@M�h@Mp�@MO�@MV@L�/@Lz�@K�m@Kƨ@K��@K��@K��@K��@K�@KS�@K"�@J�!@J�!@J�\@Jn�@Jn�@JM�@JJ@I�^@I��@I��@Ihs@I�@H�9@Hr�@HQ�@G�;@Fv�@E@E�@EV@D��@D(�@C�
@C��@C"�@B��@B�!@B�\@Bn�@BM�@A��@A��@AG�@@�`@@��@@��@@��@@A�@?�@?l�@?K�@?K�@?K�@?;d@?
=@>�+@>v�@>E�@=�-@=/@<��@<9X@<1@;��@;�
@;C�@:�\@9�@9��@9x�@97L@8��@8��@8��@7l�@6�@6�+@6E�@6$�@5�@5��@5@5��@5�@4�/@4I�@3�m@3C�@2��@2n�@1�@1�7@1x�@1hs@17L@0�`@0bN@0 �@/l�@.�y@.�R@.�+@.$�@-�@-@-�@-�@,�/@,(�@+�
@+t�@+C�@*�@*�\@*J@)��@)&�@(��@(Ĝ@(��@(�@(bN@(A�@(  @'��@'l�@'K�@&ȴ@&��@&�+@&V@&$�@&@%�T@%@%�@%?}@$�@$��@$�@$j@$I�@$�@#��@#�m@#ƨ@#��@#S�@"�H@"M�@"�@"J@!��@!�@!�#@!�#@!�^@!X@!%@ r�@ bN@ b@��@�w@|�@+@
=@��@ȴ@�+@5?@{@@�@�-@�@�j@��@j@I�@I�@(�@�
@��@C�@o@o@o@�@�!@~�@n�@M�@�@�@�#@��@hs@&�@%@�`@�@1'@  @��@�P@\)@�y@ff@E�@5?@�@��@�h@?}@V@��@��@��@�/@�D@z�@j@j@j@Z@I�@(�@ƨ@t�@C�@�@��@��@�!@�!@n�@=q@�@�#@��@X@&�@��@Ĝ@�@r�@A�@1'@b@��@�P@;d@ȴ@�+@v�@E�@�T@��@?}@V@�/@�@�D@Z@9X@(�@1@�
@�F@�@dZ@C�@33@o@
��@
��@
n�@
�@	�#@	�7@	hs@	X@	X@	G�@	%@	%@	%@	%A�ƨA���A���A���A���A��/A��`A��HA���A���A��yA��`A��TA��HA��`A��HA��HA��TA��TA��/A��;A��HA��A��#A��TA��HA��/A��TA��`A��mA��`A��yA��yA��`A��mA��A��yA��mA��A��A��yA��A��A��A��yA��A��A��A��A��A��A��A��A��A��yA��A��A��yA��yA��A��A��yA��A��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A��A��A��A��A��A��A���A���A��A��A���A���A��A��A���A���A��A���A���A��A��A���A���A��A��A���A���A��A���A���A��A��A���A���A��A��A���A���A��A��A���A���A��A���A���A��A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�A�A�  A���A�  A�%A���A�  A�JA�A�%A�VA�JA�A�
=A�bA�1A�%A�1A�VA�JA�VA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�&�A�5?A�=qA�A�A�O�A�M�A�M�A�Q�A�S�A�Q�A�Q�A�XA�bNA�^5A�^5A�bNA�`BA�bNA�ffA�ffA�dZA�bNA�ffA�hsA�dZA�dZA�hsA�hsA�ffA�hsA�n�A�l�A�l�A�p�A�~�A�~�A�|�AуA�~�A�~�AхAхAсAуAч+Aщ7AхAхAыDAыDAщ7AэPAэPAэPAщ7AыDAя\Aя\AэPAёhAѝ�Aѩ�Aѩ�Aѩ�AѬAѰ!AѰ!AѬAѬAѰ!AѲ-AѮAѬAѲ-AѲ-AѰ!AѬAѮAѮAѬAѧ�AѬAѮAѮAѩ�Aѩ�AѲ-AѴ9AѴ9AѲ-AѶFAѺ^AѸRAѴ9AѲ-AѶFAѸRAѲ-AѰ!Aѩ�Aѧ�Aѡ�AсA�x�A�t�A�t�A�n�A�jA�n�A�p�A�l�A�jA�hsA�jA�l�A�l�A�hsA�hsA�jA�hsA�dZA�`BA�\)A�S�A�K�A�C�A�;dA�5?A�+A�"�A��A��A��A���A��#A���AиRAв-AХ�AЇ+A�z�A�|�A�t�A�l�A�ffA�`BA�7LA�1'A�5?A�=qA�7LA�1'A�$�A�5?A�{A�VA�1A�A�A�A���A���A��A��/A�ƨA�ĜAϾwA�XA�ZA�VA�K�A�S�A�1'A�1A���A��;A�Aκ^AΣ�A΅A�VA�33A���A��;AͲ-A�bNA�?}A��A�O�A˼jA�~�A�A�AʼjA�r�A�7LA��A���AɃA�"�AȸRA�`BA��A�C�AƗ�A���Aś�AĲ-A�(�A�t�A�x�A��uA�ffA��\A�M�A��mA��DA�Q�A��A���A��7A��^A��7A�dZA�E�A�oA��A��`A���A��A��A�bNA��A���A��RA�l�A�G�A�5?A��A�JA�A�%A�%A�  A�A�A�  A�  A�  A���A���A���A���A���A��A��A���A���A��A��A��A��A��A��mA��mA��A��TA��/A��#A���A���A�A�A�ƨA���A��+A��+A��+A�VA��A�$�A���A���A��jA���A�dZA�E�A�&�A�{A�bA�%A���A���A��mA���A��9A��\A��A�bNA��A���A��A�jA�XA�VA�K�A�9XA�(�A��A���A��A��yA��`A��TA��A���A���A�A���A��PA��A�t�A�n�A�jA�VA�C�A�;dA�/A�&�A�$�A� �A��A�VA�
=A�
=A�VA�VA�bA�
=A���A���A��uA�XA�+A�{A�1A��A��`A��;A��
A�ĜA�ȴA���A���A���A��A��A��7A��PA��+A��A�~�A��A��A�z�A��A�|�A�|�A�v�A�n�A�hsA�hsA�dZA�E�A�dZA�dZA�\)A�XA�/A�/A�
=A��FA��\A�l�A���A��RA���A�t�A�I�A�bA�A��A���A��9A���A��A�XA� �A�  A��`A��A�ĜA��-A��uA�~�A�|�A�v�A�l�A�ffA�S�A�C�A�C�A�=qA�$�A�A��A���A��DA�p�A�l�A�jA�S�A�K�A�O�A�O�A�O�A�M�A�I�A�E�A�$�A� �A��A�{A�{A�%A���A��yA��A��jA��A�M�A�+A�$�A��A��A��A�{A��A���A��A�`BA� �A�ȴA�^5A�=qA� �A�{A�%A���A��/A���A��RA���A��\A��7A�v�A�C�A�{A���A���A���A���A��A���A��+A�`BA�E�A�1'A�(�A�&�A��A��A�JA�  A��TA��FA��hA��A�v�A�p�A�jA�dZA�^5A�\)A�5?A�1A���A��yA��wA��hA�r�A�l�A�VA�G�A�=qA�5?A�&�A�bA��HA���A���A�^5A��A��A��RA��7A�VA��A��
A��A�dZA���A��A�~�A�VA�/A��A��A��9A��+A�=qA��A�  A��A�ȴA��A�-A��A�{A�  A��yA���A��!A��hA��7A��+A�~�A�I�A��A�ȴA��A�VA�(�A�A��`A���A��RA���A��uA�t�A��A��uA�`BA�E�A�5?A�/A��A���A���A��-A��\A�ffA�I�A�5?A�-A��A�A��A��A��wA���A��\A�x�A�ZA�1'A�%A��/A��^A���A�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                         A���A��;A��#A��TA��TA��HA��/A��HA��TA��mA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A���A���A���A���A���A���A���A���A���A���A���A�  A�1A�
=A�{A��A��A�5?A�S�A�`BA�dZA�jA�z�AуAщ7AэPAѣ�AѮAѰ!AѬAѴ9AѴ9AэPA�n�A�jA�ZA�+A���A�r�A�5?A�oA��A�p�A��/A��mAˏ\A�VAŲ-A�ƨA�r�A�&�A��yA��A���A��mA�M�A��yA�?}A��A���A���A�~�A��A�JA���A�A�+A�K�A���A�K�A�I�A�jA�I�A�1'A��+A��^A���A�
=A��A�ZA�ĜA��A��A���A�5?A��A�1A���A�x�A��HA��A��A�JA���A��uA�z�A��-A���A��A}/Ax �Av�!At��Ap�Am�Ak�AkG�AkoAhv�Ab��A]�#AX1'AU��AR��AM�AL�DAK�TAJ��AH�DAFv�AE��ADȴAB-A@�A;A8�RA7�A6I�A4A�A2A0I�A/`BA-�A*��A)�A)C�A(1'A%\)A!��A�TAv�Al�A�`AVA�A1A;dA|�A"�A�/A�A�\AA&�AS�A�yA�uAƨA^5A�-AVAffA��A
=A?}A��A�AI�AbA�Ax�A�A~�AM�A5?A5?Ar�A��A�jAVAt�A�
A-A9XA��AffAffA1A��A��A�7A;dA/A;dA+An�A-A�PA�AhsA
�A
v�A	�A	��A�`AVA  A;dA��AA�A�/A�DA�TA%A{A?}A Z@���@�33@�~�@���@�l�@��+@���@���@��@�=q@���@�O�@�C�@�!@�~�@�M�@���@�&�@�u@�1@��-@�Z@��@�l�@�;d@�!@�h@���@�+@��@�X@�z�@�Q�@�1@���@���@���@��
@㕁@�+@���@��@���@޸R@�J@ݲ-@݁@�%@ܛ�@܋D@�Z@��H@�v�@ٙ�@���@؋D@ؓu@�1@ם�@�dZ@�33@��H@�M�@Ցh@���@�C�@�bN@�l�@θR@�O�@�V@���@��`@��/@̬@�r�@�b@�"�@���@ʰ!@�ff@�@�G�@�I�@�33@ƸR@�~�@�p�@��/@ċD@�S�@¸R@§�@§�@��7@�(�@�;d@�n�@���@��h@�%@��9@�/@���@��`@�V@���@��`@�`B@�@���@�K�@�"�@��@��!@�^5@�J@��@�p�@���@��w@��@��y@���@�V@���@��u@�r�@�Z@�  @�l�@�;d@���@���@�`B@�V@��/@��D@�bN@�A�@�(�@��m@��@���@�M�@�V@�V@�M�@�X@���@��@���@��@�|�@�o@��R@�ff@�=q@���@�/@���@�Z@��@���@���@�S�@��@���@�v�@��@���@�/@��9@�I�@��@��;@��F@��@�K�@�o@��@���@�x�@�/@�%@��/@�z�@��@��@��
@��w@���@�l�@�+@��\@�=q@��^@�`B@�Ĝ@�j@��@�
=@��H@���@�ȴ@���@�V@���@�O�@��@���@��@�9X@���@�dZ@�+@���@�ff@��T@��^@��7@�G�@��@�Ĝ@��9@��@� �@���@�K�@��@���@�~�@�v�@�M�@�J@�@���@��7@��@�O�@�%@�Ĝ@�bN@�b@��
@�|�@�C�@��@��!@�ff@��@�hs@��@�j@��@�  @��
@��@�dZ@��@�ȴ@��\@�5?@�J@��@���@��h@�%@��;@��F@��@�S�@��y@���@�v�@�V@�5?@�{@��@���@���@���@��h@��@�O�@��@�Ĝ@��u@� �@�\)@��@�ȴ@���@�E�@��#@���@��7@�`B@�?}@��`@���@���@�A�@� �@�  @��
@���@�t�@�"�@��y@��@��\@�ff@�J@��@��T@�hs@���@��9@���@�r�@�A�@�b@�@K�@~�R@~��@~��@~��@~��@~ff@~$�@~$�@~{@}�@}@}�@}?}@|�@|Z@|Z@|�@{��@{t�@{33@z�!@y�#@x�9@w�;@w;d@v��@u�@u�h@u/@t�D@sƨ@s��@st�@s"�@r��@r=q@qx�@q7L@p�`@p�u@pA�@o|�@n�y@nȴ@n5?@m/@l��@l�@l1@k�m@kdZ@j�@j��@j~�@j-@i�@ihs@h��@hA�@h �@g�@g�P@gl�@g
=@fE�@e�@e�h@e/@d��@d�@dj@dI�@c��@ct�@cC�@cC�@b��@b=q@a�7@a�@`��@`��@`��@`�u@_��@^�R@^�+@^V@^5?@^5?@^{@]�@]@]�-@]��@]��@]�@]�@\(�@[ƨ@[��@[dZ@[C�@[@Z^5@Yx�@XĜ@W�;@WK�@V�R@VE�@V@U��@Up�@U�@T�j@Tj@T�@S��@SdZ@SC�@S33@R�@R~�@Q��@Qx�@Q7L@PĜ@O��@OK�@N�y@Nv�@NE�@N$�@M�T@M@M�h@Mp�@MO�@MV@L�/@Lz�@K�m@Kƨ@K��@K��@K��@K��@K�@KS�@K"�@J�!@J�!@J�\@Jn�@Jn�@JM�@JJ@I�^@I��@I��@Ihs@I�@H�9@Hr�@HQ�@G�;@Fv�@E@E�@EV@D��@D(�@C�
@C��@C"�@B��@B�!@B�\@Bn�@BM�@A��@A��@AG�@@�`@@��@@��@@��@@A�@?�@?l�@?K�@?K�@?K�@?;d@?
=@>�+@>v�@>E�@=�-@=/@<��@<9X@<1@;��@;�
@;C�@:�\@9�@9��@9x�@97L@8��@8��@8��@7l�@6�@6�+@6E�@6$�@5�@5��@5@5��@5�@4�/@4I�@3�m@3C�@2��@2n�@1�@1�7@1x�@1hs@17L@0�`@0bN@0 �@/l�@.�y@.�R@.�+@.$�@-�@-@-�@-�@,�/@,(�@+�
@+t�@+C�@*�@*�\@*J@)��@)&�@(��@(Ĝ@(��@(�@(bN@(A�@(  @'��@'l�@'K�@&ȴ@&��@&�+@&V@&$�@&@%�T@%@%�@%?}@$�@$��@$�@$j@$I�@$�@#��@#�m@#ƨ@#��@#S�@"�H@"M�@"�@"J@!��@!�@!�#@!�#@!�^@!X@!%@ r�@ bN@ b@��@�w@|�@+@
=@��@ȴ@�+@5?@{@@�@�-@�@�j@��@j@I�@I�@(�@�
@��@C�@o@o@o@�@�!@~�@n�@M�@�@�@�#@��@hs@&�@%@�`@�@1'@  @��@�P@\)@�y@ff@E�@5?@�@��@�h@?}@V@��@��@��@�/@�D@z�@j@j@j@Z@I�@(�@ƨ@t�@C�@�@��@��@�!@�!@n�@=q@�@�#@��@X@&�@��@Ĝ@�@r�@A�@1'@b@��@�P@;d@ȴ@�+@v�@E�@�T@��@?}@V@�/@�@�D@Z@9X@(�@1@�
@�F@�@dZ@C�@33@o@
��@
��@
n�@
�@	�#@	�7@	hs@	X@	X@	G�@	%@	%@	%G�O�A�ƨA���A���A���A���A��/A��`A��HA���A���A��yA��`A��TA��HA��`A��HA��HA��TA��TA��/A��;A��HA��A��#A��TA��HA��/A��TA��`A��mA��`A��yA��yA��`A��mA��A��yA��mA��A��A��yA��A��A��A��yA��A��A��A��A��A��A��A��A��A��yA��A��A��yA��yA��A��A��yA��A��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A��A��A��A��A��A��A���A���A��A��A���A���A��A��A���A���A��A���A���A��A��A���A���A��A��A���A���A��A���A���A��A��A���A���A��A��A���A���A��A��A���A���A��A���A���A��A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�A�A�  A���A�  A�%A���A�  A�JA�A�%A�VA�JA�A�
=A�bA�1A�%A�1A�VA�JA�VA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�&�A�5?A�=qA�A�A�O�A�M�A�M�A�Q�A�S�A�Q�A�Q�A�XA�bNA�^5A�^5A�bNA�`BA�bNA�ffA�ffA�dZA�bNA�ffA�hsA�dZA�dZA�hsA�hsA�ffA�hsA�n�A�l�A�l�A�p�A�~�A�~�A�|�AуA�~�A�~�AхAхAсAуAч+Aщ7AхAхAыDAыDAщ7AэPAэPAэPAщ7AыDAя\Aя\AэPAёhAѝ�Aѩ�Aѩ�Aѩ�AѬAѰ!AѰ!AѬAѬAѰ!AѲ-AѮAѬAѲ-AѲ-AѰ!AѬAѮAѮAѬAѧ�AѬAѮAѮAѩ�Aѩ�AѲ-AѴ9AѴ9AѲ-AѶFAѺ^AѸRAѴ9AѲ-AѶFAѸRAѲ-AѰ!Aѩ�Aѧ�Aѡ�AсA�x�A�t�A�t�A�n�A�jA�n�A�p�A�l�A�jA�hsA�jA�l�A�l�A�hsA�hsA�jA�hsA�dZA�`BA�\)A�S�A�K�A�C�A�;dA�5?A�+A�"�A��A��A��A���A��#A���AиRAв-AХ�AЇ+A�z�A�|�A�t�A�l�A�ffA�`BA�7LA�1'A�5?A�=qA�7LA�1'A�$�A�5?A�{A�VA�1A�A�A�A���A���A��A��/A�ƨA�ĜAϾwA�XA�ZA�VA�K�A�S�A�1'A�1A���A��;A�Aκ^AΣ�A΅A�VA�33A���A��;AͲ-A�bNA�?}A��A�O�A˼jA�~�A�A�AʼjA�r�A�7LA��A���AɃA�"�AȸRA�`BA��A�C�AƗ�A���Aś�AĲ-A�(�A�t�A�x�A��uA�ffA��\A�M�A��mA��DA�Q�A��A���A��7A��^A��7A�dZA�E�A�oA��A��`A���A��A��A�bNA��A���A��RA�l�A�G�A�5?A��A�JA�A�%A�%A�  A�A�A�  A�  A�  A���A���A���A���A���A��A��A���A���A��A��A��A��A��A��mA��mA��A��TA��/A��#A���A���A�A�A�ƨA���A��+A��+A��+A�VA��A�$�A���A���A��jA���A�dZA�E�A�&�A�{A�bA�%A���A���A��mA���A��9A��\A��A�bNA��A���A��A�jA�XA�VA�K�A�9XA�(�A��A���A��A��yA��`A��TA��A���A���A�A���A��PA��A�t�A�n�A�jA�VA�C�A�;dA�/A�&�A�$�A� �A��A�VA�
=A�
=A�VA�VA�bA�
=A���A���A��uA�XA�+A�{A�1A��A��`A��;A��
A�ĜA�ȴA���A���A���A��A��A��7A��PA��+A��A�~�A��A��A�z�A��A�|�A�|�A�v�A�n�A�hsA�hsA�dZA�E�A�dZA�dZA�\)A�XA�/A�/A�
=A��FA��\A�l�A���A��RA���A�t�A�I�A�bA�A��A���A��9A���A��A�XA� �A�  A��`A��A�ĜA��-A��uA�~�A�|�A�v�A�l�A�ffA�S�A�C�A�C�A�=qA�$�A�A��A���A��DA�p�A�l�A�jA�S�A�K�A�O�A�O�A�O�A�M�A�I�A�E�A�$�A� �A��A�{A�{A�%A���A��yA��A��jA��A�M�A�+A�$�A��A��A��A�{A��A���A��A�`BA� �A�ȴA�^5A�=qA� �A�{A�%A���A��/A���A��RA���A��\A��7A�v�A�C�A�{A���A���A���A���A��A���A��+A�`BA�E�A�1'A�(�A�&�A��A��A�JA�  A��TA��FA��hA��A�v�A�p�A�jA�dZA�^5A�\)A�5?A�1A���A��yA��wA��hA�r�A�l�A�VA�G�A�=qA�5?A�&�A�bA��HA���A���A�^5A��A��A��RA��7A�VA��A��
A��A�dZA���A��A�~�A�VA�/A��A��A��9A��+A�=qA��A�  A��A�ȴA��A�-A��A�{A�  A��yA���A��!A��hA��7A��+A�~�A�I�A��A�ȴA��A�VA�(�A�A��`A���A��RA���A��uA�t�A��A��uA�`BA�E�A�5?A�/A��A���A���A��-A��\A�ffA�I�A�5?A�-A��A�A��A��A��wA���A��\A�x�A�ZA�1'A�%A��/A��^A���A�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B(XB'�B(�B)*B(�B(�B(�B(�B(�B(�B)�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B)�B)�B)_B)_B)�B(�B)�B)�B(�B)*B)*B)*B)*B(�B)�B*0B,B-CB0�B2�B2�B<�BJ�BP�BR�BS�BY�B^B`BBb�BncBu�By�B~�B��B�eB��B�wB�OB��B��B�mB��B��B�aB��B��B�B��B�9B� B�sB�B�BMB \B8�B:*B:^B>�BHBK�BJ#BM�BJXBB�BB�B<6B7�B7�B9�BF�BJ�BG�BAUB?�B1�B=B�B�;B��B�mB��B��B��B�~B��Bq�BC-B0UB)�B"4BIB_B�B
��B
�iB
�B
یB
�B
��B
{JB
iB
_�B
@OB
3�B
&�B
.B	�fB	�KB	�`B	��B	ԕB	��B	��B	zxB	j�B	g8B	MjB	G�B	C�B	?B	>BB	4nB	1[B	,=B	(�B	'�B	%B	�B	7B	CB	�B	 �B	"4B	!-B	#:B	 �B	�B	qB	�B	�B��B�B�`B�2B�B��B	�B	�B	�B	xB	�B	B	�B	�B	uB	�B	-�B	2�B	2�B	0!B	*0B	%FB	"�B	$tB	�B	$@B	'B	)_B	*�B	*eB	,B	-�B	,�B	1�B	33B	7�B	@�B	H�B	U�B	_pB	e`B	qvB	~]B	�4B	��B	�'B	��B	ƨB	�mB	�?B	�yB	�yB	��B	�KB	�B	��B	�B	ܒB	�WB	چB	چB	�B	��B	�dB	�5B	�B	�B	�;B	�B	��B	�B	�B	�QB	�9B	�,B	ѷB	уB	��B	ѷB	��B	�}B	уB	ѷB	��B	�&B	҉B	ԕB	��B	��B	��B	��B	�]B	چB	�QB	��B	�B	��B	רB	��B	�#B	�WB	�)B	ܒB	��B	�B	ޞB	�;B	�B	�vB	��B	��B	�`B	�B	��B	��B	�B	��B	�8B	�B	�B	�KB	�iB	�iB	�5B	�B	�B	��B	� B	�/B	�5B	�B	��B	��B	��B	�fB	�2B	�>B	��B	��B	��B	�B	�2B	�lB	�B	��B	��B	��B	�B	�2B	�B	�
B	�B	�sB	�yB	��B	��B	�B	�B	�;B	�oB	��B	�B	�)B	��B	�B	��B	�B	�B	�B	��B	�>B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	� B	�B	��B	��B	�B	��B
uB
�B

rB
oB
B
�B
�B
@B
B
oB
uB
B
MB
�B
�B
{B
{B
SB
YB
SB
SB
$B
�B
�B
�B
1B
�B
eB
�B
�B
�B
�B
�B
7B
�B
B
IB
�B
�B
CB
�B
~B
B
�B
�B
�B
�B
 'B
 'B
 'B
"hB
"hB
#nB
$tB
$tB
%B
%B
%�B
&LB
&B
&�B
&�B
($B
(�B
)�B
)�B
*0B
+B
+B
,B
,�B
-CB
0!B
/B
.�B
.�B
.B
.IB
.B
.�B
.IB
.�B
.�B
.�B
.�B
/B
/�B
0!B
0UB
0�B
1�B
1[B
49B
3�B
3�B
3�B
3�B
4B
4nB
6B
6zB
6�B
7�B
7�B
8�B
9�B
:�B
:�B
:�B
;�B
<�B
<�B
<�B
=�B
>�B
>B
>BB
>wB
@OB
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
CaB
CaB
C�B
DgB
D�B
E9B
FB
F?B
F�B
G�B
G�B
HB
HB
H�B
IB
I�B
JXB
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M6B
M�B
M�B
M6B
MjB
M6B
OBB
OB
N�B
OBB
OBB
PB
P}B
P�B
P�B
P�B
P�B
QNB
QB
QB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
R B
S&B
R B
R�B
R�B
S�B
T,B
S�B
S�B
S�B
S�B
T�B
TaB
U2B
U�B
U2B
U�B
UgB
VB
U�B
VmB
VB
V�B
VmB
V�B
W�B
WsB
X�B
Z�B
\]B
\]B
\]B
\�B
\�B
\�B
]/B
]�B
]�B
]dB
]�B
\�B
]/B
]�B
]�B
]dB
]�B
]�B
]�B
]dB
^B
^�B
_;B
_;B
_pB
`B
_�B
_�B
_�B
`B
a|B
aHB
b�B
c B
c B
cTB
c�B
dZB
d&B
d�B
dZB
d�B
dZB
e�B
e�B
e�B
ffB
ffB
ffB
g�B
g8B
gB
g�B
h�B
iB
iyB
iDB
iDB
i�B
jB
jB
jB
jKB
jB
j�B
k�B
k�B
k�B
k�B
lWB
k�B
l�B
m]B
m]B
m�B
m�B
m�B
n�B
n�B
n�B
o B
pB
pB
o�B
poB
p�B
p�B
p�B
p�B
p�B
p�B
poB
qB
qAB
qvB
q�B
q�B
q�B
rB
rB
rGB
rGB
r|B
rGB
rGB
r�B
tTB
t�B
t�B
t�B
t�B
t�B
tB
s�B
tTB
t�B
uZB
u�B
v`B
v�B
v`B
v�B
v�B
w�B
x8B
x�B
yrB
yrB
yrB
zB
zDB
z�B
{B
{B
{B
{�B
|PB
|�B
|�B
}"B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
cB
�B
cB
�B
�B
�B
�B
�B
� B
� B
� B
�4B
�iB
��B
��B
�4B
�B
� B
�4B
�B
�B
�;B
��B
�oB
�B
��B
��B
��B
�B
��B
�B
�GB
��B
��B
�GB
�GB
��B
�B
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
�SB
��B
��B
�YB
��B
�YB
��B
�+B
��B
�lB
��B
�	B
�rB
�rB
�	B
�=B
��B
�DB
�xB
�xB
�xB
��B
�xB
�xB
��B
��B
�~B
��B
��B
��B
�"B
�VB
��B
��B
��B
��B
�VB
�(B
�(B
��B
�.B
��B
�bB
��B
�4B
�4B
�4B
��B
�B
�:B
�@B
��B
�uB
�uB
��B
�B
�{B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�YB
�YB
�YB
�+B
�+B
�+B
�_B
�_B
��B
��B
��B
��B
�eB
��B
�eB
��B
��B
�B
�7B
�B
�7B
�7B
�kB
�kB
��B
�qB
�qB
�qB
��B
��B
��B
��B
��B
�CB
�CB
�B
��B
�~B
��B
��B
�B
�B
�B
�B
��B
��B
�VB
�!B
�VB
�VB
�VB
��B
�'B
�'B
�\B
��B
��B
��B
��B
�bB
��B
��B
��B
��B
��B
��B
�hB
�hB
��B
�B
�:B
�:B
�B
��B
��B
��B
��B
�tB
��B
��B
��B
�zB
�FB
�B
��B
�LB
��B
��B
��B
��B
�RB
��B
��B
��B
��B
��B
��B
�XB
��B
�$B
�$B
�XB
�$B
�XB
��B
�*B
�_B
��B
��B
��B
��B
��B
�0B
�0B
�eB
��B
��B
�kB
�kB
��B
��B
�=B
�=B
��B
��B
��B
�CB
�wB
��B
�B
�B
�IB
�}B
��B
��B
�OB
�B
�OB
��B
��B
�!B
�!B
�!B
�UB
��B
��B
�'B
�'B
��B
��B
�[B
��B
��B
��B
�aB
�aB
��B
��B
��B
��B
��B
�hB
�hB
�hB
�hB%FB&�B%�B(XB'�B*eB%B'�B)�B*�B'RB(�B)�B(�B'B(�B)�B)_B&�B*�B(�B(�B)�B(�B&�B(�B)�B)*B(XB'�B(�B'�B'�B)�B)_B'�B(�B)�B(XB'�B)�B)*B'�B)*B*0B(�B'�B(�B)*B'�B)_B)�B(�B(�B)�B)_B'�B)_B)�B(XB'�B)�B(�B'�B)�B)�B'�B(XB)�B)*B'�B)�B)�B'�B(�B*0B($B'�B)_B)�B($B($B)�B(�B'�B)*B)�B'�B'�B)�B*0B'�B(�B*0B'�B'�B)�B)_B'�B($B(�B)�B'�B(XB*0B(�B(XB)�B*eB(�B(�B*eB)�B(�B)�B*�B)_B(�B*eB*eB(�B(�B*0B*eB(XB)�B*eB*0B($B(XB*0B(�B'�B*�B*0B)�B(�B*�B*0B(�B(�B*0B)�B(XB)*B*�B(�B(XB)�B*0B(�B)_B*�B)_B(�B)�B*�B(�B(XB)�B(�B(XB)�B(�B(XB*�B*�B($B(�B)�B($B'�B(�B*0B)_B'�B)�B)�B(�B(XB)�B(XB(XB*0B*0B(XB)_B)�B(�B'�B)�B)_B'�B(�B*0B(�B($B(�B*�B)�B)�B*�B*eB,=B(XB*eB*eB($B.}B+�B+B.�B.}B.B/B-CB,B/B*�B/�B1'B/�B0!B2�B3�B1[B2-B2�B3�B2-B1�B33B4B1�B3�B3�B1�B1�B49B4nB5�B2�B@OBC�BB[BH�BJ�BJ�BI�BK�BMBJ�BN�BP�BQ�BN<BN�BR�BR�BQ�BR�BS[BR BQ�BR�BS�BQ�BRTBS�BT,BS&BUgBVmBUgBWsBW�B]�B[WB]dB^B\�B]�B_;B_B]�B]�B`BB`�B^�BaHBb�B`�B`�Ba|Bc Bb�Bb�Bb�Bd&BffBe�Bn�BrBtBv+Bt�Bu%Bv�Bv�Bu�BuZBw2Bx�Bx8By	Bz�B|B{BzDB{B|�B|�B|�BcB�{B��B��B��B��B�B��B�oB�SB��B�1B��B�+B��B�IB��B��B�-B�B�B�qB��B�wB�}B�CB��B�wB��B�OB��B��B�B��B�!B��B��B��B��B�B�dB��B�B�UB� B��B�UBB��B� B�BȴB�BƨB�[B�9BɆB�gB��B�-B�9B�tB�gB�B�EBÖB�[B�B�3B��B�BȴB�[B�-B��BB� B� B��BB�9B�B�B�B��B�wB��B��B�<B�aB��B�jB��B��B�$B��B��B�dB��B�HB�3B�}B�UB�'B�B��B�gB��B��B�XB��B��B��B��B��B͟B�wB��B͟B��B��B�6B��B��B �B�B�B��B�B�rB�	B��B�cB��B �BB4�B�B�B_B�B+B�B�B.B BbB4B�B/�B;0B5?B5B4�B9$B8�B9�B8�B8�B:�B9�B8�B:*B:^B:*B9XB:�B;0B9XB:*B:�B;dB9�B9XB:�B:�B:^B9$B9XB;0B:�B9$B;0B:^B;0B:^B:�B:�B9�B:*B>wB=�B9�B9�BDgB>B=<B@�BAUB@�BF?BE�BF�BI�BFtBC-BFBF?BE9BF�BHKBK�BL0BG�BM�BZ�BU2BK)BL�BK�BH�BJ#BJXBJ�BJXBK�BG�BIBH�BGBGzBG�BF�BH�BMBI�BI�BK�BJ�BI�BM6BL0BJ�BI�BK)BHKBG�BHBI�BIBH�BJXBM6BOBNpBP�BX�BS�BW�BTaBM6BNpBN�BH�BH�BF?BIRBAUBMjBB�BFtBE9BB'BB[B@�BB[BB[BD3BAUBB�BD3BB�BA�B@�BB�BC-B@�B?�BB[BHKB:�B:�BB�BA BFB:�BK�BGzBB�B=<BG�B>wB9�B8RB@OBAUB7LB9�B;�B9�B9$B;0B=�B=�B:*B:�B6�B8�B7LB9�B7LB4nB6B6B5�B6zB8�B33B6FB5�B8�B5�B9�B;�B7B7B6�B9�B9�B8�B8RB7LB8B7�B:^B9XB9$B?�B:�B9�B:^B>B=<B<6B?�BG�BL�BF?BD�BF?BG�BF�BF?BXyBTaBNpBN<BP�BVmBQNBDgBFtBD�BCaBCaBF�BF�BGEBJXBE�BFtBGzBOvBK)BGzBHKBD3BB�BE9BA�BC�BA�BAUBA B>�B=�B>�BAUBA�BA�BD3BH�BEmB?�B@�B?HB=<B<�B=qB<�B@�B=B9�B:�B:^B:�B6FB2�B3hB0�B0UB/B/B,qB.�B&�B-B(�B#B�B�B�BB7B�BB�B_B�B�B	�B+B
�BB	7B+B	�B �B��B�B�VB�B��B�B��B��B�;B�B�;B�B��B�
B��B�B�B�DB�B�yB�yB��B��B�TB�&B��B� B�B�B��BچB�B՛B�&BӏB��B�&B�BB��B�6B̘BɺB�?B�zB�tB�9B�9B��B�B��B��B�<B�qB��B��B�?B�-B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021121618245720211216182457IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021122623325620211226233256QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021122623325620211226233256QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365620220126093656IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                