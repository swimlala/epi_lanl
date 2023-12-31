CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-09-25T23:12:23Z creation; 2022-02-04T23:30:04Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  c(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 1   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � W(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � ^�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` }H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   }�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210925231223  20220204223517  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_183                 6810_008521_183                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٖ9	V�@ٖ9	V�11  @ٖ9IQ��@ٖ9IQ��@0wk;�<�@0wk;�<��d�q�i�C�d�q�i�C11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @@  @}p�@�p�@�  @�  AG�A�RA ��A+�A@  Aa�A���A�  A��A��A��AϮA�Q�A�  B   BQ�BQ�BQ�B   B'�B/�B7�B@  BH  BP(�BXz�B`z�Bh(�Bo�
Bw�
B�{B�  B�{B�(�B�  B��B�  B�  B�{B�{B�{B�=qB�{B�  B�{B�  B��B�{B�  B�  B�  B��B��B�{B�  B�  B�{B�  B�  B��B��B��C 
=C
=C  C  C
=C

=C
=C
=C
=C
=C
=C��C  C  C  C
=C {C!��C#��C%�C(
=C*{C,
=C.  C/��C1��C4
=C5��C7��C:  C<{C>  C@
=CA��CC��CF
=CG��CJ  CK��CN  CP  CR
=CT
=CV  CW��CY��C[��C]��C`  Cb  Cc��Cf  Cg��Ci�Cl  Cn
=Cp
=Cr  Ct  Cu��Cx  Cz
=C|
=C~
=C�C�  C���C���C���C�  C�C�C�
=C�  C���C�  C���C�C�  C�
=C�  C�  C���C�  C�  C���C���C���C���C�C�  C�C�
=C�  C�  C���C�C�
=C�  C��C���C���C���C���C�
=C�
=C�  C�  C�  C�  C�  C�C�  C��C�  C�C�
=C�  C���C���C�C�C�C�  C�  C���C���C���C���C�C�C�C�C���C�  C�C�
=C�  C���C���C���C�  C�  C���C���C�  C�C���C��C���C�  C���C���C���C�  C�C�  C���C���C���C���C���C���C�  C�
=C�
=C�
=C�  C���C�  C�C�  C�
=C�
=C���C���C���C���C�  C�  C���C���C���C���C���C�  C�
=C�  C���C�C�  C���D   D ��D�D� D�qD� D  D�D  D� D�D}qD�qD}qD�qD� D  D� D��D	}qD
�D
��D�D� D�qD� D  D� D  D� D�qDz�D�qD� D�D}qD  D� D�qD� D  D��D�D}qD�qD� D  D� D  Dz�D�qD� D  D��D  D��D�qD� DD� D��D��D�D��D �D � D �qD!}qD!�qD"� D#  D#� D$  D$� D$�qD%� D%�qD&� D'  D'��D(  D(}qD)  D)� D)�qD*� D+�D+��D,  D,��D-  D-� D.  D.�D/�D/��D0  D0��D1  D1}qD1�qD2}qD2�qD3}qD3�qD4� D4�qD5}qD6�D6��D7  D7}qD7�qD8}qD8�qD9}qD9��D:z�D:�qD;� D<  D<� D<�qD=}qD>  D>� D>�qD?� D@  D@� DA  DA}qDA��DB� DCDC� DD  DD��DEDE� DE��DF� DF�qDG}qDH�DH��DI  DI}qDJ�DJ� DK�DK�DL�DL� DL�qDM� DN  DN}qDO  DO� DO�qDPz�DP�RDQxRDQ�qDRz�DR��DS� DS�qDT� DU  DU}qDV�DV� DW  DW��DXDX��DY  DY� DY�qDZz�DZ��D[}qD[�qD\� D]�D]��D^  D^� D_  D_��D`�D`}qDa  Da�Db�Db}qDc�Dc��Dd�Dd�De  Dez�De�qDf� Dg  Dg}qDg�qDh� Dh�qDiz�Di��Dj}qDk  Dk��Dl  Dlz�Dm  Dm� Dn  Dn�Do  Do}qDo�qDp��Dq�Dq��Dr�Dr}qDs  Ds�DtDt� Dt�qDu��DvDv��Dw  Dw� Dx�Dx��Dy  Dy}qDy�qDz� D{D{��D{�qD|� D}�D}}qD~  D~� D~��Dz�D�  D�AHD��HD�� D�  D�>�D�~�D��HD�  D�=qD��HD�� D���D�AHD��HD��HD�HD�>�D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D���D�>�D�� D��HD�HD�>�D�� D��HD�  D�=qD�� D��HD�  D�AHD�� D�� D�HD�AHD��HD�� D�  D�AHD�~�D��qD���D�>�D�~�D�� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�>�D�� D���D��qD�=qD�~�D���D���D�@ D��HD��HD���D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D���D�  D�@ D�~�D�� D�HD�=qD�}qD���D�  D�>�D�}qD���D�HD�@ D�}qD�� D�HD�@ D�� D�� D���D�@ D�� D�� D�HD�@ D�}qD���D�  D�@ D��HD��HD�HD�B�D���D�D��D�AHD�� D��qD��qD�@ D�� D�� D���D�@ D��HD�� D�HD�C�D���D�� D���D�@ D�� D���D���D�@ D��HD�D��D�>�D�}qD���D�  D�AHD�� D���D�  D�@ D�~�D���D���D�>�D��HD��HD�  D�AHD��HD���D�  D�@ D��HD�D���D�>�D�� D���D�  D�AHD��HD�� D���D�AHD���D�� D�  D�AHD��HD�D�  D�@ D���D�D�  D�@ D��HD�� D�  D�@ D�� D�� D�HD�AHD�� D��HD�HD�@ D��HD�� D��qD�=qD�~�D���D���D�@ D��HD�� D��qD�>�D�� D�� D���D�@ D��HD�D��D�@ D�~�D���D���D�>�D�� D�� D�  D�>�D�� D�� D���D�>�D��HD��HD�  D�>�D�~�D�� D�HD�AHD��HD��HD�  D�>�D�}qD��qD���D�@ D�� D���D�HD�C�D���D���D���D�B�D�D��HD���D�@ DÀ D��HD�HD�@ DĀ D��HD�  D�@ D�~�D�� D��D�B�DƁHDƾ�D���D�@ Dǀ D�� D���D�>�DȀ D�� D���D�@ Dɂ�D��HD���D�@ Dʀ D�� D�HD�>�Dˀ D��HD�HD�@ D�}qD�� D�  D�@ D̀ D;�D�  D�AHD΂�D��HD�  D�@ DρHD�� D��qD�>�DЀ D�� D���D�>�DсHD��HD�HD�>�DҀ D�� D�  D�@ DӀ DӾ�D��qD�>�D�~�DԾ�D���D�>�DՀ D�� D�  D�=qDր D�D�HD�AHDׁHD�� D�  D�>�D�}qDؽqD�  D�AHDفHD��HD�HD�@ Dڀ Dھ�D�  D�AHDہHD��HD�  D�>�D�}qD�� D�HD�@ D݁HD�� D�  D�@ D�~�D޾�D�  D�@ D�~�D��HD�HD�=qD�~�D��HD�HD�>�D�}qD�� D�  D�>�D�~�D�� D���D�>�D� D��HD�HD�@ D�~�D�� D�HD�B�D� D徸D���D�>�D�~�D�� D�HD�@ D�~�D�qD���D�AHD�HD�� D��qD�>�D�HD��HD���D�>�D� D꾸D�  D�AHD�~�D�� D��D�AHD�HD��HD�  D�@ D� D��HD�HD�AHD�HD�� D�HD�>�D� D�D�HD�@ D�~�D�� D�  D�@ D�~�D�D�  D�@ D�s3?\)?aG�?���?\?��H@\)@#�
@:�H@Q�@aG�@}p�@���@�@�  @��@���@�ff@У�@��H@���@�
=AG�AffA(�A�A��A\)A$z�A)��A0��A7�A>{AC33AH��AO\)AUA]p�Adz�Aj=qAp  Au�A|(�A���A���A�\)A��\A�{A�G�A��
A��RA��\A�A�Q�A��HA�ffA���A��A�\)A�=qA�p�A���A��
A��RA���A��Aȣ�A��HA�A�G�A���A�\)A��A�A��A�33A�{A陚A���A�A��A���A�Q�A�33A�B (�B��B33B��B�B\)B	�B
�\B�
B�B�RBz�B�B33Bz�B{B�
BG�B�\B�
B��B33B z�B!B#
=B$��B&ffB'�B(��B*ffB,  B-B.�HB0(�B1B3�B5�B6=qB7�B9�B:�HB<(�B=p�B>�RB@Q�BB{BC33BDz�BEBG33BH��BJffBK�BL��BN=qBP  BQp�BR�RBS�
BUp�BW
=BX��BY�B[
=B\z�B^{B_�B`��Bb{Bc\)Bd��BfffBg�Bh��Bj{Bk�BmG�BnffBo�Bp��Br�RBt(�BuG�Bv�\Bx(�By�B{\)B|��B}B�B�z�B�G�B��B�z�B��B�B��\B�33B��
B�Q�B��HB���B�=qB���B�
=B��B�=qB���B��B��B�{B��\B���B�G�B���B�(�B���B���B�G�B��B�(�B���B��HB�33B��B�=qB��\B���B�G�B��B�=qB��\B���B�G�B��
B�(�B�z�B���B�p�B��
B�=qB�ffB�
=B�p�B��
B�{B�ffB���B�p�B��B�  B�z�B���B�p�B��
B�{B�z�B�
=B��B�B�(�B���B�G�B��B��B�Q�B��HB�\)B�B�  B�ffB���B�p�B��B�  B�z�B�
=B�p�B��B�{B���B��B�p�B�B�(�B��RB�33B���B�B�=qB���B�33B��B��
B�=qB���B��B��B��
B�{B��RB��B��B��
B�(�B���B��B���B��
B�(�B��\B��B���B��B�=qB���B��B���B�B�(�B���B��B��B��
B�(�B��\B�
=B��B��
B�(�B�z�B���B�p�B��B�Q�B�z�B���B�p�B��B�Q�B���B���B�\)B��B�ffB���B�33BÙ�B��B�=qBĸRB�G�Bř�B��B�=qBƸRB��BǙ�B��B�Q�Bȏ\B��HB�33BɮB�{Bʏ\B���B�G�B˙�B�{B̏\B���B�\)B�B�{B�z�B��HB�\)B��
B�Q�BУ�B���B�\)B��
B�Q�BҸRB�33BӅB��
B�(�BԸRB��BծB�{B�ffB���B�33Bי�B�(�BظRB�33Bٙ�B��B�Q�BڸRB�G�BۮB�=qBܣ�B��HB�G�BݮB�{Bޣ�B�33Bߙ�B��B�=qB�RB��B�B�{B�ffB���B�33B㙚B�(�B��B�
=B�\)B�B�(�B��B��B�B��
B�(�B�\B�
=B�B�  B�Q�B�RB�
=B�p�B�B�=qB�RB�33B홚B�  B�Q�B��B�
=B�B�{B��\B���B�\)B�B�{B�\B�33B�B�  B�Q�B��RB�G�B��
B�Q�B��RB�
=B�p�B��B��\B�
=B��B��
B�=qB���B�
=B���B�(�B���B���B�\)B��
B�ffB��HB�G�B�C 
=C =qC �\C ��C  C33CffC��C�C(�C\)C�\C�RC  CG�C�C�RC�HC�C\)C��C�C(�CQ�C�\C��C�CffC��C�HC{CG�C�\C�
C�CffC��C�
C	
=C	Q�C	��C	�
C
�C
Q�C
�\C
�RC  C=qC�\C�
C{CQ�C�\C��C
=CQ�C��C�HC�CQ�C�\C�
C�Cp�C�C�HC(�CffC��C�HC33Cz�CC
=C=qCp�C�RC  CG�C�\C�
C{C=qCz�CC
=CQ�C��C�
C{CQ�C��C�HC(�Cz�CC
=CQ�C��C��C  CG�C�\C�HC(�Cp�C�C�HC�CffC�C  CQ�C�\C��C
=CQ�C��C��C=qCz�C�RC��C=qC�\C�HC (�C p�C ��C ��C!33C!z�C!�RC"
=C"Q�C"��C"��C#=qC#p�C#�RC#��C$G�C$��C$�HC%33C%p�C%�C%�C&(�C&p�C&�RC'  C'Q�C'�C'��C(33C(z�C(�RC(��C)Q�C)��C)�C*33C*p�C*�C*�HC+(�C+z�C+C,{C,\)C,��C,�
C-�C-Q�C-��C-�HC.33C.z�C.�
C/{C/Q�C/��C/�
C0�C0p�C0�RC1  C1G�C1�\C1��C2{C2G�C2�C2��C3
=C3Q�C3��C3�HC4(�C4z�C4�RC5  C533C5p�C5��C5�C6(�C6�C6��C7
=C7Q�C7�\C7C8{C8ffC8�C9  C9=qC9z�C9�RC9��C:=qC:�C:��C;{C;ffC;�C;��C<=qC<z�C<C=  C==qC=p�C=C>
=C>Q�C>��C>�C?33C?z�C?�RC?��C@=qC@z�C@�RC@��CA33CAp�CA�RCB  CB=qCBz�CBCC  CCG�CC�\CC�
CD�CDffCD��CD�CE33CE�CE��CF{CF\)CF��CF��CG{CGQ�CG�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                    ?u@   @@  @}p�@�p�@�  @�  AG�A�RA ��A+�A@  Aa�A���A�  A��A��A��AϮA�Q�A�  B   BQ�BQ�BQ�B   B'�B/�B7�B@  BH  BP(�BXz�B`z�Bh(�Bo�
Bw�
B�{B�  B�{B�(�B�  B��B�  B�  B�{B�{B�{B�=qB�{B�  B�{B�  B��B�{B�  B�  B�  B��B��B�{B�  B�  B�{B�  B�  B��B��B��C 
=C
=C  C  C
=C

=C
=C
=C
=C
=C
=C��C  C  C  C
=C {C!��C#��C%�C(
=C*{C,
=C.  C/��C1��C4
=C5��C7��C:  C<{C>  C@
=CA��CC��CF
=CG��CJ  CK��CN  CP  CR
=CT
=CV  CW��CY��C[��C]��C`  Cb  Cc��Cf  Cg��Ci�Cl  Cn
=Cp
=Cr  Ct  Cu��Cx  Cz
=C|
=C~
=C�C�  C���C���C���C�  C�C�C�
=C�  C���C�  C���C�C�  C�
=C�  C�  C���C�  C�  C���C���C���C���C�C�  C�C�
=C�  C�  C���C�C�
=C�  C��C���C���C���C���C�
=C�
=C�  C�  C�  C�  C�  C�C�  C��C�  C�C�
=C�  C���C���C�C�C�C�  C�  C���C���C���C���C�C�C�C�C���C�  C�C�
=C�  C���C���C���C�  C�  C���C���C�  C�C���C��C���C�  C���C���C���C�  C�C�  C���C���C���C���C���C���C�  C�
=C�
=C�
=C�  C���C�  C�C�  C�
=C�
=C���C���C���C���C�  C�  C���C���C���C���C���C�  C�
=C�  C���C�C�  C���D   D ��D�D� D�qD� D  D�D  D� D�D}qD�qD}qD�qD� D  D� D��D	}qD
�D
��D�D� D�qD� D  D� D  D� D�qDz�D�qD� D�D}qD  D� D�qD� D  D��D�D}qD�qD� D  D� D  Dz�D�qD� D  D��D  D��D�qD� DD� D��D��D�D��D �D � D �qD!}qD!�qD"� D#  D#� D$  D$� D$�qD%� D%�qD&� D'  D'��D(  D(}qD)  D)� D)�qD*� D+�D+��D,  D,��D-  D-� D.  D.�D/�D/��D0  D0��D1  D1}qD1�qD2}qD2�qD3}qD3�qD4� D4�qD5}qD6�D6��D7  D7}qD7�qD8}qD8�qD9}qD9��D:z�D:�qD;� D<  D<� D<�qD=}qD>  D>� D>�qD?� D@  D@� DA  DA}qDA��DB� DCDC� DD  DD��DEDE� DE��DF� DF�qDG}qDH�DH��DI  DI}qDJ�DJ� DK�DK�DL�DL� DL�qDM� DN  DN}qDO  DO� DO�qDPz�DP�RDQxRDQ�qDRz�DR��DS� DS�qDT� DU  DU}qDV�DV� DW  DW��DXDX��DY  DY� DY�qDZz�DZ��D[}qD[�qD\� D]�D]��D^  D^� D_  D_��D`�D`}qDa  Da�Db�Db}qDc�Dc��Dd�Dd�De  Dez�De�qDf� Dg  Dg}qDg�qDh� Dh�qDiz�Di��Dj}qDk  Dk��Dl  Dlz�Dm  Dm� Dn  Dn�Do  Do}qDo�qDp��Dq�Dq��Dr�Dr}qDs  Ds�DtDt� Dt�qDu��DvDv��Dw  Dw� Dx�Dx��Dy  Dy}qDy�qDz� D{D{��D{�qD|� D}�D}}qD~  D~� D~��Dz�D�  D�AHD��HD�� D�  D�>�D�~�D��HD�  D�=qD��HD�� D���D�AHD��HD��HD�HD�>�D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D���D�>�D�� D��HD�HD�>�D�� D��HD�  D�=qD�� D��HD�  D�AHD�� D�� D�HD�AHD��HD�� D�  D�AHD�~�D��qD���D�>�D�~�D�� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�>�D�� D���D��qD�=qD�~�D���D���D�@ D��HD��HD���D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D���D�  D�@ D�~�D�� D�HD�=qD�}qD���D�  D�>�D�}qD���D�HD�@ D�}qD�� D�HD�@ D�� D�� D���D�@ D�� D�� D�HD�@ D�}qD���D�  D�@ D��HD��HD�HD�B�D���D�D��D�AHD�� D��qD��qD�@ D�� D�� D���D�@ D��HD�� D�HD�C�D���D�� D���D�@ D�� D���D���D�@ D��HD�D��D�>�D�}qD���D�  D�AHD�� D���D�  D�@ D�~�D���D���D�>�D��HD��HD�  D�AHD��HD���D�  D�@ D��HD�D���D�>�D�� D���D�  D�AHD��HD�� D���D�AHD���D�� D�  D�AHD��HD�D�  D�@ D���D�D�  D�@ D��HD�� D�  D�@ D�� D�� D�HD�AHD�� D��HD�HD�@ D��HD�� D��qD�=qD�~�D���D���D�@ D��HD�� D��qD�>�D�� D�� D���D�@ D��HD�D��D�@ D�~�D���D���D�>�D�� D�� D�  D�>�D�� D�� D���D�>�D��HD��HD�  D�>�D�~�D�� D�HD�AHD��HD��HD�  D�>�D�}qD��qD���D�@ D�� D���D�HD�C�D���D���D���D�B�D�D��HD���D�@ DÀ D��HD�HD�@ DĀ D��HD�  D�@ D�~�D�� D��D�B�DƁHDƾ�D���D�@ Dǀ D�� D���D�>�DȀ D�� D���D�@ Dɂ�D��HD���D�@ Dʀ D�� D�HD�>�Dˀ D��HD�HD�@ D�}qD�� D�  D�@ D̀ D;�D�  D�AHD΂�D��HD�  D�@ DρHD�� D��qD�>�DЀ D�� D���D�>�DсHD��HD�HD�>�DҀ D�� D�  D�@ DӀ DӾ�D��qD�>�D�~�DԾ�D���D�>�DՀ D�� D�  D�=qDր D�D�HD�AHDׁHD�� D�  D�>�D�}qDؽqD�  D�AHDفHD��HD�HD�@ Dڀ Dھ�D�  D�AHDہHD��HD�  D�>�D�}qD�� D�HD�@ D݁HD�� D�  D�@ D�~�D޾�D�  D�@ D�~�D��HD�HD�=qD�~�D��HD�HD�>�D�}qD�� D�  D�>�D�~�D�� D���D�>�D� D��HD�HD�@ D�~�D�� D�HD�B�D� D徸D���D�>�D�~�D�� D�HD�@ D�~�D�qD���D�AHD�HD�� D��qD�>�D�HD��HD���D�>�D� D꾸D�  D�AHD�~�D�� D��D�AHD�HD��HD�  D�@ D� D��HD�HD�AHD�HD�� D�HD�>�D� D�D�HD�@ D�~�D�� D�  D�@ D�~�D�D�  D�@ G�O�?\)?aG�?���?\?��H@\)@#�
@:�H@Q�@aG�@}p�@���@�@�  @��@���@�ff@У�@��H@���@�
=AG�AffA(�A�A��A\)A$z�A)��A0��A7�A>{AC33AH��AO\)AUA]p�Adz�Aj=qAp  Au�A|(�A���A���A�\)A��\A�{A�G�A��
A��RA��\A�A�Q�A��HA�ffA���A��A�\)A�=qA�p�A���A��
A��RA���A��Aȣ�A��HA�A�G�A���A�\)A��A�A��A�33A�{A陚A���A�A��A���A�Q�A�33A�B (�B��B33B��B�B\)B	�B
�\B�
B�B�RBz�B�B33Bz�B{B�
BG�B�\B�
B��B33B z�B!B#
=B$��B&ffB'�B(��B*ffB,  B-B.�HB0(�B1B3�B5�B6=qB7�B9�B:�HB<(�B=p�B>�RB@Q�BB{BC33BDz�BEBG33BH��BJffBK�BL��BN=qBP  BQp�BR�RBS�
BUp�BW
=BX��BY�B[
=B\z�B^{B_�B`��Bb{Bc\)Bd��BfffBg�Bh��Bj{Bk�BmG�BnffBo�Bp��Br�RBt(�BuG�Bv�\Bx(�By�B{\)B|��B}B�B�z�B�G�B��B�z�B��B�B��\B�33B��
B�Q�B��HB���B�=qB���B�
=B��B�=qB���B��B��B�{B��\B���B�G�B���B�(�B���B���B�G�B��B�(�B���B��HB�33B��B�=qB��\B���B�G�B��B�=qB��\B���B�G�B��
B�(�B�z�B���B�p�B��
B�=qB�ffB�
=B�p�B��
B�{B�ffB���B�p�B��B�  B�z�B���B�p�B��
B�{B�z�B�
=B��B�B�(�B���B�G�B��B��B�Q�B��HB�\)B�B�  B�ffB���B�p�B��B�  B�z�B�
=B�p�B��B�{B���B��B�p�B�B�(�B��RB�33B���B�B�=qB���B�33B��B��
B�=qB���B��B��B��
B�{B��RB��B��B��
B�(�B���B��B���B��
B�(�B��\B��B���B��B�=qB���B��B���B�B�(�B���B��B��B��
B�(�B��\B�
=B��B��
B�(�B�z�B���B�p�B��B�Q�B�z�B���B�p�B��B�Q�B���B���B�\)B��B�ffB���B�33BÙ�B��B�=qBĸRB�G�Bř�B��B�=qBƸRB��BǙ�B��B�Q�Bȏ\B��HB�33BɮB�{Bʏ\B���B�G�B˙�B�{B̏\B���B�\)B�B�{B�z�B��HB�\)B��
B�Q�BУ�B���B�\)B��
B�Q�BҸRB�33BӅB��
B�(�BԸRB��BծB�{B�ffB���B�33Bי�B�(�BظRB�33Bٙ�B��B�Q�BڸRB�G�BۮB�=qBܣ�B��HB�G�BݮB�{Bޣ�B�33Bߙ�B��B�=qB�RB��B�B�{B�ffB���B�33B㙚B�(�B��B�
=B�\)B�B�(�B��B��B�B��
B�(�B�\B�
=B�B�  B�Q�B�RB�
=B�p�B�B�=qB�RB�33B홚B�  B�Q�B��B�
=B�B�{B��\B���B�\)B�B�{B�\B�33B�B�  B�Q�B��RB�G�B��
B�Q�B��RB�
=B�p�B��B��\B�
=B��B��
B�=qB���B�
=B���B�(�B���B���B�\)B��
B�ffB��HB�G�B�C 
=C =qC �\C ��C  C33CffC��C�C(�C\)C�\C�RC  CG�C�C�RC�HC�C\)C��C�C(�CQ�C�\C��C�CffC��C�HC{CG�C�\C�
C�CffC��C�
C	
=C	Q�C	��C	�
C
�C
Q�C
�\C
�RC  C=qC�\C�
C{CQ�C�\C��C
=CQ�C��C�HC�CQ�C�\C�
C�Cp�C�C�HC(�CffC��C�HC33Cz�CC
=C=qCp�C�RC  CG�C�\C�
C{C=qCz�CC
=CQ�C��C�
C{CQ�C��C�HC(�Cz�CC
=CQ�C��C��C  CG�C�\C�HC(�Cp�C�C�HC�CffC�C  CQ�C�\C��C
=CQ�C��C��C=qCz�C�RC��C=qC�\C�HC (�C p�C ��C ��C!33C!z�C!�RC"
=C"Q�C"��C"��C#=qC#p�C#�RC#��C$G�C$��C$�HC%33C%p�C%�C%�C&(�C&p�C&�RC'  C'Q�C'�C'��C(33C(z�C(�RC(��C)Q�C)��C)�C*33C*p�C*�C*�HC+(�C+z�C+C,{C,\)C,��C,�
C-�C-Q�C-��C-�HC.33C.z�C.�
C/{C/Q�C/��C/�
C0�C0p�C0�RC1  C1G�C1�\C1��C2{C2G�C2�C2��C3
=C3Q�C3��C3�HC4(�C4z�C4�RC5  C533C5p�C5��C5�C6(�C6�C6��C7
=C7Q�C7�\C7C8{C8ffC8�C9  C9=qC9z�C9�RC9��C:=qC:�C:��C;{C;ffC;�C;��C<=qC<z�C<C=  C==qC=p�C=C>
=C>Q�C>��C>�C?33C?z�C?�RC?��C@=qC@z�C@�RC@��CA33CAp�CA�RCB  CB=qCBz�CBCC  CCG�CC�\CC�
CD�CDffCD��CD�CE33CE�CE��CF{CF\)CF��CF��CG{CGQ�CG�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                    @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�hsA�n�A�^5A�dZA�n�A�E�A�1'A�+A�/A�+A�/A�9XA�7LA�7LA�7LA�7LA�5?A�5?A�5?A�7LA�33A�5?A�33A�33A�33A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�9XA�7LA�5?A�7LA�7LA�5?A�/A�VA��`AۼjA�AھwA�x�A�oAٗ�A�l�Aש�A�l�A��A��mA�jA��mA��A���A�A���A�z�A�jA�Q�A���A�jA�p�A�|�AƑhAļjA��TA���A�r�A��wA�33A���A��`A��9A���A�hsA��yA�A��A��#A��/A��RA�S�A��A���A��A��RA��A��A�ĜA���A�
=A��A�?}A�(�A�7LA�dZA�bNA���A�dZA�ffA�VA��+A�?}A��wA���A���A�  A�z�A��FA�XA�  A{�Av��At�\As/Aq�ApbNAe�hAcO�A^ZAZI�AY��AY+AW33AU�AUAS��AR�!ARAP{AO7LAM�;AKl�AJbNAG��AGO�AE��ADjAB�RAA��A?�^A<�A;XA9hsA5K�A1ƨA0�uA/l�A.$�A-��A-C�A-
=A,��A,$�A+��A*��A*�uA*^5A+&�A+�A*�A)ƨA)�-A)x�A(�uA'�A'��A'G�A%��A$r�A$1A#�mA#�PA#%A"n�A!dZA!&�A!�A!p�A ��At�A��A��Ap�AȴAbNA$�Av�AbNA�A�RA$�A7LA�A�A��A��A�AbAQ�A�A�AƨAdZA��A�FAoA��A�FAS�AC�AK�A�;@���@�V@��@�j@���@�;d@�\)@�+@��@�hs@�G�@���@�S�@�-@��@� �@�  @�^5@�E�@��/@��@�P@�@��@�r�@�(�@�o@���@�j@��`@睲@�`B@�+@��H@��@�9@���@���@�S�@�^5@�x�@�Ĝ@�A�@��@�Z@߅@�@ܴ9@ݙ�@��@܃@�ƨ@�C�@���@�v�@�M�@�$�@ى7@�/@�x�@���@���@�7L@�V@�V@��@���@���@��/@ج@أ�@أ�@�Z@ץ�@�"�@�@�^5@�%@�z�@�I�@�1'@�A�@�r�@�9X@�b@�l�@���@�ȴ@җ�@�v�@�{@���@�z�@�I�@�9X@�1'@ϥ�@θR@�^5@�J@́@�&�@���@�z�@�bN@�Q�@�I�@�b@���@˅@��@ʰ!@�n�@�{@��#@ɡ�@Ɂ@�X@�7L@ȼj@��@���@��H@�n�@��@�`B@��/@Ĭ@�  @Ý�@�|�@�33@���@�n�@�J@��-@�?}@��u@�1'@�33@�ȴ@��+@���@���@�X@��@�r�@��P@��@�33@��+@�@�?}@�Ĝ@��u@�z�@�I�@�ƨ@�l�@�+@�"�@��H@�V@�J@��#@�hs@��@��j@�Z@� �@��@�K�@�
=@���@���@�`B@��@��/@���@��u@�1'@��
@���@��P@�S�@�"�@�
=@���@�^5@��^@��@���@�r�@��;@�K�@�
=@�o@�ȴ@���@���@��!@���@���@���@�n�@�5?@��T@��@���@��@��u@� �@�;d@�V@�=q@�$�@��^@��9@���@���@�@�M�@���@��@��@��j@�Q�@�1@�ƨ@���@�|�@�"�@��@��R@�n�@�M�@�@���@��@�%@���@�I�@�b@���@���@��P@�S�@��@���@�M�@�{@���@�?}@���@���@�j@�9X@��@��@�\)@�33@���@�v�@��@��^@�?}@���@��@��9@���@��D@�z�@�j@�9X@���@��@�S�@�o@��H@��!@��+@�ff@��@��T@���@���@��7@�O�@�&�@��`@��u@�Q�@�9X@���@��w@��@�;d@���@�E�@�J@���@�@��7@�`B@�?}@�V@�Ĝ@��D@�Z@�1'@���@��
@��w@�t�@�S�@�@���@�ȴ@��!@�^5@�E�@�-@��-@�G�@��@��`@�Ĝ@��@���@�r�@�A�@��@�+@��H@��\@�=q@�@�@��#@�X@�V@��@���@���@�z�@�A�@�(�@�1@���@��w@�t�@�33@�o@�@��@�ȴ@��R@�v�@�M�@�-@�@���@��7@�p�@�O�@�/@��@��j@�@�w@��@\)@
=@~E�@|�/@|I�@|(�@|1@{ƨ@{�@{S�@z�!@y��@yX@x�u@x�`@x�9@x��@y�@y&�@y7L@y7L@xĜ@x�@x �@v�y@vff@vV@v5?@u�@u��@uO�@t��@t(�@s�F@s�@s"�@r��@r=q@rJ@q�@q�7@q%@p��@o�w@o|�@o��@o��@o+@n��@nV@n{@mO�@l�D@k�F@k"�@j�@j��@j^5@j=q@i�@i7L@h��@hQ�@h  @g�w@g;d@f�y@f�R@f�+@fV@e�@e`B@d�@d�@c�F@b�H@b�@a�@a��@a��@a%@`Ĝ@`r�@_�;@_�@_l�@_+@^��@^�@^�+@^E�@^5?@]�@]�h@]�@]/@\�/@\(�@\�@[��@[��@[�m@[ƨ@[��@Z��@Z-@Y�@Y��@Y��@Y��@Yhs@Y%@XĜ@XA�@Xb@W�;@V��@V��@Vv�@Vff@VE�@V{@U@U@U�-@U�-@Up�@U�@Tz�@S�@So@S@R�!@Q�@QG�@Q%@P��@O�@O��@O�P@O|�@O;d@Nv�@M�-@M�@M/@L9X@K�
@K�
@KdZ@J�H@J��@JM�@JJ@JJ@I�@I��@IG�@HĜ@H�@G�@Gl�@G;d@F��@Fȴ@F��@Fff@F$�@E�h@D��@D�@D�D@D(�@C�
@C�@C"�@B�!@B�@A�@A��@A��@Ahs@A&�@@��@@Ĝ@@r�@@1'@@  @?|�@?;d@?
=@>��@>5?@=�h@<�/@<�@<��@<Z@;ƨ@;S�@;o@;@:��@:n�@:=q@9�#@9��@9x�@8�`@8�9@8��@8�@81'@8 �@8b@8b@7�;@7|�@7;d@7
=@7
=@6�y@6�@6ȴ@65?@5�-@5�h@5p�@5?}@4��@4z�@4j@4(�@3��@3��@3�m@3�F@3�@3t�@3"�@2�@2�@2��@2~�@2-@1��@1G�@1&�@0��@/�@/+@.�R@.v�@.ff@.V@-O�@,�j@,��@+ƨ@+C�@*��@*^5@*-@)�7@)&�@(r�@( �@'�@'�w@'|�@'+@&��@&ȴ@&��@&v�@%�@%@%�h@%p�@%`B@%O�@%?}@%V@$��@$Z@#ƨ@#��@#o@"��@!��@!��@!�@ Q�@�@�@�;@��@�P@�P@��@�P@�P@|�@�P@K�@+@�@�+@5?@ff@@��@p�@O�@?}@��@��@z�@I�@9X@��@��@�m@�F@��@dZ@C�@33@@��@~�@n�@n�@^5@^5@M�@-@�@��@�7@G�@�@�`@��@Q�@1'@1'@1'@1'@1'@1'@ �@ �@ �@ �@ �@�;@  @  @+@��@v�@V@5?@5?@$�@$�@�@��@�@O�@��@�@�@�@��@j@�@ƨ@�@C�@o@o@o@�@��@�!A�n�A�ffA�dZA�l�A�ffA�dZA�n�A�l�A�n�A�`BA�^5A�`BA�`BA�hsA�hsA�n�A�p�A�hsA�=qA�1'A�7LA�1'A�/A�+A�+A�-A�1'A�/A�+A�+A�/A�33A�1'A�+A�/A�5?A�=qA�;dA�;dA�33A�-A�33A�9XA�;dA�9XA�9XA�7LA�7LA�7LA�5?A�7LA�;dA�7LA�5?A�7LA�9XA�9XA�7LA�5?A�7LA�9XA�5?A�1'A�33A�7LA�7LA�33A�1'A�5?A�9XA�5?A�33A�7LA�7LA�5?A�33A�7LA�9XA�7LA�5?A�5?A�7LA�9XA�33A�/A�1'A�33A�5?A�1'A�1'A�7LA�9XA�33A�/A�1'A�5?A�5?A�33A�/A�1'A�33A�5?A�1'A�1'A�33A�7LA�33A�1'A�1'A�5?A�7LA�5?A�33A�33A�7LA�9XA�5?A�33A�5?A�9XA�9XA�5?A�33A�5?A�7LA�7LA�5?A�1'A�5?A�9XA�7LA�5?A�33A�5?A�9XA�9XA�7LA�33A�5?A�9XA�9XA�7LA�5?A�5?A�;dA�9XA�9XA�5?A�33A�7LA�9XA�9XA�5?A�5?A�7LA�9XA�9XA�5?A�5?A�7LA�;dA�9XA�5?A�7LA�;dA�;dA�7LA�5?A�5?A�9XA�7LA�5?A�33A�33A�7LA�7LA�7LA�5?A�5?A�5?A�9XA�;dA�;dA�5?A�33A�9XA�;dA�7LA�7LA�5?A�7LA�7LA�5?A�1'A�5?A�9XA�7LA�5?A�33A�7LA�9XA�5?A�1'A�1'A�5?A�5?A� �A��A� �A�"�A� �A��A��A�bA�bA�%A���A���A���A��A��mA��mA��A��A��TA��A��;A��;A��;A��
A���A��
A���A�ƨA���A۸RA۶FA۩�AۃA�^5A�M�A�C�A� �A��mA��/A��#A��
A���A���A�ȴA���A�ȴA�ƨA���AڼjAڸRAڲ-AڬAڟ�Aڟ�Aڝ�Aڗ�Aڏ\A�z�A�n�A�ffA�ZA�33A�(�A�/A�&�A�$�A��A�{A�bA�
=A��A��TA��HA��/A���A٩�AٍPAًDAٍPA�z�A�ZA�&�A���A��`Aش9AؑhA�t�A�;dA��A�JA�%A��yA��;A��/A��#A��#A���A���A�ƨAח�A�1'A��A�1A��/A֧�A֍PA�|�A�bNA�G�A�5?A���A�ĜAա�A՗�AՇ+A�bNA��A�1A��HAԮA�r�A�7LA�-A� �A�
=A��A��TA���A���A���AӸRAӟ�A�t�A�"�Aҗ�AғuAҏ\A�A�A���A���A���A��`A�bAѩ�A�$�A��TAЬA�ȴAП�AЋDAЍPAЇ+A�~�A�l�A�$�A�JA�1A�1A�A�  A�  A�VA��A��/A��
A���AϸRAϰ!Aϰ!Aϴ9A���Aϩ�A�l�A�p�A�bNA�VA��Aδ9A΋DA΃A�M�A�+A�{A���A���A���A��A��TA��A���A�ȴAͺ^AͲ-A͡�A͕�A�~�A�`BA�?}A�A��HA�ȴA̶FA̩�Ḁ�Ȁ\A�l�A�bA��A���A˟�A�~�A�dZA�VA�G�A�?}A�9XA�9XA�5?A�5?A�-A��A�A���A���A��yA��`A��#A���AʶFAʧ�Aʝ�AʁA�|�A�p�A�`BA�O�A�7LA� �A�A�ĜAɺ^Aɣ�Aɉ7A�bNA�I�A��Aȡ�A�Q�A���A�x�A�dZA�Q�A�K�A�;dA�$�A�JA���A��mA��HA��HA���AƝ�A��A��yAũ�A�t�A�;dA��Aĺ^AčPA�n�A�^5A�O�A�?}A�7LA�/A�(�A��A��Aô9AÍPAÉ7A�r�A�dZA�S�A�9XA��A��
A®A\A�VA� �A��mA��wA���A�~�A�VA��A�x�A�G�A��A��mA��A�ƨA��A�ffA�\)A�VA�VA�VA�VA�K�A��A�JA���A���A���A���A���A���A�  A�  A�A��A���A��#A��mA��yA��yA��A��A��A��A��mA��/A�ƨA���A�ffA� �A�1A�  A���A��TA���A��\A�z�A�x�A�v�A�x�A�t�A�t�A�l�A�^5A�G�A�
=A��;A���A��A���A�1'A���A��#A���A���A���A�A��wA��RA��A���A�~�A�XA��A��\A�$�A��A���A���A��\A��A��^A���A�ffA�bA��!A�Q�A�1'A�
=A��jA��A�bNA�
=A�bNA���A���A��\A�p�A�`BA�9XA���A���A�v�A�XA�-A�ȴA��A��A���A��\A�dZA�G�A�33A��A��#A���A���A���A���A���A���A�ȴA�ĜA��-A��hA�M�A��uA�oA�A�  A�A���A�ƨA��uA�XA��A�  A�  A���A��A��A��HA���A���A�ZA� �A�
=A���A��A�Q�A���A�~�A�p�A�C�A�-A� �A�oA���A��
A���A���A�A��RA���A���A�z�A�\)A�7LA�JA��A��A�ĜA���A��A�l�A�S�A�+A��/A��uA��
A���A�33A��/A���A�ƨA�^5A�A�A��A��RA�JA��`A���A��hA���A��PA�&�A���A��^A�hsA�oA��^A�|�A�$�A��mA��FA�`BA�1'A�-A�$�A���A�ȴA��A���A��A�K�A�{A���A��A��
A���A�33A��A��7A��A���A�S�A��A��A���A�33A���A���A�v�A�I�A�7LA��`A��A�1'A��RA�A��!A���A��\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                    A�hsA�hsA�n�A�^5A�dZA�n�A�E�A�1'A�+A�/A�+A�/A�9XA�7LA�7LA�7LA�7LA�5?A�5?A�5?A�7LA�33A�5?A�33A�33A�33A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�9XA�7LA�5?A�7LA�7LA�5?A�/A�VA��`AۼjA�AھwA�x�A�oAٗ�A�l�Aש�A�l�A��A��mA�jA��mA��A���A�A���A�z�A�jA�Q�A���A�jA�p�A�|�AƑhAļjA��TA���A�r�A��wA�33A���A��`A��9A���A�hsA��yA�A��A��#A��/A��RA�S�A��A���A��A��RA��A��A�ĜA���A�
=A��A�?}A�(�A�7LA�dZA�bNA���A�dZA�ffA�VA��+A�?}A��wA���A���A�  A�z�A��FA�XA�  A{�Av��At�\As/Aq�ApbNAe�hAcO�A^ZAZI�AY��AY+AW33AU�AUAS��AR�!ARAP{AO7LAM�;AKl�AJbNAG��AGO�AE��ADjAB�RAA��A?�^A<�A;XA9hsA5K�A1ƨA0�uA/l�A.$�A-��A-C�A-
=A,��A,$�A+��A*��A*�uA*^5A+&�A+�A*�A)ƨA)�-A)x�A(�uA'�A'��A'G�A%��A$r�A$1A#�mA#�PA#%A"n�A!dZA!&�A!�A!p�A ��At�A��A��Ap�AȴAbNA$�Av�AbNA�A�RA$�A7LA�A�A��A��A�AbAQ�A�A�AƨAdZA��A�FAoA��A�FAS�AC�AK�A�;@���@�V@��@�j@���@�;d@�\)@�+@��@�hs@�G�@���@�S�@�-@��@� �@�  @�^5@�E�@��/@��@�P@�@��@�r�@�(�@�o@���@�j@��`@睲@�`B@�+@��H@��@�9@���@���@�S�@�^5@�x�@�Ĝ@�A�@��@�Z@߅@�@ܴ9@ݙ�@��@܃@�ƨ@�C�@���@�v�@�M�@�$�@ى7@�/@�x�@���@���@�7L@�V@�V@��@���@���@��/@ج@أ�@أ�@�Z@ץ�@�"�@�@�^5@�%@�z�@�I�@�1'@�A�@�r�@�9X@�b@�l�@���@�ȴ@җ�@�v�@�{@���@�z�@�I�@�9X@�1'@ϥ�@θR@�^5@�J@́@�&�@���@�z�@�bN@�Q�@�I�@�b@���@˅@��@ʰ!@�n�@�{@��#@ɡ�@Ɂ@�X@�7L@ȼj@��@���@��H@�n�@��@�`B@��/@Ĭ@�  @Ý�@�|�@�33@���@�n�@�J@��-@�?}@��u@�1'@�33@�ȴ@��+@���@���@�X@��@�r�@��P@��@�33@��+@�@�?}@�Ĝ@��u@�z�@�I�@�ƨ@�l�@�+@�"�@��H@�V@�J@��#@�hs@��@��j@�Z@� �@��@�K�@�
=@���@���@�`B@��@��/@���@��u@�1'@��
@���@��P@�S�@�"�@�
=@���@�^5@��^@��@���@�r�@��;@�K�@�
=@�o@�ȴ@���@���@��!@���@���@���@�n�@�5?@��T@��@���@��@��u@� �@�;d@�V@�=q@�$�@��^@��9@���@���@�@�M�@���@��@��@��j@�Q�@�1@�ƨ@���@�|�@�"�@��@��R@�n�@�M�@�@���@��@�%@���@�I�@�b@���@���@��P@�S�@��@���@�M�@�{@���@�?}@���@���@�j@�9X@��@��@�\)@�33@���@�v�@��@��^@�?}@���@��@��9@���@��D@�z�@�j@�9X@���@��@�S�@�o@��H@��!@��+@�ff@��@��T@���@���@��7@�O�@�&�@��`@��u@�Q�@�9X@���@��w@��@�;d@���@�E�@�J@���@�@��7@�`B@�?}@�V@�Ĝ@��D@�Z@�1'@���@��
@��w@�t�@�S�@�@���@�ȴ@��!@�^5@�E�@�-@��-@�G�@��@��`@�Ĝ@��@���@�r�@�A�@��@�+@��H@��\@�=q@�@�@��#@�X@�V@��@���@���@�z�@�A�@�(�@�1@���@��w@�t�@�33@�o@�@��@�ȴ@��R@�v�@�M�@�-@�@���@��7@�p�@�O�@�/@��@��j@�@�w@��@\)@
=@~E�@|�/@|I�@|(�@|1@{ƨ@{�@{S�@z�!@y��@yX@x�u@x�`@x�9@x��@y�@y&�@y7L@y7L@xĜ@x�@x �@v�y@vff@vV@v5?@u�@u��@uO�@t��@t(�@s�F@s�@s"�@r��@r=q@rJ@q�@q�7@q%@p��@o�w@o|�@o��@o��@o+@n��@nV@n{@mO�@l�D@k�F@k"�@j�@j��@j^5@j=q@i�@i7L@h��@hQ�@h  @g�w@g;d@f�y@f�R@f�+@fV@e�@e`B@d�@d�@c�F@b�H@b�@a�@a��@a��@a%@`Ĝ@`r�@_�;@_�@_l�@_+@^��@^�@^�+@^E�@^5?@]�@]�h@]�@]/@\�/@\(�@\�@[��@[��@[�m@[ƨ@[��@Z��@Z-@Y�@Y��@Y��@Y��@Yhs@Y%@XĜ@XA�@Xb@W�;@V��@V��@Vv�@Vff@VE�@V{@U@U@U�-@U�-@Up�@U�@Tz�@S�@So@S@R�!@Q�@QG�@Q%@P��@O�@O��@O�P@O|�@O;d@Nv�@M�-@M�@M/@L9X@K�
@K�
@KdZ@J�H@J��@JM�@JJ@JJ@I�@I��@IG�@HĜ@H�@G�@Gl�@G;d@F��@Fȴ@F��@Fff@F$�@E�h@D��@D�@D�D@D(�@C�
@C�@C"�@B�!@B�@A�@A��@A��@Ahs@A&�@@��@@Ĝ@@r�@@1'@@  @?|�@?;d@?
=@>��@>5?@=�h@<�/@<�@<��@<Z@;ƨ@;S�@;o@;@:��@:n�@:=q@9�#@9��@9x�@8�`@8�9@8��@8�@81'@8 �@8b@8b@7�;@7|�@7;d@7
=@7
=@6�y@6�@6ȴ@65?@5�-@5�h@5p�@5?}@4��@4z�@4j@4(�@3��@3��@3�m@3�F@3�@3t�@3"�@2�@2�@2��@2~�@2-@1��@1G�@1&�@0��@/�@/+@.�R@.v�@.ff@.V@-O�@,�j@,��@+ƨ@+C�@*��@*^5@*-@)�7@)&�@(r�@( �@'�@'�w@'|�@'+@&��@&ȴ@&��@&v�@%�@%@%�h@%p�@%`B@%O�@%?}@%V@$��@$Z@#ƨ@#��@#o@"��@!��@!��@!�@ Q�@�@�@�;@��@�P@�P@��@�P@�P@|�@�P@K�@+@�@�+@5?@ff@@��@p�@O�@?}@��@��@z�@I�@9X@��@��@�m@�F@��@dZ@C�@33@@��@~�@n�@n�@^5@^5@M�@-@�@��@�7@G�@�@�`@��@Q�@1'@1'@1'@1'@1'@1'@ �@ �@ �@ �@ �@�;@  @  @+@��@v�@V@5?@5?@$�@$�@�@��@�@O�@��@�@�@�@��@j@�@ƨ@�@C�@o@o@o@�@��G�O�A�n�A�ffA�dZA�l�A�ffA�dZA�n�A�l�A�n�A�`BA�^5A�`BA�`BA�hsA�hsA�n�A�p�A�hsA�=qA�1'A�7LA�1'A�/A�+A�+A�-A�1'A�/A�+A�+A�/A�33A�1'A�+A�/A�5?A�=qA�;dA�;dA�33A�-A�33A�9XA�;dA�9XA�9XA�7LA�7LA�7LA�5?A�7LA�;dA�7LA�5?A�7LA�9XA�9XA�7LA�5?A�7LA�9XA�5?A�1'A�33A�7LA�7LA�33A�1'A�5?A�9XA�5?A�33A�7LA�7LA�5?A�33A�7LA�9XA�7LA�5?A�5?A�7LA�9XA�33A�/A�1'A�33A�5?A�1'A�1'A�7LA�9XA�33A�/A�1'A�5?A�5?A�33A�/A�1'A�33A�5?A�1'A�1'A�33A�7LA�33A�1'A�1'A�5?A�7LA�5?A�33A�33A�7LA�9XA�5?A�33A�5?A�9XA�9XA�5?A�33A�5?A�7LA�7LA�5?A�1'A�5?A�9XA�7LA�5?A�33A�5?A�9XA�9XA�7LA�33A�5?A�9XA�9XA�7LA�5?A�5?A�;dA�9XA�9XA�5?A�33A�7LA�9XA�9XA�5?A�5?A�7LA�9XA�9XA�5?A�5?A�7LA�;dA�9XA�5?A�7LA�;dA�;dA�7LA�5?A�5?A�9XA�7LA�5?A�33A�33A�7LA�7LA�7LA�5?A�5?A�5?A�9XA�;dA�;dA�5?A�33A�9XA�;dA�7LA�7LA�5?A�7LA�7LA�5?A�1'A�5?A�9XA�7LA�5?A�33A�7LA�9XA�5?A�1'A�1'A�5?A�5?A� �A��A� �A�"�A� �A��A��A�bA�bA�%A���A���A���A��A��mA��mA��A��A��TA��A��;A��;A��;A��
A���A��
A���A�ƨA���A۸RA۶FA۩�AۃA�^5A�M�A�C�A� �A��mA��/A��#A��
A���A���A�ȴA���A�ȴA�ƨA���AڼjAڸRAڲ-AڬAڟ�Aڟ�Aڝ�Aڗ�Aڏ\A�z�A�n�A�ffA�ZA�33A�(�A�/A�&�A�$�A��A�{A�bA�
=A��A��TA��HA��/A���A٩�AٍPAًDAٍPA�z�A�ZA�&�A���A��`Aش9AؑhA�t�A�;dA��A�JA�%A��yA��;A��/A��#A��#A���A���A�ƨAח�A�1'A��A�1A��/A֧�A֍PA�|�A�bNA�G�A�5?A���A�ĜAա�A՗�AՇ+A�bNA��A�1A��HAԮA�r�A�7LA�-A� �A�
=A��A��TA���A���A���AӸRAӟ�A�t�A�"�Aҗ�AғuAҏ\A�A�A���A���A���A��`A�bAѩ�A�$�A��TAЬA�ȴAП�AЋDAЍPAЇ+A�~�A�l�A�$�A�JA�1A�1A�A�  A�  A�VA��A��/A��
A���AϸRAϰ!Aϰ!Aϴ9A���Aϩ�A�l�A�p�A�bNA�VA��Aδ9A΋DA΃A�M�A�+A�{A���A���A���A��A��TA��A���A�ȴAͺ^AͲ-A͡�A͕�A�~�A�`BA�?}A�A��HA�ȴA̶FA̩�Ḁ�Ȁ\A�l�A�bA��A���A˟�A�~�A�dZA�VA�G�A�?}A�9XA�9XA�5?A�5?A�-A��A�A���A���A��yA��`A��#A���AʶFAʧ�Aʝ�AʁA�|�A�p�A�`BA�O�A�7LA� �A�A�ĜAɺ^Aɣ�Aɉ7A�bNA�I�A��Aȡ�A�Q�A���A�x�A�dZA�Q�A�K�A�;dA�$�A�JA���A��mA��HA��HA���AƝ�A��A��yAũ�A�t�A�;dA��Aĺ^AčPA�n�A�^5A�O�A�?}A�7LA�/A�(�A��A��Aô9AÍPAÉ7A�r�A�dZA�S�A�9XA��A��
A®A\A�VA� �A��mA��wA���A�~�A�VA��A�x�A�G�A��A��mA��A�ƨA��A�ffA�\)A�VA�VA�VA�VA�K�A��A�JA���A���A���A���A���A���A�  A�  A�A��A���A��#A��mA��yA��yA��A��A��A��A��mA��/A�ƨA���A�ffA� �A�1A�  A���A��TA���A��\A�z�A�x�A�v�A�x�A�t�A�t�A�l�A�^5A�G�A�
=A��;A���A��A���A�1'A���A��#A���A���A���A�A��wA��RA��A���A�~�A�XA��A��\A�$�A��A���A���A��\A��A��^A���A�ffA�bA��!A�Q�A�1'A�
=A��jA��A�bNA�
=A�bNA���A���A��\A�p�A�`BA�9XA���A���A�v�A�XA�-A�ȴA��A��A���A��\A�dZA�G�A�33A��A��#A���A���A���A���A���A���A�ȴA�ĜA��-A��hA�M�A��uA�oA�A�  A�A���A�ƨA��uA�XA��A�  A�  A���A��A��A��HA���A���A�ZA� �A�
=A���A��A�Q�A���A�~�A�p�A�C�A�-A� �A�oA���A��
A���A���A�A��RA���A���A�z�A�\)A�7LA�JA��A��A�ĜA���A��A�l�A�S�A�+A��/A��uA��
A���A�33A��/A���A�ƨA�^5A�A�A��A��RA�JA��`A���A��hA���A��PA�&�A���A��^A�hsA�oA��^A�|�A�$�A��mA��FA�`BA�1'A�-A�$�A���A�ȴA��A���A��A�K�A�{A���A��A��
A���A�33A��A��7A��A���A�S�A��A��A���A�33A���A���A�v�A�I�A�7LA��`A��A�1'A��RA�A��!A���A��\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�kQBYB�B+B�BBBSB$B�B�B�B�B�BYBYB�B�B�B$B�B�B�B�B�B�B�B�B�B�B�B$B�B$B$B�B�B�B�B�B_BeB�B%B)�B-�B'�B#:B�B�B
�JB
�B
�WB
��B
�*B
�FB
�B
�B
�@B
��B
��B
�bB
��B
�OB
�3B
�BB
��B
�B
�
B
��BMB�B%zB$�B�B'RBbNB��B��B�;B�zB�wB�0B�HB�B�B�DB�B�VB��B�B��B��B�TB�XBŢB�UB�!B��BzDBc�BUgBK�B2�B 'BB
��B
ޞB
��B
�B
�OB
��B
�B
��B
z�B
e�B
VB
?�B
(XB
"4B
_B
�B	�B	��B	˒B	�6B	��B	�4B	��B	�MB	�bB	��B	��B	�MB	~�B	u%B	tB	ffB	`�B	V�B	MjB	J�B	@�B	=qB	.�B	1[B	($B	!�B	 �B	!B	FB	
rB	
�B	�B	�B	B	%zB	'�B	>wB	I�B	K�B	J#B	S�B	{B	�+B	�uB	� B	�4B	��B	��B	��B	�\B	�'B	��B	��B	��B	�-B	��B	�:B	�@B	�B	��B	�B	ӏB	�?B	֡B	�jB	�B	��B	�B	��B	�dB	�B	��B	��B	�B	�sB	��B	�)B	خB	�yB	�gB	��B	бB	�/B	��B	�mB	�B	��B	�,B	�B	�B	��B	��B	��B	P}B	N�B	K�B	1�B	.IB	/B	2aB	:�B	@B	F?B	J�B	NpB	O�B	N�B	O�B	VB	XyB	XyB	YB	X�B	^B	aB	c B	_B	l"B	n�B	n/B	i�B	h�B	l�B	n�B	rB	~(B	�B	��B	�uB	�B	�B	��B	�_B	��B	�B	��B	�B	�=B	��B	�B	�FB	��B	�OB	�eB	�B	�$B	�XB	��B	�kB	��B	��B	��B	�aB	�nB	��B	�6B	��B	� B	��B	�aB	ĜB	ȴB	��B	��B	��B	��B	�<B	уB	ӏB	�9B	�B	�?B	רB	ԕB	֡B	�B	خB	ںB	�vB	�B	�
B	�B	�2B	��B	�`B	�B	�&B	�2B	�
B	�B	��B	�)B	�iB	�B	�MB	�B	�fB	�rB	�xB	�JB	�JB	��B	��B	��B	�]B
 4B
B
B
oB
B
�B
�B
GB
GB
�B
B
�B
MB
�B
fB
	�B
	lB
	7B
	�B
	�B
	�B
	�B
�B
(B
�B
�B
bB
4B
�B
�B
�B
uB
�B
�B
{B
MB
�B
B
�B
�B
�B
�B
YB
�B
�B
�B
B
kB
�B
CB
B
B
CB
~B
IB
IB
OB
�B
�B
!B
!B
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
!-B
!-B
!�B
!�B
!�B
!�B
!�B
!�B
"hB
#nB
%FB
%FB
$�B
$�B
$�B
&B
)*B
)_B
)�B
*eB
*�B
+B
+B
+B
,=B
,=B
-B
,�B
-B
,�B
-B
.�B
0!B
/B
0�B
0�B
0�B
1�B
1[B
1�B
2�B
2�B
3�B
3�B
49B
4�B
6FB
6�B
7B
7B
7LB
7�B
7�B
8RB
8�B
8�B
8�B
8�B
9$B
9�B
9�B
:^B
:�B
;�B
;�B
;�B
<B
<jB
=B
=�B
=<B
>B
=�B
<�B
=�B
>B
>BB
=�B
<�B
>B
@�B
C-B
C�B
A�B
B'B
DgB
C�B
DgB
EB
FtB
GzB
GzB
G�B
HB
H�B
H�B
HKB
H�B
H�B
H�B
IRB
IRB
JXB
J�B
J�B
J�B
J�B
K^B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
LdB
L�B
L�B
L�B
L�B
MB
M6B
MB
M6B
M6B
M�B
M�B
M�B
M�B
N<B
NB
N�B
NpB
N�B
O�B
O�B
OvB
O�B
O�B
OvB
OvB
PHB
P}B
P}B
QNB
RTB
T,B
T�B
UgB
UgB
U�B
V�B
V9B
V�B
V�B
XEB
W�B
X�B
YKB
Y�B
Y�B
Y�B
Z�B
[�B
[WB
[�B
[�B
\]B
\)B
\�B
]dB
^B
^B
]dB
]�B
]�B
^B
]�B
]dB
]/B
^B
]�B
]�B
]�B
]�B
]�B
^5B
\�B
\�B
\]B
\�B
[�B
\]B
[�B
Z�B
[�B
[�B
[�B
[�B
\)B
]dB
]�B
^5B
^B
^�B
aHB
cTB
c�B
d&B
e,B
gB
g8B
gB
g8B
h>B
gmB
g�B
g�B
g�B
h>B
h>B
h�B
h�B
iyB
i�B
jB
jKB
jB
i�B
jB
jB
jB
j�B
jB
jKB
j�B
k�B
k�B
l"B
l"B
l"B
m�B
l�B
ncB
m�B
n/B
n�B
n/B
ncB
n/B
n/B
n/B
n/B
ncB
ncB
n�B
ncB
ncB
n�B
o�B
o�B
p;B
p;B
p�B
poB
q�B
rGB
rGB
rGB
sMB
sMB
s�B
tB
t�B
t�B
t�B
u%B
u%B
u%B
u%B
u%B
t�B
u%B
t�B
t�B
t�B
u%B
u�B
uZB
u�B
u�B
uZB
uZB
uZB
v`B
u�B
v`B
v`B
v+B
v`B
v�B
xlB
x�B
yrB
y>B
y	B
y>B
y>B
y>B
yrB
yrB
yrB
z�B
{JB
{B
{JB
|PB
|�B
}"B
|�B
{�B
|�B
|�B
~(B
}�B
}�B
~(B
~�B
~�B
~�B
~�B
.B
� B
� B
�B
�;B
�B
�B
��B
��B
�uB
�B
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
��B
�%B
�%B
�YB
�YB
��B
�+B
�_B
��B
��B
��B
�fB
��B
�B
��B
��B
�	B
�	B
��B
�	B
�=B
�=B
�=B
��B
��B
��B
�DB
�DB
�xB
�~B
�~B
��B
�PB
�B
�B
��B
��B
�VB
�VB
�"B
��B
��B
��B
�(B
��B
�(B
�\B
�(B
�(B
�\B
��B
��B
�\B
�\B
��B
�.B
�.B
�bB
�.B
�bB
�.B
�.B
�4B
�4B
� B
� B
��B
�bB
��B
��B
�bB
�.B
�bB
�bB
� B
�4B
� B
� B
� B
��B
��B
�B
�:B
�:B
��B
��B
��B
�uB
��B
�uB
�uB
�@B
�uB
�B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
��B
��B
�1B
�eB
��B
��B
��B
�B
�kB
��B
��B
�	B
��B
�=B
�=B
�=B
�qB
�qB
�xB
��B
�xB
�~B
�IB
�B
�B
��B
�\B
��B
��B
�'B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�'B
�-B
��B
��B
��B
�bB
��B
��B
�4B
�hB
�hB
�hB
��B
�nB
�nB
�nB
��B
�B
��B
��B
�@B
�B
�tB
�tB
�tB
��B
��B
�FB
�FB
�zB
�zB
�zB
�zB
��B
�zB
��B
�LB
��B
��B
��B
�B
�RB
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
�RB
��B
�*B
�_B
�_B
��B
��B
��B
��B
��B
�0B
��B
�0B
��B
�B
�B
��B
�B
�B
�kB
�B
�=B
�qB
��B
��B
��B
��B
�B
�@�OvB�B_B�B�B�B�BYB+B=B$BB�B�B�B�BMB�B%FB�B$BMBB�B�BSB�B�B�BYB�B�B�B�BB�BB�BSB�B�B�BMBB�B+B�BMB�B�BSB�B�B+B$B�B�B�B+B�B�B�B�B$B�BB�B�BSB�B$B�BSBB�B�B�B�B�BYB$B�BB�B_B$BSB�B$B$B�B�B�B�B�B�B�B�B�BYB�B�BYB�B�B�B�B�B�BSB�BB�B�BB�B�B�B$B�B�B�B_B�B�BSB�B�BYBB�BYB+B�BSB�B�B�BYBSBBYB_B�B�B�BB�B+B�BBB�B�BYBSB�B�B_BYB�B�B�B�BSB�BYB�B�B�B�B�B�B+B$B�BYB�B�B�B�B�B$B+B1B$B�B�B+B_B�B�B�B�B�B�BYB1B1B�B�B�BeB�B�B+B�B�B�BkB�B�B�B�BIB 'B#B#B!bB$@B%FB$�B"�B"�B%�B'�B%zB%B%B'�B'�B&LB'�B(�B)*B*�B(�B+6B49B2�B3�B0�B3hB-�B+�B*0B(�B($B(�B)_B'�B'B&�B(�B(XB&�B%B&�B%�B$@B"�B#:B$@B$tB!�B�B!�B$B	B�BB�BeBOB�BBB+B�BuB:B�B�B~B
�BB
�B~B�BB
�VB
��B
��B
�VB
�+B
��B
�B
�B
�5B
�B
�B
�B
�)B
�B
�WB
�AB
�B
�B
�B
��B
�B
�B
خB
�B
�KB
��B
چB
��B
ʌB
�^B
��B
�HB
�HB
��B
�#B
�^B
�-B
B
��B
�dB
�dB
�^B
�XB
�$B
�^B
�RB
�B
��B
�jB
�B
�KB
�9B
��B
�-B
�[B
�CB
��B
�wB
��B
��B
��B
�?B
��B
��B
�}B
��B
�=B
�B
��B
��B
�tB
��B
��B
��B
�:B
��B
��B
�:B
�B
��B
��B
��B
�B
�zB
��B
��B
�IB
�zB
�XB
��B
��B
�FB
�LB
�'B
��B
��B
�LB
��B
�nB
�:B
�4B
��B
��B
�B
�nB
�4B
��B
��B
�'B
��B
��B
�bB
��B
�bB
��B
�~B
��B
��B
�=B
��B
�1B
��B
��B
��B
�0B
��B
�wB
��B
�B
��B
��B
��B
��B
��B
�IB
��B
�[B
�9B
�-B
�aB
�B
�3B
��B
��B
��B
�*B
�dB
�OB
��B
��B
�dB
��B
�OB
��B
��B
ȀB
�[B
��B
��B
��B
��B
�mB
�B
ݘB
��B
�B
�B
��B
�B
�B
�HB
�ZB
�TB
�,B
�B
�HB
�&B
�
B
�B
��B
��B
�%B
�8B 4B
��BAB
��B
��B
�VB
�.B
��B 4B
��BB�BDB	�B�B
=B�B�B�B�B@B:B�B�B�BB�B!bB#�B&�B2aB4nB+B+�B&�B!�B"�B+6B!B�B�BVB�B~B�B#nB�B!B �B 'B 'B �B$�B'�B,�B6zB<jBB�BN�Bc�Bj�BpoBv�B{B|PB~�B�oB��B��B�PB��B��B|�B|�BzDB~�B�B�+B��B��B�B�4B�BcB�B��B�AB�xB�_B�@B�RB��B�B��B�nB�B��B�wB�CB��B�6B�B��B�}B��B�<B��B�B��B�BB�qB�OB�B�pB��B�B�|B�5B�B��B��B�)B�>B�KB�DB1B��B��B��B�B�B�>B��B��B�TB�B��BoB�BBDB1BBB�BxB�]B�VB�VB��B�"B��B��B�B��B��B��BB�B�	B��B�B�]B�B��B�B�GB�B�B��B�fB�`B�TB��B�B�mB�B�B�/B�#B�ZB�jB�BJ�B��B�TB�vB̘B�6B�B��B�RB��B�XB��B�XBɺB�)BȴBǮB�RB��B�HB�}B��B��B�jB��B��B��B�^B�KB��B��B�IB��B��B��B��B�VB��B��B�FB�B�}B��B��B�+B}�B|�B�BsMBtBs�BuZBj�Bi�Bm�B_;B[�B[#B`�B\�BWsBT�BUgBZ�BU�BN<BL�BLdBO�BS[BM6BIRBM�BD�B:�B6zB4nB9�B33B6�B&�B*0B"�B 'B+BMB'�B$B�B	7B 4B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021092523122320210925231223IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021100602004520211006020045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021100602004520211006020045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365320220126093653IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                