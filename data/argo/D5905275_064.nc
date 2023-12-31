CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-10-11T12:07:08Z creation; 2023-04-26T19:14:28Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20191011120708  20230426191428  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               @   @AA  AOAO7316_008644_064                 7316_008644_064                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @��^��@��^��11  @��^Z�c @��^Z�c @)�hۋ�q@)�hۋ�q�cݵI�HV�cݵI�HV11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�@B�\@z�H@��R@��R@�  A ��A��A!G�A,(�A@  AaG�A���A�Q�A��A�\)A�  A�  A�\)A�  B   B�
B�
B  B   B'�
B0  B8  B?�
BG�BO�
BW�
B_�
BhQ�Bp(�Bw�
B�
B�{B�  B�  B�  B�  B�  B�{B�(�B�  B�  B�  B�{B�{B�{B�  B��B��B�  B��B�  B�(�B�{B�{B�  B��B��B��B�  B��B�  B�  B��C�C��C  C  C

=C
=C  C  C��C�C��C��C��C  C  C   C"
=C$  C%�C'��C)��C+��C.
=C0  C1�C3��C6  C8
=C:
=C<
=C>  C?��CA��CC��CF
=CH{CJ
=CL
=CN
=CO��CQ�CS��CV  CX  CZ{C\
=C^  C`
=Cb  Cc��Cf  Ch  Ci��Cl
=Cn{Cp
=Cr  Ct  Cu��Cx
=Cz{C|
=C}��C�C���C�  C�C�  C���C���C���C���C�  C�
=C�  C���C�  C�  C�
=C�C�C�  C�  C���C���C�  C�  C���C�C�C�C�C���C���C�  C���C���C�  C���C���C���C���C�  C���C�  C�  C���C���C���C���C���C�C�  C���C�C�
=C�  C���C�  C�C�  C���C���C���C�  C�C�
=C�C���C�  C�  C���C�  C�C�C�  C�  C�  C�  C���C���C���C���C���C���C�  C�  C�C�C�C�C�C���C�  C�  C���C�C�  C���C�  C���C���C�  C�  C���C���C���C�  C���C���C���C���C���C�  C�  C���C�C�  C���C���C�C�C���C���C�C�C�C�C�  C���C�  D   D ��D�D� D  D� DD�D  DxRD��Dz�D�qD}qD�qD�DD��D	�D	��D
�D
��D  D}qD�D��D  D� D  D� D�qD}qD�qD}qD�qD}qD�qDz�D��D� D  D}qD�D��D  D��D  Dz�D�qD��D�D��D�D� D  D��D�qD}qD  D��D�qD}qD�D��D   D � D!  D!� D"�D"��D#�D#� D#�qD$}qD$�qD%}qD&  D&� D'�D'��D'�qD(}qD)�D)��D*�D*� D+  D+� D,  D,� D,�qD-� D.�D.��D/  D/� D0  D0� D0�qD1}qD1�qD2}qD2�qD3� D4  D4� D5  D5� D6�D6� D7  D7��D8  D8��D9D9��D9�qD:� D;�D;��D<�D<� D=  D=� D>  D>� D>�qD?� D@�D@}qD@��DAz�DA�qDB� DC�DC��DD  DD� DE  DE}qDF  DF}qDG  DG� DG��DH}qDI�DI�DJDJ� DJ�qDK}qDK�qDL� DM  DM}qDM�qDN��DO�DO}qDP  DP� DP�qDQz�DR  DR��DS  DS}qDS�qDT� DUDU� DV  DV��DV�qDW� DX  DX��DY  DY}qDZ  DZ� D[�D[}qD[�qD\� D\�qD]z�D^  D^��D_�D_� D`  D`}qDa  Da��DbDb� Db�qDc� Dd  Dd� De  De� Df�Df�Dg  Dg� DhDh� Di  Di�Dj�Dj}qDj�qDkz�Dl  Dl� Dl��Dm� DnDn��Do�Do� Dp  Dp��Dq  Dqz�Dq�qDr}qDr�qDs� Dt�Dt��Du�Du� Du�qDv� Dw�Dw� Dw�qDxz�Dx��Dy}qDy�qDz� D{D{� D{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�� D�D�HD�@ D�� D��HD�  D�@ D�� D�� D���D�>�D��HD�D�  D�@ D��HD��HD�  D�@ D���D��HD���D�@ D��HD��HD�  D�@ D�� D���D�  D�@ D�� D��HD�HD�>�D�~�D��HD�HD�AHD��HD��HD�  D�>�D�~�D��qD���D�@ D�~�D���D���D�>�D�� D��HD�HD�@ D�~�D�� D�HD�AHD�~�D���D���D�>�D��HD�� D���D�@ D��HD�� D�  D�>�D�� D�D��D�@ D�� D�� D���D�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�� D��HD�HD�AHD�� D��qD�  D�AHD��HD��HD�HD�@ D�� D�D���D�=qD�~�D�� D�  D�AHD��HD��HD�HD�AHD�~�D��qD�  D�AHD�~�D�� D�HD�>�D�� D��HD�  D�>�D��HD�� D���D�@ D�� D���D�HD�>�D�}qD�� D���D�>�D��HD�D�HD�B�D�� D�� D�  D�@ D�� D�� D��D�B�D��HD���D�  D�AHD�� D���D�  D�AHD��HD�� D�  D�>�D��HD�� D��qD�>�D��HD��HD�HD�AHD���D�� D���D�>�D�� D��HD�HD�B�D��HD�� D�HD�B�D���D��HD�  D�AHD�� D�� D���D�@ D��HD�� D���D�=qD�� D��HD��D�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D��qD��qD�=qD��HD��HD�  D�@ D�~�D�� D���D�>�D�� D��HD�HD�@ D�~�D��qD�  D�AHD�� D��HD��D�AHD�� D���D���D�=qD�� D��HD�  D�@ D��HD��HD�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D���D���D�@ D�� D���D�  D�@ D�~�D���D���D�>�D D��HD��D�AHDÀ D��HD�HD�@ DĀ D�� D�  D�@ Dŀ Dž�D���D�@ DƁHD�� D�  D�B�Dǂ�D�D�  D�>�D�~�DȾ�D���D�>�D�~�D��HD�HD�@ D�~�D�� D�  D�@ DˁHD��HD�HD�AHD́HD�D�  D�@ D̀ D�� D�HD�AHD΀ D�� D�  D�>�Dπ D��HD�  D�@ DЀ D�� D���D�@ DсHD�� D�  D�AHD�~�DҾ�D�HD�AHDӀ D�� D�HD�@ DԁHD�� D���D�@ D�~�Dվ�D�  D�@ D�~�D־�D�  D�AHD׀ D׾�D���D�>�D؀ D��HD�HD�B�DفHD��HD�  D�@ Dڀ D�� D�  D�@ DہHD�D�HD�>�D܀ D��HD�  D�@ D݁HD�D�  D�@ DށHD��HD�  D�AHD߁HD��HD�HD�>�D�� DྸD�  D�>�D� D�� D�  D�@ D�~�D��HD�HD�@ D� D�� D�  D�=qD�~�D�� D���D�@ D�HD徸D�  D�@ D�HD澸D���D�>�D�HD��HD�  D�@ D� D��HD��D�@ D�}qD�qD��qD�>�D� D�� D�  D�AHD� D뾸D�HD�@ D� D�� D�  D�AHD�HD���D�  D�AHD�HD�qD�  D�@ D�~�D�� D�HD�AHD���D��HD�  D�@ D� D�� D�HD�>�D�~�D�� D�  D�B�D�HD�D�  D�@ D� D�� D�HD�@ D�~�D�� D�HD�B�D��HD��HD���D�>�D��HD�� D���D�>�D�� D�� D���D�@ D�� D�� D�  D�AHD�q�>�G�?.{?u?�33?��@�@+�@J=q@c�
@��\@���@�p�@��@�(�@˅@�Q�@�ff@�
=A�\A	��A��A�A\)A'
=A.{A5�A;�AC33AK�AQ�AX��A_\)AfffAmp�As33Ax��A~{A�=qA��A�\)A���A�(�A�ffA�Q�A�=qA�(�A�ffA�Q�A��A��
A�{A�Q�A��A��A�A�  A���A�33A�p�A��A�G�A�33A�p�A�\)A�G�A��HA���A�
=A�G�A��HA���AƸRA�G�A�33A���AθRA���A�33A���AָRA���AۅA��A޸RA��A�33A�p�A�
=A���A�33A�p�A�\)A�G�A�33A�A�  A���A��
A�{B (�B ��B{B
=B(�B��B�B33B(�B��B	�B
=B(�B��B�B�HB  B��BB�RB�
B��B��B�\B�B��Bp�B=qB33BQ�B�B{B�HB   B ��B!B"�\B#�B$��B%��B&=qB'33B((�B)G�B*=qB+33B,  B,��B-B.�HB/�
B0��B1��B2ffB3�B4��B5p�B6=qB7
=B8  B8��B9�B:�HB;�B<z�B=p�B>�\B?\)B@Q�BA�BA�BC
=BD(�BEG�BF{BF�HBG�
BH��BI�BK
=BL(�BMG�BN{BO
=BPQ�BQp�BR�RBS�
BT��BV{BW
=BXQ�BY��BZ�HB\(�B]�B^=qB_�B`��Ba�Bc33Bd��Be�Bg
=Bh(�Bip�Bj�HBl  Bmp�Bn�RBp(�Bq��Bs
=BtQ�Bu��Bw
=BxQ�ByB{33B|��B~{B�B�z�B�33B��
B��\B�33B��
B�z�B�33B��
B��\B�G�B�  B��RB�p�B�=qB��HB���B�Q�B�
=B�B��\B�G�B�{B���B�B���B�p�B�=qB�
=B��
B��RB��B�=qB�
=B��
B�z�B�33B��
B�z�B���B��B�{B��\B��B���B�  B�z�B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB��\B���B�\)B��
B�(�B��\B���B�p�B��
B�=qB���B�
=B��B�  B�ffB���B�33B��B�{B�z�B���B�\)B��
B�=qB���B��B��B��B�ffB���B��B��B�  B�ffB���B�G�B��B�(�B���B�
=B�p�B��B�ffB��HB�33B��B�(�B��\B���B�p�B��
B�=qB���B��B��B��B�ffB���B�G�B��B�{B��\B���B�\)B��
B�=qB��RB��B��B�{B�z�B���B�\)B��
B�=qBĸRB��Bř�B�  B�z�B��HB�G�B�B�(�Bȣ�B��Bə�B��B�Q�BʸRB��B˅B��B�Q�B���B�G�B�B�(�BΣ�B��BυB�  B�z�B��HB�\)B��B�ffBҸRB�G�BӮB�(�Bԣ�B��BՅB��B�ffB��HB�\)B��
B�Q�B��HB�\)B��
B�Q�B���B�G�B��
B�=qB���B�G�B�B�=qB޸RB�G�B߮B�=qB�RB�G�B�B�=qB��HB�p�B�  B�z�B�
=B�B�{B�\B�
=B癚B�{B��B�33B�B�=qB���B�\)B��
B�ffB��HB�\)B��
B�ffB��HB�\)B��B�z�B��B�B�{B�\B�
=B�B�=qB���B�\)B��B�z�B��HB�\)B��B��\B�33B�B�=qB��RB�G�B��
B�ffB���B���B�(�B��RB�33B�C �C ffC �RC  CG�C�\C��C{CQ�C�\C�
C(�Cp�C�RC��C=qCz�CC
=CQ�C��C��C=qC�C��C
=CQ�C��C�C=qC�\C�
C	{C	\)C	��C	��C
G�C
��C
�HC(�CffC�RC
=C\)C�C�C33C�\C�HC(�CffC�RC{CffC�C  CG�C��C��C=qC�C�HC=qC�C��C�Cz�C��C{C\)C�RC
=C\)C��C�CG�C��C�C=qC�\C�HC=qC�C�
C(�C�C�
C(�Cp�C�RC{Cp�CC�Cp�CC
=CffC�RC�Cp�C�RC
=CffCC {C ffC �RC!{C!p�C!��C"�C"p�C"C#(�C#�C#�
C$�C$z�C$�
C%33C%�\C%�HC&33C&z�C&�
C'33C'�\C'�HC(33C(�\C(�C)Q�C)��C)��C*G�C*��C+
=C+ffC+�RC,{C,ffC,C-�C-z�C-��C.�C.z�C.�HC/(�C/z�C/��C033C0�C0�HC1(�C1z�C1�
C2=qC2�C2��C3(�C3�\C3�HC433C4�C4�HC5G�C5��C5�C6=qC6��C6��C7Q�C7��C7�C8G�C8�C9  C9G�C9��C9��C:\)C:�C;  C;G�C;��C<  C<Q�C<��C<�C==qC=��C=��C>33C>�\C>�C?=qC?�\C?�
C@33C@�\C@�HCA33CA�CA�HCB=qCB�\CB�
CC(�CC�\CC�CD33CD�CD�HCE=qCE�\CE�
CF(�CF�CF�HCG33CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                       ?�  @�@B�\@z�H@��R@��R@�  A ��A��A!G�A,(�A@  AaG�A���A�Q�A��A�\)A�  A�  A�\)A�  B   B�
B�
B  B   B'�
B0  B8  B?�
BG�BO�
BW�
B_�
BhQ�Bp(�Bw�
B�
B�{B�  B�  B�  B�  B�  B�{B�(�B�  B�  B�  B�{B�{B�{B�  B��B��B�  B��B�  B�(�B�{B�{B�  B��B��B��B�  B��B�  B�  B��C�C��C  C  C

=C
=C  C  C��C�C��C��C��C  C  C   C"
=C$  C%�C'��C)��C+��C.
=C0  C1�C3��C6  C8
=C:
=C<
=C>  C?��CA��CC��CF
=CH{CJ
=CL
=CN
=CO��CQ�CS��CV  CX  CZ{C\
=C^  C`
=Cb  Cc��Cf  Ch  Ci��Cl
=Cn{Cp
=Cr  Ct  Cu��Cx
=Cz{C|
=C}��C�C���C�  C�C�  C���C���C���C���C�  C�
=C�  C���C�  C�  C�
=C�C�C�  C�  C���C���C�  C�  C���C�C�C�C�C���C���C�  C���C���C�  C���C���C���C���C�  C���C�  C�  C���C���C���C���C���C�C�  C���C�C�
=C�  C���C�  C�C�  C���C���C���C�  C�C�
=C�C���C�  C�  C���C�  C�C�C�  C�  C�  C�  C���C���C���C���C���C���C�  C�  C�C�C�C�C�C���C�  C�  C���C�C�  C���C�  C���C���C�  C�  C���C���C���C�  C���C���C���C���C���C�  C�  C���C�C�  C���C���C�C�C���C���C�C�C�C�C�  C���C�  D   D ��D�D� D  D� DD�D  DxRD��Dz�D�qD}qD�qD�DD��D	�D	��D
�D
��D  D}qD�D��D  D� D  D� D�qD}qD�qD}qD�qD}qD�qDz�D��D� D  D}qD�D��D  D��D  Dz�D�qD��D�D��D�D� D  D��D�qD}qD  D��D�qD}qD�D��D   D � D!  D!� D"�D"��D#�D#� D#�qD$}qD$�qD%}qD&  D&� D'�D'��D'�qD(}qD)�D)��D*�D*� D+  D+� D,  D,� D,�qD-� D.�D.��D/  D/� D0  D0� D0�qD1}qD1�qD2}qD2�qD3� D4  D4� D5  D5� D6�D6� D7  D7��D8  D8��D9D9��D9�qD:� D;�D;��D<�D<� D=  D=� D>  D>� D>�qD?� D@�D@}qD@��DAz�DA�qDB� DC�DC��DD  DD� DE  DE}qDF  DF}qDG  DG� DG��DH}qDI�DI�DJDJ� DJ�qDK}qDK�qDL� DM  DM}qDM�qDN��DO�DO}qDP  DP� DP�qDQz�DR  DR��DS  DS}qDS�qDT� DUDU� DV  DV��DV�qDW� DX  DX��DY  DY}qDZ  DZ� D[�D[}qD[�qD\� D\�qD]z�D^  D^��D_�D_� D`  D`}qDa  Da��DbDb� Db�qDc� Dd  Dd� De  De� Df�Df�Dg  Dg� DhDh� Di  Di�Dj�Dj}qDj�qDkz�Dl  Dl� Dl��Dm� DnDn��Do�Do� Dp  Dp��Dq  Dqz�Dq�qDr}qDr�qDs� Dt�Dt��Du�Du� Du�qDv� Dw�Dw� Dw�qDxz�Dx��Dy}qDy�qDz� D{D{� D{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�� D�D�HD�@ D�� D��HD�  D�@ D�� D�� D���D�>�D��HD�D�  D�@ D��HD��HD�  D�@ D���D��HD���D�@ D��HD��HD�  D�@ D�� D���D�  D�@ D�� D��HD�HD�>�D�~�D��HD�HD�AHD��HD��HD�  D�>�D�~�D��qD���D�@ D�~�D���D���D�>�D�� D��HD�HD�@ D�~�D�� D�HD�AHD�~�D���D���D�>�D��HD�� D���D�@ D��HD�� D�  D�>�D�� D�D��D�@ D�� D�� D���D�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�� D��HD�HD�AHD�� D��qD�  D�AHD��HD��HD�HD�@ D�� D�D���D�=qD�~�D�� D�  D�AHD��HD��HD�HD�AHD�~�D��qD�  D�AHD�~�D�� D�HD�>�D�� D��HD�  D�>�D��HD�� D���D�@ D�� D���D�HD�>�D�}qD�� D���D�>�D��HD�D�HD�B�D�� D�� D�  D�@ D�� D�� D��D�B�D��HD���D�  D�AHD�� D���D�  D�AHD��HD�� D�  D�>�D��HD�� D��qD�>�D��HD��HD�HD�AHD���D�� D���D�>�D�� D��HD�HD�B�D��HD�� D�HD�B�D���D��HD�  D�AHD�� D�� D���D�@ D��HD�� D���D�=qD�� D��HD��D�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D��qD��qD�=qD��HD��HD�  D�@ D�~�D�� D���D�>�D�� D��HD�HD�@ D�~�D��qD�  D�AHD�� D��HD��D�AHD�� D���D���D�=qD�� D��HD�  D�@ D��HD��HD�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D���D���D�@ D�� D���D�  D�@ D�~�D���D���D�>�D D��HD��D�AHDÀ D��HD�HD�@ DĀ D�� D�  D�@ Dŀ Dž�D���D�@ DƁHD�� D�  D�B�Dǂ�D�D�  D�>�D�~�DȾ�D���D�>�D�~�D��HD�HD�@ D�~�D�� D�  D�@ DˁHD��HD�HD�AHD́HD�D�  D�@ D̀ D�� D�HD�AHD΀ D�� D�  D�>�Dπ D��HD�  D�@ DЀ D�� D���D�@ DсHD�� D�  D�AHD�~�DҾ�D�HD�AHDӀ D�� D�HD�@ DԁHD�� D���D�@ D�~�Dվ�D�  D�@ D�~�D־�D�  D�AHD׀ D׾�D���D�>�D؀ D��HD�HD�B�DفHD��HD�  D�@ Dڀ D�� D�  D�@ DہHD�D�HD�>�D܀ D��HD�  D�@ D݁HD�D�  D�@ DށHD��HD�  D�AHD߁HD��HD�HD�>�D�� DྸD�  D�>�D� D�� D�  D�@ D�~�D��HD�HD�@ D� D�� D�  D�=qD�~�D�� D���D�@ D�HD徸D�  D�@ D�HD澸D���D�>�D�HD��HD�  D�@ D� D��HD��D�@ D�}qD�qD��qD�>�D� D�� D�  D�AHD� D뾸D�HD�@ D� D�� D�  D�AHD�HD���D�  D�AHD�HD�qD�  D�@ D�~�D�� D�HD�AHD���D��HD�  D�@ D� D�� D�HD�>�D�~�D�� D�  D�B�D�HD�D�  D�@ D� D�� D�HD�@ D�~�D�� D�HD�B�D��HD��HD���D�>�D��HD�� D���D�>�D�� D�� D���D�@ D�� D�� D�  D�AHG�O�>�G�?.{?u?�33?��@�@+�@J=q@c�
@��\@���@�p�@��@�(�@˅@�Q�@�ff@�
=A�\A	��A��A�A\)A'
=A.{A5�A;�AC33AK�AQ�AX��A_\)AfffAmp�As33Ax��A~{A�=qA��A�\)A���A�(�A�ffA�Q�A�=qA�(�A�ffA�Q�A��A��
A�{A�Q�A��A��A�A�  A���A�33A�p�A��A�G�A�33A�p�A�\)A�G�A��HA���A�
=A�G�A��HA���AƸRA�G�A�33A���AθRA���A�33A���AָRA���AۅA��A޸RA��A�33A�p�A�
=A���A�33A�p�A�\)A�G�A�33A�A�  A���A��
A�{B (�B ��B{B
=B(�B��B�B33B(�B��B	�B
=B(�B��B�B�HB  B��BB�RB�
B��B��B�\B�B��Bp�B=qB33BQ�B�B{B�HB   B ��B!B"�\B#�B$��B%��B&=qB'33B((�B)G�B*=qB+33B,  B,��B-B.�HB/�
B0��B1��B2ffB3�B4��B5p�B6=qB7
=B8  B8��B9�B:�HB;�B<z�B=p�B>�\B?\)B@Q�BA�BA�BC
=BD(�BEG�BF{BF�HBG�
BH��BI�BK
=BL(�BMG�BN{BO
=BPQ�BQp�BR�RBS�
BT��BV{BW
=BXQ�BY��BZ�HB\(�B]�B^=qB_�B`��Ba�Bc33Bd��Be�Bg
=Bh(�Bip�Bj�HBl  Bmp�Bn�RBp(�Bq��Bs
=BtQ�Bu��Bw
=BxQ�ByB{33B|��B~{B�B�z�B�33B��
B��\B�33B��
B�z�B�33B��
B��\B�G�B�  B��RB�p�B�=qB��HB���B�Q�B�
=B�B��\B�G�B�{B���B�B���B�p�B�=qB�
=B��
B��RB��B�=qB�
=B��
B�z�B�33B��
B�z�B���B��B�{B��\B��B���B�  B�z�B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB��\B���B�\)B��
B�(�B��\B���B�p�B��
B�=qB���B�
=B��B�  B�ffB���B�33B��B�{B�z�B���B�\)B��
B�=qB���B��B��B��B�ffB���B��B��B�  B�ffB���B�G�B��B�(�B���B�
=B�p�B��B�ffB��HB�33B��B�(�B��\B���B�p�B��
B�=qB���B��B��B��B�ffB���B�G�B��B�{B��\B���B�\)B��
B�=qB��RB��B��B�{B�z�B���B�\)B��
B�=qBĸRB��Bř�B�  B�z�B��HB�G�B�B�(�Bȣ�B��Bə�B��B�Q�BʸRB��B˅B��B�Q�B���B�G�B�B�(�BΣ�B��BυB�  B�z�B��HB�\)B��B�ffBҸRB�G�BӮB�(�Bԣ�B��BՅB��B�ffB��HB�\)B��
B�Q�B��HB�\)B��
B�Q�B���B�G�B��
B�=qB���B�G�B�B�=qB޸RB�G�B߮B�=qB�RB�G�B�B�=qB��HB�p�B�  B�z�B�
=B�B�{B�\B�
=B癚B�{B��B�33B�B�=qB���B�\)B��
B�ffB��HB�\)B��
B�ffB��HB�\)B��B�z�B��B�B�{B�\B�
=B�B�=qB���B�\)B��B�z�B��HB�\)B��B��\B�33B�B�=qB��RB�G�B��
B�ffB���B���B�(�B��RB�33B�C �C ffC �RC  CG�C�\C��C{CQ�C�\C�
C(�Cp�C�RC��C=qCz�CC
=CQ�C��C��C=qC�C��C
=CQ�C��C�C=qC�\C�
C	{C	\)C	��C	��C
G�C
��C
�HC(�CffC�RC
=C\)C�C�C33C�\C�HC(�CffC�RC{CffC�C  CG�C��C��C=qC�C�HC=qC�C��C�Cz�C��C{C\)C�RC
=C\)C��C�CG�C��C�C=qC�\C�HC=qC�C�
C(�C�C�
C(�Cp�C�RC{Cp�CC�Cp�CC
=CffC�RC�Cp�C�RC
=CffCC {C ffC �RC!{C!p�C!��C"�C"p�C"C#(�C#�C#�
C$�C$z�C$�
C%33C%�\C%�HC&33C&z�C&�
C'33C'�\C'�HC(33C(�\C(�C)Q�C)��C)��C*G�C*��C+
=C+ffC+�RC,{C,ffC,C-�C-z�C-��C.�C.z�C.�HC/(�C/z�C/��C033C0�C0�HC1(�C1z�C1�
C2=qC2�C2��C3(�C3�\C3�HC433C4�C4�HC5G�C5��C5�C6=qC6��C6��C7Q�C7��C7�C8G�C8�C9  C9G�C9��C9��C:\)C:�C;  C;G�C;��C<  C<Q�C<��C<�C==qC=��C=��C>33C>�\C>�C?=qC?�\C?�
C@33C@�\C@�HCA33CA�CA�HCB=qCB�\CB�
CC(�CC�\CC�CD33CD�CD�HCE=qCE�\CE�
CF(�CF�CF�HCG33CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A� �A� �A��A� �A� �A��A��A��A�(�A�&�A�&�A�(�A�(�A�$�A��A�JA��yA�hA�oA��A���A�VA�G�A�p�A�~�A�XA�XAϧ�Aʲ-A��`A�\)A���A��#A�7LA�VA�^5A�p�A�-A�t�A�1'A�-A���A�z�A���A���A�I�A��A�VA�  A���A���A�ȴA�x�A���A|M�Aw��Ap^5Ai��Ag�hAf�RAe�AbA�A]/AZ�AZ=qAZ��AY��AWAP��AN�AKG�AE�wADQ�AC��ACx�AB��AA�
A@�A=�A<�A<�A;�A;dZA;O�A;?}A:�`A:�A:�A9��A9t�A9/A8��A8=qA8{A7�^A7x�A733A6�uA5�A3�PA2�HA2-A/O�A*1'A(A�A'l�A'�A'dZA'S�A'+A&�A&��A&�A'"�A'�A'A&�HA&~�A&(�A&-A&$�A& �A&$�A&$�A& �A&JA%�A%`BA$�A$�9A$�!A$�\A$=qA#�^A#O�A#A"�jA"��A"�\A"z�A" �A!�wA!hsA!+A!A bNA�^AC�AoA�A��AVA5?AbA��A�^AhsA&�A�A�HA�/A��Ar�A1'A��At�AVA�HAȴA�\A~�AE�A��A;dA�A1A��Al�A�Az�A�AjAQ�A �A�TA��AC�A�`A�A�#A��At�A33A"�AA�RA�A�TAoA�HA��Ar�AE�A��A��A�A��A��A�AO�A�A9XA��AA�7A\)A%AĜA�+AZA��AG�AVA
��A
�9A
�\A
~�A
bNA	|�A�`A��Az�A^5A�AA�A�7A?}A�A�/An�A��Al�A�AĜAVA�A�A�hA;dAVAȴA��A�\Av�AffAI�A�A��AS�A&�A �/A ��A -@��y@��@��^@��h@�hs@��9@��@��m@�b@�1@�t�@�v�@���@�G�@�Ĝ@�z�@�bN@�1@�K�@���@�v�@��@�hs@���@�l�@�+@��@�{@�X@��@�@��@�\)@��@��@@�&�@�Z@�  @땁@�S�@�@�^@�D@�Q�@�w@��H@�~�@�J@�O�@�%@�A�@��m@�P@�"�@�{@�`B@���@��/@�Ĝ@��@�r�@�Z@�A�@��m@�S�@��H@�ff@ݲ-@���@�t�@ڗ�@�J@ٙ�@�/@؃@���@�l�@�=q@�@ՙ�@�X@��@�1'@���@�\)@ҟ�@�hs@��@���@�Z@�1@�C�@���@��#@�/@��/@���@���@̛�@�Q�@˾w@�S�@�33@�
=@��y@�n�@��#@���@�dZ@�^5@�J@�x�@�&�@�%@��`@�Ĝ@ě�@�r�@�dZ@°!@��@�p�@�?}@��@�(�@�|�@��R@�J@��7@�V@���@�A�@�33@�V@��@�=q@�{@��-@�`B@���@�I�@��
@�dZ@��H@�ff@�-@�{@���@��7@�hs@�O�@��`@��u@��@���@���@�|�@�@���@���@�p�@�&�@�V@�%@��@���@��m@���@�ƨ@��F@�+@���@�ȴ@�ȴ@���@���@���@�~�@���@�V@��u@�1'@�1@�ƨ@�\)@�
=@��y@�ȴ@�^5@��@��#@�@�G�@��j@�j@��@��F@�\)@�"�@�~�@�-@�J@���@���@�G�@���@�1'@�b@�  @��;@��@�"�@�ȴ@��+@�M�@�-@�@��#@���@��@�Ĝ@��9@�Z@��m@�+@��R@�n�@�-@��@�J@��#@�x�@�O�@���@�Q�@��w@�|�@�l�@�\)@�33@��!@��@��@���@��@�G�@�7L@��@�r�@��F@�+@��R@���@��+@�n�@�x�@��/@��u@�  @�|�@�C�@��H@�^5@�$�@��@���@�x�@�/@�V@��`@��@��;@���@��@��@��!@��\@�E�@��#@���@�G�@���@��9@��@���@�r�@�Q�@�(�@���@���@�dZ@�S�@�K�@�;d@���@��#@��-@�G�@��@�A�@�9X@� �@�b@��@��;@��
@���@�;d@�o@��H@�M�@��T@�@���@�p�@�O�@�&�@�r�@�b@�\)@��@�@��!@�n�@�n�@�ff@�E�@�J@��#@���@�`B@�?}@���@��@�Z@�  @�P@;d@~�@~5?@}��@}/@|��@|�j@|�D@|z�@{��@{33@y��@yhs@y%@x�9@xbN@x1'@wl�@v�y@v�@v�@vȴ@v�+@v$�@u�-@t��@t�@s��@s@rn�@q��@q�@p��@p �@o\)@n�@nE�@m@m�@m�@l��@l�/@l�@k��@k33@j�!@j=q@i��@h�`@hQ�@g�;@g�P@f�@f��@e�h@d�@d9X@d1@c��@cƨ@c��@cS�@c@b�\@a�#@aG�@`��@`bN@`Q�@`Q�@_�;@_K�@_+@^�@^ff@]�@\��@\��@\9X@\�@[ƨ@[dZ@[o@Z��@Z�!@Z�@ZJ@Y��@Yhs@Y&�@X�9@Xr�@X �@W�w@W;d@V��@V5?@V{@U��@U�h@Up�@U`B@UO�@UO�@U?}@T�@T�D@S��@S��@R�@Q��@Q��@QX@Q�@P�`@PĜ@P�@P �@O�w@Ol�@O+@O�@N�y@N��@N��@Nv�@Nff@M�@Mp�@M`B@M?}@M�@L�/@Lz�@K�m@KC�@J�@J�!@J��@J��@J��@J�\@J-@I�7@IG�@I7L@I7L@I7L@I&�@I%@H��@HA�@G��@G|�@G;d@F��@F�y@F��@Fv�@E�T@E@E��@E�@E/@D�@D�/@D�j@Dz�@D1@CC�@Co@C@B�@B�@B�H@B��@B�!@B~�@B-@A�7@A&�@@bN@?�@?
=@=�T@=�@<��@<j@<j@<j@<j@<j@<Z@;�
@;�@;33@:��@:��@:M�@9��@9X@97L@8��@8�@8bN@81'@7�w@7��@7�P@7�P@7�P@7\)@6��@6��@6ff@6{@5�h@5O�@4�j@4�D@49X@3�m@3��@3��@3S�@2�@2��@2n�@2=q@1�#@17L@0�`@0��@0�u@0�@0bN@0  @/;d@.�R@.v�@.{@-�-@-�@,�@,�/@,�@,I�@,1@+�m@+ƨ@+�F@+��@+@*�@*��@*�!@*�\@*~�@*~�@*~�@*^5@*�@)��@)X@)�@(Ĝ@(�u@( �@(  @'�@'�@'|�@'K�@&��@&V@&$�@&@%�@%�T@%�-@%�@%O�@%V@$�@$j@$(�@$1@$1@#��@#�m@#�
@#t�@#S�@#C�@#C�@#33@"��@"~�@"�@!�@!�#@!��@!hs@!hs@!hs@ Ĝ@ �@ �u@   @|�@l�@l�@\)@K�@;d@;d@+@�@
=@��@�y@�y@�+@{@��@��@�@�@�@�/@��@��@j@j@Z@(�@�
@��@��@��@�@@��@��@��@X@7L@%@Ĝ@��@�@Q�@  @�;@;d@��@�@��@��@5?@�-@�@O�@��@��@Z@I�@�@��@�
@�F@��@��@�@dZ@S�@��@�!@�\@~�@~�@n�@=q@��@��@��@�7@x�@hs@&�@��@�u@�u@r�@bN@Q�@1'@ �@b@b@  @�A�A��A��A��A��A��A�{A��A��A� �A��A��A�"�A�"�A� �A��A��A� �A� �A��A��A�"�A��A�$�A��A��A��A� �A�$�A�&�A�$�A�&�A�+A�(�A�$�A�$�A�&�A�(�A�&�A�$�A�&�A�(�A�(�A�$�A�$�A�(�A�+A�&�A�$�A�(�A�+A�(�A�$�A�(�A�+A�(�A�&�A�$�A�+A�+A�&�A�&�A�-A�(�A�$�A�$�A�&�A�&�A�"�A� �A�"�A�$�A�$�A� �A��A� �A�"�A��A��A��A��A��A�{A�oA�
=A�A�  A���A���A�  A���A��A��;A��/A���A���A�FA�A��A晚A�7A�v�A�`BA�C�A�/A�(�A��A�oA�%A���A��TA���A�A�jA�^A��A嗍A�DA�x�A�t�A�bNA�C�A��A�ȴA�A�r�A�hsA�ffA�S�A�9XA�+A�"�A��A�A��mA��/A�ȴA�FA��A�\A�r�A�E�A��A��A��A�VA�-A�{A��A���A�A�$�A���A���A�VA�{A��mA޾wAޏ\A�`BA�{A�Aݝ�A�v�A�C�A��#A�VA�v�A��Aٝ�A�&�Aة�A�33A֥�A�S�A� �A�C�AӑhA��HA�dZA�1A�\)A���Aϡ�A�%A�C�A�O�A�\)A�hsAʙ�A�I�Aɺ^A�~�A�A�A�1A�A��yA���AȼjAȰ!Aȟ�AȃA�x�A�v�A�dZA�(�AǺ^A�z�Aç�A�M�A�oA�ȴA���A��DA��A���A�E�A��A�r�A�Q�A�I�A�5?A�1A�ƨA�p�A�bA�A���A��HA���A�r�A�G�A���A�{A��hA��\A��wA�&�A���A�9XA��/A�$�A�VA��
A���A�  A��A��PA��+A�r�A�A��A�;dA�"�A��A��!A�bNA�^5A�bA���A���A�hsA�A�A�33A��`A�bNA���A�7LA���A��hA�v�A�dZA�%A�ĜA��FA���A��PA�^5A�C�A�&�A�A��`A���A��A�dZA�?}A��A���A�A���A�S�A�
=A�A�r�A�+A��/A��PA�1'A��A���A��^A���A��\A�v�A�dZA�ZA�K�A�=qA��A��A��;A���A���A��uA�Q�A�bA��wA���A�\)A��A��HA��-A��A�=qA�
=A���A��uA�l�A�VA�/A��`A���A�M�A��A��
A���A�;dA���A���A���A���A�?}A���A�-A�  A���A��A�33A�
=A��A��A�`BA�-A��A�p�A��A�ȴA�XA��A��`A��A��^A���A�n�A�-A��A���A�E�A���A�A�A��A�VA���A��;A���A��+A�9XA���A��FA�$�A��;A���A�v�A�E�AK�A~jA|��A|ZA{��A{l�A{/Az�Az��Az^5Ay�Ax��AwƨAw+Av�AvVAu�7At��AsC�Ar�ArbAo��An�\Amt�Al��Ak�^AkAjVAi�;AihsAiAh��Ah�Ah�Ag�Ag�wAg��AgG�Ag�AgoAf��Af�Af�Af��Af�RAf��Afz�AfffAfVAfbAe��Ad�yAd�+AdjAd-Ad{AdbAc��AcdZAb��Aa��A`��A_ƨA^��A^^5A]�TA]x�A]VA\��A[�wA[x�A[XA[G�A[?}A[/A[oAZ�yAY�mAY�AY��AZ�AZM�AZ�AZ��AZ�RAZȴAZ�/AZ�/AZ�HAZ�AZȴAZ�9AZ��AZ�AZbNAZAY��AY��AY�AYt�AYhsAY/AX��AXAWx�AV�uAU�AT �AR��AQ��AQ�APM�AO��AO
=AN��AN��ANv�ANI�AN�AM�-AM;dAL��ALM�AL$�AK�AK�AJ�`AHȴAGAF�/AE�AE`BAD�yAD��AD�RAD��ADv�AD^5AD9XAD(�AD�ADbAC�ACp�AC��AC/ACt�AC��AC�AC��AC�7ACXACO�AC;dAC&�AC
=AB��AB�yAB�`ABĜAB^5AA��AA�TAA�#AA��AA��AAl�AA&�A@��A@A�A?�FA?/A>��A>ȴA>n�A>  A=K�A<��A<��A<��A<��A<��A<�+A<r�A<VA<9XA<$�A<�A<{A<bA<1A<A;��A;��A;�A;�A;�A;�wA;l�A;`BA;XA;S�A;O�A;K�A;O�A;S�A;O�A;O�A;K�A;K�A;O�A;O�A;K�A;/A;oA:��A:�A:�A:�HA:��A:�jA:��A:��A:�+A:v�A:ZA:M�A:=qA:$�A:bA:1A:  A:  A9�mA9��A9�wA9�-A9��A9��A9��A9�A9hsA9XA9O�A9C�A97LA933A9/A9&�A9�A8��A8��A8�`A8��A8��A8v�A8ffA8M�A81'A8(�A8$�A8$�A8(�A8$�A8�A8JA8A7��A7�A7�TA7��A7�FA7�A7l�A7hsA7l�A7|�A7�A7�7A7�A7hsA7\)A7/A7�A7
=A7A6��A6�9A6�+A6v�A6r�A6bNA6=qA5��A5��A5\)A5oA4�DA4A3�#A3��A3C�A3"�A3VA2��A2�`A2�/A2�A2��A2�A2��A2�9A2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                       A��A��A��A��A� �A� �A��A� �A� �A��A��A��A�(�A�&�A�&�A�(�A�(�A�$�A��A�JA��yA�hA�oA��A���A�VA�G�A�p�A�~�A�XA�XAϧ�Aʲ-A��`A�\)A���A��#A�7LA�VA�^5A�p�A�-A�t�A�1'A�-A���A�z�A���A���A�I�A��A�VA�  A���A���A�ȴA�x�A���A|M�Aw��Ap^5Ai��Ag�hAf�RAe�AbA�A]/AZ�AZ=qAZ��AY��AWAP��AN�AKG�AE�wADQ�AC��ACx�AB��AA�
A@�A=�A<�A<�A;�A;dZA;O�A;?}A:�`A:�A:�A9��A9t�A9/A8��A8=qA8{A7�^A7x�A733A6�uA5�A3�PA2�HA2-A/O�A*1'A(A�A'l�A'�A'dZA'S�A'+A&�A&��A&�A'"�A'�A'A&�HA&~�A&(�A&-A&$�A& �A&$�A&$�A& �A&JA%�A%`BA$�A$�9A$�!A$�\A$=qA#�^A#O�A#A"�jA"��A"�\A"z�A" �A!�wA!hsA!+A!A bNA�^AC�AoA�A��AVA5?AbA��A�^AhsA&�A�A�HA�/A��Ar�A1'A��At�AVA�HAȴA�\A~�AE�A��A;dA�A1A��Al�A�Az�A�AjAQ�A �A�TA��AC�A�`A�A�#A��At�A33A"�AA�RA�A�TAoA�HA��Ar�AE�A��A��A�A��A��A�AO�A�A9XA��AA�7A\)A%AĜA�+AZA��AG�AVA
��A
�9A
�\A
~�A
bNA	|�A�`A��Az�A^5A�AA�A�7A?}A�A�/An�A��Al�A�AĜAVA�A�A�hA;dAVAȴA��A�\Av�AffAI�A�A��AS�A&�A �/A ��A -@��y@��@��^@��h@�hs@��9@��@��m@�b@�1@�t�@�v�@���@�G�@�Ĝ@�z�@�bN@�1@�K�@���@�v�@��@�hs@���@�l�@�+@��@�{@�X@��@�@��@�\)@��@��@@�&�@�Z@�  @땁@�S�@�@�^@�D@�Q�@�w@��H@�~�@�J@�O�@�%@�A�@��m@�P@�"�@�{@�`B@���@��/@�Ĝ@��@�r�@�Z@�A�@��m@�S�@��H@�ff@ݲ-@���@�t�@ڗ�@�J@ٙ�@�/@؃@���@�l�@�=q@�@ՙ�@�X@��@�1'@���@�\)@ҟ�@�hs@��@���@�Z@�1@�C�@���@��#@�/@��/@���@���@̛�@�Q�@˾w@�S�@�33@�
=@��y@�n�@��#@���@�dZ@�^5@�J@�x�@�&�@�%@��`@�Ĝ@ě�@�r�@�dZ@°!@��@�p�@�?}@��@�(�@�|�@��R@�J@��7@�V@���@�A�@�33@�V@��@�=q@�{@��-@�`B@���@�I�@��
@�dZ@��H@�ff@�-@�{@���@��7@�hs@�O�@��`@��u@��@���@���@�|�@�@���@���@�p�@�&�@�V@�%@��@���@��m@���@�ƨ@��F@�+@���@�ȴ@�ȴ@���@���@���@�~�@���@�V@��u@�1'@�1@�ƨ@�\)@�
=@��y@�ȴ@�^5@��@��#@�@�G�@��j@�j@��@��F@�\)@�"�@�~�@�-@�J@���@���@�G�@���@�1'@�b@�  @��;@��@�"�@�ȴ@��+@�M�@�-@�@��#@���@��@�Ĝ@��9@�Z@��m@�+@��R@�n�@�-@��@�J@��#@�x�@�O�@���@�Q�@��w@�|�@�l�@�\)@�33@��!@��@��@���@��@�G�@�7L@��@�r�@��F@�+@��R@���@��+@�n�@�x�@��/@��u@�  @�|�@�C�@��H@�^5@�$�@��@���@�x�@�/@�V@��`@��@��;@���@��@��@��!@��\@�E�@��#@���@�G�@���@��9@��@���@�r�@�Q�@�(�@���@���@�dZ@�S�@�K�@�;d@���@��#@��-@�G�@��@�A�@�9X@� �@�b@��@��;@��
@���@�;d@�o@��H@�M�@��T@�@���@�p�@�O�@�&�@�r�@�b@�\)@��@�@��!@�n�@�n�@�ff@�E�@�J@��#@���@�`B@�?}@���@��@�Z@�  @�P@;d@~�@~5?@}��@}/@|��@|�j@|�D@|z�@{��@{33@y��@yhs@y%@x�9@xbN@x1'@wl�@v�y@v�@v�@vȴ@v�+@v$�@u�-@t��@t�@s��@s@rn�@q��@q�@p��@p �@o\)@n�@nE�@m@m�@m�@l��@l�/@l�@k��@k33@j�!@j=q@i��@h�`@hQ�@g�;@g�P@f�@f��@e�h@d�@d9X@d1@c��@cƨ@c��@cS�@c@b�\@a�#@aG�@`��@`bN@`Q�@`Q�@_�;@_K�@_+@^�@^ff@]�@\��@\��@\9X@\�@[ƨ@[dZ@[o@Z��@Z�!@Z�@ZJ@Y��@Yhs@Y&�@X�9@Xr�@X �@W�w@W;d@V��@V5?@V{@U��@U�h@Up�@U`B@UO�@UO�@U?}@T�@T�D@S��@S��@R�@Q��@Q��@QX@Q�@P�`@PĜ@P�@P �@O�w@Ol�@O+@O�@N�y@N��@N��@Nv�@Nff@M�@Mp�@M`B@M?}@M�@L�/@Lz�@K�m@KC�@J�@J�!@J��@J��@J��@J�\@J-@I�7@IG�@I7L@I7L@I7L@I&�@I%@H��@HA�@G��@G|�@G;d@F��@F�y@F��@Fv�@E�T@E@E��@E�@E/@D�@D�/@D�j@Dz�@D1@CC�@Co@C@B�@B�@B�H@B��@B�!@B~�@B-@A�7@A&�@@bN@?�@?
=@=�T@=�@<��@<j@<j@<j@<j@<j@<Z@;�
@;�@;33@:��@:��@:M�@9��@9X@97L@8��@8�@8bN@81'@7�w@7��@7�P@7�P@7�P@7\)@6��@6��@6ff@6{@5�h@5O�@4�j@4�D@49X@3�m@3��@3��@3S�@2�@2��@2n�@2=q@1�#@17L@0�`@0��@0�u@0�@0bN@0  @/;d@.�R@.v�@.{@-�-@-�@,�@,�/@,�@,I�@,1@+�m@+ƨ@+�F@+��@+@*�@*��@*�!@*�\@*~�@*~�@*~�@*^5@*�@)��@)X@)�@(Ĝ@(�u@( �@(  @'�@'�@'|�@'K�@&��@&V@&$�@&@%�@%�T@%�-@%�@%O�@%V@$�@$j@$(�@$1@$1@#��@#�m@#�
@#t�@#S�@#C�@#C�@#33@"��@"~�@"�@!�@!�#@!��@!hs@!hs@!hs@ Ĝ@ �@ �u@   @|�@l�@l�@\)@K�@;d@;d@+@�@
=@��@�y@�y@�+@{@��@��@�@�@�@�/@��@��@j@j@Z@(�@�
@��@��@��@�@@��@��@��@X@7L@%@Ĝ@��@�@Q�@  @�;@;d@��@�@��@��@5?@�-@�@O�@��@��@Z@I�@�@��@�
@�F@��@��@�@dZ@S�@��@�!@�\@~�@~�@n�@=q@��@��@��@�7@x�@hs@&�@��@�u@�u@r�@bN@Q�@1'@ �@b@b@  G�O�A�A��A��A��A��A��A�{A��A��A� �A��A��A�"�A�"�A� �A��A��A� �A� �A��A��A�"�A��A�$�A��A��A��A� �A�$�A�&�A�$�A�&�A�+A�(�A�$�A�$�A�&�A�(�A�&�A�$�A�&�A�(�A�(�A�$�A�$�A�(�A�+A�&�A�$�A�(�A�+A�(�A�$�A�(�A�+A�(�A�&�A�$�A�+A�+A�&�A�&�A�-A�(�A�$�A�$�A�&�A�&�A�"�A� �A�"�A�$�A�$�A� �A��A� �A�"�A��A��A��A��A��A�{A�oA�
=A�A�  A���A���A�  A���A��A��;A��/A���A���A�FA�A��A晚A�7A�v�A�`BA�C�A�/A�(�A��A�oA�%A���A��TA���A�A�jA�^A��A嗍A�DA�x�A�t�A�bNA�C�A��A�ȴA�A�r�A�hsA�ffA�S�A�9XA�+A�"�A��A�A��mA��/A�ȴA�FA��A�\A�r�A�E�A��A��A��A�VA�-A�{A��A���A�A�$�A���A���A�VA�{A��mA޾wAޏ\A�`BA�{A�Aݝ�A�v�A�C�A��#A�VA�v�A��Aٝ�A�&�Aة�A�33A֥�A�S�A� �A�C�AӑhA��HA�dZA�1A�\)A���Aϡ�A�%A�C�A�O�A�\)A�hsAʙ�A�I�Aɺ^A�~�A�A�A�1A�A��yA���AȼjAȰ!Aȟ�AȃA�x�A�v�A�dZA�(�AǺ^A�z�Aç�A�M�A�oA�ȴA���A��DA��A���A�E�A��A�r�A�Q�A�I�A�5?A�1A�ƨA�p�A�bA�A���A��HA���A�r�A�G�A���A�{A��hA��\A��wA�&�A���A�9XA��/A�$�A�VA��
A���A�  A��A��PA��+A�r�A�A��A�;dA�"�A��A��!A�bNA�^5A�bA���A���A�hsA�A�A�33A��`A�bNA���A�7LA���A��hA�v�A�dZA�%A�ĜA��FA���A��PA�^5A�C�A�&�A�A��`A���A��A�dZA�?}A��A���A�A���A�S�A�
=A�A�r�A�+A��/A��PA�1'A��A���A��^A���A��\A�v�A�dZA�ZA�K�A�=qA��A��A��;A���A���A��uA�Q�A�bA��wA���A�\)A��A��HA��-A��A�=qA�
=A���A��uA�l�A�VA�/A��`A���A�M�A��A��
A���A�;dA���A���A���A���A�?}A���A�-A�  A���A��A�33A�
=A��A��A�`BA�-A��A�p�A��A�ȴA�XA��A��`A��A��^A���A�n�A�-A��A���A�E�A���A�A�A��A�VA���A��;A���A��+A�9XA���A��FA�$�A��;A���A�v�A�E�AK�A~jA|��A|ZA{��A{l�A{/Az�Az��Az^5Ay�Ax��AwƨAw+Av�AvVAu�7At��AsC�Ar�ArbAo��An�\Amt�Al��Ak�^AkAjVAi�;AihsAiAh��Ah�Ah�Ag�Ag�wAg��AgG�Ag�AgoAf��Af�Af�Af��Af�RAf��Afz�AfffAfVAfbAe��Ad�yAd�+AdjAd-Ad{AdbAc��AcdZAb��Aa��A`��A_ƨA^��A^^5A]�TA]x�A]VA\��A[�wA[x�A[XA[G�A[?}A[/A[oAZ�yAY�mAY�AY��AZ�AZM�AZ�AZ��AZ�RAZȴAZ�/AZ�/AZ�HAZ�AZȴAZ�9AZ��AZ�AZbNAZAY��AY��AY�AYt�AYhsAY/AX��AXAWx�AV�uAU�AT �AR��AQ��AQ�APM�AO��AO
=AN��AN��ANv�ANI�AN�AM�-AM;dAL��ALM�AL$�AK�AK�AJ�`AHȴAGAF�/AE�AE`BAD�yAD��AD�RAD��ADv�AD^5AD9XAD(�AD�ADbAC�ACp�AC��AC/ACt�AC��AC�AC��AC�7ACXACO�AC;dAC&�AC
=AB��AB�yAB�`ABĜAB^5AA��AA�TAA�#AA��AA��AAl�AA&�A@��A@A�A?�FA?/A>��A>ȴA>n�A>  A=K�A<��A<��A<��A<��A<��A<�+A<r�A<VA<9XA<$�A<�A<{A<bA<1A<A;��A;��A;�A;�A;�A;�wA;l�A;`BA;XA;S�A;O�A;K�A;O�A;S�A;O�A;O�A;K�A;K�A;O�A;O�A;K�A;/A;oA:��A:�A:�A:�HA:��A:�jA:��A:��A:�+A:v�A:ZA:M�A:=qA:$�A:bA:1A:  A:  A9�mA9��A9�wA9�-A9��A9��A9��A9�A9hsA9XA9O�A9C�A97LA933A9/A9&�A9�A8��A8��A8�`A8��A8��A8v�A8ffA8M�A81'A8(�A8$�A8$�A8(�A8$�A8�A8JA8A7��A7�A7�TA7��A7�FA7�A7l�A7hsA7l�A7|�A7�A7�7A7�A7hsA7\)A7/A7�A7
=A7A6��A6�9A6�+A6v�A6r�A6bNA6=qA5��A5��A5\)A5oA4�DA4A3�#A3��A3C�A3"�A3VA2��A2�`A2�/A2�A2��A2�A2��A2�9A2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�vB��B�}B�BбB҉BбB�vB�NBϫB��B��B��BרBרB�yB�EB��B��B	FB	<B	h
B	s�B	v�B	��B	�(B	�:B	��B	��B	��B	��B	�@B	��B	�4B	�rB	�[B	��B	�B	�qB	�LB	�WB
*eB
|B
�oB
�bB
~�B
�B
�B
zxB
j�B
bB
PHB
?}B
$�B
B	�KB	ʌB	��B	�FB	�B	v�B	`�B	Y�B	T�B	T�B	VB	G�B	<jB	d�B	��B	�tB	��B	�B	ȀB	�9B	��B	�B	��B	�tB	�B	�]B	��B	�mB	�EB	�`B	�B
#�B
+B
-CB
<jB
HKB
S�B
`BB
e,B
iyB
qB
v`B
w�B
zDB
�fB
�DB
��B
��B
��B
~(B
x�B
kQB
6�B
B'B
J�B
[�B
a�B
g8B
p;B
tB
{B
�B
��B
��B
�:B
��B
�kB
�B
��B
��B
��B
�B
��B
�LB
�B
��B
��B
��B
�B
��B
�B
��B
��B
�B
�B
��B
�B
��B
�FB
�B
��B
�tB
�B
��B
�hB
�'B
��B
��B
��B
�=B
�B
��B
��B
�0B
��B
��B
��B
��B
�B
�B
�RB
��B
�zB
��B
�:B
�bB
��B
��B
��B
��B
��B
�CB
��B
��B
��B
� B
�(B
�bB
�"B
�\B
��B
��B
�.B
��B
��B
��B
��B
�xB
��B
��B
�B
�uB
��B
��B
� B
~�B
�;B
y>B
wfB
v�B
u�B
t�B
u�B
s�B
u%B
x8B
w2B
v+B
u�B
oiB
ncB
l�B
l�B
l�B
k�B
j�B
h�B
hsB
gmB
g8B
f�B
e�B
e�B
d�B
cTB
b�B
bNB
bNB
]dB
Z�B
YB
YB
X�B
W�B
V9B
V�B
UgB
T�B
T,B
R�B
Q�B
PB
N<B
MjB
L�B
K�B
I�B
IB
J#B
I�B
IRB
H�B
HB
G�B
GEB
F�B
G�B
G�B
F?B
E9B
D�B
C�B
B�B
A�B
@�B
=�B
=qB
<�B
<�B
;dB
:�B
<�B
=�B
=�B
>�B
;�B
:�B
:^B
9$B
8RB
8�B
8�B
6�B
6zB
6�B
4�B
6�B
3�B
2�B
2aB
2�B
0�B
/�B
/�B
/OB
/OB
.IB
-�B
.B
/�B
-B
,�B
,qB
+�B
+B
+�B
(�B
(�B
($B
'�B
&�B
&�B
%�B
%zB
&B
%B
$�B
$tB
$�B
#�B
"�B
"�B
"hB
!�B
!�B
!�B
!bB
!-B
�B
�B
�B
~B
xB
B
�B
�B
+B
�B
+B
�B
�B
B
�B
�B
�B
�B
�B
!B
�B
VB
�B
B
B
�B
�B
�B
�B
�B
�B
~B
~B
IB
~B
�B
IB
�B
�B
�B
CB
CB
�B
B
B
�B
�B
xB
�B
�B
�B
qB
=B
�B
�B
kB
B
�B
_B
_B
�B
�B
�B
YB
_B
�B
�B
_B
1B
�B
�B
_B
�B
�B
+B
�B
�B
$B
�B
$B
�B
SB
SB
�B
B
�B
�B
YB
�B
�B
YB
�B
�B
�B
SB
SB
B
B
MB
MB
�B
�B
eB
+B
�B
�B
1B
+B
_B
�B
1B
B
B
kB
�B
xB
B
�B
qB
qB
kB
�B
eB
1B
�B
1B
�B
�B
B
B
�B
�B
�B
B
B
B
�B
�B
xB
�B
qB
�B
~B
B
B
OB
�B
�B
�B
�B
�B
�B
�B
 'B
 'B
!�B
!bB
!�B
"hB
"�B
!�B
!�B
"4B
"4B
!�B
!�B
"hB
"hB
"4B
"�B
#�B
$�B
$�B
$�B
$�B
$tB
&LB
&�B
'RB
'�B
(�B
(�B
(�B
(�B
*�B
+kB
,B
,�B
,B
,B
+�B
-�B
-B
-CB
.IB
.�B
.B
/OB
/�B
/�B
/�B
0UB
0UB
0�B
0�B
0�B
1'B
0�B
1[B
1�B
0�B
1'B
0UB
0UB
0�B
0�B
1�B
2aB
2-B
2-B
2-B
2aB
2aB
2�B
2�B
3�B
3�B
3�B
3hB
2�B
5B
5�B
5B
6zB
7�B
7�B
7LB
7�B
7�B
7�B
7�B
7�B
8RB
8RB
8�B
8RB
:*B
9�B
9�B
9�B
:*B
9�B
:^B
<6B
<B
>BB
>BB
>�B
@B
@�B
@�B
@�B
AUB
A�B
A�B
B[B
B[B
B[B
B�B
C-B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E9B
E�B
EmB
EB
E�B
F?B
GzB
HB
HB
HKB
H�B
H�B
J#B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K^B
K�B
L�B
L�B
L�B
M�B
NB
N<B
N�B
N�B
OB
OBB
OB
OB
PB
PB
P}B
P}B
QB
P�B
Q�B
R B
R B
R B
R�B
R B
S�B
S�B
TaB
T,B
TaB
TaB
T�B
TaB
T�B
T�B
U�B
U�B
VB
V9B
VB
U�B
V�B
V�B
V9B
V�B
V�B
XEB
XB
X�B
YB
YB
YKB
Y�B
ZB
Y�B
ZB
Z�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
\�B
\)B
\]B
\]B
\�B
\]B
\�B
\)B
\)B
\]B
\]B
]/B
\�B
^B
^�B
^5B
^�B
^�B
_B
^�B
_B
_�B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`vB
`vB
`vB
`vB
`BB
`�B
`vB
aHB
a|B
aB
aHB
aHB
`�B
`�B
`�B
a|B
a�B
a�B
a�B
a�B
a�B
a|B
a�B
_;B
bNB
b�B
b�B
c B
c B
b�B
cTB
c B
c�B
c�B
c�B
c�B
d&B
d&B
d&B
d&B
dZB
d�B
e�B
e,B
e`B
e`B
e`B
e,B
e`B
e`B
e`B
e`B
f2B
e�B
f�B
gmB
h
B
h�B
h�B
iyB
i�B
i�B
iyB
iyB
iyB
iDB
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
k�B
lWB
l"B
lWB
lWB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m]B
m�B
m�B
n/B
n�B
n�B
o5B
oiB
oiB
oiB
o�B
o�B
pB
poB
p;B
p�B
qB
qAB
qAB
qAB
qB
qB
qvB
q�B
q�B
r|B
r|B
r�B
sB
sMB
sMB
sMB
s�B
sB
s�B
s�B
s�B
s�B
tB
s�B
tB
tTB
tB
tB
tB
tB
tTB
t�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
wfB
w�B
xB
w�B
y	B
x�B
y	B
y	B
y>B
y	B
y>B
yrB
yrB
y�B
zDB
zDB
z�B
zxB
zxB
zxB
zxB
z�B
{B
{B
z�B
z�B
z�B
{JB
{�B
|B
{B
{�B
|�B
|PB
|PB
{�B
}�B
}VB
|�B
~�B
~�B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�4B
��B
�oB
�oB
�;B
��B
�B
�;B
�;B
��B
��B
�uB
�uB
��B
��B
��B
�uB
�{B
�GB
��B
�MB
�{B
�{B
�B
��B
�B
�MB
�MB
��B
�MB
��B
�B
��B
�%B
��B
�YB
�+B
�_B
��B
�_B
�fB
�7B
�lB
��B
��B
�=B
�=B
�	B
�lB
�lB
�7B
�7B
�DB
�DB
��B
�xB
�xB
�B
�DB
��B
�xB
�JB
�B
�xB
�B
�JB
��B
�PB
��B
�PB
�~B
�B
�B
��B
�PB
��B
��B
��B̘B��B��BуB��B�BB��B��B��B��B�B�B��B� B��B�NB�HBΥB�vB�HB�[BѷB�pB��BҽB�}BбB͟BϫBӏB�aB՛B��B��BרBרB�
B�BخB��BרB�?B�?B�mB֡B��BרB�KB��BخB�sB�yB�B�EB�?B�yBچB�BרB�
B�BخB��B�B�)BیB�B��B�8B�DB�B�B�B��B� B��B�AB��B	�B	�B	B	uB	PB	�B	�B	 �B	&LB	*0B	*eB	*eB	0�B	8�B	F?B	GzB	M6B	QNB	^B	c�B	e�B	h
B	l�B	m�B	qAB	r�B	tB	sMB	rB	q�B	sB	u%B	sB	s�B	tTB	uZB	tTB	v+B	y�B	y	B	yrB	w2B	zxB	�B	��B	��B	��B	��B	��B	��B	�.B	��B	��B	�VB	��B	�\B	�4B	��B	�(B	�VB	��B	��B	�B	��B	��B	��B	��B	�B	��B	�+B	�FB	��B	�B	��B	�B	̘B	�VB	��B	��B	��B	�1B	�xB	�B	�qB	��B	��B	�B	��B	�XB	�aB	�rB	��B	�{B	�	B	��B	��B	� B	�IB	��B	��B	��B	�lB	�{B	�_B	�1B	��B	��B	��B	��B	��B	�OB	��B	��B	�$B	�YB	��B	�$B	�\B	��B	��B	�VB	�B	��B	�B	�_B	�SB	��B	�SB	��B	��B
^�B	�B	��B	��B	�nB	��B	��B	�FB	�B	��B	��B	�qB	�_B	��B	�B	��B	��B	�B	�B	��B	��B	�eB	�XB	�4B	��B	��B	�B	�B	�HB	�`B	�B	�B	��B
�B	�B
-wB
tTB
jB
q�B
m�B
p�B
y�B
�VB
�xB
�oB
�B
� B
��B
��B
��B
��B
�VB
��B
��B
|�B
x8B
v+B
�B
y�B
~(B
�B
y>B
��B
�;B
��B
��B
�B
�B
�%B
��B
��B
��B
�YB
��B
��B
��B
��B
�GB
�uB
��B
��B
cB
�B
}"B
��B
.B
z�B
|B
�4B
x�B
v`B
m�B
p�B
o B
m�B
j�B
m]B
jB
h�B
h�B
n/B
h�B
e,B
c�B
c�B
e`B
d�B
d�B
^B
^�B
^B
YKB
XEB
V9B
VmB
V�B
NpB
R B
NB
GB
A B
FtB
B�B
B[B
GEB
<B
A�B
5B
6�B
4�B
3�B
:*B
.B
 �B
1[B
YB
FB
�B
 \B
 �B
#nB
$tB
B	�B	��B	�cB	��B	��B	��B	�B	��B	ٴB	ԕB	�aB	ӏB	՛B	�B	��B	ϫB	�XB	ܒB	�B	�wB	��B	�cB	ȴB	�6B	��B	�B	��B	�:B	�B	�!B	�'B	��B	��B	��B	��B	�IB	�_B	�"B	��B	�fB	��B	�SB	��B	�B	�_B	�xB	��B	zB	|PB	�{B	��B	uZB	l�B	{�B	��B	k�B	sMB	e�B	h�B	g8B	a�B	^�B	`B	\�B	Z�B	]�B	^�B	YKB	Z�B	Z�B	\)B	VmB	U2B	V�B	T�B	U�B	T�B	T�B	S[B	U�B	R�B	T,B	XyB	Z�B	^�B	R�B	K�B	S&B	M6B	N�B	OvB	a�B	ZQB	YKB	U�B	M�B	I�B	I�B	J�B	FB	I�B	:�B	O�B	9XB	<�B	7�B	7LB	8�B	8RB	:*B	G�B	9$B	;�B	F?B	n/B	v�B	�MB	�B	�=B	�4B	� B	�4B	��B	��B	��B	��B	�=B	��B	��B	��B	��B	��B	�B	�tB	�jB	ɆB	�XB	�zB	�,B	�sB	�HB	��B	�?B	�aB	��B	��B	�B	��B	��B	ȴB	�mB	��B	��B	�B	�RB	�6B	��B	�#B	��B	��B	��B	�UB	��B	��B	�\B	��B	��B	�oB	��B	�@B	�B	��B	��B	�{B	��B	�CB	�tB	�B	��B	�B	�}B	��B	ɆB	�RB	�BB	�#B	�jB	�gB	�/B	�B	ߤB	ߤB	� B	�yB	�B	ںB	�B	ٴB	�B	�2B	��B	��B	��B	�NB	˒B	ǮB	��B	�9B	�B	��B	�}B	��B	�-B	��B	�mB	�gB	��B	�B	�sB	�pB	�,B	�B	�B	�"B	� B	�B	��B	�TB	��B	��B
SB
!-B
&B
(XB
*0B
+6B
+�B
*�B
)�B
*�B
+6B
+�B
+B
+B
)�B
+6B
0�B
6FB
;�B
:�B
;0B
<jB
>B
D3B
E�B
F?B
G�B
H�B
L�B
MB
NB
R B
U2B
W
B
YB
Z�B
]dB
`B
a�B
b�B
b�B
a�B
c B
d�B
gB
gB
f�B
h>B
iyB
iDB
h�B
jB
k�B
o5B
o�B
qB
o5B
v�B
s�B
tTB
u�B
w2B
wfB
w�B
w�B
v`B
v+B
xB
y>B
xB
w�B
w�B
x�B
x�B
zxB
~]B
~(B
�B
��B
��B
�lB
�DB
�JB
��B
�B
�(B
�1B
�7B
�~B
�B
��B
�DB
��B
�lB
�1B
��B
�"B
�PB
� B
��B
�B
��B
�1B
�	B
�+B
��B
�B
�B
� B
}�B
}�B
~(B
|B
|�B
z�B
}�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                       BϤB��B�~B�B��BҢBгBϟB�gB��BӮB��B�B׸B׷B؏B؄B�HB��B	1B	>B	jKB	v	B	{oB	��B	�B	��B	��B	�B	�qB	��B	�B	��B	��B	�uB	�B	��B	�=B	�fB	�B	��B
J0B
��B
��B
�mB
�BB
��B
��B
��B
r�B
n�B
^bB
U�B
G�B
�B	�B	��B	��B	�\B	�5B	��B	h�B	]2B	Z�B	^�B	e�B	O#B	?yB	c�B	�%B	��B	�B	��B	�B	֒B	�B	��B	�B	�OB	��B	�B	۬B	�}B	�6B	��B	�`B
#�B
+IB
.�B
=�B
I�B
T�B
agB
f7B
j�B
r�B
v�B
x�B
{OB
�oB
��B
��B
�TB
�B
�eB
�nB
{�B
=�B
D�B
J�B
\EB
a�B
g�B
q4B
t�B
zhB
�<B
��B
��B
��B
��B
��B
�B
�	B
��B
��B
�B
��B
��B
�SB
��B
�dB
�KB
�2B
�/B
�CB
��B
�aB
�'B
��B
�B
�\B
�B
��B
�uB
��B
�SB
��B
�B
��B
��B
�vB
��B
�sB
�HB
��B
�YB
�&B
�$B
��B
��B
��B
��B
�CB
��B
�B
��B
�iB
��B
��B
�B
��B
��B
��B
�{B
��B
��B
��B
��B
�!B
��B
�8B
��B
�$B
��B
��B
�NB
�B
��B
�B
��B
�1B
��B
��B
�wB
��B
��B
�eB
��B
�yB
�tB
��B
z!B
x=B
w�B
v�B
u�B
vLB
s�B
uB
x�B
w�B
w�B
w�B
q
B
oKB
m�B
m`B
m�B
m#B
k�B
i�B
i,B
h�B
i�B
g�B
fjB
fB
eDB
c�B
c0B
ecB
d\B
^�B
[ B
Y�B
ZB
Y�B
X8B
V�B
W�B
VB
U�B
U�B
T�B
SeB
Q�B
OB
N�B
N
B
LrB
JB
JQB
J�B
J�B
I�B
H�B
HvB
G�B
G�B
G�B
InB
H�B
F�B
FNB
E�B
E�B
E�B
C
B
A>B
>9B
=�B
=�B
=�B
;zB
:cB
<�B
>�B
?�B
@B
<bB
;�B
:�B
9iB
9B
:B
9nB
7lB
7�B
7�B
6mB
8�B
4`B
3%B
3�B
4B
1B
0�B
0�B
0�B
/�B
.�B
.}B
0�B
1^B
-�B
-oB
,�B
,aB
-IB
-�B
)�B
)�B
)�B
(�B
'iB
'�B
&�B
&�B
&�B
%�B
%�B
&OB
&&B
$]B
#B
"�B
"�B
"\B
"4B
!�B
"B
"?B
 �B
�B
 GB
/B
�B
�B
	B
yB
B
B
wB
rB
�B
�B
6B
iB
RB
 �B
 �B
 B
 hB
!�B
)B
�B
�B
mB
B
�B
�B
&B
B
�B
�B
�B
B
�B
B
,B
1B
B
/B
oB
�B
�B
�B
�B
�B
B
#B
B
�B
�B
�B
�B
0B
�B
�B
;B
B
�B
�B
�B
�B
`B
DB
)B
�B
SB
�B
dB
�B
�B
�B
iB
B
9B
�B
B
�B
B
,B
�B
�B
B
cB
/B
�B
�B
�B
B
�B
AB
�B
�B
�B
B
�B
RB
hB
�B
�B
B
�B
IB
0B
B
�B
+B
dB
�B
<B
/B
�B
�B
B
]B
�B
B
�B
8B
B
B
�B
�B
�B
pB
AB
�B
B
�B
EB
�B
�B
�B
>B
�B
0B
^B
�B
TB
dB
B
�B
BB
dB
�B
�B
 �B
 B
 .B
�B
 FB
 MB
 �B
!B
":B
!�B
"MB
#PB
$&B
"�B
"�B
"�B
"]B
"+B
"iB
##B
"�B
"�B
#�B
$�B
%'B
%B
$�B
%B
%�B
'ZB
'NB
'�B
(&B
(�B
)!B
)B
*;B
,3B
,vB
,�B
,�B
,5B
,aB
-�B
/B
-�B
.aB
/@B
/2B
.�B
0;B
0.B
0fB
0oB
0�B
0�B
1B
0�B
1PB
2[B
1B
2ZB
2B
1IB
1tB
0�B
1#B
1IB
15B
2�B
2�B
2EB
2JB
2�B
2�B
2�B
36B
3�B
4/B
3�B
3�B
3�B
4-B
6�B
6B
5�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
8�B
8�B
8�B
9zB
:�B
:B
:	B
:!B
:tB
:fB
;�B
=B
=[B
>�B
>�B
?~B
@�B
@�B
@�B
AB
A�B
BXB
BeB
B�B
B�B
B�B
C�B
C�B
D�B
DsB
D�B
EB
EqB
F3B
FB
E�B
E�B
E�B
E�B
E�B
F�B
GfB
HB
HzB
HkB
H�B
H�B
IpB
J�B
I�B
I�B
I�B
I�B
J B
J7B
J�B
K�B
KzB
L"B
K�B
L�B
M%B
M!B
M�B
NZB
N�B
N�B
O!B
O#B
OnB
OhB
OBB
O�B
P�B
P�B
QB
P�B
Q�B
Q�B
REB
R�B
R�B
R�B
R�B
S-B
T4B
T<B
T�B
TFB
T�B
T�B
T�B
T�B
UBB
U�B
V1B
VHB
VoB
VOB
VB
VKB
W]B
WB
V�B
WWB
W�B
X�B
XuB
YB
YAB
YmB
Y�B
ZB
Z_B
ZB
Z�B
Z�B
Z�B
[1B
Z�B
[`B
[5B
[DB
[�B
\B
\B
\BB
\�B
\oB
\�B
\�B
\�B
\qB
\�B
\EB
\}B
\�B
\�B
]�B
]wB
^�B
^�B
^�B
_B
_B
_/B
_B
_oB
`B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
aB
a�B
a�B
aVB
a\B
aLB
`�B
`�B
aB
bB
a�B
a�B
a�B
a�B
a�B
a�B
bCB
_�B
b�B
b�B
b�B
c_B
c:B
c/B
c�B
c�B
c�B
c�B
c�B
dB
dfB
d>B
dNB
dpB
d�B
eIB
e�B
eBB
erB
efB
etB
eCB
e�B
e�B
e�B
f B
f�B
f�B
g�B
hB
i$B
iB
i�B
i�B
i�B
i�B
i~B
iB
i�B
i�B
jlB
joB
j�B
j�B
kB
k�B
k�B
k�B
lB
l�B
lKB
l�B
l�B
mB
l�B
l�B
l�B
l�B
m$B
m�B
m�B
m�B
nB
nGB
n�B
n�B
o!B
o�B
o�B
o�B
o�B
p4B
pB
pZB
p�B
p�B
qqB
qdB
q�B
qWB
qWB
q9B
qyB
r0B
r.B
r*B
r�B
r�B
s=B
sKB
seB
s�B
s�B
s�B
sAB
s�B
s�B
s�B
t>B
t7B
tB
tCB
tuB
t3B
t#B
t&B
tFB
t�B
t�B
u�B
v>B
v�B
wB
wlB
w�B
w�B
w�B
xB
x@B
xpB
yYB
yB
y,B
yB
yTB
y?B
yuB
y�B
y�B
zB
z�B
z�B
z�B
z~B
z�B
z�B
z�B
{B
{9B
{)B
z�B
z�B
{B
{�B
|B
|LB
{�B
|B
|�B
|XB
|`B
|XB
}�B
}YB
}�B
B
~�B
~dB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
~�B
�B
�B
��B
��B
�rB
�sB
�QB
��B
�;B
�lB
�DB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�|B
��B
��B
��B
��B
�YB
��B
�BB
��B
��B
�B
��B
�.B
�JB
��B
�>B
�$B
��B
�_B
��B
��B
��B
��B
�MB
��B
��B
��B
�_B
�\B
�B
��B
��B
�UB
��B
�cB
�hB
��B
�~B
��B
�EB
��B
�B
��B
�kB
�)B
��B
�WB
��B
��B
�TB
��B
�_B
��B
�;B
�.B
��B
�UB
��B
��G�O�B̘B��B��BуB��B�BB��B��B��B��B�B�B��B� B��B�NB�HBΥB�vB�HB�[BѷB�pB��BҽB�}BбB͟BϫBӏB�aB՛B��B��BרBרB�
B�BخB��BרB�?B�?B�mB֡B��BרB�KB��BخB�sB�yB�B�EB�?B�yBچB�BרB�
B�BخB��B�B�)BیB�B��B�8B�DB�B�B�B��B� B��B�AB��B	�B	�B	B	uB	PB	�B	�B	 �B	&LB	*0B	*eB	*eB	0�B	8�B	F?B	GzB	M6B	QNB	^B	c�B	e�B	h
B	l�B	m�B	qAB	r�B	tB	sMB	rB	q�B	sB	u%B	sB	s�B	tTB	uZB	tTB	v+B	y�B	y	B	yrB	w2B	zxB	�B	��B	��B	��B	��B	��B	��B	�.B	��B	��B	�VB	��B	�\B	�4B	��B	�(B	�VB	��B	��B	�B	��B	��B	��B	��B	�B	��B	�+B	�FB	��B	�B	��B	�B	̘B	�VB	��B	��B	��B	�1B	�xB	�B	�qB	��B	��B	�B	��B	�XB	�aB	�rB	��B	�{B	�	B	��B	��B	� B	�IB	��B	��B	��B	�lB	�{B	�_B	�1B	��B	��B	��B	��B	��B	�OB	��B	��B	�$B	�YB	��B	�$B	�\B	��B	��B	�VB	�B	��B	�B	�_B	�SB	��B	�SB	��B	��B
^�B	�B	��B	��B	�nB	��B	��B	�FB	�B	��B	��B	�qB	�_B	��B	�B	��B	��B	�B	�B	��B	��B	�eB	�XB	�4B	��B	��B	�B	�B	�HB	�`B	�B	�B	��B
�B	�B
-wB
tTB
jB
q�B
m�B
p�B
y�B
�VB
�xB
�oB
�B
� B
��B
��B
��B
��B
�VB
��B
��B
|�B
x8B
v+B
�B
y�B
~(B
�B
y>B
��B
�;B
��B
��B
�B
�B
�%B
��B
��B
��B
�YB
��B
��B
��B
��B
�GB
�uB
��B
��B
cB
�B
}"B
��B
.B
z�B
|B
�4B
x�B
v`B
m�B
p�B
o B
m�B
j�B
m]B
jB
h�B
h�B
n/B
h�B
e,B
c�B
c�B
e`B
d�B
d�B
^B
^�B
^B
YKB
XEB
V9B
VmB
V�B
NpB
R B
NB
GB
A B
FtB
B�B
B[B
GEB
<B
A�B
5B
6�B
4�B
3�B
:*B
.B
 �B
1[B
YB
FB
�B
 \B
 �B
#nB
$tB
B	�B	��B	�cB	��B	��B	��B	�B	��B	ٴB	ԕB	�aB	ӏB	՛B	�B	��B	ϫB	�XB	ܒB	�B	�wB	��B	�cB	ȴB	�6B	��B	�B	��B	�:B	�B	�!B	�'B	��B	��B	��B	��B	�IB	�_B	�"B	��B	�fB	��B	�SB	��B	�B	�_B	�xB	��B	zB	|PB	�{B	��B	uZB	l�B	{�B	��B	k�B	sMB	e�B	h�B	g8B	a�B	^�B	`B	\�B	Z�B	]�B	^�B	YKB	Z�B	Z�B	\)B	VmB	U2B	V�B	T�B	U�B	T�B	T�B	S[B	U�B	R�B	T,B	XyB	Z�B	^�B	R�B	K�B	S&B	M6B	N�B	OvB	a�B	ZQB	YKB	U�B	M�B	I�B	I�B	J�B	FB	I�B	:�B	O�B	9XB	<�B	7�B	7LB	8�B	8RB	:*B	G�B	9$B	;�B	F?B	n/B	v�B	�MB	�B	�=B	�4B	� B	�4B	��B	��B	��B	��B	�=B	��B	��B	��B	��B	��B	�B	�tB	�jB	ɆB	�XB	�zB	�,B	�sB	�HB	��B	�?B	�aB	��B	��B	�B	��B	��B	ȴB	�mB	��B	��B	�B	�RB	�6B	��B	�#B	��B	��B	��B	�UB	��B	��B	�\B	��B	��B	�oB	��B	�@B	�B	��B	��B	�{B	��B	�CB	�tB	�B	��B	�B	�}B	��B	ɆB	�RB	�BB	�#B	�jB	�gB	�/B	�B	ߤB	ߤB	� B	�yB	�B	ںB	�B	ٴB	�B	�2B	��B	��B	��B	�NB	˒B	ǮB	��B	�9B	�B	��B	�}B	��B	�-B	��B	�mB	�gB	��B	�B	�sB	�pB	�,B	�B	�B	�"B	� B	�B	��B	�TB	��B	��B
SB
!-B
&B
(XB
*0B
+6B
+�B
*�B
)�B
*�B
+6B
+�B
+B
+B
)�B
+6B
0�B
6FB
;�B
:�B
;0B
<jB
>B
D3B
E�B
F?B
G�B
H�B
L�B
MB
NB
R B
U2B
W
B
YB
Z�B
]dB
`B
a�B
b�B
b�B
a�B
c B
d�B
gB
gB
f�B
h>B
iyB
iDB
h�B
jB
k�B
o5B
o�B
qB
o5B
v�B
s�B
tTB
u�B
w2B
wfB
w�B
w�B
v`B
v+B
xB
y>B
xB
w�B
w�B
x�B
x�B
zxB
~]B
~(B
�B
��B
��B
�lB
�DB
�JB
��B
�B
�(B
�1B
�7B
�~B
�B
��B
�DB
��B
�lB
�1B
��B
�"B
�PB
� B
��B
�B
��B
�1B
�	B
�+B
��B
�B
�B
� B
}�B
}�B
~(B
|B
|�B
z�B
}�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<@Y�<��G<�%.=_�<�Bg<��<:^<#�
<��<��}<6;<#�
<d1=�<�kD=��<(9Q<��<���<o�h<6�-<rH<#�
<#�
<`o<u<�� =A<�T)<�]p=a�<��<�`�<���<�\8<#�
<#�
<#�
<88&<�<#�
<#�
<#�
<#�
<%xV<�$�<7��<@�a<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<E<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019101112070820191011120708IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019102111002920191021110029QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019102111002920191021110029QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906573120200109065731IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                