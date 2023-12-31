CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-09-17T16:37:57Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20200917163757  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               c   cAA  AOAO7314_008642_099                 7314_008642_099                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�8�]�y�@�8�]�y�11  @�8�6z@�8�6z@1��mr@1��mr�b�u�!�S�b�u�!�S11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @}p�@�  @�G�@�G�A   A  A   A+�A?\)A_\)A\)A��A�Q�A�Q�A�Q�AϮA�  A�Q�B (�B�
B  B  B   B((�B0  B8  B@  BH  BP  BX  B`(�Bg�
Bp(�Bx(�B�{B�Q�B��B��B��B��B��B�{B�{B�{B�{B�  B�{B�{B�  B��B�  B�  B�{B�{B�  B�  B��B��B��B�  B�  B�{B�  B�{B�  B��C   C��C  C  C��C
  C  C��C
=C
=C  C  C
=C  C  C  C   C"
=C$
=C&  C(  C*
=C,  C.{C0  C2  C4  C6  C7��C9��C;��C>  C?��CA��CD  CF
=CH  CI��CL  CN  CO��CQ��CT  CV
=CW��CZ  C[��C^  C`  Cb  Cd
=Cf
=Ch
=Cj  Ck��Cn  Cp
=Cr  Cs�Cu��Cw��Cz  C|  C}��C�  C�C�C�C�  C�  C�  C�  C�  C���C���C���C���C�  C�
=C�
=C�  C�  C�C�C���C�  C�C�  C���C���C�  C�C�C�  C���C�  C�  C�  C���C���C���C�  C�
=C�C�C�C�C�  C�  C���C�C�
=C�
=C�C�C�C�
=C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�C�C�  C�C�C�  C�  C�C�  C�  C�  C�  C�C���C���C���C�  C�C�  C���C�C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C�
=C�
=C���C���C�  C�  C�  C�C�  C�  C���C���C���C�  C�  C�  C�C�  C���C�  C�C�  C�  C�
=C�C�C�C�  C�  D   D }qD  D� D�D� D�qD� D�D��D  D}qD�qD� D  D}qD�D��D	�D	��D	�qD
� D�D}qD  D��D�D�D�D}qD  D� D��D}qD�qD� D�D�DD� D�qD}qD  D� D  D}qD�D��D�D��D  D� D  D� D�qD}qD�qD}qD��D}qD�qD}qD�D�D D � D �qD!}qD!�qD"� D#  D#� D$�D$�D%�D%� D&�D&�D'D'�D(D(��D)�D)��D)�qD*}qD+  D+� D,  D,� D-  D-� D.  D.��D/�D/� D/�qD0� D1  D1}qD2  D2��D3�D3}qD4  D4� D4�qD5� D6�D6� D7  D7� D8  D8� D8�qD9� D:D:� D:�qD;� D<�D<��D=  D=� D>  D>� D?�D?��D@  D@}qDA  DA��DB�DB� DB�qDC��DD  DDz�DD�qDE}qDF  DF� DG  DG� DH  DH��DI�DI��DI�qDJ� DK  DK� DK�qDL}qDL�qDMz�DM�qDN� DO  DO}qDO�qDP}qDP��DQ� DR�DR� DS  DS� DT  DT� DU  DU}qDV  DV}qDV�qDW}qDX  DX� DY�DY� DY�qDZ� D[�D[� D[�qD\� D]�D]� D^  D^}qD^��D_}qD`�D`��D`�qDa}qDb  Db��Dc�Dc� Dc�qDdz�Dd�qDe��Df  Df}qDf��Dg� Dh�Dh��Di  Di}qDj  Dj� Dk  Dk}qDk��Dl� Dm�Dm��Dn�Dn��Do�Do��DpDp�Dq�Dq� Dr  Dr��DsDs��Dt  Dt��Du  Du� Du�qDvz�Dw  Dw�DxDx� Dy  Dy}qDy�qDz��D{  D{� D|�D|� D}�D}� D}�qD~z�D~�qD� D�qD�@ D��HD�� D���D�=qD�|)D�� D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD�  D�AHD�~�D�� D�  D�>�D�}qD�� D�HD�B�D��HD�� D�HD�@ D�~�D�� D���D�>�D�~�D�� D��D�B�D�� D�� D�  D�@ D�� D�� D�  D�@ D���D�D�  D�@ D��HD��HD���D�>�D�~�D���D�HD�AHD�~�D���D�HD�B�D�~�D���D�HD�AHD��HD��HD�HD�AHD�~�D���D���D�>�D��HD�D�  D�@ D��HD���D���D�@ D��HD�� D���D�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD�� D���D���D�>�D�~�D�� D�HD�B�D���D�D�  D�@ D�� D��qD�  D�AHD�� D���D�  D�AHD��HD�� D�  D�@ D��HD�� D�  D�AHD�~�D�� D�HD�@ D��HD�� D�  D�@ D��HD�� D���D�@ D�� D���D�  D�AHD��HD��HD���D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�~�D�� D�HD�@ D�~�D�� D�  D�AHD��HD�� D�HD�B�D��HD�� D���D�@ D�� D�� D���D�=qD�� D��HD�  D�=qD�~�D���D�  D�>�D�~�D���D���D�AHD��HD���D���D�>�D�� D�� D���D�>�D�� D���D�  D�AHD��HD��HD�HD�AHD��HD�� D���D�AHD��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�� D�� D�  D�AHD�~�D�� D�HD�>�D�� D�� D���D�>�D�� D�� D���D�>�D�� D�� D�  D�>�D�� D��HD�HD�>�D�~�D�� D���D�>�D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�@ D�~�Dþ�D�  D�B�DāHD�� D�HD�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�>�D�~�DǾ�D�  D�AHDȀ D�� D�  D�AHDɁHD��HD�HD�@ Dʀ D��HD�  D�>�D�~�D˾�D���D�@ D́HD�� D�  D�@ D̀ D�� D�  D�@ D΀ D��HD�  D�>�D�~�DϾ�D�  D�AHDЁHD��HD�HD�@ DсHD��HD�HD�AHDҁHD�� D���D�>�DӀ D�� D�  D�@ DԁHD��HD���D�>�DՀ D��HD�HD�@ Dր D�� D�  D�>�D�~�D��HD��D�@ D�~�D�� D�  D�>�DفHD�D��D�@ D�}qDڽqD�  D�AHDۂ�D��HD���D�>�D܀ D��HD�  D�>�D�~�Dݾ�D�  D�AHDހ D޾�D�  D�@ D߁HD�� D���D�@ D�� DྸD�HD�@ D� D�� D���D�>�D� D�� D���D�@ D� D㾸D���D�@ D� D�� D�HD�AHD�HD��HD�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�AHD� D�� D���D�>�D�~�D�� D�HD�@ D�~�D��HD�  D�>�D�~�D�� D���D�=qD�}qD쾸D�  D�@ D�HD�� D���D�>�D� D�� D�  D�@ D� D�� D�  D�@ D��HD��HD�HD�AHD�HD��HD�  D�>�D� D�� D�  D�AHD�HD�� D���D�@ D�HD���D��qD�AHD��HD���D���D�>�D��HD��HD�  D�AHD���D��HD���D�@ D�� D��HD�HD�AHD��HD��HD�HD�4{D�t{?\)?L��?��?���?\?�G�?��H@�@(�@+�@5@E�@Tz�@aG�@n{@�  @�ff@�{@�@�(�@��
@��@�33@�(�@��
@˅@�33@��H@��
@�@�33@�(�A�\AffA
�HA�RA33A�A(�A   A$z�A(Q�A,(�A0  A4z�A8Q�A<��A@��AE�AI��AN{AQ�AVffAZ�HA_\)Ac33Ag�Al(�Ap��Au�Ay��A~{A�G�A��A�A�  A�=qA�(�A�ffA�Q�A��\A��A��A��A��
A�ffA�Q�A�=qA�z�A�ffA���A��HA��A�
=A�G�A�33A�p�A��A��A�(�A�{A�Q�A�=qA�z�A�ffA�Q�A�=qA�(�A�ffA�Q�A�=qA�z�AָRAأ�Aڏ\A�z�A�ffA��A�\A���A�
=A��A��HA���A�RA��A��HA���A��RA���A��HA���A��RB Q�BG�B=qB33B(�B�B{B
=B  B��B
{B
=B(�BG�BffB�B��BB�RB�
B�B=qB\)Bz�BB�HB  B�BffB�B z�B!��B"�RB#�
B$��B&{B'
=B((�B)G�B*ffB+�B,z�B-��B.�RB/�
B0��B1�B3
=B4(�B5p�B6�\B7�
B8��B9�B;33B<Q�B=G�B>�\B?�B@��BA�BC
=BD(�BEG�BFffBG\)BHz�BI��BJ�RBK�
BL��BN{BO\)BPQ�BQp�BR�\BS�BT��BU�BV�HBX(�BY�BZ=qB[\)B\z�B]��B^�\B_�B`��BaBb�RBc�
Bd��Be�Bf�HBh  Bh��Bi�Bk
=Bl(�Bm�Bn=qBo\)BpQ�Bqp�BrffBs�Btz�Bup�BvffBw�Bx��ByBz�HB|  B|��B~{B
=B�{B�z�B���B�p�B��
B�=qB���B�
=B�\)B���B��B�(�B�ffB���B��HB���B�33B�\)B���B��
B�  B�(�B�Q�B�z�B���B���B�
=B�33B�\)B��B��B��B�{B�Q�B�z�B��RB���B��B�\)B���B�B�  B�=qB�ffB���B���B���B�33B�p�B���B��B��B�(�B�Q�B��\B���B��B�G�B��B�B�  B�=qB�z�B���B��HB�
=B�\)B��B��B��B�(�B�z�B���B��HB��B�p�B��B��B�(�B�Q�B�z�B���B���B��B�G�B���B��B�{B�ffB��\B��HB��B�\)B���B�B�{B�Q�B��\B���B���B�G�B��B�B�{B�Q�B���B��HB�33B��B��
B�{B�ffB���B��HB��B�p�B�B�{B�ffB���B�
=B�\)B���B��B�(�B�ffB��RB�
=B�p�B��B�{B�ffB���B���B�33B��B�B�(�B�z�B���B��B��B��
B�(�B�z�B��RB�
=B�\)B���B��B�=qB��RB�
=B�\)B�B�  B�Q�B��\B���B�G�B���B�  B�ffB���B���B�G�B���B�  B�ffB��RB�
=B�\)B��B�{B�Q�B��RB�
=B�p�B��
B�(�B��\B��HB�33B��B��
B�(�B��\B���B�G�B��B�{B�ffB���B��B�\)B��
B�(�B��\B���B�\)B�B�{B�ffB���B�33B���B�{B�ffB���B�G�B���B�  B�Q�B��RB�
=B�p�B��B�Q�B��RB�
=B�\)B��B�(�B��\B�
=B�\)B�B�{B\B�
=B�p�B��B�=qBģ�B�
=BŅB�  B�Q�BƸRB�
=BǅB��B�ffB���B�33BɅB��B�ffB��HB�\)B�B�(�B̏\B��HB�p�B��B�Q�BθRB�
=Bϙ�B�{BЏ\B���B�p�B��
B�Q�B���B�\)B��
B�=qBԣ�B��Bՙ�B�(�B֣�B��Bי�B�  B�z�B�
=BمB�{B�z�B���B�p�B�  B܏\B�
=B݅B�  B�z�B���Bߙ�B�(�B��\B�
=B�B�(�B�RB�G�B�B�(�B�RB�G�B��B�Q�B���B�G�B��
B�ffB���B�\)B��B�z�B��B뙚B�{B�z�B��B��B�=qB���B�G�B�B�Q�B���B�B�{B��B��B�B�Q�B���B��B�  B�z�B��B�B�=qB��RB�\)B�  B��\B�
=B���B�Q�B��HB�\)B��B��\B�33B��C �C ffC �RC
=CQ�C��C�HC33C�C��C{CG�C��C�CG�C�C��C{CffC�RC
=CG�C�\C�C=qC�CC{CffC�RC	
=C	Q�C	��C	�C
=qC
�C
C�CffC�RC  C=qC�\C�HC�Cp�CC{CQ�C��C��CG�Cz�C��C{Cp�C�C��C=qC��C�
C{CffC�RC��C=qCz�C�
C
=CQ�C��C�C(�Cp�C�RC  CG�C�CC{C\)C��C�HC�Cz�C�RC  C33C�C�
C{C\)C��C�C33Cp�C�RC  CG�C�C��C{C\)C�\C�
C(�Cp�C��C�C=qCz�C�RC   C Q�C �\C C!{C!\)C!��C!�
C"(�C"p�C"�RC"�C#33C#�\C#C$  C$Q�C$��C$�
C%{C%p�C%�C%�C&=qC&�C&C'  C'Q�C'��C'�
C((�C(p�C(�C(�C)=qC)�C)C*
=C*Q�C*��C*�
C+(�C+p�C+�C+��C,=qC,z�C,�RC-
=C-G�C-z�C-C.  C.=qC.ffC.��C.��C.��C/{C/=qC/ffC/z�C/��C/C/�HC/��C0�C0G�C0\)C0p�C0��C0C0�
C0�C1�C1=qC1Q�C1p�C1��C1�RC1��C2  C2�C233C2\)C2�C2��C2C2�C3{C3(�C3Q�C3�C3��C3�RC3�C4{C433C4\)C4�\C4��C4�
C5  C5�C5G�C5z�C5�\C5�RC5�C6
=C6(�C6ffC6�\C6��C6�HC7  C7�C7Q�C7p�C7��C7��C7��C8{C8G�C8p�C8�\C8C8�C9{C933C9ffC9�\C9��C9�HC:
=C:�C:Q�C:�C:��C:�
C;
=C;(�C;G�C;�C;��C;��C<  C<(�C<G�C<z�C<��C<��C=  C=�C=Q�C=z�C=��C=��C>  C>{C>=qC>z�C>��C>�RC>��C?�C?=qC?p�C?��C?�RC?�C@{C@33C@ffC@�C@��C@�
CA
=CA�CAG�CAz�CA��CA��CA��CB{CBG�CBffCB�\CBCB�HCC  CC=qCC\)CCz�CC�CC��CC��CD(�CDG�CDffCD��CDCD�HCE{CE33CEQ�CE�\CE�CE��CF  CF�CFG�CFz�CF��CFCF��CG
=CG=qCGffCG�CG��CG�HCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                ?�  @   @@  @}p�@�  @�G�@�G�A   A  A   A+�A?\)A_\)A\)A��A�Q�A�Q�A�Q�AϮA�  A�Q�B (�B�
B  B  B   B((�B0  B8  B@  BH  BP  BX  B`(�Bg�
Bp(�Bx(�B�{B�Q�B��B��B��B��B��B�{B�{B�{B�{B�  B�{B�{B�  B��B�  B�  B�{B�{B�  B�  B��B��B��B�  B�  B�{B�  B�{B�  B��C   C��C  C  C��C
  C  C��C
=C
=C  C  C
=C  C  C  C   C"
=C$
=C&  C(  C*
=C,  C.{C0  C2  C4  C6  C7��C9��C;��C>  C?��CA��CD  CF
=CH  CI��CL  CN  CO��CQ��CT  CV
=CW��CZ  C[��C^  C`  Cb  Cd
=Cf
=Ch
=Cj  Ck��Cn  Cp
=Cr  Cs�Cu��Cw��Cz  C|  C}��C�  C�C�C�C�  C�  C�  C�  C�  C���C���C���C���C�  C�
=C�
=C�  C�  C�C�C���C�  C�C�  C���C���C�  C�C�C�  C���C�  C�  C�  C���C���C���C�  C�
=C�C�C�C�C�  C�  C���C�C�
=C�
=C�C�C�C�
=C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�C�C�  C�C�C�  C�  C�C�  C�  C�  C�  C�C���C���C���C�  C�C�  C���C�C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C�
=C�
=C���C���C�  C�  C�  C�C�  C�  C���C���C���C�  C�  C�  C�C�  C���C�  C�C�  C�  C�
=C�C�C�C�  C�  D   D }qD  D� D�D� D�qD� D�D��D  D}qD�qD� D  D}qD�D��D	�D	��D	�qD
� D�D}qD  D��D�D�D�D}qD  D� D��D}qD�qD� D�D�DD� D�qD}qD  D� D  D}qD�D��D�D��D  D� D  D� D�qD}qD�qD}qD��D}qD�qD}qD�D�D D � D �qD!}qD!�qD"� D#  D#� D$�D$�D%�D%� D&�D&�D'D'�D(D(��D)�D)��D)�qD*}qD+  D+� D,  D,� D-  D-� D.  D.��D/�D/� D/�qD0� D1  D1}qD2  D2��D3�D3}qD4  D4� D4�qD5� D6�D6� D7  D7� D8  D8� D8�qD9� D:D:� D:�qD;� D<�D<��D=  D=� D>  D>� D?�D?��D@  D@}qDA  DA��DB�DB� DB�qDC��DD  DDz�DD�qDE}qDF  DF� DG  DG� DH  DH��DI�DI��DI�qDJ� DK  DK� DK�qDL}qDL�qDMz�DM�qDN� DO  DO}qDO�qDP}qDP��DQ� DR�DR� DS  DS� DT  DT� DU  DU}qDV  DV}qDV�qDW}qDX  DX� DY�DY� DY�qDZ� D[�D[� D[�qD\� D]�D]� D^  D^}qD^��D_}qD`�D`��D`�qDa}qDb  Db��Dc�Dc� Dc�qDdz�Dd�qDe��Df  Df}qDf��Dg� Dh�Dh��Di  Di}qDj  Dj� Dk  Dk}qDk��Dl� Dm�Dm��Dn�Dn��Do�Do��DpDp�Dq�Dq� Dr  Dr��DsDs��Dt  Dt��Du  Du� Du�qDvz�Dw  Dw�DxDx� Dy  Dy}qDy�qDz��D{  D{� D|�D|� D}�D}� D}�qD~z�D~�qD� D�qD�@ D��HD�� D���D�=qD�|)D�� D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD�  D�AHD�~�D�� D�  D�>�D�}qD�� D�HD�B�D��HD�� D�HD�@ D�~�D�� D���D�>�D�~�D�� D��D�B�D�� D�� D�  D�@ D�� D�� D�  D�@ D���D�D�  D�@ D��HD��HD���D�>�D�~�D���D�HD�AHD�~�D���D�HD�B�D�~�D���D�HD�AHD��HD��HD�HD�AHD�~�D���D���D�>�D��HD�D�  D�@ D��HD���D���D�@ D��HD�� D���D�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD�� D���D���D�>�D�~�D�� D�HD�B�D���D�D�  D�@ D�� D��qD�  D�AHD�� D���D�  D�AHD��HD�� D�  D�@ D��HD�� D�  D�AHD�~�D�� D�HD�@ D��HD�� D�  D�@ D��HD�� D���D�@ D�� D���D�  D�AHD��HD��HD���D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�~�D�� D�HD�@ D�~�D�� D�  D�AHD��HD�� D�HD�B�D��HD�� D���D�@ D�� D�� D���D�=qD�� D��HD�  D�=qD�~�D���D�  D�>�D�~�D���D���D�AHD��HD���D���D�>�D�� D�� D���D�>�D�� D���D�  D�AHD��HD��HD�HD�AHD��HD�� D���D�AHD��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�� D�� D�  D�AHD�~�D�� D�HD�>�D�� D�� D���D�>�D�� D�� D���D�>�D�� D�� D�  D�>�D�� D��HD�HD�>�D�~�D�� D���D�>�D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�@ D�~�Dþ�D�  D�B�DāHD�� D�HD�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�>�D�~�DǾ�D�  D�AHDȀ D�� D�  D�AHDɁHD��HD�HD�@ Dʀ D��HD�  D�>�D�~�D˾�D���D�@ D́HD�� D�  D�@ D̀ D�� D�  D�@ D΀ D��HD�  D�>�D�~�DϾ�D�  D�AHDЁHD��HD�HD�@ DсHD��HD�HD�AHDҁHD�� D���D�>�DӀ D�� D�  D�@ DԁHD��HD���D�>�DՀ D��HD�HD�@ Dր D�� D�  D�>�D�~�D��HD��D�@ D�~�D�� D�  D�>�DفHD�D��D�@ D�}qDڽqD�  D�AHDۂ�D��HD���D�>�D܀ D��HD�  D�>�D�~�Dݾ�D�  D�AHDހ D޾�D�  D�@ D߁HD�� D���D�@ D�� DྸD�HD�@ D� D�� D���D�>�D� D�� D���D�@ D� D㾸D���D�@ D� D�� D�HD�AHD�HD��HD�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�AHD� D�� D���D�>�D�~�D�� D�HD�@ D�~�D��HD�  D�>�D�~�D�� D���D�=qD�}qD쾸D�  D�@ D�HD�� D���D�>�D� D�� D�  D�@ D� D�� D�  D�@ D��HD��HD�HD�AHD�HD��HD�  D�>�D� D�� D�  D�AHD�HD�� D���D�@ D�HD���D��qD�AHD��HD���D���D�>�D��HD��HD�  D�AHD���D��HD���D�@ D�� D��HD�HD�AHD��HD��HD�HD�4{G�O�?\)?L��?��?���?\?�G�?��H@�@(�@+�@5@E�@Tz�@aG�@n{@�  @�ff@�{@�@�(�@��
@��@�33@�(�@��
@˅@�33@��H@��
@�@�33@�(�A�\AffA
�HA�RA33A�A(�A   A$z�A(Q�A,(�A0  A4z�A8Q�A<��A@��AE�AI��AN{AQ�AVffAZ�HA_\)Ac33Ag�Al(�Ap��Au�Ay��A~{A�G�A��A�A�  A�=qA�(�A�ffA�Q�A��\A��A��A��A��
A�ffA�Q�A�=qA�z�A�ffA���A��HA��A�
=A�G�A�33A�p�A��A��A�(�A�{A�Q�A�=qA�z�A�ffA�Q�A�=qA�(�A�ffA�Q�A�=qA�z�AָRAأ�Aڏ\A�z�A�ffA��A�\A���A�
=A��A��HA���A�RA��A��HA���A��RA���A��HA���A��RB Q�BG�B=qB33B(�B�B{B
=B  B��B
{B
=B(�BG�BffB�B��BB�RB�
B�B=qB\)Bz�BB�HB  B�BffB�B z�B!��B"�RB#�
B$��B&{B'
=B((�B)G�B*ffB+�B,z�B-��B.�RB/�
B0��B1�B3
=B4(�B5p�B6�\B7�
B8��B9�B;33B<Q�B=G�B>�\B?�B@��BA�BC
=BD(�BEG�BFffBG\)BHz�BI��BJ�RBK�
BL��BN{BO\)BPQ�BQp�BR�\BS�BT��BU�BV�HBX(�BY�BZ=qB[\)B\z�B]��B^�\B_�B`��BaBb�RBc�
Bd��Be�Bf�HBh  Bh��Bi�Bk
=Bl(�Bm�Bn=qBo\)BpQ�Bqp�BrffBs�Btz�Bup�BvffBw�Bx��ByBz�HB|  B|��B~{B
=B�{B�z�B���B�p�B��
B�=qB���B�
=B�\)B���B��B�(�B�ffB���B��HB���B�33B�\)B���B��
B�  B�(�B�Q�B�z�B���B���B�
=B�33B�\)B��B��B��B�{B�Q�B�z�B��RB���B��B�\)B���B�B�  B�=qB�ffB���B���B���B�33B�p�B���B��B��B�(�B�Q�B��\B���B��B�G�B��B�B�  B�=qB�z�B���B��HB�
=B�\)B��B��B��B�(�B�z�B���B��HB��B�p�B��B��B�(�B�Q�B�z�B���B���B��B�G�B���B��B�{B�ffB��\B��HB��B�\)B���B�B�{B�Q�B��\B���B���B�G�B��B�B�{B�Q�B���B��HB�33B��B��
B�{B�ffB���B��HB��B�p�B�B�{B�ffB���B�
=B�\)B���B��B�(�B�ffB��RB�
=B�p�B��B�{B�ffB���B���B�33B��B�B�(�B�z�B���B��B��B��
B�(�B�z�B��RB�
=B�\)B���B��B�=qB��RB�
=B�\)B�B�  B�Q�B��\B���B�G�B���B�  B�ffB���B���B�G�B���B�  B�ffB��RB�
=B�\)B��B�{B�Q�B��RB�
=B�p�B��
B�(�B��\B��HB�33B��B��
B�(�B��\B���B�G�B��B�{B�ffB���B��B�\)B��
B�(�B��\B���B�\)B�B�{B�ffB���B�33B���B�{B�ffB���B�G�B���B�  B�Q�B��RB�
=B�p�B��B�Q�B��RB�
=B�\)B��B�(�B��\B�
=B�\)B�B�{B\B�
=B�p�B��B�=qBģ�B�
=BŅB�  B�Q�BƸRB�
=BǅB��B�ffB���B�33BɅB��B�ffB��HB�\)B�B�(�B̏\B��HB�p�B��B�Q�BθRB�
=Bϙ�B�{BЏ\B���B�p�B��
B�Q�B���B�\)B��
B�=qBԣ�B��Bՙ�B�(�B֣�B��Bי�B�  B�z�B�
=BمB�{B�z�B���B�p�B�  B܏\B�
=B݅B�  B�z�B���Bߙ�B�(�B��\B�
=B�B�(�B�RB�G�B�B�(�B�RB�G�B��B�Q�B���B�G�B��
B�ffB���B�\)B��B�z�B��B뙚B�{B�z�B��B��B�=qB���B�G�B�B�Q�B���B�B�{B��B��B�B�Q�B���B��B�  B�z�B��B�B�=qB��RB�\)B�  B��\B�
=B���B�Q�B��HB�\)B��B��\B�33B��C �C ffC �RC
=CQ�C��C�HC33C�C��C{CG�C��C�CG�C�C��C{CffC�RC
=CG�C�\C�C=qC�CC{CffC�RC	
=C	Q�C	��C	�C
=qC
�C
C�CffC�RC  C=qC�\C�HC�Cp�CC{CQ�C��C��CG�Cz�C��C{Cp�C�C��C=qC��C�
C{CffC�RC��C=qCz�C�
C
=CQ�C��C�C(�Cp�C�RC  CG�C�CC{C\)C��C�HC�Cz�C�RC  C33C�C�
C{C\)C��C�C33Cp�C�RC  CG�C�C��C{C\)C�\C�
C(�Cp�C��C�C=qCz�C�RC   C Q�C �\C C!{C!\)C!��C!�
C"(�C"p�C"�RC"�C#33C#�\C#C$  C$Q�C$��C$�
C%{C%p�C%�C%�C&=qC&�C&C'  C'Q�C'��C'�
C((�C(p�C(�C(�C)=qC)�C)C*
=C*Q�C*��C*�
C+(�C+p�C+�C+��C,=qC,z�C,�RC-
=C-G�C-z�C-C.  C.=qC.ffC.��C.��C.��C/{C/=qC/ffC/z�C/��C/C/�HC/��C0�C0G�C0\)C0p�C0��C0C0�
C0�C1�C1=qC1Q�C1p�C1��C1�RC1��C2  C2�C233C2\)C2�C2��C2C2�C3{C3(�C3Q�C3�C3��C3�RC3�C4{C433C4\)C4�\C4��C4�
C5  C5�C5G�C5z�C5�\C5�RC5�C6
=C6(�C6ffC6�\C6��C6�HC7  C7�C7Q�C7p�C7��C7��C7��C8{C8G�C8p�C8�\C8C8�C9{C933C9ffC9�\C9��C9�HC:
=C:�C:Q�C:�C:��C:�
C;
=C;(�C;G�C;�C;��C;��C<  C<(�C<G�C<z�C<��C<��C=  C=�C=Q�C=z�C=��C=��C>  C>{C>=qC>z�C>��C>�RC>��C?�C?=qC?p�C?��C?�RC?�C@{C@33C@ffC@�C@��C@�
CA
=CA�CAG�CAz�CA��CA��CA��CB{CBG�CBffCB�\CBCB�HCC  CC=qCC\)CCz�CC�CC��CC��CD(�CDG�CDffCD��CDCD�HCE{CE33CEQ�CE�\CE�CE��CF  CF�CFG�CFz�CF��CFCF��CG
=CG=qCGffCG�CG��CG�HCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@�_G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A٥�A٧�A٩�AٮAٰ!A٩�A٩�Aٰ!Aٰ!Aٲ-Aٰ!AٶFAٺ^Aٺ^Aٺ^Aٺ^AټjAٸRAٺ^AٸRAٺ^Aٲ-A١�A��A�/A׺^A׋DA�I�A��HA�r�A�A�A��A��A��AռjA�~�A�bA�I�A�|�A�(�A�1A���A���AǬA��`AĶFAô9A®A�
=A���A��7A�E�A���A�M�A��+A��^A�ZA�oA�  A�-A���A��A�5?A��A�{A�A�A�O�A��
A��FA�ĜA��^A�ƨA�  A���A�  A�I�A�7LA��A��HA�Q�A�VA�r�A���A�VA�G�A�ffA��
A��hA�%A�`BA���A�l�A��/A���A��TA��RA}��Az�DAv��Apr�Al9XAj�Ag�AbZA^Q�A[7LAY�mAT^5AP^5AL��AI;dAGdZADJA@�A?`BA=��A9��A8�yA8$�A65?A3�mA2~�A1S�A/�A.{A,A�A+oA*ZA)�A(��A(9XA'|�A%��A$�A#hsA"VA"1'A"�A!ƨA!��A!��A!��A!;dA��A~�AƨA�`AbNA�A��A?}A�Az�A  A�PA�yAn�A?}AA�AȴA�TAhsA�A�!Ap�A�-A��A�-A�A�A�DAffAbA�A�AAG�A��A(�A�mAp�A7LA�A�/A~�A�A\)A��A1'A�A  Ax�AK�A��AI�A|�A�/A�A&�A��A�;A\)A?}A
�A
(�A	33AȴAbNA�TA��A��A�A{A?}A�A\)A�!AffA��A��AXA
=AA ��A �A �HA ��A {@�o@��+@��7@�G�@�%@��@�  @�K�@���@�E�@��h@��@���@���@��u@�9X@��;@���@�\)@�"�@�@��/@�D@��@�;d@�+@�^@��@�r�@��@�\)@��@���@�r�@�1@땁@�o@���@�5?@陚@�G�@蛦@��;@�C�@�!@�$�@噚@��`@�1@��
@��@�33@�!@�5?@��@�7@��`@߾w@��@�n�@��@�G�@ܣ�@��;@�C�@��@ڗ�@��@��@��@١�@�X@�7L@�V@ؼj@�1'@�l�@�o@�=q@���@�`B@��@��`@�Ĝ@���@�+@�ff@ѡ�@�`B@��@�Ĝ@�1@ϕ�@�\)@�o@Χ�@�J@�&�@�bN@�1'@��@˥�@ʸR@��@ɩ�@�X@Ȭ@�Z@ȃ@�I�@��
@�t�@��@Ɨ�@���@�hs@��`@�b@�33@�@��@���@���@�I�@��@��@�"�@�ȴ@���@�~�@�^5@��@��^@�G�@���@��;@��@�dZ@�
=@���@�V@��@��7@��@���@��D@�bN@�b@���@���@�M�@��@���@�`B@�7L@��@�1'@���@�l�@�@��R@�ff@��@��^@�hs@�/@��/@��u@��@��@�+@��H@�ȴ@��R@�~�@�@�hs@�V@�Ĝ@�Z@�A�@�b@�|�@�33@��@��y@�V@��#@��^@���@�/@���@��j@���@�I�@���@��@�S�@�o@���@�^5@�{@��#@���@�G�@��/@�r�@�  @���@�33@���@���@�$�@���@��T@�O�@���@�j@��@��P@�+@���@�~�@�^5@�5?@�{@�J@�@��@��@�%@��/@���@�A�@��@���@��w@��P@�\)@�33@���@��+@�5?@��@���@���@�p�@�?}@��@���@��@�Z@�(�@��;@���@���@�33@�
=@��!@�v�@�-@���@��7@��@��@���@��j@�(�@��F@�K�@�S�@�\)@�K�@�@�v�@��@��T@�x�@�O�@�V@���@��@�ƨ@���@���@�|�@�dZ@�
=@�ff@�ff@��\@�V@�@�hs@�&�@���@�Z@�b@��;@��F@���@�t�@�33@��@��!@��\@�V@�-@��h@��@�X@�/@��/@���@��D@�I�@�ƨ@�K�@�@���@���@��\@�v�@�E�@�$�@��@��^@���@��7@��@�G�@��@�%@��@�z�@�A�@�  @��w@���@��@�+@��H@�ȴ@�~�@�M�@�E�@�-@���@��T@���@��^@��-@��7@�/@���@��/@�bN@�b@l�@~��@~v�@}V@|�/@|��@|I�@{�@{o@z�\@y��@yG�@x��@x��@x1'@w�@v�y@v��@u�@u�-@u�@uO�@t�@t9X@s��@s�
@s��@st�@s33@r��@r�\@r�@q��@q��@qx�@q%@pĜ@pbN@p �@o��@o|�@o�@nȴ@nV@m�T@m`B@m�@l�/@l�@lj@l9X@k��@k@j-@iX@h�`@hĜ@h�u@hA�@g�w@g�@g�@g�@g��@g;d@f�R@fv�@e�T@e/@d�D@d�D@dj@dj@d�@c�F@c�@cC�@co@b��@b�!@b�!@b�!@b�!@b�@ahs@aX@a7L@`��@`�u@` �@_�@_l�@_;d@^�@^v�@]@\�@[dZ@Z��@Z�@Y�^@Yx�@Yhs@Yx�@Yx�@X��@X�9@Xr�@W�;@W�P@W;d@V�R@V�+@U@UV@T�j@T��@TZ@S��@S�@R�@R�\@R^5@R^5@RM�@Q�#@QG�@P�@O�@O�w@N�y@N{@M�@M��@Mp�@L��@LZ@Kƨ@K�F@Kt�@KS�@K"�@K@J��@J=q@I��@Ix�@IG�@H��@Hb@GK�@G+@F��@F�y@Fff@E�@E�-@D�j@Dj@Dz�@D9X@D(�@C��@C"�@B��@BM�@B=q@A�^@@��@@r�@@Q�@@b@?�P@>��@>�R@>ff@=��@=�h@=?}@<��@<9X@;��@;S�@;"�@;@:��@:-@9�#@9��@9��@9x�@8��@8Q�@8  @7�P@7K�@6�y@6ȴ@6��@6v�@6v�@6{@5�-@5�@4�@4��@49X@3�
@3dZ@3"�@2�H@2~�@2-@1�#@1�7@1G�@1&�@1%@0��@0bN@0 �@0  @0  @/�;@/��@/�w@/�w@/��@/;d@.�@.ff@.5?@.@-��@-�h@-/@,�j@,z�@,9X@+��@+�@+33@*��@*�@)��@)�#@)�^@)��@)��@)��@)x�@(��@(��@(�`@(�9@(Q�@'�@'��@';d@&ȴ@&�+@&V@&$�@%�h@%O�@%�@$��@$��@$�j@$j@$�@#��@#t�@#S�@#33@"�@"��@"�\@"=q@"�@!��@!��@!��@!�^@!��@!X@!%@ �u@ �@ Q�@   @�w@|�@K�@
=@�y@��@{@�T@�-@��@?}@�/@�j@��@z�@j@9X@�
@��@�@dZ@C�@"�@"�@o@��@�!@n�@^5@^5@M�@=q@J@�#@��@�7@x�@x�@X@&�@%@�9@�u@�u@�@r�@Q�@A�@1'@b@�w@�P@�P@�P@�P@�P@|�@l�@\)@;d@+@
=@�+@$�@��@�@O�@/@V@�j@�D@j@I�@��@��@dZ@dZ@"�@~�@M�@J@��@�@�^@G�@�9@��@��@r�@A�@�;@K�@�y@ȴ@�R@��@��@v�@�T@�@p�@p�@`B@/@�/@��@z�@Z@I�@9X@9X@�@�@1@�m@�
@�
@ƨ@��@��@33@
�!@
~�@
=q@
-@
�@
�@
J@	��@	��@	��A١�A٥�A٧�A٧�A٧�A٧�A٧�A٩�A٥�A٩�A٩�A٧�AٮA٬A٬Aٰ!AٮAٮAٰ!Aٴ9Aٴ9A٬A٬A٩�A٧�A٧�A٧�A٧�A٧�A٩�A٬Aٲ-Aٰ!Aٰ!Aٰ!AٮAٮAٰ!Aٲ-Aٰ!Aٰ!Aٲ-Aٰ!Aٰ!Aٲ-AٶFAٸRAٸRAٸRAٸRAٸRAٺ^Aٺ^AٸRAٸRAٺ^Aٺ^Aٺ^Aٺ^AټjAٺ^AټjAٺ^Aٺ^Aٺ^Aٺ^Aٺ^Aٺ^AټjAټjAټjAٺ^AٸRAٸRAٸRAٸRAٺ^Aٺ^Aٺ^Aٺ^Aٺ^AټjAپwAپwAټjAټjAٺ^AټjAٺ^Aٺ^AٸRAٸRAٺ^Aٺ^AٸRAٺ^AټjAٺ^AٸRAٺ^Aٺ^Aٺ^Aٺ^Aٺ^Aٺ^AٸRAٺ^AٸRAٸRAٸRAٸRAٸRAٺ^Aٺ^Aٺ^Aٺ^Aٺ^AٸRAٸRAٸRAٸRAٶFAٰ!A٬Aٲ-Aٲ-Aٰ!A٬Aٰ!AٶFAٴ9A٩�Aٗ�Aى7AمAفA�\)A���AؾwAة�Aؕ�AؑhA�|�A�XA�=qA�7LA�/A�&�A���A��A���A׼jA׶FAײ-A׬Aק�Aף�Aם�Aכ�Aו�A׋DAׅA�~�A�t�A�hsA�VA�S�A�VA�I�A�9XA�&�A��A���A���A��A��`Aֺ^A֩�A֗�Aև+A�~�A�t�A�hsA�dZA�\)A�I�A�A�A�E�A�E�A�I�A�C�A�;dA��A��A��#A��TA��A���A�  A�A�  A���A��A��A��yA��mA��TA��TA��TA��HA��#A��#A���A���A�ȴA�ȴA���AվwAվwAոRAմ9Aղ-Aթ�Aա�A՛�AՑhAՃA�t�A�bNA�G�AԴ9A��Aӣ�A�l�A�"�A�ZA�ƨAѰ!Aѩ�Aѥ�Aљ�A�z�A�"�A���A��#Aд9AЅA�^5A�?}A�$�A� �A�{A��`A���A�ƨAρA�+A��/A�z�A�5?A�oA��A͡�A�ffA�9XA���A��
A���A̸RA̗�A�XA�
=A��mA�ȴA˸RA˕�A�~�A�t�A�n�A�M�A�+A�(�A��A�%A��/Aʩ�AʃA�ZA�1'A�oA�  A��yA��
A�ȴAɾwAɲ-Aɥ�Aə�AɍPAɃA�x�A�dZA�S�A�=qA�$�A�JA���A��A��A��yA��`A��HA��/A���A���A���A���A��
A��yA��A��A���A���A���A�A���A�1A�A�%A�
=A�JA�VA�JA�JA�  A��Aȕ�A�`BA�`BA�E�A�/A��A�ȴAǇ+A�`BA�M�A�?}A�1'A�$�A�{A���A���Aƛ�A�jA�I�A�$�A��mAţ�Aś�Ař�Aŗ�Aŕ�Aŏ\AōPAŋDAŋDA�l�A�33A��A��A���Aĥ�AăA�S�A�S�A�Q�A�?}A�9XA�/A��A�1A��A���Aú^Aç�A×�AËDA�|�A�n�A�^5A�=qA�JA��/A���A°!A�A�A�A�A�AhA\APADAAA�t�A�K�A�$�A��A�ĜA���A�~�A�7LA�JA��A��/A�ȴA��^A���A���A��DA�x�A�jA�K�A�"�A�%A��A���A��FA���A��\A�x�A�^5A�G�A�9XA�$�A���A���A��FA���A�Q�A�{A��A��;A��A���A���A�ƨA�A��^A��A���A��DA��A�|�A�v�A�n�A�l�A�t�A��A��+A��+A�v�A�VA�1'A�
=A��wA�jA�E�A�
=A��A���A��A�`BA�33A��^A�ffA���A��#A���A�ȴA��^A�A��A���A�z�A�ZA�ffA�r�A��A�l�A�K�A�=qA�?}A�K�A�VA�\)A�dZA�bNA�O�A�5?A���A�A���A�l�A�K�A�?}A�&�A���A��A��TA���A��!A��\A�v�A�^5A�E�A�+A�VA��A���A���A���A���A���A��yA�1A��A���A�z�A�\)A�A�A�K�A�33A�%A��TA���A�G�A�33A�/A�/A�1'A�5?A�5?A�;dA�1'A�oA�  A��A��HA�ȴA��9A��A���A�t�A�I�A�1'A�  A��RA�^5A���A�~�A�?}A�9XA�K�A��A�ȴA���A��A�z�A�K�A�33A�-A� �A��A�JA�A��mA���A��FA���A�v�A�33A�bA���A��PA�M�A�1'A�$�A�A��A��A�ĜA��FA�`BA���A��mA��#A���A��FA���A�l�A�/A�  A���A���A���A��9A�ZA� �A��!A�?}A��wA��A�l�A�bNA�;dA��A�A��yA���A��A��7A�G�A��A���A��yA���A���A��7A�bNA�A�A�1'A�33A�C�A�E�A�S�A�ZA�M�A�A�A�-A��A��`A���A�z�A�O�A��A�v�A�A�A�1'A��A��A�{A�{A��A�~�A�
=A��FA���A�z�A�bNA�A�A��A��\A� �A���A�jA�O�A�(�A��`A��9A��!A���A��A���A��PA�\)A� �A��wA�p�A���A�p�A��PA��PA�\)A�|�A�5?A�ȴA��\A�XA�&�A���A�r�A�&�A��mA��A�t�A�S�A�
=A���A��A���A�9XA��jA�M�A���A�p�A�=qA��A�VA�1A���A��A��A��A�A���A�1'A��/A�`BA�{A��mA���A��RA��!A��A�E�A��A��A�%A��uA�/A��`A��\A�VA�ȴA���A�x�A�VA��A���A���A���A��A�bNA�+A��A��-A��A�S�A�%A��wA�`BA���A���A�M�A��A�hsA�I�A�&�A���A��wA���A�p�A�?}A�-A�%A��`A�A��PA�l�A�XA�E�A�;dA�+A�%A��mA���A��FA���A�p�A�Q�A�33A�&�A��A�JA�  A��mA���A��-A�bNA��#A�ZA�JA��A���A�bNA�G�A�oA��
A���A�hsA�?}A�VA�ȴA�x�A�G�A�VA+A~�DA~�A}A}x�A}VA|�`A|��A|�DA|bNA|=qA| �A|  A{��A{��A{t�A{?}Az��AzQ�Ay�Ay7LAyVAx�/Ax��Ax�jAx�Axv�Ax-AwAwO�Av�`AvffAv  Au��Au%At�At=qAr��Ar5?Aq��AqG�Aq%Ap��Ap��ApE�Ao�Am��Am7LAl�AlȴAl�Al�+AljAlZAl5?Al�Al{Ak�Ak�#Ak�^Ak��Ak�Akp�Ak33Ak%Aj�AjȴAj�RAj��Aj�\Aj�Ajv�AjffAj=qAj{Ai��Ai�AiVAh��Ah=qAg�TAgoAe��Ad�/AdQ�Ac��AcƨAc�AcXAc/Ab��Ab�jAb�\Abr�Ab-Aa�#Aa�PAaVA`�+A_�A^�A^�DA^ffA^VA^^5A^E�A^E�A^I�A^A�A^$�A^bA]x�A\-A[l�A[\)A[XA[/A[+A[+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                A٥�A٧�A٩�AٮAٰ!A٩�A٩�Aٰ!Aٰ!Aٲ-Aٰ!AٶFAٺ^Aٺ^Aٺ^Aٺ^AټjAٸRAٺ^AٸRAٺ^Aٲ-A١�A��A�/A׺^A׋DA�I�A��HA�r�A�A�A��A��A��AռjA�~�A�bA�I�A�|�A�(�A�1A���A���AǬA��`AĶFAô9A®A�
=A���A��7A�E�A���A�M�A��+A��^A�ZA�oA�  A�-A���A��A�5?A��A�{A�A�A�O�A��
A��FA�ĜA��^A�ƨA�  A���A�  A�I�A�7LA��A��HA�Q�A�VA�r�A���A�VA�G�A�ffA��
A��hA�%A�`BA���A�l�A��/A���A��TA��RA}��Az�DAv��Apr�Al9XAj�Ag�AbZA^Q�A[7LAY�mAT^5AP^5AL��AI;dAGdZADJA@�A?`BA=��A9��A8�yA8$�A65?A3�mA2~�A1S�A/�A.{A,A�A+oA*ZA)�A(��A(9XA'|�A%��A$�A#hsA"VA"1'A"�A!ƨA!��A!��A!��A!;dA��A~�AƨA�`AbNA�A��A?}A�Az�A  A�PA�yAn�A?}AA�AȴA�TAhsA�A�!Ap�A�-A��A�-A�A�A�DAffAbA�A�AAG�A��A(�A�mAp�A7LA�A�/A~�A�A\)A��A1'A�A  Ax�AK�A��AI�A|�A�/A�A&�A��A�;A\)A?}A
�A
(�A	33AȴAbNA�TA��A��A�A{A?}A�A\)A�!AffA��A��AXA
=AA ��A �A �HA ��A {@�o@��+@��7@�G�@�%@��@�  @�K�@���@�E�@��h@��@���@���@��u@�9X@��;@���@�\)@�"�@�@��/@�D@��@�;d@�+@�^@��@�r�@��@�\)@��@���@�r�@�1@땁@�o@���@�5?@陚@�G�@蛦@��;@�C�@�!@�$�@噚@��`@�1@��
@��@�33@�!@�5?@��@�7@��`@߾w@��@�n�@��@�G�@ܣ�@��;@�C�@��@ڗ�@��@��@��@١�@�X@�7L@�V@ؼj@�1'@�l�@�o@�=q@���@�`B@��@��`@�Ĝ@���@�+@�ff@ѡ�@�`B@��@�Ĝ@�1@ϕ�@�\)@�o@Χ�@�J@�&�@�bN@�1'@��@˥�@ʸR@��@ɩ�@�X@Ȭ@�Z@ȃ@�I�@��
@�t�@��@Ɨ�@���@�hs@��`@�b@�33@�@��@���@���@�I�@��@��@�"�@�ȴ@���@�~�@�^5@��@��^@�G�@���@��;@��@�dZ@�
=@���@�V@��@��7@��@���@��D@�bN@�b@���@���@�M�@��@���@�`B@�7L@��@�1'@���@�l�@�@��R@�ff@��@��^@�hs@�/@��/@��u@��@��@�+@��H@�ȴ@��R@�~�@�@�hs@�V@�Ĝ@�Z@�A�@�b@�|�@�33@��@��y@�V@��#@��^@���@�/@���@��j@���@�I�@���@��@�S�@�o@���@�^5@�{@��#@���@�G�@��/@�r�@�  @���@�33@���@���@�$�@���@��T@�O�@���@�j@��@��P@�+@���@�~�@�^5@�5?@�{@�J@�@��@��@�%@��/@���@�A�@��@���@��w@��P@�\)@�33@���@��+@�5?@��@���@���@�p�@�?}@��@���@��@�Z@�(�@��;@���@���@�33@�
=@��!@�v�@�-@���@��7@��@��@���@��j@�(�@��F@�K�@�S�@�\)@�K�@�@�v�@��@��T@�x�@�O�@�V@���@��@�ƨ@���@���@�|�@�dZ@�
=@�ff@�ff@��\@�V@�@�hs@�&�@���@�Z@�b@��;@��F@���@�t�@�33@��@��!@��\@�V@�-@��h@��@�X@�/@��/@���@��D@�I�@�ƨ@�K�@�@���@���@��\@�v�@�E�@�$�@��@��^@���@��7@��@�G�@��@�%@��@�z�@�A�@�  @��w@���@��@�+@��H@�ȴ@�~�@�M�@�E�@�-@���@��T@���@��^@��-@��7@�/@���@��/@�bN@�b@l�@~��@~v�@}V@|�/@|��@|I�@{�@{o@z�\@y��@yG�@x��@x��@x1'@w�@v�y@v��@u�@u�-@u�@uO�@t�@t9X@s��@s�
@s��@st�@s33@r��@r�\@r�@q��@q��@qx�@q%@pĜ@pbN@p �@o��@o|�@o�@nȴ@nV@m�T@m`B@m�@l�/@l�@lj@l9X@k��@k@j-@iX@h�`@hĜ@h�u@hA�@g�w@g�@g�@g�@g��@g;d@f�R@fv�@e�T@e/@d�D@d�D@dj@dj@d�@c�F@c�@cC�@co@b��@b�!@b�!@b�!@b�!@b�@ahs@aX@a7L@`��@`�u@` �@_�@_l�@_;d@^�@^v�@]@\�@[dZ@Z��@Z�@Y�^@Yx�@Yhs@Yx�@Yx�@X��@X�9@Xr�@W�;@W�P@W;d@V�R@V�+@U@UV@T�j@T��@TZ@S��@S�@R�@R�\@R^5@R^5@RM�@Q�#@QG�@P�@O�@O�w@N�y@N{@M�@M��@Mp�@L��@LZ@Kƨ@K�F@Kt�@KS�@K"�@K@J��@J=q@I��@Ix�@IG�@H��@Hb@GK�@G+@F��@F�y@Fff@E�@E�-@D�j@Dj@Dz�@D9X@D(�@C��@C"�@B��@BM�@B=q@A�^@@��@@r�@@Q�@@b@?�P@>��@>�R@>ff@=��@=�h@=?}@<��@<9X@;��@;S�@;"�@;@:��@:-@9�#@9��@9��@9x�@8��@8Q�@8  @7�P@7K�@6�y@6ȴ@6��@6v�@6v�@6{@5�-@5�@4�@4��@49X@3�
@3dZ@3"�@2�H@2~�@2-@1�#@1�7@1G�@1&�@1%@0��@0bN@0 �@0  @0  @/�;@/��@/�w@/�w@/��@/;d@.�@.ff@.5?@.@-��@-�h@-/@,�j@,z�@,9X@+��@+�@+33@*��@*�@)��@)�#@)�^@)��@)��@)��@)x�@(��@(��@(�`@(�9@(Q�@'�@'��@';d@&ȴ@&�+@&V@&$�@%�h@%O�@%�@$��@$��@$�j@$j@$�@#��@#t�@#S�@#33@"�@"��@"�\@"=q@"�@!��@!��@!��@!�^@!��@!X@!%@ �u@ �@ Q�@   @�w@|�@K�@
=@�y@��@{@�T@�-@��@?}@�/@�j@��@z�@j@9X@�
@��@�@dZ@C�@"�@"�@o@��@�!@n�@^5@^5@M�@=q@J@�#@��@�7@x�@x�@X@&�@%@�9@�u@�u@�@r�@Q�@A�@1'@b@�w@�P@�P@�P@�P@�P@|�@l�@\)@;d@+@
=@�+@$�@��@�@O�@/@V@�j@�D@j@I�@��@��@dZ@dZ@"�@~�@M�@J@��@�@�^@G�@�9@��@��@r�@A�@�;@K�@�y@ȴ@�R@��@��@v�@�T@�@p�@p�@`B@/@�/@��@z�@Z@I�@9X@9X@�@�@1@�m@�
@�
@ƨ@��@��@33@
�!@
~�@
=q@
-@
�@
�@
J@	��@	��G�O�A١�A٥�A٧�A٧�A٧�A٧�A٧�A٩�A٥�A٩�A٩�A٧�AٮA٬A٬Aٰ!AٮAٮAٰ!Aٴ9Aٴ9A٬A٬A٩�A٧�A٧�A٧�A٧�A٧�A٩�A٬Aٲ-Aٰ!Aٰ!Aٰ!AٮAٮAٰ!Aٲ-Aٰ!Aٰ!Aٲ-Aٰ!Aٰ!Aٲ-AٶFAٸRAٸRAٸRAٸRAٸRAٺ^Aٺ^AٸRAٸRAٺ^Aٺ^Aٺ^Aٺ^AټjAٺ^AټjAٺ^Aٺ^Aٺ^Aٺ^Aٺ^Aٺ^AټjAټjAټjAٺ^AٸRAٸRAٸRAٸRAٺ^Aٺ^Aٺ^Aٺ^Aٺ^AټjAپwAپwAټjAټjAٺ^AټjAٺ^Aٺ^AٸRAٸRAٺ^Aٺ^AٸRAٺ^AټjAٺ^AٸRAٺ^Aٺ^Aٺ^Aٺ^Aٺ^Aٺ^AٸRAٺ^AٸRAٸRAٸRAٸRAٸRAٺ^Aٺ^Aٺ^Aٺ^Aٺ^AٸRAٸRAٸRAٸRAٶFAٰ!A٬Aٲ-Aٲ-Aٰ!A٬Aٰ!AٶFAٴ9A٩�Aٗ�Aى7AمAفA�\)A���AؾwAة�Aؕ�AؑhA�|�A�XA�=qA�7LA�/A�&�A���A��A���A׼jA׶FAײ-A׬Aק�Aף�Aם�Aכ�Aו�A׋DAׅA�~�A�t�A�hsA�VA�S�A�VA�I�A�9XA�&�A��A���A���A��A��`Aֺ^A֩�A֗�Aև+A�~�A�t�A�hsA�dZA�\)A�I�A�A�A�E�A�E�A�I�A�C�A�;dA��A��A��#A��TA��A���A�  A�A�  A���A��A��A��yA��mA��TA��TA��TA��HA��#A��#A���A���A�ȴA�ȴA���AվwAվwAոRAմ9Aղ-Aթ�Aա�A՛�AՑhAՃA�t�A�bNA�G�AԴ9A��Aӣ�A�l�A�"�A�ZA�ƨAѰ!Aѩ�Aѥ�Aљ�A�z�A�"�A���A��#Aд9AЅA�^5A�?}A�$�A� �A�{A��`A���A�ƨAρA�+A��/A�z�A�5?A�oA��A͡�A�ffA�9XA���A��
A���A̸RA̗�A�XA�
=A��mA�ȴA˸RA˕�A�~�A�t�A�n�A�M�A�+A�(�A��A�%A��/Aʩ�AʃA�ZA�1'A�oA�  A��yA��
A�ȴAɾwAɲ-Aɥ�Aə�AɍPAɃA�x�A�dZA�S�A�=qA�$�A�JA���A��A��A��yA��`A��HA��/A���A���A���A���A��
A��yA��A��A���A���A���A�A���A�1A�A�%A�
=A�JA�VA�JA�JA�  A��Aȕ�A�`BA�`BA�E�A�/A��A�ȴAǇ+A�`BA�M�A�?}A�1'A�$�A�{A���A���Aƛ�A�jA�I�A�$�A��mAţ�Aś�Ař�Aŗ�Aŕ�Aŏ\AōPAŋDAŋDA�l�A�33A��A��A���Aĥ�AăA�S�A�S�A�Q�A�?}A�9XA�/A��A�1A��A���Aú^Aç�A×�AËDA�|�A�n�A�^5A�=qA�JA��/A���A°!A�A�A�A�A�AhA\APADAAA�t�A�K�A�$�A��A�ĜA���A�~�A�7LA�JA��A��/A�ȴA��^A���A���A��DA�x�A�jA�K�A�"�A�%A��A���A��FA���A��\A�x�A�^5A�G�A�9XA�$�A���A���A��FA���A�Q�A�{A��A��;A��A���A���A�ƨA�A��^A��A���A��DA��A�|�A�v�A�n�A�l�A�t�A��A��+A��+A�v�A�VA�1'A�
=A��wA�jA�E�A�
=A��A���A��A�`BA�33A��^A�ffA���A��#A���A�ȴA��^A�A��A���A�z�A�ZA�ffA�r�A��A�l�A�K�A�=qA�?}A�K�A�VA�\)A�dZA�bNA�O�A�5?A���A�A���A�l�A�K�A�?}A�&�A���A��A��TA���A��!A��\A�v�A�^5A�E�A�+A�VA��A���A���A���A���A���A��yA�1A��A���A�z�A�\)A�A�A�K�A�33A�%A��TA���A�G�A�33A�/A�/A�1'A�5?A�5?A�;dA�1'A�oA�  A��A��HA�ȴA��9A��A���A�t�A�I�A�1'A�  A��RA�^5A���A�~�A�?}A�9XA�K�A��A�ȴA���A��A�z�A�K�A�33A�-A� �A��A�JA�A��mA���A��FA���A�v�A�33A�bA���A��PA�M�A�1'A�$�A�A��A��A�ĜA��FA�`BA���A��mA��#A���A��FA���A�l�A�/A�  A���A���A���A��9A�ZA� �A��!A�?}A��wA��A�l�A�bNA�;dA��A�A��yA���A��A��7A�G�A��A���A��yA���A���A��7A�bNA�A�A�1'A�33A�C�A�E�A�S�A�ZA�M�A�A�A�-A��A��`A���A�z�A�O�A��A�v�A�A�A�1'A��A��A�{A�{A��A�~�A�
=A��FA���A�z�A�bNA�A�A��A��\A� �A���A�jA�O�A�(�A��`A��9A��!A���A��A���A��PA�\)A� �A��wA�p�A���A�p�A��PA��PA�\)A�|�A�5?A�ȴA��\A�XA�&�A���A�r�A�&�A��mA��A�t�A�S�A�
=A���A��A���A�9XA��jA�M�A���A�p�A�=qA��A�VA�1A���A��A��A��A�A���A�1'A��/A�`BA�{A��mA���A��RA��!A��A�E�A��A��A�%A��uA�/A��`A��\A�VA�ȴA���A�x�A�VA��A���A���A���A��A�bNA�+A��A��-A��A�S�A�%A��wA�`BA���A���A�M�A��A�hsA�I�A�&�A���A��wA���A�p�A�?}A�-A�%A��`A�A��PA�l�A�XA�E�A�;dA�+A�%A��mA���A��FA���A�p�A�Q�A�33A�&�A��A�JA�  A��mA���A��-A�bNA��#A�ZA�JA��A���A�bNA�G�A�oA��
A���A�hsA�?}A�VA�ȴA�x�A�G�A�VA+A~�DA~�A}A}x�A}VA|�`A|��A|�DA|bNA|=qA| �A|  A{��A{��A{t�A{?}Az��AzQ�Ay�Ay7LAyVAx�/Ax��Ax�jAx�Axv�Ax-AwAwO�Av�`AvffAv  Au��Au%At�At=qAr��Ar5?Aq��AqG�Aq%Ap��Ap��ApE�Ao�Am��Am7LAl�AlȴAl�Al�+AljAlZAl5?Al�Al{Ak�Ak�#Ak�^Ak��Ak�Akp�Ak33Ak%Aj�AjȴAj�RAj��Aj�\Aj�Ajv�AjffAj=qAj{Ai��Ai�AiVAh��Ah=qAg�TAgoAe��Ad�/AdQ�Ac��AcƨAc�AcXAc/Ab��Ab�jAb�\Abr�Ab-Aa�#Aa�PAaVA`�+A_�A^�A^�DA^ffA^VA^^5A^E�A^E�A^I�A^A�A^$�A^bA]x�A\-A[l�A[\)A[XA[/A[+A[+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�AB�B�AB�AB�AB�vB�B�B�B�;B��B�oB��B��B�B�B��B��B�`B��B�lB��B	B	�pB
_B
IB
�B
kB
B
B
#�B
.IB
<6B
<6B
6�B
1'B
FB
JB
�B	��B	�pB	�B
�B
 'B
#�B
+�B
0UB
2�B
;0B
<6B
@B
T,B
\�B
{�B
��B
iB
�B
�B
��B
��B	B/�B@�BVBc�BC-B@OB?�B7�B>B:�B<BC�BL0BU�Bb�B�B�rB~�Bf�BC-B8�BBuB
՛B
�fB
lWB
aB
R�B
E9B
4B
)*B
B	�VB	��B	�B	��B	�=B	�B	|PB	f�B	^�B	U�B	:�B	$�B	hB	�B��B�]B��B�B�"B�BBǮB��B�wB�B�[B��B�nB��B��B��B��B��B�jB�B��B��B�wB��B�B��B��B͟BیB�#B�;B��B�oB�vB�B�rB	�B		B	
	B	�B	:B	�B	uB	@B	B	kB	!B	&�B	1�B	1'B	C�B	\�B	{B	}�B	��B	��B	�B	��B	�B	��B	�OB	�?B	��B	��B	��B	�3B	��B	�'B	�?B	�KB	�^B	�)B	�^B	��B	��B	�^B	˒B	ɺB	�?B	�B	�aB	��B	��B	��B	�^B	�<B	�B	��B	ƨB	��B	��B	��B	ĜB	�B	ǮB	�B	�B	ɆB	ƨB	�?B	ĜB	�EB	͟B	�RB	�KB	ȀB	�zB	ǮB	�B	�?B	ƨB	��B	�#B	��B	�<B	�<B	�pB	��B	�vB	�vB	�}B	ΥB	�B	��B	�^B	�)B	��B	��B	�#B	ȴB	�RB	��B	�^B	��B	��B	��B	�B	��B	�B	�B	�HB	�}B	�B	�B	��B	�NB	��B	бB	��B	�HB	�B	��B	�B	��B	ӏB	�NB	� B	ѷB	ҽB	��B	�&B	�&B	��B	�,B	�aB	��B	��B	�aB	՛B	՛B	�2B	��B	՛B	՛B	�B	՛B	�B	֡B	��B	�EB	خB	��B	�yB	��B	�EB	��B	�B	��B	��B	�B	�KB	�B	�KB	�B	خB	��B	�KB	��B	��B	�B	��B	�B	�EB	�B	�B	ٴB	ٴB	�B	�B	خB	��B	�B	ںB	�B	ٴB	��B	چB	یB	��B	��B	یB	�WB	�WB	��B	��B	ݘB	ݘB	��B	�B	��B	��B	�B	��B	�B	�B	�8B	�B	�B	�B	�DB	��B	�KB	�B	�B	�QB	�B	��B	�B	�WB	�WB	�B	�WB	��B	��B	��B	��B	�cB	�B	��B	� B	� B	�5B	�B	�oB	��B	�B	�vB	�AB	��B	�B	�B	�B	�%B	�+B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	�B	�xB	�xB	��B	�B	��B	�VB	�"B	�"B	��B	��B	��B	��B	��B	��B	�]B	��B	�]B
 iB	��B	�cB	��B
  B	�cB	��B
 iB	��B	��B	�cB
 4B
 �B
;B
oB
oB
;B
�B
�B
B
B
AB
uB
�B
�B
GB
GB
B
�B
�B
B
�B
�B
%B
+B
�B
�B
�B
�B
	�B

	B
	�B

=B

	B
	�B
B
xB
�B
�B
B
�B
�B
�B
"B
�B
�B
(B
\B
�B
 B
�B
:B
�B
B
@B
�B
�B
B
B
�B
B
MB
�B
�B
SB
SB
�B
�B
$B
�B
�B
�B
�B
�B
	B
CB
CB
�B
B
B
�B
OB
OB
�B
�B
�B
OB
�B
!-B
!bB
 �B
"4B
"�B
"�B
"�B
"�B
!�B
!�B
"�B
#�B
$B
"�B
"�B
$@B
$B
$@B
$tB
$B
$tB
%B
%zB
%FB
%�B
&�B
&B
&LB
($B
'�B
(�B
(�B
)�B
*0B
*�B
*�B
*0B
*�B
*�B
+6B
+�B
+kB
+�B
+�B
,B
,�B
-wB
-�B
-wB
-wB
-�B
-CB
-wB
.IB
.IB
/OB
/�B
0UB
/�B
0�B
1�B
1�B
1[B
2aB
2-B
2-B
2aB
2aB
2�B
2�B
33B
2�B
3hB
3�B
3�B
3�B
4�B
49B
5?B
5tB
6zB
7�B
7LB
7LB
7LB
8RB
8B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
<B
<B
<�B
=B
=B
=<B
=qB
>B
?HB
?�B
?�B
?�B
@B
@B
@B
@B
@OB
@�B
@OB
@OB
@�B
@�B
A B
AUB
A�B
A�B
A�B
B'B
B�B
C-B
B�B
CaB
C�B
C�B
C�B
D3B
C�B
C�B
C�B
CaB
C�B
C�B
C�B
EB
E9B
EmB
E�B
E�B
GB
GB
F�B
GzB
GB
F�B
GB
GzB
HB
IRB
IRB
IRB
IRB
I�B
JXB
J�B
J�B
J�B
J�B
K�B
L0B
K�B
K�B
L0B
LdB
MB
M6B
MB
M6B
M6B
MjB
M�B
MjB
NpB
NpB
OvB
P}B
P�B
P�B
P�B
QNB
RTB
RTB
Q�B
Q�B
QNB
Q�B
QNB
QNB
R�B
R�B
S&B
R�B
S&B
S[B
T�B
T�B
UgB
VB
V9B
V9B
V�B
V�B
W�B
XB
X�B
X�B
X�B
XyB
Y�B
Z�B
ZB
ZQB
ZB
Y�B
ZQB
ZB
ZB
ZQB
ZQB
Z�B
[#B
Z�B
Z�B
Z�B
[WB
Z�B
[WB
[�B
\]B
]/B
\�B
]/B
]�B
\�B
_;B
_pB
_�B
`BB
_�B
_pB
_;B
_�B
`BB
`�B
`vB
`�B
`�B
a|B
a�B
a�B
bB
b�B
b�B
b�B
b�B
c�B
d�B
d�B
d�B
d�B
e`B
e�B
ffB
f�B
ffB
f�B
gB
gB
g8B
h
B
h
B
hsB
h>B
hsB
hsB
hsB
h�B
iDB
jB
jKB
jB
j�B
kB
k�B
k�B
l"B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
o B
o B
o5B
o5B
o5B
o B
oiB
o�B
pB
poB
p�B
p�B
qB
qB
q�B
q�B
rB
rGB
rGB
r�B
r�B
s�B
tB
s�B
tB
tTB
tTB
tTB
tB
tTB
u%B
t�B
t�B
u%B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
w�B
w�B
w�B
xB
xB
w�B
xlB
x�B
y	B
x�B
y	B
y>B
yrB
yrB
y�B
y�B
zB
zB
zB
zDB
zB
zB
zB
z�B
z�B
z�B
{JB
{B
{B
{B
{�B
{�B
|B
|�B
}VB
}VB
}�B
}"B
~(B
~]B
~]B
~]B
~�B
~]B
~�B
.B
.B
cB
�B
�B
� B
� B
�iB
��B
��B
�;B
�;B
�;B
�oB
��B
��B
�AB
�uB
�uB
�uB
�uB
��B
�B
�{B
��B
�{B
�{B
��B
��B
�MB
�MB
��B
��B
�YB
��B
��B
��B
��B
��B
�+B
�+B
�_B
�_B
��B
��B
�fB
�1B
�1B
�fB
�fB
�1B
��B
��B
�7B
�7B
�7B
��B
��B
��B
��B
�=B
��B
��B
�B
�B
�B
�B
�xB
��B
�B
�B
�B
�JB
��B
�~B
�~B
�~B
��B
�~B
�~B
�JB
��B
��B
��B
��B
��B
�"B
�VB
��B
��B
��B
�(B
�(B
�\B
�\B
�\B
�\B
�\B
��B
��B
��B
��B
��B
�bB
� B
� B
��B
��B
��B
��B
��B
�B
�:B
��B��B�B�B�vB�vB��B�GB�B�|B�B�B�GB�B�B�;B�B��B�vB�B�;B�oB�AB�vB�AB�B�vB�vB�B�vB�B�B� B�;B�B�B��B�B�oB�oB�AB�B�B��B�B�B�B�B�oB�B�B��B�;B�iB�B�B�iB�B�B�iB�iB��B�5B�B�B�B�B�B�B��B�iB��B�oB�B�vB��B�B�AB�B�vB�B�B�B�GB�B�TB��B�ZB��B�ZB�%B�ZB�ZB��B��B�+B�ZB�`B��B�`B�+B�ZB��B��B�B��B��B��B�`B��B�8B�rB��B��B�fB�B�8B�lB�rB�	B��B�DB�xB��B��B�PB��B	B��B	oB�B�MB�B	uB	,B	7�B	C�B	c B	�BB	��B	�VB
oB
B
�B
�B
$B
�B
�B
B
 'B
�B
!�B
IB
B
�B
�B
B
B
�B
�B
	B
�B
eB
�B
kB
eB
�B
�B
1B
CB
�B
�B
�B
�B
�B
kB
qB
"4B
�B
!�B
�B
~B
B
�B
�B
	B
qB
B
!bB
!�B
&LB
&�B
(�B
/�B
-�B
&�B
&B
*0B
/�B
6B
8RB
;dB
=B
;�B
<6B
=B
<�B
=B
<�B
<6B
<�B
;�B
<6B
=<B
9XB
9�B
9�B
7�B
7�B
6zB
5�B
4�B
49B
5?B
49B
4�B
5B
2�B
.�B
-CB
*�B
B[B
�B
�B
DB
PB
%zB
  B	��B
 iB
AB
�B
�B
�B
DB
�B
kB
FB
1B
SB
B
:B
uB
�B
�B
B
#�B
�B
�B
1B
�B
JB
	B
(B
B
YB
�B	��B	��B	��B	��B
  B	�>B	�AB	��B	�B	�B	�B	�NB	�B	��B	�sB	��B	�gB	�sB	�B	�mB	�2B	�aB	��B	ΥB	�0B	�0B	�XB	ɆB	ɺB	��B	��B	�XB	�XB	ɺB	ʌB	̘B	�B	�HB	�,B	�gB	�mB	��B	��B	�B	�sB	�vB	�B	�2B	��B	�(B
uB
�B
	B
JB
JB
�B
�B
�B
�B
B
 B
(B
�B
B
oB
:B
B
B
�B
1B
'�B
%B
~B
$tB
�B
"�B
#:B
"�B
"hB
OB
�B
�B
�B
~B
�B
�B
 �B
B
�B
�B
�B
(�B
'RB
'RB
'�B
(�B
)�B
)*B
(XB
(XB
0!B
,qB
*�B
.}B
'�B
*�B
-B
,�B
)_B
)�B
,�B
,B
,�B
-B
-wB
.�B
1[B
0�B
1[B
1�B
2aB
1�B
1'B
1�B
1�B
3�B
0�B
2�B
3hB
4�B
4B
33B
2�B
2-B
2�B
2�B
2�B
2�B
3�B
2-B
1'B
8RB
9�B
<�B
@�B
?}B
D�B
I�B
@�B
>�B
<jB
;�B
;�B
;dB
:�B
;dB
;dB
:�B
;�B
<B
:�B
=qB
?}B
?B
@�B
?HB
A�B
B�B
@�B
@B
A B
JXB
J�B
LdB
S�B
XEB
V�B
ZB
W�B
W�B
W�B
YB
XyB
Z�B
\]B
^�B
^B
^�B
^�B
^5B
^�B
`B
b�B
e�B
q�B
v`B
v�B
{�B
��B
��B
�bB
��B
��B
��B
��B
�uB
�=B
�B
��B
��B
�B
r�B
dZB
^jB
XEB
d�B
d�B
jB
sMB
p�B
p�B
o�B
q�B
}�B
��B
��B
��B
�B
�B
��B
�'B
��B
�9B
�RB
�dB
��B
˒B
ǮB
��B
�RB
ȀB
�B
�dB
�aB
�B
�jB
��B
�
B
�iB
�B
�|B
��B
�B
�fB
��B
��B
��B
�"B
��B �B;BDB	B �B	B �BB,qB8�B>wB7BB'B@B@�BA�B@�B@OB?�B?�BF?BK�BRTBU�BYKB[#B[WBYBV�B]dBf�Bb�Bh�BiyBf�Ba�BXBO�B3hB1�BB�BS�BC�B@�B=<B?HBE9BA BA B?�BA�B?}BA�BC�B@�B>BB<B=�B?HB6zB9$B6zB5�B1[B2�B8�B9�B9�B=<B;�BN�B?}B:^B8�B8B7�B:*B?�B?B<�B9�B8�B:�B<jBC�B:*BFB<6BI�BHBC�BEmBI�BLdBK�BK�BM�BN�BT�BU2BT,BR�BT�BX�BV�BXB\]B^jB\�B^5BqvBx�B|PB�lB��B�oB��B��B�MB�@B��B�lB��B��B~�B|PB|�B|�B|Bz�B~(B�~B�Bp;BjBe�Bb�B`vBj�BV�Ba|BT�B9�B>�B9�B<�B2-B0UB1[B5�B=qB<jB=�B=qB7�B)�B�B�B$B_B
��B	�B-CB"�B�BbBB�B
��B
�|B
�yB
��B
�TB
̘B
ȴB
�qB
��B
�FB
��B
�B
}�B
}�B
t�B
t�B
n�B
k�B
jB
iyB
e`B
e`B
~�B
oiB
m�B
j�B
b�B
m�B
V9B
W
B
R�B
MB
M�B
T�B
T,B
T�B
W?B
UgB
Q�B
G�B
E�B
J�B
H�B
@�B
<B
6�B
:�B
8�B
4�B
2�B
4�B
0UB
+kB
/�B
/B
*�B
&�B
(XB
(�B
$@B
%B
!bB
�B
=B
B
FB
	7B

�B
�B

�B
MB
1B
SB
�B
uB	��B
oB	��B	��B	��B	��B	��B	��B	�8B	��B	�B	�B	�B	�GB	�B	�)B	�yB	�B	�B	�8B	�B	��B	�B	�B	��B	�HB	ںB	��B	�aB	�&B	�B	�B	��B	ɺB	��B	�[B	�wB	��B	�qB	�<B	��B	�jB	�UB	��B	��B	��B	�B	��B	��B	�nB	�-B	��B	�!B	�B	��B	�~B	��B	�CB	��B	��B	��B	��B	��B	�FB	��B	��B	��B	��B	��B	�hB	�\B	�(B	�	B	�lB	�_B	�B	�;B	�oB	�zB	v�B	��B	v`B	zDB	u�B	s�B	yrB	��B	�B	k�B	pB	iB	h>B	iB	h
B	f�B	h>B	e�B	c�B	g�B	e�B	c�B	c�B	d&B	_�B	b�B	c�B	`B	^B	_B	]�B	\�B	[�B	[#B	Y�B	\)B	ZB	ZB	[WB	Y�B	V�B	U�B	P�B	_pB	aB	PB	L0B	D�B	AUB	A B	>�B	<�B	=<B	?}B	6zB	7�B	9�B	6FB	5�B	7�B	3�B	=<B	5?B	+kB	#�B	$@B	 �B	 'B	!B	�B	 'B		B	qB	&B	9XB	JB	�B	�B	VB	VB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                B��B�B�B��B��B��B�bB��B�-B��B�-B��B�vB�\B�B�B�B�B�B�$B�DB��B	gB	��B

�B
�B
~B
B
�B
�B
�B
�B
,�B
-CB
)yB
.IB
�B
EB
B	�/B	ňB	�B
KB
�B
	B
!�B
&fB
'�B
3�B
2�B
7�B
HKB
PHB
v`B
�GB
\�B
��B
�ZB
��B
�fB
��B$�B33BK�B]�B9	B3�B6�B-�B4�B1'B6�B:xBBBI�BTFB��B�oBx�Bb�B:B8�B�BB
ܬB
�GB
j�B
\B
S@B
?cB
.IB
)*B
B	�B	�}B	��B	�,B	�EB	� B	zB	\�B	Z�B	VSB	8�B	�B	�B		�B��B�B�B�B��B�)B�qB��B�dB�B�,B�_B��B�;B��B��B��B��B�[B�aB�B��B�'B�oB��B�2B��B��B̳B�JB�B�7B��B�4B��B�B�dB�PB��B	 B	�B	{B	B	�B	�B	�B	�B	�B	#�B	B	1'B	K�B	n�B	p!B	t�B	{JB	zxB	��B	�FB	�	B	��B	��B	��B	��B	�FB	�&B	��B	��B	��B	�JB	�qB	��B	�qB	�"B	��B	��B	�qB	�6B	�	B	�B	�ZB	��B	�VB	�B	��B	��B	��B	�]B	��B	�%B	��B	�aB	��B	�XB	��B	�$B	�0B	�qB	��B	�RB	��B	�	B	�OB	�0B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�]B	��B	��B	�.B	��B	��B	�uB	��B	�OB	�OB	��B	�jB	�jB	��B	��B	��B	�*B	�0B	�jB	��B	��B	��B	�B	�;B	��B	�'B	�AB	��B	�B	�OB	��B	��B	�-B	��B	�uB	��B	��B	��B	��B	�B	�B	��B	��B	ÖB	��B	�MB	ŢB	�gB	��B	żB	��B	�B	ƎB	�YB	�?B	ǮB	ƎB	�%B	�tB	�+B	�B	�B	��B	��B	�7B	��B	��B	�#B	��B	�XB	��B	��B	�DB	�=B	�DB	��B	�#B	�XB	ʌB	�#B	�	B	��B	�rB	�DB	�=B	��B	�xB	�=B	�rB	��B	�B	�XB	�xB	ˬB	�xB	ʌB	��B	�#B	�B	�B	ʌB	��B	�DB	�JB	͹B	��B	��B	̘B	̘B	͟B	�pB	�<B	��B	�\B	��B	�bB	��B	�SB	��B	ևB	��B	خB	��B	�EB	�1B	��B	��B	ۦB	یB	ܒB	�B	ܒB	�B	�/B	ݲB	�/B	�/B	�dB	�dB	�B	�5B	ބB	�;B	ߤB	�pB	�B	�BB	�'B	�vB	��B	��B	��B	�4B	�hB	�B	�TB	�&B	�B	�B	�B	�B	�B	��B	�B	�XB	�XB	�*B	��B	��B	�eB	�B	�B	�B	�B	�B	�WB	��B	�wB	�cB	��B	��B	��B	�cB	�iB	��B	�B	�B	�5B	�B	�B	�vB	�UB	�oB	�vB	�vB	�;B	�B	��B	�B	��B	�;B	�vB	�B	�GB	�aB	�|B	�B	��B	��B	�B	�B	�B	��B	�B	�B	��B	��B	�ZB	��B	��B	��B	��B	�`B	��B	�8B	�8B	�$B	�XB	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�"B	�(B	��B	��B	�B	��B	��B
  B
 iB
UB
'B
�B
B
{B
�B
3B
�B
�B
9B
?B
�B
%B
B
�B
B
EB
�B
�B
B
fB
	B

	B
	�B
	�B

�B
�B
�B
�B
6B
�B
�B
�B
�B
�B
�B
B
�B
\B
�B
�B
:B
�B
&B
uB
�B
�B
FB
TB
 B
�B
MB
MB
�B
aB
MB
B
2B
MB
�B
gB
B
�B
SB
�B
sB

B
�B
�B
�B
B
�B
�B
�B
�B
CB
�B
�B
�B
B
�B
)B
�B
�B
�B
�B
5B
OB
B
jB
�B
B
�B
;B
VB
 \B
 �B
!-B
 �B
!�B
"�B
"NB
"hB
#:B
"�B
"�B
#TB
# B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%B
%�B
%zB
&LB
&�B
(>B
(sB
($B
(>B
(�B
)DB
)DB
)�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
-)B
-wB
-�B
-�B
./B
.�B
.�B
/�B
0oB
0oB
0�B
1B
0�B
1B
1B
0�B
1AB
1AB
1'B
1vB
1�B
1�B
2-B
2|B
2�B
2�B
3B
3�B
4B
3�B
4B
4nB
4�B
4�B
5ZB
5%B
5%B
4�B
4B
4TB
4�B
5B
5�B
5�B
5�B
6zB
6�B
8B
7�B
8B
8�B
88B
72B
7�B
8B
8�B
:DB
:B
:B
:B
:xB
:�B
;B
;B
;0B
;�B
="B
<�B
<�B
<�B
=B
=VB
=�B
=�B
=�B
>(B
>(B
>�B
?.B
?.B
?�B
?}B
@iB
A;B
AUB
AoB
AoB
B[B
CB
C-B
B�B
B�B
B'B
B�B
B'B
B�B
C�B
C�B
C�B
C�B
DMB
DB
E�B
E�B
F%B
F�B
F�B
G+B
G�B
G�B
H�B
H�B
J	B
I�B
IRB
I7B
J�B
K�B
K)B
K^B
J�B
J�B
J�B
J�B
J�B
KB
KxB
K�B
K�B
K�B
KxB
LB
L�B
K�B
LB
LdB
MjB
N"B
M�B
N�B
N�B
M�B
O�B
PB
P�B
QNB
P�B
PHB
O�B
P�B
Q�B
Q�B
QB
Q�B
Q�B
R�B
R�B
R�B
S&B
S[B
S[B
S�B
S�B
T�B
UMB
U�B
U�B
U�B
V�B
V�B
W$B
W?B
W$B
W�B
XB
W�B
X+B
X�B
X�B
YB
X�B
Y1B
X�B
YeB
Y�B
ZQB
Z�B
[#B
[qB
[�B
\B
\�B
\�B
]B
]dB
]�B
^B
^OB
^jB
^�B
^�B
_!B
_�B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`\B
`�B
`�B
a-B
abB
a�B
a�B
a�B
b�B
b�B
b�B
c B
c:B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
e`B
ezB
fB
f�B
ffB
f�B
gB
gB
g�B
g�B
g�B
hXB
hXB
hsB
h�B
h�B
h�B
iDB
i�B
i�B
iyB
i�B
i�B
j0B
j0B
jB
jB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l"B
l=B
l=B
l=B
l�B
l�B
l�B
m�B
nB
nB
n/B
nB
o B
o B
o B
o B
o5B
oB
o�B
o�B
o�B
pB
p;B
p;B
p�B
p�B
q'B
qvB
q[B
q�B
q�B
q�B
rB
r�B
r�B
r�B
sB
r�B
r�B
sB
s�B
s�B
tTB
tTB
tB
tB
tTB
tTB
t�B
t�B
u%B
u�B
wB
wB
wLB
w�B
wLB
wfB
w�B
w�B
xB
xB
x8B
y	B
yXB
y$B
x�B
y$B
y	B
x�B
yrB
y�B
y�B
y�B
zB
z�B
z�B
z^B
z�B
{dB
{dB
{�B
{�B
{�B
{�B
|B
|�B
|PB
|�B
|�B
|�B
}<B
}�B
}VB
}"B
}"B
}<B
}"B
}<B
}VB
~�B
~wB
~wB
~�B
~�B
~�B
B
cB
�B
�B
�B
�B
� B
�B
�B
� B
�B
�OB
�4B
�4B
�iB
��B
�UB
��B
��B
�'B
�'B
�'B
�'B
�[B
��B
��G�O�B�bB�-B�B��B��B�hB��B�4B�B�B�B��B��B�B��B�B�hB��B�B��B��B��B��B��B�B��B��B�B��B�-B�-BߊB��B��B�-B�bB�B��B��B��B�-B�-B�bB�-B�-B��B��B��B�'B��B�bB��B��B��B��B��B�'B�'B��B��B�\B߾B��B��B��B�'B�'B��B�\B��B�\B��B�4B��B�hB�B��B�B��B�B�B�B��B�@B��B�zB��B�FB��B�B��B��B�zB�LB�B��B��B�B��B�B��B�B�RB�B�XB�XB�RB��B�RB��B��B�XB�$B��B�B��B��B��B�B�=B��B�B�B�B��B�B�B�B��B�@B��B�B	�B	xB	($B	4B	S�B	��B	�B	��B	��B	�aB	�B
-B
zB
�B
	RB

XB
}B
B
 B
�B
dB
B
�B
dB
dB

�B
�B
^B
0B
	�B

#B

�B
	�B
	�B
	B
	�B
�B
B

#B
6B

�B
	�B

�B
�B
�B
0B
TB
BB
�B
jB
0B
)B
^B
�B

XB
�B
TB
�B
�B
B
�B
5B
?B
mB
�B
 BB
&fB
(�B
+�B
-]B
,"B
,�B
-]B
,�B
-]B
-)B
,�B
-)B
,"B
,�B
-�B
)�B
*KB
)�B
(
B
(>B
&�B
%�B
$�B
$�B
%�B
$�B
%,B
%`B
# B
;B
�B
#B
2�B

�B
 OB	��B	��B
�B	�UB	�B	�B	�B	�B	�HB
EB	��B
�B

�B
�B
	�B
�B
mB
�B
�B
	RB
?B
mB
,B
6B
B
	�B
 B	��B	�XB	�}B	�aB	��B	�?B	�IB	�6B	�CB	�CB	�UB	�B	�B	�'B	�	B	��B	� B	ңB	�\B	�FB	��B	�1B	żB	��B	��B	��B	ňB	ĶB	�;B	��B	��B	��B	��B	��B	�B	�DB	�DB	��B	��B	�B	��B	��B	�]B	��B	āB	żB	��B	�=B	�B	� B	��B	��B	�B	�B	�0B	�}B	��B	��B	�XB	��B	��B	�B	��B	�B	�B	�jB
UB	�}B	�BB
[B
�B
�B
aB
gB
	RB
	�B
�B
gB
�B
�B
)B
&B
�B
�B
�B
�B
�B
6B
6B
�B
B
B
B
dB
	B
	�B
�B
KB
�B
�B
�B
�B
B
B
�B
�B
 vB
�B
#B
�B
�B
�B
dB
�B
�B
B
/B
]B
�B
dB
�B
B
!�B
 �B
!�B
"B
"�B
"NB
!|B
!�B
"NB
#�B
!HB
# B
#�B
%,B
$ZB
#�B
# B
"�B
"�B
# B
#TB
"�B
#�B
"�B
!|B
(�B
*B
-)B
1B
/�B
4�B
9�B
1B
/ B
,�B
,"B
+�B
+�B
*�B
+�B
+�B
*�B
+�B
,WB
*�B
-�B
/�B
/iB
1AB
/�B
1�B
2�B
1AB
0oB
1vB
:�B
:�B
<�B
C�B
H�B
G+B
JrB
G�B
G�B
G�B
I�B
H�B
KB
L�B
O(B
NVB
O(B
O(B
N�B
N�B
PbB
R�B
U�B
a�B
f�B
gRB
l"B
r�B
z�B
��B
��B
��B
|�B
��B
��B
zxB
{JB
|B
sB
rGB
c:B
T�B
N�B
H�B
UB
UB
ZkB
c�B
`�B
a-B
`'B
a�B
nB
}�B
�+B
�B
�MB
�YB
��B
�bB
�-B
�tB
��B
��B
�B
��B
��B
�B
��B
��B
�LB
��B
ĜB
�?B
ΥB
�,B
�EB
ߤB
��B
�B
�B
��B
�B
��B
�B
��B
�]B
�5B
��B
�vB
�BDB BDB4BVB�B)*B.�B'RB2aB0UB0�B1�B0�B0�B/�B/�B6zB<BBuBE�BIlBKDBKxBI7BF�BM�BV�BR�BX�BY�BV�BRBH1B?�B#�B!�B33BDB49B0�B-wB/�B5tB1[B1[B0!B1�B/�B1�B3�B1'B.}B,=B-�B/�B&�B)_B&�B%�B!�B#B(�B)�B)�B-wB+�B?B/�B*�B)*B(XB'�B*eB0!B/OB-B*0B(�B*�B,�B4B*eB6FB,qB:*B8RB4B5�B9�B<�B;�B<B>B?BD�BEmBDgBB�BEBIBGBHKBL�BN�BM6BNpBa�Bh�BlqBy�B��B��B��B��B�mB�aB|�By�B��Bw�Bn�BlqBl�BmBl=BkBnIB|�Bo�B`\BZ�BV9BS&BP�B[#BGBQ�BEB)�B/B)�B,�B"hB �B!�B&B-�B,�B-�B-�B($B�B�B�BFB
��B
�)B
��B~B�B�B �B
�VB
�B
��B
�B
ٴB
�B
B
��B
��B
��B
�B
��B
|6B
utB
nB
nB
d�B
eB
^�B
[�B
Z�B
Y�B
U�B
U�B
n�B
_�B
]�B
[=B
R�B
^OB
F�B
G_B
CB
=VB
>(B
D�B
D�B
EB
G�B
E�B
A�B
8B
6+B
;B
9	B
1AB
,WB
'B
*�B
)DB
%,B
#TB
$�B
 �B
�B
 BB
pB
#B

B
�B
KB
�B
�B
�B
DB
�B
�B
�B	��B	�B	��B	�JB	��B	��B	��B	�GB	��B	� B	��B	�cB	�WB	�B	�B	�,B	�fB	�B	�B	��B	�B	�&B	�B	�B	ݘB	��B	�sB	�
B	רB	�,B	�TB	�B	�B	�dB	ѷB	�)B	�UB	��B	ÖB	�qB	�qB	�0B	�*B	�BB	��B	��B	�CB	��B	��B	�VB	��B	��B	�'B	�7B	�_B	��B	�B	�FB	��B	��B	�bB	��B	��B	�\B	��B	�JB	��B	� B	�(B	�B	�7B	�GB	��B	�OB	HB	B	�UB	�[B	��B	�B	�B	z�B	y�B	w�B	}�B	q�B	q�B	��B	gB	shB	f�B	j�B	fB	d@B	i�B	{dB	q�B	\B	`�B	Y�B	X�B	Y�B	X�B	W$B	X�B	V�B	TFB	X_B	V�B	TB	T{B	T�B	PbB	SuB	T{B	P�B	N�B	O�B	N"B	M�B	LJB	K�B	JrB	L�B	J�B	J�B	K�B	JrB	G_B	F%B	AoB	O�B	Q�B	@�B	<�B	5%B	1�B	1�B	/5B	-]B	-�B	0B	'B	(>B	*B	&�B	&2B	(
B	$&B	-�B	%�B	�B	,B	�B	B	�B	�B	BB	�B	�B	�B	�B	)�B��B	3B�B��B��B�}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<SPo<,�</��<�t�<E=<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(՟<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'%�<#�
<#�
<#�
<#�
<#�
<#�
<,b<#�
<#�
<#�
<#�
<#�
<#�
<#�
<@k<#�
<�[�<|��<\j�<�g�</]E<fc�<0��<�?�<#�
<$m<}�<m�2<Jm�<��<���<1��<NKY<��<X�6<#�
<;��<���<V�m<+�<#�
<��<]ĵ<Od<?�<#�
<8�<9��<#�
<#�
<H��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0151(+/-0.0023)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0151(+/-0.0023)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0];                                                                                         PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0];                                                                                         PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2020091716375720200917163757IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020092717012020200927170120QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020092717012020200927170120QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014120210427140141IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                