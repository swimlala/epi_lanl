CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-11-06T08:25:03Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20201106082503  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               h   hAA  AOAO7314_008642_104                 7314_008642_104                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�EP�	�@�EP�	�11  @�EPPH�@�EPPH�@1S6z��@1S6z���b䞭Ր��b䞭Ր�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?��H@@  @�  @�  @�  @�p�@��RA  A ��A,(�A@  A`��A�  A�  A�  A���A�Q�A�  A߮A�  B Q�B�
B�
B�
B   B'�
B/�
B8  B@(�BH(�BO�
BX  B`  Bh  Bp  Bx(�B�  B�  B�(�B�(�B�  B�  B��B��B�  B�{B�{B�  B�  B��B��B��B�  B��B��B��B�  B��B�  B�{B��B�  B�  B�  B�  B�  B�  B�  C   C
=C  C
=C��C	�C  C
=C��C��C  C  C  C��C��C  C   C"  C$  C&
=C(  C)��C,  C-��C/�C2  C4{C6
=C8  C:
=C<
=C>  C?��CA��CC�CF  CH{CJ{CL
=CN
=CP
=CR  CT  CU��CX  CY��C[��C^  C`  Cb  Cc��Cf  Ch  Ci��Cl
=Cn  Co��Cq�Ct  Cv  Cx  Cz
=C{��C}��C�  C�  C�C�  C���C�  C���C�  C�  C�C�
=C�C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C���C�  C�  C���C�  C���C�  C�C�C�  C�  C���C���C�C�
=C�  C���C�  C�
=C�C�C�C�C�  C���C���C�  C�C�C�  C���C�  C�  C���C�C�C���C���C�  C�C���C�  C�  C���C���C���C�  C�
=C�
=C�\C�C�  C�  C�  C���C�  C���C���C�  C�C�C�  C�  C�C���C���C�  C�C�  C�C�
=C�C���C���C���C�  C���C���C�  C�  C�C���C�  C�C�  C�  C�  C�C�C�C�C�  C�  C�C�C���C�  C�  C�  C�  C���C�  C�C���D z�D �qD}qD�qD� D  D� D  D� D  D� D  D� D�D��D�D� D�qD	}qD	�qD
� D
�qD}qD�qD� D  D��D  D� D�D��D  D}qD�qD}qD�qD� D�qD}qD�D��D  D� D  D� D�D� D�qD��D  D� D�D}qD  D��DD��D  D}qD  D� D�D� D�qD � D!�D!� D"  D"��D#�D#��D$  D$}qD%�D%�D&D&� D'  D'}qD'�qD(� D)  D)� D*D*��D+�D+��D+�qD,}qD,�qD-� D.  D.� D/�D/� D/��D0}qD1  D1}qD2  D2� D2�qD3}qD4  D4��D5�D5��D5�qD6z�D7  D7��D7�qD8� D8�qD9}qD:  D:��D;�D;}qD;�qD<��D=�D=� D>  D>��D?  D?� D@  D@}qDA�DA��DA�qDB}qDC  DC� DD�DD��DE  DE}qDF  DF��DGDG��DH�DH� DH�qDI}qDI�qDJ}qDJ�qDK�DL  DL}qDL�qDM� DN�DN��DO  DO}qDO��DP� DQ�DQ� DR�DR��DR�qDS� DT  DT� DU  DU��DV  DV� DW  DW��DX�DX��DY�DY��DZ  DZ� D[  D[� D\  D\� D]  D]��D^  D^}qD^�qD_� D`  D`��Da�Da�Db  Db}qDc  Dc}qDc��Dd}qDe  De��De�qDfz�Df��Dgz�Dg��Dh}qDh�qDi� Di�qDj}qDk  Dk��Dl�Dl� Dl�qDm}qDn  Dn��DoDo��Dp  Dp� Dq  Dq}qDq�qDr��DsDs��Dt�Dt� Du  Du��Dv  Dv}qDw  Dw��Dx  Dx� Dx�qDyz�Dy�qDz� D{  D{� D|D|��D}  D}� D}�qD~��D  D}qD�HD�@ D�~�D�� D��D�@ D�� D�D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D��HD�  D�=qD�~�D�� D���D�@ D���D��HD�  D�@ D�~�D���D�  D�@ D�~�D���D��qD�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D��HD���D�  D�AHD��HD�� D�  D�@ D�~�D���D�  D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D���D�>�D�~�D���D�  D�@ D�� D���D���D�@ D�~�D�� D�HD�AHD��HD���D�  D�@ D�� D�D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�>�D�~�D��HD�  D�>�D��HD�� D��qD�=qD�� D��HD�HD�AHD�� D��HD�  D�AHD��HD�D�  D�>�D�� D��HD�HD�@ D�~�D���D�HD�>�D�~�D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D�HD�AHD�� D�� D���D�>�D�~�D�� D���D�@ D��HD�� D�  D�>�D�� D�� D�  D�AHD�� D���D���D�>�D�~�D�� D�  D�AHD�~�D���D�  D�=qD�� D�� D��qD�>�D�� D�� D���D�>�D�~�D��HD�HD�AHD��HD�� D�  D�>�D��HD��HD�  D�@ D�~�D��qD���D�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�@ D��HD���D�  D�@ D�~�D���D�  D�@ D��HD�� D��D�B�D�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�@ D�}qD���D���D�>�D�� D�� D���D�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD D�� D�  D�@ DÁHD��HD�  D�@ DĀ D�� D�HD�AHD�~�Dž�D�HD�AHDƀ D�� D�HD�AHDǀ D��HD�  D�@ DȀ D��HD�  D�>�D�~�D�� D�  D�@ D�~�D��HD�HD�@ D�~�D˾�D�  D�@ D�~�D̾�D�  D�@ D�~�DͽqD�  D�AHD΂�D��HD�  D�@ D�~�DϾ�D���D�@ DЁHDо�D�  D�AHDт�D��HD�  D�@ DҀ D�� D�  D�AHDӀ D�� D��D�AHDԀ D��HD�  D�>�D�~�Dվ�D�  D�@ DցHD��HD�HD�@ D׀ D�� D�  D�@ D؀ Dؾ�D�  D�B�DفHD�� D���D�AHDڀ Dھ�D�  D�>�Dۀ D��HD�  D�@ D܀ D�� D�  D�>�D�~�Dݾ�D�  D�AHDހ D޾�D�  D�AHD߀ D�� D�HD�@ D�� D�� D���D�>�D�~�D�� D�  D�=qD�~�D⾸D�  D�@ D� D��HD�  D�>�D�~�D�� D���D�=qD�~�D�� D���D�=qD�~�D�� D�  D�>�D�~�D�� D��D�AHD�HD�� D�  D�>�D�~�D龸D�  D�@ D� D��HD�HD�@ D� D�� D�  D�@ D� D�� D�  D�@ D�~�D���D���D�>�D�~�DD���D�@ D� D�� D�  D�AHD�~�D�� D�  D�=qD�~�D�D���D�@ D� D�� D���D�>�D� D�� D�HD�@ D� D��HD�  D�AHD���D��HD�  D�AHD�� D���D���D�>�D�� D��HD�  D�@ D��HD�� D���D�AHD��HD��HD��\D�/\?8Q�?B�\?u?���?�33?��?�@�@z�@!G�@333@@  @Q�@\(�@n{@�G�@���@��@�Q�@�  @�ff@�\)@���@�  @Ǯ@У�@�@�p�@��@���@�z�@�p�A�\A
=A�A�RA33A
=A�A\)A#�
A'�A,(�A0  A4z�A8��A<��AAG�AEAI��AMp�AQ�AVffAZ=qA^�RAb�\AfffAj�HAn�RAs33Aw�Az�HA~�RA���A��A�p�A�\)A���A�33A�p�A�\)A���A��HA�z�A��RA���A��\A�(�A�{A�  A�=qA��
A�A�  A�=qA�z�A�ffA���A��HA���A�
=A���A�33A�p�A�\)A�G�AÅA�p�AǮA��A��
A�{A�  A��A��
A�{A�Q�A�=qA�z�A޸RA��A��HA��A�\)A陚A�A�A�  A�=qA�z�A��RA�G�A�33A�B   B�B=qB33Bz�B��B�RB�
B	�B
=qB\)B��BB
=B(�BG�BffB�B��B�B
=B(�BG�BffB�B��B�B
=B (�B!p�B"ffB#�B$��B%�B'
=B((�B)G�B*ffB+�B,��B-B.�HB/�
B0��B2{B3
=B4Q�B5p�B6�\B7�B8��B9��B:�RB;�
B<��B=�B>�HB?�
B@��BA�BC
=BD  BE�BF{BG33BHQ�BIp�BJ�\BK�BL��BM�BO
=BP  BQ�BRffBS\)BTz�BU��BV�HBX  BX��BZ=qB[\)B\z�B]��B^�\B_�
B`��Ba�Bc
=Bd(�Be�Bf{Bg
=Bh  Bh��Bi�Bj�HBk�Bl��Bmp�BnffBo\)BpQ�BqG�BrffBs\)Btz�Bup�Bv�\Bw�Bxz�By��Bz�RB{�B|��B}�B
=B�  B�z�B�
=B��B�{B��\B�
=B���B�{B���B�33B�B�=qB���B�\)B��
B�ffB��HB�\)B��B�ffB��HB�\)B�B�=qB��\B���B�G�B���B��
B�{B�Q�B��\B��RB��HB�
=B�33B�p�B���B�B��B�(�B�Q�B��\B��RB��HB�
=B�G�B��B��B��B�{B�Q�B��\B���B�
=B�G�B��B�B�{B�Q�B��\B���B���B�33B�p�B��B��B�(�B�ffB��\B��HB��B�G�B��B��
B�{B�Q�B��\B��HB��B�p�B��B��B�=qB�z�B��RB��HB�33B�p�B��B��B�=qB�z�B���B�
=B�\)B��B��B�=qB��\B��RB��B�\)B��B��B�(�B�z�B��RB���B�G�B���B��B�=qB��\B��HB�33B�p�B��
B�{B�ffB��RB���B�G�B��B��
B�{B�ffB���B��B�p�B�B�  B�Q�B���B��HB�
=B�p�B�B�{B�ffB��RB�
=B�G�B���B��
B�{B�Q�B���B��HB��B�p�B��
B�(�B�ffB��RB���B�G�B��B��
B�{B�ffB��RB�
=B�\)B��B��B�(�B�z�B��RB�
=B�G�B��B��
B�(�B�z�B���B��B�\)B��B�  B�Q�B���B�
=B�\)B�B�{B�ffB���B�
=B�p�B��
B�(�B�z�B���B��B�\)B�B�{B�z�B��HB�G�B���B��B�=qB��\B�
=B�p�B��
B�=qB��\B���B�G�B��B�(�B\B��HB�33BîB�  B�ffB���B�33BŮB�{B�ffB���B�33BǅB��B�ffB��HB�G�BɮB�  B�ffB���B�G�B�B�=qḄ�B���B�p�B��
B�ffB���B�33Bϙ�B�{BЏ\B�
=B�p�B��
B�=qB���B�\)BӮB�{B�z�B�
=BՅB�  B�z�B��HB�\)B��
B�ffB��HB�G�BٮB�=qB���B�G�BۮB�{B܏\B��BݮB�{B�z�B���B�p�B�  B�z�B���B�G�B��
B�Q�B��HB�G�B�B�(�B��B��B噚B�{B�z�B���B�p�B��
B�ffB���B�\)B�B�=qB���B�\)B�B�(�B�RB�G�B��
B�=qB��B�33B�B�Q�B���B�33B�B�=qB���B�\)B�B�(�B���B�\)B��
B�Q�B���B�\)B��B�ffB��HB�\)B��B�z�B���B�p�B�  B��\B��B��B�  B��\B��B��C �C Q�C �\C �
C�CffC��C�HC(�Cp�C�C�C33Cz�CC
=CG�C�C�
C�CffC��C�HC(�Cp�C�RC��C=qC�\C�
C�C\)C�C	  C	=qC	z�C	��C
�C
\)C
��C
�HC(�Cz�C�RC��CG�C�\C�
C{CffC�RC��C=qC�C�
C�CffC��C  CG�C�C��C(�Cp�C�RC  CQ�C��C�HC(�Cz�C��C{C\)C��C��CQ�C�C��C(�Cz�CC  C\)C��C�C(�C�C��C{C\)C��C��CG�C�C�
C(�Cp�C�RC  CQ�C��C�HC33C�C��C{CffC�C��C=qC��C�C (�C p�C �RC!
=C!\)C!��C!�HC"=qC"�C"C#
=C#\)C#�C#�C$33C$�C$�
C%{C%\)C%�C%��C&33C&�\C&�
C'{C'ffC'�RC(  C(=qC(�\C(�HC)(�C)ffC)C*
=C*Q�C*��C*��C+33C+p�C+��C,{C,Q�C,�C-  C-=qC-�C-�
C.�C.\)C.�RC/
=C/G�C/�\C/�HC0(�C0ffC0C1
=C1Q�C1��C1��C2=qC2z�C2��C3(�C3p�C3�C4  C4\)C4��C4�
C5(�C5�C5C6  C6Q�C6��C6�C7(�C7�C7��C8{C8\)C8�RC8��C9=qC9��C9�C:33C:z�C:�
C;�C;ffC;C<
=C<Q�C<��C<��C=33C=�C=�
C>(�C>p�C>��C?{C?Q�C?�C@  C@G�C@�\C@�CA33CAz�CA��CB�CBffCB�RCC
=CCQ�CC��CC��CD=qCD�CD�HCE�CEffCECF{CFQ�CF�CG  CG=qCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ?��?��H@@  @�  @�  @�  @�p�@��RA  A ��A,(�A@  A`��A�  A�  A�  A���A�Q�A�  A߮A�  B Q�B�
B�
B�
B   B'�
B/�
B8  B@(�BH(�BO�
BX  B`  Bh  Bp  Bx(�B�  B�  B�(�B�(�B�  B�  B��B��B�  B�{B�{B�  B�  B��B��B��B�  B��B��B��B�  B��B�  B�{B��B�  B�  B�  B�  B�  B�  B�  C   C
=C  C
=C��C	�C  C
=C��C��C  C  C  C��C��C  C   C"  C$  C&
=C(  C)��C,  C-��C/�C2  C4{C6
=C8  C:
=C<
=C>  C?��CA��CC�CF  CH{CJ{CL
=CN
=CP
=CR  CT  CU��CX  CY��C[��C^  C`  Cb  Cc��Cf  Ch  Ci��Cl
=Cn  Co��Cq�Ct  Cv  Cx  Cz
=C{��C}��C�  C�  C�C�  C���C�  C���C�  C�  C�C�
=C�C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C���C�  C�  C���C�  C���C�  C�C�C�  C�  C���C���C�C�
=C�  C���C�  C�
=C�C�C�C�C�  C���C���C�  C�C�C�  C���C�  C�  C���C�C�C���C���C�  C�C���C�  C�  C���C���C���C�  C�
=C�
=C�\C�C�  C�  C�  C���C�  C���C���C�  C�C�C�  C�  C�C���C���C�  C�C�  C�C�
=C�C���C���C���C�  C���C���C�  C�  C�C���C�  C�C�  C�  C�  C�C�C�C�C�  C�  C�C�C���C�  C�  C�  C�  C���C�  C�C���D z�D �qD}qD�qD� D  D� D  D� D  D� D  D� D�D��D�D� D�qD	}qD	�qD
� D
�qD}qD�qD� D  D��D  D� D�D��D  D}qD�qD}qD�qD� D�qD}qD�D��D  D� D  D� D�D� D�qD��D  D� D�D}qD  D��DD��D  D}qD  D� D�D� D�qD � D!�D!� D"  D"��D#�D#��D$  D$}qD%�D%�D&D&� D'  D'}qD'�qD(� D)  D)� D*D*��D+�D+��D+�qD,}qD,�qD-� D.  D.� D/�D/� D/��D0}qD1  D1}qD2  D2� D2�qD3}qD4  D4��D5�D5��D5�qD6z�D7  D7��D7�qD8� D8�qD9}qD:  D:��D;�D;}qD;�qD<��D=�D=� D>  D>��D?  D?� D@  D@}qDA�DA��DA�qDB}qDC  DC� DD�DD��DE  DE}qDF  DF��DGDG��DH�DH� DH�qDI}qDI�qDJ}qDJ�qDK�DL  DL}qDL�qDM� DN�DN��DO  DO}qDO��DP� DQ�DQ� DR�DR��DR�qDS� DT  DT� DU  DU��DV  DV� DW  DW��DX�DX��DY�DY��DZ  DZ� D[  D[� D\  D\� D]  D]��D^  D^}qD^�qD_� D`  D`��Da�Da�Db  Db}qDc  Dc}qDc��Dd}qDe  De��De�qDfz�Df��Dgz�Dg��Dh}qDh�qDi� Di�qDj}qDk  Dk��Dl�Dl� Dl�qDm}qDn  Dn��DoDo��Dp  Dp� Dq  Dq}qDq�qDr��DsDs��Dt�Dt� Du  Du��Dv  Dv}qDw  Dw��Dx  Dx� Dx�qDyz�Dy�qDz� D{  D{� D|D|��D}  D}� D}�qD~��D  D}qD�HD�@ D�~�D�� D��D�@ D�� D�D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D��HD�  D�=qD�~�D�� D���D�@ D���D��HD�  D�@ D�~�D���D�  D�@ D�~�D���D��qD�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D��HD���D�  D�AHD��HD�� D�  D�@ D�~�D���D�  D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D���D�>�D�~�D���D�  D�@ D�� D���D���D�@ D�~�D�� D�HD�AHD��HD���D�  D�@ D�� D�D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�>�D�~�D��HD�  D�>�D��HD�� D��qD�=qD�� D��HD�HD�AHD�� D��HD�  D�AHD��HD�D�  D�>�D�� D��HD�HD�@ D�~�D���D�HD�>�D�~�D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D�HD�AHD�� D�� D���D�>�D�~�D�� D���D�@ D��HD�� D�  D�>�D�� D�� D�  D�AHD�� D���D���D�>�D�~�D�� D�  D�AHD�~�D���D�  D�=qD�� D�� D��qD�>�D�� D�� D���D�>�D�~�D��HD�HD�AHD��HD�� D�  D�>�D��HD��HD�  D�@ D�~�D��qD���D�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�@ D��HD���D�  D�@ D�~�D���D�  D�@ D��HD�� D��D�B�D�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�@ D�}qD���D���D�>�D�� D�� D���D�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD D�� D�  D�@ DÁHD��HD�  D�@ DĀ D�� D�HD�AHD�~�Dž�D�HD�AHDƀ D�� D�HD�AHDǀ D��HD�  D�@ DȀ D��HD�  D�>�D�~�D�� D�  D�@ D�~�D��HD�HD�@ D�~�D˾�D�  D�@ D�~�D̾�D�  D�@ D�~�DͽqD�  D�AHD΂�D��HD�  D�@ D�~�DϾ�D���D�@ DЁHDо�D�  D�AHDт�D��HD�  D�@ DҀ D�� D�  D�AHDӀ D�� D��D�AHDԀ D��HD�  D�>�D�~�Dվ�D�  D�@ DցHD��HD�HD�@ D׀ D�� D�  D�@ D؀ Dؾ�D�  D�B�DفHD�� D���D�AHDڀ Dھ�D�  D�>�Dۀ D��HD�  D�@ D܀ D�� D�  D�>�D�~�Dݾ�D�  D�AHDހ D޾�D�  D�AHD߀ D�� D�HD�@ D�� D�� D���D�>�D�~�D�� D�  D�=qD�~�D⾸D�  D�@ D� D��HD�  D�>�D�~�D�� D���D�=qD�~�D�� D���D�=qD�~�D�� D�  D�>�D�~�D�� D��D�AHD�HD�� D�  D�>�D�~�D龸D�  D�@ D� D��HD�HD�@ D� D�� D�  D�@ D� D�� D�  D�@ D�~�D���D���D�>�D�~�DD���D�@ D� D�� D�  D�AHD�~�D�� D�  D�=qD�~�D�D���D�@ D� D�� D���D�>�D� D�� D�HD�@ D� D��HD�  D�AHD���D��HD�  D�AHD�� D���D���D�>�D�� D��HD�  D�@ D��HD�� D���D�AHD��HD��HD��\G�O�?8Q�?B�\?u?���?�33?��?�@�@z�@!G�@333@@  @Q�@\(�@n{@�G�@���@��@�Q�@�  @�ff@�\)@���@�  @Ǯ@У�@�@�p�@��@���@�z�@�p�A�\A
=A�A�RA33A
=A�A\)A#�
A'�A,(�A0  A4z�A8��A<��AAG�AEAI��AMp�AQ�AVffAZ=qA^�RAb�\AfffAj�HAn�RAs33Aw�Az�HA~�RA���A��A�p�A�\)A���A�33A�p�A�\)A���A��HA�z�A��RA���A��\A�(�A�{A�  A�=qA��
A�A�  A�=qA�z�A�ffA���A��HA���A�
=A���A�33A�p�A�\)A�G�AÅA�p�AǮA��A��
A�{A�  A��A��
A�{A�Q�A�=qA�z�A޸RA��A��HA��A�\)A陚A�A�A�  A�=qA�z�A��RA�G�A�33A�B   B�B=qB33Bz�B��B�RB�
B	�B
=qB\)B��BB
=B(�BG�BffB�B��B�B
=B(�BG�BffB�B��B�B
=B (�B!p�B"ffB#�B$��B%�B'
=B((�B)G�B*ffB+�B,��B-B.�HB/�
B0��B2{B3
=B4Q�B5p�B6�\B7�B8��B9��B:�RB;�
B<��B=�B>�HB?�
B@��BA�BC
=BD  BE�BF{BG33BHQ�BIp�BJ�\BK�BL��BM�BO
=BP  BQ�BRffBS\)BTz�BU��BV�HBX  BX��BZ=qB[\)B\z�B]��B^�\B_�
B`��Ba�Bc
=Bd(�Be�Bf{Bg
=Bh  Bh��Bi�Bj�HBk�Bl��Bmp�BnffBo\)BpQ�BqG�BrffBs\)Btz�Bup�Bv�\Bw�Bxz�By��Bz�RB{�B|��B}�B
=B�  B�z�B�
=B��B�{B��\B�
=B���B�{B���B�33B�B�=qB���B�\)B��
B�ffB��HB�\)B��B�ffB��HB�\)B�B�=qB��\B���B�G�B���B��
B�{B�Q�B��\B��RB��HB�
=B�33B�p�B���B�B��B�(�B�Q�B��\B��RB��HB�
=B�G�B��B��B��B�{B�Q�B��\B���B�
=B�G�B��B�B�{B�Q�B��\B���B���B�33B�p�B��B��B�(�B�ffB��\B��HB��B�G�B��B��
B�{B�Q�B��\B��HB��B�p�B��B��B�=qB�z�B��RB��HB�33B�p�B��B��B�=qB�z�B���B�
=B�\)B��B��B�=qB��\B��RB��B�\)B��B��B�(�B�z�B��RB���B�G�B���B��B�=qB��\B��HB�33B�p�B��
B�{B�ffB��RB���B�G�B��B��
B�{B�ffB���B��B�p�B�B�  B�Q�B���B��HB�
=B�p�B�B�{B�ffB��RB�
=B�G�B���B��
B�{B�Q�B���B��HB��B�p�B��
B�(�B�ffB��RB���B�G�B��B��
B�{B�ffB��RB�
=B�\)B��B��B�(�B�z�B��RB�
=B�G�B��B��
B�(�B�z�B���B��B�\)B��B�  B�Q�B���B�
=B�\)B�B�{B�ffB���B�
=B�p�B��
B�(�B�z�B���B��B�\)B�B�{B�z�B��HB�G�B���B��B�=qB��\B�
=B�p�B��
B�=qB��\B���B�G�B��B�(�B\B��HB�33BîB�  B�ffB���B�33BŮB�{B�ffB���B�33BǅB��B�ffB��HB�G�BɮB�  B�ffB���B�G�B�B�=qḄ�B���B�p�B��
B�ffB���B�33Bϙ�B�{BЏ\B�
=B�p�B��
B�=qB���B�\)BӮB�{B�z�B�
=BՅB�  B�z�B��HB�\)B��
B�ffB��HB�G�BٮB�=qB���B�G�BۮB�{B܏\B��BݮB�{B�z�B���B�p�B�  B�z�B���B�G�B��
B�Q�B��HB�G�B�B�(�B��B��B噚B�{B�z�B���B�p�B��
B�ffB���B�\)B�B�=qB���B�\)B�B�(�B�RB�G�B��
B�=qB��B�33B�B�Q�B���B�33B�B�=qB���B�\)B�B�(�B���B�\)B��
B�Q�B���B�\)B��B�ffB��HB�\)B��B�z�B���B�p�B�  B��\B��B��B�  B��\B��B��C �C Q�C �\C �
C�CffC��C�HC(�Cp�C�C�C33Cz�CC
=CG�C�C�
C�CffC��C�HC(�Cp�C�RC��C=qC�\C�
C�C\)C�C	  C	=qC	z�C	��C
�C
\)C
��C
�HC(�Cz�C�RC��CG�C�\C�
C{CffC�RC��C=qC�C�
C�CffC��C  CG�C�C��C(�Cp�C�RC  CQ�C��C�HC(�Cz�C��C{C\)C��C��CQ�C�C��C(�Cz�CC  C\)C��C�C(�C�C��C{C\)C��C��CG�C�C�
C(�Cp�C�RC  CQ�C��C�HC33C�C��C{CffC�C��C=qC��C�C (�C p�C �RC!
=C!\)C!��C!�HC"=qC"�C"C#
=C#\)C#�C#�C$33C$�C$�
C%{C%\)C%�C%��C&33C&�\C&�
C'{C'ffC'�RC(  C(=qC(�\C(�HC)(�C)ffC)C*
=C*Q�C*��C*��C+33C+p�C+��C,{C,Q�C,�C-  C-=qC-�C-�
C.�C.\)C.�RC/
=C/G�C/�\C/�HC0(�C0ffC0C1
=C1Q�C1��C1��C2=qC2z�C2��C3(�C3p�C3�C4  C4\)C4��C4�
C5(�C5�C5C6  C6Q�C6��C6�C7(�C7�C7��C8{C8\)C8�RC8��C9=qC9��C9�C:33C:z�C:�
C;�C;ffC;C<
=C<Q�C<��C<��C=33C=�C=�
C>(�C>p�C>��C?{C?Q�C?�C@  C@G�C@�\C@�CA33CAz�CA��CB�CBffCB�RCC
=CCQ�CC��CC��CD=qCD�CD�HCE�CEffCECF{CFQ�CF�CG  CG=qCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A؛�Aء�Aإ�Aأ�Aء�A؟�Aء�Aأ�Aا�Aة�Aؙ�AجAخAخAجAجAجAز-Aذ!AجAؼjAؾwAؾwAغ^AؾwAؼjAؾwA�ĜA�ƨA�ƨA�ĜA�ĜA�ĜAؾwAذ!Aز-AؾwAغ^Aة�Aץ�A�hsA�/A���A���A�r�A�"�Aκ^A͓uA̮A�bNA��A�XA��A�O�A�|�A�~�A�K�A��!A�"�A�~�A���A�~�A�r�A�"�A�(�A�-A�p�A��;A�I�A���A���A�5?A��TA���A��mA�VA���A�ƨA�\)A�%A��A��mA�x�A��RA�-A�A�C�A��\A��!A�bA�A��TA�5?A�r�A��#A�v�A��TA��#A�-A�ZA�=qA�\)A��`A�^5A�ĜA}�A|  Ay��Av=qAqp�Al�Ad�+Ab^5Aap�A\-AW�
AV��AT��AP�AMALZAJM�AH9XAF  ABVAAVA@��A?t�A=��A;�A8��A7S�A5�A4�jA4bNA3�
A2��A1G�A/��A.�\A,jA+�A)�A(��A&�A%��A$-A#l�A!�^A ��A   A�yA�PA5?AS�A;dA~�A��A�A��A�
A?}A��A�A��A�AdZA��AI�A?}A��A��A1'A�^A�#A�mAbA7LAl�A
�jA	�#A	l�A	%A��AVA1At�A��AJAbA��AO�A��A�A%A�uAVAA�A �A�#AC�AA��A^5A�TA�A�A�A �/A {@�+A ZA?}AXA&�A ��A b@�"�@�E�@�J@���@� �@��m@���@�  @�|�@��@�V@��R@�ff@�@���@��@�  @�z�@�@��@�{@��@�ƨ@��`@��@��@�n�@�ȴ@��@�@�@�|�@�l�@�S�@��H@��@��@�  @��;@�;d@�@��^@�j@�+@�^@�&�@� �@畁@��@旍@��@��#@�/@��@��@�7L@�j@�z�@�r�@���@���@�I�@�V@�p�@�@���@�S�@�o@��@ާ�@��@��@�9X@ۍP@���@�J@ّh@��/@�r�@��
@�n�@���@�/@ԛ�@�r�@�Z@�A�@�ƨ@�K�@�=q@ѡ�@��@д9@�b@Ϯ@��y@���@�hs@̼j@�Q�@��;@�S�@���@�5?@�{@��#@�hs@���@ȓu@�A�@��@Ǖ�@�33@�^5@��T@�x�@�%@�r�@��m@��
@���@��
@�l�@�
=@�n�@�?}@���@��j@�z�@�9X@��;@��@�+@��y@���@�$�@�@�%@�r�@�1'@� �@�b@��;@���@�;d@�M�@�hs@�7L@���@� �@���@�|�@�o@��@�V@�J@��@���@��@�(�@��m@���@�C�@��y@���@�@��@�7L@��@��`@���@���@��u@��@�Q�@��@�ƨ@�t�@�o@��@���@��+@�{@���@���@���@�x�@�G�@�/@��@�r�@��@�C�@�@��!@�5?@��^@�hs@�%@�z�@��;@���@�t�@�t�@�o@���@�=q@��@�{@�$�@���@�/@���@��9@���@���@���@��D@� �@���@�@���@�ff@��@��-@��7@��7@���@���@��D@�r�@�9X@��F@�K�@���@�"�@��@�ff@�M�@�{@��#@��^@��7@��@�z�@��
@�;d@���@�J@��-@�O�@��@�z�@� �@��F@�K�@�@��@�ȴ@�E�@�J@��T@���@��^@�p�@�G�@��@���@�j@�1'@��@��
@�|�@�S�@�33@���@�~�@�M�@��#@��-@���@�X@��@��`@��/@�r�@�A�@�1'@� �@��@���@�;d@���@��+@�ff@�-@��@�hs@��@���@���@��@�b@��w@��P@�\)@��y@�v�@�$�@��-@�x�@�7L@�V@��/@���@��@��@�bN@� �@��F@�S�@�+@���@�v�@�=q@�{@��#@��^@�G�@���@���@�j@��@���@�|�@�S�@�"�@�ȴ@�^5@�J@��T@���@��h@�`B@�V@��/@��9@�j@�Z@�1'@��@�w@;d@~ȴ@~v�@~{@}�h@}�@|�j@|9X@{�
@{ƨ@{��@{o@zn�@y�@y�#@y�7@yX@yX@x��@x1'@w�;@w�@w�P@w�P@w\)@v��@vv�@v5?@u�@u��@t��@tj@t�@s�F@r��@rn�@q�@qhs@q��@q7L@q%@pĜ@pr�@o�@o�@n��@nv�@n$�@m��@mO�@l�/@l�@lI�@k��@k"�@j��@j~�@j-@i��@i��@i��@i��@i�@hr�@h1'@hb@g��@gl�@g
=@f�@f��@f5?@e��@eO�@eO�@ep�@e`B@e`B@e�h@e`B@d�@dZ@c�F@cdZ@cC�@co@b��@b��@b�\@a�^@a&�@a&�@`��@`1'@_\)@^�R@^�+@^ff@^E�@^5?@^@]�T@]`B@\�@\�j@\�/@\�@\��@\�@[��@[�@[C�@["�@Z�H@Z=q@Y��@Y�7@Yhs@YX@Y%@XĜ@X�@XA�@W�;@W�@W;d@V�@V��@V��@V��@Vv�@V{@U��@Up�@U`B@U?}@U�@T��@T�D@T(�@S��@S��@SdZ@R�H@RJ@Q�@P�9@P1'@O�@O�@Ol�@O;d@O�@O
=@Nȴ@NV@M�T@Mp�@L�D@L9X@Kƨ@KC�@J�@J��@J�\@J=q@I��@Ihs@Ihs@I%@HbN@H �@G�@G\)@F��@F$�@E�T@E�h@EO�@D��@D�@D��@Dz�@D(�@C��@C"�@B�@B��@B�\@B�@B^5@BM�@BJ@A�7@@�`@@�9@@�@@b@?�@>ȴ@>��@>E�@=��@=O�@<�j@<j@<�@;�m@;�
@;t�@;o@:��@:�\@:~�@:=q@9�@9��@97L@8�`@8��@8�9@8bN@7��@7K�@6ȴ@6��@6��@65?@5��@5`B@4�/@4��@4�@4I�@3��@333@2��@2n�@1��@1�^@1x�@17L@0�`@0Ĝ@0��@0r�@0 �@/�@/�;@/��@/l�@/+@/
=@.�R@.ff@.5?@.@-��@-�h@,�/@,�D@,j@,(�@+ƨ@+dZ@+33@+@*�H@*��@*-@)�@)�#@)�^@)��@)X@)G�@)%@(Ĝ@(�u@(Q�@(b@'��@'\)@'+@'
=@&�y@&ȴ@&V@&{@%�T@%�h@%`B@%/@%V@$�@$��@$z�@$z�@$Z@$9X@#��@#�m@#�m@#�
@#��@#dZ@#S�@#C�@#33@"�H@"��@"-@!�^@!��@!�7@!�@ ��@ �u@ bN@ Q�@ 1'@ 1'@��@�@K�@�@��@v�@V@$�@@`B@O�@V@��@��@��@��@Z@�@�m@��@dZ@33@o@�@�H@�H@�H@~�@=q@��@X@�@Ĝ@�u@�@r�@Q�@��@��@�P@|�@l�@;d@��@ȴ@�+@ff@5?@$�@�@@�h@�@�@`B@�@�@�/@�@j@(�@�@��@�F@�@C�@"�@@@�@�H@�H@��@��@-@�@��@��@�^@��@�^@��@��@��@7L@�`@��@�u@bN@1'@ �@ �@�@�@��@��@|�@
=@�@ȴ@�R@�R@��@�+@V@5?@{@�@�-@p�@/@��@��@�@j@jAؙ�A؛�A؛�A؛�A؛�A؟�A؟�Aء�Aء�Aأ�Aإ�Aأ�Aإ�Aإ�Aإ�Aأ�Aأ�Aأ�Aأ�A؟�Aء�Aء�A؟�A؟�Aء�Aء�Aء�Aأ�Aء�Aإ�Aء�Aأ�Aأ�Aا�Aا�Aإ�Aأ�Aا�Aإ�AخAخAإ�Aؗ�Aء�AجAذ!AخAخAخAخAخAخAخAخAخAخAجAجAة�AخAخAخAذ!AخAخAخAخAخAخAجAجAجAجAجAجAجAا�Aة�AجAة�AجAخAخAخAخAجAة�Aا�Aة�AجAخAخAة�Aا�Aز-Aش9Aغ^AؼjAغ^AضFAة�Aإ�Aة�AظRAضFAش9Aذ!AخAجAة�Aة�Aة�Aة�Aة�AضFAغ^AؼjAؼjAؾwAؾwAؾwAؾwA���AؼjAؼjAؼjAؾwAؾwAؾwAؾwAؾwAؾwAؾwAؾwAؾwAغ^Aغ^Aغ^AظRAؼjAؾwA���A���A���AؾwAؾwAؼjAؼjAؾwAغ^Aغ^AؼjAغ^AؼjAؼjAؼjAؼjAؼjAؼjAؾwAؾwAؾwAؾwA���A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ĜA�ƨA�ȴA�ȴA�ƨA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA�ĜA�A�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA���A�AؾwAغ^Aإ�Aء�Aء�AجAذ!Aش9AؾwAظRAؼjAضFAذ!AجAجAجAخAظRAؾwAؾwAضFAؾwA�AؾwAغ^AؼjAؼjAغ^AظRAظRAؼjAؼjAغ^AظRAذ!AخAخAخAة�Aا�Aإ�Aء�A�v�A�XA�XA�/A׍PA��HA�A֡�A֓uAև+A�r�A�n�A�dZA�bNA�`BA�VA�A�A�7LA� �A�%A�A�  A���A���Aղ-AՑhA�E�A���A���A��A��A��yA���A�ƨAԩ�Aԕ�A�hsA�I�A�(�A��AӸRAӋDA�O�A�-A�{A��A��/AҶFAҕ�AҁA�hsA�K�A�=qA�(�A��A�
=A�A�  A��A��#A���A���A���A���A�ȴA�ȴA�ȴA�ĜA�AѼjAѰ!Aї�Aѡ�Aѥ�Aѡ�Aѡ�Aч+A�|�A�z�A�t�A�r�A�p�A�dZA�O�A�?}A�(�A�JA��yAк^AБhA�XA�=qA�+A��A�
=A���A���A���A�ƨAϕ�A�^5A�C�A��A��A���A�ĜAμjAθRAθRAκ^AΣ�A�z�A�XA�A�A��A�JA��A��mA��/A;wA͟�AͅA�t�A�K�A�1'A�
=A�A���A��TA���A�ȴA̼jA̬A̟�A̗�A̓uA̓uA̓uA̙�A̛�ȂhA�z�A�VA�&�A�A��
Aˣ�A�^5A�"�A� �A�JA��yAʮA�hsA�;dA�+A�&�A��A�{A�1A���A��A��/A�ȴAɲ-Aɝ�AɃA�G�A�(�AƾwA�O�A��HAōPA�$�A��HA�Aİ!A�x�A�33A���AÕ�A�ffA�Q�A�9XA��A�%A�  A���A��A��TA��
A¼jA�r�A�S�A�JA��yA�C�A��jA�|�A�I�A�-A��A�1A���A��A��TA���A��FA��uA�C�A��HA�Q�A��A�
=A�A���A��A���A��RA�S�A�x�A�"�A��A���A��uA�n�A�?}A�/A�&�A�bA��A��RA�VA�&�A�A��TA��-A�l�A�/A�{A�%A��A��!A�dZA�1A��HA��#A�ƨA�~�A�C�A�1A�ƨA���A�r�A�K�A�-A��A�bA�A��A��;A�ȴA�A��wA��-A���A���A���A���A��DA��A�z�A�r�A�\)A�K�A�/A��A��wA�|�A�C�A�oA���A��#A��9A���A��uA�|�A�E�A��A�ĜA��!A��+A�p�A�VA�C�A�9XA�1'A�JA��A���A��jA��9A��A�dZA�G�A�/A���A� �A���A��uA�x�A�v�A�p�A�`BA�A�A�5?A��A�VA���A��mA���A��-A���A��uA�r�A�hsA�^5A�O�A�?}A�"�A��A�JA�  A���A��A��A��`A���A�n�A�?}A��A��`A���A���A�G�A�+A���A��A��+A�O�A�oA��A���A��DA�jA�S�A�+A��A��RA���A�~�A�l�A�K�A�7LA��A��!A��PA�XA�(�A�{A�A��yA���A�ĜA���A��RA���A��+A�n�A�dZA�A�A� �A���A��A��/A�ĜA��A���A�l�A�E�A�1A���A��mA��/A���A��\A�l�A�1'A��yA�z�A�7LA���A�A��+A�7LA�"�A�JA���A��A��HA���A��FA���A���A��+A�n�A�C�A�bA���A���A���A���A��HA���A��hA��\A��PA��7A�z�A�^5A�+A��A�A�A���A��+A�n�A�n�A�jA�ffA�hsA�Q�A���A���A��RA��A�I�A��A��A��RA��DA��A�x�A�jA�VA�(�A�
=A�%A�  A��yA�ĜA��!A��\A�E�A���A��#A���A��\A��!A��9A��FA��A���A��uA�I�A��A�ȴA��9A�z�A��A��A�A��A���A�^5A��`A��\A�E�A�{A��TA��A�O�A���A��RA��A�VA�+A��mA���A�XA��TA��A�;dA�ƨA�x�A�O�A�"�A�A��mA��#A�ȴA��^A��!A���A���A���A��A�^5A�I�A�I�A�E�A�?}A�9XA�/A�JA��hA�XA��A�1'A��;A���A��7A�v�A�^5A�+A���A�A�A��`A���A�M�A�  A��FA��DA�hsA�7LA��A��A��DA�x�A�XA�1'A�VA��;A��A��/A�bNA�+A���A��A���A��hA��A���A��-A���A�~�A�l�A�XA�9XA��TA���A��A��A�z�A�`BA��A��A�hsA�A�hA�A~��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             A؛�Aء�Aإ�Aأ�Aء�A؟�Aء�Aأ�Aا�Aة�Aؙ�AجAخAخAجAجAجAز-Aذ!AجAؼjAؾwAؾwAغ^AؾwAؼjAؾwA�ĜA�ƨA�ƨA�ĜA�ĜA�ĜAؾwAذ!Aز-AؾwAغ^Aة�Aץ�A�hsA�/A���A���A�r�A�"�Aκ^A͓uA̮A�bNA��A�XA��A�O�A�|�A�~�A�K�A��!A�"�A�~�A���A�~�A�r�A�"�A�(�A�-A�p�A��;A�I�A���A���A�5?A��TA���A��mA�VA���A�ƨA�\)A�%A��A��mA�x�A��RA�-A�A�C�A��\A��!A�bA�A��TA�5?A�r�A��#A�v�A��TA��#A�-A�ZA�=qA�\)A��`A�^5A�ĜA}�A|  Ay��Av=qAqp�Al�Ad�+Ab^5Aap�A\-AW�
AV��AT��AP�AMALZAJM�AH9XAF  ABVAAVA@��A?t�A=��A;�A8��A7S�A5�A4�jA4bNA3�
A2��A1G�A/��A.�\A,jA+�A)�A(��A&�A%��A$-A#l�A!�^A ��A   A�yA�PA5?AS�A;dA~�A��A�A��A�
A?}A��A�A��A�AdZA��AI�A?}A��A��A1'A�^A�#A�mAbA7LAl�A
�jA	�#A	l�A	%A��AVA1At�A��AJAbA��AO�A��A�A%A�uAVAA�A �A�#AC�AA��A^5A�TA�A�A�A �/A {@�+A ZA?}AXA&�A ��A b@�"�@�E�@�J@���@� �@��m@���@�  @�|�@��@�V@��R@�ff@�@���@��@�  @�z�@�@��@�{@��@�ƨ@��`@��@��@�n�@�ȴ@��@�@�@�|�@�l�@�S�@��H@��@��@�  @��;@�;d@�@��^@�j@�+@�^@�&�@� �@畁@��@旍@��@��#@�/@��@��@�7L@�j@�z�@�r�@���@���@�I�@�V@�p�@�@���@�S�@�o@��@ާ�@��@��@�9X@ۍP@���@�J@ّh@��/@�r�@��
@�n�@���@�/@ԛ�@�r�@�Z@�A�@�ƨ@�K�@�=q@ѡ�@��@д9@�b@Ϯ@��y@���@�hs@̼j@�Q�@��;@�S�@���@�5?@�{@��#@�hs@���@ȓu@�A�@��@Ǖ�@�33@�^5@��T@�x�@�%@�r�@��m@��
@���@��
@�l�@�
=@�n�@�?}@���@��j@�z�@�9X@��;@��@�+@��y@���@�$�@�@�%@�r�@�1'@� �@�b@��;@���@�;d@�M�@�hs@�7L@���@� �@���@�|�@�o@��@�V@�J@��@���@��@�(�@��m@���@�C�@��y@���@�@��@�7L@��@��`@���@���@��u@��@�Q�@��@�ƨ@�t�@�o@��@���@��+@�{@���@���@���@�x�@�G�@�/@��@�r�@��@�C�@�@��!@�5?@��^@�hs@�%@�z�@��;@���@�t�@�t�@�o@���@�=q@��@�{@�$�@���@�/@���@��9@���@���@���@��D@� �@���@�@���@�ff@��@��-@��7@��7@���@���@��D@�r�@�9X@��F@�K�@���@�"�@��@�ff@�M�@�{@��#@��^@��7@��@�z�@��
@�;d@���@�J@��-@�O�@��@�z�@� �@��F@�K�@�@��@�ȴ@�E�@�J@��T@���@��^@�p�@�G�@��@���@�j@�1'@��@��
@�|�@�S�@�33@���@�~�@�M�@��#@��-@���@�X@��@��`@��/@�r�@�A�@�1'@� �@��@���@�;d@���@��+@�ff@�-@��@�hs@��@���@���@��@�b@��w@��P@�\)@��y@�v�@�$�@��-@�x�@�7L@�V@��/@���@��@��@�bN@� �@��F@�S�@�+@���@�v�@�=q@�{@��#@��^@�G�@���@���@�j@��@���@�|�@�S�@�"�@�ȴ@�^5@�J@��T@���@��h@�`B@�V@��/@��9@�j@�Z@�1'@��@�w@;d@~ȴ@~v�@~{@}�h@}�@|�j@|9X@{�
@{ƨ@{��@{o@zn�@y�@y�#@y�7@yX@yX@x��@x1'@w�;@w�@w�P@w�P@w\)@v��@vv�@v5?@u�@u��@t��@tj@t�@s�F@r��@rn�@q�@qhs@q��@q7L@q%@pĜ@pr�@o�@o�@n��@nv�@n$�@m��@mO�@l�/@l�@lI�@k��@k"�@j��@j~�@j-@i��@i��@i��@i��@i�@hr�@h1'@hb@g��@gl�@g
=@f�@f��@f5?@e��@eO�@eO�@ep�@e`B@e`B@e�h@e`B@d�@dZ@c�F@cdZ@cC�@co@b��@b��@b�\@a�^@a&�@a&�@`��@`1'@_\)@^�R@^�+@^ff@^E�@^5?@^@]�T@]`B@\�@\�j@\�/@\�@\��@\�@[��@[�@[C�@["�@Z�H@Z=q@Y��@Y�7@Yhs@YX@Y%@XĜ@X�@XA�@W�;@W�@W;d@V�@V��@V��@V��@Vv�@V{@U��@Up�@U`B@U?}@U�@T��@T�D@T(�@S��@S��@SdZ@R�H@RJ@Q�@P�9@P1'@O�@O�@Ol�@O;d@O�@O
=@Nȴ@NV@M�T@Mp�@L�D@L9X@Kƨ@KC�@J�@J��@J�\@J=q@I��@Ihs@Ihs@I%@HbN@H �@G�@G\)@F��@F$�@E�T@E�h@EO�@D��@D�@D��@Dz�@D(�@C��@C"�@B�@B��@B�\@B�@B^5@BM�@BJ@A�7@@�`@@�9@@�@@b@?�@>ȴ@>��@>E�@=��@=O�@<�j@<j@<�@;�m@;�
@;t�@;o@:��@:�\@:~�@:=q@9�@9��@97L@8�`@8��@8�9@8bN@7��@7K�@6ȴ@6��@6��@65?@5��@5`B@4�/@4��@4�@4I�@3��@333@2��@2n�@1��@1�^@1x�@17L@0�`@0Ĝ@0��@0r�@0 �@/�@/�;@/��@/l�@/+@/
=@.�R@.ff@.5?@.@-��@-�h@,�/@,�D@,j@,(�@+ƨ@+dZ@+33@+@*�H@*��@*-@)�@)�#@)�^@)��@)X@)G�@)%@(Ĝ@(�u@(Q�@(b@'��@'\)@'+@'
=@&�y@&ȴ@&V@&{@%�T@%�h@%`B@%/@%V@$�@$��@$z�@$z�@$Z@$9X@#��@#�m@#�m@#�
@#��@#dZ@#S�@#C�@#33@"�H@"��@"-@!�^@!��@!�7@!�@ ��@ �u@ bN@ Q�@ 1'@ 1'@��@�@K�@�@��@v�@V@$�@@`B@O�@V@��@��@��@��@Z@�@�m@��@dZ@33@o@�@�H@�H@�H@~�@=q@��@X@�@Ĝ@�u@�@r�@Q�@��@��@�P@|�@l�@;d@��@ȴ@�+@ff@5?@$�@�@@�h@�@�@`B@�@�@�/@�@j@(�@�@��@�F@�@C�@"�@@@�@�H@�H@��@��@-@�@��@��@�^@��@�^@��@��@��@7L@�`@��@�u@bN@1'@ �@ �@�@�@��@��@|�@
=@�@ȴ@�R@�R@��@�+@V@5?@{@�@�-@p�@/@��@��@�@jG�O�Aؙ�A؛�A؛�A؛�A؛�A؟�A؟�Aء�Aء�Aأ�Aإ�Aأ�Aإ�Aإ�Aإ�Aأ�Aأ�Aأ�Aأ�A؟�Aء�Aء�A؟�A؟�Aء�Aء�Aء�Aأ�Aء�Aإ�Aء�Aأ�Aأ�Aا�Aا�Aإ�Aأ�Aا�Aإ�AخAخAإ�Aؗ�Aء�AجAذ!AخAخAخAخAخAخAخAخAخAخAجAجAة�AخAخAخAذ!AخAخAخAخAخAخAجAجAجAجAجAجAجAا�Aة�AجAة�AجAخAخAخAخAجAة�Aا�Aة�AجAخAخAة�Aا�Aز-Aش9Aغ^AؼjAغ^AضFAة�Aإ�Aة�AظRAضFAش9Aذ!AخAجAة�Aة�Aة�Aة�Aة�AضFAغ^AؼjAؼjAؾwAؾwAؾwAؾwA���AؼjAؼjAؼjAؾwAؾwAؾwAؾwAؾwAؾwAؾwAؾwAؾwAغ^Aغ^Aغ^AظRAؼjAؾwA���A���A���AؾwAؾwAؼjAؼjAؾwAغ^Aغ^AؼjAغ^AؼjAؼjAؼjAؼjAؼjAؼjAؾwAؾwAؾwAؾwA���A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ĜA�ƨA�ȴA�ȴA�ƨA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA�ĜA�A�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA���A�AؾwAغ^Aإ�Aء�Aء�AجAذ!Aش9AؾwAظRAؼjAضFAذ!AجAجAجAخAظRAؾwAؾwAضFAؾwA�AؾwAغ^AؼjAؼjAغ^AظRAظRAؼjAؼjAغ^AظRAذ!AخAخAخAة�Aا�Aإ�Aء�A�v�A�XA�XA�/A׍PA��HA�A֡�A֓uAև+A�r�A�n�A�dZA�bNA�`BA�VA�A�A�7LA� �A�%A�A�  A���A���Aղ-AՑhA�E�A���A���A��A��A��yA���A�ƨAԩ�Aԕ�A�hsA�I�A�(�A��AӸRAӋDA�O�A�-A�{A��A��/AҶFAҕ�AҁA�hsA�K�A�=qA�(�A��A�
=A�A�  A��A��#A���A���A���A���A�ȴA�ȴA�ȴA�ĜA�AѼjAѰ!Aї�Aѡ�Aѥ�Aѡ�Aѡ�Aч+A�|�A�z�A�t�A�r�A�p�A�dZA�O�A�?}A�(�A�JA��yAк^AБhA�XA�=qA�+A��A�
=A���A���A���A�ƨAϕ�A�^5A�C�A��A��A���A�ĜAμjAθRAθRAκ^AΣ�A�z�A�XA�A�A��A�JA��A��mA��/A;wA͟�AͅA�t�A�K�A�1'A�
=A�A���A��TA���A�ȴA̼jA̬A̟�A̗�A̓uA̓uA̓uA̙�A̛�ȂhA�z�A�VA�&�A�A��
Aˣ�A�^5A�"�A� �A�JA��yAʮA�hsA�;dA�+A�&�A��A�{A�1A���A��A��/A�ȴAɲ-Aɝ�AɃA�G�A�(�AƾwA�O�A��HAōPA�$�A��HA�Aİ!A�x�A�33A���AÕ�A�ffA�Q�A�9XA��A�%A�  A���A��A��TA��
A¼jA�r�A�S�A�JA��yA�C�A��jA�|�A�I�A�-A��A�1A���A��A��TA���A��FA��uA�C�A��HA�Q�A��A�
=A�A���A��A���A��RA�S�A�x�A�"�A��A���A��uA�n�A�?}A�/A�&�A�bA��A��RA�VA�&�A�A��TA��-A�l�A�/A�{A�%A��A��!A�dZA�1A��HA��#A�ƨA�~�A�C�A�1A�ƨA���A�r�A�K�A�-A��A�bA�A��A��;A�ȴA�A��wA��-A���A���A���A���A��DA��A�z�A�r�A�\)A�K�A�/A��A��wA�|�A�C�A�oA���A��#A��9A���A��uA�|�A�E�A��A�ĜA��!A��+A�p�A�VA�C�A�9XA�1'A�JA��A���A��jA��9A��A�dZA�G�A�/A���A� �A���A��uA�x�A�v�A�p�A�`BA�A�A�5?A��A�VA���A��mA���A��-A���A��uA�r�A�hsA�^5A�O�A�?}A�"�A��A�JA�  A���A��A��A��`A���A�n�A�?}A��A��`A���A���A�G�A�+A���A��A��+A�O�A�oA��A���A��DA�jA�S�A�+A��A��RA���A�~�A�l�A�K�A�7LA��A��!A��PA�XA�(�A�{A�A��yA���A�ĜA���A��RA���A��+A�n�A�dZA�A�A� �A���A��A��/A�ĜA��A���A�l�A�E�A�1A���A��mA��/A���A��\A�l�A�1'A��yA�z�A�7LA���A�A��+A�7LA�"�A�JA���A��A��HA���A��FA���A���A��+A�n�A�C�A�bA���A���A���A���A��HA���A��hA��\A��PA��7A�z�A�^5A�+A��A�A�A���A��+A�n�A�n�A�jA�ffA�hsA�Q�A���A���A��RA��A�I�A��A��A��RA��DA��A�x�A�jA�VA�(�A�
=A�%A�  A��yA�ĜA��!A��\A�E�A���A��#A���A��\A��!A��9A��FA��A���A��uA�I�A��A�ȴA��9A�z�A��A��A�A��A���A�^5A��`A��\A�E�A�{A��TA��A�O�A���A��RA��A�VA�+A��mA���A�XA��TA��A�;dA�ƨA�x�A�O�A�"�A�A��mA��#A�ȴA��^A��!A���A���A���A��A�^5A�I�A�I�A�E�A�?}A�9XA�/A�JA��hA�XA��A�1'A��;A���A��7A�v�A�^5A�+A���A�A�A��`A���A�M�A�  A��FA��DA�hsA�7LA��A��A��DA�x�A�XA�1'A�VA��;A��A��/A�bNA�+A���A��A���A��hA��A���A��-A���A�~�A�l�A�XA�9XA��TA���A��A��A�z�A�`BA��A��A�hsA�A�hA�A~��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	6zB	5tB	4�B	5?B	6FB	6zB	5tB	4�B	5tB	6B	7�B	5tB	5B	4�B	5tB	5tB	5�B	5?B	6B	7B	5?B	5�B	6B	6FB	6zB	6�B	6�B	6B	5�B	5�B	5�B	5�B	5�B	7B	8�B	7�B	6FB	6zB	5�B	\)B	��B	��B	�/B	��B	��B	�B
 �B	�B	��B	��B	�MB
1B	��B
�B
'B
+�B
J#B
jKB
�\B
�B
�HB
�[B
�B�B �B1'B<�BEmBR�BQB\�Bj�Bc Bd�B^jB]�BYBS&BU2BR�BR�BQNBPHBO�BW
B\�BXB[WB`vBncBffBN�B/OB�B
�B
�,B
�B
�UB
�=B
�PB
y	B
gB
LdB
<�B
B	�B	�yB	רB	�?B	��B	�B	U2B	B'B	=�B	/B	B	�B�B��B��B��B��BϫB��B�B�RB�B�B�[B��B�IB�B��B��B�LB��B��B�-B�CB��B�eB�_B�+B��B�eB��B��B��B�1B��B��B��B��B��B��B�RB��B��B��B�B��B��B��B��B�$B�*B�6B�wB�qB�qB��B��B��B��B�}BBɺB͟B�?B�KBӏB��B�9BߤB�
B�)B��B�iB�B��B	  B	�B	 iB	�B	@B	FB		B	 'B	"�B	)�B	-�B	0UB	1�B	4B	4�B	5�B	;0B	EB	F�B	@B	F�B	T,B	jB	j�B	m�B	t�B	rGB	sMB	r�B	s�B	r�B	qAB	q�B	uZB	|PB	cB	~�B	�rB	�bB	��B	��B	��B	�'B	��B	��B	�IB	�=B	��B	�}B	�mB	�<B	�LB	��B	��B	�UB	�B	ɺB	�EB	�aB	�KB	˒B	̘B	�0B	�B	�0B	��B	�0B	͟B	��B	�zB	�'B	�BB	�UB	��B	�B	��B	��B	��B	ƨB	�zB	�B	ɆB	уB	�,B	՛B	רB	�B	�;B	�vB	ںB	یB	�;B	�vB	��B	��B	�B	ߤB	�B	�B	�NB	��B	��B	� B	�B	��B	�B	�B	��B	�B	��B	�ZB	�&B	��B	�B	�ZB	�&B	�HB	�BB	�B	�TB	�B	�NB	�B	ߤB	�pB	��B	��B	�pB	�B	�BB	�BB	�NB	�B	��B	�B	�B	��B	�ZB	�B	�,B	��B	�ZB	�,B	��B	�2B	��B	�B	�`B	�,B	�B	��B	�DB	�B	��B	��B	�B	�B	�"B	��B	��B	��B	�WB	�B	�B	�]B	��B	�)B	�B	� B	�iB	��B	�5B	�oB	�B	�GB	�MB	�B	�|B	�|B	�GB	�MB	�B	�B	�ZB	��B	��B	��B	��B	��B	�8B	�8B	��B	�2B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�JB	�B	��B	�PB	��B	��B	��B	��B	��B	�(B	��B	��B	��B	��B	��B
 4B
 4B
 �B
B
;B
oB
oB
�B
GB
�B
{B
�B
GB
�B
�B
�B
%B
YB
+B
	�B
	7B
	7B
fB
�B
	7B
	7B
	B
	B
	7B
	B
	�B
�B
	�B

rB

�B
xB
DB
�B
�B
"B
VB
VB
�B
~B
�B
�B
�B
�B
.B
�B
@B
B
B
B
uB
uB
�B
hB
oB
�B
B
B
{B
{B
B
�B
�B
�B
SB
�B
$B
$B
$B
$B
�B
�B
_B
�B
1B
�B
eB
	B
kB
�B
	B
=B
�B
B
IB
B
IB
B
OB
�B
�B
�B
VB
VB
!B
�B
�B
 \B
 \B
 \B
 �B
 �B
!�B
 �B
 �B
!-B
!bB
#�B
#�B
$B
$@B
$tB
%FB
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
($B
(XB
(�B
)_B
)*B
)*B
)�B
*0B
*eB
*eB
+B
+B
,B
,=B
,qB
-CB
,qB
,�B
-CB
-�B
.}B
.�B
/B
/�B
0UB
0�B
0�B
0�B
1[B
1�B
1�B
2-B
2aB
2�B
2�B
3hB
3hB
3�B
3�B
4nB
4B
4B
4nB
5?B
5?B
5B
5B
5?B
5tB
4�B
5B
5�B
6FB
6�B
7�B
7�B
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
8�B
8�B
8�B
9�B
9�B
:^B
:�B
<�B
=�B
=qB
=�B
=�B
>�B
>wB
>B
=�B
>B
=�B
=�B
>�B
?�B
@�B
?HB
?�B
@OB
@�B
A B
@�B
@�B
AUB
AUB
A�B
B�B
C-B
C-B
C�B
CaB
C�B
CaB
C�B
D3B
EB
E9B
EmB
GEB
H�B
HKB
I�B
I�B
H�B
HB
GB
G�B
H�B
IB
I�B
IRB
IB
JXB
IB
H�B
IB
I�B
IB
IRB
I�B
J#B
J#B
JXB
J�B
K)B
K�B
K�B
K�B
K�B
L0B
L�B
N�B
M�B
NpB
O�B
PHB
P�B
P�B
QB
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
S[B
S[B
T,B
TaB
T,B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
T�B
T�B
T�B
U2B
UgB
U�B
U�B
UgB
VmB
W
B
W�B
XB
XEB
X�B
X�B
YB
YKB
YKB
YKB
YKB
Y�B
Y�B
ZB
ZQB
ZQB
Z�B
[#B
[#B
[#B
[WB
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]dB
^B
^jB
^jB
^�B
_B
_pB
_pB
_�B
`vB
`vB
a�B
bB
bB
b�B
c�B
c�B
dZB
d�B
dZB
dZB
d�B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e,B
e�B
f2B
ffB
ffB
f�B
f�B
gB
gmB
gmB
gmB
g�B
h
B
h
B
h�B
h�B
h�B
iB
iDB
i�B
jB
kB
kQB
kQB
l"B
lWB
l"B
l�B
l�B
l�B
m]B
n/B
ncB
n�B
o B
o�B
oiB
o�B
o�B
p;B
p;B
p;B
poB
p�B
qB
p�B
qAB
qvB
q�B
q�B
q�B
rB
rGB
r|B
r|B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
t�B
u%B
u�B
u�B
u�B
v`B
v+B
v`B
v�B
w2B
w�B
xB
x8B
x8B
xlB
x�B
y>B
yrB
y�B
zB
zB
zB
zDB
zxB
z�B
zxB
z�B
z�B
z�B
{B
z�B
{B
{JB
{B
{�B
{B
{�B
{�B
{�B
|B
|PB
|PB
|PB
|�B
|�B
}VB
}VB
}VB
}"B
}"B
}�B
}�B
}�B
~(B
~(B
~]B
~�B
~�B
.B
cB
cB
�B
�B
�B
�B
�B
�iB
��B
��B
�;B
�oB
��B
��B
��B
��B
��B
�oB
�B
��B
�B
��B
�B
��B
�{B
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
�%B
��B
��B
��B
��B
��B
�+B
�+B
�+B
��B
��B
��B
��B
�1B
��B
��B
�B
�B
�lB
��B
�	B
�=B
�=B
�=B
�=B
�rB
�=B
�rB
��B
�xB
�DB
�xB
��B
�B
��B
�B
�~B
�~B
�~B
��B
�"B
��B
��B
��B
��B
��B
��B
�(B
��B
��B
��B
��B
��B
��B
�.B
�.B
�.B
�bB
�bB
� B
��B
��B
��B
� B
��B
�4B
�4B
��B
��B
�:B
��B	6�B	6FB	6�B	6�B	6FB	5�B	5�B	5tB	5tB	4�B	4�B	5?B	4nB	5B	4�B	5?B	5tB	5�B	5�B	7LB	6�B	6zB	7B	6�B	6FB	6FB	5�B	5�B	4�B	4�B	5�B	3�B	5�B	4�B	5tB	6B	5tB	5B	6�B	4B	6B	8�B	7�B	3�B	6zB	5�B	5tB	6B	6B	5�B	5tB	4�B	5?B	5tB	5?B	5B	5tB	5?B	5?B	5B	5tB	4�B	5B	4�B	5B	5B	5B	5B	4�B	5B	5?B	5�B	5�B	5�B	5tB	5�B	6zB	5tB	5?B	5�B	5B	4�B	5B	5B	5�B	5�B	6FB	5�B	5tB	5�B	5�B	5�B	5�B	6B	6FB	49B	4B	5B	5B	6�B	7�B	7LB	5tB	3�B	4�B	7�B	6�B	6�B	7�B	6�B	6�B	6�B	7LB	7B	2�B	5�B	5tB	5?B	5B	5B	5�B	5�B	5B	6FB	6zB	6FB	6B	5�B	5tB	5�B	6B	5�B	5�B	6B	6FB	6�B	6zB	6zB	7B	5�B	6FB	5�B	6B	6B	6zB	6�B	6�B	6zB	6�B	6zB	7LB	6�B	6�B	6�B	6zB	6zB	6zB	6�B	7B	6zB	6B	6�B	6�B	6zB	6�B	6FB	6FB	5�B	5�B	5tB	5�B	5tB	6B	5�B	5B	5tB	5�B	5�B	6B	5�B	5�B	6B	5B	6B	5tB	5�B	5�B	5tB	5�B	5�B	5�B	5�B	6FB	5�B	6B	5tB	5�B	5?B	5tB	5tB	6B	5�B	5�B	5�B	6B	5�B	6B	5�B	5�B	5�B	6�B	6FB	6�B	8�B	9XB	9�B	;0B	7B	9�B	8�B	7LB	9�B	8RB	6zB	9$B	8�B	8�B	8B	8�B	5�B	6�B	5�B	8RB	6B	5�B	6FB	7�B	6B	6�B	6FB	6�B	5�B	6zB	6B	6zB	5�B	6zB	5�B	5B	5?B	6FB	5?B	4nB	5tB	AUB	>BB	=qB	G�B	aHB	z�B	x8B	~�B	��B	�B	�1B	�lB	�~B	�VB	��B	��B	�@B	�MB	��B	��B	�LB	�LB	��B	��B	��B	�*B	ǮB	�BB	�<B	��B	�BB	�B	��B	B	ɆB	бB	��B	רB	��B	��B	�&B	�2B	�B	�QB	�B	��B	��B	�B	��B	�B	��B	�B	�]B	�B	�WB	��B	��B	�QB	��B	�"B	�B	��B	�B	�yB	�B	�B	��B	�DB	�QB	��B	�B	�B	��B	�yB	��B	��B	��B	�B	�sB	��B	�fB	�mB	�B	�sB	�mB	�
B	�>B	�B	��B	�B	��B	�B	�B	�DB	�QB	�"B	�)B	�B	�oB	��B	��B	�xB	��B
 4B
oB
 �B
B
 �B
 �B
 4B
AB
B
B
 iB
 �B	�(B	�.B	��B	�cB
 �B
  B	��B	�xB	�B	��B	��B	�B	�B	�%B	�;B	�/B	��B	��B	�B	�KB	�B	�B	�B	��B	��B	�B	�|B	��B	��B	��B	�B	�B	�|B	�B	�8B	��B	�B	��B	��B	��B	�TB	�B	��B	��B	�B	�B	��B	�B	��B	�B	�B	�cB	��B
�B
4B
�B
	7B
�B
AB
�B	��B	�"B
{B
YB
�B
;B
 �B	�xB	�rB	�(B	�DB	��B	��B	��B	�`B	��B	��B	�cB
$B
�B
MB
VB
OB
 \B
~B
IB
B
!B
!B
B
�B
�B
#:B
#�B
2�B
49B
2�B
-B
$�B
#�B
"�B
#B
$�B
!�B
8RB
<B
7�B
:�B
A B
CaB
I�B
M�B
LdB
K�B
OvB
P�B
U�B
d�B
_B
a�B
b�B
k�B
rGB
s�B
r�B
t�B
{JB
��B
�PB
�eB
�"B
��B
�4B
��B
��B
�B
�?B
��B
�qB
��B
��B
�qB
��B
��B
�B
��B
�B
�qB
�B
��B
��B
��B
��B
��B
�}B
�}B
��B
�gB
ɆB
�zB
��B
ݘB
��B
�AB
��B
�"B
�rB
�`B
�JB
��B
�B
�B
�BAB �B.BB1B�BBOB�B&B&�B)_B(XB(�B.}B,�B.IB.IBJ#B7B>�B<�B<6B9�B:�B<�B@�B=�B@OB?�B@�BDgBH�BI�BI�BN�BQBQ�BR�BS�BT,BT�BP�BP}BO�BO�BN�BNBM�B^5BO�BYKB[�BU�BW
B^5B^�B[�Bo5By�Bf�BkBg�Bd&Bf�Bd&BbBa�BcTBm]BaHBd&Bc�B_pBd�BaHBkQBe�B`�Bg�BaB]dB^jBbNB^5B`�B[#B]�B`BB^�B_�B[�Bb�B_pB\�BZQB\�B[�BX�BTaBV�BY�BW�BP}BN�BP�BZQBP�BQ�BR�B[�BW�BT�BN�BS[BVmBX�BN<BQ�BQ�BS�BR�BRTBT�BP�BO�BP�BP�BV�BQ�BQ�BOvBNpBM�BU2BVBP}BPBN�BN�BRTBOBBQNBJ�BM�BZ�BOBBRTBO�BOBO�BQNBU�B[�B^jBXyBWsB`B_�B_�B]�B]�BW�BU�BX�BYBZB_BWsBR�BS�BW
B\)BS&B`�BaHBbNB\�B^BVmB`Bb�Be�BjKBffBq�Bx�BpoBg�Be�Bq�Bt�BYB`BiBe�BhsBjB\�BS�BMjBJXBG�BI�B>�B;�B5B/�B+�B+kB#:BB'�BqB~B�B
��B
��B
�B
�]B
�B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�B
ߤB
�pB
��B
ݘB
چB
�B
�B
��B
�9B
��B
�'B
�XB
�B
��B
��B
�aB
��B
��B
��B
�OB
��B
�xB
�B
��B
�lB
�lB
��B
�GB
~(B
x�B
z�B
y>B
t�B
s�B
y>B
�B
l�B
a|B
Y�B
U2B
Z�B
X�B
^B
K)B
EB
B�B
@�B
=�B
;0B
:�B
C�B
M6B
-wB
#�B
�B
�B
�B
+B
�B
1B
	7B
�B
{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             B	%B	$@B	#�B	$@B	%,B	%FB	$@B	#�B	$ZB	%`B	&2B	$ZB	#�B	#�B	$ZB	$ZB	$�B	$&B	$�B	%�B	$&B	$�B	%B	%B	%`B	%zB	%zB	$�B	$�B	$�B	$�B	$�B	$�B	&LB	'RB	&fB	%`B	&2B	*KB	RB	�[B	��B	��B	�CB	��B	�&B	�tB	�!B	�nB	��B	��B
�B	�B
(B
!|B
'B
BAB
bhB
�B
�B
�;B
��B
��B
�FB�B)yB.�B7�BD3BG�BYKBa-BXyBX�BP�BP�BMPBJXBK^BD�BEBB�BCaBB'BL~BP.BK)BO(BS�Be`B`\BH�B.�BB
��B
��B
�B
��B
�B
�B
s�B
_VB
E�B
:�B
�B	�B	�B	��B	ĶB	�B	�XB	K�B	5�B	=�B	+�B	�B��B��B��B��B��B�^BƎB̘B��B�_B�*B��B�B�)B��B�FB��B�$B�sB��B��B�gB�B��B�6B��B�xB�VB�6B�B��B��B��B��B��B��B��B�	B��B�1B�#B��B�WB�#B��B�oB�B��B�1B��B��B�;B�!B��B��B�eB��B�XB�cB��B�6BB�8B��B�3B�SB�tB�.B�EB�OB��B��B�TB��B��B��B�5B�8B	�B	3B	
rB	�B	�B		B	�B	 �B	!�B	$�B	$tB	%`B	*B	6zB	8�B	0�B	33B	@4B	X�B	Z�B	^5B	fB	c B	c�B	b�B	d�B	c B	`�B	`�B	d&B	l"B	nB	kQB	v�B	��B	��B	�TB	�NB	�VB	�B	�NB	�B	��B	��B	�5B	�	B	��B	��B	�'B	�B	��B	��B	�JB	�B	��B	��B	�B	��B	��B	�B	�dB	��B	��B	��B	��B	�*B	��B	�}B	��B	��B	�;B	�!B	��B	�TB	��B	�LB	��B	�B	�;B	ÖB	ĜB	�B	�KB	ϑB	��B	�xB	��B	ϫB	�HB	�BB	�BB	�\B	ϫB	�hB	ңB	҉B	�[B	�&B	�B	��B	��B	��B	�2B	�B	ԯB	��B	ӏB	�@B	�&B	�[B	�FB	��B	�hB	�HB	�TB	�[B	�@B	ҽB	��B	ϑB	ϑB	ΊB	ΥB	�vB	�B	�.B	�vB	ѷB	�TB	ҽB	�oB	�B	�oB	��B	�FB	՛B	��B	�,B	��B	�B	�B	��B	ԕB	�aB	��B	ּB	�
B	�7B	ؓB	�EB	�KB	چB	�#B	��B	یB	�)B	�CB	�CB	�CB	��B	�IB	�]B	�CB	ݲB	�OB	��B	ߊB	��B	��B	�B	�NB	� B	��B	��B	�4B	�B	� B	�B	�&B	�FB	��B	�2B	�2B	�B	�RB	��B	�B	�B	�B	�LB	��B	��B	�fB	��B	��B	��B	�eB	�B	�B	�B	�"B	�B	��B	�B	��B	��B	�kB	�)B	�}B	�B	� B	�OB	�B	�!B	�UB	�!B	�B	�B	�AB	�B	�[B	�3B	��B	��B	��B	�GB	�hB	�nB	�%B	�ZB	�ZB	�B	�>B	�>B	��B	��B	��B	�B	�8B	�8B	��B	�$B	�	B	�rB	�RB	�$B	�*B	��B	�^B	�DB	�<B	�VB	�VB	��B	�BB	�qB	�B	��B	�B
 iB	�.B	�}B
  B
uB
uB
�B
uB
�B
{B
�B
�B
B
AB
uB
B
B
3B
�B
9B
�B
B
%B
YB
YB
?B
?B
�B
+B
+B
1B
KB
�B
�B
�B

�B
	�B

	B

�B

�B
B
�B
~B
JB
�B
�B
�B
�B
pB
B
pB
pB
pB
B
vB
B
�B
�B
�B
HB
hB
�B
bB
}B
�B
[B
[B
[B
�B
FB
�B
MB
9B
�B
B
�B
B
B
�B
$B
YB
�B
yB
�B
yB
�B
B
B
�B
�B
7B
�B
�B
qB
)B
�B
�B
]B
xB
�B
B
jB
�B
;B
�B
�B
 B
 B
 �B
!B
!-B
!HB
!�B
!�B
"hB
"�B
"�B
"�B
# B
#�B
#nB
#TB
#�B
$�B
$@B
$&B
$�B
$�B
$�B
#�B
$@B
$�B
%FB
&LB
&�B
&�B
&�B
&�B
&�B
'RB
(
B
(
B
($B
($B
(
B
(
B
(�B
'�B
(
B
(�B
)B
)*B
)�B
)�B
,"B
,�B
,�B
,�B
-wB
./B
-�B
-)B
-)B
-wB
-B
,�B
-�B
/ B
0B
.�B
/5B
/�B
/�B
0;B
0B
/�B
0oB
0�B
1vB
1�B
2GB
2aB
2�B
2�B
2�B
2|B
3MB
3�B
4nB
4B
49B
6FB
7fB
7B
8�B
8�B
8lB
7�B
6FB
6�B
7�B
8RB
8�B
8lB
8�B
9�B
8B
8B
8�B
9rB
8�B
8lB
8�B
9$B
9$B
9rB
9�B
:�B
:�B
:�B
:�B
:�B
;B
<�B
=�B
<�B
=�B
>�B
?}B
@4B
@4B
@OB
@�B
@�B
AUB
A�B
A�B
A�B
BAB
BuB
B�B
C{B
C{B
CB
B�B
CB
C{B
C�B
C�B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
D�B
D�B
F%B
F�B
G+B
GzB
GzB
G�B
HB
H1B
HKB
HKB
H�B
H�B
IB
IB
I�B
I�B
I�B
I�B
JXB
J=B
J=B
J�B
KDB
LB
K�B
K�B
LJB
K�B
K�B
LB
MB
MjB
M�B
M�B
NB
N<B
NpB
N�B
N�B
O�B
O�B
Q B
Q4B
Q4B
Q�B
S@B
R�B
S[B
S�B
S�B
S�B
S�B
SuB
S�B
T�B
S�B
S�B
S�B
TFB
TaB
T�B
U2B
UgB
U�B
UgB
U�B
VB
VSB
VmB
VmB
V�B
W
B
W$B
WsB
XB
W�B
W�B
XEB
X�B
YKB
Y�B
ZB
ZQB
Z�B
[�B
[�B
[�B
[�B
[�B
\B
\�B
]~B
]�B
]�B
^�B
^�B
^�B
_B
_B
_;B
_;B
_VB
_�B
_�B
`B
`B
`\B
`�B
`�B
`�B
aB
a-B
abB
a�B
a�B
bNB
bB
a�B
bNB
b�B
cB
cB
cB
c B
cTB
c�B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
e,B
ezB
e`B
e�B
e�B
f�B
f�B
gB
g8B
gRB
g�B
h
B
hXB
h�B
h�B
i*B
iB
iB
iyB
iyB
i�B
iyB
i�B
i�B
i�B
i�B
i�B
j0B
jeB
jB
j�B
jB
j�B
kB
k6B
kkB
kQB
kQB
k�B
lB
lB
lqB
lWB
lWB
lB
lqB
l�B
l�B
mB
mCB
m)B
m]B
m�B
m�B
ncB
ncB
n}B
n�B
n�B
n}B
n�B
oB
o�B
o�B
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qAB
qB
rGB
rB
rGB
r�B
r|B
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
t�B
t�B
uB
u%B
u�B
u�B
u�B
u�B
vB
v+B
vB
v+B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
xB
xB
x�B
x�B
y	B
y>B
y$B
y>B
y>B
yXB
y>B
y�B
y�B
zxB
zDB
z�B
z�B
z�B
z�B
{B
{dB
{dB
{�B
|�B
}<B
}�B
}�B
~B
}�B
}�B
~B
~BB
~�B
~wB
~�B
~�B
B
~�B
.B
B
.B
cB
}B
� B
�B
�B
�B
�B
� B
�OB
�OB
��B
�B
� G�O�B	%�B	%B	%zB	%zB	%B	$�B	$tB	$@B	$@B	#nB	#�B	$B	#:B	#�B	#�B	$B	$@B	$tB	$tB	&B	%zB	%FB	%�B	%�B	%B	%B	$�B	$tB	#�B	#nB	$�B	"hB	$tB	#nB	$@B	$�B	$@B	#�B	%zB	"�B	$�B	'RB	&LB	"�B	%FB	$tB	$@B	$�B	$�B	$�B	$@B	#�B	$B	$@B	$B	#�B	$@B	$B	$B	#�B	$@B	#nB	#�B	#�B	#�B	#�B	#�B	#�B	#�B	#�B	$B	$tB	$�B	$tB	$@B	$�B	%FB	$@B	$B	$tB	#�B	#�B	#�B	#�B	$�B	$�B	%B	$tB	$@B	$tB	$tB	$�B	$�B	$�B	%B	#B	"�B	#�B	#�B	%�B	&�B	&B	$@B	"�B	#nB	&�B	%�B	%�B	&�B	%zB	%�B	%�B	&B	%�B	!�B	$�B	$@B	$B	#�B	#�B	$tB	$tB	#�B	%B	%FB	%B	$�B	$�B	$@B	$�B	$�B	$�B	$�B	$�B	%B	%�B	%FB	%FB	%�B	$tB	%B	$tB	$�B	$�B	%FB	%zB	%zB	%FB	%zB	%FB	&B	%zB	%�B	%zB	%FB	%FB	%FB	%zB	%�B	%FB	$�B	%zB	%zB	%FB	%�B	%B	%B	$tB	$tB	$@B	$tB	$@B	$�B	$tB	#�B	$@B	$tB	$�B	$�B	$�B	$�B	$�B	#�B	$�B	$@B	$tB	$tB	$@B	$tB	$tB	$tB	$�B	%B	$�B	$�B	$@B	$tB	$B	$@B	$@B	$�B	$tB	$tB	$�B	$�B	$�B	$�B	$�B	$�B	$�B	%�B	%B	%�B	'�B	($B	(�B	)�B	%�B	(XB	'RB	&B	(XB	'B	%FB	'�B	'�B	'�B	&�B	'�B	$�B	%�B	$�B	'B	$�B	$�B	%B	&LB	$�B	%zB	%B	%zB	$�B	%FB	$�B	%FB	$�B	%FB	$�B	#�B	$B	%B	$B	#:B	$@B	0!B	-B	,=B	6zB	PB	i�B	gB	m�B	poB	s�B	v�B	x8B	{JB	}"B	~]B	��B	�B	�B	��B	�mB	�B	�B	�mB	�HB	��B	��B	�`B	��B	��B	��B	��B	��B	�vB	�AB	�8B	�cB	ȚB	�YB	өB	ԯB	��B	��B	�FB	�B	��B	�B	�LB	��B	ބB	�\B	ߊB	߾B	�B	�IB	�	B	ٚB	ؓB	�B	�qB	��B	�eB	ؓB	�_B	�+B	�_B	��B	ؓB	��B	�B	ؓB	�=B	��B	ؓB	�+B	ٚB	ؓB	�qB	��B	�$B	׍B	�B	�B	�YB	�$B	�B	ּB	��B	�_B	ٚB	�7B	ބB	��B	�1B	��B	�B	��B	��B	�kB	�!B	�B	�LB	�*B	�qB	��B	�!B	�B	�B	�OB	�OB	��B	��B	��B	�B	�B	�B	��B	��B	�kB	�B	�B	�B	�wB	�*B	��B	�RB	�B	��B	��B	��B	��B	��B	�qB	�qB	��B	��B	�eB	�YB	յB	׍B	ܬB	�OB	�-B	�B	�B	�B	��B	�bB	�-B	�kB	��B	ևB	�=B	�~B	�B	�@B	�B	��B	�B	�B	�:B	�hB	�B	��B	�B	�bB	�CB	�B	��B
3B	��B	�FB	��B	�jB	��B	��B	�B	��B	�-B	�B	��B	��B	�OB	�*B	�$B	��B	��B	�zB	�B	�B	�B	�B	�tB	�B
�B	�UB	��B
B
B
B
0B
�B

�B
�B
�B
�B
dB
jB
�B
�B
!�B
"�B
!|B
�B
[B
�B
�B
�B
[B
}B
'B
*�B
&2B
)DB
/�B
2B
8�B
<PB
;B
:�B
>(B
?�B
D�B
S�B
M�B
PbB
Q4B
Z�B
`�B
b�B
abB
c�B
i�B
zxB
{�B
��B
|�B
|PB
�B
��B
��B
��B
��B
�RB
�B
�eB
�qB
�B
�kB
�kB
��B
��B
��B
�B
��B
�wB
��B
��B
�B
�IB
�B
�B
�!B
��B
�B
�B
�^B
�0B
�}B
��B
�yB
�B
�
B
��B
��B
�B
ޞB
�B
�B
��B
�5B
��B�B�BJB�B�BVB�BB�B�BYBBqB�B�B8�B%�B-)B+QB*�B(>B)B+B/5B,"B.�B.cB/5B2�B72B8lB8lB=VB?�B@ BA;BBBB�BC{B?.B>�B>]B>(B=VB<�B<BL�B>(BG�BJrBDBE�BL�BMPBJ=B]�BhXBUMBY�BVSBR�BUMBR�BP�BPbBQ�B[�BO�BR�BRoBM�BSuBO�BY�BTFBO(BVBO�BK�BL�BP�BL�BO(BI�BLJBN�BMBNVBJ=BQhBM�BKDBH�BKDBJ	BG_BB�BEBHfBFYB>�B=VB?.BH�B?cB@ BAoBJrBF%BCB=<BA�BD�BG+B<�B@�B@4BBABA B@�BCaB?cB>wB?HB?HBEB@OB@B>B=B<jBC�BD�B?B>�B=<B=qB@�B=�B?�B9�B<6BIB=�B@�B>wB=�B>wB?�BDgBJ=BL�BF�BFBN�BN"BN"BLBLJBFtBD3BGzBG�BH�BM�BFBA BB'BE�BJ�BA�BO\BO�BP�BK^BL�BEBN�BQhBTFBX�BT�B`\BgB^�BVSBTB`'Bc:BG�BN�BW�BT�BV�BX�BK^BB�B<B8�B6zB8B-wB*eB#�BB7BB�B�BB
	B
�B
��B
�B
�pB
ߤB
��B
�KB
�B
�B
�sB
�mB
՛B
�aB
�sB
ԕB
�9B
�NB
�<B
�B
�dB
�0B
�B
̘B
�EB
�B
��B
��B
��B
�
B
��B
��B
�jB
�B
��B
��B
�QB
�B
�dB
�)B
��B
}qB
xB
xB
y�B
q�B
l�B
g�B
i�B
g�B
cnB
bhB
g�B
nIB
[qB
P.B
H�B
C�B
I7B
G�B
L�B
9�B
3�B
1�B
/5B
,WB
)�B
)yB
2GB
;�B
CB
�B
�B
�B

�B
�B	�B
�B	�B	��B	�GG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<O��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<.||<�^<X��<���<6ޟ<A#�<St�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<fI�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<=n�<=TL<��<#�
<#�
<#�
<��+<6��<:|F<#�
<HK<#�
<3��<z�7<96Q<#�
<#�
<=�L<�<��<��z<#�
<#�
<��E<a�<#�
<#�
<s�<#�
<#�
<#�
<#�
<#�
<AC<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0167(+/-0.0027)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0167(+/-0.0027)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0];                                                                                         PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0];                                                                                         PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2020110608250320201106082503IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020111608004620201116080046QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020111608004620201116080046QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014220210427140142IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                