CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-01-13T16:30:22Z creation; 2022-09-06T18:25:47Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20220113163022  20220907192128  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               j   jAA  AOAO7825_008765_106                 7825_008765_106                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @ٱ�hH��@ٱ�hH��11  @ٱ�����@ٱ�����@4�hƒ�@4�hƒ��e#a���e#a��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�@B�\@�G�@�G�@��R@�  A ��A\)A   A-p�AAG�AaG�A�  A�  A���A�  A��A�  A�  A�  A��B�
B(�BQ�B�
B'�
B0  B7�
B?�
BG�BO�BW�
B_�
Bg�
Bo�
Bx  B�
B��B�  B��B��B��B��B�  B�  B�(�B�(�B��B��B��B��
B�  B�  B��B��B�  B�  B�{B�  B�  B�  B�  B�{B�  B�{B�(�B�{B�  C   C
=C  C�C�C	��C  C{C�C  C��C��C  C  C  C  C 
=C"{C${C&  C'��C)��C,  C.{C0
=C2
=C4  C5��C7��C9�C;�C>  C@  CB  CC��CF  CH  CJ  CL
=CN
=CP{CR
=CT  CU��CW�CY�C[��C^
=C`{Cb  Cc��Ce��Cg�Ci��Cl  Cn  Cp
=Cr
=Ct
=Cv  Cx
=Cz
=C|  C~
=C�C�C���C�  C���C���C�  C�C���C���C�
=C�C�  C�  C���C���C�  C�
=C�C�  C���C���C�  C�  C�C�  C���C���C���C��C�  C�\C�
=C�  C�C�C�C�  C���C���C���C���C���C���C���C���C�  C�  C���C�  C�  C�  C�  C���C�C�
=C�C���C���C���C�  C�  C�  C�C�
=C�  C���C�  C���C���C���C���C���C���C�  C�C�  C�  C���C���C���C���C���C�  C�C�  C���C���C���C�  C���C���C�C�  C���C���C�  C���C���C�C���C���C���C�  C�C�\C�C�  C�C�
=C�
=C�  C�  C�C�C�
=C�C�
=C�
=C�
=C�C�  C���C���C�  C�C�  C�  D �D � D  D� D  D� D  D�DD}qD�qD��D�D� D��D}qD�D��D�qD	}qD	�qD
}qD
�qD� DD�D  D� D  D� D�D� D�qD� D�D��D  Dz�D�qD� D�D� D�qD� D�D�D�D}qD��D}qD  D}qD�qD� D�D� D�qD� D�D� D  D}qD  D� D �D ��D!  D!��D"�D"� D#  D#� D$�D$��D%�D%� D&  D&� D'�D'}qD'�qD(� D(�qD)z�D*  D*��D+  D+}qD,�D,��D-  D-� D.  D.�D/  D/}qD/�qD0� D1�D1�D2  D2z�D3  D3��D4  D4� D4�qD5�D6�D6z�D7  D7��D8�D8��D8�qD9}qD:D:��D;D;��D<�D<��D<�qD=}qD>  D>��D?�D?� D@  D@� DA  DA��DB�DB� DC�DC��DD�DD�DEDE�DFDF�DF�qDGz�DH  DH��DI  DI}qDI�qDJ}qDK  DK��DL�DL}qDL��DM}qDN�DN� DO  DO� DP  DP� DQ  DQ}qDR  DR��DS  DS� DT  DT� DU  DU��DVDV� DW  DW�DX  DX� DY  DYz�DZ  DZ�D[�D[��D\D\��D]�D]�D^�D^}qD^�qD_� D`�D`� Da  Da� Db  Db� Db�qDc}qDd�Dd}qDd�qDe��Df�Df��Dg  Dg� Dh  Dh� Dh�qDi}qDi�qDj}qDj�qDk� Dl�Dl� Dm  Dm� Dm�qDn� Dn�qDo}qDp  Dp�DqDq��Dr  Dr}qDr�qDsz�Ds��Dtz�Dt��Duz�Dv  Dv��Dw  Dw}qDw�qDx� Dx�qDy}qDz  Dz��D{�D{}qD{�qD|��D}�D}z�D}�qD~� D  D}qD�HD�@ D�� D��HD��D�@ D�~�D��qD��)D�=qD�~�D�� D�HD�@ D�~�D���D���D�@ D��HD���D���D�@ D�� D���D�HD�AHD��HD��HD���D�>�D�~�D���D�HD�@ D�}qD���D�  D�@ D�� D��HD�  D�AHD���D��HD�  D�AHD���D��HD�  D�>�D�� D��HD�HD�@ D�� D���D�  D�@ D�~�D���D��qD�>�D��HD�� D�  D�>�D�~�D�� D��D�AHD�� D��qD��qD�=qD�~�D���D�HD�@ D�}qD���D�  D�>�D�~�D���D���D�>�D�~�D��HD��D�AHD���D�D�HD�@ D�~�D�� D�HD�@ D�� D���D�HD�B�D�� D��qD�HD�B�D���D�D�HD�@ D�~�D���D���D�@ D��HD���D�  D�AHD�� D��HD��D�B�D�� D�� D�HD�>�D�~�D�� D�HD�B�D�� D���D��qD�=qD�}qD�� D�  D�<)D�|)D���D���D�@ D�� D���D�  D�AHD�� D���D�HD�@ D�~�D��HD��D�B�D��HD�� D�  D�@ D�� D�� D��qD�=qD���D�D���D�>�D�~�D���D�HD�B�D��HD���D�  D�B�D��HD��HD�HD�@ D���D��HD��qD�=qD�}qD��qD�  D�@ D�� D�D��D�AHD�� D�� D�  D�>�D�~�D���D�  D�@ D�}qD���D�  D�@ D�}qD���D�  D�@ D��HD�D��D�C�D�� D��HD�HD�@ D�� D��qD�  D�C�D��HD���D�HD�@ D�}qD�� D�HD�AHD���D�D�HD�@ D�� D���D���D�AHD��HD�� D��D�AHD�� D���D���D�@ D�~�D�� D�HD�>�D�}qD��qD���D�<)D�}qD���D��qD�=qD��HD�D�  D�>�D�� D�D�HD�@ D�}qD���D�  D�AHDHD¾�D�  D�B�DÃ�D�D�HD�>�DĀ D�D�HD�@ DŁHD�D�  D�AHDƂ�D�� D�  D�AHDǁHD��HD���D�@ DȀ D�� D�HD�>�Dɀ D�� D��qD�>�D�~�D�� D��D�B�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D́HD��HD�HD�AHD�~�Dξ�D���D�@ Dπ DϽqD��qD�AHDЁHDо�D�  D�@ D�~�DѾ�D��D�B�DҀ DҽqD���D�>�D�}qD�� D�HD�>�D�|)DԽqD���D�>�DՀ DսqD���D�>�D�}qD�� D�HD�@ DׁHD��HD���D�>�D؀ Dؾ�D��qD�@ DفHD�� D�HD�B�DځHD�� D�  D�>�D�}qD۾�D���D�@ D܁HD�� D�  D�@ D�~�Dݾ�D�  D�@ Dހ D��HD�  D�>�D�~�D߾�D���D�>�D�}qD�� D�HD�AHD�HD�� D�HD�@ D�~�D⾸D���D�@ D�~�D�qD���D�@ D�~�D�� D��D�@ D� D�� D���D�@ D� D�qD���D�AHD炏D��HD���D�@ D�~�D辸D�  D�@ D� D�� D�HD�>�D�~�D�� D�  D�@ D�~�D��HD��D�B�D� D쾸D��qD�>�D�~�D��qD��qD�@ D� D�� D�  D�>�D� D�� D�HD�@ D�~�D�� D�HD�@ D�D�D�  D�@ D� D�D���D�AHD�D�� D��qD�>�D�~�D�� D�HD�@ D��HD�� D�  D�>�D�|)D���D�  D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D���D�=q>�?\)?aG�?���?�Q�?�?��H@��@&ff@@  @O\)@h��@u@��@�{@�(�@�G�@�\)@�@��
@���@�@޸R@�=q@�z�@�p�A�
AQ�A�RA�\A��Ap�A#33A'
=A.{A1G�A8Q�A;�AB�\AEAL��AP  AVffAZ=qA`��Ac�
Aj�HAn�RAtz�Ay��A\)A�=qA�(�A�\)A���A�z�A�ffA��A��A��RA���A��
A�A���A��\A�{A��A��\A���A�\)A���A���A�
=A�G�A�(�A�{A�G�A��HA�{A�Q�A�33A��A�  A��A�p�A�\)Aڏ\A�z�A߮AᙚA��A�
=A�\A�(�A�A�G�A���A��RA�=qA��
A�\)B ��B{B\)Bz�B�B33Bz�B	��B33B(�BB�RBQ�BG�B
=B�
B��BffB(�BG�B�RB  BG�B�RB�B!p�B"ffB$(�B$��B&�RB'�B)G�B*=qB+�B-�B.=qB/�B0��B2�\B3�B5�B6{B7�B8��B:ffB;\)B<��B>{B?�B@��BB=qBC\)BD��BF{BG33BH��BIBK\)BLQ�BM�BN�HBP��BQp�BS
=BT(�BU��BV�HBX(�BYp�BZ�\B\(�B]�B^�RB_�Bap�Bb=qBd  Bd��BfffBg�Bh��BjffBk33Bl��Bm�Bo�Bpz�BqBs
=BtQ�Bup�Bv�\Bx(�By�Bz�RB{�B}�B~=qB�B�ffB��B�B�=qB���B��B�Q�B���B��B�(�B���B�p�B�=qB���B��B�{B��RB��B�  B��HB�G�B�{B���B�33B�  B�z�B�G�B�B��\B��B�B�ffB���B��
B�=qB��B���B�=qB���B��B�=qB���B���B�  B��HB��B�  B���B�G�B�(�B���B�G�B�  B�z�B�G�B�B��\B��B��B�z�B��HB�B�=qB���B���B�(�B���B�\)B�=qB���B�p�B�  B��\B�\)B��B��\B��B��B�z�B��HB�B�{B��HB�p�B�{B��RB�33B��B�ffB��B��B�Q�B��B���B�ffB���B��B�ffB��HB�B�Q�B�
=B��B�Q�B�33BÙ�Bď\B�
=B��B�z�B�33B�{Bȏ\B�p�B�{Bʣ�B˙�B�{B���B͙�B�(�B��Bϙ�B�ffB��BѮB�z�B�33B�Bԣ�B�33B�  B֣�B�G�B�{Bأ�B�p�B�{BڸRBۙ�B�{B��HBݙ�B�{B�
=Bߙ�B�ffB�
=B�B��B��B�  B�RB�\)B�=qB���B�B�Q�B�33B�  B��B뙚B�{B�
=B�B�ffB�\)B�  B�RB�B�(�B��B�B�z�B�\)B��B��HB���B�(�B��B�B�z�B�\)B��B��RB�p�B�{B���B��C =qC �C �HCQ�C��C{CffCC=qCz�C��CQ�C��C�Cp�CC=qC�\C  C\)C�C33C�C�HC	\)C	��C
{C
z�C
��CG�C��C��Cp�C�RC�C�C��C=qC�\C��C=qCp�CC{C=qC�C��C�HC�CffC�C��C�C(�CffCz�C�RC�C
=CQ�Cz�C��C�C
=C33Cz�C��CC
=C(�CQ�C��C��C�C�C33Cz�C�C��C{C=qCffC��CC  C(�CQ�C��C�RC�HC(�CG�Cz�C�RC��C{C=qC\)C��C��C�HC(�CffCz�C�RC��C
=C=qC�C��C��C{C33C\)C��C��C�C33C\)Cz�CC��C{CQ�C�\C�C�
C �C G�C ffC �C �C!  C!=qC!z�C!��C!��C"{C"33C"\)C"��C"�HC#  C#=qC#p�C#�\C#�HC$
=C$(�C$p�C$��C$C%  C%G�C%ffC%��C%�HC&  C&(�C&z�C&�\C&C'
=C'33C'Q�C'��C'��C'�C((�C(p�C(�C(�RC)  C){C)Q�C)�\C)�C)�HC*�C*=qC*p�C*�C*C+  C+33C+Q�C+�\C+C+�HC,{C,\)C,p�C,��C,�HC-  C-33C-z�C-��C-��C.{C.33C.\)C.��C.�
C.��C/33C/p�C/�\C/C0  C0�C0Q�C0��C0�C0�HC1(�C1Q�C1p�C1C1�HC2
=C2G�C2�C2��C2�
C3�C3Q�C3p�C3�C3�C4{C4=qC4�C4�RC4�HC5�C5\)C5�C5�C6  C6�C6G�C6�\C6C6�HC7(�C7ffC7�C7�RC8  C833C8Q�C8�\C8�
C9  C9(�C9ffC9�C9�
C:
=C:\)C:z�C:�C;
=C;33C;Q�C;��C;�
C<  C<=qC<�C<��C<�HC=(�C=Q�C=�C=��C=��C>(�C>z�C>�C>��C?
=C?G�C?ffC?�C?�C@{C@G�C@��C@��C@�HCA(�CAp�CA�\CA��CB{CB=qCBp�CB�RCB��CC{CCQ�CC��CC�
CC��CD=qCD�CD�RCD�HCE(�CEp�CE�CE�
CF
=CF\)CF��CF�
CG  CG=qCG�\CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         ?��@�@B�\@�G�@�G�@��R@�  A ��A\)A   A-p�AAG�AaG�A�  A�  A���A�  A��A�  A�  A�  A��B�
B(�BQ�B�
B'�
B0  B7�
B?�
BG�BO�BW�
B_�
Bg�
Bo�
Bx  B�
B��B�  B��B��B��B��B�  B�  B�(�B�(�B��B��B��B��
B�  B�  B��B��B�  B�  B�{B�  B�  B�  B�  B�{B�  B�{B�(�B�{B�  C   C
=C  C�C�C	��C  C{C�C  C��C��C  C  C  C  C 
=C"{C${C&  C'��C)��C,  C.{C0
=C2
=C4  C5��C7��C9�C;�C>  C@  CB  CC��CF  CH  CJ  CL
=CN
=CP{CR
=CT  CU��CW�CY�C[��C^
=C`{Cb  Cc��Ce��Cg�Ci��Cl  Cn  Cp
=Cr
=Ct
=Cv  Cx
=Cz
=C|  C~
=C�C�C���C�  C���C���C�  C�C���C���C�
=C�C�  C�  C���C���C�  C�
=C�C�  C���C���C�  C�  C�C�  C���C���C���C��C�  C�\C�
=C�  C�C�C�C�  C���C���C���C���C���C���C���C���C�  C�  C���C�  C�  C�  C�  C���C�C�
=C�C���C���C���C�  C�  C�  C�C�
=C�  C���C�  C���C���C���C���C���C���C�  C�C�  C�  C���C���C���C���C���C�  C�C�  C���C���C���C�  C���C���C�C�  C���C���C�  C���C���C�C���C���C���C�  C�C�\C�C�  C�C�
=C�
=C�  C�  C�C�C�
=C�C�
=C�
=C�
=C�C�  C���C���C�  C�C�  C�  D �D � D  D� D  D� D  D�DD}qD�qD��D�D� D��D}qD�D��D�qD	}qD	�qD
}qD
�qD� DD�D  D� D  D� D�D� D�qD� D�D��D  Dz�D�qD� D�D� D�qD� D�D�D�D}qD��D}qD  D}qD�qD� D�D� D�qD� D�D� D  D}qD  D� D �D ��D!  D!��D"�D"� D#  D#� D$�D$��D%�D%� D&  D&� D'�D'}qD'�qD(� D(�qD)z�D*  D*��D+  D+}qD,�D,��D-  D-� D.  D.�D/  D/}qD/�qD0� D1�D1�D2  D2z�D3  D3��D4  D4� D4�qD5�D6�D6z�D7  D7��D8�D8��D8�qD9}qD:D:��D;D;��D<�D<��D<�qD=}qD>  D>��D?�D?� D@  D@� DA  DA��DB�DB� DC�DC��DD�DD�DEDE�DFDF�DF�qDGz�DH  DH��DI  DI}qDI�qDJ}qDK  DK��DL�DL}qDL��DM}qDN�DN� DO  DO� DP  DP� DQ  DQ}qDR  DR��DS  DS� DT  DT� DU  DU��DVDV� DW  DW�DX  DX� DY  DYz�DZ  DZ�D[�D[��D\D\��D]�D]�D^�D^}qD^�qD_� D`�D`� Da  Da� Db  Db� Db�qDc}qDd�Dd}qDd�qDe��Df�Df��Dg  Dg� Dh  Dh� Dh�qDi}qDi�qDj}qDj�qDk� Dl�Dl� Dm  Dm� Dm�qDn� Dn�qDo}qDp  Dp�DqDq��Dr  Dr}qDr�qDsz�Ds��Dtz�Dt��Duz�Dv  Dv��Dw  Dw}qDw�qDx� Dx�qDy}qDz  Dz��D{�D{}qD{�qD|��D}�D}z�D}�qD~� D  D}qD�HD�@ D�� D��HD��D�@ D�~�D��qD��)D�=qD�~�D�� D�HD�@ D�~�D���D���D�@ D��HD���D���D�@ D�� D���D�HD�AHD��HD��HD���D�>�D�~�D���D�HD�@ D�}qD���D�  D�@ D�� D��HD�  D�AHD���D��HD�  D�AHD���D��HD�  D�>�D�� D��HD�HD�@ D�� D���D�  D�@ D�~�D���D��qD�>�D��HD�� D�  D�>�D�~�D�� D��D�AHD�� D��qD��qD�=qD�~�D���D�HD�@ D�}qD���D�  D�>�D�~�D���D���D�>�D�~�D��HD��D�AHD���D�D�HD�@ D�~�D�� D�HD�@ D�� D���D�HD�B�D�� D��qD�HD�B�D���D�D�HD�@ D�~�D���D���D�@ D��HD���D�  D�AHD�� D��HD��D�B�D�� D�� D�HD�>�D�~�D�� D�HD�B�D�� D���D��qD�=qD�}qD�� D�  D�<)D�|)D���D���D�@ D�� D���D�  D�AHD�� D���D�HD�@ D�~�D��HD��D�B�D��HD�� D�  D�@ D�� D�� D��qD�=qD���D�D���D�>�D�~�D���D�HD�B�D��HD���D�  D�B�D��HD��HD�HD�@ D���D��HD��qD�=qD�}qD��qD�  D�@ D�� D�D��D�AHD�� D�� D�  D�>�D�~�D���D�  D�@ D�}qD���D�  D�@ D�}qD���D�  D�@ D��HD�D��D�C�D�� D��HD�HD�@ D�� D��qD�  D�C�D��HD���D�HD�@ D�}qD�� D�HD�AHD���D�D�HD�@ D�� D���D���D�AHD��HD�� D��D�AHD�� D���D���D�@ D�~�D�� D�HD�>�D�}qD��qD���D�<)D�}qD���D��qD�=qD��HD�D�  D�>�D�� D�D�HD�@ D�}qD���D�  D�AHDHD¾�D�  D�B�DÃ�D�D�HD�>�DĀ D�D�HD�@ DŁHD�D�  D�AHDƂ�D�� D�  D�AHDǁHD��HD���D�@ DȀ D�� D�HD�>�Dɀ D�� D��qD�>�D�~�D�� D��D�B�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D́HD��HD�HD�AHD�~�Dξ�D���D�@ Dπ DϽqD��qD�AHDЁHDо�D�  D�@ D�~�DѾ�D��D�B�DҀ DҽqD���D�>�D�}qD�� D�HD�>�D�|)DԽqD���D�>�DՀ DսqD���D�>�D�}qD�� D�HD�@ DׁHD��HD���D�>�D؀ Dؾ�D��qD�@ DفHD�� D�HD�B�DځHD�� D�  D�>�D�}qD۾�D���D�@ D܁HD�� D�  D�@ D�~�Dݾ�D�  D�@ Dހ D��HD�  D�>�D�~�D߾�D���D�>�D�}qD�� D�HD�AHD�HD�� D�HD�@ D�~�D⾸D���D�@ D�~�D�qD���D�@ D�~�D�� D��D�@ D� D�� D���D�@ D� D�qD���D�AHD炏D��HD���D�@ D�~�D辸D�  D�@ D� D�� D�HD�>�D�~�D�� D�  D�@ D�~�D��HD��D�B�D� D쾸D��qD�>�D�~�D��qD��qD�@ D� D�� D�  D�>�D� D�� D�HD�@ D�~�D�� D�HD�@ D�D�D�  D�@ D� D�D���D�AHD�D�� D��qD�>�D�~�D�� D�HD�@ D��HD�� D�  D�>�D�|)D���D�  D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D���G�O�>�?\)?aG�?���?�Q�?�?��H@��@&ff@@  @O\)@h��@u@��@�{@�(�@�G�@�\)@�@��
@���@�@޸R@�=q@�z�@�p�A�
AQ�A�RA�\A��Ap�A#33A'
=A.{A1G�A8Q�A;�AB�\AEAL��AP  AVffAZ=qA`��Ac�
Aj�HAn�RAtz�Ay��A\)A�=qA�(�A�\)A���A�z�A�ffA��A��A��RA���A��
A�A���A��\A�{A��A��\A���A�\)A���A���A�
=A�G�A�(�A�{A�G�A��HA�{A�Q�A�33A��A�  A��A�p�A�\)Aڏ\A�z�A߮AᙚA��A�
=A�\A�(�A�A�G�A���A��RA�=qA��
A�\)B ��B{B\)Bz�B�B33Bz�B	��B33B(�BB�RBQ�BG�B
=B�
B��BffB(�BG�B�RB  BG�B�RB�B!p�B"ffB$(�B$��B&�RB'�B)G�B*=qB+�B-�B.=qB/�B0��B2�\B3�B5�B6{B7�B8��B:ffB;\)B<��B>{B?�B@��BB=qBC\)BD��BF{BG33BH��BIBK\)BLQ�BM�BN�HBP��BQp�BS
=BT(�BU��BV�HBX(�BYp�BZ�\B\(�B]�B^�RB_�Bap�Bb=qBd  Bd��BfffBg�Bh��BjffBk33Bl��Bm�Bo�Bpz�BqBs
=BtQ�Bup�Bv�\Bx(�By�Bz�RB{�B}�B~=qB�B�ffB��B�B�=qB���B��B�Q�B���B��B�(�B���B�p�B�=qB���B��B�{B��RB��B�  B��HB�G�B�{B���B�33B�  B�z�B�G�B�B��\B��B�B�ffB���B��
B�=qB��B���B�=qB���B��B�=qB���B���B�  B��HB��B�  B���B�G�B�(�B���B�G�B�  B�z�B�G�B�B��\B��B��B�z�B��HB�B�=qB���B���B�(�B���B�\)B�=qB���B�p�B�  B��\B�\)B��B��\B��B��B�z�B��HB�B�{B��HB�p�B�{B��RB�33B��B�ffB��B��B�Q�B��B���B�ffB���B��B�ffB��HB�B�Q�B�
=B��B�Q�B�33BÙ�Bď\B�
=B��B�z�B�33B�{Bȏ\B�p�B�{Bʣ�B˙�B�{B���B͙�B�(�B��Bϙ�B�ffB��BѮB�z�B�33B�Bԣ�B�33B�  B֣�B�G�B�{Bأ�B�p�B�{BڸRBۙ�B�{B��HBݙ�B�{B�
=Bߙ�B�ffB�
=B�B��B��B�  B�RB�\)B�=qB���B�B�Q�B�33B�  B��B뙚B�{B�
=B�B�ffB�\)B�  B�RB�B�(�B��B�B�z�B�\)B��B��HB���B�(�B��B�B�z�B�\)B��B��RB�p�B�{B���B��C =qC �C �HCQ�C��C{CffCC=qCz�C��CQ�C��C�Cp�CC=qC�\C  C\)C�C33C�C�HC	\)C	��C
{C
z�C
��CG�C��C��Cp�C�RC�C�C��C=qC�\C��C=qCp�CC{C=qC�C��C�HC�CffC�C��C�C(�CffCz�C�RC�C
=CQ�Cz�C��C�C
=C33Cz�C��CC
=C(�CQ�C��C��C�C�C33Cz�C�C��C{C=qCffC��CC  C(�CQ�C��C�RC�HC(�CG�Cz�C�RC��C{C=qC\)C��C��C�HC(�CffCz�C�RC��C
=C=qC�C��C��C{C33C\)C��C��C�C33C\)Cz�CC��C{CQ�C�\C�C�
C �C G�C ffC �C �C!  C!=qC!z�C!��C!��C"{C"33C"\)C"��C"�HC#  C#=qC#p�C#�\C#�HC$
=C$(�C$p�C$��C$C%  C%G�C%ffC%��C%�HC&  C&(�C&z�C&�\C&C'
=C'33C'Q�C'��C'��C'�C((�C(p�C(�C(�RC)  C){C)Q�C)�\C)�C)�HC*�C*=qC*p�C*�C*C+  C+33C+Q�C+�\C+C+�HC,{C,\)C,p�C,��C,�HC-  C-33C-z�C-��C-��C.{C.33C.\)C.��C.�
C.��C/33C/p�C/�\C/C0  C0�C0Q�C0��C0�C0�HC1(�C1Q�C1p�C1C1�HC2
=C2G�C2�C2��C2�
C3�C3Q�C3p�C3�C3�C4{C4=qC4�C4�RC4�HC5�C5\)C5�C5�C6  C6�C6G�C6�\C6C6�HC7(�C7ffC7�C7�RC8  C833C8Q�C8�\C8�
C9  C9(�C9ffC9�C9�
C:
=C:\)C:z�C:�C;
=C;33C;Q�C;��C;�
C<  C<=qC<�C<��C<�HC=(�C=Q�C=�C=��C=��C>(�C>z�C>�C>��C?
=C?G�C?ffC?�C?�C@{C@G�C@��C@��C@�HCA(�CAp�CA�\CA��CB{CB=qCBp�CB�RCB��CC{CCQ�CC��CC�
CC��CD=qCD�CD�RCD�HCE(�CEp�CE�CE�
CF
=CF\)CF��CF�
CG  CG=qCG�\CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�p�A�r�A�r�A�n�A�p�A�r�A�r�A�v�A�x�A�x�A�x�A�z�A�r�A�^5A�G�A�-A�oA�1A�  A��A��
AȶFAȲ-AȬAț�A�~�A�jA�VA�I�A�C�A��A�?}A���A��A��A��HA���AƗ�A�-Aš�A���Aė�A�1'AÉ7A´9A�{A�{A�JA�K�A��A���A�n�A��^A��uA��yA� �A��#A���A��\A�`BA�;dA��mA�/A�\)A�p�A�z�A�{A�M�A�\)A�C�A��;A�{A��A��RA�9XA���A�A���A�O�A���A���A���A��mA�/A��#A��\A�S�A��#A��`A��A�=qA�l�A��A��TA��`A�VA���A���A�XA�oA��/A�oA��!A��PA�ZA�A�C�A�&�A~��A~�A}��A}�FA{XAv�AqoAn�9AmXAk�FAkVAj��Ai�AghsAc��Ab��Aa�mA_p�A^=qA]�#A\��AZ��AY�hAX=qAWVAVI�AUK�AS�TAP��AN^5AK�AG33AD��ACK�AB�+AA
=A@��A@{A?l�A>ȴA<�uA:Q�A:E�A:�A9oA8�A77LA5hsA4z�A3ƨA3C�A2VA1XA/K�A.~�A-��A,VA+33A*ffA)dZA(�A(�\A'�A%�mA%A$�HA$n�A!�A �\A 1A`BA��Ap�A�uA/AQ�Ap�A�`A��A�-A�`AQ�A+A&�AQ�A{A1'A~�AXA�RAƨA
I�A	�;A	hsAbNA�`A�
AffA��A�A�mA�;A�
A�^A�A��A�7Al�A33A(�AG�A ��A 1@��P@�\)@�O�@��P@�ff@�@��`@� �@���@�\)@���@�p�@�bN@��H@��@�@� �@���@�C�@�v�@��@�@��@�ȴ@�u@�=q@���@�ƨ@�@ܓu@��
@��y@�$�@��#@�@ٙ�@�7L@ؓu@�  @ץ�@�l�@�;d@���@�v�@�@��H@Ѓ@ϝ�@��@Η�@�hs@���@�z�@��@�%@�z�@�1@Ǿw@ǅ@�S�@�o@��@� �@\@��#@�x�@��`@��@�Q�@��@���@��@���@�v�@�O�@��u@��m@�K�@�
=@��@�v�@��@��`@�Q�@��@�ȴ@�J@���@��h@�j@��@�+@��!@�ff@�$�@�@��@��^@���@���@���@��h@�`B@���@��/@�Ĝ@��D@��@�@���@��T@�7L@���@��@�9X@��@��
@��w@�dZ@�=q@�7L@��@�z�@�(�@�  @���@�1@�  @��
@�l�@�\)@�"�@���@��R@�n�@�-@�X@�V@���@��u@�t�@�v�@��+@�M�@���@�X@�G�@�/@�/@��@�%@���@��/@�V@��@��
@�dZ@�+@��@�{@�@��#@��T@���@��@�7L@�O�@�O�@�p�@�x�@�G�@�Ĝ@�I�@�z�@�G�@�hs@���@�Q�@�b@��m@�l�@��H@��+@�M�@�J@��^@���@���@��^@��^@�&�@��/@���@�A�@�b@��
@�"�@���@���@���@�v�@�^5@�=q@�-@�{@���@���@���@�@��@�G�@���@��j@���@�I�@��@��
@��w@��P@�33@�ȴ@�n�@��@��#@�x�@���@��/@��9@��u@�bN@�b@��;@��;@�ƨ@���@��@�K�@�
=@���@�M�@���@��-@��7@�`B@��9@�A�@��@��m@�ƨ@��P@�dZ@�33@��@��H@���@��R@���@�~�@�ff@�M�@�J@���@���@�?}@�&�@�%@���@��@���@�Q�@�ƨ@�K�@�o@���@���@�J@���@�@��^@��-@��-@��-@���@���@�7L@�Z@�1@���@��;@���@�C�@�"�@��@���@��!@���@�~�@�ff@���@�`B@�7L@��@��`@��j@��9@��@��@���@�bN@�9X@��@�  @��@�P@;d@~ȴ@~5?@}@}�@}/@|�D@|(�@{�
@{C�@z�@z��@z=q@zJ@y��@yhs@y&�@x�9@x�u@x1'@w�P@v�y@v�+@v{@t�@t�@s�@so@s@s@r�@r�!@r-@q��@q7L@pQ�@o|�@o�@n�R@nE�@m��@m@m�-@m�-@mO�@l�@lj@l�@l�@k��@k��@k�m@kƨ@k�@kC�@j�@j�!@jn�@i�#@i&�@h�@g�@f�y@fff@e�@eO�@dI�@c��@c�@c33@co@b��@bn�@a�@a��@a�7@aG�@`�`@`�u@`Q�@_��@_�P@_|�@_|�@_l�@_\)@_+@^��@]�@\9X@[��@[t�@Z��@Z-@Yx�@XĜ@X �@W�@W��@W|�@W\)@W+@V��@V��@VE�@U�@U�h@U?}@T�@TZ@S�F@R�H@R~�@R=q@R-@Q��@Qx�@QX@Q�@P�`@P�`@P��@P�@PQ�@PA�@PA�@P1'@O�w@O\)@N��@N�y@N�@Nȴ@Nȴ@N�R@N�R@N��@N��@N��@N��@N��@N��@N�R@N�R@N�R@N�R@Nv�@NV@M�-@M/@Lz�@L1@K��@KdZ@KdZ@K"�@J�H@J��@J~�@JM�@J�@I�^@Ihs@I%@H�9@HbN@HA�@H1'@H  @G�P@GK�@G�@Fȴ@F��@F��@FV@F@E�T@E��@E@E@E�-@E�-@E��@E�@E/@E�@D�@D�j@D�@D9X@C�F@CC�@B�@B��@B��@A��@Ahs@@��@@bN@@b@?�w@?
=@>�+@>V@=��@=�h@=`B@<��@<Z@<�@;��@;�
@;��@;dZ@;C�@;@:�H@:��@:�!@:��@:�\@:^5@:M�@:=q@:-@9��@9��@9x�@9x�@97L@9&�@97L@9�@8 �@7K�@7
=@6ff@6$�@5�T@5/@5V@4�@4j@3��@3�@3S�@3"�@3@2��@2�!@2��@2��@2�\@2~�@2~�@2^5@2M�@2=q@1�^@0A�@/��@/��@/|�@/l�@/K�@.��@.�R@.��@.E�@.{@-@-`B@,z�@,I�@,1@+dZ@*��@*�\@*^5@*-@*�@)�@)�^@)��@)&�@)%@(Ĝ@(�u@(�@(r�@(r�@(r�@(Q�@'�@&5?@%�@$Z@#��@#��@#t�@#t�@#t�@#S�@#C�@#"�@"��@"-@!X@ �`@ �u@ Q�@ 1'@   @�@�;@�P@l�@K�@K�@;d@�@E�@@��@��@��@�h@�h@�h@��@�h@�@p�@p�@`B@?}@�@��@�/@�@�D@j@I�@ƨ@S�@o@-@7L@�`@Ĝ@Ĝ@Ĝ@Ĝ@�9@��@�@Q�@�@��@�y@��@��@�+@�+@v�@ff@V@V@V@E�@5?@{@�@�h@p�@/@�@��@�@��@�@j@9X@(�@�@1@��@��@�
@��@C�@~�@=q@-@J@��@G�@��@  @|�@;d@��@�@�R@v�@V@5?@$�@@�T@�T@@p�@�@�@Z@�F@t�@S�@C�@33@
��@	�@	��@	��@	x�@	X@	G�@	�@Ĝ@�9@��@�u@�u@�u@�u@bN@1'@b@�@�@|�@|�@l�@+@ȴ@��@�+@ff@ff@V@5?@{@�@��@�-@�@?}@/@��@�@�/@�@��A�r�A�t�A�r�A�r�A�p�A�p�A�r�A�r�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�p�A�r�A�p�A�r�A�n�A�r�A�n�A�t�A�n�A�t�A�n�A�t�A�p�A�r�A�p�A�t�A�n�A�r�A�n�A�r�A�p�A�p�A�p�A�r�A�n�A�t�A�r�A�r�A�r�A�p�A�t�A�n�A�v�A�p�A�v�A�r�A�v�A�r�A�x�A�t�A�v�A�x�A�v�A�x�A�t�A�x�A�v�A�x�A�x�A�z�A�v�A�z�A�t�A�z�A�t�A�|�A�t�A�|�A�t�A�z�A�t�A�z�A�v�A�|�A�v�A�|�A�v�A�|�A�x�A�|�A�r�A�p�A�jA�n�A�jA�dZA�bNA�Q�A�Q�A�O�A�S�A�Q�A�G�A�A�A�=qA�33A�33A�-A�33A�+A�(�A�(�A�$�A�{A�{A�bA�bA�
=A�JA�1A�1A�1A�%A�1A�A�%A�  A�A���A���A���A���A���A���A���A��`A��TA��A��/A��
A��#A���A�ȴAȾwAȶFAȶFAȲ-AȲ-AȲ-Aȴ9AȲ-Aȴ9AȰ!AȲ-AȮAȲ-Aȩ�AȮAȧ�AȬAȥ�Aȧ�Aȝ�Aȟ�Aș�Aȕ�Aȕ�AȍPAȋDAȁAȁA�v�A�v�A�n�A�r�A�n�A�l�A�hsA�dZA�`BA�ZA�ZA�S�A�VA�O�A�O�A�K�A�I�A�K�A�E�A�I�A�C�A�G�A�A�A�G�A�A�A�E�A�A�A�A�A�A�A�7LA�7LA�$�A�1AǼjAǩ�A�r�A�C�A�JA�A���A�A��A���A��A��A���A��A��A��yA��A��A��A��A��A��A��yA��A��A��A��A��mA��yA��/A��#A���A���A���A�A���Aƴ9Aƴ9AƮAƲ-AƬAƗ�AƍPA�l�A�dZA�C�A�A�A�-A��A��A��;A���A�ĜAť�Ať�AœuA�^5A�&�A��A���A���A��A��HA�AĬAġ�Aĕ�AąA�x�A�ffA�\)A�E�A�;dA�$�A���A��`A��A���A�ĜA×�A�M�A��A��yA��A��A�ȴA¾wA¡�A���A��uA���A�M�A��!A��A��
A��!A��A�&�A��A��-A��A�ZA�+A�
=A��yA��;A��wA��DA�~�A�M�A��A�%A�ȴA���A���A��7A�hsA�A�A�/A��A�JA���A��HA��9A��+A�z�A�r�A�jA�\)A�K�A� �A���A���A��yA�ZA���A��;A��\A�n�A�S�A��A��RA�7LA�  A�A��+A� �A��hA�^5A�VA��mA���A�ffA�I�A���A��;A�^5A�5?A���A���A���A��9A���A���A��hA��DA��DA��A�|�A�jA�dZA�\)A�K�A�G�A�E�A�?}A�?}A�33A�"�A��A�  A��A���A��A���A�l�A�(�A�{A���A�ĜA�~�A�VA��mA�VA��hA�&�A�JA�A��A��jA��uA��+A�~�A�r�A�ffA�XA�I�A�oA���A��A���A��7A�XA�=qA�&�A�{A��TA��\A�C�A��A��A���A���A�`BA��A���A��A��RA�jA�33A��A��RA���A�=qA�33A�33A�-A�/A�-A�(�A�+A�%A��yA��`A���A���A��A���A���A���A��uA��+A�bNA�=qA��A�JA�A���A��A��A��^A��A���A���A�ffA�E�A�{A��A���A�M�A�"�A�VA��A��A��`A��;A�ȴA���A��A�l�A�XA�A�A�bA���A��\A�bNA�C�A�$�A�VA��A��!A�jA��A��#A���A�^5A� �A��;A��jA��+A�K�A���A���A�v�A�M�A�7LA��A��wA���A���A���A��DA�t�A�`BA�XA�I�A�bA�1A���A���A��mA���A���A�E�A��/A��!A��A�XA�=qA�$�A�oA���A��TA���A��A��uA��A�M�A���A��/A���A�ĜA�ĜA��9A���A���A��7A��A�|�A�p�A�S�A�?}A�"�A��TA���A��jA���A���A���A��PA��A�|�A�ZA�;dA�-A��A�  A�  A���A���A��A��A��A��A��mA��mA��/A���A���A���A�A���A��hA�M�A�&�A��mA���A�r�A�oA�ƨA���A�x�A�XA�XA�ZA�VA�S�A�S�A�VA�Q�A�Q�A�S�A�G�A�5?A� �A�
=A���A��A�ƨA��PA�t�A�n�A�ZA�I�A�&�A�
=A��A��`A��A���A�t�A�VA�ĜA���A��7A��A�~�A�v�A�r�A�l�A�\)A�VA�Q�A�M�A�E�A�=qA�=qA�=qA�5?A�1'A�&�A���A���A��-A�O�A�7LA�9XA�5?A�5?A�1'A� �A�%A��A��A��yA��`A��mA��TA��TA��TA��mA��TA��`A��`A��;A��TA��TA��`A��`A��mA��TA��`A��`A��TA��TA��mA��`A��;A��TA��#A�ȴA�ƨA�A��A�VA��A���A��A�~�A�r�A��/A��DA��A�p�A�O�A���A�^5A�1A���A���A�x�A�t�A��+A�jA�bNA�hsA�ffA�dZA�VA�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�p�A�r�A�r�A�n�A�p�A�r�A�r�A�v�A�x�A�x�A�x�A�z�A�r�A�^5A�G�A�-A�oA�1A�  A��A��
AȶFAȲ-AȬAț�A�~�A�jA�VA�I�A�C�A��A�?}A���A��A��A��HA���AƗ�A�-Aš�A���Aė�A�1'AÉ7A´9A�{A�{A�JA�K�A��A���A�n�A��^A��uA��yA� �A��#A���A��\A�`BA�;dA��mA�/A�\)A�p�A�z�A�{A�M�A�\)A�C�A��;A�{A��A��RA�9XA���A�A���A�O�A���A���A���A��mA�/A��#A��\A�S�A��#A��`A��A�=qA�l�A��A��TA��`A�VA���A���A�XA�oA��/A�oA��!A��PA�ZA�A�C�A�&�A~��A~�A}��A}�FA{XAv�AqoAn�9AmXAk�FAkVAj��Ai�AghsAc��Ab��Aa�mA_p�A^=qA]�#A\��AZ��AY�hAX=qAWVAVI�AUK�AS�TAP��AN^5AK�AG33AD��ACK�AB�+AA
=A@��A@{A?l�A>ȴA<�uA:Q�A:E�A:�A9oA8�A77LA5hsA4z�A3ƨA3C�A2VA1XA/K�A.~�A-��A,VA+33A*ffA)dZA(�A(�\A'�A%�mA%A$�HA$n�A!�A �\A 1A`BA��Ap�A�uA/AQ�Ap�A�`A��A�-A�`AQ�A+A&�AQ�A{A1'A~�AXA�RAƨA
I�A	�;A	hsAbNA�`A�
AffA��A�A�mA�;A�
A�^A�A��A�7Al�A33A(�AG�A ��A 1@��P@�\)@�O�@��P@�ff@�@��`@� �@���@�\)@���@�p�@�bN@��H@��@�@� �@���@�C�@�v�@��@�@��@�ȴ@�u@�=q@���@�ƨ@�@ܓu@��
@��y@�$�@��#@�@ٙ�@�7L@ؓu@�  @ץ�@�l�@�;d@���@�v�@�@��H@Ѓ@ϝ�@��@Η�@�hs@���@�z�@��@�%@�z�@�1@Ǿw@ǅ@�S�@�o@��@� �@\@��#@�x�@��`@��@�Q�@��@���@��@���@�v�@�O�@��u@��m@�K�@�
=@��@�v�@��@��`@�Q�@��@�ȴ@�J@���@��h@�j@��@�+@��!@�ff@�$�@�@��@��^@���@���@���@��h@�`B@���@��/@�Ĝ@��D@��@�@���@��T@�7L@���@��@�9X@��@��
@��w@�dZ@�=q@�7L@��@�z�@�(�@�  @���@�1@�  @��
@�l�@�\)@�"�@���@��R@�n�@�-@�X@�V@���@��u@�t�@�v�@��+@�M�@���@�X@�G�@�/@�/@��@�%@���@��/@�V@��@��
@�dZ@�+@��@�{@�@��#@��T@���@��@�7L@�O�@�O�@�p�@�x�@�G�@�Ĝ@�I�@�z�@�G�@�hs@���@�Q�@�b@��m@�l�@��H@��+@�M�@�J@��^@���@���@��^@��^@�&�@��/@���@�A�@�b@��
@�"�@���@���@���@�v�@�^5@�=q@�-@�{@���@���@���@�@��@�G�@���@��j@���@�I�@��@��
@��w@��P@�33@�ȴ@�n�@��@��#@�x�@���@��/@��9@��u@�bN@�b@��;@��;@�ƨ@���@��@�K�@�
=@���@�M�@���@��-@��7@�`B@��9@�A�@��@��m@�ƨ@��P@�dZ@�33@��@��H@���@��R@���@�~�@�ff@�M�@�J@���@���@�?}@�&�@�%@���@��@���@�Q�@�ƨ@�K�@�o@���@���@�J@���@�@��^@��-@��-@��-@���@���@�7L@�Z@�1@���@��;@���@�C�@�"�@��@���@��!@���@�~�@�ff@���@�`B@�7L@��@��`@��j@��9@��@��@���@�bN@�9X@��@�  @��@�P@;d@~ȴ@~5?@}@}�@}/@|�D@|(�@{�
@{C�@z�@z��@z=q@zJ@y��@yhs@y&�@x�9@x�u@x1'@w�P@v�y@v�+@v{@t�@t�@s�@so@s@s@r�@r�!@r-@q��@q7L@pQ�@o|�@o�@n�R@nE�@m��@m@m�-@m�-@mO�@l�@lj@l�@l�@k��@k��@k�m@kƨ@k�@kC�@j�@j�!@jn�@i�#@i&�@h�@g�@f�y@fff@e�@eO�@dI�@c��@c�@c33@co@b��@bn�@a�@a��@a�7@aG�@`�`@`�u@`Q�@_��@_�P@_|�@_|�@_l�@_\)@_+@^��@]�@\9X@[��@[t�@Z��@Z-@Yx�@XĜ@X �@W�@W��@W|�@W\)@W+@V��@V��@VE�@U�@U�h@U?}@T�@TZ@S�F@R�H@R~�@R=q@R-@Q��@Qx�@QX@Q�@P�`@P�`@P��@P�@PQ�@PA�@PA�@P1'@O�w@O\)@N��@N�y@N�@Nȴ@Nȴ@N�R@N�R@N��@N��@N��@N��@N��@N��@N�R@N�R@N�R@N�R@Nv�@NV@M�-@M/@Lz�@L1@K��@KdZ@KdZ@K"�@J�H@J��@J~�@JM�@J�@I�^@Ihs@I%@H�9@HbN@HA�@H1'@H  @G�P@GK�@G�@Fȴ@F��@F��@FV@F@E�T@E��@E@E@E�-@E�-@E��@E�@E/@E�@D�@D�j@D�@D9X@C�F@CC�@B�@B��@B��@A��@Ahs@@��@@bN@@b@?�w@?
=@>�+@>V@=��@=�h@=`B@<��@<Z@<�@;��@;�
@;��@;dZ@;C�@;@:�H@:��@:�!@:��@:�\@:^5@:M�@:=q@:-@9��@9��@9x�@9x�@97L@9&�@97L@9�@8 �@7K�@7
=@6ff@6$�@5�T@5/@5V@4�@4j@3��@3�@3S�@3"�@3@2��@2�!@2��@2��@2�\@2~�@2~�@2^5@2M�@2=q@1�^@0A�@/��@/��@/|�@/l�@/K�@.��@.�R@.��@.E�@.{@-@-`B@,z�@,I�@,1@+dZ@*��@*�\@*^5@*-@*�@)�@)�^@)��@)&�@)%@(Ĝ@(�u@(�@(r�@(r�@(r�@(Q�@'�@&5?@%�@$Z@#��@#��@#t�@#t�@#t�@#S�@#C�@#"�@"��@"-@!X@ �`@ �u@ Q�@ 1'@   @�@�;@�P@l�@K�@K�@;d@�@E�@@��@��@��@�h@�h@�h@��@�h@�@p�@p�@`B@?}@�@��@�/@�@�D@j@I�@ƨ@S�@o@-@7L@�`@Ĝ@Ĝ@Ĝ@Ĝ@�9@��@�@Q�@�@��@�y@��@��@�+@�+@v�@ff@V@V@V@E�@5?@{@�@�h@p�@/@�@��@�@��@�@j@9X@(�@�@1@��@��@�
@��@C�@~�@=q@-@J@��@G�@��@  @|�@;d@��@�@�R@v�@V@5?@$�@@�T@�T@@p�@�@�@Z@�F@t�@S�@C�@33@
��@	�@	��@	��@	x�@	X@	G�@	�@Ĝ@�9@��@�u@�u@�u@�u@bN@1'@b@�@�@|�@|�@l�@+@ȴ@��@�+@ff@ff@V@5?@{@�@��@�-@�@?}@/@��@�@�/@�G�O�A�r�A�t�A�r�A�r�A�p�A�p�A�r�A�r�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�p�A�r�A�p�A�r�A�n�A�r�A�n�A�t�A�n�A�t�A�n�A�t�A�p�A�r�A�p�A�t�A�n�A�r�A�n�A�r�A�p�A�p�A�p�A�r�A�n�A�t�A�r�A�r�A�r�A�p�A�t�A�n�A�v�A�p�A�v�A�r�A�v�A�r�A�x�A�t�A�v�A�x�A�v�A�x�A�t�A�x�A�v�A�x�A�x�A�z�A�v�A�z�A�t�A�z�A�t�A�|�A�t�A�|�A�t�A�z�A�t�A�z�A�v�A�|�A�v�A�|�A�v�A�|�A�x�A�|�A�r�A�p�A�jA�n�A�jA�dZA�bNA�Q�A�Q�A�O�A�S�A�Q�A�G�A�A�A�=qA�33A�33A�-A�33A�+A�(�A�(�A�$�A�{A�{A�bA�bA�
=A�JA�1A�1A�1A�%A�1A�A�%A�  A�A���A���A���A���A���A���A���A��`A��TA��A��/A��
A��#A���A�ȴAȾwAȶFAȶFAȲ-AȲ-AȲ-Aȴ9AȲ-Aȴ9AȰ!AȲ-AȮAȲ-Aȩ�AȮAȧ�AȬAȥ�Aȧ�Aȝ�Aȟ�Aș�Aȕ�Aȕ�AȍPAȋDAȁAȁA�v�A�v�A�n�A�r�A�n�A�l�A�hsA�dZA�`BA�ZA�ZA�S�A�VA�O�A�O�A�K�A�I�A�K�A�E�A�I�A�C�A�G�A�A�A�G�A�A�A�E�A�A�A�A�A�A�A�7LA�7LA�$�A�1AǼjAǩ�A�r�A�C�A�JA�A���A�A��A���A��A��A���A��A��A��yA��A��A��A��A��A��A��yA��A��A��A��A��mA��yA��/A��#A���A���A���A�A���Aƴ9Aƴ9AƮAƲ-AƬAƗ�AƍPA�l�A�dZA�C�A�A�A�-A��A��A��;A���A�ĜAť�Ať�AœuA�^5A�&�A��A���A���A��A��HA�AĬAġ�Aĕ�AąA�x�A�ffA�\)A�E�A�;dA�$�A���A��`A��A���A�ĜA×�A�M�A��A��yA��A��A�ȴA¾wA¡�A���A��uA���A�M�A��!A��A��
A��!A��A�&�A��A��-A��A�ZA�+A�
=A��yA��;A��wA��DA�~�A�M�A��A�%A�ȴA���A���A��7A�hsA�A�A�/A��A�JA���A��HA��9A��+A�z�A�r�A�jA�\)A�K�A� �A���A���A��yA�ZA���A��;A��\A�n�A�S�A��A��RA�7LA�  A�A��+A� �A��hA�^5A�VA��mA���A�ffA�I�A���A��;A�^5A�5?A���A���A���A��9A���A���A��hA��DA��DA��A�|�A�jA�dZA�\)A�K�A�G�A�E�A�?}A�?}A�33A�"�A��A�  A��A���A��A���A�l�A�(�A�{A���A�ĜA�~�A�VA��mA�VA��hA�&�A�JA�A��A��jA��uA��+A�~�A�r�A�ffA�XA�I�A�oA���A��A���A��7A�XA�=qA�&�A�{A��TA��\A�C�A��A��A���A���A�`BA��A���A��A��RA�jA�33A��A��RA���A�=qA�33A�33A�-A�/A�-A�(�A�+A�%A��yA��`A���A���A��A���A���A���A��uA��+A�bNA�=qA��A�JA�A���A��A��A��^A��A���A���A�ffA�E�A�{A��A���A�M�A�"�A�VA��A��A��`A��;A�ȴA���A��A�l�A�XA�A�A�bA���A��\A�bNA�C�A�$�A�VA��A��!A�jA��A��#A���A�^5A� �A��;A��jA��+A�K�A���A���A�v�A�M�A�7LA��A��wA���A���A���A��DA�t�A�`BA�XA�I�A�bA�1A���A���A��mA���A���A�E�A��/A��!A��A�XA�=qA�$�A�oA���A��TA���A��A��uA��A�M�A���A��/A���A�ĜA�ĜA��9A���A���A��7A��A�|�A�p�A�S�A�?}A�"�A��TA���A��jA���A���A���A��PA��A�|�A�ZA�;dA�-A��A�  A�  A���A���A��A��A��A��A��mA��mA��/A���A���A���A�A���A��hA�M�A�&�A��mA���A�r�A�oA�ƨA���A�x�A�XA�XA�ZA�VA�S�A�S�A�VA�Q�A�Q�A�S�A�G�A�5?A� �A�
=A���A��A�ƨA��PA�t�A�n�A�ZA�I�A�&�A�
=A��A��`A��A���A�t�A�VA�ĜA���A��7A��A�~�A�v�A�r�A�l�A�\)A�VA�Q�A�M�A�E�A�=qA�=qA�=qA�5?A�1'A�&�A���A���A��-A�O�A�7LA�9XA�5?A�5?A�1'A� �A�%A��A��A��yA��`A��mA��TA��TA��TA��mA��TA��`A��`A��;A��TA��TA��`A��`A��mA��TA��`A��`A��TA��TA��mA��`A��;A��TA��#A�ȴA�ƨA�A��A�VA��A���A��A�~�A�r�A��/A��DA��A�p�A�O�A���A�^5A�1A���A���A�x�A�t�A��+A�jA�bNA�hsA�ffA�dZA�VA�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B��B�B͟B�B��B��B�B��B�BΥB�pB�pB�<B�B�<B�<B�pB��B�jB̘B˒B��BɺBɺB�RBɆBȴB�zB�B�B�B�EB��B�tB�tB��BȀBƨB�[B�zBʌB��B�B�WB�B�DBBB"hB*eB0!BH�BJ#BH�BK�BK�BH�BC-BL0BB[B;0B8�B2�B+6B&B%FB"�B \B�B%zB��B�B�B�"B�B�9B˒B��B��B�6B�0B��B{�B`�BOBLdBGzB=�B4B 'B�B
�B��B��B�lB�DB�TB�]B� B�vB�dB�?B��B��B{Bv�BpoBiBO�BLdBH�BGEB7B,qB"hB�BkB$B�B
�(B
�B
�jB
��B
�BB
��B
�B
�OB
�LB
�B
��B
��B
y>B
m�B
iDB
e�B
Z�B
S�B
L�B
EmB
AUB
:�B
5?B
(�B
	B
�B	��B	� B	�DB	�&B	�B	�QB	��B	ӏB	��B	�B	� B	�wB	�}B	��B	�B	��B	��B	�*B	��B	�:B	�\B	�B	��B	�B	�"B	�"B	�%B	��B	��B	~]B	}�B	{�B	y	B	r�B	p;B	n�B	q�B	e�B	d&B	b�B	^�B	]/B	[�B	V�B	S�B	R�B	N�B	OB	NB	JXB	H�B	I�B	D�B	@�B	=qB	CaB	;0B	9�B	6B	5�B	4B	/�B	.�B	0�B	+�B	.IB	/OB	3�B	8�B	=qB	?}B	?�B	@OB	@OB	@�B	@�B	AUB	CaB	L�B	R�B	Y�B	\)B	Z�B	X�B	`�B	^�B	_pB	bB	bNB	b�B	b�B	a|B	bB	a�B	a�B	a�B	b�B	cTB	cTB	c�B	cTB	c�B	g�B	d&B	bB	ffB	f2B	jB	kB	l�B	tB	u%B	v�B	z�B	{�B	|�B	|�B	}�B	� B	��B	��B	��B	�B	��B	�B	�B	��B	�B	�(B	�.B	��B	��B	��B	�~B	�B	��B	�zB	�LB	�RB	��B	��B	��B	��B	�*B	��B	��B	��B	�B	��B	�LB	��B	�XB	�^B	��B	�<B	�B	��B	�B	��B	ʌB	�)B	��B	�dB	�B	� B	��B	��B	�yB	یB	ܒB	��B	�sB	�B	��B	�B	�B	�;B	��B	�GB	�B	��B	��B	�+B	�`B	��B	��B	��B	�`B	��B	��B	�]B
 �B
�B
�B
bB
�B
FB
�B
�B
�B
eB
�B
"hB
#B
&�B
(�B
*�B
+6B
+�B
-�B
4�B
8�B
9$B
9�B
=�B
A�B
C�B
G�B
M6B
LdB
MB
K�B
M6B
J�B
P�B
TaB
V9B
YB
[�B
]�B
]�B
^�B
_�B
bNB
e,B
gB
jB
l�B
l�B
lWB
k�B
m�B
q�B
tTB
v�B
y�B
z�B
|B
}�B
}�B
�B
��B
�oB
��B
��B
��B
��B
�(B
�4B
��B
��B
�:B
��B
�B
�uB
��B
��B
�oB
�oB
��B
�eB
�B
��B
��B
��B
��B
��B
��B
�kB
�IB
��B
�B
��B
�UB
�[B
�-B
�aB
��B
��B
��B
��B
��B
�FB
��B
�RB
��B
��B
��B
�*B
��B
�dB
��B
�qB
�BB
��B
�}B
� B
�'B
��B
�[B
��B
�aB
�9B
�tB
�tB
�tB
�tB
�?B
�B
�B
�zB
�B
ʌB
ʌB
��B
��B
�6B
�<B
ΥB
ΥB
�B
ϫB
��B
�HB
��B
��B
�B
�NB
уB
��B
��B
� B
ҽB
�[B
��B
��B
��B
�2B
��B
�B
�9B
��B
�EB
�QB
چB
چB
�WB
�B
�B
�5B
�5B
�5B
�5B
�B
��B
ݘB
�B
�B
�HB
�HB
�HB
�NB
�B
� B
�B
�B
�B
�B
�B
�B
�fB
�fB
�fB
�B
�8B
�8B
�8B
�8B
�8B
�8B
��B
�
B
�>B
�>B
�B
�DB
�yB
�KB
�B
�QB
�B
�B
�"B
�B
��B
�B
�B
��B
�cB
�cB
��B
��B
� B
�iB
�iB
�B
�B
�B
�B
�B
�|B
��B
��B
��B
�B
�|B
�|B
�B
�B
�B
�MB
�TB
��B
��B
�%B
�`B
��B
��B
��B
��B
�fB
��B
��B
��B
��B
�	B
��B
�	B
�	B
�>B
�rB
�rB
�rB
��B
�B
��B
�JB
��B
��B
�"B
��B
�]B
��B
�.B  B
��B
��B iB �BBB �B;B�BBBABuB�BuBuBuB�B{BB�B�B�BYB+B�B1B�B�B	7B	B	B	lB	7B	�B
	B
	B
rB
rB
�B
�B�B�B�B�B�B~B~B�BPBBB�B�B�B�BPB�B�B�B(B(B(B(B(B(B�B�B�B(B(B\B(B(B(B�B�B�B�B�B�B4B B�B4B4B�B�BBBBBB@BuB�B�B�B�B�B�B�BBB�B�B�B�B�B�B�B�B�BB�BSB�B�B�B�BB$BYB�BYB$B$B�B_B�B1B�B1B�B7B�B�BkB�B	B�B�B�BqB�BCBB�BxB�B�B�B�BB�B�B�B~B~B~BIB�B~BBB�B�BVB \B�B \B!-B �B �B!�B!�B"hB"�B"�B"�B#:B#B#:B#B#B#:B#B#B"�B"�B$@B%�B%�B%�B%�B%�B%�B&LB&B&LB&�B&�B&�B'RB($B'�B(XB(�B)�B)�B)�B*0B)�B*0B*eB*�B+kB+B+kB+kB+kB+kB+6B+B*�B,B-B-�B.}B.�B.�B.�B.�B.}B.�B.}B.�B.�B/�B0UB0�B0�B1[B1'B1�B1�B1[B1�B1�B1�B1�B1�B1�B3hB3�B3�B3�B3hB3�B3�B3�B3�B3�B3�B3�B3�B3�B4B4B4B4nB4nB4nB4�B4nB5B5?B5?B6zB7�B7�B7�B7�B7�B7�B7�B7�B7�B8B8B8�B9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B:*B:�B:�B:�B:�B;0B;0B;dB;dB;�B<B<B<B<6B<6B<B<B<6B<jB=�B=�B=�B=�B=�B>wB>wB?�B@B@�B@�B@�BA BA�BA�BA�BA�BB'BB[BB'BB[BB'BB�BB�BC-BC�BC�BD3BC�BC�BD�BFBE�BF?BF?BFtBFtBF�BGEBGzBGzBGzBGzBGzBGEBG�BG�BHBHBH�BH�BH�BH�BIBIBIRBIRBI�BI�BI�BJ#BJXBJXBJ�BJ�BK)BK)BK)BK�BK�BK�BK�BK�B�<B��B�B�<B�<B�B͟B�jB�B�B�<B��B��B��B��B��B�<B�jB�<B��B�<B�6B�B�6B�B��B�B��B�B�B�BB�jB�pB�jB�B�jB��B͟BΥB�pB��B�<B�B�jBΥB�pB�BΥB�6BϫB�6B�BB��BϫB�jB�BB�6B�B��B�jB�pB��B�B͟BΥB�BΥB��B�BB�6B�BB�jB�BB�6BϫB�B�BB�6B��B�B�B�6B�B̘B�B�B��B�B�B��B˒B˒B�0B�pBϫBϫBʌB��B��B��B͟B�0B��B̘B��B��BǮB��BɆBȴB��BʌBɆB��B�RB��B�RB�#B��B�B�#B�KBʌBȀB�XB�KBɺBȴBɆB�RBȴB�#BȴB��B�B�XB�EB�RBǮB��B��B�KB�zBƨB�B��B�B�tB�zBƨB��B�B��B�B�B��B��BŢB�B�?B�KB��B�zB�zB��B�B�tB�KB��BȴB�B��B�mB��BƨB�B��B�?B��B��B�zBŢB�EB��BƨBƨB��B�B�9B�zB�9B�zB�B�B��BƨB��B�B�EB�mB�zB�B��B��B�<B�B�B��BB��B�B��B�'B��B�-B��B�gB�EB�B��BʌB��B��BɺB��BɺB��BɺBɆB��B�B��B�#B�pB�pB�B�BбB�&B��B�EB�?B�B��B�B�BߤB��BߤB�&B�fB��B�iB�B�B�`B��B�]B~B�BbB@B@B�B�BkB�B7BqBB�B�B!B \B �B"�B(�B%�B$�B#nB$�B+�B2-B0�B-�B/B+B-B+�B.�BB�BF?BG�BOBBO�BQ�BAUBG�BFBK)BI�BI�BK^BGBI�BE�BGBFBL�BG�BF�BS[BM�BK�BS�BH�BH�BJXBM6BJ�BH�BI�BGEBGzBHBLdBF?BC�BAUBA�BA B=�BB�BWsBk�BMjBD�BN�BR�B8�B;dB7LB<�B:�BMB49B3hB&�B2�BQNB0UB0!B5�B5�B4�B.IB1�B0UB6�B-�B2-B)�B+6B&�B)�B'�B&�B'B"�B&�B%�B(�B&B%�B$@B$tB"4B#�B!�B!�B$@B�B!�B�B 'B�B�B�BCB�B�B�BB B�B1�BJXB�(B�fB�B �B�fB�B�B�|B�MB�vB�B�B��B�;B�5B��B�B�iB��B�B�mB�B�B�B��B��B�BچBߤB��B�B��B�}B֡B�?B�HB�EB�?B�0B��B��B��B�HB�OB�UB�OB�B�[B�OBÖB��B��B�UB��B��B��B��B��B�9B�3B�HB��B��B�wB��B��B�XB�dB��B�dB��B��B�3B�<B��B�$B�B��B��B��B��B�@B��B�-B�$B��B��B��B�eB�MB�PB��B��B� B�{B�%B��B.B{�BuZBu�Bx8BqBjBlWBjKBi�BffBZQBZBUgBYBaHBQ�BO�BN�BP�BVmBR�BLdBPHB^BI�BJ�BH�BM�BH�BK�BVBN�BK�BM�BJ�BE�BHKBE�BG�BF�BC�BI�BFBC-BO�BH�BA�BA B?}B=�B?B>BB>�B:�B;0B9XB:�B<6B9�B9�B=qB0�B/�B4nB-�B,qB'�B"4B($B-�B'�B�B"4B�BSB�B$B�B�BB�BMB�B�B�B B\B�B�B�BB	lB�B�B��B�BMB�B�]B��B��B��B�B�B�%B��B�B��B�B�B�B��B�B�B�B�B�2B��B�GB�2B��B��B�	B��B��B��B�2B��B��B�|B�B�DB��B�B�
B�B�`B�B�`B�`B�`B�fB�B�ZB��B��B�B�TB�QBѷB�B�vB�B�gB�[B��BרB��BںB�<B҉B��B�B��BуB�B�}B�<B�B��B�HBϫB�B��B�pB��B�BΥB��B��B��B�jB��B�^B�XB�^B��B�?B��B��B��B�B��B�kB��B�*B��B�B��B�B�xB�!B�OB�}B��B�B��B�B�BzxB}"B~�B|PB{JB|�B~�B|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2022011316302220220113163022IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022012315011920220123150119QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022012315011920220123150119QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225920220906072259IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                