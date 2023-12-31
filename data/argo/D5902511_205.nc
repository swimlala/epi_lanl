CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-03-17T21:16:53Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�        =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�        c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�        ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȉ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�          PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 5   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       <�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       c�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20220317211653  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_205                 6810_008521_205                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��tmq�b@��tmq�b11  @��t�쿱@��t�쿱@0vr2L�@0vr2L��d�����d����11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @}p�@�p�@�  @�  @��RA  A ��A,(�A@  A`  A�Q�A���A���A�  A�Q�A�Q�A�Q�A��B (�B(�BQ�B(�B   B((�B0(�B8  B?�
BG�
BO�BW�
B`Q�Bh  Bo�
Bx(�B�  B��B�{B�{B�  B�  B�  B��B�  B�{B�  B�  B��B��B�  B�{B�  B�  B�{B�  B�  B��B��B�  B�  B��B��B�  B��B��B�  B�{C 
=C  C
=C  C�C	��C  C
=C
=C  C  C  C��C
=C
=C{C 
=C!��C#��C%��C(  C*  C+��C-��C0  C1��C4  C6
=C8
=C:  C<
=C=��C@  CB  CD  CF{CH
=CI��CL  CM��CO��CR  CS��CV  CX  CZ  C[��C]��C_�Ca��Cd
=Cf  Ch  Cj  Cl
=Cn  Cp
=Cr
=Ct  Cu��Cx  Cz
=C|{C~  C�  C�  C�  C�C���C���C�C�  C�C���C�  C�  C���C�  C�C�  C�  C���C�  C���C���C�C���C���C���C���C�  C���C�  C�C�C�C���C���C�C�  C�  C�C���C���C�C�
=C�
=C�
=C���C���C���C�  C�  C�C�  C�  C���C��C���C�  C�C�  C�  C���C�  C�C�C���C���C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C�C�  C��C���C�  C�  C�C�  C�C�C�C�C�C�  C�C�
=C�  C���C�C�  C���C���C���C�  C�C�C�  C���C�  C�C�
=C���C���C�  C���C���C�  C�C�
=C�C�  C�  C�  C�  C���C���C�  C�  C�  C�C�D D �DD��D�D��D�D� D  D}qD  D}qD�qD}qD�qDz�D��D}qD�qD	}qD	�qD
� D
�qD}qD  D� D�D}qD�qD}qD  D��D  D}qD  Dz�D��D}qD�qD��DD��DD�DD��D�D��D�D�DD��D�D� D�Dz�D�qD� D  D��D�D� D  D� D �D � D ��D!z�D"  D"�D#�D#}qD#��D$z�D$�qD%��D&  D&� D'D'��D'�qD(}qD)  D)� D*  D*��D+�D+� D,  D,�D-D-��D.�D.��D.�qD/� D0D0� D1  D1��D1�qD2}qD2�qD3}qD4D4��D5�D5� D6�D6�D7�D7z�D7��D8}qD8�qD9}qD9�qD:��D;  D;}qD<�D<��D<�qD=� D>�D>�D?�D?}qD?��D@��DADA}qDA�qDB��DC  DC� DC�qDD}qDE�DE��DF�DF��DG�DG}qDG�qDH��DIDI��DJ  DJ��DKDK� DL  DL�DMDM� DN  DN��DO  DO� DP�DP��DQDQ�DR�DR� DR��DS� DT�DT��DT�qDU� DV  DV}qDW�DW��DX�DX��DYDY��DZ  DZ� D[  D[}qD[��D\� D]�D]��D^�D^�D_  D_xRD_�qD`�Da�Da��Db  Db� Db�qDc}qDc�qDd� De  De� De��DfxRDf�qDg� Dg��Dh}qDh�qDi}qDi�qDj}qDk  Dk� Dl�Dl�DmDm�Dn�Dn}qDn�qDo� Dp  Dp��DqDq� Dr  Dr� Dr�qDs� Ds�qDt� Du�Du��Dv�Dv� Dw  Dw��Dw�qDx� Dy�Dy��Dz�Dz}qDz�qD{}qD{�qD|}qD|��D}� D~  D~z�D~�qD��D��D�B�D��HD�D��D�@ D�� D��HD�HD�@ D�~�D�� D���D�>�D�� D��HD�HD�>�D�~�D�� D�HD�@ D�� D��HD�HD�B�D��HD�� D�  D�@ D��HD��HD���D�>�D��HD�� D��qD�>�D�� D�� D�HD�>�D�~�D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D���D�@ D���D�D�HD�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D���D���D�B�D�� D���D�  D�@ D�~�D���D���D�AHD��HD��HD�HD�@ D�� D���D���D�AHD�� D�� D�  D�@ D��HD���D�  D�AHD�� D���D���D�@ D�~�D�� D�  D�AHD�� D��HD�HD�@ D�� D��HD�  D�@ D��HD�� D�  D�AHD�� D���D�HD�AHD�~�D��qD���D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D���D�HD�AHD��HD���D�  D�@ D�� D�� D�  D�@ D�~�D���D�  D�@ D�� D��HD��D�AHD��HD��HD���D�@ D��HD��HD�  D�@ D�~�D���D��qD�>�D�� D���D��qD�AHD��HD��HD��D�>�D�~�D��HD�  D�>�D��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�AHD���D�D�  D�@ D��HD�� D���D�>�D�~�D���D��qD�>�D�� D��qD���D�@ D�� D���D�  D�AHD�� D���D���D�AHD�� D���D�  D�@ D��HD��HD�  D�>�D�� D��HD�HD�AHD�� D��HD�HD�B�D�� D��qD���D�@ D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D�}qD���D�  D�AHD���D��HD�  D�@ D��HD�� D�  D�@ D�~�D���D�HD�AHD�� D���D�HD�@ D D��HD�  D�>�DÀ D�� D���D�=qD�}qDľ�D���D�@ Dŀ D�� D���D�>�Dƀ D�D��D�@ D�}qDǾ�D���D�>�DȀ D��HD�HD�>�DɁHD��HD���D�@ D�~�D�� D���D�=qDˀ D�� D�  D�AHD́HD�� D���D�>�D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�~�D�� D�  D�AHDр D�� D�  D�@ DҀ DҾ�D���D�=qD�~�D�� D���D�=qD�}qDԾ�D�  D�>�D�~�D�� D�  D�B�Dւ�D��HD�  D�@ D׀ D��HD�HD�>�D؀ D��HD���D�=qD�~�D�� D���D�AHDځHD�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�AHD݁HD�� D�  D�AHDހ D�� D�HD�@ D߀ D��HD�HD�>�D�� D��HD��D�AHD�~�D�� D�HD�AHD� D⾸D���D�>�D�~�D㾸D���D�>�D� D�� D�  D�AHD傏D��HD�  D�AHD� D�� D���D�@ D� D羸D�  D�AHD�HD辸D��qD�@ D邏D��HD���D�@ D� D�� D��D�AHD�HD��HD�HD�AHD�HD�D�  D�AHD�~�D�� D�HD�>�D�~�D�qD��qD�AHD�HD�� D�HD�B�D�~�D�D�  D�@ D�~�D�D�HD�B�D� D�� D�HD�@ D�}qD�D�HD�B�D�HD���D�HD�AHD�� D���D���D�>�D�e?�?8Q�?�=q?���?Ǯ?�@\)@#�
@333@G�@^�R@p��@�  @���@�33@�p�@��
@��@�@��R@��@�{@�Q�@�  @���@��@�(�AG�AffA�A{A33A
=A�A!�A%�A(��A/\)A4z�A8Q�A?\)ADz�AHQ�AN�RAS�
AW�A]p�Ac33Ag
=Al(�Ar�\Aw
=Az�HA�Q�A��A�p�A�\)A��\A��A�
=A��A�z�A�ffA���A�(�A�{A�  A�33A�A�\)A��A���A��RA�G�A�(�A�{A�Q�A�33A��A�\)A\A���A�ffAə�A�(�A�{A���A�(�A�A�Q�A�33A�A�\)A�\A��A�
=A陚A�(�A�A�Q�A�A��A�\)A��\A���A��RB ��B{B
=B��BB�HB(�B	��B
ffB�B�BffB\)B��BffB\)B��B{B�Bz�B��B33B��B��B�RB z�B!��B"�RB$Q�B%��B&�RB'�
B)p�B*�HB+�B-�B.�HB/�
B0��B2ffB4  B4��B6=qB7�
B9�B:{B;�B=�B>{B?\)BA�BB{BC33BD��BFffBG�BH��BJ=qBK�
BL��BNffBO�
BP��BR=qBT  BU�BVffBX  BYG�BZffB[�
B]p�B^�\B_�Bap�Bb�RBc�
Be�Bf�HBh  Bi�Bj�\Bl(�Bmp�BnffBo�
BqG�Br�\Bs�BuG�Bv�\Bw�Byp�Bz�RB{�B}G�B~�HB�  B��\B�\)B�(�B��RB�\)B�{B��HB��B�{B��HB��B�Q�B��HB���B�ffB��B��B�=qB�
=B��
B�Q�B�
=B��
B���B��B�B���B�\)B��
B���B�p�B�{B���B�p�B�Q�B��HB�p�B�(�B���B��B�(�B���B�B�Q�B��HB���B�ffB���B��B�ffB��B���B�Q�B��B��
B�Q�B��B��B�z�B��B��
B���B�p�B�  B��\B�\)B�=qB��HB��B�(�B���B�B�Q�B���B��
B��\B�33B��B��\B�33B�B�ffB�G�B�  B�z�B��B�  B���B�33B�B���B�\)B��B��\B�p�B�(�B¸RBÅB�=qB���B�p�B�Q�B��BǅB�Q�B�33B��
B�Q�B��B��Ḅ�B�33B��
BθRBυB�  BУ�B�p�B�(�BҸRB�G�B�  B���Bՙ�B�(�B֣�B�\)B�(�B���BمB�  Bڣ�B�\)B�(�B���B�33B��B޸RB�p�B�  B��B�B�{B��B�\)B�(�B���B�\)B�{B���B�p�B��B��B�p�B�(�B�\B�G�B�{B��B�33B��
B�RB�\)B��
B�z�B�G�B�  B�\B��B�  B���B��B��
B��\B�G�B�B�ffB�33B��
B��\B�
=B��B�ffB�33B��B�=qB���B��C {C ffC ��C(�Cz�C�RC  CffCC{C\)C��C{CffC�C��CG�C�RC  CG�C��C
=CQ�C�\C�CQ�C�\C�
C	33C	��C	�C
33C
z�C
�
C33Cz�CC�C�C�
C{Cp�C�
C�CffC��C33Cp�C�RC{Cz�CC
=Cp�C�
C
=C\)C��C�CffCC(�Cp�C�RC{Cz�C��C
=CQ�C�C{CQ�C�\C�C=qC�C�C  CQ�C�\CC��C=qCz�C��C�
C�CQ�Cp�C��C�HC  C�C\)C��C�RC�HC{CQ�Cz�C��C�HC
=C33CQ�C��C��C�C{CQ�C�C�C��C 
=C =qC ffC �C ��C!  C!{C!G�C!�\C!�C!��C"{C"Q�C"ffC"�\C"C#
=C#=qC#\)C#�C#��C#�C$�C$G�C$ffC$��C$�
C%
=C%(�C%G�C%z�C%�RC%�C&  C&33C&p�C&��C&�RC&�HC'(�C'\)C'z�C'��C'��C(  C(33C(Q�C(p�C(�C(�HC)
=C)(�C)Q�C)�C)C)�
C)��C*33C*ffC*��C*�RC*�
C+  C+G�C+ffC+�C+�C+��C,�C,G�C,ffC,�\C,��C-  C-{C-=qC-�C-�RC-�HC.  C.=qC.z�C.��C.C.��C/=qC/ffC/�\C/�RC/�C0(�C0\)C0�C0�C0�C1(�C1\)C1z�C1��C1�HC2(�C2G�C2p�C2�C2�C3�C3=qC3p�C3�C3�C4{C433C4z�C4�RC4�HC5
=C5G�C5�\C5�RC5�C6�C6ffC6��C6C6�C7=qC7z�C7�C7��C8
=C8Q�C8�\C8�RC8�HC9�C9p�C9��C9��C:  C:Q�C:��C:C;  C;Q�C;�\C;C;��C<=qC<�\C<�
C=  C==qC=�C=�
C>{C>G�C>z�C>��C?�C?Q�C?�C?��C@{C@ffC@��C@CA
=CA\)CA��CA��CB  CBQ�CB��CB�
CC  CC=qCCz�CC��CD{CD=qCDp�CD�CD��CE=qCEz�CE�CE�
CF{CF\)CF��CFCF��CG(�CGp�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                   ?�  @   @@  @}p�@�p�@�  @�  @��RA  A ��A,(�A@  A`  A�Q�A���A���A�  A�Q�A�Q�A�Q�A��B (�B(�BQ�B(�B   B((�B0(�B8  B?�
BG�
BO�BW�
B`Q�Bh  Bo�
Bx(�B�  B��B�{B�{B�  B�  B�  B��B�  B�{B�  B�  B��B��B�  B�{B�  B�  B�{B�  B�  B��B��B�  B�  B��B��B�  B��B��B�  B�{C 
=C  C
=C  C�C	��C  C
=C
=C  C  C  C��C
=C
=C{C 
=C!��C#��C%��C(  C*  C+��C-��C0  C1��C4  C6
=C8
=C:  C<
=C=��C@  CB  CD  CF{CH
=CI��CL  CM��CO��CR  CS��CV  CX  CZ  C[��C]��C_�Ca��Cd
=Cf  Ch  Cj  Cl
=Cn  Cp
=Cr
=Ct  Cu��Cx  Cz
=C|{C~  C�  C�  C�  C�C���C���C�C�  C�C���C�  C�  C���C�  C�C�  C�  C���C�  C���C���C�C���C���C���C���C�  C���C�  C�C�C�C���C���C�C�  C�  C�C���C���C�C�
=C�
=C�
=C���C���C���C�  C�  C�C�  C�  C���C��C���C�  C�C�  C�  C���C�  C�C�C���C���C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C�C�  C��C���C�  C�  C�C�  C�C�C�C�C�C�  C�C�
=C�  C���C�C�  C���C���C���C�  C�C�C�  C���C�  C�C�
=C���C���C�  C���C���C�  C�C�
=C�C�  C�  C�  C�  C���C���C�  C�  C�  C�C�D D �DD��D�D��D�D� D  D}qD  D}qD�qD}qD�qDz�D��D}qD�qD	}qD	�qD
� D
�qD}qD  D� D�D}qD�qD}qD  D��D  D}qD  Dz�D��D}qD�qD��DD��DD�DD��D�D��D�D�DD��D�D� D�Dz�D�qD� D  D��D�D� D  D� D �D � D ��D!z�D"  D"�D#�D#}qD#��D$z�D$�qD%��D&  D&� D'D'��D'�qD(}qD)  D)� D*  D*��D+�D+� D,  D,�D-D-��D.�D.��D.�qD/� D0D0� D1  D1��D1�qD2}qD2�qD3}qD4D4��D5�D5� D6�D6�D7�D7z�D7��D8}qD8�qD9}qD9�qD:��D;  D;}qD<�D<��D<�qD=� D>�D>�D?�D?}qD?��D@��DADA}qDA�qDB��DC  DC� DC�qDD}qDE�DE��DF�DF��DG�DG}qDG�qDH��DIDI��DJ  DJ��DKDK� DL  DL�DMDM� DN  DN��DO  DO� DP�DP��DQDQ�DR�DR� DR��DS� DT�DT��DT�qDU� DV  DV}qDW�DW��DX�DX��DYDY��DZ  DZ� D[  D[}qD[��D\� D]�D]��D^�D^�D_  D_xRD_�qD`�Da�Da��Db  Db� Db�qDc}qDc�qDd� De  De� De��DfxRDf�qDg� Dg��Dh}qDh�qDi}qDi�qDj}qDk  Dk� Dl�Dl�DmDm�Dn�Dn}qDn�qDo� Dp  Dp��DqDq� Dr  Dr� Dr�qDs� Ds�qDt� Du�Du��Dv�Dv� Dw  Dw��Dw�qDx� Dy�Dy��Dz�Dz}qDz�qD{}qD{�qD|}qD|��D}� D~  D~z�D~�qD��D��D�B�D��HD�D��D�@ D�� D��HD�HD�@ D�~�D�� D���D�>�D�� D��HD�HD�>�D�~�D�� D�HD�@ D�� D��HD�HD�B�D��HD�� D�  D�@ D��HD��HD���D�>�D��HD�� D��qD�>�D�� D�� D�HD�>�D�~�D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D���D�@ D���D�D�HD�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D���D���D�B�D�� D���D�  D�@ D�~�D���D���D�AHD��HD��HD�HD�@ D�� D���D���D�AHD�� D�� D�  D�@ D��HD���D�  D�AHD�� D���D���D�@ D�~�D�� D�  D�AHD�� D��HD�HD�@ D�� D��HD�  D�@ D��HD�� D�  D�AHD�� D���D�HD�AHD�~�D��qD���D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D���D�HD�AHD��HD���D�  D�@ D�� D�� D�  D�@ D�~�D���D�  D�@ D�� D��HD��D�AHD��HD��HD���D�@ D��HD��HD�  D�@ D�~�D���D��qD�>�D�� D���D��qD�AHD��HD��HD��D�>�D�~�D��HD�  D�>�D��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�AHD���D�D�  D�@ D��HD�� D���D�>�D�~�D���D��qD�>�D�� D��qD���D�@ D�� D���D�  D�AHD�� D���D���D�AHD�� D���D�  D�@ D��HD��HD�  D�>�D�� D��HD�HD�AHD�� D��HD�HD�B�D�� D��qD���D�@ D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D�}qD���D�  D�AHD���D��HD�  D�@ D��HD�� D�  D�@ D�~�D���D�HD�AHD�� D���D�HD�@ D D��HD�  D�>�DÀ D�� D���D�=qD�}qDľ�D���D�@ Dŀ D�� D���D�>�Dƀ D�D��D�@ D�}qDǾ�D���D�>�DȀ D��HD�HD�>�DɁHD��HD���D�@ D�~�D�� D���D�=qDˀ D�� D�  D�AHD́HD�� D���D�>�D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�~�D�� D�  D�AHDр D�� D�  D�@ DҀ DҾ�D���D�=qD�~�D�� D���D�=qD�}qDԾ�D�  D�>�D�~�D�� D�  D�B�Dւ�D��HD�  D�@ D׀ D��HD�HD�>�D؀ D��HD���D�=qD�~�D�� D���D�AHDځHD�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�AHD݁HD�� D�  D�AHDހ D�� D�HD�@ D߀ D��HD�HD�>�D�� D��HD��D�AHD�~�D�� D�HD�AHD� D⾸D���D�>�D�~�D㾸D���D�>�D� D�� D�  D�AHD傏D��HD�  D�AHD� D�� D���D�@ D� D羸D�  D�AHD�HD辸D��qD�@ D邏D��HD���D�@ D� D�� D��D�AHD�HD��HD�HD�AHD�HD�D�  D�AHD�~�D�� D�HD�>�D�~�D�qD��qD�AHD�HD�� D�HD�B�D�~�D�D�  D�@ D�~�D�D�HD�B�D� D�� D�HD�@ D�}qD�D�HD�B�D�HD���D�HD�AHD�� D���D���D�>�G�O�?�?8Q�?�=q?���?Ǯ?�@\)@#�
@333@G�@^�R@p��@�  @���@�33@�p�@��
@��@�@��R@��@�{@�Q�@�  @���@��@�(�AG�AffA�A{A33A
=A�A!�A%�A(��A/\)A4z�A8Q�A?\)ADz�AHQ�AN�RAS�
AW�A]p�Ac33Ag
=Al(�Ar�\Aw
=Az�HA�Q�A��A�p�A�\)A��\A��A�
=A��A�z�A�ffA���A�(�A�{A�  A�33A�A�\)A��A���A��RA�G�A�(�A�{A�Q�A�33A��A�\)A\A���A�ffAə�A�(�A�{A���A�(�A�A�Q�A�33A�A�\)A�\A��A�
=A陚A�(�A�A�Q�A�A��A�\)A��\A���A��RB ��B{B
=B��BB�HB(�B	��B
ffB�B�BffB\)B��BffB\)B��B{B�Bz�B��B33B��B��B�RB z�B!��B"�RB$Q�B%��B&�RB'�
B)p�B*�HB+�B-�B.�HB/�
B0��B2ffB4  B4��B6=qB7�
B9�B:{B;�B=�B>{B?\)BA�BB{BC33BD��BFffBG�BH��BJ=qBK�
BL��BNffBO�
BP��BR=qBT  BU�BVffBX  BYG�BZffB[�
B]p�B^�\B_�Bap�Bb�RBc�
Be�Bf�HBh  Bi�Bj�\Bl(�Bmp�BnffBo�
BqG�Br�\Bs�BuG�Bv�\Bw�Byp�Bz�RB{�B}G�B~�HB�  B��\B�\)B�(�B��RB�\)B�{B��HB��B�{B��HB��B�Q�B��HB���B�ffB��B��B�=qB�
=B��
B�Q�B�
=B��
B���B��B�B���B�\)B��
B���B�p�B�{B���B�p�B�Q�B��HB�p�B�(�B���B��B�(�B���B�B�Q�B��HB���B�ffB���B��B�ffB��B���B�Q�B��B��
B�Q�B��B��B�z�B��B��
B���B�p�B�  B��\B�\)B�=qB��HB��B�(�B���B�B�Q�B���B��
B��\B�33B��B��\B�33B�B�ffB�G�B�  B�z�B��B�  B���B�33B�B���B�\)B��B��\B�p�B�(�B¸RBÅB�=qB���B�p�B�Q�B��BǅB�Q�B�33B��
B�Q�B��B��Ḅ�B�33B��
BθRBυB�  BУ�B�p�B�(�BҸRB�G�B�  B���Bՙ�B�(�B֣�B�\)B�(�B���BمB�  Bڣ�B�\)B�(�B���B�33B��B޸RB�p�B�  B��B�B�{B��B�\)B�(�B���B�\)B�{B���B�p�B��B��B�p�B�(�B�\B�G�B�{B��B�33B��
B�RB�\)B��
B�z�B�G�B�  B�\B��B�  B���B��B��
B��\B�G�B�B�ffB�33B��
B��\B�
=B��B�ffB�33B��B�=qB���B��C {C ffC ��C(�Cz�C�RC  CffCC{C\)C��C{CffC�C��CG�C�RC  CG�C��C
=CQ�C�\C�CQ�C�\C�
C	33C	��C	�C
33C
z�C
�
C33Cz�CC�C�C�
C{Cp�C�
C�CffC��C33Cp�C�RC{Cz�CC
=Cp�C�
C
=C\)C��C�CffCC(�Cp�C�RC{Cz�C��C
=CQ�C�C{CQ�C�\C�C=qC�C�C  CQ�C�\CC��C=qCz�C��C�
C�CQ�Cp�C��C�HC  C�C\)C��C�RC�HC{CQ�Cz�C��C�HC
=C33CQ�C��C��C�C{CQ�C�C�C��C 
=C =qC ffC �C ��C!  C!{C!G�C!�\C!�C!��C"{C"Q�C"ffC"�\C"C#
=C#=qC#\)C#�C#��C#�C$�C$G�C$ffC$��C$�
C%
=C%(�C%G�C%z�C%�RC%�C&  C&33C&p�C&��C&�RC&�HC'(�C'\)C'z�C'��C'��C(  C(33C(Q�C(p�C(�C(�HC)
=C)(�C)Q�C)�C)C)�
C)��C*33C*ffC*��C*�RC*�
C+  C+G�C+ffC+�C+�C+��C,�C,G�C,ffC,�\C,��C-  C-{C-=qC-�C-�RC-�HC.  C.=qC.z�C.��C.C.��C/=qC/ffC/�\C/�RC/�C0(�C0\)C0�C0�C0�C1(�C1\)C1z�C1��C1�HC2(�C2G�C2p�C2�C2�C3�C3=qC3p�C3�C3�C4{C433C4z�C4�RC4�HC5
=C5G�C5�\C5�RC5�C6�C6ffC6��C6C6�C7=qC7z�C7�C7��C8
=C8Q�C8�\C8�RC8�HC9�C9p�C9��C9��C:  C:Q�C:��C:C;  C;Q�C;�\C;C;��C<=qC<�\C<�
C=  C==qC=�C=�
C>{C>G�C>z�C>��C?�C?Q�C?�C?��C@{C@ffC@��C@CA
=CA\)CA��CA��CB  CBQ�CB��CB�
CC  CC=qCCz�CC��CD{CD=qCDp�CD�CD��CE=qCEz�CE�CE�
CF{CF\)CF��CFCF��CG(�CGp�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                   @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ƨA�ƨA�ĜA�A�A�ĜA�ƨA�ƨA�ȴA�ĜA���A�ĜA̾wA���A�ȴA�ĜA���A���A̼jA̶FA̲-ȂhA̍PȀ\ÁA�jA�^5A�^5A�\)A�\)A�XA�XA�XA�XA�XA�XA�VA�S�A�S�A�Q�A�Q�A�M�A�K�A�I�A�I�A�E�A�E�A�E�A�C�A�C�A�A�A�A�A�A�A�C�A�A�A�A�A�?}A�?}A�?}A�A�A�A�A�=qA�;dA�9XA�33A�-A�&�A��A�VA���A��A��TA���A���A˲-A˩�A˝�Aˇ+A�r�A�l�A�hsA�^5A�S�A�C�A�(�A���A�A��HA���A�oA�K�A¡�A�A�M�A���A�C�A��FA���A��^A�VA�  A��^A���A�=qA�1'A�\)A���A��+A��jA���A��DA�%A���A��uA�A�l�A�`BA�n�A�%A�bA��mA�ȴA���A��DA��`A�t�A�S�A���A�%A�(�A�#Aw�^As��Aq��Ai�A\�AX9XAU��ATr�ASXAQO�AJ�ADĜAA+A>��A=S�A<JA:�\A7;dA5C�A2~�A.M�A+��A*�!A)�A%�^A${A$  A#�mA#��A#��A#�PA#dZA#S�A"~�A �`A �A I�A�FA�A%A��A 5?A �DA -AA�A�-AA�A��A��A��A�hA�A7LA�A�uA��A��A�`AbNAVAbNA%AAA
E�A	t�A	�A	��A
�uA�mA�!AK�A��AdZA�jAn�AbAAt�A&�A
�jA
E�A�RA�#AS�A�uA1'A��A�A\)A"�A�
A33A&�A%A�jA�!A^5A��AK�A�A �D@���@���@�~�@�E�@���@��@��;@�|�@�C�@�@���@�=q@�@��`@��\@�?}@���@�bN@� �@�@�~�@��@�/@�Q�@��@�J@홚@��m@�ff@�^@�7@��@�bN@���@睲@畁@�P@�-@�/@�Z@�@�C�@�!@ᙚ@��`@�I�@�l�@�5?@��@�x�@܃@���@ۍP@�dZ@��@�n�@�@�&�@ش9@��m@�l�@�M�@�7L@�1@�o@Ұ!@���@��@Ѓ@�1'@��m@Ͼw@��@��@̬@˥�@�dZ@�C�@�o@ʸR@��T@ɩ�@ɑh@�G�@� �@Ǯ@�33@���@��@ř�@�7L@��@ēu@�  @å�@�t�@�o@�@�M�@�$�@�{@���@�?}@���@�r�@�I�@�+@���@�M�@�5?@�$�@��@�{@��@�-@�=q@�=q@�{@��-@���@��m@�+@�$�@��T@���@�/@���@���@�ƨ@�@��@���@���@�@���@�&�@���@�Ĝ@�9X@���@��w@��@�~�@�ff@��@�G�@�V@��`@��@�1@�\)@�o@��@��!@�ff@�-@���@��7@�Ĝ@�Z@�ƨ@�;d@���@���@��!@�~�@�E�@�@���@�G�@��@�%@���@�Ĝ@��@�j@�Q�@� �@��@��@�@�ȴ@��@���@�O�@�Ĝ@���@�r�@�1'@��
@�\)@�+@�@��\@�-@��@�@��#@��@�G�@�/@��@��@�(�@�  @�t�@�S�@�"�@���@��+@�=q@���@���@��h@�x�@��@�j@�I�@�A�@�(�@��m@��@�;d@��+@�$�@�J@�{@��@�X@��@�9X@��@��@���@�C�@�o@���@�v�@�J@���@�x�@�G�@�%@�z�@� �@��
@��F@�\)@�o@��!@�~�@�^5@�E�@�-@��@��-@�/@��`@��@��@�Z@�1'@���@���@�dZ@�"�@�ȴ@��\@�ff@�-@�@��^@�hs@��@��/@��9@�r�@�(�@�1@��m@��w@��P@�o@���@���@�ff@�-@�@��@���@�/@��@�Ĝ@�z�@��@���@��@�ff@�-@��@�{@��@���@�X@�G�@��@���@��@���@�K�@�|�@�dZ@���@��w@�ƨ@��F@��P@�\)@�$�@�p�@�p�@�G�@�Ĝ@��@�r�@�A�@�(�@�b@�P@K�@~��@~�+@
=@��@�;@�@K�@
=@~��@~�y@~��@~E�@}�@}�-@}�@}V@|�@|9X@{�F@{C�@{o@z�\@z�@y�#@y�#@y�^@yG�@xb@w�@v��@v$�@u��@up�@u?}@t��@t��@t�@t�@t�j@t(�@s��@r�@r~�@q��@q��@q�7@p��@p��@p��@o\)@n�R@m��@m/@l�j@k�m@kt�@j�@j-@i��@ix�@i7L@h�@hA�@g�@g�@g+@f�@f��@fE�@f@e��@d9X@d1@cS�@c@b�@b�H@b��@b�!@b-@aX@`Ĝ@`bN@`A�@`b@_�@_��@_�w@_��@_K�@^��@]��@]/@\�@\�j@\�D@\Z@[�F@[C�@Z�H@Z��@Z-@Yhs@X1'@W��@W|�@W�@Vv�@V{@U�T@U�h@U�@T��@T�@SC�@R��@R�\@R~�@RM�@R-@RJ@Q�7@QG�@Q%@P�`@P��@Pr�@PA�@O��@O;d@O+@O�@Nv�@M�T@MO�@L��@L��@Lz�@K�
@K�@Kt�@Ko@K@J�!@I�#@IX@H��@Hr�@H1'@H  @G�@G|�@G
=@F��@F$�@F{@F@E�h@D��@D�D@DZ@D�@C�@CS�@B�@B^5@A��@A%@@A�@@  @?�w@?l�@>v�@=��@=�h@=O�@=/@<Z@;��@;S�@:�H@:�!@:~�@:^5@:J@9��@9��@9�^@9�^@9�^@9G�@8��@8�9@8b@7�w@7|�@7K�@7�@7
=@6��@6�@6v�@5�@5@5`B@4��@4I�@49X@3��@3��@3"�@1��@17L@1�@0�`@0�u@0r�@0A�@/�w@/|�@/+@.�y@.�R@.ff@.$�@.$�@.@-p�@,�/@,�@,��@,�D@,z�@,Z@,1@+�F@+��@+dZ@+o@*�\@*-@)�^@)X@)%@(��@(Ĝ@(�@(Q�@( �@'�;@'�P@'|�@'+@&�y@&��@&v�@&V@&$�@&$�@&@%�-@%`B@$��@$�@$z�@$I�@$9X@$�@$�@#��@#�m@#ƨ@#��@#33@#@"�H@"��@"~�@"=q@!��@!7L@ ��@ bN@   @��@�@�+@V@E�@$�@�T@��@O�@/@�@��@�@Z@9X@(�@1@��@"�@��@��@��@�\@=q@��@��@�7@X@�@��@�9@r�@Q�@1'@b@�@��@�@l�@\)@+@
=@�y@�@�R@��@�+@V@E�@�@�-@`B@�/@�@z�@Z@9X@1@�m@ƨ@��@dZ@33@o@�H@��@��@�\@n�@^5@M�@=q@-@-@J@��@��@��@��@�@�@�^@��@G�@�9@�@bN@Q�@1'@ �@  @�@�@�;@�w@�@�P@K�@�@
=@�y@�@ȴ@��@�+@v�@5?@@�T@��@��@@�-@�-@�-@��@�h@�h@`B@O�@?}@/@�@V@V@V@��@��@�@�/@�j@��@I�@I�@I�@I�@I�@9X@(�@��@ƨ@��@�A�ĜA���A���A���A���A���A�ƨA���A�ĜA�ĜA�ȴA�ƨA�ĜA�ƨA�ĜA�ȴA�ĜA�A�A�ĜA���A�A�ĜA���A�ĜA�ȴA�ĜA���A�A�ȴA�ĜA�ĜA�ƨA�ĜA���A�ȴA�ƨA���A���A�ƨA�A���A̼jA���A�A̾wA���A�A���A���A�ĜA�A�A�ƨA�ȴA�ĜA�A�ĜA̾wA̾wA̺^A̼jA̺^A̸RA̾wA̾wA���A�A�ȴA�ĜA�ƨA�ȴA�ȴA�ȴA���A�ȴA�ƨA���A�ƨA�A���A̾wA�ƨA���A���A�ƨA���A���A�ƨA�ȴA���A���A�ƨA���A���A�ȴA���A���A�ĜA̶FA̰!A̩�A̶FA̴9A̧�A̰!A�A���A̸RA̼jA̴9A̺^A̺^A̲-A̰!A̝�A̙�A̗�Ȁ\Ȁ\A̓uA̍PA̍PȂhȀ\A̋DA̋DȂhȂhA̍PȀ\A̓uA̓uȀ\A̍PȀ\ÁA�|�ÁA�|�A�r�A�r�A�v�A�jA�bNA�bNA�dZA�`BA�^5A�`BA�`BA�ZA�`BA�`BA�\)A�\)A�`BA�\)A�ZA�\)A�`BA�^5A�ZA�\)A�`BA�ZA�^5A�`BA�\)A�ZA�\)A�VA�VA�ZA�XA�S�A�XA�ZA�XA�VA�ZA�ZA�VA�XA�ZA�XA�VA�XA�ZA�ZA�VA�XA�ZA�ZA�VA�ZA�ZA�VA�ZA�ZA�VA�XA�\)A�XA�VA�VA�ZA�VA�S�A�VA�XA�XA�Q�A�S�A�VA�VA�Q�A�S�A�VA�VA�S�A�Q�A�S�A�VA�Q�A�O�A�Q�A�S�A�Q�A�O�A�Q�A�S�A�M�A�O�A�S�A�Q�A�M�A�O�A�O�A�O�A�I�A�I�A�M�A�K�A�G�A�K�A�M�A�K�A�G�A�I�A�M�A�K�A�G�A�K�A�K�A�E�A�E�A�I�A�I�A�E�A�G�A�I�A�E�A�C�A�E�A�I�A�G�A�C�A�C�A�C�A�G�A�G�A�C�A�C�A�E�A�G�A�C�A�C�A�E�A�E�A�C�A�?}A�E�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�C�A�?}A�A�A�C�A�?}A�?}A�C�A�E�A�?}A�A�A�C�A�C�A�?}A�A�A�C�A�C�A�?}A�?}A�C�A�C�A�A�A�?}A�A�A�C�A�?}A�=qA�?}A�A�A�A�A�?}A�;dA�;dA�?}A�A�A�A�A�?}A�=qA�?}A�A�A�A�A�=qA�=qA�?}A�A�A�=qA�?}A�C�A�A�A�=qA�A�A�C�A�A�A�=qA�?}A�A�A�?}A�;dA�;dA�=qA�?}A�9XA�;dA�=qA�;dA�7LA�7LA�;dA�;dA�7LA�5?A�7LA�7LA�5?A�1'A�7LA�33A�/A�/A�1'A�1'A�-A�(�A�+A�-A�(�A�$�A�$�A�(�A�+A�&�A�"�A�"�A� �A��A��A��A��A�oA�
=A�
=A�VA�VA�JA�%A���A���A���A��A��A��A��A��A��mA��yA��A��yA��`A��`A��TA��/A��
A��A��A��A���A�ȴA�ȴA���A���A˼jA˼jA�A���A˸RA˶FA˶FA˲-AˮAˮAˮA˧�A˥�A˧�AˬA˧�A˥�A˥�Aˣ�A˛�A˕�A˗�A˓uAˍPAˉ7Aˉ7A�~�A�z�A�x�A�v�A�t�A�p�A�n�A�p�A�r�A�n�A�jA�l�A�n�A�l�A�hsA�jA�l�A�jA�hsA�ffA�hsA�hsA�dZA�dZA�hsA�ffA�bNA�`BA�dZA�`BA�\)A�\)A�^5A�XA�XA�ZA�\)A�XA�S�A�XA�XA�S�A�Q�A�Q�A�Q�A�K�A�I�A�I�A�I�A�E�A�C�A�E�A�C�A�?}A�=qA�?}A�=qA�9XA�5?A�9XA�/A�$�A�&�A�&�A�"�A��A��A��A�VA�  A���A��A��A���A��A��A��A��A��yA��`A��HA��/A��#A���A�AʾwAʲ-Aʩ�AʍPA�x�A�VA�5?A�{A���A��A���A���AɶFAɡ�Aə�AɑhAɇ+A�bNA�1'A�A���A��#A���Aȴ9AȰ!Aț�A�XA�{A��`AǺ^AǇ+A�ffA�C�A�VA���A��TA���A�p�A�C�A��mAŗ�A�$�A���Aě�A�|�A�S�A�AöFA� �A�A�  A���A��A¶FA�A�A�A7AA�l�A�1'A��mA���A�jA�A�A��A��A���A�l�A�7LA� �A�
=A��A��#A��!A�p�A�A�A��A��;A���A�XA�1A���A���A�VA��A��
A���A�jA�"�A��^A�bNA���A��9A�^5A��A��A���A��DA��#A��A�l�A�bA���A��PA�VA�JA��/A�v�A�Q�A���A���A���A�hsA�=qA��A���A���A�ȴA�ĜA��jA��A���A���A���A��PA�dZA�M�A�G�A�E�A�7LA�$�A��A�oA�bA�%A�  A�  A���A���A�ĜA���A��jA��jA��jA��RA��9A��9A��9A��FA��FA��9A��!A��A���A���A��DA�t�A�hsA�^5A�O�A�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                   A���A���A�ƨA�ƨA�ĜA�A�A�ĜA�ƨA�ƨA�ȴA�ĜA���A�ĜA̾wA���A�ȴA�ĜA���A���A̼jA̶FA̲-ȂhA̍PȀ\ÁA�jA�^5A�^5A�\)A�\)A�XA�XA�XA�XA�XA�XA�VA�S�A�S�A�Q�A�Q�A�M�A�K�A�I�A�I�A�E�A�E�A�E�A�C�A�C�A�A�A�A�A�A�A�C�A�A�A�A�A�?}A�?}A�?}A�A�A�A�A�=qA�;dA�9XA�33A�-A�&�A��A�VA���A��A��TA���A���A˲-A˩�A˝�Aˇ+A�r�A�l�A�hsA�^5A�S�A�C�A�(�A���A�A��HA���A�oA�K�A¡�A�A�M�A���A�C�A��FA���A��^A�VA�  A��^A���A�=qA�1'A�\)A���A��+A��jA���A��DA�%A���A��uA�A�l�A�`BA�n�A�%A�bA��mA�ȴA���A��DA��`A�t�A�S�A���A�%A�(�A�#Aw�^As��Aq��Ai�A\�AX9XAU��ATr�ASXAQO�AJ�ADĜAA+A>��A=S�A<JA:�\A7;dA5C�A2~�A.M�A+��A*�!A)�A%�^A${A$  A#�mA#��A#��A#�PA#dZA#S�A"~�A �`A �A I�A�FA�A%A��A 5?A �DA -AA�A�-AA�A��A��A��A�hA�A7LA�A�uA��A��A�`AbNAVAbNA%AAA
E�A	t�A	�A	��A
�uA�mA�!AK�A��AdZA�jAn�AbAAt�A&�A
�jA
E�A�RA�#AS�A�uA1'A��A�A\)A"�A�
A33A&�A%A�jA�!A^5A��AK�A�A �D@���@���@�~�@�E�@���@��@��;@�|�@�C�@�@���@�=q@�@��`@��\@�?}@���@�bN@� �@�@�~�@��@�/@�Q�@��@�J@홚@��m@�ff@�^@�7@��@�bN@���@睲@畁@�P@�-@�/@�Z@�@�C�@�!@ᙚ@��`@�I�@�l�@�5?@��@�x�@܃@���@ۍP@�dZ@��@�n�@�@�&�@ش9@��m@�l�@�M�@�7L@�1@�o@Ұ!@���@��@Ѓ@�1'@��m@Ͼw@��@��@̬@˥�@�dZ@�C�@�o@ʸR@��T@ɩ�@ɑh@�G�@� �@Ǯ@�33@���@��@ř�@�7L@��@ēu@�  @å�@�t�@�o@�@�M�@�$�@�{@���@�?}@���@�r�@�I�@�+@���@�M�@�5?@�$�@��@�{@��@�-@�=q@�=q@�{@��-@���@��m@�+@�$�@��T@���@�/@���@���@�ƨ@�@��@���@���@�@���@�&�@���@�Ĝ@�9X@���@��w@��@�~�@�ff@��@�G�@�V@��`@��@�1@�\)@�o@��@��!@�ff@�-@���@��7@�Ĝ@�Z@�ƨ@�;d@���@���@��!@�~�@�E�@�@���@�G�@��@�%@���@�Ĝ@��@�j@�Q�@� �@��@��@�@�ȴ@��@���@�O�@�Ĝ@���@�r�@�1'@��
@�\)@�+@�@��\@�-@��@�@��#@��@�G�@�/@��@��@�(�@�  @�t�@�S�@�"�@���@��+@�=q@���@���@��h@�x�@��@�j@�I�@�A�@�(�@��m@��@�;d@��+@�$�@�J@�{@��@�X@��@�9X@��@��@���@�C�@�o@���@�v�@�J@���@�x�@�G�@�%@�z�@� �@��
@��F@�\)@�o@��!@�~�@�^5@�E�@�-@��@��-@�/@��`@��@��@�Z@�1'@���@���@�dZ@�"�@�ȴ@��\@�ff@�-@�@��^@�hs@��@��/@��9@�r�@�(�@�1@��m@��w@��P@�o@���@���@�ff@�-@�@��@���@�/@��@�Ĝ@�z�@��@���@��@�ff@�-@��@�{@��@���@�X@�G�@��@���@��@���@�K�@�|�@�dZ@���@��w@�ƨ@��F@��P@�\)@�$�@�p�@�p�@�G�@�Ĝ@��@�r�@�A�@�(�@�b@�P@K�@~��@~�+@
=@��@�;@�@K�@
=@~��@~�y@~��@~E�@}�@}�-@}�@}V@|�@|9X@{�F@{C�@{o@z�\@z�@y�#@y�#@y�^@yG�@xb@w�@v��@v$�@u��@up�@u?}@t��@t��@t�@t�@t�j@t(�@s��@r�@r~�@q��@q��@q�7@p��@p��@p��@o\)@n�R@m��@m/@l�j@k�m@kt�@j�@j-@i��@ix�@i7L@h�@hA�@g�@g�@g+@f�@f��@fE�@f@e��@d9X@d1@cS�@c@b�@b�H@b��@b�!@b-@aX@`Ĝ@`bN@`A�@`b@_�@_��@_�w@_��@_K�@^��@]��@]/@\�@\�j@\�D@\Z@[�F@[C�@Z�H@Z��@Z-@Yhs@X1'@W��@W|�@W�@Vv�@V{@U�T@U�h@U�@T��@T�@SC�@R��@R�\@R~�@RM�@R-@RJ@Q�7@QG�@Q%@P�`@P��@Pr�@PA�@O��@O;d@O+@O�@Nv�@M�T@MO�@L��@L��@Lz�@K�
@K�@Kt�@Ko@K@J�!@I�#@IX@H��@Hr�@H1'@H  @G�@G|�@G
=@F��@F$�@F{@F@E�h@D��@D�D@DZ@D�@C�@CS�@B�@B^5@A��@A%@@A�@@  @?�w@?l�@>v�@=��@=�h@=O�@=/@<Z@;��@;S�@:�H@:�!@:~�@:^5@:J@9��@9��@9�^@9�^@9�^@9G�@8��@8�9@8b@7�w@7|�@7K�@7�@7
=@6��@6�@6v�@5�@5@5`B@4��@4I�@49X@3��@3��@3"�@1��@17L@1�@0�`@0�u@0r�@0A�@/�w@/|�@/+@.�y@.�R@.ff@.$�@.$�@.@-p�@,�/@,�@,��@,�D@,z�@,Z@,1@+�F@+��@+dZ@+o@*�\@*-@)�^@)X@)%@(��@(Ĝ@(�@(Q�@( �@'�;@'�P@'|�@'+@&�y@&��@&v�@&V@&$�@&$�@&@%�-@%`B@$��@$�@$z�@$I�@$9X@$�@$�@#��@#�m@#ƨ@#��@#33@#@"�H@"��@"~�@"=q@!��@!7L@ ��@ bN@   @��@�@�+@V@E�@$�@�T@��@O�@/@�@��@�@Z@9X@(�@1@��@"�@��@��@��@�\@=q@��@��@�7@X@�@��@�9@r�@Q�@1'@b@�@��@�@l�@\)@+@
=@�y@�@�R@��@�+@V@E�@�@�-@`B@�/@�@z�@Z@9X@1@�m@ƨ@��@dZ@33@o@�H@��@��@�\@n�@^5@M�@=q@-@-@J@��@��@��@��@�@�@�^@��@G�@�9@�@bN@Q�@1'@ �@  @�@�@�;@�w@�@�P@K�@�@
=@�y@�@ȴ@��@�+@v�@5?@@�T@��@��@@�-@�-@�-@��@�h@�h@`B@O�@?}@/@�@V@V@V@��@��@�@�/@�j@��@I�@I�@I�@I�@I�@9X@(�@��@ƨ@��G�O�A�ĜA���A���A���A���A���A�ƨA���A�ĜA�ĜA�ȴA�ƨA�ĜA�ƨA�ĜA�ȴA�ĜA�A�A�ĜA���A�A�ĜA���A�ĜA�ȴA�ĜA���A�A�ȴA�ĜA�ĜA�ƨA�ĜA���A�ȴA�ƨA���A���A�ƨA�A���A̼jA���A�A̾wA���A�A���A���A�ĜA�A�A�ƨA�ȴA�ĜA�A�ĜA̾wA̾wA̺^A̼jA̺^A̸RA̾wA̾wA���A�A�ȴA�ĜA�ƨA�ȴA�ȴA�ȴA���A�ȴA�ƨA���A�ƨA�A���A̾wA�ƨA���A���A�ƨA���A���A�ƨA�ȴA���A���A�ƨA���A���A�ȴA���A���A�ĜA̶FA̰!A̩�A̶FA̴9A̧�A̰!A�A���A̸RA̼jA̴9A̺^A̺^A̲-A̰!A̝�A̙�A̗�Ȁ\Ȁ\A̓uA̍PA̍PȂhȀ\A̋DA̋DȂhȂhA̍PȀ\A̓uA̓uȀ\A̍PȀ\ÁA�|�ÁA�|�A�r�A�r�A�v�A�jA�bNA�bNA�dZA�`BA�^5A�`BA�`BA�ZA�`BA�`BA�\)A�\)A�`BA�\)A�ZA�\)A�`BA�^5A�ZA�\)A�`BA�ZA�^5A�`BA�\)A�ZA�\)A�VA�VA�ZA�XA�S�A�XA�ZA�XA�VA�ZA�ZA�VA�XA�ZA�XA�VA�XA�ZA�ZA�VA�XA�ZA�ZA�VA�ZA�ZA�VA�ZA�ZA�VA�XA�\)A�XA�VA�VA�ZA�VA�S�A�VA�XA�XA�Q�A�S�A�VA�VA�Q�A�S�A�VA�VA�S�A�Q�A�S�A�VA�Q�A�O�A�Q�A�S�A�Q�A�O�A�Q�A�S�A�M�A�O�A�S�A�Q�A�M�A�O�A�O�A�O�A�I�A�I�A�M�A�K�A�G�A�K�A�M�A�K�A�G�A�I�A�M�A�K�A�G�A�K�A�K�A�E�A�E�A�I�A�I�A�E�A�G�A�I�A�E�A�C�A�E�A�I�A�G�A�C�A�C�A�C�A�G�A�G�A�C�A�C�A�E�A�G�A�C�A�C�A�E�A�E�A�C�A�?}A�E�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�C�A�?}A�A�A�C�A�?}A�?}A�C�A�E�A�?}A�A�A�C�A�C�A�?}A�A�A�C�A�C�A�?}A�?}A�C�A�C�A�A�A�?}A�A�A�C�A�?}A�=qA�?}A�A�A�A�A�?}A�;dA�;dA�?}A�A�A�A�A�?}A�=qA�?}A�A�A�A�A�=qA�=qA�?}A�A�A�=qA�?}A�C�A�A�A�=qA�A�A�C�A�A�A�=qA�?}A�A�A�?}A�;dA�;dA�=qA�?}A�9XA�;dA�=qA�;dA�7LA�7LA�;dA�;dA�7LA�5?A�7LA�7LA�5?A�1'A�7LA�33A�/A�/A�1'A�1'A�-A�(�A�+A�-A�(�A�$�A�$�A�(�A�+A�&�A�"�A�"�A� �A��A��A��A��A�oA�
=A�
=A�VA�VA�JA�%A���A���A���A��A��A��A��A��A��mA��yA��A��yA��`A��`A��TA��/A��
A��A��A��A���A�ȴA�ȴA���A���A˼jA˼jA�A���A˸RA˶FA˶FA˲-AˮAˮAˮA˧�A˥�A˧�AˬA˧�A˥�A˥�Aˣ�A˛�A˕�A˗�A˓uAˍPAˉ7Aˉ7A�~�A�z�A�x�A�v�A�t�A�p�A�n�A�p�A�r�A�n�A�jA�l�A�n�A�l�A�hsA�jA�l�A�jA�hsA�ffA�hsA�hsA�dZA�dZA�hsA�ffA�bNA�`BA�dZA�`BA�\)A�\)A�^5A�XA�XA�ZA�\)A�XA�S�A�XA�XA�S�A�Q�A�Q�A�Q�A�K�A�I�A�I�A�I�A�E�A�C�A�E�A�C�A�?}A�=qA�?}A�=qA�9XA�5?A�9XA�/A�$�A�&�A�&�A�"�A��A��A��A�VA�  A���A��A��A���A��A��A��A��A��yA��`A��HA��/A��#A���A�AʾwAʲ-Aʩ�AʍPA�x�A�VA�5?A�{A���A��A���A���AɶFAɡ�Aə�AɑhAɇ+A�bNA�1'A�A���A��#A���Aȴ9AȰ!Aț�A�XA�{A��`AǺ^AǇ+A�ffA�C�A�VA���A��TA���A�p�A�C�A��mAŗ�A�$�A���Aě�A�|�A�S�A�AöFA� �A�A�  A���A��A¶FA�A�A�A7AA�l�A�1'A��mA���A�jA�A�A��A��A���A�l�A�7LA� �A�
=A��A��#A��!A�p�A�A�A��A��;A���A�XA�1A���A���A�VA��A��
A���A�jA�"�A��^A�bNA���A��9A�^5A��A��A���A��DA��#A��A�l�A�bA���A��PA�VA�JA��/A�v�A�Q�A���A���A���A�hsA�=qA��A���A���A�ȴA�ĜA��jA��A���A���A���A��PA�dZA�M�A�G�A�E�A�7LA�$�A��A�oA�bA�%A�  A�  A���A���A�ĜA���A��jA��jA��jA��RA��9A��9A��9A��FA��FA��9A��!A��A���A���A��DA�t�A�hsA�^5A�O�A�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5tBbB.B�B�B�B�B.B�B�B.B.B�B�B�B�B�B�B�B�B\B�B�B�BVBVB�BVB�B�B�B�BPBB�B�BPBB�B�B~B�B~B�BJBJBBJBB�B�BDB�B�B�BDB�BDBB
�BB
�BB
�B
rB	�B	7B1B�B_BB�B�B �B�cB��B��B�B��B��B�8B��B��B��B�2B�2B��B��B��B�lB�DB�(BJB~BMBeB$B/�BA�BD3BA�BB�BB�BB'B@�B@OBE�B?�BB[B8�B1[B-�B#:B�B�;B��B�kB�=Bm]BV�B?�B/OB �B
�&B
��B
��B
�eB
�tB
�'B
��B
�DB
.B
l�B
2�B
@B	�cB	��B	� B	m)B	bB	XEB	O�B	GzB	1�B	SB	�B	(B	B	
�B	DB	�B	B��B	  B��B�B�B	B��B��B��B��B�%B��B��B�rB	SB��B��B	�B	(B	�B	(�B	EmB	XEB	h
B	lWB	g�B	T�B	FB	A�B	I�B	L0B	MjB	MjB	K^B	E9B	7�B	7B	=B	6�B	8B	1[B	*0B	%B	qB	�B	=B	B	"�B	,�B	;dB	Z�B	jB	u�B	��B	��B	�wB	�qB	�kB	�*B	�0B	�kB	�qB	�IB	��B	�tB	�tB	��B	��B	��B	�B	��B	�LB	��B	�B	�6B	��B	��B	��B	�B	��B	�B	�B	�3B	�tB	��B	�EB	�zB	�B	�RB	�dB	�pB	�BB	�B	�B	��B	�vB	�BB	ϫB	�<B	�B	�dB	��B	�dB	��B	�sB	��B	یB	ںB	��B	خB	چB	�
B	רB	�sB	�EB	��B	�
B	רB	�QB	��B	��B	�QB	��B	��B	��B	ܒB	�]B	ݘB	�B	��B	��B	�dB	�;B	�B	��B	�5B	�TB	��B	�fB	�2B	�2B	�B	�fB	��B	�mB	�B	�WB	�B	�B	��B	�B	�B	��B	�B	�]B	�;B	�]B	�WB	��B	�cB	��B	� B	��B	��B	�B	�B	�|B	��B	�B	�TB	��B	��B	�8B	�B	�8B	�rB	�B	�B	��B	��B	�DB	��B	�B	�B	��B	�B	��B	�VB	�"B	��B
 iB
 �B
B
�B
�B
uB
�B
B
�B
fB
fB
�B
�B
fB
�B

	B
�B
�B
1B
+B
1B
�B
�B
�B
�B
fB
�B
	7B
	lB
	7B
	�B

	B

	B

=B
JB
�B
�B
�B
(B
�B
�B
.B
B
�B
�B
�B
�B
�B
�B
�B
MB
SB
YB
�B
	B
qB
=B
=B
�B
�B
�B
IB
�B
�B
�B
!B
�B
�B
�B
 'B
 'B
 \B
 �B
!�B
!�B
#:B
"4B
"hB
"�B
"�B
"�B
#nB
$�B
&B
%�B
%�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
*0B
)�B
*0B
+B
*0B
*�B
+kB
+�B
,=B
-B
-�B
-�B
-�B
.�B
/�B
/OB
/B
/OB
.�B
/B
/�B
/�B
/�B
/�B
2-B
2�B
33B
2-B
2�B
1�B
2-B
2aB
2�B
2�B
2�B
33B
4nB
4nB
4B
49B
4�B
6B
5�B
7B
7�B
8�B
8B
8�B
9�B
:^B
9�B
9�B
:�B
:�B
;�B
;dB
;�B
;�B
<6B
<jB
=B
>wB
>BB
?B
?�B
?}B
?�B
?�B
?�B
@OB
@�B
@�B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
AUB
A�B
B[B
B�B
B�B
B�B
DgB
DgB
D�B
D�B
D3B
D�B
DgB
D�B
E9B
E�B
F?B
F�B
G�B
HB
H�B
K^B
L�B
MB
LdB
K)B
I�B
K^B
LdB
M�B
OB
OvB
O�B
O�B
OvB
M�B
JXB
J#B
J�B
J�B
J�B
K)B
K�B
K�B
LdB
MjB
M�B
N<B
N�B
QNB
TaB
T�B
T�B
U�B
U�B
U�B
U�B
VB
VB
VmB
VmB
VmB
W
B
W?B
W?B
W�B
W�B
W�B
XyB
X�B
X�B
X�B
X�B
YB
Z�B
[WB
[�B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
^5B
^�B
_;B
_pB
_;B
_pB
_�B
_pB
_;B
`BB
_pB
`BB
`BB
`vB
aHB
aHB
a�B
bNB
b�B
b�B
b�B
c�B
c�B
c�B
dZB
e,B
f2B
f�B
f�B
gB
f�B
h>B
h
B
h�B
h�B
h�B
h�B
h�B
h�B
iyB
jB
jB
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
kQB
lWB
l�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
n�B
oiB
o�B
oiB
pB
p�B
p�B
p�B
qB
qvB
qvB
qvB
q�B
q�B
q�B
rB
rB
rGB
r|B
sB
r�B
r�B
r�B
sB
sMB
sMB
tB
tB
tB
s�B
t�B
t�B
uZB
u�B
u�B
u�B
v�B
w2B
w2B
w�B
wfB
w�B
xlB
y	B
yrB
y�B
zB
zB
zDB
zDB
z�B
{B
{B
z�B
z�B
{B
{�B
{�B
{�B
{�B
|PB
|B
|�B
|�B
}"B
}�B
~]B
~]B
~(B
~]B
�B
� B
�4B
�4B
�iB
��B
��B
��B
�B
�B
�uB
�AB
�B
��B
�GB
�MB
�SB
��B
�YB
�YB
��B
�+B
�_B
��B
��B
��B
��B
��B
��B
�fB
��B
�fB
�B
�7B
�lB
�7B
��B
��B
��B
��B
��B
��B
��B
��B
�xB
��B
�B
�~B
�~B
��B
��B
�B
�PB
�PB
�B
�"B
�VB
�VB
�VB
�VB
�VB
��B
�(B
�(B
�(B
�\B
��B
��B
�.B
�hB
��B
��B
�B
�B
�:B
�oB
�oB
��B
��B
��B
�@B
�uB
��B
��B
��B
��B
��B
��B
�B
�{B
��B
�MB
��B
��B
��B
��B
��B
��B
�B
��B
�B
��B
�SB
��B
��B
��B
��B
�YB
��B
��B
��B
�YB
�_B
��B
��B
��B
��B
��B
��B
�eB
��B
��B
��B
��B
�7B
�7B
�kB
�7B
�kB
��B
�=B
�qB
�=B
�=B
�qB
�qB
�B
�B
�B
�CB
�xB
�xB
��B
�B
��B
�B
�B
�B
�IB
�~B
��B
��B
��B
�B
�OB
�B
��B
��B
��B
��B
��B
�!B
�VB
��B
�'B
�\B
��B
��B
��B
�-B
�-B
�bB
�-B
��B
��B
��B
��B
��B
��B
��B
�4B
�4B
�hB
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
�:B
��B
�B
�@B
�@B
�@B
�tB
�tB
��B
��B
��B
��B
��B
��B
�FB
�FB
�FB
�FB
�FB
�zB
�zB
��B
��B
�B
�B
�LB
�LB
�LB
�LB
�LB
�LB
�B
��B
�LB
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
��B
�B
�B
�RB
��B
�XB
��B
��B
��B
��B
��B
�_B
��B
��B
�B
��A�w2B�B�BbB�B�B B(BhB�B�B�B B\B�B�B.B�B�B�B�B�B(B4B�B�BbB�B�B�B4B�B\B�B�B�B4B(B\B�B�B.B B�B�BbBbB�BbBbB(B�BbB�B�B�B B�B�B�B\B�B�B�B�B"BbB\BVB B�BPB�B�B\B�BhB�BbBbB�B\B�B�B\B4B\B(B B�B(B\B�B�B�B.B(B�BhB�B�B�B�B+B�B�B:B�B�BB\BB"B�BuB:B�B�BbBBPB�B�B�B�B�B�B�B�B�B�B�BVB�B�B�B�B�B�B�B�BVB�B.BhB�B�B"BVB�B�B�BB�B�B"B�B�B�B�B~BPBVB�B~B�B�BJBPB�B~B�B�BJB�BVBBB�B�B~BJB"B�B~B�B�B�BxB�B"BPBB�B�B�B�B"B�B�BVBPB�B�BVBB�B�B�B�B~BB"BPB�BJB�BB�B�BB�B�B�B�B�B~B
�BB�B~B�B�B�BxBJB�B�B�BJB�B�BxBBPBBDBB�B~BDBBPB�BxBPB�BxBDBB�B
�B�BB~BB
�B~BBJBB�B�B�BxBDB~B~B
�B
�B�B�B
�B
�B~BJB
�B
�BBJB
�B
�B�B�BDBB�BBxBB�BxB
�B�B~B
�B
rB~BB
�BDB~BB
�B
�B�BJB
�B
rBDB~BB
rBxBB�B
rB
	BBB�B
=B
	B
	B
�B�BB
	B
=B�B�B
rB
=B�BxB	�B
�BBDB	�B
�B�BDB
	B
rB�B�B
	B	7B�B
rB	�B	�BB
rB	B	7B
=B
�B	BfB	�B
	B�B�B	�B	lB�B�B	B	7B�B�B_BfB�B+B�B_B�B_B�B_B�B�B�BB�B%BMB�BB�B�B�B�BuB�B�B �BBuB�B iB �BBB �B  BB iB��B�]B��B �B�]B�VB��B��B�(B�B��B��B�VB��B��B��B�B��B��B��B�JB�DB�B��B�JB�B��B�JB�>B�rB��B�xB��B�B�DB�rB��B��B��B��B�B��B��B��B��B��B�2B��B��B��B��B��B��B�2B��B�lB�B��B��B�8B�8B�`B��B�lB�fB��B�B�lB�2B�`B�fB�8B��B��B��B�8B��B�+B�B�lB�fB�`B�fB�8B��B�`B��B�8B��B��B�B�lB�+B�B��B�fB��B�2B��B�B�`B�`B��B��B�rB�2B��B��B��B��B��B�ZB�+B��B��B�`B�+B��B��B��B��B��B�8B�xB��B�xB�DB�xB�`B�ZB��B��B�2B�+B�ZB��B��B�	B�2B�B��B�B�%B�	B��B��B��B��B��B�DBB��B��B�lB��BYB�BfB�B�B�BAB
rB~B
	B�BB~B
rB	7B	7B�B�B�B	�B
	B	�BPBBuBB�B�B{BuBIB�B�BoB�B�BMB�BxB$BkBB�B�B"�B�B�B"4B%FB#:B!�B&�B)_B-wB,B/OB-�B33B,qB+�B(�B1�BA�BC�BFB=�BA�B>�B<�B@�B;dBGEB>BA�BB�BFtBB[BFtBIBC�BB[BA B@OBA BB'BA�B@OB@OBB[BHBC�B@�B@�BB�BC�BC�BA�BA BB�BB�BAUBA�BGEBC�BB�BC-BAUB@�BA�BB�BB�BAUB?�B?}B@OB@�B@�B?�B@OBB�BB'BB�BAUB@OB?HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022031721165320220317211653IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022032715305320220327153053QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022032715305320220327153053QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                