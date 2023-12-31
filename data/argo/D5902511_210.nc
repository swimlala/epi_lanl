CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-04-27T01:17:28Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     8  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   U@   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     8  [P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   s�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     8  y�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                     PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 #(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ;`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ;�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   A�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   G�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T M�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   N   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   N   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   N$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   N,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � N4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   N�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   N�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    N�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        N�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        O    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       O   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OArgo profile    3.1 1.2 19500101000000  20220427011728  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_210                 6810_008521_210                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��~#c�W@��~#c�W11  @��~Ov_�@��~Ov_�@0%��R�@0%��R��d�e�%�d�e�%11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?aG�?��@=p�@}p�@��R@�  @�G�A   A\)A\)A,(�A@  A^�RA\)A�  A�\)A��A�  AϮA߮A�  A��B  B(�B  B   B(  B0  B8(�B@(�BHQ�BP  BW�
B_�
Bh  Bp  Bw�
B�  B�  B�  B�  B�  B�  B��B�  B��B��B��B�  B��B�  B�{B��B��B�  B�{B�  B�{B�{B�{B�  B��B�  B�{B�(�B�{B�  B�(�B�(�C {C{C
=C
=C  C	��C��C  C  C  C
=C
=C
=C  C
=C
=C��C"  C#��C&  C({C*  C+��C.{C0
=C2  C4  C6  C7�C:  C<  C>
=C@  CB
=CC��CF
=CH  CJ
=CL
=CN
=CP{CR  CT  CV
=CW��CY��C[��C^  C`
=Cb
=Cd
=Cf
=Ch
=Cj  Ck�Cm�Co��Cr  Cs��Cv  Cx  Cz  C{��C~  C�  C�  C�  C�  C�C���C���C�  C��C�  C�C�C�C�
=C�\C�C�  C�C�C�C�  C�  C�C�C�C�  C�C�
=C�C�C�
=C�C���C�  C���C���C�C�
=C�  C�C�  C�  C�C�
=C�  C���C���C�  C�  C�  C�
=C�C�C�
=C�  C�  C�C�C�C�  C�  C�C�C�  C���C���C���C���C�  C�C�C�
=C�
=C�  C���C�  C�  C�  C���C�C���C���C�C�
=C�C�C�C�C�  C���C�  C�C�C���C���C�  C�C���C�  C�C�C�  C�  C���C���C���C�  C�  C�  C���C���C���C���C��C���C�  C�C�  C���C�  C�C�  C�  C�
=C�C�  C�  C�D   D }qDD� D�qD� D�D��D�D��D  D��D�D� D�qD� D  D}qD�qD	z�D
  D
� D  D�D�D� D  D��D  D}qD�qD� D  D}qD�qD� D  D� DD��D�D� D�D� D�qD� D  D� D  D� D�qD� DD��D�D��D  D��DD� D  D��D�D��D �D }qD!  D!}qD"  D"��D#  D#}qD#��D$� D$�qD%}qD%�qD&}qD&�qD'}qD(  D(� D(��D)z�D*  D*��D*�qD+}qD,  D,� D-�D-� D.  D.��D.�qD/� D0  D0z�D0�qD1� D2  D2� D2�qD3� D4  D4� D5�D5� D5��D6}qD7  D7��D8�D8��D9  D9� D:  D:� D;D;��D<  D<��D=  D=� D>  D>}qD?�D?�D@�D@}qD@��DA}qDA�qDBz�DB�qDC}qDD  DD�DE�DE� DF�DF�DG�DG�DH�DH}qDH�qDI� DJ  DJ��DK�DK}qDK�qDL� DM  DM� DN  DN� DO  DO� DP  DP� DP�qDQz�DR  DR��DR�qDS� DT�DT� DU�DU��DV�DV��DV��DW}qDX�DX��DY  DY��DZ  DZ� D[  D[}qD\  D\}qD]  D]��D]�qD^z�D^�qD_}qD`  D`��Da�Da�DbDb��Db�qDcz�Dc�qDd� De  De}qDe�qDf��Dg  Dg� DhDh��Di  Di}qDi�qDj� Dj�qDkz�Dl  Dl� Dl�qDm� DnDn��Do�Do�Dp�Dp}qDp��Dq}qDr  Dr��DsDs�Dt  Dt}qDu  Du� Dv  Dv}qDw  Dw��Dx  Dx� Dy  Dy}qDz  Dz��D{  D{}qD|  D|}qD|�qD}� D~�D~��D�D��D�HD�AHD���D��HD���D�=qD�~�D�� D�  D�>�D�~�D�� D���D�>�D�� D��HD��D�AHD�� D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D���D�@ D��HD���D�HD�AHD�~�D��qD��qD�@ D���D���D��\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?B�\?�\)?\?�(�@
=q@&ff@8Q�@J=q@fff@u@��
@��@�p�@��@��@��R@���@�33@�G�@���@�
=A�A�A(�A�AQ�Ap�A"�\A(��A/\)A4z�A:=qA@��AEAJ�HAQG�AXQ�A]p�Aa�Ah��Ap��Au�Az=qA���A��A�p�A���A�(�A�ffA���A�z�A�\)A�G�A�(�A��A��A���A�  A�=qA���A���A��A�A�Q�A��
A�
=A�G�A��
A�\)A��A�z�A�  A��HA�p�A�  A��
A޸RA���A��
A�\)A�=qA���A�\)A��HA�{A���A��HA��RB ��BB\)B��B{B33B��B
=qB33Bz�B{B
=B  BG�B�\B\)B��B��B�\B�B��BG�B=qB�B(�B��B�B
=B�B (�B!G�B"ffB"�RB#�B$��B%p�B&{B'33B((�B(��B)��B*�HB+\)B,  B,��B.{B.�RB/33B0Q�B1G�B2{B2�RB3�
B4��B5G�B6ffB7\)B8(�B8��B9B:�HB;�B<(�B=�B>=qB>�HB?\)B@��BA�BABB�HBC�
BD(�BE�BF{BG
=BG�BH(�BH��BI�BJ�RBK33BK�
BL��BMBN{BN�HBO�
BPQ�BP��BQ�BR�RBS\)BT(�BT��BU�BV�HBW\)BX  BYG�BY�BZ�\B[�B\��B]G�B^{B_33B`(�B`��Bap�Bb�\Bc�Bdz�Bd��Bf=qBg\)Bh(�Bh��BiBj�RBk33BlQ�Bmp�Bm�Bn�\Bo�
Bp��Bq�BqBr�RBs�
Btz�Bt��Bv{Bv�HBw�Bxz�Byp�BzffB{
=B{�B|��B}�B~ffB33B�{B���B�
=B�\)B�B�Q�B���B��B�p�B�  B��\B��HB��B��B�=qB��\B��HB�\)B��B�=qB���B�33B��B�{B�ffB���B�p�B��B�=qB�z�B��B��B��
B�Q�B��HB�33B��B�  B�z�B���B��B��B�(�B��\B���B�G�B�B�=qB���B��HB�\)B��B�ffB��RB�
=B�p�B�  B�z�B���B��B��B�(�B��\B��HB�G�B��
B�Q�B��RB���B��B�  B�z�B���B�33B�B�=qB�z�B���B��B�  B�Q�B��RB�33B��B�  B�Q�B��RB�G�B�B�{B�ffB���B�\)B��B��B�ffB��HB�33B��B�{B�z�B���B�G�B���B�(�B���B�
=B�G�B��B�(�B���B�
=B�G�B�B�Q�B���B��B�p�B�  B�z�B��HB�33B��B�=qB��RB��B��B��B�z�B�
=B�\)B��B�Q�B��HB�33B��B�{B���B���B�G�B��
B�ffB���B�
=B���B�{B�z�B��RB�33B�B�=qB�z�B��HB�p�B�  B�ffB£�B��BîB�=qBģ�B���B�G�B��
B�ffB��HB�\)BǮB�{Bȣ�B�33Bə�B��B�ffB�
=B˅B��B�Q�B̸RB�G�B��B�ffBθRB�33Bϙ�B�=qBиRB��BхB�  Bҏ\B�
=B�\)B�B�(�BԸRB�G�BծB�  B�ffB���BׅB��B�(�Bأ�B�33Bٙ�B��B�Q�B���B�\)B�B�(�B�z�B��HB�p�B��B�=qBޣ�B�
=Bߙ�B�  B�=qB�RB�33B�B�{B�ffB���B�\)B��B�(�B�\B��B�B��
B�Q�B��HB�G�B癚B�=qB���B��B�p�B��B�\B�
=B�\)B�B�Q�B���B�G�B홚B��B�z�B���B�\)B�B�{B�RB��B�p�B�B�=qB���B�G�B�B��B�ffB���B�p�B��B�=qB���B�G�B���B��B�ffB���B�p�B��B�(�B��RB�33B��B��B�z�B�
=B�p�B�B�(�B���B�33B��C   C G�C z�C ��C �
C(�Cp�C��CC  CG�C��CC��C33C�C��C
=C=qCp�C�C��C=qC�C�RC�C(�Cz�CC�C(�Cz�CC��C�CffC�RC	  C	(�C	\)C	�RC
  C
33C
ffC
�RC
=CG�Cp�C��C{CQ�C�CC
=C\)C��C�C�CQ�C�C��C33Cp�C�C��CG�C��C�HC{C\)C��C��CG�C�\CC  CG�C��C��C(�CffC�C  CQ�C��C��C{CQ�C��C��CG�Cz�CC
=C\)C�RC��C33Cp�CC�Cp�C�RC�C(�Cz�C�
C(�CffC�C��C33C�\C�HC33C�C��C
=CQ�C��C�C =qC ��C ��C!=qC!�C!��C"{C"z�C"��C#�C#\)C#��C#�C$=qC$�\C$��C%G�C%��C%�HC&�C&p�C&C'�C'p�C'C({C(ffC(�C)  C)G�C)��C)��C*Q�C*�C+  C+Q�C+��C+�C,=qC,�\C,�C-G�C-��C-�C.G�C.�C.��C/�C/z�C/�
C033C0�\C0�C1=qC1�\C1�
C233C2�C2��C3�C3p�C3�
C433C4�\C4�HC5G�C5��C5�C6=qC6��C6�HC733C7�C7��C8�C8�C8�
C933C9�\C9��C:(�C:z�C:��C;�C;ffC;C<{C<p�C<C=
=C=ffC=�RC>
=C>\)C>��C>��C?33C?�C?��C@(�C@ffC@CA{CAffCA�RCB
=CB\)CB��CB�CC33CC�CC��CD{CDffCDCE�CEffCE�CE��CF=qCF�CF��CG{CGffCG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                         1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?aG�?��@=p�@}p�@��R@�  @�G�A   A\)A\)A,(�A@  A^�RA\)A�  A�\)A��A�  AϮA߮A�  A��B  B(�B  B   B(  B0  B8(�B@(�BHQ�BP  BW�
B_�
Bh  Bp  Bw�
B�  B�  B�  B�  B�  B�  B��B�  B��B��B��B�  B��B�  B�{B��B��B�  B�{B�  B�{B�{B�{B�  B��B�  B�{B�(�B�{B�  B�(�B�(�C {C{C
=C
=C  C	��C��C  C  C  C
=C
=C
=C  C
=C
=C��C"  C#��C&  C({C*  C+��C.{C0
=C2  C4  C6  C7�C:  C<  C>
=C@  CB
=CC��CF
=CH  CJ
=CL
=CN
=CP{CR  CT  CV
=CW��CY��C[��C^  C`
=Cb
=Cd
=Cf
=Ch
=Cj  Ck�Cm�Co��Cr  Cs��Cv  Cx  Cz  C{��C~  C�  C�  C�  C�  C�C���C���C�  C��C�  C�C�C�C�
=C�\C�C�  C�C�C�C�  C�  C�C�C�C�  C�C�
=C�C�C�
=C�C���C�  C���C���C�C�
=C�  C�C�  C�  C�C�
=C�  C���C���C�  C�  C�  C�
=C�C�C�
=C�  C�  C�C�C�C�  C�  C�C�C�  C���C���C���C���C�  C�C�C�
=C�
=C�  C���C�  C�  C�  C���C�C���C���C�C�
=C�C�C�C�C�  C���C�  C�C�C���C���C�  C�C���C�  C�C�C�  C�  C���C���C���C�  C�  C�  C���C���C���C���C��C���C�  C�C�  C���C�  C�C�  C�  C�
=C�C�  C�  C�D   D }qDD� D�qD� D�D��D�D��D  D��D�D� D�qD� D  D}qD�qD	z�D
  D
� D  D�D�D� D  D��D  D}qD�qD� D  D}qD�qD� D  D� DD��D�D� D�D� D�qD� D  D� D  D� D�qD� DD��D�D��D  D��DD� D  D��D�D��D �D }qD!  D!}qD"  D"��D#  D#}qD#��D$� D$�qD%}qD%�qD&}qD&�qD'}qD(  D(� D(��D)z�D*  D*��D*�qD+}qD,  D,� D-�D-� D.  D.��D.�qD/� D0  D0z�D0�qD1� D2  D2� D2�qD3� D4  D4� D5�D5� D5��D6}qD7  D7��D8�D8��D9  D9� D:  D:� D;D;��D<  D<��D=  D=� D>  D>}qD?�D?�D@�D@}qD@��DA}qDA�qDBz�DB�qDC}qDD  DD�DE�DE� DF�DF�DG�DG�DH�DH}qDH�qDI� DJ  DJ��DK�DK}qDK�qDL� DM  DM� DN  DN� DO  DO� DP  DP� DP�qDQz�DR  DR��DR�qDS� DT�DT� DU�DU��DV�DV��DV��DW}qDX�DX��DY  DY��DZ  DZ� D[  D[}qD\  D\}qD]  D]��D]�qD^z�D^�qD_}qD`  D`��Da�Da�DbDb��Db�qDcz�Dc�qDd� De  De}qDe�qDf��Dg  Dg� DhDh��Di  Di}qDi�qDj� Dj�qDkz�Dl  Dl� Dl�qDm� DnDn��Do�Do�Dp�Dp}qDp��Dq}qDr  Dr��DsDs�Dt  Dt}qDu  Du� Dv  Dv}qDw  Dw��Dx  Dx� Dy  Dy}qDz  Dz��D{  D{}qD|  D|}qD|�qD}� D~�D~��D�D��D�HD�AHD���D��HD���D�=qD�~�D�� D�  D�>�D�~�D�� D���D�>�D�� D��HD��D�AHD�� D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D���D�@ D��HD���D�HD�AHD�~�D��qD��qD�@ D���D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?B�\?�\)?\?�(�@
=q@&ff@8Q�@J=q@fff@u@��
@��@�p�@��@��@��R@���@�33@�G�@���@�
=A�A�A(�A�AQ�Ap�A"�\A(��A/\)A4z�A:=qA@��AEAJ�HAQG�AXQ�A]p�Aa�Ah��Ap��Au�Az=qA���A��A�p�A���A�(�A�ffA���A�z�A�\)A�G�A�(�A��A��A���A�  A�=qA���A���A��A�A�Q�A��
A�
=A�G�A��
A�\)A��A�z�A�  A��HA�p�A�  A��
A޸RA���A��
A�\)A�=qA���A�\)A��HA�{A���A��HA��RB ��BB\)B��B{B33B��B
=qB33Bz�B{B
=B  BG�B�\B\)B��B��B�\B�B��BG�B=qB�B(�B��B�B
=B�B (�B!G�B"ffB"�RB#�B$��B%p�B&{B'33B((�B(��B)��B*�HB+\)B,  B,��B.{B.�RB/33B0Q�B1G�B2{B2�RB3�
B4��B5G�B6ffB7\)B8(�B8��B9B:�HB;�B<(�B=�B>=qB>�HB?\)B@��BA�BABB�HBC�
BD(�BE�BF{BG
=BG�BH(�BH��BI�BJ�RBK33BK�
BL��BMBN{BN�HBO�
BPQ�BP��BQ�BR�RBS\)BT(�BT��BU�BV�HBW\)BX  BYG�BY�BZ�\B[�B\��B]G�B^{B_33B`(�B`��Bap�Bb�\Bc�Bdz�Bd��Bf=qBg\)Bh(�Bh��BiBj�RBk33BlQ�Bmp�Bm�Bn�\Bo�
Bp��Bq�BqBr�RBs�
Btz�Bt��Bv{Bv�HBw�Bxz�Byp�BzffB{
=B{�B|��B}�B~ffB33B�{B���B�
=B�\)B�B�Q�B���B��B�p�B�  B��\B��HB��B��B�=qB��\B��HB�\)B��B�=qB���B�33B��B�{B�ffB���B�p�B��B�=qB�z�B��B��B��
B�Q�B��HB�33B��B�  B�z�B���B��B��B�(�B��\B���B�G�B�B�=qB���B��HB�\)B��B�ffB��RB�
=B�p�B�  B�z�B���B��B��B�(�B��\B��HB�G�B��
B�Q�B��RB���B��B�  B�z�B���B�33B�B�=qB�z�B���B��B�  B�Q�B��RB�33B��B�  B�Q�B��RB�G�B�B�{B�ffB���B�\)B��B��B�ffB��HB�33B��B�{B�z�B���B�G�B���B�(�B���B�
=B�G�B��B�(�B���B�
=B�G�B�B�Q�B���B��B�p�B�  B�z�B��HB�33B��B�=qB��RB��B��B��B�z�B�
=B�\)B��B�Q�B��HB�33B��B�{B���B���B�G�B��
B�ffB���B�
=B���B�{B�z�B��RB�33B�B�=qB�z�B��HB�p�B�  B�ffB£�B��BîB�=qBģ�B���B�G�B��
B�ffB��HB�\)BǮB�{Bȣ�B�33Bə�B��B�ffB�
=B˅B��B�Q�B̸RB�G�B��B�ffBθRB�33Bϙ�B�=qBиRB��BхB�  Bҏ\B�
=B�\)B�B�(�BԸRB�G�BծB�  B�ffB���BׅB��B�(�Bأ�B�33Bٙ�B��B�Q�B���B�\)B�B�(�B�z�B��HB�p�B��B�=qBޣ�B�
=Bߙ�B�  B�=qB�RB�33B�B�{B�ffB���B�\)B��B�(�B�\B��B�B��
B�Q�B��HB�G�B癚B�=qB���B��B�p�B��B�\B�
=B�\)B�B�Q�B���B�G�B홚B��B�z�B���B�\)B�B�{B�RB��B�p�B�B�=qB���B�G�B�B��B�ffB���B�p�B��B�=qB���B�G�B���B��B�ffB���B�p�B��B�(�B��RB�33B��B��B�z�B�
=B�p�B�B�(�B���B�33B��C   C G�C z�C ��C �
C(�Cp�C��CC  CG�C��CC��C33C�C��C
=C=qCp�C�C��C=qC�C�RC�C(�Cz�CC�C(�Cz�CC��C�CffC�RC	  C	(�C	\)C	�RC
  C
33C
ffC
�RC
=CG�Cp�C��C{CQ�C�CC
=C\)C��C�C�CQ�C�C��C33Cp�C�C��CG�C��C�HC{C\)C��C��CG�C�\CC  CG�C��C��C(�CffC�C  CQ�C��C��C{CQ�C��C��CG�Cz�CC
=C\)C�RC��C33Cp�CC�Cp�C�RC�C(�Cz�C�
C(�CffC�C��C33C�\C�HC33C�C��C
=CQ�C��C�C =qC ��C ��C!=qC!�C!��C"{C"z�C"��C#�C#\)C#��C#�C$=qC$�\C$��C%G�C%��C%�HC&�C&p�C&C'�C'p�C'C({C(ffC(�C)  C)G�C)��C)��C*Q�C*�C+  C+Q�C+��C+�C,=qC,�\C,�C-G�C-��C-�C.G�C.�C.��C/�C/z�C/�
C033C0�\C0�C1=qC1�\C1�
C233C2�C2��C3�C3p�C3�
C433C4�\C4�HC5G�C5��C5�C6=qC6��C6�HC733C7�C7��C8�C8�C8�
C933C9�\C9��C:(�C:z�C:��C;�C;ffC;C<{C<p�C<C=
=C=ffC=�RC>
=C>\)C>��C>��C?33C?�C?��C@(�C@ffC@CA{CAffCA�RCB
=CB\)CB��CB�CC33CC�CC��CD{CDffCDCE�CEffCE�CE��CF=qCF�CF��CG{CGffCG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                         1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�AҲ-Aҧ�Aҧ�Aҧ�Aҡ�Aҟ�A҉7A�x�A�l�A�bNA�O�A�;dA�"�A��A�VA�  A�  A���A���A���A���A��A��A��A��A��A��A��yA��TA��HA���Aѡ�A� �A�`BA�ffA��AʋDAɴ9A�|�A��A�jA�9XAǙ�A��AƓuA�S�A�+A��mA�JAþwA�-A��;A�ĜA��A�t�A�r�A��A��A��yA���A�A��A�A��/A�Q�A��A��A��`A��A��DA��wA�r�A��HA���A��A���A��wA��mA�+A���A�I�A�A��HA�jA�{A���A�XA��A�^5A���A�I�A�A�G�A���A�S�A�S�A��jA�M�A��A�G�A�+A�Q�A�VA�n�A�p�A��-A�(�A�A�A���A�ĜA�A�I�A~{A}`BAxVAs�FArbAq&�Am\)Al1Ak��Aj�`Afz�Ab�yA]XA\z�AZ��AV(�AQ�AK�hAJȴAHȴAGt�AE&�ACl�AAl�A@(�A>�A=�^A<�A;/A7\)A6Q�A65?A5�#A5
=A3�A3p�A3\)A3�A2�\A1�
A0 �A.�jA-��A,A+33A*JA(=qA&v�A%"�A#�#A"�DA E�A��A�HA��A��A�DA�A�AO�Ar�A�`A��AVA1A��A-A�PA�/AffA�
A��A��A�uA{A{AVA�A(�A�
A
~�A	�A�9Az�A  A`BAVA-A��A��A=qA(�AoA�wAXA&�AA ��A ��@��w@���@�bN@�
=@�-@�\@���@�^5@��`@���@�C�@�`B@��@��`@蛦@���@�@�9X@�hs@�7L@�I�@��H@�j@߶F@޸R@��@ܛ�@�b@�;d@ۍP@��@�1@� �@�1'@ܛ�@�X@ܓu@��m@ܼj@݉7@���@���@�K�@�n�@��@���@٩�@��@�  @׮@�33@��y@�$�@�@�/@�ƨ@���@�5?@ѩ�@��`@ϥ�@θR@ͩ�@���@�I�@�1'@��@�ƨ@˕�@�S�@�o@��y@���@�=q@�G�@ȋD@ǶF@���@ź^@�hs@�Ĝ@�1'@Å@�;d@���@���@��@���@�9X@��@�Ĝ@���@��@��@�z�@�x�@�hs@���@��;@�bN@���@�@�o@�@��@¸R@�$�@�?}@��7@���@�%@��D@���@�j@���@�|�@�S�@���@�n�@���@��-@�&�@� �@���@��#@�@�5?@�~�@��@�E�@���@��h@���@�1@�\)@��R@�^5@��@���@��7@�X@�/@���@�Ĝ@�9X@��P@�+@�o@��y@��R@�{@���@���@�?}@�Ĝ@�r�@�1'@�1@��@�ƨ@�dZ@�@��y@���@�n�@�=q@�$�@�{@��T@�X@�&�@���@�1'@�1@��F@�dZ@��@�v�@�^5@�=q@�@��-@�X@�%@�z�@�1@��@���@�K�@�
=@��!@�{@���@��@�X@�O�@�G�@�/@��@��@�V@�%@��`@��u@� �@���@�t�@�+@��@��R@�n�@��@�@��T@���@���@�r�@�9X@��
@�t�@��y@�5?@���@��@��@��@���@�`B@�7L@��@�V@��/@��/@���@���@���@���@�1'@���@�
=@��R@��R@�~�@�V@�@��-@��@���@�1'@��w@��@�|�@��H@�E�@��@���@�X@�V@���@�Ĝ@�Q�@�  @���@�\)@�"�@��@��\@�V@��@�@��@��`@��9@��D@��@��@�t�@��@���@���@�=q@��@���@�r�@�1'@�1@���@��
@��w@���@�|�@�dZ@�\)@�S�@�@���@�^5@�E�@�-@�{@��@��-@�p�@�G�@��@���@��9@��@��9@��@�1@��F@��@�\)@�S�@�;d@�33@��@�
=@��H@�V@��@���@���@�X@�7L@��/@���@�I�@��@��m@��P@�\)@�;d@���@���@�ff@�E�@��@���@��T@���@���@���@���@�%@��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AҬAҰ!AҴ9Aң�AҲ-Aң�Aҧ�Aҥ�Aҧ�AҴ9Aҥ�Aҡ�Aҡ�Aҡ�Aҟ�Aҥ�Aң�Aҝ�A҃Aҗ�A҃A�z�A�z�A�p�A�l�A�l�A�hsA�r�A�VA�M�A�O�A�G�A�A�A�A�A�9XA�+A�&�A�+A�"�A��A��A��A��A�{A��A��A�oA�oA�{A�VA�JA�JA�%A�A�  A�A�  A���A�A���A���A�  A�A���A���A�  A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A��yA��A��`A��HA��mA��TA��;A��HA��`A��`A��HA��`A��`A��TA��/A��;A��TA��HA��;A��TA��HA��;A��TA��TA��/A��A��
A��A��A���A���A���A���A���A���A���A���A���A���A�AѼjAѴ9Aѥ�A�XA�VA��HA���AХ�A�$�A�I�A�v�A�bA��/AͶFA͕�A�S�A�JA̰!A�r�A�+A���A���A���A���A���A˥�Aˇ+A�jA�S�A�E�A�A�A�9XA�-A�+A�$�A��A��A��A��A��A�oA�bA�1A�%A���A���A��`AʸRA�l�A�1'A�{A���A��HA��
A�ƨAɸRAɰ!AɬAɮAɩ�Aɡ�Aɝ�Aɡ�Aɟ�Aə�Aɏ\AɋDAɁA�dZA�O�A�?}A�1'A� �A�JA�A��HAȺ^Aȧ�Aȡ�AȍPAȁA�n�A�r�A�r�A�n�A�ffA�bNA�bNA�ZA�S�A�Q�A�S�A�K�A�?}A�;dA�;dA�/A�&�A�VA���A��/A���AǺ^AǬAǃA�r�A�ffA�S�A�=qA��A���A��TA���A���A���A�ĜAƶFAƲ-Aư!AƮAƩ�Aƥ�AƑhAƑhAƁA�z�A�z�A�t�A�n�A�dZA�^5A�ZA�O�A�E�A�C�A�A�A�?}A�5?A�1'A�1'A�/A�+A�$�A�"�A�"�A� �A�{A�JA�A���A��A��HA��;A���A�ƨAżjAŰ!Aś�AŋDA�t�A�$�A��#AľwAĩ�Aĕ�Aĉ7A�~�A�l�A�?}A���A�ĜAÝ�A�ffA�9XA�%A���A¡�A�hsA�&�A���A�  A�  A���A��A��A��A��`A��HA��;A��HA��HA��A���A���A���A���A�ƨA���A�ƨA��wA��RA��-A���A���A���A���A���A���A�~�A�ffA�C�A��A��A��A��+A�p�A�XA�Q�A�I�A�5?A��A��/A��-A��7A�n�A�\)A�M�A�A�A�1'A�&�A��A�bA�1A���A��mA���A��^A��A���A���A��7A�n�A�(�A���A��A���A��A���A�r�A�/A�%A���A���A�l�A��A��A�A��9A���A���A��DA��A�x�A�l�A�VA�M�A�I�A�(�A�JA��A��HA���A���A��jA���A�hsA�M�A�"�A��yA���A��uA�^5A�7LA��yA��PA�G�A�"�A�  A��A��RA���A���A��A�^5A�5?A�
=A��A�ȴA���A��DA�|�A�n�A�p�A�n�A�dZA�\)A�K�A�G�A�7LA�7LA�-A�1'A�1'A�&�A� �A��A��A��A�VA�1A��HA���A���A���A��\A�r�A�O�A�;dA���A�ĜA�Q�A��A���A��HA��^A���A��uA�v�A�^5A�Q�A�G�A�/A�"�A�VA���A��A��#A�ƨA��-A��-A���A���A��hA�z�A�^5A�I�A�33A��A�A��mA�ƨA���A��A�VA�-A���A��-A��hA�ffA�M�A�?}A�-A�%A�%A�  A���A��A��HA���A��FA��uA�v�A�\)A�C�A�A���A���A��7A�XA�9XA�A���A��A��
A�ĜA���A�x�A�O�A�$�A��A���A��7A�p�A�G�A� �A���A��yA���A���A��+A��A�~�A�jA�Q�A�&�A��A���A�x�A�n�A�`BA�S�A�G�A�-A��A�
=A���A��A��^A���A���A��\A��A�z�A�t�A�p�A�jA�`BA�VA�O�A��A��#A��hA�oA��FA��uA�t�A�XA�(�A�1A��A��`A��/A���A�ĜA��A��uA��7A�|�A�ffA�Q�A�A�A�33A�-A�(�A� �A��A�
=A��A��A�XA�/A��A�ƨA���A�ZA��;A�t�A�Q�A�&�A��A�%A��A�ȴA���A�|�A�n�A�C�A��A���A���A��7A�&�A��TA���A�ĜA��^A��!A���A���A���A�|�A��PA�ĜA��TA���A�p�A��FA�S�A�  A��`A���A��wA���A�E�A���A�\)A��A�ĜA���A�\)A��A��#A��hA�=qA�hsA��A��
A���A�1'A��TA��DA��;A�|�A�Q�A�7LA��A�oA�A��
A��FA���A���A��+A�t�A�ffA�XA�E�A�+A��A�  A���A�$�A��/A��hA�VA�O�A�E�A�C�A�E�A�G�A�E�A�C�A�C�A�?}A�1'A�$�A�JA���A��`A���A�VA�1'A��A���A��hA�M�A�/A��A��A���A���A�C�A���A�ZA�+A�JA��mA���A�;dA���A�K�A�/A�1A��A�ZA�(�A��A��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                         1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AҲ-Aҧ�Aҧ�Aҧ�Aҡ�Aҟ�A҉7A�x�A�l�A�bNA�O�A�;dA�"�A��A�VA�  A�  A���A���A���A���A��A��A��A��A��A��A��yA��TA��HA���Aѡ�A� �A�`BA�ffA��AʋDAɴ9A�|�A��A�jA�9XAǙ�A��AƓuA�S�A�+A��mA�JAþwA�-A��;A�ĜA��A�t�A�r�A��A��A��yA���A�A��A�A��/A�Q�A��A��A��`A��A��DA��wA�r�A��HA���A��A���A��wA��mA�+A���A�I�A�A��HA�jA�{A���A�XA��A�^5A���A�I�A�A�G�A���A�S�A�S�A��jA�M�A��A�G�A�+A�Q�A�VA�n�A�p�A��-A�(�A�A�A���A�ĜA�A�I�A~{A}`BAxVAs�FArbAq&�Am\)Al1Ak��Aj�`Afz�Ab�yA]XA\z�AZ��AV(�AQ�AK�hAJȴAHȴAGt�AE&�ACl�AAl�A@(�A>�A=�^A<�A;/A7\)A6Q�A65?A5�#A5
=A3�A3p�A3\)A3�A2�\A1�
A0 �A.�jA-��A,A+33A*JA(=qA&v�A%"�A#�#A"�DA E�A��A�HA��A��A�DA�A�AO�Ar�A�`A��AVA1A��A-A�PA�/AffA�
A��A��A�uA{A{AVA�A(�A�
A
~�A	�A�9Az�A  A`BAVA-A��A��A=qA(�AoA�wAXA&�AA ��A ��@��w@���@�bN@�
=@�-@�\@���@�^5@��`@���@�C�@�`B@��@��`@蛦@���@�@�9X@�hs@�7L@�I�@��H@�j@߶F@޸R@��@ܛ�@�b@�;d@ۍP@��@�1@� �@�1'@ܛ�@�X@ܓu@��m@ܼj@݉7@���@���@�K�@�n�@��@���@٩�@��@�  @׮@�33@��y@�$�@�@�/@�ƨ@���@�5?@ѩ�@��`@ϥ�@θR@ͩ�@���@�I�@�1'@��@�ƨ@˕�@�S�@�o@��y@���@�=q@�G�@ȋD@ǶF@���@ź^@�hs@�Ĝ@�1'@Å@�;d@���@���@��@���@�9X@��@�Ĝ@���@��@��@�z�@�x�@�hs@���@��;@�bN@���@�@�o@�@��@¸R@�$�@�?}@��7@���@�%@��D@���@�j@���@�|�@�S�@���@�n�@���@��-@�&�@� �@���@��#@�@�5?@�~�@��@�E�@���@��h@���@�1@�\)@��R@�^5@��@���@��7@�X@�/@���@�Ĝ@�9X@��P@�+@�o@��y@��R@�{@���@���@�?}@�Ĝ@�r�@�1'@�1@��@�ƨ@�dZ@�@��y@���@�n�@�=q@�$�@�{@��T@�X@�&�@���@�1'@�1@��F@�dZ@��@�v�@�^5@�=q@�@��-@�X@�%@�z�@�1@��@���@�K�@�
=@��!@�{@���@��@�X@�O�@�G�@�/@��@��@�V@�%@��`@��u@� �@���@�t�@�+@��@��R@�n�@��@�@��T@���@���@�r�@�9X@��
@�t�@��y@�5?@���@��@��@��@���@�`B@�7L@��@�V@��/@��/@���@���@���@���@�1'@���@�
=@��R@��R@�~�@�V@�@��-@��@���@�1'@��w@��@�|�@��H@�E�@��@���@�X@�V@���@�Ĝ@�Q�@�  @���@�\)@�"�@��@��\@�V@��@�@��@��`@��9@��D@��@��@�t�@��@���@���@�=q@��@���@�r�@�1'@�1@���@��
@��w@���@�|�@�dZ@�\)@�S�@�@���@�^5@�E�@�-@�{@��@��-@�p�@�G�@��@���@��9@��@��9@��@�1@��F@��@�\)@�S�@�;d@�33@��@�
=@��H@�V@��@���@���@�X@�7L@��/@���@�I�@��@��m@��P@�\)@�;d@���@���@�ff@�E�@��@���@��T@���@���@���@���@�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AҬAҰ!AҴ9Aң�AҲ-Aң�Aҧ�Aҥ�Aҧ�AҴ9Aҥ�Aҡ�Aҡ�Aҡ�Aҟ�Aҥ�Aң�Aҝ�A҃Aҗ�A҃A�z�A�z�A�p�A�l�A�l�A�hsA�r�A�VA�M�A�O�A�G�A�A�A�A�A�9XA�+A�&�A�+A�"�A��A��A��A��A�{A��A��A�oA�oA�{A�VA�JA�JA�%A�A�  A�A�  A���A�A���A���A�  A�A���A���A�  A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A��yA��A��`A��HA��mA��TA��;A��HA��`A��`A��HA��`A��`A��TA��/A��;A��TA��HA��;A��TA��HA��;A��TA��TA��/A��A��
A��A��A���A���A���A���A���A���A���A���A���A���A�AѼjAѴ9Aѥ�A�XA�VA��HA���AХ�A�$�A�I�A�v�A�bA��/AͶFA͕�A�S�A�JA̰!A�r�A�+A���A���A���A���A���A˥�Aˇ+A�jA�S�A�E�A�A�A�9XA�-A�+A�$�A��A��A��A��A��A�oA�bA�1A�%A���A���A��`AʸRA�l�A�1'A�{A���A��HA��
A�ƨAɸRAɰ!AɬAɮAɩ�Aɡ�Aɝ�Aɡ�Aɟ�Aə�Aɏ\AɋDAɁA�dZA�O�A�?}A�1'A� �A�JA�A��HAȺ^Aȧ�Aȡ�AȍPAȁA�n�A�r�A�r�A�n�A�ffA�bNA�bNA�ZA�S�A�Q�A�S�A�K�A�?}A�;dA�;dA�/A�&�A�VA���A��/A���AǺ^AǬAǃA�r�A�ffA�S�A�=qA��A���A��TA���A���A���A�ĜAƶFAƲ-Aư!AƮAƩ�Aƥ�AƑhAƑhAƁA�z�A�z�A�t�A�n�A�dZA�^5A�ZA�O�A�E�A�C�A�A�A�?}A�5?A�1'A�1'A�/A�+A�$�A�"�A�"�A� �A�{A�JA�A���A��A��HA��;A���A�ƨAżjAŰ!Aś�AŋDA�t�A�$�A��#AľwAĩ�Aĕ�Aĉ7A�~�A�l�A�?}A���A�ĜAÝ�A�ffA�9XA�%A���A¡�A�hsA�&�A���A�  A�  A���A��A��A��A��`A��HA��;A��HA��HA��A���A���A���A���A�ƨA���A�ƨA��wA��RA��-A���A���A���A���A���A���A�~�A�ffA�C�A��A��A��A��+A�p�A�XA�Q�A�I�A�5?A��A��/A��-A��7A�n�A�\)A�M�A�A�A�1'A�&�A��A�bA�1A���A��mA���A��^A��A���A���A��7A�n�A�(�A���A��A���A��A���A�r�A�/A�%A���A���A�l�A��A��A�A��9A���A���A��DA��A�x�A�l�A�VA�M�A�I�A�(�A�JA��A��HA���A���A��jA���A�hsA�M�A�"�A��yA���A��uA�^5A�7LA��yA��PA�G�A�"�A�  A��A��RA���A���A��A�^5A�5?A�
=A��A�ȴA���A��DA�|�A�n�A�p�A�n�A�dZA�\)A�K�A�G�A�7LA�7LA�-A�1'A�1'A�&�A� �A��A��A��A�VA�1A��HA���A���A���A��\A�r�A�O�A�;dA���A�ĜA�Q�A��A���A��HA��^A���A��uA�v�A�^5A�Q�A�G�A�/A�"�A�VA���A��A��#A�ƨA��-A��-A���A���A��hA�z�A�^5A�I�A�33A��A�A��mA�ƨA���A��A�VA�-A���A��-A��hA�ffA�M�A�?}A�-A�%A�%A�  A���A��A��HA���A��FA��uA�v�A�\)A�C�A�A���A���A��7A�XA�9XA�A���A��A��
A�ĜA���A�x�A�O�A�$�A��A���A��7A�p�A�G�A� �A���A��yA���A���A��+A��A�~�A�jA�Q�A�&�A��A���A�x�A�n�A�`BA�S�A�G�A�-A��A�
=A���A��A��^A���A���A��\A��A�z�A�t�A�p�A�jA�`BA�VA�O�A��A��#A��hA�oA��FA��uA�t�A�XA�(�A�1A��A��`A��/A���A�ĜA��A��uA��7A�|�A�ffA�Q�A�A�A�33A�-A�(�A� �A��A�
=A��A��A�XA�/A��A�ƨA���A�ZA��;A�t�A�Q�A�&�A��A�%A��A�ȴA���A�|�A�n�A�C�A��A���A���A��7A�&�A��TA���A�ĜA��^A��!A���A���A���A�|�A��PA�ĜA��TA���A�p�A��FA�S�A�  A��`A���A��wA���A�E�A���A�\)A��A�ĜA���A�\)A��A��#A��hA�=qA�hsA��A��
A���A�1'A��TA��DA��;A�|�A�Q�A�7LA��A�oA�A��
A��FA���A���A��+A�t�A�ffA�XA�E�A�+A��A�  A���A�$�A��/A��hA�VA�O�A�E�A�C�A�E�A�G�A�E�A�C�A�C�A�?}A�1'A�$�A�JA���A��`A���A�VA�1'A��A���A��hA�M�A�/A��A��A���A���A�C�A���A�ZA�+A�JA��mA���A�;dA���A�K�A�/A�1A��A�ZA�(�A��A��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                         1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
.B
/OB
.B
.}B
.}B
.}B
,�B
.IB
.IB
.}B
-B
.�B
-�B
-�B
-�B
.B
-�B
-�B
-�B
-�B
-�B
.B
.B
.}B
.IB
.}B
.IB
.�B
/OB
/�B
0�B
6�B
�IB*0B>�B=BG�BV9BZQBn/Bv+B|�B�FB��B�*B�'B�tB͟B�&B��BB�BCB�B(�B+�B)*B,B.�B.B/OB3�B:*B?HB=�B<�BA BH�BHKBH�BMBUgBT�B\)Bd&Bg�Bk�Bv+Bw�BxBw2B}�Bv�Br�Bl"BgmBS�BD�B3�BeB	�B�&B��B�RB��BqABdZB^jBV�BC�B@OB9�B$�BuB
�JB
ޞB
�B
��B
�_B
��B
��B
�4B
^�B
Z�B
K�B
#�B
$B
�B
�B	�5B	��B	�
B	��B	�B	�@B	��B	��B	w2B	s�B	Q�B	HB	A�B	<jB	8�B	.}B	,B	%zB	!B	�B	@B	:B	�B�	B�B�DB��B�fB�B�B�cB�B��B��B�HB��B��B�BچB�B��B��B�0B��B��B�6B�qB�wB�qB��B��B��B�B��B� B��B�FB��B�3B��B��B�B��B�tB��BĜB��B�B�B�B�B�TB�B��B	B	JB	�B	�B	@B	�B	kB	IB	!�B	�B	�B	$�B	(�B	(�B	)�B	)_B	)�B	(�B	-�B	*�B	(�B	&�B	"�B	�B	�B	�B	B	oB	oB	 �B��B��B��B��B��B	  B	�B	!�B	�B	=B	1B	B	oB	hB	�B	B	�B	$tB	,B	,�B	-wB	0!B	4�B	>B	=B	<�B	C�B	Q�B	U2B	Z�B	\�B	aHB	c�B	d&B	e�B	gB	m�B	qvB	t�B	u�B	v+B	y	B	{�B	{�B	{B	{�B	}�B	}"B	cB	~]B	.B	�iB	�;B	�oB	��B	��B	��B	��B	�oB	�;B	��B	��B	�GB	�{B	��B	��B	�~B	�~B	��B	��B	�_B	��B	��B	�CB	��B	��B	��B	�hB	�B	�B	��B	��B	�0B	��B	��B	�B	��B	�B	��B	�9B	��B	�5B	�B	�B	�B	�B	��B	�TB	��B	� B	�8B	��B	��B	��B	��B	��B	��B	�B	� B	��B	�)B	��B	��B	�AB	�B	�|B	�	B	�B	��B	�rB	��B	��B	�.B	��B	�VB	�(B	��B
 �B
�B
uB
GB
�B
�B
�B
+B
_B
_B
�B
	7B
�B
�B
	B
	�B

�B
�B
�B
B
B
PB
�B
�B
�B
VB
VB
VB
VB
�B
�B
�B
�B
 B
�B
hB
hB
oB
�B
�B
�B
uB
�B
{B
MB
�B
�B
�B
YB
�B
�B
�B
B
qB
qB
�B
B
B
CB
xB
CB
CB
CB
xB
IB
�B
VB
�B
�B
 �B
 �B
!-B
!�B
!�B
!�B
"�B
#nB
$B
#�B
$tB
$tB
%zB
&LB
&B
&B
%�B
%�B
&�B
&�B
&�B
&�B
'B
'�B
'RB
'RB
'B
'B
'RB
'�B
)_B
+B
,B
+�B
,qB
,qB
,�B
,�B
.B
.}B
/B
/�B
/�B
0!B
1�B
2-B
2�B
2�B
3�B
49B
3�B
49B
5?B
5�B
6zB
6zB
6�B
7�B
7�B
7�B
8�B
8�B
9�B
:�B
:^B
:�B
;�B
;�B
<jB
<�B
<�B
<jB
=�B
>BB
?}B
?HB
?B
>�B
>�B
?HB
?HB
?}B
?�B
?�B
?�B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
AUB
@�B
@�B
@�B
A�B
A�B
B'B
B'B
B[B
A�B
B'B
A�B
B'B
A�B
B[B
B�B
C-B
C-B
CaB
C�B
DgB
DgB
D�B
E�B
E�B
FtB
G�B
G�B
HKB
H�B
H�B
IRB
I�B
I�B
I�B
J#B
I�B
J#B
J#B
JXB
K^B
J�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
,qB
0!B
-�B
2�B
*�B
.B
-B
.�B
,�B
)�B
/�B
.}B
/�B
.IB
/B
,=B
-B
/OB
/�B
*�B
+B
0!B
/B
-�B
/OB
-�B
-CB
%B
6�B
-B
-B
-�B
/OB
.B
.}B
1�B
-�B
-B
/B
/B
-�B
,�B
.}B
.�B
,qB
-CB
.�B
-B
-B
.IB
/B
-B
-CB
.�B
.IB
-B
-�B
.IB
-B
-�B
.�B
-wB
,qB
.B
/B
-CB
-B
.B
.}B
-B
-�B
.�B
-�B
-B
.}B
.�B
-CB
-CB
.}B
.�B
,�B
,�B
/�B
.�B
.}B
-wB
-B
.�B
-CB
-CB
/OB
.}B
-CB
-�B
/OB
-B
-CB
/OB
.IB
-CB
.�B
/OB
.}B
-wB
/�B
-CB
/�B
.�B
-�B
-�B
/OB
.�B
-CB
.}B
/OB
.}B
-CB
.�B
/�B
.B
-wB
/OB
/B
-CB
.B
/B
.B
-�B
/B
.�B
-B
.IB
/OB
.}B
-wB
.�B
/�B
-�B
-wB
/OB
0!B
.IB
/OB
0�B
/�B
.IB
/B
0!B
/B
.B
.�B
1'B
0�B
/OB
/OB
/�B
.}B
/�B
0�B
.IB
.�B
0�B
1�B
1'B
/�B
/�B
1�B
1[B
/�B
0�B
1�B
1�B
0!B
/�B
0�B
/�B
/�B
1'B
2aB
2�B
G�B
O�B
Y�B
Z�B
_;B
{�B
�[B
��B
�B
�WB
��B
�rBYBB)*B+kB8�B5B=B9�B6FB9�B@�B@�B@OB>�B@OB=�B>B?HB>B=<B=�B>B<B;�B=B>wB=B;�B;dB=<B=<B>wBH�BK�BO�BO�BT�BT,BT�BV�BV9BWsBVmBU2BV9BXEBXBU�BUgBW?BXEBYKBYB]dB^�BaB`vBc�BiDBjBs�BrGBuZBr|Bv�BtBxlBt�Bt�Bt�Bv+BwfBv+Bw�BzBy�Bx�B|B}�B}VBz�B}�B~�B��B�%B�lB�~B��B��B�	B��B��B��B��B��B��B�[B�3B�-B�-B�3B�LB�tB�?B�?B��B�B��B�XB�B��B�qB��B�BB��B��B��B��B�gB�gB�3B��B��BƨB�9B�mB��B�B�B�BƨBɆB��B�^B�XB�6B��B�<B�BѷB� BӏB��B�gB��B�B��B��B�)B��B�5B��B��B��B�]B��B�B�cB	�B�B\BFBeB!BOB�B�B7BCBB�BBB�B	B�BCB�B�B=BCB~B=B=B�BBIBB�BBIB�B�B �B �B#:B$B&�B.IB%�B+B&�B%FB&�B+B*�B1'B,=B,B-wB,=B*�B)�B)�B*eB*0B)�B'RB'�B*�B)�B)_B'�B&LB&�B%�B(�B/B=<B0!B+�B-�B-B,qB0�B+�B2aB.�B1�B5B1[B2-B,�B,qB,�B.�B.�B,B-�B.IB/B/B3�B/OB0UB1[B0!B-CB.}B3hB2-B1�B49B3hB0�B:�B2-B7B;�B:�B<B4�B9XB9XB=qB8RB:�B8RBA BD3B<�B@OBEBA�B>B@�BA�B>wB<�B?B9XBB'B<jBA B<jB?B<�B;�B>BB<6B?HB;�B:�B<�B;�BH�BC�BF?B<6B>�BB�B>wBB�B?}BJ#BQ�BD3BEmBEBK)BD�BFtBI�BGzBF�BD3BG�BG�BJ�BE�BGzBK^BHBH�BEBH�BH�BGEBGzBIBJ�BH�BJ�BJ�BN<BNBL0BK)BNpBR�BT,BVBS&BQ�BU�BR�BS&BTaBR�BR�BS[BVBVBUgBT�BW
BW�BV�BWsBa�BcTBZ�B]/BaB`Be�B`�Bb�Bd�BffBh�Bh
Bc�Bg8Bh
Bn�Be�Bf�BkBh
Bj�Bg�BjKBpoBo�Bl�Bm]BpBr|Bs�B�B|PBv+Bt�Bv�Bu�By>BxBv�Bx8By	B{JBzBw�Bu�Bv�BxlBw�Bv�Bu%Bu%Bu�Bu%Bt�B�B|�B�4B�YB~�B{By>BzDB{ByrBx�Bv�Bt�Bt�BuZBv`Bw�BsBs�BsMBp�BoiBoiBncBm�BkBlWBh>Bi�Bt�Be�Bm)Be,Bd�B_Ba�Bn�BW
BS[BR�BJ#BF�BG�BHKBEmBC-BB'BC�B?�B9�B9�B:*B6FB-�B$@B �BxB�B1B�BB�B%�B 'B�B�B�B�	B��BܒB˒B�dB�mBÖB҉B��B��B�dB�qB��B�=B��B��B��B�\B��B��B��B��B�(B��B�B��Bv+Bm)Bk�Bf�Bh�Be�Bf�Bc Bc�B`vB`BBa�B_pB_�B^�B[�BYBW
BjKBWsBU2BX�BXyBEBH�BC�BB�BB�BA�BB�B@B@BB�BB�BAUB>BB=B>�B>wB4�B3hB7�B1�B-�B#:BVB#:B�BB"�B#nB�BB�B�B1B �B
=B
�DB
�QB
�B
�B
�B
��B
��B
�Q444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                         4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                         4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022042701172820220427011728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022050622313520220506223135QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022050622313520220506223135QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                