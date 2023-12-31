CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  -   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-10-15T15:02:54Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  Vp   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  \�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  v4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  |�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  Հ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \ P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \ (   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h .p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` G�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   H8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   N8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   T8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T Z8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   Z�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   Z�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   Z�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   Z�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � Z�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   [,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   [H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    [P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        [p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        [x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       [�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    [�Argo profile    3.1 1.2 19500101000000  20221015150254  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_232                 6810_008521_232                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��c{t�@��c{t�11  @��c���@��c���@2��g�@2��g��d�#ᆘ5�d�#ᆘ511  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @@  @�G�@�G�@�G�@�G�@��RA�RA\)A,��A@  A`  A�  A�  A�Q�A�Q�A�  AϮA�  A�  B   B(�B  B�
B�
B(  B0  B8Q�B?�
BG�
BP  BX(�B`(�Bg�
Bp  Bw�
B�
B�  B�(�B�{B�{B�(�B�  B��
B��
B�  B�{B�(�B�  B��B�  B�{B�{B��B�  B�  B��B�  B�  B�(�B�(�B��B��B�{B�{B��B�{B�(�C   C
=C�C��C  C
  C  C��C��C  C  C��C  C  C  C
=C 
=C"  C$  C&
=C(
=C*  C+��C.  C0
=C2
=C4
=C5��C7�C9��C;�C>
=C?��CA��CC��CF  CH
=CI��CL  CN  CP  CR
=CT  CV  CW��CZ  C\  C^  C`  Cb  Cc��Cf
=Cg��Cj  Cl  Cn  Cp  Cr  Ct
=Cv  Cx
=Cz
=C{��C}�C�  C�C�C�C�C�
=C�
=C�C�  C���C�  C�C���C���C�  C�C�C�
=C�  C�  C�C�  C���C�  C�  C���C�  C���C���C�  C���C���C�  C�C�
=C�
=C�  C���C���C���C�  C�  C�C�
=C�C�C�  C�  C�C�  C�  C�  C�  C���C�  C�C�  C���C�  C�
=C�  C�C�C�  C���C���C�C�C�C�  C�  C�C�C���C���C�  C�C�
=C�C���C�  C�\C�C�  C���C���C���C�  C���C���C�  C�
=C�C���C���C�  C�  C���C�C�C�C�  C���C���C���C�  C���C���C���C�C�
=C�  C�C�C���C���C�  C�
=C�
=C�
=C�  C�C�C���C�  C���C���C�  D   D � D�D��D  D� D�D�D�D� D�D��DD��D  D��D�D��D	�D	��D
�D
}qD  D� D  D�D  D� D��D� DD��D  D� D�D��D�qD� D�D��D�qDz�D  D�D  D}qD  D��D�qD}qD�qD� D�D��DD��D�D��D  D��D  D� D�D�D �D � D!  D!��D"  D"� D#  D#��D$�D$� D%  D%� D&  D&��D'  D'� D'�qD(z�D(�qD)� D*  D*��D+�D+�D,�D,� D,�qD-� D.�D.}qD.�qD/� D/�qD0}qD0�qD1� D1�qD2}qD3�D3}qD4  D4��D5D5��D6  D6� D7  D7}qD8  D8��D9D9��D:  D:��D;�D;��D<  D<� D=  D=� D>�D>��D>�qD?}qD?�qD@� DA  DA� DB�DB��DC�DC}qDC��DDz�DE  DE� DF  DF� DG�DG��DG�qDH� DI  DI}qDI�qDJ� DK  DK}qDK��DL}qDM  DM}qDN  DN��DO�DO�DP�DP� DP��DQ}qDQ�qDR� DS  DSz�DS��DT}qDT��DUz�DU��DV}qDW  DW}qDW�qDX� DY�DY}qDY�qDZ� D[  D[��D\�D\��D]�D]� D^  D^xRD^�qD_��D`  D`� D`�qDa}qDb�Db��Dc  Dc}qDc�qDd� De  De}qDe�qDf� Df�qDgz�Dg��Dh}qDi  Di��Dj  Dj� Dk  Dk� Dl  Dl��DmDm��Dn  Dn��Dn�qDo}qDp  Dp� Dq�Dq��Dr  Dr}qDs  Ds��Dt  Dt��Du�Du��Dv�Dv� Dw  Dw��Dx�Dx�DyDy� Dy�qDz}qD{  D{� D{�qD|� D}�D}� D~�D~� D~�qD��D�HD�AHD�~�D���D���D�>�D�~�D���D�HD�AHD��HD��HD��D�AHD��HD��HD�HD�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�AHD��HD���D���D�=qD�~�D���D���D�@ D��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D���D��qD�=qD�� D��HD�D�AHD��HD�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�>�D�� D�� D�  D�@ D�� D���D���D�@ D��HD��HD��fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?8Q�?�  ?���?Ǯ?�@�@(�@333@=p�@O\)@h��@s33@�ff@�\)@�
=@��
@���@�z�@��R@��
@�\)@�
=@�G�@�@��@��RA�\AQ�A(�A  AffA��A{A#33A%A+�A0  A2�\A8Q�A:�HA@��AC�
AI��AL(�AP��AUAXQ�A]p�A`��Ae�Aj=qAl��Aq�AvffAy��A\)A���A��
A��RA�  A�33A�{A��A��HA��A��A�=qA�(�A��A�G�A�z�A�ffA���A�(�A�{A���A��
A�A���A��A�p�A���A��HA��A�Q�A�=qA���A�\)A���A�(�A�p�A�Q�A��HA�z�A߮AᙚA�A�RA�Q�A��HA��A�
=A��A�A�ffA�  A��\A�p�A�\)B ��BffB33BQ�B{B�HB(�B	p�B
�\B�
B��BffB\)Bz�B{B�\B(�B�B{B�Bz�BB
=B  BG�B=qB�
B ��B"{B#
=B$(�B%B&�RB(  B)G�B*{B+�
B,��B-B/\)B0(�B1p�B2�HB3�
B5p�B6{B7�B8��B:{B;�B<Q�B>{B>�RB@(�BA��BBffBD  BD��BF{BG�BH��BIBK33BL(�BMBN�RBP(�BQG�BR=qBS�
BT��BV{BW\)BX(�BYBZ�RB[�
B]p�B^=qB_�B`��Ba��Bc33Bd  Be�BfffBg\)Bh��Bj{Bj�HBlz�BmBn�\Bp  Bq�Br=qBs�Bt��Bu�Bw
=Bx��ByBz�RB|Q�B}p�B~ffB�  B�z�B���B��B�Q�B���B���B�{B���B�p�B�  B�z�B�\)B�B�ffB��B���B�Q�B�
=B��B�Q�B���B�p�B�(�B��\B�\)B��B�ffB�G�B��B�Q�B��B��B�{B��HB�p�B��
B���B�G�B��B�z�B��B���B�ffB���B�p�B�Q�B��RB�p�B�(�B�z�B�G�B��B�ffB��B��
B�=qB���B���B�{B���B��B��B�z�B�33B��
B�=qB�
=B���B�{B���B�p�B�  B��HB��B��B���B�p�B�  B��\B�\)B�  B��\B�33B�{B��\B��B��B���B��B��
B��\B�
=B�B��\B�
=B��B�z�B���B��B�ffB��HB�p�B�=qB���B�p�B�  B¸RBÅB�  BĸRBŅB�  BƏ\B�\)B�{Bȣ�B�33B�  Bʏ\B��B��
B̏\B�
=BͮB�z�B���Bϙ�B�ffB��HB�p�B�=qB���BӅB�{B��HBՅB�{B֣�B�G�B�{BظRB�33B�Bڏ\B�33B�B�z�B�33BݮB�=qB��Bߙ�B�(�B���B�B�(�B��HB㙚B�  B��B�B�{B��B�33B�  B��B��B��B��B��B�B�z�B��B�B�(�B�
=BB�{B��HB�B�(�B�RB�p�B�=qB���B�G�B�{B���B�\)B��B���B�p�B�  B���B�\)B�{B���B�33B��B���B�33B�C =qC ��C �
C{Cp�C��C  C=qC��C�
C  CQ�C�\C�RC��C=qC\)C�\C��C  C{C=qCz�C��C�C�
C
=C33CG�Cp�C�CC�HC�CG�C\)Cz�C�C�HC��C{CQ�Cp�C�C�RC��C	
=C	(�C	ffC	�\C	��C	��C
  C
{C
=qC
z�C
�\C
�C
�C
=C(�CffC�C��C�C
=C33Cp�C�C��C�C
=C�CffC�\C��C�
C
=C(�CQ�C�\C��C��C
=C(�CG�C�\C�C��C
=C=qCG�C��C�RC�
C�C=qC\)C��C�
C�C33Cp�C�\C�RC�HC(�CQ�C�C��C�C{CffC�\C�RC  C(�CG�C��CC�HC33C\)C�C�
C  C33Cz�C��C�HC
=C33C�C��C�
C�C=qCp�C�RC�C
=CG�C�\C�C�C�C\)Cz�C�RC  C(�CQ�C��C�
C��C33Cp�C�\C�
C
=C33Cz�C�RC�HC
=CQ�C�C�C�
C {C \)C �\C �RC �HC!
=C!Q�C!�C!��C!��C"{C"Q�C"p�C"�\C"��C#
=C#=qC#\)C#��C#�
C#�C$(�C$ffC$�\C$�C$�HC%�C%\)C%p�C%��C%�
C&
=C&(�C&G�C&��C&��C&�C'
=C'=qC'z�C'��C'�RC'��C((�C(\)C(�\C(�RC(�HC)
=C)=qC)Q�C)�\C)C)��C*�C*=qC*\)C*�\C*��C*��C+(�C+\)C+�C+�C+�
C,  C,(�C,Q�C,z�C,�RC,�C-�C-Q�C-z�C-��C-�RC-�C.(�C.Q�C.ffC.�\C.C.��C/(�C/Q�C/ffC/�\C/�C/�HC0{C0G�C0ffC0��C0�C0��C0��C1(�C1\)C1z�C1��C1�RC1�
C2  C233C2\)C2�C2�C2�RC2�C3{C3Q�C3�C3��C3C3�C4{C433C4p�C4��C4��C5  C5(�C5=qC5ffC5��C5��C5��C6�C6Q�C6�C6�C6�
C7
=C7(�C7Q�C7p�C7��C7�RC7�HC8
=C8=qC8p�C8��C8�
C9  C933C9\)C9z�C9��C9��C:  C:33C:\)C:��C:C:��C;{C;=qC;ffC;�\C;�RC;�HC<{C<G�C<ffC<��C<�
C=  C=33C=\)C=�\C=C=�C>�C>Q�C>�C>�C>�
C>��C?(�C?Q�C?z�C?�C?�HC@{C@=qC@p�C@��C@��CA  CA33CAffCA��CA��CA��CB�CBQ�CBz�CB�CB�CC{CCG�CCz�CC�CC�HCD
=CD=qCDz�CD��CD�
CE
=CE=qCEp�CE��CE�HCF{CF=qCFz�CF�CF�HCG{CGG�CGz�CG�CG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?u@   @@  @�G�@�G�@�G�@�G�@��RA�RA\)A,��A@  A`  A�  A�  A�Q�A�Q�A�  AϮA�  A�  B   B(�B  B�
B�
B(  B0  B8Q�B?�
BG�
BP  BX(�B`(�Bg�
Bp  Bw�
B�
B�  B�(�B�{B�{B�(�B�  B��
B��
B�  B�{B�(�B�  B��B�  B�{B�{B��B�  B�  B��B�  B�  B�(�B�(�B��B��B�{B�{B��B�{B�(�C   C
=C�C��C  C
  C  C��C��C  C  C��C  C  C  C
=C 
=C"  C$  C&
=C(
=C*  C+��C.  C0
=C2
=C4
=C5��C7�C9��C;�C>
=C?��CA��CC��CF  CH
=CI��CL  CN  CP  CR
=CT  CV  CW��CZ  C\  C^  C`  Cb  Cc��Cf
=Cg��Cj  Cl  Cn  Cp  Cr  Ct
=Cv  Cx
=Cz
=C{��C}�C�  C�C�C�C�C�
=C�
=C�C�  C���C�  C�C���C���C�  C�C�C�
=C�  C�  C�C�  C���C�  C�  C���C�  C���C���C�  C���C���C�  C�C�
=C�
=C�  C���C���C���C�  C�  C�C�
=C�C�C�  C�  C�C�  C�  C�  C�  C���C�  C�C�  C���C�  C�
=C�  C�C�C�  C���C���C�C�C�C�  C�  C�C�C���C���C�  C�C�
=C�C���C�  C�\C�C�  C���C���C���C�  C���C���C�  C�
=C�C���C���C�  C�  C���C�C�C�C�  C���C���C���C�  C���C���C���C�C�
=C�  C�C�C���C���C�  C�
=C�
=C�
=C�  C�C�C���C�  C���C���C�  D   D � D�D��D  D� D�D�D�D� D�D��DD��D  D��D�D��D	�D	��D
�D
}qD  D� D  D�D  D� D��D� DD��D  D� D�D��D�qD� D�D��D�qDz�D  D�D  D}qD  D��D�qD}qD�qD� D�D��DD��D�D��D  D��D  D� D�D�D �D � D!  D!��D"  D"� D#  D#��D$�D$� D%  D%� D&  D&��D'  D'� D'�qD(z�D(�qD)� D*  D*��D+�D+�D,�D,� D,�qD-� D.�D.}qD.�qD/� D/�qD0}qD0�qD1� D1�qD2}qD3�D3}qD4  D4��D5D5��D6  D6� D7  D7}qD8  D8��D9D9��D:  D:��D;�D;��D<  D<� D=  D=� D>�D>��D>�qD?}qD?�qD@� DA  DA� DB�DB��DC�DC}qDC��DDz�DE  DE� DF  DF� DG�DG��DG�qDH� DI  DI}qDI�qDJ� DK  DK}qDK��DL}qDM  DM}qDN  DN��DO�DO�DP�DP� DP��DQ}qDQ�qDR� DS  DSz�DS��DT}qDT��DUz�DU��DV}qDW  DW}qDW�qDX� DY�DY}qDY�qDZ� D[  D[��D\�D\��D]�D]� D^  D^xRD^�qD_��D`  D`� D`�qDa}qDb�Db��Dc  Dc}qDc�qDd� De  De}qDe�qDf� Df�qDgz�Dg��Dh}qDi  Di��Dj  Dj� Dk  Dk� Dl  Dl��DmDm��Dn  Dn��Dn�qDo}qDp  Dp� Dq�Dq��Dr  Dr}qDs  Ds��Dt  Dt��Du�Du��Dv�Dv� Dw  Dw��Dx�Dx�DyDy� Dy�qDz}qD{  D{� D{�qD|� D}�D}� D~�D~� D~�qD��D�HD�AHD�~�D���D���D�>�D�~�D���D�HD�AHD��HD��HD��D�AHD��HD��HD�HD�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�AHD��HD���D���D�=qD�~�D���D���D�@ D��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D���D��qD�=qD�� D��HD�D�AHD��HD�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�>�D�� D�� D�  D�@ D�� D���D���D�@ D��HD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?8Q�?�  ?���?Ǯ?�@�@(�@333@=p�@O\)@h��@s33@�ff@�\)@�
=@��
@���@�z�@��R@��
@�\)@�
=@�G�@�@��@��RA�\AQ�A(�A  AffA��A{A#33A%A+�A0  A2�\A8Q�A:�HA@��AC�
AI��AL(�AP��AUAXQ�A]p�A`��Ae�Aj=qAl��Aq�AvffAy��A\)A���A��
A��RA�  A�33A�{A��A��HA��A��A�=qA�(�A��A�G�A�z�A�ffA���A�(�A�{A���A��
A�A���A��A�p�A���A��HA��A�Q�A�=qA���A�\)A���A�(�A�p�A�Q�A��HA�z�A߮AᙚA�A�RA�Q�A��HA��A�
=A��A�A�ffA�  A��\A�p�A�\)B ��BffB33BQ�B{B�HB(�B	p�B
�\B�
B��BffB\)Bz�B{B�\B(�B�B{B�Bz�BB
=B  BG�B=qB�
B ��B"{B#
=B$(�B%B&�RB(  B)G�B*{B+�
B,��B-B/\)B0(�B1p�B2�HB3�
B5p�B6{B7�B8��B:{B;�B<Q�B>{B>�RB@(�BA��BBffBD  BD��BF{BG�BH��BIBK33BL(�BMBN�RBP(�BQG�BR=qBS�
BT��BV{BW\)BX(�BYBZ�RB[�
B]p�B^=qB_�B`��Ba��Bc33Bd  Be�BfffBg\)Bh��Bj{Bj�HBlz�BmBn�\Bp  Bq�Br=qBs�Bt��Bu�Bw
=Bx��ByBz�RB|Q�B}p�B~ffB�  B�z�B���B��B�Q�B���B���B�{B���B�p�B�  B�z�B�\)B�B�ffB��B���B�Q�B�
=B��B�Q�B���B�p�B�(�B��\B�\)B��B�ffB�G�B��B�Q�B��B��B�{B��HB�p�B��
B���B�G�B��B�z�B��B���B�ffB���B�p�B�Q�B��RB�p�B�(�B�z�B�G�B��B�ffB��B��
B�=qB���B���B�{B���B��B��B�z�B�33B��
B�=qB�
=B���B�{B���B�p�B�  B��HB��B��B���B�p�B�  B��\B�\)B�  B��\B�33B�{B��\B��B��B���B��B��
B��\B�
=B�B��\B�
=B��B�z�B���B��B�ffB��HB�p�B�=qB���B�p�B�  B¸RBÅB�  BĸRBŅB�  BƏ\B�\)B�{Bȣ�B�33B�  Bʏ\B��B��
B̏\B�
=BͮB�z�B���Bϙ�B�ffB��HB�p�B�=qB���BӅB�{B��HBՅB�{B֣�B�G�B�{BظRB�33B�Bڏ\B�33B�B�z�B�33BݮB�=qB��Bߙ�B�(�B���B�B�(�B��HB㙚B�  B��B�B�{B��B�33B�  B��B��B��B��B��B�B�z�B��B�B�(�B�
=BB�{B��HB�B�(�B�RB�p�B�=qB���B�G�B�{B���B�\)B��B���B�p�B�  B���B�\)B�{B���B�33B��B���B�33B�C =qC ��C �
C{Cp�C��C  C=qC��C�
C  CQ�C�\C�RC��C=qC\)C�\C��C  C{C=qCz�C��C�C�
C
=C33CG�Cp�C�CC�HC�CG�C\)Cz�C�C�HC��C{CQ�Cp�C�C�RC��C	
=C	(�C	ffC	�\C	��C	��C
  C
{C
=qC
z�C
�\C
�C
�C
=C(�CffC�C��C�C
=C33Cp�C�C��C�C
=C�CffC�\C��C�
C
=C(�CQ�C�\C��C��C
=C(�CG�C�\C�C��C
=C=qCG�C��C�RC�
C�C=qC\)C��C�
C�C33Cp�C�\C�RC�HC(�CQ�C�C��C�C{CffC�\C�RC  C(�CG�C��CC�HC33C\)C�C�
C  C33Cz�C��C�HC
=C33C�C��C�
C�C=qCp�C�RC�C
=CG�C�\C�C�C�C\)Cz�C�RC  C(�CQ�C��C�
C��C33Cp�C�\C�
C
=C33Cz�C�RC�HC
=CQ�C�C�C�
C {C \)C �\C �RC �HC!
=C!Q�C!�C!��C!��C"{C"Q�C"p�C"�\C"��C#
=C#=qC#\)C#��C#�
C#�C$(�C$ffC$�\C$�C$�HC%�C%\)C%p�C%��C%�
C&
=C&(�C&G�C&��C&��C&�C'
=C'=qC'z�C'��C'�RC'��C((�C(\)C(�\C(�RC(�HC)
=C)=qC)Q�C)�\C)C)��C*�C*=qC*\)C*�\C*��C*��C+(�C+\)C+�C+�C+�
C,  C,(�C,Q�C,z�C,�RC,�C-�C-Q�C-z�C-��C-�RC-�C.(�C.Q�C.ffC.�\C.C.��C/(�C/Q�C/ffC/�\C/�C/�HC0{C0G�C0ffC0��C0�C0��C0��C1(�C1\)C1z�C1��C1�RC1�
C2  C233C2\)C2�C2�C2�RC2�C3{C3Q�C3�C3��C3C3�C4{C433C4p�C4��C4��C5  C5(�C5=qC5ffC5��C5��C5��C6�C6Q�C6�C6�C6�
C7
=C7(�C7Q�C7p�C7��C7�RC7�HC8
=C8=qC8p�C8��C8�
C9  C933C9\)C9z�C9��C9��C:  C:33C:\)C:��C:C:��C;{C;=qC;ffC;�\C;�RC;�HC<{C<G�C<ffC<��C<�
C=  C=33C=\)C=�\C=C=�C>�C>Q�C>�C>�C>�
C>��C?(�C?Q�C?z�C?�C?�HC@{C@=qC@p�C@��C@��CA  CA33CAffCA��CA��CA��CB�CBQ�CBz�CB�CB�CC{CCG�CCz�CC�CC�HCD
=CD=qCDz�CD��CD�
CE
=CE=qCEp�CE��CE�HCF{CF=qCFz�CF�CF�HCG{CGG�CGz�CG�CG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�Aއ+Aޙ�Aޛ�Aޙ�Aޙ�Aޛ�Aޙ�Aޙ�Aޛ�Aޝ�Aޡ�Aޟ�Aޡ�Aޡ�Aޟ�Aޡ�Aޝ�Aޝ�Aޛ�A�x�A�dZA�E�A� �A���A��A��mA��yA��mA��mA��HAݶFAݧ�Aݴ9Aݣ�A��HA�?}A�  A٬A�I�A��;AռjA�1'A�(�A�/AҋDAѼjA��A�ZA���A��TA�C�A�t�A�A�O�AƟ�A�v�A��A��TA���A�ĜA�p�A��;A�t�A��
A��A��A���A�A�A�ZA��jA���A��A�+A�ƨA�O�A��A���A�dZA��A�`BA�A�A�z�A���A�E�A��\A�v�A�ffA���A�33A�`BA�  A��jA�E�A��#A��A�(�A�^5A�&�A��mA��jA��-A��A��A���A�+A�=qA��A���A�E�A�5?A�E�A���A�n�A���A��mA�E�A��A��uA�9XA�ĜA��uA��DA}��A|1Ay��At�uApz�An�DAl��Aj�jAiS�AhM�AfbAbbA]p�AX��AU�hATbNAT$�AS"�AQAO7LAL�\AG�^AF�uAE
=AA��A<��A9x�A8A6�A5��A4ȴA4r�A4$�A3��A3S�A1��A/�mA.�jA-�A,��A+A*-A'�A&^5A&9XA%�A%/A$jA#�TA �jA�-A��A�^A AhsA�PAM�A\)AA+A�AM�A��A��A�hA�A�A�yA��A{AC�A"�AĜAƨA�uA{A��A%A
��A
ZA
-A	�wA	K�A	K�A	G�A	?}A	7LA	+A�`A9XA�A�A��At�A�AI�A�AĜAt�A J@�x�@���@���@���@�
=@�C�@��@��m@�|�@���@��y@�J@���@�|�@��\@�n�@�ff@���@���@�V@���@�@���@��D@���@��@���@�n�@�%@�l�@�E�@�h@���@�j@�F@睲@�ȴ@�Q�@���@�?}@��@��@�`B@�t�@��H@�"�@߾w@ߍP@�\)@��H@�J@ݡ�@�/@۶F@�A�@���@�Q�@�=q@д9@�b@�K�@�33@�1@�z�@У�@�9X@�C�@Ώ\@�V@�5?@��@�?}@̛�@� �@�|�@��@��y@�^5@�J@�J@ɉ7@��/@�I�@�  @ǶF@�
=@Ɵ�@�E�@�?}@�V@�l�@�V@°!@�E�@���@�hs@���@��@�I�@�\)@��+@��@��T@��^@���@�hs@���@���@�t�@�
=@���@�-@���@�X@�?}@���@�1'@���@�E�@�-@��@�p�@���@�1'@���@���@�|�@���@���@��@��^@�hs@�`B@�7L@�%@���@� �@���@���@�dZ@�+@���@�{@���@�?}@���@��@�1'@���@���@�S�@�"�@�v�@�J@�=q@��@��7@�7L@��@���@���@�33@��y@���@�-@�-@��@��T@�`B@��@��
@���@��P@�\)@��+@��@��@�7L@��@���@�Ĝ@���@��@�I�@��m@���@�\)@��@��R@���@�ff@�$�@��@�p�@�%@���@�Z@�Q�@�I�@� �@��;@��F@�t�@�33@�
=@���@�J@���@�p�@�X@�V@��j@�Q�@�b@���@��m@���@�|�@�dZ@��@�~�@�M�@�-@�@��-@���@���@�p�@�&�@�%@���@��@�Ĝ@�Z@��
@���@�t�@�t�@��P@�t�@�K�@�33@�
=@�ff@�{@��@�@��-@���@�x�@�O�@�7L@��@�bN@�1'@���@��
@�t�@��@���@��+@�-@���@�J@�J@��^@�hs@�7L@�/@��@�V@�%@��/@���@��@�Q�@�1'@�b@��m@���@��@�\)@�K�@��@���@���@�v�@�ff@�E�@�$�@��#@��h@��@�p�@�?}@�V@�j@� �@��@���@�\)@��y@��\@�~�@�^5@���@��-@���@��@��j@�z�@� �@��
@�b@�Q�@���@��@��@�+@�33@�+@�@��R@�=q@���@���@�p�@���@���@�Ĝ@��9@�r�@�;@�@l�@~ȴ@~v�@~@}�@}?}@|��@|��@|�j@|�D@|9X@{��@{S�@|(�@|�@{�
@{dZ@{"�@z�\@z-@y��@y&�@x��@xQ�@w�;@w��@w\)@v�y@vE�@v{@u@u/@t��@t(�@s��@sC�@r�!@r=q@q��@q�@p�u@o�@o|�@o\)@o;d@o+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AށA�z�AރAޙ�Aޗ�Aޝ�Aޗ�Aޛ�Aޝ�Aޙ�Aޙ�Aޝ�Aޗ�Aޛ�Aޙ�Aޗ�Aޝ�Aޙ�Aޙ�Aޝ�Aޗ�Aޝ�Aޙ�Aޛ�Aޛ�Aޗ�Aޝ�Aޙ�Aޟ�Aޛ�Aޗ�Aޟ�Aޝ�Aޝ�Aޡ�Aޛ�Aޣ�Aޡ�Aޝ�Aޡ�Aޛ�Aޡ�Aޛ�Aޣ�Aޝ�Aޡ�Aޣ�Aޝ�Aޣ�Aޟ�Aޡ�Aޥ�Aޟ�Aޡ�Aޡ�Aޟ�Aޣ�Aޝ�Aޡ�Aޣ�Aޝ�Aޣ�Aޥ�Aޟ�Aޣ�Aޝ�Aޝ�Aޡ�Aޟ�Aޥ�Aޟ�Aޥ�Aޟ�Aޟ�Aޥ�Aޝ�Aޙ�Aޛ�Aޗ�Aޟ�Aޛ�Aޙ�Aޟ�Aޝ�Aޟ�Aޣ�Aޟ�Aޣ�Aޟ�Aޙ�Aޝ�Aމ7Aމ7Aމ7A�p�A�v�A�jA�t�A�x�A�t�A�\)A�S�A�\)A�z�A�p�A�hsA�M�A�K�A�S�A�I�A�?}A�?}A�5?A��A�/A�7LA�;dA�JA�%A�1A�A�A���A���A���A���A���A���A��A��A��yA��A��`A��mA��mA��`A��yA��`A��A��mA��mA��A��`A��yA��yA��`A��A��mA��mA��A��`A��`A��yA��`A��yA��TA��yA��mA��mA��mA��TA��mA��HA��HA��HA��#A��
A�ƨAݶFAݴ9Aݩ�Aݩ�Aݩ�Aݥ�AݬAݥ�Aݧ�Aݣ�Aݣ�Aݩ�Aݧ�AݮAݲ-Aݴ9AݼjAݾwAݾwA�ȴAݼjAݩ�Aݡ�Aݏ\A݃A�l�A�bNA�XA�-A��`A�Q�A�JAۺ^A۸RAۑhA�x�A�oAڧ�A�jA�C�A��A���A��A��/A���A���Aٴ9A٬Aٲ-A٬Aٝ�Aٙ�AفA���A�ffA��A״9A׉7A�^5A�A�A��A�A��A֧�A�O�A��A��/A�ƨA���A՟�A�|�A�dZA�M�A�K�A�G�A�$�A�bA���A��A��
A�XA��TA��
AӺ^Aӕ�A�t�A�S�A�=qA��A���A�ĜAҮAҝ�AғuAҋDA�t�A�ffA�ZA�/A���Aѡ�A�|�A�jA�VA�7LA�bA��A��#A���AЏ\A�C�A���Aϥ�A�jA�?}A�bA�ƨA�E�A͍PA�?}A��HA�bNA���A�|�A�S�A�9XA��AʍPA�p�A�A�A���A�bNA�(�A��A�ȴAȓuAȇ+A�|�A�|�A�x�A�jA�G�A���A���AǺ^Aǲ-Aǡ�AǕ�AǅA�|�A�jA�=qA�5?A�(�A��A���AƶFAƕ�AƁA�`BA�+AōPA�l�A�^5A�S�A�C�A�E�A�=qA�7LA��A�JA�%A���A��A��A��mA��A��
A��
A��/A��;A���A���A���A���A���A���A���A���A�ƨAĲ-AĬAĥ�Aĕ�A�v�A�ffA�O�A�A�A�&�A�A��yA���A�ĜAò-AÝ�AÉ7A�z�A�jA�dZA�bNA�A�A� �A�A��HAº^APA�O�A�(�A��A�A��
A���A��jA���A�v�A��A��mA���A��;A�C�A�A��;A��jA���A���A��DA�r�A�VA�=qA�33A��A��#A���A��\A�v�A�&�A��A�%A��`A���A��FA���A���A���A���A���A���A��uA��hA���A��uA��\A��hA��uA��PA��PA��\A��A�t�A�hsA�\)A�O�A�G�A�A�A�;dA�33A�-A�+A�$�A��A��A�bA�1A�A�%A���A���A��A��TA��TA���A���A���A���A��uA���A��hA��A�~�A�~�A�x�A�jA�`BA�Q�A�E�A�=qA�7LA�+A��A��A�  A��`A��/A���A��9A�\)A�%A���A���A�p�A�C�A�-A�A��HA��A���A�ĜA�ĜA���A���A��A�v�A�VA��yA�VA���A��!A��7A�r�A�p�A�jA�K�A�=qA�/A�  A��#A�ĜA�r�A�5?A��A�VA���A��HA��^A��hA�l�A�?}A��A��A���A���A��A�^5A�=qA�VA�ĜA�v�A�7LA���A���A�=qA���A��/A�ƨA��^A��9A���A���A���A��7A��A�z�A�r�A�l�A�^5A�M�A�=qA�(�A�
=A���A��A��A��A��A��;A��#A���A��FA��hA�l�A�O�A�1'A�JA��A�ƨA��A���A���A��hA���A��\A��+A��7A��7A��A�~�A��A��A�x�A�t�A�z�A�t�A�l�A�jA�jA�jA�l�A�l�A�hsA�ffA�dZA�hsA�`BA�`BA�^5A�\)A�XA�C�A�A�A�;dA�VA��A��^A��uA��+A��A��A�p�A�`BA�Q�A�A�A�33A�(�A� �A�{A�  A��`A�ȴA��jA���A�|�A�n�A�n�A�hsA�XA�?}A�JA��#A�ȴA���A�`BA�;dA��A�
=A���A��`A�ȴA���A�p�A� �A���A��A��mA��#A���A���A��-A���A��\A�dZA�dZA�XA�Q�A�K�A�A�A�?}A�?}A�C�A�C�A�=qA�=qA�=qA�?}A�=qA�9XA�33A�-A��A�JA���A���A��!A��A�^5A�M�A�=qA�"�A��A��A�
=A��A��#A���A�ƨA�A��A���A���A��\A��PA��\A��DA�9XA��#A�oA���A��+A�VA��A���A�S�A�%A��jA��A�hsA�S�A�;dA�/A�/A�(�A��A�
=A���A��`A��^A��PA�XA�1'A�{A���A��mA���A���A�I�A���A��A��PA�oA���A��RA���A��PA�l�A�=qA�$�A��A��/A��
A���A���A���A���A���A��9A���A��PA��A�z�A�p�A�M�A�A��
A��-A��\A�`BA�C�A�9XA�&�A�oA�%A���A���A��A��A��mA���A�ƨA��A�~�A�jA�\)A�\)A�VA�=qA��A�A��TA��/A��#A��/A���A���A���A�dZA���A���A�/A��A�E�A��yA��^A�v�A�Q�A�7LA� �A�{A�
=A�A���A��mA���A��jA��-A���A�p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aއ+Aޙ�Aޛ�Aޙ�Aޙ�Aޛ�Aޙ�Aޙ�Aޛ�Aޝ�Aޡ�Aޟ�Aޡ�Aޡ�Aޟ�Aޡ�Aޝ�Aޝ�Aޛ�A�x�A�dZA�E�A� �A���A��A��mA��yA��mA��mA��HAݶFAݧ�Aݴ9Aݣ�A��HA�?}A�  A٬A�I�A��;AռjA�1'A�(�A�/AҋDAѼjA��A�ZA���A��TA�C�A�t�A�A�O�AƟ�A�v�A��A��TA���A�ĜA�p�A��;A�t�A��
A��A��A���A�A�A�ZA��jA���A��A�+A�ƨA�O�A��A���A�dZA��A�`BA�A�A�z�A���A�E�A��\A�v�A�ffA���A�33A�`BA�  A��jA�E�A��#A��A�(�A�^5A�&�A��mA��jA��-A��A��A���A�+A�=qA��A���A�E�A�5?A�E�A���A�n�A���A��mA�E�A��A��uA�9XA�ĜA��uA��DA}��A|1Ay��At�uApz�An�DAl��Aj�jAiS�AhM�AfbAbbA]p�AX��AU�hATbNAT$�AS"�AQAO7LAL�\AG�^AF�uAE
=AA��A<��A9x�A8A6�A5��A4ȴA4r�A4$�A3��A3S�A1��A/�mA.�jA-�A,��A+A*-A'�A&^5A&9XA%�A%/A$jA#�TA �jA�-A��A�^A AhsA�PAM�A\)AA+A�AM�A��A��A�hA�A�A�yA��A{AC�A"�AĜAƨA�uA{A��A%A
��A
ZA
-A	�wA	K�A	K�A	G�A	?}A	7LA	+A�`A9XA�A�A��At�A�AI�A�AĜAt�A J@�x�@���@���@���@�
=@�C�@��@��m@�|�@���@��y@�J@���@�|�@��\@�n�@�ff@���@���@�V@���@�@���@��D@���@��@���@�n�@�%@�l�@�E�@�h@���@�j@�F@睲@�ȴ@�Q�@���@�?}@��@��@�`B@�t�@��H@�"�@߾w@ߍP@�\)@��H@�J@ݡ�@�/@۶F@�A�@���@�Q�@�=q@д9@�b@�K�@�33@�1@�z�@У�@�9X@�C�@Ώ\@�V@�5?@��@�?}@̛�@� �@�|�@��@��y@�^5@�J@�J@ɉ7@��/@�I�@�  @ǶF@�
=@Ɵ�@�E�@�?}@�V@�l�@�V@°!@�E�@���@�hs@���@��@�I�@�\)@��+@��@��T@��^@���@�hs@���@���@�t�@�
=@���@�-@���@�X@�?}@���@�1'@���@�E�@�-@��@�p�@���@�1'@���@���@�|�@���@���@��@��^@�hs@�`B@�7L@�%@���@� �@���@���@�dZ@�+@���@�{@���@�?}@���@��@�1'@���@���@�S�@�"�@�v�@�J@�=q@��@��7@�7L@��@���@���@�33@��y@���@�-@�-@��@��T@�`B@��@��
@���@��P@�\)@��+@��@��@�7L@��@���@�Ĝ@���@��@�I�@��m@���@�\)@��@��R@���@�ff@�$�@��@�p�@�%@���@�Z@�Q�@�I�@� �@��;@��F@�t�@�33@�
=@���@�J@���@�p�@�X@�V@��j@�Q�@�b@���@��m@���@�|�@�dZ@��@�~�@�M�@�-@�@��-@���@���@�p�@�&�@�%@���@��@�Ĝ@�Z@��
@���@�t�@�t�@��P@�t�@�K�@�33@�
=@�ff@�{@��@�@��-@���@�x�@�O�@�7L@��@�bN@�1'@���@��
@�t�@��@���@��+@�-@���@�J@�J@��^@�hs@�7L@�/@��@�V@�%@��/@���@��@�Q�@�1'@�b@��m@���@��@�\)@�K�@��@���@���@�v�@�ff@�E�@�$�@��#@��h@��@�p�@�?}@�V@�j@� �@��@���@�\)@��y@��\@�~�@�^5@���@��-@���@��@��j@�z�@� �@��
@�b@�Q�@���@��@��@�+@�33@�+@�@��R@�=q@���@���@�p�@���@���@�Ĝ@��9@�r�@�;@�@l�@~ȴ@~v�@~@}�@}?}@|��@|��@|�j@|�D@|9X@{��@{S�@|(�@|�@{�
@{dZ@{"�@z�\@z-@y��@y&�@x��@xQ�@w�;@w��@w\)@v�y@vE�@v{@u@u/@t��@t(�@s��@sC�@r�!@r=q@q��@q�@p�u@o�@o|�@o\)@o;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AށA�z�AރAޙ�Aޗ�Aޝ�Aޗ�Aޛ�Aޝ�Aޙ�Aޙ�Aޝ�Aޗ�Aޛ�Aޙ�Aޗ�Aޝ�Aޙ�Aޙ�Aޝ�Aޗ�Aޝ�Aޙ�Aޛ�Aޛ�Aޗ�Aޝ�Aޙ�Aޟ�Aޛ�Aޗ�Aޟ�Aޝ�Aޝ�Aޡ�Aޛ�Aޣ�Aޡ�Aޝ�Aޡ�Aޛ�Aޡ�Aޛ�Aޣ�Aޝ�Aޡ�Aޣ�Aޝ�Aޣ�Aޟ�Aޡ�Aޥ�Aޟ�Aޡ�Aޡ�Aޟ�Aޣ�Aޝ�Aޡ�Aޣ�Aޝ�Aޣ�Aޥ�Aޟ�Aޣ�Aޝ�Aޝ�Aޡ�Aޟ�Aޥ�Aޟ�Aޥ�Aޟ�Aޟ�Aޥ�Aޝ�Aޙ�Aޛ�Aޗ�Aޟ�Aޛ�Aޙ�Aޟ�Aޝ�Aޟ�Aޣ�Aޟ�Aޣ�Aޟ�Aޙ�Aޝ�Aމ7Aމ7Aމ7A�p�A�v�A�jA�t�A�x�A�t�A�\)A�S�A�\)A�z�A�p�A�hsA�M�A�K�A�S�A�I�A�?}A�?}A�5?A��A�/A�7LA�;dA�JA�%A�1A�A�A���A���A���A���A���A���A��A��A��yA��A��`A��mA��mA��`A��yA��`A��A��mA��mA��A��`A��yA��yA��`A��A��mA��mA��A��`A��`A��yA��`A��yA��TA��yA��mA��mA��mA��TA��mA��HA��HA��HA��#A��
A�ƨAݶFAݴ9Aݩ�Aݩ�Aݩ�Aݥ�AݬAݥ�Aݧ�Aݣ�Aݣ�Aݩ�Aݧ�AݮAݲ-Aݴ9AݼjAݾwAݾwA�ȴAݼjAݩ�Aݡ�Aݏ\A݃A�l�A�bNA�XA�-A��`A�Q�A�JAۺ^A۸RAۑhA�x�A�oAڧ�A�jA�C�A��A���A��A��/A���A���Aٴ9A٬Aٲ-A٬Aٝ�Aٙ�AفA���A�ffA��A״9A׉7A�^5A�A�A��A�A��A֧�A�O�A��A��/A�ƨA���A՟�A�|�A�dZA�M�A�K�A�G�A�$�A�bA���A��A��
A�XA��TA��
AӺ^Aӕ�A�t�A�S�A�=qA��A���A�ĜAҮAҝ�AғuAҋDA�t�A�ffA�ZA�/A���Aѡ�A�|�A�jA�VA�7LA�bA��A��#A���AЏ\A�C�A���Aϥ�A�jA�?}A�bA�ƨA�E�A͍PA�?}A��HA�bNA���A�|�A�S�A�9XA��AʍPA�p�A�A�A���A�bNA�(�A��A�ȴAȓuAȇ+A�|�A�|�A�x�A�jA�G�A���A���AǺ^Aǲ-Aǡ�AǕ�AǅA�|�A�jA�=qA�5?A�(�A��A���AƶFAƕ�AƁA�`BA�+AōPA�l�A�^5A�S�A�C�A�E�A�=qA�7LA��A�JA�%A���A��A��A��mA��A��
A��
A��/A��;A���A���A���A���A���A���A���A���A�ƨAĲ-AĬAĥ�Aĕ�A�v�A�ffA�O�A�A�A�&�A�A��yA���A�ĜAò-AÝ�AÉ7A�z�A�jA�dZA�bNA�A�A� �A�A��HAº^APA�O�A�(�A��A�A��
A���A��jA���A�v�A��A��mA���A��;A�C�A�A��;A��jA���A���A��DA�r�A�VA�=qA�33A��A��#A���A��\A�v�A�&�A��A�%A��`A���A��FA���A���A���A���A���A���A��uA��hA���A��uA��\A��hA��uA��PA��PA��\A��A�t�A�hsA�\)A�O�A�G�A�A�A�;dA�33A�-A�+A�$�A��A��A�bA�1A�A�%A���A���A��A��TA��TA���A���A���A���A��uA���A��hA��A�~�A�~�A�x�A�jA�`BA�Q�A�E�A�=qA�7LA�+A��A��A�  A��`A��/A���A��9A�\)A�%A���A���A�p�A�C�A�-A�A��HA��A���A�ĜA�ĜA���A���A��A�v�A�VA��yA�VA���A��!A��7A�r�A�p�A�jA�K�A�=qA�/A�  A��#A�ĜA�r�A�5?A��A�VA���A��HA��^A��hA�l�A�?}A��A��A���A���A��A�^5A�=qA�VA�ĜA�v�A�7LA���A���A�=qA���A��/A�ƨA��^A��9A���A���A���A��7A��A�z�A�r�A�l�A�^5A�M�A�=qA�(�A�
=A���A��A��A��A��A��;A��#A���A��FA��hA�l�A�O�A�1'A�JA��A�ƨA��A���A���A��hA���A��\A��+A��7A��7A��A�~�A��A��A�x�A�t�A�z�A�t�A�l�A�jA�jA�jA�l�A�l�A�hsA�ffA�dZA�hsA�`BA�`BA�^5A�\)A�XA�C�A�A�A�;dA�VA��A��^A��uA��+A��A��A�p�A�`BA�Q�A�A�A�33A�(�A� �A�{A�  A��`A�ȴA��jA���A�|�A�n�A�n�A�hsA�XA�?}A�JA��#A�ȴA���A�`BA�;dA��A�
=A���A��`A�ȴA���A�p�A� �A���A��A��mA��#A���A���A��-A���A��\A�dZA�dZA�XA�Q�A�K�A�A�A�?}A�?}A�C�A�C�A�=qA�=qA�=qA�?}A�=qA�9XA�33A�-A��A�JA���A���A��!A��A�^5A�M�A�=qA�"�A��A��A�
=A��A��#A���A�ƨA�A��A���A���A��\A��PA��\A��DA�9XA��#A�oA���A��+A�VA��A���A�S�A�%A��jA��A�hsA�S�A�;dA�/A�/A�(�A��A�
=A���A��`A��^A��PA�XA�1'A�{A���A��mA���A���A�I�A���A��A��PA�oA���A��RA���A��PA�l�A�=qA�$�A��A��/A��
A���A���A���A���A���A��9A���A��PA��A�z�A�p�A�M�A�A��
A��-A��\A�`BA�C�A�9XA�&�A�oA�%A���A���A��A��A��mA���A�ƨA��A�~�A�jA�\)A�\)A�VA�=qA��A�A��TA��/A��#A��/A���A���A���A�dZA���A���A�/A��A�E�A��yA��^A�v�A�Q�A�7LA� �A�{A�
=A�A���A��mA���A��jA��-A���A�p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B2�B2�B2�B2�B3hB2�B33B33B2�B33B2aB33B2�B2�B2�B2aB2-B1�B1�B.B.}B,qB,�B,=B,=B+�B+�B,=B,B+�B)*B'�B*�B1�B5tB+�B�B	B.�B-�B1�B4�B?BB'BOvBS&B`�B�{B�?BǮB��B�BB�B��B�DB��B��BGB{B�B�B�B�B~B$�B4B;dB<6BA�BA�B@�B@�BA�BB�BB�BG�BHKBG�BG�BGBE9B>wB=<B=�B:�B:�B9�B:*B6�B2�B.�B&�B!�B \B�B�B�BoB�QB�EB�HB�tB�6B��B�lB~(Bi�BNB"4BB
�]B
��B
�B
�,B
ÖB
�hB
�7B
��B
��B
��B
�4B
|�B
K)B
9�B
%�B
�B	�ZB	�B	��B	ҽB	�B	��B	�hB	��B	�iB	`BB	M6B	E9B	A�B	@�B	7B	,�B	%�B	�B	�B	B	 �B�B��B�;BںBخBԕB��B�BϫB�pB�dB��B��B��B�<B�XB�XB�hB��B��B��B�dB��B��B�XB��B�?B��B��BɆB�3B�}B�B�-B�IBǮB�KBںB�B�6BӏB�
B��B��BߤB�`B� B�B	�B	�B	�B	B		�B	�B	�B	�B	B	+B	OB	�B	!�B	"hB	'�B	/OB	/OB	2�B	1[B	4B	1�B	<B	E�B	A B	0!B	&�B	 \B	!B	"hB	<jB	+�B	6zB	;�B	A�B	T�B	S[B	ZB	b�B	bNB	ffB	p�B	p�B	tB	{JB	y>B	y	B	qB	m�B	m�B	kB	o B	n/B	m�B	sB	rGB	qvB	o�B	o5B	m�B	kQB	h
B	e,B	dZB	f2B	aB	VB	[�B	a�B	d�B	f�B	aB	a|B	l"B	y>B	zB	{�B	}�B	}�B	|B	|�B	z�B	w�B	pB	n�B	jB	jB	k�B	n/B	qAB	xlB	~�B	�uB	��B	��B	�PB	�"B	��B	�\B	��B	��B	�bB	�hB	��B	��B	��B	�B	��B	�eB	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�kB	��B	�B	�nB	��B	�XB	��B	��B	��B	��B	�9B	�B	�B	��B	�B	ʌB	�)B	͟B	ϫB	��B	�B	��B	҉B	�&B	�&B	��B	֡B	خB	��B	�sB	�EB	�B	�WB	�/B	�pB	�HB	�|B	�BB	��B	��B	�B	�&B	��B	��B	�B	�B	��B	��B	�B	�QB	��B	�B	�cB	�B	�vB	�B	�B	�;B	�B	�vB	�|B	�B	��B	��B	��B	�>B	�B	��B	�rB	��B	�>B	��B	�B	�B	��B	�(B	��B	��B
B
  B
  B	�.B	��B	��B
 �B
 �B
�B
AB
�B
B
�B
�B
�B
�B
%B
�B
�B
YB
�B
�B
�B
�B
�B
B
~B
~B
�B
"B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
�B
�B
:B
�B
�B
bB
�B
4B
:B
:B
:B
@B
�B
:B
B
oB
�B
�B
B
�B
�B
@B
@B
@B
uB
�B
�B
�B
SB
�B
+B
+B
�B
�B
B
kB
=B
�B
�B
qB
=B
�B
�B
B
�B
�B
�B
�B
!B
 �B
�B
�B
 �B
!bB
"4B
"hB
"�B
#B
#B
#nB
$tB
$�B
'RB
'B
(XB
(�B
(�B
(�B
(XB
(XB
+�B
,�B
,qB
.�B
.}B
/OB
/�B
1�B
2�B
2�B
2�B
33B
3�B
3�B
3�B
3�B
3�B
49B
5�B
4�B
5tB
5�B
6B
5�B
4�B
4�B
4�B
49B
3hB
4�B
5B
4�B
6�B
6�B
7LB
7LB
:*B
:*B
8RB
8�B
:^B
:�B
<B
<6B
<�B
<B
;�B
;0B
;0B
;0B
;0B
;0B
:�B
;�B
:�B
:�B
:�B
:�B
:�B
;dB
<6B
<jB
<�B
=�B
>�B
?HB
?HB
?}B
?B
>wB
>�B
?}B
@B
@OB
@OB
@B
@�B
@�B
AUB
A�B
B[B
B[B
B[B
B�B
CaB
CaB
C�B
D3B
D�B
E9B
E9B
E9B
E�B
EmB
FB
FB
FtB
GEB
G�B
HB
H�B
H�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B2-B2�B3�B1�B3hB1'B3�B2aB1�B3�B3hB1�B4nB2aB2�B4B2-B3�B33B2-B3�B1�B3�B2�B2�B3�B1�B4B2-B3�B4nB1�B3hB49B1�B4B1�B2aB4nB2-B4�B2�B3�B1[B3�B2�B2-B3�B1�B33B33B1�B3�B2�B2�B3�B2-B2�B33B1�B3�B1�B1[B3�B1�B3�B2�B1�B33B1'B3hB1�B2�B2�B1'B3�B4�B0�B2�B1[B1�B2aB1[B2-B2aB0�B2�B1[B2�B1�B1�B0�B1�B-CB/�B-wB.}B*eB.B1'B,=B.�B,�B&�B)_B0�B-B-CB)�B,�B)�B,B-B2�B(�B,qB,�B6�B,qB+�B-wB+�B+6B,=B+B,�B+kB-CB.�B,B-CB+�B*�B*�B+B+�B*0B-B+B,�B,B+B-CB+6B+B-B+B,�B,�B+6B,�B,qB+�B,qB*�B-B+�B,=B+�B,B,qB*eB,�B,B+kB,qB)�B+kB*�B)�B)�B($B'B)*B&�B(�B&�B($B'�B&B&�B(�B($B)_B,qB-wB/B.IB5B2�B0�B1�B1'B/�B-wB/OB5�B4�BCaB:^B-CB*eB2aB0�B1�B$�B+BYB�B$B�B�B1B�BkB=B_BB~B=B!�B0�B:�B2aB2aB,=B.IB,qB.}B*�B)�B3�B2-B4B2aB/�B.IB2�B1�B2aB3hB0�B49B;dB5tB5?B5B5?BR�BA�B<�B=�B@�B?}B@�B?�B>wBFBJ�BGEBK^BP}BR�BT,BR�BS[BZ�B\)BL�BO�BMjBP}BT�B[#B[�B`Bd�Bk�Bs�B{B�B�B�GB�+B��B��B�B��B��B�zB�B��B�0B��B�TB��BȴB��B��B�BٴBߤB�dB�BݘBߤB�BޞB�B�,B�GB�B�B�B�,B�B�B�BB� B�TBݘBߤB�KB�B�8B�B��B��B�B~B��B�DB�B�"B�JB�B�"B iB��B �BBB�B�B{B�B�BuB�B�B%B�B�B�B�BGBAB{BYBSB�B�B�B
=B�B�B�B�B�B�B�BbBB�B�B�B�BoBSB�B�BIBB#�B&B%zB#�B$�B$�B#nB%�B&�B,B2�B+6B1[BO�BB�B<�B:�B9�B:*B9�B9�B:^B<B<6B9�B<�BEmBA�B<BAUBD3B@�BB�BA�BA�BD�BB�B?}B@BAUB?�B@B@�BA B>�B?�BAUB?�B?BAUB@OB>�BB�BA BA�BA BA�BB�BA BAUBB'BB�B@�B@�BB[BA�BA�BAUBA�B@�BAUB@�BC-BC-B>�BB[BK�BC-BA�BD3B@�B@�BB�BC-BA�BB'BDgBB�BB�BCaBB�BA�BB�BC�BA�BD�BC�B@�BA BG�BM6BL�BM�BD�BL�BI�BGzBJ�BHKBEBE9BD�BC�BB�BHBFBA�BB�B]�BX�BH�BJXBI�BE�BD�BFBE�BEBEBGEBF�BG�BJ�BK�BC�BCaBHBE�BF�BGzBHBI�BS�BD�BC-BB'BEBF?BC�BC�BMjBHBE�BJ�BJ#BOBFBAUB@�B@�B>�B?}B?B>BB@�B=<B=�B>�B=B>wB>�B=�B>�BB�B<�B<�B=<B;�B:�B=�B;�B<jB=B@�B=�B<6B<6B?B=<B=�B@�B:*B9�B<jB9XB:^B<B:^B9�B;�B:�B9XB9$B<�B;0B9�B8�B;0B:^B:�B;0B:*B9�B:�B:�B:*B7B8�B9�B9�B9XB6B;dB7�B7�B@�B>�B=�B=�B7�B7�B7�B6�B8�B8�B7LB6FB5�B6B5?B6FB6B7B3�B6�B3�B2�B0�B0UB0�B1[B6B3�B,=B4nB4�B0!B,�B+kB)�B+6B-�B.IB.�B0�B)�B%�B%�B%�B$�B$�B(�B'B'�B+6B%B"hB$B"�B!�B#:B"hB �B �B"�B!�B!-B�B�B�B \B!bB!-B �B!�B �BVB"4BVBIB�B�B�B�B�B�B=B�B�BxBIBOBB�B�B1B�B+kB+�B)�B�BB�B�B�B �BB!B:B�B�B�B
�B1B	B�B_B�BYB�B1B�BAB��B�]B��B��B�xB�BSB iB��B�B�B�NB�ZB�B��B��BݘB��B��B�B�B�EB�B��B�
B��B�
B�2BԕBӏB�&B��B��B�TB�HBуB��B�B�KB�XB�#B�KB�zB�?B�BŢB�B�tB�B�B��B��B��B�<B�qB��B�B��B�B�LB��B�?B��B�9B�nB��B��B�=B�qB��B��B�B�MB��B��B��B�1B�%B�SB��B�B��B��B�B�B�uB�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                   444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                   444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022101515025420221015150254IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022101619310520221016193105QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022101619310520221016193105QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                