CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-06-06T05:08:44Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  Z�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  rx   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  xh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ř   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ˈ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 6h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   6�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   <�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   B�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T H�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   I   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   I$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   I,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   I4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � I<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   I�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   I�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    I�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        J    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        J   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       J   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    JArgo profile    3.1 1.2 19500101000000  20220606050844  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_215                 6810_008521_215                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�Շ��ݘ@�Շ��ݘ11  @�Շ���@�Շ���@1���	@1���	�d���=���d���=��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @=p�@�  @�  @��R@�  A   A  A ��A,��A@  A`  A�Q�A�Q�A��A�\)A��AϮA߮A�B   B�
B  BQ�B (�B(  B0  B8(�B@(�BH  BP  BX  B_�
Bh  Bp(�Bx(�B�  B��B�{B�{B��B�{B�{B��
B�  B�{B�{B��B�  B�(�B�  B�{B�  B��B��B�  B��
B�  B��B�  B�{B�  B�  B��B�  B��
B��B��C   C  C��C  C��C	��C��C��C
=C
=C  C
=C{C
=C��C  C��C!�C#��C&  C(  C*  C,  C.  C/��C1�C4{C6  C7�C:
=C<
=C>  C?��CA�CC�CE��CG��CI�HCL  CN
=CO�CR  CT
=CU��CX
=CZ
=C\  C^  C`
=Cb  Cc��Ce�Cg��Ci��Cl  Cn
=Cp
=Cr  Ct  Cu��Cw��Cz
=C|
=C~  C�  C�  C�
=C�  C���C�C���C�\C�C�  C�C�  C���C�  C���C���C�C�
=C�  C�  C���C���C�
=C�  C�  C���C�C���C���C�  C���C���C�C�
=C�C�C���C���C�  C���C���C���C�  C�
=C�
=C�  C�  C�  C�C���C�  C�C�  C���C���C���C���C���C�  C�C�  C�  C�C���C���C�  C�  C�  C�  C�C�C�
=C�C���C���C�  C�C�C�
=C�C�C�C�C�  C���C���C���C���C�  C�  C�  C���C���C�  C�
=C�C���C�  C�C�  C�  C���C���C���C�  C�
=C�C���C�  C�C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  D   D � D  D� D�D� D  D}qD�qD� D  D� D�qD� D�qD� D  D��D	�D	� D
  D
� D  D��D  D��D�D}qD  D��D�D� D  D��D  D� D�D� D  D� D  D��D�D�D�D� D�D��D  D� D  D��D  D}qD�qD}qD  D� D  D� D�D��D�D}qD��D z�D �qD!z�D"  D"� D#  D#� D$�D$� D%  D%� D&�D&� D'  D'� D(  D(��D)�D)}qD*  D*��D+  D+}qD+�qD,� D-  D-� D.  D.��D/D/� D/�qD0z�D0�qD1}qD2  D2� D3  D3� D3�qD4}qD4�qD5z�D5��D6z�D6�qD7� D8  D8}qD8�qD9� D:�D:� D:�qD;� D<  D<}qD=  D=� D=��D>z�D>�qD?}qD?�qD@��DA  DA� DB�DB� DB��DC}qDC�qDDz�DD��DE}qDF  DF��DGDG� DG�qDH}qDI  DI��DJ  DJ}qDJ�qDK� DL�DL}qDM  DM��DM�qDN}qDO  DO� DP�DP� DP�qDQ� DR  DR� DR�qDSz�DS��DT}qDT�qDU}qDU�qDV��DW�DW��DX  DX� DY  DY� DZ  DZ��D[  D[}qD\  D\� D]  D]� D^  D^��D_�D_}qD`  D`� Da  Da� Db�Db� Dc  Dc��Dd  Dd}qDd�qDe� DfDf�Dg�Dg}qDg�RDh}qDi  Di��Dj  Dj}qDj�qDk� Dl  Dl� Dm  Dm�DnDn��Dn�qDo}qDp  Dp}qDq  Dq}qDq�qDr}qDr�qDs}qDs�qDt� Du  Du��Dv�Dv� Dv��Dw��Dx�Dx��Dy�Dy}qDy�qDz��D{�D{�D|�D|��D|�qD}� D~  D~}qD  D��D�  D�AHD��HD�� D���D�AHD�~�D���D�HD�AHD�}qD���D�HD�@ D�~�D�� D�HD�@ D���D��HD�  D�B�D��HD��HD�  D�@ D�~�D��qD�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�HD�@ D�� D�� D�  D�>�D�� D�� D���D�AHD�� D���D���D�@ D�� D��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?W
=?�\)?���?�Q�?�ff@�\@��@#�
@0��@=p�@O\)@aG�@k�@�G�@���@�{@�
=@�  @�ff@���@�
=@�p�@\@���@�33@ٙ�@��
@�@��@�(�A�Az�A	��Ap�A  A�A��A�A\)A#�
A'
=A*=qA/\)A333A5A:�HA?\)AA�AEAK�AO\)AQ�AW�AZ�HA^{Ac33AhQ�Aj�HAo\)As�
AvffA|(�A�  A���A�z�A�{A��A��\A�(�A�{A�  A��HA��
A�{A���A��HA�(�A�{A���A��HA�(�A�ffA�G�A��HA�z�A��RA�G�A��\A��A��A�G�A��HA��A��A�G�A��HA��AǮA���A��HA�p�A�
=A���A��
A�p�A�
=Aٙ�A��
A��A�
=AᙚA�33A���A�\)A���A�\A��A�ffA��A��HA�(�A�ffA���A��A��
A�{B (�B ��BB33B�B��B�B�HB�B��B
{B
�RB  BG�B�B�HB(�Bp�B�\B33B��BB�\B�B��B�B�RB  BG�B{B�HB Q�B!p�B"=qB#
=B$Q�B%��B&ffB'\)B(��B)B*ffB+�
B,��B-B/
=B0  B0��B2{B3\)B4Q�B5�B6=qB7�B8Q�B9G�B:�\B;\)B<(�B=p�B>�RB?\)B@(�BAp�BB�\BC33BDz�BEBF�\BG\)BHz�BI�BJffBK�BL��BM�BN�RBO�
BQ�BR{BR�HBT(�BUp�BV�\BW33BXz�BYBZ�\B[�B\��B^{B^�HB_�
Ba�Ba�Bb�HBdQ�BeG�Bf{Bf�HBhQ�Bip�BjffBk33Blz�BmBnffBo�Bp��Bq�Br�\Bs�Bu�Bv{Bv�HBx(�ByG�Bz{B{
=B|z�B}p�B~=qB�B�Q�B���B�G�B��B�Q�B���B�\)B�  B�z�B��HB���B�=qB��RB��B��B�Q�B�
=B�\)B��
B���B�33B���B�{B���B�\)B��B�ffB��HB�G�B�{B���B�
=B�p�B�{B��RB�33B���B�(�B��HB�G�B�B�=qB��HB�p�B��B�Q�B�
=B��B�{B��\B�33B��
B�Q�B���B�G�B�{B��\B�
=B�p�B�(�B��RB�33B���B�=qB��HB�G�B�B�ffB���B�\)B��
B�z�B��B��B��B�z�B��B�B�(�B���B�G�B��B�Q�B���B��B�{B���B���B���B�=qB���B�33B��B�Q�B���B��B��
B�=qB���B��B��B�ffB��HB���B�(�B�z�B�
=B���B�=qB���B�G�B��B�=qB��HB�p�B��
B�ffB���B���B��B�z�B�33B���B�{B¸RB�\)B��B�=qBĸRB�p�B�  B�z�B��HBǅB�(�BȸRB�
=Bə�B�(�B���B�\)B��
B�=qB��HBͅB�{B�z�B���BϮB�=qBУ�B��B��
B�ffB��HB�G�B��Bԏ\B��BՅB�  B֣�B�G�B��
B�=qBظRB��B��
B�ffB���B�\)B�B�Q�B���B݅B��B�ffB���B߮B�{B�z�B��B�B�(�B��B�33B��
B�ffB���B�G�B��
B�z�B�
=B�p�B��B�ffB�
=B�B�(�B�\B�
=B뙚B�Q�B��HB�G�B��B�Q�B���B�B��B�ffB���B�B�{B�z�B���B�B�{B�\B��HB�\)B�  B��\B�
=B�p�B��
B�ffB�
=B���B�{B�ffB�
=B���B�(�B��\B���B��B�{B���B��HB�p�B��C G�C �C �C �HC(�Cp�C�C�HC{CG�C�\C�
C
=CG�Cp�C�RC  C=qCp�C��C�HC33Cp�C��C�
C{CffC�C�
C
=CG�C��C�HC{CG�Cp�C�C	  C	G�C	p�C	��C	�
C
�C
ffC
�C
�HC
=C=qCz�C��C
=C33CffC��C�HC(�Cp�C��C��C
=CG�C�C��C{CG�Cz�C��C�HC(�CffC��C��C  CG�C�C��C  C(�CffC�C�C33CffC��C��C  C=qC�\C��C  C=qCp�C�RC  CQ�C�\CC��C33Cp�CC  C=qCp�C��C�HC�Cp�C�RC�HC�CffC��C�C�CG�C�\C�HC(�C\)C�\C��C(�CffC��C��C
=C\)C�C�C�CQ�C��C�HC (�C p�C ��C �HC!�C!ffC!�C"  C"Q�C"��C"�HC#�C#ffC#��C#�HC$=qC$�\C$�
C%�C%\)C%��C%�HC&33C&z�C&�
C'{C'Q�C'��C'�HC(33C(�C(��C)  C)G�C)�\C)�HC*33C*z�C*C+  C+G�C+�C+��C,�C,p�C,��C-
=C-G�C-��C-��C.G�C.��C.�HC/�C/p�C/�RC0
=C0\)C0�C1  C1Q�C1�\C1�
C2�C2ffC2�RC3{C3ffC3�RC4
=C4Q�C4�\C4�
C5�C5p�C5C6�C6ffC6�C7  C7Q�C7�\C7�
C8�C8ffC8C9(�C9z�C9C:
=C:Q�C:��C:��C;Q�C;�C<
=C<G�C<�\C<�HC=33C=��C=��C>G�C>�\C>�C?(�C?z�C?�
C@33C@�\C@�HCA33CAp�CA��CB(�CB�CB�
CC(�CCp�CC�CD  CD\)CDCE{CEQ�CE��CE��CFG�CF��CG  CGQ�CG�\CG�
CH�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@   @=p�@�  @�  @��R@�  A   A  A ��A,��A@  A`  A�Q�A�Q�A��A�\)A��AϮA߮A�B   B�
B  BQ�B (�B(  B0  B8(�B@(�BH  BP  BX  B_�
Bh  Bp(�Bx(�B�  B��B�{B�{B��B�{B�{B��
B�  B�{B�{B��B�  B�(�B�  B�{B�  B��B��B�  B��
B�  B��B�  B�{B�  B�  B��B�  B��
B��B��C   C  C��C  C��C	��C��C��C
=C
=C  C
=C{C
=C��C  C��C!�C#��C&  C(  C*  C,  C.  C/��C1�C4{C6  C7�C:
=C<
=C>  C?��CA�CC�CE��CG��CI�HCL  CN
=CO�CR  CT
=CU��CX
=CZ
=C\  C^  C`
=Cb  Cc��Ce�Cg��Ci��Cl  Cn
=Cp
=Cr  Ct  Cu��Cw��Cz
=C|
=C~  C�  C�  C�
=C�  C���C�C���C�\C�C�  C�C�  C���C�  C���C���C�C�
=C�  C�  C���C���C�
=C�  C�  C���C�C���C���C�  C���C���C�C�
=C�C�C���C���C�  C���C���C���C�  C�
=C�
=C�  C�  C�  C�C���C�  C�C�  C���C���C���C���C���C�  C�C�  C�  C�C���C���C�  C�  C�  C�  C�C�C�
=C�C���C���C�  C�C�C�
=C�C�C�C�C�  C���C���C���C���C�  C�  C�  C���C���C�  C�
=C�C���C�  C�C�  C�  C���C���C���C�  C�
=C�C���C�  C�C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  D   D � D  D� D�D� D  D}qD�qD� D  D� D�qD� D�qD� D  D��D	�D	� D
  D
� D  D��D  D��D�D}qD  D��D�D� D  D��D  D� D�D� D  D� D  D��D�D�D�D� D�D��D  D� D  D��D  D}qD�qD}qD  D� D  D� D�D��D�D}qD��D z�D �qD!z�D"  D"� D#  D#� D$�D$� D%  D%� D&�D&� D'  D'� D(  D(��D)�D)}qD*  D*��D+  D+}qD+�qD,� D-  D-� D.  D.��D/D/� D/�qD0z�D0�qD1}qD2  D2� D3  D3� D3�qD4}qD4�qD5z�D5��D6z�D6�qD7� D8  D8}qD8�qD9� D:�D:� D:�qD;� D<  D<}qD=  D=� D=��D>z�D>�qD?}qD?�qD@��DA  DA� DB�DB� DB��DC}qDC�qDDz�DD��DE}qDF  DF��DGDG� DG�qDH}qDI  DI��DJ  DJ}qDJ�qDK� DL�DL}qDM  DM��DM�qDN}qDO  DO� DP�DP� DP�qDQ� DR  DR� DR�qDSz�DS��DT}qDT�qDU}qDU�qDV��DW�DW��DX  DX� DY  DY� DZ  DZ��D[  D[}qD\  D\� D]  D]� D^  D^��D_�D_}qD`  D`� Da  Da� Db�Db� Dc  Dc��Dd  Dd}qDd�qDe� DfDf�Dg�Dg}qDg�RDh}qDi  Di��Dj  Dj}qDj�qDk� Dl  Dl� Dm  Dm�DnDn��Dn�qDo}qDp  Dp}qDq  Dq}qDq�qDr}qDr�qDs}qDs�qDt� Du  Du��Dv�Dv� Dv��Dw��Dx�Dx��Dy�Dy}qDy�qDz��D{�D{�D|�D|��D|�qD}� D~  D~}qD  D��D�  D�AHD��HD�� D���D�AHD�~�D���D�HD�AHD�}qD���D�HD�@ D�~�D�� D�HD�@ D���D��HD�  D�B�D��HD��HD�  D�@ D�~�D��qD�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�HD�@ D�� D�� D�  D�>�D�� D�� D���D�AHD�� D���D���D�@ D�� D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?W
=?�\)?���?�Q�?�ff@�\@��@#�
@0��@=p�@O\)@aG�@k�@�G�@���@�{@�
=@�  @�ff@���@�
=@�p�@\@���@�33@ٙ�@��
@�@��@�(�A�Az�A	��Ap�A  A�A��A�A\)A#�
A'
=A*=qA/\)A333A5A:�HA?\)AA�AEAK�AO\)AQ�AW�AZ�HA^{Ac33AhQ�Aj�HAo\)As�
AvffA|(�A�  A���A�z�A�{A��A��\A�(�A�{A�  A��HA��
A�{A���A��HA�(�A�{A���A��HA�(�A�ffA�G�A��HA�z�A��RA�G�A��\A��A��A�G�A��HA��A��A�G�A��HA��AǮA���A��HA�p�A�
=A���A��
A�p�A�
=Aٙ�A��
A��A�
=AᙚA�33A���A�\)A���A�\A��A�ffA��A��HA�(�A�ffA���A��A��
A�{B (�B ��BB33B�B��B�B�HB�B��B
{B
�RB  BG�B�B�HB(�Bp�B�\B33B��BB�\B�B��B�B�RB  BG�B{B�HB Q�B!p�B"=qB#
=B$Q�B%��B&ffB'\)B(��B)B*ffB+�
B,��B-B/
=B0  B0��B2{B3\)B4Q�B5�B6=qB7�B8Q�B9G�B:�\B;\)B<(�B=p�B>�RB?\)B@(�BAp�BB�\BC33BDz�BEBF�\BG\)BHz�BI�BJffBK�BL��BM�BN�RBO�
BQ�BR{BR�HBT(�BUp�BV�\BW33BXz�BYBZ�\B[�B\��B^{B^�HB_�
Ba�Ba�Bb�HBdQ�BeG�Bf{Bf�HBhQ�Bip�BjffBk33Blz�BmBnffBo�Bp��Bq�Br�\Bs�Bu�Bv{Bv�HBx(�ByG�Bz{B{
=B|z�B}p�B~=qB�B�Q�B���B�G�B��B�Q�B���B�\)B�  B�z�B��HB���B�=qB��RB��B��B�Q�B�
=B�\)B��
B���B�33B���B�{B���B�\)B��B�ffB��HB�G�B�{B���B�
=B�p�B�{B��RB�33B���B�(�B��HB�G�B�B�=qB��HB�p�B��B�Q�B�
=B��B�{B��\B�33B��
B�Q�B���B�G�B�{B��\B�
=B�p�B�(�B��RB�33B���B�=qB��HB�G�B�B�ffB���B�\)B��
B�z�B��B��B��B�z�B��B�B�(�B���B�G�B��B�Q�B���B��B�{B���B���B���B�=qB���B�33B��B�Q�B���B��B��
B�=qB���B��B��B�ffB��HB���B�(�B�z�B�
=B���B�=qB���B�G�B��B�=qB��HB�p�B��
B�ffB���B���B��B�z�B�33B���B�{B¸RB�\)B��B�=qBĸRB�p�B�  B�z�B��HBǅB�(�BȸRB�
=Bə�B�(�B���B�\)B��
B�=qB��HBͅB�{B�z�B���BϮB�=qBУ�B��B��
B�ffB��HB�G�B��Bԏ\B��BՅB�  B֣�B�G�B��
B�=qBظRB��B��
B�ffB���B�\)B�B�Q�B���B݅B��B�ffB���B߮B�{B�z�B��B�B�(�B��B�33B��
B�ffB���B�G�B��
B�z�B�
=B�p�B��B�ffB�
=B�B�(�B�\B�
=B뙚B�Q�B��HB�G�B��B�Q�B���B�B��B�ffB���B�B�{B�z�B���B�B�{B�\B��HB�\)B�  B��\B�
=B�p�B��
B�ffB�
=B���B�{B�ffB�
=B���B�(�B��\B���B��B�{B���B��HB�p�B��C G�C �C �C �HC(�Cp�C�C�HC{CG�C�\C�
C
=CG�Cp�C�RC  C=qCp�C��C�HC33Cp�C��C�
C{CffC�C�
C
=CG�C��C�HC{CG�Cp�C�C	  C	G�C	p�C	��C	�
C
�C
ffC
�C
�HC
=C=qCz�C��C
=C33CffC��C�HC(�Cp�C��C��C
=CG�C�C��C{CG�Cz�C��C�HC(�CffC��C��C  CG�C�C��C  C(�CffC�C�C33CffC��C��C  C=qC�\C��C  C=qCp�C�RC  CQ�C�\CC��C33Cp�CC  C=qCp�C��C�HC�Cp�C�RC�HC�CffC��C�C�CG�C�\C�HC(�C\)C�\C��C(�CffC��C��C
=C\)C�C�C�CQ�C��C�HC (�C p�C ��C �HC!�C!ffC!�C"  C"Q�C"��C"�HC#�C#ffC#��C#�HC$=qC$�\C$�
C%�C%\)C%��C%�HC&33C&z�C&�
C'{C'Q�C'��C'�HC(33C(�C(��C)  C)G�C)�\C)�HC*33C*z�C*C+  C+G�C+�C+��C,�C,p�C,��C-
=C-G�C-��C-��C.G�C.��C.�HC/�C/p�C/�RC0
=C0\)C0�C1  C1Q�C1�\C1�
C2�C2ffC2�RC3{C3ffC3�RC4
=C4Q�C4�\C4�
C5�C5p�C5C6�C6ffC6�C7  C7Q�C7�\C7�
C8�C8ffC8C9(�C9z�C9C:
=C:Q�C:��C:��C;Q�C;�C<
=C<G�C<�\C<�HC=33C=��C=��C>G�C>�\C>�C?(�C?z�C?�
C@33C@�\C@�HCA33CAp�CA��CB(�CB�CB�
CC(�CCp�CC�CD  CD\)CDCE{CEQ�CE��CE��CFG�CF��CG  CGQ�CG�\CG�
CH�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�7LA�7LA��A�A���A��A��A���AՃA�r�A�p�A�A�A��A��TA��/A��
A���A�ĜAԼjA԰!Aԩ�AԋDA�1A�l�A�I�A�+A���A��
AҋDA�(�AѾwA�=qA��AЕ�A�Q�A�7LA�9XA�5?A�"�A�JA��A���A�ĜAϝ�A�z�A�jA�K�A�/A��AΏ\A�%A�C�A��;A̅A�/A��TA�=qAʇ+A�bA�1A�A�G�A��A��
A�p�A��/AǶFA�I�AƗ�A�-A�x�Aė�A�7LA�S�A���A�ĜA���A��;A��yA�$�A��RA���A��;A�1'A��A�%A��A�+A��A���A��\A��-A��A�bA�-A��
A���A�n�A���A�33A�?}A���A��DA�bNA��jA�bNA��A���A���A�v�A�I�A��;A�^5A���A��DA�A��DA��A�oA�"�A��hA�M�A�C�A���A��A���A��Axz�AvbAt�Am��Aix�Af�\Ab�9A[��AY�
AWXATĜAQ��AM�hAH^5AE�mAD=qAC�-AB�ABjA@Q�A?&�A>1A<�!A;&�A7K�A5��A4�`A4JA2�/A0r�A.9XA,9XA++A)��A(jA'\)A'XA';dA&ȴA&��A&�A&�RA&M�A&  A$~�A#"�A#hsA#�-A#?}A#"�A"�A"jA!�^A ��A 9XA 5?A��A/A�A5?A�;A��A�/A�mA�AQ�A�`AbA�A�AVA��A�A�\AA�AA�A5?AAhsA
�/A
z�A
jA
{A	��A	�hA	t�A	+A��AJA�7A�7A�A�jAƨAS�A��AbA�PA�A�A\)A?}A&�A��AM�A ��A J@�S�@�+@���@��@��m@��@�^5@���@�1@�n�@�O�@��@�\@��@�^@�hs@�v�@�{@�X@蛦@�  @�9X@��@�l�@��
@���@�\@��T@���@���@���@�j@���@���@��@�=q@�V@���@��
@�o@��#@�x�@�?}@��@؃@�\)@��H@�V@�E�@�=q@�-@�-@�^5@�ff@�=q@�%@�1'@�ƨ@�S�@���@�ȴ@ҧ�@�J@��T@Ѻ^@�7L@�r�@��m@ϥ�@�S�@��H@͉7@��`@���@̼j@̓u@�bN@�Q�@�9X@���@˅@�|�@�33@ʗ�@�5?@�@�p�@ȋD@�I�@��
@�dZ@�C�@���@�~�@�@ř�@�p�@�X@�O�@�?}@�V@�Z@Å@�ȴ@\@�M�@��@�x�@�/@��@�b@�C�@���@�=q@��@��@���@�X@�?}@��u@�dZ@���@��H@��\@�{@��#@��-@�&�@���@�r�@�1@��P@�33@�"�@��H@��+@�5?@��@�hs@�%@�bN@�+@�^5@��@�7L@��@��/@�Q�@�(�@���@��@���@��P@�|�@�
=@��y@��R@��+@�n�@��@��7@�/@��9@�I�@�b@��m@�dZ@�ff@�J@��#@���@�G�@��`@�j@���@�"�@��@���@�=q@��#@���@�/@��9@�j@� �@��@�@��@���@�v�@�5?@�@��T@���@�X@�%@�r�@�1'@��@��P@�;d@�o@���@���@�-@��@���@�p�@�X@�/@���@�ƨ@�|�@�t�@�S�@���@�v�@�J@�/@���@��`@��`@��/@��`@���@�r�@��F@�\)@���@��+@�E�@��^@�hs@�7L@���@��9@��@�1'@���@��m@��
@��F@�|�@�33@�@��@���@�v�@�$�@��#@��@�G�@�G�@�?}@�%@��/@���@���@�9X@� �@�  @��m@��P@�dZ@�;d@���@��@�^5@��@��#@�@���@��h@��@�7L@�%@���@���@�Q�@�A�@� �@��
@���@�C�@�o@���@��y@���@�v�@��+@�-@��@��-@�X@�/@�&�@���@��j@�b@���@�l�@�l�@�dZ@�\)@�C�@�33@���@��R@���@�~�@��@�@���@�X@��@���@�Ĝ@�Z@��@��;@��;@���@��F@��P@���@�~�@�{@��-@��h@�G�@���@�Ĝ@��@�Z@�1'@�w@\)@\)@K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�5?A�9XA�7LA�1'A�9XA�9XA�33A�1'A��A�VA��A��A�%A�A���A�A�  A�  A���A���A��A��A��A��A��#A�ȴA���A��#A���A��;AվwAգ�AՋDAՉ7AՃA�~�A�x�A�r�A�t�A�t�A�l�A�l�A�r�A�r�A�l�A�VA�=qA�5?A� �A��A��A��A��A��TA��HA��`A��A��yA��A��yA��HA��TA��HA��;A��TA��HA��/A��HA��;A��#A��/A��;A��A��A��/A��/A��A��
A��#A��A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA�ƨA�ȴA�ĜA���A���A�AԾwAԾwA���AԼjAԺ^AԼjAԸRAԲ-AԴ9AԴ9A԰!AԮAԲ-AԬAԬA԰!AԬAԮA԰!Aԩ�Aԩ�AԬAԥ�Aԣ�Aԥ�Aԝ�AԑhAԓuAԑhAԇ+AԁAԁA�jA�dZA�XA�E�A�$�A��
Aӡ�Aӗ�Aӏ\AӇ+A�t�A�hsA�dZA�dZA�`BA�XA�XA�VA�M�A�I�A�I�A�C�A�?}A�?}A�=qA�7LA�1'A�/A�-A� �A�oA�JA�
=A�  A���A��A��A��TA��mA��`A��HA��TA��/A���A���A�AҴ9Aҧ�Aҡ�Aҙ�A҃A�z�A�v�A�jA�XA�K�A�=qA�+A��A��A�bA�1A�A���A��yAѴ9Aѣ�Aѝ�Aя\Aч+A�x�A�bNA�M�A�5?A�$�A��A�JA�A���A���A��A��A��A��`A��/A���AС�AГuAЉ7AЇ+A�~�A�v�A�v�A�p�A�bNA�XA�M�A�C�A�9XA�33A�5?A�9XA�5?A�33A�7LA�;dA�7LA�5?A�=qA�?}A�;dA�9XA�7LA�5?A�5?A�7LA�5?A�33A�5?A�33A�-A�33A�9XA�/A�+A�(�A�(�A��A�bA��A��A�bA�VA�VA�{A�VA�A���A���A���A��A��yA��`A��;A��A���A���A���A���A���A���A���A���A���A���A�ĜA�A�ĜAϼjA϶FAϰ!Aϰ!Aϥ�Aϙ�AϑhAϓuAϑhAχ+A�~�A�|�A�|�A�x�A�t�A�r�A�t�A�r�A�l�A�jA�jA�hsA�dZA�^5A�\)A�ZA�S�A�M�A�K�A�G�A�=qA�9XA�9XA�9XA�5?A�/A�+A�+A��A�1A�  A�  A���A��A���A���AξwAΰ!AΙ�AΕ�AΏ\A΍PA�x�A�jA�Q�A�G�A�1'A��A���A��HA�ȴAͮA͋DA�\)A�O�A�=qA�(�A��A�{A�VA�  A���A��mA���A�ƨA���A̸RA̴9A̩�A̕�A�~�A�t�A�jA�S�A�E�A�?}A�9XA�33A�(�A��A��A��A�
=A�  A���A��A��A˼jAˡ�AˋDA�x�A�dZA�I�A�"�A�1A���A��yA�ȴAʮAʛ�Aʇ+A�`BA�9XA�$�A�{A�VA�JA�VA�oA�oA�VA�1A�
=A�JA�JA�1A�A�A�%A�1A�1A�A�  A�  A�A���A��Aɛ�A�\)A�K�A�?}A�+A��A�oA�A���A��A��A��A��A��yA��yA��A��A��`A��/A��
A�ƨAȰ!Aȏ\Aȏ\Aȇ+AȁA�z�A�bNA�I�A�&�A�A��`A��/A���A���A���A���A���A�ĜA���AǼjAǸRAǲ-Aǥ�Aǝ�AǗ�AǑhAǇ+A�t�A�VA�&�A�A���AƲ-Aƥ�Aƥ�Aƣ�Aƣ�AƝ�AƋDA�dZA�ZA�VA�I�A�C�A�5?A� �A�bA�A��A��mA��TA���Aś�A�`BA�/A�JA���A��yA��#AĸRAğ�Ać+A�x�A�t�A�n�A�jA�ffA�^5A�M�A�A�A�/A��A��A�1A��A���AÍPA�hsA�E�A�$�A���A��AA�=qA�  A��A��/A��RA��A�O�A�oA���A��A��#A���A�ƨA��wA���A��uA��\A�|�A�Q�A��A���A�v�A�$�A��`A���A���A�G�A�1A��`A��
A�ȴA��jA���A��PA�ZA�A���A�E�A���A���A�M�A�bA���A���A�l�A�O�A�A�A� �A�  A��yA��jA��PA�^5A�(�A��TA���A�v�A�\)A�Q�A�C�A�/A�&�A� �A��A�VA���A��
A��\A��TA�z�A�A�A��A��A�ffA�C�A��A��A��+A�\)A�/A�
=A��`A�ƨA�ƨA��A���A���A��A�x�A�v�A�n�A�ZA�?}A��A��A�;dA�XA�+A��A�|�A�O�A�A���A��;A�ƨA��A��uA�t�A�XA�7LA��A�bA�A��A��/A�ȴA��A��\A�p�A�Q�A�33A�{A�%A�A�  A�  A���A��A��A�ȴA��FA���A���A��A�=qA�bA���A��;A�ȴA���A��A�dZA�VA�K�A�K�A�;dA�oA��A��A��\A�~�A�^5A�1A���A��DA�x�A�ZA�7LA�"�A��A�JA�A���A��A��HA��A���A�A��RA��9A��A���A���A���A���A�|�A�G�A���A�ƨA�A�A��DA�E�A�-A�A��mA���A���A��\A�v�A�hsA�ZA�I�A�/A�%A�ĜA���A�~�A�O�A� �A��A��RA�z�A�A�A��HA��A���A���A��hA��PA��7A��+A��A��A�z�A�x�A�n�A�^5A�K�A�G�A�=qA�$�A��A��#A���A�=qA���A���A�x�A�ZA�G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�7LA�7LA��A�A���A��A��A���AՃA�r�A�p�A�A�A��A��TA��/A��
A���A�ĜAԼjA԰!Aԩ�AԋDA�1A�l�A�I�A�+A���A��
AҋDA�(�AѾwA�=qA��AЕ�A�Q�A�7LA�9XA�5?A�"�A�JA��A���A�ĜAϝ�A�z�A�jA�K�A�/A��AΏ\A�%A�C�A��;A̅A�/A��TA�=qAʇ+A�bA�1A�A�G�A��A��
A�p�A��/AǶFA�I�AƗ�A�-A�x�Aė�A�7LA�S�A���A�ĜA���A��;A��yA�$�A��RA���A��;A�1'A��A�%A��A�+A��A���A��\A��-A��A�bA�-A��
A���A�n�A���A�33A�?}A���A��DA�bNA��jA�bNA��A���A���A�v�A�I�A��;A�^5A���A��DA�A��DA��A�oA�"�A��hA�M�A�C�A���A��A���A��Axz�AvbAt�Am��Aix�Af�\Ab�9A[��AY�
AWXATĜAQ��AM�hAH^5AE�mAD=qAC�-AB�ABjA@Q�A?&�A>1A<�!A;&�A7K�A5��A4�`A4JA2�/A0r�A.9XA,9XA++A)��A(jA'\)A'XA';dA&ȴA&��A&�A&�RA&M�A&  A$~�A#"�A#hsA#�-A#?}A#"�A"�A"jA!�^A ��A 9XA 5?A��A/A�A5?A�;A��A�/A�mA�AQ�A�`AbA�A�AVA��A�A�\AA�AA�A5?AAhsA
�/A
z�A
jA
{A	��A	�hA	t�A	+A��AJA�7A�7A�A�jAƨAS�A��AbA�PA�A�A\)A?}A&�A��AM�A ��A J@�S�@�+@���@��@��m@��@�^5@���@�1@�n�@�O�@��@�\@��@�^@�hs@�v�@�{@�X@蛦@�  @�9X@��@�l�@��
@���@�\@��T@���@���@���@�j@���@���@��@�=q@�V@���@��
@�o@��#@�x�@�?}@��@؃@�\)@��H@�V@�E�@�=q@�-@�-@�^5@�ff@�=q@�%@�1'@�ƨ@�S�@���@�ȴ@ҧ�@�J@��T@Ѻ^@�7L@�r�@��m@ϥ�@�S�@��H@͉7@��`@���@̼j@̓u@�bN@�Q�@�9X@���@˅@�|�@�33@ʗ�@�5?@�@�p�@ȋD@�I�@��
@�dZ@�C�@���@�~�@�@ř�@�p�@�X@�O�@�?}@�V@�Z@Å@�ȴ@\@�M�@��@�x�@�/@��@�b@�C�@���@�=q@��@��@���@�X@�?}@��u@�dZ@���@��H@��\@�{@��#@��-@�&�@���@�r�@�1@��P@�33@�"�@��H@��+@�5?@��@�hs@�%@�bN@�+@�^5@��@�7L@��@��/@�Q�@�(�@���@��@���@��P@�|�@�
=@��y@��R@��+@�n�@��@��7@�/@��9@�I�@�b@��m@�dZ@�ff@�J@��#@���@�G�@��`@�j@���@�"�@��@���@�=q@��#@���@�/@��9@�j@� �@��@�@��@���@�v�@�5?@�@��T@���@�X@�%@�r�@�1'@��@��P@�;d@�o@���@���@�-@��@���@�p�@�X@�/@���@�ƨ@�|�@�t�@�S�@���@�v�@�J@�/@���@��`@��`@��/@��`@���@�r�@��F@�\)@���@��+@�E�@��^@�hs@�7L@���@��9@��@�1'@���@��m@��
@��F@�|�@�33@�@��@���@�v�@�$�@��#@��@�G�@�G�@�?}@�%@��/@���@���@�9X@� �@�  @��m@��P@�dZ@�;d@���@��@�^5@��@��#@�@���@��h@��@�7L@�%@���@���@�Q�@�A�@� �@��
@���@�C�@�o@���@��y@���@�v�@��+@�-@��@��-@�X@�/@�&�@���@��j@�b@���@�l�@�l�@�dZ@�\)@�C�@�33@���@��R@���@�~�@��@�@���@�X@��@���@�Ĝ@�Z@��@��;@��;@���@��F@��P@���@�~�@�{@��-@��h@�G�@���@�Ĝ@��@�Z@�1'@�w@\)@\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�5?A�9XA�7LA�1'A�9XA�9XA�33A�1'A��A�VA��A��A�%A�A���A�A�  A�  A���A���A��A��A��A��A��#A�ȴA���A��#A���A��;AվwAգ�AՋDAՉ7AՃA�~�A�x�A�r�A�t�A�t�A�l�A�l�A�r�A�r�A�l�A�VA�=qA�5?A� �A��A��A��A��A��TA��HA��`A��A��yA��A��yA��HA��TA��HA��;A��TA��HA��/A��HA��;A��#A��/A��;A��A��A��/A��/A��A��
A��#A��A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA�ƨA�ȴA�ĜA���A���A�AԾwAԾwA���AԼjAԺ^AԼjAԸRAԲ-AԴ9AԴ9A԰!AԮAԲ-AԬAԬA԰!AԬAԮA԰!Aԩ�Aԩ�AԬAԥ�Aԣ�Aԥ�Aԝ�AԑhAԓuAԑhAԇ+AԁAԁA�jA�dZA�XA�E�A�$�A��
Aӡ�Aӗ�Aӏ\AӇ+A�t�A�hsA�dZA�dZA�`BA�XA�XA�VA�M�A�I�A�I�A�C�A�?}A�?}A�=qA�7LA�1'A�/A�-A� �A�oA�JA�
=A�  A���A��A��A��TA��mA��`A��HA��TA��/A���A���A�AҴ9Aҧ�Aҡ�Aҙ�A҃A�z�A�v�A�jA�XA�K�A�=qA�+A��A��A�bA�1A�A���A��yAѴ9Aѣ�Aѝ�Aя\Aч+A�x�A�bNA�M�A�5?A�$�A��A�JA�A���A���A��A��A��A��`A��/A���AС�AГuAЉ7AЇ+A�~�A�v�A�v�A�p�A�bNA�XA�M�A�C�A�9XA�33A�5?A�9XA�5?A�33A�7LA�;dA�7LA�5?A�=qA�?}A�;dA�9XA�7LA�5?A�5?A�7LA�5?A�33A�5?A�33A�-A�33A�9XA�/A�+A�(�A�(�A��A�bA��A��A�bA�VA�VA�{A�VA�A���A���A���A��A��yA��`A��;A��A���A���A���A���A���A���A���A���A���A���A�ĜA�A�ĜAϼjA϶FAϰ!Aϰ!Aϥ�Aϙ�AϑhAϓuAϑhAχ+A�~�A�|�A�|�A�x�A�t�A�r�A�t�A�r�A�l�A�jA�jA�hsA�dZA�^5A�\)A�ZA�S�A�M�A�K�A�G�A�=qA�9XA�9XA�9XA�5?A�/A�+A�+A��A�1A�  A�  A���A��A���A���AξwAΰ!AΙ�AΕ�AΏ\A΍PA�x�A�jA�Q�A�G�A�1'A��A���A��HA�ȴAͮA͋DA�\)A�O�A�=qA�(�A��A�{A�VA�  A���A��mA���A�ƨA���A̸RA̴9A̩�A̕�A�~�A�t�A�jA�S�A�E�A�?}A�9XA�33A�(�A��A��A��A�
=A�  A���A��A��A˼jAˡ�AˋDA�x�A�dZA�I�A�"�A�1A���A��yA�ȴAʮAʛ�Aʇ+A�`BA�9XA�$�A�{A�VA�JA�VA�oA�oA�VA�1A�
=A�JA�JA�1A�A�A�%A�1A�1A�A�  A�  A�A���A��Aɛ�A�\)A�K�A�?}A�+A��A�oA�A���A��A��A��A��A��yA��yA��A��A��`A��/A��
A�ƨAȰ!Aȏ\Aȏ\Aȇ+AȁA�z�A�bNA�I�A�&�A�A��`A��/A���A���A���A���A���A�ĜA���AǼjAǸRAǲ-Aǥ�Aǝ�AǗ�AǑhAǇ+A�t�A�VA�&�A�A���AƲ-Aƥ�Aƥ�Aƣ�Aƣ�AƝ�AƋDA�dZA�ZA�VA�I�A�C�A�5?A� �A�bA�A��A��mA��TA���Aś�A�`BA�/A�JA���A��yA��#AĸRAğ�Ać+A�x�A�t�A�n�A�jA�ffA�^5A�M�A�A�A�/A��A��A�1A��A���AÍPA�hsA�E�A�$�A���A��AA�=qA�  A��A��/A��RA��A�O�A�oA���A��A��#A���A�ƨA��wA���A��uA��\A�|�A�Q�A��A���A�v�A�$�A��`A���A���A�G�A�1A��`A��
A�ȴA��jA���A��PA�ZA�A���A�E�A���A���A�M�A�bA���A���A�l�A�O�A�A�A� �A�  A��yA��jA��PA�^5A�(�A��TA���A�v�A�\)A�Q�A�C�A�/A�&�A� �A��A�VA���A��
A��\A��TA�z�A�A�A��A��A�ffA�C�A��A��A��+A�\)A�/A�
=A��`A�ƨA�ƨA��A���A���A��A�x�A�v�A�n�A�ZA�?}A��A��A�;dA�XA�+A��A�|�A�O�A�A���A��;A�ƨA��A��uA�t�A�XA�7LA��A�bA�A��A��/A�ȴA��A��\A�p�A�Q�A�33A�{A�%A�A�  A�  A���A��A��A�ȴA��FA���A���A��A�=qA�bA���A��;A�ȴA���A��A�dZA�VA�K�A�K�A�;dA�oA��A��A��\A�~�A�^5A�1A���A��DA�x�A�ZA�7LA�"�A��A�JA�A���A��A��HA��A���A�A��RA��9A��A���A���A���A���A�|�A�G�A���A�ƨA�A�A��DA�E�A�-A�A��mA���A���A��\A�v�A�hsA�ZA�I�A�/A�%A�ĜA���A�~�A�O�A� �A��A��RA�z�A�A�A��HA��A���A���A��hA��PA��7A��+A��A��A�z�A�x�A�n�A�^5A�K�A�G�A�=qA�$�A��A��#A���A�=qA���A���A�x�A�ZA�G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B4nB4nB2�B2�B1�B2-B/�B1�B.B-CB+6B+kB(�B)�B*0B*�B+6B+�B,B,qB-CB9$Bt�B�XB��B��B�LB�zB��B�[B��B�0B�FB��B�1B��B��B�B�0B��B�'B�'B��B��B��B�hB��B��B�?B�aB��B�[B��B��B��B��B�'B�3B��B�qB��B�)B�B��B��B�aB�HB��B��B��B��B	�B	B'�B3hB<6BD�B9�B<�B6FB:�B7B5�B3�B4�BB�BS�BS&BW
BV�BY�B]/B[�B\�BZ�BW�BT�BXEBEmB?}B:*B1�B&�B#nB"�B�B
rB�B�oB�,B��B�B�}B�RB�YB��B� BcTBVmB=B�B
��B
�mB
�*B
��B
l�B
^jB
33B
�B
�B	�mB	��B	��B	�fB	o5B	MjB	LdB	1[B	8B	1�B	 'B	�B	 B	�B	�B	hB	!�B	+B	,�B	$�B	&B	+B	fB	oB�VB��B�B��B�B�B�B�B�vB	�B	�B	�B	'�B	5B	MjB	[�B	_B	g�B	o5B	�B	�B	�hB	�bB	�:B	��B	��B	�B	�_B	�*B	�B	��B	�B	�HB	��B	�qB	�-B	��B	�:B	��B	�B	�"B	�rB	��B	�B	}�B	z�B	{B	|�B	�B	��B	��B	��B	��B	�YB	��B	�B	��B	�\B	��B	��B	��B	��B	��B	��B	� B	�VB	��B	��B	�B	�fB	��B	��B	�FB	��B	��B	��B	�nB	��B	�B	��B	�B	��B	��B	�B	�3B	��B	�9B	��B	�aB	�B	�RB	��B	��B	�aB	�B	��B	�-B	�$B	�B	��B	��B	��B	��B	�6B	�BB	�B	�EB	��B	ȴB	��B	�B	��B	�wB	��B	�zB	��B	��B	ӏB	�NB	�B	�HB	�B	�XB	�XB	��B	� B	�&B	��B	�9B	��B	��B	��B	��B	�QB	�#B	�#B	��B	�/B	��B	��B	��B	��B	��B	�)B	�]B	�pB	�HB	��B	�B	�;B	��B	��B	�pB	��B	�B	�B	�|B	�NB	��B	�B	��B	�B	�B	�B	� B	�B	� B	��B	�B	��B	�B	�B	�B	�
B	�B	�DB	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�)B	�)B	�B	�B	�B	��B	�|B	�B	�B	�B	�TB	��B	�%B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�]B	��B	�.B	��B	��B
 4B
 iB
 �B
 �B
oB
�B
�B
MB
�B
�B
%B
+B
fB
fB
	B
	B
	lB

�B
�B
�B
"B
�B
(B
�B
�B
.B
�B
4B
 B
�B
�B
hB
�B
�B
�B
�B
�B
:B
�B
�B
{B
{B
�B
MB
�B
�B
�B
�B
�B
�B
�B
B
�B
kB
7B
	B
=B
=B
�B
B
�B
B
B
�B
�B
 \B
 'B
 \B
 �B
"hB
"hB
#B
#B
"�B
#B
$B
%�B
%B
$�B
%B
'RB
&LB
'�B
(�B
(�B
(�B
(XB
(�B
(XB
'�B
(XB
)�B
)_B
*eB
)�B
*eB
,B
+�B
,B
,�B
,�B
-CB
-�B
.B
.B
.IB
.IB
.�B
/B
/B
/OB
/OB
0UB
1'B
1�B
2aB
2�B
2aB
2�B
33B
3hB
33B
49B
5?B
5?B
5�B
5�B
7B
6�B
7�B
7LB
7LB
8RB
8B
8B
8RB
7�B
7�B
7LB
8�B
8B
8�B
8�B
9�B
9XB
9�B
:*B
:�B
:�B
:�B
:�B
:�B
;�B
<jB
<�B
=B
<jB
<�B
=�B
=<B
=<B
=�B
=�B
?HB
>�B
?}B
@�B
@�B
@�B
A�B
AUB
A�B
AUB
AUB
A�B
B'B
A�B
AUB
AUB
AUB
AUB
AUB
A�B
B'B
B[B
B[B
B'B
B[B
B�B
C�B
DgB
E9B
E�B
E�B
F?B
F�B
GEB
G�B
G�B
H�B
H�B
IRB
I�B
I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B3�B4�B3�B3hB5tB2�B4B6zB4�B1[B4B-CB5tB4�B0�B1�B3hB33B0�B33B2�B3hB2aB1�B5B2�B1�B0�B0�B.�B0UB2�B.�B2�B+B.�B*�B-wB/B,�B,�B-CB,B)�B*0B-CB,qB&�B+B/B'�B'�B1[B'�B'B'�B&�B($B)�B(�B(�B*�B(�B)�B+B(�B)�B*�B(�B)�B+B)�B)*B+6B+B)�B)�B+B+�B)�B*0B,qB+�B)�B+6B,B+B*�B,=B+�B*0B,=B,B+�B*�B+�B-B,B*�B,�B,qB*�B,=B-B+B,=B-�B+6B+6B-B-�B+kB-wB-CB+kB,�B,�B+�B-�B,�B+�B.}B/B-�B0�B7LB:^B:�B<�B<�B:�BB�BE�BH�BS[Bd�B�FB�*B��B��B��B�B��B�*B��B��B��B��B��B��B�*B�RB�XB��B�XB��B��B��B��B�LB��B��B�B��B�B�B�FB�LB��B��B�zB�B��B�B��B�FB�FB�9B��B��B��B�B�hB��B��B��B��B�[B��B�-B�!B��B�!B�OB��B��B��B�B�wB��B��B��B�B�qB��B��B�LB��B�RB�B��B�FB�B��B��B��B�XB��B�\B�OB�	B�CB��B�qB�	B��B��B�1B�SB�SB�B�B�MB�YB��B��B�SB��B��B��B�=B�VB�VB�B��B�:B��B��B�B�tB�B�B��B��B��B��B��B��B�B��B��B��B�CB�}B�IB�wB�B�[B��B��B�UB��B�-B��B��B�}B�B�OB��B�[B��B��B��B��B��B�-B�9B��B�aB��B�9B�B��B�nB�hB�nB��B�-B�9B�?B��B��B��B�B�B��B��B��B�9B��B��B�hB�B��B�hB��B��B�nB�B�B��B��B�hB�9B�tB�tB��B��B�B��B�nB�B�B�LB�B�B�-B�9B��B��B��B��B��B�9B�'B�-B��B�nB��B��B�[B�9B�nB��B��B�'B��B�OB�IB��B�wB��B��B�}B��B��B�B�}B��B��B��B��B��B�'B��B�OB��B�UB�UB��B�B�OB�!B�B��B�OB��B�aB��B��B��B��B��B��B�!B��B�hB��B�[B��B�tB��B�nB�nB�B��B��B��B��B�^B��B�qB�B��B��B�BB��B��B�6B��B��B�B�qB�B�6B�B˒B��BȀB�^B�dB�jB��B�jB�<B�B�0B˒B̘B��B�jB��B̘B�vBΥBϫB��B�HB�NB�TB�,B��B�B��B�mB�KB�#B�,B�&B��B��B��BуB�TB� B�NBѷB��B�BϫB��B͟B�dB��B��BѷB��B��B�B��B�KB�
B�mB�gB�mB�WB�B��B��B�B�pB��B�B�B�fB�B�5B�lBB�B�B�BGB �B �B�B	lB	lB
rB
�BB"B�B B�B�B�B!bB�B�B�B �B#�B*�B'RB'�B&�B'�B+6B/OB/�B0�B/OB2-B3hB8B5�B<6B8�B:�B=<B<�B:�B;�B>wB@�B<jB>�BB�BB�BIBK^BJ�BCaB>�BAUBDgB<jB8�B7LB5�B4�B5B6FB?HB<6BA�B:^B>BB=<B?�B8�B9�B7LB5tB2�B5B6B6zB5tB:�B5�B9�B7�B9XBB�B:�B8RB8RB9XB7�B7LB4�B2�B3hB4B6zB:*BN<B8�B2�B6zB7�B2�B/OB1'B4�B7LB2-B49B3hB33B5tB1'B8RB1�B2�B5�B5�B4B3�B4�B8RB8RB9XB\�BNpB:�BD3BGBxBL�BNpBOBPHBO�BO�BR�BR�BUgBTaBRTBQ�BR�BS�BTaBVmBXBYBW�BXyBYKBW?BV�BV�BVBT�BV�BXyBX�BX�BW
BV�BZ�B`�B]�B[WB\�B\�B_pB^�B\�B[�B[�BZB]/B\�B]/B\]BW
BXyB\�BgB]�BYKBYB\)B\�BZQBZBY�BYKBX�BYBW
BV�BW
BV�BW�BW
BV�BV�BS�BS�BR�BW
BX�BW?BW�Be�BW�BNBGzBH�BFBF�BG�BAUBB'B?�B>B=<B>wB?HBB�B<�B8RB?}B7�B8�B:�B5�B:�B9�B,=B(XB%�B(�B'B(XB&B%zB$@B$�B$B%FB%FB%�BOB�B#�B \B'�B'�B%�BBBBoB�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022060605084420220606050844IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022061600011420220616000114QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�205F03E         200703E         AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022061600011420220616000114QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                