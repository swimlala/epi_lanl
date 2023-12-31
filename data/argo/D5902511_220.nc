CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-07-16T09:04:05Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       Y�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       v�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�          	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` /0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   /�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   5�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ;�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T A�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   A�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   A�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   A�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   A�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � B   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   B�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   B�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    B�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        B�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        B�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       B�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    B�Argo profile    3.1 1.2 19500101000000  20220716090405  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_220                 6810_008521_220                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�ߑ���@�ߑ���11  @�ߒI�^@�ߒI�^@1�s-�@1�s-��d�qu�!��d�qu�!�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @�  @�  @�  @�  A   A  A\)A+�A@  A_\)A�  A�  A�  A�\)A�  A�  A߮A�A��B  B  B(�B Q�B((�B0  B8  B@(�BH  BO�
BW�B_�Bh(�Bo�
Bx  B�{B�  B��B��
B��B��B�  B�{B�{B�  B�  B��B��B��
B��B�{B�{B�{B�{B�(�B�(�B�(�B�(�B�{B�{B�  B�  B�{B�  B��B��
B�  C 
=C��C��C
=C��C	��C
=C{C
=C
=C  C
=C  C�C
=C  C��C"  C$
=C&
=C(  C*  C,  C.  C0  C2{C4  C6  C8  C:
=C;�C>  C@
=CB{CD
=CF  CH  CJ
=CK�CN  CO��CR  CT{CV  CX  CZ
=C\
=C^  C`  Cb
=Cd
=Ce��Ch  Cj
=Cl  Cn
=Cp
=Cr  Ct  Cu�Cw�Cz  C|{C~
=C�  C�C�C�C�
=C�C�
=C�C�  C�  C�  C���C���C�  C�C�  C�  C���C���C���C���C�C�  C���C�  C�  C���C�  C�  C���C�  C���C�  C�C�  C�  C�  C�  C���C���C���C���C�  C�C�  C���C���C�  C�C�C�C�C�C�
=C�
=C�C�C�  C�  C�  C���C���C�C�  C���C�  C�  C�C�  C���C�  C�C�  C�  C�C�
=C�  C���C�  C�  C�  C���C�  C�
=C�C���C���C�  C�
=C�
=C�C�C�C�C�C���C���C���C���C�  C�C�
=C�C�  C�  C�  C�  C�  C�  C���C�C�
=C�\C�  C���C���C�  C�  C�C�  C���C���C�  C�C�C�C�C�  D   D � D �qD}qD  D� D�D� D�qD� D�qD� D�D� D�qD�DD��D	  D	� D
  D
��D  Dz�D�qD� D�D� D�qD��DD� D�qD}qD  D� D�qD}qD  D��D  D� D  D}qD  D� D��D� D�D}qD�qD� D�D��D�D�D�qD}qD�D�DD��D�D��D �D }qD �qD!� D"  D"� D#  D#}qD$�D$��D%  D%}qD&  D&� D'  D'� D(�D(��D)�D)��D*  D*}qD*�qD+}qD+�qD,��D-D-� D.  D.��D/  D/��D0D0� D1  D1� D1�qD2� D3�D3��D4�D4��D5�D5}qD5��D6}qD7  D7��D8  D8}qD9�D9� D:  D:� D;  D;� D;�qD<}qD=�D=�D>�D>}qD?  D?� D@�D@�D@�qDA}qDB  DB}qDC  DC�DD�DD� DE  DE��DF  DF}qDG  DG� DH�DH��DI  DI}qDJ  DJ�DK�DK}qDK�qDL}qDM  DM� DM�qDN��DO�DO��DP�DP}qDQ  DQ�DR�DR� DS�DS� DT�DT��DU  DU}qDU��DV}qDW  DW� DX�DX��DY  DY��DZ�DZ��D[  D[� D[�qD\� D]�D]��D]�qD^}qD_  D_}qD_��D`}qDa  Da� Db�Db��Dc�Dc� Dd  Dd��De�De��Df�Df� Dg  Dg� Dh  Dh��Di  Diz�Di��Dj� Dk�Dk� Dl  Dl��Dm  Dm� Dn�Dn� Do  Do��Dp�Dp��Dq  Dq� Dq�qDrz�Ds  Ds�DtDt}qDt��Du��DvDv��Dw�Dw��Dx�Dx��Dy  Dy� Dz  Dz�D{D{��D|�D|�D}  D}��D~D~��D~�qDz�D�qD�AHD�� D��qD���D�@ D��HD��HD�  D�@ D�� D��HD�  D�@ D��HD��HD�  D�@ D��HD��HD�HD�AHD�� D�� D���D�@ D��HD�� D�HD�AHD�� D���D�  D�@ D�~�D��HD�HD�AHD��HD�� D�  D�B�D��HD���D���D�@ D��HD��HD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?u?��?���?\?�(�?��H@\)@(�@&ff@8Q�@B�\@L��@aG�@p��@}p�@�ff@���@��@�Q�@�  @��@���@�33@��H@��R@�ff@�\)@�33@�Q�@�\@�ff@���@�z�@��H@��RA�\AffA
=qA�A\)A�
A�A�A(�A�RA!G�A%A*=qA-p�A1G�A6ffA8Q�A<��AAG�AEAHQ�AL(�AQG�AU�AXQ�AZ�HA_\)Ac�
AeAj�HAn�RAqG�AuAy��A|(�A�Q�A��\A��
A�A�  A���A��\A��A��RA��A���A��
A��A�ffA���A��HA��
A�A�  A��A��HA�p�A�\)A�Q�A��A�(�A�A�
=A���A�33A��A�ffA�Q�A��\A��
A�p�A�  A��A�33A�z�AƸRA���Aʏ\A��
A�ffA�Q�A�G�A�33A��A�  A�G�A��HA���A޸RA���A�\A��
A�{A�Q�A�\A��
A�A�Q�A�=qA��
A�A�Q�A���A�33A�p�A��B ��BG�B=qB�BQ�B��B{B\)B(�B��B	�B
=B  B��B�B�HB�
B��B�B�HB�Bz�B�B�HB�Bz�B��B�HB�BQ�B��B�HB�B Q�B!��B"�HB#\)B$Q�B%B&�\B'33B((�B)G�B*�\B+\)B,  B-G�B.ffB/\)B0  B0��B2=qB3\)B4(�B4��B5�B7
=B8(�B8��B9B:�RB<  B=G�B=B>�RB@  BA�BABB�\BC�BD��BE�BFffBG\)BH��BI��BJffBK33BL  BM�BN{BO33BO�BPz�BQBR�RBS�BTz�BU�BV=qBW33BX(�BX��BYBZ�HB\(�B\��B]B^�RB_�
Ba�Ba�Bb�RBd  Be�Bf{Bf�HBg�Bh��BiBj�HBk�BlQ�BmG�Bn�\Bo�Bpz�BqG�Br=qBs�Bt��Bu��Bv�\Bw\)Bxz�By��Bz�RB{�B|Q�B}G�B~�\B�B�{B��\B�33B�B�(�B��\B�
=B��B�=qB���B��B��B�{B��RB�G�B��B�(�B��\B�33B��
B�Q�B��RB�G�B�  B�ffB��HB�\)B�  B��\B�
=B�p�B��B��\B�33B��B�{B���B�33B�B�=qB��RB�33B��B�z�B�
=B�\)B��
B�z�B��B��B�=qB��RB�33B��B�ffB���B�\)B��
B�z�B�
=B�p�B��
B�Q�B���B��B�  B�Q�B��HB�p�B�{B��\B���B��B�(�B��RB�\)B��
B�Q�B���B��B�(�B��RB��B��B�(�B��HB��B�(�B���B��B��B�=qB���B��B�(�B���B��B��B�=qB���B��B�{B��\B�
=B��B�ffB���B�p�B�  B���B�\)B�  B���B�33B��B�=qB��HB�p�B�(�B���B�\)B��
B�ffB���BîB�Q�B��HB�\)B��
B�ffB��BǮB�Q�B��HB�\)B��
B�ffB�
=BˮB�=qB���B�33B�B�Q�B�
=Bϙ�B�{BЏ\B�
=Bљ�B�Q�B���Bә�B�{Bԏ\B�
=Bՙ�B�=qB��HBׅB�{Bأ�B�
=Bٙ�B�(�B���BۅB�(�BܸRB�33B�B�ffB��B߮B�=qB�RB�G�B��B��B�G�B�B�=qB���B�p�B�  B�RB�\)B��B�Q�B��HB陚B�=qB��HB�G�B��
B�ffB��B��B�=qB���B�G�B��B��\B�33B��
B�z�B��B�B�=qB��RB�\)B�{B���B�G�B��
B�ffB�
=B��B�ffB�
=B���B�{B���B�G�B��B��\B�33B��
C 33C p�C C  CQ�C��C��CG�C��C�C33Cp�C�RC  CQ�C��C��CG�C��C��C33C�C��C
=C\)C��C��CG�C��C�C	33C	�C	C

=C
Q�C
��C
��C=qC�\C�HC�CffC��C�C33C�C�HC33Cz�CC��C33Cz�C�HC(�CffC��C�C33Cz�C�
C�CffC�RC��C=qCz�CC
=CffC�C��C=qC�\C�
C�CffC�C�C33Cz�C�RC  C=qC�\C�HC(�Cp�C�RC  C=qC�CC  CG�C�\C��C{CffC��C  C=qC�C��C{C\)C��C�HC33Cp�C�C�C 33C �C ��C!{C!\)C!�C!��C"=qC"�C"��C#{C#\)C#��C#�HC$(�C$ffC$�RC$��C%G�C%�\C%��C&�C&p�C&�RC'  C'=qC'�\C'�
C((�C(p�C(�C)  C)Q�C)��C)�C*33C*�C*��C+�C+p�C+�RC,  C,Q�C,��C,�C-33C-�\C-�
C.33C.p�C.��C/�C/\)C/�C0  C0Q�C0�C0��C1Q�C1�C2  C2Q�C2��C2��C3G�C3��C3�C4=qC4�\C4�HC533C5�C5�
C6(�C6z�C6��C7�C7z�C7C8{C8ffC8C9(�C9�C9�
C:33C:�\C:�HC;=qC;�C;�HC<=qC<��C=  C=\)C=C>{C>p�C>��C?�C?p�C?��C@33C@��C@��CA\)CA�RCB{CBp�CB��CC(�CCz�CC�
CD(�CD�\CD��CEQ�CE�RCF{CFp�CF�
CG33CG�\CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�\@@  @�  @�  @�  @�  A   A  A\)A+�A@  A_\)A�  A�  A�  A�\)A�  A�  A߮A�A��B  B  B(�B Q�B((�B0  B8  B@(�BH  BO�
BW�B_�Bh(�Bo�
Bx  B�{B�  B��B��
B��B��B�  B�{B�{B�  B�  B��B��B��
B��B�{B�{B�{B�{B�(�B�(�B�(�B�(�B�{B�{B�  B�  B�{B�  B��B��
B�  C 
=C��C��C
=C��C	��C
=C{C
=C
=C  C
=C  C�C
=C  C��C"  C$
=C&
=C(  C*  C,  C.  C0  C2{C4  C6  C8  C:
=C;�C>  C@
=CB{CD
=CF  CH  CJ
=CK�CN  CO��CR  CT{CV  CX  CZ
=C\
=C^  C`  Cb
=Cd
=Ce��Ch  Cj
=Cl  Cn
=Cp
=Cr  Ct  Cu�Cw�Cz  C|{C~
=C�  C�C�C�C�
=C�C�
=C�C�  C�  C�  C���C���C�  C�C�  C�  C���C���C���C���C�C�  C���C�  C�  C���C�  C�  C���C�  C���C�  C�C�  C�  C�  C�  C���C���C���C���C�  C�C�  C���C���C�  C�C�C�C�C�C�
=C�
=C�C�C�  C�  C�  C���C���C�C�  C���C�  C�  C�C�  C���C�  C�C�  C�  C�C�
=C�  C���C�  C�  C�  C���C�  C�
=C�C���C���C�  C�
=C�
=C�C�C�C�C�C���C���C���C���C�  C�C�
=C�C�  C�  C�  C�  C�  C�  C���C�C�
=C�\C�  C���C���C�  C�  C�C�  C���C���C�  C�C�C�C�C�  D   D � D �qD}qD  D� D�D� D�qD� D�qD� D�D� D�qD�DD��D	  D	� D
  D
��D  Dz�D�qD� D�D� D�qD��DD� D�qD}qD  D� D�qD}qD  D��D  D� D  D}qD  D� D��D� D�D}qD�qD� D�D��D�D�D�qD}qD�D�DD��D�D��D �D }qD �qD!� D"  D"� D#  D#}qD$�D$��D%  D%}qD&  D&� D'  D'� D(�D(��D)�D)��D*  D*}qD*�qD+}qD+�qD,��D-D-� D.  D.��D/  D/��D0D0� D1  D1� D1�qD2� D3�D3��D4�D4��D5�D5}qD5��D6}qD7  D7��D8  D8}qD9�D9� D:  D:� D;  D;� D;�qD<}qD=�D=�D>�D>}qD?  D?� D@�D@�D@�qDA}qDB  DB}qDC  DC�DD�DD� DE  DE��DF  DF}qDG  DG� DH�DH��DI  DI}qDJ  DJ�DK�DK}qDK�qDL}qDM  DM� DM�qDN��DO�DO��DP�DP}qDQ  DQ�DR�DR� DS�DS� DT�DT��DU  DU}qDU��DV}qDW  DW� DX�DX��DY  DY��DZ�DZ��D[  D[� D[�qD\� D]�D]��D]�qD^}qD_  D_}qD_��D`}qDa  Da� Db�Db��Dc�Dc� Dd  Dd��De�De��Df�Df� Dg  Dg� Dh  Dh��Di  Diz�Di��Dj� Dk�Dk� Dl  Dl��Dm  Dm� Dn�Dn� Do  Do��Dp�Dp��Dq  Dq� Dq�qDrz�Ds  Ds�DtDt}qDt��Du��DvDv��Dw�Dw��Dx�Dx��Dy  Dy� Dz  Dz�D{D{��D|�D|�D}  D}��D~D~��D~�qDz�D�qD�AHD�� D��qD���D�@ D��HD��HD�  D�@ D�� D��HD�  D�@ D��HD��HD�  D�@ D��HD��HD�HD�AHD�� D�� D���D�@ D��HD�� D�HD�AHD�� D���D�  D�@ D�~�D��HD�HD�AHD��HD�� D�  D�B�D��HD���D���D�@ D��HD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?u?��?���?\?�(�?��H@\)@(�@&ff@8Q�@B�\@L��@aG�@p��@}p�@�ff@���@��@�Q�@�  @��@���@�33@��H@��R@�ff@�\)@�33@�Q�@�\@�ff@���@�z�@��H@��RA�\AffA
=qA�A\)A�
A�A�A(�A�RA!G�A%A*=qA-p�A1G�A6ffA8Q�A<��AAG�AEAHQ�AL(�AQG�AU�AXQ�AZ�HA_\)Ac�
AeAj�HAn�RAqG�AuAy��A|(�A�Q�A��\A��
A�A�  A���A��\A��A��RA��A���A��
A��A�ffA���A��HA��
A�A�  A��A��HA�p�A�\)A�Q�A��A�(�A�A�
=A���A�33A��A�ffA�Q�A��\A��
A�p�A�  A��A�33A�z�AƸRA���Aʏ\A��
A�ffA�Q�A�G�A�33A��A�  A�G�A��HA���A޸RA���A�\A��
A�{A�Q�A�\A��
A�A�Q�A�=qA��
A�A�Q�A���A�33A�p�A��B ��BG�B=qB�BQ�B��B{B\)B(�B��B	�B
=B  B��B�B�HB�
B��B�B�HB�Bz�B�B�HB�Bz�B��B�HB�BQ�B��B�HB�B Q�B!��B"�HB#\)B$Q�B%B&�\B'33B((�B)G�B*�\B+\)B,  B-G�B.ffB/\)B0  B0��B2=qB3\)B4(�B4��B5�B7
=B8(�B8��B9B:�RB<  B=G�B=B>�RB@  BA�BABB�\BC�BD��BE�BFffBG\)BH��BI��BJffBK33BL  BM�BN{BO33BO�BPz�BQBR�RBS�BTz�BU�BV=qBW33BX(�BX��BYBZ�HB\(�B\��B]B^�RB_�
Ba�Ba�Bb�RBd  Be�Bf{Bf�HBg�Bh��BiBj�HBk�BlQ�BmG�Bn�\Bo�Bpz�BqG�Br=qBs�Bt��Bu��Bv�\Bw\)Bxz�By��Bz�RB{�B|Q�B}G�B~�\B�B�{B��\B�33B�B�(�B��\B�
=B��B�=qB���B��B��B�{B��RB�G�B��B�(�B��\B�33B��
B�Q�B��RB�G�B�  B�ffB��HB�\)B�  B��\B�
=B�p�B��B��\B�33B��B�{B���B�33B�B�=qB��RB�33B��B�z�B�
=B�\)B��
B�z�B��B��B�=qB��RB�33B��B�ffB���B�\)B��
B�z�B�
=B�p�B��
B�Q�B���B��B�  B�Q�B��HB�p�B�{B��\B���B��B�(�B��RB�\)B��
B�Q�B���B��B�(�B��RB��B��B�(�B��HB��B�(�B���B��B��B�=qB���B��B�(�B���B��B��B�=qB���B��B�{B��\B�
=B��B�ffB���B�p�B�  B���B�\)B�  B���B�33B��B�=qB��HB�p�B�(�B���B�\)B��
B�ffB���BîB�Q�B��HB�\)B��
B�ffB��BǮB�Q�B��HB�\)B��
B�ffB�
=BˮB�=qB���B�33B�B�Q�B�
=Bϙ�B�{BЏ\B�
=Bљ�B�Q�B���Bә�B�{Bԏ\B�
=Bՙ�B�=qB��HBׅB�{Bأ�B�
=Bٙ�B�(�B���BۅB�(�BܸRB�33B�B�ffB��B߮B�=qB�RB�G�B��B��B�G�B�B�=qB���B�p�B�  B�RB�\)B��B�Q�B��HB陚B�=qB��HB�G�B��
B�ffB��B��B�=qB���B�G�B��B��\B�33B��
B�z�B��B�B�=qB��RB�\)B�{B���B�G�B��
B�ffB�
=B��B�ffB�
=B���B�{B���B�G�B��B��\B�33B��
C 33C p�C C  CQ�C��C��CG�C��C�C33Cp�C�RC  CQ�C��C��CG�C��C��C33C�C��C
=C\)C��C��CG�C��C�C	33C	�C	C

=C
Q�C
��C
��C=qC�\C�HC�CffC��C�C33C�C�HC33Cz�CC��C33Cz�C�HC(�CffC��C�C33Cz�C�
C�CffC�RC��C=qCz�CC
=CffC�C��C=qC�\C�
C�CffC�C�C33Cz�C�RC  C=qC�\C�HC(�Cp�C�RC  C=qC�CC  CG�C�\C��C{CffC��C  C=qC�C��C{C\)C��C�HC33Cp�C�C�C 33C �C ��C!{C!\)C!�C!��C"=qC"�C"��C#{C#\)C#��C#�HC$(�C$ffC$�RC$��C%G�C%�\C%��C&�C&p�C&�RC'  C'=qC'�\C'�
C((�C(p�C(�C)  C)Q�C)��C)�C*33C*�C*��C+�C+p�C+�RC,  C,Q�C,��C,�C-33C-�\C-�
C.33C.p�C.��C/�C/\)C/�C0  C0Q�C0�C0��C1Q�C1�C2  C2Q�C2��C2��C3G�C3��C3�C4=qC4�\C4�HC533C5�C5�
C6(�C6z�C6��C7�C7z�C7C8{C8ffC8C9(�C9�C9�
C:33C:�\C:�HC;=qC;�C;�HC<=qC<��C=  C=\)C=C>{C>p�C>��C?�C?p�C?��C@33C@��C@��CA\)CA�RCB{CBp�CB��CC(�CCz�CC�
CD(�CD�\CD��CEQ�CE�RCF{CFp�CF�
CG33CG�\CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�VA�bA�oA�bA�VA�VA�JA�JA�JA�VA��A�&�A�&�A�"�A��A��A�"�A�"�A�(�A�%A��
A؟�A؏\A؋DA؅A؃A�z�A�x�A�t�A�jA�dZA�bNA�ZA�%A��TA��
A���Aԡ�A�VA�ȴA��A�S�AёhA��mAГuAЃA��`Aβ-A��A��#A͓uA�9XA�ffA�bA��#A˟�A��/Aɣ�Aȡ�AǛ�A�v�A���A�`BAĩ�A�%A�-A���A���A��yA��A�~�A�VA�  A��RA��A��A�hsA��A�^5A��9A�S�A��A�ffA�;dA�ĜA�z�A��#A��^A��A�7LA�{A�&�A� �A���A�jA��\A�VA���A�|�A�(�A�hsA��A�`BA���A�I�A�G�A�~�A��uA�=qA��A�Q�A�Q�A�
=A�jA�Q�A�ȴA�+A�-A�A{�7AwhsAsdZAq?}Ao�PAm\)Ag��Ab9XA_�wA^�A\n�A[t�AZ��AZr�AX�AQ\)AK;dAI�hAHz�AF�AC��A@(�A=XA<  A9A7�FA6�A4�jA1��A/��A.M�A-A*ffA'��A&~�A%�
A$��A#�A!�-A!�A �A�^A�!A/A{A�7AjA�^Ap�A�A�;AC�A�A�\A�#AA��A�FA
VA	��A	��A
jA
r�A
��A
=qA	��A�`A=qA�AC�A9XAO�AbNA�@��
@�/@���@��-@� �@�X@�dZ@�@�o@�t�@�z�@�Q�@���@�J@��H@�R@���@���@�I�@���@@�ȴ@�;d@�h@�r�@��@�n�@���@��;@��y@��@�h@���@� �@߮@�dZ@�33@�o@�O�@�bN@�9X@�b@��m@�ƨ@ۅ@ڧ�@ف@��@�1'@���@�x�@��;@ӶF@ӶF@ӶF@Ӯ@ӕ�@�S�@�33@�"�@��@�
=@��H@��@���@�1@�I�@˾w@�|�@�C�@�+@��y@���@�~�@ȋD@ǶF@��@���@�~�@�7L@Ĭ@��m@��@��@°!@�E�@�x�@�G�@��@���@��@���@��@���@��w@�dZ@�"�@�+@�\)@���@�b@�(�@�b@�K�@���@��+@���@�n�@���@���@�Ĝ@��@�r�@�(�@��F@�;d@��!@���@�~�@�~�@�v�@�5?@��@��^@���@�x�@�O�@���@�9X@�b@��F@�\)@�33@��@���@��\@�~�@�^5@�5?@��@���@��-@�G�@��`@��9@�Z@�1'@��@���@�dZ@�C�@���@��@�~�@�$�@��@�x�@�9X@�"�@�|�@��
@�1@�1'@�9X@�(�@���@�S�@�M�@��h@�@�j@��P@�
=@��H@��H@�o@�+@��y@��H@���@�@�v�@�=q@�-@���@� �@���@�~�@�M�@�=q@�@���@�x�@��7@��7@��7@��@�Z@�ƨ@��P@�"�@�
=@��y@���@���@�~�@�v�@�ff@�^5@�{@��7@�&�@��@��9@��9@�bN@�9X@�1@�|�@�33@�+@�o@�@��@�$�@���@��@��j@��9@���@��u@�r�@�I�@���@��!@�ff@���@��@�/@���@��@��@�Z@�9X@�1@��;@���@�C�@��@�~�@�=q@��#@���@�p�@�/@��@��j@��D@�A�@��@���@��
@�l�@�
=@�ȴ@��+@�^5@�=q@��@�J@���@�x�@��@��9@��D@�j@�9X@�b@���@���@��P@�|�@��@��y@�ff@�5?@�J@���@��@�/@��`@�z�@�  @��@��;@�ƨ@�dZ@�@���@���@�E�@�=q@�=q@�5?@�$�@�{@���@��@�@�p�@��@��9@�Ĝ@���@���@�  @��m@���@�C�@�dZ@�l�@�C�@��y@���@��+@�ff@�V@�=q@�@��^@�/@���@��9@�I�@�(�@�b@��@|�@~��@~��@~�+@~V@}�-@}/@|�@|�j@{��@z�H@y��@yhs@x��@w�w@wK�@w�@w�@v�@v�+@vff@v5?@v{@u��@up�@uV@t�/@t�@tI�@s��@r�!@rJ@q�7@p��@p�u@p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�bA�JA�VA�oA�JA�VA�oA�{A�VA��A�bA�bA��A�{A�bA�JA�oA�
=A�JA�oA�bA�
=A�bA�bA�
=A�VA�oA�JA�JA�bA�1A�1A�
=A�VA�
=A�1A�VA�VA�
=A�JA�bA�JA�JA�VA�VA�bA�oA��A�{A�$�A�+A� �A�"�A�-A�(�A�&�A�&�A�$�A� �A�$�A�"�A�(�A�+A�$�A�(�A�+A�&�A�"�A�/A�+A��A� �A�{A�{A��A��A��A��A��A��A��A��A��A��A� �A� �A��A��A��A��A��A��A�"�A��A��A�"�A�$�A�"�A�&�A�$�A� �A�"�A�"�A�$�A��A��A��A��A� �A�(�A�+A�7LA�7LA�$�A�"�A�$�A� �A� �A�"�A� �A���A��A���A�A��A��A�bA�A�
=A�JA�A���A���AؾwAغ^Aؙ�A؝�Aا�AؑhAؓuA؟�Aغ^AضFAؓuAؗ�Aؕ�A؏\A؏\AؓuA؏\A؋DA؍PAؑhA؍PA؉7A؍PA؍PA؉7A؉7A؍PA؋DA؇+A؃A؇+A؇+A؁A؁A؃A؇+A؃A؁A؃A؇+A؃A�~�A؃A؅A�~�A�~�A؁A�~�A�z�A�v�A�x�A�|�A�x�A�t�A�v�A�x�A�x�A�t�A�t�A�z�A�z�A�x�A�t�A�t�A�v�A�x�A�t�A�r�A�r�A�t�A�t�A�n�A�jA�hsA�jA�ffA�dZA�ffA�hsA�ffA�bNA�bNA�hsA�ffA�bNA�`BA�`BA�bNA�dZA�dZA�bNA�`BA�bNA�dZA�dZA�dZA�^5A�`BA�bNA�bNA�\)A�XA�M�A��A�{A�A���A�  A�
=A�1A�A���A���A��TA��yA���A���A��/A��/A׮AבhA׃A׃A�t�A�?}A�A�E�A��A���A��TA���A���A�ȴA���A�A�AԼjAԼjAԺ^AԮAԣ�Aԏ\Aԉ7AԃA�|�A�bNA�dZA�\)A�\)A�O�A�K�A�33A��A�%A��A��#AӶFAӗ�AӉ7AӃA�hsA�A�A�5?A�{A�%A���A��TAҼjA�|�A�l�A�ZA�G�A�5?A�-A�"�A�bA���AѸRAѩ�Aя\A�`BA�1'A�(�A��A�1A��mA��A���A���A�ƨAд9AЛ�AГuAЏ\AЏ\AЍPAЋDAЉ7AЍPAЇ+AЃAЃAЁAЁA�~�A�x�A�S�A�{A���A��mA���Aϥ�AύPA�O�A��A��
AΗ�A·+A�p�A�S�A�M�A�E�A�/A�JA�  A��A��A��A��yA��HA��A���A���A���A;wAʹ9Aͣ�A͟�A͇+A�t�A�l�A�ZA�A�A�;dA�;dA�7LA�33A�$�A��
Ạ�A�t�A�XA�K�A�A�A�1'A�$�A��A�{A�VA�VA�A���A���A��A��yA��;A���A˾wA˴9A˰!A˲-A˰!AˮA˟�A�z�A�t�A�p�A�n�A�C�A��A�~�A�bNA�5?A�A�A���AɶFA�v�A�Q�A�5?A��A�{A�  AȶFA�l�A�33A�"�A��A�
=A���A���AǏ\A�$�A���A���AƼjAƝ�A�|�A�Q�A�1'A��A�%A��A��HA���AŬAţ�Aş�Aŉ7A�|�A�n�A�VA�A�A�33A��A���A���Aģ�A�t�A�ffA�dZA�ffA�ffA�\)A�1'A��A�ȴA�n�A�{A���A�n�A�$�A��;A��!A�C�A�%A���A���A�r�A�S�A�9XA�/A�$�A���A���A���A��A�S�A�&�A��A���A���A�ĜA��wA��A���A�bNA��#A�S�A�{A���A��hA��A�~�A�|�A�z�A�z�A�t�A�l�A�jA�bNA�`BA�XA�7LA��A��A�t�A���A��9A��A�\)A�7LA�oA��HA��-A��A�^5A�I�A�9XA��A�  A��mA���A�A��wA��jA��^A��-A���A���A���A���A���A���A�z�A�^5A�I�A�A�A�1'A�-A�$�A��A��A�oA�JA�  A���A�jA�ƨA��A���A�?}A��mA��RA��RA��jA��wA��jA��!A�|�A�"�A�ĜA�S�A�(�A��A���A��jA���A���A���A��uA��A�^5A�1'A�&�A�bA��A��uA�\)A�9XA��jA�JA���A�ffA�O�A��A�A���A���A��`A��uA�n�A��`A�=qA���A���A��A���A�x�A�ffA�`BA�XA�?}A�33A�(�A�oA�A��HA���A�M�A�&�A��A�JA���A���A�x�A�"�A���A��RA�bNA�=qA��mA��hA�G�A�
=A�ĜA�hsA�?}A� �A��`A�ƨA���A��A�dZA�?}A�JA���A�z�A��A���A�I�A��A���A���A��A��A�ZA��A��A�  A��/A��A���A�ffA��yA�S�A�JA��HA���A���A�v�A�S�A�oA��yA���A���A���A���A�bNA��`A�r�A��TA�ĜA��A�VA�+A�VA���A���A���A�hsA�=qA��yA��RA���A��7A�(�A�%A��A��^A�G�A��hA�K�A�%A��A�ffA�I�A�1'A� �A��A�
=A���A��A���A��A���A�v�A�Q�A�I�A�5?A�oA��yA��A�ĜA��FA���A���A�~�A�G�A�%A���A���A��A�K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�bA�oA�bA�VA�VA�JA�JA�JA�VA��A�&�A�&�A�"�A��A��A�"�A�"�A�(�A�%A��
A؟�A؏\A؋DA؅A؃A�z�A�x�A�t�A�jA�dZA�bNA�ZA�%A��TA��
A���Aԡ�A�VA�ȴA��A�S�AёhA��mAГuAЃA��`Aβ-A��A��#A͓uA�9XA�ffA�bA��#A˟�A��/Aɣ�Aȡ�AǛ�A�v�A���A�`BAĩ�A�%A�-A���A���A��yA��A�~�A�VA�  A��RA��A��A�hsA��A�^5A��9A�S�A��A�ffA�;dA�ĜA�z�A��#A��^A��A�7LA�{A�&�A� �A���A�jA��\A�VA���A�|�A�(�A�hsA��A�`BA���A�I�A�G�A�~�A��uA�=qA��A�Q�A�Q�A�
=A�jA�Q�A�ȴA�+A�-A�A{�7AwhsAsdZAq?}Ao�PAm\)Ag��Ab9XA_�wA^�A\n�A[t�AZ��AZr�AX�AQ\)AK;dAI�hAHz�AF�AC��A@(�A=XA<  A9A7�FA6�A4�jA1��A/��A.M�A-A*ffA'��A&~�A%�
A$��A#�A!�-A!�A �A�^A�!A/A{A�7AjA�^Ap�A�A�;AC�A�A�\A�#AA��A�FA
VA	��A	��A
jA
r�A
��A
=qA	��A�`A=qA�AC�A9XAO�AbNA�@��
@�/@���@��-@� �@�X@�dZ@�@�o@�t�@�z�@�Q�@���@�J@��H@�R@���@���@�I�@���@@�ȴ@�;d@�h@�r�@��@�n�@���@��;@��y@��@�h@���@� �@߮@�dZ@�33@�o@�O�@�bN@�9X@�b@��m@�ƨ@ۅ@ڧ�@ف@��@�1'@���@�x�@��;@ӶF@ӶF@ӶF@Ӯ@ӕ�@�S�@�33@�"�@��@�
=@��H@��@���@�1@�I�@˾w@�|�@�C�@�+@��y@���@�~�@ȋD@ǶF@��@���@�~�@�7L@Ĭ@��m@��@��@°!@�E�@�x�@�G�@��@���@��@���@��@���@��w@�dZ@�"�@�+@�\)@���@�b@�(�@�b@�K�@���@��+@���@�n�@���@���@�Ĝ@��@�r�@�(�@��F@�;d@��!@���@�~�@�~�@�v�@�5?@��@��^@���@�x�@�O�@���@�9X@�b@��F@�\)@�33@��@���@��\@�~�@�^5@�5?@��@���@��-@�G�@��`@��9@�Z@�1'@��@���@�dZ@�C�@���@��@�~�@�$�@��@�x�@�9X@�"�@�|�@��
@�1@�1'@�9X@�(�@���@�S�@�M�@��h@�@�j@��P@�
=@��H@��H@�o@�+@��y@��H@���@�@�v�@�=q@�-@���@� �@���@�~�@�M�@�=q@�@���@�x�@��7@��7@��7@��@�Z@�ƨ@��P@�"�@�
=@��y@���@���@�~�@�v�@�ff@�^5@�{@��7@�&�@��@��9@��9@�bN@�9X@�1@�|�@�33@�+@�o@�@��@�$�@���@��@��j@��9@���@��u@�r�@�I�@���@��!@�ff@���@��@�/@���@��@��@�Z@�9X@�1@��;@���@�C�@��@�~�@�=q@��#@���@�p�@�/@��@��j@��D@�A�@��@���@��
@�l�@�
=@�ȴ@��+@�^5@�=q@��@�J@���@�x�@��@��9@��D@�j@�9X@�b@���@���@��P@�|�@��@��y@�ff@�5?@�J@���@��@�/@��`@�z�@�  @��@��;@�ƨ@�dZ@�@���@���@�E�@�=q@�=q@�5?@�$�@�{@���@��@�@�p�@��@��9@�Ĝ@���@���@�  @��m@���@�C�@�dZ@�l�@�C�@��y@���@��+@�ff@�V@�=q@�@��^@�/@���@��9@�I�@�(�@�b@��@|�@~��@~��@~�+@~V@}�-@}/@|�@|�j@{��@z�H@y��@yhs@x��@w�w@wK�@w�@w�@v�@v�+@vff@v5?@v{@u��@up�@uV@t�/@t�@tI�@s��@r�!@rJ@q�7@p��@p�uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�bA�JA�VA�oA�JA�VA�oA�{A�VA��A�bA�bA��A�{A�bA�JA�oA�
=A�JA�oA�bA�
=A�bA�bA�
=A�VA�oA�JA�JA�bA�1A�1A�
=A�VA�
=A�1A�VA�VA�
=A�JA�bA�JA�JA�VA�VA�bA�oA��A�{A�$�A�+A� �A�"�A�-A�(�A�&�A�&�A�$�A� �A�$�A�"�A�(�A�+A�$�A�(�A�+A�&�A�"�A�/A�+A��A� �A�{A�{A��A��A��A��A��A��A��A��A��A��A� �A� �A��A��A��A��A��A��A�"�A��A��A�"�A�$�A�"�A�&�A�$�A� �A�"�A�"�A�$�A��A��A��A��A� �A�(�A�+A�7LA�7LA�$�A�"�A�$�A� �A� �A�"�A� �A���A��A���A�A��A��A�bA�A�
=A�JA�A���A���AؾwAغ^Aؙ�A؝�Aا�AؑhAؓuA؟�Aغ^AضFAؓuAؗ�Aؕ�A؏\A؏\AؓuA؏\A؋DA؍PAؑhA؍PA؉7A؍PA؍PA؉7A؉7A؍PA؋DA؇+A؃A؇+A؇+A؁A؁A؃A؇+A؃A؁A؃A؇+A؃A�~�A؃A؅A�~�A�~�A؁A�~�A�z�A�v�A�x�A�|�A�x�A�t�A�v�A�x�A�x�A�t�A�t�A�z�A�z�A�x�A�t�A�t�A�v�A�x�A�t�A�r�A�r�A�t�A�t�A�n�A�jA�hsA�jA�ffA�dZA�ffA�hsA�ffA�bNA�bNA�hsA�ffA�bNA�`BA�`BA�bNA�dZA�dZA�bNA�`BA�bNA�dZA�dZA�dZA�^5A�`BA�bNA�bNA�\)A�XA�M�A��A�{A�A���A�  A�
=A�1A�A���A���A��TA��yA���A���A��/A��/A׮AבhA׃A׃A�t�A�?}A�A�E�A��A���A��TA���A���A�ȴA���A�A�AԼjAԼjAԺ^AԮAԣ�Aԏ\Aԉ7AԃA�|�A�bNA�dZA�\)A�\)A�O�A�K�A�33A��A�%A��A��#AӶFAӗ�AӉ7AӃA�hsA�A�A�5?A�{A�%A���A��TAҼjA�|�A�l�A�ZA�G�A�5?A�-A�"�A�bA���AѸRAѩ�Aя\A�`BA�1'A�(�A��A�1A��mA��A���A���A�ƨAд9AЛ�AГuAЏ\AЏ\AЍPAЋDAЉ7AЍPAЇ+AЃAЃAЁAЁA�~�A�x�A�S�A�{A���A��mA���Aϥ�AύPA�O�A��A��
AΗ�A·+A�p�A�S�A�M�A�E�A�/A�JA�  A��A��A��A��yA��HA��A���A���A���A;wAʹ9Aͣ�A͟�A͇+A�t�A�l�A�ZA�A�A�;dA�;dA�7LA�33A�$�A��
Ạ�A�t�A�XA�K�A�A�A�1'A�$�A��A�{A�VA�VA�A���A���A��A��yA��;A���A˾wA˴9A˰!A˲-A˰!AˮA˟�A�z�A�t�A�p�A�n�A�C�A��A�~�A�bNA�5?A�A�A���AɶFA�v�A�Q�A�5?A��A�{A�  AȶFA�l�A�33A�"�A��A�
=A���A���AǏ\A�$�A���A���AƼjAƝ�A�|�A�Q�A�1'A��A�%A��A��HA���AŬAţ�Aş�Aŉ7A�|�A�n�A�VA�A�A�33A��A���A���Aģ�A�t�A�ffA�dZA�ffA�ffA�\)A�1'A��A�ȴA�n�A�{A���A�n�A�$�A��;A��!A�C�A�%A���A���A�r�A�S�A�9XA�/A�$�A���A���A���A��A�S�A�&�A��A���A���A�ĜA��wA��A���A�bNA��#A�S�A�{A���A��hA��A�~�A�|�A�z�A�z�A�t�A�l�A�jA�bNA�`BA�XA�7LA��A��A�t�A���A��9A��A�\)A�7LA�oA��HA��-A��A�^5A�I�A�9XA��A�  A��mA���A�A��wA��jA��^A��-A���A���A���A���A���A���A�z�A�^5A�I�A�A�A�1'A�-A�$�A��A��A�oA�JA�  A���A�jA�ƨA��A���A�?}A��mA��RA��RA��jA��wA��jA��!A�|�A�"�A�ĜA�S�A�(�A��A���A��jA���A���A���A��uA��A�^5A�1'A�&�A�bA��A��uA�\)A�9XA��jA�JA���A�ffA�O�A��A�A���A���A��`A��uA�n�A��`A�=qA���A���A��A���A�x�A�ffA�`BA�XA�?}A�33A�(�A�oA�A��HA���A�M�A�&�A��A�JA���A���A�x�A�"�A���A��RA�bNA�=qA��mA��hA�G�A�
=A�ĜA�hsA�?}A� �A��`A�ƨA���A��A�dZA�?}A�JA���A�z�A��A���A�I�A��A���A���A��A��A�ZA��A��A�  A��/A��A���A�ffA��yA�S�A�JA��HA���A���A�v�A�S�A�oA��yA���A���A���A���A�bNA��`A�r�A��TA�ĜA��A�VA�+A�VA���A���A���A�hsA�=qA��yA��RA���A��7A�(�A�%A��A��^A�G�A��hA�K�A�%A��A�ffA�I�A�1'A� �A��A�
=A���A��A���A��A���A�v�A�Q�A�I�A�5?A�oA��yA��A�ĜA��FA���A���A�~�A�G�A�%A���A���A��A�K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B9$B9�B9�B9�B9�B9�B:�B:*B:^B:�B9$B:�B:^B:�B9�B9�B9�B9�B:^B8�B7LB5?B4�B4�B4B3hB33B33B3hB2�B1�B1'B1[B2�B33BB�BQ�BR�BS�BQBH�BB�BU�Bq�B��B��B��B�B�B�B�BbB�BB�B&LB@�Bf�Bl"Bo�Bs�BrBo5Bp�Bm�BqABo�BpBncBu�BqABpBy�B~]B��B�%B�xB��B��BzBtTB\)BL�B'�B&BB+B1B	�B��B��B�ZB�BӏB�B�3B�FB�B.BrBU�BJXB@�B8�B0�BVB�BB
��B
��B
��B
�B
~�B
qB
jB
S&B
F�B
<6B
(�B
�B	��B	҉B	�)B	�}B	��B	��B	��B	s�B	p�B	h
B	a�B	[#B	T,B	V�B	E9B	-�B	�B	B	FB		lB�VB�B��B��B�]B�gB�0B�B��B��B��B�9B�!B�=B��B�eB�B��B�kB��B��B��B��B��B�'B�OB�xB��B�eB��B��B��B�B�:B�4B��B�(B�\B�PB�oB��B��B�jB��B��B�
BбB�BB�BB�B�jB�B�sB�gB��B��B��B��B��B��B�B�PB	�B	{B	=B	IB	�B	�B	�B	"hB	"�B	 �B	�B	�B	IB	xB	�B	�B	�B	!B	!�B	!bB	!�B	!�B	"�B	$B	#:B	#�B	%B	&B	&�B	4B	8�B	9$B	9�B	9�B	9�B	:^B	=B	>�B	@B	C�B	F�B	K�B	OvB	PHB	Q�B	RTB	R�B	WsB	\�B	^�B	`B	`vB	aHB	bNB	d�B	d&B	e`B	c�B	c�B	d&B	e`B	h�B	iDB	i�B	jKB	j�B	jB	qvB	t�B	v�B	x�B	}�B	��B	�+B	��B	��B	��B	�(B	��B	��B	�\B	�B	�B	�JB	��B	��B	�bB	��B	��B	��B	��B	�LB	�6B	�}B	�B	�B	�wB	��B	�OB	��B	��B	�aB	��B	��B	��B	��B	�RB	�$B	�jB	�<B	�BB	�wB	�HB	��B	ÖB	��B	�9B	ŢB	�B	�B	ɺB	˒B	�6B	�B	�B	бB	�}B	�HB	�HB	�B	�B	бB	�NB	҉B	��B	�[B	��B	֡B	��B	�yB	�QB	�#B	��B	��B	��B	ޞB	��B	�&B	��B	�`B	�B	�B	�TB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�8B	��B	��B	��B	�(B	��B
  B
 �B
�B
�B
SB
B
YB
+B
uB
oB
;B
uB
�B
�B
�B
�B
�B
_B
�B
_B
�B
�B
	�B
B
�B
\B
�B
FB
�B
B
B
�B
SB
SB
�B
�B
1B
�B
qB
B
�B
�B
�B
�B
�B
�B
"�B
#�B
%zB
%�B
%�B
%�B
%�B
%�B
%�B
'RB
(�B
(XB
)*B
)�B
*0B
*eB
*�B
*�B
*�B
+B
+kB
+6B
,B
,�B
,�B
-�B
-�B
/OB
/�B
0�B
1[B
1'B
1'B
1[B
1�B
1�B
1�B
2-B
2�B
3�B
4B
4nB
4nB
4�B
4�B
4�B
4�B
5?B
6FB
6B
6B
6B
6�B
6�B
7�B
7�B
7�B
7�B
8�B
8�B
:*B
9�B
9�B
:*B
:*B
:�B
:�B
;�B
<B
;�B
<B
;�B
=<B
=B
=B
=qB
>BB
>BB
>BB
>B
>BB
>BB
>BB
>wB
>wB
?}B
@B
@OB
@�B
@�B
B'B
C�B
C-B
CaB
B�B
B�B
C�B
D�B
D�B
D�B
D�B
DgB
DgB
DgB
D�B
E�B
F?B
E�B
FB
GB
GEB
GEB
GzB
GzB
G�B
G�B
HB
HB
H�B
I�B
I�B
I�B
I�B
J�B
K)B
K�B
K�B
L�B
MB
M6B
NB
NpB
N�B
N�B
OB
OB
OBB
OvB
O�B
OvB
O�B
PHB
P�B
QNB
Q�B
Q�B
R B
RTB
R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B8B8�B:�B9XB8B<B9XB8�B9�B:�B8�B:�B:�B8�B9�B:�B9XB9�B:*B9�B8�B9$B:�B9$B8�B:�B9�B8�B;�B:*B8�B;�B;�B9�B8�B;�B;0B9�B8�B;dB;�B8�B:�B:�B9�B:�B:^B:^B9�B:^B:�B9�B<B;�B:^B<6B<�B:�B;dB:*B9$B<jB9�B9�B;�B9�B7�B:�B=B:�B;�B<�B:*B?HB8�B9XB9XB:�B8�B9$B<jB:^B:*B9�B8B8�B9�B9�B:�B8�B8�B:^B9$B7�B:*B:�B9�B;0B=B9�B9�B7�B:*B<�B9$B9�B:�B9�B8RB8�B8RB=�B:*B:^B?�B;0B9�B;�B;�B9�B:�B8B:�B9XB6zB6B:�B;dB9�B8RB6FBA�B:�B5tB6FB6B7�B5�B7LB6FB5?B2aB49B6FB5B4B49B5�B5B3�B4B5�B5tB3hB4nB5�B4B4nB5B5?B3�B3�B4�B5B3hB2�B4�B4nB3�B2�B4B4�B3�B2aB3�B4�B3�B1�B3�B3�B2-B2aB4B4nB3hB1�B33B49B3�B2aB2�B49B3�B2-B2-B2�B4B4B33B2aB33B3�B3�B2�B2-B49B4B2-B1'B2�B3hB2aB2aB0�B4B2aB0UB0UB1�B2aB2�B1�B0�B0UB1[B2�B1'B/�B/�B0!B1�B0�B/�B/�B1[B0�B3�B3hB0�B2�B4B0�B0�B1'B49B4�B/�B7�B1�B2-B1�B33B0�B5?B6zB33B0�B33B9XBS&Bj�BS�BS�BPBT�BQ�BR�BQNBPBR BS�BS&BNpBPHBS&BT�BR BRTBR BW�BS�BS[BQ�BQ�BRTBS�BU�BQ�BPBS�BS[BS&BLdBI�BI�BIRBK)BK�BGEBD�BG�BH�BLdB>�BC-BB�BA�B@�BAUBEmBQ�BM�BOBBT�BZQBc�B`�Bd&Bj�Br�Bu�Bv`Bv�By	B}�B�GB��B��B�%B�%B�_B��B�YB��B��B�7B�lB�B��B�rB��B��B��B��B��B��B�BB��B�|B�ZB�%B��B��B�.B�.B �BYB
=B	B�B�B+B_B1B
�B	7B�B1B	lB
�BxBDB�B�B�B�B�B�B.B�B"B�B$tB�BOBIBCBIB!BOB�B�B�B�BOB�B�B�B�B�B �B \B!�B!-B �B#�B!�B)�B.IB.B,�B,qB5tB@�B?BNBd&Bc�B_;BbBkBjBi�BjBi�Be`BiDBv�BtTBo Bl"Bk�Bk�Bj�Bl"Bs�Bv�Bs�BqvBncBq�Br|BtBxlBxlBt�Bs�Bo�BrBs�Bn�Bo�Bp�Bl�BncBp;BpoBoiBoiBqABrGBu%Bu�BpBk�Bi�BiBi�BsMBk�Bm]BsMBr|Bo BpoBq�Bl�Bm)BsBrBm�BrBo�Bp�Bm)Bl�BkQBq�Bq�BqvBpBtBqABlWBp�Bo�Bl�Bl"BlWBjBp;B��Bu�Bv`Bv�Bs�BrBrGBqvBp�Bo�Bo�BpBn�Bp�Bo Bo Bs�BpBt�B�SBzBv�BxBy>BzB{B}�B}VB��B�iB��B�uB��B��B�_B��B��B��B��B��B�%B��B�YB��B�1B�fB�rB�(B��B��B�7B�	B�7B��B�B��B�YB�YB��B�+B��B��B�OB�=B�rB�iB{BwfBw�Bx�BxlBz�B��B��B�B|Bp�BlWBd�Bd�B\�B\]B\�B\�B[�BaBT�BO�BQBS&BLdBA�BA�BV�BJ�B5�B1'B$�B0UB!�B	B�B�B)_B=B6zB'�B/OB�B�B�B�BB@BMBYBB@B�BBB�B&�B1B�B�B�BCB"�BYBbBB�B+B"BYB�B  B�B�rB�B��B�;B�5B�B��B�B�B�B�yB��B�B�QB�B�BܒB�WB�/B�]B��B�|B��B��B��B��B�BچB��B�B�B�B�B�tB�qB��B�B�B��B��B��B��B�FB�B�3B��B�B�B�uB��B�~B��B��B�JB�B��B�SB}"B|�B|�BzDBr�Bn/Bv+BzB.Bc�Bd�B]�BWsBPBOBLdBK�BK^BJ�BIBH�BC�BB�BHB?}B=B=B>�B<B7�B8�B49B33B2�B1[B3�B1[B)�B(�BVB)�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                            444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                            444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022071609040520220716090405IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071714012820220717140128QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071714012820220717140128QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                