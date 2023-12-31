CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  7   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-08-15T19:13:34Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  V�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  ]0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  v�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  }X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p 
�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p *�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 1h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` K    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   K�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Q�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   W�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ]�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ]�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ]�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ]�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ]�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ]�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ^t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ^�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ^�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ^�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ^�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ^�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ^�Argo profile    3.1 1.2 19500101000000  20220815191334  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_224                 6810_008521_224                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��-�L�B@��-�L�B11  @��-���@��-���@2,)��;�@2,)��;��d��@���d��@��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@B�\@�G�@�  @�  @�G�A ��AG�A!G�A,(�A>�RA`  A�  A��A�Q�A�Q�A���AУ�A�Q�A�Q�B z�B(�B�
B�B�
B(  B0(�B8Q�B@Q�BH  BO�
BW�B_�Bg�
Bp  Bx  B�B��
B�  B�{B�  B�{B�{B�{B�{B�{B�  B�  B��B��
B�  B�{B�(�B�(�B�{B�{B�{B�  B�  B�  B�  B�  B��B�  B�(�B�(�B�{B�  C   C��C  C  C��C	��C  C
=C  C  C
=C  C  C  C  C  C   C!��C#�C&
=C(
=C*  C,  C.  C0
=C2
=C3��C5��C8  C:  C<
=C>  C@
=CB
=CD
=CF  CH
=CJ{CL
=CN  CO��CQ��CT{CV  CX  CY��C\  C]�C`  Cb  Cd
=Cf  Ch  Cj
=Ck��Cm��Cp  Cr  Cs��Cv  Cx  Cz  C|
=C~
=C�  C�  C���C�  C���C�  C���C�  C�C�  C���C�  C�C�  C�  C�  C���C�  C���C�  C�  C�  C�
=C�
=C�
=C�C�  C�C�
=C�C�C�  C�  C�  C�  C�  C�C�C���C���C�  C�  C�  C�C�C�C�  C�  C���C���C�  C���C�  C�C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�C�C�
=C�C���C���C���C�  C���C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C�C�C�C�  C�  C�C�  C�  C�  C�  C�
=C�  C���C�  C�C�  C�  C�  C�  C�C�C�C�C�  C���C���C�  C�
=C�C�C�C�  C�C�  C�C�C�  C�D   D � D  D}qD�qDz�D�qD� D�D��D�D��D�qD}qD�qD}qD�qD� D	  D	}qD
  D
��D�D}qD  D��D�D��D  D}qD�qD��D  D}qD�qD}qD�D��D�qD� DD��D�D� D�qD� D  D� D��D}qD�D}qD�qD�D  D� D�D��D�qDz�D��D}qD  D}qD   D � D!  D!��D!�qD"� D#  D#��D$�D$� D%  D%��D&�D&�D'D'�D(D(}qD(�qD)� D*  D*��D+  D+}qD,  D,�D-�D-}qD.�D.��D/  D/� D0  D0}qD0��D1xRD1�qD2� D3  D3z�D4  D4}qD4��D5� D6�D6� D7  D7��D8�D8� D8�qD9z�D:  D:� D;  D;}qD;��D<}qD=  D=��D>�D>��D?�D?�D@�D@� DA  DA� DB  DB��DC�DC�DD�DDz�DD��DE}qDF  DF}qDG  DG��DH�DH� DH�qDI}qDJ  DJ��DK  DK��DL�DL��DM�DM�DN  DN� DO  DO}qDO�qDP}qDQ  DQ��DQ��DR}qDS  DS� DS�qDT� DU�DU� DV  DV� DW�DW��DW�qDX��DY�DYz�DY�qDZ� DZ�qD[}qD[�qD\z�D\�qD]��D^  D^}qD_  D_�D`�D`� Da�Da�DbDb� Dc�Dc�Dd  Dd��De  De}qDe�qDf� DgDg� Dg�qDh�DiDi��Dj  Dj� Dj��Dkz�Dl  Dl}qDl��Dm}qDm�qDn}qDo  Do� Dp  Dp}qDp��Dq� Dr  Dr� Ds�Ds� Ds��Dt� DuDu��Du�qDv� Dw  Dw�DxDx� Dy�Dy� Dy��Dzz�Dz�qD{��D{�qD|z�D|�qD}� D~�D~��D~�qD}qD�qD�=qD�~�D�� D���D�>�D�~�D�� D���D�>�D��HD�� D�HD�@ D�~�D�� D��D�C�D���D�� D�HD�B�D��HD���D�  D�AHD�� D�� D�HD�AHD��HD���D���D�@ D��HD�D�HD�>�D�� D�D��D�AHD��HD�D�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D�� D��HD�HD�>�D�� D�D�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?B�\?�  ?�{?Ǯ?�G�@�\@z�@�R@5@E�@O\)@fff@xQ�@��
@�\)@�@��R@�=q@��@�Q�@�ff@�{@�@�G�@�@��@�(�A�
A�A(�A�A�A��A\)A$z�A'�A+�A1G�A3�
A8��A>�RAC33AEAK�AP  AS33AX��A\��A_\)Ae�Aj=qAl��Ar�\Aw
=Az=qA~{A��A��
A�p�A�Q�A�=qA�(�A�
=A�Q�A��HA�A�\)A���A���A��RA���A��
A��RA���A��A�{A�Q�A��HA�{A��A��\A�p�A�
=A��A���AƸRA���A�(�A�AУ�AӅA�p�A�\)A�=qA���A�ffA�G�A�(�A�{A�Q�A�33A�A�\)A�=qA��A��RA���A�z�A�{B Q�B�B�HB�
BG�B�RB�B��B
=qB�B��B��B33BQ�Bp�B�HB  B�B�RB�B��B=qB�Bz�B=qB33B Q�B!�B#
=B$(�B%B&�\B'�B)p�B*=qB+�B-�B-�B/�B0��B1�B3
=B4��B5�B6�HB8(�B9B:�HB<  B=��B>�HB?�BAG�BB�RBC�
BD��BF�\BG�
BH��BJffBK�
BL��BN{BO�BP��BR{BS�BT��BU�BW\)BX��BYB[\)B\��B]��B_
=B`z�Bap�Bb�RBdQ�BeG�BfffBh  Bi�Bj{Bk�Bm�Bn{Bo\)Bp��Bq�Bs
=Bt��Bu�Bv�HBxz�By��Bz�\B|  B}��B~ffB�
B���B���B�B�ffB��HB��B�Q�B��RB�G�B�  B���B�33B�B��\B��B���B�Q�B��B��B�(�B���B���B�  B��RB��B�  B�z�B�\)B��B�Q�B��B��
B�Q�B��HB��B�=qB���B��B�=qB���B�33B��B���B�
=B��B�ffB��B��B�(�B��RB��B�(�B���B�33B�  B���B��B��B�z�B���B��B�=qB���B�\)B��B���B�\)B�B�=qB�
=B��B�{B���B�p�B��
B�ffB�
=B��B�=qB��RB�\)B�{B�z�B�
=B�B�ffB��HB�p�B�{B���B�33B��
B��\B�
=B��B�=qB��HB�\)B��
B��\B�33B���B�(�B���B�\)B�B�ffB�
=B��B�  B¸RB�G�BÙ�B�ffB���B�p�B�  Bƣ�B�33BǙ�B�Q�B��HB�G�B��Bʏ\B��B˅B�(�B���B�p�B�B�ffB�
=Bϙ�B�  B�z�B�33B�B�=qB���B�p�B�{B�z�B��B��
B�z�B���B�p�B�{B��HB�p�B��B�z�B��B��
B�Q�B���B�p�B�=qBޣ�B�G�B�  B�RB�G�B��
B�z�B�G�B��B�ffB�
=B��
B�ffB�
=B�B�Q�B��B��
B�ffB���B�B�z�B���B�B�(�B���B�p�B�{B��B�
=B�B�Q�B���B�G�B�B�(�B��RB�33B��B��B�ffB��HB�\)B��B��
B�=qB���B��B�p�B���B�(�B�ffB���B�
=B��B��B�  B�z�B��HB�
=B�G�B�B�(�B�z�B���B��B��B�C   C (�C \)C �\C ��C C  C33CQ�Cp�C��C��C  C(�CG�Cp�C��C�HC{C33CQ�C�\CC��C{C33Cz�C�C�
C�C�C\)C��C�RC�HC{CQ�C�C�C��C
=CG�Cz�C��CC
=CG�CffC�\C�RC	  C	33C	Q�C	z�C	�RC	��C
�C
=qC
p�C
�C
�C  C33Cp�C�C�
C  C(�Cp�C�C��C��C33Cp�C�C��C��C�CffC��C�RC��C33C\)Cz�C��C�C{C33C\)C��C�
C  C�C\)C�\C�RC�HC{CG�C�C�C��C
=CG�CffC�C��C
=C(�CQ�C�\CC�HC{CQ�Cz�C��CC
=C=qC\)Cz�C�RC�C
=C(�CffC��C�RC�
C{CQ�C\)C�CC�HC��C33C\)Cz�C��C�
C
=C�C=qCp�C�C�RC�HC{C=qCQ�C�CC�
C��C�C\)Cz�C�\C��C  C
=C33Cp�C�C��C�HC  C�C33Cp�C��C�C��C
=C33CG�CffC��C��C�C   C 33C p�C �\C ��C C ��C!(�C!G�C!ffC!�C!�RC!�C!��C"�C"Q�C"p�C"�C"C"��C#
=C#(�C#Q�C#�C#C#�HC$  C$�C$Q�C$�C$�C$C%  C%=qC%\)C%p�C%��C%�HC&{C&33C&G�C&�C&�C&�
C&��C'{C'=qC'p�C'�C'�
C'�C(
=C(Q�C(�C(��C(C(��C)(�C)ffC)��C)�RC)�HC*�C*\)C*�C*�C*��C+
=C+G�C+z�C+��C+��C+��C,33C,p�C,��C,C,�C-(�C-ffC-��C-C-�C.{C.Q�C.�\C.��C.��C/(�C/\)C/��C/�
C0  C033C0z�C0C0�HC1{C1Q�C1��C1�HC2{C2=qC2p�C2�RC3  C3(�C3\)C3��C3�C4�C4G�C4z�C4C5
=C5=qC5ffC5��C5�HC6(�C6\)C6�\C6��C7
=C7\)C7�\C7C7��C833C8�C8C8�C9�C9\)C9��C9�C:(�C:G�C:�C:�RC;  C;G�C;�\C;C;��C<�C<\)C<�C<��C={C=Q�C=�\C=�HC>�C>\)C>�C>�RC>��C?=qC?�C?�RC?�C@�C@Q�C@�C@�
CA{CAQ�CAz�CA�CA�HCB33CBp�CB��CB��CC  CC33CCz�CCCC�CD{CD=qCDz�CD�RCE  CE33CEQ�CE�CE�RCE�CF�CF\)CF�\CFCF�CG{CG=qCGp�CG��CG�HCH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@�\@B�\@�G�@�  @�  @�G�A ��AG�A!G�A,(�A>�RA`  A�  A��A�Q�A�Q�A���AУ�A�Q�A�Q�B z�B(�B�
B�B�
B(  B0(�B8Q�B@Q�BH  BO�
BW�B_�Bg�
Bp  Bx  B�B��
B�  B�{B�  B�{B�{B�{B�{B�{B�  B�  B��B��
B�  B�{B�(�B�(�B�{B�{B�{B�  B�  B�  B�  B�  B��B�  B�(�B�(�B�{B�  C   C��C  C  C��C	��C  C
=C  C  C
=C  C  C  C  C  C   C!��C#�C&
=C(
=C*  C,  C.  C0
=C2
=C3��C5��C8  C:  C<
=C>  C@
=CB
=CD
=CF  CH
=CJ{CL
=CN  CO��CQ��CT{CV  CX  CY��C\  C]�C`  Cb  Cd
=Cf  Ch  Cj
=Ck��Cm��Cp  Cr  Cs��Cv  Cx  Cz  C|
=C~
=C�  C�  C���C�  C���C�  C���C�  C�C�  C���C�  C�C�  C�  C�  C���C�  C���C�  C�  C�  C�
=C�
=C�
=C�C�  C�C�
=C�C�C�  C�  C�  C�  C�  C�C�C���C���C�  C�  C�  C�C�C�C�  C�  C���C���C�  C���C�  C�C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�C�C�
=C�C���C���C���C�  C���C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C�C�C�C�  C�  C�C�  C�  C�  C�  C�
=C�  C���C�  C�C�  C�  C�  C�  C�C�C�C�C�  C���C���C�  C�
=C�C�C�C�  C�C�  C�C�C�  C�D   D � D  D}qD�qDz�D�qD� D�D��D�D��D�qD}qD�qD}qD�qD� D	  D	}qD
  D
��D�D}qD  D��D�D��D  D}qD�qD��D  D}qD�qD}qD�D��D�qD� DD��D�D� D�qD� D  D� D��D}qD�D}qD�qD�D  D� D�D��D�qDz�D��D}qD  D}qD   D � D!  D!��D!�qD"� D#  D#��D$�D$� D%  D%��D&�D&�D'D'�D(D(}qD(�qD)� D*  D*��D+  D+}qD,  D,�D-�D-}qD.�D.��D/  D/� D0  D0}qD0��D1xRD1�qD2� D3  D3z�D4  D4}qD4��D5� D6�D6� D7  D7��D8�D8� D8�qD9z�D:  D:� D;  D;}qD;��D<}qD=  D=��D>�D>��D?�D?�D@�D@� DA  DA� DB  DB��DC�DC�DD�DDz�DD��DE}qDF  DF}qDG  DG��DH�DH� DH�qDI}qDJ  DJ��DK  DK��DL�DL��DM�DM�DN  DN� DO  DO}qDO�qDP}qDQ  DQ��DQ��DR}qDS  DS� DS�qDT� DU�DU� DV  DV� DW�DW��DW�qDX��DY�DYz�DY�qDZ� DZ�qD[}qD[�qD\z�D\�qD]��D^  D^}qD_  D_�D`�D`� Da�Da�DbDb� Dc�Dc�Dd  Dd��De  De}qDe�qDf� DgDg� Dg�qDh�DiDi��Dj  Dj� Dj��Dkz�Dl  Dl}qDl��Dm}qDm�qDn}qDo  Do� Dp  Dp}qDp��Dq� Dr  Dr� Ds�Ds� Ds��Dt� DuDu��Du�qDv� Dw  Dw�DxDx� Dy�Dy� Dy��Dzz�Dz�qD{��D{�qD|z�D|�qD}� D~�D~��D~�qD}qD�qD�=qD�~�D�� D���D�>�D�~�D�� D���D�>�D��HD�� D�HD�@ D�~�D�� D��D�C�D���D�� D�HD�B�D��HD���D�  D�AHD�� D�� D�HD�AHD��HD���D���D�@ D��HD�D�HD�>�D�� D�D��D�AHD��HD�D�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D�� D��HD�HD�>�D�� D�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?B�\?�  ?�{?Ǯ?�G�@�\@z�@�R@5@E�@O\)@fff@xQ�@��
@�\)@�@��R@�=q@��@�Q�@�ff@�{@�@�G�@�@��@�(�A�
A�A(�A�A�A��A\)A$z�A'�A+�A1G�A3�
A8��A>�RAC33AEAK�AP  AS33AX��A\��A_\)Ae�Aj=qAl��Ar�\Aw
=Az=qA~{A��A��
A�p�A�Q�A�=qA�(�A�
=A�Q�A��HA�A�\)A���A���A��RA���A��
A��RA���A��A�{A�Q�A��HA�{A��A��\A�p�A�
=A��A���AƸRA���A�(�A�AУ�AӅA�p�A�\)A�=qA���A�ffA�G�A�(�A�{A�Q�A�33A�A�\)A�=qA��A��RA���A�z�A�{B Q�B�B�HB�
BG�B�RB�B��B
=qB�B��B��B33BQ�Bp�B�HB  B�B�RB�B��B=qB�Bz�B=qB33B Q�B!�B#
=B$(�B%B&�\B'�B)p�B*=qB+�B-�B-�B/�B0��B1�B3
=B4��B5�B6�HB8(�B9B:�HB<  B=��B>�HB?�BAG�BB�RBC�
BD��BF�\BG�
BH��BJffBK�
BL��BN{BO�BP��BR{BS�BT��BU�BW\)BX��BYB[\)B\��B]��B_
=B`z�Bap�Bb�RBdQ�BeG�BfffBh  Bi�Bj{Bk�Bm�Bn{Bo\)Bp��Bq�Bs
=Bt��Bu�Bv�HBxz�By��Bz�\B|  B}��B~ffB�
B���B���B�B�ffB��HB��B�Q�B��RB�G�B�  B���B�33B�B��\B��B���B�Q�B��B��B�(�B���B���B�  B��RB��B�  B�z�B�\)B��B�Q�B��B��
B�Q�B��HB��B�=qB���B��B�=qB���B�33B��B���B�
=B��B�ffB��B��B�(�B��RB��B�(�B���B�33B�  B���B��B��B�z�B���B��B�=qB���B�\)B��B���B�\)B�B�=qB�
=B��B�{B���B�p�B��
B�ffB�
=B��B�=qB��RB�\)B�{B�z�B�
=B�B�ffB��HB�p�B�{B���B�33B��
B��\B�
=B��B�=qB��HB�\)B��
B��\B�33B���B�(�B���B�\)B�B�ffB�
=B��B�  B¸RB�G�BÙ�B�ffB���B�p�B�  Bƣ�B�33BǙ�B�Q�B��HB�G�B��Bʏ\B��B˅B�(�B���B�p�B�B�ffB�
=Bϙ�B�  B�z�B�33B�B�=qB���B�p�B�{B�z�B��B��
B�z�B���B�p�B�{B��HB�p�B��B�z�B��B��
B�Q�B���B�p�B�=qBޣ�B�G�B�  B�RB�G�B��
B�z�B�G�B��B�ffB�
=B��
B�ffB�
=B�B�Q�B��B��
B�ffB���B�B�z�B���B�B�(�B���B�p�B�{B��B�
=B�B�Q�B���B�G�B�B�(�B��RB�33B��B��B�ffB��HB�\)B��B��
B�=qB���B��B�p�B���B�(�B�ffB���B�
=B��B��B�  B�z�B��HB�
=B�G�B�B�(�B�z�B���B��B��B�C   C (�C \)C �\C ��C C  C33CQ�Cp�C��C��C  C(�CG�Cp�C��C�HC{C33CQ�C�\CC��C{C33Cz�C�C�
C�C�C\)C��C�RC�HC{CQ�C�C�C��C
=CG�Cz�C��CC
=CG�CffC�\C�RC	  C	33C	Q�C	z�C	�RC	��C
�C
=qC
p�C
�C
�C  C33Cp�C�C�
C  C(�Cp�C�C��C��C33Cp�C�C��C��C�CffC��C�RC��C33C\)Cz�C��C�C{C33C\)C��C�
C  C�C\)C�\C�RC�HC{CG�C�C�C��C
=CG�CffC�C��C
=C(�CQ�C�\CC�HC{CQ�Cz�C��CC
=C=qC\)Cz�C�RC�C
=C(�CffC��C�RC�
C{CQ�C\)C�CC�HC��C33C\)Cz�C��C�
C
=C�C=qCp�C�C�RC�HC{C=qCQ�C�CC�
C��C�C\)Cz�C�\C��C  C
=C33Cp�C�C��C�HC  C�C33Cp�C��C�C��C
=C33CG�CffC��C��C�C   C 33C p�C �\C ��C C ��C!(�C!G�C!ffC!�C!�RC!�C!��C"�C"Q�C"p�C"�C"C"��C#
=C#(�C#Q�C#�C#C#�HC$  C$�C$Q�C$�C$�C$C%  C%=qC%\)C%p�C%��C%�HC&{C&33C&G�C&�C&�C&�
C&��C'{C'=qC'p�C'�C'�
C'�C(
=C(Q�C(�C(��C(C(��C)(�C)ffC)��C)�RC)�HC*�C*\)C*�C*�C*��C+
=C+G�C+z�C+��C+��C+��C,33C,p�C,��C,C,�C-(�C-ffC-��C-C-�C.{C.Q�C.�\C.��C.��C/(�C/\)C/��C/�
C0  C033C0z�C0C0�HC1{C1Q�C1��C1�HC2{C2=qC2p�C2�RC3  C3(�C3\)C3��C3�C4�C4G�C4z�C4C5
=C5=qC5ffC5��C5�HC6(�C6\)C6�\C6��C7
=C7\)C7�\C7C7��C833C8�C8C8�C9�C9\)C9��C9�C:(�C:G�C:�C:�RC;  C;G�C;�\C;C;��C<�C<\)C<�C<��C={C=Q�C=�\C=�HC>�C>\)C>�C>�RC>��C?=qC?�C?�RC?�C@�C@Q�C@�C@�
CA{CAQ�CAz�CA�CA�HCB33CBp�CB��CB��CC  CC33CCz�CCCC�CD{CD=qCDz�CD�RCE  CE33CEQ�CE�CE�RCE�CF�CF\)CF�\CFCF�CG{CG=qCGp�CG��CG�HCH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�C�A�E�A�G�A�I�A�M�A�Q�A�Q�A�S�A�S�A�VA�S�A�VA�S�A�VA�XA�\)A�\)A�S�A�VA�VA�O�A�O�A�I�A�?}A�$�A�A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��TA��;A�ƨAש�AדuAׇ+A�^5A�VA�p�A�jA� �AґhA��`AΛ�A�I�A�ƨA�=qA��AɬA�33AƋDA��mAŉ7A�x�A��A��A��yA�  A�;dA��A��A��A���A�bA���A��A���A�G�A��HA�$�A�n�A�/A���A���A���A��-A�K�A�+A�`BA��/A��A�%A��`A���A�{A�r�A�-A��7A�VA�?}A��hA�G�A��+A�p�A��yA���A�dZA��A�r�A��uA�r�A�ffA��A�VA�#A�7A`BA~�A}�TA}7LAz��AyC�Au�hAr�jAo��AmXAk/AjbNAip�Ahz�AfE�AeS�AcAZ�AV��AT�AOdZAKAIx�AF1AA�A>�A<�A;�A9��A8��A8ffA7�A7�A5�7A3G�A1�mA1�7A0�A/�FA.Q�A,$�A*�`A*ĜA*��A*I�A(�A(ffA'�A'/A%ƨA#oA!�A!�7AƨA�A�!A��AVAjA  AO�A�wA|�A�jAn�A��A�yA �A\)A��AVA$�A��A&�AG�A-A��A�A
�uA
(�A	�wA	S�A�RA��A5?Ax�A�
Az�A�7AffA��A �A z�A {@���@�bN@�~�@�G�@�Z@��@�ff@�r�@�ȴ@�x�@�b@�=q@�t�@�@�@� �@睲@�z�@�(�@�1@���@���@�I�@޸R@�$�@ܛ�@�33@ف@�+@�o@�=q@�@ՙ�@�X@���@�Ĝ@�V@պ^@��
@�+@�\)@ӥ�@�9X@��`@Ցh@�%@���@���@��`@ԛ�@Ӿw@�n�@љ�@с@щ7@с@�x�@�?}@��/@�r�@�9X@�1'@�  @�C�@��@��@·+@��@�@͑h@��/@ˍP@��@���@ʸR@�V@���@ɡ�@Ɂ@���@ț�@�z�@�I�@�(�@��@�"�@�-@���@��@�@�Z@��@öF@�"�@�ȴ@�M�@�J@��h@�G�@��@��`@�bN@�  @�ƨ@��F@���@�o@���@�n�@�n�@�ff@��@��@�@�%@��@�1@��m@��F@��@�V@�{@��@�V@���@��/@�z�@��;@�l�@�;d@�o@��y@���@�ff@��@�@���@���@�Q�@��@���@��P@�t�@�dZ@�S�@�"�@���@�5?@�O�@��`@��@��D@�j@�I�@�1'@�1@���@�S�@��@�o@�@��R@�-@��@��@�%@�Ĝ@��u@��@��@�33@�
=@���@��7@� �@���@��@�\)@�+@�ȴ@�n�@�M�@���@���@��7@�&�@�  @��@�t�@�t�@�t�@�l�@�dZ@�S�@�@���@���@�ff@�5?@��h@���@���@�Z@���@��y@��\@�n�@�-@�@��@�@�hs@��u@�(�@���@�\)@�C�@�"�@���@��@��\@�^5@�=q@�5?@��@�@���@�X@�&�@��/@��D@��@��m@���@�dZ@�33@�"�@�
=@��@�V@�=q@��@��T@��-@��@��@�Ĝ@��9@��u@��@�Q�@�b@��;@�t�@�
=@��!@�E�@�@���@�/@���@��j@�z�@�9X@�1@��;@��w@��F@���@��P@�\)@��y@��\@�ff@�E�@�{@��^@��h@�`B@�V@��`@��9@��@�j@�Z@�9X@���@��w@��@�C�@��@���@�~�@�5?@��#@��h@�G�@�&�@���@���@�bN@�Q�@�A�@�1'@�(�@��;@��F@��P@�l�@�K�@�o@���@�^5@�5?@�-@�$�@�J@��@���@���@��@�hs@��@��@�I�@�b@��w@�K�@��@��R@�^5@�V@�-@���@�G�@���@���@���@��@�Z@�(�@�1@�P@K�@~ȴ@~�+@~E�@~@}��@}��@}?}@|�D@|�@{��@{"�@z�H@z��@z~�@z-@y��@yG�@x�`@x��@xr�@x1'@xb@w�@w�w@wl�@w
=@v�@v��@vff@v5?@u@u�@u/@t�D@t1@sƨG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�?}A�?}A�I�A�G�A�C�A�G�A�G�A�C�A�G�A�I�A�C�A�M�A�I�A�E�A�I�A�I�A�K�A�Q�A�Q�A�M�A�S�A�Q�A�O�A�S�A�VA�Q�A�Q�A�XA�Q�A�VA�XA�Q�A�S�A�XA�XA�Q�A�VA�VA�VA�S�A�XA�XA�S�A�XA�XA�S�A�XA�VA�Q�A�Q�A�Q�A�K�A�Q�A�S�A�VA�XA�ZA�VA�S�A�XA�ZA�VA�ZA�VA�XA�ZA�ZA�ZA�`BA�\)A�XA�\)A�`BA�\)A�`BA�bNA�VA�XA�\)A�VA�ZA�XA�S�A�S�A�Q�A�K�A�O�A�VA�S�A�ZA�\)A�XA�XA�ZA�\)A�S�A�XA�VA�Q�A�O�A�S�A�Q�A�K�A�M�A�O�A�K�A�S�A�VA�S�A�I�A�M�A�K�A�G�A�K�A�O�A�I�A�E�A�E�A�G�A�A�A�?}A�C�A�?}A�9XA�?}A�?}A�;dA�9XA�"�A�oA�JA�
=A�A�1A�A�  A�A���A�  A�A���A���A�A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A��A��A���A���A���A���A���A��A���A���A��A���A���A��A���A���A���A��A���A���A��A���A���A��A���A���A��A��A���A��A��A���A���A��A���A��A��A��A���A��A��A���A��A��A��A��A��A���A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��HA��`A��`A��HA��TA��mA��TA��TA��TA��`A��HA��#A��
A���A���A�ȴA�ȴA�ƨA�A׼jA׾wA׺^Aװ!Aק�Aף�Aכ�Aי�Aח�Aו�AבhA׏\AדuAדuAבhA׏\AדuA׏\A׋DA׉7AׅA�z�A�t�A�v�A�n�A�hsA�`BA�S�A�=qA�G�A�A�A�G�A�G�A�XA�bNA�ffA�t�A�x�A�z�A�r�A�n�A�jA�hsA�dZA�l�A�t�A�|�A�~�A�x�A�`BA�E�A��A֩�A�~�A�M�A���A�\)A�&�A�^5A��A�dZA��A��A�A�dZA�`BA�ZA�/A��HA�v�A�-A���A�ȴA�dZA�S�A�  A͑hA�33A��mA̰!A�n�A�"�A���A�E�A��A���A���Aʙ�A�~�A�z�A�jA�S�A�C�A�-A��A��A�oA�%A���A���A��A��`A��;A��/A���AɸRAɺ^Aɧ�Aɗ�Aɧ�Aɉ7A�|�A�r�A�ffA�K�A�
=A�-A�r�A�bNA�M�A�G�A�;dA�5?A�{A��Aơ�Aƙ�AƉ7A�z�A�x�A�p�A�M�A�-A�1A�  A���A��A��A��mA��HA��/A��;A��TA��TA��HA��;A��TA��;A���AŲ-Aŧ�Aţ�Aŗ�A�~�A�S�A�G�A�5?A�A�A�ƨAģ�AđhAēuAēuAď\AąA�~�A�v�A�bNA�-A��mAð!A�x�A�E�A�-A� �A�oA���A��`A´9A�~�A�K�A�&�A�A��mA��^A���A��uA�dZA�/A��A�VA�A���A��A��yA��yA��TA��/A��A��#A��A���A��wA���A�S�A�JA��mA��/A���A���A�?}A���A���A�v�A�`BA�K�A�7LA�(�A��A�A���A��A��mA��TA��/A��#A��/A��#A���A���A���A���A���A�ƨA���A�t�A�dZA�?}A�+A�
=A��7A��
A��A�S�A�1'A���A��wA���A��A�^5A�?}A�"�A�JA��A���A�ƨA��jA��FA��!A��!A��A���A��DA�t�A�l�A�dZA�VA�C�A�$�A��A�{A�oA�VA�  A��A��#A��jA���A�v�A��A��
A�ȴA��^A���A��PA�l�A�E�A�$�A���A���A�A��9A��\A�?}A��A���A��A���A��DA��A�jA�/A���A��;A���A���A��^A���A���A���A��\A�x�A��A��A�z�A�dZA�G�A�=qA�/A�$�A�A��HA�ĜA��DA�-A��#A�jA�JA��!A�t�A�dZA�M�A�1'A�-A�-A�%A��A�ĜA���A��hA�^5A�`BA�M�A�33A�+A��A���A�x�A�$�A��A��jA�~�A�bNA�7LA�{A��A���A��!A�t�A�ZA�S�A�O�A�O�A�O�A�S�A�S�A�Q�A�K�A�I�A�G�A�E�A�C�A�?}A�7LA�5?A�9XA�5?A�+A�A��A�A�A��FA�l�A�M�A�"�A��mA��RA�hsA��A��RA�n�A�A�A�5?A�"�A���A���A��A�E�A��A��A���A�l�A�S�A�/A��yA���A���A��A�~�A�l�A�$�A��jA��uA�C�A��;A���A�O�A�{A��A��A�ƨA��9A���A��\A�z�A�(�A��A�ĜA���A��A�l�A�O�A�=qA��A���A��mA��A���A��9A���A��A�bNA�C�A�+A��A�
=A�A�  A���A��TA���A���A���A��wA��!A���A���A���A��\A��7A�|�A�r�A�ffA�\)A�K�A�=qA�7LA�&�A�
=A��/A���A��\A�~�A�ZA�C�A�9XA��A�ȴA�jA��A��A��yA��mA��#A���A��hA�|�A�ZA�bA�A��\A�^5A�C�A��A��/A���A���A��RA��FA��9A��!A��A��A���A�|�A�?}A��A���A��jA���A�n�A�"�A�%A���A���A��A��HA�ȴA��^A��-A���A�r�A�C�A�$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�E�A�G�A�I�A�M�A�Q�A�Q�A�S�A�S�A�VA�S�A�VA�S�A�VA�XA�\)A�\)A�S�A�VA�VA�O�A�O�A�I�A�?}A�$�A�A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��TA��;A�ƨAש�AדuAׇ+A�^5A�VA�p�A�jA� �AґhA��`AΛ�A�I�A�ƨA�=qA��AɬA�33AƋDA��mAŉ7A�x�A��A��A��yA�  A�;dA��A��A��A���A�bA���A��A���A�G�A��HA�$�A�n�A�/A���A���A���A��-A�K�A�+A�`BA��/A��A�%A��`A���A�{A�r�A�-A��7A�VA�?}A��hA�G�A��+A�p�A��yA���A�dZA��A�r�A��uA�r�A�ffA��A�VA�#A�7A`BA~�A}�TA}7LAz��AyC�Au�hAr�jAo��AmXAk/AjbNAip�Ahz�AfE�AeS�AcAZ�AV��AT�AOdZAKAIx�AF1AA�A>�A<�A;�A9��A8��A8ffA7�A7�A5�7A3G�A1�mA1�7A0�A/�FA.Q�A,$�A*�`A*ĜA*��A*I�A(�A(ffA'�A'/A%ƨA#oA!�A!�7AƨA�A�!A��AVAjA  AO�A�wA|�A�jAn�A��A�yA �A\)A��AVA$�A��A&�AG�A-A��A�A
�uA
(�A	�wA	S�A�RA��A5?Ax�A�
Az�A�7AffA��A �A z�A {@���@�bN@�~�@�G�@�Z@��@�ff@�r�@�ȴ@�x�@�b@�=q@�t�@�@�@� �@睲@�z�@�(�@�1@���@���@�I�@޸R@�$�@ܛ�@�33@ف@�+@�o@�=q@�@ՙ�@�X@���@�Ĝ@�V@պ^@��
@�+@�\)@ӥ�@�9X@��`@Ցh@�%@���@���@��`@ԛ�@Ӿw@�n�@љ�@с@щ7@с@�x�@�?}@��/@�r�@�9X@�1'@�  @�C�@��@��@·+@��@�@͑h@��/@ˍP@��@���@ʸR@�V@���@ɡ�@Ɂ@���@ț�@�z�@�I�@�(�@��@�"�@�-@���@��@�@�Z@��@öF@�"�@�ȴ@�M�@�J@��h@�G�@��@��`@�bN@�  @�ƨ@��F@���@�o@���@�n�@�n�@�ff@��@��@�@�%@��@�1@��m@��F@��@�V@�{@��@�V@���@��/@�z�@��;@�l�@�;d@�o@��y@���@�ff@��@�@���@���@�Q�@��@���@��P@�t�@�dZ@�S�@�"�@���@�5?@�O�@��`@��@��D@�j@�I�@�1'@�1@���@�S�@��@�o@�@��R@�-@��@��@�%@�Ĝ@��u@��@��@�33@�
=@���@��7@� �@���@��@�\)@�+@�ȴ@�n�@�M�@���@���@��7@�&�@�  @��@�t�@�t�@�t�@�l�@�dZ@�S�@�@���@���@�ff@�5?@��h@���@���@�Z@���@��y@��\@�n�@�-@�@��@�@�hs@��u@�(�@���@�\)@�C�@�"�@���@��@��\@�^5@�=q@�5?@��@�@���@�X@�&�@��/@��D@��@��m@���@�dZ@�33@�"�@�
=@��@�V@�=q@��@��T@��-@��@��@�Ĝ@��9@��u@��@�Q�@�b@��;@�t�@�
=@��!@�E�@�@���@�/@���@��j@�z�@�9X@�1@��;@��w@��F@���@��P@�\)@��y@��\@�ff@�E�@�{@��^@��h@�`B@�V@��`@��9@��@�j@�Z@�9X@���@��w@��@�C�@��@���@�~�@�5?@��#@��h@�G�@�&�@���@���@�bN@�Q�@�A�@�1'@�(�@��;@��F@��P@�l�@�K�@�o@���@�^5@�5?@�-@�$�@�J@��@���@���@��@�hs@��@��@�I�@�b@��w@�K�@��@��R@�^5@�V@�-@���@�G�@���@���@���@��@�Z@�(�@�1@�P@K�@~ȴ@~�+@~E�@~@}��@}��@}?}@|�D@|�@{��@{"�@z�H@z��@z~�@z-@y��@yG�@x�`@x��@xr�@x1'@xb@w�@w�w@wl�@w
=@v�@v��@vff@v5?@u@u�@u/@t�D@t1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�?}A�?}A�I�A�G�A�C�A�G�A�G�A�C�A�G�A�I�A�C�A�M�A�I�A�E�A�I�A�I�A�K�A�Q�A�Q�A�M�A�S�A�Q�A�O�A�S�A�VA�Q�A�Q�A�XA�Q�A�VA�XA�Q�A�S�A�XA�XA�Q�A�VA�VA�VA�S�A�XA�XA�S�A�XA�XA�S�A�XA�VA�Q�A�Q�A�Q�A�K�A�Q�A�S�A�VA�XA�ZA�VA�S�A�XA�ZA�VA�ZA�VA�XA�ZA�ZA�ZA�`BA�\)A�XA�\)A�`BA�\)A�`BA�bNA�VA�XA�\)A�VA�ZA�XA�S�A�S�A�Q�A�K�A�O�A�VA�S�A�ZA�\)A�XA�XA�ZA�\)A�S�A�XA�VA�Q�A�O�A�S�A�Q�A�K�A�M�A�O�A�K�A�S�A�VA�S�A�I�A�M�A�K�A�G�A�K�A�O�A�I�A�E�A�E�A�G�A�A�A�?}A�C�A�?}A�9XA�?}A�?}A�;dA�9XA�"�A�oA�JA�
=A�A�1A�A�  A�A���A�  A�A���A���A�A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A��A��A���A���A���A���A���A��A���A���A��A���A���A��A���A���A���A��A���A���A��A���A���A��A���A���A��A��A���A��A��A���A���A��A���A��A��A��A���A��A��A���A��A��A��A��A��A���A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��HA��`A��`A��HA��TA��mA��TA��TA��TA��`A��HA��#A��
A���A���A�ȴA�ȴA�ƨA�A׼jA׾wA׺^Aװ!Aק�Aף�Aכ�Aי�Aח�Aו�AבhA׏\AדuAדuAבhA׏\AדuA׏\A׋DA׉7AׅA�z�A�t�A�v�A�n�A�hsA�`BA�S�A�=qA�G�A�A�A�G�A�G�A�XA�bNA�ffA�t�A�x�A�z�A�r�A�n�A�jA�hsA�dZA�l�A�t�A�|�A�~�A�x�A�`BA�E�A��A֩�A�~�A�M�A���A�\)A�&�A�^5A��A�dZA��A��A�A�dZA�`BA�ZA�/A��HA�v�A�-A���A�ȴA�dZA�S�A�  A͑hA�33A��mA̰!A�n�A�"�A���A�E�A��A���A���Aʙ�A�~�A�z�A�jA�S�A�C�A�-A��A��A�oA�%A���A���A��A��`A��;A��/A���AɸRAɺ^Aɧ�Aɗ�Aɧ�Aɉ7A�|�A�r�A�ffA�K�A�
=A�-A�r�A�bNA�M�A�G�A�;dA�5?A�{A��Aơ�Aƙ�AƉ7A�z�A�x�A�p�A�M�A�-A�1A�  A���A��A��A��mA��HA��/A��;A��TA��TA��HA��;A��TA��;A���AŲ-Aŧ�Aţ�Aŗ�A�~�A�S�A�G�A�5?A�A�A�ƨAģ�AđhAēuAēuAď\AąA�~�A�v�A�bNA�-A��mAð!A�x�A�E�A�-A� �A�oA���A��`A´9A�~�A�K�A�&�A�A��mA��^A���A��uA�dZA�/A��A�VA�A���A��A��yA��yA��TA��/A��A��#A��A���A��wA���A�S�A�JA��mA��/A���A���A�?}A���A���A�v�A�`BA�K�A�7LA�(�A��A�A���A��A��mA��TA��/A��#A��/A��#A���A���A���A���A���A�ƨA���A�t�A�dZA�?}A�+A�
=A��7A��
A��A�S�A�1'A���A��wA���A��A�^5A�?}A�"�A�JA��A���A�ƨA��jA��FA��!A��!A��A���A��DA�t�A�l�A�dZA�VA�C�A�$�A��A�{A�oA�VA�  A��A��#A��jA���A�v�A��A��
A�ȴA��^A���A��PA�l�A�E�A�$�A���A���A�A��9A��\A�?}A��A���A��A���A��DA��A�jA�/A���A��;A���A���A��^A���A���A���A��\A�x�A��A��A�z�A�dZA�G�A�=qA�/A�$�A�A��HA�ĜA��DA�-A��#A�jA�JA��!A�t�A�dZA�M�A�1'A�-A�-A�%A��A�ĜA���A��hA�^5A�`BA�M�A�33A�+A��A���A�x�A�$�A��A��jA�~�A�bNA�7LA�{A��A���A��!A�t�A�ZA�S�A�O�A�O�A�O�A�S�A�S�A�Q�A�K�A�I�A�G�A�E�A�C�A�?}A�7LA�5?A�9XA�5?A�+A�A��A�A�A��FA�l�A�M�A�"�A��mA��RA�hsA��A��RA�n�A�A�A�5?A�"�A���A���A��A�E�A��A��A���A�l�A�S�A�/A��yA���A���A��A�~�A�l�A�$�A��jA��uA�C�A��;A���A�O�A�{A��A��A�ƨA��9A���A��\A�z�A�(�A��A�ĜA���A��A�l�A�O�A�=qA��A���A��mA��A���A��9A���A��A�bNA�C�A�+A��A�
=A�A�  A���A��TA���A���A���A��wA��!A���A���A���A��\A��7A�|�A�r�A�ffA�\)A�K�A�=qA�7LA�&�A�
=A��/A���A��\A�~�A�ZA�C�A�9XA��A�ȴA�jA��A��A��yA��mA��#A���A��hA�|�A�ZA�bA�A��\A�^5A�C�A��A��/A���A���A��RA��FA��9A��!A��A��A���A�|�A�?}A��A���A��jA���A�n�A�"�A�%A���A���A��A��HA�ȴA��^A��-A���A�r�A�C�A�$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
�xB
��B
�xB
��B
�DB
�xB
�DB
�xB
�xB
�DB
�xB
�xB
��B
�DB
�DB
�xB
�xB
��B
�DB
��B
��B
��B
��B
�JB
��B
��B
�"B
��B
��B
��B
��B
��B
�"B
�VB
�VB
�VB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�"B
��B
��B
��B
��B
��B
��B
��B
��B
�kB
��B
�,B%B6B�B��B��B�8B�JB�B��B��BB�B�B�B�B"B�BxBB�B�B3�BD�BC-BA�B?�B5�B4�B5?B4�B4B'RB'�B,�BB��B��B�BƨB��B��B��B�[B��B�qB�BcBo B]dB1[BbB
�B
�EB
�[B
�B
�wB
��B
��B
�:B
�B
��B
�+B
��B
l�B
e,B
_�B
[�B
X�B
T�B
I�B
E9B
5�B
�B
�B	�B	��B	�KB	�2B	��B	��B	�B	�\B	��B	�B	e`B	D�B	;�B	0�B	B	PB	�B�B��B��B�yB��B��BуB�vB�B�BB�XB�B�gB�gB��B�B��B�^B�RB��B�B�LB�aB�[B�B��B�$B�nB�hB�:B�'B��B��B��B�B�B��B��B�VB�~B�CB��B��B�qB�kB�_B�B��B�B�SB�\B�bB��B�PB�PB�PB�PB�VB��B�\B�.B��B��B�+B�lB��B�xB��B�PB�JB�B��B�rB��B�xB��B�rB�=B��B�YB��B��B��B��B��B��B��B��B��B�@B��B��B�FB�7B�!B�B�FB��B��B��B�'B��B�B�zB�XB�qBB� B��B��B�TB�B�B��B		�B	PB	�B	JB	~B	�B	{B	�B	#nB	&�B	'�B	*�B	-B	0�B	2�B	4�B	4�B	4�B	5�B	7B	8�B	:*B	?�B	EB	E�B	FtB	GEB	F�B	HKB	JXB	OvB	R�B	YKB	Z�B	\�B	d�B	f�B	g8B	h�B	iyB	k�B	pB	s�B	t�B	tB	u�B	x�B	x8B	|�B	��B	��B	��B	�	B	�PB	�.B	� B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	��B	�6B	�B	�UB	�aB	��B	�-B	��B	�B	��B	��B	�wB	�}B	��B	��B	��B	�B	ƨB	�zB	ǮB	ȀB	�RB	�#B	�)B	��B	��B	�,B	�B	�
B	خB	�KB	ٴB	ٴB	ٴB	ںB	�)B	�jB	�NB	�B	�&B	�B	��B	�`B	�B	�fB	�B	�B	�DB	�yB	�yB	��B	��B	�B	��B	�|B	��B	�B	��B	��B	��B	��B	��B	��B	�>B	�8B	�lB	�lB	��B	��B	��B	��B	��B	��B	��B
�B
�B
B
SB
SB
B
�B
SB
SB
�B

	B

rB
�B
�B
~B
B
�B
DB
~B
�B
(B
(B
.B
�B
�B
hB
oB
FB
�B
�B
�B
B
SB
�B
$B
�B
+B
_B
eB
CB
IB
�B
�B
 \B
 �B
"�B
#�B
$�B
%�B
&B
&B
&B
&LB
&�B
'�B
'B
'RB
($B
(�B
(�B
*0B
)�B
)�B
*0B
*0B
*�B
+kB
+kB
,qB
,qB
,�B
-B
-B
.IB
.IB
.�B
/B
/OB
/�B
/�B
/�B
0!B
/�B
0UB
/�B
0�B
1[B
1�B
2-B
2-B
2�B
33B
33B
3�B
49B
4nB
4�B
5?B
5B
5B
5?B
5�B
5�B
6B
6zB
6�B
7B
6�B
8B
8RB
8RB
8�B
8B
8�B
9$B
9XB
9$B
8�B
8�B
8�B
9�B
9XB
9�B
9�B
9�B
:�B
;dB
<B
<B
<B
;�B
<B
<B
<jB
<�B
<�B
<�B
=qB
=�B
>wB
>wB
?B
?HB
?HB
@OB
@�B
@OB
@�B
AUB
AUB
A�B
B'B
B�B
B�B
C-B
C�B
D3B
D�B
EB
E�B
E�B
FB
FB
F?B
FtB
F�B
G�B
G�B
HKB
H�B
H�B
IB
IB
IRB
I�B
JXB
J�B
J�B
K^B
K�B
K�B
K�B
K�B
LdB
MB
M6B
M�B
M�B
M�B
N<B
NB
N<B
N�B
N�B
N�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�rB
�~B
��B
��B
��B
��B
�B
�B
��B
�B
�B
�~B
��B
�B
�PB
�	B
�DB
�B
�=B
�xB
��B
�rB
�DB
��B
�B
�rB
��B
�DB
�7B
�~B
�DB
��B
�B
�JB
��B
�rB
�~B
�DB
��B
�B
�JB
�rB
�xB
�B
�rB
��B
�B
�=B
�B
��B
�DB
��B
�PB
��B
��B
�xB
��B
�rB
�xB
��B
�B
��B
�JB
�=B
�JB
�xB
�DB
��B
��B
�=B
��B
��B
��B
��B
�~B
�rB
�rB
��B
�xB
�=B
�~B
��B
��B
�B
�B
�xB
��B
��B
�=B
�xB
��B
�lB
��B
�~B
��B
��B
�B
��B
�xB
��B
��B
�	B
��B
��B
��B
��B
��B
�B
�	B
��B
��B
��B
��B
��B
��B
�rB
��B
�B
�~B
��B
��B
�PB
�=B
�B
��B
�B
�B
�PB
�B
�4B
�(B
��B
��B
�\B
��B
��B
��B
�B
��B
�"B
�PB
��B
��B
�B
�VB
��B
�B
��B
��B
��B
��B
��B
�PB
��B
�VB
��B
��B
��B
�\B
��B
�B
��B
��B
�PB
��B
�(B
��B
�PB
��B
�"B
��B
��B
��B
��B
�(B
�VB
��B
�VB
�(B
�PB
��B
�(B
�B
��B
�(B
�"B
�B
��B
��B
�PB
�VB
�\B
�B
��B
�(B
�VB
�PB
��B
��B
�B
�VB
�(B
�PB
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
��B
��B
��B
��B
��B
��B
��B
�VB
��B
��B
�PB
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�(B
�PB
��B
��B
��B
�B
��B
�VB
�PB
��B
��B
��B
��B
�"B
��B
�PB
��B
��B
�(B
�PB
��B
�PB
��B
��B
�PB
��B
�VB
��B
�B
�B
��B
��B
�~B
��B
��B
�PB
��B
��B
�(B
��B
��B
�"B
��B
�PB
��B
��B
��B
��B
��B
��B
�VB
�PB
��B
�\B
�\B
�~B
�"B
�\B
�PB
�B
�"B
��B
��B
�PB
�\B
�(B
��B
��B
��B
�"B
��B
�"B
�(B
��B
��B
�VB
�(B
��B
��B
��B
�.B
�(B
�B
�@B
�4B
�:B
�B
��B
�!B
�VB
��B
��B
��B
��B
�BB
�OB
�B
ΥB
҉B
ٴB
��B
�yB
��B
�B
�2B
��B
�"B;B�B	7B�BMB#:B)�B*0B,�BH�BE9BH�Bs�B��B��B�$B�:B�XB�tB��B�bB��B�9B�<B��B�B�tB��B�.B��B�B�B��B�B�ZB�PB�rB��B��B��B�PB�B�>B��B��B��B�xB�PB�B�JB��B��B�xB��B�B��B��B��B��B�>B��B��B�"B�%B�rB��B��B�`B�lB�B*�B	7B�B�BABGBoBDB�B�B�B�B	�B�B�B	7BVB~B�BfB	�B�B�B�B�B�B+B�B�B�B%B%B+B	7B�B�B�B�BB	lBB�(B�B
�BJB
	B
	B
=B
rB
rB
=B
�B�B BuB(B"B~B�BB
	B~BB�B�BVB�B.B@B�B�BFB\BB~B~BPB�BB
	BBBxB	�B+B	�B�B
	B�B�B�B�B\B�B"4B�BkB�B�BYBYB$B�B�B$BYB�BB_B1B+B�BqB�BIB"4B$�B+6B2�B+�B+�B-wB*0B-wBB'BH�BA�B?�B>�BDgBD3BB�BE�BFBGzBGzBE�BF?BE�BB�BC-BC�BC�BA�BA�BCaBEmBEB@�BB'BB�BEmB>�B@�B@OB@�B?�BA�B@�BB�BD3BD�BC�BT�B?�B?�B@�B>wB<�B?B=�B:^B>BB9�B1[B3�B5?BB�B4�B4�B8RB2�B7LB0�B4�B9XB9�B6�B5�B4B6B5�B4nB3�B6B:�B-wB2�B/OB8�B8�B/�B1�B1�B7B/�B1'B7B>�B9�B9�B:�B?�B1[B4�B6FB4nB2aB2-B6�B2�B?�B9�B3�B1�B1�B1�B4B2-B5�B>wB:*B2�B6FB7�B3�B.�B0�B,qB-B'�B(�B0!B%FB$@B#B$tB'RB&LB%�B&�B'RB'�B'�B'�B&LB%�B&�B&�B$�B$tB&B-wB0�B5B:�B&B)_B($B.�B(XB+�B,qB#:B&�B�BB�B�B�BeB
rB	�B
rBB�B�JB�|B�B��B�8B�`B�fB�B�KB��B��BچB�&BܒB�]B�2BѷB�}B��B�NB�BB�jB�0B��B� B�B�^BɺB͟B�tB�mBƨBĜB�mB�UB� B��B�}B��B� B�B��B��B��B��B��B�LB��B��B��B��B�B�zB�FB�tB�9B�-B��B��B��B��B�aB��B�OB��B��B��B��B��B�wB�B��B�0B�$B�kB��B��B�XB�VB��B��B��B�VB��B��B�=B��B�_B��B�B��B��B��B��B��B��B��B��B� B}�B}"B.B��B�SB.Bt�Bs�BwfBu�Bu�Bo5BjKBh
Bh
BgmBe�Bc Bc�BdZB`vB]�BZ�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                 4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                 4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022081519133420220815191334IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022081717030620220817170306QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022081717030620220817170306QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                