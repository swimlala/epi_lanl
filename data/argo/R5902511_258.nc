CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  T   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:01Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  V�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ](   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ~p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ۠   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 2(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � 8�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` Sp   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   S�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Y�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T e�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   f$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   f,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   f4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   f<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � fD   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   f�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   f�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    f�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        g   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        g   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       g   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    g Argo profile    3.1 1.2 19500101000000  20230721225001  20230721225001  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�&��ŷA@�&��ŷA11  @�&����@�&����@3QQ���@3QQ����d�g��t~�d�g��t~11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�=q?��H@=p�@�  @�G�@\@�  A ��AG�A ��A,��A?\)A`  A�Q�A�Q�A�  A�Q�A�Q�AϮA߮A�\)A�\)B�
B�B�
B (�B(  B/�
B7�B?�
BH  BP  BX  B_�
Bh  Bp  Bw�
B�{B�{B�  B�{B�(�B�{B��B�  B�{B�{B�  B��
B��
B��B��B�{B�  B��
B��B�  B�{B�  B�{B�{B�{B�  B��
B��
B��B�  B��B��C 
=C  C  C  C{C

=C
=C
=C  C��C�C  C
=C
=C  C��C   C"
=C${C&{C(
=C*
=C,  C-�C/�C1�HC3�C6  C7��C:  C;��C=��C?�CA�CD  CF
=CG��CI��CL
=CN  CP
=CR  CT
=CV  CX  CZ
=C\
=C^  C`  Cb  Cc��Cf  Cg��Cj
=Cl  Cn
=Cp  Cq��Ct  Cu��Cx
=Cz  C{��C}��C�  C�C�  C�C�  C���C�  C�  C�C���C���C���C�  C�C�  C���C�  C�C�  C�  C�  C���C�  C�  C���C���C���C���C�C�
=C�C�C�  C���C�  C�C�  C�  C�  C�C�C���C�  C�  C���C���C�  C�  C�C���C�  C�  C���C���C���C�  C�C���C�  C�C�C�
=C�
=C���C�  C�
=C�  C���C�  C�  C�  C�C�C�C�  C�C�C�  C�
=C�C�C�C���C���C�C���C���C�  C�C���C���C���C�C�C�C�  C�  C�  C�
=C�
=C���C���C�  C�
=C�C�  C�C�C���C�  C���C�  C���C���C�  C�  C�  C�C�  C�C�
=C�  C���C�  C�C�C�C�  D   D }qD �qD��D�D��D  D� D�D�DD� D�qD}qD�D��DD��D�qD	}qD	�qD
��D  D� D  D��D�D}qD��D}qD  D��DD��D�qD}qD  D� D  D}qD  D}qD  D� D�qD� D�D� D  D� D�qD��D�D� D�D��D  D��DD��D�D}qD�qD� D�qD }qD!  D!}qD!��D"}qD#�D#�D$  D$� D$�qD%}qD&  D&��D'�D'��D(�D(��D)�D)� D)�qD*z�D+  D+� D+�qD,z�D,�qD-�D.  D.}qD/  D/}qD/��D0}qD1  D1��D2�D2� D3  D3� D3�qD4z�D5  D5��D6�D6� D7  D7��D8  D8��D9D9� D9�RD:}qD;D;��D;�qD<}qD<�qD=}qD>�D>� D?  D?��D@�D@� DA  DA� DB  DB��DC  DC}qDC��DD}qDE  DE� DF�DF}qDF��DG}qDH  DH}qDH�qDI��DJ�DJ� DJ�qDK}qDL�DL��DM�DM�DN  DN� DO  DOz�DO�qDP}qDQ  DQ��DR�DR� DS  DS}qDS��DTz�DT��DU� DVDV��DW�DW� DX�DX}qDY  DY�DZDZ�D[�D[�D\  D\}qD\��D]� D^D^��D_�D_� D_�qD`z�Da  Da�Da�qDbz�Dc  Dc� Dd�Dd� De  De��DfDf� Df�RDg� Dh�Dh�Di�Di� Dj  Dj}qDk  Dk}qDk�qDl�Dm  Dmz�Dm�qDn� Do�Do��Dp  Dp}qDp�qDq� Dr�Dr��Ds�Ds��Dt�Dt��Du�Du� Dv  Dv��Dw  Dw� Dx  Dx}qDx�qDy}qDy�qDz}qD{�D{�D|  D|� D}D}��D~  D~}qD~��D}qD�  D�AHD�� D��HD���D�>�D��HD��HD�  D�>�D�}qD���D���D�>�D�~�D�� D�  D�=qD�}qD���D�  D�AHD�� D��HD�HD�@ D�� D�D��D�AHD�~�D��qD�HD�B�D�~�D�� D��D�AHD��HD�� D���D�7
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�>���?B�\?��?��R?���?�ff@   @��@&ff@8Q�@O\)@\(�@p��@��
@��@�@��\@��@�\)@�p�@��
@˅@ٙ�@޸R@���@�33@�(�Az�A
=A��A33AffAp�A!G�A'
=A,��A0��A5A<(�A@  AE�AK�AP  AS�
AZ=qA_\)Ab�\Ah��Amp�Aq�Aw�Az�HA���A�33A���A�  A��\A�z�A��RA�=qA��
A�ffA�G�A��HA�p�A�Q�A��A���A��A�G�A��A�{A��A��\A��A�
=A���A�(�A�A��A��HA���AƸRAə�A��
A�p�A�  A�33A��A�
=A�=qA�z�A�{A���A��
A�p�A�Q�A��HA�z�A�\)A�=qA��
A��RA�G�A��HA�B (�B ��BffB�
B��B{B\)BQ�B	p�B
=B  B��B�\B�B��B{B�Bz�Bp�B33B(�BG�B�HB�B�B�\B\)B ��B"{B"�HB$z�B%��B&�RB((�B)�B*=qB+�B,��B-B/
=B0z�B1G�B3
=B4(�B5�B6ffB7�
B8��B:=qB;�B<z�B>{B?33B@Q�BA��BC33BD(�BEp�BF�HBH  BH��BJ�\BK�BL��BNffBO\)BPz�BQBS\)BT(�BU��BV�HBW�
BX��BZffB[�B\��B]�B_�B`Q�Ba��Bc33Bdz�Bep�Bf�RBh(�Bip�BjffBl  BmG�Bn=qBo�Bq�Br{Bs\)Bt��BuBw33Bx��By�B{
=B|(�B}B~�HB�  B��RB�\)B��
B��\B�G�B�B�=qB���B��B�(�B���B�G�B�  B�z�B��HB���B�{B�ffB��B��B�  B�Q�B��HB�\)B��B�{B��\B��RB�33B��B�B�=qB���B��HB�G�B��B��B�(�B���B���B��B���B�  B�(�B�z�B���B�33B�p�B��
B�Q�B�z�B���B�G�B�p�B��B�(�B��\B��HB�
=B�p�B��B�=qB�ffB���B�G�B�\)B�B�=qB��\B��RB��B���B��
B�{B��\B���B�
=B��B��B�  B�Q�B���B�G�B��B�B�  B�z�B��HB�
=B�\)B��
B�=qB�ffB���B�G�B�p�B��B�=qB���B���B�
=B��B��B�(�B��\B�
=B�33B��B�  B�ffB�z�B�
=B��B�B�  B�z�B��HB��B�p�B�  B�=qB��\B���B�p�B�B�  B���B��HB��B���B�{B�Q�B���B�33B�p�B�B�=qB���B���B�33B���B�{B�ffB���B�33B���B�B�Q�B���B�
=B�\)B��B�ffB���B��B��B��B�Q�B���B�\)B���B��B��\B��HB�G�B���B�(�B���B��HB�33B�B�=qB�z�B��HBÅBîB�(�BĸRB�
=B�\)B��
B�Q�Bƣ�B���BǅB�  B�ffBȸRB��B�B�(�B�z�B���B˅B��
B�=qB��HB�33B͙�B�{BθRB�
=B�\)B�  B�ffBиRB�\)B�B�  B�z�B��BӅB��
B�ffB��HB�G�Bՙ�B�=qB֣�B��B׮B�  B�ffB���B�G�BٮB�=qBڸRB�
=BۅB�{B�ffB���B�\)B��
B�{Bޏ\B�33B�p�B�  B��\B���B�G�B��
B�(�B�RB�G�B�B�  B�\B���B�G�B��B�{B�\B��B�B��
B�z�B���B�33B��
B�(�B�\B�33B�B��
B�z�B���B�33B��B�Q�B��B���B�p�B�{B�Q�B���B�G�B�B�  B�z�B��B�p�B�B�{B��RB��B�p�B�B�Q�B��RB�
=B��B�  B�ffB���B�
=B��B�  B�=qB���B�33B���B��
B�Q�B��HB�33B��B��
B�ffB���B�33B��C   C =qC \)C �C �
C ��C�CffC�\C�C�C33CQ�Cz�C�C�C�CG�CffC��C�HC
=C(�C\)C��C��C��C�C\)C�\C�RC�
C�CQ�C�C�C��C
=CG�Cz�C��C��C��C{CQ�C�C�RC�
C��C	(�C	ffC	��C	C	�C
�C
Q�C
�\C
C
�C
=C=qC�C�C��C
=CG�Cp�C��C�RC�C�C\)C��C��C��C{C=qCz�C�C�C{C33C\)C�\CC  C33CQ�Cz�C��C�
C{CG�CffC�\CC  C=qCffC�CC  C(�CQ�C�C�RC
=C33C\)C�CC  C=qCp�C��CC��C33Cp�C��CC��C(�CffC��C��C�C�CQ�C��C��C  C33C\)C�\CC  C=qCp�C�RC�HC
=C=qCz�CC  C33CffC��C��C  C(�C\)C�\C�
C�CQ�Cz�C�C�C33Cp�C�C�C �C G�C �\C �
C!�C!Q�C!z�C!�C"  C"=qC"z�C"��C"�HC#{C#\)C#��C#�HC$�C$Q�C$�C$�RC$�C%=qC%�C%C&  C&=qC&ffC&��C&�HC'{C'\)C'��C'�HC((�C(Q�C(�C(C){C)\)C)��C)��C)��C*G�C*�\C*C*��C+33C+p�C+�C,  C,G�C,�\C,��C-
=C-=qC-z�C-�RC-�C.(�C.�C.��C/{C/Q�C/�\C/��C0  C0G�C0��C0�HC1(�C1\)C1��C1�
C2�C2ffC2�RC3  C333C3p�C3�C4  C4=qC4�C4��C5
=C5G�C5z�C5C6{C6\)C6��C6��C7
=C7=qC7z�C7C8{C8Q�C8�\C8��C9
=C933C9ffC9�C9��C:=qC:z�C:�RC:�C;�C;\)C;��C;��C<{C<Q�C<�\C<�HC=(�C=p�C=�C=�HC>{C>\)C>��C>�HC?�C?ffC?�C?�C@(�C@p�C@�RCA  CA=qCAz�CA�RCA�HCB(�CBffCB�CB�CC(�CCp�CC�RCC��CD33CDz�CD�CD��CE33CEp�CE��CE�CF(�CFffCF��CF�
CG{CGQ�CG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�=q?��H@=p�@�  @�G�@\@�  A ��AG�A ��A,��A?\)A`  A�Q�A�Q�A�  A�Q�A�Q�AϮA߮A�\)A�\)B�
B�B�
B (�B(  B/�
B7�B?�
BH  BP  BX  B_�
Bh  Bp  Bw�
B�{B�{B�  B�{B�(�B�{B��B�  B�{B�{B�  B��
B��
B��B��B�{B�  B��
B��B�  B�{B�  B�{B�{B�{B�  B��
B��
B��B�  B��B��C 
=C  C  C  C{C

=C
=C
=C  C��C�C  C
=C
=C  C��C   C"
=C${C&{C(
=C*
=C,  C-�C/�C1�HC3�C6  C7��C:  C;��C=��C?�CA�CD  CF
=CG��CI��CL
=CN  CP
=CR  CT
=CV  CX  CZ
=C\
=C^  C`  Cb  Cc��Cf  Cg��Cj
=Cl  Cn
=Cp  Cq��Ct  Cu��Cx
=Cz  C{��C}��C�  C�C�  C�C�  C���C�  C�  C�C���C���C���C�  C�C�  C���C�  C�C�  C�  C�  C���C�  C�  C���C���C���C���C�C�
=C�C�C�  C���C�  C�C�  C�  C�  C�C�C���C�  C�  C���C���C�  C�  C�C���C�  C�  C���C���C���C�  C�C���C�  C�C�C�
=C�
=C���C�  C�
=C�  C���C�  C�  C�  C�C�C�C�  C�C�C�  C�
=C�C�C�C���C���C�C���C���C�  C�C���C���C���C�C�C�C�  C�  C�  C�
=C�
=C���C���C�  C�
=C�C�  C�C�C���C�  C���C�  C���C���C�  C�  C�  C�C�  C�C�
=C�  C���C�  C�C�C�C�  D   D }qD �qD��D�D��D  D� D�D�DD� D�qD}qD�D��DD��D�qD	}qD	�qD
��D  D� D  D��D�D}qD��D}qD  D��DD��D�qD}qD  D� D  D}qD  D}qD  D� D�qD� D�D� D  D� D�qD��D�D� D�D��D  D��DD��D�D}qD�qD� D�qD }qD!  D!}qD!��D"}qD#�D#�D$  D$� D$�qD%}qD&  D&��D'�D'��D(�D(��D)�D)� D)�qD*z�D+  D+� D+�qD,z�D,�qD-�D.  D.}qD/  D/}qD/��D0}qD1  D1��D2�D2� D3  D3� D3�qD4z�D5  D5��D6�D6� D7  D7��D8  D8��D9D9� D9�RD:}qD;D;��D;�qD<}qD<�qD=}qD>�D>� D?  D?��D@�D@� DA  DA� DB  DB��DC  DC}qDC��DD}qDE  DE� DF�DF}qDF��DG}qDH  DH}qDH�qDI��DJ�DJ� DJ�qDK}qDL�DL��DM�DM�DN  DN� DO  DOz�DO�qDP}qDQ  DQ��DR�DR� DS  DS}qDS��DTz�DT��DU� DVDV��DW�DW� DX�DX}qDY  DY�DZDZ�D[�D[�D\  D\}qD\��D]� D^D^��D_�D_� D_�qD`z�Da  Da�Da�qDbz�Dc  Dc� Dd�Dd� De  De��DfDf� Df�RDg� Dh�Dh�Di�Di� Dj  Dj}qDk  Dk}qDk�qDl�Dm  Dmz�Dm�qDn� Do�Do��Dp  Dp}qDp�qDq� Dr�Dr��Ds�Ds��Dt�Dt��Du�Du� Dv  Dv��Dw  Dw� Dx  Dx}qDx�qDy}qDy�qDz}qD{�D{�D|  D|� D}D}��D~  D~}qD~��D}qD�  D�AHD�� D��HD���D�>�D��HD��HD�  D�>�D�}qD���D���D�>�D�~�D�� D�  D�=qD�}qD���D�  D�AHD�� D��HD�HD�@ D�� D�D��D�AHD�~�D��qD�HD�B�D�~�D�� D��D�AHD��HD�� D���D�7
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�>���?B�\?��?��R?���?�ff@   @��@&ff@8Q�@O\)@\(�@p��@��
@��@�@��\@��@�\)@�p�@��
@˅@ٙ�@޸R@���@�33@�(�Az�A
=A��A33AffAp�A!G�A'
=A,��A0��A5A<(�A@  AE�AK�AP  AS�
AZ=qA_\)Ab�\Ah��Amp�Aq�Aw�Az�HA���A�33A���A�  A��\A�z�A��RA�=qA��
A�ffA�G�A��HA�p�A�Q�A��A���A��A�G�A��A�{A��A��\A��A�
=A���A�(�A�A��A��HA���AƸRAə�A��
A�p�A�  A�33A��A�
=A�=qA�z�A�{A���A��
A�p�A�Q�A��HA�z�A�\)A�=qA��
A��RA�G�A��HA�B (�B ��BffB�
B��B{B\)BQ�B	p�B
=B  B��B�\B�B��B{B�Bz�Bp�B33B(�BG�B�HB�B�B�\B\)B ��B"{B"�HB$z�B%��B&�RB((�B)�B*=qB+�B,��B-B/
=B0z�B1G�B3
=B4(�B5�B6ffB7�
B8��B:=qB;�B<z�B>{B?33B@Q�BA��BC33BD(�BEp�BF�HBH  BH��BJ�\BK�BL��BNffBO\)BPz�BQBS\)BT(�BU��BV�HBW�
BX��BZffB[�B\��B]�B_�B`Q�Ba��Bc33Bdz�Bep�Bf�RBh(�Bip�BjffBl  BmG�Bn=qBo�Bq�Br{Bs\)Bt��BuBw33Bx��By�B{
=B|(�B}B~�HB�  B��RB�\)B��
B��\B�G�B�B�=qB���B��B�(�B���B�G�B�  B�z�B��HB���B�{B�ffB��B��B�  B�Q�B��HB�\)B��B�{B��\B��RB�33B��B�B�=qB���B��HB�G�B��B��B�(�B���B���B��B���B�  B�(�B�z�B���B�33B�p�B��
B�Q�B�z�B���B�G�B�p�B��B�(�B��\B��HB�
=B�p�B��B�=qB�ffB���B�G�B�\)B�B�=qB��\B��RB��B���B��
B�{B��\B���B�
=B��B��B�  B�Q�B���B�G�B��B�B�  B�z�B��HB�
=B�\)B��
B�=qB�ffB���B�G�B�p�B��B�=qB���B���B�
=B��B��B�(�B��\B�
=B�33B��B�  B�ffB�z�B�
=B��B�B�  B�z�B��HB��B�p�B�  B�=qB��\B���B�p�B�B�  B���B��HB��B���B�{B�Q�B���B�33B�p�B�B�=qB���B���B�33B���B�{B�ffB���B�33B���B�B�Q�B���B�
=B�\)B��B�ffB���B��B��B��B�Q�B���B�\)B���B��B��\B��HB�G�B���B�(�B���B��HB�33B�B�=qB�z�B��HBÅBîB�(�BĸRB�
=B�\)B��
B�Q�Bƣ�B���BǅB�  B�ffBȸRB��B�B�(�B�z�B���B˅B��
B�=qB��HB�33B͙�B�{BθRB�
=B�\)B�  B�ffBиRB�\)B�B�  B�z�B��BӅB��
B�ffB��HB�G�Bՙ�B�=qB֣�B��B׮B�  B�ffB���B�G�BٮB�=qBڸRB�
=BۅB�{B�ffB���B�\)B��
B�{Bޏ\B�33B�p�B�  B��\B���B�G�B��
B�(�B�RB�G�B�B�  B�\B���B�G�B��B�{B�\B��B�B��
B�z�B���B�33B��
B�(�B�\B�33B�B��
B�z�B���B�33B��B�Q�B��B���B�p�B�{B�Q�B���B�G�B�B�  B�z�B��B�p�B�B�{B��RB��B�p�B�B�Q�B��RB�
=B��B�  B�ffB���B�
=B��B�  B�=qB���B�33B���B��
B�Q�B��HB�33B��B��
B�ffB���B�33B��C   C =qC \)C �C �
C ��C�CffC�\C�C�C33CQ�Cz�C�C�C�CG�CffC��C�HC
=C(�C\)C��C��C��C�C\)C�\C�RC�
C�CQ�C�C�C��C
=CG�Cz�C��C��C��C{CQ�C�C�RC�
C��C	(�C	ffC	��C	C	�C
�C
Q�C
�\C
C
�C
=C=qC�C�C��C
=CG�Cp�C��C�RC�C�C\)C��C��C��C{C=qCz�C�C�C{C33C\)C�\CC  C33CQ�Cz�C��C�
C{CG�CffC�\CC  C=qCffC�CC  C(�CQ�C�C�RC
=C33C\)C�CC  C=qCp�C��CC��C33Cp�C��CC��C(�CffC��C��C�C�CQ�C��C��C  C33C\)C�\CC  C=qCp�C�RC�HC
=C=qCz�CC  C33CffC��C��C  C(�C\)C�\C�
C�CQ�Cz�C�C�C33Cp�C�C�C �C G�C �\C �
C!�C!Q�C!z�C!�C"  C"=qC"z�C"��C"�HC#{C#\)C#��C#�HC$�C$Q�C$�C$�RC$�C%=qC%�C%C&  C&=qC&ffC&��C&�HC'{C'\)C'��C'�HC((�C(Q�C(�C(C){C)\)C)��C)��C)��C*G�C*�\C*C*��C+33C+p�C+�C,  C,G�C,�\C,��C-
=C-=qC-z�C-�RC-�C.(�C.�C.��C/{C/Q�C/�\C/��C0  C0G�C0��C0�HC1(�C1\)C1��C1�
C2�C2ffC2�RC3  C333C3p�C3�C4  C4=qC4�C4��C5
=C5G�C5z�C5C6{C6\)C6��C6��C7
=C7=qC7z�C7C8{C8Q�C8�\C8��C9
=C933C9ffC9�C9��C:=qC:z�C:�RC:�C;�C;\)C;��C;��C<{C<Q�C<�\C<�HC=(�C=p�C=�C=�HC>{C>\)C>��C>�HC?�C?ffC?�C?�C@(�C@p�C@�RCA  CA=qCAz�CA�RCA�HCB(�CBffCB�CB�CC(�CCp�CC�RCC��CD33CDz�CD�CD��CE33CEp�CE��CE�CF(�CFffCF��CF�
CG{CGQ�CG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�A�A�?}A�-A��A�oA��/A���A��
A���A���Aқ�A�hsA�S�A�K�A�G�A�7LA��A���A��A���A�z�A�-A��HAϮA�|�A�A�A��A΃A�{A͉7A��A��TA�~�A˝�A�JA�ĜA�^5A�JAř�Aã�A�oA���A��9A�
=A��/A��;A���A�x�A��A��wA���A�|�A��
A��\A��A�hsA��+A�/A��RA��A�l�A�1A���A�I�A��`A�jA���A��DA�bNA�G�A�XA�$�A���A�oA�bA��FA�t�A���A�M�A��A��A�  A�ffA�1A��A�JA�-A��wA��A��RA�A�A�p�A���A�jA��#A���A��uA�I�A��9A�r�A���A�VA�
=A��hA��;A��A%A~$�A}t�A|��A{t�Ay�7At��ApM�Am�
AmoAlQ�Ai��Ac��A`v�A]`BAY/ASt�AN��AL�/AJ�\AH-AE�AD�\AC�ACx�ABA�A=�mA=l�A;\)A9�#A8��A8bNA7�PA4�uA/�PA.bNA,�RA(�A&ĜA%��A%�A$ �A#�-A#�A#\)A#x�A#XA"$�A �jA|�A�uA��A�A9XA33A��AM�A�A�A�RA"�A�A �A|�A�TAt�A&�A��AE�AXA��A��A=qA��A
�A	��A	+A�Ar�A=qA  A�TA`BA��A��A��AjA{A�7AC�A%A�+A�A�A�A�AhsA �A ��A v�A I�@��;@���@�M�@��@�?}@��j@�o@���@��m@���@�K�@��@���@�J@�ff@��@�@�1'@�ƨ@��@�F@�@�{@�x�@�X@�/@�u@�@�D@�1'@�C�@��@��@�x�@�bN@�A�@�1@�ȴ@���@ۍP@�V@��@�~�@�X@܋D@���@��H@�J@��T@ٺ^@؛�@��y@� �@�ȴ@���@�p�@�r�@�r�@Гu@�Z@У�@��;@�l�@�|�@���@͡�@���@�1@��m@�ƨ@�\)@�ȴ@�^5@�1'@��
@Ǖ�@�dZ@�C�@��@ư!@Ƨ�@Ə\@�-@őh@�hs@őh@�x�@�/@��`@Ĭ@ă@�j@� �@î@�K�@�+@�V@�@�p�@��`@�I�@�  @�\)@��y@���@�~�@�{@��@�@���@��-@���@�hs@�`B@�G�@��/@��u@�I�@�  @���@�|�@�\)@�o@�ȴ@�~�@���@�hs@���@�A�@�b@��w@��@���@�V@�`B@���@� �@�1@�1@��@�ȴ@�ff@���@���@�hs@��/@��u@�9X@� �@�b@��;@�|�@�S�@�+@��H@���@�M�@�$�@�@���@��@�&�@��D@�Z@�1'@�b@��m@�C�@���@���@��-@���@�G�@��/@�bN@� �@��@�K�@��@���@�~�@�{@��7@�V@���@�j@�Q�@�I�@�9X@��@��F@�t�@�
=@��H@���@���@�ff@�{@��@��/@��u@�A�@��
@�t�@��@��y@��R@���@�ff@�J@�@��7@�/@�Ĝ@��@�I�@�9X@� �@�  @�  @��@���@��R@�=q@�@��T@���@��-@��@�7L@��@�1'@��@�b@�  @��m@���@���@�t�@�C�@�"�@�@�ȴ@�~�@�M�@�=q@�$�@�{@�@��@��@��@�@���@���@��\@��+@�v�@�n�@�V@�E�@�E�@��@��T@�?}@��@��@�I�@�1@�  @��@��;@�t�@��\@�ff@�ff@�V@�=q@�5?@�-@�-@�$�@�J@��@��^@���@��`@�1'@�1@�  @��m@��w@���@�l�@�+@�~�@�{@�@��T@��^@��@��@��@��@��`@���@��@�V@��@��`@��j@�j@�9X@��
@���@�|�@�t�@�l�@�K�@���@�~�@��@�@���@�p�@��@��`@��u@� �@�ƨ@�S�@�;d@��@��y@���@�ff@��@�`B@�V@�%@��j@�1'@\)@~@}�h@}��@}��@}��@}�h@}`B@}O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�?}A�=qA�E�A�;dA�E�A�;dA�A�A�E�A�A�A�?}A�?}A�33A�/A�-A�&�A�&�A��A�{A�bA�-A���A�  A��A���A���A���A���A���A���A���A��A��
A��A���A���A���A���A�Aҧ�A҅A�~�A�|�A�v�A�n�A�p�A�ffA�dZA�bNA�\)A�ZA�ZA�S�A�S�A�Q�A�K�A�O�A�O�A�I�A�I�A�O�A�I�A�I�A�K�A�G�A�I�A�I�A�C�A�G�A�G�A�C�A�A�A�A�A�9XA�33A�+A�"�A��A�JA��A��#A��;A��/A��#A��;A��/A���A�ȴAѾwAёhAуA�p�A�"�A�A�  A��A��yA��HA���A�ĜA�AиRAЮAЧ�AН�AБhAЃA�r�A�hsA�ffA�\)A�O�A�=qA�1'A�$�A��A�JA���A��A��yA��;A���A���A��
A���A�ȴAϸRAϧ�Aϙ�AϓuAχ+AσAχ+AρAρA�x�A�jA�ZA�G�A�A�A�A�A�=qA�7LA�7LA�-A�"�A� �A��A�VA���A��AΣ�AΉ7A΃A�n�A�\)A�M�A�C�A�A�A�+A�A��A��/A���A���AͬA�z�A�ffA�\)A�A�A�-A�+A��A��A�oA�1A���A��A��TA��HA��HA��HA���A̲-A̝�A̍PA�v�A�`BA�O�A���A˼jA˥�AˋDA˃A�~�A˃A�t�A�33A�%A��A���AʮA�r�A��A��
Aɧ�A�ffA�$�A���Aȕ�A�M�A�A�A�C�A�?}A�=qA�C�A�E�A�A�A�?}A��mAǍPAƾwA�
=A���A�ƨA�|�A��`A�  A�ȴA�n�A�5?A�ȴA�n�A�G�A�(�A�{A�1A�%A���A���A���A�  A�1A�+A�S�A�A�A�A�(�A�1A´9A�I�A�bA��A��9A��uA�v�A�bNA�S�A�;dA��A�%A��A�ĜA���A���A���A���A�ƨA��HA��^A���A���A��-A�A��A��
A��/A��`A�A��A�33A�9XA�C�A�K�A�C�A�-A��A�A���A���A�%A�A�
=A��A��A�^5A�K�A�33A��A���A��/A���A���A���A��wA���A��FA���A���A�JA�(�A�33A�/A�(�A�&�A��`A��!A�z�A�Q�A��A�ȴA���A��+A��A�~�A�x�A�jA�hsA�ffA�ZA�I�A�=qA�(�A�JA��TA���A�\)A�/A�(�A�"�A��A�{A�{A�A��mA��HA���A���A���A���A��hA��DA��A��+A��A�l�A�1'A���A�\)A�7LA�VA�A���A��wA���A�|�A�jA�&�A���A�p�A��\A�l�A�7LA�A�A���A��A��;A���A�A��RA��FA��FA��-A��A���A���A��A��A��A��A�t�A�dZA�;dA��A�JA�%A�A���A��A���A��A�`BA�-A���A��FA�S�A��A��A��A���A���A�ĜA��9A���A��A�t�A�^5A�VA�O�A�I�A�G�A�C�A�9XA�5?A�1'A�-A��A�1A��A��jA�{A��!A�z�A�ffA�Q�A�9XA��A�1A���A��mA���A�A���A�jA�bNA�\)A�O�A�I�A�A�A�(�A��A�%A���A��+A�bNA�A�A�5?A�5?A�{A�A���A��mA��
A���A��wA��9A�S�A�+A�E�A�&�A� �A���A��!A�|�A�z�A�G�A�9XA�7LA�=qA�7LA�33A�&�A��A��A��A��
A��HA���A���A���A�A���A���A��A�x�A�jA�`BA�XA�K�A�?}A�7LA�5?A�/A� �A�bA���A��TA���A�ƨA�ȴA�ĜA��wA��-A���A���A��hA��hA�~�A�hsA�E�A��A���A��+A�p�A�^5A�O�A�I�A�=qA�9XA�7LA�-A�$�A�oA��TA��\A�+A��A��
A��RA��uA�r�A�\)A�I�A�33A�C�A�1A��A��`A��;A��;A���A�A��A��+A�hsA�S�A�-A�A��`A��A���A�ĜA��wA��^A��9A��A���A���A���A���A���A���A��uA��A�ZA�33A�A��RA��A��A�O�A�+A��A�A��yA��TA��-A��A�G�A�+A��A��A�dZA�C�A�&�A�JA���A��9A��A���A���A���A���A���A���A��uA��\A��+A��+A��+A��A�|�A�t�A�jA�M�A�?}A�=qA��A��A���A�bNA��A�~�A�JA�ĜA��jA��9A��A���A���A�~�A�^5A�+A�VA��yA��jA��7A�^5A�;dA���A���A�^5A�1'A��A���A���A��hA�n�A�ZA�?}A�$�A���A�ƨA�r�A�M�A�C�A�1'A�VA��
A�r�A���A��wA���A��PA�l�A�;dA��A�%A��A�x�A�M�A�;dA�33A�VA���A�n�A�\)A�Q�A�C�A�33A� �A�1A��!A��A�M�A� �A���A��A��A���A���A��A��uA�hsA�A�A�$�A��A��A�1A��A���A�ƨA��RA���A��A�ZA�33A���A���A��wA���A�~�A�=qA��
A��\A�|�A�r�A�jA�bNA�9XA��A��wA��uA��A�ZA��A�JA�A��A��#A��FA���A���A���A��DA��+A��A�z�A�ZA�?}A�$�A��A���A���A���A��A�jA�dZA�S�A�M�A�?}A�33A��A��A�ȴA��wA���A�JA�I�A��
A��\A�n�A�XA�?}A�?}A�(�A��A���A��A��#A���A�ĜA���A�ĜA��A���A���A���A���A���A���A���A���A���A���A���A��hA��\A��PA��7A��A��A��A�z�A�r�A�hsA�`BA�-A���A���A���A��wA���A��A��`A���A�ƨA��RA���A��7A�p�A�hsA�Q�A�=qA�1'A�(�A��A�oA�%A��A���A��9A���A��+A�l�A�XA�G�A�=qA�7LA�1'A�-A�(�A�$�A��A�JA�  A���A��A��HA���A��wA���A��\A��A�r�A�jA�^5A�E�A�7LA��A���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�A�A�?}A�-A��A�oA��/A���A��
A���A���Aқ�A�hsA�S�A�K�A�G�A�7LA��A���A��A���A�z�A�-A��HAϮA�|�A�A�A��A΃A�{A͉7A��A��TA�~�A˝�A�JA�ĜA�^5A�JAř�Aã�A�oA���A��9A�
=A��/A��;A���A�x�A��A��wA���A�|�A��
A��\A��A�hsA��+A�/A��RA��A�l�A�1A���A�I�A��`A�jA���A��DA�bNA�G�A�XA�$�A���A�oA�bA��FA�t�A���A�M�A��A��A�  A�ffA�1A��A�JA�-A��wA��A��RA�A�A�p�A���A�jA��#A���A��uA�I�A��9A�r�A���A�VA�
=A��hA��;A��A%A~$�A}t�A|��A{t�Ay�7At��ApM�Am�
AmoAlQ�Ai��Ac��A`v�A]`BAY/ASt�AN��AL�/AJ�\AH-AE�AD�\AC�ACx�ABA�A=�mA=l�A;\)A9�#A8��A8bNA7�PA4�uA/�PA.bNA,�RA(�A&ĜA%��A%�A$ �A#�-A#�A#\)A#x�A#XA"$�A �jA|�A�uA��A�A9XA33A��AM�A�A�A�RA"�A�A �A|�A�TAt�A&�A��AE�AXA��A��A=qA��A
�A	��A	+A�Ar�A=qA  A�TA`BA��A��A��AjA{A�7AC�A%A�+A�A�A�A�AhsA �A ��A v�A I�@��;@���@�M�@��@�?}@��j@�o@���@��m@���@�K�@��@���@�J@�ff@��@�@�1'@�ƨ@��@�F@�@�{@�x�@�X@�/@�u@�@�D@�1'@�C�@��@��@�x�@�bN@�A�@�1@�ȴ@���@ۍP@�V@��@�~�@�X@܋D@���@��H@�J@��T@ٺ^@؛�@��y@� �@�ȴ@���@�p�@�r�@�r�@Гu@�Z@У�@��;@�l�@�|�@���@͡�@���@�1@��m@�ƨ@�\)@�ȴ@�^5@�1'@��
@Ǖ�@�dZ@�C�@��@ư!@Ƨ�@Ə\@�-@őh@�hs@őh@�x�@�/@��`@Ĭ@ă@�j@� �@î@�K�@�+@�V@�@�p�@��`@�I�@�  @�\)@��y@���@�~�@�{@��@�@���@��-@���@�hs@�`B@�G�@��/@��u@�I�@�  @���@�|�@�\)@�o@�ȴ@�~�@���@�hs@���@�A�@�b@��w@��@���@�V@�`B@���@� �@�1@�1@��@�ȴ@�ff@���@���@�hs@��/@��u@�9X@� �@�b@��;@�|�@�S�@�+@��H@���@�M�@�$�@�@���@��@�&�@��D@�Z@�1'@�b@��m@�C�@���@���@��-@���@�G�@��/@�bN@� �@��@�K�@��@���@�~�@�{@��7@�V@���@�j@�Q�@�I�@�9X@��@��F@�t�@�
=@��H@���@���@�ff@�{@��@��/@��u@�A�@��
@�t�@��@��y@��R@���@�ff@�J@�@��7@�/@�Ĝ@��@�I�@�9X@� �@�  @�  @��@���@��R@�=q@�@��T@���@��-@��@�7L@��@�1'@��@�b@�  @��m@���@���@�t�@�C�@�"�@�@�ȴ@�~�@�M�@�=q@�$�@�{@�@��@��@��@�@���@���@��\@��+@�v�@�n�@�V@�E�@�E�@��@��T@�?}@��@��@�I�@�1@�  @��@��;@�t�@��\@�ff@�ff@�V@�=q@�5?@�-@�-@�$�@�J@��@��^@���@��`@�1'@�1@�  @��m@��w@���@�l�@�+@�~�@�{@�@��T@��^@��@��@��@��@��`@���@��@�V@��@��`@��j@�j@�9X@��
@���@�|�@�t�@�l�@�K�@���@�~�@��@�@���@�p�@��@��`@��u@� �@�ƨ@�S�@�;d@��@��y@���@�ff@��@�`B@�V@�%@��j@�1'@\)@~@}�h@}��@}��@}��@}�h@}`B@}O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�?}A�=qA�E�A�;dA�E�A�;dA�A�A�E�A�A�A�?}A�?}A�33A�/A�-A�&�A�&�A��A�{A�bA�-A���A�  A��A���A���A���A���A���A���A���A��A��
A��A���A���A���A���A�Aҧ�A҅A�~�A�|�A�v�A�n�A�p�A�ffA�dZA�bNA�\)A�ZA�ZA�S�A�S�A�Q�A�K�A�O�A�O�A�I�A�I�A�O�A�I�A�I�A�K�A�G�A�I�A�I�A�C�A�G�A�G�A�C�A�A�A�A�A�9XA�33A�+A�"�A��A�JA��A��#A��;A��/A��#A��;A��/A���A�ȴAѾwAёhAуA�p�A�"�A�A�  A��A��yA��HA���A�ĜA�AиRAЮAЧ�AН�AБhAЃA�r�A�hsA�ffA�\)A�O�A�=qA�1'A�$�A��A�JA���A��A��yA��;A���A���A��
A���A�ȴAϸRAϧ�Aϙ�AϓuAχ+AσAχ+AρAρA�x�A�jA�ZA�G�A�A�A�A�A�=qA�7LA�7LA�-A�"�A� �A��A�VA���A��AΣ�AΉ7A΃A�n�A�\)A�M�A�C�A�A�A�+A�A��A��/A���A���AͬA�z�A�ffA�\)A�A�A�-A�+A��A��A�oA�1A���A��A��TA��HA��HA��HA���A̲-A̝�A̍PA�v�A�`BA�O�A���A˼jA˥�AˋDA˃A�~�A˃A�t�A�33A�%A��A���AʮA�r�A��A��
Aɧ�A�ffA�$�A���Aȕ�A�M�A�A�A�C�A�?}A�=qA�C�A�E�A�A�A�?}A��mAǍPAƾwA�
=A���A�ƨA�|�A��`A�  A�ȴA�n�A�5?A�ȴA�n�A�G�A�(�A�{A�1A�%A���A���A���A�  A�1A�+A�S�A�A�A�A�(�A�1A´9A�I�A�bA��A��9A��uA�v�A�bNA�S�A�;dA��A�%A��A�ĜA���A���A���A���A�ƨA��HA��^A���A���A��-A�A��A��
A��/A��`A�A��A�33A�9XA�C�A�K�A�C�A�-A��A�A���A���A�%A�A�
=A��A��A�^5A�K�A�33A��A���A��/A���A���A���A��wA���A��FA���A���A�JA�(�A�33A�/A�(�A�&�A��`A��!A�z�A�Q�A��A�ȴA���A��+A��A�~�A�x�A�jA�hsA�ffA�ZA�I�A�=qA�(�A�JA��TA���A�\)A�/A�(�A�"�A��A�{A�{A�A��mA��HA���A���A���A���A��hA��DA��A��+A��A�l�A�1'A���A�\)A�7LA�VA�A���A��wA���A�|�A�jA�&�A���A�p�A��\A�l�A�7LA�A�A���A��A��;A���A�A��RA��FA��FA��-A��A���A���A��A��A��A��A�t�A�dZA�;dA��A�JA�%A�A���A��A���A��A�`BA�-A���A��FA�S�A��A��A��A���A���A�ĜA��9A���A��A�t�A�^5A�VA�O�A�I�A�G�A�C�A�9XA�5?A�1'A�-A��A�1A��A��jA�{A��!A�z�A�ffA�Q�A�9XA��A�1A���A��mA���A�A���A�jA�bNA�\)A�O�A�I�A�A�A�(�A��A�%A���A��+A�bNA�A�A�5?A�5?A�{A�A���A��mA��
A���A��wA��9A�S�A�+A�E�A�&�A� �A���A��!A�|�A�z�A�G�A�9XA�7LA�=qA�7LA�33A�&�A��A��A��A��
A��HA���A���A���A�A���A���A��A�x�A�jA�`BA�XA�K�A�?}A�7LA�5?A�/A� �A�bA���A��TA���A�ƨA�ȴA�ĜA��wA��-A���A���A��hA��hA�~�A�hsA�E�A��A���A��+A�p�A�^5A�O�A�I�A�=qA�9XA�7LA�-A�$�A�oA��TA��\A�+A��A��
A��RA��uA�r�A�\)A�I�A�33A�C�A�1A��A��`A��;A��;A���A�A��A��+A�hsA�S�A�-A�A��`A��A���A�ĜA��wA��^A��9A��A���A���A���A���A���A���A��uA��A�ZA�33A�A��RA��A��A�O�A�+A��A�A��yA��TA��-A��A�G�A�+A��A��A�dZA�C�A�&�A�JA���A��9A��A���A���A���A���A���A���A��uA��\A��+A��+A��+A��A�|�A�t�A�jA�M�A�?}A�=qA��A��A���A�bNA��A�~�A�JA�ĜA��jA��9A��A���A���A�~�A�^5A�+A�VA��yA��jA��7A�^5A�;dA���A���A�^5A�1'A��A���A���A��hA�n�A�ZA�?}A�$�A���A�ƨA�r�A�M�A�C�A�1'A�VA��
A�r�A���A��wA���A��PA�l�A�;dA��A�%A��A�x�A�M�A�;dA�33A�VA���A�n�A�\)A�Q�A�C�A�33A� �A�1A��!A��A�M�A� �A���A��A��A���A���A��A��uA�hsA�A�A�$�A��A��A�1A��A���A�ƨA��RA���A��A�ZA�33A���A���A��wA���A�~�A�=qA��
A��\A�|�A�r�A�jA�bNA�9XA��A��wA��uA��A�ZA��A�JA�A��A��#A��FA���A���A���A��DA��+A��A�z�A�ZA�?}A�$�A��A���A���A���A��A�jA�dZA�S�A�M�A�?}A�33A��A��A�ȴA��wA���A�JA�I�A��
A��\A�n�A�XA�?}A�?}A�(�A��A���A��A��#A���A�ĜA���A�ĜA��A���A���A���A���A���A���A���A���A���A���A���A��hA��\A��PA��7A��A��A��A�z�A�r�A�hsA�`BA�-A���A���A���A��wA���A��A��`A���A�ƨA��RA���A��7A�p�A�hsA�Q�A�=qA�1'A�(�A��A�oA�%A��A���A��9A���A��+A�l�A�XA�G�A�=qA�7LA�1'A�-A�(�A�$�A��A�JA�  A���A��A��HA���A��wA���A��\A��A�r�A�jA�^5A�E�A�7LA��A���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�DB
�B
�B
��B
�KB
�B
��B
�B
�B
�QB
�B
�"B
�B
�B
�B
��B�BYB"hBC-BK�BQNBXB^jBa�Bf2BpoBtTB��B�bB��B�QB�B�B�B5�B*�B�B�B��B��B��B4B�BXBsB�BںBܒB�2BŢB�B��B�lB��B��B.By�Bs�Bt�Bk�BqABm�Bl�BaBZBT�BQ�BNpB?B+kBBBoB�B�AB�&BɆB��B��B�fBncBS&B=�B?HB4�B7B.}B�B"B iB
�B
�cB
�B
��B
�BB
��B
�tB
�'B
��B
s�B
l"B
c�B
]�B
W
B
OB
C�B
<�B
6�B
0UB
+B
 'B
�B	��B	�2B	�wB	��B	��B	�'B	yrB	`B	P�B	1�B	 'B	uB��B��B�B��B֡B��B�NB�vB�aB�dB��B��B�}B�'B�B��B�\B�B��BsBo�Bj�Bl"Bm]Bn�Bo5BqABs�B�uB�OB��B��B��B�B��B�OB��B��B��B��B�hB��B��B��B�IB��B�'B��B��B��B��B�B�B�=B��B�B�!B�[B��B�3B��B��B�$B�RB��B�B�B��B��B��B��B��B�B�RB�RBɺBҽB�aB��B��B�mB�B��B�B��B�2B�sB��B��B�
B�ZB�B��B�B�QB�]B��B��B	fB	�B	_B	
�B	:B	�B	"hB	�B	=B	�B	~B	%�B	+B	4�B	4�B	2�B	 \B	�B	%FB	)�B	0�B	9$B	?�B	<�B	>wB	MB	RTB	_�B	kB	iDB	g8B	f�B	k�B	o B	s�B	rGB	kB	e�B	^jB	Z�B	[WB	XyB	\]B	`vB	e�B	h�B	q�B	r�B	v�B	y	B	y>B	{B	|PB	��B	�;B	��B	�B	��B	�SB	�SB	��B	��B	��B	��B	��B	��B	�eB	�eB	�eB	��B	�IB	��B	�tB	��B	�LB	��B	��B	�B	�$B	�$B	�RB	��B	��B	�kB	�B	�B	��B	�IB	��B	��B	�OB	�aB	�tB	��B	�$B	��B	��B	�*B	�^B	��B	��B	�0B	��B	�6B	�6B	�6B	�jB	��B	�B	��B	��B	��B	�3B	�gB	�aB	�?B	ȀB	�B	ʌB	��B	̘B	ʌB	�^B	͟B	ѷB	҉B	�2B	�9B	�
B	�mB	�?B	��B	��B	��B	�B	�KB	ںB	�#B	�WB	��B	ܒB	ݘB	�dB	�jB	�B	�;B	�vB	�B	��B	�B	�B	�B	�`B	��B	�B	�mB	�>B	�sB	�B	�B	�DB	�B	�B	�"B	�)B	�]B	��B	�iB	�oB	��B	��B	�vB	�B	�B	��B	�MB	�B	��B	�ZB	��B	�`B	�fB	�8B	�DB	��B	�B	��B	�B	�VB	��B	�VB	��B	��B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	�.B	�cB	��B	�cB
 �B
�B
B
�B
�B
�B
B
{B
B
�B
�B
YB
�B
�B
_B
�B
	7B
	�B
	�B

=B

rB

�B
�B
�B
�B
xB

�B
B
�B
xB
�B
PB
�B
�B
�B
�B
�B
�B
VB
"B
�B
VB
�B
bB
�B
�B
oB
�B
4B
hB
hB
B
B
B
�B
{B
FB
�B
{B
FB
FB
�B
�B
�B
{B
�B
$B
$B
�B
YB
�B
�B
�B
�B
�B
1B
eB
eB
�B
�B
�B
7B
B
�B
IB
 �B
"4B
!�B
!�B
"4B
"4B
"hB
"�B
#:B
#�B
#�B
#�B
$B
$�B
%zB
&�B
&�B
'B
'RB
'RB
&�B
&�B
&�B
'B
'RB
'RB
'B
'RB
'RB
'RB
'RB
($B
'�B
'�B
(�B
(�B
(�B
*eB
)�B
)_B
)_B
)*B
)�B
)�B
)�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�
B
�B
�B
�B
�B
�sB
�B
��B
�yB
�B
�B
�yB
�B
�B
�B
�B
��B
��B
��B
��B
�2B
�cB
�)B
�yB
�"B
�B
�"B
�B
��B
��B
�DB
��B
�KB
��B
��B
�B
��B
�AB
��B
��B
�B
��B
�B
�QB
�KB
�B
�"B
��B
�B
�B
�B
�"B
��B
��B
�B
�vB
�B
�B
�B
�B
�B
��B
�8B
��B
�B
�xB
�B
��B
��B
�]B{B�B�B	�B�BbB�B=B�BB�B�BeBIB �B#�B1�B2�B4nBGBEmBEBGBH�BH�BIBK�BJ�BJ�BM�BNBN�BP�BQ�BQNBR�BR BR�BT�BU2BWsBYKBY�BZB]/B`B]�B_pB_pB^B\)Ba|Bc�Bc�Bb�Bc B^�Ba�Ba�BaHBf�Be�BiDBk�Bo5BoiBqvBpBp�BqvBp�Bq�BtBr�Br�Bu�BwfBzDB��B�_B�1B��B� B�@B��B�B�	B�LB��B�B�3B�tB�dB�KB��B�B�,B�B�
B��B�#B��B�)BߤB�|B�B��B�B�fB��B�B�;B�oB��B��B��BB�BqB#nB$�B%zB#�B&�B4B7�B8�BA�B:�B9�B7�B1'B!�B#:B�B B B�B1B�BB�B�BAB�B�B~B+B��B� B��B��B��BҽB�B�TB�RB�gB�dB��B��B�B�B��B�CB�9B�*B�mB�HB��B�BYB�BB�B%�B/�B�BSB@B�BBBxB	B
	BB�B�B1B�B;BuBBuB%FB2aB9�B7B;0BA�BJ�BM�BK�BN<BVBQNBW
Bg�Bg�BjBo Bp�Bm]Bk�Bh>Bf2Bk�Bm�Bo�Bx8B�B��B�AB�B��B� B�B��B��B�4B�+B��B��B��B��B��B�9B��B�mB��B�]B�TB�B�B��B��B��BݘB�jB��B�QB��B�B�QB�]B��B��BیBܒB��B��BܒB�B��B�B�jB̘B̘B�#B�B�RB�B��BŢBʌBŢB��B�}B� B�B��B�OB��B�6B��B��B��B�=B��B��B��B�@B��B�}B��B�qB��B��B�+B�~B�7B�=B��B��B��B�B�B��B��B�%B��B��B��B�lB��B�uB�B�YB��B�1B�{B�{B�B~�B� B�oB��B�B}�B�oB�4B~�B��B|PBx�B~]Bz�By�Bx�B|PBy>Bz�BxlBz�Bv�Bt�Bv�Bt�Br�Br�Bs�Br�Bq�Bs�BsBr�Bu�B��BxBrBp�Bp�Bo�Bp�Bl�Bm]Bm�Bm]Bi�Bm�Bm)BhsBh
BiDBh�Bf�BiyB��By	Bs�Bx�Be�Bu�Bl�Bl�BqvBjBk�BsMBkBe�BiDBiDB|�Bo5B��Ba�Bd�Ba|Bt�BiB`�B]�Bb�B`BB^B^jB`BBa|B]�B`vBZB[#BU2BXEBXBV�BU2BYKBS�BW�BU2BT�BU2BR�BR�BR�BR�BP�BR BR�BS�BR BS[BS[BRTBOBBO�BQNBRTBMBOBL�BJ�BL�BN�BN<BPHBTaBD3BB[BA B:�B9�B9�B8B5�B6B6FB6�B5�B7B0UB%�B#:B!�B �B�B B�BB/�BSB�B%BB{BMB�B
	BPBBuB�BkB�B�BB�BMB�B@B�B:B�B:B4B�BbBBMB	B�BkB+B"�B�BSB�VB�8B��B�DB��B�]B��B��B�;B�`B�B�B�BB�dBܒB��B� B�B�NB�}B��BϫB͟B��B͟B˒B�B��B�)B��B�B�KBȀB�^B�B��B��B�9B��B��B�BB��B�RB�B�\B�B�B�IB�B��B�xB�B��B�YB�SB�:B��B�B�B�uB��B��B�B�4B��B|�Bv�Bt�Br�Bo�Bs�Br|Bn�BcTB_pBaB`�BbBf�B`BK�BI�BF�BF�BE�B@OB>B=�BJ#B6B5B2�B;�BNB?}BB�B@BA�B@�B=�B?HBB[B;�B9�B4�B-�B,B5tB8�B5�B8�B7�B>�B:^B9�B6zB4�B7�B5tB5B3�B49B1�B1�B3�B.}B1'B+B($B&�B'B)�B+BxB�B�BMBB7B�BoB	�B
rB�BJB�BB�BB	BB
��B 4B
��B
��B
�PB
�]B�B
�>B
�lB
�`B
�"B
�B
��B
�B
��B
�B
��B
��B
��B
�`B
�8B
�mB
��B
�jB
�B
�B
�B
��B
�6B
�tB
��B
B
��B
� B
��B
�-B
�9B
�<B
�UB
�-B
�$B
��B
��B
�$B
�$B
��B
��B
�B
�LB
�XB
��B
�B
��B
��B
��B
�B
�B
�aB
��B
��B
�[B
�'B
��B
�=B
��B
��B
ʌB
�B
� B
�hB
��B
�TB
��B
�lB
uZB
t�B
iDB
rGB
r|B
t�B
tB
p;B
o5B
ncB
k�B
m)B
l"B
k�B
l"B
g�B
gB
h>B
f2B
d�B
d�B
bNB
aB
a|B
`BB
_�B
_�B
_B
^5B
\]B
[�B
Z�B
Z�B
Z�B
[#B
YKB
U2B
TaB
U2B
R�B
T,B
R�B
Q�B
Q�B
Q4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                     444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B
�B
��B
��B
�2B
�B
��B
�>B
�lB
��B
�B
�B
�rB
�
B
��B
�]B
�%B
�.B�B�B?}BG�BM�BT`BZ�B^5Bb�Bl�Bp�B�%B��B�B֡B�B�B�B2-B&�B�B�B�EB�9B�8B�B�BT`BoiB�nB�
B��BтB��B�gB�CB��B��B� B{~Bu�BpBqABh
Bm�BjBiB]cBVmBQBN<BJ�B;dB'�B	kB\B�B	�B�B�vB��B��B�$B��Bj�BOvB9�B;�B1'B3gB*�BB
rB
��B
�fB
�B
�TB
�9B
��B
��B
��B
�wB
�B
o�B
hrB
`AB
Y�B
SZB
K^B
?�B
9#B
33B
,�B
'RB
wB
�B	��B	тB	��B	� B	��B	�wB	u�B	\]B	M5B	.B	wB��B��B�(B��B�KB��B�B͞B��B��B��B��B�KB��B�wB�jB��B��B�VB�BoiBl"Bg8BhrBi�BkBk�Bm�Bp;B��B��B��B�.B��B�eB��B��B�7B�0B�B��B��B�B��B�B��B�CB�wB�OB��B�'B��B�dB�^B��B��B�^B�qB��B�B��B�B��B�tB��B�B�mB�mB�9B��B�<B� B�&B�[BŢBŢB�
B�BбB�BB�BҽB��B�KB��B�B�B��B�%B�B�ZB�B�B�B�`B�B�B�GB��B	�B	�B	�B	+B	�B	B	�B	0B	�B	�B	�B	"3B	'RB	0�B	1'B	.�B	�B	7B	!�B	&B	,�B	5tB	<B	8�B	:�B	IQB	N�B	\)B	glB	e�B	c�B	b�B	h>B	kPB	p;B	n�B	glB	bB	Z�B	W
B	W�B	T�B	X�B	\�B	bB	d�B	n.B	o B	sMB	uYB	u�B	w�B	x�B	}"B	}�B	�:B	�_B	�	B	��B	��B	�B	�@B	��B	�B	�FB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�nB	�tB	�tB	��B	��B	��B	��B	�XB	�^B	��B	��B	�B	�6B	��B	��B	��B	�9B	�tB	�B	��B	�zB	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�^B	��B	�B	�BB	��B	��B	��B	B	��B	�gB	��B	�#B	��B	��B	ǮB	��B	�B	��B	тB	҉B	�ZB	ҽB	ӏB	�&B	�,B	�,B	�`B	՛B	�
B	�sB	קB	�EB	��B	��B	ٴB	ںB	�WB	ۋB	��B	��B	�5B	�iB	�iB	��B	�B	�B	��B	�B	�B	��B	��B	�`B	�B	�B	��B	�rB	�yB	�B	�JB	�B	�B	�(B	�.B	��B	��B	�cB	�5B	�B	�B	�AB	�B	�GB	�B	�B	�B	��B	��B	�fB	�B	�lB	��B	�	B	��B	�>B	�	B	��B	��B	�B	��B	�B	��B	�B	�JB	�JB	�~B	��B	��B	��B	�"B	�(B	�\B	��B	��B	�.B	�bB	��B
 iB
�B
@B
�B
B
GB
�B
�B
�B
�B
%B
�B
�B
+B
�B
�B
1B
�B
+B
eB
�B
�B
	B
	�B
	�B
	�B

=B

=B

=B

=B

�B

rB

=B

�B
B
�B
IB
�B
�B
�B
�B
�B
�B
\B
bB
bB
.B
�B
�B
�B
�B
�B
�B
�B
�B
4B
�B
B
tB
tB
@B
�B
�B
�B
�B
�B
LB
�B
�B
�B
�B
B
�B
�B
RB
�B
�B
�B
�B
OB
B
�B
�B
�B
!B
�B
�B
�B
�B
 [B
!-B
!�B
"�B
"�B
#nB
#�B
#�B
#9B
#9B
"�B
#nB
#�B
#�B
#nB
#�B
#�B
#�B
#�B
$tB
$@B
$B
$�B
%FB
$�B
&�B
%�B
%�B
%�B
%zB
%�B
%�B
&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�lB
�lB
�fB
�ZB
��B
�TB
��B
��B
��B
��B
�,B
��B
��B
��B
��B
��B
�lB
�B
��B
�JB
�NB
�%B
�B
�B
�B
�yB
��B
�rB
�`B
�rB
�lB
�,B
�B
�B
�B
�B
�8B
�DB
�fB
�B
�B
�B
�2B
��B
�B
�lB
�B
�B
�fB
�rB
�>B
�B
��B
�lB
�rB
�B
�B
��B
��B
��B
�WB
�iB
�oB
�oB
��B
�B
��B
�fB
��B
�`B
�	B
�JB
��B
��BBGB�B
	B�BB�B�BRB�BB�B�BIB�B.IB/B0�BCaBA�BAUBCaBD�BEBEmBHBGBGBJ#BJWBK)BMBN<BM�BOBBNpBOBQNBQ�BS�BU�BV8BVmBYB\]BY�B[�B[�BZQBXyB]�B`B`AB_B_pB[#B^5B^B]�Bb�BbNBe�Bh
Bk�Bk�Bm�BlWBl�Bm�Bl�Bn.BpoBo5Bo BrGBs�Bv�B�MB��B��B�B�PB��B��B�nB�YB��B�$B�kB��B��B��BěB�B�QB�|B�`B�ZB�8B�sB�B�yB��B��B�B�;B�B�B�>B�lB�B�B�;B��B�1B	kBB�B�B!-B!�B 'B"�B0UB49B4�B=�B7KB6EB4B-wBB�B�BPBPB�B�BBoB:B  B��B 4B�.B�B{B�7B�PB�5B�NB�EB�B�QBΤBŢB��B��B��B�FB�RB�bB��B��B��B�zB��B̘B�EB�iB�B�BbB�B!�B,<BCB�B�BB	kBeB�BSBYB_BBB�BGB��B��B�VB�B!�B.�B6B3gB7�B>BBF�BI�BHBJ�BRTBM�BSZBd%Bc�Bf�BkPBl�Bi�Bh
Bd�Bb�Bg�Bi�Bl"Bt�B�oB�4B~�B�SB�B�PB�VB�=B��B��B�{B�:B�4B��B�3B�NB��B�BBҽB�BحBߤB�B��B�;B�AB�B��BںB�B֡B�>B�WB֡BحB�B�B��B��B�B�KB��B�`B�<B�WBɺB��B��B�sB�QBŢB�aB�3B��B��B��B��B��B�pB�^B��B��B�B��B��B��B�B��B�BB�B�B��B�!B��B�B��B�3B��B�{B��B��B��B�+B��B�%B�SB�SB�MB�B�uB�B�GB�:B��B��B~�BbB��B�B��B�B�B~\B{JB|PB}�B��B�oBzDB}�B|�B{B�Bx�Bt�Bz�Bv�Bu�Bu%Bx�Bu�Bw1Bt�Bv�BsMBqABr�BqABo5Bo5BpBo5Bm�BpBoiBo BrGB��BtSBncBl�Bm(Bl"Bl�Bh�Bi�BjBi�Be�BjJBiyBd�BdZBe�Bd�Bb�Be�B.BuYBp;Bu%BbNBq�BiDBiBm�Bf�Bh
Bo�BglBbNBe�Be�By>Bk�B��B^5B`�B]�Bp�Be`B\�BY�B^�B\�BZQBZ�B\�B]�BZB\�BVmBWsBQ�BT�BT`BR�BQ�BU�BPBS�BQ�BQBQ�BN�BOBBN�BOBBM5BNpBOBO�BNpBO�BO�BN�BK�BK�BM�BN�BIQBK^BIBGBIBK)BJ�BL�BP�B@�B>�B=pB7KB6B5�B4mB2-B2aB2�B2�B2-B3gB,�B"3B�BOBB�BPB�BhB+�B�B�BuBoB��B �B�BYB	�BeB�BFB�BFB:BhBB�B�B�B�B�B�B�B�B�B�BVB�BYB:B�B{B�B�B�B��B�B��B��B�7B��B��B�B�B�B�lB��BܒBٴB��B�#B�pB�jB͞B��B�5B��B��B�#B��B��B�QB�KB�yB�?B�gBěB��BǮB�UB�BB�6B��B�B��B��B�-B��B�UB��B�eB�eB��B�eB�0B��B�RB�B��B��B��B�	B�eB�VB��B��B��B{�B|�B.By	BsBqABo Bk�Bo�Bn�Bj�B_�B[�B]cB]/B^iBcB\]BHBF
BB�BB�BB&B<�B:^B:)BFsB2aB1[B/OB8BJWB;�B>�B<jB=�B<�B:)B;�B>�B8B6B1'B)�B(XB1�B4�B2-B4�B3�B;0B6�B5�B2�B1'B4B1�B1[B/�B0�B.IB.IB0 B*�B-wB'RB$tB#9B#nB&B'RB�BFB:B�BhB�BB�B%B�B
	B�BB
�bB
�(B
�VBSB
�VB
�B
��B
�JB
�B
��B
��BB
��B
��B
�B
�rB
�B
�MB
�iB
�DB
��B
�>B
�2B
�8B
�B
�B
�B
�)B
ںB
��B
��B
��B
�EB
ɆB
��B
�B
��B
�B
�pB
�B
�}B
��B
��B
��B
�}B
�tB
��B
��B
�tB
�tB
�B
�9B
�mB
��B
��B
��B
�gB
�3B
��B
�3B
�UB
�UB
��B
�B
��B
��B
�wB
�B
��B
�B
��B
��B
�aB
�PB
��B
�4B
ΤB
�3B
��B
q�B
p�B
e�B
n�B
n�B
qB
poB
l�B
k�B
j�B
h>B
iyB
hrB
g�B
hrB
d%B
cTB
d�B
b�B
`�B
aB
^�B
]cB
]�B
\�B
[�B
[�B
[WB
Z�B
X�B
XB
W>B
V�B
V�B
WsB
U�B
Q�B
P�B
Q�B
OB
P|B
OBB
M�B
NB
Mj4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                     444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225001                            20230721225001AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122500120230721225001  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500120230721225001QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500120230721225001QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               