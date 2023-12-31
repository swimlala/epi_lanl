CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:03Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  Q�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  W   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  l�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  rX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � 
�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   !    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   '    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   -    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T 3    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   3T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   3\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   3d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   3l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 3t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   3�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        48   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        4@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       4H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    4PArgo profile    3.1 1.2 19500101000000  20230721225003  20230721225003  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�+�sK��@�+�sK��11  @�+����@@�+����@@2���*�@2���*��d�n���V�d�n���V11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?��@�@@  @}p�@��R@�G�@�G�A   A  A\)A+�A@  A_\)A�  A�  A��A�\)A�Q�AУ�A�Q�A�Q�B   B�
B(�B  B�
B((�B0(�B7�
B@  BH  BP  BX(�B`  Bh  Bp(�Bx  B�  B�{B�  B��B��B�{B�  B��
B��B��B��B�  B�{B�{B��
B��B�{B�{B�{B�  B��B�  B��B��B�  B�{B�  B��B�  B��
B�  B�{B��C�C��C��C�C	�C�C  C
=C��C
=C��C�C  C
=C  C   C!��C$
=C%�C'�C)�C+��C.
=C0  C2  C3��C6  C8{C:  C;��C>
=C@
=CB  CD
=CF  CG��CJ  CL  CN  CP
=CR  CT  CV  CW��CY�C\  C^  C`
=Ca��Cd
=Cf  Cg��Ci��Cl  Cn  Cp
=Cr
=Cs��Cv
=Cx
=Cz
=C|  C~
=C��C���C�  C���C���C�C�  C���C�C�  C�  C�C�  C���C�  C�C�  C�C�C�  C���C�  C�C�
=C�C�  C�  C�C�C�  C���C�  C�  C���C���C���C�  C���C���C���C�  C�  C���C�  C���C���C���C�  C�  C���C���C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�  C���C���C�C�C�  C�  C���C�  C���C���C�  C�  C�  C���C�  C�
=C�C�  C�C�C���C�  C�C���C���C�  C�C�C�C���C�  C�  C���C�  C�  C�  C�C���C�  C�  C�C�C���C���C�  C�
=C�C���C���C���C���C���C�  C�  C�  C�  C�C�C�  C���C�  C�  C���C���C�D �D }qD  D}qD�qD}qD�qDz�D��D}qD�D� D�qD��DD��D  D� D	�D	� D
�D
� D
�qD}qD�qD� D�qD� D�qD}qD�qD��DD�D�D� D�D��D�D� D  D��D�D��D�D}qD�qD}qD�qD� D  D}qD  D}qD�qD��D  D}qD�qD� D  D� D�D�D   D ��D!�D!��D"D"�D#�D#� D$  D$� D%  D%� D%�qD&� D'  D'� D(  D(}qD(�qD)� D)�qD*z�D*�qD+z�D+��D,z�D,�qD-� D.�D.� D/  D/��D/�qD0z�D1  D1��D2  D2��D3  D3� D4  D4}qD4�qD5� D6�D6�D7  D7� D8D8�D9�D9�D:D:� D;  D;z�D;��D<}qD=�D=��D=�qD>}qD?  D?� D@  D@}qD@�qDA�DB�DB}qDB��DC��DD  DD��DE�DE� DF�DF� DF�qDG� DH  DH� DI  DI��DJ  DJ}qDJ�qDK}qDK�qDL� DL�qDMz�DM��DN}qDO  DO}qDO�qDPz�DQ  DQ��DR�DR�DSDS��DT�DT}qDT��DUz�DU�qDV� DWDW��DW�qDXz�DX�qDY� DZ�DZ� DZ�qD[� D\�D\� D\�qD]z�D^  D^�D_�D_��D`�D`}qD`��Da� Da�qDb��Dc�Dc��Dd�Dd� De�De��Df�Df��Df�qDg}qDh�Dh�Di  Di� Dj  Dj��Dj�qDk}qDk�qDl}qDl�qDm� Dn�Dn� Do�Do��Do�qDpz�Dp��Dqz�Dq�qDr��Ds�Ds��Dt�Dt}qDt�qDu��Dv  Dvz�Dv�qDw��Dx  Dx}qDx�qDy}qDz�Dz� D{  D{� D|  D|�D}  D}z�D}�qD~� D�D� D�HD�B�D��HD���D�  D�AHD�~�D���D�  D�AHD��HD�D�HD�AHD��HD���D���D�@ D�~�D��qD��qD�>�D�~�D�� D�  D�B�D���D���D��qD�>�D�~�D���D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�>�D�~�D�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�~�D���D�  D�@ D��HD��HD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?L��?�z�?��
?��@   @
=q@�R@333@B�\@Tz�@n{@}p�@��@���@�(�@��
@���@�Q�@\@�=q@�33@�  @�=q@��@��HA33A
=A�AG�A
=A�HA   A&ffA*�HA.�RA5�A:=qA>�RAB�\AHQ�AN{AQ�AUA\��A`��Adz�Aj=qAo\)As�
Aw�A|��A���A��\A��A��A��A��
A��RA���A��\A��A�  A��A��
A��RA�G�A��HA��A�  A��A��
A�
=A���A��A�A���A��HA��A�Q�A�33A��AǮA��HA�p�A�\)Aҏ\A�p�A�\)Aٙ�A���A�\)A�G�A�z�A�
=A��A�33A�ffA�Q�A�\A�{A�\)A��\A��A��B ��B=qB�Bz�B�B\)Bz�B	��B33B��B��B
=Bz�B�B�HBz�B�B
=B(�B��B
=B(�BG�B�HB z�B!p�B"�\B$(�B%��B&�\B'�B)�B*�\B+�B,z�B-�B/\)B0z�B1p�B333B4z�B5p�B6�HB8Q�B9p�B:�\B<  B=p�B>=qB?�B@��BBffBC33BD(�BEBG
=BH  BH��BJffBK�
BL��BM�BO\)BPz�BQp�BRffBS�
BU�BU�BV�HBXQ�BYp�BZ=qB[33B\z�B]p�B^=qB_33B`��BaBb�\Bc�Bd��BeBf�\Bg�Bh��Bj{Bj�RBk�Bl��Bn=qBo
=Bp  BqG�Br�\Bs�BtQ�Bu��Bv�HBw�
Bx��By��Bz�RB|(�B}G�B~=qB
=B�{B���B�G�B��B�(�B���B�\)B��
B�Q�B�
=B��B��
B�z�B�33B��B�{B���B�G�B��
B�=qB���B��B��B��\B�G�B�B�=qB���B�p�B��
B�Q�B�
=B��B�{B���B�\)B��
B�=qB��HB��B�  B�z�B���B��B�=qB���B��B��
B�=qB��RB�\)B��B�ffB���B�p�B�{B�z�B��HB�p�B�(�B���B�
=B���B�(�B���B���B��B�(�B��\B�
=B��B�Q�B��RB�G�B�  B��\B��HB���B�=qB��\B�G�B��
B�Q�B��RB�\)B�  B�ffB��HB��B��
B�Q�B�
=B��B�  B��\B�G�B��
B�=qB��HB���B�{B��\B�33B��B�z�B���B��B�Q�B���B�p�B�(�B��\B�33B�  B�z�B�
=B�B�z�B���B��B�Q�B�
=BÅB�{BĸRBř�B�=qB���B�\)B�=qB���B�\)B�  Bʣ�B�p�B�  B̏\B�G�B�{BΏ\B��B��
BУ�B�33B�Bҏ\B�G�B��
B�Q�B��B��
B�Q�B��HBׅB�Q�B���BمB�  BڸRB�p�B��B�z�B�G�B��
B�ffB���B߮B�z�B��B�B�(�B���B㙚B�Q�B��HB�B�{B��HB癚B�=qB���B�\)B�  B��HB�B�  B��B�\)B�{B���B�G�B��
B��\B�\)B��B�\B�33B�B�z�B�G�B�  B��\B��B�B�z�B�33B��B�Q�B��HB���B�Q�B���B���B�(�B��RB�\)C   C ffC �RC
=CG�C�\C�HC33C��C�C(�Cp�CC(�Cz�C�
C�Cp�C�RC  C\)C�RC  CG�C�C�HC33Cz�CC	  C	G�C	�C	�
C
(�C
p�C
��C{C\)C��C�HC33C�C�HC33Cz�C�C
=C\)C�C��CQ�C��C��C=qC�\C�HC(�CffCC
=CQ�C��C�C=qC��C��CG�C��C�C33C�\C�HC=qC�\C�HC(�CffCC
=Cp�CC�CffC�RC  CG�C�\C�CQ�C��C  CQ�C��C�C=qC��C�CG�C��C�CG�C��C   C \)C �RC!
=C!\)C!��C!�C"=qC"�C"�HC#=qC#��C#��C$G�C$��C$��C%=qC%�\C%�HC&G�C&�C'
=C'\)C'�RC(
=C(Q�C(��C)  C)Q�C)��C)��C*\)C*�C+
=C+\)C+�RC,{C,ffC,C-�C-p�C-C.�C.p�C.C/{C/ffC/�RC0
=C0ffC0C1�C1z�C1�
C233C2�\C2�HC3(�C3�C3�
C433C4�C4�
C5(�C5�C5��C6{C6ffC6�C7  C7Q�C7�\C7�HC8�C8ffC8��C8�
C9{C9G�C9p�C9��C9�HC:�C:G�C:z�C:�C:�
C;  C;�C;=qC;ffC;�\C;�RC;�C<�C<G�C<z�C<��C<�C<�HC=
=C=33C=\)C=�C=�RC=��C>�C>G�C>p�C>�C>�C>��C?  C?(�C?ffC?��C?C?�C@
=C@(�C@G�C@z�C@�RC@�HCA{CA=qCAffCAz�CA��CA�
CB  CB�CBG�CBz�CB��CB�
CC  CC33CCQ�CC�CC�RCC�HCD{CDG�CDffCD�\CD�RCD�HCE
=CE33CEffCE�CE�RCE��CF(�CF\)CFz�CF��CF��CF��CG�CGG�CGz�CG��CG��CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@�@@  @}p�@��R@�G�@�G�A   A  A\)A+�A@  A_\)A�  A�  A��A�\)A�Q�AУ�A�Q�A�Q�B   B�
B(�B  B�
B((�B0(�B7�
B@  BH  BP  BX(�B`  Bh  Bp(�Bx  B�  B�{B�  B��B��B�{B�  B��
B��B��B��B�  B�{B�{B��
B��B�{B�{B�{B�  B��B�  B��B��B�  B�{B�  B��B�  B��
B�  B�{B��C�C��C��C�C	�C�C  C
=C��C
=C��C�C  C
=C  C   C!��C$
=C%�C'�C)�C+��C.
=C0  C2  C3��C6  C8{C:  C;��C>
=C@
=CB  CD
=CF  CG��CJ  CL  CN  CP
=CR  CT  CV  CW��CY�C\  C^  C`
=Ca��Cd
=Cf  Cg��Ci��Cl  Cn  Cp
=Cr
=Cs��Cv
=Cx
=Cz
=C|  C~
=C��C���C�  C���C���C�C�  C���C�C�  C�  C�C�  C���C�  C�C�  C�C�C�  C���C�  C�C�
=C�C�  C�  C�C�C�  C���C�  C�  C���C���C���C�  C���C���C���C�  C�  C���C�  C���C���C���C�  C�  C���C���C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�  C���C���C�C�C�  C�  C���C�  C���C���C�  C�  C�  C���C�  C�
=C�C�  C�C�C���C�  C�C���C���C�  C�C�C�C���C�  C�  C���C�  C�  C�  C�C���C�  C�  C�C�C���C���C�  C�
=C�C���C���C���C���C���C�  C�  C�  C�  C�C�C�  C���C�  C�  C���C���C�D �D }qD  D}qD�qD}qD�qDz�D��D}qD�D� D�qD��DD��D  D� D	�D	� D
�D
� D
�qD}qD�qD� D�qD� D�qD}qD�qD��DD�D�D� D�D��D�D� D  D��D�D��D�D}qD�qD}qD�qD� D  D}qD  D}qD�qD��D  D}qD�qD� D  D� D�D�D   D ��D!�D!��D"D"�D#�D#� D$  D$� D%  D%� D%�qD&� D'  D'� D(  D(}qD(�qD)� D)�qD*z�D*�qD+z�D+��D,z�D,�qD-� D.�D.� D/  D/��D/�qD0z�D1  D1��D2  D2��D3  D3� D4  D4}qD4�qD5� D6�D6�D7  D7� D8D8�D9�D9�D:D:� D;  D;z�D;��D<}qD=�D=��D=�qD>}qD?  D?� D@  D@}qD@�qDA�DB�DB}qDB��DC��DD  DD��DE�DE� DF�DF� DF�qDG� DH  DH� DI  DI��DJ  DJ}qDJ�qDK}qDK�qDL� DL�qDMz�DM��DN}qDO  DO}qDO�qDPz�DQ  DQ��DR�DR�DSDS��DT�DT}qDT��DUz�DU�qDV� DWDW��DW�qDXz�DX�qDY� DZ�DZ� DZ�qD[� D\�D\� D\�qD]z�D^  D^�D_�D_��D`�D`}qD`��Da� Da�qDb��Dc�Dc��Dd�Dd� De�De��Df�Df��Df�qDg}qDh�Dh�Di  Di� Dj  Dj��Dj�qDk}qDk�qDl}qDl�qDm� Dn�Dn� Do�Do��Do�qDpz�Dp��Dqz�Dq�qDr��Ds�Ds��Dt�Dt}qDt�qDu��Dv  Dvz�Dv�qDw��Dx  Dx}qDx�qDy}qDz�Dz� D{  D{� D|  D|�D}  D}z�D}�qD~� D�D� D�HD�B�D��HD���D�  D�AHD�~�D���D�  D�AHD��HD�D�HD�AHD��HD���D���D�@ D�~�D��qD��qD�>�D�~�D�� D�  D�B�D���D���D��qD�>�D�~�D���D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�>�D�~�D�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�~�D���D�  D�@ D��HD��HD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?L��?�z�?��
?��@   @
=q@�R@333@B�\@Tz�@n{@}p�@��@���@�(�@��
@���@�Q�@\@�=q@�33@�  @�=q@��@��HA33A
=A�AG�A
=A�HA   A&ffA*�HA.�RA5�A:=qA>�RAB�\AHQ�AN{AQ�AUA\��A`��Adz�Aj=qAo\)As�
Aw�A|��A���A��\A��A��A��A��
A��RA���A��\A��A�  A��A��
A��RA�G�A��HA��A�  A��A��
A�
=A���A��A�A���A��HA��A�Q�A�33A��AǮA��HA�p�A�\)Aҏ\A�p�A�\)Aٙ�A���A�\)A�G�A�z�A�
=A��A�33A�ffA�Q�A�\A�{A�\)A��\A��A��B ��B=qB�Bz�B�B\)Bz�B	��B33B��B��B
=Bz�B�B�HBz�B�B
=B(�B��B
=B(�BG�B�HB z�B!p�B"�\B$(�B%��B&�\B'�B)�B*�\B+�B,z�B-�B/\)B0z�B1p�B333B4z�B5p�B6�HB8Q�B9p�B:�\B<  B=p�B>=qB?�B@��BBffBC33BD(�BEBG
=BH  BH��BJffBK�
BL��BM�BO\)BPz�BQp�BRffBS�
BU�BU�BV�HBXQ�BYp�BZ=qB[33B\z�B]p�B^=qB_33B`��BaBb�\Bc�Bd��BeBf�\Bg�Bh��Bj{Bj�RBk�Bl��Bn=qBo
=Bp  BqG�Br�\Bs�BtQ�Bu��Bv�HBw�
Bx��By��Bz�RB|(�B}G�B~=qB
=B�{B���B�G�B��B�(�B���B�\)B��
B�Q�B�
=B��B��
B�z�B�33B��B�{B���B�G�B��
B�=qB���B��B��B��\B�G�B�B�=qB���B�p�B��
B�Q�B�
=B��B�{B���B�\)B��
B�=qB��HB��B�  B�z�B���B��B�=qB���B��B��
B�=qB��RB�\)B��B�ffB���B�p�B�{B�z�B��HB�p�B�(�B���B�
=B���B�(�B���B���B��B�(�B��\B�
=B��B�Q�B��RB�G�B�  B��\B��HB���B�=qB��\B�G�B��
B�Q�B��RB�\)B�  B�ffB��HB��B��
B�Q�B�
=B��B�  B��\B�G�B��
B�=qB��HB���B�{B��\B�33B��B�z�B���B��B�Q�B���B�p�B�(�B��\B�33B�  B�z�B�
=B�B�z�B���B��B�Q�B�
=BÅB�{BĸRBř�B�=qB���B�\)B�=qB���B�\)B�  Bʣ�B�p�B�  B̏\B�G�B�{BΏ\B��B��
BУ�B�33B�Bҏ\B�G�B��
B�Q�B��B��
B�Q�B��HBׅB�Q�B���BمB�  BڸRB�p�B��B�z�B�G�B��
B�ffB���B߮B�z�B��B�B�(�B���B㙚B�Q�B��HB�B�{B��HB癚B�=qB���B�\)B�  B��HB�B�  B��B�\)B�{B���B�G�B��
B��\B�\)B��B�\B�33B�B�z�B�G�B�  B��\B��B�B�z�B�33B��B�Q�B��HB���B�Q�B���B���B�(�B��RB�\)C   C ffC �RC
=CG�C�\C�HC33C��C�C(�Cp�CC(�Cz�C�
C�Cp�C�RC  C\)C�RC  CG�C�C�HC33Cz�CC	  C	G�C	�C	�
C
(�C
p�C
��C{C\)C��C�HC33C�C�HC33Cz�C�C
=C\)C�C��CQ�C��C��C=qC�\C�HC(�CffCC
=CQ�C��C�C=qC��C��CG�C��C�C33C�\C�HC=qC�\C�HC(�CffCC
=Cp�CC�CffC�RC  CG�C�\C�CQ�C��C  CQ�C��C�C=qC��C�CG�C��C�CG�C��C   C \)C �RC!
=C!\)C!��C!�C"=qC"�C"�HC#=qC#��C#��C$G�C$��C$��C%=qC%�\C%�HC&G�C&�C'
=C'\)C'�RC(
=C(Q�C(��C)  C)Q�C)��C)��C*\)C*�C+
=C+\)C+�RC,{C,ffC,C-�C-p�C-C.�C.p�C.C/{C/ffC/�RC0
=C0ffC0C1�C1z�C1�
C233C2�\C2�HC3(�C3�C3�
C433C4�C4�
C5(�C5�C5��C6{C6ffC6�C7  C7Q�C7�\C7�HC8�C8ffC8��C8�
C9{C9G�C9p�C9��C9�HC:�C:G�C:z�C:�C:�
C;  C;�C;=qC;ffC;�\C;�RC;�C<�C<G�C<z�C<��C<�C<�HC=
=C=33C=\)C=�C=�RC=��C>�C>G�C>p�C>�C>�C>��C?  C?(�C?ffC?��C?C?�C@
=C@(�C@G�C@z�C@�RC@�HCA{CA=qCAffCAz�CA��CA�
CB  CB�CBG�CBz�CB��CB�
CC  CC33CCQ�CC�CC�RCC�HCD{CDG�CDffCD�\CD�RCD�HCE
=CE33CEffCE�CE�RCE��CF(�CF\)CFz�CF��CF��CF��CG�CGG�CGz�CG��CG��CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЍPAЕ�AЗ�AЕ�AЕ�AЙ�AЙ�AЙ�AЙ�AЙ�AЛ�AЛ�AЛ�AЛ�AН�AЗ�AЙ�AЛ�AН�AН�AН�AН�AН�AЛ�AН�AЕ�AЙ�AЗ�AГuAЕ�AЍPAЃA�`BA�oA��A��A���Aʙ�A�
=A�5?A���A�"�AƲ-AŲ-Aĝ�A�C�A�p�A���A���A���A�x�A���A��A���A��A��yA���A�"�A���A���A���A�{A��A���A��A���A��9A�9XA���A���A�-A��DA�ĜA�l�A�1A�p�A�p�A���A���A�?}A�VA�l�A�"�A���A��/A���A�$�A�ƨA���A��uA��FA��RA�z�A��A{��Ax�Av�RAu?}AsƨAq��Am�Aj��Ah�HAe?}Ac|�Ab��A_�#A]XA\ĜA\E�AZ��AWx�AT��AQp�AOt�ANZAKVAH��AG?}AE�AD�DAC�#AA�
A@��A?��A??}A=/A:��A9��A9"�A7%A4bA2�RA2�A1t�A0JA/A-t�A+`BA*r�A({A$JA"��A!O�Al�A��AbNA�A��A`BA��A�At�Av�Ap�A�A�;A�!A=qAv�AdZA33AĜA-Ap�A�AK�A+A
=A
�/A
E�A	O�A�A+A9XA+A�9AA�A�-A7LA��AbNA�#A�@��;@�?}@�S�@��!@�J@�`B@��/@��@��y@�p�@�b@���@�1'@�dZ@���@�ff@�@��@���@�/@�Q�@��;@�7L@�j@㝲@���@�&�@�Ĝ@��@�33@��H@ޏ\@�ff@ݙ�@ܴ9@���@�&�@׮@ְ!@�Ĝ@Ӆ@�
=@Ұ!@�V@���@ѩ�@�&�@���@��@���@�|�@��y@�hs@̓u@ˮ@ʧ�@�V@�`B@��@�;d@���@�V@Ĭ@î@�o@�ȴ@�=q@���@���@�7L@� �@�ƨ@���@��@��!@��\@�V@�@��@�V@��@�|�@�S�@�+@�ȴ@�^5@�=q@�E�@��R@��@�S�@��P@�1@�bN@��@���@�;d@�ƨ@� �@�r�@�bN@�j@�bN@�  @���@�t�@��R@�J@�V@�b@�@���@���@��@�ƨ@�|�@�
=@��!@���@��h@��#@� �@��@�I�@��`@���@���@��u@�(�@�b@��m@��w@�o@���@�M�@�{@�@��7@�x�@�G�@�V@���@�Ĝ@��D@�I�@�b@��;@��w@���@�E�@�x�@��@��@�(�@�b@�1@�  @��m@��F@�S�@�+@���@���@��+@�^5@�M�@��@���@�hs@��`@���@�O�@�/@��/@��9@���@�r�@��;@�33@���@��y@��!@�V@��@�p�@�X@�?}@�G�@�V@���@�r�@���@��m@��m@��m@��m@��@��@�S�@���@�n�@���@�hs@�%@��/@�Ĝ@��u@�1'@��w@�+@��y@���@���@�~�@�v�@�^5@���@��#@��^@��h@���@��`@��/@��9@�I�@��@��@�l�@�o@���@��!@���@�^5@�M�@�$�@���@�`B@��u@�I�@�1'@� �@� �@�1@�  @��@��m@��
@�ƨ@�dZ@��@��@���@��+@�E�@�$�@��@�x�@�V@��9@�A�@�  @��w@��@�;d@��H@���@�v�@�V@�@��7@�%@��/@��j@��@�1'@��w@�l�@�33@�
=@�ȴ@�n�@�J@��T@��^@�hs@��@���@���@��j@���@�Q�@�  @��@��;@��
@��F@�\)@�;d@�
=@��!@��@��@���@�x�@�O�@�7L@�/@�&�@��@��@���@�I�@���@��;@�ƨ@���@��P@��P@��@�|�@�|�@�|�@�l�@�dZ@�C�@�o@��@��@��y@���@�~�@�v�@�5?@��@���@�O�@���@��@�bN@�1@�  @���@��@��@��w@��P@��P@�l�@�+@�@��@��@��@�5?@�@��@��#@�@��-@���@��7@�&�@��/@��j@��u@�Q�@��@~�+@~E�@~E�@~E�@~E�@~@}V@|Z@{"�@z�H@zn�@z�@y��@y7L@x�`@x�9@xr�@xQ�@w�P@w�@v�@v�R@vff@u@up�@t�@t�@t�D@tj@t(�@s�
@s��@sC�@s@r��@r�\@rM�@rJ@q�#@q�7@qG�@p��@p��@p�9@pA�@o�w@n�y@nff@nE�@n{@m�T@mp�@l�j@l(�@k��@k�F@kt�@k33@koG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЇ+AЅAБhAЏ\AГuAЗ�AЕ�AЙ�AЙ�AЗ�AЗ�AЙ�AЗ�AГuAЕ�AЙ�AЕ�AГuAЛ�AЛ�AЗ�AЗ�AЙ�AЛ�AЗ�AЗ�AН�AЙ�AЕ�AЛ�AЛ�AЗ�AЗ�AН�AЛ�AЙ�AН�AН�AЛ�AЗ�AЛ�AН�AЙ�AЗ�AН�AН�AЙ�AН�AП�AЛ�AЗ�AН�AН�AЙ�AЙ�AС�AН�AН�AН�AН�AЙ�AН�AП�AН�AЕ�AЗ�AЙ�AЕ�AЕ�AЛ�AЗ�AГuAЙ�AП�AЛ�AЙ�AП�AЛ�AЙ�AН�AН�AЙ�AЛ�AП�AН�AЙ�AП�AП�AЛ�AЛ�AП�AН�AЛ�AП�AН�AЙ�AН�AП�AЛ�AН�AС�AЛ�AЛ�AП�AН�AЛ�AН�AП�AЙ�AЛ�AП�AЙ�AЙ�AП�AС�AЛ�AЙ�AН�AН�AЙ�AН�AП�AЛ�AЙ�AН�AУ�AН�AБhAГuAГuAГuAГuAЗ�AЙ�AЗ�AГuAЙ�AН�AЛ�AЕ�AЙ�AЙ�AЗ�AЕ�AЙ�AЙ�AЕ�AЕ�AЕ�AБhAЏ\AГuAЕ�AГuAЕ�AЗ�AЙ�AЗ�AГuAЕ�AГuAЍPAЋDAЇ+AЋDAЋDAЏ\AЏ\AЅA�z�A�l�A�t�A�l�A�`BA�bNA�dZA�bNA�ZA�Q�A�Q�A�M�A�C�A�E�A�1A��;A�AϬAϗ�A�O�A�$�A�A�ƨAΙ�A΅A�C�A̾wA�jA�Q�A�1A�A˝�A�\)A�A�A�&�A�{A���A��`A���A���Aʺ^AʶFAʮAʡ�Aʗ�AʍPAʅA�x�A�jA�^5A�?}A�{A��A���A�Aɲ-Aɧ�Aɡ�A�VA��A�  A��;Aș�A�ffA�=qA���Aǝ�A�x�A�hsA�XA�C�A�9XA�33A�(�A�{A�
=A���A��;A���A���AƸRAƮAƥ�Aƛ�A�~�A�bNA�$�AŸRA�x�A�jA�I�A�VA��A�AĬAě�A�~�A�t�A�r�A�n�A�bNA�VA�S�A�Q�A�I�A� �A���A��
AîA×�AÓuA�|�A�M�A��A���A+A�~�A�33A��A�5?A���A���A�^5A��A���A���A���A�z�A�bNA�K�A�&�A��A� �A��A�{A�JA�9XA�bA���A�S�A���A��mA���A�l�A�G�A�JA���A��9A�p�A�oA��`A��RA���A���A��+A�l�A�K�A�bA���A�K�A�1A��A���A�ZA�+A��/A�C�A��\A�A�A�VA�%A���A��A��A��A��
A���A��jA��RA��A���A�|�A�n�A�E�A�33A�oA�A���A���A���A���A���A���A���A���A��A��yA��mA���A��A���A�r�A�K�A�"�A�VA��A��7A�G�A���A���A�1A��7A�5?A��A��;A�ȴA��-A�z�A�G�A��A��A��#A��RA��DA�p�A�I�A��A���A���A���A��+A�G�A��A�A��A���A��A�$�A��A�l�A�  A���A�K�A�{A���A���A��\A�?}A��A�ƨA���A�S�A�33A�"�A��A���A���A�l�A�-A�A��HA���A�5?A��;A��RA��-A��DA�+A��uA�5?A��/A�ȴA���A�z�A�l�A�&�A��A���A���A�ĜA��jA���A��hA��7A�~�A�n�A�dZA�`BA�M�A�bA��^A�hsA�"�A��#A���A�  A��wA���A�%A�VA��A�p�A��A���A���A���A�hsA�5?A���A� �A��+A��A��;A���A�33A��A��HA���A��jA��A���A�p�A�1A�ƨA���A�C�A�VA��
A���A�r�A�VA�
=A��;A���A���A�~�A�t�A�jA�dZA�bNA�^5A�Q�A�9XA�7LA�5?A�1'A�%A��9A�n�A�I�A�A�z�A��9A�v�A�E�A�VA��#A��FA���A�p�A�XA�-A��A�ƨA���A��A�\)A�?}A� �A�A��mA��TA���A���A�1'A�O�A��A��7A�jA�?}A���A��^A�~�A�JA�ƨA���A�~�A�jA�XA�O�A� �A���A��FA�`BA�"�A���A�7LA��\A�jA�Q�A�O�A�M�A�A�A��A�`BA�ƨA���A��hA�jA�O�A��A�A~1A}�^A}oA{�A{+Az�9AzM�Ay�mAyl�Ax�AxQ�Aw�Aw/Aw%Av�HAv�jAv��Av�DAv�DAv�DAv^5Av9XAu��AuC�At��At=qAt(�At�AtbAtAs�As�As��As�As��Ast�AsK�As&�Ar�Ar�RAr�ArM�Aq��Aq��AqK�Aq&�Ap�Ap�uAp5?Ap�Ao�Ao��Ao+An��An��An�\An  Am�-Amp�Al��AlVAl �Ak�Ak�;AkƨAkp�AkAj�jAjM�Aj=qAj�Ai�
Ai�wAi�wAi��Ai��Ai�Ail�Ai`BAiG�Ai"�Ah�yAh�AhjAhA�Ag�;Ag%Afz�Af  Ae�wAex�AeC�Ae;dAe7LAeoAd��AdffAd5?Ad�Ad  Ac��Ac��Act�Ac\)AcXAcG�AcG�AcG�Ac?}Ac33Ac7LAc33Ac33Ac+Ac&�Ac"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЍPAЕ�AЗ�AЕ�AЕ�AЙ�AЙ�AЙ�AЙ�AЙ�AЛ�AЛ�AЛ�AЛ�AН�AЗ�AЙ�AЛ�AН�AН�AН�AН�AН�AЛ�AН�AЕ�AЙ�AЗ�AГuAЕ�AЍPAЃA�`BA�oA��A��A���Aʙ�A�
=A�5?A���A�"�AƲ-AŲ-Aĝ�A�C�A�p�A���A���A���A�x�A���A��A���A��A��yA���A�"�A���A���A���A�{A��A���A��A���A��9A�9XA���A���A�-A��DA�ĜA�l�A�1A�p�A�p�A���A���A�?}A�VA�l�A�"�A���A��/A���A�$�A�ƨA���A��uA��FA��RA�z�A��A{��Ax�Av�RAu?}AsƨAq��Am�Aj��Ah�HAe?}Ac|�Ab��A_�#A]XA\ĜA\E�AZ��AWx�AT��AQp�AOt�ANZAKVAH��AG?}AE�AD�DAC�#AA�
A@��A?��A??}A=/A:��A9��A9"�A7%A4bA2�RA2�A1t�A0JA/A-t�A+`BA*r�A({A$JA"��A!O�Al�A��AbNA�A��A`BA��A�At�Av�Ap�A�A�;A�!A=qAv�AdZA33AĜA-Ap�A�AK�A+A
=A
�/A
E�A	O�A�A+A9XA+A�9AA�A�-A7LA��AbNA�#A�@��;@�?}@�S�@��!@�J@�`B@��/@��@��y@�p�@�b@���@�1'@�dZ@���@�ff@�@��@���@�/@�Q�@��;@�7L@�j@㝲@���@�&�@�Ĝ@��@�33@��H@ޏ\@�ff@ݙ�@ܴ9@���@�&�@׮@ְ!@�Ĝ@Ӆ@�
=@Ұ!@�V@���@ѩ�@�&�@���@��@���@�|�@��y@�hs@̓u@ˮ@ʧ�@�V@�`B@��@�;d@���@�V@Ĭ@î@�o@�ȴ@�=q@���@���@�7L@� �@�ƨ@���@��@��!@��\@�V@�@��@�V@��@�|�@�S�@�+@�ȴ@�^5@�=q@�E�@��R@��@�S�@��P@�1@�bN@��@���@�;d@�ƨ@� �@�r�@�bN@�j@�bN@�  @���@�t�@��R@�J@�V@�b@�@���@���@��@�ƨ@�|�@�
=@��!@���@��h@��#@� �@��@�I�@��`@���@���@��u@�(�@�b@��m@��w@�o@���@�M�@�{@�@��7@�x�@�G�@�V@���@�Ĝ@��D@�I�@�b@��;@��w@���@�E�@�x�@��@��@�(�@�b@�1@�  @��m@��F@�S�@�+@���@���@��+@�^5@�M�@��@���@�hs@��`@���@�O�@�/@��/@��9@���@�r�@��;@�33@���@��y@��!@�V@��@�p�@�X@�?}@�G�@�V@���@�r�@���@��m@��m@��m@��m@��@��@�S�@���@�n�@���@�hs@�%@��/@�Ĝ@��u@�1'@��w@�+@��y@���@���@�~�@�v�@�^5@���@��#@��^@��h@���@��`@��/@��9@�I�@��@��@�l�@�o@���@��!@���@�^5@�M�@�$�@���@�`B@��u@�I�@�1'@� �@� �@�1@�  @��@��m@��
@�ƨ@�dZ@��@��@���@��+@�E�@�$�@��@�x�@�V@��9@�A�@�  @��w@��@�;d@��H@���@�v�@�V@�@��7@�%@��/@��j@��@�1'@��w@�l�@�33@�
=@�ȴ@�n�@�J@��T@��^@�hs@��@���@���@��j@���@�Q�@�  @��@��;@��
@��F@�\)@�;d@�
=@��!@��@��@���@�x�@�O�@�7L@�/@�&�@��@��@���@�I�@���@��;@�ƨ@���@��P@��P@��@�|�@�|�@�|�@�l�@�dZ@�C�@�o@��@��@��y@���@�~�@�v�@�5?@��@���@�O�@���@��@�bN@�1@�  @���@��@��@��w@��P@��P@�l�@�+@�@��@��@��@�5?@�@��@��#@�@��-@���@��7@�&�@��/@��j@��u@�Q�@��@~�+@~E�@~E�@~E�@~E�@~@}V@|Z@{"�@z�H@zn�@z�@y��@y7L@x�`@x�9@xr�@xQ�@w�P@w�@v�@v�R@vff@u@up�@t�@t�@t�D@tj@t(�@s�
@s��@sC�@s@r��@r�\@rM�@rJ@q�#@q�7@qG�@p��@p��@p�9@pA�@o�w@n�y@nff@nE�@n{@m�T@mp�@l�j@l(�@k��@k�F@kt�@k33@koG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЇ+AЅAБhAЏ\AГuAЗ�AЕ�AЙ�AЙ�AЗ�AЗ�AЙ�AЗ�AГuAЕ�AЙ�AЕ�AГuAЛ�AЛ�AЗ�AЗ�AЙ�AЛ�AЗ�AЗ�AН�AЙ�AЕ�AЛ�AЛ�AЗ�AЗ�AН�AЛ�AЙ�AН�AН�AЛ�AЗ�AЛ�AН�AЙ�AЗ�AН�AН�AЙ�AН�AП�AЛ�AЗ�AН�AН�AЙ�AЙ�AС�AН�AН�AН�AН�AЙ�AН�AП�AН�AЕ�AЗ�AЙ�AЕ�AЕ�AЛ�AЗ�AГuAЙ�AП�AЛ�AЙ�AП�AЛ�AЙ�AН�AН�AЙ�AЛ�AП�AН�AЙ�AП�AП�AЛ�AЛ�AП�AН�AЛ�AП�AН�AЙ�AН�AП�AЛ�AН�AС�AЛ�AЛ�AП�AН�AЛ�AН�AП�AЙ�AЛ�AП�AЙ�AЙ�AП�AС�AЛ�AЙ�AН�AН�AЙ�AН�AП�AЛ�AЙ�AН�AУ�AН�AБhAГuAГuAГuAГuAЗ�AЙ�AЗ�AГuAЙ�AН�AЛ�AЕ�AЙ�AЙ�AЗ�AЕ�AЙ�AЙ�AЕ�AЕ�AЕ�AБhAЏ\AГuAЕ�AГuAЕ�AЗ�AЙ�AЗ�AГuAЕ�AГuAЍPAЋDAЇ+AЋDAЋDAЏ\AЏ\AЅA�z�A�l�A�t�A�l�A�`BA�bNA�dZA�bNA�ZA�Q�A�Q�A�M�A�C�A�E�A�1A��;A�AϬAϗ�A�O�A�$�A�A�ƨAΙ�A΅A�C�A̾wA�jA�Q�A�1A�A˝�A�\)A�A�A�&�A�{A���A��`A���A���Aʺ^AʶFAʮAʡ�Aʗ�AʍPAʅA�x�A�jA�^5A�?}A�{A��A���A�Aɲ-Aɧ�Aɡ�A�VA��A�  A��;Aș�A�ffA�=qA���Aǝ�A�x�A�hsA�XA�C�A�9XA�33A�(�A�{A�
=A���A��;A���A���AƸRAƮAƥ�Aƛ�A�~�A�bNA�$�AŸRA�x�A�jA�I�A�VA��A�AĬAě�A�~�A�t�A�r�A�n�A�bNA�VA�S�A�Q�A�I�A� �A���A��
AîA×�AÓuA�|�A�M�A��A���A+A�~�A�33A��A�5?A���A���A�^5A��A���A���A���A�z�A�bNA�K�A�&�A��A� �A��A�{A�JA�9XA�bA���A�S�A���A��mA���A�l�A�G�A�JA���A��9A�p�A�oA��`A��RA���A���A��+A�l�A�K�A�bA���A�K�A�1A��A���A�ZA�+A��/A�C�A��\A�A�A�VA�%A���A��A��A��A��
A���A��jA��RA��A���A�|�A�n�A�E�A�33A�oA�A���A���A���A���A���A���A���A���A��A��yA��mA���A��A���A�r�A�K�A�"�A�VA��A��7A�G�A���A���A�1A��7A�5?A��A��;A�ȴA��-A�z�A�G�A��A��A��#A��RA��DA�p�A�I�A��A���A���A���A��+A�G�A��A�A��A���A��A�$�A��A�l�A�  A���A�K�A�{A���A���A��\A�?}A��A�ƨA���A�S�A�33A�"�A��A���A���A�l�A�-A�A��HA���A�5?A��;A��RA��-A��DA�+A��uA�5?A��/A�ȴA���A�z�A�l�A�&�A��A���A���A�ĜA��jA���A��hA��7A�~�A�n�A�dZA�`BA�M�A�bA��^A�hsA�"�A��#A���A�  A��wA���A�%A�VA��A�p�A��A���A���A���A�hsA�5?A���A� �A��+A��A��;A���A�33A��A��HA���A��jA��A���A�p�A�1A�ƨA���A�C�A�VA��
A���A�r�A�VA�
=A��;A���A���A�~�A�t�A�jA�dZA�bNA�^5A�Q�A�9XA�7LA�5?A�1'A�%A��9A�n�A�I�A�A�z�A��9A�v�A�E�A�VA��#A��FA���A�p�A�XA�-A��A�ƨA���A��A�\)A�?}A� �A�A��mA��TA���A���A�1'A�O�A��A��7A�jA�?}A���A��^A�~�A�JA�ƨA���A�~�A�jA�XA�O�A� �A���A��FA�`BA�"�A���A�7LA��\A�jA�Q�A�O�A�M�A�A�A��A�`BA�ƨA���A��hA�jA�O�A��A�A~1A}�^A}oA{�A{+Az�9AzM�Ay�mAyl�Ax�AxQ�Aw�Aw/Aw%Av�HAv�jAv��Av�DAv�DAv�DAv^5Av9XAu��AuC�At��At=qAt(�At�AtbAtAs�As�As��As�As��Ast�AsK�As&�Ar�Ar�RAr�ArM�Aq��Aq��AqK�Aq&�Ap�Ap�uAp5?Ap�Ao�Ao��Ao+An��An��An�\An  Am�-Amp�Al��AlVAl �Ak�Ak�;AkƨAkp�AkAj�jAjM�Aj=qAj�Ai�
Ai�wAi�wAi��Ai��Ai�Ail�Ai`BAiG�Ai"�Ah�yAh�AhjAhA�Ag�;Ag%Afz�Af  Ae�wAex�AeC�Ae;dAe7LAeoAd��AdffAd5?Ad�Ad  Ac��Ac��Act�Ac\)AcXAcG�AcG�AcG�Ac?}Ac33Ac7LAc33Ac33Ac+Ac&�Ac"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�B�B��B�B��B�B�SB�SB�SB�SB��B��B��B��B��B��B��B��B��B��B��B�B��B��B�MB�{B��B��B��B�B��B~�B��B��B�B �B1B�B8�Be�Bs�Bz�B�JB��B�JB�"B�FB~�B�ABy>Bj�BiyBiyBc�BQBOBN<BJ#BH�BG�BLdB6zB/B'�B �B7B�B�B�;B�`B�B�OB�tB�1Bv+BHB&�B�B
�]B
�B
�B
�KB
�vB
�-B
��B
�@B
��B
m]B
V9B
I�B
5?B
$tB
�B	��B	�mB	ԕB	�&B	�B	�XB	��B	��B	�OB	��B	��B	�B	}�B	poB	jB	c�B	ZB	G�B	3�B	"�B	�B	
�B�JB�5B��B�BޞB�yB��B�B˒B�B��B�XB�hB�!B�wB��B��B�OB�xB�B��B��B��B��B�DB��B�oB�B�B��B}"B|�B{B{�Bz�Bx8BzxBw�BrBlWBhsBe�BdZBqBqvBp�BrGBqBp�Bn�Bj�BjBi�Bi�Bj�Bj�BjKBh
BiyBjBn�Br�BrGBsMBqBrBqvBo�Br�Bh
BdZBc Bc�Bb�BcTBc�BdZBf�Be�Ba|Bc�Bb�Bh�Bi�BiBi�Bo5Bt�BuZBw�B�;B�;B��B��B�$B��B��B�IB��B�!B�\B��B��B�XB��B��B��BɺB�dB�jB�vB��BҽB��B�mBٴBޞBߤB�B�TB�yB�QB�cB�;B�B�TB�`B�2B��B��B��B�PB��B	 iB	�B	�B	B		�B	�B	4B	�B	B	VB	�B	!�B	)�B	-CB	1[B	9$B	=�B	>�B	?�B	D�B	J�B	P}B	T�B	[WB	`�B	cTB	e�B	i�B	m�B	pB	qB	u�B	}�B	�B	�B	��B	�hB	�uB	��B	��B	�B	�'B	�4B	��B	�'B	��B	�zB	�0B	��B	��B	��B	�!B	��B	�B	�CB	�[B	�qB	�eB	��B	��B	��B	��B	�6B	�BB	�BB	�wB	�HB	�B	��B	��B	B	�[B	�aB	�-B	ÖB	ÖB	�3B	�3B	�mB	�mB	�9B	ĜB	�B	ƨB	��B	��B	�mB	ƨB	�B	ȀB	�B	ʌB	�)B	��B	͟B	�<B	ΥB	��B	��B	ϫB	бB	�TB	��B	�2B	�9B	֡B	��B	�dB	��B	�)B	�dB	�5B	�B	��B	�B	�B	��B	�B	��B	�B	�B	�B	�>B	�KB	��B	�B	�]B	��B	��B	�B	�B	�;B	��B	�B	��B	�B	�|B	�%B	��B	�%B	��B	�%B	�%B	��B	��B	��B	�+B	�+B	�`B	�`B	�2B	�rB	�rB	�>B	��B	�B	�JB	�B	�B	��B	�"B	��B	��B	��B	��B	��B	�cB
 iB
  B
B
�B
�B
�B
%B
YB
�B
YB
�B
�B
�B
�B
�B
�B
fB
1B
�B
	7B
	�B

	B

	B

rB
~B
�B
�B
VB
�B
�B
�B
�B
�B
�B
bB
�B
�B
oB
�B
oB
�B
@B
uB
B
{B
�B
�B
MB
SB
�B
�B
�B
$B
�B
�B
�B
�B
�B
�B
�B
eB
eB
eB
B
B
B
�B
kB
=B
�B
B
CB
CB
CB
CB
CB
CB
CB
�B
!B
VB
�B
�B
�B
 'B
 'B
 \B
 \B
 \B
 \B
 �B
 �B
!-B
!bB
!�B
!�B
!�B
!�B
!-B
 �B
 �B
 'B
�B
�B
VB
 'B
 \B
!-B
!-B
!�B
!�B
!�B
"hB
!�B
#nB
$@B
%B
%B
$�B
$�B
%B
&�B
&�B
&�B
&LB
&�B
&�B
&LB
&�B
'�B
(�B
(�B
(�B
(�B
(�B
*0B
*0B
*0B
)�B
)�B
)�B
*eB
*�B
+B
*�B
*�B
*�B
+B
+6B
+6B
+�B
+�B
,B
-B
-CB
-wB
-wB
-�B
.B
-�B
.B
.IB
.IB
.�B
.�B
/OB
/�B
/�B
0!B
0!B
0UB
0�B
0�B
0�B
0�B
0�B
1'B
1'B
1'B
1�B
1�B
33B
33B
33B
3�B
4nB
5�B
6B
6zB
6FB
6FB
6FB
6B
5�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�_B�SB��B�YB�B�SB��B�B�MB��B�SB�MB��B��B�SB��B��B��B�B�B��B�YB�SB�B��B�%B��B��B�YB��B�MB�YB��B��B�B�YB�MB��B�SB�%B��B�B�SB��B�B��B��B��B�B��B�%B�{B��B�%B��B�{B�B��B��B��B�YB�B��B��B��B�B��B��B�B��B��B��B�{B�B�YB�YB�B��B�SB��B��B�B�SB��B��B�YB�{B��B��B��B�B��B��B�MB�B�SB��B�B�B��B�B��B��B�GB��B�YB��B��B�+B��B��B��B�YB�B��B��B��B��B�{B��B�MB��B��B��B��B��B��B��B��B��B�B��B��B��B�{B��B��B��B�GB�YB�GB�uB��B��B�B�uB��B�MB�AB�B��B�uB��B�GB��B��B�B��B�MB�oB��B��B�B�BcB�uB�uB�B�{B�1B�;B.B�B� B~�B}VB~�B}�B�iB|PB|�B}VB{�B�\B��B��B�lB�DB��B�B�VB�tB��B�nB�B�B��BޞB��B�lB�5B��B��B��B��B�BB%BYB�BYB�B	B
	B�B�B	7B�B�B�B�B#:B%FB$�B'RB'�B&�B8�B<6B>�BB�BOBBTaBYKBd�Br|Bo�Bn/BncBqvBqABp�BsBv�Bt�Bu�B|�BzDBy�By>Bz�Bz�B{B�B��B�B�SB��B��B��B�B��B��B�B�B��B�B�B��B��B��B��B�	B�	B��B�VB�B�B�(B�=B��B��B�(B��B��B�EB�~B��B�~B��B{JB|B~�B|�B}�B�B�oB~�B~�B��B|�Bx�By�Bx�Bw2B�UB{ByrB��B��Bt�Bp�Bc�Bi�Bj�Bj�Bc�BlWBu%BjBk�Bh>BiDBh>Bg�BjKBo�Bo�Bo�BffBh>BbBjB`vBe�BtBdZB^�BW
BQNBT�BRTBOvBS�BOBBO�BQBLdBNpBOvBR BMBTaBP�BN�BN�BK)BL0BK^BJ#BI�BJ#BH�BH�BH�BH�BG�BL�BI�BFtBJ#BF�BD�BD3BHBI�BE�B^Be�BIRBN<BC�B9�B6�B6B6�B7�B8�B49B4B/OB.B0�B,=B-�B)�B*�B$�B%�B$@B+�BOB�BqB �B'RB-wB�B)�B!-B�BB	�BYB1B�B�BuB�B�B��B�ZB��B�B� B�B��B�AB�5B��B�/B��B�B��B�B�B�B�B��B��B�wB�gB�$B�LB�0B��B��B�wB�=B�B��B��B�B��B��B��B�VB��B�:B�B��B��B��B��B��By>Bz�B�hBp�Bo Bm]BYBP�Be�B?}B<�B?}BJ#B4nB:�B-�B �BVB#:B�BPB�B�B
�B
rB�B�BoBGB�B
��B
�rB
�B
�oB
�]B
��B
�2B
�`B
�B
��B
�B
�pB
ݘB
�WB
��B
��B
خB
�gB
�9B
�aB
��B
��B
�HB
�^B
ӏB
ѷB
�HB
�?B
�^B
��B
�UB
��B
�kB
��B
��B
�B
��B
��B
�7B
��B
��B
�{B
��B
��B
�B
�	B
�JB
�JB
�4B
l�B
qB
n/B
poB
o B
l�B
e�B
n/B
g�B
b�B
S�B
R B
PHB
M�B
K�B
OBB
K�B
I�B
J�B
8�B
A B
L�B
5tB
)�B
#�B
$B
 �B
"�B
3�B
9�B
�B
uB
�B
�B
�B
B
�B
�B	�JB
	�B	��B	��B	�oB	�B	�"B	�yB	�B	�"B	�HB	�#B	��B	�mB	ӏB	�2B	ԕB	�TB	�TB	�,B	ҽB	ٴB	՛B	�dB	��B	�)B	ɆB	��B	�RB	��B	�B	ǮB	ǮB	�B	��B	��B	��B	��B	�B	�HB	�wB	��B	��B	�?B	�B	�zB	�9B	�nB	�B	��B	��B	��B	�*B	�RB	�*B	��B	�hB	�@B	��B	�*B	�B	�4B	��B	�B	��B	�LB	��B	�B	��B	�qB	��B	�kB	�=B	�kB	�B	�	B	�	B	��B	��B	�~B	�!B	��B	��B	�OB	��B	�eB	��B	�B	�$B	��B	�MB	�@B	��B	�_B	��B	�_B	�uB	� B	��B	�:B	��B	��B	�"B	��B	��B	�~B	��B	��B	�B	��B	�xB	�DB	��B	��B	�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B��B�oB�oB�:B�oB�B�oB��B��B��B��B�:B�:B�:B�:B�:B�B�B��B�B�:B��B�oB�B��B��B�B� B� B~�B~\B~(B{JB.B�B��B�"B�B�B4�Ba�Bo�Bv�B��B�IB��B�rB��Bz�B~�Bu�Bg8Be�Be�B`ABMjBK^BJ�BFsBE9BC�BH�B2�B+kB#�BB�B  B�iB�B�B�^B��B��B��Br{BDgB#B
	B
��B
��B
�QB
՛B
��B
�}B
�OB
��B
��B
i�B
R�B
F
B
1�B
 �B
�B	�1B	�B	��B	�vB	�aB	��B	�FB	�B	��B	�B	��B	�kB	y�B	l�B	ffB	`B	VmB	D3B	0 B	�B	'B	+B��B�B�,B�B��B��B�HB�WB��B�aB�HB��B��B�qB��B�B�B��B��B�RB�B�.B��B�B��B~(B}�B}VB}VB}"ByrBx�Bw�Bx7Bw1Bt�Bv�BtBncBh�Bd�BbNB`�Bm]Bm�Bm(Bn�Bm]Bm(BkBg8BffBf2Be�Bg8BgBf�BdZBe�Bf�BkBo Bn�Bo�Bm]BncBm�Bk�Bo5BdZB`�B_pB_�B_;B_�B`AB`�BcBbNB]�B`AB_;Be,Bf2Be`Be�Bk�BqABq�BtB}�B}�B�B�B�tB�B�$B��B��B�qB��B�'B��B��B��B��B�B�
BȴBɺB��B�<B�B�HBҽB�B��B��B�cBߤB��B�B�B�B�WB�B�B�B��B��B�MB��B�DB��B	 �B	:B	oB	%B	IB	�B	�B	_B	�B	B	�B	&LB	)�B	-�B	5tB	:)B	:�B	<B	@�B	F�B	L�B	QNB	W�B	\�B	_�B	bNB	f2B	jB	lWB	m]B	rGB	zDB	�oB	�_B	�IB	��B	��B	�FB	�B	�_B	�wB	��B	�IB	�wB	�B	��B	��B	��B	�0B	�B	�qB	�BB	�kB	��B	��B	��B	��B	�BB	�B	��B	��B	��B	��B	��B	��B	��B	�jB	�BB	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�aB	��B	�,B	�&B	��B	��B	�gB	��B	�mB	��B	�yB	�B	��B	ʌB	��B	�)B	�)B	��B	�B	ΤB	�BB	тB	҉B	��B	�EB	ٴB	�KB	�yB	ٴB	څB	�QB	�#B	�]B	�]B	�/B	��B	�AB	��B	��B	��B	�B	�B	�8B	��B	�B	�JB	�JB	��B	��B	�B	�"B	��B	�.B	��B	��B	�uB	�AB	�uB	�AB	�uB	�uB	��B	�B	�GB	�{B	�{B	�B	�B	�B	��B	��B	��B	��B	�`B	��B	��B	�lB	�B	�rB	�DB	��B	��B	�B	��B	��B	��B	�PB	�VB	�(B
  B
B
uB
�B
�B
�B
B
B
GB
GB
GB
B
�B
�B
�B
�B
�B
YB
YB
�B
�B
	B

	B

�B
B
B
B
CB
B
�B
�B
�B
B
�B
'B
�B
'B
�B
�B
bB
�B
�B
4B
�B
�B
�B
�B
@B
tB
�B
�B
B
B
FB
B
LB
�B
�B
�B
RB
RB
RB
B
�B
�B
$B
_B
�B
�B
�B
�B
�B
�B
�B
�B
qB
�B
�B
B
CB
wB
wB
�B
�B
�B
�B
B
B
}B
�B
�B
B
OB
�B
}B
IB
IB
wB
B
=B
�B
wB
�B
}B
}B
OB
OB
OB
�B
�B
�B
 �B
!bB
!bB
!-B
 �B
!bB
#B
"�B
"�B
"�B
"�B
"�B
"�B
#B
$@B
$�B
$�B
$�B
$�B
$�B
&�B
&�B
&�B
&LB
&LB
&LB
&�B
&�B
'RB
'B
'B
'B
'RB
'�B
'�B
'�B
'�B
(XB
)^B
)�B
)�B
)�B
)�B
*dB
*0B
*dB
*�B
*�B
+B
+6B
+�B
+�B
,B
,qB
,qB
,�B
,�B
-B
-B
-BB
-BB
-wB
-wB
-wB
-�B
.IB
/�B
/�B
/�B
0 B
0�B
1�B
2aB
2�B
2�B
2�B
2�B
2aB
2-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�B��B�oB��B�B�oB��B��B��B��B�B�@B��B�4B�B�B�iB�iB�@B��B��B�iB�B�uB�4B�B��B��B��B��B�B�4B�oB��B��B�4B��B�uB�:B�iB��B�@B�iB�:B�@B�:B�iB�:B�uB�B�B�uB�@B�B�oB�@B��B�4B��B�iB�4B�:B�B�iB�4B�:B�oB� B�B��B�BbB��B��BbB��B��B� B�4B�oB��B�4B�B��B�B�4B�B�B�iB�B�@B��B�iB��B��B�iB�oB�@B�iB�B�:B�B�:B��B��B�4B�{B�@B~�B�@B��B�iB��B��B�B� B�B�@B��B��B�:B��B�4B� B�@B��B� B.B�iB�4B� B~�B�B�:B.B.B�B��B�B~�B� B��BbB~�B��B��B~�BbB��B~�B|�B�B�4B.B~\B~�B��B}�B~(B.B|B|B{�B~�B~�B~\B�B��B}�B{~B{�B|PB{JBy�B{By�B|�Bx�Bx�By�Bx7B��B��B�B��B��B�*B�nB��B��B�!B��B�[B�B�B��B�2B��B�B�GB��B�1B�	B��B�\BuB�BB�B�BSBYBB�B�B�B	�B.B�B�B!�B �B#�B$B#9B5B8�B;0B?HBK�BP�BU�BaBn�Bk�BjBj�Bm�Bm�Bm(BoiBr�BqBrBy	Bv�Bu�Bu�Bv�Bv�Bw�B{�B}"B�SB��B��B�1B��B�nB��B�!B�\B�kB��B�kB�kB��B�7B�	B��B�YB�YB��B��B�VB�_B�xB��B�7B�CB�xB�=B��BÕB��B�B��B~(Bw�BxlB{JBx�BzB{�B}�Bz�Bz�B~(By	Bt�Bu�Bt�Bs�B��Bw�Bu�B~(B�'BqBl�B`ABe�BgBgB`ABh�BquBf�Bh>Bd�Be�Bd�Bc�Bf�Bk�Bl"Bl"Bb�Bd�B^iBffB\�BbBpoB`�B[#BSZBM�BQBN�BK�BO�BK�BL/BMjBH�BJ�BK�BNpBIQBP�BM5BK)BJ�BGyBH�BG�BFsBF?BFsBE9BEBE9BD�BD3BIBF
BB�BFsBB�B@�B@�BDgBF
BA�BZQBa�BE�BJ�B@B6EB2�B2aB33B49B5?B0�B0UB+�B*dB,�B(�B*0B%�B'B �B"3B �B($B�B�B�B�B#�B)�B=B&B}BBhB%B�B�BCBB��B�`B��B�DB�B�.B��B�PB��B�;B�B�B�B�B�B��B�B��B��B��B�lB��B�B��B��B�tB��B��B��B�B��B��B�dB�B�9B�nB��B�9B�OB��B�OB��B�nB�7B��B��B�7B��Bu�Bw1B��Bl�BkPBi�BUgBMBbB;�B8�B;�BFsB0�B6�B)�BB�B�B�B	�B	B	B�B�B
=BB
��B
��BB
��B
��B
��B
�B
�B
��B
�B
�B
��B
�;B
�WB
��B
��B
קB
�>B
�EB
��B
ѷB
҉B
бB
�B
�2B
̘B
ǮB
��B
�B
̘B
��B
��B
�B
��B
�B
��B
�$B
��B
�RB
�B
�CB
��B
��B
�B
��B
�B
�!B
�eB
�YB
��B
��B
��B
h�B
m]B
jB
l�B
kPB
iDB
a�B
jB
c�B
_;B
O�B
NpB
L�B
I�B
HB
K�B
HB
F?B
GEB
4�B
=pB
IB
1�B
&LB
 'B
 [B
�B
�B
/�B
6EB
�B
�B
B
�B
	�B
VB
�B
1B	��B
�B	�DB	�+B	�B	��B	�rB	��B	�`B	�rB	ݘB	�sB	�&B	ҽB	��B	тB	��B	ΤB	ΤB	�|B	�B	�B	��B	ٴB	�EB	�yB	��B	�?B	ŢB	�9B	�mB	��B	��B	�aB	�B	�B	�<B	�6B	�dB	��B	��B	�)B	�)B	��B	�UB	��B	��B	��B	�kB	�B	�B	�<B	�zB	��B	�zB	�B	��B	��B	�9B	�zB	�kB	��B	�!B	�[B	��B	��B	��B	�XB	�B	��B	�!B	��B	��B	��B	�RB	�YB	�YB	��B	�B	��B	�qB	�7B	�CB	��B	�@B	��B	��B	�eB	�tB	�FB	��B	��B	�'B	��B	�B	��B	��B	�PB	�4B	��B	��B	�!B	�rB	�7B	�	B	��B	��B	�7B	�eB	��B	��B	��B	�%B	��B	�S4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225003                            20230721225003AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122500320230721225003  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500320230721225003QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500320230721225003QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             