CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:53Z creation      
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
resolution        =���   axis      Z        H  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  W�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  s�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 
   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` %�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   &@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ,@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   2@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T 8@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   8�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   8�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   8�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   8�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 8�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   94   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   9P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    9X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        9x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        9�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       9�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    9�Argo profile    3.1 1.2 19500101000000  20230721224953  20230721224953  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�i��Ϯ@�i��Ϯ11  @�j  �@�j  �@2?v_ح�@2?v_ح��dłj��F�dłj��F11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?�  @   @@  @�  @�G�@��R@�  A   A  A ��A,��A@  A_\)A�  A�  A�Q�A�Q�A��AϮA�  A��B (�BQ�BQ�B(�B (�B(  B/�B7�B?�
BG�
BO�
BX(�B`(�Bh  Bp  Bx  B�  B�  B��B��B��
B��B�{B�  B�  B�{B�(�B��B��B�{B�{B��B��B�  B�  B�  B��B��
B��B�{B�{B�  B�  B�{B��B�  B�  B��C   C  C��C��C��C

=C  C  C  C  C  C  C
=C��C  C�C��C"  C#�C&
=C({C)��C+��C.
=C0
=C1��C3��C6
=C8
=C:
=C<  C=��C@
=CB  CC��CE��CG��CI��CK��CM�CO��CQ��CS��CV
=CW��CZ  C\  C^
=C_��Ca��Cc�Ce��Cg��Cj
=Cl  Cn
=Co��Cr  Cs�Cv  Cx{Cz
=C|  C~
=C�  C�  C�C�  C�  C�C�C�  C�C�C���C���C�  C���C���C���C���C���C�  C�C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C���C���C�C�C�C�
=C�
=C�C�C�C���C�  C���C���C���C�  C�C�  C���C�  C�C�C�
=C�C�  C���C���C���C�C�  C�  C�C���C�  C�  C�  C�  C�C�C�C�  C�  C�  C���C���C���C���C�  C�C�C���C���C���C�C�C�  C�  C�  C���C�  C�  C�  C�C�  C�  C�C�  C���C�  C�  C���C�  C�C�C�  C���C���C�  C���C���C���C�  C�  C���C���C���C���C���C�  C���C���C�C�  C�  C���C�  C���C���C���D }qD  D��D�D� D  D� D  D� D��D}qD  D� D�qD� D�D� D	�D	��D	�qD
z�D
��D� D  Dz�D��D� D�qDz�D�qD� D  D��D�D��D�qD� D  D}qD  D� D�D�D  D}qD�qD��D�D}qD  D� D�D� D�qD� D�D�D�D}qD�qD� DD�D�qD }qD!  D!�D"D"��D#  D#� D$  D$��D%D%��D%��D&}qD'  D'� D(  D(}qD(�qD)}qD*�D*� D*�qD+� D+�qD,}qD-  D-��D.�D.� D.�qD/� D0D0� D1�D1� D2  D2}qD2�qD3� D4  D4}qD5  D5� D5�qD6� D6�qD7}qD8�D8� D8��D9}qD9�qD:}qD;  D;� D<  D<� D=�D=��D=�qD>��D?�D?� D@�D@��D@�qDA� DB  DBz�DC  DC��DD  DD� DEDE��DE��DFxRDF�qDG}qDG��DH}qDH�qDI}qDJ�DJ��DK  DK}qDK�qDLz�DL�qDM� DN  DN� DO  DO}qDO��DPz�DP��DQz�DR  DR�DSDS��DT  DT��DU�DU��DVDV��DW�DW� DW�qDX}qDY�DY��DY�qDZ}qDZ�qD[z�D[�qD\}qD\�qD]� D]�qD^� D_�D_��D`�D`��Da�Da��Db�Db��Dc  Dc� Dd  Dd��De  De}qDe�qDfz�Df�qDg� Dg�qDh}qDi�Di�Dj�Dj� Dj��Dk}qDl�Dl� Dl�qDm� Dn�Dn�Do�Do� Dp  Dp}qDp��Dq}qDq�qDr� DsDs��Dt  Dt� Du  Du}qDu��Dvz�Dv��Dw� DxDx��Dy�Dy�Dz�Dz}qDz�qD{� D|  D|}qD|��D}� D~�D~}qD  D��D�HD�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD�� D���D�>�D�~�D���D�  D�AHD�� D��HD�HD�B�D��HD��HD�HD�AHD�� D�� D�HD�AHD��HD�D�HD�AHD�� D�D��D�B�D��HD��HD�  D�@ D��HD�� D�  D�@ D��HD�� D�HD�@ D�� D�� D�  D�B�D���D��HD�HD�AHD�� D���D�  D�@ D�}qD���D�  D�AHD���D�� D�  D�@ D��HD��HD���D�@ D�� D�� D���D�>�D�� D�� D���D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D�}qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?\)?k�?�z�?�33?�(�@   @\)@&ff@5@E�@\(�@h��@}p�@���@�\)@��H@��
@��@�@��R@�ff@�33@��H@��
@�\)@�
=@��RAA��A{A33A
=A�A!G�A%�A)��A0  A2�\A8��A<��A@��AEAJ�HAN{AS�
AW�AZ�HA`��AeAh��Al��Ar�\AuAy��A\)A�G�A�33A�{A��A���A�z�A�ffA�Q�A��HA�p�A�\)A�G�A��
A��RA�Q�A��A��A��RA���A�(�A�A�  A��\A���A��RA���A��A�p�A���A�=qA�z�A�\)A�G�A�33A�ffA�  A��A���A�\)A���A�33A�{A�  A��A���A�RA���A��
A�{A�A�\A��A��RA�G�A�(�A�B (�B��BffB�B�B=qB33Bz�B	�B
�HB  Bp�B�\B�B��B=qB33B(�B��B�RB�B�BffB33BQ�B�B�HB�B!G�B"ffB#33B$z�B%�B&�RB'�
B)p�B*=qB+\)B,��B-�B.�HB0z�B1��B2ffB4(�B5�B6{B7�B8��B9B:�HB<Q�B=B>�\B?�
BAG�BBffBC\)BD��BF=qBG33BH(�BI��BK
=BL(�BM�BN�\BO�
BP��BQBS33BT(�BUG�BV�HBW�BX��BZffB[�B\Q�B]�B^�HB`  Bap�Bb�RBc�Bd��Bf=qBg33Bh(�Bip�Bj�HBk�Bl��Bn=qBo33Bp(�Bq��Br�RBs�Bt��Bv=qBw
=BxQ�By��BzffB{�
B}�B}�B33B�=qB��RB�G�B�  B��\B�
=B��B�ffB��HB�G�B�  B��RB��B��B�z�B��HB�p�B�=qB���B�33B��B��\B���B���B�ffB���B�G�B�  B���B�
=B���B�Q�B���B�G�B��B��\B���B��B�Q�B��RB�G�B�  B��\B�
=B���B�{B���B�\)B�B�ffB��B���B�  B��RB�\)B��
B�=qB���B���B�  B���B�\)B�B�=qB���B��B��B��\B�G�B�B�=qB��HB��B��B�ffB��B�B�=qB��RB�p�B�(�B��\B�
=B�B�z�B��HB�p�B�=qB���B�33B��B���B�
=B���B�Q�B���B�p�B�  B���B�\)B��
B�Q�B�
=B�B�(�B��RB�p�B�{B�z�B��B��
B�ffB��HB�p�B�(�B���B�33B�B�z�B�
=B�p�B�=qB���B�G�B�Bȏ\B��BɅB�(�B���B�\)B��
B�Q�B���BͮB�{BΏ\B�\)B��
B�=qB��HBљ�B�  B�z�B�33B��
B�(�BԸRB�\)B�  B�z�B��HB�p�B�(�BظRB�
=Bٙ�B�Q�B���B�33B�  B܏\B���Bݙ�B�ffB���B�\)B�{B���B�G�B��
B��B�
=B㙚B�ffB��HB�\)B�{B���B�33B�B�z�B��B�B�=qB�RB�33B�  B�\B���B홚B�Q�B�RB�\)B�{B�z�B�
=B�B�ffB��HB�p�B�{B���B�p�B��B��\B�G�B��B�ffB���B�B�z�B���B��B�Q�B���B�\)B�{B��RB�33B�C G�C �\C ��C{Cz�C��C
=C\)CC
=CG�C��C
=CQ�C�\C�CG�C�\C�
C33C��C�
C�Cz�C�
C(�C\)CC	�C	p�C	�RC
{C
z�C
��C
=CffC��C(�CffC�RC
=Cz�C��C(�CffC�C{Cz�C�RC
=C\)CC{CQ�C��C{C\)C��C��CQ�C�C��CG�C�C{C\)C��C  C\)C�C��CG�C�C{CffC�C
=CQ�C�RC�Cp�C�RC
=C\)C�RC(�C�C��C�Cp�C�RC{Cp�C�
C�CffCC (�C �C ��C!{C!ffC!�RC"{C"z�C"�
C#�C#ffC#C${C$p�C$�
C%33C%�\C%�HC&33C&z�C&��C'(�C'�C'�C(G�C(��C(�C)=qC)�\C)�HC*(�C*z�C*��C+=qC+��C+�C,33C,�C,�HC-G�C-��C.  C.G�C.��C.�HC/G�C/�C0
=C0p�C0C1{C1ffC1�RC2  C2Q�C2��C3  C3\)C3��C3�
C4�C4p�C4C5{C5G�C5�C5�RC6  C6=qC6z�C6�RC6�HC7  C7=qC7p�C7�C7��C7�C8{C8Q�C8�C8��C8��C8��C9(�C9\)C9�\C9�RC9�
C:
=C:=qC:p�C:�\C:�RC:�HC;
=C;=qC;z�C;�C;�HC<  C<(�C<G�C<�C<�RC<�C={C=33C=\)C=�\C=C>  C>{C>=qC>ffC>��C>��C>��C?�C?=qC?ffC?��C?�
C?�C@{C@=qC@z�C@�C@��C@�HCA{CA=qCAp�CA��CA�
CA�CB
=CB33CBffCB��CB��CB��CC�CC=qCC\)CC�\CC��CC��CD{CD(�CDQ�CD�\CD�RCD�CE{CE33CEG�CEp�CE��CE��CF
=CF33CF\)CF�CF��CFCF�CG�CGQ�CGz�CG��CG�RCG�HCH{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333333                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @   @@  @�  @�G�@��R@�  A   A  A ��A,��A@  A_\)A�  A�  A�Q�A�Q�A��AϮA�  A��B (�BQ�BQ�B(�B (�B(  B/�B7�B?�
BG�
BO�
BX(�B`(�Bh  Bp  Bx  B�  B�  B��B��B��
B��B�{B�  B�  B�{B�(�B��B��B�{B�{B��B��B�  B�  B�  B��B��
B��B�{B�{B�  B�  B�{B��B�  B�  B��C   C  C��C��C��C

=C  C  C  C  C  C  C
=C��C  C�C��C"  C#�C&
=C({C)��C+��C.
=C0
=C1��C3��C6
=C8
=C:
=C<  C=��C@
=CB  CC��CE��CG��CI��CK��CM�CO��CQ��CS��CV
=CW��CZ  C\  C^
=C_��Ca��Cc�Ce��Cg��Cj
=Cl  Cn
=Co��Cr  Cs�Cv  Cx{Cz
=C|  C~
=C�  C�  C�C�  C�  C�C�C�  C�C�C���C���C�  C���C���C���C���C���C�  C�C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C���C���C�C�C�C�
=C�
=C�C�C�C���C�  C���C���C���C�  C�C�  C���C�  C�C�C�
=C�C�  C���C���C���C�C�  C�  C�C���C�  C�  C�  C�  C�C�C�C�  C�  C�  C���C���C���C���C�  C�C�C���C���C���C�C�C�  C�  C�  C���C�  C�  C�  C�C�  C�  C�C�  C���C�  C�  C���C�  C�C�C�  C���C���C�  C���C���C���C�  C�  C���C���C���C���C���C�  C���C���C�C�  C�  C���C�  C���C���C���D }qD  D��D�D� D  D� D  D� D��D}qD  D� D�qD� D�D� D	�D	��D	�qD
z�D
��D� D  Dz�D��D� D�qDz�D�qD� D  D��D�D��D�qD� D  D}qD  D� D�D�D  D}qD�qD��D�D}qD  D� D�D� D�qD� D�D�D�D}qD�qD� DD�D�qD }qD!  D!�D"D"��D#  D#� D$  D$��D%D%��D%��D&}qD'  D'� D(  D(}qD(�qD)}qD*�D*� D*�qD+� D+�qD,}qD-  D-��D.�D.� D.�qD/� D0D0� D1�D1� D2  D2}qD2�qD3� D4  D4}qD5  D5� D5�qD6� D6�qD7}qD8�D8� D8��D9}qD9�qD:}qD;  D;� D<  D<� D=�D=��D=�qD>��D?�D?� D@�D@��D@�qDA� DB  DBz�DC  DC��DD  DD� DEDE��DE��DFxRDF�qDG}qDG��DH}qDH�qDI}qDJ�DJ��DK  DK}qDK�qDLz�DL�qDM� DN  DN� DO  DO}qDO��DPz�DP��DQz�DR  DR�DSDS��DT  DT��DU�DU��DVDV��DW�DW� DW�qDX}qDY�DY��DY�qDZ}qDZ�qD[z�D[�qD\}qD\�qD]� D]�qD^� D_�D_��D`�D`��Da�Da��Db�Db��Dc  Dc� Dd  Dd��De  De}qDe�qDfz�Df�qDg� Dg�qDh}qDi�Di�Dj�Dj� Dj��Dk}qDl�Dl� Dl�qDm� Dn�Dn�Do�Do� Dp  Dp}qDp��Dq}qDq�qDr� DsDs��Dt  Dt� Du  Du}qDu��Dvz�Dv��Dw� DxDx��Dy�Dy�Dz�Dz}qDz�qD{� D|  D|}qD|��D}� D~�D~}qD  D��D�HD�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD�� D���D�>�D�~�D���D�  D�AHD�� D��HD�HD�B�D��HD��HD�HD�AHD�� D�� D�HD�AHD��HD�D�HD�AHD�� D�D��D�B�D��HD��HD�  D�@ D��HD�� D�  D�@ D��HD�� D�HD�@ D�� D�� D�  D�B�D���D��HD�HD�AHD�� D���D�  D�@ D�}qD���D�  D�AHD���D�� D�  D�@ D��HD��HD���D�@ D�� D�� D���D�>�D�� D�� D���D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D�}qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?\)?k�?�z�?�33?�(�@   @\)@&ff@5@E�@\(�@h��@}p�@���@�\)@��H@��
@��@�@��R@�ff@�33@��H@��
@�\)@�
=@��RAA��A{A33A
=A�A!G�A%�A)��A0  A2�\A8��A<��A@��AEAJ�HAN{AS�
AW�AZ�HA`��AeAh��Al��Ar�\AuAy��A\)A�G�A�33A�{A��A���A�z�A�ffA�Q�A��HA�p�A�\)A�G�A��
A��RA�Q�A��A��A��RA���A�(�A�A�  A��\A���A��RA���A��A�p�A���A�=qA�z�A�\)A�G�A�33A�ffA�  A��A���A�\)A���A�33A�{A�  A��A���A�RA���A��
A�{A�A�\A��A��RA�G�A�(�A�B (�B��BffB�B�B=qB33Bz�B	�B
�HB  Bp�B�\B�B��B=qB33B(�B��B�RB�B�BffB33BQ�B�B�HB�B!G�B"ffB#33B$z�B%�B&�RB'�
B)p�B*=qB+\)B,��B-�B.�HB0z�B1��B2ffB4(�B5�B6{B7�B8��B9B:�HB<Q�B=B>�\B?�
BAG�BBffBC\)BD��BF=qBG33BH(�BI��BK
=BL(�BM�BN�\BO�
BP��BQBS33BT(�BUG�BV�HBW�BX��BZffB[�B\Q�B]�B^�HB`  Bap�Bb�RBc�Bd��Bf=qBg33Bh(�Bip�Bj�HBk�Bl��Bn=qBo33Bp(�Bq��Br�RBs�Bt��Bv=qBw
=BxQ�By��BzffB{�
B}�B}�B33B�=qB��RB�G�B�  B��\B�
=B��B�ffB��HB�G�B�  B��RB��B��B�z�B��HB�p�B�=qB���B�33B��B��\B���B���B�ffB���B�G�B�  B���B�
=B���B�Q�B���B�G�B��B��\B���B��B�Q�B��RB�G�B�  B��\B�
=B���B�{B���B�\)B�B�ffB��B���B�  B��RB�\)B��
B�=qB���B���B�  B���B�\)B�B�=qB���B��B��B��\B�G�B�B�=qB��HB��B��B�ffB��B�B�=qB��RB�p�B�(�B��\B�
=B�B�z�B��HB�p�B�=qB���B�33B��B���B�
=B���B�Q�B���B�p�B�  B���B�\)B��
B�Q�B�
=B�B�(�B��RB�p�B�{B�z�B��B��
B�ffB��HB�p�B�(�B���B�33B�B�z�B�
=B�p�B�=qB���B�G�B�Bȏ\B��BɅB�(�B���B�\)B��
B�Q�B���BͮB�{BΏ\B�\)B��
B�=qB��HBљ�B�  B�z�B�33B��
B�(�BԸRB�\)B�  B�z�B��HB�p�B�(�BظRB�
=Bٙ�B�Q�B���B�33B�  B܏\B���Bݙ�B�ffB���B�\)B�{B���B�G�B��
B��B�
=B㙚B�ffB��HB�\)B�{B���B�33B�B�z�B��B�B�=qB�RB�33B�  B�\B���B홚B�Q�B�RB�\)B�{B�z�B�
=B�B�ffB��HB�p�B�{B���B�p�B��B��\B�G�B��B�ffB���B�B�z�B���B��B�Q�B���B�\)B�{B��RB�33B�C G�C �\C ��C{Cz�C��C
=C\)CC
=CG�C��C
=CQ�C�\C�CG�C�\C�
C33C��C�
C�Cz�C�
C(�C\)CC	�C	p�C	�RC
{C
z�C
��C
=CffC��C(�CffC�RC
=Cz�C��C(�CffC�C{Cz�C�RC
=C\)CC{CQ�C��C{C\)C��C��CQ�C�C��CG�C�C{C\)C��C  C\)C�C��CG�C�C{CffC�C
=CQ�C�RC�Cp�C�RC
=C\)C�RC(�C�C��C�Cp�C�RC{Cp�C�
C�CffCC (�C �C ��C!{C!ffC!�RC"{C"z�C"�
C#�C#ffC#C${C$p�C$�
C%33C%�\C%�HC&33C&z�C&��C'(�C'�C'�C(G�C(��C(�C)=qC)�\C)�HC*(�C*z�C*��C+=qC+��C+�C,33C,�C,�HC-G�C-��C.  C.G�C.��C.�HC/G�C/�C0
=C0p�C0C1{C1ffC1�RC2  C2Q�C2��C3  C3\)C3��C3�
C4�C4p�C4C5{C5G�C5�C5�RC6  C6=qC6z�C6�RC6�HC7  C7=qC7p�C7�C7��C7�C8{C8Q�C8�C8��C8��C8��C9(�C9\)C9�\C9�RC9�
C:
=C:=qC:p�C:�\C:�RC:�HC;
=C;=qC;z�C;�C;�HC<  C<(�C<G�C<�C<�RC<�C={C=33C=\)C=�\C=C>  C>{C>=qC>ffC>��C>��C>��C?�C?=qC?ffC?��C?�
C?�C@{C@=qC@z�C@�C@��C@�HCA{CA=qCAp�CA��CA�
CA�CB
=CB33CBffCB��CB��CB��CC�CC=qCC\)CC�\CC��CC��CD{CD(�CDQ�CD�\CD�RCD�CE{CE33CEG�CEp�CE��CE��CF
=CF33CF\)CF�CF��CFCF�CG�CGQ�CGz�CG��CG�RCG�HCH{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333333                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�`BA�`BA�dZA�^5A�\)A�l�A�l�A�l�A�hsA�jA�ffA�dZA�;dA�O�A�K�A̰!A�|�A���A̟�A�M�A���A��yA��;A���A�~�A���A̬A���A���A��A��A���A̗�A�ffA�$�A��mAˏ\A��A��TAʶFAʋDA�jA�(�A�bA�ĜA�jA�`BA�33A��yAȲ-Aȗ�AȑhA�~�A�33A�%Aǉ7A��A�  AƁA���A�t�A�x�A��!A�`BA�S�A�ƨA���A���A�C�A��A��A�ZA��mA��^A���A���A�x�A���A�
=A��A�\)A�-A�+A�|�A���A�G�A��^A��RA��A�t�A�M�A�Q�A��HA�E�A��/A��9A�`BA���A��A�`BA�-A�A�bNA�ffA�VA�\)A�\)A���A��AG�A}`BAz{Ax�yAv��As�^Ap=qAl�DAf�AbZAa�;A`��A^~�A\�uAZ �AY�#AY��AYAWAT�DATI�AS�
AP�+AL��AL��AKG�AIp�AH�DAH �AG�#AGXAFE�AEl�ADE�ACG�ABĜA?�A=��A< �A:v�A:A9��A7?}A4bNA4=qA3��A3?}A2�HA2jA1��A01'A.I�A,�!A+��A*��A)��A)C�A(~�A'K�A&r�A&(�A%G�A$�A$1A#|�A"jA 1A�At�A��A�wA7LAZAt�A�#A�A
=A��A�A$�A�jAdZA�HA=qA�^A\)A
��A
�!A
^5A
�A	l�A��AAdZA�yA��A(�AJA�A�A�A��A�A�/A��A�mA��A��A�
A`BA �H@�t�@��!@�ff@�-@�A�@���@��@��@���@�~�@�M�@���@�\)@��@�;d@���@�@�@ꟾ@��@�X@�&�@��@��@�j@�9X@�(�@� �@��
@�dZ@���@��@�7L@߮@޸R@ݙ�@ܬ@�1@�K�@���@�5?@١�@�`B@��/@��m@�"�@�^5@�E�@֟�@ו�@�;d@��H@պ^@� �@�V@���@Ѳ-@щ7@�&�@�%@мj@�(�@��;@�l�@�S�@�+@ΰ!@̋D@�1@�"�@�"�@��@�~�@��@�Z@�I�@ǅ@�
=@�{@�?}@�b@î@���@��h@��@���@��j@�Q�@��@��@�$�@�@��@�X@���@���@�j@��@���@�t�@�C�@�+@�
=@��y@���@��!@��+@�$�@�&�@�z�@�(�@��
@��@��y@��R@���@��\@�v�@�M�@�@��@���@��u@�j@��;@�t�@�;d@��!@��7@��@���@���@��9@��D@�j@��;@�dZ@�o@��@�v�@�M�@�E�@���@��@���@��@�Z@��@���@�K�@��@���@��@��^@�/@��/@��j@��@���@�Z@�(�@��
@��P@�S�@�C�@�33@�@���@��y@��@���@�E�@�E�@�{@��-@�`B@���@���@��F@�l�@���@���@�M�@�@��^@���@�`B@�Ĝ@���@�bN@�1'@���@�C�@�;d@�"�@�o@��@���@�@�hs@�7L@�%@��/@��j@��D@�j@�Q�@�A�@�  @�l�@��y@���@���@�M�@��@���@�`B@�?}@�&�@�V@���@���@��@�A�@��;@��@�33@�o@��@�ȴ@��\@�v�@�M�@��@��-@�hs@�7L@���@��@��@�I�@�(�@�1@��w@�t�@�K�@�"�@�@���@�^5@�=q@�{@��^@��h@��@�`B@�7L@�V@��/@�z�@�(�@��;@���@�l�@�K�@��y@���@�ff@�@��^@�X@�%@���@�A�@��m@��
@��
@��
@��
@��
@��
@���@��w@�\)@���@�~�@�J@�@���@���@���@�hs@��/@��D@�1'@�1'@���@�dZ@��!@���@�~�@�ff@�M�@�{@��@��-@��7@�X@��@��@���@���@��@�l�@�S�@�o@�ȴ@���@���@�M�@��@���@��-@��@�hs@�7L@��@���@���@��`@��u@�;@K�@K�@~ȴ@}�@}@}p�@|��@|�@{S�@z��@z~�@z~�@z^5@z^5@zn�@zJ@y�7@yX@yG�@y�@xĜ@xQ�@w�;@w�@w
=@v��@v��@v$�@v$�@u�@u�h@u��@u?}@uV@t�D@tZ@t(�@sƨ@s��@sdZ@s33@so@r��@r�!@rn�@rJ@q��@qx�@p��@pr�@o�w@o;d@n��@nȴ@n��@nff@n{@m��@mp�@l�/@lj@l1@k�m@k��@kC�@k@j�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�XA�`BA�bNA�\)A�`BA�`BA�\)A�^5A�\)A�bNA�hsA�\)A�bNA�hsA�bNA�^5A�^5A�XA�\)A�\)A�ZA�jA�l�A�l�A�n�A�l�A�jA�p�A�l�A�n�A�p�A�hsA�hsA�l�A�hsA�jA�jA�ffA�l�A�hsA�dZA�ffA�dZA�bNA�ffA�dZA�bNA�hsA�ffA�bNA�`BA�^5A�S�A�Q�A�Q�A�K�A�$�A�VA��`Aΰ!AΣ�A�v�A�Q�A�A��A���A͓uA�v�A�XA�9XA�(�A�"�A�{A�JA��A�ĜA̓uÃÃÃȦ+A�~�A�z�A�v�A�l�A�t�A̓uA̾wA��HA��A��TA��HA���A���A̮Ạ�Ḁ�A̙�A̕�A̛�A̗�Ȁ\A̍PA�v�A�ZA�M�A�oA���A�  A�A�  A���A���A��A��A��A��A��A��A��TA��/A��/A��mA��;A��A��;A��TA��
A��
A��HA��/A���A�ƨA�ĜA���A˰!A˕�A�|�A�n�A�n�A�t�A�t�A�z�A˗�A˸RA���A��`A�A�"�A�Q�A̙�A̼jA���A��#A���A̼jA��A��`A�1A�/A��A�A��A���A̰!A���A��;A��/A��TA��mA��A��A��yA��A��A��A��A���A���A��A��A��A��mA��HA��HA��#A��#A���A̲-Ḁ�A̧�A̡�A̛�A̕�ÃA�v�A�r�A�v�A�p�A�dZA�^5A�XA�G�A�=qA�;dA�+A� �A��A�bA�A�  A���A��A��mA��;A���A���A�ƨA˲-Aˡ�AˋDA�r�A�`BA�VA�K�A�33A� �A�%A�A���A��A��A��yA��`A��`A��A���A���A�Aʺ^AʸRAʰ!Aʣ�Aʡ�Aʟ�Aʏ\AʋDAʉ7AʅA�|�A�|�AʁA�z�A�v�A�p�A�jA�\)A�I�A�;dA�33A�&�A�+A�&�A� �A��A��A��A��A�{A�VA�JA�  A��A��A��TA���AɸRAɧ�Aɕ�A�~�A�r�A�r�A�jA�hsA�ffA�bNA�^5A�`BA�ffA�bNA�^5A�bNA�bNA�S�A�K�A�?}A�;dA�-A�$�A��A�bA�  A��A��HA��/A��A��A��#A���AȮAȩ�Aȧ�Aȟ�Aȝ�Aȟ�Aț�Aȕ�Aȕ�Aȕ�Aȗ�Aȕ�AȓuAȕ�Aȕ�Aȏ\AȍPAȍPAȋDAȅAȅAȅAȃA�x�A�jA�\)A�I�A�9XA�7LA�+A� �A��A�bA�bA�1A�1A�%A�A���A��A���AǼjAǛ�A�`BA�C�A�9XA�-A�(�A� �A��A�VA�
=A�VA�JA�A�%A�A���A���A���A���A���A��mAƋDA�^5A�G�A�/A�
=A���A�~�A�I�A��TAğ�A�G�Aß�A��A���A�XA�C�A�7LA���A��;A���A��RA��PA�\)A�;dA�&�A�VA���A���A���A��PA�x�A�p�A�n�A�;dA���A�9XA��A���A���A���A�Q�A�/A���A���A�/A�ȴA��jA��\A��A��A�\)A���A�\)A��A���A���A��FA��RA��9A��A���A���A���A���A���A��uA��A�n�A�jA�^5A�$�A��9A�^5A��TA��wA��RA�~�A�l�A�O�A�;dA�(�A�$�A�{A��A���A���A�p�A�\)A�9XA�-A�&�A�oA�  A���A��HA�ƨA��^A���A��A�A�A��RA��A��9A�ffA�G�A�bA���A�~�A�ZA�bA�l�A���A�p�A�VA�C�A�(�A�%A�~�A��A�(�A�5?A���A�G�A��;A���A�`BA�VA��A�=qA�t�A�9XA���A���A��A�&�A��wA���A���A��A��!A��A���A��DA�XA��A��A��DA�G�A�A���A��A�dZA�O�A�O�A�"�A�1A�A��9A��9A��RA��uA�G�A���A�XA��/A��7A�~�A�`BA�9XA��A��A��A��A�ĜA��PA�M�A���A���A��A�l�A�5?A���A��A�ȴA��A��hA�p�A�O�A�&�A���A��wA��DA�`BA�(�A���A�A�~�A��uA�A�A��A���A�v�A��A��uA�O�A��A���A�ƨA�A�p�A�{A��-A�E�A���A�\)A�M�A�G�A�E�A�C�A�C�A�E�A�G�A�K�A�?}A��TA��!A��!A��-A��9A��9A��9A��-A��!A��9A��FA��FA��FA��A���A���A��uA�z�A�jA�ZA�K�A�G�A�A�A�;dA�1'A�$�A�oA�VA�JA�A���A��mA�ƨA��!A��A�l�A�Q�A�I�A�7LA��A��A�  A��A��#A���A�ȴA��FA��!A���A��hA�|�A�p�A�`BA�\)A�O�A�C�A�?}A�?}A�?}A�=qA�9XA�7LA�33A�7LA�7LA�33A�33A�/A�1'A��A��A�{A�bA�oA�{A�
=A�JA�%A�%A�  A�  A���A��TA��#A���A���A�x�A�bNA�Q�A�M�A�I�A�E�A�E�A�A�A�=qA�7LA�1A��PA�`BA�VA�p�A�-A�{A��A��#A��RA��uA�hsA�9XA��
A��-A���A���A���A��PA�z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333333                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�^5A�`BA�`BA�dZA�^5A�\)A�l�A�l�A�l�A�hsA�jA�ffA�dZA�;dA�O�A�K�A̰!A�|�A���A̟�A�M�A���A��yA��;A���A�~�A���A̬A���A���A��A��A���A̗�A�ffA�$�A��mAˏ\A��A��TAʶFAʋDA�jA�(�A�bA�ĜA�jA�`BA�33A��yAȲ-Aȗ�AȑhA�~�A�33A�%Aǉ7A��A�  AƁA���A�t�A�x�A��!A�`BA�S�A�ƨA���A���A�C�A��A��A�ZA��mA��^A���A���A�x�A���A�
=A��A�\)A�-A�+A�|�A���A�G�A��^A��RA��A�t�A�M�A�Q�A��HA�E�A��/A��9A�`BA���A��A�`BA�-A�A�bNA�ffA�VA�\)A�\)A���A��AG�A}`BAz{Ax�yAv��As�^Ap=qAl�DAf�AbZAa�;A`��A^~�A\�uAZ �AY�#AY��AYAWAT�DATI�AS�
AP�+AL��AL��AKG�AIp�AH�DAH �AG�#AGXAFE�AEl�ADE�ACG�ABĜA?�A=��A< �A:v�A:A9��A7?}A4bNA4=qA3��A3?}A2�HA2jA1��A01'A.I�A,�!A+��A*��A)��A)C�A(~�A'K�A&r�A&(�A%G�A$�A$1A#|�A"jA 1A�At�A��A�wA7LAZAt�A�#A�A
=A��A�A$�A�jAdZA�HA=qA�^A\)A
��A
�!A
^5A
�A	l�A��AAdZA�yA��A(�AJA�A�A�A��A�A�/A��A�mA��A��A�
A`BA �H@�t�@��!@�ff@�-@�A�@���@��@��@���@�~�@�M�@���@�\)@��@�;d@���@�@�@ꟾ@��@�X@�&�@��@��@�j@�9X@�(�@� �@��
@�dZ@���@��@�7L@߮@޸R@ݙ�@ܬ@�1@�K�@���@�5?@١�@�`B@��/@��m@�"�@�^5@�E�@֟�@ו�@�;d@��H@պ^@� �@�V@���@Ѳ-@щ7@�&�@�%@мj@�(�@��;@�l�@�S�@�+@ΰ!@̋D@�1@�"�@�"�@��@�~�@��@�Z@�I�@ǅ@�
=@�{@�?}@�b@î@���@��h@��@���@��j@�Q�@��@��@�$�@�@��@�X@���@���@�j@��@���@�t�@�C�@�+@�
=@��y@���@��!@��+@�$�@�&�@�z�@�(�@��
@��@��y@��R@���@��\@�v�@�M�@�@��@���@��u@�j@��;@�t�@�;d@��!@��7@��@���@���@��9@��D@�j@��;@�dZ@�o@��@�v�@�M�@�E�@���@��@���@��@�Z@��@���@�K�@��@���@��@��^@�/@��/@��j@��@���@�Z@�(�@��
@��P@�S�@�C�@�33@�@���@��y@��@���@�E�@�E�@�{@��-@�`B@���@���@��F@�l�@���@���@�M�@�@��^@���@�`B@�Ĝ@���@�bN@�1'@���@�C�@�;d@�"�@�o@��@���@�@�hs@�7L@�%@��/@��j@��D@�j@�Q�@�A�@�  @�l�@��y@���@���@�M�@��@���@�`B@�?}@�&�@�V@���@���@��@�A�@��;@��@�33@�o@��@�ȴ@��\@�v�@�M�@��@��-@�hs@�7L@���@��@��@�I�@�(�@�1@��w@�t�@�K�@�"�@�@���@�^5@�=q@�{@��^@��h@��@�`B@�7L@�V@��/@�z�@�(�@��;@���@�l�@�K�@��y@���@�ff@�@��^@�X@�%@���@�A�@��m@��
@��
@��
@��
@��
@��
@���@��w@�\)@���@�~�@�J@�@���@���@���@�hs@��/@��D@�1'@�1'@���@�dZ@��!@���@�~�@�ff@�M�@�{@��@��-@��7@�X@��@��@���@���@��@�l�@�S�@�o@�ȴ@���@���@�M�@��@���@��-@��@�hs@�7L@��@���@���@��`@��u@�;@K�@K�@~ȴ@}�@}@}p�@|��@|�@{S�@z��@z~�@z~�@z^5@z^5@zn�@zJ@y�7@yX@yG�@y�@xĜ@xQ�@w�;@w�@w
=@v��@v��@v$�@v$�@u�@u�h@u��@u?}@uV@t�D@tZ@t(�@sƨ@s��@sdZ@s33@so@r��@r�!@rn�@rJ@q��@qx�@p��@pr�@o�w@o;d@n��@nȴ@n��@nff@n{@m��@mp�@l�/@lj@l1@k�m@k��@kC�@k@j�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�XA�`BA�bNA�\)A�`BA�`BA�\)A�^5A�\)A�bNA�hsA�\)A�bNA�hsA�bNA�^5A�^5A�XA�\)A�\)A�ZA�jA�l�A�l�A�n�A�l�A�jA�p�A�l�A�n�A�p�A�hsA�hsA�l�A�hsA�jA�jA�ffA�l�A�hsA�dZA�ffA�dZA�bNA�ffA�dZA�bNA�hsA�ffA�bNA�`BA�^5A�S�A�Q�A�Q�A�K�A�$�A�VA��`Aΰ!AΣ�A�v�A�Q�A�A��A���A͓uA�v�A�XA�9XA�(�A�"�A�{A�JA��A�ĜA̓uÃÃÃȦ+A�~�A�z�A�v�A�l�A�t�A̓uA̾wA��HA��A��TA��HA���A���A̮Ạ�Ḁ�A̙�A̕�A̛�A̗�Ȁ\A̍PA�v�A�ZA�M�A�oA���A�  A�A�  A���A���A��A��A��A��A��A��A��TA��/A��/A��mA��;A��A��;A��TA��
A��
A��HA��/A���A�ƨA�ĜA���A˰!A˕�A�|�A�n�A�n�A�t�A�t�A�z�A˗�A˸RA���A��`A�A�"�A�Q�A̙�A̼jA���A��#A���A̼jA��A��`A�1A�/A��A�A��A���A̰!A���A��;A��/A��TA��mA��A��A��yA��A��A��A��A���A���A��A��A��A��mA��HA��HA��#A��#A���A̲-Ḁ�A̧�A̡�A̛�A̕�ÃA�v�A�r�A�v�A�p�A�dZA�^5A�XA�G�A�=qA�;dA�+A� �A��A�bA�A�  A���A��A��mA��;A���A���A�ƨA˲-Aˡ�AˋDA�r�A�`BA�VA�K�A�33A� �A�%A�A���A��A��A��yA��`A��`A��A���A���A�Aʺ^AʸRAʰ!Aʣ�Aʡ�Aʟ�Aʏ\AʋDAʉ7AʅA�|�A�|�AʁA�z�A�v�A�p�A�jA�\)A�I�A�;dA�33A�&�A�+A�&�A� �A��A��A��A��A�{A�VA�JA�  A��A��A��TA���AɸRAɧ�Aɕ�A�~�A�r�A�r�A�jA�hsA�ffA�bNA�^5A�`BA�ffA�bNA�^5A�bNA�bNA�S�A�K�A�?}A�;dA�-A�$�A��A�bA�  A��A��HA��/A��A��A��#A���AȮAȩ�Aȧ�Aȟ�Aȝ�Aȟ�Aț�Aȕ�Aȕ�Aȕ�Aȗ�Aȕ�AȓuAȕ�Aȕ�Aȏ\AȍPAȍPAȋDAȅAȅAȅAȃA�x�A�jA�\)A�I�A�9XA�7LA�+A� �A��A�bA�bA�1A�1A�%A�A���A��A���AǼjAǛ�A�`BA�C�A�9XA�-A�(�A� �A��A�VA�
=A�VA�JA�A�%A�A���A���A���A���A���A��mAƋDA�^5A�G�A�/A�
=A���A�~�A�I�A��TAğ�A�G�Aß�A��A���A�XA�C�A�7LA���A��;A���A��RA��PA�\)A�;dA�&�A�VA���A���A���A��PA�x�A�p�A�n�A�;dA���A�9XA��A���A���A���A�Q�A�/A���A���A�/A�ȴA��jA��\A��A��A�\)A���A�\)A��A���A���A��FA��RA��9A��A���A���A���A���A���A��uA��A�n�A�jA�^5A�$�A��9A�^5A��TA��wA��RA�~�A�l�A�O�A�;dA�(�A�$�A�{A��A���A���A�p�A�\)A�9XA�-A�&�A�oA�  A���A��HA�ƨA��^A���A��A�A�A��RA��A��9A�ffA�G�A�bA���A�~�A�ZA�bA�l�A���A�p�A�VA�C�A�(�A�%A�~�A��A�(�A�5?A���A�G�A��;A���A�`BA�VA��A�=qA�t�A�9XA���A���A��A�&�A��wA���A���A��A��!A��A���A��DA�XA��A��A��DA�G�A�A���A��A�dZA�O�A�O�A�"�A�1A�A��9A��9A��RA��uA�G�A���A�XA��/A��7A�~�A�`BA�9XA��A��A��A��A�ĜA��PA�M�A���A���A��A�l�A�5?A���A��A�ȴA��A��hA�p�A�O�A�&�A���A��wA��DA�`BA�(�A���A�A�~�A��uA�A�A��A���A�v�A��A��uA�O�A��A���A�ƨA�A�p�A�{A��-A�E�A���A�\)A�M�A�G�A�E�A�C�A�C�A�E�A�G�A�K�A�?}A��TA��!A��!A��-A��9A��9A��9A��-A��!A��9A��FA��FA��FA��A���A���A��uA�z�A�jA�ZA�K�A�G�A�A�A�;dA�1'A�$�A�oA�VA�JA�A���A��mA�ƨA��!A��A�l�A�Q�A�I�A�7LA��A��A�  A��A��#A���A�ȴA��FA��!A���A��hA�|�A�p�A�`BA�\)A�O�A�C�A�?}A�?}A�?}A�=qA�9XA�7LA�33A�7LA�7LA�33A�33A�/A�1'A��A��A�{A�bA�oA�{A�
=A�JA�%A�%A�  A�  A���A��TA��#A���A���A�x�A�bNA�Q�A�M�A�I�A�E�A�E�A�A�A�=qA�7LA�1A��PA�`BA�VA�p�A�-A�{A��A��#A��RA��uA�hsA�9XA��
A��-A���A���A���A��PA�z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333333                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�FB
��B
�tB
��B
�LB
��B
��B
��B
��B
�FB
��B
�zB
�@B
�*B
��B
�B
ԕB
�
B)*B4�B@BZ�Bg�Bq�B|�B�_B��B��BCB&LB0!B2�B6zB>BBEBM6BP�BZ�Bn�BzDB�iB�SB�=B�FB�$B�~B��B��B��B�6B�-B�BʌB�BخB��B�B��BچB��B�#B�WB�B��B�WB��B�dB��B~(BuZBD�B0�B]/B^jB[WBE�B9$B	B�B��B�B�XB��B�Bw�B]dBMjB1�B+B,�B/�B �B
�(B
ӏB
�B
��B
�RB
��B
��B
�B
��B
��B
��B
�~B
�1B
n/B
hsB
`BB
S[B
4nB
&�B
qB
oB	�`B	�B	�B	��B	�RB	��B	t�B	v`B	wfB	o5B	h�B	T�B	K�B	IB	B�B	?B	0�B	0�B	0�B	(XB	�B	"4B	.�B	�B		B	�B	~B	�B	�B	FB	:B	�B	%B	B�B�cB�mB�B�jB��BʌBɺB�)BʌB��BʌB�XB�9B�jB��B��B�<B�BB��B��B��B��B�RB�B�B�B�NB� B�B�B��B�B�}B�B�$B��B�IB�B�IB��B��B�:B�{B�:B��B�+B�xB�'B�tB�RB��B�B��B�-B�tB��B��B�B�B�vB�}B��B�aB՛B��B� B��B��B� B��B�B�B�B�;B�B�B�B��B�vB�iB�5B�5B��B��B�cB�B�B�AB�5B� B�B�B�]B	.B	"hB	'�B	*�B	,qB	+�B	+�B	*�B	*�B	)*B	,qB	.}B	1�B	/�B	0�B	4B	8RB	<jB	<6B	;0B	?�B	DgB	DgB	FB	K�B	O�B	R�B	Y�B	\]B	e�B	h�B	j�B	i�B	e�B	e`B	d�B	gmB	h�B	k�B	l�B	n/B	m�B	m)B	m�B	rB	{JB	~(B	{B	{B	y�B	��B	��B	� B	��B	�.B	�B	��B	�MB	�B	�B	��B	�hB	��B	�:B	�@B	��B	�B	��B	��B	�+B	�=B	��B	�!B	��B	��B	�B	��B	��B	�XB	��B	��B	�0B	�kB	�B	�B	��B	�3B	��B	��B	��B	�wB	�HB	��B	��B	��B	�RB	ɺB	��B	ʌB	�<B	�B	��B	уB	�TB	��B	��B	�2B	֡B	ٴB	�B	�B	�KB	�B	ٴB	��B	��B	�]B	��B	��B	��B	�)B	��B	ܒB	�]B	��B	�/B	�B	�B	�B	ߤB	��B	�|B	��B	�B	�B	��B	�B	�B	�mB	��B	��B	�B	��B	��B	��B	�B	�WB	��B	�)B	�B	�cB	�B	��B	�B	�;B	�oB	�|B	�B	�vB	�B	��B	��B	�B	�8B	��B	�2B	��B	�lB	��B	��B	��B	�JB	��B	�(B	��B	��B	��B	��B	��B
  B
  B
 iB
 �B
 �B
 �B
 �B
 4B
  B
 �B
B
uB
�B
uB
GB
�B
�B
�B
�B
B
B
B
�B
�B
YB
_B
�B
�B
�B
�B
	B
	lB
	7B

	B
�B
�B
B
B
�B
PB
�B
�B
�B
(B
�B
.B
.B
.B
�B
hB
4B
�B
oB
�B
uB
uB
�B
FB
B
{B
B
MB
�B
SB
SB
B
YB
$B
�B
+B
�B
eB
B
�B
	B
�B
�B
�B
�B
�B
�B
�B
B
CB
�B
�B
�B
�B
VB
�B
�B
!B
�B
 �B
 �B
!�B
"hB
#:B
#�B
#�B
#�B
$@B
$@B
$�B
%B
%FB
%�B
%�B
%zB
%zB
&�B
'B
&�B
'B
'B
'B
'�B
'�B
($B
(XB
(�B
)_B
)�B
*0B
*eB
*0B
+B
+6B
+kB
+6B
+�B
-B
-B
.}B
.B
/�B
0!B
0UB
/OB
/OB
/B
/�B
0!B
0�B
1'B
1'B
1'B
1[B
2�B
3�B
3�B
3�B
3�B
49B
49B
4�B
4�B
4�B
4�B
5?B
6�B
7�B
8B
8�B
8�B
9XB
9�B
9�B
9�B
9�B
:^B
:^B
:^B
:^B
:^B
:�B
:�B
;0B
<B
<jB
<�B
=<B
=qB
>BB
>BB
>BB
>wB
>wB
>�B
>�B
>�B
?B
?HB
?}B
?}B
?}B
?�B
?�B
@B
@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�FB
��B
�tB
��B
�B
��B
��B
�B
�zB
�B
�B
��B
��B
�FB
�B
��B
�zB
�B
��B
��B
��B
�@B
�B
�FB
��B
�FB
�B
��B
�zB
��B
��B
��B
�B
�@B
�B
�zB
�B
��B
��B
��B
��B
�FB
�zB
��B
�B
�tB
�FB
�B
�nB
��B
��B
�@B
��B
�RB
��B
��B
�IB
��B
��B
��B
�nB
��B
��B
˒B
�qB
��B
ȀB
��B
�EB
�tB
��B
�B
ȴB
ȀB
��B
�NB
یB
خB
�QB
ܒB
�/B
�B
��B
�TB
�B
��BBVBOB+B1�B5�B6�B2�B4�B1�B2-B6B5B3�B9XB:�B8�B=<B<�B:*BIBJ�BR BS[BW
B[WB]dBc Bb�Ba�Bd&BgmBf�Bm)BlWBl�Bk�BqABrBp�Bs�Bu�Bv�BwfBzxB��B~�B|�B~(B�B��B�{B��B��B�DB��B�1B��B��B�OB��B�dB�[BޞB�pB�BB
�B"B�B�BFB{B%�B/�B.B)�B%FB�B!�B)�B+�B.B.�B.�B/�B1�B1'B0�B0�B2�B1�B0�B5�B49B2�B4B4B1�B3�B6zB8�B<B<6B;0B=B>�B>wBA BC-BCaBA�BC�BF?BF?BE�BIBJ�BJXBM�BOBBM�BM�BOBBOBBO�BO�BO�BPHBR�BR�BQ�BV9BYKBZQB`BBc Bb�Bc�Bj�Bo5Bt�Bs�Bw2Bx8BxBy�Bz�By>B|B|PB{�B~�B��B� B��B�B�B�AB��B��B��B�SB�_B��B��B��B��B�B�7B��B��B��B�B��B�uB�{B��B�B��B�FB��B��B��B�B��B�B�eB��B��B�-B��B��B�FB��B��B�FB��B��B��B��B��B��B�$B��B��B�$B�B�B��B��B�B��B�B��B��B��B��B�B��B�B�dB��B��B�aB��B�?B�tB�mB�tB�B�B��B�BɺB��BɺB�RB�^B˒B��B��B̘B�0B�^B��B��BбB� B�aB�EB��BیBںB��B�B��BیB��B�WB��B��B�;B�pB�TB�B�B�|BޞB�pB��B�dB�B�/B��B��BچB�dBچB��B��B�B�B��B��B�KB�
B�?B��B�9BچB�5B�#B��B��B҉B�B��B�B;B�2BҽB�vB��BΥBϫB�@B�,B�[B��B��B��B� B�B��B��BѷB�NB�pB�QB�B�`BخBخB��BܒB��B�
B��B�B�?B�RB�OB�?B� BخB��B�B��B��B��B�GB�B�B��BcB�AB~�BcB~�B|�B|BwfBw�Bv�Bv�Bp�Bm�Bl�B\�B}�B	B�BSBCB�B'RB)�B=�BD�BFB]�BZ�B^�BcTB]�B^�B`B_�B[�B^�B_B[WBYBX�Bd�BW�Bd�BOBBOBBE9BQ�BI�BA BA�BB[BP�BB�B2aB+�B)�B'�B%FB1�B
�B��BMB(�BSB4B�B+BB�PB��B �B��B�"B�!B�!B��B��B��B�$B�	B��B��B�'B��B�tB��B�*B��B��B�fB��B�$B��Bz�B� B�Bu%B�iB}"Bp;BsMB~]BqAB��B`Bm�B[#BQ�BX�BT,BS&BT�BR�B[�B>BB?�B:^B:�B1�B.}B,�B,�B)�B'�B%�B+kB/�B,=B.B-CB,�B+6B*0B.}B-�B*�B)�B+�BF?BIB �B�B1�BVB�BhB�B
�VB;B B
�5B
�B
�B
��B
�B
��B
��B
��B
�BB
��B
�B
��B
��B
��B
��B
�B
��B
�$B
��B
��B
��B
�B
�RB
�XB
��B
��B
�LB
�LB
��B
�$B
��B
��B
�RB
��B
�RB
�zB
�nB
�3B
��B
�B
�?B
�hB
��B
��B
��B
��B
�B
��B
�aB
�-B
��B
�B
��B
�B
��B
��B
��B
�B
�CB
��B
��B
��B
�6B
�6B
�CB
��B
�0B
�kB
��B
��B
��B
�_B
�$B
��B
��B
��B
��B
�*B
�B
�XB
��B
�*B
�eB
��B
�6B
��B
��B
�$B
��B
�RB
�_B
�B
��B
��B
��B
��B
�B
��B
�nB
��B
��B
�B
�!B
�B
��B
�	B
��B
�7B
��B
�+B
�B
�-B
��B
�\B
�B
�$B
��B
~�B
�4B
xB
y�B
x�B
{�B
zDB
�B
m�B
ncB
i�B
jB
j�B
j44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                             44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B
��B
�3B
��B
��B
��B
�-B
��B
�-B
�-B
��B
�-B
��B
��B
�zB
�B
�aB
��B
�ZB%zB1'B<jBW
Bd%Bn.Bx�B��B��B�%B�B"�B,qB/B2�B:�BAUBI�BM5BW>Bj�Bv�B|�B��B��B��B�tB��B��B�@B�OB��B�}B�gB��B�QB��B�>B�B�B��B�8B�sBקB�^B�BקB�B��B�@BzxBq�BA B,�BYBZ�BW�BB&B5tBYB	7B�AB�eB��B�B}VBtBY�BI�B-�B'RB)*B,<BB
�xB
��B
�^B
��B
��B
�3B
�B
�kB
�LB
��B
�9B
��B
��B
jB
d�B
\�B
O�B
0�B
#9B
�B	��B	�B	�]B	��B	�9B	��B	�=B	p�B	r�B	s�B	k�B	d�B	P�B	HKB	EmB	>�B	;dB	,�B	-BB	,�B	$�B	4B	�B	+6B	�B	YB	B	�B	�B	B	�B	�B	�B	uB	 iB��B�B�B�cBںB�B��B�
B�yB��B�B��BƨB��B��B�#B�B��B��B�B�HB�?B�EBŢB�QB�^B�jB͞B�pB�WB�UB�B�jB��B�XB�tB��B��B�hB��B�.B��B��B��B��B��B�{B��B�wB��B��B�$B�kB��B�}B��B�&B�KB�WB�^B��B��B�<BбB��B�HB�pB�B�B�pB�>B��B��B��B�B��B��B��B�(B��B�B�B�B�B�B�B��B��B�B�B�PB�iB�iB��B	~B	�B	$@B	'B	(�B	($B	'�B	'B	&�B	%zB	(�B	*�B	.B	,<B	,�B	0UB	4�B	8�B	8�B	7�B	<B	@�B	@�B	B[B	HKB	K�B	OB	V8B	X�B	bB	e,B	gB	e�B	a�B	a�B	aGB	c�B	d�B	h
B	iDB	jB	jJB	iyB	jJB	ncB	w�B	zxB	w�B	wfB	u�B	�4B	�	B	�PB	��B	�~B	�bB	�4B	��B	�nB	�hB	��B	��B	��B	��B	��B	��B	�\B	�:B	�:B	�{B	��B	�B	�qB	�CB	�!B	�UB	�-B	�B	��B	�B	��B	��B	��B	�^B	�kB	��B	��B	��B	��B	��B	��B	��B	��B	�,B	�9B	ŢB	�
B	�?B	��B	ʌB	�jB	�<B	��B	ΤB	�B	�NB	тB	��B	�B	�gB	�gB	՛B	��B	�B	�8B	�B	حB	�KB	�KB	�KB	�yB	�B	��B	حB	�KB	�B	�QB	�WB	�WB	��B	�/B	��B	�;B	�B	��B	�NB	�TB	�TB	�B	�,B	�,B	��B	�2B	�8B	�8B	�lB	�B	�DB	�yB	��B	�B	��B	�"B	�]B	�B	�B	��B	��B	��B	�iB	�B	��B	�SB	�B	��B	�B	��B	��B	��B	�+B	�+B	��B	��B	�xB	�B	��B	��B	��B	��B	�PB	�PB	��B	��B	��B	��B	��B	��B	�PB	�"B	�\B	��B	��B	��B	��B
 4B
 �B
:B
:B
oB
oB
oB
:B
B
�B
�B
MB
B
B
B
SB
�B
�B
YB
�B
1B
eB
eB
	7B
	�B

	B

=B
B
xB
B
~B
~B
~B
IB
�B
�B
�B
�B
�B
�B
�B
.B
�B
bB
�B
hB
�B
:B
�B
�B
nB
�B
tB
�B
{B
B
�B
RB
$B
YB
*B
�B
�B
*B
*B
*B
*B
_B
�B
B
B
B
B
�B
B
�B
qB
B
IB
IB
OB
�B
�B
�B
 'B
�B
 �B
 �B
 �B
!bB
!�B
!�B
!�B
!�B
!�B
"�B
#nB
#B
#nB
#nB
#nB
#�B
$B
$tB
$�B
%FB
%�B
&B
&�B
&�B
&�B
'RB
'�B
'�B
'�B
'�B
)^B
)^B
*�B
*dB
+�B
,qB
,�B
+�B
+�B
+kB
+�B
,qB
-B
-wB
-wB
-wB
-�B
/OB
0 B
/�B
/�B
0 B
0�B
0�B
0�B
1'B
1'B
1'B
1�B
2�B
4B
4mB
4�B
5B
5�B
5�B
6EB
6EB
6EB
6�B
6�B
6�B
6�B
6�B
7B
7B
7�B
8RB
8�B
8�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;dB
;�B
;�B
;�B
;�B
<B
<6B
<jB
<jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�3B
��B
��B
��B
�3B
�hB
�-B
�B
�[B
��B
�[B
�[B
�B
�'B
��B
�nB
�@B
��B
�nB
��B
�-B
��B
��B
�[B
��B
��B
��B
�hB
��B
��B
��B
��B
��B
�bB
��B
�bB
��B
�[B
��B
�'B
�-B
�B
��B
��B
�3B
�[B
��B
��B
�UB
��B
��B
�-B
��B
�@B
��B
�-B
�3B
��B
�0B
��B
��B
��B
�#B
��B
��B
��B
�<B
��B
�&B
ÕB
��B
�9B
�UB
�B
��B
�B
͞B
��B
��B
֡B
��B
�B
��B
�5B
ߤB
�`B
�5B
�\B
�B�B'RB.B1�B33B.�B0�B-�B.}B2aB1[B0 B5�B7KB4�B9�B8�B6zBEmBGEBNpBO�BSZBW�BY�B_pB_B^B`vBc�BcBiyBh�Bh�Bh>Bm�BncBm(Bp;BrGBsMBs�Bv�B~�B{By>BzxB~\B|�B�B�B�4B��B��B��B�@B��B��B�BȴBϫB��B��B  BoB+B
rB�BB�B�B!�B,<B*dB&LB!�BBOB%�B'�B*dB+6B+6B,<B.IB-wB-B,�B.�B-�B,�B2-B0�B/B0UB0UB.IB0 B2�B4�B8RB8�B7�B9XB:�B:�B=pB?}B?�B>B@BB�BB�BA�BEmBF�BF�BJ#BK�BI�BJ#BK�BK�BK�BL/BL/BL�BOBBOBBNBR�BU�BV�B\�B_pB_;B`ABg8Bk�Bp�BpBs�Bt�BtSBu�Bv�Bu�BxlBx�BxB{B}"B|PB}"BbBbB~�B�B�@B�B��B��B�GB�@B�MB��B�SB��B�7B�B�!B�\B��B��B��B��B�nB�@B��B��B��B�B�nB�LB�RB��B��B��B�}B��B�B��B��B��B��B�9B�B�3B��B�B�3B�tB�B��B�tB�^B�kB��B�BB�[B��B�gB��B�B�)B�#B�RB��B�XB��B�0B� B��B�NBB��B��B��B�gB�mB�9B�gB�
B�EB�
BŢBǮB��B�EB�B��BȀBǮB�B�#B�B�pBбBԕB�,B��B�
B�B�mB�8B��B�BקB�2B�BۋB��BߤB��B��B��B��B��B�)BٴB�QB�B�#B�B��BٴB��B�8B�>B�mB�`B�&B� B՛B�ZBӏB�8B҉B��BڅB�sB�&B�#B��B�WB�B�B��BтB�B��B�8B��B��B��B�|BϫB�NB�<B�B�pB��B�&B�B�B͞B��B֡B�WB�B��B��B�2B��B�,B�ZB�8B�fBӏBŢB��BB�pB��B�B�nB�B��B�B�B~\B}VB}"B{�B~�Bz�B{�B{Bx�BxlBs�BtBsMBsBm(Bi�Bh�BX�By�BYBFB�B�BB#�B&B:)B@�BB[BZBW
BZ�B_�BZB[#B\]B\)BXBZ�B[WBW�BU�BT�BaGBT,B`�BK�BK�BA�BNBE�B=pB>BB>�BM5B?HB.�B'�B%�B#�B!�B-�B�B��B�B%B�B�B:B{B�bB��B�B��B�"B�rB�qB�qB�B�!B�B�tB�YB�$B�B�wB��B��B�B�zB�OB��B��B�MB�tB�=Bv�B|PB�oBquB|�ByrBl�Bo�Bz�Bm�B��B\]BjJBWsBNBT�BP|BOvBQBOBW�B:�B<6B6�B7B.B*�B)*B(�B&B$@B!�B'�B,B(�B*dB)�B)*B'�B&�B*�B*0B&�B&LB($BB�B�B�B7B-�B�BLB�B �B
��B
��BPB
�B
��B
��B
�B
�iB
�9B
�B
�0B
��B
��B
�^B
��B
�)B
�#B
�<B
�mB
�KB
�tB
�B
�9B
��B
�mB
��B
��B
�9B
�9B
��B
��B
�EB
�tB
�?B
��B
��B
�9B
��B
��B
��B
��B
��B
�UB
��B
��B
��B
�B
�B
�B
�aB
�OB
��B
�}B
�BB
�UB
��B
�[B
�B
�0B
��B
�kB
��B
��B
�0B
�*B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
�tB
��B
��B
��B
��B
�zB
�nB
��B
��B
�zB
��B
�9B
��B
�9B
��B
�tB
��B
��B
��B
�nB
�9B
��B
�B
�9B
�hB
�-B
��B
��B
��B
�nB
�qB
�kB
�$B
�YB
�$B
��B
�B
�{B
�bB
�}B
��B
��B
�RB
�tB
�B
{JB
|�B
tSB
u�B
u%B
xB
v�B
~\B
jB
j�B
f2B
ffB
gB
f�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                             44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224953                            20230721224953AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495320230721224953  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495320230721224953QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495320230721224953QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             