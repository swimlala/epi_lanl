CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:58Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  P�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  j\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  o�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ̀   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  (   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   !0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T '0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   '�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   '�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   '�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   '�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � '�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ($   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   (@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    (H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        (h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        (p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    (�Argo profile    3.1 1.2 19500101000000  20230721224958  20230721224958  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�$��@�$��11  @�$O��`@�$O��`@3��@3���d�Y��|��d�Y��|�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?�  @�\@B�\@�  @�p�@��R@�G�AG�A  A   A*�HA?\)A`  A���A�Q�A��A��A�Q�AУ�A߮A�  B (�B  B�B  B Q�B((�B/�
B8  B@  BH  BP  BX  B`  Bg�Bo�
Bx(�B�  B��B��B�  B�{B�{B��
B��B��B��B�  B�  B��B�  B�  B��B��B��
B�{B��B��B�  B��B��
B��B�(�B�  B��B�  B�{B�  B��B��
C��C��C
=C
=C
  C{C{C
=C��C�HC�C��C��C  C
=C   C!�HC#��C%��C(
=C*  C,{C-��C/��C1�C3��C6  C8
=C:{C<
=C>  C?��CB  CC��CE�CG��CJ
=CL{CN
=CP{CR  CT  CU��CW��CZ
=C\  C^  C`
=Ca��Cd
=Cf  Ch
=Cj
=Cl
=Cm��Cp  Cr  Cs��Cv
=Cw��Cy��C|  C~
=C�C�C�  C���C���C���C���C���C�C�C�C�C���C�  C���C���C�  C�  C��C���C�  C�  C�  C�  C���C���C�C�C�  C�  C�  C�  C�  C�
=C�C�  C�  C�  C�  C�C�
=C�C���C���C�C���C�  C�  C���C�C�C�  C���C���C�  C�  C�  C�  C���C�  C�
=C�C�C�  C�  C�C�
=C�
=C�  C�  C�C�  C���C���C���C���C�  C�C�  C�C�C�
=C�C�C�  C�  C�C�  C���C���C���C�C�C�  C���C�  C�C�  C�C�C���C���C�  C���C���C�  C���C�C�C�  C�  C���C���C���C���C���C�  C�C�
=C�C���C���C�  C�C�  C�C�C�  D   D � DD�D�D� D�D��D  D� D�D� D�qD� D  D� D  D� D	  D	��D
�D
� D
�qD� D�qDz�D��D}qD�qD}qD  D� D�qD� D  D}qD�qD}qD  D��D  D}qD�qD� D�D� D�qD}qD��D}qD�D�D�D}qD  D��D�D��D�D��D  D}qD�D� D�qD }qD!  D!��D"  D"� D#  D#��D$  D$}qD$�qD%� D&�D&� D'  D'��D(�D(��D)  D)}qD*  D*� D*�qD+� D,�D,��D-�D-� D.�D.� D.�qD/� D0�D0�D1�D1}qD1��D2��D3�D3��D4  D4� D5D5�D6�D6� D6�qD7��D8  D8��D9D9}qD9��D:}qD:�qD;}qD<�D<��D=D=��D>  D>� D>�qD?� D@�D@��DA  DA� DB�DB��DC�DC� DD  DD� DE�DE}qDF  DF��DF�qDGz�DG��DH��DIDI��DJ�DJ� DJ��DK}qDK�qDL}qDL�qDMz�DN  DN�DO�DO� DP  DP}qDQ  DQ� DQ�qDR��DS  DS}qDS�qDT��DT�qDUz�DU�qDV}qDW  DW��DX  DX}qDX�qDY� DZ  DZ� D[�D[�D\  D\}qD]  D]��D^�D^}qD^�qD_}qD`  D`��Da  Da� Db  Db}qDb�qDc� Dd�Dd��De�De}qDe�qDf��Dg  Dg� Dh�Dh}qDh��Di� DjDj� Dk�Dk}qDk��Dl��Dm�Dm��Dn�Dn��Do�Do� Do�qDpz�Dq  Dq� Dq�qDr}qDs  Ds� Dt�Dt� Dt��Du��Dv  Dv� DwDw��Dx  Dx}qDy  Dy� Dz  Dz��D{D{� D|  D|��D}�D}� D~�D~��D~�qD� D�  D�>�D�� D�� D���D�>�D��HD���D��qD�@ D���D�D�HD�@ D�� D�� D���D�>�D��HD�� D���D�@ D��HD��HD�  D�@ D�~�D�� D�HD�AHD�~�D���D�  D�@ D�� D�� D��qD�>�D�~�D���D���D�>�D��HD��HD�  D�=qD�~�D���D���D�@ D�� D��HD�HD�@ D��HD��HD�HD�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD�� D�D��D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D�� D���D�@ D��HD�� D�HD�AHD�� D��HD�  D�@ D���D��HD�HD�AHD�� D��HD�HD�AHD��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?��?.{?�\)?���?Ǯ?�@�@
=@+�@=p�@G�@\(�@p��@}p�@���@�33@�(�@��
@���@�Q�@�  @�{@�@޸R@�=q@�\)@���A�\A�A
=qA��Az�A��Ap�A#33A&ffA*�HA1G�A3�
A8Q�A>�RAC33AG
=AL��AP��ATz�AZ=qA]p�Aa�Ag�Al(�Ao\)As�
Ay��A~{A���A��A�{A�\)A�=qA���A��RA���A��
A�p�A�\)A��\A�(�A�{A�G�A�33A��A�  A��A��A�
=A���A��\A�p�A�  A�G�A��
A��RA�  A��HA�p�A�
=Aə�A�z�A�{A�  A�33A��A�
=A��A�(�A�A��A�=qA�(�A�
=A�G�A�33A�A�Q�A��A�z�A�
=A���A��HA�p�A�\)B z�B�B
=B�
B��B=qB�HBQ�B	��B
=qB\)B��Bp�BffB�
B��B��B�RB  B��BB
=B  B��B=qB33B  Bp�BffB
=B Q�B!p�B"{B#\)B$z�B%G�B&{B'\)B(Q�B(��B)�B+33B,  B,��B-�B.�RB/33B0z�B1G�B1�B333B4  B4��B5B6�HB7�B8Q�B9G�B:ffB;
=B<  B=G�B=�B>�HB@(�B@��BABC
=BD(�BD��BF{BG\)BH(�BIG�BJ�RBK�BLz�BM��BN�HBO�
BP��BQ�BS33BT(�BT��BVffBW�BX(�BY��BZ�HB[�
B\��B^=qB_�B`Q�Ba��Bc
=Bd  Be�Bf=qBg�Bh��BiBj�\Bl(�Bmp�BnffBo\)Bp��Br{Br�HBtQ�Bu��Bv�\Bw�By�BzffB{\)B|��B~=qB
=B�{B��RB��B�{B��\B�\)B�  B�ffB�33B�  B�ffB���B��
B��\B�
=B��B�ffB���B��B�=qB�
=B��B�(�B��HB���B�  B��RB�p�B��B��\B�\)B��
B�ffB�33B��
B�Q�B��B��
B�ffB��HB��B�z�B��B��B�Q�B��B��
B�Q�B��B��B�z�B�
=B��
B�z�B�
=B��B�z�B���B��
B��\B��B�B�z�B�\)B�  B��\B�G�B�(�B���B�\)B�{B���B���B�(�B��HB��B�Q�B��HB��B�z�B��B��B�=qB�
=B�B�Q�B��HB�B�Q�B���B���B�Q�B��HB��B�Q�B���B�\)B�(�B��RB�33B�  B�z�B�
=B��
B�z�B���BŮB�z�B�33BǙ�B�Q�B��BɮB�(�B���B�B�=qB��HBͮB�ffB���BυB�Q�B���B�p�B�(�B���BӅB�{BԸRBՙ�B�{B���Bי�B�(�Bأ�B�\)B�{Bڏ\B�G�B�  B�z�B�
=B�B�ffB���B�\)B�(�B�RB�33B��
B��B��B㙚B�=qB���B�B��B��B�\)B��B�ffB���B�B�Q�B�RB�\)B�{B�RB�\)B�B�ffB�
=B��
B�ffB���B�B�=qB�RB�\)B�  B���B�
=B���B�(�B��HB��B�  B�z�B��B�B�Q�B��HB�G�B�  B��RB�G�B��B�=qB���B�p�C 
=C Q�C �C �
C(�Cz�CC  CG�C��C�C(�CffC�C  C=qCz�CC�CffC�RC�C33Cz�C��C(�Cp�C�RC�C(�Cz�C��C	{C	Q�C	�\C	�
C
33C
p�C
�C
�C33C�C�
C�CQ�C��C�
C�CffCC{C\)C�\C�
C{CffC�RC  C33Cz�C��C�Cp�C�RC�C=qC�\C�HC(�Cp�C�C��C\)C�C  CG�C��C�
C{CffC�RC
=C\)C�RC��C=qC�C�HC33C�\C�
C{C\)C�RC{CffC�RC
=CQ�C��C�C=qC��C�C=qC�C��C{CffC�RC {C p�C �RC!  C!G�C!��C!��C"Q�C"��C#  C#Q�C#��C#��C$Q�C$�\C$�HC%33C%�\C%�HC&33C&�\C&�HC'=qC'��C'��C(Q�C(��C(��C)=qC)�C)�HC*G�C*��C*��C+Q�C+�C,  C,\)C,��C,�C-=qC-�\C-�HC.=qC.�\C.�HC/=qC/�\C/�HC0=qC0�C0�
C1(�C1z�C1��C2(�C2z�C2�HC333C3�\C3��C4Q�C4�C5
=C5\)C5�C6  C6Q�C6��C6��C7Q�C7�C8
=C8ffC8��C9(�C9�C9�HC:G�C:��C;  C;Q�C;�C<{C<p�C<��C=(�C=z�C=�
C>33C>�C>�HC?=qC?��C?��C@\)C@�CA{CAffCA��CB33CB�\CB��CCQ�CC��CD
=CDp�CD��CE33CE�\CE�CFQ�CF�CG
=CG\)CG�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @�\@B�\@�  @�p�@��R@�G�AG�A  A   A*�HA?\)A`  A���A�Q�A��A��A�Q�AУ�A߮A�  B (�B  B�B  B Q�B((�B/�
B8  B@  BH  BP  BX  B`  Bg�Bo�
Bx(�B�  B��B��B�  B�{B�{B��
B��B��B��B�  B�  B��B�  B�  B��B��B��
B�{B��B��B�  B��B��
B��B�(�B�  B��B�  B�{B�  B��B��
C��C��C
=C
=C
  C{C{C
=C��C�HC�C��C��C  C
=C   C!�HC#��C%��C(
=C*  C,{C-��C/��C1�C3��C6  C8
=C:{C<
=C>  C?��CB  CC��CE�CG��CJ
=CL{CN
=CP{CR  CT  CU��CW��CZ
=C\  C^  C`
=Ca��Cd
=Cf  Ch
=Cj
=Cl
=Cm��Cp  Cr  Cs��Cv
=Cw��Cy��C|  C~
=C�C�C�  C���C���C���C���C���C�C�C�C�C���C�  C���C���C�  C�  C��C���C�  C�  C�  C�  C���C���C�C�C�  C�  C�  C�  C�  C�
=C�C�  C�  C�  C�  C�C�
=C�C���C���C�C���C�  C�  C���C�C�C�  C���C���C�  C�  C�  C�  C���C�  C�
=C�C�C�  C�  C�C�
=C�
=C�  C�  C�C�  C���C���C���C���C�  C�C�  C�C�C�
=C�C�C�  C�  C�C�  C���C���C���C�C�C�  C���C�  C�C�  C�C�C���C���C�  C���C���C�  C���C�C�C�  C�  C���C���C���C���C���C�  C�C�
=C�C���C���C�  C�C�  C�C�C�  D   D � DD�D�D� D�D��D  D� D�D� D�qD� D  D� D  D� D	  D	��D
�D
� D
�qD� D�qDz�D��D}qD�qD}qD  D� D�qD� D  D}qD�qD}qD  D��D  D}qD�qD� D�D� D�qD}qD��D}qD�D�D�D}qD  D��D�D��D�D��D  D}qD�D� D�qD }qD!  D!��D"  D"� D#  D#��D$  D$}qD$�qD%� D&�D&� D'  D'��D(�D(��D)  D)}qD*  D*� D*�qD+� D,�D,��D-�D-� D.�D.� D.�qD/� D0�D0�D1�D1}qD1��D2��D3�D3��D4  D4� D5D5�D6�D6� D6�qD7��D8  D8��D9D9}qD9��D:}qD:�qD;}qD<�D<��D=D=��D>  D>� D>�qD?� D@�D@��DA  DA� DB�DB��DC�DC� DD  DD� DE�DE}qDF  DF��DF�qDGz�DG��DH��DIDI��DJ�DJ� DJ��DK}qDK�qDL}qDL�qDMz�DN  DN�DO�DO� DP  DP}qDQ  DQ� DQ�qDR��DS  DS}qDS�qDT��DT�qDUz�DU�qDV}qDW  DW��DX  DX}qDX�qDY� DZ  DZ� D[�D[�D\  D\}qD]  D]��D^�D^}qD^�qD_}qD`  D`��Da  Da� Db  Db}qDb�qDc� Dd�Dd��De�De}qDe�qDf��Dg  Dg� Dh�Dh}qDh��Di� DjDj� Dk�Dk}qDk��Dl��Dm�Dm��Dn�Dn��Do�Do� Do�qDpz�Dq  Dq� Dq�qDr}qDs  Ds� Dt�Dt� Dt��Du��Dv  Dv� DwDw��Dx  Dx}qDy  Dy� Dz  Dz��D{D{� D|  D|��D}�D}� D~�D~��D~�qD� D�  D�>�D�� D�� D���D�>�D��HD���D��qD�@ D���D�D�HD�@ D�� D�� D���D�>�D��HD�� D���D�@ D��HD��HD�  D�@ D�~�D�� D�HD�AHD�~�D���D�  D�@ D�� D�� D��qD�>�D�~�D���D���D�>�D��HD��HD�  D�=qD�~�D���D���D�@ D�� D��HD�HD�@ D��HD��HD�HD�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD�� D�D��D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D�� D���D�@ D��HD�� D�HD�AHD�� D��HD�  D�@ D���D��HD�HD�AHD�� D��HD�HD�AHD��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?��?.{?�\)?���?Ǯ?�@�@
=@+�@=p�@G�@\(�@p��@}p�@���@�33@�(�@��
@���@�Q�@�  @�{@�@޸R@�=q@�\)@���A�\A�A
=qA��Az�A��Ap�A#33A&ffA*�HA1G�A3�
A8Q�A>�RAC33AG
=AL��AP��ATz�AZ=qA]p�Aa�Ag�Al(�Ao\)As�
Ay��A~{A���A��A�{A�\)A�=qA���A��RA���A��
A�p�A�\)A��\A�(�A�{A�G�A�33A��A�  A��A��A�
=A���A��\A�p�A�  A�G�A��
A��RA�  A��HA�p�A�
=Aə�A�z�A�{A�  A�33A��A�
=A��A�(�A�A��A�=qA�(�A�
=A�G�A�33A�A�Q�A��A�z�A�
=A���A��HA�p�A�\)B z�B�B
=B�
B��B=qB�HBQ�B	��B
=qB\)B��Bp�BffB�
B��B��B�RB  B��BB
=B  B��B=qB33B  Bp�BffB
=B Q�B!p�B"{B#\)B$z�B%G�B&{B'\)B(Q�B(��B)�B+33B,  B,��B-�B.�RB/33B0z�B1G�B1�B333B4  B4��B5B6�HB7�B8Q�B9G�B:ffB;
=B<  B=G�B=�B>�HB@(�B@��BABC
=BD(�BD��BF{BG\)BH(�BIG�BJ�RBK�BLz�BM��BN�HBO�
BP��BQ�BS33BT(�BT��BVffBW�BX(�BY��BZ�HB[�
B\��B^=qB_�B`Q�Ba��Bc
=Bd  Be�Bf=qBg�Bh��BiBj�\Bl(�Bmp�BnffBo\)Bp��Br{Br�HBtQ�Bu��Bv�\Bw�By�BzffB{\)B|��B~=qB
=B�{B��RB��B�{B��\B�\)B�  B�ffB�33B�  B�ffB���B��
B��\B�
=B��B�ffB���B��B�=qB�
=B��B�(�B��HB���B�  B��RB�p�B��B��\B�\)B��
B�ffB�33B��
B�Q�B��B��
B�ffB��HB��B�z�B��B��B�Q�B��B��
B�Q�B��B��B�z�B�
=B��
B�z�B�
=B��B�z�B���B��
B��\B��B�B�z�B�\)B�  B��\B�G�B�(�B���B�\)B�{B���B���B�(�B��HB��B�Q�B��HB��B�z�B��B��B�=qB�
=B�B�Q�B��HB�B�Q�B���B���B�Q�B��HB��B�Q�B���B�\)B�(�B��RB�33B�  B�z�B�
=B��
B�z�B���BŮB�z�B�33BǙ�B�Q�B��BɮB�(�B���B�B�=qB��HBͮB�ffB���BυB�Q�B���B�p�B�(�B���BӅB�{BԸRBՙ�B�{B���Bי�B�(�Bأ�B�\)B�{Bڏ\B�G�B�  B�z�B�
=B�B�ffB���B�\)B�(�B�RB�33B��
B��B��B㙚B�=qB���B�B��B��B�\)B��B�ffB���B�B�Q�B�RB�\)B�{B�RB�\)B�B�ffB�
=B��
B�ffB���B�B�=qB�RB�\)B�  B���B�
=B���B�(�B��HB��B�  B�z�B��B�B�Q�B��HB�G�B�  B��RB�G�B��B�=qB���B�p�C 
=C Q�C �C �
C(�Cz�CC  CG�C��C�C(�CffC�C  C=qCz�CC�CffC�RC�C33Cz�C��C(�Cp�C�RC�C(�Cz�C��C	{C	Q�C	�\C	�
C
33C
p�C
�C
�C33C�C�
C�CQ�C��C�
C�CffCC{C\)C�\C�
C{CffC�RC  C33Cz�C��C�Cp�C�RC�C=qC�\C�HC(�Cp�C�C��C\)C�C  CG�C��C�
C{CffC�RC
=C\)C�RC��C=qC�C�HC33C�\C�
C{C\)C�RC{CffC�RC
=CQ�C��C�C=qC��C�C=qC�C��C{CffC�RC {C p�C �RC!  C!G�C!��C!��C"Q�C"��C#  C#Q�C#��C#��C$Q�C$�\C$�HC%33C%�\C%�HC&33C&�\C&�HC'=qC'��C'��C(Q�C(��C(��C)=qC)�C)�HC*G�C*��C*��C+Q�C+�C,  C,\)C,��C,�C-=qC-�\C-�HC.=qC.�\C.�HC/=qC/�\C/�HC0=qC0�C0�
C1(�C1z�C1��C2(�C2z�C2�HC333C3�\C3��C4Q�C4�C5
=C5\)C5�C6  C6Q�C6��C6��C7Q�C7�C8
=C8ffC8��C9(�C9�C9�HC:G�C:��C;  C;Q�C;�C<{C<p�C<��C=(�C=z�C=�
C>33C>�C>�HC?=qC?��C?��C@\)C@�CA{CAffCA��CB33CB�\CB��CCQ�CC��CD
=CDp�CD��CE33CE�\CE�CFQ�CF�CG
=CG\)CG�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�{A�{A� �A��A� �A�(�A��A��A�{A��/AѾwAљ�A�G�A�+A�"�A��A�oA�
=A�A���A���A��A��A��mA��`A���Aк^AЇ+Aϣ�A�;dÁA˺^A��A��#A�9XA���AȾwA�K�A���AǁA�+A�  Aƴ9A�jA�(�A��ADA��A��hA��+A�|�A�ffA�;dA�ffA���A�Q�A�5?A�ȴA�n�A���A�9XA�9XA�A���A�7LA���A��A�z�A�7LA��DA��/A���A��A��+A�JA���A��TA��FA�A�r�A��A�;dA���A�7LA���A��FA�K�A�oA�  A�O�A�{A�bNA���A��A{%AwXAv~�AsVAo��Am�mAl��AkG�Aj�/Ajr�Ai�TAh�uAf=qAd~�Ad{Ac��Ab(�A^ffA\1'AX�/AW?}AU�TAQ��AO�#AM�TAJ�RAI�-AI
=AGoAE`BAD9XAB��AA?}A?�A=XA;�-A:JA8�A5��A3�A2A�A/��A-�TA-S�A,�A+A)��A'�A%��A$-A"��A!�;A jA`BA��A-A~�A��A��An�AI�AM�A�A�AoA33A7LA��A�A��A�hA;dA�A��AjA�A�A�RAjAQ�A �Ap�A��A �A
��A�jAJA�A~�A�
A`BA9XA�AI�A�FAO�A �jA Q�@�E�@�Q�@�p�@�ff@�l�@�E�@�V@�bN@�b@�F@�t�@�R@���@�  @�M�@���@���@�j@� �@�@���@�n�@�{@�@��@畁@�  @��@�j@���@��@�X@�O�@��@���@���@�;d@��@��@�E�@�v�@�=q@��;@�+@޸R@�&�@��@ᙚ@�;d@ݙ�@���@�
=@�b@���@ם�@�-@ա�@�G�@���@�K�@�=q@��@���@�Ĝ@υ@�/@���@̣�@��@˥�@�S�@�33@ʟ�@ɑh@�O�@�X@�X@Ȭ@Ȭ@��@ȋD@Ǯ@�ff@ʗ�@ʧ�@�S�@ˍP@�{@�V@��@��@���@�G�@�Q�@��;@ǶF@ǍP@�K�@���@�=q@���@ź^@�`B@��@���@�1'@Ý�@�V@���@��h@�O�@��@��/@�Q�@���@�ȴ@�n�@�^5@�V@�E�@��^@�V@�bN@�1'@�1@��@���@�ƨ@��F@���@�;d@��y@�n�@��-@��@��@�  @�t�@��@�E�@�-@�=q@��@�@���@�hs@��@���@��9@��D@�A�@��@�
=@��!@��+@�J@���@��7@�hs@�/@���@��@��m@���@�v�@��#@��^@��@�7L@�&�@��@�9X@���@���@��@��P@�l�@�"�@��y@�ȴ@���@�v�@��@��T@���@��h@�7L@���@���@�z�@���@�|�@�l�@�;d@��y@�E�@��@���@��^@�p�@�%@�j@��@��m@��
@�ƨ@��@�|�@�K�@��R@��@�G�@�%@��`@�Ĝ@��9@���@�bN@��
@�@���@�n�@�^5@�=q@�{@�J@���@��-@�X@�G�@��D@�Q�@�A�@��w@�K�@��H@���@��\@�~�@�ff@�{@��T@��^@�p�@�7L@��`@���@��D@�A�@��;@���@���@�dZ@�;d@�;d@�+@��@��\@�E�@��@���@���@��-@�X@���@���@�j@�ƨ@�l�@�l�@�l�@�\)@�K�@�;d@�ȴ@�n�@��@���@��^@��@���@���@�r�@�I�@�1'@��@�ƨ@���@��@��R@�^5@�=q@�5?@�{@��T@��7@�/@���@���@��/@��j@��@��D@�r�@�Q�@��@��;@���@�\)@�+@���@�ff@�=q@�{@���@��T@��#@�@�x�@���@��@��F@��@�dZ@�S�@�dZ@�;d@�33@��@��H@��@��#@���@�`B@�X@�X@�?}@���@��@�Q�@�A�@�b@�ƨ@�t�@�S�@�o@��y@���@���@�~�@�^5@�M�@��@��@��-@�p�@�O�@��@��/@���@��9@�1'@�1@�  @�w@�@l�@~��@~v�@}�@}�h@}O�@}�@|�j@{��@{@z�!@zn�@z-@zJ@y��@y7L@xbN@x �@wl�@vȴ@u@u?}@tZ@s�
@sS�@r�!@q��@p��@pQ�@o�;@o�w@oK�@nȴ@nV@n$�@m�@m@m�@m?}@l�@l�D@lI�@l1@k�
@kƨ@k�F@k��@k��@k�@k33@j~�@i��@iX@h�`@hQ�@gK�@fȴ@f�R@f��@f��@fff@e@eO�@d�/@d�D@dZ@d�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�oA��A�oA�bA�oA�oA�{A��A��A� �A� �A� �A��A��A��A� �A��A�&�A�(�A�&�A�-A�$�A�{A��A�VA�oA��A��A�{A��A�oA�JA��#A�ƨA���AѼjA�ĜA���A���AѓuAсA�n�A�t�A�^5A�M�A�XA�K�A�K�A�=qA�9XA�1'A�-A�-A�+A�&�A�(�A�+A�$�A�&�A�(�A�"�A��A�"�A��A��A� �A��A��A��A��A�{A��A�{A�bA�{A�oA�VA�oA�bA�
=A�JA�JA�1A�1A�
=A�%A�A�1A�A�  A�A�A���A�A���A���A���A���A���A���A���A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��yA��A��A��yA��mA��A��mA��`A��yA��mA��TA��mA��yA��mA��`A��A��mA��HA��`A��HA��#A��#A���A�ȴA���A���A�ȴA���A���A�ȴAоwAоwAк^Aд9Aв-Aв-AЬAХ�AХ�AП�AЏ\AЉ7A�|�A�bNA�VA�A�A�(�A�VA��`Aϕ�Aχ+A�Q�A�G�A�;dA�/A�%A��
AΟ�A�
=AͶFAͥ�A͝�A�|�A�+A��TA�hsA�jA�$�A���A���A��A��A��/A�ȴA˴9A˝�A�r�A�`BA�I�A�(�A��A�bA�JA�JA��Aʴ9A��AɬAɏ\AɃA�l�A�VA�S�A�S�A�C�A�+A��A��A�{A�bA�
=A���A��A��yA��
A���A���A�ƨA���A���AȸRAȥ�Aȕ�A�|�A�ZA�;dA�"�A��A��A�JA���A���A��A��;A���AǺ^Aǟ�A�|�A�p�A�ZA�K�A�;dA�33A�+A�$�A�$�A��A�{A�VA�%A���A���A���A��A��`A��;A���A�ĜAƝ�A�Aţ�A�x�A�\)A�K�A�A�A�?}A�=qA�7LA�&�A��A��A�AľwA�1'A��A��A��#AËDA�(�A��;A�ƨA�r�A���A��hA��TA���A�bA��TA���A��jA���A��uA���A��7A��A��A��+A��+A��A��A��7A��+A��A�~�A�|�A�x�A�r�A�l�A�l�A�jA�bNA�^5A�\)A�S�A�K�A�A�A�;dA�-A��A�A��mA�A��A��A��!A�|�A�VA�33A�
=A��/A��hA�VA�5?A���A�hsA�1'A���A���A�v�A�Q�A�M�A��RA�l�A�ƨA���A�\)A��!A�z�A�E�A��A���A��A���A�bNA���A��;A���A��wA��-A���A��A�t�A�bNA�I�A�C�A�;dA�/A�(�A�%A��TA�|�A�C�A�$�A�VA���A���A�Q�A��-A�"�A�ĜA�|�A�oA��A�|�A�jA�;dA�XA�/A���A���A�n�A�Q�A�=qA�bA��
A���A��FA�1'A���A�dZA�A���A�jA�C�A�7LA�"�A�bA���A���A��jA���A���A�|�A�hsA�Q�A��A��mA�?}A�1'A��A�VA��`A���A�ĜA��!A���A��uA�n�A�VA�=qA�{A�
=A���A��mA��jA���A��A�jA�O�A�+A�  A��mA��jA�XA�1A��-A�VA��A�ĜA��RA���A�x�A�9XA�A���A�?}A�ƨA���A��hA��A�Q�A�(�A�
=A���A���A�A�A�VA���A��TA��/A���A��9A���A�ZA�-A�VA��A���A���A��PA�~�A�r�A��A�ȴA���A�S�A��FA��A�O�A�1'A��A�ȴA���A���A�1'A��;A�x�A�;dA�1A���A�t�A��A�7LA���A��A��uA��\A��DA��7A��A�z�A�ffA�A�1'A��A��A�bA�bNA��FA�n�A��RA��A�9XA�(�A�oA���A��A���A��^A��-A���A���A�jA�bA�I�A���A�z�A�VA���A�dZA��TA�p�A�7LA�JA�x�A��`A��PA�XA�
=A�A��DA�n�A�ffA�ZA�A��HA��-A�ffA��A��mA�A�A��TA��9A��DA�ZA�7LA��A���A��;A���A��FA���A�|�A�G�A�{A���A�{A��-A��PA�~�A�p�A�`BA�XA�E�A�9XA�bA��;A���A�mA|n�Az�Ax�/Ax�Aw��Aw�-Awp�AwC�Aw&�AwVAv�Av�`Av��Av��Avz�Av1'Au+At-AsC�Ar��Ar�DArffAq�#Ap�Ao�#Ao+AnȴAn��An�DAnZAm�
Am�AmO�Am+Am
=Al�/Al�jAl�DAl{AkdZAkC�Ak;dAk33Ak"�Ak%Aj��Aj�/Aj��Aj��Aj�Ajv�Ajn�Ajn�Ajn�AjffAjI�Aj(�Ai�TAi��Aix�AiC�Ai/Aio1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{A�{A� �A��A� �A�(�A��A��A�{A��/AѾwAљ�A�G�A�+A�"�A��A�oA�
=A�A���A���A��A��A��mA��`A���Aк^AЇ+Aϣ�A�;dÁA˺^A��A��#A�9XA���AȾwA�K�A���AǁA�+A�  Aƴ9A�jA�(�A��ADA��A��hA��+A�|�A�ffA�;dA�ffA���A�Q�A�5?A�ȴA�n�A���A�9XA�9XA�A���A�7LA���A��A�z�A�7LA��DA��/A���A��A��+A�JA���A��TA��FA�A�r�A��A�;dA���A�7LA���A��FA�K�A�oA�  A�O�A�{A�bNA���A��A{%AwXAv~�AsVAo��Am�mAl��AkG�Aj�/Ajr�Ai�TAh�uAf=qAd~�Ad{Ac��Ab(�A^ffA\1'AX�/AW?}AU�TAQ��AO�#AM�TAJ�RAI�-AI
=AGoAE`BAD9XAB��AA?}A?�A=XA;�-A:JA8�A5��A3�A2A�A/��A-�TA-S�A,�A+A)��A'�A%��A$-A"��A!�;A jA`BA��A-A~�A��A��An�AI�AM�A�A�AoA33A7LA��A�A��A�hA;dA�A��AjA�A�A�RAjAQ�A �Ap�A��A �A
��A�jAJA�A~�A�
A`BA9XA�AI�A�FAO�A �jA Q�@�E�@�Q�@�p�@�ff@�l�@�E�@�V@�bN@�b@�F@�t�@�R@���@�  @�M�@���@���@�j@� �@�@���@�n�@�{@�@��@畁@�  @��@�j@���@��@�X@�O�@��@���@���@�;d@��@��@�E�@�v�@�=q@��;@�+@޸R@�&�@��@ᙚ@�;d@ݙ�@���@�
=@�b@���@ם�@�-@ա�@�G�@���@�K�@�=q@��@���@�Ĝ@υ@�/@���@̣�@��@˥�@�S�@�33@ʟ�@ɑh@�O�@�X@�X@Ȭ@Ȭ@��@ȋD@Ǯ@�ff@ʗ�@ʧ�@�S�@ˍP@�{@�V@��@��@���@�G�@�Q�@��;@ǶF@ǍP@�K�@���@�=q@���@ź^@�`B@��@���@�1'@Ý�@�V@���@��h@�O�@��@��/@�Q�@���@�ȴ@�n�@�^5@�V@�E�@��^@�V@�bN@�1'@�1@��@���@�ƨ@��F@���@�;d@��y@�n�@��-@��@��@�  @�t�@��@�E�@�-@�=q@��@�@���@�hs@��@���@��9@��D@�A�@��@�
=@��!@��+@�J@���@��7@�hs@�/@���@��@��m@���@�v�@��#@��^@��@�7L@�&�@��@�9X@���@���@��@��P@�l�@�"�@��y@�ȴ@���@�v�@��@��T@���@��h@�7L@���@���@�z�@���@�|�@�l�@�;d@��y@�E�@��@���@��^@�p�@�%@�j@��@��m@��
@�ƨ@��@�|�@�K�@��R@��@�G�@�%@��`@�Ĝ@��9@���@�bN@��
@�@���@�n�@�^5@�=q@�{@�J@���@��-@�X@�G�@��D@�Q�@�A�@��w@�K�@��H@���@��\@�~�@�ff@�{@��T@��^@�p�@�7L@��`@���@��D@�A�@��;@���@���@�dZ@�;d@�;d@�+@��@��\@�E�@��@���@���@��-@�X@���@���@�j@�ƨ@�l�@�l�@�l�@�\)@�K�@�;d@�ȴ@�n�@��@���@��^@��@���@���@�r�@�I�@�1'@��@�ƨ@���@��@��R@�^5@�=q@�5?@�{@��T@��7@�/@���@���@��/@��j@��@��D@�r�@�Q�@��@��;@���@�\)@�+@���@�ff@�=q@�{@���@��T@��#@�@�x�@���@��@��F@��@�dZ@�S�@�dZ@�;d@�33@��@��H@��@��#@���@�`B@�X@�X@�?}@���@��@�Q�@�A�@�b@�ƨ@�t�@�S�@�o@��y@���@���@�~�@�^5@�M�@��@��@��-@�p�@�O�@��@��/@���@��9@�1'@�1@�  @�w@�@l�@~��@~v�@}�@}�h@}O�@}�@|�j@{��@{@z�!@zn�@z-@zJ@y��@y7L@xbN@x �@wl�@vȴ@u@u?}@tZ@s�
@sS�@r�!@q��@p��@pQ�@o�;@o�w@oK�@nȴ@nV@n$�@m�@m@m�@m?}@l�@l�D@lI�@l1@k�
@kƨ@k�F@k��@k��@k�@k33@j~�@i��@iX@h�`@hQ�@gK�@fȴ@f�R@f��@f��@fff@e@eO�@d�/@d�D@dZ@d�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�oA��A�oA�bA�oA�oA�{A��A��A� �A� �A� �A��A��A��A� �A��A�&�A�(�A�&�A�-A�$�A�{A��A�VA�oA��A��A�{A��A�oA�JA��#A�ƨA���AѼjA�ĜA���A���AѓuAсA�n�A�t�A�^5A�M�A�XA�K�A�K�A�=qA�9XA�1'A�-A�-A�+A�&�A�(�A�+A�$�A�&�A�(�A�"�A��A�"�A��A��A� �A��A��A��A��A�{A��A�{A�bA�{A�oA�VA�oA�bA�
=A�JA�JA�1A�1A�
=A�%A�A�1A�A�  A�A�A���A�A���A���A���A���A���A���A���A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��yA��A��A��yA��mA��A��mA��`A��yA��mA��TA��mA��yA��mA��`A��A��mA��HA��`A��HA��#A��#A���A�ȴA���A���A�ȴA���A���A�ȴAоwAоwAк^Aд9Aв-Aв-AЬAХ�AХ�AП�AЏ\AЉ7A�|�A�bNA�VA�A�A�(�A�VA��`Aϕ�Aχ+A�Q�A�G�A�;dA�/A�%A��
AΟ�A�
=AͶFAͥ�A͝�A�|�A�+A��TA�hsA�jA�$�A���A���A��A��A��/A�ȴA˴9A˝�A�r�A�`BA�I�A�(�A��A�bA�JA�JA��Aʴ9A��AɬAɏ\AɃA�l�A�VA�S�A�S�A�C�A�+A��A��A�{A�bA�
=A���A��A��yA��
A���A���A�ƨA���A���AȸRAȥ�Aȕ�A�|�A�ZA�;dA�"�A��A��A�JA���A���A��A��;A���AǺ^Aǟ�A�|�A�p�A�ZA�K�A�;dA�33A�+A�$�A�$�A��A�{A�VA�%A���A���A���A��A��`A��;A���A�ĜAƝ�A�Aţ�A�x�A�\)A�K�A�A�A�?}A�=qA�7LA�&�A��A��A�AľwA�1'A��A��A��#AËDA�(�A��;A�ƨA�r�A���A��hA��TA���A�bA��TA���A��jA���A��uA���A��7A��A��A��+A��+A��A��A��7A��+A��A�~�A�|�A�x�A�r�A�l�A�l�A�jA�bNA�^5A�\)A�S�A�K�A�A�A�;dA�-A��A�A��mA�A��A��A��!A�|�A�VA�33A�
=A��/A��hA�VA�5?A���A�hsA�1'A���A���A�v�A�Q�A�M�A��RA�l�A�ƨA���A�\)A��!A�z�A�E�A��A���A��A���A�bNA���A��;A���A��wA��-A���A��A�t�A�bNA�I�A�C�A�;dA�/A�(�A�%A��TA�|�A�C�A�$�A�VA���A���A�Q�A��-A�"�A�ĜA�|�A�oA��A�|�A�jA�;dA�XA�/A���A���A�n�A�Q�A�=qA�bA��
A���A��FA�1'A���A�dZA�A���A�jA�C�A�7LA�"�A�bA���A���A��jA���A���A�|�A�hsA�Q�A��A��mA�?}A�1'A��A�VA��`A���A�ĜA��!A���A��uA�n�A�VA�=qA�{A�
=A���A��mA��jA���A��A�jA�O�A�+A�  A��mA��jA�XA�1A��-A�VA��A�ĜA��RA���A�x�A�9XA�A���A�?}A�ƨA���A��hA��A�Q�A�(�A�
=A���A���A�A�A�VA���A��TA��/A���A��9A���A�ZA�-A�VA��A���A���A��PA�~�A�r�A��A�ȴA���A�S�A��FA��A�O�A�1'A��A�ȴA���A���A�1'A��;A�x�A�;dA�1A���A�t�A��A�7LA���A��A��uA��\A��DA��7A��A�z�A�ffA�A�1'A��A��A�bA�bNA��FA�n�A��RA��A�9XA�(�A�oA���A��A���A��^A��-A���A���A�jA�bA�I�A���A�z�A�VA���A�dZA��TA�p�A�7LA�JA�x�A��`A��PA�XA�
=A�A��DA�n�A�ffA�ZA�A��HA��-A�ffA��A��mA�A�A��TA��9A��DA�ZA�7LA��A���A��;A���A��FA���A�|�A�G�A�{A���A�{A��-A��PA�~�A�p�A�`BA�XA�E�A�9XA�bA��;A���A�mA|n�Az�Ax�/Ax�Aw��Aw�-Awp�AwC�Aw&�AwVAv�Av�`Av��Av��Avz�Av1'Au+At-AsC�Ar��Ar�DArffAq�#Ap�Ao�#Ao+AnȴAn��An�DAnZAm�
Am�AmO�Am+Am
=Al�/Al�jAl�DAl{AkdZAkC�Ak;dAk33Ak"�Ak%Aj��Aj�/Aj��Aj��Aj�Ajv�Ajn�Ajn�Ajn�AjffAjI�Aj(�Ai�TAi��Aix�AiC�Ai/Aio1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B2aB1[B1�B1�B0�B0�B2-B0!B0�B4B2-B1�B3hB4B49B4nB4nB5tB5�B6B6�B7B7�B7�B8RB<jB@�BE9B_�B��BںB��B�pB�B�B�B"�B?�BY�Bt�B�;B�MB�B�xB�IB��B�xB�JB�B��B��B��B�7B�.B��B~(B��Bk�BV�BA�B8�B/�B)_B,�B(�B&BB�B�B�DB�B��BٴB�B��B�_B�zB�B��B��BffBD3B"hB
�B
�)B
�QB
уB
��B
�xB
��B
u�B
jB
W�B
C�B
49B
AB	�B	�"B	�B	�0B	��B	��B	��B	��B	�XB	��B	�B	�'B	�B	�$B	��B	u�B	k�B	[#B	P�B	J�B	8RB	,B	(�B	�B	�B	(B	_B	B��B��B��B��B� BޞB�mB�B�[B��B�B��B��B��B��B��B�=B��B�B�FB�oB�4B��B��B��B��B��B�MB��B�iBzxBs�BqBrGBx�B��B�	B�B�_B�IB�eB�aB��B��B��B�<B�dB��B�OB�BǮB�#BیB��B�B��B��B��B�&BߤB�jB��B�TBΥB�B�BB�BB�,B��B�jB�B�dB��B��B��B�9B��B��B�B��B�*B�wB�HB�B�}B�OBBB��B�OB��B�'BÖB��BרB��BچB��B�B�B�B�B�mB�B�&B�B�B�B��B� B��B�`B��B��B		�B	JB	DB	1B	�B	B	B	 iB	SB	�B	�B	{B	�B	�B	fB	+B	YB	SB	
	B	B	DB	�B	�B	FB	�B	CB	 �B	"4B	%�B	($B	+�B	2�B	6�B	:*B	:*B	:*B	J#B	RTB	ZQB	cTB	hsB	i�B	hsB	iyB	uZB	{�B	|PB	}VB	|�B	}"B	}"B	|�B	}VB	�B	�B	�B	��B	��B	�B	�B	�B	�YB	�%B	��B	��B	��B	�~B	��B	�MB	�_B	��B	��B	��B	��B	�=B	��B	��B	��B	��B	��B	��B	��B	�zB	�B	��B	�6B	�IB	��B	��B	��B	�B	��B	�B	��B	�dB	��B	��B	��B	�gB	ĜB	�9B	�mB	��B	�B	ɆB	��B	ΥB	�BB	ϫB	уB	��B	ӏB	��B	�aB	ԕB	��B	�9B	�
B	�B	�B	ںB	��B	�]B	�;B	�vB	�B	�|B	�B	�B	��B	� B	��B	�B	��B	�,B	��B	��B	�
B	�B	�DB	�DB	��B	�B	�B	�8B	�B	��B	�B	��B	��B	�B	�)B	�5B	�B	�B	�B	�B	��B	�`B	�+B	�`B	��B	��B	�B	��B	�>B	��B	�B	��B	�rB	�>B	�>B	�DB	�JB	��B	�PB	�PB	��B	��B	�]B	�.B
  B	��B	��B
oB
B
oB
�B
B
�B
B
B
GB
GB
B
MB
�B
MB
B
�B
�B
�B
�B
�B
YB
%B
�B
+B
fB
fB
�B
	lB
	lB
	�B
	�B
	�B
	lB

	B
B
DB
B
B
~B
JB
JB
~B
JB
JB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
bB
 B
hB
B
uB
�B
FB
�B
{B
�B
MB
SB
�B
�B
�B
$B
�B
�B
�B
�B
�B
_B
_B
�B
�B
+B
eB
�B
kB
kB
7B
7B
�B
eB
�B
	B
�B
�B
�B
CB
xB
�B
 �B
 \B
 �B
 �B
 �B
�B
!�B
 �B
 �B
!�B
"hB
#�B
$�B
#�B
$B
%FB
&LB
&�B
'�B
(�B
)*B
(�B
(�B
)_B
)*B
)_B
)�B
)�B
*�B
+�B
,qB
,�B
,�B
-B
.}B
0UB
0�B
0UB
0�B
0UB
1[B
0�B
1'B
1�B
2aB
2�B
2�B
3hB
4nB
4�B
4�B
4�B
4nB
4B
3�B
3hB
2�B
2aB
33B
3hB
3�B
33B
3hB
49B
5�B
6FB
7�B
7�B
7�B
7�B
7LB
7�B
7LB
7B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
8RB
8�B
9$B
9XB
:*B
;�B
<jB
;�B
;�B
;�B
<B
<jB
<�B
=B
<�B
<�B
<�B
<�B
<jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1�B1�B3�B0�B2�B2�B1[B1[B2-B49B/�B3hB3hB1'B1[B2�B1�B0�B1[B/OB0UB0�B/�B2aB/B2�B2aB/�B/�B0!B1�B/B0�B2aB>�B/OB0�B1�B-�B,�B33B1'B2�B6FB2-B4nB33B1'B5?B3�B3�B2aB4�B3�B2�B3hB5B4B33B5B4B33B49B5B2�B4�B5?B33B4�B5?B3�B49B5?B3�B49B5�B33B4nB5�B3�B4B5�B4�B49B5�B5�B4�B6�B6B4nB5tB6�B4�B5�B6�B4�B6B7LB4�B6�B7�B5�B6B7�B6FB6B7�B6�B5�B7�B7LB6B6zB7�B6�B6�B8B7LB6zB8RB6�B6FB8�B8�B7B8B8�B6�B7�B8�B7�B6�B7�B8RB6zB7�B9�B7�B9$B9�B9�B;dB=�B<�B<�B=�B<jB<B=�BA�B?�B@�BB�BB[BA�BB�BC�BB�BB�BE�BC�BE�BH�BH�BJ�BNpBQBZ�BcTBd�Bn/Bi�Bk�Bl"By	B�4B�B��B�^B��B�BBɺBچBچB�2BǮB��B�5B�B�B�|B�B�vB��B�BݘB�#B�;B�jBچB�pBޞB��B�B��BoB��B�,B��B�KB�B��B�B�B�B�B�BBBhB�B$B�B�B�B�B�B!-B!bB%FB+B-CB1�B;0BC�BJ�BM�BO�BT�BT�BV9B^BbNBe�Bj�Br�Bt�Bu%BzB~�B�;B� B��B��B�B��B��B�B�GB��B��B��B�%B�%B�B��B�_B��B�xB��B��B�qB��B�B��B�qB�qB�!B��B�B�:B�B��B��B�nB�\B��B�bB��B�B��B�	B��B��B�\B��B�4B{B~�B�uB� B.B��B�AB��B��B��B�_B�+B�YB��B�lB��B��B�+B��B��B��B��B�lB�B�fB�fB�7B��B�%B��B�DB�B��B�xB�FB��B��B��B�B�=B�rB��B�hBy	BcB�Bz�B~(B{�BxB��Bp�Bk�B�B��Bs�Bs�B��Bh>BbNBa�BZQBVmBW
BZ�Bb�BZ�BG�BH�BEmBC�BAUB?HB=B<�B;�B8B7�B5?B6zB8�B2-BB�B/OB'�B$�B(�B,=B,B7B+6B(�B&�B%B!�B,=BxB)*B7�B.}B.IB-B-wB($B($B+�B&�B \B$�B,=B*�B�B'�B�B�B�B�B�B�B B.B�BDBxB	lBB
	B�B	7B	7B��B�B�B+B��B�PB��B�DB�fB��B��B��B��B�B�GB��B��B�B�|B�iB�B� B�B��B�5B��B�TB�]B�jB�5B�&B�6B��BбB��B�B�aBܒB�<B�?B�IB�UB��B��B��B�'B��B�dB��B��B�LB�zB��B��B��B�B�B�B�:B�nB��B�nB�B�OB��B�\B��B�VB�zB�B��B��B�SB�xB�oB��B�4B��B��B�{B~�B|�B}VBp;B��B�4BK^BJ�BGzBE�BC�B@�B@�BB�BVmB4�B)�B%�B&�B2�BVBoB"B iB
��B
�B
��B
��B
�vB
�B
��B
�B
�B
�yB
�)B
��B
�B
�B
�B
�sB
�yB
�vB
�&B
�sB
��B
��B
��B
�[B
�B
��B
��B
�B
�kB
�OB
�B
�~B
��B
��B
�_B
�qB
�4B
�B
�	B
�B
{�B
~�B
|PB
u%B
v+B
s�B
r�B
j�B
m�B
i�B
o5B
iDB
e`B
m]B
u%B
[WB
P�B
LdB
K�B
K)B
GzB
C�B
B�B
F�B
:�B
E9B
HKB
�;B
1'B
xB
�B
@B
%B
fB
B	��B	�]B	�JB	��B	�B	�B	�	B	�8B
@B	��B
 �B	�DB	��B	�/B	�"B	�QB	�B	ںB	ԕB	�jB	�dB	�B	уB	ɆB	�EB	ŢB	B	�B	�}B	�B	��B	�EB	�jB	�0B	��B	��B	�XB	�RB	��B	��B	�FB	��B	��B	�XB	��B	�B	��B	�RB	��B	�<B	�B	��B	�B	��B	��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                    4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B.�B-�B.B.B-BB-B.}B,qB,�B0UB.}B.IB/�B0UB0�B0�B0�B1�B1�B2aB2�B3gB3�B49B4�B8�B=BA�B\)B��B�
B�/B��B��B�BB�B<6BV8Bp�B}�B��B�_B��B��B�B��B��B}VB�:B�MB��B��B�~B��BzxB��Bh
BR�B>BB5B,B%�B)*B%FB"hBVB1BB��B�oB�"B�B�[B�IB��B��B�UB�@B��Bb�B@�B�B
��B
�yB
�B
��B
�B
��B
�7B
q�B
ffB
T,B
?�B
0�B	��B	�lB	�rB	�WB	ȀB	�NB	��B	�B	�?B	��B	��B	�dB	�wB	�_B	�tB	��B	q�B	h>B	WsB	M5B	GB	4�B	(XB	%B	$B	�B	xB	�B�VB�	B�B�.B�DB�pB��BҽB�^B��B�B�UB��B�3B��B�B�B��B��B�hB��B��B��B��B��B�	B��B�GB��B~(B|�Bv�BpBm]Bn�Bu%B�B�YB�VB��B��B��B��B�)B�HB��B��B��B��B��B�UB��B�sB��B�NB�B�,B�B�NB�vB��BںB�,BΤB��B�^B˒B˒B�|B�BɺB�aB��B�B� B��B��B�3B�B�mB��B�zB��B��B�dB��B��B��B��B�<B��B�B�wB��B�<B��B�2B��B�/B�TB�lB�fB��B�B��B�vB��B�B�cB�/B�pB�;B�B�,B�7B	%B	�B	�B	�B��B�\B�VB��B	�B	 �B	 4B��B��B	GB	�B	{B	�B	�B	YB	eB	�B	1B	CB	�B	B	�B	IB	�B	"3B	$tB	'�B	.�B	2�B	6zB	6zB	6zB	FsB	N�B	V�B	_�B	d�B	e�B	d�B	e�B	q�B	xB	x�B	y�B	y>B	yrB	yrB	y	B	y�B	{�B	|B	|B	}"B	}�B	~\B	�iB	�iB	��B	�uB	�B	�B	��B	��B	�!B	��B	��B	��B	��B	�B	�B	��B	�7B	��B	�!B	��B	�'B	��B	�-B	��B	�nB	��B	��B	��B	�<B	�BB	�BB	�UB	��B	�mB	�KB	��B	�B	�)B	�B	��B	��B	��B	��B	�&B	�aB	��B	�B	��B	˒B	��B	��B	�BB	��B	�HB	бB	��B	�B	҉B	�ZB	�gB	�`B	�
B	�>B	حB	ۋB	��B	�cB	��B	�iB	��B	�;B	�pB	�AB	��B	�B	�|B	�B	�B	�ZB	��B	�B	�B	�%B	��B	��B	�B	��B	�B	�TB	�%B	�2B	�
B	�yB	�B	��B	�]B	�cB	��B	�AB	�B	�{B	�B	�B	�MB	�SB	��B	��B	��B	�`B	�+B	��B	��B	��B	��B	��B	�7B	��B	��B	�>B	�B	��B	�~B	�PB	�B	�JB	��B	�VB	��B	�.B	�\B
 4B	�bB	�bB	��B	��B
 iB
 �B
B
 �B
 iB
 �B
B
B
GB
�B
�B
uB
B
{B
�B
�B
�B
�B
�B
�B
%B
�B
�B
YB
_B
�B
_B
	kB
�B
�B
�B
�B
�B
�B
	kB

	B

=B

	B

�B
B
B
IB
IB
�B
�B
PB
�B
VB
�B
.B
�B
�B
�B
4B
�B
�B
�B
B
B
tB
@B
�B
�B
�B
FB
�B
�B
FB
�B
{B
�B
B
�B
�B
�B
�B
�B
�B
B
YB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
OB
IB
IB
�B
�B
 'B
 �B
 'B
 [B
!�B
"�B
#9B
$@B
%FB
%zB
%FB
%FB
%�B
%zB
%�B
&B
%�B
&�B
($B
(�B
(�B
(�B
)^B
*�B
,�B
,�B
,�B
,�B
,�B
-�B
-BB
-wB
.B
.�B
.�B
/OB
/�B
0�B
1'B
1'B
0�B
0�B
0UB
0 B
/�B
/B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
1�B
2�B
3�B
4B
4B
3�B
3�B
3�B
3�B
3gB
3�B
3�B
3�B
4B
49B
49B
49B
4B
4B
4B
4B
4B
49B
4mB
4�B
4�B
5tB
5�B
6zB
8B
8�B
8B
7�B
8B
8RB
8�B
9#B
9XB
9#B
8�B
8�B
8�B
8�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B.B.IB0 B-B.�B/OB-�B-�B.}B0�B,B/�B/�B-wB-�B.�B-�B-B-�B+�B,�B,�B,<B.�B+kB.�B.�B+�B,B,qB-�B+kB-BB.�B:�B+�B-BB.IB*0B(�B/�B-wB/OB2�B.}B0�B/�B-wB1�B/�B0 B.�B1'B0 B.�B/�B1[B0UB/�B1[B0UB/�B0�B1[B/OB0�B1�B/�B1'B1�B/�B0�B1�B/�B0�B1�B/�B0�B1�B/�B0UB1�B1'B0�B2-B1�B1'B33B2aB0�B1�B2�B1'B1�B2�B0�B2aB3�B1'B2�B4B1�B2aB49B2�B2aB49B2�B2-B49B3�B2aB2�B49B2�B33B4mB3�B2�B4�B33B2�B4�B5B3gB4mB5?B2�B3�B5?B4B2�B49B4�B2�B3�B6B4B5tB6B5�B7�B:)B8�B9#B:)B8�B8RB:)B=�B<6B=B>�B>�B=�B?B@NB>�B?HBB&B@BA�BE9BEBGBJ�BMjBV�B_�BaGBjBe�Bh>BhrBuYB|�B�_B��B��B�B��B�
B��B��B�B��B�GBڅB��B�B��B�cB��B�)B��B��B�sBۋBںB��B��B��B�#B��B�B��B�.B�|B�B�B�WB�>B�B	�B	7B	B
	B	kB	kB�B�BtB�BBB7B�B}B�B!�B'RB)�B.B7�B?�BGEBJ#BL/BP�BP�BR�BZQB^�BbBg8Bo Bp�BquBv`Bz�B}�B|PB~(B~(B}VB~(B�4B�iB�B��B�4B�4B�uB�uB�oB�B��B��B��B�3B��B��B��B�eB�*B��B��B�qB�7B�eB��B�RB��B�!B��B��B��B��B��B�\B��B�YB�LB�LB��B�GB|�Bw�B{B~�B|PB{~B.B~�B.B.B� B��B�{B��B��B��B��B��B�{B�B��B�MB��B��B�SB��B��B��B�%B�uB�%B��B�SB�1B��B��B��B�CB��B�SB��B��B�B��BuYB{�BbBv�BzxBxBtSB|�Bm(Bh>B�\B� Bo�Bo�B��Bd�B^�B^BV�BR�BSZBW
B^�BV�BD3BD�BA�B?�B=�B;�B9XB9#B7�B4mB49B1�B2�B5?B.}B?B+�B$@B �B%FB(�B(XB3gB'�B%FB"�B!bB�B(�B�B%zB4B*�B*�B)^B)�B$tB$tB'�B#9B�B!-B(�B&�BB$B7B@B�B�BBIBPB~B
=B�B�B�BeBYB�B�B�B��B�B��B{B��B��B�B��B�B�7B�MB�B�B��B�B�B��B��B��B�B��B�PB��B�8B�B�+BߤB�BںBڅB�vBɆB�EB�B�/B�[B��B��B��B��B��B��B�B�0B��B�wB�6B��B�$B�*B��B��B��B��B�@B�XB�hB�[B��B��B��B��B�kB��B��B��B�B��B��B�kB�$B�B��B��B��B�B��B�B�!B�B{By>By�Bl�B�	B|�BG�BGBC�BB&B@B<�B=<B?BR�B0�B&B!�B#9B/B
�B�B
rB
��B
��B
�oB
�+B
�+B
��B
�cB
�"B
��B
��B
��B
�yB
�1B+B
�TB
�B
��B
��B
��B
�vB
��B
�6B
�B
�EB
��B
�aB
�'B
��B
�XB
��B
��B
�eB
��B
�3B
�B
��B
��B
��B
�hB
�YB
�eB
xB
z�B
x�B
quB
r{B
pB
o5B
gB
jB
e�B
k�B
e�B
a�B
i�B
quB
W�B
M5B
H�B
HKB
GyB
C�B
@B
?HB
B�B
7B
A�B
D�B
}�B
-wB
�B
�B
�B
uB
�B
oB	�B	��B	��B	�+B	�SB	��B	�YB	�B
�B	��B	�"B	�B	�#B	�B	�rB	�B	�`B	�
B	��B	ɺB	ȴB	�^B	��B	��B	ÕB	��B	��B	�[B	��B	�[B	�B	ÕB	��B	��B	��B	�B	��B	��B	��B	�?B	��B	�B	��B	��B	��B	�mB	�9B	��B	�?B	��B	�gB	��B	�[B	� B	�O4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                    4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224958                            20230721224958AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495820230721224958  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495820230721224958QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495820230721224958QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             