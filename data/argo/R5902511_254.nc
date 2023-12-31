CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:56Z creation      
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
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  X�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  u�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0 (   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` /X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   /�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   5�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ;�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T A�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   B   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   B   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   B   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   B$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � B,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   B�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   B�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    B�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        B�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        B�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       C    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    CArgo profile    3.1 1.2 19500101000000  20230721224956  20230721224956  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�#z��@�#z��11  @�#I���@�#I���@2���rG@2���rG�d�ǎ�k�d�ǎ�k11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�  @   @@  @z�H@��R@�  @�  A   A  A ��A+�A@  A`��A�  A���A���A���A�Q�A�Q�A�Q�A�Q�B (�B  B�
B�B   B(Q�B0(�B7�B?�BG�
BO�BW�
B`  Bg�Bo�
Bx(�B�
B��
B�{B�  B��B��B��B�{B�{B�(�B�  B�  B��B��
B��
B��B�  B��
B�  B�{B��B��B��B�{B�{B��B�  B�  B��B�  B�{B�{C   C
=C
=C  C  C

=C
=C  C  C��C��C  C  C  C  C��C��C"  C$
=C&  C(  C*  C,
=C.  C/�C1��C4
=C6  C8
=C:  C<  C=��C@  CB  CC�CF  CH  CJ
=CK��CM��CO��CQ��CT  CV
=CX
=CZ
=C\
=C]��C`  Cb  Cc��Cf  Ch
=Ci�Cl  Cn
=Cp
=Cr  Ct  Cu��Cx  Cz{C|
=C~
=C�  C���C�  C���C�C�
=C�C�C�  C�C�  C�C�
=C�
=C�C�
=C�
=C�  C�  C�  C�C���C���C�  C�  C�
=C�C�C�  C�  C�C�C�  C���C���C�  C�  C�C�
=C�C�  C�  C���C�  C�C�  C�C�C�
=C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C�  C�
=C�C�  C�C�C���C���C�  C�  C�C�  C�  C�C�  C���C�  C�C�C���C�  C�  C�  C�  C�  C�  C�  C���C�C�  C���C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C�C�\C�
=C�  C�  C�C�C�  C�  C�C�C�  C�C�C�C���C���C���C���C���C�  C���C���C���D   D ��D�D��D  D}qD�D}qD�qD� D  D}qD�qD��DD�D  D}qD�qD	� D
�D
� D
��DxRD��Dz�D  D��D�D�DD��D  D� D�qD}qD  D� D�D}qD  D� D�qD}qD�D� D�qD}qD�qD��D�D}qD�qD��D�D�DD� D  D� D  D� D  D��D   D }qD ��D!� D"�D"� D#  D#� D$  D$��D$�qD%xRD%�qD&�D'D'� D(  D(� D)  D)z�D*  D*�D+D+�D,�D,��D,�qD-}qD-�qD.� D/  D/z�D0�D0��D0�qD1}qD1�qD2}qD3  D3��D4  D4� D5  D5��D5�qD6z�D7  D7��D7�qD8��D9�D9��D:D:� D;  D;� D;�qD<z�D<�qD=� D>  D>� D>�qD?z�D?��D@z�D@�qDA� DB�DB� DB�qDC� DD�DD��DE  DE��DFDF}qDF��DG� DG�qDH� DIDI�DJDJ��DJ�qDKz�DL  DL� DM  DM}qDM�qDN� DN��DOz�DP�DP� DQ  DQ��DR  DR� DR��DS}qDT�DT� DT�qDU}qDV  DV��DW�DW� DX�DX��DY  DY� DZ  DZ}qDZ�qD[� D\�D\��D\�qD]}qD]�qD^z�D^�qD_� D`  D`� Da  Da��Db�Db}qDc�Dc�Dc�qDd}qDd�qDe� Df�Df��Dg�Dg�Dh�Dh}qDh��Di}qDj�Dj}qDk  Dk� Dk��DlxRDm�Dm��DnDn�Do�Doz�Do��Dp}qDq  Dq� Dr�Dr�Ds�Ds� Dt  Dt� Du  Du� Dv  Dv��Dw�Dw��DxDx��Dy  Dy��Dz�Dz� D{�D{��D{�qD|��D}  D}z�D~�D~�D�D��D�qD�>�D�� D���D���D�@ D��HD�� D�  D�@ D�� D��HD�HD�@ D�� D�� D���D�AHD��HD�� D���D�@ D�� D���D���D�@ D��HD�� D�HD�AHD��HD��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�>�?L��?��?��R?���?�@�@��@333@B�\@W
=@n{@}p�@��@�
=@�G�@���@���@��
@�=q@ٙ�@�G�@�@�Q�AG�A�A�AG�AffA=qA!G�A%A*=qA0��A5A9��A@��AFffAJ�HAP  AVffAZ�HA`  AfffAl(�Ap  AuA|(�A\)A�33A�A��A�33A�A��A��A�A�  A�33A��A�  A��HA�A��A��HA�p�A�\)A��HA�p�A�\)A��\A�p�A�
=A\A���A�
=A��A���A�ffA�G�A�(�AָRAأ�AۅA�ffA�  A��HA�A�A��A���A�ffA���A�A�A��A��HA�p�A�\)B ��B�\B�B��B=qB�Bz�B	B\)BQ�Bp�B33BQ�BG�B�RB(�B�B=qB�
B��B�B�B��B��B33B (�B!G�B"�HB#�
B$��B&�\B'�B(��B*=qB+\)B,z�B.{B/\)B0Q�B1��B333B4  B5p�B7
=B8(�B9�B:�RB<  B<��B>=qB?�B@��BABC\)BD��BE�BG
=BHz�BI�BJ�HBL(�BM��BO
=BP  BQ�BR�\BT(�BT��BVffBX  BYG�BZ=qB[�B]�B^ffB_\)B`��BbffBc�Bd��Bf{Bg�Bh��Bi�Bk�Bl��BmBo
=Bp��BqBs
=Btz�Bu�Bv�HBx  By��Bz�RB{�
B}�B~�RB�
B�z�B�G�B�  B�z�B�33B��B�z�B��B��
B��\B���B��B�z�B��B���B�=qB���B���B�{B���B���B�{B��RB�G�B�  B���B��B��B��\B�33B��B�Q�B���B�B�Q�B��HB���B�ffB��HB��B�=qB���B��B�{B���B��B�=qB���B���B�Q�B���B�p�B�Q�B���B��B�Q�B�
=B��B�=qB���B�B�Q�B���B�B�z�B��B��B��\B�G�B��
B��\B�\)B��B�z�B�p�B�{B���B�33B�{B��RB�33B�  B��HB�p�B�  B���B��B�=qB��HB��B��\B�G�B��B���B�p�B�=qB���B��B�z�B�33B�Bď\B�\)B�  BƏ\B�p�B�=qB�
=Bə�B�=qB��BˮB�Q�B�33B��
B�z�B��B�  B���B�\)B�  B��HBӮB�(�B���B��
B�z�B�
=B��BظRB�G�B�(�B��HB�p�B�(�B�
=Bݙ�B�ffB��B�B�Q�B�33B��B�z�B�33B�  B�RB�33B�{B��HB�p�B�{B��HB�B�Q�B���B뙚B�z�B�33B��B�ffB�G�B��B��\B�G�B�{B��B�G�B�{B��RB�33B��B��RB�\)B�B�ffB�33B�  B��\B��B��B��RB�G�B��
B��RB�33B�C G�C ��C �
C33C�C��C  CG�C��C�
C
=C\)C�\CC  CG�C�C��C��C
=CG�Cp�C�\C��C
=C�CQ�C�\C�C�
C�CQ�Cz�C�\C��C
=C�CffC��C�C�HC	�C	Q�C	z�C	��C	��C

=C
33C
G�C
�C
�C
��C  CG�Cp�C��C�RC��C33C\)C�C�RC  C33CffC��C�RC�HC(�CffC�\C�C�C(�CffCz�C�C�C�CQ�Cp�C��C�
C{C\)Cz�C��C�HC(�CffC�C�RC  C=qCffC��C��C
=CG�C�C�RC�C{CG�C��C�
C  C(�C\)C��C�HC{CQ�Cz�C��C�
C{C\)C��C�RC��C{CffC��C��C��C33Cp�C�C�C{C=qCp�C�RC��C�CG�C�CC  C=qCQ�C�CC  C33C\)C�\C��C
=CG�Cz�C�C�
C {C \)C ��C �
C!
=C!=qC!ffC!��C!�HC"�C"\)C"�C"�C"�C#�C#\)C#��C#�HC$  C$33C$p�C$�RC$��C%�C%G�C%z�C%�RC%�C&33C&ffC&�\C&C&��C'(�C'ffC'�C'�C({C(=qC(p�C(�C(�C)�C)G�C)p�C)��C)�HC*(�C*ffC*�\C*�RC*�C+{C+Q�C+��C+��C+��C,�C,Q�C,�C,��C-  C-33C-p�C-��C-��C-��C.(�C.\)C.��C.�HC/
=C/33C/\)C/�C/�RC/�C0(�C0\)C0��C0��C1  C133C1Q�C1�C1�RC1��C233C2ffC2��C2��C3  C3�C3Q�C3�C3C3��C4�C4\)C4�C4�RC4�
C5  C533C5ffC5��C5�
C6  C6�C6Q�C6z�C6��C6�
C7  C7=qC7z�C7�C7�HC8
=C8=qC8p�C8��C8C8�C9{C9G�C9z�C9�RC9��C:{C:=qC:ffC:��C:C:��C;(�C;\)C;�C;C;��C<(�C<\)C<��C<C<��C=�C=G�C=z�C=�C=�C>�C>Q�C>�C>�RC>�C?(�C?\)C?�\C?�RC?�HC@{C@=qC@p�C@��C@��CA  CA33CAz�CA��CA�
CB
=CB=qCBp�CB�\CBCB��CC�CCG�CC�CC�RCC�HCD�CD\)CD�\CD��CE  CE=qCEp�CE��CE�HCF{CFQ�CF�CF�RCF�HCG{CGG�CGz�CG�CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @@  @z�H@��R@�  @�  A   A  A ��A+�A@  A`��A�  A���A���A���A�Q�A�Q�A�Q�A�Q�B (�B  B�
B�B   B(Q�B0(�B7�B?�BG�
BO�BW�
B`  Bg�Bo�
Bx(�B�
B��
B�{B�  B��B��B��B�{B�{B�(�B�  B�  B��B��
B��
B��B�  B��
B�  B�{B��B��B��B�{B�{B��B�  B�  B��B�  B�{B�{C   C
=C
=C  C  C

=C
=C  C  C��C��C  C  C  C  C��C��C"  C$
=C&  C(  C*  C,
=C.  C/�C1��C4
=C6  C8
=C:  C<  C=��C@  CB  CC�CF  CH  CJ
=CK��CM��CO��CQ��CT  CV
=CX
=CZ
=C\
=C]��C`  Cb  Cc��Cf  Ch
=Ci�Cl  Cn
=Cp
=Cr  Ct  Cu��Cx  Cz{C|
=C~
=C�  C���C�  C���C�C�
=C�C�C�  C�C�  C�C�
=C�
=C�C�
=C�
=C�  C�  C�  C�C���C���C�  C�  C�
=C�C�C�  C�  C�C�C�  C���C���C�  C�  C�C�
=C�C�  C�  C���C�  C�C�  C�C�C�
=C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C�  C�
=C�C�  C�C�C���C���C�  C�  C�C�  C�  C�C�  C���C�  C�C�C���C�  C�  C�  C�  C�  C�  C�  C���C�C�  C���C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C�C�\C�
=C�  C�  C�C�C�  C�  C�C�C�  C�C�C�C���C���C���C���C���C�  C���C���C���D   D ��D�D��D  D}qD�D}qD�qD� D  D}qD�qD��DD�D  D}qD�qD	� D
�D
� D
��DxRD��Dz�D  D��D�D�DD��D  D� D�qD}qD  D� D�D}qD  D� D�qD}qD�D� D�qD}qD�qD��D�D}qD�qD��D�D�DD� D  D� D  D� D  D��D   D }qD ��D!� D"�D"� D#  D#� D$  D$��D$�qD%xRD%�qD&�D'D'� D(  D(� D)  D)z�D*  D*�D+D+�D,�D,��D,�qD-}qD-�qD.� D/  D/z�D0�D0��D0�qD1}qD1�qD2}qD3  D3��D4  D4� D5  D5��D5�qD6z�D7  D7��D7�qD8��D9�D9��D:D:� D;  D;� D;�qD<z�D<�qD=� D>  D>� D>�qD?z�D?��D@z�D@�qDA� DB�DB� DB�qDC� DD�DD��DE  DE��DFDF}qDF��DG� DG�qDH� DIDI�DJDJ��DJ�qDKz�DL  DL� DM  DM}qDM�qDN� DN��DOz�DP�DP� DQ  DQ��DR  DR� DR��DS}qDT�DT� DT�qDU}qDV  DV��DW�DW� DX�DX��DY  DY� DZ  DZ}qDZ�qD[� D\�D\��D\�qD]}qD]�qD^z�D^�qD_� D`  D`� Da  Da��Db�Db}qDc�Dc�Dc�qDd}qDd�qDe� Df�Df��Dg�Dg�Dh�Dh}qDh��Di}qDj�Dj}qDk  Dk� Dk��DlxRDm�Dm��DnDn�Do�Doz�Do��Dp}qDq  Dq� Dr�Dr�Ds�Ds� Dt  Dt� Du  Du� Dv  Dv��Dw�Dw��DxDx��Dy  Dy��Dz�Dz� D{�D{��D{�qD|��D}  D}z�D~�D~�D�D��D�qD�>�D�� D���D���D�@ D��HD�� D�  D�@ D�� D��HD�HD�@ D�� D�� D���D�AHD��HD�� D���D�@ D�� D���D���D�@ D��HD�� D�HD�AHD��HD��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�>�?L��?��?��R?���?�@�@��@333@B�\@W
=@n{@}p�@��@�
=@�G�@���@���@��
@�=q@ٙ�@�G�@�@�Q�AG�A�A�AG�AffA=qA!G�A%A*=qA0��A5A9��A@��AFffAJ�HAP  AVffAZ�HA`  AfffAl(�Ap  AuA|(�A\)A�33A�A��A�33A�A��A��A�A�  A�33A��A�  A��HA�A��A��HA�p�A�\)A��HA�p�A�\)A��\A�p�A�
=A\A���A�
=A��A���A�ffA�G�A�(�AָRAأ�AۅA�ffA�  A��HA�A�A��A���A�ffA���A�A�A��A��HA�p�A�\)B ��B�\B�B��B=qB�Bz�B	B\)BQ�Bp�B33BQ�BG�B�RB(�B�B=qB�
B��B�B�B��B��B33B (�B!G�B"�HB#�
B$��B&�\B'�B(��B*=qB+\)B,z�B.{B/\)B0Q�B1��B333B4  B5p�B7
=B8(�B9�B:�RB<  B<��B>=qB?�B@��BABC\)BD��BE�BG
=BHz�BI�BJ�HBL(�BM��BO
=BP  BQ�BR�\BT(�BT��BVffBX  BYG�BZ=qB[�B]�B^ffB_\)B`��BbffBc�Bd��Bf{Bg�Bh��Bi�Bk�Bl��BmBo
=Bp��BqBs
=Btz�Bu�Bv�HBx  By��Bz�RB{�
B}�B~�RB�
B�z�B�G�B�  B�z�B�33B��B�z�B��B��
B��\B���B��B�z�B��B���B�=qB���B���B�{B���B���B�{B��RB�G�B�  B���B��B��B��\B�33B��B�Q�B���B�B�Q�B��HB���B�ffB��HB��B�=qB���B��B�{B���B��B�=qB���B���B�Q�B���B�p�B�Q�B���B��B�Q�B�
=B��B�=qB���B�B�Q�B���B�B�z�B��B��B��\B�G�B��
B��\B�\)B��B�z�B�p�B�{B���B�33B�{B��RB�33B�  B��HB�p�B�  B���B��B�=qB��HB��B��\B�G�B��B���B�p�B�=qB���B��B�z�B�33B�Bď\B�\)B�  BƏ\B�p�B�=qB�
=Bə�B�=qB��BˮB�Q�B�33B��
B�z�B��B�  B���B�\)B�  B��HBӮB�(�B���B��
B�z�B�
=B��BظRB�G�B�(�B��HB�p�B�(�B�
=Bݙ�B�ffB��B�B�Q�B�33B��B�z�B�33B�  B�RB�33B�{B��HB�p�B�{B��HB�B�Q�B���B뙚B�z�B�33B��B�ffB�G�B��B��\B�G�B�{B��B�G�B�{B��RB�33B��B��RB�\)B�B�ffB�33B�  B��\B��B��B��RB�G�B��
B��RB�33B�C G�C ��C �
C33C�C��C  CG�C��C�
C
=C\)C�\CC  CG�C�C��C��C
=CG�Cp�C�\C��C
=C�CQ�C�\C�C�
C�CQ�Cz�C�\C��C
=C�CffC��C�C�HC	�C	Q�C	z�C	��C	��C

=C
33C
G�C
�C
�C
��C  CG�Cp�C��C�RC��C33C\)C�C�RC  C33CffC��C�RC�HC(�CffC�\C�C�C(�CffCz�C�C�C�CQ�Cp�C��C�
C{C\)Cz�C��C�HC(�CffC�C�RC  C=qCffC��C��C
=CG�C�C�RC�C{CG�C��C�
C  C(�C\)C��C�HC{CQ�Cz�C��C�
C{C\)C��C�RC��C{CffC��C��C��C33Cp�C�C�C{C=qCp�C�RC��C�CG�C�CC  C=qCQ�C�CC  C33C\)C�\C��C
=CG�Cz�C�C�
C {C \)C ��C �
C!
=C!=qC!ffC!��C!�HC"�C"\)C"�C"�C"�C#�C#\)C#��C#�HC$  C$33C$p�C$�RC$��C%�C%G�C%z�C%�RC%�C&33C&ffC&�\C&C&��C'(�C'ffC'�C'�C({C(=qC(p�C(�C(�C)�C)G�C)p�C)��C)�HC*(�C*ffC*�\C*�RC*�C+{C+Q�C+��C+��C+��C,�C,Q�C,�C,��C-  C-33C-p�C-��C-��C-��C.(�C.\)C.��C.�HC/
=C/33C/\)C/�C/�RC/�C0(�C0\)C0��C0��C1  C133C1Q�C1�C1�RC1��C233C2ffC2��C2��C3  C3�C3Q�C3�C3C3��C4�C4\)C4�C4�RC4�
C5  C533C5ffC5��C5�
C6  C6�C6Q�C6z�C6��C6�
C7  C7=qC7z�C7�C7�HC8
=C8=qC8p�C8��C8C8�C9{C9G�C9z�C9�RC9��C:{C:=qC:ffC:��C:C:��C;(�C;\)C;�C;C;��C<(�C<\)C<��C<C<��C=�C=G�C=z�C=�C=�C>�C>Q�C>�C>�RC>�C?(�C?\)C?�\C?�RC?�HC@{C@=qC@p�C@��C@��CA  CA33CAz�CA��CA�
CB
=CB=qCBp�CB�\CBCB��CC�CCG�CC�CC�RCC�HCD�CD\)CD�\CD��CE  CE=qCEp�CE��CE�HCF{CFQ�CF�CF�RCF�HCG{CGG�CGz�CG�CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��
A��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A�-A̕�A�|�A�Q�A�/A�$�A��A�
=A��A��`A��HA��HA���A˾wAˏ\A�?}A��HA���A�ȴAʮA�/A��AǇ+A�=qAŰ!A�1A��A�~�A�5?A�ZA�z�A�l�A���A�M�A��;A�x�A�bA�E�A�9XA�ZA��9A��\A�I�A��A���A��HA�(�A��A�(�A��TA��FA�bA���A�9XA�^5A���A��FA��DA�jA�9XA��#A��A���A�/A�$�A���A�v�A���A�~�A��mA�(�A��A�XA���A���A��RA���A���A�33A��uA���A��yA���A�(�A|��A{%Ay��Aw�mAr�Am�;Ak��Akp�AioAd�jAa�-A_C�A\�HA\~�A\E�A[G�AX��AWt�AUAS�-AQƨAP$�AJĜAG��AF�!AD��AC�TAB�9AAC�A?K�A=|�A<��A;�A;33A9�-A5�A4jA1�PA0�!A.�yA,�A,{A+ƨA*��A*jA)��A(�A%��A%XA$=qA"(�A��A�A~�A`BA�AĜA-AQ�A-A��AhsA�A�A"�AM�A|�AoA�!AffA�A�^A�`A�DAbNA��An�A�A�Ap�A�9AbAO�A
�A	��AȴA �A�jA��A��AO�A��A��A-A�PA\)AJA��A ��@�
=@�$�@�%@�K�@�V@�7L@��`@���@�1@�+@���@��7@�r�@�F@��@��@�O�@�(�@�{@�G�@���@�h@���@�j@�u@畁@�V@�Q�@㝲@�S�@��y@�n�@��T@�A�@�33@���@ާ�@݁@�ƨ@�o@��H@�^5@�G�@��/@�1'@�l�@��y@�J@�&�@���@җ�@�ff@Ѻ^@Ь@�bN@϶F@·+@ͺ^@̴9@�Z@��@˅@�\)@�+@ʸR@ɑh@�?}@��`@��@ǶF@�@Ƨ�@Ə\@�ff@�J@�O�@ļj@ċD@��@���@î@�|�@�o@�-@��@��#@�hs@�%@���@���@��@���@�E�@�{@���@�7L@��9@�9X@��F@��@��\@�5?@�5?@�@�?}@�G�@��`@��
@�|�@��@��H@�ȴ@���@�~�@�ff@��#@�?}@�Ĝ@��D@�A�@� �@���@��@��P@�l�@�\)@�K�@�C�@�;d@�@��!@�V@���@�p�@�Ĝ@�Z@�b@��m@���@���@�C�@�@��H@���@��!@���@�=q@��@��j@�bN@�(�@�b@��;@���@�t�@�\)@�33@���@�V@��#@�x�@���@�r�@��;@���@�dZ@���@��@��\@��@�O�@���@�Z@���@�|�@��@��\@�v�@�^5@�^5@�5?@�J@���@�p�@�/@���@�z�@�9X@��F@�;d@�"�@��@��y@�n�@�$�@���@��#@��^@��7@�G�@�V@��@���@�j@�A�@��@��;@��w@�l�@���@�=q@��T@���@��^@��-@��-@��-@���@�x�@�`B@�?}@���@���@��D@���@���@�;d@���@��R@���@���@�n�@�ff@�^5@�M�@�$�@���@�@��7@�O�@�/@���@��9@��u@�z�@�1'@�b@���@��w@�|�@�K�@��@���@�V@�$�@��T@�@��h@�hs@�hs@�X@�?}@��@�Ĝ@�r�@�I�@��@���@��@�S�@��\@�J@��@�@��7@�`B@�?}@�%@���@�bN@���@�t�@��@��+@�$�@�{@��#@���@���@�@���@���@��^@�?}@���@�bN@�Q�@��@�;d@��@���@���@���@��!@���@��T@�X@�O�@�X@�O�@�?}@�?}@�?}@�?}@�?}@�7L@��@�z�@��@�ƨ@��@���@��@�S�@��@�@��y@���@���@���@���@���@���@���@�E�@�$�@�{@�J@��@���@��-@��h@��7@�p�@�`B@�XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AθRAθRA��A���A���A��A��
A���A���A��A���A��A��`A��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A���A���A��A��A��A��A��A��A��A���A���AΟ�A�p�A�  AͅA���A���A̴9A̩�Ạ�A̝�A̕�Ȁ\A̍PA̋DȦ+A̅A̅ÁA�x�A�l�A�`BA�VA�VA�ZA�Q�A�G�A�A�A�7LA�-A�1'A�(�A�&�A�-A�(�A�(�A�+A�$�A�"�A�&�A� �A��A��A��A��A��A��A�oA�VA�bA�JA�A�A���A���A���A���A��A��yA��A��A��`A��TA��`A��`A��;A��HA��HA��HA��/A��;A��TA��yA��mA��mA��yA��TA���A���A���A���A���A���A���A���A�ƨA�ƨA�A˼jA˺^A˰!A˧�A˝�A˗�AˋDA�~�A�t�A�v�A�r�A�`BA�?}A�+A�
=A���A��A��A��/A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ƨA�A�A�ƨA�AʾwAʧ�AʁA�hsA�VA�G�A�;dA�+A��AɼjA�\)A�A�A�(�A�  A��;AȶFA�;dA��Aǧ�A�\)A�JA��
Aư!Aƥ�A�l�A�{A��A��mA��/A�ĜA�AžwAŗ�A�~�A�r�A�K�A�&�A���A��;AĮA�x�A�jA�ZA�%AøRA×�A�z�A��TA�A���A��A��9A�x�A�t�A�1'A�VA��`A���A�v�A�ffA�S�A�;dA�33A�{A��A���A�ZA� �A��mA�ƨA��7A�ZA�ZA�I�A�%A�  A��yA�A���A��uA�hsA�ZA�K�A�G�A�&�A��A��mA��yA��#A��A��RA���A���A��A�n�A�`BA�E�A�=qA�1'A�+A��A���A���A��+A�bNA�(�A�A��HA��RA�l�A�;dA�
=A��9A���A�dZA�;dA�;dA�;dA�n�A�S�A�%A��
A���A�XA�+A�%A��`A��jA�n�A�ȴA�{A�~�A�^5A�$�A�VA���A��`A�ȴA���A�XA�E�A�5?A��A�%A��wA���A��uA��7A�v�A�Q�A��/A��uA�=qA�"�A��RA�$�A�I�A�bNA���A��+A��
A�v�A�$�A���A��PA�7LA���A���A�=qA�VA��PA�  A�A���A��#A���A��yA�%A�%A���A�  A��TA�ȴA��FA���A��hA�~�A�v�A�t�A�t�A�l�A�ZA�C�A�7LA��A���A��9A���A��7A�x�A�^5A�C�A� �A�JA��A��#A���A�A���A��DA��7A��A�x�A�ZA�G�A�7LA�-A�$�A��A���A��/A�ƨA���A�t�A�+A��A�n�A���A�x�A�hsA�dZA�XA�;dA�VA���A���A��`A�A���A��A�jA�S�A�G�A�9XA� �A���A���A���A�l�A�M�A�?}A�(�A��A�VA�1A���A��`A��!A�M�A��A�p�A�?}A�VA��#A���A�t�A�O�A�-A���A���A��A��hA�x�A�\)A�?}A�/A��A�bA�JA���A��wA�x�A�M�A�"�A�A��yA���A���A��A��\A�|�A�ffA�1'A��A�
=A��A��A��A��/A�A��FA��-A��-A��A���A���A���A��\A�jA�jA�bNA�\)A�K�A�A�A�+A�JA��A���A�ĜA��A�t�A��A���A�`BA�(�A�
=A���A���A��^A���A���A���A���A��DA��A��A�~�A�|�A�~�A�~�A�~�A�x�A�n�A�n�A�p�A�l�A�ffA�^5A�O�A�C�A��RA�?}A��A��
A��jA���A��A��uA��+A��+A��A�x�A�p�A�p�A�r�A�l�A�ffA�A�A�oA��/A��A��A���A��A�z�A�r�A�`BA�M�A�I�A�?}A�{A�A�  A���A��A��mA��`A��;A���A��-A��A�bA��TA���A��wA���A��PA�~�A�t�A�^5A�C�A� �A�oA��A��;A��
A���A�ƨA���A��RA���A��DA�v�A�ffA�S�A�E�A�=qA�5?A�"�A�
=A��A���A���A���A��\A�x�A�p�A�ZA�G�A�$�A���A��-A�\)A�A��#A���A��PA�jA�5?A��A�~�A��A�A�A���A�n�A�{A���A��A���A�ƨA��-A���A��PA��A��7A��+A�v�A�t�A�n�A�ffA�ZA�E�A�?}A�-A��A���A���A��A��HA�A��-A���A���A���A�t�A�dZA�Q�A� �A��A�{A�1A���A��A��`A���A�Q�A��A�jA��A��yA���A�t�A�E�A���A�~�A�jA�G�A��yA��A�x�A�l�A�=qA��\A�bNA�E�A�+A��A��A��A���A��9A���A�~�A�\)A�/Al�A~�DA~bA}�A}C�A|�/A|�A|��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��
A��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A�-A̕�A�|�A�Q�A�/A�$�A��A�
=A��A��`A��HA��HA���A˾wAˏ\A�?}A��HA���A�ȴAʮA�/A��AǇ+A�=qAŰ!A�1A��A�~�A�5?A�ZA�z�A�l�A���A�M�A��;A�x�A�bA�E�A�9XA�ZA��9A��\A�I�A��A���A��HA�(�A��A�(�A��TA��FA�bA���A�9XA�^5A���A��FA��DA�jA�9XA��#A��A���A�/A�$�A���A�v�A���A�~�A��mA�(�A��A�XA���A���A��RA���A���A�33A��uA���A��yA���A�(�A|��A{%Ay��Aw�mAr�Am�;Ak��Akp�AioAd�jAa�-A_C�A\�HA\~�A\E�A[G�AX��AWt�AUAS�-AQƨAP$�AJĜAG��AF�!AD��AC�TAB�9AAC�A?K�A=|�A<��A;�A;33A9�-A5�A4jA1�PA0�!A.�yA,�A,{A+ƨA*��A*jA)��A(�A%��A%XA$=qA"(�A��A�A~�A`BA�AĜA-AQ�A-A��AhsA�A�A"�AM�A|�AoA�!AffA�A�^A�`A�DAbNA��An�A�A�Ap�A�9AbAO�A
�A	��AȴA �A�jA��A��AO�A��A��A-A�PA\)AJA��A ��@�
=@�$�@�%@�K�@�V@�7L@��`@���@�1@�+@���@��7@�r�@�F@��@��@�O�@�(�@�{@�G�@���@�h@���@�j@�u@畁@�V@�Q�@㝲@�S�@��y@�n�@��T@�A�@�33@���@ާ�@݁@�ƨ@�o@��H@�^5@�G�@��/@�1'@�l�@��y@�J@�&�@���@җ�@�ff@Ѻ^@Ь@�bN@϶F@·+@ͺ^@̴9@�Z@��@˅@�\)@�+@ʸR@ɑh@�?}@��`@��@ǶF@�@Ƨ�@Ə\@�ff@�J@�O�@ļj@ċD@��@���@î@�|�@�o@�-@��@��#@�hs@�%@���@���@��@���@�E�@�{@���@�7L@��9@�9X@��F@��@��\@�5?@�5?@�@�?}@�G�@��`@��
@�|�@��@��H@�ȴ@���@�~�@�ff@��#@�?}@�Ĝ@��D@�A�@� �@���@��@��P@�l�@�\)@�K�@�C�@�;d@�@��!@�V@���@�p�@�Ĝ@�Z@�b@��m@���@���@�C�@�@��H@���@��!@���@�=q@��@��j@�bN@�(�@�b@��;@���@�t�@�\)@�33@���@�V@��#@�x�@���@�r�@��;@���@�dZ@���@��@��\@��@�O�@���@�Z@���@�|�@��@��\@�v�@�^5@�^5@�5?@�J@���@�p�@�/@���@�z�@�9X@��F@�;d@�"�@��@��y@�n�@�$�@���@��#@��^@��7@�G�@�V@��@���@�j@�A�@��@��;@��w@�l�@���@�=q@��T@���@��^@��-@��-@��-@���@�x�@�`B@�?}@���@���@��D@���@���@�;d@���@��R@���@���@�n�@�ff@�^5@�M�@�$�@���@�@��7@�O�@�/@���@��9@��u@�z�@�1'@�b@���@��w@�|�@�K�@��@���@�V@�$�@��T@�@��h@�hs@�hs@�X@�?}@��@�Ĝ@�r�@�I�@��@���@��@�S�@��\@�J@��@�@��7@�`B@�?}@�%@���@�bN@���@�t�@��@��+@�$�@�{@��#@���@���@�@���@���@��^@�?}@���@�bN@�Q�@��@�;d@��@���@���@���@��!@���@��T@�X@�O�@�X@�O�@�?}@�?}@�?}@�?}@�?}@�7L@��@�z�@��@�ƨ@��@���@��@�S�@��@�@��y@���@���@���@���@���@���@���@�E�@�$�@�{@�J@��@���@��-@��h@��7@�p�@�`B@�XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AθRAθRA��A���A���A��A��
A���A���A��A���A��A��`A��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A���A���A��A��A��A��A��A��A��A���A���AΟ�A�p�A�  AͅA���A���A̴9A̩�Ạ�A̝�A̕�Ȁ\A̍PA̋DȦ+A̅A̅ÁA�x�A�l�A�`BA�VA�VA�ZA�Q�A�G�A�A�A�7LA�-A�1'A�(�A�&�A�-A�(�A�(�A�+A�$�A�"�A�&�A� �A��A��A��A��A��A��A�oA�VA�bA�JA�A�A���A���A���A���A��A��yA��A��A��`A��TA��`A��`A��;A��HA��HA��HA��/A��;A��TA��yA��mA��mA��yA��TA���A���A���A���A���A���A���A���A�ƨA�ƨA�A˼jA˺^A˰!A˧�A˝�A˗�AˋDA�~�A�t�A�v�A�r�A�`BA�?}A�+A�
=A���A��A��A��/A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ƨA�A�A�ƨA�AʾwAʧ�AʁA�hsA�VA�G�A�;dA�+A��AɼjA�\)A�A�A�(�A�  A��;AȶFA�;dA��Aǧ�A�\)A�JA��
Aư!Aƥ�A�l�A�{A��A��mA��/A�ĜA�AžwAŗ�A�~�A�r�A�K�A�&�A���A��;AĮA�x�A�jA�ZA�%AøRA×�A�z�A��TA�A���A��A��9A�x�A�t�A�1'A�VA��`A���A�v�A�ffA�S�A�;dA�33A�{A��A���A�ZA� �A��mA�ƨA��7A�ZA�ZA�I�A�%A�  A��yA�A���A��uA�hsA�ZA�K�A�G�A�&�A��A��mA��yA��#A��A��RA���A���A��A�n�A�`BA�E�A�=qA�1'A�+A��A���A���A��+A�bNA�(�A�A��HA��RA�l�A�;dA�
=A��9A���A�dZA�;dA�;dA�;dA�n�A�S�A�%A��
A���A�XA�+A�%A��`A��jA�n�A�ȴA�{A�~�A�^5A�$�A�VA���A��`A�ȴA���A�XA�E�A�5?A��A�%A��wA���A��uA��7A�v�A�Q�A��/A��uA�=qA�"�A��RA�$�A�I�A�bNA���A��+A��
A�v�A�$�A���A��PA�7LA���A���A�=qA�VA��PA�  A�A���A��#A���A��yA�%A�%A���A�  A��TA�ȴA��FA���A��hA�~�A�v�A�t�A�t�A�l�A�ZA�C�A�7LA��A���A��9A���A��7A�x�A�^5A�C�A� �A�JA��A��#A���A�A���A��DA��7A��A�x�A�ZA�G�A�7LA�-A�$�A��A���A��/A�ƨA���A�t�A�+A��A�n�A���A�x�A�hsA�dZA�XA�;dA�VA���A���A��`A�A���A��A�jA�S�A�G�A�9XA� �A���A���A���A�l�A�M�A�?}A�(�A��A�VA�1A���A��`A��!A�M�A��A�p�A�?}A�VA��#A���A�t�A�O�A�-A���A���A��A��hA�x�A�\)A�?}A�/A��A�bA�JA���A��wA�x�A�M�A�"�A�A��yA���A���A��A��\A�|�A�ffA�1'A��A�
=A��A��A��A��/A�A��FA��-A��-A��A���A���A���A��\A�jA�jA�bNA�\)A�K�A�A�A�+A�JA��A���A�ĜA��A�t�A��A���A�`BA�(�A�
=A���A���A��^A���A���A���A���A��DA��A��A�~�A�|�A�~�A�~�A�~�A�x�A�n�A�n�A�p�A�l�A�ffA�^5A�O�A�C�A��RA�?}A��A��
A��jA���A��A��uA��+A��+A��A�x�A�p�A�p�A�r�A�l�A�ffA�A�A�oA��/A��A��A���A��A�z�A�r�A�`BA�M�A�I�A�?}A�{A�A�  A���A��A��mA��`A��;A���A��-A��A�bA��TA���A��wA���A��PA�~�A�t�A�^5A�C�A� �A�oA��A��;A��
A���A�ƨA���A��RA���A��DA�v�A�ffA�S�A�E�A�=qA�5?A�"�A�
=A��A���A���A���A��\A�x�A�p�A�ZA�G�A�$�A���A��-A�\)A�A��#A���A��PA�jA�5?A��A�~�A��A�A�A���A�n�A�{A���A��A���A�ƨA��-A���A��PA��A��7A��+A�v�A�t�A�n�A�ffA�ZA�E�A�?}A�-A��A���A���A��A��HA�A��-A���A���A���A�t�A�dZA�Q�A� �A��A�{A�1A���A��A��`A���A�Q�A��A�jA��A��yA���A�t�A�E�A���A�~�A�jA�G�A��yA��A�x�A�l�A�=qA��\A�bNA�E�A�+A��A��A��A���A��9A���A�~�A�\)A�/Al�A~�DA~bA}�A}C�A|�/A|�A|��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�]B
��B
��B
��B
�VB
��B
��B
�VB
��B
�]B
��B
��B
��B
��B
��B
�VB
�"B
��B
��B
��BMB�B&B/OB:^BG�BR B\�Bm�B�4B��B�eB��B�?B��B�B��B��B��BSB�B6zBU2BaB{�B��B��B��B|Bv�B�XB��B��B�kB��B|�Bo B`vBM�B@�BLdBK^B\�BYKBZ�B_pB`�B?�B+B<jBUgBS&BJXB?�B1'B�B{B�B�	B�B��B��B��B�'B�!B��B}�BxBc�B\)BPBFB*�B�B�B�B
�B
��B
�<B
�tB
��B
��B
e�B
R B
7LB
'�B
�B

�B	�B	��B	�^B	��B	�B	�B	~�B	w�B	h>B	e�B	d�B	b�B	XEB	P}B	D�B	=B	/�B	&�B	IB	�B	�B��B�B�yB�B� B� B�vB�BB�dBרB�B�9B�6B��B�eB��B��B�+B��B�4B��B��B�SB��B~�B{BxlBsBrGBv+B|�B~�B�oB��B��B�4B��B��B��B��B��B��B��B��B��B��B�FB�$B�RB�LB��B�nB��B��B�=B��B��B��B�\B�'B��B�CB��B��B�'B�B�FB�B��B�_B�0B��B�9B��B��B�aB�RB�dB��B�B�dB�B��B��B�B�B�B�oB�B��B�B�fB��B�B��B	�B	�B	B	 �B	+B	+B	�B	�B	(B	4B	uB	�B	IB	OB	�B	�B	$B	)_B	*�B	*0B	,=B	.IB	.}B	1'B	1�B	3�B	8�B	;�B	AUB	A B	C�B	G�B	I�B	J�B	MB	R�B	UgB	Y�B	Z�B	\�B	^B	^�B	_�B	a�B	g�B	h>B	jB	m�B	pB	r�B	s�B	s�B	tB	u�B	yrB	zB	{�B	}�B	~]B	�4B	�B	��B	��B	�_B	��B	��B	�B	��B	�FB	�YB	�+B	��B	�	B	��B	�B	�@B	��B	�RB	��B	�kB	�kB	�B	�B	�OB	�UB	�9B	�B	��B	�*B	�^B	��B	��B	��B	��B	�<B	��B	��B	� B	�'B	�aB	�3B	�gB	��B	�mB	ŢB	��B	ŢB	ŢB	�B	�zB	�RB	�6B	͟B	�NB	҉B	��B	�aB	��B	��B	�?B	�
B	��B	֡B	֡B	֡B	�yB	��B	یB	ܒB	��B	��B	��B	��B	ޞB	ߤB	�B	�B	�B	��B	��B	�2B	��B	��B	�B	�B	�B	�B	��B	�)B	�/B	�B	�B	�B	�AB	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�2B	��B	�>B	��B	�B	��B	�xB	��B	�PB	�PB	��B	��B	��B	��B	�]B	��B	��B
 �B
 �B
;B
oB
oB
AB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
+B
�B
_B
+B
	lB

rB

�B

�B

�B

�B
B

�B
DB
�B
�B
B
PB
PB
B
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
4B
�B
B
:B
�B
�B
�B
�B
�B
@B
B
�B
�B
MB
�B
�B
SB
�B
_B
�B
�B
�B
eB
eB
eB
�B
�B
�B
�B
�B
�B
eB
B
�B
�B
�B
kB
�B
�B
	B
�B
�B
CB
�B
�B
OB
!B
�B
VB
VB
!B
�B
 �B
 �B
!-B
!�B
"�B
#:B
#:B
#:B
#:B
#nB
#nB
#nB
%FB
&B
&B
&B
&B
%�B
%�B
&LB
%�B
%�B
&�B
&�B
&�B
&�B
'RB
'B
'�B
)_B
)_B
)_B
)_B
)_B
)�B
)�B
*eB
*�B
,qB
-CB
-�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�"B
�VB
��BB
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
�"B
�]B
��B
��B
�"B
�]B
�"B
�VB
��B
��B
�"B
��B
�VB
��B
��B
��B
��B
�"B
��B
�"B
��B
��B
�VB
��B
�]B
�(B
��B
�(B
��B
��B
��B
��B
�]B
��B
��B
��B
�(B
�(B
��B
��B
��B
�VB
��B
��B
��B
�]B
�(B
��B
�"B
��B
�"B
�"B
��B
��B
�PB
��B
��B
�VB
��B
�B
��B
�]B
�PB
��B
�VB
�VB
��B
�B
��B
�B
�JB
��B
�JB
�xB
��B
��B
��B
��B
�B
��B
�JB
��B�BOB�B�B+B	B�B~B!B!�B!�B!�B#nB$�B$�B$tB'B)�B+�B-CB-B,B/OB3�B5B8RB9�B8RB<jB<�B:�B<�BE�BD�BGBH�BIBL0BOBOvBQNBT,BS[BS�BWsBY�BYB[�B`�Bd&BgmBjBk�BkQBm]Bs�Bu%Bu�B{JB~�B�B�B��B�B�:B��B��B��B�~B��B�B�XB��B�=B��B��B�'B��B�B�aB��B�3B��B��B��B��B��B�zB�zB��B��B�6B��B��B�wB��B�aB�dB�B�dB�pB�,B�#B�B�MB�%B�`B��B�8B�2B�>B�xB�	B�xB�VB��B��B�"B��B��B��B��B�BPB�B�B�BuBuB�B"�B7�B:^B2-B4B7�B7LB7�BQ�BK�BW?BYKBWsB`�BV�BS[Ba�Be�Bg8BjBq�Bz�BzxB}VB�B~]B{B|�B��B�uB�GB��B�~B�_B��B�MB��B��B�\B�	B�"B�oBrGBsBqABtB�B� B{�B� Bw2BtTBq�Bq�BxlB�B�OB��B�B��B�aB�aB�B�[B��B��B��B��B�B��B��B��B��B�1B��B�SB�qB�B��B��B��B�SB��B��B��Bz�B{�Bx�Br�BpBo�BncBiDBncBe,BcTBd�Bb�B]�BY�BW�B\�BK�BJXBD3B9�BJ�B5?BH�B=BEBU�BO�B@�BI�BFtBJXBI�BE9BMjBNB]/Bf�BW
B[�B_�B\]B\)BYBU�B\�BW�BR�BY�BW�BW�B`�BX�BZBh
Bd&BiyBiDBZ�BS[Bk�Bl�Bt�Bu�Bd�BA�B7B^B@�B@OB?�B5B5�B)�B'�B2-B�B*0B0�B(�B,=B1�B?BA�BJ#BRTBS&BR�BWsBYBVBVmBV9BTaBS[BR�BP�BQ�BTaBT�BQ�BV�BY�BQ�BR BO�BN�BN�BN�BOvBL�BH�BHKBGzBEBI�BE�BB'BA BC�BE9BC-B?B>BB>B;dB>�B=�B9�B9XB7LB=�B9XB?HB:�B!bB!�B�B!B!bBBqB=B�BIB1B7BeB�B{B�B�B_B+B$B�BhB�B�B�B
�BxB�BB�B\B{B
rBAB��B�VB��B��B��B��B�TB�vB��B� B�cB�B�B�B�mB��B��B�`B�mB��B�BݘBںB�yB��B�B�ZB҉B�EB�3B�B��B�[B�B��B��B��B�<B��B�B�^B��B��B��B��B��B��B�aB��B��B�LB��B��B��B�B�_B��B�B�B�!B��B�B�4B�rB��B�DB��B��B��B�B�4B��B�;B� B��B�;B~]B~]B}"B~�B� B|�B{B{�BxlB{�BzDB{B�Br�Bq�Bh�Bh>Bp;Bc Bf�Bb�Bc�Bc�Bc�Ba�BbNB_pBa|B_pBcTB^Bh�B[�BT�BU�BYBT�BU�BY�BOBN�BM�BW?BPHBJ�BIRBJ�BG�BE�BFtBE�BC�Bp;BJ#B4nB/OB0!B/B-CB+B+6B,�B+B*eB&�B'�B$�B�B 'B \B 'B \B!�BVB�BB�B�B7B�B�B+B�B�BuB�BB.B�B�B
	B"B	7B(B	7B 4B
�>B
��BB
�B
�|B
��B
��B
�rB
��B
�B
��B
�B
�XB
ɆB
��B
�mB
ŢB
˒B
�gB
ÖB
�HB
B
�UB
�B
��B
�LB
��B
��B
�B
�}B
��B
��B
��B
�FB
�XB
�B
��B
�nB
�B
�9B
��B
��B
�}B
��B
��B
��B
�eB
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
�	B
�.B
�B
x�B
y�B
.B
t�B
o5B
k�B
t�B
j�B
c�B
[�B
W�B
XEB
V�B
Y�B
W?B
T�B
Q�B
R�B
M�B
OBB
\�B
J�B
J�B
A�B
;�B
<�B
7LB
5t4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B
��B
��B
�DB
�>B
�B
��B
�B
�B
��B
�B
��B
�B
��B
�DB
�B
��B
��B
�rB
�	B
�B
�7B�B�B"hB+�B6�BD3BNpBYKBi�B|�B�LB��B��B��B��B�gB�NB��B�>B�B�B2�BQ�B]cBxB}�B�1B��BxlBsB��B��B�B��B��Bx�BkPB\�BJ#B=<BH�BG�BX�BU�BW>B[�B]/B<B'RB8�BQ�BOvBF�B<6B-wB*B�B
	B�YB�`B�KB�B��B�wB�qB.BzBtSB_�BXyBLdBB[B'B�B@B�B
��B
� B
��B
��B
�FB
��B
bNB
NpB
3�B
#�B
*B
+B	��B	�B	��B	� B	�XB	�hB	{B	tB	d�B	bB	aGB	_B	T�B	L�B	A B	9XB	,B	"�B	�B	�B	  B��B�WB��B��B�pB�pB��BܒBٴB��B�gB��B��B��B��B�CB�B�{B�4B��B��B�=B��B}"B{BwfBt�BoiBn�Br{By>Bz�B}�B��B��B��B��B�B��B��B�=B�=B��B�B�IB�OB��B�tB��B��B��B��B�B�=B��B��B�$B�IB��B�wB�B��B�=B��B�wB�bB��B�hB�B��B��B��B��B�NB�B��BŢBȴB�NB�mBٴB�cB�;B�5B��B�B��B�B��B�;B��B�B��B��B�B	  B��B�VB�"B	{B	{B		7B	
=B	xB	�B	�B	@B	�B	�B	�B	B	 [B	%�B	&�B	&�B	(�B	*�B	*�B	-wB	.B	0 B	5B	8B	=�B	=pB	@B	D3B	E�B	GB	IQB	N�B	Q�B	V8B	W>B	X�B	ZQB	[#B	[�B	^5B	c�B	d�B	ffB	jB	lWB	o5B	pB	o�B	poB	q�B	u�B	v`B	xB	zDB	z�B	|�B	}VB	� B	��B	��B	�B	�+B	�eB	�7B	��B	��B	�{B	�B	�YB	��B	�UB	��B	�3B	��B	��B	��B	��B	�XB	�dB	��B	��B	��B	�mB	�B	�zB	��B	�B	��B	�KB	�KB	��B	�0B	��B	�pB	�wB	��B	��B	��B	�&B	��B	��B	�&B	��B	��B	�aB	��B	ŢB	ɆB	��B	͞B	��B	�B	бB	�B	�NB	ӏB	�ZB	�&B	��B	��B	��B	��B	�>B	��B	��B	�KB	�KB	�B	�B	��B	��B	�]B	�B	��B	�;B	�AB	�B	�,B	�2B	��B	�fB	��B	�B	�>B	�yB	�B	��B	��B	�]B	�B	� B	��B	�B	�oB	�;B	��B	�B	�GB	��B	�GB	��B	�B	�MB	��B	�+B	�`B	�+B	��B	�7B	��B	��B	�	B	��B	�>B	�DB	��B	�B	�JB	��B	��B	��B	��B	��B	��B	�bB
 �B
 �B
 �B
B
 �B
 �B
 �B
 �B
B
B
GB
{B
{B
�B
�B
{B
�B
�B
�B
�B
+B
+B
_B
+B
�B
�B
1B
	kB
	�B
	�B
	kB
	�B
	B
	B
	�B

=B

=B

	B

�B
CB
�B
B
B
�B
!B
VB
�B
'B
'B
'B
'B
'B
�B
bB
4B
4B
�B
�B
:B
�B
�B
�B
B
B
�B
�B
�B
�B
LB
�B
B
LB
LB
B
�B
RB
B
B
B
�B
$B
$B
YB
*B
*B
�B
*B
*B
�B
qB
�B
�B
�B
qB
B
B
B
}B
OB
!B
�B
�B
�B
�B
�B
�B
�B
!�B
"hB
"hB
"hB
"hB
"3B
"3B
"�B
"3B
"3B
"�B
"�B
#B
#9B
#�B
#nB
$B
%�B
%�B
%�B
%�B
%�B
&B
&B
&�B
&�B
(�B
)�B
)�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�rB
��B
�B
�VB
��B
�	B
�B
��B
�B
��B
��B
�B
��B
�rB
�7B
�rB
��B
�B
�	B
�rB
��B
�rB
��B
�JB
��B
�rB
�JB
��B
�	B
��B
�B
�>B
�rB
�JB
�rB
��B
��B
��B
�>B
��B
�xB
�	B
�xB
�B
��B
�	B
�B
��B
�>B
�JB
�	B
�xB
�xB
�>B
�B
�B
��B
��B
�DB
��B
��B
�xB
��B
�rB
��B
�rB
�rB
�JB
��B
��B
�DB
��B
��B
�B
�lB
��B
��B
��B
��B
��B
��B
�7B
�lB
�B
��B
��B
�	B
��B
��B
�7B
��B
�%B
��B
�fB
�B
��B
�	B1B�BBB{BYB*B�BqBBB�B�B �B �B �B#nB%�B'�B)�B)^B(XB+�B0 B1[B4�B6B4�B8�B8�B6�B9#BB&BA BCaBD�BEmBH�BK^BK�BM�BP|BO�BO�BS�BVBUgBXEB\�B`vBc�Bf�Bh
Bg�Bi�Bo�BquBq�Bw�Bz�B{�B�iB��B�kB��B�B��B��B��B�CB�hB��B��B��B�IB�IB�wB�B�UB��B��B��B�'B��B�'B��B��B��B��B�B�?B��B�0B��B��B�0B��BȴB�^BȴB��B�|B�sB��B�B�uB�B�%B�B�B��B��B�YB��B��B�	B�	B�rB�JB��B�DB�B��B	�B	B�B!B�B�B:B!B49B6�B.}B0UB3�B3�B3�BM�BHKBS�BU�BS�B]/BR�BO�B^5BbBc�Bf�Bn.Bw1Bv�By�B~\Bz�BwfBy>B}�B~�B�B�B��B��B��B��B��B��B��B�YB�rB��Bn�BoiBm�BpoBbB|PBxB|PBs�Bp�Bn.Bm�Bt�B�kB��B��B�^B�IB��B��B�dB��B��B�*B�B�@B�^B�LB��B��B��B��B�B��B��B�kB�$B��B�B��B��B}"B�Bw1Bx7Bu%Bo BlWBk�Bj�Be�Bj�Ba|B_�B`�B_BZBV8BS�BYBG�BF�B@�B6BF�B1�BEB9XBAUBR BK�B=<BF
BB�BF�BE�BA�BI�BJWBYBb�BSZBW�B\)BX�BXyBU�BR BYBT,BN�BV8BS�BT,B]/BU2BVmBdZB`vBe�Be�BW>BO�Bg�BiDBqBrGBaB=�B3gBZQB=<B<�B<6B1[B2-B%�B$@B.}B=B&�B-B%B(�B-�B;dB>BFsBN�BOvBOBS�BU�BRTBR�BR�BP�BO�BN�BMBNBP�BP�BM�BR�BVBM�BNpBL/BJ�BK)BJ�BK�BH�BEBD�BC�BAUBE�BB&B>wB=pB@NBA�B?}B;dB:�B:^B7�B;0B9�B6EB5�B3�B9�B5�B;�B7B�B�BBqB�BkB�B�B�B�B�B�B�BFB�B�BB�B{BtB�B�B�B�B�B+B�B	B_B�B�B�B�B��B��B��B�B��B�AB�MB�B��B�"B�PB�B��B�`B�TB�B�GB�AB�B�B�NB�QB��B�
B��B� B�mB�B��BÕB��B�dB�)B��B�^B��B��B�BB��B�KB�RB��B�9B�9B�3B�B�3B��B��B� B��B��B�IB�<B�B�^B��B��B�RB�XB�qB��B�bB��B��B�B��B.B��B��B~\B|�B}�B}�B|PB}�B}�Bz�Bz�ByrB{B|PBy	Bw�BxBt�BxBv�BwfB�_Bo Bn.Be,Bd�Bl�B_pBb�B_;B`AB`AB`B^B^�B[�B]�B[�B_�BZQBd�BXBP�BQ�BUgBQBQ�BVBK^BJ�BJ#BS�BL�BGEBE�BGBD3BB&BB�BA�B?�Bl�BFsB0�B+�B,qB+kB)�B'RB'�B)*B'RB&�B#B$@B �BBwB�BwB�B�B�B7B_B*B�B�B�BLB{BB�B�B'B\B~B
=BBYB
rB�BxB�B
��B
��B
��B
�\B
� B
��B
�.B
�8B
��B
�JB
�`B
�>B
��B
ƨB
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�RB
�0B
��B
�6B
��B
�^B
��B
�B
�?B
�B
��B
��B
�gB
�B
��B
�[B
��B
��B
�'B
��B
��B
��B
��B
��B
�B
�B
��B
� B
�3B
�B
��B
�B
�4B
�CB
�7B
�YB
�~B
�iB
t�B
u�B
{~B
p�B
k�B
h
B
qB
g8B
_�B
XEB
S�B
T�B
R�B
V8B
S�B
QB
N<B
OBB
I�B
K�B
YB
F�B
GEB
>BB
8B
9#B
3�B
1�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224956                            20230721224956AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495620230721224956  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495620230721224956QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495620230721224956QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               