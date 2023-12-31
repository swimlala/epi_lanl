CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:48Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   O�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       T�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       m�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` @   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   !   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   !   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � !   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   !�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   !�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    !�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        !�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        !�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       !�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    !�Argo profile    3.1 1.2 19500101000000  20230721224948  20230721224948  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�Ϊ�=q@�Ϊ�=q11  @���-� @���-� @2˼+��@2˼+���d��t�j�d��t�j11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?��?��H@@  @��\@��
@\@�G�@�p�A�RA   A+�A@  A`��A�Q�A�Q�A�  A�\)A�\)A�  A�Q�A�B   B  B  B�
B   B((�B0  B7�
B?�
BG�
BO�BW�
B_�Bg�Bo�
Bw�
B�  B�{B�(�B�(�B�  B�{B�  B��B��B�{B�(�B�{B�{B�{B�  B�{B�(�B�{B�{B�  B��B��
B��B�{B�{B�{B�{B�{B�(�B�  B�  B�{C 
=C��C  C
=C
=C
  C��C��C
=C
=C��C
=C
=C  C  C
=C   C!�C#��C&  C(  C)�C+�C-�C/�C1��C4  C6
=C8
=C9�HC<
=C>  C?�CA��CC��CE��CG��CI��CK��CN
=CP
=CR
=CT
=CV  CW��CY��C[�C]��C`  Cb  Cc��Cf  Ch
=Cj  Cl
=Cn
=Cp
=Cr  Ct  Cv
=Cx
=Cz�C|
=C~
=C�C�  C�C�C���C�  C�C�  C�
=C�C�  C�  C�  C�C�C�
=C���C���C���C�  C�  C���C���C���C�C�
=C�  C�  C�  C���C���C���C���C�  C���C���C�  C���C���C�  C���C���C���C���C�  C�C�  C���C�  C�  C���C���C�C�C�  C���C�  C�  C���C�  C�  C�C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�C�C�  C�C�C�  C�  C�  C�C�  C���C�  C���C��C���C���C�C�C�  C�C�C�C�  C�C�
=C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C���C�  C�
=C�
=C�\C�C�  C�  C�  C�C�  C���C���C�  D   D � D  D��D�D� D�qD� D  D��D�qD}qDD�D�D��D�qD}qD	�D	� D
�D
��D�D� D  D�DD��D�qD� D�qD� DD��D��D� D�D��D  D� D�D�DD�D�D� D�qD� D�D��D  D}qD�D��D�D��D��D}qD  D}qD�qDz�D��D}qD�qD }qD �qD!� D"�D"��D#  D#��D$�D$��D%  D%z�D%�qD&��D'�D'��D(  D(� D)�D)}qD)��D*� D+D+��D,  D,� D-  D-��D.�D.� D.�qD/}qD0  D0� D0�qD1��D2  D2z�D3  D3��D4�D4��D5D5�D6�D6}qD6�qD7}qD7�qD8� D9  D9� D:  D:}qD:�qD;��D<D<�D=�D=� D>�D>}qD>�qD?��D@�D@}qD@�qDA��DB  DB� DC  DC}qDD�DD��DE  DE��DF  DF� DG  DG� DH  DH��DI�DI}qDJ  DJ}qDJ�qDK� DL�DL��DM  DM��DM�qDN}qDO  DO� DP�DP��DQ�DQ}qDQ�qDR� DS�DS}qDS�qDT� DU�DU��DU�qDVz�DV�qDW��DX  DX}qDX�qDY}qDZ�DZ��D[�D[�D\  D\}qD\�qD]}qD^  D^� D^�qD_� D`�D`}qDa  Da��Db  Db��Dc�Dc��Dd  Dd� De  De��Df  Dfz�Df�qDg}qDh  Dh� Dh�qDiz�Di��Djz�Dj�qDk� Dl  Dl� Dm  Dm� Dn�Dn�Do�Do� Dp  Dp� Dp�qDqz�Dq��Dr� Ds�Ds��Ds�qDt��Du�Du}qDv  Dv��DwDw}qDx  Dx�Dy�Dy� Dz  Dz}qD{�D{��D|  D|� D}D}� D~  D~� D  D� D�qD�@ D�}qD���D���D�>�D�~�D�� D��D�AHD�~�D���D�  D�>�D�}qD��qD���D�AHD�� D���D�HD�B�D��HD�� D�  D�AHD��HD�D�  D�=qD�~�D�D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D��D�HD�AHD�� D�� D���D�=qD�~�D���D���D�>�D�~�D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD��HD�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD���D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?aG�?�  ?�{?Ǯ?�@�@(�@(��@B�\@W
=@h��@��\@�=q@�z�@��\@�=q@�z�@�G�@�=q@�z�@�  @�ff@�33@���A�\AQ�A�A�RAz�A��A(�A"�\A'�A*=qA/\)A333A7
=A<��AAG�AE�AJ�HAP  ATz�AZ=qA_\)Ac�
Aj=qAn�RAs�
Az�HA\)A��A���A�  A�=qA�p�A�Q�A��\A�ffA���A��A�
=A�G�A��A�\)A�G�A��
A�
=A���A��
A��RA��A��
A��RA���AÅAƸRA�G�A��HA�{A�  A�=qA�p�A�ffA�G�A�33A�z�A�
=A���A�=qA���A�{A�Q�A��A�33A�A�
=A�A�(�A�p�A��A��\A��
A�ffB Q�B�B�RB�B��B{B33B(�B	B
�HB�B�B{B
=B��B�B�RB(�Bp�B=qB�B��BB33B(�BG�B�RB�B ��B"{B#
=B$��B%p�B&�RB(  B(��B*=qB+\)B,(�B-B.�HB/�B1G�B2ffB3\)B4��B6{B6�HB8z�B9B:�RB<(�B=��B>ffB@  BA�BA�BC�BD��BE��BG
=BH  BI�BJ�RBK�BL��BN=qBO33BPQ�BQBR�\BT  BUp�BVffBX  BX��BZ=qB\  B\��B^{B_�B`��BaBc\)Bd��Be��Bg
=Bhz�Bip�Bj�RBlQ�BmG�Bn�\Bp  Bp��Br=qBs�
Bt��BuBw\)Bxz�By��B{
=B|z�B}p�B~�\B�  B���B��B��B�ffB�
=B��
B�=qB�
=B���B�(�B���B���B�  B���B��B�  B���B��B��
B���B�G�B�B�z�B��B���B�ffB��HB��B�=qB��RB�p�B�(�B���B�33B��B���B��B��B���B�
=B�B�ffB��HB���B�=qB��RB�p�B�(�B��\B�G�B�  B�z�B�
=B�B�z�B���B��B�(�B���B�p�B�  B���B�p�B��
B���B�\)B��
B�z�B�G�B�B�ffB��B���B�(�B��HB�p�B��B��RB�\)B�B��\B�33B�B�=qB�
=B���B�{B���B�p�B��B�z�B�G�B��
B�Q�B�
=B��B�(�B��HB��B�  B��RB�\)B�B�z�B��B��B�=qB���BÅB�  Bģ�B�\)B��
B�Q�B�
=B�B�(�BȸRBɅB��B�z�B�33B��
B�=qB���BͅB�(�B�z�B�33B��
B�Q�B���BхB�{Bҏ\B��B��
B�Q�BԸRB�\)B�  B�ffB�
=Bי�B�  B�z�B��BٮB�{B���B�p�B��B�ffB�33B�B�(�B���B�p�B��B�ffB��BᙚB�{B���B�G�B�B�Q�B�
=B�p�B�  B�RB�G�B�B�z�B���B�p�B�=qB�RB�33B��B��B�G�B��B�ffB��B�B�(�B��HB�\)B��
B��B��B�B�ffB���B�\)B�{B��RB��B��
B��\B���B��B�=qB��HB�\)B�  B��RB�G�B��
B�z�B�33B��C (�C �C �
C{CffCC{CQ�C��C
=C\)C�\C�HCG�C��C�
C(�C�C�HC{CffC��C�C\)C�RC{Cp�C�C��C	\)C	�RC	��C
G�C
��C
=CG�C��C�CQ�C�C�C33C�\C��C=qCz�C�
C=qC�C��C(�C�\C�
C�C�C�C=qC�C�
C=qC��C��C33C��C�HC(�Cz�C�HC=qC�\C�
C(�C�C�CG�C�C�HCG�C�\C�
C=qC��C�HC(�C��C��CG�C�C�HCQ�C��C�C33C��C  C\)C��C�C G�C �C!�C!p�C!C"{C"p�C"�
C#33C#�\C#�HC$33C$�\C$�C%Q�C%��C%�HC&=qC&�\C&��C'Q�C'�C(  C(Q�C(��C)  C)\)C)�RC*
=C*Q�C*��C+  C+ffC+�RC,  C,G�C,�C-
=C-\)C-��C-��C.G�C.�RC/{C/\)C/��C/��C0Q�C0�RC1{C1p�C1�RC2
=C2ffC2�C3
=C3p�C3C4�C4p�C4C5{C5ffC5C6�C6z�C6��C7(�C7�\C7�HC8=qC8�C8�
C9(�C9z�C9��C:�C:�C:�
C;=qC;��C;�HC<33C<�C<�
C=(�C=z�C=�
C>33C>�C>�
C?(�C?�C?�
C@(�C@�C@�
CA(�CA�CA�HCB33CB�CB�HCC33CC�CC�HCD=qCD�\CD�CE=qCE�CE�HCF33CF�CF�HCG33CG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��?��H@@  @��\@��
@\@�G�@�p�A�RA   A+�A@  A`��A�Q�A�Q�A�  A�\)A�\)A�  A�Q�A�B   B  B  B�
B   B((�B0  B7�
B?�
BG�
BO�BW�
B_�Bg�Bo�
Bw�
B�  B�{B�(�B�(�B�  B�{B�  B��B��B�{B�(�B�{B�{B�{B�  B�{B�(�B�{B�{B�  B��B��
B��B�{B�{B�{B�{B�{B�(�B�  B�  B�{C 
=C��C  C
=C
=C
  C��C��C
=C
=C��C
=C
=C  C  C
=C   C!�C#��C&  C(  C)�C+�C-�C/�C1��C4  C6
=C8
=C9�HC<
=C>  C?�CA��CC��CE��CG��CI��CK��CN
=CP
=CR
=CT
=CV  CW��CY��C[�C]��C`  Cb  Cc��Cf  Ch
=Cj  Cl
=Cn
=Cp
=Cr  Ct  Cv
=Cx
=Cz�C|
=C~
=C�C�  C�C�C���C�  C�C�  C�
=C�C�  C�  C�  C�C�C�
=C���C���C���C�  C�  C���C���C���C�C�
=C�  C�  C�  C���C���C���C���C�  C���C���C�  C���C���C�  C���C���C���C���C�  C�C�  C���C�  C�  C���C���C�C�C�  C���C�  C�  C���C�  C�  C�C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�C�C�  C�C�C�  C�  C�  C�C�  C���C�  C���C��C���C���C�C�C�  C�C�C�C�  C�C�
=C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C���C�  C�
=C�
=C�\C�C�  C�  C�  C�C�  C���C���C�  D   D � D  D��D�D� D�qD� D  D��D�qD}qDD�D�D��D�qD}qD	�D	� D
�D
��D�D� D  D�DD��D�qD� D�qD� DD��D��D� D�D��D  D� D�D�DD�D�D� D�qD� D�D��D  D}qD�D��D�D��D��D}qD  D}qD�qDz�D��D}qD�qD }qD �qD!� D"�D"��D#  D#��D$�D$��D%  D%z�D%�qD&��D'�D'��D(  D(� D)�D)}qD)��D*� D+D+��D,  D,� D-  D-��D.�D.� D.�qD/}qD0  D0� D0�qD1��D2  D2z�D3  D3��D4�D4��D5D5�D6�D6}qD6�qD7}qD7�qD8� D9  D9� D:  D:}qD:�qD;��D<D<�D=�D=� D>�D>}qD>�qD?��D@�D@}qD@�qDA��DB  DB� DC  DC}qDD�DD��DE  DE��DF  DF� DG  DG� DH  DH��DI�DI}qDJ  DJ}qDJ�qDK� DL�DL��DM  DM��DM�qDN}qDO  DO� DP�DP��DQ�DQ}qDQ�qDR� DS�DS}qDS�qDT� DU�DU��DU�qDVz�DV�qDW��DX  DX}qDX�qDY}qDZ�DZ��D[�D[�D\  D\}qD\�qD]}qD^  D^� D^�qD_� D`�D`}qDa  Da��Db  Db��Dc�Dc��Dd  Dd� De  De��Df  Dfz�Df�qDg}qDh  Dh� Dh�qDiz�Di��Djz�Dj�qDk� Dl  Dl� Dm  Dm� Dn�Dn�Do�Do� Dp  Dp� Dp�qDqz�Dq��Dr� Ds�Ds��Ds�qDt��Du�Du}qDv  Dv��DwDw}qDx  Dx�Dy�Dy� Dz  Dz}qD{�D{��D|  D|� D}D}� D~  D~� D  D� D�qD�@ D�}qD���D���D�>�D�~�D�� D��D�AHD�~�D���D�  D�>�D�}qD��qD���D�AHD�� D���D�HD�B�D��HD�� D�  D�AHD��HD�D�  D�=qD�~�D�D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D��D�HD�AHD�� D�� D���D�=qD�~�D���D���D�>�D�~�D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD��HD�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD���D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?aG�?�  ?�{?Ǯ?�@�@(�@(��@B�\@W
=@h��@��\@�=q@�z�@��\@�=q@�z�@�G�@�=q@�z�@�  @�ff@�33@���A�\AQ�A�A�RAz�A��A(�A"�\A'�A*=qA/\)A333A7
=A<��AAG�AE�AJ�HAP  ATz�AZ=qA_\)Ac�
Aj=qAn�RAs�
Az�HA\)A��A���A�  A�=qA�p�A�Q�A��\A�ffA���A��A�
=A�G�A��A�\)A�G�A��
A�
=A���A��
A��RA��A��
A��RA���AÅAƸRA�G�A��HA�{A�  A�=qA�p�A�ffA�G�A�33A�z�A�
=A���A�=qA���A�{A�Q�A��A�33A�A�
=A�A�(�A�p�A��A��\A��
A�ffB Q�B�B�RB�B��B{B33B(�B	B
�HB�B�B{B
=B��B�B�RB(�Bp�B=qB�B��BB33B(�BG�B�RB�B ��B"{B#
=B$��B%p�B&�RB(  B(��B*=qB+\)B,(�B-B.�HB/�B1G�B2ffB3\)B4��B6{B6�HB8z�B9B:�RB<(�B=��B>ffB@  BA�BA�BC�BD��BE��BG
=BH  BI�BJ�RBK�BL��BN=qBO33BPQ�BQBR�\BT  BUp�BVffBX  BX��BZ=qB\  B\��B^{B_�B`��BaBc\)Bd��Be��Bg
=Bhz�Bip�Bj�RBlQ�BmG�Bn�\Bp  Bp��Br=qBs�
Bt��BuBw\)Bxz�By��B{
=B|z�B}p�B~�\B�  B���B��B��B�ffB�
=B��
B�=qB�
=B���B�(�B���B���B�  B���B��B�  B���B��B��
B���B�G�B�B�z�B��B���B�ffB��HB��B�=qB��RB�p�B�(�B���B�33B��B���B��B��B���B�
=B�B�ffB��HB���B�=qB��RB�p�B�(�B��\B�G�B�  B�z�B�
=B�B�z�B���B��B�(�B���B�p�B�  B���B�p�B��
B���B�\)B��
B�z�B�G�B�B�ffB��B���B�(�B��HB�p�B��B��RB�\)B�B��\B�33B�B�=qB�
=B���B�{B���B�p�B��B�z�B�G�B��
B�Q�B�
=B��B�(�B��HB��B�  B��RB�\)B�B�z�B��B��B�=qB���BÅB�  Bģ�B�\)B��
B�Q�B�
=B�B�(�BȸRBɅB��B�z�B�33B��
B�=qB���BͅB�(�B�z�B�33B��
B�Q�B���BхB�{Bҏ\B��B��
B�Q�BԸRB�\)B�  B�ffB�
=Bי�B�  B�z�B��BٮB�{B���B�p�B��B�ffB�33B�B�(�B���B�p�B��B�ffB��BᙚB�{B���B�G�B�B�Q�B�
=B�p�B�  B�RB�G�B�B�z�B���B�p�B�=qB�RB�33B��B��B�G�B��B�ffB��B�B�(�B��HB�\)B��
B��B��B�B�ffB���B�\)B�{B��RB��B��
B��\B���B��B�=qB��HB�\)B�  B��RB�G�B��
B�z�B�33B��C (�C �C �
C{CffCC{CQ�C��C
=C\)C�\C�HCG�C��C�
C(�C�C�HC{CffC��C�C\)C�RC{Cp�C�C��C	\)C	�RC	��C
G�C
��C
=CG�C��C�CQ�C�C�C33C�\C��C=qCz�C�
C=qC�C��C(�C�\C�
C�C�C�C=qC�C�
C=qC��C��C33C��C�HC(�Cz�C�HC=qC�\C�
C(�C�C�CG�C�C�HCG�C�\C�
C=qC��C�HC(�C��C��CG�C�C�HCQ�C��C�C33C��C  C\)C��C�C G�C �C!�C!p�C!C"{C"p�C"�
C#33C#�\C#�HC$33C$�\C$�C%Q�C%��C%�HC&=qC&�\C&��C'Q�C'�C(  C(Q�C(��C)  C)\)C)�RC*
=C*Q�C*��C+  C+ffC+�RC,  C,G�C,�C-
=C-\)C-��C-��C.G�C.�RC/{C/\)C/��C/��C0Q�C0�RC1{C1p�C1�RC2
=C2ffC2�C3
=C3p�C3C4�C4p�C4C5{C5ffC5C6�C6z�C6��C7(�C7�\C7�HC8=qC8�C8�
C9(�C9z�C9��C:�C:�C:�
C;=qC;��C;�HC<33C<�C<�
C=(�C=z�C=�
C>33C>�C>�
C?(�C?�C?�
C@(�C@�C@�
CA(�CA�CA�HCB33CB�CB�HCC33CC�CC�HCD=qCD�\CD�CE=qCE�CE�HCF33CF�CF�HCG33CG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˩�A˸RA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A�ƨA�ȴA���A�ȴA�ĜAˮA˛�Aˏ\A�z�A�;dA�$�A��A�A�  A���A���AʾwA��yA�ffA��A�A���A��/A���A�ĜAȾwAȰ!Aȩ�Aȝ�AȓuAȏ\Aȉ7A�|�A�x�A�v�A�jA�^5A�Q�A�S�A�VA�VA�\)A� �AǾwA�1'A��A�bNA�+A��\A� �A�$�A���A��uA�-A��-A�XA�ZA�\)A�XA��;A�\)A���A��uA��9A��A��FA��A��PA�|�A�+A�S�A��A�;dA�&�A�|�A��`A�K�A��A��HA�9XA���A��uA���A�E�A��A���A�9XA�VA��A���A�A��7A�n�A��A��A~n�A}
=Ayl�Avv�At�uAqoAm�TAk�hAi�#Ag&�Ae�hAbA�Aa�A`5?A]�hA[��A[VAZ��AWƨAUC�AS��AQ;dAOS�AM��ALr�AK��AK?}AJ��AH9XAFQ�AEl�AD�AC\)AB�!AB-AA/A?�;A>-A<�RA;�A8�A4��A3�#A3
=A2�jA2n�A29XA0~�A.��A,��A*�\A(��A&ĜA$�RA"��A �+A�`A��A��A=qA�AXA/A�yAA��Al�A\)A7LA+A�A�/AZAn�AZA�`A7LA�PAdZA �AO�A~�A��A�A �A�RA��A	�;A	�Az�A�-Av�A�AA�-A�TA�TA�A�FAK�A��Az�AffA  A�A�AM�A�A �9A b@�@�j@�J@�hs@�/@��u@���@�S�@��T@�"�@�(�@�X@��H@�C�@��@�V@�^5@�p�@�O�@��@��@� �@�l�@��@�?}@�/@�A�@��@�O�@��@�l�@�E�@䛦@�+@�%@��
@�hs@�ff@�1'@�dZ@ץ�@�+@�V@�z�@�t�@�/@ϝ�@��H@�^5@ͺ^@�O�@�Ĝ@�j@�1@��@�ƨ@ˮ@˅@ʇ+@ɺ^@��@Ǿw@�o@ƸR@Ƈ+@�V@���@Ł@�/@��@Ĭ@�(�@���@�o@��@�ff@���@���@��@�(�@��@�\)@�33@�ȴ@�5?@��7@�&�@�V@��j@�  @�;d@��\@��-@�7L@���@�bN@�(�@�  @��F@�|�@�v�@�-@��@��#@�p�@���@��D@��@��P@�\)@�C�@�+@�@��@�~�@���@���@��@��@���@��@�Q�@��@�33@���@�V@�@�x�@�G�@���@�r�@�A�@��@���@�+@��\@��+@�^5@�$�@���@��#@�@���@�G�@��`@���@��@�bN@��;@���@�\)@�
=@��@��R@�v�@�@�p�@�?}@��@��@���@��@�  @��@���@�C�@�
=@�=q@��#@��7@�`B@��@���@�A�@� �@��@�dZ@���@�^5@�E�@�5?@�@��T@��@���@�9X@�  @���@��m@��P@�33@��y@�~�@�5?@��@�/@���@��/@���@���@�Z@�1'@�1'@�1'@�  @�|�@�33@��@���@��\@�=q@�{@��T@��7@��@�Ĝ@��D@�bN@�1'@��m@���@��F@�dZ@�+@���@�V@�=q@�$�@���@�@�G�@�7L@��@��/@���@�A�@��;@��@�|�@�o@��H@�ȴ@��!@���@�v�@�V@�5?@��@��@��^@��@�?}@���@���@�bN@�b@��;@���@�|�@�C�@�"�@�o@��@��H@���@���@��+@�~�@�V@�5?@�5?@�$�@�J@���@���@���@��`@��u@�A�@���@���@�t�@�K�@�+@��y@��R@�~�@�5?@��^@�hs@�G�@��@���@���@���@�Z@��@��@���@���@�K�@�
=@���@���@�~�@�M�@�J@��h@�/@��`@��u@�r�@�9X@�;@|�@l�@K�@~��@~�y@~�y@~ȴ@~E�@}�-@}@}`B@|�D@|Z@|1@{��@z�!@zM�@y��@yG�@y�@xr�@w��@wK�@v{@u��@up�@u/@u/@uV@t��@t��@t�/@t�j@t�D@tz�@t9X@t�@s��@s�m@sƨ@s�@sC�@s"�@s@r��@rM�@q�^@q%@p�u@pA�@o�;@o��@o\)@o+@n�y@nȴ@n�R@nff@m�-@m�@l�j@lZ@k�
@k��@kdZ@kS�@kS�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˗�A˓uA˶FA˼jA˶FA˲-A˶FA˾wA�ƨA���A���A�ƨA�ƨA�ĜA�ƨA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A�ȴA���A���A���A���A���A�ƨA���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ƨA�ĜA�ƨA���A�ƨA�ĜA���A���A�ȴA���A�ȴA�ĜA�ƨA���A���A���A���A�ĜA�ȴA���A�ȴA���A�ƨA�ȴA�ȴA�A�ƨA�ĜA˼jA�ĜA�ĜA�ƨA���A�A˸RA˲-AˬA˥�A˩�AˬA˩�A˲-A˰!A˰!A˰!Aˡ�A˗�A�z�A˕�A˗�Aˉ7AˑhAˏ\AˑhAˏ\A˓uAˇ+AˍPAˇ+A�|�A˅A�x�A�VA�Q�A�A�A�C�A�A�A�;dA�1'A�+A�+A�(�A�(�A�"�A�&�A�$�A� �A�"�A��A��A��A�VA�VA�VA�1A�JA�1A�A�A�  A���A�A�  A�  A�A�A���A�A���A���A���A���A���A���A���A���A���A���A���A�A���A���A���A��Aʴ9Aʙ�A�jA�C�A�&�A�  A��TA�ƨAɲ-Aɣ�AɋDA�p�A�hsA�VA�I�A�E�A�9XA�&�A�"�A��A�%A�%A�1A�A�A�%A�A�  A�A���A���A���A���A��A��mA��mA��HA��/A��;A��#A��
A��#A���A���A��
A���A���A���A�ƨA�ȴA�ƨA�A�ĜA�AȾwA�A�AȾwA���AȾwAȺ^AȸRAȴ9AȰ!AȰ!Aȩ�AȬAȬAȧ�AȬAȬAȧ�Aȥ�Aȩ�Aȥ�Aȟ�Aȟ�Aȝ�Aȕ�Aș�Aȗ�AȑhAȓuAȕ�Aȏ\AȓuAȓuAȏ\AȑhAȓuAȏ\AȋDAȋDAȍPAȋDAȇ+Aȉ7AȋDAȇ+AȅAȇ+AȁA�z�A�z�A�z�A�v�A�v�A�z�A�v�A�v�A�z�A�v�A�v�A�z�A�v�A�r�A�v�A�t�A�l�A�n�A�n�A�jA�ffA�hsA�dZA�bNA�bNA�^5A�ZA�XA�\)A�XA�Q�A�VA�S�A�O�A�S�A�S�A�O�A�S�A�S�A�Q�A�VA�VA�VA�S�A�XA�XA�S�A�VA�XA�S�A�S�A�VA�ZA�S�A�VA�ZA�VA�XA�`BA�bNA�^5A�`BA�ZA�Q�A�?}A�/A�(�A�oA�1A���A��A��HA���A�AǮAǝ�AǗ�AǑhAǃA�dZA�A�A�7LA��A���Aƴ9AƝ�A�?}A�;dAāA�-A�JA���Aò-Aá�A�r�A�XA�M�A�9XA�&�A�"�A�"�A�
=A°!A���A���A�r�A��A���A���A��PA��A�n�A�O�A�
=A���A�K�A�bA���A���A��A�bNA�`BA�XA�=qA�bA��A��!A���A���A��hA��uA��hA��PA��DA�r�A��A���A�XA�{A�p�A��/A���A�XA�33A�{A���A�I�A��FA�I�A��A�A�A�-A�{A�%A��mA���A�Q�A�
=A��\A�S�A��#A�ĜA���A�n�A��PA�dZA�v�A��-A��A�n�A� �A��A�`BA�;dA�M�A�dZA�r�A�l�A�?}A�oA���A��#A���A��-A��hA�~�A�hsA�ffA�`BA�A�A��A��#A��#A���A�A��FA��-A���A���A��uA��hA��7A�~�A�l�A�hsA�=qA�z�A��
A��jA���A��+A�1A���A�`BA�$�A��A�A��9A���A���A��uA��A��A��uA���A��A���A��wA��A��DA��+A�|�A� �A�G�A�~�A�M�A�9XA��A��A���A��+A�-A��HA�ĜA���A�x�A��yA�{A���A�`BA���A��#A��A�v�A�$�A��
A���A�|�A�VA�$�A��yA��hA�?}A��
A�v�A��FA��A��mA���A��uA�z�A�jA�G�A�(�A��A��A��#A�ƨA��A��uA�z�A�\)A�C�A�&�A�bA��A��
A��A�~�A��hA�XA�9XA�"�A�%A���A��FA��uA�r�A�bNA�VA�/A��A�1A���A��HA��wA��\A�x�A�\)A�33A��A�VA��FA�1A���A�hsA��A��A��+A�  A��mA�ƨA�jA�;dA��A�A���A�A���A�C�A�%A�n�A��A�;dA�&�A��A�bA��TA�C�A��A�K�A��A�
=A���A��A�A���A�S�A���A�dZA��A��hA�bNA� �A��PA�bNA�VA�9XA�JA��`A���A�n�A�O�A�A�A�33A�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A˩�A˸RA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A�ƨA�ȴA���A�ȴA�ĜAˮA˛�Aˏ\A�z�A�;dA�$�A��A�A�  A���A���AʾwA��yA�ffA��A�A���A��/A���A�ĜAȾwAȰ!Aȩ�Aȝ�AȓuAȏ\Aȉ7A�|�A�x�A�v�A�jA�^5A�Q�A�S�A�VA�VA�\)A� �AǾwA�1'A��A�bNA�+A��\A� �A�$�A���A��uA�-A��-A�XA�ZA�\)A�XA��;A�\)A���A��uA��9A��A��FA��A��PA�|�A�+A�S�A��A�;dA�&�A�|�A��`A�K�A��A��HA�9XA���A��uA���A�E�A��A���A�9XA�VA��A���A�A��7A�n�A��A��A~n�A}
=Ayl�Avv�At�uAqoAm�TAk�hAi�#Ag&�Ae�hAbA�Aa�A`5?A]�hA[��A[VAZ��AWƨAUC�AS��AQ;dAOS�AM��ALr�AK��AK?}AJ��AH9XAFQ�AEl�AD�AC\)AB�!AB-AA/A?�;A>-A<�RA;�A8�A4��A3�#A3
=A2�jA2n�A29XA0~�A.��A,��A*�\A(��A&ĜA$�RA"��A �+A�`A��A��A=qA�AXA/A�yAA��Al�A\)A7LA+A�A�/AZAn�AZA�`A7LA�PAdZA �AO�A~�A��A�A �A�RA��A	�;A	�Az�A�-Av�A�AA�-A�TA�TA�A�FAK�A��Az�AffA  A�A�AM�A�A �9A b@�@�j@�J@�hs@�/@��u@���@�S�@��T@�"�@�(�@�X@��H@�C�@��@�V@�^5@�p�@�O�@��@��@� �@�l�@��@�?}@�/@�A�@��@�O�@��@�l�@�E�@䛦@�+@�%@��
@�hs@�ff@�1'@�dZ@ץ�@�+@�V@�z�@�t�@�/@ϝ�@��H@�^5@ͺ^@�O�@�Ĝ@�j@�1@��@�ƨ@ˮ@˅@ʇ+@ɺ^@��@Ǿw@�o@ƸR@Ƈ+@�V@���@Ł@�/@��@Ĭ@�(�@���@�o@��@�ff@���@���@��@�(�@��@�\)@�33@�ȴ@�5?@��7@�&�@�V@��j@�  @�;d@��\@��-@�7L@���@�bN@�(�@�  @��F@�|�@�v�@�-@��@��#@�p�@���@��D@��@��P@�\)@�C�@�+@�@��@�~�@���@���@��@��@���@��@�Q�@��@�33@���@�V@�@�x�@�G�@���@�r�@�A�@��@���@�+@��\@��+@�^5@�$�@���@��#@�@���@�G�@��`@���@��@�bN@��;@���@�\)@�
=@��@��R@�v�@�@�p�@�?}@��@��@���@��@�  @��@���@�C�@�
=@�=q@��#@��7@�`B@��@���@�A�@� �@��@�dZ@���@�^5@�E�@�5?@�@��T@��@���@�9X@�  @���@��m@��P@�33@��y@�~�@�5?@��@�/@���@��/@���@���@�Z@�1'@�1'@�1'@�  @�|�@�33@��@���@��\@�=q@�{@��T@��7@��@�Ĝ@��D@�bN@�1'@��m@���@��F@�dZ@�+@���@�V@�=q@�$�@���@�@�G�@�7L@��@��/@���@�A�@��;@��@�|�@�o@��H@�ȴ@��!@���@�v�@�V@�5?@��@��@��^@��@�?}@���@���@�bN@�b@��;@���@�|�@�C�@�"�@�o@��@��H@���@���@��+@�~�@�V@�5?@�5?@�$�@�J@���@���@���@��`@��u@�A�@���@���@�t�@�K�@�+@��y@��R@�~�@�5?@��^@�hs@�G�@��@���@���@���@�Z@��@��@���@���@�K�@�
=@���@���@�~�@�M�@�J@��h@�/@��`@��u@�r�@�9X@�;@|�@l�@K�@~��@~�y@~�y@~ȴ@~E�@}�-@}@}`B@|�D@|Z@|1@{��@z�!@zM�@y��@yG�@y�@xr�@w��@wK�@v{@u��@up�@u/@u/@uV@t��@t��@t�/@t�j@t�D@tz�@t9X@t�@s��@s�m@sƨ@s�@sC�@s"�@s@r��@rM�@q�^@q%@p�u@pA�@o�;@o��@o\)@o+@n�y@nȴ@n�R@nff@m�-@m�@l�j@lZ@k�
@k��@kdZ@kS�@kS�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˗�A˓uA˶FA˼jA˶FA˲-A˶FA˾wA�ƨA���A���A�ƨA�ƨA�ĜA�ƨA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A�ȴA���A���A���A���A���A�ƨA���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ƨA�ĜA�ƨA���A�ƨA�ĜA���A���A�ȴA���A�ȴA�ĜA�ƨA���A���A���A���A�ĜA�ȴA���A�ȴA���A�ƨA�ȴA�ȴA�A�ƨA�ĜA˼jA�ĜA�ĜA�ƨA���A�A˸RA˲-AˬA˥�A˩�AˬA˩�A˲-A˰!A˰!A˰!Aˡ�A˗�A�z�A˕�A˗�Aˉ7AˑhAˏ\AˑhAˏ\A˓uAˇ+AˍPAˇ+A�|�A˅A�x�A�VA�Q�A�A�A�C�A�A�A�;dA�1'A�+A�+A�(�A�(�A�"�A�&�A�$�A� �A�"�A��A��A��A�VA�VA�VA�1A�JA�1A�A�A�  A���A�A�  A�  A�A�A���A�A���A���A���A���A���A���A���A���A���A���A���A�A���A���A���A��Aʴ9Aʙ�A�jA�C�A�&�A�  A��TA�ƨAɲ-Aɣ�AɋDA�p�A�hsA�VA�I�A�E�A�9XA�&�A�"�A��A�%A�%A�1A�A�A�%A�A�  A�A���A���A���A���A��A��mA��mA��HA��/A��;A��#A��
A��#A���A���A��
A���A���A���A�ƨA�ȴA�ƨA�A�ĜA�AȾwA�A�AȾwA���AȾwAȺ^AȸRAȴ9AȰ!AȰ!Aȩ�AȬAȬAȧ�AȬAȬAȧ�Aȥ�Aȩ�Aȥ�Aȟ�Aȟ�Aȝ�Aȕ�Aș�Aȗ�AȑhAȓuAȕ�Aȏ\AȓuAȓuAȏ\AȑhAȓuAȏ\AȋDAȋDAȍPAȋDAȇ+Aȉ7AȋDAȇ+AȅAȇ+AȁA�z�A�z�A�z�A�v�A�v�A�z�A�v�A�v�A�z�A�v�A�v�A�z�A�v�A�r�A�v�A�t�A�l�A�n�A�n�A�jA�ffA�hsA�dZA�bNA�bNA�^5A�ZA�XA�\)A�XA�Q�A�VA�S�A�O�A�S�A�S�A�O�A�S�A�S�A�Q�A�VA�VA�VA�S�A�XA�XA�S�A�VA�XA�S�A�S�A�VA�ZA�S�A�VA�ZA�VA�XA�`BA�bNA�^5A�`BA�ZA�Q�A�?}A�/A�(�A�oA�1A���A��A��HA���A�AǮAǝ�AǗ�AǑhAǃA�dZA�A�A�7LA��A���Aƴ9AƝ�A�?}A�;dAāA�-A�JA���Aò-Aá�A�r�A�XA�M�A�9XA�&�A�"�A�"�A�
=A°!A���A���A�r�A��A���A���A��PA��A�n�A�O�A�
=A���A�K�A�bA���A���A��A�bNA�`BA�XA�=qA�bA��A��!A���A���A��hA��uA��hA��PA��DA�r�A��A���A�XA�{A�p�A��/A���A�XA�33A�{A���A�I�A��FA�I�A��A�A�A�-A�{A�%A��mA���A�Q�A�
=A��\A�S�A��#A�ĜA���A�n�A��PA�dZA�v�A��-A��A�n�A� �A��A�`BA�;dA�M�A�dZA�r�A�l�A�?}A�oA���A��#A���A��-A��hA�~�A�hsA�ffA�`BA�A�A��A��#A��#A���A�A��FA��-A���A���A��uA��hA��7A�~�A�l�A�hsA�=qA�z�A��
A��jA���A��+A�1A���A�`BA�$�A��A�A��9A���A���A��uA��A��A��uA���A��A���A��wA��A��DA��+A�|�A� �A�G�A�~�A�M�A�9XA��A��A���A��+A�-A��HA�ĜA���A�x�A��yA�{A���A�`BA���A��#A��A�v�A�$�A��
A���A�|�A�VA�$�A��yA��hA�?}A��
A�v�A��FA��A��mA���A��uA�z�A�jA�G�A�(�A��A��A��#A�ƨA��A��uA�z�A�\)A�C�A�&�A�bA��A��
A��A�~�A��hA�XA�9XA�"�A�%A���A��FA��uA�r�A�bNA�VA�/A��A�1A���A��HA��wA��\A�x�A�\)A�33A��A�VA��FA�1A���A�hsA��A��A��+A�  A��mA�ƨA�jA�;dA��A�A���A�A���A�C�A�%A�n�A��A�;dA�&�A��A�bA��TA�C�A��A�K�A��A�
=A���A��A�A���A�S�A���A�dZA��A��hA�bNA� �A��PA�bNA�VA�9XA�JA��`A���A�n�A�O�A�A�A�33A�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B($B)�B(�B)_B(�B(�B(�B(�B(�B(�B)*B(�B)_B)_B)�B)�B)�B)�B)�B)�B+�B,�B-�B/B5tB9XB@BUgBwfB��B��B�jBQ�B�CBʌB��B� B��B��B�.B��B��B�cB�cB�.B�cB��B��B��B��B��B��B 4BBB �BABYBMB�JB��BߤB�B�[B�HB��B�B��B�zB�!B�YB��B�7B��B��B�DB{BtBo�BZBJ#BR BkBaBHKB9�B�B
	B�(B�dBӏB��B�-B�:B�$B��B�YBo BW?BG�B'�B(B 4B
�?B
�-B
�?B
�LB
��B
.B
\]B
AUB
;0B
.}B
B
�B	�JB	�
B	��B	� B	��B	�*B	�$B	��B	��B	��B	�B	cB	yrB	y�B	^jB	\]B	V�B	GzB	F?B	>�B	9$B	6FB	3hB	+kB	!bB	IB	�B	�B	�B	�B	
�B	{B��B��B�)B��B�HB̘B�B�mB��B�'B��B�XB�B��B�B��B�bB��B�kB��B�B��B�B�B��B�oB��B�B��B��B�SB�$B��B�qB�-B��B��B�9B�NB��B��B	�B	B	�B	:B	B		�B�(B�B�/B�B�B�B��B�oB��B�DB�B	B	�B	B	YB	+B	eB	_B	�B	B	B	�B	�B	�B	B	hB	�B	B	�B	B	�B	\B	B	�B		�B�]B	DB	�B	&�B	.IB	*eB	(XB	B�B	H�B	H�B	M�B	NB	M�B	Q�B	PHB	S[B	YB	d�B	j�B	k�B	ffB	e,B	e`B	dZB	^�B	Z�B	W
B	S&B	K�B	B�B	D�B	J�B	J�B	K�B	M�B	S�B	S�B	V�B	YKB	[�B	`B	`BB	a|B	bNB	b�B	b�B	e`B	e�B	e`B	kQB	l�B	poB	tB	u�B	y	B	{�B	}�B	��B	�{B	�B	��B	��B	�JB	��B	��B	�.B	��B	�FB	��B	��B	��B	��B	�~B	�OB	��B	�-B	�B	��B	��B	��B	�6B	�B	��B	��B	��B	�nB	�zB	��B	��B	��B	�*B	�<B	�}B	�B	�B	��B	��B	��B	�mB	�B	�?B	�?B	�?B	�tB	�B	ɺB	�^B	��B	��B	��B	�<B	�6B	��B	�HB	��B	�B	�BB	��B	бB	��B	�[B	��B	�aB	��B	�B	�B	�EB	�KB	�)B	ܒB	��B	��B	��B	ݘB	�;B	�vB	�B	��B	�HB	�B	��B	��B	�B	��B	�fB	�mB	�B	��B	�]B	��B	��B	�5B	��B	�|B	�B	�TB	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B
  B
 �B
 �B
B
GB
{B
GB
�B
MB
SB
�B
+B
1B
	lB

	B
fB
�B
	7B
	�B

�B

=B

=B

�B
�B
PB
VB
(B
�B
(B
�B
 B
 B
hB
4B
�B
 B
 B
�B
�B
oB
:B
oB
@B
�B
B
{B
FB
FB
FB
{B
�B
�B
�B
MB
�B
�B
�B
B
�B
�B
�B
YB
$B
�B
+B
_B
_B
_B
�B
�B
1B
eB
�B
kB
�B
B
CB
�B
�B
 �B
 �B
 �B
 �B
!-B
!�B
!�B
!bB
!�B
!�B
!�B
!�B
"hB
"�B
"�B
"�B
"4B
$@B
"�B
"hB
"�B
!bB
 �B
 �B
 �B
 �B
"4B
"4B
#:B
$tB
#�B
$@B
%�B
%zB
%�B
'RB
'�B
'RB
'�B
'�B
(�B
($B
(�B
(XB
)_B
*�B
*eB
*eB
*eB
)�B
*0B
*eB
)�B
*�B
+6B
+�B
+�B
,qB
,�B
,�B
,�B
-CB
.IB
/B
0UB
1[B
1�B
2aB
2aB
2-B
1�B
1'B
0�B
0�B
0�B
1[B
1�B
2aB
4B
3�B
3�B
5B
4�B
4�B
4�B
4�B
4�B
5?B
5�B
6B
6FB
6FB
6FB
6FB
6�B
6�B
7B
6�B
6�B
7�B
8B
8�B
9�B
9�B
:*B
:�B
;dB
;dB
;�B
<6B
<6B
<6B
<�B
=B
=qB
=B
=B
=�B
=�B
=�B
=�B
=�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B)�B(XB$@B($B*�B*eB)�B'RB)*B(�B'�B+B($B)_B)�B($B(�B)�B'�B(�B)*B($B)�B'�B)�B)*B'�B*0B)�B(XB(XB*�B($B(�B*eB'�B(�B)�B($B(�B)�B($B)*B)�B(XB*eB)�B(�B(�B)�B(�B)*B*0B)_B(XB*0B(�B(XB*�B)*B*eB)_B(�B*0B*eB(XB)_B*�B(�B)�B*�B*0B)_B*�B*0B)*B*�B)*B(�B)�B(�B*�B)�B)_B*�B)�B*0B+�B($B)�B*eB(�B+B.B,�B,�B-�B(�B+kB+�B+kB,=B+B+kB-wB/�B.�B+�B-B/B+kB-B+�B.�B+�B/�B*eB.B.}B,qB33B:�B1�B49B2�B4nB5?B7�B7�B7�B8RB7�B:�B9$B9�B;dB:�B=<B=B=<BB�BC-BC�BJ�BH�BK�BS�B[�B`vBe�BncBsMBuZBzBzB�B��B�hB�B�XB��B��B�qB�wB��B�B�!B�'B��B��B�$B��B҉B��B��B�B$�B1[BFtBT,Bc�Bm�Bv�B��B��B��B��B�3B��B��B�3BŢB�XB�sB��B�?BٴB��BخBںB�WBیBޞB�B��BߤB�B��B�B�2B��B�B��B�B��B�VB��B�"B�.B�VB��B�cB�.B�]B iB��B�cB  B�"B��B�cB�]B��B��B��B��B��B��B��B��B��B��B��B��B  B �B��B��BB�.B��B 4B��B��B 4B��B��B iB��B��B 4B�.B�(B�cB iB��B�]B��B iB 4B�.BBB��B�cB iB��B�]B  B��B�(B  B  B��B �BB�cB  B �B��B��BB��B��B��B��B��B��B�cB�]B��B�cB��B�(B�.B �B�cB�cB;B��B 4B�B  B 4B�BB �B �BoB �B �BBB 4B�B�B;B��BB;B��B;B iB�]BB{B�B�B�B	lB�BB�BYB+BMB1B	BGB�B{BB��B�.B�B�xB��B�>B�DB�B�]B�B1BVB�AB��B�|B� B��B�B�vB��B�QB��B�B��B�
B��B1[B�B�TB�XB�zB��B�HB��B��B��B��B�tB��B��B�*B�zB�BB��B�[B��B�nB�B�zB��B��B��B�B�B�6B�B��B��B��B��B��B�}B�B�$B�*B�B��B��B��B�=B��B�kB�UB��B�oB�FB�B�YB��B�@B�B��B�B�B��B��B��B�B��Bp�B��B�hB�B�B��B��B� B~�B�B��B��B��B��B��B��B�uB�@B��B��B�DB�1B�xB�DB�GB|�B{JB{�By>BzDBv�Bu�Bt�Bu%Bs�Br|BsMBpoBn/Bp;BcBjB]�BW
BZ�Be�Bc�BLdBU2BOBBM6BG�BH�BF?BG�BIRBFtBF?BP�BhsBlWBl�Bl�Bo BgBdZBp;B�Bq�BS&BQ�BU�BT�BN�BI�BY�BB�B>wB7�B:�BM�B9�B&�B1�BMBeB�B1B1B�BJB	�B	lB	�B_B�B �B �BuB\B�B��B�yB�QB�/B�KB��B��B�2B�?BѷB�NBуB�pB�B�jBȀB��B��B��B��BȴB�vB�BB��B��B��B�B�FB��B��B�7B��B��B�	B�B��B�B��B�B��B��B��B��B�{B�B��B}�BzDB�BpoBh�B}"BdZBYBbBb�BUgBQBOBBPBC�BI�BQB>BYB9�B5�B%FB!�B�B&LB.IB#�BSB�BYBB
��B�B'�BB
��B
� B
��B
چB
�B
��B
�B
ɺB
ÖB
�RB
�B
�B
�aB
��B
�dB
�tB
��B
�R4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                        44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B$tB%�B$�B%�B%B%B%FB%FB%FB%B%zB%FB%�B%�B%�B&B&LB&B&B&LB'�B)*B)�B+kB1�B5�B<jBQ�Bs�B��B�BںBNB��B��B�8B�pB��B��B�~B��B�JB��B��B�~B��B�B��B��B�B�B��B��B�VB�VB�"B��B�B �B��B��B��B��B��B��B��B�^B�IB��B�qB��B�B��B�=B��B��BwfBpoBl"BVmBFsBNpBglB]cBD�B6BBYB�xBٴB��B�?B�}B��B�tB�B��BkPBS�BC�B#�BxB
��B
ӏB
�}B
��B
��B
��B
{~B
X�B
=�B
7�B
*�B
bB
�B	��B	�ZB	�NB	�pB	�B	�zB	�tB	�0B	��B	�'B	�iB	{�B	u�B	u�B	Z�B	X�B	R�B	C�B	B�B	:�B	5tB	2�B	/�B	'�B	�B	�B	LB	�B	IB		�B	�B��B��B�B�yB�2B̘B��B�aB��B�B�wB�B��B�gB�B�RB��B��B�B��B�FB�nB��B�\B�\B�'B��B��B�VB�.B��B��B�tB��B��B�}B��B��B҉BޞB�2B�(B�.B	VB	�B	�B		kB	�B�xB��B�B�B��B�B�%B�B��B��B�fB	oB	
	B	\B	�B	{B	�B	�B	�B	eB	_B	�B	$B	B	nB	�B	�B	\B	@B	bB	�B	�B	eB	�B	%B��B	�B	�B	#B	*�B	&�B	$�B	?HB	D�B	D�B	I�B	JWB	I�B	M�B	L�B	O�B	U�B	aGB	g8B	g�B	b�B	a|B	a�B	`�B	Z�B	V�B	SZB	OvB	G�B	?HB	@�B	GB	GB	G�B	I�B	PB	PHB	R�B	U�B	XB	\]B	\�B	]�B	^�B	_B	_B	a�B	a�B	a�B	g�B	iDB	l�B	poB	rGB	uYB	x7B	zDB	}"B	�B	�oB	�B	�B	��B	�	B	�IB	�~B	�!B	��B	�B	��B	�$B	�0B	��B	��B	�CB	�}B	�[B	��B	��B	�B	��B	�dB	�<B	��B	��B	��B	��B	�9B	�B	�EB	�zB	��B	��B	�jB	�jB	��B	�B	�HB	��B	�[B	B	B	B	��B	�aB	�
B	ǮB	�KB	�KB	�#B	ʌB	ɆB	�#B	̘B	�5B	�dB	˒B	�/B	�B	�<B	ϫB	�B	бB	� B	�TB	�`B	ԕB	՛B	�yB	��B	�B	�KB	�KB	��B	ۋB	��B	�cB	�/B	ݘB	�B	�AB	�GB	��B	�NB	�B	�B	�B	�>B	�B	�B	�JB	�B	�.B	��B	�iB	�B	�B	�AB	�B	�MB	�B	�SB	��B	�`B	�+B	��B	�B	�7B	�B	��B	�B	�PB	��B	��B	�\B	��B	��B	��B
 4B
 �B
�B
B
{B
�B
�B
YB
�B
�B
�B
%B
�B
�B
�B
�B
�B
	�B

�B
xB
�B
xB
IB
PB
PB
�B
�B
�B
PB
PB
�B
!B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
:B
B
�B
�B
�B
B
B
nB
�B
@B
B
�B
tB
B
{B
�B
�B
�B
LB
LB
�B
�B
B
�B
�B
_B
�B
B
B
B
IB
B
�B
}B
�B
�B
�B
OB
OB
OB
OB
�B
�B
!B
!B
�B
 �B
�B
�B
!B
�B
IB
B
�B
IB
�B
�B
�B
 �B
 'B
 �B
"3B
!�B
"3B
#�B
$B
#�B
$B
$B
$�B
$tB
%B
$�B
%�B
&�B
&�B
&�B
&�B
%�B
&�B
&�B
&LB
&�B
'�B
'�B
($B
(�B
(�B
)*B
)*B
)�B
*�B
+kB
,�B
-�B
.IB
.�B
.�B
.}B
-�B
-wB
-B
-B
-B
-�B
-�B
.�B
0UB
0 B
0 B
1[B
0�B
0�B
0�B
0�B
1'B
1�B
2-B
2aB
2�B
2�B
2�B
2�B
2�B
33B
3gB
33B
33B
3�B
4mB
5?B
5�B
6EB
6zB
6�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
9XB
9�B
9XB
9XB
9�B
9�B
9�B
:)B
:)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B&LB$�B �B$tB'B&�B&LB#�B%zB$�B$B'RB$tB%�B%�B$tB%FB&B#�B%B%zB$tB&LB$@B&LB%zB$B&�B&B$�B$�B&�B$tB%B&�B$B%FB%�B$tB%B&LB$tB%zB%�B$�B&�B&B%B%FB&B$�B%zB&�B%�B$�B&�B%FB$�B&�B%zB&�B%�B%B&�B&�B$�B%�B&�B%FB&LB'B&�B%�B'B&�B%zB'B%zB%FB&LB%FB&�B%�B%�B'B&LB&�B($B$tB&B&�B$�B'RB*dB)*B(�B*0B$�B'�B'�B'�B(�B'RB'�B)�B+�B+6B($B)^B+kB'�B)^B($B+B($B,<B&�B*dB*�B(�B/�B7KB.IB0�B/OB0�B1�B49B3�B4B4�B3�B6�B5tB6B7�B7KB9�B9XB9�B?B?}B@BF�BEBHKBPBXEB\�Ba�Bj�Bo�Bq�Bv`Bv`B�oB�@B��B�nB��B�LB�0B��B��B�B�dB�qB�wB��B��B�tB�B��B�B��B
	B!-B-�BB�BP|B_�BjBsMB~�B�'B��B�OB��B�B�)B��B��BƨB��B�,BӏB�B�2B��B�
BקB��B��B�WB�B��B��B�.B��B�B��B�`B��B�lB�1B��B��B�rB�~B��B�B��B�~B��B��B�JB��B�PB�rB��B��B��B�DB��B�JB�JB�JB�DB��B�B�DB�B��B��B�PB��B�JB�JB�VB�~B��B��B��B�JB��B�B�B��B�B��B��B�~B�xB��B��B��B��B�JB��B��B�~B�VB�VB��B��B��B�JB��B�PB�B�xB�PB�PB��B��B�VB��B�PB�"B�JB�JB�VB�B��B��B��B��B��B��B��B�B��B�JB�xB�~B��B��B��B��B�B��B�(B�PB��B�(B�VB�"B�"B��B�"B��B�\B�VB��B��B��B��B��B�VB��B�B��B��B��B�VB��B 4B�B �B�BGBoBMB�B{B �B�BSB��B �B��B�VB�B�~B�(B��B��B��B��B�(B�B	B�B
�B�B�B��B�pB�AB��B��B�)B֡B�EB��B� B�ZB�B-�B�fBߤBƨB��B�B��B��B��B�HB�,B��B�B��B�zB��B��B�'B��B�OB��B�gB��B�OB�B�6B�^B�RB��B�RB��B�*B��B� B�*B��B�^B�tB�zB�hB��B�B�B��B�LB��B��B�B��B��B�VB��B��B��B�nB�LB�eB�\B��B�'B�1B�nB}�Bl�B�B��B�hB�kB�!B�7B|PBz�B�eB�+B�B�B�B�B�FB��B��B�=B�7B��B��B��B��B�Bx�Bw�Bx7Bu�Bv�BsBrGBp�BquBo�Bn�Bo�Bl�BjBl�B{�Bf�BZBSZBV�Ba�B_�BH�BQ�BK�BI�BD3BE9BB�BC�BE�BB�BB�BMBd�Bh�Bh�Bh�BkPBcTB`�Bl�B�eBn.BOvBM�BQ�BQNBK)BE�BV8B?B:�B3�B7BJ#B6EB"�B.B�B�B@B�B�B�B�B�B�B%B�B@B��B�"B��B�B�WB�2B��B֡B�B՛B�B�,BтBӏB�B͞B��B��B�QBɺB��B�9B�&B�NB��B�B��B��B��B�B�IB�[B��B��B�B��B�B�FB�YB�bB��B�\B�!B�hB��B�%B�MB��B�B|B��By�Bv�B~\Bl�Be,ByrB`�BUgB^iB_;BQ�BMjBK�BLdB?�BE�BMjB:^BUgB6EB2-B!�BOBCB"�B*�B�B�B�B�B
�\B
�B�B#�B
�bB
�B
�PB
��B
��B
�gB
�EB
�B
�
B
��B
ŢB
�jB
�[B
��B
�KB
��B
��B
��B
��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                        44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224948                            20230721224948AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122494820230721224948  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122494820230721224948QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122494820230721224948QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               