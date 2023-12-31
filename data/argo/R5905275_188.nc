CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:57Z creation      
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
resolution        =���   axis      Z        p  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  c,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p ?<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ^�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223257  20230426223257  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @��X���@��X���11  @�ʓ�C�@�ʓ�C�@)�Sy���@)�Sy����cu\=�x�cu\=�x11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?�  @   @=p�@�G�@�  @�  @޸RA   A��A   A,��A@��A`��A�  A�  A�Q�A�Q�A�  AϮA�  A�  B   B�
B�
B�
B�
B'�
B/�
B7�
B?�
BH  BO�
BW�
B`  Bh(�Bp(�Bx  B�{B�(�B�  B��B��B��B��B�  B�  B��B�{B�{B�  B�{B�{B�{B�{B�{B�  B��B�{B�{B�=qBۮB��
B�  B�  B�  B��B�  B�  B��B��C��C  C��C��C	��C
=C{C
=C
=C
=C�C��C  C
=C��C�C!��C#��C%��C(  C*  C+��C-�C/��C2  C3��C5��C7��C:  C<
=C>
=C@
=CA��CC��CE��CG��CJ
=CL
=CN
=CP
=CQ��CT  CV
=CX  CZ
=C[��C]�C_��Cb  Cc��Cf  Ch{Cj  Ck��Cn  Co��Cq�Cs�Cv  Cx{Cz  C{��C~  C�C�  C�  C���C���C���C���C���C���C�  C�C�
=C�  C���C���C�  C�  C���C���C�  C�  C���C���C�  C�  C�C�C�  C�  C�  C�  C�C���C���C���C�  C�C�C�  C�C�
=C�C�  C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�  C�  C�
=C�C�
=C�
=C���C���C���C���C���C�  C���C���C���C���C���C���C���C���C�C�C�C�C�C�
=C�C�  C���C���C�  C���C���C���C�  C�C�C�C�C�C�C�
=C�  C���C�  C���C���C���C���C���C�  C�C�  C�  C�  C���C�  C���C���C���C�  C�C�C�
=C�C�  C���C���C���C���C���C���C���C�D �D �D�D��DD� D��Dz�D��D}qD�qD� D  D��DD��D  D��D	D	��D	�qD
��DD��D  D�D�D��D  D}qD�D� D�qD��D  D� D  D� D  D� D�qD}qD�D��D  D� D�qD�D�D��D  D}qD�D� D�qD��DD��D�D� D�D� D�qD}qD�qD ��D!�D!��D"  D"}qD#  D#�D$�D$� D%�D%� D&�D&��D&�qD'� D(�D(� D(�qD)� D*�D*��D+�D+��D,�D,� D-  D-� D.�D.��D/�D/� D0�D0��D1�D1��D2�D2� D3  D3� D4  D4� D5  D5� D6  D6}qD6�qD7z�D7��D8� D9�D9z�D:  D:� D:�RD;xRD;��D<z�D<�qD=� D>�D>�D?D?�D@  D@��DA�DA}qDA�qDB}qDC  DC� DD  DD� DD�qDE}qDE�qDF� DG�DG� DH  DH� DI  DI� DI�qDJz�DJ�qDK� DL  DL� DM  DM}qDN  DN��DO�DO� DO�qDP� DQ  DQ� DR  DR��DS�DS��DT  DT� DT�qDU� DV  DV��DW�DW� DW�qDX� DY�DY��DZ�DZ��D[  D[� D[�qD\}qD\�qD]z�D]��D^z�D^�qD_� D`  D`}qDa  Da}qDb  Db��Dc�Dc}qDd  Dd��Dd�qDe� Df�Df}qDf�qDg� Dh  Dh��Di�Di� Dj  Dj��Dk  Dk� Dl  Dl}qDm  Dm}qDm�qDn� Do  Do� Dp�Dp� Dp�qDqz�Dq�qDr}qDs  Ds��Dt  Dt}qDu  Du��Dv  Dv}qDw  Dw� Dx  Dx�Dy  Dy� Dz  Dz� D{�D{��D|�D|}qD|�qD}� D~  D~� D~�qD� D�HD�AHD�~�D��qD�  D�@ D�� D���D��qD�@ D�� D�� D�HD�AHD���D�� D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D�~�D���D���D�@ D��HD���D��qD�>�D�� D�� D���D�@ D�~�D�� D�  D�@ D�� D��HD���D�>�D�~�D�� D�HD�AHD��HD�� D�HD�@ D�~�D�� D���D�=qD�}qD�� D�HD�@ D�~�D��qD�  D�B�D��HD��HD�  D�@ D�~�D���D�HD�@ D�}qD��qD�  D�B�D��HD�D�  D�=qD�}qD��qD��qD�@ D�� D���D�  D�@ D��HD���D��)D�=qD�~�D�� D�HD�@ D�~�D�� D�  D�B�D��HD�� D�HD�@ D�}qD���D�  D�@ D�~�D�� D���D�@ D��HD�� D�  D�>�D��HD��HD�HD�AHD�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D��HD�HD�B�D��HD���D��qD�=qD�~�D���D��qD�@ D���D�D�HD�@ D�� D��HD��D�@ D�� D�� D��qD�>�D�~�D���D�  D�@ D��HD��HD���D�@ D���D�� D���D�@ D�~�D��qD���D�@ D�� D��HD�  D�=qD�~�D�� D�  D�AHD���D�� D���D�AHD��HD���D�  D�B�D���D��HD�  D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�>�D�~�D�� D��qD�=qD�� D�� D�HD�AHD��HD��HD���D�@ D�� D��HD�HD�AHD���D�D��D�@ D��HD��HD�HD�AHD�~�D�� D�  D�@ D��HD��HD���D�>�D�~�D�� D�  D�AHD�� D���D���D�@ D�� D�� D�  D�@ D D��HD�  D�@ DÁHD�� D�  D�@ DĀ D�� D�HD�AHD�~�Dž�D�HD�AHD�~�Dƾ�D�HD�B�DǁHD�� D�  D�>�DȁHD��HD�  D�@ DɁHD��HD�  D�@ D�~�Dʾ�D�  D�AHDˁHD��HD���D�=qD�~�D̾�D��qD�>�D̀ D�� D�  D�@ D΀ D�� D���D�>�D�~�DϾ�D��qD�>�DЀ D�� D�HD�@ D�~�D��HD�HD�AHDҀ DҾ�D�  D�>�DӀ D��HD�  D�@ DԀ DԾ�D���D�@ DՀ D�� D�  D�@ D�~�DֽqD�  D�AHD׀ D�� D�HD�AHD؀ D��HD�HD�B�Dق�D��HD�HD�B�DځHD��HD�  D�>�Dۀ D�� D�  D�>�D�~�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��HD��D�AHD߀ D߾�D�HD�AHD��HD�� D�  D�@ D� D�� D�  D�@ D�HD�� D���D�@ D� D�� D���D�>�D�~�D�qD�  D�>�D�~�D徸D���D�@ D�}qD澸D���D�>�D�~�D羸D�  D�=qD�}qD�� D�HD�@ D�~�D�� D�  D�=qD�}qD��HD�HD�>�D�HD��HD���D�@ D� D�� D�  D�@ D� D�� D�  D�>�D�}qD�� D�  D�AHD� DﾸD�  D�>�D�}qD�� D�HD�AHD� D�� D�  D�AHD� D�qD��)D�>�D�D�D�  D�@ D� D��HD�HD�@ D��HD��HD�HD�@ D�~�D�� D�HD�@ D�}qD�� D�HD�@ D�}qD�� D��D�@ D�� D�D��
?��?W
=?��?��R?Ǯ?�@   @�@&ff@5@E�@Y��@n{@}p�@�ff@�\)@��H@��
@��@�z�@�  @�=q@�33@�(�@�ff@��@�Q�A ��Az�A
=qA\)A�\AA��A�RA!�A%�A(��A.{A333A5A8��A=p�AA�ADz�AH��AN{AQG�ATz�AX��A]p�A`��Adz�Ag
=Ak�Ao\)As�
AvffAz�HA\)A���A��HA�p�A�  A�=qA��
A�{A���A�33A��A�\)A��A�z�A��RA���A��HA�p�A��A���A��
A�{A�Q�A�=qA�z�A�
=A�G�A��A�p�A�  A\A���A�ffA���A˅A�p�AϮA��A�(�A�
=A���A�33A�p�A�  A�\A���A�
=A�G�A��
A�ffA��A�\A��A��A�=qA�z�A�ffB z�BB33B(�BG�B�RB  B	G�B
ffB�B��B�B\)B��B��B�HB(�B��B�RB�
B��BffB�B��B�B
=B z�B!B#
=B$(�B%G�B&�\B'�
B)�B*{B+33B,��B-�B/
=B0  B1G�B2�\B3�
B4��B5�B733B8��B9B;
=B<  B=�B>�\B?�
BA�BB=qBC\)BD��BF{BG\)BH��BI��BK
=BLz�BMBN�HBP  BQp�BR�HBT  BU�BV�\BW�
BYG�BZ�\B[�B\��B^ffB_�B`��Bb{Bc�Bd��Bf=qBg\)Bh��Bi�Bk\)Bl��Bm�Bo33Bpz�BqBs33Btz�Bu��Bv�HBxQ�By��B{
=B|(�B}p�B~�HB�(�B��RB�G�B��B���B�\)B�  B���B�33B��
B��\B�33B��B�z�B��B�B�ffB��B�B�ffB���B���B�Q�B�
=B��B�=qB���B�p�B�(�B��HB�p�B�  B���B�p�B�{B��RB�G�B��
B�z�B�33B��
B�z�B�
=B���B�=qB���B��B�=qB���B�p�B�(�B���B�p�B�  B���B�G�B��B���B�G�B��B�ffB�
=B�B�ffB��B�B�Q�B���B��B�(�B���B��B�{B��RB�\)B�{B��RB�\)B�  B��\B��B��
B��\B�33B��B�z�B��B��B�Q�B��B�B�ffB�
=B��B�=qB��HB��B�(�B��HB�p�B�{B��RB�33B��
B�z�B�33B��
B�z�B�
=Bř�B�(�B���B�\)B�  Bȣ�B�33B�B�=qB��HB˅B�(�B̸RB�G�B�B�Q�B���Bϙ�B�Q�B��HBхB�  Bң�B�G�B�  Bԣ�B�33B�B�Q�B���BׅB�{B؏\B���B�G�Bٙ�B��B�(�B�ffBڣ�B���B��HB��HB��HB��HB���B�
=B��B��B��B��B��B��B��B�33B�33B�G�B�G�B�\)B�\)B�\)B�G�B�G�B�\)B�\)B�p�B�p�BۅB�p�B�p�B�p�B�\)B�\)B�\)B�\)B�p�B�p�BۅBۅBۅBۅBۅBۅB�p�B�p�B�p�B�p�B�p�BۅBۙ�BۮBۙ�Bۙ�BۮBۙ�Bۙ�Bۙ�Bۙ�Bۙ�Bۙ�Bۙ�BۮBۮB�B�B�B�B�BۮBۮBۮBۮBۮBۮBۮB�B�B�BۮBۮBۮBۮBۙ�BۅBۅBۅB�p�B�p�B�p�BۅBۅBۙ�Bۙ�Bۙ�BۅBۙ�BۅBۙ�Bۙ�BۮB�B��
B��B��B�{B�=qB�Q�B�Q�B�z�B܏\BܸRB���B��HB���B���B�
=B�33B�\)B݅BݮB��
B��
B�  B�{B�=qB�Q�B�ffBޏ\Bޣ�B���B���B��B�G�B�\)B߅B߮B��
B�  B�{B�=qB�ffB��\B��B���B���B��B�G�B�p�BᙚB�B�  B�(�B�z�B��B��HB��B�\)B�B��B�=qB�z�B���B��B�\)B�B��B�=qB�z�B���B��B�\)B�B�  B�Q�B��B���B�G�B陚B��B�(�B�z�B��HB��B�p�B��
B�(�B�z�B��HB�33B�p�B��
B�(�B�\B��HB�G�BB�  B�ffB���B�33B�B�{B�z�B��HB�\)B�B�(�B�\B���B�\)B��
B�(�B��\B���B�\)B�B�(�B��\B���B�p�B��
B�=qB���B��B���B�  B�ffB���B�33B���B�{B�ffB���B�33B���C   C =qC p�C ��C �HC{CG�C�\CC  C=qCp�C��C�HC{CG�Cz�C�C�C�C\)C��C�
C{CG�C�CC  C33Cp�C��C�HC{CG�C�\C��C{CQ�C�\C�
C	
=C	G�C	�C	C
  C
=qC
�C
C
=CG�C�\C��C
=CG�C�CC  C=qC�C��C{C\)C��C�HC(�CffC�C�C33Cz�C��C{C\)C��C�C33Cp�C�RC  CG�C�\C�
C(�Cz�CC
=CQ�C�\C�
C�CffC�C  CQ�C��C�HC33Cz�CC
=CQ�C��C�C=qC�\C�
C(�Cp�C�C  CQ�C��C��CG�C�\C��C�CffC�RC{C\)C�C��C =qC �C ��C!�C!ffC!�RC"{C"\)C"�C"��C#=qC#z�C#��C$�C$ffC$�RC%
=C%\)C%�C&  C&=qC&�\C&�
C'33C'�C'�HC((�C(z�C(�RC){C)ffC)�RC*
=C*\)C*��C*�C+=qC+�\C+�C,=qC,�\C,�
C-�C-ffC-C.{C.p�C.�RC/  C/G�C/��C/�C0G�C0��C0�C133C1�C1��C2�C2p�C2C3�C3\)C3��C3��C4=qC4��C4�C5=qC5�C5�
C6�C6ffC6�C7
=C7Q�C7�C8  C8G�C8�\C8�
C9�C9ffC9��C:{C:ffC:�C:��C;=qC;�C;�
C<(�C<z�C<��C={C=\)C=�C>  C>\)C>�RC?  C?G�C?�\C?�
C@33C@�C@�
CA(�CAp�CA�RCB  CBQ�CB�CC  CCG�CC�\CC�HCD(�CDp�CD��CE(�CEp�CE�RCF  CF\)CF�CG  CGG�CG�\CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114441114444441414144441414144444441414444444444111441444444414144444444444144444444444441414441414111141114111111411111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       ?�  @   @=p�@�G�@�  @�  @޸RA   A��A   A,��A@��A`��A�  A�  A�Q�A�Q�A�  AϮA�  A�  B   B�
B�
B�
B�
B'�
B/�
B7�
B?�
BH  BO�
BW�
B`  Bh(�Bp(�Bx  B�{B�(�B�  B��B��B��B��B�  B�  B��B�{B�{B�  B�{B�{B�{B�{B�{B�  B��B�{B�{B�=qBۮB��
B�  B�  B�  B��B�  B�  B��B��C��C  C��C��C	��C
=C{C
=C
=C
=C�C��C  C
=C��C�C!��C#��C%��C(  C*  C+��C-�C/��C2  C3��C5��C7��C:  C<
=C>
=C@
=CA��CC��CE��CG��CJ
=CL
=CN
=CP
=CQ��CT  CV
=CX  CZ
=C[��C]�C_��Cb  Cc��Cf  Ch{Cj  Ck��Cn  Co��Cq�Cs�Cv  Cx{Cz  C{��C~  C�C�  C�  C���C���C���C���C���C���C�  C�C�
=C�  C���C���C�  C�  C���C���C�  C�  C���C���C�  C�  C�C�C�  C�  C�  C�  C�C���C���C���C�  C�C�C�  C�C�
=C�C�  C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�  C�  C�
=C�C�
=C�
=C���C���C���C���C���C�  C���C���C���C���C���C���C���C���C�C�C�C�C�C�
=C�C�  C���C���C�  C���C���C���C�  C�C�C�C�C�C�C�
=C�  C���C�  C���C���C���C���C���C�  C�C�  C�  C�  C���C�  C���C���C���C�  C�C�C�
=C�C�  C���C���C���C���C���C���C���C�D �D �D�D��DD� D��Dz�D��D}qD�qD� D  D��DD��D  D��D	D	��D	�qD
��DD��D  D�D�D��D  D}qD�D� D�qD��D  D� D  D� D  D� D�qD}qD�D��D  D� D�qD�D�D��D  D}qD�D� D�qD��DD��D�D� D�D� D�qD}qD�qD ��D!�D!��D"  D"}qD#  D#�D$�D$� D%�D%� D&�D&��D&�qD'� D(�D(� D(�qD)� D*�D*��D+�D+��D,�D,� D-  D-� D.�D.��D/�D/� D0�D0��D1�D1��D2�D2� D3  D3� D4  D4� D5  D5� D6  D6}qD6�qD7z�D7��D8� D9�D9z�D:  D:� D:�RD;xRD;��D<z�D<�qD=� D>�D>�D?D?�D@  D@��DA�DA}qDA�qDB}qDC  DC� DD  DD� DD�qDE}qDE�qDF� DG�DG� DH  DH� DI  DI� DI�qDJz�DJ�qDK� DL  DL� DM  DM}qDN  DN��DO�DO� DO�qDP� DQ  DQ� DR  DR��DS�DS��DT  DT� DT�qDU� DV  DV��DW�DW� DW�qDX� DY�DY��DZ�DZ��D[  D[� D[�qD\}qD\�qD]z�D]��D^z�D^�qD_� D`  D`}qDa  Da}qDb  Db��Dc�Dc}qDd  Dd��Dd�qDe� Df�Df}qDf�qDg� Dh  Dh��Di�Di� Dj  Dj��Dk  Dk� Dl  Dl}qDm  Dm}qDm�qDn� Do  Do� Dp�Dp� Dp�qDqz�Dq�qDr}qDs  Ds��Dt  Dt}qDu  Du��Dv  Dv}qDw  Dw� Dx  Dx�Dy  Dy� Dz  Dz� D{�D{��D|�D|}qD|�qD}� D~  D~� D~�qD� D�HD�AHD�~�D��qD�  D�@ D�� D���D��qD�@ D�� D�� D�HD�AHD���D�� D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D�~�D���D���D�@ D��HD���D��qD�>�D�� D�� D���D�@ D�~�D�� D�  D�@ D�� D��HD���D�>�D�~�D�� D�HD�AHD��HD�� D�HD�@ D�~�D�� D���D�=qD�}qD�� D�HD�@ D�~�D��qD�  D�B�D��HD��HD�  D�@ D�~�D���D�HD�@ D�}qD��qD�  D�B�D��HD�D�  D�=qD�}qD��qD��qD�@ D�� D���D�  D�@ D��HD���D��)D�=qD�~�D�� D�HD�@ D�~�D�� D�  D�B�D��HD�� D�HD�@ D�}qD���D�  D�@ D�~�D�� D���D�@ D��HD�� D�  D�>�D��HD��HD�HD�AHD�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D��HD�HD�B�D��HD���D��qD�=qD�~�D���D��qD�@ D���D�D�HD�@ D�� D��HD��D�@ D�� D�� D��qD�>�D�~�D���D�  D�@ D��HD��HD���D�@ D���D�� D���D�@ D�~�D��qD���D�@ D�� D��HD�  D�=qD�~�D�� D�  D�AHD���D�� D���D�AHD��HD���D�  D�B�D���D��HD�  D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�>�D�~�D�� D��qD�=qD�� D�� D�HD�AHD��HD��HD���D�@ D�� D��HD�HD�AHD���D�D��D�@ D��HD��HD�HD�AHD�~�D�� D�  D�@ D��HD��HD���D�>�D�~�D�� D�  D�AHD�� D���D���D�@ D�� D�� D�  D�@ D D��HD�  D�@ DÁHD�� D�  D�@ DĀ D�� D�HD�AHD�~�Dž�D�HD�AHD�~�Dƾ�D�HD�B�DǁHD�� D�  D�>�DȁHD��HD�  D�@ DɁHD��HD�  D�@ D�~�Dʾ�D�  D�AHDˁHD��HD���D�=qD�~�D̾�D��qD�>�D̀ D�� D�  D�@ D΀ D�� D���D�>�D�~�DϾ�D��qD�>�DЀ D�� D�HD�@ D�~�D��HD�HD�AHDҀ DҾ�D�  D�>�DӀ D��HD�  D�@ DԀ DԾ�D���D�@ DՀ D�� D�  D�@ D�~�DֽqD�  D�AHD׀ D�� D�HD�AHD؀ D��HD�HD�B�Dق�D��HD�HD�B�DځHD��HD�  D�>�Dۀ D�� D�  D�>�D�~�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��HD��D�AHD߀ D߾�D�HD�AHD��HD�� D�  D�@ D� D�� D�  D�@ D�HD�� D���D�@ D� D�� D���D�>�D�~�D�qD�  D�>�D�~�D徸D���D�@ D�}qD澸D���D�>�D�~�D羸D�  D�=qD�}qD�� D�HD�@ D�~�D�� D�  D�=qD�}qD��HD�HD�>�D�HD��HD���D�@ D� D�� D�  D�@ D� D�� D�  D�>�D�}qD�� D�  D�AHD� DﾸD�  D�>�D�}qD�� D�HD�AHD� D�� D�  D�AHD� D�qD��)D�>�D�D�D�  D�@ D� D��HD�HD�@ D��HD��HD�HD�@ D�~�D�� D�HD�@ D�}qD�� D�HD�@ D�}qD�� D��D�@ D�� D�D��
?��?W
=?��?��R?Ǯ?�@   @�@&ff@5@E�@Y��@n{@}p�@�ff@�\)@��H@��
@��@�z�@�  @�=q@�33@�(�@�ff@��@�Q�A ��Az�A
=qA\)A�\AA��A�RA!�A%�A(��A.{A333A5A8��A=p�AA�ADz�AH��AN{AQG�ATz�AX��A]p�A`��Adz�Ag
=Ak�Ao\)As�
AvffAz�HA\)A���A��HA�p�A�  A�=qA��
A�{A���A�33A��A�\)A��A�z�A��RA���A��HA�p�A��A���A��
A�{A�Q�A�=qA�z�A�
=A�G�A��A�p�A�  A\A���A�ffA���A˅A�p�AϮA��A�(�A�
=A���A�33A�p�A�  A�\A���A�
=A�G�A��
A�ffA��A�\A��A��A�=qA�z�A�ffB z�BB33B(�BG�B�RB  B	G�B
ffB�B��B�B\)B��B��B�HB(�B��B�RB�
B��BffB�B��B�B
=B z�B!B#
=B$(�B%G�B&�\B'�
B)�B*{B+33B,��B-�B/
=B0  B1G�B2�\B3�
B4��B5�B733B8��B9B;
=B<  B=�B>�\B?�
BA�BB=qBC\)BD��BF{BG\)BH��BI��BK
=BLz�BMBN�HBP  BQp�BR�HBT  BU�BV�\BW�
BYG�BZ�\B[�B\��B^ffB_�B`��Bb{Bc�Bd��Bf=qBg\)Bh��Bi�Bk\)Bl��Bm�Bo33Bpz�BqBs33Btz�Bu��Bv�HBxQ�By��B{
=B|(�B}p�B~�HB�(�B��RB�G�B��B���B�\)B�  B���B�33B��
B��\B�33B��B�z�B��B�B�ffB��B�B�ffB���B���B�Q�B�
=B��B�=qB���B�p�B�(�B��HB�p�B�  B���B�p�B�{B��RB�G�B��
B�z�B�33B��
B�z�B�
=B���B�=qB���B��B�=qB���B�p�B�(�B���B�p�B�  B���B�G�B��B���B�G�B��B�ffB�
=B�B�ffB��B�B�Q�B���B��B�(�B���B��B�{B��RB�\)B�{B��RB�\)B�  B��\B��B��
B��\B�33B��B�z�B��B��B�Q�B��B�B�ffB�
=B��B�=qB��HB��B�(�B��HB�p�B�{B��RB�33B��
B�z�B�33B��
B�z�B�
=Bř�B�(�B���B�\)B�  Bȣ�B�33B�B�=qB��HB˅B�(�B̸RB�G�B�B�Q�B���Bϙ�B�Q�B��HBхB�  Bң�B�G�B�  Bԣ�B�33B�B�Q�B���BׅB�{B؏\B���B�G�Bٙ�B��B�(�B�ffBڣ�B���B��HB��HB��HB��HB���B�
=B��B��B��B��B��B��B��B�33B�33B�G�B�G�B�\)B�\)B�\)B�G�B�G�B�\)B�\)B�p�B�p�BۅB�p�B�p�B�p�B�\)B�\)B�\)B�\)B�p�B�p�BۅBۅBۅBۅBۅBۅB�p�B�p�B�p�B�p�B�p�BۅBۙ�BۮBۙ�Bۙ�BۮBۙ�Bۙ�Bۙ�Bۙ�Bۙ�Bۙ�Bۙ�BۮBۮB�B�B�B�B�BۮBۮBۮBۮBۮBۮBۮB�B�B�BۮBۮBۮBۮBۙ�BۅBۅBۅB�p�B�p�B�p�BۅBۅBۙ�Bۙ�Bۙ�BۅBۙ�BۅBۙ�Bۙ�BۮB�B��
B��B��B�{B�=qB�Q�B�Q�B�z�B܏\BܸRB���B��HB���B���B�
=B�33B�\)B݅BݮB��
B��
B�  B�{B�=qB�Q�B�ffBޏ\Bޣ�B���B���B��B�G�B�\)B߅B߮B��
B�  B�{B�=qB�ffB��\B��B���B���B��B�G�B�p�BᙚB�B�  B�(�B�z�B��B��HB��B�\)B�B��B�=qB�z�B���B��B�\)B�B��B�=qB�z�B���B��B�\)B�B�  B�Q�B��B���B�G�B陚B��B�(�B�z�B��HB��B�p�B��
B�(�B�z�B��HB�33B�p�B��
B�(�B�\B��HB�G�BB�  B�ffB���B�33B�B�{B�z�B��HB�\)B�B�(�B�\B���B�\)B��
B�(�B��\B���B�\)B�B�(�B��\B���B�p�B��
B�=qB���B��B���B�  B�ffB���B�33B���B�{B�ffB���B�33B���C   C =qC p�C ��C �HC{CG�C�\CC  C=qCp�C��C�HC{CG�Cz�C�C�C�C\)C��C�
C{CG�C�CC  C33Cp�C��C�HC{CG�C�\C��C{CQ�C�\C�
C	
=C	G�C	�C	C
  C
=qC
�C
C
=CG�C�\C��C
=CG�C�CC  C=qC�C��C{C\)C��C�HC(�CffC�C�C33Cz�C��C{C\)C��C�C33Cp�C�RC  CG�C�\C�
C(�Cz�CC
=CQ�C�\C�
C�CffC�C  CQ�C��C�HC33Cz�CC
=CQ�C��C�C=qC�\C�
C(�Cp�C�C  CQ�C��C��CG�C�\C��C�CffC�RC{C\)C�C��C =qC �C ��C!�C!ffC!�RC"{C"\)C"�C"��C#=qC#z�C#��C$�C$ffC$�RC%
=C%\)C%�C&  C&=qC&�\C&�
C'33C'�C'�HC((�C(z�C(�RC){C)ffC)�RC*
=C*\)C*��C*�C+=qC+�\C+�C,=qC,�\C,�
C-�C-ffC-C.{C.p�C.�RC/  C/G�C/��C/�C0G�C0��C0�C133C1�C1��C2�C2p�C2C3�C3\)C3��C3��C4=qC4��C4�C5=qC5�C5�
C6�C6ffC6�C7
=C7Q�C7�C8  C8G�C8�\C8�
C9�C9ffC9��C:{C:ffC:�C:��C;=qC;�C;�
C<(�C<z�C<��C={C=\)C=�C>  C>\)C>�RC?  C?G�C?�\C?�
C@33C@�C@�
CA(�CAp�CA�RCB  CBQ�CB�CC  CCG�CC�\CC�HCD(�CDp�CD��CE(�CEp�CE�RCF  CF\)CF�CG  CGG�CG�\CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114441114444441414144441414144444441414444444444111441444444414144444444444144444444444441414441414111141114111111411111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��mA��yA��A��A��A��A��A��A��A��A��A���A��A��;A�ȴAͺ^A�~�A�VA��A��A��#A���A���A�ƨA���A̼jA̼jA���A���A��yA�JA� �A�/A�&�A�A�1'A�G�A�\)A�`BA�l�A�jA�jA�p�A�r�A�t�A�v�AͅA͝�A�A��;A��HA��/A;wAͬAͅA̅A��`A��^A��DA��HA��A�&�A�XA���A���A�dZA�~�A~�+A{��AxE�As7LAlffAhQ�Ac&�A`��A]|�AZE�AW��AT�`AS�PAP�AN�AM�AMC�AK��AHE�AE/ABVAA�A@�A?dZA>�jA?oA>�HA>JA=33A<�A<5?A;?}A7+A6��A7"�A6�+A5��A4~�A3�A3�;A3&�A2�!A2M�A/��A.��A-��A-�-A-�PA,��A+A*A(ĜA'l�A%�mA%33A%��A%"�A#�A#�A"��A"��A"ZA"�A!�^A!��A!XA!&�A z�A �9A �!A �A��AhsA;dAĜA��A��A �A�#A��A=qA�#A��At�AO�AoA��A��A��AM�A  AƨAt�A��AffAE�A$�AbA�mA�;AAXAG�A��A��A|�A?}AVA�AbNA  A�PA&�A�yA��A��A�AA�;A�^A��AhsAG�A&�A
=A�A�A~�AI�AbA�;A��A�hAO�A&�A�9A �A��A;dA��A�+A��A��An�A1A�-AC�A
�A
��A
9XA
  A	��A	?}A��A �A��AXA��A�A=qA�wA;dA�A��A�`A��A�DAn�AZA1'A�TAXA��A��A��A�PAp�AK�A+AVA �jA v�A Q�A �@��;@�t�@�dZ@�33@���@�\)@��@��@�A�@��
@�b@��m@��P@��@���@�?}@���@�Q�@��m@�C�@��H@�@���@���@�Q�@���@��@���@�h@���@�@��@�;d@��H@�~�@�V@�@�`B@���@�9@��@�@�;d@��y@���@��@��@�bN@�1'@��;@�w@�@�+@�O�@�V@�j@߶F@���@�{@ݺ^@ݡ�@�V@�9X@�K�@�$�@٩�@���@ו�@��@�^5@ա�@���@�j@Ӿw@�
=@���@��/@�z�@�9X@϶F@��H@���@�%@̴9@̋D@ˮ@��@�ȴ@ʧ�@�ff@���@ɩ�@ɑh@�/@�1'@ǥ�@�"�@���@�-@Ł@�X@��@ă@�(�@��m@å�@��@�-@��^@��@�z�@���@��w@�\)@��R@�^5@�=q@���@�O�@�/@���@�Z@��@��y@�v�@�V@��@�%@�bN@��@���@�t�@���@�M�@�$�@��^@�/@��u@���@�\)@�
=@���@�=q@�G�@�z�@�bN@� �@��;@�S�@��H@�@�p�@�&�@���@��@�j@�9X@� �@���@��\@�$�@��^@�7L@�%@��/@�Q�@�1'@��@��
@�33@��H@��R@�-@��#@�@��@�9X@�ƨ@�|�@�C�@���@�=q@�$�@��#@��@��@��`@��u@�j@�A�@��w@��@�K�@���@�~�@�$�@���@��@�&�@��@��@���@�r�@�(�@��
@�ƨ@���@��P@�l�@�"�@��R@���@�v�@�E�@�x�@�%@��D@�9X@�b@��m@��
@��@�t�@�+@��y@��+@�V@��@��-@�x�@��@�r�@�A�@�1@�ƨ@���@�K�@��y@�ȴ@��!@���@�J@��@���@�/@�%@��j@��D@�Q�@�|�@�;d@�o@��@��\@�M�@��@���@��-@���@�hs@�&�@��@�V@���@�Z@�1@���@��w@���@��@�\)@�;d@���@��@�ȴ@���@�5?@��#@�p�@���@�j@�9X@��;@��@���@�K�@���@�~�@��@��T@�@���@�p�@�?}@��/@��D@�1'@�1@��;@�ƨ@���@�S�@�
=@�ȴ@��\@�~�@�n�@�5?@���@��7@�7L@��@��@���@���@��@�j@� �@�;@|�@K�@+@~�y@~�+@}�-@|�/@|z�@|9X@{�m@{dZ@{@z��@z��@z�\@z-@yhs@xĜ@w��@w�@v�@v�R@v�+@vV@v@u��@u��@u`B@s�F@rn�@q��@q��@q�7@p�9@p�u@p�u@p�@p�@p�@pr�@pbN@pb@oK�@m�@l�j@l1@kƨ@k��@k��@kt�@k33@j�@j^5@j�@i�#@ix�@h��@g�@gK�@f�y@f��@fv�@fff@fff@fff@fV@f{@e�@e�@d�D@c�F@c�@cdZ@cS�@c"�@c@b�H@b��@b��@b�!@b~�@a��@a��@ax�@a7L@`�9@`bN@`  @_��@_��@_\)@_;d@_+@_�@^��@^�@^�R@^$�@]�-@]/@\��@\1@[�@[C�@["�@Z�H@Z��@Z^5@Yx�@Xr�@W�@W|�@Wl�@W+@W
=@V�@V��@Vv�@V5?@U�T@U�-@Up�@UV@T�@T�j@T�@Tj@T�@S�F@S"�@R��@R��@R��@R�\@R~�@Rn�@R=q@Q�#@Q��@P��@PA�@O�@OK�@N�@Nff@NE�@M@M�@L��@LZ@L�@K�
@K�@KC�@Ko@J�H@J��@J�!@J^5@I��@I��@I�7@Ix�@I7L@H��@H�u@G�;@F��@F�+@F5?@E�@E�h@D��@Dz�@DZ@C�
@C33@B�\@B~�@Bn�@B-@A��@A�7@A&�@@�9@@�@@  @?�P@>�@>ff@>@=@=�h@=�@=`B@<�/@<j@<�@;�m@;��@:�@:^5@:�@:J@9�@9��@8��@8b@7�;@7|�@6�y@6��@6v�@6ff@65?@6@5�T@5�T@5�T@5�T@5�T@5�-@5�@4�/@3�m@3�@3o@2�@2��@2~�@1��@1hs@1X@0�`@0bN@0A�@/�;@/�@.��@.5?@-��@-V@,�/@,�j@,z�@,j@,I�@,(�@+�m@+�
@+��@+��@+t�@+33@+33@+33@*��@*J@)��@)7L@(��@(1'@'�@'�;@'�P@';d@'
=@'
=@'
=@&�@&ȴ@&��@&5?@%�T@%@%�-@%�-@%�@%?}@%/@%V@$�@$�@#��@#dZ@#S�@#33@"��@"~�@"-@"�@!�^@!G�@ Ĝ@ bN@ 1'@�@�@l�@��@ȴ@�R@v�@5?@�T@��@�j@�D@I�@1@ƨ@o@��@��@^5@=q@�@�@�^@��@x�@hs@&�@�`@�9@�@Q�@�@
=@�y@�@ȴ@�R@ff@{@�T@��@�@`B@/@��@��@�D@z�@Z@I�@(�@�m@�
@��@t�@33@o@�@��@��@�!@��@�\@~�@M�@J@�#@��@��@X@%@��@�`@Ĝ@r�@1'@�w@��@\)@+@+@�@��@�y@�@ȴ@�R@��@��@��@�+@V@{@��@�h@�h@�@?}@�j@z�@j@j@Z@Z@9X@(�@(�@��@C�@o@
�@
�@
��@
�!@
~�@
M�@
-@
�@
J@
J@	�@	��@	x�@	&�@��@�9@�@�@r�@r�@r�@r�@bN@A�@b@�@|�@|�@|�A��HA��TA��`A��`A��yA��yA��mA��mA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��yA��A��A��HA���A���A���A�ȴA���A���A��
A�AͼjAͰ!Aͺ^AͬAͰ!A�A�AͲ-A;wA;wA͡�A�r�A�n�A�ffA�l�A�jA�l�A�n�A�ZA�\)A�K�A�E�A�G�A�?}A�"�A��A�$�A��A�A���A���A��A��A��A��A��A��A��mA��;A��#A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A�ƨA�ĜA�ĜA�ƨA�ƨA���A̾wA���A�A���A̾wA̺^A̼jA̾wA̾wA̼jA̺^A̼jA���A̾wA̼jA̼jA̼jA̾wA̾wA̼jA̼jA���A�A�A���A���A�ĜA���A���A���A���A��A��;A��HA��`A��yA���A���A�  A�  A�A�VA�oA��A��A��A��A�"�A�$�A�$�A�"�A�+A�1'A�/A�-A�1'A�33A�33A�1'A�/A�&�A��A�{A�JA�A���A���A���A�  A� �A�1'A�7LA�5?A�/A�+A�5?A�;dA�E�A�K�A�M�A�O�A�S�A�\)A�^5A�^5A�ZA�ZA�\)A�^5A�^5A�\)A�^5A�dZA�l�A�l�A�n�A�n�A�jA�jA�l�A�l�A�l�A�hsA�hsA�jA�jA�l�A�jA�ffA�hsA�n�A�p�A�p�A�n�A�l�A�n�A�p�A�r�A�r�A�p�A�n�A�p�A�t�A�t�A�t�A�p�A�p�A�t�A�v�A�t�A�t�A�r�A�r�A�v�A�x�A�x�A�v�A�x�A�x�A�~�A̓A͉7A͋DA͍PA͑hA͑hA͛�A͡�Aͣ�Aͥ�AͮA͸RA;wA�ƨA�ȴA���A���A���A��
A��;A��mA��`A��`A��;A��;A��;A��HA��TA��HA��HA��;A��/A��/A��/A��#A���A���A���AͼjAͶFAͲ-AͲ-AͰ!AͰ!AͮAͬAͧ�Aͧ�Aͧ�Aͩ�Aͩ�A͝�A�v�A�S�A�7LA��A���A̛�A�p�A�O�A��A��A�|�A���A�VAɬA�ĜA��`A�\)A��#A�&�A�~�A�^5A��A���A� �A���A�ffA���A��A�1A��hA�{A��-A��PA�v�A�n�A�hsA�\)A�XA�XA�ZA�^5A�`BA�`BA�^5A�VA�Q�A�I�A�K�A�O�A�O�A�O�A�O�A�O�A�K�A�I�A�E�A�C�A�A�A�A�A�C�A�E�A�E�A�E�A�C�A�C�A�C�A�A�A�A�A�A�A�A�A�7LA�7LA�33A�33A�+A�33A��A�oA���A���A��A��A��HA��#A�ƨA���A���A���A���A��\A�ffA�^5A�Q�A��A�bA���A���A�A��A�1A��/A��mA��TA��mA��`A��mA��yA��A���A���A�"�A�33A�/A�`BA�`BA�l�A���A�l�A��FA���A���A��
A���A��HA��mA��A�  A�%A��A��RA���A���A���A��A�n�A�;dA�
=A��HA��A���A�C�A��A��^A�jA��A�I�A�G�A��A�XA�A�K�A�I�A���A���A�?}A���A�p�A�
=A���A���A�S�A���A���A���A���A�1'A�ZA�K�A��A��\A�I�A��;A�Q�A��A�A��9A���A��uA�z�A�ZA�&�A�A��A�|�A�S�A��A�ƨA��\A�+A���A�/A��A��A���A��A���A��RA�\)A�1'A��A��mA��wA�r�A�A�A��TA�  A��^A���A�-A�dZA���A�|�A�$�A���A�E�A��A��7A�
=A�ĜA�?}A�ffA�A���A��7A��A�\)A�(�A��/A��-A���A��+A�XA���A�=qA���A��A��A��mA���A�JA���A�l�A�{A��A� �A���A�bNA�-A��/A���A��A�\)A�=qA�bA��A���A���A�O�A��A~bA};dA|��A|��A|n�A|=qA|(�A|�A|1A{�A{�;A{��A{�wA{��A{|�A{"�Az�!Az9XAy�PAx�Aw�Aw+Av��Av��Av^5AvE�AvbAu�^AuXAt��As�7Ap��Ao��An�AmAm"�Al�yAl�RAlr�AlQ�Ak��Ak|�Aj�HAjVAi�Ai"�Ah�AhbNAgC�Af�AfM�Ae��Ad�AdbAc;dAb�\Ab�Aa�TAa��Aa�^Aa��Aa|�Aa/AaVA`��A`�uA`=qA` �A_�A_|�A^I�A]"�A\M�A[��A[t�A[/AZ��AZ��AZI�AY��AY�wAY�PAY\)AY/AX�AXv�AW��AWXAV�+AU�AU��AUO�AU�ATȴATjAT �AS��AS�#ASAS�AS��ASdZAR��AR5?AQ�AQO�AQ%AP��AP=qAP  AOAOp�AO;dAN�/AN�!AN�ANZAN9XAN$�ANbAM��AM�#AM�FAM��AM�AMt�AM`BAM?}AM�AL�yAL�9ALv�ALE�ALJAK��AK/AJ{AIG�AHQ�AH5?AH�AH{AG�TAGXAFȴAFQ�AE��AD��ADE�ACp�AB�AB�RAB�DABA�AB{AA��AA�;AA��AAO�AAVA@�A@�A@ĜA@�!A@��A@�\A@=qA?��A?�PA?|�A?x�A?|�A?t�A?dZA?C�A?33A?
=A>�RA>VA>�uA>��A>��A>�HA>��A?oA?�A?"�A?"�A?�A?A>��A>�A>�/A>ȴA>�RA>��A>n�A>I�A> �A=�;A=��A=hsA=C�A=/A=+A=&�A=;dA=;dA=+A=A<��A<�A<�A<��A<��A<��A<z�A<Q�A<bA;��A;�A;��A;A;��A;t�A;?}A:��A:�+A9�A7�mA7VA6ffA6n�A6~�A6��A6ȴA6�A6�/A7oA7�A7/A77LA7;dA7;dA7/A7
=A6��A6��A6�jA6�9A6r�A6bNA6^5A6ZA6E�A6(�A5�#A5�FA5�A5t�A5G�A4�/A4r�A4=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111114111111111111111111114411111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       A��TA��mA��yA��A��A��A��A��A��A��A��A��A���A��A��;A�ȴAͺ^A�~�A�VA��A��A��#A���A���A�ƨA���A̼jA̼jA���A���A��yA�JA� �A�/A�&�A�A�1'A�G�A�\)A�`BA�l�A�jA�jA�p�A�r�A�t�A�v�AͅA͝�A�A��;A��HA��/A;wAͬAͅA̅A��`A��^A��DA��HA��A�&�A�XA���A���A�dZA�~�A~�+A{��AxE�As7LAlffAhQ�Ac&�A`��A]|�AZE�AW��AT�`AS�PAP�AN�AM�AMC�AK��AHE�AE/ABVAA�A@�A?dZA>�jA?oA>�HA>JA=33A<�A<5?A;?}A7+A6��A7"�A6�+A5��A4~�A3�A3�;A3&�A2�!A2M�A/��A.��A-��A-�-A-�PA,��A+A*A(ĜA'l�A%�mA%33A%��A%"�A#�A#�A"��A"��A"ZA"�A!�^A!��A!XA!&�A z�A �9A �!A �A��AhsA;dAĜA��A��A �A�#A��A=qA�#A��At�AO�AoA��A��A��AM�A  AƨAt�A��AffAE�A$�AbA�mA�;AAXAG�A��A��A|�A?}AVA�AbNA  A�PA&�A�yA��A��A�AA�;A�^A��AhsAG�A&�A
=A�A�A~�AI�AbA�;A��A�hAO�A&�A�9A �A��A;dA��A�+A��A��An�A1A�-AC�A
�A
��A
9XA
  A	��A	?}A��A �A��AXA��A�A=qA�wA;dA�A��A�`A��A�DAn�AZA1'A�TAXA��A��A��A�PAp�AK�A+AVA �jA v�A Q�A �@��;@�t�@�dZ@�33@���@�\)@��@��@�A�@��
@�b@��m@��P@��@���@�?}@���@�Q�@��m@�C�@��H@�@���@���@�Q�@���@��@���@�h@���@�@��@�;d@��H@�~�@�V@�@�`B@���@�9@��@�@�;d@��y@���@��@��@�bN@�1'@��;@�w@�@�+@�O�@�V@�j@߶F@���@�{@ݺ^@ݡ�@�V@�9X@�K�@�$�@٩�@���@ו�@��@�^5@ա�@���@�j@Ӿw@�
=@���@��/@�z�@�9X@϶F@��H@���@�%@̴9@̋D@ˮ@��@�ȴ@ʧ�@�ff@���@ɩ�@ɑh@�/@�1'@ǥ�@�"�@���@�-@Ł@�X@��@ă@�(�@��m@å�@��@�-@��^@��@�z�@���@��w@�\)@��R@�^5@�=q@���@�O�@�/@���@�Z@��@��y@�v�@�V@��@�%@�bN@��@���@�t�@���@�M�@�$�@��^@�/@��u@���@�\)@�
=@���@�=q@�G�@�z�@�bN@� �@��;@�S�@��H@�@�p�@�&�@���@��@�j@�9X@� �@���@��\@�$�@��^@�7L@�%@��/@�Q�@�1'@��@��
@�33@��H@��R@�-@��#@�@��@�9X@�ƨ@�|�@�C�@���@�=q@�$�@��#@��@��@��`@��u@�j@�A�@��w@��@�K�@���@�~�@�$�@���@��@�&�@��@��@���@�r�@�(�@��
@�ƨ@���@��P@�l�@�"�@��R@���@�v�@�E�@�x�@�%@��D@�9X@�b@��m@��
@��@�t�@�+@��y@��+@�V@��@��-@�x�@��@�r�@�A�@�1@�ƨ@���@�K�@��y@�ȴ@��!@���@�J@��@���@�/@�%@��j@��D@�Q�@�|�@�;d@�o@��@��\@�M�@��@���@��-@���@�hs@�&�@��@�V@���@�Z@�1@���@��w@���@��@�\)@�;d@���@��@�ȴ@���@�5?@��#@�p�@���@�j@�9X@��;@��@���@�K�@���@�~�@��@��T@�@���@�p�@�?}@��/@��D@�1'@�1@��;@�ƨ@���@�S�@�
=@�ȴ@��\@�~�@�n�@�5?@���@��7@�7L@��@��@���@���@��@�j@� �@�;@|�@K�@+@~�y@~�+@}�-@|�/@|z�@|9X@{�m@{dZ@{@z��@z��@z�\@z-@yhs@xĜ@w��@w�@v�@v�R@v�+@vV@v@u��@u��@u`B@s�F@rn�@q��@q��@q�7@p�9@p�u@p�u@p�@p�@p�@pr�@pbN@pb@oK�@m�@l�j@l1@kƨ@k��@k��@kt�@k33@j�@j^5@j�@i�#@ix�@h��@g�@gK�@f�y@f��@fv�@fff@fff@fff@fV@f{@e�@e�@d�D@c�F@c�@cdZ@cS�@c"�@c@b�H@b��@b��@b�!@b~�@a��@a��@ax�@a7L@`�9@`bN@`  @_��@_��@_\)@_;d@_+@_�@^��@^�@^�R@^$�@]�-@]/@\��@\1@[�@[C�@["�@Z�H@Z��@Z^5@Yx�@Xr�@W�@W|�@Wl�@W+@W
=@V�@V��@Vv�@V5?@U�T@U�-@Up�@UV@T�@T�j@T�@Tj@T�@S�F@S"�@R��@R��@R��@R�\@R~�@Rn�@R=q@Q�#@Q��@P��@PA�@O�@OK�@N�@Nff@NE�@M@M�@L��@LZ@L�@K�
@K�@KC�@Ko@J�H@J��@J�!@J^5@I��@I��@I�7@Ix�@I7L@H��@H�u@G�;@F��@F�+@F5?@E�@E�h@D��@Dz�@DZ@C�
@C33@B�\@B~�@Bn�@B-@A��@A�7@A&�@@�9@@�@@  @?�P@>�@>ff@>@=@=�h@=�@=`B@<�/@<j@<�@;�m@;��@:�@:^5@:�@:J@9�@9��@8��@8b@7�;@7|�@6�y@6��@6v�@6ff@65?@6@5�T@5�T@5�T@5�T@5�T@5�-@5�@4�/@3�m@3�@3o@2�@2��@2~�@1��@1hs@1X@0�`@0bN@0A�@/�;@/�@.��@.5?@-��@-V@,�/@,�j@,z�@,j@,I�@,(�@+�m@+�
@+��@+��@+t�@+33@+33@+33@*��@*J@)��@)7L@(��@(1'@'�@'�;@'�P@';d@'
=@'
=@'
=@&�@&ȴ@&��@&5?@%�T@%@%�-@%�-@%�@%?}@%/@%V@$�@$�@#��@#dZ@#S�@#33@"��@"~�@"-@"�@!�^@!G�@ Ĝ@ bN@ 1'@�@�@l�@��@ȴ@�R@v�@5?@�T@��@�j@�D@I�@1@ƨ@o@��@��@^5@=q@�@�@�^@��@x�@hs@&�@�`@�9@�@Q�@�@
=@�y@�@ȴ@�R@ff@{@�T@��@�@`B@/@��@��@�D@z�@Z@I�@(�@�m@�
@��@t�@33@o@�@��@��@�!@��@�\@~�@M�@J@�#@��@��@X@%@��@�`@Ĝ@r�@1'@�w@��@\)@+@+@�@��@�y@�@ȴ@�R@��@��@��@�+@V@{@��@�h@�h@�@?}@�j@z�@j@j@Z@Z@9X@(�@(�@��@C�@o@
�@
�@
��@
�!@
~�@
M�@
-@
�@
J@
J@	�@	��@	x�@	&�@��@�9@�@�@r�@r�@r�@r�@bN@A�@b@�@|�@|�@|�A��HA��TA��`A��`A��yA��yA��mA��mA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��yA��A��A��HA���A���A���A�ȴA���A���A��
A�AͼjAͰ!Aͺ^AͬAͰ!A�A�AͲ-A;wA;wA͡�A�r�A�n�A�ffA�l�A�jA�l�A�n�A�ZA�\)A�K�A�E�A�G�A�?}A�"�A��A�$�A��A�A���A���A��A��A��A��A��A��A��mA��;A��#A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A�ƨA�ĜA�ĜA�ƨA�ƨA���A̾wA���A�A���A̾wA̺^A̼jA̾wA̾wA̼jA̺^A̼jA���A̾wA̼jA̼jA̼jA̾wA̾wA̼jA̼jA���A�A�A���A���A�ĜA���A���A���A���A��A��;A��HA��`A��yA���A���A�  A�  A�A�VA�oA��A��A��A��A�"�A�$�A�$�A�"�A�+A�1'A�/A�-A�1'A�33A�33A�1'A�/A�&�A��A�{A�JA�A���A���A���A�  A� �A�1'A�7LA�5?A�/A�+A�5?A�;dA�E�A�K�A�M�A�O�A�S�A�\)A�^5A�^5A�ZA�ZA�\)A�^5A�^5A�\)A�^5A�dZA�l�A�l�A�n�A�n�A�jA�jA�l�A�l�A�l�A�hsA�hsA�jA�jA�l�A�jA�ffA�hsA�n�A�p�A�p�A�n�A�l�A�n�A�p�A�r�A�r�A�p�A�n�A�p�A�t�A�t�A�t�A�p�A�p�A�t�A�v�A�t�A�t�A�r�A�r�A�v�A�x�A�x�A�v�A�x�A�x�A�~�A̓A͉7A͋DA͍PA͑hA͑hA͛�A͡�Aͣ�Aͥ�AͮA͸RA;wA�ƨA�ȴA���A���A���A��
A��;A��mA��`A��`A��;A��;A��;A��HA��TA��HA��HA��;A��/A��/A��/A��#A���A���A���AͼjAͶFAͲ-AͲ-AͰ!AͰ!AͮAͬAͧ�Aͧ�Aͧ�Aͩ�Aͩ�A͝�A�v�A�S�A�7LA��A���A̛�A�p�A�O�A��A��A�|�A���A�VAɬA�ĜA��`A�\)A��#A�&�A�~�A�^5A��A���A� �A���A�ffA���A��A�1A��hA�{A��-A��PA�v�A�n�A�hsA�\)A�XA�XA�ZA�^5A�`BA�`BA�^5A�VA�Q�A�I�A�K�A�O�A�O�A�O�A�O�A�O�A�K�A�I�A�E�A�C�A�A�A�A�A�C�A�E�A�E�A�E�A�C�A�C�A�C�A�A�A�A�A�A�A�A�A�7LA�7LA�33A�33A�+A�33A��A�oA���A���A��A��A��HA��#A�ƨA���A���A���A���A��\A�ffA�^5A�Q�A��A�bA���A���A�A��A�1A��/A��mA��TA��mA��`A��mA��yA��A���A���A�"�A�33A�/A�`BA�`BA�l�A���A�l�A��FA���A���A��
A���A��HA��mA��A�  A�%A��A��RA���A���A���A��A�n�A�;dA�
=A��HA��A���A�C�A��A��^A�jA��A�I�A�G�A��A�XA�A�K�A�I�A���A���A�?}A���A�p�A�
=A���A���A�S�A���A���A���A���A�1'A�ZA�K�A��A��\A�I�A��;A�Q�A��A�A��9A���A��uA�z�A�ZA�&�A�A��A�|�A�S�A��A�ƨA��\A�+A���A�/A��A��A���A��A���A��RA�\)A�1'A��A��mA��wA�r�A�A�A��TA�  A��^A���A�-A�dZA���A�|�A�$�A���A�E�A��A��7A�
=A�ĜA�?}A�ffA�A���A��7A��A�\)A�(�A��/A��-A���A��+A�XA���A�=qA���A��A��A��mA���A�JA���A�l�A�{A��A� �A���A�bNA�-A��/A���A��A�\)A�=qA�bA��A���A���A�O�A��A~bA};dA|��A|��A|n�A|=qA|(�A|�A|1A{�A{�;A{��A{�wA{��A{|�A{"�Az�!Az9XAy�PAx�Aw�Aw+Av��Av��Av^5AvE�AvbAu�^AuXAt��As�7Ap��Ao��An�AmAm"�Al�yAl�RAlr�AlQ�Ak��Ak|�Aj�HAjVAi�Ai"�Ah�AhbNAgC�Af�AfM�Ae��Ad�AdbAc;dAb�\Ab�Aa�TAa��Aa�^Aa��Aa|�Aa/AaVA`��A`�uA`=qA` �A_�A_|�A^I�A]"�A\M�A[��A[t�A[/AZ��AZ��AZI�AY��AY�wAY�PAY\)AY/AX�AXv�AW��AWXAV�+AU�AU��AUO�AU�ATȴATjAT �AS��AS�#ASAS�AS��ASdZAR��AR5?AQ�AQO�AQ%AP��AP=qAP  AOAOp�AO;dAN�/AN�!AN�ANZAN9XAN$�ANbAM��AM�#AM�FAM��AM�AMt�AM`BAM?}AM�AL�yAL�9ALv�ALE�ALJAK��AK/AJ{AIG�AHQ�AH5?AH�AH{AG�TAGXAFȴAFQ�AE��AD��ADE�ACp�AB�AB�RAB�DABA�AB{AA��AA�;AA��AAO�AAVA@�A@�A@ĜA@�!A@��A@�\A@=qA?��A?�PA?|�A?x�A?|�A?t�A?dZA?C�A?33A?
=A>�RA>VA>�uA>��A>��A>�HA>��A?oA?�A?"�A?"�A?�A?A>��A>�A>�/A>ȴA>�RA>��A>n�A>I�A> �A=�;A=��A=hsA=C�A=/A=+A=&�A=;dA=;dA=+A=A<��A<�A<�A<��A<��A<��A<z�A<Q�A<bA;��A;�A;��A;A;��A;t�A;?}A:��A:�+A9�A7�mA7VA6ffA6n�A6~�A6��A6ȴA6�A6�/A7oA7�A7/A77LA7;dA7;dA7/A7
=A6��A6��A6�jA6�9A6r�A6bNA6^5A6ZA6E�A6(�A5�#A5�FA5�A5t�A5G�A4�/A4r�A4=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111114111111111111111111114411111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BǮB�zBǮB�zBǮB�zB��BǮBǮB�EBǮB�zB�EBǮBȀB��BȀB�XBɆB��BɆB��B��BʌB�XB�XB��B��B�6B��B��B��B	.B	�B	"hB	%�B	>B	J�B	R�B	S�B	X�B	Z�B	[�B	]�B	_;B	`B	bB	gB	n/B	y�B	��B	��B	�fB	��B	��B	�SB	��B	��B	��B	�B	��B	�B	��B	�`B	�B	��B	��B	�jB	�6B	��B	�bB	��B	�B	w2B	ffB	U2B	K)B	6�B	0�B	#:B	�B	{B	B	�B	 �B	oB��B�B�B�)B�B��B	xB	B	�B	!�B	($B	.IB	33B	4�B	QB	j�B	�B	��B	�B	�zB	��B	˒B	�,B	��B	�2B	��B	�]B	��B	�B	�yB	�B	�B	��B	�B	�B	�MB	�DB
�B
"�B
-�B
6FB
:�B
B�B
D�B
IB
M�B
O�B
Z�B
b�B
aHB
d�B
iyB
g�B
gmB
h�B
g�B
h�B
`�B
\�B
\�B
_;B
b�B
c�B
e�B
iB
j�B
n�B
q�B
t�B
u%B
y	B
y�B
x�B
v�B
v`B
s�B
sMB
r�B
s�B
v�B
v�B
v�B
w�B
v+B
uZB
~�B
�iB
��B
��B
��B
�B
� B
|�B
|�B
}�B
~�B
~]B
~(B
~�B
{�B
{B
zxB
zDB
y�B
y>B
x�B
xB
v�B
v`B
t�B
s�B
sMB
r�B
q�B
r�B
p�B
o�B
o�B
k�B
jB
f�B
f�B
ffB
h�B
k�B
k�B
kQB
iyB
hsB
e�B
e`B
c�B
a�B
`�B
^�B
^5B
[�B
X�B
XB
U�B
S�B
RTB
Q�B
O�B
N<B
PB
PHB
P�B
PHB
O�B
OvB
N�B
N�B
OvB
N�B
I�B
HB
E�B
E9B
C�B
C-B
@�B
@�B
>wB
>B
;�B
:�B
8�B
7LB
6FB
6B
,B
+�B
+B
)*B
(XB
+6B
0�B
/�B
3�B
1�B
0UB
/�B
.}B
.�B
.}B
,qB
+�B
,B
)�B
&�B
)�B
%FB
$B
#�B
$�B
"�B
$@B
"4B
!-B
 \B
�B
 \B
�B
�B
OB
�B
�B
B
CB
B
�B
�B
�B
CB
�B
�B
B
B
�B
_B
�B
+B
�B
FB
@B
�B
�B
oB
�B
�B
�B
@B
:B
(B
�B
 B
.B
\B
VB
\B
�B
�B
�B
B
�B
�B
�B
B

�B
	�B
�B
xB
�B
~B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
FB
�B
�B
�B
�B
B
�B
�B
B
B
$B
�B
�B
$B
_B
YB
$B
_B
�B
�B
�B
�B
�B
1B
_B
�B
�B
�B
B
B
�B
7B
	B
kB
B
�B
7B
kB
B
1B
�B
�B
�B
_B
�B
�B
�B
YB
+B
+B
eB
eB
�B
CB
~B
B
B
�B
~B
�B
�B
B
OB
OB
�B
�B
�B
�B
�B
"hB
!�B
!�B
"�B
"hB
$tB
$�B
#�B
#:B
"�B
#B
$�B
$@B
$B
$@B
$B
$tB
$tB
%B
$�B
%B
&LB
&B
&�B
'�B
'�B
(�B
)*B
)�B
)�B
)�B
*0B
*�B
+6B
,=B
,�B
,�B
-B
,�B
-B
-wB
-CB
-B
,�B
,�B
.}B
.B
.}B
.}B
.IB
.}B
.B
.IB
.IB
.IB
.B
.IB
.B
.}B
.B
.B
.�B
.�B
.�B
/OB
/B
/OB
/�B
/�B
/�B
/�B
/�B
0�B
/�B
1'B
1'B
1'B
1�B
1�B
2aB
4B
3�B
4B
49B
4�B
5B
5�B
5tB
5�B
5tB
6B
6B
5�B
5�B
6B
6�B
7B
7B
7B
7�B
7�B
7�B
8B
8RB
8RB
7�B
8RB
8�B
8�B
9XB
:^B
:�B
:�B
;0B
:�B
:�B
;�B
;dB
<�B
=B
=<B
=B
=<B
=qB
=qB
>B
>wB
?HB
?HB
?HB
?}B
?}B
@OB
@�B
@�B
@�B
@�B
@�B
AUB
B'B
B[B
B�B
B�B
B�B
B�B
C-B
C-B
C�B
D3B
D�B
D�B
D�B
DgB
D�B
D�B
E�B
FtB
F?B
FtB
F�B
GzB
G�B
GzB
GzB
GEB
G�B
HKB
H�B
J#B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
K�B
OB
N�B
N�B
NpB
OB
OBB
N�B
OB
N�B
N�B
N�B
NpB
NpB
N<B
N�B
O�B
P}B
P�B
P}B
P}B
PHB
PB
PB
PB
PB
O�B
PB
PB
QNB
Q�B
R B
R�B
S&B
S&B
S�B
S�B
S[B
S�B
S�B
S[B
S�B
U�B
UgB
U�B
UgB
U�B
VB
VB
VB
VmB
VB
V9B
V�B
W�B
W�B
W�B
XB
XEB
X�B
YB
YB
YKB
Y�B
Y�B
YB
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[�B
\)B
[�B
[�B
[�B
[�B
[�B
\]B
]/B
^�B
^�B
^�B
^�B
_B
^�B
_B
_B
_B
_pB
_�B
_�B
`B
`B
`BB
`BB
`B
`BB
`�B
`�B
a|B
a|B
a�B
a�B
a|B
a|B
a|B
a�B
a�B
a|B
b�B
bB
b�B
b�B
d&B
c�B
c�B
d�B
d�B
e�B
e,B
e�B
e�B
e�B
f2B
f2B
ffB
f2B
f2B
f�B
f�B
f�B
f�B
gB
gB
g8B
gB
h
B
h�B
iDB
h�B
iyB
iyB
j�B
jB
jB
j�B
k�B
k�B
kQB
kQB
k�B
k�B
k�B
l"B
k�B
l"B
k�B
l�B
lWB
lWB
l"B
lWB
k�B
l"B
l"B
l�B
l�B
l�B
l�B
m)B
m�B
n/B
n/B
m�B
m�B
ncB
o B
oiB
o�B
o�B
p;B
p;B
p;B
p;B
poB
p�B
p�B
p�B
p�B
p�B
poB
p�B
poB
qB
q�B
rGB
r�B
sB
r�B
s�B
s�B
s�B
tTB
uZB
u�B
u�B
u�B
u�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
v+B
w2B
xB
xB
x8B
x8B
x8B
x8B
x8B
xB
x8B
x8B
xB
xlB
x�B
zB
zxB
zxB
zxB
z�B
z�B
z�B
z�B
z�B
{B
{B
|�B
}"B
|�B
|�B
|�B
}"B
}�B
~(B
~]B
~�B
~]B
~(B
�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
cB
.B
~�B
.B
.B
cB
cB
�B
�4B
�4B
�4B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�SB
�B
�SB
�%B
��B
��B
�1B
�1B
��B
��B
�B
��B
�fB
�1B
�1B
��B
��B
�_B
�+B
�_B
�_B
��B
��B
��B
�1B
�fB
��B
��B
�B
�B
�7B
�7B
�lB
��B
�=B
�=B
�rB
��B
��B
��B
�B
��B
�B
��B
�B
��B
��B
��B
�B
�"B
�VB
��B
��B
�VB
�VB
��B
��B
�\B
�\B
��B
��B
��B
��B
�.B
�bB
�bB
��B
�bB
�bB
�bB
��B
��B
� B
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
�B
��B
��B
��B
�B
�B
�FB
�FB
�FB
�{B
��B
��B
�B
�MB
�B
�B
�MB
�MB
��B
��B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�$B
��B
��B
��B
�YB�EBǮB�KB�B��B�?B�KB��B�B�mB�BȀB��B�zB�KB�EB��B�zBȴBǮB�tB�zBȴB��BƨB��BȀBȀB��B�B�EB�KBȀB�B�?B��BȀB�B��B�B�KB�B�B�BȀB�zB��B�EB�B�zB�B�zB��BǮBŢB�?B�B��B�B�zB��BǮBƨB�BȴB�zB�B��BɆB�B��BɺB�?BȴB�KB̘BǮB��B��B�BB�B�tB��BʌB�B�B�,B˒B��B�B��BʌB�zB�BɆB˒B��B�-B�zB�#B�jBɺB��B�^B�RB�#B�B�KB�KB�XB��BɆBȴBʌB��B˒B��B��B�#B��B�^BɺB�RBɺB��B�^B�)B��B��B��B˒B��BɺB�#B��B�^B��BɺB�B��B�)B�#B�RB�RB��B�^B��B��B�RB��B��B˒B�#B��B�^B�dB�dB��B��B��B͟B̘B��B�B�pB��BбB�}B��B�KB��B��B�/B��B�vB��B�B�5B�vB�B�>B��B	GB	_B	B	VB	VB	�B	4B	B	�B	_B	�B	�B	!B	�B	�B	 'B	!-B	"�B	$�B	!�B	#:B	!bB	$B	$@B	&LB	(�B	)*B	1�B	?}B	?�B	?}B	?�B	@�B	C�B	FtB	DgB	JXB	NpB	P}B	QB	P�B	Q�B	R�B	S�B	T,B	R�B	Q�B	R B	R�B	T�B	U2B	XEB	W�B	W?B	W�B	YB	Y�B	YB	XB	XEB	[WB	[#B	[�B	Z�B	ZB	Z�B	\�B	[�B	Z�B	[�B	\)B	]dB	^5B	^5B	]dB	]/B	^jB	_�B	`BB	_pB	^�B	^�B	_pB	`�B	`�B	_�B	_B	_�B	`vB	a|B	a|B	aHB	`�B	bB	c�B	c�B	e`B	e�B	f2B	f2B	hsB	h>B	kQB	m)B	o5B	n�B	oiB	o�B	s�B	x�B	xB	z�B	{B	|�B	�B	��B	�{B	�4B	��B	��B	��B	�rB	�B	�rB	�lB	�1B	�fB	�7B	��B	�lB	�7B	��B	��B	�B	��B	��B	�GB	�B	�B	�B	�;B	��B	��B	�;B	��B	�AB	�AB	��B	� B	��B	�fB	��B	�DB	��B	��B	��B	��B	�PB	�FB	��B	�qB	�_B	�~B	��B	�KB
oB	خB	�B	ܒB	�B	�XB	�
B	��B	��B	�?B	��B	��B	��B	ںB	�TB	ɺB	��B	�gB	��B	��B	�aB	�mB	ŢB	�3B	�3B	�-B	B	�'B	��B	��B	��B	ŢB	��B	��B	�-B	��B	��B	��B	�aB	�gB	�9B	��B	ŢB	�mB	ĜB	ÖB	��B	ÖB	�gB	��B	�3B	��B	��B	ÖB	��B	�9B	ÖB	�aB	�aB	ÖB	� B	ǮB	�B	ʌB	ȴB	ǮB	��B	ɆB	�KB	ɆB	ÖB	�0B	��B	�zB	�EB	�9B	��B	�-B	��B	�B	�,B	�B	��B	�EB	�[B	�<B	�-B	�B	�B	�tB	�?B	��B	�3B	B	�9B	�<B	��B	��B	��B	�aB	�HB	��B	̘B	��B	��B	��B	��B	��B	�gB	��B	��B	�$B	�zB	ŢB	�tB	͟B	�gB	��B	��B	�?B	�?B	��B	��B	��B	��B	�9B	��B	�?B	�B	�]B	��B	��B	�'B	��B	��B	�3B	ݘB	�5B	�XB	ӏB	��B	�vB	��B	�B	��B	�pB	��B	��B	�DB	��B
B	�B	�?B	�B	�ZB	�2B	�iB	��B	�B	��B	�B	�B	�B	��B	�>B	�B	�B	�>B	��B	�;B	�B	�B	��B	��B	�B	�B
%�B	��B
ZQB
f2B	��B	�B	��B	�|B	��B	��B	�|B	�B	��B	�B	�.B	�QB	خB	��B	�B	�B	��B	�B	��B	�BB	�NB	��B	�5B	�yB	�B	�`B	�	B	��B	خB	бB	�yB	�B	�KB	�NB	�BB	̘B	�B	�&B	ϫB
�B	�B	�`B	��B	�"B	�|B	��B	�B	�TB	��B	�[B	��B	��B	��B	ɺB	�<B	��B	�B	�FB	��B	��B	��B	�2B	� B	�tB	��B	�B	��B	��B	�eB	��B	��B	��B	��B	��B	�B	�:B	��B	�4B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�4B	��B	�\B	��B	�qB	�zB	�uB	��B	��B	�;B	~]B	~�B	|�B	zB	}�B	~(B	��B	}�B	��B	p�B	oiB	z�B	�B	uZB	m�B	t�B	ncB	n/B	qvB	gmB	`B	\�B	Y�B	X�B	WsB	YB	X�B	R�B	R�B	UgB	P}B	MjB	K�B	OB	m�B	H�B	RTB	?HB	9XB	:�B	7LB	9$B	8�B	6FB	3�B	2�B	1�B	-�B	2�B	0�B	-wB	/�B	4nB	)�B	#:B	$�B	$B	$B	#nB	�B	xB	eB	�B	�B	�B	7B	B	$@B	�B	FB	B	MB	bB	�B	�B	�B	xB	B		�B		�B	_B	�B	B	MB	�B	B	�B	GB	B��B	 �B��B	;B	B	B�]B��B�JB��B	�B	oB	%B	;B�%B�%B�|B�fB��B�DB��B��B��B�MB��B��B�)B�cB��B�yB�"B�B��B��B�WB�B�"B�B�B��B��B�2B�B��B��B�>B�JB�"B��B	�B	�B	JB	�B	
�B	 �B	�B	hB	�B	 B	B	SB	eB	�B	B	=B		B	xB	IB	�B	IB	 'B	�B	�B	!-B	$B	$�B	%FB	%FB	%zB	&�B	'RB	%FB	+�B	.IB	/�B	.�B	.�B	.IB	,qB	,qB	49B	3hB	5B	33B	1�B	2�B	0!B	/OB	1[B	0!B	5B	4�B	=�B	GzB	[�B	E�B	D�B	T�B	QB	[WB	UgB	f�B	e�B	r�B	s�B	x8B	{B	|�B	}VB	�iB	�B	��B	��B	�+B	�	B	��B	�hB	�nB	��B	�LB	��B	�IB	��B	�nB	�-B	�RB	��B	��B	�aG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111114111111111111111111114411111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       B�`B�,B�`B�,B�`B�,B��B�`B�`B��B�`B�,B��B�`B�2BB�2B�
B�8BáB�8B�sB�sB�>B�
B�
B�sB�yB��BΰB�uB��B		�B	^B	B	aB	7�B	D>B	L;B	M�B	R`B	T8B	U>B	WJB	X�B	Y�B	[�B	`�B	g�B	s�B	~�B	�RB	�B	}�B	{UB	B	��B	��B	ҔB	ƳB	�vB	�SB	�B	�B	�cB	֭B	�xB	�B	��B	�[B	�B	�EB	y~B	p�B	`B	N�B	D�B	0�B	*<B	�B	�B	-B	�B�hB��B�!B�_B��B�CB��B��B�qB	*B	�B	^B	�B	!�B	'�B	,�B	.TB	J�B	deB	{�B	��B	��B	�,B	��B	�DB	��B	�|B	��B	�~B	�B	�B	�7B	�+B	�_B	��B	�B	�MB	�B	��B	��B
�B
�B
'�B
/�B
4yB
<vB
>NB
B�B
G�B
I�B
TlB
\iB
Z�B
^uB
c+B
a�B
aB
bYB
a�B
b�B
Z\B
VDB
VxB
X�B
\iB
]�B
_�B
b�B
deB
h~B
k\B
n:B
n�B
r�B
sYB
rSB
p{B
pB
mhB
l�B
l�B
m�B
pFB
pFB
pFB
q�B
o�B
oB
xwB
zB
zOB
zOB
zOB
y~B
y�B
v�B
vkB
w�B
xwB
xB
w�B
xCB
u�B
t�B
t*B
s�B
sYB
r�B
rSB
q�B
p�B
pB
n:B
m�B
l�B
l�B
k�B
lbB
j�B
iPB
i�B
elB
c�B
`�B
`MB
`B
bYB
e�B
e�B
eB
c+B
b%B
_�B
_B
]oB
[cB
Z�B
XPB
W�B
UrB
R�B
Q�B
O�B
MuB
LB
KiB
I�B
G�B
I�B
I�B
J�B
I�B
I]B
I(B
H�B
H�B
I(B
HWB
ClB
A�B
?�B
>�B
=�B
<�B
:�B
:�B
8)B
7�B
5KB
4�B
2�B
0�B
/�B
/�B
%�B
%QB
$�B
"�B
"
B
$�B
*pB
)jB
-NB
+�B
*B
)�B
(/B
(�B
(/B
&#B
%�B
%�B
#EB
 gB
#EB
�B
�B
UB
[B
�B
�B
�B
�B
B
�B
B
�B
�B
B
<B
�B
�B
�B
�B
pB
6B
^B
�B
�B
RB
�B
�B
zB
B
�B
�B
3B
�B
�B
UB
aB
!B
[B
OB
qB
�B
�B
�B
	wB

�B
	�B
	B
B
	B
6B
6B
^B
�B
^B
^B
�B
�B
XB
�B
eB
*B
�B
0B
kB
qB
�B
<B
	�B
OB
�B
�B
�B
�B
aB
�B
aB
3B
aB
aB
�B
3B
gB
�B
�B
�B
nB
nB
�B
B
B
�B
B
tB
?B
�B
tB
�B
�B
B
tB
RB
�B
�B
�B
�B
�B
�B
B
�B
RB
�B
B
�B
�B
zB
�B
tB
B
?B
?B
?B
B
�B
�B
B
B
RB
�B
0B
�B
�B
�B
0B
XB
^B
�B
B
B
6B
jB
�B
pB
�B
B
�B
}B
�B
B
&B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
&B
�B
[B
�B
�B
�B
 3B
!mB
!mB
"sB
"�B
#EB
#�B
#EB
#�B
$KB
$�B
%�B
&�B
&WB
&�B
&WB
&�B
')B
&�B
&�B
&�B
&�B
(/B
'�B
(/B
(/B
'�B
(/B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(/B
'�B
'�B
(dB
(�B
(�B
)B
(�B
)B
)jB
)�B
)jB
)jB
)�B
*<B
)�B
*�B
*�B
*�B
+vB
+BB
,B
-�B
-�B
-�B
-�B
.TB
.�B
/ZB
/&B
/ZB
/&B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
12B
12B
1�B
1�B
2B
2B
1�B
2B
28B
2mB
3
B
4B
4EB
4EB
4�B
4�B
4yB
5KB
5B
6�B
6�B
6�B
6�B
6�B
7#B
7#B
7�B
8)B
8�B
8�B
8�B
9/B
9/B
:B
:5B
:jB
:jB
:�B
:jB
;B
;�B
<B
<�B
<vB
<vB
<vB
<�B
<�B
=�B
=�B
>NB
>NB
>NB
>B
>�B
>�B
?�B
@&B
?�B
@&B
@�B
A,B
A`B
A,B
A,B
@�B
A`B
A�B
BfB
C�B
D>B
DsB
DsB
D�B
D�B
EDB
EyB
EyB
E�B
H�B
HWB
HWB
H"B
H�B
H�B
H�B
H�B
H�B
HWB
HWB
H"B
H"B
G�B
H�B
I�B
J/B
JcB
J/B
J/B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
K B
K5B
K�B
LoB
L�B
L�B
MAB
MAB
MB
MAB
MuB
MB
M�B
O�B
OB
OMB
OB
OMB
O�B
O�B
O�B
PB
O�B
O�B
P�B
QZB
QZB
QZB
Q�B
Q�B
R`B
R�B
R�B
R�B
SfB
SfB
S1B
SfB
SfB
SfB
S�B
T8B
TlB
T�B
U>B
U�B
U�B
U�B
UrB
U>B
UrB
VB
V�B
XPB
X�B
X�B
XPB
X�B
X�B
X�B
X�B
X�B
Y"B
YVB
YVB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[.B
[.B
[cB
[cB
[.B
[.B
[.B
[cB
[�B
[.B
\4B
[�B
\iB
\�B
]�B
]�B
]�B
^uB
^uB
_GB
^�B
_GB
_{B
_{B
_�B
_�B
`B
_�B
_�B
`MB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
b�B
b�B
b�B
c+B
c+B
deB
c�B
c�B
deB
e7B
e7B
eB
eB
elB
e�B
e�B
e�B
elB
e�B
e�B
f�B
f	B
f	B
e�B
f	B
e�B
e�B
e�B
f=B
f�B
f�B
f�B
f�B
gxB
g�B
g�B
g�B
g�B
hB
h�B
iB
iPB
i�B
i�B
i�B
i�B
i�B
j!B
jVB
j�B
jVB
jVB
jVB
j!B
jVB
j!B
j�B
k�B
k�B
lbB
l�B
l�B
m4B
m�B
m�B
nB
oB
o�B
ouB
ouB
ouB
n�B
n�B
n�B
ouB
o@B
ouB
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r�B
s�B
t*B
t*B
t*B
t_B
t�B
t�B
t�B
t�B
t�B
u1B
vkB
v�B
vkB
v7B
v�B
v�B
wqB
w�B
xB
xCB
xB
w�B
yIB
x�B
x�B
x�B
x�B
xCB
xwB
xwB
yB
x�B
x�B
x�B
x�B
yB
yB
yIB
y�B
y�B
y�B
zOB
zOB
z�B
{UB
{UB
{UB
{UB
{�B
{�B
|�B
}�B
~3B
~�B
~�B
~�B
B
~�B
B
�B
��B
��B
��B
��B
��B
�zB
��B
�LB
�B
��B
��B
�zB
��B
�B
��B
�B
�B
�FB
�zB
�zB
��B
�B
�LB
�LB
��B
��B
��B
��B
�B
�RB
��B
��B
�$B
�XB
�XB
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
�B
�<B
�<B
�B
�B
�<B
��B
�B
�B
�CB
�wB
��B
��B
��B
�B
�B
�IB
�B
�B
�B
�IB
�}B
��B
�OB
�OB
�OB
��B
�UB
�UB
��B
��B
�UB
��B
��B
��B
��B
�[B
��B
��B
��B
��B
��B
�-B
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
�B
�9B
�nB
�nB
�nB
�nB
�nB
�nB
�9B
��B
��B
�tB
�tB
��B
�B��B�`B��B��B��B��B��B��B��B�B��B�2B��B�,B��B��B��B�,B�fB�`B�&B�,B�fBB�ZB��B�2B�2B��B��B��B��B�2B��B��B��B�2B��B��B��B��B��B��B��B�2B�,B��B��B��B�,B��B�,BáB�`B�TB��B��BB��B�,B��B�`B�ZB��B�fB�,B��BŭB�8B��B�sB�lB��B�fB��B�JB�`BʗB��B��B��B�&BB�>B��B��B��B�DB�sB��B�yB�>B�,B��B�8B�DBħB��B�,B��B�B�lB�|B�B�B��B��B��B��B�
B�sB�8B�fB�>B�yB�DB�sBáB��B�yB�B�lB�B�lB�sB�B��BáBáB�sB�DBħB�lB��B�sB�B�sB�lB��B�sB��B��B�B�B�sB�B�sBáB�B�sBŭB�DB��B�sB�B�B�BħBŭBǅB�QB�JBŭBƳB�"BɑB�cB�/B˞B��B�rBէB��B֭B�(B�{B�lB��B�(B�hB��B�=B��B	B	�B	B	B		wB	
�B	�B	3B	B	�B	6B	�B	�B	jB	�B	�B	�B	[B	}B	�B	B	�B	�B	�B	"sB	"�B	+vB	9/B	9�B	9/B	9cB	:5B	=|B	@&B	>B	D
B	H"B	J/B	J�B	JcB	KiB	L�B	MAB	M�B	L�B	KiB	K�B	L;B	N|B	N�B	Q�B	QZB	P�B	QZB	S1B	S�B	S1B	Q�B	Q�B	U	B	T�B	UrB	T8B	S�B	T�B	V�B	U�B	T8B	U>B	U�B	WB	W�B	W�B	WB	V�B	XB	Y�B	Y�B	Y"B	XPB	XPB	Y"B	Z\B	Z�B	Y�B	X�B	Y�B	Z(B	[.B	[.B	Z�B	Z�B	[�B	]:B	]oB	_B	_GB	_�B	_�B	b%B	a�B	eB	f�B	h�B	hJB	iB	iPB	mhB	rSB	q�B	t_B	u1B	v�B	yIB	}bB	}-B	y�B	}�B	�LB	�LB	�$B	��B	�$B	�B	��B	�B	��B	�RB	�B	��B	��B	��B	~�B	�tB	nB	|�B	}�B	|�B	{�B	z�B	zOB	zOB	z�B	{�B	{�B	{�B	zOB	y�B	|\B	�B	�^B	��B	��B	��B	�wB	�qB	�B	��B	�nB	�#B	�B	�0B	�B	��B	�!B	�`B	��B	�DB	��B	�
B	мB	áB	ԠB	��B	��B	ħB	�pB	�lB	�B	�lB	�B	�B	��B	��B	�B	�B	�TB	��B	��B	��B	�AB	��B	��B	�|B	��B	�TB	�|B	�|B	��B	��B	��B	�vB	�B	�B	��B	��B	�TB	�B	�NB	�HB	�|B	�HB	�B	�|B	��B	��B	�|B	�HB	��B	��B	�HB	�B	�B	�HB	��B	�`B	��B	�>B	�fB	�`B	��B	�8B	��B	�8B	�HB	��B	ħB	�,B	��B	��B	�vB	��B	ςB	��B	��B	��B	ħB	��B	�B	��B	��B	��B	��B	�&B	��B	��B	��B	�AB	��B	��B	�cB	��B	��B	�B	��B	��B	�JB	�8B	�yB	��B	�8B	�B	�B	�>B	��B	��B	�,B	�TB	�&B	�QB	�B	��B	��B	��B	��B	��B	��B	�QB	�gB	��B	�ZB	��B	��B	�B	�3B	��B	��B	�QB	�<B	��B	�JB	��B	�
B	�AB	�xB	�(B	ӚB	عB	էB	�"B	�uB	�uB	��B	ڑB	��B	�AB	��B	ٿB	�B	��B	�B	�nB	��B	�B	�7B	�eB	��B	�B	��B	�MB	�_B	��B	�{B	��B	�GB	�:B	؅B	ͪB	�iB	�:B
aB	�~B
TB
_�B	�B	��B	�B	�.B	֭B	��B	�.B	عB	�rB	�oB	��B	�B	�`B	�FB	�B	�oB	�~B	عB	߰B	��B	� B	��B	��B	�+B	�:B	�B	�B	юB	�`B	�cB	�+B	��B	��B	� B	��B	�JB	��B	��B	�]B
�B	�lB	�B	�B	��B	�.B	�yB	��B	�B	�B	�B	�B	��B	��B	�lB	��B	�>B	��B	��B	��B	��B	�NB	��B	��B	�&B	ڑB	��B	��B	�yB	�B	��B	�gB	�3B	�aB	�gB	��B	��B	��B	��B	��B	�sB	��B	�gB	�EB	��B	�vB	�pB	��B	�?B	�gB	�B	��B	�?B	�B	��B	�#B	�,B	�'B	��B	��B	z�B	xB	xCB	v7B	s�B	w�B	w�B	}�B	w�B	}�B	jVB	iB	t�B	y~B	oB	gCB	n�B	hB	g�B	k(B	aB	Y�B	VxB	SfB	R�B	Q%B	R�B	R�B	L;B	L�B	OB	J/B	GB	EDB	H�B	gCB	BfB	LB	8�B	3
B	4�B	0�B	2�B	2�B	/�B	-NB	,�B	+BB	'�B	,HB	*�B	')B	)5B	. B	#EB	�B	�B	�B	�B	 B	pB	*B	B	zB	?B	nB	�B	�B	�B	EB	�B	�B	�B	
B	�B	�B	kB	*B	�B	RB	�B	B�:B��B��B�hB��B�nB��B��B�~B�OB�~B��B��B��B�B��B��B�7B	�B�!B��B��B��B��B�.B�B�eB��B�{B�~B�~B��B�eB�B��B�B�B�+B��B��B�xB�B�	B�1B��B��B�=B�rB�B��B�B�B�YB��B��B��B��B��B�hB	�B		wB	XB�OB	kB	B	�B	
�B	�B	B	B	�B	�B	�B	�B	*B	�B	jB	�B	�B	�B	<B	�B	�B	[B	�B	�B	,B	 3B	!B	�B	%�B	'�B	)5B	(�B	(�B	'�B	&#B	&#B	-�B	-B	.�B	,�B	+BB	,HB	)�B	)B	+B	)�B	.�B	.TB	7WB	A,B	UrB	?�B	>�B	N|B	J�B	U	B	OB	`�B	_�B	l�B	m4B	q�B	t�B	v�B	wB	zB	|�B	��B	�aB	��B	��B	��B	�B	� B	��B	��B	�gB	��B	��B	� B	��B	�B	�]B	�yB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223257                            20230426223257AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622325720230426223257  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325720230426223257QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325720230426223257QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               