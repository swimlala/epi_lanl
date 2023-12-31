CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-02T17:01:02Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [p   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230502170102  20230502170102  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�%�Zt"�@�%�Zt"�11  @�%�����@�%�����@+�X��@+�X���c�ֶZ���c�ֶZ��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?��?��H@=p�@�G�@�G�@��R@�  @��RA  A   A+�A?\)A_\)A�  A�Q�A���A�Q�A�Q�A�Q�A߮A�Q�B Q�B(�B  B�
B   B((�B0  B7�
B?�
BH  BO�
BW�
B`  Bh(�Bp(�BxQ�B�  B�  B��B�  B�{B�{B�{B��B��B��B��B�  B�  B��B�  B�{B�{B�{B�  B�  B�  B��
B��B��B��B�  B�  B��B�  B��B�  B�  C   C
=C
=C  C
=C
  C  C
=C
=C  C  C  C
=C{C  C��C   C"  C$  C%��C(  C*  C,
=C.  C0  C2  C3��C5�C8  C:
=C<  C=��C@  CB  CD
=CF  CG�CI��CL  CN
=CP{CR
=CT  CV  CX  CZ  C\  C^
=C`  Ca��Cd  Cf  Ch
=Cj
=Cl  Cm��Co�Cr  Ct
=Cu��Cw��Cy��C|  C}��C�C���C�  C�  C���C�  C�C�C�C�  C���C���C���C���C�  C�  C�  C�C�  C���C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�  C���C���C�C�C�  C�C�
=C�
=C�C�C�C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C���C�  C�  C�  C�C�  C�C�  C�  C�  C�  C�C���C���C�  C���C�  C�  C���C�  C�C�  C�C�
=C�  C�  C�  C���C���C�  C�C�C�C�  C�  C���C���C���C���C���C�  C�  C��C���C�  C�  C��C�  C�
=C�
=C�C�C�  C���C���C���C�  C���C���C�  C���C���C�  C���C�  C�
=C�C���C�  C�C�  D   D ��D �qD}qD�D� D  D��D�D� DD�D  D��DD��D  D� D	�D	� D	�qD
� D
�qD}qD�qD� D�D� D�qD��D  Dz�D�qD� D�D� D�qD��D  D��D  D� D�D� D  D� D�D�D�D��D  D}qD�qD� D  D� D  D� D  D}qD  D� D�qD� D   D � D!�D!��D"  D"� D#  D#��D$�D$� D$�qD%� D%�qD&}qD'  D'��D(  D(}qD)  D)��D*�D*� D+�D+��D,�D,��D-  D-}qD.  D.� D/  D/��D0�D0� D1  D1� D2�D2� D2�qD3� D4  D4��D5  D5}qD6�D6� D6��D7� D8D8��D9�D9� D:  D:� D;  D;}qD<  D<��D=  D=� D>�D>��D>�qD?� D@  D@� DA  DA� DB  DB��DC�DC��DD�DD��DE�DE� DF  DF� DF�qDG� DH  DH� DI  DI}qDI�qDJ}qDK  DKz�DK��DL� DM  DM� DM�qDN� DO  DO}qDP  DP� DQ�DQ��DQ�qDR� DS  DS��DT�DT��DU  DU�DVDV��DW  DW}qDW�qDX� DY�DY��DZ  DZ� D[  D[}qD\  D\� D]�D]��D^  D^� D_�D_��D`�D`� Da�Da��Db  Dbz�Db�qDc� Dd�Dd��De  De� De�qDfz�Df�qDg}qDg��Dhz�Dh�qDi}qDj  Dj� Dj��Dkz�Dk�qDl� Dm  Dm� Dm�qDn��DoDo�Dp�Dp� Dq  Dq��Dr  Dr}qDr��Ds}qDt�Dt� Du  Du� Dv  Dv}qDv�qDw}qDw�qDx� Dy  Dy� Dz�Dz��D{�D{��D|�D|��D}D}� D}�qD~� D�D� D��D�>�D�� D��HD�  D�@ D�� D���D�HD�AHD��HD�D�  D�>�D��HD�� D�  D�@ D�� D�� D�HD�@ D�� D�� D���D�@ D�� D��qD���D�@ D��HD��HD�  D�@ D��HD�� D���D�=qD�� D��HD���D�=qD�~�D�� D���D�>�D�� D�D��D�AHD�� D���D�  D�B�D��HD��HD�  D�>�D��HD���D�  D�@ D���D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD���D�>�D�}qD���D�  D�>�D�~�D��qD�  D�AHD�~�D��qD���D�@ D��HD�� D��qD�=qD��HD�D�  D�=qD�~�D�� D���D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�D�HD�@ D���D��HD�  D�AHD���D���D�HD�@ D��HD�� D�  D�AHD�� D���D��)D�@ D��HD�� D���D�>�D��HD�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�~�D��qD�  D�@ D�� D���D��qD�AHD��HD��HD�HD�>�D�� D�� D���D�@ D��HD�� D���D�=qD�� D�D�HD�@ D�~�D�� D��D�@ D�� D�D�HD�AHD��HD�D�  D�=qD�� D�D��D�B�D�� D��qD���D�>�D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D�  D�>�D�}qD�� D�HD�@ D�~�D��HD�  D�=qD�}qD�� D���D�>�D�~�D���D���D�@ D�� D��HD��D�>�D�� D��HD�  D�=qD�~�D�D��D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�HD�@ D�� D�� D�  D�AHD��HD�� D���D�@ DHD�� D�  D�@ D�~�D�� D��D�B�DĀ D�� D�  D�>�D�~�D�� D���D�@ D�~�Dƾ�D�  D�@ Dǀ DǾ�D�  D�>�D�~�D�� D�  D�@ DɁHD��HD�  D�@ Dʀ Dʾ�D���D�>�Dˀ D�� D�  D�AHD̀ D̾�D�  D�@ D̀ D��HD�HD�>�D�~�D�� D�  D�AHDρHD��HD���D�>�DЁHD�� D�  D�AHDсHD�� D���D�@ D�~�D�� D�HD�AHDӁHD�� D�  D�@ D�~�DԾ�D�  D�AHDՁHD�� D�  D�AHDցHD��HD�  D�AHD�~�D׾�D�  D�@ D؁HD��HD���D�>�Dـ D�� D��D�AHDځHD��HD�HD�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D���D�@ DށHD�� D�  D�>�D߀ D�� D�  D�>�D�� D�D�  D�AHD�HD�� D�  D�AHD�HD��HD�  D�>�D� D�� D��D�@ D�~�D�� D�  D�AHD傏D��HD��D�@ D�~�D澸D���D�@ D� D�� D�HD�AHD� D��HD�  D�>�D� D龸D���D�@ D� D꾸D�  D�@ D�~�D뾸D���D�@ D�HD�� D�  D�@ D�~�D�� D�  D�>�D� D��HD�  D�>�D�HD��HD���D�>�D�}qD�D�  D�@ D� D�� D�  D�AHD� D�� D�HD�AHD� D�� D��D�AHD� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�@ D��HD��HD�  D�@ D�~�D�� D�  D�@ D���D�D�  D�>�D�� D��3D��R>�G�?��?W
=?��?�33?���?�@�@
=@(��@:�H@G�@Y��@p��@�  @��@�\)@���@��@���@�z�@��R@�=q@�33@�(�@�@��@�(�A�
A��A{Az�A�HA   A%A+�A1�A7
=A<(�AB�\AH��AN{AS33AX��A`  Ae�Aj=qAp��AvffA|(�A���A��A�ffA���A�(�A��RA���A��A�\)A��A��A�  A��HA��A�  A��HA�A���A��HA�A���A��
A�{A���A�(�A�
=A�G�A�(�A�
=Aљ�A�(�AָRAٙ�A�z�A�
=A�G�A�(�A�
=A��A�z�A�RA�G�A�(�A�
=A���A�(�A�ffB z�B�B33Bz�B��B�HB(�B	p�B
�RB  Bp�B�HB  BG�B�\B  B��B�HB(�Bp�B
=B��B�B\)B ��B"{B#�B$��B&�\B'�
B)�B*ffB,  B-p�B.�RB0  B1G�B2�RB4(�B5p�B6�RB8  B9p�B;
=B<Q�B=��B>�RB@(�BABB�HBD(�BEG�BF�\BH  BIp�BJ�\BK�BL��BNffBO�BP��BR{BS33BT��BU�BW\)BXz�BY��BZ�HB\(�B]p�B^�\B_�B`��Bb=qBc�Bd��BeBg
=Bhz�BiBk33BlQ�Bmp�Bn�RBp(�Bq��Br�RBt  BuG�Bv�\Bx  ByG�Bz�\B{�B|��B~ffB�
B��\B�33B�B�z�B��B��
B�z�B��B�B�Q�B�
=B�B�ffB�
=B��B�Q�B��HB���B�Q�B�
=B���B�(�B��HB���B�Q�B���B��B�(�B���B��B�=qB��RB�\)B�  B���B�\)B�  B���B�33B��
B�z�B�33B��
B�z�B�
=B���B�=qB���B���B�=qB���B�\)B�  B��RB�\)B��B��\B��B��B�Q�B���B��B�Q�B��HB�p�B�{B��RB�\)B�{B���B�33B��
B��\B�33B��B��\B��B�B�ffB��B��
B�z�B��B��B�Q�B���B�B�ffB���B���B�(�B���B�p�B�{B��RB�\)B��B�z�B��B��B�ffB�
=B��B�=qB���B�p�B�{Bģ�B�\)B��BƏ\B�
=BǙ�B�=qB��HBɅB�{Bʣ�B��BˮB�(�B̸RB�33BͮB�{B�ffBΣ�B��HB�
=B�G�B�p�Bϙ�Bϙ�Bϙ�Bϙ�BυB�p�BυBυBϙ�Bϙ�Bϙ�Bϙ�Bϙ�BϮB�B��B�  B�{B�{B�(�B�Q�B�ffBЏ\BиRB���B�
=B�33B�G�B�\)BхBѮB��
B�{B�=qB�z�Bң�B���B�
=B�33B�\)Bә�B��
B�{B�=qBԏ\B���B��B�G�BՅB�B��B�(�B�Q�B֣�B��HB�33BׅB�B�{B�ffBظRB�
=B�\)Bٙ�B�  B�Q�Bڣ�B�
=BۅB��B�=qBܸRB�33Bݙ�B�{Bޏ\B���B�\)B��
B�Q�B�RB��BᙚB�{B�z�B���B�\)B��
B�=qB�RB��B噚B�{B�\B�
=B�B��B�ffB��HB�\)B��
B�=qB�RB��B뙚B�  B�z�B���B�p�B��
B�Q�B���B�33B�B�(�B��B�
=B�p�B��B�ffB��HB�G�B��
B�=qB��RB�33B���B�(�B��\B�
=B��B�  B�ffB���B�\)B�B�=qB���B��B���B�  B�z�B���B�p�B��B�ffB��HB�G�B�C {C Q�C �C C ��C33Cp�C��C�HC{CG�Cz�C�RC�C(�C\)C�\C�RC��C(�C\)C�\C��C  C=qCp�C�C�HC�CQ�C�CC��C=qCp�C��C�HC{CQ�Cz�C�RC�C	(�C	\)C	��C	��C

=C
=qC
p�C
��C
�HC{CQ�C�C�RC��C(�C\)C�\C��C  C=qCz�C�RC  C33Cz�C�RC�C33CffC��C�HC�CQ�C��C�
C{CG�C�CC
=CG�C�\C�
C{C\)C��C�HC�C\)C��C�C(�CffC�C��C=qC�C��C{C\)C��C�C(�Cp�C�RC  CG�C�\C�HC(�Cp�CC  CG�C�\C�
C�CffC�RC  CQ�C��C�C=qC�C��C{C\)C�C��C G�C ��C �C!33C!�C!��C"�C"\)C"�C"��C#=qC#�C#��C$�C$p�C$�RC%
=C%Q�C%��C%�C&33C&z�C&C'
=C'\)C'��C'��C(G�C(��C(�C)=qC)�\C)�HC*(�C*z�C*C+{C+\)C+�C,  C,\)C,�C-  C-G�C-��C-�C.=qC.�C.�
C/(�C/z�C/��C0(�C0z�C0�
C1(�C1z�C1C2{C2ffC2�RC3
=C3ffC3C4{C4ffC4�C5  C5\)C5��C6  C6\)C6�C7
=C7\)C7�C7��C8Q�C8��C8��C9G�C9�C:  C:Q�C:��C:��C;G�C;��C;��C<Q�C<�C=  C=ffC=�RC>
=C>\)C>�C?  C?\)C?C@{C@p�C@CA
=CAffCA�RCB{CBp�CB��CC(�CCz�CCCD{CDp�CD��CE(�CE�CE�
CF(�CF�CF�
CG�CG�CG�
CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444414144441111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      ?��?��H@=p�@�G�@�G�@��R@�  @��RA  A   A+�A?\)A_\)A�  A�Q�A���A�Q�A�Q�A�Q�A߮A�Q�B Q�B(�B  B�
B   B((�B0  B7�
B?�
BH  BO�
BW�
B`  Bh(�Bp(�BxQ�B�  B�  B��B�  B�{B�{B�{B��B��B��B��B�  B�  B��B�  B�{B�{B�{B�  B�  B�  B��
B��B��B��B�  B�  B��B�  B��B�  B�  C   C
=C
=C  C
=C
  C  C
=C
=C  C  C  C
=C{C  C��C   C"  C$  C%��C(  C*  C,
=C.  C0  C2  C3��C5�C8  C:
=C<  C=��C@  CB  CD
=CF  CG�CI��CL  CN
=CP{CR
=CT  CV  CX  CZ  C\  C^
=C`  Ca��Cd  Cf  Ch
=Cj
=Cl  Cm��Co�Cr  Ct
=Cu��Cw��Cy��C|  C}��C�C���C�  C�  C���C�  C�C�C�C�  C���C���C���C���C�  C�  C�  C�C�  C���C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�  C���C���C�C�C�  C�C�
=C�
=C�C�C�C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C���C�  C�  C�  C�C�  C�C�  C�  C�  C�  C�C���C���C�  C���C�  C�  C���C�  C�C�  C�C�
=C�  C�  C�  C���C���C�  C�C�C�C�  C�  C���C���C���C���C���C�  C�  C��C���C�  C�  C��C�  C�
=C�
=C�C�C�  C���C���C���C�  C���C���C�  C���C���C�  C���C�  C�
=C�C���C�  C�C�  D   D ��D �qD}qD�D� D  D��D�D� DD�D  D��DD��D  D� D	�D	� D	�qD
� D
�qD}qD�qD� D�D� D�qD��D  Dz�D�qD� D�D� D�qD��D  D��D  D� D�D� D  D� D�D�D�D��D  D}qD�qD� D  D� D  D� D  D}qD  D� D�qD� D   D � D!�D!��D"  D"� D#  D#��D$�D$� D$�qD%� D%�qD&}qD'  D'��D(  D(}qD)  D)��D*�D*� D+�D+��D,�D,��D-  D-}qD.  D.� D/  D/��D0�D0� D1  D1� D2�D2� D2�qD3� D4  D4��D5  D5}qD6�D6� D6��D7� D8D8��D9�D9� D:  D:� D;  D;}qD<  D<��D=  D=� D>�D>��D>�qD?� D@  D@� DA  DA� DB  DB��DC�DC��DD�DD��DE�DE� DF  DF� DF�qDG� DH  DH� DI  DI}qDI�qDJ}qDK  DKz�DK��DL� DM  DM� DM�qDN� DO  DO}qDP  DP� DQ�DQ��DQ�qDR� DS  DS��DT�DT��DU  DU�DVDV��DW  DW}qDW�qDX� DY�DY��DZ  DZ� D[  D[}qD\  D\� D]�D]��D^  D^� D_�D_��D`�D`� Da�Da��Db  Dbz�Db�qDc� Dd�Dd��De  De� De�qDfz�Df�qDg}qDg��Dhz�Dh�qDi}qDj  Dj� Dj��Dkz�Dk�qDl� Dm  Dm� Dm�qDn��DoDo�Dp�Dp� Dq  Dq��Dr  Dr}qDr��Ds}qDt�Dt� Du  Du� Dv  Dv}qDv�qDw}qDw�qDx� Dy  Dy� Dz�Dz��D{�D{��D|�D|��D}D}� D}�qD~� D�D� D��D�>�D�� D��HD�  D�@ D�� D���D�HD�AHD��HD�D�  D�>�D��HD�� D�  D�@ D�� D�� D�HD�@ D�� D�� D���D�@ D�� D��qD���D�@ D��HD��HD�  D�@ D��HD�� D���D�=qD�� D��HD���D�=qD�~�D�� D���D�>�D�� D�D��D�AHD�� D���D�  D�B�D��HD��HD�  D�>�D��HD���D�  D�@ D���D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD���D�>�D�}qD���D�  D�>�D�~�D��qD�  D�AHD�~�D��qD���D�@ D��HD�� D��qD�=qD��HD�D�  D�=qD�~�D�� D���D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�D�HD�@ D���D��HD�  D�AHD���D���D�HD�@ D��HD�� D�  D�AHD�� D���D��)D�@ D��HD�� D���D�>�D��HD�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�~�D��qD�  D�@ D�� D���D��qD�AHD��HD��HD�HD�>�D�� D�� D���D�@ D��HD�� D���D�=qD�� D�D�HD�@ D�~�D�� D��D�@ D�� D�D�HD�AHD��HD�D�  D�=qD�� D�D��D�B�D�� D��qD���D�>�D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D�  D�>�D�}qD�� D�HD�@ D�~�D��HD�  D�=qD�}qD�� D���D�>�D�~�D���D���D�@ D�� D��HD��D�>�D�� D��HD�  D�=qD�~�D�D��D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�HD�@ D�� D�� D�  D�AHD��HD�� D���D�@ DHD�� D�  D�@ D�~�D�� D��D�B�DĀ D�� D�  D�>�D�~�D�� D���D�@ D�~�Dƾ�D�  D�@ Dǀ DǾ�D�  D�>�D�~�D�� D�  D�@ DɁHD��HD�  D�@ Dʀ Dʾ�D���D�>�Dˀ D�� D�  D�AHD̀ D̾�D�  D�@ D̀ D��HD�HD�>�D�~�D�� D�  D�AHDρHD��HD���D�>�DЁHD�� D�  D�AHDсHD�� D���D�@ D�~�D�� D�HD�AHDӁHD�� D�  D�@ D�~�DԾ�D�  D�AHDՁHD�� D�  D�AHDցHD��HD�  D�AHD�~�D׾�D�  D�@ D؁HD��HD���D�>�Dـ D�� D��D�AHDځHD��HD�HD�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D���D�@ DށHD�� D�  D�>�D߀ D�� D�  D�>�D�� D�D�  D�AHD�HD�� D�  D�AHD�HD��HD�  D�>�D� D�� D��D�@ D�~�D�� D�  D�AHD傏D��HD��D�@ D�~�D澸D���D�@ D� D�� D�HD�AHD� D��HD�  D�>�D� D龸D���D�@ D� D꾸D�  D�@ D�~�D뾸D���D�@ D�HD�� D�  D�@ D�~�D�� D�  D�>�D� D��HD�  D�>�D�HD��HD���D�>�D�}qD�D�  D�@ D� D�� D�  D�AHD� D�� D�HD�AHD� D�� D��D�AHD� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�@ D��HD��HD�  D�@ D�~�D�� D�  D�@ D���D�D�  D�>�D�� D��3D��R>�G�?��?W
=?��?�33?���?�@�@
=@(��@:�H@G�@Y��@p��@�  @��@�\)@���@��@���@�z�@��R@�=q@�33@�(�@�@��@�(�A�
A��A{Az�A�HA   A%A+�A1�A7
=A<(�AB�\AH��AN{AS33AX��A`  Ae�Aj=qAp��AvffA|(�A���A��A�ffA���A�(�A��RA���A��A�\)A��A��A�  A��HA��A�  A��HA�A���A��HA�A���A��
A�{A���A�(�A�
=A�G�A�(�A�
=Aљ�A�(�AָRAٙ�A�z�A�
=A�G�A�(�A�
=A��A�z�A�RA�G�A�(�A�
=A���A�(�A�ffB z�B�B33Bz�B��B�HB(�B	p�B
�RB  Bp�B�HB  BG�B�\B  B��B�HB(�Bp�B
=B��B�B\)B ��B"{B#�B$��B&�\B'�
B)�B*ffB,  B-p�B.�RB0  B1G�B2�RB4(�B5p�B6�RB8  B9p�B;
=B<Q�B=��B>�RB@(�BABB�HBD(�BEG�BF�\BH  BIp�BJ�\BK�BL��BNffBO�BP��BR{BS33BT��BU�BW\)BXz�BY��BZ�HB\(�B]p�B^�\B_�B`��Bb=qBc�Bd��BeBg
=Bhz�BiBk33BlQ�Bmp�Bn�RBp(�Bq��Br�RBt  BuG�Bv�\Bx  ByG�Bz�\B{�B|��B~ffB�
B��\B�33B�B�z�B��B��
B�z�B��B�B�Q�B�
=B�B�ffB�
=B��B�Q�B��HB���B�Q�B�
=B���B�(�B��HB���B�Q�B���B��B�(�B���B��B�=qB��RB�\)B�  B���B�\)B�  B���B�33B��
B�z�B�33B��
B�z�B�
=B���B�=qB���B���B�=qB���B�\)B�  B��RB�\)B��B��\B��B��B�Q�B���B��B�Q�B��HB�p�B�{B��RB�\)B�{B���B�33B��
B��\B�33B��B��\B��B�B�ffB��B��
B�z�B��B��B�Q�B���B�B�ffB���B���B�(�B���B�p�B�{B��RB�\)B��B�z�B��B��B�ffB�
=B��B�=qB���B�p�B�{Bģ�B�\)B��BƏ\B�
=BǙ�B�=qB��HBɅB�{Bʣ�B��BˮB�(�B̸RB�33BͮB�{B�ffBΣ�B��HB�
=B�G�B�p�Bϙ�Bϙ�Bϙ�Bϙ�BυB�p�BυBυBϙ�Bϙ�Bϙ�Bϙ�Bϙ�BϮB�B��B�  B�{B�{B�(�B�Q�B�ffBЏ\BиRB���B�
=B�33B�G�B�\)BхBѮB��
B�{B�=qB�z�Bң�B���B�
=B�33B�\)Bә�B��
B�{B�=qBԏ\B���B��B�G�BՅB�B��B�(�B�Q�B֣�B��HB�33BׅB�B�{B�ffBظRB�
=B�\)Bٙ�B�  B�Q�Bڣ�B�
=BۅB��B�=qBܸRB�33Bݙ�B�{Bޏ\B���B�\)B��
B�Q�B�RB��BᙚB�{B�z�B���B�\)B��
B�=qB�RB��B噚B�{B�\B�
=B�B��B�ffB��HB�\)B��
B�=qB�RB��B뙚B�  B�z�B���B�p�B��
B�Q�B���B�33B�B�(�B��B�
=B�p�B��B�ffB��HB�G�B��
B�=qB��RB�33B���B�(�B��\B�
=B��B�  B�ffB���B�\)B�B�=qB���B��B���B�  B�z�B���B�p�B��B�ffB��HB�G�B�C {C Q�C �C C ��C33Cp�C��C�HC{CG�Cz�C�RC�C(�C\)C�\C�RC��C(�C\)C�\C��C  C=qCp�C�C�HC�CQ�C�CC��C=qCp�C��C�HC{CQ�Cz�C�RC�C	(�C	\)C	��C	��C

=C
=qC
p�C
��C
�HC{CQ�C�C�RC��C(�C\)C�\C��C  C=qCz�C�RC  C33Cz�C�RC�C33CffC��C�HC�CQ�C��C�
C{CG�C�CC
=CG�C�\C�
C{C\)C��C�HC�C\)C��C�C(�CffC�C��C=qC�C��C{C\)C��C�C(�Cp�C�RC  CG�C�\C�HC(�Cp�CC  CG�C�\C�
C�CffC�RC  CQ�C��C�C=qC�C��C{C\)C�C��C G�C ��C �C!33C!�C!��C"�C"\)C"�C"��C#=qC#�C#��C$�C$p�C$�RC%
=C%Q�C%��C%�C&33C&z�C&C'
=C'\)C'��C'��C(G�C(��C(�C)=qC)�\C)�HC*(�C*z�C*C+{C+\)C+�C,  C,\)C,�C-  C-G�C-��C-�C.=qC.�C.�
C/(�C/z�C/��C0(�C0z�C0�
C1(�C1z�C1C2{C2ffC2�RC3
=C3ffC3C4{C4ffC4�C5  C5\)C5��C6  C6\)C6�C7
=C7\)C7�C7��C8Q�C8��C8��C9G�C9�C:  C:Q�C:��C:��C;G�C;��C;��C<Q�C<�C=  C=ffC=�RC>
=C>\)C>�C?  C?\)C?C@{C@p�C@CA
=CAffCA�RCB{CBp�CB��CC(�CCz�CCCD{CDp�CD��CE(�CE�CE�
CF(�CF�CF�
CG�CG�CG�
CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444414144441111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��A��#A��A��A��/A��#A��#A��/A��TA��HA��HA��TA��TA��`A��`A��TA��mA��TA��`A��yA��`A��TA��TA��A��yA��yA��mA��yA��yAҬA�ffA�9XA��A���A�\)A�O�A�33A���AС�A�v�A�dZA�K�A�1'A�$�A�$�A��;A�S�A�Q�AͶFA�9XA̕�A��A˛�A�=qA�
=A�"�A���A���A��mA��hA���A�p�A��`A��A��wA�=qA��A�bNA���A��A�5?A���A�r�A���A�(�A��Az��Ax�DAs�Ap�An�Ai33Af�AbjA^�RA]�wA[hsAZJAYAW�^AV�yAV1AR��ANȴAL�\AK��AJ��AH$�AE��ADȴACdZAB�AA��AAG�A?��A>jA<��A;S�A:�HA9�mA9%A8E�A7�A6��A6 �A5\)A4z�A4A�A3�TA3;dA1�A0r�A/�-A/&�A/&�A.�yA/dZA/�#A/�-A.v�A-?}A-"�A,�A,��A,Q�A+��A+�A*ffA(�A( �A(JA(JA&�HA%�PA$jA#p�A"��A"A!O�A ��A v�A�Ax�A&�A �A��A/A�9A��A�AK�AVA�A�FA�A��Ap�AG�A�AXA��AĜAA�A��A�HA��AAdZA?}A�A�yA��A �A��AXA�A�!AjAQ�A5?A�A{A{AA��A�;A�;A�;A��A�FA�hAG�A%AVA�A�PA7LAĜA�DAVA��Ap�AoA
�!A
�A
v�A
M�A
�A	�A	A	S�A�HA��A~�A �A�mA��AdZA�A�A��A��Av�AQ�A��Al�AĜAv�A-A�
A��AXA;dA�A~�AE�AbA��A33A ��A ��A A�A @��@�K�@�+@�o@���@��7@���@��@��@���@�~�@��@�?}@��@��/@�z�@��@���@��y@�~�@�-@���@�&�@�Z@��;@�C�@��H@�\@�-@���@��`@�j@��m@�
=@�n�@�J@�O�@�9@��;@�R@��@�p�@�Ĝ@畁@�"�@�+@�p�@�j@���@�dZ@�@⟾@��@�7@�7L@�9@ߝ�@���@ޟ�@��@ݑh@�x�@�?}@��@�bN@�9X@۝�@ۅ@�@��@�G�@��/@�Ĝ@�9X@�dZ@ָR@�ff@�=q@�-@���@�@Ցh@���@ԃ@ӝ�@�K�@��y@ҟ�@�^5@љ�@���@У�@�(�@϶F@�|�@�"�@�v�@���@́@�hs@�X@�7L@�%@��`@�z�@˝�@�C�@��H@ʸR@ʇ+@ɑh@�Ĝ@�Z@�  @�
=@��@Ɨ�@�$�@�hs@��@�z�@ēu@�1@Å@��@�o@��y@¸R@\@�@�O�@��9@��D@�A�@�ƨ@��@��R@�M�@���@�p�@�/@��j@�j@�b@���@�J@���@��@���@�9X@���@��m@��
@�ƨ@�|�@�K�@�+@��!@�E�@�5?@��@�X@���@��9@��
@�
=@���@�v�@�E�@��#@��h@�G�@�%@���@�A�@���@�@���@�^5@�@��-@�p�@�/@��j@�Q�@�A�@��@���@�M�@�-@�J@��^@�&�@���@��D@�r�@�(�@���@�C�@�@���@�M�@�{@��@��@���@�I�@���@�C�@�@��H@��R@��\@�ff@�-@��@�@��@�p�@�7L@��@��9@�j@�Q�@�9X@�(�@�1@��
@���@�t�@�C�@�@�ȴ@�V@�-@��@���@��h@�`B@���@�j@��
@�S�@�@���@���@�n�@�=q@��@���@�X@���@��D@�bN@�1'@�ƨ@�K�@�o@���@�V@��@���@���@��-@�p�@�O�@�G�@��@���@���@��u@�I�@��@���@�\)@���@�~�@�^5@�E�@��@��-@���@�hs@��`@��u@�9X@�  @��@�o@�~�@��@��@���@��-@�p�@�?}@���@���@��j@�bN@��@��@��P@�|�@�l�@�S�@��!@�V@�{@��#@��^@��h@�`B@��`@��9@��D@�z�@�bN@�9X@��;@���@�l�@�\)@�C�@�
=@���@�ff@�$�@�J@���@��h@�hs@�V@���@��u@�bN@�9X@��;@��P@�33@�+@���@���@�n�@�E�@��@���@��@��@��@���@��D@�bN@�A�@�(�@�b@�  @�;@��@
=@~{@}�h@}`B@|��@|��@|9X@{��@{��@{�@{o@zn�@y��@y��@y��@y�@y��@yhs@y�@x��@xr�@x  @w��@w�@w��@w\)@v�@vE�@u�-@uV@t�@t��@t�D@tj@t(�@s�
@s�F@s��@st�@r��@r~�@r�@q��@pĜ@o�;@o
=@nv�@n@m�h@m�@l��@l�D@l�@k�m@k��@kS�@j��@i�@i7L@h�9@hQ�@g��@gK�@g+@f�@f�R@f$�@e�h@d�/@d�@dz�@dZ@d1@c�@b��@a��@a�@`bN@`1'@`  @_�w@_��@_�P@_;d@_
=@^�R@^5?@]��@\z�@\(�@[��@[�F@Zn�@Z�@Z�@ZJ@Y��@Y��@Y�@Y��@Y7L@XA�@W�@Wl�@Vȴ@VV@V@U��@T��@T9X@S�@SS�@S"�@R�\@Q�7@Q&�@P�`@P�@P  @O�@OK�@O�@N��@N@M�-@M�h@MO�@L�@LZ@K�m@K��@KS�@KC�@K33@J�@J��@Jn�@JM�@I�#@I�7@I�7@IX@H��@H�u@HbN@HA�@G��@Gl�@G+@F�@F$�@E��@Ep�@D��@Dj@DI�@DI�@D9X@C�m@C�@CS�@Co@B��@B��@B=q@A�^@Ax�@A7L@@�`@@��@@�u@@�u@@r�@@A�@@  @?\)@?�@>��@>�@>�y@>��@>V@>V@>$�@=@<�/@<Z@<9X@;��@;33@;@:n�@:-@:J@9�@9��@9x�@97L@9&�@8�9@8�u@8bN@8 �@7�@7�P@7+@7�@7
=@6�R@6v�@6{@5�@5�T@5��@5�-@5p�@4��@4(�@3�F@3dZ@3C�@333@3@2�!@2=q@1��@1�7@1%@0�9@0bN@01'@0 �@0  @/�;@/|�@/K�@/+@.��@.�R@.ff@.E�@.E�@-�@-�@,��@,�D@+��@+S�@+o@*�H@*n�@)��@(��@(��@(bN@(1'@'�w@';d@&��@&V@&{@%@%�h@%`B@%�@$�@$9X@$1@$1@#��@#�
@#ƨ@#ƨ@#ƨ@#��@#�@#33@#o@"��@"�!@"�!@"n�@"J@!G�@ ��@ ��@ ��@ r�@  �@�@�P@l�@;d@
=@�@��@E�@{@��@@p�@?}@V@�D@I�@��@��@S�@�!@�\@M�@�@�7@�@�`@�9@Q�@ �@�;@��@�P@�P@K�@+@
=@ȴ@v�@V@{@��@`B@?}@?}@�@�j@�D@9X@�
@��@S�@�@��@��@�\@=q@��@��@x�@�@��@Ĝ@�@�;@|�@l�@K�@�@��@��@�+@ff@5?@$�@{@@�h@p�@O�@/@�@V@�/@�D@Z@(�@�@�@�m@�F@��@S�@"�@
�H@
�\@
n�@
M�@
J@	�#@	��@	��@	��@	�7@	7L@Ĝ@��@��@��A��A��A��A��/A��;A��A��
A��#A��/A��/A��#A��A��#A��#A��A���A��A��/A��#A��A��A��/A��/A��#A��A��A��;A��/A��A��
A��/A��`A��TA��HA��HA��TA��HA��;A��;A��`A��`A��HA��HA��`A��mA��TA��HA��HA��mA��TA��HA��TA��mA��mA��`A��`A��mA��mA��TA��`A��yA��mA��`A��HA��`A��mA��TA��HA��HA��`A��mA��TA��`A��mA��yA��mA��HA��TA��mA��`A��;A��TA��`A��`A��`A��`A��mA��yA��yA��mA��yA��A��A��A��mA��TA��`A��yA��yA��TA��;A��;A��`A��`A��`A��HA��`A��mA��mA��;A��HA��TA��A��A��A��A��A��yA��yA��`A��mA��A��A��A��A��yA��`A��mA��yA��yA��HA��TA��mA��A��yA��mA��HA��mA��A��A��A��A��A��A��A��yA��A���A���A���Aҥ�AҍPA҇+A҉7A�l�A�bNA�p�A�hsA�p�A�?}A�=qA�=qA�5?A�33A�7LA�9XA�33A��A� �A�VA�oA��A��A�bA�A�
=A���Aѧ�AхA�jA�\)A�ZA�ZA�XA�XA�ZA�ZA�XA�O�A�K�A�C�A�?}A�=qA�=qA�7LA�-A�(�A�$�A� �A�{A���A��HA���A�ƨA���AЩ�AУ�AЛ�AЏ\AЃA�|�A�z�A�x�A�v�A�r�A�n�A�n�A�jA�hsA�bNA�^5A�ZA�XA�VA�Q�A�K�A�E�A�C�A�A�A�A�A�=qA�1'A�&�A�"�A�$�A�&�A�&�A�$�A�"�A�"�A�$�A�$�A�+A�&�A�$�A� �A�"�A�&�A�  A��`A��
A���AϼjAϲ-Aϕ�AύPA�n�A�S�A� �A���A��A�Aβ-A΃A��AͰ!A͟�Aͩ�A���A���A���Aͣ�A͍PA�z�A�^5A�33A�$�A�bA��yA�ȴA̲-A̛�ÁA�r�A�l�A�S�A�A�A�(�A�&�A��A���A��
A˺^AˮA˩�A˛�AˋDA�r�A�?}A���Aʰ!A�bNA��mAɓuA��A�A�A�x�A��A��A�XA�VA��A���AŸRA�VA��AļjAēuA�t�A�VA�E�A�?}A�+A�
=A���A��`Aú^AÏ\A�`BA�M�A�A�A�;dA�1'A�$�A� �A�oA�1A���A��mA�`BA���A�n�A�7LA�G�A���A��`A�hsA��A��PA�p�A���A�?}A��`A��A�^5A�-A��TA���A�^5A�ZA��A��A��PA�`BA��A�+A�%A��A���A�;dA�M�A�ȴA�;dA���A�  A���A���A�I�A�oA��A���A��A��PA�5?A��FA�ZA�`BA��A�%A�7LA���A��^A�A�7LA�"�A�A�A�S�A��A��A��HA�jA�oA��yA��#A��jA�bNA�=qA�/A�"�A�bA���A��#A��A�Q�A�-A�{A�%A�1A�A��A��mA��A��yA��HA��/A��/A��;A��TA��mA��A��A���A��TA���A�A�"�A�1'A�+A�&�A�JA��TA��+A�ZA�oA�bA���A��mA���A�G�A�JA�ĜA��!A���A��hA�p�A�K�A�$�A��A��A��mA��TA��mA���A��wA���A�hsA�ZA�G�A��A�VA�  A��A���A�A��FA���A��A�hsA�S�A�5?A��A�1A���A���A�~�A�dZA�ffA�l�A�n�A�l�A�ffA�bNA�hsA�^5A�O�A�1'A�A���A�O�A���A�(�A��A���A�XA�=qA�A�ĜA���A�{A�S�A�ȴA�A���A�n�A��A���A�v�A�1'A��/A��DA�%A��^A�t�A�{A�ĜA��+A�A�A�ƨA�&�A��DA�K�A��RA�G�A��A��\A�x�A�ffA���A�JA~�RA~jA~E�A~A}��A|�yA{XAz��Az�\Az�+AzjAzZAzE�AyƨAyl�Ay?}AyC�Ay&�Ax��Awp�Av  Au�At�/At��AtjAs�#As�hAr��ArAql�Ap�HAp~�Apz�ApQ�Ao�mAo�#Ao�#Ao��Ao��Aol�AoC�Ao�An{Ak�AkAjI�Aix�AiVAh=qAg��Ag�7Ag"�Af�jAf  Ae��Ae�Ael�Ae+Ad�jAd1'AbĜAaVA`��A_�#A_l�A^��A^�A^�A^�A^^5A^I�A^A�A^5?A^�A]�A]7LA]�A\��A\�+A[��A[�A[oAZ�AZ�AZ=qAZ1AZJAZ{AY�mAZJAZ  AYƨAY�AYp�AX��AX�AXQ�AX�AW�#AW��AW��AW|�AW�hAW�^AWC�AW"�AW�AV�AVȴAV��AV�\AVn�AVffAV=qAV  AU��AU�ATffASASVAR9XAQp�AP�RAPJAO��AO�AN�uAN-AM�;AM?}AL�AL�RALr�ALQ�AL{AKƨAK�FAK��AK��AK�7AK�AKx�AKdZAKO�AK�AJ�`AJ��AJ{AI��AIG�AHffAGt�AF��AFr�AFJAE��AE�wAE�-AE�^AE�7AE"�AD��AD��ADz�ADI�AD1AC�#AC��ACx�AC;dABv�AB�ABVAB$�AB1AA�AA��AA��AA�^AA��AA�PAA�AA��AA�hAAp�AA`BAA;dAA
=AAVA@r�A@{A@{A@1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      A��#A��A��#A��A��A��/A��#A��#A��/A��TA��HA��HA��TA��TA��`A��`A��TA��mA��TA��`A��yA��`A��TA��TA��A��yA��yA��mA��yA��yAҬA�ffA�9XA��A���A�\)A�O�A�33A���AС�A�v�A�dZA�K�A�1'A�$�A�$�A��;A�S�A�Q�AͶFA�9XA̕�A��A˛�A�=qA�
=A�"�A���A���A��mA��hA���A�p�A��`A��A��wA�=qA��A�bNA���A��A�5?A���A�r�A���A�(�A��Az��Ax�DAs�Ap�An�Ai33Af�AbjA^�RA]�wA[hsAZJAYAW�^AV�yAV1AR��ANȴAL�\AK��AJ��AH$�AE��ADȴACdZAB�AA��AAG�A?��A>jA<��A;S�A:�HA9�mA9%A8E�A7�A6��A6 �A5\)A4z�A4A�A3�TA3;dA1�A0r�A/�-A/&�A/&�A.�yA/dZA/�#A/�-A.v�A-?}A-"�A,�A,��A,Q�A+��A+�A*ffA(�A( �A(JA(JA&�HA%�PA$jA#p�A"��A"A!O�A ��A v�A�Ax�A&�A �A��A/A�9A��A�AK�AVA�A�FA�A��Ap�AG�A�AXA��AĜAA�A��A�HA��AAdZA?}A�A�yA��A �A��AXA�A�!AjAQ�A5?A�A{A{AA��A�;A�;A�;A��A�FA�hAG�A%AVA�A�PA7LAĜA�DAVA��Ap�AoA
�!A
�A
v�A
M�A
�A	�A	A	S�A�HA��A~�A �A�mA��AdZA�A�A��A��Av�AQ�A��Al�AĜAv�A-A�
A��AXA;dA�A~�AE�AbA��A33A ��A ��A A�A @��@�K�@�+@�o@���@��7@���@��@��@���@�~�@��@�?}@��@��/@�z�@��@���@��y@�~�@�-@���@�&�@�Z@��;@�C�@��H@�\@�-@���@��`@�j@��m@�
=@�n�@�J@�O�@�9@��;@�R@��@�p�@�Ĝ@畁@�"�@�+@�p�@�j@���@�dZ@�@⟾@��@�7@�7L@�9@ߝ�@���@ޟ�@��@ݑh@�x�@�?}@��@�bN@�9X@۝�@ۅ@�@��@�G�@��/@�Ĝ@�9X@�dZ@ָR@�ff@�=q@�-@���@�@Ցh@���@ԃ@ӝ�@�K�@��y@ҟ�@�^5@љ�@���@У�@�(�@϶F@�|�@�"�@�v�@���@́@�hs@�X@�7L@�%@��`@�z�@˝�@�C�@��H@ʸR@ʇ+@ɑh@�Ĝ@�Z@�  @�
=@��@Ɨ�@�$�@�hs@��@�z�@ēu@�1@Å@��@�o@��y@¸R@\@�@�O�@��9@��D@�A�@�ƨ@��@��R@�M�@���@�p�@�/@��j@�j@�b@���@�J@���@��@���@�9X@���@��m@��
@�ƨ@�|�@�K�@�+@��!@�E�@�5?@��@�X@���@��9@��
@�
=@���@�v�@�E�@��#@��h@�G�@�%@���@�A�@���@�@���@�^5@�@��-@�p�@�/@��j@�Q�@�A�@��@���@�M�@�-@�J@��^@�&�@���@��D@�r�@�(�@���@�C�@�@���@�M�@�{@��@��@���@�I�@���@�C�@�@��H@��R@��\@�ff@�-@��@�@��@�p�@�7L@��@��9@�j@�Q�@�9X@�(�@�1@��
@���@�t�@�C�@�@�ȴ@�V@�-@��@���@��h@�`B@���@�j@��
@�S�@�@���@���@�n�@�=q@��@���@�X@���@��D@�bN@�1'@�ƨ@�K�@�o@���@�V@��@���@���@��-@�p�@�O�@�G�@��@���@���@��u@�I�@��@���@�\)@���@�~�@�^5@�E�@��@��-@���@�hs@��`@��u@�9X@�  @��@�o@�~�@��@��@���@��-@�p�@�?}@���@���@��j@�bN@��@��@��P@�|�@�l�@�S�@��!@�V@�{@��#@��^@��h@�`B@��`@��9@��D@�z�@�bN@�9X@��;@���@�l�@�\)@�C�@�
=@���@�ff@�$�@�J@���@��h@�hs@�V@���@��u@�bN@�9X@��;@��P@�33@�+@���@���@�n�@�E�@��@���@��@��@��@���@��D@�bN@�A�@�(�@�b@�  @�;@��@
=@~{@}�h@}`B@|��@|��@|9X@{��@{��@{�@{o@zn�@y��@y��@y��@y�@y��@yhs@y�@x��@xr�@x  @w��@w�@w��@w\)@v�@vE�@u�-@uV@t�@t��@t�D@tj@t(�@s�
@s�F@s��@st�@r��@r~�@r�@q��@pĜ@o�;@o
=@nv�@n@m�h@m�@l��@l�D@l�@k�m@k��@kS�@j��@i�@i7L@h�9@hQ�@g��@gK�@g+@f�@f�R@f$�@e�h@d�/@d�@dz�@dZ@d1@c�@b��@a��@a�@`bN@`1'@`  @_�w@_��@_�P@_;d@_
=@^�R@^5?@]��@\z�@\(�@[��@[�F@Zn�@Z�@Z�@ZJ@Y��@Y��@Y�@Y��@Y7L@XA�@W�@Wl�@Vȴ@VV@V@U��@T��@T9X@S�@SS�@S"�@R�\@Q�7@Q&�@P�`@P�@P  @O�@OK�@O�@N��@N@M�-@M�h@MO�@L�@LZ@K�m@K��@KS�@KC�@K33@J�@J��@Jn�@JM�@I�#@I�7@I�7@IX@H��@H�u@HbN@HA�@G��@Gl�@G+@F�@F$�@E��@Ep�@D��@Dj@DI�@DI�@D9X@C�m@C�@CS�@Co@B��@B��@B=q@A�^@Ax�@A7L@@�`@@��@@�u@@�u@@r�@@A�@@  @?\)@?�@>��@>�@>�y@>��@>V@>V@>$�@=@<�/@<Z@<9X@;��@;33@;@:n�@:-@:J@9�@9��@9x�@97L@9&�@8�9@8�u@8bN@8 �@7�@7�P@7+@7�@7
=@6�R@6v�@6{@5�@5�T@5��@5�-@5p�@4��@4(�@3�F@3dZ@3C�@333@3@2�!@2=q@1��@1�7@1%@0�9@0bN@01'@0 �@0  @/�;@/|�@/K�@/+@.��@.�R@.ff@.E�@.E�@-�@-�@,��@,�D@+��@+S�@+o@*�H@*n�@)��@(��@(��@(bN@(1'@'�w@';d@&��@&V@&{@%@%�h@%`B@%�@$�@$9X@$1@$1@#��@#�
@#ƨ@#ƨ@#ƨ@#��@#�@#33@#o@"��@"�!@"�!@"n�@"J@!G�@ ��@ ��@ ��@ r�@  �@�@�P@l�@;d@
=@�@��@E�@{@��@@p�@?}@V@�D@I�@��@��@S�@�!@�\@M�@�@�7@�@�`@�9@Q�@ �@�;@��@�P@�P@K�@+@
=@ȴ@v�@V@{@��@`B@?}@?}@�@�j@�D@9X@�
@��@S�@�@��@��@�\@=q@��@��@x�@�@��@Ĝ@�@�;@|�@l�@K�@�@��@��@�+@ff@5?@$�@{@@�h@p�@O�@/@�@V@�/@�D@Z@(�@�@�@�m@�F@��@S�@"�@
�H@
�\@
n�@
M�@
J@	�#@	��@	��@	��@	�7@	7L@Ĝ@��@��@��A��A��A��A��/A��;A��A��
A��#A��/A��/A��#A��A��#A��#A��A���A��A��/A��#A��A��A��/A��/A��#A��A��A��;A��/A��A��
A��/A��`A��TA��HA��HA��TA��HA��;A��;A��`A��`A��HA��HA��`A��mA��TA��HA��HA��mA��TA��HA��TA��mA��mA��`A��`A��mA��mA��TA��`A��yA��mA��`A��HA��`A��mA��TA��HA��HA��`A��mA��TA��`A��mA��yA��mA��HA��TA��mA��`A��;A��TA��`A��`A��`A��`A��mA��yA��yA��mA��yA��A��A��A��mA��TA��`A��yA��yA��TA��;A��;A��`A��`A��`A��HA��`A��mA��mA��;A��HA��TA��A��A��A��A��A��yA��yA��`A��mA��A��A��A��A��yA��`A��mA��yA��yA��HA��TA��mA��A��yA��mA��HA��mA��A��A��A��A��A��A��A��yA��A���A���A���Aҥ�AҍPA҇+A҉7A�l�A�bNA�p�A�hsA�p�A�?}A�=qA�=qA�5?A�33A�7LA�9XA�33A��A� �A�VA�oA��A��A�bA�A�
=A���Aѧ�AхA�jA�\)A�ZA�ZA�XA�XA�ZA�ZA�XA�O�A�K�A�C�A�?}A�=qA�=qA�7LA�-A�(�A�$�A� �A�{A���A��HA���A�ƨA���AЩ�AУ�AЛ�AЏ\AЃA�|�A�z�A�x�A�v�A�r�A�n�A�n�A�jA�hsA�bNA�^5A�ZA�XA�VA�Q�A�K�A�E�A�C�A�A�A�A�A�=qA�1'A�&�A�"�A�$�A�&�A�&�A�$�A�"�A�"�A�$�A�$�A�+A�&�A�$�A� �A�"�A�&�A�  A��`A��
A���AϼjAϲ-Aϕ�AύPA�n�A�S�A� �A���A��A�Aβ-A΃A��AͰ!A͟�Aͩ�A���A���A���Aͣ�A͍PA�z�A�^5A�33A�$�A�bA��yA�ȴA̲-A̛�ÁA�r�A�l�A�S�A�A�A�(�A�&�A��A���A��
A˺^AˮA˩�A˛�AˋDA�r�A�?}A���Aʰ!A�bNA��mAɓuA��A�A�A�x�A��A��A�XA�VA��A���AŸRA�VA��AļjAēuA�t�A�VA�E�A�?}A�+A�
=A���A��`Aú^AÏ\A�`BA�M�A�A�A�;dA�1'A�$�A� �A�oA�1A���A��mA�`BA���A�n�A�7LA�G�A���A��`A�hsA��A��PA�p�A���A�?}A��`A��A�^5A�-A��TA���A�^5A�ZA��A��A��PA�`BA��A�+A�%A��A���A�;dA�M�A�ȴA�;dA���A�  A���A���A�I�A�oA��A���A��A��PA�5?A��FA�ZA�`BA��A�%A�7LA���A��^A�A�7LA�"�A�A�A�S�A��A��A��HA�jA�oA��yA��#A��jA�bNA�=qA�/A�"�A�bA���A��#A��A�Q�A�-A�{A�%A�1A�A��A��mA��A��yA��HA��/A��/A��;A��TA��mA��A��A���A��TA���A�A�"�A�1'A�+A�&�A�JA��TA��+A�ZA�oA�bA���A��mA���A�G�A�JA�ĜA��!A���A��hA�p�A�K�A�$�A��A��A��mA��TA��mA���A��wA���A�hsA�ZA�G�A��A�VA�  A��A���A�A��FA���A��A�hsA�S�A�5?A��A�1A���A���A�~�A�dZA�ffA�l�A�n�A�l�A�ffA�bNA�hsA�^5A�O�A�1'A�A���A�O�A���A�(�A��A���A�XA�=qA�A�ĜA���A�{A�S�A�ȴA�A���A�n�A��A���A�v�A�1'A��/A��DA�%A��^A�t�A�{A�ĜA��+A�A�A�ƨA�&�A��DA�K�A��RA�G�A��A��\A�x�A�ffA���A�JA~�RA~jA~E�A~A}��A|�yA{XAz��Az�\Az�+AzjAzZAzE�AyƨAyl�Ay?}AyC�Ay&�Ax��Awp�Av  Au�At�/At��AtjAs�#As�hAr��ArAql�Ap�HAp~�Apz�ApQ�Ao�mAo�#Ao�#Ao��Ao��Aol�AoC�Ao�An{Ak�AkAjI�Aix�AiVAh=qAg��Ag�7Ag"�Af�jAf  Ae��Ae�Ael�Ae+Ad�jAd1'AbĜAaVA`��A_�#A_l�A^��A^�A^�A^�A^^5A^I�A^A�A^5?A^�A]�A]7LA]�A\��A\�+A[��A[�A[oAZ�AZ�AZ=qAZ1AZJAZ{AY�mAZJAZ  AYƨAY�AYp�AX��AX�AXQ�AX�AW�#AW��AW��AW|�AW�hAW�^AWC�AW"�AW�AV�AVȴAV��AV�\AVn�AVffAV=qAV  AU��AU�ATffASASVAR9XAQp�AP�RAPJAO��AO�AN�uAN-AM�;AM?}AL�AL�RALr�ALQ�AL{AKƨAK�FAK��AK��AK�7AK�AKx�AKdZAKO�AK�AJ�`AJ��AJ{AI��AIG�AHffAGt�AF��AFr�AFJAE��AE�wAE�-AE�^AE�7AE"�AD��AD��ADz�ADI�AD1AC�#AC��ACx�AC;dABv�AB�ABVAB$�AB1AA�AA��AA��AA�^AA��AA�PAA�AA��AA�hAAp�AA`BAA;dAA
=AAVA@r�A@{A@{A@1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
"4B
"4B
"4B
"hB
"hB
!�B
"hB
"hB
!�B
!�B
!�B
!-B
!bB
!bB
!bB
!bB
 �B
 �B
 �B
 �B
!-B
 �B
 �B
 �B
!�B
!bB
!bB
 �B
 �B
 �B
�B
�B
B
�B
~B
�B
�B
{B
B	�JB	��B	��B	�B	�|B	��B	�TB	�rB	��B	�B
!-B
4B
)*B
+kB
(�B
5�B
9�B
5B
7�B
dZB
D3B
-�B
P}B
f�B
v�B
��B
��B
�EB
�RB
��B
�FB
�zB
�B
��B
{JB
4�B
.B	�5B	ϫB	�EB	�nB	�B	�	B	��B	r|B	o5B	Z�B	Y�B	YKB	K^B	F�B	=<B	6zB	3hB	:^B	8RB	7�B	5tB	6�B	8�B	8RB	TaB	R�B	P�B	TaB	XEB	U2B	IRB	I�B	L�B	kQB	rB	sB	u�B	{�B	}VB	��B	��B	�B	�"B	��B	�B	��B	��B	��B	�qB	�B	�zB	�mB	�TB	�"B	�rB	�]B	�5B	�AB	�+B	�.B
�B
�B
�B	��B
�B
~B
B
�B
"4B
#�B
$tB
%�B
&�B
$@B
'�B
'�B
($B
+�B
-�B
5�B
5tB
7�B
;0B
8�B
<B
?B
@B
<�B
;�B
=<B
=�B
?HB
?HB
@�B
F�B
H�B
K�B
O�B
K^B
H�B
HKB
I�B
K)B
J�B
J�B
K)B
K�B
NpB
NB
O�B
RTB
Q�B
Q�B
Q�B
S[B
TaB
T�B
T�B
U2B
XyB
_�B
b�B
d�B
e�B
f�B
f�B
f2B
ffB
c�B
_�B
]dB
[WB
Y�B
W�B
V9B
T�B
RTB
O�B
O�B
N�B
NpB
NpB
M�B
MB
MB
M6B
L�B
J�B
J�B
J�B
J#B
IRB
J�B
I�B
IRB
H�B
H�B
G�B
GB
GEB
GEB
EmB
C�B
C�B
A�B
A�B
@�B
?�B
?�B
?�B
=�B
=qB
<�B
;�B
9�B
9XB
9�B
8�B
8�B
8RB
7�B
7B
6�B
7�B
5B
49B
49B
3�B
4�B
3hB
2-B
1�B
1�B
0�B
0�B
0UB
0!B
.}B
.IB
.B
.B
,�B
,=B
+6B
+kB
*�B
*eB
)�B
*�B
(�B
(�B
($B
&�B
&�B
&�B
%FB
%�B
$�B
#�B
"�B
#:B
"4B
!bB
 �B
!�B
 �B
�B
�B
�B
�B
VB
�B
OB
�B
�B
B
�B
�B
B
�B
�B
xB
CB
�B
xB
OB
�B
�B
�B
=B
	B
B
~B
B
�B
IB
B
B
IB
�B
B
�B
�B
�B
�B
�B
 \B
!�B
 �B
!-B
!-B
!-B
 �B
 �B
 �B
 �B
 �B
 \B
 'B
 'B
�B
�B
�B
�B
�B
 \B
�B
VB
 �B
 'B
�B
�B
!B
�B
IB
CB
=B
7B
7B
CB
IB
�B
~B
IB
�B
B
�B
OB
�B
�B
B
xB
�B
	B
kB
�B
eB
�B
_B
_B
_B
B
�B
�B
_B
�B
�B
7B
7B
kB
=B
�B
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
�B
�B
�B
!B
�B
�B
!B
�B
!B
�B
�B
 'B
 'B
 'B
�B
 'B
�B
�B
 'B
 �B
!bB
!-B
"4B
!�B
!-B
!-B
 �B
!bB
 �B
 \B
 'B
�B
�B
 �B
!-B
!�B
"�B
"hB
"�B
#B
$tB
#�B
#�B
#�B
#:B
$B
$�B
%B
%�B
&�B
&�B
&�B
&LB
&LB
'RB
'RB
'�B
(XB
)*B
(�B
)_B
)_B
)�B
)�B
*0B
)�B
*eB
*0B
*�B
+B
+B
+�B
+�B
+�B
+�B
,�B
,qB
.}B
.�B
.�B
.IB
/OB
/OB
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1'B
1[B
1'B
1'B
0�B
0UB
0�B
0�B
0�B
0�B
1'B
1�B
1�B
1[B
2-B
1�B
2�B
2-B
3hB
2�B
2�B
2�B
2�B
3hB
2�B
3hB
2�B
3hB
3hB
3�B
4nB
4�B
5�B
6B
6B
6FB
6FB
6�B
6�B
7B
7LB
7B
7�B
8�B
8�B
8�B
8�B
8�B
8�B
:^B
9�B
:*B
:�B
:^B
:�B
:�B
<6B
<B
<B
<B
<B
<jB
=B
=<B
=qB
=<B
=�B
>B
>�B
?B
>�B
>wB
>�B
>�B
?B
?}B
@B
?�B
@B
?�B
@�B
B�B
A�B
A�B
B[B
B'B
B[B
B�B
B�B
B[B
B[B
C�B
C�B
C�B
DgB
D3B
DgB
DgB
DgB
D3B
D3B
C�B
E9B
F?B
F?B
F?B
F�B
GB
GzB
HKB
HB
G�B
H�B
H�B
IRB
I�B
I�B
J#B
I�B
IRB
J�B
JXB
J�B
JXB
JXB
J�B
K)B
K�B
K�B
L0B
L�B
M6B
L�B
MB
M6B
MB
MjB
M6B
M6B
MB
MjB
MjB
MjB
M�B
M�B
N<B
OB
O�B
O�B
O�B
PHB
PHB
PHB
P�B
P�B
P�B
P�B
QB
QNB
R�B
RTB
R�B
R�B
S�B
S[B
S[B
S[B
S[B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U2B
VB
VmB
W?B
W�B
W?B
W�B
W�B
W�B
W�B
XEB
W�B
XB
XEB
YKB
Z�B
ZB
ZB
Z�B
[�B
[WB
[WB
[�B
[WB
[WB
[WB
[�B
[�B
]/B
\�B
\�B
^5B
]�B
^B
^jB
^�B
^�B
^�B
^�B
^�B
_;B
`BB
`B
`B
`vB
`�B
`�B
`�B
`�B
a|B
a|B
a|B
aHB
a�B
bB
a�B
b�B
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
d&B
d&B
dZB
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
g8B
g8B
g8B
gB
g�B
g�B
g�B
g�B
h
B
h
B
hsB
h�B
h�B
h�B
iDB
iB
iB
iB
iyB
i�B
i�B
jKB
jKB
jB
jB
jB
kB
kB
j�B
kB
k�B
l�B
l�B
lWB
m)B
m)B
m)B
m�B
m�B
m�B
m�B
m�B
n/B
ncB
m�B
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
o�B
pB
p;B
p�B
poB
p�B
poB
poB
p�B
p�B
qvB
qvB
qvB
qvB
qvB
qvB
q�B
rB
rGB
rGB
r�B
r�B
sMB
sMB
sMB
sMB
sB
s�B
sMB
sMB
sMB
s�B
s�B
s�B
sMB
t�B
t�B
t�B
t�B
u�B
uZB
u%B
uZB
u�B
v�B
v�B
v�B
w2B
v�B
w�B
wfB
xB
x8B
x�B
yrB
y>B
y>B
y>B
y�B
zB
zxB
zxB
zxB
z�B
z�B
z�B
{B
{B
{JB
{�B
|B
|PB
|PB
|PB
|�B
|�B
}�B
}�B
}VB
}VB
}�B
}�B
~(B
~(B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
.B
�B
�B
�B
�4B
�iB
�4B
�iB
��B
�;B
�B
��B
�B
�uB
��B
�B
�B
�GB
�{B
��B
��B
�MB
�B
�B
�MB
��B
��B
��B
�B
��B
�SB
��B
��B
�%B
��B
��B
��B
��B
�+B
�_B
��B
�1B
��B
�B
��B
�B
�7B
��B
�	B
��B
�rB
�rB
��B
��B
��B
��B
��B
�B
�~B
��B
��B
��B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�"B
�VB
��B
��B
��B
��B
��B
�(B
��B
��B
��B
��B
�bB
��B
��B
��B
�4B
�hB
�hB
�hB
�hB
��B
�:B
��B
�B
�B
��B
!�B
"�B
#B
!�B
 �B
"�B
"�B
"4B
 �B
!�B
"�B
#nB
!�B
!�B
"hB
#:B
"hB
!-B
!�B
#B
"�B
!�B
!�B
#B
#B
#nB
!bB
!�B
#:B
#:B
!-B
 'B
!�B
"hB
"hB
 \B
 �B
!�B
!�B
 �B
 �B
!�B
!�B
!-B
 �B
 �B
!�B
"4B
 'B
!-B
"hB
!�B
 �B
 �B
"4B
!�B
 �B
 �B
"hB
"4B
 �B
 �B
!bB
"�B
!bB
 �B
�B
!�B
!�B
 �B
�B
!-B
"�B
 �B
 'B
!�B
!�B
!-B
 'B
 \B
"hB
 �B
�B
 �B
�B
!�B
 �B
 'B
 'B
!-B
!�B
 'B
!�B
 \B
"hB
!�B
!bB
 �B
�B
 �B
"4B
!�B
 'B
 �B
 �B
"4B
 �B
 'B
 'B
#B
!�B
 \B
 �B
 \B
!bB
"�B
!�B
!�B
 �B
#�B
!-B
!�B
"�B
 �B
 \B
"hB
!bB
!�B
 �B
 \B
!-B
!�B
!�B
 �B
�B
 �B
!bB
�B
!�B
 \B
 \B
!-B
!�B
!�B
 \B
"�B
 \B
!bB
�B
&LB
�B
�B
7B
�B
 \B
B
4B
�B
�B
MB
B
:B
B
�B
:B
bB
B
�B
�B
�B

�B

=B
.B
\B
�B
	7B
�B
�B
�B
�B
{B
oB
 �B
;B
�B
�B
�B
B
MB
MB
�B
�B
MB
B
GB
�B
�B
{B
AB
�B
 �B
B	��B	�"B	�(B	��B	��B	��B	�DB	�B	��B	�8B	�2B	��B	�fB	�lB	�fB	�2B	�+B	�`B	��B	��B	�`B	�ZB	�B	�B	�TB	�B	�TB	�|B	�GB	��B	�MB	�B	�B	�;B	��B	�;B	�B	�B	��B	�B	�vB	�B	�+B	��B	��B	�JB
;B	��B	�fB	�B	��B	��B	�TB	�AB	�`B	�B	�B	�B	�iB	�B	�B	�oB	�%B	�MB	�VB	�B
�B
IB
1�B
;0B
7�B
7�B
9�B
1�B
0�B
0�B
/�B
,B
($B
*�B
,�B
#nB
*�B
.�B
0!B
-wB
(�B
)�B
(�B
'�B
,qB
)�B
'�B
'�B
&�B
%�B
*�B
49B
1�B
2�B
6FB
3hB
GB
?�B
@�B
3hB
33B
?HB
3hB
1[B
/�B
1[B
AUB
6FB
6�B
2�B
3hB
3�B
1�B
0�B
1�B
0�B
-B
,B
-CB
.B
3�B
2-B
0�B
.}B
,�B
)�B
%�B
$B
VB
$B
PB
"4B
8RB
�B
 �B
:�B
)_B
/�B
(�B
<6B
6�B
:*B
qAB
�B
p;B
YB
W�B
Z�B
Z�B
d�B
W�B
VB
ZB
W
B
P}B
U�B
iDB
TaB
H�B
FB
?B
WsB
�4B
y�B
�{B
�B
x8B
k�B
ffB
d&B
\�B
W�B
Q�B
K^B
D3B
I�B
@�B
$B
P}B
49B
A�B
�B
'RB
)�B
!�B
�B
 �B
5�B
-CB
 'B
2aB
4nB
<�B
F?B
IRB
FB
N�B
c B
W?B
XyB
V�B
[�B
Z�B
c�B
c B
{�B
j�B
l�B
h>B
h
B
kB
l�B
t�B
w2B
xlB
zxB
zDB
y	B
zxB
{JB
y	B
y�B
~�B
��B
��B
��B
��B
��B
��B
�jB
�B
��B
�EB
�&B
уB
�BB
�KB
ƨB
�B
�B
ȴB
�B
�)B
�B
�<B
��B
��B
��B
��B
��B
��B
��B
��B
�UB
��B
�B
��B
��B
�nB
��B
�0B
�0B
��B
�tB
�LB
�:B
�@B
��B
�B
�4B
��B
��B
�tB
��B
��B
�eB
�zB
��B
��B
�zB
��B
��B
��B
��B
��B
�B
��B
�BB
ĜB
�UB
�XB
ɆB
��B
�UB
�B
�kB
�hB
��B
�B
�	B
��B
�aB
�B
{�B
c�B
WsB
b�B
OBB
H�B
HB
EmB
C�B
@�B
1'B
/�B
+�B
#nB
�B
B
�B
%�B
	lB
fB
(B	��B	��B
JB
	�B
�B
SB	��B	��B	�#B	�mB	�
B	�yB	�&B	�KB	��B	�?B	ǮB	�gB	�tB	��B	�B	��B	B	�6B	�wB	��B	�B	ԕB	��B	��B	�3B	��B	��B	��B	��B	�0B	��B	��B	�B	�VB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	�IB	��B	��B	�VB	�lB	|�B	z�B	}�B	v�B	v�B	v+B	zB	l�B	k�B	jKB	k�B	m�B	p;B	��B	m]B	g�B	b�B	^5B	[�B	XB	Z�B	[#B	^5B	[�B	W�B	ZB	YB	V�B	qvB	UgB	W?B	a�B	WsB	Z�B	YKB	S�B	Z�B	RTB	P�B	P�B	K^B	M�B	D�B	HKB	OB	H�B	F?B	QB	N<B	D3B	<B	=�B	=�B	A�B	@�B	6zB	B[B	7LB	1�B	6�B	7�B	6zB	7�B	2-B	5B	1[B	2aB	1[B	2aB	6�B	;dB	5tB	?�B	9�B	4nB	8RB	:*B	6�B	7�B	;0B	0UB	9�B	:�B	8�B	8B	5�B	49B	7�B	5B	5tB	6�B	6B	7�B	5tB	4nB	6FB	5B	4�B	6FB	5�B	:�B	33B	7�B	?}B	=�B	6B	/�B	-wB	3�B	3�B	9�B	CaB	P�B	W?B	S�B	T,B	R�B	S�B	R�B	R�B	S&B	N�B	VB	U2B	N�B	QB	RTB	QNB	OB	Q�B	R�B	S&B	T,B	U2B	S�B	W�B	[�B	[�B	XyB	VB	MB	QNB	f2B	R�B	PHB	O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      B
�B
�B
�B
B
B
�B
B
B
�B
}B
HB
�B
B
B
B
B
�B
�B
�B
wB
�B
�B
wB
wB
HB
B
B
�B
wB
wB
^B
?B
�B
	�B
0B	�\B	�hB	�-B	��B	��B	�B	�{B	�:B	�.B	�B	�B	�$B	�B	�PB
�B
-�B
"�B
%B
"sB
/�B
3sB
.�B
1�B
^B
=�B
'^B
J/B
`MB
p{B
~3B
��B
��B
�B
��B
��B
�,B
��B
�pB
t�B
.�B
	�B	��B	�]B	��B	� B	��B	��B	~�B	l.B	h�B	TlB	S�B	R�B	EB	@�B	6�B	0,B	-B	4B	2B	1�B	/&B	0`B	2�B	2B	NB	L;B	J�B	NB	Q�B	N�B	CB	C�B	FB	eB	k�B	l�B	ouB	u�B	wB	z�B	��B	��B	��B	�qB	��B	�NB	��B	�HB	�#B	��B	�,B	�B	�B	��B	�$B	�B	��B	��B	��B	��B	�nB	��B	��B	�CB
 tB
0B
�B
dB
�B
UB
&B
�B
 �B
�B
!mB
!mB
!�B
%�B
'^B
/ZB
/&B
1�B
4�B
2�B
5�B
8�B
9�B
6QB
5KB
6�B
7�B
8�B
8�B
:jB
@�B
B2B
EDB
I�B
EB
B2B
A�B
ClB
D�B
D>B
DsB
D�B
EyB
H"B
G�B
I�B
LB
K5B
KiB
K�B
MB
NB
NGB
N�B
N�B
R+B
YVB
\4B
^uB
_�B
`MB
`MB
_�B
`B
]oB
Y�B
WB
U	B
SfB
QZB
O�B
N�B
LB
I]B
I�B
H�B
H"B
H"B
G�B
F�B
F�B
F�B
FB
D�B
D�B
D�B
C�B
CB
D>B
C�B
CB
B�B
B�B
A�B
@�B
@�B
@�B
?B
=HB
=�B
;pB
;;B
:jB
9cB
9cB
9cB
7WB
7#B
6QB
5B
3�B
3
B
3�B
2mB
2�B
2B
1gB
0�B
0�B
1gB
.�B
-�B
-�B
-�B
.TB
-B
+�B
+BB
+vB
*�B
*<B
*B
)�B
(/B
'�B
'�B
'�B
&WB
%�B
$�B
%B
$�B
$B
#yB
$�B
"?B
"�B
!�B
 �B
 �B
 3B
�B
aB
�B
�B
NB
�B
�B
B
�B
HB
�B
<B
<B
jB
<B
B
�B
B
6B
�B
�B
�B
dB
�B
^B
^B
*B
�B
�B
*B
B
�B
�B
XB
�B
�B
�B
0B
�B
dB
�B
�B
�B
�B
6B
�B
dB
�B
�B
jB
<B
B
�B
�B
�B
�B
�B
wB
BB
wB
BB
BB
B
�B
�B
�B
pB
�B
�B
�B
B
<B
B
wB
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
dB
0B
�B
�B
�B
dB
B
6B
�B
�B
*B
�B
�B
B
LB
B
EB
B
B
B
�B
�B
LB
B
zB
�B
�B
�B
B
�B
XB
^B
�B
^B
�B
�B
<B
<B
pB
�B
jB
6B
6B
jB
�B
jB
<B
�B
6B
�B
�B
<B
�B
�B
�B
�B
�B
�B
pB
�B
�B
B
�B
�B
HB
�B
�B
�B
B
�B
B
�B
<B
�B
BB
�B
HB
NB
B
�B
�B
&B
�B
UB
UB
�B
�B
[B
�B
aB
 3B
 gB
 gB
�B
�B
!B
!B
!mB
"
B
"�B
"�B
#B
#B
#EB
#yB
#�B
#yB
$B
#�B
$KB
$�B
$�B
%QB
%QB
%QB
%QB
&�B
&#B
(/B
(dB
(dB
'�B
)B
)B
)jB
)jB
)5B
*<B
*�B
*pB
*�B
*�B
+vB
+BB
*�B
+B
*�B
*�B
*<B
*B
*<B
*pB
*pB
*pB
*�B
+�B
+BB
+B
+�B
+vB
,HB
+�B
-B
,|B
,HB
,|B
,�B
-B
,HB
-B
,�B
-B
-B
-NB
. B
.�B
/�B
/�B
/�B
/�B
/�B
0`B
0�B
0�B
0�B
0�B
1�B
2mB
2mB
2mB
2mB
28B
28B
4B
3�B
3�B
4EB
4B
4yB
4�B
5�B
5�B
5�B
5�B
5�B
6B
6�B
6�B
7#B
6�B
7�B
7�B
8�B
8�B
8]B
8)B
8�B
8]B
8�B
9/B
9�B
9cB
9�B
9�B
:�B
<AB
;�B
;pB
<B
;�B
<B
<AB
<AB
<B
<B
=|B
=HB
=�B
>B
=�B
>B
>B
>B
=�B
=�B
=|B
>�B
?�B
?�B
?�B
@ZB
@�B
A,B
A�B
A�B
A�B
B2B
B�B
CB
C8B
C�B
C�B
C�B
CB
DsB
D
B
D>B
D
B
D
B
D>B
D�B
EDB
E�B
E�B
FB
F�B
FB
F�B
F�B
F�B
GB
F�B
F�B
F�B
GB
GB
GB
GQB
G�B
G�B
H�B
I�B
I]B
I�B
I�B
I�B
I�B
JcB
J�B
JcB
J�B
J�B
K B
L;B
LB
L�B
L�B
MAB
MB
MB
MB
MB
M�B
NGB
N�B
N|B
N|B
N|B
N�B
N�B
O�B
PB
P�B
Q�B
P�B
Q�B
Q�B
Q�B
QZB
Q�B
Q�B
Q�B
Q�B
R�B
T8B
S�B
S�B
T8B
U�B
U	B
U	B
U>B
U	B
U	B
U	B
U>B
U>B
V�B
VxB
VxB
W�B
W~B
W�B
XB
XPB
XPB
X�B
XPB
XPB
X�B
Y�B
Y�B
Y�B
Z(B
Z\B
Z\B
Z\B
Z\B
[.B
[.B
[.B
Z�B
[�B
[�B
[�B
\iB
\iB
\iB
\4B
\�B
]B
]:B
]:B
]oB
]�B
]�B
]�B
^B
^�B
^�B
^uB
^�B
_B
_GB
_GB
_{B
`B
`MB
`MB
`�B
`�B
`�B
`�B
`�B
aSB
aSB
a�B
a�B
a�B
a�B
b%B
bYB
b�B
b�B
b�B
b�B
b�B
b�B
c+B
c_B
c_B
c�B
c�B
d1B
d1B
d1B
d�B
d�B
d�B
d�B
e7B
f=B
frB
f	B
f�B
f�B
f�B
gCB
gCB
gCB
gxB
gxB
g�B
hB
g�B
h~B
hJB
h~B
h~B
h�B
iPB
iPB
iPB
iPB
i�B
i�B
jVB
j!B
jVB
j!B
j!B
jVB
j�B
k(B
k(B
k(B
k(B
k(B
k(B
k\B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m4B
l�B
l�B
l�B
mhB
mhB
m4B
l�B
n:B
n:B
n�B
n�B
ouB
oB
n�B
oB
o�B
pFB
p�B
pFB
p�B
p�B
qLB
qB
q�B
q�B
r�B
s$B
r�B
r�B
r�B
sYB
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
ueB
u�B
vB
vB
vB
v7B
vkB
w=B
w=B
wB
wB
w=B
wqB
w�B
w�B
w�B
w�B
xB
xB
xCB
xwB
x�B
x�B
yIB
y~B
y~B
y�B
zB
y�B
zB
z�B
z�B
{�B
{�B
{�B
|'B
|�B
|�B
|�B
|�B
}-B
}bB
}�B
}�B
}�B
}�B
}�B
~3B
~hB
~�B
~�B
~�B
B
nB
�B
�B
�B
�@B
�@B
�tB
��B
�B
�zB
��B
��B
��B
��B
��B
��B
�RB
��B
��B
�$B
�$B
�XB
��B
�^B
��B
��B
��B
�0B
��B
��B
��B
��B
��B
��B
��B
�kB
�kB
�kB
��B
��B
��B
��B
�B
�qB
��B
��B
��B
��B
��B
�CB
�CB
�wB
��B
�B
�IB
�IB
�}B
��B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
��B
}B
NB
�B
}B
�B
�B
�B
�B
wB
HB
�B
 B
�B
HB
B
�B
B
�B
HB
�B
�B
}B
HB
�B
�B
 B
B
�B
�B
�B
�B
�B
}B
B
B
B
wB
�B
}B
BB
BB
�B
�B
�B
wB
�B
HB
�B
�B
�B
B
}B
wB
BB
�B
�B
wB
wB
B
�B
wB
wB
B
NB
B
wB
<B
HB
}B
BB
pB
�B
NB
BB
�B
HB
HB
�B
�B
B
B
�B
pB
wB
�B
}B
wB
�B
�B
�B
�B
�B
HB
B
B
}B
B
�B
<B
wB
�B
}B
�B
wB
BB
�B
BB
�B
�B
�B
HB
B
BB
B
B
�B
HB
HB
BB
UB
�B
�B
�B
BB
B
B
B
�B
�B
B
�B
}B
}B
wB
pB
�B
B
�B
}B
B
B
�B
HB
HB
B
NB
B
B
<B
�B
�B
[B
�B
�B
B
�B

�B	�nB
RB
�B
�B
�B
�B

}B
�B

B
�B
9B
	CB
gB
XB
�B
	�B
	B
eB
�B
�B
�B
�B
LB	�-B	�!B	��B	��B	�UB	��B	�hB	��B	��B	��B	�nB	�:B	��B	��B	��B	�\B	��B	�-B	��B	��B	�OB	��B	�wB	��B	��B	�_B	�YB	�YB	��B	��B	�B	��B	��B	�{B	�B	�B	�B	��B	��B	�B	�{B	�B	�B	�B	�hB	�:B	�B	��B	�B	�.B	��B	�B	��B	�\B	�B	��B	�B	��B	�B	�B	�nB	�bB	�(B	�4B	��B	�{B	�uB	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�bB	�bB	�\B	�B	��B	�MB	�!B	��B	��B	�B	��B
zB
�B
+BB
4�B
1gB
1�B
3sB
+BB
*<B
*<B
)jB
%�B
!�B
$�B
&WB
 B
$KB
(dB
)�B
')B
"?B
#yB
"?B
!�B
&#B
#yB
!�B
!mB
 gB
aB
$KB
-�B
+�B
,�B
/�B
-B
@�B
9�B
:jB
-B
,�B
8�B
-B
+B
)�B
+B
;B
/�B
0�B
,|B
-B
-NB
+�B
*pB
+vB
*pB
&�B
%�B
&�B
'�B
-NB
+�B
*�B
(/B
&WB
#�B
�B
�B
B
�B
B
�B
2B
�B
wB
4EB
#B
)5B
"?B
5�B
0�B
3�B
j�B
��B
i�B
S1B
Q�B
TlB
T�B
^AB
QZB
O�B
S�B
P�B
J/B
OMB
b�B
NB
B2B
?�B
8�B
Q%B
��B
s�B
}-B
z�B
q�B
e�B
`B
]�B
VxB
QZB
KiB
EB
=�B
ClB
:jB
�B
J/B
-�B
;�B
3B
!B
#EB
HB
�B
wB
/�B
&�B
�B
,B
. B
6�B
?�B
CB
?�B
H�B
\�B
P�B
R+B
P�B
UrB
T�B
]:B
\�B
u�B
d�B
frB
a�B
a�B
d�B
f�B
nnB
p�B
rB
t*B
s�B
r�B
t*B
t�B
r�B
sYB
xCB
��B
~3B
��B
��B
�9B
��B
�B
��B
�pB
��B
��B
�5B
��B
��B
�ZB
��B
��B
�fB
��B
��B
��B
��B
�]B
�vB
�2B
��B
�8B
��B
�|B
�TB
�B
��B
��B
��B
�gB
� B
��B
��B
��B
�QB
�&B
��B
��B
��B
��B
��B
��B
�wB
��B
�&B
��B
�NB
�B
�,B
��B
��B
�,B
�mB
��B
��B
�|B
��B
��B
�`B
��B
�NB
�B
�
B
�8B
��B
�B
��B
�B
�B
�QB
��B
��B
��B
�B
��B
ueB
]oB
Q%B
\�B
H�B
B�B
A�B
?B
=|B
:jB
*�B
)jB
%�B
 B
pB
�B
<B
�B
B
B
�B	�qB	�@B
�B
�B
aB	�B	�qB	�B	��B	�B	мB	�+B	��B	��B	ħB	��B	�`B	�B	�&B	��B	��B	��B	�AB	��B	�)B	�QB	�B	�GB	�5B	��B	��B	��B	�jB	��B	��B	��B	��B	�KB	��B	�B	�BB	�0B	��B	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	�B	�B	v�B	t�B	w�B	pFB	p{B	o�B	s�B	f�B	e7B	c�B	e�B	gxB	i�B	�tB	gB	a�B	\4B	W�B	U>B	Q�B	T�B	T�B	W�B	U>B	QZB	S�B	R�B	PSB	k(B	OB	P�B	[�B	Q%B	TlB	R�B	MuB	T8B	LB	J�B	J�B	EB	GQB	>NB	A�B	H�B	B�B	?�B	J�B	G�B	=�B	5�B	7�B	7WB	;;B	:jB	0,B	<B	0�B	+BB	0�B	12B	0,B	12B	+�B	.�B	+B	,B	+B	,B	0�B	5B	/&B	9�B	3�B	. B	2B	3�B	0`B	1gB	4�B	*B	3>B	4yB	2�B	1�B	/ZB	-�B	1gB	.�B	/&B	0`B	/�B	1gB	/&B	. B	/�B	.�B	.�B	/�B	/�B	4EB	,�B	1�B	9/B	7WB	/�B	)�B	')B	-NB	-NB	3�B	=B	JcB	P�B	MuB	M�B	LoB	M�B	LoB	L�B	L�B	HWB	O�B	N�B	H�B	J�B	LB	K B	H�B	KiB	L�B	L�B	M�B	N�B	M�B	Q�B	U�B	U�B	R+B	O�B	F�B	K B	_�B	LoB	I�B	I]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230502170102                            20230502170102AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023050217010220230502170102  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023050217010220230502170102QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023050217010220230502170102QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               