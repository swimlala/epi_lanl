CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-20T04:00:43Z creation      
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
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [X   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  c8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɀ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x 0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230720040043  20230720040043  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�9H@��@�9H@��11  @�9H��B�@�9H��B�@0X�n�wp@0X�n�wp�d*gw�Z�d*gw�Z11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?k�?�@@  @}p�@�p�@��R@�G�A   A  A ��A-p�AA�AaG�A\)A�  A�  A��A�  A�  A�  A�A�\)B(�B(�B  B (�B((�B/�
B8(�B@(�BH  BP(�BX  B`  Bh  Bp(�Bx  B�{B�(�B�  B��
B�  B�  B�{B�(�B�(�B�(�B�  B�  B�  B�  B�  B��
B��B�{B�  B�{B�  B��B��B�{B�  B�  B�(�B�{B�{B�{B�(�B�{C 
=C
=C��C  C
=C	��C  C
=C
=C{C�C
=C
=C�C�C  C�HC!��C$  C&  C'��C)�C+�C.  C0  C2
=C4  C5��C7��C:  C<
=C>
=C@  CB
=CD
=CF{CH
=CJ
=CL
=CN  CP
=CR
=CS��CV
=CX{CZ  C[��C^
=C`{Cb
=Cd  Ce��Ch  Cj
=Cl{Cn
=Co��Cr
=Ct
=Cv
=Cx{Cz  C|  C}��C��C�
=C�
=C�C���C���C���C�  C���C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C���C�  C�C�C�  C�  C�  C�  C���C���C�C�
=C�C�  C�C�  C���C���C�  C�C�
=C�C�C�C�C�C�C�C���C�C�
=C�\C�
=C�C�C�C�
=C�
=C�
=C�  C���C��C�  C�C���C���C���C�  C�C�C���C��C���C�C�
=C�  C�  C�  C�  C���C�C�C�  C�  C���C�  C�  C�
=C���C���C�  C�C�
=C�C���C�  C�  C�C�  C���C���C���C���C���C�  C�  C�  C�  C�  C���C�  C���C���C�  C�C�C�  C���C�  C�  C�  C���C���C���C���C�C�  C���D �D � D ��Dz�D�qD� DD��D�D� D  D}qD�D�D��DxRD  D��D�qD	��D
�D
� D  D�D  D}qD�D�D  D}qD�qD�D�D}qD  D��D�D� D  D��D�D��D��Dz�D  D��D�D}qD  D��DD� D�qD� D�D��DD��D  D}qD��D}qD�qD}qD�qD � D!�D!��D"�D"�D#D#� D$�D$�D%�D%��D&  D&� D'�D'��D(D(�D)  D)z�D)��D*z�D*��D+z�D+�qD,� D-  D-� D.  D.��D/  D/��D0�D0�D1�D1��D2�D2� D2�qD3xRD4�D4�D5  D5��D5�qD6z�D6�qD7}qD7�RD8z�D9  D9��D9�qD:xRD:��D;� D<  D<��D=�D=�D>�D>��D>�qD?}qD@  D@��DA�DA��DB�DB��DB�qDC}qDD  DD��DEDE�DF  DF� DG  DGz�DH  DH� DI  DI� DI�qDJ��DK�DK}qDL  DL}qDL��DM�DN�DN}qDO  DO��DP�DPz�DP��DQ�DR  DR}qDS�DS� DT  DT� DUDU��DV  DV}qDW  DW��DX  DX� DY�DY� DY�qDZ� DZ�qD[� D[�qD\� D]D]�D^D^��D_  D_� D`�D`�Da�Da�Db  Db��Dc�Dc��DdDd��De�De� Df  Df� Df��Dg� Dh  Dh}qDi  Di}qDi��Djz�Dj�qDk}qDk�qDl}qDm  Dm��Dn  Dn� Do  Do��Dp�Dp��Dq�Dq}qDr  Dr��Ds�Ds� Ds�qDt� Du�Du�DvDv}qDw�Dw� Dw�qDx}qDy  Dy� Dy�qDzz�Dz��D{z�D|  D|}qD|��D}� D~D~��D  D� D��D�@ D�~�D���D�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�>�D�~�D��HD�HD�@ D��HD�� D���D�=qD�~�D��HD��D�AHD�� D�� D��qD�AHD���D��HD�HD�C�D���D�� D�  D�>�D�~�D�� D��D�B�D���D��HD�HD�>�D�~�D�� D��D�B�D�� D��qD���D�>�D�}qD�� D�HD�AHD��HD�� D�  D�AHD��HD���D�  D�AHD�� D��HD�HD�>�D�� D��HD���D�@ D��HD���D��)D�=qD�~�D��HD��D�@ D�~�D��HD�HD�>�D�~�D���D���D�>�D��HD�D�  D�>�D�~�D���D��qD�<)D�~�D�� D�HD�C�D��HD���D�HD�C�D�� D���D���D�@ D�~�D���D��qD�@ D���D���D��qD�=qD�� D��HD�  D�@ D��HD��HD���D�>�D�~�D�� D�  D�AHD��HD�D�HD�=qD�|)D�� D��D�AHD���D��HD�HD�B�D���D��HD�HD�>�D�� D���D��qD�>�D���D���D�  D�B�D���D�� D�HD�@ D�}qD���D���D�=qD��HD��qD�HD�=qD�~�D��qD���D�C�D�~�D���D�  D�@ D�|)D���D�  D�AHD�� D��HD�HD�AHD��HD�� D�  D�@ D��HD���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�B�D�� D���D�HD�@ D�� D�D���D�B�D�� D���D���D�B�D��HD�� D�HD�=qD�}qD��HD���D�@ D�}qD�� D��D�@ D�}qD�� D���D�@ D�� D�� D�HD�B�D��HD�D��D�AHD���D�� D�HD�AHD�� D��HD�  D�AHD���D�� D�HD�@ D�}qD��HD��D�>�D�|)D�� D���D�>�D���D�� D�  D�AHD���D��HD��)D�>�D�}qD���D�HD�@ D D�� D�HD�@ DÀ D��HD�  D�>�DĀ Dľ�D�  D�@ D�~�Dž�D�HD�AHDƀ D��HD�  D�@ D�~�DǾ�D���D�@ D�~�D�� D��D�AHDɁHD��HD�HD�@ Dʀ D�� D�  D�@ DˁHD�� D�  D�@ D�~�D�� D�  D�AHD́HD�� D�  D�>�D�~�D�� D�  D�@ Dπ D��HD���D�>�DЀ D�� D���D�<)Dр DѾ�D���D�AHDҀ D�� D�HD�=qD�~�D��HD���D�>�DԀ D��HD�HD�=qDՀ D�D���D�>�Dր D�� D��D�>�D�~�D�� D��qD�=qD؀ Dؾ�D��D�@ DفHD�� D��D�@ Dڀ Dھ�D�  D�@ Dۂ�D۾�D�  D�AHD܂�D��HD���D�@ D�~�Dݾ�D�  D�@ Dނ�D�� D���D�@ D߀ D�� D��D�>�D�� D��HD�HD�AHD� D�� D�  D�@ D�~�D�� D�HD�AHD� D��HD�  D�>�D�~�D侸D�  D�@ D�~�D徸D���D�AHD�HD��HD�HD�@ D� D�� D�  D�@ D�~�D辸D���D�>�D�~�D龸D���D�>�D�HD��HD�HD�AHD�HD�� D�HD�AHD�~�D쾸D���D�@ D�HD���D���D�@ D�~�D�� D�HD�AHD�HD�� D���D�B�D��HD��HD�  D�>�D�HD�� D�  D�@ D� D�D�  D�AHD�HD�D���D�@ D� D�D��D�AHD�� D���D�  D�@ D�� D���D���D�@ D�� D��qD�  D�@ D��HD�D�HD�AHD��HD�� D�HD�>�?�?k�?�=q?��R?\?�@�@z�@#�
@333@E�@Y��@p��@�  @��@���@�
=@�G�@�=q@��@��H@\@˅@�z�@�(�@��
@���@�33@�(�AG�AA��A{A�AA��A{A"�\A'
=A*�HA0  A4z�A8��A=p�AA�AFffAJ�HAO\)ATz�AX��A]p�AaG�AeAj�HAn�RAs33Aw�A|��A�  A��A�(�A�{A�Q�A�=qA�z�A��RA���A��\A���A�
=A�G�A��A�{A��A�G�A�(�A�ffA�Q�A��\A�z�A��RA���A��HA��A��RA���A��HA���A�
=A���A�33A��A�
=A���Aʏ\A�z�AθRA���A��HA��A�\)A���A��HA���A�\)A�G�A�33A�p�A�\)A陚A��
A�A�A��A��
A�{A�  A�=qA�(�A�{B Q�BG�BffB\)Bz�B��B�RB�B��B	B
�HB  B�B{B33BQ�BG�BffB\)Bz�B��B�\B�
B�B�B33B(�B�B�\B\)B z�B!B"�\B#�
B$��B%�B'
=B((�B)G�B*ffB+�B,z�B-��B.�RB/�
B0��B2{B3
=B4(�B5p�B6�\B7�B8��B9B:�HB<  B=�B>{B?\)B@Q�BA��BB�HBD(�BE�BF=qBG�BH��BIBJ�HBL  BM�BN=qBO33BPz�BQp�BR=qBS33BTQ�BUp�BV�\BW�BX��BY�B[
=B\(�B]G�B^�\B_�
Ba�Bb=qBc\)BdQ�Bep�Bf�\Bg�
Bh��BiBj�RBk�Bl��Bn{Bo
=Bp(�Bq�BrffBs�Bt��Bv{Bw33Bx(�ByG�BzffB{\)B|��B}��B~�RB�B�Q�B���B�p�B��B�ffB���B��B�  B�z�B��HB�\)B��B�z�B�
=B���B�  B��RB�p�B�{B��\B��B�B�(�B���B��B��B�=qB���B�\)B��B���B�33B��
B�ffB��HB�G�B��
B�Q�B���B���B�(�B��RB�33B���B�{B���B�
=B��B�ffB��HB�p�B�  B��\B���B�G�B��
B�Q�B���B���B�(�B���B���B��B��B�z�B��B�B�=qB��HB�\)B��B�Q�B��RB��B�B�Q�B�
=B��B�(�B��\B���B�p�B�{B���B�\)B��
B�z�B��RB�G�B�B�=qB���B��B�(�B��\B���B�p�B�  B���B�G�B��
B�Q�B���B�p�B�B�=qB���B��B�  B���B�
=B�\)B��B�ffB���B���B�{B��\B��HB�\)B�{B���B�33B���B�{B£�B��B��
B�z�B���BŅB��
B�z�B��BǮB�Q�Bȣ�B�33BɮB�z�B�
=B�\)B��B�z�B�G�B��
B�Q�B���B�G�B�{BУ�B�G�Bљ�B�{B��HBӅB�{B�z�B���B�p�B�=qB���B�\)B�B�=qB��HBٙ�B�=qB�z�B��Bۙ�B�z�B�
=B݅B�  B�z�B�33B��B��\B���B�B�(�B��HB�B��B�ffB�G�B��B�Q�B��HB�p�B�=qB��HB�B��B�\B�G�B�  B�\B��B�B��B�\)B�  B�ffB�
=B�  B�RB��B�B���B�G�B�  B�z�B��B�{B��RB�G�B��B��RB��B�(�B��RB�\)B�Q�B���B��C {C p�C �HC=qCz�C�
CG�C��C�
C(�C��C
=CG�C��C
=Cp�C�RC
=C\)C�
C33Cp�CC=qC��C�
C	33C	�C
  C
G�C
��C{Cp�C�RC
=C\)C�
C�Cz�C�CG�C�C�HC\)CC  CQ�C�
C33Cz�C�
CQ�C�C��CG�C��C33Cp�C��CG�C�\C�C\)C�RC  CffC�
C{CffC�CG�C�C�HCffC��C  Cp�C�RC{C�C�HC(�C�\C  C=qC��C
=CQ�C�C �C ffC �RC!33C!�C!�
C"G�C"��C"�HC#=qC#�RC$  C$Q�C$��C%(�C%p�C%��C&=qC&z�C&�
C'G�C'�\C'��C(ffC(��C)
=C)p�C)�C*�C*z�C*C+�C+�\C+��C,�C,��C,�HC-(�C-��C-�C.33C.�C.��C/=qC/�RC0  C0G�C0C1�C1\)C1��C2(�C2ffC2�HC3=qC3z�C3��C4G�C4�\C5{C5ffC5�C633C6�C6��C7G�C7��C7�C8ffC8�C9�C9�C9C:=qC:�\C:�HC;\)C;��C<{C<ffC<�RC==qC=�C=�HC>\)C>��C?�C?p�C?C@G�C@�C@��CAffCA�CB(�CB�CB��CCQ�CC��CD{CDp�CD�RCE=qCE�CF
=CFffCF�CG33CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                          ?k�?�@@  @}p�@�p�@��R@�G�A   A  A ��A-p�AA�AaG�A\)A�  A�  A��A�  A�  A�  A�A�\)B(�B(�B  B (�B((�B/�
B8(�B@(�BH  BP(�BX  B`  Bh  Bp(�Bx  B�{B�(�B�  B��
B�  B�  B�{B�(�B�(�B�(�B�  B�  B�  B�  B�  B��
B��B�{B�  B�{B�  B��B��B�{B�  B�  B�(�B�{B�{B�{B�(�B�{C 
=C
=C��C  C
=C	��C  C
=C
=C{C�C
=C
=C�C�C  C�HC!��C$  C&  C'��C)�C+�C.  C0  C2
=C4  C5��C7��C:  C<
=C>
=C@  CB
=CD
=CF{CH
=CJ
=CL
=CN  CP
=CR
=CS��CV
=CX{CZ  C[��C^
=C`{Cb
=Cd  Ce��Ch  Cj
=Cl{Cn
=Co��Cr
=Ct
=Cv
=Cx{Cz  C|  C}��C��C�
=C�
=C�C���C���C���C�  C���C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C���C�  C�C�C�  C�  C�  C�  C���C���C�C�
=C�C�  C�C�  C���C���C�  C�C�
=C�C�C�C�C�C�C�C���C�C�
=C�\C�
=C�C�C�C�
=C�
=C�
=C�  C���C��C�  C�C���C���C���C�  C�C�C���C��C���C�C�
=C�  C�  C�  C�  C���C�C�C�  C�  C���C�  C�  C�
=C���C���C�  C�C�
=C�C���C�  C�  C�C�  C���C���C���C���C���C�  C�  C�  C�  C�  C���C�  C���C���C�  C�C�C�  C���C�  C�  C�  C���C���C���C���C�C�  C���D �D � D ��Dz�D�qD� DD��D�D� D  D}qD�D�D��DxRD  D��D�qD	��D
�D
� D  D�D  D}qD�D�D  D}qD�qD�D�D}qD  D��D�D� D  D��D�D��D��Dz�D  D��D�D}qD  D��DD� D�qD� D�D��DD��D  D}qD��D}qD�qD}qD�qD � D!�D!��D"�D"�D#D#� D$�D$�D%�D%��D&  D&� D'�D'��D(D(�D)  D)z�D)��D*z�D*��D+z�D+�qD,� D-  D-� D.  D.��D/  D/��D0�D0�D1�D1��D2�D2� D2�qD3xRD4�D4�D5  D5��D5�qD6z�D6�qD7}qD7�RD8z�D9  D9��D9�qD:xRD:��D;� D<  D<��D=�D=�D>�D>��D>�qD?}qD@  D@��DA�DA��DB�DB��DB�qDC}qDD  DD��DEDE�DF  DF� DG  DGz�DH  DH� DI  DI� DI�qDJ��DK�DK}qDL  DL}qDL��DM�DN�DN}qDO  DO��DP�DPz�DP��DQ�DR  DR}qDS�DS� DT  DT� DUDU��DV  DV}qDW  DW��DX  DX� DY�DY� DY�qDZ� DZ�qD[� D[�qD\� D]D]�D^D^��D_  D_� D`�D`�Da�Da�Db  Db��Dc�Dc��DdDd��De�De� Df  Df� Df��Dg� Dh  Dh}qDi  Di}qDi��Djz�Dj�qDk}qDk�qDl}qDm  Dm��Dn  Dn� Do  Do��Dp�Dp��Dq�Dq}qDr  Dr��Ds�Ds� Ds�qDt� Du�Du�DvDv}qDw�Dw� Dw�qDx}qDy  Dy� Dy�qDzz�Dz��D{z�D|  D|}qD|��D}� D~D~��D  D� D��D�@ D�~�D���D�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�>�D�~�D��HD�HD�@ D��HD�� D���D�=qD�~�D��HD��D�AHD�� D�� D��qD�AHD���D��HD�HD�C�D���D�� D�  D�>�D�~�D�� D��D�B�D���D��HD�HD�>�D�~�D�� D��D�B�D�� D��qD���D�>�D�}qD�� D�HD�AHD��HD�� D�  D�AHD��HD���D�  D�AHD�� D��HD�HD�>�D�� D��HD���D�@ D��HD���D��)D�=qD�~�D��HD��D�@ D�~�D��HD�HD�>�D�~�D���D���D�>�D��HD�D�  D�>�D�~�D���D��qD�<)D�~�D�� D�HD�C�D��HD���D�HD�C�D�� D���D���D�@ D�~�D���D��qD�@ D���D���D��qD�=qD�� D��HD�  D�@ D��HD��HD���D�>�D�~�D�� D�  D�AHD��HD�D�HD�=qD�|)D�� D��D�AHD���D��HD�HD�B�D���D��HD�HD�>�D�� D���D��qD�>�D���D���D�  D�B�D���D�� D�HD�@ D�}qD���D���D�=qD��HD��qD�HD�=qD�~�D��qD���D�C�D�~�D���D�  D�@ D�|)D���D�  D�AHD�� D��HD�HD�AHD��HD�� D�  D�@ D��HD���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�B�D�� D���D�HD�@ D�� D�D���D�B�D�� D���D���D�B�D��HD�� D�HD�=qD�}qD��HD���D�@ D�}qD�� D��D�@ D�}qD�� D���D�@ D�� D�� D�HD�B�D��HD�D��D�AHD���D�� D�HD�AHD�� D��HD�  D�AHD���D�� D�HD�@ D�}qD��HD��D�>�D�|)D�� D���D�>�D���D�� D�  D�AHD���D��HD��)D�>�D�}qD���D�HD�@ D D�� D�HD�@ DÀ D��HD�  D�>�DĀ Dľ�D�  D�@ D�~�Dž�D�HD�AHDƀ D��HD�  D�@ D�~�DǾ�D���D�@ D�~�D�� D��D�AHDɁHD��HD�HD�@ Dʀ D�� D�  D�@ DˁHD�� D�  D�@ D�~�D�� D�  D�AHD́HD�� D�  D�>�D�~�D�� D�  D�@ Dπ D��HD���D�>�DЀ D�� D���D�<)Dр DѾ�D���D�AHDҀ D�� D�HD�=qD�~�D��HD���D�>�DԀ D��HD�HD�=qDՀ D�D���D�>�Dր D�� D��D�>�D�~�D�� D��qD�=qD؀ Dؾ�D��D�@ DفHD�� D��D�@ Dڀ Dھ�D�  D�@ Dۂ�D۾�D�  D�AHD܂�D��HD���D�@ D�~�Dݾ�D�  D�@ Dނ�D�� D���D�@ D߀ D�� D��D�>�D�� D��HD�HD�AHD� D�� D�  D�@ D�~�D�� D�HD�AHD� D��HD�  D�>�D�~�D侸D�  D�@ D�~�D徸D���D�AHD�HD��HD�HD�@ D� D�� D�  D�@ D�~�D辸D���D�>�D�~�D龸D���D�>�D�HD��HD�HD�AHD�HD�� D�HD�AHD�~�D쾸D���D�@ D�HD���D���D�@ D�~�D�� D�HD�AHD�HD�� D���D�B�D��HD��HD�  D�>�D�HD�� D�  D�@ D� D�D�  D�AHD�HD�D���D�@ D� D�D��D�AHD�� D���D�  D�@ D�� D���D���D�@ D�� D��qD�  D�@ D��HD�D�HD�AHD��HD�� D�HD�>�?�?k�?�=q?��R?\?�@�@z�@#�
@333@E�@Y��@p��@�  @��@���@�
=@�G�@�=q@��@��H@\@˅@�z�@�(�@��
@���@�33@�(�AG�AA��A{A�AA��A{A"�\A'
=A*�HA0  A4z�A8��A=p�AA�AFffAJ�HAO\)ATz�AX��A]p�AaG�AeAj�HAn�RAs33Aw�A|��A�  A��A�(�A�{A�Q�A�=qA�z�A��RA���A��\A���A�
=A�G�A��A�{A��A�G�A�(�A�ffA�Q�A��\A�z�A��RA���A��HA��A��RA���A��HA���A�
=A���A�33A��A�
=A���Aʏ\A�z�AθRA���A��HA��A�\)A���A��HA���A�\)A�G�A�33A�p�A�\)A陚A��
A�A�A��A��
A�{A�  A�=qA�(�A�{B Q�BG�BffB\)Bz�B��B�RB�B��B	B
�HB  B�B{B33BQ�BG�BffB\)Bz�B��B�\B�
B�B�B33B(�B�B�\B\)B z�B!B"�\B#�
B$��B%�B'
=B((�B)G�B*ffB+�B,z�B-��B.�RB/�
B0��B2{B3
=B4(�B5p�B6�\B7�B8��B9B:�HB<  B=�B>{B?\)B@Q�BA��BB�HBD(�BE�BF=qBG�BH��BIBJ�HBL  BM�BN=qBO33BPz�BQp�BR=qBS33BTQ�BUp�BV�\BW�BX��BY�B[
=B\(�B]G�B^�\B_�
Ba�Bb=qBc\)BdQ�Bep�Bf�\Bg�
Bh��BiBj�RBk�Bl��Bn{Bo
=Bp(�Bq�BrffBs�Bt��Bv{Bw33Bx(�ByG�BzffB{\)B|��B}��B~�RB�B�Q�B���B�p�B��B�ffB���B��B�  B�z�B��HB�\)B��B�z�B�
=B���B�  B��RB�p�B�{B��\B��B�B�(�B���B��B��B�=qB���B�\)B��B���B�33B��
B�ffB��HB�G�B��
B�Q�B���B���B�(�B��RB�33B���B�{B���B�
=B��B�ffB��HB�p�B�  B��\B���B�G�B��
B�Q�B���B���B�(�B���B���B��B��B�z�B��B�B�=qB��HB�\)B��B�Q�B��RB��B�B�Q�B�
=B��B�(�B��\B���B�p�B�{B���B�\)B��
B�z�B��RB�G�B�B�=qB���B��B�(�B��\B���B�p�B�  B���B�G�B��
B�Q�B���B�p�B�B�=qB���B��B�  B���B�
=B�\)B��B�ffB���B���B�{B��\B��HB�\)B�{B���B�33B���B�{B£�B��B��
B�z�B���BŅB��
B�z�B��BǮB�Q�Bȣ�B�33BɮB�z�B�
=B�\)B��B�z�B�G�B��
B�Q�B���B�G�B�{BУ�B�G�Bљ�B�{B��HBӅB�{B�z�B���B�p�B�=qB���B�\)B�B�=qB��HBٙ�B�=qB�z�B��Bۙ�B�z�B�
=B݅B�  B�z�B�33B��B��\B���B�B�(�B��HB�B��B�ffB�G�B��B�Q�B��HB�p�B�=qB��HB�B��B�\B�G�B�  B�\B��B�B��B�\)B�  B�ffB�
=B�  B�RB��B�B���B�G�B�  B�z�B��B�{B��RB�G�B��B��RB��B�(�B��RB�\)B�Q�B���B��C {C p�C �HC=qCz�C�
CG�C��C�
C(�C��C
=CG�C��C
=Cp�C�RC
=C\)C�
C33Cp�CC=qC��C�
C	33C	�C
  C
G�C
��C{Cp�C�RC
=C\)C�
C�Cz�C�CG�C�C�HC\)CC  CQ�C�
C33Cz�C�
CQ�C�C��CG�C��C33Cp�C��CG�C�\C�C\)C�RC  CffC�
C{CffC�CG�C�C�HCffC��C  Cp�C�RC{C�C�HC(�C�\C  C=qC��C
=CQ�C�C �C ffC �RC!33C!�C!�
C"G�C"��C"�HC#=qC#�RC$  C$Q�C$��C%(�C%p�C%��C&=qC&z�C&�
C'G�C'�\C'��C(ffC(��C)
=C)p�C)�C*�C*z�C*C+�C+�\C+��C,�C,��C,�HC-(�C-��C-�C.33C.�C.��C/=qC/�RC0  C0G�C0C1�C1\)C1��C2(�C2ffC2�HC3=qC3z�C3��C4G�C4�\C5{C5ffC5�C633C6�C6��C7G�C7��C7�C8ffC8�C9�C9�C9C:=qC:�\C:�HC;\)C;��C<{C<ffC<�RC==qC=�C=�HC>\)C>��C?�C?p�C?C@G�C@�C@��CAffCA�CB(�CB�CB��CCQ�CC��CD{CDp�CD�RCE=qCE�CF
=CFffCF�CG33CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�7LA�9XA�7LA�7LA�5?A�5?A�7LA�33A�7LA�9XA�+A�&�A�$�A�&�A�
=A��A��`A��HA��#A��
A���A���A�ȴA�ƨA�A���AӾwAӺ^AӺ^AӼjAӺ^AӸRAӶFAӲ-AӮAә�A�z�A�-A�JA�hsA�C�A��TA�{A�ZÁA�A�VA�;dA���A��A��A���A��A��AÝ�A��A²-A�x�A��9A��!A�ffA�1'A�33A�JA��A���A�A�A��A�x�A��jA�JA�E�A�  A��7A���A�%A���A�{A��mA��-A���A��A�A�1'A�A���A��A��;A�E�A�"�A�/A��^A��HA��mA���A�1'A�^5A���A��HA��A�/A��A���A��uA�A��#A�
=A�%A���A���A��A��uA�Q�A�M�A~��A{�Ax�DAuVAtbNApbAlVAiAe��Ab��A_A^Q�A[��AR�AN�DAM��AM&�AL1AH��AE��ADȴAC��ACC�AB�jAA�PA@�\A?�A=XA;��A:��A7��A6��A65?A5��A4�yA4ffA2A/��A/�A.�!A.�A-A+�7A)&�A&�A&1'A%l�A$�A$A�A#`BA"$�A bA�HA��AbA��AZA��A�A33A^5A1A�AJAXA��A��A��AƨA�A�jA�;AA�AbNA�\A�A�wAA
�+A
=qA	�A	XA	A^5A�AdZAZA�Ax�AVA�A�A��AȴA�A5?A�7AoA~�A��A��AS�A �HA  �@��\@�@�X@���@��@�b@��@�{@�`B@�&�@�O�@���@��m@�@��y@���@�Q�@��@��y@�J@���@�%@�I�@�  @��@�7@���@�r�@�C�@�5?@�/@�Ĝ@��@�ƨ@�S�@�@�M�@�@�@�9@�j@��@�;d@�+@�5?@�@��@�V@�  @�l�@�=q@���@�?}@�`B@��u@� �@���@߾w@��H@ݡ�@�X@�z�@�1'@��@�l�@��@�M�@ٺ^@�?}@�V@���@�b@���@���@�z�@��m@���@҇+@�J@���@��m@��@ΰ!@Η�@��@�?}@�1@���@ʇ+@��@ɺ^@�x�@�%@�j@�b@Ǖ�@��@�=q@�J@�V@þw@�@���@�ȴ@+@��h@�X@�V@��/@���@��@�Q�@���@�n�@��@��@��
@�dZ@�\)@��@�n�@�@��^@�7L@���@� �@�S�@�33@��H@���@�M�@�J@��#@��7@�?}@���@���@��@�l�@��+@�=q@��T@���@��-@��7@��@���@�A�@�1@���@�G�@���@��9@��j@���@�z�@�j@�Q�@�9X@��@���@��!@��@��@�{@���@�p�@���@��m@��F@��+@�E�@��@��@��@�?}@�p�@�p�@���@���@�p�@��@�A�@�1'@�1'@��;@��@�o@���@��@���@�O�@��@���@�bN@�  @��P@�dZ@�S�@�33@���@�^5@�=q@�@���@��@�Ĝ@�bN@� �@�ƨ@���@�\)@�"�@�"�@�o@��R@��\@�E�@�=q@�J@��#@���@��@�p�@�hs@�O�@�/@��9@�r�@�Z@�z�@���@���@�z�@�1@�o@��R@��R@�ff@�V@���@���@�@�/@���@�Q�@�A�@�A�@�1@���@���@�ff@�5?@�$�@�=q@�V@�ff@�^5@���@�hs@�/@�O�@�`B@�hs@�`B@�G�@��@��u@��@�j@��@���@��F@�l�@�\)@�;d@�o@��H@�ff@��@��@��7@�7L@�/@��@���@�z�@�Q�@�(�@���@��@��@�\)@�K�@�33@��@���@���@�^5@�-@�@��#@���@���@���@��h@�%@���@�z�@�1@���@�t�@��@���@�5?@��@�@�p�@��@��9@��@��u@�I�@��m@��
@��
@��
@���@���@��@�;d@�+@��@���@�~�@�ff@�E�@�5?@�{@��@��h@�`B@��@��/@��@�r�@�(�@��@�;d@�
=@���@��y@�ȴ@��\@�M�@�J@��T@���@���@�@���@�x�@�7L@��j@�bN@��@�@;d@~�R@~5?@}�T@}�h@}�@|��@|Z@{�m@{S�@z�\@y��@y�^@x�`@xQ�@x �@x  @w�w@w�P@v�@vV@v@u��@uO�@t��@t�@t�@s��@s33@r��@rM�@q��@qG�@p��@pĜ@pr�@o�@o�P@oK�@o�@n�y@n��@m�@mp�@mO�@l��@lZ@kƨ@kS�@k33@ko@k@j�H@jM�@i�^@i�7@ihs@i�@hbN@h  @g��@g;d@f�R@fE�@f@e@e�@e?}@d�@d�/@d�D@cƨ@cC�@c@b^5@a��@a��@a7L@`�`@`Ĝ@`�u@`  @_+@^�@^�R@^�+@^v�@^E�@]�T@]�@]�@\��@\�@\��@\j@\1@[�F@[dZ@["�@Z��@ZJ@Y��@Y�7@Yhs@YG�@X�9@XA�@Xb@W�;@W��@W;d@W+@V��@V@U�@Up�@Up�@U`B@T�@T�D@T(�@Sƨ@SC�@So@R�H@R��@R�!@Rn�@RJ@Q��@QX@Q&�@P�9@P  @Ol�@O�@M��@M�h@Mp�@M`B@M?}@L�/@L��@L�D@Lz�@LI�@K�F@K"�@J�\@J^5@JM�@J-@I��@I�@HA�@G�@G|�@GK�@G�@F��@FE�@F{@E��@EO�@E?}@E/@D�/@D��@DI�@D�@Cƨ@C@B-@A��@A&�@@��@@r�@@1'@@b@?�@?��@?\)@>��@>@=��@=?}@<��@<�D@<j@<I�@;��@;��@;33@:��@:-@9�#@9��@9��@9G�@8�9@8bN@8A�@8 �@7�@7�w@7+@6�+@5�@5�-@5`B@5�@4�@3�m@3S�@2��@2M�@2�@1�^@1hs@17L@1&�@1�@1%@0�@01'@0b@/�;@/|�@/;d@.�y@.�R@.��@.ff@-�T@-�@,��@,��@,�D@,I�@+�m@+�m@+�m@+�
@+�F@+��@+dZ@+S�@+@*��@*~�@*M�@)�@)��@)hs@)�@(�`@(��@(��@(��@(�@(Q�@'�w@&�R@&v�@&E�@&$�@&{@&{@&@&@%��@%�h@%`B@%�@$��@$�j@$�@$z�@$j@$I�@$9X@$(�@#��@#�
@#��@#"�@"�H@"��@"�\@"=q@"�@!��@!�@!�^@!X@!7L@!&�@!�@ Ĝ@ Q�@ b@�;@��@�w@��@�P@\)@��@��@v�@v�@v�@v�@v�@5?@@�T@��@��@p�@?}@��@�j@�D@Z@1@��@dZ@33@�@�!@�\@n�@-@��@��@hs@7L@&�@%@��@��@��@Ĝ@��@A�@ �@�;@��@�w@�w@�w@�w@�@K�@�@
=@��@�R@�+@ff@E�@$�@{@�T@��@�-@��@�h@p�@/@�@�/@��@�D@Z@��@�F@��@��@dZ@S�@S�@C�@C�@"�@�@��@~�@=q@��@�^@hs@G�@�@Ĝ@�9@�9@�@A�@  @�;@��@�w@|�@;d@;d@�@��@�@�@ȴ@�R@�+@v�@V@$�@{@�T@�-@��A�9XA�?}A�=qA�9XA�A�A�5?A�;dA�5?A�9XA�9XA�7LA�9XA�33A�33A�9XA�7LA�5?A�5?A�33A�33A�/A�33A�5?A�33A�1'A�7LA�5?A�7LA�7LA�7LA�7LA�7LA�5?A�7LA�5?A�9XA�9XA�9XA�=qA�;dA�=qA�=qA�9XA�(�A��A�(�A�&�A�&�A�&�A� �A�"�A�(�A�+A�&�A�&�A�"�A� �A�$�A�$�A� �A�"�A�+A�"�A�-A�1'A�5?A�1'A�$�A��A��A�A��A��A��A�
=A��A���A��A��A��A��mA��`A��`A��mA��`A��`A��TA��TA��TA��TA��TA��HA��HA��HA��TA��TA��TA��;A��/A��#A��#A��#A��#A��#A��/A��/A��/A��/A��#A��#A��#A��#A��A��A��#A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA���A���A�ȴA�ȴA�ȴA�ƨA�ƨA�ĜA�ƨA�ƨA�ĜA�A�A�A���A���A�A�A�A�A�A�A�A���A���A���A���A���A�A���AӾwAӾwAӼjAӾwAӼjAӾwAӼjAӾwAӾwAӼjAӾwAӾwAӾwAӾwAӼjAӼjAӼjAӼjAӺ^AӺ^AӺ^AӸRAӸRAӸRAӸRAӸRAӸRAӸRAӺ^AӼjAӼjAӼjAӺ^AӺ^AӺ^AӺ^AӺ^AӺ^AӸRAӸRAӶFAӶFAӲ-AӴ9AӰ!AӰ!AӮAӰ!AӰ!AӰ!AӮAӰ!AӰ!AӰ!AӲ-AӰ!AӰ!AӮAө�Aӧ�Aӡ�Aӛ�Aӕ�Aӏ\Aӏ\AӍPAӍPAӋDAӉ7AӃA�t�A�hsA�hsA�\)A�ZA�Q�A�G�A�?}A�;dA�+A��A���A҅A�C�A�ƨA���A�G�Aκ^A�z�A�l�A�jA�jA�jA�ffA�\)A�\)A�\)A�ZA�Q�A�K�A�E�A�&�A��A�bA���A��A��yA��/A���A���Aʹ9A͝�A̓A�C�A�  A��A̮A�bNA�S�A�^5A�^5A�^5A�ZA�XA�VA�M�A�VA�ZA�\)A�p�Ả7A̡�A̸RA���A��TA��A�A̾wA̶FA̲-A̛�A�z�A��mA�ƨAˋDA�bAʴ9A�p�A�G�A�oAɰ!A�E�A��A���Aȝ�A�z�A�G�A��A�bA�1A��A���Aǰ!AǅA�z�A�S�A�+A�A��yA��
A�A�t�A�C�A�;dA�7LA�bA��HA���A��A���A�ĜA�ƨA�ȴA���AŰ!Aŗ�AŅA�;dA� �A�A��/A���AĲ-Aě�AċDA�`BA�=qA�{A���A��A��TA��;A���A�Aò-AÙ�AÉ7A�n�A�VA�`BA�dZA�+A�oA���A��A��mA��#A�ȴA¶FA©�A�A�A�A�A�A�A�A�z�A�O�A�+A�1A��mA�ĜA��9A���A���A�v�A�O�A��A��mA��A��PA�n�A�ZA�33A�VA��;A��DA��A��A��7A�G�A��A��A�(�A�/A�33A�9XA�9XA�;dA�;dA�5?A�-A�+A�$�A�bA�r�A�
=A���A�`BA��yA���A�`BA�^5A�x�A��PA���A��A��9A��jA��!A���A�O�A�=qA�9XA�;dA�C�A�K�A�A�A��A�  A��yA��;A���A��FA���A���A�r�A�XA�7LA��A��A��wA��A��PA�hsA�XA��A���A��yA��;A��A��jA���A�ffA�ĜA���A�hsA�-A�A���A�ƨA��^A��9A��PA��A�v�A�n�A�hsA�C�A���A�hsA�O�A��
A�|�A�Q�A�VA���A�ȴA��-A�v�A�bA���A�E�A��A��A���A�l�A��;A��A�S�A�
=A��A���A��
A���A���A�ȴA�ȴA���A��hA���A���A���A��+A��DA�n�A�1'A�%A��`A��;A��/A���A�t�A���A���A��A���A�v�A�\)A�;dA�"�A��A���A�l�A�1'A��/A���A�z�A�VA�?}A�JA��;A��FA�p�A�;dA��A��yA��TA��wA���A���A�E�A���A�5?A��`A�|�A�A�A�-A�(�A�+A�(�A�$�A�+A�(�A��A�bA���A���A�jA��HA�^5A�7LA�(�A�JA��A���A�VA��A��hA�I�A���A��A�
=A��^A��hA�$�A��/A���A�A��hA�C�A��A��uA�5?A��;A��DA�bNA���A���A��!A���A��7A�n�A�;dA�VA��A���A���A���A���A��PA�bNA�=qA��yA���A�G�A��A�\)A�A���A��!A��A�\)A�A�A��A���A��HA��RA�|�A�"�A��A�{A��!A��PA�1'A��DA���A�p�A�bA���A�1'A���A�ĜA�r�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                          A�9XA�7LA�9XA�7LA�7LA�5?A�5?A�7LA�33A�7LA�9XA�+A�&�A�$�A�&�A�
=A��A��`A��HA��#A��
A���A���A�ȴA�ƨA�A���AӾwAӺ^AӺ^AӼjAӺ^AӸRAӶFAӲ-AӮAә�A�z�A�-A�JA�hsA�C�A��TA�{A�ZÁA�A�VA�;dA���A��A��A���A��A��AÝ�A��A²-A�x�A��9A��!A�ffA�1'A�33A�JA��A���A�A�A��A�x�A��jA�JA�E�A�  A��7A���A�%A���A�{A��mA��-A���A��A�A�1'A�A���A��A��;A�E�A�"�A�/A��^A��HA��mA���A�1'A�^5A���A��HA��A�/A��A���A��uA�A��#A�
=A�%A���A���A��A��uA�Q�A�M�A~��A{�Ax�DAuVAtbNApbAlVAiAe��Ab��A_A^Q�A[��AR�AN�DAM��AM&�AL1AH��AE��ADȴAC��ACC�AB�jAA�PA@�\A?�A=XA;��A:��A7��A6��A65?A5��A4�yA4ffA2A/��A/�A.�!A.�A-A+�7A)&�A&�A&1'A%l�A$�A$A�A#`BA"$�A bA�HA��AbA��AZA��A�A33A^5A1A�AJAXA��A��A��AƨA�A�jA�;AA�AbNA�\A�A�wAA
�+A
=qA	�A	XA	A^5A�AdZAZA�Ax�AVA�A�A��AȴA�A5?A�7AoA~�A��A��AS�A �HA  �@��\@�@�X@���@��@�b@��@�{@�`B@�&�@�O�@���@��m@�@��y@���@�Q�@��@��y@�J@���@�%@�I�@�  @��@�7@���@�r�@�C�@�5?@�/@�Ĝ@��@�ƨ@�S�@�@�M�@�@�@�9@�j@��@�;d@�+@�5?@�@��@�V@�  @�l�@�=q@���@�?}@�`B@��u@� �@���@߾w@��H@ݡ�@�X@�z�@�1'@��@�l�@��@�M�@ٺ^@�?}@�V@���@�b@���@���@�z�@��m@���@҇+@�J@���@��m@��@ΰ!@Η�@��@�?}@�1@���@ʇ+@��@ɺ^@�x�@�%@�j@�b@Ǖ�@��@�=q@�J@�V@þw@�@���@�ȴ@+@��h@�X@�V@��/@���@��@�Q�@���@�n�@��@��@��
@�dZ@�\)@��@�n�@�@��^@�7L@���@� �@�S�@�33@��H@���@�M�@�J@��#@��7@�?}@���@���@��@�l�@��+@�=q@��T@���@��-@��7@��@���@�A�@�1@���@�G�@���@��9@��j@���@�z�@�j@�Q�@�9X@��@���@��!@��@��@�{@���@�p�@���@��m@��F@��+@�E�@��@��@��@�?}@�p�@�p�@���@���@�p�@��@�A�@�1'@�1'@��;@��@�o@���@��@���@�O�@��@���@�bN@�  @��P@�dZ@�S�@�33@���@�^5@�=q@�@���@��@�Ĝ@�bN@� �@�ƨ@���@�\)@�"�@�"�@�o@��R@��\@�E�@�=q@�J@��#@���@��@�p�@�hs@�O�@�/@��9@�r�@�Z@�z�@���@���@�z�@�1@�o@��R@��R@�ff@�V@���@���@�@�/@���@�Q�@�A�@�A�@�1@���@���@�ff@�5?@�$�@�=q@�V@�ff@�^5@���@�hs@�/@�O�@�`B@�hs@�`B@�G�@��@��u@��@�j@��@���@��F@�l�@�\)@�;d@�o@��H@�ff@��@��@��7@�7L@�/@��@���@�z�@�Q�@�(�@���@��@��@�\)@�K�@�33@��@���@���@�^5@�-@�@��#@���@���@���@��h@�%@���@�z�@�1@���@�t�@��@���@�5?@��@�@�p�@��@��9@��@��u@�I�@��m@��
@��
@��
@���@���@��@�;d@�+@��@���@�~�@�ff@�E�@�5?@�{@��@��h@�`B@��@��/@��@�r�@�(�@��@�;d@�
=@���@��y@�ȴ@��\@�M�@�J@��T@���@���@�@���@�x�@�7L@��j@�bN@��@�@;d@~�R@~5?@}�T@}�h@}�@|��@|Z@{�m@{S�@z�\@y��@y�^@x�`@xQ�@x �@x  @w�w@w�P@v�@vV@v@u��@uO�@t��@t�@t�@s��@s33@r��@rM�@q��@qG�@p��@pĜ@pr�@o�@o�P@oK�@o�@n�y@n��@m�@mp�@mO�@l��@lZ@kƨ@kS�@k33@ko@k@j�H@jM�@i�^@i�7@ihs@i�@hbN@h  @g��@g;d@f�R@fE�@f@e@e�@e?}@d�@d�/@d�D@cƨ@cC�@c@b^5@a��@a��@a7L@`�`@`Ĝ@`�u@`  @_+@^�@^�R@^�+@^v�@^E�@]�T@]�@]�@\��@\�@\��@\j@\1@[�F@[dZ@["�@Z��@ZJ@Y��@Y�7@Yhs@YG�@X�9@XA�@Xb@W�;@W��@W;d@W+@V��@V@U�@Up�@Up�@U`B@T�@T�D@T(�@Sƨ@SC�@So@R�H@R��@R�!@Rn�@RJ@Q��@QX@Q&�@P�9@P  @Ol�@O�@M��@M�h@Mp�@M`B@M?}@L�/@L��@L�D@Lz�@LI�@K�F@K"�@J�\@J^5@JM�@J-@I��@I�@HA�@G�@G|�@GK�@G�@F��@FE�@F{@E��@EO�@E?}@E/@D�/@D��@DI�@D�@Cƨ@C@B-@A��@A&�@@��@@r�@@1'@@b@?�@?��@?\)@>��@>@=��@=?}@<��@<�D@<j@<I�@;��@;��@;33@:��@:-@9�#@9��@9��@9G�@8�9@8bN@8A�@8 �@7�@7�w@7+@6�+@5�@5�-@5`B@5�@4�@3�m@3S�@2��@2M�@2�@1�^@1hs@17L@1&�@1�@1%@0�@01'@0b@/�;@/|�@/;d@.�y@.�R@.��@.ff@-�T@-�@,��@,��@,�D@,I�@+�m@+�m@+�m@+�
@+�F@+��@+dZ@+S�@+@*��@*~�@*M�@)�@)��@)hs@)�@(�`@(��@(��@(��@(�@(Q�@'�w@&�R@&v�@&E�@&$�@&{@&{@&@&@%��@%�h@%`B@%�@$��@$�j@$�@$z�@$j@$I�@$9X@$(�@#��@#�
@#��@#"�@"�H@"��@"�\@"=q@"�@!��@!�@!�^@!X@!7L@!&�@!�@ Ĝ@ Q�@ b@�;@��@�w@��@�P@\)@��@��@v�@v�@v�@v�@v�@5?@@�T@��@��@p�@?}@��@�j@�D@Z@1@��@dZ@33@�@�!@�\@n�@-@��@��@hs@7L@&�@%@��@��@��@Ĝ@��@A�@ �@�;@��@�w@�w@�w@�w@�@K�@�@
=@��@�R@�+@ff@E�@$�@{@�T@��@�-@��@�h@p�@/@�@�/@��@�D@Z@��@�F@��@��@dZ@S�@S�@C�@C�@"�@�@��@~�@=q@��@�^@hs@G�@�@Ĝ@�9@�9@�@A�@  @�;@��@�w@|�@;d@;d@�@��@�@�@ȴ@�R@�+@v�@V@$�@{@�T@�-@��A�9XA�?}A�=qA�9XA�A�A�5?A�;dA�5?A�9XA�9XA�7LA�9XA�33A�33A�9XA�7LA�5?A�5?A�33A�33A�/A�33A�5?A�33A�1'A�7LA�5?A�7LA�7LA�7LA�7LA�7LA�5?A�7LA�5?A�9XA�9XA�9XA�=qA�;dA�=qA�=qA�9XA�(�A��A�(�A�&�A�&�A�&�A� �A�"�A�(�A�+A�&�A�&�A�"�A� �A�$�A�$�A� �A�"�A�+A�"�A�-A�1'A�5?A�1'A�$�A��A��A�A��A��A��A�
=A��A���A��A��A��A��mA��`A��`A��mA��`A��`A��TA��TA��TA��TA��TA��HA��HA��HA��TA��TA��TA��;A��/A��#A��#A��#A��#A��#A��/A��/A��/A��/A��#A��#A��#A��#A��A��A��#A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA���A���A�ȴA�ȴA�ȴA�ƨA�ƨA�ĜA�ƨA�ƨA�ĜA�A�A�A���A���A�A�A�A�A�A�A�A���A���A���A���A���A�A���AӾwAӾwAӼjAӾwAӼjAӾwAӼjAӾwAӾwAӼjAӾwAӾwAӾwAӾwAӼjAӼjAӼjAӼjAӺ^AӺ^AӺ^AӸRAӸRAӸRAӸRAӸRAӸRAӸRAӺ^AӼjAӼjAӼjAӺ^AӺ^AӺ^AӺ^AӺ^AӺ^AӸRAӸRAӶFAӶFAӲ-AӴ9AӰ!AӰ!AӮAӰ!AӰ!AӰ!AӮAӰ!AӰ!AӰ!AӲ-AӰ!AӰ!AӮAө�Aӧ�Aӡ�Aӛ�Aӕ�Aӏ\Aӏ\AӍPAӍPAӋDAӉ7AӃA�t�A�hsA�hsA�\)A�ZA�Q�A�G�A�?}A�;dA�+A��A���A҅A�C�A�ƨA���A�G�Aκ^A�z�A�l�A�jA�jA�jA�ffA�\)A�\)A�\)A�ZA�Q�A�K�A�E�A�&�A��A�bA���A��A��yA��/A���A���Aʹ9A͝�A̓A�C�A�  A��A̮A�bNA�S�A�^5A�^5A�^5A�ZA�XA�VA�M�A�VA�ZA�\)A�p�Ả7A̡�A̸RA���A��TA��A�A̾wA̶FA̲-A̛�A�z�A��mA�ƨAˋDA�bAʴ9A�p�A�G�A�oAɰ!A�E�A��A���Aȝ�A�z�A�G�A��A�bA�1A��A���Aǰ!AǅA�z�A�S�A�+A�A��yA��
A�A�t�A�C�A�;dA�7LA�bA��HA���A��A���A�ĜA�ƨA�ȴA���AŰ!Aŗ�AŅA�;dA� �A�A��/A���AĲ-Aě�AċDA�`BA�=qA�{A���A��A��TA��;A���A�Aò-AÙ�AÉ7A�n�A�VA�`BA�dZA�+A�oA���A��A��mA��#A�ȴA¶FA©�A�A�A�A�A�A�A�A�z�A�O�A�+A�1A��mA�ĜA��9A���A���A�v�A�O�A��A��mA��A��PA�n�A�ZA�33A�VA��;A��DA��A��A��7A�G�A��A��A�(�A�/A�33A�9XA�9XA�;dA�;dA�5?A�-A�+A�$�A�bA�r�A�
=A���A�`BA��yA���A�`BA�^5A�x�A��PA���A��A��9A��jA��!A���A�O�A�=qA�9XA�;dA�C�A�K�A�A�A��A�  A��yA��;A���A��FA���A���A�r�A�XA�7LA��A��A��wA��A��PA�hsA�XA��A���A��yA��;A��A��jA���A�ffA�ĜA���A�hsA�-A�A���A�ƨA��^A��9A��PA��A�v�A�n�A�hsA�C�A���A�hsA�O�A��
A�|�A�Q�A�VA���A�ȴA��-A�v�A�bA���A�E�A��A��A���A�l�A��;A��A�S�A�
=A��A���A��
A���A���A�ȴA�ȴA���A��hA���A���A���A��+A��DA�n�A�1'A�%A��`A��;A��/A���A�t�A���A���A��A���A�v�A�\)A�;dA�"�A��A���A�l�A�1'A��/A���A�z�A�VA�?}A�JA��;A��FA�p�A�;dA��A��yA��TA��wA���A���A�E�A���A�5?A��`A�|�A�A�A�-A�(�A�+A�(�A�$�A�+A�(�A��A�bA���A���A�jA��HA�^5A�7LA�(�A�JA��A���A�VA��A��hA�I�A���A��A�
=A��^A��hA�$�A��/A���A�A��hA�C�A��A��uA�5?A��;A��DA�bNA���A���A��!A���A��7A�n�A�;dA�VA��A���A���A���A���A��PA�bNA�=qA��yA���A�G�A��A�\)A�A���A��!A��A�\)A�A�A��A���A��HA��RA�|�A�"�A��A�{A��!A��PA�1'A��DA���A�p�A�bA���A�1'A���A�ĜA�r�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B B�BbB�B B B�B B�B BhB�B4B�B�B�B�B�BhB4B B�B�B�BbB.BbB�B�B�B�B�B(B�B"B�B
	BYB1B�B,�B=<BHKBg8B�lB��B҉B�B.B�_B��BȀB�BGB�BT�B��B��B�@B�CB��B��B��B�wB��B��B��B��B�B�vB��B�B��B�B�Bu�B��B}VBkBc�BV9BUgBR�B)�BVB
	B�B�B�B��B�oB֡BǮB��B��B��B��Bu�BjBU2B>wB1�BPB
�B
�^B
��B
�$B
�B
jB
N�B
GzB
4�B
�B	�>B	�B	��B	�zB	�=B	�-B	�B	yrB	i�B	X�B	F�B	5�B	'�B	�B	�B�,BߤBܒB�BуB�3B�wB��B�^B�zB��B�?B�hB��B��B�B�eB�B�B�B�@B��B�_B��B�hB�4B�\B�nB��B��B�wB��B�3B�B��B��B�RB�BǮB�BB�yBߤB��B�|B��B�,B�B�B�%B	�B	�B	xB	IB	!�B	%zB	,qB	0!B	5�B	/OB	@�B	R B	T,B	T�B	W�B	W�B	[�B	^jB	c�B	f2B	i�B	jB	kB	gmB	jB	p;B	qAB	r�B	v`B	v�B	�GB	��B	��B	��B	�B	��B	}�B	�B	�B	�B	��B	��B	��B	�fB	��B	�\B	��B	�4B	�hB	��B	�_B	�kB	�qB	�xB	��B	�xB	��B	��B	��B	��B	�^B	��B	�^B	�0B	�^B	��B	�BB	�wB	�OB	��B	�zB	�#B	��B	�HB	ѷB	�&B	�TB	�TB	�NB	��B	��B	�B	�dB	��B	�^B	̘B	�jB	͟B	̘B	�#B	�B	��B	�B	�XB	� B	�sB	֡B	�B	�B	�#B	�QB	��B	�B	�jB	ޞB	�5B	�B	��B	�B	��B	��B	ݘB	�dB	��B	��B	�yB	�EB	�gB	��B	��B	��B	уB	��B	�B	��B	��B	�B	�}B	�dB	��B	�dB	�<B	ϫB	�HB	�HB	�BB	бB	ѷB	�B	бB	�NB	бB	�B	��B	�B	уB	��B	՛B	�B	چB	��B	��B	�)B	��B	��B	�
B	��B	�
B	�mB	�#B	��B	ٴB	�KB	�yB	ٴB	ٴB	ںB	�#B	��B	ںB	�#B	�WB	�#B	�#B	�WB	�#B	��B	چB	��B	�B	�WB	��B	�B	�B	چB	ܒB	��B	��B	�dB	�/B	ޞB	��B	�B	�B	��B	��B	��B	�B	�KB	�QB	�"B	�"B	�yB	��B	��B	�B	�"B	�B	�/B	�B	�B	��B	�"B	�WB	�"B	��B	�B	�oB	�B	�B	��B	�fB	��B	�>B	�B	�xB	�JB	��B	�PB	�B	��B	��B	��B	��B
 iB
 �B
�B
uB
uB
AB
uB
�B
�B
MB
MB
�B
�B
SB
%B
YB
+B
+B
�B

	B

=B
�B
JB
B
�B
B
VB
�B
�B
�B
�B
�B
�B
\B
bB
bB
 B
@B
�B
�B
YB
1B
�B
�B
�B
B
CB
�B
�B
xB
�B
	B
7B
�B
	B
qB
�B
7B
kB
kB
IB
�B
VB
�B
�B
 'B
B
IB
B
�B
�B
 'B
 �B
!�B
 �B
 �B
!�B
 �B
�B
 \B
 'B
 'B
 'B
 �B
 �B
!bB
 �B
 �B
 'B
 �B
 \B
 \B
 �B
!bB
 �B
!-B
!�B
!�B
!�B
!�B
!�B
"4B
"�B
"�B
#:B
$@B
$tB
$@B
%B
&�B
'�B
(�B
(�B
(XB
(�B
*0B
*�B
*�B
*�B
+B
+�B
,B
,=B
,qB
,�B
.B
-�B
.IB
.}B
/OB
/�B
/�B
/�B
/�B
/�B
0UB
0�B
1'B
0�B
1�B
1�B
1�B
1[B
1�B
1[B
1[B
1�B
2-B
1�B
2�B
33B
3�B
4B
49B
4nB
5�B
5�B
5�B
5�B
6B
6zB
7B
7LB
7�B
7�B
7�B
7LB
7�B
7�B
7�B
7LB
6�B
7LB
7�B
7�B
7�B
8B
8RB
8RB
8�B
8�B
9$B
9�B
9�B
:�B
;dB
;�B
;�B
<jB
<6B
<6B
<jB
<�B
=�B
=B
=qB
=<B
=qB
=�B
=<B
=�B
>�B
>�B
?HB
?}B
@OB
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B'B
B'B
B'B
C-B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
FB
E�B
FB
F�B
F�B
GEB
GzB
HB
HKB
HKB
H�B
H�B
IB
H�B
H�B
IRB
I�B
J#B
J�B
K^B
K^B
K�B
LdB
LdB
K�B
K�B
L�B
M6B
M6B
MjB
NpB
M�B
NB
N�B
N�B
N�B
OB
N<B
N�B
N�B
OBB
OB
OvB
OvB
PB
Q�B
P�B
QNB
QNB
Q�B
R B
RTB
R B
R�B
Q�B
R�B
R B
R B
T�B
S[B
S�B
S[B
R�B
S�B
T,B
S�B
TaB
T�B
T�B
T�B
T�B
T�B
T�B
UgB
T�B
U�B
U2B
U�B
VmB
W
B
VmB
YB
W�B
XB
XEB
X�B
X�B
XyB
YB
XyB
X�B
Y�B
Y�B
ZQB
ZQB
Y�B
Z�B
ZQB
[#B
\]B
[�B
\�B
\)B
\)B
]/B
]�B
\�B
^5B
^B
]dB
^B
]�B
^5B
^B
^5B
_B
^�B
_pB
`vB
_�B
`BB
`�B
`�B
`�B
aB
aHB
aHB
bNB
b�B
b�B
c B
cTB
cTB
c�B
c�B
d&B
d&B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
gB
gB
f�B
gB
gB
gB
g�B
g�B
h�B
h>B
h�B
hsB
h�B
i�B
j�B
j�B
j�B
kB
j�B
k�B
k�B
l"B
lWB
k�B
l�B
l�B
m]B
l�B
m)B
m)B
m]B
m�B
m�B
m�B
o B
o5B
o B
n�B
n�B
o5B
o�B
o�B
o�B
o�B
o�B
o�B
oiB
oiB
o�B
pB
poB
qB
p�B
qAB
qAB
q�B
q�B
qvB
q�B
q�B
q�B
rGB
r|B
s�B
s�B
s�B
s�B
tTB
tB
tB
tB
tB
t�B
t�B
t�B
t�B
u%B
u%B
u�B
u%B
u�B
u%B
uZB
v+B
u�B
u�B
v�B
v�B
v�B
v�B
wfB
w2B
w�B
v�B
w�B
x8B
x�B
x�B
x8B
x8B
x�B
y	B
y�B
y>B
yrB
y�B
yrB
y�B
z�B
zxB
z�B
{B
z�B
z�B
zDB
z�B
{B
z�B
z�B
z�B
{B
{�B
|PB
|PB
|PB
|PB
|PB
}VB
}"B
|�B
}�B
~(B
~]B
~]B
~]B
~�B
~�B
.B
cB
~�B
~�B
�B
�B
.B
cB
�4B
�iB
� B
��B
�B
�B
�;B
��B
��B
�iB
�B
�B
��B
�B
��B
�oB
�;B
��B
��B
�B
��B
��B
�uB
�AB
�B
�AB
�uB
��B
�B
�GB
��B
��B
�MB
�B
��B
��B
��B
��B
��B
��B
�MB
�MB
��B
�B
��B
��B
�SB
��B
��B
�%B
��B
��B
��B
��B
��B
��B
�fB
��B
��B
��B
��B
��B
�1B
�fB
�B
�lB
�7B
�lB
��B
��B
�lB
�7B
�lB
�7B
��B
�	B
�rB4B�B:B�B�B�B�B�B\B�B�BoB�B:B�BhB4B�BBB�B�BhB�B4B4B�B�B�BbB�B�B�B�BbB\B.B�B�B.B(B�B.B�BuBbB�BhB�B:B�B4B B.B BB B4B.B�BhB.BB�B(B�BbB�BB(B+B:B:BB�BB4B�B�B�B�B�B�BoB:BBoB:B�B�BoB�B:BoB�BBB�B�B�B�B�B�B4B�B B�BbB�B�B�B.B.B�B�B�B�B�B�B�B�B�B�B(B�B�B�B�B\B�B�BbB�BbBbB.B�BbB�B�B.B.B4B�B�B�BbB�B�B�B�B�B.B�B(B�B�B\B�B(B\B\B�B\B(B�B�B\B(B(B(B\B(B(B(B\B\B�B(B\B\B�B�B�B�B�B.B�B�B BbB�B�BbBbB�B(B(BVB�BVB�B�B�BVB�B�B(B(B�B�B�B�B�B\B�B�B�BVBVB�B�BPB�B�BBB�B�B�B"B�BJBB�B�BB�BB�BDB_B�B_B�B�B+B_B�B�B	7B�B$�B�BA B�BxB�BBB �B$@B%B'RB($B*�B*�B,=B3hB3hB7�B=qB<�B<jB>B>�B?�B>�BB[BCaBGEBEBI�BL0BVBXEB_;Bd�Bi�Bm)Bl�Bl�Bl"Bl"BzB~�BcB�\B��B�@B��B��B�?BĜB�B�9B��B�RB�BB�#B�HB՛B�pB�<B�3B��B�*B�<B�XB�~B��B��B��B��B��B|�B}"B~]B}�BzDBx�BcB��B�;B�uB�B�VB��B��B�\B��B��B��B��B��B�kB�B��B��B�FB��B��B��B�%B��B�B��B�B�B�B��B��BuBB�B�BSB�BoBBAB�B�B�B�B�B��B6zBGBlWB}VB}�B�B��B��B�SB��B��B��B��B��B��B�B��B�DB�DB��B�(B�B��B��B�4B��B��B�qB��B��B�VB�7B�qB��B��B��B�FB��B��B��B�B��B��B�B�=B��B��B��B�_B��B��B�wB�IB�wB�B�OB�EB�B��B��BخB��B��B��B��B��B�wB� B�B��B�pBҽB�QB�BÖB�BʌB�jB��B�EB��B��B�]B�HB�HB�|B��B�
B�B�)B�5B�;B�AB�cB�B�B��BxB��B��B��B�+B�	B�B�)B�B�B�0B�B�HB�!B�9B�4B�bB�@B��B��B��B��B�hB�B��B��B�B��BzxBzBs�BlWBp�Bs�B�4B�B�~B��B~�Bx�B�B��BsBx�BpBm)Bh
BffBe�Bc�Be�B`�BiBb�B� BP�BW
BO�BI�BXEBT,BYKBRTBS�BR�BU�B\�B_pBd�BU�B2�B*�B(XB)_B(�B,�B)*B$tB"hB�B�B�BDB
�B�B�B�B�BbB_BAB�B+BB��B
=B	�B�.BuB��B��B�oB�B��B�B�5B�)B��B�B�)B�cB�B�]B��B� B��BںB��B�BޞB�B�QB�2B��B˒B� B�9B��B��B��B��B��B�aB��B�{B��B��B�JB��B��B��B��B�uB��B�oB�4B�iB�B}VB{Bx8Bt�Bt�Bt�Bq�Bp�Bi�BpoBiB^5Bi�Bd�BVBM6BF?BGEBDgB>wB?�B:*B4�B9$B1'B:^B.�B$B�BBoB�B 4B
��B
��B
�;B
�[B
B
��B
�9B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                          B�B	B�BnB�B	B	B�B	B�B	B	tB�B	@B�B	�B	�B	�B	�B	tB	@B	B�B�B�BnB:BnBBB�B�B�B4B�B.B�BB
�eB=B�B$�B5HB@WB_DB�xB��BʕB�$Bw:BkB�B��B�%B�SB��BL�B}�B�	B�LB�OB��B��B��B��B��B�B��B�BާB�B��B�B��B�'B�BnBx�BubBc(B[�BNEBMsBJ�B"BbBB��B�B�+B��B�{BέB��B��B��B~�Bx�Bm�Bb�BM>B6�B*B\B
�B
�jB
��B
�0B
{B
b�B
F�B
?�B
,�B
B	�JB	�B	��B	��B	�IB	�9B	�*B	q~B	a�B	P�B	>�B	-�B	�B	�B��B�8BװBԞB�BɏB�?B��B��B�jB��B��B�KB�tB��B��B� B�qB�$B�$B�B�LB�B�kB��B�tB�@B�hB�zB��B��B��B��B�?B�B��B��B�^B�&B��B�NBЅBװB��BوB��B�8B�B�B�1B��B	B	�B	UB	B	�B	$}B	(-B	-�B	'[B	8�B	J,B	L8B	L�B	O�B	O�B	S�B	VvB	[�B	^>B	a�B	b�B	c(B	_yB	b"B	hGB	iMB	j�B	nlB	o	B	{SB	~�B	��B	~�B	}+B	{�B	v B	w�B	{B	zB	|�B	y�B	|�B	�rB	��B	�hB	��B	�@B	�tB	��B	�kB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	�jB	��B	�jB	�<B	�jB	��B	�NB	��B	�[B	��B	��B	�/B	��B	�TB	��B	�2B	�`B	�`B	�ZB	��B	��B	�B	�pB	�B	�jB	ĤB	�vB	ūB	ĤB	�/B	�)B	��B	�#B	�dB	�,B	�B	έB	�B	�B	�/B	�]B	��B	�B	�vB	֪B	�AB	�B	��B	�B	��B	��B	դB	�pB	��B	�B	ЅB	�QB	�sB	�B	��B	�B	ɏB	��B	�B	��B	��B	�&B	ȉB	�pB	��B	�pB	�HB	ǷB	�TB	�TB	�NB	ȽB	��B	� B	ȽB	�ZB	ȽB	�B	��B	�&B	ɏB	��B	ͧB	�B	ҒB	�B	�B	�5B	��B	�B	�B	��B	�B	�yB	�/B	��B	��B	�WB	ЅB	��B	��B	��B	�/B	��B	��B	�/B	�cB	�/B	�/B	�cB	�/B	�B	ҒB	��B	�)B	�cB	��B	�)B	�)B	ҒB	ԞB	�B	��B	�pB	�;B	֪B	��B	ާB	ݡB	�
B	��B	��B	�B	�WB	�]B	�.B	�.B	�B	��B	��B	�B	�.B	��B	�;B	�B	�B	��B	�.B	�cB	�.B	��B	�B	�{B	�B	�B	�B	�rB	��B	�JB	�B	�B	�VB	�B	�\B	�(B	��B	� B	��B	��B	�uB	��B	��B	��B	��B	�MB	��B	��B	��B	�YB	�YB	��B	��B	�_B	�1B	�eB	�7B	�7B
 	B
B
IB
�B
VB
'B
�B
'B
bB
�B
�B
�B
�B
�B
�B
hB
nB
nB
	B
LB
�B
�B
eB
=B
B
�B
�B
B
OB
�B
�B
�B
�B
B
CB
�B
B
}B
�B
CB
wB
wB
UB
�B
bB
�B
�B
3B
'B
UB
'B
�B
�B
3B
�B
�B
�B
B
�B
�B
�B
hB
3B
3B
3B
�B
�B
nB
�B
�B
3B
�B
hB
hB
�B
nB
B
9B
�B
B
B
B
B
@B
�B
�B
FB
LB
�B
LB
B
�B
�B
 �B
 �B
 dB
 �B
"<B
"�B
"�B
"�B
#B
#�B
$B
$IB
$}B
$�B
& B
%�B
&UB
&�B
'[B
'�B
'�B
'�B
'�B
'�B
(aB
(�B
)3B
(�B
)�B
)�B
)�B
)gB
)�B
)gB
)gB
)�B
*9B
*B
*�B
+?B
+�B
,B
,EB
,zB
-�B
-�B
-�B
-�B
.B
.�B
/#B
/XB
/�B
/�B
/�B
/XB
/�B
/�B
/�B
/XB
.�B
/XB
/�B
/�B
/�B
0)B
0^B
0^B
0�B
0�B
10B
1�B
1�B
3B
3pB
3�B
3�B
4vB
4BB
4BB
4vB
4�B
5�B
5B
5}B
5HB
5}B
5�B
5HB
5�B
6�B
6�B
7TB
7�B
8[B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
:3B
:3B
:3B
;9B
;B
;B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>B
=�B
>B
>�B
>�B
?QB
?�B
@#B
@WB
@WB
@�B
@�B
A)B
@�B
@�B
A^B
A�B
B/B
B�B
CjB
CjB
DB
DpB
DpB
DB
DB
D�B
EBB
EBB
EvB
F|B
E�B
FB
F�B
F�B
F�B
GB
FHB
F�B
F�B
GNB
GB
G�B
G�B
H B
I�B
H�B
IZB
IZB
I�B
J,B
J`B
J,B
J�B
I�B
J�B
J,B
J,B
L�B
KgB
K�B
KgB
J�B
K�B
L8B
K�B
LmB
L�B
L�B
L�B
M
B
L�B
L�B
MsB
M
B
M�B
M>B
M�B
NyB
OB
NyB
Q#B
O�B
PB
PQB
P�B
P�B
P�B
Q#B
P�B
P�B
Q�B
Q�B
R]B
R]B
Q�B
R�B
R]B
S/B
TiB
S�B
T�B
T5B
T5B
U;B
U�B
T�B
VAB
VB
UpB
VB
U�B
VAB
VB
VAB
WB
V�B
W|B
X�B
W�B
XNB
X�B
X�B
X�B
YB
YTB
YTB
ZZB
Z�B
Z�B
[,B
[`B
[`B
[�B
[�B
\2B
\2B
\�B
\�B
]�B
^
B
^
B
^
B
^
B
_B
_B
^�B
_B
_B
_B
_�B
_�B
`�B
`JB
`�B
`B
`�B
a�B
b�B
b�B
b�B
c(B
b�B
c�B
c�B
d.B
dcB
c�B
d�B
e B
eiB
e B
e5B
e5B
eiB
e�B
e�B
e�B
gB
gAB
gB
f�B
f�B
gAB
g�B
g�B
g�B
g�B
g�B
g�B
guB
guB
g�B
hB
h{B
iB
h�B
iMB
iMB
i�B
i�B
i�B
i�B
i�B
i�B
jSB
j�B
k�B
k�B
k�B
k�B
l`B
l+B
l+B
l+B
l+B
l�B
l�B
l�B
l�B
m1B
m1B
m�B
m1B
m�B
m1B
mfB
n7B
m�B
nB
o	B
n�B
o	B
o	B
orB
o>B
o�B
o	B
o�B
pDB
p�B
p�B
pDB
pDB
p�B
qB
q�B
qJB
q~B
q�B
q~B
q�B
r�B
r�B
r�B
s"B
r�B
r�B
rPB
r�B
s"B
r�B
r�B
r�B
s"B
s�B
t\B
t\B
t\B
t\B
t\B
ubB
u.B
t�B
u�B
v4B
viB
viB
viB
v�B
wB
w:B
woB
wB
v�B
w�B
w�B
w:B
woB
x@B
xuB
xB
x�B
yB
yB
yGB
x�B
x�B
xuB
yB
yB
x�B
yB
y�B
y{B
yGB
y�B
y�B
zB
z�B
z�B
z�B
zMB
zB
zMB
z�B
z�B
{B
{SB
{�B
{�B
|YB
|%B
{�B
{�B
|�B
|�B
|�B
|�B
|YB
|YB
|�B
}+B
|�B
|�B
}_B
}�B
}�B
~1B
~�B
~�B
B
B
�B
�	B
�rB
��B
�	B
�	B
��B
��B
�=B
�rB
�B
�xB
�CB
�xB
��B
��B
�xB
�CB
�xB
�CB
��B
�B
�~B	@B�B
FB�B�B
�B�BBhB�B�B
{B	�B
FBB	tB	@B	�B
B
B
�B�B	tB	�B	@B	@B�B�BBnB�BBB�BnBhB:BBB:B4B�B:B�B�BnB�B	tB�B
FB�B	@B	B:B	BB	B	@B:B	�B	tB:B
B�B4BBnB	�B
B4B7B
FB
FBB�B
B	@B
�B
�B
�B
�B
�B
�B
{B
FB
B
{B
FB
�B
�B
{B
�B
FB
{B	�B
B
B
�B	�B	�B	�B	�B	�B	@B�B	B�BnB�B�B�B:B:BB�B�B�B�B�B�B�B�BB4B�B�B�B�BhB�B�BnBBnBnB:B�BnBBB:B:B	@B�B�B�BnB�B�B�BB�B:BB4BBBhB�B4BhBhB�BhB4B�B�BhB4B4B4BhB4B4B4BhBhB�B4BhBhB�BB�BBB:B�B�B	BnB�B�BnBnBB4B4BbB�BbB�B�B�BbB�B�B4B4BBB�B�BBhB�B�B�BbBbB�B�B\B�B�B'B'B�B�B�B.B�BVB!B�B�B!B�BB �BPB
�kB �B
�kB
��B
��B
�7B
�kB
��B
��BCB�B�B�B9,B�B�B�B!B!B�BLBB^B 0B"�B"�B$IB+tB+tB/�B5}B4�B4vB6B6�B7�B6�B:gB;mB?QB=BA�BD<BNBPQBWGB\�Ba�Be5Be Be Bd.Bd.BrBv�BwoB�hB��B�LB��B��B�KB��B�B�EB��B�^B�NB�/B�TBͧB�|B�HB�?B��B�6B�HB�dB��B��B��B�B��B}�Bt�Bu.BviBu�BrPBp�BwoB{�ByGBz�B�B�bB��B��B�hB��B�B��B��B��B�wB�B��B��B�RB��B��B��B�1B��B�B��B�B�+B�+B��B��B��B�B��B��B�_B��B�{B�B�MB��B��B��B��B��B��B.�B?BdcBubBu�BzB|�B}�B}_B~�B~�B~�B|�B{�B{�B|%B|�B�PB�PB��B�4B�B��B��B�@B��B�B�}B��B��B�bB�CB�}B��B��B�B�RB��B��B��B�B��B��B�B�IB��B��B��B�kB��B��B��B�UB��B�B�[B�QB�)B��B��BкB��B��B��B��B��B��B�,B�B�B�|B��B�]B�)B��B�)BB�vB��B�QB��B��B�iB�TB�TBوB��B�B�B�5B�AB�GB�MB�oB�B�(B��B�B�B��B�B�7B�B�+B�5B�B�B�<B�#B�TB�-B�EB�@B�nB�LB��B��B��B��B�tB�B��B��B�BBr�BrBk�BdcBh�Bk�Bx@ByB��B�Bv�Bp�Bw�By�Bk%Bp�BhBe5B`B^rB]�B[�B]�BX�BaBZ�BxBH�BOBG�BA�BPQBL8BQWBJ`BK�BJ�BM�BT�BW|B\�BM�B+B"�B dB!kB �B$�B!6B�BtB�B�B�BPB�B�B��B��B��BnB�kB�MB��B�7B�%B��BIB�B�:B��B��B�B�{B�B�B�B�AB�5B��B�B�5B�oB�B�iB�B�B��B��B��B�$B֪B�&B�]B�>B��BÞB�,B�EB��B��B��B�B��B�mB��B��B��B��B�VB��B�	By�B��Bz�Bx�By{Bx@BxuB{BubBs"BpDBl�Bl�Bl�Bi�Bh�Ba�Bh{BaBVABa�B\�BNBEBB>KB?QB<sB6�B7�B26B,�B10B)3B2jB&�BB�BB
{B	�B
�@B
��B
��B
�GB
�gB
��B
��B
�EB
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230720040043                            20230720040043AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072004004320230720040043  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072004004320230720040043QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072004004320230720040043QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               