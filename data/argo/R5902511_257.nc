CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:59Z creation      
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
resolution        =���   axis      Z        (  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  b�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (     PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 5(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ( <�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ( c�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230721224959  20230721224959  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�$O!/o�@�$O!/o�11  @�$OI���@�$OI���@3��)I�@3��)I��d�c^t)��d�c^t)�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�  @�\@@  @�  @�G�@\@�G�A ��A  A ��A,(�A?\)A`  A�  A�  A�  A�  A��AϮA�  A�\)A��B(�B�
B�
B (�B((�B0(�B8��B@z�BH(�BP(�BX  B_�
Bg�
Bo�
Bx(�B�(�B�  B��
B��B�{B�(�B�  B�{B�=qB�{B�{B�(�B�=qB�=qB�{B�  B�  B�  B�{B�  B�  B��
B��B�{B�{B�  B�(�B�(�B�  B�  B�  B�  B��C��C��C  C  C	�C�C  C
=C
=C
=C
=C  C��C��C  C   C!��C#��C%�C'��C*  C,
=C.
=C0  C2  C4  C6  C8
=C:
=C<  C>
=C@  CA��CD
=CF  CH
=CJ{CK��CN{CP  CR  CS��CU��CX  CZ  C\  C]��C_��Ca��Cd  Cf  Ch  Cj  Cl  Cn
=Cp
=Cr
=Cs��Cv  Cx
=Cz
=C|
=C}��C�C���C�  C���C�C�C�  C�  C�  C���C�  C���C���C�  C�  C�  C�  C���C�  C�C�  C�C�  C���C�C�  C�C�C�C�C���C�  C���C���C���C�  C���C���C�  C�  C�  C�  C�C���C�  C�  C���C�  C���C�  C�
=C�  C���C���C�  C�  C�C�
=C�C�C���C�  C�C�  C�  C���C�C�C���C�  C���C�  C�C�
=C�  C���C�C�C�  C���C�  C�  C�  C�  C���C���C���C���C�C�C�  C�  C���C�  C�C�  C���C�C�C�C���C���C�  C�  C���C�C�C���C���C�  C�C�  C�  C���C���C���C���C�  C�  C�C�C�C�
=C�
=C�  C���C�  C�D   D � D �qD}qDD�D�D�D�D� D��D� D�D��D�qD}qD�qD}qD	  D	� D
  D
� D  D��D  D� D  D� D  D� D  D��D�D��D  D}qD�qD��D  D}qD�D� D�qD� D  D}qD�qD}qD  D}qD  D}qD  D�D  D}qD�qD}qD  D� D�D� D  D�D   D � D!  D!�D"�D"��D"�qD#z�D$  D$��D%D%��D&  D&z�D'  D'�D(�D(� D)  D)}qD*�D*��D+�D+� D+�qD,�D-D-}qD.�D.�D/D/��D/��D0z�D0�qD1� D2�D2}qD2�qD3}qD4  D4� D4��D5z�D6  D6� D7�D7}qD8  D8��D9  D9� D:  D:�D;D;}qD;�qD<�D=  D=z�D=�qD>��D?�D?� D@  D@� D@�qDA}qDB�DB� DC�DC��DD  DD� DE�DEz�DE�qDF��DG  DG� DH  DH�DI  DI� DJ  DJ}qDJ��DK}qDL  DLz�DM  DM}qDM�qDN��DO�DO}qDP�DP�DQ  DQ}qDR�DR�DSDS}qDS�qDT� DT��DU}qDU��DV}qDW�DW�DX�DX�DYDY��DZ  DZ}qD[  D[��D[�qD\z�D\�qD]� D]�qD^xRD^��D_}qD`  D`� Da�Da��Db  Db� Dc�Dc�Dc�qDdz�Dd�qDez�De�RDfz�Dg  Dg��DhDh�Di  Di� Dj  Dj��Dk  Dk}qDk�qDl� Dm  Dm��Dn�Dn��Do  Doz�Do��Dp}qDq  Dq��Dr  Dr}qDr�qDs� Dt  Dt� Du�Du��Du�qDv}qDw�Dw��DxDx�DyDy�Dz  Dz}qDz�qD{� D{��D|}qD|�qD}}qD}�qD~}qD  D�D��D�B�D���D�� D�  D�>�D�~�D���D��qD�@ D��HD�� D�HD�AHD�� D�� D�  D�>�D�~�D���D�  D�@ D��HD�� D�  D�@ D��HD��HD�  D�B�D�� D���D�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D��HD�HD�@ D�� D��HD�  D�@ D�� D��HD�  D�AHD��HD�� D�  D�@ D�� D���D�  D�AHD�� D��HD��D�@ D�~�D���D�  D�@ D�}qD���D���D�@ D��HD�� D���D�@ D�� D���D���D�>�D�~�D��HD��D�@ D�� D�� D���D�@ D�� D���D�  D�@ D��HD�� D�  D�@ D��HD�� D�  D�>�D�~�D��qD��qD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�~�D���D�  D�>�D�~�D��qD�  D�@ D�� D�� D���D�@ D��HD�� D���D�@ D�� D���D���D�@ D��HD�� D�HD�B�D��HD��HD�HD�@ D�}qD���D�  D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D�~�D���D���D�=qD�}qD���D�HD�@ D��HD�� D��qD�>�D�� D��HD�HD�AHD��HD��HD���D�=qD�� D�� D�HD�AHD��HD�D�HD�@ D�� D��HD���D�=qD�~�D�� D�HD�@ D�� D��HD�  D�@ D��HD���D���D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D�� D��qD�  D�@ D��HD��HD�  D�@ D�~�D��HD�HD�@ D��HD�� D���D�>�D��HD�� D�  D�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D��HD�  D�@ D���D�D��D�@ D��HD��HD�  D�AHD�� D�� D�  D�>�D�}qD�� D�  D�>�D��HD�� D�  D�AHD��HD�� D�  D�@ D D�� D�  D�>�DÀ D��HD�  D�@ DĀ Dľ�D�  D�@ Dŀ D�� D�  D�@ DƁHD�� D�  D�AHDǁHD�� D�HD�B�DȀ DȾ�D�HD�B�Dɂ�D��HD�HD�B�DʁHDʽqD��qD�=qD�}qD˽qD���D�@ D̀ D�� D���D�@ D͂�D��HD���D�AHD΀ D�� D�  D�>�Dπ D�D�HD�@ DЁHDо�D��qD�@ DсHD�D�HD�AHDҀ DҾ�D���D�>�DӀ D�� D���D�>�DԁHD��HD���D�>�DՀ D��HD�  D�>�D�~�D�� D��D�AHD׀ D�� D�  D�@ D؀ D�� D�HD�B�Dك�D�D�  D�@ D�~�Dڼ)D�  D�AHDۀ D�D�  D�>�D܀ D�� D�  D�@ D�~�DݽqD��qD�>�DށHD�� D���D�@ D߁HD߾�D���D�@ D�� D��HD�HD�AHD�}qD�qD���D�@ D� D⾸D���D�>�D�HD��HD�  D�AHD䂏D�� D�  D�@ D�~�D�� D�HD�AHD悏D��HD�  D�AHD� D�� D��qD�=qD�}qD�qD���D�@ D�HD��HD���D�>�D�~�D�� D���D�@ D� D뾸D�  D�>�D�}qD쾸D�  D�@ D�~�D���D���D�AHDD��HD��qD�<)D�~�D�� D�  D�AHD��HD��HD�  D�>�D� D�D�  D�B�D�HD�qD���D�@ D�~�D�� D�HD�B�D�HD��HD�  D�>�D�}qD�� D�  D�AHD�� D���D�  D�B�D��HD���>���?�?W
=?�z�?�Q�?�ff@�@(�@+�@G�@\(�@k�@��
@�{@�@�  @��@�z�@�G�@���@�33@�G�@�@��@�p�Az�A��A\)A�AQ�A{A$z�A'�A-p�A333A6ffA<��AB�\AEAL(�AQ�AUA\��AaG�AfffAl(�AqG�Au�A|(�A�Q�A�=qA��A�\)A�G�A�z�A��RA���A��
A�ffA���A��
A�{A���A��
A�A���A��A�p�A���A��HA�{A���A��HA�A���A��HA�A���Aʏ\A�A�Q�Aҏ\A�{Aأ�Aڏ\A�{A��A�\A�ffA��A��HA�ffA���A��HA�ffA�G�A�33A�{B ��B��B33B��B��B33B��B	B
=B��B�B
=B��B�B33B��B=qB33B�BffB�BG�B�\B�B!p�B"�\B#�
B%��B&�RB(  B)B*�\B,Q�B-��B.�HB0(�B1B2�HB3�
B5��B6�RB7�B9G�B:�RB;�B<��B>=qB?\)B@Q�BA��BC
=BC�
BD��BF�\BH  BH��BJffBK�
BL��BN{BO�BP��BQ�BS\)BTQ�BU��BW33BXQ�BYp�B[
=B\(�B]p�B_33B`  Bap�Bc
=Bd  BeG�Bg
=Bh(�BiG�Bj�HBlQ�BmG�Bn�\Bp(�BqBr�RBt(�BuBw
=Bx  By��B{
=B|  B}G�B~�HB�
B���B�\)B��
B��\B�G�B��B�ffB�33B�B�Q�B�
=B�B�Q�B��HB��B�ffB��HB�p�B�=qB��HB�p�B�(�B��HB�\)B�(�B��HB�\)B��B��RB�\)B��
B��\B�\)B��
B�ffB�G�B��B�ffB��B��
B�z�B���B��B�z�B�
=B���B�Q�B��B���B�Q�B��B��B�=qB��B��
B�Q�B��B��B���B�33B��B��RB�G�B��
B��\B�\)B��
B�ffB�33B�B�(�B��HB���B�  B�ffB�33B��B��B�z�B��B�p�B�B�Q�B���B��HB�\)B�B�  B�=qB��RB�
=B�G�B��B�(�B�Q�B���B��B��B��B�{B���B��HB�33B���B�{B�z�B��RB��B��B�{B�Q�B���B�G�B���B��
B�z�B��RB�
=B���B��B�(�B¸RB��B�\)B��
B�Q�Bď\B�
=BŅB�B�(�Bƣ�B���B�G�B��
B�{B�ffB��HB�G�Bə�B��
B�ffB���B�
=B�p�B�  B�(�B�z�B�
=B�\)B͙�B�(�B�ffBΣ�B�33Bϙ�B�B�=qBиRB���B�33BѮB�{B�=qBҸRB��B�G�Bә�B�(�B�Q�BԸRB�33B�\)B�B�Q�B֏\B��HB�p�B�B�  B؏\B�
=B�G�Bٙ�B�(�Bڏ\B���B�G�B�B�{B�ffB��HB�\)Bݙ�B��Bޏ\B���B��Bߙ�B��B�=qB���B��B�p�B�B�=qB�RB��HB�33B�B�{B�Q�B��B�33B�p�B�B�Q�B��B���B�33B�B�(�B�z�B�RB��B�B��B�(�B�RB��B�\)B�B�(�B�\B���B�33B��B��B�Q�B���B�
=B�\)B��
B�Q�B�z�B��HB�p�B�B�  B�\B���B�33B�B�=qB�z�B���B�\)B�B�{B�z�B���B�\)B��B�{B���B�
=B�\)B���B�=qB��RB�
=B�\)B�B�=qB��RB���B�G�B�B�Q�B��RB���B�\)B��C (�C ffC z�C �C �C33CffC�\C�RC�C=qC\)C�CC  C=qCffC�\CC
=C=qC\)C��C�HC
=C=qCffC��C�HC(�CQ�Cz�C�C  C33CffC�CC  C=qCp�C�\C��C	{C	=qC	ffC	��C	�HC
{C
=qC
\)C
��C
�C{C=qCp�C�RC��C{C=qCz�C�RC��C{C=qCz�CC��C�CG�C�C��C  C(�CQ�C�C��C
=C�C\)C��C�
C  C�C\)C��C�
C��C�C\)C��C��C�C(�Cp�C��CC�C�Cp�C�C�
C��C33Cz�C�C��C  C33Cp�C��C�HC�CG�Cp�C��C�HC{C33CffC�C�HC
=C=qC\)C��C�HC
=C33CffC�\C��C{CQ�Cz�C��C��C  C=qC�C�RC�C{C=qCp�C��C�HC�C\)C��CC�HC{CQ�C�\CC   C �C Q�C z�C ��C �C!(�C!ffC!�\C!C!��C"�C"Q�C"�\C"C#
=C#G�C#�C#�RC#�C$�C$Q�C$�C$C%
=C%Q�C%�C%�C%�HC&�C&Q�C&��C&�
C'�C'ffC'��C'�
C({C(G�C(z�C(�RC)
=C)G�C)�\C)��C*
=C*=qC*z�C*�RC*��C+33C+z�C+�RC,
=C,G�C,z�C,�C,�HC-�C-ffC-�C-�C.33C.p�C.��C.�
C/{C/\)C/�C/�C0(�C0Q�C0��C0�HC1(�C1p�C1�C1�C2�C2\)C2��C2��C3{C3\)C3��C3�
C4�C4Q�C4�C4�RC5  C5G�C5�\C5�
C6�C6\)C6��C6��C7
=C7Q�C7��C7�HC8(�C8ffC8��C8�
C9�C9p�C9�RC9��C:(�C:\)C:��C:�C;=qC;z�C;�RC;�C<�C<\)C<��C<�C=33C=z�C=�C=��C>�C>\)C>��C>�
C?{C?\)C?��C?�HC@�C@ffC@��C@�HCA�CA\)CA��CA�
CB{CBQ�CB�CB��CC
=CC\)CC��CC�CD(�CDp�CD�CD��CE(�CEffCE�CE�CF33CFz�CF�CF��CG=qCGz�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                      ?�  @�\@@  @�  @�G�@\@�G�A ��A  A ��A,(�A?\)A`  A�  A�  A�  A�  A��AϮA�  A�\)A��B(�B�
B�
B (�B((�B0(�B8��B@z�BH(�BP(�BX  B_�
Bg�
Bo�
Bx(�B�(�B�  B��
B��B�{B�(�B�  B�{B�=qB�{B�{B�(�B�=qB�=qB�{B�  B�  B�  B�{B�  B�  B��
B��B�{B�{B�  B�(�B�(�B�  B�  B�  B�  B��C��C��C  C  C	�C�C  C
=C
=C
=C
=C  C��C��C  C   C!��C#��C%�C'��C*  C,
=C.
=C0  C2  C4  C6  C8
=C:
=C<  C>
=C@  CA��CD
=CF  CH
=CJ{CK��CN{CP  CR  CS��CU��CX  CZ  C\  C]��C_��Ca��Cd  Cf  Ch  Cj  Cl  Cn
=Cp
=Cr
=Cs��Cv  Cx
=Cz
=C|
=C}��C�C���C�  C���C�C�C�  C�  C�  C���C�  C���C���C�  C�  C�  C�  C���C�  C�C�  C�C�  C���C�C�  C�C�C�C�C���C�  C���C���C���C�  C���C���C�  C�  C�  C�  C�C���C�  C�  C���C�  C���C�  C�
=C�  C���C���C�  C�  C�C�
=C�C�C���C�  C�C�  C�  C���C�C�C���C�  C���C�  C�C�
=C�  C���C�C�C�  C���C�  C�  C�  C�  C���C���C���C���C�C�C�  C�  C���C�  C�C�  C���C�C�C�C���C���C�  C�  C���C�C�C���C���C�  C�C�  C�  C���C���C���C���C�  C�  C�C�C�C�
=C�
=C�  C���C�  C�D   D � D �qD}qDD�D�D�D�D� D��D� D�D��D�qD}qD�qD}qD	  D	� D
  D
� D  D��D  D� D  D� D  D� D  D��D�D��D  D}qD�qD��D  D}qD�D� D�qD� D  D}qD�qD}qD  D}qD  D}qD  D�D  D}qD�qD}qD  D� D�D� D  D�D   D � D!  D!�D"�D"��D"�qD#z�D$  D$��D%D%��D&  D&z�D'  D'�D(�D(� D)  D)}qD*�D*��D+�D+� D+�qD,�D-D-}qD.�D.�D/D/��D/��D0z�D0�qD1� D2�D2}qD2�qD3}qD4  D4� D4��D5z�D6  D6� D7�D7}qD8  D8��D9  D9� D:  D:�D;D;}qD;�qD<�D=  D=z�D=�qD>��D?�D?� D@  D@� D@�qDA}qDB�DB� DC�DC��DD  DD� DE�DEz�DE�qDF��DG  DG� DH  DH�DI  DI� DJ  DJ}qDJ��DK}qDL  DLz�DM  DM}qDM�qDN��DO�DO}qDP�DP�DQ  DQ}qDR�DR�DSDS}qDS�qDT� DT��DU}qDU��DV}qDW�DW�DX�DX�DYDY��DZ  DZ}qD[  D[��D[�qD\z�D\�qD]� D]�qD^xRD^��D_}qD`  D`� Da�Da��Db  Db� Dc�Dc�Dc�qDdz�Dd�qDez�De�RDfz�Dg  Dg��DhDh�Di  Di� Dj  Dj��Dk  Dk}qDk�qDl� Dm  Dm��Dn�Dn��Do  Doz�Do��Dp}qDq  Dq��Dr  Dr}qDr�qDs� Dt  Dt� Du�Du��Du�qDv}qDw�Dw��DxDx�DyDy�Dz  Dz}qDz�qD{� D{��D|}qD|�qD}}qD}�qD~}qD  D�D��D�B�D���D�� D�  D�>�D�~�D���D��qD�@ D��HD�� D�HD�AHD�� D�� D�  D�>�D�~�D���D�  D�@ D��HD�� D�  D�@ D��HD��HD�  D�B�D�� D���D�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D��HD�HD�@ D�� D��HD�  D�@ D�� D��HD�  D�AHD��HD�� D�  D�@ D�� D���D�  D�AHD�� D��HD��D�@ D�~�D���D�  D�@ D�}qD���D���D�@ D��HD�� D���D�@ D�� D���D���D�>�D�~�D��HD��D�@ D�� D�� D���D�@ D�� D���D�  D�@ D��HD�� D�  D�@ D��HD�� D�  D�>�D�~�D��qD��qD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�~�D���D�  D�>�D�~�D��qD�  D�@ D�� D�� D���D�@ D��HD�� D���D�@ D�� D���D���D�@ D��HD�� D�HD�B�D��HD��HD�HD�@ D�}qD���D�  D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D�~�D���D���D�=qD�}qD���D�HD�@ D��HD�� D��qD�>�D�� D��HD�HD�AHD��HD��HD���D�=qD�� D�� D�HD�AHD��HD�D�HD�@ D�� D��HD���D�=qD�~�D�� D�HD�@ D�� D��HD�  D�@ D��HD���D���D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D�� D��qD�  D�@ D��HD��HD�  D�@ D�~�D��HD�HD�@ D��HD�� D���D�>�D��HD�� D�  D�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D��HD�  D�@ D���D�D��D�@ D��HD��HD�  D�AHD�� D�� D�  D�>�D�}qD�� D�  D�>�D��HD�� D�  D�AHD��HD�� D�  D�@ D D�� D�  D�>�DÀ D��HD�  D�@ DĀ Dľ�D�  D�@ Dŀ D�� D�  D�@ DƁHD�� D�  D�AHDǁHD�� D�HD�B�DȀ DȾ�D�HD�B�Dɂ�D��HD�HD�B�DʁHDʽqD��qD�=qD�}qD˽qD���D�@ D̀ D�� D���D�@ D͂�D��HD���D�AHD΀ D�� D�  D�>�Dπ D�D�HD�@ DЁHDо�D��qD�@ DсHD�D�HD�AHDҀ DҾ�D���D�>�DӀ D�� D���D�>�DԁHD��HD���D�>�DՀ D��HD�  D�>�D�~�D�� D��D�AHD׀ D�� D�  D�@ D؀ D�� D�HD�B�Dك�D�D�  D�@ D�~�Dڼ)D�  D�AHDۀ D�D�  D�>�D܀ D�� D�  D�@ D�~�DݽqD��qD�>�DށHD�� D���D�@ D߁HD߾�D���D�@ D�� D��HD�HD�AHD�}qD�qD���D�@ D� D⾸D���D�>�D�HD��HD�  D�AHD䂏D�� D�  D�@ D�~�D�� D�HD�AHD悏D��HD�  D�AHD� D�� D��qD�=qD�}qD�qD���D�@ D�HD��HD���D�>�D�~�D�� D���D�@ D� D뾸D�  D�>�D�}qD쾸D�  D�@ D�~�D���D���D�AHDD��HD��qD�<)D�~�D�� D�  D�AHD��HD��HD�  D�>�D� D�D�  D�B�D�HD�qD���D�@ D�~�D�� D�HD�B�D�HD��HD�  D�>�D�}qD�� D�  D�AHD�� D���D�  D�B�D��HD���>���?�?W
=?�z�?�Q�?�ff@�@(�@+�@G�@\(�@k�@��
@�{@�@�  @��@�z�@�G�@���@�33@�G�@�@��@�p�Az�A��A\)A�AQ�A{A$z�A'�A-p�A333A6ffA<��AB�\AEAL(�AQ�AUA\��AaG�AfffAl(�AqG�Au�A|(�A�Q�A�=qA��A�\)A�G�A�z�A��RA���A��
A�ffA���A��
A�{A���A��
A�A���A��A�p�A���A��HA�{A���A��HA�A���A��HA�A���Aʏ\A�A�Q�Aҏ\A�{Aأ�Aڏ\A�{A��A�\A�ffA��A��HA�ffA���A��HA�ffA�G�A�33A�{B ��B��B33B��B��B33B��B	B
=B��B�B
=B��B�B33B��B=qB33B�BffB�BG�B�\B�B!p�B"�\B#�
B%��B&�RB(  B)B*�\B,Q�B-��B.�HB0(�B1B2�HB3�
B5��B6�RB7�B9G�B:�RB;�B<��B>=qB?\)B@Q�BA��BC
=BC�
BD��BF�\BH  BH��BJffBK�
BL��BN{BO�BP��BQ�BS\)BTQ�BU��BW33BXQ�BYp�B[
=B\(�B]p�B_33B`  Bap�Bc
=Bd  BeG�Bg
=Bh(�BiG�Bj�HBlQ�BmG�Bn�\Bp(�BqBr�RBt(�BuBw
=Bx  By��B{
=B|  B}G�B~�HB�
B���B�\)B��
B��\B�G�B��B�ffB�33B�B�Q�B�
=B�B�Q�B��HB��B�ffB��HB�p�B�=qB��HB�p�B�(�B��HB�\)B�(�B��HB�\)B��B��RB�\)B��
B��\B�\)B��
B�ffB�G�B��B�ffB��B��
B�z�B���B��B�z�B�
=B���B�Q�B��B���B�Q�B��B��B�=qB��B��
B�Q�B��B��B���B�33B��B��RB�G�B��
B��\B�\)B��
B�ffB�33B�B�(�B��HB���B�  B�ffB�33B��B��B�z�B��B�p�B�B�Q�B���B��HB�\)B�B�  B�=qB��RB�
=B�G�B��B�(�B�Q�B���B��B��B��B�{B���B��HB�33B���B�{B�z�B��RB��B��B�{B�Q�B���B�G�B���B��
B�z�B��RB�
=B���B��B�(�B¸RB��B�\)B��
B�Q�Bď\B�
=BŅB�B�(�Bƣ�B���B�G�B��
B�{B�ffB��HB�G�Bə�B��
B�ffB���B�
=B�p�B�  B�(�B�z�B�
=B�\)B͙�B�(�B�ffBΣ�B�33Bϙ�B�B�=qBиRB���B�33BѮB�{B�=qBҸRB��B�G�Bә�B�(�B�Q�BԸRB�33B�\)B�B�Q�B֏\B��HB�p�B�B�  B؏\B�
=B�G�Bٙ�B�(�Bڏ\B���B�G�B�B�{B�ffB��HB�\)Bݙ�B��Bޏ\B���B��Bߙ�B��B�=qB���B��B�p�B�B�=qB�RB��HB�33B�B�{B�Q�B��B�33B�p�B�B�Q�B��B���B�33B�B�(�B�z�B�RB��B�B��B�(�B�RB��B�\)B�B�(�B�\B���B�33B��B��B�Q�B���B�
=B�\)B��
B�Q�B�z�B��HB�p�B�B�  B�\B���B�33B�B�=qB�z�B���B�\)B�B�{B�z�B���B�\)B��B�{B���B�
=B�\)B���B�=qB��RB�
=B�\)B�B�=qB��RB���B�G�B�B�Q�B��RB���B�\)B��C (�C ffC z�C �C �C33CffC�\C�RC�C=qC\)C�CC  C=qCffC�\CC
=C=qC\)C��C�HC
=C=qCffC��C�HC(�CQ�Cz�C�C  C33CffC�CC  C=qCp�C�\C��C	{C	=qC	ffC	��C	�HC
{C
=qC
\)C
��C
�C{C=qCp�C�RC��C{C=qCz�C�RC��C{C=qCz�CC��C�CG�C�C��C  C(�CQ�C�C��C
=C�C\)C��C�
C  C�C\)C��C�
C��C�C\)C��C��C�C(�Cp�C��CC�C�Cp�C�C�
C��C33Cz�C�C��C  C33Cp�C��C�HC�CG�Cp�C��C�HC{C33CffC�C�HC
=C=qC\)C��C�HC
=C33CffC�\C��C{CQ�Cz�C��C��C  C=qC�C�RC�C{C=qCp�C��C�HC�C\)C��CC�HC{CQ�C�\CC   C �C Q�C z�C ��C �C!(�C!ffC!�\C!C!��C"�C"Q�C"�\C"C#
=C#G�C#�C#�RC#�C$�C$Q�C$�C$C%
=C%Q�C%�C%�C%�HC&�C&Q�C&��C&�
C'�C'ffC'��C'�
C({C(G�C(z�C(�RC)
=C)G�C)�\C)��C*
=C*=qC*z�C*�RC*��C+33C+z�C+�RC,
=C,G�C,z�C,�C,�HC-�C-ffC-�C-�C.33C.p�C.��C.�
C/{C/\)C/�C/�C0(�C0Q�C0��C0�HC1(�C1p�C1�C1�C2�C2\)C2��C2��C3{C3\)C3��C3�
C4�C4Q�C4�C4�RC5  C5G�C5�\C5�
C6�C6\)C6��C6��C7
=C7Q�C7��C7�HC8(�C8ffC8��C8�
C9�C9p�C9�RC9��C:(�C:\)C:��C:�C;=qC;z�C;�RC;�C<�C<\)C<��C<�C=33C=z�C=�C=��C>�C>\)C>��C>�
C?{C?\)C?��C?�HC@�C@ffC@��C@�HCA�CA\)CA��CA�
CB{CBQ�CB�CB��CC
=CC\)CC��CC�CD(�CDp�CD�CD��CE(�CEffCE�CE�CF33CFz�CF�CF��CG=qCGz�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�\)A�^5A�`BA�\)A�XA�M�A�I�A�G�A�K�A�C�A�9XA��A���A���A�t�A�G�A�=qA�;dA�G�A�G�A�I�A�C�A�9XA�1'A�(�A�$�A�{A��AЕ�A�K�A���A��`A���A�t�AɼjA�+AȲ-AǴ9A�G�A��mA�$�A��#A�n�A�JA��yA��
A�^5A�VA�|�A���A�`BA�A���A��A�jA�JA��A���A�r�A�v�A�x�A���A�A�XA�r�A���A�hsA�ĜA�G�A�C�A���A��A���A�ZA�r�A��jA��yA�l�A��TA�p�A���A�VA�5?A��A�n�A��A��A���A�7LA�hsA���A�|�A���A�?}A��A��9A���A�`BA��/A~ȴAw�At��Ar��Aq�-Ap�An�Alv�Ai��Ac��Aa�Aa�Aa+A`�!A`n�A`(�A_��A_�FA_�A^jA\�AY�AS|�AQ��AOAM��AM7LAKK�AI�PAGx�AEƨAD�ABQ�A=�hA;�PA:��A:�uA9A7��A5��A4�A2�A0~�A/`BA-��A+��A)ƨA(�RA'7LA&�/A%�mA%33A$ZA#?}A!%A��A5?A"�A��A1'AbA�#A?}AVAJA�#A^5A�A��AdZAG�A�A�A�`A�HA�HA�/A��A��At�A�yAbNA��AG�A��A=qAbAhsA
��A	�^A��A7LA�yA5?A{A"�A~�AQ�AJA��AVAbA�A|�Al�A ��@��@�o@�S�@�J@�V@���@��;@��@��@�-@�R@��@�^5@�M�@�M�@�7@���@�@�Z@�C�@�n�@�X@��u@�@�@��T@�$�@�@߶F@���@�@�z�@ۅ@�\)@�^5@��/@��@�@ָR@�5?@�G�@Ԭ@ӶF@�S�@�`B@Ь@�Q�@���@Ϯ@υ@�ȴ@���@̴9@��`@�7L@�J@�x�@�+@�dZ@���@�`B@�X@�@�X@���@��@��#@��@�{@�@ɉ7@�hs@�7L@ɩ�@ɲ-@ə�@ȴ9@�bN@�A�@��
@�|�@�ȴ@�ff@��@���@�G�@�Q�@��;@��
@å�@Å@�l�@��@��@+@��-@�`B@��`@��@��u@�bN@�b@���@�l�@�+@��@�~�@�@�`B@���@�A�@�9X@���@��F@��P@��@�l�@���@�5?@��T@��@�@��@���@�?}@���@��u@�1@���@�C�@�"�@���@���@�n�@�V@��@��@��D@��@�|�@�S�@�C�@�"�@�
=@���@���@�n�@�^5@�V@�E�@�{@��#@��^@���@�p�@���@��j@�9X@��F@�dZ@�;d@�ȴ@��\@�V@��-@�&�@��j@��@�r�@�(�@��m@�t�@�C�@��H@�n�@�5?@��@���@��@�x�@�hs@��@��@�I�@���@�@�$�@��#@��h@�hs@�G�@�%@���@���@��@��@��F@���@��@�K�@�o@�ȴ@�V@���@�&�@��@�V@���@��@��@�I�@��m@��
@�ƨ@���@�K�@��R@�=q@�$�@�-@�{@��@��@��@��T@���@���@���@��h@�`B@�&�@��@��@�r�@�Z@� �@�1@�1@��@��w@�;d@���@���@��+@�V@��@��@��@���@�z�@� �@��@��w@��P@�l�@�33@���@��\@�V@��@��^@���@�x�@�V@���@���@��`@��u@� �@��@��
@��@���@�t�@�C�@�ȴ@�ff@�=q@��@�@�@��@��#@��-@��@�O�@�%@��@��w@�@�V@�5?@�$�@�{@�J@���@���@��@���@��^@��@��@��@�j@�(�@��m@���@�l�@�K�@�"�@�ȴ@�V@��T@���@��@�%@��D@�Q�@�1@���@��@�l�@���@�M�@�{@��T@���@���@�X@�7L@�/@�/@�&�@��@�V@�Ĝ@�j@� �@��@���@���@��;@�|�@�;d@�o@�
=@�
=@�@���@��@�ȴ@���@��\@�v�@��^@�X@�G�@�G�@�?}@��@��`@��j@�bN@�1@K�@~ff@}��@}��@}?}@}V@|Z@{C�@z�H@z�\@zJ@y&�@xbN@w�P@w
=@vE�@u`B@t��@s�F@s�@s"�@rn�@qG�@o��@o��@n�y@n{@m��@mp�@mV@l��@l(�@k�m@kƨ@k��@k�@kS�@k"�@k@j~�@i�#@hbN@g�w@fȴ@f{@e�-@e��@e�h@e?}@eV@d�/@d��@dz�@dI�@d1@c�
@c�F@c�@c"�@b��@b~�@b=q@a��@a7L@a�@a�@`�`@`��@`Ĝ@`�9@`��@`�@`bN@`b@_K�@^�+@]�@]?}@\�@\��@\��@\�D@\j@[�
@[dZ@["�@Z��@ZM�@Y�@Yx�@YG�@Y7L@Y%@XĜ@W�@W��@W�@V��@Vff@V@U��@U�@UV@S�
@S"�@R~�@RM�@R�@RJ@Q�#@Q�7@Qhs@QX@Q7L@PĜ@Pr�@PQ�@P �@Pb@P  @O�@P  @O�@O��@O�@O�P@N�y@N5?@M�h@M/@L�@L�j@L��@L�D@Lj@L9X@L1@K�
@K33@J�@J��@J��@J^5@J-@JJ@I�@I�#@I��@I��@IX@I7L@H��@H�@HbN@H1'@G�@G|�@G;d@F�y@F�R@FE�@E�T@E`B@D��@D�@C�F@C�@CdZ@CC�@C33@B�@B��@B^5@BJ@A��@@�`@@�@@bN@?�@?l�@?�@>��@>v�@>E�@>$�@=�-@=p�@=`B@=/@=�@<�@<�@<z�@<(�@;�@;dZ@;33@:��@:�\@:n�@:-@9�#@9�^@9X@9%@8�9@8bN@8  @7|�@6�y@6�R@6v�@6@5?}@4�j@4z�@49X@3�
@3�@3C�@3@2~�@2J@1�7@1�@1%@1%@1%@0��@0��@0�9@0�u@0bN@0Q�@01'@0 �@0  @/�;@/��@/��@/|�@/K�@/+@.�y@.ff@-�-@,��@,�j@,(�@+�m@+��@+33@*�H@*��@*~�@*n�@*M�@*=q@*�@)��@)�#@)��@)X@)%@(Ĝ@(r�@( �@'�;@'��@'��@&ȴ@&ff@&ff@&V@&V@&$�@%��@%��@$��@$�/@$��@$�@$��@$z�@$I�@#��@#33@"�@"��@"��@"M�@"J@!��@!hs@!G�@!&�@!%@ ��@ �`@ Ĝ@ ��@ ��@ ��@ r�@ Q�@ A�@ b@�@�w@�@|�@K�@�y@�+@{@��@��@�h@V@��@j@I�@1@��@t�@S�@S�@33@"�@"�@@�@��@�!@�!@��@�\@�\@n�@�@��@��@�^@��@hs@&�@�9@r�@r�@bN@A�@1'@b@�;@��@|�@ȴ@��@ff@5?@�T@��@�h@`B@�@��@�@��@�j@�D@z�@Z@9X@1@ƨ@t�@33@@^5@��@�^@��@�7@�7@hs@�@��@�u@A�@A�@ �@�@�@|�@K�@�y@ȴ@��@�+@v�@5?@$�@{@{@{@@@@�-@?}@�@�@�/@��@�@��@��@z�@z�@Z@1@�m@�
@ƨ@��@�@C�A�O�A�bNA�`BA�`BA�\)A�ZA�`BA�\)A�XA�bNA�`BA�^5A�dZA�`BA�ZA�^5A�\)A�`BA�\)A�M�A�Q�A�O�A�G�A�K�A�G�A�I�A�?}A�A�A�Q�A�K�A�M�A�O�A�E�A�E�A�A�A�=qA�?}A�7LA�33A�5?A�JA�
=A���A���A��;A��`A���A���A���A���A���A��#A�ƨA���A���A��#A���A�AѾwAѸRAэPAсA�l�A�ZA�K�A�K�A�G�A�E�A�M�A�E�A�G�A�E�A�?}A�=qA�A�A�9XA�33A�5?A�1'A�;dA�;dA�?}A�I�A�M�A�G�A�G�A�E�A�E�A�I�A�G�A�C�A�I�A�I�A�G�A�K�A�K�A�E�A�I�A�M�A�G�A�I�A�K�A�E�A�G�A�E�A�=qA�;dA�?}A�;dA�7LA�=qA�7LA�5?A�7LA�33A�/A�33A�/A�+A�-A�(�A�$�A�+A�$�A�"�A�(�A�"�A�"�A�&�A��A��A��A��A�{A�{A�VA�  A���A��A��A��yA��`A��#A���A���A�AУ�A�|�A�O�A�/A���A�ȴAϏ\A�?}AΟ�A�VA���A���A���A��yAʹ9A�z�A�=qA��A���A��mA���A̡�A��/A�{A�A��`A�ȴAʶFAʥ�Aʟ�Aʝ�AʅA�S�A� �A�JA��A���Aɴ9Aɏ\A�|�A�dZA�VA�=qA�"�A�oA�A���A��A��A�ĜAț�AȁA�M�A��A���Aǟ�AǁA�p�A�dZA�ZA�ZA�O�A�;dA�5?A�33A�/A�"�A�oA��AƶFA�|�A�`BA�Q�A�E�A�+A�JA��yA��TA��`A��TA��/A��/A���A���Aŝ�AōPA�t�A�S�A�I�A�?}A�;dA�/A�"�A��A���A�A�x�A�E�A��A��mAå�A�dZA�5?A�bA��A�A�A�|�A�ffA�`BA�^5A�bNA�XA�I�A�=qA�1'A�oA�A��A��TA���A��^A���A��+A�l�A�ZA�A�A�$�A�VA���A��`A�ƨA��!A��hA�l�A�dZA�9XA� �A�bA�%A��A��;A�ƨA��RA���A�p�A��A�bNA�A��7A�ffA�9XA���A�ȴA��!A���A�z�A�=qA�{A��yA�A��-A��A���A���A��hA��+A�~�A�~�A�v�A�l�A�jA�bNA�Q�A�;dA��A��A���A�A���A���A��A�v�A�t�A�v�A�r�A�p�A�dZA�Q�A�C�A�;dA�/A� �A�bA�VA�VA�
=A���A�A���A��A��A��yA�ȴA�ĜA��wA��A���A���A��7A�z�A�t�A�hsA�O�A�?}A�+A��A�1A���A��mA��/A��#A���A�
=A��RA�x�A�&�A���A�VA�S�A��mA���A��A�dZA�;dA�%A���A��A���A�jA�G�A�1'A��A���A��`A���A���A��A��A�x�A�t�A�p�A�^5A�=qA�(�A��A�1A��A��
A�ƨA��RA���A�t�A�G�A�{A���A��PA�dZA�K�A�7LA�+A��A�A��#A��jA���A�|�A�bNA�C�A�&�A��mA��-A��A�9XA���A��A�Q�A��yA���A�v�A�"�A��RA�l�A�=qA�"�A���A�|�A�ffA�G�A�oA�ƨA���A���A���A���A��uA��A�-A��9A��A���A���A�7LA��mA��;A��/A��A��A��/A��;A��/A��
A��
A���A��RA���A���A��hA��DA�z�A�l�A�dZA�bNA�^5A�K�A�$�A��#A���A��A�\)A�Q�A�Q�A�?}A�=qA�9XA�-A�VA���A�G�A��FA�ZA�A�/A��hA�jA�^5A�O�A�?}A�5?A�+A�$�A��A��A�oA�1A�  A���A��A��
A��!A��DA��+A��+A��A��A��A��A�t�A�n�A�hsA�bNA�ZA�M�A�A�A�;dA�5?A�"�A�VA��A���A�~�A�K�A��A��A���A���A�v�A�C�A�A��
A���A��hA�z�A�jA�E�A�%A���A�A��#A���A���A�A�ĜA�A��RA���A���A���A���A��\A��7A�|�A�\)A�=qA� �A��A��/A��9A�dZA��A��wA��A��A�^5A�7LA�A��mA��RA��+A�~�A�p�A�S�A�G�A�=qA�oA���A��jA��A���A���A���A��uA��uA��hA��hA��7A��A�t�A�XA�E�A��A���A��/A���A�E�A��^A�S�A��
A�r�A�A���A�A��uA�ffA�dZA�VA�&�A��A��yA��A���A���A���A���A���A���A���A�ĜA��jA��A��A�O�A��A��TA��;A��HA��;A��
A��FA�t�A�O�A�O�A�9XA�%A��A��!A�dZA�E�A�bA��A���A��A��\A�`BA�I�A�/A�oA���A��A��A���A��A�t�A�E�A�JA�x�A��A�jA��A�  A��A��
A��RA���A�t�A�33A��A��A��TA��/A��
A���A���A�ƨA�ĜA���A��^A��9A��!A���A���A��A�dZA�=qA� �A��A�VA���A��;A���A��FA���A�~�A�v�A�jA�ZA�I�A�;dA�-A�{A���A���A��A��A��A��A��mA���A��+A�$�A�A��#A��A�VA��A�{A�ȴA��-A���A���A���A���A���A���A���A��DA�|�A�t�A�jA�ZA�A�A�?}A�?}A�;dA�9XA�5?A�1'A�(�A��!A���A���A��DA�K�A�"�A`BA~1'A}�hA}�A|1'A{�Azr�Axz�AwAw�Avr�Av=qAv$�Au�Au�PAup�Au�Au%At�RAt�As�wAsp�Ar��ArȴArz�Arv�Ar^5ArM�Ar(�Ar�Aq�Aq�
Aq�Aq��Aq|�AqdZAqG�Aq33Aq�Ap��Ap�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                      A�^5A�\)A�^5A�`BA�\)A�XA�M�A�I�A�G�A�K�A�C�A�9XA��A���A���A�t�A�G�A�=qA�;dA�G�A�G�A�I�A�C�A�9XA�1'A�(�A�$�A�{A��AЕ�A�K�A���A��`A���A�t�AɼjA�+AȲ-AǴ9A�G�A��mA�$�A��#A�n�A�JA��yA��
A�^5A�VA�|�A���A�`BA�A���A��A�jA�JA��A���A�r�A�v�A�x�A���A�A�XA�r�A���A�hsA�ĜA�G�A�C�A���A��A���A�ZA�r�A��jA��yA�l�A��TA�p�A���A�VA�5?A��A�n�A��A��A���A�7LA�hsA���A�|�A���A�?}A��A��9A���A�`BA��/A~ȴAw�At��Ar��Aq�-Ap�An�Alv�Ai��Ac��Aa�Aa�Aa+A`�!A`n�A`(�A_��A_�FA_�A^jA\�AY�AS|�AQ��AOAM��AM7LAKK�AI�PAGx�AEƨAD�ABQ�A=�hA;�PA:��A:�uA9A7��A5��A4�A2�A0~�A/`BA-��A+��A)ƨA(�RA'7LA&�/A%�mA%33A$ZA#?}A!%A��A5?A"�A��A1'AbA�#A?}AVAJA�#A^5A�A��AdZAG�A�A�A�`A�HA�HA�/A��A��At�A�yAbNA��AG�A��A=qAbAhsA
��A	�^A��A7LA�yA5?A{A"�A~�AQ�AJA��AVAbA�A|�Al�A ��@��@�o@�S�@�J@�V@���@��;@��@��@�-@�R@��@�^5@�M�@�M�@�7@���@�@�Z@�C�@�n�@�X@��u@�@�@��T@�$�@�@߶F@���@�@�z�@ۅ@�\)@�^5@��/@��@�@ָR@�5?@�G�@Ԭ@ӶF@�S�@�`B@Ь@�Q�@���@Ϯ@υ@�ȴ@���@̴9@��`@�7L@�J@�x�@�+@�dZ@���@�`B@�X@�@�X@���@��@��#@��@�{@�@ɉ7@�hs@�7L@ɩ�@ɲ-@ə�@ȴ9@�bN@�A�@��
@�|�@�ȴ@�ff@��@���@�G�@�Q�@��;@��
@å�@Å@�l�@��@��@+@��-@�`B@��`@��@��u@�bN@�b@���@�l�@�+@��@�~�@�@�`B@���@�A�@�9X@���@��F@��P@��@�l�@���@�5?@��T@��@�@��@���@�?}@���@��u@�1@���@�C�@�"�@���@���@�n�@�V@��@��@��D@��@�|�@�S�@�C�@�"�@�
=@���@���@�n�@�^5@�V@�E�@�{@��#@��^@���@�p�@���@��j@�9X@��F@�dZ@�;d@�ȴ@��\@�V@��-@�&�@��j@��@�r�@�(�@��m@�t�@�C�@��H@�n�@�5?@��@���@��@�x�@�hs@��@��@�I�@���@�@�$�@��#@��h@�hs@�G�@�%@���@���@��@��@��F@���@��@�K�@�o@�ȴ@�V@���@�&�@��@�V@���@��@��@�I�@��m@��
@�ƨ@���@�K�@��R@�=q@�$�@�-@�{@��@��@��@��T@���@���@���@��h@�`B@�&�@��@��@�r�@�Z@� �@�1@�1@��@��w@�;d@���@���@��+@�V@��@��@��@���@�z�@� �@��@��w@��P@�l�@�33@���@��\@�V@��@��^@���@�x�@�V@���@���@��`@��u@� �@��@��
@��@���@�t�@�C�@�ȴ@�ff@�=q@��@�@�@��@��#@��-@��@�O�@�%@��@��w@�@�V@�5?@�$�@�{@�J@���@���@��@���@��^@��@��@��@�j@�(�@��m@���@�l�@�K�@�"�@�ȴ@�V@��T@���@��@�%@��D@�Q�@�1@���@��@�l�@���@�M�@�{@��T@���@���@�X@�7L@�/@�/@�&�@��@�V@�Ĝ@�j@� �@��@���@���@��;@�|�@�;d@�o@�
=@�
=@�@���@��@�ȴ@���@��\@�v�@��^@�X@�G�@�G�@�?}@��@��`@��j@�bN@�1@K�@~ff@}��@}��@}?}@}V@|Z@{C�@z�H@z�\@zJ@y&�@xbN@w�P@w
=@vE�@u`B@t��@s�F@s�@s"�@rn�@qG�@o��@o��@n�y@n{@m��@mp�@mV@l��@l(�@k�m@kƨ@k��@k�@kS�@k"�@k@j~�@i�#@hbN@g�w@fȴ@f{@e�-@e��@e�h@e?}@eV@d�/@d��@dz�@dI�@d1@c�
@c�F@c�@c"�@b��@b~�@b=q@a��@a7L@a�@a�@`�`@`��@`Ĝ@`�9@`��@`�@`bN@`b@_K�@^�+@]�@]?}@\�@\��@\��@\�D@\j@[�
@[dZ@["�@Z��@ZM�@Y�@Yx�@YG�@Y7L@Y%@XĜ@W�@W��@W�@V��@Vff@V@U��@U�@UV@S�
@S"�@R~�@RM�@R�@RJ@Q�#@Q�7@Qhs@QX@Q7L@PĜ@Pr�@PQ�@P �@Pb@P  @O�@P  @O�@O��@O�@O�P@N�y@N5?@M�h@M/@L�@L�j@L��@L�D@Lj@L9X@L1@K�
@K33@J�@J��@J��@J^5@J-@JJ@I�@I�#@I��@I��@IX@I7L@H��@H�@HbN@H1'@G�@G|�@G;d@F�y@F�R@FE�@E�T@E`B@D��@D�@C�F@C�@CdZ@CC�@C33@B�@B��@B^5@BJ@A��@@�`@@�@@bN@?�@?l�@?�@>��@>v�@>E�@>$�@=�-@=p�@=`B@=/@=�@<�@<�@<z�@<(�@;�@;dZ@;33@:��@:�\@:n�@:-@9�#@9�^@9X@9%@8�9@8bN@8  @7|�@6�y@6�R@6v�@6@5?}@4�j@4z�@49X@3�
@3�@3C�@3@2~�@2J@1�7@1�@1%@1%@1%@0��@0��@0�9@0�u@0bN@0Q�@01'@0 �@0  @/�;@/��@/��@/|�@/K�@/+@.�y@.ff@-�-@,��@,�j@,(�@+�m@+��@+33@*�H@*��@*~�@*n�@*M�@*=q@*�@)��@)�#@)��@)X@)%@(Ĝ@(r�@( �@'�;@'��@'��@&ȴ@&ff@&ff@&V@&V@&$�@%��@%��@$��@$�/@$��@$�@$��@$z�@$I�@#��@#33@"�@"��@"��@"M�@"J@!��@!hs@!G�@!&�@!%@ ��@ �`@ Ĝ@ ��@ ��@ ��@ r�@ Q�@ A�@ b@�@�w@�@|�@K�@�y@�+@{@��@��@�h@V@��@j@I�@1@��@t�@S�@S�@33@"�@"�@@�@��@�!@�!@��@�\@�\@n�@�@��@��@�^@��@hs@&�@�9@r�@r�@bN@A�@1'@b@�;@��@|�@ȴ@��@ff@5?@�T@��@�h@`B@�@��@�@��@�j@�D@z�@Z@9X@1@ƨ@t�@33@@^5@��@�^@��@�7@�7@hs@�@��@�u@A�@A�@ �@�@�@|�@K�@�y@ȴ@��@�+@v�@5?@$�@{@{@{@@@@�-@?}@�@�@�/@��@�@��@��@z�@z�@Z@1@�m@�
@ƨ@��@�@C�A�O�A�bNA�`BA�`BA�\)A�ZA�`BA�\)A�XA�bNA�`BA�^5A�dZA�`BA�ZA�^5A�\)A�`BA�\)A�M�A�Q�A�O�A�G�A�K�A�G�A�I�A�?}A�A�A�Q�A�K�A�M�A�O�A�E�A�E�A�A�A�=qA�?}A�7LA�33A�5?A�JA�
=A���A���A��;A��`A���A���A���A���A���A��#A�ƨA���A���A��#A���A�AѾwAѸRAэPAсA�l�A�ZA�K�A�K�A�G�A�E�A�M�A�E�A�G�A�E�A�?}A�=qA�A�A�9XA�33A�5?A�1'A�;dA�;dA�?}A�I�A�M�A�G�A�G�A�E�A�E�A�I�A�G�A�C�A�I�A�I�A�G�A�K�A�K�A�E�A�I�A�M�A�G�A�I�A�K�A�E�A�G�A�E�A�=qA�;dA�?}A�;dA�7LA�=qA�7LA�5?A�7LA�33A�/A�33A�/A�+A�-A�(�A�$�A�+A�$�A�"�A�(�A�"�A�"�A�&�A��A��A��A��A�{A�{A�VA�  A���A��A��A��yA��`A��#A���A���A�AУ�A�|�A�O�A�/A���A�ȴAϏ\A�?}AΟ�A�VA���A���A���A��yAʹ9A�z�A�=qA��A���A��mA���A̡�A��/A�{A�A��`A�ȴAʶFAʥ�Aʟ�Aʝ�AʅA�S�A� �A�JA��A���Aɴ9Aɏ\A�|�A�dZA�VA�=qA�"�A�oA�A���A��A��A�ĜAț�AȁA�M�A��A���Aǟ�AǁA�p�A�dZA�ZA�ZA�O�A�;dA�5?A�33A�/A�"�A�oA��AƶFA�|�A�`BA�Q�A�E�A�+A�JA��yA��TA��`A��TA��/A��/A���A���Aŝ�AōPA�t�A�S�A�I�A�?}A�;dA�/A�"�A��A���A�A�x�A�E�A��A��mAå�A�dZA�5?A�bA��A�A�A�|�A�ffA�`BA�^5A�bNA�XA�I�A�=qA�1'A�oA�A��A��TA���A��^A���A��+A�l�A�ZA�A�A�$�A�VA���A��`A�ƨA��!A��hA�l�A�dZA�9XA� �A�bA�%A��A��;A�ƨA��RA���A�p�A��A�bNA�A��7A�ffA�9XA���A�ȴA��!A���A�z�A�=qA�{A��yA�A��-A��A���A���A��hA��+A�~�A�~�A�v�A�l�A�jA�bNA�Q�A�;dA��A��A���A�A���A���A��A�v�A�t�A�v�A�r�A�p�A�dZA�Q�A�C�A�;dA�/A� �A�bA�VA�VA�
=A���A�A���A��A��A��yA�ȴA�ĜA��wA��A���A���A��7A�z�A�t�A�hsA�O�A�?}A�+A��A�1A���A��mA��/A��#A���A�
=A��RA�x�A�&�A���A�VA�S�A��mA���A��A�dZA�;dA�%A���A��A���A�jA�G�A�1'A��A���A��`A���A���A��A��A�x�A�t�A�p�A�^5A�=qA�(�A��A�1A��A��
A�ƨA��RA���A�t�A�G�A�{A���A��PA�dZA�K�A�7LA�+A��A�A��#A��jA���A�|�A�bNA�C�A�&�A��mA��-A��A�9XA���A��A�Q�A��yA���A�v�A�"�A��RA�l�A�=qA�"�A���A�|�A�ffA�G�A�oA�ƨA���A���A���A���A��uA��A�-A��9A��A���A���A�7LA��mA��;A��/A��A��A��/A��;A��/A��
A��
A���A��RA���A���A��hA��DA�z�A�l�A�dZA�bNA�^5A�K�A�$�A��#A���A��A�\)A�Q�A�Q�A�?}A�=qA�9XA�-A�VA���A�G�A��FA�ZA�A�/A��hA�jA�^5A�O�A�?}A�5?A�+A�$�A��A��A�oA�1A�  A���A��A��
A��!A��DA��+A��+A��A��A��A��A�t�A�n�A�hsA�bNA�ZA�M�A�A�A�;dA�5?A�"�A�VA��A���A�~�A�K�A��A��A���A���A�v�A�C�A�A��
A���A��hA�z�A�jA�E�A�%A���A�A��#A���A���A�A�ĜA�A��RA���A���A���A���A��\A��7A�|�A�\)A�=qA� �A��A��/A��9A�dZA��A��wA��A��A�^5A�7LA�A��mA��RA��+A�~�A�p�A�S�A�G�A�=qA�oA���A��jA��A���A���A���A��uA��uA��hA��hA��7A��A�t�A�XA�E�A��A���A��/A���A�E�A��^A�S�A��
A�r�A�A���A�A��uA�ffA�dZA�VA�&�A��A��yA��A���A���A���A���A���A���A���A�ĜA��jA��A��A�O�A��A��TA��;A��HA��;A��
A��FA�t�A�O�A�O�A�9XA�%A��A��!A�dZA�E�A�bA��A���A��A��\A�`BA�I�A�/A�oA���A��A��A���A��A�t�A�E�A�JA�x�A��A�jA��A�  A��A��
A��RA���A�t�A�33A��A��A��TA��/A��
A���A���A�ƨA�ĜA���A��^A��9A��!A���A���A��A�dZA�=qA� �A��A�VA���A��;A���A��FA���A�~�A�v�A�jA�ZA�I�A�;dA�-A�{A���A���A��A��A��A��A��mA���A��+A�$�A�A��#A��A�VA��A�{A�ȴA��-A���A���A���A���A���A���A���A��DA�|�A�t�A�jA�ZA�A�A�?}A�?}A�;dA�9XA�5?A�1'A�(�A��!A���A���A��DA�K�A�"�A`BA~1'A}�hA}�A|1'A{�Azr�Axz�AwAw�Avr�Av=qAv$�Au�Au�PAup�Au�Au%At�RAt�As�wAsp�Ar��ArȴArz�Arv�Ar^5ArM�Ar(�Ar�Aq�Aq�
Aq�Aq��Aq|�AqdZAqG�Aq33Aq�Ap��Ap�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BI�BJ#BI�BIBIBHBHBF�BE�BGzBF�BEmBC-BAUB@BA B@OBB�BD3BJ�BL�BN�BP}BP�BQ�BRTBRTBR�BP}BRTB��B�aB�mB9�BJ�Bh
B~]B��B��B�CB�-B��B�zB�*B�0B��BɆB�tB��B�B�$B�zB�LB�$B��B�$B��B��B��B��B�uBw�Bp;B`vBNB+6B�B�"B�B�B��B�pB�3B��B�_B�~B}VBd�BaHB\�BV�BJ�BD�B'�B�B	7B
�DB
��B
��B
�sB
��B
�B
��B
��B
��B
��B
�B
ZB
T�B
NpB
1�B
~B	�B	�B	�#B	��B	�<B	��B	�!B	��B	�@B	�\B	��B	�xB	��B	�1B	��B	��B	�oB	{B	p;B	i�B	H�B	?�B	4�B	(�B	#�B	�B	�B	xB	 �B��B��B�KB�BBٴB�B�?B�HB�B�BB��B�wB��B��B��B�B��B�+B�$B��B�FB�hB�B�rB��B��B�%B��B��B�1B��B�7B��B��B��B�4B��B��B��B��B��B��B��B�$B�*B��B�B�EB�EBɆBǮB��B�#BٴB��B�BѷBϫB�B�EB��B�[B��BŢB��BǮB��B�)BǮB��B�UB��B�B��BɆB�-B�-B�BɆB�3B��B��B��B��B��B�^B��B̘B�}B֡B�B�KB��B��B�9BרB�HB�`B�mB� B�|B��B�B�B��B�2B�+B��B�B��B�GB��B�lB�B��B�JB�JB�"B��B	�B	%B	+B		B	VB	B	�B	�B	�B	"hB	-�B	1�B	8�B	-B	-�B	2�B	2�B	3�B	7�B	?HB	E�B	L�B	VB	^B	aB	c�B	e�B	jB	l"B	tB	{B	{B	|PB	y�B	y�B	x8B	x�B	v�B	x8B	y	B	y	B	|B	�B	�;B	�B	�B	�AB	��B	�B	��B	�YB	�=B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	��B	�CB	�'B	�B	�B	�0B	�B	��B	��B	�B	�IB	�}B	�[B	�[B	�aB	�hB	��B	��B	�XB	��B	��B	��B	��B	�6B	�dB	�B	��B	�B	��B	��B	�OB	�?B	��B	�XB	��B	�)B	��B	�dB	��B	�B	ϫB	�vB	ϫB	ϫB	�B	�B	�TB	�aB	�aB	��B	�KB	�WB	��B	ݘB	��B	�pB	�B	�B	�B	�pB	�vB	�BB	�BB	�B	�B	�B	�ZB	�B	�fB	�B	�>B	��B	�sB	�sB	�yB	�KB	�B	�KB	�B	�]B	��B	��B	��B	��B	��B	��B	��B	�vB	�GB	��B	�B	��B	��B	�;B	�B	�B	�B	�`B	��B	�`B	�`B	��B	�2B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�(B	��B	��B
 4B
 iB
 iB
 iB
 �B
B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
YB
%B
�B
%B
�B
B
B
B
�B
SB
SB
�B
�B
SB
%B
+B
�B
_B
1B
1B
�B
�B
	�B
	lB
	lB
	B

rB

�B

rB

�B
xB
DB
�B
�B
�B
�B
�B
VB
VB
"B
VB
VB
�B
�B
�B
�B
�B
�B
�B
�B
MB
MB
MB
MB
�B
�B
�B
�B
�B
�B
YB
�B
�B
+B
_B
�B
�B
_B
+B
1B
_B
eB
�B
1B
�B
qB
�B
qB
�B
�B
�B
 �B
�B
 \B
 'B
 \B
!�B
#nB
#:B
#�B
$@B
$�B
%B
%FB
%�B
&LB
'�B
'�B
($B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
*0B
*�B
*�B
+B
*�B
+�B
+�B
-�B
.IB
.IB
.IB
.}B
.�B
.�B
.}B
0UB
0�B
1�B
2aB
2�B
2�B
3hB
33B
49B
5B
5B
5B
4�B
4nB
4B
49B
4B
49B
3�B
4�B
5tB
5�B
6�B
7�B
7�B
7LB
6�B
7�B
7LB
7LB
7LB
7�B
7LB
7B
6�B
7B
7B
7B
7LB
7LB
7�B
9XB
:^B
:^B
:^B
:*B
:^B
:^B
:^B
:*B
:�B
:*B
:*B
:^B
:^B
:�B
:�B
:�B
:�B
:�B
;0B
;�B
;�B
<6B
<�B
=B
=B
=<B
=qB
=qB
=<B
=<B
=<B
=<B
=<B
=<B
>wB
?HB
?�B
@�B
@�B
A B
@�B
A B
A B
A�B
B'B
B'B
B�B
B�B
B�B
C-B
B�B
C-B
C-B
CaB
D�B
D�B
E9B
E�B
E�B
F?B
FB
FB
F?B
F�B
F?B
GB
G�B
G�B
G�B
G�B
HB
G�B
HB
H�B
H�B
HKB
HB
HKB
H�B
H�B
H�B
HKB
H�B
H�B
H�B
HB
IB
J#B
J�B
K)B
K^B
K^B
K^B
K�B
K^B
K�B
K�B
K�B
L�B
L�B
L�B
LdB
L�B
MB
MB
MjB
M6B
MjB
MjB
MjB
M�B
M�B
N<B
NpB
NpB
OBB
OBB
O�B
O�B
O�B
PHB
P�B
QB
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S[B
S�B
S�B
TaB
T�B
U2B
U2B
V9B
VB
V�B
V�B
W
B
W?B
W
B
W�B
W�B
XB
XEB
W�B
XEB
XEB
XyB
X�B
YKB
YKB
YB
Y�B
Y�B
Y�B
ZQB
Z�B
ZQB
[WB
[#B
[WB
[�B
\)B
\�B
\�B
\�B
]/B
]�B
^�B
^�B
_B
_pB
_�B
`B
`B
`vB
`�B
aHB
bB
bB
bB
bB
bB
bB
bNB
bNB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cTB
c�B
cTB
c�B
d&B
d�B
e`B
e`B
f2B
f2B
f�B
g8B
gmB
g�B
h
B
h>B
hsB
hsB
hsB
h�B
h�B
iB
iyB
i�B
i�B
jKB
jKB
j�B
jB
j�B
l"B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
m)B
m)B
m)B
m)B
m)B
m]B
m)B
ncB
n�B
n�B
n�B
o5B
oiB
o�B
pB
p;B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
qAB
qvB
q�B
rB
q�B
q�B
rB
r|B
r�B
sMB
s�B
sMB
sMB
r�B
sB
sMB
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
uZB
u�B
v`B
v`B
v�B
v�B
v�B
v�B
w2B
w2B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
y	B
y	B
y	B
y>B
y>B
y�B
zB
zB
zDB
zDB
z�B
z�B
{B
{B
{B
{B
{B
{B
{�B
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
|�B
}�B
~(B
~]B
~]B
~]B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
.B
�B
�B
�B
�B
�B
� B
� B
� B
� B
� B
� B
�4B
� B
� B
��B
��B
��B
�;B
�;B
�;B
�oB
�oB
�oB
��B
�oB
�oB
�B
�B
�B
�B
�B
�uB
�@��kBI�BH�BH�BI�BJ�BIRBI�BK�BG�BJ#BJ#BH�BH�BK�BHKBI�BHBI�BHBI�BF?BHKBE9BGEBFtBI�BB'BF?BGzBF�BF?BIBE�BEmBGEBD�BC�BE�BD�BQ�BC�B>�BB�BD�B>BC�BA�B@OB?BB[B@�BF�BA�B@�B=�B>wB?B@BB[BD3B>�B@�BA�B?�B=�B?B@�B@BC�BA BB�BCaBB�BB'BC-BB�B@�BB�BA�BDgBE9BHKBH�BK�BJ#BI�BK^BJ�BK�BM6BK�BK�BN�BM�BM6BOBN�BM�BPHBOBNpBP�BO�BOBQNBQ�BO�BP}BR BO�BQ�BR BPHBQNBR�BP�BR BS&BQ�BR BS[BQBR�BS�BQBR�BR�BQNBS�BR BR BS�BS�BQ�BR�BT�BQ�BP}BQ�BOvBN<BO�BOBK�BK�BP}BT�B[WB^�Bh�Br|By�B��B��B��B�B�B�B��B��B�B�dB�B�B�B�B�B$�B6FB5�B:^B>B?�BB[BB�BB'BF�BR�BXBZB_Bc�Bi�Bo BqvBv`BwfB|�B��B�B�B�SB��B�rB�VB��B��B�!B�!B�zB��B�=B�eB��B�qB�kB�qB�UB�OB��B��B�OB��B��B��B�9B��B�3B�?B��B��B��B��B�B��B�LB��B��B��B�6B�0B��B��B�XB�LB��B�^B�^B�RB�B��B��B�B��B�tB��B��BȀB�BʌB�BǮBʌB�tB��B��B�gB�tBǮBǮB�9B�KB�tBÖB��B��B�?B��B�B��B��B��B�<B��B��B�XB��B�B�XB�XB�9B��B�tB�?B�aB��B�hB��B��B�aB��B��B�aB��B�RB�'B�B��B��B�?B��B��B�BB�$B��B�jB�^B�B�$B��B��B��B��B�B��B�$B�LB��B��B�^B��B�jB��B�RB�jB��B��B�$B��B�LB�RB�RB��B��B�*B��B�XB�*B�RB��B��B��B�XB��B��B�B��B�FB�tB�nB�B�tB�9B��B��B�?B�aB�'B��B�hB��B�[B��B�B��B�CB��B��B��B��B��B��B�KB��B��B�B��B��B��B�=B�B��B�AB�uB��B� B}�B}"B{�B~�B{�B|�Bw�BuZBu�Bu�BtBv�Bv�BtBrBsBs�Bq�Bo5Bm)BncBpoBm�BlWBp;Bc�Be�Bd�Bb�B`�B`B_pBbB\]B[WB[#BW?BUgBUgBVmBP}BJ#BN�BMjBC�B?}BD�B6�B1�B5�B4�B)�B%zB �B2�B�BeB�B�BxB�B�BPB�B�B�B�B�B�BB�BbB�]B�GB��B�B�GB��B�;B�oB�AB�AB�B��B�AB�;B�iB�WB�WB��B�B�yB��B��B�/B��B�>B�TB�BޞB�BܒB�KB��B�B�pB�B�B�
B�KB�6B�NB�0B��B��B��B��B��B��B��B��B�-B��B�-B�aB��B��B��B��B�kB��B�_B�eB�B�6B��B�6B�B�kB��B��B��B��B�RB�zB�LB�B��B��B�FB��B��B��B��B��B�JB��B�1B��BtTBw�BtBr�Bv�BtTB{Bq�Bd�B^5B^�B_B\�B^5B]�B^5B^jB`vB`BB]�B`�Bb�BffBaB`�BaBb�BaHBe`Be,BW
BVBW?BWsBW�B\�BXyB[WBWsBR�BVmBW
BS[BS&BX�BS[BNpBMBK)BI�BI�BJ�BH�BH�BF?BFBEmBF�BE9BC�BGzB@OB@�BB[BEmBEmB6zB8B.}B-�B�B�B �BCB:B@BB�B�BhBDBJBfBJB�B
�B
rB�B�B	�B�B
=B�B
�cB
��B
��B
�B
��B
��B
�"B
�>B
�%B
�DB
�vB
��B
�|B
��B
�WB
�KB
�B
�ZB
�B
�B
�B
یB
��B
�5B
��B
��B
ںB
�|B
�HB
�QB
ܒB
�B
�B
�5B
�B
��B
�B
��B
��B
�B
�B
��B
��B
�B
�B
�hB
�[B
��B
�UB
�wB
�IB
��B
��B
�wB
��B
��B
�qB
��B
��B
��B
��B
��B
�hB
��B
�4B
��B
��B
��B
��B
��B
�+B
�_B
��B
��B
��B
��B
�FB
�B
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
�_B
�4B
sB
jB
[#B
YKB
Z�B
[�B
ZB
X�B
X�B
YB
X�B
YB
S�B
V�B
W?B
S�B
P�B
NB
N�B
L�B
JXB
JXB
I�B
j�B
QNB
A�B
>BB
@�B
4B
C�B
7B
%�B
&�B
�B
!B
,B
'RB
 �B
fB	�>B	�B	�%B	�B	�fB	�B	�B	�B	�QB	�B	�B	�DB	�`B	�HB	�B	�QB	�]B	��B	��B	�QB	�dB	��B	��B	ܒB	�B	خB	��B	�?B	�B	֡B	�9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                      BE�BFsBF
BEmBEmBDgBDgBC,BA�BC�BB�BA�B?}B=�B<jB=pB<�B?B@�BGBIBJ�BL�BM5BNBN�BN�BOBBL�BN�B�B��B�B5�BF�BdZBz�B�IB��B��B�}B��B��B�zB��B�NB��B��B�&B�jB�tB��B��B�tB�EB�tB��B�'B��B��B~�Bs�Bl�B\�BJWB'�B�B�rB��B�
B�B��B��B��B��B��By�BaB]�BYBR�BGB@�B$@BIB�B
��B
�2B
�B
��B
�#B
�kB
�B
��B
��B
�CB
�iB
VmB
P�B
J�B
.IB
�B	�WB	��B	�sB	� B	ʌB	�B	�qB	�B	��B	��B	�	B	��B	�%B	��B	��B	�:B	}�B	w�B	l�B	f2B	EB	<B	1'B	$�B	�B	B	!B	�B�"B��B�GB�BܒB�B�`BӏB̘B�[B��B��B��B�$B��B��B�eB��B�{B�tB�B��B��B�VB��B�7B�MB�uB�B�GB��B��B��B�	B�B��B��B�B�B�<B�6B�0B�BB�B�tB�zB�EB�dBÕBÕB��B��B�<B�sB�B�2B�TB�B��B�WBÕB�HB��B�B��B�B��B�EB�yB��B�B��B�6B�^B�)B��B�}B�}B�aB��B��B��B��B�)B�6B�KBǮB�KB��B��B��B�mB՛B�B�2B҉B��BݘB�B�B�PB��B�MB�fB�SB�.B�B�{B�;B� B�;B�B�;B��B�`B�7B��B��B�rB�	B	 4B	uB	{B	SB	
�B	VB	:B	FB	�B	�B	*0B	.IB	4�B	)^B	*0B	/B	/OB	0 B	4B	;�B	B&B	H�B	RTB	ZQB	]cB	_�B	bNB	ffB	hrB	poB	w�B	w�B	x�B	u�B	u�B	t�B	t�B	sMB	t�B	uYB	uYB	xlB	|B	}�B	}VB	~\B	~�B	~�B	�iB	�4B	��B	��B	�+B	�=B	��B	�CB	�IB	�!B	��B	�4B	�B	�tB	��B	��B	�wB	�UB	�nB	��B	�XB	�*B	�0B	�dB	��B	��B	��B	��B	��B	��B	�-B	�9B	��B	�EB	��B	�B	�#B	��B	��B	�RB	�#B	�dB	�B	��B	��B	B	�?B	ƨB	�B	�yB	�B	ȴB	�B	�^B	��B	��B	��B	��B	�dB	�jB	ΤB	бB	бB	�,B	՛B	קB	�B	��B	�B	��B	�WB	�]B	�]B	��B	��B	ܒB	ܒB	�B	�iB	��B	�B	��B	�B	��B	�B	�,B	��B	��B	��B	�B	�B	�B	�fB	�B	�JB	�JB	�JB	�B	�B	�B	�B	��B	�B	�(B	�WB	�"B	�"B	�B	�]B	�cB	� B	�B	�B	�B	�B	�B	�B	�fB	�7B	�B	�B	�B	�B	�7B	��B	�>B	�B	�xB	�JB	��B	��B	��B	��B	��B	��B	�VB	�VB	�(B	��B
  B
:B
B
B
oB
:B
B
oB
oB
�B
�B
uB
@B
uB
B
oB
oB
oB
B
�B
�B
�B
B
�B
uB
{B
B
�B
�B
�B
MB
B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
�B
1B
	�B

=B

=B

�B

�B

rB

�B

�B

�B

�B

�B
B
IB
!B
.B
B
�B
�B
�B
�B
B
�B
B
B
B
�B
�B
FB
B
{B
�B
B
�B
�B
{B
�B
�B
�B
�B
�B
$B
�B
�B
�B
0B
B
�B
�B
CB
�B
wB
�B
B
�B
�B
�B
 �B
!-B
!bB
!�B
!�B
"�B
#�B
$@B
$tB
$@B
$@B
$�B
%B
%�B
%�B
%�B
&�B
&�B
&�B
'RB
'B
'�B
'�B
)�B
*�B
*�B
*�B
*�B
+B
+6B
*�B
,�B
,�B
-�B
.�B
/B
/OB
/�B
/�B
0�B
1[B
1[B
1[B
1'B
0�B
0UB
0�B
0UB
0�B
0 B
0�B
1�B
2-B
2�B
3�B
49B
3�B
33B
3�B
3�B
3�B
3�B
3�B
3�B
3gB
33B
3gB
3gB
3gB
3�B
3�B
4B
5�B
6�B
6�B
6�B
6zB
6�B
6�B
6�B
6zB
6�B
6zB
6zB
6�B
6�B
7B
7B
7B
7B
7KB
7�B
7�B
8B
8�B
9#B
9XB
9XB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
;�B
<B
=B
=B
=pB
=<B
=pB
=pB
>BB
>wB
>wB
>�B
?B
?B
?}B
?HB
?}B
?}B
?�B
A B
A B
A�B
A�B
A�B
B�B
B[B
B[B
B�B
C,B
B�B
CaB
C�B
C�B
C�B
D3B
DgB
D3B
DgB
E9B
D�B
D�B
DgB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
DgB
EmB
FsB
GEB
GyB
G�B
G�B
G�B
G�B
G�B
HB
HB
HKB
H�B
H�B
H�B
H�B
IB
IQB
IQB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
J�B
J�B
J�B
K�B
K�B
K�B
L/B
L/B
L�B
MB
MjB
M�B
N�B
N�B
OB
OBB
OB
OBB
OvB
O�B
PB
PB
P�B
QNB
Q�B
Q�B
R�B
RTB
R�B
S&B
SZB
S�B
SZB
T,B
T,B
T`B
T�B
T,B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
V8B
VB
V8B
V�B
V�B
V�B
W�B
WsB
W�B
XB
XyB
X�B
YKB
YKB
YB
ZB
Z�B
[#B
[WB
[�B
[�B
\]B
\]B
\�B
]/B
]�B
^iB
^iB
^iB
^iB
^iB
^iB
^�B
^�B
^�B
_B
_B
_;B
_;B
_;B
_;B
_pB
_�B
_�B
_�B
_�B
_�B
`vB
aB
a�B
a�B
b�B
b�B
b�B
c�B
c�B
d%B
dZB
d�B
d�B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
f2B
f�B
f�B
gB
f�B
gB
hrB
h>B
h>B
h
B
g�B
h>B
hrB
h�B
iyB
iyB
iyB
iyB
iyB
i�B
iyB
j�B
j�B
kB
kB
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
m(B
m(B
m(B
m(B
m]B
m]B
m�B
m�B
n.B
ncB
n.B
n.B
ncB
n�B
o5B
o�B
o�B
o�B
o�B
o5B
oiB
o�B
pB
p;B
qAB
qAB
qAB
qAB
qAB
qAB
quB
quB
q�B
rGB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
u%B
u%B
u%B
u%B
uYB
uYB
uYB
u�B
u�B
u�B
v`B
v`B
v�B
v�B
w1B
w1B
wfB
wfB
w�B
w�B
w�B
w�B
xB
x7B
x7B
xlB
xlB
x�B
x�B
x�B
x�B
y>B
zB
zxB
z�B
z�B
z�B
zxB
z�B
z�B
{B
{JB
{B
{B
{B
{JB
{~B
{~B
{�B
|B
|B
|B
|B
|PB
|PB
|PB
|PB
|PB
|PB
|�B
|PB
|PB
|�B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~\B
~\B
~\B
~\B
~\B
~�B
b@�|�BF
BE9BD�BE�BGEBE�BF?BHBD3BFsBFsBD�BD�BHKBD�BE�BDgBF
BDgBE�BB�BD�BA�BC�BB�BE�B>wBB�BC�BC,BB�BEmBA�BA�BC�BA B@BA�BA BNB?�B:�B?BA B:^B@B=�B<�B;dB>�B<�BC,B>BB=<B:)B:�B;dB<jB>�B@�B:�B=<B>BB<B:)B;dB<�B<jB?�B=pB>�B?�B?HB>wB?}B?B=<B>�B=�B@�BA�BD�BEBHKBFsBF?BG�BGBHKBI�BG�BHBK)BI�BI�BK^BJ�BJ#BL�BK^BJ�BM5BL/BK^BM�BM�BK�BL�BNpBL/BM�BNpBL�BM�BOBMBNpBOvBM�BNpBO�BMjBOBO�BMjBOBN�BM�BPHBNpBNpBPBPBNBOBBQBM�BL�BNBK�BJ�BK�BK^BHKBHBL�BQBW�B[#Be,Bn�Bv+B��B��B�B�XB�XB�RB�0B�KB�jBٴB�WB��B��B�B�WB �B2�B1�B6�B:^B<6B>�B?HB>wBC,BOBBT`BVmB[WB`ABf2BkPBm�Br�Bs�By>B|�B}VBbB��B�B��B��B�B��B�qB�qB��B�*B��B��B�$B��B��B��B��B��B��B��B��B�BB�3B�3B��B� B��B��B��B��B��B��B�aB��B��B�-B�-B��B��B��B��B�)B��B��B��B��B��B��B�RB�BB�BB�XB�B��B�NB�?B��B�[B��B�QB��B��B��B�,B�&B��B��B��B��B��BěB��B��B�B�BB�B�jB�6B�#B�#B��B�B�B��B�B�mB��B��B��B�B��B��B��B��B��B��B�6B��B��B�<B��B�B��B�wB�[B�?B�?B��B� B��B��B�tB��B��B��B�gB�tB�B�9B�B��B�gB�9B�tB��B�B�B��B�B��B�KB��B��B�KB�B�tB�B��B��B��B�B��B�zB��B��B�zB��B��B��B��B��B��B�'B�gB�OB��B��B��B�UB��B��B� B�-B��B��B�wB� B��B��B��B�IB�kB�<B��B��B�9B��B�<B�-B�/BěB��B��B�kB�B��B�B��B�eB�B~�B~�B.B|PBzByrBxB{BxBx�BtBq�BrBrBpoBsMBr�BpoBncBoiBp;Bm�Bk�BiyBj�Bl�BjJBh�Bl�B`Ba�BaB^�B\�B\]B[�B^iBX�BW�BWsBS�BQ�BQ�BR�BL�BFsBJ�BI�B@NB;�BA B2�B-�B2-B0�B&LB!�BB.�BCB�B*B�B�B�BB	�B
	B�B
	BB�B:B iB��B�B��B�B�(B�cB�B�(B�B�B�B�B�]B�5B�B�B�B�B�B�B�
B��B�,B�,B�B�B�BߤB�]B��B�WB��B՛B� B�gB��B��B��B�ZB՛BɆBޞBȀB�KB�9B�B�3B��B��B�'B�B�}B�IB�}B��B�B�BB�B��B��B��B��B��B�RB��B�LB��B�RB��B��B�FB��B��B��B��B��B�nB�9B��B��B�*B�B��B�B�	B��B�%B��B|�Bp�Bs�BpoBo Br�Bp�Bw�Bm�BaBZ�BZ�B[WBYBZ�BY�BZ�BZ�B\�B\�BZB\�B^�Bb�B]cB\�B]cB_B]�Ba�Ba|BSZBRTBS�BS�BT,BX�BT�BW�BS�BOBR�BSZBO�BOvBU2BO�BJ�BIQBGyBF?BF?BGBE9BEBB�BB[BA�BC,BA�B@NBC�B<�B<�B>�BA�BA�B2�B4mB*�B)�B�B*B�B�B�B�B_B�BB�B�B�B�B�B	B�B�B�BB�B4B�B �B
��B
��B
�B
��B
��B
��B
�rB
��B
�uB
��B
��B
��B
��B
�B
�B
�B
��B
�B
��B
�cB
�B
��B
�>B
څB
�8B
�,B
�
B
��B
ݘB
֡B
��B
�B
�B
څB
�`B
�EB
�aB
�B
�<B
�dB
�XB
�B
��B
�mB
�mB
��B
��B
�B
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
��B
�B
�@B
��B
�-B
��B
�IB
�=B
��B
�7B
�0B
�{B
��B
�:B
��B
��B
��B
��B
�VB
�B
�=B
�B
�1B
�7B
�7B
�	B
��B
�IB
�4B
�:B
�+B
��B
��B
oiB
ffB
WsB
U�B
W>B
W�B
VmB
T�B
U2B
U�B
U2B
U�B
PHB
S&B
S�B
O�B
MB
JWB
J�B
H�B
F�B
F�B
F?B
g8B
M�B
=�B
:�B
<�B
0UB
?�B
3gB
!�B
#B
=B
qB
(XB
#�B	��B
�B	��B	�SB	�uB	��B	�B	�lB	��B	� B	�B	��B	��B	�B	�B	ݘB	�B	֡B	حB	�#B	�B	֡B	ٴB	�B	�8B	��B	�mB	��B	�&B	ӏB	�TB	��B	҉G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224959                            20230721224959AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495920230721224959  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495920230721224959QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495920230721224959QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               