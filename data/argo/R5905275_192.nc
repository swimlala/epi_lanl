CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:33:04Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230426223304  20230426223304  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @� �Jp@� �Jp11  @� �UUp @� �UUp @+�>WS�@+�>WS��c��n�Y�c��n�Y11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @�\@E�@�G�@��R@��R@�G�A   A\)A ��A-p�A@  A`  A�  A��A�Q�A���A��AϮA߮A�A��B  B  B  B (�B(  B/�
B7�
B@  BG�
BO�
BX(�B`(�Bh  BpQ�Bx(�B�  B�  B�{B�{B�{B�  B��B��
B��B�{B�{B��B��
B��
B��B��B�  B�{B�Q�B��B��B��B��B�  B�  B�  B�  B�  B�  B�  B��B��C   C  C
=C
=C  C
  C  C��C��C
=C
=C��C��C��C��C��C   C"  C$  C&  C(  C*  C,  C.  C/��C1��C3��C6
=C8
=C9��C<  C>
=C@
=CB
=CD
=CF  CH  CJ{CL
=CM��CP  CR
=CS��CV  CX
=CZ  C\  C^  C`  Cb
=Cd  Cf  Ch  Cj
=Cl  Cm��Cp
=Cr  Ct  Cv  Cx
=Cz
=C|
=C~
=C�C���C���C�C�C�  C�C�  C�  C�  C���C�C�  C�  C�
=C�C�C�  C���C�  C�  C�  C�C�
=C�  C�  C�  C�  C�C�C�  C���C�  C�C���C���C�  C���C���C�  C�C���C�  C�C���C�  C�  C���C�  C�  C�  C�  C���C�C�  C���C�
=C�C���C�  C�  C�  C�  C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C���C���C���C�  C�  C���C�  C�  C�C���C��C���C���C���C�C�
=C�C�  C�
=C�
=C�C�  C�  C�  C�  C�  C���C���C�C�C�  C�C�
=C�  C�  C���C�  C�C�  C�  C���C���C���C�C�  C�C�C���C�  C�C���C�  C�  C���D � D�D��DD� D�qD� D�D}qD  D� D�D�D  D}qD��D� D	D	� D
  D
��D
�qDz�D  D��D  D� D  D��D�qDz�D  D�D�D��D  D��D�D� D  D��D�D� D�D��D�D��D�D� D�qD� D�D��D�D��D  D� D  D}qD�qD� D�qD}qD   D � D!�D!��D"�D"��D#  D#��D$  D$}qD$�qD%� D&�D&��D'  D'� D(  D(� D)  D)��D*  D*}qD+  D+� D+�qD,}qD-�D-� D.  D.� D/  D/��D0�D0� D1  D1��D2  D2z�D2�qD3� D4  D4� D5�D5�D6  D6� D7  D7}qD8  D8� D8�qD9� D:�D:}qD:�qD;��D<  D<��D=D=��D>�D>�D?�D?� D@�D@��DA�DA� DB  DB� DC  DC� DC�qDD� DE  DE}qDE�qDF��DG�DG� DH  DH�DI  DI� DJ  DJz�DK  DK��DL�DL� DM  DM}qDM��DN}qDN�qDO� DP  DP� DQ�DQ}qDR  DR� DS  DS��DT  DT� DU�DU�DV  DVz�DV��DW}qDW�qDX� DY  DY� DZ�DZ� DZ�qD[� D\  D\}qD]  D]��D^�D^}qD^�qD_}qD`  D`��D`�qDa� Db�Db� Dc�Dc��Dd  Dd}qDd�qDez�Df  Df�Dg�Dg}qDg�qDh}qDi  Di��Dj  Dj}qDk  Dk}qDk�qDl��Dl�qDmz�Dn  Dn� Dn�qDo��Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt}qDu  Du��Dv  Dv}qDw�Dw�Dx�Dx��Dy  Dy}qDz  Dz� D{D{��D|  D|}qD|��D}� D~�D~� D~�qD}qD��D�>�D��HD�� D���D�@ D��HD�� D�  D�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHD��HD���D���D�>�D�}qD���D���D�>�D�� D���D���D�@ D�~�D���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�>�D�~�D�� D���D�=qD�~�D�� D���D�>�D�� D�D�HD�@ D��HD�� D���D�@ D��HD���D��qD�@ D��HD�� D���D�@ D�~�D���D�  D�>�D�~�D�� D�HD�@ D�� D�D�HD�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�� D��HD�HD�@ D��HD��HD�HD�>�D�~�D��qD��qD�@ D�� D�� D�HD�AHD�� D�� D���D�AHD��HD���D��qD�=qD�~�D���D��D�AHD�~�D��qD���D�>�D�~�D�� D�HD�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD��HD���D�@ D�� D�� D�  D�@ D��HD���D���D�B�D��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�@ D�� D���D�  D�AHD�� D���D�  D�@ D�� D��HD��D�B�D��HD�� D�  D�>�D��HD�D�HD�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D��HD�HD�B�D�� D���D���D�@ D��HD�� D��qD�@ D��HD���D��qD�>�D�� D�� D�HD�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�AHD��HD�� D�  D�@ D�� D�� D��D�C�D��HD���D�  D�AHD�~�D�� D�  D�@ D��HD���D��qD�@ D��HD���D�HD�@ D�� D��HD�  D�>�D�� D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�=qD�~�D�� D���D�>�D�~�D�� D�HD�@ DÀ D��HD�  D�@ DĀ D�� D�  D�AHDŀ D�� D�  D�AHDƁHDƾ�D���D�AHDǁHD��HD�HD�AHDȁHD�� D�  D�@ D�~�D�� D�HD�@ DʁHD��HD���D�@ D�~�D�� D�HD�@ D̀ D��HD���D�=qD̀ D��HD���D�=qD΀ D�� D�  D�@ Dπ D��HD�HD�@ D�}qDо�D���D�@ Dр D�� D�HD�AHDҁHD��HD�HD�AHDӂ�D�� D���D�@ DԀ D��HD�HD�AHDՀ D�� D�  D�>�D�~�D־�D�  D�@ D�}qD׾�D���D�>�D؀ D��HD�  D�@ D�~�Dپ�D�HD�C�Dڀ Dھ�D���D�>�Dۀ D�� D�  D�>�D܀ D�� D���D�@ D݂�D��HD�HD�AHDހ D�� D��D�B�D߀ D߽qD���D�@ D�~�D��HD�HD�=qD�}qD�� D��D�AHD�~�D�� D�  D�AHD� D�qD���D�>�D� D�� D���D�@ D� D徸D�HD�@ D� D�� D���D�>�D�~�D��HD�HD�AHD�HD辸D�  D�@ D�HD��HD���D�>�D� D�� D�HD�AHD낏D��HD�HD�@ D� D쾸D���D�@ D�HD�� D�HD�@ D�|)DD�  D�>�D�~�D��HD��D�AHD�� D�D�  D�B�D�D�D��D�AHD�HD��HD�HD�AHD�HD�� D�  D�@ D� D�� D�HD�B�D�� D���D���D�>�D�~�D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD�D���D�4{>�?#�
?k�?�=q?�{?��?�@�@
=@+�@333@G�@\(�@k�@u@�ff@�\)@�
=@��R@�ff@�33@��H@��
@˅@�@޸R@���@�\)@���A�AA
�HA�RAz�A��A��A!G�A&ffA+�A0  A4z�A8��A?\)AC�
AHQ�AMp�AS�
AX��A]p�Aa�Ag�Amp�Ar�\Aw
=A|��A���A�(�A��RA���A�z�A�\)A��A�(�A�
=A��A�z�A��RA�G�A�(�A�
=A���A��A�ffA�G�A��
A�{A���A�33A�{A�Q�A��HA�p�A�Q�A�33A��A�  A��HA�{A�Q�A��HA�p�A��A�33A�A�Q�A�\A�A��A�33A�A�Q�A��A�ffB z�B�B33B��B=qB�B��B
ffB�
BG�BffB�B�B�\B�B��B=qB�B�BffB�B��BffB�
B ��B"=qB#�
B%G�B&ffB'�B(��B*=qB+�B,��B.{B/\)B0��B2=qB3\)B4z�B5�B7\)B8��B9�B;
=B<Q�B=B?
=B@Q�BAp�BB�RBD  BEp�BF�\BG�BH��BJffBK�BL��BN=qBO\)BP��BRffBS�BT��BV=qBW�
BYG�BZ�\B[�
B]�B^�RB`(�Ba��Bb�RBd(�Be��Bg33BhQ�Bi��Bk
=Blz�Bn{Bo33Bpz�Bq�Bs�Bt��Bv{Bw\)Bx��Bz=qB{�B|��B~{B\)B�z�B�33B��
B�ffB��B��
B��\B�33B�B�z�B�G�B��B��\B�33B��
B��\B�G�B��B��\B�33B��B���B�G�B��
B��\B�33B�  B��\B��B�B�z�B�33B��
B�ffB�
=B�B�z�B��B�B�Q�B���B��B�ffB�
=B���B�=qB��HB��B�Q�B���B���B�=qB��HB���B�=qB���B��B�(�B��HB���B�Q�B���B��B�=qB���B��B�Q�B��HB���B�=qB�
=B�B�ffB���B��B�Q�B��B�B�ffB�
=B���B�=qB���B���B�Q�B��HB��B�{B���B�33B��
B��\B�33B��
B�z�B�
=B���B�=qB��HB���B�=qB���B�p�B��B�z�B�
=Bř�B�{Bƣ�B�
=B�p�B�B�{B�Q�Bȣ�B���B�33B�p�BɮB��
B�  B�{B�Q�Bʣ�B��HB��B�G�B�p�B˙�B��
B�(�B�z�B̸RB��HB��B�G�B͙�B��
B�=qBΏ\B��HB�33B�p�B�B�  B�Q�BУ�B���B�\)B�B�{B�ffBҸRB�
=B�\)BӮB�(�B�z�B���B�\)BծB�(�B�z�B��HB�33Bי�B�  B�z�B��HB�\)B�B�{Bڏ\B��HB�G�B�B�(�Bܣ�B��Bݙ�B��B�ffB޸RB�33Bߙ�B�{B��\B�
=B�B��B�Q�B��B��B㙚B�{B��B�
=B�B��B�ffB��HB�G�B�B�(�B��B��B�B�(�B�\B��B뙚B�  B�ffB��HB�\)B�B�Q�B���B�\)B��
B�Q�B���B�G�B�B�=qB��B�
=B�B�{B�\B�
=B���B�{B��\B�
=B��B�  B�ffB��HB�\)B��B�=qB��RB�33B��B�(�B��RB�33B��B�(�B��\B���B�p�B��C (�C ffC ��C ��C  C=qCz�C�C�HC�CQ�C�\CC��C33C\)C��C��C  C33CffC��C��C��C(�C\)C��CC��C(�CffC�\CC  C33C\)C�\CC  C33C\)C�\CC��C	(�C	Q�C	�\C	�RC	�C
(�C
Q�C
�C
�C
�HC{CG�Cz�C��C�
C
=C33Cp�C��C�
C
=C=qCp�C��C�
C
=C=qCp�C��C�
C
=C=qCp�C��C�
C
=C=qCp�C��C�
C
=C33CffC��C��C  C33CffC��C��C  C33CffC��C�
C
=C=qCp�C�C�C{CG�C�C�RC��C(�C\)C��C��C  C=qCp�C��C�HC{CG�C�C�RC�C�CQ�C�C��C  C=qCp�C�C�HC�CQ�C�\CC��C33Cp�C��C�C�C\)C�\C��C  C=qCp�C�C�C�C\)C��C��C 
=C G�C z�C �RC �C!�C!Q�C!�\C!C!��C"33C"p�C"�C"�HC#�C#\)C#��C#��C$
=C$G�C$�C$C%  C%=qC%z�C%�RC%��C&33C&ffC&��C&�HC'{C'Q�C'�\C'C(
=C(G�C(�C(C(��C)=qC)z�C)�RC)�C*(�C*ffC*�C*�C+(�C+ffC+��C+�C,(�C,p�C,�RC-  C-=qC-�C-C.  C.33C.z�C.C/
=C/G�C/��C/�HC0�C0\)C0��C0�HC1�C1ffC1�C1��C2=qC2�C2�
C3{C3Q�C3�\C3��C4{C4\)C4�C4��C5=qC5�C5C6  C6G�C6�\C6��C7�C7p�C7�RC8  C8=qC8z�C8C9
=C9Q�C9��C9�C:33C:z�C:C;
=C;G�C;�\C;��C<�C<ffC<�C=  C=G�C=�\C=�
C>{C>\)C>��C>�C?33C?�C?�
C@�C@ffC@�C@��CA=qCA�CA�
CB�CBffCB�RCC  CCG�CC�CC��CD{CDQ�CD�CD��CEG�CE��CE�HCF(�CFp�CF�RCG
=CG\)CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                          ?�  @�\@E�@�G�@��R@��R@�G�A   A\)A ��A-p�A@  A`  A�  A��A�Q�A���A��AϮA߮A�A��B  B  B  B (�B(  B/�
B7�
B@  BG�
BO�
BX(�B`(�Bh  BpQ�Bx(�B�  B�  B�{B�{B�{B�  B��B��
B��B�{B�{B��B��
B��
B��B��B�  B�{B�Q�B��B��B��B��B�  B�  B�  B�  B�  B�  B�  B��B��C   C  C
=C
=C  C
  C  C��C��C
=C
=C��C��C��C��C��C   C"  C$  C&  C(  C*  C,  C.  C/��C1��C3��C6
=C8
=C9��C<  C>
=C@
=CB
=CD
=CF  CH  CJ{CL
=CM��CP  CR
=CS��CV  CX
=CZ  C\  C^  C`  Cb
=Cd  Cf  Ch  Cj
=Cl  Cm��Cp
=Cr  Ct  Cv  Cx
=Cz
=C|
=C~
=C�C���C���C�C�C�  C�C�  C�  C�  C���C�C�  C�  C�
=C�C�C�  C���C�  C�  C�  C�C�
=C�  C�  C�  C�  C�C�C�  C���C�  C�C���C���C�  C���C���C�  C�C���C�  C�C���C�  C�  C���C�  C�  C�  C�  C���C�C�  C���C�
=C�C���C�  C�  C�  C�  C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C���C���C���C�  C�  C���C�  C�  C�C���C��C���C���C���C�C�
=C�C�  C�
=C�
=C�C�  C�  C�  C�  C�  C���C���C�C�C�  C�C�
=C�  C�  C���C�  C�C�  C�  C���C���C���C�C�  C�C�C���C�  C�C���C�  C�  C���D � D�D��DD� D�qD� D�D}qD  D� D�D�D  D}qD��D� D	D	� D
  D
��D
�qDz�D  D��D  D� D  D��D�qDz�D  D�D�D��D  D��D�D� D  D��D�D� D�D��D�D��D�D� D�qD� D�D��D�D��D  D� D  D}qD�qD� D�qD}qD   D � D!�D!��D"�D"��D#  D#��D$  D$}qD$�qD%� D&�D&��D'  D'� D(  D(� D)  D)��D*  D*}qD+  D+� D+�qD,}qD-�D-� D.  D.� D/  D/��D0�D0� D1  D1��D2  D2z�D2�qD3� D4  D4� D5�D5�D6  D6� D7  D7}qD8  D8� D8�qD9� D:�D:}qD:�qD;��D<  D<��D=D=��D>�D>�D?�D?� D@�D@��DA�DA� DB  DB� DC  DC� DC�qDD� DE  DE}qDE�qDF��DG�DG� DH  DH�DI  DI� DJ  DJz�DK  DK��DL�DL� DM  DM}qDM��DN}qDN�qDO� DP  DP� DQ�DQ}qDR  DR� DS  DS��DT  DT� DU�DU�DV  DVz�DV��DW}qDW�qDX� DY  DY� DZ�DZ� DZ�qD[� D\  D\}qD]  D]��D^�D^}qD^�qD_}qD`  D`��D`�qDa� Db�Db� Dc�Dc��Dd  Dd}qDd�qDez�Df  Df�Dg�Dg}qDg�qDh}qDi  Di��Dj  Dj}qDk  Dk}qDk�qDl��Dl�qDmz�Dn  Dn� Dn�qDo��Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt}qDu  Du��Dv  Dv}qDw�Dw�Dx�Dx��Dy  Dy}qDz  Dz� D{D{��D|  D|}qD|��D}� D~�D~� D~�qD}qD��D�>�D��HD�� D���D�@ D��HD�� D�  D�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHD��HD���D���D�>�D�}qD���D���D�>�D�� D���D���D�@ D�~�D���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�>�D�~�D�� D���D�=qD�~�D�� D���D�>�D�� D�D�HD�@ D��HD�� D���D�@ D��HD���D��qD�@ D��HD�� D���D�@ D�~�D���D�  D�>�D�~�D�� D�HD�@ D�� D�D�HD�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�� D��HD�HD�@ D��HD��HD�HD�>�D�~�D��qD��qD�@ D�� D�� D�HD�AHD�� D�� D���D�AHD��HD���D��qD�=qD�~�D���D��D�AHD�~�D��qD���D�>�D�~�D�� D�HD�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD��HD���D�@ D�� D�� D�  D�@ D��HD���D���D�B�D��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�@ D�� D���D�  D�AHD�� D���D�  D�@ D�� D��HD��D�B�D��HD�� D�  D�>�D��HD�D�HD�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D��HD�HD�B�D�� D���D���D�@ D��HD�� D��qD�@ D��HD���D��qD�>�D�� D�� D�HD�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�AHD��HD�� D�  D�@ D�� D�� D��D�C�D��HD���D�  D�AHD�~�D�� D�  D�@ D��HD���D��qD�@ D��HD���D�HD�@ D�� D��HD�  D�>�D�� D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�=qD�~�D�� D���D�>�D�~�D�� D�HD�@ DÀ D��HD�  D�@ DĀ D�� D�  D�AHDŀ D�� D�  D�AHDƁHDƾ�D���D�AHDǁHD��HD�HD�AHDȁHD�� D�  D�@ D�~�D�� D�HD�@ DʁHD��HD���D�@ D�~�D�� D�HD�@ D̀ D��HD���D�=qD̀ D��HD���D�=qD΀ D�� D�  D�@ Dπ D��HD�HD�@ D�}qDо�D���D�@ Dр D�� D�HD�AHDҁHD��HD�HD�AHDӂ�D�� D���D�@ DԀ D��HD�HD�AHDՀ D�� D�  D�>�D�~�D־�D�  D�@ D�}qD׾�D���D�>�D؀ D��HD�  D�@ D�~�Dپ�D�HD�C�Dڀ Dھ�D���D�>�Dۀ D�� D�  D�>�D܀ D�� D���D�@ D݂�D��HD�HD�AHDހ D�� D��D�B�D߀ D߽qD���D�@ D�~�D��HD�HD�=qD�}qD�� D��D�AHD�~�D�� D�  D�AHD� D�qD���D�>�D� D�� D���D�@ D� D徸D�HD�@ D� D�� D���D�>�D�~�D��HD�HD�AHD�HD辸D�  D�@ D�HD��HD���D�>�D� D�� D�HD�AHD낏D��HD�HD�@ D� D쾸D���D�@ D�HD�� D�HD�@ D�|)DD�  D�>�D�~�D��HD��D�AHD�� D�D�  D�B�D�D�D��D�AHD�HD��HD�HD�AHD�HD�� D�  D�@ D� D�� D�HD�B�D�� D���D���D�>�D�~�D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD�D���D�4{>�?#�
?k�?�=q?�{?��?�@�@
=@+�@333@G�@\(�@k�@u@�ff@�\)@�
=@��R@�ff@�33@��H@��
@˅@�@޸R@���@�\)@���A�AA
�HA�RAz�A��A��A!G�A&ffA+�A0  A4z�A8��A?\)AC�
AHQ�AMp�AS�
AX��A]p�Aa�Ag�Amp�Ar�\Aw
=A|��A���A�(�A��RA���A�z�A�\)A��A�(�A�
=A��A�z�A��RA�G�A�(�A�
=A���A��A�ffA�G�A��
A�{A���A�33A�{A�Q�A��HA�p�A�Q�A�33A��A�  A��HA�{A�Q�A��HA�p�A��A�33A�A�Q�A�\A�A��A�33A�A�Q�A��A�ffB z�B�B33B��B=qB�B��B
ffB�
BG�BffB�B�B�\B�B��B=qB�B�BffB�B��BffB�
B ��B"=qB#�
B%G�B&ffB'�B(��B*=qB+�B,��B.{B/\)B0��B2=qB3\)B4z�B5�B7\)B8��B9�B;
=B<Q�B=B?
=B@Q�BAp�BB�RBD  BEp�BF�\BG�BH��BJffBK�BL��BN=qBO\)BP��BRffBS�BT��BV=qBW�
BYG�BZ�\B[�
B]�B^�RB`(�Ba��Bb�RBd(�Be��Bg33BhQ�Bi��Bk
=Blz�Bn{Bo33Bpz�Bq�Bs�Bt��Bv{Bw\)Bx��Bz=qB{�B|��B~{B\)B�z�B�33B��
B�ffB��B��
B��\B�33B�B�z�B�G�B��B��\B�33B��
B��\B�G�B��B��\B�33B��B���B�G�B��
B��\B�33B�  B��\B��B�B�z�B�33B��
B�ffB�
=B�B�z�B��B�B�Q�B���B��B�ffB�
=B���B�=qB��HB��B�Q�B���B���B�=qB��HB���B�=qB���B��B�(�B��HB���B�Q�B���B��B�=qB���B��B�Q�B��HB���B�=qB�
=B�B�ffB���B��B�Q�B��B�B�ffB�
=B���B�=qB���B���B�Q�B��HB��B�{B���B�33B��
B��\B�33B��
B�z�B�
=B���B�=qB��HB���B�=qB���B�p�B��B�z�B�
=Bř�B�{Bƣ�B�
=B�p�B�B�{B�Q�Bȣ�B���B�33B�p�BɮB��
B�  B�{B�Q�Bʣ�B��HB��B�G�B�p�B˙�B��
B�(�B�z�B̸RB��HB��B�G�B͙�B��
B�=qBΏ\B��HB�33B�p�B�B�  B�Q�BУ�B���B�\)B�B�{B�ffBҸRB�
=B�\)BӮB�(�B�z�B���B�\)BծB�(�B�z�B��HB�33Bי�B�  B�z�B��HB�\)B�B�{Bڏ\B��HB�G�B�B�(�Bܣ�B��Bݙ�B��B�ffB޸RB�33Bߙ�B�{B��\B�
=B�B��B�Q�B��B��B㙚B�{B��B�
=B�B��B�ffB��HB�G�B�B�(�B��B��B�B�(�B�\B��B뙚B�  B�ffB��HB�\)B�B�Q�B���B�\)B��
B�Q�B���B�G�B�B�=qB��B�
=B�B�{B�\B�
=B���B�{B��\B�
=B��B�  B�ffB��HB�\)B��B�=qB��RB�33B��B�(�B��RB�33B��B�(�B��\B���B�p�B��C (�C ffC ��C ��C  C=qCz�C�C�HC�CQ�C�\CC��C33C\)C��C��C  C33CffC��C��C��C(�C\)C��CC��C(�CffC�\CC  C33C\)C�\CC  C33C\)C�\CC��C	(�C	Q�C	�\C	�RC	�C
(�C
Q�C
�C
�C
�HC{CG�Cz�C��C�
C
=C33Cp�C��C�
C
=C=qCp�C��C�
C
=C=qCp�C��C�
C
=C=qCp�C��C�
C
=C=qCp�C��C�
C
=C33CffC��C��C  C33CffC��C��C  C33CffC��C�
C
=C=qCp�C�C�C{CG�C�C�RC��C(�C\)C��C��C  C=qCp�C��C�HC{CG�C�C�RC�C�CQ�C�C��C  C=qCp�C�C�HC�CQ�C�\CC��C33Cp�C��C�C�C\)C�\C��C  C=qCp�C�C�C�C\)C��C��C 
=C G�C z�C �RC �C!�C!Q�C!�\C!C!��C"33C"p�C"�C"�HC#�C#\)C#��C#��C$
=C$G�C$�C$C%  C%=qC%z�C%�RC%��C&33C&ffC&��C&�HC'{C'Q�C'�\C'C(
=C(G�C(�C(C(��C)=qC)z�C)�RC)�C*(�C*ffC*�C*�C+(�C+ffC+��C+�C,(�C,p�C,�RC-  C-=qC-�C-C.  C.33C.z�C.C/
=C/G�C/��C/�HC0�C0\)C0��C0�HC1�C1ffC1�C1��C2=qC2�C2�
C3{C3Q�C3�\C3��C4{C4\)C4�C4��C5=qC5�C5C6  C6G�C6�\C6��C7�C7p�C7�RC8  C8=qC8z�C8C9
=C9Q�C9��C9�C:33C:z�C:C;
=C;G�C;�\C;��C<�C<ffC<�C=  C=G�C=�\C=�
C>{C>\)C>��C>�C?33C?�C?�
C@�C@ffC@�C@��CA=qCA�CA�
CB�CBffCB�RCC  CCG�CC�CC��CD{CDQ�CD�CD��CEG�CE��CE�HCF(�CFp�CF�RCG
=CG\)CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aω7AϋDAϋDAϋDAϋDAϋDAύPAύPAϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϑhAϑhAϓuAϓuAϕ�Aϕ�Aϕ�Aϗ�Aϗ�Aϗ�Aϗ�Aϙ�Aϗ�Aϗ�Aϙ�Aϙ�Aϕ�Aω7Aχ+AσA�r�A�dZA�`BA�dZA�VA�
=A�z�AˋDA�z�A�C�A���A�;dA�C�A�bA��A��9A���A���A��A���A�ffA��uA�K�A���A�+A�9XA���A�A�A��A�t�A�A�z�A�z�A���A��jA�Q�A�O�A���A�A�M�A�VA��7A���A�  A�A�x�A�t�A�C�A��PA��DA��A}��Ay�wAu�
Aol�AjJAe�Ac`BA`�uA["�AV�uAUXAT�uAS;dANbAJ��AG?}A@Q�A;��A:bA8v�A733A5VA4�A3�A3�^A4�9A5��A6�\A7&�A7�
A8A�A8n�A7�#A4�A3�A1?}A0VA/O�A.^5A+�
A*1'A)�TA)��A)�A)K�A)
=A(�A(Q�A'�TA'x�A'�A&��A&{A%A$r�A${A#XA#oA"�A"M�A!��A!\)A!/A!A �jA bNA 9XA 1A��A��A��A&�A�#A��Az�A��AA��A�+AQ�A�AS�A�A1'AAp�AC�AȴAjAZA�AXA�A��AZAJAAx�A33A��Ar�A^5AM�A9XAAA�FA�Ap�A`BA�A��AjA=qA�AJA��A"�A�A�`A�RA~�AAK�A&�A��A
=AȴA1AO�AK�A��A��A\)A
�A
�9A
�\A
ffA	�TA	\)A	"�A	AĜA~�AffAM�A �A�mA�wA?}AoA�AZA�A  A�AƨA��A\)A�AĜA�A=qA�
A"�A^5A��A�A r�@��
@�ȴ@���@��h@�7L@���@��u@�\)@��\@�{@��@��u@�ƨ@�S�@�ff@���@�?}@��@�b@�@�\)@�+@�@�@�@�7@�&�@���@�33@�!@���@�D@�l�@�;d@�~�@��@��@�@��@�@�&�@���@�z�@�Z@�
=@��@�V@�j@� �@ߥ�@��@�33@���@�7L@ܛ�@�1@�K�@���@�ȴ@ڇ+@��@�p�@���@��m@ץ�@֗�@��T@ՙ�@�Ĝ@ԃ@��@�|�@�@��T@��/@У�@�r�@�I�@�1'@�\)@θR@�^5@���@�x�@̋D@�C�@ʏ\@��@ɑh@��@�Q�@Ǿw@�
=@Ɵ�@Ƈ+@�J@�X@ēu@�  @�t�@�+@+@�$�@���@��`@�A�@��@��
@�l�@��+@�5?@��#@��7@�O�@���@�1'@���@�33@���@��R@�-@��7@�G�@�V@��D@��@��@�S�@�o@��@�ff@�E�@��#@��@�Ĝ@���@�@�E�@��@���@��7@�x�@�X@��/@��@�A�@�  @�\)@�V@��#@�O�@�G�@�&�@��/@��@�  @�S�@���@�^5@��@��@���@��-@�hs@��@�bN@�1@��w@�t�@��@�ȴ@���@��+@�^5@�-@��@�J@��T@���@��@�hs@��@��@��@���@�\)@�;d@���@��@���@�v�@�5?@���@���@�x�@�X@���@��u@� �@���@�|�@�@���@��+@�M�@��#@�hs@�7L@���@��@�bN@��@�1@�  @���@��;@�ƨ@���@�l�@�+@�o@��y@���@��R@���@�v�@��@�hs@��`@���@�9X@��w@���@���@�n�@�=q@��@��-@�`B@��@���@���@��D@�z�@� �@��w@���@�+@��@���@�-@���@���@�G�@��@���@��D@�z�@�r�@�bN@�Q�@�1'@�  @���@��@��P@�S�@�+@��@��y@�ff@��#@���@�@�@��-@���@�p�@�?}@���@�z�@� �@�1@��;@��w@��F@���@�|�@�;d@�@��@��H@���@��+@�5?@�@��7@��7@��@�&�@��j@��@�Z@�1@��F@�\)@�K�@�"�@��H@���@�M�@�{@��@���@���@�`B@���@��/@��9@�z�@�  @���@���@�S�@���@��R@�ff@�$�@�@��@���@�X@�?}@�%@��9@��@�1'@�w@�P@~�@~5?@}�-@}V@|�j@|9X@{�
@{�@{o@z�H@z��@z�@y�^@yhs@y&�@x�9@xbN@xA�@xA�@xb@w�;@w��@v�y@vV@u�T@u�-@t��@t�@t�@s�@sC�@so@r�H@r~�@r�@q�@q�^@q�^@qx�@p�`@pA�@pb@p  @o��@o�@ol�@o\)@n��@nV@m�@m�-@m/@l�/@l�j@l��@lj@k��@kC�@j�H@j=q@i�^@ihs@i&�@h��@hA�@gl�@f��@fE�@e@ep�@d��@dz�@c��@cC�@c@bn�@b-@a��@a7L@`��@`Q�@_l�@^V@^5?@]�@]��@]�h@]?}@]�@\��@\�j@\j@[ƨ@[dZ@Z^5@Y�#@Yx�@Y7L@XĜ@XA�@W|�@W;d@W�@V��@V�@VV@Up�@T��@TI�@S�m@S��@SC�@R��@R�!@R~�@R^5@Q��@Qhs@P��@P �@P �@O�;@O+@N�R@Nff@M?}@LZ@K�
@K"�@J��@J�\@J^5@J-@I��@Ihs@IX@I7L@I%@H��@H�`@H�9@Hr�@Hb@G�;@Gl�@Fȴ@F�+@Fv�@F@E��@E?}@D�D@D9X@C��@C�m@C�F@CdZ@Co@B�H@B^5@A��@A�^@A��@AX@A%@@�@@Q�@@  @?�@?�P@>�@>E�@>@=p�@=O�@=�@<��@<�j@<�j@<�@<z�@<1@;�@;33@:�H@:��@:=q@9��@9�7@9�@8�9@8bN@7�;@7�;@7��@7;d@7�@7
=@6��@5�@5�-@5�@5�@4�@41@3�@3C�@2�@2�!@2�\@2-@1��@1�7@1X@1%@0 �@/��@/|�@.�+@.E�@.{@-�T@-�@,�j@,�j@,�@,��@,z�@,�@+�m@+�@+33@+o@*�!@*^5@*-@*J@)��@)��@)��@)��@)x�@)G�@)�@(��@(�@(A�@'��@'+@&��@&5?@&@%��@%��@%�@%/@%V@$��@$�j@$j@#ƨ@#"�@"�!@"^5@!��@!hs@!G�@!�@ ��@ bN@ A�@   @�@;d@�@�R@v�@{@�h@?}@�@�/@�j@�@��@I�@�@��@�m@ƨ@dZ@@�@�@�H@n�@J@�#@�^@x�@%@Ĝ@�u@bN@A�@b@�;@|�@;d@�y@��@@p�@�@�@��@��@�D@j@Z@Z@Z@I�@�@��@��@�@t�@o@�!@n�@=q@J@��@�@�#@��@G�@7L@7L@&�@%@��@��@�u@r�@r�@bN@b@�;@�P@\)@K�@�@��@�y@�@ȴ@�R@��@v�@E�@@@�h@p�@O�@?}@/@��@��@��@�@��@�j@�@��@�D@j@j@Z@Z@I�@�@1@�
@��@33@
�@
��@
��@
~�@
n�@
n�@
n�@
=q@
�@	��@	�@	�@	��@	X@	�@	%@��@�`@�`@��@Ĝ@�9@�9Aχ+Aω7Aω7AϋDAϋDAω7Aω7AύPAύPAω7AϋDAύPAύPAω7Aω7AύPAύPAω7Aχ+AϋDAύPAϋDAω7AύPAϏ\AϋDAω7AϋDAϏ\AϏ\AϋDAϋDAϏ\AϑhAϏ\AύPAϏ\AϑhAϑhAύPAύPAϑhAϏ\AύPAύPAϏ\AϑhAϏ\AύPAϏ\AϏ\AϏ\AύPAύPAϑhAϏ\AύPAύPAϑhAϑhAϏ\AύPAϏ\AϑhAϑhAύPAύPAϑhAϑhAϑhAύPAύPAϑhAϓuAϏ\AύPAϑhAϓuAϑhAϏ\AϑhAϓuAϓuAϏ\AϏ\AϓuAϕ�AϓuAϑhAϑhAϓuAϕ�Aϕ�AϑhAϏ\AϓuAϕ�Aϕ�AϑhAϑhAϕ�Aϗ�Aϕ�AϑhAϓuAϕ�Aϗ�Aϗ�AϓuAϓuAϗ�Aϕ�AϑhAϑhAϕ�Aϗ�Aϗ�AϓuAϓuAϗ�Aϗ�Aϗ�AϓuAϕ�Aϗ�Aϙ�Aϕ�AϓuAϗ�Aϙ�Aϗ�Aϕ�Aϕ�Aϙ�Aϙ�Aϗ�Aϕ�Aϕ�Aϙ�Aϛ�Aϙ�Aϗ�Aϗ�Aϛ�Aϛ�Aϙ�Aϕ�Aϕ�Aϙ�Aϛ�Aϙ�Aϗ�Aϕ�Aϕ�Aϙ�Aϙ�Aϕ�Aϕ�Aϙ�Aϛ�Aϙ�Aϗ�Aϗ�Aϙ�Aϝ�Aϛ�Aϙ�Aϙ�Aϙ�Aϛ�Aϛ�Aϗ�AϓuAϗ�Aϙ�Aϗ�Aϗ�AϑhAϑhAϑhAϏ\Aχ+AσAσA�|�Aω7AυAυAϏ\AϋDAχ+AσAω7A�t�AσA�~�A�~�A�~�A�p�A�n�A�hsA�dZA�ffA�jA�jA�ffA�XA�XA�ZA�\)A�`BA�`BA�dZA�ffA�ffA�bNA�`BA�dZA�dZA�hsA�jA�S�A�O�A�VA�O�A�K�A�E�A�7LA�"�A��A��
AάA�hsA�ĜA͑hA�O�A��`A�M�A�$�A��A˙�A�Q�A��A���Aʛ�A�(�AɼjA�Q�A���A�I�A��HAǾwAǡ�A�hsA���A�t�A�VA�C�A� �A��/Aŏ\A�{AĶFA�t�A�E�A�$�A�A���Aò-A�n�A�/A��A�A��A�ĜA�C�A��`A��^A��-A��A���A���A��\A�~�A�n�A�bNA�G�A�5?A� �A��TA���A�"�A��A��A���A�XA�ffA�=qA�bNA���A�;dA�  A��A��!A�p�A�O�A��^A���A��A�=qA��wA���A���A�M�A�A��DA�A��DA�/A�  A���A��uA�G�A��TA�x�A�=qA���A�ƨA���A�p�A�-A���A�|�A�9XA��A��
A�Q�A��HA��hA���A���A�x�A��/A�5?A���A�/A�A��yA���A��jA��A���A���A��\A��PA��DA��DA��DA��+A�~�A�t�A�l�A�hsA�dZA�S�A�C�A�?}A�7LA�1'A��A�
=A��A���A��^A���A���A��PA�p�A�S�A�O�A�M�A�E�A�C�A�?}A�(�A��A�bA�A���A��A��;A���A��7A�&�A���A���A��DA�^5A�A�A�/A��A�VA���A��TA�ȴA��!A���A�x�A�\)A�S�A�7LA�+A��A�bA�1A�
=A���A���A���A��A��hA�E�A�r�A���A���A��A�n�A�ffA�`BA�^5A�\)A�S�A�G�A��A�A��A��`A��HA��;A��A���A��9A���A���A�~�A�`BA�;dA�%A��;A���A�A���A�p�A�ZA�E�A�E�A��A�1A�  A���A��A��`A��
A���A��-A���A��+A�dZA�5?A�
=A��A���A���A�p�A�K�A� �A�1A���A��mA��jA�|�A�K�A�-A�{A�  A���A�x�A�|�A�|�A�|�A�|�A�z�A�p�A�Q�A�7LA��yA��uA�M�A�VA���A��yA���A��-A��A���A��PA�p�A�I�A��!A�1'A�
=A���A��yA���A�ĜA���A�Q�A�A���A���A�p�A�^5A�O�A��A��TA�A��^A��!A���A��DA�`BA�?}A��A���A��+A�^5A�?}A�-A��A���A��#A�ȴA��wA���A�E�A��A�A�n�A��A�A���A��hA��+A��A�ffA�bNA�`BA�ZA�1'A��A�oA�
=A���A��A���A���A�z�A�hsA�p�A�jA�v�A�I�A��yA�`BA�;dA��A�
=A���A��A��DA�S�A�K�A�/A��A�ƨA��7A�A�A�&�A��#A�~�A�33A��/A�XA��A��A���A�Q�A�C�A�5?A��A���A��A���A�z�A�9XA��A��RA��7A�VA�/A��mA���A���A��A�bA���A���A�^5A��mA���A�=qA���A��uA��hA��A�\)A��A��!A�~�A�  A+A~�A~jA}�;A}�hA};dA|��A{��A{hsAz�HAz=qAy�TAy��Ay�7Ay"�Ax9XAw��AwC�Aw+Av��Av��AvZAu�Asp�Arn�Aq�Ap��Apn�ApM�AoXAnZAmXAljAk�hAkS�Ak�Aj~�Ai�Ahv�Ag�Ag��Ag`BAf�Af  Ae�7Ae�Ad�9Adn�Ad-Ac�Ac�^Acx�Ac&�AbȴAbn�Ab{Aa��Aa�A`�9A`ZA`bA_�-A^�HA^n�A]�A]�A[S�AX��AXVAX  AW��AWK�AWoAV�jAVA�AU��AUAUAU�wAU�AU�AU33AUATĜAT��AT��AT�AT��AT��AT�\ATv�ATM�ATE�AT�AS��ASS�ASAR�AQ/AP�jAP1'AN(�AL��AL��ALjAL{AK�wAK�PAKO�AKVAJ��AJ �AJ  AIhsAH�AG��AF��AF$�AE�AEVAC�AB  A@-A>�HA>��A>ZA=��A<��A<�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                          Aω7AϋDAϋDAϋDAϋDAϋDAύPAύPAϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϏ\AϑhAϑhAϓuAϓuAϕ�Aϕ�Aϕ�Aϗ�Aϗ�Aϗ�Aϗ�Aϙ�Aϗ�Aϗ�Aϙ�Aϙ�Aϕ�Aω7Aχ+AσA�r�A�dZA�`BA�dZA�VA�
=A�z�AˋDA�z�A�C�A���A�;dA�C�A�bA��A��9A���A���A��A���A�ffA��uA�K�A���A�+A�9XA���A�A�A��A�t�A�A�z�A�z�A���A��jA�Q�A�O�A���A�A�M�A�VA��7A���A�  A�A�x�A�t�A�C�A��PA��DA��A}��Ay�wAu�
Aol�AjJAe�Ac`BA`�uA["�AV�uAUXAT�uAS;dANbAJ��AG?}A@Q�A;��A:bA8v�A733A5VA4�A3�A3�^A4�9A5��A6�\A7&�A7�
A8A�A8n�A7�#A4�A3�A1?}A0VA/O�A.^5A+�
A*1'A)�TA)��A)�A)K�A)
=A(�A(Q�A'�TA'x�A'�A&��A&{A%A$r�A${A#XA#oA"�A"M�A!��A!\)A!/A!A �jA bNA 9XA 1A��A��A��A&�A�#A��Az�A��AA��A�+AQ�A�AS�A�A1'AAp�AC�AȴAjAZA�AXA�A��AZAJAAx�A33A��Ar�A^5AM�A9XAAA�FA�Ap�A`BA�A��AjA=qA�AJA��A"�A�A�`A�RA~�AAK�A&�A��A
=AȴA1AO�AK�A��A��A\)A
�A
�9A
�\A
ffA	�TA	\)A	"�A	AĜA~�AffAM�A �A�mA�wA?}AoA�AZA�A  A�AƨA��A\)A�AĜA�A=qA�
A"�A^5A��A�A r�@��
@�ȴ@���@��h@�7L@���@��u@�\)@��\@�{@��@��u@�ƨ@�S�@�ff@���@�?}@��@�b@�@�\)@�+@�@�@�@�7@�&�@���@�33@�!@���@�D@�l�@�;d@�~�@��@��@�@��@�@�&�@���@�z�@�Z@�
=@��@�V@�j@� �@ߥ�@��@�33@���@�7L@ܛ�@�1@�K�@���@�ȴ@ڇ+@��@�p�@���@��m@ץ�@֗�@��T@ՙ�@�Ĝ@ԃ@��@�|�@�@��T@��/@У�@�r�@�I�@�1'@�\)@θR@�^5@���@�x�@̋D@�C�@ʏ\@��@ɑh@��@�Q�@Ǿw@�
=@Ɵ�@Ƈ+@�J@�X@ēu@�  @�t�@�+@+@�$�@���@��`@�A�@��@��
@�l�@��+@�5?@��#@��7@�O�@���@�1'@���@�33@���@��R@�-@��7@�G�@�V@��D@��@��@�S�@�o@��@�ff@�E�@��#@��@�Ĝ@���@�@�E�@��@���@��7@�x�@�X@��/@��@�A�@�  @�\)@�V@��#@�O�@�G�@�&�@��/@��@�  @�S�@���@�^5@��@��@���@��-@�hs@��@�bN@�1@��w@�t�@��@�ȴ@���@��+@�^5@�-@��@�J@��T@���@��@�hs@��@��@��@���@�\)@�;d@���@��@���@�v�@�5?@���@���@�x�@�X@���@��u@� �@���@�|�@�@���@��+@�M�@��#@�hs@�7L@���@��@�bN@��@�1@�  @���@��;@�ƨ@���@�l�@�+@�o@��y@���@��R@���@�v�@��@�hs@��`@���@�9X@��w@���@���@�n�@�=q@��@��-@�`B@��@���@���@��D@�z�@� �@��w@���@�+@��@���@�-@���@���@�G�@��@���@��D@�z�@�r�@�bN@�Q�@�1'@�  @���@��@��P@�S�@�+@��@��y@�ff@��#@���@�@�@��-@���@�p�@�?}@���@�z�@� �@�1@��;@��w@��F@���@�|�@�;d@�@��@��H@���@��+@�5?@�@��7@��7@��@�&�@��j@��@�Z@�1@��F@�\)@�K�@�"�@��H@���@�M�@�{@��@���@���@�`B@���@��/@��9@�z�@�  @���@���@�S�@���@��R@�ff@�$�@�@��@���@�X@�?}@�%@��9@��@�1'@�w@�P@~�@~5?@}�-@}V@|�j@|9X@{�
@{�@{o@z�H@z��@z�@y�^@yhs@y&�@x�9@xbN@xA�@xA�@xb@w�;@w��@v�y@vV@u�T@u�-@t��@t�@t�@s�@sC�@so@r�H@r~�@r�@q�@q�^@q�^@qx�@p�`@pA�@pb@p  @o��@o�@ol�@o\)@n��@nV@m�@m�-@m/@l�/@l�j@l��@lj@k��@kC�@j�H@j=q@i�^@ihs@i&�@h��@hA�@gl�@f��@fE�@e@ep�@d��@dz�@c��@cC�@c@bn�@b-@a��@a7L@`��@`Q�@_l�@^V@^5?@]�@]��@]�h@]?}@]�@\��@\�j@\j@[ƨ@[dZ@Z^5@Y�#@Yx�@Y7L@XĜ@XA�@W|�@W;d@W�@V��@V�@VV@Up�@T��@TI�@S�m@S��@SC�@R��@R�!@R~�@R^5@Q��@Qhs@P��@P �@P �@O�;@O+@N�R@Nff@M?}@LZ@K�
@K"�@J��@J�\@J^5@J-@I��@Ihs@IX@I7L@I%@H��@H�`@H�9@Hr�@Hb@G�;@Gl�@Fȴ@F�+@Fv�@F@E��@E?}@D�D@D9X@C��@C�m@C�F@CdZ@Co@B�H@B^5@A��@A�^@A��@AX@A%@@�@@Q�@@  @?�@?�P@>�@>E�@>@=p�@=O�@=�@<��@<�j@<�j@<�@<z�@<1@;�@;33@:�H@:��@:=q@9��@9�7@9�@8�9@8bN@7�;@7�;@7��@7;d@7�@7
=@6��@5�@5�-@5�@5�@4�@41@3�@3C�@2�@2�!@2�\@2-@1��@1�7@1X@1%@0 �@/��@/|�@.�+@.E�@.{@-�T@-�@,�j@,�j@,�@,��@,z�@,�@+�m@+�@+33@+o@*�!@*^5@*-@*J@)��@)��@)��@)��@)x�@)G�@)�@(��@(�@(A�@'��@'+@&��@&5?@&@%��@%��@%�@%/@%V@$��@$�j@$j@#ƨ@#"�@"�!@"^5@!��@!hs@!G�@!�@ ��@ bN@ A�@   @�@;d@�@�R@v�@{@�h@?}@�@�/@�j@�@��@I�@�@��@�m@ƨ@dZ@@�@�@�H@n�@J@�#@�^@x�@%@Ĝ@�u@bN@A�@b@�;@|�@;d@�y@��@@p�@�@�@��@��@�D@j@Z@Z@Z@I�@�@��@��@�@t�@o@�!@n�@=q@J@��@�@�#@��@G�@7L@7L@&�@%@��@��@�u@r�@r�@bN@b@�;@�P@\)@K�@�@��@�y@�@ȴ@�R@��@v�@E�@@@�h@p�@O�@?}@/@��@��@��@�@��@�j@�@��@�D@j@j@Z@Z@I�@�@1@�
@��@33@
�@
��@
��@
~�@
n�@
n�@
n�@
=q@
�@	��@	�@	�@	��@	X@	�@	%@��@�`@�`@��@Ĝ@�9@�9Aχ+Aω7Aω7AϋDAϋDAω7Aω7AύPAύPAω7AϋDAύPAύPAω7Aω7AύPAύPAω7Aχ+AϋDAύPAϋDAω7AύPAϏ\AϋDAω7AϋDAϏ\AϏ\AϋDAϋDAϏ\AϑhAϏ\AύPAϏ\AϑhAϑhAύPAύPAϑhAϏ\AύPAύPAϏ\AϑhAϏ\AύPAϏ\AϏ\AϏ\AύPAύPAϑhAϏ\AύPAύPAϑhAϑhAϏ\AύPAϏ\AϑhAϑhAύPAύPAϑhAϑhAϑhAύPAύPAϑhAϓuAϏ\AύPAϑhAϓuAϑhAϏ\AϑhAϓuAϓuAϏ\AϏ\AϓuAϕ�AϓuAϑhAϑhAϓuAϕ�Aϕ�AϑhAϏ\AϓuAϕ�Aϕ�AϑhAϑhAϕ�Aϗ�Aϕ�AϑhAϓuAϕ�Aϗ�Aϗ�AϓuAϓuAϗ�Aϕ�AϑhAϑhAϕ�Aϗ�Aϗ�AϓuAϓuAϗ�Aϗ�Aϗ�AϓuAϕ�Aϗ�Aϙ�Aϕ�AϓuAϗ�Aϙ�Aϗ�Aϕ�Aϕ�Aϙ�Aϙ�Aϗ�Aϕ�Aϕ�Aϙ�Aϛ�Aϙ�Aϗ�Aϗ�Aϛ�Aϛ�Aϙ�Aϕ�Aϕ�Aϙ�Aϛ�Aϙ�Aϗ�Aϕ�Aϕ�Aϙ�Aϙ�Aϕ�Aϕ�Aϙ�Aϛ�Aϙ�Aϗ�Aϗ�Aϙ�Aϝ�Aϛ�Aϙ�Aϙ�Aϙ�Aϛ�Aϛ�Aϗ�AϓuAϗ�Aϙ�Aϗ�Aϗ�AϑhAϑhAϑhAϏ\Aχ+AσAσA�|�Aω7AυAυAϏ\AϋDAχ+AσAω7A�t�AσA�~�A�~�A�~�A�p�A�n�A�hsA�dZA�ffA�jA�jA�ffA�XA�XA�ZA�\)A�`BA�`BA�dZA�ffA�ffA�bNA�`BA�dZA�dZA�hsA�jA�S�A�O�A�VA�O�A�K�A�E�A�7LA�"�A��A��
AάA�hsA�ĜA͑hA�O�A��`A�M�A�$�A��A˙�A�Q�A��A���Aʛ�A�(�AɼjA�Q�A���A�I�A��HAǾwAǡ�A�hsA���A�t�A�VA�C�A� �A��/Aŏ\A�{AĶFA�t�A�E�A�$�A�A���Aò-A�n�A�/A��A�A��A�ĜA�C�A��`A��^A��-A��A���A���A��\A�~�A�n�A�bNA�G�A�5?A� �A��TA���A�"�A��A��A���A�XA�ffA�=qA�bNA���A�;dA�  A��A��!A�p�A�O�A��^A���A��A�=qA��wA���A���A�M�A�A��DA�A��DA�/A�  A���A��uA�G�A��TA�x�A�=qA���A�ƨA���A�p�A�-A���A�|�A�9XA��A��
A�Q�A��HA��hA���A���A�x�A��/A�5?A���A�/A�A��yA���A��jA��A���A���A��\A��PA��DA��DA��DA��+A�~�A�t�A�l�A�hsA�dZA�S�A�C�A�?}A�7LA�1'A��A�
=A��A���A��^A���A���A��PA�p�A�S�A�O�A�M�A�E�A�C�A�?}A�(�A��A�bA�A���A��A��;A���A��7A�&�A���A���A��DA�^5A�A�A�/A��A�VA���A��TA�ȴA��!A���A�x�A�\)A�S�A�7LA�+A��A�bA�1A�
=A���A���A���A��A��hA�E�A�r�A���A���A��A�n�A�ffA�`BA�^5A�\)A�S�A�G�A��A�A��A��`A��HA��;A��A���A��9A���A���A�~�A�`BA�;dA�%A��;A���A�A���A�p�A�ZA�E�A�E�A��A�1A�  A���A��A��`A��
A���A��-A���A��+A�dZA�5?A�
=A��A���A���A�p�A�K�A� �A�1A���A��mA��jA�|�A�K�A�-A�{A�  A���A�x�A�|�A�|�A�|�A�|�A�z�A�p�A�Q�A�7LA��yA��uA�M�A�VA���A��yA���A��-A��A���A��PA�p�A�I�A��!A�1'A�
=A���A��yA���A�ĜA���A�Q�A�A���A���A�p�A�^5A�O�A��A��TA�A��^A��!A���A��DA�`BA�?}A��A���A��+A�^5A�?}A�-A��A���A��#A�ȴA��wA���A�E�A��A�A�n�A��A�A���A��hA��+A��A�ffA�bNA�`BA�ZA�1'A��A�oA�
=A���A��A���A���A�z�A�hsA�p�A�jA�v�A�I�A��yA�`BA�;dA��A�
=A���A��A��DA�S�A�K�A�/A��A�ƨA��7A�A�A�&�A��#A�~�A�33A��/A�XA��A��A���A�Q�A�C�A�5?A��A���A��A���A�z�A�9XA��A��RA��7A�VA�/A��mA���A���A��A�bA���A���A�^5A��mA���A�=qA���A��uA��hA��A�\)A��A��!A�~�A�  A+A~�A~jA}�;A}�hA};dA|��A{��A{hsAz�HAz=qAy�TAy��Ay�7Ay"�Ax9XAw��AwC�Aw+Av��Av��AvZAu�Asp�Arn�Aq�Ap��Apn�ApM�AoXAnZAmXAljAk�hAkS�Ak�Aj~�Ai�Ahv�Ag�Ag��Ag`BAf�Af  Ae�7Ae�Ad�9Adn�Ad-Ac�Ac�^Acx�Ac&�AbȴAbn�Ab{Aa��Aa�A`�9A`ZA`bA_�-A^�HA^n�A]�A]�A[S�AX��AXVAX  AW��AWK�AWoAV�jAVA�AU��AUAUAU�wAU�AU�AU33AUATĜAT��AT��AT�AT��AT��AT�\ATv�ATM�ATE�AT�AS��ASS�ASAR�AQ/AP�jAP1'AN(�AL��AL��ALjAL{AK�wAK�PAKO�AKVAJ��AJ �AJ  AIhsAH�AG��AF��AF$�AE�AEVAC�AB  A@-A>�HA>��A>ZA=��A<��A<�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�4B	�4B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�qB	�_B	�SB	��B	��B	�=B	h�B	7LB	)�B	>�B	y�B	��B	��B	�oB	��B
�B
��B
�dB
�KB
��B
�*B
�0BhB1�B6�B8RB1[B0�BA�BM6BI�BJ#BDgB>wB<�B5tB0!B$B�B
��B
ߤB
ȴB
��B
��B
��B
}�B
h�B
?B
'�B
�B	�B	�XB	�B	��B	��B	{�B	o5B	aHB	V�B	Q�B	6B	8B	:^B	B�B	:^B	,=B	%�B	B	MB�B�B��B	GB	JB		B	5B	Y�B	��B	��B	�BB	��B	�B	��B
B	�B	�|B	� B	ѷB	��B	��B	�KB	�gB	ɆB	ʌB	͟B	��B	҉B	��B	�B	�`B
�B
�B
bB
�B
!B
CB
�B
�B
�B
 \B
#�B
&B
'�B
'�B
(�B
+kB
-wB
-�B
.�B
/�B
/�B
/B
3�B
9�B
>B
<B
C�B
C-B
CaB
D�B
DgB
EB
J�B
K�B
K�B
M�B
M�B
M�B
PHB
O�B
OvB
QNB
R�B
R�B
R B
R�B
R B
R B
Q�B
Q�B
Q�B
R�B
R B
R�B
S�B
UgB
U�B
U�B
VmB
W
B
W?B
Y�B
YB
W�B
W
B
WsB
X�B
\)B
Z�B
Z�B
Z�B
\)B
\�B
_B
]/B
]/B
_pB
`B
c�B
^B
W�B
W�B
a|B
e�B
ffB
d�B
cTB
b�B
aB
aHB
\]B
[�B
[�B
[WB
Y�B
YKB
X�B
YB
XEB
XEB
W�B
VB
UgB
UgB
S[B
R�B
R B
Q�B
Q�B
Q�B
Q�B
PB
O�B
M�B
MjB
JXB
E�B
B�B
AUB
:�B
:�B
9�B
7�B
6�B
5�B
5�B
6zB
6�B
5B
4nB
3hB
4B
0�B
0�B
0�B
/�B
.IB
.IB
.�B
-wB
.}B
.�B
.�B
0�B
.�B
.�B
.B
1[B
1'B
.�B
/OB
,=B
&�B
&B
%FB
#nB
%FB
!-B
 'B
B
xB
�B
�B
#nB
!bB
	B
7B
7B
7B
kB
�B
�B
!B
CB
B
�B
�B
�B
~B
�B
CB
=B
=B
7B
7B
�B
�B
+B
+B
�B
�B
�B
MB
�B
{B
B
FB
�B
�B
B
{B
�B
{B
B
MB
�B
B
B
MB
MB
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
7B
kB
�B
�B
�B
qB
�B
qB
=B
B
�B
�B
�B
�B
qB
CB
�B
xB
�B
�B
B
B
�B
�B
qB
	B
�B
�B
qB
�B
	B
�B
	B
qB
B
�B
�B
B
B
�B
B
B
OB
OB
�B
�B
VB
!B
 \B
!�B
!�B
"4B
!�B
"hB
"4B
"hB
"4B
"4B
"hB
"hB
"4B
"4B
"hB
"hB
"hB
!�B
"hB
#�B
#nB
$@B
$@B
$�B
%B
%B
$�B
%�B
&B
&�B
'�B
'�B
'�B
(XB
(�B
*0B
)�B
*0B
*�B
*eB
*�B
)�B
)�B
)_B
)�B
*0B
*�B
+kB
+�B
,B
+�B
,=B
,qB
,qB
-CB
-wB
-�B
-�B
.}B
.}B
.}B
.IB
.�B
/B
/�B
/�B
/OB
/�B
/�B
0UB
/�B
0!B
0�B
0�B
0UB
0!B
0UB
0�B
0UB
0UB
0�B
0�B
0UB
0UB
0�B
0�B
0�B
1[B
1�B
1�B
2-B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
4B
3�B
49B
49B
3�B
4�B
5�B
6FB
5�B
5�B
5�B
5�B
5�B
6B
6FB
7B
7�B
7�B
7�B
7�B
8B
7�B
8B
8RB
8�B
8�B
8�B
8�B
8�B
9�B
9XB
:�B
:^B
9�B
9�B
:�B
;0B
;dB
;0B
<B
<6B
<�B
<6B
<�B
<�B
=<B
=�B
=�B
=�B
=�B
=�B
>wB
>�B
>wB
>wB
?B
?}B
?�B
?�B
@OB
@�B
@�B
A B
AUB
A B
A B
B'B
A�B
A�B
B[B
B�B
B�B
C�B
CaB
C�B
DgB
D�B
E9B
E�B
E�B
FtB
FtB
GB
GzB
GzB
G�B
HKB
G�B
HKB
HKB
H�B
H�B
IB
IB
I�B
I�B
J#B
J�B
K)B
K)B
K)B
LdB
L0B
MB
L�B
MB
MB
M6B
M�B
M�B
M�B
M�B
M�B
M�B
NpB
N�B
N�B
N�B
N�B
N�B
OB
N�B
OvB
OvB
O�B
O�B
P�B
P}B
PHB
P}B
P}B
QNB
QNB
QNB
Q�B
Q�B
Q�B
Q�B
R B
R�B
R B
R�B
R�B
R�B
R�B
R�B
S&B
T,B
S�B
S�B
T�B
S�B
U2B
TaB
T�B
T�B
VmB
V9B
VmB
VmB
V�B
VmB
V�B
V�B
W
B
W
B
WsB
W�B
XEB
YB
X�B
YKB
YKB
Y�B
Y�B
ZQB
Y�B
ZB
Y�B
Y�B
ZQB
[�B
[�B
[�B
[�B
[�B
\]B
\]B
\�B
\�B
\�B
\�B
]�B
^B
]�B
]dB
^B
^5B
^�B
^B
`BB
`BB
`vB
aB
`�B
`�B
`�B
`�B
aHB
aB
aB
aB
aHB
aHB
aHB
aHB
a�B
a�B
a�B
b�B
c�B
c�B
cTB
dZB
d&B
d�B
d�B
e,B
e,B
e,B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
gmB
h
B
h>B
h
B
hsB
h
B
h�B
iB
iyB
iyB
jB
jKB
jKB
jKB
jB
jB
jB
jB
j�B
kQB
k�B
l"B
l"B
lWB
lWB
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
o B
n�B
o B
o5B
o�B
pB
p;B
poB
poB
poB
poB
p�B
qB
qB
qB
qB
rB
q�B
rGB
r�B
r�B
r�B
sB
tB
tB
tB
tB
tB
tTB
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
v+B
v`B
v`B
v`B
v`B
v�B
v�B
v�B
w2B
w2B
wfB
w�B
xlB
x�B
y	B
y>B
yrB
yrB
y�B
y�B
y�B
zB
y�B
zB
z�B
{JB
{B
{B
|PB
|PB
|PB
|�B
|�B
}VB
}"B
}�B
}�B
}�B
~�B
~(B
~]B
~�B
cB
cB
�B
�B
�B
� B
� B
�4B
�4B
�iB
�4B
�iB
�B
�B
�B
��B
�B
��B
��B
�B
��B
�AB
��B
��B
�B
�B
�B
�{B
�{B
��B
�B
�B
��B
�SB
��B
��B
��B
�%B
�YB
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
�_B
��B
�fB
�fB
��B
��B
��B
��B
��B
�lB
��B
��B
��B
��B
��B
�	B
�=B
�=B
�rB
�=B
�=B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�JB
�~B
�~B
��B
�PB
�PB
��B
��B
��B
��B
�"B
�VB
�VB
�"B
�VB
��B
��B
�(B
�\B
�\B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�bB
��B
��B
��B
��B
� B
� B
��B
�4B
�hB
�hB
�hB
�4B
��B
�:B
�oB
�oB
�oB
�oB
��B
��B
��B
��B
�B	��B	�hB	�B	��B	��B	��B	�hB	�bB	��B	��B	��B	��B	�-B	��B	��B	��B	��B	�4B	�B	�bB	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	�B	��B	�4B	��B	�4B	�B	��B	��B	�-B	�B	�B	��B	��B	�hB	�B	��B	��B	��B	��B	�hB	��B	�-B	��B	��B	��B	�bB	��B	�hB	��B	��B	��B	��B	�4B	�bB	�bB	��B	�:B	��B	��B	�bB	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	��B	��B	��B	�B	��B	�-B	�bB	��B	�nB	��B	��B	��B	�nB	�B	��B	�-B	��B	�B	�B	��B	�-B	��B	�B	�4B	��B	��B	��B	��B	�bB	�-B	�bB	��B	�:B	�4B	��B	�bB	�B	��B	��B	�-B	�hB	�B	��B	�-B	��B	��B	��B	�bB	�-B	�4B	��B	�hB	�bB	��B	��B	�B	�hB	�-B	��B	�4B	��B	��B	��B	��B	��B	�4B	��B	��B	�bB	��B	�hB	�hB	��B	��B	��B	��B	��B	�4B	��B	�4B	��B	�hB	��B	�\B	�bB	�4B	�nB	�-B	�\B	��B	��B	�B	�\B	��B	�bB	�B	�-B	�	B	�kB	�OB	��B	��B	�VB	��B	�!B	�!B	��B	��B	�	B	��B	��B	��B	�kB	��B	�7B	��B	�kB	�+B	��B	��B	�$B	�$B	��B	�MB	�{B	�SB	�_B	�MB	��B	��B	��B	��B	�$B	�B	�MB	�eB	�.B	�"B	�.B	�(B	��B	��B	��B	��B	{�B	�AB	�B	}VB	[#B	XEB	XEB	d�B	;dB	D3B	5�B	4nB	.�B	0�B	)*B	-�B	$tB	 'B	2�B	/B	2�B	/�B	/B	;dB	R�B	U�B	YKB	\�B	e`B	z�B	�=B	��B	�qB	��B	��B	�qB	��B	�9B	ȀB	ҽB	��B	�
B	�mB	�yB	ޞB	��B	�fB	�%B	�ZB	�%B	�B	��B	�+B	�8B	�DB	��B	��B	�VB
;B
�B
hB
�B
.}B
cB
`�B
{�B
��B
��B
�B
�}B
�?B
��B
��B
��B
ƨB
�HB
�TB
�gB
��B
��B
��B
�B
�QB
�`B
��B
�
B
�pB
��B
��B
�gB
�tB
�B
��B
��B
��B
�}B
��B
�nB
��B
�B
��B
��B
�FB
�.B
��B
�_B
�qB
�_B
��B
��B
�hB
��B
�*B
�B
�9B
��B
�$B
�RB
��B
��B
�B
��B
��B
��B
�'B
ɆB
�}B
�&B
�B
�/B
�+B
��BB+B�B$B�B�B �B%�B)�B-CB0�B1[B1�B2-B1�B5�B6�B6FB6�B7�B6zB6�B6�B6FB5tB7�B5tB5B5�B7�BA�BB�B<B0UB4B4B2�B1'B0�B2�B33B0�B1�B/�B/�B0�B1'B/�B49B.�B2-B0�B.�B0�B6FB8�B3�B:*BEmBN<Bg8BYBM�BOBBM6BK�BK^BJ�BI�BJ�BI�BOBBH�BJ�BIRBH�BG�BG�BI�BJ�BGzBGzBM6BI�BG�BPHBEBEmBFBGzBGzBC�BA�BA�BH�B@�B>�B>BB?�B>BB<�B=<B=B;�B>�B?B?HB=�B9�B;�B:�B>�B=<B=�B:*B8B8B=<B<6B3�B2�B,�B0UB<�B(�B+�B,qB,�B+kB,=B.}B0�B33B;dB3�B3�B-�B%�B$B&B"�B�B�BVB�B!B3�B�B"BB
	BB�B%B�BuB
��B
�PB
��B
�B
�GB
�fB
�B
�B
�>B
�mB
��B
�KB
�B
�TB
�B
��B
یB
��B
��B
��B
�B
��B
��B
�B
�?B
ʌB
�}B
�?B
�[B
B
��B
�<B
��B
��B
��B
�XB
�qB
�@B
��B
��B
�!B
�$B
�xB
�OB
�!B
��B
�SB
��B
��B
�$B
��B
��B
�@B
�YB
��B
�\B
��B
��B
��B
�oB
~�B
�B
v�B
s�B
xB
x8B
qAB
qvB
n/B
f�B
c�B
j�B
`vB
]/B
f2B
u%B
YB
I�B
A�B
8�B
9�B
<B
;�B
5�B
3�B
0!B
49B
2-B
$�B
)�B
OB
#B
.B
B
�B
B
�B
�B
B
AB
YB	�5B	��B	��B	�JB	�VB	�#B	��B	�[B	�XB	��B	ںB	�gB	�dB	�BB	�dB	��B	��B	��B	�UB	�eB	��B	�B	�RB	�B	��B	�B	�kB	��B	�$B	�:B	�hB	��B	�B	��B	�$B	��B	�B	�DB	|�B	~�B	��B	��B	��B	��B	|PB	q�B	w2B	}VB	��B	�AB	v�B	p�B	r�B	zxB	s�B	k�B	m�B	gmB	e�B	d�B	cTB	b�B	a�B	aHB	_�B	\�B	[�B	[�B	Z�B	V9B	S�B	O�B	Q�B	XyB	H�B	M�B	Y�B	jB	W
B	@�B	<jB	?�B	<�B	6FB	5tB	9�B	3�B	'�B	0�B	49B	6zB	>�B	<�B	8RB	7�B	1�B	3�B	8�B	;0B	<jB	=�B	@B	?�B	<�B	=�B	B�B	>wB	CaB	C-B	G�B	7LB	<�B	W�B	-wB	1'B	0�B	2-B	2aB	,�B	,qB	,B	0�B	+�B	!�B	3�B	#nB	1[B	)�B	B	YB	!-B	,=B	-B	�B	,B	oB	�B	�B	�B	oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                          B	��B	��B	�}B	�}B	��B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�HB	��B	��B	�HB	��B	��B	��B	�#B	�B	�B	�9B	��B	��B	b�B	0�B	#yB	8]B	sYB	�EB	̤B	�!B	�SB
<B
�kB
�B
��B
��B
��B
��BB+vB0`B2B+B*pB;pBF�BC�BC�B>B8)B6QB/&B)�B�B
}B
�nB
�VB
�fB
��B
�<B
��B
w=B
bYB
8�B
!9B
LB	�:B	�
B	��B	�EB	��B	u�B	h�B	Z�B	PSB	KiB	/�B	1�B	4B	<AB	4B	%�B	�B	�B��B��B��B�=B��B	�B	�B	.�B	SfB	~�B	��B	��B	�uB	ٿB	�{B	��B	�JB	�.B	��B	�iB	��B	�sB	��B	�B	�8B	�>B	�QB	ȋB	�;B	էB	�YB	�B	�bB
6B

B
�B
�B
�B
6B
�B
<B
B
�B
�B
!9B
!�B
"�B
%B
')B
'^B
(�B
)�B
)�B
(�B
-�B
3�B
7�B
5�B
=HB
<�B
=B
>�B
>B
>�B
DsB
EyB
EyB
G�B
GQB
GQB
I�B
I�B
I(B
K B
LoB
L�B
K�B
L�B
K�B
K�B
K5B
KiB
K�B
LoB
K�B
LoB
MAB
OB
O�B
OMB
PB
P�B
P�B
SfB
S1B
Q�B
P�B
Q%B
R�B
U�B
TlB
TlB
TlB
U�B
VxB
X�B
V�B
V�B
Y"B
Y�B
]oB
W�B
Q�B
Q�B
[.B
_{B
`B
^AB
]B
\iB
Z�B
Z�B
VB
U�B
U>B
U	B
SfB
R�B
R�B
R�B
Q�B
Q�B
Q�B
O�B
OB
OB
MB
L;B
K�B
K�B
K5B
KiB
KiB
I�B
I]B
GQB
GB
D
B
?TB
<vB
;B
4�B
4�B
3sB
1�B
0�B
/�B
/ZB
0,B
0`B
.�B
. B
-B
-�B
*�B
*�B
*<B
)5B
'�B
'�B
(dB
')B
(/B
(dB
(dB
*<B
(dB
(�B
'�B
+B
*�B
(�B
)B
%�B
 gB
�B
�B
 B
�B
�B
�B
�B
*B
6B
<B
 B
B
�B
�B
�B
�B
B
�B
dB
�B
�B
�B
^B
^B
�B
0B
dB
�B
�B
�B
�B
�B
zB
tB
�B
�B
�B
�B
nB
�B
�B
-B
�B
�B
�B
[B
�B
-B
�B
-B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
3B
�B
9B
�B
�B
9B
nB
nB
�B
9B
tB
�B
tB
?B
�B
zB
EB
�B
�B
zB
�B
�B
B
RB
RB
�B
#B
XB
#B
�B
�B
�B
�B
�B
�B
#B
�B
XB
*B
XB
^B
�B
�B
�B
�B
#B
�B
�B
RB
#B
�B
�B
RB
�B
#B
�B
�B
^B
�B
�B
^B
�B
�B
B
B
jB
�B
B
�B
B
HB
HB
�B
�B
B
�B
B
�B
�B
B
B
�B
�B
B
B
B
�B
B
UB
 B
�B
�B
[B
�B
�B
�B
aB
�B
 gB
!mB
!9B
!mB
"
B
"�B
#�B
#EB
#�B
$�B
$B
$KB
#yB
#EB
#B
#yB
#�B
$�B
%B
%�B
%�B
%�B
%�B
&#B
&#B
&�B
')B
'�B
'�B
(/B
(/B
(/B
'�B
(dB
(�B
)�B
)�B
)B
)jB
)jB
*B
)�B
)�B
*<B
*pB
*B
)�B
*B
*<B
*B
*B
*<B
*pB
*B
*B
*pB
*<B
*�B
+B
+vB
+BB
+�B
+�B
,|B
,�B
,|B
,�B
,|B
,�B
,�B
-NB
-NB
-�B
-�B
-�B
-�B
-�B
.TB
/ZB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
12B
1�B
1�B
1�B
1�B
1�B
1�B
2B
2mB
2�B
2mB
2mB
2mB
3>B
3
B
4yB
4B
3�B
3�B
4yB
4�B
5B
4�B
5�B
5�B
6QB
5�B
6QB
6�B
6�B
7WB
7WB
7WB
7WB
7�B
8)B
8]B
8)B
8)B
8�B
9/B
9cB
9cB
:B
:5B
:�B
:�B
;B
:�B
:�B
;�B
;�B
;pB
<B
<vB
<vB
=|B
=B
=HB
>B
>NB
>�B
?�B
?�B
@&B
@&B
@�B
A,B
A,B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C8B
C8B
C�B
DsB
D�B
D�B
D�B
FB
E�B
F�B
FB
F�B
F�B
F�B
GQB
G�B
G�B
G�B
GQB
G�B
H"B
H�B
HWB
H�B
H�B
H�B
H�B
HWB
I(B
I(B
I�B
I�B
JcB
J/B
I�B
J/B
J/B
K B
K B
K B
KiB
K�B
KiB
K�B
K�B
L;B
K�B
L;B
LoB
L�B
LoB
L�B
L�B
M�B
MuB
MuB
NGB
M�B
N�B
NB
N|B
N�B
PB
O�B
PB
PB
PSB
PB
P�B
PSB
P�B
P�B
Q%B
Q�B
Q�B
R�B
R�B
R�B
R�B
SfB
SfB
TB
S�B
S�B
S�B
S�B
TB
U>B
U>B
U�B
UrB
U�B
VB
VB
VDB
VDB
VDB
V�B
W~B
W�B
W~B
WB
W�B
W�B
XPB
W�B
Y�B
Y�B
Z(B
Z�B
Z\B
Z�B
Z\B
Z\B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[cB
[�B
[�B
\4B
]:B
]oB
]B
^B
]�B
^uB
^�B
^�B
^�B
^�B
_GB
_{B
_GB
_{B
`B
`�B
`�B
`�B
aB
a�B
a�B
a�B
b%B
a�B
bYB
b�B
c+B
c+B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
deB
eB
e�B
e�B
e�B
f	B
f	B
f=B
f�B
f�B
gCB
gxB
gCB
gxB
g�B
gxB
gxB
hB
h�B
h~B
h�B
h�B
iPB
i�B
i�B
j!B
j!B
j!B
j!B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
nB
n:B
nnB
n�B
n�B
oB
o@B
o�B
o�B
o�B
pB
pB
pB
pB
pFB
pFB
p�B
p�B
p�B
qB
q�B
rB
r�B
r�B
r�B
s$B
s$B
sYB
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u1B
u1B
vB
vB
vB
v7B
vkB
wB
v�B
w=B
wqB
w�B
xCB
w�B
xB
xwB
yB
yB
yIB
y~B
y~B
y�B
y�B
y�B
y�B
zB
y�B
zB
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}-B
}-B
}�B
}�B
}�B
~3B
B
�B
�B
�B
�B
�B
�tB
�tB
�tB
�tB
�tB
�tB
��B
��B
�FB
�B
�B
��B
�B
�B
�LB
�LB
��B
��B
��B
�B
�RB
�RB
�RB
�RB
��B
��B
��B
��B
�$B
��B
��B
��B
��B
�^B
�^B
�^B
��B
��B
��B
��B
��B
��B
�0B
�0B
�eB
�B
�B
�6B
�kB
��B
��B
��B
�B
�B
��B
�B
�qB
�qB
��B
�B
�B
�CB
�wB
�CB
�CB
�CB
�wB
�CB
�wB
��B
�B
�IB
�IB
�}B
�IB
��B
��B
�}B
��B
�B
�B
�B
��B
�OB
��B
�!B
�!B
�!B
�!B
�UB
�UB
��B
��B
��B	��B	�B	��B	�}B	�HB	��B	�B	�B	��B	�NB	�NB	�wB	��B	��B	�NB	��B	��B	��B	��B	�B	��B	�}B	��B	��B	�wB	�}B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�HB	�B	��B	�HB	��B	�HB	��B	�B	�}B	��B	��B	��B	��B	�B	��B	�B	��B	�wB	�HB	��B	��B	�B	�B	��B	��B	�HB	��B	�B	��B	��B	�HB	�wB	��B	��B	�HB	�BB	�}B	��B	�}B	��B	�HB	�B	��B	�HB	��B	�HB	�NB	��B	�HB	��B	�B	�NB	� B	��B	��B	�HB	� B	��B	�}B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	�HB	�NB	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�NB	�}B	��B	�B	��B	��B	��B	�HB	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�NB	��B	�HB	��B	��B	��B	��B	�NB	�B	�wB	�B	�B	�HB	�BB	�HB	��B	�NB	��B	��B	��B	�NB	�B	�HB	�B	�B	��B	� B	��B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	��B	�dB	�B	�dB	��B	��B	��B	�UB	��B	�wB	��B	��B	�B	��B	��B	�zB	�B	��B	��B	�RB	��B	��B	�gB	��B	�-B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�qB	�OB	��B	��B	u�B	{�B	~�B	wB	T�B	Q�B	Q�B	^uB	5B	=�B	/ZB	. B	(�B	*pB	"�B	'^B	&B	�B	,HB	(�B	,�B	)�B	(�B	5B	L�B	OMB	R�B	VxB	_B	t�B	��B	��B	�#B	��B	��B	�#B	��B	��B	�2B	�oB	ҔB	мB	�B	�+B	�PB	�B	�B	��B	�B	��B	�B	�uB	��B	��B	��B	�B	�wB	�B	��B
6B
B
6B
(/B
yB
Z�B
ueB
�eB
�KB
��B
�/B
��B
�QB
�mB
�QB
�ZB
��B
�B
�B
�B
�uB
֭B
��B
�B
�B
ًB
�B
�"B
юB
˞B
�B
�&B
��B
�WB
�KB
�gB
�/B
�mB
� B
�BB
��B
�^B
�RB
��B
��B
��B
�B
�#B
�B
��B
{�B
�B
�wB
��B
��B
��B
�;B
��B
�B
�gB
�gB
��B
�2B
�yB
��B
��B
�8B
�/B
��B
�1B
��B
��B
�YB
��B �B	�B�B�B�BwB�B#EB&�B*<B+B+vB+�B+�B/�B0�B/�B0�B1�B0,B0�B0�B/�B/&B12B/&B.�B/ZB1gB;;B<vB5�B*B-�B-�B,HB*�B*�B,|B,�B*�B+�B)jB)�B*<B*�B)jB-�B(�B+�B*�B(�B*�B/�B28B-�B3�B?BG�B`�BS1BG�BH�BF�BEDBEBD�BClBDsBC8BH�BBfBD>BCBB2BA�BA�BC8BD>BA,BA,BF�BClBA`BI�B>�B?B?�BA,BA,B=|B;�B;pBB2B:�B8�B7�B9cB7�B6�B6�B6�B5KB8]B8�B8�B7�B3�B5B4EB8]B6�B7WB3�B1�B1�B6�B5�B-�B,HB&�B*B6QB"sB%�B&#B&WB%B%�B(/B*�B,�B5B-NB-�B'�BaB�B�B�B�BdBBXB�B-�B�B�B�B�B�BLB
��BUB
�'B
�qB
�B
�B
��B
��B
�B
��B
��B
��B
�B
ީB
��B
�SB
�B
��B
ڑB
�>B
�uB
˞B
ǅB
ǹB
ǅB
áB
��B
��B
�>B
�/B
��B
�B
�AB
��B
��B
��B
�EB
��B
�
B
�#B
��B
��B
�aB
��B
��B
�*B
�B
��B
��B
�B
��B
�nB
��B
�OB
�IB
��B
�B
�?B
�B
nB
�FB
{�B
{!B
xCB
��B
p�B
mhB
q�B
q�B
j�B
k(B
g�B
`�B
]�B
deB
Z(B
V�B
_�B
n�B
R�B
ClB
;pB
2mB
3>B
5�B
5B
/ZB
-�B
)�B
-�B
+�B
�B
#EB
B
�B
'�B
�B
	CB
�B
6B
 tB	��B	��B
B	��B	�B	��B	��B	�B	��B	ܝB	�B	�
B	��B	�lB	�B	�B	��B	�B	��B	�BB	��B	�B	�B	�|B	��B	�B	��B	�XB	��B	�B	��B	��B	��B	�B	��B	��B	�vB	��B	��B	��B	��B	v7B	xwB	��B	�eB	��B	�tB	vB	k\B	p�B	wB	nB	{�B	pFB	jVB	lbB	t*B	m�B	elB	gxB	aB	_{B	^uB	]B	\iB	[cB	Z�B	Y�B	V�B	U�B	U>B	T8B	O�B	MAB	I]B	K�B	R+B	BfB	GQB	SfB	d1B	P�B	:�B	6B	9cB	6�B	/�B	/&B	3>B	-�B	!mB	*pB	-�B	0,B	8]B	6�B	2B	1�B	+�B	-�B	2�B	4�B	6B	7WB	9�B	9�B	6�B	7WB	<AB	8)B	=B	<�B	A�B	0�B	6�B	QZB	')B	*�B	*<B	+�B	,B	&WB	&#B	%�B	*pB	%�B	�B	-NB	 B	+B	#EB	�B	B	�B	%�B	&�B	<B	%�B�!B	^B	eB	
IB�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223304                            20230426223304AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622330420230426223304  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330420230426223304QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330420230426223304QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               