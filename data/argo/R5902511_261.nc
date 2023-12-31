CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:05Z creation      
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
resolution        =���   axis      Z        H  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  b�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȩ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H =�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H e   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230721225005  20230721225005  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�+�_���@�+�_���11  @�+ꈈ��@�+ꈈ��@2��#��@2��#���d�F
�L0�d�F
�L011  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?u@   @@  @��\@�G�@�G�@޸R@�p�A�RA   A+�A?\)A`  A�  A�  A�\)A��A�Q�A�Q�A�  A�Q�B z�BQ�BQ�B  B�B'�B/�B7�
B@  BG�BO�
BX(�B`Q�Bh  Bp  Bw�
B�  B�(�B�{B�{B��B��B�  B��B�  B�  B�  B�  B��B��
B�  B�{B�  B��B�  B�  B�{B�  B�  B�(�B�(�B�{B�  B�{B�(�B�{B�  B�{C   C  C
=C  C  C
  C
=C
=C
=C  C��C  C
=C  C  C  C 
=C"
=C$
=C&
=C'��C*  C,{C.�C0{C2
=C4  C6  C8  C9��C<  C>
=C?��CB{CD
=CE��CH  CJ  CK��CM�CP
=CR
=CT  CV
=CX  CZ  C\  C^  C_��Ca��Cc��Ce��Ch
=Cj  Cl
=Cm��Cp  Cr
=Ct  Cu�Cx
=Cy�C{��C~
=C�C���C�  C���C���C�  C�  C�C�  C���C���C�  C�C�C�C�  C���C�C�
=C�  C�C���C�  C���C���C�C�
=C�  C�  C�  C���C���C�  C�  C�  C�
=C�
=C�  C�  C�C���C���C�C�C�C�
=C�C�  C�  C�  C�  C���C��C���C�  C�  C�C�  C���C�  C�  C�  C�  C���C��C���C���C�  C�C�C���C��C���C�C�C���C�C�C���C�  C�C�  C���C�  C�
=C�
=C�
=C�C���C���C���C�  C���C�C�
=C�
=C�
=C�  C�C�C���C�  C�  C���C���C���C�  C�C�C�  C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�C�  C���C�C�C���C���C���D ��D  D� D  Dz�D��D}qD  D��D  D}qD�qD}qDD��D�qD}qD	  D	}qD
�D
�D
�qDz�D��Dz�D��D}qD  D}qD�qD}qD�qD}qD�qD}qD�D�D�D}qD��D}qD  D��D�qD��D�D� D�qD� D�D� D�qDz�D  D�DD� D�D�DD�D�D}qD   D �D!�D!�D"�D"� D#�D#�D$  D$z�D$�qD%� D&�D&��D&��D'xRD'��D(� D)D)}qD)�qD*� D+  D+� D,  D,� D-  D-�D.D.� D.�qD/}qD/�qD0� D1D1��D1�qD2� D3  D3� D4�D4�D5D5��D6�D6� D7  D7��D8D8}qD8��D9� D:  D:}qD:�qD;��D<D<�D=�D=}qD=�qD>}qD?  D?��D@�D@��DADA��DA�qDB� DC�DC� DC�qDD� DEDE�DF  DF}qDG  DG� DH�DH�DI�DI� DJ  DJ��DK�DK��DL�DL��DM  DM��DN  DN� DO  DOz�DO�qDP� DQ�DQ�DR�DR��DSDS�DT  DT}qDU�DU��DV  DV}qDV��DW}qDX�DX� DX�qDY}qDZ  DZ��D[�D[� D\  D\��D\�qD]}qD^  D^}qD_  D_��D`  D`� Da�Da� Db�Db�Dc�Dc� Dc�qDd}qDd�qDez�Df  Df� Df�qDg}qDg�qDh}qDi  Di��Dj�Dj� Dk  Dk}qDk�qDl� Dm  Dm}qDn  Dn}qDn�qDo}qDo�qDp� Dq  Dq� Dr  Dr� Dr�qDs� Dt�Dt��Du  Du��Dv  Dv}qDw  Dw��Dx�Dx��Dx�qDy� Dz�Dz�D{�D{� D{�qD|� D}�D}� D}�qD~}qD  D� D�HD�B�D��HD�� D�  D�@ D�� D�� D�HD�AHD�� D��HD�  D�>�D��HD�� D���D�=qD�~�D���D���D�@ D�~�D�� D�HD�@ D��HD��HD�  D�>�D�~�D�� D�HD�>�D�� D�� D�  D�@ D�~�D���D�  D�AHD�~�D��qD���D�>�D��HD�� D�  D�AHD���D�� D���D�@ D�~�D�� D�  D�@ D�� D��HD���D�>�D�� D�� D���D�@ D�� D��HD��D�@ D�� D�� D���D�>�D�~�D���D�  D�B�D��HD�� D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�}qD�� D�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�@ D�}qD���D�  D�>�D�� D��HD��D�AHD��HD��HD�  D�AHD�� D��qD���D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D�� D��D�B�D��HD��HD�  D�@ D�� D���D���D�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D�� D���D�>�D�� D��HD�HD�AHD�� D�� D�  D�>�D�~�D��HD�  D�>�D�~�D���D�  D�@ D�~�D��HD�HD�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�~�D�� D�  D�>�D��HD�� D���D�>�D�� D��HD�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�B�D��HD��HD�  D�AHD�~�D���D���D�>�D��HD��HD�HD�>�D�~�D�� D�  D�AHD��HD�� D�  D�@ D�~�D�� D�HD�AHD��HD��HD�  D�@ D���D��HD���D�>�D�~�D���D���D�AHD���D��HD�  D�AHD��HD�� D�HD�B�D���D��HD�  D�>�D�~�D�� D�  D�AHD D�� D�  D�AHDÀ DýqD�  D�B�DāHD��HD�  D�@ Dŀ D�� D���D�@ D�~�D�� D�HD�@ Dǀ DǾ�D�  D�AHDȀ D�� D���D�AHDɁHDɾ�D���D�@ D�~�Dʾ�D�HD�AHDˁHD��HD��D�AHD̀ D��HD�HD�AHD͂�D�D�  D�=qD�~�D��HD�  D�@ D�~�DϽqD�  D�AHD�~�Dо�D���D�=qD�}qDѽqD�HD�AHD�~�DҾ�D�  D�>�D�}qDӾ�D�  D�AHDԁHD�� D�  D�>�D�}qD��HD��D�@ D�~�D��HD�  D�@ D׀ D׾�D�HD�AHD؁HD�� D��qD�@ DفHD��HD��D�AHDڂ�D�D��D�@ D�}qD۽qD�HD�AHD܁HD��HD���D�>�D݁HD��HD���D�>�D�~�D޾�D�  D�AHD߁HD�� D���D�=qD�}qD��HD�HD�>�D�~�D�� D�HD�AHD�~�D⾸D�  D�>�D�~�D�� D�  D�@ D� D��HD�  D�>�D� D��HD�HD�AHD�HD��HD�HD�>�D� D��HD�HD�AHD�HD��HD�HD�B�D�HD龸D�HD�AHD�HD�� D�  D�AHD�HD��HD�HD�B�D� D�� D�  D�@ D�~�D���D�HD�AHD� D�� D�HD�B�D� DﾸD�  D�AHD�� D�� D�  D�@ D�~�D�qD���D�@ D�~�D�qD���D�@ D� D��HD�  D�@ D�HD�� D�HD�@ D�� D���D���D�@ D��HD�� D�  D�@ D�� D�D�  D�B�D�� D���?\)?W
=?�\)?��?�@\)@(��@5@Q�@fff@}p�@��@�z�@�G�@�{@�
=@��
@�\)@�
=@��
@��@���A�\A��A(�A33AQ�A(�A#�
A'�A-p�A333A7�A>�RAC�
AG�AN�RAR�\AW�A^�RAb�\Ag�Amp�AqG�AxQ�A|��A���A��
A��A���A��\A�p�A�  A��A��A�
=A���A�z�A�ffA���A�(�A�ffA�G�A�33A��RA���A��A��RA���A��A��RA���A�33AƸRAȣ�A��
AθRAУ�AӅAָRAأ�A�33A�ffA���A�33A�ffA�Q�A�33A�ffA�  A�33A�A��A�33A�p�B   B��B�RB�
B��B�RB�
B	��B
�RB�
Bp�B�HB�BG�B�HB�Bp�B�HB  B�B�RB  B�B�\B (�B!�B"�\B$(�B%G�B&ffB((�B)G�B*ffB,  B-G�B.ffB0(�B1G�B2ffB4(�B5�B6�RB8  B8��B:�\B;�B<��B>�\B?�B@��BB=qBC�BD��BE��BG33BH��BIp�BJ�\BL  BMp�BNffBO�BQG�BRffBS�BU�BV{BW
=BXz�BY�B[
=B\  B]p�B^�HB_�B`��Bb=qBc
=Bd(�Bep�BfffBg33Bh��Bi��Bj=qBk�Bl��Bm�Bn{Bo
=Bo�BpQ�BqG�Br=qBrffBs33Bt(�Btz�Bt��Bv{Bv=qBv�HBx  BxQ�Bx��ByBzffBz�RB{�B|z�B|��B}G�B~=qB~�RB33B�{B�ffB���B���B�p�B��B��B�ffB���B��HB�G�B�B��B�=qB��RB���B��B���B��
B�{B�z�B��HB�
=B�p�B��
B��B�ffB��RB��HB�33B���B��
B�{B�z�B��HB���B�G�B��B�{B�=qB�z�B��HB�\)B��B�B�=qB��\B���B��B���B��B�{B��\B��HB��B���B�  B�=qB�z�B��HB�\)B���B��
B�Q�B��RB��HB�33B��B��B�(�B���B��B�G�B��B�{B�Q�B���B�33B�p�B��
B�Q�B���B��HB�p�B��
B�{B�ffB���B�\)B��B�{B��\B��HB�33B�B�  B�Q�B��HB�33B��B�  B�z�B���B��B��B��B�Q�B��HB�33B��B�(�B�ffB���B�\)B���B�(�B���B��HB�p�B��B�(�B���B�
=B�\)B�  B�Q�B���B�33B���B�  B��\B���B�p�B��
B�(�B��RB��B�p�B�{B�Q�B��RB�G�B���B�  B�z�B�
=B�\)B�B�Q�B��RB�
=B���B�{B�Q�B���B�p�B��B�(�B��RB�
=B���B�(�B�ffB��B���B��B��\B���B�\)B�{B�ffB��HB��B�  B�Q�B���BÅB��
B�ffB�
=B�\)B��
B�z�B��HB�G�B��B�ffBȸRB�G�B��B�Q�Bʣ�B�G�B��
B�(�Ḅ�B�G�B�B�(�BθRB�\)BϮB�(�B���B�33Bљ�B�(�B��HB�33Bә�B�Q�B��HB�33B�B�z�B���B�G�B��
B�z�B��HB�\)B�  B�z�B���B�p�B�  B�Q�B��HB�p�B�B�(�B��HB�33Bߙ�B�=qB�RB�33B�B�  B��B�33B�B��B�z�B�
=B�G�B��B�ffB���B�G�B��
B�=qB�\B�33B�B�  B��B��B�B��B�ffB�
=B�\)B�B�(�B�RB�G�BB�{B�RB�33B�p�B��B�\B���B�G�B�B�Q�B���B�G�B���B�{B��RB�
=B�p�B��
B�z�B���B�G�B�B�Q�B��HB��B��B�(�B���B��HB�\)B��B�(�B��\B��B���B��C �C Q�C ��C �
C  C�CffC��C�
C��C�CffC��C��C��C=qCz�C��C�
C�C\)C�\C�RC�C�Cp�C��C��C
=CQ�Cz�C�C�C=qCp�C��C��C{CQ�Cz�C��C�HC	33C	p�C	�\C	C

=C
Q�C
�C
��C
�C(�CffC�C�C{CG�Cz�CC  CQ�C�C�C��CG�Cp�C��C�C33Cp�C��C��C{C\)C��C�
C��C33C�CC�HC{Cp�C�C�C�CQ�C��C�HC(�C\)C�C��C{CffC��C��C
=C=qCz�CC
=CQ�C�\C�RC��C=qC�\C��C  C=qCz�C��C
=CG�Cz�C�C
=CG�Cp�C�C��C=qC�CC��C33CffC�C��C=qCz�C�RC�C(�Cp�CC 
=C =qC z�C �C!  C!Q�C!��C!��C"  C"G�C"��C"�
C#
=C#G�C#�C#�
C${C$Q�C$�C$�RC$�C%=qC%z�C%��C&  C&33C&ffC&��C&�C'=qC'p�C'�C'��C((�C(\)C(��C(�HC)�C)p�C)�C)�C*�C*Q�C*�C*��C+{C+\)C+��C+�HC,�C,Q�C,�\C,C-  C-=qC-�C-��C.
=C.G�C.�C.�RC.�HC/{C/Q�C/��C/�HC0�C0Q�C0�C0C0��C1(�C1ffC1��C1�C2(�C2ffC2��C2�
C3
=C3G�C3�C3��C4�C4\)C4��C4��C5
=C5=qC5�C5��C6
=C6Q�C6�\C6�HC7�C7\)C7��C7�
C8{C8Q�C8�\C8C9  C9G�C9�C9�RC9��C:G�C:�C:��C;  C;=qC;z�C;�RC;�C<�C<\)C<��C<�
C=�C=ffC=��C=�C>33C>p�C>�C>�C?�C?\)C?��C?�
C@{C@Q�C@��C@�HCA�CAffCA�CA�CB(�CB\)CB��CB�
CC�CC\)CC��CC�
CD{CDQ�CD�\CD��CE
=CEG�CE�CE��CF{CF\)CF��CF�HCG�CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            ?u@   @@  @��\@�G�@�G�@޸R@�p�A�RA   A+�A?\)A`  A�  A�  A�\)A��A�Q�A�Q�A�  A�Q�B z�BQ�BQ�B  B�B'�B/�B7�
B@  BG�BO�
BX(�B`Q�Bh  Bp  Bw�
B�  B�(�B�{B�{B��B��B�  B��B�  B�  B�  B�  B��B��
B�  B�{B�  B��B�  B�  B�{B�  B�  B�(�B�(�B�{B�  B�{B�(�B�{B�  B�{C   C  C
=C  C  C
  C
=C
=C
=C  C��C  C
=C  C  C  C 
=C"
=C$
=C&
=C'��C*  C,{C.�C0{C2
=C4  C6  C8  C9��C<  C>
=C?��CB{CD
=CE��CH  CJ  CK��CM�CP
=CR
=CT  CV
=CX  CZ  C\  C^  C_��Ca��Cc��Ce��Ch
=Cj  Cl
=Cm��Cp  Cr
=Ct  Cu�Cx
=Cy�C{��C~
=C�C���C�  C���C���C�  C�  C�C�  C���C���C�  C�C�C�C�  C���C�C�
=C�  C�C���C�  C���C���C�C�
=C�  C�  C�  C���C���C�  C�  C�  C�
=C�
=C�  C�  C�C���C���C�C�C�C�
=C�C�  C�  C�  C�  C���C��C���C�  C�  C�C�  C���C�  C�  C�  C�  C���C��C���C���C�  C�C�C���C��C���C�C�C���C�C�C���C�  C�C�  C���C�  C�
=C�
=C�
=C�C���C���C���C�  C���C�C�
=C�
=C�
=C�  C�C�C���C�  C�  C���C���C���C�  C�C�C�  C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�C�  C���C�C�C���C���C���D ��D  D� D  Dz�D��D}qD  D��D  D}qD�qD}qDD��D�qD}qD	  D	}qD
�D
�D
�qDz�D��Dz�D��D}qD  D}qD�qD}qD�qD}qD�qD}qD�D�D�D}qD��D}qD  D��D�qD��D�D� D�qD� D�D� D�qDz�D  D�DD� D�D�DD�D�D}qD   D �D!�D!�D"�D"� D#�D#�D$  D$z�D$�qD%� D&�D&��D&��D'xRD'��D(� D)D)}qD)�qD*� D+  D+� D,  D,� D-  D-�D.D.� D.�qD/}qD/�qD0� D1D1��D1�qD2� D3  D3� D4�D4�D5D5��D6�D6� D7  D7��D8D8}qD8��D9� D:  D:}qD:�qD;��D<D<�D=�D=}qD=�qD>}qD?  D?��D@�D@��DADA��DA�qDB� DC�DC� DC�qDD� DEDE�DF  DF}qDG  DG� DH�DH�DI�DI� DJ  DJ��DK�DK��DL�DL��DM  DM��DN  DN� DO  DOz�DO�qDP� DQ�DQ�DR�DR��DSDS�DT  DT}qDU�DU��DV  DV}qDV��DW}qDX�DX� DX�qDY}qDZ  DZ��D[�D[� D\  D\��D\�qD]}qD^  D^}qD_  D_��D`  D`� Da�Da� Db�Db�Dc�Dc� Dc�qDd}qDd�qDez�Df  Df� Df�qDg}qDg�qDh}qDi  Di��Dj�Dj� Dk  Dk}qDk�qDl� Dm  Dm}qDn  Dn}qDn�qDo}qDo�qDp� Dq  Dq� Dr  Dr� Dr�qDs� Dt�Dt��Du  Du��Dv  Dv}qDw  Dw��Dx�Dx��Dx�qDy� Dz�Dz�D{�D{� D{�qD|� D}�D}� D}�qD~}qD  D� D�HD�B�D��HD�� D�  D�@ D�� D�� D�HD�AHD�� D��HD�  D�>�D��HD�� D���D�=qD�~�D���D���D�@ D�~�D�� D�HD�@ D��HD��HD�  D�>�D�~�D�� D�HD�>�D�� D�� D�  D�@ D�~�D���D�  D�AHD�~�D��qD���D�>�D��HD�� D�  D�AHD���D�� D���D�@ D�~�D�� D�  D�@ D�� D��HD���D�>�D�� D�� D���D�@ D�� D��HD��D�@ D�� D�� D���D�>�D�~�D���D�  D�B�D��HD�� D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�}qD�� D�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�@ D�}qD���D�  D�>�D�� D��HD��D�AHD��HD��HD�  D�AHD�� D��qD���D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D�� D��D�B�D��HD��HD�  D�@ D�� D���D���D�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D�� D���D�>�D�� D��HD�HD�AHD�� D�� D�  D�>�D�~�D��HD�  D�>�D�~�D���D�  D�@ D�~�D��HD�HD�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�~�D�� D�  D�>�D��HD�� D���D�>�D�� D��HD�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�B�D��HD��HD�  D�AHD�~�D���D���D�>�D��HD��HD�HD�>�D�~�D�� D�  D�AHD��HD�� D�  D�@ D�~�D�� D�HD�AHD��HD��HD�  D�@ D���D��HD���D�>�D�~�D���D���D�AHD���D��HD�  D�AHD��HD�� D�HD�B�D���D��HD�  D�>�D�~�D�� D�  D�AHD D�� D�  D�AHDÀ DýqD�  D�B�DāHD��HD�  D�@ Dŀ D�� D���D�@ D�~�D�� D�HD�@ Dǀ DǾ�D�  D�AHDȀ D�� D���D�AHDɁHDɾ�D���D�@ D�~�Dʾ�D�HD�AHDˁHD��HD��D�AHD̀ D��HD�HD�AHD͂�D�D�  D�=qD�~�D��HD�  D�@ D�~�DϽqD�  D�AHD�~�Dо�D���D�=qD�}qDѽqD�HD�AHD�~�DҾ�D�  D�>�D�}qDӾ�D�  D�AHDԁHD�� D�  D�>�D�}qD��HD��D�@ D�~�D��HD�  D�@ D׀ D׾�D�HD�AHD؁HD�� D��qD�@ DفHD��HD��D�AHDڂ�D�D��D�@ D�}qD۽qD�HD�AHD܁HD��HD���D�>�D݁HD��HD���D�>�D�~�D޾�D�  D�AHD߁HD�� D���D�=qD�}qD��HD�HD�>�D�~�D�� D�HD�AHD�~�D⾸D�  D�>�D�~�D�� D�  D�@ D� D��HD�  D�>�D� D��HD�HD�AHD�HD��HD�HD�>�D� D��HD�HD�AHD�HD��HD�HD�B�D�HD龸D�HD�AHD�HD�� D�  D�AHD�HD��HD�HD�B�D� D�� D�  D�@ D�~�D���D�HD�AHD� D�� D�HD�B�D� DﾸD�  D�AHD�� D�� D�  D�@ D�~�D�qD���D�@ D�~�D�qD���D�@ D� D��HD�  D�@ D�HD�� D�HD�@ D�� D���D���D�@ D��HD�� D�  D�@ D�� D�D�  D�B�D�� D���?\)?W
=?�\)?��?�@\)@(��@5@Q�@fff@}p�@��@�z�@�G�@�{@�
=@��
@�\)@�
=@��
@��@���A�\A��A(�A33AQ�A(�A#�
A'�A-p�A333A7�A>�RAC�
AG�AN�RAR�\AW�A^�RAb�\Ag�Amp�AqG�AxQ�A|��A���A��
A��A���A��\A�p�A�  A��A��A�
=A���A�z�A�ffA���A�(�A�ffA�G�A�33A��RA���A��A��RA���A��A��RA���A�33AƸRAȣ�A��
AθRAУ�AӅAָRAأ�A�33A�ffA���A�33A�ffA�Q�A�33A�ffA�  A�33A�A��A�33A�p�B   B��B�RB�
B��B�RB�
B	��B
�RB�
Bp�B�HB�BG�B�HB�Bp�B�HB  B�B�RB  B�B�\B (�B!�B"�\B$(�B%G�B&ffB((�B)G�B*ffB,  B-G�B.ffB0(�B1G�B2ffB4(�B5�B6�RB8  B8��B:�\B;�B<��B>�\B?�B@��BB=qBC�BD��BE��BG33BH��BIp�BJ�\BL  BMp�BNffBO�BQG�BRffBS�BU�BV{BW
=BXz�BY�B[
=B\  B]p�B^�HB_�B`��Bb=qBc
=Bd(�Bep�BfffBg33Bh��Bi��Bj=qBk�Bl��Bm�Bn{Bo
=Bo�BpQ�BqG�Br=qBrffBs33Bt(�Btz�Bt��Bv{Bv=qBv�HBx  BxQ�Bx��ByBzffBz�RB{�B|z�B|��B}G�B~=qB~�RB33B�{B�ffB���B���B�p�B��B��B�ffB���B��HB�G�B�B��B�=qB��RB���B��B���B��
B�{B�z�B��HB�
=B�p�B��
B��B�ffB��RB��HB�33B���B��
B�{B�z�B��HB���B�G�B��B�{B�=qB�z�B��HB�\)B��B�B�=qB��\B���B��B���B��B�{B��\B��HB��B���B�  B�=qB�z�B��HB�\)B���B��
B�Q�B��RB��HB�33B��B��B�(�B���B��B�G�B��B�{B�Q�B���B�33B�p�B��
B�Q�B���B��HB�p�B��
B�{B�ffB���B�\)B��B�{B��\B��HB�33B�B�  B�Q�B��HB�33B��B�  B�z�B���B��B��B��B�Q�B��HB�33B��B�(�B�ffB���B�\)B���B�(�B���B��HB�p�B��B�(�B���B�
=B�\)B�  B�Q�B���B�33B���B�  B��\B���B�p�B��
B�(�B��RB��B�p�B�{B�Q�B��RB�G�B���B�  B�z�B�
=B�\)B�B�Q�B��RB�
=B���B�{B�Q�B���B�p�B��B�(�B��RB�
=B���B�(�B�ffB��B���B��B��\B���B�\)B�{B�ffB��HB��B�  B�Q�B���BÅB��
B�ffB�
=B�\)B��
B�z�B��HB�G�B��B�ffBȸRB�G�B��B�Q�Bʣ�B�G�B��
B�(�Ḅ�B�G�B�B�(�BθRB�\)BϮB�(�B���B�33Bљ�B�(�B��HB�33Bә�B�Q�B��HB�33B�B�z�B���B�G�B��
B�z�B��HB�\)B�  B�z�B���B�p�B�  B�Q�B��HB�p�B�B�(�B��HB�33Bߙ�B�=qB�RB�33B�B�  B��B�33B�B��B�z�B�
=B�G�B��B�ffB���B�G�B��
B�=qB�\B�33B�B�  B��B��B�B��B�ffB�
=B�\)B�B�(�B�RB�G�BB�{B�RB�33B�p�B��B�\B���B�G�B�B�Q�B���B�G�B���B�{B��RB�
=B�p�B��
B�z�B���B�G�B�B�Q�B��HB��B��B�(�B���B��HB�\)B��B�(�B��\B��B���B��C �C Q�C ��C �
C  C�CffC��C�
C��C�CffC��C��C��C=qCz�C��C�
C�C\)C�\C�RC�C�Cp�C��C��C
=CQ�Cz�C�C�C=qCp�C��C��C{CQ�Cz�C��C�HC	33C	p�C	�\C	C

=C
Q�C
�C
��C
�C(�CffC�C�C{CG�Cz�CC  CQ�C�C�C��CG�Cp�C��C�C33Cp�C��C��C{C\)C��C�
C��C33C�CC�HC{Cp�C�C�C�CQ�C��C�HC(�C\)C�C��C{CffC��C��C
=C=qCz�CC
=CQ�C�\C�RC��C=qC�\C��C  C=qCz�C��C
=CG�Cz�C�C
=CG�Cp�C�C��C=qC�CC��C33CffC�C��C=qCz�C�RC�C(�Cp�CC 
=C =qC z�C �C!  C!Q�C!��C!��C"  C"G�C"��C"�
C#
=C#G�C#�C#�
C${C$Q�C$�C$�RC$�C%=qC%z�C%��C&  C&33C&ffC&��C&�C'=qC'p�C'�C'��C((�C(\)C(��C(�HC)�C)p�C)�C)�C*�C*Q�C*�C*��C+{C+\)C+��C+�HC,�C,Q�C,�\C,C-  C-=qC-�C-��C.
=C.G�C.�C.�RC.�HC/{C/Q�C/��C/�HC0�C0Q�C0�C0C0��C1(�C1ffC1��C1�C2(�C2ffC2��C2�
C3
=C3G�C3�C3��C4�C4\)C4��C4��C5
=C5=qC5�C5��C6
=C6Q�C6�\C6�HC7�C7\)C7��C7�
C8{C8Q�C8�\C8C9  C9G�C9�C9�RC9��C:G�C:�C:��C;  C;=qC;z�C;�RC;�C<�C<\)C<��C<�
C=�C=ffC=��C=�C>33C>p�C>�C>�C?�C?\)C?��C?�
C@{C@Q�C@��C@�HCA�CAffCA�CA�CB(�CB\)CB��CB�
CC�CC\)CC��CC�
CD{CDQ�CD�\CD��CE
=CEG�CE�CE��CF{CF\)CF��CF�HCG�CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�9XA�=qA�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�33A�(�A�$�A���A���AХ�AС�AП�AЛ�AЗ�AГuAБhAЏ\AЋDAЅA�v�A�jA��A��mAͅA�5?A��`A�x�A�hsA���A�G�AȁAǸRA�;dA�G�Aé�A��A���A�&�A�&�A��A��+A��\A�1'A�^5A��A�JA���A��wA�ZA��A�^5A��RA�ffA���A��jA�z�A���A��DA�p�A���A���A�t�A�9XA���A��;A�M�A��A�~�A�5?A���A��A��A�
=A��+A���A��A�9XA���A��TA�ƨA��
A��A�ĜA��#A�|�A��FA}l�Ay�-AxffAv�At~�As
=Aq�Ao�-Al��Aj^5Af~�Ae��Ae��AeoAdVAcA`ffA[�wAW�#AU"�AS�AO�AJ�AI��AF��AF-AE\)AC\)AA�mA@=qA>bA;��A;"�A8�HA8=qA7G�A4�A3�A1t�A/�A.v�A-&�A+A* �A(VA'��A&�DA#�A#�A!��A!%A ffA�HA�9AXA�DA��AXAoA�A�9A�uA=qAv�AC�A�`A�uA^5A��Ap�A�HA�DAQ�A1'A�A�mAO�A�DA�^A�A+A
1A	�TA	�FA	/A1A��A  A��Az�A~�A��A ��@��m@��m@�&�@�ȴ@�E�@�@�j@�@�\@�j@�ƨ@�~�@�~�@�=q@��@�/@��
@�p�@�9X@�ƨ@�"�@�~�@���@�X@��@�9X@�1@��;@ߝ�@�;d@�V@���@�O�@� �@�
=@�J@؃@��H@�?}@���@�Ĝ@ԣ�@�9X@��
@�S�@�o@ҸR@�7L@ϕ�@�v�@��@�hs@�/@��@�%@̼j@˥�@�@�
=@őh@��/@�1@���@��
@ÍP@�dZ@�S�@���@�5?@��-@�V@��@�j@�(�@�b@��
@�K�@�o@���@��y@�ƨ@�+@�S�@�S�@��\@��7@�p�@�G�@�G�@�/@���@���@���@��@��D@�Z@�(�@���@��@�33@��R@�~�@�M�@�{@���@���@�p�@�Ĝ@��j@�j@��@���@���@�|�@�33@��y@���@�E�@�ff@�v�@���@�C�@�l�@�K�@�@��R@�5?@�p�@��@���@���@��@�9X@�  @���@���@�|�@�+@�V@�J@��@���@�X@��j@�Z@�(�@��w@�dZ@�C�@���@�ȴ@���@�ff@�V@��@�@���@��@��7@�`B@�Z@���@��w@���@�l�@�C�@���@�E�@�@��#@���@���@���@�Z@� �@��@��@��R@���@���@��\@���@�ff@�5?@���@�p�@�`B@��@�Ĝ@��@��D@�bN@���@�|�@�
=@��y@�ȴ@�E�@�5?@��@���@�hs@�7L@��`@��u@�j@�b@�"�@��+@��@��T@��^@��7@��@�p�@�&�@���@���@���@�j@�b@��;@�ƨ@�|�@�+@�"�@�"�@�"�@��@���@�{@��#@��7@�X@���@���@�1'@���@���@��P@�dZ@�33@�@��@��+@�5?@�{@��T@�hs@���@��`@��9@�r�@�j@�A�@��@��@��@�\)@�o@���@�$�@��T@���@�/@��u@�(�@��@���@�\)@�C�@��R@�J@��^@�hs@�hs@�hs@�?}@���@��@�Z@�9X@� �@�b@�  @��m@��;@���@�dZ@���@�V@�$�@��@���@���@��7@��@�hs@�/@��`@��j@���@��@�Z@�b@��m@��
@���@��F@���@�t�@�t�@�
=@��@��@�ȴ@�-@���@�G�@���@���@��`@��9@�I�@�  @��;@��w@���@��@�+@�n�@��@��^@���@�X@��@��@��/@���@�Ĝ@���@�j@�I�@��@�w@K�@+@~�@~5?@}/@}V@|�@|��@|j@|(�@{�
@{��@{C�@z�H@z��@zM�@z-@z�@z�@y�@yG�@x�`@xr�@w�;@v�y@v5?@v@u�T@u�T@u�T@u��@u@uO�@u�@t��@t�@t�/@t��@t�@tj@t(�@s�m@s��@s"�@r�\@q�7@q7L@p��@pbN@pQ�@pA�@p1'@pb@o��@o|�@o�@n�@n�R@n��@n��@n�+@nv�@nff@nff@nE�@n{@m/@l��@l�j@l(�@j��@i�#@ix�@iX@i7L@i%@h�9@h �@g\)@g;d@g
=@f�@fff@e?}@d�@dj@d9X@d1@c��@b�@b��@b�@a��@aG�@`��@`��@`�`@`A�@_K�@^��@^��@^v�@^ff@^$�@]�@]V@\�/@\�j@\�D@\(�@[��@[ƨ@[�@[o@Z�@Z��@Y�#@Y%@W�@W�w@W�@W�P@W\)@W
=@Vff@U�-@U/@T��@T�@TZ@S��@S��@SS�@S@R��@Q��@Q�7@Q�@P�@O�;@Ol�@N�y@N��@N�+@M��@M?}@MV@L�@L�@LZ@L1@K�
@K��@KC�@K33@K"�@K"�@Ko@K@J�@J��@J~�@J=q@IG�@I%@Hr�@G��@F�+@F$�@E��@E`B@D��@D��@Dz�@Dj@DZ@D�@C�m@CdZ@B��@B~�@B^5@B-@A�^@Ahs@A&�@@�`@@bN@?�@?�P@?|�@?K�@>�y@>�R@>��@>ff@=�@=�h@=?}@<��@<�j@<�D@<Z@<�@;�
@;ƨ@;��@;�@;t�@;S�@;o@:n�@:=q@:=q@:-@:J@97L@8Q�@8A�@8  @7�P@7|�@7l�@7l�@7+@6��@5�T@5/@5�@5V@5V@5V@4z�@3�F@3t�@3S�@3C�@3C�@333@3o@2��@2��@2�\@2~�@2M�@2=q@1��@1��@1hs@0��@0�@0  @/|�@/;d@/�@.��@.�@.ȴ@.��@.�+@.V@.5?@-@-�-@-��@-�h@-�@-p�@-`B@-?}@-V@,�@,z�@+��@*�@*��@*=q@)��@)x�@)X@)&�@(Ĝ@(�u@(Q�@(b@(  @'��@'��@'��@'�@'\)@'K�@';d@';d@'+@'+@'+@'+@'�@'�@&�y@&�+@&V@&5?@%�-@%�h@%�@%`B@$��@$��@$��@$��@$z�@$j@$Z@$I�@$(�@$1@#��@#�m@#ƨ@#�F@#��@#S�@#33@#"�@#o@"�@"�H@"�H@"��@"~�@"M�@"-@!�@!hs@ Ĝ@ �9@ �u@ �@ 1'@|�@;d@�@
=@��@ff@E�@5?@{@�T@��@�h@`B@O�@?}@/@�@��@�@�/@�/@��@�D@�@dZ@n�@-@J@J@��@�#@�#@�^@�7@G�@��@�9@�u@Q�@  @�;@�@|�@+@�@�y@�R@V@$�@$�@{@��@@�@?}@?}@V@�/@�/@��@�j@�D@I�@(�@�@1@�
@t�@33@"�@o@�\@-@��@�#@�^@��@��@�7@�7@x�@X@X@G�@�@��@�9@��@��@�u@Q�@b@�w@�P@\)@�@
=@ȴ@E�@$�@{@�@��@O�@V@��@�/@�@�D@j@Z@I�@(�@�m@�
@�F@��@�@dZ@C�@"�@
�H@
n�@
=q@
�@	��@	x�A�9XA�9XA�9XA�;dA�9XA�9XA�=qA�;dA�?}A�?}A�=qA�A�A�;dA�=qA�?}A�;dA�?}A�A�A�;dA�?}A�?}A�;dA�=qA�?}A�9XA�=qA�=qA�;dA�A�A�;dA�?}A�?}A�;dA�?}A�=qA�;dA�A�A�;dA�;dA�?}A�9XA�=qA�?}A�9XA�=qA�33A�33A�1'A�&�A�$�A�"�A�&�A�&�A�$�A�33A�/A�1'A�33A��A��A��A��A��A�1A�
=A���A��TA���A�ȴA��
A���Aк^AиRAЬAЧ�AЩ�AЩ�AС�AУ�AС�AП�AП�AУ�AС�AП�AУ�AП�AП�AУ�AН�AС�AН�AЛ�AП�AЛ�AЛ�AН�AЙ�AЗ�AЛ�AЗ�AЕ�AЛ�AЗ�AЕ�AЗ�AЕ�AЏ\AГuAЕ�AЏ\AГuAЕ�AБhAЏ\AГuAГuAЏ\AБhAГuAЋDAЍPAЍPAЋDAЉ7AЍPAЋDAЇ+AЋDAЉ7AЅAЉ7AЁA�|�A�~�A�v�A�x�A�v�A�r�A�v�A�t�A�p�A�r�A�n�A�hsA�ffA�^5A�Q�A�K�A�E�A�/A���Aϣ�Aω7A�jA�/A�+A�ȴA�9XA�A��HA���A͓uA�ffA�ZA�A�A��A��`ȂhA� �A�A��`A˺^A˓uA�\)A� �A��Aʧ�Aʕ�AʋDAʇ+A�|�A�t�A�v�A�z�A�v�A�t�A�t�A�z�A�x�A�z�A�|�A�r�A�p�A�z�A�r�A�r�A�t�A�ffA�dZA�^5A�XA�M�A�E�A�=qA�(�A��A�JA�A���A���A���A��A��mA��TA��/A���A���A�Aɣ�Aɇ+A�l�A�O�A�7LA�"�A��A���A���AȲ-Aȏ\AȑhAȏ\AȋDAȍPAȏ\AȅAȃA�~�A�t�A�p�A�l�A�dZA�XA�Q�A�1'A��AǶFAǬAǝ�AǑhAǗ�AǛ�AǑhA�x�A�9XA��yA���AƾwAƧ�AƍPA�dZA�A�A�7LA��Aŧ�A���Aİ!AđhA�t�A�^5A�O�A�C�A�;dA�9XA�33A�(�A��A�oA���A��A��yA��;A�AöFAé�Aß�AÙ�AÅA�VA�=qA�&�A���A�ȴA�A�/A�A���A�XA�"�A���A�ĜA��hA�l�A� �A�  A��A��HA���A���A�t�A�1'A� �A���A���A�1'A��A��#A��^A���A�\)A�C�A��`A��7A�`BA�-A�1A���A���A���A�l�A��A��wA���A�jA�%A���A���A���A�v�A�;dA��A���A�ĜA���A�n�A�Q�A�A���A�p�A�C�A�5?A��A���A�^5A��A���A��mA��RA��HA���A�^5A�VA�E�A��TA��`A��!A��A��+A��hA��PA��hA��7A�C�A�(�A�{A��A�ȴA���A��PA�~�A�v�A�n�A�A�A�-A�"�A��A��A�bA�
=A�%A��A��;A��TA��/A�ƨA��A��hA�~�A�hsA�?}A�7LA��A�  A��
A��A�z�A�dZA�C�A�VA��yA���A�t�A�?}A�oA��A���A���A�ZA�$�A��A�ĜA���A���A��A�r�A�5?A�O�A��A���A��`A���A���A�A��wA���A��jA��9A��9A��-A��A��A��-A���A��A�|�A�\)A�9XA�oA��
A�|�A�;dA�
=A���A���A�x�A�XA�?}A�&�A�VA���A��#A��-A���A�ffA�5?A�1A��A���A���A�~�A�XA�1'A��A�VA�A���A��#A��!A��A�r�A�p�A�`BA�\)A�M�A�{A���A�^5A�XA�G�A�5?A�{A��A���A��A��DA�p�A�VA�-A�{A�
=A�A���A��yA���A���A�l�A�O�A�E�A�?}A�(�A���A��wA���A��A�r�A�jA�dZA�VA�7LA�1A���A��RA��A�XA�O�A�C�A��A��A��jA�~�A�
=A��!A�M�A��A���A��A�E�A��A�A��A�ƨA���A��DA��PA��A�^5A�JA��yA��`A��;A��;A��HA��;A��#A���A�ĜA���A�~�A�7LA��`A��A��A�$�A��FA�ffA�?}A�9XA�5?A�(�A�"�A��A�A��;A��RA��A�XA���A�A�A��A��#A��^A��9A��-A���A��uA�|�A�ffA�5?A�A�x�A�9XA�$�A�"�A��A�{A�A��/A�z�A���A�C�A�(�A�&�A�"�A��A��A�  A��A���A��9A��-A���A��A�ZA�S�A�S�A�I�A�I�A�?}A��A���A��mA��`A��A���A���A���A���A��7A�l�A�ZA�Q�A�7LA�(�A��A�JA���A��A��A���A��FA���A�|�A�ZA� �A��wA�S�A�A�A�=qA�;dA�G�A�I�A�E�A�A�A�;dA��A��A��A�A���A��mA��yA��A��A��A��`A��TA��HA��TA��`A��TA��TA��;A��;A���A���A�ȴA�ƨA�A��9A���A���A��A�ffA�(�A��A�O�A��mA���A�n�A�ffA�\)A�K�A�"�A���A�M�A��A�1A���A���A�Q�A���A�M�A��;A��A�+A��A��A�S�A�A��PA�jA�A��mA��HA��A�A��RA���A��PA�O�A��A���A�XA�(�A�  A��HA���A��uA�?}A���A�ZA��A~r�A}�A}p�A|r�A{\)Az�yAz��Az(�Ay��Ay�^Ay�Ay��AyS�Ay�Ax��Ax�HAx��Ax��AxbNAx1'Aw�AwAw��Aw�Aw\)Aw33Av��Avz�Av=qAvJAu�
Au��Au�At��At�uAtM�As�;As�-As`BAs33As33As+As�As�Ar��Ar�Ar�!Arz�ArVAr-ArAq�mAq��Aq��AqdZAp�HApn�ApAoƨAo�hAohsAo?}An�An^5Am��AmC�Am%Alr�Ak�AkAk��Akt�Aj��AjM�Aj9XAj9XAj-Aj$�Ai��Ag�^AfffAfA�Af9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            A�9XA�9XA�=qA�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�33A�(�A�$�A���A���AХ�AС�AП�AЛ�AЗ�AГuAБhAЏ\AЋDAЅA�v�A�jA��A��mAͅA�5?A��`A�x�A�hsA���A�G�AȁAǸRA�;dA�G�Aé�A��A���A�&�A�&�A��A��+A��\A�1'A�^5A��A�JA���A��wA�ZA��A�^5A��RA�ffA���A��jA�z�A���A��DA�p�A���A���A�t�A�9XA���A��;A�M�A��A�~�A�5?A���A��A��A�
=A��+A���A��A�9XA���A��TA�ƨA��
A��A�ĜA��#A�|�A��FA}l�Ay�-AxffAv�At~�As
=Aq�Ao�-Al��Aj^5Af~�Ae��Ae��AeoAdVAcA`ffA[�wAW�#AU"�AS�AO�AJ�AI��AF��AF-AE\)AC\)AA�mA@=qA>bA;��A;"�A8�HA8=qA7G�A4�A3�A1t�A/�A.v�A-&�A+A* �A(VA'��A&�DA#�A#�A!��A!%A ffA�HA�9AXA�DA��AXAoA�A�9A�uA=qAv�AC�A�`A�uA^5A��Ap�A�HA�DAQ�A1'A�A�mAO�A�DA�^A�A+A
1A	�TA	�FA	/A1A��A  A��Az�A~�A��A ��@��m@��m@�&�@�ȴ@�E�@�@�j@�@�\@�j@�ƨ@�~�@�~�@�=q@��@�/@��
@�p�@�9X@�ƨ@�"�@�~�@���@�X@��@�9X@�1@��;@ߝ�@�;d@�V@���@�O�@� �@�
=@�J@؃@��H@�?}@���@�Ĝ@ԣ�@�9X@��
@�S�@�o@ҸR@�7L@ϕ�@�v�@��@�hs@�/@��@�%@̼j@˥�@�@�
=@őh@��/@�1@���@��
@ÍP@�dZ@�S�@���@�5?@��-@�V@��@�j@�(�@�b@��
@�K�@�o@���@��y@�ƨ@�+@�S�@�S�@��\@��7@�p�@�G�@�G�@�/@���@���@���@��@��D@�Z@�(�@���@��@�33@��R@�~�@�M�@�{@���@���@�p�@�Ĝ@��j@�j@��@���@���@�|�@�33@��y@���@�E�@�ff@�v�@���@�C�@�l�@�K�@�@��R@�5?@�p�@��@���@���@��@�9X@�  @���@���@�|�@�+@�V@�J@��@���@�X@��j@�Z@�(�@��w@�dZ@�C�@���@�ȴ@���@�ff@�V@��@�@���@��@��7@�`B@�Z@���@��w@���@�l�@�C�@���@�E�@�@��#@���@���@���@�Z@� �@��@��@��R@���@���@��\@���@�ff@�5?@���@�p�@�`B@��@�Ĝ@��@��D@�bN@���@�|�@�
=@��y@�ȴ@�E�@�5?@��@���@�hs@�7L@��`@��u@�j@�b@�"�@��+@��@��T@��^@��7@��@�p�@�&�@���@���@���@�j@�b@��;@�ƨ@�|�@�+@�"�@�"�@�"�@��@���@�{@��#@��7@�X@���@���@�1'@���@���@��P@�dZ@�33@�@��@��+@�5?@�{@��T@�hs@���@��`@��9@�r�@�j@�A�@��@��@��@�\)@�o@���@�$�@��T@���@�/@��u@�(�@��@���@�\)@�C�@��R@�J@��^@�hs@�hs@�hs@�?}@���@��@�Z@�9X@� �@�b@�  @��m@��;@���@�dZ@���@�V@�$�@��@���@���@��7@��@�hs@�/@��`@��j@���@��@�Z@�b@��m@��
@���@��F@���@�t�@�t�@�
=@��@��@�ȴ@�-@���@�G�@���@���@��`@��9@�I�@�  @��;@��w@���@��@�+@�n�@��@��^@���@�X@��@��@��/@���@�Ĝ@���@�j@�I�@��@�w@K�@+@~�@~5?@}/@}V@|�@|��@|j@|(�@{�
@{��@{C�@z�H@z��@zM�@z-@z�@z�@y�@yG�@x�`@xr�@w�;@v�y@v5?@v@u�T@u�T@u�T@u��@u@uO�@u�@t��@t�@t�/@t��@t�@tj@t(�@s�m@s��@s"�@r�\@q�7@q7L@p��@pbN@pQ�@pA�@p1'@pb@o��@o|�@o�@n�@n�R@n��@n��@n�+@nv�@nff@nff@nE�@n{@m/@l��@l�j@l(�@j��@i�#@ix�@iX@i7L@i%@h�9@h �@g\)@g;d@g
=@f�@fff@e?}@d�@dj@d9X@d1@c��@b�@b��@b�@a��@aG�@`��@`��@`�`@`A�@_K�@^��@^��@^v�@^ff@^$�@]�@]V@\�/@\�j@\�D@\(�@[��@[ƨ@[�@[o@Z�@Z��@Y�#@Y%@W�@W�w@W�@W�P@W\)@W
=@Vff@U�-@U/@T��@T�@TZ@S��@S��@SS�@S@R��@Q��@Q�7@Q�@P�@O�;@Ol�@N�y@N��@N�+@M��@M?}@MV@L�@L�@LZ@L1@K�
@K��@KC�@K33@K"�@K"�@Ko@K@J�@J��@J~�@J=q@IG�@I%@Hr�@G��@F�+@F$�@E��@E`B@D��@D��@Dz�@Dj@DZ@D�@C�m@CdZ@B��@B~�@B^5@B-@A�^@Ahs@A&�@@�`@@bN@?�@?�P@?|�@?K�@>�y@>�R@>��@>ff@=�@=�h@=?}@<��@<�j@<�D@<Z@<�@;�
@;ƨ@;��@;�@;t�@;S�@;o@:n�@:=q@:=q@:-@:J@97L@8Q�@8A�@8  @7�P@7|�@7l�@7l�@7+@6��@5�T@5/@5�@5V@5V@5V@4z�@3�F@3t�@3S�@3C�@3C�@333@3o@2��@2��@2�\@2~�@2M�@2=q@1��@1��@1hs@0��@0�@0  @/|�@/;d@/�@.��@.�@.ȴ@.��@.�+@.V@.5?@-@-�-@-��@-�h@-�@-p�@-`B@-?}@-V@,�@,z�@+��@*�@*��@*=q@)��@)x�@)X@)&�@(Ĝ@(�u@(Q�@(b@(  @'��@'��@'��@'�@'\)@'K�@';d@';d@'+@'+@'+@'+@'�@'�@&�y@&�+@&V@&5?@%�-@%�h@%�@%`B@$��@$��@$��@$��@$z�@$j@$Z@$I�@$(�@$1@#��@#�m@#ƨ@#�F@#��@#S�@#33@#"�@#o@"�@"�H@"�H@"��@"~�@"M�@"-@!�@!hs@ Ĝ@ �9@ �u@ �@ 1'@|�@;d@�@
=@��@ff@E�@5?@{@�T@��@�h@`B@O�@?}@/@�@��@�@�/@�/@��@�D@�@dZ@n�@-@J@J@��@�#@�#@�^@�7@G�@��@�9@�u@Q�@  @�;@�@|�@+@�@�y@�R@V@$�@$�@{@��@@�@?}@?}@V@�/@�/@��@�j@�D@I�@(�@�@1@�
@t�@33@"�@o@�\@-@��@�#@�^@��@��@�7@�7@x�@X@X@G�@�@��@�9@��@��@�u@Q�@b@�w@�P@\)@�@
=@ȴ@E�@$�@{@�@��@O�@V@��@�/@�@�D@j@Z@I�@(�@�m@�
@�F@��@�@dZ@C�@"�@
�H@
n�@
=q@
�@	��@	x�A�9XA�9XA�9XA�;dA�9XA�9XA�=qA�;dA�?}A�?}A�=qA�A�A�;dA�=qA�?}A�;dA�?}A�A�A�;dA�?}A�?}A�;dA�=qA�?}A�9XA�=qA�=qA�;dA�A�A�;dA�?}A�?}A�;dA�?}A�=qA�;dA�A�A�;dA�;dA�?}A�9XA�=qA�?}A�9XA�=qA�33A�33A�1'A�&�A�$�A�"�A�&�A�&�A�$�A�33A�/A�1'A�33A��A��A��A��A��A�1A�
=A���A��TA���A�ȴA��
A���Aк^AиRAЬAЧ�AЩ�AЩ�AС�AУ�AС�AП�AП�AУ�AС�AП�AУ�AП�AП�AУ�AН�AС�AН�AЛ�AП�AЛ�AЛ�AН�AЙ�AЗ�AЛ�AЗ�AЕ�AЛ�AЗ�AЕ�AЗ�AЕ�AЏ\AГuAЕ�AЏ\AГuAЕ�AБhAЏ\AГuAГuAЏ\AБhAГuAЋDAЍPAЍPAЋDAЉ7AЍPAЋDAЇ+AЋDAЉ7AЅAЉ7AЁA�|�A�~�A�v�A�x�A�v�A�r�A�v�A�t�A�p�A�r�A�n�A�hsA�ffA�^5A�Q�A�K�A�E�A�/A���Aϣ�Aω7A�jA�/A�+A�ȴA�9XA�A��HA���A͓uA�ffA�ZA�A�A��A��`ȂhA� �A�A��`A˺^A˓uA�\)A� �A��Aʧ�Aʕ�AʋDAʇ+A�|�A�t�A�v�A�z�A�v�A�t�A�t�A�z�A�x�A�z�A�|�A�r�A�p�A�z�A�r�A�r�A�t�A�ffA�dZA�^5A�XA�M�A�E�A�=qA�(�A��A�JA�A���A���A���A��A��mA��TA��/A���A���A�Aɣ�Aɇ+A�l�A�O�A�7LA�"�A��A���A���AȲ-Aȏ\AȑhAȏ\AȋDAȍPAȏ\AȅAȃA�~�A�t�A�p�A�l�A�dZA�XA�Q�A�1'A��AǶFAǬAǝ�AǑhAǗ�AǛ�AǑhA�x�A�9XA��yA���AƾwAƧ�AƍPA�dZA�A�A�7LA��Aŧ�A���Aİ!AđhA�t�A�^5A�O�A�C�A�;dA�9XA�33A�(�A��A�oA���A��A��yA��;A�AöFAé�Aß�AÙ�AÅA�VA�=qA�&�A���A�ȴA�A�/A�A���A�XA�"�A���A�ĜA��hA�l�A� �A�  A��A��HA���A���A�t�A�1'A� �A���A���A�1'A��A��#A��^A���A�\)A�C�A��`A��7A�`BA�-A�1A���A���A���A�l�A��A��wA���A�jA�%A���A���A���A�v�A�;dA��A���A�ĜA���A�n�A�Q�A�A���A�p�A�C�A�5?A��A���A�^5A��A���A��mA��RA��HA���A�^5A�VA�E�A��TA��`A��!A��A��+A��hA��PA��hA��7A�C�A�(�A�{A��A�ȴA���A��PA�~�A�v�A�n�A�A�A�-A�"�A��A��A�bA�
=A�%A��A��;A��TA��/A�ƨA��A��hA�~�A�hsA�?}A�7LA��A�  A��
A��A�z�A�dZA�C�A�VA��yA���A�t�A�?}A�oA��A���A���A�ZA�$�A��A�ĜA���A���A��A�r�A�5?A�O�A��A���A��`A���A���A�A��wA���A��jA��9A��9A��-A��A��A��-A���A��A�|�A�\)A�9XA�oA��
A�|�A�;dA�
=A���A���A�x�A�XA�?}A�&�A�VA���A��#A��-A���A�ffA�5?A�1A��A���A���A�~�A�XA�1'A��A�VA�A���A��#A��!A��A�r�A�p�A�`BA�\)A�M�A�{A���A�^5A�XA�G�A�5?A�{A��A���A��A��DA�p�A�VA�-A�{A�
=A�A���A��yA���A���A�l�A�O�A�E�A�?}A�(�A���A��wA���A��A�r�A�jA�dZA�VA�7LA�1A���A��RA��A�XA�O�A�C�A��A��A��jA�~�A�
=A��!A�M�A��A���A��A�E�A��A�A��A�ƨA���A��DA��PA��A�^5A�JA��yA��`A��;A��;A��HA��;A��#A���A�ĜA���A�~�A�7LA��`A��A��A�$�A��FA�ffA�?}A�9XA�5?A�(�A�"�A��A�A��;A��RA��A�XA���A�A�A��A��#A��^A��9A��-A���A��uA�|�A�ffA�5?A�A�x�A�9XA�$�A�"�A��A�{A�A��/A�z�A���A�C�A�(�A�&�A�"�A��A��A�  A��A���A��9A��-A���A��A�ZA�S�A�S�A�I�A�I�A�?}A��A���A��mA��`A��A���A���A���A���A��7A�l�A�ZA�Q�A�7LA�(�A��A�JA���A��A��A���A��FA���A�|�A�ZA� �A��wA�S�A�A�A�=qA�;dA�G�A�I�A�E�A�A�A�;dA��A��A��A�A���A��mA��yA��A��A��A��`A��TA��HA��TA��`A��TA��TA��;A��;A���A���A�ȴA�ƨA�A��9A���A���A��A�ffA�(�A��A�O�A��mA���A�n�A�ffA�\)A�K�A�"�A���A�M�A��A�1A���A���A�Q�A���A�M�A��;A��A�+A��A��A�S�A�A��PA�jA�A��mA��HA��A�A��RA���A��PA�O�A��A���A�XA�(�A�  A��HA���A��uA�?}A���A�ZA��A~r�A}�A}p�A|r�A{\)Az�yAz��Az(�Ay��Ay�^Ay�Ay��AyS�Ay�Ax��Ax�HAx��Ax��AxbNAx1'Aw�AwAw��Aw�Aw\)Aw33Av��Avz�Av=qAvJAu�
Au��Au�At��At�uAtM�As�;As�-As`BAs33As33As+As�As�Ar��Ar�Ar�!Arz�ArVAr-ArAq�mAq��Aq��AqdZAp�HApn�ApAoƨAo�hAohsAo?}An�An^5Am��AmC�Am%Alr�Ak�AkAk��Akt�Aj��AjM�Aj9XAj9XAj-Aj$�Ai��Ag�^AfffAfA�Af9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�MB�MB��B�MB��B�MB��B��B��B��B��B�MB�B��B��B�{B��B��B�B� B��B�iB�4B�4B� B� B.B~�B}�B|�B.B��B��B��B�5B�BB)�B?HBB�BO�B�AB��B�B�VB��B�IB�"BzBc�BOvBA�B;�BC-BDgBOBBO�BMjBE�B=�B1�B2�B0�B)*B�B@B�B��B�B�)B��B�jB��B��B��B_pBOB6FB%B
��B
�AB
�B
�B
��B
��B
�UB
��B
��B
��B
��B
�:B
y�B
S[B
:�B
%�B
�B	�B	�jB	�gB	�^B	�qB	��B	��B	��B	�_B	��B	�"B	��B	�(B	��B	�%B	~�B	l�B	L�B	6zB	'B	IB	B��B��B�oB�B�B�?BʌB�B��B��B�tB��B��B��B��B�OB��B��B��B��B��B��B�+B��B�GB.B�B~(B~�B.B��B|�Bz�BuZBv`Br�BsBrBqBo5Bp;BkBf�BffBe`Bg8Bf2BjKBkBl�Bm]Bn/Bn/Bq�Br�Bs�Bs�Bs�Bm�Bm)Bk�BjBhsBqvBq�Bp�Bo Bq�BrGBpBn/BiyBg�BdZBbNBc Ba�BbBc�BaHBc�B_�Bf�Bm]Bu�Bu�B{JBcB��B�oB�GB�B��B��B��B��B�eB��B�qB��B��B��B��B��B�eB�IB�B��B��B�gB�3B�B�EBȀB��B˒B�jB��BߤB�TB�B�
B�DB�B��B�B�"B��B� B�AB��B�2B�fB�xB�(B��B	{B	�B	
	B	
�B	"B	�B	{B	�B	�B	�B	$@B	)�B	-�B	0UB	:*B	CaB	IB	N�B	X�B	[WB	a|B	f2B	h
B	kQB	m)B	n/B	o�B	o�B	poB	rGB	sB	s�B	v�B	xlB	w�B	y�B	|PB	~]B	.B	~�B	� B	��B	�xB	�B	�~B	��B	��B	��B	��B	�"B	�.B	�B	�B	�eB	�B	�@B	�_B	��B	�!B	��B	��B	�B	�tB	��B	�$B	�dB	�B	��B	�B	�HB	�}B	��B	�'B	��B	�9B	ŢB	ƨB	�tB	ƨB	�EB	ʌB	�B	�#B	�0B	��B	�B	�B	�}B	҉B	ӏB	�,B	՛B	՛B	�2B	��B	�sB	�B	�B	خB	�B	�B	��B	��B	�dB	�B	��B	�BB	��B	��B	�B	��B	�B	�B	�B	�BB	�vB	�B	�HB	��B	�,B	�2B	�sB	�B	�KB	�B	�B	��B	�cB	��B	��B	�iB	�;B	��B	�B	�|B	��B	��B	��B	�B	�B	�%B	�fB	�8B	��B	�rB	��B	�DB	��B	�JB	�B	��B	��B	��B	�B	��B	��B	��B	��B	�.B	�.B	��B	��B
  B
 4B
uB
uB
�B
�B
�B
�B
YB
�B
�B
_B
�B
1B
1B
�B
	lB
	�B
	�B

rB
xB
B
B
�B
"B
�B
VB
VB
(B
(B
\B
\B
.B
�B
hB
hB
oB
@B
@B
uB
�B
�B
uB
FB
�B
�B
B
�B
�B
�B
B
B
YB
YB
$B
YB
�B
�B
+B
_B
1B
B
7B
=B
�B
7B
B
kB
�B
�B
CB
IB
IB
IB
IB
~B
B
�B
�B
�B
�B
B
�B
�B
VB
�B
�B
 'B
�B
�B
�B
"hB
#B
#nB
$tB
%�B
%�B
%�B
&B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
($B
(�B
)*B
)*B
)*B
)*B
)_B
)�B
)_B
)�B
*0B
*�B
*�B
+B
,=B
-B
,�B
,�B
-CB
-CB
-wB
-�B
-�B
.}B
.�B
/B
/�B
/�B
/�B
/�B
/�B
1'B
1�B
2-B
2�B
2�B
3hB
4B
4nB
4�B
4�B
5tB
5�B
6�B
7B
7LB
7LB
7LB
7LB
7�B
7�B
7�B
7�B
8B
8�B
8�B
8�B
9$B
:�B
:^B
:�B
:^B
:�B
:�B
:�B
:�B
:�B
;0B
;dB
;dB
;dB
;�B
;�B
;�B
;�B
;�B
;�B
<�B
<jB
<6B
<jB
;dB
9�B
9XB
9$B
8�B
8�B
8�B
9$B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8RB
8RB
8�B
8�B
:*B
:^B
:�B
:�B
:�B
:^B
:�B
;�B
;�B
;�B
;�B
;�B
<B
<�B
<�B
<�B
<�B
<�B
=qB
=<B
=�B
=�B
>B
>B
>BB
?B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
AUB
A�B
B[B
B[B
B�B
B�B
C-B
CaB
C�B
C�B
D3B
EB
E9B
E�B
FB
GB
GEB
HB
G�B
G�B
H�B
H�B
IRB
I�B
I�B
I�B
JXB
JXB
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
L�B
M6B
M�B
N�B
N�B
OB
OBB
O�B
O�B
O�B
O�B
O�B
PB
PB
P�B
QNB
Q�B
QNB
Q�B
Q�B
R B
RTB
R�B
S&B
S�B
S�B
S�B
S�B
TaB
TaB
TaB
T�B
T�B
UgB
U�B
U�B
VB
V9B
V9B
V�B
V�B
V�B
W
B
V�B
W
B
W
B
WsB
XEB
XB
W�B
W�B
W�B
YB
YB
YB
Y�B
ZB
ZB
ZB
ZB
ZQB
ZB
\)B
\)B
\)B
\)B
[�B
[�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^5B
^jB
^5B
^5B
^jB
^5B
^�B
_B
_;B
_�B
`B
`�B
aB
a|B
a|B
a�B
a�B
a�B
a�B
bB
bNB
b�B
cTB
c B
cTB
cTB
cTB
cTB
cTB
c�B
c�B
c�B
c�B
e�B
e�B
f2B
f�B
gB
g8B
g8B
g8B
gmB
g�B
g�B
h>B
h>B
h�B
h�B
h�B
h�B
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
i�B
jB
jKB
jKB
kB
kB
kB
kB
k�B
k�B
l"B
l"B
lWB
lWB
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m]B
m]B
m�B
m�B
m�B
m]B
m�B
m�B
m�B
n/B
n�B
o5B
o B
o B
o B
o�B
p;B
p;B
poB
p;B
p�B
p�B
qB
qB
qB
qAB
qvB
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rB
q�B
q�B
rB
rB
r|B
s�B
s�B
tB
tTB
tTB
tTB
tTB
t�B
tTB
t�B
t�B
t�B
u%B
u%B
uZB
u�B
u�B
u�B
v+B
v�B
v+B
v�B
v�B
wfB
wfB
wfB
v�B
wfB
wfB
wfB
w�B
w�B
xB
xB
xB
x8B
x8B
x�B
x�B
x�B
x�B
y	B
y>B
yrB
y�B
y>B
y>B
y�B
zB
zB
zDB
zDB
zxB
zxB
zxB
zxB
zxB
z�B
zxB
z�B
z�B
{JB
{B
{JB
{JB
{B
{B
|B
|�B
|�B
}"B
}VB
}VB
}�B
~(B
~(B
~(B
~(B
~�B
~�B
.B
.B
cB
�B
�B
�B
�B
�B
�4B
�iB
�iB
��B
��B
��B
�;B
�;B
�;B
��B
��B
�B
�AB
�GB
�uB�iB��B��B��B�B��B��B��B�B��B�SB��B�SB��B��B�SB�MB��B��B�B�B�SB��B�B��B�B��B�B�B�SB�B��B��B��B��B�B��B�B�MB�B��B��B�GB��B�{B��B��B��B�B��B�{B�uB�AB��B�B�MB�B��B��B�MB�GB�YB�%B��B�B�B��B~�B�oB{�B�uB�;B�uB~(B�iB�BcB�oB�4B~(B�B��B~�B�iB�iB.B��B�iB�B�oB�B�iB��BcB��B��BcB��B�oB.B��B�;B.B�iB�B�B�B��B�4B.B�;B�BcB� B�;BcB.B��B�4B~�B��B�B~]B.B�4B~]BcB�B}�B~�B�B}�B�B�B}"B.B}�B}�B~(B|�B}"B}VB{JB|B}VB|�B{�Bz�B{�B{JB|�B��B��B��B��B�%B��B��B�B�'B��B�:B��B��B�nB�^B��B�B�UB��B��B�zB�^B�}BݘB�B�MB�MB��B 4B�BMBMBSB	lB\BoBBBB�BMB�BSBB�B�BIBVB$@B$�B"4B#�B$�B#:B(XB%�B%�B'�B)*B)*B'�B+6B,�B,�B.�B0UB0�B1[B8�B:^B>�B@�BCaBCaBB�BH�BG�BH�BF?BC-BC�BC�BB�BA�BDgBB'BA�BC-BB�B@�BAUBB[B@OBF?BH�B@�B=BAUB@OBH�BYBc Bm]B{�B{Bw�Bz�B{�B|B��B�iB{�B}VB��B�1B��B�.B�B�:B��B��B� B�(B��B��B�hB��B�B��B��B�.B��B�:B�bB��B��B��B�YB�oB��B�YB��B��B�hB�B�qB��B��B��B��B��B��B��B�CB�xB��B�B�hB�B��B��B�B��B�bB��B��B��B��B�B�B��B��B��B��B��B�%B�fB��B�B��B�B~�B|PB�MBrBpBjBu�Bk�Bg�BhsBkQBb�B`B[�BcTB]�BW�BVBR�B[#BS[BP}BOBDgBCaBGEBRTB;�B:^B:*B:�BVB8RBUgB:�B8�B7LB9$B8�B8BIRB<�B<6BEBAUBB�BC�B?BA�BC�BIBC�BD�BCaBB�BD3BC�BB�BHKBFtBEmBK)BQ�BR BP}BQ�BTaBU2BO�BQBQBQ�BP�BOBK�BN�BO�BN�BNBQNBOBK^BI�BMBHKBPHBGzBJ�BFtBA B>B>�B<�BFBOvB<jB6zB6�B5tB0�B1�B2�B1'B1'B2�B1[B/�B1[B0�B/B4B4nB0UB4nB33B49B6FB6FB2aB+�B5tB-�B.IB-�B+�B*eB*�B*0B*0B&�B'�B)_B&�B&�B"hB!bB �B!bB�B�B1B_B�B�B�B�BeB�B
�B�BBB�B$B�B��B�B�(B�]B�B��B��B�B�B�fB�lB�B�GB�B�5B�B�5B�B�B�>B��B�B�&B�B�2B�)BںBخB֡B�2BԕB՛BچB҉B��B�aBȴB�BÖB��B�)BӏB�#B�B�pB��B��B�LB�B��B��B��B�zB�_B��B�\B��B��B��B��B�kB��B�YB�SB�FB��B�B��B��B�uB�\B��B�bB��B��BcBzBtBe�B^B]�B`BB]dB^5B`vB]�B]�BY�BT,Bd�BX�BA�BE9B?}B:^B8�B7�B9$B6�B5�B7�B;0B1�B*�B 'B�BIBBVB�B)�BHB
��B
�DB
��B
�B
�B
��B
�rB�B
��B
�B
�B
�TB
�B
�iB
�B
�]B
�/B
�B
�DB
� B
�B
��B
�B
�B
��B
��B
�
B
�B
�B
�B
�B
�5B
�B
یB
ݘB
چB
یB
ٴB
�sB
�mB
՛B
�mB
�B
ԕB
خB
��B
��B
ɺB
��B
��B
�B
��B
��B
� B
��B
�[B
��B
��B
�BB
�OB
�<B
�jB
��B
�dB
�B
�BB
�6B
�6B
��B
��B
��B
�dB
��B
��B
��B
��B
�B
��B
�zB
�LB
��B
�hB
�[B
��B
�'B
��B
��B
�B
�B
��B
�oB
��B
� B
��B
��B
�	B
�YB
}"B
}"B
�B
��B
{JB
zxB
rGB
n�B
iDB
XB
bB
VmB
^�B
J#B
HB
O�B
?�B
>BB
>BB
=B
7�B
:�B
6B
?�B
;�B
4nB
6B
'�B
)�B
!�B
OB
%�B
=B
/B
kB
$B
$@B	�.B	��B
DB	�"B	�B	�oB	��B	�"B	��B	�B	��B	��B	�B	�B	�dB	�B	�B	�B	�dB	ݘB	��B	�KB	�EB	�?B	֡B	خB	՛B	ΥB	�B	͟B	��B	�NB	ʌB	ʌB	��B	�0B	��B	ȴB	�B	��B	�HB	��B	��B	�B	��B	�dB	�B	�6B	��B	��B	�B	��B	�B	��B	��B	�nB	��B	��B	�B	�_B	��B	�=B	��B	�-B	�tB	�zB	��B	�4B	�~B	��B	��B	�B	��B	��B	�uB	�:B	�.B	��B	��B	��B	�\B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                            B��B��B��B��B�B��B�B�B�B��B�:B��B�iB��B.B�B.B}"B|B|PB|�B|�B|�B|�B|PB|PB{~B{JBy�Bx�B{~B�B��B� B�B
=BkB%�B;�B?BK�B~�B��B�VB��B�B��B�rBv`B`BK�B>BB8B?}B@�BK�BK�BI�BB&B:)B-�B.�B,�B%zB=B�B�B��B��B�yB�#B��B��B�:B�1B[�BK^B2�B!bB
��B
�B
�B
��B
�>B
�5B
��B
��B
�B
��B
��B
��B
v+B
O�B
7KB
!�B
�B	��B	ںB	ѷB	ǮB	��B	�?B	�6B	�3B	��B	��B	�rB	�=B	�xB	�CB	�uB	{B	h�B	IB	2�B	#nB	�B�\B�B�+B�B�B�]BӏB��B�gB��B��B��B�0B��B��B��B��B��B�4B�'B�CB�IB�	B�{B��B�B{~B~\BzxBz�B{~B}�By	Bv�Bq�Br�Bo BoiBncBm]Bk�Bl�BglBb�Bb�Ba�Bc�Bb�Bf�BglBh�Bi�BjBjBn.Bo5BpBp;BpBjJBiyBg�BffBd�Bm�Bm�Bl�BkPBn.Bn�BlWBjBe�Bc�B`�B^�B_pB^5B^iB`AB]�B`B\)Bb�Bi�BrBq�Bw�B{�B~(B}�B�B�oB��B��B�B�FB��B��B��B��B��B�IB�'B�LB��B��B�UB�#B�B��B��B�UBÕB��B�EB��BɺB� B��BߤB��B�ZB�B�`B�,B�fB�rB�"B�PB�B�;B�B�B��B�xB�B��B	B	YB	+B	
rB	CB	�B	@B	�B	B	 �B	&B	*0B	,�B	6zB	?�B	EmB	J�B	U2B	W�B	]�B	b�B	dZB	g�B	iyB	jB	k�B	k�B	l�B	n�B	oiB	p;B	r�B	t�B	tB	v+B	x�B	z�B	{~B	{B	|PB	�MB	��B	�kB	��B	�1B	�B	��B	�=B	�rB	�~B	�VB	�nB	��B	�eB	��B	��B	�6B	�qB	�BB	�'B	�aB	��B	��B	�tB	��B	�^B	��B	�dB	��B	��B	��B	�wB	�NB	��B	��B	��B	��B	��B	ÕB	��B	�mB	�sB	ȀB	�KB	�WB	�dB	��B	��B	��B	�|B	��B	��B	тB	�&B	��B	�`B	�`B	��B	�gB	�mB	�B	�KB	ٴB	�QB	�)B	ܒB	�)B	�)B	�cB	�)B	�]B	�]B	�]B	ܒB	��B	�cB	ݘB	�;B	�|B	�B	��B	��B	�B	�lB	�
B	�B	�B	�JB	�B	�B	�B	�.B	� B	��B	�B	�;B	�AB	�oB	��B	�uB	�B	�B	��B	��B	��B	��B	�1B	��B	�lB	�7B	�B	�7B	�lB	�>B	�B	�DB	�JB	�~B	�~B	�JB	�JB	�PB	��B	��B	��B
 4B
 4B
B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
%B
%B
�B
�B
eB
eB
	7B

rB

=B

�B

�B
xB
xB
�B
�B
~B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
4B
�B
�B
hB
nB
�B
�B
tB
�B
�B
FB
{B
�B
�B
RB
�B
�B
$B
�B
RB
�B
$B
*B
�B
�B
�B
�B
�B
�B
kB
B
B
B
B
kB
B
�B
�B
=B
�B
wB
CB
=B
B
�B
UB
�B
 �B
!�B
"3B
"3B
"hB
!�B
!�B
"�B
$B
$B
$B
$B
$tB
%B
%zB
%zB
%zB
%zB
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'RB
(�B
)^B
)*B
)*B
)�B
)�B
)�B
*0B
*0B
*�B
+B
+kB
,B
,B
,B
,B
,B
-wB
-�B
.}B
.�B
/OB
/�B
0UB
0�B
0�B
1'B
1�B
1�B
33B
3gB
3�B
3�B
3�B
3�B
3�B
4B
49B
49B
4mB
4�B
4�B
5?B
5tB
6�B
6�B
6�B
6�B
6�B
6�B
7B
7B
7KB
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
8�B
8�B
8�B
8�B
7�B
6B
5�B
5tB
5?B
5?B
5B
5tB
5?B
5B
5B
4�B
5B
5B
5B
4�B
4�B
4�B
4�B
5?B
5?B
6zB
6�B
6�B
6�B
6�B
6�B
7KB
7�B
8B
8B
8B
8B
8RB
8�B
8�B
9#B
9#B
9#B
9�B
9�B
9�B
9�B
:^B
:^B
:�B
;dB
<B
=B
<�B
<�B
<�B
<�B
=B
=�B
>BB
>�B
>�B
>�B
?HB
?}B
?�B
@B
?�B
@�B
AUB
A�B
B&B
B[B
CaB
C�B
DgB
D3B
D3B
E9B
E9B
E�B
E�B
E�B
F?B
F�B
F�B
GEB
G�B
G�B
G�B
G�B
HB
HB
HB
HB
HB
HKB
IQB
H�B
I�B
J#B
K)B
J�B
K^B
K�B
K�B
L/B
L/B
L/B
L/B
LdB
LdB
MB
M�B
M�B
M�B
M�B
N<B
NpB
N�B
N�B
OvB
O�B
PB
O�B
PHB
P�B
P�B
P�B
QB
QB
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
S&B
S&B
SZB
S&B
SZB
SZB
S�B
T�B
T`B
T,B
S�B
T,B
UgB
U�B
U�B
V8B
VmB
VmB
VmB
VmB
V�B
VmB
XyB
XyB
XyB
XyB
XEB
XB
YB
ZB
Y�B
Y�B
Y�B
ZB
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
\)B
\]B
\�B
]cB
]�B
]�B
^B
^B
^5B
^5B
^iB
^�B
^�B
_�B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
bB
bB
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
d%B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
f�B
f�B
glB
glB
glB
glB
h>B
h>B
hrB
hrB
h�B
h�B
h�B
h�B
h�B
iB
iB
iB
iB
iB
iDB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jJB
jJB
jB
jB
kB
k�B
kPB
kPB
kPB
k�B
l�B
l�B
l�B
l�B
m(B
m(B
m]B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n.B
ncB
n.B
n.B
ncB
ncB
n�B
o�B
p;B
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qAB
qAB
quB
quB
q�B
rB
rB
rGB
r{B
r�B
r{B
sB
sB
s�B
s�B
s�B
sMB
s�B
s�B
s�B
tB
tB
tSB
tSB
tSB
t�B
t�B
u%B
t�B
t�B
t�B
uYB
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w1B
w�B
w�B
w�B
w�B
w�B
w�B
xlB
x�B
x�B
yrB
y�B
y�B
zB
zxB
zxB
zxB
zxB
{B
{JB
{~B
{~B
{�B
{�B
|B
|B
|B
|B
|�B
|�B
|�B
|�B
}"B
}"B
}�B
}�B
}�B
}�B
~(B
~\B
~�B
�B
~�B|�B��B��B�4BbB�B�4B�B�iB~�B��B� B��B�B�4B��B��B� B�B�iB�iB��B�B�iB��B�iB�B�oBbB��B�iB�4B��B�4B��B�oB~�B�oB��BbB�:B� B�B�B�B��B��B�4B�iB~�B�B~�B~�B��B~\B��BbB~�B�@B��B�B��B�uB�4B|B�iB~(B{B}�Bx7B~�B}�B~�BzxB|�B{�B{�B}�B|�BzxB|B}"B{B|�B|�B{~B}"B|�B{�B}�B|B|�B}�B{�B}"B|�B{�B}"B}�B{~B}"B}�B{~B|�B}VB{�B{�B}�B|�B{~B}�B|B{�B|PB}�B{�B{~B}"B|�B{B|�B|Bz�B{~B|�Bz�B{�B|BzBz�B|BzDB|B{�ByrB{~By�By�BzxBx�ByrBy�Bw�BxlBy�Bx�BxBw1Bx7Bw�By>B� B�%B~(B��B�uB�B��B�nB�wB�B��B�B�B��B��B�?B�mB��B�B�HB��BǮB��B��B�TB�B�B��B��B��B �B �B�B�B�B�BVB\BnB:B�BB�BnBB7B�B�B �B �B�B 'B!-B�B$�B"3B!�B#�B%zB%zB$@B'�B(�B(�B+B,�B-B-�B4�B6�B;0B=<B?�B?�B?BE9BC�BD�BB�B?}B?�B@B>�B>B@�B>wB>B?}B?HB=<B=�B>�B<�BB�BEB=B9XB=�B<�BD�BUgB_pBi�Bx7BwfBtBv�BxBxlB}"B|�Bx7By�B�B��B�IB�~B�VB��B�B�B�PB�xB�B��B��B�B�\B��B��B�~B��B��B��B��B�B�!B��B��B��B��B�:B�B��B�_B��B�3B�CB�OB��B�IB��B��B��B��B��B�_B��B�[B��B�*B�[B�B��B�*B�B�B��B�hB�\B�B��B��B�.B��B�uB��B�MBbB�IB�iBz�Bx�B��BncBlWBf�BrBg�Bc�Bd�Bg�B_B\]BW�B_�BZBT,BRTBOBWsBO�BL�BK^B@�B?�BC�BN�B8B6�B6zB7KBRTB4�BQ�B7B5?B3�B5tB4�B4mBE�B9#B8�BAUB=�B>�B?�B;dB=�B?�BEmB@NBA B?�B?B@�B?�B?HBD�BB�BA�BGyBM�BNpBL�BNBP�BQ�BK�BMjBMjBNBM5BK^BHKBJ�BK�BJ�BJWBM�BK^BG�BF?BIQBD�BL�BC�BGBB�B=pB:^B;0B8�BB[BK�B8�B2�B33B1�B-BB.IB/OB-wB-wB.�B-�B,<B-�B,�B+kB0UB0�B,�B0�B/�B0�B2�B2�B.�B'�B1�B)�B*�B*0B'�B&�B&�B&�B&�B"�B#�B%�B#9B"�B�B�B�B�B�B0B�B�B�BBLBB�B	�B+B�B_B_B	BtBB��B��B�xB��B 4B�1B��B��B�SB�B��B�iB�B�WB�B��B�B�B�lB�B�AB�cB�vB�TB�B�yB�
B��B��BтB��B��B��B��B�)BбB�B�aB��B�)B�yB��B�sB�QB��B�<B��B��B�mB�*B�6B�B��B��B�3B��B�=B�CB��B�OB��B��B��B��B��B�4B�\B�.B�:B��B��B��B��B�B��B{�Bv`BpoBbNBZQBY�B\�BY�BZ�B\�BY�BY�BV8BP|BaBT�B>BA�B;�B6�B4�B49B5tB33B2-B4B7�B.IB'BwB�B�BeB�B=B&BDgB
�+B
��B
�%B
�SB
�SB
�MB
��B�B
�AB
�iB
�WB
�B
�`B
�B
��B
�B
�B
��B
�B
�PB
�B
�B
�TB
��B
�GB
�B
�ZB
��B
��B
��B
�B
څB
�cB
��B
��B
��B
��B
�B
��B
ҽB
��B
ҽB
�TB
��B
��B
�B
��B
�
B
�HB
�BB
�dB
�BB
�BB
�pB
��B
��B
�BB
�6B
��B
��B
��B
��B
�B
��B
�RB
��B
��B
��B
�?B
�9B
�EB
��B
�B
�B
�0B
��B
�aB
�'B
��B
��B
�6B
��B
��B
��B
�wB
�B
�LB
�^B
�UB
��B
��B
�B
�PB
�'B
��B
�YB
��B
yrB
yrB
{�B
��B
w�B
v�B
n�B
j�B
e�B
T`B
^iB
R�B
Z�B
FsB
DgB
K�B
<B
:�B
:�B
9XB
3�B
6�B
2aB
<6B
8B
0�B
2aB
$@B
&LB
�B
�B
!�B
�B
+kB
�B
tB
 �B	�~B	�B
�B	�rB	�WB	�B	�B	�rB	�GB	��B	�B	�,B	�B	��B	ٴB	��B	�WB	�WB	ٴB	��B	�B	՛B	ԕB	ӏB	��B	��B	��B	��B	�WB	��B	�)B	͞B	��B	��B	�EB	ȀB	�B	�B	�dB	�)B	��B	��B	�?B	�dB	��B	��B	�RB	��B	��B	�EB	�aB	��B	�[B	��B	�KB	��B	�B	��B	�XB	��B	��B	��B	��B	�}B	��B	��B	�B	��B	��B	��B	��B	�UB	��B	��B	��B	��B	�~B	��B	�?B	�LB	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225005                            20230721225005AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122500520230721225005  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500520230721225005QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500520230721225005QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               